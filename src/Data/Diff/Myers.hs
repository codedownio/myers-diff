{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Data.Diff.Myers (
  diffTexts
  , diffStrings
  , diff
  , Edit(..)
  ) where

import Control.Monad.Primitive
import Data.Bits (xor)
import Data.Function
import Data.Text as T
import Data.Vector.Unboxed as VU
import Data.Vector.Unboxed.Mutable as V
import Prelude hiding (read)


data Edit = EditDelete { deletePositionOld :: Int }
          | EditInsert { insertPositionOld :: Int
                       , insertPositionNew :: Int }
  deriving (Show, Eq)

-- TODO: switch from slice to unsafeSlice once things are good

diffTexts :: Text -> Text -> IO [Edit]
diffTexts left right = do
  -- This is faster than V.fromList (T.unpack left), right?
  leftThawed <- V.generate (T.length left) (\i -> T.index left i)
  rightThawed <- V.generate (T.length right) (\i -> T.index right i)

  diff leftThawed rightThawed

-- | To use in benchmarking against other libraries that use String
diffStrings :: String -> String -> IO [Edit]
diffStrings left right = do
  leftThawed <- VU.thaw $ VU.fromList left
  rightThawed <- VU.thaw $ VU.fromList right

  diff leftThawed rightThawed

-- * Core

diff :: (
  PrimMonad m, Unbox a, Eq a
  ) => MVector (PrimState m) a -> MVector (PrimState m) a -> m [Edit]
diff e f = diff' e f 0 0

diff' :: (
  PrimMonad m, Unbox a, Eq a
  ) => MVector (PrimState m) a -> MVector (PrimState m) a -> Int -> Int -> m [Edit]
diff' e f i j = do
  let bigN = V.length e
  let bigM = V.length f
  let bigL = bigN + bigM
  let bigZ = (2 * (min bigN bigM)) + 2

  if | bigN > 0 && bigM > 0 -> do
         let w = bigN - bigM
         g <- new bigZ
         p <- new bigZ

         flip fix 0 $ \loopBaseH -> \case
           h | not (h <= ((bigL `pyDiv` 2) + (if (bigL `pyMod` 2) /= 0 then 1 else 0))) -> return []
           h -> do
             let loopH = loopBaseH (h + 1)
             flip fix (0 :: Int) $ \loopBaseR -> \case
               r | not (r <= 1) -> loopH
               r -> do
                 let loopR = loopBaseR (r + 1)
                 let (c, d, o, m) = if r == 0 then (g, p, 1, 1) else (p, g, 0, -1)
                 flip fix (negate (h - (2 * (max 0 (h - bigM))))) $ \loopBaseK -> \case
                   k | not (k <= (h - (2 * (max 0 (h - bigN))))) -> loopR
                   k -> do
                     let loopK = loopBaseK (k + 2)
                     a <- do
                       prevC <- read c ((k-1) `pyMod` bigZ)
                       nextC <- read c ((k+1) `pyMod` bigZ)
                       return (if ((k == (-h) || k /= h) && (prevC < nextC)) then nextC else prevC + 1)
                     let b = a - k
                     let (s, t) = (a, b)

                     (a, b) <- flip fix (a, b) $ \loop (a', b') -> do
                       eVal <- read e (((1 - o) * bigN) + (m*a') + (o - 1))
                       fVal <- read f (((1 - o) * bigM) + (m*b') + (o - 1))
                       if | a' < bigN && b' < bigM && eVal == fVal -> loop (a' + 1, b' + 1)
                          | otherwise -> pure (a', b')

                     write c (k `pyMod` bigZ) a
                     let z = negate (k - w)

                     cVal <- read c (k `pyMod` bigZ)
                     dVal <- read d (z `pyMod` bigZ)
                     if | (bigL `pyMod` 2 == o) && (z >= (negate (h-o))) && (z <= (h-o)) && (cVal + dVal >= bigN) -> do
                            let (bigD, x, y, u, v) = if o == 1 then ((2*h)-1, s, t, a, b) else (2*h, bigN-a, bigM-b, bigN-s, bigM-t)
                            if | bigD > 1 || (x /= u && y /= v) -> do
                                  ret1 <- diff' (V.slice 0 x e) (V.slice 0 y f) i j
                                  ret2 <- diff' (V.slice u (bigN - u) e) (V.slice v (bigM - v) f) (i+u) (j+v)
                                  return (ret1 <> ret2) -- TODO: switch from lists to Seq for faster (log-time) concat
                               | bigM > bigN ->
                                  diff' (V.slice 0 0 e) (V.slice bigN (bigM - bigN) f) (i+bigN) (j+bigN)
                               | bigM < bigN ->
                                  diff' (V.slice bigM (bigN - bigM) e) (V.slice 0 0 f) (i+bigM) (j+bigM)
                               | otherwise -> return []
                        | otherwise -> loopK


     | bigN > 0 ->
         return [EditDelete (i + n) | n <- [0..(bigN - 1)]]
     | otherwise -> do
         return [EditInsert i (j + n) | n <- [0..(bigM - 1)]]

pyMod :: Integral a => a -> a -> a
pyMod x y = if y >= 0 then x `mod` y else (x `mod` y) - y

pyDiv :: Integral a => a -> a -> a
pyDiv x y = if (x < 0) `xor` (y < 0) then -((-x) `div` y) else x `div` y

-- def diff(e, f, i=0, j=0):
--     #  Documented at http://blog.robertelder.org/diff-algorithm/
--     N,M,L,Z = len(e),len(f),len(e)+len(f),2*min(len(e),len(f))+2
--     if N > 0 and M > 0:
--         w,g,p = N-M,[0]*Z,[0]*Z
--         for h in range(0, (L//2+(L%2!=0))+1):
--             for r in range(0, 2):
--                 c,d,o,m = (g,p,1,1) if r==0 else (p,g,0,-1)
--                 for k in range(-(h-2*max(0,h-M)), h-2*max(0,h-N)+1, 2):
--                     a = c[(k+1)%Z] if (k==-h or k!=h and c[(k-1)%Z]<c[(k+1)%Z]) else c[(k-1)%Z]+1
--                     b = a-k
--                     s,t = a,b
--                     while a<N and b<M and e[(1-o)*N+m*a+(o-1)]==f[(1-o)*M+m*b+(o-1)]:
--                         a,b = a+1,b+1
--                     c[k%Z],z=a,-(k-w)
--                     if L%2==o and z>=-(h-o) and z<=h-o and c[k%Z]+d[z%Z] >= N:
--                         D,x,y,u,v = (2*h-1,s,t,a,b) if o==1 else (2*h,N-a,M-b,N-s,M-t)
--                         if D > 1 or (x != u and y != v):
--                             return diff(e[0:x],f[0:y],i,j)+diff(e[u:N],f[v:M],i+u,j+v)
--                         elif M > N:
--                             return diff([],f[N:M],i+N,j+N)
--                         elif M < N:
--                             return diff(e[M:N],[],i+M,j+M)
--                         else:
--                             return []
--     elif N > 0: #  Modify the return statements below if you want a different edit script format
--         return [{"operation": "delete", "position_old": i+n} for n in range(0,N)]
--     else:
--         return [{"operation": "insert", "position_old": i,"position_new":j+n} for n in range(0,M)]

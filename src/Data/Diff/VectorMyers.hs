{-# LANGUAGE OverloadedLists #-}

module Data.Diff.VectorMyers (
  diffTexts
  , diffTextsToChangeEvents
  , diffStrings

  , diffTextsIO
  , diffTextsToChangeEventsIO
  , diffStringsIO

  , diff
  , Edit(..)

  , editScriptToChangeEvents
  ) where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits (xor)
import Data.Diff.MyersShim
import Data.Diff.Types
import qualified Data.Foldable as F
import Data.Function
import Data.Sequence as Seq
import Data.Text as T
import Data.Vector.Unboxed as VU
import Data.Vector.Unboxed.Mutable as VUM
import Prelude hiding (read)


-- * Pure version uses ST

diffTexts :: Text -> Text -> Seq Edit
diffTexts left right = runST $ do
  -- This is faster than VU.fromList (T.unpack left), right?
  let l = VU.generate (T.length left) (\i -> T.index left i)
  let r = VU.generate (T.length right) (\i -> T.index right i)
  diff l r

diffTextsToChangeEvents :: Text -> Text -> [ChangeEvent]
diffTextsToChangeEvents left right = runST $ do
  -- This is faster than VU.fromList (T.unpack left), right?
  let l = VU.generate (T.length left) (\i -> T.index left i)
  let r = VU.generate (T.length right) (\i -> T.index right i)
  edits <- diff l r
  return $ F.toList $ editScriptToChangeEvents l r edits

-- | To use in benchmarking against other libraries that use String
diffStrings :: String -> String -> Seq Edit
diffStrings left right = runST $ do
  let leftThawed = VU.fromList left
  let rightThawed = VU.fromList right
  diff leftThawed rightThawed

-- * IO version to benchmark against

diffTextsIO :: Text -> Text -> IO (Seq Edit)
diffTextsIO left right = do
  -- This is faster than VU.fromList (T.unpack left), right?
  let l = VU.generate (T.length left) (\i -> T.index left i)
  let r = VU.generate (T.length right) (\i -> T.index right i)
  diff l r

diffTextsToChangeEventsIO :: Text -> Text -> IO [ChangeEvent]
diffTextsToChangeEventsIO left right = do
  -- This is faster than VU.fromList (T.unpack left), right?
  let l = VU.generate (T.length left) (\i -> T.index left i)
  let r = VU.generate (T.length right) (\i -> T.index right i)
  edits <- diff l r
  return $ F.toList $ editScriptToChangeEvents l r edits

-- | To use in benchmarking against other libraries that use String
diffStringsIO :: String -> String -> IO (Seq Edit)
diffStringsIO left right = do
  let leftThawed = VU.fromList left
  let rightThawed = VU.fromList right
  diff leftThawed rightThawed

-- * Core

diff :: (
  PrimMonad m, Unbox a, Eq a, Show a
  ) => Vector a -> Vector a -> m (Seq Edit)
diff e f = diff' e f 0 0

diff' :: (
  PrimMonad m, Unbox a, Eq a, Show a
  ) => Vector a -> Vector a -> Int -> Int -> m (Seq Edit)
diff' e f i j = do
  let (bigN, bigM) = (VU.length e, VU.length f)
  let bigZ = (2 * (min bigN bigM)) + 2
  g <- new bigZ
  p <- new bigZ
  diff'' g p e f i j

diff'' :: (
  PrimMonad m, Unbox a, Eq a, Show a
  ) => MVector (PrimState m) Int -> MVector (PrimState m) Int -> Vector a -> Vector a -> Int -> Int -> m (Seq Edit)
diff'' g' p' e f i j = do
  let (bigN, bigM) = (VU.length e, VU.length f)
  let (bigL, bigZ) = (bigN + bigM, (2 * (min bigN bigM)) + 2)

  if | bigN > 0 && bigM > 0 -> do
         let w = bigN - bigM

         -- Clear out the reused memory vectors
         let g = VUM.unsafeSlice 0 bigZ g'
         VUM.set g 0
         let p = VUM.unsafeSlice 0 bigZ p'
         VUM.set p 0

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
                     aInitial <- do
                       prevC <- unsafeRead c ((k-1) `pyMod` bigZ)
                       nextC <- unsafeRead c ((k+1) `pyMod` bigZ)
                       return (if (k == (-h) || (k /= h && (prevC < nextC))) then nextC else prevC + 1)
                     let bInitial = aInitial - k
                     let (s, t) = (aInitial, bInitial)

                     (a, b) <- flip fix (aInitial, bInitial) $ \loop (a', b') -> do
                       if | a' < bigN && b' < bigM -> do
                              let eVal = e `unsafeIndex` (((1 - o) * bigN) + (m*a') + (o - 1))
                              let fVal = f `unsafeIndex` (((1 - o) * bigM) + (m*b') + (o - 1))
                              if | eVal == fVal -> loop (a' + 1, b' + 1)
                                 | otherwise -> pure (a', b')
                          | otherwise -> pure (a', b')

                     write c (k `pyMod` bigZ) a
                     let z = negate (k - w)

                     cVal <- unsafeRead c (k `pyMod` bigZ)
                     dVal <- unsafeRead d (z `pyMod` bigZ)
                     if | (bigL `pyMod` 2 == o) && (z >= (negate (h-o))) && (z <= (h-o)) && (cVal + dVal >= bigN) -> do
                            let (bigD, x, y, u, v) = if o == 1 then ((2*h)-1, s, t, a, b) else (2*h, bigN-a, bigM-b, bigN-s, bigM-t)
                            if | bigD > 1 || (x /= u && y /= v) -> do
                                  ret1 <- diff'' g p (VU.unsafeSlice 0 x e) (VU.unsafeSlice 0 y f) i j
                                  ret2 <- diff'' g p (VU.unsafeSlice u (bigN - u) e) (VU.unsafeSlice v (bigM - v) f) (i+u) (j+v)
                                  return (ret1 <> ret2)
                               | bigM > bigN -> do
                                  diff'' g p (VU.unsafeSlice 0 0 e) (VU.unsafeSlice bigN (bigM - bigN) f) (i+bigN) (j+bigN)
                               | bigM < bigN -> do
                                  diff'' g p (VU.unsafeSlice bigM (bigN - bigM) e) (VU.unsafeSlice 0 0 f) (i+bigM) (j+bigM)
                               | otherwise -> return []
                        | otherwise -> loopK


     | bigN > 0 -> return [EditDelete i (i + (bigN - 1))]
     | bigM == 0 -> return []
     | otherwise -> return [EditInsert i j (j + (bigM - 1))]

{-# INLINABLE pyMod #-}
pyMod :: Integral a => a -> a -> a
pyMod x y = if y >= 0 then x `mod` y else (x `mod` y) - y

{-# INLINABLE pyDiv #-}
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

-- * Testing

-- printVector :: (Unbox a, PrimMonad m, Show a) => Text -> MVector (PrimState m) a -> m ()
-- printVector name v = do
--   frozen <- VU.freeze v
--   traceM [iii|#{name}: #{frozen}|]

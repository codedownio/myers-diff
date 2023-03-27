{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Data.Diff.Myers (
  diff
  , Edit(..)
  ) where

import Control.Loop
import Data.Array.IO
import Data.Function


class AbstractVector a where
  abstractLen :: a -> Int

data Edit = EditDelete { deletePositionOld :: Int }
          | EditInsert { insertPositionOld :: Int
                       , insertPositionNew :: Int }

diff e f i j = do
  let bigN = abstractLen e
  let bigM = abstractLen f
  let bigL = bigN + bigM
  let bigZ = (2 * (min bigN bigM)) + 2

  if | bigN > 0 && bigM > 0 -> do
         let w = bigN - bigM
         g :: IOUArray Int Int <- newArray (0, bigZ - 1) 0
         p :: IOUArray Int Int <- newArray (0, bigZ - 1) 0

         numLoopState 0 ((bigL `div` 2) + (if (bigL `mod` 2) /= 0 then 1 else 0)) [] $ \_lastState h -> do
           numLoopState (0 :: Int) 1 [] $ \_lastState r -> do
             let (c, d, o, m) = if r == 0 then (g, p, 1, 1) else (p, g, 0, -1)
             forLoopState (negate (h - (2 * (max 0 (h - bigM))))) (<= (h - (2 * (max 0 (h - bigN))))) (+ 2) [] $ \_lastState k -> do
               a <- do
                 prevC <- readArray c ((k-1) `mod` bigZ)
                 nextC <- readArray c ((k+1) `mod` bigZ)
                 return (if (k==(-h) || k /= h && prevC < nextC) then nextC else prevC + 1) -- TODO: precedence of || and && matches python?
               let b = a - k
               let (s, t) = (a, b)

               (a, b) <- flip fix (a, b) $ \loop (a, b) -> do
                 eVal <- readArray e ((1 - o) * bigN + m*a + (o - 1))
                 fVal <- readArray f ((1 - o) * bigM + m*b + (o - 1))
                 if | a < bigN && b < bigM && eVal == fVal -> loop (a + 1, b + 1)
                    | otherwise -> pure (a, b)

               writeArray c (k `mod` bigZ) a
               let z = negate (k - w)

               cVal <- readArray c (k `mod` bigZ)
               dVal <- readArray d (z `mod` bigZ)
               if | (bigL `mod` 2 == o) && (z >= (negate (h-o))) && (z <= h - o) && (cVal + dVal >= bigN) -> do
                      let (bigD, x, y, u, v) = if o == 1 then (2 * (h-1), s, t, a, b) else (2*h, bigN-a, bigM-b, bigN-s, bigM-t)
                      if | bigD > 1 || (x /= u && y /= v) ->
                            -- diff (e[0:x], f[0:y], i, j) + diff (e[u:N], f[v:M], i+u, j+v)
                            undefined
                         | bigM > bigN ->
                            -- diff([], f[bigN:bigM], i+bigN, j+bigN)
                            undefined
                         | bigM < bigN ->
                            -- diff(e[bigM:bigN], [], i+bigM, j+bigM)
                            undefined
                         | otherwise -> return []
                  | otherwise -> return []


     | bigN > 0 ->
         return [EditDelete (i + n) | n <- [0..(bigN - 1)]]
     | otherwise -> do
         return [EditInsert i (j + n) | n <- [0..(bigM - 1)]]

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

module Main where

import qualified Data.Matrix as M
import qualified Data.Maybe as  M1
import qualified Data.IntSet as S
import qualified Data.Vector as V
type SudokuBoard = M.Matrix Int
data IntermediateState = IMS {s::SudokuBoard,pos::(Int,Int),usedValues::[Int] }

findAllEmpty::SudokuBoard->[(Int,Int)]
findAllEmpty sb  = [(i,j)|i<-[1..9],j<-[1..9],(M.getElem i j sb) == 0]

findNextEmpty :: SudokuBoard-> Maybe (Int,Int)
findNextEmpty sb = M1.listToMaybe (findAllEmpty sb)


allNumbers = S.fromList [1..9]

next :: SudokuBoard->[IntermediateState]->SudokuBoard
next sb l =
   case findNextEmpty sb of
        Nothing -> sb
        Just p  -> case tryUnvisited sb p of
                    Nothing -> previous l
                    Just x  -> next (s x) (x:l)

previous [] = error "No intermediate states"
previous (x:xs) =
   case tryVisited x of
      Nothing ->  previous xs
      (Just y) -> next (s y) (y:xs)

tryUnvisited :: SudokuBoard -> (Int,Int) -> Maybe IntermediateState
tryUnvisited sb p =
    let f = findCandidates p [] sb
    in if S.null f then
         Nothing
       else
          let sb' = M.setElem (S.findMin f) p sb
          in Just $ IMS sb' p [(S.findMin f)]
tryVisited :: IntermediateState-> Maybe IntermediateState
tryVisited ims = 
      let f = findCandidates (pos ims) (usedValues ims) (s ims)
      in if S.null f then
           Nothing
          else
            let sb' = M.setElem (S.findMin f) (pos ims) (s ims)
            in Just $ IMS sb' (pos ims) ((S.findMin f):(usedValues ims))

findCandidates :: (Int,Int)->[Int]->SudokuBoard->S.IntSet
findCandidates p uvs  sb =
    let r = S.fromList . V.toList $  M.getRow (fst p) sb
        c = S.fromList . V.toList $  M.getCol (snd p) sb
        b = S.fromList . V.toList $  M.getMatrixAsVector (getBox sb p)
        u = S.fromList uvs
        k = S.delete 0 $ S.unions [r,c,b] 
     in S.difference (S.difference allNumbers k) u

getBox :: SudokuBoard->(Int,Int)->SudokuBoard
getBox sb (i,j)  =
     if i>=1 && i<=3 && j>=1 && j<=3 then
            M.submatrix 1 3 1 3 sb
     else if i>=1 && i<=3 && j>=4 && j<=6 then
          M.submatrix 1 3 4 6 sb
     else if i>=1 && i<=3 && j>=7 && j<=9 then
          M.submatrix 1 3 7 9 sb
     else if i>=4 && i<=6 && j>=1 && j<=3 then
          M.submatrix 4 6 1 3 sb
     else if i>=4 && i<=6 && j>=4 && j<=6 then
          M.submatrix 4 6 4 6 sb
     else if i>=4 && i<=6 && j>=7 && j<=9 then
          M.submatrix 4 6 7 9 sb
     else if i>=7 && i<=9 && j>=1 && j<=3 then
          M.submatrix 7 9 1 3 sb
     else if i>=7 && i<=9 && j>=4 && j<=6 then
          M.submatrix 7 9 4 6 sb
     else if i>=7 && i<=9 && j>=7 && j<=9 then
          M.submatrix 7 9 7 9 sb
     else
        error "No good coordinates"
test = M.fromList 9 9 [0,0,0,1,3,2,4,0,0,0,7,0,0,0,8,1,0,0,0,2,9,7,6,0,5,0,8,0,9,6,8,0,1,0,4,5,7,4,0,0,0,0,0,0,1,5,0,0,0,4,0,7,2,6,0,5,0,4,0,0,0,0,0,4,0,2,9,1,5,0,0,0,9,0,7,0,0,3,2,0,0]

solve =  next test []

main :: IO ()
main = print "ssss"

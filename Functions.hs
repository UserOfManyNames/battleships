module Functions where
import Data.List (intercalate, delete, nub)

{- Player name
Represents the name of the player, which the player enters at the start of the game
-}   
type Player = [Char] 


{- Coordinates
Represents coordinates on the grid, the first element of the tuple represents the
coordinate on the x axis and the second element represents the coordinate on the y axis. 
-}  
type Coordinates = (Int,Int)


{- Grid
Represent the grid, which the game is played on. 
-}  
type Grid = [[Char]]

{-createGrid
    Creates a grid
    PRE:
    RETURNS: a list of five strings that consists of 5 elements.
    SIDE EFFECTS:-
    EXAMPLES:-
-}
createGrid :: IO Grid
createGrid = return ["*****","*****","*****","*****","*****"]


{- validMove [Coordinates]
     A function that checks if a move is valid or not in a 5x5 grid. 
     PRE:-
     RETURNS: A Bool: True if valid, if not it's False.
     SIDE EFFECTS:-
     EXAMPLES: 
     validMove [(1,1)] == True
     validMove [(7,2)] == False 
     validMove [(2,7)] == False 
-}
validMove :: [Coordinates] -> Bool 
validMove [] = False 
validMove l 
            | ((areValid l) && (duplicateCoord l)) && lengthList l = True 
            | otherwise = False

{- lengthList [Coordinates]
     A function that checks if the lnegths of a list of tuples of integers are in the range: range = 1.
     PRE:-
     RETURNS: A Bool: True if valid, if not it's False.
     SIDE EFFECTS:-
     EXAMPLES: 
     lengthList [(1,1),(1,2),(1,3)] == False
     lengthList [(1,1)] == True
     lengthList [] == False 
     lengthList [1,2,3,4] => error. (Not a Coordinates type)
-}
lengthList :: [Coordinates] -> Bool 
lengthList [] = False 
lengthList list 
               | length list == 1 = True
               | otherwise = False 

{- areValid [Coordinates]
     A function that checks if all the coordinates in the input list are valid or not on a 5x5 Grid
     PRE:-
     RETURNS: A Bool: True if valid, if not it's False 
     SIDE EFFECTS:-
     EXAMPLES: 
     areValid [(1,1),(1,2),(1,3)] == True 
     areValid [(5,5)] == True 
     areValid [(2,8),(1,2),(1,3)] == False 
-}
areValid :: [Coordinates] -> Bool
areValid [] = False 
areValid (x:xs) = listChecker (map isValid (x:xs)) && duplicateCoord (x:xs)


{- listChecker [Bool]
     A function that checks if a list contain elements of Bool that are all True. 
     PRE:-
     RETURNS: A Bool: True if list elements are only True, if not it's False
     SIDE EFFECTS:-
     EXAMPLES:
     listChecker [True,True,True] == True 
     listChecker [False,True,True] == False 
     listChecker [False,False,False] == False 
-}

listChecker :: [Bool] -> Bool
listChecker [] = False 
listChecker (x:xs)
                  | all (== True) (x:xs) = True
                  | otherwise = False
                  

{- isValid Coordinates 
     A function that checks if a single coordinate is valid or not on a 5x5 grid.
     PRE:-
     RETURNS: A Bool: True if valid, if not it's False.
     SIDE EFFECTS:-
     EXAMPLES:
     isValid (4,1) == True 
     isValid (3,6) == False 
     isValid (6,3) == False
-}

isValid :: Coordinates -> Bool
isValid (a,b) 
              | ((5 >=a) && (a > 0)) && ((5 >= b) && (b > 0) ) = True
              | otherwise = False


{- duplicateCoord [Coordinates]
     A function that checks if the player is shooting at the same coordinate twice.
     PRE:-
     RETURNS: A Bool: False if it's not a duplicate, otherwise True. 
     SIDE EFFECTS:-
     EXAMPLES: 
     duplicateCoord [(1,2),(1,2)] == False
     duplicateCoord [(1,1),(1,2),(1,1)] == False 
     duplicateCoord [(2,1),(2,2),(2,3)] == True 
-}

duplicateCoord :: [Coordinates] -> Bool 
duplicateCoord [] = True 
duplicateCoord [x] = True
duplicateCoord l = if (length (nub l) /= length l) == True then False else True 


{- hitMissGrid [Coordinates] [Coordinates] Grid 
     A function that notifies the player if a boat is hit or not.
     PRE:-
     RETURNS: The newly marked grid.
     SIDE EFFECTS:-
     EXAMPLES: 

     hitMissGrid [(1,1)] [(1,1),(1,2),(1,3)] ["*****","*****","*****","*****","*****"] => ["x****","*****","*****","*****","*****"]

     hitMissGrid [(1,1),(1,2),(1,3)] [(2,1),(2,2),(2,3)] ["*****","*****","*****","*****","*****"] => ["ooo**","*****","*****","*****","*****"]

     hitMissGrid [(145,17),(19,28),(10,30)] [(61,86),(786,86),(976,976)] ["*****","*****","*****","*****","*****"] => *** Exception: Prelude.!!: index too large
-}

hitMissGrid :: [Coordinates] -> [Coordinates] -> Grid -> IO Grid 
hitMissGrid coords boats grid
                 | (boatsHitBool coords boats) == False = return (replaceValues coords 'o' grid)
                 | (boatsHitBool coords boats) == True = return (replaceValues (boatsHit coords boats) 'x' grid)


  
 
{- boatsHitBool [Coordinates] [Coordinates] 
     A function that takes the coordinates of the boats and shots and checks if its a hit or a miss. 
     PRE:-
     RETURNS: A Bool: True if hit, False if it's a miss 
     SIDE EFFECTS:-
     EXAMPLES: 
     boatsHitBool [(1,2)] [(1,2),(1,3)] == True 
     boatsHitBool [(1,1)] [(1,1)] == True 
     boatsHitBool [(1,1)] [(1,2)] == False 
  -}

boatsHitBool :: [Coordinates] -> [Coordinates] -> Bool
boatsHitBool coords boats = if  (boatsHit coords boats) == [] then False else True 


{- boatsHit [Coordinates] [Coordinates] 
     A function that compares the coordinates of the boats and shots fired to see if there are any intersections of the lists. 
     PRE:-
     RETURNS: A list of Coordinates or any empty list. 
     SIDE EFFECTS:-
     EXAMPLES: 
     boatsHit [(1,1),(1,2)] [(1,2),(1,3)] == [(1,2)]
     boatsHit [(1,1)] [(1,1)] == [(1,1)] 
     boatsHit [(1,1)] [(1,2)] == []
-}

boatsHit :: [Coordinates] -> [Coordinates] -> [Coordinates]
boatsHit [] _ = []
boatsHit _ [] = []
boatsHit (x:xs) boats 
                      | x `elem` boats = x : boatsHit xs (delete x boats)
                      | otherwise = boatsHit xs boats 

{- replaceValues [Coordinates] Char Grid 
     A function that replaces hit coordinates with a character.
     PRE: Coordinates must match the chosen input Grid (indeces), otherwise the function will not run. 
     RETURNS: A new Grid replacing the elements with the chosen Char corresponding to the list of Coordinates.
     SIDE EFFECTS:-
     EXAMPLES:

     replaceValues [(1,1)] 'x' ["*****","*****","*****","*****","*****"] == ["x****","*****","*****","*****","*****"]

     replaceValues [(2,1),(2,2),(2,3),(2,4),(2,5)] 'c' ["*****","*****","*****","*****","*****"] == ["*****","ccccc","*****","*****","*****"]

     replaceValues [(5,5)] 'x' [['*']] => *** Exception: Prelude.!!: index too large
-}

replaceValues :: [Coordinates] -> Char -> Grid -> Grid 
replaceValues (x:xs) val g = if (length (x:xs) == 1) then replaceVal x val g else replaceValues xs val (replaceVal x val g)
  

{- replaceVal [Coordinates] Char Grid 
     A helper functiton for replaceValues that replaces a single element in a grid.
     PRE: Coordinates must match the chosen input Grid (indeces), otherwise the function will not run.
     RETURNS: A new Grid replacing the element with the chosen Char corresponding to the coordinate.
     SIDE EFFECTS:-
     EXAMPLES:

     replaceVal (1,1) 'x' ["*****","*****","*****","*****","*****"] == ["x****","*****","*****","*****","*****"]

     replaceVal (5,5) 'x' ["*****","*****","*****","*****","*****"] == ["*****","*****","*****","*****","****x"]

     replaceVal (6,6) 'x' ["*****","*****","*****","*****","*****"] => *** Exception: Prelude.!!: index too large
-}
                
replaceVal :: Coordinates -> Char -> Grid -> Grid
repalceVal _ _ [] = []
replaceVal (a,b) val g = newList g val (a-1,b-1) 

{- newList Grid Char Coordinates 
     A function that uses the list indeces to locate and change a value on the grid.
     PRE: Coordinate integers can't be larger than 4 since this function counts the index starting from 0 (in Battleships there is for example no (0,0) coordinate). 
     RETURNS: A new grid corresponing to the input coordinate. 
     SIDE EFFECTS: ... side effects, if any, including exceptions ...
     EXAMPLES: 

     newList ["*****","*****","*****","*****","*****"] 'b' (4,4) == ["*****","*****","*****","*****","****b"]

     newList ["*****","*****","*****","*****","*****"] 'x' (1,2) == ["*****","**x**","*****","*****","*****"]

     newList ["*****","*****","*****","*****","*****"] 'b' (5,5) => *** Exception: Prelude.!!: index too large
-}

newList :: Grid -> Char -> Coordinates -> Grid
newList m x (r,c) = take r m ++ [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++ drop (r + 1) m


{- endGameCheck [Coordinates] [Coordinates] Grid 
     A function that checks if the computer or player has won the game (sunk all ships)
     PRE:-
     RETURNS: A Bool: True if someone has won, False if the game is not finished yet 
     SIDE EFFECTS:-
     EXAMPLES:
     endGameCheck [(1,1),(1,2),(1,3)] ["xxx**","*o***","*ooo*","*****","*****"] == True
     endGameCheck [(1,1),(2,2),(3,3)] ["x****","*x***","**x**","*****","*****"] == True
     endGameCheck [(1,1),(1,2),(1,3),(1,4),(1,5)] ["**xxx","*****","*****","*****","*****"] == False 
-}

endGameCheck :: [Coordinates] -> Grid -> Bool 
endGameCheck boats g = g == (replaceValues boats 'x' g) 


{- toTuple Integer Integer
    A function that converts two integers into a coordinate in a list
    PRE: -
    RETURNS: A list containing a tuple of two integers
    SIDE EFFECTS: -
    EXAMPLES: 
    toTuple 1 5 => [(1,5)]
    toTuple 20 34 => [(20,34)]
-}
toTuple :: Int -> Int -> IO [Coordinates]
toTuple x y = return [(x,y)] 


{- toTupleList x y j k h g r t w v
    Takes ten integers and compiles then into a list of tuples.
    PRE:-
    RETURNS: A list of tuples.
    SIDE EFFECTS:-
    EXAMPLES: toTupleList 1 2 3 4 5 6 7 8 9 10 = [(1,2),(3,4),(5,6),(7,8),(9,10)]
              toTupleList 34 23 52 4 123 36 734 58 19 150 = [(34,23),(52,4),(123,36),(734,58),(19,150)]
              toTupleList 1 1 1 1 1 1 1 2 2 1 = [(1,1),(1,1),(1,1),(1,2),(2,1)]
-}
toTupleList :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO [Coordinates]
toTupleList x y j k h g r t w v = return [(x, y),(j,k),(h,g),(r,t),(w,v)]  


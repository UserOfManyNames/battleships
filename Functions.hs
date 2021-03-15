module Functions where
import Data.List (intercalate, delete, nub)

{- Player name
Represents the name of the player, which the player enters at the start of the game.
-}   
type Player = [Char] 


{- Coordinates
Represents coordinates on the grid, the first element of the tuple represents the
coordinate on the x axis and the second element represents the coordinate on the y axis. 
Invariant: Each element of the tuple is within the range of 1 ≤ x ≤ 5.
-}  
type Coordinates = (Int,Int)


{- Grid
Represent the grid, which the game is played on. 
Invariant: The list contains 5 elements which are lists of Chars since the game is played on a 5x5 grid. 
-}  
type Grid = [[Char]]

{-createGrid
    Creates a 5x5 grid. 
    RETURNS: A list of five strings that consists of 5 elements.
-}
createGrid :: IO Grid
createGrid = return ["*****","*****","*****","*****","*****"]

{- areValid [Coordinates]
     A function that checks if all the coordinates in the input list are valid or not on a 5x5 Grid
     RETURNS: A Bool: True if valid, if not it's False 
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
     RETURNS: A Bool: True if list elements are only True, if not it's False
     EXAMPLES:
     listChecker [True,True,True] == True 
     listChecker [False,True,True] == False 
     listChecker [False,False,False] == False 
-}
listChecker :: [Bool] -> Bool
listChecker [] = False 
listChecker (x:xs)= all (== True) (x:xs)
                  

{- isValid Coordinates 
     A function that checks if a single coordinate is valid or not on a 5x5 grid.
     RETURNS: A Bool: True if valid, if not it's False.
     EXAMPLES:
     isValid (4,1) == True 
     isValid (3,6) == False 
     isValid (6,3) == False
-}
isValid :: Coordinates -> Bool
isValid (a,b) = (5 >=a) && (a > 0) && (5 >= b) && (b > 0)


{- duplicateCoord [Coordinates]
     A function that checks if the player is shooting at the same coordinate twice.
     RETURNS: A Bool: False if it's not a duplicate, otherwise True. 
     EXAMPLES: 
     duplicateCoord [(1,2),(1,2)] == False
     duplicateCoord [(1,1),(1,2),(1,1)] == False 
     duplicateCoord [(2,1),(2,2),(2,3)] == True 
-}
duplicateCoord :: [Coordinates] -> Bool 
duplicateCoord [] = True 
duplicateCoord [x] = True
duplicateCoord l = length (nub l) == length l


{- hitMissGrid Coordinates [Coordinates] Grid 
     A function that notifies the player if a boat is hit or not.
     PRE: Index cannot be larger than five due to the 5x5 grid limit. 
     RETURNS: The newly marked grid.
     EXAMPLES: 
     hitMissGrid (1,1) [(1,1),(1,2),(1,3)] ["*****","*****","*****","*****","*****"] => ["x****","*****","*****","*****","*****"]
     hitMissGrid (1,1) [(2,1),(2,2),(2,3)] ["*****","*****","*****","*****","*****"] => ["ooo**","*****","*****","*****","*****"]
     hitMissGrid (5,1) [(5,1)] ["*****","*****","*****","*****","*****"] => ["*****","*****","*****","*****","x****"]
-}
hitMissGrid :: Coordinates -> [Coordinates] -> Grid -> IO Grid 
hitMissGrid coords boats grid
                 | (boatsHitBool coords boats) == False = return (replaceVal coords 'o' grid)
                 | (boatsHitBool coords boats) == True = return (replaceVal (boatsHit coords boats) 'x' grid)

 
{- boatsHitBool Coordinates [Coordinates] 
     A function that takes the coordinates of the boats and shots and checks if its a hit or a miss. 
     RETURNS: A Bool: True if hit, False if it's a miss 
     EXAMPLES: 
     boatsHitBool (1,2) [(1,2),(1,3)] == True 
     boatsHitBool (1,1) [(1,1)] == True 
     boatsHitBool (1,1) [(1,2)] == False 
  -}
boatsHitBool :: Coordinates -> [Coordinates] -> Bool
boatsHitBool coords boats = boatsHit coords boats /= (0,0)


{- boatsHit Coordinates [Coordinates] 
     A function that compares the coordinates of the boats and shots fired to see if there are any intersections of the lists. 
     RETURNS: A list of Coordinates or any empty list. 
     EXAMPLES: 
     boatsHit (1,1) [(1,2),(1,3)] == (0,0)
     boatsHit (1,1) [(1,1)] == (1,1) 
     boatsHit (1,1) [(1,2)] == (0,0)
-}
boatsHit :: Coordinates -> [Coordinates] -> Coordinates
boatsHit coor boats 
                      | coor `elem` boats = coor
                      | otherwise = (0,0)


{- replaceVal [Coordinates] Char Grid 
     A helper functiton for replaceValues that replaces a single element in a grid.
     PRE: Coordinates must match the chosen input Grid (indeces), otherwise the function will not run.
     RETURNS: A new Grid replacing the element with the chosen Char corresponding to the coordinate.
     EXAMPLES:

     replaceVal (1,1) 'x' ["*****","*****","*****","*****","*****"] == ["x****","*****","*****","*****","*****"]

     replaceVal (5,5) 'x' ["*****","*****","*****","*****","*****"] == ["*****","*****","*****","*****","****x"]

     replaceVal (3,1) 'x' ["*****","*****","*****","*****","*****"] == ["*****","*****","x****","*****","*****"]
-}            
replaceVal :: Coordinates -> Char -> Grid -> Grid
replaceVal _ _ [] = []
replaceVal (a,b) val g = newList g val (a-1,b-1) 

{- newList Grid Char Coordinates 
     A function that uses the list indeces to locate and change a value on the grid.
     PRE: Coordinate integers can't be larger than 4 since this function counts the index starting from 0 (in Battleships there is for example no (0,0) coordinate). 
     RETURNS: A new grid corresponing to the input coordinate. 
     EXAMPLES: 

     newList ["*****","*****","*****","*****","*****"] 'b' (4,4) == ["*****","*****","*****","*****","****b"]

     newList ["*****","*****","*****","*****","*****"] 'x' (1,2) == ["*****","**x**","*****","*****","*****"]

     newList ["*****","*****","*****","*****","*****"] 'b' (4,4) == ["*****","*****","*****","*****","****b"]
-}
newList :: Grid -> Char -> Coordinates -> Grid
newList m x (r,c) = take r m ++ [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++ drop (r + 1) m


{- endGameCheck [Coordinates] Grid 
     A function that checks if the computer or player has won the game (sunk all ships)
     RETURNS: A Bool: True if someone has won, False if the game is not finished yet 
     EXAMPLES:
     endGameCheck [(1,1),(1,2),(1,3)] ["xxx**","*o***","*ooo*","*****","*****"] == True
     endGameCheck [(1,1),(2,2),(3,3)] ["x****","*x***","**x**","*****","*****"] == True
     endGameCheck [(1,1),(1,2),(1,3),(1,4),(1,5)] ["**xxx","*****","*****","*****","*****"] == False 
-}
endGameCheck :: [Coordinates] -> Grid -> Bool 
endGameCheck boats g = g == (replaceValuesList boats 'x' g) 

{- replaceValuesList [Coordinates] Char Grid 
     A function that replaces hit coordinates with a character.
     PRE: Coordinates must match the chosen input Grid (indeces), otherwise the function will not run. 
     RETURNS: A new Grid replacing the elements with the chosen Char corresponding to the list of Coordinates.
     EXAMPLES:

     replaceValuesList [(1,1)] 'x' ["*****","*****","*****","*****","*****"] == ["x****","*****","*****","*****","*****"]

     replaceValuesList [(2,1),(2,2),(2,3),(2,4),(2,5)] 'c' ["*****","*****","*****","*****","*****"] == ["*****","ccccc","*****","*****","*****"]

     replaceValuesList [(1,1),(2,2),(3,3),(4,4),(5,5)] 'b' ["*****","*****","*****","*****","*****"] == ["b****","*b***","**b**","***b*","****b"]
-}
replaceValuesList :: [Coordinates] -> Char -> Grid -> Grid 
replaceValuesList (x:xs) val g = if (length (x:xs) == 1) then replaceVal x val g else replaceValuesList xs val (replaceVal x val g)


{- toTuple Int Int
    A function that converts two integers into a coordinate
    RETURNS: A list containing a tuple of two integers
    EXAMPLES: 
    toTuple 1 5 => (1,5)
    toTuple 20 34 => (20,34)
-}
toTuple :: Int -> Int -> IO Coordinates
toTuple x y = return (x,y) 


{- toTupleList Int Int Int Int Int Int Int Int Int Int
    Takes ten integers and compiles then into a list of  tuples.
    RETURNS: A list of tuples.
    EXAMPLES: toTupleList 1 2 3 4 5 6 7 8 9 10 = [(1,2),(3,4),(5,6),(7,8),(9,10)]
              toTupleList 34 23 52 4 123 36 734 58 19 150 = [(34,23),(52,4),(123,36),(734,58),(19,150)]
              toTupleList 1 1 1 1 1 1 1 2 2 1 = [(1,1),(1,1),(1,1),(1,2),(2,1)]
-}
toTupleList :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO [Coordinates]
toTupleList x y j k h g r t w v = return [(x, y),(j,k),(h,g),(r,t),(w,v)]  


{-validBoats [Coordinates]
    Checks that an input of boats is formulated correctly.
    RETURNS: A Bool: True if the boats are valid, False if not. 
    EXAMPLES: validBoats [(1,1)] == False
              validBoats [(1,1),(1,2),(1,2),(1,3),(1,4)] == False
              validBoats [(1,2),(2,3),(3,4),(4,5),(5,6)] == False
              validBoats [(1,1),(1,2),(1,3),(1,4),(1,5)] == True
-}
validBoats :: [Coordinates] -> Bool
validBoats boats = (listChecker (map isValid boats ) && duplicateCoord boats) && length boats == 5


{- usedCoords Coordinates [Coordinates]
     A function that checks if a coordinate already exists in a list of coordinates and is a valid coordinate for the game. 
     RETURNS: A Bool: True if the coordiante is not in the list already, otherwise it's False.
     EXAMPLES: usedCoords (1,2) [(1,1)] == True
               usedCoords (1,1) [(1,1),(1,2)] == False 
               usedCoords (5,4) [(1,3),(2,4),(5,4)] == False 
  -}
usedCoords :: Coordinates -> [Coordinates] -> Bool
usedCoords userShot usedShots = duplicateCoord (addToList userShot usedShots) && areValid (addToList userShot usedShots)


{- addToList Coordinates [Coordinates]
     A function that adds a new coordinate to a list of coordinates by adding a new list.
     RETURNS: A new list similar to the second argument of the function but now it also contains the first argument of the function. 
     EXAMPLES: addToList (1,2) [(1,1)] == [(1,2),(1,1)]
               addToList (1,1) [(1,1),(1,2)] == [(1,1),(1,1),(1,2)]
               addToList (5,4) [(1,3),(2,4),(5,4)] == [(5,4),(1,3),(2,4),(5,4)]
  -}
addToList :: Coordinates -> [Coordinates] -> [Coordinates]
addToList userShot usedShots = userShot:usedShots





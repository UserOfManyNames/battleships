module Main where
import Control.Exception (SomeException, evaluate, catch)
import Data.List (intercalate, delete, nub)
import System.Exit (exitSuccess)
import System.Random (getStdRandom, randomR)
import System.IO.Unsafe (unsafePerformIO)
import Test.HUnit ( assertEqual, Test(..) )
import Functions as F 

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


-- A GAME BY: Katarina Feldtmann, Yasmine Odelbrink and Christian Ocklind --


{- main
     A function that runs the game and determines it's variables.
     PRE:-
     RETURNS:-
     SIDE EFFECTS: 
     EXAMPLES:-
-}
main :: IO ()
main = do
   putStrLn "Do you want to play a game? y/n"
   answer <- getLine
   if answer == "y" then do
      putStrLn "Welcome to Battleships! Whats your Name?"
      name <- getLine
      putStrLn ("Hi " ++ name ++ "! You will play on a 5x5 grid and fire your shots using [(Row,Column)] coordinates against the computer. 'x' is a hit and 'o' is a miss!")
      computerGameState <- F.createGrid
      playerGameState <- F.createGrid
      putStrLn "Choose your 5 boat coordinates:"
      playerBoats <- validInput
      putStrLn "Your boats have been stationed!"
      cC <- F.toTupleList (unsafePerformIO (getStdRandom (randomR (1, 5)))) (unsafePerformIO (getStdRandom (randomR (1, 5)))) (unsafePerformIO (getStdRandom (randomR (1, 5)))) (unsafePerformIO (getStdRandom (randomR (1, 5))))(unsafePerformIO (getStdRandom (randomR (1, 5))))(unsafePerformIO (getStdRandom (randomR (1, 5)))) (unsafePerformIO (getStdRandom (randomR (1, 5))))(unsafePerformIO (getStdRandom (randomR (1, 5)))) (unsafePerformIO (getStdRandom (randomR (1, 5)))) (unsafePerformIO (getStdRandom (randomR (1, 5)))) :: IO [F.Coordinates]
      game computerGameState playerGameState playerBoats cC name
   else exitSuccess

{- game
     Plays the actual game 
     PRE:-
     RETURNS:-
     SIDE EFFECTS: Allows the player to interacts with the game. 
     EXAMPLES:-
-}
game :: F.Grid -> F.Grid -> [F.Coordinates] -> [F.Coordinates] -> String -> IO ()
game computerGameState playerGameState playerBoats cC name = do
     printGameState computerGameState playerGameState
     putStrLn "FIRE!"
     playerShot <- validInput
     if F.validMove playerShot then
        do newGameState <- F.hitMissGrid playerShot cC computerGameState
           if F.boatsHitBool playerShot cC && F.endGameCheck cC newGameState then do
                 putStrLn (name ++" Wins!!!")
                 main
           else do 
                  computerShot <- F.toTuple (unsafePerformIO (getStdRandom (randomR (1, 5)))) (unsafePerformIO (getStdRandom (randomR (1, 5)))) :: IO [F.Coordinates]
                  if F.validMove computerShot then
                     do newPlayerGameState <- F.hitMissGrid computerShot playerBoats playerGameState 
                        if F.boatsHitBool computerShot playerBoats then do
                           putStrLn "Computer shot at your grid and hit!"
                           if F.endGameCheck playerBoats newPlayerGameState then do 
                                putStrLn "Computer wins!"
                                main
                           else game newGameState newPlayerGameState playerBoats cC name
                        else putStrLn "Computer shot at your grid and missed!" 
                        game newGameState newPlayerGameState playerBoats cC name
                  else game playerGameState newGameState playerBoats cC name
     else putStrLn "Invalid move, you must shoot at an unknown coordinate on the grid"
     game computerGameState playerGameState playerBoats cC name


{- printGameState 
     A function that prints out the current gamestate after every round
     PRE:-
     RETURNS:-
     SIDE EFFECTS: Prints the grids for the player to see
     EXAMPLES:-
-}
printGameState :: F.Grid -> F.Grid -> IO ()
printGameState (x:y:z:l:m) (a:b:c:d:e) = do
  putStrLn ""
  putStrLn "Computer grid:"
  print x 
  print y 
  print z
  print l
  putStrLn $ showLastList m
  putStrLn ""
  putStrLn "Player Grid:"
  print a 
  print b 
  print c 
  print d 
  putStrLn $ showLastList e
  putStrLn ""
  

{- showLastList
     A function that ensures that the last row of the grid is printed out correctly in the console.
     PRE:-
     RETURNS: A string, in our case its used to represent a row in our grid. 
     SIDE EFFECTS:-
     EXAMPLES:
     showLastList [] == ""
     showLastList ["hello"] == "\"hello\""
     showLastList [1,2,3,4,5] == "1 2 3 4 5"
-}
showLastList :: Show a => [a] -> String
showLastList = intercalate " " . map show


{-validInput 
    Checks that an input is formulated correctly.
    PRE:-
    RETURNS:-
    SIDE EFFECTS:-
    EXAMPLES: validInput [(2,3)] == True
              validInput (2,3) == False
              validInput [(2,3),(3,4)] == False
    CREDIT: Taken from Lab 15 and slightly modified to fit our program.
-}
validInput :: IO [F.Coordinates]
validInput = do
  catch (do
    line <- getLine 
    evaluate (read line))  
    ((\_ -> do   
       putStrLn "Wrong Input! Correct format is: [(Row,Column)]"
       validInput) :: SomeException -> IO [F.Coordinates])


-- TEST CASES --

-- areValid tests
test1 :: Test 
test1 = TestCase $ assertEqual "areValid [(1,1),(1,2),(1,3)]" True (areValid [(1,1),(1,2),(1,3)])

test2 :: Test 
test2 = TestCase $ assertEqual "areValid [(2,8),(1,2),(1,3)]" False (areValid [(2,8),(1,2),(1,3)])

test3 :: Test
test3 = TestCase $ assertEqual "areValid []" False  (areValid [])

-- validMove tests
test4 :: Test 
test4 = TestCase $ assertEqual "validMove [(1,1)]" True (validMove [(1,1)])

test5 :: Test 
test5 = TestCase $ assertEqual "validMove [(7,2),(1,2),(1,3)]" False (validMove [(7,2),(1,2),(1,3)])

test6 :: Test 
test6 = TestCase $ assertEqual "validMove (boatsHit [(1,8),(2,1)] [(1,1),(1,8)])" False (validMove (boatsHit [(1,8),(2,1)] [(1,1),(1,8)]))

test7 :: Test 
test7 = TestCase $ assertEqual "validMove (boatsHit [(1,1),(2,1)] [(1,1),(1,5)])" True (validMove (boatsHit [(1,1),(2,1)] [(1,1),(1,5)]))

-- endGameCheck tests
test8 :: Test 
test8 = TestCase $ assertEqual "endGameCheck [(1,1),(1,2),(1,3)] [xxx**,*o***,*ooo*,*****,*****]" True (endGameCheck [(1,1),(1,2),(1,3)] ["xxx**","*o***","*ooo*","*****","*****"])

test9 :: Test 
test9 = TestCase $ assertEqual "endGameCheck [(1,1),(1,2),(1,3),(1,4),(1,5)] [**xxx,*****,*****,*****,*****]" False (endGameCheck [(1,1),(1,2),(1,3),(1,4),(1,5)] ["**xxx","*****","*****","*****","*****"])

test10 :: Test
test10 = TestCase $ assertEqual "endGameCheck [(5,1),(5,2),(5,3),(5,4),(5,5)] [*****,*o***,*ooo*,*****,xxxxx]" True (endGameCheck [(5,1),(5,2),(5,3),(5,4),(5,5)] ["*****","*o***","*ooo*","*****","xxxxx"])

-- boatsHit tests
test11 :: Test 
test11 = TestCase $ assertEqual "boatsHit [(1,1),(1,2)] [(1,2),(1,3)]" [(1,2)] (boatsHit [(1,1),(1,2)] [(1,2),(1,3)])

test12 :: Test 
test12 = TestCase $ assertEqual "boatsHit [(1,1)] [(1,1)]" [(1,1)] (boatsHit [(1,1)] [(1,1)])

test13 :: Test
test13 = TestCase $ assertEqual "boatsHit [(1,1),(1,2)] [(2,1),(2,2)]" [] (boatsHit [(1,1),(1,2)] [(2,1),(2,2)])

-- TO RUN ALL TESTS: WRITE "runTestTT tests" IN GHCi --

tests :: Test
tests = TestList [TestLabel "test1" test1,TestLabel "test2" test2,TestLabel "test3" test3,TestLabel "test4" test4,TestLabel "test5" test5,TestLabel "test6" test6,TestLabel "test7" test7,TestLabel "test8" test8,TestLabel "test9" test9,TestLabel "test10" test10,TestLabel "test11" test11,TestLabel "test12" test12,TestLabel "test13" test13] 


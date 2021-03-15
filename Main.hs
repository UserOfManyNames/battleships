module Main where
import Control.Exception (SomeException, evaluate, catch)
import Data.List (intercalate, delete, nub)
import System.Exit (exitSuccess)
import System.Random (getStdRandom, randomR)
import Test.HUnit ( runTestTT,assertEqual, Test(..) )
import System.IO.Unsafe (unsafePerformIO)
import Functions as F 

-- A GAME BY: Katarina Feldtmann, Yasmine Odelbrink and Christian Ocklind --

{- main
     A function that runs the game and determines it's variables.
     SIDE EFFECTS: Runs the game 
-}
main :: IO ()
main = do
   putStrLn $ "\x1b[36m" ++ "Do you want to play a game?" ++ "\x1b[33m" ++ " y/n" ++ "\x1b[0m"
   answer <- getLine
   if answer == "y" then do
      putStrLn $ "\x1b[36m" ++ "Welcome to Battleships!" ++ "\x1b[33m" ++ " Whats your Name?" ++ "\x1b[0m"
      name <- getLine
      putStrLn $ "\x1b[36m" ++ "Hi " ++ name ++ "! You will play on a 5x5 grid and fire your shots using (Row,Column) coordinates against the computer. 'x' is a hit and 'o' is a miss!" 
      computerGameState <- F.createGrid
      playerGrid <- F.createGrid
      putStrLn $ "\x1b[33m" ++ "Choose your 5 boat coordinates:" ++ "\x1b[0m"
      playerBoats <- validInput
      let usedCompCoords = [] :: [F.Coordinates]
      let usedPlayerCoords = [] :: [F.Coordinates] 
      if F.validBoats playerBoats then do 
        putStrLn $ ""
        putStrLn $ "\x1b[36m" ++ "Your boats have been stationed at " ++ show playerBoats ++ "!" ++ "\x1b[0m"
        let compGetBoats n = if F.duplicateCoord (unsafePerformIO n) then n else compGetBoats (F.toTupleList (unsafePerformIO (getStdRandom (randomR (1, 5)))) (unsafePerformIO (getStdRandom (randomR (1, 5)))) (unsafePerformIO (getStdRandom (randomR (1, 5)))) (unsafePerformIO (getStdRandom (randomR (1, 5))))(unsafePerformIO (getStdRandom (randomR (1, 5))))(unsafePerformIO (getStdRandom (randomR (1, 5)))) (unsafePerformIO (getStdRandom (randomR (1, 5))))(unsafePerformIO (getStdRandom (randomR (1, 5)))) (unsafePerformIO (getStdRandom (randomR (1, 5)))) (unsafePerformIO (getStdRandom (randomR (1, 5)))) :: IO [F.Coordinates]) 
        cC <- compGetBoats (F.toTupleList (unsafePerformIO (getStdRandom (randomR (1, 5)))) (unsafePerformIO (getStdRandom (randomR (1, 5)))) (unsafePerformIO (getStdRandom (randomR (1, 5)))) (unsafePerformIO (getStdRandom (randomR (1, 5))))(unsafePerformIO (getStdRandom (randomR (1, 5))))(unsafePerformIO (getStdRandom (randomR (1, 5)))) (unsafePerformIO (getStdRandom (randomR (1, 5))))(unsafePerformIO (getStdRandom (randomR (1, 5)))) (unsafePerformIO (getStdRandom (randomR (1, 5)))) (unsafePerformIO (getStdRandom (randomR (1, 5)))) :: IO [F.Coordinates]) 
        let compBoatLoop = if F.duplicateCoord cC then do displayBoats computerGameState playerGrid playerBoats cC name usedCompCoords usedPlayerCoords else compBoatLoop
        compBoatLoop 
      else putStrLn $ "\x1b[31m" ++ "Wrong Input! " ++ "\x1b[33m" ++ "Correct format is: [(Row,Column),(Row,Column),(Row,Column),(Row,Column),(Row,Column)] and no duplicates are allowed!" ++ "\x1b[0m"
      main      
   else exitSuccess


{- game
     Plays the actual game 
     SIDE EFFECTS: Allows the player to interacts with the game. 
-}
game :: F.Grid -> F.Grid -> [F.Coordinates] -> [F.Coordinates] -> F.Player -> [F.Coordinates] -> [F.Coordinates] -> IO ()
game computerGameState playerGameState playerBoats cC name usedCompCoords usedPlayerCoords = do
     printGameState computerGameState playerGameState name 
     putStrLn $ "\x1b[33m" ++ "FIRE!" ++ "\x1b[0m"  
     playerShot <- validInputShot
     if (F.isValid playerShot) && (F.usedCoords playerShot usedPlayerCoords) then
        do newGameState <- F.hitMissGrid playerShot cC computerGameState
           let newUsedPlayerCoords = F.addToList playerShot usedPlayerCoords
           if F.boatsHitBool playerShot cC && F.endGameCheck cC newGameState then do
                 putStrLn $ "\x1b[33m" ++ name ++" Wins!!!" ++ "\x1b[0m" 
                 main
           else do 
                  let compGetShot n = if (F.usedCoords (unsafePerformIO n) usedCompCoords) then n else compGetShot (F.toTuple (unsafePerformIO (getStdRandom (randomR (1, 5)))) (unsafePerformIO (getStdRandom (randomR (1, 5)))) :: IO F.Coordinates)
                  computerShot <- compGetShot (F.toTuple (unsafePerformIO (getStdRandom (randomR (1, 5)))) (unsafePerformIO (getStdRandom (randomR (1, 5)))) :: IO F.Coordinates)
                  let compShotValidLoop = if (F.usedCoords computerShot usedCompCoords) then True else compShotValidLoop
                  if (F.isValid computerShot) && compShotValidLoop then do
                        let newUsedCompCoords = F.addToList computerShot usedCompCoords
                        newPlayerGameState <- F.hitMissGrid computerShot playerBoats playerGameState 
                        if F.boatsHitBool computerShot playerBoats then do
                           putStrLn $ "\x1b[33m" ++ "Computer shot at your grid and hit your boat at " ++ show computerShot ++ "!" ++ "\x1b[0m"
                           if F.endGameCheck playerBoats newPlayerGameState then do 
                                putStrLn $ "\x1b[31m" ++ "Computer wins!" ++ "\x1b[0m"
                                main
                           else game newGameState newPlayerGameState playerBoats cC name newUsedCompCoords newUsedPlayerCoords
                        else putStrLn $ "\x1b[33m" ++ "Computer shot at your grid and missed!" ++ "\x1b[0m"
                        game newGameState newPlayerGameState playerBoats cC name newUsedCompCoords newUsedPlayerCoords
                  else game newGameState playerGameState playerBoats cC name usedCompCoords newUsedPlayerCoords
     else putStrLn $ "\x1b[31m" ++ "Invalid move, you must shoot at an unknown coordinate on the grid!" ++ "\x1b[0m"
     game computerGameState playerGameState playerBoats cC name usedCompCoords usedPlayerCoords


{- printGameState 
     A function that prints out the current gamestate after every round
     SIDE EFFECTS: Prints the grids for the player to see
-}
printGameState :: F.Grid -> F.Grid -> F.Player -> IO ()
printGameState (x:y:z:l:m:_) (a:b:c:d:e:_) name = do
  putStrLn $ ""
  putStrLn $ ""
  putStrLn $ "\x1b[31m" ++ "Computer Grid:"
  putStrLn $ id x 
  putStrLn $ id y 
  putStrLn $ id z
  putStrLn $ id l
  putStrLn $ id m
  putStrLn $ "" ++ "\x1b[0m"
  putStrLn $ id "\x1b[32m" ++ name ++ "'s Grid:"
  putStrLn $ id a 
  putStrLn $ id b 
  putStrLn $ id c 
  putStrLn $ id d 
  putStrLn $ id e
  putStrLn $ "" ++ "\x1b[0m"


{- displayBoats
     A function that makes it possible for the player to see it's own boats in the game function. It does it by changing the coordinates on the input grid according to the chosen boats.
     SIDE EFFECTS: Displays that the grid is being changed and that it has been updated successfully.
-}
displayBoats :: F.Grid -> F.Grid -> [F.Coordinates] -> [F.Coordinates] -> F.Player -> [F.Coordinates] -> [F.Coordinates] -> IO ()
displayBoats computerGameState playerGrid playerBoats cC name usedCompCoords usedPlayerCoords = do
  putStrLn $ "\x1b[33m" ++ "Displaying boats..." 
  playerGameState <- return (F.replaceValuesList playerBoats '~' playerGrid)
  putStrLn $ "Grid Updated!" ++ "\x1b[0m"
  game computerGameState playerGameState playerBoats cC name usedCompCoords usedPlayerCoords
  
{-validInput 
    Checks that an input is formulated correctly.
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
       putStrLn $ "\x1b[31m" ++ "Wrong Input! " ++ "\x1b[33m" ++ "Correct format is: [(Row,Column)]" ++ "\x1b[0m"
       validInput) :: SomeException ->  IO [F.Coordinates])


{-validInputShot 
    Checks that the input for the players shot is formulated correctly.
    EXAMPLES: validInputShot (2,3) == True
              validInputShot [(2,3)] == False
              validInputShot (2,3),(3,4) == False
    CREDIT: Taken from Lab 15 and slightly modified to fit our program.
-}
validInputShot :: IO F.Coordinates
validInputShot = do
  catch (do
    line <- getLine 
    evaluate (read line))  
    ((\_ -> do   
       putStrLn $ "\x1b[31m" ++ "Wrong Input! " ++ "\x1b[33m" ++ "Correct format is: (Row,Column)" ++ "\x1b[0m"
       validInputShot) :: SomeException -> IO F.Coordinates)


-- TEST CASES --

-- areValid tests
test1 :: Test 
test1 = TestCase $ assertEqual "areValid [(1,1),(1,2),(1,3)]" True (areValid [(1,1),(1,2),(1,3)])

test2 :: Test 
test2 = TestCase $ assertEqual "areValid [(2,8),(1,2),(1,3)]" False (areValid [(2,8),(1,2),(1,3)])

test3 :: Test
test3 = TestCase $ assertEqual "areValid []" False  (areValid [])

-- isValid tests
test4 :: Test 
test4 = TestCase $ assertEqual "isValid (1,1)" True (isValid (1,1))

test5 :: Test 
test5 = TestCase $ assertEqual "isValid (7,2)" False (isValid (7,2))

test6 :: Test 
test6 = TestCase $ assertEqual "isValid (1,8)" False (isValid (1,8))

test7 :: Test 
test7 = TestCase $ assertEqual "isValid (2,1)" True (isValid (2,1))

-- endGameCheck tests
test8 :: Test 
test8 = TestCase $ assertEqual "endGameCheck [(1,1),(1,2),(1,3)] [xxx**,*o***,*ooo*,*****,*****]" True (endGameCheck [(1,1),(1,2),(1,3)] ["xxx**","*o***","*ooo*","*****","*****"])

test9 :: Test 
test9 = TestCase $ assertEqual "endGameCheck [(1,1),(1,2),(1,3),(1,4),(1,5)] [**xxx,*****,*****,*****,*****]" False (endGameCheck [(1,1),(1,2),(1,3),(1,4),(1,5)] ["**xxx","*****","*****","*****","*****"])

test10 :: Test
test10 = TestCase $ assertEqual "endGameCheck [(5,1),(5,2),(5,3),(5,4),(5,5)] [*****,*o***,*ooo*,*****,xxxxx]" True (endGameCheck [(5,1),(5,2),(5,3),(5,4),(5,5)] ["*****","*o***","*ooo*","*****","xxxxx"])

-- validBoats tests
test11 :: Test 
test11 = TestCase $ assertEqual "validBoats [(1,1)]" False (validBoats [(1,1)])

test12 :: Test 
test12 = TestCase $ assertEqual "validBoats [(1,1),(1,2),(1,2),(1,3),(1,4)]" False (validBoats [(1,1),(1,2),(1,2),(1,3),(1,4)])

test13 :: Test
test13 = TestCase $ assertEqual "validBoats [(1,1),(1,2),(1,3),(1,4),(1,5)]" True (validBoats [(1,1),(1,2),(1,3),(1,4),(1,5)])

-- usedCoords tests
test14 :: Test
test14 = TestCase $ assertEqual "usedCoords (1,2) [(1,1)]" True (usedCoords (1,2) [(1,1)])

test15 :: Test
test15 = TestCase $ assertEqual "usedCoords (3,4) [(3,4),(1,2)]" False (usedCoords (3,4) [(3,4),(1,2)])

test16 :: Test
test16 = TestCase $ assertEqual "usedCoords (5,5) [(1,3),(2,4),(5,5)]" False (usedCoords (5,5) [(1,3),(2,4),(5,5)])

-- replaceVal tests
test17 :: Test
test17= TestCase $ assertEqual "replaceVal (5,1) 'x' [*****,*****,*****,*****,*****]" ["*****","*****","*****","*****","x****"] (replaceVal (5,1) 'x' ["*****","*****","*****","*****","*****"])

test18 :: Test
test18= TestCase $ assertEqual "replaceVal (5,5) '~' [*****,*****,*****,*****,*****]" ["*****","*****","*****","*****","****~"] (replaceVal (5,5) '~' ["*****","*****","*****","*****","*****"])

test19 :: Test
test19= TestCase $ assertEqual "replaceVal (3,1) 'o' [*****,*****,*****,*****,*****]" ["*****","*****","o****","*****","*****"] (replaceVal (3,1) 'o' ["*****","*****","*****","*****","*****"])

test20 :: Test
test20= TestCase $ assertEqual "replaceVal (3,5) 'x' [*****,*****,*****,*****,*****]" ["*****","*****","****x","*****","*****"] (replaceVal (3,5) 'x' ["*****","*****","*****","*****","*****"])

-- TO RUN ALL TESTS: WRITE "runtests" IN GHCi --
runtests = runTestTT $ TestList [test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11,test12,test13,test14,test15,test16,test17,test18,test19,test20]

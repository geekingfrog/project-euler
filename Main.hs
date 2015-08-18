import System.Environment (getArgs)

import Problem001 as P1
import Problem002 as P2
import Problem003 as P3
import Problem004 as P4
import Problem005 as P5
import Problem006 as P6
import Problem007 as P7
import Problem008 as P8
import Problem009 as P9
import Problem010 as P10
import Problem011 as P11
import Problem012 as P12
import Problem013 as P13
import Problem014 as P14

main :: IO ()
main = do
  args <- getArgs
  case args of
    [x] -> putStrLn $ runSolution $ read x
    _ -> print usage

usage = "cabal run 6"

runSolution :: Int -> String
runSolution 1 = show P1.answer
runSolution 2 = show P2.answer
runSolution 3 = show P3.answer
runSolution 4 = show P4.answer
runSolution 5 = show P5.answer
runSolution 6 = show P6.answer
runSolution 7 = show P7.answer
runSolution 8 = show P8.answer
runSolution 9 = show P9.answer
runSolution 10 = show P10.answer
runSolution 11 = show P11.answer
runSolution 12 = show P12.answer
runSolution 13 = show P13.answer
runSolution 14 = show P14.answer
runSolution _ = undefined

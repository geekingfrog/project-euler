import System.Environment (getArgs)

import Problem003 as P3
import Problem004 as P4
import Problem005 as P5
import Problem006 as P6
import Problem007 as P7
import Problem008 as P8
import Problem009 as P9
import Problem010 as P10
import Problem011 as P11

main :: IO ()
main = do
  args <- getArgs
  case args of
    [x] -> putStrLn $ runSolution $ read x
    _ -> print usage

usage = "cabal run 6"

runSolution :: Int -> String
runSolution 3 = show P3.answer
runSolution 4 = show P4.answer
runSolution 5 = show P5.answer
runSolution 6 = show P6.answer
runSolution 7 = show P7.answer
runSolution 8 = show P8.answer
runSolution 9 = show P9.answer
runSolution 10 = show P10.answer
runSolution 11 = show P11.answer
runSolution _ = undefined

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
import Problem015 as P15
import Problem016 as P16
import Problem017 as P17
import Problem018 as P18

main :: IO ()
main = do
  args <- getArgs
  case args of
    [x] -> (runSolution $ read x) >>= putStrLn
    _ -> print usage

usage :: String
usage = "cabal run 6"

runSolution :: Int -> IO String
runSolution 1 = return $ show P1.answer
runSolution 2 = return $ show P2.answer
runSolution 3 = return $ show P3.answer
runSolution 4 = return $ show P4.answer
runSolution 5 = return $ show P5.answer
runSolution 6 = return $ show P6.answer
runSolution 7 = return $ show P7.answer
runSolution 8 = return $ show P8.answer
runSolution 9 = return $ show P9.answer
runSolution 10 = return $ show P10.answer
runSolution 11 = return $ show P11.answer
runSolution 12 = return $ show P12.answer
runSolution 13 = return $ show P13.answer
runSolution 14 = return $ show P14.answer
runSolution 15 = return $ show P15.answer
runSolution 16 = return $ show P16.answer
runSolution 17 = return $ show P17.answer
runSolution 18 = return $ show P18.answer
runSolution _ = undefined

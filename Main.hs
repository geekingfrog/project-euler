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
import Problem019 as P19
import Problem020 as P20
import Problem021 as P21
import Problem022 as P22
import Problem023 as P23
import Problem024 as P24
import Problem025 as P25
import Problem026 as P26
import Problem027 as P27
import Problem028 as P28
import Problem029 as P29
import Problem030 as P30
import Problem031 as P31
import Problem067 as P67

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
runSolution 19 = return $ show P19.answer
runSolution 20 = return $ show P20.answer
runSolution 21 = return $ show P21.answer
runSolution 22 = P22.answer >>= (return . show)
runSolution 23 = return $ show P23.answer
runSolution 24 = return $ show P24.answer
runSolution 25 = return $ show P25.answer
runSolution 26 = return $ show P26.answer
runSolution 27 = return $ show P27.answer
runSolution 28 = return $ show P28.answer
runSolution 29 = return $ show P29.answer
runSolution 30 = return $ show P30.answer
runSolution 31 = return $ show P31.answer
runSolution 67 = P67.answer >>= (return . show)
runSolution _ = undefined

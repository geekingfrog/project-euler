module Problem019 (answer) where

-- I could compute manually the answer in a pure fashion but this will
-- be an exercise in using the date/time facilities of haskell instead

import Data.Time.Calendar (fromGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)

answer :: Int
answer = length [1 | y <- [1901..2000], m <- [1..12], isSunday y m 1]

isSunday :: Integer -> Int -> Int -> Bool
isSunday year month day = 7 == dayOfWeek
  where
    calendarDay = fromGregorian year month day
    dayOfWeek = third $ toWeekDate calendarDay
    third (_,_,c) = c


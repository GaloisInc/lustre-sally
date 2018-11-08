-- | Here we parse out progress messages from sally
module SallyProgress where

import Data.Char(isSpace)
import Text.Read(readMaybe)
import Data.Time(LocalTime,parseTimeM,defaultTimeLocale)
import Control.Monad(msum)

getNotes :: String -> ([(LocalTime,Int)], String)
getNotes cs =
  case cs of
    [] -> ([],[])
    _ | all isSpace l -> getNotes rest
      | otherwise ->
         case infoLine l of
           Nothing -> ([],cs)
           Just (t,mb) ->
              case mb of
                Nothing -> getNotes rest
                Just n -> let (more,fin) = getNotes rest
                          in ((t,n):more,fin)
  where (l,rest0) = break (== '\n') cs
        rest = drop 1 rest0

infoLine :: String -> Maybe (LocalTime,Maybe Int)
infoLine xs = case break isSpace xs of
                (stamp,rest) -> do t <- time stamp
                                   pure (t, number rest)

number :: String -> Maybe Int
number = msum . map readMaybe . words

time :: String -> Maybe LocalTime
time = parseTimeM True defaultTimeLocale format
  where
  format = "[%F.%T]"


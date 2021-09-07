module Time where

import Data.Word 
import Data.Int 
import Data.Time.Clock 
import Data.Time.Clock.POSIX 
import Data.Time.Format 

-- | MIGHT BE OFF BY LEAP SECONDS
convertTime :: Word64 -> UTCTime
convertTime = posixSecondsToUTCTime . realToFrac . diffPosix . fromWindowsTick 
  where 
    fromWindowsTick x = x `div` 10000000
    diffPosix x = x -  11644473600

prettyTime :: UTCTime -> String 
prettyTime u = formatTime defaultTimeLocale rfc822DateFormat u

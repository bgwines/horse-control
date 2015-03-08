-- | Utility functions
module Horse.Utils
( -- * conversions
  eitherToMaybe
) where

-- | Lossily convert, discarding the error object
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left e) = Nothing
eitherToMaybe (Right x) = Just x
-- | Utility functions
module Horse.Utils
( -- * conversions
  eitherToMaybe
, maybeToEither

, stringToHash

-- * combinators
, (|$|)
, (|<$>|)
) where

import Horse.Types

import Control.Applicative ((<$>))

import Data.ByteString (pack)

import qualified Data.Convertible as Convert

--import Horse.IO (loadHead)

(|$|) :: Maybe (a -> a) -> a -> a
(Nothing) |$| x = x
(Just f) |$| x = f x

(|<$>|) :: (Functor f) => Maybe (a -> a) -> f a -> f a
(Nothing) |<$>| x = x
(Just f) |<$>| x = f <$> x

-- | Lossily convert, discarding the error object
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

maybeToEither :: Maybe b -> Either Error b
maybeToEither Nothing = Left "Maybe conversion was from Nothing"
maybeToEither (Just x) = Right x

-- | Conversion function for hashes
stringToHash :: String -> Hash
stringToHash = pack  . map Convert.convert

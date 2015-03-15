-- | Utility functions
module Horse.Utils
( -- * conversions
  eitherToMaybe
, maybeToEither

, stringToHash

-- * general
, putStrLn'

-- * combinators
, (|$|)
, (|<$>|)
) where

import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)

import Horse.Types

import Control.Applicative ((<$>))

import Data.ByteString (pack)

import qualified Data.Convertible as Convert

-- | Like <$>, but where the function is contained in
-- | a functor instead of the argument (in this case,
-- | restricted to Maybe, not all functors). If the
-- | maybe function is `Nothing`, then no application
-- | happens. otherwise, we apply the function
(|$|) :: Maybe (a -> a) -> a -> a
(Nothing) |$| x = x
(Just f) |$| x = f x

-- | Like `(|$|)`, but where the argument to the function
-- | is wrapped in a functor
(|<$>|) :: (Functor f) => Maybe (a -> a) -> f a -> f a
(Nothing) |<$>| x = x
(Just f) |<$>| x = f <$> x

-- | `putStrLn` but for the `EitherT` monad
putStrLn' :: String -> EitherT a IO ()
putStrLn' = liftIO . putStrLn

-- | Lossily convert, discarding the error object
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

-- | Convert, with a error message to be used if the Maybe is Nothing
maybeToEither :: Error -> Maybe b -> Either Error b
maybeToEither error Nothing = Left error
maybeToEither _ (Just x) = Right x

-- | Conversion function for hashes
stringToHash :: String -> Hash
stringToHash = pack  . map Convert.convert

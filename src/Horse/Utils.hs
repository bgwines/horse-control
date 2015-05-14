-- | Utility functions used by the implementation.
module Horse.Utils
( -- * conversions
  maybeToEither
, fromEitherMaybeDefault
--, stringToHash
, hashToString

-- * general
, iterateMaybe
, toMaybe
, whenM
, unlessM

-- * combinators
, (|<$>|)
, (</>)
) where

import Prelude hiding (print, putStrLn)
import qualified Prelude (print)

import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)

import Horse.Types

import Control.Applicative ((<$>))

import Data.ByteString (pack, unpack)

import qualified Data.Default as Default

import qualified Data.Convertible as Convert

-- * conversions

-- | Convert, with a error message to be used if the `Maybe` is `Nothing`.
maybeToEither :: Error -> Maybe b -> Either Error b
maybeToEither error Nothing = Left error
maybeToEither _ (Just x) = Right x

-- | Pick (in decreasing order of preference) if exists: the `Right` value
--   in the `Either`, the `Just` value in the `Maybe` or the default value
--   for the type.
fromEitherMaybeDefault :: (Default.Default b) => Either a b -> Maybe b -> b
fromEitherMaybeDefault (Left  _) Nothing  = Default.def
fromEitherMaybeDefault (Right x) Nothing  = x
fromEitherMaybeDefault (Left  _) (Just y) = y
fromEitherMaybeDefault (Right _) (Just y) = y

-- | Conversion function for hashes.
--stringToHash :: String -> Hash
--stringToHash = pack . map Convert.convert

-- | Conversion function for hashes (other direction).
hashToString :: Hash -> String
hashToString = map Convert.convert . unpack

-- * general

-- | Wrap in a `Just` if the predicate function returns `True`
toMaybe :: a -> (a -> Bool) -> Maybe a
toMaybe a f = if f a
    then Just a
    else Nothing

-- | `iterate` until the "next" function returns a `Nothing`.
iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f curr = case f curr of
    Nothing -> []
    (Just next) -> (:) next $ iterateMaybe f next

-- | Monadic `when`.
whenM :: Monad m => m Bool -> m () -> m ()
whenM mCond action = mCond >>= (`when` action)

-- | Monadic `unless`.
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mCond action = mCond >>= (`unless` action)

-- * combinators

-- | Most easily understood from its implementation:
--
--     > (Nothing) |<$>| x = x
--     > (Just f) |<$>| x = f <$> x
(|<$>|) :: (Functor f) => Maybe (a -> a) -> f a -> f a
(Nothing) |<$>| x = x
(Just f) |<$>| x = f <$> x

-- | Concatenate two filepaths. E.g.:
--  
--       > λ "a/b" </> "c"
--       > "a/b/c"
(</>) :: FilePath -> FilePath -> FilePath
a </> b = a ++ "/" ++ b

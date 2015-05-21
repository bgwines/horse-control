-- | Utility functions used by the implementation.
module Horse.Utils
( -- * conversions
  note
, hush
, fromEitherMaybeDefault
--, stringToHash
, hashToString

-- * general
, iterateMaybe
, toMaybe
, hushT

-- * combinators
, (|<$>|)
, (</>)
) where

import Prelude hiding (print, putStrLn)
import qualified Prelude (print)

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)

import Horse.Types

import Control.Applicative ((<$>))

import Data.ByteString (pack, unpack)

import qualified Data.Default as Default

import qualified Data.Convertible as Convert

-- * conversions

-- 'hush' for the respective monad transformers.
hushT :: (Monad m) => EitherT l m r -> MaybeT m r
hushT = MaybeT . liftM hush . runEitherT

-- | Convert, with a error message to be used if the `Maybe` is `Nothing`.
note :: Error -> Maybe b -> Either Error b
note error Nothing = Left error
note _ (Just x) = Right x

-- | Convert, supressing the error message.
hush :: Either l r -> Maybe r
hush (Left _) = Nothing
hush (Right r) = Just r

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

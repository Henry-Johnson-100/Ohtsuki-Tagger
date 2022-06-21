{- |
Module      : Text.QueryParser
Description : Contains Utilities for extracting a TaggerQL syntax tree out of a raw query.

License     : GPL-3
Maintainer  : monawasensei@gmail.com

The TaggerQL syntax tree \*should\* be traversable or at least a functor so that
the raw patterns inside of it can be read from the database.
-}
module Text.QueryParser (
  Term (..),
  Phrase (..),
) where

import Text.QueryParser.Internal
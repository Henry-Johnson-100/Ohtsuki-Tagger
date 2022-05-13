{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Event.Parser
  ( PseudoDescriptor (..),
    PseudoSubTag (..),
    ParseError (..),
    QuerySection (..),
    SetArithmeticLiteral (..),
    QueryCriteriaLiteral (..),
    SubList (..),
    parseQuery,
    parseQuerySections,
    pseudoDescriptorText,
    mapMQuerySection,
    mapMSubList,
    mapMQueryToken,
    combineQuerySections,
  )
where

import Data.Functor.Identity (Identity)
import qualified Data.Text as T
import Text.Parsec
  ( ParseError,
    ParsecT,
    char,
    eof,
    lookAhead,
    many,
    many1,
    manyTill,
    noneOf,
    oneOf,
    parse,
    spaces,
    try,
    (<?>),
    (<|>),
  )
import Type.Model.Prim (FileSetArithmetic (..), Intersectable (..), QueryCriteria (..))

parseQuery :: T.Text -> Either ParseError [PseudoSubTag]
parseQuery = parse (spaces' >> pseudoQueryParser) "Query"

parseQuerySections ::
  T.Text -> Either ParseError [QuerySection (SubList (QueryToken PseudoDescriptor))]
parseQuerySections = parse (spaces' >> querySectionParser) "Query"

-- | A newtype wrapper around Data.Text.Text
newtype PseudoDescriptor = PDescriptor T.Text deriving (Show, Eq)

pseudoDescriptorText :: PseudoDescriptor -> T.Text
pseudoDescriptorText (PDescriptor t) = t

-- | (PseudoDescriptor, [PseudoDescriptor])
type PseudoSubTag = (PseudoDescriptor, [PseudoDescriptor])

data SubList a = SubList
  { subListHead :: !a,
    subListContents :: ![a]
  }
  deriving (Show, Eq, Functor)

mapMSubList :: Monad m => (t -> m a) -> SubList t -> m (SubList a)
mapMSubList f (SubList h ts) = do
  h' <- f h
  ts' <- mapM f ts
  return $ SubList h' ts'

type TextFieldParser a = ParsecT T.Text () Identity a

data SetArithmeticLiteral = ALiteral !FileSetArithmetic | ANoLiteral deriving (Show, Eq)

data QueryCriteriaLiteral = CLiteral !QueryCriteria | CNoLiteral deriving (Show, Eq)

data QuerySection a = QuerySection
  { sectionSetArithmetic :: !SetArithmeticLiteral,
    sectionContents :: ![a]
  }
  deriving (Show, Eq, Functor)

instance Foldable QuerySection where
  foldr f acc (QuerySection _ xs) = foldr f acc xs

mapMQuerySection :: Monad m => (a1 -> m a2) -> QuerySection a1 -> m (QuerySection a2)
mapMQuerySection fm (QuerySection a xs) = do
  xs' <- mapM fm xs
  return (QuerySection a xs')

data QueryToken a = QueryToken
  { tokenCriteria :: !QueryCriteriaLiteral,
    tokenContents :: !a
  }
  deriving (Show, Eq, Functor)

mapMQueryToken :: Monad m => (t -> m a) -> QueryToken t -> m (QueryToken a)
mapMQueryToken f (QueryToken c x) = do
  x' <- f x
  return $ QueryToken c x'

-- | Left associative combination of query sections
--
-- Applies the set arithmetic of the first to the set combination function of the two.
--
-- Resulting QuerySection the right set arithmetic
combineQuerySections ::
  ([a] -> [a] -> [a]) ->
  (a -> a -> Bool) ->
  QuerySection a ->
  QuerySection a ->
  QuerySection a
combineQuerySections defaultCombo f (QuerySection xa xs) (QuerySection ya ys) =
  case xa of
    ALiteral Union -> QuerySection ya (unionBy f xs ys)
    ALiteral Intersect -> QuerySection ya (intersectBy f xs ys)
    ALiteral Diff -> QuerySection ya (diffBy f xs ys)
    ANoLiteral -> QuerySection ya $ defaultCombo xs ys

setArithmeticLiteralParser :: TextFieldParser SetArithmeticLiteral
setArithmeticLiteralParser =
  unionParser
    <|> intersectParser
    <|> diffParser
  where
    unionParser :: TextFieldParser SetArithmeticLiteral
    unionParser = do
      oneOf "uU"
      char '|'
      return . ALiteral $ Union
    intersectParser :: TextFieldParser SetArithmeticLiteral
    intersectParser = do
      oneOf "iI"
      char '|'
      return . ALiteral $ Intersect
    diffParser :: TextFieldParser SetArithmeticLiteral
    diffParser = do
      oneOf "dD"
      char '|'
      return . ALiteral $ Diff
    noLiteralParser :: TextFieldParser SetArithmeticLiteral
    noLiteralParser = return ANoLiteral

queryCriteriaParser :: TextFieldParser QueryCriteriaLiteral
queryCriteriaParser =
  byTagParser
    <|> byRelationParser
    <|> byPatternParser
    <|> noLiteralParser
  where
    byTagParser :: TextFieldParser QueryCriteriaLiteral
    byTagParser = do
      oneOf "tT"
      char '.'
      return . CLiteral $ ByTag
    byRelationParser :: TextFieldParser QueryCriteriaLiteral
    byRelationParser = do
      oneOf "rR"
      char '.'
      return . CLiteral $ ByRelation
    byPatternParser :: TextFieldParser QueryCriteriaLiteral
    byPatternParser = do
      oneOf "pP"
      char '.'
      return . CLiteral $ ByPattern
    noLiteralParser :: TextFieldParser QueryCriteriaLiteral
    noLiteralParser = return CNoLiteral

{-
t.otsuki_yui {r.gym_clothes p.%yui_otsuki% } u| r.machikado_mazoku_characters {t.smile} d| lilith_statue
-}

querySectionParser ::
  TextFieldParser [QuerySection (SubList (QueryToken PseudoDescriptor))]
querySectionParser = do
  h <- querySectionHeadParser
  spaces'
  t <- many querySectionTailParser
  return $ h : t
  where
    querySectionHeadParser ::
      TextFieldParser (QuerySection (SubList (QueryToken PseudoDescriptor)))
    querySectionHeadParser = do
      spaces'
      a <- setArithmeticLiteralParser <|> return ANoLiteral
      spaces'
      sls <-
        manyTillNoConsume
          queryTokenEitherParser
          (eofNoLiteralParser <|> setArithmeticLiteralParser)
      return $ QuerySection a sls

    querySectionTailParser ::
      TextFieldParser
        (QuerySection (SubList (QueryToken PseudoDescriptor)))
    querySectionTailParser = do
      spaces'
      a <- setArithmeticLiteralParser
      spaces'
      sls <-
        manyTillNoConsume
          queryTokenEitherParser
          (eofNoLiteralParser <|> setArithmeticLiteralParser)
      return $ QuerySection a sls

    manyTillNoConsume :: TextFieldParser a -> TextFieldParser b -> TextFieldParser [a]
    manyTillNoConsume p end = manyTill p (lookAhead end)

    eofNoLiteralParser :: TextFieldParser SetArithmeticLiteral
    eofNoLiteralParser = do
      eof
      return ANoLiteral

    queryTokenEitherParser :: TextFieldParser (SubList (QueryToken PseudoDescriptor))
    queryTokenEitherParser = do
      qt <-
        try subTagTokenParser <|> do
          qt' <- descriptorTokenParser
          return $ SubList qt' []
      spaces'
      return qt

    subTagTokenParser :: TextFieldParser (SubList (QueryToken PseudoDescriptor))
    subTagTokenParser = do
      subHead <- descriptorTokenParser
      spaces'
      char '{'
      spaces'
      subList <- do
        many $ do
          sd'' <- descriptorTokenParser
          spaces'
          return sd''
      char '}'
      spaces'
      return (SubList subHead subList)

    descriptorTokenParser :: TextFieldParser (QueryToken PseudoDescriptor)
    descriptorTokenParser = do
      tc <- queryCriteriaParser
      pd <- pseudoDescriptorParser
      return $ QueryToken tc pd

spaces' :: TextFieldParser ()
spaces' = spaces <?> ""

pseudoQueryParser :: TextFieldParser [PseudoSubTag]
pseudoQueryParser =
  many pseudoEitherParser
  where
    pseudoEitherParser :: ParsecT T.Text () Identity PseudoSubTag
    pseudoEitherParser = do
      pd <- try pseudoSubTagQueryParser <|> fmap (,[]) pseudoDescriptorParser
      spaces
      return pd

-- | Parses a descriptor
pseudoDescriptorParser :: TextFieldParser PseudoDescriptor
pseudoDescriptorParser =
  fmap (PDescriptor . T.pack) . many1 . noneOf $ "{} \t\n"

pseudoSubTagQueryParser :: TextFieldParser PseudoSubTag
pseudoSubTagQueryParser = do
  rawDes <- pseudoDescriptorParser
  spaces'
  char '{'
  spaces'
  subDess <- do
    many $ do
      sd'' <- pseudoDescriptorParser
      spaces'
      return sd''
  char '}'
  spaces'
  return (rawDes, subDess)

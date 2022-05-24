{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Event.CLI.Type
  ( CLIFlag (..),
    TaggerOpts (..),
    getTaggerOpt,
    nullOpts,
    hasQueryFlag,
    getQuery,
  )
where

import qualified IO

-- |
-- optionArguments -> [CLIFlag]
--
-- nonOptions -> [String]
--
-- optionErrors -> [String]
data TaggerOpts = TaggerOpts
  { optionArguments :: ![CLIFlag],
    nonOptions :: ![String],
    optionErrors :: ![String]
  }
  deriving (Show, Eq)

nullOpts :: TaggerOpts -> Bool
nullOpts (TaggerOpts [] [] []) = True
nullOpts _ = False

hasQueryFlag :: TaggerOpts -> Bool
hasQueryFlag = any (\f -> case f of Query _ -> True; _ -> False) . optionArguments

getQuery :: TaggerOpts -> Maybe String
getQuery opts =
  if not . hasQueryFlag $ opts
    then Nothing
    else getQuery' . optionArguments $ opts
  where
    getQuery' [] = Nothing
    getQuery' (Query s : _) = Just s
    getQuery' (_ : xs) = getQuery' xs

uncurryOpts :: ([CLIFlag], [String], [String]) -> TaggerOpts
uncurryOpts (cs, ns, es) = TaggerOpts cs ns es

data CLIFlag
  = Version
  | Query !String
  deriving (Show, Eq)

cliFlags :: [IO.OptDescr CLIFlag]
cliFlags =
  [ IO.Option
      ['v']
      ["version"]
      (IO.NoArg Version)
      "Show version.",
    IO.Option
      ['q']
      ["query"]
      (IO.ReqArg Query "QUERY")
      "Query the database using TaggerQL and \
      \a list of file paths. \
      \Implicit query criteria tokens default to 'Tag'.\n\
      \Ex. \"otsuki_yui {r.cute} d| r.season i| sweater\""
  ]

getTaggerOpt :: [String] -> TaggerOpts
getTaggerOpt = uncurryOpts . IO.getOpt IO.RequireOrder cliFlags
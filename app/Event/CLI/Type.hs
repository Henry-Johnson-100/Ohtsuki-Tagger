{-# OPTIONS_GHC -Wno-typed-holes #-}

module Event.CLI.Type
  ( CLIFlag (..),
    TaggerOpts (..),
    getTaggerOpt,
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
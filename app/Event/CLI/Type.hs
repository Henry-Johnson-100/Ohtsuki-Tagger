{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Event.CLI.Type
  ( CLIFlag (..),
    TaggerOpts (..),
    cliFlags,
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

data CLIFlag
  = Version
  | Query !String
  | Move !String
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
      \Ex. \"otsuki_yui {r.cute} d| r.season i| sweater\"",
    IO.Option
      ['m']
      ["move"]
      (IO.ReqArg Move "MOVE")
      "Move/rename a file in both the database and filesystem. \
      \The renaming will fail in the event that file does not exist in either the \
      \database or filesystem."
  ]

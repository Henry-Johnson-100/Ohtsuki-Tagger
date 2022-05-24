{-# OPTIONS_GHC -Wno-typed-holes #-}

module Event.CLI.Type
  ( CLIFlag (..),
    getTaggerOpt,
  )
where

import qualified IO

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

getTaggerOpt :: [String] -> ([CLIFlag], [String], [String])
getTaggerOpt = IO.getOpt IO.RequireOrder cliFlags
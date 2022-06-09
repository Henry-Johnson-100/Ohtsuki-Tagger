{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Event.CLI.Type
  ( OptionRecord (..),
    baseOptionRecord,
    optionRecordFlags,
  )
where

import qualified IO

data OptionRecord = OptionRecord
  { optionQuery :: !(Maybe String),
    optionDatabaseFile :: !(Maybe String),
    optionDatabasePath :: !(Maybe String),
    optionMove :: !(Maybe String),
    optionDelete :: !Bool,
    optionRemove :: !Bool,
    optionVersion :: !Bool,
    optionAdd :: ![String],
    optionRunTagger :: !Bool
  }
  deriving (Show, Eq)

baseOptionRecord :: OptionRecord
baseOptionRecord =
  OptionRecord
    Nothing
    Nothing
    Nothing
    Nothing
    False
    False
    False
    []
    True

setDontRun :: OptionRecord -> OptionRecord
setDontRun opts = opts {optionRunTagger = False}

optionRecordFlags :: [IO.OptDescr (OptionRecord -> OptionRecord)]
optionRecordFlags =
  [ IO.Option
      ['v']
      ["version"]
      (IO.NoArg (\opts -> setDontRun opts {optionVersion = True}))
      "Show version.",
    IO.Option
      ['q']
      ["query"]
      (IO.ReqArg (\s opts -> setDontRun opts {optionQuery = Just s}) "QUERY")
      "Query the database using TaggerQL and \
      \a list of file paths. \
      \Implicit query criteria tokens default to 'Tag'.\n\
      \Ex. \"otsuki_yui {r.cute} d| r.season i| sweater\"",
    IO.Option
      ['f']
      ["database-file"]
      (IO.ReqArg (\s opts -> setDontRun opts {optionDatabaseFile = Just s}) "DATABASE FILE")
      "Specify a single database file to run actions on. \
      \Actions run on the database file are specified with the -d, -r, or -m flags.\
      \If no operations are specified, then nothing happens to the database file.",
    IO.Option
      ['d']
      ["delete"]
      (IO.NoArg (\opts -> setDontRun opts {optionDelete = True}))
      "Delete the file specified with -f from both the database AND the system. \
      \You may just want to use -r to remove it from the database \
      \but keep it in the system.",
    IO.Option
      ['r']
      ["remove"]
      (IO.NoArg (\opts -> setDontRun opts {optionRemove = True}))
      "Removes the file specified with -f from the database BUT keeps it in the system.",
    IO.Option
      ['m']
      ["move"]
      (IO.ReqArg (\s opts -> setDontRun opts {optionMove = Just s}) "MOVE")
      "Moves the file specified with -f to the given path.",
    IO.Option
      ['a']
      ["add"]
      (IO.ReqArg (\s opts -> setDontRun opts {optionAdd = s : optionAdd opts}) "ADD")
      "Add the file(s) at the given path to the database.",
    IO.Option
      ['p']
      ["database-path"]
      (IO.ReqArg (\s opts -> opts {optionDatabasePath = Just s}) "DATABASE PATH")
      "Perform operations or run tagger with the given database path. \
      \Leave blank to use the database specified in the config file."
  ]

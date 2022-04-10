{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use ?~" #-}
{-# HLINT ignore "Use <&>" #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Redundant <$>" #-}

module Main where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Char
import Data.List
import Data.Maybe
import Data.Text hiding (head, map, take)
import Database.SQLite.Simple (close, open)
import Database.Tagger.Access
import Database.Tagger.Type
import Event.Task
import Monomer
import Monomer.Common.Lens
import Node.Application
import System.Process
import Type.Model

fwtUnion :: [FileWithTags] -> [FileWithTags] -> [FileWithTags]
fwtUnion = Data.List.unionBy fwtFileEqual

fwtIntersect :: [FileWithTags] -> [FileWithTags] -> [FileWithTags]
fwtIntersect = Data.List.intersectBy fwtFileEqual

fwtDiff :: [FileWithTags] -> [FileWithTags] -> [FileWithTags]
fwtDiff = Data.List.deleteFirstsBy fwtFileEqual

doSetAction ::
  FileSetArithmetic ->
  [FileWithTags] ->
  [FileWithTags] ->
  [FileWithTags]
doSetAction a s o =
  case a of
    Union -> s `fwtUnion` o
    Intersect -> s `fwtIntersect` o
    Diff -> s `fwtDiff` o

taggerEventHandler ::
  WidgetEnv TaggerModel TaggerEvent ->
  WidgetNode TaggerModel TaggerEvent ->
  TaggerModel ->
  TaggerEvent ->
  [AppEventResponse TaggerModel TaggerEvent]
taggerEventHandler wenv node model event =
  case event of
    TaggerInit ->
      [ Task
          ( DescriptorTreePut
              <$> (getALLInfraTree (model ^. dbConn))
          ),
        ( Task (UnrelatedDescriptorTreePut <$> getUnrelatedInfraTree (model ^. dbConn))
        ),
        Model $
          model
            & fileSingle
              .~ ( Just
                     ( FileWithTags
                         (File (-1) "/home/monax/Pictures/dog.jpg")
                         []
                     )
                 )
      ]
    FileSinglePut i -> [Model $ model & fileSingle .~ (Just i)]
    FileSingleMaybePut mi -> [Model $ model & fileSingle .~ mi]
    FileSetArithmetic a -> [Model $ model & fileSetArithmetic .~ a]
    FileSetQueryCriteria q -> [Model $ model & queryCriteria .~ q]
    FileSelectionUpdate ts ->
      [ Model $
          model & fileSelection
            .~ (doSetAction (model ^. fileSetArithmetic) (model ^. fileSelection) ts)
      ]
    FileSelectionPut fwts ->
      [ Model $ model & fileSelection .~ fwts
      ]
    FileSelectionStageQuery t -> [Model $ model & fileSelectionQuery .~ t]
    FileSelectionAppendQuery t ->
      [ Model $
          model & fileSelectionQuery
            .~ (Data.Text.unwords [model ^. fileSelectionQuery, t])
      ]
    TagsStringAppend t ->
      [ Model $
          model
            & tagsString .~ (Data.Text.unwords [model ^. tagsString, t])
      ]
    FileSelectionCommitQuery ->
      [ Task
          ( FileSelectionUpdate
              <$> ( doQueryWithCriteria
                      (model ^. queryCriteria)
                      (model ^. dbConn)
                      (Data.Text.words (model ^. fileSelectionQuery))
                  )
          )
      ]
    FileSelectionClear -> [Model $ model & fileSelection .~ []]
    DescriptorTreePut tr -> [Model $ model & descriptorTree .~ tr]
    UnrelatedDescriptorTreePut tr -> [Model $ model & unrelatedDescriptorTree .~ tr]
    -- #TODO no widget for this currently
    DescriptorTreePutParent ->
      [ Task
          ( DescriptorTreePut
              <$> ( getParentDescriptorTree
                      (model ^. dbConn)
                      (model ^. descriptorTree)
                  )
          )
      ]
    DescriptorCreateRelation ms is ->
      [ Task (PutExtern <$> relateTo (model ^. dbConn) ms is),
        Task
          ( RequestDescriptorTree
              <$> return (maybe "#ALL#" (descriptor) (getNode (model ^. descriptorTree)))
          )
      ]
    ToggleDoSoloTag ->
      [Model $ model & (doSoloTag .~ (not (model ^. doSoloTag)))]
    RequestDescriptorTree s ->
      [ Task
          ( DescriptorTreePut
              <$> (lookupInfraDescriptorTree (model ^. dbConn) s)
          )
      ]
    RefreshUnrelatedDescriptorTree ->
      [ Task (UnrelatedDescriptorTreePut <$> getUnrelatedInfraTree (model ^. dbConn))
      ]
    ShellCmd ->
      [ Task
          ( PutExtern
              <$> do
                unless (Data.Text.null (model ^. shellCmd)) $ do
                  let procArgs = Data.Text.words (model ^. shellCmd)
                  p <-
                    createProcess
                      . shell
                      $ Data.List.unwords
                        . putFileArgs
                          (map unpack procArgs)
                        . map
                          ( Data.Text.unpack
                              . filePath
                              . file
                          )
                        $ (model ^. fileSelection)
                  putStrLn $ "Running " ++ unpack (model ^. shellCmd)
          )
      ]
    PutExtern _ -> []
    TagCommitTagsString ->
      [ Task
          ( let cs = model ^. dbConn
                ds = Data.Text.words $ model ^. tagsString
             in if (model ^. doSoloTag)
                  then
                    FileSingleMaybePut
                      <$> ( fmap
                              head'
                              ( tagThenGetRefresh
                                  cs
                                  (maybeToList (model ^. fileSingle))
                                  ds
                              )
                          )
                  else
                    FileSelectionPut
                      <$> ( tagThenGetRefresh
                              cs
                              (model ^. fileSelection)
                              ds
                          )
          )
      ]
    DebugPrintSelection -> [Task (PutExtern <$> print (model ^. fileSelection))]

-- | Replaces "${file} in the first list with the entirety of the second."
putFileArgs :: [String] -> [String] -> [String]
putFileArgs args files =
  let atFileArg = Data.List.break (== "%file") args
   in (fst atFileArg) ++ files ++ (tail' . snd $ atFileArg)
  where
    tail' [] = []
    tail' (_ : xs) = xs

taggerApplicationUI ::
  WidgetEnv TaggerModel TaggerEvent ->
  TaggerModel ->
  WidgetNode TaggerModel TaggerEvent
taggerApplicationUI wenv model = widgetTree
  where
    widgetTree =
      hgrid
        [ vgrid
            [ fileSelectionWidget (model ^. fileSelection),
              vstack
                [ queryWidget,
                  descriptorTreeQuadrantWidget
                    (model ^. descriptorTree)
                    (model ^. unrelatedDescriptorTree)
                ]
            ],
          vgrid
            [ vstack [configPanel, button "print selection" DebugPrintSelection],
              (fileSinglePreviewWidget model)
            ]
        ]

taggerApplicationConfig :: [AppConfig TaggerEvent]
taggerApplicationConfig =
  [ appInitEvent TaggerInit
  ]
    ++ themeConfig

runTaggerWindow :: Connection -> IO ()
runTaggerWindow c =
  startApp
    (emptyTaggerModel c)
    taggerEventHandler
    taggerApplicationUI
    taggerApplicationConfig

main :: IO ()
main = do
  dbConn <- open "/home/monax/Repo/Haskell/tagger/images.db"
  runTaggerWindow dbConn
  close dbConn

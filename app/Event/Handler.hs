{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use ?~" #-}
{-# HLINT ignore "Redundant <$>" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Event.Handler
  ( taggerEventHandler,
  )
where

import Control.Lens
import Control.Monad
import qualified Data.List
import Data.Maybe
import qualified Data.Text
import Database.Tagger.Type
import Event.Task
import Monomer
import System.Process
import Type.Model

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
    DescriptorTreePutParent ->
      [ Task
          ( DescriptorTreePut
              <$> ( getParentDescriptorTree
                      (model ^. dbConn)
                      (model ^. descriptorTree)
                  )
          )
      ]
    DescriptorCommitNewDescriptorText ->
      [ Task
          ( PutExtern
              <$> createNewDescriptors
                (model ^. dbConn)
                (Data.Text.words (model ^. newDescriptorText))
          ),
        Task (RefreshBothDescriptorTrees <$ pure ())
      ]
    DescriptorDelete d ->
      [ Task (PutExtern <$> deleteDescriptor (model ^. dbConn) d),
        Task (RefreshBothDescriptorTrees <$ pure ())
      ]
    DescriptorCreateRelation ms is ->
      [ Task (PutExtern <$> relateTo (model ^. dbConn) ms is),
        Task (RefreshBothDescriptorTrees <$ pure ())
      ]
    DescriptorUnrelate is ->
      [ Task (PutExtern <$> unrelate (model ^. dbConn) is),
        Task (RefreshBothDescriptorTrees <$ pure ())
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
    RefreshBothDescriptorTrees ->
      [ Task
          ( RequestDescriptorTree
              <$> (return . maybe "#ALL#" descriptor . getNode $ model ^. descriptorTree)
          ),
        Task (RefreshUnrelatedDescriptorTree <$ pure ())
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
                          (map Data.Text.unpack procArgs)
                        . map
                          ( Data.Text.unpack
                              . filePath
                              . file
                          )
                        $ (model ^. fileSelection)
                  putStrLn $ "Running " ++ Data.Text.unpack (model ^. shellCmd)
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

-- | Replaces "%file" in the first list with the entirety of the second.
putFileArgs :: [String] -> [String] -> [String]
putFileArgs args files =
  let atFileArg = Data.List.break (== "%file") args
   in (fst atFileArg) ++ files ++ (tail' . snd $ atFileArg)
  where
    tail' [] = []
    tail' (_ : xs) = xs

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

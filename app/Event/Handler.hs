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
import Control.Monad (unless)
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Text as T
import Database.Tagger.Type
import Event.Task
import Monomer
  ( AppEventResponse,
    EventResponse (Model, Task),
    WidgetEnv,
    WidgetNode,
  )
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
            .~ (T.unwords [model ^. fileSelectionQuery, t])
      ]
    TagsStringAppend t ->
      [ Model $
          model
            & tagsString .~ (T.unwords [model ^. tagsString, t])
      ]
    FileSelectionCommitQuery ->
      [ Task
          ( FileSelectionUpdate
              <$> ( doQueryWithCriteria
                      (model ^. queryCriteria)
                      (model ^. dbConn)
                      (T.words (model ^. fileSelectionQuery))
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
                (T.words (model ^. newDescriptorText))
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
                unless (T.null (model ^. shellCmd)) $ do
                  let procArgs = T.words (model ^. shellCmd)
                  p <-
                    createProcess
                      . shell
                      $ L.unwords
                        . putFileArgs
                          (map T.unpack procArgs)
                        . map
                          ( T.unpack
                              . filePath
                              . file
                          )
                        $ (model ^. fileSelection)
                  putStrLn $ "Running " ++ T.unpack (model ^. shellCmd)
          )
      ]
    PutExtern _ -> []
    TagCommitTagsString ->
      [ Task
          ( let cs = model ^. dbConn
                ds = T.words $ model ^. tagsString
             in if (model ^. doSoloTag)
                  then
                    FileSingleMaybePut
                      <$> ( fmap
                              head'
                              ( tagThenGetRefresh
                                  cs
                                  (M.maybeToList (model ^. fileSingle))
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
  let atFileArg = L.break (== "%file") args
   in (fst atFileArg) ++ files ++ (tail' . snd $ atFileArg)
  where
    tail' [] = []
    tail' (_ : xs) = xs

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
  where
    fwtUnion = L.unionBy fwtFileEqual
    fwtIntersect = L.intersectBy fwtFileEqual
    fwtDiff = L.deleteFirstsBy fwtFileEqual
{-# LANGUAGE BangPatterns #-}
{-# HLINT ignore "Use ?~" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Redundant <$>" #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use section" #-}

module Event.Handler
  ( taggerEventHandler,
  )
where

import Control.Lens ((&), (.~), (^.))
import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.Tagger.Type
import Event.Task
import IO
import Monomer
  ( AppEventResponse,
    EventResponse (Model, Task),
    WidgetEnv,
    WidgetNode,
  )
import Type.Config
import Type.Model

fwtUnion :: [FileWithTags] -> [FileWithTags] -> [FileWithTags]
fwtUnion = L.unionBy fwtFileEqual

fwtIntersect :: [FileWithTags] -> [FileWithTags] -> [FileWithTags]
fwtIntersect = L.intersectBy fwtFileEqual

fwtDiff :: [FileWithTags] -> [FileWithTags] -> [FileWithTags]
fwtDiff = L.deleteFirstsBy fwtFileEqual

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

dbConnTask e f = maybe (Task (PutExtern <$> pure ())) (Task . fmap e . f) . connInstance

taggerEventHandler ::
  WidgetEnv TaggerModel TaggerEvent ->
  WidgetNode TaggerModel TaggerEvent ->
  TaggerModel ->
  TaggerEvent ->
  [AppEventResponse TaggerModel TaggerEvent]
taggerEventHandler wenv node model event =
  case event of
    TaggerInit ->
      [ dbConnTask DescriptorTreePut getALLInfraTree (model ^. dbConn),
        dbConnTask UnrelatedDescriptorTreePut getUnrelatedInfraTree (model ^. dbConn),
        Model $
          model
            & fileSingle
              .~ Just
                ( FileWithTags
                    (File (-1) "/home/monax/Pictures/dog.jpg")
                    []
                )
      ]
    FileSinglePut i -> [Model $ model & fileSingle .~ Just i]
    FileSingleMaybePut mi -> [Model $ model & fileSingle .~ mi]
    FileSetArithmetic a -> [Model $ model & fileSetArithmetic .~ a]
    FileSetArithmeticNext ->
      [ Model $
          model
            & fileSetArithmetic .~ next (model ^. fileSetArithmetic)
      ]
    FileSetQueryCriteria q -> [Model $ model & queryCriteria .~ q]
    FileSetQueryCriteriaNext ->
      [ Model $
          model
            & queryCriteria .~ next (model ^. queryCriteria)
      ]
    FileSelectionUpdate ts ->
      [ Model $
          model & fileSelection
            .~ doSetAction (model ^. fileSetArithmetic) (model ^. fileSelection) ts
      ]
    FileSelectionPut fwts ->
      [ Model $ model & fileSelection .~ fwts
      ]
    FileSelectionRefresh_ ->
      [ dbConnTask
          FileSelectionPut
          (flip getRefreshedFWTs (model ^. fileSelection))
          (model ^. dbConn),
        dbConnTask
          FileSingleMaybePut
          ( \activeDbConn ->
              do
                mrefreshed <-
                  M.maybe
                    (return [])
                    (getRefreshedFWTs activeDbConn . (: []))
                    (model ^. fileSingle)
                return . head' $ mrefreshed
          )
          (model ^. dbConn)
      ]
    FileSelectionAppendQuery t ->
      [ Model $
          model & fileSelectionQuery
            .~ T.unwords [model ^. fileSelectionQuery, t]
      ]
    TagsStringAppend t ->
      [ Model $
          model
            & tagsString .~ T.unwords [model ^. tagsString, t]
      ]
    TagsStringClear -> [Model $ model & tagsString .~ ""]
    FileSelectionCommitQuery ->
      [ dbConnTask
          FileSelectionUpdate
          ( \activeDbConn ->
              doQueryWithCriteria
                (model ^. queryCriteria)
                activeDbConn
                (T.words (model ^. fileSelectionQuery))
          )
          (model ^. dbConn),
        asyncEvent FileSelectionQueryClear
      ]
    FileSelectionClear ->
      [ Model $ model & fileSelection .~ [],
        asyncEvent FileSelectionQueryClear
      ]
    FileSelectionQueryClear -> [Model $ model & fileSelectionQuery .~ ""]
    FileSingleNextFromFileSelection ->
      let !ps = popCycleList (model ^. fileSelection)
          !mi = head' ps
       in [ Model
              . (fileSelection .~ ps)
              . (fileSingle .~ mi)
              . (doSoloTag .~ True)
              $ model
          ]
    FileSinglePrevFromFileSelection ->
      let !ps = dequeueCycleList (model ^. fileSelection)
          !mi = head' ps
       in [ Model
              . (fileSelection .~ ps)
              . (fileSingle .~ mi)
              . (doSoloTag .~ True)
              $ model
          ]
    DescriptorTreePut tr -> [Model $ model & descriptorTree .~ tr]
    UnrelatedDescriptorTreePut tr -> [Model $ model & unrelatedDescriptorTree .~ tr]
    DescriptorTreePutParent ->
      [ dbConnTask
          DescriptorTreePut
          ( flip
              getParentDescriptorTree
              (model ^. descriptorTree)
          )
          (model ^. dbConn)
      ]
    DescriptorCommitNewDescriptorText ->
      [ dbConnTask
          PutExtern
          (flip createNewDescriptors (T.words (model ^. newDescriptorText)))
          (model ^. dbConn),
        asyncEvent RefreshBothDescriptorTrees
      ]
    DescriptorDelete d ->
      [ dbConnTask PutExtern (flip deleteDescriptor d) (model ^. dbConn),
        asyncEvent RefreshBothDescriptorTrees
      ]
    DescriptorCreateRelation ms is ->
      [ dbConnTask PutExtern (\activeConn -> relateTo activeConn ms is) (model ^. dbConn),
        asyncEvent RefreshBothDescriptorTrees
      ]
    DescriptorUnrelate is ->
      [ dbConnTask PutExtern (flip unrelate is) (model ^. dbConn),
        asyncEvent RefreshBothDescriptorTrees
      ]
    ToggleDoSoloTag ->
      [Model $ model & (doSoloTag .~ not (model ^. doSoloTag))]
    RequestDescriptorTree s ->
      [dbConnTask DescriptorTreePut (flip lookupInfraDescriptorTree s) (model ^. dbConn)]
    RefreshUnrelatedDescriptorTree ->
      [dbConnTask UnrelatedDescriptorTreePut getUnrelatedInfraTree (model ^. dbConn)]
    RefreshBothDescriptorTrees ->
      [ Task
          ( RequestDescriptorTree
              <$> (return . maybe "#ALL#" descriptor . getNode $ model ^. descriptorTree)
          ),
        asyncEvent RefreshUnrelatedDescriptorTree
      ]
    ShellCmd ->
      [ Task
          ( PutExtern
              <$> do
                CM.unless (T.null (model ^. shellCmd)) $ do
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
      [ asyncEvent $
          if model ^. doSoloTag
            then TagCommitTagsStringDoSolo
            else TagCommitTagsStringDoSelection,
        asyncEvent TagsStringClear
      ]
    TagCommitTagsStringDoSolo ->
      [ dbConnTask
          PutExtern
          ( \activeConn ->
              (if isUntagMode (model ^. taggingMode) then untagWith else tag)
                activeConn
                (M.maybeToList $ model ^. fileSingle)
                (T.words $ model ^. tagsString)
          )
          (model ^. dbConn),
        asyncEvent FileSelectionRefresh_
      ]
    TagCommitTagsStringDoSelection ->
      [ dbConnTask
          PutExtern
          ( \activeConn ->
              ( if isUntagMode $ model ^. taggingMode
                  then untagWith
                  else tag
              )
                activeConn
                (model ^. fileSelection)
                (T.words $ model ^. tagsString)
          )
          (model ^. dbConn),
        asyncEvent FileSelectionRefresh_
      ]
    NewFileTextCommit ->
      [ dbConnTask FileSelectionPut (flip addPath (model ^. newFileText)) $
          model ^. dbConn,
        Model $ model & newFileText .~ ""
      ]
    InitializeDatabase ->
      [ dbConnTask
          PutExtern
          (runInitScript (T.unpack . dbInit $ model ^. programConfig))
          (model ^. dbConn)
      ]
    ToggleVisibilityMode vm ->
      [ let currentVM = model ^. programVisibility
         in Model $ model & programVisibility .~ (if currentVM == vm then Main else vm)
      ]
    DebugPrintSelection -> [Task (PutExtern <$> print (model ^. fileSelection))]

-- | Replaces "%file" in the first list with the entirety of the second.
putFileArgs :: [String] -> [String] -> [String]
putFileArgs args files =
  let atFileArg = L.break (== "%file") args
   in fst atFileArg ++ files ++ (tail' . snd $ atFileArg)

tail' :: [a] -> [a]
tail' [] = []
tail' (_ : xs) = xs

last' :: [a] -> Maybe a
last' [] = Nothing
last' xs = Just . last $ xs

init' :: [a] -> [a]
init' [] = []
init' xs = init xs

-- | Take the first item and put it on the end
popCycleList :: [a] -> [a]
popCycleList [] = []
popCycleList (x : xs) = xs ++ [x]

-- | Take the last item and put it on the front
dequeueCycleList :: [a] -> [a]
dequeueCycleList [] = []
dequeueCycleList xs = last xs : init xs

asyncEvent :: e -> EventResponse s e sp ep
asyncEvent = Task . flip (<$) emptyM
  where
    emptyM = pure ()
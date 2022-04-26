{-# LANGUAGE BangPatterns #-}
{-# HLINT ignore "Use ?~" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant <$>" #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use section" #-}
{-# HLINT ignore "Use list comprehension" #-}

module Event.Handler
  ( taggerEventHandler,
  )
where

import Control.Lens ((%~), (&), (.~), (^.))
import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.Tagger.Access (activateForeignKeyPragma)
import Database.Tagger.Type
import Event.Task
import IO
import Monomer
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

dbConnTask ::
  (a -> TaggerEvent) ->
  (Connection -> IO a) ->
  TaggedConnection ->
  EventResponse s TaggerEvent sp ep
dbConnTask e f =
  maybe
    (Task (IOEvent <$> hPutStrLn stderr "Database connection not active."))
    (Task . fmap e . f)
    . connInstance

singleFileEventHandler ::
  WidgetEnv TaggerModel TaggerEvent ->
  WidgetNode TaggerModel TaggerEvent ->
  TaggerModel ->
  SingleFileEvent ->
  [AppEventResponse TaggerModel TaggerEvent]
singleFileEventHandler wenv node model event =
  case event of
    SingleFilePut fwt ->
      [ model *~ (singleFileModel . singleFile) .~ Just fwt,
        asyncEvent (DoSingleFileEvent SingleFileGetTagCounts)
      ]
    SingleFileMaybePut mfwt ->
      [ model *~ (singleFileModel . singleFile) .~ mfwt,
        asyncEvent (DoSingleFileEvent SingleFileGetTagCounts)
      ]
    SingleFileNextFromFileSelection ->
      let !ps = popCycleList (model ^. (fileSelectionModel . fileSelection))
          !mi = head' ps
       in [ Model
              . ((fileSelectionModel . fileSelection) .~ ps)
              . ((singleFileModel . singleFile) .~ mi)
              . (doSoloTag .~ True)
              $ model,
            asyncEvent (DoSingleFileEvent SingleFileGetTagCounts)
          ]
    SingleFilePrevFromFileSelection ->
      let !ps = dequeueCycleList (model ^. (fileSelectionModel . fileSelection))
          !mi = head' ps
       in [ Model
              . ((fileSelectionModel . fileSelection) .~ ps)
              . ((singleFileModel . singleFile) .~ mi)
              . (doSoloTag .~ True)
              $ model,
            asyncEvent (DoSingleFileEvent SingleFileGetTagCounts)
          ]
    SingleFilePutTagCounts_ tcs -> [model *~ (singleFileModel . tagCounts) .~ tcs]
    SingleFileGetTagCounts ->
      [ dbConnTask
          (DoSingleFileEvent . SingleFilePutTagCounts_)
          (flip getTagCounts . maybe [] tags $ model ^. (singleFileModel . singleFile))
          (model ^. dbConn)
      ]

configurationEventHandler ::
  WidgetEnv TaggerModel TaggerEvent ->
  WidgetNode TaggerModel TaggerEvent ->
  TaggerModel ->
  ConfigurationEvent ->
  [AppEventResponse TaggerModel TaggerEvent]
configurationEventHandler wenv node model event =
  case event of
    ExportAll -> [Task (IOEvent <$> exportConfig (model ^. programConfig))]

fileSelectionEventHandler ::
  WidgetEnv TaggerModel TaggerEvent ->
  WidgetNode TaggerModel TaggerEvent ->
  TaggerModel ->
  FileSelectionEvent ->
  [AppEventResponse TaggerModel TaggerEvent]
fileSelectionEventHandler wenv node model event =
  case event of
    FileSelectionUpdate fwts ->
      [ model *~ (fileSelectionModel . fileSelection)
          .~ doSetAction
            (model ^. (fileSelectionModel . setArithmetic))
            (model ^. (fileSelectionModel . fileSelection))
            fwts
      ]
    FileSelectionPut fwts ->
      [ model *~ (fileSelectionModel . fileSelection) .~ fwts
      ]
    FileSelectionRefresh_ ->
      [ dbConnTask
          (DoFileSelectionEvent . FileSelectionPut)
          (flip getRefreshedFWTs (model ^. (fileSelectionModel . fileSelection)))
          (model ^. dbConn),
        dbConnTask
          (DoSingleFileEvent . SingleFileMaybePut)
          ( \activeConn ->
              do
                mrefreshed <-
                  M.maybe
                    (return [])
                    (getRefreshedFWTs activeConn . (: []))
                    (model ^. (singleFileModel . singleFile))
                return . head' $ mrefreshed
          )
          (model ^. dbConn)
      ]
    FileSelectionAppendToQueryText t ->
      [ model *~ (fileSelectionModel . queryText)
          .~ T.unwords [model ^. (fileSelectionModel . queryText), t]
      ]
    FileSelectionCommitQueryText ->
      [ dbConnTask
          (DoFileSelectionEvent . FileSelectionUpdate)
          ( \activeDbConn ->
              doQueryWithCriteria
                (model ^. (fileSelectionModel . queryCriteria))
                activeDbConn
                (T.words (model ^. (fileSelectionModel . queryText)))
          )
          (model ^. dbConn),
        asyncEvent (DoFileSelectionEvent FileSelectionQueryTextClear)
      ]
    FileSelectionClear ->
      [ model *~ (fileSelectionModel . fileSelection) .~ [],
        asyncEvent (DoFileSelectionEvent FileSelectionQueryTextClear)
      ]
    FileSelectionQueryTextClear -> [model *~ (fileSelectionModel . queryText) .~ ""]
    FileSelectionSetArithmetic a ->
      [model *~ (fileSelectionModel . setArithmetic) .~ a]
    FileSelectionNextSetArithmetic ->
      [model *~ (fileSelectionModel . setArithmetic) %~ next]
    FileSelectionPrevSetArithmetic ->
      [model *~ (fileSelectionModel . setArithmetic) %~ prev]
    FileSelectionQueryCriteria q ->
      [model *~ (fileSelectionModel . queryCriteria) .~ q]
    FileSelectionNextQueryCriteria ->
      [model *~ (fileSelectionModel . queryCriteria) %~ next]
    FileSelectionPrevQueryCriteria ->
      [model *~ (fileSelectionModel . queryCriteria) %~ prev]
    FileSelectionShuffle ->
      [ Task
          ( DoFileSelectionEvent . FileSelectionPut
              <$> shuffle (model ^. (fileSelectionModel . fileSelection))
          )
      ]

taggerEventHandler ::
  WidgetEnv TaggerModel TaggerEvent ->
  WidgetNode TaggerModel TaggerEvent ->
  TaggerModel ->
  TaggerEvent ->
  [AppEventResponse TaggerModel TaggerEvent]
taggerEventHandler wenv node model event =
  case event of
    TaggerInit ->
      if model ^. (programConfig . dbconf . dbconfAutoConnect)
        then [asyncEvent DatabaseConnect]
        else []
    DoSingleFileEvent evt -> singleFileEventHandler wenv node model evt
    DoConfigurationEvent evt -> configurationEventHandler wenv node model evt
    DoFileSelectionEvent evt -> fileSelectionEventHandler wenv node model evt
    TagsStringAppend t ->
      [ Model $
          model
            & tagsString .~ T.unwords [model ^. tagsString, t]
      ]
    TagsStringClear -> [model *~ tagsString .~ ""]
    DescriptorTreePut tr -> [model *~ descriptorTree .~ tr]
    UnrelatedDescriptorTreePut tr -> [model *~ unrelatedDescriptorTree .~ tr]
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
          IOEvent
          (flip createNewDescriptors (T.words (model ^. newDescriptorText)))
          (model ^. dbConn),
        asyncEvent RefreshBothDescriptorTrees
      ]
    DescriptorDelete d ->
      [ dbConnTask IOEvent (flip deleteDescriptor d) (model ^. dbConn),
        asyncEvent RefreshBothDescriptorTrees
      ]
    DescriptorCreateRelation ms is ->
      [ dbConnTask IOEvent (\activeConn -> relateTo activeConn ms is) (model ^. dbConn),
        asyncEvent RefreshBothDescriptorTrees
      ]
    DescriptorUnrelate is ->
      [ dbConnTask IOEvent (flip unrelate is) (model ^. dbConn),
        asyncEvent RefreshBothDescriptorTrees
      ]
    ToggleDoSoloTag ->
      [model *~ (doSoloTag .~ not (model ^. doSoloTag))]
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
          ( IOEvent
              <$> runShellCmds
                (words . T.unpack $ model ^. shellCmd)
                ( if model ^. doSoloTag && M.isJust singlefwt
                    then (: []) . fwtPath . M.fromJust $ singlefwt
                    else selectionFwts
                )
          )
      ]
      where
        fwtPath = T.unpack . filePath . file
        selectionFwts = map fwtPath $ model ^. fileSelectionModel . fileSelection
        singlefwt = model ^. singleFileModel . singleFile
    IOEvent _ -> []
    TagCommitTagsString ->
      [ asyncEvent $
          if model ^. doSoloTag
            then TagCommitTagsStringDoSolo
            else TagCommitTagsStringDoSelection,
        asyncEvent TagsStringClear
      ]
    TagCommitTagsStringDoSolo ->
      [ dbConnTask
          IOEvent
          ( \activeConn ->
              (if isUntagMode (model ^. taggingMode) then untagWith else tag)
                activeConn
                (M.maybeToList $ model ^. (singleFileModel . singleFile))
                (T.words $ model ^. tagsString)
          )
          (model ^. dbConn),
        asyncEvent (DoFileSelectionEvent FileSelectionRefresh_)
      ]
    TagCommitTagsStringDoSelection ->
      [ dbConnTask
          IOEvent
          ( \activeConn ->
              (if isUntagMode (model ^. taggingMode) then untagWith else tag)
                activeConn
                (model ^. (fileSelectionModel . fileSelection))
                (T.words $ model ^. tagsString)
          )
          (model ^. dbConn),
        asyncEvent (DoFileSelectionEvent FileSelectionRefresh_)
      ]
    TaggingModeNext -> [model *~ taggingMode %~ next]
    TaggingModePrev -> [model *~ taggingMode %~ prev]
    NewFileTextCommit ->
      [ dbConnTask
          (DoFileSelectionEvent . FileSelectionPut)
          (flip addPath (model ^. newFileText))
          $ model ^. dbConn,
        model *~ newFileText .~ ""
      ]
    DatabaseInitialize ->
      [ dbConnTask
          IOEvent
          (runInitScript (T.unpack $ model ^. (programConfig . dbconf . dbconfInit)))
          (model ^. dbConn)
      ]
    ToggleVisibilityMode vm ->
      [ let currentVM = model ^. programVisibility
         in model *~ programVisibility .~ (if currentVM == vm then Main else vm)
      ]
    DatabaseConnectionPut_ tc -> [model *~ dbConn .~ tc]
    DatabaseConnect ->
      [ Task $
          DatabaseConnectionPut_ <$> do
            maybe (pure ()) close . connInstance $ model ^. dbConn
            let newConnTag = model ^. (programConfig . dbconf . dbconfPath)
            newConnInstance <- open . T.unpack $ newConnTag
            activateForeignKeyPragma newConnInstance
            return . TaggedConnection newConnTag . Just $ newConnInstance,
        asyncEvent RefreshBothDescriptorTrees
      ]
    DatabaseBackup ->
      [ Task
          ( IOEvent
              <$> ( maybe
                      ( hPutStrLn
                          stderr
                          "Failed to backup, no database is currently connected."
                      )
                      ( flip
                          backupDbConn
                          ( T.unpack $
                              model
                                ^. ( programConfig
                                       . dbconf
                                       . dbconfBackup
                                   )
                          )
                      )
                      . connInstance
                      $ model ^. dbConn
                  )
          )
      ]

-- I will never understand how the stupid fixity stuff works
infixl 3 *~

(*~) :: a -> (a -> s) -> EventResponse s e sp ep
m *~ a = Model $ m & a

-- | Replaces "%file" in the first list with the entirety of the second.
putFileArgs :: [String] -> [String] -> [String]
putFileArgs args files =
  let !atFileArg = L.break (L.isInfixOf "%file") args
   in fst atFileArg ++ files ++ (tail' . snd $ atFileArg)

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
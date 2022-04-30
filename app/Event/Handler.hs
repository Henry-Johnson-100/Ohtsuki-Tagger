{-# LANGUAGE BangPatterns #-}
{-# HLINT ignore "Use ?~" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant <$>" #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use section" #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Use :" #-}

module Event.Handler
  ( taggerEventHandler,
  )
where

import Control.Lens ((%~), (&), (.~), (^.))
import Control.Monad
import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.Tagger.Access (activateForeignKeyPragma, lookupDescriptorPattern)
import Database.Tagger.Type
import Event.Task
import IO
import Monomer
import Type.BufferList
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

descriptorTreeEventHandler ::
  WidgetEnv TaggerModel TaggerEvent ->
  WidgetNode TaggerModel TaggerEvent ->
  TaggerModel ->
  DescriptorEvent ->
  [AppEventResponse TaggerModel TaggerEvent]
descriptorTreeEventHandler wenv node model event =
  case event of
    DescriptorTreePut mLens toPut ->
      [ model *~ (descriptorModel . mLens . rootTree) .~ toPut
      ]
    DescriptorTreePutParent mLens ->
      [ dbConnTask
          (DoDescriptorEvent . DescriptorTreePut mLens)
          ( flip
              getParentDescriptorTree
              (model ^. (descriptorModel . mLens . rootTree))
          )
          (model ^. dbConn)
      ]
    RequestDescriptorTree mLens d ->
      [ dbConnTask
          (DoDescriptorEvent . DescriptorTreePut mLens)
          (flip lookupInfraDescriptorTree d)
          (model ^. dbConn)
      ]
    RefreshDescriptorTree mLens ->
      [ Task
          ( DoDescriptorEvent . RequestDescriptorTree mLens
              <$> ( return
                      . maybe (model ^. descriptorModel . mLens . rootName) descriptor
                      . getNode
                      $ model
                        ^. (descriptorModel . mLens . rootTree)
                  )
          )
      ]
    RenameDescriptor ->
      [ dbConnTask
          IOEvent
          ( \c -> do
              d <-
                fmap head'
                  . lookupDescriptorPattern c
                  . T.strip
                  $ (model ^. descriptorModel . renameDescriptorFrom)
              maybeM_
                ( flip
                    (renameDescriptor c)
                    (T.strip $ model ^. descriptorModel . renameDescriptorTo)
                )
                d
          )
          (model ^. dbConn)
      ]
        ++ ( asyncEvent . DoDescriptorEvent
               <$> [ RefreshDescriptorTree mainDescriptorTree,
                     RefreshDescriptorTree unrelatedDescriptorTree
                   ]
           )

maybeM_ :: Monad m => (a -> m ()) -> Maybe a -> m ()
maybeM_ = M.maybe (pure ())

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
      let !ps = cPop $ model ^. fileSelectionModel . fileSelection
          !mi = cHead ps
       in [ Model
              . ((fileSelectionModel . fileSelection) .~ ps)
              . ((singleFileModel . singleFile) .~ mi)
              . (doSoloTag .~ True)
              $ model,
            asyncEvent (DoSingleFileEvent SingleFileGetTagCounts)
          ]
    SingleFilePrevFromFileSelection ->
      let !ps = cDequeue $ model ^. fileSelectionModel . fileSelection
          !mi = cHead ps
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
          .~ cFromList
            ( doSetAction
                (model ^. (fileSelectionModel . setArithmetic))
                (cCollect $ model ^. (fileSelectionModel . fileSelection))
                fwts
            )
      ]
    FileSelectionPut fwts ->
      [ model *~ (fileSelectionModel . fileSelection) .~ cFromList fwts
      ]
    FileSelectionRefresh_ ->
      [ dbConnTask
          (DoFileSelectionEvent . FileSelectionPut)
          ( flip
              getRefreshedFWTs
              ( cCollect $
                  model ^. (fileSelectionModel . fileSelection)
              )
          )
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
      [ model *~ (fileSelectionModel . fileSelection) .~ emptyBufferList,
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
              <$> (shuffle . cCollect $ (model ^. (fileSelectionModel . fileSelection)))
          )
      ]
    LazyBufferLoad ->
      [ model *~ fileSelectionModel . fileSelection
          %~ takeToBuffer (model ^. programConfig . selectionconf . selectionBufferSize)
      ]
    LazyBufferLoadAll ->
      [ model *~ fileSelectionModel . fileSelection %~ toBuffer
      ]
    LazyBufferFlush ->
      [ model *~ fileSelectionModel . fileSelection %~ emptyBuffer
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
    DoDescriptorEvent evt -> descriptorTreeEventHandler wenv node model evt
    TagsStringClear -> [model *~ tagsString .~ ""]
    DescriptorCommitNewDescriptorText ->
      [ dbConnTask
          IOEvent
          (flip createNewDescriptors (T.words (model ^. newDescriptorText)))
          (model ^. dbConn)
      ]
        ++ ( asyncEvent . DoDescriptorEvent
               <$> [ RefreshDescriptorTree mainDescriptorTree,
                     RefreshDescriptorTree unrelatedDescriptorTree
                   ]
           )
    DescriptorDelete d ->
      [ dbConnTask IOEvent (flip deleteDescriptor d) (model ^. dbConn)
      ]
        ++ ( asyncEvent . DoDescriptorEvent
               <$> [ RefreshDescriptorTree mainDescriptorTree,
                     RefreshDescriptorTree unrelatedDescriptorTree
                   ]
           )
    DescriptorCreateRelation ms is ->
      [ dbConnTask IOEvent (\activeConn -> relateTo activeConn ms is) (model ^. dbConn)
      ]
        ++ ( asyncEvent . DoDescriptorEvent
               <$> [ RefreshDescriptorTree mainDescriptorTree,
                     RefreshDescriptorTree unrelatedDescriptorTree
                   ]
           )
    DescriptorUnrelate is ->
      [ dbConnTask IOEvent (flip unrelate is) (model ^. dbConn)
      ]
        ++ ( asyncEvent . DoDescriptorEvent
               <$> [ RefreshDescriptorTree mainDescriptorTree,
                     RefreshDescriptorTree unrelatedDescriptorTree
                   ]
           )
    ToggleDoSoloTag ->
      [model *~ (doSoloTag .~ not (model ^. doSoloTag))]
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
        selectionFwts =
          map fwtPath . cCollect $
            model ^. fileSelectionModel . fileSelection
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
                (cCollect $ model ^. (fileSelectionModel . fileSelection))
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
            return . TaggedConnection newConnTag . Just $ newConnInstance
      ]
        ++ ( asyncEvent . DoDescriptorEvent
               <$> [ RefreshDescriptorTree mainDescriptorTree,
                     RefreshDescriptorTree unrelatedDescriptorTree
                   ]
           )
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
    DropTargetAppendText_ l df d ->
      [Model $ model & l %~ flip T.append (" " `T.append` df d)]

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
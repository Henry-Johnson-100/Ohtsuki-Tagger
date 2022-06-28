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
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import qualified Data.HashSet as HashSet
import qualified Data.Maybe as M
import qualified Data.Text as T
import Database.SQLite.Simple (Connection, close, open)
import Database.Tagger.Access
  ( activateForeignKeyPragma,
    lookupDescriptorPattern,
  )
import Database.Tagger.Type
  ( Descriptor (descriptor, descriptorId),
    FileWithTags (..),
    Tag (tagDescriptor),
    getNode,
  )
import Event.Parser (parseQuery)
import Event.Task
  ( addPath,
    associateTag,
    createNewDescriptors,
    createRepresentative,
    deleteDescriptor,
    exportConfig,
    getParentDescriptorTree,
    getRepresentative,
    lookupInfraDescriptorTree,
    relateTo,
    renameDescriptor,
    tag,
    unrelate,
    untag,
    untagWithTag,
  )
import qualified IO
import Monomer
  ( AppEventResponse,
    EventResponse (Event, Model, Request, Task),
    WidgetEnv,
    WidgetNode,
    WidgetRequest (UpdateWindow),
    WindowRequest (WindowSetTitle),
  )
import Type.BufferList
  ( bufferListFromList,
    emptyBuffer,
    takeToBuffer,
    toBuffer,
  )
import Type.Model
  ( ConfigurationEvent (..),
    Cyclic (next, prev),
    DescriptorEvent (..),
    FileSelectionEvent (..),
    FileSetArithmetic (..),
    HasConnInstance (connInstance),
    HasConnName (connName),
    HasDbConn (dbConn),
    HasDescriptorModel (descriptorModel),
    HasDoSoloTag (doSoloTag),
    HasFileSelection (fileSelection),
    HasFileSelectionModel (fileSelectionModel),
    HasLastAccessed (lastAccessed),
    HasLastBackup (lastBackup),
    HasMainDescriptorTree (mainDescriptorTree),
    HasNewDescriptorText (newDescriptorText),
    HasNewFileText (newFileText),
    HasProgramConfig (programConfig),
    HasProgramVisibility (programVisibility),
    HasQueryCriteria (queryCriteria),
    HasQueryText (queryText),
    HasRenameDescriptorFrom (renameDescriptorFrom),
    HasRenameDescriptorTo (renameDescriptorTo),
    HasRepresentativeFile (representativeFile),
    HasSetArithmetic (setArithmetic),
    HasTaggingMode (taggingMode),
    HasTagsString (tagsString),
    HasUnrelatedDescriptorTree (unrelatedDescriptorTree),
    Intersectable (diffBy, intersectBy, unionBy),
    ProgramVisibility (Main),
    TaggedConnection (TaggedConnection),
    TaggedConnectionEvent (..),
    TaggerEvent (..),
    TaggerModel,
    dbconf,
    dbconfAutoConnect,
    dbconfPath,
    rootName,
    rootTree,
    selectionBufferSize,
    selectionconf,
  )
import Util.Core (head', hoistMaybe, maybeWithList, (!++))

doSetAction ::
  Intersectable l =>
  FileSetArithmetic ->
  (a -> a -> Bool) ->
  l a ->
  l a ->
  l a
doSetAction a f = case a of
  Union -> unionBy f
  Intersect -> intersectBy f
  Diff -> diffBy f

dbConnTask ::
  (a -> TaggerEvent) ->
  (Connection -> IO a) ->
  TaggedConnection ->
  EventResponse s TaggerEvent sp ep
dbConnTask e f tc =
  maybe
    (Task (IOEvent <$> IO.hPutStrLn IO.stderr "Database connection not active."))
    (Task . fmap e . f)
    $ tc ^. connInstance

descriptorTreeEventHandler ::
  WidgetEnv TaggerModel TaggerEvent ->
  WidgetNode TaggerModel TaggerEvent ->
  TaggerModel ->
  DescriptorEvent ->
  [AppEventResponse TaggerModel TaggerEvent]
descriptorTreeEventHandler _ _ model event =
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
    RepresentativeFilePut_ r -> [model *~ descriptorModel . representativeFile .~ r]
    RepresentativeFileClear -> [model *~ descriptorModel . representativeFile .~ Nothing]
    RepresentativeFileLookup d ->
      [ Task
          ( DoDescriptorEvent . RepresentativeFilePut_
              <$> runMaybeT
                ( do
                    c <- hoistMaybe $ model ^. dbConn . connInstance
                    getRepresentative c d
                )
          )
      ]
    RepresentativeCreate f d ->
      [ dbConnTask
          IOEvent
          (\c -> createRepresentative c f d Nothing)
          (model ^. dbConn)
      ]
  where
    maybeM_ :: Monad m => (a -> m ()) -> Maybe a -> m ()
    maybeM_ = M.maybe (pure ())

-- singleFileEventHandler ::
--   WidgetEnv TaggerModel TaggerEvent ->
--   WidgetNode TaggerModel TaggerEvent ->
--   TaggerModel ->
--   SingleFileEvent ->
--   [AppEventResponse TaggerModel TaggerEvent]
-- singleFileEventHandler _ _ model event =
--   case event of
--     SingleFilePut fwt ->
--       [ model *~ (singleFileModel . singleFile) .~ Just fwt,
--         asyncEvent (DoSingleFileEvent SingleFileGetTagCounts)
--       ]
--     SingleFileMaybePut mfwt ->
--       [ model *~ (singleFileModel . singleFile) .~ mfwt,
--         asyncEvent (DoSingleFileEvent SingleFileGetTagCounts)
--       ]
--     -- SingleFileNextFromFileSelection -> --#FIXME
--     --   let !ps = cPop $ model ^. fileSelectionModel . fileSelection
--     --       !mi = cHead ps
--     --    in [ Model
--     --           . ((fileSelectionModel . fileSelection) .~ ps)
--     --           . ((singleFileModel . singleFile) .~ mi)
--     --           . (doSoloTag .~ True)
--     --           $ model,
--     --         asyncEvent (DoSingleFileEvent SingleFileGetTagCounts)
--     --       ]
--     -- SingleFilePrevFromFileSelection ->
--     --   let !ps = cDequeue $ model ^. fileSelectionModel . fileSelection
--     --       !mi = cHead ps
--     --    in [ Model
--     --           . ((fileSelectionModel . fileSelection) .~ ps)
--     --           . ((singleFileModel . singleFile) .~ mi)
--     --           . (doSoloTag .~ True)
--     --           $ model,
--     --         asyncEvent (DoSingleFileEvent SingleFileGetTagCounts)
--     --       ]
--     SingleFilePutTagCounts_ tcs -> [model *~ (singleFileModel . tagCounts) .~ tcs]
--     SingleFileGetTagCounts ->
--       [ dbConnTask
--           (DoSingleFileEvent . SingleFilePutTagCounts_)
--           ( flip
--               getDescriptorOccurrenceMap
--               ( maybeWithList
--                   (map (descriptorId . tagDescriptor) . HashSet.toList . tags) -- #FIXME
--                   $ model ^. singleFileModel . singleFile
--               )
--           )
--           (model ^. dbConn)
--       ]
--     SingleFileUntag t ->
--       [ dbConnTask IOEvent (\c -> untagWithTag c [t]) (model ^. dbConn),
--         asyncEvent (DoFileSelectionEvent FileSelectionRefresh_)
--       ]
--     SingleFileAssociateTag tWith t ->
--       [ dbConnTask
--           IOEvent
--           (\c -> associateTag c tWith t)
--           (model ^. dbConn),
--         asyncEvent (DoFileSelectionEvent FileSelectionRefresh_)
--       ]

configurationEventHandler ::
  WidgetEnv TaggerModel TaggerEvent ->
  WidgetNode TaggerModel TaggerEvent ->
  TaggerModel ->
  ConfigurationEvent ->
  [AppEventResponse TaggerModel TaggerEvent]
configurationEventHandler _ _ model event =
  case event of
    ExportAll -> [Task (IOEvent <$> exportConfig (model ^. programConfig))]

fileSelectionEventHandler ::
  WidgetEnv TaggerModel TaggerEvent ->
  WidgetNode TaggerModel TaggerEvent ->
  TaggerModel ->
  FileSelectionEvent ->
  [AppEventResponse TaggerModel TaggerEvent]
fileSelectionEventHandler _ _ model event =
  case event of
    -- FileSelectionUpdate fwts -> -- #FIXME
    --   [ model *~ (fileSelectionModel . fileSelection)
    --       .~ ( doSetAction
    --              (model ^. (fileSelectionModel . setArithmetic))
    --              fwtFileEqual
    --              (model ^. (fileSelectionModel . fileSelection))
    --              . cFromList
    --              $ fwts
    --          )
    --   ]
    -- FileSelectionPut bfwts -> --#FIXME
    --   [ model *~ (fileSelectionModel . fileSelection) .~ bfwts
    --   ]
    -- FileSelectionBufferPut fwts ->
    --   [ model *~ fileSelectionModel . fileSelection . buffer .~ fwts
    --   ]
    -- FileSelectionListPut fwts ->
    --   [ model *~ fileSelectionModel . fileSelection . list .~ fwts
    --   ]
    -- FileSelectionRefresh_ ->
    --   [ dbConnTask
    --       (DoFileSelectionEvent . FileSelectionBufferPut)
    --       (flip getRefreshedFWTs $ model ^. fileSelectionModel . fileSelection . buffer)
    --       (model ^. dbConn),
    --     dbConnTask
    --       (DoFileSelectionEvent . FileSelectionListPut)
    --       (flip getRefreshedFWTs $ model ^. fileSelectionModel . fileSelection . list)
    --       (model ^. dbConn),
    --     dbConnTask
    --       (DoSingleFileEvent . SingleFileMaybePut)
    --       ( \activeConn ->
    --           do
    --             mrefreshed <-
    --               M.maybe
    --                 (return [])
    --                 (getRefreshedFWTs activeConn . (: []))
    --                 (model ^. (singleFileModel . singleFile))
    --             return . head' $ mrefreshed
    --       )
    --       (model ^. dbConn)
    --   ]
    -- FileSelectionCommitQueryText ->
    --   [ dbConnTask
    --       (DoFileSelectionEvent . FileSelectionPut)
    --       ( \activeDbConn ->
    --           runQuery
    --             activeDbConn
    --             (model ^. fileSelectionModel . setArithmetic)
    --             (model ^. fileSelectionModel . queryCriteria)
    --             (model ^. fileSelectionModel . fileSelection)
    --             (model ^. fileSelectionModel . queryText)
    --       )
    --       (model ^. dbConn),
    --     asyncEvent (DoFileSelectionEvent FileSelectionQueryTextClear)
    --   ]
    FileSelectionClear ->
      [ model *~ (fileSelectionModel . fileSelection) .~ mempty,
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
    -- FileSelectionShuffle -> -- #FIXME
    --   let !shuffledBufferList =
    --         shuffleBufferList $ model ^. fileSelectionModel . fileSelection
    --    in [ Task
    --           ( DoFileSelectionEvent
    --               . FileSelectionBufferPut
    --               <$> fmap _buffer shuffledBufferList
    --           ),
    --         Task
    --           ( DoFileSelectionEvent
    --               . FileSelectionListPut
    --               <$> fmap _list shuffledBufferList
    --           )
    --       ]
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

-- FlipInSelectionOrdering -> -- #FIXME
--   [ model *~ fileSelectionModel . selectionDetailsOrdering
--       %~ (\(OrderingMode b d) -> OrderingMode b (next d))
--   ]
-- CycleInSelectionOrderingBy ->
--   [ model
--       *~ fileSelectionModel . selectionDetailsOrdering
--         %~ (\(OrderingMode b d) -> OrderingMode (next b) d)
--   ]

taggedConnectionEventHandler ::
  WidgetEnv TaggerModel TaggerEvent ->
  WidgetNode TaggerModel TaggerEvent ->
  TaggerModel ->
  TaggedConnectionEvent ->
  [AppEventResponse TaggerModel TaggerEvent]
taggedConnectionEventHandler _ _ model event =
  case event of
    TaggedConnectionPutLastAccess t -> [model *~ dbConn . lastAccessed .~ t]
    TaggedConnectionPutLastBackup t -> [model *~ dbConn . lastBackup .~ t]

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
        then [Event DatabaseConnect]
        else
          [ Event
              ( UpdateWindowTitleConnectionString
                  ( T.pack . IO.takeFileName . T.unpack $
                      model ^. programConfig . dbconf . dbconfPath
                  )
                  False
              )
          ]
    -- DoSingleFileEvent evt -> singleFileEventHandler wenv node model evt
    DoConfigurationEvent evt -> configurationEventHandler wenv node model evt
    DoFileSelectionEvent evt -> fileSelectionEventHandler wenv node model evt
    DoDescriptorEvent evt -> descriptorTreeEventHandler wenv node model evt
    DoTaggedConnectionEvent evt -> taggedConnectionEventHandler wenv node model evt
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
    -- ShellCmd -> -- #FIXME
    --   [ Task
    --       ( IOEvent
    --           <$> runShellCmds
    --             (words . T.unpack $ model ^. programConfig . shellCmd)
    --             ( if model ^. doSoloTag && M.isJust singlefwt
    --                 then (: []) . fwtPath . M.fromJust $ singlefwt
    --                 else selectionFwts
    --             )
    --       )
    --   ]
    --   where
    --     fwtPath = T.unpack . filePath . file
    --     selectionFwts =
    --       map fwtPath . cCollect $
    --         model ^. fileSelectionModel . fileSelection
    --     singlefwt = model ^. singleFileModel . singleFile
    IOEvent _ -> []
    TagCommitTagsString ->
      [ asyncEvent $
          if model ^. doSoloTag
            then TagCommitTagsStringDoSolo
            else TagCommitTagsStringDoSelection,
        asyncEvent TagsStringClear
      ]
    -- TagCommitTagsStringDoSolo -> -- #FIXME
    --   [ dbConnTask
    --       IOEvent
    --       ( \activeConn ->
    --           ( if isUntagMode (model ^. taggingMode)
    --               then untag
    --               else tag
    --           )
    --             activeConn
    --             (M.maybeToList $ model ^. (singleFileModel . singleFile))
    --             (parseQuery $ model ^. tagsString)
    --       )
    --       (model ^. dbConn),
    --     asyncEvent (DoFileSelectionEvent FileSelectionRefresh_)
    --   ]
    -- TagCommitTagsStringDoSelection -> -- #FIXME
    --   [ dbConnTask
    --       IOEvent
    --       ( \activeConn ->
    --           ( if isUntagMode (model ^. taggingMode)
    --               then untag
    --               else tag
    --           )
    --             activeConn
    --             (cCollect $ model ^. (fileSelectionModel . fileSelection))
    --             (parseQuery $ model ^. tagsString)
    --       )
    --       (model ^. dbConn),
    --     asyncEvent (DoFileSelectionEvent FileSelectionRefresh_)
    --   ]
    TaggingModeNext -> [model *~ taggingMode %~ next]
    TaggingModePrev -> [model *~ taggingMode %~ prev]
    NewFileTextCommit ->
      [ dbConnTask
          (DoFileSelectionEvent . FileSelectionPut)
          (\conn -> fmap bufferListFromList . addPath conn $ (model ^. newFileText))
          $ model ^. dbConn,
        model *~ newFileText .~ ""
      ]
    DatabaseInitialize ->
      [ dbConnTask
          IOEvent
          IO.runInitScript
          (model ^. dbConn)
      ]
    ToggleVisibilityMode vm ->
      [ let currentVM = model ^. programVisibility
         in model *~ programVisibility .~ (if currentVM == vm then Main else vm)
      ]
    DatabaseConnectionPut_ tc ->
      [ model *~ dbConn .~ tc,
        Event
          ( UpdateWindowTitleConnectionString
              (tc ^. connName)
              (M.isJust $ tc ^. connInstance)
          )
      ]
    DatabaseConnect ->
      [ Task $
          DatabaseConnectionPut_ <$> do
            maybe (pure ()) close $ model ^. dbConn . connInstance
            let newConnTag = T.unpack $ model ^. (programConfig . dbconf . dbconfPath)
                newConnName = IO.takeFileName newConnTag
            newConnInstance <- open newConnTag
            activateForeignKeyPragma newConnInstance
            IO.updateTaggerDBInfo newConnInstance
            lastAccessedDT <- IO.getLastAccessDateTime newConnInstance
            lastBackupDT <- IO.getLastBackupDateTime newConnInstance
            return $
              TaggedConnection
                (T.pack newConnName)
                (Just newConnInstance)
                lastAccessedDT
                lastBackupDT
      ]
        ++ ( asyncEvent . DoDescriptorEvent
               <$> [ RefreshDescriptorTree mainDescriptorTree,
                     RefreshDescriptorTree unrelatedDescriptorTree
                   ]
           )
    -- DatabaseBackup -> -- #FIXME related to the new database backup scheme, see the Unreleased CHANGELOG entry.
    --   [ dbConnTask
    --       (DoTaggedConnectionEvent . TaggedConnectionPutLastBackup)
    --       ( \c -> do
    --           IO.saveToBackup
    --             c
    --             (T.unpack $ model ^. programConfig . dbconf . dbconfBackup)
    --           IO.getLastBackupDateTime c
    --       )
    --       (model ^. dbConn)
    --   ]
    DatabaseClose ->
      [ Task
          ( IOEvent
              <$> maybe (pure ()) close (model ^. dbConn . connInstance)
          )
      ]
    DropTargetAppendText_ l df d ->
      [Model $ model & l %~ flip T.append (" " `T.append` df d)]
    UpdateWindowTitleConnectionString t active ->
      [ Request . UpdateWindow . WindowSetTitle $
          "tagger | In database: "
            !++ t
            !++ " | Active: "
            !++ if active then "Yes" else "No"
      ]
    RefreshApplication ->
      [ Event (DoDescriptorEvent (RefreshDescriptorTree mainDescriptorTree)),
        Event (DoDescriptorEvent (RefreshDescriptorTree unrelatedDescriptorTree)),
        Event (DoFileSelectionEvent FileSelectionRefresh_)
      ]

-- I will never understand how the stupid fixity stuff works
infixl 3 *~

(*~) :: a -> (a -> s) -> EventResponse s e sp ep
m *~ a = Model $ m & a

asyncEvent :: e -> EventResponse s e sp ep
asyncEvent = Task . flip (<$) emptyM
  where
    emptyM = pure ()
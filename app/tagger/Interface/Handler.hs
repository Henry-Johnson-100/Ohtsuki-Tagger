{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant if" #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant multi-way if" #-}

module Interface.Handler (
  taggerEventHandler,
) where

import Control.Lens ((%~), (&), (.~), (<|), (^.), (|>))
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Event
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.HierarchyMap (empty)
import qualified Data.HierarchyMap as HAM
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe
import Data.Model
import Data.Model.Shared
import Data.Sequence (Seq ((:<|), (:|>)))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Database.Tagger
import Interface.Handler.Internal
import Interface.Widget.Internal.Query (queryTextFieldKey)
import Interface.Widget.Internal.Selection (fileSelectionScrollWidgetNodeKey)
import Monomer
import Paths_tagger
import System.FilePath
import System.IO
import Text.TaggerQL.Expression.Engine
import Text.TaggerQL.Expression.Parser
import Util

taggerEventHandler ::
  WidgetEnv TaggerModel TaggerEvent ->
  WidgetNode TaggerModel TaggerEvent ->
  TaggerModel ->
  TaggerEvent ->
  [AppEventResponse TaggerModel TaggerEvent]
taggerEventHandler
  wenv
  node
  model@(_taggermodelConnection -> conn)
  event =
    case event of
      DoAddFileEvent e -> addFileEventHandler wenv node model e
      DoFocusedFileEvent e -> focusedFileEventHandler wenv node model e
      DoFileSelectionEvent e -> fileSelectionEventHandler wenv node model e
      DoDescriptorTreeEvent e -> descriptorTreeEventHandler wenv node model e
      DoQueryEvent e -> queryEventHandler wenv node model e
      DoTagInputEvent e -> tagInputEventHandler wenv node model e
      TaggerInit ->
        [ Event (DoDescriptorTreeEvent DescriptorTreeInit)
        , SetFocusOnKey . WidgetKey $ queryTextFieldKey
        ]
      RefreshUI ->
        [ Event . DoDescriptorTreeEvent $ RefreshBothDescriptorTrees
        , Event . DoFocusedFileEvent $ RefreshFocusedFileAndSelection
        ]
      CloseConnection -> [Task (Unit <$> close conn)]
      Unit _ -> []
      AnonymousEvent (fmap (\(TaggerAnonymousEvent e) -> e) -> es) -> es
      Mempty (TaggerLens l) -> [Model $ model & l .~ mempty]
      NextHistory (TaggerLens l) ->
        [ Model $
            model
              & l . text .~ (fromMaybe "" . getHist $ model ^. l . history)
              & l . history %~ nextHist
        ]
      PrevHistory (TaggerLens l) ->
        [ Model $
            model
              & l . text .~ (fromMaybe "" . getHist $ model ^. l . history)
              & l . history %~ prevHist
        ]
      ToggleVisibilityLabel (TaggerLens l) t ->
        [Model $ model & l %~ flip togglePaneVis (VisibilityLabel t)]
      AppendText (TaggerLens l) t ->
        [ let currentText = model ^. l
           in Model $
                model & l
                  %~ flip
                    (<>)
                    ( ( if currentText == ""
                          then ""
                          else " "
                      )
                        <> t
                    )
        ]

fileSelectionEventHandler ::
  WidgetEnv TaggerModel TaggerEvent ->
  WidgetNode TaggerModel TaggerEvent ->
  TaggerModel ->
  FileSelectionEvent ->
  [AppEventResponse TaggerModel TaggerEvent]
fileSelectionEventHandler
  wenv
  node
  model@(_taggermodelConnection -> conn)
  event =
    case event of
      ClearTaggingSelection ->
        [Model $ model & fileSelectionModel . taggingSelection .~ mempty]
      CycleNextFile ->
        case model ^. fileSelectionModel . selection of
          Seq.Empty -> []
          _ :<| Seq.Empty -> []
          (f' :<| (f :<| fs)) ->
            [ Event . DoFocusedFileEvent . PutFile $ f
            , Model $ model & fileSelectionModel . selection .~ (f <| (fs |> f'))
            ]
      CycleOrderCriteria ->
        [Model $ model & fileSelectionTagListModel . ordering %~ cycleOrderCriteria]
      CycleOrderDirection ->
        [Model $ model & fileSelectionTagListModel . ordering %~ cycleOrderDir]
      CyclePrevFile ->
        case model ^. fileSelectionModel . selection of
          Seq.Empty -> []
          (Seq.Empty :|> _) -> []
          ((f' :<| fs) :|> f) ->
            [ Event . DoFocusedFileEvent . PutFile $ f
            , Model $ model & fileSelectionModel . selection .~ (f <| (f' <| fs))
            ]
      DeleteFileFromFileSystem fk ->
        [ Task (Unit <$> rmFile conn fk)
        , Event . DoFileSelectionEvent . RemoveFileFromSelection $ fk
        ]
      DoFileSelectionWidgetEvent e -> fileSelectionWidgetEventHandler wenv node model e
      IncludeTagListInfraToPattern t -> anonymousTask $ do
        !infraDs <-
          fmap (HS.fromList . map descriptorId . concat)
            . mapM (flip getAllInfra conn . descriptorId)
            <=< flip queryForDescriptorByPattern conn
            $ t
        callback
          [Model $! model & fileSelectionTagListModel . include .~ infraDs]
      MakeFileSelectionInfoMap fseq ->
        [ let fiTuple (File fk fp) = (fromIntegral fk, constructFileInfo fp)
              m = F.toList $ fiTuple <$> fseq
           in Model $
                model
                  & fileSelectionModel
                    . fileSelectionInfoMap
                  .~ IntMap.fromList m
        ]
      PutChunkSequence ->
        [ Model $
            model & fileSelectionModel . chunkSequence
              .~ Seq.fromList
                [ 0
                .. selectionChunkLength model
                ]
        ]
      PutFilesNoCombine
        ( uncurry (Seq.><)
            . (\(x, y) -> (y, x))
            . Seq.breakl
              ( ( concreteTaggedFile $
                    model ^. focusedFileModel . focusedFile
                )
                  ==
              ) ->
            fseq
          ) ->
          [ Model $ model & fileSelectionModel . selection .~ fseq
          , Event
              ( DoFileSelectionEvent
                  (RefreshTagOccurrencesWith (fmap fileId fseq))
              )
          , Event (DoFileSelectionEvent . MakeFileSelectionInfoMap $ fseq)
          , Event
              . DoFileSelectionEvent
              . DoFileSelectionWidgetEvent
              $ ResetFileSelectionWidgetScroll
          , Event . DoFileSelectionEvent $ PutChunkSequence
          ]
            ++ ( case fseq of
                  Seq.Empty -> []
                  (f :<| _) -> [Event . DoFocusedFileEvent . PutFile $ f]
               )
      PutTagOccurrenceHashMap_ ohm ->
        [Model $ model & fileSelectionTagListModel . occurrences .~ ohm]
      RefreshSpecificFile fk ->
        [ Task
            ( maybe
                (Unit ())
                (DoFileSelectionEvent . RefreshSpecificFile_)
                <$> queryForSingleFileByFileId fk conn
            )
        ]
      RefreshSpecificFile_ f@(File fk fp) ->
        let curSeq = model ^. fileSelectionModel . selection
            maybeIx = Seq.findIndexR ((==) fk . fileId) curSeq
         in [ Model $
                model & fileSelectionModel . selection
                  %~ maybe
                    (f <|)
                    (Seq.adjust (const f))
                    maybeIx
                  & fileSelectionModel . fileSelectionInfoMap
                    . fileInfoAt (fromIntegral fk)
                    .~ constructFileInfo fp
            , Event $ DoFileSelectionEvent RefreshTagOccurrences
            ]
      RefreshFileSelection ->
        [ Event (DoFileSelectionEvent RefreshTagOccurrences)
        , Event
            ( DoFileSelectionEvent
                ( MakeFileSelectionInfoMap $
                    model ^. fileSelectionModel . selection
                )
            )
        , Event . DoFileSelectionEvent $ PutChunkSequence
        ]
      RefreshTagOccurrences ->
        [ Event . DoFileSelectionEvent . RefreshTagOccurrencesWith . fmap fileId $
            model ^. fileSelectionModel . selection
        ]
      RemoveFileFromDatabase fk ->
        [ Task (Unit <$> deleteFiles [fk] conn)
        , Event . DoFileSelectionEvent . RemoveFileFromSelection $ fk
        ]
      RemoveFileFromSelection fk ->
        let curSeq = model ^. fileSelectionModel . selection
            maybeIx = Seq.findIndexR ((==) fk . fileId) curSeq
         in maybe
              []
              ( \sindex ->
                  [ Event . DoFileSelectionEvent . PutFilesNoCombine
                      . flip Seq.deleteAt curSeq
                      $ sindex
                  ]
              )
              maybeIx
      RenameFile fk ->
        [ let newRenameText =
                model
                  ^. fileSelectionModel
                    . fileSelectionInfoMap
                    . fileInfoAt (fromIntegral fk)
                    . fileInfoRenameText
           in Task
                ( do
                    -- refetch the fk from the db,
                    -- to put the calculation in the Maybe monad
                    result <- do
                      mvFile conn fk newRenameText
                      queryForSingleFileByFileId fk conn
                    maybe
                      (return $ Unit ())
                      (return . DoFileSelectionEvent . RefreshSpecificFile_)
                      result
                )
        , Event . DoFocusedFileEvent $ RefreshFocusedFileAndSelection
        ]
      RunSelectionShellCommand ->
        [ Task
            ( Unit
                <$> runShellCmd
                  (T.strip $ model ^. shellText)
                  ( F.toList
                      ( T.unpack . filePath <$> model
                          ^. fileSelectionModel
                            . selection
                      )
                  )
            )
        ]
      RefreshTagOccurrencesWith fks ->
        [ Task
            ( DoFileSelectionEvent . PutTagOccurrenceHashMap_
                <$> foldM
                  ( \acc fk ->
                      HM.unionWith (+) acc
                        . F.foldl'
                          ( \acc' d ->
                              if not (HM.member d acc')
                                then HM.insert d 1 acc'
                                else acc'
                          )
                          HM.empty
                        <$> queryForDescriptorByFileId fk conn
                  )
                  HM.empty
                  fks
            )
        ]
      ShuffleSelection ->
        [ Task
            ( DoFileSelectionEvent . PutFilesNoCombine
                <$> shuffleSequence (model ^. fileSelectionModel . selection)
            )
        ]
      TagSelect fk ->
        [ Model $
            model & fileSelectionModel . taggingSelection
              %~ \hs -> if HS.member fk hs then HS.delete fk hs else HS.insert fk hs
        ]
      TagSelectWholeChunk ->
        [ Model $
            model & fileSelectionModel . taggingSelection %~ \hs ->
              F.foldl (\hs' f -> HS.insert (fileId f) hs') hs (getSelectionChunk model)
        ]
      ToggleSelectionView ->
        [ Model $
            model
              & fileSelectionModel
                . fileSelectionVis
              %~ toggleAltVis
        ]

addFileEventHandler ::
  WidgetEnv TaggerModel TaggerEvent ->
  WidgetNode TaggerModel TaggerEvent ->
  TaggerModel ->
  AddFileEvent ->
  [AppEventResponse TaggerModel TaggerEvent]
addFileEventHandler _wenv _wnode model@((^. connection) -> conn) e =
  case e of
    AddFiles ->
      let !currentAddFileText =
            model
              ^. fileSelectionModel
                . addFileModel
                . input
                . text
       in [ Model $
              model & fileSelectionModel . addFileModel . inProgress .~ True
                & fileSelectionModel . addFileModel . input . history
                  %~ putHist (T.strip currentAddFileText)
          , Task $
              DoAddFileEvent AddFileDone
                <$ addFiles conn currentAddFileText
          ]
    AddFileDone ->
      [Model $ model & fileSelectionModel . addFileModel . inProgress .~ False]
    AddFilePath fp ->
      [ Model $ model & fileSelectionModel . addFileModel . inProgress .~ True
      , Task $ DoAddFileEvent AddFileDone <$ addFiles conn (T.pack fp)
      ]
    PutDirectoryList fs ->
      [Model $ model & fileSelectionModel . addFileModel . directoryList .~ fs]
    ScanDirectories ->
      [Task $ DoAddFileEvent . PutDirectoryList <$> getDirectories conn]
    ToggleAddFileVisibility ->
      let !currentVis = model ^. fileSelectionModel . addFileModel . visibility
       in [ -- Scan directories if the new visibility is showing the directory list
            if hasVis VisibilityMain currentVis
              then Event . DoAddFileEvent $ ScanDirectories
              else Event (Unit ())
          , Model $ model & fileSelectionModel . addFileModel . visibility %~ toggleAltVis
          ]

queryEventHandler ::
  WidgetEnv TaggerModel TaggerEvent ->
  WidgetNode TaggerModel TaggerEvent ->
  TaggerModel ->
  QueryEvent ->
  [AppEventResponse TaggerModel TaggerEvent]
queryEventHandler _wenv _node model@((^. connection) -> conn) event =
  case event of
    RunQuery ->
      let !rawQuery = T.strip $ model ^. fileSelectionModel . queryModel . input . text
       in [ either (const (Event . Unit $ ())) runQueryExpressionTask
              . parseQueryExpression
              $ rawQuery
          , Model $
              model
                & fileSelectionModel
                  . queryModel
                  . input
                  . history
                  %~ putHist rawQuery
          ]
 where
  runQueryExpressionTask =
    Task
      . fmap
        ( DoFileSelectionEvent
            . PutFilesNoCombine
            . Seq.sortBy (\x y -> filePath x `compare` filePath y)
            . HS.foldl' (Seq.|>) Seq.empty
        )
      . yuiQLFileQueryExpression conn

fileSelectionWidgetEventHandler ::
  WidgetEnv TaggerModel TaggerEvent ->
  WidgetNode TaggerModel TaggerEvent ->
  TaggerModel ->
  FileSelectionWidgetEvent ->
  [AppEventResponse TaggerModel TaggerEvent]
fileSelectionWidgetEventHandler
  _
  _
  model
  event = case event of
    CycleNextChunk ->
      let cSeq = model ^. fileSelectionModel . chunkSequence
       in case cSeq of
            (f' :<| (f :<| fs)) ->
              [ Model $
                  model & fileSelectionModel . currentChunk .~ f
                    & fileSelectionModel . chunkSequence .~ (f <| (fs |> f'))
              ]
            _ -> []
    CyclePrevChunk ->
      let cSeq = model ^. fileSelectionModel . chunkSequence
       in case cSeq of
            (f' :<| (fs :|> f)) ->
              [ Model $
                  model & fileSelectionModel . currentChunk .~ f
                    & fileSelectionModel . chunkSequence .~ (f <| (f' <| fs))
              ]
            _ -> []
    ResetFileSelectionWidgetChunk ->
      [ Model $
          model
            & fileSelectionModel . currentChunk .~ 0
      , Event . DoFileSelectionEvent . DoFileSelectionWidgetEvent $
          ResetFileSelectionWidgetScroll
      ]
    ResetFileSelectionWidgetScroll ->
      [ Message
          (WidgetKey fileSelectionScrollWidgetNodeKey)
          ScrollReset
      ]

focusedFileEventHandler ::
  WidgetEnv TaggerModel TaggerEvent ->
  WidgetNode TaggerModel TaggerEvent ->
  TaggerModel ->
  FocusedFileEvent ->
  [AppEventResponse TaggerModel TaggerEvent]
focusedFileEventHandler
  _
  _
  model@(_taggermodelConnection -> conn)
  event =
    case event of
      DeleteTag t ->
        [ Task (Unit <$> deleteTags [t] conn)
        , Event . DoFocusedFileEvent $ RefreshFocusedFileAndSelection
        ]
      MoveTag
        (ConcreteTag oldTagKey (Descriptor dk dp) oldSubTagKey)
        newMaybeSubTagKey ->
          let (fileId -> fk) =
                concreteTaggedFile $
                  model
                    ^. focusedFileModel . focusedFile
           in [ Task $
                  DoFocusedFileEvent RefreshFocusedFileAndSelection <$ moveTagTask fk
              ]
         where
          moveTagTask :: RecordKey File -> IO ()
          moveTagTask fk = do
            result <-
              runExceptT $ do
                withExceptT
                  (const "Cannot move tags of the default file.")
                  ( guard (fk /= focusedFileDefaultRecordKey) ::
                      ExceptT String IO ()
                  )
                withExceptT
                  ( const
                      ( "Cannot move tag, "
                          ++ T.unpack dp
                          ++ ", to be a subtag of itself."
                      )
                  )
                  ( guard
                      ( maybe
                          True
                          ( \newSubTagKey ->
                              not
                                . HAM.isInfraTo newSubTagKey oldTagKey
                                . HAM.mapHierarchyMap concreteTagId
                                . concreteTaggedFileDescriptors
                                $ model ^. focusedFileModel . focusedFile
                          )
                          newMaybeSubTagKey
                      ) ::
                      ExceptT String IO ()
                  )
                withExceptT
                  ( const
                      ( "Tag, "
                          ++ T.unpack dp
                          ++ ", is already subtagged to the destination."
                      )
                  )
                  ( guard
                      ( oldSubTagKey
                          /= newMaybeSubTagKey
                      ) ::
                      ExceptT String IO ()
                  )
                newTags <-
                  lift $
                    insertTags [(fk, dk, newMaybeSubTagKey)] conn
                -- moving all old subtags to the new tag
                -- or else they will be cascade deleted when the old tag is.
                lift $ moveSubTags ((oldTagKey,) <$> newTags) conn
                lift $ deleteTags [oldTagKey] conn
            either (hPutStrLn stderr) return result
      PutConcreteFile cf@(ConcreteTaggedFile (File _ fp) _) ->
        [ Model $
            model
              & focusedFileModel . focusedFile .~ cf
              & focusedFileModel . renderability .~ getRenderability fp
        ]
      PutFile (File fk _) ->
        [ Task
            ( do
                cft <- runMaybeT $ queryForConcreteTaggedFileWithFileId fk conn
                maybe
                  ( DoFocusedFileEvent . PutConcreteFile <$> do
                      defaultFile <- T.pack <$> getDataFileName focusedFileDefaultDataFile
                      return $
                        ConcreteTaggedFile
                          ( File
                              focusedFileDefaultRecordKey
                              defaultFile
                          )
                          empty
                  )
                  (return . DoFocusedFileEvent . PutConcreteFile)
                  cft
            )
        ]
      RefreshFocusedFileAndSelection ->
        [ Event
            . DoFocusedFileEvent
            . PutFile
            . concreteTaggedFile
            $ model ^. focusedFileModel . focusedFile
        , Event . DoFileSelectionEvent $ RefreshFileSelection
        ]
      RunFocusedFileShellCommand ->
        [ Task
            ( Unit
                <$> runShellCmd
                  (T.strip $ model ^. shellText)
                  [ T.unpack . filePath . concreteTaggedFile $
                      model
                        ^. focusedFileModel
                          . focusedFile
                  ]
            )
        ]
      -- Should:
      -- submit a tag in the db
      -- refresh the focused file detail widget
      -- refresh the selection if tag counts are displayed (?)
      TagFile dk mtk ->
        let (File fk _) =
              concreteTaggedFile $
                model
                  ^. focusedFileModel . focusedFile
         in [ Task
                ( Unit
                    <$> if or [fk == focusedFileDefaultRecordKey]
                      then hPutStrLn stderr "Cannot tag the default file."
                      else void $ insertTags [(fk, dk, mtk)] conn
                )
            , Event . DoFocusedFileEvent $ RefreshFocusedFileAndSelection
            ]
      UnSubTag tk ->
        [ Task (Unit <$> unSubTags [tk] conn)
        , Event . DoFocusedFileEvent $ RefreshFocusedFileAndSelection
        ]

tagInputEventHandler ::
  WidgetEnv TaggerModel TaggerEvent ->
  WidgetNode TaggerModel TaggerEvent ->
  TaggerModel ->
  TagInputEvent ->
  [AppEventResponse TaggerModel TaggerEvent]
tagInputEventHandler _wenv _wnode model@((^. connection) -> conn) e =
  case e of
    RunTagExpression ->
      let !rawTagText = T.strip $ model ^. tagInputModel . input . text
          focusedFileFK =
            fileId
              . concreteTaggedFile
              $ model ^. focusedFileModel . focusedFile
          !fks =
            if model ^. tagInputModel . isTagSelection
              then
                focusedFileFK :
                HS.toList (model ^. fileSelectionModel . taggingSelection)
              else [focusedFileFK]
       in ( if model ^. tagInputModel . isTagDelete
              then
                [ Task $
                    DoFocusedFileEvent RefreshFocusedFileAndSelection
                      <$ yuiQLDeleteTags conn fks rawTagText
                ]
              else
                [ Task $
                    DoFocusedFileEvent RefreshFocusedFileAndSelection
                      <$ mapM (\fk -> yuiQLTagFile fk conn rawTagText) fks
                ]
          )
            ++ [ Model $
                  model & tagInputModel . input . history %~ putHist rawTagText
                    & tagInputModel . input . text .~ mempty
               ]
    ToggleTagInputOptionPane ->
      [ Model $
          model & tagInputModel . visibility
            %~ flip togglePaneVis (VisibilityLabel tagInputOptionPaneLabel)
      ]

{- |
 Performs some IO then executes the returned 'AppEventResponse`s

 The list of 'AppEventResponse`s is like a callback to be executed after the
 IO body is executed.
-}
anonymousTask ::
  IO [AppEventResponse TaggerModel TaggerEvent] ->
  [EventResponse s TaggerEvent sp ep]
anonymousTask = (: []) . Task . fmap anonymousEvent

{- |
 'return` alias
-}
callback :: Monad m => a -> m a
callback = return

-- this is kind of stupid but whatever.
getRenderability :: Text -> Renderability
getRenderability (takeExtension . T.unpack . T.toLower -> ext)
  | ext `elem` [".jpg", ".png", ".jfif", ".bmp", ".gif", ".jpeg"] = RenderAsImage
  | ext `elem` [".mp3", ".mp4", ".webm", ".mkv", ".m4v", ".wav", ".flac", ".ogg"] =
    RenderingNotSupported
  | otherwise = RenderAsText

descriptorTreeEventHandler ::
  WidgetEnv TaggerModel TaggerEvent ->
  WidgetNode TaggerModel TaggerEvent ->
  TaggerModel ->
  DescriptorTreeEvent ->
  [AppEventResponse TaggerModel TaggerEvent]
descriptorTreeEventHandler
  _
  _
  model@(_taggermodelConnection -> conn)
  event =
    case event of
      CreateRelation (Descriptor mk _) (Descriptor ik _) ->
        [ Task
            ( Unit <$> do
                insertDescriptorRelation mk ik conn
            )
        , Event (DoDescriptorTreeEvent RefreshBothDescriptorTrees)
        ]
      DeleteDescriptor (Descriptor dk _) ->
        [ Task (Unit <$> deleteDescriptors [dk] conn)
        , Event (DoDescriptorTreeEvent RefreshBothDescriptorTrees)
        ]
      DescriptorTreeInit ->
        [ Event (DoDescriptorTreeEvent RefreshUnrelated)
        , Event
            ( DoDescriptorTreeEvent
                ( RequestFocusedNode "#META#"
                )
            )
        , Task
            ( DoDescriptorTreeEvent . PutUnrelatedNode_
                <$> (head <$> queryForDescriptorByPattern "#UNRELATED#" conn)
            )
        ]
      InsertDescriptor ->
        let !newDesText =
              T.strip $
                model ^. descriptorTreeModel . newDescriptorText
         in [ Task
                ( DoDescriptorTreeEvent RefreshBothDescriptorTrees
                    <$ yuiQLCreateDescriptors conn newDesText
                )
            , Model $ model & descriptorTreeModel . newDescriptorText .~ mempty
            ]
      PutFocusedTree_ nodeName ds desInfoMap ->
        [ Model $
            model
              & descriptorTreeModel . focusedTree .~ ds
              & descriptorTreeModel . focusedNode .~ nodeName
              & descriptorTreeModel . descriptorInfoMap %~ IntMap.union desInfoMap
        ]
      PutUnrelated_ ds desInfoMap ->
        [ Model $
            model & descriptorTreeModel . unrelated .~ ds
              & descriptorTreeModel . descriptorInfoMap %~ IntMap.union desInfoMap
        ]
      PutUnrelatedNode_ d -> [Model $ model & descriptorTreeModel . unrelatedNode .~ d]
      RefreshBothDescriptorTrees ->
        [ Model $ model & descriptorTreeModel . descriptorInfoMap .~ IntMap.empty
        , Event (DoDescriptorTreeEvent RefreshUnrelated)
        , Event (DoDescriptorTreeEvent RefreshFocusedTree)
        ]
      RefreshFocusedTree ->
        [ Event
            ( DoDescriptorTreeEvent
                ( RequestFocusedNode . descriptor $
                    model ^. descriptorTreeModel . focusedNode
                )
            )
        ]
      RefreshUnrelated ->
        [ Task
            ( DoDescriptorTreeEvent . uncurry PutUnrelated_ <$> do
                unrelatedDs <- queryForDescriptorByPattern "#UNRELATED#" conn
                ds <-
                  concat
                    <$> mapM
                      (flip getInfraChildren conn . descriptorId)
                      unrelatedDs
                dsInfos <- IntMap.unions <$> mapM (toDescriptorInfo conn) ds
                return (ds, dsInfos)
            )
        ]
      RequestFocusedNode p ->
        [ Task
            ( DoDescriptorTreeEvent . (\(x, y, z) -> PutFocusedTree_ x y z) <$> do
                ds <- queryForDescriptorByPattern p conn
                d <-
                  maybe
                    (head <$> queryForDescriptorByPattern "#ALL#" conn)
                    return
                    . head'
                    $ ds
                ids <- getInfraChildren (descriptorId d) conn
                idsInfoMap <- IntMap.unions <$> mapM (toDescriptorInfo conn) ids
                return (d, ids, idsInfoMap)
            )
        ]
      RequestFocusedNodeParent ->
        [ Task
            ( do
                pd <-
                  runMaybeT $
                    getMetaParent
                      (descriptorId $ model ^. descriptorTreeModel . focusedNode)
                      conn
                maybe
                  (pure (Unit ()))
                  (pure . DoDescriptorTreeEvent . RequestFocusedNode . descriptor)
                  pd
            )
        ]
      UpdateDescriptor rkd@(RecordKey (fromIntegral -> dk)) ->
        let updateText =
              T.strip $
                model
                  ^. descriptorTreeModel
                    . descriptorInfoMap
                    . descriptorInfoAt dk
                    . renameText
         in if T.null updateText
              then []
              else
                [ Task
                    ( Unit
                        <$> updateDescriptors [(updateText, rkd)] conn
                    )
                , Event (DoDescriptorTreeEvent RefreshBothDescriptorTrees)
                ]

toDescriptorInfo :: TaggedConnection -> Descriptor -> IO (IntMap.IntMap DescriptorInfo)
toDescriptorInfo tc (Descriptor dk p) = do
  let consDes b = DescriptorInfo b p
  di <- consDes <$> hasInfraRelations dk tc
  return $ IntMap.singleton (fromIntegral dk) di

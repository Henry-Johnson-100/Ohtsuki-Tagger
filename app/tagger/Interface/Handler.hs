{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}

module Interface.Handler (
  taggerEventHandler,
) where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Config
import Data.Event
import Data.HierarchyMap (empty)
import Data.Model
import Data.Model.Shared
import Data.Text (Text)
import qualified Data.Text as T
import Database.Tagger
import Monomer
import Paths_tagger
import System.FilePath
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
      DoFocusedFileEvent e -> focusedFileEventHandler wenv node model e
      DoDescriptorTreeEvent e -> descriptorTreeEventHandler wenv node model e
      TaggerInit -> [Event (DoDescriptorTreeEvent DescriptorTreeInit)]
      RefreshUI ->
        [ Event (DoDescriptorTreeEvent RefreshBothDescriptorTrees)
        ]
      ToggleMassOperate -> [Model $ model & isMassOperation %~ not]
      ToggleTagMode -> [Model $ model & isTagMode %~ not]
      CloseConnection -> [Task (IOEvent <$> close conn)]
      IOEvent _ -> []
      ClearTextField (TaggerLens l) -> [Model $ model & l .~ ""]

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
      PutFocusedFile (File fk _) ->
        [ Task
            ( do
                cft <- runMaybeT $ queryForConcreteTaggedFileWithFileId fk conn
                maybe
                  ( DoFocusedFileEvent . PutFocusedFile_ <$> do
                      defaultFile <- T.pack <$> getDataFileName focusedFileDefaultDataFile
                      return $ ConcreteTaggedFile (File (-1) defaultFile) empty
                  )
                  (return . DoFocusedFileEvent . PutFocusedFile_)
                  cft
            )
        ]
      PutFocusedFile_ cf@(ConcreteTaggedFile (File _ fp) _) ->
        [ Model $
            model & focusedFileModel . focusedFile .~ cf
              & focusedFileModel . renderability .~ getRenderability fp
        ]

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
            ( IOEvent <$> do
                insertDescriptorRelation mk ik conn
            )
        , Event (DoDescriptorTreeEvent RefreshBothDescriptorTrees)
        ]
      DeleteDescriptor (Descriptor dk _) ->
        [ Task (IOEvent <$> deleteDescriptors [dk] conn)
        , Event (DoDescriptorTreeEvent RefreshBothDescriptorTrees)
        ]
      DescriptorTreeInit ->
        [ Event (DoDescriptorTreeEvent RefreshUnrelated)
        , Event
            ( DoDescriptorTreeEvent
                ( RequestFocusedNode $
                    model
                      ^. conf
                        . descriptorTreeConf
                        . treeRootRequest
                )
            )
        , Task
            ( DoDescriptorTreeEvent . PutUnrelatedNode_
                <$> (head <$> queryForDescriptorByPattern "#UNRELATED#" conn)
            )
        ]
      InsertDescriptor ->
        [ Task
            ( IOEvent <$> do
                let newDesText =
                      T.words
                        . T.strip
                        $ model ^. descriptorTreeModel . newDescriptorText
                unless (null newDesText) (insertDescriptors newDesText conn)
            )
        , Event (DoDescriptorTreeEvent RefreshBothDescriptorTrees)
        , Event . ClearTextField $ TaggerLens (descriptorTreeModel . newDescriptorText)
        ]
      PutFocusedTree_ nodeName ds ->
        [ Model $
            model
              & descriptorTreeModel . focusedTree .~ ds
              & descriptorTreeModel . focusedNode .~ nodeName
        ]
      PutUnrelated_ ds -> [Model $ model & descriptorTreeModel . unrelated .~ ds]
      PutUnrelatedNode_ d -> [Model $ model & descriptorTreeModel . unrelatedNode .~ d]
      PutUpdateDescriptorFrom d ->
        [ Model $
            model
              & descriptorTreeModel . updateDescriptorFrom ?~ d
        ]
      RefreshBothDescriptorTrees ->
        [ Event (DoDescriptorTreeEvent RefreshUnrelated)
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
            ( DoDescriptorTreeEvent . PutUnrelated_ <$> do
                unrelatedDs <- queryForDescriptorByPattern "#UNRELATED#" conn
                ds <-
                  concat
                    <$> mapM
                      (flip getInfraChildren conn . descriptorId)
                      unrelatedDs
                mapM (toDescriptorInfo conn) ds
            )
        ]
      RequestFocusedNode p ->
        [ Task
            ( DoDescriptorTreeEvent . uncurry PutFocusedTree_ <$> do
                ds <- queryForDescriptorByPattern p conn
                d <-
                  maybe
                    (head <$> queryForDescriptorByPattern "#ALL#" conn)
                    return
                    . head'
                    $ ds
                ids <- getInfraChildren (descriptorId d) conn
                idsInfo <- mapM (toDescriptorInfo conn) ids
                return (d, idsInfo)
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
                  (pure (IOEvent ()))
                  (pure . DoDescriptorTreeEvent . RequestFocusedNode . descriptor)
                  pd
            )
        ]
      ToggleDescriptorTreeVisibility l ->
        [ let currentVis = model ^. visibilityModel . descriptorTreeVis
           in Model $
                model & visibilityModel . descriptorTreeVis
                  .~ ( if currentVis == VisibilityMain
                        then VisibilityLabel l
                        else VisibilityMain
                     )
        ]
      UpdateDescriptor ->
        maybe
          []
          ( \d ->
              let updateText = T.strip $ model ^. descriptorTreeModel . updateDescriptorTo
               in if T.null updateText
                    then []
                    else
                      [ Task
                          ( IOEvent
                              <$> updateDescriptors [(updateText, descriptorId d)] conn
                          )
                      , Event
                          . ClearTextField
                          $ TaggerLens (descriptorTreeModel . updateDescriptorTo)
                      , Model $
                          model
                            & descriptorTreeModel . updateDescriptorFrom .~ Nothing
                      , Event (DoDescriptorTreeEvent RefreshBothDescriptorTrees)
                      ]
          )
          (model ^. descriptorTreeModel . updateDescriptorFrom)

toDescriptorInfo :: TaggedConnection -> Descriptor -> IO DescriptorWithInfo
toDescriptorInfo tc d@(Descriptor dk _) =
  DescriptorWithInfo d <$> hasInfraRelations dk tc

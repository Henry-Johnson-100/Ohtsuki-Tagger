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
import Data.Model
import qualified Data.Text as T
import Database.Tagger
import Monomer
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
      RefreshBothDescriptorTrees ->
        [ Event (DoDescriptorTreeEvent RefreshUnrelated)
        , Event (DoDescriptorTreeEvent RefreshFocusedTree)
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
      PutUnrelated_ ds -> [Model $ model & descriptorTreeModel . unrelated .~ ds]
      PutUnrelatedNode_ d -> [Model $ model & descriptorTreeModel . unrelatedNode .~ d]
      RefreshFocusedTree ->
        [ Event
            ( DoDescriptorTreeEvent
                ( RequestFocusedNode . descriptor $
                    model ^. descriptorTreeModel . focusedNode
                )
            )
        ]
      PutFocusedTree_ nodeName ds ->
        [ Model $
            model
              & descriptorTreeModel . focusedTree .~ ds
              & descriptorTreeModel . focusedNode .~ nodeName
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
      CreateRelation (Descriptor mk _) (Descriptor ik _) ->
        [ Task
            ( IOEvent <$> do
                insertDescriptorRelation mk ik conn
            )
        , Event (DoDescriptorTreeEvent RefreshBothDescriptorTrees)
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
      PutUpdateDescriptorFrom d ->
        [ Model $
            model
              & descriptorTreeModel . updateDescriptorFrom ?~ d
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
      DeleteDescriptor (Descriptor dk _) ->
        [ Task (IOEvent <$> deleteDescriptors [dk] conn)
        , Event (DoDescriptorTreeEvent RefreshBothDescriptorTrees)
        ]

toDescriptorInfo :: TaggedConnection -> Descriptor -> IO DescriptorWithInfo
toDescriptorInfo tc d = do
  ch <- getInfraChildren (descriptorId d) tc
  return . DescriptorWithInfo d . not . null $ ch
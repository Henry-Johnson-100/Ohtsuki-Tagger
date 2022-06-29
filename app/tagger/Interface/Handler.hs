{-# LANGUAGE OverloadedStrings #-}

module Interface.Handler (
  taggerEventHandler,
) where

import Control.Lens
import Data.Config
import Data.Event
import Data.Model
import Database.Tagger
import Monomer
import Util

taggerEventHandler ::
  WidgetEnv TaggerModel TaggerEvent ->
  WidgetNode TaggerModel TaggerEvent ->
  TaggerModel ->
  TaggerEvent ->
  [AppEventResponse TaggerModel TaggerEvent]
taggerEventHandler wenv node model event =
  case event of
    DoDescriptorTreeEvent e -> descriptorTreeEventHandler wenv node model e
    TaggerInit -> [Event (DoDescriptorTreeEvent DescriptorTreeInit)]
    ToggleMassOperate -> [Model $ model & isMassOperation %~ not]
    ToggleTagMode -> [Model $ model & isTagMode %~ not]
    CloseConnection -> [Task (IOEvent <$> close (model ^. connection))]
    IOEvent _ -> []

descriptorTreeEventHandler ::
  WidgetEnv TaggerModel TaggerEvent ->
  WidgetNode TaggerModel TaggerEvent ->
  TaggerModel ->
  DescriptorTreeEvent ->
  [AppEventResponse TaggerModel TaggerEvent]
descriptorTreeEventHandler _ _ model event =
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
      ]
    RefreshBothDescriptorTrees ->
      [ Event (DoDescriptorTreeEvent RefreshUnrelated)
      , Event (DoDescriptorTreeEvent RefreshFocusedTree)
      ]
    RefreshUnrelated ->
      [ Task
          ( DoDescriptorTreeEvent . PutUnrelated_ <$> do
              let conn = model ^. connection
              unrelatedDs <- queryForDescriptorByPattern "#UNRELATED#" conn
              ds <- concat <$> mapM (flip getInfraChildren conn . descriptorId) unrelatedDs
              mapM (toDescriptorInfo conn) ds
          )
      ]
    PutUnrelated_ ds -> [Model $ model & descriptorTreeModel . unrelated .~ ds]
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
              let conn = model ^. connection
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

toDescriptorInfo :: TaggedConnection -> Descriptor -> IO DescriptorWithInfo
toDescriptorInfo tc d = do
  ch <- getInfraChildren (descriptorId d) tc
  return . DescriptorWithInfo d . not . null $ ch
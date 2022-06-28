{-# LANGUAGE OverloadedStrings #-}

module Interface.Handler (
  taggerEventHandler,
) where

import Control.Lens
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
      , Event (DoDescriptorTreeEvent RefreshFocusedTree)
      ]
    RefreshUnrelated ->
      [ Task
          ( DoDescriptorTreeEvent . PutUnrelated_ <$> do
              let conn = model ^. connection
              unrelatedDs <- queryForDescriptorByPattern "#UNRELATED#" conn
              concat <$> mapM (flip getAllInfra conn . descriptorId) unrelatedDs
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
              ids <- getAllInfra (descriptorId d) conn
              return (d, ids)
          )
      ]
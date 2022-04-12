{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Node.Micro.DescriptorTree where

import qualified Data.List as L
import qualified Data.Text as T
import Database.Tagger.Type
import Monomer
import Node.Micro.Button
import Node.Micro.Colors
import Node.Micro.Prim
import Type.Model

generalDescriptorTreeWidget ::
  WidgetModel s =>
  DescriptorTree ->
  [WidgetNode s TaggerEvent] ->
  (Descriptor -> WidgetNode s TaggerEvent) ->
  WidgetNode s TaggerEvent
generalDescriptorTreeWidget tr bs dAction =
  flip styleBasic [border 1 textBlack] . box_ [alignTop, alignLeft] $
    hstack_
      []
      [ vsplit (vstack_ [] bs, spacer),
        separatorLine,
        descriptorTreeWidget (sortChildren tr) dAction
      ]

explorableDescriptorTreeWidget ::
  WidgetModel s =>
  DescriptorTree ->
  WidgetNode s TaggerEvent
explorableDescriptorTreeWidget tr =
  dropTarget
    ( \d' ->
        maybe
          (PutExtern ())
          (\m' -> DescriptorCreateRelation [m'] [d'])
          (getNode tr)
    )
    $ generalDescriptorTreeWidget
      tr
      [ requestDescriptorTreeButton "#ALL#",
        descriptorTreePutParentButton,
        descriptorDeleteDroppableButton
      ]
      requestDescriptorTreeButtonForTreeLeaf

unrelatedDescriptorTreeWidget ::
  WidgetModel s =>
  DescriptorTree ->
  WidgetNode s TaggerEvent
unrelatedDescriptorTreeWidget tr =
  dropTarget (\d' -> DescriptorUnrelate [d']) $
    generalDescriptorTreeWidget
      tr
      [refreshUnrelatedDescriptorTreeButton]
      requestDescriptorTreeButtonForTreeLeaf -- #TODO change this

treeLeafDescriptorWidget ::
  WidgetModel s =>
  Color ->
  Int ->
  Descriptor ->
  (Descriptor -> WidgetNode s TaggerEvent) ->
  WidgetNode s TaggerEvent
treeLeafDescriptorWidget tc l d a =
  hstack_ [] $
    [ label (T.replicate l "--" !++ "|"),
      draggable d $
        a d
          `styleBasic` [ textColor tc,
                         bgColor bgDefault,
                         border 0 bgDefault,
                         padding 0
                       ]
          `styleHover` [bgColor bgLightGray]
    ]

descriptorTreeWidget ::
  (WidgetModel s) =>
  DescriptorTree ->
  (Descriptor -> WidgetNode s TaggerEvent) ->
  WidgetNode s TaggerEvent
descriptorTreeWidget tr dAction =
  box . stdScroll . flip styleBasic [textFont "Regular"] . buildTreeWidget dAction $ tr
  where
    buildTreeWidget ::
      (WidgetModel s) =>
      (Descriptor -> WidgetNode s TaggerEvent) ->
      DescriptorTree ->
      WidgetNode s TaggerEvent
    buildTreeWidget action = buildTreeWidgetAccum 0 (vstack []) action
      where
        buildTreeWidgetAccum ::
          (WidgetModel s) =>
          Int ->
          WidgetNode s TaggerEvent ->
          (Descriptor -> WidgetNode s TaggerEvent) ->
          DescriptorTree ->
          WidgetNode s TaggerEvent
        buildTreeWidgetAccum l acc action tr =
          case tr of
            NullTree -> acc
            Infra d ->
              vstack
                [ acc,
                  treeLeafDescriptorWidget
                    textBlack
                    l
                    d
                    action
                ]
            Meta d cs ->
              appendVStack
                ( vstack
                    [ acc,
                      hstack
                        [ treeLeafDescriptorWidget
                            textBlue
                            l
                            d
                            action
                        ]
                    ]
                )
                ( vstack $
                    map
                      ( \c ->
                          case c of
                            Infra d' ->
                              treeLeafDescriptorWidget
                                textBlack
                                (l + 1)
                                d'
                                action
                            Meta d' _ ->
                              treeLeafDescriptorWidget
                                textBlue
                                (l + 1)
                                d'
                                action
                            NullTree ->
                              spacer
                                `styleBasic` [padding 0, border 0 bgDefault]
                      )
                      cs
                )
    appendVStack x y = vstack [x, y]

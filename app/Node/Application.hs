{-# LANGUAGE FlexibleContexts #-}
{-# HLINT ignore "Use lambda" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant flip" #-}
{-# HLINT ignore "Redundant $" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Node.Application
  ( themeConfig,
    fileSelectionWidget,
    fileSinglePreviewWidget,
    descriptorTreeWidget,
    configPanel,
    queryWidget,
  )
where

import Control.Lens ((^.))
import Data.List (foldl', map)
import Data.Text (Text, append, intercalate, pack, replicate)
import qualified Database.Tagger.Type as TaggerNew.Type
import Monomer
import Node.Color
import Node.Micro
import Type.Model
import Prelude hiding (concat, replicate, unlines, unwords)

class GetPlainText g where
  getPlainText :: g -> Text

instance GetPlainText TaggerNew.Type.File where
  getPlainText = TaggerNew.Type.filePath

instance GetPlainText TaggerNew.Type.Descriptor where
  getPlainText = TaggerNew.Type.descriptor

instance GetPlainText TaggerNew.Type.FileWithTags where
  getPlainText = getPlainText . TaggerNew.Type.file

themeConfig :: [AppConfig e]
themeConfig =
  [ appWindowTitle "Tagger",
    appTheme lightTheme,
    appFontDef "Regular" "/usr/local/share/fonts/i/iosevka_light.ttf",
    appFontDef "Thin" "/usr/local/share/fonts/i/iosevka_thin.ttf",
    appFontDef "Bold" "/usr/local/share/fonts/i/iosevka_bold.ttf"
  ]

(!++) :: Text -> Text -> Text
(!++) = Data.Text.append

configPanel ::
  ( WidgetModel s,
    HasFileSetArithmetic s FileSetArithmetic,
    HasQueryCriteria s QueryCriteria,
    HasFileSelectionQuery s Text,
    HasShellCmd s Text,
    HasTagsString s Text
  ) =>
  WidgetNode s TaggerEvent
configPanel =
  box . vgrid $
    [ shellCmdWidget
    ]

descriptorTreeWidget ::
  (WidgetModel s, HasDescriptorTree s TaggerNew.Type.DescriptorTree) => s -> WidgetNode s TaggerEvent
descriptorTreeWidget model =
  box . stdScroll . flip styleBasic [textFont "Regular"] $
    vstack [resetDescriptorTreeButton, buildTreeWidget (model ^. descriptorTree)]
  where
    buildTreeWidget :: (WidgetModel s) => TaggerNew.Type.DescriptorTree -> WidgetNode s TaggerEvent
    buildTreeWidget = buildTreeWidgetAccum 0 (vstack [])
      where
        buildTreeWidgetAccum ::
          (WidgetModel s) =>
          Int ->
          WidgetNode s TaggerEvent ->
          TaggerNew.Type.DescriptorTree ->
          WidgetNode s TaggerEvent
        buildTreeWidgetAccum l acc tr =
          case tr of
            TaggerNew.Type.NullTree -> acc
            TaggerNew.Type.Infra d -> vstack [acc, makeDepthWidget textBlack l d]
            TaggerNew.Type.Meta d cs ->
              appendVStack
                (vstack [acc, hstack [makeDepthWidget textBlue l d]])
                ( vstack $
                    map
                      ( \c ->
                          case c of
                            TaggerNew.Type.Infra d' -> makeDepthWidget textBlack (l + 1) d'
                            TaggerNew.Type.Meta d' _ -> makeDepthWidget textBlue (l + 1) d'
                            TaggerNew.Type.NullTree ->
                              spacer
                                `styleBasic` [padding 0, border 0 bgDefault]
                      )
                      cs
                )
        makeDepthWidget textColor' l' d' =
          hstack
            [ label (replicate l' "--" !++ "|"),
              dButton d'
                `styleBasic` [ textColor textColor',
                               bgColor bgDefault,
                               border 0 bgDefault
                             ]
                `styleHover` [bgColor bgLightGray],
              appendToQueryButton (TaggerNew.Type.descriptor d')
            ]
    appendVStack x y = vstack [x, y]
    dButton :: (WidgetModel s) => TaggerNew.Type.Descriptor -> WidgetNode s TaggerEvent
    dButton d =
      flip styleBasic [padding 0, bgColor bgDefault, border 0 bgDefault]
        . flip styledButton (TaggerNew.Type.descriptor d)
        . RequestDescriptorTree
        . TaggerNew.Type.descriptor
        $ d

fileSelectionWidget :: (WidgetModel s) => [TaggerNew.Type.FileWithTags] -> WidgetNode s TaggerEvent
fileSelectionWidget fwts =
  let fileWithTagsZone =
        map
          (\fwt -> fileWithTagWidget [previewButton fwt, selectButton fwt] fwt)
      fileWithTagsStack = stdScroll $ box_ [] . vstack . fileWithTagsZone $ fwts
   in stdDelayTooltip "File Database" fileWithTagsStack
  where
    -- A widget that shows a FileWithTags, an arbitrary number of buttons
    -- and sizes appropriately to the parent container
    fileWithTagWidget ::
      (WidgetModel s) =>
      [WidgetNode s TaggerEvent] ->
      TaggerNew.Type.FileWithTags ->
      WidgetNode s TaggerEvent
    fileWithTagWidget bs fwt =
      let _temp x = const . label $ ""
          buttonGridNode bs' =
            box_ [alignLeft] $ hstack_ [] bs'
          fileNode f' = box_ [alignLeft] $ flip label_ [ellipsis] (TaggerNew.Type.filePath $ f')
          tagsNode ts' =
            box_ [alignTop, alignLeft] $
              flip
                label_
                []
                (intercalate ", " . map TaggerNew.Type.descriptor $ ts')
                `styleBasic` [textColor textBlue]
          fwtSplitNode (fn', tn') =
            box_ [alignLeft] $ vsplit_ [] $ (stdScroll fn', stdScroll tn')
       in box_
            [alignLeft]
            $ vstack_
              []
              [ hstack_ [] $
                  [ buttonGridNode bs,
                    fwtSplitNode (fileNode . TaggerNew.Type.file $ fwt, tagsNode . TaggerNew.Type.tags $ fwt)
                  ],
                separatorLine
              ]

fileSinglePreviewWidget ::
  (WidgetModel s, HasFileSingle s (Maybe TaggerNew.Type.FileWithTags), HasDoSoloTag s Bool) =>
  s ->
  WidgetNode s TaggerEvent
fileSinglePreviewWidget = imageZone
  where
    imageZone ::
      (WidgetModel s, HasFileSingle s (Maybe TaggerNew.Type.FileWithTags), HasDoSoloTag s Bool) =>
      s ->
      WidgetNode s TaggerEvent
    imageZone model =
      box_ [onClick ToggleDoSoloTag]
        . vsplit_ []
        $ (imagePreview model, doSoloTagCheckBox)
      where
        imagePreview ::
          (WidgetModel s, WidgetEvent e, HasFileSingle s (Maybe TaggerNew.Type.FileWithTags)) =>
          s ->
          WidgetNode s e
        imagePreview m' =
          box_
            []
            $ maybe
              (label "No Preview")
              (flip image_ [alignBottom, fitEither] . getPlainText)
              (m' ^. fileSingle)
        doSoloTagCheckBox ::
          (WidgetModel s, HasDoSoloTag s Bool) => WidgetNode s TaggerEvent
        doSoloTagCheckBox =
          box_
            [alignCenter]
            $ labeledCheckbox_
              "Solo Tagging Mode"
              doSoloTag
              [textRight, maxLines 1, ellipsis]
              `styleBasic` [ textFont "Thin",
                             paddingB 5,
                             paddingT 0,
                             border 0 bgDefault,
                             radius 0
                           ]

queryWidget ::
  ( WidgetModel s,
    HasFileSelectionQuery s Text,
    HasQueryCriteria s QueryCriteria,
    HasFileSetArithmetic s FileSetArithmetic
  ) =>
  WidgetNode s TaggerEvent
queryWidget =
  keystroke [("Enter", FileSelectionCommitQuery)] $
    hgrid_
      []
      [ box_ [] . hstack_ [] $ [clearSelectionButton, commitQueryButton, queryTextField],
        box_ [] $
          hstack_ [] [setQueryCriteriaDropdown, setArithmeticDropdown]
      ]

tagCommitWidget ::
  ( WidgetModel s,
    HasTagsString s Text
  ) =>
  WidgetNode s TaggerEvent
tagCommitWidget =
  keystroke [("Enter", TagCommitTagsString)]
    . hstack_ []
    $ [tagCommitButton, tagsStringTextField]
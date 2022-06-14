{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# HLINT ignore "Redundant $" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Node.Micro where

import Control.Lens ((^.))
import qualified Data.List as L
import qualified Data.Text as T
import Database.Tagger.Type
  ( Descriptor (descriptor),
    DescriptorTree (..),
    File (filePath),
    FileWithTags (file),
    Representative (repDescriptorId, repFileId),
    getNode,
    sortChildren,
  )
import Monomer
  ( CmbAlignCenter (alignCenter),
    CmbAlignLeft (alignLeft),
    CmbAlignTop (alignTop),
    CmbBgColor (bgColor),
    CmbBorder (border),
    CmbEllipsis (ellipsis),
    CmbFitHeight (fitHeight),
    CmbPadding (padding),
    CmbStyleBasic (styleBasic),
    CmbStyleHover (styleHover),
    CmbTextColor (textColor),
    CmbTextFont (textFont),
    CmbTextLeft (textLeft),
    CmbTextSize (textSize),
    CmbWheelRate (wheelRate),
    Color (Color),
    StyleState,
    WidgetEvent,
    WidgetModel,
    WidgetNode,
    black,
    blue,
    box,
    box_,
    button,
    draggable,
    dropTarget,
    dropTargetStyle,
    dropTarget_,
    dropdown,
    hstack,
    hstack_,
    image_,
    keystroke,
    keystroke_,
    label,
    label_,
    labeledCheckbox,
    lightGray,
    numericField,
    scroll_,
    separatorLine,
    spacer,
    textField,
    textField_,
    tooltipDelay,
    tooltip_,
    vsplit,
    vstack,
    vstack_,
    white,
  )
import Node.Color (yuiBlue, yuiOrange, yuiYellow)
import Type.BufferList
import Type.Config (DescriptorTreeConfig, TaggerConfig)
import Type.Model
import Util.Core
  ( (!++),
  )

type TaggerWidget = WidgetNode TaggerModel TaggerEvent

getPathComponents :: Int -> T.Text -> T.Text
getPathComponents n p =
  let !brokenPath = T.splitOn "/" p
      !droppedDirs = length brokenPath - n
   in (!++) ((T.pack . show) droppedDirs !++ ".../")
        . T.intercalate "/"
        . drop droppedDirs
        $ brokenPath

class GetPlainText g where
  getPlainText :: g -> T.Text

instance GetPlainText File where
  getPlainText = filePath

instance GetPlainText Descriptor where
  getPlainText = descriptor

instance GetPlainText FileWithTags where
  getPlainText = getPlainText . file

stdDelayTooltip :: T.Text -> WidgetNode s e -> WidgetNode s e
stdDelayTooltip = flip tooltip_ [tooltipDelay 750]

stdScroll :: WidgetNode s e -> WidgetNode s e
stdScroll = scroll_ [wheelRate 50]

labeledWidget ::
  (WidgetModel s, WidgetEvent e) => T.Text -> WidgetNode s e -> WidgetNode s e
labeledWidget l w =
  box_ [alignLeft] . vstack_ [] $
    [label l `styleBasic` [textSize 16], w]

{-
 ____  _   _ _____ _____ ___  _   _ ____
| __ )| | | |_   _|_   _/ _ \| \ | / ___|
|  _ \| | | | | |   | || | | |  \| \___ \
| |_) | |_| | | |   | || |_| | |\  |___) |
|____/ \___/  |_|   |_| \___/|_| \_|____/
-}

styledButton :: (WidgetModel s) => TaggerEvent -> T.Text -> WidgetNode s TaggerEvent
styledButton a t =
  button t a

buttonStylingBasic :: [StyleState]
buttonStylingBasic = [bgColor white, border 0 white]

buttonStylingHover :: [StyleState]
buttonStylingHover = [bgColor lightGray]

lazyBufferLoadButton :: WidgetNode TaggerModel TaggerEvent
lazyBufferLoadButton = styledButton (DoFileSelectionEvent LazyBufferLoad) "Load"

lazyBufferLoadAllButton :: WidgetNode TaggerModel TaggerEvent
lazyBufferLoadAllButton = styledButton (DoFileSelectionEvent LazyBufferLoadAll) "All"

lazyBufferFlushButton :: WidgetNode TaggerModel TaggerEvent
lazyBufferFlushButton = styledButton (DoFileSelectionEvent LazyBufferFlush) "Flush"

newFileTextCommitButton ::
  (WidgetModel s) => WidgetNode s TaggerEvent
newFileTextCommitButton = styledButton NewFileTextCommit "Add Path"

tagCommitButton ::
  (WidgetModel s) => WidgetNode s TaggerEvent
tagCommitButton = styledButton TagCommitTagsString "with"

descriptorNewCommitButton ::
  (WidgetModel s) => WidgetNode s TaggerEvent
descriptorNewCommitButton =
  styledButton DescriptorCommitNewDescriptorText "New Descriptor"

initializeDatabaseButton :: (WidgetModel s) => WidgetNode s TaggerEvent
initializeDatabaseButton =
  styledButton DatabaseInitialize "Initialize Database"
    `styleBasic` [bgColor (Color 212 0 0 0.83)]

databaseConnectButton :: (WidgetModel s) => WidgetNode s TaggerEvent
databaseConnectButton = styledButton DatabaseConnect "Connect"

databaseBackupButton :: WidgetModel s => WidgetNode s TaggerEvent
databaseBackupButton = styledButton DatabaseBackup "Backup"

configurationExportButton :: (WidgetModel s) => WidgetNode s TaggerEvent
configurationExportButton =
  styledButton (DoConfigurationEvent ExportAll) "Export Configuration"

toggleConfigConfigureVisibility :: (WidgetModel s) => WidgetNode s TaggerEvent
toggleConfigConfigureVisibility = styledButton (ToggleVisibilityMode Config) "Config"

toggleDatabaseConfigureVisibility :: (WidgetModel s) => WidgetNode s TaggerEvent
toggleDatabaseConfigureVisibility =
  styledButton (ToggleVisibilityMode Database) "Database"

toggleSelectionConfigureVisibility :: (WidgetModel s) => WidgetNode s TaggerEvent
toggleSelectionConfigureVisibility =
  styledButton
    (ToggleVisibilityMode Selection)
    "Selection"

toggleDescriptorConfigureVisibility :: WidgetNode TaggerModel TaggerEvent
toggleDescriptorConfigureVisibility =
  styledButton
    (ToggleVisibilityMode ProgramVisibilityDescriptor)
    "Descriptor"

descriptorDeleteWidget :: WidgetModel s => WidgetNode s TaggerEvent
descriptorDeleteWidget =
  box_ []
    . dropTarget DescriptorDelete
    . styledButton (IOEvent ())
    $ "X"

commitQueryButton ::
  (WidgetModel s) => WidgetNode s TaggerEvent
commitQueryButton =
  styledButton
    (DoFileSelectionEvent FileSelectionCommitQueryText)
    "with"

doShellCmdButton ::
  (WidgetModel s) => WidgetNode s TaggerEvent
doShellCmdButton = styledButton ShellCmd "Cmd"

resetDescriptorTreeToButton ::
  (WidgetModel s) =>
  T.Text ->
  WidgetNode s TaggerEvent
resetDescriptorTreeToButton t =
  styledButton
    ( DoDescriptorEvent . RequestDescriptorTree mainDescriptorTree $ t
    -- RequestDescriptorTree t
    )
    "↺"

resetUnrelatedDescriptorTree :: WidgetNode TaggerModel TaggerEvent
resetUnrelatedDescriptorTree =
  styledButton
    (DoDescriptorEvent (RefreshDescriptorTree unrelatedDescriptorTree))
    -- RefreshUnrelatedDescriptorTree
    "↺"

parentDescriptorTreeButton ::
  (WidgetModel s) => WidgetNode s TaggerEvent
parentDescriptorTreeButton =
  styledButton
    (DoDescriptorEvent (DescriptorTreePutParent mainDescriptorTree))
    -- DescriptorTreePutParent
    "↑"

selectButton ::
  (WidgetModel s) =>
  FileWithTags ->
  WidgetNode s TaggerEvent
selectButton =
  flip styledButton "Select"
    . DoFileSelectionEvent
    . FileSelectionUpdate
    . (: [])

-- previewButton :: -- #FIXME
--   (WidgetModel s) =>
--   FileWithTags ->
--   WidgetNode s TaggerEvent
-- previewButton = flip styledButton "Preview" . DoSingleFileEvent . SingleFilePut

-- fileSingleNextFromFileSelectionButton ::
--   (WidgetModel s) => WidgetNode s TaggerEvent
-- fileSingleNextFromFileSelectionButton =
--   styledButton (DoSingleFileEvent SingleFileNextFromFileSelection) "Next"

-- fileSinglePrevFromFileSelectionButton ::
--   (WidgetModel s) => WidgetNode s TaggerEvent
-- fileSinglePrevFromFileSelectionButton =
--   styledButton (DoSingleFileEvent SingleFilePrevFromFileSelection) "Prev"

clearSelectionButton ::
  (WidgetModel s) =>
  WidgetNode s TaggerEvent
clearSelectionButton =
  styledButton
    (DoFileSelectionEvent FileSelectionClear)
    "Clear"

fileSelectionShuffleButton :: WidgetNode TaggerModel TaggerEvent
fileSelectionShuffleButton =
  styledButton
    (DoFileSelectionEvent FileSelectionShuffle)
    "Shuffle"

appendToQueryButton :: WidgetModel s => T.Text -> WidgetNode s TaggerEvent
appendToQueryButton t =
  styledButton
    ( DropTargetAppendText_ (fileSelectionModel . queryText) id $
        t
    )
    "Add"

treeLeafButtonRequestDescriptorTree ::
  WidgetModel s =>
  Descriptor ->
  WidgetNode s TaggerEvent
treeLeafButtonRequestDescriptorTree d =
  styledButton
    ( (DoDescriptorEvent . RequestDescriptorTree mainDescriptorTree)
        . descriptor
        $ d
    )
    (descriptor d)

flipInSelectionOrderingButton :: OrdDirection -> TaggerWidget
flipInSelectionOrderingButton =
  styledButton
    (DoFileSelectionEvent FlipInSelectionOrdering)
    . T.pack
    . show

cycleInSelectionOrderingByButton :: OrderingBy -> TaggerWidget
cycleInSelectionOrderingByButton o =
  styledButton
    (DoFileSelectionEvent CycleInSelectionOrderingBy)
    (case o of Alphabetical -> "ABC"; _ -> "123")

{-
 _____ _______  _______ _____ ___ _____ _     ____  ____
|_   _| ____\ \/ /_   _|  ___|_ _| ____| |   |  _ \/ ___|
  | | |  _|  \  /  | | | |_   | ||  _| | |   | | | \___ \
  | | | |___ /  \  | | |  _|  | || |___| |___| |_| |___) |
  |_| |_____/_/\_\ |_| |_|   |___|_____|_____|____/|____/
-}

descriptorTreeConfigureMainRequestTextField :: WidgetNode TaggerModel TaggerEvent
descriptorTreeConfigureMainRequestTextField =
  textField (programConfig . descriptorTreeConf . descriptorTreeMainRequest)

dbPathTextField ::
  (WidgetModel s, HasProgramConfig s TaggerConfig) =>
  WidgetNode s TaggerEvent
dbPathTextField = textField (programConfig . dbconf . dbconfPath)

-- dbBackupTextField :: -- #FIXME Related to the new backup scheme.
--   (WidgetModel s, HasProgramConfig s TaggerConfig) => WidgetNode s TaggerEvent
-- dbBackupTextField = textField (programConfig . dbconf . dbconfBackup)

dbAutoConnectCheckBox ::
  (WidgetModel s, HasProgramConfig s TaggerConfig) =>
  WidgetNode s TaggerEvent
dbAutoConnectCheckBox =
  labeledCheckbox
    "Auto-Connect"
    (programConfig . dbconf . dbconfAutoConnect)

queryTextField ::
  WidgetNode TaggerModel TaggerEvent
queryTextField =
  dropTarget (DropTargetAppendText_ (fileSelectionModel . queryText) descriptor) $
    textField_ (fileSelectionModel . queryText) []

descriptorNewTextField ::
  (WidgetModel s, HasNewDescriptorText s T.Text) => WidgetNode s TaggerEvent
descriptorNewTextField =
  textField_ newDescriptorText []

newFileTextField ::
  (WidgetModel s, HasNewFileText s T.Text) => WidgetNode s TaggerEvent
newFileTextField = textField newFileText

selectionDisplayParentsNumberField ::
  WidgetNode TaggerModel TaggerEvent
selectionDisplayParentsNumberField =
  labeledWidget "Display Parent Folders"
    . flip styleBasic [textLeft]
    . numericField
    $ (programConfig . selectionconf . selectionDisplayParents)

selectionDisplayBufferSizeNumberField ::
  WidgetNode TaggerModel TaggerEvent
selectionDisplayBufferSizeNumberField =
  labeledWidget "Selection Buffer Size"
    . flip styleBasic [textLeft]
    . numericField
    $ (programConfig . selectionconf . selectionBufferSize)

tagsStringTextField ::
  WidgetNode TaggerModel TaggerEvent
tagsStringTextField =
  dropTarget (DropTargetAppendText_ tagsString descriptor) $ textField_ tagsString []

shellCmdTextField ::
  TaggerWidget
shellCmdTextField = textField_ (programConfig . shellCmd) []

renameDescriptorWidget :: WidgetNode TaggerModel TaggerEvent
renameDescriptorWidget =
  box_ []
    . labeledWidget "Rename Descriptor"
    . keystroke_
      [("Enter", DoDescriptorEvent RenameDescriptor)]
      []
    . hstack_ []
    $ [ dropTarget
          ( DropTargetAppendText_
              (descriptorModel . renameDescriptorFrom)
              descriptor
          )
          . textField
          $ (descriptorModel . renameDescriptorFrom),
        styledButton (DoDescriptorEvent RenameDescriptor) "To",
        dropTarget
          ( DropTargetAppendText_
              (descriptorModel . renameDescriptorTo)
              descriptor
          )
          . textField
          $ (descriptorModel . renameDescriptorTo)
      ]

setQueryCriteriaDropdown ::
  WidgetNode TaggerModel TaggerEvent
setQueryCriteriaDropdown =
  dropdown
    (fileSelectionModel . queryCriteria)
    [ByTag, ByRelation, ByPattern, ByUntagged]
    (label . T.pack . show)
    (label . T.pack . show)

setArithmeticDropdown ::
  WidgetNode TaggerModel TaggerEvent
setArithmeticDropdown =
  dropdown
    (fileSelectionModel . setArithmetic)
    [Union, Intersect, Diff]
    (label . T.pack . show)
    (label . T.pack . show)

taggingModeDropdown ::
  (WidgetModel s, WidgetEvent e, HasTaggingMode s TaggingMode) =>
  WidgetNode s e
taggingModeDropdown =
  dropdown
    taggingMode
    [TagMode, UntagMode]
    (label . T.pack . show)
    (label . T.pack . show)

shellCmdWidget :: TaggerWidget
shellCmdWidget =
  keystroke [("Enter", ShellCmd)]
    . hstack
    $ [shellCmdTextField, doShellCmdButton]

draggableDescriptorListWidget ::
  (WidgetModel s, WidgetEvent e) => [Descriptor] -> WidgetNode s e
draggableDescriptorListWidget =
  box_ [alignLeft]
    . hstack
    . L.intersperse spacer
    . L.foldl' (\ws d -> ws ++ [draggableDescriptorWidget d]) []

draggableDescriptorWidget ::
  (WidgetModel s, WidgetEvent e) => Descriptor -> WidgetNode s e
draggableDescriptorWidget d =
  box_ [alignLeft]
    . draggable d
    . flip styleBasic [textColor blue]
    . flip label_ [ellipsis]
    . getPlainText
    $ d

mainDescriptorTreeWidget ::
  DescriptorTreeConfig ->
  DescriptorTree ->
  WidgetNode TaggerModel TaggerEvent
mainDescriptorTreeWidget dtrConf tr =
  dropTarget
    ( \d' ->
        maybe
          (IOEvent ())
          (\m' -> DescriptorCreateRelation [m'] [d'])
          (getNode tr)
    )
    $ generalDescriptorTreeWidget
      tr
      [ resetDescriptorTreeToButton (dtrConf ^. descriptorTreeMainRequest),
        parentDescriptorTreeButton,
        descriptorDeleteWidget
      ]
      treeLeafButtonRequestDescriptorTree
      dtrConf

unrelatedDescriptorTreeWidget ::
  DescriptorTreeConfig ->
  DescriptorTree ->
  WidgetNode TaggerModel TaggerEvent
unrelatedDescriptorTreeWidget dtrConf tr =
  dropTarget (\d' -> DescriptorUnrelate [d']) $
    generalDescriptorTreeWidget
      tr
      [resetUnrelatedDescriptorTree]
      (label . getPlainText)
      dtrConf

generalDescriptorTreeWidget ::
  DescriptorTree ->
  [WidgetNode TaggerModel TaggerEvent] ->
  (Descriptor -> WidgetNode TaggerModel TaggerEvent) ->
  DescriptorTreeConfig ->
  WidgetNode TaggerModel TaggerEvent
generalDescriptorTreeWidget tr bs dAction _ =
  flip styleBasic [border 1 black] . box_ [alignTop, alignLeft] $
    hstack_
      []
      [ vsplit (vstack_ [] bs, spacer),
        separatorLine,
        descriptorTreeWidget (sortChildren tr) dAction
      ]
  where
    descriptorTreeWidget ::
      DescriptorTree ->
      (Descriptor -> WidgetNode TaggerModel TaggerEvent) ->
      WidgetNode TaggerModel TaggerEvent
    descriptorTreeWidget descriptorTreeWidgetTr descriptorTreeWidgetDAction =
      box . stdScroll . flip styleBasic [textFont "Regular"]
        . buildTreeWidget descriptorTreeWidgetDAction
        $ descriptorTreeWidgetTr
      where
        buildTreeWidget ::
          (Descriptor -> WidgetNode TaggerModel TaggerEvent) ->
          DescriptorTree ->
          WidgetNode TaggerModel TaggerEvent
        buildTreeWidget action = buildTreeWidgetAccum 0 (vstack []) action
          where
            buildTreeWidgetAccum ::
              Int ->
              WidgetNode TaggerModel TaggerEvent ->
              (Descriptor -> WidgetNode TaggerModel TaggerEvent) ->
              DescriptorTree ->
              WidgetNode TaggerModel TaggerEvent
            buildTreeWidgetAccum l acc buildTreeWidgetAccumAction buildTreeWidgetAccumTr =
              case buildTreeWidgetAccumTr of
                NullTree -> acc
                Infra d ->
                  vstack
                    [ acc,
                      treeLeafDescriptorWidget
                        black
                        l
                        d
                        buildTreeWidgetAccumAction
                    ]
                Meta d cs ->
                  appendVStack
                    ( vstack
                        [ acc,
                          hstack
                            [ treeLeafDescriptorWidget
                                yuiBlue
                                l
                                d
                                buildTreeWidgetAccumAction
                            ]
                        ]
                    )
                    ( vstack $
                        map
                          ( \c ->
                              case c of
                                Infra d' ->
                                  treeLeafDescriptorWidget
                                    black
                                    (l + 1)
                                    d'
                                    buildTreeWidgetAccumAction
                                Meta d' _ ->
                                  treeLeafDescriptorWidget
                                    yuiBlue
                                    (l + 1)
                                    d'
                                    buildTreeWidgetAccumAction
                                NullTree ->
                                  spacer
                                    `styleBasic` [padding 0, border 0 white]
                          )
                          cs
                    )
        appendVStack x y = vstack [x, y]
        treeLeafDescriptorWidget ::
          Color ->
          Int ->
          Descriptor ->
          (Descriptor -> WidgetNode TaggerModel TaggerEvent) ->
          WidgetNode TaggerModel TaggerEvent
        treeLeafDescriptorWidget tc l d a =
          hstack_ [] $
            [ label (T.replicate l "--" !++ "|"),
              dropTarget_
                (DoDescriptorEvent . flip RepresentativeCreate d . file)
                [ dropTargetStyle
                    [border 1 black]
                ]
                . draggable d
                . flip
                  styleBasic
                  [ textColor tc,
                    bgColor white,
                    border 0 white,
                    padding 0
                  ]
                . flip
                  styleHover
                  [bgColor lightGray]
                . a
                $ d
            ]

-- imageDetailWidget :: -- #FIXME this one definition is TOO LONG!
--   TaggerModel -> TaggerWidget
-- imageDetailWidget m =
--   flip styleBasic [borderL 1 black, rangeWidth 160 800]
--     . box_ [alignLeft]
--     . vstack_ []
--     $ [ label "Details:",
--         spacer,
--         label "Solo Tagging Mode"
--           `styleBasic` [textColor yuiOrange]
--           `nodeVisible` (m ^. doSoloTag),
--         spacer
--         ,
--         vsplit_
--           [splitIgnoreChildResize True]
--           (detailTagsWidget, inSelectionWidget)
--       ]
--   where
--     -- detailTagsWidget :: TaggerWidget
--     -- detailTagsWidget =
--     --   let !tags' = maybe HashSet.empty tags $ m ^. singleFileModel . singleFile
--     --       !tagMapTuple = tagSetToTagMapTuple tags'
--     --    in vstack_
--     --         []
--     --         [ label "Tags:",
--     --           hstack_
--     --             []
--     --             [ spacer,
--     --               flip styleBasic [border 1 black]
--     --                 . vscroll_ [wheelRate 50]
--     --                 . vstack_
--     --                   []
--     --                 $ ( map
--     --                       ( \t' ->
--     --                           showTagAndSubTags
--     --                             t'
--     --                             (snd tagMapTuple)
--     --                             (m ^. singleFileModel . tagCounts)
--     --                       )
--     --                       . filter (not . isSubTag)
--     --                       . IntMap.elems
--     --                       . fst
--     --                       $ tagMapTuple
--     --                   )
--     --                   ++ [singleFileDropTargets]
--     --             ]
--     --         ]
--     --   where
--     --     singleFileDropTargets :: TaggerWidget
--     --     singleFileDropTargets =
--     --       hgrid_
--     --         []
--     --         [ dropTarget_
--     --             (DoSingleFileEvent . SingleFileUntag)
--     --             [dropTargetStyle [bgColor yuiYellow]]
--     --             . flip styleBasic [border 1 black]
--     --             . box_ [alignMiddle, alignBottom]
--     --             . flip styleBasic [minHeight 80, maxHeight 81, maxWidth 80]
--     --             . label
--     --             $ "Untag"
--     --         ]
--         -- showTagAndSubTags :: Tag -> SubTagMap -> OccurrenceMap Descriptor -> TaggerWidget
--         -- showTagAndSubTags t stm om =
--         --   hgrid_
--         --     []
--         --     [ box_ [alignLeft] $
--         --         maybe
--         --           ( hstack_
--         --               []
--         --               [ dropTarget_
--         --                   (DoSingleFileEvent . SingleFileAssociateTag t)
--         --                   [dropTargetStyle [border 1 black]]
--         --                   . draggable t
--         --                   . flip styleBasic [textColor yuiBlue]
--         --                   . label
--         --                   . descriptor
--         --                   . tagDescriptor
--         --                   $ t,
--         --                 spacer
--         --               ]
--         --           )
--         --           ( \ts ->
--         --               hstack_ [] $
--         --                 ( dropTarget_
--         --                     (DoSingleFileEvent . SingleFileAssociateTag t)
--         --                     [dropTargetStyle [border 1 black]]
--         --                     . draggable t
--         --                     . flip styleBasic [textColor yuiBlue]
--         --                     . label
--         --                     . descriptor
--         --                     . tagDescriptor
--         --                     $ t
--         --                 ) :
--         --                 ( label " { " :
--         --                   ( L.intersperse (label ", ")
--         --                       . map
--         --                         ( \t' ->
--         --                             draggable t'
--         --                               . flip styleBasic [padding 0]
--         --                               . box_ [alignLeft]
--         --                               . label
--         --                               . descriptor
--         --                               . tagDescriptor
--         --                               $ t'
--         --                         )
--         --                       $ ts
--         --                   )
--         --                     ++ [ label " } ",
--         --                          box_ []
--         --                            . label
--         --                            . T.pack
--         --                            . show
--         --                            . M.fromMaybe 0
--         --                            $ IntMap.lookup (descriptorId . tagDescriptor $ t) om
--         --                        ]
--         --                 )
--         --           )
--         --           (IntMap.lookup (tagId t) stm)
--         --     ]
--     inSelectionWidget :: TaggerWidget
--     inSelectionWidget =
--       vstack_
--         []
--         [ hstack_
--             []
--             [ label $
--                 "In Selection: "
--                   !++ "("
--                   !++ (T.pack . show . length . totalBufferList)
--                     (m ^. fileSelectionModel . fileSelection)
--                   !++ ")",
--               spacer,
--               cycleInSelectionOrderingByButton
--                 . (\(OrderingMode b _) -> b)
--                 $ m ^. fileSelectionModel . selectionDetailsOrdering,
--               flipInSelectionOrderingButton
--                 . (\(OrderingMode _ d) -> d)
--                 $ m ^. fileSelectionModel . selectionDetailsOrdering
--             ]
--             --   ,
--             -- spacer,
--             -- hstack_
--             --   []
--             --   [ spacer,
--             --     flip styleBasic [border 1 black]
--             --       . vscroll_ [wheelRate 50]
--             --       . vstack_ []
--             --       . map imageDetailDescriptor
--             --       . sortDescriptorIntTuple
--             --         (m ^. fileSelectionModel . selectionDetailsOrdering)
--             --       -- this is total jank but it works now
--             --       $ ( let selD =
--             --                 (\xs -> if null xs then [] else L.foldl1' union xs)
--             --                   . map (HashSet.toList . HashSet.map tagDescriptor . tags) -- #FIXME
--             --                   . cCollect
--             --                   $ m ^. fileSelectionModel . fileSelection
--             --               om =
--             --                 foldOccurrences
--             --                   . map tagDescriptor
--             --                   . concatMap (HashSet.toList . tags) -- #FIXME
--             --                   . cCollect
--             --                   $ m ^. fileSelectionModel . fileSelection
--             --            in decodeOccurrencesWith selD om
--             --         )
--             --   ]
--         ]
--       where
--         sortDescriptorIntTuple ::
--           OrderingMode -> [(Descriptor, Int)] -> [(Descriptor, Int)]
--         sortDescriptorIntTuple (OrderingMode b d) xs =
--           case b of
--             Alphabetical -> if d == Asc then L.sortOn fst xs else L.sortOn (Down . fst) xs
--             Numerical -> if d == Asc then L.sortOn snd xs else L.sortOn (Down . snd) xs
--     imageDetailDescriptor ::
--       (WidgetModel s) =>
--       (Descriptor, Int) ->
--       WidgetNode s TaggerEvent
--     imageDetailDescriptor (d, c) =
--       hgrid_ [] $
--         [ draggable_ d []
--             . box_ [alignLeft]
--             . flip styleBasic [textColor yuiBlue]
--             . label
--             . getPlainText
--             $ d,
--           flip styleBasic [paddingL 15]
--             . box_ [alignLeft]
--             . flip styleBasic [textColor yuiBlue]
--             . label
--             . T.pack
--             . show
--             $ c
--         ]

representativeFilePreview :: Maybe Representative -> TaggerWidget
representativeFilePreview mr =
  labeledWidget
    ( maybe
        "No Representative"
        ( (!++) "Representative File for: "
            . descriptor
            . repDescriptorId
        )
        mr
    )
    . flip styleBasic [border 1 black]
    . dropTarget_
      (DoDescriptorEvent . RepresentativeFileLookup)
      [dropTargetStyle [border 2 yuiYellow]]
    . box_ []
    . maybe
      (label "")
      (flip image_ [fitHeight, alignCenter] . filePath . repFileId)
    $ mr

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant flip" #-}
{-# HLINT ignore "Use ||" #-}

module Interface.Widget.Internal (
  TaggerWidget,
  hidePossibleUIVis,
  -- fileSelectionOperationWidget,
  tagTextNodeKey,
  zstackTaggingWidgetVis,
  zstackQueryWidgetVis,
  focusedFileWidget,
  taggerInfoWidget,
) where

import Control.Lens hiding (both)
import Data.Event
import Data.Model
import Data.Model.Shared
import Data.Text (Text)
import qualified Data.Text as T
import Database.Tagger.Type
import Interface.Theme
import Interface.Widget.Internal.Core
import Interface.Widget.Internal.Type (TaggerWidget)
import Monomer
import Monomer.Graphics.Lens

hidePossibleUIVis :: Text
hidePossibleUIVis = "hide-possible-elements"

{-
 _____ ___ _     _____
|  ___|_ _| |   | ____|
| |_   | || |   |  _|
|  _|  | || |___| |___
|_|   |___|_____|_____|

 ____  _____ _     _____ ____ _____ ___ ___  _   _
/ ___|| ____| |   | ____/ ___|_   _|_ _/ _ \| \ | |
\___ \|  _| | |   |  _|| |     | |  | | | | |  \| |
 ___) | |___| |___| |__| |___  | |  | | |_| | |\  |
|____/|_____|_____|_____\____| |_| |___\___/|_| \_|

__        _____ ____   ____ _____ _____
\ \      / /_ _|  _ \ / ___| ____|_   _|
 \ \ /\ / / | || | | | |  _|  _|   | |
  \ V  V /  | || |_| | |_| | |___  | |
   \_/\_/  |___|____/ \____|_____| |_|

-}

{-
 _____ ___   ____ _   _ ____  _____ ____
|  ___/ _ \ / ___| | | / ___|| ____|  _ \
| |_ | | | | |   | | | \___ \|  _| | | | |
|  _|| |_| | |___| |_| |___) | |___| |_| |
|_|   \___/ \____|\___/|____/|_____|____/

 _____ ___ _     _____
|  ___|_ _| |   | ____|
| |_   | || |   |  _|
|  _|  | || |___| |___
|_|   |___|_____|_____|

__        _____ ____   ____ _____ _____
\ \      / /_ _|  _ \ / ___| ____|_   _|
 \ \ /\ / / | || | | | |  _|  _|   | |
  \ V  V /  | || |_| | |_| | |___  | |
   \_/\_/  |___|____/ \____|_____| |_|

-}

mainPaneFloatingOpacity :: Double
mainPaneFloatingOpacity = 0.5

tagTextNodeKey :: Text
tagTextNodeKey = "tag-text-field"

zstackTaggingWidgetVis :: Text
zstackTaggingWidgetVis = "show-tag-field"

zstackQueryWidgetVis :: Text
zstackQueryWidgetVis = "show-query-field"

focusedFileWidget :: TaggerModel -> TaggerWidget
focusedFileWidget m =
  box_ []
    . withStyleBasic [minHeight 300]
    $ hsplit_
      [splitIgnoreChildResize True, splitHandleSize 10]
      ( withStyleBasic [borderR 1 black] focusedFileMainPane
      , withNodeVisible
          (not $ (m ^. visibilityModel) `hasVis` VisibilityLabel hidePossibleUIVis)
          $ undefined
      )
 where
  focusedFileMainPane =
    zstack_
      [onlyTopActive_ False]
      [ dropTarget_
          (DoFocusedFileEvent . PutFile)
          [dropTargetStyle [border 3 yuiOrange]]
          . dropTarget_
            (\(Descriptor dk _) -> DoFocusedFileEvent (TagFile dk Nothing))
            [dropTargetStyle [border 3 yuiBlue]]
          . dropTarget_
            (DoFocusedFileEvent . UnSubTag . concreteTagId)
            [dropTargetStyle [border 1 yuiRed]]
          . withStyleBasic []
          . box_
            [ mergeRequired
                ( \_ m1 m2 ->
                    concreteTaggedFile (m1 ^. focusedFileModel . focusedFile)
                      /= concreteTaggedFile (m2 ^. focusedFileModel . focusedFile)
                )
            ]
          $ ( case m ^. focusedFileModel . renderability of
                RenderAsImage -> imagePreviewRender
                _ -> imagePreviewRender
            )
            (filePath . concreteTaggedFile $ (m ^. focusedFileModel . focusedFile))
      , withNodeVisible
          ( not $
              (m ^. visibilityModel) `hasVis` VisibilityLabel hidePossibleUIVis
          )
          . box_
            [alignBottom, alignLeft, ignoreEmptyArea]
          $ vstack
            [ hstack [zstackNextImage, zstackTaggingWidget]
            , hstack [zstackPrevImage, zstackQueryWidget]
            ]
      ]
   where
    zstackNextImage =
      withStyleBasic [bgColor $ yuiLightPeach & a .~ mainPaneFloatingOpacity] $
        styledButton_ [resizeFactor (-1)] "↑" (DoFileSelectionEvent CycleNextFile)
    zstackPrevImage =
      withStyleBasic [bgColor $ yuiLightPeach & a .~ mainPaneFloatingOpacity] $
        styledButton_ [resizeFactor (-1)] "↓" (DoFileSelectionEvent CyclePrevFile)
    zstackQueryWidget :: TaggerWidget
    zstackQueryWidget =
      box_ [alignLeft, ignoreEmptyArea]
        . withStyleBasic [maxWidth 450]
        $ hstack_
          []
          [ vstack . (: [])
              . withStyleBasic
                [ bgColor $
                    yuiLightPeach
                      & a .~ mainPaneFloatingOpacity
                ]
              $ styledButton_
                [resizeFactor (-1)]
                "Query"
                ( DoFocusedFileEvent
                    (ToggleFocusedFilePaneVisibility zstackQueryWidgetVis)
                )
                -- , withNodeVisible isVisible queryTextField
          ]
     where
    -- isVisible =
    --   (m ^. focusedFileModel . focusedFileVis)
    --     `hasVis` VisibilityLabel zstackQueryWidgetVis
    zstackTaggingWidget :: TaggerWidget
    zstackTaggingWidget =
      box_ [alignLeft, ignoreEmptyArea]
        . withStyleBasic [maxWidth 400]
        $ hstack
          [ vstack . (: [])
              . withStyleBasic
                [ bgColor $
                    yuiLightPeach
                      & a .~ mainPaneFloatingOpacity
                ]
              $ styledButton_
                [resizeFactor (-1)]
                "Tag"
                ( DoFocusedFileEvent
                    (ToggleFocusedFilePaneVisibility zstackTaggingWidgetVis)
                )
          , withNodeVisible
              isVisible
              tagTextField
          ]
     where
      isVisible =
        (m ^. focusedFileModel . focusedFileVis)
          `hasVis` VisibilityLabel zstackTaggingWidgetVis

imagePreviewRender :: Text -> TaggerWidget
imagePreviewRender fp = image_ fp [fitEither, alignCenter]

tagTextField :: TaggerWidget
tagTextField =
  keystroke_
    [ ("Enter", DoFocusedFileEvent CommitTagText)
    , ("Up", DoFocusedFileEvent NextTagHist)
    , ("Down", DoFocusedFileEvent PrevTagHist)
    ]
    []
    . dropTarget_
      (DoFocusedFileEvent . AppendTagText . descriptor . concreteTagDescriptor)
      [dropTargetStyle [border 1 yuiRed]]
    . dropTarget_
      (DoFocusedFileEvent . AppendTagText . descriptor)
      [dropTargetStyle [border 1 yuiBlue]]
    . withNodeKey tagTextNodeKey
    . withStyleBasic [bgColor (yuiLightPeach & a .~ mainPaneFloatingOpacity)]
    $ textField_
      (focusedFileModel . tagText)
      [ onChange
          ( \t ->
              if T.null t
                then DoFocusedFileEvent ResetTagHistIndex
                else
                  IOEvent
                    ()
          )
      ]

{-
 ____  _____ ____   ____ ____  ___ ____ _____ ___  ____
|  _ \| ____/ ___| / ___|  _ \|_ _|  _ \_   _/ _ \|  _ \
| | | |  _| \___ \| |   | |_) || || |_) || || | | | |_) |
| |_| | |___ ___) | |___|  _ < | ||  __/ | || |_| |  _ <
|____/|_____|____/ \____|_| \_\___|_|    |_| \___/|_| \_\

 _____ ____  _____ _____
|_   _|  _ \| ____| ____|
  | | | |_) |  _| |  _|
  | | |  _ <| |___| |___
  |_| |_| \_\_____|_____|

__        _____ ____   ____ _____ _____
\ \      / /_ _|  _ \ / ___| ____|_   _|
 \ \ /\ / / | || | | | |  _|  _|   | |
  \ V  V /  | || |_| | |_| | |___  | |
   \_/\_/  |___|____/ \____|_____| |_|
-}

{-
 _____  _    ____  ____ _____ ____
|_   _|/ \  / ___|/ ___| ____|  _ \
  | | / _ \| |  _| |  _|  _| | |_) |
  | |/ ___ \ |_| | |_| | |___|  _ <
  |_/_/   \_\____|\____|_____|_| \_\

 ___ _   _ _____ ___
|_ _| \ | |  ___/ _ \
 | ||  \| | |_ | | | |
 | || |\  |  _|| |_| |
|___|_| \_|_|   \___/

__        _____ ____   ____ _____ _____
\ \      / /_ _|  _ \ / ___| ____|_   _|
 \ \ /\ / / | || | | | |  _|  _|   | |
  \ V  V /  | || |_| | |_| | |___  | |
   \_/\_/  |___|____/ \____|_____| |_|
-}

taggerInfoWidget :: TaggerModel -> TaggerWidget
taggerInfoWidget m@((^. taggerInfoModel) -> tim) =
  withNodeVisible
    ( not $
        (m ^. visibilityModel)
          `hasVis` VisibilityLabel hidePossibleUIVis
    )
    . box_ [alignMiddle]
    $ vstack $
      withStyleBasic [paddingT 2.5, paddingB 2.5]
        <$> ( [ flip label_ [resizeFactor (-1)] $ tim ^. message
              , flip label_ [resizeFactor (-1)] $ tim ^. versionMessage
              ]
                ++ ( (\(h, t) -> label_ (h <> ": " <> (tim ^. t)) [resizeFactor (-1)])
                      <$> [ ("In Directory", workingDirectory)
                          , ("Version", version)
                          , ("Last Accessed", lastAccessed)
                          ]
                   )
            )
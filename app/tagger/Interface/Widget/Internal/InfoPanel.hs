{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

-- unused for now because I don't really care for it.
module Interface.Widget.Internal.InfoPanel (widget) where

import Control.Lens ((^.))
import Data.Model (
  HasLastAccessed (lastAccessed),
  HasMessage (message),
  HasTaggerInfoModel (taggerInfoModel),
  HasVersion (version),
  HasVersionMessage (versionMessage),
  HasWorkingDirectory (workingDirectory),
  TaggerModel,
 )
import Interface.Widget.Internal.Core (withStyleBasic)
import Interface.Widget.Internal.Type (TaggerWidget)
import Monomer (
  CmbAlignMiddle (alignMiddle),
  CmbPaddingB (paddingB),
  CmbPaddingT (paddingT),
  CmbResizeFactor (resizeFactor),
  box_,
  label_,
  vstack,
 )

widget :: TaggerModel -> TaggerWidget
widget m = taggerInfoWidget m

taggerInfoWidget :: TaggerModel -> TaggerWidget
taggerInfoWidget ((^. taggerInfoModel) -> tim) =
  box_ [alignMiddle] $
    vstack $
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
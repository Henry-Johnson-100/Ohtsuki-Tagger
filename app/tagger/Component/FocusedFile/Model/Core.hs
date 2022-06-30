module Component.FocusedFile.Model.Core (
  FocusedFileModel (..),
  Renderability (..),
  createFocusedFileModel,
  focusedFileDefaultDataFile,
) where

import qualified Data.HierarchyMap as HAM
import Data.OccurrenceMap (OccurrenceMap)
import qualified Data.OccurrenceMap as OM
import Data.Text (Text)
import Database.Tagger.Type (
  ConcreteTaggedFile (ConcreteTaggedFile),
  File (File),
 )

data FocusedFileModel = FocusedFileModel
  { _focusedfilemodelFocusedFile :: ConcreteTaggedFile
  , _focusedfilemodelRenderability :: Renderability
  , _focusedfilemodelTagOccurrences :: OccurrenceMap
  }
  deriving (Show, Eq)

createFocusedFileModel :: Text -> FocusedFileModel
createFocusedFileModel fp =
  FocusedFileModel
    { _focusedfilemodelFocusedFile = ConcreteTaggedFile (File (-1) fp) HAM.empty
    , _focusedfilemodelRenderability = RenderingNotSupported
    , _focusedfilemodelTagOccurrences = OM.empty
    }

focusedFileDefaultDataFile :: FilePath
focusedFileDefaultDataFile = "Yui_signature_SS.png"

data Renderability
  = RenderAsImage
  | RenderAsText
  | RenderingNotSupported
  deriving (Show, Eq, Enum)

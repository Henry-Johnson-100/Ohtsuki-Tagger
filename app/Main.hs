{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use ?~" #-}
{-# HLINT ignore "Use <&>" #-}

module Main where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Char
import Data.List
import Data.Maybe
import Data.Text hiding (head, map, take)
import Database.Tagger.Access
import Database.Tagger.Type
import Monomer
import Monomer.Common.Lens
import Node.Application
import Type.Model

type ConnString = String

queryByTag :: ConnString -> [String] -> IO [FileWithTags]
queryByTag cs ts =
  connectThenRun cs $ do
    tagQResult <- mapM lookupDescriptorAutoKey ts
    fileAKsWithTags <-
      fmap (Data.List.foldl1' union)
        . mapM fetchFilesWithTag
        . catMaybes
        $ tagQResult
    filesWithTags <- fmap catMaybes . mapM getFileWithTags $ fileAKsWithTags
    liftIO . return $ filesWithTags

queryUntagged :: ConnString -> [String] -> IO [FileWithTags]
queryUntagged cs ns =
  connectThenRun cs $ do
    untaggedKeys <-
      case ns of
        [] -> fetchUntaggedFiles
        (n : _) ->
          if Data.List.all isDigit n
            then fmap (take . read $ n) fetchUntaggedFiles
            else fetchUntaggedFiles
    filesWithTags <- fmap catMaybes . mapM getFileWithTags $ untaggedKeys
    liftIO . return $ filesWithTags

-- | Kinda hacky
queryRelation :: ConnString -> [String] -> IO [FileWithTags]
queryRelation cs ds =
  connectThenRun cs $ do
    tagQResult <- fmap catMaybes . mapM lookupDescriptorAutoKey $ ds
    infraTrees <-
      fmap (Data.List.concatMap flattenTree) . mapM fetchInfraTree $ tagQResult
    fwts <- liftIO . queryByTag cs . map descriptor $ infraTrees
    liftIO . return $ fwts

doQueryWithCriteria :: QueryCriteria -> ConnString -> [String] -> IO [FileWithTags]
doQueryWithCriteria qc =
  case qc of
    ByTag -> queryByTag
    ByRelation -> queryRelation
    ByUntagged -> queryUntagged

getAllFilesIO :: ConnString -> IO [FileWithTags]
getAllFilesIO =
  flip
    connectThenRun
    (fetchAllFiles >>= fmap catMaybes . mapM getFileWithTags >>= liftIO . return)

getAllIndexedDescriptorsIO :: ConnString -> IO [Descriptor]
getAllIndexedDescriptorsIO =
  flip
    connectThenRun
    ( fetchAllIndexedDescriptors
        >>= liftIO . return
    )

lookupDescriptorTree :: ConnString -> String -> IO DescriptorTree
lookupDescriptorTree cs lk = connectThenRun cs $ do
  mk <- lookupDescriptorAutoKey lk
  ktr <- maybe (return NullTree) (fetchInfraTree) mk
  liftIO . return $ ktr

doSetAction :: Eq a => FileSetArithmetic -> [a] -> [a] -> [a]
doSetAction a s o =
  case a of
    Union -> s `union` o
    Intersect -> s `intersect` o
    Diff -> s \\ o

taggerEventHandler ::
  WidgetEnv TaggerModel TaggerEvent ->
  WidgetNode TaggerModel TaggerEvent ->
  TaggerModel ->
  TaggerEvent ->
  [AppEventResponse TaggerModel TaggerEvent]
taggerEventHandler wenv node model event =
  case event of
    TaggerInit ->
      [ Task
          ( DescriptorTreePut
              <$> (lookupDescriptorTree (model ^. connectionString) "Character")
          ),
        Model $
          model
            & fileSingle
              .~ (Just (FileWithTags (File "/home/monax/Pictures/dog.jpg") []))
      ]
    FileSinglePut i -> [Model $ model & fileSingle .~ (Just i)]
    FileSetArithmetic a -> [Model $ model & fileSetArithmetic .~ a]
    FileSetQueryCriteria q -> [Model $ model & queryCriteria .~ q]
    FileSelectionUpdate ts ->
      [ Model $
          model & fileSelection
            .~ (doSetAction (model ^. fileSetArithmetic) (model ^. fileSelection) ts)
      ]
    FileSelectionStageQuery t -> [Model $ model & fileSelectionQuery .~ t]
    FileSelectionAppendQuery t ->
      [ Model $
          model & fileSelectionQuery
            .~ (Data.Text.unwords [model ^. fileSelectionQuery, t])
      ]
    FileSelectionCommitQuery ->
      [ Task
          ( FileSelectionUpdate
              <$> ( doQueryWithCriteria
                      (model ^. queryCriteria)
                      (model ^. connectionString)
                      (map unpack . Data.Text.words $ (model ^. fileSelectionQuery))
                  )
          )
      ]
    FileSelectionClear -> [Model $ model & fileSelection .~ []]
    DescriptorTreePut tr -> [Model $ model & descriptorTree .~ tr]
    ToggleDoSoloTag ->
      [Model $ model & (doSoloTag .~ (not (model ^. doSoloTag)))]
    RequestDescriptorTree s ->
      [ Task
          ( DescriptorTreePut
              <$> (lookupDescriptorTree (model ^. connectionString) (unpack s))
          )
      ]

taggerApplicationUI ::
  WidgetEnv TaggerModel TaggerEvent ->
  TaggerModel ->
  WidgetNode TaggerModel TaggerEvent
taggerApplicationUI wenv model = widgetTree
  where
    widgetTree =
      hgrid
        [ vgrid
            [ fileSelectionWidget (model ^. fileSelection),
              descriptorTreeWidget model
            ],
          vgrid
            [ configPanel,
              (fileSinglePreviewWidget model)
            ]
        ]

taggerApplicationConfig :: [AppConfig TaggerEvent]
taggerApplicationConfig =
  [ appInitEvent TaggerInit
  ]
    ++ themeConfig

runTaggerWindow :: IO ()
runTaggerWindow =
  startApp
    (emptyTaggerModel "/home/monax/Repo/Haskell/TaggerLib/images_test.db")
    taggerEventHandler
    taggerApplicationUI
    taggerApplicationConfig

main :: IO ()
main = runTaggerWindow

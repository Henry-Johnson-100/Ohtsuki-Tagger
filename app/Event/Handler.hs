{-# LANGUAGE BangPatterns #-}
{-# HLINT ignore "Use ?~" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Redundant <$>" #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Event.Handler
  ( taggerEventHandler,
  )
where

import Control.Lens ((&), (.~), (^.))
import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Text as T
import Database.Tagger.Type
import Event.Task
import Monomer
  ( AppEventResponse,
    EventResponse (Model, Task),
    WidgetEnv,
    WidgetNode,
  )
import System.Process (createProcess, shell)
import Type.Model

fwtUnion :: [FileWithTags] -> [FileWithTags] -> [FileWithTags]
fwtUnion = L.unionBy fwtFileEqual

fwtIntersect :: [FileWithTags] -> [FileWithTags] -> [FileWithTags]
fwtIntersect = L.intersectBy fwtFileEqual

fwtDiff :: [FileWithTags] -> [FileWithTags] -> [FileWithTags]
fwtDiff = L.deleteFirstsBy fwtFileEqual

doSetAction ::
  FileSetArithmetic ->
  [FileWithTags] ->
  [FileWithTags] ->
  [FileWithTags]
doSetAction a s o =
  case a of
    Union -> s `fwtUnion` o
    Intersect -> s `fwtIntersect` o
    Diff -> s `fwtDiff` o

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
              <$> getALLInfraTree (model ^. dbConn)
          ),
        Task (UnrelatedDescriptorTreePut <$> getUnrelatedInfraTree (model ^. dbConn)),
        Model $
          model
            & fileSingle
              .~ Just
                ( FileWithTags
                    (File (-1) "/home/monax/Pictures/dog.jpg")
                    []
                )
      ]
    FileSinglePut i -> [Model $ model & fileSingle .~ Just i]
    FileSingleMaybePut mi -> [Model $ model & fileSingle .~ mi]
    FileSetArithmetic a -> [Model $ model & fileSetArithmetic .~ a]
    FileSetArithmeticNext ->
      [ Model $
          model
            & fileSetArithmetic .~ next (model ^. fileSetArithmetic)
      ]
    FileSetQueryCriteria q -> [Model $ model & queryCriteria .~ q]
    FileSetQueryCriteriaNext ->
      [ Model $
          model
            & queryCriteria .~ next (model ^. queryCriteria)
      ]
    FileSelectionUpdate ts ->
      [ Model $
          model & fileSelection
            .~ doSetAction (model ^. fileSetArithmetic) (model ^. fileSelection) ts
      ]
    FileSelectionPut fwts ->
      [ Model $ model & fileSelection .~ fwts
      ]
    FileSelectionRefresh_ ->
      [ Task
          ( FileSelectionPut
              <$> getRefreshedFWTs (model ^. dbConn) (model ^. fileSelection)
          ),
        Task
          ( FileSingleMaybePut <$> do
              mrefreshed <-
                M.maybe
                  (return [])
                  (getRefreshedFWTs (model ^. dbConn) . (: []))
                  (model ^. fileSingle)
              return . head' $ mrefreshed
          )
      ]
    FileSelectionAppendQuery t ->
      [ Model $
          model & fileSelectionQuery
            .~ T.unwords [model ^. fileSelectionQuery, t]
      ]
    TagsStringAppend t ->
      [ Model $
          model
            & tagsString .~ T.unwords [model ^. tagsString, t]
      ]
    FileSelectionCommitQuery ->
      [ Task
          ( FileSelectionUpdate
              <$> doQueryWithCriteria
                (model ^. queryCriteria)
                (model ^. dbConn)
                (T.words (model ^. fileSelectionQuery))
          ),
        asyncEvent FileSelectionQueryClear
      ]
    FileSelectionClear ->
      [ Model $ model & fileSelection .~ [],
        asyncEvent FileSelectionQueryClear
      ]
    FileSelectionQueryClear -> [Model $ model & fileSelectionQuery .~ ""]
    FileSingleNextFromFileSelection ->
      let !ps = popCycleList (model ^. fileSelection)
          !mi = head' ps
       in [ Model . (fileSelection .~ ps) . (fileSingle .~ mi) $ model
          ]
    FileSinglePrevFromFileSelection ->
      let !ps = dequeueCycleList (model ^. fileSelection)
          !mi = head' ps
       in [Model . (fileSelection .~ ps) . (fileSingle .~ mi) $ model]
    DescriptorTreePut tr -> [Model $ model & descriptorTree .~ tr]
    UnrelatedDescriptorTreePut tr -> [Model $ model & unrelatedDescriptorTree .~ tr]
    DescriptorTreePutParent ->
      [ Task
          ( DescriptorTreePut
              <$> getParentDescriptorTree
                (model ^. dbConn)
                (model ^. descriptorTree)
          )
      ]
    DescriptorCommitNewDescriptorText ->
      [ Task
          ( PutExtern
              <$> createNewDescriptors
                (model ^. dbConn)
                (T.words (model ^. newDescriptorText))
          ),
        asyncEvent RefreshBothDescriptorTrees
      ]
    DescriptorDelete d ->
      [ Task (PutExtern <$> deleteDescriptor (model ^. dbConn) d),
        asyncEvent RefreshBothDescriptorTrees
      ]
    DescriptorCreateRelation ms is ->
      [ Task (PutExtern <$> relateTo (model ^. dbConn) ms is),
        asyncEvent RefreshBothDescriptorTrees
      ]
    DescriptorUnrelate is ->
      [ Task (PutExtern <$> unrelate (model ^. dbConn) is),
        asyncEvent RefreshBothDescriptorTrees
      ]
    ToggleDoSoloTag ->
      [Model $ model & (doSoloTag .~ not (model ^. doSoloTag))]
    RequestDescriptorTree s ->
      [ Task
          ( DescriptorTreePut
              <$> lookupInfraDescriptorTree (model ^. dbConn) s
          )
      ]
    RefreshUnrelatedDescriptorTree ->
      [ Task (UnrelatedDescriptorTreePut <$> getUnrelatedInfraTree (model ^. dbConn))
      ]
    RefreshBothDescriptorTrees ->
      [ Task
          ( RequestDescriptorTree
              <$> (return . maybe "#ALL#" descriptor . getNode $ model ^. descriptorTree)
          ),
        asyncEvent RefreshUnrelatedDescriptorTree
      ]
    ShellCmd ->
      [ Task
          ( PutExtern
              <$> do
                CM.unless (T.null (model ^. shellCmd)) $ do
                  let procArgs = T.words (model ^. shellCmd)
                  p <-
                    createProcess
                      . shell
                      $ L.unwords
                        . putFileArgs
                          (map T.unpack procArgs)
                        . map
                          ( T.unpack
                              . filePath
                              . file
                          )
                        $ (model ^. fileSelection)
                  putStrLn $ "Running " ++ T.unpack (model ^. shellCmd)
          )
      ]
    PutExtern _ -> []
    TagCommitTagsString ->
      [ asyncEvent $
          if model ^. doSoloTag
            then TagCommitTagsStringDoSolo
            else TagCommitTagsStringDoSelection
      ]
    TagCommitTagsStringDoSolo ->
      [ let dds = T.words $ model ^. tagsString
            conn = model ^. dbConn
            sf = M.maybeToList $ model ^. fileSingle
            tm = model ^. taggingMode
         in Task
              ( PutExtern <$> (if isUntagMode tm then untagWith else tag) conn sf dds
              ),
        asyncEvent FileSelectionRefresh_
      ]
    TagCommitTagsStringDoSelection ->
      [ let dds = T.words $ model ^. tagsString
            conn = model ^. dbConn
            fs = model ^. fileSelection
            tm = model ^. taggingMode
         in Task
              ( PutExtern
                  <$> (if isUntagMode tm then untagWith else tag) conn fs dds
              ),
        asyncEvent FileSelectionRefresh_
      ]
    DebugPrintSelection -> [Task (PutExtern <$> print (model ^. fileSelection))]

-- | Replaces "%file" in the first list with the entirety of the second.
putFileArgs :: [String] -> [String] -> [String]
putFileArgs args files =
  let atFileArg = L.break (== "%file") args
   in fst atFileArg ++ files ++ (tail' . snd $ atFileArg)

tail' :: [a] -> [a]
tail' [] = []
tail' (_ : xs) = xs

last' :: [a] -> Maybe a
last' [] = Nothing
last' xs = Just . last $ xs

init' :: [a] -> [a]
init' [] = []
init' xs = init xs

-- | Take the first item and put it on the end
popCycleList :: [a] -> [a]
popCycleList [] = []
popCycleList (x : xs) = xs ++ [x]

-- | Take the last item and put it on the front
dequeueCycleList :: [a] -> [a]
dequeueCycleList [] = []
dequeueCycleList xs = last xs : init xs

asyncEvent :: e -> EventResponse s e sp ep
asyncEvent = Task . flip (<$) emptyM
  where
    emptyM = pure ()
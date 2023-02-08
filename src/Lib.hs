{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Lib
  ( Task,
    loadAllOrgTasks,
  )
where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericToEncoding, toEncoding)
import Data.List qualified as L (sort)
import Data.List.NonEmpty (toList)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Org (OrgDoc, OrgFile (orgDoc), Priority (priority), Section (sectionPriority), Todo (TODO), Words (Bold, Highlight, Image, Italic, Link, Plain, Punct, Strike, Underline, Verbatim), docSections, org, sectionDoc, sectionHeading, sectionTodo)
import Data.Text (Text, empty, singleton)
import Data.Text qualified as T (concat)
import Data.Text.IO qualified as TIO (readFile)
import GHC.Generics (Generic)
import System.Directory (getDirectoryContents)
import System.FilePath (takeExtension)

data Heading = Heading
  { headingTodo :: Maybe Todo,
    headingText :: Text,
    headingPriority :: Maybe Priority
  }

data Task = Task
  { taskText :: Text,
    taskPriority :: Maybe Text
  }
  deriving (Generic, Show)

instance Eq Task where
  (Task {taskText = t1, taskPriority = p1}) == (Task {taskText = t2, taskPriority = p2}) = t1 == t2 && p1 == p2

instance Ord Task where
  (Task {taskPriority = Just p1}) `compare` (Task {taskPriority = Just p2}) = p1 `compare` p2
  (Task {taskPriority = Nothing}) `compare` (Task {taskPriority = Nothing}) = EQ
  (Task {taskPriority = Just _}) `compare` (Task {taskPriority = Nothing}) = GT
  (Task {taskPriority = Nothing}) `compare` (Task {taskPriority = Just _}) = LT

instance ToJSON Task where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Task

flattenWord :: Words -> Text
flattenWord (Bold x) = x
flattenWord (Italic x) = x
flattenWord (Highlight x) = x
flattenWord (Underline x) = x
flattenWord (Verbatim x) = x
flattenWord (Strike _) = empty
flattenWord (Link _ _) = empty
flattenWord (Image _) = empty
flattenWord (Punct x) = singleton x
flattenWord (Plain x) = x

flattenWords :: [Words] -> Text
flattenWords = T.concat . map flattenWord

extractSections :: OrgDoc -> [Section]
extractSections doc = docSections doc ++ concatMap (extractSections . sectionDoc) (docSections doc)

newHeading :: Section -> Heading
newHeading sec = Heading (sectionTodo sec) (flattenWords $ toList $ sectionHeading sec) (sectionPriority sec)

newTask :: Heading -> Maybe Task
newTask Heading {headingTodo = Just TODO, headingText = text, headingPriority = p} = Just (Task text (fmap priority p))
newTask _ = Nothing

convertTasks :: [Heading] -> [Task]
convertTasks = mapMaybe newTask

extractHeadings :: OrgDoc -> [Heading]
extractHeadings = map newHeading . extractSections

extractTasks :: OrgDoc -> [Task]
extractTasks = convertTasks . extractHeadings

loadOrgFile :: FilePath -> IO (Maybe OrgDoc)
loadOrgFile path = fmap orgDoc . org <$> TIO.readFile path

loadOrgTasks :: FilePath -> IO (Maybe [Task])
loadOrgTasks path = fmap extractTasks <$> loadOrgFile path

isOrgFile :: FilePath -> Bool
isOrgFile path = ".org" == takeExtension path

appendRoot :: FilePath -> FilePath -> FilePath
appendRoot root x = root ++ x

loadAllOrgTasks :: FilePath -> IO [Task]
loadAllOrgTasks path = do
  allFiles <- getDirectoryContents path
  let filtered = filter isOrgFile $ map (appendRoot path) allFiles
  allTasks <- mapM loadOrgTasks filtered
  let woMaybe = L.sort . concat $ catMaybes allTasks
  return woMaybe

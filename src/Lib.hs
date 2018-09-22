{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Lib where

import Prelude hiding (
  FilePath,
  )
import Data.List (
  sort,
  partition,
  )
import Data.Maybe (
  isJust,
  fromJust,
  )
import Data.Text (
  Text,
  pack,
  unpack,
  commonPrefixes,
  dropWhileEnd,
  )
import System.Directory (
  getHomeDirectory,
  )
import qualified System.Directory as SD
import Shelly (
  Sh,
  shelly,
  verbosely,
  toTextIgnore,
  liftIO,
  )
import Filesystem.Path.CurrentOS (
  FilePath,
  fromText,
  splitDirectories,
  (</>),
  )
import qualified Data.Map.Lazy as Map
import qualified Filesystem.Path.CurrentOS as FP
import qualified Shelly as S


{-
  TODO
  - Unit test all the functions I have at this time
  - Learn about a json library (probably Aeson) so that I can
    serialize directory trees
  - Write the code to walk a directory tree in two ways:
    - constructively (this is the only one that's really needed)
    - destructively, cleaning up any dead symlinks based on
      a previous run
    - maybe add a third run to clean up empty directories, after
      symlinks are gone
 -}

shI :: Show a => Sh a -> IO a
shI action = shelly $ verbosely $ do
  out <- action
  liftIO $ print out
  return out


-- Equivalent of python's os.path.expanduser

expandTilde :: FilePath -> Sh FilePath
expandTilde = liftIO . expandTilde_

expandTilde_ :: FilePath -> IO FilePath
expandTilde_ path = case splitDirectories path of
  ["~"] -> toFilePath <$> getHomeDirectory
  ["~/"] -> toFilePath <$> getHomeDirectory
  "~/" : rest -> do
    home <- toFilePath <$> getHomeDirectory
    return $ FP.concat $ home : rest
  _ -> return path
  where
    toFilePath = fromText . pack


-- Tools for manipulating FilePaths logically (without doing IO,
-- which many of the FP.* and S.* functions do)

stripLastSlash :: FilePath -> FilePath
stripLastSlash = fromText . (dropWhileEnd (== '/')) . toTextIgnore

equivFilePath :: FilePath -> FilePath -> Bool
equivFilePath a b = stripLastSlash a == stripLastSlash b

data PathRelation = PrEquiv FilePath
                  | PrRootAndRelative FilePath FilePath
                  | PrCousins FilePath FilePath
                  deriving (Show, Eq)


findPathRelation :: FilePath -> FilePath -> PathRelation
findPathRelation a b =
  let
    splitA = FP.splitDirectories a
    splitB = FP.splitDirectories b
    go :: [FilePath] -> [FilePath] -> [FilePath] -> PathRelation
    go aa bb commonRoot =
      let
        rootPath = stripLastSlash $ FP.concat commonRoot
        relPath pathList = stripLastSlash $ FP.concat pathList
        absPath pathList = stripLastSlash $ rootPath </> (relPath pathList)
      in case (aa, bb) of
        ([], [])       -> PrEquiv rootPath
        ([], _)        -> PrRootAndRelative rootPath (relPath bb)
        (_, [])        -> PrRootAndRelative rootPath (relPath aa)
        (ah:at, bh:bt) -> if ah `equivFilePath` bh
          then go at bt (commonRoot ++ [ah])
          else PrCousins (absPath aa) (absPath bb)
  in
    go splitA splitB []


isParentDir :: FilePath -> FilePath -> Bool
isParentDir parent child = case (findPathRelation parent child) of
  PrEquiv _ -> True
  PrRootAndRelative root _ -> parent `equivFilePath` root
  _ -> False


relativePath :: FilePath -> FilePath -> Either Text FilePath
relativePath parent child = case (findPathRelation parent child) of
  PrEquiv _ -> Right ""
  PrCousins _ _ -> err
  PrRootAndRelative root relative -> if parent `equivFilePath` root
    then Right relative
    else err
  where
    err = Left $ "Path " <> (toTextIgnore parent) <>
                 " is not a parent of " <> (toTextIgnore child)

relativeTo :: FilePath -> FilePath -> FilePath
relativeTo child parent = case (relativePath parent child) of
  Left _ ->
    let msg = (toTextIgnore child) <> " not a child of " <> (toTextIgnore parent)
    in error $ unpack msg
  Right relative -> relative

-- Definition and creating FsTree data

data FsTree = FsLeaf FilePath
            | FsDir FilePath [FsTree]
  deriving (Show, Eq)

repoFsTree :: FilePath -> Sh FsTree
repoFsTree path =
  do
    path_ <- expandTilde path
    isDir <- S.test_d path_
    if isDir then do
      let notGit = not . (== (path_ </> ".git"))
      contents <-  (filter notGit) <$> S.ls path_
      FsDir path_ <$> mapM repoFsTree contents
    else
      return $ FsLeaf path_

-- Manipulating FsTree data

makeRelative :: FsTree -> FsTree
makeRelative (FsLeaf path) = FsLeaf path
makeRelative (FsDir path subtrees) =
  FsDir "." $ map (go path) subtrees
  where
    go :: FilePath -> FsTree -> FsTree
    go parent (FsLeaf f) = FsLeaf $ f `relativeTo` parent
    go parent (FsDir d subtrs) =
      let relativeD = d `relativeTo` parent
      in FsDir relativeD (map (go $ parent </> relativeD) subtrs)


makeAbsolute :: FilePath -> FsTree -> FsTree
makeAbsolute root (FsLeaf path) = FsLeaf $ root </> path
makeAbsolute root (FsDir path subtrees) =
  FsDir absPath $ map (makeAbsolute absPath) subtrees
  where absPath = if path == "."
          then root
          else root </> path


pruneEmptyDirs :: FsTree -> Maybe FsTree
pruneEmptyDirs f@(FsLeaf _) = Just f
pruneEmptyDirs d@(FsDir _ _) = go d
  where
   go f@(FsLeaf _) = Just f
   go (FsDir _ []) = Nothing
   go (FsDir p subtrees) =
     let pruned = pruneNothingFromList $ map go subtrees
     in case pruned of
       [] -> Nothing
       _ -> Just $ FsDir p pruned


pruneNothingFromList :: [Maybe a] -> [a]
pruneNothingFromList = (map fromJust) . (filter isJust)


-- printing FsTree data

pprintTree :: FsTree -> IO ()
pprintTree = putStrLn . unpack . prettyTextTree

prettyTextTree :: FsTree -> Text
prettyTextTree tree = go "" tree where
  showPath :: Text -> FilePath -> Text
  showPath prefix filePath = prefix <> toTextIgnore filePath <> "\n"
  go :: Text -> FsTree -> Text
  go prefix (FsLeaf path) = showPath prefix path
  go prefix (FsDir path subdirs) =
    (showPath prefix path) <>
    (mconcat $ map (go $ prefix <> "| ") subdirs)


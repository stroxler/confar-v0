{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Lib where

import Prelude hiding (
  FilePath,
  )
import Data.Maybe (
  isJust,
  fromJust,
  )
import Data.Text (
  Text,
  pack,
  unpack,
  dropWhileEnd,
  )
import qualified Data.Set as Set
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
import qualified Filesystem.Path.CurrentOS as FP
import qualified Shelly as S


{-
  TODO: in a major refactor
  - Rework my library interactions
    - Make an FPath typeclass that can be converted to
      Prelude.FilePath, String, Text, and FP.FilePath.
      Make instances for all of them
    - Get rid of the unix library
  - Rework the current FsTree class
    - Rename the current FsTree type to FsNode, and make it
      polymorphic
    - Make a new FsTree class to contain the top-level info
    - When creating an FsTree, maybe take an FsNode and map it
      to use a record for the path that should have all three
      path formats: absolute, relative to root, relative to parent.
      Alternatively, have an enum on FsTree so that it's easy
      to toggle between these or something.
  - Extend the functionality
    - Learn to serialize to yaml or json with Aeson
    - Learn to make subcommands using Options.Applicative
    - Figure out a data structure - with serialization - to
      record the state of a confar setup.
      - Extend the install to remove dead links if they are
        removed from a repo
      - Write a command that will clear the state so you can do
        a fresh install
    - Create a config file that lets you specify, in a directory
      root, locations of repos to use. Make a new command that
      will handle multiple repos and serialize all of the results.
      Try to design it so that it can handle files moving between
      confar repos.
    - Extend the aforementioned config so that we can also git
      clone all the repos; that way we can specify git urls
      in the target directory rather than file locations.
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


reservedGitNames :: Set.Set FilePath
reservedGitNames = Set.fromList
  [".git"
  , ".gitignore"
  , ".gitattributes"
  , ".mailmap"
  ]


data FsTree = FsLeaf FilePath
            | FsDir FilePath [FsTree]
  deriving (Show, Eq)

repoFsTree :: FilePath -> Sh FsTree
repoFsTree path =
  do
    path_ <- expandTilde path
    isDir <- S.test_d path_
    if isDir then do
      let notGit p = not $ Set.member (FP.basename p) reservedGitNames
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


-- doing confar stuff  (CONF As Repositories)

confarInstall :: FilePath -> FilePath -> Sh ()
confarInstall repoDir targetDir = do
  repoTree <- pruneEmptyDirs <$> repoFsTree repoDir
  case repoTree of
    Nothing -> error $ "Cannot confarWalk an empty directory at " <> (show repoDir)
    Just leaf@(FsLeaf _) -> error $ "Cannot confarWalk just a file " <> (show leaf)
    Just tree@(FsDir root _) ->
      walkFsTree handleDir handleFile tree
      where
        relativeToTarget path = targetDir </> (path `relativeTo` root)
        handleDir path = (S.mkdir_p . relativeToTarget) path
        handleFile path =
          let
            src = unpack $ toTextIgnore path
            dst = unpack $ toTextIgnore $ relativeToTarget path
          in liftIO $ do
            exists <- SD.doesPathExist dst
            if exists
            then do
              okay <- do
                 isLink <- SD.pathIsSymbolicLink dst
                 if isLink
                 then (== src) <$> SD.getSymbolicLinkTarget dst
                 else return False
              if okay
              then putStrLn $ "Skipping existing symlink " <> dst <> " -> " <> src
              else error $ "Cannot create symlink at " <> dst
            else do
              putStrLn $ "Symlinking " <> src <> " to " <> dst
              SD.createFileLink src dst


walkFsTree :: (FilePath -> Sh ()) -> (FilePath -> Sh ()) -> FsTree -> Sh ()
walkFsTree onDir onLeaf tree = case tree of
  FsLeaf path -> do
    onLeaf path
  FsDir path children -> do
    onDir path
    mapM_ (walkFsTree onDir onLeaf) children


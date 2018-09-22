{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Test.Hspec
import Test.HUnit as Export hiding (path)
import Lib
import Filesystem.Path.CurrentOS (splitDirectories, FilePath)
import Shelly (shelly, relativeTo)
import Control.Exception (evaluate)


main :: IO ()
main = hspec $ do
  describe "expandTilde_" $ do
    it "is a noop on root paths" $ do
      expandTilde_ "/some/dir/" `shouldReturn` "/some/dir/"
    it "is a noop on relative paths" $ do
      expandTilde_ "some/dir/" `shouldReturn` "some/dir/"
    it "handles tilde-prefixed paths" $ do
      let
        didExpand :: IO Bool
        didExpand = do
          expansion <- expandTilde_ "~/some/file"
          return (
            case splitDirectories expansion of
              "~" : _ -> False
              _ : ["some/", "file"] -> True
              _ -> False
            )
        in
        didExpand `shouldReturn` False
  describe "expandTilde" $ do
    it "should call expandTilde_" $ do
      (shelly $ expandTilde "/abs/path") `shouldReturn` "/abs/path"


  describe "equivFilePath" $ do
    it "should be True when file paths are exactly the same" $ do
      equivFilePath "/my/file/path" "/my/file/path" `shouldBe` True
    it "should be True when file paths are the same up to '/'" $ do
      equivFilePath "/my/file/path" "/my/file/path/" `shouldBe` True
      equivFilePath "/my/file/path/" "/my/file/path" `shouldBe` True
    it "should be False for differing paths" $ do
      equivFilePath "/my/first" "/my/second" `shouldBe` False
      equivFilePath "/my/parent" "/my/parent/child" `shouldBe` False

  describe "findPathRelation" $ do
    it "should handle equivalent paths correctly" $ do
      findPathRelation "/same/p" "/same/p" `shouldBe` PrEquiv "/same/p"
      findPathRelation "/same/p/" "/same/p" `shouldBe` PrEquiv "/same/p"
      findPathRelation "/same/p" "/same/p/" `shouldBe` PrEquiv "/same/p"
    it "should handle unrelated cousins correctly" $ do
      findPathRelation "rel/p" "/abs/p/" `shouldBe` PrCousins "rel/p" "/abs/p"
    it "should handle related cousins correctly" $ do
      findPathRelation "rel/p/a/" "rel/p/b" `shouldBe` PrCousins "rel/p/a" "rel/p/b"
      findPathRelation "/abs/p/a/" "/abs/p/b" `shouldBe` PrCousins "/abs/p/a" "/abs/p/b"
    it "should handle a or b as a root, with abs path and no trailing slash" $ do
      findPathRelation "/p/to/a" "/p/to/a/b" `shouldBe` PrRootAndRelative "/p/to/a" "b"
      findPathRelation "/p/to/b/a" "/p/to/b" `shouldBe` PrRootAndRelative "/p/to/b" "a"
    it "should handle a or b as a root, with rel path and trailing slash" $ do
      findPathRelation "p/to/a/" "p/to/a/b/" `shouldBe` PrRootAndRelative "p/to/a" "b"
      findPathRelation "p/to/b/a/" "p/to/b/" `shouldBe` PrRootAndRelative "p/to/b" "a"
    it "should handle a or b as root, with the other nested" $ do
      findPathRelation "p/to/a/" "p/to/a/b/b/" `shouldBe` PrRootAndRelative "p/to/a" "b/b"
      findPathRelation "p/to/b/a/a/" "p/to/b/" `shouldBe` PrRootAndRelative "p/to/b" "a/a"

  describe "isParentDir" $ do
    it "should return True for equivalent paths" $ do
      "/path/a" `isParentDir` "/path/a/" `shouldBe` True
      "path/a/" `isParentDir` "path/a" `shouldBe` True
    it "should correctly identify parent directory, with or without slash" $ do
      "/some/path/" `isParentDir` "/some/path/and/child" `shouldBe` True
      "rel/path" `isParentDir` "rel/path/child" `shouldBe` True
    it "should correctly identify non-parent directory" $ do
      "/some/path/" `isParentDir` "/some/path_/child" `shouldBe` False
      "/some/path" `isParentDir` "/some/path_/child" `shouldBe` False

  describe "relativePath" $ do
    it "should correctly handle equivalent paths" $ do
      relativePath "/path/a" "/path/a/" `shouldBe` Right ""
      relativePath "path/a/" "path/a" `shouldBe` Right ""
    it "should handle a parent and child" $ do
      relativePath "/some/path/" "/some/path/and/child/" `shouldBe` Right "and/child"
      relativePath "rel/path" "rel/path/child" `shouldBe` Right "child"
    it "should correctly give Left on cousins" $ do
      relativePath "/p/a" "/p/b" `shouldBe` Left "Path /p/a is not a parent of /p/b"
    it "should correctly give Left when child candidate is actually parent" $ do
      relativePath "/p/a/b" "/p/a" `shouldBe` Left "Path /p/a/b is not a parent of /p/a"

  describe "repoFsTree" $ do
    let expectedInitial = FsDir
          "test/data/fake_repo"
          [ FsLeaf "test/data/fake_repo/stuff.txt"
          , FsDir
            "test/data/fake_repo/inner_directory"
            [ FsLeaf "test/data/fake_repo/inner_directory/stuff.txt"]]
    let expectedRelative = FsDir
          "."
          [ FsLeaf "stuff.txt"
          , FsDir
            "inner_directory"
            [ FsLeaf "stuff.txt" ]
          ]
    let hasEmptyDirs = FsDir
          "nonempty"
          [ FsLeaf "nonempty/file.txt"
          , FsDir "nonempty/empty0" []
          , FsDir "nonempty/empty1" [ FsDir "nonempty/empty1/empty2" [] ]
          ]
    it "should give correct output on some test data" $ do
      let repo = "test/data/fake_repo"
      result <- shelly $ repoFsTree repo
      result @?= expectedInitial
    it "should be as expected after makeRelative" $ do
      let result = makeRelative expectedInitial
      result @?= expectedRelative
    it "should be as expected after makeAbsolute" $ do
      let result = makeAbsolute "test/data/fake_repo" expectedRelative
      result @?= expectedInitial
    it "should be properly prunned by pruneEmptyDirs" $ do
      pruneEmptyDirs (FsDir "empty" []) @?= Nothing
      pruneEmptyDirs expectedInitial @?= Just expectedInitial
      let result = pruneEmptyDirs hasEmptyDirs
      result @?= (Just $ FsDir "nonempty" [ FsLeaf "nonempty/file.txt" ])

  {-
  describe "filesInRepository" $ do
    it "should produce correct, sorted output on a fake repo" $ do
      result <- shelly $ do
        absPaths <- filesInRepository repo
        mapM (relativeTo repo) absPaths
      result @?= ["inner_directory", "stuff.txt",
                  "inner_directory/stuff.txt"]

  describe "isParentOf" $ do
    it "should return False with no common prefix" $ do
      isParentOf "/some/path" "some/path" `shouldBe` False
    it "should return False with incomplete common prefix" $ do
      isParentOf "some/otherpath" "some/path" `shouldBe` False
    it "should return False when parent is a prefix but not at a '/'" $ do
      isParentOf "some/path" "some/path_to/file" `shouldBe` False
    it "should return True for an empty parent" $ do
      isParentOf "" "something" `shouldBe` True
    it "should return True on exact match" $ do
      isParentOf "some/path" "some/path" `shouldBe` True
    it "should return True with full common prefix" $ do
      isParentOf "some/path" "some/path/to/thing" `shouldBe` True

-}

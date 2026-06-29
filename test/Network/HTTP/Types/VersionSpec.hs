{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Network.HTTP.Types.VersionSpec (main, spec) where

import Test.Hspec

import Network.HTTP.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Regression tests" $
        mapM_ checkVersion allVersions
    describe "Patterns" $ do
        patternMatch http09 (\Http09 -> pure ())
        patternMatch http10 (\Http10 -> pure ())
        patternMatch http11 (\Http11 -> pure ())
        patternMatch http20 (\Http20 -> pure ())
        patternMatch http30 (\Http30 -> pure ())

patternMatch :: HttpVersion -> (HttpVersion -> IO ()) -> SpecWith ()
patternMatch hv f =
    it name $ f hv `shouldReturn` ()
  where
    name = "match the version " <> show hv

-- | [("Rendered", {constant}, {literal})]
allVersions :: [(String, HttpVersion, HttpVersion)]
allVersions =
    [ ("HTTP/0.9", http09, HttpVersion 0 9)
    , ("HTTP/1.0", http10, HttpVersion 1 0)
    , ("HTTP/1.1", http11, HttpVersion 1 1)
    , ("HTTP/2.0", http20, HttpVersion 2 0)
    , ("HTTP/3.0", http30, HttpVersion 3 0)
    ]

checkVersion :: (String, HttpVersion, HttpVersion) -> Spec
checkVersion (str, v1, v2) =
    it str $ do
        v1 `shouldBe` v2
        show v1 `shouldBe` str

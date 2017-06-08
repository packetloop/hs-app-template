{-# LANGUAGE OverloadedStrings #-}

module App.OptionsSpec
  ( spec
  ) where

import App.Options

import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

spec :: Spec
spec = describe "App.OptionsSpec" $ do
    it "should parse tags" $
      string2Tags "club_name:indica,deploy_id:manual" `shouldBe` [StatsTag ("club_name", "indica"), StatsTag ("deploy_id", "manual")]

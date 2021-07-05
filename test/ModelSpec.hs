{-# LANGUAGE OverloadedStrings #-}

module ModelSpec where

import Model (colorToMaybeColorType, ColorType(..))

import Test.Tasty.HUnit     ((@?=), Assertion)


unit_colorToMaybeColorTypeDependencyErrorColor :: Assertion
unit_colorToMaybeColorTypeDependencyErrorColor =
  let actual   = traverse colorToMaybeColorType ["green", "GREEN", "GreEn"]
      expected = Just [DependencyErrorColor, DependencyErrorColor, DependencyErrorColor]
  in actual @?= expected


unit_colorToMaybeColorTypeCompilerErrorColor :: Assertion
unit_colorToMaybeColorTypeCompilerErrorColor =
  let actual   = traverse colorToMaybeColorType ["red", "RED", "rEd"]
      expected = Just [CompilerErrorColor, CompilerErrorColor, CompilerErrorColor]
  in actual @?= expected


unit_colorToMaybeColorTypeCompilerSuggestionColor :: Assertion
unit_colorToMaybeColorTypeCompilerSuggestionColor =
  let actual   = traverse colorToMaybeColorType ["yellow", "YELLOW", "yELLoW"]
      expected = Just [CompilerSuggestionColor, CompilerSuggestionColor, CompilerSuggestionColor]
  in actual @?= expected


unit_colorToMaybeColorTypeOtherColor :: Assertion
unit_colorToMaybeColorTypeOtherColor =
  let actual   = colorToMaybeColorType <$> ["indigo", "LIme", "Rouge", "Sky Blue"]
      expected = [Nothing, Nothing, Nothing, Nothing]
  in actual @?= expected


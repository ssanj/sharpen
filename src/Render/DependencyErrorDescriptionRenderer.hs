{-# LANGUAGE OverloadedStrings #-}

module Render.DependencyErrorDescriptionRenderer (render) where

import Data.Foldable (traverse_)
import Model       (ColorTheme, Printer(..), showt)
import RenderModel (DependencyErrorDescription(..))
import Theme       (renderProblem, fromTheme)

render :: ColorTheme -> DependencyErrorDescription -> IO ()
render = renderGeneralProblemsInFile . fromTheme


renderGeneralErrorHeader :: Printer -> DependencyErrorDescription -> IO ()
renderGeneralErrorHeader printer (DependencyErrorDescription title path _) =
  let heading     = "-- " <> showt title <> " ---------- " <> showt path
      titleColor = printerTitleColor printer $ heading
      paragraph  = printerParagraph printer
  in titleColor >> paragraph


renderGeneralProblemsInFile :: Printer -> DependencyErrorDescription  -> IO ()
renderGeneralProblemsInFile printer probsInFile =
  do
    printerBorder printer
    renderGeneralErrorHeader printer probsInFile
    printerParagraph printer
    traverse_  (renderProblem printer) $ dependencyErrorDescriptionPathProblemDescriptions probsInFile
    printerBorder printer

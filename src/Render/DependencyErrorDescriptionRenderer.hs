{-# LANGUAGE OverloadedStrings #-}

module Render.DependencyErrorDescriptionRenderer (render) where

import Data.Foldable (traverse_)
import Model         (ColorTheme, Printer(..), showt)
import RenderModel   (DependencyErrorDescription(..))
import Theme         (fromTheme)
import Render.Common (renderProblem)


render :: ColorTheme -> DependencyErrorDescription -> IO ()
render = renderGeneralProblemsInFile . fromTheme


renderGeneralProblemsInFile :: Printer -> DependencyErrorDescription  -> IO ()
renderGeneralProblemsInFile printer probsInFile =
  do
    printerBorder printer
    renderGeneralErrorHeader printer probsInFile
    printerParagraph printer
    traverse_  (renderProblem printer) $ dependencyErrorDescriptionPathProblemDescriptions probsInFile
    printerBorder printer


renderGeneralErrorHeader :: Printer -> DependencyErrorDescription -> IO ()
renderGeneralErrorHeader printer (DependencyErrorDescription errorHeading path _) =
  let heading   = "-- " <> showt errorHeading <> " ---------- " <> showt path
      title     = printerTitleColor printer heading
      paragraph = printerParagraph printer
  in title >> paragraph

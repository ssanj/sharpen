module Render.CompilerErrorDescriptionRenderer where

import Model       (ColorTheme(..))
import RenderModel (CompilerErrorRenderModel(..))
import Theme       (renderStats2, renderCompilerErrorDescription2, fromTheme)

render :: ColorTheme -> CompilerErrorRenderModel -> IO ()
render colorTheme compilerErrorRenderModel =
  let
      compilerErrorDesc    = compilerErrorRenderModelCompilerErrorDescription compilerErrorRenderModel
      numProblemsDisplayed = compilerErrorRenderModelProblemsToDisplay compilerErrorRenderModel
      stats                = compilerErrorRenderModelStats compilerErrorRenderModel
      printer              = fromTheme colorTheme
      renderEnabledStats   = renderStats2 printer stats numProblemsDisplayed
      renderCompilerErrors = renderCompilerErrorDescription2 printer compilerErrorDesc
  in renderCompilerErrors >> renderEnabledStats

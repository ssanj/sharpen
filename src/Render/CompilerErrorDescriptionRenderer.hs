module Render.CompilerErrorDescriptionRenderer where

import Model       (ColorTheme(..))
import RenderModel (CompilerErrorRenderModel(..))
import Theme       (renderStats, renderCompilerErrorDescription, fromTheme)

render :: ColorTheme -> CompilerErrorRenderModel -> IO ()
render colorTheme compilerErrorRenderModel =
  let
      compilerErrorDesc    = compilerErrorRenderModelCompilerErrorDescription compilerErrorRenderModel
      numProblemsDisplayed = compilerErrorRenderModelProblemsToDisplay compilerErrorRenderModel
      stats                = compilerErrorRenderModelStats compilerErrorRenderModel
      printer              = fromTheme colorTheme
      renderEnabledStats   = renderStats printer stats numProblemsDisplayed
      renderCompilerErrors = renderCompilerErrorDescription printer compilerErrorDesc
  in renderCompilerErrors >> renderEnabledStats

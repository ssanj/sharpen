module Render.CompilerErrorDescriptionRenderer where

import RenderModel (CompilerErrorRenderModel(..))

import Theme (renderStats, renderCompilerErrorDescription)

render :: CompilerErrorRenderModel -> IO ()
render compilerErrorRenderModel =
  let
      compilerErrorDesc    = compilerErrorRenderModelCompilerErrorDescription compilerErrorRenderModel
      numProblemsDisplayed = compilerErrorRenderModelProblemsToDisplay compilerErrorRenderModel
      stats                = compilerErrorRenderModelStats compilerErrorRenderModel

      renderEnabledStats   = renderStats stats numProblemsDisplayed
      renderCompilerErrors = renderCompilerErrorDescription compilerErrorDesc
  in renderCompilerErrors >> renderEnabledStats

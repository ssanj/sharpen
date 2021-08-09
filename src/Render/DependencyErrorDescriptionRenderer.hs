module Render.DependencyErrorDescriptionRenderer where

import RenderModel (DependencyErrorDescription)
import Theme       (renderGeneralProblemsInFile)

-- Could this be a typeclass?
render :: DependencyErrorDescription -> IO ()
render = renderGeneralProblemsInFile

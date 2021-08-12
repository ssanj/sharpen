module Render.DependencyErrorDescriptionRenderer where

import Model       (ColorTheme)
import RenderModel (DependencyErrorDescription)
import Theme       (renderGeneralProblemsInFile2, fromTheme)

-- Could this be a typeclass?
render :: ColorTheme -> DependencyErrorDescription -> IO ()
render = renderGeneralProblemsInFile2 . fromTheme

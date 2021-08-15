module Render.DependencyErrorDescriptionRenderer where

import Model       (ColorTheme)
import RenderModel (DependencyErrorDescription)
import Theme       (renderGeneralProblemsInFile, fromTheme)

render :: ColorTheme -> DependencyErrorDescription -> IO ()
render = renderGeneralProblemsInFile . fromTheme

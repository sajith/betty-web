module Betty.Vendor where

import Data.Text    (Text)

import Yesod.Static

------------------------------------------------------------------------

-- It should help to accumulate all the third party path cruft in one
-- place rather than all over the place.  (Maybe not?)

------------------------------------------------------------------------

staticPath :: [Text] -> Route Static
staticPath xs = StaticRoute xs []

vendorPath :: [Text] -> Route Static
vendorPath xs = staticPath ("vendor":xs)

------------------------------------------------------------------------

-- faCss :: StaticRoute
faCss :: Route Static
faCss = vendorPath ["font-awesome-4.2.0", "css", "font-awesome.min.css"]

------------------------------------------------------------------------

modernizrJs :: StaticRoute
modernizrJs = staticPath ["js", "modernizr.custom.03621.js"]

------------------------------------------------------------------------

jQueryJs :: StaticRoute
jQueryJs = staticPath ["js", "jquery-1.11.1.min.js"]

------------------------------------------------------------------------

bootstrapJs :: StaticRoute
bootstrapJs = staticPath ["js", "bootstrap.min.js"]

bootstrapCss :: StaticRoute
bootstrapCss = staticPath ["css", "bootstrap.min.css"]

------------------------------------------------------------------------

respondJs :: StaticRoute
respondJs = staticPath ["js", "respond.min.js"]

------------------------------------------------------------------------

jQueryUiCss :: StaticRoute
jQueryUiCss = vendorPath ["jquery-ui-1.11.1", "jquery-ui.min.css"]

jQueryUiJs :: StaticRoute
jQueryUiJs = vendorPath ["jquery-ui-1.11.1", "jquery-ui.min.js"]

------------------------------------------------------------------------

timepickerCss :: StaticRoute
timepickerCss = vendorPath ["jquery-ui-timepicker-0.3.3",
                            "jquery.ui.timepicker.css"]

timepickerJs :: StaticRoute
timepickerJs = vendorPath ["jquery-ui-timepicker-0.3.3",
                           "jquery.ui.timepicker.js"]

timepickerThemeCss :: StaticRoute
timepickerThemeCss = vendorPath ["jquery-ui-timepicker-0.3.3",
                                 "include", "ui-1.10.0", "ui-lightness",
                                 "jquery-ui-1.10.0.custom.min.css"]

------------------------------------------------------------------------

jqPlotCss :: StaticRoute
jqPlotCss = vendorPath ["jqplot-1.0.8", "jquery.jqplot.min.css"]

jqPlotJs :: StaticRoute
jqPlotJs = vendorPath ["jqplot-1.0.8", "jquery.jqplot.min.js"]

jqPlotDateAxisJs :: StaticRoute
jqPlotDateAxisJs = vendorPath ["jqplot-1.0.8", "plugins",
                               "jqplot.dateAxisRenderer.min.js"]

------------------------------------------------------------------------

d3Js :: StaticRoute
d3Js = vendorPath ["d3-3.5.5", "d3.min.js"]

------------------------------------------------------------------------

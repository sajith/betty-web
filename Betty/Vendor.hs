module Betty.Vendor where

import Settings.StaticFiles
import Yesod.Static         (StaticRoute)

------------------------------------------------------------------------

-- It should help to accumulate all the third party path cruft in one
-- place rather than all over the place.  (Maybe not?)

------------------------------------------------------------------------

faCss :: StaticRoute
faCss = vendor_font_awesome_4_2_0_css_font_awesome_min_css

------------------------------------------------------------------------

jQueryUiCss :: StaticRoute
jQueryUiCss = vendor_jquery_ui_1_11_1_jquery_ui_min_css

jQueryUiJs :: StaticRoute
jQueryUiJs = vendor_jquery_ui_1_11_1_jquery_ui_min_js

------------------------------------------------------------------------

timepickerCss :: StaticRoute
timepickerCss = vendor_jquery_ui_timepicker_0_3_3_jquery_ui_timepicker_css

timepickerJs :: StaticRoute
timepickerJs = vendor_jquery_ui_timepicker_0_3_3_jquery_ui_timepicker_js

timepickerThemeCss :: StaticRoute
timepickerThemeCss = vendor_jquery_ui_timepicker_0_3_3_include_ui_1_10_0_ui_lightness_jquery_ui_1_10_0_custom_min_css

------------------------------------------------------------------------

jqPlotCss :: StaticRoute
jqPlotCss = vendor_jqplot_1_0_8_jquery_jqplot_min_css

jqPlotJs :: StaticRoute
jqPlotJs = vendor_jqplot_1_0_8_jquery_jqplot_min_js

jqPlotDateAxisJs :: StaticRoute
jqPlotDateAxisJs = vendor_jqplot_1_0_8_plugins_jqplot_dateAxisRenderer_min_js

------------------------------------------------------------------------

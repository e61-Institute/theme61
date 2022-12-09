# theme61 0.2.0

* Improvements to the automatic discrete colour palette selection.
* Changes to the theme to fix graphs at a 0.75 aspect ratio.
* Added an alternate minimal theme called `theme_e61_clean()`.
* New add-in that creates charts based on `esquisse`.
* Updated `save_e61()` to allow you to save plots other than the most recently generated one.
* Many functions have been renamed to more closely align with existing ggplot2 function names, with `_e61` appended to them, for example `e61_colour_manual` now becomes `scale_colour_e61`. The old functions have now been deprecated and will be made defunct in future versions. Please stop using them and use the new functions instead.

# theme61 0.1.0

This is the first functional release of theme61 containing everything you need to make a graph in the e61 style.

* This includes `ggplot` functions that customise the theme, axes and other graph elements.
* Implements the e61 Institute colour palette in `scale_colour/fill_*` function and provides the colour names as data files for manual access as required.
* Save functions to ensure that default graphs have the appropriate sizing and dimensions.

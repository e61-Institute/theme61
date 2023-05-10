# theme61 0.4.1

08 May 2023

#### New functions

* Added new functions `set_open_graph()` and `unset_open_graph()` which toggle an option to automatically open a graph that has been created with `save_e61()`. Helpful for your workflow when perfecting graphs.

#### New/changed functionality

* Change the default alignment of plot labels to left-align and added a new argument `hjust` to `plot_label()`/`mplot_label()` to change the alignment.
* Added the ability to add a white box around text in `plot_label()`/`mplot_label()` through `geom` argument.
* Added `y` argument to `add_zeroline()` so you can put your zero line at values other than zero.
* Added argument to `scale_x_continuous_e61()` to add back the first/last labels that are removed by default.
* Slightly increased default size of plot labels to match size of axis text.

#### Bug fixes and documentation changes

* Permanent fix to the bug that was temporarily bodged in v0.4.1. Now dates can be entered as strings in `plot_label()` and they will be converted to dates.
* Fixed a bug in `scale_y_continuous_e61()` that made `y_top` stop working and also refactored the code.
* Fixed a bug in `mplot_label()` that didn't allow a vector of colours to be supplied.
* Added more documentation explaining how `rel_heights` in `mpanel_e61()` works.
* Miscellaneous minor documentation improvements.

# theme61 0.4.0

21 Apr 2023

* Add the ability to make multi-panel graphs via `mpanel_e61()` (referring to combining multiple discrete graphs into a single panel, not faceted graphs).
* Add dual y-axis support allowing for graphs with secondary y-axes with different scales to be created.
* Add experimental automatic graph height calculation in `save_e61()`, which should reduce the need for users to manually specify `height` when saving graphs.
* Add argument to `save_e61()` to allow you to save the data used to produce the graph as a csv.
* Correct whitespace margins around graphs.
* Improvements to various documentation.

# theme61 0.3.1

15 Mar 2023

* Refactored `mplot_label()` to use `plot_label()` as the underlying function rather than a `data.frame` and `geom_text()`. This fixed two bugs where the function throws an error if `colour` is an aesthetic in `ggplot()` `aes` and the `x` argument was not automatically converting dates.
* Changed the colour palette so the third colour is now dark blue rather than grey.

# theme61 0.3.0

09 Mar 2023

* Add new function `format_flip_bar_charts()` that applies most of the formatting needed to make horizontal bar graphs pretty.
* Add new functions `plot_label()` and `mplot_label()` that make it easier to add on-graph plot labels. `mplot_label()` is the vectorised version that lets you do multiple labels in one function.
* Add new function `add_zeroline()` to add an appropriately formatted black line along the zero y-axis.
* New functionality in `scale_y_continuous_e61()` to specify break intervals directly in the `limits` argument.
* New vignette explaining how and why to use on-graph labelling over legends.
* Add new messages and information to `save_e61()` to try and explain why users need to set their own dimensions to get good graph aesthetics.
* Add argument to turn off text wrapping in `labs_e61()`.
* Add argument to change the aspect ratio in `theme_e61()`.
* Change default dimension behaviour in `save_e61()` to make graphs look sensible at 8.5 cm widths (corresponds to half width of a Word document).
* Change `base_size` of graph text so the font size is readable at 8.5 cm widths.
* Remove `y_title_top_e61()` and incorporate functionality directly into `theme_e61()` as the default. Users can choose to disable this behaviour with the `y_top = FALSE` argument.
* Update the Getting Started vignette to showcase new functionality.
* Fix issue with `labels` argument in `scale_colour/fill_e61()` not working.
* Other minor improvements to functions, messages and documentation.

# theme61 0.2.1

20 Dec 2022

* Minor adjustments to title/footnote font size and default max character width to account for new fixed aspect ratio.

# theme61 0.2.0

14 Dec 2022

* Adds a base map function `e61_map()` with sensible defaults for mapping.
* Improvements to the automatic discrete colour palette selection.
* Changes to the theme to fix graphs at a 0.75 aspect ratio.
* Added an alternate minimal theme called `theme_e61_clean()`.
* New add-in that creates charts based on `esquisse`.
* Updated `save_e61()` to allow you to save plots other than the most recently generated one.
* Many functions have been renamed to more closely align with existing ggplot2 function names, with `_e61` appended to them, for example `e61_colour_manual` now becomes `scale_colour_e61`. The old functions have now been deprecated and will be made defunct in future versions. Please stop using them and use the new functions instead.

# theme61 0.1.0

13 May 2022

This is the first functional release of theme61 containing everything you need to make a graph in the e61 style.

* This includes `ggplot` functions that customise the theme, axes and other graph elements.
* Implements the e61 Institute colour palette in `scale_colour/fill_*` function and provides the colour names as data files for manual access as required.
* Save functions to ensure that default graphs have the appropriate sizing and dimensions.

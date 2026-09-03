# theme61 0.8.1

#### Performance improvements and bug fixes

* Saving to several file formats at once (e.g. `format = c("svg", "pdf", "png")`) is now considerably faster: the graph is drawn once and every other format is converted from that single render, rather than being redrawn from scratch per format. Saved files are unchanged.
* The minimum required version of rsvg is now 2.6.0. Earlier versions contain a resizing bug in `rsvg_svg()` that affects the rescaling `save_e61()` does when saving PNG and JPG files.
* theme61 installs a smaller set of packages: `gh` and `here` are no longer needed at all, and `magick` and `remotes` have moved to Suggests. `add_e61_logo()` now asks you to install `magick` if you don't have it; everything else works as before. Updating from the in-session prompt no longer installs Suggests either, so re-installing pulls down much less.
* Fixed `patchwork` being dropped from `Imports` by the dependency trim above, which broke every multi-panel `save_e61()`/`save_multi()` call (`patchwork` is still a hard, unguarded dependency there).

#### Deprecated features

* `set_base_size()` is now deprecated. Use `set_t61_options(list(theme61.base_size = ...))` instead.
* Removed the `theme61.max_discrete_colours`/`theme61.max_discrete_fills` options; the discrete palette limit of 12 is fixed and was never actually configurable via these. `set_t61_options()` now hard-errors if you try to set either.
* `theme_e61_spatial()` is now defunct. Use `theme_e61()` instead -- spatial styling has applied automatically on save/print for a full release cycle now.
* `save_e61()`'s `title`/`subtitle`/`footnotes`/`sources`, `ncol`/`nrow`/`align`/`axis`, and `pad_width`/`pad_height`/`outer_width`/`outer_height`/`height_adj`/`rel_heights`/`spacing_adj` arguments are now defunct, after a full release cycle of warning. Use `labs`, `layout`, and `spacing` instead.

# theme61 0.8.0

#### New and changed features

* Automatic plot labelling! `plot_label()` now has an algorithm that attempts to automatically position plot labels in sensible locations for most common geom types. See the documentation for more information: [plot_label()](https://e61-institute.github.io/theme61/reference/plot_label.html) or `?plot_label()`.
* New function to set session-wide options for the package: `set_t61_options()`. This allows you to alter various functionality in theme61, see the documentation for the options ([set_t61_options()](https://e61-institute.github.io/theme61/reference/set_t61_options.html) or `?set_t61_options()`). Some of the key new options include: `theme61.auto_theme` controls whether `ggplot()` automatically applies `theme_e61()`; and `theme61.iterate_mode`, which skips all of theme61's automatic styling and rendering, printing graphs with plain ggplot2 defaults.
* Multi-panel graphs can be returned as a plot object using a new `return_plot_obj` argument to `save_e61()`. This is useful for printing in the Plots pane, or for use in a Shiny app.
* Added a `build_up` argument to `save_e61()` for single-panel graphs. When `TRUE`, saves a sequence of files (`filename_1`, `filename_2`, ..., `filename_N`) that each reveal one more category/series/stack than the last, for stepping a chart across several PowerPoint slides.
* The arguments available in `save_e61()` became increasingly cluttered and complicated as new features were added. So ~30 top-level arguments have now been grouped into four named lists to make it easier to use.
* Y-axis text now defaults to left-aligned when the y-axis is categorical (e.g. horizontal bar charts), instead of hugging the axis line, so it lines up with the left-aligned plot title/subtitle/y-axis title.
* Use the new theme61 Claude skill for assistance in making graphs, `e61_chart_maker()` has been removed.

#### Performance improvements and bug fixes

* Graphs should save faster thanks to internal improvements to save_e61().
* Fixed an issue where `labs_e61` would leave whitespace above subtitles and y-axis titles when a plot had a subtitle but no title.
* Improvements to margins, text wrapping, whitespace and text overlap/clipping issues.
* Fixed issue with PDFs not rendering as previews in Viewer.

# theme61 0.7.1

09 Jan 2026

#### New features

* Plots now render like they would appear when saved in the Viewer pane when they are printed via `print()` (this happens automatically when a `ggplot()` object is run).
* Added `ytitle_wrap` argument to `labs_e61` so you can custom wrap the y-axis titles just like other graph titles.
* New, simpler approach to specifying which facets for labels to appear on using a new `panel` argument in `plot_label`.

#### Bug fixes

* theme61 now works with ggplot2 v4.0.0 and above (this will automatically update when you install the new package).
* `save_e61` no longer ignores custom aspect ratios.
* Maps made using `sf` functions now retain their aspect ratios rather than using the (bad) default aspect ratios designed for normal graphs.
* Package now loads even if there is no internet connection.
* Labels on facet graphs now respect ordered factor ordering rather than resetting them.
* A better default of labels showing on all facet panels if no facet is specified, rather than erroring out with an uninformative error message.
* Fix issue with secondary y-axis not appearing by default on certain graphs.
* Fix issue with facet panel spacing when axes do not appear on all panels.
* Fix error when using a transformed y-axis (e.g. `scale_y_continuous(trans = "log10")`), where the automatic aesthetic scaling would replace the transformed scale and compare its limits against the untransformed data.

# theme61 0.7.0

01 Jul 2025

* New graph design. No longer will we make graphs that people think were made by the RBA!

# theme61 0.6.4

24 Jun 2025

#### New features

* New function `set_format` that sets the default file format that graphs get saved as in the session.
* New custom geom `geom_pointbar` that combines `geom_point` and `geom_errorbar` in one.
* New colours for states and territories/capital cities, accessible via `scale_colour_e61_aus`/`scale_fill_e61_aus` or directly through the named vector `e61_aus_colours`.

#### Bug fixes

* Fixed an issue where ridgeline plots (and other plots with a discrete y-axis) could not be saved with `save_e61`.
* Fixed an issue where `save_e61` dropped map legends.
* Fixed an issue with footnote wrapping when `pad_width` was used.
* Minor documentation fixes.
* Minor backend adjustments to colour palette functions.

# theme61 0.6.3

12 Mar 2025

#### Improved functionality

* Add preview mode to `save_e61` that does not save the file but allows the graph to be viewed in the Viewer pane.
* The `chart_type` argument in `save_e61` has been updated so that it now allows you to adjust the aspect ratio of your chart in an easy and consistent way. There are three valid values `normal` (aspect ratio of 0.75), `wide` (aspect ratio of 0.5) and `square` (aspect ratio of 1). You can also supply a list of values to `save_e61` if you are saving multiple charts.
* Added the ability to save JPEGs. This functions in a similar way to how PNGs are currently saved - you can use the `res` argument in `save_e61` to adjust the resolution.

#### Bug fixes

* Fixed an issue where y-axis labels were not being aligned correctly when saving multiple plots at the same time. 
* Fixed an issue where where you could not use a * in footnotes (e.g. for describing the level of statistical significance) without `save_e61` interpreting this as a new footnote.
* Fixed legend positioning argument that broke due to ggplot2 3.5.0 release.
* Fixed issue with y-axis labels being out-of-position in certain graphs.
* Replaced an uninformative error message when y-axis limits did not include the full range of the data with a more informative error message.
* Fixed image rendering issues on the website (I think).


# theme61 0.6.2

11 Dec 2023

#### Improved functionality

* theme61 now requires an R version of at least 4.3.0.
* Improvements to `plot_label`: now supports facetted graphs! As well as other back-end changes.
* We now have new shades of e61 Blue Dark `#10485E`, Blue Light `#196F91`, Orange Dark `#ED7F00` and Orange Light `#FFC537`. These changes aim to improve the contrast between colours when they are all used in the same graph.
* Added an argument to `save_e61` to fix an issue with `y_top = FALSE` not positioning the y-axis label correctly.
* Change the default colour of single line graphs from black to e61 Teal Dark.
* Changed the ordering of when colours appear in the default e61 palette.

#### Bug fixes

* Fix a weird edge case with y-axis breaks relating to the oddities of floating point maths.
* theme61 would blow up uninformatively if you tried to supply graph data containing `NA` or `NaN`. Now catches this.
* Removed the dplyr dependency.

# theme61 0.6.1

09 Nov 2023

* Graphs will automatically appear in the Viewer pane when you save them with `save_e61()`.
* For users of base maps, there is now a new function `setup_stadia_maps()` to help you register an API key with Stadia Maps.
* Fixed changes to base maps that meant `e61_map()` no longer worked and renamed the function to `add_map_e61()`.
* `as.factor` no longer breaks variable detection in `scale_colour_e61()`.
* Fixed various issues with y-axis labels and scales.
* Workaround for a 401 Authentication error when loading the package.

# theme61 0.6.0

06 Nov 2023

#### Automatic graph sizing, and y-axis, colour and fill scaling

This is a major breaking change. Your code will be simpler but may need some changes to work.

Most of these improvements are applied when you save graphs using `save_e61()`, so will not show up in the preview.

* Graphs now automatically size themselves correctly. You no longer need to figure out the appropriate graph height.
* Titles, subtitles and footnotes will automatically scale to fit the panel width. 
* Y-axis will automatically set limits and breaks to aesthetic defaults. You can override this by providing your own limits using `scale_y_continuous_e61()`.
* Colour/fill will automatically apply the e61 Institute colour palette. This works if you define a fill/colour variable in your main `ggplot` call (e.g. `ggplot(data, aes(x, y, fill = fill))`). You no longer need to specify the number of colours when using `scale_colour/fill_e61()`.
* Added different default graph dimensions for micro notes, research notes and PowerPoint presentations. 

These features are implemented directly in `theme61::ggplot()`. This masks the `ggplot()` function in `ggplot2` so if you wish to call the original function you need to specify the namespace like so `ggplot2::ggplot()`. Make sure you load `theme61` *after* `ggplot2` otherwise the correct functions will not be called.

#### Other changes

* All functionality in `mplot_label()` has been combined into `plot_label()`. `mplot_label()` is now deprecated and users need to rename all instances of `mplot_label()` to `plot_label()`.
* `mpanel_e61()` has been retired as functionality has been incorporated directly into `save_e61()`.
* Added console messages to help correct common graphing mistakes.
* Changed default font of graphs to 'PT Sans' to be consistent with research note font.
* Allow rotation of labels in `plot_label()`.
* Renamed `add_zeroline()` to `add_baseline()`, and `e61_palette()` to `palette_e61()`.

#### Bug fixes and documentation updates

* Fixed an issue where saving graphs with no title created whitespace above the plot.
* Documentation and vignettes rewritten to reflect new workflow.

# theme61 0.5.0

26 May 2023

#### New functions

* Added new functions `set_open_graph()` and `unset_open_graph()` which toggle an option to automatically open a graph that has been created with `save_e61()`. Helpful for your workflow when perfecting graphs.

#### New/changed functionality

* Add ability to save graphs in multiple formats at once using the new `format` argument in `save_e61()`.
* Add support in `save_e61()` for saving images in PDF format for compatibility with LaTeX.
* Change the default alignment of plot labels to left-align and added a new argument `hjust` to `plot_label()`/`mplot_label()` to change the alignment.
* Added the ability to add a white box around text in `plot_label()`/`mplot_label()` through `geom` argument.
* Added `y` argument to `add_zeroline()` so you can put your zero line at values other than zero.
* Added argument to `scale_x_continuous_e61()` to add back the first/last labels that are removed by default.
* Slightly increased default size of plot labels to match size of axis text.
* `save_e61()` now notifies you if you forget to use the theme and scale functions.

#### Bug fixes

* Permanent fix to the bug that was temporarily bodged in v0.4.1. Now dates can be entered as strings in `plot_label()` and they will be converted to dates.
* Fixed a bug in `scale_y_continuous_e61()` that made `y_top` stop working and also refactored the code.
* Fixed a bug in `mplot_label()` that didn't allow a vector of colours to be supplied.

#### Documentation updates

* Added a new Graph standards vignette based on masterclass materials.
* Added more documentation explaining how `rel_heights` in `mpanel_e61()` works.
* Miscellaneous minor documentation improvements.

# theme61 0.4.1

08 May 2023

* Fixing a bug in `plot_label()` caused by a change to how dates work in R 4.3.0. Unfortunately this means that if you are using dates to specify x-axis locations for your label, you will now have to surround them in `as.Date()` rather than just putting in the plain string.

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

# theme61 (development version)

#### New features

* `plot_label()` text is now automatically repositioned to a nearby, non-overlapping spot on the chart when you call `save_e61()`, instead of staying exactly where you specified. This applies to single-panel charts where the label's colour matches a `geom_line()`/`geom_point()`/`geom_col()`/`geom_bar()`/`geom_area()`/`geom_pointbar()` series and the label isn't rotated; you can set `plot_label(..., auto_position = FALSE)` to always use the exact position you supply, in which case `x`/`y` are required (closes #159). Rotated text (`angle != 0`) can't be auto-positioned either, so `x`/`y` are required for that too. Column/bar series are matched on `fill` rather than `colour`. Area series are also matched on `fill`; where there's room, the label is placed fully inside the band in a colour that contrasts with the fill (white or black), falling back to the usual outside placement in the fill's own colour where the band is too narrow. `geom_pointbar()` series are matched on `colour`, and the buffer is measured against the full error-bar extent, not just the point.
* `x`/`y` are now optional in `plot_label()` when `auto_position = TRUE` (the default) -- if you don't supply them, the label falls back through: a good spot found by the placement algorithm, then any empty space on the chart at all, then the centre of the panel as a last resort so the label always stays visible rather than vanishing. If you do supply `x`/`y`, that position is now always used exactly as given -- the placement algorithm never runs for that label, so it can't be second-guessed by something the algorithm merely scores "better".
* Set `plot_label(..., print_position = TRUE)` to have the chart's final label positions printed to the console (as copy-pasteable `plot_label()` arguments) whenever the plot is displayed, or pass `save_e61(..., print_label_positions = TRUE)` to do the same when saving. Handy for pinning the auto-positioned spots, or hand-tweaking just one or two.
* Automatically-positioned `plot_label()` text now avoids sitting on top of a y-axis gridline where possible, only touching one if there's genuinely no other spot available for it.
* Automatic positioning now also works on multi-panel `save_e61()` charts (e.g. `save_e61(plot1, plot2, ...)`), positioning each panel's labels independently against its own data. Previously these labels were silently dropped unless you supplied `x`/`y` yourself.
* Auto-positioning is slow relative to the rest of a chart's render, since it needs to actually render the plot to know where the content is. The automatic Viewer preview shown when you `print()` a chart now uses a cheap, render-free approximate placement instead (near the label's own series, not checked for overlap with other content), so iterating on a chart in the Viewer no longer pays that cost on every print -- `save_e61()` itself always resolves the real, optimised position regardless. You can opt into the same fast placement yourself via `save_e61(..., fast_labels = TRUE)` (e.g. for a quick preview render), and `plot_label(..., x =, y =)` positions are unaffected either way.
* Added a `theme61.auto_theme` option (default `TRUE`) that controls whether `ggplot()` automatically applies `theme_e61()`. Set it to `FALSE` via `set_t61_options(list(theme61.auto_theme = FALSE))` to apply your own theme instead.
* Added a `theme61.iterate_mode` option (default `FALSE`) for fast data-analysis iteration. When `TRUE`, `ggplot()`/`print()` skip all of theme61's automatic styling (theme, scales, facet spacing) and the Viewer pane preview render entirely, so graphs print to the Plots pane with plain ggplot2 defaults as quickly as possible. The masked `ggsave()`, `labs()`, `facet_wrap()` and `facet_grid()` also stop redirecting to their theme61 equivalents and pass straight through to the underlying ggplot2 function instead. theme61 functions you call explicitly (e.g. `scale_colour_e61()`, `labs_e61()`) still apply as normal - except that `labs_e61()` itself skips its HTML/markdown subtitle styling in this mode (since it relies on `theme_e61()`'s `ggtext::element_markdown()`, which isn't applied automatically), so the subtitle and y-axis title stay plain text instead of showing literal `<span>`/`<br>` tags. Enable it with `set_t61_options(list(theme61.iterate_mode = TRUE))`.
* Renamed the `theme61.open_e61_graph` option to `theme61.open_in_browser` to better reflect what it does (controls whether `save_e61()` also opens the saved graph in the browser, alongside the Viewer pane). `set_open_graph_browser()`/`unset_open_graph_browser()` are unaffected; only users setting the option directly via `options()`/`set_t61_options()` need to update the name.
* Added `THEME61_DISABLE_GEOM_DEFAULTS` and `THEME61_DISABLE_VERSION_CHECK` environment variables (both enabled by default) to opt out of theme61 overwriting ggplot2's session-wide geom colour/fill defaults, and the startup check against GitHub for a newer release, respectively. Unlike the `theme61.*` options, these run once at load time, so they must be set (to `"1"`) before `library(theme61)` is called - see `?set_t61_options` for details. Joins the existing `THEME61_DISABLE_FONT_DOWNLOAD`, which is now documented in the same place.
* `print()` once again prefers focusing the Viewer pane by default (best-effort), reverting #302. Now that `theme61.iterate_mode` exists as a dedicated fast/Plots-pane-only mode, there's no need for the softer opt-in `theme61.focus_viewer_on_print` option (removed) - it's back to unconditional, like it was before #302.
* Added a `return_plot_obj` argument to `save_e61()` for multi-panel graphs (2 or more plots). When `TRUE`, `save_e61()` skips saving entirely and returns the composed multi-panel plot object instead of writing it to disk - e.g. to print it in the Plots pane, or use it in a Shiny app (closes #216). No `filename` is required in this mode. Not supported for single-panel graphs, since you can already just print the ggplot object directly. Note the returned object's layout (text sizes, panel spacing) is computed for a fixed target size (`dim`, or the same defaults `save_e61()` would otherwise use), so it won't reflow if you resize the device afterwards - the same way a saved image wouldn't.
* Added a `theme61.auto_label` option (default `TRUE`) that controls whether `plot_label()`'s automatic positioning is available at all. Set it to `FALSE` via `set_t61_options(list(theme61.auto_label = FALSE))` to restore the pre-auto-positioning behaviour: `x`/`y` are always required (`plot_label()` errors immediately if you omit them, the same as `auto_position = FALSE`), and no auto-positioning work is attempted, so there's no performance cost from the feature at all.
* `plot_label()`'s `label` and `colour` now default to a `scale_colour_manual()`/`scale_fill_manual()` on the plot, if there is one, instead of always assuming the e61 palette. Omit `label` entirely to label every series with its own data value (in the scale's resolved order, e.g. respecting `reverse = TRUE`); supply `label` yourself and its colours are matched positionally against the scale's levels instead (assumes `label` is written in the same order). An explicit `colour` still always wins outright, exactly as before.

#### Performance

* `save_e61()`/`print()` no longer re-render the same plot from scratch several times over. `update_scales()` now builds the plot once and reuses that build for its internal y-variable, secondary-axis and y-min/max checks (previously up to 3 separate builds), and `save_single()` no longer builds the plot just to check for `coord_flip()` (reads the coord class off the plot object instead) or builds it unconditionally to count facet panels (now only when facets are actually present). No change in output.

#### Bug fixes

* Fixed multi-panel charts (`save_e61()` with more than one plot) rendering with overlapping panel titles and extra whitespace on the left. The panel margin used to measure title/subtitle/caption wrap widths no longer disagreed with the (much smaller) margin theme_e61() actually applies, which was throwing off the wrapping calculations.
* Fixed the shared title/subtitle/footnote text on multi-panel charts sometimes clipping past the right edge of the graph. These weren't rendered in the chart's font family, so their wrap width was measured against the wrong font.
* Renamed the undocumented `quiet_wrap` option to `quiet_mask` (it controls the messages shown when the masked `ggsave()`/`labs()` pass your call through to `save_e61()`/`labs_e61()`). It's intentionally not part of `set_t61_options()`/the `theme61.*` namespace - it only suppresses a message rather than controlling real functionality, so it stays a plain `options(quiet_mask = TRUE)` setting rather than cluttering the documented options list.
* Fixed an issue where `labs_e61` would leave whitespace above the subtitle when a plot had a subtitle but no title (the empty title was still reserving vertical space).
* Fixed the same issue for a y-axis title with no subtitle (`labs_e61(y = ...)` with `y_top = TRUE`, the default) - it was being prefixed with an empty, invisible subtitle line that still reserved space above it.
* Reduced the outer `plot.margin` in `theme_e61()` so graphs have less dead space above the title, below the footnotes, and to the left/right of the axes.
* Fixed text wrapping for titles, subtitles and footnotes (especially on multi-panel graphs with long footnotes) undershooting or overflowing the available width. Text width is now measured using the actual font that will be rendered instead of an approximate built-in font table, which was under-measuring text by 10-15% and could let wrapped lines run past the edge of the graph.
* Fixed the text-wrapping algorithm wrapping lines earlier than necessary: it was charging every candidate line for a trailing space that is never actually rendered after the line's last word, which could reject a word that would otherwise have fit.
* Fixed the multi-panel title/subtitle/caption width budget being ~0.4cm narrower than it needed to be: `patchwork` sizes that row using a throwaway plot with its own default margin and places the text outside of it, reserving horizontal space beyond what was already accounted for. That margin is now explicitly zeroed out so the full available width gets used.
* Fixed the per-panel title/subtitle/footnote width budget in multi-panel graphs (`labs_e61()` applied to an individual panel) being narrower than that panel's true rendered width: it accounted for the panel's content and axes but not its own left/right margin. Also removed an unexplained 1% haircut applied to the width budget for single-panel graphs.
* `save_e61` now always previews an SVG version of the graph in the Viewer pane, even when saving to other formats such as PDF or PNG. This fixes an error where RStudio's Viewer pane could fail to open non-SVG formats (e.g. a "chrome-extension" popup error when saving PDFs).
* Y-axis text now defaults to left-aligned when the y-axis is categorical (e.g. horizontal bar charts), instead of hugging the axis line, so it lines up with the left-aligned plot title/subtitle/y-axis title. This can still be overridden with `theme(axis.text.y = element_text(hjust = ...))`.
* Automatically-positioned `plot_label()` text could occasionally spill off the edge of the chart; the bounds check now correctly measures against the plotting panel rather than the full raster canvas (which also includes the axis-title/tick-label margin).
* Automatic label positioning now works on `coord_flip()` charts for `geom_line()`/`geom_point()`/`geom_col()`/`geom_bar()` series, instead of computing a position in the wrong coordinate space and dropping the label off the chart entirely. `geom_area()`/`geom_pointbar()` series aren't supported under `coord_flip()` yet and fall back to the position you supply.
* Resolving an auto-positioned `plot_label()` (e.g. via `save_e61()`, or automatically when a plot is printed) could leave the *original* plot object mutated with that resolved position too, since a plot layer's data is a data.table and mutates by reference. This meant printing a plot and then saving it separately could re-run positioning against a stale, already-resolved position from the print instead of a clean one.
* Automatically-positioned `plot_label()` text could be placed as if the plotting panel were a different size than it actually renders at, occasionally landing on top of chart content near an edge instead of nearby whitespace. This happened when an axis needed dynamic space for its labels (e.g. many closely-spaced/dodged x-axis categories combined with a title) -- the panel size was previously predicted from the chart's layout units without a device to resolve them against, which silently mismeasured that case. The panel size is now measured directly from an actual render instead.
* Fixed `save_e61()` erroring with "Supplied limits are outside the data's range" when auto-positioning fell back to placing a `geom_area()` label outside a band that was too narrow to fit it, on a chart using `coord_cartesian(ylim = ...)` to show more vertical room than the data's own axis limits. Candidate positions are now kept within the axis's real limits, not just the wider, currently-visible range.
* Fixed `save_e61(plot1, plot2, ..., preview = TRUE)` (multi-panel, no `filename`) silently dropping the first plot from the saved/previewed graph. `filename` is `save_e61()`'s first argument, so passing plots positionally without also naming `filename` matched the first plot to it instead of to the plot list.

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

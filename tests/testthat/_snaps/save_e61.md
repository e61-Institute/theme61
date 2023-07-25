# Test advisory messages

    Code
      suppressWarnings(save_e61(withr::local_tempfile(fileext = ".svg"), gg, height = 1))
    Message <cliMessage>
      
      Please fix the following issues with your graph
      ! Use 'theme_e61()' in your ggplot code to ensure the e61 theme is applied.
      ! Use 'scale_x/y_continuous_e61()' in your ggplot code to ensure the graph axes render correctly.
      ! Your y-axis label is missing. Please provide the units of the axis for the reader.

---

    Code
      suppressWarnings(save_e61(withr::local_tempfile(fileext = ".svg"), gg, height = 1))
    Message <cliMessage>
      
      Please fix the following issues with your graph
      ! Use 'theme_e61()' in your ggplot code to ensure the e61 theme is applied.
      ! Use 'scale_x/y_continuous_e61()' in your ggplot code to ensure the graph axes render correctly.
      ! Use 'scale_colour/fill_e61()' in your ggplot code to ensure the e61 colour palette is used.

---

    Code
      suppressWarnings(save_e61(withr::local_tempfile(fileext = ".svg"), gg, height = 1))
    Message <cliMessage>
      
      Please fix the following issues with your graph
      ! Your y-axis label is missing. Please provide the units of the axis for the reader.

---

    Code
      suppressWarnings(save_e61(withr::local_tempfile(fileext = ".svg"), gg, height = 1))
    Message <cliMessage>
      
      Please fix the following issues with your graph
      ! Your y-axis label is too long. Consider if the information needed to interpret the graph is already in the title, and only specify the units in the y-axis label.


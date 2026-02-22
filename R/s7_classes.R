# Theme spec class (S7) used by ggplot2 v4 update_ggplot() dispatch
e61_theme_spec_class <- S7::new_class(
  "e61_theme_spec",
  properties = list(
    args = S7::class_list,
    variant = S7::class_character,
    version = S7::class_integer
  )
)

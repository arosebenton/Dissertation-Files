
# Custom theme for spatial visualizations
map_theme <- function(title_size = NULL, xaxis_size = NULL, yaxis_size = NULL, 
                      strip_size = NULL, caption.hjust = 1, caption.vjust = 0, 
                      x_axis_face = NULL, y_axis_face = NULL, transparent = FALSE, 
                      show.axis = TRUE, ...) {
  .theme <- theme_void() + theme(
    # Specify the default settings for the plot title
    plot.title = element_text(
      size = 22,
      face = "bold",
      family = "serif"
    ),
    # Specify the default settings for caption text
    plot.caption = element_text(
      size = 12,
      family = "serif",
      hjust = caption.hjust,
      vjust = caption.vjust
    ),
    # Specify the default settings for subtitle text
    plot.subtitle = element_text(
      size = 16,
      family = "serif"
    ),
    # Specify the default settings for legend titles
    legend.title = element_text(
      size = 16,
      face = "bold",
      family = "serif"
    ),
    # Specify the default settings for legend text
    legend.text = element_text(
      size = 14,
      family = "serif"
    ),
    # Font Settings for Face Strips
    strip.text = element_text(
      size = 14,
      face = "bold",
      family = "serif"
    ),
    # Additional Settings Passed to theme()
    ...
  )
  # Plot Transparency
  if (transparent == TRUE) {
    .theme <- .theme + theme(
      panel.background = element_rect(fill = "transparent", colour = NA),
      #
      plot.background = element_rect(fill = "transparent", colour = NA),
      #
      legend.background = element_rect(fill = "transparent", colour = NA),
      #
      legend.key = element_rect(fill = "transparent", colour = NA)
    )
  }
  # Disable X and Y Axis
  if (show.axis == FALSE) {
    .theme <- .theme + theme(
      # Specify the default settings for axis titles
      axis.title = element_blank(),
      # Specify the default settings for axis text
      axis.text = element_blank()
    )
  } else if (show.axis == TRUE) {
    .theme <- .theme + theme(
      # Specify the default settings for axis titles
      axis.title = element_text(
        size = title_size,
        face = "bold",
        family = "serif"
      ),
      # Specify the default settings specific to the x axis title
      axis.title.y = element_text(
        size = yaxis_size, 
        face = "bold", 
        margin = margin(r = 10, l = -10)
      ),
      # Specify the default settings specific to the y axis title
      axis.title.x = element_text(
        size = xaxis_size, 
        face = "bold", 
        margin = margin(t = 10, b = -10)
      ),
      # Specify the default settings for x axis text
      axis.text.x = element_text(
        size = 14,
        family = "serif",
        face = x_axis_face
      ),
      # Specify the default settings for y axis text
      axis.text.y = element_text(
        size = 14,
        family = "serif",
        face = y_axis_face
      )
    )
  }
  return(.theme)
}

tu_theme <- function() {
  theme_minimal() +
    theme(axis.text.y = element_text(size = 12), 
          axis.title.y = element_text(size = 12, face = 'bold'),
          axis.title.x = element_text(size = 12, face = 'bold'),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1, size = 11),
          plot.title = element_text(size = 20, face = "bold"),
          plot.subtitle = element_text(size = 11, face = "italic", margin=margin(0,0,5,0)),
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 12, face = 'bold'),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
}
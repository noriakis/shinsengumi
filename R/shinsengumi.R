
## https://meghan.rbind.io/blog/2022-10-11-creating-custom-color-palettes-with-ggplot2/

shinsengumi_color <- function(...) {
  
  shinsengumi_colors <- c(
    `darkasagi`     = "#5B83AD",
    `lightasagi`   = "#7ACEF0",
    `wikiasagi` = "#00A5BF",
    `red`  =  "#FF0000",
    `gold` = "#FABC00",
    `white` = "#F2F3F8",
    `gray`     = "#818BA6"
  )
  
  cols <- c(...)
  
  if (is.null(cols))
    return (shinsengumi_colors)
  
  shinsengumi_colors[cols]
}

shinsengumi_palette <- function(palette = "darkasagi", ...) {
  
  shinsengumi_palettes <- list(
    `darkasagi` = shinsengumi_color("darkasagi", "white", "gray"),
    `lightasagi` = shinsengumi_color("lightasagi", "white", "gray"),
    `wikiasagi` = shinsengumi_color("wikiasagi", "white", "gray"),
    `red` = shinsengumi_color("red", "gold", "gray", "white")
  )
  
  shinsengumi_palettes[[palette]]
  
}

palette_gen <- function(palette = "darkasagi", direction = 1) {
  
  function(n) {
    
    if (n > length(shinsengumi_palette(palette)))
      warning("Not enough colors in this palette!")
    
    else {
      
      all_colors <- shinsengumi_palette(palette)
      
      all_colors <- unname(unlist(all_colors))
      
      all_colors <- if (direction >= 0) all_colors else rev(all_colors)
      
      color_list <- all_colors[1:n]
      
    }
  }
}


scales::show_col(shinsengumi_palette("darkasagi"), cex_label = 2)


palette_gen <- function(palette = "darkasagi", direction = 1) {
  
  function(n) {
    
    if (n > length(shinsengumi_palette(palette)))
      warning("Not enough colors in this palette!")
    
    else {
      
      all_colors <- shinsengumi_palette(palette)
      
      all_colors <- unname(unlist(all_colors))
      
      all_colors <- if (direction >= 0) all_colors else rev(all_colors)
      
      color_list <- all_colors[1:n]
      
    }
  }
}


palette_gen_c <- function(palette = "darkasagi", direction = 1, ...) {
  
  pal <- shinsengumi_palette(palette)
  
  pal <- if (direction >= 0) pal else rev(pal)
  
  colorRampPalette(pal, ...)
  
}

scale_fill_shinsengumi <- function(palette = "darkasagi", direction = 1, ...) {
  
  ggplot2::discrete_scale(
    "fill", "shinsengumi",
    palette_gen(palette, direction),
    ...
  )
}

scale_colour_shinsengumi <- function(palette = "darkasagi", direction = 1, ...) {
  
  ggplot2::discrete_scale(
    "colour", "shinsengumi",
    palette_gen(palette, direction),
    ...
  )
}

scale_color_shinsengumi <- scale_colour_shinsengumi

scale_color_shinsengumi_c <- function(palette = "darkasagi", direction = 1, ...) {
  
  pal <- palette_gen_c(palette = palette, direction = direction)
  
  scale_color_gradientn(colors = pal(256), ...)
  
}

library(dplyr);library(ggplot2)
library(palmerpenguins)

penguins %>% 
  ggplot(aes(x=bill_length_mm , y=flipper_length_mm, color=species)) +
  geom_point() + 
  scale_color_shinsengumi("lightasagi") + 
  theme_minimal()


  geom_bar(stat = "identity") +
  scale_fill_shinsengumi("lightasagi") +
  labs(title = "The count of each species in the palmerpenguins data set") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_linedraw() +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        panel.grid.major.x = element_blank())

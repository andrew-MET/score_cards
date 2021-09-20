library(ggplot2)
library(dplyr)
library(tibble)
library(purrr)
library(forcats)
library(cowplot)

scores <- tibble(
  param = c(
    "T2m",
    "T850",
    "T700",
    "T500",
    "Q2m",
    "Q850",
    "Q700",
    "Q500",
    "S10m",
    "S850",
    "S700",
    "S500",
    "AccPcp1h",
    "AccPcp3h",
    "AccPcp6h",
    "AccPcp12h",
    "MSLP",
    "Z850",
    "Z700",
    "Z500"
  )
)

lead_time <- seq(0, 48, 3)
scores    <- map_dfr(lead_time, ~ mutate(scores, lead_time = .x))

areas  <- c("coastal", "inland", "mountain")
scores <- map_dfr(areas, ~ mutate(scores, area = .x))

classify <- function(vec, val, n) {
  breaks        <- quantile(abs(vec), seq(0, 1, length.out = n + 1))
  breaks[1]     <- 0
  breaks[n + 1] <- Inf
  class_found   <- FALSE
  class_label   <- 0
  while(!class_found) {
    class_label <- class_label + 1
    class_found <- abs(val) >= breaks[class_label] && abs(val) < breaks[class_label + 1] 
  }
  as.integer(class_label)
}

scores <- mutate(
  scores, 
  diff    = rnorm(nrow(scores)),
  sig     = map_int(diff, ~ classify(diff, .x, 3)),
  diff    = case_when(
    param == "AccPcp12h" & lead_time < 12 ~ NA_real_,
    param == "AccPcp6h" & lead_time < 6 ~ NA_real_,
    TRUE                                   ~ diff
  ),
  area    = fct_inorder(area),
  param   = fct_inorder(param),
  sig     = case_when(
    is.na(diff) ~ NA_integer_, 
    TRUE        ~ sig
  ),
  better  = case_when(
    diff == 0 ~ "no",
    diff >  0 ~ "better",
    diff < 0  ~ "worse",
    TRUE      ~ NA_character_
  )
)


bar_card <- function(scores) {
  ggplot(
    scores, 
    aes(
      x = factor(lead_time), 
      y = diff, 
      fill = better, 
      colour = better, 
      alpha = factor(sig)
    )
  ) + 
    geom_col() + 
    facet_wrap(
      vars(area, param), 
      ncol     = 4, 
      labeller = label_bquote(italic(.(toupper(as.character(area))))*":"~bold(.(as.character(param))))
    ) + 
    scale_shape_manual(values = c(24, 25)) + 
    scale_size_manual(values = c(0.75, 1.25, 1.75)) + 
    scale_colour_manual(values = c("#CC4356", "#5643CC")) +
    scale_fill_manual(values = c("#CC4356", "#5643CC")) +
    scale_alpha_manual(values = c(0, 0.5, 1)) +
    theme_minimal() + 
    theme(
      legend.position = "none", 
      panel.grid.major = element_blank()
    ) +
    labs (x = NULL, y = NULL)
}

point_card <- function(scores, ncol = 1) {
  ggplot(
    scores, aes(
      x = factor(lead_time),
      y = fct_rev(param),
      fill = better,
      colour = better,
      size = factor(sig),
      shape = better
    )
  ) +
    geom_point() +
    facet_wrap(vars(area), ncol = ncol) +
    scale_shape_manual(values = c(24, 25)) +
    scale_size_manual(values = c(0.5, 1.5, 2.75)) +
    scale_colour_manual(values = c("#5643CC", "#CC4356")) +
    scale_fill_manual(values = c("#5643CC", "#CC4356")) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.major = element_blank()
    ) +
    labs (x = "Lead Time", y = "Parameter")
  
}
library(tidyverse)
library(rlang)
library(cowplot)

prepare_data <- function(data, var) {
  var = enquo(var)
  
  data %>%
  group_by_at(., vars(!!var)) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  rownames_to_column(var = "id") %>%
  gather(key = "key", value = "value", -id, -n) %>%
  mutate(key = factor(key),
         value_f = factor(value),
         id = as.numeric(as.character(id))) %>%
  ungroup() %>%
  dplyr::filter(id <= 10) %>%
  add_count(key, wt = (n * value), name = "nn") %>%
  filter(nn > 0) %>%
  mutate(key = fct_drop(key),
         key = fct_reorder(key, .x = n * value, .fun = sum),
         y = as.numeric(key))
}

make_vertical_bar_plot <- function(data) {
  data %>% 
    distinct(id, n) %>% {
        ggplot(data = .) +
          geom_bar(aes(x = id, y = n), 
                   stat='identity') +
          geom_text(aes(x = id, y = n, 
                        label = scales::number_format(big.mark = ",")(n)),
                    nudge_y = 20) +
          scale_y_continuous(name = "Intersection Size",
                             expand = c(0,0),
                             limits = c(0,max(data$n * 1.05))) +
          scale_x_continuous(name = NULL,
                             labels = NULL,
                             limits = c(0.5, (max(data$id)) + 0.5),
                             expand = c(0,0),
                             breaks = 1:max(data$id)) +
          theme_cowplot() +
          theme(plot.margin = unit(c(5.5,0,0,0), units = "points"))
    }
}

make_horizontal_bar_plot <- function(data) {
  data %>% 
    count(key, wt = -n * value) %>% {
      ggplot(data = .) +
        geom_bar(aes(x = key, y = n), 
                 stat = "identity", 
                 position = "identity") +
        scale_y_continuous(name = "Set Size",
                           labels = function(x) -x,
                           expand = c(0,0)) +
        scale_x_discrete(name = NULL,
                         labels = NULL,
                         expand = c(0,0)) +
        coord_flip() +
        theme_cowplot() +
        theme(axis.line.y = element_blank(),
              axis.ticks.y = element_blank(),
          plot.margin = unit(c(5.5,0,0,0), units = "points"))
    }

}
 
make_matrix_plot <- function(data) {
  ggplot(data = data) +
    geom_point(
      aes(x = id,
          y = y, 
          color = value_f,
          alpha = (0.5 + value)), 
      shape = 16,
      size = 3) + 
    geom_line(
      aes(group = interaction(value_f, id), 
          x = id, 
          y = y, 
          color = value_f,
          alpha = 1.5 * value),
      size = 1) + 
    geom_rect(
      data = tibble(ymin = unique(data$y[which(data$y %% 2 == 1)] - 0.5),
                    ymax = unique(data$y[which(data$y %% 2 == 1)] + 0.5),
                    xmin = 0.5,
                    xmax = length(unique(data$id)) + 0.5),
      aes(xmin = xmin, xmax = xmax,
          ymin = ymin, ymax = ymax,
          fill = "black", 
          alpha = 0.1)) +
    scale_color_manual(name = NULL, 
                       palette = function(n) rep("black",n)) +
    scale_fill_identity() +
    scale_x_continuous(name = "   ",
                       labels = NULL,
                       expand = c(0,0)) +
    scale_y_continuous(name = NULL,
                       breaks = 1:length(levels(data$key)),
                       labels = levels(data$key),
                       expand = c(0,0)) +
    scale_alpha_continuous(name = NULL, labels = NULL, range = c(0, 1)) +
    theme_cowplot() +
    theme(axis.ticks = element_blank(),
          legend.position = "none",
          axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)),
          plot.margin = unit(c(0,5.5,0,0), units = "points"))
  }

make_upset_plot <- function(data, ...) {
  prepare_data(movies, ...) %>% {
    plot_grid(
      NULL, 
      make_vertical_bar_plot(.), 
      make_horizontal_bar_plot(.), 
      make_matrix_plot(.), 
      rel_widths = c(1,2),
      align = "hv",
      ncol = 2)
  }
}

make_upset_plot(movies, Action:Western)
library(rlang)
library(tidyverse)
library(cowplot)


#' Title
#'
#' @param data
#' @param vars
#' @param n_intersections
#' @param show_empty_intersections
#' @param intersection_order
#' @param set_order
#'
#' @return
#' @export
#'
#' @examples
prepare_data <- function(data, set_vars, intersection_group_var,
                         is_summarised = FALSE, n_intersections = 10,
                         show_empty_sets = FALSE,
                         intersection_order = c("original", "descending"),
                         set_order = c("original", "descending"),
                         intersection_colour_fn = function(x) "black") {
  # Quote arguments -----
  set_vars <- enquo(set_vars)
  intersection_group_var <- enquo(intersection_group_var)

  # Helper functions -----
  order_intersections <- function(df, intersection_order) {
    switch(intersection_order[1],
      original = return(df),
      descending = return(arrange(df, desc(intersection_count))),
      stop("Argument 'intersection_order' must have value in c('original', 'descending'): ", intersection_order)
    )
  }

  remove_empty_sets <- function(df, show_empty_sets) {
    if(show_empty_sets) {
      df
    } else {
      filter(df, set_count > 0) %>%
        mutate(set_name = fct_drop(set_name))
      }
  }

  order_sets <- function(df, set_order) {
    switch(set_order[1],
      original = return(df),
      descending = return(mutate(df, set_name = fct_reorder(set_name, .x = intersection_count * is_member, .fun = sum))),
      stop("Argument 'set_order' must have value in c('original', 'descending'): ", set_order)
    )
  }

  add_intersection_colour <- function(df) {
    df %>%
      group_by(intersection_id) %>%
      mutate(intersection_colour = rep(intersection_colour_fn(set_name[as.logical(is_member)]) ,length(intersection_id)),
             intersection_colour = factor(as.character(intersection_colour))) %>%
      ungroup()
  }

  # Summarise if not already ----
  if(!is_summarised) {
    summarised_data <- data %>%
      group_by_at(., vars(!!intersection_group_var, !!set_vars)) %>%
      summarise(intersection_count = n()) %>%
      ungroup() %>%
      mutate(intersection_id = apply(select_at(., vars(!!set_vars)), 1, function(x) sum(x * 2^(0:(length(x)-1))) )) %>%
      ungroup()
  } else {
    summarised_data <- data %>%
      mutate(intersection_id = apply(select_at(., vars(!!set_vars)), 1, function(x) sum(x * 2^(0:(length(x)-1))) ))
  }

  # Main body -----
  summarised_data %>%
    order_intersections(., intersection_order) %>%
    gather(key = "set_name", value = "is_member", -intersection_id, -!!intersection_group_var, -intersection_count) %>%
    mutate(set_name = factor(set_name),
           is_member_fct = factor(is_member),
           intersection_id = as.numeric(as.character(intersection_id))) %>%
    dplyr::filter(intersection_id <= n_intersections) %>%
    add_count(set_name, wt = (intersection_count * is_member), name = "set_count") %>%
    remove_empty_sets(., show_empty_sets) %>%
    add_intersection_colour() %>%
    order_sets(., set_order) %>%
    mutate(set_number = as.numeric(set_name)) %>%
    ID()
}

#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
make_vertical_bar_plot <- function(data, intersection_group_var, ...) {
  intersection_group_var <- enquo(intersection_group_var)

  data %>%
    distinct(intersection_id, intersection_count, intersection_group_var) %>% {
        ggplot(data = .) +
          geom_bar(aes(x = intersection_id, y = intersection_count, fill = !!intersection_group_var),
                   stat='identity') +
          geom_text(aes(x = intersection_id, y = intersection_count,
                        label = scales::number_format(big.mark = ",")(intersection_count)),
                    nudge_y = (max(data$intersection_count))/40) +
          scale_y_continuous(name = "Intersection Size",
                             expand = c(0,0),
                             limits = c(0,max(data$intersection_count * 1.05))) +
          scale_x_continuous(name = NULL,
                             labels = NULL,
                             limits = c(0.5, (max(data$intersection_id)) + 0.5),
                             expand = c(0,0),
                             breaks = 1:max(data$intersection_id)) +
          theme_cowplot() +
          theme(plot.margin = unit(c(5.5,0,0,0), units = "points"))
    }
}

#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
make_horizontal_bar_plot <- function(data) {
  ggplot(data = data) +
    geom_bar(aes(x = set_name, y = -set_count),
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

#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
make_matrix_plot <- function(data, colour_palette = viridis::viridis_pal()) {

  ggplot(data = data,
         aes(x = intersection_id,
             y = set_number)) +
    geom_point(
      aes(colour = if_else(is_member == 1, intersection_colour %>% as.character(), is_member_fct %>% as.character()),
          alpha = (0.5 + is_member)),
      shape = 16,
      size = 3) +
    geom_line(
      aes(group = interaction(is_member_fct, intersection_id),
          alpha = 1.5 * is_member,
          colour = if_else(is_member == 1, intersection_colour %>% as.character(), is_member_fct %>% as.character())),
      size = 1) +
    geom_rect(
      data = tibble(ymin = unique(data$set_number[which(data$set_number %% 2 == 1)] - 0.5),
                    ymax = unique(data$set_number[which(data$set_number %% 2 == 1)] + 0.5),
                    xmin = 0.5,
                    xmax = length(unique(data$intersection_id)) + 0.5),
      aes(xmin = xmin, xmax = xmax,
          ymin = ymin, ymax = ymax,
          # override the defaults that don't make sense here
          x = NULL, y = NULL, color = NULL,
          fill = "black",
          alpha = 0.1)) +
    scale_color_manual(name = NULL,
                       palette = colour_palette) +
    scale_fill_identity() +
    scale_x_continuous(name = "   ",
                       labels = NULL,
                       expand = c(0,0)) +
    scale_y_continuous(name = NULL,
                       breaks = 1:length(levels(data$set_name)),
                       labels = levels(data$set_name),
                       limits = c(0.5, length(levels(data$set_name)) + 0.5),
                       expand = c(0,0)) +
    scale_alpha_continuous(name = NULL, labels = NULL, range = c(0, 1)) +
    theme_cowplot() +
    theme(axis.ticks = element_blank(),
          legend.position = "none",
          axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)),
          plot.margin = unit(c(0,5.5,0,0), units = "points"))
  }

#' Title
#'
#' @param data
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
make_upset_plot <- function(data, ...) {
  prepare_data(data, ...) %>% {
    plot_grid(
      NULL,
      make_vertical_bar_plot(., ...),
      make_horizontal_bar_plot(.),
      make_matrix_plot(.),
      rel_widths = c(1,2),
      align = "hv",
      ncol = 2)
  }
}

# Testing -----

movies <- read_delim("inst/extdata/movies.csv", ";")

make_upset_plot(movies, Action:Western, is_summarised = FALSE,
                n_intersections = 20,
                show_empty_sets = FALSE,
                intersection_order = "descending",
                set_order = "descending",
                intersection_colour_fn = function(set_vector) length(set_vector)
                )
movies %>%
  mutate(decade = floor(ReleaseDate/10)*10) %>%
  make_upset_plot(., Action:Western, decade,
               is_summarised = FALSE,
                n_intersections = 10,
                show_empty_sets = FALSE,
                intersection_order = "descending",
                set_order = "descending",
                intersection_colour_fn = function(set_vector) length(set_vector)
                )

prepare_data(movies, Action:Western,
             intersection_colour_fn = function(set_names) setequal(set_names, c("Western", "Romance"))) %>%
  make_matrix_plot(colour_palette = function(n) c(rep("Black", n-1), "Red"))




printx <- function(...) {
  list(...)$x
}

printx(x = 1, y = 2)




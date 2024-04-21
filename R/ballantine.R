library(tidyverse)
library(finalfit)

#' Creates ballantine graphs
#'
#' Based on a dataset, dependent variable, and up to 3 independent variables,
#' creates a ballantine plot displaying the amount of shared and unique
#' variance is explained by each independent variable to the dependent variable.
#'
#' Produces a ggplot object. Best used as a visualization shorthand.
#'
#' @param data dataframe object to be used
#' @param dep dependent variable to be regressed on. Must be a character type.
#'     Column must exist in `data`.
#' @param ind_list character vector of independent variables to regress
#'     dependent variable on. Max length of 3. Columns must exist in `data`.
#' @param round number of decimal points to round values to. Defaults to 2.
#' @param leftover logical indicating whether the leftover/unexplained variance
#'     should be included outside of the main ballantine graph. Defaults to
#'     TRUE.
#'
#' @export
#'
#' @examples
#' #one independent variable
#' ballantine(mtcars, "hp", "disp")
#'
#' #two independent variables
#' ballantine(mtcars, "hp", c("disp", "wt"))
#'
#' #three independent variables
#' ballantine(mtcars, "hp", c("disp", "wt", "qsec"))

ballantine <- function(data, dep, ind_list, round = 2, leftover = T) {
  temp_list <- list()
  for (ind in ind_list) {
    temp <- list(ind = 1:0)
    temp_list <- c(temp_list, temp)
  }
  names(temp_list) <- ind_list

  temp_df <- as.data.frame(expand.grid(temp_list))

  for (col in colnames(temp_df)) {
    temp_df[[col]] <- ifelse(temp_df[[col]], col, 1)
  }

  temp_df <- temp_df %>%
    mutate(pred = unlist(pmap(., paste, sep = "+"))) %>%
    mutate(form = paste(dep, "~", pred)) %>%
    filter(row_number() <= n() - 1)

  model_list <- temp_df$form %>%
    map(as.formula) %>%
    map(lm, data = data)

  r_squares <- c()
  for (model in model_list) {
    r_squares <- append(r_squares, summary(model)$r.squared)
  }

  if (length(ind_list) == 1) {
    final_plot <- one_ind(ind_list, r_squares, round, leftover)
  } else if (length(ind_list) == 2) {
    final_plot <- two_ind(ind_list, r_squares, round, leftover)
  } else if (length(ind_list) == 3) {
    final_plot <- three_ind(ind_list, r_squares, round, leftover)
  } else {
    stop("length of ind_list must be 1-3")
  }

  return(final_plot)
}

#' Plotting for 1 independent variable
#'
#' Creates a ballantine plot for 1 independent variable. For internal use only.
#'
#' @param ind_list see ind_list for `ballantine()`
#' @param r_squares numeric vector of the r_squares of all the generated models.
#' Contact Shawn for specifications regarding which models they refer to.
#' @param round see round for `ballantine()`
#' @param leftover see leftover for `ballantine()`
#'
#' @noRd

one_ind <- function(ind_list, r_squares, round, leftover) {

  remaining <- round_tidy(1 - r_squares, round)

  df <- data.frame(x = 0, y = 0, Rsquare = round_tidy(r_squares, round),
                   ind = ind_list)

  plot <- ggplot(data = df, aes(x = x, y = y)) +
    geom_point(size = 100, shape = 1) +
    theme_bw() +
    scale_y_continuous(limits = c(-0.25, 0.25)) +
    scale_x_continuous(limits = c(-0.25, 0.25)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", linewidth=2),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank()) +
    geom_text(aes(label = ind, x = x, y = y + 0.1)) +
    geom_text(aes(label = Rsquare, x = x, y = y))

  if (leftover) {
    plot <- plot +
      geom_text(aes(label = remaining, x = x - 0.2, y = 0.1))
  }

  return(plot)
}

#' Plotting for 2 independent variables
#'
#' Creates a ballantine plot for 2 independent variables. For internal use only.
#'
#' @param ind_list see ind_list for `ballantine()`
#' @param r_squares numeric vector of the r_squares of all the generated models.
#' Contact Shawn for specifications regarding which models they refer to.
#' @param round see round for `ballantine()`
#' @param leftover see leftover for `ballantine()`
#'
#' @noRd

two_ind <- function(ind_list, r_squares, round, leftover) {

  base_mat <- matrix(c(1, 1, 1, 0, 1, 1, 1, 1, 0), 3, 3, byrow = T)
  rowred <- solve(base_mat, r_squares)

  df_r <- data.frame(x = c(-0.15, 0, 0.15),
                     y = c(0, 0, 0),
                     Rs = round_tidy(rowred, round))

  remaining <- round_tidy(1 - sum(rowred), round)

  df <- data.frame(x = c(-.075, .075), y = c(0, 0), ind = ind_list)

  plot <- ggplot(data = df, aes(x = x, y = y)) +
    geom_point(size = 100, shape = 1) +
    theme_bw() +
    scale_y_continuous(limits = c(-0.3, 0.3)) +
    scale_x_continuous(limits = c(-0.3, 0.3)) +
    geom_text(aes(label = ind, x = x + x/3, y = 0.25)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", linewidth=2),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank()) +
    geom_text(data = df_r, aes(label = Rs, x = x, y = y))

  if (leftover) {
    plot <- plot +
      geom_text(aes(label = remaining, x = 0, y = -0.25))
  }

  if (sum(rowred < 0) > 0) {
    warning("1 or more areas includes a negative value!")
  }

  return(plot)
}

#' Plotting for 3 independent variables
#'
#' Creates a ballantine plot for 3 independent variables. For internal use only.
#'
#' @param ind_list see ind_list for `ballantine()`
#' @param r_squares numeric vector of the r_squares of all the generated models.
#' Contact Shawn for specifications regarding which models they refer to.
#' @param round see round for `ballantine()`
#' @param leftover see leftover for `ballantine()`
#'
#' @noRd

three_ind <- function(ind_list, r_squares, round, leftover) {
  base_mat <- matrix(c(1, 1, 1, 1, 1, 1, 1,
                       0, 1, 1, 1, 1, 1, 1,
                       1, 1, 1, 1, 0, 1, 1,
                       0, 0, 1, 1, 0, 1, 1,
                       1, 1, 1, 1, 1, 1, 0,
                       0, 1, 1, 0, 1, 1, 0,
                       1, 1, 1, 1, 0, 0, 0),
                     7, 7, byrow = T)
  rowred <- solve(base_mat, r_squares)

  df_r <- data.frame(x = c(0, -0.125, 0, 0.125, -0.2, 0, 0.2),
                     y = c(0.25, 0.1, 0, 0.1, -0.2, -0.25, -0.2),
                     Rs = round_tidy(rowred, round))

  remaining <- round_tidy(1 - sum(rowred), round)

  df <- data.frame(x = c(0, -.075, .075),
                   y = c(0.075, -0.075, -0.075), ind = ind_list)

  plot <- ggplot(data = df, aes(x = x, y = y)) +
    geom_point(size = 100, shape = 1) +
    theme_bw() +
    scale_y_continuous(limits = c(-0.4, 0.4)) +
    scale_x_continuous(limits = c(-0.4, 0.4)) +
    geom_text(aes(label = ind,
                  x = c(0, -0.35, 0.35),
                  y = c(0.4, -0.15, -0.15))) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", linewidth=2),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank()) +
    geom_text(data = df_r, aes(label = Rs, x = x, y = y))

  if (leftover) {
    plot <- plot +
      geom_text(aes(label = remaining, x = -0.35, y = 0.35))
  }

  if (sum(rowred < 0) > 0) {
    warning("1 or more areas includes a negative value!")
  }

  return(plot)
}

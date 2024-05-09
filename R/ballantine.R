#' @import dplyr
#' @import purrr
#' @import ggplot2
#' @import finalfit
#' @import ggforce
#' @import stringr


#' @title Creates ballantine graphs
#'
#' @description Based on a dataset, dependent variable, and up to 4 independent variables,
#' creates a ballantine plot displaying the amount of shared and unique
#' variance is explained by each independent variable to the dependent variable.
#'
#' Produces a ggplot object. Best used as a visualization shorthand.
#'
#' @param data dataframe object to be used
#' @param dep dependent variable to be regressed on. Must be a character type.
#'     Column must exist in `data`.
#' @param ind_list character vector of independent variables to regress
#'     dependent variable on. Max length of 4. Columns must exist in `data`.
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
#'
#' #three independent variables
#' ballantine(mtcars, "hp", c("disp", "wt", "qsec", "drat"))

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
  } else if (length(ind_list) == 4) {
    final_plot <- four_ind(ind_list, r_squares, round, leftover)
  } else {
    stop("length of ind_list must be 1-4")
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

  remaining <- str_replace(as.character(round_tidy(1 - r_squares, round)),
                           '0.', '.')

  df <- data.frame(x = 0, y = 0, r = 0.5,
                   Rsquare = str_replace(as.character(round_tidy(r_squares, round)),
                                         '0.', '.'),
                   ind = ind_list)

  plot <- ggplot(data = df) +
    geom_circle(aes(x0 = x, y0 = y, r = r)) +
    theme_bw() +
    scale_y_continuous(limits = c(-1, 1)) +
    scale_x_continuous(limits = c(-1, 1)) +
    coord_fixed() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", linewidth=2),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank()) +
    geom_text(aes(label = ind, x = x, y = y + 0.6)) +
    geom_text(aes(label = Rsquare, x = x, y = y))

  if (leftover) {
    plot <- plot +
      annotate("text", label = remaining, x = -0.7, y = 0.8)
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

  df_r <- data.frame(x = c(-0.4, 0, 0.4),
                     y = c(0, 0, 0),
                     Rs = str_replace(as.character(round_tidy(rowred, round)),
                                      '0.', '.'))

  remaining <- str_replace(as.character(round_tidy(1 - sum(rowred), round)),
                           '0.', '.')

  df <- data.frame(x = c(-.25, .25), y = c(0, 0),
                   r = c(0.5, 0.5),
                   ind = ind_list)

  plot <- ggplot(data = df) +
    geom_circle(aes(x0 = x, y0 = y, r = r)) +
    theme_bw() +
    scale_y_continuous(limits = c(-1, 1)) +
    scale_x_continuous(limits = c(-1, 1)) +
    coord_fixed() +
    geom_text(aes(label = ind, x = x + x/2, y = .6)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", linewidth=2),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank()) +
    geom_text(data = df_r, aes(label = Rs, x = x, y = y))

  if (leftover) {
    plot <- plot +
      annotate("text", label = remaining, x = 0, y = -0.6)
  }

  if (sum(rowred < 0) > 0) {
    warning("1 or more areas contains a negative value! Suppression is present")
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

  df_r <- data.frame(x = c(0, -0.4, 0, 0.4, -0.55, 0, 0.55),
                     y = c(0.5, 0.1, 0, 0.1, -0.4, -0.55, -0.4),
                     Rs = str_replace(as.character(round_tidy(rowred, round)),
                                      '0.', '.'))

  remaining <- str_replace(as.character(round_tidy(1 - sum(rowred), round)),
                           '0.', '.')

  df <- data.frame(x = c(0, -.3, .3),
                   y = c(0.25, -0.25, -0.25),
                   r = c(.6, .6, .6),
                   ind = ind_list)

  plot <- ggplot(data = df) +
    geom_circle(aes(x0 = x, y0 = y, r = r)) +
    theme_bw() +
    scale_y_continuous(limits = c(-1, 1)) +
    scale_x_continuous(limits = c(-1, 1)) +
    coord_fixed() +
    geom_text(aes(label = ind,
                  x = c(0, -0.5, 0.5),
                  y = c(0.95, -0.95, -0.95))) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", linewidth=2),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank()) +
    geom_text(data = df_r, aes(label = Rs, x = x, y = y))

  if (leftover) {
    plot <- plot +
      annotate("text", label = remaining, x = -0.7, y = 0.8)
  }

  if (sum(rowred < 0) > 0) {
    warning("1 or more areas contains a negative value! Suppression is present")
  }

  return(plot)
}

#' Plotting for 4 independent variables
#'
#' Creates a ballantine plot for 4 independent variables. For internal use only.
#'
#' @param ind_list see ind_list for `ballantine()`
#' @param r_squares numeric vector of the r_squares of all the generated models.
#' Contact Shawn for specifications regarding which models they refer to.
#' @param round see round for `ballantine()`
#' @param leftover see leftover for `ballantine()`
#'
#' @noRd


four_ind <- function(ind_list, r_squares, round, leftover) {

  base_mat <- matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                       1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                       0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                       0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                       1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                       1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1,
                       0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                       0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1,
                       1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1,
                       1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0,
                       0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1,
                       0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0,
                       1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1,
                       1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 1, 0,
                       0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1),
                     15, 15, byrow = T)

  rowred <- solve(base_mat, r_squares)

  df_r <- data.frame(x = c(-0.25, 0.25,
                           -0.8, -0.45, 0, 0.45, 0.8,
                           -0.2, 0.2,
                           -0.3, 0, 0.3,
                           -0.1, 0.1,
                           0),
                     y = c(0.35, 0.35,
                           0.2, 0.2, 0.15, 0.2, 0.2,
                           -0.1, -0.1,
                           -0.4, -0.3, -0.4,
                           -0.47, -0.47,
                           -0.55),
                     Rs = str_replace(as.character(round_tidy(rowred, round)),
                                      '0.', '.'))

  remaining <- str_replace(as.character(round_tidy(1 - sum(rowred), round)),
                           '0.', '.')

  df <- data.frame(x = c(-.36, -0.15, 0.15, .36),
                   y = c(-.1, 0, 0, -.1),
                   a = c(0.7, 0.7, 0.7, 0.7),
                   b = c(0.3, 0.3, 0.3, 0.3),
                   angle = c(-pi/4, -pi/4, pi/4, pi/4),
                   ind = ind_list)

  plot <- ggplot(data = df) +
    geom_ellipse(aes(x0 = x, y0 = y, a = a, b = b, angle = angle)) +
    theme_bw() +
    scale_y_continuous(limits = c(-1, 1)) +
    scale_x_continuous(limits = c(-1, 1)) +
    coord_fixed() +
    geom_text(aes(label = ind,
                  x = c(-.8, -.4, .4, .8),
                  y = c(-.3, .6, .6, -.3))) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", linewidth=2),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank()) +
    geom_text(data = df_r, aes(label = Rs, x = x, y = y))

  if (leftover) {
    plot <- plot +
      annotate("text", label = remaining, x = -0.8, y = 0.8)
  }

  if (sum(rowred < 0) > 0) {
    warning("1 or more areas contains a negative value! Suppression is present")
  }

  return(plot)
}

#' @title plot_cluster
#'
#' @description Plot cluster evolution for ensemble forecast data.
#'
#' This function visualizes the evolution of clusters over time for
#' ensemble forecast data.
#'
#' It allows customization of colors, line widths,
#' points, fading effects, and axis appearance. Optionally, the processed
#' data can also be returned.
#'
#' @param df A dataframe created by the define_cluster function
#' @param col_palette Character vector of colors to use for clusters.
#' @param return_data Logical. If TRUE, returns a list containing the plot and the processed data.
#' @param linesize Numeric. defines which size the line should have
#' @param linealpha Numeric between 0 and 1. Transparency of the cluster lines.
#' @param points List. Additional arguments for adding points (e.g. size).
#' @param fade_till_end Logical. If TRUE, cluster colors fade until the end of the time series.
#' @param steps Numeric. Number of extra steps between 2 time steps, creates smoother coloring
#' @param background Character. Background color of the plot.
#' @param axis_bg Character. Background color of the axis.
#' @param grid_col Character. Color of grid lines in the plot.
#' @param xaxis_text List of parameters passed to \code{element_text()} for x-axis text (e.g., color, size).
#' @param yaxis_text List of parameters passed to \code{element_text()} for y-axis text.
#' @param break_times Character. Breaks for the x-axis in \code{scale_x_datetime}.
#'
#' @return If \code{return_data = FALSE} (default), returns a \code{ggplot} object.
#' If \code{return_data = TRUE}, returns a list containing the plot and the data needed for the plot
#'
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @export
#' @examples plot_cluster(
#'   data,
#'   col_palette = c("#FFFB00","#FF7E20","#FF0000","#ED0DFF"),
#'   linesize = 1,
#'   fade_till_end = T,
#'   break_times = "2 day",
#'   points= list(size = 1.5)
#'
#')

plot_cluster <- function(data,
                         col_palette= c("#67FF00","#FFF000","#FFB556","#FF7133","#FF0000","#5700A4","#2100FF"),
                         return_data = FALSE,
                         linesize = 1.2,
                         linealpha = 1,
                         points = list(),
                         fade = TRUE,
                         fade_till_end = FALSE,
                         legend = FALSE,
                         steps = 1,
                         background = "grey37",
                         axis_bg = "grey37",
                         grid_col = "white",
                         xaxis_text = list(color = "black", size = 10),
                         yaxis_text = list(color = "black", size = 10),
                         break_times = "1 day"){

  df <- data

  if(!is.list(points)) stop("points has to be a list")
  if(!is.list(xaxis_text)) stop("xaxis_text has to be a list")
  if(!is.list(yaxis_text)) stop("yaxis_text has to be a list")
  if(!is.numeric(linesize)) stop("has to be numeric")
  if(!is.numeric(steps) || steps < 1) stop("steps muss >=1 sein")


  #Farben zuordnen
  n_clusters <- length(unique(df$Clusterlabel))
  color_ramp <- colorRampPalette(col_palette)
  colors <- color_ramp(n_clusters)
  sorted_clusters <- sort(unique(df$Clusterlabel))
  cluster_colors <- setNames(colors, sorted_clusters)
  df$Color <- cluster_colors[df$Clusterlabel]

  if(!fade){
    # plot erstellen
    plot <- ggplot(df) +
      geom_line(aes(x = date_and_time, y = Measure,
                    color = Color, group = Member),
                linewidth = linesize,
                alpha = linealpha,
                na.rm = TRUE) +
      scale_color_identity(
        guide  = if(legend) "legend" else "none",
        breaks = if(legend) cluster_colors else NULL,
        labels = if(legend) names(cluster_colors) else NULL,
        name   = if(legend) "Cluster" else NULL
      ) +
      scale_x_datetime(date_labels = "%d.%m. %H:00", date_breaks = break_times) +
      theme(
        panel.background = element_rect(fill = background),
        plot.background  = element_rect(fill = axis_bg),
        panel.grid.major  = element_line(color = grid_col),
        axis.text.x = do.call(element_text, xaxis_text),
        axis.text.y = do.call(element_text, yaxis_text),
        legend.background = element_rect(fill = axis_bg)
      )

    # Punkte hinzufügen
    if(length(points) > 0) {
      plot <- plot +
        do.call(geom_point, c(
          list(aes(x = date_and_time, y = Measure, color = Color),
               data = df,
               show.legend = FALSE),
          points
        ))
    }

    # daten mitgeben
    if(return_data){
      return(list(plot = plot, data = df))
    } else {
      return(plot)
    }
  }



  color_steps <- function(colors, breaks, max_ts) {
    res <- character(0)

    if (length(colors) > 1 && fade_till_end) {
      breaks[length(breaks)] <- max_ts + 1
    }

    for (i in seq_along(colors)) {
      start <- breaks[i]
      end <- if (i < length(colors)) breaks[i+1] - 1 else max_ts
      n <- end - start + 1

      col_start <- colors[i]
      col_end   <- if (i < length(colors)) colors[i+1] else colors[i]

      ramp <- colorRampPalette(c(col_start, col_end))(n)
      ramp <- ramp[round(seq(1, length(ramp), length.out = n))]

      res <- c(res, ramp)
    }
    res
  }

  df <- df |>
    group_by(Member) |>
    arrange(Timeindex, .by_group = TRUE) |>
    mutate(
      min_ts = ave(Timeindex, Color, FUN = min),
      dyncolor = color_steps(unique(Color[order(min_ts)]),
                             unique(min_ts[order(min_ts)]),
                             max(Timeindex)),
      Timeindex_end = lead(Timeindex),
      date_and_time_end = lead(date_and_time),
      Measure_end   = lead(Measure)
    ) |>
    ungroup()

  #df ohne extra zwischenschritte speichern
  noextrastepsdf <- df

  #extra schritte erstellen
  if(steps > 1){
    extrasteps <- function(x1, y1, x2, y2, col1, col2, steps){

      # falls col2 fehlt oder NA mach wie col1
      if(is.na(col2)) col2 <- col1

      #gleicmäßige schritte
      x <- seq(x1, x2, length.out = steps + 1)
      y <- seq(y1, y2, length.out = steps + 1)
      cols <- colorRampPalette(c(col1, col2))(steps)


      #zwischenschritte als df
      data.frame(
        date_and_time = x[-(steps+1)],
        Measure = y[-(steps+1)],
        date_and_time_end = x[-1],
        Measure_end = y[-1],
        dyncolor = cols
      )
    }

    #zwischenschritte einfügen
    df <- df |>
      filter(!is.na(Timeindex_end)) |>
      group_by(Member) |>
      group_modify(~{
        do.call(rbind, Map(extrasteps,
                           .x$date_and_time,
                           .x$Measure,
                           .x$date_and_time_end,
                           .x$Measure_end,
                           .x$dyncolor,
                           lead(.x$dyncolor),
                           MoreArgs = list(steps = steps)))
      }) |>
      ungroup()
  }


  #plot erstellen
  plot <- ggplot(df) +
    geom_segment(aes(x = date_and_time, y = Measure,
                     xend = date_and_time_end, yend = Measure_end,
                     color = dyncolor),
                 linewidth = linesize,
                 alpha = linealpha,
                 na.rm = TRUE) +
    scale_color_identity(
      guide = if(legend) "legend" else "none",
      breaks = if(legend) cluster_colors else NULL,
      labels = if(legend) names(cluster_colors) else NULL,
      name   = if(legend) "Cluster" else NULL
    ) +
    scale_x_datetime(date_labels = "%d.%m. %H:00", date_breaks = break_times) +
    theme(
      panel.background = element_rect(fill = background),
      plot.background  = element_rect(fill = axis_bg),
      panel.grid.major  = element_line(color = grid_col),
      axis.text.x = do.call(element_text, xaxis_text),
      axis.text.y = do.call(element_text, yaxis_text),
      legend.background = element_rect(fill = axis_bg)
    )

  # Punkte hinzufügen (wie vorher)
  if(length(points) > 0) {
    plot <- plot +
      do.call(geom_point, c(
        list(aes(x = date_and_time, y = Measure, color = dyncolor),
             data = noextrastepsdf,
             show.legend = FALSE),
        points
      ))
  }


  #daten mitgeben?
  if(return_data){
    return(list(plot = plot,data = df,steplessdata = noextrastepsdf))
  } else {
    return(plot)
  }

}

#' Enclust: Ensemble Clustering and Visualization
#'
#' The `Enclust` package provides tools to fetch environmental ensemble forecast data,
#' perform dynamic clustering on the data, and visualize the evolution of clusters over time.
#' It is designed for analyzing ensemble forecasts from NOMADS.
#'
#' Main functions:
#' \itemize{
#'   \item \code{\link{get_data}}: Fetches environmental forecast data from NOMADS
#'         and loads it into a dataframe.
#'   \item \code{\link{define_cluster}}: Performs dynamic clustering on ensemble
#'         forecast data, allocating members to clusters and generating clusterlabels.
#'   \item \code{\link{plot_cluster}}: Visualizes the temporal evolution of clusters
#'         from clustered ensemble data.
#'   \item \code{\link{cluster_info}}: Provides a summary of clustered dataframes
#'         created by \code{define_cluster}, including cluster sizes, statistics,
#'         and member histories.
#' }
#'
#'
"_PACKAGE"

#'@title Get cluster information
#'
#'@description This function gives a summary of the clustered dataframe of define_cluster
#'
#'@param df A dataframe which was created using the define_cluster function
#'
#'@return A list (invisibly) containing:
#' \describe{
#'   \item{cluster_amount}{Number of unique clusters at the last time step}
#'   \item{cluster_sizes}{Table of cluster sizes at the last time step}
#'   \item{cluster_stats}{Data frame with statistics for each cluster }
#'   \item{cluster_stats_full}{Data frame that contains the cluster statistics for every time step}
#'   \item{cluster_history}{Data frame showing each member's cluster assignment for each member and time step}
#'   \item{cluster_history_full}{Data frame showing original cluster assignments for each member and time step}
#'   \item{original_data}{Dataframe of the original data with Timeindex, date_and_time, Member, and Measure}
#' }
#'
#' @details
#' When calling the function, the following elements are printed to the console:
#' \itemize{
#'   \item cluster_amount
#'   \item cluster_sizes
#'   \item cluster_stats
#' }
#'
#' @import dplyr
#' @import tidyr
#' @export
#'
#' @examples
#' cluster_info(data)
#' cluster_info(data)$cluster_history
cluster_info <- function(df) {

  #Wie viele Cluster entstanden?
  cluster_amount <- length(unique(df$Clusterlabel))

  #Welche Cluster sind mit wie viel Membern entstanden ?
  cluster_sizes <- table(df$Clusterlabel[df$Timeindex == max(df$Timeindex)])



  cluster_stats <- df |>
    group_by(Clusterlabel) |>
    summarise(
      since          = min(date_and_time,na.rm = TRUE),
      mean_measure   = mean(Measure, na.rm = TRUE),
      median_measure = median(Measure, na.rm = TRUE),
      sd_measure     = sd(Measure, na.rm = TRUE),
      min_measure    = min(Measure, na.rm = TRUE),
      max_measure    = max(Measure, na.rm = TRUE),
      betweenss      = mean(betweenss),
      totss          = mean(totss),
      totwithinss    = mean(totwithinss),
      betweensstotss = mean(betweensstotss),
      .groups = "drop"
    )

  cluster_stats_full <- df |>
    group_by(Clusterlabel, Timeindex) |>
    summarise(
      mean_measure   = mean(Measure, na.rm = TRUE),
      median_measure = median(Measure, na.rm = TRUE),
      sd_measure     = sd(Measure, na.rm = TRUE),
      min_measure    = min(Measure, na.rm = TRUE),
      max_measure    = max(Measure, na.rm = TRUE),
      betweenss      = ifelse(all(is.na(betweenss)), NA, max(betweenss, na.rm = TRUE)),
      totss          = ifelse(all(is.na(totss)), NA, max(totss, na.rm = TRUE)),
      totwithinss    = ifelse(all(is.na(totwithinss)), NA, max(totwithinss, na.rm = TRUE)),
      betweensstotss = ifelse(all(is.na(betweensstotss)), NA, max(betweensstotss, na.rm = TRUE)),
      .groups = "drop"
    )


  #Wann in welches Cluster
  cluster_history <- df |>
    select(Timeindex, Member, Clusterlabel) |>
    pivot_wider(names_from = Member, values_from = Clusterlabel)

  cluster_history_full <- df |>
    select(Timeindex, Member, Cluster) |>
    pivot_wider(names_from = Member, values_from = Cluster)


  original_data <- df |> select(Timeindex,date_and_time,Member,Measure)

  cat("Cluster Amount:\n")
  print(cluster_amount)

  cat("\nCluster Sizes:\n")
  print(cluster_sizes)

  cat("\nCluster Stats:\n")
  print(t(cluster_stats))

  invisible(list(
    cluster_amount = cluster_amount,
    cluster_sizes = cluster_sizes,
    cluster_stats = cluster_stats,
    cluster_stats_full = cluster_stats_full,
    cluster_history = cluster_history,
    cluster_history_full = cluster_history_full,
    original_data = original_data))
}

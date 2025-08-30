#' @title define_cluster
#'
#' @description
#' Define clusters in ensemble forecast data
#'
#' This function performs dynamic clustering on ensemble forecast data.
#'
#' It applies k-means clustering for each time step where a certain limit is reached.
#' And allocates the Clusters to their Members. The next Clustering only happens with members inside the current cluster.
#' Clusterlabels are given by the size of the clusters - the Cluster with the most members gets 1,second most 2,...
#'
#' The Labels of the cluster give a hint about their development over time.
#' @param data A dataframe created by the function get_data
#'
#' Or a dataframe that at least includes : a clomun named Timeindex and at least 1 column with numeric data
#' @param k Initial number of clusters that should be created when the limit is reached.
#'
#' @param limit A numeric value or a function that defines the limit that has to be reached for the clustering
#'
#' If a number is provided, it is treated as the limit for \code{max(diff(km$centers))}.
#'
#' Default is \code{max(diff(km$centers)) > 4}
#'
#' If given a custom limit function the function should return TRUE if the clustering should be done
#' @param max_cluster Maximum allowed number of clusters.
#'
#' @return A data frame with additional columns:
#' \describe{
#'   \item{Cluster}{Raw Clustersequence (eg. 1.1.1.1)}
#'   \item{Clusterlabel}{Clusterlabel that only counts splits as a new cluster}
#'   \item{betweenss, totss, totwithinss, betweensstotss, rsquarred}{Cluster statistics per time step}
#'   \item{Measure}{Original numeric measurement}
#'   \item{Member}{Different ensemble member}
#'   \item{Timeindex}{Time step index}
#'   \item{date_and_time}{Date and time matching the time steps}
#' }
#'
#'
#' @import tidyr
#' @import dplyr
#' @import Ckmeans.1d.dp
#' @export
#'
#' @examples
#' clustered_df <- define_cluster(data,k=2,limit=4)
#'
define_cluster <- function(data,
                           k=2,
                           limit = function(km,subdata,a=4) max(diff(km$centers)) > a,
                           max_cluster = 8){


  #wenn Zahl übergeben als limit verwenden
  if (is.numeric(limit)) {
    splitvalue <- limit
    limit <- function(km, subdata) max(diff(km$centers)) > splitvalue
  }


  #Falsche Eingaben abfangen
  if(k<2) stop("k has to be >= 2")
  if(!is.function(limit)) stop("limit has to be a custom function or a number")
  if (!is.data.frame(data)) stop("data must be a data.frame or tibble")
  if (!"Timeindex" %in% colnames(data)) stop("data must contain a 'Timeindex' column")
  if (length(setdiff(names(data)[sapply(data, is.numeric)], "Timeindex")) < 2) {
    stop("data must contain at least 2 numeric columns besides 'Timeindex' ")
  }


  df <- pivot_longer(data,
                     cols = where(is.numeric) & !matches("Timeindex"),
                     names_to = "Member",
                     values_to = "Measure"
  )

  cluster_mapping <- function(x) {
    res <- character(length(x))

    #zerlege string
    for (i in seq_along(x)) {
      parts <- strsplit(x[i], "\\.")[[1]]

      # Entferne 1en am Ende, außer wenn nur eine 1 übrig
      while (length(parts) > 1 && tail(parts, 1) == "1") {
        parts <- parts[-length(parts)]
      }

      res[i] <- paste(parts, collapse = ".")
    }

    names(res) <- x
    return(res)
  }

  #cluster mit kmeans
  df$Cluster <- "1"
  df$Clusterlabel <- NA_integer_
  df$betweenss <- NA_integer_
  df$totss <- NA_integer_
  df$totwithinss <- NA_integer_
  df$betweensstotss <- NA_integer_
  df$exvarianz <- NA_integer_
  df$rsquarred <- NA_integer_
  df$clustersize <- NA_integer_


  start_k <- k

  for (ts in unique(df$Timeindex)) {

    for (clust in unique(df$Cluster[df$Timeindex == ts])) {
      idx <- df$Timeindex == ts & df$Cluster == clust

      df$clustersize[idx] <- sum(idx)

      if(sum(idx) < 2) next

      # k wählen je nach Punktmenge
      n_points <- sum(idx)
      k <- min(start_k, max(2, ceiling(n_points / 2)))

      km <- Ckmeans.1d.dp(df$Measure[idx], k = k)

      # kmeans Sachen speichern
      df$betweenss[idx] <- km$betweenss
      df$totss[idx] <- km$totss
      df$totwithinss[idx] <- km$tot.withinss
      df$betweensstotss[idx] <- km$betweenss/km$totss
      df$rsquarred[idx] <- 1 - km$tot.withinss/km$totss

      map <- cluster_mapping(unique(df$Cluster))
      df$Clusterlabel <- map[df$Cluster]
      if(length(unique(df$Clusterlabel)) >= max_cluster) next
      if(!limit(km,df[idx,])) next


      # Clustergrößen ermitteln
      cluster_sizes <- table(km$cluster)

      # Cluster nach Größe sortieren (größtes zuerst)
      sorted_clusters <- order(cluster_sizes, decreasing = TRUE)

      # Neue Clusternamen erstellen: größtes Cluster = 1, zweitgrößtes = 2, ...
      new_labels <- km$cluster
      for (i in seq_along(sorted_clusters)) {
        new_labels[km$cluster == sorted_clusters[i]] <- i
      }

      # Clustergröße für jedes Element speichern
      df$clustersize[idx] <- cluster_sizes[km$cluster]

      # Labels anpassen - anhängen der neuen Cluster-Labels
      df$Cluster[df$Timeindex >= ts & df$Cluster == clust] <- paste0(
        df$Cluster[df$Timeindex >= ts & df$Cluster == clust], ".", new_labels
      )

    }

  }

  return(df)
}

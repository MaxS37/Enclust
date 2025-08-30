#' @title Get environmental data
#' @description This Function fetches data from https://nomads.ncep.noaa.gov, transforms it and loads it into a dataframe.
#'
#'
#' @param date string that defines the date of the forecast
#'
#' has to be in the format "yyyymmdd"
#' @param time string that defines the time of the forecast
#'
#' only "00","06","12","18" are valid inputs
#'
#' defines from which time the forecast should be
#' @param variable String that defines wich environmental variable should be fetched
#'
#' some examples: tmpprs,hgtprs,ugrdprs
#' @param lat Integer latitude of the desired place with .5 accuracy
#'
#' ranges from 0-360
#' @param lon Integer longitude of the desired place with .5 accuracy
#'
#' ranges from 0-719
#' @param pressr Integer pressurelevels for the data
#'
#' ranges from 0-11 - higher values = higher in the atmosphere
#'
#'
#'
#' @returns A Dataframe with environmental data for 31 Members of a Ensembleforcast from NOMADS depending on the given parameters
#' @import tidyr
#' @import httr2
#' @import dplyr
#' @export
#'
#' @examples
#' x <- get_data("20250825",lat = 280,lon = 30)
#' y <- get_data(variable = "hgtprs",pressr = 0)
get_data <- function(date = gsub("-","",Sys.Date()),
                     time="00",
                     lat=282,
                     lon=28,
                     variable = "tmpprs",
                     pressr = 2){

  if(variable %in% c("hgtprs","rhprs","tmpprs","ugrdprs","vgrdprs")) {
    base_url <- paste0("https://nomads.ncep.noaa.gov/dods/gefs/gefs",
                       date,
                       "/gefs_pgrb2ap5_all_",
                       time,
                       "z.ascii?",
                       variable,
                       "[0:30][0:64][",
                       pressr,
                       "][",
                       lat,
                       "][",
                       lon,
                       "]")
  } else {
    base_url <- paste0("https://nomads.ncep.noaa.gov/dods/gefs/gefs",
                       date,
                       "/gefs_pgrb2ap5_all_",
                       time,
                       "z.ascii?",
                       variable,
                       "[0:30][0:64][",
                       lat,
                       "][",
                       lon,
                       "]")
  }
  #Get request der base URL um body in einem String zu speichern
  raw_weather_data <- httr2::request(base_url) |> httr2::req_perform() |> httr2::resp_body_string()

  if(substr(raw_weather_data,0,6) == "<html>")
    stop("Data was not able to be fetched, try other Parameters")

  #ersetze alle Zeilenumbrüche (auch zusammenhängend mit einem #)
  weather_data <- gsub("\n+","#",raw_weather_data)

  #Trenne die Daten bei jedem # und mache einen Vector aus den Elementen
  weather_data_vector <- strsplit(weather_data,"#") |> unlist()

  #Filtere nur die Zeilen, die mit "[" beginnen
  weather_data_vector_filtered <- weather_data_vector[grepl("^\\[",weather_data_vector)]


  #Zeitdaten erheben, Zeit ist ab 01.01.01 in Tagen gegeben
  time_data <- weather_data_vector[grep("^time", weather_data_vector)+1] |> strsplit(split = ",") |> unlist()

  time_and_dates <- as.POSIXct("0001-01-01",tz = "UTC") + as.numeric(time_data) * 86400

  # erstelle ein Dataframe aus den Daten, was pro Ensemble eine Spalte hat und Zeitindex- und Zeitpunktspalte
  df <- data.frame(weather_data_vector_filtered)

  df$index <- substr(df$weather_data_vector_filtered, 1,regexpr(",",df$weather_data_vector_filtered) - 1)

  df$Measure <- substr(df$weather_data_vector_filtered,regexpr(",",df$weather_data_vector_filtered) + 1,nchar(df$weather_data_vector_filtered))
  df$Measure[df$Measure == " 9.999E20"] <- NA


  df$Member <- paste0("Member_",(substr(df$index, 2, regexpr("\\]",df$index) - 1)))

  df$Timeindex <-  substr(df$index,
                          regexpr("\\]",df$index) + 2,
                          regexpr("\\]",df$index) + attr(regexpr("\\].*?\\]", df$index),"match.length")-2)

  #lösche ersten beiden Spalten
  df <- df[, -(1:2)]

  newdf <- pivot_wider(df,names_from = Member,values_from = Measure)

  newdf[] <- lapply(newdf, as.numeric)

  if (variable %in% c("tmpprs","tsoil0_10cm","tmp2m","tmin2m","tmax2m"))
    newdf <- newdf |> mutate(across(starts_with("Member_"), ~ . - 273.15))

  newdf$date_and_time <- time_and_dates


  message("Data was fetched from: ",base_url)
  return (newdf)
}

# Enclust

**Enclust** ist ein R-Paket zur Analyse und Clusterung von Zeitreihen, speziell für Umweltdaten.  
Es bietet Funktionen zur Datenerhebung, dem Clustering und Visualisierung dieser.

## Installation

Man kann das Paket direkt in R-Stdio installieren:

```r
# Falls devtools nicht installiert ist
install.packages("devtools")

# Enclust von GitHub installieren
devtools::install_github("MaxS37/Enclust")
```

Anschließend kann man beispielsweise Temperaturvorhersagen von Sachsen als Plot ausgeben lassen mit diesem Code:
```r
library(Enclust)
get_data() |> define_cluster(max_cluster = 12) |> plot_cluster(steps = 10)
```

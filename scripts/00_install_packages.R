# Pakete fuer die vorhandenen Kursskripte einmalig installieren.
# block = 1: Warmup und Block 1; block = 2: zusätzlich Block 2.
block <- 1

packages <- c(
  "terra", "sf", "dplyr", "tidyr", "lubridate", "ggplot2", "viridis",
  "gstat", "randomForest", "leaflet",
  "rstac", "CDSE"
)

if (block >= 2) packages <- unique(c(packages, "landscapemetrics", "geodata", "supercells"))

missing <- setdiff(packages, rownames(installed.packages()))

if (length(missing) > 0) {
  message("Installiere fehlende Pakete: ", paste(missing, collapse = ", "))
  install.packages(
    missing,
    repos = "https://cloud.r-project.org",
    dependencies = c("Depends", "Imports", "LinkingTo")
  )
}

still_missing <- setdiff(packages, rownames(installed.packages()))
if (length(still_missing) > 0) {
  stop("Installation nicht abgeschlossen. Es fehlen: ",
       paste(still_missing, collapse = ", "))
}

message("Alle benoetigten R-Pakete sind installiert.")

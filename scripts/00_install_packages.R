# Install packages once. Run this file only if packages are missing.
packages <- c("terra", "sf", "dplyr", "tidyr", "lubridate", "ggplot2", "viridis", "gstat", "randomForest", "leaflet")
missing <- setdiff(packages, rownames(installed.packages()))
if (length(missing) > 0) install.packages(missing, dependencies = TRUE)

# WS05: WorldClim-Temperaturjahresgang laden und drei Segmentierungen rechnen.
# Eingabe: data/klima_aoi.gpkg, ein Polygon, ungefähr 100 km Kantenlänge.
# Ausgabe: data/temperatur_monate.tif; output/klima/ mit Segmenten und Parametern.
library(terra)
library(sf)
library(geodata)
library(supercells)

# 1. Gebiet und Datenbezug
gebiet <- st_read("data/klima_aoi.gpkg", quiet = TRUE) |> st_transform(25832)
stopifnot(nrow(gebiet) == 1, all(st_is_valid(gebiet)), !any(st_is_empty(gebiet)))
bb <- st_bbox(gebiet)
vorlage <- rast(xmin = floor(bb["xmin"] / 1000) * 1000,
                xmax = ceiling(bb["xmax"] / 1000) * 1000,
                ymin = floor(bb["ymin"] / 1000) * 1000,
                ymax = ceiling(bb["ymax"] / 1000) * 1000,
                resolution = 1000, crs = "EPSG:25832")
z <- "output/klima"
dir.create(z, recursive = TRUE, showWarnings = FALSE)

# 2. Download: zwölf Monatsmittel, 1970–2000, WorldClim 2.1, Grad Celsius.
# geodata verwendet bereits vorhandene lokale Downloads erneut.
roh <- geodata::worldclim_country(country = "DEU", var = "tavg", path = "data")
stopifnot(nlyr(roh) == 12)
# Bandnummern ausdrücklich aus den Quelldateinamen lesen.
monate <- as.integer(sub(".*_([0-9]+)$", "\\1", names(roh)))
stopifnot(setequal(monate, 1:12))
roh <- roh[[order(monate)]]
x <- project(roh, vorlage, method = "bilinear")
innen <- rasterize(vect(gebiet), vorlage, field = 1, background = NA, touches = FALSE)
x <- mask(x, innen)
x <- mask(x, sum(!is.na(x)) == 12, maskvalues = 0)
names(x) <- sprintf("tavg_%02d", 1:12)
if (global(!is.na(x[[1]]), "sum", na.rm = TRUE)[1, 1] < 100) {
  stop("Zu wenige vollständige Klimazellen: Gebiet und Datenabdeckung prüfen.")
}
writeRaster(x, "data/temperatur_monate.tif", overwrite = TRUE)
write.csv(data.frame(band = 1:12, monat = 1:12, einheit = "Grad Celsius",
                     produkt = "WorldClim 2.1", bezug = "1970-2000", abrufdatum = Sys.Date()),
          file.path(z, "datenbezug.csv"), row.names = FALSE)

# 3. Nur Segmentzahl oder Kompaktheit ändern; Eingabewerte nicht standardisieren.
parameter <- data.frame(variante = c("k50_c1", "k100_c1", "k50_c5"),
                        k = c(50, 100, 50), compactness = c(1, 1, 5))
for (i in seq_len(nrow(parameter))) {
  s <- supercells(x, k = parameter$k[i], compactness = parameter$compactness[i])
  s$segment_id <- seq_len(nrow(s))
  a <- as.numeric(st_area(s)) / 1e6
  parameter$n_segmente[i] <- nrow(s)
  parameter$min_km2[i] <- min(a)
  parameter$median_km2[i] <- median(a)
  parameter$max_km2[i] <- max(a)
  st_write(s, file.path(z, paste0(parameter$variante[i], ".gpkg")),
           delete_dsn = TRUE, quiet = TRUE)
}
write.csv(parameter, file.path(z, "parameter.csv"), row.names = FALSE)
writeLines(capture.output(sessionInfo()), file.path(z, "sessionInfo.txt"))
print(parameter)

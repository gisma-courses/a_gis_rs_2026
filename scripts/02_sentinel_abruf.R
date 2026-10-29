# Sentinel-2: Sommeraufnahmen selbst suchen und abrufen
# Name / Datum: ergänzen
# Grundlage: gisma/LV-19-d19-006-25-R,
# src/S1_observation/01-2_get_cdse-sentinel-data.R
# Reduzierte Lehrfassung: eigenes AOI, lokale Dateien, RGB/Rot/NIR/SCL.
# Start: RStudio-Projekt aus WS01 öffnen; abschnittsweise ausführen.

library(sf)
library(terra)
library(rstac)
library(dplyr)
library(CDSE)

# 1. Einstellungen -----------------------------------------------------
jahre <- 2018:2022
monate <- 6:8
anzahl_abrufe <- 1  # Erst einen Abruf prüfen; danach auf Inf setzen.
aoi_datei <- "data/aoi.gpkg"
ordner <- "data/sentinel"
dir.create(ordner, recursive = TRUE, showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)

# 2. Eigenes Gebiet: ein kleines Polygon in Hessen ----------------------
aoi <- st_read(aoi_datei, quiet = TRUE) |> st_transform(32632)
stopifnot(nrow(aoi) == 1, all(st_is_valid(aoi)))
bb <- st_bbox(aoi)
if (max(bb["xmax"] - bb["xmin"], bb["ymax"] - bb["ymin"]) > 5000) {
  stop("Für diese Übung ein Gebiet mit höchstens 5 km Kantenlänge wählen.")
}
plot(st_geometry(aoi))

# 3. Katalog abfragen: Metadaten, noch keine Raster ----------------------
zeitraum <- paste0(min(jahre), "-01-01T00:00:00Z/",
                   max(jahre), "-12-31T23:59:59Z")
treffer <- rstac::stac("https://stac.dataspace.copernicus.eu/v1/") |>
  rstac::stac_search(
    collections = "sentinel-2-l2a",
    bbox = as.numeric(st_bbox(st_transform(aoi, 4326))),
    datetime = zeitraum, limit = 500
  ) |>
  rstac::post_request() |>
  rstac::items_fetch()
saveRDS(treffer, "data/stac_treffer.rds")
tabelle <- rstac::items_as_tibble(treffer)

kandidaten <- tabelle |>
  mutate(datum = as.Date(datetime),
         jahr = as.integer(format(datum, "%Y")),
         monat = as.integer(format(datum, "%m")),
         wolken = as.numeric(`eo:cloud_cover`)) |>
  filter(jahr %in% jahre, monat %in% monate) |>
  group_by(jahr, datum) |>
  summarise(szenen = n(), wolken_mittel = mean(wolken, na.rm = TRUE),
            .groups = "drop") |>
  filter(is.finite(wolken_mittel)) |>
  arrange(jahr, wolken_mittel, datum)
write.csv(kandidaten, "output/aufnahmekandidaten.csv", row.names = FALSE)
print(kandidaten)

# Vorschlag: ein Tag je Jahr, mit geringster mittlerer Szenenbewölkung.
# Das ist noch keine Prüfung der Wolken über unserem kleinen Gebiet.
vorschlag <- kandidaten |> group_by(jahr) |> slice_head(n = 1) |> ungroup()
if (nrow(vorschlag) != length(jahre)) stop("Für mindestens ein Jahr fehlen Kandidaten.")

# Diese Datei bleibt bei Wiederholung erhalten: Hier wählen Sie Ihre Tage.
if (!file.exists("data/auswahl.csv")) {
  write.csv(vorschlag, "data/auswahl.csv", row.names = FALSE)
}
auswahl <- read.csv("data/auswahl.csv")
auswahl$datum <- as.Date(auswahl$datum)
auswahl <- auswahl[order(auswahl$datum), ]
stopifnot(nrow(auswahl) > 0, !anyNA(auswahl$datum),
          !anyDuplicated(auswahl$datum),
          all(auswahl$datum %in% kandidaten$datum))
print(auswahl)

# 4. Anmeldung: Werte aus der persönlichen .Renviron ---------------------
id <- Sys.getenv("CDSE_ID")
secret <- Sys.getenv("CDSE_SECRET")
if (id == "" || secret == "") stop("CDSE_ID / CDSE_SECRET fehlen.")
client <- CDSE::GetOAuthClient(id = id, secret = secret)

# 5. Serverausgabe definieren -------------------------------------------
# Reflexionen als FLOAT32; SCL und dataMask bleiben als eigene Bänder.
# NDVI wird erst anschließend lokal berechnet.
evalscript <- '//VERSION=3
function setup() {
  return {
    input: [{bands: ["B02", "B03", "B04", "B08", "SCL", "dataMask"]}],
    output: {bands: 6, sampleType: "FLOAT32"}
  };
}
function evaluatePixel(s) {
  return [s.B02, s.B03, s.B04, s.B08, s.SCL, s.dataMask];
}'
writeLines(evalscript, "data/sentinel_evalscript.js")

# 6. Erst ein Datum, anschließend die gewählte Reihe --------------------
for (i in seq_len(min(nrow(auswahl), anzahl_abrufe))) {
  tag <- as.character(auswahl$datum[i])
  datei <- file.path(ordner, paste0("s2_", tag, ".tif"))
  if (file.exists(datei)) next
  message("Abruf: ", tag)
  bild <- CDSE::GetImage(
    aoi = aoi, time_range = tag,
    collection = "sentinel-2-l2a", script = evalscript,
    format = "image/tiff", mosaicking_order = "leastCC",
    resolution = 10, client = client
  )
  stopifnot(inherits(bild, "SpatRaster"), nlyr(bild) == 6)
  names(bild) <- c("B02", "B03", "B04", "B08", "SCL", "dataMask")
  writeRaster(bild, datei, overwrite = FALSE)
}
# Tagesabruf: Überlappende Kacheln können mosaikiert werden.
# Das ausgewählte Datum ist festgelegt, keine einzelne Produkt-ID.
writeLines(capture.output(sessionInfo()), "output/sessionInfo.txt")

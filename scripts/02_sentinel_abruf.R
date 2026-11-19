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

# Ein festes UTM-Gitter für alle Termine, an vollen 10-m-Zellen ausgerichtet.
vorlage <- rast(xmin = floor(bb["xmin"] / 10) * 10,
                xmax = ceiling(bb["xmax"] / 10) * 10,
                ymin = floor(bb["ymin"] / 10) * 10,
                ymax = ceiling(bb["ymax"] / 10) * 10,
                resolution = 10, crs = "EPSG:32632")
gittergrenze <- st_as_sf(as.polygons(ext(vorlage), crs = crs(vorlage)))
bbox_wgs84 <- as.numeric(st_bbox(st_transform(gittergrenze, 4326)))

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
auswahl <- auswahl[order(auswahl$datum), , drop = FALSE]
stopifnot(nrow(auswahl) == length(jahre), !anyNA(auswahl$datum),
          !anyDuplicated(auswahl$datum),
          setequal(as.integer(format(auswahl$datum, "%Y")), jahre),
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

# Ohne aoi-Argument liefert GetImage das Originalgitter in WGS84 zurück.
# So wird dessen interne gemeinsame Reprojektion aller sechs Bänder umgangen.
# Reflexionen werden bilinear, SCL und dataMask mit nächstem Nachbarn übertragen.
auf_gitter <- function(rohdaten, vorlage) {
  stopifnot(inherits(rohdaten, "SpatRaster"), nlyr(rohdaten) == 6)
  bild <- c(project(rohdaten[[1:4]], vorlage, method = "bilinear"),
            project(rohdaten[[5:6]], vorlage, method = "near"))
  names(bild) <- c("B02", "B03", "B04", "B08", "SCL", "dataMask")
  for (band in c("SCL", "dataMask")) {
    werte <- values(bild[[band]], mat = FALSE)
    erlaubt <- if (band == "SCL") 0:11 else 0:1
    if (any(!is.na(werte) & !werte %in% erlaubt)) {
      stop("Ungültige Klassenwerte im Qualitätsband ", band)
    }
  }
  bild
}

# 6. Erst ein Datum, anschließend die gewählte Reihe --------------------
# Bei jedem Abruf den aktuellen Stand der Auswahl-Datei einlesen.
auswahl <- read.csv("data/auswahl.csv")
auswahl$datum <- as.Date(auswahl$datum)
auswahl <- auswahl[order(auswahl$datum), , drop = FALSE]
stopifnot(nrow(auswahl) == length(jahre), !anyNA(auswahl$datum),
          !anyDuplicated(auswahl$datum),
          setequal(as.integer(format(auswahl$datum, "%Y")), jahre),
          all(auswahl$datum %in% kandidaten$datum))
for (i in seq_len(min(nrow(auswahl), anzahl_abrufe))) {
  tag <- as.character(auswahl$datum[i])
  datei <- file.path(ordner, paste0("s2_", tag, ".tif"))
  manifest_datei <- paste0(datei, ".rds")
  anfrage <- list(datum = tag, aoi = st_as_binary(st_geometry(aoi)),
                  gitter = as.vector(ext(vorlage)), crs = crs(vorlage),
                  aufloesung = res(vorlage), evalscript = evalscript,
                  mosaicking = "leastCC", verfahren = "bilinear/near-v1")
  if (file.exists(datei)) {
    if (!file.exists(manifest_datei) ||
        !identical(readRDS(manifest_datei)$anfrage, anfrage) ||
        !compareGeom(rast(datei)[[1]], vorlage, stopOnError = FALSE)) {
      stop("Vorhandener Abruf passt nicht zu Gebiet/Einstellungen: ", datei,
           ". Alten Ordner data/sentinel archivieren und erneut abrufen.")
    }
    next
  }
  message("Abruf: ", tag)
  rohdaten <- CDSE::GetImage(
    bbox = bbox_wgs84, time_range = tag,
    collection = "sentinel-2-l2a", script = evalscript,
    format = "image/tiff", mosaicking_order = "leastCC",
    resolution = 10, client = client
  )
  bild <- auf_gitter(rohdaten, vorlage)
  writeRaster(bild, datei, overwrite = FALSE)
  saveRDS(list(anfrage = anfrage, abgerufen = Sys.time(),
               CDSE = as.character(packageVersion("CDSE"))), manifest_datei)
}
# Tagesabruf: Überlappende Kacheln können mosaikiert werden.
# Das ausgewählte Datum ist festgelegt, keine einzelne Produkt-ID.
writeLines(capture.output(sessionInfo()), "output/sessionInfo.txt")

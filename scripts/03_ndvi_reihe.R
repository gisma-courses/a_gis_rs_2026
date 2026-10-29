# NDVI-Reihe aus den selbst abgerufenen Sentinel-2-Daten
# Name / Datum: ergänzen
# Eingabe: data/aoi.gpkg, data/auswahl.csv, data/sentinel/s2_DATUM.tif
# Ausgabe: NDVI pro Datum, Rasterreihe, CSV und Verlaufsgrafik
# Start: dasselbe RStudio-Projekt wie beim Abruf.

library(sf)
library(terra)

# 1. Einstellungen und Eingaben ----------------------------------------
aoi <- vect(st_read("data/aoi.gpkg", quiet = TRUE) |> st_transform(32632))
auswahl <- read.csv("data/auswahl.csv")
daten <- sort(as.Date(auswahl$datum))
dateien <- file.path("data/sentinel", paste0("s2_", daten, ".tif"))
if (!all(file.exists(dateien))) stop("Es fehlen Abrufe aus data/auswahl.csv.")
dir.create("output/ndvi", recursive = TRUE, showWarnings = FALSE)

# Arbeitsentscheidung: SCL 4 Vegetation, 5 unbewachsen, 6 Wasser,
# 7 unklassifiziert bleiben erhalten. Schatten, Wolken, Schnee usw. entfallen.
# SCL ist ursprünglich gröber als die 10-m-Bänder und keine perfekte Maske.
scl_behalten <- c(4, 5, 6, 7)
ndvi_liste <- list()

# 2. Einheitliches Gitter prüfen; pro Tag maskieren und NDVI rechnen -----
for (i in seq_along(dateien)) {
  bild <- rast(dateien[i])
  names(bild) <- c("B02", "B03", "B04", "B08", "SCL", "dataMask")
  if (i == 1) vorlage <- bild[[1]]
  if (!compareGeom(vorlage, bild[[1]], stopOnError = FALSE)) {
    stop("Rastergitter unterscheiden sich. Abrufgebiet/Einstellungen prüfen.")
  }
  gueltig <- bild[["dataMask"]] == 1 & bild[["SCL"]] %in% scl_behalten
  nenner <- bild[["B08"]] + bild[["B04"]]
  ndvi <- ifel(gueltig & nenner > 0,
               (bild[["B08"]] - bild[["B04"]]) / nenner, NA)
  ndvi <- mask(ndvi, aoi)
  names(ndvi) <- paste0("NDVI_", format(daten[i], "%Y%m%d"))
  writeRaster(ndvi, file.path("output/ndvi", paste0(names(ndvi), ".tif")),
              overwrite = TRUE)
  ndvi_liste[[i]] <- ndvi
}
reihe <- rast(ndvi_liste)
time(reihe) <- daten
writeRaster(reihe, "output/ndvi_reihe.tif", overwrite = TRUE)
# Explizite Datumszuordnung: GeoTIFF-Zeitinformationen werden nicht von
# allen Programmen interpretiert.
write.csv(data.frame(band = seq_along(daten), datum = daten),
          "output/ndvi_banddaten.csv", row.names = FALSE)

# 3. Zeitverlauf und Datenabdeckung -------------------------------------
innen <- rasterize(aoi, vorlage, field = 1, background = NA)
n_gebiet <- global(!is.na(innen), "sum", na.rm = TRUE)[1, 1]
n_gueltig <- global(!is.na(reihe), "sum", na.rm = TRUE)[, 1]

# Zusätzlich alle Zeitpunkte auf exakt denselben gültigen Pixeln prüfen.
gemeinsam <- sum(!is.na(reihe)) == nlyr(reihe)
reihe_gleich <- mask(reihe, gemeinsam, maskvalues = 0)
n_gemeinsam <- global(gemeinsam, "sum", na.rm = TRUE)[1, 1]

verlauf <- data.frame(
  datum = daten,
  ndvi_mittel = global(reihe, "mean", na.rm = TRUE)[, 1],
  gueltig_prozent = 100 * n_gueltig / n_gebiet,
  ndvi_gleiche_pixel = global(reihe_gleich, "mean", na.rm = TRUE)[, 1]
)
write.csv(verlauf, "output/ndvi_verlauf.csv", row.names = FALSE)
print(verlauf)
cat("An allen Terminen gültige Pixel:", n_gemeinsam, "von", n_gebiet, "\n")

# 4. Gleiche Skala für die Karten; echte Aufnahmedaten auf der Zeitachse --
png("output/ndvi_karten.png", width = 1600, height = 1000)
plot(reihe, range = c(-1, 1))
dev.off()

png("output/ndvi_verlauf.png", width = 1200, height = 800)
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
plot(verlauf$datum, verlauf$ndvi_mittel, type = "b", pch = 19,
     ylim = c(-1, 1), xlab = "Aufnahmedatum", ylab = "Mittlerer NDVI")
lines(verlauf$datum, verlauf$ndvi_gleiche_pixel,
      type = "b", col = "blue", pch = 1)
legend("bottomleft", c("Je Termin gültige Pixel", "An allen Terminen gültig"),
       col = c("black", "blue"), pch = c(19, 1), lty = 1, bty = "n")
plot(verlauf$datum, verlauf$gueltig_prozent, type = "h", lwd = 4,
     ylim = c(0, 100), xlab = "Aufnahmedatum", ylab = "Gültige Pixel (%)")
dev.off()
# Verbindungslinien dienen der Lesbarkeit; zwischen Terminen fehlen Beobachtungen.

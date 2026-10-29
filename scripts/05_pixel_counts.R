# Hausaufgabe WS03: pro Pixel gültige und bewölkte Beobachtungen zählen
# Name / Datum: ergänzen
# Eingabe: exakt die Termine in data/auswahl.csv, keine übrigen TIFF-Dateien.
# Kategorien beziehen sich auf diese ausgewählten Tage, nicht auf das Jahr.

library(sf)
library(terra)
aoi <- vect(st_read("data/aoi.gpkg", quiet = TRUE) |> st_transform(32632))
auswahl <- read.csv("data/auswahl.csv")
daten <- sort(as.Date(auswahl$datum))
stopifnot(length(daten) > 0, !anyNA(daten), !anyDuplicated(daten))
dateien <- file.path("data/sentinel", paste0("s2_", daten, ".tif"))
if (!all(file.exists(dateien))) stop("Ausgewählte Tagesdateien fehlen.")
z <- "output/pixel_counts"
dir.create(z, recursive = TRUE, showWarnings = FALSE)
scl_behalten <- c(4, 5, 6, 7) # Identisch zu Skript 03 halten.

# NA in einer Bedingung wird als FALSE behandelt.
null_eins <- function(x) ifel(is.na(x), 0, ifel(x, 1, 0))

for (i in seq_along(dateien)) {
  x <- rast(dateien[i])
  stopifnot(nlyr(x) == 6)
  names(x) <- c("B02", "B03", "B04", "B08", "SCL", "dataMask")
  if (i == 1) vorlage <- x[[1]]
  if (!compareGeom(vorlage, x[[1]], stopOnError = FALSE)) {
    stop("Unterschiedliche Rastergitter: Abrufe prüfen.")
  }
  scl <- x[["SCL"]]
  da <- null_eins(x[["dataMask"]] == 1 & !is.na(scl) & scl != 0)
  nenner <- x[["B08"]] + x[["B04"]]
  valid <- null_eins(da == 1 & scl %in% scl_behalten & nenner > 0)
  cloudy <- null_eins(da == 1 & scl %in% c(8, 9, 10))
  schatten <- null_eins(da == 1 & scl == 3)
  sonstig <- da - valid - cloudy - schatten
  fehlt <- 1 - da
  tag <- c(valid, cloudy, schatten, sonstig, fehlt)
  names(tag) <- c("n_valid", "n_cloudy", "n_shadow", "n_other", "n_missing")
  if (i == 1) counts <- tag else counts <- counts + tag
}
counts <- mask(counts, aoi)
n_termine <- length(daten)
beobachtet <- n_termine - counts[["n_missing"]]
valid_pct <- 100 * counts[["n_valid"]] / n_termine
cloudy_pct <- ifel(beobachtet > 0,
                  100 * counts[["n_cloudy"]] / beobachtet, NA)
names(valid_pct) <- "valid_prozent_aller_termine"
names(cloudy_pct) <- "cloudy_prozent_beobachteter_termine"

# Die fünf disjunkten Kategorien müssen pro AOI-Pixel N ergeben.
abweichung <- global(abs(sum(counts) - n_termine), "max", na.rm = TRUE)[1, 1]
stopifnot(is.finite(abweichung), abweichung == 0)
writeRaster(counts, file.path(z, "beobachtungs_counts.tif"), overwrite = TRUE)
writeRaster(c(valid_pct, cloudy_pct), file.path(z, "beobachtungs_anteile.tif"),
            overwrite = TRUE)
write.csv(data.frame(datum = daten), file.path(z, "gezaehlte_termine.csv"), row.names = FALSE)
write.csv(freq(counts[["n_valid"]]), file.path(z, "haeufigkeit_valid.csv"), row.names = FALSE)

png(file.path(z, "valid_cloudy_counts.png"), width = 1400, height = 700)
par(mfrow = c(1, 2))
plot(counts[["n_valid"]], range = c(0, n_termine), main = "Gültige Beobachtungen")
plot(counts[["n_cloudy"]], range = c(0, n_termine), main = "Wolken einschließlich Cirrus")
dev.off()
print(global(counts, "mean", na.rm = TRUE))

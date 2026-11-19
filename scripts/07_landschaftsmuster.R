# WS04: Zusammensetzung, Anordnung und Fenstergröße vergleichen.
# Eingabe: data/landbedeckung.tif, kategorisch, metrisch, mindestens 8 km breit/hoch.
# Ausgabe: output/muster/ mit Rastern, Kennwerten und Klassenanteilen.
library(terra)
library(landscapemetrics)

x <- rast("data/landbedeckung.tif")
stopifnot(nlyr(x) == 1, !is.lonlat(x))
v <- values(x, mat = FALSE)
stopifnot(any(is.finite(v)), all(is.na(v) | (is.finite(v) & v == round(v))))
if (min(xmax(x) - xmin(x), ymax(x) - ymin(x)) < 8000) {
  stop("Für die Fenstervergleiche mindestens 8 km Kantenlänge bereitstellen.")
}
z <- "output/muster"
dir.create(z, recursive = TRUE, showWarnings = FALSE)

# Nur gültige Zellwerte umordnen; NoData und Klassenhäufigkeiten bleiben erhalten.
set.seed(42)
y <- x
innen <- which(!is.na(v))
v[innen] <- v[innen][sample.int(length(innen))]
values(y) <- v
varianten <- list(original = x, umgeordnet = y)
mitte <- c((xmin(x) + xmax(x)) / 2, (ymin(x) + ymax(x)) / 2)
for (km in c(2, 4, 8)) {
  h <- km * 500
  varianten[[paste0("fenster_", km, "km")]] <- crop(
    x, ext(mitte[1] - h, mitte[1] + h, mitte[2] - h, mitte[2] + h), snap = "in")
}
kennwerte <- anteile <- list()
for (name in names(varianten)) {
  r <- varianten[[name]]
  f <- freq(r)
  f$anteil <- f$count / sum(f$count)
  f$variante <- name
  anteile[[name]] <- f
  kennwerte[[name]] <- data.frame(
    variante = name, n_pixel = sum(f$count),
    breite_m = xmax(r) - xmin(r), hoehe_m = ymax(r) - ymin(r),
    shdi = lsm_l_shdi(r)$value,
    # Zellbasierte Shannon-Entropie in Bits ist SHDI / log(2).
    h_zellen_bits = -sum(f$anteil * log2(f$anteil)),
    ai = lsm_l_ai(r, directions = 4)$value,
    # ENT verwendet Randhäufigkeiten aus Nachbarschaftspaaren.
    ent_bits = lsm_l_ent(r, neighbourhood = 4, base = "log2")$value,
    relmutinf = lsm_l_relmutinf(r, neighbourhood = 4, base = "log2")$value)
  writeRaster(r, file.path(z, paste0(name, ".tif")), overwrite = TRUE)
}
kennwerte <- do.call(rbind, kennwerte)
write.csv(kennwerte, file.path(z, "kennwerte.csv"), row.names = FALSE)
write.csv(do.call(rbind, anteile), file.path(z, "klassenanteile.csv"), row.names = FALSE)
writeLines(capture.output(sessionInfo()), file.path(z, "sessionInfo.txt"))
print(kennwerte)

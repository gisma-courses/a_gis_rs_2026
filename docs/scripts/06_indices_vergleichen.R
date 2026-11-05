# WS03: NDVI und kNDVI auf drei Vergleichsflächen
# Eingaben: Ergebnisse aus Skript 03; data/vergleichsflaechen.gpkg, Feld typ
# kNDVI = tanh(NDVI^2), RBF-Kernel mit sigma = (NIR + Rot) / 2.
library(sf)
library(terra)

r <- rast("output/ndvi_reihe.tif")
b <- read.csv("output/ndvi_banddaten.csv")
b$datum <- as.Date(b$datum)
stopifnot(nrow(b) == nlyr(r), all(b$band == seq_len(nlyr(r))), !anyNA(b$datum))
f <- st_read("data/vergleichsflaechen.gpkg", quiet = TRUE) |> st_transform(crs(r))
stopifnot("typ" %in% names(f), all(st_is_valid(f)), nrow(f) == 3,
          setequal(f$typ, c("wald", "offenland", "auffaellig")))
f <- f[match(c("wald", "offenland", "auffaellig"), f$typ), ]
dir.create("output/indices", recursive = TRUE, showWarnings = FALSE)

# Ausschließlich dieselben, an allen Terminen gültigen Zellen vergleichen.
gemeinsam <- sum(!is.na(r)) == nlyr(r)
r <- mask(r, gemeinsam, maskvalues = 0)
k <- tanh(r^2)
names(k) <- sub("NDVI", "kNDVI", names(r), fixed = TRUE)
writeRaster(k, "output/indices/kndvi_reihe.tif", overwrite = TRUE)
ergebnisse <- list()
for (i in seq_len(nrow(f))) {
  innen <- rasterize(vect(f[i, ]), r[[1]], field = 1,
                    background = NA, touches = FALSE)
  a <- mask(r, innen)
  ka <- mask(k, innen)
  n <- global(!is.na(a[[1]]), "sum", na.rm = TRUE)[1, 1]
  if (n == 0) stop("Keine gemeinsamen gültigen Zellen für ", f$typ[i],
                   ": Flächenlage, AOI und Bildauswahl prüfen.")
  # Erst pro Pixel transformieren, dann mitteln: tanh(mean(NDVI)^2) wäre etwas anderes.
  ergebnisse[[i]] <- data.frame(
    typ = f$typ[i], datum = b$datum, n_pixel = n,
    ndvi = global(a, "mean", na.rm = TRUE)[, 1],
    kndvi = global(ka, "mean", na.rm = TRUE)[, 1])
}
d <- do.call(rbind, ergebnisse)
write.csv(d, "output/indices/flaechenvergleich.csv", row.names = FALSE)
write.csv(b, "output/indices/banddaten.csv", row.names = FALSE)
png("output/indices/flaechenvergleich.png", width = 1400, height = 800)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
farben <- c("darkgreen", "orange3", "purple")
for (index in c("ndvi", "kndvi")) {
  plot(range(b$datum), if (index == "ndvi") c(-1, 1) else c(0, 1), type = "n",
       xlab = "Aufnahmedatum", ylab = index)
  for (i in seq_len(nrow(f))) {
    z <- d[d$typ == f$typ[i], ]
    lines(z$datum, z[[index]], type = "b", pch = 19, col = farben[i])
  }
  legend("bottomleft", f$typ, col = farben, lty = 1, pch = 19, bty = "n")
}
dev.off()
print(d)

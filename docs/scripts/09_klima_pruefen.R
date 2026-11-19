# WS06: Segmentierungen auf derselben 1-km-Datenbasis beurteilen.
# Eingaben: Ergebnisse aus Skript 08. Ausgabe: output/klima/pruefung/.
library(terra)
library(sf)
library(supercells)
x <- rast("data/temperatur_monate.tif")
stopifnot(nlyr(x) == 12, !is.lonlat(x), all(res(x) == 1000))
z <- "output/klima/pruefung"
dir.create(z, recursive = TRUE, showWarnings = FALSE)

# Vergleichssegmentierung aus einem gröberen Gitter; dieselben Parameter.
grob <- aggregate(x, fact = 2, fun = mean, na.rm = TRUE)
s_grob <- supercells(grob, k = 50, compactness = 1)
s_grob$segment_id <- seq_len(nrow(s_grob))
st_write(s_grob, file.path(z, "k50_c1_2km.gpkg"), delete_dsn = TRUE, quiet = TRUE)
varianten <- c(k50_c1 = "output/klima/k50_c1.gpkg",
               k100_c1 = "output/klima/k100_c1.gpkg",
               k50_c5 = "output/klima/k50_c5.gpkg",
               k50_c1_2km = file.path(z, "k50_c1_2km.gpkg"))
werte <- values(x, mat = TRUE)
# Nur vollständig beobachtete 1-km-Zellen gehen in alle Bewertungen ein.
gueltig <- rowSums(is.finite(werte)) == 12
statistik <- nachbarn <- zusammen <- list()
for (name in names(varianten)) {
  s <- st_read(varianten[[name]], quiet = TRUE) |> st_transform(crs(x))
  ids <- rasterize(vect(s), x[[1]], field = "segment_id", background = NA, touches = FALSE)
  id <- values(ids, mat = FALSE)
  if (any(gueltig & is.na(id))) stop("Segmentierung deckt gültige Klimazellen nicht ab: ", name)
  # Gleiche Flächengewichte, da alle bewerteten Zellen 1 km² groß sind.
  tab <- lapply(sort(unique(id[gueltig])), function(k) {
    a <- werte[gueltig & !is.na(id) & id == k, , drop = FALSE]
    data.frame(segment_id = k, monat = 1:12, n_pixel = nrow(a),
               mittel = colMeans(a),
               sd = if (nrow(a) > 1) apply(a, 2, sd) else rep(NA_real_, 12),
               # Populationsvarianz: ein Segment mit nur einer Zelle hat Streuung null.
               varianz = colMeans(sweep(a, 2, colMeans(a))^2))
  })
  tab <- do.call(rbind, tab)
  tab$variante <- name
  statistik[[name]] <- tab
  zusammen[[name]] <- data.frame(variante = name, n_segmente = length(unique(tab$segment_id)),
                                innere_varianz = weighted.mean(tab$varianz, tab$n_pixel),
                                n_pixel = sum(gueltig))
  # Nachbarn teilen eine Grenzlinie; reine Eckkontakte zählen nicht.
  kontakt <- st_relate(s, pattern = "F***1****")
  paare <- do.call(rbind, lapply(seq_along(kontakt), function(i) {
    j <- kontakt[[i]][kontakt[[i]] > i]
    if (length(j)) data.frame(a = s$segment_id[i], b = s$segment_id[j]) else NULL
  }))
  if (!is.null(paare)) {
    paare$mittlere_abs_differenz <- vapply(seq_len(nrow(paare)), function(i) {
      a <- tab$mittel[tab$segment_id == paare$a[i]]
      b <- tab$mittel[tab$segment_id == paare$b[i]]
      if (length(a) != 12 || length(b) != 12) return(NA_real_)
      mean(abs(a - b))
    }, numeric(1))
    paare$variante <- name
    nachbarn[[name]] <- paare
  }
  # Drei verbundene Segmente auswählen: ein Segment mit zwei Grenznachbarn.
  kandidaten <- which(lengths(kontakt) >= 2)
  if (length(kandidaten)) {
    i <- kandidaten[1]
    auswahl <- s$segment_id[c(i, kontakt[[i]][1:2])]
    png(file.path(z, paste0(name, "_jahresgaenge.png")), width = 1000, height = 700)
    grenzen <- range(tab$mittel[tab$segment_id %in% auswahl])
    plot(1:12, rep(NA_real_, 12), type = "n", ylim = grenzen + c(-0.1, 0.1),
         xlab = "Monat", ylab = "Temperatur (Grad Celsius)", main = name)
    for (j in seq_along(auswahl)) {
      a <- tab[tab$segment_id == auswahl[j], ]
      if (nrow(a)) lines(a$monat, a$mittel, type = "b", col = j, pch = j)
    }
    legend("topleft", paste("Segment", auswahl), col = 1:3, lty = 1, pch = 1:3, bty = "n")
    dev.off()
  }
  writeRaster(ids, file.path(z, paste0(name, "_ids.tif")), overwrite = TRUE)
}
write.csv(do.call(rbind, statistik), file.path(z, "segment_monate.csv"), row.names = FALSE)
write.csv(do.call(rbind, nachbarn), file.path(z, "nachbarschaft.csv"), row.names = FALSE)
write.csv(do.call(rbind, zusammen), file.path(z, "variantenvergleich.csv"), row.names = FALSE)
print(do.call(rbind, zusammen))

# Synthetic inputs; no WorldClim download. Run from the repository root.
repo <- normalizePath('.')
library(terra)
library(sf)
library(geodata)
library(supercells)
library(landscapemetrics)
tmp <- tempfile('block2-', tmpdir = '/tmp')
dir.create(tmp)
setwd(tmp)
dir.create('data')
r <- rast(nrows = 100, ncols = 100, xmin = 450000, xmax = 460000,
          ymin = 5590000, ymax = 5600000, crs = 'EPSG:25832')
xy <- crds(r)
values(r) <- ifelse(xy[, 1] < 455000, 1, ifelse(xy[, 2] < 5595000, 2, 3))
values(r)[1:10] <- NA
writeRaster(r, 'data/landbedeckung.tif')
source(file.path(repo, 'scripts/07_landschaftsmuster.R'))
t <- read.csv('output/muster/kennwerte.csv')
stopifnot(nrow(t) == 5, abs(t$shdi[1] - t$shdi[2]) < 1e-12,
          abs(t$h_zellen_bits[1] - t$h_zellen_bits[2]) < 1e-12,
          t$ai[1] > t$ai[2])

r <- rast(nrows = 100, ncols = 100, xmin = 450000, xmax = 550000,
          ymin = 5500000, ymax = 5600000, crs = 'EPSG:25832')
xy <- crds(r)
fixture <- rast(lapply(1:12, function(m) {
  a <- r
  values(a) <- 10 + 8 * sin((m - 4) * pi / 6) +
    (xy[, 1] - 500000) / 30000 + sin((xy[, 2] - 5550000) / 20000)
  a
}))
names(fixture) <- paste0('wc2.1_30s_tavg_', 1:12)
aoi <- st_as_sf(as.polygons(ext(r), crs = crs(r)))
st_write(aoi, 'data/klima_aoi.gpkg', quiet = TRUE)
# Replace only the network boundary; all preparation and segmentation code runs.
assignInNamespace('worldclim_country', function(...) fixture, ns = 'geodata')
source(file.path(repo, 'scripts/08_klima_segmentieren.R'))
stopifnot(nlyr(rast('data/temperatur_monate.tif')) == 12,
          all(res(rast('data/temperatur_monate.tif')) == 1000))
source(file.path(repo, 'scripts/09_klima_pruefen.R'))
t <- read.csv('output/klima/pruefung/variantenvergleich.csv')
stopifnot(nrow(t) == 4, length(unique(t$n_pixel)) == 1,
          all(is.finite(t$innere_varianz)), all(t$innere_varianz >= 0),
          length(list.files('output/klima/pruefung', pattern = 'jahresgaenge.png')) == 4)
cat('PASS: pattern permutation, windows, climate preparation, segmentation, shared evaluation and annual curves\n')
cat('Test outputs:', tmp, '\n')

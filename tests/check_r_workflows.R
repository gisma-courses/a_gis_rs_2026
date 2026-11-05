suppressPackageStartupMessages({library(sf);library(terra)})
repo <- normalizePath('.')
for (f in list.files(file.path(repo,'scripts'), pattern='\\.[Rr]$',full.names=TRUE)) parse(f)
# Reprojection keeps categorical values, uses a fixed metric grid.
p <- parse(file.path(repo,'scripts/02_sentinel_abruf.R'))
for (e in p) if (is.call(e) && identical(e[[1]],as.name('<-')) && identical(e[[2]],as.name('auf_gitter'))) eval(e)
r <- rast(nrows=10,ncols=10,xmin=8.5,xmax=8.51,ymin=50.5,ymax=50.51,crs='EPSG:4326');values(r)<-0.2
scl<-r;values(scl)<-rep(rep(c(4,8),each=5),10)
x<-c(r,r,r,r,scl,r*0+1)
target<-project(r,'EPSG:32632',res=10)
y<-auf_gitter(x,target)
stopifnot(compareGeom(y[[1]],target),all(na.omit(values(y[[5]])) %in% c(4,8)),all(na.omit(values(y[[6]]))==1))
# Original end-to-end regression: former 400% coverage becomes 100%.
tmp<-tempfile('course-test-',tmpdir='/tmp');dir.create(tmp);setwd(tmp)
dir.create('data');dir.create('data/sentinel');dir.create('output')
aoi<-st_sf(geometry=st_as_sfc('POLYGON ((9 9, 31 9, 31 31, 9 31, 9 9))',crs=32632));st_write(aoi,'data/aoi.gpkg',quiet=TRUE)
dates<-as.Date(paste0(2018:2022,'-07-01'));write.csv(data.frame(datum=dates),'data/auswahl.csv',row.names=FALSE)
r<-rast(nrows=4,ncols=4,xmin=0,xmax=40,ymin=0,ymax=40,crs='EPSG:32632');values(r)<-0.1
x<-c(r,r,r,r*5,r*0+4,r*0+1)
for(d in as.character(dates))writeRaster(x,paste0('data/sentinel/s2_',d,'.tif'))
source(file.path(repo,'scripts/03_ndvi_reihe.R'))
stopifnot(all(read.csv('output/ndvi_verlauf.csv')$gueltig_prozent==100),n_gemeinsam==4)
source(file.path(repo,'scripts/05_pixel_counts.R'))
stopifnot(global(!is.na(counts[[1]]),'sum',na.rm=TRUE)[1,1]==4)
source(file.path(repo,'scripts/04_zeitreihe_addon.R'))
stopifnot(abs(steigung)<1e-10)
# Three zones, nonlinear index computed pixelwise.
f<-st_sf(typ=c('wald','offenland','auffaellig'), geometry=st_as_sfc(c(
 'POLYGON ((10 10,20 10,20 20,10 20,10 10))',
 'POLYGON ((20 10,30 10,30 20,20 20,20 10))',
 'POLYGON ((10 20,20 20,20 30,10 30,10 20))'),crs=32632))
st_write(f,'data/vergleichsflaechen.gpkg',quiet=TRUE)
source(file.path(repo,'scripts/06_indices_vergleichen.R'))
stopifnot(nrow(d)==15,all(d$n_pixel==1),all(abs(d$kndvi-tanh((2/3)^2))<1e-6))
# All five count classes remain mutually exclusive, including missing data.
v<-values(x);v[,5]<-rep(c(4,8,3,11,0,7,9,10),2);v[,6]<-1;values(x)<-v
for(dat in as.character(dates))writeRaster(x,paste0('data/sentinel/s2_',dat,'.tif'),overwrite=TRUE)
source(file.path(repo,'scripts/05_pixel_counts.R'))
stopifnot(global(abs(sum(counts)-5),'max',na.rm=TRUE)[1,1]==0)
# Old interpolated SCL is rejected, rather than counted as an unknown class.
v<-values(x);v[,5]<-4.6;values(x)<-v;writeRaster(x,paste0('data/sentinel/s2_',dates[1],'.tif'),overwrite=TRUE)
e<-try(source(file.path(repo,'scripts/03_ndvi_reihe.R')),silent=TRUE);stopifnot(inherits(e,'try-error'))
# A cache hit is accepted only for the same AOI and settings; no HTTP call is made.
ordner <- "data/cache"
dir.create(ordner)
auswahl <- data.frame(datum=dates[1])
anzahl_abrufe <- 1
aoi <- st_read("data/aoi.gpkg", quiet=TRUE)
vorlage <- x[[1]]
evalscript <- "test-request"
loop <- p[[which(vapply(p, function(e) is.call(e) && identical(e[[1]], as.name("for")), logical(1)))]]
i <- 1
for (e in as.list(loop[[4]])[2:5]) eval(e)
writeRaster(x, datei)
saveRDS(list(anfrage=anfrage), manifest_datei)
eval(loop)
aoi <- st_buffer(aoi, 1)
e <- try(eval(loop), silent=TRUE)
stopifnot(inherits(e, "try-error"), grepl("Vorhandener Abruf passt nicht", as.character(e)))
unlink(manifest_datei)
e <- try(eval(loop), silent=TRUE)
stopifnot(inherits(e, "try-error"), grepl("Vorhandener Abruf passt nicht", as.character(e)))
cat('PASS: syntax, fixed grid, categorical resampling, AOI coverage, counts, trend, kNDVI, corrupt-SCL rejection, AOI cache isolation\n')
cat('Test outputs:',tmp,'\n')

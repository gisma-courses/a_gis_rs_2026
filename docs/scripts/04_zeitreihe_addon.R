# Add-on: Sommerliche NDVI-Reihe beschreiben und ihre Empfindlichkeit prüfen
# Name / Datum: ergänzen
# Voraussetzung: 03_ndvi_reihe.R erfolgreich ausgeführt.
# Eingaben werden neu geladen; keine Objekte aus früheren Sitzungen nötig.
# Zeitraum: ausgewählte Sommerbeobachtungen, keine Monatsmittel.

library(terra)

# 1. Daten und Zeitzuordnung --------------------------------------------
r <- rast("output/ndvi_reihe.tif")
b <- read.csv("output/ndvi_banddaten.csv")
b$datum <- as.Date(b$datum)
stopifnot(nrow(b) == nlyr(r), all(b$band == seq_len(nlyr(r))),
          !anyNA(b$datum), !anyDuplicated(b$datum), nlyr(r) >= 3)
o <- order(b$datum)
r <- r[[o]]
datum <- b$datum[o]
z <- "output/zeitreihe"
dir.create(z, recursive = TRUE, showWarnings = FALSE)

# Für jeden Termin dieselben Pixel: räumliche Unterstützung konstant halten.
gemeinsam <- sum(!is.na(r)) == nlyr(r)
n <- global(gemeinsam, "sum", na.rm = TRUE)[1, 1]
if (!is.finite(n) || n == 0) stop("Keine gemeinsamen gültigen Pixel: Bildauswahl prüfen.")
r <- mask(r, gemeinsam, maskvalues = 0)
writeRaster(gemeinsam, file.path(z, "gemeinsame_pixel.tif"), overwrite = TRUE)

# 2. Beobachtungsreihe --------------------------------------------------
d <- data.frame(
  datum = datum,
  tag_im_jahr = as.integer(format(datum, "%j")),
  jahre_seit_start = as.numeric(datum - min(datum)) / 365.25,
  ndvi = global(r, "mean", na.rm = TRUE)[, 1]
)
d$delta_vorher <- c(NA_real_, diff(d$ndvi))
d$delta_pro_jahr <- c(NA_real_, diff(d$ndvi) / diff(d$jahre_seit_start))
d$abweichung_start <- d$ndvi - d$ndvi[1]
# Differenzen in NDVI-Einheiten, keine Prozentänderungen.

# 3. Beschreibender linearer Trend --------------------------------------
modell <- lm(ndvi ~ jahre_seit_start, data = d)
d$trend <- as.numeric(predict(modell))
d$residuum <- d$ndvi - d$trend
steigung <- unname(coef(modell)[2])

# Einen Termin auslassen: Wird die Steigung von einem Einzeltermin getragen?
# Dies ist eine Sensitivitätsprüfung, keine unabhängige Validierung.
sensitiv <- data.frame(
  ausgelassen = datum,
  steigung = vapply(seq_len(nrow(d)), function(i) {
    unname(coef(lm(ndvi ~ jahre_seit_start, data = d[-i, ]))[2])
  }, numeric(1))
)
write.csv(d, file.path(z, "zeitreihe_kennwerte.csv"), row.names = FALSE)
write.csv(sensitiv, file.path(z, "sensitivitaet.csv"), row.names = FALSE)
write.csv(data.frame(
  gemeinsame_pixel = n,
  steigung_ndvi_pro_jahr = steigung,
  sensitivitaet_min = min(sensitiv$steigung),
  sensitivitaet_max = max(sensitiv$steigung)
), file.path(z, "zusammenfassung.csv"), row.names = FALSE)
print(d)
print(sensitiv)

# 4. Wo liegen Zu- und Abnahmen zwischen erster und letzter Aufnahme? ----
differenz <- r[[nlyr(r)]] - r[[1]]
names(differenz) <- "NDVI_letzter_minus_erster_Termin"
writeRaster(differenz, file.path(z, "differenz_ende_start.tif"), overwrite = TRUE)

# 5. Eine Grafik für Verlauf, Änderungen und Sensitivität ----------------
png(file.path(z, "zeitreihenanalyse.png"), width = 1400, height = 1500)
par(mfrow = c(3, 1), mar = c(4, 5, 3, 1))
plot(d$datum, d$ndvi, type = "b", pch = 19,
     ylim = range(c(d$ndvi, d$trend)) + c(-0.02, 0.02),
     xlab = "Aufnahmedatum", ylab = "Mittlerer NDVI",
     main = sprintf("Sommerbeobachtungen; Steigung %.4f NDVI/Jahr", steigung))
lines(d$datum, d$trend, col = "blue", lwd = 2)
legend("bottomleft", c("Beobachtungen", "Lineare Beschreibung"),
       col = c("black", "blue"), lty = 1, bty = "n")

plot(d$datum[-1], d$delta_vorher[-1], type = "h", lwd = 5,
     ylim = range(c(0, d$delta_vorher), na.rm = TRUE) + c(-0.01, 0.01),
     xlab = "Enddatum des jeweiligen Vergleichs", ylab = "NDVI-Differenz",
     main = "Änderung gegenüber der vorherigen Aufnahme")
abline(h = 0, col = "grey")

plot(sensitiv$ausgelassen, sensitiv$steigung, pch = 19,
     ylim = range(c(0, steigung, sensitiv$steigung)) + c(-0.001, 0.001),
     xlab = "Ausgelassener Aufnahmetermin", ylab = "Steigung (NDVI/Jahr)",
     main = "Wie stark beeinflussen einzelne Termine die Steigung?")
abline(h = steigung, col = "blue", lty = 2)
abline(h = 0, col = "grey")
dev.off()

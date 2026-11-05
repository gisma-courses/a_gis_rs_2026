Die verbindlichen Quellen sind `index.qmd`, `pages/`, `slides/` und `scripts/`.

Nach Änderungen zuerst die Downloadpakete und anschließend die Website erneuern:

```sh
python3 tools/build_downloads.py
python3 tests/check_materials.py
Rscript tests/check_r_workflows.R
Rscript tests/check_warmup.R
quarto render
python3 tests/check_materials.py --site docs
```

Die R-Prüfung verwendet synthetische Daten unter `/tmp`; sie führt keine CDSE-Anmeldung und keinen Satellitendownload aus. Sie prüft das Zielgitter, kategoriales Resampling, AOI-Abdeckung, Pixelzählung, den beschreibenden Trend und den NDVI/kNDVI-Flächenvergleich. Für einen vollständigen Liveabruf werden persönliche CDSE-Zugangsdaten benötigt. OTB muss für Worksheet 11 zusätzlich installiert sein.

`assets/Archive.zip` erhält seine Eingangsdaten, übernimmt aber die aktuellen Warmup-Skripte und die aktuelle Startanleitung. Die nummerierten Sentinel-ZIPs bleiben als kompatible Kopien erhalten. Quellenkopien in `assets/advanced_gi_kursueberblick/` und `sentinel_ws02 (2)/` werden ebenfalls aus `pages/ws/` erzeugt.

#!/usr/bin/env python3
"""Refresh downloadable archives and retained source mirrors from course sources."""
from pathlib import Path
import io
import zipfile

ROOT = Path(__file__).resolve().parents[1]

def write_member(z, name, content):
    # Stable archive metadata avoids binary changes when sources are unchanged.
    info = zipfile.ZipInfo(name, date_time=(2026, 1, 1, 0, 0, 0))
    info.external_attr = (0o40755 if name.endswith('/') else 0o100644) << 16
    z.writestr(info, content, compress_type=zipfile.ZIP_DEFLATED)

def refresh(path, replacements):
    """Keep bundled input data; replace source files with current versions."""
    with zipfile.ZipFile(path) as old:
        entries = [(i.filename, old.read(i)) for i in old.infolist()
                   if i.filename not in replacements]
    with zipfile.ZipFile(path, 'w', compression=zipfile.ZIP_DEFLATED) as new:
        for name, content in entries:
            write_member(new, name, content)
        for name, content in replacements.items():
            write_member(new, name, content)

def contents(paths):
    return {str(p.relative_to(ROOT)): p.read_bytes() for p in paths}

warmup_paths = [ROOT / 'scripts' / n for n in (
    '00_install_packages.R', '01_run_microclimate_beginner.R',
    'microclimate_warmup_minimal.R', 'microclimate_beginner_workflow_SIMPLE.r', 'miniflow.R')]
warmup = contents(warmup_paths)
warmup['README_START_HERE.txt'] = (ROOT/'assets/README_START_HERE.txt').read_bytes()
refresh(ROOT/'assets/Archive.zip', warmup)

# The separate synthetic beginner project retains its own data and instructions.
prefix = 'microclimate_beginner_rproj/'
refresh(ROOT/'assets/microclimate_beginner_rproj.zip', {
    prefix+'01_run_microclimate_beginner.R': (ROOT/'scripts/01_run_microclimate_beginner.R').read_bytes(),
    prefix+'00_install_packages.R': (ROOT/'scripts/00_install_packages.R').read_bytes(),
})

sheets = sorted((ROOT/'pages/ws').glob('*.qmd'))
for p in sheets:
    target = ROOT/'assets/advanced_gi_kursueberblick/worksheets'/p.name
    target.write_bytes(p.read_bytes())
# Retained extraction folder uses exactly the same sources as the website.
for name in ('ws-02.qmd', 'ws-03.qmd', 'ws-02-zusatz.qmd'):
    (ROOT/'sentinel_ws02 (2)/pages/ws'/name).write_bytes((ROOT/'pages/ws'/name).read_bytes())

source_paths = [ROOT/'index.qmd', *sheets, ROOT/'pages/microclimate_warmup_reader.qmd',
                *sorted((ROOT/'slides').glob('*.qmd')), *sorted((ROOT/'scripts').glob('*.R'))]
with zipfile.ZipFile(ROOT/'assets/advanced_gi_kursueberblick.zip', 'w', zipfile.ZIP_DEFLATED) as z:
    for name, content in contents(source_paths).items():
        write_member(z, name, content)
    write_member(z, 'README.txt', 'Quellmaterialien des Kurses. Für Website-Build und Datenpakete das vollständige Repository verwenden.\n')

sentinel_paths = [ROOT/'pages/ws'/n for n in ('ws-02.qmd','ws-03.qmd','ws-02-zusatz.qmd','ws-10.qmd')]
sentinel_paths += [ROOT/'scripts'/n for n in ('02_sentinel_abruf.R','03_ndvi_reihe.R',
                   '04_zeitreihe_addon.R','05_pixel_counts.R','06_indices_vergleichen.R')]
buffer = io.BytesIO()
with zipfile.ZipFile(buffer, 'w', zipfile.ZIP_DEFLATED) as z:
    for name, content in contents(sentinel_paths).items():
        write_member(z, name, content)
    write_member(z, 'README.md', '# Sentinel-Unterlagen\n\nIm Projekt aus Worksheet 01 weiterarbeiten. '
               'Skript 02 lädt Aufnahmen; 03 berechnet NDVI; 06 vergleicht NDVI/kNDVI. '
               'Die Zusatzaufgaben verwenden 05 und 04. '
               'AOI und Zugangsdaten werden nicht mitgeliefert.\n')
for name in ('sentinel_ws02.zip','sentinel_ws02 (1).zip','sentinel_ws02 (2).zip'):
    (ROOT/'assets'/name).write_bytes(buffer.getvalue())
print('Downloadpakete und Quellenkopien aktualisiert.')

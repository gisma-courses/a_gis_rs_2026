#!/usr/bin/env python3
"""Check course navigation, nonempty worksheets and generated download contents."""
from pathlib import Path
import re
import zipfile

ROOT = Path(__file__).resolve().parents[1]
errors = []
source_files = [ROOT/'index.qmd', *ROOT.glob('pages/**/*.qmd'), *ROOT.glob('slides/*.qmd')]
for path in source_files:
    text = path.read_text()
    for number, line in enumerate(text.splitlines(), 1):
        for link in re.findall(r'\]\(([^)]+)\)', line):
            target = link.split('#')[0].split('?')[0]
            if target and not re.match(r'\w+://|mailto:', target):
                if not (path.parent/target).exists():
                    errors.append(f'{path.relative_to(ROOT)}:{number}: missing {target}')
    if 'Ersetze den Abschnitt' in text or 'Die SuS' in text:
        errors.append(f'{path}: editorial residue')
for n in range(1, 13):
    p = ROOT/f'pages/ws/ws-{n:02}.qmd'
    body = p.read_text().split('---', 2)[2].strip()
    if not body:
        errors.append(f'{p}: empty worksheet')
config = (ROOT/'_quarto.yml').read_text()
for target in re.findall(r'href: (\S+)', config):
    if not (ROOT/target).exists():
        errors.append(f'_quarto.yml: missing {target}')
for archive in ['assets/Archive.zip', 'assets/sentinel_ws02.zip']:
    with zipfile.ZipFile(ROOT/archive) as z:
        for name in z.namelist():
            if name.startswith(('scripts/', 'pages/')) and (ROOT/name).is_file():
                if z.read(name) != (ROOT/name).read_bytes():
                    errors.append(f'{archive}: stale {name}')
for p in (ROOT/'assets/advanced_gi_kursueberblick/worksheets').glob('*.qmd'):
    if p.read_bytes() != (ROOT/'pages/ws'/p.name).read_bytes():
        errors.append(f'{p}: stale source mirror')
if errors:
    raise SystemExit('\n'.join(errors))
print('PASS: local links, navigation, 12 worksheets, editorial cleanup, download sources')

# Run after `quarto render`: check actual output URLs and resource copies.
if __name__ == '__main__':
    import argparse
    from html.parser import HTMLParser
    from urllib.parse import unquote, urlsplit

    parser = argparse.ArgumentParser()
    parser.add_argument('--site', type=Path)
    args = parser.parse_args()
    if args.site:
        site = args.site.resolve()

        class Links(HTMLParser):
            def __init__(self):
                super().__init__()
                self.targets = []

            def handle_starttag(self, tag, attrs):
                for key, value in attrs:
                    if value and (tag, key) in {('a', 'href'), ('img', 'src'),
                                               ('script', 'src'), ('link', 'href')}:
                        self.targets.append(value)

        errors = []
        pages = [site/'index.html', *site.glob('pages/**/*.html'), *site.glob('slides/*.html')]
        for path in pages:
            links = Links()
            links.feed(path.read_text())
            for link in links.targets:
                u = urlsplit(link)
                if u.scheme or u.netloc or not u.path:
                    continue
                target = site/unquote(u.path).lstrip('/') if u.path.startswith('/') else path.parent/unquote(u.path)
                if not target.exists():
                    errors.append(f'{path.relative_to(site)}: missing {link}')
        for p in (ROOT/'scripts').glob('*.R'):
            target = site/'scripts'/p.name
            if not target.exists() or target.read_bytes() != p.read_bytes():
                errors.append(f'{target}: missing or stale script download')
        if (site/'assets/Archive.zip').read_bytes() != (ROOT/'assets/Archive.zip').read_bytes():
            errors.append('Archive.zip: stale site resource')
        if errors:
            raise SystemExit('\n'.join(errors))
        print(f'PASS: {len(pages)} rendered pages, output URLs and script/ZIP downloads')

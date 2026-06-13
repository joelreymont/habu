#!/usr/bin/env python3
"""trust-lint: TRUST is the checker's escape hatch — `s" name" s" effect" TRUST`
declares a word's stack effect WITHOUT checking its body. Every trusted word is
part of the trusted base, a soundness cliff: a wrong declared effect lets the
checker certify programs built on a lie. So the set must stay small, audited, and
TESTED. This lint pins the manifest (TRUSTED.md) to the code: every TRUST site in
src/ must have a manifest row, and every manifest row must cite at least one test.
"""
import re, sys, pathlib

SITE = re.compile(r's"\s+([^"]+?)"\s+s"\s+[^"]*"\s+TRUST')   # name = group 1

# --- collect TRUST sites in src/ ---
sites = {}                                                   # name -> "file:line"
for f in sorted(pathlib.Path("src").rglob("*.f")):
    for n, line in enumerate(f.read_text().splitlines(), 1):
        code = line.split("\\", 1)[0]                        # strip line comment
        m = SITE.search(code)
        if m:
            sites[m.group(1)] = f"{f}:{n}"

# --- parse TRUSTED.md table: rows are | Word | Effect | Reason | Tests | ... | ---
manifest = {}                                                # word -> tests cell
mpath = pathlib.Path("TRUSTED.md")
if not mpath.exists():
    print("trust-lint: TRUSTED.md missing — the trust manifest is required")
    sys.exit(1)
for line in mpath.read_text().splitlines():
    if not line.lstrip().startswith("|"):
        continue
    cells = [c.strip() for c in line.strip().strip("|").split("|")]
    if len(cells) < 4:
        continue
    word = cells[0].strip("`").strip()
    if not word or word.lower() == "word" or set(word) <= set("-: "):
        continue                                             # header / separator
    manifest[word] = cells[3]                                # Tests column

# --- cross-check ---
bad = 0
for name, site in sorted(sites.items()):
    if name not in manifest:
        print(f"UNMANIFESTED {site}: `{name}` is TRUSTed but has no TRUSTED.md row")
        bad += 1
    elif not manifest[name].strip():
        print(f"UNTESTED {site}: `{name}` has an empty Tests cell in TRUSTED.md")
        bad += 1
for word in sorted(manifest):
    if word not in sites:
        print(f"STALE-ROW TRUSTED.md: `{word}` has a row but no TRUST site in src/")
        bad += 1

print(f"trust-lint: {len(sites)} TRUST site(s), {len(manifest)} manifest row(s), {bad} finding(s)")
sys.exit(1 if bad else 0)

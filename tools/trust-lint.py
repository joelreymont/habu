#!/usr/bin/env python3
"""trust-lint: TRUST is the checker's escape hatch — `s" name" s" effect" TRUST`
declares a word's stack effect WITHOUT checking its body. Every trusted word is
part of the trusted base, a soundness cliff: a wrong declared effect lets the
checker certify programs built on a lie. So the set must stay small, audited, and
TESTED. This lint pins the manifest (TRUSTED.md) to the code: every TRUST site in
src/ must have a manifest row with the same effect string, and every manifest row
must cite at least one test.
"""
import re, sys, pathlib

SITE = re.compile(r's"\s+([^"]+?)"\s+s"\s+([^"]*?)"\s+TRUST')

def norm_effect(s):
    return " ".join(s.strip().split()).lower()

# --- collect TRUST sites in src/ ---
sites = {}                                                   # name -> (effect, site)
bad = 0
for f in sorted(pathlib.Path("src").rglob("*.f")):
    for n, line in enumerate(f.read_text().splitlines(), 1):
        code = line.split("\\", 1)[0]                        # strip line comment
        m = SITE.search(code)
        if m:
            name, effect = m.group(1), m.group(2)
            site = f"{f}:{n}"
            if name in sites:
                print(f"DUPLICATE-TRUST {site}: `{name}` already trusted at {sites[name][1]}")
                bad += 1
            sites[name] = (effect, site)

# --- parse TRUSTED.md table: rows are | Word | Effect | Reason | Tests | ... | ---
manifest = {}                                                # word -> (effect, tests)
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
    if word in manifest:
        print(f"DUPLICATE-ROW TRUSTED.md: `{word}` appears more than once")
        bad += 1
    manifest[word] = (cells[1].strip("`").strip(), cells[3])

# --- cross-check ---
for name, (effect, site) in sorted(sites.items()):
    if name not in manifest:
        print(f"UNMANIFESTED {site}: `{name}` is TRUSTed but has no TRUSTED.md row")
        bad += 1
    elif norm_effect(effect) != norm_effect(manifest[name][0]):
        print(f"EFFECT-DRIFT {site}: `{name}` code effect `{effect}` != TRUSTED.md `{manifest[name][0]}`")
        bad += 1
    elif not manifest[name][1].strip():
        print(f"UNTESTED {site}: `{name}` has an empty Tests cell in TRUSTED.md")
        bad += 1
for word in sorted(manifest):
    if word not in sites:
        print(f"STALE-ROW TRUSTED.md: `{word}` has a row but no TRUST site in src/")
        bad += 1

print(f"trust-lint: {len(sites)} TRUST site(s), {len(manifest)} manifest row(s), {bad} finding(s)")
sys.exit(1 if bad else 0)

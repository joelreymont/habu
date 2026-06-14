#!/usr/bin/env python3
"""stale-status-lint: the self-check counts live in ONE place, STATUS.md. A
count-shaped string anywhere else goes stale the next time the checker certifies
a word (it has been 783 -> 860 -> 890), so this fails the gate if one reappears
outside STATUS.md and LESSONS.md (the historical log, where past counts are the
record). Replace the number with a pointer to STATUS.md instead.
"""
import datetime as _dt
import re, sys, pathlib

ALLOWED = {"STATUS.md", "LESSONS.md", "tools/stale-status-lint.py"}

# count-shaped: a `NNN/0/0` verdict triple, or a real `NNN certified` count
# (3+ digits — so the verdict-code text "1 certified / 0 rejected" is not a hit).
PATS = [
    re.compile(r"\b\d{3,}/\d+/\d+\b"),
    re.compile(r"\b\d{3,}\s+certified\b"),
    re.compile(r"\b\d{3,}\s+uncheckable\b"),
]
SKIP = ("/.jj/", "/.git/", "/.dots/")   # vcs + dot-CLI storage, not project docs

root = pathlib.Path(".")
bad = 0
status = pathlib.Path("STATUS.md").read_text()
match = re.search(r"^Last verified:\s*(\d{4}-\d{2}-\d{2})\s*$", status, re.M)
today = _dt.date.today().isoformat()
if not match:
    print("STALE-STATUS STATUS.md: missing `Last verified: YYYY-MM-DD`")
    bad += 1
elif match.group(1) != today:
    print(f"STALE-STATUS STATUS.md: Last verified is {match.group(1)}, expected {today}")
    bad += 1
for p in sorted(root.glob("**/*.md")):
    rel = p.as_posix()
    if rel in ALLOWED or any(s in "/" + rel for s in SKIP):
        continue
    for n, line in enumerate(p.read_text().splitlines(), 1):
        for pat in PATS:
            if pat.search(line):
                print(f"STALE-STATUS {rel}:{n}: count-shaped string — "
                      f"point to STATUS.md instead of quoting a number")
                bad += 1
                break
print(f"stale-status-lint: {bad} finding(s)")
sys.exit(1 if bad else 0)

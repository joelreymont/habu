---
title: dot on re-quotes created-at
status: open
priority: 2
issue-type: task
created-at: "2026-08-19T12:08:51.184052+02:00"
---

CLI bug (effstore-2, 2026-08-19): dot on rewrote created-at: "2026-08-18T..." as created-at: "\"2026-08-18T...\"" - it re-quotes an already-quoted value. Roughly half the active dots in the tree carry the corrupted form (earlier sightings noted it as a quirk; it is a bug). Fix the CLI's frontmatter writer to parse-then-serialize instead of wrapping; sweep the corrupted leaves in the same change. Until fixed, repair by hand after every dot on.

CORPUS SWEPT 2026-08-21 (tracker GC), so the CLI fix now has a clean baseline
and its own regression is easy to state. Measured before the sweep: 361 of 2044
leaves carried extra layers, and the damage COMPOUNDS - each `dot on` wraps the
value again, so the depths ran 210 leaves at two layers, 142 at three, 7 at
four and 2 at five. Stripping exactly one layer would therefore have left 151
leaves still corrupt; the sweep decoded each value to its plain ISO-8601
timestamp and re-emitted one layer, which is the same parse-then-serialize this
dot asks the CLI to do. Every one of the 2041 leaves now sits at exactly one
layer, and running the sweep a second time produced a byte-identical diff.

What this leaves for the CLI fix: the sweep repaired data, not the writer. The
next `dot on` will start the compounding again. The regression to add with the
fix is a round trip - write a leaf, run `dot on` and `dot off` against it
repeatedly, and assert `created-at` is byte-identical every time, because a
single transition looks harmless and only repetition exposes the wrap.

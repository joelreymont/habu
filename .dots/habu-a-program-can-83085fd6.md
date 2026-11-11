---
title: "A program can store into a DYNAMIC-BUFFER's NAME#base and NAME#ctl cells"
status: active
priority: 2
issue-type: task
created-at: "2026-11-11T17:38:32.541216+02:00"
---

Problem (read, not probed; from the owner-columns design 2026-11-11): the storage definers lay NAME#base and NAME#ctl as ordinary words and no rule in checker.f or verify-source.f was found that refuses a program's store into them, so '1000000 N-TAG#ctl !' would defeat the bounds check (dynamic-storage.f ADDRESS :145-153). Related: habu-checked-src-can-d80dc074, habu-user-src-arms-bc2ab900. Acceptance: a probe shows whether checked source outside the definer can name or store into the two cells; if it can, the cells are private to the definer's generated words and a rejected program proves it. Files: src/core/layout-buffer.f, src/core/checker.f. Verify: the rejected program in every mode. Depends: none. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-ctl (hunt, brief .out/briefs/hunt-buffer-cells-load-admits.md).

Probed 2026-11-11 (engine carl-arena3/bin/hb, probes ~/.cache/tmp/carl-abuf-probe/q5-same.f, q5-other.f, q5-known.f): `--load` admits `1000000 TB#ctl !` and `SPOT TB#base !` at top level and in a body, from the buffer's own package and from another (`DBQ:TB#ctl`): rc 0, prints 1000000. `tools/check.f` (--all-errors and --verify-only) refuses the same source: E-UNDEFINED-TOP-LEVEL at TB#ctl, E-UNDEFINED in the bodies, rc 70. So load and check disagree. Not run: an index after the store.

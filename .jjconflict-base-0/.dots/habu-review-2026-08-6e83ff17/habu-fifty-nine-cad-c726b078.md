---
title: fifty-nine CAD-NUM reopen shims cite a closed dot
status: open
priority: 1
issue-type: task
created-at: "2026-08-22T22:38:25.924468+02:00"
---

Problem: lib/cad-num-types.f:11-17 states no public raw mint or role->n projection, yet 59 files reopen 'package CAD-NUM public : X>N ... INDEX>N ;' to export the private cast under a local name (lib/float.f:31-34, process-env.f:35-39, object.f:18-22, ptx/ad.f:349-353, examples/string-regex.f:197-200, maki/lower/ew.f:59-61, mm.f:84-86, red.f:76-78, launch.f:65-67, golden.f:38-40, cad.f:121-127, +48), all citing 'Retire with TVK-RAW (habu-nominal-storage-raw-a3430ef2)' which closed 2026-07-15. This is the forwarding shim AGENTS.md forbids, 59 times, and makes the private wordlist public by convention. Acceptance: publish INDEX>N / BYTE-LEN>N / BYTE-OFF>N / ITEM-COUNT>N once in CAD-NUM (or drop the role-returning STR:/VEC: surface whose every consumer erases the role), delete all 59 blocks; package-diff-lint green; a lint row refuses a reopen of CAD-NUM outside its own files. Files: lib/cad-num-types.f and the 59 sites. Verify: rg '^\s*package CAD-NUM' counts 1; full gate. Depends: none. Ownership: CAD-NUM. Claim: unassigned.

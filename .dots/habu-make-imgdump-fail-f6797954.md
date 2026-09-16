---
title: Make imgdump fail closed without a snapshot trailer
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T11:59:50.347266+03:00"
---

Problem: tools/imgdump.f --pc and the plain dump are wrong for a baked engine that has no snapshot trailer (the current engines: --snap reports no-snapshot): FIND-DICT's longest-run heuristic locks onto a 92-record false positive at offset 0x103e4 and every --pc answer is then wrong rather than an error (found while profiling the build, 2026-09-16; the profile mapped addresses by walking the live dictionary instead). Acceptance: without a trailer imgdump refuses by name (or reads the dictionary through the image's real layout cells), --pc on a known word address of a baked engine returns that word, and a fixture pins both. Files: tools/imgdump.f, its test. Verify: the fixture through bin/hb against bin/hb itself. Depends: none. Ownership: hazel line. Claim: unassigned.

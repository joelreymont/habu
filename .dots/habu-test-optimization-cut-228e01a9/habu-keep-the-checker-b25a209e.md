---
title: "Keep the checker's record sound after a duplicate throw"
status: open
priority: 3
issue-type: task
created-at: "2026-10-01T12:05:45.693651+02:00"
---

Problem: when a duplicate definition throws inside VERIFY (src/habu/verify-source.f / src/core/checker.f), the checker's record of the redefined word is left wrong: in one shared scope a later clean word calling it got a phantom E-CAP-TRUSTED (review 109 fixture $HOME/.cache/tmp/kestrel-r4-rev109/fx/dup; r4-expand commit 1090caa9). r4-expand 09b1a06e now ends an all-errors check at a duplicate, so check.f no longer reaches it, but any session that continues after the throw (the REPL, a CATCH around INCLUDED/EVALUATE) still can. Found by the r4-expand c7 fix worker. Acceptance: reproduce through a path that continues after the duplicate throw (REPL via a pty or a CATCH around an include), seen failing first; fix the layer that leaves the record half-written so the record after the throw equals the record before it; or show with evidence that no path continues after that throw and record the rule in a comment at the throw site. Files: src/habu/verify-source.f or src/core/checker.f and a test through the real load path. Baked: rebuild, g1 == g2, two-generation build.

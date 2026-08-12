---
title: A declaration inside an unchecked window is name-only
status: open
priority: 2
issue-type: task
created-at: "2026-08-11T22:30:00.000000+02:00"
---

Found by the primsweep stage-2 lane (2026-08-11): 0 set-check zeroes HOOK-CELL, and EM-COMPILE-PUBLISH branches to the sig-less path when it is zero - so a TRUSTED: (or any declared-signature definer) inside a 0 set-check window publishes the WORD and registers NO effect. It fails closed downstream (uncheckable, not certified) but looks exactly like a broken fixture, and it silently converted a whole suite's 19 declarations into name-only stubs. Probe the shape: should the definer REFUSE (or the checker reject) a declared-signature definition while the hook is zeroed, so the degradation is loud at the line instead of two suites away? Mind the legitimate uses: enumerate what the tree actually defines inside 0 set-check windows before ruling; if refusal is wrong for some class, the answer may be a lint. Files: src/habu/habu2.f (EM-COMPILE-PUBLISH), src/core/checker.f or the lint. Depends: none.

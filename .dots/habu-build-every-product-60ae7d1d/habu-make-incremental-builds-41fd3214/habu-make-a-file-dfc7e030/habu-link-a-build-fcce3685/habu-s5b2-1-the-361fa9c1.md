---
title: "S5b2-1: the checker checks a build apart from hb (CHECK:OPEN, CHECK:CLOSE)"
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T23:20:37.908380+02:00"
---

Problem: one symbol table and one type registry serve hb and every build (probes a-e, /tmp/jerry-big/s5b2-probes.md): two builds defining one name with different effects, or a product redefining a word hb has (STR=, WL:GLOBAL?), refuse E-DUPLICATE-DEFINITION 78 at src/core/check-hook.f CHECKER-CERT-DUP?; after the close the build's names and STRUCTURE families stay in hb's checker. Joel 2026-11-05 (DECISIONS 'the checker during a build'): CHECK:OPEN ( -- ) and CHECK:CLOSE ( -- ), called only by LINKER:WITH's open and close. Design /tmp/jerry-big/design-s5b2.md: while a build is open each checker store's pointer names a fresh arena holding the store's prefix; CLOSE frees them and puts hb's pointers and cells back byte for byte. Acceptance: probes a-e pass as cases in test/linker.f; hb's stores unchanged after a build (HOST-CHECK). Files: src/core/checker.f, src/core/type-family.f, src/habu/linker.f, test/linker.f. Verify: bin/hb --load test/run.f -- linker, on an hb rebuilt from the tree. Depends: S5b1b landed; lane R (LSP-CHECK) landed so the package name CHECK is free; Joel's approval of the design's new public words CHECK:STORE+ and CHECK:CELL+ (not approved; Joel away 2026-11-05). Ownership: jerry. Claim: unassigned.

Finding (S5b1b review, 2026-11-05): test/linker.f does not notice when OUTER:WITHOUT-PACKAGE's PKG-RECOVER skips the checker's resync (`NCOMP-DISPATCH:DECL-PKG-RESYNC-OFF PKG-NOTIFY`, src/habu/packages.f:266); probe /Users/joel/.cache/tmp/rv-fold2.ykow/no-resync2.log passed rc 0 with the resync removed. Whether a missing resync shows anywhere in production is not known. S5b2-1 changes how the checker sees a build's open and close, so its cases should include one that fails without that resync.

---
title: A build under tier 1 dies loading the build tool
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T15:14:53.217694+02:00"
---

Problem: with a first file holding '1 set-tier', 'bin/hb --load tier1.f tools/hb-build.f -- --target wasm t.f -o t.wasm' prints 'ncomp: cannot compile START' and 'hb: uncaught throw code -8402' (E-NFEED-SPAN) while the build tool loads, on installed master 4aad814c; which START is not found (src/lib/crypto/sha1.f:134 is a candidate). docs/design.md 'Machinery being replaced' keeps tier 1 only for paths it still owns, and docs/architecture.md:148-151 still lists Wasm modules among them though a Wasm build compiles on tier 0. Acceptance: the cause is named as path:line; then either tier 1 compiles the build tool or set-tier refuses tier 1 where a build runs, and architecture.md names only the paths tier 1 still owns. Depends: none. Claim: unassigned.

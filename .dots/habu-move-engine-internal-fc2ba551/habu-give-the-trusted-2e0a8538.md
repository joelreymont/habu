---
title: "Give the TRUSTED:-wrapped primitives private rows"
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T23:00:56.840265+01:00"
---

Problem: primitives reached only through a TRUSTED: wrapper have a global row: trust-sig! (src/habu/interpreter.f), set-check and set-preflight (src/core/check-hook.f), rep-mark (src/core/checker.f), c2-init-stow, c2-records-stow and c2-invoke (src/lib/c2-memory/owner-runtime.f), does-patch (src/compiler/native/dict.f), def-occ-select (checker.f RECORD-LINK-OCC, in no package), and int-mark, whose callers (src/core/sumtype.f, src/lib/xml/c2.f) mark words already in private sections. Acceptance: each has a private row in each package that calls it and no global row; each wrapper is a plain definition in that package; int-mark and its two calls are deleted; the engine builds itself (hb2 == hb3) and the suites of the touched files pass. Depends: none. Claim: unassigned.

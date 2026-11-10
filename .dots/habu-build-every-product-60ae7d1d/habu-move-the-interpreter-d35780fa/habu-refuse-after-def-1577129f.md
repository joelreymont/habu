---
title: Refuse ; after def-open at tier 0
status: closed
priority: 3
issue-type: task
created-at: "2026-10-04T04:41:56.203496+03:00"
closed-at: "2026-11-10T22:35:26.490841+02:00"
close-reason: "obsolete: tier 0 is now T0 and the ordinary colon is the def-open path: src/habu/definers.f:358 DEF-RECORD opens with def-open, :660 DEF-SEMI? hands the body to the compiler then def-close; on the audit's arm64 engine (some commits behind master) ': A ( -- n ) 1 ; : B ( -- n ) A 1+ ; B .' printed 2, rc 0; the JIT-era probe is gone"
---

Problem (lane 573, on 7c03e870): at tier 0, `def-open` followed by `;` crashes the engine (exit 134, SIGSEGV) when the body calls a word: DEF-OPEN (src/habu/habu2.f ~:4112) sets up no return-address frame, and while `def-close` refuses tier 0, `;` does not. Probe $HOME/.cache/tmp/kestrel-jerry-ctlflow/pr/d0*. Fix: one rule at the responsible layer: either `;` refuses a def-open'ed definition at tier 0 as def-close does, by a named code, or DEF-OPEN builds the frame `;` expects; say which and why. Acceptance: the probe is refused with a named record (rc 70) or compiles and runs correctly, never crashes; seen crashing first; baked: rebuild, g1 == g2 with .names, two-generation build. After: 7a137417.

Census 2026-11-04: an acceptance case of its parent; fix it in the code that stays (named above), never in the machinery the parent replaces.

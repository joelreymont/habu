---
title: "C2c: check each certified effect's facts text"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-06T20:16:59.583198+02:00"
closed-at: "2026-11-07T04:02:42.606987+02:00"
close-reason: "implemented, reviewed, every child closed, landed with C2c (master after b05b406c): gate chain /tmp/jc.7p fixpoint at gen2 rc 0 (real 445 s), full suite 293 of 293 PASS rc 0 (/tmp/jt.yi/suite.log); lsp-boundary's stale 28-variable fixture widened to 276 (kkwqyszy)"
---

Problem: design r2 D5 (/tmp/jerry-big/design-facts-r2.md): the writer parses the facts text it wrote and refuses the definition when the parse differs from what the body proved (the lead's choice, 2026-11-06). C2b1's second fold makes the text say every row kind (the 177 outer rows D3 would have normalized included), so nothing is normalized. Acceptance: render.f FACTS-SAME? renders a published record with REND-FACTS, parses it in facts mode in the word's package and compares with E-CONTENT-SAME?, restoring the effect store on every exit; check-hook.f HOOK refuses a definition this check published whose text differs ('hook: facts text differs: NAME: ( TEXT )'); hb builds and the probe shows every live row same. Nothing writes entry bytes 32-39 (C2c2). Brief: /tmp/jerry-big/brief-c2c.md. Files: src/core/render.f, src/core/check-hook.f. Verify: candidate build within 5 minutes; probe; bin/hb --load test/run.f -- engine checker-soundness diag-position. Depends: C2b1 (habu-c2b-read-facts-f698adb6). Ownership: those files. Claim: agent=worker workspace=.jj-ws.noindex/jerry-c2c.

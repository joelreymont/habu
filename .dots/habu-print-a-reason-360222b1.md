---
title: "Print a reason rejection's code in its text"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T09:25:46.217346+02:00"
---

Problem: src/core/render.f DIAG-PROSE (:1759-1814) leads a rejection's text with its code for most kinds (E-BAD-LOCAL-SHAPE, E-UNDEFINED, E-LINEAR-LOCAL, ...), but a reason rejection (RF-REASON, the 64 MD- reasons) prints only 'at TOKEN reason': measured on the installed hb, ': A ( n -- ) 1 = if 1 then ;' prints "habu: in a: at 'then' the if arm changes the stack's depth ..." with no code, while a locals rejection prints 'E-BAD-LOCAL-SHAPE habu: in c: ...'. MD-CODE$ (:1064) has each reason's code and reaches only the packet (:1563). An agent reading stderr cannot match a reason rejection by code; tests match its prose instead (test/engine-suite.f CBAD-QL- cases, test/c2-memory-rejections.f :41, found by the a2a lane of habu-give-a-quotation-304dcb8d). Acceptance: every rejection's text carries its code in one place and form; the tests that match a reason's prose for want of a code match the code. Files: src/core/render.f, the tests that assert exact rejection text. Verify: full suite (shared checker diagnostics). Depends: none. Ownership: render.f DIAG-PROSE. Claim: unassigned.

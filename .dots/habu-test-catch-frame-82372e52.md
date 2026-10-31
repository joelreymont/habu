---
title: Test catch-frame restores at tier 1
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T13:28:26.218771+02:00"
---

Problem: at tier 1 (arm64 1 set-tier; x86-64 boots at tier 1) the catch-frame THROW-* helpers never touch RSP-CELL or LOOPSP-CELL: 99 >r 7 throw is movz x0,99; movz x0,7 then the call, and ?do keeps its index in registers, so test/catch-frame.f checks nothing there in either TRUSTED: or : form (lane tc3 review, 2026-10-31). Acceptance: a catch-frame case that fails at tier 1 when a caught throw does not restore the caller's return or loop stack. Files: test/catch-frame.f. Verify: catch-frame row at tier 0 and tier 1. Depends: none. Ownership: test/catch-frame.f. Claim: unassigned.

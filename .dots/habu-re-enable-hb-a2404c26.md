---
title: "Re-enable hb's name stripping"
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T07:59:56.932704+02:00"
---

Problem: Joel 2026-11-07: 'disable name stripping if it's slowing you down and make a dot to re-enable it later'. The normal hb build seals (src/core/internal-mark.f IMK-SEAL :152 marks unrowed names DNAME-INT; src/habu/aot-capture.f ACAP-NAMED? :754 drops them), so a new public word a source-loaded tool (the linker) calls needs a PPRIM row and a probe needs a whitebox build (C5a-1: an extra build and a 4.7-min hunt). It is being disabled: the normal build takes the whitebox path (tools/native-build-core.f TARGET-ENGINE-UNSEAL :220, CAPTURE-WITH-NAMES :781). Acceptance: the product build strips private names by the privacy decision (DECISIONS 'privacy': private sections, no owner rows or marks), and every test the disabling turned off runs again. Files: src/core/internal-mark.f, src/habu/aot-capture.f, tools/native-build-core.f, the disabled tests. Verify: full suite. Depends: the privacy design. Ownership: unassigned. Claim: unassigned.

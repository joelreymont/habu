---
title: "Waiting case: a Wasm address past the build's data is refused by a bare WLINK code"
status: open
priority: 4
issue-type: task
created-at: "2026-11-10T19:18:46.637009+02:00"
---

Problem (feature review B, F6, not reproduced): src/habu/link-copy.f DATA-OFFSET accepts an address below SPACE-SIZE while src/arch/wasm/link.f FINAL refuses a data target past the image's length with E-WLINK-UNRESOLVED, which hb-build prints as a throw code blamed on app-end.f, naming no word. Probe: `create END : MAIN ( -- ) END drop ;` (an address at DP) builds and runs to 0; no checked program was found that holds a literal address past DP. Acceptance: once a program reaches it, ADDR-SITE+ and ROW+ refuse by name. Files: src/arch/wasm/link-module.f. Verify: that program's build. Depends: none. Ownership: none. Claim: unassigned.

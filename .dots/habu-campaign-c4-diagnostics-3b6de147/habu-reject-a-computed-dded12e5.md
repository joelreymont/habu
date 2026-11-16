---
title: Reject a computed 0 throw in a built program
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T14:26:19.954279+01:00"
---

Problem: a program whose MAIN throws a computed 0 ends silently with status 0 once built (arm64 and Wasm), while `hb --load` stops it with E-THROW-ZERO and 67: the built entry's catch answers 0 for a return and for a 0 throw alike (`rc 0= if exit then`, src/habu/app-end.f / main.f entry). Found by the W218 feature review. Acceptance: a built program that throws a computed 0 prints the E-THROW-ZERO record and exits 67 on arm64 and Wasm, as under --load; a test holds it. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-dd1.

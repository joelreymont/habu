---
title: Specify Windows x64 and ARM64 native profiles
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T22:32:29.013488+03:00"
---

Problem: PA-r2 §1.1 requires two Windows compiler hosts and §15.3-15.4 rests on a Windows/COM design that does not exist yet (Joel, 2026-10-03). Acceptance: parked until that design exists in docs/ and a Windows lane exists; then L07-L11 and N12 over the x86 backend, PE/DLL/unwind/TLS providers and a Windows OS seam. Windows stays 'required, unqualified' in docs/portability.md until then (decided by the Fable review). Files: src/abi/ (new), src/os/windows* (new), native/x64ir.f by agreement with the Intel lane. Verify: native self-build on each Windows host. Depends: the Windows/COM design; habu-link-native-fragments-5949ec30, habu-own-host-svcs-b1368bbe. Ownership: unassigned until a lane exists. Lane: dave, parked. Claim: unassigned.

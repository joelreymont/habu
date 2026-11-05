---
title: "Correct native-runtime.f's float and fmt row claim"
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T12:06:26.026985+02:00"
---

Problem: src/habu/native-runtime.f:8-12 says src/lib/float.f and src/lib/fmt.f are rows by choice because habu2.f requires fmt for number text, but no row in native-runtime.f requires either and bin/hb.map names neither (rg, 2026-11-05). Acceptance: either the comment drops the claim, or the rows exist because a carried word needs them (find habu2.f's number-text path first). Files: src/habu/native-runtime.f. Verify: rg float.f/fmt.f in bin/hb.map matches the comment; tools/manifest-lint.f rc 0. Depends: none. Ownership: native-runtime.f header. Claim: unassigned.

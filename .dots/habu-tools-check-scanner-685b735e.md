---
title: "tools/check: scanner lacks package-block support"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T10:39:19.844247+02:00\""
---

Found by the JSON family-hint work (2026-07-13): tools/check-core.f's CHK dispatch (~line 834) handles TYPEFAMILY/SUMTYPE/ENUM/PRODUCT/DEFTYPE but has NO 'package' block support - foreign-package families cannot be declared through the check CLI path, so its JSON contract can only pin global families (the foreign qualified 'family' hint form is unreachable via tools/check.f). Fix: add package/end-;package block recognition to the scanner mirroring the native loader's package handling (and verify-source's), then extend tools/check-test-lib.f with a cross-package family fixture + a foreign-family JSON pin. Tools lane; medium.

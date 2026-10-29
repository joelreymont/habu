---
title: End a T0 signature at its first )
status: active
priority: 1
issue-type: task
created-at: "2026-10-29T04:30:09.908424+02:00"
---

Workspace: jerry-char, commit tmkrmrrs on ryxpxvxv. T0 SKIP-HEAD ended the signature only at a standalone ) token, so ': BUMP ( n -- n) 1 + ;' compiled empty under T0 with no error; every generated field accessor (src/core/sumtype.f TDECL-ADDR-FIELD-WORD, '( ptr F -- ptr T)') was a no-op, and tender test/docx-write.f (-6102) and test/corpus.f (-3907) failed under T0 (A5). Fix: end at the first ) byte as the engine's ( does; row SIG-TIGHT in test/t0-install.f. Both tender tests pass under T0 after it.

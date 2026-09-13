---
title: Give the native build origin callback a declared callable effect
status: active
priority: 1
issue-type: task
created-at: "2026-09-13T22:01:25.123527+03:00"
---

Fresh native B1 from 051fc42a (SHA256
e11a2779a50ee55f77ed5bbedfcbd83438d5836e7ce876bc81440484c72fe0f1) rejects
`tools/native-build.f` at BUILD's inline `[: code-origin ;]` with
E-NELAB-QUOT (-8651). NATIVE-BUILD:RUN is resolved by evaluate only after
required returns, so no visible consumer supplies the inline calling convention.

The driver now declares checked ORIGIN `( n n -- n )` and passes its typed tick
at the existing dynamic boundary. Tier 1 and EXECUTABLE-BUILD:WITH are unchanged;
no compiler or additional unchecked boundary is needed.

The same B1 running `--load test/native-build-entry.f` passes: the production
tool and its native driver load fully and reach the exact exit-74 missing-output
diagnostic. The pre-fix actual tool load exits 67 with -8651. Reserved-name lint
passes. Independent review and the full tracked B2 build remain with integration.

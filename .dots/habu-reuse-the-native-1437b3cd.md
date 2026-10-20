---
title: Reuse the native writer FILEs
status: active
priority: 2
issue-type: task
created-at: "2026-10-15T14:53:03.367193+03:00"
---

Engine writer source loads after target capture and remains uncached; measured native-build baseline spent 21.218s loading it versus 0.153s emitting. Use the existing FILE loader for this late stage with the fresh target owner and loader. Preserve donor versus target NATIVE-LAYOUT tables and capture the target before loading writer code. Existing BUILD-CORE closes its FILE scope before FINISH, so a second-stage lifetime must be explicit; do not leave active FILE callbacks in the captured image. Extend the real writer edit E2E before implementation to prove unchanged writer hits, changed writer recompilation, runnable products and cached/cold executable plus map equality. No preloading donor-bound writer or separate image framework. Root owns integrated native gates and independent review.

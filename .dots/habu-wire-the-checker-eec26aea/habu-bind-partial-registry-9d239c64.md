---
title: Bind partial registry imports to exact prefix contents
status: active
priority: 1
issue-type: task
created-at: "2026-09-13T22:08:28.696695+03:00"
---

Confirmed before the repair: independently replay rpi-prefix with FIELD value n, capture suffix FIELD value rpi-prefix, roll back, replay the prefix with FIELD value r at identical counts, then REG-AOT-INSTALL accepts the captured suffix. Safe current-source control/log: build/payload/registry-prefix-identity.f and .log. No invalid memory access executed. D01 protects reused deltas but does not bind a fresh import to its pre-capture prefix. The unaccepted v8 partial format now carries every store through its closed end while retaining base/delta-count rows; validate all canonical prefix bytes before publishing, preserve live derived cells and copy only delta rows. Exactly TF.TAILNEXT and SV.CTOR-SYM are canonicalized. Empty deltas still carry prefix identity because signature graphs can reference existing families. Incoming/producer schemas refuse dynamic SCH-CON codes in the carried prefix too until canonical constructor identities are implemented. Durable RI-PREFIX-IDENTITY in test/aot-registry-identity-child.f proves equal-count foreign prefix refusal with all published bytes/counts/indexes unchanged and rebuilt identical-prefix admission. Await independent review and integration; full persistent owner format is separate.

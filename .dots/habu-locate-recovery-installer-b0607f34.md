---
title: Locate recovery installer window pointer
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T09:18:27.362858+03:00"
---

Documented build-fixpoint-refresh --all on reviewed96efb124 passes stage compiler fixpoint but fails aot-chain capture: pre-window declared cells holding a window address=1, refill=0, then an unnamed load-time installer and E-BUILD-STATUS -2802. Native product is not emitted. Identify the exact cell and installer ownership/lifetime before changing capture or retention; preserve refusal for genuine cross-window pointers and do not add a blanket allowlist. Check whether the existing public compiler install provides the ordinary native-build bootstrap route independently. Acceptance for this task is a corrected responsible boundary with a real recovery artifact consumer, independent review and relevant capture/recovery checks. Keep separate from native FILE performance claims.

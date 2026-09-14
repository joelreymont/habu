---
title: Carry the checker-owner guard in the cold compiler closure
status: open
priority: 2
issue-type: task
created-at: "2026-09-14T03:34:37.746232+03:00"
---

The current empty native writer lets tools/aot-chain-capture.f reach its real 7,766-record compiler window. After admitting the legitimate 3,635,865-byte portable effect pool, capture refuses FIELD at blob offset 56,720: its callee VALIDATE was required by aot-arm before WINDOW-OPEN and lies in the excluded prelude, while the booting cold prefix has no corresponding owner. Preserve the genuine source dependency in the target closure at its owner; do not export or model an excluded dependency. Exact reduction and logs: /tmp/cedar-chain-payload-probe.f and /tmp/cedar-chain-payload-grown.{out,err}. Deferred while the separate dynamic pool storage leaf is completed; current chain migration remains unlanded.

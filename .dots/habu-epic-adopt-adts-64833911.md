---
title: "Epic: adopt ADTs across maki"
status: open
priority: 2
issue-type: task
created-at: "2026-07-03T23:36:48.966746+02:00"
---

Goal dot: types must be used extensively in maki. After TFAM 9/10 land: option/result in maki host APIs (error paths, lookup returns); enum families for opcodes/modes; after TFAM 15: PTX IR nodes as products; after TFAM 16 boxed policy: recursive IR/autograd-tape/ONNX-graph ADTs by value (until then typed ptr + arena). Prioritize boxed policy inside TFAM 16 by maki need. Convenience gaps to watch: no deriving (eq/hash) in v1, no layout-polymorphic params (see capability dot). Success: maki suite green with ADT-typed public APIs, no new trust rows.

---
title: "PRODUCER: producer-id registry + wire codec"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-17T15:33:59.950451+02:00\""
---

Per-family leg of plan 23.9 foreign-id contract (676d5a7b): new owner package PRODUCER, registry-intern origin, version-independent identity; NEEDS-DECISION (engineering, resolve in-dot): what canonically identifies a producer (name? name+kind? content key of a producer descriptor?) - derive from how the plan's provenance sections consume producer identity. Publish constructor + refinements + wire codec pair with tests. Files: new maki producer owner file, focused test, FILEMAP. Ownership: V2 artifact id codecs.

Claim: agent=idfam2 workspace=.jj-ws/fable-idfam2 (tri-dot lane; disjoint new owner files)

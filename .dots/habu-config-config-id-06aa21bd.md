---
title: "CONFIG: config-id registry + wire codec"
status: open
priority: 2
issue-type: task
created-at: "2026-07-17T15:33:59.955550+02:00"
---

Per-family leg of plan 23.9 foreign-id contract (676d5a7b): new owner package CONFIG, registry-intern origin; NEEDS-DECISION (engineering, resolve in-dot): which config facts are canonical for identity (the plan's target/config/numeric fact split is the guide). Publish constructor + refinements + wire codec pair with tests. Files: new maki config owner file, focused test, FILEMAP. Ownership: V2 artifact id codecs.

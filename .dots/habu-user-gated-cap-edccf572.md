---
title: "USER-GATED: CAP capability-id vocabulary + codec"
status: open
priority: 2
issue-type: task
created-at: "2026-07-17T15:33:59.960725+02:00"
---

Per-family leg of plan 23.9 foreign-id contract (676d5a7b): new owner package CAP with a CLOSED capability vocabulary (8-byte code wire class). USER/PRODUCT DECISION REQUIRED before implementation: the initial capability vocabulary content (which capabilities exist as envelope-recordable identities - e.g. device-launch, fs-write, network, spawn; the V2 capability-and-budget enforcement dot habu-v2-capability-and-0970a96d is the consumer to reconcile with). After the decision: constructor + refinements + codec + tests are mechanical. Ownership: V2 artifact id codecs (user-gated).

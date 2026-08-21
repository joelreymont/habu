---
title: Build shared compiler substrate
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:53:09.575827+02:00"
---

Full context: design sections 5-6 and PLAN.md shared-substrate items 1-18 require one immutable, validated, deterministic substrate for all compiler dialects. Required result: IDs, policies, context, storage, sources, interning, schemas, tables, builder/freeze, verification, canonical codec, pass witnesses, facade, and package protection. Acceptance: malformed modules reject with named diagnostics; canonical equivalents encode identically; no frozen mutation or public raw cast resolves.

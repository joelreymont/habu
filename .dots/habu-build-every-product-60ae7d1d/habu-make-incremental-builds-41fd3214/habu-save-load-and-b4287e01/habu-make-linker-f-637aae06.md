---
title: "Make linker.f's SEEN linear in loads"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T15:06:36.296725+02:00"
---

Problem: SEEN (src/habu/linker.f:587) scans every earlier load in LOADS for each new one, quadratic in a build's loads, and hb's own files now build through the linker. Acceptance: SEEN answers through a table keyed by path (or the loader's own registry), constant time per load; the file-objects test unchanged. Depends: none. Claim: unassigned, owner carl.

---
title: Delete WRITES-BYTES and WRITES-ARG
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T11:26:07.108338+02:00"
blocks:
  - habu-delete-the-store-fd3e8182
---

Problem: after the store guard's deletion (habu-delete-the-store-fd3e8182) WRITES-BYTES and WRITES-ARG declare FFI writes nothing reads; about 60 files call them, Zen and Maki included. Acceptance: once an engine without the guard is installed, each consumer's owner has dropped its calls and both words and every Habu call are deleted. Depends: habu-delete-the-store-fd3e8182. Claim: unassigned.

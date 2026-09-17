---
title: forty-eight one-line public aliases should be EXPORT
status: closed
priority: 3
issue-type: task
created-at: "2026-08-22T22:38:25.949786+02:00"
closed-at: "2026-09-16T14:34:50.545369+03:00"
close-reason: "superseded by habu-campaign-c3-the-a2477c89: Still true in part: one-line public forwarders remain where EXPORT says the same thing at no cost"
---

Problem: lib/task.f:671-719 (15), ffi-abi.f:769-830 (21), object-link.f:713-777 (25), object.f:451-467 (7), string.f:285-291 (7) define ': X ( ... ) X ;' forwarders in public sections; docs/forth.md:398-416 documents EXPORT NAME (no body, zero cost) and the engine has it (habu2.f:1952). Acceptance: EXPORT or define once in public; zero forwarders; package lint green. Files: as listed. Verify: lib tests. Depends: none. Ownership: lib packages. Claim: unassigned.

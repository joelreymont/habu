---
title: Preserve source invalidator ownership in capture
status: active
priority: 2
issue-type: task
created-at: "2026-10-15T15:38:40.808047+03:00"
---

Independent Astra review of 31a58fda found NUNIT-CAPTURE RUN-OWNED replaces a legal top-level CODE-RECLAIM:INVALIDATE! installation with the old callback and throws new 7243. Public xref.f only forbids installation during a pending definition. The throw also masks an original nonzero RUN-BODY error. Preserve legal source callback changes and primary failure while capture prunes call/address sites at the actual reclamation floor. Use the existing reclamation ownership boundary without a new user-visible refusal or a general subscriber framework. Revise the existing real-load reclaim E2E before the production fix: legal replacement remains installed, receives subsequent reclaim, and a primary throw is unchanged. Preserve compaction/frame offsets and chaining. Independent focused Astra follow-up required.

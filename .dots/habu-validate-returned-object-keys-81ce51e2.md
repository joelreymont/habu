---
title: Validate cached content against the requested object key
status: closed
priority: 1
issue-type: bug
created-at: "2026-09-14T12:30:00Z"
close-reason: "Compare the computed digest against PATH!'s preserved requested key; the aliased returned-key regression now passes."
---

OBJSTORE:STORE returns its internal KEY-BUF span. LOAD previously recomputed the loaded record's digest into KEY-BUF and then compared against the caller's key pointer. If the caller retained STORE's returned span, that comparison was tautological and accepted another valid record under the wrong content-addressed filename.

The focused LOAD-RETURNED-KEY regression stores an object, replaces its file with a different valid object, then loads using the original returned key span. Before the comparison fix, the optimized object-cache fixture exited 1 at assertion 14 because LOAD returned successfully. LOAD now compares against the requested key already preserved by PATH! in NAME-BUF. The same fixture exits 0, with the existing invalid-key, missing-file, invalid-schema and wrong-digest refusals intact. This is an adjacent baseline defect found while removing fixed payload buffers, not a claim that dynamic allocation introduced it.

---
title: Replace stdin closure audits with one real bootstrap check
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-08-23T18:41:29.859754+02:00\\\"\""
closed-at: "2026-09-16T14:34:48.766136+03:00"
close-reason: "superseded by habu-campaign-c1-finish-1f129a00: (merge target habu-put-the-recovery-4dabaab9 superseded) The audits it asked to delete are already gone; its surviving half is the single scheduled end-to-end recovery check"
---

Delete stdin-closure lint, bootstrap shell parser, mirror audit duplication, and their fixtures. Retain one end-to-end HABU_BOOTSTRAP_CHECK_ONLY recovery test.

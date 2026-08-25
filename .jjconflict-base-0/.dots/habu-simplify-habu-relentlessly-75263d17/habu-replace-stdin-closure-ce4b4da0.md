---
title: Replace stdin closure audits with one real bootstrap check
status: active
priority: 1
issue-type: task
created-at: "\"2026-08-23T18:41:29.859754+02:00\""
---

Delete stdin-closure lint, bootstrap shell parser, mirror audit duplication, and their fixtures. Retain one end-to-end HABU_BOOTSTRAP_CHECK_ONLY recovery test.

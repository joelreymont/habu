---
title: "Refuse what check.f's report buffers cannot hold"
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T10:48:54.259015+02:00"
---

Problem (r4-lintcap, 650af471): check.f's report stages die instead of giving a named refusal when their fixed buffers are smaller than what check.f accepts: tools/lint/json-writer.f:35,40 dies rc 76 at its 16 KiB packet limit (a 20 KB name); tools/check-all-errors-core.f:134 dies when its 32 KiB --all-errors report buffer fills; the standalone diag-origin CLI keeps its own 256 KiB read cap (tools/diag-origin-core.f:11,340, DO-FILE-CAP) while check.f's origin pass now takes CHK-SRC-CAP bytes. Since 650af471 these dies leave no temp root, but the user gets a prose die, no record. Acceptance: each limit is either sized from what check.f accepts (and say why that bound holds) or refused by name with a located/JSON record and a documented rc in prose and --json-errors; the diag-origin CLI takes the same source cap as check.f; cases seen to fail first through tools/check-test-lib.f (and tools/diag-origin-test.f for the CLI). Files: tools/lint/json-writer.f, tools/check-all-errors-core.f, tools/diag-origin-core.f, the tests. Base: after 650af471 lands.

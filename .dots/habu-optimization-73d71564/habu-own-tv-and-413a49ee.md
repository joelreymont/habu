---
title: Own TV and SEEN scratch mappings
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-09-25T10:20:32.617944+02:00\\\"\""
closed-at: "2026-09-25T11:39:29.929318+02:00"
close-reason: Reviewed TV/SEEN mapped ownership accepted. Focused engine/type/diagnostic/window/registry checks, native fixpoint and 490 suites pass. TV removes 122784 additional DATA bytes; combined Maki is 21702368 bytes and passes routing/negotiation with identical exports. Native is 2906359 bytes; no isolated file saving claimed. Evidence ~/.cache/tmp/habu-opt-round3/{tv,integrated}.
---

Replace eleven 1280-cell TV boot planes (112640 B) and 1280-cell SEEN boot store (10240 B): 122880 B potential static reservations, not measured file savings. TV owns one mmap split into eleven capacity-sized planes; SEEN owns another. Preserve encoded zero and UNBOUND sentinels, full old-capacity copy, declared-count reserves, lazy restored access, and capture-only release. Failure modes: negative or overflowing demand, mmap refusal, munmap refusal and cleanup error, stale borrowed pointers after growth, stale high-water after capture, and uninitialized grown tails. Rework engine fixture to cross an honest capacity. Acceptance: focused existing E2E paths and full gate after shared build lane opens; integration/review/push by parent.

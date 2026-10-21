---
title: Publish DYNAMIC-BUFFER growth before unmapping
status: closed
priority: 3
issue-type: task
created-at: "2026-10-03T18:10:14.142080+03:00"
closed-at: "2026-10-21T16:29:08.383201+03:00"
close-reason: "Landed in usrsyzqx 'Lock dynamic buffer growth across fork': GROW and RELEASE-RECORD unmap under the registry lock. test/dynamic-buffer-tasks.f fork hammer (10 forks): unfixed release path killed about 9 of 10 children by SIGSEGV, fixed 0; growth-only regression caught rarely. Fable review ACCEPT; full native suite 486/486 with one pre-existing failure fixed by tpsrxxnk."
---

Found by lane 449 forklocks: src/core/dynamic-storage.f RESERVE (:142) grows a buffer that already has registry capacity outside the registry lock and unmaps the old mapping before publishing the new one; a child forked inside that window (or any reader racing it) can touch unmapped memory. Acceptance: order growth so the new mapping is published before the old is unmapped (or hold the lock that lane 449 holds across fork), with a fork hammer that crashes children before and none after.

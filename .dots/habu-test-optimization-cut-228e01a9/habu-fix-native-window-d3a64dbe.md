---
title: Fix native-window fixture order and roster review
status: closed
priority: 1
issue-type: task
created-at: "2026-09-29T21:56:04.580128+02:00"
closed-at: "2026-09-29T23:19:40.718444+02:00"
close-reason: "Landed 7f784572 (review PASS after simplification): capture.f requires the fixtures and owns the roster (a dropped fixture fails the window), payload runs before any preparation, rollback sits between payload and tape-detach so both NORET-COMPACT branches run (the no-boundary branch had no exerciser). Gate 493/493."
---

Review of ba1e72af (Run the native-window fixtures in one tier-1 child) left three MEDIUM findings. (1) test/native-window-capture.f runs CHECKER-TAPE RUN before OWNER-PAYLOAD-CHECK RUN, so payload's unprepared-checker CHECK-USABLE and its first-preparation ?FROZEN/CHECK-USABLE now run after tape-detach's three CHECKER-CAPTURE-PREPAREs; each ran on a fresh checker when the fixtures were separate rows. Restore the coverage: run payload's checks before tape-detach and update the order comments (capture.f header, tape-detach.f:2-3, payload.f:3-4). (2) test/compiler/native-prefix-rollback.f:10 MARK makes every later preparation take NORET-COMPACT's boundary branch (src/core/checker.f near :17677-17680 and :17737); state that condition in native-window-owner.f's fixture-order comment, or order the fixtures so the non-boundary branch is still exercised where it was before. (3) The fixture roster is duplicated in test/native-window-owner.f TIER1-CASE (~:138-148) and test/native-window-capture.f; deleting a block from capture.f passes silently. Make one file own the roster so a dropped fixture fails. LOW: docs/compiler-measurements.md:332 names deleted test/compiler/native-checker-prefix.f. Acceptance: each MEDIUM fixed with a mutation that shows the restored detection (payload check on an already-prepared checker; dropped roster entry fails the row); native-window-owner row passes standalone; before/after seconds. Files: test/native-window-*.f, test/compiler/native-prefix-rollback.f comments, docs/compiler-measurements.md. Depends: none.

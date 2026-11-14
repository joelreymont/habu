---
title: Share one binary search
status: active
priority: 2
issue-type: task
created-at: "2026-11-14T14:46:57.307116+01:00"
---

Problem: the linker, the arm64 kernel link, Unicode and JSON each clone the same bisection loop: src/habu/linker.f EXT-OF, ROW-EXT-OF, EM-OWNER, OWNER-EM; src/habu/link-copy.f STRING-OF, SEG-OF; src/arch/arm64/link-kernel.f FIND, FIRST-REF, FIRST-SPAN; src/lib/unicode/class.f and script.f UPPER-BOUND; src/lib/json/equal.f NARROW. Acceptance: one SORT:PARTITION-POINT (src/lib/sort.f) answers the first index of a range whose predicate fails, every site above calls it and its own loop is gone; the engine builds itself and the linker, Unicode and JSON tests pass. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-bs.

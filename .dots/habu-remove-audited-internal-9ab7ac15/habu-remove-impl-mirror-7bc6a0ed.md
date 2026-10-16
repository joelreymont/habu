---
title: Remove implementation mirror assertions
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.991967+03:00"
---

Delete target-native-layout numeric-offset golden and registration, retaining semantic grow/shrink and older-donor transition tests. Delete gate-json-assert copied suggestion prose lookup/equality, retain repair-class and meaningful nonempty suggestion checks. Separate commits if these become independent significant changes; verify existing actual layout/diagnostic behaviors.

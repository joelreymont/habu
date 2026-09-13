---
title: Preserve the complete live DATA region during snapshot capture
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-13T22:57:42.845983+03:00\""
---

Tracked B3 writes an application image, but restored JIT compilation prints zeroed diagnostic strings and native compilation refuses E-HIR-DUP (-8285). SNAP:SND-ZERO-DEAD-HEAP infers dead memory from IMK-NDICT0; native-build reserves the live 512 KiB NSTR window before that variable. Actual DATA-START 1417048, IMK-NDICT0 offset 1941336 differ by exactly 524288. Remove the obsolete inferred sweep from snapshot writing; owners must retire their own transient state. Verify original failing saved-image compiler reuse in both tiers, native application callback relocation, and repeated capture; record any separate legacy-builder canonicalization failure at its actual owner.

Implemented: removed the inferred heap sweep and corrected IMK-NDICT0's comment.
The original failing native-defer-image suite passes on tracked B3 with the source
writer. Its strengthened regression also passes: both restored generations compile
and execute fresh JIT and native words, saved native installers relocate, and a
fresh all-native process recaptures successfully. Logs:
`/tmp/cedar-B3-defer-preserve.log` and
`/tmp/cedar-B3-defer-preserve-both.log` (both rc 0, `test: ok`).
Independent review and the next rebuilt full gate remain pending. Other legacy
builder and capture failures are not covered by this result.

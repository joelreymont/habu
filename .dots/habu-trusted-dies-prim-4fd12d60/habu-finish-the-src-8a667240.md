---
title: Finish the source-owned native runtime build
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-10T18:03:13.327440+03:00\""
---

Owner: Cedar, with app_image on warm source replay. Combined cold build through d8b044e7 passes using the private crossing seed. Reviewed fixes keep declaration registration with the retained checker, reset user effects alongside the dictionary prefix, reserve literal pools before evaluation rollback, and lower traps through owned diagnostics plus engine die. Combined native-prefix-declarations, native-string and native-trap tests pass.

Reviewed next revision 72e54994 replaces the raw-only registrar slot with one private record of existing raw/effect/defer/cast operations; it needs a cold rebuild because the slot changes relocation kind. Warm replay now reaches checker quotation handling; its source contract migration remains unfinished. Complete a native self-rebuild, restore working no-binary recovery, remove obsolete capture-host assumptions from tools/tests, validate a matching binary/source pair and update bootstrap documentation. Do not route ordinary algorithms through PRIM or revive the retired compiler.

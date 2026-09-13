---
title: Select optimizing compilation before every executable input
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-11T16:07:57.859028+03:00\""
---

Owner: Rowan .jj-ws/rowan-tier; routing candidate b44b46279a81 reviewed with two missed child producers: test/native-resource-image.f BUILD (~63), test/snapshot-writer.f BUILD-WITH (~107). Both spawn default-JIT engines, compile fixture sources, then APP-IMAGE:SAVE. Prepend tier1 before those sources. Existing nine candidate sites cover BF-APPEND-RUN-PRELUDE, maker, stdin capture host, aot-chain-capture, hb-build --repl writer, native-build and app-image children.

Bootstrap correction: /tmp/cedar-crossing-realpath/hb-stdin has neither set-tier nor NCOMP:COMPILE; matching stage2-src falls back to legacy JIT, so absence of set-tier cannot be treated as proof of optimizing mode. Prefer existing tiered bin/hb with1 set-tier, repairing its actual selfbuild failures. Rebuilding hb-stdin is not prerequisite. Full old /tmp/cedar-reviewed-integrated-native has NCOMP resident and XT equals dispatch cell, a different host from the cold seed; do not add a no-op setter.

Acceptance: actual native selfbuild, hb-build application executable, fresh capture/restore and recapture use optimizing code for every retained definition; ordinary --load/REPL remain JIT. Native test routing is separately integrated817922ca. Coordinate retained-code save invariant with the provenance dot. No claim based solely on tier@ at save time.

Cedar review 2026-09-13: app-image.f selects tier1 only at line 73, after loading its helper closure. On pending rowan-tier binary 43bca321 (exact producing commit not independently matched), requiring app-image.f from default0 reported tier 1 but 192 retained JIT spans, first FS-FALSE. This source path contradicts the pending SNAP refusal and the documented require/app/MAIN/SAVE recipe. Select AOT before the image helper closure; keep rejection of user JIT code compiled earlier. The landed root also retains this late selection. Verify through the documented public entry, without a fixture-only tier prelude.

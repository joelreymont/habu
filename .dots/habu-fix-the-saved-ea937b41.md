---
title: Fix the saved native builder crash
status: active
priority: 1
issue-type: task
created-at: "2026-10-15T20:15:33.489391+03:00"
---

Normal tools/native-build.f with the first matching definition-cache engine (SHA 0b39ba1d) creates its support image, then that saved support process SIGBUSes during the target build before writing any definition containers. Retained command/output: external habu-native-definition-batch.3tinjw2e/cached-engine-cold.{out,err}; support cache key 9bdacb8d2b2e0992067acf3c72dcb85b1dee09c9550cc4197e1f6419c23aaf9c; PC 0x101d22bf8, LR 0x1001d9494. Diagnose the actual stale call/continuation or capture boundary using the saved executable, fix its responsible owner, and verify the existing normal native-build cold/warm/edit E2E. A direct uncached source build passes; bypassing the production saved-support path is not the fix.

LLDB identifies the faulting live dictionary row as CHECKER-REG's newly compiled RECORD-PUBLISHED (ordinal3947), in an rw- code page. The retained source checker's RECORD-PUBLISHED forwards to it while EM-DEF-OCC:LAPPEND still holds publication write bands open. Preserve the caller's declared code/record/control-flow bands, close them before invoking the checker observer, and restore those exact spans afterward. A callback must execute with code RX even when its entry shares the newly written page; bulk publication must retain its caller's writable extents after the callback.

---
title: "habu: certified words silently read below stack base at top level (FOO2 class)"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-12T09:11:15.731585+02:00\""
---

Crash-RCA residue (dot habu-hb-crash-bare-c5be6634, internal-word gate lane). A CHECKED word executed at bare top level with fewer cells than its declared inputs silently reads below the stack base when net depth stays >= 0: ': FOO2 ( n -- n n ) dup ; FOO2' on an empty stack runs rc 0 consuming garbage. The internal-word gate (DNAME-INT) closed the engine-internal class; certified user words remain exposed. Fix shape proven by precedent: record min-in in dict records at certification time (checker pokes the record like wide-mark does) and generalize the existing LARITY pre-BLR depth guard (14 deref prims already use it) to all certified words. Principled endpoint is a checker-modeled typed top level. Also fold in the cheap census residue: audit prim axioms vs top-level exposure below the IMK-NDICT0 watermark (unwalked by the marking pass; only int-mark is explicitly self-sealed). Files: src/habu/habu1.f (LARITY), src/habu/habu2.f (dispatch), src/core/checker.f (min-in poke at cert). Regression: extend test/internal-word-gate.f with a FOO2-class negative (underdepth certified word -> named diagnostic rc 70, not silent read).

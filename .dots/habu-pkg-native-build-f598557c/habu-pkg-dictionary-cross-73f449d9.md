---
title: Package dictionary cross reference
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T21:50:00.224811+02:00"
---

Current master package census: src/habu/xref.f:6-341 contributes 88 unowned definitions to the cold prefix, 12 extended names and 984 definition-name bytes. It mixes read-only dictionary inspection, eleven mutable scratch cells, globally callable TRUSTED record/address casts at 16-18, protected-constructor boot wiring, raw patching/retirement/forget/hide mutation at 239-308, and user commands. Split ownership into package XREF for the read API and package DICT-LIFECYCLE for the narrowly proven mutation API. Export qualified LATEST/FIND/FOUND?/NULL/NAME$/START/LEN/FLAGS/WORDLIST/RETIRED? and only the lifecycle entries required across build boundaries; keep casts, scans, scratch, comparison, patching, seal checks, indices, and constructor installation private. Keep only documented UI wrappers XREF, SEE, WORDS, and lowercase undefine global as intentional language surface. Replace global lookup scratch with typed locals or owned state so inspection is reentrant. Preserve exact output, dictionary identity, AOT closure, constructor protection, undefine/hide/forget semantics, snapshot, cold/warm boot, and fixpoint. Prove all twelve extended internals and legacy helper globals reject, qualified APIs work, UI output is byte-exact, nested/concurrent reads cannot corrupt state, and every mutation remains fail-closed. Measure dictionary-name/JIT/DATA/CODELEN and lookup/mutation latency. Serialize mutation edits with habu-seal-owners-syntax-63051652 and habu-tfam-2b-iii-5d25b52f; those retain sink semantics.

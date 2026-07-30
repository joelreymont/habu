---
title: Add scoped cell storage
status: active
priority: 2
issue-type: task
created-at: "2026-07-30T13:14:39.821758+02:00"
---

Problem: the direct GPT-2 config parser must create aligned JSON reader state without a package-global buffer, but MEM scopes only byte storage and JR:INIT rejects ptr u8. Result: package MEM adds WITH-CELLS ( R CAD-NUM:alloc-cell-count [ R ptr a CAD-NUM:alloc-cell-count -- S ] -- S ). It allocates through MEM:ALLOC-CELLS, runs the quotation with the exact validated count, and always releases the same mapping after normal return or throw. Factor one private catch, cleanup, nesting, and primary-error driver shared by WITH-BYTES and WITH-CELLS; typed byte and cell adapters alone allocate, invoke, and release their pointer roles. Cell release uses the existing checked BYTE-VIEW and existing size projection; add no trusted word. Owner: lib/memory.f and lib/memory-test.f only. Production red: a checked representative HFCFG callback cannot pass WITH-BYTES storage to JR:INIT, while the same callback must certify with WITH-CELLS. Acceptance: exact and large counts, body results, body throw, allocation refusal, nested cell scopes, and mixed byte/cell nesting preserve values and release each mapping once; existing WITH-BYTES behavior and trust inventory stay unchanged; focused memory, JSON reader, typed-local, package, trust, and native gates pass. Forbidden: public manual cell release, ptr u8-to-ptr a cast, new trusted boundary, duplicate scope driver, escaped owner type, global consumer storage, compatibility alias, version, lint, or unrelated memory refactor. Smallest owning check: the temporary checked JR:INIT representative plus lib/memory-test.f. Claim: agent=codex-with-cells workspace=.jj-ws/habu-add-scoped-cell-d00f868a.

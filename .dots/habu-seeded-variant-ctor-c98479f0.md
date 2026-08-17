---
title: Seeded variant ctor-syms are capture-absolute into an unaligned store
status: open
priority: 2
issue-type: task
created-at: "2026-08-17T02:51:12.475489+02:00"
---

src/core/type-family.f REG-AOT-LOAD bulk-copies SUMV rows whose SV.CTOR-SYM cells are absolute CHECKER SYM STORE ids from the capture engine (measured: vid 326/327 fam cmpkind carry ctor-sym 7750/7751, past the target's boot SYM-N 3147). The sym store is NOT one of the 8 base-aligned registry stores, so these ids are meaningless in the target. Today the defect is MASKED because the loader never resets the SVX ctor index (SUMV-N grows past SVX-HI, a direction SVX-SYNC deliberately does not watch), so SVX never answers for seeded rows - which is exactly test/checker-scan-index-suite.f's ds=55 differential red. Adding the honest '0 SVX-GEN !' reset (tried, reverted) turns the mask into active mis-binding: as the target interns syms, a fresh definition's sym id eventually EQUALS a carried ctor-sym and SVX hands the fresh word a stale seeded variant - measured as lib/process.f's PROC-CAPTURE>RESULT rejecting at RESULT:OK ('expected: a actual: pcap:captured<>') when and only when enough definitions precede it (fs.f cut=30 red, cut=40 clean, deterministic; unseeded hb-host always clean), which wedged the install fixpoint. Fix direction is a design decision: ctor-syms cannot travel as absolute ids - re-derive at load (needs pkg+name per variant row at intake, cf SUMV-CTOR-PKG$), or zero on load + re-intern on first use. The SVX reset must land WITH that repair, never alone. Blocks habu-seeded-words-invisible-c7505a49.

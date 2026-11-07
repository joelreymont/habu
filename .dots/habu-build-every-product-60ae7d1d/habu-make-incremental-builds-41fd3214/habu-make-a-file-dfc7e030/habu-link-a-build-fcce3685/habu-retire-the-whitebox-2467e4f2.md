---
title: Retire the whitebox build option with the old hb builder
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T23:20:37.924915+02:00"
---

Problem: the whitebox option of tools/native-build.f and tools/hb-build.f, CLASS-WHITEBOX (tools/native-build-core.f) and internal-mark.f's IMAGE-WHITEBOX/ENGINE-UNSEAL? branch build an unsealed engine. After the suite pruning no test uses it; docs/bootstrap.md 'Recovery routes' still builds a whitebox host when an older host cannot build a tree with a new baked checker API. DECISIONS 'the old builders' (2026-11-05): the old hb builder goes once the arm64 cross-build writes a working hb. Acceptance: with the old hb builder, the option, the class, the internal-mark branch and the bootstrap.md route go; a cross-built hb is the recovery route. Files: tools/native-build.f, tools/hb-build.f, tools/native-build-core.f, src/habu/internal-mark.f, docs/bootstrap.md. Verify: rg whitebox over src tools test docs prints only unrelated words; the cross-build recovery route builds hb. Depends: the arm64 cross-build writes a working hb (habu-link-a-build-fcce3685). Ownership: unassigned. Claim: unassigned.

From the record-facts! seal-floor review (2026-11-07): `seed-ndict!` (habu1.f BSEEDNDICTSET) sets SEAL-NDICT-CELL to 0 for any top-level caller, so every floor check (BNDAPPEND, BNDSET, record-facts!, the package seal) holds only until a program runs it: `: U1 ( -- ) ; ndict@ 1 - seed-ndict!` then `: P10 ( -- ) 0 0 record-facts! ; P10` exits 0 on a candidate built from this lane and rewrites kernel record 0, and the same line lets `package NPUB` reopen (exit 0, otherwise 84 E-SEALED-PACKAGE) on the installed hb too (/tmp/rf2probe.Tkwk p10, p11; p12 without the line). Only the loading builders' rewinds need it (src/habu/prefix-rewind.f:21, src/habu/window-rewind.f:15, tools/cold-load.f, tools/bootstrap.sh:165); test/prof-rewind-child.f's seed-rewind case and the x86-64 kernel tests also call it. Acceptance case: once the old builder goes, no program lowers the seal floor: p10 and p11 die E-UNDEFINED (70) at `seed-ndict!` if the word goes, else 83 and 84; with `ndict!` in its place they exit 83 and 84.

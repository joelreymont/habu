---
title: Add the engine writers the Habu interpreter needs
status: closed
priority: 2
issue-type: task
closed-at: "2026-09-30T17:01:18.937396+03:00"
close-reason: "done: seven EPRIM writer rows (ARM64 DEFWRITE bodies, x86 refusals); test/engine-writers.f ok at exact statuses 79/84/83; spark chain from 925c1c6e converges gen2-5 at efddfcc6, gate 506 of 506 rc 0; ThinkPad x86 proof 128 images at unchanged statuses, bad=0, plain and Mac-shim identical."
created-at: "2026-09-30T14:52:44.675778+03:00"
---

Problem: the Habu interpreter cannot write what `package`, `export` and `:` change. After the seal a store into the friend arena (CUR, WIDN, DEF-WL, TSIG, PKG-*), BODYBUF, DEF-TIER-CELL or the TIER-PROV band exits 83 (`src/habu/data-bands.f:18-29`, measured on hb-master-3dc7); records sit in the protected region behind the PROT window; no row writes a namespace row, an alias or a pending record (`src/habu/prims.f:518-593`). Writable after the seal (measured): PEND, BODYLEN, TRUSTED, DOESB, TASKS-LIVE, USE-DEPTH, USE-PKG-SAVE, USE-WIDS, DEF-TKA/DEF-TKL, EXIT-HOOK.
Acceptance: seven global EPRIM rows, each `ETRUSTED-ONLY!`, registered `ENGINE-PRIMS:GLOBAL-INT-WID` on both targets, each contract stated beside it in prims.f:
- `namespace-record ( ptr u8 n bool -- n )`: publishes a namespace row at NDICT: [0] a fresh wid; [8] a second fresh wid when the flag is set, else 0; [40] -1 (as `habu2.f:7939-7950`, `3622-3634`); indexes it; answers its index; refuses a colon in the name.
- `namespace-private ( n -- )`: gives namespace row n, whose [8] is 0, a fresh private wid (`7956-7963`).
- `alias-record ( ptr u8 n n n -- )` name, source index, wid: publishes the source's [0] and [8] and exactly its IMM, WIDE and MIN-IN bits (`8398-8405`); refuses a namespace or retired source.
- `package-scope! ( n n -- )` namespace index, parent wid: sets PKG-PUB and PKG-PRI from the row, PKG-REC to the row's address, PKG-PARENT to the wid; `-1 0` clears all four; refuses a non-namespace row or one without a private wid.
- `def-open ( ptr u8 n n n -- )` name, wid, kind (0 or DKIND VAL/ADDR/CAST): writes record NDICT unpublished, [0] = CP after the name, [8] = 0; PEND-CELL := that record; clears TSIG, TCSIG, DOESB, TRUSTED; DEF-TIER := TIER-CELL; TIER-PROV OPEN-CELL := CP; refuses when a definition is pending or CP >= DBASE+REGION-$4000.
- `body-append ( ptr u8 n -- )`: appends the bytes and one space to BODYBUF as LBCS does (`habu1.f:3663-3688`); refuses BODYLEN+u+1 past BODYBUF-CAP (unsigned).
- `trust-sig! ( ptr u8 n -- )`: sets TSIG-A/U; refuses when no definition is pending.
Record writers (`namespace-record`, `alias-record`, `def-open`) store a name of >= 1 byte; a name over 16 bytes goes at CP, 4-aligned, marked native provenance, as C-STORE-NAME (`habu2.f:3205-3239`); they refuse a live (folded name, wid) pair, NDICT at DICT-CAP and a name spill past the code ceiling; `alias-record` and `def-open` also refuse wid -1 or -2. Every refusal exits (none throws): 79 task live (the five dictionary and scope rows), 84 protected wid after the seal (`alias-record`, `def-open`), 83 otherwise. Callers check first and print the engine's text.
Bodies: ARM64 in `habu2.f`, new package DEFWRITE, sharing one name-store emitter with C-STORE-NAME (name in registers, capacity failure a caller-supplied label), registered in EMIT-PRIMITIVE-SECTIONS beside `does-record` (`habu2.f:11160`). x86: each row registers as a refusal (`[: REFUSE-BODY ;] ENGINE-PRIMS:GLOBAL-INT-WID PRIM-WID`) in a new `DEFINITION,` section appended to `KERNEL,` until K9e replaces them.
Files: `src/habu/prims.f`, `src/habu/habu2.f`, `src/habu/kernel-x64.f`, `test/engine-writers.f` (package ENGINE-WRITERS-TEST), `test/gate-stdlib-cases.f` (SUITE engine-writers), `docs/x86-64.md` (kernel inventory).
Verify: `bin/hb --load test/engine-writers.f`, one spawned fixture per case through the engine's own loop, each row observed through an existing consumer: a namespace row answers `using`, a `package` reopen and `P:X` (a flag-less row the same after `namespace-private`); an alias runs its source and stays immediate; `package-scope!` makes a private word resolve bare and `;package` closes the scope; a TRUSTED: word calls `def-open`, `body-append`, `trust-sig!` at `1 set-tier`, then `42 ;` in the engine loop publishes a word printing 42 whose entry answers code-origin 1; a long name answers code-origin 1; each refusal by its exact rc. Pre-change: E-UNDEFINED. Rebuild, chain from gen2, gate; the x86-64-kernel-* suites unchanged.
Serialise `habu2.f` (I6, I9a, I10c) and `prims.f` (I5e, I6, I9a).
Route: krait lands after the Linux proof; Alder pools the Mac gate.
Ownership: krait (Intel lane).
Claim: unassigned.

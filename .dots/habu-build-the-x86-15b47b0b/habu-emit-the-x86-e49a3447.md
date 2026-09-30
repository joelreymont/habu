---
title: Emit the x86 bodies of the engine writers
status: closed
priority: 2
issue-type: task
created-at: "2026-09-30T14:52:44.686639+03:00"
blocks:
  - habu-add-the-engine-ebe5d757
  - habu-model-the-code-c40c75d1
closed-at: "2026-09-30T19:05:00.000000+03:00"
close-reason: "Seven x86 definition-writer bodies; 25 images at their stated statuses (10x0, 21, 5x79, 7x83, 2x84), x86 proof plain == Mac shim, ARM64 rebuild == eff4ff42; Opus review LAND"
---

Problem: I4c registers its seven engine-writer rows on x86 as refusals, but the Habu interpreter X6 runs needs them.
Acceptance: real bodies in `kernel-x64.f` `DEFINITION,` with I4c's contracts and exit codes: one name-store helper (inline, or at CP inside WINDOW-OPEN with `X64PROV:NATIVE-RANGE,` over it); RECORD-AT,, WINDOW-SPAN, WINDOW-CLOSE, as DOES-RECORD, does (`kernel-x64.f:2000-2052`); the live-name probe of the one-wordlist search (`:491`), `HIDX-ADD,` (`:661`), `PROT-BITS,` (`:2493`), TASK-LIVE-GUARD,; a store of the code pointer into OPEN-CELL.
Files: `src/habu/kernel-x64.f`; `test/x86-64-kernel-definition.f` (package X64K-DEFINITION: one booted image per row outcome, one per refusal code, one `-negative` 21); SUITE x86-64-kernel-definition; `docs/x86-64.md`.
Verify: host `bin/hb --load test/x86-64-kernel-definition.f`; ThinkPad: each image natively with the status its header names.
Route: direct (x86-only files).
Ownership: krait (Intel lane).
Claim: unassigned.

Preflight corrections (2026-09-30; override the lines above where they differ):
- Helpers now in `src/habu/kernel-x64.f`: TASK-LIVE-GUARD, :185 (exits 79), CODE-SLOT :70, FIND-LBL :419 (the probe is FIND-HELPER, :510, called the way SEARCH-WL-BODY :2136 calls it: rdi/rsi/rdx survive, rax is the record or 0), HIDX-ADD, :673, SLOT-UP, :1897, RECORD-AT, :1968, DOES-RECORD, :2039-2090, WINDOW-SPAN,/OPEN,/CLOSE, :2099-2101, SEAL-TRAP-LBL :1653 (exits 83), PROT-BITS, :2531, and `X64PROV:NATIVE-RANGE,`/`OPEN,` at `code-origin-x64.f:324,335`. Every helper call clobbers rax rcx rdx rsi rdi r8-r11, so keep the arguments in a machine-stack frame, the way DOES-RECORD, does.
- ARM64 twins (`habu2.f`, DEFWRITE; effects as in `prims.f:606-667`): NAMESPACE-RECORD :3536 `( ptr u8 n bool -- n )` 79/83; NAMESPACE-PRIVATE :3568 `( n -- )` 79/83; ALIAS-RECORD :3585 `( ptr u8 n n n -- )` 79/83/84; PACKAGE-SCOPE :3620 `( n n -- )` 79/83; DEF-OPEN :3643 `( ptr u8 n n n -- )` 79/83/84; BODY-APPEND :3678 `( ptr u8 n -- )` 83; TRUST-SIG :3691 `( ptr u8 n -- )` 83. Two helper exits can also end a row: 74 (the index is exhausted) and 101 (the origin table is full). Keep each twin's check order. Every refusal comes before the first store and writes nothing to fd 2.
- Contracts fixed:
  - 84 is the twin of LPROTWIDQ (`habu1.f:4044`). It applies only when SEAL-NDICT-CELL≠0. Wids 1 and 2 are always protected, a wid ≥PROT-WID-MAX (unsigned) never is, and any other wid is judged by the PROT-BITS, bit.
  - A long name goes at CP, zero-padded to the next CODE-SLOT, inside WINDOW-OPEN, over [CP, slot). NATIVE-RANGE covers [old CP, slot) and CP moves to the slot. The room check refuses when CP plus the slot-padded length is ≥DBASE+REGION-$4000, unsigned.
  - The OPEN-CELL store is `X64PROV:OPEN,` after the name store. It stores that slot, the same value [0] takes.
  - body-append stores nothing while P2-CELL≠0 (`habu1.f:3672`).
  - A kind is refused when kind AND NOT DKIND:MASK≠0.
- Files add:
  - `test/gate-stdlib-cases.f`: the SUITE goes after :1440-1442.
  - `src/habu/prims.f`, comments only: at :618, "4-aligned" becomes "a code slot, 4 bytes ARM64, 16 x86-64"; the CP-writer list at :516-524 gains the three record writers.
  - `docs/x86-64.md`: the "Code slots" bullet at :1136 and the "Definition rows" section at :1713.
- Baked: only `prims.f`, and the change is comment-only. Show that the ARM64 product is byte-identical to master's, as K8c did. No chain and no full gate.
- Pre-change check: a harness image that pushes `hello 0 0` and calls `def-open` exits 76 with `hb: def-open is not in the x86-64 kernel`.
- Tests (put the region at rest with REST, first; after each write, PUSH-BANDS, reads 0 and a PROBE, of the record page faults):
  - Images that exit 0: -namespace-record (build the index with HIDX-BUILD, first, then `xref-search-wl` finds the row); -namespace-private; -alias-record (the source's IMM, WIDE and MIN-IN bits are copied, its VAL bit is not); -package-scope (set, then `-1 0`); -def-open; -def-open-state; -def-open-long (a 17-byte name moves CP 32 bytes and the pad is 0; `code-origin` answers 1); -body-append (exactly at BODYBUF-CAP); -body-append-pass2; -trust-sig.
  - -definition-negative exits 21.
  - Refusals: 79, one image for each of the five rows that guard. 83: a live pair that differs only in case; `namespace-private` on a row whose [8] is set; a DNAME-INT alias source; `-1 5`; a 17-byte def-open with CP at ceiling-32; body-append at CAP+1; trust-sig! with nothing pending. 84: alias-record and def-open, one image each.
  - That is 25 images. The file header and the docs table state each image's status.
- Verify: the host `--load`s the test. On the ThinkPad, run each image natively, and run every other x86-64-kernel-* image to show its status is unchanged. The model for the new test is `test/x86-64-kernel-atomics.f` (REST, :138, PROBE, :146, PUSH-BANDS, :168, DOES-CASE :413).

\ layout.f - shared native image, dictionary, and snapshot layout constants.

20 constant XREG-RBASE
26 constant DBASE
27 constant NDICT
28 constant CP

\ ---- the general registers the running engine occupies -----------------------
\ THE authority. A compiled routine that writes one of these destroys the engine
\ underneath itself: a probe that allocated x20 as a scratch base replaced the
\ DATA/RBASE value and the process died 134 (CG-13). The compiler's own machine
\ contract (src/compiler/a64-effect.f) derives its GPR set by REMOVING this mask,
\ so a register claimed here is refused by every GPR constructor, set, sequence,
\ pool and allocation path in the chain without a second edit anywhere.
\
\ It is built from the register constants rather than written out as a list, so
\ claiming a new engine register is one line here and nothing else.
\
\ ENGINE-GPR:DSTACK is declared HERE and not read from src/arch/arm64/mnem.f's
\ XDS, even though mnem.f is that number's historic home. mnem.f is an
\ emitter-side vocabulary the BUILD chain compiles; this file is also re-read by
\ the booted engine at every `--load`, and mnem.f is not. A derivation through
\ XDS therefore compiles during the build and dies E-UNDEFINED at runtime - which
\ is exactly what happened when it was tried. The two must agree, and
\ src/habu/rt.f - the first consumer of XDS, loaded after this file in both
\ build chains - executes that agreement (RT:DSTACK-AGREE) and dies on mismatch,
\ so a change to either stops the build rather than drifting.
package ENGINE-GPR

public

19 constant DSTACK

1 DSTACK lshift
1 XREG-RBASE lshift or
1 DBASE lshift or
1 NDICT lshift or
1 CP lshift or
constant MASK

;package

$800000 constant REGION
\ RBASE-VA: snapshot CANONICAL region base (a fixed portability sentinel, NOT the
\ runtime map address). The live JIT region maps at __text base + REGION-OFF (see
\ habu2.f EM-MMAP-CODE-REGION) so every call site and callee sit inside BL's
\ +/-128 MiB reach; snap-rebase canonicalizes region-internal pointers to this
\ sentinel and the loader rebases them back to the live region base, keeping images
\ byte-identical across runs and imgdump's region->offset math target-independent.
$300000000 constant RBASE-VA
\ REGION-OFF: offset of the live region base past the runtime-discovered __text base
\ (XREG-RBASE). Clears the loaded image with wide headroom and keeps region_end -
\ __text (= REGION-OFF + REGION) far below BL-REACH. A multiple of the largest target
\ page (16 KiB on macOS) so the mmap hint is page-aligned on both targets.
$1000000 constant REGION-OFF
\ BL-REACH: AArch64 BL imm26 reach (+/-128 MiB). The boot assertion dies BL-RANGE-RC
\ if the mapped region falls outside this of __text -- the permanent guarantee stage
\ C's direct-BL emission relies on.
$8000000 constant BL-REACH
81 constant BL-RANGE-RC
$48425350414E5321 constant SNAP-MAGIC
\ Version 4: the live JIT region moved from a fixed VA to __text base + REGION-OFF
\ (BL range); region-internal pointers now canonicalize to the RBASE-VA sentinel and
\ the loader rebases them to the live region base. A pre-4 engine maps the region at
\ the old fixed VA and cannot relocate a v4 image's region pointers, so it fails
\ closed rc 80.
\ Version 5: the region no longer has to land at a particular address at all. The
\ loader accepts whatever base the kernel gives it and relocates every recorded
\ region-to-text call displacement, and every persisted data cell that was
\ declared to hold a region address (both tables live in the SNAP-RELOC band near
\ the end of this file). A version 5 image therefore stores its call immediates in
\ the canonical "region sits exactly REGION-OFF above __text" form, and its
\ declared address cells relative to the RBASE-VA sentinel, rather than in the
\ writing run's own form. A version 4 engine would read both as live values and
\ jump to wild addresses, so it must fail closed rc 80 instead.
\ Version 6: the address literals the compiler builds inside region code -- the
\ four-instruction MOVZ/MOVK chain that pushes a quotation entry, a `[']` target
\ or a `postpone` target -- are canonicalized as well, region-valued ones against
\ the RBASE-VA sentinel and engine-text-valued ones against text base 0, from a
\ third table in the SNAP-RELOC band. A version 5 image stores those chains as
\ the writing run's own absolute addresses, so a version 5 engine and a version 6
\ image disagree about what the chain bytes mean in both directions and each must
\ fail closed rc 80 rather than execute the other's literals.
6 constant SNAP-FORMAT-VERSION

\ --- snapshot trailer geometry: the single owner ----------------------------
\ The trailer is the last thing in the authenticated text extent, so its base is
\ (the header's text-size field + IMAGE-TEXT-TRAILER-ADJ) - SNAP-TRL-BYTES; the
\ codesign blob and any page padding follow it and are not part of the extent.
\ Everything that reads or writes a trailer derives its size and field offsets
\ from here: the writer (src/habu/snap-lib.f SNAP:WRITE-BYTES), the loader
\ (src/habu/habu2.f EM-SNAPSHOT-RESTORE), the image dumper (tools/imgdump.f) and
\ the two fixtures that doctor a real image (test/snapshot-writer.f,
\ tools/build-fixpoint-test.f). They diverged once - the readers kept the legacy
\ 40-byte size after the format grew to 48 - and every reader then addressed the
\ wrong cells while still finding plausible values, so the size lives in exactly
\ one place now.
48 constant SNAP-TRL-BYTES        \ magic, text base, ndict, region len, data len, version
8 constant SNAP-TRL-TBASE         \ snapshot-time text base (canonically 0)
16 constant SNAP-TRL-NDICT        \ dictionary record count
24 constant SNAP-TRL-REGLEN       \ region payload length
32 constant SNAP-TRL-DATALEN      \ data payload length
40 constant SNAP-TRL-VERSION      \ SNAP-FORMAT-VERSION of the writing engine
\ The pre-version trailer. Only the loader may name it, and only to recognise a
\ legacy image and fail it closed rc 80 instead of misreading its fields.
40 constant SNAP-TRL-LEGACY-BYTES

\ DICT-SIZE = CFSTK-OFF (= DICT-CAP * DREC record slots) + $1000 control-flow
\ stack; the code area follows at DBASE+DICT-SIZE inside the REGION.
\ Grown $61000 -> $C1000 with DICT-CAP 8192 -> 16384 (the gate-runner-support
\ tool closure needs ~9.5k records; dot habu-gate-runner-entry-81c84af0).
\ Grown $C1000 -> $181000 with DICT-CAP 16384 -> 32768 and REGION $400000 ->
\ $800000 (dot habu-lprot-narrow-protection-03cc8d7f): both sides grew because
\ maki peaked ndict 16347/16384 (dict side full) AND the code area measured ~92%
\ of the 4 MB split via LATEST/XREF (code side full). Narrowed dict-poke flips
\ (LPROTREC) keep the seal-time internal-mark cost off the grown region, so the
\ 4 MB -> 8 MB growth stays under the runtime ratchet. Keep DICT-CAP/CFSTK-OFF/
\ DICT-SIZE/HIDX-SLOTS/HIDX-BYTES in step.
$181000 constant DICT-SIZE
48 constant DREC
16 constant DNAME-INL
1 constant OWNER-API-PUB-WID
2 constant OWNER-API-PRI-WID
3 constant FIRST-DYNAMIC-WID
\ --- the wordlist cell's two non-wordlist values ---------------------------
\ A record's wordlist cell is HALF THE HASH INDEX'S KEY (habu1.f C-HIDX-INS
\ keys a slot on the folded name XOR this cell), and it is written once, at
\ publication, BEFORE the record is inserted - which is what lets a probe with a
\ caller's wid find the row or prove it absent.
\
\ NAMESPACE keeps that rule: a package's own row carries it from birth, so the
\ row is on this key's chain and LFIND's qualifier probe finds it there.
\ RETIRED breaks it. src/habu/xref.f XREF-RETIRE stamps it on the cell of a
\ record that is ALREADY in the table, so the row stays on the chain of the wid
\ it was published under. Every lookup keyed on a REAL wid still agrees with a
\ scan (the row's cell no longer matches, so both skip it), but a lookup keyed
\ on RETIRED itself cannot be answered by the table at all - and retiring one
\ name twice puts two rows under the key, which an insert-once table has no slot
\ shape for. habu1.f BSWL therefore keeps its linear scan for exactly this wid.
\
\ Both live in a package rather than beside the constants above because they are
\ new: the surrounding global surface is this file's packaging debt (dot
\ habu-give-layout-f-315df2ca), and a name that has no bare callers yet has no
\ reason to join it. `package SNAP-RELOC` further down is the same shape.
package DICT-WL
public
-1 constant NAMESPACE
-2 constant RETIRED
;package

\ WID:MAX is the largest value a wordlist id may take; it bounds every
\ WID-indexed table and the snapshot's registry validation.
package WID
public
$FFFFFFFE constant MAX
;package
$000FFFFFFFFFFFFF constant DNAME-LEN-MASK
\ DNAME-MIN-IN (bits 52-59): certified minimum input arity in cells, poked at
\ certification time (checker RECMI latch -> publish tails / seal-time
\ internal-mark pass; dot habu-habu-certified-words-84e84eaf). LFIND folds the
\ byte into x13 bits 8-15; EM-INTERPRET-FIND fails closed BEFORE the BLR when
\ the interpret stack holds fewer cells, so a certified word can never consume
\ below-base garbage at bare top level. 0 = unmarked (unchecked words and
\ words without signatures: the documented boundary). Compiled calls inside
\ checked words are checker-proven and carry no guard. Claimed band: bits
\ 52-59 of the record flags cell [16], below IMM/EXT/WIDE/INT (bits 60-63)
\ and above the narrowed name-length field (bits 0-51); native length reads
\ clear the top 12 bits (LSLI 12 / LSRI 12).
$0FF0000000000000 constant DNAME-MIN-IN-MASK
$1000000000000000 constant DNAME-IMM
$2000000000000000 constant DNAME-EXT
\ DNAME-WIDE (bit 62): the word's recorded stack effect carries a
\ wider-than-cell layout value in some row, so executing it at INTERPRET level
\ would land a multi-cell bundle on the untyped interpret stack where scalar
\ dup/drop/swap silently corrupt it (dot habu-tfam-12-interpret-10b385b1).
\ LFIND folds the bit into x13 bit 3; EM-INTERPRET-FIND and interpret ' fail
\ closed on it. Set by xref.f XREF-WIDE-MARK; the checker marks at signature
\ record time once the sequenced src/core/checker.f half lands. Compile-mode
\ calls inside checked definitions are unaffected (pass-2 lowers them).
$4000000000000000 constant DNAME-WIDE
\ DNAME-INT (bit 63): engine-internal executable word - a COLON record defined
\ by the engine-prefix source with no checker-known effect (no certified or
\ trusted signature and no primitive axiom). Set by the seal-time marking pass
\ (src/core/internal-mark.f) so the executable top-level name universe equals
\ the checker's; data records (create/variable/constant, does>-instances) are
\ exempt - push-only bodies, auto-trusted by C-CALL-TRUST-LASTC-* whenever a
\ hook is installed. LFIND folds the bit into x13 bit 4; EM-INTERPRET-FIND and
\ interpret ' fail closed on it with `hb: internal engine word: <token>` +
\ rc 70 (dot habu-hb-crash-bare-c5be6634). Compile-mode references (explicitly
\ unchecked user code, TRUSTED: bodies, hide.f refresh shims) are unaffected:
\ those are declared trusted boundaries.
$8000000000000000 constant DNAME-INT
32768 constant DICT-CAP
$180000 constant CFSTK-OFF
24 constant CF-REC
8 constant CF-LOCN
16 constant CF-LOCF
\ Control-flow stack region [CFSTK-OFF, DICT-SIZE): cell 0 is the depth counter,
\ then CF-REC-byte records. CFSTK-REGION-CAP is how many records fit above the
\ counter without spilling into the JIT code area at DBASE+DICT-SIZE. LCFPUSH
\ rejects (named rc-70) at depth == CFSTK-DEPTH-MAX, BEFORE the write, so a push
\ can never overflow the region (dot habu-cap-native-control-a5669829; the
\ opposite-direction sibling of the LCFPOP orphan-underflow guard). The cap is
\ min(region capacity, a sane ceiling) and always exceeds the checker's CFS cap
\ 31 (CF-PUSH marks UNCK past 31), so no checker-certified nesting is rejected.
DICT-SIZE CFSTK-OFF - 1 cells - CF-REC / constant CFSTK-REGION-CAP   \ 170 records fit
256 constant CFSTK-SANE-MAX                                          \ forward sanity ceiling
CFSTK-REGION-CAP CFSTK-SANE-MAX min constant CFSTK-DEPTH-MAX         \ 170 = min(region, sane)

\ Profiler counter band: one 64-bit sample counter per dictionary slot, reserved
\ at the very top of the DATA region [DATA-SIZE - PROF-CNT-BYTES, DATA-SIZE).
\ Sized from DICT-CAP so it always covers every slot BPROF-ON zeroes and EMIT-PROF
\ indexes (NDICT never exceeds DICT-CAP); src/habu/prof.f derives PROF-CNT (the band
\ base offset) as DATA-SIZE - PROF-CNT-BYTES. Grows in step with DICT-CAP with no
\ magic byte count, so the band can never fall short of the slots it serves.
DICT-CAP cells constant PROF-CNT-BYTES

25 constant SOURCE-HEADROOM-PCT
$400000 constant SOURCE-ARENA-CAP
SOURCE-ARENA-CAP constant IBUFSZ
\ The 2026-07-15 owner-persistence composite was 1,687,332 bytes; 25% headroom
\ required 2,109,165, so the smallest power of two selected SOURCE-ARENA-CAP.
\ The fixpoint regression enforces the policy from live stage2 and cold-prefix
\ measurements.
\ IBUFSZ overflow is labeled rc 74. Keep the bootstrap mirror token-identical.
20 constant DATA

0 constant DP-CELL
8 constant HND-CELL
16 constant LOCN-CELL
24 constant LOCF-CELL
$3000 constant LOCNAMES
24 constant LOC-REC
\ --- Friend arena (TFAM 2b-i): one contiguous write-protected band
\ [FRIEND-ARENA, FRIEND-ARENA+FRIEND-ARENA-LEN) holding the boot-seal latch plus
\ every checker/wordlist crown-jewel cell (CUR/WIDN/HOOK/DEF-WL, the TRUSTED:
\ TSIG/TCSIG/CRSIG signature cells, the package PKG-* cells, and the DEFER-*
\ cells). The latch cell IS the arena base: it holds 0 while the engine loads its
\ own canonical source (range guard inert) and FRIEND-ARENA-LEN once SEAL-FRIEND
\ runs at the end of the cold prefix. Self-sealing: post-seal any raw write into
\ the band — including the latch itself — is trapped fail-closed, so the seal is
\ a one-way monotonic latch. The band sits BELOW DATA-START, so allot/,/c,/the DP
\ heap (bounded >= DATA-START by DP-CHECK) can never reach it; only sinks that
\ store to a computed address (! c! +! atomic* patch32 snap-rebase, and syscall
\ write buffers) carry the runtime range check. The old scattered slots were
\ since reclaimed: $2780..$27A0/$27C0..$27E8 are free again after the pass-2
\ transaction moved into TXN-STATE-OFF; $27A8 remains CMM-CELL below ($1A0 stays
\ free). One more guarded band, the constructor protected-WID registry below,
\ is checked by the same PROT-GUARD.
\ The 18th cell (SEAL-NDICT-CELL, $A8) holds the seal-time ndict watermark (TFAM
\ 2b-iii). The latch is sealed EARLY (EMIT-SEAL-FRIEND, before the engine's own
\ checker/xref/stdlib source is even evaluated), so the watermark is captured
\ later by SEAL-CAPTURE (habu1.f BSEALCAP) tokens: a baseline at the end of
\ xref.f plus the cold-prefix assembler's token at the true engine-prefix end
\ (after script-argv.f), once ndict is the full engine boundary and no user
\ record exists yet.
\ The dictionary-truncation words (HIDE-DEFS-FROM/FORGET-DEFS-FROM, xref.f) reject
\ a post-seal FORGET below it. It lives inside the sealed band so user source
\ cannot lower the watermark to bypass the guard. ---
$20 constant FRIEND-ARENA               \ arena base offset within the DATA region (x20)
$90 constant FRIEND-ARENA-LEN           \ 18 cells: latch + 16 crown jewels + seal-ndict watermark
FRIEND-ARENA constant FRIEND-LATCH-CELL \ 0 = friend on/open, FRIEND-ARENA-LEN = sealed
$A8 constant SEAL-NDICT-CELL            \ seal-time ndict watermark (TFAM 2b-iii); 0 until SEAL-CAPTURE

\ ENGINE-ERROR:BAD-TAG is the runtime exit status when a compiled MATCH reaches its invalid-tag
\ fallback (TFAM 10 slice 3, docs/type-families.md §16/§24). The compiler emits a
\ self-contained die (write "hb: bad <family> tag\n" to fd 2 + NR-EXIT-GROUP) with
\ NO normal continuation at the tail of every MATCH. A well-typed scrutinee never
\ reaches it; a forged tag (TRUSTED constructor) exits deterministically with this
\ code. 85 is free repo-wide (the fixed engine exits are 64/67/69/70/71/74/75/76/
\ 77/78/83/84/127); it sits above the seal codes in the runtime-exit family.
67 constant UNCAUGHT-RC                 \ deterministic exit status for an uncaught top-level throw (BTHROW
                                        \ THROW-NOREC): the raw code was exit_group'd and kernel-masked to
                                        \ 8 bits, so a multiple of 256 exited 0 silently - fail-open. 67 is
                                        \ free repo-wide (64/70/71/74/76/78/83/84/127 are the other fixed
                                        \ engine exits; 69/77 collide with checker/lint codes).
$28 constant CUR-CELL
$30 constant WIDN-CELL
$38 constant HOOK-CELL
$40 constant DEF-WL-CELL
$48 constant TSIG-A-CELL
$50 constant TSIG-U-CELL
$58 constant TCSIG-A-CELL
$60 constant TCSIG-U-CELL
$68 constant CRSIG-A-CELL
$70 constant CRSIG-U-CELL
$78 constant PKG-PUB-CELL
$80 constant PKG-PRI-CELL
$88 constant PKG-PARENT-CELL
$90 constant PKG-REC-CELL
$98 constant DEFER-META-CELL
$A0 constant DEFER-XT-CELL
\ CMFAM-CELL: resolved construct family id, live only between the family and
\ variant operand tokens of one `construct` form (CMM-CELL state 1 -> 2; TFAM
\ 10 slice 2). Eager family resolution at the family token means no operand
\ string is stashed across a possible REPL line refill. $1B0 has no exact user
\ and no covering ranged region (GTOD-SCRATCH is $1E0..$1F0; the seal suite's
\ deliberate poke hole is $1A0 — left alone).
$1B0 constant CMFAM-CELL
\ MATCH-lowering compile state (TFAM 10 slice 3, docs §16). All DATA-relative
\ (x20), in the reclaimed $B0..$1B0 free band above the friend arena ($20..$B0)
\ and below CMFAM-CELL ($1B0) — rg-verified unused (the seal-suite poke hole $1A0
\ is left alone; the fam stack tops out at $D0+CMFR-MAX*8 = $1A0). CMBK-CELL is a
\ 64-bit branch-kind bitstack (J-OF pushes 0, EM-ADT-MATCH-OF pushes 1, J-ENDOF
\ pops+checks) so ENDOF re-arms the match token machine (CMM=4) only for a MATCH
\ variant branch, never a CASE arm or a nested case/match ENDOF — the compiler
\ analogue of the checker's CF-ENDOF-DISPATCH frame-kind routing. CMTAG/CMPADS
\ hold the pending variant (tag,M-p) between a variant token and its OF (never
\ nested: no token falls between them). CMFR is the nesting fam stack indexed by
\ CMFRD (match depth); a level's fam feeds later variant resolution and the
\ ;MATCH bad-tag family-name die. Definition-scoped: CMFRD/CMBK cleared at
\ colon/TRUSTED: entry and by EM-RESET-COMPILE-STATE alongside CMM-CELL.
$B0 constant CMBK-CELL                  \ ENDOF branch-kind bitstack (0=case arm, 1=match branch)
$B8 constant CMTAG-CELL                 \ pending MATCH variant tag (VAR -> OF)
$C0 constant CMPADS-CELL                \ pending MATCH variant zero pads M-p (VAR -> OF)
$C8 constant CMFRD-CELL                 \ MATCH nesting depth (0 = not in a match)
$D0 constant CMFR-OFF                   \ MATCH fam stack base (one cell per open match)
26 constant CMFR-MAX                    \ levels: $D0..$1A0 = 26 cells (checker caps CF frames at 30)
$1B8 constant BODYLEN-CELL
$1C0 constant RBASE-CELL
$1C8 constant LOOPSP-CELL
$1D0 constant S0-CELL
$3640 constant REPLH-CELL
$3648 constant RSAVCP-CELL
$3650 constant RSAVND-CELL
$3658 constant RSAVDP-CELL
$3660 constant RSAVSP-CELL
$3668 constant RRECP-CELL
$3670 constant ARGC-CELL
$3678 constant ARGV-CELL
$3680 constant ENVP-CELL
\ Fixed startup DATA offsets for the native argc and vector pointers.
\ Retirement: habu-builder-trust-rows-c5d41af6.
s" ARGC-CELL" s" -- n" TRUST
s" ARGV-CELL" s" -- n" TRUST
s" ENVP-CELL" s" -- n" TRUST
$3688 constant PEND-CELL
$3690 constant TKA-CELL
$3698 constant TKL-CELL
$36A0 constant INP-CELL
$36A8 constant INE-CELL
$36C0 constant BPA-CELL
$36D0 constant BPTAB-OFF
$37E8 constant BPWBASE-CELL
$37F0 constant BPWN-CELL
$43C0 constant EVAL-FRAME
$40 constant EVAL-FRAME-SIZE
$6 constant EVAL-FRAME-SHIFT
$10 constant EVAL-MAX-DEPTH
\ $2780..$27A8 (TSIG/TCSIG/CRSIG) relocated into the friend arena above.
\ RPKG-* / PKGRESYNC-CELL (dot habu-recovery-pkg-scope-e0bd98e2): the REPL-line
\ analogue of the eval-frame PKGSNAP band. EM-REPL-READ (LREAD) snapshots the five
\ live package-scope cells at line-start; EM-REPL-RECOVER (LRREC) restores them so a
\ compile error typed at the tty REPL rolls the open-package scope back to the
\ line-start scope, alongside the existing RSAVCP/RSAVND/RSAVDP/RSAVSP rollback.
\ PKGRESYNC-CELL is armed by both recovery legs and drained once at LMAIN
\ (EM-PKG-RESYNC): when the restored engine scope is global it resets the checker's
\ own package scope (checker-end-package) so engine and checker stay in step. These
\ six cells sit in the reclaimed $2780..$27C0 band (rg-verified unused repo-wide,
\ documented free above); small DATA-relative offsets, direct LDR/STR.
$2780 constant RPKG-CUR
$2788 constant RPKG-PUB
$2790 constant RPKG-PRI
$2798 constant RPKG-PARENT
$27A0 constant RPKG-REC
$27C0 constant PKGRESYNC-CELL
$27B0 constant DOESB-CELL
$27B8 constant TRUSTED-CELL
$37D0 constant EVALD-CELL
$37D8 constant EVALERR-CELL
$37E0 constant LMAINP-CELL
$3C88 constant TASK-TCB-CELL
$3C90 constant TASKS-LIVE-CELL
$3C98 constant HIDXP-CELL
\ HIDX:CLAIMS: how many hash-index slots have ever been claimed - live rows
\ plus the garbage that rollback churn leaves behind (a slot whose stale index
\ is re-covered when NDICT regrows). LHIDXADD compacts the table in place the
\ moment this count crosses HIDX:LOAD-MAX, so the count is exact by
\ construction: C-HIDX-INS increments it exactly when it claims an EMPTY slot,
\ and only the rebuild (which re-derives it from live [0,NDICT)) ever lowers
\ it. Lives at $27C8 in the reclaimed $27C0..$27D8 PKG-* band (rg-verified
\ unused repo-wide); engine-emitted code is its only writer. In a package for
\ the same reason DICT-WL is: new names have no reason to join this file's
\ global packaging debt.
package HIDX
public
$27C8 constant CLAIMS
;package
\ EVALREC-CELL: runtime address of the eval-frame throw-unwind entry (LEVALREC,
\ habu2.f), set at startup like LMAINP-CELL so the throw primitive (a leaf prim that
\ cannot name emit-time labels) can branch to it. It must sit in a DATA slot no
\ compiled source ever writes: $3A00..$3C88 is the lib/ffi-abi.f FFI buffer block
\ (FFI-BUF-OFF etc.), $3C88..$3CA0 is the task TCB cells; the protected-WID registry
\ count/table follow at $3CB8..$40C0 — so these $3CA0..$3CB8 slots are free engine
\ cells between the FFI block and the registry.
$3CA0 constant EVALREC-CELL
\ AOT-SEED-DONE-CELL: one-shot flag set the first time the post-cold-prefix AOT
\ seed runs at LEXIT (EM-COMPILE-EXIT), so REPL re-entry does not re-seed. Lives in
\ the $3CA0..$3CB8 free engine gap between the FFI block and the registry count cell.
$3CA8 constant AOT-SEED-DONE-CELL
\ AOT-SEED-ARM-CELL: set to 1 only on the interactive repl entry (C-SOURCE SRC-REPL),
\ so the AOT REPL seed runs solely when the engine is about to present the REPL --
\ never for pipe programs, `--load` tool runs, or the snapshot builder (which retires
\ the toolchain and runs SNAPGO before LEXIT). Zeroed by DATA-INIT for every boot.
$3CB0 constant AOT-SEED-ARM-CELL
\ --- protected-WID registry (TFAM 2b-v): count cell + u32 table. Records the WIDs of
\ sealed system / generated constructor packages created in the friend window;
\ PROT-WID? membership (habu1.f) gates the sealed-WID guards.
\
\ SHAPE: a WID-INDEXED BITMAP. Bit w of the $400-byte band at PROT-BITS-OFF is set
\ exactly when wordlist w is protected, so membership and insertion are O(1) and the
\ set can hold ANY subset of the WIDs the engine can index. It replaced a flat
\ 256-entry u32 append-only table (dot habu-replace-the-protected-ca920a8f). That
\ table's capacity was the number of public ADT families ONE PROCESS may declare --
\ a quantity unrelated to any resource the program controls -- and it was scanned
\ linearly by every sealed-WID guard. It filled at 246/256 on master, so the maki
\ suite's next public family died with an uncaught 7169 that named an innocent enum
\ in whatever file happened to declare next. Raising the number (16 -> 256 once
\ already, dot habu-seal-protwid-cap-6f1c9d2b) only moves that cliff.
\
\ CAPACITY: PROT-WID-MAX is now a WID BOUND, not a slot count: the highest wordlist
\ id + 1 that can ever be protected. The band is the SAME $3CC0..$40C0 the 256-slot
\ table occupied, so nothing above it moves, and $400 bytes of bitmap index 8192 WIDs
\ against the 700 a full maki suite run allocates. prot-wid-room reports
\ PROT-WID-MAX - WIDN, i.e. how many more wordlists may still be allocated AND
\ protected, so a declaration's preflight is exact instead of approximate, and
\ prot-wid-add names the bound itself when handed a WID at or above it. Growing the
\ bound later means widening the band upward into the free $40C8..$43C0 gap and
\ bumping UNCGH-CELL, exactly as the 16 -> 256 raise did.
\
\ TRANSITION: the tag cell ($3CB8) and the band base ($3CC0) are DELIBERATELY
\ UNCHANGED, because aot-capture.f ACAP-PWID-CAPTURE reads the LIVE metabuild host
\ registry at these offsets (via data-base) during the self-hosting build -- the host
\ is the PREVIOUS engine, so during the changeover a bitmap-era binary is built by a
\ table-era one. The cell that held the table's count now holds PROT-REG-TAG when the
\ band is a bitmap; a table-era engine leaves a count there, which is 0..256 and can
\ never collide with the tag. The capture reads the tag, takes the bitmap path on a
\ match and the legacy table path otherwise, and dies loudly on any third shape, so
\ the changeover cannot silently misread either lineage. The legacy reader retires
\ once the seed has rolled past the transition (dot habu-retire-the-legacy-31ad57bc).
\
\ The band stays engine-reserved -- no compiled source writes it, the DP heap is
\ bounded >= DATA-START (above it) and snapshot saves it. [PROT-REG-OFF,
\ +PROT-REG-LEN) is a SECOND range checked by PROT-GUARD, rejecting user data stores
\ into the tag cell, the bitmap, or the uncaught-throw hook. The code-emit sinks
\ cp!/ndict! (habu1.f BCPSET/BNDSET) ARE range-guarded too: each PROT-GUARDs the
\ address it redirects a write to, so a post-seal cp!/ndict! into either band fails
\ closed at the sink. ---
$3CB8 constant PROT-REG-TAG-CELL        \ bitmap-shape tag; UNCHANGED offset (aot-capture reads it live at build time)
$50574249544D4150 constant PROT-REG-TAG \ "PWBITMAP": written by every path that publishes the band; unreachable as a legacy count
$3CC0 constant PROT-BITS-OFF            \ protected-WID bitmap base; UNCHANGED offset (aot-capture reads it live)
8192 constant PROT-WID-MAX              \ WID bound: bits 0..8191 span $3CC0..$40C0, the exact band the 256-slot table held
PROT-WID-MAX 8 / constant PROT-BITS-BYTES
PROT-BITS-OFF PROT-BITS-BYTES + constant PROT-BITS-END
PROT-REG-TAG-CELL constant PROT-REG-OFF \ second PROT-GUARD band base (= tag cell)
PROT-BITS-END 1 cells + PROT-REG-OFF - constant PROT-REG-LEN  \ $410: tag + bitmap + UNCGH-CELL = $3CB8..$40C8
\ Legacy aliases: ONLY the transitional capture path may use these, to read a
\ table-era host's registry. Nothing in the running engine reads the band this way.
PROT-REG-TAG-CELL constant PROT-WID-LEGACY-N-CELL
PROT-BITS-OFF constant PROT-WID-LEGACY-OFF
256 constant PROT-WID-LEGACY-MAX
\ UNCGH-CELL: runtime address of the uncaught-top-level-throw reporter (LUNCAUGHT,
\ habu2.f), stored at boot (EM-STARTUP-RUNTIME-STATE) beside RRECP/EVALREC so the leaf
\ BTHROW primitive (which cannot name a habu2.f label) can branch to it when a throw
\ reaches THROW-NOREC with no handler and no REPL. Moved $3D00 -> $40C0 (above the grown
\ protected-WID band); not read live at build time so the relocation is safe.
\ Like EVALREC/AOT-SEED it is a fixed engine cell no compiled source writes (the mmap'd
\ DATA region is zero until boot).
$40C0 constant UNCGH-CELL
16 constant AOT-CREC-ROW
$4000 constant AOT-NAMES-CAP
\ Dict-name hash index: slots stay a power of 2 (LFIND probes with the
\ HIDX-SLOTS 1 - mask) and 2x DICT-CAP so the load factor stays <= 50%;
\ bytes = slots * 4 (u32 entries). Grown with DICT-CAP 32768: HIDX-SLOTS $10000
\ ($10000 = 65536 exceeds MOVZ imm16, so the three habu1.f probe sites load it
\ with LIT64), HIDX-BYTES $40000.
$10000 constant HIDX-SLOTS
$40000 constant HIDX-BYTES
\ HIDX:LOAD-MAX: the claimed-slot count at which LHIDXADD compacts the table
\ in place instead of letting chains grow toward a full wrap. Three quarters of
\ the file: strictly above DICT-CAP ($8000), so a compaction always lands the
\ count back at NDICT with room to spare and can never itself be at the bound,
\ and strictly below HIDX-SLOTS, so an insert always meets an empty slot within
\ its chain and the full-wrap path is structurally unreachable (it dies loudly
\ if reached; nothing silently disables the index any more - CG-25).
package HIDX
public
$C000 constant LOAD-MAX
;package
$36B8 constant FRCLM-CELL
$37F8 constant SNAP-CELL
$1D8 constant SSCR-CELL
$1E0 constant GTOD-SCRATCH
$200 constant VSP-CELL
$210 constant VTAG-OFF
$250 constant VVAL-OFF
32 constant VSMAX
$600 constant LOOP-STK-OFF
$800 constant BODYBUF-OFF
8000 constant BODYBUF-CAP
$568 constant RSP-CELL
$570 constant EXITH-CELL
$578 constant LVD-CELL
$580 constant LVH-OFF
$2C0 constant LVF-OFF
$560 constant LASTC-CELL
$1F0 constant DOESP-CELL
$230 constant CREATEP-CELL
$238 constant QPATCH-CELL
$240 constant QENT-CELL
$248 constant QXH-CELL
$250 constant DEF-TKA-CELL
$258 constant DEF-TKL-CELL
\ CMM-CELL: compile-loop ADT-lowering mode (TFAM 10, docs/type-families.md §16),
\ mirroring the checker's MM token machine: 0 = off; slices 2-3 arm it at a
\ `construct`/`MATCH` keyword so the operand tokens are captured BEFORE the
\ local/keyword/literal/call/undefined dispatch and never hit dictionary lookup.
\ Tested fail-closed at the LCOMPILE head (EM-COMPILE-ADT-MODE): armed with no
\ handler dies deterministically. Definition-scoped: cleared at colon/TRUSTED:
\ entry and by EM-RESET-COMPILE-STATE. Lives at $27A8, the last old CRSIG slot
\ (freed when CRSIG moved into the friend arena) between the reclaimed $27A0
\ slot and DOESB-CELL ($27B0) — rg-verified unused repo-wide. NOTE the low
\ "free hole" $260 is NOT usable: VVAL-OFF ($250) + VSMAX cells spans
\ $250..$350, and DEF-TKA/DEF-TKL survive inside it only because their liveness
\ is confined to the definition NAME token, when the virtual stack is empty.
$27A8 constant CMM-CELL
\ PKG-* ($27C0..$27D8), the old DEFER-META slot ($27E0), and the old
\ $2780..$27A0 pass-2 cells are reclaimed by the immutable lowering
\ transaction. The final old defer slot is the protected compile-immediate
\ preflight hook. The
\ checker installs it before arming HOOK-CELL; the compiler calls it before
\ every source-defined immediate in a checked body, and 0 is an invalid armed
\ state. It persists through snapshots like HOOK-CELL.
$27E8 constant COMPILE-PREFLIGHT-CELL
\ The retired descriptor-hook slot $27F0 is reused by TOP-HOOK-CELL below.
\ TOP-HOOK-CELL: top-row token hook xt (dot habu-typed-top-engine-2b2e88aa,
\ docs/typed-top-level.md §2.1). 0 = no hook: the interpret dispatch is
\ byte-for-byte today's behavior (tier 0). Installed only through the
\ fail-closed `set-top-check` prim (habu1.f BSETTOPCHECK), which mirrors
\ `set-check`/BSETCHECK's live-code install window; the cell is its own
\ PROT-GUARD band (habu1.f GUARD-SPAN/PROT-GUARD) so a post-seal raw store
\ traps ENGINE-ERROR:SEAL-VIOLATION exactly like the HOOK-CELL crown jewel. It sits in
\ the reclaimed band between TRUSTED-CELL ($27B8) and RSTK-OFF ($2800) and is
\ snapshot-persistent (< DATA-START) like HOOK-CELL. When installed, the
\ interpret dispatch points (habu2.f EM-INTERPRET-FIND / EM-INTERPRET-NUMBER
\ / the pushing string keywords / C-TICK / C-CHAR) emit one pre-continue
\ event per token through LTOPHOOK; the hook's effect is
\ ( ptr u8 n n n -- ): token addr, token len, class (TOP-EV-*), LFIND flags
\ (word/tick classes; 0 for literal classes).
$27F0 constant TOP-HOOK-CELL
\ ENGINE-SNAP-XT-CELL retains the first cold-prefix checker's snapshot-prepare
\ hook while a --build payload loads a second checker copy. Snapshot capture
\ invokes that first-copy hook before serializing DATA; capture and startup
\ clear it so no build-process code pointer reaches a restored image. $27F8 is
\ the final reclaimed cell before RSTK-OFF and is protected with TOP-HOOK-CELL.
$27F8 constant ENGINE-SNAP-XT-CELL
COMPILE-PREFLIGHT-CELL constant ENGINE-HOOK-OFF
3 cells constant ENGINE-HOOK-LEN
\ Top-row event class codes: the protocol between the interpret dispatch and
\ an installed top-row hook. Word/tick events pass the LFIND flag word
\ (bit 0 found, bit 1 DNAME-IMM, bits 8-15 DNAME-MIN-IN); literals pass 0.
1 constant TOP-EV-NUM       \ number literal pushed ( n )
2 constant TOP-EV-STR       \ s" / S\" literal pushed ( ptr u8 n )
3 constant TOP-EV-CSTR      \ c" / C\" counted literal pushed ( ptr )
4 constant TOP-EV-CHAR      \ char literal pushed ( n )
5 constant TOP-EV-TICK      \ ' pushed a found word's xt
6 constant TOP-EV-WORD      \ found word about to execute (pre-BLR)
$2800 constant RSTK-OFF
256 constant RSTK-CELLS
RSTK-OFF RSTK-CELLS cells + constant RSTK-END

\ ---- catch/throw handler frame (habu1.f BCATCH/BTHROW, habu2.f
\ EM-EVAL-THROW-RECOVER; bootstrap/cg/forth.fs mirror) ----
\ A HNDF-SIZE machine-stack frame chained through HND-CELL ([DATA+8]). Grown from
\ 48 to also save the user return-stack depth (RSP-CELL) and loop-stack depth
\ (LOOPSP-CELL) so a caught throw restores the COMPLETE caller execution frame
\ (dot habu-restore-complete-exec-abb8baca). Field offsets are byte offsets from
\ the frame base; every delivery site (BTHROW, LEVLD, seed BTHROW) reads them and
\ validates the sentinel + saved depths BEFORE any restore store. Keep this block
\ byte-for-byte in step with the bootstrap/cg/forth.fs mirror.
64 constant HNDF-SIZE                   \ frame bytes (was 48; +RSP +LOOPSP +MAGIC, 16-B aligned)
\ frame layout: 0 prev-HND | 8 data-sp(x19) | 16 machine-sp | 24 resume-pc
\               32 link | 40 saved-RSP | 48 saved-LOOPSP | 56 sentinel
BODYBUF-OFF LOOP-STK-OFF - 16 / constant LOOP-STK-FRAMES  \ 32 nested DO/LOOP frames (16 B each)
$CA7CF4A3E00D constant CATCH-FRAME-MAGIC \ frame sentinel: forged/adjacent-mutated frames fail closed

\ Compiler lowering transaction. All mutable pass-2 authority lives in one
\ engine band. The frozen source+certificate lives in a separately mmap'd,
\ maximum-target-page-rounded allocation whose base and capacity are held in
\ the protected state. It is read-only during replay and unmapped at commit.
$5000 constant TXN-STATE-OFF
$3000 constant TXN-STATE-LEN
$10000 constant PROT-PAGE-MAX

TXN-STATE-OFF       constant TXN-ACTIVE-CELL
TXN-STATE-OFF $8  + constant TXN-SRC-A-CELL
TXN-STATE-OFF $10 + constant TXN-SRC-U-CELL
TXN-STATE-OFF $18 + constant TXN-CERT-A-CELL
TXN-STATE-OFF $20 + constant TXN-CERT-U-CELL
TXN-STATE-OFF $28 + constant TXN-BIND-I-CELL
TXN-STATE-OFF $30 + constant P2-CELL
TXN-STATE-OFF $38 + constant TXN-WF-I-CELL
TXN-STATE-OFF $40 + constant P2BODY0-CELL
TXN-STATE-OFF $48 + constant P2INP-CELL
TXN-STATE-OFF $50 + constant P2INE-CELL
TXN-STATE-OFF $58 + constant P2DP-CELL
TXN-STATE-OFF $60 + constant P2W0-CELL
TXN-STATE-OFF $68 + constant P2W1-CELL
TXN-STATE-OFF $70 + constant P2W2-CELL
TXN-STATE-OFF $78 + constant P2W3-CELL
TXN-STATE-OFF $80 + constant P2LOC0-CELL
TXN-STATE-OFF $88 + constant TXN-FETCH-I-CELL
TXN-STATE-OFF $90 + constant TXN-BLOB-A-CELL
TXN-STATE-OFF $98 + constant TXN-BLOB-CAP-CELL
TXN-STATE-OFF $100 + constant TXN-LIVE-W-OFF
64 constant TXN-LIVE-W-CAP

\ --- Pre-trust defer pending table (dot habu-engine-pre-trust-77410827) ---
\ `defer NAME ( E )` declared in src/core/checker.f BEFORE `: TRUST`
\ (checker.f:7685) cannot run C-CALL-TRUST-PEND / C-CALL-CHECKER-DEFER, which
\ LFIND `trust`/`checker-defer` (undefined until 5208/7687) and die exit 70. When
\ they are absent C-DEFER instead COPIES the defer's qualified name + effect
\ signature into a slot of this fixed table; DRAIN-PRETRUST (called once in
\ checker.f right after `: TRUST`) replays both registrations for every slot.
\ Populated and drained entirely inside the checker.f prefix load, so the table is
\ empty by snapshot time; it sits at the TOP of the reserved region and bumps
\ DATA-START (protected-WID growth precedent) so no existing engine offset moves.
\ Overflow (and an over-long name/sig) dies at declaration; a non-empty table at
\ SEAL-CAPTURE dies (undrained backstop) — both fail-closed, named. Slot access
\ uses a computed band base (PD-TABLE-OFF > the DATA-relative scaled-imm range),
\ then small in-slot field offsets. Slot: [0]=name-len [8]=sig-len
\ [PD-NAME-OFF..)=name bytes [PD-SIG-OFF..)=sig bytes.
\ Protection class: UNGUARDED engine scratch (BODYBUF class, not a PROT-GUARD
\ band). A raw user store into it grants nothing beyond the public
\ `trust`/`checker-defer` words — the drain replays only name+sig slot copies —
\ and the table is empty whenever user source runs. test/protection-span.f pins
\ this class at the DATA-START edge.
48 constant PD-CAP                          \ pending slots: stage-2b pre-7687 needs ~20 (B5 + render/snapshot class); headroom
48 constant PD-NAME-CAP                      \ max qualified defer-name bytes per slot
64 constant PD-SIG-CAP                       \ max effect-signature bytes per slot
0  constant PD-NLEN-OFF                       \ in-slot: name length (u64)
8  constant PD-SLEN-OFF                       \ in-slot: sig length (u64)
16 constant PD-NAME-OFF                       \ in-slot: name bytes
PD-NAME-OFF PD-NAME-CAP + constant PD-SIG-OFF \ in-slot: sig bytes
PD-SIG-OFF PD-SIG-CAP + constant PD-SLOT      \ per-slot stride
8 constant PD-SLOTS-REL                       \ slots begin after the u64 band count cell
TXN-STATE-OFF TXN-STATE-LEN + constant PD-TABLE-OFF   \ band base (= old DATA-START); [0]=count
PD-TABLE-OFF PD-SLOTS-REL + PD-CAP PD-SLOT * + constant PD-TABLE-END

\ --- Package-scope eval-frame snapshot band (dot habu-recovery-pkg-scope-e0bd98e2) ---
\ A compile-error recovery must roll the OPEN-PACKAGE scope back to the boundary
\ scope, exactly as the eval frame rolls back CP/NDICT/DP/XDS: an in-package
\ definition aborted inside `evaluate` must NOT leave the package dangling open so
\ that later top-level defines land in it silently. Package scope is five live
\ cells (CUR/PKG-PUB/PKG-PRI/PKG-PARENT/PKG-REC); WIDN is monotonic and is NOT
\ rolled back (stale wids are harmless, like NDICT-truncated records). Unlike the
\ zeroed compile-state (EM-RESET-COMPILE-STATE), package scope can be legitimately
\ NON-zero across a boundary (a package open before the evaluate/REPL line), so it
\ is a boundary SNAPSHOT+RESTORE, not a reset. B-EVAL (habu1.f) saves the five
\ cells into PKGSNAP[EVALD]; the LEVALREC pop loop restores PKGSNAP[EVALD-1] per
\ escaped frame. Eight-cell power-of-2 stride (five used) so the slot address is a
\ shift, mirroring EVAL-FRAME-SHIFT. Transient (empty at rest / snapshot time). It
\ sits at the TOP of the reserved region and bumps DATA-START (PD-table / protected-
\ WID growth precedent) so no existing engine offset moves.
PD-TABLE-END constant PKGSNAP-OFF
$40 constant PKGSNAP-STRIDE              \ 8-cell slot (5 used), power-of-2 for shift addressing
6 constant PKGSNAP-SHIFT
0  constant PKGSNAP-CUR                   \ in-slot offsets, matching the C-PACKAGE cells below
8  constant PKGSNAP-PUB
16 constant PKGSNAP-PRI
24 constant PKGSNAP-PARENT
32 constant PKGSNAP-REC
40 constant PKGSNAP-USE                   \ in-slot: `using`-scope depth (slot 5 of the 8-cell stride)
PKGSNAP-OFF EVAL-MAX-DEPTH PKGSNAP-STRIDE * + constant PKGSNAP-END

\ --- `using`-scope import band (dot habu-using-import-pkg-a07dd7ba) ---
\ Consumer-side namespace import: `using NAME` makes package NAME's PUBLIC wordlist
\ visible to bare lookup until `;using`, `;package`, or the end of the load file.
\ Live state is a small fixed-capacity stack of public wordlist IDs plus a depth
\ counter, all in one DATA band above PKGSNAP (bumping DATA-START, the PKGSNAP /
\ protected-WID growth precedent, so no existing engine offset moves). The depth
\ cell is the single source of truth for how many usings are live: engine machine
\ code owns it (C-USING pushes, C-END-USING pops, `package`/`;package` and the
\ eval-frame / REPL boundaries save+restore it), and the checker reads the same
\ cell through `data-base USE-DEPTH-CELL +` so its parallel package-name mirror
\ (checker.f CHK-USE-NAMES) is bounded by the identical depth with no separate
\ counter to drift. The depth is always 0 at rest (usings are file-local and closed
\ before seal/snapshot), so the band is transient like PKGSNAP.
16 constant USE-MAX                       \ concurrent `using` capacity (E-code on overflow)
PKGSNAP-END constant USE-BAND-OFF
USE-BAND-OFF          constant USE-DEPTH-CELL      \ live using depth (u64)
USE-BAND-OFF 8 +      constant USE-PKG-SAVE-CELL   \ depth saved at `package` open (`;package` restores)
USE-BAND-OFF 16 +     constant USE-RPKG-SAVE-CELL  \ depth saved at REPL line start (recover restores)
USE-BAND-OFF 24 +     constant USE-WIDS-OFF        \ public-wid array base (USE-MAX u64 cells)
USE-WIDS-OFF USE-MAX cells + constant USE-BAND-END

\ --- snapshot relocation bookkeeping (dot habu-relocate-snapshot-region-752042fe) ---
\ Two tables that let a snapshot image be restored at a region address the
\ writing run never saw. Both live in the engine-reserved DATA band below
\ DATA-START, so the ordinary snapshot DATA copy carries them with no new image
\ section, and both are keyed by an OFFSET rather than an address, so their own
\ contents are the same in every run and never need canonicalising.
\ The engine half of this subsystem reopens this package in src/habu/habu2.f and
\ the snapshot writer's half reopens it in src/habu/snap-lib.f.
package SNAP-RELOC
public

\ Exit status for a corrupt call map: the loader found a recorded region-to-text
\ call site that does not hold a call instruction, so the image's region bytes and
\ its call map come from different builds or one of them is damaged. Relocating it
\ anyway would write a wild branch into live code, so the image is refused. It
\ lives here beside BL-RANGE-RC rather than in the
\ src/core/engine-error.f registry for the same reason those two do: the engine
\ emitter reads its exit statuses from this file while it is being compiled, one
\ generation before a new src/core constant would be reachable. 95 is the next
\ free status above that registry's last entry (94), and 96 and 97 follow it.
95 constant CALLMAP-RC
\ Exit status for an overfull address-cell table: more cells were declared to hold
\ a region address than XTCELL-CAP has room for. Continuing would silently drop a
\ cell and leave a stale writer-run address in a restored image, so the engine
\ stops instead.
96 constant XTCELL-RC
\ Exit status for a corrupt address-literal map: the loader found a recorded
\ address-literal site that does not hold the four-instruction MOVZ/MOVK chain the
\ compiler emits there, so the image's region bytes and its literal map come from
\ different builds or one of them is damaged. Rewriting the four immediates anyway
\ would plant a wild address in live code, so the image is refused.
97 constant ADDRMAP-RC
\ Exit status for a declared address cell that does not lie inside DATA. Every row
\ of the table below is a DATA offset, and the three passes that consume it -- the
\ writer's canonicalise, the loader's relocate, and the declaration itself -- index
\ DATA by that offset with no further arithmetic. An offset that is negative or
\ that reaches past the region makes all three read and write memory that is not
\ the cell anyone meant, so it is refused where it is first seen instead of being
\ carried into an image.
98 constant XTBAND-RC

\ Call-site map: one bit per four-byte word of the JIT region, recording every
\ call site whose callee lives in the engine's loaded __text instead of inside the
\ region. Those calls are the only instructions whose displacement is not the same
\ in the run that wrote a snapshot image and the run that restores it. A call from
\ one region address to another keeps its distance wherever the region is mapped;
\ a call from the region into __text does not, because the kernel picks the region
\ base and the loader picks the image base independently.
\ A site is recorded when the call is created, at the single call-emit chokepoint
\ (habu2.f EMIT-CEMITBL) and at the AOT call-site patcher (EM-AOT-PATCH-SITES), so
\ nothing ever has to recognise a call again by decoding region bytes -- which
\ could not be done soundly, because a compiled word may carry inline
\ non-instruction data.
\ The snapshot writer rewrites every recorded site to the displacement it would
\ have if the region sat exactly REGION-OFF above __text, and the loader rewrites
\ it again for the distance this run actually got.
\ The size is fixed and derived from REGION, so the map cannot overflow and needs
\ no capacity check: grow REGION and the map grows with it.
REGION 32 / constant CALLMAP-BYTES        \ one bit per region word (REGION / 4 / 8)
USE-BAND-END constant CALLMAP-OFF
CALLMAP-OFF CALLMAP-BYTES + constant CALLMAP-END

\ Address-literal map: the same shape as the call map, one bit per four-byte word
\ of the JIT region, recording the FIRST word of every four-instruction MOVZ/MOVK
\ chain the compiler builds an execution token with. Those are the quotation entry
\ address a `[: ... ;]` pushes and the target a `[']` or a `postpone` pushes; the
\ chain names a word's code, which lives either inside the region or in the
\ engine's loaded __text, and neither of those keeps its address between the run
\ that writes a snapshot image and the run that restores it.
\ A separate map rather than a second bit in the call map: a call site and a chain
\ start are different instruction shapes at different addresses, the two passes
\ rewrite completely different fields, and a two-bit call map would cost the same
\ bytes while forcing the already-proven call pass to decode a tag it does not
\ need. Two one-bit maps also let each pass keep the identical bit-scan loop.
\ Membership is recorded where the compiler decides the literal is an address of
\ CODE: habu2.f C-CODE-ADDR is the single emit point, and the AOT seed's code-
\ literal rebase (EM-AOT-RELOC-CODE) is the second producer, which knows its sites
\ are code literals because they come from the captured code-site list rather than
\ the data-site list. The sibling C-DATA-ADDR literals are deliberately NOT
\ recorded: they hold DATA addresses, and DATA is mapped at a fixed address in
\ every run, so they are already the same in the writing and the restoring run.
\ Nothing ever recognises a chain by looking at region bytes or at the value a
\ chain carries: a compiled word may hold inline non-instruction data, and an
\ ordinary integer may hold any value at all.
\ Sized from REGION like the call map, so it cannot overflow and needs no capacity
\ check, and keyed by region offset, so its own contents are run-invariant.
REGION 32 / constant ADDRMAP-BYTES        \ one bit per region word (REGION / 4 / 8)
CALLMAP-END constant ADDRMAP-OFF
ADDRMAP-OFF ADDRMAP-BYTES + constant ADDRMAP-END

\ Address-cell table: the DATA offset of every persisted cell that was DECLARED to
\ hold a JIT-region address. Region code moves between the run that writes an
\ image and the run that restores it, but DATA is mapped at a fixed address, so a
\ cell in DATA that points into the region is stale the moment the image is
\ restored somewhere else -- the crash is an immediate jump to the writing run's
\ address on the first deferred call.
\ Membership is recorded where the cell's kind is decided, never inferred from
\ what the cell happens to contain: the `defer` handler registers a dispatch cell
\ when it allocates it, the `is` handler registers the cell it is about to store
\ into, and the three engine hook cells are registered by name at cold boot
\ (habu2.f). Scanning DATA for values that fall in some address band would be a
\ guess -- an ordinary integer can hold any value at all -- and is deliberately
\ not what this does.
\ Layout: a count cell followed by XTCELL-CAP offset cells. The engine appends
\ only offsets that are not already present, so a cell registered by both `defer`
\ and `is` is listed once and is relocated once.
4096 constant XTCELL-CAP                  \ declared address cells one image may carry
ADDRMAP-END constant XTCELL-N-CELL        \ live count of used rows
XTCELL-N-CELL 8 + constant XTCELL-ROWS-OFF
XTCELL-ROWS-OFF XTCELL-CAP cells + constant XTCELL-END

\ The largest offset a declared cell may carry. All three consumers of a row --
\ the declaration itself (habu2.f EMIT-MARK), the writer's canonicalise
\ (snap-lib.f SND-CANON-XT-CELLS) and the loader's relocate (habu2.f EMIT-XT) --
\ add the offset to a DATA base and read or write the eight bytes there, so the
\ whole cell has to be inside the region. Comparing UNSIGNED against this bound
\ rejects a negative offset in the same instruction, because a negative offset is
\ an enormous unsigned one -- and negative is the shape the real defect produced:
\ a participant table grown into anonymous mmap sat about 4.6e12 bytes BELOW
\ data-base (dot habu-seal-the-declaration-7183177e).
\
\ Containment is the whole rule; the cell is deliberately NOT required to be on a
\ cell boundary. The DP heap does not cell-align what `create` hands out -- two
\ adjacent `create X 4 cells allot` tables measure at DATA offsets 5114415 and
\ 5114447, both 7 mod 8 -- and cell loads and stores at any byte address are
\ well defined on this target, so every declared cell in the tree is already
\ reached through an unaligned access that works. An alignment clause here would
\ reject the engine's own tables on the first `xt!`, which is exactly what it did
\ when this guard was first written with one.
DATA-SIZE 8 - constant XTCELL-OFF-MAX

;package

\ DATA-START: first offset of the user DP heap (allot/,/c,); everything below is
\ engine-reserved state (snapshot saves [0,DATA-START); DP-CHECK bounds the heap
\ >= DATA-START; task-user cells stop at EVAL-FRAME and sixteen evaluator
\ frames occupy $43C0..$47C0. The lowering state ends at $8000; the pre-trust defer
\ pending band follows, then the immutable lowering blob lives outside DATA.
SNAP-RELOC:XTCELL-END constant DATA-START

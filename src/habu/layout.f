\ layout.f - shared native image, dictionary, and snapshot layout constants.

20 constant XREG-RBASE
26 constant DBASE
27 constant NDICT
28 constant CP

$400000 constant REGION
$300000000 constant RBASE-VA
$48425350414E5321 constant SNAP-MAGIC

\ DICT-SIZE = CFSTK-OFF (= DICT-CAP * DREC record slots) + $1000 control-flow
\ stack; the code area follows at DBASE+DICT-SIZE inside the $400000 REGION.
\ Grown $61000 -> $C1000 with DICT-CAP 8192 -> 16384 (the gate-runner-support
\ tool closure needs ~9.5k records; dot habu-gate-runner-entry-81c84af0).
\ Keep DICT-CAP/CFSTK-OFF/DICT-SIZE/HIDX-SLOTS/HIDX-BYTES in step.
$C1000 constant DICT-SIZE
48 constant DREC
16 constant DNAME-INL
$0FFFFFFFFFFFFFFF constant DNAME-LEN-MASK
$1000000000000000 constant DNAME-IMM
$2000000000000000 constant DNAME-EXT
\ DNAME-WIDE (bit 62; 63 stays free): the word's recorded stack effect carries a
\ wider-than-cell layout value in some row, so executing it at INTERPRET level
\ would land a multi-cell bundle on the untyped interpret stack where scalar
\ dup/drop/swap silently corrupt it (dot habu-tfam-12-interpret-10b385b1).
\ LFIND folds the bit into x13 bit 3; EM-INTERPRET-FIND and interpret ' fail
\ closed on it. Set by xref.f XREF-WIDE-MARK; the checker marks at signature
\ record time once the sequenced src/core/checker.f half lands. Compile-mode
\ calls inside checked definitions are unaffected (pass-2 lowers them).
$4000000000000000 constant DNAME-WIDE
16384 constant DICT-CAP
$C0000 constant CFSTK-OFF
24 constant CF-REC
8 constant CF-LOCN
16 constant CF-LOCF

$180000 constant IBUFSZ   \ boot source-prefix + program input buffer; the copy
                           \ loops exit a SILENT 74 at this cap (habu2 SRC-SFAIL/
                           \ SRC-BFAIL) - grown 1M->1.5M when the src/core prefix
                           \ neared the wall (item 12 slice-3a; keep the bootstrap
                           \ mirror in cg/forth.fs in sync)
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
\ write buffers) carry the runtime range check. The old scattered slots ($1A0,
\ $260, $2780.., $27C0..) are now free holes. A SECOND guarded band (the
\ protected-WID registry, PROT-REG-OFF below) is checked by the same PROT-GUARD.
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
83 constant E-SEAL-VIOLATION            \ process exit status for a post-seal protected write
84 constant E-SEAL-PACKAGE              \ exit status for a sealed system-package open/reopen from user source
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
$3800 constant EVAL-FRAME
$40 constant EVAL-FRAME-SIZE
$6 constant EVAL-FRAME-SHIFT
$8 constant EVAL-MAX-DEPTH
\ $2780..$27A8 (TSIG/TCSIG/CRSIG) relocated into the friend arena above.
$27B0 constant DOESB-CELL
$27B8 constant TRUSTED-CELL
$37D0 constant EVALD-CELL
$37D8 constant EVALERR-CELL
$37E0 constant LMAINP-CELL
$3C88 constant TASK-TCB-CELL
$3C90 constant TASKS-LIVE-CELL
$3C98 constant HIDXP-CELL
\ EVALREC-CELL: runtime address of the eval-frame throw-unwind entry (LEVALREC,
\ habu2.f), set at startup like LMAINP-CELL so the throw primitive (a leaf prim that
\ cannot name emit-time labels) can branch to it. It must sit in a DATA slot no
\ compiled source ever writes: $3A00..$3C88 is the lib/ffi-abi.f FFI buffer block
\ (FFI-BUF-OFF etc.), $3C88..$3CA0 is the task cells above, and lib/task.f grows
\ TASK-USER-BASE up from $3D00 — so this $3CA0 slot in the $3CA0..$3D00 gap is the
\ single free engine cell between those two library regions.
$3CA0 constant EVALREC-CELL
\ AOT-SEED-DONE-CELL: one-shot flag set the first time the post-cold-prefix AOT
\ seed runs at LEXIT (EM-COMPILE-EXIT), so REPL re-entry does not re-seed. Lives
\ in the same $3CA0..$3D00 free engine gap as EVALREC-CELL, above the task cells.
$3CA8 constant AOT-SEED-DONE-CELL
\ AOT-SEED-ARM-CELL: set to 1 only on the interactive repl entry (C-SOURCE SRC-REPL),
\ so the AOT REPL seed runs solely when the engine is about to present the REPL --
\ never for pipe programs, `--load` tool runs, or the snapshot builder (which retires
\ the toolchain and runs SNAPGO before LEXIT). Zeroed by DATA-INIT for every boot.
$3CB0 constant AOT-SEED-ARM-CELL
\ --- protected-WID registry (TFAM 2b-v): count cell + u32 table. Placed in the same
\ proven-safe $3CA0..$3D00 engine gap as EVALREC/AOT-SEED-* (slots no compiled source
\ ever writes) -- NOT in the low friend arena, whose $A8+ tail is transient checker
\ scratch during stage-engine source evaluation. Records the WIDs of sealed system /
\ generated constructor packages created in the friend window; PROT-WID? membership
\ (habu1.f) gates the sealed-WID guards. u32 entries so wordlist IDs above 255 fit.
\ The band [PROT-REG-OFF, +PROT-REG-LEN) fills $3CB8..$3D00 (below TASK-USER-BASE) and
\ is a SECOND range checked by PROT-GUARD, rejecting user data stores into the count
\ cell or table. The code-emit sinks cp!/ndict! (habu1.f BCPSET/BNDSET) ARE
\ range-guarded too: each PROT-GUARDs the address it redirects a write to, so a
\ post-seal cp!/ndict! into either band fails closed at the sink. ---
$3CB8 constant PROT-WID-N-CELL          \ protected-WID count (u32 in a full cell)
$3CC0 constant PROT-WID-OFF             \ protected-WID table base (PROT-WID-MAX u32 entries)
16 constant PROT-WID-MAX                \ table capacity (16 u32 = $40 -> fills the gap to $3D00)
PROT-WID-N-CELL constant PROT-REG-OFF   \ second PROT-GUARD band base (= count cell)
PROT-WID-OFF PROT-WID-MAX 4 * +  PROT-REG-OFF -  constant PROT-REG-LEN  \ $48: count + table
\ UNCGH-CELL: runtime address of the uncaught-top-level-throw reporter (LUNCAUGHT,
\ habu2.f), stored at boot (EM-STARTUP-RUNTIME-STATE) beside RRECP/EVALREC so the leaf
\ BTHROW primitive (which cannot name a habu2.f label) can branch to it when a throw
\ reaches THROW-NOREC with no handler and no REPL. Sits at $3D00 - the slot directly
\ below the task-user region, which now starts at $3D08 (lib/task.f TASK-USER-BASE).
\ Like EVALREC/AOT-SEED/PROT-WID it is a fixed engine cell no compiled source writes
\ (task-user cells allocate up from $3D08; the mmap'd DATA region is zero until boot).
$3D00 constant UNCGH-CELL
\ Dict-name hash index: slots stay a power of 2 (LFIND probes with the
\ HIDX-SLOTS 1 - mask) and 2x DICT-CAP so the load factor stays <= 50%;
\ bytes = slots * 4 (u32 entries). Grown with DICT-CAP 16384.
$8000 constant HIDX-SLOTS
$20000 constant HIDX-BYTES
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
\ DEF-WL ($260), PKG-* ($27C0..$27D8) and DEFER-* ($27E0..$27E8) relocated into
\ the friend arena above; those low/high slots are now free holes.
$2800 constant RSTK-OFF
$4000 constant DATA-START

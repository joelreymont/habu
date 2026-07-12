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
                           \ loops + EMIT-SOURCE-READ exit a LABELED rc-74 at this
                           \ cap ("hb: source prefix buffer full", habu2 SRC-SFAIL/
                           \ SRC-BFAIL/sbufull) - grown 1M->1.5M when the src/core
                           \ prefix neared the wall (item 12 slice-3a; keep the
                           \ bootstrap mirror in cg/forth.fs in sync)
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
\ free). Two more guarded bands (the constructor protected-WID registry and the
\ sealed-owner WID registry below) are checked by the same PROT-GUARD.
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
\ E-BAD-TAG: runtime exit status when a compiled MATCH reaches its invalid-tag
\ fallback (TFAM 10 slice 3, docs/type-families.md §16/§24). The compiler emits a
\ self-contained die (write "hb: bad <family> tag\n" to fd 2 + NR-EXIT-GROUP) with
\ NO normal continuation at the tail of every MATCH. A well-typed scrutinee never
\ reaches it; a forged tag (TRUSTED constructor) exits deterministically with this
\ code. 85 is free repo-wide (the fixed engine exits are 64/67/69/70/71/74/75/76/
\ 77/78/83/84/127); it sits above the seal codes in the runtime-exit family.
85 constant E-BAD-TAG
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
\ PROT-WID? membership (habu1.f) gates the sealed-WID guards. u32 entries so wordlist
\ IDs above 255 fit. Each PUBLIC ADT family consumes ONE slot (xref.f PROT-WID-CTOR-ADD
\ -> prot-wid-add per family constructor wordlist), so the capacity is the number of
\ public ADT families a session may declare. Raised 16 -> 256 (dot
\ habu-seal-protwid-cap-6f1c9d2b): 16 overflowed at the 17th public family (silent
\ exit 84), and a realistic switchover (a public stdlib plus user Option/Result/...
\ families) declares dozens-to-hundreds. The count cell ($3CB8) and table base ($3CC0)
\ are DELIBERATELY UNCHANGED: aot-capture.f ACAP-PWID-CAPTURE reads the LIVE metabuild
\ host registry at these offsets (via dbase@) during the self-hosting build, so moving
\ them would make the transitional build read the old-layout host at a new offset (a
\ garbage count -> "protected-WID registry overflow"). Instead the 256-slot table
\ ($3CC0..$40C0) grows UPWARD and UNCGH-CELL/TASK-USER-BASE/DATA-START are bumped above
\ it; those cells are not read live at build time, so relocating them is safe. It stays
\ engine-reserved -- no compiled source writes it, the DP heap is bounded >= DATA-START
\ (above the table) and snapshot saves it. The band [PROT-REG-OFF, +PROT-REG-LEN) is a
\ SECOND range checked by PROT-GUARD, rejecting user data stores into the count cell,
\ table, or uncaught-throw hook. The code-emit sinks cp!/ndict! (habu1.f BCPSET/BNDSET)
\ ARE range-guarded too:
\ each PROT-GUARDs the address it redirects a write to, so a post-seal cp!/ndict! into
\ either band fails closed at the sink. ---
$3CB8 constant PROT-WID-N-CELL          \ protected-WID count (u32); UNCHANGED offset (aot-capture reads it live at build time)
$3CC0 constant PROT-WID-OFF             \ protected-WID table base (PROT-WID-MAX u32); UNCHANGED offset (aot-capture reads it live)
256 constant PROT-WID-MAX               \ table capacity (256 u32 = $400, spans $3CC0..$40C0); raised from 16 (dot habu-seal-protwid-cap-6f1c9d2b)
PROT-WID-N-CELL constant PROT-REG-OFF   \ second PROT-GUARD band base (= count cell)
PROT-WID-OFF PROT-WID-MAX 4 * +  1 cells +  PROT-REG-OFF -  constant PROT-REG-LEN  \ $410: registry + UNCGH-CELL = $3CB8..$40C8
\ UNCGH-CELL: runtime address of the uncaught-top-level-throw reporter (LUNCAUGHT,
\ habu2.f), stored at boot (EM-STARTUP-RUNTIME-STATE) beside RRECP/EVALREC so the leaf
\ BTHROW primitive (which cannot name a habu2.f label) can branch to it when a throw
\ reaches THROW-NOREC with no handler and no REPL. Moved $3D00 -> $40C0 (above the grown
\ 256-slot protected-WID table); not read live at build time so the relocation is safe.
\ Like EVALREC/AOT-SEED it is a fixed engine cell no compiled source writes (the mmap'd
\ DATA region is zero until boot).
$40C0 constant UNCGH-CELL
\ --- sealed-owner WID registry: count plus atomic u32 (public,private) rows.
\ This registry is distinct from the constructor protected-WID table above: owner
\ role checks must distinguish callable public WIDs from inaccessible private WIDs,
\ while constructor protection keeps its existing flat-table ABI. The band starts
\ immediately after the sixteen evaluator frames ($43C0..$47C0) and ends before
\ the lowering transaction at $5000. A 256-row table occupies $808 bytes and leaves
\ $38 bytes of separation, so no runtime scratch range moves and old constructor
\ offsets remain byte-for-byte stable. PROT-GUARD treats this as its own protected
\ interval; the hidden mutator stores each aligned pair atomically, then
\ release-publishes the count consumed by acquire scans. Cold entry clears the
\ count and every row before any test-only build hook runs. ---
$47C0 constant OWNER-WID-N-CELL
$47C8 constant OWNER-WID-OFF
8 constant OWNER-WID-ROW
0 constant OWNER-WID-PUB
4 constant OWNER-WID-PRI
256 constant OWNER-WID-MAX
OWNER-WID-OFF OWNER-WID-MAX OWNER-WID-ROW * + constant OWNER-WID-END
OWNER-WID-N-CELL constant OWNER-REG-OFF
OWNER-WID-END OWNER-REG-OFF - constant OWNER-REG-LEN
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
\ PKG-* ($27C0..$27D8), DEFER-* ($27E0..$27E8), the retired descriptor hook at
\ $27F0, and the old $2780..$27A0 pass-2 cells are reclaimed by the immutable
\ lowering transaction.
$2800 constant RSTK-OFF

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

\ DATA-START: first offset of the user DP heap (allot/,/c,); everything below is
\ engine-reserved state (snapshot saves [0,DATA-START); DP-CHECK bounds the heap
\ >= DATA-START; task-user cells stop at EVAL-FRAME and sixteen evaluator
\ frames occupy $43C0..$47C0. The lowering state ends exactly at $8000; its
\ immutable variable-sized blob is outside DATA and cannot alias the user heap.
TXN-STATE-OFF TXN-STATE-LEN + constant DATA-START

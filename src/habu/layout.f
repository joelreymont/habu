\ layout.f - shared native image, dictionary, and snapshot layout constants.

20 constant XREG-RBASE
26 constant DBASE
27 constant NDICT
28 constant CP

$400000 constant REGION
$300000000 constant RBASE-VA
$48425350414E5321 constant SNAP-MAGIC

$61000 constant DICT-SIZE
48 constant DREC
16 constant DNAME-INL
$0FFFFFFFFFFFFFFF constant DNAME-LEN-MASK
$1000000000000000 constant DNAME-IMM
$2000000000000000 constant DNAME-EXT
8192 constant DICT-CAP
$60000 constant CFSTK-OFF
24 constant CF-REC
8 constant CF-LOCN
16 constant CF-LOCF

$100000 constant IBUFSZ
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
\ protected-WID registry, PROT-REG-OFF below) is checked by the same PROT-GUARD. ---
$20 constant FRIEND-ARENA               \ arena base offset within the DATA region (x20)
$88 constant FRIEND-ARENA-LEN           \ 17 cells: latch + 16 crown jewels
FRIEND-ARENA constant FRIEND-LATCH-CELL \ 0 = friend on/open, FRIEND-ARENA-LEN = sealed
83 constant E-SEAL-VIOLATION            \ process exit status for a post-seal protected write
84 constant E-SEAL-PACKAGE              \ exit status for a sealed system-package open/reopen from user source
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
\ cell or table. Code-emit sinks (cp!/ndict!) are not yet range-guarded; closing that
\ bypass for both bands is dot habu-range-reject-cp-e2eed7e4. ---
$3CB8 constant PROT-WID-N-CELL          \ protected-WID count (u32 in a full cell)
$3CC0 constant PROT-WID-OFF             \ protected-WID table base (PROT-WID-MAX u32 entries)
16 constant PROT-WID-MAX                \ table capacity (16 u32 = $40 -> fills the gap to $3D00)
PROT-WID-N-CELL constant PROT-REG-OFF   \ second PROT-GUARD band base (= count cell)
PROT-WID-OFF PROT-WID-MAX 4 * +  PROT-REG-OFF -  constant PROT-REG-LEN  \ $48: count + table
$4000 constant HIDX-SLOTS
$10000 constant HIDX-BYTES
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

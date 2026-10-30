\ layout.f - shared native image, dictionary, and snapshot layout constants.
\ The cold prefix loads stack-abi.f first; require is not defined at this stage.

20 constant XREG-RBASE
26 constant DBASE
27 constant NDICT
28 constant CP

\ The general registers the running engine occupies.
\ The authority: a compiled routine that writes one of these destroys the engine.
\ src/compiler/native-effect.f derives its GPR set by removing this mask, so a
\ register claimed here is refused by every GPR allocation path.
\ ENGINE-GPR:A64-DSTACK is stated here, not read from src/arch/arm64/mnem.f XDS:
\ mnem.f is not in every engine's prefix, so a derivation compiles in the build
\ and dies E-UNDEFINED at run time. src/habu/rt.f RT:DSTACK-AGREE dies when the
\ two differ. ENGINE-GPR:X64-DSTACK is read here by src/arch/x86-64/machine.f
\ X64M:DSTACK-GPR and src/arch/x86-64/rt.f.
\ Both targets' registers are declared on every target: this file cannot branch
\ at top level and an ARM64 engine emits x86-64 code. DSTACK and MASK select the
\ running target's pair at run time and refuse a target that has neither.
package ENGINE-GPR

\ src/core/cell.f's CORE-LAYOUT-RC, stated here because the Gforth recovery's
\ stack check (test/bootstrap-engine-stack.fs) loads this file without cell.f.
$4C constant UNKNOWN-TARGET-RC

public

19 constant A64-DSTACK

1 A64-DSTACK lshift
1 XREG-RBASE lshift or
1 DBASE lshift or
1 NDICT lshift or
1 CP lshift or
constant A64-MASK

\ docs/xcomp.md "Machine model" fixes these six, and this is their one statement:
\ src/compiler/native/x64ir.f RESERVED-MASK reads X64-MASK and adds rsp, which
\ the machine holds and the engine does not. X64-DSTACK is A64-DSTACK's twin;
\ each of the four after rbx is the twin of the ARM64 constant at the head of
\ this file named beside it; rbx has none. The x86-64 boot
\ (src/habu/boot-x64.f) sets each register by its name here.
12 constant X64-DSTACK
3 constant X64-INTERP          \ rbx, the interpreter register
5 constant X64-RBASE           \ rbp, the user area: XREG-RBASE, x20
13 constant X64-DBASE          \ r13, the code region the records live in: DBASE
14 constant X64-NDICT          \ r14, the dictionary record count: NDICT
15 constant X64-CP             \ r15, the code pointer: CP

1 X64-INTERP lshift
1 X64-RBASE lshift or
1 X64-DSTACK lshift or
1 X64-DBASE lshift or
1 X64-NDICT lshift or
1 X64-CP lshift or
constant X64-MASK

: DSTACK ( -- n )
   HB-TARGET-LINUX-X86-64? if X64-DSTACK exit then
   HB-TARGET-LINUX? HB-TARGET-MACOS? or if A64-DSTACK exit then
   s" layout: ENGINE-GPR: unknown target" UNKNOWN-TARGET-RC die ;

: MASK ( -- n )
   HB-TARGET-LINUX-X86-64? if X64-MASK exit then
   HB-TARGET-LINUX? HB-TARGET-MACOS? or if A64-MASK exit then
   s" layout: ENGINE-GPR: unknown target" UNKNOWN-TARGET-RC die ;

;package

\ Fixed budget for dictionary and code, including the retained build host and
\ its replacement core. The larger record band leaves the code band at its
\ measured 30,404,608 bytes. Keep the Gforth recovery mirror in step.
$2300000 constant REGION
\ RBASE-VA: the snapshot's canonical region base, a fixed sentinel, not the map
\ address. The live region maps at __text base + REGION-OFF (habu2.f
\ EM-MMAP-CODE-REGION) to keep calls inside BL's +/-128 MiB; snap-rebase
\ canonicalizes region pointers to this sentinel and the loader rebases them, so
\ images are byte-identical across runs.
$300000000 constant RBASE-VA
\ Canonical code-region hint past __text. The engine clamps it after the loaded
\ image and checks the resulting mapping against BL-REACH at boot.
$1000000 constant REGION-OFF
\ BL-REACH: AArch64 BL imm26 reach (+/-128 MiB). The boot dies BL-RANGE-RC when
\ the mapped region falls outside it from __text; direct-BL emission relies on it.
$8000000 constant BL-REACH
81 constant BL-RANGE-RC
$48425350414E5321 constant SNAP-MAGIC
\ The outer format version belongs to src/habu/snapshot-format.f, loaded
\ independently of this potentially cached layout by every writer and reader.

\ Snapshot trailer geometry; this is its single owner.
\ The trailer ends the authenticated text extent: its base is (the header's
\ text-size field + IMAGE-TEXT-TRAILER-ADJ) - SNAP-TRL-BYTES; the codesign blob
\ and page padding follow outside the extent. Every trailer reader and writer
\ derives size and offsets from here (src/habu/snap-lib.f SNAP:WRITE-BYTES,
\ src/habu/habu2.f EM-SNAPSHOT-RESTORE, tools/imgdump.f,
\ test/snapshot-writer.f). The cell at offset 8 changed meaning with the outer
\ format: src/habu/snapshot-format.f names it (SNAPSHOT-FORMAT:HEAP-FIELD).
48 constant SNAP-TRL-BYTES        \ magic, heap form, ndict, region len, data len, version
16 constant SNAP-TRL-NDICT        \ dictionary record count
24 constant SNAP-TRL-REGLEN       \ region payload length
32 constant SNAP-TRL-DATALEN      \ data payload length
40 constant SNAP-TRL-VERSION      \ SNAPSHOT-FORMAT:VERSION of the writing engine
\ The pre-version trailer. Only the loader may name it, and only to recognise a
\ legacy image and fail it closed rc 80 instead of misreading its fields.
40 constant SNAP-TRL-LEGACY-BYTES

\ DICT-SIZE = CFSTK-OFF (= DICT-CAP * DREC record slots) + $1000 control-flow
\ stack; the code area follows at DBASE+DICT-SIZE inside the REGION.
\ Keep DICT-CAP/CFSTK-OFF/DICT-SIZE/HIDX-SLOTS/HIDX-BYTES in step; dictionary
\ growth spends code-band bytes (CODE-BAND:BYTES). Maki's STEP import needs over
\ 65,536 checked records.
$601000 constant DICT-SIZE
\ CODE-BAND:BYTES: what REGION leaves for emitted code, [DBASE+DICT-SIZE,
\ DBASE+REGION). The two bands share one budget: lifting DICT-CAP alone once
\ left 36 KB of code band. A core rebuild retains its running compiler while
\ compiling the replacement; the 35 MiB region gives the two generations
\ 30,404,608 code bytes (a 10 MiB region left 7,335,936 and failed the guarded
\ core rebuild at EM-COMPILE-UNDEF). The budget is fixed at run time; anonymous
\ pages populate on demand and snapshots write only [SDB, CP).
package CODE-BAND
public
REGION DICT-SIZE - constant BYTES
;package
48 constant DREC
16 constant DNAME-INL
1 constant OWNER-API-PUB-WID
2 constant OWNER-API-PRI-WID
3 constant FIRST-DYNAMIC-WID
\ The wordlist cell's three non-wordlist values. A package row keeps its public
\ and private wids in the cells where a word keeps its code span; it is
\ NAMESPACE while live and RETIRED-NAMESPACE once a rollback retires it
\ (habu1.f EM-DEF-OCC:LRETIRE), so one aligned store retires it and every code
\ span reader skips it by either value (xref.f XREF:PACKAGE-WL?).
\ A record's wordlist cell is half the hash index's key (habu1.f C-HIDX-INS keys
\ on the folded name XOR this cell). Publication writes the cell before the
\ insert, and HIDX-EMIT:LREBUILD keys every record on its cell as it stands.
\ NAMESPACE keeps that rule: a package row carries it from birth, so LFIND's
\ qualifier probe finds it. RETIRED breaks it: src/habu/xref.f XREF:RETIRE
\ stamps a record already in the table, which stays on its old wid's chain until
\ a rebuild. A lookup on a real wid still agrees with a scan, but one keyed on
\ RETIRED cannot be answered by the table (retiring one name twice puts two rows
\ under one key), so habu1.f BSWL keeps its linear scan for this wid.
\ RETIRED-NAMESPACE leaves a row on NAMESPACE's chain the same way, and no
\ lookup is keyed on it.
\ habu2.f DEFWRITE:RECORD-WID writes a logged wid back after a retirement; a
\ rebuild between them re-keys the row on RETIRED's chain, so record-wid!
\ indexes the record under each wid it writes (HIDX-EMIT:LREKEY).
package DICT-WL
public
-1 constant NAMESPACE
-2 constant RETIRED
-3 constant RETIRED-NAMESPACE
;package

\ WID:MAX is the largest value a wordlist id may take; it bounds every
\ WID-indexed table and the snapshot's registry validation.
package WID
public
$FFFFFFFE constant MAX
;package
\ A `constant` or `create`d word pushes one decided value, and a compiled mention
\ means that value. Only the definer knows this: the value lives in emitted
\ instructions the region rule below forbids reading, and running a word to ask
\ is its side effects. The definer stamps the kind; every reader asks the record.
\ The two-bit kind is an enum: a `constant` pushes a number, a `create`d or
\ `variable` word a DATA address a snapshot must relocate, so folding the second
\ as the first bakes an address a restore cannot move. CAST: uses the remaining
\ kind for a declared identity retype: its body stays callable, but a compiled
\ mention only renames the checked value.
\ `does>` (habu2.f DOESPATCH:EMIT) clears the bits in the window it patches the
\ RET, on the record and every newer record with the same code pointer (an
\ `EXPORT` alias copies DKIND:VAL), retired ones up to NDICT-HW-CELL included,
\ since a raising `ndict!` can re-expose them. A stamped record's body is the
\ definer's own.
\ Band: bits 50-51 of the record flags cell [16] (map at DNAME-MIN-IN-MASK).
\ The AOT seed carries the pair in a byte of its compact record
\ (src/habu/aot-capture.f). The recovery CAST: definer stamps the same kind, so
\ bootstrap/cg's name readers clear the same bits.
package DKIND
public
$0004000000000000 constant VAL       \ kind 1: the body pushes a decided number
$0008000000000000 constant ADDR      \ kind 2: the body pushes its DATA address
VAL ADDR or constant CAST           \ kind 3: a declared identity retype
VAL ADDR or constant MASK
;package
\ DNAME-ALLOC: the aligned DATA allocation start of a created word, defined in
\ src/habu/created-body-abi.f, which the checker reads too.
CREATED-BODY-ABI:DNAME-ALLOC constant DNAME-ALLOC
\ DNAME-CELL names the exact one-cell DATA extent made by variable. CREATE
\ leaves it clear: a created body may allot bytewide storage.
$0001000000000000 constant DNAME-CELL
\ DNAME-REP (bit 46) marks a CAST: record (kind 3 above) whose certified
\ output mints no pointer and no quotation: a representation cast, a scalar
\ retype the native compiler lowers as the identity it is. The checker sets it
\ through rep-mark when CAST-CERTIFY finds no mint; scoped view casts and
\ LINEAR: records never carry it. Capture carries it in the compact record's
\ kind byte (src/habu/aot-capture.f) and a native unit object row in its flags.
$0000400000000000 constant DNAME-REP
\ The flag bits above the name length in [16], bits 46-63. Every native length
\ read clears them by this count (LSLI then LSRI). Two files load before this
\ one and spell the mask as a literal: src/core/checker.f CK-REC-LEN-MASK and
\ src/habu/engine-origin-early.f RECORD-LEN-MASK. A flag carved from the length
\ is this edit and those two.
18 constant DNAME-FLAG-BITS
-1 DNAME-FLAG-BITS rshift constant DNAME-LEN-MASK
\ DNAME-MIN-IN (bits 52-59): certified minimum input arity in cells, poked at
\ certification (checker RECMI latch -> publish tails / seal-time internal-mark
\ pass). LFIND folds the byte into x13 bits 8-15; EM-INTERPRET-FIND fails closed
\ before the BLR when the interpret stack holds fewer cells. 0 = unmarked
\ (unchecked words and words without signatures). Compiled calls inside checked
\ words are checker-proven and carry no guard. Flags cell [16]: IMM/EXT/WIDE/INT
\ 60-63, min-in 52-59, definer kind 50-51, DNAME-OWNED 49, DNAME-CELL 48,
\ DNAME-ALLOC 47, DNAME-REP 46, name length 0-45.
$0FF0000000000000 constant DNAME-MIN-IN-MASK
$1000000000000000 constant DNAME-IMM
$2000000000000000 constant DNAME-EXT
\ DNAME-WIDE (bit 62): the word's recorded effect carries a wider-than-cell
\ value in some row, so running it at interpret level would leave a multi-cell
\ bundle on the untyped interpret stack. LFIND folds the bit into x13 bit 3;
\ EM-INTERPRET-FIND and interpret ' fail closed on it. Set by the `wide-mark`
\ prim (habu1.f BWIDEMARK). Compiled calls inside checked definitions are
\ unaffected.
$4000000000000000 constant DNAME-WIDE
\ DNAME-INT (bit 63): an engine-prefix COLON record with no checker-known effect
\ (no certified or trusted signature, no primitive axiom), set by the seal-time
\ pass (src/core/internal-mark.f) so the executable top-level names equal the
\ checker's. Data records are exempt: push-only bodies, auto-trusted by
\ LASTC-TRUST:PUBLISH whenever a hook is installed. LFIND folds the bit into x13
\ bit 4; EM-INTERPRET-FIND and interpret ' fail closed with `hb: internal engine
\ word: <token>` rc 70. Compile-mode references (unchecked code, TRUSTED:
\ bodies) are declared trusted boundaries and unaffected.
$8000000000000000 constant DNAME-INT
\ DNAME-OWNED (bit 49): an internal primitive its owner's rows type. A package
\ row (src/habu/prims.f EPPRIM: ... ECLOSE-PRIVATE) states its effect, so
\ ENGINE-PRIMS:DNAME stamps the bit beside DNAME-INT. It opens nothing
\ DNAME-INT closes: the prompt, `'`, search-wl and `[']` still refuse the
\ record. Only the compile guards read it (habu2.f C-COMPILE-CALL-GUARD at tier
\ 0; at tier 1 src/compiler/native/dict.f NDICT:INT-CALL?, which CALL-BINDING
\ and src/compiler/native/hir-word.f RESOLVE-SITE ask), and with it a compiled
\ call is the checker's decision through the rows: the owner's checked callers
\ compile, and the global trusted-only row refuses a checked caller elsewhere.
\ DNAME-INT without it keeps the compile refusal outside a TRUSTED: body.
$0002000000000000 constant DNAME-OWNED
\ DICT-CAP: dictionary record slots. 131072 exceeds the imm16 field, so the
\ DICT-CAP comparison sites (habu2.f, bootstrap/cg/forth.fs) load it with LIT64,
\ one instruction for a single-chunk value; asm.f ?IMM16 dies on a missed site.
131072 constant DICT-CAP
$600000 constant CFSTK-OFF
32 constant CF-REC
8 constant CF-LOCN
16 constant CF-LOCF
24 constant CF-KIND
\ CF-KIND names the structure that pushed the entry, so a closer refuses an
\ entry it does not close before popping or patching (habu2.f C-CF-AT): `begin`
\ and `if` both record a CP. One kind serves every opener the same closers
\ accept: `else` leaves an IF entry, `?do` a DO entry, a match variant arm an OF.
1 constant CFK-IF      \ if, else: closed by then or else
2 constant CFK-BEGIN   \ begin: closed by until, again, or by repeat under a WHILE
3 constant CFK-WHILE   \ while, opened on a BEGIN: closed by repeat
4 constant CFK-DO      \ do, ?do: closed by loop or +loop
5 constant CFK-CASE    \ case: closed by endcase through its ENDOF entries
6 constant CFK-OF      \ of, a match variant arm: closed by endof
7 constant CFK-ENDOF   \ endof: closed by endcase or ;match, continued by of
8 constant CFK-MATCH   \ match: closed by ;match through its ENDOF entries
9 constant CFK-QUOT    \ [: closed by ;]
\ Control-flow stack region [CFSTK-OFF, DICT-SIZE): cell 0 is the depth counter,
\ then CF-REC-byte records. CFSTK-REGION-CAP is how many records fit above the
\ counter without spilling into the JIT code area at DBASE+DICT-SIZE. LCFPUSH
\ rejects (named rc-70) at depth == CFSTK-DEPTH-MAX, BEFORE the write, so a push
\ can never overflow the region (the opposite-direction sibling of the LCFPOP
\ orphan-underflow guard). The cap is min(region capacity, a sane ceiling) and
\ always exceeds the checker's CFS cap 31 (CF-PUSH marks UNCK past 31), so no
\ checker-certified nesting is rejected.
DICT-SIZE CFSTK-OFF - 1 cells - CF-REC / constant CFSTK-REGION-CAP   \ 127 records fit
256 constant CFSTK-SANE-MAX                                          \ forward sanity ceiling
CFSTK-REGION-CAP CFSTK-SANE-MAX min constant CFSTK-DEPTH-MAX         \ 127 = min(region, sane)

\ Profiler counter band: one 64-bit sample counter per dictionary slot, reserved
\ at the very top of the DATA region [DATA-SIZE - PROF-CNT-BYTES, DATA-SIZE).
\ Sized from DICT-CAP so it always covers every slot BPROF-ON zeroes and EMIT-PROF
\ indexes (NDICT never exceeds DICT-CAP); src/habu/prof-abi.f PROF-BAND-AT
\ derives the band base offset as DATA-SIZE - PROF-CNT-BYTES. The band opens
\ with PROF-STATE-BYTES of the profiler's own cells (sample total and limit, the
\ two buckets, the dictionary base recorded by prof-on, the arena, the clock)
\ and the counters follow. Grows in step with DICT-CAP with no magic byte
\ count, so the band can never fall short of the slots it serves.
56 constant PROF-STATE-BYTES
DICT-CAP cells PROF-STATE-BYTES + constant PROF-CNT-BYTES

\ Cold prefix and argv or stdin input share the read allowance. Baked source
\ has separate space sized from SRCN by SOURCE-ARENA-LEN in habu2.f. Each argv
\ file is bounded by it too, and the file reader reserves an EOF probe; overflow
\ exits 74 with `hb: source prefix buffer full` (test/main-argv.f,
\ tools/build-fixpoint-test.f SOURCE-BOUNDARY). The prefix is read without its
\ comment and blank lines (habu2.f EMIT-SOURCE-READ-PREFIX); argv files and the
\ --build payload are read byte for byte. A seeded engine emits no cold prefix,
\ so its program input starts at the arena base.
$400000 constant SOURCE-ARENA-CAP
SOURCE-ARENA-CAP constant IBUFSZ
20 constant DATA

0 constant DP-CELL
8 constant HND-CELL
16 constant LOCN-CELL
24 constant LOCF-CELL
$3000 constant LOCNAMES
24 constant LOC-REC
\ A LOC-REC is an 8-byte bare-name length followed by the name bytes; a bare
\ name wider than the field is refused by C-LBRACE-STORE-ONE (habu2.f) before
\ it is stored. checker.f LOC-NAME-W and the LLOCWIDEMSG text state the same 16.
LOC-REC 8 - constant LOC-NAME-CAP
64 constant LOC-RECS          \ LOCNAMES holds this many LOC-RECs ($600 bytes); checker.f LOC-CAP states the same 64
\ Tier 1's arm64 body capture scopes the locals it records by block, as
\ src/habu/definers.f BODY-BLOCK-STEP does on x86-64 (habu2.f CAPTURE-BLOCK):
\ CAPDEPTH-CELL is the block depth, LOCDEPTHS the depth each LOCNAMES record
\ was declared at (one cell per LOC-REC), and TOKDEPTH-CELL the depth as the
\ current token began, restored with TOKLOCN-CELL's mark. A definition opener
\ zeroes the depth with LOCN-CELL. They sit in the $2800..$3000 free header
\ band past PROGRAM-FILE-CELL ($2DB0).
$2DB8 constant CAPDEPTH-CELL
$2DC0 constant TOKDEPTH-CELL
$2DC8 constant LOCDEPTHS        \ mirrored: src/habu/snap-lib.f
\ The friend arena: one write-protected band [FRIEND-ARENA,
\ FRIEND-ARENA+FRIEND-ARENA-LEN) holding the boot-seal latch and every
\ checker/wordlist crown-jewel cell (CUR/WIDN/HOOK/DEF-WL, the TRUSTED:
\ TSIG/TCSIG/CRSIG cells, the PKG-* cells, the DEFER-* cells). The latch is the
\ arena base: 0 while the engine loads its own source (guard inert),
\ FRIEND-ARENA-LEN once SEAL-FRIEND runs at the end of the cold prefix. Post-seal
\ any raw write into the band, the latch included, traps, so the seal is one-way.
\ The band sits below DATA-START, out of reach of the DP heap (DP-CHECK); only
\ sinks storing to a computed address (! c! +! atomic* patch32 snap-rebase,
\ syscall write buffers) carry the range check. PROT-GUARD also checks the
\ protected-WID registry below.
\ SEAL-NDICT-CELL ($A8) holds the seal-time ndict watermark. The latch seals
\ early (habu1.f BSEALFRIEND), so SEAL-CAPTURE tokens (habu1.f BSEALCAP) capture
\ the watermark later: a baseline at the end of xref.f and the cold-prefix
\ assembler's token at the engine-prefix end (after script-argv.f).
\ HIDE-DEFS-FROM/FORGET-DEFS-FROM (xref.f) refuse a post-seal FORGET below it;
\ it is in the sealed band so user source cannot lower it.
$20 constant FRIEND-ARENA               \ arena base offset within the DATA region (x20)
$90 constant FRIEND-ARENA-LEN           \ 18 cells: latch + 16 crown jewels + seal-ndict watermark
FRIEND-ARENA constant FRIEND-LATCH-CELL \ 0 = friend on/open, FRIEND-ARENA-LEN = sealed
$A8 constant SEAL-NDICT-CELL            \ seal-time ndict watermark; 0 until SEAL-CAPTURE

67 constant UNCAUGHT-RC                 \ deterministic exit status for an uncaught top-level throw (BTHROW
                                        \ THROW-NOREC): the kernel masks a raw code to 8 bits, so a multiple
                                        \ of 256 would exit 0.
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
\ DEFER-MAGIC: the first cell of the meta trailer `defer` writes just past a
\ deferred word's code, followed by the address of its dispatch cell. A reader
\ holding a record recognises a defer's trailer by this word, never by the
\ shape of what follows the code (an ordinary integer can hold any value). Its
\ readers are habu2.f C-DEFER-META-WRITE and C-DEFER-TARGET-META (build) and
\ src/compiler/native/dict.f (an `is` at run time); this is its one statement.
$4842444546455201 constant DEFER-MAGIC
\ CMFAM-CELL: resolved construct family id, live only between the family and
\ variant operand tokens of one `construct` form (CMM-CELL state 1 -> 2). Eager
\ resolution means no operand string is stashed across a REPL line refill.
\ $1A0 is the seal suite's deliberate poke hole.
$1B0 constant CMFAM-CELL
\ MATCH-lowering compile state, DATA-relative in the $B0..$1B0 band above the
\ friend arena; the fam stack tops out at $D0+CMFR-MAX*8 = $1A0. CMBK-CELL is a
\ 64-bit branch-kind bitstack (J-OF pushes 0, EM-ADT-MATCH-OF pushes 1, J-ENDOF
\ pops and checks), so ENDOF re-arms the match token machine (CMM=4) only for a
\ MATCH variant branch: the compiler analogue of the checker's
\ CF-ENDOF-DISPATCH. CMTAG/CMPADS hold the pending variant (tag, M-p) between a
\ variant token and its OF. CMFR is the fam stack indexed by CMFRD (match
\ depth); a level's fam feeds variant resolution and the ;MATCH bad-tag die.
\ CMFRD/CMBK clear at colon/TRUSTED: entry and in EM-RESET-COMPILE-STATE.
$B0 constant CMBK-CELL                  \ ENDOF branch-kind bitstack (0=case arm, 1=match branch)
$B8 constant CMTAG-CELL                 \ pending MATCH variant tag (VAR -> OF)
$C0 constant CMPADS-CELL                \ pending MATCH variant zero pads M-p (VAR -> OF)
$C8 constant CMFRD-CELL                 \ MATCH nesting depth (0 = not in a match)
$D0 constant CMFR-OFF                   \ MATCH fam stack base (one cell per open match)
26 constant CMFR-MAX                    \ levels: $D0..$1A0 = 26 cells (checker caps CF frames at 30)
$1B8 constant BODYLEN-CELL
$1C0 constant RBASE-CELL
$1C8 constant LOOPSP-CELL
STACK-ABI:BASE-CELL constant S0-CELL
$3640 constant REPLH-CELL
$3648 constant RSAVCP-CELL
$3650 constant RSAVND-CELL
$3658 constant RSAVDP-CELL
$3660 constant RSAVSP-CELL
$3668 constant RRECP-CELL
\ RSAVROWS-CELL: the checker's record end as the REPL line began, plus one,
\ read through the source owner (habu2.f EM-REPL-READ); 0 when no owner
\ records. A failed line cuts the records back to it as it rolls CP, NDICT and
\ DP back (EM-REPL-RECOVER), so no definition the line drops keeps a record or
\ a pending window. It sits in the $2800..$3000 free header band, below $7FF8
\ for the `DATA <off> LDR` form.
$2D38 constant RSAVROWS-CELL
$3670 constant ARGC-CELL
$3678 constant ARGV-CELL
$3680 constant ENVP-CELL
\ PROGRAM-FILE-CELL: argv[1]'s string once the launcher (habu2.f SRC-FPLAIN,
\ main.f RUN) runs it as the program file, else 0; src/core/include.f reads it.
\ It takes $2DB0, the first cell after TASK-CHAIN-CELL.
$2DB0 constant PROGRAM-FILE-CELL
$3688 constant PEND-CELL
\ A definition whose capture ran a parsing immediate has construction inputs
\ outside BODYBUF. The native cache reads this only through the running engine's
\ native-pending-opaque? primitive. The cell is transient and clears with
\ PEND-CELL on close or abandonment. $2D48 follows NATIVE-HOST-CELLS:DIVISION-PRIM.
$2D48 constant PEND-OPAQUE-CELL
$3690 constant TKA-CELL
$3698 constant TKL-CELL
$36A0 constant INP-CELL          \ mirrored: src/core/layout-buffer.f STGT-INP-CELL, src/core/checker.f CK-INP-OFF
$36A8 constant INE-CELL          \ mirrored: src/core/checker.f CK-INE-OFF
\ FWDTGT-CELL: where the last forward branch tier 0 patched lands, as an
\ offset from DBASE (habu2.f LPAT). jit.f LVFUSE refuses to rewind code a
\ branch lands in. Definition-scoped like JIT-SNAP:SP-CELL: the colon entries,
\ EM-P2-START and EM-ABORT-COMPILE-STATE zero it.
$36B8 constant FWDTGT-CELL
\ LIDX-CELL: 1 while the code tier 0 emits next may read the innermost open
\ loop's index from x29 (regalloc.f LIDX), 0 when the index must be reloaded
\ from its memory frame (habu2.f J-I). Definition-scoped like FWDTGT-CELL and
\ zeroed beside it; every call and every control-flow push and pop zeroes it
\ too. It sits at JIT-QUOT:END, the first cell after the quotation frames;
\ data-claims.f proves it disjoint.
$2C40 constant LIDX-CELL
\ BPTRAMP-CELL: the code address of the persistent-breakpoint trampolines,
\ MAXBP two-word slots debug.f reserves in the code band and declares with xt!.
$36C0 constant BPTRAMP-CELL
$36D0 constant BPTAB-OFF
$37E8 constant BPWBASE-CELL
$37F0 constant BPWN-CELL
\ EVAL-TOP-CELL: the innermost evaluate frame, zero at rest. habu1.f
\ EMIT-EVAL-BODY (`evaluate-closed`, and ARM64's `evaluate`) pushes its frame on
\ the native stack; x86-64's Habu `evaluate`
\ (src/habu/interpret.f OUTER:EVAL-FRAMED) writes the same fields into a
\ mapping of its own. EVAL-PREV links each to the frame it nests in.
$43C0 constant EVAL-TOP-CELL
STACK-ABI:EVAL-BYTES constant EVAL-FRAME-SIZE
$40 constant EVAL-PREV
$48 constant EVAL-PKG
\ EVAL-INB: the outer evaluate's input-buffer START, saved beside INP ([frame+0])
\ and INE ([frame+8]) so a nested evaluate restores it. PKGSNAP fills
\ EVAL-PKG..$80 (its last cell, PKGSNAP:FLOOR, is $78) and STACK-ABI:EVAL-BASE
\ and EVAL-CAP hold $80..$90, so EVAL-INB takes $90, EVAL-FRAME:USE-FLOOR $98,
\ EVAL-FRAME:USE-WIDS $A0..$120 and EVAL-FRAME:PEND $120; $128 is held for the
\ frame's saved next wordlist id (WIDN), STACK-ABI:EVAL-SEG takes $130,
\ EVAL-FRAME:ROWS $138 and EVAL-FRAME:SX-CTX..SX-SRC $140..$158; $158 pads the
\ frame to STACK-ABI:EVAL-BYTES ($160), which keeps the native stack 16-byte
\ aligned.
$90 constant EVAL-INB
package EVAL-FRAME
public
\ USE-FLOOR: the using depth the buffer may close down to. B-EVAL starts it at
\ the depth the buffer enters at, so a `;using` at or below it, which would
\ close a using the includer opened, is refused (habu2.f C-END-USING). A
\ `;package` that closes a package opened before the buffer restores a lower
\ depth; the buffer's next `using` lowers the floor to meet it (C-USING-PUSH),
\ and the clean exit restores the depth to the lower of the two
\ (EM-EVAL-CLEAN-EXIT), so no using the buffer opened stays open.
$98 constant USE-FLOOR
\ USE-WIDS: the includer's USE-MAX used-public wids (USE-WIDS-OFF), which a
\ throw puts back with the depth and the floor (habu2.f LEVALREC). A buffer
\ that closes its includer's package and then opens a using writes that using
\ into a slot the includer keeps; the checker reads each slot's name back from
\ the restored wid (checker.f CHECKER-RESYNC).
$A0 constant USE-WIDS
\ PEND: PEND-CELL as the buffer began. A definition the buffer opened must
\ close in it, so its end refuses one still open (habu2.f C-DEF-SOURCE-END); one
\ already open here belongs to an outer buffer, whose immediate word's evaluate
\ compiles into it, and a throw that unwinds this buffer leaves it compiling
\ (habu2.f EM-EVAL-THROW-RECOVER).
$120 constant PEND
\ ROWS: the checker's record end as the buffer began, plus one, read through
\ the source owner (habu1.f EMIT-EVAL-BODY); 0 when no owner records. A throw that
\ unwinds a buffer owning the compile cuts the records back to it as it rolls
\ the dictionary back (habu2.f EM-EVAL-THROW-RECOVER), so no definition the
\ rollback drops keeps a record or a pending window.
$138 constant ROWS
\ SX-CTX: the source observer this buffer adopted, 0 when it adopted none. An
\ observed word that evaluates a buffer hands the observer to that buffer
\ (habu1.f EMIT-EVAL-BODY, outer.f OBSERVE-ARM): SX-TICKET and SX-SRC keep the
\ word's ticket and its source start, which the clean exit and the throw
\ recovery put back with the word's run pending again (habu1.f
\ ENGINE-EMIT:SX-RESTORE,, outer.f OBSERVE-RESTORE). The observer's frame needs
\ no cell: adoption requires it to be the frame this buffer nests in, EVAL-PREV.
$140 constant SX-CTX
$148 constant SX-TICKET
$150 constant SX-SRC
;package

\ Every engine load refusal reaching the LCOMPILEDIE tail names its source as
\ `hb: <message> at <path>:<line>`; emitted code reads these cells as it reads
\ INCLUDE-EVALERR-CELL.
\ PATH-CELL / PATHLEN-CELL: the innermost open include frame's path, 0/0 when
\ no file is open (tty REPL, `-e`, the boot prefix). src/core/include.f
\ SOURCE-ROOT:PUSH copies the resolved path into its frame and publishes it; POP
\ republishes the parent's. The address is the frame's, not INCLUDE-PATH's,
\ because a nested include overwrites that buffer.
\ INB-CELL: the start of the buffer INP walks, so the tail counts newlines in
\ [INB, INP). habu1.f B-EVAL sets it and saves the outer value in EVAL-INB. It
\ is not beside INP/INE: $36B0 is regalloc.f FRFREE-CELL and $36C8 is
\ address-cells.f INDEX-CELL.
\ All three sit in the $2800..$3000 free header band, below $7FF8 for a
\ `DATA <off> LDR` and below DATA-START.
package SRCLOC
public
$2800 constant PATH-CELL
$2808 constant PATHLEN-CELL
$2810 constant INB-CELL
\ The loader alone publishes the execution bytes owned by the active file.
\ An inner evaluate changes INB, but never changes this file span.
$2D68 constant OWNED-CELL
$2D70 constant OWNEDLEN-CELL
\ CURSOR: INP-CELL as a declared cell, the byte address the scanner reads next.
\ The native FILE cache reads it at a loader cut.
INP-CELL RESERVED-PTR-U8-CELL CURSOR
;package

\ RPKG-* / PKGRESYNC-CELL: the REPL line's package snapshot. EM-REPL-READ
\ (LREAD) saves the five live package-scope cells at line start;
\ EM-REPL-RECOVER (LRREC) restores them on a compile error, beside the
\ RSAVCP/RSAVND/RSAVDP/RSAVSP rollback. Both recovery legs arm PKGRESYNC-CELL
\ and EM-PKG-RESYNC drains it, making the checker re-read the restored scope:
\ LEVALREC drains before it delivers the throw, the LMAIN top drains the REPL
\ leg's (habu2.f). The six cells take $2780..$27C0.
$2780 constant RPKG-CUR
$2788 constant RPKG-PUB
$2790 constant RPKG-PRI
$2798 constant RPKG-PARENT
$27A0 constant RPKG-REC
$27C0 constant PKGRESYNC-CELL
package RPKG
public
\ FLOOR: the line-start using floor (USE-PKG-SAVE-CELL), restored with the five
\ RPKG-* cells above; PKGSNAP:FLOOR below says why. It takes $4810, the first
\ cell above STACK-ABI:LOOP-BASE-CELL below TXN-STATE-OFF ($5000), claimed in
\ src/habu/data-claims.f.
$4810 constant FLOOR
\ WIDS: the line-start used-public wids, USE-MAX cells, restored with the depth
\ and the floor; EVAL-FRAME:USE-WIDS above says why. It takes the run after
\ FLOOR, claimed in data-claims.f.
$4818 constant WIDS
;package
$27B0 constant DOESB-CELL
$27B8 constant TRUSTED-CELL
$37D0 constant EVALD-CELL        \ mirrored: src/core/checker.f CK-EVALD-OFF
$37D8 constant EVALERR-CELL
$37E0 constant LMAINP-CELL
$3C88 constant TASK-TCB-CELL
\ HALT-UNWIND-CELL: nonzero while a TASK:HALT unwinds the task that observed it,
\ else 0, which a fresh task region and every non-task region hold. Only that
\ task's own thread writes it: src/lib/task.f HALT-UNWIND, which PAUSE calls,
\ sets it before it throws, and the catch family reads it on its throw-resume
\ path (habu1.f BCATCH-FORWARD, BFINALLY, BC2-INVOKE; their kernel-x64.f
\ twins), and the `throw` primitive keeps a halt's 0 instead of refusing it
\ (habu1.f BTHROWPRIM, kernel-x64.f THROW-CODE,). An inner
\ catch rethrows at once and never pushes; the task's root catch, whose frame's
\ prev HND is 0, clears it and delivers the code; finally and c2-invoke clear it
\ while their cleanup runs and set it again to continue the unwind. $3870
\ follows ENGINE-MAIN:ARENA-CELL ($3868), below $7FF8 for a `DATA <off> LDR`.
$3870 constant HALT-UNWIND-CELL
$3C90 constant TASKS-LIVE-CELL
$3C98 constant HIDXP-CELL
\ HIDX:CLAIMS: hash-index slots ever claimed, live rows plus the stale slots
\ rollback leaves. C-HIDX-INS raises it only when it claims an EMPTY slot, only
\ the rebuild (from live [0,NDICT)) lowers it, and LHIDXADD compacts the table
\ when it crosses HIDX:LOAD-MAX. Engine-emitted code is its only writer.
package HIDX
public
$27C8 constant CLAIMS
;package
\ PROT: the x86-64 kernel's write-window cells (src/habu/kernel-x64.f).
package PROT
public
$27D0 constant WINDOW
$27D8 constant WLO
$27E0 constant RLO
$47C0 constant RHI
$47C8 constant CF
;package

\ REGION-ALIAS:DELTA-CELL: the region's RW alias minus its RX base (DBASE). The
\ region is mapped twice (habu2.f EM-MMAP-CODE-REGION): code runs from DBASE and
\ every store into the region goes to its address plus this delta. Each run maps
\ its own alias, so the cell is live state: EM-DATA-INIT publishes it, the boot
\ republishes it after a snapshot restore and every task region copies it. A
\ forked child maps its own alias at the parent's alias address, so the delta it
\ inherits stays true (habu1.f LIBC-OS:FORK). Below $7FF8 so `DATA <off> LDR`
\ names it directly.
\ Neither view changes protection after boot: compiling never calls mprotect.
\ Measured on macos-arm64 with `/usr/bin/time -l`: a source-prefix boot (the
\ hb-stdin engine `install` promotes) takes 2101 page reclaims and 0.01 s system
\ time, and `install --force` 40120 page reclaims and 0.5 s system time.
package REGION-ALIAS
public
$43B0 constant DELTA-CELL
;package

\ AOT-CELLS:SIG-POOL-CELL / SIG-LEN-CELL: the baked signature pool's address and
\ length, published by the seed (habu2.f EM-SEED-AOT) as it registers the
\ captured records and read by the checker's lazy intake for a seeded word it
\ has no effect for. Zero until a seed runs: no pool, and the intake is a no-op.
\ The pool is __text, the same at every boot, so it is never copied. They take
\ $47D0/$47D8; src/core/checker.f mirrors the two offsets (CK-AOT-SIG-*-OFF)
\ because it loads before this file, as with CK-PKG-REC-OFF.
\ AOT-CELLS holds every AOT cell this file declares: the engine bakes this file,
\ so a cell declared in AOT-SIG, AOT-SPAN or AOT-WINDOW would make aot-decl.f,
\ habu1.f and habu2.f reopen a baked package in a native build
\ (docs/forth-card.md § 6).
package AOT-CELLS
public
$47D0 constant SIG-POOL-CELL
$47D8 constant SIG-LEN-CELL
;package

\ AOT-CELLS:SPAN-TABLE-CELL / SPAN-N-CELL / SPAN-BASE-CELL: the baked CODE-SPAN
\ table, its row count and the address the blob was copied to, published by the
\ seed (habu2.f EM-SEED-AOT) when the blob lands. A word with no dictionary
\ record is accounted for only by its row, which src/habu/aot-closure.f reads to
\ retarget PC-relative branches. A row is a blob offset; the base makes it an
\ address. Zero in all three means no stripped spans (nothing captured, or a
\ whitebox image that kept every name).
\ They sit in the $600..$800 header hole BODYBUF-OFF names free, above
\ GENIO-ABI's band. The run above BOOT-LAYOUT:HEAP-START-CELL is not free:
\ stack-abi.f holds $47E8..$4810, then the transaction and USER-BAND.
package AOT-CELLS
public
$660 constant SPAN-TABLE-CELL
$668 constant SPAN-N-CELL
$670 constant SPAN-BASE-CELL
;package

\ SIGNAL-ABI: the baked async-signal-safe handler stub (src/habu/crash.f
\ EMIT-SIGNAL-HANDLER, label LSIGH) and the word it reads. STUB-CELL is the
\ stub's address, a program's sa_handler; FD-PTR-CELL is the address of the fd
\ word, through which a program stores its self-pipe's write end; FD-CELL is the
\ word's offset, claimed so nothing else takes it. Every boot writes the two
\ published cells (habu2.f EM-STARTUP-RUNTIME-STATE, aot-link-arm.f,
\ boot-x64.f), and the engine's boot clears the fd word because snap-lib.f
\ SND-COPY carries DATA from offset zero; a stripped image's DATA is fresh.
\ The stub reads the fd word at the absolute address DATA-VA + FD-CELL, never
\ through x20: a handler runs on whichever thread receives the signal, and that
\ thread's x20 is its own task region (src/lib/task.f PREPARE maps it zeroed).
\ DATA-VA is fixed (src/os/<target>/layout.f; EM-MMAP-DATA-REGION refuses another
\ address): the LOCK-CELL class of process-wide cell. A spawned task's region
\ carries neither published cell, so a library reads them once from the main
\ task and keeps them.
\ The cells follow AOT-SPAN's in the $600..$800 hole, below $7FF8 (the boot
\ stores them as `DATA <off> STR`) and below DATA-START.
package SIGNAL-ABI
public
$678 constant STUB-CELL
$680 constant FD-PTR-CELL
$688 constant FD-CELL
;package

\ BOOT-LAYOUT:HEAP-START-CELL: the running engine's DP-heap floor as a DATA
\ offset, stored by habu2.f EM-DATA-INIT from the DATA-START it hands DP.
\ tools/native-build.f splits the host's declared address rows at the host's
\ floor, which is not the loaded tree's: growing a reserved band (XTCELL-CAP)
\ moves the tree's DATA-START but not the host's, and classifying by the source
\ constant makes the capture refuse the host's own heap rows (aot-capture.f
\ ACAP-TARGET-REFUSE, exit 74; measured: seed floor 958280, grown tree 1220424,
\ heap row at DATA+1140613). An offset needs no relocation, so it is not a
\ declared address cell. Zero means an engine without the cell (no heap starts
\ at 0); tools/native-build.f names its fallback. It sits in the fixed header so
\ no band growth moves it.
package BOOT-LAYOUT
public
$47E0 constant HEAP-START-CELL
;package


\ GENIO-ABI: the routing cells of the generic I/O layer (src/lib/genio-core.f)
\ that engine code reads; everything else about a device is library storage.
\ OUT-CELL and IN-CELL hold a device index, never an address: 0 is the built-in
\ terminal, 1..DEVICES a row of the write table. They are per task (task.f
\ TASK-REGION-INIT copies them), and nothing canonicalises a code address in a
\ task region, so an execution token there would survive a snapshot or capture
\ pointing into the builder's region.
\ The write table holds execution tokens in the engine header, where SNAP-RELOC
\ canonicalises declared cells: genio-core.f publishes a device's write through
\ QUOTATION-STORAGE:STORE and clears every row through IMAGE-LIFECYCLE before a
\ capture. TASK-REGION-INIT copies the rows too, sound because a task region is
\ never snapshotted or captured. DEVICES is small so the table costs no
\ allocation on a microcontroller.
\ BUSY-CELL stops re-entry: a device write may reach `type`, and while one runs
\ the funnel takes the terminal path.
\ ACTIVE-CELL names the device an operation runs for: devices share their eight
\ quotations and a quotation cannot capture a local (docs/forth.md). The funnel
\ publishes it around the write, genio-core.f around every other operation.
\ The band is $600..$660 of the header hole BODYBUF-OFF names free, below $7FF8
\ (the funnel's first instruction is `DATA <off> LDR`) and below DATA-START; the
\ word-frame lane's $7D0/$7D8 are untouched.
package GENIO-ABI
public
8 constant DEVICES                 \ device rows; a device index is 1..DEVICES
$600 constant OUT-CELL             \ per-task current output device index (0 = terminal)
$608 constant IN-CELL              \ per-task current input device index (0 = terminal)
$610 constant BUSY-CELL            \ output-funnel re-entrancy guard (0 = idle)
$618 constant ACTIVE-CELL          \ device index an operation is running for (0 = none)
$620 constant WRITE-OFF            \ DEVICES cells: row i-1 = device i's write operation
WRITE-OFF DEVICES 8 * + constant END
;package


\ EVALREC-CELL: runtime address of the eval-frame throw-unwind entry (habu2.f
\ LEVALREC), set at startup like LMAINP-CELL so the leaf throw primitive can
\ branch to it. $3CA0..$3CB8 are engine cells no compiled source writes, between
\ the FFI buffer block ($3A00..$3C88, src/lib/ffi-abi.f), the task TCB cells
\ ($3C88..$3CA0) and the protected-WID registry ($3CB8..$40C0).
$3CA0 constant EVALREC-CELL
\ AOT-SEED-DONE-CELL: one-shot flag set the first time the post-cold-prefix AOT
\ seed runs (EM-COMPILE-EXIT, at the end of the engine prefix stream), so neither
\ the user stream's own exhaustion nor REPL re-entry can seed a second time. It
\ is the seed's only guard; the seed runs on every boot.
$3CA8 constant AOT-SEED-DONE-CELL
\ BOOT-SRC:USER-END: the end of the user source stream while the engine-prefix
\ stream runs, 0 once it is installed (one-shot). habu2.f's source setup builds
\ the prefix and the user program (piped bytes, `s" f.f" required` rows, or the
\ baked LSRC) into one mapping; the cold-prefix routine publishes the prefix end
\ as INE and the mode publishes the buffer end here. "Source exhausted"
\ (EM-COMPILE-EXIT, LEX0) thus comes at the end of the engine prefix on every
\ boot, where the engine is complete and no user token has run: the AOT seed
\ runs there, then LEX0 installs [INE, USER-END) and re-enters the interpreter,
\ as EM-REPL-READ does for a typed line. Arming the seed without splitting the
\ streams fired it after the batch program it serves. DATA-INIT zeroes it.
package BOOT-SRC
public
$3CB0 constant USER-END
;package
\ The protected-WID registry records the WIDs of sealed system / generated
\ constructor packages created in the friend window; habu1.f LPROTWIDQ
\ membership (EMIT-PROTWID) gates the sealed-WID guards.
\ Bit w of the $400-byte band at PROT-BITS-OFF is set exactly when wordlist w is
\ protected, so the set holds any subset of the WIDs the engine indexes.
\ PROT-WID-MAX is a WID bound: $400 bytes index 8192 WIDs, and prot-wid-add
\ names the bound when handed a WID at or above it. A wider bitmap must first
\ move the FFI length buffers: $40C8..$41C8 is FFI:FFI-REG-LEN-BUF-OFF
\ (src/lib/ffi-abi.f), and APP-ENTRY:XT-CELL, the AOT capture window and the
\ evaluator-pointer band follow from $43A0; growing it also moves UNCGH-CELL.
\ No compiled source writes the band, the DP heap is bounded >= DATA-START, and
\ snapshots save it. [PROT-REG-OFF, +PROT-REG-LEN) is a second PROT-GUARD range,
\ refusing user stores into the tag cell, the bitmap or the uncaught-throw hook;
\ cp!/ndict! (habu1.f BCPSET/BNDSET) PROT-GUARD the address they redirect writes
\ to, so a post-seal cp!/ndict! into either band fails closed.
$3CB8 constant PROT-REG-TAG-CELL        \ bitmap-shape tag; capture requires this exact runtime shape
$50574249544D4150 constant PROT-REG-TAG \ "PWBITMAP": written by every path that publishes the band
$3CC0 constant PROT-BITS-OFF            \ protected-WID bitmap base
8192 constant PROT-WID-MAX              \ WID bound: bits 0..8191 span $3CC0..$40C0, the exact band the 256-slot table held
PROT-WID-MAX 8 / constant PROT-BITS-BYTES
PROT-BITS-OFF PROT-BITS-BYTES + constant PROT-BITS-END
PROT-REG-TAG-CELL constant PROT-REG-OFF \ second PROT-GUARD band base (= tag cell)
PROT-BITS-END 1 cells + PROT-REG-OFF - constant PROT-REG-LEN  \ $410: tag + bitmap + UNCGH-CELL = $3CB8..$40C8
\ UNCGH-CELL: runtime address of the uncaught-top-level-throw reporter (habu2.f
\ LUNCAUGHT), stored at boot (EM-STARTUP-RUNTIME-STATE) so the leaf BTHROW
\ primitive can branch to it when a throw reaches THROW-NOREC with no handler
\ and no REPL. A fixed engine cell no compiled source writes; DATA is zero until
\ boot.
$40C0 constant UNCGH-CELL
\ The callback band: C calling back into checked code (src/lib/ffi-callback.f).
\ Eight cells every region carries at the same offset, in the run between the
\ FFI length buffers ($40C8..$41C8) and APP-ENTRY:XT-CELL ($43A0). Every cell is
\ correct at zero, which is what a fresh task region, a fresh DATA mapping and a
\ stripped image's anonymous DATA all hold.
\   CB-XDS..CB-CP  the VM frame an outbound FFI core publishes after its pops and
\                  restores after its BLR (habu1.f BCB-ENTER / BCB-LEAVE), LIFO
\                  through the core's own $30 stack frame. CB-XDS 0 = no frame.
\   CB-OWNER       the thread pointer (TPIDR_EL0 on Linux, TPIDRRO_EL0 on macOS)
\                  of the one thread allowed on this region; 0 = free.
\   CB-FRAME       the live marshal frame of the innermost callback.
\   CB-XTS/ROWS    growable descriptor and binding tables, read through the
\                  main region at absolute address DATA-VA.
\ A binding row holds region, owner, move count and claim region. The owner is
\ 0 idle, a thread pointer inside the slot, or CB-ROW-BUSY while a mover holds
\ it. A call that meets BUSY waits for the hand-back (habu1.f BCALLBACK-THUNK).
$41C8 constant CB-XDS
$41D0 constant CB-DBASE
$41D8 constant CB-NDICT
$41E0 constant CB-CP
$41E8 constant CB-OWNER
$41F0 constant CB-FRAME
$41F8 constant CB-XTS
$4200 constant CB-ROWS
$4208 constant CB-END
CB-END CB-XDS - constant CB-BAND-BYTES
16 constant CB-POOL                \ boot stubs retained for seed compatibility
32 constant CB-ROW-BYTES
0 constant CB-ROW-REGION
8 constant CB-ROW-OWNER
1 constant CB-ROW-BUSY
\ The marshal frame CB-FRAME names while a dispatch runs: x0..x7 as C passed
\ them, then d0..d7 from CB-FRAME-FLOATS. The dispatch leaves its result in the
\ x0 or the d0 slot, and the thunk returns both.
$40 constant CB-FRAME-FLOATS
\ Compact AOT dict record: five u32 words (blob-off/public-WID, code-len/private-WID,
\ name-off, flags|min-in, wid). The name-off word is a full u32 so the deduped name
\ pool can outgrow 64 KiB with the captured window; the compiler chain needs ~51 KiB
\ of names where the metabuild REPL window needs 953 bytes.
20 constant AOT-CREC-ROW
\ The deduplicated pool admits at most DICT-CAP entries, each containing a
\ one-byte length and at most 255 name bytes. Storage grows to the used size.
DICT-CAP 256 * constant AOT-NAMES-CAP
\ Dict-name hash index: slots are a power of 2 (LFIND probes with the
\ HIDX-SLOTS 1 - mask) and 2x DICT-CAP, so the load factor stays <= 50% and
\ DICT-CAP stays a power of two; bytes = slots * 4 (u32 entries). HIDX-SLOTS
\ exceeds MOVZ imm16, so the habu1.f probe sites load it with LIT64.
DICT-CAP 2 * constant HIDX-SLOTS
HIDX-SLOTS 4 * constant HIDX-BYTES
\ HIDX:LOAD-MAX: the claimed-slot count at which LHIDXADD compacts the table
\ in place instead of letting chains grow toward a full wrap. Three quarters of
\ the file: strictly above DICT-CAP, so a compaction always lands the
\ count back at NDICT with room to spare and can never itself be at the bound,
\ and strictly below HIDX-SLOTS, so an insert always meets an empty slot within
\ its chain and the full-wrap path is unreachable (HIDX-EMIT:LFULL dies loudly
\ if it is reached).
package HIDX
public
HIDX-SLOTS 3 * 4 / constant LOAD-MAX
;package
$37F8 constant SNAP-CELL
\ ENGINE-MAIN:XT-CELL is the DATA cell a seeded x86-64 boot calls the engine's
\ MAIN through (docs/xcomp.md "Build and bootstrap"). It is a separate cell
\ from APP-ENTRY:XT-CELL because that one also switches the argv and stdin
\ conventions to a saved application's. CHECKER-REG:SEAL installs the captured
\ MAIN there; only the x86-64 boot reads it, and an ARM64 boot, which reaches
\ the kernel's own source and REPL routes, never does. It sits in the
\ single-cell run above SNAP-CELL ($37F8); a fresh DATA mapping reads it zero
\ until set.
package ENGINE-MAIN
public
$3808 constant XT-CELL
\ The captured x86 reporter is written here while the ARM build host is
\ sealed. The linker publishes its translated XT in protected UNCGH-CELL.
\ $3858 follows POLICY-ABI:KEYWORD-CELL and is outside PROT-REG.
$3858 constant REPORT-CELL
\ The x86-64 Habu loop's source arena (src/habu/main.f SOURCE-ARENA): a mapping this
\ process made, so the snapshot writer zeroes it in copied DATA (snap-lib.f
\ SND-ZERO-LIVE) and every boot reads it zero until MAIN maps its stream.
\ $3868 follows PROVIDED-XT:EVALUATE-RANGE-CELL ($3860).
$3868 constant ARENA-CELL
;package

\ PROVIDED-XT names the dispatch cell of each src/habu/prims.f row an x86-64
\ captured runtime provides (EPREFIX-PROVIDED!). In a seeded x86-64 build the
\ row keeps its record and name, and its body is a stub that jumps through the
\ cell (src/habu/kernel-x64.f X64KERNEL:PROVIDED-ROW); the x86-64
\ `evaluate-closed` jumps through EVALUATE-CELL as well. The captured runtime
\ stores its word there with `xt!` (src/habu/interpret.f
\ OUTER:INSTALL-EVALUATE, which native-runtime.f CHECKER-REG:SEAL runs), so the
\ capture carries the cell as a fixed address row (tools/native-layout.f) and
\ the x86-64 link restores it on every boot (src/habu/link-x64.f FIXED-CELLS).
\ An ARM64 record, and an x86-64 build with no captured runtime,
\ keep the assembly body and never read the cell. EVALUATE-CELL follows the
\ three-cell origin record ($3820..$3837), NCOMP-DISPATCH:COMP-CELL ($3838)
\ and the free cells $3840..$3847.
package PROVIDED-XT
public
$3848 constant EVALUATE-CELL
\ A seeded x86-64 `evaluate-range` jumps through its own cell the same way
\ (src/habu/interpret.f OUTER:EVAL-FRAMED-RANGE). It follows
\ ENGINE-MAIN:REPORT-CELL ($3858) below $3A00.
$3860 constant EVALUATE-RANGE-CELL
;package

\ The x86-64 policy writer calls the checked interpreter's dispatch-keyword
\ predicate before admitting a package. CHECKER-REG:SEAL installs that callback;
\ its fixed code cell is captured and restored with the other engine callbacks.
\ No ARM64 code reads the cell.
package POLICY-ABI
public
$3850 constant KEYWORD-CELL
;package

$1D8 constant SSCR-CELL
$1E0 constant GTOD-SCRATCH
$200 constant VSP-CELL
$210 constant VTAG-OFF
$250 constant VVAL-OFF
32 constant VSMAX


\ TIER-CELL selects tier 1. It sits at $358, so JIT-SNAP's depth cell must not:
\ the first `BEGIN` in a tier-0 definition would write this cell and the two
\ declaration-owner cells above it.
\ the two declaration-owner cells above it.
\
\ Tier 1 still has no fallback: an unset XT-CELL dies at NCOMP-EMIT:LOAD with the
\ seed-integrity code. TIER-CELL selects which compiler runs; it never rescues a
\ broken one.
package NCOMP-DISPATCH
public
$358 constant XT-CELL

\ The callback operations belong to the compiler selected by XT-CELL. A fresh
\ source compiler may coexist with a retained active compiler in a build window;
\ both operations must follow the active compiler rather than dictionary names.
\ These two free header cells sit after ENGINE-MAIN:XT-CELL and before FFI.
$3810 constant CALLBACK-STUB-CELL
$3818 constant CALLBACK-ENTRY-CELL
\ The optional three-cell origin record follows at $3820..$3837, read through
\ the three-cell native-origin-cell getter.
\ Tier 0's compiler entry: T0:COMPILE on an arm64 engine, installed by
\ CHECKER-REG:SEAL through src/habu/runtime-arm64.f. Zero on an engine with no
\ compiler, where tier 0 is the JIT. $3840..$3847 is free.
$3838 constant COMP-CELL

\ TIER-CELL ( 0 | 1 ) -- which compiler the `:` handlers dispatch to.
\
\   0 = tier 0: the compiler in COMP-CELL, or the legacy JIT (LCOMPILE and its
\       closure) when COMP-CELL is zero. Every `--load` and the REPL; a fresh
\       DATA page starts at 0.
\   1 = tier 1, the IR pipeline reached through XT-CELL (NCOMP:COMPILE).
\       Selected deliberately with `1 set-tier`. An executable build compiles
\       at its caller's tier; the engine build selects tier 1 for its target,
\       whose capture requires native code.
\
\ On an engine whose tier 0 is the JIT, tier 1 must be selected BEFORE the
\ first definition on a tier-1 path: NCOMP's READ-PRIOR exits on GLUE-UNKNOWN
\ when it meets a body the JIT compiled.
\
\ Read by emitted code as `DATA TIER-CELL LDR` (12-bit scaled immediate), so it
\ stays 8-aligned and below $7FF8 -- see the measurement recorded at the AOT
\ window cells further down this file.
$370 constant TIER-CELL
\ Latched at the colon (0 JIT, 1 XT-CELL, 2 COMP-CELL); an immediate may only
\ select the NEXT definition.
$388 constant DEF-TIER-CELL
\ The outer executable-build scope saves the caller's selection once and
\ restores it when it closes; nested scopes only count depth.
$390 constant BUILD-DEPTH-CELL
$398 constant BUILD-TIER-CELL
\ RECOVER-CELL ( 0 | -1 ) -- whether the reader whose `;` called the compiler
\ recovers from a refused definition: src/habu/definers.f DEF-SEMI? sets it
\ around its compile, catches E-NCOMP-RECOVERY and reads on. In a multi-error
\ run the compiler then abandons a refused definition unpublished without a
\ second report; under any other reader it refuses with E-NCOMP-VERDICT
\ (compiler.f RECORD). Zero outside that compile, so no image carries it set.
\ It takes the first cell of the free $690..$6D0 hole below LVF-OFF.
$690 constant RECOVER-CELL
\ One owner record keeps every engine declaration registrar paired with the
\ active compiler, including while the source dictionary/checker is replaced.
\ The record is DATA; its fields are execution tokens of existing private
\ checker operations, with no new checked-callable trust alias.
$360 constant DECL-CELL
$368 constant TARGET-DECL-CELL
\ The two cells as declared names: each holds an owner record pointer.
DECL-CELL RESERVED-PTR-U8-CELL DECL-A
TARGET-DECL-CELL RESERVED-PTR-U8-CELL TARGET-DECL-A
CHECKER-OWNER-ABI:RAW-OFF constant DECL-RAW-OFF
CHECKER-OWNER-ABI:EFFECT-OFF constant DECL-EFFECT-OFF
CHECKER-OWNER-ABI:DEFER-OFF constant DECL-DEFER-OFF
CHECKER-OWNER-ABI:CAST-OFF constant DECL-CAST-OFF
CHECKER-OWNER-ABI:LINEAR-OFF constant DECL-LINEAR-OFF
CHECKER-OWNER-ABI:USING-OFF constant DECL-USING-OFF
CHECKER-OWNER-ABI:PACKAGE-OFF constant DECL-PACKAGE-OFF
CHECKER-OWNER-ABI:PUBLIC-OFF constant DECL-PUBLIC-OFF
CHECKER-OWNER-ABI:PRIVATE-OFF constant DECL-PRIVATE-OFF
CHECKER-OWNER-ABI:END-PACKAGE-OFF constant DECL-END-PACKAGE-OFF
CHECKER-OWNER-ABI:SOURCE-ROW-OFF constant DECL-SOURCE-ROW-OFF
CHECKER-OWNER-ABI:SOURCE-CON-OFF constant DECL-SOURCE-CON-OFF
CHECKER-OWNER-ABI:EXPORT-OFF constant DECL-EXPORT-OFF
CHECKER-OWNER-ABI:WIDE-OFF constant DECL-WIDE-OFF
CHECKER-OWNER-ABI:RESET-OFF constant DECL-RESET-OFF
CHECKER-OWNER-ABI:CAPTURE-OFF constant DECL-CAPTURE-OFF
CHECKER-OWNER-ABI:TRUSTED-TICK-OFF constant DECL-TRUSTED-TICK-OFF
CHECKER-OWNER-ABI:PKG-RESYNC-OFF constant DECL-PKG-RESYNC-OFF
\ The rest is the optimizing front end's half of the record. That front end is
\ the checker's scan (the source tape, the does> split, per-call-site facts, the
\ effect store), reached through the record rather than by name so that a run
\ which replaces the source dictionary scans its files with its own checker
\ instance, not the one the engine was built against.
\ Fields are appended, so no offset moves; checker-owner-abi.f owns the offsets
\ and the byte count, and checker.f checks the count against the cells it
\ commits.
CHECKER-OWNER-ABI:CHECK-OFF constant DECL-CHECK-OFF
CHECKER-OWNER-ABI:TAPE-INSTALL-OFF constant DECL-TAPE-INSTALL-OFF
CHECKER-OWNER-ABI:TAPE-ARM-OFF constant DECL-TAPE-ARM-OFF
CHECKER-OWNER-ABI:TAPE-DISARM-OFF constant DECL-TAPE-DISARM-OFF
CHECKER-OWNER-ABI:TAPE-ADVANCE-OFF constant DECL-TAPE-ADVANCE-OFF
CHECKER-OWNER-ABI:DOES-CHECK-OFF constant DECL-DOES-CHECK-OFF
CHECKER-OWNER-ABI:DOES-IN-OFF constant DECL-DOES-IN-OFF
CHECKER-OWNER-ABI:DOES-OUT-OFF constant DECL-DOES-OUT-OFF
CHECKER-OWNER-ABI:DOES-WIDE-OFF constant DECL-DOES-WIDE-OFF
CHECKER-OWNER-ABI:DOES-IN-N-OFF constant DECL-DOES-IN-N-OFF
CHECKER-OWNER-ABI:DOES-OUT-N-OFF constant DECL-DOES-OUT-N-OFF
CHECKER-OWNER-ABI:DOES-IN-SLOT-OFF constant DECL-DOES-IN-SLOT-OFF
CHECKER-OWNER-ABI:DOES-OUT-SLOT-OFF constant DECL-DOES-OUT-SLOT-OFF
CHECKER-OWNER-ABI:CALL-CELLS-OFF constant DECL-CALL-CELLS-OFF
CHECKER-OWNER-ABI:CALL-BINDING-OFF constant DECL-CALL-BINDING-OFF
CHECKER-OWNER-ABI:UNJUDGED-BINDING-OFF constant DECL-UNJUDGED-BINDING-OFF
CHECKER-OWNER-ABI:CALL-GLUE-OFF constant DECL-CALL-GLUE-OFF
CHECKER-OWNER-ABI:CALL-MATCH-OFF constant DECL-CALL-MATCH-OFF
CHECKER-OWNER-ABI:CALL-QUOT-IN-OFF constant DECL-CALL-QUOT-IN-OFF
CHECKER-OWNER-ABI:CALL-QUOT-OUT-OFF constant DECL-CALL-QUOT-OUT-OFF
CHECKER-OWNER-ABI:INIT-LAYOUT-OFF constant DECL-INIT-LAYOUT-OFF
CHECKER-OWNER-ABI:FIELD-SPAN-OFF constant DECL-FIELD-SPAN-OFF
CHECKER-OWNER-ABI:C2-STOW-OFF constant DECL-C2-STOW-OFF
CHECKER-OWNER-ABI:TRUST-DECL-OFF constant DECL-TRUST-DECL-OFF
CHECKER-OWNER-ABI:PARSE-IMM-OFF constant DECL-PARSE-IMM-OFF
\ EFFECT-QUERY resolves a name into the
\ instance's query state and every reader below reads THAT state, so all of them
\ have to reach the same owner or a reader answers about another instance's query.
CHECKER-OWNER-ABI:EFFECT-QUERY-OFF constant DECL-EFFECT-QUERY-OFF
CHECKER-OWNER-ABI:EFFECT-DIN-N-OFF constant DECL-EFFECT-DIN-N-OFF
CHECKER-OWNER-ABI:EFFECT-DOUT-N-OFF constant DECL-EFFECT-DOUT-N-OFF
CHECKER-OWNER-ABI:EFFECT-DIN-CELLS-OFF constant DECL-EFFECT-DIN-CELLS-OFF
CHECKER-OWNER-ABI:EFFECT-DOUT-CELLS-OFF constant DECL-EFFECT-DOUT-CELLS-OFF
CHECKER-OWNER-ABI:EFFECT-DIN-SLOT-OFF constant DECL-EFFECT-DIN-SLOT-OFF
CHECKER-OWNER-ABI:EFFECT-DOUT-SLOT-OFF constant DECL-EFFECT-DOUT-SLOT-OFF
CHECKER-OWNER-ABI:EFFECT-DIN-CON-OFF constant DECL-EFFECT-DIN-CON-OFF
CHECKER-OWNER-ABI:EFFECT-DOUT-CON-OFF constant DECL-EFFECT-DOUT-CON-OFF
CHECKER-OWNER-ABI:EFFECT-STACK-STABLE-OFF constant DECL-EFFECT-STACK-STABLE-OFF
CHECKER-OWNER-ABI:EFFECT-DIN-QUOT-OFF constant DECL-EFFECT-DIN-QUOT-OFF
CHECKER-OWNER-ABI:EFFECT-DOUT-QUOT-OFF constant DECL-EFFECT-DOUT-QUOT-OFF
CHECKER-OWNER-ABI:EFFECT-QUOT-UP-OFF constant DECL-EFFECT-QUOT-UP-OFF
CHECKER-OWNER-ABI:EFFECT-QUOT-SIMPLE-OFF constant DECL-EFFECT-QUOT-SIMPLE-OFF
CHECKER-OWNER-ABI:EFFECT-CATCH-CELLS-OFF constant DECL-EFFECT-CATCH-CELLS-OFF
CHECKER-OWNER-ABI:EFFECT-EXEC-CELLS-OFF constant DECL-EFFECT-EXEC-CELLS-OFF
CHECKER-OWNER-ABI:EFFECT-FINALLY-CELLS-OFF constant DECL-EFFECT-FINALLY-CELLS-OFF
CHECKER-OWNER-ABI:EFFECT-MATCH-CELLS-OFF constant DECL-EFFECT-MATCH-CELLS-OFF
CHECKER-OWNER-ABI:CTL-DEAD-OFF constant DECL-CTL-DEAD-OFF
CHECKER-OWNER-ABI:WF-W-AT-OFF constant DECL-WF-W-AT-OFF
CHECKER-OWNER-ABI:REC-MIN-IN-OFF constant DECL-REC-MIN-IN-OFF
CHECKER-OWNER-ABI:REC-WIDE-PUBLISH-OFF constant DECL-REC-WIDE-PUBLISH-OFF
\ The scan whose verdict nobody enforces, with the render suppressed for it.
\ A TRUSTED: body is scanned only to fill the tape, so the suppression has to
\ happen in the instance that renders; the counter itself is unreachable from a
\ baked compiler, which is the whole reason this is an operation and not a cell.
CHECKER-OWNER-ABI:CHECK-UNJUDGED-OFF constant DECL-CHECK-UNJUDGED-OFF
\ The diagnostic that scan suppressed, rendered by the instance that scanned.
CHECKER-OWNER-ABI:CHECK-REPORT-OFF constant DECL-CHECK-REPORT-OFF
\ A hook-less definition's declaration, recorded as its row without authority.
CHECKER-OWNER-ABI:DECLARED-ROW-OFF constant DECL-DECLARED-ROW-OFF
\ A refused definition's cut back to the store end it began at, and that end.
CHECKER-OWNER-ABI:RETRACT-ROWS-OFF constant DECL-RETRACT-ROWS-OFF
CHECKER-OWNER-ABI:ROWS-END-OFF constant DECL-ROWS-END-OFF
\ The compile window: opened with the code a callback's checker write is refused
\ with once the definition's rows are recorded, shut with 0 before its own.
CHECKER-OWNER-ABI:WRITE-WINDOW-OFF constant DECL-WRITE-WINDOW-OFF
\ Family/variant ids and all metadata about them share the live source owner.
CHECKER-OWNER-ABI:FAMILY-MATCH-OFF constant DECL-FAMILY-MATCH-OFF
CHECKER-OWNER-ABI:FAMILY-CON-OFF constant DECL-FAMILY-CON-OFF
CHECKER-OWNER-ABI:FAMILY-CVAR-OFF constant DECL-FAMILY-CVAR-OFF
CHECKER-OWNER-ABI:FAMILY-VARIANT-OFF constant DECL-FAMILY-VARIANT-OFF
CHECKER-OWNER-ABI:FAMILY-SLOTS-OFF constant DECL-FAMILY-SLOTS-OFF
CHECKER-OWNER-ABI:FAMILY-VARIANTS-OFF constant DECL-FAMILY-VARIANTS-OFF
CHECKER-OWNER-ABI:FAMILY-NAME-OFF constant DECL-FAMILY-NAME-OFF
CHECKER-OWNER-ABI:VARIANT-TAG-OFF constant DECL-VARIANT-TAG-OFF
CHECKER-OWNER-ABI:VARIANT-PADS-OFF constant DECL-VARIANT-PADS-OFF
CHECKER-OWNER-ABI:VARIANT-PAY-CELLS-OFF constant DECL-VARIANT-PAY-CELLS-OFF
CHECKER-OWNER-ABI:VARIANT-PAY-TERMS-OFF constant DECL-VARIANT-PAY-TERMS-OFF
\ The verifier pre-pass: the symbol, definer and top-level questions it asks
\ the live checker, the stretch it reports, the clause a TRUSTED: definer
\ publishes, and the frame around one run.
CHECKER-OWNER-ABI:VERIFY-RECORD-SYM-OFF constant DECL-VERIFY-RECORD-SYM-OFF
CHECKER-OWNER-ABI:VERIFY-FIND-SYM-OFF constant DECL-VERIFY-FIND-SYM-OFF
CHECKER-OWNER-ABI:VERIFY-CREATES-SYM-OFF constant DECL-VERIFY-CREATES-SYM-OFF
CHECKER-OWNER-ABI:VERIFY-RECORD-CREATED-OFF constant DECL-VERIFY-RECORD-CREATED-OFF
CHECKER-OWNER-ABI:VERIFY-SOURCE-DOES-OFF constant DECL-VERIFY-SOURCE-DOES-OFF
CHECKER-OWNER-ABI:VERIFY-RENDERS-OFF constant DECL-VERIFY-RENDERS-OFF
CHECKER-OWNER-ABI:VERIFY-START-OFF constant DECL-VERIFY-START-OFF
CHECKER-OWNER-ABI:VERIFY-DONE-OFF constant DECL-VERIFY-DONE-OFF
CHECKER-OWNER-ABI:VERIFY-TOP-OFF constant DECL-VERIFY-TOP-OFF
CHECKER-OWNER-ABI:VERIFY-DEFERRED-OFF constant DECL-VERIFY-DEFERRED-OFF
CHECKER-OWNER-ABI:VERIFY-REACH-OFF constant DECL-VERIFY-REACH-OFF
CHECKER-OWNER-ABI:VERIFY-SYM-IDENTITY-OFF constant DECL-VERIFY-SYM-IDENTITY-OFF
CHECKER-OWNER-ABI:VERIFY-SYM-SOURCE-OFF constant DECL-VERIFY-SYM-SOURCE-OFF
CHECKER-OWNER-ABI:VERIFY-DEFERRED-BODY-OFF constant DECL-VERIFY-DEFERRED-BODY-OFF
CHECKER-OWNER-ABI:VERIFY-DECL-OFF constant DECL-VERIFY-DECL-OFF
CHECKER-OWNER-ABI:VERIFY-DECL-ARM-OFF constant DECL-VERIFY-DECL-ARM-OFF
CHECKER-OWNER-ABI:VERIFY-CTOR-DECL-OFF constant DECL-VERIFY-CTOR-DECL-OFF
CHECKER-OWNER-ABI:EXPORT-ACCEPT-OFF constant DECL-EXPORT-ACCEPT-OFF
CHECKER-OWNER-ABI:VERIFY-DECL-DISARM-OFF constant DECL-VERIFY-DECL-DISARM-OFF
CHECKER-OWNER-ABI:VERIFY-USES-OFF constant DECL-VERIFY-USES-OFF
\ Native multi-error recovery and diagnostics, answered by the live checker.
CHECKER-OWNER-ABI:MULTI-ERROR-OFF constant DECL-MULTI-ERROR-OFF
CHECKER-OWNER-ABI:RESET-REPORT-OFF constant DECL-RESET-REPORT-OFF
CHECKER-OWNER-ABI:JSON-REPORTED-OFF constant DECL-JSON-REPORTED-OFF
CHECKER-OWNER-ABI:RECOVERY-USED-OFF constant DECL-RECOVERY-USED-OFF
CHECKER-OWNER-ABI:DOES-UNJUDGED-OFF constant DECL-DOES-UNJUDGED-OFF
CHECKER-OWNER-ABI:NATIVE-DOES-FINISH-OFF constant DECL-NATIVE-DOES-FINISH-OFF
CHECKER-OWNER-ABI:NATIVE-DOES-REPORT-OFF constant DECL-NATIVE-DOES-REPORT-OFF
CHECKER-OWNER-ABI:CERT-BYTES-OFF constant DECL-CERT-BYTES-OFF
CHECKER-OWNER-ABI:CERT-KEEP-OFF constant DECL-CERT-KEEP-OFF
CHECKER-OWNER-ABI:CERT-KEPT-OFF constant DECL-CERT-KEPT-OFF
\ Completion: the names a top-level cursor sees.
CHECKER-OWNER-ABI:VERIFY-EACH-VISIBLE-OFF constant DECL-VERIFY-EACH-VISIBLE-OFF
CHECKER-OWNER-ABI:WITH-TICK-ORDER-OFF constant DECL-WITH-TICK-ORDER-OFF
CHECKER-OWNER-ABI:VERIFY-SOURCE-CLAUSE-OFF constant DECL-VERIFY-SOURCE-CLAUSE-OFF
;package


\ Below BODYBUF-OFF: $600..$690 is GENIO-ABI, AOT-SPAN and SIGNAL-ABI, $690
\ NCOMP-DISPATCH:RECOVER-CELL, $698..$6A8 LVS-OFF, $6A8..$6C0 the `,` run,
\ $6C0..$6D0 free, $6D0..$750 LVF-OFF, $750..$7D0 LVQ-OFF and $7D0..$7E0
\ FRAME-CELL and QFRAME-CELL.
$800 constant BODYBUF-OFF
\ The latest `,` run of a created body, defined in src/habu/created-body-abi.f,
\ which the checker reads too.
CREATED-BODY-ABI:COMMA-RUN-CELL constant COMMA-RUN-CELL
CREATED-BODY-ABI:COMMA-END-CELL constant COMMA-END-CELL
CREATED-BODY-ABI:COMMA-BASE-CELL constant COMMA-BASE-CELL
8000 constant BODYBUF-CAP
$568 constant RSP-CELL
$570 constant EXITH-CELL
$578 constant LVD-CELL
$580 constant LVH-OFF
\ LVF-OFF: the locals-frame bytes open when each DO level was entered, which
\ that level's `leave` releases back to. It takes the top LV-LEVELS cells of
\ the $698..$750 hole below LVQ-OFF.
$6D0 constant LVF-OFF
$750 constant LVQ-OFF      \ `?do` entry-branch offset per DO level; 0 for `do`
\ LVS-OFF: one byte per DO level, the JIT-SNAP frame index that level's `do` or
\ `?do` took, written when the level opens and read by `leave` to reconcile to
\ it without popping the frames above. LV-LEVELS bytes.
$698 constant LVS-OFF
\ LVH, LVF and LVQ hold one cell per level up to GENIO-ABI ($600), LVQ-OFF
\ ($750) and FRAME-CELL ($7D0): a definition nests at most this many `do`/`?do`
\ levels.
16 constant LV-LEVELS
$560 constant LASTC-CELL
$1F0 constant DOESP-CELL
$230 constant CREATEP-CELL
$238 constant QPATCH-CELL
$240 constant QENT-CELL
$248 constant QXH-CELL
\ FRAME-CELL: the open body's link-register frame. The JIT is single pass, so
\ `:` emits the save at entry and records the slot here, and every emitter that
\ puts a call in the body counts it in the low two bits (bit 0 one call, bit 1 a
\ second; habu2.f FRAME-CALL,), free because code addresses are four-byte
\ aligned. At `;` EM-COMPILE-RET reads where the slot is and whether x30 can be
\ destroyed, and EM-COMPILE-TAIL whether a tail branch removes the only call.
\ Zero means no frame. QFRAME-CELL scopes it across a quotation as QXH-CELL
\ scopes the EXIT chain. Definition-scoped, cleared by EM-RESET-COMPILE-STATE; a
\ snapshot zeroes what a closed quotation left in QFRAME-CELL (snap-lib.f
\ SND-ZERO-LIVE).
$7D0 constant FRAME-CELL
$7D8 constant QFRAME-CELL
$250 constant DEF-TKA-CELL
$258 constant DEF-TKL-CELL
\ CMM-CELL: compile-loop ADT-lowering mode (docs/forth.md "Structures And
\ Enums"), mirroring the checker's match-mode token machine: 0 = off; a
\ `construct`/`MATCH` keyword arms it so the operand tokens are captured before
\ the local/keyword/literal/call/undefined dispatch and never reach lookup. The
\ LCOMPILE head (EM-COMPILE-ADT-MODE) dies when it is armed with no handler.
\ Cleared at colon/TRUSTED: entry and by EM-RESET-COMPILE-STATE. The low $260
\ hole is not usable: VVAL-OFF ($250) + VSMAX cells spans $250..$350, and
\ DEF-TKA/DEF-TKL live inside it only while the definition NAME token runs, when
\ the virtual stack is empty.
$27A8 constant CMM-CELL
\ COMPILE-PREFLIGHT-CELL: the protected compile-immediate preflight hook. The
\ checker installs it before arming HOOK-CELL; the compiler calls it before
\ every source-defined immediate in a checked body, and 0 is an invalid armed
\ state. It persists through snapshots like HOOK-CELL; `0 set-check` parks it in
\ the SUSP-HOOK band below.
$27E8 constant COMPILE-PREFLIGHT-CELL
\ TOP-HOOK-CELL: top-row token hook xt (src/core/top-row.f); 0 = no hook.
\ Installed only through the fail-closed `set-top-check` prim (habu1.f
\ BSETTOPCHECK), which mirrors BSETCHECK's install window; the cell is its own
\ PROT-GUARD band (habu1.f GUARD-SPAN), so a post-seal raw store traps
\ ENGINE-ERROR:SEAL-VIOLATION like HOOK-CELL. Snapshot-persistent like HOOK-CELL.
\ When installed, the interpret dispatch points (habu2.f EM-INTERPRET-FIND /
\ EM-INTERPRET-NUMBER / the pushing string keywords / C-TICK / C-CHAR) emit one
\ event per token through LTOPHOOK; the hook's effect is ( ptr u8 n n n -- ):
\ token addr, token len, class (TOP-EV-*), LFIND flags.
$27F0 constant TOP-HOOK-CELL
\ The ENGINE-HOOK band is the two hook cells above.
COMPILE-PREFLIGHT-CELL constant ENGINE-HOOK-OFF
2 cells constant ENGINE-HOOK-LEN
\ SUSP-HOOK-CELL, SUSP-PREFLIGHT-CELL: the (hook, preflight) pair `0 set-check`
\ suspends until `set-check` of the same hook resumes it (the rule is
\ docs/forth.md's "`0 set-check` suspends the armed hook"). Boot zeroes both;
\ after the seal the set-check and discard-check prims (habu1.f BSETCHECK and
\ BDISCARDCHECK, kernel-x64.f SET-CHECK-BODY and DISCARD-CHECK-BODY) are their
\ only writers. The pair is a PROT-GUARD band of its
\ own: a resume copies SUSP-PREFLIGHT-CELL
\ into COMPILE-PREFLIGHT-CELL without set-preflight's window check, so a raw
\ post-seal store here would be a second, unchecked preflight installer. Both
\ cells hold xts and persist through snapshots like HOOK-CELL, so they are
\ declared address cells (habu2.f RELOC-EMIT:MARK-CELL) with a
\ tools/native-layout.f row each. $2D80 and $2D88 sit in the $2800..$3000 free
\ header band, below $7FF8 for the `DATA <off> LDR/STR` form and below
\ DATA-START.
$2D80 constant SUSP-HOOK-CELL
$2D88 constant SUSP-PREFLIGHT-CELL
SUSP-HOOK-CELL constant SUSP-HOOK-OFF
2 cells constant SUSP-HOOK-LEN
\ REFUSAL-ABI:CODE-CELL: the code of the last refusal whose diagnostic the checker
\ rendered before throwing it (src/core/checker.f CHECKER-REFUSE, which spells
\ this offset because this file loads after it), 0 until one is. The uncaught
\ top-level throw reporter (habu2.f LUNCAUGHT) exits RC-REJECT for a code equal
\ to it, as every refusal exits, and UNCAUGHT-RC for any other. A catch that
\ takes the throw leaves it set: src/core/include.f LOAD-CURRENT catches every
\ throw of a loaded file and rethrows the same code, so a refusal reaches the
\ reporter as that rethrow. A bare throw of the same code after a refusal of it
\ was caught therefore exits RC-REJECT too. Not sealed, because the checker
\ writes it after the seal; a snapshot carries none (snap-lib.f SND-ZERO-LIVE).
\ $27F8 follows the ENGINE-HOOK band.
package REFUSAL-ABI
public
$27F8 constant CODE-CELL
;package
\ EXIT-HOOK-CELL: the process-exit vector, an xt or 0 for no hook. The engine
\ calls it once with the exit code in x0, the cell cleared FIRST, immediately
\ before the exit_group of a DELIBERATE program exit: the normal top-level exit
\ (habu2.f EM-COMPILE-EXIT, LRBYE), `die` (habu1.f BDIE), the uncaught top-level
\ throw (habu2.f LUNCAUGHT) and a stripped application's own exit(0)
\ (src/habu/aot-link-arm.f EMIT-EXIT-HOOK). The engine's fail-closed exits - every
\ ENGINE-ERROR:* rc and the 70/74/75/76 band - do NOT call it: a broken engine
\ must not run program code. Clearing before the call is what stops a hook that
\ dies or throws from re-entering itself; the exit path it lands in finds the
\ vector empty. The catch handler chain (HND-CELL) is emptied with it, so a
\ throw from the hook is uncaught and ends the process with the hook's code:
\ `die` runs inside the program's catches, and a hook's throw caught there would
\ return from `die`. The REPL route (REPLH-CELL) is disarmed too, since a tty
\ REPL takes an otherwise uncaught throw back to its prompt. A stripped image's
\ exit follows its root's return, when the chain is already empty. The hook's
\ effect is ( -- ) and it runs on the data stack as it stands, because `die`
\ may be running on a task thread whose stack is its own.
\ It holds a code pointer, so it is declared for snapshot relocation beside the
\ other hook cells (habu2.f EM-DATA-INIT RELOC-EMIT:MARK-CELL) and carries the
\ tools/native-layout.f row every declared engine cell below the heap floor
\ needs. A stripped image claims it FRESH (src/habu/aot-owned-cells.f): a new
\ process has no hook, and the library that wants one arms it at runtime.
\ It is not in the ENGINE-HOOK-OFF band: those two are the checker's crown
\ jewels, sealed against a post-seal raw store, and this vector is armed by
\ ordinary library code (src/lib/fs-mutate.f) after the seal. $2818 follows
\ SRCLOC's three cells in the $2800..$3000 header band, below $7FF8 so the
\ engine names it with a `DATA <off> LDR/STR` 12-bit scaled immediate, and below
\ DATA-START so no compiled source reaches it by allot.
$2818 constant EXIT-HOOK-CELL
\ TOKBODY-CELL .. TOKFRF-CELL and TOKDEPTH-CELL: the token-start mark, seven
\ cells. Before every token the interpret loop stores BODYLEN-CELL, LOCN-CELL
\ and its block depth CAPDEPTH-CELL, CP (as an offset
\ from the region base, like every code position a DATA cell keeps, so no
\ image carries a live address) and the virtual stack's VSP-CELL, VRFREE-CELL
\ and FRFREE-CELL here (habu2.f EM-COMMENT). Only a definition's end or abort
\ also stores two of them: arm64 C-CLEAR-LOCALS zeroes the locals marks
\ TOKLOCN-CELL and TOKDEPTH-CELL with the locals themselves. A
\ throw that unwinds an evaluate frame begun inside an open definition leaves
\ that definition compiling (habu2.f EM-EVAL-THROW-RECOVER) and puts all seven
\ back: the text the checker certifies holds no token whose compile the throw
\ abandoned, a `{:` group refused part way declares none of its names, and the
\ code and the spill a refused token emitted before its check are gone. A spill
\ only moves virtual stack entries to the data stack and leaves the entries
\ themselves in place, so the depth and the two register masks restore the
\ virtual stack. Every other compile cell is the refusing word's to leave
\ alone: it checks first.
\ One mark serves every frame. Popping a frame leaves it at the innermost
\ buffer's current token, or at that buffer's end once it exits cleanly, a
\ point after a complete token, so each frame a throw pops keeps its buffer's
\ completed tokens, as the control-flow stack, locals and quotation state do
\ (test/runtime-regression-test.f GE-EMIT-DEEP).
\ $2820..$2848 follow EXIT-HOOK-CELL in the $2800..$3000 header band.
$2820 constant TOKBODY-CELL
$2828 constant TOKLOCN-CELL
$2830 constant TOKCP-CELL
$2838 constant TOKVSP-CELL
$2840 constant TOKVRF-CELL
$2848 constant TOKFRF-CELL
\ TKA-CELL as an opener stores PEND-CELL: the name token (`:`, kernel:,
\ TRUSTED:) or the word that ran def-open; C-DEF-SOURCE-END locates a source
\ that ends inside the definition there. CAST: and defer set PEND-CELL only
\ within their keyword, so no source can end inside them. A transient address
\ like TKA-CELL, zeroed at snapshot: no native-layout row, no stripped-image
\ claim.
$2850 constant PENDTKA-CELL
\ JIT-QUOT: quotations open inside quotations on tier 0. QPATCH-CELL, QENT-CELL,
\ QXH-CELL and QFRAME-CELL describe the INNERMOST open quotation, which is all
\ that J-EXIT, the locals guards and `;]` read. A `[:` opened while one is open
\ parks those four cells in the next frame, in that order, before it reuses
\ them, and its `;]` pops the frame back after closing its own body: the AOT
\ scanner's open row and the enclosing row each opener records
\ (src/compiler/native/elaborate.f QOPEN-ROW and QCLOSE-ROW). LEVELS is the most
\ quotations open at once, the checker's control-frame capacity (src/core/checker.f
\ CF-PUSH holds 32 frames and each `[:` takes one), so the loader refuses no
\ nesting the checker certifies; one more is refused by name (habu2.f
\ EM-QUOT-NEST-DIE). SP-CELL counts the parked frames. It is definition-scoped
\ like QPATCH-CELL and is zeroed beside it in every reset run, so a definition
\ that fails with quotations open leaves nothing parked; a snapshot zeroes
\ the frames themselves (snap-lib.f SND-ZERO-LIVE). The innermost
\ quotation never needs a frame, so there are LEVELS-1. Both sit after
\ PENDTKA-CELL in the $2800..$3000 free header band, below $7FF8 for the 12-bit
\ scaled `DATA <off> LDR/STR` form, and below DATA-START.
package JIT-QUOT
public
32 constant LEVELS                                  \ quotations open at once
32 constant FRAME-BYTES                             \ QPATCH, QENT, QXH, QFRAME
$2858 constant SP-CELL                              \ frames parked
$2860 constant STK-OFF                              \ base of the frame area
STK-OFF LEVELS 1- FRAME-BYTES * + constant END
;package
\ FLOORREC-CELL: runtime address of the underdepth throw entry (LFLOORREC,
\ habu2.f), stored at startup like EVALREC-CELL because the crash handler that
\ jumps there (crash.f C-CRASH-DATA-RECOVER) cannot name an emit-time label: a
\ stripped image bakes the same handler. The handler resumes a data access below
\ the base at that address, from the engine's own code or the JIT region, and
\ CODE-END-CELL bounds the first: [RBASE-CELL, CODE-END-CELL) is the engine code
\ up to its baked source (LSRC). Zero means no entry, and the fault keeps the
\ named bounds exit: a stripped image leaves both cells at its mapping's zero,
\ and a task region reads zero because src/lib/task.f copies only the cells it
\ names. Both hold this process's addresses, so boot stores them and snapshots
\ (snap-lib.f SND-ZERO-LIVE) zero them. They sit in the free header band for
\ the reasons EXIT-HOOK-CELL gives: $2CD0..$2CE8, past the quotation frames
\ (JIT-QUOT:END, $2C40); data-claims.f proves each cell disjoint at native
\ build time.
$2CD0 constant FLOORREC-CELL
\ CLOSED-FREE-CELL heads the idle closed-text data stacks, each idle stack's
\ first cell linking the next: habu1.f B-EVAL-CLOSED takes one (maps one when
\ the list is empty) and gives it back on the clean return, and habu2.f
\ LEVALREC gives back the stack of every closed frame a throw pops. The stacks
\ are this process's mappings, so boot (habu2.f EM-STARTUP-RUNTIME-STATE) and
\ snapshots (snap-lib.f SND-ZERO-LIVE) zero the cell. It sits beside
\ FLOORREC-CELL in the free header band.
$2CD8 constant CLOSED-FREE-CELL
$2CE0 constant CODE-END-CELL       \ end of the engine's own code: see FLOORREC-CELL
\ DATA-FLOOR-CELL: the lowest DATA offset a rewind of DATA may return DP to, 0
\ for none. The literal store (src/compiler/native/string.f PUBLISH) opens a
\ segment at `here` inside whatever is running and raises the floor to the
\ segment's end, so the segment and every address it answered outlive a failed
\ evaluation and a failed REPL line (habu2.f EM-EVAL-THROW-RECOVER,
\ EM-REPL-RECOVER) and a rolled-back generated declaration
\ (src/core/generated-declaration-dictionary.f DICTIONARY-DP!): each puts DP at
\ the higher of its saved value and the floor. An offset rather than an
\ address, so nothing relocates it: a snapshot carries it with the rest of
\ DATA, and a native or stripped image starts at 0, which no rewind reaches,
\ because every DP it saves is above the `here` it boots with. It sits past
\ CODE-END-CELL in the $2800..$3000 free header band, below $7FF8 for the
\ 12-bit scaled `DATA <off> LDR` form, and below DATA-START.
$2CE8 constant DATA-FLOOR-CELL
\ The callback CODE cells at $2CF8, $2D00 and $2D10 are declared in
\ native-observer-cells.f, which also loads on an older baked host during a build;
\ the host owner claims $2D10 separately from the publication and code
\ invalidation callbacks. Host dispatch and the current producer fact query
\ occupy $2D20..$2D30.
\ DEF-OCC:PTR-CELL holds this process's dictionary occurrence allocation, not an
\ image address: last-issued, then DICT-CAP u64 slots. A snapshot DATA copy
\ clears it and the receiving process restores its own.
package DEF-OCC
public
$2D08 constant PTR-CELL
DICT-CAP 1+ cells constant STATE-BYTES
;package
\ The x86-64 port's three header cells take $2D98..$2DA8, past
\ SUSP-PREFLIGHT-CELL ($2D88) and NSTR-ABI:INTERN-CELL ($2D90). DPHW-CELL takes
\ $2D58 and NDICT-HW-CELL $2D60; $2D78 is the unclaimed cell
\ test/native-build-host-unknown-code.f relies on.
\ The two shadow cells hold the native compiler's fixed and does> shadow
\ callbacks; TASK-CHAIN-CELL heads the x86-64 task chain. No captured image
\ keeps a live stack address.
\ TEXT-SIZE-CELL: the running image's text content size in bytes from the base
\ RBASE-CELL holds - the header's text-size field less IMAGE-TEXT-CONTENT-ADJ.
\ The words that bound the engine's text (src/core/source-producer.f
\ ENGINE-TEXT?, src/habu/terminal-call.f ENGINE-TEXT, src/habu/snapshot-format.f
\ TEXT-SIZE, src/habu/snap-lib.f ENGINE-TEXT-SIZE) read it rather than the layout
\ constants they were loaded with: a build window for another OS loads that OS's
\ header offsets. A size, not an address, so snapshots carry it unzeroed. Every
\ boot stores it before a word runs: arm64 habu2.f EM-TEXT-SIZE at cold boot and
\ after snapshot restore, x86-64 boot-x64.f DATA-INIT on every entry.
$2D50 constant TEXT-SIZE-CELL
\ DPHW-CELL: the release high-water, the highest DP (a DATA offset) that any
\ path lowering DP has lowered it from. Every such path updates it: habu1.f
\ DP-LOWER and BALLOT, habu2.f EM-EVAL-THROW-RECOVER and EM-REPL-RECOVER,
\ interpret.f FRAME-ROLLBACK-CODE, src/core/generated-declaration-dictionary.f
\ DICTIONARY-DP!, kernel-x64.f ALLOT-BODY. Space above both HERE and it is still
\ the zero of a fresh page, so `allot` zeroes only the part of its request below
\ it and never touches the pages a large allot reserves. 0 reads as nothing
\ released. A snapshot zeroes it (snap-lib.f SND-ZERO-LIVE): an image's bytes
\ must not depend on what its writer rolled back.
$2D58 constant DPHW-CELL
\ NDICT-HW-CELL: the largest record count NDICT was ever lowered from; the
\ dictionary high-water is the larger of NDICT and this cell. Records are written
\ only at the head, so only a lowering of NDICT leaves published records above
\ the count. Every lowering (ndict!, seed-ndict!, evaluate's and the REPL's
\ recovery) passes through habu1.f EM-DEF-OCC:LCOUNT or kernel-x64.f OCC-COUNT,
\ which raise this cell. A raising ndict! re-exposes the retired records below
\ it, so the does> patch clears alias kinds up to the high-water, not to NDICT
\ (habu2.f DOESPATCH CLEAR-KIND, kernel-x64.f DOES-KIND-CLEAR). 0 reads as
\ nothing retired. A snapshot zeroes it (snap-lib.f SND-ZERO-LIVE, through its
\ copy DICT-HW-CELL): an image keeps only the records below NDICT.
$2D60 constant NDICT-HW-CELL

\ INTERN-CELL holds the xt of the literal interner (src/compiler/native/string.f
\ NSTR:INTERN), 0 until that file installs it with xt!. Tier 0 reads it with
\ `DATA <off> LDR` (src/habu/habu2.f C-SDQ and its siblings): a zero cell keeps
\ the literal's bytes in the code.
package NSTR-ABI
public
$2D90 constant INTERN-CELL
;package
package NCOMP-DISPATCH
public
$2D98 constant FIXED-SHADOW-CELL
$2DA0 constant DOES-SHADOW-CELL
1 constant FIXED-VAL-KIND
2 constant FIXED-ADDR-KIND
;package
$2DA8 constant TASK-CHAIN-CELL
\ The design seal (src/lib/vocabulary.f) confines a sealed source to the
\ vocabulary its harness admitted. POLICY-NDICT-CELL is 0 while nothing is
\ sealed, else the NDICT the seal stored; a record at or above that index is the
\ design's own. POLICY-BITS-OFF is a WID-indexed bitmap shaped like
\ PROT-BITS-OFF's: bit w admits wordlist w, and every reader tests w against
\ PROT-WID-MAX before it addresses the band. habu1.f BPOLICYADMIT and
\ BPOLICYSEAL write the vocabulary; SOURCE-ROOT:POLICY-ROOT writes the ordinary
\ require xt before the seal. All three refuse mutation once sealed.
\ The readers are the places a source token is read: habu2.f LPOLICYREC for a
\ found record, LKWCMP for a matched keyword, LUNDEF for a miss,
\ NCOMP-EMIT:CAPTURE-IMMEDIATE for every tier-1 body token, and EMIT-QUALIFY-DEF
\ for a definition name, which under the seal may spell no dispatch row
\ (LROWWALK). The native compiler never reads them. The loader reads the
\ watermark through its own copy of the offset, src/core/include.f
\ INCLUDE-POLICY-NDICT-CELL.
\ Below $7FF8 for `DATA <off> LDR` and below DATA-START; data-claims.f proves
\ neither field overlaps a neighbour. The snapshot writer zeros its DATA copy
\ and the warm loader zeros the restored fields, so a fresh process is unsealed.
$4898 constant POLICY-NDICT-CELL
$48A0 constant POLICY-BITS-OFF
$4CA0 constant POLICY-REQUIRE-CELL  \ ordinary require xt; zero unless opted in
\ CLAUSE-CELL: nonzero once the pending definition has a `;does` clause - set by
\ the `does>` keyword and by does-record, which an imported definer reaches with
\ no keyword - and cleared wherever the engine clears DOESB-CELL. habu1.f
\ BNDAPPEND and its x86-64 twin read it to append the clause record; DOESB-CELL
\ stays the clause's offset into BODYBUF, which an imported definer never fills.
$4CA8 constant CLAUSE-CELL
\ Top-row event class codes: the protocol between the interpret dispatch and
\ an installed top-row hook. Word/tick events pass the LFIND flag word
\ (bit 0 found, bit 1 DNAME-IMM, bits 8-15 DNAME-MIN-IN); literals pass 0.
1 constant TOP-EV-NUM       \ number literal pushed ( n )
2 constant TOP-EV-STR       \ s" / S\" literal pushed ( ptr u8 n )
3 constant TOP-EV-CSTR      \ c" / C\" counted literal pushed ( ptr )
4 constant TOP-EV-CHAR      \ char literal pushed ( n )
5 constant TOP-EV-TICK      \ ' pushed a found word's xt
6 constant TOP-EV-WORD      \ found word about to execute (pre-BLR)
\ The user return stack is a guarded mapping (STACK-ABI): a band inside the
\ $8000 header cannot carry an inaccessible page. A slot is
\ [STACK-ABI:RETURN-BASE-CELL] + depth*8, the depth in RSP-CELL.
\ $2800..$3000 is free header space.
STACK-ABI:RETURN-CELLS constant RSTK-CELLS

\ The catch/throw handler frame (habu1.f BCATCH/BTHROW, habu2.f
\ EM-EVAL-THROW-RECOVER; bootstrap/cg/forth.fs mirrors it).
\ A HNDF-SIZE machine-stack frame chained through HND-CELL ([DATA+8]). It saves
\ RSP-CELL, LOOPSP-CELL and the active data-stack allocation, so a caught throw
\ restores the complete caller frame. Every delivery site (BTHROW, LEVLD, seed
\ BTHROW) validates the sentinel, saved depths and allocation before any restore
\ store. Keep the layout byte-for-byte in step with bootstrap/cg/forth.fs. The
\ native catch family (habu1.f BCATCH-FORWARD, BFINALLY, BC2-INVOKE;
\ kernel-x64.f twins) reads HALT-UNWIND-CELL to keep a halt unwinding; the frame
\ carries nothing for it.
STACK-ABI:CATCH-BYTES constant HNDF-SIZE
\ frame layout: 0 prev-HND | 8 data-sp(x19) | 16 machine-sp | 24 resume-pc
\               32 link | 40 saved-RSP | 48 saved-LOOPSP | 56 sentinel
\               64 saved-base | 72 saved-capacity
STACK-ABI:LOOP-FRAMES constant LOOP-STK-FRAMES
STACK-ABI:CATCH-MAGIC constant CATCH-FRAME-MAGIC

\ Compiler lowering transaction: all mutable pass-2 authority lives in one engine
\ band. The frozen source+certificate lives in a separate mmap'd allocation,
\ rounded to the largest target page, whose base and capacity are held in the
\ protected state; read-only during replay, unmapped at commit.
$5000 constant TXN-STATE-OFF
\ The declared extent: the cells end at TXN-LIVE-W-OFF + TXN-LIVE-W-CAP cells =
\ $5300. DATA-BANDS guards this length, so PROT-GUARD refuses a store anywhere in
\ it, and data-claims.f refuses a transaction cell past it: growing the
\ transaction is a layout edit the build checks.
$300 constant TXN-STATE-LEN
$10000 constant PROT-PAGE-MAX          \ the mapping alignment (= STACK-ABI:PAGE-BYTES; rt.f executes the agreement)

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

\ The run the transaction reserved and never used, handed to the libraries that
\ need storage a task owns. Every other candidate below TASK-REGION-BYTES ($10000,
\ the whole a task has) is unavailable to task libraries: USE-BAND and
\ SNAP-RELOC's XTCELL table extend past 2.6 MB and would alias in the main
\ thread's region. Pending-defer scratch lives after the engine's private bands,
\ outside the task arena. USER-REGION-END is the fixed library arena boundary.
$8000 constant USER-REGION-END

\ STRING-ABI is src/lib/string.f's SB builder, declared here because string.f is
\ baked into the engine below src/lib/ffi-abi.f, which requires it back:
\ `require src/lib/task.f` from string.f fails the engine build with an undefined
\ STR= inside ffi-abi's TOK-IS?, so SB cannot take a TASK:+USER row.
\ Carved from the top of the run so USER-BAND keeps the growable end.
package STRING-ABI
public
$400 constant SB-BUF-BYTES                  \ src/lib/string.f SB-CAP
8 constant SB-LEN-BYTES                     \ src/lib/string.f SB-LEN
SB-BUF-BYTES SB-LEN-BYTES + constant BYTES
USER-REGION-END BYTES - constant START
START constant SB-BUF-OFF
SB-BUF-OFF SB-BUF-BYTES + constant SB-LEN-OFF
USER-REGION-END constant END
;package

\ FMT-ABI is src/lib/fmt.f's integer render buffer, its four scratch cells, and
\ the base-10^9 limbs for exact binary64 scientific text. src/habu/habu2.f
\ requires fmt.f, so fmt is in the engine's own closure, and a require of
\ src/lib/task.f there would bake the task runtime (pthread, mmap, the FFI
\ staging tables) into the base image. The rule: a module the engine bakes takes
\ a declared band.
\
\ Carved directly below STRING-ABI. Every member is its measured width. The four
\ cells sit at the band base, a cell boundary, so they are cell-aligned whatever
\ the render buffer's width. The greatest exact coefficient is (2^53-1)*5^1074,
\ 767 decimal digits, so 86 nine-digit limbs suffice. BYTES is the member sum
\ rounded up to the cell the base needs.
package FMT-ABI
public
4 constant SCRATCH-CELLS               \ FMT-NUM-U, FMT-IX, FMT-FR, FMT-DV
86 constant SCI-LIMBS                   \ ceil(767/9), base 10^9
20 constant NUM-BUF-BYTES              \ src/lib/fmt.f FMT-NUM-CAP = STR-I64-DIGITS + 1
SCRATCH-CELLS SCI-LIMBS + cells NUM-BUF-BYTES + constant MEMBER-BYTES
MEMBER-BYTES 7 + 8 / 8 * constant BYTES     \ the band base is a cell boundary
STRING-ABI:START constant END
END BYTES - constant START
START constant NUM-U-OFF
NUM-U-OFF 8 + constant IX-OFF
IX-OFF 8 + constant FR-OFF
FR-OFF 8 + constant DV-OFF
DV-OFF 8 + constant SCI-LIMBS-OFF
SCI-LIMBS-OFF SCI-LIMBS cells + constant NUM-BUF-OFF
;package

\ FS-ABI is src/lib/fs.f's per-call slots: one cause cell and the five cells one
\ transfer threads, the stat buffer, the one-byte read probe and the NUL-padded
\ path copy. fs.f is not baked, but tools/native-build-core.f requires it to
\ build the engine, and a require of src/lib/task.f there would load the task
\ runtime into the build tool ahead of its target: a module the engine's build
\ needs takes a declared band too.
\
\ The product image tools/native-build.f emits carries this file's constants and
\ src/lib/errors.f's codes from the tree it was built in. fs.f loads from source
\ in the build tool, so an engine that predates a band here dies `E-UNDEFINED:
\ FS-ABI:STAT-BYTES`, rc 70, before any target work: build one engine from the
\ declaration alone (with E-FS-BAND), then the tree with it. A baked module costs
\ no such bootstrap: its require is a no-op in a booted engine.
\
\ The walk state is not here: fs.f maps a context per FS:WALK-FILES,
\ FS:WALK-FILES-IF or FS:REMOVE-TREE call, so two tasks walk at once.
\
\ Sized as FMT-ABI: the five cells at the band base, then the buffers.
package FS-ABI
public
6 constant IO-CELLS                    \ ERRNO, IO-FD, IO-LEN, IO-RD, IO-OFF, IO-WR
256 constant STAT-BYTES                \ src/lib/fs.f STAT-CAP
1 constant PROBE-BYTES                 \ src/lib/fs.f READ-PROBE-CAP
PATH-CAP 1 + constant PATHZ-BYTES       \ src/lib/fs.f FS:PATHZ-CAP
IO-CELLS cells STAT-BYTES + PROBE-BYTES + PATHZ-BYTES + constant MEMBER-BYTES
MEMBER-BYTES 7 + 8 / 8 * constant BYTES     \ the band base is a cell boundary
FMT-ABI:START constant END
END BYTES - constant START
START constant ERRNO-OFF
ERRNO-OFF 8 + constant FD-OFF
FD-OFF 8 + constant LEN-OFF
LEN-OFF 8 + constant RD-OFF
RD-OFF 8 + constant OFF-OFF
OFF-OFF 8 + constant WR-OFF
WR-OFF 8 + constant STAT-OFF
STAT-OFF STAT-BYTES + constant PROBE-OFF
PROBE-OFF PROBE-BYTES + constant PATHZ-OFF
;package

\ FS-MUT-ABI is src/lib/fs-mutate.f's per-call path scratch: the second
\ NUL-padded path every two-path syscall stages (rename, symlink, the stream
\ copy's destination), the temporary name FS:MKTEMP-DIR-IN builds and returns a
\ span into, and FS:ATOMIC-WRITE-FILE's `.tmp` sibling. Declared here for
\ FS-ABI's reason (tools/native-build-core.f requires fs-mutate.f), at the same
\ bootstrap cost: an engine that predates it dies `E-UNDEFINED:
\ FS-MUT-ABI:PATHZ2-BYTES`, rc 70.
\
\ Two things stay process-wide on purpose: the 8 KiB copy buffer (3072 + 8192
\ does not fit the 7408 bytes USER-BAND had free; COPY-FILE and
\ FS:COPY-FILE-STREAM are the module's one single-task pair) and the 64-path
\ cleanup registry, a process exit registry every task registers into.
\
\ Sized as FS-ABI; no cell lives in this band, so the rounding is its only slack.
package FS-MUT-ABI
public
PATH-CAP 1 + constant PATHZ2-BYTES      \ src/lib/fs.f FS:PATHZ-CAP
PATH-CAP constant ATOMIC-BYTES
PATH-CAP constant TMP-BYTES
PATHZ2-BYTES ATOMIC-BYTES + TMP-BYTES + constant MEMBER-BYTES
MEMBER-BYTES 7 + 8 / 8 * constant BYTES     \ the band base is a cell boundary
FS-ABI:START constant END
END BYTES - constant START
START constant PATHZ2-OFF
PATHZ2-OFF PATHZ2-BYTES + constant ATOMIC-OFF
ATOMIC-OFF ATOMIC-BYTES + constant TMP-OFF
;package

\ MEM-ABI is src/lib/memory.f's WITH-BYTES scope head: one cell holding the
\ running task's innermost live scope mapping, 0 when none is open. Each
\ mapping's header links the enclosing one, so nesting has no bound. memory.f is
\ baked, so it takes a declared band, at FS-MUT-ABI's bootstrap cost: the build
\ host compiles src/habu/snap-lib.f, which names MEM-ABI:START. A region is a
\ fresh zeroed mapping, so every task and every stripped image starts with no
\ scope open. Carved directly below FS-MUT-ABI, whose start is a cell boundary.
package MEM-ABI
public
1 cells constant BYTES
FS-MUT-ABI:START constant END
END BYTES - constant START
START constant HEAD-OFF
;package

\ USER-BAND is what src/lib/task.f `TASK:+USER` hands out, base and bound both.
package USER-BAND
public
TXN-STATE-OFF TXN-STATE-LEN + constant START
MEM-ABI:START constant END
;package

\ A `defer NAME ( E )` in src/core/checker.f before `: TRUST` cannot call
\ C-CALL-TRUST-PEND / C-CALL-CHECKER-DEFER: their LFIND of `trust` and
\ `checker-defer` fails. C-DEFER and the Habu loop's `defer` (src/habu/definers.f
\ PD-HOLD) then copy the qualified name and effect signature into a slot here;
\ DRAIN-PRETRUST, right after `: TRUST`, replays both registrations per slot, so
\ the table is empty by snapshot time. Overflow or an over-long name or sig dies
\ at declaration; a non-empty table at SEAL-CAPTURE dies. The table sits after
\ the private engine bands, below DATA-START, so its growth never moves the
\ using ABI or relocation maps. PD-TABLE-OFF is past the scaled-immediate range,
\ so slots use a computed base. Slot: [0]=name-len [8]=sig-len
\ [PD-NAME-OFF..)=name bytes [PD-SIG-OFF..)=sig bytes.
\ Unguarded engine scratch (BODYBUF class, not a PROT-GUARD band): a raw store
\ grants nothing beyond the public `trust`/`checker-defer` words, and the table
\ is empty whenever user source runs. test/protection-span.f pins this class at
\ the DATA-START edge.
128 constant PD-CAP                         \ pending slots with room beyond the checker prefix
48 constant PD-NAME-CAP                      \ max qualified defer-name bytes per slot
64 constant PD-SIG-CAP                       \ max effect-signature bytes per slot
0  constant PD-NLEN-OFF                       \ in-slot: name length (u64)
8  constant PD-SLEN-OFF                       \ in-slot: sig length (u64)
16 constant PD-NAME-OFF                       \ in-slot: name bytes
PD-NAME-OFF PD-NAME-CAP + constant PD-SIG-OFF \ in-slot: sig bytes
PD-SIG-OFF PD-SIG-CAP + constant PD-SLOT      \ per-slot stride
8 constant PD-SLOTS-REL                       \ slots begin after the u64 band count cell

\ Fields of each evaluate frame. Clean exit restores the using depth to the
\ buffer's floor (EVAL-FRAME:USE-FLOOR); throw recovery restores the entry depth
\ (PKGSNAP-USE), the used-public wids (EVAL-FRAME:USE-WIDS) and the package scope.
0  constant PKGSNAP-CUR
8  constant PKGSNAP-PUB
16 constant PKGSNAP-PRI
24 constant PKGSNAP-PARENT
32 constant PKGSNAP-REC
40 constant PKGSNAP-USE
package PKGSNAP
public
\ FLOOR saves the open package's using floor (USE-PKG-SAVE-CELL): a buffer that
\ closes the package, opens another and throws has set that cell to the other
\ package's floor.
48 constant FLOOR
;package


\ `using NAME` makes package NAME's public wordlist visible to bare lookup until
\ `;using`, `;package`, or the end of the load file. A fixed stack of public
\ wordlist IDs plus a depth cell, the one count of live usings: engine code owns
\ it (C-USING pushes, C-END-USING pops, `package`/`;package` and the eval-frame
\ and REPL boundaries save and restore it), and the checker reads the same cell
\ through `data-base USE-DEPTH-CELL +`, so its package-name mirror (checker.f
\ CK-USE-NAMES) has no counter of its own to drift. The depth is 0 at rest:
\ usings are file-local and closed before seal or snapshot.
16 constant USE-MAX                       \ concurrent `using` capacity (E-code on overflow)
\ A published engine ABI, not derived from the pending table's capacity:
\ checker.f loads before layout.f and reads this offset in the running compiler.
$9C08 constant USE-BAND-OFF
USE-BAND-OFF          constant USE-DEPTH-CELL      \ live using depth (u64)
USE-BAND-OFF 8 +      constant USE-PKG-SAVE-CELL   \ depth saved at `package` open (`;package` restores)
USE-BAND-OFF 16 +     constant USE-RPKG-SAVE-CELL  \ depth saved at REPL line start (recover restores)
USE-BAND-OFF 24 +     constant USE-WIDS-OFF        \ public-wid array base (USE-MAX u64 cells)
USE-WIDS-OFF USE-MAX cells + constant USE-BAND-END

\ Two tables that let a snapshot image be restored at a region address the
\ writing run never saw. Both live below DATA-START, so the snapshot DATA copy
\ carries them with no new image section, and both are keyed by offset, so their
\ contents are the same in every run. src/habu/habu2.f (the engine half) and
\ src/habu/snap-lib.f (the writer half) reopen this package.
package SNAP-RELOC
public

\ Exit status for a corrupt call map: a recorded region-to-text call site does
\ not hold a call instruction, so the region bytes and the call map come from
\ different builds or one is damaged; relocating would write a wild branch into
\ live code. These statuses live here beside BL-RANGE-RC, not in
\ src/core/engine-error.f: the engine emitter reads them from this file one
\ generation before a new src/core constant is reachable. That registry leaves
\ 95..99 and 103 to this file.
95 constant CALLMAP-RC
\ Exit status for an overfull address-cell table (more cells than XTCELL-CAP):
\ dropping a cell would leave a stale writer-run address in a restored image.
96 constant XTCELL-RC
\ Exit status for a corrupt address-literal map: a recorded site does not hold
\ the MOVZ/MOVK carrier the compiler emits there, so the region bytes and the
\ literal map disagree; rewriting its immediates would plant a wild address in
\ live code.
97 constant ADDRMAP-RC
\ Exit status for a declared address cell outside DATA: the writer's
\ canonicalise, the loader's relocate and the declaration itself index DATA by
\ the row's offset with no further check, so it is refused where first seen.
98 constant XTBAND-RC
\ Exit status for one address cell declared with both relocation kinds: either
\ winner would make snapshot or AOT relocation silently wrong.
99 constant XTKIND-RC

\ Call-site map: one bit per four-byte word of the JIT region, marking every call
\ whose callee is in the engine's loaded __text. Those are the only instructions
\ whose displacement differs between the writing and the restoring run: the
\ kernel picks the region base and the loader the image base independently. A
\ site is recorded when the call is created (habu2.f EMIT-CEMITBL, the call-emit
\ chokepoint, and EM-AOT-PATCH-SITES), because region bytes cannot be decoded
\ soundly: a compiled word may carry inline non-instruction data. The writer
\ rewrites each site as if the region sat REGION-OFF above __text; the loader
\ rewrites it for this run's distance. Sized from REGION, so it cannot overflow.
REGION 32 / constant CALLMAP-BYTES        \ one bit per region word (REGION / 4 / 8)
USE-BAND-END constant CALLMAP-OFF
CALLMAP-OFF CALLMAP-BYTES + constant CALLMAP-END

\ Address-literal map: the call map's shape, marking the first word of every
\ address carrier. A CODE carrier has four move-wide halves, shared DATA normally
\ three; CODE includes the entry a `[: ... ;]` pushes and the target a `[']`
\ pushes. A separate map, not a second call-map bit: the shapes, addresses and
\ rewritten fields differ, and each pass keeps the same bit-scan loop.
\ Recorded where the compiler decides a literal is an address: habu2.f
\ C-CODE-ADDR, C-DATA-ADDR and C-DATA-ADDR-RAW, the native chain's publication
\ seam, and the AOT seed's code-literal rebase (EM-AOT-RELOC-CODE).
\ DATA sites are recorded for the AOT capture: the captured blob lands at a
\ different DP in the seeded engine, so a DATA chain needs rebasing there, while
\ fixed-VA DATA needs nothing on snapshot restore. habu2.f EMIT-ADDRS runs once
\ per band (the JIT region, __text) and rewrites a chain only when its address
\ falls in that band; DATA at the fixed DATA-VA lies above both at both ends of a
\ snapshot (the writer folds the region to the RBASE-VA sentinel), so a DATA site
\ is rewritten by neither.
\ The map stores no kind: a consumer classifies a recorded value by its window's
\ DATA and code spans, which cannot overlap, and refuses a value in neither
\ (src/habu/aot-capture.f). Nothing recognises a chain from region bytes or from
\ its value: a word may hold inline data and an integer any value.
\ Sized from REGION like the call map.
REGION 32 / constant ADDRMAP-BYTES        \ one bit per region word (REGION / 4 / 8)
CALLMAP-END constant ADDRMAP-OFF
ADDRMAP-OFF ADDRMAP-BYTES + constant ADDRMAP-END

\ Site-row band: the x86-64 view of the same bytes as the two maps above,
\ [CALLMAP-OFF, ADDRMAP-END), 2 MiB. An x86-64 instruction is not a four-byte
\ word, so a bit per region word cannot name its sites: the `call rel32` opcode
\ and the REX byte of a `mov r64, imm64` sit at any byte offset. On that target
\ the bytes hold a u64 count at SITE-N-CELL and then that many rows from
\ SITE-ROWS-OFF, SITE-ROW-BYTES each: the region byte offset of the site's first
\ instruction byte as a little-endian u32, then its kind as a u8 at
\ SITE-KIND-OFF. The row names the instruction, as a bit names the BL word or a
\ chain's first MOVZ; the patched field is at +1 of a call and at
\ +MOVABS-IMM-OFF of a literal (src/habu/address-carrier.f).
\
\ Both views are declared on every target. src/ has no [if] and a top-level
\ `if` is refused on the product engine, so this file cannot choose one; which
\ view the bytes hold is decided at run time by HB-TARGET-LINUX-X86-64? in
\ src/habu/sites.f, the one reader of both.
\
\ The rows are strictly ascending by offset, one per offset, so the reader yields
\ a span's sites in bitmap order without sorting a live band. On x86-64
\ `callmap-set` and `addrmap-set` insert a row at its offset's place (an append
\ for code published at the code pointer, else a move of the rows above), so a
\ caller records sites in any order. `code-publish` and `reloc-maps-clear` remove
\ the rows of a span (src/habu/prims.f). Kinds start at 1, so a zeroed row is no
\ kind.
\
\ Unlike a bitmap, the band can fill. SITE-CAP rows fit; a writer refuses one
\ more with SITE-RC, and the reader refuses with the same status a count above
\ SITE-CAP, a row at or after REGION, a row of no known kind and a row not above
\ the one before it. SITE-RC lives here for the reason CALLMAP-RC does.
\
\ Snapshot relocation does not apply on x86-64: the region and DATA are mapped
\ at fixed addresses there. The rows serve publication and the AOT capture.
CALLMAP-OFF constant SITE-N-CELL                \ live row count (u64)
SITE-N-CELL 8 + constant SITE-ROWS-OFF
4 constant SITE-KIND-OFF                        \ the kind follows the u32 offset
SITE-KIND-OFF 1 + constant SITE-ROW-BYTES       \ offset u32 LE + kind u8
ADDRMAP-END SITE-ROWS-OFF - SITE-ROW-BYTES / constant SITE-CAP
1 constant SITE-CALL
2 constant SITE-ADDR
103 constant SITE-RC

\ Address-cell table: the DATA offset and kind of every persisted cell declared
\ to hold an address. XT cells hold JIT-region addresses, DATA-pointer cells
\ addresses in the DATA heap. Only XT values need snapshot canonicalisation:
\ region code moves on restore and DATA does not. Both kinds need the
\ declaration for AOT capture, where the captured DATA window moves and a raw
\ pointer into it would escape in sparse bytes. Membership is recorded where the
\ cell's kind is decided, never inferred from its contents (an integer can hold
\ any value): PERSISTED-PTR-VARIABLE registers its DATA-pointer cell, the
\ `defer`/`is` handlers register dispatch cells, and habu2.f registers the
\ engine hook cells by name at cold boot.
\ Legacy layout: a count, then XTCELL-CAP tagged offset cells; bit 63 is the
\ DATA-pointer kind and the rest the DATA offset. The engine appends an
\ identical declaration only once and refuses one cell declared with both kinds.
\ These coordinates freeze the original band and heap floor. New engines use
\ ADDRESS-CELLS' header and inline boot rows in this band, then grow into mmap.
\ XTCELL-CAP bounds only legacy host rows; it is not a runtime declaration limit.
65536 constant XTCELL-CAP
$8000000000000000 constant XTCELL-DATA-TAG
$7FFFFFFFFFFFFFFF constant XTCELL-OFF-MASK
ADDRMAP-END constant XTCELL-N-CELL        \ live count of used rows
XTCELL-N-CELL 8 + constant XTCELL-ROWS-OFF
XTCELL-ROWS-OFF XTCELL-CAP cells + constant XTCELL-END

\ The largest offset a declared cell may carry. The declaration (habu2.f
\ EMIT-MARK), the writer's canonicalise (snap-lib.f SND-CANON-XT-CELLS) and the
\ loader's relocate (habu2.f EMIT-XT) each access the eight bytes at the DATA
\ base plus the offset, so the whole cell must lie inside the region; the
\ unsigned compare also rejects negative offsets.
\ The cell need not be cell-aligned: `create` rounds its field (habu2.f LCREATE),
\ but a cell reached through `allot` after byte-sized data, or inside a copied
\ DATA window, keeps its residue (two adjacent `create X 4 cells allot` tables
\ measured 7 mod 8), and unaligned cell loads and stores are defined here.
DATA-SIZE 8 - constant XTCELL-OFF-MAX

;package

\ The application entry is outside compiler scratch storage. Zero preserves
\ ordinary hb CLI routing; a saved application calls this entry before input.
package APP-ENTRY
public
$43A0 constant XT-CELL
;package

\ AOT-ARM:OPEN writes B0-CELL, and WINDOW-CLOSE clears it. Tier 0 reads it to
\ keep a mention of a created word made before the window opened a call
\ (habu2.f EM-COMPILE-CALL).
\ T0 is latched when the seed allocates its wordlists: later boot-run entries
\ can allocate more, so the sealed-wordlist gate cannot derive it from WIDN.
package AOT-CELLS
public
$43A8 constant T0-CELL           \ first wordlist id the seed allocated for the window
$43B8 constant B0-CELL           \ first address of the open window's code span
;package

\ src/habu/jit.f LVSNAP and LVRECON snapshot the virtual stack at BEGIN and
\ reconcile to it at the back edge. The frames sit far above $7FF8, so jit.f
\ reaches them with LIT64 and an add, not `DATA <off> LDR` (12-bit scaled) or
\ `ADDI` (12-bit unscaled): two instructions per BEGIN and back edge at compile
\ time, for a band no header growth can collide with. Snapshot capture requires
\ zero compile depth; the loader clears the frames, which are live only inside a
\ definition.
\ The depth cell stays low: it is definition-scoped like VSP-CELL, LVD-CELL and
\ EXITH-CELL, so the colon handlers and EM-ABORT-COMPILE-STATE zero it in the
\ same run of `9 DATA <off> STR,` from one zeroed register; up here it would
\ need a 64-bit literal and a second register inside a routine that clobbers x9
\ only. Left out of that run, a definition that fails after an opener leaves the
\ depth raised.
package JIT-SNAP
public
\ One frame per open IF, BEGIN, WHILE or DO entry, pushed after C-CF-ROOM admits
\ the entry (`else` pops one and pushes one), so SP-CELL <= the CF depth <=
\ CFSTK-DEPTH-MAX and no push needs a check.
CFSTK-DEPTH-MAX constant FRAMES
24 constant FRAME-BYTES                             \ one frame is (k, p0, p1)
$378 constant SP-CELL                               \ depth; low, beside TIER-CELL
SNAP-RELOC:XTCELL-END constant STK-OFF              \ base of the frame area
STK-OFF FRAMES FRAME-BYTES * + constant END
;package

\ DATA-START: first offset of the user DP heap (allot/,/c,); everything below is
\ engine-reserved state (snapshot stores selected fixed sections); DP-CHECK bounds the heap
\ >= DATA-START; task-user cells stop at EVAL-TOP-CELL. Its reserved band
\ ends at $47C0, with PROT:RHI/PROT:CF taking the two cells directly
\ above them. The lowering state ends at $8000; the immutable lowering blob
\ lives outside DATA. Pending-defer scratch follows the private engine bands.

\ The active source observer is one native stack frame per owned file.
$2CA8 constant SOURCE-EXEC-CTX-CELL
208 constant SOURCE-EXEC-FRAME-BYTES
0 constant SX-LR
8 constant SX-PREV
16 constant SX-UNIT
24 constant SX-SRC
32 constant SX-LEN
40 constant SX-BEFORE
48 constant SX-AFTER
56 constant SX-CONT
72 constant SX-ARMED
80 constant SX-EVAL
88 constant SX-FLOOR
96 constant SX-RFLOOR
104 constant SX-PENDING
112 constant SX-TICKET
120 constant SX-DEPTH
128 constant SX-RDEPTH
136 constant SX-OP
144 constant SX-REC
152 constant SX-ENTRY
160 constant SX-SUSPEND
168 constant SX-WRITES
176 constant SX-CONSTRUCTION
184 constant SX-HANDLER
192 constant SX-POS

\ The checker overlay's replay scope (habu2.f DEFWRITE:REPLAY-OPEN /
\ REPLAY-CLOSE). replay-open saves the dictionary and scope cells here and sets
\ LATCH; replay-record raises HW, the highest NDICT a replay record reached;
\ replay-close checks the live state against these cells, restores them and
\ zeroes the whole band, LATCH included; replay-widn! lowers WIDN no further
\ than the saved one. The band is zero whenever no overlay is open. It is a
\ guarded band (DATA-BANDS below), not scratch: a raw store that forged the
\ saved NDICT, HW or WIDN would turn replay-close into a record eraser and
\ either into a WIDN setter. USE-RPKG-SAVE is REPL-line state, not scope, and
\ is not saved.
\ A fixed offset, because source reads LATCH: xref.f XREF-UNDEFINE-GUARD and
\ outer.f OVERLAY-GUARD read it as a DATA offset, and tools/native-build.f runs
\ the tree's copies of both in a donor whose reserved bands may differ, where an
\ offset derived from the moving bands names another band's cell. The band
\ takes the start of the unclaimed run $4208..$4398 below DECLARED-LOG:ADDR-CELL;
\ DATA-CLAIMS refuses a USE-MAX that would outgrow the run.
package REPLAY-SCOPE
public
$4208 constant LATCH                                \ 0: no overlay open
LATCH CELL + constant HW
HW CELL + constant NDICT
NDICT CELL + constant CP
CP CELL + constant WIDN
WIDN CELL + constant CUR
CUR CELL + constant PKG-PUB
PKG-PUB CELL + constant PKG-PRI
PKG-PRI CELL + constant PKG-PARENT
PKG-PARENT CELL + constant PKG-REC
PKG-REC CELL + constant USE-DEPTH
USE-DEPTH CELL + constant USE-PKG-SAVE
USE-PKG-SAVE CELL + constant USE-WIDS               \ USE-MAX cells
USE-WIDS USE-MAX cells + constant END
;package

\ The scoped package evaluator owns one transient dispatch xt. A source load
\ disarms it on return or throw; a snapshot refuses a capture with one armed
\ (snap-lib.f VERIFY-QUIESCENT). A fixed offset for REPLAY-SCOPE's reason:
\ outer.f UNIT? and UNIT-EVENT read the cell as a DATA offset and execute a
\ nonzero one as an xt, in a donor too. It takes the cell after REPLAY-SCOPE;
\ DATA-CLAIMS refuses a replay scope that grows into it.
$42E8 constant UNIT-COMPILE-CELL

\ The protected DATA bands, as one table.
\ Every engine's span guard walks this table: arm64 GUARD-SPAN (src/habu/habu1.f)
\ and x86-64 PROT-SPAN (src/habu/kernel-x64.f). Each builder reads it twice, for
\ the hull [LO, HI) of its bounding test and for the per-band interval tests, so
\ one added row widens both. The zero-length row ends every walk, so no count
\ can fall out of step with the rows. src/habu/data-claims.f BANDS-ASSERT checks
\ every row against its DATA-CLAIMS claim when an engine is built.
\ bootstrap/cg/forth.fs mirrors the six bands its stage0 engine owns.
package DATA-BANDS

create TAB
   FRIEND-ARENA ,                 FRIEND-ARENA-LEN ,
   PROT-REG-OFF ,                 PROT-REG-LEN ,
   ENGINE-HOOK-OFF ,              ENGINE-HOOK-LEN ,
   SUSP-HOOK-OFF ,                SUSP-HOOK-LEN ,
   NCOMP-DISPATCH:TIER-CELL ,     1 cells ,
   NCOMP-DISPATCH:DEF-TIER-CELL , 3 cells ,
   UNIT-COMPILE-CELL ,            1 cells ,
   SOURCE-EXEC-CTX-CELL ,         1 cells ,
   REPLAY-SCOPE:LATCH ,           REPLAY-SCOPE:END REPLAY-SCOPE:LATCH - ,
   BODYBUF-OFF ,                  BODYBUF-CAP 2 + ,
   TXN-STATE-OFF ,                TXN-STATE-LEN ,
   0 ,                            0 ,

public

: OFF ( n -- n )
   {: ix:n :}
   ix 2 * cells TAB + @ ;

: LEN ( n -- n )
   {: ix:n :}
   ix 2 * 1 + cells TAB + @ ;

: LO ( -- n )
   0 OFF  1 BEGIN dup LEN 0 <> WHILE
      dup OFF rot min swap  1+
   REPEAT drop ;

: HI ( -- n )
   0 OFF 0 LEN +  1 BEGIN dup LEN 0 <> WHILE
      dup OFF over LEN + rot max swap  1+
   REPEAT drop ;

;package

\ Pending storage is private to the pre-trust capture (habu2.f C-PD-CAPTURE,
\ definers.f PD-HOLD) and DRAIN-PRETRUST. Its capacity may grow without moving
\ any published engine slot or live map.
JIT-SNAP:END constant PD-TABLE-OFF
PD-TABLE-OFF PD-SLOTS-REL + PD-CAP PD-SLOT * + constant PD-TABLE-END

\ A definition compiled with the hook cell empty has its declaration as its row
\ (src/core/checker.f CHECKER-DECLARED-ROW!), recorded by the source owner. A
\ cold boot has no owner until checker.f claims the source at its end
\ (CLAIM-COLD-SOURCE), so the empty-hook publish arm (habu2.f
\ EM-COMPILE-PUBLISH-HOOKED, seed forth.fs EMIT-COMPILE-PUBLISH-HOOKED) appends
\ each signed `:` definition it publishes before then to this log; the claim
\ records every logged declaration's row (checker.f CK-DECLARED-LOG-DRAIN), then
\ zeroes the slots and ADDR-CELL. The log is empty whenever user source runs and
\ at every capture.
\ A slot is four cells: the pending record (PEND-CELL), the open package's
\ namespace record (PKG-REC-CELL, 0 outside a package), the signature's address
\ and its length. The checker keys each row by the record's wordlist and that
\ namespace record, read through `ptr-field`, so no record number crosses.
\ CAP: the prefix through checker.f publishes about 2330 signed `:` definitions;
\ a full log dies at the definition, exit 72 (the pre-trust table's
\ boot-integrity class), rather than lose a row. Unguarded engine scratch, as the
\ pre-trust table: the claim records rows without authority from it.
\
\ ADDR-CELL holds the log's address while it holds entries, else 0. The seed
\ lays its bands out at other offsets (forth.fs SNAPSTK-END), so the checker
\ finds the log through this one fixed cell (checker.f CK-DECLARED-LOG-CELL);
\ an engine without the log reads 0, an empty log. Below $7FF8 for
\ `DATA <off> LDR` and below DATA-START, it is the last cell of the run
\ $4208..$43A0 below APP-ENTRY:XT-CELL, which the seed leaves free; REPLAY-SCOPE
\ and UNIT-COMPILE-CELL hold the start of that run.
package DECLARED-LOG
public
$4398 constant ADDR-CELL
4096 constant CAP
0  constant REC-OFF                                \ in-slot: the pending record
8  constant PKG-OFF                                \ in-slot: the namespace record or 0
16 constant SIG-A-OFF                              \ in-slot: the signature's address
24 constant SIG-U-OFF                              \ in-slot: the signature's length
32 constant SLOT                                   \ per-slot stride
8  constant SLOTS-REL                              \ slots begin after the u64 count cell
PD-TABLE-END constant OFF
OFF SLOTS-REL + CAP SLOT * + constant END
;package
DECLARED-LOG:END constant DATA-START

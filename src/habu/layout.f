split-str
\ layout.f - shared native image and dictionary layout constants.
\ A start-up row loaded before require exists, so it requires nothing.

20 constant XREG-RBASE
26 constant DBASE
27 constant NDICT
28 constant CP

\ The general registers the running engine occupies.
\ A compiled routine that writes one of these destroys the engine, except
\ X64-INDEX, which any callee may write.
\ A register claimed here is rejected by every GPR allocation path.
\ Both targets' registers are declared on every target: this file cannot branch
\ at top level and an ARM64 engine emits x86-64 code.
package ENGINE-GPR
public

19 constant A64-DSTACK

1 A64-DSTACK lshift
1 XREG-RBASE lshift or
1 DBASE lshift or
1 NDICT lshift or
1 CP lshift or
constant A64-MASK

\ Each is the twin of the ARM64 register named beside it.
3 constant X64-DSTACK          \ rbx, the data stack pointer: A64-DSTACK
12 constant X64-INDEX          \ r12, the innermost loop index: t0-arm64's x29
5 constant X64-RBASE           \ rbp, the user area: XREG-RBASE, x20
13 constant X64-DBASE          \ r13, the code region the records live in: DBASE
14 constant X64-NDICT          \ r14, the dictionary record count: NDICT
15 constant X64-CP             \ r15, the code pointer: CP

1 X64-DSTACK lshift
1 X64-RBASE lshift or
1 X64-INDEX lshift or
1 X64-DBASE lshift or
1 X64-NDICT lshift or
1 X64-CP lshift or
constant X64-MASK

: DSTACK ( -- n )
   HB-TARGET-LINUX-X86-64? if X64-DSTACK exit then
   HB-TARGET-LINUX? HB-TARGET-MACOS? or if A64-DSTACK exit then
   s" layout: ENGINE-GPR: unknown target" EXIT-STATUS:INTERNAL die ;

;package

\ Fixed budget for dictionary and code, including the retained build host and
\ its replacement core.
$2300000 constant REGION
\ A definition's CP and name slots stay below this offset above DBASE, so the
\ region's last $4000 bytes stay the runtime's protected tail.
REGION $4000 - constant CODE-CEILING
\ The code region's offset past the largest text and its read-write tail: from
\ the image's base where Linux reserves it, from __text where macOS hints it.
$2000000 constant REGION-OFF
\ Where the ELF image's carrier load starts, from its base: past the code region.
REGION-OFF REGION + constant CARRIER-OFF
\ AArch64 BL imm26 reach (+/-128 MiB): direct-BL emission relies on the code
\ region lying within it of __text.
$8000000 constant BL-REACH

\ DICT-SIZE = CFSTK-OFF (= DICT-CAP * DREC record slots) + $1000 control-flow
\ stack; the code area follows at DBASE+DICT-SIZE inside the REGION.
\ Keep DICT-BITS/CFSTK-OFF/DICT-SIZE in step. Maki's STEP
\ import needs over 65,536 checked records.
$601000 constant DICT-SIZE
\ CODE-BAND:BYTES: what REGION leaves for emitted code, [DBASE+DICT-SIZE,
\ DBASE+REGION). The two bands share one budget.
package CODE-BAND
public
REGION DICT-SIZE - constant BYTES
;package
48 constant DREC
\ A name past DNAME-INL bytes or a `;does` clause's (DOES-REC) is out of line:
\ [24] holds its address and [16] DNAME-EXT, which readers test, not the length.
8 constant DNAME-INL
1 constant OWNER-API-PUB-WID
2 constant OWNER-API-PRI-WID
4 constant FIRST-DYNAMIC-WID
\ A `constant` or `create`d word pushes one decided value, and a compiled mention
\ means that value. Only the definer knows this: the value lives in emitted
\ instructions the region rule below forbids reading, and running a word to ask
\ is its side effects. The definer stamps the kind; every reader asks the record.
\ The two-bit kind is an enum: a `constant` pushes a number, a `create`d or
\ `variable` word a DATA address the link rewrites to the image's, so folding
\ the second as the first bakes in the building engine's address. CAST: uses the
\ remaining kind for a declared identity retype: its body stays callable, but a
\ compiled mention only renames the checked value.
\ A stamped record's body is the definer's own.
\ Band: bits 50-51 of the record flags cell [16].
package DKIND
public
$0004000000000000 constant VAL       \ kind 1: the body pushes a decided number
$0008000000000000 constant ADDR      \ kind 2: the body pushes its DATA address
VAL ADDR or constant CAST           \ kind 3: a declared identity retype
VAL ADDR or constant MASK
;package
\ DNAME-ALLOC: the aligned DATA allocation start of a created word.
CREATED-BODY-ABI:DNAME-ALLOC constant DNAME-ALLOC
\ DNAME-CELL names the exact one-cell DATA extent made by variable. CREATE
\ leaves it clear: a created body may allot bytewide storage.
$0001000000000000 constant DNAME-CELL
\ DNAME-REP (bit 46) marks a CAST: record (kind 3 above) whose certified
\ output mints no pointer and no quotation: a representation cast, a scalar
\ retype the native compiler lowers as the identity it is. Scoped view casts and
\ LINEAR: records never carry it.
$0000400000000000 constant DNAME-REP
\ DNAME-SEALED (bit 45): set on a package's namespace record by seal-package,
\ and on every engine package by its build before program source.
$0000200000000000 constant DNAME-SEALED
\ DNAME-TARGET (bit 44): a target word, which a cross-build refuses to run.
$0000100000000000 constant DNAME-TARGET
\ [16] bits 44-63: flags above the name length, which every length read clears by this count.
20 constant DNAME-FLAG-BITS
-1 DNAME-FLAG-BITS rshift constant DNAME-LEN-MASK
$1000000000000000 constant DNAME-IMM
$2000000000000000 constant DNAME-EXT
\ DNAME-WIDE (bit 62): the word's recorded effect carries a wider-than-cell
\ value in some row. Such a word runs at top level: the interpret row is typed
\ per cell and carries each multi-cell value as one item of its type, and the
\ checker refuses only an engine stack primitive that would move part of one
\ (src/core/check-walk.f TOP-XPORT-GUARD). Compiled calls inside checked
\ definitions are unaffected.
$4000000000000000 constant DNAME-WIDE
\ DNAME-INT (bit 63): a kernel primitive or engine helper no source may run.
\ Compile-mode references (unchecked code, TRUSTED:
\ bodies) are declared trusted boundaries and unaffected.
$8000000000000000 constant DNAME-INT
\ DICT-CAP: dictionary record slots.
17 constant DICT-BITS
1 DICT-BITS lshift constant DICT-CAP
$600000 constant CFSTK-OFF

\ Profiler counter band: one 64-bit sample counter per dictionary slot, reserved
\ at the very top of the DATA region [DATA-SIZE - PROF-CNT-BYTES, DATA-SIZE).
\ The band opens with PROF-STATE-BYTES of the profiler's own cells (sample
\ total and limit, the two buckets, the dictionary base recorded by prof-on, the
\ arena, the clock, the handlers inside and the control words' ticket lock) and
\ the counters follow.
80 constant PROF-STATE-BYTES
DICT-CAP cells PROF-STATE-BYTES + constant PROF-CNT-BYTES

0 constant DP-CELL
8 constant HND-CELL
16 constant LOCN-CELL
\ The capture's block depth, which scopes the locals it records, and raw state:
\ 0 none, -1 a parsing keyword's operand, d+1 inside a `[type]` span at depth d.
$2DB8 constant CAPDEPTH-CELL
$2DC0 constant TOKDEPTH-CELL
$2DC8 constant RAWNEXT-CELL
$2DD0 constant TOKRAW-CELL
\ SEAL-NDICT-CELL holds the seal-time ndict watermark.
$A8 constant SEAL-NDICT-CELL

$28 constant CUR-CELL
$30 constant WIDN-CELL             \ the next wordlist id the engine hands out
$38 constant HOOK-CELL
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
\ DEFER-MAGIC: the first cell of the meta trailer `defer` writes just past a
\ deferred word's code, followed by the address of its dispatch cell. A reader
\ holding a record recognises a defer's trailer by this word, never by the
\ shape of what follows the code (an ordinary integer can hold any value).
$4842444546455201 constant DEFER-MAGIC
$1B8 constant BODYLEN-CELL
$1C0 constant RBASE-CELL
$1C8 constant LOOPSP-CELL
STACK-ABI:BASE-CELL constant S0-CELL
$3670 constant ARGC-CELL
$3678 constant ARGV-CELL
$3680 constant ENVP-CELL
\ PROGRAM-FILE-CELL: argv[1]'s string once the launcher runs it as the program
\ file, else 0.
$2DB0 constant PROGRAM-FILE-CELL
\ PEND-CELL: the record of the definition the engine holds open and not yet
\ published, 0 when none.
$3688 constant PEND-CELL
$3690 constant TKA-CELL
$3698 constant TKL-CELL
$36A0 constant INP-CELL
$36A8 constant INE-CELL
\ TOKENS-CELL: tokens the engine has read, a REPL line or evaluated text begun
\ counting as one and a restated token not. Only differences are read.
$36B0 constant TOKENS-CELL
\ BPTRAMP-CELL: the code address of the persistent-breakpoint trampolines,
\ BREAKPOINT:BYTES of trap instructions reserved in the code band.
$36C0 constant BPTRAMP-CELL
package BREAKPOINT
public
8 constant SLOTS
\ The longest form: two words and a quad on arm64, a 15-byte instruction and
\ `jmp rel32` on x86-64.
: TRAMP ( -- n ) HB-TARGET-LINUX-X86-64? if 20 exit then 16 ;
: BYTES ( -- n ) SLOTS TRAMP * ;
;package
$36D0 constant BPTAB-OFF
$37E8 constant BPWBASE-CELL
$37F0 constant BPWN-CELL
\ EVAL-TOP-CELL: the innermost evaluate frame, zero at rest. EVAL-PREV links
\ each to the frame it nests in.
$43C0 constant EVAL-TOP-CELL
$F8 constant EVAL-FRAME-SIZE
$40 constant EVAL-PREV
$48 constant EVAL-PKG
\ EVAL-INB: the outer evaluate's input-buffer start, which a nested evaluate restores.
$90 constant EVAL-INB
\ EVAL-INP, EVAL-INE: the outer input's read position and end, which the evaluate
\ puts back as it ends.
0 constant EVAL-INP
8 constant EVAL-INE
\ EVAL-TKA, EVAL-TKL: the outer input's last token (TKA-CELL, TKL-CELL), which
\ the evaluate puts back as it ends, so the token never names a buffer released
\ after it.
$A8 constant EVAL-TKA
$E0 constant EVAL-TKL
package EVAL-FRAME
public
\ PEND: PEND-CELL as the buffer began. A definition the buffer opened must
\ close in it, so its end refuses one still open; one already open here belongs
\ to an outer buffer, whose immediate word's evaluate compiles into it, and a
\ throw that unwinds this buffer leaves it compiling.
$A0 constant PEND
\ NDICT: the dictionary count as the buffer began.
$30 constant NDICT
\ ROWS: the checker's record end as the buffer began, plus one, 0 when no owner
\ records.
$B0 constant ROWS
\ HELD: the checker's held facts count as the buffer began, plus one, 0 when no
\ owner holds facts.
$E8 constant HELD
\ ENCLOSING: the enclosing package's four cells (ENCLOSING below), in its order,
\ which a throw puts back with the open package's.
$B8 constant ENCLOSING
\ PKG-OUTSIDE: PKG-OUTSIDE-CELL as the buffer began, which a throw puts back.
\ A clean exit does too unless a `package` ran; one left open is the includer's.
$D8 constant PKG-OUTSIDE
\ SECTION: SECTION-CELL as the buffer began, which its end puts back.
$98 constant SECTION
\ REQUIRES: the require registry's count as the buffer began.
$F0 constant REQUIRES
;package

\ PATH-CELL / PATHLEN-CELL: the innermost open include frame's path, 0/0 with
\ no include frame open: the root source stream's own text (stdin, tty or
\ piped, or the `--load` stream) and the boot prefix. The program file of a
\ plain `hb <file>` has a frame of its own. The address is the frame's, not
\ INCLUDE-PATH's, because a nested include overwrites that buffer.
\ INB-CELL: the start of the buffer INP walks. It is not beside INP/INE: $36C8
\ is INDEX-CELL.
\ All three sit in the $2800..$3000 free header band, below $7FF8 for a
\ `DATA <off> LDR` and below DATA-START.
package SRCLOC
private
EXPORT POINTER-STORAGE:RESERVED-PTR-U8-CELL
public
$2800 constant PATH-CELL
$2808 constant PATHLEN-CELL
$2810 constant INB-CELL
\ The loader alone publishes the execution bytes owned by the active file.
\ An inner evaluate changes INB, but never changes this file span.
$2D68 constant OWNED-CELL
$2D70 constant OWNEDLEN-CELL
\ CURSOR: the byte address the scanner reads next.
INP-CELL RESERVED-PTR-U8-CELL CURSOR
\ SOURCE-CELL: the address of the process's loader record, its top frame and
\ current root. include.f stores it with `ptr-cell!` as it loads, so a
\ cross-build's copy of include.f routes the store to the image it writes while
\ its code reads the running hb's record.
$2C48 constant SOURCE-CELL
\ The record's first cell is its top frame, the innermost open file's. A
\ frame's shared cells: the frame of the file it interrupted, 0 for none, the
\ file's path, its size, -1 until read, and its execution copy, 0 when it runs
\ the bytes after HEADER-BYTES.
0 constant REC-TOP
0 constant FR-PREV
2 cells constant FR-PATHLEN
4 cells constant FR-SIZE
6 cells constant FR-EXEC-COPY
18 cells constant FR-PATH
FR-PATH PATH-CAP + 1 cells + constant FR-ROOT
FR-ROOT PATH-CAP + 1 cells + constant HEADER-BYTES
\ READS-CELL: the process's compile-reads record (src/core/compile-reads.f), as
\ SOURCE-CELL holds its own: a cross-build's copy of that file uses hb's record.
$2C50 constant READS-CELL
;package

$27B0 constant DOESB-CELL
$27B8 constant TRUSTED-CELL
$37D0 constant EVALD-CELL
$3C88 constant TASK-TCB-CELL
\ HALT-UNWIND-CELL: nonzero while a TASK:HALT unwinds the task that observed it;
\ only that task's thread writes it. $3870 is below $7FF8 for `DATA <off> LDR`.
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

\ REGION-ALIAS:DELTA-CELL: the region's RW alias minus its RX base (DBASE). The
\ region is mapped twice: code runs from DBASE and every store into the region
\ goes to its address plus this delta. Each run maps its own alias, so the cell
\ is live state: the boot publishes it and every task region copies it. A
\ forked child maps its own alias at the parent's alias address, so the delta it
\ inherits stays true. Below $7FF8 so `DATA <off> LDR` names it directly.
\ Neither view changes protection after boot: compiling never calls mprotect.
package REGION-ALIAS
public
$43B0 constant DELTA-CELL
;package

\ SIGNAL-ABI: the baked async-signal-safe handler stub and the word it reads.
\ STUB-CELL is the stub's address, a program's sa_handler; FD-PTR-CELL is the
\ address of the fd word, through which a program stores its self-pipe's write
\ end; FD-CELL is the word's offset, claimed so nothing else takes it. Every
\ boot writes the two published cells and none writes the fd word: DATA is
\ mapped fresh, so it starts zero.
\ The stub reads the fd word at the absolute address DATA-VA + FD-CELL, never
\ through x20: a handler runs on whichever thread receives the signal, and that
\ thread's x20 is its own task region.
\ DATA-VA is fixed. A spawned task's region carries neither published cell, so
\ a library reads them once from the main task and keeps them.
\ The cells sit in the $600..$800 hole, below $7FF8 (the boot
\ stores them as `DATA <off> STR`) and below DATA-START.
\ A program reads the two published cells through STUB and FD.
package SIGNAL-ABI
private EXPORT BOOT:data-base
public
$678 constant STUB-CELL
$680 constant FD-PTR-CELL
$688 constant FD-CELL
\ Both answer zero where the boot published nothing.
: STUB ( -- n )
   data-base STUB-CELL + @ ;
\ Read as a pointer: an offset from this task's region would name a different
\ word per task, and a zero one in a spawned task.
: FD ( -- ^n )
   data-base FD-PTR-CELL + 0 ptr-field @ ;
;package


\ GENIO-ABI: the routing cells of the generic I/O layer that engine code reads;
\ everything else about a device is library storage.
\ OUT-CELL and IN-CELL hold a device index, never an address: 0 is the built-in
\ terminal, 1..DEVICES a row of the write table. They are per task.
\ The write table holds this run's write handlers, stored undeclared, so a
\ build's image never holds one; TASK-REGION-INIT copies the rows to a task.
\ DEVICES is small so the table costs no allocation on a microcontroller.
\ BUSY-CELL stops re-entry: a device write may reach `type`, and while one runs
\ the funnel takes the terminal path.
\ ACTIVE-CELL names the device an operation runs for: devices share their eight
\ quotations and a quotation cannot capture a local. The funnel
\ publishes it around the write, genio-core.f around every other operation.
\ The band is $600..$660 of the header hole below BODY-TEXT:BASE-CELL, below $7FF8
\ (the funnel's first instruction is `DATA <off> LDR`) and below DATA-START.
package GENIO-ABI
public
8 constant DEVICES
$600 constant OUT-CELL
$608 constant IN-CELL
$610 constant BUSY-CELL            \ output-funnel re-entrancy guard (0 = idle)
$618 constant ACTIVE-CELL          \ device index an operation is running for (0 = none)
$620 constant WRITE-OFF            \ DEVICES cells: row i-1 = device i's write operation
WRITE-OFF DEVICES 8 * + constant END
;package


\ UNCGH-CELL: runtime address of the uncaught-throw reporter, written by the
\ link: BTHROW, a leaf, branches through it.
$40C0 constant UNCGH-CELL
\ The callback band: C calling back into checked code.
\ Eight cells every region carries at the same offset, in the run between the
\ FFI length buffers ($40C8..$41C8) and APP-ENTRY:XT-CELL ($43A0). Every cell is
\ correct at zero, which is what a fresh task region, a fresh DATA mapping and a
\ stripped image's DATA all hold.
\   CB-XDS..CB-CP  the VM frame an outbound FFI core publishes after its pops and
\                  restores after its BLR, LIFO
\                  through the core's own $30 stack frame. CB-XDS 0 = no frame.
\   CB-OWNER       the thread pointer (TPIDR_EL0 on Linux, TPIDRRO_EL0 on macOS)
\                  of the one thread allowed on this region; 0 = free.
\   CB-FRAME       the live marshal frame of the innermost callback.
\   CB-XTS/ROWS    growable descriptor and binding tables, read through the
\                  main region at absolute address DATA-VA.
\ A binding row holds region, owner and move count. The owner is
\ 0 idle, a thread pointer inside the slot, CB-ROW-BUSY while a mover holds it,
\ or 4*n + CB-ROW-CALLER for a row bound to the caller's context with n threads
\ inside; such a row's region is 0.
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
16 constant CB-POOL                \ boot callback stubs
24 constant CB-ROW-BYTES
0 constant CB-ROW-REGION
8 constant CB-ROW-OWNER
1 constant CB-ROW-BUSY
2 constant CB-ROW-CALLER
\ pthread_getspecific's address and the key, in the main region only.
$4330 constant CB-TLS-GET
$4338 constant CB-TLS-KEY
\ The marshal frame CB-FRAME names while a dispatch runs: x0..x7 as C passed
\ them, then d0..d7 from CB-FRAME-FLOATS. The dispatch leaves its result in the
\ x0 or the d0 slot, and the thunk returns both.
$40 constant CB-FRAME-FLOATS
\ Dict-name hash index: twice DICT-CAP u32 slots; live keys fill half at most.
\ HIDX-MIX spreads one name's sequential wids, which a bare XOR packs in a run.
DICT-BITS 1 + constant HIDX-BITS
1 HIDX-BITS lshift constant HIDX-SLOTS
$9E3779B97F4A7C15 constant HIDX-MIX
HIDX-SLOTS 4 * constant HIDX-BYTES
\ HIDX:LOAD-MAX: the claimed-slot count at which LHIDXADD compacts the table
\ in place instead of letting chains grow toward a full wrap. Three quarters of
\ the table: strictly above DICT-CAP, so a compaction always lands the
\ count back at NDICT with room to spare and can never itself be at the bound,
\ and strictly below HIDX-SLOTS, so an insert always meets an empty slot within
\ its chain and the full-wrap path is unreachable.
package HIDX
public
HIDX-SLOTS 3 * 4 / constant LOAD-MAX
;package
\ ENGINE-MAIN:XT-CELL is the DATA cell an engine's boot calls its MAIN through.
\ It is a separate cell from APP-ENTRY:XT-CELL because that one also switches
\ the argv and stdin conventions to a saved application's. A fresh DATA mapping
\ reads it zero until set.
package ENGINE-MAIN
public
$3808 constant XT-CELL
;package

\ PROVIDED-XT: cells holding the running engine's words that primitives and
\ declarations call, stored with `xt!`; x86-64 restores them on every boot.
package PROVIDED-XT
public
$3848 constant EVALUATE-CELL
\ `evaluate-closed` runs its text through the word this cell holds.
$3858 constant EVALUATE-CLOSED-CELL
$3860 constant EVALUATE-RANGE-CELL
\ `define-word`, `compile,`, `;` and `literal` run the running
\ engine's interpreter words these cells hold.
$3878 constant DEFINE-CELL
$3890 constant COMPILE-CELL
$38B8 constant END-CELL
$3898 constant LITERAL-CELL
\ A definer appends an item of an ITEM-ABI op through the word this cell holds.
$3888 constant ITEM-CELL
\ A declaration makes each child word through CHILD-CELL's CHILD-RUN, with a
\ CHILD-ABI shape code; a definer reads the cell, since inside a build a name
\ would bind the tree's copy.
$38B0 constant CHILD-CELL
\ A storage definer the checker does not compile reads its declaration line
\ through the word this cell holds.
$38A8 constant STORAGE-LINE-CELL
\ A storage definer whose element is whole cells reads its line through the
\ word this cell holds.
$38C0 constant CELL-LINE-CELL
\ A definer's type and effect values are schema nodes the running engine's
\ CHECK:TYPE-OP, which this cell holds, makes from a TYPE-OP-ABI code.
$38A0 constant TYPE-OP-CELL
\ A definer declares a variable of a type node through the running engine's
\ word this cell holds.
$38C8 constant VARIABLE-CELL
\ A storage definer passes its written name to the running engine's word
\ this cell holds, which rejects it where the definer's words could not be
\ defined.
$38D0 constant SEAL-CELL
\ A declaration passes the name it will define to the running engine's word
\ this cell holds, which rejects it where `define-word` would.
$38D8 constant TARGET-CELL
;package

\ A child word's shape: from it the engine makes the child's body and the effect
\ it registers. An OFF child adds its parameter, a byte offset, to `^a`,
\ answering `^a` for CELL-OFF, `^^b` for PTR-OFF, `^u8` for BYTE-OFF.
\ A FIELD child's parameter is a committed field id, an INDEX child's a family
\ id, a VALUE child's the number it answers. STORED to ARENA-DISPOSE and
\ DEFERRED are storage children: STORED's parameter is 0, or its type's node
\ when the declaration spells none, that of the others their routine's entry,
\ and the effects of STORED, BUFFER, DEFERRED, DYNAMIC, SLOT, SLOTS, WORK,
\ WORKS, ARENA-NEW and ARENA-DISPOSE answer `^t` for the type t the declaration
\ spells or STORED's node is; HEAP-NEW answers it and HEAP-RELEASE takes it. An
\ INIT-GET or INIT-SET child's parameter is the committed field id it reads or
\ writes through its receiver. A MAKE or UNMAKE child's parameter is its
\ product family: MAKE takes the fields and answers the value, UNMAKE the reverse.
\ A NOMINAL-IN or NOMINAL-OUT child's parameter is its DEFTYPE family:
\ NOMINAL-IN takes n and answers the nominal, NOMINAL-OUT the reverse. A SPAN
\ child's parameter is the committed field id of a BUFFER field: it takes
\ `^F<..>` and answers the field's bytes as `SPAN:span<u8>`.

package CHILD-ABI
public
0 constant CELL-OFF
1 constant PTR-OFF
2 constant BYTE-OFF
3 constant FIELD
4 constant INDEX
5 constant VALUE
6 constant STORED
7 constant BUFFER
8 constant BIND
9 constant GROW
10 constant DYNAMIC
11 constant RESERVE
12 constant RELEASE
13 constant SLOT
14 constant SLOTS
15 constant WORK
16 constant WORKS
17 constant HEAP-NEW
18 constant HEAP-RELEASE
19 constant ARENA-NEW
20 constant ARENA-DISPOSE
21 constant INIT-GET
22 constant INIT-SET
23 constant MAKE
24 constant UNMAKE
25 constant NOMINAL-IN
26 constant NOMINAL-OUT
27 constant SPAN
28 constant DEFERRED

;package

\ TYPE-OP's codes on a and b: INT n, PTR a pointer to a, EFFECT an empty effect,
\ IN and OUT b pushed on effect a, NAMED the built-in type or family the a, b
\ bytes spell, READ the one type element they spell.
package TYPE-OP-ABI
public
0 constant INT
2 constant PTR
3 constant EFFECT
4 constant IN
5 constant OUT
6 constant NAMED
7 constant READ
;package

\ The checked interpreter's keyword test ( wid -- name len hit ), which every
\ engine's policy writer calls before admitting a package; RUNTIME:INSTALL
\ stores it.
package POLICY-ABI
public
$3850 constant KEYWORD-CELL
;package

$1D8 constant SSCR-CELL
$1E0 constant GTOD-SCRATCH
$200 constant VSP-CELL
$250 constant VVAL-OFF
32 constant VSMAX


package NCOMP-DISPATCH
public

\ The callback operations are NDEFINITION's, which the COMP-CELL compiler
\ uses. A fresh source
\ compiler may coexist with a retained active compiler in a build window; both
\ operations must follow the active compiler rather than dictionary names.
$3810 constant CALLBACK-STUB-CELL
$3818 constant CALLBACK-ENTRY-CELL
\ BODY-HOOK-CELL, when nonzero, is an xt `( -- ^u8 bool )` the `:` and
\ TRUSTED: heads call with their record pending.
$3838 constant BODY-HOOK-CELL
\ Tier 0's compiler: COMP's COMPILE, installed by COMP:INSTALL through
\ RUNTIME:INSTALL.
$3840 constant COMP-CELL
\ Tier 0's compiler for a body with no text ( val kind ), installed with
\ COMP-CELL; the Habu loop's `constant`, `create` and `defer` call it.
$6C0 constant FIXED-COMP-CELL

\ Latched at the colon (0 no compiler, 1 COMP-CELL); an
\ immediate may only select the NEXT definition.
$388 constant COMP-LATCH-CELL
\ RECOVER-CELL ( 0 | -1 ) -- whether the reader whose `;` called the compiler
\ recovers from a refused definition: DEF-SEMI? sets it around its compile,
\ catches E-COMP-RECOVERY and reads on. In a multi-error run the compiler then
\ abandons a refused definition unpublished without a second report; under any
\ other reader it refuses with E-COMP-VERDICT. Zero outside that compile, so no
\ image carries it set.
$690 constant RECOVER-CELL
\ The owner record each cell points at is DATA; its fields are execution tokens
\ of private checker operations.
$360 constant DECL-CELL
$368 constant TARGET-DECL-CELL
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
CHECKER-OWNER-ABI:EXPORT-OFF constant DECL-EXPORT-OFF
CHECKER-OWNER-ABI:TRUSTED-TICK-OFF constant DECL-TRUSTED-TICK-OFF
CHECKER-OWNER-ABI:PKG-RESYNC-OFF constant DECL-PKG-RESYNC-OFF
\ The rest are called by the engine and the source verifier.
CHECKER-OWNER-ABI:REC-WIDE-PUBLISH-OFF constant DECL-REC-WIDE-PUBLISH-OFF
\ The cut back to the store end a definition or buffer began at, that end, and
\ the held facts count.
CHECKER-OWNER-ABI:RETRACT-ROWS-OFF constant DECL-RETRACT-ROWS-OFF
CHECKER-OWNER-ABI:ROWS-END-OFF constant DECL-ROWS-END-OFF
CHECKER-OWNER-ABI:HELD-END-OFF constant DECL-HELD-END-OFF
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
CHECKER-OWNER-ABI:VERIFY-SHADOW-OFF constant DECL-VERIFY-SHADOW-OFF
CHECKER-OWNER-ABI:VERIFY-DECL-ARM-OFF constant DECL-VERIFY-DECL-ARM-OFF
CHECKER-OWNER-ABI:VERIFY-CTOR-DECL-OFF constant DECL-VERIFY-CTOR-DECL-OFF
CHECKER-OWNER-ABI:VERIFY-DECL-DISARM-OFF constant DECL-VERIFY-DECL-DISARM-OFF
CHECKER-OWNER-ABI:VERIFY-USES-OFF constant DECL-VERIFY-USES-OFF
\ Completion: the names a top-level cursor sees.
CHECKER-OWNER-ABI:VERIFY-EACH-VISIBLE-OFF constant DECL-VERIFY-EACH-VISIBLE-OFF
CHECKER-OWNER-ABI:WITH-TICK-ORDER-OFF constant DECL-WITH-TICK-ORDER-OFF
CHECKER-OWNER-ABI:VERIFY-SOURCE-CLAUSE-OFF constant DECL-VERIFY-SOURCE-CLAUSE-OFF
;package


\ The open definition's body text lives in one mapped block the engine grows:
\ text at [base, base+cap), then the BODY-SOURCE rows from base+cap, at most cap
\ of them, since every run appends at least its separator and rows at or past
\ BODYLEN drop first. A linked image holds 0 in the four cells, as the engine
\ stores them only in the running hb's header.
package BODY-TEXT
public
$800 constant BASE-CELL     \ the block's address, 0 before the first capture
$808 constant CAP-CELL      \ the text capacity in bytes, 0 before
$810 constant SIZE-CELL     \ the block's length in bytes, 17 * cap, 0 before; guards read BASE and SIZE
$818 constant ROWS-CELL     \ the BODY-SOURCE row count
8192 constant FIRST-CAP     \ the capacity mapped at the first capture, doubled as it grows
;package

\ A `define-word` definition's items {kind a b off len}, walked where each
\ starts in its text.
package BODY-ITEMS
public
$4340 constant BASE-CELL     \ the block's address
$4348 constant CAP-CELL      \ the capacity in rows
$4350 constant COUNT-CELL    \ the rows held
$4358 constant EFFECT-CELL   \ the effect's schema node, read for the signature
$4360 constant LOCALS-CELL   \ the LOCAL items so far: the next one's index
5 constant ROW               \ cells per row
0 constant KIND
1 constant A
2 constant B
3 constant OFF
4 constant LEN
64 constant FIRST-CAP        \ rows at the first item, doubled as it grows
;package

\ The kinds of an item row. NAME spans the definition's name and SIG its
\ signature with its parentheses; CALL's a is a record index; INT's a is the
\ value; TYPE's a is the schema node of the family `match` or `construct`
\ takes. A KEYWORD or OPERAND item is its spelling alone.
\ LOCAL's a is the local's type node and b its index; LOCAL-REF's a is that index.
package BODY-ITEM-KIND
public
1 constant NAME
2 constant SIG
3 constant CALL
4 constant INT
5 constant KEYWORD
6 constant TYPE
7 constant OPERAND
8 constant LOCAL
9 constant LOCAL-REF
;package

\ The ops of an item PROVIDED-XT:ITEM-CELL appends.
package ITEM-ABI
public
0 constant KEYWORD
1 constant OPERAND
2 constant TYPE
\ A call of the word the name binds where the definer runs.
3 constant CALL
\ A call of the word `PKG:TAIL` spells exactly, the top-level package's public
\ one, which no part can shadow.
4 constant CALL-EXACT
\ A local of a type node.
5 constant LOCAL
\ A read of a local's index.
6 constant LOCAL-REF
;package
\ The latest `,` run of a created body.
CREATED-BODY-ABI:COMMA-RUN-CELL constant COMMA-RUN-CELL
CREATED-BODY-ABI:COMMA-END-CELL constant COMMA-END-CELL
CREATED-BODY-ABI:COMMA-BASE-CELL constant COMMA-BASE-CELL
$568 constant RSP-CELL
$570 constant EXITH-CELL
$560 constant LASTC-CELL
$230 constant CREATEP-CELL
$250 constant DEF-TKA-CELL
$258 constant DEF-TKL-CELL
\ CMM-CELL: compile-loop ADT-lowering mode: 0 = off; a `construct`/`MATCH`
\ keyword arms it so the operand tokens are captured before the
\ local/keyword/literal/call/undefined dispatch and never reach lookup. The low
\ $260 hole is not usable: VVAL-OFF ($250) + VSMAX cells spans $250..$350, and
\ DEF-TKA/DEF-TKL live inside it only while the definition NAME token runs, when
\ the virtual stack is empty.
$27A8 constant CMM-CELL
\ COMPILE-PREFLIGHT-CELL: the protected compile-immediate preflight hook. The
\ checker installs it before arming HOOK-CELL; the compiler calls it before
\ every source-defined immediate in a checked body, and 0 is an invalid armed
\ state. `0 set-check` parks it in the SUSP-HOOK band below.
$27E8 constant COMPILE-PREFLIGHT-CELL
\ SUSP-HOOK-CELL, SUSP-PREFLIGHT-CELL: the (hook, preflight) pair `0 set-check`
\ suspends until `set-check` of the same hook resumes it. Both start zero.
$2D80 constant SUSP-HOOK-CELL
$2D88 constant SUSP-PREFLIGHT-CELL
SUSP-HOOK-CELL constant SUSP-HOOK-OFF
2 cells constant SUSP-HOOK-LEN
\ THROWS-CELL: the number of the newest throw this thread began, zero in a fresh
\ region. A kernel catch throwing a code it took on (habu1.f BRETHROW) keeps it;
\ finally's throw on counts a new one.
$27F8 constant THROWS-CELL
\ STAMP-CELL: the number of the throw that carries the kept record, 0 for none.
$2848 constant STAMP-CELL
\ EXIT-HOOK-CELL: the process-exit vector, an xt ( -- ) or 0. A deliberate exit clears it and the
\ handler chain (HND-CELL) before the call, so the hook's own die or throw ends the process.
$2818 constant EXIT-HOOK-CELL
\ EXIT-OWNER-CELL: the DATA base of the thread that owns the process exit, 0
\ while none does; below DATA-START so no allot reaches it.
$2840 constant EXIT-OWNER-CELL
package BOOT
private
CAST: >PTR ( n -- ^n )
public
\ The main region's EXIT-HOOK-CELL from any thread, the process's one vector.
\ Main reads data-base: a cross build's host is not mapped at DATA-VA.
: EXIT-VECTOR ( -- ^n )
   data-base TASK-TCB-CELL + @ 0= if
      data-base EXIT-HOOK-CELL + exit
   then
   DATA-VA EXIT-HOOK-CELL + >PTR ;
;package
\ TOKBODY-CELL .. TOKVSP-CELL, TOKDEPTH-CELL and TOKRAW-CELL: the token-start
\ mark, six cells. Before every token the interpret loop stores BODYLEN-CELL,
\ LOCN-CELL, CAPDEPTH-CELL, RAWNEXT-CELL, CP (as an offset from the region
\ base, like every code position a DATA cell keeps, so no image carries a live
\ address) and the virtual stack's VSP-CELL here. Two other writers: arm64's
\ end or abort of a definition zeroes TOKLOCN-CELL, TOKDEPTH-CELL and
\ TOKRAW-CELL with the locals, and a refused prefix operand, such as an `is`
\ target, lowers TOKBODY-CELL to its opener, zeroing TOKRAW-CELL when the
\ opener goes too. A throw that unwinds an evaluate frame begun inside an open
\ definition leaves that definition compiling and puts all six back: the text
\ the checker certifies holds no token whose compile the throw abandoned, a
\ `{:` group refused part way declares none of its names, and the code and the
\ spill a refused token emitted before its check are gone. A spill
\ only moves virtual stack entries to the data stack and leaves the entries
\ themselves in place, so the depth restores the virtual stack. Every other
\ compile cell is the refusing word's to leave alone: it checks first.
\ One mark serves every frame. Popping a frame leaves it at the innermost
\ buffer's current token, or at that buffer's end once it exits cleanly, a
\ point after a complete token, so each frame a throw pops keeps its buffer's
\ completed tokens, as the control-flow stack, locals and quotation state do.
$2820 constant TOKBODY-CELL
$2828 constant TOKLOCN-CELL
$2830 constant TOKCP-CELL
$2838 constant TOKVSP-CELL
\ PENDTKA-CELL: TKA-CELL as it stood when an opener stored PEND-CELL, the name
\ token (`:`, kernel:, TRUSTED:) or the word that ran def-open; a source that
\ ends inside the definition is located there. CAST: and defer set PEND-CELL
\ only within their keyword, so no source can end inside them. A transient
\ address like TKA-CELL: no stripped-image claim.
$2850 constant PENDTKA-CELL
\ FLOORREC-CELL: runtime address of the underdepth throw entry, stored at
\ startup because the crash handler that jumps there cannot name an emit-time
\ label: a stripped image bakes the same handler. Zero means no entry, and the
\ fault keeps the named bounds exit: a stripped image leaves the cell at its
\ mapping's zero, and a task region reads zero because src/lib/task.f copies
\ only the cells it names. It and CODE-END-CELL hold this process's addresses,
\ so boot stores them.
$2CD0 constant FLOORREC-CELL
\ FAULT-MARK-CELL: nonzero from the crash handler's delivery of a memory fault
\ as a throw until a catch resumes; a fault while it is set dies by the signal.
\ A task's region starts it zero: src/lib/task.f copies only the cells it names.
$2D48 constant FAULT-MARK-CELL
\ DBASE-CELL: the live region's base in the main DATA, every engine thread's
\ DBASE. An engine's boot stores it; a stripped program, which has no live
\ region, leaves it at its mapping's zero.
$2D90 constant DBASE-CELL
\ REJECTION-JSON-CELL: nonzero when rejections print as JSON. Every thread
\ reads the main DATA's cell; a task region's stays zero.
$2D98 constant REJECTION-JSON-CELL
\ REJECTION-SENT-CELL: the error records the process wrote to a descriptor. Every thread bumps the main
\ DATA's cell, a build's program hb's; a task region's stays zero.
$2DD8 constant REJECTION-SENT-CELL
\ REJECTION-CARRY-A-CELL, REJECTION-CARRY-U-CELL: the bytes of the record STAMP-CELL's throw carries, set by
\ the REJECTION copy that passed it, so a build's hb takes in its program's record before the space closes.
$2DE0 constant REJECTION-CARRY-A-CELL
$2DE8 constant REJECTION-CARRY-U-CELL
\ MAIN-STACK-TOP-CELL: the main thread's SP once an engine's boot has stored
\ the DATA image; every frame a Habu word makes on that thread lies below it.
\ A stripped program leaves it at its mapping's zero.
$2D30 constant MAIN-STACK-TOP-CELL
\ MAIN-THREAD-CELL: the main thread's pointer, CB-SELF's value, stored by an
\ engine's boot beside MAIN-STACK-TOP-CELL. A stripped program leaves it zero.
$2D38 constant MAIN-THREAD-CELL
\ CLOSED-FREE-CELL heads the idle closed-text data stacks, each idle stack's
\ first cell linking the next. The stacks are this process's mappings, so a
\ process starts with the cell zero.
$2CD8 constant CLOSED-FREE-CELL
\ CODE-END-CELL: the end of the kernel's code, [RBASE-CELL, CODE-END-CELL); a
\ linked program's words follow it. The linker maps the running hb's kernel
\ primitives by it.
$2CE0 constant CODE-END-CELL
\ DATA-FLOOR-CELL: the lowest DATA offset a rewind of DATA may return DP to, 0
\ for none. Storage published inside whatever is running (a literal segment, a
\ grown callback or task-row table) raises the floor to its end, so it and every
\ address into it outlive a failed evaluation and a rolled-back generated
\ declaration: each puts DP at the higher of its saved value and the floor. An
\ offset rather than an address, so nothing relocates it: a native or stripped
\ image starts at 0, which no rewind reaches, because every DP it saves is above
\ the `here` it boots with.
$2CE8 constant DATA-FLOOR-CELL
\ PRIM-TABLE-CELL: the kernel's primitive table, count in the cell before, as
\ an offset from RBASE-CELL's address; 0 in a stripped program.
$2CF0 constant PRIM-TABLE-CELL
\ DEF-OCC:PTR-CELL holds this process's dictionary occurrence allocation, not an
\ image address: last-issued, then DICT-CAP u64 slots. Every boot maps a fresh
\ one and stores it here.
package DEF-OCC
public
$2D08 constant PTR-CELL
DICT-CAP 1+ cells constant STATE-BYTES
;package
\ TEXT-SIZE-CELL: the running image's text content size in bytes from the base
\ RBASE-CELL holds - the header's text-size field less IMAGE-TEXT-CONTENT-ADJ.
\ The words that bound the engine's text read it rather than the layout
\ constants they were loaded with: a build window for another OS loads that OS's
\ header offsets. Every boot stores it before a word runs.
$2D50 constant TEXT-SIZE-CELL
\ DPHW-CELL: the release high-water, the highest DP (a DATA offset) that any
\ path lowering DP has lowered it from. Every such path updates it. Space above
\ both HERE and it is still the zero of a fresh page, so `allot` zeroes only the
\ part of its request below it and never touches the pages a large allot
\ reserves. 0 reads as nothing released.
$2D58 constant DPHW-CELL
\ NDICT-HW-CELL: the largest record count NDICT was ever lowered from; the
\ dictionary high-water is the larger of NDICT and this cell. Records are written
\ only at the head, so only a lowering of NDICT leaves published records above
\ the count. 0 reads as nothing retired.
$2D60 constant NDICT-HW-CELL

package NCOMP-DISPATCH
public
$2DA0 constant DOES-SHADOW-CELL
;package
$2DA8 constant TASK-CHAIN-CELL
\ The design seal confines a sealed source to the vocabulary its harness
\ admitted. POLICY-NDICT-CELL is 0 while nothing is sealed, else the NDICT the
\ seal stored; a record at or above that index is the design's own.
\ POLICY-BITS-OFF is a WID-indexed bitmap: bit w admits wordlist w, and every
\ reader tests w against POLICY-WID-MAX before it addresses the band. Every
\ writer of the vocabulary and of the require xt refuses once the seal is set.
\ Only the engine's token reader tests the vocabulary; the native compiler never
\ reads it. An engine's kernel object presets POLICY-NDICT-CELL to 0, so a fresh
\ engine starts unsealed.
$4898 constant POLICY-NDICT-CELL
$48A0 constant POLICY-BITS-OFF
8192 constant POLICY-WID-MAX
POLICY-WID-MAX 8 / constant POLICY-BITS-BYTES
$4CA0 constant POLICY-REQUIRE-CELL  \ ordinary require xt; zero unless opted in
\ CLAUSE-CELL: nonzero once the pending definition has a `;does` clause - set by
\ the `does>` keyword and by does-record, which an imported definer reaches with
\ no keyword - and cleared wherever the engine clears DOESB-CELL. DOESB-CELL
\ stays the clause's offset into the body text, which an imported definer never
\ fills.
$4CA8 constant CLAUSE-CELL
\ Top-row event classes: what a top-level token pushed or is about to run.
1 constant TOP-EV-NUM       \ number literal pushed ( n )
2 constant TOP-EV-STR       \ s" / S\" literal pushed ( str )
3 constant TOP-EV-CSTR      \ c" / C\" counted literal pushed ( ^ )
4 constant TOP-EV-CHAR      \ char literal pushed ( n )
5 constant TOP-EV-TICK      \ ' pushed a found word's xt
6 constant TOP-EV-WORD      \ found word about to execute (pre-BLR)

STACK-ABI:PAGE-BYTES constant PROT-PAGE-MAX   \ the mapping alignment

\ USER-BAND is the per-task header task.f keeps its cells in: TASK-DEFER-OFF,
\ then AREA-OFF, the running task's user area's address, 0 on the main thread.
package USER-BAND
public
$5000 constant START
START 1 cells + constant AREA-OFF
START 2 cells + constant END
;package

\ MEM-ABI is src/lib/memory.f's scope head: one cell naming the
\ running task's first scope chunk, 0 until its first scope; that chunk's header
\ names the innermost scope. A region is a fresh zeroed mapping, so every task
\ and every stripped image starts with no scope open.
package MEM-ABI
public
1 cells constant BYTES
USER-BAND:END constant START
START BYTES + constant END
START constant HEAD-OFF
;package

\ FMT-ABI is src/lib/fmt.f's integer render buffer, its four scratch cells, and
\ the base-10^9 limbs for exact binary64 scientific text. src/habu/habu2.f
\ requires fmt.f, so fmt is in the engine's own closure, and a require of
\ src/lib/task.f there would bake the task runtime (pthread, mmap, the FFI
\ staging tables) into the base image. The rule: a module the engine bakes takes
\ a declared band.
\
\ The four cells sit at the band base, a cell boundary, so they are cell-aligned
\ whatever the render buffer's width. The greatest exact coefficient is
\ (2^53-1)*5^1074, 767 decimal digits, so 86 nine-digit limbs suffice.
package FMT-ABI
public
4 constant SCRATCH-CELLS
86 constant SCI-LIMBS
21 constant NUM-BUF-BYTES
SCRATCH-CELLS SCI-LIMBS + cells NUM-BUF-BYTES + constant MEMBER-BYTES
MEMBER-BYTES 7 + 8 / 8 * constant BYTES
MEM-ABI:END constant START
START BYTES + constant END
START constant NUM-U-OFF
NUM-U-OFF 8 + constant IX-OFF
IX-OFF 8 + constant FR-OFF
FR-OFF 8 + constant DV-OFF
DV-OFF 8 + constant SCI-LIMBS-OFF
SCI-LIMBS-OFF SCI-LIMBS cells + constant NUM-BUF-OFF
;package

\ STRING-ABI is src/lib/string-option.f's SB builder, declared here because
\ string-option.f is baked into the engine below src/lib/ffi-abi.f, which
\ requires it back.
package STRING-ABI
public
$400 constant SB-BUF-BYTES
8 constant SB-LEN-BYTES
SB-BUF-BYTES SB-LEN-BYTES + constant BYTES
FMT-ABI:END constant START
START BYTES + constant END
private
START constant SB-BUF-OFF
SB-BUF-OFF SB-BUF-BYTES + constant SB-LEN-OFF
;package

\ FFI-ABI is src/lib/ffi-abi.f's per-task call staging, declared here because
\ ffi-abi.f is baked into the engine.
package FFI-ABI
public
\ The call stubs load x0-x7 and x8, the indirect-result register.
9 cells constant BUF-BYTES
8 cells constant FBUF-BYTES
16 cells constant STACK-BUF-BYTES
$40 constant DLBUF-BYTES
$3A00 constant START
START constant BUF-OFF
BUF-OFF BUF-BYTES + constant FBUF-OFF
FBUF-OFF FBUF-BYTES + constant STACK-BUF-OFF
STACK-BUF-OFF STACK-BUF-BYTES + constant DLBUF-OFF
DLBUF-OFF DLBUF-BYTES + START - constant BYTES
;package

\ FS-ABI is src/lib/fs.f's per-call slots. 64 text bytes hold every errno's
\ text on macOS (the longest is 47 bytes) and glibc (49).
package FS-ABI
public
6 constant IO-CELLS
256 constant STAT-BYTES
1 constant PROBE-BYTES
PATH-CAP 1 + constant PATHZ-BYTES
64 constant TEXT-BYTES
IO-CELLS cells STAT-BYTES + PROBE-BYTES + PATHZ-BYTES + TEXT-BYTES + constant MEMBER-BYTES
MEMBER-BYTES 7 + 8 / 8 * constant BYTES     \ the band base is a cell boundary
STRING-ABI:END constant START
START BYTES + constant END
START constant ERRNO-OFF
ERRNO-OFF 8 + constant FD-OFF
FD-OFF 8 + constant LEN-OFF
LEN-OFF 8 + constant RD-OFF
RD-OFF 8 + constant OFF-OFF
OFF-OFF 8 + constant WR-OFF
WR-OFF 8 + constant STAT-OFF
STAT-OFF STAT-BYTES + constant PROBE-OFF
PROBE-OFF PROBE-BYTES + constant PATHZ-OFF
PATHZ-OFF PATHZ-BYTES + constant TEXT-OFF

\ Fields of the buffer the stat64 and lstat64 primitives fill.
4 constant STAT-MODE-OFF                \ 16 bits
48 constant STAT-MTIME-SEC-OFF
56 constant STAT-MTIME-NS-OFF
64 constant STAT-CTIME-SEC-OFF
72 constant STAT-CTIME-NS-OFF
96 constant STAT-SIZE-OFF
$F000 constant S-IFMT
$8000 constant S-IFREG
$4000 constant S-IFDIR
$A000 constant S-IFLNK
;package

\ REASON-ABI is src/lib/reason.f's per-task cells, a band because reason.f
\ builds for Wasm, where src/lib/task.f does not.
package REASON-ABI
public
4 cells constant BYTES
FS-ABI:END constant START
START BYTES + constant END
START constant CODE-OFF
CODE-OFF 8 + constant TEXT-OFF
TEXT-OFF 8 + constant NEXT-OFF
NEXT-OFF 8 + constant HIDE-OFF
;package

\ FS-MUT-ABI, src/lib/fs-mutate.f's per-call path scratch, is the last band: the
\ bands' cells precede the path slots, so each stays inside a scaled load's reach.
package FS-MUT-ABI
public
PATH-CAP 1 + constant PATHZ2-BYTES
PATH-CAP constant ATOMIC-BYTES
PATH-CAP constant TMP-BYTES
PATHZ2-BYTES ATOMIC-BYTES + TMP-BYTES + constant MEMBER-BYTES
MEMBER-BYTES 7 + 8 / 8 * constant BYTES     \ the band base is a cell boundary
REASON-ABI:END constant START
START BYTES + constant END
START constant PATHZ2-OFF
PATHZ2-OFF PATHZ2-BYTES + constant ATOMIC-OFF
ATOMIC-OFF ATOMIC-BYTES + constant TMP-OFF
;package

\ A `defer NAME ( E )` in src/core/checker.f before the checker defines
\ `trust-decl` and `checker-defer` cannot register itself, so the Habu loop's
\ `defer` copies the qualified name and effect signature into a slot here;
\ DRAIN-PRETRUST, right after both, replays both registrations per slot, so
\ the table is empty after it. Overflow or an over-long name or sig dies
\ at declaration. The table sits after
\ the private engine bands, below DATA-START, so its growth never moves the
\ using ABI or relocation maps. PD-TABLE-OFF is past the scaled-immediate range,
\ so slots use a computed base. Slot: [0]=name-len [8]=sig-len
\ [PD-NAME-OFF..)=name bytes [PD-SIG-OFF..)=sig bytes.
\ The table is empty whenever user source runs.
128 constant PD-CAP                         \ pending slots with room beyond the checker prefix
48 constant PD-NAME-CAP
64 constant PD-SIG-CAP
0  constant PD-NLEN-OFF                       \ in-slot: name length (u64)
8  constant PD-SLEN-OFF                       \ in-slot: sig length (u64)
16 constant PD-NAME-OFF                       \ in-slot: name bytes
PD-NAME-OFF PD-NAME-CAP + constant PD-SIG-OFF \ in-slot: sig bytes
PD-SIG-OFF PD-SIG-CAP + constant PD-SLOT      \ per-slot stride
8 constant PD-SLOTS-REL                       \ slots begin after the u64 band count cell

\ Fields of each evaluate frame: the package scope and using depth (PKGSNAP-USE)
\ as its text began, which a throw puts back.
0  constant PKGSNAP-CUR
8  constant PKGSNAP-PUB
16 constant PKGSNAP-PRI
24 constant PKGSNAP-PARENT
32 constant PKGSNAP-REC
40 constant PKGSNAP-USE
package PKGSNAP
public
\ FLOOR saves the open package's using floor (USE-PKG-SAVE-CELL): a REPL line may
\ close an earlier line's package and open another, which rewrites it.
48 constant FLOOR
;package


\ A fixed stack of public wordlist IDs plus a depth cell, the one count of live
\ usings: engine code owns
\ it (`using` pushes, `;using` pops, `package`/`;package` and the eval-frame
\ and REPL boundaries save and restore it), and the checker reads the same cell
\ through `data-base USE-DEPTH-CELL +`, so its package-name mirror
\ has no counter of its own to drift. The depth is 0 at rest:
\ usings are file-local and closed before seal.
16 constant USE-MAX                       \ concurrent `using` capacity (E-code on overflow)
\ A published engine ABI, not derived from the pending table's capacity.
$9C08 constant USE-BAND-OFF
USE-BAND-OFF          constant USE-DEPTH-CELL      \ live using depth (u64)
USE-BAND-OFF 8 +      constant USE-PKG-SAVE-CELL   \ depth saved at `package` open (`;package` restores)
USE-BAND-OFF 24 +     constant USE-WIDS-OFF        \ public-wid array base (USE-MAX u64 cells)
USE-WIDS-OFF USE-MAX cells + constant USE-BAND-END

\ Two tables of the code region's recorded sites: calls into __text and the
\ first instruction of each address literal. Both live below DATA-START and are
\ keyed by region offset.
package RELOC
public

\ Call-site map: one bit per four-byte word of the code region, marking every call
\ whose callee is in the engine's loaded __text. A site is recorded when the call
\ is published, because region bytes cannot be decoded soundly: a compiled word
\ may carry inline non-instruction data.
REGION 32 / constant CALLMAP-BYTES
USE-BAND-END constant CALLMAP-OFF
CALLMAP-OFF CALLMAP-BYTES + constant CALLMAP-END

\ Address-literal map: the call map's shape, marking the first word of every
\ address carrier. A CODE carrier has four move-wide halves, shared DATA normally
\ three; CODE includes the entry a `[: ... ;]` pushes and the target a `[']`
\ pushes. Recorded where the compiler decides a literal is an address.
\ Nothing recognises a chain from region bytes or from its value: a word may
\ hold inline data and an integer any value.
REGION 32 / constant ADDRMAP-BYTES
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
\ +MOVABS-IMM-OFF of a literal.
\
\ Both views are declared on every target.
\
\ The rows are strictly ascending by offset, one per offset. On x86-64
\ `callmap-set` and `addrmap-set` insert a row at its offset's place, so a
\ caller records sites in any order. `code-publish` and `reloc-maps-clear` remove
\ the rows of a span. Kinds start at 1, so a zeroed row is no
\ kind.
\
\ Unlike a bitmap, the band can fill. SITE-CAP rows fit; a writer refuses one
\ more with EXIT-STATUS:SITE.
CALLMAP-OFF constant SITE-N-CELL                \ live row count (u64)
SITE-N-CELL 8 + constant SITE-ROWS-OFF
4 constant SITE-KIND-OFF
SITE-KIND-OFF 1 + constant SITE-ROW-BYTES
ADDRMAP-END SITE-ROWS-OFF - SITE-ROW-BYTES / constant SITE-CAP
1 constant SITE-CALL
2 constant SITE-ADDR

\ Address-cell table: the DATA offset and kind of every cell declared
\ to hold an address. XT cells hold code-region addresses, DATA-pointer cells
\ addresses in the DATA heap. Membership is recorded where
\ the cell's kind is decided, never inferred from its contents (an integer can
\ hold any value).
\ A scratch pointer cell is a DATA-pointer row that also carries bit 62: the
\ link writes it as null and never reads it, so a reader that knows only bit 63
\ treats it as a DATA pointer, never as an xt.
\ The engine appends an identical declaration only once and refuses one cell
\ declared with two kinds. ADDRESS-CELLS' header and inline boot rows use this
\ band, then grow into mmap.
65536 constant XTCELL-CAP
$8000000000000000 constant XTCELL-DATA-TAG
$4000000000000000 constant XTCELL-SCRATCH-TAG
$3FFFFFFFFFFFFFFF constant XTCELL-OFF-MASK
ADDRMAP-END constant XTCELL-N-CELL        \ live count of used rows
XTCELL-N-CELL 8 + constant XTCELL-ROWS-OFF
XTCELL-ROWS-OFF XTCELL-CAP cells + constant XTCELL-END

\ The largest offset from DATA's base, or the open image's, at which a declared
\ cell lies whole; a cell after byte-sized data from `allot` may be unaligned.
DATA-SIZE 8 - constant XTCELL-OFF-MAX

;package

\ The application entry is outside compiler scratch storage. Zero preserves
\ ordinary hb CLI routing; a saved application calls this entry before input.
package APP-ENTRY
private EXPORT BOOT:data-base
public
$43A0 constant XT-CELL

: XT! ( [ -- renders parses creates nominal roots ] -- )
   data-base XT-CELL + xt! ;
;package

\ The checker overlay's replay scope: what an open overlay saved and restores
\ (HW, the highest NDICT a replay record reached); all zero when none is open.
package REPLAY-SCOPE
public
CB-END constant LATCH                               \ 0: no overlay open
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
USE-PKG-SAVE CELL + constant USE-WIDS
USE-WIDS USE-MAX cells + constant END
\ ENCLOSING: the enclosing package's four cells (ENCLOSING below), in its
\ order, saved and restored with the scope. It takes the cells after
\ ENCLOSING, which replay-close zeroes too.
$4310 constant ENCLOSING
\ PEND: PEND-CELL as replay-open found it, 0 for none; PEND-REC: a copy of
\ that record's DREC bytes, which end at APP-ENTRY:XT-CELL.
$4368 constant PEND
PEND CELL + constant PEND-REC
;package

\ While a part is open, the package it is a part of: that package's public
\ wid, private wid, parent wid and row, as PKG-PUB-CELL..PKG-REC-CELL held
\ them before the part opened; all four are 0 when no part is open. Only the
\ engine's own code and package-scope! write them. $42E8, the cell before them,
\ is free.
package ENCLOSING
public
$42F0 constant PUB
PUB CELL + constant PRI
PRI CELL + constant PARENT
PARENT CELL + constant REC
REC CELL + constant END
;package

\ Nonzero while the open package was opened outside the buffer being read, so no
\ part opens; buffers, `package` and CHECKER-OVERLAY write it.
$4DD0 constant PKG-OUTSIDE-CELL

\ The section in force: 0 host, 1 target; `host` and `target` write it.
$4DD8 constant SECTION-CELL

RELOC:XTCELL-END constant PD-TABLE-OFF
PD-TABLE-OFF PD-SLOTS-REL + PD-CAP PD-SLOT * + constant PD-TABLE-END

\ Rows (text offset, source) for the runs EMIT-BCAP appends, ascending: a run
\ first drops the rows at or past BODYLEN. The source is the input-buffer offset
\ of the bytes a run copies, NONE for a run the buffer read lacks, or BUILT of
\ the offset of the token that built the run.
\ The rows live in the body text block after its text: row i at
\ BODY-TEXT:BASE-CELL's base + cap + i * ROW, the count at BODY-TEXT:ROWS-CELL.
package BODY-SOURCE
public
16 constant ROW                     \ bytes per row
-1 constant NONE
\ Its own inverse: BUILT of a built row's source gives the offset back.
: BUILT ( n -- n ) -2 swap - ;
: BUILT? ( n -- bool ) NONE < ;
;package
\ DATA-START: the user DP heap's floor (DP-CHECK); all below is the engine's.
PD-TABLE-END constant DATA-START
\ The bytes below a build's image where a routed header store marks its cell:
\ DATA-START and the cell every such store sets.
DATA-START 8 + $FFFF + $10000 / $10000 * constant ROUTED-BYTES

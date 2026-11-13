\ layout.f - shared native image, dictionary, and snapshot layout constants.
\ A start-up row loaded before require exists, so it requires nothing.

20 constant XREG-RBASE
26 constant DBASE
27 constant NDICT
28 constant CP

\ The general registers the running engine occupies.
\ A compiled routine that writes one of these destroys the engine.
\ A register claimed here is refused by every GPR allocation path.
\ Both targets' registers are declared on every target: this file cannot branch
\ at top level and an ARM64 engine emits x86-64 code.
package ENGINE-GPR

$4C constant UNKNOWN-TARGET-RC

public

19 constant A64-DSTACK

1 A64-DSTACK lshift
1 XREG-RBASE lshift or
1 DBASE lshift or
1 NDICT lshift or
1 CP lshift or
constant A64-MASK

\ X64-DSTACK is A64-DSTACK's twin; each of the four after rbx is the twin of
\ the ARM64 constant at the head of this file named beside it; rbx has none.
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

;package

\ Fixed budget for dictionary and code, including the retained build host and
\ its replacement core.
$2300000 constant REGION
\ Canonical code-region hint past __text. The engine clamps it after the loaded
\ image and checks the resulting mapping against BL-REACH at boot.
$1000000 constant REGION-OFF
\ BL-REACH: AArch64 BL imm26 reach (+/-128 MiB). The boot dies BL-RANGE-RC when
\ the mapped region falls outside it from __text; direct-BL emission relies on it.
$8000000 constant BL-REACH
81 constant BL-RANGE-RC
$48425350414E5321 constant SNAP-MAGIC

\ The trailer ends the authenticated text extent: its base is (the header's
\ text-size field + IMAGE-TEXT-TRAILER-ADJ) - SNAP-TRL-BYTES; the codesign blob
\ and page padding follow outside the extent. Every trailer reader and writer
\ derives size and offsets from here.
48 constant SNAP-TRL-BYTES        \ magic, heap form, ndict, region len, data len, version
16 constant SNAP-TRL-NDICT
24 constant SNAP-TRL-REGLEN
32 constant SNAP-TRL-DATALEN
40 constant SNAP-TRL-VERSION      \ SNAPSHOT-FORMAT:VERSION of the writing engine
\ The pre-version trailer. Only the loader may name it, and only to recognise a
\ legacy image and fail it closed rc 80 instead of misreading its fields.
40 constant SNAP-TRL-LEGACY-BYTES

\ DICT-SIZE = CFSTK-OFF (= DICT-CAP * DREC record slots) + $1000 control-flow
\ stack; the code area follows at DBASE+DICT-SIZE inside the REGION.
\ Keep DICT-CAP/CFSTK-OFF/DICT-SIZE/HIDX-SLOTS/HIDX-BYTES in step. Maki's STEP
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
\ `variable` word a DATA address a snapshot must relocate, so folding the second
\ as the first bakes an address a restore cannot move. CAST: uses the remaining
\ kind for a declared identity retype: its body stays callable, but a compiled
\ mention only renames the checked value.
\ A stamped record's body is the definer's own.
\ Band: bits 50-51 of the record flags cell [16] (map at DNAME-MIN-IN-MASK).
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
\ DNAME-MIN-IN (bits 52-59): certified minimum input arity in cells, 0 when unmarked: the
\ interpret loop refuses a call the stack cannot feed; a compiled call carries no guard.
$0FF0000000000000 constant DNAME-MIN-IN-MASK
$1000000000000000 constant DNAME-IMM
$2000000000000000 constant DNAME-EXT
\ DNAME-WIDE (bit 62): the word's recorded effect carries a wider-than-cell
\ value in some row, so running it at interpret level would leave a multi-cell
\ bundle on the untyped interpret stack. Compiled calls inside checked
\ definitions are unaffected.
$4000000000000000 constant DNAME-WIDE
\ DNAME-INT (bit 63): a kernel primitive or engine helper no source may run.
\ Compile-mode references (unchecked code, TRUSTED:
\ bodies) are declared trusted boundaries and unaffected.
$8000000000000000 constant DNAME-INT
\ DNAME-OWNED (bit 49): an internal primitive its owner's rows type. It opens
\ nothing DNAME-INT closes: the prompt, `'`, search-wl and `[']` still refuse
\ the record. With it a compiled call is the checker's decision through the
\ rows: the owner's checked callers compile, and the global trusted-only row
\ refuses a checked caller elsewhere.
\ DNAME-INT without it keeps the compile refusal outside a TRUSTED: body.
$0002000000000000 constant DNAME-OWNED
\ DICT-CAP: dictionary record slots.
131072 constant DICT-CAP
$600000 constant CFSTK-OFF
32 constant CF-REC
\ Control-flow stack region [CFSTK-OFF, DICT-SIZE): cell 0 is the depth counter,
\ then CF-REC-byte records. CFSTK-DEPTH-MAX always exceeds the checker's CFS
\ cap 31.
DICT-SIZE CFSTK-OFF - 1 cells - CF-REC / constant CFSTK-REGION-CAP
256 constant CFSTK-SANE-MAX
CFSTK-REGION-CAP CFSTK-SANE-MAX min constant CFSTK-DEPTH-MAX

\ Profiler counter band: one 64-bit sample counter per dictionary slot, reserved
\ at the very top of the DATA region [DATA-SIZE - PROF-CNT-BYTES, DATA-SIZE).
\ The band opens with PROF-STATE-BYTES of the profiler's own cells (sample
\ total and limit, the two buckets, the dictionary base recorded by prof-on, the
\ arena, the clock, the handlers inside and the control words' ticket lock) and
\ the counters follow.
80 constant PROF-STATE-BYTES
DICT-CAP cells PROF-STATE-BYTES + constant PROF-CNT-BYTES

\ The cold prefix, argv files and stdin share this allowance, less an EOF probe;
\ overflow exits 74 `hb: source prefix buffer full`.
$400000 constant SOURCE-ARENA-CAP
20 constant DATA

0 constant DP-CELL
8 constant HND-CELL
16 constant LOCN-CELL
$3000 constant LOCNAMES
24 constant LOC-REC
\ A LOC-REC is an 8-byte bare-name length followed by the name bytes; a bare
\ name wider than the field is refused before it is stored.
LOC-REC 8 - constant LOC-NAME-CAP
64 constant LOC-RECS          \ LOCNAMES holds this many LOC-RECs ($600 bytes)
\ The body capture scopes the locals it records by block:
\ CAPDEPTH-CELL is the block depth and TOKDEPTH-CELL the depth as the current
\ token began, restored with TOKLOCN-CELL's mark.
$2DB8 constant CAPDEPTH-CELL
$2DC0 constant TOKDEPTH-CELL
\ SEAL-NDICT-CELL holds the seal-time ndict watermark.
\ HIDE-DEFS-FROM/FORGET-DEFS-FROM refuse a post-seal FORGET below it.
$A8 constant SEAL-NDICT-CELL

67 constant UNCAUGHT-RC                 \ deterministic exit status for an uncaught top-level throw (BTHROW):
                                        \ the kernel masks a raw code to 8 bits, so a multiple
                                        \ of 256 would exit 0.
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
$A0 constant DEFER-XT-CELL
\ DEFER-MAGIC: the first cell of the meta trailer `defer` writes just past a
\ deferred word's code, followed by the address of its dispatch cell. A reader
\ holding a record recognises a defer's trailer by this word, never by the
\ shape of what follows the code (an ordinary integer can hold any value).
$4842444546455201 constant DEFER-MAGIC
\ CMBK-CELL and CMFRD-CELL: zeroed by both kernels' compile reset; nothing
\ reads them.
$B0 constant CMBK-CELL
$C8 constant CMFRD-CELL
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
\ BPTRAMP-CELL: the code address of the persistent-breakpoint trampolines,
\ BPTRAMP-BYTES of BRK words, reserved in the code band.
$36C0 constant BPTRAMP-CELL
64 constant BPTRAMP-BYTES
$36D0 constant BPTAB-OFF
$37E8 constant BPWBASE-CELL
$37F0 constant BPWN-CELL
\ EVAL-TOP-CELL: the innermost evaluate frame, zero at rest. EVAL-PREV links
\ each to the frame it nests in.
$43C0 constant EVAL-TOP-CELL
STACK-ABI:EVAL-BYTES constant EVAL-FRAME-SIZE
$40 constant EVAL-PREV
$48 constant EVAL-PKG
\ EVAL-INB: the outer evaluate's input-buffer start, which a nested evaluate restores. The frame
\ ends at STACK-ABI:EVAL-BYTES ($170), a multiple of 16; $128 is held for WIDN and $130 is free.
$90 constant EVAL-INB
package EVAL-FRAME
public
\ USE-WIDS: the includer's USE-MAX used-public wids, which a throw puts back
\ with the depth and the floor. A buffer that closes its includer's package and
\ then opens a using writes that using into a slot the includer keeps.
$A0 constant USE-WIDS
\ PEND: PEND-CELL as the buffer began. A definition the buffer opened must
\ close in it, so its end refuses one still open; one already open here belongs
\ to an outer buffer, whose immediate word's evaluate compiles into it, and a
\ throw that unwinds this buffer leaves it compiling.
$120 constant PEND
\ ROWS: the checker's record end as the buffer began, plus one, read through
\ the source owner; 0 when no owner records. A throw that unwinds a buffer
\ owning the compile cuts the records back to it as it rolls the dictionary
\ back, so no definition the rollback drops keeps a record or a pending window.
$138 constant ROWS
\ ENCLOSING: the enclosing package's four cells (ENCLOSING below), in its order,
\ which a throw puts back with the open package's.
$140 constant ENCLOSING
\ PKG-OUTSIDE: PKG-OUTSIDE-CELL as the buffer began, which a throw puts back.
\ A clean exit does too unless a `package` ran; one left open is the includer's.
$160 constant PKG-OUTSIDE
\ SECTION: SECTION-CELL as the buffer began, which its end puts back.
$98 constant SECTION
;package

\ PATH-CELL / PATHLEN-CELL: the innermost open include frame's path, 0/0 with
\ no include frame open: the root source stream's own text (stdin, tty or
\ piped, or the `--load` and `--build` streams) and the boot prefix. The
\ program file of a plain `hb <file>` has a frame of its own. The address is
\ the frame's, not INCLUDE-PATH's, because a nested include overwrites that
\ buffer.
\ INB-CELL: the start of the buffer INP walks. It is not beside INP/INE: $36C8
\ is INDEX-CELL.
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
\ CURSOR: the byte address the scanner reads next.
INP-CELL RESERVED-PTR-U8-CELL CURSOR
\ SOURCE-CELL: the address of the process's loader record, its top frame,
\ current root and scope count. include.f stores it with `ptr-cell!` as it
\ loads, so a cross-build's copy of include.f routes the store to the image it
\ writes while its code reads the running hb's record.
$2C48 constant SOURCE-CELL
\ READS-CELL: the process's compile-reads record (src/core/compile-reads.f), as
\ SOURCE-CELL holds its own: a cross-build's copy of that file uses hb's record.
$2C50 constant READS-CELL
;package

\ PKGRESYNC-CELL: LEVALREC arms it and EM-PKG-RESYNC drains it before the
\ throw is delivered, making the checker re-read the restored scope.
$27C0 constant PKGRESYNC-CELL
$27B0 constant DOESB-CELL
$27B8 constant TRUSTED-CELL
$37D0 constant EVALD-CELL
$37D8 constant EVALERR-CELL
$3C88 constant TASK-TCB-CELL
\ HALT-UNWIND-CELL: nonzero while a TASK:HALT unwinds the task that observed it,
\ else 0, which a fresh task region and every non-task region hold. Only that
\ task's own thread writes it: HALT-UNWIND, which PAUSE calls, sets it before it
\ throws, and the catch family reads it on its throw-resume path. An inner
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
\ PROT: the x86-64 kernel's write-window cells.
package PROT
public
$27D0 constant WINDOW
$27D8 constant WLO
$27E0 constant RLO
$47C0 constant RHI
$47C8 constant CF
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

\ AOT-CELLS holds every AOT cell this file declares: the engine bakes this file,
\ so a cell declared in AOT-SPAN or AOT-WINDOW would make aot-decl.f,
\ habu1.f and habu2.f reopen a baked package in a native build.
\ AOT-CELLS:SPAN-TABLE-CELL / SPAN-N-CELL / SPAN-BASE-CELL: the baked CODE-SPAN
\ table, its row count and the address the blob was copied to; only the x86-64
\ linker writes them. A word with no dictionary record is accounted for only by
\ its row. A row is a blob offset; the base makes it an address. Zero in all
\ three means no stripped spans.
\ They sit in the $600..$800 header hole below BODY-TEXT:BASE-CELL, above
\ GENIO-ABI's band. The run above BOOT:HEAP-START-CELL is not free:
\ stack-abi.f holds $47E8..$4810, then the transaction and USER-BAND.
package AOT-CELLS
public
$660 constant SPAN-TABLE-CELL
$668 constant SPAN-N-CELL
$670 constant SPAN-BASE-CELL
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
\ The cells follow AOT-SPAN's in the $600..$800 hole, below $7FF8 (the boot
\ stores them as `DATA <off> STR`) and below DATA-START.
package SIGNAL-ABI
public
$678 constant STUB-CELL
$680 constant FD-PTR-CELL
$688 constant FD-CELL
;package


\ GENIO-ABI: the routing cells of the generic I/O layer that engine code reads;
\ everything else about a device is library storage.
\ OUT-CELL and IN-CELL hold a device index, never an address: 0 is the built-in
\ terminal, 1..DEVICES a row of the write table. They are per task, and nothing
\ canonicalises a code address in a task region, so an execution token there
\ would survive a snapshot or capture pointing into the builder's region.
\ The write table holds execution tokens in the engine header, where SNAP-RELOC
\ canonicalises declared cells. TASK-REGION-INIT copies the rows too, sound
\ because a task region is never snapshotted or captured. DEVICES is small so
\ the table costs no allocation on a microcontroller.
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


\ EVALREC-CELL: runtime address of the eval-frame throw-unwind entry, set at
\ startup so the leaf throw primitive can branch to it. $3CA0..$3CB8 are engine
\ cells no compiled source writes, between the FFI buffer block ($3A00..$3C88),
\ the task TCB cells ($3C88..$3CA0) and the protected-WID registry
\ ($3CB8..$40C0).
$3CA0 constant EVALREC-CELL
\ The protected-WID registry records the WIDs of sealed system / generated
\ constructor packages created in the friend window; membership gates the
\ sealed-WID guards.
\ Bit w of the $400-byte band at PROT-BITS-OFF is set exactly when wordlist w is
\ protected, so the set holds any subset of the WIDs the engine indexes.
\ PROT-WID-MAX is a WID bound: $400 bytes index 8192 WIDs, and prot-wid-add
\ names the bound when handed a WID at or above it. A wider bitmap must first
\ move CB-XDS at $41C8, and APP-ENTRY:XT-CELL, the AOT capture window and the
\ evaluator-pointer band from $43A0; growing it also moves UNCGH-CELL.
\ No compiled source writes the band, the DP heap is bounded >= DATA-START, and
\ snapshots save it.
$3CB8 constant PROT-REG-TAG-CELL        \ bitmap-shape tag; capture requires this exact runtime shape
$50574249544D4150 constant PROT-REG-TAG \ "PWBITMAP": written by every path that publishes the band
$3CC0 constant PROT-BITS-OFF
8192 constant PROT-WID-MAX
PROT-WID-MAX 8 / constant PROT-BITS-BYTES
PROT-BITS-OFF PROT-BITS-BYTES + constant PROT-BITS-END
PROT-REG-TAG-CELL constant PROT-REG-OFF
PROT-BITS-END 1 cells + PROT-REG-OFF - constant PROT-REG-LEN
\ UNCGH-CELL: runtime address of the uncaught-throw reporter, written by the
\ link: BTHROW, a leaf, branches through it.
$40C0 constant UNCGH-CELL
\ The callback band: C calling back into checked code.
\ Eight cells every region carries at the same offset, in the run between the
\ FFI length buffers ($40C8..$41C8) and APP-ENTRY:XT-CELL ($43A0). Every cell is
\ correct at zero, which is what a fresh task region, a fresh DATA mapping and a
\ stripped image's anonymous DATA all hold.
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
16 constant CB-POOL                \ boot stubs retained for seed compatibility
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
\ Compact AOT dict record: five u32 words (blob-off/public-WID, code-len/private-WID,
\ name-off, flags|min-in, wid). The name-off word is a full u32 so the deduped name
\ pool can outgrow 64 KiB with the captured window.
20 constant AOT-CREC-ROW
\ The deduplicated pool admits at most DICT-CAP entries, each containing a
\ one-byte length and at most 255 name bytes. Storage grows to the used size.
DICT-CAP 256 * constant AOT-NAMES-CAP
\ Dict-name hash index: slots are a power of 2 (probes with the
\ HIDX-SLOTS 1 - mask) and 2x DICT-CAP, so the load factor stays <= 50% and
\ DICT-CAP stays a power of two; bytes = slots * 4 (u32 entries).
DICT-CAP 2 * constant HIDX-SLOTS
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
$37F8 constant SNAP-CELL
\ ENGINE-MAIN:XT-CELL is the DATA cell an engine's boot calls its MAIN through.
\ It is a separate cell from APP-ENTRY:XT-CELL because that one also switches
\ the argv and stdin conventions to a saved application's. It sits in the
\ single-cell run above SNAP-CELL ($37F8); a fresh DATA mapping reads it zero
\ until set.
package ENGINE-MAIN
public
$3808 constant XT-CELL
\ The captured x86 reporter is written here while the ARM build host is
\ sealed. The linker publishes its translated XT in protected UNCGH-CELL.
\ $3858 is outside PROT-REG.
$3858 constant REPORT-CELL
\ The x86-64 Habu loop's source arena: a mapping this process made, which every
\ boot reads zero until MAIN maps its stream.
$3868 constant ARENA-CELL
;package

\ PROVIDED-XT: the dispatch cell of each src/habu/prims.f row a captured
\ runtime provides, stored with `xt!` and restored on every x86-64 boot as a
\ fixed address row. A seeded x86-64 row is a stub that jumps through it, and
\ x86-64's `evaluate-closed` calls EVALUATE-CELL's word; arm64's rows run the
\ cells' words.
package PROVIDED-XT
public
$3848 constant EVALUATE-CELL
\ A seeded x86-64 `evaluate-range` jumps through its own cell the same way.
$3860 constant EVALUATE-RANGE-CELL
\ `define-word`, `body-append`, `postponed` and `literal` run the running
\ engine's interpreter words these cells hold.
$3878 constant DEFINE-CELL
$3880 constant APPEND-CELL
$3888 constant POSTPONE-CELL
$3898 constant LITERAL-CELL
\ A declaration makes each child word through CHILD-CELL's CHILD-RUN, with a
\ CHILD-ABI shape code.
$38B0 constant CHILD-CELL
\ A storage definer the checker does not compile reads its declaration line
\ through the CHECK:STORAGE-LINE this cell holds.
$38A8 constant STORAGE-LINE-CELL
\ A definer's type and effect values are schema nodes the running engine's
\ CHECK:TYPE-OP, which this cell holds, makes from a TYPE-OP-ABI code.
$38A0 constant TYPE-OP-CELL
;package

\ A child word's shape: from it the engine makes the child's body and the effect
\ it registers. An OFF child adds its parameter, a byte offset, to `ptr a`,
\ answering `ptr a` for CELL-OFF, `ptr ptr b` for PTR-OFF, `ptr u8` for BYTE-OFF.
\ A FIELD child's parameter is a committed field id, an INDEX child's a family
\ id, a VALUE child's the number it answers. STORED to SLOTS are storage
\ children: the parameter is 0, and the effects of STORED, BUFFER, DYNAMIC, SLOT
\ and SLOTS answer `ptr t` for the type the declaration spells.
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
;package

\ A type operation on operands a and b: INT `n`, FAMILY family a on fresh
\ parameters, PTR a pointer to type a, EFFECT an empty effect, IN and OUT
\ effect a with type b as its new top input or output.
package TYPE-OP-ABI
public
0 constant INT
1 constant FAMILY
2 constant PTR
3 constant EFFECT
4 constant IN
5 constant OUT
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


\ A latch whose compiler cell (COMP-CELL) is zero dies with
\ ENGINE-ERROR:AOT-SEED.
package NCOMP-DISPATCH
public

\ The callback operations are NDEFINITION's, which the COMP-CELL compiler
\ uses. A fresh source
\ compiler may coexist with a retained active compiler in a build window; both
\ operations must follow the active compiler rather than dictionary names.
$3810 constant CALLBACK-STUB-CELL
$3818 constant CALLBACK-ENTRY-CELL
\ The optional three-cell origin record follows at $3820..$3837, read through
\ the three-cell native-origin-cell getter.
\ BODY-HOOK-CELL, when nonzero, is an xt `( -- ptr u8 bool )` the `:` and
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
\ One owner record keeps every engine declaration registrar paired with the
\ active compiler, including while the source dictionary/checker is replaced.
\ The record is DATA; its fields are execution tokens of existing private
\ checker operations.
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
CHECKER-OWNER-ABI:EXPORT-OFF constant DECL-EXPORT-OFF
CHECKER-OWNER-ABI:TRUSTED-TICK-OFF constant DECL-TRUSTED-TICK-OFF
CHECKER-OWNER-ABI:PKG-RESYNC-OFF constant DECL-PKG-RESYNC-OFF
\ The rest are called by the native compiler, the engine and the source verifier.
CHECKER-OWNER-ABI:DOES-CHECK-OFF constant DECL-DOES-CHECK-OFF
CHECKER-OWNER-ABI:SIG-COUNTED-OFF constant DECL-SIG-COUNTED-OFF
CHECKER-OWNER-ABI:UNJUDGED-CLOSED-OFF constant DECL-UNJUDGED-CLOSED-OFF
CHECKER-OWNER-ABI:PIN-SPAN-OFF constant DECL-PIN-SPAN-OFF
CHECKER-OWNER-ABI:PIN-TOKEN-OFF constant DECL-PIN-TOKEN-OFF
CHECKER-OWNER-ABI:CALL-CELLS-OFF constant DECL-CALL-CELLS-OFF
CHECKER-OWNER-ABI:CALL-BINDING-OFF constant DECL-CALL-BINDING-OFF
CHECKER-OWNER-ABI:UNJUDGED-BINDING-OFF constant DECL-UNJUDGED-BINDING-OFF
CHECKER-OWNER-ABI:CALL-MATCH-OFF constant DECL-CALL-MATCH-OFF
CHECKER-OWNER-ABI:INIT-LAYOUT-OFF constant DECL-INIT-LAYOUT-OFF
CHECKER-OWNER-ABI:FIELD-SPAN-OFF constant DECL-FIELD-SPAN-OFF
CHECKER-OWNER-ABI:REC-MIN-IN-OFF constant DECL-REC-MIN-IN-OFF
CHECKER-OWNER-ABI:REC-WIDE-PUBLISH-OFF constant DECL-REC-WIDE-PUBLISH-OFF
\ The scan whose verdict nobody enforces, with the render suppressed for it.
\ A TRUSTED: body is scanned only to fill the tape, so the suppression has to
\ happen in the instance that renders; the counter itself is unreachable from a
\ baked compiler, which is the whole reason this is an operation and not a cell.
CHECKER-OWNER-ABI:CHECK-UNJUDGED-OFF constant DECL-CHECK-UNJUDGED-OFF
\ A refused definition's cut back to the store end it began at, and that end.
CHECKER-OWNER-ABI:RETRACT-ROWS-OFF constant DECL-RETRACT-ROWS-OFF
CHECKER-OWNER-ABI:ROWS-END-OFF constant DECL-ROWS-END-OFF
\ The compile window: opened with the code a callback's checker write is refused
\ with once the definition's rows are recorded, shut with 0 before its own.
CHECKER-OWNER-ABI:WRITE-WINDOW-OFF constant DECL-WRITE-WINDOW-OFF
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
CHECKER-OWNER-ABI:VERIFY-DECL-DISARM-OFF constant DECL-VERIFY-DECL-DISARM-OFF
CHECKER-OWNER-ABI:VERIFY-USES-OFF constant DECL-VERIFY-USES-OFF
CHECKER-OWNER-ABI:RESET-REPORT-OFF constant DECL-RESET-REPORT-OFF
CHECKER-OWNER-ABI:JSON-REPORTED-OFF constant DECL-JSON-REPORTED-OFF
CHECKER-OWNER-ABI:DOES-UNJUDGED-OFF constant DECL-DOES-UNJUDGED-OFF
CHECKER-OWNER-ABI:NATIVE-DOES-REPORT-OFF constant DECL-NATIVE-DOES-REPORT-OFF
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

\ The items of a definition `define-word` opened: rows of ROW cells
\ {kind, a, b, off, len} in a block the interpreter grows, off and len the
\ item's spelling in the body text. The checker walks them in place of the
\ text while RAW-CELL is 0; RAW-CELL is nonzero once text no item spans joined
\ the capture. All four cells are 0 outside such a definition, so no image
\ holds the block's address. They take $4340..$4360 of the run below
\ DECLARED-LOG:ADDR-CELL.
package BODY-ITEMS
public
$4340 constant BASE-CELL     \ the block's address
$4348 constant CAP-CELL      \ the capacity in rows
$4350 constant COUNT-CELL    \ the rows held
$4358 constant RAW-CELL
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
\ value.
package BODY-ITEM-KIND
public
1 constant NAME
2 constant SIG
3 constant CALL
4 constant INT
;package
\ The latest `,` run of a created body.
CREATED-BODY-ABI:COMMA-RUN-CELL constant COMMA-RUN-CELL
CREATED-BODY-ABI:COMMA-END-CELL constant COMMA-END-CELL
CREATED-BODY-ABI:COMMA-BASE-CELL constant COMMA-BASE-CELL
$568 constant RSP-CELL
$570 constant EXITH-CELL
$578 constant LVD-CELL
$560 constant LASTC-CELL
$230 constant CREATEP-CELL
$238 constant QPATCH-CELL
\ FRAME-CELL: the open body's entry slot, where `:` emitted the link-register
\ save. Zero means no frame. QFRAME-CELL scopes it across a quotation.
$7D0 constant FRAME-CELL
$7D8 constant QFRAME-CELL
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
\ state. It persists through snapshots like HOOK-CELL; `0 set-check` parks it in
\ the SUSP-HOOK band below.
$27E8 constant COMPILE-PREFLIGHT-CELL
\ TOP-HOOK-CELL: top-row token hook xt; 0 = no hook. Installed only through the
\ fail-closed `set-top-check` prim. Snapshot-persistent like HOOK-CELL.
\ The hook's effect is ( ptr u8 n n n -- ): token addr, token len, class
\ (TOP-EV-*), LFIND flags.
$27F0 constant TOP-HOOK-CELL
COMPILE-PREFLIGHT-CELL constant ENGINE-HOOK-OFF
2 cells constant ENGINE-HOOK-LEN
\ SUSP-HOOK-CELL, SUSP-PREFLIGHT-CELL: the (hook, preflight) pair `0 set-check`
\ suspends until `set-check` of the same hook resumes it. Both start zero.
$2D80 constant SUSP-HOOK-CELL
$2D88 constant SUSP-PREFLIGHT-CELL
SUSP-HOOK-CELL constant SUSP-HOOK-OFF
2 cells constant SUSP-HOOK-LEN
\ REFUSAL-ABI:CODE-CELL: the code of the last refusal whose diagnostic the
\ checker rendered before throwing it, 0 until one is. The uncaught top-level
\ throw reporter exits RC-REJECT for a code equal to it, as every refusal exits,
\ and UNCAUGHT-RC for any other. A catch that takes the throw leaves it set. A
\ bare throw of the same code after a refusal of it was caught therefore exits
\ RC-REJECT too. Not sealed, because the checker writes it after the seal.
package REFUSAL-ABI
public
$27F8 constant CODE-CELL
;package
\ EXIT-HOOK-CELL: the process-exit vector, an xt or 0 for no hook. The engine
\ calls it once with the exit code in x0, the cell cleared FIRST, immediately
\ before the exit_group of a DELIBERATE program exit: `die`, the uncaught
\ top-level throw and the exit(0) after the entry returns. The
\ engine's fail-closed exits - every ENGINE-ERROR:* rc and the 70/74/75/76
\ band - do NOT call it: a broken engine must not run program code. Clearing
\ before the call is what stops a hook that dies or throws from re-entering
\ itself; the exit path it lands in finds the vector empty. The catch handler
\ chain (HND-CELL) is emptied with it, so a throw from the hook is uncaught and
\ ends the process with the hook's code: `die` runs inside the program's
\ catches, and a hook's throw caught there would return from `die`. A stripped
\ image's exit follows its root's return, when the chain is already empty. The
\ hook's effect is ( -- ) and it runs on the data stack as it stands, because
\ `die` may be running on a task thread whose stack is its own. A new process
\ has no hook, and the library that wants one arms it at runtime.
\ $2818 follows SRCLOC's three cells in the $2800..$3000 header band,
\ below $7FF8 so the engine names it with a `DATA <off> LDR/STR` 12-bit scaled
\ immediate, and below DATA-START so no compiled source reaches it by allot.
$2818 constant EXIT-HOOK-CELL
\ TOKBODY-CELL .. TOKVSP-CELL and TOKDEPTH-CELL: the token-start mark, five
\ cells. Before every token the interpret loop stores BODYLEN-CELL, LOCN-CELL
\ and its block depth CAPDEPTH-CELL, CP (as an offset
\ from the region base, like every code position a DATA cell keeps, so no
\ image carries a live address) and the virtual stack's VSP-CELL here.
\ Two other writers: arm64
\ C-CLEAR-LOCALS zeroes TOKLOCN-CELL and TOKDEPTH-CELL with the locals at a
\ definition's end or abort, and a refused `is` target lowers TOKBODY-CELL to
\ its opener.
\ A throw that unwinds an evaluate frame begun inside an open definition leaves
\ that definition compiling and puts all five
\ back: the text the checker certifies holds no token whose compile the throw
\ abandoned, a `{:` group refused part way declares none of its names, and the
\ code and the spill a refused token emitted before its check are gone. A spill
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
\ address like TKA-CELL, zeroed at snapshot: no stripped-image claim.
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
\ for none. The literal store opens a segment at `here` inside whatever is
\ running and raises the floor to the segment's end, so the segment and every
\ address it answered outlive a failed evaluation and a rolled-back generated
\ declaration: each puts DP at the higher of its saved value and the floor. An
\ offset rather than an address, so nothing relocates it: a snapshot carries it
\ with the rest of DATA, and a native or stripped image starts at 0, which no
\ rewind reaches, because every DP it saves is above the `here` it boots with.
$2CE8 constant DATA-FLOOR-CELL
\ PRIM-TABLE-CELL: the kernel's primitive table, count in the cell before, as
\ an offset from RBASE-CELL's address; 0 in a stripped program.
$2CF0 constant PRIM-TABLE-CELL
\ DEF-OCC:PTR-CELL holds this process's dictionary occurrence allocation, not an
\ image address: last-issued, then DICT-CAP u64 slots. A snapshot DATA copy
\ clears it and the receiving process restores its own.
package DEF-OCC
public
$2D08 constant PTR-CELL
DICT-CAP 1+ cells constant STATE-BYTES
;package
\ TEXT-SIZE-CELL: the running image's text content size in bytes from the base
\ RBASE-CELL holds - the header's text-size field less IMAGE-TEXT-CONTENT-ADJ.
\ The words that bound the engine's text read it rather than the layout
\ constants they were loaded with: a build window for another OS loads that OS's
\ header offsets. A size, not an address, so snapshots carry it unzeroed. Every
\ boot stores it before a word runs.
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
\ the count. A raising ndict! re-exposes the retired records below it, so the
\ does> patch clears alias kinds up to the high-water, not to NDICT. 0 reads as
\ nothing retired.
$2D60 constant NDICT-HW-CELL

package NCOMP-DISPATCH
public
$2D98 constant FIXED-SHADOW-CELL
$2DA0 constant DOES-SHADOW-CELL
;package
$2DA8 constant TASK-CHAIN-CELL
\ The design seal confines a sealed source to the vocabulary its harness
\ admitted. POLICY-NDICT-CELL is 0 while nothing is sealed, else the NDICT the
\ seal stored; a record at or above that index is the design's own.
\ POLICY-BITS-OFF is a WID-indexed bitmap shaped like PROT-BITS-OFF's: bit w
\ admits wordlist w, and every reader tests w against PROT-WID-MAX before it
\ addresses the band. Every writer of the vocabulary and of the require xt
\ refuses once the seal is set. Only the engine's token reader tests the
\ vocabulary; the native compiler never reads it. The snapshot writer zeros its DATA copy and
\ the warm loader zeros the restored fields, so a fresh process is unsealed.
$4898 constant POLICY-NDICT-CELL
$48A0 constant POLICY-BITS-OFF
$4CA0 constant POLICY-REQUIRE-CELL  \ ordinary require xt; zero unless opted in
\ CLAUSE-CELL: nonzero once the pending definition has a `;does` clause - set by
\ the `does>` keyword and by does-record, which an imported definer reaches with
\ no keyword - and cleared wherever the engine clears DOESB-CELL. DOESB-CELL
\ stays the clause's offset into the body text, which an imported definer never
\ fills.
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

\ The catch/throw handler frame.
\ A STACK-ABI:CATCH-BYTES machine-stack frame chained through HND-CELL
\ ([DATA+8]).
\ frame layout: 0 prev-HND | 8 data-sp(x19) | 16 machine-sp | 24 resume-pc
\               32 link | 40 saved-RSP | 48 saved-LOOPSP | 56 unused
\               64 saved-base | 72 saved-capacity | 80 saved-BUSY
\               88 saved-ACTIVE

STACK-ABI:PAGE-BYTES constant PROT-PAGE-MAX   \ the mapping alignment

\ The end of the run handed to the libraries that need storage a task owns.
$8000 constant USER-REGION-END

\ STRING-ABI is src/lib/string.f's SB builder, declared here because string.f is
\ baked into the engine below src/lib/ffi-abi.f, which requires it back.
\ Carved from the top of the run so USER-BAND keeps the growable end.
package STRING-ABI
public
$400 constant SB-BUF-BYTES
8 constant SB-LEN-BYTES
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
\ The four cells sit at the band base, a cell boundary, so they are cell-aligned
\ whatever the render buffer's width. The greatest exact coefficient is
\ (2^53-1)*5^1074, 767 decimal digits, so 86 nine-digit limbs suffice.
package FMT-ABI
public
4 constant SCRATCH-CELLS
86 constant SCI-LIMBS
20 constant NUM-BUF-BYTES
SCRATCH-CELLS SCI-LIMBS + cells NUM-BUF-BYTES + constant MEMBER-BYTES
MEMBER-BYTES 7 + 8 / 8 * constant BYTES
STRING-ABI:START constant END
END BYTES - constant START
START constant NUM-U-OFF
NUM-U-OFF 8 + constant IX-OFF
IX-OFF 8 + constant FR-OFF
FR-OFF 8 + constant DV-OFF
DV-OFF 8 + constant SCI-LIMBS-OFF
SCI-LIMBS-OFF SCI-LIMBS cells + constant NUM-BUF-OFF
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

\ FS-MUT-ABI is src/lib/fs-mutate.f's per-call path scratch.
package FS-MUT-ABI
public
PATH-CAP 1 + constant PATHZ2-BYTES
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

\ MEM-ABI is src/lib/memory.f's scope head: one cell naming the
\ running task's first scope chunk, 0 until its first scope; that chunk's header
\ names the innermost scope. A region is a fresh zeroed mapping, so every task
\ and every stripped image starts with no scope open.
package MEM-ABI
public
1 cells constant BYTES
FS-MUT-ABI:START constant END
END BYTES - constant START
START constant HEAD-OFF
;package

\ USER-BAND is the run of the per-task header src/lib/task.f keeps its own cells in.
package USER-BAND
public
$5000 constant START
MEM-ABI:START constant END
;package

\ A `defer NAME ( E )` in src/core/checker.f before `: TRUST` cannot register
\ itself: the lookup of `trust` and `checker-defer` fails. The Habu loop's
\ `defer` then copies the qualified name
\ and effect signature into a slot here;
\ DRAIN-PRETRUST, right after `: TRUST`, replays both registrations per slot, so
\ the table is empty by snapshot time. Overflow or an over-long name or sig dies
\ at declaration; a non-empty table at SEAL-CAPTURE dies. The table sits after
\ the private engine bands, below DATA-START, so its growth never moves the
\ using ABI or relocation maps. PD-TABLE-OFF is past the scaled-immediate range,
\ so slots use a computed base. Slot: [0]=name-len [8]=sig-len
\ [PD-NAME-OFF..)=name bytes [PD-SIG-OFF..)=sig bytes.
\ A raw store grants nothing beyond the public `trust`/`checker-defer` words,
\ and the table is empty whenever user source runs.
128 constant PD-CAP                         \ pending slots with room beyond the checker prefix
48 constant PD-NAME-CAP
64 constant PD-SIG-CAP
0  constant PD-NLEN-OFF                       \ in-slot: name length (u64)
8  constant PD-SLEN-OFF                       \ in-slot: sig length (u64)
16 constant PD-NAME-OFF                       \ in-slot: name bytes
PD-NAME-OFF PD-NAME-CAP + constant PD-SIG-OFF \ in-slot: sig bytes
PD-SIG-OFF PD-SIG-CAP + constant PD-SLOT      \ per-slot stride
8 constant PD-SLOTS-REL                       \ slots begin after the u64 band count cell

\ Fields of each evaluate frame. Clean exit restores the using depth to the
\ buffer's floor; throw recovery restores the entry depth
\ (PKGSNAP-USE), the used-public wids and the package scope.
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


\ A fixed stack of public wordlist IDs plus a depth cell, the one count of live
\ usings: engine code owns
\ it (`using` pushes, `;using` pops, `package`/`;package` and the eval-frame
\ and REPL boundaries save and restore it), and the checker reads the same cell
\ through `data-base USE-DEPTH-CELL +`, so its package-name mirror
\ has no counter of its own to drift. The depth is 0 at rest:
\ usings are file-local and closed before seal or snapshot.
16 constant USE-MAX                       \ concurrent `using` capacity (E-code on overflow)
\ A published engine ABI, not derived from the pending table's capacity.
$9C08 constant USE-BAND-OFF
USE-BAND-OFF          constant USE-DEPTH-CELL      \ live using depth (u64)
USE-BAND-OFF 8 +      constant USE-PKG-SAVE-CELL   \ depth saved at `package` open (`;package` restores)
USE-BAND-OFF 24 +     constant USE-WIDS-OFF        \ public-wid array base (USE-MAX u64 cells)
USE-WIDS-OFF USE-MAX cells + constant USE-BAND-END

\ Two tables of the code region's recorded sites, which AOT capture reads: calls
\ into __text and the first instruction of each address literal. Both live below
\ DATA-START and are keyed by region offset.
package SNAP-RELOC
public

\ These exit statuses live here beside BL-RANGE-RC, not in
\ src/core/engine-error.f: the engine emitter reads them from this file one
\ generation before a new src/core constant is reachable. That registry leaves
\ 96, 98, 99 and 103 to this file.
\
\ Exit status when address-cell storage cannot be allocated or is malformed.
96 constant XTCELL-RC
\ Exit status for a declared address cell whose offset lies outside
\ [0, XTCELL-OFF-MAX].
98 constant XTBAND-RC
\ Exit status for one address cell declared with two kinds: either kind, kept
\ alone, would relocate the cell wrongly.
99 constant XTKIND-RC

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
\ DATA sites are recorded for the AOT capture: the captured blob lands at a
\ different DP in the seeded engine, so a DATA chain needs rebasing there, while
\ fixed-VA DATA needs nothing on snapshot restore.
\ The map stores no kind: a consumer classifies a recorded value by its window's
\ DATA and code spans, which cannot overlap, and refuses a value in neither.
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
\ The rows are strictly ascending by offset, one per offset, so the reader yields
\ a span's sites in bitmap order without sorting a live band. On x86-64
\ `callmap-set` and `addrmap-set` insert a row at its offset's place, so a
\ caller records sites in any order. `code-publish` and `reloc-maps-clear` remove
\ the rows of a span. Kinds start at 1, so a zeroed row is no
\ kind.
\
\ Unlike a bitmap, the band can fill. SITE-CAP rows fit; a writer refuses one
\ more with SITE-RC, and the reader refuses with the same status a count above
\ SITE-CAP, a row at or after REGION, a row of no known kind and a row not above
\ the one before it. SITE-RC lives here for the reason XTCELL-RC does.
\
\ Snapshot relocation does not apply on x86-64: the region and DATA are mapped
\ at fixed addresses there. The rows serve publication and the AOT capture.
CALLMAP-OFF constant SITE-N-CELL                \ live row count (u64)
SITE-N-CELL 8 + constant SITE-ROWS-OFF
4 constant SITE-KIND-OFF
SITE-KIND-OFF 1 + constant SITE-ROW-BYTES
ADDRMAP-END SITE-ROWS-OFF - SITE-ROW-BYTES / constant SITE-CAP
1 constant SITE-CALL
2 constant SITE-ADDR
103 constant SITE-RC

\ Address-cell table: the DATA offset and kind of every cell declared
\ to hold an address. XT cells hold code-region addresses, DATA-pointer cells
\ addresses in the DATA heap. Only XT values need snapshot canonicalisation:
\ region code moves on restore and DATA does not. Both kinds need the
\ declaration for AOT capture, where the captured DATA window moves and a raw
\ pointer into it would escape in sparse bytes. Membership is recorded where the
\ cell's kind is decided, never inferred from its contents (an integer can hold
\ any value).
\ A scratch pointer cell is a DATA-pointer row that also carries bit 62: every
\ save writes it as null and never reads it, so a reader that knows only bit 63
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
public
$43A0 constant XT-CELL

: XT! ( [ -- ] -- )
   data-base XT-CELL + xt! ;
;package

\ AOT-ARM:OPEN writes B0-CELL, and WINDOW-CLOSE clears it. Tier 0 reads it to
\ keep a mention of a created word made before the window opened a call.
\ T0 is latched when the seed allocates its wordlists: later boot-run entries
\ can allocate more, so the sealed-wordlist gate cannot derive it from WIDN.
package AOT-CELLS
public
$43A8 constant T0-CELL           \ first wordlist id the seed allocated for the window
$43B8 constant B0-CELL           \ first address of the open window's code span
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

\ The path `A:B` built for `package B` inside package A;
\ a path of CAP bytes or more is refused.
package PACKAGE-PATH
public
$4CD0 constant OFF
$100 constant CAP
;package

\ Nonzero while the open package was opened outside the buffer being read, so no
\ part opens; buffers, `package` and CHECKER-OVERLAY write it.
$4DD0 constant PKG-OUTSIDE-CELL

\ The section in force: 0 host, 1 target; `host` and `target` write it.
$4DD8 constant SECTION-CELL

SNAP-RELOC:XTCELL-END constant PD-TABLE-OFF
PD-TABLE-OFF PD-SLOTS-REL + PD-CAP PD-SLOT * + constant PD-TABLE-END

\ Rows (text offset, input-buffer offset or NONE) for the runs
\ EMIT-BCAP appends, ascending: a run first drops the rows at or past BODYLEN.
\ The rows live in the body text block after its text: row i at
\ BODY-TEXT:BASE-CELL's base + cap + i * ROW, the count at BODY-TEXT:ROWS-CELL.
package BODY-SOURCE
public
16 constant ROW                     \ bytes per row
-1 constant NONE
;package
\ DATA-START: the user DP heap's floor (DP-CHECK); all below is the engine's.
PD-TABLE-END constant DATA-START
\ The bytes below a build's image where a routed header store marks its cell:
\ DATA-START and the cell every such store sets.
DATA-START 8 + $FFFF + $10000 / $10000 * constant ROUTED-BYTES

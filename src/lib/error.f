\ error.f - canonical stdlib throw codes. -999999..-100000 is reserved for
\ external applications and libraries (docs/forth.md, Errors). A file owns the
\ whole block between its FIRST and LAST, minted or not (tools/error-code-lint.f);
\ a subsystem that outgrows its block takes a second region, never renumbers.
\ A package's block is minted in the package's file; a comment here says where.
\ STORAGE CLASS. PROCESS-WIDE: the rows ERROR:CODE and ERROR:FAULT lay in data
\ space and the persisted head cell that holds the newest one's address. A row
\ holds its own fault flag: only this package reads a row's layout.

\ ERROR-TEXT is src/core/util.f's, which every prefix loads first.
require src/habu/layout.f
require src/habu/xref.f
require src/core/bytes.f
require src/lib/prelude.f

\ Arrays: -2000..-2099
-2000 constant E-A-FIRST
-2099 constant E-A-LAST
-2000 constant E-A-EMPTY
-2001 constant E-A-BOUNDS

\ Filesystem/files: -2100..-2199
-2100 constant E-FS-FIRST
-2199 constant E-FS-LAST
-2100 constant E-FS-PATH
-2101 constant E-FS-STAT
-2102 constant E-FS-OPEN
-2103 constant E-FS-DIR
-2105 constant E-FS-IO
-2106 constant E-FS-CAPACITY
-2107 constant E-FS-PATH-UNSAFE
-2108 constant E-FS-BAND        \ layout FS-ABI band widths disagree with the caps here
-2110 constant E-FS-TIMEOUT     \ a descriptor transfer reached its deadline

\ Strings: -2200..-2299
-2200 constant E-STR-FIRST
-2299 constant E-STR-LAST
-2200 constant E-STR-BOUNDS
-2201 constant E-STR-CAPACITY

\ Maps: -2400..-2499
-2400 constant E-MAP-FIRST
-2499 constant E-MAP-LAST
-2400 constant E-MAP-FULL
-2401 constant E-MAP-BAD-CAP

\ Processes: -2500..-2599
-2500 constant E-PROC-FIRST
-2599 constant E-PROC-LAST
-2500 constant E-PROC-SPAWN
-2501 constant E-PROC-WAIT
-2502 constant E-PROC-TIMEOUT
-2503 constant E-PROC-OUTPUT
-2504 constant E-PROC-TRUNCATED
-2505 constant E-PROC-ENV
-2506 constant E-PROC-PATH
-2507 constant E-PROC-PTY-CAPACITY
-2508 constant E-PROC-PTY-HANDLE
-2510 constant E-PROC-CANCELLED   \ the caller's borrowed cancellation fd became ready

\ Time/date: -2600..-2699
-2600 constant E-TIME-FIRST
-2699 constant E-TIME-LAST
-2600 constant E-TIME-RANGE
-2601 constant E-TIME-CAPACITY
-2602 constant E-TIME-CLOCK

\ Property tests: -2700..-2799
-2700 constant E-PROP-FIRST
-2799 constant E-PROP-LAST
-2700 constant E-PROP-SEED
-2701 constant E-PROP-GENERATOR
-2703 constant E-PROP-CAPACITY

\ Builds: -2800..-2899
-2800 constant E-BUILD-FIRST
-2899 constant E-BUILD-LAST
-2800 constant E-BUILD-SOURCE
-2801 constant E-BUILD-COMMAND
-2802 constant E-BUILD-STATUS
-2803 constant E-BUILD-PATH
-2804 constant E-BUILD-BOOT-DRIFT
-2805 constant E-BUILD-CERTIFY
-2806 constant E-BUILD-INSTALL

\ Diagnostics: -2900..-2999
-2900 constant E-DIAG-FIRST
-2999 constant E-DIAG-LAST
-2900 constant E-DIAG-SCHEMA
-2901 constant E-DIAG-CAPACITY \ re-registered as the same (code, name) in src/core/render.f package RDIAG, which compiles before src/lib/
-2902 constant E-DIAG-ORIGIN

\ Tables: -3000..-3099
-3000 constant E-TBL-FIRST
-3099 constant E-TBL-LAST
-3000 constant E-TBL-BOUNDS
-3001 constant E-TBL-FIELD

\ JSON writer: -3100..-3199
-3100 constant E-JSON-WRITE-FIRST
-3199 constant E-JSON-WRITE-LAST
-3100 constant E-JSON-WRITE-CAPACITY   \ the value does not fit the caller's output buffer
-3101 constant E-JSON-WRITE-BYTE       \ a negative U or indent, or string, key or part text that is not UTF-8
-3102 constant E-JSON-WRITE-STATE      \ writer not open, or a call its layout refuses
-3103 constant E-JSON-WRITE-OUTPUT     \ output buffer is null, or its capacity is negative
-3104 constant E-JSON-WRITE-SOURCE     \ appended span is negative, null with a positive length, or inside a growable output past its written bytes

\ OS-backed memory: -3200..-3299
-3200 constant E-MEM-FIRST
-3299 constant E-MEM-LAST
-3200 constant E-MEM-SIZE
-3201 constant E-MEM-MAP
-3202 constant E-MEM-TOTALITY
-3203 constant E-MEM-UNMAP
-3204 constant E-ALLOC-STATE           \ an allocator filled twice

\ Vectors: -3300..-3399
-3300 constant E-VEC-FIRST
-3399 constant E-VEC-LAST
-3300 constant E-VEC-BOUNDS
-3301 constant E-VEC-CAPACITY

\ PTX DSL: -3400..-3499
-3400 constant E-PTX-FIRST
-3499 constant E-PTX-LAST
-3400 constant E-PTX-SYNTAX
-3401 constant E-PTX-BLOCK
-3402 constant E-PTX-NOIMPL   \ tile op typed but not lowered to PTX
-3403 constant E-PTX-NOVJP    \ forward word has no registered adjoint (VJP)
-3404 constant E-PTX-ADCAP    \ AD reverse-pass token capacity exceeded
-3405 constant E-PTX-AD-OVERFLOW
-3406 constant E-PTX-AD-UNDERFLOW
-3407 constant E-PTX-AD-UNKNOWN
-3408 constant E-PTX-AD-OUTPUT
-3409 constant E-PTX-IR-OVERFLOW
-3410 constant E-PTX-IR-UNKNOWN
-3411 constant E-PTX-AD-CONTROL
-3412 constant E-PTX-EMIT      \ spawned kernel-emit child exited nonzero (its stderr surfaced)
-3413 constant E-PTX-CAP       \ in-process PTX capture buffer overflowed
-3416 constant E-KABI-CAP          \ kernel-ABI record capacity (params/fields/name pool) exceeded
-3417 constant E-KABI-FIELD        \ kernel-ABI field: unknown name/index, or no .param offset
-3418 constant E-KABI-DUP          \ kernel-ABI duplicate field name / conflicting extent-token source
-3419 constant E-KABI-TOKEN        \ kernel-ABI empty or oversized name/extent token
-3420 constant E-KEXPORT-KERNEL    \ kernel-export: unknown kernel name / record-name mismatch
-3421 constant E-KEXPORT-OUTDIR    \ kernel-export: out-dir missing or not a directory
-3422 constant E-KEXPORT-EMPTY     \ kernel-export: producer emitted no PTX
-3423 constant E-PTXTC-ARCH        \ ASSEMBLE invoked before the assembler target was set
-3424 constant E-PTXTC-PTXAS       \ no ptxas executable found on any known path
-3425 constant E-PTXTC-PROBE       \ resolved ptxas --version probe failed (spawn/timeout/truncated capture)
-3426 constant E-PTXTC-VERSION     \ ptxas --version produced output but no parseable release line
-3427 constant E-PTXTC-DIGEST      \ resolved ptxas unreadable or its SHA-256 is not the pinned allowlisted identity
-3428 constant E-PTXTC-STALE       \ sm_121 target: resolved ptxas is older than the pinned 13.3 floor

\ FFI: -3500..-3599
-3500 constant E-FFI-FIRST
-3599 constant E-FFI-LAST
-3500 constant E-FFI-ARITY
-3501 constant E-FFI-SYNTAX
-3502 constant E-FFI-DLSYM
\ FUNCTION: with no library selected in the declaring scope: state LIBRARY or
\ PROCESS-SYMBOLS beside the declarations rather than inherit another file's.
-3503 constant E-FFI-LIBRARY
-3504 constant E-FFI-NUL           \ a string with a 0 byte handed to C, which would end it early
-3505 constant E-FFI-DLOPEN        \ a library a FUNCTION: names could not be opened
\ src/lib/ffi-callback.f: a callback frame accessor used outside a live callback,
\ a descriptor no declaration made, or a dispatch whose body was never stored.
-3507 constant E-FFI-CALLBACK-STATE

\ Tasking/threads: -3600..-3699
-3600 constant E-TASK-FIRST
-3699 constant E-TASK-LAST
-3600 constant E-TASK-SIZE
-3603 constant E-TASK-THREAD
-3604 constant E-TASK-STATE
-3605 constant E-TASK-USER
-3606 constant E-TASK-SEM-STATE     \ semaphore initialized twice
-3607 constant E-TASK-SEM-COUNT     \ semaphore count out of range
-3608 constant E-TASK-SEM-HOST      \ host has no usable unnamed semaphores
-3609 constant E-TASK-JOIN          \ join of a task never activated or already joined
-3611 constant E-TASK-SEM-POOL      \ pooled semaphore: the handle is not a pooled record
-3612 constant E-TASK-NO-RESULT     \ the task ended without storing a result; carried by the err arm of a join
-3614 constant E-TASK-SLEEP-MS      \ TASK:SLEEP or TASK:WAIT-MS duration below zero or past the last deadline a cell holds

\ Engine self-identity: -3800..-3899
-3800 constant E-ENGINE-FIRST
-3899 constant E-ENGINE-LAST
-3800 constant E-ENGINE-PATH    \ own executable path cannot be resolved
-3801 constant E-ENGINE-KEY     \ own binary cannot be content-hashed
-3802 constant E-STACK-UNGUARDED \ run-in-stack was handed an extent that is not a guarded stack mapping (re-registered as the same (code, name) in src/habu/stack-abi.f)
-3803 constant E-PROF-RATE       \ prof-rate was handed an interval below 0 or past the OS timer's longest
-3804 constant E-EVAL-RESIDUE    \ an evaluate-closed text left cells above its floor (re-registered as the same (code, name) in src/habu/stack-abi.f)

\ JSON reader: -3900..-3999, minted by src/lib/json/read.f, a file of package JSON (src/lib/json.f).

\ Remote device harness (ssh zed): -4000..-4099
-4000 constant E-ZED-FIRST
-4099 constant E-ZED-LAST
-4000 constant E-ZED-UNREACH     \ ssh could not reach the host (connect/auth)
-4001 constant E-ZED-RC          \ remote command exited nonzero
-4002 constant E-ZED-PUT         \ scp/rsync artifact transfer failed
-4003 constant E-ZED-TOOLCHAIN   \ required remote tool missing (ptxas/nvcc/...)
-4004 constant E-ZED-ARG         \ bad harness argument or buffer capacity
-4005 constant E-ZED-DISABLED    \ device required but HABU_ZED unset/0
\ -4006 unassigned: a ZED run's expired deadline throws E-PROC-TIMEOUT.
\ E-ZED-EMIT is thrown downstream: Loom's tools/ptx/zed-gradcheck-suite.f
\ raises it.
-4007 constant E-ZED-EMIT        \ local artifact emit (bin/hb spawn) failed

\ Floating mathematics: -9020..-9029, minted by src/lib/fmath.f, which owns package
\ FMATH.

\ Source-composition discovery: -4100..-4199
-4100 constant E-DISC-FIRST
-4199 constant E-DISC-LAST
-4100 constant E-DISC-SHADOW
-4101 constant E-DISC-DYNAMIC
-4102 constant E-DISC-OPENER
-4103 constant E-DISC-UNTERM
-4104 constant E-DISC-CAPACITY
-4105 constant E-DISC-RETIRE

\ Test-suite DSL: -4200..-4299
-4200 constant E-SUITE-FIRST
-4299 constant E-SUITE-LAST
-4200 constant E-SUITE-MODE   \ GROUP mode token missing or not SEQ/PARA
-4201 constant E-SUITE-NAME   \ GROUP name missing or a reserved DSL keyword
-4202 constant E-SUITE-ROW    \ row argument list ran into a row keyword or the end of input (no ;SUITE)

\ Evaluation grader: -4300..-4399
-4300 constant E-EVAL-FIRST
-4399 constant E-EVAL-LAST
-4300 constant E-EVAL-EMIT    \ candidate emit infrastructure failed
-4301 constant E-EVAL-VOCAB   \ judge image lacks a task-required word

\ Test infrastructure: -4400..-4499
-4400 constant E-TEST-FIRST
-4499 constant E-TEST-LAST
-4400 constant E-TEST-CAPACITY

\ Owned growable byte buffer: -5700..-5799 (past the stdlib range -2000..-4999;
\ -5000..-5699 are owned by research/maki modules that keep codes in their own files)
-5700 constant E-BUF-FIRST
-5799 constant E-BUF-LAST
-5700 constant E-BUF-BOUNDS      \ byte offset / active length outside the buffer
-5701 constant E-BUF-CAPACITY    \ zero, negative, or cell-overflowing byte capacity
-5702 constant E-BUF-STATE       \ a buffer in the wrong state for the operation
-5703 constant E-BUF-OVERLAP     \ source and destination byte ranges overlap

\ Open-addressing integer-key hash probe: -5800..-5899
-5800 constant E-HM-FIRST
-5899 constant E-HM-LAST
-5800 constant E-HM-CAP     \ probe capacity is not a nonzero power of two
-5801 constant E-HM-FULL    \ probe scanned every slot without an empty slot or the key

\ Number formatting: -5900..-5999
-5900 constant E-FMT-FIRST
-5999 constant E-FMT-LAST
-5900 constant E-FMT-DOMAIN      \ unsigned formatter (SB-U/.U) given a negative value
-5901 constant E-FMT-OVERFLOW    \ SB-FIX scaled magnitude |x|*10^k does not fit an i64
-5902 constant E-FMT-BAND        \ layout FMT-ABI band width disagrees with FMT-NUM-CAP

\ Bounded pointers (package SPAN, src/lib/span.f): -6100..-6199. The block sits
\ after src/lib/type/deftype.f's own -6001/-6002; that file keeps its codes to
\ itself and reserves no range.
-6100 constant E-SPAN-FIRST
-6199 constant E-SPAN-LAST
-6100 constant E-SPAN-RANGE      \ a narrowing or an index outside the span's byte reach
-6101 constant E-SPAN-CAPACITY   \ a copy source longer than the destination span's reach
-6102 constant E-SPAN-LENGTH     \ a negative reach at the mint, or a negative source length

\ JSON reader performance samples: -6300..-6399
-6300 constant E-JRP-FIRST
-6399 constant E-JRP-LAST
-6300 constant E-JRP-SAMPLE      \ sample stored out of workload order or past the declared total
-6301 constant E-JRP-RANGE       \ workload or sample index outside the sample table

\ Engine arithmetic: -6400..-6499
-6400 constant E-ARITH-FIRST
-6499 constant E-ARITH-LAST
-6400 constant E-DIV-ZERO       \ `/`, `mod` or `/mod` given a zero divisor (thrown by the
                                \ engine's own primitive bodies through
                                \ the same (code, name) re-registered in src/habu/arith-abi.f)

\ Native test runner (package TEST): -6500..-6599
-6500 constant E-TR-FIRST
-6599 constant E-TR-LAST
-6500 constant E-TR-PATH-LEN    \ a persistent-root path longer than the runner's buffer

\ Shared compiler block: -6600..-6699; later compiler stages mint from
\ -8000..-8999.
-6600 constant E-IR-FIRST
-6699 constant E-IR-LAST
\ Compiler target contract (package CTARGET): -6610..-6619
-6610 constant E-CTGT-ABI         \ the ABI is not one the architecture can run
-6611 constant E-CTGT-ENDIAN      \ the byte order is not one the ABI is defined for
-6612 constant E-CTGT-PTR         \ the pointer width is not one the ABI is defined for
-6613 constant E-CTGT-FEATURE-BITS \ the raw feature mask holds a bit outside the vocabulary
-6614 constant E-CTGT-FEATURE     \ the feature set holds a feature the architecture has not
-6615 constant E-CTGT-BASE        \ the baseline instruction-set feature is absent
-6619 constant E-CTGT-ID          \ a backend id must be positive

\ Compiler numerical policy (package CNUM): -6620..-6629
-6620 constant E-CNUM-CONTRACT    \ contraction is allowed under a bit-exact rewrite licence

\ Compiler target/policy binding (package CBIND): -6630..-6639
-6630 constant E-CBIND-CONTRACT   \ contraction is allowed but the target has no fused multiply-add

\ Frozen compiler identity schema: -6700..-6799
-6700 constant E-CID-FIRST
-6799 constant E-CID-LAST
-6700 constant E-CID-FAMILY   \ the live checker type-family registry does not match the frozen identity family manifest
-6701 constant E-CID-TOKEN    \ a canonical manifest token holds a byte outside the printable non-space alphabet
-6702 constant E-CID-ROW      \ a schema vector row index or table slot is outside its declared table
-6703 constant E-CID-LEX      \ the shared source lexer refused the identity source it was asked to read
-6704 constant E-CID-DEF      \ a named identity definition is missing, duplicated, or unterminated in that source
-6705 constant E-CID-CONST    \ a named identity constant is missing, duplicated, or not a literal in that source
-6708 constant E-CID-REPLAY   \ a require replay handed back a module identity the allocator had already issued

\ Checker behavior and schema checks (package CHECKER-MODEL-PROOF): -6860..-6879.
-6860 constant E-CMP-FIRST
-6879 constant E-CMP-LAST
-6860 constant E-CMP-ROW      \ a shared checker-model vector index or table slot is outside its declared table
-6861 constant E-CMP-VOCAB    \ the checker's concrete type vocabulary - a name, code, class, width, or sign row of CT-INIT, or the CC-MAX ceiling - is not what the frozen table carries
-6862 constant E-CMP-CONTROL  \ the checker's control-flow dispatch table or a control frame kind is not what the frozen table carries
-6863 constant E-CMP-POOL     \ the frozen schema's own string pool or table is full

\ Instruction encoding checks: -6900..-6919.
-6900 constant E-CIE-FIRST
-6919 constant E-CIE-LAST
-6900 constant E-CIE-ROW      \ a shared instruction vector index or table slot is outside its declared table
-6901 constant E-CIE-FORM     \ an instruction form code no row of the frozen vocabulary names

\ Content-key folds: -6920..-6929, minted in src/lib/content-key.f, which owns
\ package CONTENT-KEY.

\ Snapshot relocation checks: -6880..-6899.
-6880 constant E-CRL-FIRST
-6899 constant E-CRL-LAST
-6880 constant E-CRL-ROW      \ a shared relocation vector index or table slot is outside its declared table
-6881 constant E-CRL-STRUCT   \ a frozen relocation structure - a pinned band constant, a canonicalization body, or the shape of a vector row - is not what the shipped source carries
-6882 constant E-CRL-DECODE   \ the shipped relocation pass carries an instruction, operand, symbol, or label the machine that runs it was never taught
-6883 constant E-CRL-FAULT    \ the shipped relocation pass, run over a synthetic image, touched an address outside every declared segment or ran past its step budget

\ Compiler growth region: -8000..-8999, the compiler's second block. Sub-blocks
\ are 20 codes wide, each headed below by its range; a stage that outgrows its
\ own takes the next one no `-8NNN constant` under src, tools or test claims.
-8000 constant E-COMP-FIRST
-8999 constant E-COMP-LAST

\ ARM64 routine machine-effect contracts (package NEFF): -8200..-8219
\ Minted for src/compiler/native-effect.f: what an emitted ARM64 routine does
\ to the registers, flags, link register, stack pointer and control.
-8200 constant E-NEFF-GPR     \ a general-register set naming a register no routine can hold state in: x18 is platform-reserved, x30 is the link register and has its own field, 31 is the zero register or the stack pointer, or the bit is past the 32-register file
-8201 constant E-NEFF-FPR     \ a floating-register set bit past the 32-register file
-8202 constant E-NEFF-ROLE    \ one register in two roles that exclude each other: a declared result that is also declared destroyed
-8203 constant E-NEFF-FRAME   \ a frame size that is negative, not a multiple of the stack alignment, or past the reach of the load and store offset field
-8204 constant E-NEFF-SP      \ a stack-pointer delta that rises above the caller's stack, is not a multiple of the stack alignment, is deeper than the declared frame, or is nonzero on a routine whose control returns
-8205 constant E-NEFF-LINK    \ a link-register effect that cannot hold: control returns or tail-calls while x30 is declared destroyed
-8206 constant E-NEFF-CONTROL \ results declared on a routine that never returns
-8207 constant E-NEFF-TRAIT   \ a trait mask holding a bit outside the vocabulary of things a routine can do
-8208 constant E-NEFF-SLOT    \ a frame slot the routine cannot address: a width no load or store form carries, a negative or unaligned offset, an offset past the frame, or an offset past the reach of that width's offset field
-8209 constant E-NEFF-SEQ     \ an ordered register list no calling convention can have: more positions than one cell holds, a register no routine can hold state in, one register named at two positions, or bits set past the last position
-8210 constant E-NEFF-CONV    \ a contract whose declared convention and whose places are two different statements: a data-stack convention naming a register, or a register convention naming a slot of the caller's data stack


\ -8220..-8239 unassigned: E-HIR-UNMODELED refuses what checked source may
\ compile a word into.

\ What T0 cannot compile: -8280..-8299.
-8286 constant E-HIR-UNMODELED  \ a source word this dialect cannot compile: unmodeled, or never declared at all
-8294 constant E-UNMODELED-IMMEDIATE  \ a body uses an immediate that is not parse-neutral and the compiler does not model: refused at its token, it never runs

\ The does> cut of a definition's source: -8400..-8409
-8407 constant E-NFEED-CUT      \ the does> cut the engine recorded is not just past a `does> ` in the definition source: it lies outside the source, or after other bytes - the source and the pending definition disagree

\ Native ARM64 data-stack calling convention: -8480..-8499
\
\ A position of a calling convention is a PLACE: a register or a data-stack
\ slot. Refusals another authority owns keep its name: a non-canonical place
\ list is NEFF's E-NEFF-SEQ, a register no routine may hold state in E-NEFF-GPR,
\ an operand outside its field the assembler's.
-8480 constant E-NEFF-KIND    \ a place read as the kind it is not: the register of a data-stack slot, or the slot index of a register

\ Native ARM64 control flow: -8500..-8519
-8506 constant E-A64EMIT-REACH  \ a branch distance that is not a whole instruction or does not fit the field its form encodes it in, or an adr or adrp distance its 21-bit field cannot hold
-8519 constant E-NELAB-BUNDLE   \ a multi-cell value the elaborator would have to take apart or would have to guess the shape of: one cell of such a value parked on the return stack, where the cells would be separated with every count still agreeing; or a declared row whose values the chain cannot place exactly, which is a segmentation it may not guess at; or a `!` through a pointer to a multi-cell family whose value on that vector does not begin, hold together and end where the certified width says, which is the same segmentation met from the writing side. It is also what a WIDTH on an operation that is not a memory access raises, which would mean the chain and the checker had read one token as two different words. A CAST:'s regrouping raises it for a window of cells that starts inside a value, which checked source cannot reach; and a scan whose width facts are incomplete (a token the scan could not apply while a multi-cell value was live) raises it before anything lowers, since the chain would have to guess where that value's cells lie

\ Native publication: -8560..-8579
-8561 constant E-NPUB-ROOM    \ the emission does not fit in what is left of the code arena under the engine's own end reserve
-8563 constant E-NPUB-OFFSET  \ an instruction whose source-map offset does not lie inside the emission it belongs to, or that is not instruction aligned
-8564 constant E-NEMIT-STATE  \ a reader asked src/compiler/native/emission.f about rows no backend has sealed, or that its RETIRE row has cleared; rows opened over live ones or added to none; a placement asked of an unplaced emission
-8565 constant E-NEMIT-ROW    \ a row index at or past the count the sealed emission holds, a row offset outside the emission, an empty emission, a trailing return longer than it, or a call site of neither kind
-8566 constant E-NPUB-PLACE   \ an emission whose branches were measured from an address that is not the code slot this seam is claiming for it

-8570 constant E-COMP-STATE   \ a compiler entry reached while another one is open, or a checker scan or store write from a callback while a definition compiles
-8572 constant E-COMP-VERDICT \ the engine's own check did not certify the pending definition
-8573 constant E-COMP-NAME    \ the pending record and the checker's tape name different definitions
-8574 constant E-COMP-OWNER   \ the checker that owns the source carries no operation for a front-end step this compiler needs: a replacement checker published a declaration-owner record without it, so the scan, the source tape, the does> split or a call fact has nowhere to come from
-8575 constant E-COMP-BINDING \ no successful scan or resolved binding exists for a required call ordinal
-8576 constant E-COMP-RECOVERY \ the active checker counted a refusal or borrowed its recovery fact; abandon the unpublished definition and continue
-8579 constant E-COMP-ARITY   \ the checker holds no declared effect for the pending definition

\ Asking the running dictionary about a spelling: -8600..-8609
-8600 constant E-NDICT-NAME     \ a spelling that denotes no word where the definition naming it is compiled: the open package's two wordlists, the global wordlist, the used publics and the namespace record for a qualified token all answer absent
-8601 constant E-NDICT-VALUE    \ the word a spelling denoted did not leave exactly one value where it was entered, so it is not a word whose whole meaning is the value it pushes and what it left is not its answer
-8602 constant E-NDICT-KIND     \ the record a spelling denoted carries no definer kind: it was not made by `constant`, `create` or `variable`, or a `does>` clause took that kind away from it, so entering it would run a body rather than collect a decided value

\ Native publication, continued: -8621..-8623
-8621 constant E-NPUB-RELOC   \ an emission that leaves through a branch to an address outside the code region: the snapshot relocation record can only describe a branch-with-link, so such a branch would survive a restore holding the writing run's displacement
-8622 constant E-NPUB-PENDING \ the pending record is not the unpublished slot the dictionary count points at
-8623 constant E-NPUB-TARGET  \ a sealed emission for another machine than the engine publishing it runs on: the code region holds only that machine's instructions, so another's bytes are refused before anything about the record is asked

\ Native emission and string storage: -8635..-8639
-8636 constant E-NSTR-CAP       \ a string literal body larger than one segment's arena (512 KB): the store opens another segment for any other body its last one cannot hold. The addresses it has already answered are compiled into published routines, so a body it cannot hold is a refusal rather than a reused address
-8637 constant E-NSTR-BODY      \ a string literal of negative length, which no reader produces and no arena can hold
-8638 constant E-INTERN-REWIND  \ a string interned while a build's data space is open after DP moved back inside it: a build does not go on once DP moves back inside its space, so a text interned after that is refused (re-registered as the same (code, name) in src/habu/intern-table.f package INTERN, which compiles before src/lib/)

\ The register-file description and the machine description over it:
\ -8670..-8672
-8670 constant E-NREGFILE       \ a register-file description that cannot describe a machine: a file of no registers or more than one set's bit mask holds, a reserved register outside the file it is reserved from, a file whose every register is reserved so nothing is allocatable at all, a call-destroyed register that is not allocatable in the first place, or a spill slot width that is not a positive power of two
-8671 constant E-NMACH          \ a machine description that cannot describe a machine: a link register set holding more than one register, a link register or a stack pointer that is not a register of the general file or that the file says is allocatable, a stack alignment that is not a positive power of two, a frame bound that is negative or not a multiple of that alignment, or a negative offset-field reach

\ The shadow target's routines: -8680..-8689
\
\ Refusals of the shadow chain's stages and of publication keep their own
\ names.
-8680 constant E-NSHADOW-STATE  \ a shadow opened over an open one or natively with no address rewriter installed, or its binding, a count or a source function asked of none
-8681 constant E-NSHADOW-ROW    \ a patched record the shadow does not hold, a taken emission whose bytes or rows are not as stated, or a `does>` clause function the taken emission does not have or that starts where the definer enters
-8682 constant E-NSHADOW-TARGET \ an emission taken for another machine than the one the shadow's binding names

\ Native x86-64 instruction emission: -8780..-8799.
-8790 constant E-X64EMIT-REACH  \ a branch, call or tail branch whose displacement is outside the signed thirty-two bits a rel32 field holds

\ Embedded instruction constructors (compiler growth region).
-8830 constant E-X64ASM-OPERAND
-8840 constant E-A32ASM-OPERAND
-8860 constant E-C6XASM-OPERAND

\ C66x instruction facts for scheduling and packet checks: -8870..-8879.
-8870 constant E-C6XFACTS-DECODE   \ a word outside the constructed subset

\ C66x subset interpreter for host tests: -8880..-8889.
-8880 constant E-C6XSIM-DECODE     \ a word the interpreter does not model
-8881 constant E-C6XSIM-FAULT      \ memory access outside the modelled window
-8882 constant E-C6XSIM-LIMIT      \ the cycle budget ran out
-8883 constant E-C6XSIM-OPERAND    \ a register, address or program outside the model
-8884 constant E-C6XSIM-CONFLICT   \ an execute packet or write SPRUGH7 3.8 forbids

\ C6000 EABI helper emission: -8890..-8899.
-8890 constant E-C6XEABI-OPERAND   \ no helper with that index, or a NOP or branch given to EMIT
-8891 constant E-C6XEABI-CAPACITY  \ the program or fixup table is full

\ ELF32 bounded binary reader: -9000..-9019.
-9000 constant E-ELF32-FIRST
-9019 constant E-ELF32-LAST
-9000 constant E-ELF32-FORMAT
-9001 constant E-ELF32-BOUNDS
-9002 constant E-ELF32-UNSUPPORTED

\ IPv4 UDP host library: -9100..-9109.
-9100 constant E-UDP4-FIRST
-9109 constant E-UDP4-LAST
-9100 constant E-UDP4-OPERAND
-9101 constant E-UDP4-PLATFORM
\ -9102 unassigned: a FUNCTION: symbol that is missing is E-FFI-DLSYM.
-9103 constant E-UDP4-RESULT

\ Raw serial host library: -9110..-9119.
-9110 constant E-SERIAL-FIRST
-9119 constant E-SERIAL-LAST
-9110 constant E-SERIAL-OPERAND
-9111 constant E-SERIAL-PLATFORM
\ -9112 unassigned: a FUNCTION: symbol that is missing is E-FFI-DLSYM.
-9113 constant E-SERIAL-RESULT

\ XMODEM packet codec and serial transfer: -9120..-9139.
-9120 constant E-XMODEM-FIRST
-9139 constant E-XMODEM-LAST
-9120 constant E-XMODEM-OPERAND
-9121 constant E-XMODEM-FRAME
-9122 constant E-SXMODEM-OPERAND
-9123 constant E-SXMODEM-TIMEOUT
-9124 constant E-SXMODEM-CLOSED
-9125 constant E-SXMODEM-IO
-9126 constant E-SXMODEM-CANCELLED
-9127 constant E-SXMODEM-RETRIES
-9128 constant E-SXMODEM-CAPACITY

\ Pseudoterminal pairs for test peers: -9160..-9169 (-9140..-9159 belong to
\ src/lib/byte-edit.f and src/lib/unicode.f).
-9160 constant E-PTY-FIRST
-9169 constant E-PTY-LAST
-9160 constant E-PTY-OPEN
-9161 constant E-PTY-IOCTL
-9162 constant E-PTY-IO
-9163 constant E-PTY-CAPACITY

\ Directory listings: -9170..-9179.
-9170 constant E-FS-LIST-FIRST
-9179 constant E-FS-LIST-LAST
-9170 constant E-FS-LIST-OPEN
-9171 constant E-FS-LIST-READ
-9172 constant E-FS-LIST-ENTRY
-9173 constant E-FS-LIST-CAPACITY
-9174 constant E-FS-LIST-ACTIVE \ a context already owns an active listing

\ IPv4 TCP host library: -9180..-9189.
-9180 constant E-TCP4-FIRST
-9189 constant E-TCP4-LAST
-9180 constant E-TCP4-OPERAND
-9181 constant E-TCP4-PLATFORM
\ -9182 unassigned: a FUNCTION: symbol that is missing is E-FFI-DLSYM.
-9183 constant E-TCP4-RESULT

\ Bounded task queues: -9190..-9199.
-9190 constant E-QUEUE-FIRST
-9199 constant E-QUEUE-LAST
-9190 constant E-QUEUE-OPERAND
-9191 constant E-QUEUE-STATE
-9192 constant E-QUEUE-TABLE        \ more queue definitions than the package table holds

\ Generic I/O devices: -9220..-9229.
-9220 constant E-GENIO-FIRST
-9229 constant E-GENIO-LAST
-9220 constant E-GENIO-OPERAND
-9221 constant E-GENIO-STATE
-9222 constant E-GENIO-IO
-9223 constant E-GENIO-FULL       \ no device row free

\ HTTPS client over libcurl: -9230..-9239.
-9230 constant E-CURL-FIRST
-9239 constant E-CURL-LAST
-9230 constant E-CURL-PLATFORM
\ -9231 unassigned: a FUNCTION: symbol that is missing is E-FFI-DLSYM.
-9232 constant E-CURL-STATE
-9233 constant E-CURL-RESULT
-9234 constant E-CURL-OPERAND
\ -9235 unassigned: src/lib/net/curl.f grows its transfer records.

\ Authenticated encryption and HMAC over libcrypto (package CRYPTO):
\ -9270..-9279.
-9270 constant E-CRYPTO-FIRST
-9279 constant E-CRYPTO-LAST
-9270 constant E-CRYPTO-OPERAND    \ a key, nonce, span length or output span the operation cannot accept
-9271 constant E-CRYPTO-SEAL       \ libcrypto refused a step of the encryption
-9272 constant E-CRYPTO-OPEN       \ libcrypto refused a step of the decryption
-9273 constant E-CRYPTO-TAG        \ the tag did not authenticate the ciphertext and its associated data
-9274 constant E-CRYPTO-RANDOM     \ the random generator reported failure
-9275 constant E-CRYPTO-MAC        \ libcrypto refused the keyed hash, or answered a digest of the wrong width
-9276 constant E-CRYPTO-PLATFORM   \ this module is qualified on Linux only, and refuses a foreign target before it binds
-9277 constant E-CRYPTO-KEY        \ RS256 key encoding, import or validation refusal
-9278 constant E-CRYPTO-VERIFY     \ RS256 verification setup or operation failure
-9279 constant E-CRYPTO-SIGN       \ RS256 signing setup or generation failure

\ PostgreSQL over libpq: -9250..-9259, minted by src/lib/pg.f, which owns package
\ PG. The block is full; PG's row readers are -9320..-9329 and its growth region
\ -9560..-9569.

\ Process signals on a self-pipe (package SIGNAL): -9280..-9289.
-9280 constant E-SIGNAL-FIRST
-9289 constant E-SIGNAL-LAST
-9280 constant E-SIGNAL-STATE     \ a word that needs the facility before INIT, or a second INIT without RELEASE
-9281 constant E-SIGNAL-ABI       \ the published stub cells read zero: no baked stub, or not the main task
-9282 constant E-SIGNAL-NUMBER    \ a signal number outside the range sigaction installs
-9283 constant E-SIGNAL-INSTALL   \ sigaction refused the install or the restore
-9284 constant E-SIGNAL-READ      \ the descriptor answered short of the four-byte number
-9285 constant E-SIGNAL-POLL      \ poll refused the read end with an errno
-9286 constant E-SIGNAL-CLOSE     \ closing an end of the self-pipe failed

\ The io_uring readiness loop (package AIO): -9290..-9299.
-9290 constant E-AIO-FIRST
-9299 constant E-AIO-LAST
-9290 constant E-AIO-SETUP        \ io_uring_setup or one of the ring mappings was refused
-9291 constant E-AIO-ENTER        \ io_uring_enter was refused, or took fewer entries than offered
-9292 constant E-AIO-STATE        \ no loop running, or a second START
-9293 constant E-AIO-FULL         \ the kernel's io_uring ring is full: no submission entry free, or as many operations in flight as half the completion ring holds
-9294 constant E-AIO-BUSY         \ STOP with an operation still in flight
-9295 constant E-AIO-GROUP        \ the group is empty, holds no such ticket, or names no defined group
-9296 constant E-AIO-BOUNDS       \ a transfer count past its allocation or below zero, a file offset below -1, or a socket address length not positive
-9297 constant E-AIO-POLL-BOUND   \ Darwin's poll(2) takes at most OPEN_MAX (10240) descriptors per call, and one more would be needed

\ Package PG's row readers: -9320..-9329, minted by src/lib/pg-rows.f.

\ The WebSocket frame codec, RFC 6455 (package WS, src/lib/net/ws-frame.f):
\ -9330..-9339. The codes after E-WS-MASK are its reserve.
-9330 constant E-WS-FIRST
-9339 constant E-WS-LAST
-9330 constant E-WS-RSV           \ a reserved bit set in a frame's first byte
-9331 constant E-WS-OPCODE        \ a reserved opcode
-9332 constant E-WS-CONTROL       \ a control frame that is fragmented or carries more than 125 bytes
-9333 constant E-WS-LENGTH        \ a negative payload length, or an extended length with its high bit set or not in its shortest form
-9334 constant E-WS-TOO-BIG       \ a payload longer than the receiver's bound
-9335 constant E-WS-MASK          \ a mask bit the sender must not set or must set, or a key the sender cannot use

\ The HTTP/1.1 server (package HTTP, src/lib/net/http.f): -9340..-9349.
-9340 constant E-HTTP-FIRST
-9349 constant E-HTTP-LAST
-9340 constant E-HTTP-STATE       \ START on a running server, STOP on a stopped one, a hook installed while one runs, a second STATIC-ROOT, a TAKE-OVER outside a running handler or a second one inside it, a BODY$ or BODY-READER outside its running handler or after its TAKE-OVER, BEARER:ISSUER!, BEARER:METADATA! or TEST-ISSUER:CLIENT+ while serving, BEARER:ADMIT? before ISSUER!, or BEARER:TOKEN or BEARER:REFRESH before CLIENT!
-9341 constant E-HTTP-HANDLE      \ a request or response handle past its request, or one over another worker's slot, or a slot asked by a task that is no worker
-9342 constant E-HTTP-WORKERS     \ a worker count outside 1..MAX-WORKERS, or an idle deadline under 1 ms
-9343 constant E-HTTP-CAPACITY    \ more hooks, headers or bound segments than a table holds, or a negative body length
-9344 constant E-HTTP-ROUTE       \ more routes than the table holds, or an empty method or a pattern not starting with /
-9345 constant E-HTTP-STATIC      \ a bad or non-directory static root, a URL wider than PATH-CAP, or a directory cycle
-9346 constant E-HTTP-SOCKET      \ the listener could not be bound, put to listen or asked its own address
-9347 constant E-HTTP-BODY-READ   \ a handler's body read that did not complete; the server answers it with the read's refusal, never as a fault

\ Base64 (package BASE64): -9350..-9359.
-9350 constant E-BASE64-FIRST
-9359 constant E-BASE64-LAST
-9350 constant E-BASE64-CHAR      \ a byte outside the selected alphabet
-9351 constant E-BASE64-PAD       \ misplaced/forbidden '=', or nonzero unused bits
-9352 constant E-BASE64-LENGTH    \ padded length not divisible by four, or URL remainder one

\ Private C2 owner-frame runtime.
-9360 constant E-C2-CAPACITY       \ this task has no owner frame left
-9361 constant E-C2-STATE          \ a frame operation contradicts its transition state

\ File URIs (package URI, src/lib/uri.f): -9370..-9379.
-9370 constant E-URI-FIRST
-9379 constant E-URI-LAST
-9370 constant E-URI-SCHEME       \ a scheme other than `file`, compared without case, or none
-9371 constant E-URI-AUTHORITY    \ after `file:`, anything but `//`, an empty or `localhost` authority, and the `/` that starts the path
-9372 constant E-URI-ESCAPE       \ a `%` without two hex digits after it, or a bare `?` or `#`, which a file URI's path carries only escaped
-9373 constant E-URI-RELATIVE     \ a path to encode that does not start with `/`, so no file URI names it

\ Content-Length framing (package CONTENT-LENGTH, src/lib/content-length.f): -9380..-9389.
-9380 constant E-CONTENT-LENGTH-FIRST
-9389 constant E-CONTENT-LENGTH-LAST
-9380 constant E-CONTENT-LENGTH-MALFORMED  \ a header block that names no single decimal body length within the reader's maximum or a charset other than UTF-8, or a line that is not `token: value` ended by CR LF within LINE-CAP
-9381 constant E-CONTENT-LENGTH-TRUNCATED  \ end of file inside a header block or a body
-9382 constant E-CONTENT-LENGTH-STATE      \ a reader used before BIND or after a refusal, NEXT-LENGTH with a body pending, BODY with none, or BIND with a buffer under LINE-CAP + 2 bytes or a negative maximum

\ JSON-RPC 2.0 envelope (package JSON-RPC, src/lib/json-rpc.f): -9390..-9399.
-9390 constant E-JSON-RPC-FIRST
-9399 constant E-JSON-RPC-LAST
-9390 constant E-JSON-RPC-ID             \ a response written with an empty id

\ Language server: -9400..-9401, minted by tools/lsp-core.f, which owns package
\ LSP.

\ WebSocket connections over the HTTP server (package WS, src/lib/net/ws.f):
\ -9410..-9419.
-9410 constant E-WS-CONN-FIRST
-9419 constant E-WS-CONN-LAST
-9410 constant E-WS-CLOSED        \ a send on a socket that is closing, closed or gone, or one whose queue overflowed and lost it
-9411 constant E-WS-CODE          \ a close status no endpoint may send: outside 1000..4999, reserved or unassigned
-9414 constant E-WS-TEXT          \ SEND-TEXT of bytes that are not UTF-8 (RFC 6455 section 5.6)
-9415 constant E-WS-STATE         \ ACCEPT or STOP outside a run or while START still starts it, a START while one starts or runs, a STOP while another STOP runs or on WS's own task
-9417 constant E-WS-LIMIT         \ a WS:START limit below 1 or past the handle's 32-bit slot field

\ SSE owns -9450..-9459. DECIMAL owns -9460..-9469.
\ JSONL framing (package JSONL, src/lib/jsonl.f): -9470..-9479.
\ MCP tool server and client (package MCP, src/lib/net/mcp.f): -9480..-9489,
\ minted there.
\ Secret store, native or host-bound: -9500..-9509, minted in src/lib/secret.f.

\ Intel disassembly: -9510..-9519, minted by src/arch/x86-64/disasm.f,
\ which owns package X64DIS.
\ Raw DEFLATE and CRC-32: -9520..-9529, minted in src/lib/zlib.f, which owns
\ package ZLIB.
\ Strict UTF-16LE conversion: -9530..-9539, minted in src/lib/utf16.f, which
\ owns package UTF16.
\ OS random bytes: -9540..-9549, minted in src/lib/entropy.f, which owns
\ package ENTROPY.
\ Online processor count and load average: -9550..-9559, minted in
\ src/lib/os-cpu.f, which owns package OS-CPU.
\ PostgreSQL's growth region: -9560..-9569, minted by src/lib/pg.f.
\ Word contracts read from source: -9570..-9579, minted in src/lib/word-docs.f,
\ which owns package WORD-DOCS.
\ Scoped commands: -9580..-9589, minted in src/lib/process-command.f, which
\ owns package CMD.
\ Tier 0 emitter seam: -9590..-9599, minted in src/habu/t0-emit.f, package
\ COMP.
\ Stderr capture for tests: -9610..-9619, minted in src/lib/test/stderr.f, which
\ owns package STDERR.
\ A task to each job: -9620..-9629, minted in src/lib/crew.f, which owns
\ package CREW.
\ OAuth bearer tokens: -9630..-9639, minted in src/lib/net/bearer.f, which owns
\ package BEARER.
\ Keeping the machine awake: -9640..-9649, minted in src/lib/power.f, which
\ owns package POWER.
\ Code signing: -9650..-9659, minted in src/lib/codesign.f, which owns package
\ CODESIGN.
\ State machines: -9660..-9669, minted in src/lib/fsm.f, which owns package
\ FSM.
\ Codes the ERROR:CODE tests declare: -9670..-9689, in src/lib/error-test.f,
\ src/lib/log-test.f and test/fixtures/error/.
\ The linker's failed build: -9690..-9699, minted in src/habu/linker.f, package
\ LINKER.
\ Model chat and embedding client: -9700..-9799, minted in src/lib/net/model.f,
\ which owns package MODEL.

\ Wasm back end: -9800..-9834, minted in src/arch/wasm/: WLEB -9800..-9804,
\ WLINK -9830..-9834.
\ Adobe Ascii85: -9840..-9849, minted in src/lib/base85.f, which owns package
\ BASE85.
\ Finite binary64 text: -9860..-9869, minted in src/lib/f64-text.f, which owns
\ package F64-TEXT.

\ T0 (src/habu/t0.f): -9600..-9609, minted here, not in package COMP, because
\ src/compiler/native/definition.f MALFORMED? reads them.
-9600 constant E-T0-FIRST
-9609 constant E-T0-LAST
-9600 constant E-T0-UNSUPPORTED  \ a token T0 does not compile
-9601 constant E-T0-CONTROL      \ a control word without its opener, a body ending inside one, or a token no path reaches
-9604 constant E-T0-BINDS        \ the body binds other than its scan's count of locals
-9606 constant E-T0-MATCH        \ a MATCH or construct the checker's facts do not bear out
-9607 constant E-T0-DEFER        \ `is` names no deferred word
-9608 constant E-T0-BUNDLE       \ an operation over a width it cannot move

\ HEAD and each row's first cell are declared address cells holding the newest
\ row and the row before, NULL-PTR for none, so the linker relocates them.
package ERROR

private

255 constant NAME-CAP
\ `-`, 19 digits, ` constant ` and the name.
NAME-CAP 30 + constant LINE-CAP

PERSISTED-PTR-VARIABLE HEAD
create LINE LINE-CAP allot
variable LINE-U
create NL $0A c,

: ROW-PREV ( ptr n -- ptr n )  0 ptr-field @ ;
: ROW-CODE ( ptr n -- n )  cell+ @ ;


: ROW-FAULT? ( ptr n -- bool )  4 cells + @ 0<> ;


: ROW-NAME$ ( ptr n -- ptr u8 n ) {: r:ptr :}
   r 5 cells + BYTE-VIEW r 2 cells + @ ;


: ROW-TEXT$ ( ptr n -- ptr u8 n ) {: r:ptr :}
   r 5 cells + BYTE-VIEW r 2 cells + @ + r 3 cells + @ ;


: FIND ( n -- ptr n ) {: code:n :}
   HEAD @
   begin dup NULL-PTR <> while
      dup ROW-CODE code = if exit then
      ROW-PREV
   repeat ;


\ The open package's own spelling, empty outside a package.
: PKG$ ( -- ptr u8 n )
   data-base PKG-REC-CELL + @ dup 0= if drop s" " exit then
   XREF-ADDR>REC XREF-NAME$ ;


: SAY ( ptr u8 n -- )  ERROR-TEXT:TYPE ;


: SAY-INT ( n -- )
   dup 0< if s" -" SAY then
   ERROR-TEXT:DIGITS$ SAY ;


: SAY-NAME ( ptr u8 n -- ) {: name:ptr nameu:n :}
   PKG$ {: p:ptr pu:n :}
   pu 0<> if p pu SAY s" :" SAY then
   name nameu SAY ;


\ A refusal's line ends here; the empty die adds nothing to it.
: REFUSE ( -- )
   NL 1 SAY
   s" " 70 die ;


\ A refusal's first words: the declaring word, FAULT when the flag is set.
: SAY-WORD ( bool -- )
   if s" ERROR:FAULT: " SAY exit then
   s" ERROR:CODE: " SAY ;


: SAME-NAME? ( ptr n ptr u8 n -- bool )
   {: at:ptr name:ptr nameu:n :}
   at ROW-NAME$ {: r:ptr ru:n :}
   PKG$ {: p:ptr pu:n :}
   pu 0= if r ru name nameu XREF-STR=CI exit then
   ru pu nameu + 1+ <> if false exit then
   r pu + c@ $3A <> if false exit then
   r pu p pu XREF-STR=CI if r pu + 1+ nameu name nameu XREF-STR=CI exit then
   false ;


\ Ends the process, exit 70 and one line on fd 2 that starts with the declaring
\ word, for a missing name, a name past 255 bytes, an empty text, or a code
\ outside 0..255 another name holds.
: CHECK ( n ptr u8 n n bool -- )
   {: code:n name:ptr nameu:n textu:n fault:bool :}
   nameu 0= if
      fault SAY-WORD s" no name after the code " SAY code SAY-INT REFUSE
   then
   nameu NAME-CAP > if
      fault SAY-WORD s" the name " SAY name nameu SAY
      s"  is longer than 255 bytes" SAY REFUSE
   then
   textu 0= if
      fault SAY-WORD name nameu SAY-NAME s"  has an empty text" SAY REFUSE
   then
   code 0 < code 255 > or 0= if exit then
   code FIND {: at:ptr :}
   at NULL-PTR = if exit then
   at name nameu SAME-NAME? if exit then
   fault SAY-WORD code SAY-INT s"  is " SAY at ROW-NAME$ SAY
   s" , declared again as " SAY name nameu SAY-NAME REFUSE ;


: LINE+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a LINE LINE-U @ + u BYTE-COPY
   u LINE-U +! ;


: LINE-INT+ ( n -- )
   dup 0< if s" -" LINE+ then
   ERROR-TEXT:DIGITS$ LINE+ ;


\ Evaluated text keeps every E- word a folded constant in the current wordlist.
: DEFINE ( n ptr u8 n -- ) {: code:n name:ptr nameu:n :}
   0 LINE-U !
   code LINE-INT+ s"  constant " LINE+ name nameu LINE+
   LINE LINE-U @ evaluate-closed ;


: BYTES, ( ptr u8 n -- ) {: a:ptr u:n :}
   a here u BYTE-COPY
   u allot ;


: PAD, ( -- )
   begin here NULL-PTR - 1 cells 1- and 0<> while 0 c, repeat ;


\ A row is five cells, the previous row, the code, the qualified name's length,
\ the text's length and the fault flag (-1 or 0), then the name and the text.
: ROW, ( n ptr u8 n ptr u8 n bool -- )
   {: code:n name:ptr nameu:n text:ptr textu:n fault:bool :}
   align
   PKG$ {: p:ptr pu:n :}
   here CELL-VIEW {: at:ptr :}
   here ptr-cell-mark 0 ,
   HEAD @ at 0 ptr-field !
   code ,
   pu nameu + pu 0<> if 1+ then ,
   textu ,
   fault bool>n ,
   p pu BYTES,
   pu 0<> if s" :" BYTES, then
   name nameu BYTES,
   text textu BYTES,
   PAD,
   at HEAD ! ;


\ CODE's and FAULT's declaration of the name after them, the flag saying FAULT.
: DECLARE ( n ptr u8 n bool -- )
   {: code:n text:ptr textu:n fault:bool :}
   parse-name {: name:ptr nameu:n :}
   code name nameu textu fault CHECK
   code name nameu DEFINE
   code name nameu text textu fault ROW, ;

public

\ A name `constant` refuses is refused as `constant` refuses it,
\ E-DUPLICATE-DEFINITION for one the wordlist holds.
: CODE ( n ptr u8 n -- )  false DECLARE ;
generates: CODE ( -- n )

\ Declares a code as CODE does, refusals included, and marks it a fault: a
\ defect in the program or its environment rather than in what its caller gave.
: FAULT ( n ptr u8 n -- )  true DECLARE ;
generates: FAULT ( -- n )

\ True when the newest declaration of a code was by FAULT; false when it was by
\ CODE or when there is none.
: FAULT? ( n -- bool )
   FIND dup NULL-PTR = if drop false exit then
   ROW-FAULT? ;

\ The newest declaration's name or text for a code, empty when none.
: NAME$ ( n -- ptr u8 n )
   FIND dup NULL-PTR = if drop s" " exit then
   ROW-NAME$ ;


: TEXT$ ( n -- ptr u8 n )
   FIND dup NULL-PTR = if drop s" " exit then
   ROW-TEXT$ ;

seal-package
;package

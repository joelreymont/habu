\ errors.f - canonical stdlib throw codes. -999999..-100000 is reserved for
\ external applications and libraries (docs/forth.md, Errors). A file owns the
\ whole block between its FIRST and LAST, minted or not (tools/error-code-lint.f);
\ a subsystem that outgrows its block takes a second region, never renumbers.
\ A package's block is minted in the package's file; a comment here says where.
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state (constants only).

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
\ -2104 unassigned.
-2105 constant E-FS-IO
-2106 constant E-FS-CAPACITY
-2107 constant E-FS-PATH-UNSAFE
-2108 constant E-FS-BAND        \ layout FS-ABI band widths disagree with the caps here
\ -2109 unassigned.
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
-2509 constant E-PROC-HOST        \ a host-specific request number was needed on a host that is neither supported target
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
-2702 constant E-PROP-SHRINK
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
-3100 constant E-JW-FIRST
-3199 constant E-JW-LAST
-3100 constant E-JW-CAPACITY   \ the value does not fit the caller's output buffer
-3101 constant E-JW-BYTE       \ a byte outside 0-255, a negative U, or STRING/KEY text that is not UTF-8
-3102 constant E-JW-STATE      \ writer used before JSON-WRITE:OPEN or after CLOSE
-3103 constant E-JW-OUTPUT     \ output buffer is null, or its capacity is negative
-3104 constant E-JW-SOURCE     \ appended span is negative, or null with a positive length

\ OS-backed memory: -3200..-3299
-3200 constant E-MEM-FIRST
-3299 constant E-MEM-LAST
-3200 constant E-MEM-SIZE
-3201 constant E-MEM-MAP
-3202 constant E-MEM-TOTALITY
-3203 constant E-MEM-UNMAP

\ Vectors: -3300..-3399
-3300 constant E-VEC-FIRST
-3399 constant E-VEC-LAST
-3300 constant E-VEC-BOUNDS
-3301 constant E-VEC-CAPACITY
-3302 constant E-VEC-STATE

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
\ -3414 and -3415 unassigned.
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
-3506 constant E-FFI-CALLBACK-FULL  \ thrown by no code
\ src/lib/ffi-callback.f: a callback frame accessor used outside a live callback,
\ a descriptor no declaration made, or a dispatch whose body was never stored.
-3507 constant E-FFI-CALLBACK-STATE
\ dlclose refused an owned library reference while preparing an image; the
\ handle stays and the next PREPARE retries it.
-3508 constant E-FFI-DLCLOSE

\ Tasking/threads: -3600..-3699
-3600 constant E-TASK-FIRST
-3699 constant E-TASK-LAST
-3600 constant E-TASK-SIZE
\ -3601 and -3602 unassigned: package FFI names a missing symbol E-FFI-DLSYM.
-3603 constant E-TASK-THREAD
-3604 constant E-TASK-STATE
-3605 constant E-TASK-USER
-3606 constant E-TASK-SEM-STATE     \ semaphore not initialized, initialized twice, or destroyed
-3607 constant E-TASK-SEM-COUNT     \ semaphore count out of range
-3608 constant E-TASK-SEM-HOST      \ host has no usable unnamed semaphores
-3609 constant E-TASK-JOIN          \ join of a task never activated or already joined
-3610 constant E-TASK-MAILBOX       \ message operation no mailbox can serve: no TCB, target not running, or self
-3611 constant E-TASK-SEM-POOL      \ pooled semaphore: the handle is not a pooled record
-3612 constant E-TASK-NO-RESULT     \ the task ended without storing a result; carried by the err arm of a join
-3613 constant E-TASK-EXIT-TABLE    \ more cleanup rows held at capture than the image's table carries
-3614 constant E-TASK-SLEEP-MS      \ TASK:SLEEP or TASK:STOP-FOR duration below zero or past the last deadline a cell holds

\ Engine self-identity: -3800..-3899
-3800 constant E-ENGINE-FIRST
-3899 constant E-ENGINE-LAST
-3800 constant E-ENGINE-PATH    \ own executable path cannot be resolved
-3801 constant E-ENGINE-KEY     \ own binary cannot be content-hashed
-3802 constant E-STACK-UNGUARDED \ run-in-stack was handed an extent that is not a guarded stack mapping (thrown by src/habu/habu1.f BRUNSTACK through the same (code, name) re-registered in src/habu/stack-abi.f)
-3803 constant E-PROF-RATE       \ prof-rate was handed an interval below 0 (thrown by src/habu/prof.f BPROF-RATE and src/habu/kernel-x64.f's prof-rate row through the same (code, name) re-registered in src/habu/prof-abi.f)
-3804 constant E-EVAL-RESIDUE    \ an evaluate-closed text left cells above its floor (thrown by src/habu/habu1.f B-EVAL-CLOSED through the same (code, name) re-registered in src/habu/stack-abi.f)

\ JSON reader: -3900..-3999, minted by src/lib/json-read.f, which owns package JR.

\ Remote device harness (ssh zed): -4000..-4099
-4000 constant E-ZED-FIRST
-4099 constant E-ZED-LAST
-4000 constant E-ZED-UNREACH     \ ssh could not reach the host (connect/auth)
-4001 constant E-ZED-RC          \ remote command exited nonzero
-4002 constant E-ZED-PUT         \ scp/rsync artifact transfer failed
-4003 constant E-ZED-TOOLCHAIN   \ required remote tool missing (ptxas/nvcc/...)
-4004 constant E-ZED-ARG         \ bad harness argument or buffer capacity
-4005 constant E-ZED-DISABLED    \ device required but HABU_ZED unset/0
\ -4006 unassigned: a ZED run's expired deadline throws E-PROC-TIMEOUT from
\ CMD:RUN-RC.
\ E-ZED-EMIT is thrown downstream: Loom's tools/ptx/zed-gradcheck-suite.f
\ raises it when its bin/hb spawn that emits a PTX kernel exits nonzero.
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
-5702 constant E-BUF-STATE       \ touch of a disposed buffer, or re-init of a live one
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
-6103 constant E-SPAN-UNTERM     \ bare-copy lint: unterminated string literal hid the rest of a file
-6104 constant E-SPAN-REGISTRY   \ bare-copy lint: malformed primitive-axiom row hid the rest of a file

\ JSON reader performance samples: -6300..-6399
-6300 constant E-JRP-FIRST
-6399 constant E-JRP-LAST
-6300 constant E-JRP-SAMPLE      \ sample stored out of workload order or past the declared total
-6301 constant E-JRP-RANGE       \ workload or sample index outside the sample table

\ Engine arithmetic: -6400..-6499
-6400 constant E-ARITH-FIRST
-6499 constant E-ARITH-LAST
-6400 constant E-DIV-ZERO       \ `/`, `mod` or `/mod` given a zero divisor (thrown by the
                                \ engine's own primitive bodies in src/habu/habu1.f through
                                \ the same (code, name) re-registered in src/habu/arith-abi.f)

\ Native test runner (package TEST): -6500..-6599
-6500 constant E-TR-FIRST
-6599 constant E-TR-LAST
-6500 constant E-TR-PATH-LEN    \ a persistent-root path longer than the runner's buffer

\ Shared compiler IR representation: -6600..-6699. The block is FULL: later
\ compiler stages mint from -8000..-8999 at the end of this file.
-6600 constant E-IR-FIRST
-6699 constant E-IR-LAST
-6600 constant E-IR-MODULE-ZERO
-6601 constant E-IR-MODULE-RANGE
-6602 constant E-IR-INDEX-RANGE
-6603 constant E-IR-INDEX-BOUND
-6604 constant E-IR-OWNER
-6605 constant E-IR-SCALAR-RANGE
-6606 constant E-IR-MODULE-EXHAUSTED
\ Compiler target contract (package CTARGET): -6610..-6619
-6610 constant E-CTGT-ABI         \ the ABI is not one the architecture can run
-6611 constant E-CTGT-ENDIAN      \ the byte order is not one the ABI is defined for
-6612 constant E-CTGT-PTR         \ the pointer width is not one the ABI is defined for
-6613 constant E-CTGT-FEATURE-BITS \ the raw feature mask holds a bit outside the vocabulary
-6614 constant E-CTGT-FEATURE     \ the feature set holds a feature the architecture has not
-6615 constant E-CTGT-BASE        \ the baseline instruction-set feature is absent
-6616 constant E-CTGT-UNLOADED    \ no backend for the architecture is loaded in this image
-6617 constant E-CTGT-REGISTERED  \ a second backend claims an architecture that already has a row
-6618 constant E-CTGT-ROW         \ an unpublished descriptor row was requested
-6619 constant E-CTGT-ID          \ a backend id must be positive

\ Compiler numerical policy (package CNUM): -6620..-6629
-6620 constant E-CNUM-CONTRACT    \ contraction is allowed under a bit-exact rewrite licence

\ Compiler target/policy binding (package CBIND): -6630..-6639
-6630 constant E-CBIND-CONTRACT   \ contraction is allowed but the target has no fused multiply-add

\ Compiler context ownership (package IR-CTX): -6640..-6649
-6640 constant E-IR-CTX-STALE     \ a context handle was used after its context tore down
-6641 constant E-IR-CTX-DEPTH     \ context nesting exceeded the registry capacity
-6643 constant E-IR-CTX-SERIALS   \ serial allocation reached its committed ceiling
-6644 constant E-IR-CTX-SCRATCH   \ scratch size or total exceeds the addressable range
-6645 constant E-IR-CTX-CEILING   \ a context-creation serial ceiling outside the accepted range
-6646 constant E-IR-CTX-SIZE      \ a scratch request of zero or negative bytes
-6647 constant E-IR-CTX-STATE     \ context registry or session state is invalid
-6648 constant E-IR-CTX-NESTED    \ scratch was taken for a context that a deeper live context encloses

\ Compiler IR arena (package IR-ARENA): -6650..-6659
-6650 constant E-IR-ARENA-STALE   \ an arena handle was used after abort, after retire, or after its context's teardown
-6651 constant E-IR-ARENA-OWNER   \ an index or context was presented to an arena that did not mint it
-6652 constant E-IR-ARENA-FULL    \ an append reached the arena's committed cell ceiling
-6653 constant E-IR-ARENA-FROZEN  \ a mutation word ran against a frozen arena
\ -6654 unassigned.
-6655 constant E-IR-ARENA-BOUND   \ an arena index at or past the readable cell count
-6656 constant E-IR-ARENA-CEIL    \ an arena cell count outside the accepted range: a creation ceiling, or a reservation
\ -6657 and -6658 unassigned.
-6659 constant E-IR-ARENA-STATE   \ persisted arena state failed its consistency recheck

\ Compiler source registry (package IR-SOURCE): -6660..-6669
-6660 constant E-IR-SRC-STATE     \ a registry arena failed its header or row-shape recheck
-6661 constant E-IR-SRC-OWNER     \ a module key or source-id presented to a registry that does not own it
-6662 constant E-IR-SRC-BOUND     \ a source-id whose ordinal is at or past the registered count
-6663 constant E-IR-SRC-ORIGIN    \ an origin that is not an already-registered source; every cycle attempt lands here
-6664 constant E-IR-SRC-SPAN      \ a byte span with a negative bound or one crossing its source's end
-6665 constant E-IR-SRC-CAP       \ a registry capacity outside the accepted range, or a registration past it
-6666 constant E-IR-SRC-LEN       \ a negative source byte length at registration
-6667 constant E-IR-SRC-ROOT      \ an origin read on a root source, which has none

\ Compiler symbol interner (package IR-SYM): -6670..-6679
-6670 constant E-IR-SYM-STATE     \ a symbol store failed its header, row-shape, or byte-span recheck
-6671 constant E-IR-SYM-OWNER     \ a module key, symbol-id, or store pairing this interner does not own
-6672 constant E-IR-SYM-BOUND     \ a symbol-id whose ordinal is at or past the interned count
-6673 constant E-IR-SYM-CAP       \ a symbol capacity outside the accepted range, or an intern past it
-6674 constant E-IR-SYM-BYTES     \ a byte capacity outside the accepted range, or bytes past the committed ceiling
-6675 constant E-IR-SYM-LEN       \ a negative symbol byte length at intern
-6676 constant E-IR-SYM-RANGE     \ an invalid or undersized destination span for symbol bytes or identities

\ Compiler type table (package IR-TYPE): -6680..-6690
-6680 constant E-IR-TYPE-STATE    \ a type store failed its header, row-shape, span, or stored-reference recheck
-6681 constant E-IR-TYPE-OWNER    \ a module key, type-id, or store pairing this type table does not own
-6682 constant E-IR-TYPE-BOUND    \ a type reference at or past the constructed count: forward references are impossible
-6683 constant E-IR-TYPE-CAP      \ a row capacity outside the accepted range, or a construction past it
-6684 constant E-IR-TYPE-LIST     \ a list-pool capacity outside the accepted range, or a function-type list past it
-6685 constant E-IR-TYPE-SCALAR   \ a scalar width/signedness combination no dialect defines
-6686 constant E-IR-TYPE-TARGET   \ a float format or address space the bound target contract rejects
-6687 constant E-IR-TYPE-STAGE    \ function-type stage misuse: an end without a begin, or a begin while one is open
-6688 constant E-IR-TYPE-ARITY    \ a staged parameter or result list past the committed arity ceiling
-6689 constant E-IR-TYPE-KIND     \ a field reader applied to a type of a different kind
-6690 constant E-IR-TYPE-RANGE    \ a render span smaller than the rendered type text

\ Compiler attribute table (package IR-ATTR): -6691..-6699, completing the
\ -6600..-6699 shared compiler IR block.
-6691 constant E-IR-ATTR-STATE    \ an attribute store failed its header, row shape, or stored symbol check
-6692 constant E-IR-ATTR-OWNER    \ a module key, attr-id, or symbol id this attribute table does not own
-6693 constant E-IR-ATTR-BOUND    \ an attribute id at or past the constructed count
-6694 constant E-IR-ATTR-CAP      \ an attribute capacity outside the accepted range or a new row past the ceiling
-6695 constant E-IR-ATTR-KIND     \ a field reader applied to an attribute of a different kind

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

\ Compiler growth region: -8000..-8999, the compiler's second block: -6600..-6699
\ is full and -6700..-6799 is the frozen identity schema below it. Sub-blocks are
\ 20 codes wide, each headed below by its range; a stage that outgrows its own
\ takes the next one no `-8NNN constant` under src, tools or test claims.
-8000 constant E-COMP-FIRST
-8999 constant E-COMP-LAST

\ Compiler dialect schema records (package IR-SCHEMA): -8000..-8019
-8000 constant E-IR-SCHEMA-STATE   \ a schema store failed its header, row-shape, window, or stored-code recheck
-8001 constant E-IR-SCHEMA-OWNER   \ a module key, symbol id, type id, or store pairing this schema table does not own
-8002 constant E-IR-SCHEMA-BOUND   \ a schema list index at or past the length the row records
-8003 constant E-IR-SCHEMA-CAP     \ a row or pool capacity outside the accepted range, or a definition past a committed ceiling
-8004 constant E-IR-SCHEMA-STAGE   \ schema-stage misuse: a begin while one is open, an end without a begin, or a field declared twice
-8005 constant E-IR-SCHEMA-FIELD   \ a required schema field was never declared before the definition closed
-8006 constant E-IR-SCHEMA-ARITY   \ an operand, result, successor, or region count outside its ceiling, or a variadic tail that is not last
-8007 constant E-IR-SCHEMA-EFFECT  \ an effect class, domain, memory space, and alias combination no operation can have
-8008 constant E-IR-SCHEMA-TERM    \ a terminator rule violated: successors on a non-terminator, or results on a terminator
-8009 constant E-IR-SCHEMA-TARGET  \ required target capabilities the bound contract does not provide
-8010 constant E-IR-SCHEMA-DUP     \ an opcode name this dialect's schema table already defines
-8011 constant E-IR-SCHEMA-OPCODE  \ an opcode name no schema in this dialect's table defines
-8012 constant E-IR-SCHEMA-KIND    \ a field reader applied to a schema whose effect shape does not carry that field
-8014 constant E-IR-SCHEMA-VERSION \ a schema major or minor version outside the accepted range
-8015 constant E-IR-SCHEMA-TIE     \ a tied operand declaration naming no fixed entry, a mismatched type, or a field tied twice
-8016 constant E-IR-SCHEMA-FIXED   \ a fixed-register declaration naming no fixed entry of its side's list, a field fixed twice, or a register no file numbers

\ Compiler operation and value store (package IR-OP): -8020..-8039
-8020 constant E-IR-OP-STATE     \ an operation store failed its header, row-shape, window, or stored-code recheck
-8021 constant E-IR-OP-OWNER     \ a module key, symbol, value, type, attribute, block id, or store pairing this table does not own
-8022 constant E-IR-OP-BOUND     \ an operation id, value id, or list index at or past the count the table records
-8023 constant E-IR-OP-CAP       \ a row or pool capacity outside the accepted range, or an append past a committed ceiling
-8024 constant E-IR-OP-STAGE     \ operation-stage misuse: a begin while one is open, an end without a begin, or a field declared twice
-8025 constant E-IR-OP-FIELD     \ a required operation field was never declared before the append closed
-8026 constant E-IR-OP-ARITY     \ an operand, result, or successor count the opcode's schema does not allow, or a staged list past its ceiling
-8027 constant E-IR-OP-SSA       \ an operand naming a value that is not already defined; every forward reference and cycle lands here
-8028 constant E-IR-OP-WINDOW    \ a stored window that does not continue exactly where the record before it ended
-8029 constant E-IR-OP-KIND      \ a field reader applied to a value whose definition kind does not carry that field

\ Compiler function and block structure (package IR-FUN): -8040..-8059
-8040 constant E-IR-FUN-STATE    \ a function or block store failed its header, row-shape, window, or stored-code recheck
-8041 constant E-IR-FUN-OWNER    \ a module key, symbol, type, attribute, value, block, operation id, or store pairing this table does not own
-8042 constant E-IR-FUN-BOUND    \ a function id, block id, or list index at or past the count the table records
-8043 constant E-IR-FUN-CAP      \ a row or pool capacity outside the accepted range, or an append past a committed ceiling
-8044 constant E-IR-FUN-STAGE    \ function or block stage misuse: a begin while one is open, an end without a begin, a field declared twice, or an end out of nesting order
-8045 constant E-IR-FUN-FIELD    \ a required function or block field was never declared before the append closed
-8046 constant E-IR-FUN-WINDOW   \ a stored window that does not continue exactly where the record before it ended
-8047 constant E-IR-FUN-PARENT   \ a block whose parent function is not the function whose block window names it
-8048 constant E-IR-FUN-ARG      \ a value in a block's argument window that is not that block's argument at that position
-8049 constant E-IR-FUN-TERM     \ a block that does not end in exactly one terminator operation
-8050 constant E-IR-FUN-LINKAGE  \ a linkage, visibility, and body combination no function can have
-8051 constant E-IR-FUN-DUP      \ a symbol this module's function table already defines
-8052 constant E-IR-FUN-SIG      \ a function signature type that is not a code reference
-8053 constant E-IR-FUN-TARGET   \ a calling convention the bound target contract does not provide

\ Compiler builder, abort, and freeze lifecycle (package IR-BUILD): -8060..-8079
-8060 constant E-IR-BUILD-STALE   \ a builder or frozen-module handle this registry never minted, or whose owning context has torn down
-8061 constant E-IR-BUILD-FROZEN  \ a mutation, a reader, or a second freeze through a builder handle FREEZE already consumed
-8062 constant E-IR-BUILD-ABORTED \ any use of a builder handle ABORT already retired
-8063 constant E-IR-BUILD-OWNER   \ a builder presented together with a context that is not the one that created it
-8064 constant E-IR-BUILD-OPEN    \ a freeze attempted while this builder still holds an open function, block, or operation stage
-8065 constant E-IR-BUILD-STAGE   \ builder stage misuse: a begin while one is open, an end without a begin, or a stage another builder holds
-8066 constant E-IR-BUILD-PLAN    \ a table-ceiling plan field outside the accepted range, declared twice, or missing when a builder was created
-8067 constant E-IR-BUILD-CEILING \ a table holds more records than the ceiling this builder committed to
-8068 constant E-IR-BUILD-SLOTS   \ the builder registry has no free slot
-8069 constant E-IR-BUILD-SERIALS \ builder generation serials reached their ceiling
-8070 constant E-IR-BUILD-STATE   \ persisted builder registry state failed its consistency recheck
-8071 constant E-IR-BUILD-RETIRED \ any use of a frozen-module handle RETIRE already gave back: the pass that superseded this module released its tables, so every view and index it minted is stale

\ Compiler structural freeze verification (package IR-VERIFY): -8080..-8099
-8080 constant E-IR-VERIFY-STATE   \ a derived edge store failed its header, row-shape, or window recheck
-8081 constant E-IR-VERIFY-OWNER   \ a table or module key presented to the verifier that this module does not own
-8082 constant E-IR-VERIFY-BOUND   \ a stored identity or window index at or past the count its table records
-8083 constant E-IR-VERIFY-CAP     \ a module with more blocks or control-flow edges than the verifier's committed working set
-8084 constant E-IR-VERIFY-COVER   \ an operation outside every block window, or a block outside every function window
-8085 constant E-IR-VERIFY-PARENT  \ a block and the function whose window claims it disagree about the parent
-8086 constant E-IR-VERIFY-TERM    \ a block that does not end in exactly one terminator operation
-8087 constant E-IR-VERIFY-SUCC    \ a successor naming a block this module never defined
-8088 constant E-IR-VERIFY-SUCCARG \ successor arguments that do not match the destination block's arguments in count or type
-8089 constant E-IR-VERIFY-DEF     \ a value whose defining operation does not claim it at that result position
-8090 constant E-IR-VERIFY-ARGDEF  \ a block-argument value whose block does not exist or whose argument window does not contain it
-8091 constant E-IR-VERIFY-DOM     \ an operand naming a value whose defining block does not dominate the using block
-8092 constant E-IR-VERIFY-SCOPE   \ an operand naming a value defined in another function
-8093 constant E-IR-VERIFY-OPTYPE  \ an operand whose value type is not the type the opcode's schema declares
-8094 constant E-IR-VERIFY-RESTYPE \ a result whose declared type is not the type the opcode's schema declares
-8095 constant E-IR-VERIFY-ATTRKEY \ a required attribute key the operation omits, a key repeated, or an undeclared key on an opcode without the extension set
-8096 constant E-IR-VERIFY-EFFECT  \ an operation whose effect class disagrees with the opcode's schema
-8097 constant E-IR-VERIFY-SYMBOL  \ a symbol a record names that this module's interner never interned, or a function name defined twice
-8098 constant E-IR-VERIFY-SPAN    \ a source span that is not a valid slice of a registered source
-8099 constant E-IR-VERIFY-TARGET  \ an operation the context's bound target contract cannot execute

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


\ Native stage N0 source tape (package NTAPE): -8240..-8259
\
\ Byte spans are IR-SOURCE's and reject with E-IR-SRC-SPAN, so no span code is
\ minted here.
-8240 constant E-NTAPE-STATE    \ a tape arena failed its header or row-shape recheck
-8241 constant E-NTAPE-OWNER    \ a module key or an identity presented to a tape that does not own it
-8242 constant E-NTAPE-BOUND    \ a token ordinal at or past the count the tape records
-8243 constant E-NTAPE-CAP      \ a tape capacity outside the accepted range, or an append past it
-8244 constant E-NTAPE-KIND     \ a token kind outside the vocabulary, or a reader applied to a kind that has no such field
-8245 constant E-NTAPE-MODE     \ a stored parser mode outside the vocabulary
-8246 constant E-NTAPE-LITERAL  \ a literal value a token of that kind may not carry
-8247 constant E-NTAPE-ORIGIN   \ an expansion parent that is not an already-appended token of this tape
-8248 constant E-NTAPE-ROOT     \ an origin read on a directly lexed token, which has none

\ -8220..-8239 unassigned: E-HIR-UNMODELED refuses what checked source may
\ compile a word into.

\ Native source fixtures and code probes: -8260..-8279
-8261 constant E-CODEGEN-PROBE-SUBJECT    \ a named subject word is not in the live dictionary
-8267 constant E-NSRC-CAP                 \ source text longer than the chain fixture's text buffer
\ A word's dictionary record states the span its callers may copy, which is
\ everything before its trailing return - or, for a routine that leaves by a
\ branch, the whole of it. A reader that wants the code's real extent has to put
\ the return back, and it is only entitled to do that when the return is there.
-8273 constant E-CODEGEN-PROBE-EXTENT     \ a word's record neither ends in a tail branch nor is followed by the return the engine's record convention puts after it

\ Native stage N1 straight-line HIR dialect (package HIR): -8280..-8299; NELAB
\ and T0 both throw -8294.
\
\ The straight-line subset: the closed opcode set the dialect registers and the
\ source-word model. Schema records are IR-SCHEMA's and reject with its codes.
-8280 constant E-HIR-STATE      \ a word-model arena failed its header, row-shape, window, or stored-code recheck
-8281 constant E-HIR-OWNER      \ a module key or symbol presented to a word model that does not own it
-8282 constant E-HIR-BOUND      \ a row index or a pick index at or past the count the table records
-8283 constant E-HIR-CAP        \ a row or pick-pool capacity outside the accepted range, or a declaration past it
-8284 constant E-HIR-CLASS      \ a stored meaning outside the vocabulary, or a reader or declarer applied to a meaning it does not serve
-8285 constant E-HIR-DUP        \ a source word this word model already models
-8286 constant E-HIR-UNMODELED  \ a source word this dialect cannot compile: unmodeled, or never declared at all
-8287 constant E-HIR-KIND       \ a source-tape token kind the straight-line subset does not model
-8288 constant E-HIR-STAGE      \ rename-staging misuse: a begin while one is open, a pick without a begin, or an end without a begin
-8289 constant E-HIR-PICK       \ a stack rename that picks an input it never consumed, or an input or output count outside its ceiling; or a CAST: whose input and output cell counts differ or are zero
-8290 constant E-HIR-OPCODE     \ a word bound to an opcode this dialect's schema table does not define
-8291 constant E-HIR-DIALECT    \ a module whose schema table was created for another dialect or another schema version
-8292 constant E-HIR-ADDR       \ a relocation kind for a literal that this dialect's hir.addr does not name
-8293 constant E-HIR-VOCAB      \ a frozen module asked for a vocabulary entry it does not hold: it froze before HIR:ENSURE-VOCABULARY made the whole vocabulary present
-8294 constant E-UNMODELED-IMMEDIATE  \ a body uses an immediate that is not parse-neutral and no native tier models: refused at its token, it never runs

\ Native stage N1 straight-line elaborator (package NELAB): -8300..-8319
\
\ What the elaborator asks another authority stays that authority's refusal: a
\ word the dialect cannot compile is E-HIR-UNMODELED, a token kind the subset
\ does not model is E-HIR-KIND, a malformed operation is IR-OP's. -8302 is
\ unassigned.
-8300 constant E-NELAB-SHAPE     \ a tape that is not one colon definition: no tokens at all, or a first token that is not the defined name
-8301 constant E-NELAB-MODE      \ a definition token consumed in the wrong parser mode for its place: the name outside interpreting, or a body word outside compiling
-8303 constant E-NELAB-ARITY     \ a declared input or output count outside the accepted range, or a body that does not leave exactly the declared outputs
-8304 constant E-NELAB-UNDER     \ a word consuming more values than the compile-time value vector holds
-8305 constant E-NELAB-CAP       \ a compile-time value vector past the elaborator's ceiling

\ Native ARM64 register allocation (package A64RA): -8320..-8329
\
\ Refusals another authority owns keep its name: a register no routine may hold
\ state in is NEFF's E-NEFF-GPR, a malformed module IR-OP's or IR-VERIFY's.
-8320 constant E-A64RA-BIND      \ allocation attempted before the dialect's module and type identities were bound, or a second binding over a live one
-8321 constant E-A64RA-MODULE    \ a frozen module that is not the bound one
-8322 constant E-A64RA-STATE     \ an allocation reader used before the walk sealed one, or after a later walk replaced it
-8323 constant E-A64RA-SHAPE     \ a module this leaf cannot allocate: no function at all, no block control leaves through or more than one, a successor ordinal outside the function, or a value of the module that no function defines
-8324 constant E-A64RA-FRAME     \ a function after the first needs spill slots, and a module holds one frame. A module's first function is the definition's own routine and the rest are its quotation bodies; the frame, its base and its slot count are one per module (N-SLOTS, BASE-N, and the reserve sized from A64RA:FRAME), so a body that will not fit its values in registers has nowhere to put them. Every quotation body in the tree is one word call and spills nothing, which is why this is a refusal and not a second frame.
-8325 constant E-A64RA-CLASS     \ a value whose type is not the dialect's general-register type, so no general register can hold it
-8326 constant E-A64RA-TARGET    \ a context bound to a target these registers do not belong to
-8327 constant E-A64RA-CAP       \ a value, block or plan ordinal outside the allocator's tables: more of them in one routine than the tables hold, or a read past the count the sealed walk recorded
-8328 constant E-A64RA-TIE       \ a schema-declared tie that cannot be honoured: the kept value is still needed afterwards, or its register is not free
-8329 constant E-A64RA-PRESSURE  \ more values have to be spilled at once than ANY frame of this chain holds slots for: the demand passed the tighter of what the architecture can describe and what a pass can name a slot of. The allocator derives the frame from what the walk needs (A64RA:FRAME). Register pressure itself is a spill, not a refusal; the one operation that no spill can serve is E-A64RA-POOL

\ Native ARM64 allocation validator (package A64RAV): -8330..-8339
\
\ It re-derives every live interval from the frozen module rather than reading
\ the allocator's belief.
-8330 constant E-A64RAV-STATE     \ acceptance asked for with no sealed allocation, or an accepted answer read after a later walk replaced it
-8331 constant E-A64RAV-MODULE    \ a module that is not the one the allocation was made from
-8332 constant E-A64RAV-CONTRACT  \ a routine contract that is not the one the allocation was made under
-8333 constant E-A64RAV-INTERVAL  \ a recorded live interval that is not the one the module's own definitions and uses give
-8334 constant E-A64RAV-COVER     \ a value of the block with no assignment, or an assignment for a value the block does not hold
-8335 constant E-A64RAV-REGISTER  \ an assigned register outside the set the routine contract says it may destroy
-8336 constant E-A64RAV-OVERLAP   \ two values live at the same time assigned the same register
-8337 constant E-A64RAV-CLASS     \ a value whose type is not the dialect's general-register type
-8338 constant E-A64RAV-TIE       \ a schema-declared tie the assignment breaks: the tied result and operand are not the same register
-8339 constant E-A64RAV-SHAPE     \ a module with no function at all, or a function with more than one block control leaves through, or a successor ordinal outside it, re-derived rather than taken from the allocator

\ Native ARM64 machine dialect (package A64IR): -8340..-8359
\
\ A malformed A64IR operation is IR-SCHEMA's or IR-OP's; these are the facts only
\ the dialect knows.
-8340 constant E-A64IR-DIALECT  \ a module whose schema table was created for another dialect or another schema version
-8341 constant E-A64IR-IMM      \ a move-wide immediate outside the sixteen-bit field the form holds
-8342 constant E-A64IR-SHIFT    \ a move-wide shift that does not select one of the four halves of a general register
-8343 constant E-A64IR-SLOT     \ a frame-slot offset the memory forms cannot address: negative, not a multiple of the eight bytes they move, or past the reach of their scaled twelve-bit offset field
-8344 constant E-A64IR-FRAME    \ a reserved frame size no routine can declare: negative, not a multiple of the stack alignment, or past the deepest frame the offset field can reach
-8345 constant E-A64IR-OFF      \ an add or subtract immediate outside the unsigned twelve-bit field the form holds: negative, or past the largest value that field carries with no shift
-8346 constant E-A64IR-MASK     \ a mask the logical immediate forms cannot carry: the field holds a rotated contiguous run of ones repeating at a power-of-two element width, which excludes zero, all ones, and every run that is broken
-8347 constant E-A64IR-FUN      \ a function ordinal the address-of-a-function form cannot name: negative, so it denotes no function of any emission. WHICH emission it must be a function of is the emitter's fact and not the dialect's, and is refused there
-8348 constant E-A64IR-OPCODE   \ an ordinal outside the machine dialect's closed opcode vocabulary
-8349 constant E-A64IR-DATA     \ a DATA offset outside the mapping or its resulting address beyond 48 bits

\ Native ARM64 instruction selection (package A64SEL): -8360..-8379
\
\ Refusals another authority owns keep its name: an out-of-range move-wide
\ operand is E-A64IR-IMM or E-A64IR-SHIFT, a malformed operation IR-OP's.
-8360 constant E-A64SEL-BIND    \ selection attempted before the source dialect's opcode identities were bound, or a second binding over a live one
-8361 constant E-A64SEL-SOURCE  \ a frozen module that is not the bound one, or source text whose digest is not the one the module recorded
-8362 constant E-A64SEL-OPCODE  \ an operation whose opcode is none of the source dialect's
-8363 constant E-A64SEL-SHAPE   \ a module or function this leaf cannot select: not exactly one source, or a function with no block at all
-8364 constant E-A64SEL-TRAP    \ an operation whose schema says it may trap: trapping arithmetic has no machine lowering in this dialect yet
-8365 constant E-A64SEL-ATTR    \ a constant whose attribute is not under the source dialect's value key
-8366 constant E-A64SEL-CAP     \ more values in one function than the selector's value map holds

\ Native ARM64 instruction emission (package A64EMIT): -8380..-8399
\
\ Refusals another authority owns keep its name: a register answered from a
\ stale acceptance is E-A64RAV-STATE, an operand or attribute value outside its
\ field is src/arch/arm64/asm.f's.
-8380 constant E-A64EMIT-STATE   \ a reader asked about an emission that has not happened, or that a later refused run replaced
-8381 constant E-A64EMIT-BIND    \ emission attempted before the machine dialect's identities were bound, or a second binding over a live one
-8382 constant E-A64EMIT-MODULE  \ a frozen module that is not the bound one, or a builder of another dialect or schema version
-8383 constant E-A64EMIT-ALLOC   \ no accepted register assignment at all, or one accepted for a different module
-8384 constant E-A64EMIT-SHAPE   \ not the straight-line subset: no function at all, a function ordinal outside the module, or a block whose only return is not its last operation
-8385 constant E-A64EMIT-OPCODE  \ an operation whose opcode is none of the machine dialect's family
-8386 constant E-A64EMIT-ATTR    \ a move-wide operation that carries no attribute under the key the dialect declares for it
-8387 constant E-A64EMIT-CAP     \ more instructions in one block than the emission buffers hold
-8388 constant E-A64EMIT-TARGET  \ emission under a context bound to a machine these instructions are not for
-8389 constant E-A64EMIT-BOUND   \ an instruction or source-map index at or past the count the sealed emission holds
-8390 constant E-A64EMIT-LAYOUT  \ the instructions written and the instructions the layout counted disagree: the writer reached a block at an offset the layout did not put it at, or ended the routine at a different length
-8391 constant E-A64EMIT-BODY    \ how many instructions of an emission are its body, asked about a routine that calls: a call site publishes and takes back through the very data-stack forms a routine's own crossings use, so the emission less its crossings would not be that routine's body
-8392 constant E-A64EMIT-ADDR    \ an address chain this emission cannot record: a movn claiming to carry an address, a run of lanes that is not exactly the carrier's width in one register, or more chains than the site table holds

\ Native stage N0 source-tape producer (package NFEED): -8400..-8409
\
\ Fills a source tape from the checker's reader. Refusals another authority owns
\ keep its name: a full tape is E-NTAPE-CAP, a span outside its source
\ IR-SOURCE's.
-8400 constant E-NFEED-STATE    \ a producer word reached in a state that has no meaning for it: no unit open, a unit still scanning, or a second unit over a live one
-8401 constant E-NFEED-SCAN     \ a foreign scan or an unexpected defining-word segment reached the open unit
-8402 constant E-NFEED-SPAN     \ a token whose bytes are not the bytes at the offset it claims, or that leaves the scanned text
-8403 constant E-NFEED-ORDER    \ an append answered an ordinal other than the next one: the tape gained a row this producer did not write
-8404 constant E-NFEED-KIND     \ a token class this stage has no tape kind for
-8405 constant E-NFEED-LITERAL  \ an integer literal whose value this stage cannot read back
-8406 constant E-NFEED-TEXT     \ a scan whose text is longer than the buffer the unit was opened with
-8407 constant E-NFEED-CUT      \ the does> cut the engine recorded is not just past a `does> ` in the definition source: it lies outside the source, or after other bytes - the source and the pending definition disagree

\ Native ARM64 spill lowering and frame slots: -8440..-8459
\
\ The spill-lowering pass (package A64SPILL) and the frame-slot refusals of A64RA
\ and A64RAV. Refusals another authority owns keep its name: a slot no load or
\ store form can reach is NEFF's E-NEFF-SLOT, a slot offset outside the
\ dialect's field E-A64IR-SLOT, a malformed module IR-OP's.
-8440 constant E-A64SPILL-BIND   \ a rewrite attempted before the machine dialect's identities were bound, or a second binding over a live one
-8441 constant E-A64SPILL-PLAN   \ no sealed spill plan at all, one made for another module, or one a later allocation replaced
-8442 constant E-A64SPILL-SHAPE  \ a module this pass cannot rewrite: no function at all, a function whose frame operations are neither nothing nor exactly a selector's prologue, an empty block, a span naming another source, or a value used before it is defined
-8444 constant E-A64SPILL-OPCODE \ an operation whose opcode is none of the machine dialect's family, so this pass has no form to rebuild it as
-8445 constant E-A64SPILL-CAP    \ more values or more inserted operations in one block than the rewriter's tables hold

\ The allocator's second refusal, and the validator's frame-slot ones.
-8446 constant E-A64RA-POOL      \ one operation needs more registers at a single instant than the routine may destroy, and every register holds a value that same operation needs: no spill can free one
-8447 constant E-A64RAV-SLOT     \ a frame access whose slot the module and the contract disagree about: a slot the routine cannot address, or one no allocation claimed
-8448 constant E-A64RAV-SHARE    \ two values live at the same time stored in one frame slot
-8449 constant E-A64RAV-RELOAD   \ a reload reading a slot nothing stored to before it, or a slot whose last store carried another value
-8450 constant E-A64RAV-FRAME    \ a module whose reserved frame is not the frame the routine contract declares, or whose frame is reserved or released somewhere it cannot be

\ Native ARM64 fixed-register binding (packages A64RA and A64RAV): -8460..-8479
\
\ NEFF owns the declaration of a routine's argument and result registers and
\ refuses one that cannot be a convention; these are whether the allocation can
\ honour it and whether the finished assignment does.
-8460 constant E-A64RA-FIXED     \ a declared argument, result or schema-fixed operand register the allocation cannot honour: more declared positions than the routine has arguments or returned values, a register outside the set the routine may write, one value returned at two declared positions, one class declared into two registers, or a declared register already held where it has to be taken
-8461 constant E-A64RAV-FIXED    \ an assignment that does not put a declared argument, returned value or schema-fixed operand or result where the contract or the form declares it, or one made for a contract declaring more positions than the module has

\ Native ARM64 data-stack calling convention: -8480..-8499
\
\ A position of a calling convention is a PLACE: a register or a data-stack
\ slot. Refusals another authority owns keep its name: a non-canonical place
\ list is NEFF's E-NEFF-SEQ, a register no routine may hold state in E-NEFF-GPR,
\ an operand outside its field the assembler's.
-8480 constant E-NEFF-KIND    \ a place read as the kind it is not: the register of a data-stack slot, or the slot index of a register
-8481 constant E-A64IR-DSLOT    \ a data-stack slot offset the load and store forms cannot address: negative, not a multiple of the eight bytes they move, or past the reach of their scaled twelve-bit offset field
-8482 constant E-A64IR-DBYTES   \ a data-stack pointer adjustment no routine can make: negative, not a multiple of one stack cell, or past the twelve-bit immediate that claims it
-8483 constant E-A64SEL-PLACE   \ a calling convention this selector has no rule for: one side mixing register places with data-stack places
-8484 constant E-A64RA-PLACE    \ a place list the allocation cannot honour: one side mixing register places with data-stack places, or data-stack places declared on a module whose interface is still block arguments or terminator operands
-8485 constant E-A64RAV-PLACE   \ the same, re-derived: a mixed side, or data-stack places on a module that still carries its interface as block arguments or terminator operands
-8486 constant E-A64RAV-DSTACK  \ a data-stack entry or exit sequence that is not the one the contract declares: a missing or misplaced adjustment, a wrong byte count, a load or store naming a slot no place declares, or a module that reaches both the data stack and a frame
-8487 constant E-A64IR-DWB      \ a pointer move a transfer cannot carry in its own encoding: nothing at all, which is the plain access and not a fused one, not a multiple of one stack cell, or past the magnitude the nine-bit signed writeback field holds in either sign

\ Native ARM64 control flow: -8500..-8519
\
\ A refusal another authority owns keeps its name: a successor naming a block no
\ function defines is IR-VERIFY's E-IR-VERIFY-SUCC, a branch operand outside its
\ field the assembler's.
-8500 constant E-A64IR-COND     \ a condition code the compare form cannot carry: outside the four-bit field the conditional forms encode it in
-8501 constant E-HIR-CONTROL    \ a source word read as a control word when its row models something else, or a stored control code outside this dialect's vocabulary
-8502 constant E-NELAB-CTRL     \ a control structure the elaborator cannot close: a closer with no opener, a closer that does not match the opener it meets, a word that stands in the middle of a structure with no such structure open, a second one of those over one structure, a `while` after a word that already left the definition, a loop closed by a word that does not open the block its `while`s branch out to, or a body that ends with a structure still open
-8503 constant E-NELAB-JOIN     \ two paths into one join that do not agree: arms leaving different numbers of values on the compile-time value stack, two `while`s leaving one loop at different depths, a loop body that does not leave the stack as its header takes it, an edge whose width disagrees with the width the first edge into that block stated, a block opened with arguments that no edge into it stated the types of, or a checked body's result cells grouped otherwise than the row it declares (a `TRUSTED:` body's declared row is the assertion, and its cells take that grouping)
-8505 constant E-A64EMIT-BLOCK  \ a branch whose successor names no block of the function this emission laid out
-8506 constant E-A64EMIT-REACH  \ a branch whose displacement does not fit the field its form encodes it in
-8504 constant E-NELAB-BLOCK    \ more blocks or more open control structures in one definition than the elaborator's tables hold
-8507 constant E-A64RA-EDGE     \ a block argument and the values handed to it across an edge cannot share one register: two of them are live at the same time, or the edge hands over a different number of values than the destination takes
-8508 constant E-A64RA-SPILL    \ a routine needs a value put in a frame slot and has none this pass may take: every class holding a register there is one an edge or a schema tie made of more than one value, holds a block argument, is read by a data-stack operation, or reaches the frame from a block that is neither the one the caller enters nor the one control leaves through
-8509 constant E-A64RAV-EDGE    \ the same edge rule, re-derived: an assignment in which the register holding a terminator's operand is not the register holding the successor's block argument at that position

\ Native ARM64 memory access: -8520..-8539
\
\ The source dialect's ordering value and computed-address load and store, and
\ the machine dialect's two forms. A refusal another authority owns keeps its
\ name: a source word the dialect cannot compile is E-HIR-UNMODELED, a row read
\ as a meaning it does not carry E-HIR-CLASS, an address operand outside its
\ field the assembler's.
-8519 constant E-NELAB-BUNDLE   \ a multi-cell value the elaborator would have to take apart or would have to guess the shape of: one cell of such a value parked on the return stack, where the cells would be separated with every count still agreeing; or a declared row whose values the chain cannot place exactly (NDICT:GLUE-UNKNOWN), which is a segmentation it may not guess at; or a `!` through a pointer to a multi-cell family whose value on that vector does not begin, hold together and end where the certified width says, which is the same segmentation met from the writing side. It is also what a WIDTH on an operation that is not a memory access raises, which would mean the chain and the checker had read one token as two different words. A CAST:'s regrouping raises it for a window of cells that starts inside a value, which checked source cannot reach; and a scan certificate that says its width facts are incomplete (checker.f SCAN-INCOMPLETE: a token the scan could not apply while a multi-cell value was live) raises it before anything lowers, since the chain would have to guess where that value's cells lie
-8520 constant E-NELAB-TOKEN    \ the definition's one memory order is wrong for the operation being staged: either the operation's schema declares more than one token among its operands or among its results, which the elaborator has no rule for, or it takes an order in a definition the pre-scan found no memory word in
-8521 constant E-A64SEL-MEM     \ a memory operation in a routine whose convention names no data-stack place at all: the generic memory order of this dialect begins where the routine takes the caller's operands, and a routine that takes and publishes nothing has no beginning for it
-8522 constant E-A64RAV-ORDER   \ a memory order the module mints and does not pass on exactly once: a token nothing reads leaves the order after it unstated, and a token two operations read is two orders claiming to be one

\ Native typed locals: -8540..-8549
\
\ Only the elaborator sees a local, so no other package gains a code here. A
\ rebinding or an address-of by name is E-HIR-UNMODELED. A name the dialect
\ already models is not refused: a local means the local, as in the engine. The
\ only name refused is the quotation's closer, whose spans are found before the
\ locals frame exists.
-8540 constant E-NELAB-LOCAL     \ a locals declaration this elaborator has no rule for: a group opened inside another group or inside a control structure, a group left unclosed, a closer with no opener, a declared name the quotation pre-scan reads before there is a locals frame to consult (`;]`), the same name declared twice, a token in the group that is not a name, a body whose locals differ in number from its certified binds, or locals on a direct tape with no certificate
-8541 constant E-NELAB-LOCAL-CAP \ more locals, or more locals groups, in one definition than the elaborator's tables hold

\ Native calls: -8550..-8559
\
\ `RECURSE`'s callee contract destroys exactly the registers the allocator hands
\ out, so the caller's live values cross the call on its data stack and its
\ return address in the routine's frame. A word this dialect's table never
\ declared is E-HIR-UNMODELED.
\
\ A CALL TO ANOTHER WORD ADDS THREE FACTS: the callee's arity (the word model's
\ row), its entry address (the machine dialect's field), and the address the
\ CALLING routine will be written at, the publication seam's answer, measured
\ against the seam's own claim before a byte is written.
-8550 constant E-NELAB-CALL     \ a call the elaborator has no rule for: fewer values on the compile-time vector than the callee declares it takes, more values live across the call than the vector can hold afterwards, or a call reached with no memory order live
-8551 constant E-A64SEL-CALL    \ a call the selector has no lowering for. The one cause a production contract can reach: a routine declared under the register convention, which never took the data-stack pointer a call site hands its arguments over through. The other four hold two derivations of one fact against each other and no contract src/compiler/native/abi.f builds can reach them, so a run that reports one is a defect in the chain rather than a program it cannot compile: a frame too small to hold the saved link register, a contract declaring a call in a module that contains none, a module containing a call under a contract that declares none, or a call whose operand and result lists disagree about how many values are live across it
-8552 constant E-A64RAV-CALL    \ a call site that is not the sequence the dialect lowers a call to: a store run that does not name slots zero upwards in order, a byte count that is not the run it stands for, or a saved link register the module does not restore from the slot it saved it into
-8553 constant E-A64RAV-OWNER   \ a frame slot given to the wrong owner: a spilled value placed in the slot a calling routine keeps its caller's return address in, or a prologue access naming a slot that is not that one
-8554 constant E-HIR-CALLEE     \ a callable-word declaration this dialect cannot hold: an entry address that is negative or not instruction aligned, or an argument or result count that is negative
-8555 constant E-A64IR-ENTRY    \ a callee entry address the branch-with-link form cannot name: negative, or not the address of a whole instruction
-8556 constant E-A64EMIT-PLACE  \ an emission that names a callee and was told no address of its own to measure the branch from, a placement that is negative or not instruction aligned, or a second placement declared over a live one
-8557 constant E-A64RAV-CLOBBER \ a value live across a call assigned a register in the caller's allocatable pool, all of which the callee may destroy
-8558 constant E-A64EMIT-CLOBBER \ an emission that writes a general or floating register the accepted allocation claimed for no value

\ Native publication: -8560..-8579
\
\ An accepted emission becomes one pending colon definition's routine.
-8561 constant E-NPUB-ROOM    \ the emission does not fit in what is left of the code arena under the engine's own end reserve
-8562 constant E-NPUB-SIZE    \ an emission that cannot be a word body: no instructions at all, or a byte size that is not a whole number of instructions
-8563 constant E-NPUB-OFFSET  \ an instruction whose source-map offset does not lie inside the emission it belongs to, or that is not instruction aligned
-8564 constant E-NEMIT-STATE  \ a reader asked src/compiler/native/emission.f about rows no backend has sealed, or that its RETIRE row has cleared; rows opened over live ones or added to none; a placement asked of an unplaced emission
-8565 constant E-NEMIT-ROW    \ a row index at or past the count the sealed emission holds, a row offset outside the emission, an empty emission, a trailing return longer than it, or a call site of neither kind
-8566 constant E-NPUB-PLACE   \ an emission whose branches were measured from an address that is not the code slot this seam is claiming for it

-8570 constant E-NCOMP-STATE   \ a compiler entry reached while another one is open, or a checker scan or store write from a callback while a definition compiles (src/core/checker.f WRITE-WINDOW)
-8571 constant E-NCOMP-TEXT    \ definition source longer than the engine capture can hold
-8572 constant E-NCOMP-VERDICT \ the engine's own check did not certify the pending definition
-8573 constant E-NCOMP-NAME    \ the pending record and the checker's tape name different definitions
-8574 constant E-NCOMP-OWNER   \ the checker that owns the source carries no operation for a front-end step this compiler needs: a replacement checker published a declaration-owner record without it, so the scan, the source tape, the does> split or a call fact has nowhere to come from
-8575 constant E-NCOMP-BINDING \ no successful scan or resolved binding exists for a required call ordinal
-8576 constant E-NCOMP-RECOVERY \ the active checker counted a refusal or borrowed its recovery fact; abandon the unpublished definition and continue
-8579 constant E-NCOMP-ARITY   \ the checker holds no declared effect for the pending definition

\ The float subset: -8580..-8589
\
\ A double lives in one unboxed cell, so only its type separates it from an
\ integer. A refusal another authority owns keeps its name: a double type on a
\ machine with no floating unit is IR-TYPE's E-IR-TYPE-TARGET, a float literal
\ spelling the tape cannot read back E-NFEED-LITERAL, an operand type its schema
\ does not declare IR-OP's.
-8580 constant E-NELAB-TYPE     \ a compile-time value whose type is not the one the position wants and is not a crossing this dialect performs: a double handed to an operation that computes with cells, or a double stored into a memory cell, which no leaf places yet
-8581 constant E-A64RA-FILE     \ two values of two different register files joined into one class: an edge or a schema tie whose two ends cannot share a register because no register holds both

\ Native branch decoding and unit artifacts
-8597 constant E-NUNIT-PROFILE  \ package state or relocation that this native artifact format cannot represent
-8598 constant E-NBR-RANGE      \ a branch-with-link whose two addresses are not both whole instructions, or whose displacement does not fit the twenty-six-bit field the form encodes it in
-8599 constant E-NUNIT-FILE     \ malformed or incompatible native package artifact

\ Data-stack residency: -8610..-8619
\
\ A value the routine only hands on stays in the caller's data-stack slot it
\ arrived in. The selector computes which slot holds which value and the
\ allocation validator derives it AGAIN from the module; these are the second
\ derivation's refusals. Whether the slot holds the value the program meant is
\ the selector's; this validator does not check it.
\
\ Asking the running dictionary about a spelling: -8600..-8609
\
\ src/compiler/native/dict.f walks the engine's resolution order for a name and,
\ for a data word or a constant, takes what the record it found leaves. Both
\ refusals are the question having no answer.
-8600 constant E-NDICT-NAME     \ a spelling that denotes no word where the definition naming it is compiled: the open package's two wordlists, the global wordlist, the used publics and the namespace record for a qualified token all answer absent
-8601 constant E-NDICT-VALUE    \ the word a spelling denoted did not leave exactly one value where it was entered, so it is not a word whose whole meaning is the value it pushes and what it left is not its answer
-8602 constant E-NDICT-KIND     \ the record a spelling denoted carries no definer kind: it was not made by `constant`, `create` or `variable`, or a `does>` clause took that kind away from it, so entering it would run a body rather than collect a decided value

-8610 constant E-A64RAV-DRES    \ a data-stack slot published with no store in front of it that no path defines: an omitted store for a slot nothing ever wrote, one a call between the write and the publication destroyed, or a read of such a slot - and, fail-closed rather than reachable, a residency descent that was still moving after every cell of the map could have fallen as far as it can
-8611 constant E-A64RAV-DKEEP   \ a data-stack access the emission had no reason to make: a store writing a slot that already holds that very value, a load of a slot whose value is already in a register, or a load whose result nothing reads

\ Leaving a routine through its last callee: -8620..-8629
\
\ A routine whose final call hands back exactly what it hands back branches to
\ the callee instead. The elaborator and the contract decide which routines, and
\ src/compiler/native/select.f derives it again; this is the second
\ derivation's refusal, about the SITE: NEFF refuses a contract declaring both a
\ tail call and a destroyed link register, and
\ src/compiler/native/regalloc-verify.f a module whose terminator and contract
\ disagree.
-8620 constant E-A64SEL-TAIL    \ a call the selector was told to leave through that it cannot: a value live across it, a callee whose arity is not this routine's own, a data-stack adjustment the branch would have had to carry, or a contract declaring a tail call over a module with no such site in it - and the other way round, a site built under a contract that declares an ordinary return
-8621 constant E-NPUB-RELOC   \ an emission that leaves through a branch to an address outside the code region: the snapshot relocation record can only describe a branch-with-link, so such a branch would survive a restore holding the writing run's displacement
-8622 constant E-NPUB-PENDING \ the pending record is not the unpublished slot the dictionary count points at
-8623 constant E-NPUB-TARGET  \ a sealed emission for another machine than the engine publishing it runs on: the code region holds only that machine's instructions, so another's bytes are refused before anything about the record is asked

\ Native emission and string storage: -8635..-8639
\ A multiply-add with the zero register as addend encodes a different operation.
-8635 constant E-A64EMIT-ADDEND  \ a multiply-add whose addend register is the zero register: that word is a plain multiply, so emitting one would encode a different operation
-8636 constant E-NSTR-CAP       \ a string literal body larger than one segment's arena (512 KB), or one a stripped link copies into its window's closed pool past the room that pool kept (src/compiler/native/string.f WINDOW-CLOSE): a pool opens another segment for any other body its last one cannot hold. The addresses it has already answered are compiled into published routines, so a body it cannot hold is a refusal rather than a reused address
-8637 constant E-NSTR-BODY      \ a string literal of negative length, which no reader produces and no arena can hold

\ The counted-loop closed form: -8660..-8669
\
\ src/compiler/native/loop.f rewrites a counted loop whose whole body accumulates
\ an affine function of the index into the arithmetic it would compute. A loop
\ it cannot PROVE closed-formable is not an error (the scan answers zero and the
\ module passes untouched), so every code here is about input the pass cannot
\ read.
-8660 constant E-NLOOP-BIND     \ a rewrite attempted before the source dialect's identities were bound, or a second binding over a live one
-8661 constant E-NLOOP-SHAPE    \ a module this pass cannot rewrite: a span naming another source, a value used before it is defined, or a value of a type this dialect does not declare
-8663 constant E-NLOOP-OPCODE   \ an operation whose opcode is none of the source dialect's family, so this pass has no form to rebuild it as
-8664 constant E-NLOOP-CAP      \ more values, blocks or invariant addends in one function than the rewriter's tables hold
-8665 constant E-NLOOP-PLAN     \ a rewrite asked for when the scan had recognised no loop, so there is nothing to fold and the caller and this pass disagree about what was measured

\ The register-file description, the machine description over it and the
\ dialect's vocabulary beside them: -8670..-8672
\
\ src/compiler/native/regfile.f is the one declaration a backend makes about its
\ register files. Its checked constructor is the single moment it can be wrong,
\ so it has a single code and nothing downstream re-judges it.
-8670 constant E-NREGFILE       \ a register-file description that cannot describe a machine: a file of no registers or more than one set's bit mask holds, a reserved register outside the file it is reserved from, a file whose every register is reserved so nothing is allocatable at all, a call-destroyed register that is not allocatable in the first place, or a spill slot width that is not a positive power of two
\ src/compiler/native/machine.f adds the facts a routine contract needs over the
\ register files, with the same single moment and single code.
-8671 constant E-NMACH          \ a machine description that cannot describe a machine: a link register set holding more than one register, a link register or a stack pointer that is not a register of the general file or that the file says is allocatable, a stack alignment that is not a positive power of two, a frame bound that is negative or not a multiple of that alignment, or a negative offset-field reach
\ src/compiler/native/dialect.f is the vocabulary a machine dialect hands the
\ register allocator, built whole or not at all: it is wrong only when a reader
\ asks for a key the dialect declared absent.
-8672 constant E-NDIALECT       \ a dialect's vocabulary asked for a key it declared absent - x86-64 has no write-back addressing and interns no symbol for one - so there is no symbol to answer with and naming another key would be worse than saying nothing

\ The shadow target's routines: -8680..-8689
\
\ src/compiler/native/shadow.f keeps the routine a definition compiles to for a
\ second machine and the map from each published record to it. Refusals of the
\ shadow chain's stages and of publication keep their own names.
-8680 constant E-NSHADOW-STATE  \ a shadow opened over an open one or natively with no address rewriter installed, or its binding, an emission or a row asked of none
-8681 constant E-NSHADOW-ROW    \ a record, emission or row index at or past what the shadow holds, or a `does>` clause function the taken emission does not have or that starts where the definer enters
-8682 constant E-NSHADOW-TARGET \ an emission taken for another machine than the one the shadow's binding names

\ The trap terminator's message: -8640..-8649
\
\ src/compiler/native/trap.f owns the routine every compiled trap branches to and
\ the message each trap site carries.
-8641 constant E-NTRAP-NAME     \ an empty family or callee name, which no trap message could name
-8643 constant E-A64SEL-ORDER   \ a block whose edges disagree about the memory order it is entered with, or that no edge reaches at all. The machine two-way branch carries no operands, so an order two paths differ on cannot be handed over as an argument here; a source that needs one says so among the block's own arguments

\ The tag-dispatch forms: -8650..-8659
\
\ `MATCH … ;MATCH`, `case … endcase` and `construct`: the elaborator reads their
\ operands with a pre-pass that mirrors the checker's token machine. A form the
\ checker refuses never reaches the chain, so a refusal here means the chain and
\ the checker read one body differently.
\ A quotation body is a second FUNCTION of the module. Every quotation shape the
\ elaborator cannot take apart is one refusal, distinct from E-HIR-UNMODELED so
\ a census tells a declined quotation from an unknown word.
-8651 constant E-NELAB-QUOT     \ a quotation this elaborator has no rule for: a `[:` the body never closes; a `;]` with no `[:` open; a quotation nothing consumes (a cast whose input is a quotation does), or one two consumers reach because its value was duplicated; a body whose declared effect is not an ordinary routine's; or a body holding a locals group

\ `is NAME` stores into the defer's dispatch cell, whose address is read from the
\ record's meta trailer. The trailer's magic is checked: a record that is not a
\ defer's has whatever follows its code there.
-8653 constant E-NELAB-DEFER    \ an `is` this elaborator cannot bind: no token after it, a token after it that is not a name, a name that denotes no word where the definition is compiled, a record whose meta trailer does not carry the defer magic, or a deferred word whose declared effect the checker cannot state in cells

\ src/compiler/native/elaborate.f QROW-CK throws it for a quotation row outside
\ the definition's QN rows.
-8652 constant E-NELAB-QUOT-CAP \ more quotation bodies in one definition than a module of the native chain holds functions for

-8650 constant E-NELAB-MATCH    \ a tag-dispatch form this elaborator cannot shape: a family or variant token the registry declines, a form whose operand token is missing or is not a name, an `of`, `endof`, `endcase` or `;match` with no such form open, an arm reached with fewer values on the compile-time vector than the form consumes, a scrutinee whose bundle on that vector does not begin, hold together and end where its family's width says - which is what an instantiation wider than the declared one looks like from here - or an arm whose payload has several FIELDS in more CELLS, where no per-field width says where its values begin

\ Native x86-64 machine dialect (package X64IR): -8740..-8759.
\
\ Operand bounds are the x86-64 fields: a signed 32-bit immediate, disp32, a
\ six-bit shift count and a four-bit tttn condition. The relocatable literal has
\ no immediate refusal because `mov r64, imm64` holds every cell, so the ARM64
\ move-wide refusals have no counterpart.
-8740 constant E-X64IR-DIALECT  \ a module whose schema table was created for another dialect or another schema version
-8741 constant E-X64IR-OPCODE   \ an ordinal outside the machine dialect's closed opcode vocabulary
-8742 constant E-X64IR-IMM      \ an immediate outside the signed thirty-two bits the ALU and compare forms hold
-8743 constant E-X64IR-SHIFT    \ a shift count outside the six bits the machine masks a shift to
-8744 constant E-X64IR-SLOT     \ a frame-slot offset the memory forms cannot address: negative, not a multiple of the eight bytes they move, or past the reach of their signed thirty-two bit displacement
-8745 constant E-X64IR-FRAME    \ a reserved frame size no routine can declare: negative, not a multiple of the stack alignment, or past the deepest frame this dialect describes
-8746 constant E-X64IR-DSLOT    \ a data-stack slot offset the load and store forms cannot address: past the reach behind the pointer, not a multiple of one stack cell, or past the displacement field
-8747 constant E-X64IR-DBYTES   \ a data-stack pointer adjustment no routine can make: not a multiple of one stack cell, or past the immediate that claims it
-8748 constant E-X64IR-COND     \ a condition code the conditional forms cannot carry: outside the four-bit field Jcc and SETcc encode it in
-8749 constant E-X64IR-FUN      \ a function ordinal the address-of-a-function form cannot name: negative, so it denotes no function of any emission
-8750 constant E-X64IR-ENTRY    \ a callee entry address the call form cannot name: zero or negative, so it is the address of no emitted routine

\ Native x86-64 instruction selection (package X64SEL): -8760..-8779.
\
\ The first eleven answer the ARM64 selector's refusals on this machine. The last
\ has no ARM64 counterpart: a contract carries the machine it is about. -8772 and
\ -8773 unassigned.
-8760 constant E-X64SEL-BIND    \ selection attempted before the source dialect's opcode identities were bound, or a second binding over a live one
-8761 constant E-X64SEL-SOURCE  \ a frozen module that is not the bound one, or a builder holding a dialect that is not HIR
-8762 constant E-X64SEL-OPCODE  \ an operation whose opcode is none of the source dialect's
-8763 constant E-X64SEL-SHAPE   \ a module or function this pass cannot select: not exactly one source, or a function with no block at all
-8764 constant E-X64SEL-TRAP    \ an operation whose schema says it may trap lowered to a form that does not reproduce the trap - trapping arithmetic has no x86-64 lowering - or a trap or terminal call in a routine whose convention names no data-stack place
-8765 constant E-X64SEL-ATTR    \ an operation whose attribute is not under the source dialect's key for it
-8766 constant E-X64SEL-CAP     \ more values, blocks or edge operands in one function than this pass's maps hold
-8767 constant E-X64SEL-PLACE   \ a calling convention this selector has no rule for: one side mixing register places with data-stack places, or a function whose arity is not the one the contract declares
-8768 constant E-X64SEL-MEM     \ a memory operation in a routine whose convention names no data-stack place at all, so the generic memory order of this dialect has no beginning
-8769 constant E-X64SEL-CALL    \ a call this selector has no lowering for: a routine declared under the register convention, which never took the data-stack pointer a call site hands its arguments over through, a contract declaring a call in a module that contains none, a module containing a call under a contract that declares none, or a call whose operand and result lists disagree about how many values are live across it
-8770 constant E-X64SEL-TAIL    \ a call the selector was told to leave through that it cannot: a value live across it, a callee whose arity is not this routine's own, a data-stack adjustment the branch would have had to carry, or a contract and a module that disagree about whether there is such a site
-8771 constant E-X64SEL-ORDER   \ a block whose edges disagree about the memory order it is entered with, or that no edge reaches at all
-8774 constant E-X64SEL-MACHINE \ a routine contract whose machine is not the one this backend lowers for

\ Native x86-64 instruction emission (package X64EMIT): -8780..-8799.
\
\ The emitter owns its byte sink and places an emission at a slot the caller
\ names. Besides the forms every emitter refuses, it refuses an emission out of
\ order (bound, placed, sealed, retired), a cursor that disagrees with the
\ measured start and a displacement no rel32 holds.
-8780 constant E-X64EMIT-FORM   \ an operation this emitter does not render: a negate, which no selection reaches, or a double comparison whose condition and result count are not `gt` with one result or `equal` with two
-8781 constant E-X64EMIT-SHAPE  \ a module this emitter cannot write: no function at all, a function ordinal outside the module, an empty block, a block that does not end in a terminator, or a terminator that is not its block's last operation
-8782 constant E-X64EMIT-ACCEPT \ no accepted register assignment at all, or one accepted for a different module
-8783 constant E-X64EMIT-MODULE \ emission before the machine dialect's identities were bound, a frozen module that is not the bound one, or a builder of another dialect or schema version
-8784 constant E-X64EMIT-ATTR   \ an operation carrying no attribute under the key the dialect declares for its form
-8785 constant E-X64EMIT-OPCODE \ an operation whose opcode is none of the machine dialect's family
-8786 constant E-X64EMIT-STATE  \ a reader asked about an emission that is not sealed: none has been written since the last retire, or the last run stopped before it sealed
-8787 constant E-X64EMIT-PLACE  \ a placement that is not a slot this machine's routines start at: a second placement over a live one, a negative slot, or one off the machine's stack alignment
-8788 constant E-X64EMIT-LAYOUT \ the bytes written and the bytes the layout measured disagree: the writer reached a block or a function at an offset the measuring pass did not put it at, or ended the emission at a different length
-8789 constant E-X64EMIT-BOUND  \ a block, function or address-site index at or past the count the sealed emission holds
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
\ -9270..-9279. -9240..-9249 is tools/image-size-lib.f and -9260..-9269
\ tools/manifest-lint-core.f.
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
-9292 constant E-AIO-STATE        \ no loop running, a second START, or a ticket this task does not own
-9293 constant E-AIO-FULL         \ the kernel's io_uring ring is full: no submission entry free, or as many operations in flight as the completion ring holds
-9294 constant E-AIO-BUSY         \ STOP with an operation still in flight
-9295 constant E-AIO-GROUP        \ the group is empty, holds no such ticket, or names no defined group
-9296 constant E-AIO-BOUNDS       \ a transfer count past its allocation or below zero, a file offset below -1, or a socket address length not positive
-9297 constant E-AIO-POLL-BOUND   \ Darwin's poll(2) takes at most OPEN_MAX (10240) descriptors per call, and one more would be needed

\ Image lifecycle hooks (package IMAGE-LIFECYCLE, src/lib/image-lifecycle.f):
\ -9310..-9319. -9300..-9309 is src/habu/debug.f's.
-9310 constant E-LIFECYCLE-FIRST
-9319 constant E-LIFECYCLE-LAST
-9310 constant E-LIFECYCLE-LATE   \ a persistent hook registered a one-shot hook during PREPARE, after the one-shot phase: nothing runs it before the capture

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
-9340 constant E-HTTP-STATE       \ START on a running server, STOP on a stopped one, a hook installed while one runs, a second STATIC-ROOT, a TAKE-OVER outside a running handler or a second one inside it, BEARER:ISSUER!, BEARER:METADATA! or TEST-ISSUER:CLIENT+ while serving, BEARER:ADMIT? before ISSUER!, or BEARER:TOKEN before CLIENT!
-9341 constant E-HTTP-HANDLE      \ a request or response handle past its request, or one over another worker's slot, or a slot asked by a task that is no worker
-9342 constant E-HTTP-WORKERS     \ a worker count outside 1..MAX-WORKERS, or an idle deadline under 1 ms
-9343 constant E-HTTP-CAPACITY    \ more hooks, headers or bound segments than a table holds, or a negative body length
-9344 constant E-HTTP-ROUTE       \ more routes than the table holds, or an empty method or a pattern not starting with /
-9345 constant E-HTTP-STATIC      \ a bad or non-directory static root, a URL wider than PATH-CAP, or a directory cycle
-9346 constant E-HTTP-SOCKET      \ the listener could not be bound, put to listen or asked its own address
\ -9347 unassigned: a response grows instead of refusing.

\ Base64 (package BASE64): -9350..-9359.
-9350 constant E-BASE64-FIRST
-9359 constant E-BASE64-LAST
-9350 constant E-BASE64-CHAR      \ a byte outside the selected alphabet
-9351 constant E-BASE64-PAD       \ misplaced/forbidden '=', or nonzero unused bits
-9352 constant E-BASE64-LENGTH    \ padded length not divisible by four, or URL remainder one

\ Private C2 owner-frame runtime.
-9360 constant E-C2-CAPACITY       \ this task has no owner frame left
-9361 constant E-C2-STATE          \ a frame operation contradicts its transition state
-9364 constant E-C2-CAPTURE        \ capture saw an active owner or loan in main

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
-9416 constant E-WS-WAKE          \ START could not open its wake pipe
-9417 constant E-WS-LIMIT         \ a WS:START limit below 1 or past the handle's 32-bit slot field

\ Native host facts: -9440..-9449, minted by src/compiler/native/host.f,
\ which owns package NHOST.
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
\ Tier 0 emitter seam: -9590..-9599, minted in src/habu/t0-emit.f, which owns
\ package T0-EMIT.
\ A task to each job: -9620..-9629, minted in src/lib/crew.f, which owns
\ package CREW.
\ OAuth bearer tokens: -9630..-9639, minted in src/lib/net/bearer.f, which owns
\ package BEARER.
\ Model chat and embedding client: -9700..-9799, minted in src/lib/net/model.f,
\ which owns package MODEL.

\ Wasm back end: -9800..-9834, minted in src/arch/wasm/: WLEB -9800..-9804, WSTRUCT
\ -9805..-9809, WCTL -9815..-9819, WSEL -9820..-9824, WENC -9825..-9829,
\ WLINK -9830..-9834.

\ T0 (src/habu/t0.f): -9600..-9609, minted here, not in package T0, because
\ src/compiler/native/definition.f MALFORMED? reads them.
-9600 constant E-T0-FIRST
-9609 constant E-T0-LAST
-9600 constant E-T0-UNSUPPORTED  \ a token T0 does not compile: T0:UNSUPPORTED$
-9601 constant E-T0-CONTROL      \ a control word without its opener, a body ending inside one, or a token no path reaches: T0:UNSUPPORTED$
-9604 constant E-T0-BINDS        \ the body binds other than its scan's count of locals
-9606 constant E-T0-MATCH        \ a MATCH or construct the checker's facts do not bear out, as tier 1's E-NELAB-MATCH: T0:UNSUPPORTED$
-9607 constant E-T0-DEFER        \ `is` names no deferred word: T0:UNSUPPORTED$
-9608 constant E-T0-BUNDLE       \ an operation over a width it cannot move, as tier 1's E-NELAB-BUNDLE

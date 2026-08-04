\ errors.f - canonical stdlib throw codes.
\
\ A library owns an inclusive block of codes, usually 100 wide. FIRST and LAST
\ are the block's two bounds, and they reserve the whole range between them for
\ the file that declares them, whether or not every code in it has been minted
\ yet: tools/error-code-lint.f reports any other file that claims a code inside
\ someone else's declared range. A subsystem that outgrows one block takes a
\ second, separately named region rather than renumbering the codes it already
\ ships; the compiler at -6600..-6699 and -8000..-8999 is the first such case.

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
-2104 constant E-FS-DEPTH
-2105 constant E-FS-IO
-2106 constant E-FS-CAPACITY
-2107 constant E-FS-PATH-UNSAFE

\ Strings: -2200..-2299
-2200 constant E-STR-FIRST
-2299 constant E-STR-LAST
-2200 constant E-STR-BOUNDS
-2201 constant E-STR-CAPACITY

\ Regex: -2300..-2399
-2300 constant E-RX-FIRST
-2399 constant E-RX-LAST
-2300 constant E-RX-SYNTAX
-2301 constant E-RX-CAPACITY

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

\ Diagnostics: -2900..-2999
-2900 constant E-DIAG-FIRST
-2999 constant E-DIAG-LAST
-2900 constant E-DIAG-SCHEMA
-2901 constant E-DIAG-CAPACITY
-2902 constant E-DIAG-ORIGIN

\ Tables: -3000..-3099
-3000 constant E-TBL-FIRST
-3099 constant E-TBL-LAST
-3000 constant E-TBL-BOUNDS
-3001 constant E-TBL-FIELD

\ JSON writer: -3100..-3199
-3100 constant E-JW-FIRST
-3199 constant E-JW-LAST
-3100 constant E-JW-CAPACITY
-3101 constant E-JW-BYTE

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
-3402 constant E-PTX-NOIMPL   \ tile op typed but not yet lowered to PTX (codegen = M4e)
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
-3414 constant E-PTX-OPT-OVERFLOW  \ PTX optimizer line/symbol/output arena overflowed
-3415 constant E-PTX-OPT-SYNTAX    \ PTX optimizer parse invariant violated (should be fail-closed opaque)
-3416 constant E-KABI-CAP          \ kernel-ABI record capacity (params/fields/name pool) exceeded
-3417 constant E-KABI-FIELD        \ kernel-ABI field: unknown name/index, or no .param offset
-3418 constant E-KABI-DUP          \ kernel-ABI duplicate field name / conflicting extent-token source
-3419 constant E-KABI-TOKEN        \ kernel-ABI empty or oversized name/extent token
-3420 constant E-KEXPORT-KERNEL    \ kernel-export: unknown kernel name / record-name mismatch
-3421 constant E-KEXPORT-OUTDIR    \ kernel-export: out-dir missing or not a directory
-3422 constant E-KEXPORT-EMPTY     \ kernel-export: producer emitted no PTX
-3423 constant E-PTXTC-ARCH        \ ASSEMBLE invoked before TC-ARCH! set the assembler target
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

\ Tasking/threads: -3600..-3699
-3600 constant E-TASK-FIRST
-3699 constant E-TASK-LAST
-3600 constant E-TASK-SIZE
-3601 constant E-TASK-DLOPEN
-3602 constant E-TASK-DLSYM
-3603 constant E-TASK-THREAD
-3604 constant E-TASK-STATE
-3605 constant E-TASK-USER

\ Object/linker records: -3700..-3799
-3700 constant E-OBJ-FIRST
-3799 constant E-OBJ-LAST
-3700 constant E-OBJ-SCHEMA
-3701 constant E-OBJ-CAPACITY
-3702 constant E-OBJ-FIELD

\ Engine self-identity: -3800..-3899
-3800 constant E-ENGINE-FIRST
-3899 constant E-ENGINE-LAST
-3800 constant E-ENGINE-PATH    \ own executable path cannot be resolved
-3801 constant E-ENGINE-KEY     \ own binary cannot be content-hashed

\ JSON reader: -3900..-3999
-3900 constant E-JR-FIRST
-3999 constant E-JR-LAST
-3900 constant E-JR-MALFORMED   \ value expected but got a bare word / bad char / trailing comma
-3901 constant E-JR-STRING      \ unterminated string or unescaped control byte in a string
-3902 constant E-JR-ESCAPE      \ invalid backslash or \uXXXX escape sequence
-3903 constant E-JR-SURROGATE   \ lone or mismatched UTF-16 surrogate in a \u escape
-3904 constant E-JR-DEPTH       \ container nesting exceeds JR-MAX-DEPTH
-3905 constant E-JR-TRAILING    \ non-whitespace after the top-level value
-3906 constant E-JR-NUMBER      \ malformed number token or integer overflow
-3907 constant E-JR-EOF         \ input ended while a value/container/key was expected
-3908 constant E-JR-COLON       \ missing ':' after an object key
-3909 constant E-JR-COMMA       \ expected ',' or the matching container close
-3910 constant E-JR-STATE       \ accessor called on the wrong token kind or dst buffer too small
-3911 constant E-JR-BOUNDS      \ source-cursor read past the input buffer (internal invariant)

package JR

public

-3912 constant E-CAPACITY    \ caller storage is smaller than JR:STORAGE-BYTES
-3913 constant E-STORAGE     \ caller storage is null or not cell-aligned
-3914 constant E-SOURCE      \ source length is negative or a positive length has a null source

;package

\ Remote device harness (ssh zed): -4000..-4099
-4000 constant E-ZED-FIRST
-4099 constant E-ZED-LAST
-4000 constant E-ZED-UNREACH     \ ssh could not reach the host (connect/auth)
-4001 constant E-ZED-RC          \ remote command exited nonzero
-4002 constant E-ZED-PUT         \ scp/rsync artifact transfer failed
-4003 constant E-ZED-TOOLCHAIN   \ required remote tool missing (ptxas/nvcc/...)
-4004 constant E-ZED-ARG         \ bad harness argument or buffer capacity
-4005 constant E-ZED-DISABLED    \ device required but HABU_ZED unset/0
-4006 constant E-ZED-TIMEOUT     \ remote command exceeded the timeout
-4007 constant E-ZED-EMIT        \ local artifact emit (bin/hb spawn) failed

\ Source-composition discovery: -4100..-4199 (merge renumber from -3800; E-ENGINE owns -3800)
-4100 constant E-DISC-FIRST
-4199 constant E-DISC-LAST
-4100 constant E-DISC-SHADOW
-4101 constant E-DISC-DYNAMIC
-4102 constant E-DISC-OPENER
-4103 constant E-DISC-UNTERM
-4104 constant E-DISC-CAPACITY
-4105 constant E-DISC-RETIRE

\ Test-suite DSL: -4200..-4299 (merge renumber from -3900; E-JR owns -3900)
-4200 constant E-SUITE-FIRST
-4299 constant E-SUITE-LAST
-4200 constant E-SUITE-MODE   \ GROUP mode token missing or not SEQ/PARA
-4201 constant E-SUITE-NAME   \ GROUP name missing or a reserved DSL keyword

\ Evaluation grader: -4300..-4399
-4300 constant E-EVAL-FIRST
-4399 constant E-EVAL-LAST
-4300 constant E-EVAL-EMIT    \ candidate emit infrastructure failed
-4301 constant E-EVAL-VOCAB   \ judge image lacks a task-required word

\ Test infrastructure: -4400..-4499
-4400 constant E-TEST-FIRST
-4499 constant E-TEST-LAST
-4400 constant E-TEST-CAPACITY

\ Report tables: -4900..-4999 (-4500..-4899 are owned by non-stdlib test/tool blocks)
-4900 constant E-REPORT-FIRST
-4999 constant E-REPORT-LAST
-4900 constant E-REPORT-CAPACITY   \ column set is full (more than 64 columns declared)

\ Owned growable byte buffer: -5700..-5799 (the stdlib range -2000..-4999 is full;
\ -5000..-5699 are owned by research/maki modules that keep codes in their own files)
-5700 constant E-BUF-FIRST
-5799 constant E-BUF-LAST
-5700 constant E-BUF-BOUNDS      \ byte offset / active length outside the buffer
-5701 constant E-BUF-CAPACITY    \ zero, negative, or cell-overflowing byte capacity
-5702 constant E-BUF-STATE       \ touch of a disposed buffer, or re-init of a live one

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

\ JSON reader performance samples: -6300..-6399
-6300 constant E-JRP-FIRST
-6399 constant E-JRP-LAST
-6300 constant E-JRP-SAMPLE      \ sample stored out of workload order or past the declared total
-6301 constant E-JRP-RANGE       \ workload or sample index outside the sample table

\ JSON reader performance phase: -6400..-6499
-6400 constant E-JRPP-FIRST
-6499 constant E-JRPP-LAST
-6400 constant E-JRPP-CHILD      \ the ratchet phase was started inside a gate-pool fork worker
-6401 constant E-JRPP-BUSY       \ the ratchet phase was started with pool workers still in flight
\ -6402 was E-JRPP-DRIFT, thrown when the calibration bracket moved twice. A
\ drifted bracket is now an inadmissible measurement rather than a failure: the
\ phase re-measures and, if the box never goes quiet, leaves by its own exit
\ status (JSON-READ-PERF-PHASE CONTENDED-RC) instead of throwing.
-6403 constant E-JRPP-MISMATCH   \ a phase fixture read a value other than the one it required
-6404 constant E-JRPP-REPEAT     \ the ratchet phase was started twice in one gate process

\ Native test runner (package TEST): -6500..-6599
-6500 constant E-TR-FIRST
-6599 constant E-TR-LAST
-6500 constant E-TR-PATH-LEN    \ a persistent-root path longer than the runner's buffer
-6501 constant E-TR-PROFILE     \ a host-profile id outside the runner's known profiles

\ Shared compiler IR representation: -6600..-6699. This block is FULL and
\ closed: the attribute table took the last free code (-6699). Later compiler
\ stages mint from the compiler growth region at -8000..-8999, declared at the
\ end of this file. The codes below keep their exact values forever; tests and
\ diagnostics already name them.
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

\ Compiler numerical policy (package CNUM): -6620..-6629
-6620 constant E-CNUM-CONTRACT    \ contraction is allowed under a bit-exact rewrite licence

\ Compiler target/policy binding (package CBIND): -6630..-6639
-6630 constant E-CBIND-CONTRACT   \ contraction is allowed but the target has no fused multiply-add
-6631 constant E-CBIND-PREIMAGE   \ a component preimage does not fit the binding preimage buffer

\ Compiler context ownership (package IR-CTX): -6640..-6649
-6640 constant E-IR-CTX-STALE     \ a context handle was used after its context tore down
-6641 constant E-IR-CTX-DEPTH     \ context nesting exceeded the registry capacity
-6642 constant E-IR-CTX-UNBOUND   \ a context slot whose owning module has not landed was used
-6643 constant E-IR-CTX-SERIALS   \ serial allocation reached its committed ceiling
-6644 constant E-IR-CTX-SCRATCH   \ a scratch request does not fit the context mapping
-6645 constant E-IR-CTX-CEILING   \ a context-creation serial ceiling outside the accepted range
-6646 constant E-IR-CTX-SIZE      \ a scratch request of zero or negative bytes
-6647 constant E-IR-CTX-STATE     \ persisted context state failed its consistency recheck

\ Compiler IR arena (package IR-ARENA): -6650..-6659
-6650 constant E-IR-ARENA-STALE   \ an arena handle was used after abort or after its context's teardown
-6651 constant E-IR-ARENA-OWNER   \ an index, mark, or context was presented to an arena that did not mint it
-6652 constant E-IR-ARENA-FULL    \ an append reached the arena's committed cell ceiling
-6653 constant E-IR-ARENA-FROZEN  \ a mutation word ran against a frozen arena
-6654 constant E-IR-ARENA-MARK    \ a rollback target outside the arena's live appended range
-6655 constant E-IR-ARENA-BOUND   \ an arena index at or past the readable cell count
-6656 constant E-IR-ARENA-CEIL    \ an arena-creation cell ceiling outside the accepted range
-6657 constant E-IR-ARENA-SLOTS   \ the live-arena registry has no free slot
-6658 constant E-IR-ARENA-SERIALS \ arena generation serials reached their ceiling
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
-6676 constant E-IR-SYM-RANGE     \ a destination span smaller than the symbol being copied

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
-6691 constant E-IR-ATTR-STATE    \ an attribute store failed its header, row-shape, payload, window, or stored-reference recheck
-6692 constant E-IR-ATTR-OWNER    \ a module key, attr-id, referenced id, or store pairing this attribute table does not own
-6693 constant E-IR-ATTR-BOUND    \ an attribute reference or element index at or past the constructed count
-6694 constant E-IR-ATTR-CAP      \ a row or pool capacity outside the accepted range, or a construction past a committed ceiling
-6695 constant E-IR-ATTR-KIND     \ a field reader applied to an attribute of a different kind or enum family
-6696 constant E-IR-ATTR-STAGE    \ staged-list misuse: a wrong or missing begin/end pairing, or a staged list past the ceiling
-6697 constant E-IR-ATTR-VALUE    \ a malformed payload: a negative string length or a duplicate record key
-6698 constant E-IR-ATTR-TARGET   \ an enum attribute stating a target fact the bound contract contradicts
-6699 constant E-IR-ATTR-RANGE    \ a destination span smaller than the copied or rendered text

\ Frozen compiler identity schema (package COMPILER-ID-PROOF): -6700..-6799
-6700 constant E-CID-FIRST
-6799 constant E-CID-LAST
-6700 constant E-CID-FAMILY   \ the live checker type-family registry does not match the frozen identity family manifest
-6701 constant E-CID-TOKEN    \ a canonical manifest token holds a byte outside the printable non-space alphabet
-6702 constant E-CID-ROW      \ a schema vector row index or table slot is outside its declared table
-6703 constant E-CID-LEX      \ the shared source lexer refused the identity source it was asked to read
-6704 constant E-CID-DEF      \ a named identity definition is missing, duplicated, or unterminated in that source
-6705 constant E-CID-CONST    \ a named identity constant is missing, duplicated, or not a literal in that source
-6706 constant E-CID-ROCQ     \ the generated Rocq obligation run did not complete cleanly
-6707 constant E-CID-AXIOM    \ a proof-manifest row is malformed, claims an assumption a gate holds at empty, or does not match the assumption set Rocq reported (package PROOF-MANIFEST, shared by both parity gates)
-6708 constant E-CID-REPLAY   \ a require replay handed back a module identity the allocator had already issued

\ Frozen compiler interning parity gate (package COMPILER-INTERN-PROOF):
\ -6800..-6819. The neighbouring block, because this gate is the interning twin
\ of the identity gate above: it binds formal/Common/Interning.v to the three
\ interners in src/compiler/ir. Its manifest failures are the shared
\ E-CID-AXIOM, because both gates read one manifest grammar. Twenty codes; the
\ next block, -6820..-6839, is the structure gate below.
\ rest of -6860..-6999 stays unclaimed.
-6800 constant E-CIN-FIRST
-6819 constant E-CIN-LAST
-6800 constant E-CIN-ROW      \ a shared interning vector index or table slot is outside its declared table
-6801 constant E-CIN-STRUCT   \ a frozen interner structure - a compared-field list, a check-before-write ordering, or a reference guard - is not what the source carries

\ Frozen compiler structure parity gate (package COMPILER-STRUCT-PROOF):
\ -6820..-6839. The next block after the interning gate, because this gate is
\ the third of the same family: it binds formal/Common/Structure.v to the
\ operation and value store and the function and block store in
\ src/compiler/ir. Its manifest failures are the shared E-CID-AXIOM, because
\ all three gates read one manifest grammar. Twenty codes; the rest of
\ -6840..-6999 stays unclaimed.
-6820 constant E-CIS-FIRST
-6839 constant E-CIS-LAST
-6820 constant E-CIS-ROW      \ a shared structure vector index or table slot is outside its declared table
-6821 constant E-CIS-STRUCT   \ a frozen structure guard - the strictly-below operand rule, a window tiling check, a call-closed guard row, or the derived terminator field - is not what the source carries

\ Frozen compiler storage and lifetime parity gate (package COMPILER-STORE-PROOF):
\ -6820..-6839. The third gate in this run of three: it binds
\ formal/Common/Storage.v to the arena and context modules the whole compiler IR
\ rests on, src/compiler/ir/arena.f and src/compiler/ir/context.f. Like its two
\ neighbours its manifest failures are the shared E-CID-AXIOM, because all three
\ gates read one manifest grammar. Twenty codes.
-6840 constant E-CST-FIRST
-6859 constant E-CST-LAST
-6840 constant E-CST-ROW      \ a shared storage vector index or table slot is outside its declared table
-6841 constant E-CST-STRUCT   \ a frozen storage structure - a pinned capacity constant, a check-before-write ordering, or a lifetime guard body - is not what the source carries

\ Frozen checker model parity gate (package CHECKER-MODEL-PROOF): -6860..-6879.
\ The next block after the storage gate, and the fifth of the same family: it
\ binds formal/Common/Effects.v and formal/Common/Control.v to the checker
\ itself, src/core/checker.f. Like its four neighbours its manifest failures are
\ the shared E-CID-AXIOM, because all five gates read one manifest grammar.
\ Twenty codes; the rest of -6880..-6999 stays unclaimed.
-6860 constant E-CMP-FIRST
-6879 constant E-CMP-LAST
-6860 constant E-CMP-ROW      \ a shared checker-model vector index or table slot is outside its declared table
-6861 constant E-CMP-VOCAB    \ the checker's concrete type vocabulary - a name, code, class, width, or sign row of CT-INIT, or the CC-MAX ceiling - is not what the frozen table carries
-6862 constant E-CMP-CONTROL  \ the checker's control-flow dispatch table or a control frame kind is not what the frozen table carries
-6863 constant E-CMP-POOL     \ the frozen schema's own string pool or table is full

\ Frozen instruction-encoding parity gate (package COMPILER-INSN-PROOF):
\ -6880..-6899. The sixth gate of the same family: it binds
\ formal/Common/Insn.v to the ARM64 assembler the engine emits through,
\ src/arch/arm64/asm.f with its mnemonic and label layers in
\ src/arch/arm64/mnem.f and src/arch/arm64/icode.f. Like its five neighbours
\ its manifest failures are the shared E-CID-AXIOM, because all six gates read
\ one manifest grammar. Twenty codes at -6900..-6919; the relocation gate
\ landed first and holds -6880..-6899; the rest of -6920..-6999 is unclaimed.
-6900 constant E-CIE-FIRST
-6919 constant E-CIE-LAST
-6900 constant E-CIE-ROW      \ a shared instruction vector index or table slot is outside its declared table
-6901 constant E-CIE-FORM     \ an instruction form code no row of the frozen vocabulary names

\ Frozen snapshot relocation parity gate (package RELOC-PROOF): -6880..-6899.
\ The next block after the checker model gate, and the sixth of the same
\ family: it binds formal/Common/Reloc.v to the two emitted relocation passes
\ in src/habu/habu2.f (SNAP-RELOC:EMIT-CALLS and SNAP-RELOC:EMIT-XT), the
\ writer's address-cell canonicalization in src/habu/snap-lib.f, and the band
\ constants in src/habu/layout.f. Like its five neighbours its manifest
\ failures are the shared E-CID-AXIOM, because all six gates read one manifest
\ grammar. Twenty codes; the rest of -6900..-6999 stays unclaimed.
-6880 constant E-CRL-FIRST
-6899 constant E-CRL-LAST
-6880 constant E-CRL-ROW      \ a shared relocation vector index or table slot is outside its declared table
-6881 constant E-CRL-STRUCT   \ a frozen relocation structure - a pinned band constant, a canonicalization body, or the shape of a vector row - is not what the shipped source carries
-6882 constant E-CRL-DECODE   \ the shipped relocation pass carries an instruction, operand, symbol, or label the machine that runs it was never taught
-6883 constant E-CRL-FAULT    \ the shipped relocation pass, run over a synthetic image, touched an address outside every declared segment or ran past its step budget

\ Compiler growth region: -8000..-8999.
\
\ Why a second region. The compiler's first block, -6600..-6699, is full. Ten
\ owners (IR ids, target contract, numeric policy, target binding, context,
\ arena, source registry, symbol interner, type table, attribute table) spent
\ all 100 codes, and the subsystem still has to build dialect schemas,
\ operation and value pools, control structure, the freeze lifecycle, the
\ verifier, canonical tables, the renderer, the codec, pass results, the
\ facade, eight dialect packages, and the native and GPU back ends. The block
\ itself cannot be widened: -6700..-6799 is the frozen identity schema right
\ below it. The next gap, -6800..-6999, was rejected on size: 200 codes is ten
\ sub-blocks, it is fenced in by a codegen test's caller sentinels at
\ -7001/-7002 and the JSON tool at -7100, and owners run on down to -7807, so
\ taking it would put the subsystem back in this exact position within the
\ campaign. -8000..-8999 is the one thousand-code range no source in the tree
\ claims, and it is large enough for every compiler owner that is planned, so
\ the subsystem never has to move again. Nothing in -6600..-6699 is renumbered.
\
\ How to take codes here. Sub-blocks are 20 codes wide, not 10: the type table
\ needed 11 and had to borrow from the attribute table's decade, which is how
\ the first block ran out. A stage that outgrows its 20 codes takes the next
\ unassigned sub-block instead of spilling into its neighbour, and records that
\ here. The map below is the whole agreement between parallel compiler lanes:
\ read your line, mint inside it, and add your names under it.
\
\   -8000..-8019  dialect schema records (package IR-SCHEMA)
\   -8020..-8039  operation, value, operand, result, and successor pools
\   -8040..-8059  function and block structure, parents, and windows
\   -8060..-8079  builder, abort, and freeze lifecycle (package IR-BUILD)
\   -8080..-8099  structural freeze verifier (package IR-VERIFY)
\   -8100..-8119  canonical table reindexing and reference remap (package IR-CANON)
\   -8120..-8139  deterministic renderer (package IR-RENDER) and structural
\                 diff (package IR-DIFF)
\   -8140..-8159  canonical wire frame and digest (package IR-ENCODE), and the
\                 decoder that rebuilds a module from a frame when it lands
\   -8160..-8179  unassigned (pass witnesses retired 2026-07-30: no consumer)
\   -8180..-8199  IR facade assembly and package protection (package IR)
\   -8200..-8219  ARM64 routine machine-effect contracts (package A64EFF)
\   -8220..-8239  native immediate-word contract table (package NIMM)
\   -8240..-8259  native stage N0 source tape (package NTAPE)
\   -8260..-8279  old-vs-new code generator comparison harness
\                 (package CODEGEN-COMPARE)
\   -8280..-8299  native stage N1 straight-line HIR dialect (package HIR)
\   -8300..-8319  native stage N1 straight-line elaborator (package NELAB)
\   -8320..-8339  native ARM64 register allocation (package A64RA) and the
\                 independent allocation validator (package A64RAV)
\   -8340..-8359  native ARM64 machine dialect (package A64IR)
\   -8360..-8379  native ARM64 instruction selection (package A64SEL)
\   -8380..-8399  native ARM64 instruction emission (package A64EMIT)
\   -8400..-8409  native stage N0 source-tape producer (package NFEED)
\   -8410..-8419  unassigned (source-bound checking result retired 2026-07-31:
\                 no consumer)
\   -8440..-8459  native ARM64 spill lowering (package A64SPILL) and the
\                 frame-slot refusals the register allocator (A64RA) and its
\                 validator (A64RAV) gained when their own block filled up
\   -8460..-8479  native ARM64 fixed-register binding: the refusals the register
\                 allocator (A64RA) and its validator (A64RAV) gained for the
\                 argument and result registers a routine contract declares,
\                 when their own blocks were full
\   -8500..-8519  native ARM64 control flow: the compare and branch forms the
\                 machine dialect (A64IR) gained, the control words the
\                 straight-line source-word model (HIR-WORD) gained, and the
\                 block construction the elaborator (NELAB) gained with them
\   -8420..-8439, -8487..-8499, -8520..-8999  unassigned. The remaining dialect packages
\                 (SIR, LIR, and the GPU stages) and the native and GPU back
\                 ends take sub-blocks from here, each named above its codes.
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
-8013 constant E-IR-SCHEMA-DIGEST  \ a presented schema-table digest does not match the recomputed one
-8014 constant E-IR-SCHEMA-VERSION \ a schema major or minor version outside the accepted range
-8015 constant E-IR-SCHEMA-TIE     \ a tied operand declaration naming no fixed entry, a mismatched type, or a field tied twice

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

\ Compiler canonical table order and reference remap (package IR-CANON): -8100..-8119
-8100 constant E-IR-CANON-STATE    \ a canonical table's own store failed its header, magic, or module-binding recheck
-8101 constant E-IR-CANON-OWNER    \ an identity presented to a canonical table that another module minted
-8102 constant E-IR-CANON-BOUND    \ a canonical-stream index or a table ordinal at or past the count this canonical table records
-8103 constant E-IR-CANON-CAP      \ a module with more interned rows, a longer name or string, or a wider keyed list than the canonicalizer's committed working set
-8104 constant E-IR-CANON-ORDER    \ a table whose stored references admit no dependency order, so no canonical numbering exists
-8105 constant E-IR-CANON-STALE    \ a module that is not a live frozen one, or a canonical-table handle this registry never minted or whose owning context has torn down
-8106 constant E-IR-CANON-RELEASED \ any use of a canonical table RELEASE already retired
-8107 constant E-IR-CANON-SLOTS    \ the canonical-table registry has no free slot
-8108 constant E-IR-CANON-SERIALS  \ canonical-table generation serials reached their ceiling

\ Compiler deterministic renderer and structural diff: -8120..-8139
\ The map's renderer sub-block, taken by the two packages that stage is built
\ from: src/compiler/ir/render.f (package IR-RENDER) turns one frozen module into
\ diagnostic text with canonical names, and src/compiler/ir/diff.f (package
\ IR-DIFF) compares two frozen modules and reports what differs. They are split
\ because rendering one module and comparing two modules are different decisions,
\ so the renderer takes -8120..-8129 and the diff takes -8130..-8139.
-8120 constant E-IR-RENDER-STALE   \ a module presented to the renderer that is not a live frozen one
-8121 constant E-IR-RENDER-ROOM    \ a destination byte span shorter than the text the presented module renders to
-8122 constant E-IR-RENDER-CAP     \ a module with more interned rows, a longer name, or a wider keyed list than the renderer's committed working set
-8123 constant E-IR-RENDER-STATE   \ a stored code or table selector outside the vocabulary the renderer spells

-8130 constant E-IR-DIFF-STALE     \ a module presented to the diff that is not a live frozen one
-8131 constant E-IR-DIFF-ROOM      \ a destination byte span shorter than the report the two presented modules differ by
-8132 constant E-IR-DIFF-CAP       \ a module pair with more rows or a wider keyed list than the diff's committed working set
-8133 constant E-IR-DIFF-STATE     \ a stored code outside the vocabulary the diff compares

\ Compiler canonical wire frame and content digest (package IR-ENCODE): -8140..-8159
\ This is the sub-block the map above already reserves for the wire codec and
\ digest, and the renderer and diff stage above owns -8120..-8139. The
\ package that took the block is IR-ENCODE, which is the encoding half of the
\ codec the map named; a decoder that rebuilds a module from a frame is a
\ separate stage and takes the rest of this sub-block when it lands.
-8140 constant E-IR-ENCODE-STATE   \ a presented frame whose leading bytes are not a canonical module frame at all
-8141 constant E-IR-ENCODE-VERSION \ a frame whose format major or minor version this encoder does not read
-8142 constant E-IR-ENCODE-FRAME   \ a frame whose byte length is not exactly its header plus the payload slots its header states
-8143 constant E-IR-ENCODE-CAP     \ a payload past the committed frame ceiling, or a stated canonical row count larger than the payload could hold
-8144 constant E-IR-ENCODE-ROOM    \ a destination byte span shorter than the frame the presented module encodes to
-8145 constant E-IR-ENCODE-BOUND   \ a payload slot index at or past the slot count the frame states

\ ARM64 routine machine-effect contracts (package A64EFF): -8200..-8219
\ The first sub-block of the dialect range the map above reserves. It belongs to
\ src/compiler/a64-effect.f, which says what an emitted ARM64 routine does to the
\ machine - which registers it reads, returns and destroys, what it does to the
\ condition flags, the link register and the stack pointer, and how control
\ leaves it. The A64IR dialect package composes these contracts with its own
\ indexed operand records and takes a later sub-block for its own failures.
-8200 constant E-A64EFF-GPR     \ a general-register set naming a register no routine can hold state in: x18 is platform-reserved, x30 is the link register and has its own field, 31 is the zero register or the stack pointer, or the bit is past the 32-register file
-8201 constant E-A64EFF-FPR     \ a floating-register set bit past the 32-register file
-8202 constant E-A64EFF-ROLE    \ one register in two roles that exclude each other: a declared result that is also declared destroyed
-8203 constant E-A64EFF-FRAME   \ a frame size that is negative, not a multiple of the stack alignment, or past the reach of the load and store offset field
-8204 constant E-A64EFF-SP      \ a stack-pointer delta that rises above the caller's stack, is not a multiple of the stack alignment, is deeper than the declared frame, or is nonzero on a routine whose control returns
-8205 constant E-A64EFF-LINK    \ a link-register effect that cannot hold: control returns or tail-calls while x30 is declared destroyed
-8206 constant E-A64EFF-CONTROL \ results declared on a routine that never returns
-8207 constant E-A64EFF-TRAIT   \ a trait mask holding a bit outside the vocabulary of things a routine can do
-8208 constant E-A64EFF-SLOT    \ a frame slot the routine cannot address: a width no load or store form carries, a negative or unaligned offset, an offset past the frame, or an offset past the reach of that width's offset field
-8209 constant E-A64EFF-SEQ     \ an ordered register list no calling convention can have: more positions than one cell holds, a register no routine can hold state in, one register named at two positions, or bits set past the last position


\ Native stage N0 source tape (package NTAPE): -8240..-8259
\
\ The first sub-block of the native back end's range. Design section 7.1 puts
\ the source tape below every IR dialect: it records the token stream the
\ compiler actually consumed, so checking, elaboration, diagnostics, and code
\ generation can prove they read the same bytes. Byte spans are IR-SOURCE's
\ concern and reject with E-IR-SRC-SPAN, so no span code is minted here.
-8240 constant E-NTAPE-STATE    \ a tape arena failed its header or row-shape recheck
-8241 constant E-NTAPE-OWNER    \ a module key or an identity presented to a tape that does not own it
-8242 constant E-NTAPE-BOUND    \ a token ordinal at or past the count the tape records
-8243 constant E-NTAPE-CAP      \ a tape capacity outside the accepted range, or an append past it
-8244 constant E-NTAPE-KIND     \ a token kind outside the vocabulary, or a reader applied to a kind that has no such field
-8245 constant E-NTAPE-MODE     \ a stored parser mode outside the vocabulary
-8246 constant E-NTAPE-LITERAL  \ a literal value a token of that kind may not carry
-8247 constant E-NTAPE-ORIGIN   \ an expansion parent that is not an already-appended token of this tape
-8248 constant E-NTAPE-ROOT     \ an origin read on a directly lexed token, which has none
-8249 constant E-NTAPE-DIGEST   \ a presented tape digest does not match the recomputed one

\ Native immediate-word contract table (package NIMM): -8220..-8239
\
\ Design section 7.1 divides compile-time immediate words into front-end
\ intrinsics, sealed compile-time computation, and an unmodeled boundary that
\ checked source must not compile. This table is where a word's class is
\ declared, so a rejection can name the word instead of failing anonymously.
-8220 constant E-NIMM-STATE     \ a contract table failed its header or row-shape recheck
-8221 constant E-NIMM-OWNER     \ a module key or symbol presented to a table that does not own it
-8222 constant E-NIMM-BOUND     \ a contract-row index at or past the count the table records
-8223 constant E-NIMM-CAP       \ a table capacity outside the accepted range, or a declaration past it
-8224 constant E-NIMM-CLASS     \ a stored class outside the vocabulary, or a declarer given a class it does not declare
-8225 constant E-NIMM-DUP       \ a symbol this table already classifies
-8226 constant E-NIMM-UNMODELED \ an immediate word checked source may not compile: unregistered, or declared unmodeled

\ Old-vs-new code generator comparison harness (package CODEGEN-COMPARE), and
\ the native chain fixtures it shares with the compiler suites (package NSRC):
\ -8260..-8279
\
\ The harness compiles a pinned corpus of small checked words through the real
\ engine, then records each word's machine-code size, its execution outputs on
\ pinned inputs, and how long one call takes. It then compiles the same words
\ again through the new native chain and records the same three facts. Four
\ conditions stop it outright: a store it cannot grow, a subject word it cannot
\ find, a corpus word the new column names that the old column never measured,
\ and an emitted routine whose answer is not in the register the call needs it
\ in. Everything the comparison itself decides is reported as a finding instead,
\ so one run names every disagreement rather than stopping at the first.
-8260 constant E-CODEGEN-COMPARE-CAP      \ a row, output-vector, or report-text store is full
-8261 constant E-CODEGEN-COMPARE-SUBJECT  \ a named subject word is not in the live dictionary
-8262 constant E-CODEGEN-COMPARE-ROW      \ a row or output index outside the recorded count
-8263 constant E-CODEGEN-COMPARE-CLOCK    \ the monotonic clock reported no elapsed time across a whole timing run
-8264 constant E-CODEGEN-COMPARE-STAGE    \ a recorded value was read before the pass that computes it had finished
-8265 constant E-CODEGEN-COMPARE-CORPUS   \ a corpus word the new column names that the old column never measured
\ -8266 is retired and must not be reused: it was the comparison harness's check
\ that an emitted routine's returned value sat in the register the C-ABI call
\ reads one from, and the harness now compiles its routines under the data-stack
\ convention, where a result is a store into a slot and the validator re-derives
\ every one of those stores against the contract.
-8267 constant E-NSRC-CAP                 \ source text longer than the chain fixture's text buffer

\ Native stage N1 straight-line HIR dialect (package HIR): -8280..-8299
\
\ Design section 7.2 makes HIR the resolved Habu IR. This sub-block covers the
\ straight-line subset: the closed opcode set the dialect registers into its
\ schema table, and the source-word model that says which Habu words that
\ dialect can compile and what each one means. The schema records themselves are
\ IR-SCHEMA's concern and reject with its own codes, so nothing here repeats
\ them.
-8280 constant E-HIR-STATE      \ a word-model arena failed its header, row-shape, window, or stored-code recheck
-8281 constant E-HIR-OWNER      \ a module key or symbol presented to a word model that does not own it
-8282 constant E-HIR-BOUND      \ a row index or a pick index at or past the count the table records
-8283 constant E-HIR-CAP        \ a row or pick-pool capacity outside the accepted range, or a declaration past it
-8284 constant E-HIR-CLASS      \ a stored meaning outside the vocabulary, or a reader or declarer applied to a meaning it does not serve
-8285 constant E-HIR-DUP        \ a source word this word model already models
-8286 constant E-HIR-UNMODELED  \ a source word this dialect cannot compile: unmodeled, or never declared at all
-8287 constant E-HIR-KIND       \ a source-tape token kind the straight-line subset does not model
-8288 constant E-HIR-STAGE      \ rename-staging misuse: a begin while one is open, a pick without a begin, or an end without a begin
-8289 constant E-HIR-PICK       \ a stack rename that picks an input it never consumed, or an input or output count outside its ceiling
-8290 constant E-HIR-OPCODE     \ a word bound to an opcode this dialect's schema table does not define
-8291 constant E-HIR-DIALECT    \ a module whose schema table was created for another dialect or another schema version

\ Native stage N1 straight-line elaborator (package NELAB): -8300..-8319
\
\ Design section 7.2's elaboration step: walk one sealed source tape and build
\ the operations of a colon definition into a live module. Everything the
\ elaborator asks another authority stays that authority's refusal - a word the
\ dialect cannot compile is E-HIR-UNMODELED, a token kind the subset does not
\ model is E-HIR-KIND, and a malformed operation is IR-OP's - so these five
\ codes are only the facts the elaborator alone can judge: the shape of the
\ definition on the tape, the parser mode each of its tokens was consumed in,
\ the declared arity, and the compile-time value vector. -8302 was the frame
\ word's immediate contract, which the elaborator no longer judges: a produced
\ tape carries no frame token to classify, so the code is retired rather than
\ reused.
-8300 constant E-NELAB-SHAPE     \ a tape that is not one colon definition: no tokens at all, or a first token that is not the defined name
-8301 constant E-NELAB-MODE      \ a definition token consumed in the wrong parser mode for its place: the name outside interpreting, or a body word outside compiling
-8303 constant E-NELAB-ARITY     \ a declared input or output count outside the accepted range, or a body that does not leave exactly the declared outputs
-8304 constant E-NELAB-UNDER     \ a word consuming more values than the compile-time value vector holds
-8305 constant E-NELAB-CAP       \ a compile-time value vector past the elaborator's ceiling

\ Native ARM64 register allocation (package A64RA): -8320..-8329
\
\ Design section 7.9's linear scan over one straight-line block of the machine
\ dialect. Refusals another authority owns keep that authority's name - a
\ register no routine may hold state in is A64EFF's E-A64EFF-GPR, and a
\ malformed module is IR-OP's or IR-VERIFY's - so these are the facts the
\ allocator alone can judge: whether it was told which dialect it is reading,
\ whether the module and contract it was handed are the ones it was told about,
\ and whether what it found can be given real registers at all.
-8320 constant E-A64RA-BIND      \ allocation attempted before the dialect's module and type identities were bound, or a second binding over a live one
-8321 constant E-A64RA-MODULE    \ a frozen module that is not the bound one
-8322 constant E-A64RA-STATE     \ an allocation reader used before the walk sealed one, or after a later walk replaced it
-8323 constant E-A64RA-SHAPE     \ a module this leaf cannot allocate: not exactly one function, no block control leaves through or more than one, a successor ordinal outside the function, or values the function does not hold
\ -8324 is retired. It refused an operation of a form the allocator did not
\ recognise, because such a form might tie its registers without saying so. A
\ form now declares its ties in its own operation schema and the allocator reads
\ them, so there is nothing left to recognise and nothing left to refuse.
-8325 constant E-A64RA-CLASS     \ a value whose type is not the dialect's general-register type, so no general register can hold it
-8326 constant E-A64RA-TARGET    \ a context bound to a target these registers do not belong to
-8327 constant E-A64RA-CAP       \ a value, block or plan ordinal outside the allocator's tables: more of them in one routine than the tables hold, or a read past the count the sealed walk recorded
-8328 constant E-A64RA-TIE       \ a schema-declared tie that cannot be honoured: the kept value is still needed afterwards, or its register is not free
-8329 constant E-A64RA-PRESSURE  \ the routine's declared frame is exhausted: more values have to be spilled at once than its frame holds slots for. Register pressure itself is now a spill, not a refusal; the one operation that no spill can serve is E-A64RA-POOL

\ Native ARM64 allocation validator (package A64RAV): -8330..-8339
\
\ Design section 7.9's independent check of a finished assignment, and section
\ 11.3's register-allocation validator. It re-derives every live interval from
\ the frozen module rather than reading the allocator's own belief, so these ten
\ are the ways an assignment can fail to be true of the module it claims to be
\ about.
-8330 constant E-A64RAV-STATE     \ acceptance asked for with no sealed allocation, or an accepted answer read after a later walk replaced it
-8331 constant E-A64RAV-MODULE    \ a module that is not the one the allocation was made from
-8332 constant E-A64RAV-CONTRACT  \ a routine contract that is not the one the allocation was made under
-8333 constant E-A64RAV-INTERVAL  \ a recorded live interval that is not the one the module's own definitions and uses give
-8334 constant E-A64RAV-COVER     \ a value of the block with no assignment, or an assignment for a value the block does not hold
-8335 constant E-A64RAV-REGISTER  \ an assigned register outside the set the routine contract says it may destroy
-8336 constant E-A64RAV-OVERLAP   \ two values live at the same time assigned the same register
-8337 constant E-A64RAV-CLASS     \ a value whose type is not the dialect's general-register type
-8338 constant E-A64RAV-TIE       \ a schema-declared tie the assignment breaks: the tied result and operand are not the same register
-8339 constant E-A64RAV-SHAPE     \ a module that is not exactly one function with one block control leaves through, or a successor ordinal outside it, re-derived rather than taken from the allocator

\ Native ARM64 machine dialect (package A64IR): -8340..-8359
\
\ The dialect that stands for real ARM64 instruction forms with virtual registers
\ as SSA values. Almost every refusal a malformed A64IR operation earns belongs
\ to IR-SCHEMA or IR-OP, which measure it against the schema table; these five
\ are the facts only the dialect knows: which schema table it may fill, the two
\ operand fields of the move-wide form, and the two of the frame forms.
-8340 constant E-A64IR-DIALECT  \ a module whose schema table was created for another dialect or another schema version
-8341 constant E-A64IR-IMM      \ a move-wide immediate outside the sixteen-bit field the form holds
-8342 constant E-A64IR-SHIFT    \ a move-wide shift that does not select one of the four halves of a general register
-8343 constant E-A64IR-SLOT     \ a frame-slot offset the memory forms cannot address: negative, not a multiple of the eight bytes they move, or past the reach of their scaled twelve-bit offset field
-8344 constant E-A64IR-FRAME    \ a reserved frame size no routine can declare: negative, not a multiple of the stack alignment, or past the deepest frame the offset field can reach

\ Native ARM64 instruction selection (package A64SEL): -8360..-8379
\
\ The pass that reads a frozen straight-line HIR module and builds the frozen
\ A64IR module its operations select to. Refusals another authority owns keep
\ that authority's name - an out-of-range move-wide operand is E-A64IR-IMM or
\ E-A64IR-SHIFT, and a malformed operation is IR-OP's - so these seven are the
\ facts the selector alone can judge: whether it was told which dialect it is
\ reading, whether the module and text it was handed are the ones it was told
\ about, and whether what it found is something this leaf can select at all.
-8360 constant E-A64SEL-BIND    \ selection attempted before the source dialect's opcode identities were bound, or a second binding over a live one
-8361 constant E-A64SEL-SOURCE  \ a frozen module that is not the bound one, or source text whose digest is not the one the module recorded
-8362 constant E-A64SEL-OPCODE  \ an operation whose opcode is none of the source dialect's
-8363 constant E-A64SEL-SHAPE   \ a module or function this leaf cannot select: not exactly one source, or a function with no block at all
-8364 constant E-A64SEL-TRAP    \ an operation whose schema says it may trap: trapping arithmetic has no machine lowering in this dialect yet
-8365 constant E-A64SEL-ATTR    \ a constant whose attribute is not under the source dialect's value key
-8366 constant E-A64SEL-CAP     \ more values in one function than the selector's value map holds

\ Native ARM64 instruction emission (package A64EMIT): -8380..-8399
\
\ The last leaf of the native chain: it turns a frozen, verified, accepted
\ A64IR module into ARM64 instruction bytes and a source map. Refusals another
\ authority owns keep that authority's name - a register answered from a stale
\ acceptance is E-A64RAV-STATE, and an operand or attribute value outside the
\ field it lands in is the assembler's own refusal in src/arch/arm64/asm.f - so
\ these twelve are the facts the emitter alone can judge: whether it was told
\ which dialect it is reading, whether the module and the register assignment it
\ was handed are the ones it was told about, whether what it found is a shape this
\ leaf can emit, whether the instructions it wrote landed where its own layout
\ said they would, whether a caller is reading an emission that exists, and
\ whether the emission it is reading is one whose body has a meaning at all.
-8380 constant E-A64EMIT-STATE   \ a reader asked about an emission that has not happened, or that a later refused run replaced
-8381 constant E-A64EMIT-BIND    \ emission attempted before the machine dialect's identities were bound, or a second binding over a live one
-8382 constant E-A64EMIT-MODULE  \ a frozen module that is not the bound one, or a builder of another dialect or schema version
-8383 constant E-A64EMIT-ALLOC   \ no accepted register assignment at all, or one accepted for a different module
-8384 constant E-A64EMIT-SHAPE   \ not the straight-line subset: not exactly one function of one block, or a block whose only return is not its last operation
-8385 constant E-A64EMIT-OPCODE  \ an operation whose opcode is none of the machine dialect's family
-8386 constant E-A64EMIT-ATTR    \ a move-wide operation that carries no attribute under the key the dialect declares for it
-8387 constant E-A64EMIT-CAP     \ more instructions in one block than the emission buffers hold
-8388 constant E-A64EMIT-TARGET  \ emission under a context bound to a machine these instructions are not for
-8389 constant E-A64EMIT-BOUND   \ an instruction or source-map index at or past the count the sealed emission holds
-8390 constant E-A64EMIT-LAYOUT  \ the instructions written and the instructions the layout counted disagree: the writer reached a block at an offset the layout did not put it at, or ended the routine at a different length
-8391 constant E-A64EMIT-BODY    \ how many instructions of an emission are its body, asked about a routine that calls: a call site publishes and takes back through the very data-stack forms a routine's own crossings use, so the emission less its crossings would not be that routine's body

\ Native stage N0 source-tape producer (package NFEED): -8400..-8409
\
\ The producer that fills a source tape from the checker's own reader. Refusals
\ another authority owns keep that authority's name - a tape that is full is
\ E-NTAPE-CAP, a span outside its source is IR-SOURCE's - so these seven are
\ the facts the producer alone can judge: whether a unit is open, whether the
\ scan that reached it is the unit's own, whether the token it was handed
\ really is the bytes at the offset it claims, whether the text the reader
\ handed over fits the buffer the caller committed, and whether this stage can
\ record the token at all.
-8400 constant E-NFEED-STATE    \ a producer word reached in a state that has no meaning for it: no unit open, a unit still scanning, or a second unit over a live one
-8401 constant E-NFEED-SCAN     \ a second or foreign reader scan reached an open unit: one unit is one scan
-8402 constant E-NFEED-SPAN     \ a token whose bytes are not the bytes at the offset it claims, or that leaves the scanned text
-8403 constant E-NFEED-ORDER    \ an append answered an ordinal other than the next one: the tape gained a row this producer did not write
-8404 constant E-NFEED-KIND     \ a token class this stage has no tape kind for
-8405 constant E-NFEED-LITERAL  \ an integer literal whose value this stage cannot read back
-8406 constant E-NFEED-TEXT     \ a scan whose text is longer than the buffer the unit was opened with

\ Native ARM64 spill lowering and frame slots: -8440..-8459
\
\ The sub-block the register-allocation stage took when its own twenty codes
\ (-8320..-8339) were spent, exactly as the map above prescribes: a stage that
\ outgrows its block takes the next unassigned one rather than spilling into a
\ neighbour. It holds the spill-lowering pass (package A64SPILL), which reads a
\ frozen machine module and builds the module in which the allocator's spill
\ decisions are real store and load operations, plus the frame-slot facts the
\ allocator (package A64RA) and its validator (package A64RAV) gained with it.
\ Refusals another authority owns keep that authority's name: a slot no load or
\ store form can reach is A64EFF's E-A64EFF-SLOT, a slot offset outside the
\ dialect's own field is E-A64IR-SLOT, and a malformed module is IR-OP's.
-8440 constant E-A64SPILL-BIND   \ a rewrite attempted before the machine dialect's identities were bound, or a second binding over a live one
-8441 constant E-A64SPILL-PLAN   \ no sealed spill plan at all, one made for another module, or one a later allocation replaced
-8442 constant E-A64SPILL-SHAPE  \ a module this pass cannot rewrite: not exactly one function of one block, a span naming another source, or a value used before it is defined
-8443 constant E-A64SPILL-SOURCE \ source text whose digest is not the one the module being rewritten recorded
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
\ The registers a routine's arguments arrive in and its results leave in are
\ declared by its contract (package A64EFF), which owns the shape of that
\ declaration and refuses one that cannot be a convention at all. These two are
\ what the register allocator and its validator add: whether the allocation this
\ machine module needs can honour that declaration, and whether the finished
\ assignment does. Both packages' own blocks were full when the binding landed.
-8460 constant E-A64RA-FIXED     \ a declared argument or result register the allocation cannot honour: more declared positions than the routine has arguments or returned values, a register outside the set the routine may write, one value returned at two declared positions, or a declared register already held where it has to be taken
-8461 constant E-A64RAV-FIXED    \ an assignment that does not put a declared argument or returned value where the contract declares it, or one made for a contract declaring more positions than the module has

\ Native ARM64 data-stack calling convention: -8480..-8499
\
\ A Habu word takes its inputs and publishes its outputs through canonical
\ data-stack slots rather than through registers, so a position of a calling
\ convention became a PLACE - a register or a data-stack slot - and the machine
\ dialect gained the four forms that reach the data stack. These are the facts
\ the five owners of that convention add. Refusals another authority owns keep
\ that authority's name: a place list that is not canonical at all is still
\ A64EFF's E-A64EFF-SEQ, a register no routine may hold state in is
\ E-A64EFF-GPR, and an operand outside the field it lands in is the assembler's.
-8480 constant E-A64EFF-KIND    \ a place read as the kind it is not: the register of a data-stack slot, or the slot index of a register
-8481 constant E-A64IR-DSLOT    \ a data-stack slot offset the load and store forms cannot address: negative, not a multiple of the eight bytes they move, or past the reach of their scaled twelve-bit offset field
-8482 constant E-A64IR-DBYTES   \ a data-stack pointer adjustment no routine can make: negative, not a multiple of one stack cell, or past the twelve-bit immediate that claims it
-8483 constant E-A64SEL-PLACE   \ a calling convention this selector has no rule for: one side mixing register places with data-stack places
-8484 constant E-A64RA-PLACE    \ a place list the allocation cannot honour: one side mixing register places with data-stack places, or data-stack places declared on a module whose interface is still block arguments or terminator operands
-8485 constant E-A64RAV-PLACE   \ the same, re-derived: a mixed side, or data-stack places on a module that still carries its interface as block arguments or terminator operands
-8486 constant E-A64RAV-DSTACK  \ a data-stack entry or exit sequence that is not the one the contract declares: a missing or misplaced adjustment, a wrong byte count, a load or store naming a slot no place declares, or a module that reaches both the data stack and a frame

\ Native ARM64 control flow: -8500..-8519
\
\ A colon body with a branch or a loop is more than one basic block, and every
\ stage of the native chain gained something for it: the machine dialect a
\ compare form and two branch forms, the source-word model the structured
\ control words, the elaborator the control stack that turns them into blocks,
\ and the emitter the block layout and the branch fixups. These are the facts
\ those owners add. A refusal another authority already owns keeps that
\ authority's name: a successor naming a block no function defines is still
\ IR-VERIFY's E-IR-VERIFY-SUCC, and a branch operand outside the field it lands
\ in is the assembler's.
-8500 constant E-A64IR-COND     \ a condition code the compare form cannot carry: outside the four-bit field the conditional forms encode it in
-8501 constant E-HIR-CONTROL    \ a source word read as a control word when its row models something else, or a stored control code outside this dialect's vocabulary
-8502 constant E-NELAB-CTRL     \ a control structure the elaborator cannot close: a closer with no opener, a closer that does not match the opener it meets, a word that stands in the middle of a structure with no such structure open, a second one of those over one structure, a `while` after a word that already left the definition, a loop closed by a word that does not open the block its `while`s branch out to, or a body that ends with a structure still open
-8503 constant E-NELAB-JOIN     \ two paths into one join that do not agree: arms leaving different numbers of values on the compile-time value stack, two `while`s leaving one loop at different depths, a loop body that does not leave the stack as its header takes it, an edge whose width disagrees with the width the first edge into that block stated, or a block opened with arguments that no edge into it stated the types of
-8505 constant E-A64EMIT-BLOCK  \ a branch whose successor names no block of the function this emission laid out
-8506 constant E-A64EMIT-REACH  \ a branch whose displacement does not fit the field its form encodes it in
-8504 constant E-NELAB-BLOCK    \ more blocks or more open control structures in one definition than the elaborator's tables hold
-8507 constant E-A64RA-EDGE     \ a block argument and the values handed to it across an edge cannot share one register: two of them are live at the same time, or the edge hands over a different number of values than the destination takes
-8508 constant E-A64RA-SPILL    \ a routine needs a value put in a frame slot and has none this pass may take: every class holding a register there is one an edge or a schema tie made of more than one value, holds a block argument, is read by a data-stack operation, or reaches the frame from a block that is neither the one the caller enters nor the one control leaves through
-8509 constant E-A64RAV-EDGE    \ the same edge rule, re-derived: an assignment in which the register holding a terminator's operand is not the register holding the successor's block argument at that position

\ Native ARM64 memory access: -8520..-8539
\
\ A colon body that reads or writes a cell of memory needs two things no earlier
\ leaf had: an ordering value that says which access happens before which, and a
\ load and a store whose address is a value the program computed rather than a
\ pointer the instruction form names. The source dialect gained the order and
\ the two words, and the machine dialect gained the two forms. These are the
\ facts those owners add. A refusal another authority already owns keeps that
\ authority's name: a source word the dialect cannot compile is still
\ E-HIR-UNMODELED, a row read as a meaning it does not carry is E-HIR-CLASS, and
\ an address operand outside the field it lands in is the assembler's.
-8520 constant E-NELAB-TOKEN    \ the definition's one memory order is wrong for the operation being staged: either the operation's schema declares more than one token among its operands or among its results, which the elaborator has no rule for, or it takes an order in a definition the pre-scan found no memory word in
-8521 constant E-A64SEL-MEM     \ a memory operation in a routine whose convention names no data-stack place at all: the generic memory order of this dialect begins where the routine takes the caller's operands, and a routine that takes and publishes nothing has no beginning for it
-8522 constant E-A64RAV-ORDER   \ a memory order the module mints and does not pass on exactly once: a token nothing reads leaves the order after it unstated, and a token two operations read is two orders claiming to be one

\ Native typed locals: -8540..-8549
\
\ A `{: … :}` group binds names to values the body then reads by name. The
\ elaborator is the only stage that ever sees a local: a bound name is a value
\ of the compile-time vector, so nothing downstream of the elaborator learns a
\ new concept and no other package gains a code here. A refusal another
\ authority already owns keeps that authority's name - a local named after a
\ word the dialect models is still E-HIR-DUP's kind of collision but it is this
\ elaborator that has no rule for it, and a source word the dialect cannot
\ compile at all is still E-HIR-UNMODELED, which is what a rebinding or an
\ address-of by name is refused as.
-8540 constant E-NELAB-LOCAL     \ a locals declaration this elaborator has no rule for: a second group in one definition, a group opened inside a control structure, a group left unclosed, a closer with no opener, a declared name the dialect already models, the same name declared twice, or a token in the group that is not a name
-8541 constant E-NELAB-LOCAL-CAP \ more locals in one definition than the elaborator's name table holds

\ Native calls: -8550..-8559
\
\ `RECURSE` is a call to the word being compiled. What is new about it is not the
\ branch - it is that the caller's own registers do not survive it, because the
\ callee is this same routine and its contract destroys exactly the registers the
\ allocator hands out. So the values the caller still needs cross the call on the
\ caller's data stack, and the caller's return address crosses it in the
\ routine's own frame. These are the facts those owners add; a refusal another
\ authority already owns keeps that authority's name - a word this dialect's
\ table never declared at all is still E-HIR-UNMODELED.
\
\ A CALL TO ANOTHER WORD ADDS THREE FACTS AND NOTHING ELSE. The callee's arity,
\ which is what the call site publishes and takes back; the callee's entry
\ address, which is where the branch goes; and the address the CALLING routine
\ will itself be written at, without which no displacement between the two can
\ be computed. The first two are the word model's row and the machine dialect's
\ field, and the third is the publication seam's answer, held by the emitter and
\ measured against the seam's own claim before a byte is written.
-8550 constant E-NELAB-CALL     \ a call the elaborator has no rule for: fewer values on the compile-time vector than the callee declares it takes, more values live across the call than the vector can hold afterwards, or a call reached with no memory order live
-8551 constant E-A64SEL-CALL    \ a call the selector has no lowering for: a routine whose convention names no data-stack place, a routine contract that does not declare that this routine calls, a contract declaring a call in a module that contains none, a frame too small to hold the saved link register, or a call whose operand and result lists disagree about how many values are live across it
-8552 constant E-A64RAV-CALL    \ a call site that is not the sequence the dialect lowers a call to: a store run that does not name slots zero upwards in order, a byte count that is not the run it stands for, or a saved link register the module does not restore from the slot it saved it into
-8553 constant E-A64RAV-OWNER   \ a frame slot given to the wrong owner: a spilled value placed in the slot a calling routine keeps its caller's return address in, or a prologue access naming a slot that is not that one
-8554 constant E-HIR-CALLEE     \ a callable-word declaration this dialect cannot hold: an entry address that is negative or not instruction aligned, or an argument or result count that is negative
-8555 constant E-A64IR-ENTRY    \ a callee entry address the branch-with-link form cannot name: negative, or not the address of a whole instruction
-8556 constant E-A64EMIT-PLACE  \ an emission that names a callee and was told no address of its own to measure the branch from, a placement that is negative or not instruction aligned, or a second placement declared over a live one
-8557 constant E-A64RAV-CLOBBER \ a value the caller keeps in a register across a call to a routine that destroys that register: an assignment the callee's recorded clobber set says the call site cannot make
-8558 constant E-A64EMIT-CLOBBER \ an emission that writes a general or floating register the accepted allocation claimed for no value, so what the routine destroys cannot be published from the allocation
-8559 constant E-NELAB-INLINE   \ a callee's recorded body the elaborator cannot splice into its caller: a recorded arity that is not the effect the caller declared for that callee, a token the caller's own word model admits with a meaning the splice has no rule for, a spliced body that reaches below the values its caller was holding, or one that does not leave the vector as the callee's declared effect says

\ Native publication: -8560..-8579
\
\ An emission becomes a word the engine's callers can reach by taking a slice of
\ the engine's own code arena and pointing an existing dictionary record at it.
\ Two owners add facts here: the publication seam, which claims the code space
\ and rewrites the record, and the migration entry, which is what runs the chain
\ for one named definition. A refusal another authority already owns keeps that
\ authority's name: an emission that no accepted allocation stands behind is
\ still A64EMIT's E-A64EMIT-ALLOC, a reader before the seal is E-A64EMIT-STATE,
\ a body the dialect cannot compile is E-HIR-UNMODELED, and a code address the
\ engine will not write to is the engine's own sealed exit rather than a throw.
-8560 constant E-NPUB-NAME    \ a name this seam cannot republish: no such word, a retired record, a package or namespace record, an engine-internal word, or an immediate word whose callers are the compiler rather than the running program
-8561 constant E-NPUB-ROOM    \ the emission does not fit in what is left of the code arena under the engine's own end reserve
-8562 constant E-NPUB-SIZE    \ an emission that cannot be a word body: no instructions at all, or a byte size that is not a whole number of instructions
-8563 constant E-NPUB-OFFSET  \ an instruction whose source-map offset does not lie inside the emission it belongs to, or that is not instruction aligned
-8564 constant E-NPUB-CAP     \ more republished words than the seam's replacement log holds
-8565 constant E-NPUB-LOG     \ a replacement asked about a word the log has no row for
-8566 constant E-NPUB-PLACE   \ an emission whose branches were measured from an address that is not the code slot this seam is claiming for it
-8567 constant E-NCLOB-WIDEN  \ a second record of what the routine at one code address destroys that names a register the first record did not: every call site compiled against the first one skipped saving exactly that register
-8568 constant E-NCLOB-CAP    \ more published routines than the clobber record's table holds
-8569 constant E-NPUB-CLOBBER \ an emission that branches to a routine destroying a register the set about to be published for it does not name: what a routine destroys has to cover what everything it calls destroys
-8578 constant E-NPUB-SLOT    \ the code slot this publication is claiming lies below the end of the routine the seam published last: code space was reclaimed without the notice everything keyed to a code address is dropped by, so a slot would be claimed twice

-8574 constant E-NINL-STATE    \ the recorded-body staging used out of order: a body staged while another one is open, a token staged with none open, or a commit with nothing staged
-8575 constant E-NINL-CAP      \ more tokens in one recorded body than a row holds, or a spelling longer than a token slot holds: a table with no room for another body declines the row instead, because a full table is no reason to refuse a word the chain compiled
-8576 constant E-NINL-DUP      \ a second recorded body for one code address: the code at an address is written once and a caller compiled against the first body would have been given another one
-8577 constant E-NINL-BOUND    \ a recorded body asked about an address it has no row for, or about a token outside the body that row holds

-8570 constant E-NMIGRATE-STATE   \ a migration reached while another one is open, a migration entered as one that calls nothing whose body staged a call, or a staged callee list left behind by a refused run: the recorder takes one definition at a time
-8571 constant E-NMIGRATE-TEXT    \ definition source longer than the recorder's text buffer, or a name longer than the log holds
-8572 constant E-NMIGRATE-VERDICT \ the engine's own check did not certify the definition, so there is no checked word to migrate
-8573 constant E-NMIGRATE-NAME    \ the definition the source published is not the name the caller asked to migrate: the newest dictionary record carries another name
-8579 constant E-NMIGRATE-CALLEE  \ a staged callee whose spelling and whose address are not one fact: a spelling that denotes no word where the definition is compiled, or one whose word's code starts somewhere other than the address staged beside it

\ The float subset: -8580..-8589
\
\ A double lives in one unboxed cell, so nothing about a value's SIZE separates
\ it from an integer and the separation has to be its type. These are the
\ refusals that make the separation real, one per stage that carries the type: a
\ compile-time value of the wrong type for the position it is handed to, and a
\ value whose type no register file of the machine dialect can hold. A refusal
\ another authority already owns keeps that authority's name - a double type on a
\ machine with no floating unit is IR-TYPE's E-IR-TYPE-TARGET, a float literal
\ spelling the tape cannot read back is E-NFEED-LITERAL, and a staged operation
\ whose operand type is not the one its schema declares is IR-OP's.
-8580 constant E-NELAB-TYPE     \ a compile-time value whose type is not the one the position wants and is not a crossing this dialect performs: a double handed to an operation that computes with cells, or a double stored into a memory cell, which no leaf places yet
-8581 constant E-A64RA-FILE     \ two values of two different register files joined into one class: an edge or a schema tie whose two ends cannot share a register because no register holds both

\ Native call-site redirection: -8590..-8609
\
\ A republication points a dictionary record at new code, which reaches every
\ caller compiled AFTER it. The callers compiled before it hold a branch to the
\ address the word occupied then, and moving those branches is what this band's
\ refusals guard. Two owners: the branch-with-link reader, which is the one
\ authority on what such an instruction says and what one has to say to reach a
\ given address, and the redirection seam, which decides whether a site may be
\ moved at all. A refusal another authority already owns keeps that authority's
\ name: a routine that is not this process's to point at is still the
\ publication seam's E-NPUB-LOG, and a widened clobber row is NCLOB's.
-8590 constant E-NREACH-NAME    \ a name that resolves to no record, to a record holding no word's code, or two names resolving to one record: there is nothing to carry from, or nothing to carry to
-8591 constant E-NREACH-WORD    \ the two records do not carry the same word: a redirection may move a word's callers onto that word's new code and onto nothing else
-8592 constant E-NREACH-ROUTINE \ the code the redirection points at is not a routine the publication seam wrote for that record: the only code a caller may be moved onto is one an accepted allocation stands behind
-8593 constant E-NREACH-EFFECT  \ the two words declare different stack effects, so a site compiled around one of them cannot enter the other
-8594 constant E-NREACH-CLOBBER \ the routine being pointed at destroys a register a caller of the old code was entitled to keep across the call: every site was compiled against what the old address destroys, and the new one may only narrow it
-8595 constant E-NREACH-RANGE   \ a call site whose distance to the new routine does not fit the branch-with-link displacement field
-8596 constant E-NREACH-NONE    \ no call instruction in the live dictionary enters the old code: either the engine copied the body into its callers, or nothing calls it, and either way there is nothing a redirection can carry
-8597 constant E-NREACH-SELF    \ the routine being pointed at itself enters the code being redirected, so carrying the redirection into it would make it call itself
-8598 constant E-NBR-RANGE      \ a branch-with-link whose two addresses are not both whole instructions, or whose displacement does not fit the twenty-six-bit field the form encodes it in

\ Data-stack residency: -8610..-8619
\
\ A value the routine only hands on lives in the caller's data-stack slot it
\ arrived in, and the emission neither lifts it into a register nor writes it
\ back. Which slot holds which value is a fact about a whole routine, so the
\ selector computes it and the allocation validator computes it AGAIN from the
\ module it is handed - these are the refusals of that second derivation. The
\ validator names a machine value, never a source one, so what it can decide is
\ that a slot the emission publishes holds something, and that no access it kept
\ was one it could have dropped. Whether the value in the slot is the value the
\ program meant to publish there is a statement about the module the selector
\ read, and it stays where it already was: dot habu-prove-a-data-df458151.
-8610 constant E-A64RAV-DRES    \ a data-stack slot published with no store in front of it that no path defines: an omitted store for a slot nothing ever wrote, one a call between the write and the publication destroyed, or a read of such a slot - and, fail-closed rather than reachable, a residency descent that was still moving after every cell of the map could have fallen as far as it can
-8611 constant E-A64RAV-DKEEP   \ a data-stack access the emission had no reason to make: a store writing a slot that already holds that very value, a load of a slot whose value is already in a register, or a load whose result nothing reads

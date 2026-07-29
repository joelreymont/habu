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
\ rest of -6820..-6999 stays unclaimed.
-6800 constant E-CIN-FIRST
-6819 constant E-CIN-LAST
-6800 constant E-CIN-ROW      \ a shared interning vector index or table slot is outside its declared table
-6801 constant E-CIN-STRUCT   \ a frozen interner structure - a compared-field list, a check-before-write ordering, or a reference guard - is not what the source carries

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
\   -8100..-8119  canonical table reindexing and reference remap
\   -8120..-8139  deterministic renderer and structural diff
\   -8140..-8159  canonical wire codec and digest (package IR-CODEC)
\   -8160..-8179  pass results and witness headers (package IR-PASS)
\   -8180..-8199  IR facade assembly and package protection (package IR)
\   -8200..-8999  unassigned. The dialect packages (HIR, SIR, LIR, A64IR, and
\                 the GPU stages) and the native and GPU back ends take
\                 sub-blocks from here, each named above its codes.
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

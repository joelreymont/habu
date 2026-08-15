\ aot-chain-capture.f — capture the native compiler chain inside a booted engine.
\
\ THE LINE ORDER OF THIS FILE IS THE CONTRACT. Read it top to bottom; every
\ `require` below is placed where it is for a reason a reordering would break
\ silently, so the file is written flat rather than factored into a library plus
\ an entry point.
\
\ WHY THE CAPTURE RUNS HERE AT ALL. The metabuild host's dictionary is not the
\ target's: a word captured there may collide with, or shadow, a word the booting
\ engine already has, and three ordered deaths proved it (an ARM64-W32 duplicate,
\ an ENGINE-ERROR duplicate, then regalloc.f's BMAX binding to the wrong EMITTER).
\ So the chain is captured in a process whose dictionary IS the target's — a
\ booted engine — and the artifact is what crosses to the metabuild.
\
\ WHY THE CHAIN LOADS FIRST. A captured call site travels as a NAME that the seed
\ resolves at the boot of the shipped engine, so a window word may only call a
\ word that engine will have. Everything this tool loads before the window opens
\ exists in THIS process and in no target. Loading the capture's own tooling first
\ was tried and refuted by measurement: aot-capture.f's closure requires
\ src/arch/arm64/asm.f, the compiler chain requires asm.f too, and `require` is a
\ no-op the second time — so the chain's words end up calling the TOOL's copy of
\ asm.f. 98 of 18602 call sites, refused by name (first: the chain's MASK calling
\ A64ASM's LIMM?). Chain first, tooling after: 0.
\
\ WHAT MAY THEREFORE LOAD BEFORE THE WINDOW: src/habu/layout.f and
\ src/habu/aot-arm.f, which exists precisely so that arming the window does not
\ drag aot-capture.f in ahead of the chain. The two are cheap for different
\ reasons, both measured in a booted bin/hb: layout.f is already registered
\ there, so its `require` adds 0 records and 0 DATA bytes and is kept only as the
\ dependency statement; aot-arm.f adds 4. asm.f, by contrast, is NOT registered —
\ requiring it in a booted engine compiles 178 records — which is the whole reason
\ it must be the chain that brings it in.
\
\ THE PRELUDE MARKS ARE THE FIRST THING THIS PROCESS DOES, before it defines a
\ variable of its own, because they bound the band the capture refuses to call
\ into: [mark, window) is every record and every DATA byte this tool added. The
\ package opens before the marks are read — a `package` line writes a dictionary
\ record too, and that record belongs below the mark with the engine's own words.
\
\ AND THE MARKS ONLY MEAN THAT IF THIS FILE IS THE FIRST THING THE PROCESS LOADS.
\ The band audit trusts everything BELOW the mark to be a word the target engine
\ has, which is true of the engine's own surface and of nothing else. Run the tool
\ behind one other file and the capture succeeds while producing an unbootable
\ seed: `bin/hb --load ...asm.f ...aot-chain-capture.f` marks asm.f's 178 records
\ as the engine's, drops them out of the window (code span 1194680 rather than
\ 1215872) and bakes calls to names the target has not got. So the tool checks it,
\ and checks it against the engine's own registry rather than against a
\ convention: src/core/include.f freezes REQUIRE-BOOT-N at the end of the boot
\ prefix, so REQUIRE-N minus REQUIRE-BOOT-N is exactly how many files THIS process
\ has loaded, and the only acceptable answer is one — this file.
\
\ Run it in a booted engine, from the repository root:
\   bin/hb --load tools/aot-chain-capture.f
\ It prints one `name=value` line per census field and exits 0, or refuses with
\ exit 74 and a named diagnostic — its own, or one of src/habu/aot-capture.f's.

package AOT-CHAIN
public
ndict@ here REQUIRE-N @ REQUIRE-BOOT-N @
variable PRE-R  variable PRE-D  variable PRE-REQ  variable BOOT-REQ
BOOT-REQ !  PRE-REQ !  PRE-D !  PRE-R !
;package

require src/habu/layout.f
require src/habu/aot-arm.f

package AOT-CHAIN
public

variable B0  variable B1      \ the window's code span
variable R0  variable R1      \ its dictionary record span
variable D0  variable D1      \ its DATA span
variable Q0  variable Q1      \ its require-registry span: the closure it loaded

\ Latch the three cursors and tell the engine where the window starts, which is
\ what makes its compile-mode inliner emit a CALL to a pre-window body rather than
\ copying it with the addresses it holds (habu2.f AOT-WINDOW:EMIT-OUTSIDE).
: OPEN ( -- )
   cp@ B0 !  ndict@ R0 !  here D0 !  REQUIRE-N @ Q0 !
   B0 @ D0 @ AOT-ARM:OPEN ;

: CLOSE ( -- )
   cp@ B1 !  ndict@ R1 !  here D1 !  REQUIRE-N @ Q1 ! ;

;package

AOT-CHAIN:OPEN
require src/compiler/native/migrate.f
AOT-CHAIN:CLOSE

\ The capture's own tooling, all of it above the window's last record and past its
\ last DATA byte. Its buffers are allotted here, so no window word can hold one of
\ their addresses and the DATA audit's third class ("above the window's DATA
\ span") stays empty for structural reasons rather than lucky ones.
require src/arch/arm64/icode.f
require src/habu/aot-decl.f
require src/habu/aot-capture.f

\ The capture's IDENTITY, loaded with the same "after the window" rule. aot-ident.f
\ turns the require-registry span above into the closure list and its digest;
\ lib/engine-id.f answers the other half, the content key of the binary this
\ capture is running in, which the metabuild recomputes over the engine it emitted
\ and compares. Both are above the window, so neither is captured.
require src/habu/aot-ident.f
require lib/engine-id.f

package AOT-CHAIN
using AOT-BUF
public

$4A constant REFUSE-RC

\ One file loaded in this process, and it is this one. Asked of the engine's own
\ require registry, so a tool run behind anything at all stops here rather than
\ marking that thing's records as the target's.
: ?FIRST ( -- )
   PRE-REQ @ BOOT-REQ @ - 1 = if exit then
   s" aot-chain-capture: files loaded before the capture=" type
   PRE-REQ @ BOOT-REQ @ - 1 - .
   s" aot-chain-capture: the capture must be the first file this process loads"
   REFUSE-RC die ;

: ?WINDOW ( -- )
   R1 @ R0 @ <> if exit then
   s" aot-chain-capture: the window is empty - the chain did not load" REFUSE-RC die ;

create CHAIN-SHA 32 allot
create HEX 64 allot

: RUN ( -- )
   ?FIRST
   ?WINDOW
   Q0 @ Q1 @ AOT-IDENT:CLOSURE!
   PRE-R @ PRE-D @ AOT-CAPTURE:PRELUDE-MARK
   B0 @ B1 @  R0 @ R1 @  D0 @ D1 @  AOT-CAPTURE:CAPTURE ;

\ One `name=value` per line. `codespan`/`dataspan` are the window's own measured
\ extents, so a reader can check the capture against the window instead of taking
\ the capture's word for its own size.
: CENSUS. ( -- )
   s" recs=" type AOT-REC-N @ .
   s" sites=" type AOT-SITE-N @ .
   s" blob=" type AOT-BLOB-LEN @ .
   s" names=" type AOT-NAMES-LEN @ .
   s" dsites=" type AOT-DSITE-N @ .
   s" csites=" type AOT-CSITE-N @ .
   s" xtsites=" type AOT-XTSITE:N @ .
   s" xtoff=" type AOT-WINDOW:XTOFF-N @ .
   s" datasz=" type AOT-DATA-SIZE @ .
   s" codespan=" type B1 @ B0 @ - .
   s" dataspan=" type D1 @ D0 @ - .
   s" bandrecs=" type R0 @ PRE-R @ - .
   s" bandbytes=" type D0 @ PRE-D @ - .
   s" closure=" type AOT-IDENT:COUNT .
   s" first=" type 0 AOT-IDENT:PATH$ type cr
   s" last=" type AOT-IDENT:COUNT 1 - AOT-IDENT:PATH$ type cr
   CHAIN-SHA AOT-IDENT:CHAIN-DIGEST
   CHAIN-SHA HEX SHA256>HEX
   s" chaindigest=" type HEX 64 type cr
   s" producer=" type ENGINE-ID:KEY$ type cr ;

: MAIN ( -- ) RUN CENSUS. ;

;package

AOT-CHAIN:MAIN

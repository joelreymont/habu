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

variable Q0  variable Q1      \ its require-registry span: the closure it loaded

\ The window's code, record, DATA and wordlist spans are AOT-ARM's, latched by
\ the words that name the two moments; the require registry is this tool's own
\ fifth axis, because it is the only process with a window to bracket it across.
: OPEN ( -- )
   AOT-ARM:WINDOW-OPEN
   REQUIRE-N @ Q0 ! ;

: CLOSE ( -- )
   AOT-ARM:WINDOW-CLOSE
   REQUIRE-N @ Q1 !
   AOT-ARM:SIG-CLOSE ;

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

\ The artifact, and the whole-span descriptor write it needs. src/habu/fdio.f is
\ the tree's one write-all loop and the only part of the build drivers' I/O a
\ booted engine can load - src/habu/driver-io.f itself stops at E-UNDEFINED: MBUF,
\ six files short of the target's image writer. Both load after the window for the
\ same reason everything else here does.
require src/habu/fdio.f
require src/habu/aot-file.f

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
   AOT-ARM:R1 @ AOT-ARM:R0 @ <> if exit then
   s" aot-chain-capture: the window is empty - the chain did not load" REFUSE-RC die ;

create CHAIN-SHA 32 allot
create HEX 64 allot

\ The closure the window compiled, read out of the engine's own require registry.
\ [Q0,Q1) is REQUIRE-N bracketed across the window, so it is exactly the files the
\ chain pulled in and nothing else. The registry read lives here rather than in
\ src/habu/aot-ident.f because this is the only process that has a window to
\ bracket - the metabuild fills the same table from the artifact's own list - and
\ because aot-ident.f has to compile in a metabuild host that carries no
\ src/core/include.f.
: ?CLOSURE ( -- )
   Q1 @ Q0 @ > if exit then
   s" aot-chain-capture: the window loaded no file" REFUSE-RC die ;

: LATCH-CLOSURE ( -- )
   ?CLOSURE
   AOT-IDENT:RESET
   Q1 @ Q0 @ ?do i REQUIRE-SLOT i REQUIRE-LEN@ AOT-IDENT:PATH+ loop ;

\ WHAT THE SEEDED ENGINE HAS TO RUN BEFORE THE CHAIN IS USABLE, and it is this
\ tool's to say, the way src/habu/stdin.f CAPTURE-REPL names its own installers.
\
\ A window word that runs AT LOAD TIME and writes a cell is the whole class. The
\ write itself never travels: what the window's own DATA holds is captured byte
\ for byte, but a cell BELOW the window belongs to the engine the chain was loaded
\ into, and in a seeded engine that cell holds whatever the target's prefix put
\ there. Only a boot-run entry can put the window's routine back.
\
\ THE TWO AXES ARE COUNTED SEPARATELY BECAUSE THEY FAIL SEPARATELY.
\   In the window: a captured declared cell arrives trapped, because what it held
\   was a code address in the process that compiled it, and the seed re-traps it.
\   A64RAV:DKEEP-HOOK is the one - the `defer` regalloc-verify.f opens for a reader
\   of its refusals - and without its installer the first refusal the verifier
\   reaches dies "defer: unset execution vector". ?XTOFF holds that count.
\   Below the window: three CHECKER-TAPE observer cells and three CODE-RECLAIM
\   watcher slots, six in all, planted by four load-time installers. Without them
\   a seeded engine dies "checker: no source-tape observer to arm" on the first
\   definition - and the three watcher rows fail SILENTLY, leaving inline and
\   publish rows pointing at code a reclamation already took back. ?TRAPPED holds
\   that count, measured against the engine's own registry rather than read off
\   this list.
\
\ So each row below carries the number of pre-window cells its installer refills,
\ and the two numbers are checked against what the process measures. A fifth
\ installer nobody declared moves the measurement and not the list, and the
\ capture stops.
variable CELLS-OWED           \ pre-window cells the declared installers refill

: RESOLVE-BAD ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n r:ptr ru:n :}
   s" aot-chain-capture: boot-run name " type a u type s"  " type r ru type cr
   s" aot-chain-capture: the seed resolves this list by name at boot and a miss is silent"
   REFUSE-RC die ;

\ Asked exactly the way the seed asks it. A qualified name resolves through the
\ namespace record's PUBLIC slot (habu1.f FIND-NMATCH) and XREF-FIND-INDEX takes
\ the same route, so a name that is private, misspelt, or belongs to a word the
\ window did not compile answers here instead of dying $52 with no message in the
\ built engine. The window test is the second half: a name that resolves to a
\ PRE-WINDOW word would run the host's copy at boot and install nothing.
: ?RESOLVES ( ptr u8 n -- ) {: a:ptr u:n :}
   a u XREF-FIND-INDEX {: k:n :}
   k 0 < if a u s" does not resolve here, so it will not resolve there" RESOLVE-BAD then
   k AOT-ARM:R0 @ >= k AOT-ARM:R1 @ < and if exit then
   a u s" resolves to a word outside the capture window" RESOLVE-BAD ;

: DECLARE ( ptr u8 n n -- ) {: a:ptr u:n cells:n :}
   a u ?RESOLVES
   a u AOT-CAPTURE:BOOTRUN+
   CELLS-OWED @ cells + CELLS-OWED ! ;

: DECLARE-ALL ( -- )
   0 CELLS-OWED !
   s" A64RAV:DKEEP-HOOK-DEFAULT" 0 DECLARE   \ its cell is in the window: see ?XTOFF
   s" NFEED:INSTALL"             3 DECLARE   \ CHECKER-TAPE scan, token, verdict
   s" NCLOB:WATCH-INSTALL"       1 DECLARE   \ CODE-RECLAIM watcher slots, one each
   s" NINL:WATCH-INSTALL"        1 DECLARE
   s" NPUB:WATCH-INSTALL"        1 DECLARE ;

: ?XTOFF ( -- )
   AOT-WINDOW:XTOFF-N @ 1 = if exit then
   s" aot-chain-capture: declared address cells in the window=" type AOT-WINDOW:XTOFF-N @ .
   s" aot-chain-capture: and this tool installs one" type cr
   s" aot-chain-capture: an unlisted declared address cell would boot untrapped"
   REFUSE-RC die ;

: ?TRAPPED ( -- )
   AOT-ARM:B0 @ AOT-ARM:B1 @ AOT-ARM:D0 @ AOT-CAPTURE:TRAPPED-BELOW {: got:n :}
   got CELLS-OWED @ = if exit then
   s" aot-chain-capture: pre-window declared cells holding a window address=" type got .
   s" aot-chain-capture: cells the declared installers refill=" type CELLS-OWED @ .
   s" aot-chain-capture: a load-time installer this list does not name runs once here and never again"
   REFUSE-RC die ;

: BOOTRUN ( -- )
   ?XTOFF
   DECLARE-ALL
   ?TRAPPED ;

: RUN ( -- )
   ?FIRST
   ?WINDOW
   LATCH-CLOSURE
   PRE-R @ PRE-D @ AOT-CAPTURE:PRELUDE-MARK
   AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE
   BOOTRUN ;

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
   s" sigs=" type CHECKER-ASIG-N .
   s" sigbytes=" type CHECKER-ASIG-ROW-BYTES CHECKER-ASIG-STR-BYTES + .
   s" sigknown=" type AOT-CAPTURE:SIG-KNOWN .
   s" sigexempt=" type AOT-CAPTURE:SIG-EXEMPT .
   s" sigrows=" type AOT-SIG-N @ .
   s" sigstr=" type AOT-SIG-STR-LEN @ .
   s" reg=" type AOT-REG-LEN @ .
   s" datasz=" type AOT-DATA-SIZE @ .
   s" runs=" type AOT-WINDOW:RUN-N @ .
   s" runbytes=" type AOT-WINDOW:RBYTES-LEN @ .
   s" codespan=" type AOT-ARM:B1 @ AOT-ARM:B0 @ - .
   s" dataspan=" type AOT-ARM:D1 @ AOT-ARM:D0 @ - .
   s" bandrecs=" type AOT-ARM:R0 @ PRE-R @ - .
   s" bandbytes=" type AOT-ARM:D0 @ PRE-D @ - .
   s" widw0=" type AOT-ARM:W0 @ .
   s" widspan=" type AOT-ARM:W1 @ AOT-ARM:W0 @ - .
   s" pwin=" type AOT-PWIN-N @ .
   s" closure=" type AOT-IDENT:COUNT .
   s" first=" type 0 AOT-IDENT:PATH$ type cr
   s" last=" type AOT-IDENT:COUNT 1 - AOT-IDENT:PATH$ type cr
   CHAIN-SHA AOT-IDENT:CHAIN-DIGEST
   CHAIN-SHA HEX SHA256>HEX
   s" chaindigest=" type HEX 64 type cr
   s" producer=" type ENGINE-ID:KEY$ type cr ;

\ ---- the artifact, and the proof that writing it is a round trip -------------
\
\ THE PRODUCER KEY is the SHA-256 of the binary this process is running, taken
\ over the path lib/engine-id.f resolved for itself. The census prints the same
\ fact in hex through ENGINE-ID:KEY$, and test/aot-chain-capture-suite.f hashes
\ bin/hb from the outside and compares - so the key the artifact carries is a
\ reading of a file this suite has already pinned, not a claim.
\
\ THE ROUND TRIP RUNS EVERY TIME AN ARTIFACT IS WRITTEN, and it is the same
\ comparison the fixpoint loop promotes across generations: write A, destroy the
\ live buffers, read A back into them, write B from what came back, and require
\ sha256(A) = sha256(B). A reader that drops a section, reads a length short, or
\ fills the wrong buffer cannot survive it, and the writer's own digest of the
\ bytes it handed the descriptor is what is compared - not a re-read of the file.
\ B is written over A's path, so what is left on disk is one artifact and the
\ assertion says the two spellings of it were identical.
\
\ THE POISON is what makes the round trip adversarial. Zeroing the buffers would
\ let a reader that skips a section pass whenever the section happened to be zero
\ - the window's boot-run list and named code sites are empty today, and its
\ address-cell table holds one row. $A5 is a byte no captured section can be
\ mistaken for, and it goes over each section's own extent plus a margin past it,
\ so a read that stops short leaves poison inside the span it claimed to fill.

create PROD 32 allot
create SHA-A 32 allot
create AHEX 64 allot
$A5 constant POISON-BYTE
64 constant POISON-MARGIN

: SMEAR ( ptr u8 n n -- ) {: a:ptr u:n cap:n :}
   u POISON-MARGIN + cap > if cap else u POISON-MARGIN + then {: n:n :}
   n 0 ?do POISON-BYTE a i + c! loop ;

: POISON ( -- )
   AOT-BLOB-BUF@ AOT-BLOB-LEN @ AOT-BLOB-CAP SMEAR
   AOT-REC-BUF@ AOT-REC-MAX 48 * +  AOT-REC-N @ AOT-CREC-ROW *
      AOT-REC-MAX AOT-CREC-ROW * SMEAR
   AOT-SITE-BUF@ AOT-SITE-N @ SITE-ROW * AOT-SITE-MAX SITE-ROW * SMEAR
   AOT-NAMES-BUF@ AOT-NAMES-LEN @ AOT-NAMES-CAP SMEAR
   AOT-DSITE-BUF@ AOT-DSITE-N @ AOT-CSITE-N @ + 4 * AOT-DSITE-MAX 4 * SMEAR
   AOT-WINDOW:XTOFF-BUF@ AOT-WINDOW:XTOFF-N @ 4 * AOT-WINDOW:XTOFF-MAX 4 * SMEAR
   AOT-WINDOW:RUN-BUF@ AOT-WINDOW:RUN-N @ 8 * AOT-WINDOW:RUN-MAX 8 * SMEAR
   AOT-WINDOW:RBYTES-BUF@ AOT-WINDOW:RBYTES-LEN @ AOT-WINDOW:RBYTES-CAP SMEAR
   AOT-XTSITE:BUF@ AOT-XTSITE:N @ 8 * AOT-XTSITE:MAX 8 * SMEAR
   AOT-BOOTRUN-BUF@ AOT-BOOTRUN-LEN @ AOT-BOOTRUN-CAP SMEAR
   AOT-PWID-BUF@ PROT-BITS-BYTES PROT-BITS-BYTES SMEAR
   AOT-PWIN-BUF@ AOT-PWIN-N @ 4 * AOT-PWIN-MAX 4 * SMEAR
   AOT-SIG-BUF@ AOT-SIG-N @ SIG-ROW * AOT-SIG-MAX SIG-ROW * SMEAR
   AOT-SIG-STR-BUF@ AOT-SIG-STR-LEN @ AOT-SIG-STR-CAP SMEAR
   AOT-REG-BUF@ AOT-REG-LEN @ AOT-REG-CAP SMEAR
   0 AOT-BLOB-LEN !  0 AOT-REC-N !  0 AOT-SITE-N !  0 AOT-NAMES-LEN !
   0 AOT-DSITE-N !  0 AOT-CSITE-N !  0 AOT-WINDOW:XTOFF-N !  0 AOT-DATA-SIZE !
   0 AOT-WINDOW:RUN-N !  0 AOT-WINDOW:RBYTES-LEN !
   0 AOT-XTSITE:N !  0 AOT-BOOTRUN-LEN !  0 AOT-PWIN-N !
   0 AOT-SIG-N !  0 AOT-SIG-STR-LEN !  0 AOT-REG-LEN !
   0 AOT-DATA-D0 !  0 AOT-CODE-B0 !  0 AOT-WID-W0 !  0 AOT-WID-SPAN !
   AOT-IDENT:RESET ;

: SAME? ( ptr u8 ptr u8 n -- bool ) {: a:ptr b:ptr n:n :}
   n 0 ?do a i + c@ b i + c@ <> if false unloop exit then loop  true ;

: ?ROUND-TRIP ( -- )
   SHA-A AOT-FILE:SHA$ drop 32 SAME? if exit then
   s" aot-chain-capture: the artifact does not survive its own round trip"
   REFUSE-RC die ;

: ARTIFACT ( ptr u8 n -- ) {: path:ptr pathu:n :}
   ENGINE-ID:PATH$ PROD SHA256-FILE 0 <> if
      s" aot-chain-capture: cannot hash the engine that is running" REFUSE-RC die
   then
   PROD path pathu AOT-FILE:WRITE
   AOT-FILE:SHA$ drop SHA-A 32 BYTE-COPY
   POISON
   PROD path pathu AOT-FILE:READ
   PROD path pathu AOT-FILE:WRITE
   ?ROUND-TRIP
   SHA-A AHEX SHA256>HEX
   s" artifact=" type AHEX 64 type cr
   s" artifactpath=" type path pathu type cr
   s" roundtrip=ok" type cr ;

: MAIN ( -- )
   RUN
   CENSUS.
   SCRIPT-ARGC 0 > if 0 SCRIPT-ARGV$ ARTIFACT then ;

;package

AOT-CHAIN:MAIN

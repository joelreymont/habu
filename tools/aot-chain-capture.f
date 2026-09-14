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
\ The window prelude loads src/habu/layout.f and src/habu/aot-arm.f.
\ The latter exists precisely so that arming the window does not
\ drag aot-capture.f in ahead of the chain. The two are cheap for different
\ reasons, both measured in a booted bin/hb: layout.f is already registered
\ there, so its `require` adds 0 records and 0 DATA bytes and is kept only as the
\ dependency statement; aot-arm.f adds 4. asm.f, by contrast, is NOT registered —
\ requiring it in a booted engine compiles 178 records — which is the whole reason
\ it must be the chain that brings it in. NSTR loads inside the window too: a
\ retained native host already carries it, while a source-only host needs its
\ code captured with the compiler. Its fresh literal pool belongs to either
\ window.
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
\ WHICH BOOTED ENGINE, and it is not the installed product. Once the chain is
\ baked, every file its closure names is `provided` and the `require` below is a
\ registry no-op, so the window comes up empty and this tool says so: under bin/hb
\ it dies "the window is empty - the chain did not load". The engine that can run
\ it is the build's capture host, which carries the same prefix with no artifact
\ declared (tools/build-fixpoint.f BF-KEEP-HOST), and the build runs exactly this
\ command line in it (BF-PREPARE-CAPTURE-ARGV), from the repository root:
\   $HB_TMP/hb-host --load tools/aot-chain-capture.f -- <artifact>
\ It prints one `name=value` line per census field and exits 0, or refuses with
\ exit 74 and a named diagnostic — its own, or one of src/habu/aot-capture.f's.
\ test/aot-chain-capture-suite.f runs it under bin/hb for that refusal, which is
\ what proves this whole file and its closure still compile in a booted engine.

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
require src/compiler/native/string.f
NSTR:WINDOW-OPEN
require src/compiler/native/compiler.f
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
require lib/sort.f

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

\ Address rows restore the window's declared pointers and vectors, including
\ their exact targets. DKEEP-HOOK-DEFAULT remains the explicit diagnostic-hook
\ initialization the compiler's boot contract asks for; it is no longer a
\ substitute for transporting every declared cell.
\
\ A code cell below the window is a separate load-time effect. NFEED now installs
\ its checker observer per compilation unit, so this tool owes no such installer.
\ Keep the independent live count below: a new pre-window installation must be
\ named, even when its address row also travels.
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
   s" A64RAV:DKEEP-HOOK-DEFAULT" 0 DECLARE ; \ its cell is in the window: see ?XTOFF

\ The rows preserve declared locations, kinds and exact null/window-relative
\ targets. Check them against the live declarations, independently of the row
\ writer. Counts alone cannot detect one missing row replaced by a duplicate.
TRUSTED: DATA-N ( -- n ) data-base ;

: ROW-U32@ ( ptr u8 -- n ) {: p:ptr :}
   p c@ p 1+ c@ 8 lshift or p 2 + c@ 16 lshift or p 3 + c@ 24 lshift or ;

DYNAMIC-BUFFER ROW-INDEX n
DYNAMIC-BUFFER NAME-INDEX n
variable NAME-COUNT

: RELEASE-ROW-INDEX ( -- ) ROW-INDEX-RELEASE NAME-INDEX-RELEASE ;

: ROW-REFUSE ( -- )
   RELEASE-ROW-INDEX
   s" aot-chain-capture: declared address rows do not match the live window"
   REFUSE-RC die ;

: LIVE-ROW ( n -- n )
   ADDRESS-CELLS:ROW@ ;

\ Sort a temporary copy, keeping the artifact's row order intact. Signed cell
\ ordering is sufficient for the packed pairs: both sort and lookup use it.
: INDEX-ROWS ( -- )
   AOT-WINDOW:XTOFF-N @ {: rows:n :}
   rows 0 < rows AOT-WINDOW:XTOFF-MAX > or if ROW-REFUSE then
   rows 0= if exit then
   rows ROW-INDEX-RESERVE
   AOT-WINDOW:XTOFF-N @ 0 ?do
      AOT-WINDOW:XTOFF-BUF@ i AOT-WINDOW:XTOFF-ROW * + {: row:ptr :}
      row ROW-U32@ 32 lshift row 4 + ROW-U32@ or i ROW-INDEX !
   loop
   0 ROW-INDEX AOT-WINDOW:XTOFF-N @ [: < ;] SORT:SORT!
   AOT-WINDOW:XTOFF-N @ 1 ?do
      i ROW-INDEX @ 32 rshift i 1- ROW-INDEX @ 32 rshift = if ROW-REFUSE then
   loop ;

\ The producer's pool is walked once. This ordered entry list is independent
\ of the artifact reader's boundary bitmap and does not outlive the check.
: INDEX-NAMES ( -- )
   0 NAME-COUNT !
   0
   begin dup AOT-NAMES-LEN @ < while
      dup {: off:n :}
      AOT-NAMES-BUF@ off + c@ {: size:n :}
      size AOT-NAMES-LEN @ off - 1- > if ROW-REFUSE then
      size 0 > if
         NAME-COUNT @ 1+ NAME-INDEX-RESERVE
         off NAME-COUNT @ NAME-INDEX ! 1 NAME-COUNT +!
      then
      size 1+ +
   repeat drop ;

: NAME-ENTRY? ( n n n -- bool ) {: off:n low:n high:n :}
   low high >= if false exit then
   low high low - 2 / + {: mid:n :}
   mid NAME-INDEX @ {: entry:n :}
   entry off = if true exit then
   entry off < if off mid 1+ high else off low mid then recurse ;

: ROW-HAS? ( n n n -- bool ) {: pair:n low:n high:n :}
   low high >= if false exit then
   low high low - 2 / + {: mid:n :}
   mid ROW-INDEX @ {: key:n :}
   key pair = if true exit then
   key pair < if pair mid 1+ high else pair low mid then recurse ;

: ?EXACT-ROW ( n n -- )
   swap 32 lshift or 0 AOT-WINDOW:XTOFF-N @ ROW-HAS? 0= if ROW-REFUSE then ;

: ROW-TARGET ( n n n -- n ) {: location:n low:n high:n :}
   low high >= if ROW-REFUSE then
   low high low - 2 / + {: mid:n :}
   mid ROW-INDEX @ {: pair:n :}
   pair $FFFFFFFF00000000 and {: entry:n :}
   entry location = if pair $FFFFFFFF and exit then
   entry location < if location mid 1+ high else location low mid then recurse ;

: ?NAMED-ROW ( n n -- ) {: loc:n target:n :}
   loc 32 lshift 0 AOT-WINDOW:XTOFF-N @ ROW-TARGET {: meta:n :}
   meta AOT-WINDOW:XTOFF-KIND-MASK and AOT-WINDOW:XTOFF-NAME-TAG <> if ROW-REFUSE then
   meta AOT-WINDOW:XTOFF-VALUE-MASK and 1- {: off:n :}
   off 0 NAME-COUNT @ NAME-ENTRY? 0= if ROW-REFUSE then
   AOT-NAMES-BUF@ off + {: name:ptr :}
   name 1+ name c@ XREF-FIND {: rec:ptr :}
   rec XREF-FOUND? 0= if ROW-REFUSE then
   rec XREF-START target <> if ROW-REFUSE then ;

: ?DECLARED-ROW ( n -- ) {: raw:n :}
   raw SNAP-RELOC:XTCELL-OFF-MASK and {: off:n :}
   DATA-N off + {: at:n :}
   at AOT-ARM:D0 @ < at CELL + AOT-ARM:D0 @ > and if ROW-REFUSE then
   at AOT-ARM:D0 @ >= at AOT-ARM:D1 @ < and {: inside:bool :}
   raw SNAP-RELOC:XTCELL-DATA-TAG and 0<> {: data?:bool :}
   data? if AOT-ARM:D0 @ AOT-ARM:D1 @ else AOT-ARM:B0 @ AOT-ARM:B1 @ then {: lo:n hi:n :}
   data-base off + @ {: target:n :}
   inside target lo >= target hi < and or 0= if exit then
   inside if
      at AOT-ARM:D1 @ CELL - > if ROW-REFUSE then
      at AOT-ARM:D0 @ - AOT-WINDOW:XTOFF-WINDOW-TAG or
   else off then {: loc:n :}
   data? 0= target 0<> and if
      target lo < target hi >= or if loc target ?NAMED-ROW exit then
   then
   target 0= if 0 else
      target lo < target hi >= or if ROW-REFUSE then
      target lo - 1+
   then
   data? if AOT-WINDOW:XTOFF-DATA-TAG or then {: meta:n :}
   loc meta ?EXACT-ROW ;

\ The compiler's hook must still be a declared code cell in this window, with
\ a live captured target. This preserves the original fixture's substantive
\ check without treating every other DATA pointer or vector as another hook.
: ?HOOK-CELL ( -- )
   s" A64RAV:DKEEP-HOOK" NDICT:SPELL-DEFER-CELL {: at:n :}
   at AOT-ARM:D0 @ < at AOT-ARM:D1 @ CELL - > or if ROW-REFUSE then
   at DATA-N - {: off:n :}
   data-base off + @ {: target:n :}
   target AOT-ARM:B0 @ < target AOT-ARM:B1 @ >= or if ROW-REFUSE then
   0
   data-base SNAP-RELOC:XTCELL-N-CELL + @ 0 ?do
      i LIVE-ROW off = if 1+ then
   loop
   1 <> if ROW-REFUSE then ;

: CHECK-ROWS ( -- )
   INDEX-ROWS INDEX-NAMES
   data-base SNAP-RELOC:XTCELL-N-CELL + @ 0 ?do i LIVE-ROW ?DECLARED-ROW loop
   ?HOOK-CELL ;

: ?XTOFF ( -- )
   AOT-ARM:D0 @ AOT-ARM:D1 @ AOT-CAPTURE:DECLARED-IN {: win:n :}
   AOT-ARM:B0 @ AOT-ARM:B1 @ AOT-ARM:D0 @ AOT-ARM:D1 @ AOT-CAPTURE:TARGETED-OUT {: out:n :}
   AOT-WINDOW:XTOFF-N @ win out + <> if ROW-REFUSE then
   \ Heapsort M rows, then at most N binary lookups: O(M log M + N log M).
   [: CHECK-ROWS ;] [: RELEASE-ROW-INDEX ;] finally ;

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
: CAPTURE-CENSUS. ( -- )
   s" recs=" type AOT-REC-N @ .
   s" sites=" type AOT-SITE-N @ .
   s" blob=" type AOT-BLOB-LEN @ .
   s" names=" type AOT-NAMES-LEN @ .
   s" dsites=" type AOT-DSITE-N @ .
   s" csites=" type AOT-CSITE-N @ .
   s" xtsites=" type AOT-XTSITE:N @ .
   s" xtoff=" type AOT-WINDOW:XTOFF-N @ . ;

: METADATA-CENSUS. ( -- )
   s" sigs=" type CHECKER-ASIG-N .
   s" sigbytes=" type CHECKER-ASIG-ROW-BYTES CHECKER-ASIG-STR-BYTES + .
   s" sigknown=" type AOT-CAPTURE:SIG-KNOWN .
   s" sigexempt=" type AOT-CAPTURE:SIG-EXEMPT .
   s" sigrows=" type AOT-SIG-N @ .
   s" sigstr=" type AOT-SIG-STR-LEN @ .
   s" reg=" type AOT-REG-LEN @ . ;

: WINDOW-CENSUS. ( -- )
   s" datasz=" type AOT-DATA-SIZE @ .
   s" runs=" type AOT-WINDOW:RUN-N @ .
   s" runbytes=" type AOT-WINDOW:RBYTES-LEN @ .
   s" codespan=" type AOT-ARM:B1 @ AOT-ARM:B0 @ - .
   s" dataspan=" type AOT-ARM:D1 @ AOT-ARM:D0 @ - .
   s" bandrecs=" type AOT-ARM:R0 @ PRE-R @ - .
   s" bandbytes=" type AOT-ARM:D0 @ PRE-D @ - .
   s" widw0=" type AOT-ARM:W0 @ .
   s" widspan=" type AOT-ARM:W1 @ AOT-ARM:W0 @ - .
   s" pwin=" type AOT-PWIN-N @ . ;

: IDENTITY-CENSUS. ( -- )
   s" closure=" type AOT-IDENT:COUNT .
   s" first=" type 0 AOT-IDENT:PATH$ type cr
   s" last=" type AOT-IDENT:COUNT 1 - AOT-IDENT:PATH$ type cr
   CHAIN-SHA AOT-IDENT:CHAIN-DIGEST
   CHAIN-SHA HEX SHA256>HEX
   s" chaindigest=" type HEX 64 type cr
   s" producer=" type ENGINE-ID:KEY$ type cr ;

: CENSUS. ( -- )
   CAPTURE-CENSUS.
   METADATA-CENSUS.
   WINDOW-CENSUS.
   IDENTITY-CENSUS. ;

\ ---- the artifact, and the proof that writing it is a round trip -------------
\
\ THE PRODUCER KEY is the SHA-256 of the binary this process is running, taken
\ over the path lib/engine-id.f resolved for itself, and the census prints the
\ same fact in hex through ENGINE-ID:KEY$. test/aot-chain-capture-suite.f hashes
\ bin/hb from the outside and compares it against the key in an artifact's
\ header, so that field is a reading of a file the suite has pinned rather than a
\ claim. The artifact it reads is the small-window one test/aot-artifact-roundtrip.f
\ writes, because a product engine cannot capture the chain at all; what that
\ suite proves about THIS file is that it loads in a booted engine and refuses by
\ name there.
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
\ - captured sections can be empty. $A5 is a byte no captured section can be
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

: POISON-CAPTURE ( -- )
   AOT-BLOB-BUF@ AOT-BLOB-LEN @ AOT-BLOB-CAP SMEAR
   AOT-REC-BUF@ AOT-REC-MAX 48 * +  AOT-REC-N @ AOT-CREC-ROW *
      AOT-REC-MAX AOT-CREC-ROW * SMEAR
   AOT-SITE-BUF@ AOT-SITE-N @ SITE-ROW * AOT-SITE-MAX SITE-ROW * SMEAR
   AOT-NAMES-BUF@ AOT-NAMES-LEN @ AOT-NAMES-CAP SMEAR ;

: POISON-WINDOW ( -- )
   AOT-DSITE-N @ AOT-CSITE-N @ + POISON-MARGIN 4 / +
   AOT-DSITE-MAX min AOT-DSITE-RESERVE
   AOT-DSITE-BUF@ AOT-DSITE-N @ AOT-CSITE-N @ + 4 * AOT-DSITE-MAX 4 * SMEAR
   AOT-WINDOW:XTOFF-N @ POISON-MARGIN AOT-WINDOW:XTOFF-ROW / +
   AOT-WINDOW:XTOFF-MAX min AOT-WINDOW:XTOFF-RESERVE
   AOT-WINDOW:XTOFF-BUF@ AOT-WINDOW:XTOFF-N @ AOT-WINDOW:XTOFF-ROW *
   AOT-WINDOW:XTOFF-MAX AOT-WINDOW:XTOFF-ROW * SMEAR
   AOT-WINDOW:RUN-BUF@ AOT-WINDOW:RUN-N @ 8 * AOT-WINDOW:RUN-MAX 8 * SMEAR
   AOT-WINDOW:RBYTES-BUF@ AOT-WINDOW:RBYTES-LEN @ AOT-WINDOW:RBYTES-CAP SMEAR ;

: POISON-BOOT ( -- )
   AOT-XTSITE:BUF@ AOT-XTSITE:N @ 8 * AOT-XTSITE:MAX 8 * SMEAR
   AOT-BOOTRUN-BUF@ AOT-BOOTRUN-LEN @ AOT-BOOTRUN-CAP SMEAR
   AOT-PWIN-BUF@ AOT-PWIN-N @ 4 * AOT-PWIN-MAX 4 * SMEAR ;

: POISON-METADATA ( -- )
   AOT-SIG-BUF@ AOT-SIG-N @ SIG-ROW * AOT-SIG-MAX SIG-ROW * SMEAR
   AOT-SIG-STR-LEN @ POISON-MARGIN + AOT-SIG-STR-CAP min AOT-SIG-STR-RESERVE
   AOT-SIG-STR-BUF@ AOT-SIG-STR-LEN @ AOT-SIG-STR-CAP SMEAR
   AOT-REG-BUF@ AOT-REG-LEN @ AOT-REG-CAP SMEAR ;

: RESET-CAPTURE ( -- )
   0 AOT-BLOB-LEN !  0 AOT-REC-N !  0 AOT-SITE-N !  0 AOT-NAMES-LEN !
   0 AOT-DSITE-N !  0 AOT-CSITE-N !  0 AOT-WINDOW:XTOFF-N !  0 AOT-DATA-SIZE !
   0 AOT-WINDOW:RUN-N !  0 AOT-WINDOW:RBYTES-LEN !
   0 AOT-XTSITE:N !  0 AOT-BOOTRUN-LEN !  0 AOT-PWIN-N !
   0 AOT-SIG-N !  0 AOT-SIG-STR-LEN !  0 AOT-REG-LEN !
   0 AOT-DATA-D0 !  0 AOT-CODE-B0 !  0 AOT-WID-W0 !  0 AOT-WID-SPAN !
   AOT-IDENT:RESET ;

: POISON ( -- )
   POISON-CAPTURE
   POISON-WINDOW
   POISON-BOOT
   POISON-METADATA
   RESET-CAPTURE ;

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

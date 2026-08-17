\ include.f - checked source include words.
\
\ `include` is source composition. Package reopening owns shared namespace;
\ this file only gives source files a checked way to load dependencies.

$400 constant INCLUDE-PATH-CAP
$80000 constant INCLUDE-BUF-CAP
$10 constant INCLUDE-MAX-DEPTH  \ typed-lib require chains outgrew 8 (2026-07-15)
$200 constant REQUIRE-MAX  \ composed maki+stdlib require closure crossed 256 (2026-07-20)
$1002 constant INCLUDE-MAP-PRIVATE-ANON
$1 constant INCLUDE-PROBE-CAP
$4A constant INCLUDE-IO-RC
$46 constant INCLUDE-EVAL-RC
$37D8 constant INCLUDE-EVALERR-CELL
INCLUDE-MAX-DEPTH INCLUDE-BUF-CAP * constant INCLUDE-BUF-TOTAL
INCLUDE-PATH-CAP 1 + constant REQUIRE-SLOT-BYTES

create INCLUDE-PATH INCLUDE-PATH-CAP 1 + allot
create INCLUDE-PROBE INCLUDE-PROBE-CAP allot
create REQUIRE-PATHS REQUIRE-MAX REQUIRE-SLOT-BYTES * allot
create REQUIRE-LENS REQUIRE-MAX cells allot

variable INCLUDE-BUFS-A
variable INCLUDE-DEPTH
variable INCLUDE-FD
variable INCLUDE-U
variable INCLUDE-RD
variable INCLUDE-PATH-A
variable INCLUDE-PATH-U
variable INCLUDE-PATH-I
variable REQUIRE-N
variable REQUIRE-BOOT-N
variable REQUIRE-BASE
variable REQUIRE-SAVE-N
variable REQUIRE-SAVE-BASE

-1 INCLUDE-FD !

: INCLUDE-FALSE ( -- bool )
   0 0= 0= ;

: INCLUDE-TRUE ( -- bool )
   0 0= ;

\ INCLUDE-MMAP-PTR refines the checked file mapping; INCLUDE-EVALUATE executes
\ its validated mapped bytes. Syscall-result provenance and evaluate are primitive
\ boundaries. Retirement: habu-primitive-effect-axiom-1119f176.
TRUSTED: INCLUDE-MMAP-PTR ( n -- ptr u8 ) ;

: INCLUDE-DIE ( ptr u8 n -- )
   INCLUDE-IO-RC die ;

: INCLUDE-EVAL-DIE ( ptr u8 n -- )
   INCLUDE-EVAL-RC die ;

: INCLUDE-CLOSE ( -- )
   INCLUDE-FD @ dup 0 >= if
      close
   else
      drop
   then
   -1 INCLUDE-FD ! ;

: INCLUDE-IO-DIE ( ptr u8 n -- )
   INCLUDE-CLOSE
   INCLUDE-DIE ;

: INCLUDE-PATH-A-FIELD ( -- ptr ptr u8 )
   INCLUDE-PATH-A 0 ptr-field ;

: INCLUDE-PATH-A@ ( -- ptr u8 )
   INCLUDE-PATH-A-FIELD @ ;

: INCLUDE-PATH-A! ( ptr u8 -- )
   INCLUDE-PATH-A-FIELD ! ;

: INCLUDE-CHECK-PATH ( ptr u8 n -- ptr u8 n )
   dup 0 <= if s" include: missing path" INCLUDE-DIE then
   dup INCLUDE-PATH-CAP > if s" include: path too long" INCLUDE-DIE then ;

: REQUIRE-SLOT ( n -- ptr u8 )
   REQUIRE-SLOT-BYTES * REQUIRE-PATHS + ;

: REQUIRE-LEN@ ( n -- n )
   cells REQUIRE-LENS + @ ;

: REQUIRE-LEN! ( n n -- ) {: u:n idx:n :}
   u REQUIRE-LENS idx cells + ! ;

: REQUIRE-BYTE= ( ptr u8 n n -- bool ) {: a:ptr idx:n i:n :}
   a i ZBYTE@ idx REQUIRE-SLOT i ZBYTE@ = ;

: REQUIRE-PATH= ( ptr u8 n n -- bool ) {: a:ptr u:n idx:n :}
   idx REQUIRE-LEN@ u <> if INCLUDE-FALSE exit then
   0 begin dup u < while
      dup a idx rot REQUIRE-BYTE= 0= if drop INCLUDE-FALSE exit then
      1+
   repeat drop INCLUDE-TRUE ;

: REQUIRE-KNOWN? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   REQUIRE-BASE @ begin dup REQUIRE-N @ < while
      dup a u rot REQUIRE-PATH= if drop INCLUDE-TRUE exit then
      1+
   repeat drop INCLUDE-FALSE ;

: REQUIRE-CHECK-ROOM ( -- )
   REQUIRE-N @ REQUIRE-MAX >= if s" require: too many files" INCLUDE-DIE then ;

: REQUIRE-STORE ( ptr u8 n -- ) {: a:ptr u:n :}
   REQUIRE-CHECK-ROOM
   REQUIRE-N @ {: idx:n :}
   a idx REQUIRE-SLOT u BYTE-COPY
   u idx REQUIRE-LEN!
   idx 1 + REQUIRE-N ! ;

\ One scratch line for diagnostics that have to name a path. Sized so the
\ longest accepted path plus the longest prefix below always fits, and the
\ append refuses rather than truncates, so a message is whole or absent.
$20 constant INCLUDE-DIAG-PREFIX-CAP
INCLUDE-PATH-CAP INCLUDE-DIAG-PREFIX-CAP + constant INCLUDE-DIAG-CAP
create INCLUDE-DIAG INCLUDE-DIAG-CAP allot
create INCLUDE-LF 1 allot
variable INCLUDE-DIAG-U

$0A INCLUDE-LF 0 ZBYTE!

: INCLUDE-DIAG-RESET ( -- )
   0 INCLUDE-DIAG-U ! ;

: INCLUDE-DIAG+ ( ptr u8 n -- ) {: a:ptr u:n :}
   INCLUDE-DIAG-U @ u + INCLUDE-DIAG-CAP > if exit then
   a INCLUDE-DIAG INCLUDE-DIAG-U @ + u BYTE-COPY
   INCLUDE-DIAG-U @ u + INCLUDE-DIAG-U ! ;

: INCLUDE-DIAG$ ( -- ptr u8 n )
   INCLUDE-DIAG INCLUDE-DIAG-U @ ;

: INCLUDE-PATH-COPY ( -- )
   0 INCLUDE-PATH-I !
   begin INCLUDE-PATH-I @ INCLUDE-PATH-U @ < while
      INCLUDE-PATH-A@ INCLUDE-PATH-I @ ZBYTE@ INCLUDE-PATH INCLUDE-PATH-I @ ZBYTE!
      INCLUDE-PATH-I @ 1 + INCLUDE-PATH-I !
   repeat ;

: INCLUDE-PATH0 ( ptr u8 n -- ptr u8 )
   INCLUDE-CHECK-PATH INCLUDE-PATH-U ! INCLUDE-PATH-A!
   INCLUDE-PATH-COPY
   0 INCLUDE-PATH INCLUDE-PATH-U @ ZBYTE!
   INCLUDE-PATH ;

: INCLUDE-CHECK-DEPTH ( n -- )
   dup 0 < if s" include: depth underflow" INCLUDE-DIE then
   INCLUDE-MAX-DEPTH >= if s" include: nested too deeply" INCLUDE-DIE then ;

\ ---- which loads the command line asked for, by name ---------------------
\
\ `bin/hb --load a.f b.f` appends one `s" a.f" script-required` per argv file
\ (src/habu/habu2.f C-SOURCE-APPEND-ARG), so the loader knows which loads the
\ user named and which are dependencies pulled in underneath them. A tool that
\ must act only when it IS the entry point - tools/build-fixpoint.f runs its
\ CLI verb then, and must stay inert when another tool requires it - used to
\ read INCLUDE-DEPTH 0 for that, which was true only while `--load` inlined its
\ argv files into the top-level stream. The depth was a proxy; this is the
\ fact, and it is recorded per frame so a nested require inside a named file
\ answers no.
create SCRIPT-NAMED-FRAME INCLUDE-MAX-DEPTH cells allot
variable SCRIPT-NAMED-PEND              \ the next INCLUDE-PUSH opens a named frame

: SCRIPT-NAMED-SLOT ( n -- ptr a )
   cells SCRIPT-NAMED-FRAME + ;

: SCRIPT-NAMED-PEND! ( bool -- )
   SCRIPT-NAMED-PEND ! ;

: INCLUDE-PUSH ( -- )
   INCLUDE-DEPTH @ INCLUDE-CHECK-DEPTH
   SCRIPT-NAMED-PEND @ INCLUDE-DEPTH @ SCRIPT-NAMED-SLOT !
   INCLUDE-FALSE SCRIPT-NAMED-PEND!
   INCLUDE-DEPTH @ 1 + INCLUDE-DEPTH ! ;

: INCLUDE-POP ( -- )
   INCLUDE-DEPTH @ 1 - dup INCLUDE-CHECK-DEPTH
   INCLUDE-DEPTH ! ;

: INCLUDE-BUFS@ ( -- ptr u8 )
   INCLUDE-BUFS-A @ INCLUDE-MMAP-PTR ;

: INCLUDE-ALLOC-BUFS ( -- )
   INCLUDE-BUFS-A @ 0= if
      0 INCLUDE-BUF-TOTAL 3 INCLUDE-MAP-PRIVATE-ANON -1 0 mmap
      dup 0 < if s" include: buffer mmap failed" INCLUDE-DIE then
      INCLUDE-BUFS-A !
   then ;

: INCLUDE-SLOT ( -- ptr u8 )
   INCLUDE-ALLOC-BUFS
   INCLUDE-DEPTH @ 1 - dup INCLUDE-CHECK-DEPTH
   INCLUDE-BUF-CAP * INCLUDE-BUFS@ + ;

\ A failed open is almost always a typo or a moved file, and the one thing the
\ reader needs is WHICH path. The message used to drop it even though
\ INCLUDE-PATH holds the resolved path at exactly that moment, so a bad
\ `require` said only "include: open failed". Now that `bin/hb --load` routes
\ its argv files through `required` too (dot habu-make-load-consult-85c88fb3),
\ this is the diagnostic a mistyped command line gets, and the raw argv reader
\ it replaced always named the path.
: INCLUDE-OPEN-DIE ( -- )
   INCLUDE-DIAG-RESET
   s" include: cannot open " INCLUDE-DIAG+
   INCLUDE-PATH INCLUDE-PATH-U @ INCLUDE-DIAG+
   INCLUDE-LF 1 INCLUDE-DIAG+
   INCLUDE-DIAG$ INCLUDE-DIE ;

: INCLUDE-OPEN ( ptr u8 n -- )
   INCLUDE-PATH0 open-rd INCLUDE-FD !
   INCLUDE-FD @ 0 < if INCLUDE-OPEN-DIE then ;

: INCLUDE-PROBE-OVERFLOW ( -- bool )
   INCLUDE-FD @ INCLUDE-PROBE INCLUDE-PROBE-CAP read INCLUDE-RD !
   INCLUDE-RD @ 0 < if s" include: read failed" INCLUDE-IO-DIE then
   INCLUDE-RD @ 0 > if s" include: file too large" INCLUDE-IO-DIE then
   INCLUDE-TRUE ;

: INCLUDE-READ-DONE? ( -- bool )
   INCLUDE-U @ INCLUDE-BUF-CAP >= if INCLUDE-PROBE-OVERFLOW exit then
   INCLUDE-FD @ INCLUDE-SLOT INCLUDE-U @ + INCLUDE-BUF-CAP INCLUDE-U @ - read INCLUDE-RD !
   INCLUDE-RD @ 0 < if s" include: read failed" INCLUDE-IO-DIE then
   INCLUDE-RD @ 0 = if INCLUDE-TRUE exit then
   INCLUDE-U @ INCLUDE-RD @ + INCLUDE-U !
   INCLUDE-FALSE ;

: INCLUDE-READ-ALL ( ptr u8 n -- ptr u8 n )
   INCLUDE-OPEN
   0 INCLUDE-U !
   begin INCLUDE-READ-DONE? 0= while repeat
   INCLUDE-CLOSE
   INCLUDE-SLOT INCLUDE-U @ ;

: INCLUDE-EVALERR? ( -- bool )
   data-base INCLUDE-EVALERR-CELL + @ 0 = 0= ;

TRUSTED: INCLUDE-EVALUATE ( ptr u8 n -- )
   evaluate ;

\ ---- Ordered source-composition event log (TFAM 5, item 5) --------------
\ The loader words append one event per source-composition act so a restricted
\ discovery pass can reconstruct include multiplicity and require/provided
\ exact-string registry state in order. Recording is gated by EVENT-ON? so a
\ normal boot/gate records nothing (no overhead, no overflow). During discovery
\ the walker sets DISCOVERY and supplies the loader-token byte span in
\ DISC-TOK-A/DISC-TOK-U; a real load reads the live token span from the
\ interpreter TKA/TKL cells instead.

$100 constant EVENT-MAX
6 constant EVENT-FIELDS
$8000 constant EVENT-POOL-CAP
$4D constant INCLUDE-EVENT-RC

0 constant EV-INCLUDED
1 constant EV-REQUIRED
2 constant EV-PROVIDED
0 constant EV-STATE-FRESH
1 constant EV-STATE-KNOWN

create EVENT-RECS EVENT-MAX EVENT-FIELDS * cells allot
create EVENT-POOL EVENT-POOL-CAP allot
variable EVENT-N
variable EVENT-POOL-N
variable EVENT-ON-V
variable EVENT-DISC-V
variable DISC-TOK-A
variable DISC-TOK-U

: EVENT-ON? ( -- bool )   EVENT-ON-V @ 0= 0= ;
: DISCOVERY? ( -- bool )  EVENT-DISC-V @ 0= 0= ;
: EVENT-ON ( -- )         1 EVENT-ON-V ! ;
: EVENT-OFF ( -- )        0 EVENT-ON-V ! ;
: DISCOVERY-ON ( -- )     1 EVENT-DISC-V ! ;
: DISCOVERY-OFF ( -- )    0 EVENT-DISC-V ! ;
: DISC-TOK! ( a n -- )    DISC-TOK-U ! DISC-TOK-A ! ;
: EVENTS-RESET ( -- )     0 EVENT-N !  0 EVENT-POOL-N ! ;

: LOADER-TOK-A ( -- a )   data-base TKA-CELL + @ ;
: LOADER-TOK-U ( -- a )   data-base TKL-CELL + @ ;
: LOADER-TOKEN-SPAN ( -- a a )  LOADER-TOK-A LOADER-TOK-U ;

: EVENT-SPAN ( -- a a )
   DISCOVERY? if DISC-TOK-A @ DISC-TOK-U @ exit then
   LOADER-TOKEN-SPAN ;

: EVENT-SLOT ( n n -- ptr a )
   swap EVENT-FIELDS * + cells EVENT-RECS + ;

: EVENT-FIELD@ ( n n -- a )  EVENT-SLOT @ ;
: EVENT-FIELD! ( a n n -- )  EVENT-SLOT ! ;

: EVENT-POOL-AT ( n -- ptr u8 )  EVENT-POOL + ;

: EVENT-RECS-ROOM ( -- )
   EVENT-N @ EVENT-MAX >= if s" events: too many events" INCLUDE-EVENT-RC die then ;

: EVENT-POOL-ROOM ( n -- )
   EVENT-POOL-N @ + EVENT-POOL-CAP > if s" events: pool overflow" INCLUDE-EVENT-RC die then ;

: EVENT-COPY-PATH ( ptr u8 n -- n n ) {: a:ptr u:n :}
   u EVENT-POOL-ROOM
   EVENT-POOL-N @ {: off:n :}
   a off EVENT-POOL-AT u BYTE-COPY
   off u + EVENT-POOL-N !
   off u ;

: EVENT-RECORD ( ptr u8 n n n -- ) {: kd:n st:n :}
   EVENT-ON? 0= if 2drop exit then
   EVENT-RECS-ROOM
   EVENT-N @ {: ix:n :}
   EVENT-SPAN {: toka:n toku:n :}
   EVENT-COPY-PATH {: off:n len:n :}
   kd    ix 0 EVENT-FIELD!
   off   ix 1 EVENT-FIELD!
   len   ix 2 EVENT-FIELD!
   toka  ix 3 EVENT-FIELD!
   toku  ix 4 EVENT-FIELD!
   st    ix 5 EVENT-FIELD!
   ix 1 + EVENT-N ! ;

: REQUIRE-STATE ( bool -- n )
   if EV-STATE-KNOWN exit then EV-STATE-FRESH ;

: EVENT-COUNT ( -- n )       EVENT-N @ ;
: EVENT-KIND@ ( n -- n )     0 EVENT-FIELD@ ;
: EVENT-STATE@ ( n -- n )    5 EVENT-FIELD@ ;
: EVENT-PATH@ ( n -- ptr u8 n ) {: ix:n :}
   ix 1 EVENT-FIELD@ EVENT-POOL-AT
   ix 2 EVENT-FIELD@ ;
: EVENT-TOK@ ( n -- a a ) {: ix:n :}
   ix 3 EVENT-FIELD@ ix 4 EVENT-FIELD@ ;

: INCLUDE-LOAD ( ptr u8 n -- )
   INCLUDE-PUSH
   INCLUDE-READ-ALL INCLUDE-EVALUATE
   INCLUDE-POP
   INCLUDE-EVALERR? if s" include: evaluation failed" INCLUDE-EVAL-DIE then ;

: included ( ptr u8 n -- )
   2dup EV-INCLUDED EV-STATE-FRESH EVENT-RECORD
   DISCOVERY? if 2drop exit then
   INCLUDE-LOAD ;

\ One body for both spellings. A path the registry already holds is skipped, so
\ the pending flag has to be cleared on every exit or it would leak into the
\ next unrelated load.
: REQUIRE-BODY ( ptr u8 n -- )
   INCLUDE-CHECK-PATH
   2dup REQUIRE-KNOWN? {: known:bool :}
   2dup EV-REQUIRED known REQUIRE-STATE EVENT-RECORD
   known if 2drop INCLUDE-FALSE SCRIPT-NAMED-PEND! exit then
   2dup REQUIRE-STORE
   DISCOVERY? if 2drop INCLUDE-FALSE SCRIPT-NAMED-PEND! exit then
   INCLUDE-LOAD ;

: required ( ptr u8 n -- )
   INCLUDE-FALSE SCRIPT-NAMED-PEND!
   REQUIRE-BODY ;

\ The `--load` argv row. Same load, and it records that the command line is
\ what asked for it.
: script-required ( ptr u8 n -- )
   INCLUDE-TRUE SCRIPT-NAMED-PEND!
   REQUIRE-BODY ;

\ Is the file being loaded right now one the command line named?
: SCRIPT-NAMED-LOAD? ( -- bool )
   INCLUDE-DEPTH @ 0= if INCLUDE-FALSE exit then
   INCLUDE-DEPTH @ 1 - SCRIPT-NAMED-SLOT @ 0= 0= ;

: provided ( ptr u8 n -- )
   INCLUDE-CHECK-PATH
   2dup REQUIRE-KNOWN? {: known:bool :}
   2dup EV-PROVIDED known REQUIRE-STATE EVENT-RECORD
   known if 2drop exit then
   REQUIRE-STORE ;

: include ( -- )
   parse-name INCLUDE-CHECK-PATH included ;
immediate

: require ( -- )
   parse-name INCLUDE-CHECK-PATH required ;
immediate

\ ---- Fresh discovery registry (TFAM 5, item 5) --------------------------
\ A restricted discovery pass must see a fresh require/provided registry so a
\ tool's own preloaded paths cannot dedup-hide a later user require/provided.
\ Raising REQUIRE-BASE to the current count makes REQUIRE-KNOWN? ignore the
\ tool's entries while discovery records into slots above the base; RESTORE
\ drops the discovery entries and reinstates the tool's registry unchanged, so
\ warm-snapshot serialization of the registry stays intact.

: REQUIRE-SNAPSHOT ( -- )
   REQUIRE-N @ REQUIRE-SAVE-N !
   REQUIRE-BASE @ REQUIRE-SAVE-BASE !
   REQUIRE-N @ REQUIRE-BASE ! ;

: REQUIRE-RESTORE ( -- )
   REQUIRE-SAVE-BASE @ REQUIRE-BASE !
   REQUIRE-SAVE-N @ REQUIRE-N ! ;

\ ---- the registry's own truncation seam -------------------------------------
\ THE ONE WAY TO MAKE THIS REGISTRY SMALLER, and it exists because storing the
\ count is not a truncation. Four other cells are cursors INTO the rows, and a
\ bare `REQUIRE-N !` leaves every one of them naming rows that are gone:
\ REQUIRE-BOOT-N would go on answering ENGINE-PROVIDES? for a file the caller
\ just dropped, REQUIRE-BASE would hide surviving rows from REQUIRE-KNOWN?
\ (dedup silently off), and the two SAVE cells would restore a discovery pass
\ back to the vanished rows. So each is brought down with the count, and none of
\ them can end up above it.
\
\ ITS CALLER IS THE BUILD'S CORE-PREFIX REWIND (src/habu/prefix-rewind.f), which
\ returns a compiling host to the end of its own boot prefix: the rows the boot
\ recorded after that point describe files whose definitions the rewind removes,
\ and a snapshot taken afterwards would otherwise persist an image that claims
\ to provide what it no longer carries (measured: the stdlib).
\
\ The two words are a package block because the rest of this file is legacy
\ global surface and the ownership gate refuses a new global here (measured:
\ "`REQUIRE-TRUNCATE` defines a changed module word outside a package"). The
\ tails are NOT spelled like the cells they move - a tail that folded onto
\ `REQUIRE-N` inside this block would be that cell.
package REQUIRE-REG

public

: COUNT ( -- n )
   REQUIRE-N @ ;

: TRUNCATE ( n -- ) {: keep:n :}
   keep 0 < keep REQUIRE-N @ > or if
      s" require: truncate outside the registry" INCLUDE-DIE
   then
   keep REQUIRE-N !
   REQUIRE-BOOT-N @ keep > if keep REQUIRE-BOOT-N ! then
   REQUIRE-BASE @ keep > if keep REQUIRE-BASE ! then
   REQUIRE-SAVE-N @ keep > if keep REQUIRE-SAVE-N ! then
   REQUIRE-SAVE-BASE @ keep > if keep REQUIRE-SAVE-BASE ! then ;

;package

\ The loader scratch a fresh pass may reset at any time: the open file, the read
\ counters, the path buffer, the discovery base and the event log. None of it
\ describes a load in flight, so resetting it underneath one is safe.
: INCLUDE-RESET-SCRATCH ( -- )
   INCLUDE-CLOSE
   0 INCLUDE-U !
   0 INCLUDE-RD !
   0 INCLUDE-PATH-U !
   0 REQUIRE-BASE !
   EVENT-OFF
   DISCOVERY-OFF
   EVENTS-RESET ;

\ Snapshot preparation adds the one thing only a snapshot needs: the mapped
\ include buffers do not survive the image, so the pointer is dropped and the
\ restored process maps fresh ones on its next load.
\
\ It does NOT touch INCLUDE-DEPTH. The depth counts the loads in flight, which
\ belong to the caller, and a snapshot is only meaningful when there are none -
\ so this asks instead of assuming. Zeroing it was the old shape, and it made
\ the word look like a general "reset the include subsystem": a caller that
\ used it that way and then returned had its own frame silently erased and
\ underflowed at INCLUDE-POP (test/compiler/ir-id-replay.f, found when `--load`
\ started loading its argv files through `required`). Callers that want the
\ resettable half now say INCLUDE-RESET-SCRATCH and say it at any depth.
: INCLUDE-SNAPSHOT-PREPARE ( -- )
   INCLUDE-DEPTH @ 0= 0= if
      s" include: snapshot prepare under an open load" INCLUDE-DIE
   then
   INCLUDE-RESET-SCRATCH
   0 INCLUDE-BUFS-A ! ;

\ ---- what the ENGINE provides, as opposed to what this process has loaded ---
\
\ The boot prefix marks its own files `provided` before any user token runs, so
\ the registry opens with exactly the engine's surface in it. Freezing the count
\ at the end of the prefix is what lets a later question separate "the engine
\ carries this" from "something in this process required it", which a plain
\ REQUIRE-KNOWN? cannot: by the time a tool asks, its own dependencies are in
\ the registry too. tools/bundle-lib-core.f needs exactly this separation - it
\ must not bundle a copy of a file the engine already loaded, and must not skip
\ one the engine does not have.
: REQUIRE-BOOT-FREEZE ( -- )
   REQUIRE-N @ REQUIRE-BOOT-N ! ;

: ENGINE-PROVIDES? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup REQUIRE-BOOT-N @ < while
      dup a u rot REQUIRE-PATH= if drop INCLUDE-TRUE exit then
      1+
   repeat drop INCLUDE-FALSE ;

\ A bundle (tools/bundle-lib.f) carries the modules this engine does NOT have
\ and states the ones it assumes. Stating the assumption is the bundle's half;
\ checking it is the engine's, so a bundle built against a richer engine and run
\ on a barer one refuses here, by name, instead of dying later on a missing word
\ or - worse - loading a second copy of a module the engine already carries.
: ?ENGINE-PROVIDES ( ptr u8 n -- ) {: a:ptr u:n :}
   a u ENGINE-PROVIDES? if exit then
   INCLUDE-DIAG-RESET
   s" bundle: this engine does not provide " INCLUDE-DIAG+
   a u INCLUDE-DIAG+
   INCLUDE-LF 1 INCLUDE-DIAG+
   INCLUDE-DIAG$ INCLUDE-DIE ;

\ constructor generation (sumtype.f, loaded earlier in the boot prefix) crosses
\ evaluate only through this audited INCLUDE-EVALUATE boundary; engines without
\ include.f (stage builders) leave TDECL-EVAL-ARMED 0 and generation stays
\ fail-closed. Bind the defer (wrapped so the quotation compiles), then arm.
: TDECL-EVAL-INSTALL ( -- ) [: INCLUDE-EVALUATE ;] is TDECL-EVAL-XT ;
TDECL-EVAL-INSTALL
-1 TDECL-EVAL-ARMED !

\ gate-images.f - the keyed images a gate settles in its pool, and which rows
\ wait for each.
\
\ ONE MODULE PER IMAGE. A keyed image is what one family module builds:
\ test/fixture-writer.f, test/cold-engine.f, test/app-image-engine.f,
\ test/preloaded-engine.f, test/whitebox-engine.f and test/saved-builder.f;
\ test/native-unit-image.f settles a keyed NBR package unit the same way.
\ Loading the module is how a file reaches the image, so a row NEEDS a family
\ when its load closure holds the family's module: the files the row loads, and
\ the files those import or launch (test/load-refs.f). A WHITEBOX-SUITE row
\ also runs on the whitebox engine itself. Nothing is declared: a row that
\ starts loading a module needs its image from then on, and a family whose
\ module loads another's needs that one first (the cold host is the writer's
\ output; the linker is built on the app image).
\
\ SETTLED ONCE, IN THE POOL. Before its first registry row the gate starts one
\ build row per needed family, <family>-build: a bin/hb child handed `require
\ <module>` and the family's settle word on stdin, on the product engine like
\ any row. A family whose prerequisite is still building starts once that one
\ passed, and never if it failed. A row that needs an image not yet settled
\ holds the registry until it is, while the rows before it keep running. A row
\ that needs a failed image is its own red pool row, naming the build row that
\ failed, and every row that needs none of the failed images runs as usual.
\
\ GRANTED, NOT RACED. Each row is told the images settled for it
\ (test/image-grant.f), so its own settle only finds and dates them; a settle
\ its load closure does not show - a child reached through a load this reader
\ cannot see - dies in that child, naming the image, instead of building it
\ beside the gate's build row.
\
\ The build rows are spawned, not forked, so the gate process loads no family
\ module: a file that loads the gate itself (test/run-cli-test.f launches
\ test/run.f) reaches no image through it.
\
\ ONE GRAPH. DERIVE reads each file once and keeps every load it names, with
\ the line naming it; test/gate-entry-guard.f walks that graph through the
\ readers below, so the guard and the grants never disagree about what a row
\ loads.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/test/suite.f
require lib/test/runner.f
require tools/lint/text.f
require tools/lint/source-lex.f
require test/load-refs.f
require test/gate-pool.f
require test/image-grant.f
require test/keyed-image.f               \ a build's CPU budget and hang guard

package GATE-IMAGES

\ ---- the families ------------------------------------------------------------

0 constant WRITER
1 constant COLD
2 constant APP
3 constant LINKER
4 constant WHITEBOX
5 constant BUILDER
6 constant NBR-UNIT
7 constant FAMILY-N

\ The family name each module checks its grant under (test/image-grant.f).
: LABEL$ ( n -- ptr u8 n ) {: f:n :}
   f WRITER = if s" fixture-writer" exit then
   f COLD = if s" cold-engine" exit then
   f APP = if s" app-image" exit then
   f LINKER = if s" linker" exit then
   f WHITEBOX = if s" whitebox-engine" exit then
   f BUILDER = if s" saved-builder" exit then
   f NBR-UNIT = if s" native-unit" exit then
   E-TBL-BOUNDS throw ;

\ A family's prerequisites come before it here: DERIVE refuses a table that
\ lists one after.
: MODULE$ ( n -- ptr u8 n ) {: f:n :}
   f WRITER = if s" test/fixture-writer.f" exit then
   f COLD = if s" test/cold-engine.f" exit then
   f APP = if s" test/app-image-engine.f" exit then
   f LINKER = if s" test/preloaded-engine.f" exit then
   f WHITEBOX = if s" test/whitebox-engine.f" exit then
   f BUILDER = if s" test/saved-builder.f" exit then
   f NBR-UNIT = if s" test/native-unit-image.f" exit then
   E-TBL-BOUNDS throw ;

\ What the build row runs once the module is loaded. The whitebox row also puts
\ the gate's private copy in place for the WHITEBOX-SUITE rows (WB$).
: SETTLE$ ( n -- ptr u8 n ) {: f:n :}
   f WRITER = if s" FIXTURE-WRITER:ENSURE" exit then
   f COLD = if s" COLD-ENGINE:ENSURE" exit then
   f APP = if s" APP-IMAGE-ENGINE:ENSURE" exit then
   f LINKER = if s" PRELOADED-ENGINE:ENSURE" exit then
   f WHITEBOX = if s" WHITEBOX-ENGINE:PROVIDE" exit then
   f BUILDER = if s" SAVED-BUILDER:ENSURE" exit then
   f NBR-UNIT = if s" NATIVE-UNIT-IMAGE:ENSURE" exit then
   E-TBL-BOUNDS throw ;

: BIT ( n -- n )
   1 swap lshift ;

\ ---- what a file loads -------------------------------------------------------
\
\ Every file is read once, whatever number of rows load it, and named by its
\ canonical path. A walk from a file gathers the families its whole closure
\ reaches, and remembers that answer for the file it started from.

$2000 constant HASH-CAP                  \ slots, a power of two; half may fill

DYNAMIC-BUFFER PATH-BYTES u8
DYNAMIC-BUFFER PATH-OFF n
DYNAMIC-BUFFER PATH-LEN n
DYNAMIC-BUFFER EDGE-OFF n                \ its first edge; -1 until it is read
DYNAMIC-BUFFER EDGE-CNT n
DYNAMIC-BUFFER OWN n                     \ a family module's own bit, else 0
DYNAMIC-BUFFER CLOSURE n                 \ families it reaches; -1 until walked
DYNAMIC-BUFFER SEEN n                    \ the walk that last reached it
DYNAMIC-BUFFER QUEUE n
DYNAMIC-BUFFER EDGES n                   \ each file's loads, back to back
DYNAMIC-BUFFER EDGE-LINE n               \ the line naming each load
DYNAMIC-BUFFER EDGE-IMPORT bool          \ TRUE for an import, FALSE for a launch

create HASH HASH-CAP cells allot         \ file id + 1 per slot; 0 is empty

variable PATH-U
variable FILE-N
variable EDGE-N
variable PROBE
variable WALK
variable QHEAD
variable QTAIL
variable REACH
variable UNION

: FILE$ ( n -- ptr u8 n ) {: id:n :}
   0 PATH-BYTES id PATH-OFF @ +
   id PATH-LEN @ ;

: FILE-ADD ( ptr u8 n -- n ) {: a:ptr u:n :}
   FILE-N @ {: id:n :}
   PATH-U @ u + PATH-BYTES-RESERVE
   id 1+ PATH-OFF-RESERVE
   id 1+ PATH-LEN-RESERVE
   id 1+ EDGE-OFF-RESERVE
   id 1+ EDGE-CNT-RESERVE
   id 1+ OWN-RESERVE
   id 1+ CLOSURE-RESERVE
   id 1+ SEEN-RESERVE
   id 1+ QUEUE-RESERVE
   a 0 PATH-BYTES PATH-U @ + u BYTE-COPY
   PATH-U @ id PATH-OFF !
   u id PATH-LEN !
   -1 id EDGE-OFF !
   0 id EDGE-CNT !
   0 id OWN !
   -1 id CLOSURE !
   0 id SEEN !
   PATH-U @ u + PATH-U !
   id 1+ FILE-N !
   id ;

: HASH-SLOT ( n -- ptr n )
   cells HASH + ;

: FNV ( ptr u8 n -- n ) {: a:ptr u:n :}
   $CBF29CE484222325 u 0 ?do a i + c@ xor $100000001B3 * loop ;

: SLOT-ID ( -- n )
   PROBE @ HASH-SLOT @ ;

\ Probe from a canonical path's hash to the slot holding it, or to the empty
\ slot it would take.
: PROBE! ( ptr u8 n -- ) {: a:ptr u:n :}
   a u FNV HASH-CAP 1- and PROBE !
   begin SLOT-ID 0<> while
      SLOT-ID 1- FILE$ a u STR= if exit then
      PROBE @ 1+ HASH-CAP 1- and PROBE !
   repeat ;

\ The id of a canonical path, added the first time it is seen.
: INTERN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u PROBE!
   SLOT-ID 0<> if SLOT-ID 1- exit then
   FILE-N @ HASH-CAP 2 / >= if
      s" gate images: more files than HASH-CAP holds" type cr
      E-TBL-BOUNDS throw
   then
   a u FILE-ADD {: id:n :}
   id 1+ PROBE @ HASH-SLOT !
   id ;

: GRAPH-RESET ( -- )
   0 PATH-U ! 0 FILE-N ! 0 EDGE-N ! 0 WALK !
   HASH-CAP 0 ?do 0 i HASH-SLOT ! loop ;

: EDGE+ ( n n bool -- ) {: id:n line:n import:bool :}
   EDGE-N @ 1+ EDGES-RESERVE
   EDGE-N @ 1+ EDGE-LINE-RESERVE
   EDGE-N @ 1+ EDGE-IMPORT-RESERVE
   id EDGE-N @ EDGES !
   line EDGE-N @ EDGE-LINE !
   import EDGE-N @ EDGE-IMPORT !
   EDGE-N @ 1+ EDGE-N ! ;

\ A reference names its file from the tree root; a path that names no file is
\ not an edge. A launch is followed only into a Forth source: a launched script
\ or data file is not lexed.
: REF ( ptr u8 n n bool -- ) {: a:ptr u:n line:n import:bool :}
   a u FILE? 0= if exit then
   a u SOURCE-ROOT:CANONICAL drop {: path:ptr pathu:n :}
   import 0= path pathu s" .f" ENDS-WITH? 0= and if exit then
   path pathu INTERN line import EDGE+ ;

: SCAN ( n -- ) {: id:n :}
   id EDGE-OFF @ 0 >= if exit then
   EDGE-N @ id EDGE-OFF !
   id FILE$ LINT-SOURCE:LOAD
   LINT-SOURCE:TEXT LINT-LEX:SOURCE
   LINT-LEX:ERROR? if
      s" gate images: cannot lex " type id FILE$ type cr
      E-SUITE-ROW throw
   then
   [: REF ;] LOAD-REFS:EACH
   EDGE-N @ id EDGE-OFF @ - id EDGE-CNT ! ;

: REACH-FILE ( n -- ) {: id:n :}
   id SEEN @ WALK @ = if exit then
   WALK @ id SEEN !
   id QTAIL @ QUEUE !
   QTAIL @ 1+ QTAIL ! ;

\ A file whose own walk is done adds its answer and is not walked again.
: EXPAND ( n -- ) {: id:n :}
   id CLOSURE @ 0 >= if id CLOSURE @ REACH @ or REACH ! exit then
   id SCAN
   id OWN @ REACH @ or REACH !
   id EDGE-CNT @ 0 ?do id EDGE-OFF @ i + EDGES @ REACH-FILE loop ;

: CLOSURE-OF ( n -- n ) {: id:n :}
   id CLOSURE @ 0 >= if id CLOSURE @ exit then
   WALK @ 1+ WALK !
   0 QHEAD ! 0 QTAIL ! 0 REACH !
   id REACH-FILE
   begin QHEAD @ QTAIL @ < while
      QHEAD @ QUEUE @ QHEAD @ 1+ QHEAD ! EXPAND
   repeat
   REACH @ id CLOSURE !
   REACH @ ;

\ The families a file reaches; a path that names no file reaches none.
: FILE-MASK ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u FILE? 0= if 0 exit then
   a u SOURCE-ROOT:CANONICAL drop INTERN CLOSURE-OF ;

\ ---- the build rows ----------------------------------------------------------

0 constant IDLE
1 constant WANTED
2 constant LIVE
3 constant READY
4 constant FAILED

\ A build row is held to a build's CPU budget, whatever the load
\ (test/keyed-image.f BOUNDED BY ITS OWN WORK). Its wall deadline is only a
\ hang guard, a minute beyond the builder's own for the key hashing and the
\ copy: the builder's deadline governs, its BUILD-RUN names the step and throws
\ E-PROC-TIMEOUT, and its EMIT removes the work directory before the throw goes
\ on.
KEYED-IMAGE:BUILD-TIMEOUT-MS 60000 + constant BUILD-ROW-TIMEOUT-MS
\ A row that needs a failed image dies as it starts, with this status.
60000 constant RED-ROW-TIMEOUT-MS
75 constant RED-ROW-RC
GT-POOL-STDIN-CAP constant PROGRAM-CAP

create STATES FAMILY-N cells allot
create SEQS FAMILY-N cells allot         \ a live build row's capture seq
create PRES FAMILY-N cells allot         \ the families each one needs first
create PROGRAM-BUF PROGRAM-CAP allot
create LABEL-BUF GT-FAIL-NAME-CAP allot
create WB-BUF FS-PATH-CAP allot

variable PROGRAM-U
variable LABEL-U
variable WB-U
variable NEEDED
variable ROW-MASK

\ How the adapter starts a build row: program, label, grant, deadline.
TYPED-VARIABLE START-XT [ ptr u8 n ptr u8 n ptr u8 n n -- ]

: STATE@ ( n -- n ) cells STATES + @ ;
: STATE! ( n n -- ) cells STATES + ! ;
: SEQ@ ( n -- n ) cells SEQS + @ ;
: SEQ! ( n n -- ) cells SEQS + ! ;
: PRE@ ( n -- n ) cells PRES + @ ;
: PRE! ( n n -- ) cells PRES + ! ;

: WB$ ( -- ptr u8 n )
   WB-BUF WB-U @ ;

: MODULE! ( n -- ) {: f:n :}
   f MODULE$ FILE? 0= if
      s" gate images: " type f LABEL$ type s"  has no module " type
      f MODULE$ type cr
      E-SUITE-ROW throw
   then
   f MODULE$ SOURCE-ROOT:CANONICAL drop INTERN {: id:n :}
   f BIT id OWN ! ;

: DERIVE-PRE ( n -- ) {: f:n :}
   f MODULE$ FILE-MASK f BIT invert and {: pre:n :}
   pre f BIT 1- invert and 0<> if
      s" gate images: " type f LABEL$ type
      s"  needs a family listed after it" type cr
      E-SUITE-ROW throw
   then
   pre f PRE! ;

\ A mask is the families a closure of files reaches, and a family's module
\ reaches its prerequisites' modules, so every mask holds the prerequisites of
\ its families.
: WANT ( n -- ) {: mask:n :}
   mask NEEDED !
   FAMILY-N 0 ?do mask i BIT and 0<> if WANTED i STATE! then loop ;

: GRANT! ( n -- ) {: mask:n :}
   IMAGE-GRANT:RESET
   FAMILY-N 0 ?do mask i BIT and 0<> if i LABEL$ IMAGE-GRANT:ADD then loop ;

: PROGRAM+ ( ptr u8 n -- ) {: a:ptr u:n :}
   PROGRAM-U @ u + PROGRAM-CAP > if E-STR-CAPACITY throw then
   a PROGRAM-BUF PROGRAM-U @ + u BYTE-COPY
   PROGRAM-U @ u + PROGRAM-U ! ;

: PROGRAM! ( n -- ) {: f:n :}
   0 PROGRAM-U !
   s" require " PROGRAM+ f MODULE$ PROGRAM+ S\" \n" PROGRAM+
   f WHITEBOX = if S\" s\" " PROGRAM+ WB$ PROGRAM+ S\" \" " PROGRAM+ then
   f SETTLE$ PROGRAM+ S\" \n" PROGRAM+ ;

: LABEL+ ( ptr u8 n -- ) {: a:ptr u:n :}
   LABEL-U @ u + GT-FAIL-NAME-CAP > if E-STR-CAPACITY throw then
   a LABEL-BUF LABEL-U @ + u BYTE-COPY
   LABEL-U @ u + LABEL-U ! ;

: BUILD-LABEL! ( n -- ) {: f:n :}
   0 LABEL-U !
   f LABEL$ LABEL+
   s" -build" LABEL+ ;

\ A build row is granted its family and the prerequisites its module settles on
\ the way.
: BUILD ( n -- ) {: f:n :}
   f PROGRAM!
   f BUILD-LABEL!
   f BIT f PRE@ or GRANT!
   PROGRAM-BUF PROGRAM-U @ LABEL-BUF LABEL-U @ IMAGE-GRANT:VALUE$
   BUILD-ROW-TIMEOUT-MS START-XT @ execute
   GT-POOL-SEQ @ f SEQ!
   KEYED-IMAGE:BUILD-CPU-MS f SEQ@ GT-POOL-CPU-BUDGET!
   LIVE f STATE! ;

\ A build row retired. The red table holds a record for every red row the pool
\ starts (test/gate-pool.f GT-POOL-RED-MAX), so no record means it passed.
: OUTCOME ( n -- ) {: f:n :}
   f SEQ@ GT-POOL-RED-FIND-SEQ 0 < if READY else FAILED then f STATE! ;

: REFRESH ( -- )
   FAMILY-N 0 ?do
      i STATE@ LIVE = if i SEQ@ GT-POOL-SEQ-LIVE? 0= if i OUTCOME then then
   loop ;

\ The first failed family in a mask, or -1. A family comes after its
\ prerequisites and a mask holds them, so this is a family whose own build
\ failed, never one left unbuilt behind it.
: FAILED-IN ( n -- n ) {: mask:n :}
   FAMILY-N 0 ?do
      mask i BIT and 0<> i STATE@ FAILED = and if i unloop exit then
   loop
   -1 ;

\ A red row's body, in its forked child: the row's mask names the build row
\ that failed, whose own red report holds the builder's status and output.
: RED-BODY ( -- )
   ROW-MASK @ FAILED-IN {: f:n :}
   SB-RESET
   f LABEL$ SB-APPEND
   s"  image build failed; its output is under FAIL: " SB-APPEND
   f LABEL$ SB-APPEND
   s" -build" SB-APPEND
   SB$ RED-ROW-RC die ;

: READY-ALL? ( n -- bool ) {: mask:n :}
   FAMILY-N 0 ?do
      mask i BIT and 0<> i STATE@ READY <> and if false unloop exit then
   loop
   true ;

\ A family behind a failed prerequisite is never built.
: STEP-ONE ( n -- ) {: f:n :}
   f STATE@ WANTED <> if exit then
   f PRE@ FAILED-IN 0 >= if FAILED f STATE! exit then
   f PRE@ READY-ALL? if f BUILD then ;

\ Retire what finished, then start every wanted family whose prerequisites
\ passed. A start waits for a free slot.
: PUMP ( -- )
   REFRESH
   FAMILY-N 0 ?do i STEP-ONE loop ;

: SETTLED? ( n -- bool ) {: mask:n :}
   FAMILY-N 0 ?do
      mask i BIT and 0<> if
         i STATE@ READY <> i STATE@ FAILED <> and if false unloop exit then
      then
   loop
   true ;

\ DERIVE wanted every family a row can need, and PUMP starts a wanted family
\ once its prerequisites passed, so a build row is live until the mask settles.
: AWAIT ( n -- ) {: mask:n :}
   begin
      PUMP
      mask SETTLED? 0=
   while
      GT-POOL-STEP
   repeat ;

: UNION-FILE ( n bool ptr u8 n -- )
   FILE-MASK UNION @ or UNION !
   drop drop ;

: STATES-RESET ( -- )
   FAMILY-N 0 ?do IDLE i STATE! 0 i PRE! loop
   0 NEEDED ! ;

\ Every family row this gate may start must fit the pool's red table.
: SIDE-CHECK ( -- )
   FAMILY-N GT-POOL-SIDE-MAX > if
      s" gate images: more families than test/gate-pool.f GT-POOL-SIDE-MAX"
      type cr
      E-SUITE-ROW throw
   then ;

public

\ Read the registry's load graph and derive the images its rows need, starting
\ nothing. Call it once the registry is complete, before START and before the
\ graph readers below.
: DERIVE ( -- )
   SIDE-CHECK
   GRAPH-RESET
   STATES-RESET
   FAMILY-N 0 ?do i MODULE! loop
   FAMILY-N 0 ?do i DERIVE-PRE loop
   0 UNION !
   [: UNION-FILE ;] TEST:VISIT-LOAD-FILES
   TEST:WHITEBOX-REGISTERED? if
      WHITEBOX MODULE$ FILE-MASK UNION @ or UNION !
   then
   UNION @ WANT ;

\ Start the build rows of the images DERIVE found needed. Call it after
\ GT-START and GT-POOL-RESET, before the first registry row; the xt starts a
\ build row from its stdin program, label, grant and deadline.
: START ( [ ptr u8 n ptr u8 n ptr u8 n n -- ] -- )
   START-XT !
   s" hb-whitebox" WB-BUF GT-PATH WB-U !
   PUMP ;

\ The engine the WHITEBOX-SUITE rows run on: this gate's private copy, which
\ the whitebox-engine build row puts in place.
: WHITEBOX$ ( -- ptr u8 n )
   WB$ ;

\ The row about to start: its load files, and whether it runs on the whitebox
\ engine.
: ROW-RESET ( -- )
   0 ROW-MASK ! ;

: ROW-FILE ( ptr u8 n -- )
   FILE-MASK ROW-MASK @ or ROW-MASK ! ;

\ A row on the whitebox engine needs what loading the engine's module would.
: ROW-WHITEBOX ( -- )
   WHITEBOX MODULE$ ROW-FILE ;

\ Settle every image the row needs, holding the registry until each build row
\ retired. TRUE when all passed; else start ROW-RED for the row.
: ROW-READY? ( -- bool )
   ROW-MASK @ AWAIT
   ROW-MASK @ FAILED-IN 0 < ;

: ROW-RED ( ptr u8 n -- ) {: label:ptr labelu:n :}
   label labelu RED-ROW-TIMEOUT-MS [: RED-BODY ;] GT-POOL-START-FORK ;

\ The grant the row's environment carries (test/image-grant.f).
: ROW-GRANT$ ( -- ptr u8 n )
   ROW-MASK @ GRANT!
   IMAGE-GRANT:VALUE$ ;

\ Settle every image this gate wanted: before a drain, so no build row runs
\ beside a sequential group.
: SETTLE-ALL ( -- )
   NEEDED @ AWAIT ;

\ ---- the load graph DERIVE read ----------------------------------------------
\
\ Files are numbered from 0. A file's loads are EDGE-COUNT consecutive edges
\ from EDGE-FIRST, in source order; each names the file loaded, the line naming
\ it and whether it is an import or a launch.

: FILE-COUNT ( -- n )
   FILE-N @ ;

\ The id of a file DERIVE read, by any path naming it. Any other path is
\ refused: a file the walk never read has no loads to trust.
: FILE-ID ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u SOURCE-ROOT:CANONICAL drop PROBE!
   SLOT-ID 0<> if SLOT-ID 1- exit then
   s" gate images: not a file of the load graph: " type a u type cr
   E-SUITE-ROW throw ;

EXPORT FILE$

: EDGE-FIRST ( n -- n )
   EDGE-OFF @ ;

: EDGE-COUNT ( n -- n )
   EDGE-CNT @ ;

: EDGE-TO ( n -- n )
   EDGES @ ;

: EDGE-LINE@ ( n -- n )
   EDGE-LINE @ ;

: EDGE-IMPORT? ( n -- bool )
   EDGE-IMPORT @ ;

;package

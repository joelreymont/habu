\ include.f - checked source include words.
\
\ `include` is source composition. Package reopening owns shared namespace;
\ this file only gives source files a checked way to load dependencies.

$400 constant INCLUDE-PATH-CAP
$80000 constant INCLUDE-BUF-CAP
$8 constant INCLUDE-MAX-DEPTH
$100 constant REQUIRE-MAX
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
variable REQUIRE-BASE
variable REQUIRE-SAVE-N
variable REQUIRE-SAVE-BASE

-1 INCLUDE-FD !

: INCLUDE-FALSE ( -- bool )
   0 0= 0= ;

: INCLUDE-TRUE ( -- bool )
   0 0= ;

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

: INCLUDE-PUSH ( -- )
   INCLUDE-DEPTH @ INCLUDE-CHECK-DEPTH
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

: INCLUDE-OPEN ( ptr u8 n -- )
   INCLUDE-PATH0 open-rd INCLUDE-FD !
   INCLUDE-FD @ 0 < if s" include: open failed" INCLUDE-DIE then ;

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

: required ( ptr u8 n -- )
   INCLUDE-CHECK-PATH
   2dup REQUIRE-KNOWN? {: known:bool :}
   2dup EV-REQUIRED known REQUIRE-STATE EVENT-RECORD
   known if 2drop exit then
   2dup REQUIRE-STORE
   DISCOVERY? if 2drop exit then
   INCLUDE-LOAD ;

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

: INCLUDE-SNAPSHOT-PREPARE ( -- )
   INCLUDE-CLOSE
   0 INCLUDE-BUFS-A !
   0 INCLUDE-DEPTH !
   0 INCLUDE-U !
   0 INCLUDE-RD !
   0 INCLUDE-PATH-U !
   0 REQUIRE-BASE !
   EVENT-OFF
   DISCOVERY-OFF
   EVENTS-RESET ;

\ constructor generation (sumtype.f, loaded earlier in the boot prefix) crosses
\ evaluate only through this audited INCLUDE-EVALUATE boundary; engines without
\ include.f (stage builders) leave the cell 0 and generation stays fail-closed.
' INCLUDE-EVALUATE TDECL-EVAL-XT !

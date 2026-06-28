\ gate-stats.f - checked append-only counters for native gate RCA.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/fs-mutate.f,
\ and lib/process-env.f.

$40000 constant GS-CAP
$80 constant GS-LINE-CAP
$0A constant GS-LF

create GS-PATH-BUF FS-PATH-CAP allot
create GS-LINE-BUF GS-LINE-CAP allot
create GS-BUF GS-CAP allot

variable GS-PATH-U
variable GS-INITED
variable GS-U
variable GS-I
variable GS-START

variable GS-TOP-PHASE
variable GS-TOP-CAPTURE
variable GS-INNER-HB
variable GS-INNER-HB-STDIN
variable GS-BOUNDARY
variable GS-WARM-HIT
variable GS-WARM-MISS
variable GS-WARM-BUILD
variable GS-WARM-SIG
variable GS-WARM-SNAP
variable GS-MAKER-HIT
variable GS-MAKER-MISS
variable GS-MAKER-BUILD
variable GS-MAKER-RUN
variable GS-CANDIDATE
variable GS-HELPER-SPAWN

: GS-FALSE ( -- bool )
   0 0= 0= ;

: GS-TRUE ( -- bool )
   0 0= ;

: GS-EMPTY$ ( -- ptr u8 n )
   GS-LINE-BUF 0 ;

: GS-PATH$ ( -- ptr u8 n )
   GS-PATH-BUF GS-PATH-U @ ;

: GS-COPY-PATH! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < if E-FS-PATH throw then
   u FS-PATH-CAP > if E-FS-PATH throw then
   a GS-PATH-BUF u BYTE-COPY
   u GS-PATH-U ! ;

: GS-ENV-PATH! ( -- )
   s" HABU_GATE_STATS" GETENV dup 0= if
      2drop 0 GS-PATH-U ! exit
   then
   GS-COPY-PATH! ;

: GS-ENSURE ( -- )
   GS-INITED @ 0= if
      GS-ENV-PATH!
      -1 GS-INITED !
   then ;

: GS-ON? ( -- bool )
   GS-ENSURE
   GS-PATH-U @ 0 > ;

: GS-ROOT! ( ptr u8 n -- ) {: root:ptr rootu:n :}
   root rootu s" gate-stats.tsv" GS-PATH-BUF JOIN-PATH GS-PATH-U !
   -1 GS-INITED !
   GS-PATH$ GS-EMPTY$ WRITE-ALL ;

: GS-ENV+ ( -- )
   GS-ON? if s" HABU_GATE_STATS" >LEN GS-PATH$ >LEN PROC-ENV+ then ;

: GS-EVENT ( ptr u8 n -- ) {: a:ptr u:n :}
   GS-ON? 0= if exit then
   u 0 < if E-STR-BOUNDS throw then
   u 1 + GS-LINE-CAP > if E-STR-CAPACITY throw then
   a GS-LINE-BUF u BYTE-COPY
   GS-LF GS-LINE-BUF u + c!
   GS-PATH$ GS-LINE-BUF u 1 + APPEND-FILE ;

: GS-INC ( ptr n -- ) {: p:ptr :}
   p @ 1 + p ! ;

: GS-RESET-COUNTS ( -- )
   0 GS-TOP-PHASE !
   0 GS-TOP-CAPTURE !
   0 GS-INNER-HB !
   0 GS-INNER-HB-STDIN !
   0 GS-BOUNDARY !
   0 GS-WARM-HIT !
   0 GS-WARM-MISS !
   0 GS-WARM-BUILD !
   0 GS-WARM-SIG !
   0 GS-WARM-SNAP !
   0 GS-MAKER-HIT !
   0 GS-MAKER-MISS !
   0 GS-MAKER-BUILD !
   0 GS-MAKER-RUN !
   0 GS-CANDIDATE !
   0 GS-HELPER-SPAWN ! ;

: GS-LINE= ( n n ptr u8 n -- bool ) {: off:n u:n key:ptr keyu:n :}
   u keyu <> if GS-FALSE exit then
   GS-BUF off BYTE+ u key keyu STR= ;

: GS-COUNT-LINE ( n n -- ) {: off:n u:n :}
   off u s" top-phase-spawn" GS-LINE= if GS-TOP-PHASE GS-INC exit then
   off u s" top-capture-spawn" GS-LINE= if GS-TOP-CAPTURE GS-INC exit then
   off u s" inner-hb-spawn" GS-LINE= if GS-INNER-HB GS-INC exit then
   off u s" inner-hb-stdin" GS-LINE= if GS-INNER-HB-STDIN GS-INC exit then
   off u s" boundary-test" GS-LINE= if GS-BOUNDARY GS-INC exit then
   off u s" warm-cache-hit" GS-LINE= if GS-WARM-HIT GS-INC exit then
   off u s" warm-cache-miss" GS-LINE= if GS-WARM-MISS GS-INC exit then
   off u s" warm-build" GS-LINE= if GS-WARM-BUILD GS-INC exit then
   off u s" warm-sig-export" GS-LINE= if GS-WARM-SIG GS-INC exit then
   off u s" warm-snapshot" GS-LINE= if GS-WARM-SNAP GS-INC exit then
   off u s" maker-cache-hit" GS-LINE= if GS-MAKER-HIT GS-INC exit then
   off u s" maker-cache-miss" GS-LINE= if GS-MAKER-MISS GS-INC exit then
   off u s" maker-build" GS-LINE= if GS-MAKER-BUILD GS-INC exit then
   off u s" maker-run" GS-LINE= if GS-MAKER-RUN GS-INC exit then
   off u s" candidate-build" GS-LINE= if GS-CANDIDATE GS-INC exit then
   off u s" helper-spawn" GS-LINE= if GS-HELPER-SPAWN GS-INC exit then ;

: GS-SCAN ( -- )
   GS-RESET-COUNTS
   0 GS-START !
   0 GS-I !
   begin GS-I @ GS-U @ < while
      GS-BUF GS-I @ BYTE+ c@ GS-LF = if
         GS-START @ GS-I @ GS-START @ - GS-COUNT-LINE
         GS-I @ 1 + GS-START !
      then
      GS-I @ 1 + GS-I !
   repeat
   GS-START @ GS-U @ < if
      GS-START @ GS-U @ GS-START @ - GS-COUNT-LINE
   then ;

: GS-READ ( -- )
   GS-PATH$ FILE? 0= if 0 GS-U ! exit then
   GS-PATH$ GS-BUF GS-CAP READ-ALL GS-U ! ;

: GS-ITEM. ( n ptr u8 n -- ) {: v:n a:ptr u:n :}
   a u type s" =" type v . ;

: GS-SUMMARY ( -- )
   GS-ON? 0= if exit then
   GS-READ
   GS-SCAN
   s" gate counts: " type
   GS-TOP-PHASE @ s" top-phase" GS-ITEM.
   GS-TOP-CAPTURE @ s" top-capture" GS-ITEM.
   GS-INNER-HB @ s" inner-hb" GS-ITEM.
   GS-INNER-HB-STDIN @ s" inner-hb-stdin" GS-ITEM.
   GS-BOUNDARY @ s" boundary" GS-ITEM.
   GS-WARM-HIT @ s" warm-hit" GS-ITEM.
   GS-WARM-MISS @ s" warm-miss" GS-ITEM.
   GS-WARM-BUILD @ s" warm-build" GS-ITEM.
   GS-WARM-SIG @ s" warm-sig" GS-ITEM.
   GS-WARM-SNAP @ s" warm-snap" GS-ITEM.
   GS-MAKER-HIT @ s" maker-hit" GS-ITEM.
   GS-MAKER-MISS @ s" maker-miss" GS-ITEM.
   GS-MAKER-BUILD @ s" maker-build" GS-ITEM.
   GS-MAKER-RUN @ s" maker-run" GS-ITEM.
   GS-CANDIDATE @ s" candidate" GS-ITEM.
   GS-HELPER-SPAWN @ s" helper-spawn" GS-ITEM.
   cr ;

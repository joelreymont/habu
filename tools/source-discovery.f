\ source-discovery.f - whole-file ordered source-composition discovery pass.
\
\ Run: bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/fs.f
\ lib/source.f tools/dynamic-tail-manifest.f tools/source-discovery.f
\
\ DISCOVER:RUN lexes an entry file's ENTIRE token stream - colon bodies
\ included - and replays every literal source-composition form against a fresh
\ require/provided registry, driving the instrumented core loader words
\ (included/required/provided) in record-only discovery mode so the ordered
\ event log (src/core/include.f) captures include multiplicity and
\ require/provided exact-string registry state without loading or compiling
\ anything. A guarded loader inside a colon body is recorded unconditionally,
\ so the event closure over-approximates (superset of the runtime closure)
\ and can never under-approximate a statically-visible loader call.
\ Redefining or undefining a loader word, retiring one through
\ UNDEFINE-IF-DEFINED, a dynamic (non-literal) loader path, and an unsupported
\ string opener (C\" / .\") before a loader word each reject fail-closed -
\ unless the entry file is a declared dynamic-tail boundary in
\ tools/dynamic-tail-manifest.f, in which case exactly those forms are
\ tolerated (skipped, never recorded) and only the statically-visible loader
\ events are produced. Recorded loader-token spans are entry-file byte offsets.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/source.f
require tools/dynamic-tail-manifest.f

package DISCOVER

$80000 constant SD-SRC-CAP
$400 constant SD-PATH-CAP

$5C constant SD-BACKSLASH
$28 constant SD-LPAREN
$29 constant SD-RPAREN
$0A constant SD-LF
$22 constant SD-DQUOTE
$20 constant SD-SP

1 constant SD-K-INCLUDED
2 constant SD-K-REQUIRED
3 constant SD-K-PROVIDED

1 constant SD-PEND-PATH
2 constant SD-PEND-OTHER

create SD-PATH SD-PATH-CAP allot

variable SD-BUF-A
variable SD-U
variable SD-I
variable SD-PATH-U
variable SD-PATH-OVF
variable SD-PEND
variable SD-LENIENT
variable SD-EMIT-LEN

: SD-BUF ( -- ptr u8 )
   SD-BUF-A @ 0= if SD-SRC-CAP SOURCE-ALLOC-BUF SD-BUF-A SOURCE-PTR-U8! then
   SD-BUF-A SOURCE-PTR-U8@ ;

: SD-BYTE ( n -- u8 )   SD-BUF + c@ ;
: SD-AT? ( n -- bool )  SD-U @ < ;
: SD-PATH-AT ( n -- ptr u8 )  SD-PATH + ;
: SD-PATH$ ( -- ptr u8 n )  0 SD-PATH-AT SD-PATH-U @ ;
: SD-TOK$ ( n n -- ptr u8 n ) {: off:n len:n :}  off SD-BUF + len ;

: SD-LENIENT? ( -- bool )   SD-LENIENT @ 0= 0= ;

\ Fail-closed guard trip: throw, unless the entry file is a declared
\ dynamic-tail boundary - then the offending form is tolerated (skipped).
: SD-REJECT ( n -- )
   SD-LENIENT? if drop exit then
   throw ;

: SD-SKIP-WS ( -- )
   begin SD-I @ SD-AT? while
      SD-I @ SD-BYTE 32 > if exit then
      SD-I @ 1+ SD-I !
   repeat ;

: SD-RAW ( -- n n )
   SD-SKIP-WS
   SD-I @ {: start:n :}
   start SD-AT? 0= if start 0 exit then
   begin SD-I @ SD-AT? while
      SD-I @ SD-BYTE 33 < if start SD-I @ start - exit then
      SD-I @ 1+ SD-I !
   repeat
   start SD-I @ start - ;

: SD-SKIP-TO ( n -- ) {: stop:n :}
   begin SD-I @ SD-AT? while
      SD-I @ SD-BYTE stop = if SD-I @ 1+ SD-I ! exit then
      SD-I @ 1+ SD-I !
   repeat ;

: SD-SKIP-LINE ( -- )   SD-LF SD-SKIP-TO ;
: SD-SKIP-PAREN ( -- )  SD-RPAREN SD-SKIP-TO ;

: SD-STR-LEAD ( n -- n )
   dup $73 = over $53 = or if drop SD-PEND-PATH exit then
   dup $2E = over $63 = or swap $43 = or if SD-PEND-OTHER exit then
   0 ;

: SD-OPENER-KIND ( n n -- n ) {: off:n len:n :}
   len 2 = if
      off 1 + SD-BYTE SD-DQUOTE = 0= if 0 exit then
      off SD-BYTE SD-STR-LEAD exit
   then
   len 3 = if
      off 1 + SD-BYTE SD-BACKSLASH = 0= if 0 exit then
      off 2 + SD-BYTE SD-DQUOTE = 0= if 0 exit then
      off SD-BYTE SD-STR-LEAD exit
   then
   0 ;

\ An oversized string literal is fine as data (the runtime loader path cap is
\ INCLUDE-PATH-CAP anyway); it only rejects when it reaches a loader word.
: SD-PATH-APPEND ( u8 -- ) {: c:n :}
   SD-PATH-U @ SD-PATH-CAP >= if 1 SD-PATH-OVF ! exit then
   c SD-PATH-U @ SD-PATH-AT c!
   SD-PATH-U @ 1+ SD-PATH-U ! ;

: SD-SCAN-ESC ( -- )
   SD-I @ SD-BYTE SD-PATH-APPEND
   SD-I @ 1+ SD-I !
   SD-I @ SD-AT? if SD-I @ SD-BYTE SD-PATH-APPEND SD-I @ 1+ SD-I ! then ;

: SD-SCAN-CHAR ( -- bool ) \ true when closing quote consumed
   SD-I @ SD-BYTE SD-DQUOTE = if SD-I @ 1+ SD-I ! STR-TRUE exit then
   SD-I @ SD-BYTE SD-PATH-APPEND
   SD-I @ 1+ SD-I !
   STR-FALSE ;

: SD-SCAN-STRING ( bool -- ) {: escaped:bool :}
   SD-I @ SD-AT? if SD-I @ SD-BYTE SD-SP = if SD-I @ 1+ SD-I ! then then
   0 SD-PATH-U !
   0 SD-PATH-OVF !
   begin SD-I @ SD-AT? while
      SD-I @ SD-BYTE SD-BACKSLASH = escaped and if
         SD-SCAN-ESC
      else
         SD-SCAN-CHAR if exit then
      then
   repeat
   E-DISC-UNTERM throw ;

: SD-COPY-PATH ( n n -- ) {: off:n len:n :}
   off SD-BUF + 0 SD-PATH-AT len BYTE-COPY
   len SD-PATH-U ! ;

: SD-LOADER-KIND ( n n -- n )
   SD-TOK$ {: a:ptr u:n :}
   a u s" included" STR=CI if SD-K-INCLUDED exit then
   a u s" required" STR=CI if SD-K-REQUIRED exit then
   a u s" provided" STR=CI if SD-K-PROVIDED exit then
   0 ;

: SD-RESERVED$? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" include" STR=CI if STR-TRUE exit then
   a u s" included" STR=CI if STR-TRUE exit then
   a u s" require" STR=CI if STR-TRUE exit then
   a u s" required" STR=CI if STR-TRUE exit then
   a u s" provided" STR=CI ;

: SD-RESERVED? ( n n -- bool )
   SD-TOK$ SD-RESERVED$? ;

: SD-CALL-LOADER ( n n n -- ) {: off:n len:n kind:n :}
   off len DISC-TOK!
   kind SD-K-INCLUDED = if SD-PATH$ included exit then
   kind SD-K-REQUIRED = if SD-PATH$ required exit then
   SD-PATH$ provided ;

: SD-DISPATCH-LOADER ( n n n n -- ) {: off:n len:n kind:n pend:n :}
   pend SD-PEND-OTHER = if E-DISC-OPENER SD-REJECT exit then
   pend SD-PEND-PATH = 0= if E-DISC-DYNAMIC SD-REJECT exit then
   SD-PATH-OVF @ 0= 0= if E-DISC-CAPACITY SD-REJECT exit then
   off len kind SD-CALL-LOADER ;

: SD-CHECK-NAME ( -- )
   SD-RAW {: off:n len:n :}
   len 0= if E-DISC-UNTERM throw then
   off len SD-RESERVED? if E-DISC-SHADOW SD-REJECT then ;

\ UNDEFINE-IF-DEFINED retiring a loader word (or fed a non-literal name that
\ cannot be proven safe) breaks loader identity for the rest of the file.
: SD-RETIRE ( n -- ) {: pend:n :}
   pend SD-PEND-PATH = 0= if E-DISC-RETIRE SD-REJECT exit then
   SD-PATH$ SD-RESERVED$? if E-DISC-RETIRE SD-REJECT then ;

: SD-LOADER-IMM ( n n n -- ) {: toff:n tlen:n kind:n :}
   SD-RAW {: poff:n plen:n :}
   plen 0= if E-DISC-DYNAMIC SD-REJECT exit then
   plen SD-PATH-CAP > if E-DISC-CAPACITY SD-REJECT exit then
   poff plen SD-COPY-PATH
   toff tlen kind SD-CALL-LOADER ;

: SD-STEP ( n n -- ) {: off:n len:n :}
   SD-PEND @ {: pend:n :}
   0 SD-PEND !
   len 1 = off SD-BYTE SD-BACKSLASH = and if SD-SKIP-LINE exit then
   len 1 = off SD-BYTE SD-LPAREN = and if SD-SKIP-PAREN exit then
   off len SD-OPENER-KIND {: opener:n :}
   opener 0= 0= if len 3 = SD-SCAN-STRING opener SD-PEND ! exit then
   off len SD-LOADER-KIND {: lkind:n :}
   lkind 0= 0= if off len lkind pend SD-DISPATCH-LOADER exit then
   off len SD-TOK$ s" :" STR= if SD-CHECK-NAME exit then
   off len SD-TOK$ s" undefine" STR=CI if SD-CHECK-NAME exit then
   off len SD-TOK$ s" UNDEFINE-IF-DEFINED" STR=CI if pend SD-RETIRE exit then
   off len SD-TOK$ s" include" STR=CI if off len SD-K-INCLUDED SD-LOADER-IMM exit then
   off len SD-TOK$ s" require" STR=CI if off len SD-K-REQUIRED SD-LOADER-IMM exit then ;

: SD-WALK ( -- )
   0 SD-I !
   0 SD-PEND !
   begin
      SD-RAW {: off:n len:n :}
      len 0= if exit then
      off len SD-STEP
   again ;

: SD-READ-ENTRY ( ptr u8 n -- ) {: pa:ptr pu:n :}
   pa pu SD-BUF SD-SRC-CAP READ-ALL SD-U ! ;

: SD-KIND-NAME ( n -- ptr u8 n ) {: kind:n :}
   kind EV-INCLUDED = if s" included" exit then
   kind EV-REQUIRED = if s" required" exit then
   s" provided" ;

: SD-EMIT-EVENT ( n ptr u8 len ptr len -- ) {: ix:n dst:ptr cap:len lenp:ptr :}
   ix EVENT-KIND@ SD-KIND-NAME >LEN dst cap lenp SOURCE-APPEND-BYTES
   SD-SP dst cap lenp SOURCE-APPEND-C
   ix EVENT-STATE@ $30 + dst cap lenp SOURCE-APPEND-C
   SD-SP dst cap lenp SOURCE-APPEND-C
   ix EVENT-PATH@ >LEN dst cap lenp SOURCE-APPEND-QPATH
   SD-LF dst cap lenp SOURCE-APPEND-C ;

public

: RUN ( ptr u8 n -- ) {: pa:ptr pu:n :}
   pa pu SD-READ-ENTRY
   pa pu DTM:KNOWN? SD-LENIENT !
   REQUIRE-SNAPSHOT
   EVENTS-RESET EVENT-ON DISCOVERY-ON
   [: SD-WALK ;] catch {: rc:n :}
   DISCOVERY-OFF EVENT-OFF
   REQUIRE-RESTORE
   rc 0= 0= if rc throw then ;

: EMIT ( ptr u8 n -- n ) {: dst:ptr cap:n :}
   0 >LEN SD-EMIT-LEN !
   cap >LEN {: clen:len :}
   0 begin dup EVENT-COUNT < while
      dup dst clen SD-EMIT-LEN SD-EMIT-EVENT
      1+
   repeat drop
   SD-EMIT-LEN @ LEN>N ;

;package

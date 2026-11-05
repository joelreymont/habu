\ Small application driver for cached-source acceptance tests.
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/test/subject.f
require test/app-builder-engine.f

package NATIVE-FILE-APP

$8000 constant CAP
DYNAMIC-BUFFER OUT u8 variable OUT-U
DYNAMIC-BUFFER ERR u8 variable ERR-U
create PROGRAM CAP allot variable PROGRAM-U
create BUILDER PATH-CAP allot variable BUILDER-U
create SRC-PATH PATH-CAP allot
create PROD-PATH PATH-CAP allot
create CACHE-PATH PATH-CAP allot

: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

\ A child's streams grow to what it writes.
: OUT-ROOM ( n -- ptr u8 ) OUT-RESERVE 0 OUT ;
: ERR-ROOM ( n -- ptr u8 ) ERR-RESERVE 0 ERR ;
public
\ These cases keep the parent host, builder support and out root fixed; they
\ mutate private app inputs. Copy the builder path before another image lookup
\ can reuse its buffer, resolving once before staging a child process.
: BUILDER$ ( -- ptr u8 n )
   BUILDER-U @ 0= if
      APP-BUILDER-ENGINE:PATH$ {: path:ptr pathu:n :}
      pathu PATH-CAP > if E-FS-CAPACITY throw then
      path BUILDER pathu BYTE-COPY
      pathu BUILDER-U !
   then
   BUILDER BUILDER-U @ ;

\ An empty stream reserved nothing, so it has no byte to view.
: OUT$ ( -- ptr u8 n ) OUT-U @ 0= if s" " exit then 0 OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n ) ERR-U @ 0= if s" " exit then 0 ERR ERR-U @ ;
\ The builder writes its file objects under the out root out (HABU_OUT,
\ tools/build-out.f).
: BUILD ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: source:ptr sourceu:n product:ptr productu:n out:ptr outu:n :}
   BUILDER$ {: builder:ptr builderu:n :}
   PROC-ARGV-ENV-RESET
   s" HABU_OUT" >LEN out outu >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --" ARG source sourceu ARG product productu ARG
   S\" 1 set-tier\nAPP-BUILD:RUN-SAVED-REPORT\n" {: stdin:ptr stdinu:n :}
   0 PROGRAM-U ! builder builderu stdin stdinu PROGRAM CAP PROGRAM-U PROC-ARGV-PROGRAM+
   builder builderu >LEN stdin stdinu >LEN
   [: OUT-ROOM ;] [: ERR-ROOM ;] 600000 >MS
   RUN-ARGV-ENV-STDIN-GROWING-BOTH-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U !
   PROGRAM PROGRAM-U @ OUT$ ERR$ rc SUBJECT:RC-TIMED-OUT
   rc ;
\ BUILD of root/source into root/product with the out root root/cache.
: BUILD-AT ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: root:ptr rootu:n source:ptr sourceu:n product:ptr productu:n :}
   root rootu source sourceu SRC-PATH FS:JOIN-PATH SRC-PATH swap
   root rootu product productu PROD-PATH FS:JOIN-PATH PROD-PATH swap
   root rootu s" cache" CACHE-PATH FS:JOIN-PATH CACHE-PATH swap
   BUILD ;
;package

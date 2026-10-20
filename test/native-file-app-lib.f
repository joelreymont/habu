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
create OUT CAP allot variable OUT-U
create ERR CAP allot variable ERR-U
create PROGRAM CAP allot variable PROGRAM-U
create BUILDER FS-PATH-CAP allot variable BUILDER-U
create SRC-PATH FS-PATH-CAP allot
create PROD-PATH FS-PATH-CAP allot
create CACHE-PATH FS-PATH-CAP allot
DYNAMIC-BUFFER SOURCE u8
variable SOURCE-START

: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;
: SKIP-LINE ( n -- ) {: size:n :}
   begin SOURCE-START @ size < while
      SOURCE-START @ SOURCE c@ 10 = if
         1 SOURCE-START +! exit
      then
      1 SOURCE-START +!
   repeat
   E-FS-IO throw ;

public
\ Resolve the production REPL builder before staging a child process. Its
\ keyed-image path buffer can be reused by a later image lookup.
: BUILDER$ ( -- ptr u8 n )
   APP-BUILDER-ENGINE:PATH$ {: path:ptr pathu:n :}
   pathu FS-PATH-CAP > if E-FS-CAPACITY throw then
   path BUILDER pathu BYTE-COPY
   pathu BUILDER-U !
   BUILDER BUILDER-U @ ;

\ Rehost the explicit-build fixtures: app.f requires local prefix/unit files,
\ so their original leading compiler/prefix requires are omitted.
: COPY-SKIP-LINES ( ptr u8 n ptr u8 n n -- )
   {: from:ptr fromu:n into:ptr intou:n lines:n :}
   from fromu FILE-SIZE {: size:n :}
   size SOURCE-RESERVE
   from fromu 0 SOURCE BYTE-VIEW size READ-ALL size <> if E-FS-IO throw then
   0 SOURCE-START !
   lines 0 ?do size SKIP-LINE loop
   into intou 0 SOURCE BYTE-VIEW SOURCE-START @ +
      size SOURCE-START @ - WRITE-ALL ;
: OUT$ ( -- ptr u8 n ) OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;
: BUILD ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: source:ptr sourceu:n product:ptr productu:n cache:ptr cacheu:n :}
   BUILDER$ {: builder:ptr builderu:n :}
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN cache cacheu >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --" ARG source sourceu ARG product productu ARG
   S\" 1 set-tier\nAPP-BUILD:RUN-SAVED-REPORT\n" {: stdin:ptr stdinu:n :}
   0 PROGRAM-U ! builder builderu stdin stdinu PROGRAM CAP PROGRAM-U PROC-ARGV-PROGRAM+
   builder builderu >LEN stdin stdinu >LEN
   OUT CAP >LEN ERR CAP >LEN 600000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U !
   PROGRAM PROGRAM-U @ OUT$ ERR$ rc SUBJECT:RC-TIMED-OUT
   rc ;
\ BUILD of root/source into root/product with the object cache at root/cache.
: BUILD-AT ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: root:ptr rootu:n source:ptr sourceu:n product:ptr productu:n :}
   root rootu source sourceu SRC-PATH JOIN-PATH SRC-PATH swap
   root rootu product productu PROD-PATH JOIN-PATH PROD-PATH swap
   root rootu s" cache" CACHE-PATH JOIN-PATH CACHE-PATH swap
   BUILD ;
;package

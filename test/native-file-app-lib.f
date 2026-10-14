\ Small application driver for FILE acceptance tests. Each BUILD runs the
\ ordinary cached source loader, which packs and imports NUNIT objects.
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/le.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require tools/app-build-image.f
require tools/native-file-object.f

package NATIVE-FILE-APP

$8000 constant CAP
create OUT CAP allot variable OUT-U
create ERR CAP allot variable ERR-U
create TARGET FS-PATH-CAP allot variable TARGET-U
create CACHE FS-PATH-CAP allot variable CACHE-U
create BUILDER FS-PATH-CAP allot variable BUILDER-U
DYNAMIC-BUFFER OBJECT u8
DYNAMIC-BUFFER SOURCE u8
variable SOURCE-START
variable FOUND
variable CODE-N variable RECORD-N variable UPDATE-N variable REQUIRE-N

: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;
: SAVE-BUILDER ( -- )
   600000 APP-BUILD-IMAGE:PATH$ {: path:ptr pathu:n :}
   pathu FS-PATH-CAP > if E-FS-CAPACITY throw then
   path BUILDER pathu BYTE-COPY
   pathu BUILDER-U ! ;
: SELECT-BUILDER ( -- )
   ENGINE-CANDIDATE:PATH$ BUILD-FIXPOINT:BF-ENGINE!
   ['] SAVE-BUILDER catch {: rc:n :}
   BUILD-FIXPOINT:BF-ENGINE-RESET
   rc 0<> if rc throw then ;
: READ-OBJECT ( ptr u8 n -- n )
   {: path:ptr pathu:n :}
   path pathu FILE-SIZE {: size:n :}
   size OBJECT-RESERVE
   path pathu 0 OBJECT BYTE-VIEW size READ-ALL size <> if E-FS-IO throw then
   size ;
: SKIP-LINE ( n -- ) {: size:n :}
   begin SOURCE-START @ size < while
      SOURCE-START @ SOURCE c@ 10 = if
         1 SOURCE-START +! exit
      then
      1 SOURCE-START +!
   repeat
   E-FS-IO throw ;
: PROFILE-OBJECT ( ptr u8 n -- )
   {: path:ptr pathu:n :}
   path pathu s" .hfo" ENDS-WITH? 0= if exit then
   path pathu READ-OBJECT {: size:n :}
   size 128 < if exit then
   0 OBJECT BYTE-VIEW 16 + LE:U64@ {: pathlen:n :}
   0 OBJECT BYTE-VIEW 24 + LE:U64@ {: rootlen:n :}
   pathlen 0 < rootlen 0 < or if exit then
   pathlen size 128 - > if exit then
   rootlen size 128 pathlen + - > if exit then
   0 OBJECT BYTE-VIEW 128 + pathlen TARGET TARGET-U @ ENDS-WITH? 0= if exit then
   FOUND @ 0<> if E-FS-IO throw then
   1 FOUND !
   path pathu
   0 OBJECT BYTE-VIEW 128 + pathlen
   0 OBJECT BYTE-VIEW 128 pathlen + + rootlen
   0 OBJECT BYTE-VIEW 32 + 64
   0 OBJECT BYTE-VIEW 96 + LE:U64@
      NFILE-OBJECT:TRY-OPEN {: h:n hit:bool :}
   hit 0= if E-FS-IO throw then
   begin
      h NFILE-OBJECT:NEXT
      {: kind:n end:n mode:n bytes:ptr payload:n more:bool :}
      end drop mode drop
      more
   while
      kind NFILE-OBJECT:RANGE-KIND = if
         payload 272 < if E-FS-IO throw then
         bytes LE:U64@ {: unitlen:n :}
         unitlen 264 < unitlen 8 + payload > or if E-FS-IO throw then
         bytes 8 + {: unit:ptr :}
         unit 8 s" HBNUNIT2" STR= 0= if E-FS-IO throw then
         unit 160 + LE:U64@ CODE-N +!
         unit 168 + LE:U64@ RECORD-N +!
         unit 216 + LE:U64@ UPDATE-N +!
         unit 224 + LE:U64@ REQUIRE-N +!
      then
   repeat
   NFILE-OBJECT:CLOSE ;

public
\ Resolve the production REPL builder before staging a child process. Its
\ keyed-image path buffer can be reused by a later image lookup.
: BUILDER$ ( -- ptr u8 n )
   SELECT-BUILDER
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
   builder builderu >LEN
   S\" 1 set-tier\nAPP-BUILD:RUN-SAVED-REPORT\n" >LEN
   OUT CAP >LEN ERR CAP >LEN 600000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U ! rc ;
: PROFILE ( ptr u8 n ptr u8 n -- n n n n )
   {: cache:ptr cacheu:n target:ptr targetu:n :}
   targetu FS-PATH-CAP > cacheu FS-PATH-CAP > or if E-FS-PATH throw then
   target TARGET targetu BYTE-COPY targetu TARGET-U !
   cache CACHE cacheu BYTE-COPY cacheu CACHE-U !
   0 FOUND ! 0 CODE-N ! 0 RECORD-N ! 0 UPDATE-N ! 0 REQUIRE-N !
   CACHE CACHE-U @ [: PROFILE-OBJECT ;] WALK-FILES
   FOUND @ 0= if E-FS-OPEN throw then
   CODE-N @ RECORD-N @ UPDATE-N @ REQUIRE-N @ ;
;package

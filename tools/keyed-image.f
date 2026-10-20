\ keyed-image.f - publish one keyed saved image in the existing build cache.
\ The caller supplies the family, key, builder engine, stdin program and
\ child timeout. Keys include source closure paths so images remain tied to
\ the tree that built them; publication is an atomic rename from a held work
\ directory. Test gate grants and budgets stay in test/keyed-image.f.

require src/lib/errors.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/build-cache.f
require src/lib/content-key.f
require tools/event-closure-lib.f

package KEYED-IMAGE

private

$10000 constant IO-CAP
75 constant BUILD-RC
128 constant NAME-CAP

create NAME-BUF NAME-CAP allot
create ENGINE-BUF FS-PATH-CAP allot
create WORK-BUF FS-PATH-CAP allot
create TMP-BUF FS-PATH-CAP allot
create OUT IO-CAP allot
create ERR IO-CAP allot

variable NAME-U
variable ENGINE-U
variable WORK-U
variable WORK-FD
variable TMP-U
variable EMIT-RC
variable CLOSURE-IDX
variable CHILD-TIMEOUT-MS
variable CAPTURE-OUT-U
variable CAPTURE-ERR-U
variable QUIET
variable RESULT-PATH
TYPED-VARIABLE RESULT-XT [ ptr u8 n -- ptr u8 n ]

\ The build in progress, which ENSURE records for the words it runs under
\ catch.
TYPED-VARIABLE FAMILY-A ptr u8
variable FAMILY-U
TYPED-VARIABLE PATH-A ptr u8
variable PATH-U
TYPED-VARIABLE PROGRAM-A ptr u8
variable PROGRAM-U
TYPED-VARIABLE ARGS-XT [ ptr u8 n -- ]

: FAMILY$ ( -- ptr u8 n )
   FAMILY-A @ FAMILY-U @ ;

: PATH$ ( -- ptr u8 n )
   PATH-A @ PATH-U @ ;

: PROGRAM$ ( -- ptr u8 n )
   PROGRAM-A @ PROGRAM-U @ ;

: ENGINE$ ( -- ptr u8 n )
   ENGINE-BUF ENGINE-U @ ;

: WORK$ ( -- ptr u8 n )
   WORK-BUF WORK-U @ ;

: TMP$ ( -- ptr u8 n )
   TMP-BUF TMP-U @ ;

: NAME$ ( -- ptr u8 n )
   NAME-BUF NAME-U @ ;

: NAME+ ( ptr u8 n -- ) {: a:ptr u:n :}
   NAME-U @ u + NAME-CAP > if E-FS-CAPACITY throw then
   a NAME-BUF NAME-U @ + u BYTE-COPY
   NAME-U @ u + NAME-U ! ;

\ hb-<family>, the stem of every image name in the family.
: STEM! ( ptr u8 n -- ) {: a:ptr u:n :}
   0 NAME-U !
   s" hb-" NAME+
   a u NAME+ ;

: COPY-OUT! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   u 0 <= if E-FS-PATH throw then
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a dst u BYTE-COPY
   u up ! ;

\ The work directory is held until WORK-CLOSE and registered for removal at
\ exit, so a die before WORK-CLOSE still removes it.
: WORK-OPEN ( -- )
   FAMILY$ BUILD-CACHE:WORK-OPEN FD>N WORK-FD !
   WORK-BUF WORK-U COPY-OUT!
   FAMILY$ STEM!
   WORK$ NAME$ TMP-BUF JOIN-PATH TMP-U ! ;

: WORK-CLOSE ( -- )
   WORK$ WORK-FD @ >FD BUILD-CACHE:WORK-CLOSE ;

\ <family>: <what>, the message a failed build dies with.
: FAILED$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   SB-RESET
   FAMILY$ SB-APPEND
   s" : " SB-APPEND
   a u SB-APPEND
   SB$ ;

: BUILD-RUN ( -- )
   PROC-ARGV-ENV-RESET
   \ The capture reads HABU_WHITEBOX_IMAGE (src/habu/aot-capture.f
   \ ACAP-WHITEBOX?) and no key folds it: it is cleared before the inherit, so
   \ no parent environment decides the bytes of a keyed image.
   s" HABU_WHITEBOX_IMAGE" >LEN s" " >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --" >LEN PROC-ARGV+
   TMP$ ARGS-XT @ execute
   ENGINE$ >LEN PROGRAM$ >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN CHILD-TIMEOUT-MS @ >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N CAPTURE-OUT-U !
   erru LEN>N CAPTURE-ERR-U !
   \ The caller owns timeout reporting. Keep the capture off stdout so a
   \ structured CLI error can be the only output, then EMIT closes the work.
   rc PROC-TIMEOUT-RC = if E-PROC-TIMEOUT throw then
   QUIET @ 0= rc 0<> or if OUT outu LEN>N type then
   2 ERR erru LEN>N write drop
   rc 0 <> if s" image build failed" FAILED$ rc die then ;

: PUBLISH ( -- )
   TMP$ EXECUTABLE? 0= if
      s" builder produced no executable image" FAILED$ BUILD-RC die
   then
   RESULT-PATH @ if
      TMP$ RESULT-XT @ execute {: path:ptr pathu:n :}
      path PATH-A ! pathu PATH-U !
   then
   TMP$ PATH$ RENAME-FILE ;

\ Pruning reports its own failures and never fails the build. The family name
\ is also the stem of the <family>-<seed>-<attempt> work directories a builder
\ that holds none made, which pruning takes once they are a day old.
: EMIT ( -- )
   WORK-OPEN
   ['] BUILD-RUN catch EMIT-RC !
   EMIT-RC @ 0 = if ['] PUBLISH catch EMIT-RC ! then
   WORK-CLOSE
   EMIT-RC @ 0 <> if EMIT-RC @ throw then
   FAMILY$ STEM!
   s" -" NAME+
   NAME$ s" " FAMILY$ PATH$ BUILD-CACHE:PRUNE ;

\ Fold the paths after either entry-resolution route has filled EC.
: CLOSURE-FILES+ ( CONTENT-KEY:fold -- CONTENT-KEY:fold )
   0 CLOSURE-IDX !
   begin CLOSURE-IDX @ EC:COUNT < while
      CLOSURE-IDX @ EC:PATH$ CONTENT-KEY:FILE+
      CLOSURE-IDX @ 1+ CLOSURE-IDX !
   repeat ;

public

\ The hex digits of a key: a family's key buffer holds this many.
64 constant KEY-HEX-LEN

\ A timed-out producer keeps its captured bytes until the caller has rendered
\ the maker error. Clear before a new attempt so an earlier build cannot leak.
: CAPTURE-RESET ( -- )
   0 CAPTURE-OUT-U ! 0 CAPTURE-ERR-U ! ;

: CAPTURE-OUT$ ( -- ptr u8 n ) OUT CAPTURE-OUT-U @ ;
: CAPTURE-ERR$ ( -- ptr u8 n ) ERR CAPTURE-ERR-U @ ;

\ Fold the ordered require/include closure of an entry file into a key, each
\ member by its content and its canonical absolute path. EC:PATH$ is the path
\ the loader resolved, the bytes `required` hands REQUIRE-STORE, so it is the
\ name the image's registry records for an application row. Its tree-relative
\ EC:NAME$ is the same in every checkout and would let one tree run another's
\ image.
: CLOSURE+ ( CONTENT-KEY:fold ptr u8 n -- CONTENT-KEY:fold )
   EC:BUILD CLOSURE-FILES+ ;

\ Observe each member before discovery reads it, then fold the same closure.
\ This lets a producer record make inputs while computing its existing key.
: CLOSURE-IN-EACH+ ( CONTENT-KEY:fold ptr u8 n ptr u8 n [ ptr u8 n -- ] -- CONTENT-KEY:fold )
   EC:BUILD-IN-EACH CLOSURE-FILES+ ;

\ A caller that already selected a source under a particular owner preserves
\ that root while folding its transitive closure.
: CLOSURE-IN+ ( CONTENT-KEY:fold ptr u8 n ptr u8 n -- CONTENT-KEY:fold )
   [: 2drop ;] CLOSURE-IN-EACH+ ;

\ The keyed path of a family's image, from its KEY-HEX-LEN hex digits and the
\ family name, into the caller's FS-PATH-CAP buffer and length cell.
: PATH! ( ptr u8 ptr u8 n ptr u8 ptr n -- )
   {: key:ptr fam:ptr famu:n dst:ptr up:ptr :}
   fam famu STEM!
   s" -" NAME+
   key KEY-HEX-LEN NAME+
   BUILD-CACHE:ROOT$ NAME$ dst JOIN-PATH up ! ;

\ Build the family's image at its keyed path unless it is already there.
\ An image found there is marked in
\ use, and one a pruner took meanwhile is built again. The build runs the
\ engine with the program on stdin and `--` then what ARGS stages, given the
\ path the builder must save to. The engine path is copied; the family name,
\ keyed path and program are read until ENSURE-WITH returns. TRUE means the existing
\ image was selected; FALSE means this call built and published it.
: ENSURE-WITH ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n [ ptr u8 n -- ] n -- bool )
   {: fam:ptr famu:n path:ptr pathu:n eng:ptr engu:n prog:ptr progu:n args timeout:n :}
   timeout CHILD-TIMEOUT-MS !
   path pathu EXECUTABLE? if path pathu BUILD-CACHE:USED if true exit then then
   fam FAMILY-A ! famu FAMILY-U !
   path PATH-A ! pathu PATH-U !
   prog PROGRAM-A ! progu PROGRAM-U !
   args ARGS-XT !
   eng engu ENGINE-BUF ENGINE-U COPY-OUT!
   false QUIET !
   0 RESULT-PATH !
   ['] EMIT catch {: rc:n :}
   0 QUIET !
   rc 0<> if rc throw then
   false ;

\ A builder whose consumed inputs determine its key chooses the destination
\ only after its child succeeds and the temporary image is executable.
: BUILD-WITH-RESULT-QUIET ( ptr u8 n ptr u8 n ptr u8 n [ ptr u8 n -- ] n bool [ ptr u8 n -- ptr u8 n ] -- ptr u8 n )
   {: fam:ptr famu:n eng:ptr engu:n prog:ptr progu:n args timeout:n quiet:bool result :}
   fam FAMILY-A ! famu FAMILY-U !
   prog PROGRAM-A ! progu PROGRAM-U !
   args ARGS-XT !
   eng engu ENGINE-BUF ENGINE-U COPY-OUT!
   timeout CHILD-TIMEOUT-MS !
   quiet QUIET !
   result RESULT-XT !
   -1 RESULT-PATH !
   ['] EMIT catch {: rc:n :}
   0 QUIET !
   0 RESULT-PATH !
   rc 0<> if rc throw then
   PATH$ ;

;package

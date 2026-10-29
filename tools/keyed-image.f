\ keyed-image.f - publish one keyed saved image under the tree's out root.
\ A family's image is BUILD-OUT:IMG$ <family> in the directory of the engine
\ that builds it, and <family>.key beside it holds the KEY-HEX-LEN hex digits it
\ was built under: FRESH? is that key equal to the caller's. The caller supplies
\ the family, key, builder engine path and that engine's key (BUILD-OUT:KEY-OF!,
\ computed once by the caller; every word that names a slot takes it right after
\ the family), stdin program and child timeout. Keys include source closure paths so images remain
\ tied to the tree that built them. A build writes into its own work directory
\ under img/ (WORK-OPEN), and a build killed before PUBLISH leaves only that
\ directory. PUBLISH empties the key, renames the image into place and writes
\ its key, each step an atomic replace that unlinks nothing: a reader between
\ them finds the empty key and builds again, never a new image under an old
\ key, and publishers of one family under one key cannot fail each other. A
\ build killed inside PUBLISH leaves the empty key, which reads as a miss, and
\ may leave the key write's temporary file beside it.
\ A slot belongs to one builder engine, but the image and its key are still two
\ files, so publishers of one family under two keys can interleave and leave
\ one's image under the other's key. One out root therefore serves one tree's
\ builds of a family on one builder engine under one selector key at a time.
\ Two keys remain possible where a selector key folds an input other than the
\ builder engine's bytes that two selections sharing the out root can vary: the
\ working directory, the engine's path spelling, donor map, target, selected
\ root or source closure. Sharing HABU_OUT across trees is outside that
\ contract. Test gate grants and budgets stay in test/keyed-image.f.

require src/lib/errors.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/content-key.f
require tools/build-out.f
require tools/event-closure-lib.f

package KEYED-IMAGE

public

\ The hex digits of a key: a family's key buffer holds this many.
64 constant KEY-HEX-LEN

private

$10000 constant IO-CAP
75 constant BUILD-RC

create ENGINE-BUF PATH-CAP allot
create WORK-BUF PATH-CAP allot
create TMP-BUF PATH-CAP allot
create PATH-BUF PATH-CAP allot
create KEY-PATH-BUF PATH-CAP allot
create KEY-READ KEY-HEX-LEN 1 + allot
create OUT IO-CAP allot
create ERR IO-CAP allot

variable ENGINE-U
variable WORK-U
variable TMP-U
variable PATH-U
variable KEY-PATH-U
variable EMIT-RC
variable CLOSURE-IDX
variable CHILD-TIMEOUT-MS
variable CAPTURE-OUT-U
variable CAPTURE-ERR-U
variable QUIET
variable RESULT-KEY
TYPED-VARIABLE RESULT-XT [ ptr u8 n -- ptr u8 ]

\ The build in progress, which ENSURE-WITH records for the words it runs under
\ catch.
TYPED-VARIABLE FAMILY-A ptr u8
variable FAMILY-U
TYPED-VARIABLE KEY-A ptr u8
TYPED-VARIABLE EKEY-A ptr u8
TYPED-VARIABLE PROGRAM-A ptr u8
variable PROGRAM-U
TYPED-VARIABLE ARGS-XT [ ptr u8 n -- ]

: FAMILY$ ( -- ptr u8 n )
   FAMILY-A @ FAMILY-U @ ;

: PROGRAM$ ( -- ptr u8 n )
   PROGRAM-A @ PROGRAM-U @ ;

: ENGINE$ ( -- ptr u8 n )
   ENGINE-BUF ENGINE-U @ ;

: WORK$ ( -- ptr u8 n )
   WORK-BUF WORK-U @ ;

: TMP$ ( -- ptr u8 n )
   TMP-BUF TMP-U @ ;

: PATH$ ( -- ptr u8 n )
   PATH-BUF PATH-U @ ;

: KEY-PATH$ ( -- ptr u8 n )
   KEY-PATH-BUF KEY-PATH-U @ ;

: COPY-OUT! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   u 0 <= if E-FS-PATH throw then
   u PATH-CAP > if E-FS-CAPACITY throw then
   a dst u BYTE-COPY
   u up ! ;

\ The family's image path into PATH-BUF and its key path into KEY-PATH-BUF,
\ under the directory of the engine whose key follows the family.
: PATHS! ( ptr u8 n ptr u8 -- )
   BUILD-OUT:IMG$ PATH-BUF PATH-U COPY-OUT!
   PATH-U @ 4 + PATH-CAP > if E-FS-CAPACITY throw then
   PATH-BUF KEY-PATH-BUF PATH-U @ BYTE-COPY
   s" .key" drop KEY-PATH-BUF PATH-U @ + 4 BYTE-COPY
   PATH-U @ 4 + KEY-PATH-U ! ;

public

\ The path of a family's image, built by the engine whose key follows the
\ family, into the caller's PATH-CAP buffer and length cell.
: PATH! ( ptr u8 n ptr u8 ptr u8 ptr n -- )
   {: fam:ptr famu:n ekey:ptr dst:ptr up:ptr :}
   fam famu ekey BUILD-OUT:IMG$ dst up COPY-OUT! ;

\ Whether the family's image, in the directory of the engine whose key follows
\ the family, is executable and was published under this key.
\ A key that cannot be opened (missing) or holds any length other than
\ KEY-HEX-LEN is a miss: the empty key of a publish in progress, and a longer
\ file, which FS:READ-ALL refuses with E-FS-CAPACITY. Every other error throws.
: FRESH? ( ptr u8 ptr u8 n ptr u8 -- bool )
   {: key:ptr fam:ptr famu:n ekey:ptr :}
   fam famu ekey PATHS!
   PATH$ FS:EXECUTABLE? 0= if false exit then
   0 [: drop KEY-PATH$ KEY-READ KEY-HEX-LEN 1 + FS:READ-ALL ;] catch {: rc:n :}
   rc E-FS-OPEN = rc E-FS-CAPACITY = or if drop false exit then
   rc 0<> if rc throw then
   KEY-HEX-LEN <> if false exit then
   KEY-READ KEY-HEX-LEN key KEY-HEX-LEN STR= ;

\ A build's own directory under the img/ of the engine whose key follows the
\ family, named <family>-<seed>-<attempt>, which the caller copies before
\ another WORK-OPEN. It is registered for removal at exit, so a die before
\ WORK-CLOSE still removes it.
: WORK-OPEN ( ptr u8 n ptr u8 -- ptr u8 n )
   {: fam:ptr famu:n ekey:ptr :}
   fam famu ekey BUILD-OUT:IMG$ SOURCE-ROOT:DIRNAME fam famu FS:MKTEMP-DIR-IN
   2dup FS:CLEANUP-TREE+ ;

: WORK-CLOSE ( ptr u8 n -- )
   2dup FS:REMOVE-TREE FS:CLEANUP-FORGET ;

\ Publish the image at tmp as the family's, built under the key by the engine
\ whose key follows the family: an empty key first, which FRESH? reads as a
\ miss, then the image, then the key.
: PUBLISH ( ptr u8 ptr u8 n ptr u8 ptr u8 n -- )
   {: key:ptr fam:ptr famu:n ekey:ptr tmp:ptr tmpu:n :}
   fam famu ekey PATHS!
   KEY-PATH$ key 0 FS:ATOMIC-WRITE-FILE
   tmp tmpu PATH$ FS:RENAME-FILE
   KEY-PATH$ key KEY-HEX-LEN FS:ATOMIC-WRITE-FILE ;

private

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

: PUBLISH-BUILT ( -- )
   TMP$ FS:EXECUTABLE? 0= if
      s" builder produced no executable image" FAILED$ BUILD-RC die
   then
   RESULT-KEY @ if TMP$ RESULT-XT @ execute KEY-A ! then
   KEY-A @ FAMILY$ EKEY-A @ TMP$ PUBLISH ;

\ The work directory goes whatever the build did, and a failure keeps its own
\ code: a throw is caught here, and a die in BUILD-RUN or PUBLISH-BUILT ends the
\ process, whose exit registry removes what WORK-OPEN registered.
: EMIT ( -- )
   FAMILY$ EKEY-A @ WORK-OPEN WORK-BUF WORK-U COPY-OUT!
   WORK$ FAMILY$ TMP-BUF FS:JOIN-PATH TMP-U !
   ['] BUILD-RUN catch EMIT-RC !
   EMIT-RC @ 0 = if ['] PUBLISH-BUILT catch EMIT-RC ! then
   WORK$ WORK-CLOSE
   EMIT-RC @ 0 <> if EMIT-RC @ throw then ;

\ Fold the paths after either entry-resolution route has filled EC.
: CLOSURE-FILES+ ( CONTENT-KEY:fold -- CONTENT-KEY:fold )
   0 CLOSURE-IDX !
   begin CLOSURE-IDX @ EC:COUNT < while
      CLOSURE-IDX @ EC:PATH$ CONTENT-KEY:FILE+
      CLOSURE-IDX @ 1+ CLOSURE-IDX !
   repeat ;

public

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

\ Build the family's image under the key unless it is already there. The build
\ runs the engine with the program on stdin and `--` then what ARGS stages,
\ given the path the builder must save to; that engine's key follows the
\ family. The engine path is copied; the key, engine key, family name and
\ program are read until ENSURE-WITH returns. TRUE means the existing image was
\ selected; FALSE means this call built and published it.
: ENSURE-WITH ( ptr u8 ptr u8 n ptr u8 ptr u8 n ptr u8 n [ ptr u8 n -- ] n -- bool )
   {: key:ptr fam:ptr famu:n ekey:ptr eng:ptr engu:n prog:ptr progu:n args timeout:n :}
   timeout CHILD-TIMEOUT-MS !
   key fam famu ekey FRESH? if true exit then
   fam FAMILY-A ! famu FAMILY-U !
   key KEY-A !
   ekey EKEY-A !
   prog PROGRAM-A ! progu PROGRAM-U !
   args ARGS-XT !
   eng engu ENGINE-BUF ENGINE-U COPY-OUT!
   false QUIET !
   0 RESULT-KEY !
   ['] EMIT catch {: rc:n :}
   0 QUIET !
   rc 0<> if rc throw then
   false ;

\ A builder whose consumed inputs determine its key learns the key only after
\ its child succeeds and the temporary image is executable: RESULT answers it
\ from the temporary image's path. The engine's key follows the family, as for
\ ENSURE-WITH. Answers the published image's path.
: BUILD-WITH-RESULT-QUIET ( ptr u8 n ptr u8 ptr u8 n ptr u8 n [ ptr u8 n -- ] n bool [ ptr u8 n -- ptr u8 ] -- ptr u8 n )
   {: fam:ptr famu:n ekey:ptr eng:ptr engu:n prog:ptr progu:n args timeout:n quiet:bool result :}
   fam FAMILY-A ! famu FAMILY-U !
   ekey EKEY-A !
   prog PROGRAM-A ! progu PROGRAM-U !
   args ARGS-XT !
   eng engu ENGINE-BUF ENGINE-U COPY-OUT!
   timeout CHILD-TIMEOUT-MS !
   quiet QUIET !
   result RESULT-XT !
   -1 RESULT-KEY !
   ['] EMIT catch {: rc:n :}
   0 QUIET !
   0 RESULT-KEY !
   rc 0<> if rc throw then
   FAMILY$ EKEY-A @ PATHS!
   PATH$ ;

;package

\ keyed-image.f - one keyed image in the build cache: where it lives, how a
\ caller finds it, and how it is built and published.
\
\ A family module owns what makes its image what it is: the content key, the
\ engine that runs the builder, the program the builder is handed on stdin, and
\ the arguments after `--`. test/fixture-writer.f, test/app-image-engine.f and
\ test/preloaded-engine.f are three. This module owns what is the same for
\ every family:
\
\ THE PATH IS THE KEY. An image lives at <cache root>/hb-<family>-<key hex>, and
\ a key covers everything its build reads and every name the image records, so
\ another engine, an edit in a keyed closure or another tree names another
\ file, and no image runs against sources other than those it was built from.
\ CLOSURE+ folds a closure into a key; discovery rejects fail-closed, so a
\ closure that cannot be reproduced cannot be keyed - the key never silently
\ covers fewer files than the build reads.
\
\ EACH TREE HAS ITS OWN IMAGES. An application image's require registry records
\ every application row by its canonical absolute path (REQUIRE-STORE,
\ src/core/include.f). Run from another checkout of the same bytes - master and
\ a .jj-ws workspace at one revision - it names the builder's files, so a
\ `require` of a closure member in the running tree misses the registry, loads
\ the file again and dies on a duplicate definition (measured from a copy:
\ test/compiler/aot-nested-body.f, `duplicate definition: FL-DOT`, exit 78).
\ The key therefore folds each member under that canonical name, and every tree
\ resolves its own key.
\
\ PUBLISHED BY RENAME. The builder saves into a private work directory in the
\ cache root (BUILD-CACHE:WORK-OPEN), so a half-written image is never visible
\ at the keyed path: only the closing rename publishes it. A second builder
\ racing this one saves an image from the same keyed sources and the rename is
\ atomic, so losing the race costs one discarded build and nothing else. The
\ work directory goes whatever the build did, and a failure keeps its own code:
\ a throw is caught here, and a die ends the process, whose exit registry
\ removes the work directory it was registered with. A build killed before
\ either leaves the directory, and the next build in that root removes it
\ once nothing holds it - the build, or the builder child that inherited its
\ hold (lib/build-cache.f, A WORK DIRECTORY IS HELD WHILE ITS BUILD LIVES).
\
\ RETAINED WHILE USED. A published image prunes its family and an image found on
\ disk is dated as in use (lib/build-cache.f), so the cache holds what some
\ tree still runs and nothing older.
\
\ SETTLED BY THE GATE. Under a gate a row settles only the images the gate
\ granted it, which the gate's own build rows settled before the row started
\ (test/image-grant.f, test/gate-images.f); ENSURE asks before it looks.
\
\ An image is handed out in place: its callers only execute it, and it writes
\ nothing but the paths they name, so no caller can clobber the shared bytes.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/build-cache.f
require lib/content-key.f
require tools/event-closure-lib.f
require test/image-grant.f

package KEYED-IMAGE

$10000 constant IO-CAP
240000 constant BUILD-TIMEOUT-MS
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
   PROC-ENV-INHERIT-MISSING
   s" --" >LEN PROC-ARGV+
   TMP$ ARGS-XT @ execute
   ENGINE$ >LEN PROGRAM$ >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN BUILD-TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   OUT outu LEN>N type
   2 ERR erru LEN>N write drop
   rc 0 <> if s" image build failed" FAILED$ rc die then ;

: PUBLISH ( -- )
   TMP$ EXECUTABLE? 0= if
      s" builder produced no executable image" FAILED$ BUILD-RC die
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

public

\ The hex digits of a key: a family's key buffer holds this many.
64 constant KEY-HEX-LEN

\ Fold the ordered require/include closure of an entry file into a key, each
\ member by its content and its canonical absolute path. EC:PATH$ is the path
\ the loader resolved, the bytes `required` hands REQUIRE-STORE, so it is the
\ name the image's registry records for an application row. Its tree-relative
\ EC:NAME$ is the same in every checkout and would let one tree run another's
\ image.
: CLOSURE+ ( CONTENT-KEY:fold ptr u8 n -- CONTENT-KEY:fold )
   EC:BUILD
   0 CLOSURE-IDX !
   begin CLOSURE-IDX @ EC:COUNT < while
      CLOSURE-IDX @ EC:PATH$ CONTENT-KEY:FILE+
      CLOSURE-IDX @ 1+ CLOSURE-IDX !
   repeat ;

\ The keyed path of a family's image, from its KEY-HEX-LEN hex digits and the
\ family name, into the caller's FS-PATH-CAP buffer and length cell.
: PATH! ( ptr u8 ptr u8 n ptr u8 ptr n -- )
   {: key:ptr fam:ptr famu:n dst:ptr up:ptr :}
   fam famu STEM!
   s" -" NAME+
   key KEY-HEX-LEN NAME+
   BUILD-CACHE:ROOT$ NAME$ dst JOIN-PATH up ! ;

\ Build the family's image at its keyed path unless it is already there, once
\ the family is granted (IMAGE-GRANT:CHECK). An image found there is marked in
\ use, and one a pruner took meanwhile is built again. The build runs the
\ engine with the program on stdin and `--` then what ARGS stages, given the
\ path the builder must save to. The engine path is copied; the family name,
\ keyed path and program are read until ENSURE returns.
: ENSURE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n [ ptr u8 n -- ] -- )
   {: fam:ptr famu:n path:ptr pathu:n eng:ptr engu:n prog:ptr progu:n args :}
   fam famu IMAGE-GRANT:CHECK
   path pathu EXECUTABLE? if path pathu BUILD-CACHE:USED if exit then then
   fam FAMILY-A ! famu FAMILY-U !
   path PATH-A ! pathu PATH-U !
   prog PROGRAM-A ! progu PROGRAM-U !
   args ARGS-XT !
   eng engu ENGINE-BUF ENGINE-U COPY-OUT!
   EMIT ;

;package

\ native-unit-image.f - the NBR package unit exported from this tree, built
\ once per gate and shared.
\
\ tools/native-unit-build.f --export-unit runs a whole native build and writes,
\ beside its engine, the unit of src/compiler/native/branch.f (package NBR): its
\ relocated code, dictionary rows and checker records. A build that imports the
\ unit publishes them in place of compiling that source. The unit is keyed to
\ the build that exported it (tools/native-source-view.f UNIT-KEY): the host
\ engine, and the canonical path and bytes of every file loaded up to NBR. Only
\ a build of this tree, by this engine, imports it; a unit exported in another
\ directory is refused there with E-NUNIT-FILE. So the export runs in the tree
\ the caller runs in, and the cache key folds the builder's closure under its
\ canonical paths (KEYED-IMAGE:CLOSURE+): every tree resolves its own unit.
\
\ The export builds the unsealed class, as the import rows do, so an import's
\ engine compares with test/whitebox-engine.f's. The unit is published by
\ rename from a work directory, as a keyed image is (test/keyed-image.f), and
\ the exported engine goes with that directory. Under a gate the unit must be
\ granted (test/image-grant.f): the gate's native-unit build row settles it
\ (test/gate-images.f), and a row that loads this module starts after it.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/build-cache.f
require lib/content-key.f
require lib/engine-candidate.f
require test/image-grant.f
require test/keyed-image.f

package NATIVE-UNIT-IMAGE

$10000 constant IO-CAP
128 constant NAME-CAP

\ The exit status of an export that wrote no unit.
75 constant UNIT-RC

create KEY-HEX KEYED-IMAGE:KEY-HEX-LEN allot
create NAME-BUF NAME-CAP allot
create PATH-BUF FS-PATH-CAP allot
create WORK-BUF FS-PATH-CAP allot
create UNIT-BUF FS-PATH-CAP allot
create ENGINE-BUF FS-PATH-CAP allot
create OUT IO-CAP allot
create ERR IO-CAP allot

variable NAME-U
variable PATH-U
variable WORK-U
variable WORK-FD
variable UNIT-U
variable ENGINE-U
variable EMIT-RC
variable RESOLVED?

: BUILDER$ ( -- ptr u8 n )
   s" tools/native-unit-build.f" ;

\ The family name: the prefix of the build's work directory, and the name a gate
\ grants the unit under (test/image-grant.f).
: FAMILY$ ( -- ptr u8 n )
   s" native-unit" ;

\ A published unit is nbr-<key hex>.unit in the build cache.
: PREFIX$ ( -- ptr u8 n )
   s" nbr-" ;

: SUFFIX$ ( -- ptr u8 n )
   s" .unit" ;

: PATH-BYTES ( -- ptr u8 n )
   PATH-BUF PATH-U @ ;

: WORK-BYTES ( -- ptr u8 n )
   WORK-BUF WORK-U @ ;

: UNIT-BYTES ( -- ptr u8 n )
   UNIT-BUF UNIT-U @ ;

: ENGINE-BYTES ( -- ptr u8 n )
   ENGINE-BUF ENGINE-U @ ;

: NAME+ ( ptr u8 n -- ) {: a:ptr u:n :}
   NAME-U @ u + NAME-CAP > if E-FS-CAPACITY throw then
   a NAME-BUF NAME-U @ + u BYTE-COPY
   NAME-U @ u + NAME-U ! ;

: RESOLVE ( -- )
   RESOLVED? @ 0 <> if exit then
   CONTENT-KEY:OPEN
   s" native-unit-v1 NBR whitebox" CONTENT-KEY:TEXT+
   ENGINE-CANDIDATE:PATH$ s" host-engine" CONTENT-KEY:FILE-NAMED+
   BUILDER$ KEYED-IMAGE:CLOSURE+
   KEY-HEX CONTENT-KEY:FINAL-HEX
   0 NAME-U !
   PREFIX$ NAME+
   KEY-HEX KEYED-IMAGE:KEY-HEX-LEN NAME+
   SUFFIX$ NAME+
   BUILD-CACHE:ROOT$ NAME-BUF NAME-U @ PATH-BUF JOIN-PATH PATH-U !
   0 0= RESOLVED? ! ;

\ The export writes into a private directory of its own, held until WORK-CLOSE
\ and registered for removal at exit (BUILD-CACHE:WORK-OPEN).
: WORK-OPEN ( -- )
   FAMILY$ BUILD-CACHE:WORK-OPEN FD>N WORK-FD !
   {: a:ptr u:n :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a WORK-BUF u BYTE-COPY
   u WORK-U !
   WORK-BYTES s" nbr.unit" UNIT-BUF JOIN-PATH UNIT-U !
   WORK-BYTES s" hb-export" ENGINE-BUF JOIN-PATH ENGINE-U ! ;

: WORK-CLOSE ( -- )
   WORK-BYTES WORK-FD @ >FD BUILD-CACHE:WORK-CLOSE ;

: ARG ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

\ The variable goes in before the inherit, so the one row the child reads is
\ this one, and the `whitebox` argument has the builder check the class of the
\ engine it wrote (test/whitebox-engine.f BUILD-ARGS).
: BUILD-ARGS ( -- )
   PROC-ARGV-ENV-RESET
   s" HABU_WHITEBOX_IMAGE" >LEN s" 1" >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG
   BUILDER$ ARG
   s" --" ARG
   s" --export-unit" ARG
   s" NBR" ARG
   UNIT-BYTES ARG
   ENGINE-BYTES ARG
   s" whitebox" ARG ;

\ A deadline that expired, this capture's or one in the build, is named and
\ thrown again, so the gate pool labels this build row TIMEOUT-UNDER-LOAD.
: BUILD-RUN ( -- )
   BUILD-ARGS
   ENGINE-CANDIDATE:PATH$ >LEN s" " >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN KEYED-IMAGE:BUILD-TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   OUT outu LEN>N type
   2 ERR erru LEN>N write drop
   rc PROC-TIMEOUT-RC = if
      s" native-unit: NBR export ran out of time" type cr
      E-PROC-TIMEOUT throw
   then
   rc 0 <> if s" native-unit: NBR export failed" rc die then ;

: PUBLISH ( -- )
   UNIT-BYTES FILE? 0= if
      s" native-unit: NBR export wrote no unit" UNIT-RC die
   then
   UNIT-BYTES PATH-BYTES RENAME-FILE ;

\ A second exporter racing this one writes the same keyed bytes and the rename
\ is atomic. The work directory goes whatever the export did, and a failure
\ keeps its own code; a published unit then prunes its family, which reports
\ its own failures and never fails the build.
: EMIT ( -- )
   WORK-OPEN
   ['] BUILD-RUN catch EMIT-RC !
   EMIT-RC @ 0 = if ['] PUBLISH catch EMIT-RC ! then
   WORK-CLOSE
   EMIT-RC @ 0 <> if EMIT-RC @ throw then
   PREFIX$ SUFFIX$ FAMILY$ PATH-BYTES BUILD-CACHE:PRUNE ;

public

\ Export the unit unless the keyed file is already on disk, once it is granted
\ (IMAGE-GRANT:CHECK). A unit found there is marked in use (BUILD-CACHE:USED),
\ and one a pruner took meanwhile is exported again.
: ENSURE ( -- )
   FAMILY$ IMAGE-GRANT:CHECK
   RESOLVE
   PATH-BYTES FILE? if PATH-BYTES BUILD-CACHE:USED if exit then then
   EMIT ;

\ The keyed unit, for a build that imports it; nothing writes to it.
: PATH$ ( -- ptr u8 n )
   ENSURE
   PATH-BYTES ;

;package

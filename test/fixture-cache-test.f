\ fixture-cache-test.f - the gate fixtures' retention in the build cache,
\ through a real publish.
\
\ The gate's keyed images share the build cache with every other writer, and
\ lib/build-cache.f bounds them: a published image prunes its family, and an
\ image found on disk is dated as used (test/keyed-image.f, test/cold-engine.f,
\ test/whitebox-engine.f). lib/build-cache-retain-test.f holds the rules; this
\ holds their wiring. A child engine runs COLD-ENGINE:ENSURE against a private
\ cache root: it finds the fixture writer image there, dated to 2020 by
\ touch(1), and publishes a cold host beside a stale host and a stale work
\ directory of the host's family.
\
\ What wrong wiring does, and the check that sees it:
\ - a hit that does not date the image it found: the writer image is still
\   dated 2020;
\ - a publish that prunes nothing, or prunes under another family's prefix or
\   work stem: the stale host or the stale work directory is still there;
\ - a publish that leaves its work directory, or a prune its claim: the root
\   holds more entries than it should;
\ - a publish or a prune that fails: the child exits nonzero.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require lib/fs-list.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-candidate.f
require lib/time.f
require lib/build-cache.f
require test/fixture-writer.f
require test/cold-engine.f

package FIXTURE-CACHE-TEST

$4000 constant IO-CAP
120000 constant RUN-TIMEOUT-MS
128 constant NAME-CAP

create ROOT FS-PATH-CAP allot
create AT-BUF FS-PATH-CAP allot
create INNER-BUF FS-PATH-CAP allot
create WRITER-NAME NAME-CAP allot
create OUT IO-CAP allot
create ERR IO-CAP allot

variable ROOT-U
variable AT-U
variable INNER-U
variable WRITER-NAME-U
variable ENTRIES
variable T0

: ROOT$ ( -- ptr u8 n )
   ROOT ROOT-U @ ;

: WRITER$ ( -- ptr u8 n )
   WRITER-NAME WRITER-NAME-U @ ;

\ The root entry with this name.
: AT$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   ROOT$ a u AT-BUF JOIN-PATH AT-U !
   AT-BUF AT-U @ ;

: OLD-HOST$ ( -- ptr u8 n )
   s" hb-cold-aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ;

: OLD-WORK$ ( -- ptr u8 n )
   s" cold-engine-1-0" ;

\ A work directory holds what its build wrote, as a killed build's does.
: WORK+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a u AT$ MAKE-DIRS
   a u AT$ s" hb-cold" INNER-BUF JOIN-PATH INNER-U !
   INNER-BUF INNER-U @ s" x" WRITE-ALL ;

\ The child has to find the writer image in its own root, or it would build one.
: WRITER+ ( -- )
   FIXTURE-WRITER:PATH$ BASENAME {: w:ptr wu:n :}
   wu NAME-CAP > if E-FS-CAPACITY throw then
   w WRITER-NAME wu BYTE-COPY
   wu WRITER-NAME-U !
   FIXTURE-WRITER:PATH$ WRITER$ AT$ COPY-FILE-STREAM
   WRITER$ AT$ CHMOD-X ;

: ARG ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: OLD ( ptr u8 n -- )
   AT$ ARG ;

: RUN ( ptr u8 n ptr u8 n -- n ) {: path:ptr pathu:n in:ptr inu:n :}
   path pathu >LEN in inu >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN RUN-TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   rc 0 <> if 2 ERR erru LEN>N write drop then
   rc ;

\ Directories last among their contents: touching one dates only the directory.
: AGE ( -- n )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" -t" ARG
   s" 202001010000" ARG
   OLD-HOST$ OLD  OLD-WORK$ OLD  WRITER$ OLD
   s" /usr/bin/touch" s" " RUN ;

: SETUP ( -- )
   s" habu-fixture-cache" HB-TMP-MKDIR {: a:ptr u:n :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a ROOT u BYTE-COPY
   u ROOT-U !
   ROOT$ CLEANUP-TREE+
   WRITER+
   OLD-HOST$ AT$ s" x" WRITE-ALL
   OLD-WORK$ WORK+
   s" touch dates the planted entries" T-LABEL
   AGE 0 T= ;

\ Set before the inherit, so the child's root is this one whatever ours says.
: PUBLISH ( -- n )
   TIME:EPOCH-SECONDS T0 !
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN ROOT$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ S\" require test/cold-engine.f\nCOLD-ENGINE:ENSURE\n" RUN ;

: COUNT+ ( ptr u8 n -- )
   2drop 1 ENTRIES +! ;

: ENTRIES# ( -- n )
   0 ENTRIES !
   ROOT$ [: COUNT+ ;] FS-LIST:EACH
   ENTRIES @ ;

: HELD? ( ptr u8 n -- bool )
   AT$ EXISTS? ;

\ 0 for an entry that is gone, which no check below accepts as recent.
: MTIME ( ptr u8 n -- n )
   AT$ FS-TRY-STAT 0= if 0 exit then
   FS-STAT-MTIME-SEC@ ;

: CHECK ( -- )
   s" the writer image starts unused for a day" T-LABEL
   WRITER$ MTIME TIME:EPOCH-SECONDS BUILD-CACHE:RETAIN-SECONDS - < TTRUE
   s" a child publishes the cold host into the private root" T-LABEL
   PUBLISH 0 T=
   s" the hit dates the writer image it found to now" T-LABEL
   WRITER$ MTIME T0 @ 1 - >= TTRUE
   s" the host just published is there" T-LABEL
   COLD-ENGINE:PATH$ BASENAME HELD? TTRUE
   s" the publish takes a stale host of its family" T-LABEL
   OLD-HOST$ HELD? TFALSE
   s" and a stale work directory of its family" T-LABEL
   OLD-WORK$ HELD? TFALSE
   s" the root holds the writer image and the host, nothing else" T-LABEL
   ENTRIES# 2 T= ;

public

: MAIN ( -- )
   T-RESET
   CLEANUP-RESET
   SETUP
   CHECK
   CLEANUP-RUN
   T-REPORT
   s" fixture-cache-test: ok" type cr ;

;package

FIXTURE-CACHE-TEST:MAIN

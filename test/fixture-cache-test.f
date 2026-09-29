\ fixture-cache-test.f - build-cache retention, through a real publish.
\
\ test/fixture-cache.f prunes a fixture family when one of its images is
\ published, and every mistake it can make is invisible to the rows that use
\ the images: taking too much costs a rebuild on the next ENSURE, taking too
\ little lets the cache grow without bound. So this publishes a cold host for
\ real - a child engine runs COLD-ENGINE:ENSURE against a private cache root -
\ beside planted entries that touch(1) dates to 2020, and then reads the root.
\ The child's ENSURE also finds the planted writer image, and the child then
\ prunes the writer family the way a publish under another key would.
\
\ What a wrong prune does, and the check that sees it:
\ - keeps a stale entry of the family: the old hb-cold-<key> or the old
\   cold-engine-<ns>-<n> work directory is still there;
\ - takes a recent one, which a gate on another tree may be using: the recent
\   host or the recent work directory is gone;
\ - takes the image it has just published: the host is missing;
\ - takes an image a caller has just used: the writer image, dated to 2020 but
\   then found by the child's ENSURE, is gone after the writer prune;
\ - reaches past its family: an old hb-whitebox-<key>, an old
\   hb-build-out-<key>, or an old name outside the family's shape is gone;
\ - leaves what it claimed behind: the root holds more entries than it should.

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

: NEW-HOST$ ( -- ptr u8 n )
   s" hb-cold-bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" ;

: OLD-WORK$ ( -- ptr u8 n )
   s" cold-engine-1-0" ;

: NEW-WORK$ ( -- ptr u8 n )
   s" cold-engine-2-0" ;

: OLD-WRITER$ ( -- ptr u8 n )
   s" hb-fixture-writer-dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd" ;

: OLD-WB$ ( -- ptr u8 n )
   s" hb-whitebox-cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc" ;

: OLD-OUT$ ( -- ptr u8 n )
   s" hb-build-out-cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc" ;

: ODD-HOST$ ( -- ptr u8 n )
   s" hb-cold-notakey" ;

: ODD-WORK$ ( -- ptr u8 n )
   s" cold-engine-12" ;

: FILE+ ( ptr u8 n -- )
   AT$ s" x" WRITE-ALL ;

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
   OLD-HOST$ OLD  OLD-WORK$ OLD  OLD-OUT$ OLD  ODD-HOST$ OLD  ODD-WORK$ OLD
   OLD-WRITER$ OLD  OLD-WB$ OLD  WRITER$ OLD
   s" /usr/bin/touch" s" " RUN ;

: SETUP ( -- )
   s" habu-fixture-cache" HB-TMP-MKDIR {: a:ptr u:n :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a ROOT u BYTE-COPY
   u ROOT-U !
   ROOT$ CLEANUP-TREE+
   WRITER+
   OLD-HOST$ FILE+  NEW-HOST$ FILE+  OLD-OUT$ FILE+  ODD-HOST$ FILE+
   OLD-WRITER$ FILE+  OLD-WB$ FILE+
   OLD-WORK$ WORK+  NEW-WORK$ WORK+  ODD-WORK$ WORK+
   s" touch dates the planted entries" T-LABEL
   AGE 0 T= ;

\ The last line prunes the writer family as a publish of hb-fixture-writer-new
\ would, after the ENSURE above found the planted writer image.
: PROGRAM$ ( -- ptr u8 n )
   S\" require test/cold-engine.f\nCOLD-ENGINE:ENSURE\ns\" hb-fixture-writer-\" s\" fixture-writer\" s\" hb-fixture-writer-new\" FIXTURE-CACHE:PRUNE\n" ;

\ Set before the inherit, so the child's root is this one whatever ours says.
: PUBLISH ( -- n )
   TIME:EPOCH-SECONDS T0 !
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN ROOT$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ PROGRAM$ RUN ;

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
   WRITER$ MTIME TIME:EPOCH-SECONDS 86400 - < TTRUE
   s" a child publishes the cold host into the private root" T-LABEL
   PUBLISH 0 T=
   s" an image a hit refreshed survives a prune by another key" T-LABEL
   WRITER$ HELD? TTRUE
   s" a hit dates the image it finds to now" T-LABEL
   WRITER$ MTIME T0 @ 1 - >= TTRUE
   s" an unused image goes in that prune" T-LABEL
   OLD-WRITER$ HELD? TFALSE
   s" the image just published stays" T-LABEL
   COLD-ENGINE:PATH$ BASENAME HELD? TTRUE
   s" a stale host of the family goes" T-LABEL
   OLD-HOST$ HELD? TFALSE
   s" a stale work directory of the family goes" T-LABEL
   OLD-WORK$ HELD? TFALSE
   s" a recent host stays: a gate on another tree may be using it" T-LABEL
   NEW-HOST$ HELD? TTRUE
   s" a recent work directory stays: its build may be live" T-LABEL
   NEW-WORK$ HELD? TTRUE
   s" a stale whitebox image stays: another family" T-LABEL
   OLD-WB$ HELD? TTRUE
   s" a stale hb-build artifact stays: not a gate fixture" T-LABEL
   OLD-OUT$ HELD? TTRUE
   s" a stale name without a key after the prefix stays" T-LABEL
   ODD-HOST$ HELD? TTRUE
   s" a stale name without -<ns>-<n> after the prefix stays" T-LABEL
   ODD-WORK$ HELD? TTRUE
   s" nothing either prune claimed is left in the root" T-LABEL
   ENTRIES# 8 T= ;

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

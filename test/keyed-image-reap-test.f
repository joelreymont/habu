\ keyed-image-reap-test.f - a killed keyed-image build's work directory goes
\ with the next build in its cache root, and a live build's stays.
\
\ test/keyed-image.f builds in a work directory inside the cache root, so the
\ finished image is published by a rename, and only the build's own exit
\ removes that directory: a build killed by a pool deadline, a signalled gate,
\ SIGKILL or a reboot leaves it. The next build reaps what such builds left
\ (lib/build-cache.f, A WORK DIRECTORY IS HELD WHILE ITS BUILD LIVES). How a
\ reap could go wrong, and what stands against each:
\ - a live build's directory taken for dead. Through pid reuse: no pid is read;
\   a build holds its directory by a lock the kernel drops when the last
\   process holding it exits. Through a directory its owner made but has not
\   locked yet: the owner, once it holds the lock, checks that the path still
\   names what it locked and makes another if not. Through a builder child
\   still writing after its parent was killed: the child inherited the lock.
\   Through a directory a build that locks nothing made: only the locked kind
\   is reaped, and PRUNE takes the other a day after its last use. Through a
\   build under another kernel sharing the root, whose lock this kernel never
\   sees: a cache root is one kernel's, and such a root is not supported.
\ - a dead build's directory kept. Through a reboot, which kills builds before
\   their cleanup: it drops every lock too, so nothing holds the directory.
\   Through its name or its age: a reaper takes every work directory nothing
\   holds, whatever its family and however recent.
\ - two reapers at once: the lock admits one; the other finds it held, or,
\   once it is released, finds the path no longer names what it opened.
\ - a reaper against the owner's publish or cleanup: the owner holds the lock
\   from before its build until after it removed the directory.
\ - a root on another filesystem than the directory the work would move to: the
\   work directory stays in the root, beside the keyed path it is renamed to.
\ - a partial directory, from a crash before the lock, during the build or
\   during a removal: mkdir is atomic, and from then on either a live process
\   holds the lock or none does, so the next reaper takes whatever is left.
\ - a filesystem that cannot lock a directory: the build refuses it rather
\   than work unprotected, and a reaper reports such a directory and leaves it.
\ - a build of no family: nothing would stand between the stem and the tail of
\   its directory's name, which no reaper takes, so WORK-OPEN refuses it.
\
\ Four builds of this test's own families run against a private root, each a
\ child engine (test/keyed-image-reap-child.f). A is killed once its builder
\ runs, and the builder is left running; B is started and left waiting; C
\ builds to completion; A's builder is killed; D builds to completion; B is
\ let go. B's directory is dated 2020 while B holds it, and the root also holds
\ a recent work directory that nothing holds, as a reboot leaves one, whose
\ family holds digit runs and dashes like the tail that closes it, and a recent
\ one in the shape of a build that locks nothing. What a wrong reap does, and
\ the check that sees it:
\ - takes a directory a killed build's child still holds: A's is gone after C;
\ - leaves a killed build's directory: A's is still there after D;
\ - takes the live build's: it is gone, or B fails to publish;
\ - leaves a recent directory nothing holds: it is still there after C;
\ - takes the unlocked shape: it is gone;
\ - reports what it passes over (a held directory, a vanished one): a build's
\   output holds a build-cache report;
\ - leaves anything else: the root holds more than the three images and the
\   unlocked directory;
\ - makes a directory for no family: WORK-OPEN returns rather than throw
\   E-FS-PATH.

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
require lib/build-cache.f
require lib/task.f
require lib/time.f
require test/image-grant.f

package KEYED-IMAGE-REAP-TEST

60 constant READY-SECONDS
50 constant POLL-MS
120000 constant TOUCH-TIMEOUT-MS
64 constant KEY-LEN
128 constant NAME-CAP
2048 constant MARK-CAP
$10000 constant LOG-CAP
$4000 constant IO-CAP
$20 constant SPACE
$61 constant KILLED
$62 constant LIVE
$63 constant NEXT
$64 constant LAST

create TMP-BUF FS-PATH-CAP allot
create ROOT-BUF FS-PATH-CAP allot
create AT-BUF FS-PATH-CAP allot
create FILE-BUF FS-PATH-CAP allot
create KILLED-BUF FS-PATH-CAP allot
create LIVE-BUF FS-PATH-CAP allot
create NAME-BUF NAME-CAP allot
create KEY-BUF KEY-LEN allot
create MARK-BUF MARK-CAP allot
create LOG-BUF LOG-CAP allot
create OUT IO-CAP allot
create ERR IO-CAP allot

variable TMP-U
variable ROOT-U
variable AT-U
variable FILE-U
variable KILLED-U
variable LIVE-U
variable NAME-U
variable ORPHAN
variable LIVE-PID
variable ENTRIES

: TMP$ ( -- ptr u8 n )
   TMP-BUF TMP-U @ ;

: ROOT$ ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: KILLED$ ( -- ptr u8 n )
   KILLED-BUF KILLED-U @ ;

: LIVE$ ( -- ptr u8 n )
   LIVE-BUF LIVE-U @ ;

: NAME$ ( -- ptr u8 n )
   NAME-BUF NAME-U @ ;

: NAME+ ( ptr u8 n -- ) {: a:ptr u:n :}
   NAME-U @ u + NAME-CAP > if E-FS-CAPACITY throw then
   a NAME-BUF NAME-U @ + u BYTE-COPY
   NAME-U @ u + NAME-U ! ;

: NAME-C+ ( n -- ) {: c:n :}
   NAME-U @ 1+ NAME-CAP > if E-FS-CAPACITY throw then
   c NAME-BUF NAME-U @ + c!
   NAME-U @ 1+ NAME-U ! ;

\ The root entry with this name.
: AT$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   ROOT$ a u AT-BUF JOIN-PATH AT-U !
   AT-BUF AT-U @ ;

\ The scratch file <build><ext>: its mark, release and log.
: FILE$ ( n ptr u8 n -- ptr u8 n ) {: c:n ext:ptr extu:n :}
   0 NAME-U !
   c NAME-C+
   ext extu NAME+
   TMP$ NAME$ FILE-BUF JOIN-PATH FILE-U !
   FILE-BUF FILE-U @ ;

: MARK$ ( n -- ptr u8 n )
   s" .mark" FILE$ ;

: RELEASE$ ( n -- ptr u8 n )
   s" .go" FILE$ ;

: LOG$ ( n -- ptr u8 n )
   s" .log" FILE$ ;

: DRIVE$ ( -- ptr u8 n )
   TMP$ s" drive.f" FILE-BUF JOIN-PATH FILE-U !
   FILE-BUF FILE-U @ ;

\ Each build is its own family, keyed by its letter repeated.
: FAMILY$ ( n -- ptr u8 n ) {: c:n :}
   0 NAME-U !
   s" reap-" NAME+
   c NAME-C+
   NAME$ ;

: KEY$ ( n -- ptr u8 n ) {: c:n :}
   KEY-LEN 0 ?do c KEY-BUF i + c! loop
   KEY-BUF KEY-LEN ;

\ hb-reap-<c>-<key>, where the build publishes.
: IMAGE$ ( n -- ptr u8 n ) {: c:n :}
   0 NAME-U !
   s" hb-reap-" NAME+
   c NAME-C+
   s" -" NAME+
   c KEY$ NAME+
   NAME$ AT$ ;

\ Directories the builds must take or leave alone, planted beside theirs.
: UNHELD$ ( -- ptr u8 n )
   s" build-cache-work-00000000-0000-0000-0000-000000000000-reap-x-1-0" ;

: UNLOCKED$ ( -- ptr u8 n )
   s" reap-a-1-0" ;

\ A planted directory holds what a build wrote, as a killed build's does.
: PLANT ( ptr u8 n -- ) {: a:ptr u:n :}
   a u AT$ MAKE-DIRS
   a u AT$ s" hb-reap-x" FILE-BUF JOIN-PATH FILE-U !
   FILE-BUF FILE-U @ s" x" WRITE-ALL ;

: ARG ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: GRANT$ ( -- ptr u8 n )
   IMAGE-GRANT:RESET
   KILLED FAMILY$ IMAGE-GRANT:ADD
   LIVE FAMILY$ IMAGE-GRANT:ADD
   NEXT FAMILY$ IMAGE-GRANT:ADD
   LAST FAMILY$ IMAGE-GRANT:ADD
   IMAGE-GRANT:VALUE$ ;

\ One build: a child engine running test/keyed-image-reap-child.f's BUILD from
\ stdin, output and errors into its log. The root and the grant go in before
\ the inherit, so the child's are these whatever ours say.
: SPAWN ( n -- pid ) {: c:n :}
   ENGINE-CANDIDATE:PATH$ {: eng:ptr engu:n :}
   DRIVE$ FS-PATHZ open-rd {: in:n :}
   in 0 < if E-FS-OPEN throw then
   c LOG$ OPEN-APPEND-FD {: log:n :}
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN ROOT$ >LEN PROC-ENV+
   IMAGE-GRANT:NAME$ >LEN GRANT$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --" ARG
   c FAMILY$ ARG
   c KEY$ ARG
   c MARK$ ARG
   c RELEASE$ ARG
   eng engu >LEN in >FD log >FD log >FD PROC-SPAWN-ARGV-ENV-IO
   in close
   log close ;

\ Wait until the build's builder has written its mark.
: MARKED? ( n -- bool ) {: c:n :}
   TIME:EPOCH-SECONDS READY-SECONDS + {: deadline:n :}
   begin
      c MARK$ EXISTS? if 0 0= exit then
      TIME:EPOCH-SECONDS deadline > if 0 0= 0= exit then
      POLL-MS >MS TASK:SLEEP
   again ;

: SPACE-AT ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 begin dup u < if a over + c@ SPACE <> else 0 0= 0= then while 1+ repeat ;

\ The mark is "<pid> <image path>": the builder's pid, and its work directory
\ as the image path's parent, into the caller's buffer and length cell.
: MARK-READ ( n ptr u8 ptr n -- n ) {: c:n dst:ptr up:ptr :}
   c MARK$ MARK-BUF MARK-CAP READ-ALL {: u:n :}
   MARK-BUF u SPACE-AT {: sp:n :}
   sp u >= if E-STR-BOUNDS throw then
   MARK-BUF sp + 1+ u sp - 1- {: path:ptr pathu:n :}
   path pathu BASENAME nip {: baseu:n :}
   path dst pathu baseu - 1- BYTE-COPY
   pathu baseu - 1- up !
   MARK-BUF sp STR>NUMBER? MATCH option
      none OF E-STR-BOUNDS throw ENDOF
      some OF ENDOF
   ;MATCH ;

: GONE? ( n -- bool )
   0 kill-errno 0<> ;

\ Wait until a killed process no longer exists, so every lock it held is gone.
: AWAIT-GONE ( n -- bool ) {: pid:n :}
   TIME:EPOCH-SECONDS READY-SECONDS + {: deadline:n :}
   begin
      pid GONE? if 0 0= exit then
      TIME:EPOCH-SECONDS deadline > if 0 0= 0= exit then
      POLL-MS >MS TASK:SLEEP
   again ;

\ A build that never marks is killed and the run stops: its checks would
\ read a directory it never named.
: UNMARKED ( pid -- ) {: pid:pid :}
   pid SIGKILL PROC-KILL-RAW drop
   pid PROC-WAIT-STATUS drop
   s" keyed-image-reap-test: a build's builder never started" 1 die ;

: EXITED ( pid -- n )
   PROC-WAIT-RC MATCH result
      ok OF ENDOF
      err OF ENDOF
   ;MATCH ;

: REPORTED? ( n -- bool )
   LOG$ LOG-BUF LOG-CAP READ-ALL {: u:n :}
   LOG-BUF u s" build-cache: cannot" CONTAINS? ;

: COUNT+ ( ptr u8 n -- )
   2drop 1 ENTRIES +! ;

: ENTRIES# ( -- n )
   0 ENTRIES !
   ROOT$ [: COUNT+ ;] FS-LIST:EACH
   ENTRIES @ ;

\ Date the live build's directory to 2020; touch dates the directory alone, not
\ what it holds.
: AGE ( -- n )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" -t" ARG
   s" 202001010000" ARG
   LIVE$ ARG
   s" /usr/bin/touch" >LEN s" " >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN
   TOUCH-TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   rc 0 <> if 2 ERR erru LEN>N write drop then
   rc ;

: SETUP ( -- )
   s" habu-keyed-reap" HB-TMP-MKDIR {: a:ptr u:n :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a TMP-BUF u BYTE-COPY
   u TMP-U !
   TMP$ CLEANUP-TREE+
   TMP$ s" root" ROOT-BUF JOIN-PATH ROOT-U !
   ROOT$ MAKE-DIRS
   DRIVE$
   S\" require test/keyed-image-reap-child.f\nKEYED-IMAGE-REAP-CHILD:BUILD\n"
   WRITE-ALL
   NEXT RELEASE$ s" go" WRITE-ALL
   LAST RELEASE$ s" go" WRITE-ALL
   UNHELD$ PLANT
   UNLOCKED$ PLANT ;

\ Kill the build once its builder runs, so nothing of it runs its cleanup, and
\ leave the builder running with the hold it inherited.
: KILL-BUILD ( -- )
   KILLED SPAWN {: a:pid :}
   KILLED MARKED? 0= if a UNMARKED then
   KILLED KILLED-BUF KILLED-U MARK-READ ORPHAN !
   s" the killed build is killed" T-LABEL
   a SIGKILL PROC-KILL-RAW RC>N 0 T=
   a PROC-WAIT-STATUS drop
   s" and leaves its work directory" T-LABEL
   KILLED$ EXISTS? TTRUE ;

: START-LIVE ( -- )
   LIVE SPAWN {: b:pid :}
   b PID>N LIVE-PID !
   LIVE MARKED? 0= if b UNMARKED then
   LIVE LIVE-BUF LIVE-U MARK-READ drop ;

: NEXT-BUILD ( -- )
   s" touch dates the live directory" T-LABEL
   AGE 0 T=
   s" the next build, of another family, exits 0" T-LABEL
   NEXT SPAWN EXITED 0 T=
   s" and publishes its image" T-LABEL
   NEXT IMAGE$ EXECUTABLE? TTRUE
   s" the killed build's directory stays while its builder runs" T-LABEL
   KILLED$ EXISTS? TTRUE
   s" the live build's stays, though dated 2020" T-LABEL
   LIVE$ EXISTS? TTRUE
   s" a recent work directory nothing holds is gone" T-LABEL
   UNHELD$ AT$ EXISTS? TFALSE
   s" a directory no build locks stays" T-LABEL
   UNLOCKED$ AT$ EXISTS? TTRUE
   s" the next build reports nothing it passed over" T-LABEL
   NEXT REPORTED? TFALSE ;

\ The builder goes the way a gate that kills a slot's process tree takes it.
: KILL-BUILDER ( -- )
   s" the killed build's builder is killed" T-LABEL
   ORPHAN @ SIGKILL kill-errno 0 T=
   s" and is gone before the last build" T-LABEL
   ORPHAN @ AWAIT-GONE TTRUE ;

: LAST-BUILD ( -- )
   s" the last build exits 0" T-LABEL
   LAST SPAWN EXITED 0 T=
   s" the killed build's work directory is gone" T-LABEL
   KILLED$ EXISTS? TFALSE
   s" the live build's stays" T-LABEL
   LIVE$ EXISTS? TTRUE
   s" the last build reports nothing it passed over" T-LABEL
   LAST REPORTED? TFALSE ;

: FINISH-LIVE ( -- )
   LIVE RELEASE$ s" go" WRITE-ALL
   s" the live build exits 0" T-LABEL
   LIVE-PID @ >PID EXITED 0 T=
   s" and publishes its image" T-LABEL
   LIVE IMAGE$ EXECUTABLE? TTRUE
   s" and removes its own work directory" T-LABEL
   LIVE$ EXISTS? TFALSE
   s" and reports nothing it passed over" T-LABEL
   LIVE REPORTED? TFALSE
   s" the root holds the three images and the directory no build locks" T-LABEL
   ENTRIES# 4 T= ;

\ WORK-OPEN refuses before it touches the root; the root is this test's own in
\ case it does not.
: NO-FAMILY ( -- )
   s" " BUILD-CACHE:WORK-OPEN FD>N close 2drop ;

: REFUSE-NO-FAMILY ( -- )
   ROOT$ BUILD-CACHE:ROOT!
   s" a work directory of no family is refused" T-LABEL
   [: NO-FAMILY ;] E-FS-PATH TTHROWSQ ;

public

: MAIN ( -- )
   T-RESET
   CLEANUP-RESET
   SETUP
   KILL-BUILD
   START-LIVE
   NEXT-BUILD
   KILL-BUILDER
   LAST-BUILD
   FINISH-LIVE
   REFUSE-NO-FAMILY
   CLEANUP-RUN
   T-REPORT
   s" keyed-image-reap-test: ok" type cr ;

;package

KEYED-IMAGE-REAP-TEST:MAIN

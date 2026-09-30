\ build-cache-retain-test.f - build-cache retention through BUILD-CACHE:PRUNE
\ and BUILD-CACHE:USED, over private roots.
\
\ Every mistake retention can make is invisible to the builds that use the
\ cache: taking too much costs a rebuild, taking too little lets the cache grow
\ without bound, and reaching past a family deletes what another writer, or the
\ user, keeps in the root. So this plants every shape of name a family owns,
\ dated to 2020 by touch(1) or left recent, beside names that only look like
\ one, has a child engine prune the family with its stderr kept, and reads the
\ root. The family is hb-rt-<key>.img with work directories rt-work-<n>-<n>:
\ a prefix and a suffix around the key, as hb-build's object cache has.
\
\ What a wrong prune does, and the check that sees it:
\ - keeps a stale entry, an atomic write's leftover, a work directory or a
\   killed pruner's claim: that name is still there;
\ - takes a recent one, which a build on another tree may be using, or another
\   pruner's live claim: that name is gone;
\ - takes the entry just published, or one USED dated after 2020: it is gone;
\ - reaches past the family or its shapes: one of the stale look-alikes is gone;
\ - deletes what is not ours: the read-only work directory it could not claim,
\   or the read-only file inside the one it claimed, is gone, or its report
\   is missing;
\ - fails its caller: the child exits nonzero;
\ - leaves its claim behind: the root holds more entries than it should.
\
\ Two child engines then publish and prune one family at once over a root of
\ stale entries, starting their sweeps together, so each finds entries the
\ other claimed between its listing and its rename, and publishes the other
\ must leave alone. A pruner that reports a vanished entry, takes a live claim
\ or a fresh publish, or leaves a stale entry fails the race.

require lib/errors.f
require lib/string.f
require lib/fmt.f
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

package BUILD-CACHE-RETAIN-TEST

$4000 constant IO-CAP
120000 constant RUN-TIMEOUT-MS
60 constant READY-SECONDS
300 constant RACE-STALE
16 constant RACE-PUBLISHES
120 constant TOUCH-BATCH
$1ED constant MODE-0755
$16D constant MODE-0555
$65 constant CH-E
256 constant NAME-CAP
2048 constant LINE-CAP

create TMP-BUF FS-PATH-CAP allot
create ROOT-BUF FS-PATH-CAP allot
create AT-BUF FS-PATH-CAP allot
create INNER-BUF FS-PATH-CAP allot
create PROG-BUF FS-PATH-CAP allot
create ERR-PATH-BUF FS-PATH-CAP allot
create NAME-BUF NAME-CAP allot
create LINE-BUF LINE-CAP allot
create OUT IO-CAP allot
create ERR IO-CAP allot

variable TMP-U
variable ROOT-U
variable AT-U
variable INNER-U
variable PROG-U
variable ERR-PATH-U
variable NAME-U
variable LINE-U
variable ERR-U
variable ENTRIES
variable TOUCHES
variable T0

: TMP$ ( -- ptr u8 n )
   TMP-BUF TMP-U @ ;

: ROOT$ ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: PROG$ ( -- ptr u8 n )
   PROG-BUF PROG-U @ ;

: ERR-PATH$ ( -- ptr u8 n )
   ERR-PATH-BUF ERR-PATH-U @ ;

: ERR$ ( -- ptr u8 n )
   ERR ERR-U @ ;

\ A path under the scratch directory.
: UNDER! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   TMP$ a u dst JOIN-PATH up ! ;

\ The root entry with this name.
: AT$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   ROOT$ a u AT-BUF JOIN-PATH AT-U !
   AT-BUF AT-U @ ;

\ ---- names --------------------------------------------------------------------

: NAME-RESET ( -- )
   0 NAME-U ! ;

: NAME+ ( ptr u8 n -- ) {: a:ptr u:n :}
   NAME-U @ u + NAME-CAP > if E-STR-CAPACITY throw then
   a NAME-BUF NAME-U @ + u BYTE-COPY
   NAME-U @ u + NAME-U ! ;

: NAME-C+ ( n -- ) {: c:n :}
   NAME-U @ NAME-CAP >= if E-STR-CAPACITY throw then
   c NAME-BUF NAME-U @ + c!
   NAME-U @ 1+ NAME-U ! ;

: NAME$ ( -- ptr u8 n )
   NAME-BUF NAME-U @ ;

\ <prefix><count copies of c><suffix>.
: REPEAT$ ( ptr u8 n n n ptr u8 n -- ptr u8 n )
   {: pre:ptr preu:n c:n count:n suf:ptr sufu:n :}
   NAME-RESET
   pre preu NAME+
   count 0 ?do c NAME-C+ loop
   suf sufu NAME+
   NAME$ ;

\ hb-rt-<64 copies of c>.img, then a tail.
: KEYED$ ( n ptr u8 n -- ptr u8 n ) {: c:n tail:ptr tailu:n :}
   s" hb-rt-" c 64 s" .img" REPEAT$ 2drop
   tail tailu NAME+
   NAME$ ;

\ Four decimal digits of n.
: DIGITS+ ( n -- ) {: n:n :}
   n 1000 / 10 mod $30 + NAME-C+
   n 100 / 10 mod $30 + NAME-C+
   n 10 / 10 mod $30 + NAME-C+
   n 10 mod $30 + NAME-C+ ;

\ hb-rt-<60 copies of c><four digits of n>.img: one of many distinct keys.
: NTH$ ( n n -- ptr u8 n ) {: c:n n:n :}
   s" hb-rt-" c 60 s" " REPEAT$ 2drop
   n DIGITS+
   s" .img" NAME+
   NAME$ ;

\ ---- the planted root ------------------------------------------------------------

: FILE+ ( ptr u8 n -- )
   AT$ s" x" WRITE-ALL ;

\ A directory holding what a build or a sweep wrote.
: DIR+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a u AT$ MAKE-DIRS
   a u AT$ s" f" INNER-BUF JOIN-PATH INNER-U !
   INNER-BUF INNER-U @ s" x" WRITE-ALL ;

: ARG ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: CAPTURE-RUN ( ptr u8 n ptr u8 n -- n ) {: path:ptr pathu:n in:ptr inu:n :}
   path pathu >LEN in inu >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN RUN-TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   erru LEN>N ERR-U !
   rc ;

: TOUCH-BEGIN ( -- )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" -t" ARG
   s" 202001010000" ARG
   0 TOUCHES ! ;

: TOUCH-RUN ( -- )
   TOUCHES @ 0= if exit then
   s" touch dates the planted entries" T-LABEL
   s" /usr/bin/touch" s" " CAPTURE-RUN 0 T= ;

\ Date a root entry to 2020. Directories go after their contents: touching one
\ dates only the directory. touch(1) takes TOUCH-BATCH paths per run.
: OLD ( ptr u8 n -- )
   AT$ ARG
   1 TOUCHES +!
   TOUCHES @ TOUCH-BATCH = if TOUCH-RUN TOUCH-BEGIN then ;

: HELD? ( ptr u8 n -- bool )
   AT$ FS-TRY-LSTAT ;

\ 0 for an entry that is gone, which no check below accepts as recent.
: MTIME ( ptr u8 n -- n )
   AT$ FS-TRY-LSTAT 0= if 0 exit then
   FS-STAT-MTIME-SEC@ ;

: COUNT+ ( ptr u8 n -- )
   2drop 1 ENTRIES +! ;

: ENTRIES# ( -- n )
   0 ENTRIES !
   ROOT$ [: COUNT+ ;] FS-LIST:EACH
   ENTRIES @ ;

\ ---- one prune -----------------------------------------------------------------

: LINE-RESET ( -- )
   0 LINE-U ! ;

: LINE+ ( ptr u8 n -- ) {: a:ptr u:n :}
   LINE-U @ u + LINE-CAP > if E-STR-CAPACITY throw then
   a LINE-BUF LINE-U @ + u BYTE-COPY
   LINE-U @ u + LINE-U ! ;

: LINE$ ( -- ptr u8 n )
   LINE-BUF LINE-U @ ;

: KEEP-NAME$ ( -- ptr u8 n )
   $34 s" " KEYED$ ;

\ The family's prune as a publish of the keep entry runs it, in a child whose
\ cache root is this one.
: PRUNE-PROGRAM$ ( -- ptr u8 n )
   LINE-RESET
   s\" require lib/build-cache.f\ncreate P 1024 allot\n" LINE+
   s\" : KEEP$ ( -- ptr u8 n ) BUILD-CACHE:ROOT$ s\" " LINE+
   KEEP-NAME$ LINE+
   s\" \" P JOIN-PATH {: u:n :} P u ;\n" LINE+
   s\" s\" hb-rt-\" s\" .img\" s\" rt-work\" KEEP$ BUILD-CACHE:PRUNE\n" LINE+
   LINE$ ;

\ Set before the inherit, so the child's root is this one whatever ours says.
: PRUNE-RUN ( -- n )
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN ROOT$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ PRUNE-PROGRAM$ CAPTURE-RUN ;

\ The family: what goes when stale, and what stays although it is.
: PLANT-FAMILY ( -- )
   $31 s" " KEYED$ FILE+
   $32 s" " KEYED$ FILE+
   $33 s" .tmp-12-0" KEYED$ FILE+
   KEEP-NAME$ FILE+
   $35 s" " KEYED$ FILE+
   s" rt-work-5-0" DIR+
   s" rt-work-6-0" DIR+
   s" build-cache-claim-7-0" DIR+
   s" build-cache-claim-8-0" DIR+ ;

\ Names that only look like the family's. Another family's prefix is as long
\ as hb-rt-, so a sweep that skips the prefix and reads a key after it takes it.
: PLANT-LOOKALIKES ( -- )
   s" hb-rt-notakey.img" FILE+
   s" hb-rt-" $36 63 s" .img" REPEAT$ FILE+
   s" hb-rt-" $66 63 s" g.img" REPEAT$ FILE+
   s" hb-rt-" $61 64 s" .idx" REPEAT$ FILE+
   $37 s" x" KEYED$ FILE+
   $38 s" .tmp" KEYED$ FILE+
   $39 s" .bak" KEYED$ FILE+
   s" hb-ot-" $31 64 s" .img" REPEAT$ FILE+
   s" " $31 64 s" .img" REPEAT$ FILE+
   s" rt-work-12" DIR+
   s" rt-work-5-0x" DIR+
   s" build-cache-claim-1" DIR+
   s" notes.txt" FILE+ ;

\ What the prune may not delete: a work directory it cannot claim (renaming a
\ directory rewrites its .. entry, which needs write permission on it), and one
\ it claims but cannot empty.
: PLANT-NOT-OURS ( -- )
   s" rt-work-9-0" DIR+
   s" rt-work-10-0/ro" DIR+ ;

: AGE-ALL ( -- )
   TOUCH-BEGIN
   $31 s" " KEYED$ OLD
   $33 s" .tmp-12-0" KEYED$ OLD
   KEEP-NAME$ OLD
   $35 s" " KEYED$ OLD
   s" rt-work-5-0" OLD
   s" build-cache-claim-7-0" OLD
   s" hb-rt-notakey.img" OLD
   s" hb-rt-" $36 63 s" .img" REPEAT$ OLD
   s" hb-rt-" $66 63 s" g.img" REPEAT$ OLD
   s" hb-rt-" $61 64 s" .idx" REPEAT$ OLD
   $37 s" x" KEYED$ OLD
   $38 s" .tmp" KEYED$ OLD
   $39 s" .bak" KEYED$ OLD
   s" hb-ot-" $31 64 s" .img" REPEAT$ OLD
   s" " $31 64 s" .img" REPEAT$ OLD
   s" rt-work-12" OLD
   s" rt-work-5-0x" OLD
   s" build-cache-claim-1" OLD
   s" notes.txt" OLD
   s" rt-work-9-0" OLD
   s" rt-work-10-0/ro" OLD
   s" rt-work-10-0" OLD
   TOUCH-RUN ;

: LOCK-NOT-OURS ( -- )
   s" rt-work-9-0" AT$ MODE-0555 CHMOD-MODE
   s" rt-work-10-0/ro" AT$ MODE-0555 CHMOD-MODE ;

: UNLOCK-NOT-OURS ( -- )
   s" rt-work-9-0" AT$ 2dup EXISTS? if MODE-0755 CHMOD-MODE else 2drop then
   s" rt-work-10-0/ro" AT$ 2dup EXISTS? if MODE-0755 CHMOD-MODE else 2drop then ;

\ The prune's exit code, with the two directories read-only only while the child
\ prunes. `finally` opens them again when the run returns or throws; nothing in
\ it dies (a die skips `finally`), and every check runs after it, so a check
\ that dies still leaves a tree the exit registry can remove. A kill of this
\ process during the run leaves them read-only.
: LOCKED-PRUNE ( -- n )
   [: LOCK-NOT-OURS PRUNE-RUN ;] [: UNLOCK-NOT-OURS ;] finally ;

\ The line a report of this verb and root entry must be.
: REPORT$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: v:ptr vu:n a:ptr u:n :}
   a u AT$ {: p:ptr pu:n :}
   SB-RESET
   s" build-cache: cannot " SB-APPEND
   v vu SB-APPEND
   s"  " SB-APPEND
   p pu SB-APPEND
   s" : " SB-APPEND
   E-FS-IO FMT:SB-INT
   S\" \n" SB-APPEND
   SB$ ;

: LINES ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 u 0 ?do a i + c@ $0A = if 1+ then loop ;

: USED-CHECKS ( -- )
   s" an entry starts unused for a day" T-LABEL
   $35 s" " KEYED$ MTIME TIME:EPOCH-SECONDS BUILD-CACHE:RETAIN-SECONDS - < TTRUE
   TIME:EPOCH-SECONDS T0 !
   s" USED keeps a hit it finds" T-LABEL
   $35 s" " KEYED$ AT$ BUILD-CACHE:USED TTRUE
   s" USED dates that entry to now" T-LABEL
   $35 s" " KEYED$ MTIME T0 @ 1 - >= TTRUE
   s" USED answers FALSE for an entry that is gone" T-LABEL
   s" hb-rt-gone.img" AT$ BUILD-CACHE:USED TFALSE ;

: PRUNE-CHECKS ( -- )
   s" the prune reports and returns normally" T-LABEL
   LOCKED-PRUNE 0 T=
   s" a stale entry of the family goes" T-LABEL
   $31 s" " KEYED$ HELD? TFALSE
   s" a stale atomic write's leftover goes" T-LABEL
   $33 s" .tmp-12-0" KEYED$ HELD? TFALSE
   s" a stale work directory goes" T-LABEL
   s" rt-work-5-0" HELD? TFALSE
   s" a stale claim a killed pruner left goes" T-LABEL
   s" build-cache-claim-7-0" HELD? TFALSE
   s" a recent entry stays: a build on another tree may be using it" T-LABEL
   $32 s" " KEYED$ HELD? TTRUE
   s" a recent work directory stays: its build may be live" T-LABEL
   s" rt-work-6-0" HELD? TTRUE
   s" a recent claim stays: another pruner is sweeping" T-LABEL
   s" build-cache-claim-8-0" HELD? TTRUE
   s" the entry just published stays" T-LABEL
   KEEP-NAME$ HELD? TTRUE
   s" an entry USED dated stays" T-LABEL
   $35 s" " KEYED$ HELD? TTRUE ;

: LOOKALIKE-CHECKS ( -- )
   s" a stale name without a key stays" T-LABEL
   s" hb-rt-notakey.img" HELD? TTRUE
   s" a stale name with a short key stays" T-LABEL
   s" hb-rt-" $36 63 s" .img" REPEAT$ HELD? TTRUE
   s" a stale name whose last key digit is not hex stays" T-LABEL
   s" hb-rt-" $66 63 s" g.img" REPEAT$ HELD? TTRUE
   s" a stale key with another suffix stays: another family" T-LABEL
   s" hb-rt-" $61 64 s" .idx" REPEAT$ HELD? TTRUE
   s" a stale name with more after the suffix stays" T-LABEL
   $37 s" x" KEYED$ HELD? TTRUE
   s" a stale .tmp without -<n>-<n> stays" T-LABEL
   $38 s" .tmp" KEYED$ HELD? TTRUE
   s" a stale name with another tail stays" T-LABEL
   $39 s" .bak" KEYED$ HELD? TTRUE
   s" a stale entry of another family stays" T-LABEL
   s" hb-ot-" $31 64 s" .img" REPEAT$ HELD? TTRUE
   s" a stale key without the prefix stays" T-LABEL
   s" " $31 64 s" .img" REPEAT$ HELD? TTRUE
   s" a stale work name without -<n>-<n> stays" T-LABEL
   s" rt-work-12" HELD? TTRUE
   s" a stale work name with more after it stays" T-LABEL
   s" rt-work-5-0x" HELD? TTRUE
   s" a stale claim name without -<n>-<n> stays" T-LABEL
   s" build-cache-claim-1" HELD? TTRUE
   s" a stale name of no family stays" T-LABEL
   s" notes.txt" HELD? TTRUE ;

: NOT-OURS-CHECKS ( -- )
   s" a work directory the prune cannot claim stays" T-LABEL
   s" rt-work-9-0/f" HELD? TTRUE
   s" what it claimed but could not remove goes back, never deleted" T-LABEL
   s" rt-work-10-0/ro/f" HELD? TTRUE
   s" the refused claim is reported" T-LABEL
   ERR$ s" prune" s" rt-work-9-0" REPORT$ CONTAINS? TTRUE
   s" the refused removal is reported" T-LABEL
   ERR$ s" remove" s" rt-work-10-0" REPORT$ CONTAINS? TTRUE
   s" nothing else is reported" T-LABEL
   ERR$ LINES 2 T=
   s" nothing the prune claimed is left in the root" T-LABEL
   ENTRIES# 20 T= ;

: FAMILY-PHASE ( -- )
   s" cache" ROOT-BUF ROOT-U UNDER!
   ROOT$ MAKE-DIRS
   PLANT-FAMILY
   PLANT-LOOKALIKES
   PLANT-NOT-OURS
   AGE-ALL
   USED-CHECKS
   PRUNE-CHECKS
   LOOKALIKE-CHECKS
   NOT-OURS-CHECKS ;

\ ---- the race -----------------------------------------------------------------

: RACE-KEY$ ( n -- ptr u8 n ) {: n:n :}
   CH-E n NTH$ ;

\ Each child publishes its own keys, pruning after every publish as a writer
\ does, once both are ready to start.
: PUBLISH-KEY$ ( n n -- ptr u8 n ) {: c:n j:n :}
   c j NTH$ ;

: PROG-LINE ( ptr u8 n -- )
   PROG$ 2swap APPEND-FILE ;

: MARK$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a u INNER-BUF INNER-U UNDER!
   INNER-BUF INNER-U @ ;

: READY$ ( n -- ptr u8 n ) {: c:n :}
   NAME-RESET
   s" ready-" NAME+
   c NAME-C+
   NAME$ MARK$ ;

\ The child marks itself ready and spins until the go mark appears.
: WAIT-LINE ( n -- ) {: c:n :}
   LINE-RESET
   s\" : GO ( -- ) s\" " LINE+
   c READY$ LINE+
   s\" \" s\" x\" WRITE-ALL begin s\" " LINE+
   s" go" MARK$ LINE+
   s\" \" EXISTS? until ;\nGO\n" LINE+
   LINE$ PROG-LINE ;

\ race-<c>.f, or with another extension.
: RACE-FILE! ( n ptr u8 n ptr u8 ptr n -- ) {: c:n ext:ptr extu:n dst:ptr up:ptr :}
   NAME-RESET
   s" race-" NAME+
   c NAME-C+
   ext extu NAME+
   NAME$ dst up UNDER! ;

: RACE-PROGRAM ( n -- ) {: c:n :}
   c s" .f" PROG-BUF PROG-U RACE-FILE!
   PROG$ 2dup EXISTS? if REMOVE-FILE else 2drop then
   s\" require lib/build-cache.f\ncreate P 1024 allot\n" PROG-LINE
   s\" : ONE ( ptr u8 n -- ) {: a:ptr u:n :} BUILD-CACHE:ROOT$ a u P JOIN-PATH {: pu:n :}\n" PROG-LINE
   s\"    P pu s\" x\" ATOMIC-WRITE-FILE\n" PROG-LINE
   s\"    s\" hb-rt-\" s\" .img\" s\" rt-work\" P pu BUILD-CACHE:PRUNE ;\n" PROG-LINE
   c WAIT-LINE
   RACE-PUBLISHES 0 ?do
      LINE-RESET
      s\" s\" " LINE+
      c i PUBLISH-KEY$ LINE+
      s\" \" ONE\n" LINE+
      LINE$ PROG-LINE
   loop ;

: OPEN-NULL ( -- n )
   s" /dev/null" FS-PATHZ open-rd {: fd:n :}
   fd 0 < if E-FS-OPEN throw then
   fd ;

: ERR-FILE! ( n -- )
   s" .err" ERR-PATH-BUF ERR-PATH-U RACE-FILE! ;

\ One child engine loading its race program, stderr into race-<c>.err.
: SPAWN ( n -- pid ) {: c:n :}
   c RACE-PROGRAM
   c ERR-FILE!
   ERR-PATH$ OPEN-APPEND-FD {: errfd:n :}
   OPEN-NULL {: nullfd:n :}
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN ROOT$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG
   PROG$ ARG
   ENGINE-CANDIDATE:PATH$ >LEN nullfd >FD -1 >FD errfd >FD PROC-SPAWN-ARGV-ENV-IO
   nullfd close
   errfd close ;

: READY? ( n -- bool )
   READY$ EXISTS? ;

\ Start both sweeps together. A child that dies before it is ready fails its
\ exit code below, so the wait gives up after READY-SECONDS rather than hang.
: GO ( -- )
   TIME:EPOCH-SECONDS READY-SECONDS + {: deadline:n :}
   begin
      $61 READY? $62 READY? and
      TIME:EPOCH-SECONDS deadline > or
   until
   s" go" MARK$ s" x" WRITE-ALL ;

: EXITED-OK ( pid -- )
   PROC-WAIT-RC MATCH result
      ok OF 0 T= ENDOF
      err OF 0 T= ENDOF
   ;MATCH ;

: ERR-SIZE ( n -- n )
   ERR-FILE! ERR-PATH$ FILE-SIZE ;

: PLANT-RACE ( -- )
   TOUCH-BEGIN
   RACE-STALE 0 ?do
      i RACE-KEY$ FILE+
      i RACE-KEY$ OLD
   loop
   TOUCH-RUN ;

: RACE-PUBLISHED# ( n -- n ) {: c:n :}
   0 RACE-PUBLISHES 0 ?do c i PUBLISH-KEY$ HELD? if 1+ then loop ;

: RACE-STALE# ( -- n )
   0 RACE-STALE 0 ?do i RACE-KEY$ HELD? if 1+ then loop ;

: RACE-PHASE ( -- )
   s" race" ROOT-BUF ROOT-U UNDER!
   ROOT$ MAKE-DIRS
   PLANT-RACE
   $61 SPAWN {: a:pid :}
   $62 SPAWN {: b:pid :}
   GO
   s" the first racing publisher exits 0" T-LABEL
   a EXITED-OK
   s" the second racing publisher exits 0" T-LABEL
   b EXITED-OK
   s" the first reports nothing: an entry the other claimed is passed over" T-LABEL
   $61 ERR-SIZE 0 T=
   s" the second reports nothing" T-LABEL
   $62 ERR-SIZE 0 T=
   s" every stale entry goes" T-LABEL
   RACE-STALE# 0 T=
   s" every publish of the first stays" T-LABEL
   $61 RACE-PUBLISHED# RACE-PUBLISHES T=
   s" every publish of the second stays" T-LABEL
   $62 RACE-PUBLISHED# RACE-PUBLISHES T=
   s" no claim or leftover remains" T-LABEL
   ENTRIES# RACE-PUBLISHES 2 * T= ;

public

: MAIN ( -- )
   T-RESET
   CLEANUP-RESET
   s" habu-build-cache-retain" HB-TMP-MKDIR {: a:ptr u:n :}
   a TMP-BUF u BYTE-COPY
   u TMP-U !
   TMP$ CLEANUP-TREE+
   FAMILY-PHASE
   RACE-PHASE
   CLEANUP-RUN
   T-REPORT
   s" build-cache-retain-test: ok" type cr ;

;package

BUILD-CACHE-RETAIN-TEST:MAIN

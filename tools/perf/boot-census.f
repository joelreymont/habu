\ boot-census.f - follow a command's process tree and count the fresh images it
\ ran and the forks that kept their parent's image.
\
\   bin/hb --load tools/perf/boot-census.f -- LOGFILE CMD [ARGS...]
\
\ runs CMD, logs every process of its tree to LOGFILE, prints the census to
\ stdout and exits with CMD's exit status, 1 when a signal ended it. macOS only:
\ it follows the tree with kqueue's EVFILT_PROC and reads images with libproc.
\
\ FOLLOWING THE TREE. Each process is registered for NOTE_FORK, NOTE_EXEC and
\ NOTE_EXIT (macOS has no NOTE_TRACK). A fork event lists the forker's children
\ with proc_listpids and registers each new one, and a process is listed when it
\ is registered too, since it may have forked before. A sweep lists the children
\ of every live process after 200 ms without events and at least every 500 ms
\ while they flow, for a child whose fork event came before its parent was
\ registered. After the root exits the census waits for the rest of the tree,
\ at most DRAIN-ROUNDS quiet ticks. A process that ends before it is listed is
\ not seen at all, one listed after it ended cannot be registered (LOST), and
\ an image read spanning exec is logged incomplete and retried on the next
\ event or sweep. The exec version prevents a queued NOTE_EXEC from counting
\ the image already observed during registration twice.
\
\ FORK OR EXEC. A process's image is its path (proc_pidpath) and its argv
\ (KERN_PROCARGS2, joined with single spaces, at most ARGV-CAP bytes), read
\ when it is registered and again on each exec event, bracketed by libproc's
\ p_idversion, which changes even on same-path/argv exec. A child whose first
\ image is its parent's latest image is inherited; it counts as a fork only
\ when it exits without an observed exec. Every other child, the root and each
\ newly observed exec version is a fresh image.
\
\ THE LOG, one line per event, t in milliseconds since the start:
\   t ROOT|CHILD pid ppid path | argv
\   t EXEC pid 0 path | argv
\   t IMAGE pid ppid path | argv   a deferred image read recovered at a sweep
\   t INCOMPLETE pid ppid - | -    exited before its image could be classified
\   t FORK pid 0 - | -
\   t EXIT pid 0 - | -              kqueue reports an exit status only to
\                                   the parent; DONE carries the root's
\   t LOST pid ppid - | -           a registration kqueue refused
\   t DONE alive=A lost=L status=S  S is the root's raw wait status
\
\ THE CENSUS: fresh images by image bucket, fork children by image basename,
\ refused registrations, incomplete image reads, and Habu argv shapes. Exec
\ notifications may coalesce: only observed image generations can be counted.
\ ABI sources: xnu bsd/sys/proc_info_private.h (PROC_PIDUNIQIDENTIFIERINFO,
\ 56 bytes, p_idversion at byte 32) and xnu tests/exec-race-128791723.c, which
\ checks that the version changes across exec.
require src/lib/errors.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/le.f
require src/lib/ffi-abi.f
require src/lib/fs.f
require src/lib/fd.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f

package BOOT-CENSUS
private

64 constant RC-USAGE               \ sysexits EX_USAGE, as ARGV:E-USAGE
70 constant RC-HOST                \ sysexits EX_SOFTWARE: not macOS
74 constant RC-IO                  \ sysexits EX_IOERR: the log cannot be opened
100000 constant PID-CAP            \ xnu PID_MAX is 99999; a larger pid is E-PROC-OUTPUT
4096 constant PATH-CAP-BYTES       \ PROC_PIDPATHINFO_MAXSIZE, sys/proc_info.h
512 constant ARGV-CAP              \ argv bytes kept per image
65536 constant RAW-CAP             \ the KERN_PROCARGS2 answer buffer
8192 constant LINE-CAP             \ one log line or key: a path, an argv and their framing
1 constant CTL-KERN                \ sys/sysctl.h
49 constant KERN-PROCARGS2         \ sys/sysctl.h
6 constant PROC-PPID-ONLY          \ proc_listpids: children of a pid (src/lib/process-tree.f)
17 constant PROC-PIDUNIQINFO       \ proc_pidinfo: proc_uniqidentifierinfo, sys/proc_info_private.h
56 constant UNIQINFO-BYTES         \ API struct size; p_idversion is u32 at byte 32
4 constant PID-BYTES               \ pid_t
4096 constant LIST-MAX             \ children of one process listed at once; a full list is E-PROC-TRUNCATED
LIST-MAX PID-BYTES * constant LIST-BYTES
32 constant KEVENT-BYTES           \ struct kevent: ident u64 @0, filter i16 @8, flags u16 @10, fflags u32 @12, data i64 @16, udata @24
$FFFB constant EVFILT-PROC         \ -5 as u16, sys/event.h
5 constant EV-ADD-ENABLE           \ EV_ADD | EV_ENABLE, sys/event.h
$40000000 constant NOTE-FORK       \ sys/event.h
$20000000 constant NOTE-EXEC       \ sys/event.h
$80000000 constant NOTE-EXIT       \ sys/event.h
NOTE-FORK NOTE-EXEC or NOTE-EXIT or constant NOTE-ALL
4 constant EINTR                   \ sys/errno.h
200000000 constant TICK-NS         \ the 200 ms sweep cadence
5000 constant SWEEP-TENTHS         \ a sweep at least every 500 ms while events flow
50 constant DRAIN-ROUNDS           \ quiet ticks to wait for stragglers after the root exited

11 constant REC-CELLS              \ a watched process:
0 constant R-PID
1 constant R-ALIVE                 \ 1 until its exit event
2 constant R-PATH                  \ TEXT offset and length of its latest image path
3 constant R-PATH-U
4 constant R-ARGV                  \ and of its argv
5 constant R-ARGV-U
6 constant R-VERSION               \ exec version, 0 until a coherent image was read
7 constant R-INHERITED             \ first image matched parent; cleared on exec
8 constant R-PPID
9 constant R-DIRTY                 \ image read needs retry at next event or sweep
10 constant R-EXEC                 \ an exec event was observed, even if its image was unavailable
4 constant KEY-CELLS               \ a census row:
0 constant K-OFF                   \ TEXT offset and length of its key
1 constant K-U
2 constant K-COUNT
3 constant K-DONE                  \ 1 once printed
1024 constant KEY-CAP              \ rows of one table; more is E-PROC-TRUNCATED

create CHANGE KEVENT-BYTES allot   \ a registration
create EVENT KEVENT-BYTES allot    \ the event a wait answers
create TICK 16 allot               \ struct timespec: the wait's 200 ms
0 TICK LE:U64!  TICK-NS TICK 8 + LE:U64!
create MIB 12 allot                \ { CTL_KERN, KERN_PROCARGS2, pid }
create RAW RAW-CAP allot
create UNIQINFO UNIQINFO-BYTES allot
variable IMAGE-VERSION
variable RAW-U                     \ sysctl's size_t, in and out
variable CUR                       \ the argv reader's offset in RAW
create PATH-BUF PATH-CAP-BYTES allot
TYPED-VARIABLE PATH-U len
create ARGS ARGV-CAP allot
TYPED-VARIABLE ARGS-U len
create LIST LIST-BYTES allot
create LINE LINE-CAP allot
TYPED-VARIABLE LINE-U len
create BUCKET-BUF LINE-CAP allot
TYPED-VARIABLE BUCKET-U len
create SHAPE-BUF LINE-CAP allot
TYPED-VARIABLE SHAPE-U len
create EXECS KEY-CAP KEY-CELLS * cells allot   \ fresh images by bucket
variable EXECS-N
create FORKS KEY-CAP KEY-CELLS * cells allot   \ fork children by basename
variable FORKS-N
create SHAPES KEY-CAP KEY-CELLS * cells allot  \ engine images by argv shape
variable SHAPES-N
DYNAMIC-BUFFER SLOT n              \ pid -> 1 + the record of its latest image, 0 when never watched
DYNAMIC-BUFFER RECS n              \ REC-CELLS per watched process
variable REC-N
DYNAMIC-BUFFER TEXT u8             \ every path, argv and key the records and rows name
variable TEXT-U
DYNAMIC-BUFFER PENDING n           \ pids whose children are still to be listed
variable PENDING-N
variable KQ
variable T0
variable ROOT
variable ALIVE
variable LOST
variable EXEC-TOTAL
variable FORK-TOTAL
variable INCOMPLETE
variable ROOT-EXITED
variable ROOT-STATUS
variable LAST-SWEEP
variable DRAINED                   \ quiet ticks since the root exited
variable DONE
variable LOG-FD
variable BEST                      \ the row TOP has found so far
variable BEST-COUNT

PROCESS-SYMBOLS

FUNCTION: KQUEUE kqueue ( -- i32 ) ;FUNCTION

FUNCTION: KEVENT kevent ( n ptr u8 n ptr u8 n ptr u8 -- i32 )
   3 KEVENT-BYTES WRITES-BYTES
;FUNCTION

FUNCTION: PID-PATH proc_pidpath ( n ptr u8 n -- i32 )
   1 2 WRITES-ARG
;FUNCTION

FUNCTION: PID-INFO proc_pidinfo ( n n n ptr u8 n -- i32 )
   3 UNIQINFO-BYTES WRITES-BYTES
;FUNCTION

FUNCTION: LIST-PIDS proc_listpids ( n n ptr u8 n -- i32 )
   2 3 WRITES-ARG
;FUNCTION

FUNCTION: SYSCTL sysctl ( ptr u8 n ptr u8 ptr u8 ptr u8 n -- i32 )
   2 RAW-CAP WRITES-BYTES
   3 8 WRITES-BYTES
;FUNCTION

FUNCTION: ERRNO-CELL __error ( -- ptr u8 ) ;FUNCTION

: ERRNO-CLEAR ( -- )
   0 ERRNO-CELL LE:U32! ;

: ERRNO@ ( -- n )
   ERRNO-CELL LE:U32@ ;

\ ---- the image of a process ---------------------------------------------------

: PATH$ ( -- ptr u8 n )   PATH-BUF PATH-U @ LEN>N ;
: ARGS$ ( -- ptr u8 n )   ARGS ARGS-U @ LEN>N ;

: NUM$ ( n -- ptr u8 n )
   SB-RESET FMT:SB-U SB$ ;

: PATH+ ( ptr u8 n -- )   PATH-BUF PATH-CAP-BYTES PATH-U BUF-APPEND ;
: ARGS+ ( ptr u8 n -- )   ARGS ARGV-CAP ARGS-U BUF-APPEND ;
: ARGS-C+ ( n -- )   ARGS ARGV-CAP ARGS-U BUF-APPEND-C ;

\ The image path of pid, or ?path-errno=E when libproc refuses it.
: PATH-READ ( n -- bool )
   {: pid:n :}
   pid PATH-BUF PATH-CAP-BYTES PID-PATH {: u:n :}
   u 0 > if u >LEN PATH-U ! true exit then
   ERRNO@ {: e:n :}
   PATH-U BUF-RESET
   s" ?path-errno=" PATH+  e NUM$ PATH+ false ;

: MORE? ( -- bool )   CUR @ RAW-U @ < ;
: BYTE ( -- n )   RAW CUR @ + c@ ;
: ARGS-ROOM? ( -- bool )   ARGS-U @ LEN>N ARGV-CAP < ;

\ The exec path at CUR, then the NULs that pad it.
: SKIP-EXEC-PATH ( -- )
   begin MORE? if BYTE 0<> else false then while 1 CUR +! repeat
   begin MORE? if BYTE 0= else false then while 1 CUR +! repeat ;

\ The argument at CUR, as much of it as fits, and CUR past its NUL.
: ARG+ ( -- )
   begin MORE? ARGS-ROOM? and if BYTE 0<> else false then while
      BYTE ARGS-C+
      1 CUR +!
   repeat
   1 CUR +! ;

\ KERN_PROCARGS2 answers argc, the exec path, its NUL padding, then the argv
\ strings, each ending in a NUL. The argv of pid, joined with single spaces and
\ cut at ARGV-CAP bytes, or ?argv-errno=E when sysctl refuses it.
: ARGS-READ ( n -- bool )
   {: pid:n :}
   ARGS-U BUF-RESET
   CTL-KERN MIB LE:U32!
   KERN-PROCARGS2 MIB 4 + LE:U32!
   pid MIB 8 + LE:U32!
   RAW-CAP RAW-U !
   MIB 3 RAW RAW-U BYTE-VIEW NULL-PTR 0 SYSCTL 0 < if
      ERRNO@ {: e:n :}
      s" ?argv-errno=" ARGS+  e NUM$ ARGS+
      false exit
   then
   RAW-U @ 4 < if false exit then
   RAW LE:U32@ {: argc:n :}
   4 CUR !
   SKIP-EXEC-PATH
   0 begin dup argc < MORE? and ARGS-ROOM? and while
      ARG+
      dup 1+ argc < ARGS-ROOM? and if STR-SPACE ARGS-C+ then
      1+
   repeat drop true ;

\ 0 is an unavailable observation, including a process that has exited.
: VERSION@ ( n -- n )
   PROC-PIDUNIQINFO 0 UNIQINFO UNIQINFO-BYTES PID-INFO {: bytes:n :}
   bytes 0= if 0 exit then
   bytes UNIQINFO-BYTES <> if E-PROC-OUTPUT throw then
   UNIQINFO 32 + LE:U32@ ;

\ A changing or unavailable version leaves no image to classify. Retry only
\ at the existing event/sweep boundary; a process may keep execing or exit.
: IMAGE-READ ( n -- bool )
   {: pid:n :}
   pid VERSION@ {: before:n :}
   pid PATH-READ {: pathok:bool :}
   pid ARGS-READ {: argsok:bool :}
   pid VERSION@ {: after:n :}
   before 0<> before after = and pathok and argsok and if
      after IMAGE-VERSION ! true exit
   then
   1 INCOMPLETE +!
   PATH-U BUF-RESET s" ?incomplete-image" PATH+
   ARGS-U BUF-RESET s" ?incomplete-image" ARGS+
   false ;

\ ---- the log -------------------------------------------------------------------

: NOW ( -- n )                     \ tenths of a millisecond since T0
   mono-ns T0 @ - 100000 / ;

: LINE+ ( ptr u8 n -- )   LINE LINE-CAP LINE-U BUF-APPEND ;
: LINE-C+ ( n -- )   LINE LINE-CAP LINE-U BUF-APPEND-C ;

: TENTHS+ ( n -- ) {: t:n :}
   t 10 / NUM$ LINE+  [char] . LINE-C+  t 10 mod NUM$ LINE+ ;

: LINE-WRITE ( -- )
   LOG-FD @ >FD LINE LINE-U @ LEN>N FD:WRITE-FULL ;

: LINE-HEAD ( ptr u8 n n n -- ) {: tag:ptr tagu:n pid:n aux:n :}
   LINE-U BUF-RESET
   NOW TENTHS+ STR-SPACE LINE-C+
   tag tagu LINE+ STR-SPACE LINE-C+
   pid NUM$ LINE+ STR-SPACE LINE-C+
   aux NUM$ LINE+ STR-SPACE LINE-C+ ;

: EVENT-LINE ( ptr u8 n n n -- )
   LINE-HEAD s" - | -" LINE+ STR-LF LINE-C+ LINE-WRITE ;

: PROC-LINE ( ptr u8 n n n -- )
   LINE-HEAD PATH$ LINE+ s"  | " LINE+ ARGS$ LINE+ STR-LF LINE-C+ LINE-WRITE ;

\ ---- text, records and census rows ---------------------------------------------

\ The bytes appended to TEXT: their offset there. TEXT holds a byte past those
\ it names, so an empty string at its end still has an address.
: TEXT+ ( ptr u8 n -- n ) {: a:ptr u:n :}
   TEXT-U @ {: at:n :}
   at u + 1+ TEXT-RESERVE
   a at TEXT u BYTE-COPY
   u TEXT-U +!
   at ;

\ Valid until the next TEXT+, whose growth may move the mapping.
: TEXT$ ( n n -- ptr u8 n ) {: off:n u:n :}
   off TEXT u ;

: REC ( n n -- ptr n ) {: i:n f:n :}
   i REC-CELLS * f + RECS ;

\ The text record i names at field f, the offset, and f + 1, the length.
: REC$ ( n n -- ptr u8 n ) {: i:n f:n :}
   i f REC @  i f 1+ REC @ TEXT$ ;

: CURRENT ( n -- n )               \ the record of pid's latest image, -1 when never watched
   SLOT @ 1- ;

: IMAGE! ( n -- ) {: i:n :}
   PATH$ TEXT+ i R-PATH REC !
   PATH-U @ LEN>N i R-PATH-U REC !
   ARGS$ TEXT+ i R-ARGV REC !
   ARGS-U @ LEN>N i R-ARGV-U REC !
   IMAGE-VERSION @ i R-VERSION REC !
   0 i R-DIRTY REC ! ;

: IMAGE-SAME? ( n -- bool ) {: i:n :}
   i R-PATH REC$ PATH$ STR=
   i R-ARGV REC$ ARGS$ STR= and ;

: ENTRY ( ptr n n -- ptr n ) {: t:ptr i:n :}   \ row i of table t
   t i KEY-CELLS * cells + ;

: ROW@ ( ptr n n n -- n ) {: t:ptr i:n f:n :}
   t i ENTRY f cells + @ ;

: ROW! ( n ptr n n n -- ) {: v:n t:ptr i:n f:n :}
   v t i ENTRY f cells + ! ;

: ROW$ ( ptr n n -- ptr u8 n ) {: t:ptr i:n :}
   t i K-OFF ROW@  t i K-U ROW@ TEXT$ ;

: ROW-OF ( ptr n n ptr u8 n -- n ) {: t:ptr n:n a:ptr u:n :}   \ the row keyed a u, -1 when none
   n 0 ?do
      t i ROW$ a u STR= if i unloop exit then
   loop
   -1 ;

\ One more of key a u in table t, whose row count is at np.
: COUNT+ ( ptr n ptr n ptr u8 n -- ) {: t:ptr np:ptr a:ptr u:n :}
   t np @ a u ROW-OF {: r:n :}
   r 0 >= if t r K-COUNT ROW@ 1+ t r K-COUNT ROW! exit then
   np @ KEY-CAP >= if E-PROC-TRUNCATED throw then
   np @ {: k:n :}
   a u TEXT+ t k K-OFF ROW!
   u t k K-U ROW!
   1 t k K-COUNT ROW!
   0 t k K-DONE ROW!
   k 1+ np ! ;

\ ---- buckets and argv shapes -----------------------------------------------------

: BASENAME ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}   \ after the last /, all of it when none
   u begin dup 0 > while
      a over 1- + c@ [char] / = if a over + u rot - exit then
      1-
   repeat drop
   a u ;

: BUCKET+ ( ptr u8 n -- )   BUCKET-BUF LINE-CAP BUCKET-U BUF-APPEND ;

: BUCKET ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a u s" /bin/hb" ENDS-WITH? if s" bin/hb" exit then
   a u s" hb-under-test" CONTAINS? if s" hb-under-test (candidate)" exit then
   a u s" /hb-stage" CONTAINS?  a u s" /hb-stdin" CONTAINS? or  a u s" /hb-seed" CONTAINS? or if
      s" fixpoint stage engine" exit
   then
   a u s" hbtmp" CONTAINS?  a u s" hb-gate" CONTAINS? or  a u s" /gate-" CONTAINS? or if
      BUCKET-U BUF-RESET
      s" other scratch binary: " BUCKET+  a u BASENAME BUCKET+
      BUCKET-BUF BUCKET-U @ LEN>N exit
   then
   a u ;

: HB-FAMILY? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" bin/hb" STARTS-WITH?
   a u s" hb-under-test" STARTS-WITH? or
   a u s" fixpoint" STARTS-WITH? or ;

: SHAPE+ ( ptr u8 n -- )   SHAPE-BUF LINE-CAP SHAPE-U BUF-APPEND ;

: TOKEN ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}   \ up to the first space
   a u STR-SPACE INDEX-OF MATCH option
     none OF a u ENDOF
     some OF IDX>N a swap ENDOF
   ;MATCH ;

: TOKEN-START ( ptr u8 n -- n ) {: a:ptr e:n :}    \ of the token that ends at e
   e begin dup 0 > if a over 1- + c@ STR-SPACE <> else false then while 1- repeat ;

\ The start of the token that ends at e, and whether it ends in .f.
: FILE-TOKEN? ( ptr u8 n -- n bool ) {: a:ptr e:n :}
   a e TOKEN-START {: s:n :}
   s  a s + e s - s" .f" ENDS-WITH? ;

\ The offset and length of the last space-separated token ending in .f; 0 -1
\ when none.
: LAST-FILE ( ptr u8 n -- n n ) {: a:ptr u:n :}
   u begin dup 0 > while
      a over 1- + c@ STR-SPACE = if 1- else
         a over FILE-TOKEN? if tuck - exit then
         nip
      then
   repeat drop
   0 -1 ;

\ Just past the first "--load ", -1 when there is none or a space or nothing
\ follows it.
: LOAD-AT ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u s" --load " FIND-SUB MATCH option
     none OF -1 ENDOF
     some OF IDX>N 7 + ENDOF
   ;MATCH {: at:n :}
   at 0 < if -1 exit then
   at u >= if -1 exit then
   a at + c@ STR-SPACE = if -1 exit then
   at ;

\ The text is one token, or two separated by one space.
: SHORT-TAIL? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0= if false exit then
   a c@ STR-SPACE = if false exit then
   a u + 1- c@ STR-SPACE = if false exit then
   a u STR-SPACE COUNT-CHAR 2 < ;

: TAIL-AT? ( ptr u8 n n -- bool ) {: a:ptr u:n i:n :}
   a i + 4 s"  -- " STR= 0= if false exit then
   a i + 4 + u i - 4 - SHORT-TAIL? ;

\ Where the first " -- " followed by a short tail to the end starts, -1 when
\ none.
: TAIL ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 begin dup 4 + u <= while
      dup a u rot TAIL-AT? if exit then
      1+
   repeat drop
   -1 ;

\ The entry file of a --load argv - its last token after "--load " that ends in
\ .f, else the first - and " -- " with the short tail when there is one.
: LOAD-SHAPE ( ptr u8 n n -- ptr u8 n ) {: a:ptr u:n at:n :}
   SHAPE-U BUF-RESET
   a at + u at - LAST-FILE {: off:n len:n :}
   len 0 < if a at + u at - TOKEN SHAPE+ else a at + off + len SHAPE+ then
   a u TAIL {: i:n :}
   i 0 >= if s"  -- " SHAPE+  a i + 4 + u i - 4 - SHAPE+ then
   SHAPE-BUF SHAPE-U @ LEN>N ;

: SHAPE-KEY ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a u LOAD-AT {: at:n :}
   at 0 >= if a u at LOAD-SHAPE exit then
   a u s" --build" CONTAINS? if s" --build" exit then
   u 0= if s" (no argv)" exit then
   a u 80 min ;

\ ---- classifying ----------------------------------------------------------------
\ Classify coherent observations in log order. An initial child comparison
\ stays pending until its parent's image is available.

: FRESH ( n -- )                   \ record i holds a fresh image, the one just read
   1 EXEC-TOTAL +!
   PATH$ BUCKET {: i:n b:ptr bu:n :}
   EXECS EXECS-N b bu COUNT+
   b bu HB-FAMILY? if SHAPES SHAPES-N ARGS$ SHAPE-KEY COUNT+ then
   i IMAGE! ;

: CHILD-SEEN ( n n -- ) {: i:n ppid:n :}
   i R-EXEC REC @ 0<> if i FRESH exit then
   ppid CURRENT {: p:n :}
   \ Missing parent evidence establishes neither exec nor inheritance. Keep
   \ this child unclassified and dirty until an event or sweep can compare it.
   p 0 >= if p R-VERSION REC @ 0<> else false then 0= if
      1 INCOMPLETE +! exit
   then
   p IMAGE-SAME? if
      1 i R-INHERITED REC !
      i IMAGE! exit
   then
   i FRESH ;

: CLASSIFY ( n -- )
   {: i:n :}
   i R-VERSION REC @ 0= if
      i R-PID REC @ ROOT @ = if i FRESH else i i R-PPID REC @ CHILD-SEEN then
      exit
   then
   i R-VERSION REC @ IMAGE-VERSION @ = if
      i R-INHERITED REC @ 0<> i R-EXEC REC @ 0<> and 0= if
         0 i R-DIRTY REC ! exit
      then
   then
   0 i R-INHERITED REC !
   i FRESH ;

\ ---- following the tree ---------------------------------------------------------

: PENDING+ ( n -- ) {: pid:n :}
   PENDING-N @ 1+ PENDING-RESERVE
   pid PENDING-N @ PENDING !
   1 PENDING-N +! ;

: CHANGE! ( n -- ) {: pid:n :}
   pid CHANGE LE:U64!
   EVFILT-PROC CHANGE 8 + LE:U16!
   EV-ADD-ENABLE CHANGE 10 + LE:U16!
   NOTE-ALL CHANGE 12 + LE:U32!
   0 CHANGE 16 + LE:U64!
   0 CHANGE 24 + LE:U64! ;

\ Register pid, a child of ppid, log it under tag and classify it; its children
\ are listed by DRAIN. A refused registration is logged LOST and leaves pid
\ unwatched, so a later sweep tries again.
: WATCH ( n n ptr u8 n -- ) {: pid:n ppid:n tag:ptr tagu:n :}
   pid 0 <= if exit then
   pid PID-CAP >= if E-PROC-OUTPUT throw then
   pid CURRENT {: c:n :}
   c 0 >= if c R-ALIVE REC @ 0<> if exit then then
   pid CHANGE!
   KQ @ CHANGE 1 EVENT 0 NULL-PTR KEVENT 0 < if
      1 LOST +!
      s" LOST" pid ppid EVENT-LINE
      exit
   then
   REC-N @ {: i:n :}
   i 1+ REC-CELLS * RECS-RESERVE
   pid i R-PID REC !
   1 i R-ALIVE REC !
   0 i R-VERSION REC !
   0 i R-INHERITED REC !
   ppid i R-PPID REC !
   1 i R-DIRTY REC !
   0 i R-EXEC REC !
   i 1+ pid SLOT !
   1 REC-N +!
   1 ALIVE +!
   pid IMAGE-READ {: valid:bool :}
   tag tagu pid ppid PROC-LINE
   valid if i CLASSIFY then
   pid PENDING+ ;

: LISTED@ ( n -- n )   PID-BYTES * LIST + LE:U32@ ;

\ Lists pid's children and registers the new ones. The list is read whole
\ before another is asked for: WATCH only queues. libproc answers a refusal
\ with 0 bytes, as it answers no children, and only errno tells them apart
\ (src/lib/process-tree.f); a full buffer may have been cut.
: SCAN-CHILDREN ( n -- ) {: pid:n :}
   ERRNO-CLEAR
   PROC-PPID-ONLY pid LIST LIST-BYTES LIST-PIDS {: bytes:n :}
   bytes 0= ERRNO@ 0<> and if E-PROC-OUTPUT throw then
   bytes LIST-BYTES >= if E-PROC-TRUNCATED throw then
   bytes PID-BYTES / 0 ?do
      i LISTED@ dup 0 > if pid s" CHILD" WATCH else drop then
   loop ;

: DRAIN ( -- )
   begin PENDING-N @ 0 > while
      -1 PENDING-N +!
      PENDING-N @ PENDING @ SCAN-CHILDREN
   repeat ;

: SWEEP ( -- )
   REC-N @ 0 ?do
      i R-ALIVE REC @ 0<> if
         i R-DIRTY REC @ 0<> if
            i R-PID REC @ IMAGE-READ if
               s" IMAGE" i R-PID REC @ i R-PPID REC @ PROC-LINE
               i CLASSIFY
            then
         then
         i R-PID REC @ SCAN-CHILDREN
      then
   loop
   DRAIN ;

: ROOT-REAP ( -- )
   ROOT @ >PID PROC-WAIT-STATUS ROOT-STATUS !
   1 ROOT-EXITED ! ;

: EXITED ( n -- ) {: pid:n :}
   pid CURRENT {: c:n :}
   c 0 >= if c R-ALIVE REC @ 0<> if
      c R-VERSION REC @ 0= if s" INCOMPLETE" pid c R-PPID REC @ EVENT-LINE then
      c R-INHERITED REC @ 0<> c R-EXEC REC @ 0= and if
         1 FORK-TOTAL +!
         PATH-U BUF-RESET c R-PATH REC$ PATH+
         FORKS FORKS-N PATH$ BASENAME COUNT+
      then
      0 c R-ALIVE REC !  -1 ALIVE +!
   then then
   s" EXIT" pid 0 EVENT-LINE
   pid ROOT @ = if ROOT-REAP then
   ROOT-EXITED @ 0<> ALIVE @ 0 <= and if 1 DONE ! then ;

: EXECED ( n -- ) {: pid:n :}
   pid CURRENT {: c:n :}
   c 0 < if E-PROC-OUTPUT throw then
   1 c R-EXEC REC !
   1 c R-DIRTY REC !
   pid IMAGE-READ {: valid:bool :}
   s" EXEC" pid 0 PROC-LINE
   valid if c CLASSIFY then ;

: EVENT-HANDLE ( -- )
   EVENT LE:U64@ {: pid:n :}
   EVENT 12 + LE:U32@ {: ff:n :}
   ff NOTE-FORK and 0<> if
      s" FORK" pid 0 EVENT-LINE
      pid SCAN-CHILDREN DRAIN
   then
   ff NOTE-EXEC and 0<> if pid EXECED then
   ff NOTE-EXIT and 0<> if pid EXITED then ;

\ A wait that timed out: sweep, and once the root has exited, stop when the
\ tree has or after DRAIN-ROUNDS such ticks.
: TICKED ( -- )
   SWEEP
   NOW LAST-SWEEP !
   ROOT-EXITED @ 0= if exit then
   ALIVE @ 0 <= if 1 DONE ! exit then
   1 DRAINED +!
   DRAINED @ DRAIN-ROUNDS > if 1 DONE ! then ;

: STEP ( -- )
   KQ @ NULL-PTR 0 EVENT 1 TICK KEVENT {: n:n :}
   n 0 < if
      ERRNO@ EINTR = if exit then
      E-PROC-OUTPUT throw
   then
   n 0= if TICKED exit then
   EVENT-HANDLE
   DONE @ 0<> if exit then
   NOW LAST-SWEEP @ - SWEEP-TENTHS > if SWEEP NOW LAST-SWEEP ! then ;

: RUN ( -- )
   begin STEP DONE @ 0<> until ;

\ ---- the census -----------------------------------------------------------------

\ Two spaces, the count right-aligned in six columns, two spaces, the key.
: .ROW ( ptr u8 n n -- ) {: a:ptr u:n c:n :}
   c NUM$ {: d:ptr du:n :}
   s"   " type
   6 du - 0 ?do space loop
   d du type s"   " type a u type cr ;

\ The undone row of t with the greatest count, the first among equals.
: TOP ( ptr n n -- n ) {: t:ptr n:n :}
   -1 BEST !  0 BEST-COUNT !
   n 0 ?do
      t i K-DONE ROW@ 0=  t i K-COUNT ROW@ BEST-COUNT @ > and if
         i BEST !  t i K-COUNT ROW@ BEST-COUNT !
      then
   loop
   BEST @ ;

: .NEXT ( ptr n n -- ) {: t:ptr n:n :}
   t n TOP {: r:n :}
   1 t r K-DONE ROW!
   t r ROW$ t r K-COUNT ROW@ .ROW ;

\ Rows by count, greatest first, ties in the order first seen.
: .MOST-COMMON ( ptr n n -- ) {: t:ptr n:n :}
   n 0 ?do t n .NEXT loop ;

: .TOTAL ( ptr u8 n n -- ) {: a:ptr u:n v:n :}
   a u type v NUM$ type cr ;

: REPORT ( -- )
   s" total exec events (fresh images): " EXEC-TOTAL @ .TOTAL
   EXECS EXECS-N @ .MOST-COMMON
   s" total fork children (no exec): " FORK-TOTAL @ .TOTAL
   FORKS FORKS-N @ .MOST-COMMON
   s" lost registrations: " LOST @ .TOTAL
   s" incomplete image reads: " INCOMPLETE @ .TOTAL
   cr s" hb-family exec argv shapes:" type cr
   SHAPES SHAPES-N @ .MOST-COMMON ;

: DONE-LINE ( -- )
   LINE-U BUF-RESET
   NOW TENTHS+
   s"  DONE alive=" LINE+ ALIVE @ NUM$ LINE+
   s"  lost=" LINE+ LOST @ NUM$ LINE+
   s"  status=" LINE+ ROOT-STATUS @ NUM$ LINE+
   STR-LF LINE-C+ LINE-WRITE ;

: EXIT-CODE ( -- n )
   ROOT-STATUS @ PROC-STATUS>OUTCOME MATCH outcome
     exited OF ENDOF
     signaled OF drop 1 ENDOF
     timeout OF 1 ENDOF
   ;MATCH ;

\ CMD with its arguments, in the census's own environment and stdio.
: SPAWN-ROOT ( -- )
   PROC-ARGV-ENV-RESET
   SCRIPT-ARGC 2 ?do i SCRIPT-ARGV$ >LEN PROC-ARGV+ loop
   PROC-ENV-INHERIT-MISSING
   1 SCRIPT-ARGV$ >LEN -1 >FD -1 >FD -1 >FD PROC-SPAWN-ARGV-ENV-IO PID>N ROOT ! ;

public

: MAIN ( -- )
   HB-TARGET-MACOS? 0= if s" boot-census: macOS only (kqueue EVFILT_PROC, libproc)" RC-HOST die then
   SCRIPT-ARGC 2 < if s" usage: bin/hb --load tools/perf/boot-census.f -- LOGFILE CMD [ARGS...]" RC-USAGE die then
   0 SCRIPT-ARGV$ FS:>PATHZ FS:O-WRONLY FS:O-CREAT or FS:O-TRUNC or FS:MODE-0644 open {: fd:n :}
   fd 0 < if s" boot-census: cannot open LOGFILE" RC-IO die then
   fd LOG-FD !
   PID-CAP SLOT-RESERVE
   KQUEUE {: kq:n :}
   kq 0 < if E-PROC-OUTPUT throw then
   kq KQ !
   mono-ns T0 !
   SPAWN-ROOT
   ROOT @ getpid s" ROOT" WATCH
   \ The spawn cannot start the root suspended, so it may end before it is
   \ registered; macOS refuses to register a process that has ended, and no
   \ exit event will come for it.
   ROOT @ CURRENT 0 < if ROOT-REAP then
   DRAIN
   RUN
   DONE-LINE
   LOG-FD @ close
   REPORT
   EXIT-CODE {: rc:n :}
   rc 0<> if s" " rc die then ;

;package

BOOT-CENSUS:MAIN

\ fs-test.f - focused tests for checked stdlib filesystem helpers.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f lib/fs-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/task.f                  \ two tasks through READ-ALL / FILE-SIZE at once

1 constant FS-TEST-EX-FAIL
$34 constant FS-TEST-U16-LO
$12 constant FS-TEST-U16-HI
$0807060504030201 constant FS-TEST-U64-VALUE
$78 constant FS-TEST-FILL-C
3 constant FS-TEST-WRITE-LEN
4 constant FS-TEST-EXACT-CAP
8 constant FS-TEST-READ-CAP
40 constant FS-TEST-DEEP-LIMIT
292 constant FS-TEST-MODE-READONLY
$40 constant FS-TEST-PAR-CAP
150 constant FS-TEST-PAR-ITERS
$61 constant FS-TEST-PAR-A-C
$62 constant FS-TEST-PAR-B-C
16 constant FS-TEST-PAR-A-N
32 constant FS-TEST-PAR-B-N

variable FS-TEST-CASE
variable FS-TEST-FAIL
variable FS-TEST-WALK-COUNT
variable FS-TEST-DEEP-IDX
variable FS-TEST-AFTER-IDX
variable FS-TEST-BETA-IDX
variable FS-TEST-FILE-COUNT
variable FS-TEST-FD
variable FS-TEST-BASE-U
variable FS-TEST-PAR-A-U
variable FS-TEST-PAR-B-U
variable FS-TEST-PAR-BAD
variable FS-TEST-PAR-DONE
variable FS-TEST-PAR-READY
variable FS-TEST-ROOT-U
variable FS-TEST-ALPHA-U
variable FS-TEST-CHILD-U
variable FS-TEST-ROOT-GIT-U
variable FS-TEST-ROOT-JJ-U
variable FS-TEST-ROOT-DOTS-U
variable FS-TEST-ALPHA-GIT-U
variable FS-TEST-CHILD-DOTS-U
variable FS-TEST-DEEP-U
variable FS-TEST-DEEP-CUR-U
variable FS-TEST-IO-U
variable FS-TEST-BIG-U
variable FS-TEST-EMPTY-U

create FS-TEST-OUT FS-PATH-CAP allot
create FS-TEST-PAR-A-BUF FS-PATH-CAP allot
create FS-TEST-PAR-B-BUF FS-PATH-CAP allot
create FS-TEST-PAR-SEED FS-TEST-PAR-CAP allot
create FS-TEST-PAR-A-DATA FS-TEST-PAR-CAP allot
create FS-TEST-PAR-B-DATA FS-TEST-PAR-CAP allot
create FS-TEST-BASE-BUF FS-PATH-CAP allot
create FS-TEST-ROOT-BUF FS-PATH-CAP allot
create FS-TEST-ALPHA-BUF FS-PATH-CAP allot
create FS-TEST-CHILD-BUF FS-PATH-CAP allot
create FS-TEST-ROOT-GIT-BUF FS-PATH-CAP allot
create FS-TEST-ROOT-JJ-BUF FS-PATH-CAP allot
create FS-TEST-ROOT-DOTS-BUF FS-PATH-CAP allot
create FS-TEST-ALPHA-GIT-BUF FS-PATH-CAP allot
create FS-TEST-CHILD-DOTS-BUF FS-PATH-CAP allot
create FS-TEST-DEEP-BUF FS-PATH-CAP allot
create FS-TEST-DEEP-CUR-BUF FS-PATH-CAP allot
create FS-TEST-IO-BUF FS-PATH-CAP allot
create FS-TEST-BIG-BUF FS-PATH-CAP allot
create FS-TEST-EMPTY-BUF FS-PATH-CAP allot
create FS-TEST-LONG FS-PATH-CAP 1 + allot
create FS-TEST-READ-BUF FS-TEST-READ-CAP allot
create FS-TEST-U16
   FS-TEST-U16-LO c, FS-TEST-U16-HI c,
create FS-TEST-U64
   1 c, 2 c, 3 c, 4 c, 5 c, 6 c, 7 c, 8 c,

: FS-TEST-ASSERT ( bool -- ) {: ok :}
   FS-TEST-CASE @ 1 + FS-TEST-CASE !
   ok 0= if
      [char] F emit FS-TEST-CASE @ .
      FS-TEST-FAIL @ 1 + FS-TEST-FAIL !
   then ;

: FS-TEST= ( n n -- ) {: got want :}
   got want = FS-TEST-ASSERT ;

: FS-TEST-TRUE ( bool -- )
   FS-TEST-ASSERT ;

: FS-TEST-FALSE ( bool -- )
   0= FS-TEST-ASSERT ;

: FS-TEST$= ( ptr u8 n ptr u8 n -- )
   STR= FS-TEST-ASSERT ;

\ `c dst over + c!` wrote c at dst+c on every pass instead of filling dst[0,u):
\ `over` copied c, not the loop index. Its only caller measured a length and
\ never read the bytes back, so it went unseen until FS-TEST-PARALLEL below
\ compared file contents. `c over dst +` takes the index.
: FS-TEST-FILL ( ptr u8 n n -- ) {: dst:ptr u c :}
   0 begin dup u < while
      c over dst + c!
      1+
   repeat drop ;

: FS-TEST-JOIN$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: pa:ptr pu na:ptr nu :}
   pa pu na nu FS-TEST-OUT JOIN-PATH
   FS-TEST-OUT swap ;

: FS-TEST-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: FS-TEST-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu na:ptr nu dst:ptr lenp:ptr :}
   pa pu na nu dst JOIN-PATH lenp ! ;

: FS-TEST-BASE ( -- ptr u8 n )
   FS-TEST-BASE-BUF FS-TEST-BASE-U @ ;

: FS-TEST-ROOT ( -- ptr u8 n )
   FS-TEST-ROOT-BUF FS-TEST-ROOT-U @ ;

: FS-TEST-ALPHA ( -- ptr u8 n )
   FS-TEST-ALPHA-BUF FS-TEST-ALPHA-U @ ;

: FS-TEST-CHILD ( -- ptr u8 n )
   FS-TEST-CHILD-BUF FS-TEST-CHILD-U @ ;

: FS-TEST-ROOT-GIT ( -- ptr u8 n )
   FS-TEST-ROOT-GIT-BUF FS-TEST-ROOT-GIT-U @ ;

: FS-TEST-ROOT-JJ ( -- ptr u8 n )
   FS-TEST-ROOT-JJ-BUF FS-TEST-ROOT-JJ-U @ ;

: FS-TEST-ROOT-DOTS ( -- ptr u8 n )
   FS-TEST-ROOT-DOTS-BUF FS-TEST-ROOT-DOTS-U @ ;

: FS-TEST-ALPHA-GIT ( -- ptr u8 n )
   FS-TEST-ALPHA-GIT-BUF FS-TEST-ALPHA-GIT-U @ ;

: FS-TEST-CHILD-DOTS ( -- ptr u8 n )
   FS-TEST-CHILD-DOTS-BUF FS-TEST-CHILD-DOTS-U @ ;

: FS-TEST-DEEP ( -- ptr u8 n )
   FS-TEST-DEEP-BUF FS-TEST-DEEP-U @ ;

: FS-TEST-DEEP-CUR ( -- ptr u8 n )
   FS-TEST-DEEP-CUR-BUF FS-TEST-DEEP-CUR-U @ ;

: FS-TEST-IO-PATH ( -- ptr u8 n )
   FS-TEST-IO-BUF FS-TEST-IO-U @ ;

: FS-TEST-BIG-PATH ( -- ptr u8 n )
   FS-TEST-BIG-BUF FS-TEST-BIG-U @ ;

: FS-TEST-EMPTY-PATH ( -- ptr u8 n )
   FS-TEST-EMPTY-BUF FS-TEST-EMPTY-U @ ;

: FS-TEST-BASE! ( -- )
   s" habu-fs" TMPDIR-MKDIR {: a:ptr u :}
   a u FS-TEST-BASE-BUF FS-TEST-BASE-U FS-TEST-COPY! ;

: FS-TEST-PATHS! ( -- )
   FS-TEST-BASE s" root" FS-TEST-ROOT-BUF FS-TEST-ROOT-U FS-TEST-PATH!
   FS-TEST-ROOT s" alpha" FS-TEST-ALPHA-BUF FS-TEST-ALPHA-U FS-TEST-PATH!
   FS-TEST-ALPHA s" aa-child" FS-TEST-CHILD-BUF FS-TEST-CHILD-U FS-TEST-PATH!
   FS-TEST-ROOT s" .git" FS-TEST-ROOT-GIT-BUF FS-TEST-ROOT-GIT-U FS-TEST-PATH!
   FS-TEST-ROOT s" .jj" FS-TEST-ROOT-JJ-BUF FS-TEST-ROOT-JJ-U FS-TEST-PATH!
   FS-TEST-ROOT s" .dots" FS-TEST-ROOT-DOTS-BUF FS-TEST-ROOT-DOTS-U FS-TEST-PATH!
   FS-TEST-ALPHA s" .git" FS-TEST-ALPHA-GIT-BUF FS-TEST-ALPHA-GIT-U FS-TEST-PATH!
   FS-TEST-CHILD s" .dots" FS-TEST-CHILD-DOTS-BUF FS-TEST-CHILD-DOTS-U FS-TEST-PATH!
   FS-TEST-BASE s" deep" FS-TEST-DEEP-BUF FS-TEST-DEEP-U FS-TEST-PATH!
   FS-TEST-BASE s" io.txt" FS-TEST-IO-BUF FS-TEST-IO-U FS-TEST-PATH!
   FS-TEST-BASE s" big.txt" FS-TEST-BIG-BUF FS-TEST-BIG-U FS-TEST-PATH!
   FS-TEST-BASE s" empty.txt" FS-TEST-EMPTY-BUF FS-TEST-EMPTY-U FS-TEST-PATH! ;

: FS-TEST-MAKE-REGISTERED-DIR ( ptr u8 n -- )
   2dup MAKE-DIR CLEANUP-DIR+ ;

: FS-TEST-WRITE-CHILD ( ptr u8 n ptr u8 n ptr u8 n -- ) {: pa:ptr pu na:ptr nu data:ptr datau :}
   pa pu na nu FS-TEST-OUT JOIN-PATH {: pathu :}
   FS-TEST-OUT pathu data datau WRITE-ALL
   FS-TEST-OUT pathu CLEANUP+ ;

: FS-TEST-MAKE-WALK-DIRS ( -- )
   FS-TEST-ROOT FS-TEST-MAKE-REGISTERED-DIR
   FS-TEST-ALPHA FS-TEST-MAKE-REGISTERED-DIR
   FS-TEST-CHILD FS-TEST-MAKE-REGISTERED-DIR
   FS-TEST-ROOT-GIT FS-TEST-MAKE-REGISTERED-DIR
   FS-TEST-ROOT-JJ FS-TEST-MAKE-REGISTERED-DIR
   FS-TEST-ROOT-DOTS FS-TEST-MAKE-REGISTERED-DIR
   FS-TEST-ALPHA-GIT FS-TEST-MAKE-REGISTERED-DIR
   FS-TEST-CHILD-DOTS FS-TEST-MAKE-REGISTERED-DIR ;

: FS-TEST-WRITE-WALK-FILES ( -- )
   FS-TEST-CHILD s" deep.txt" s" " FS-TEST-WRITE-CHILD
   FS-TEST-ALPHA s" zz-after.txt" s" " FS-TEST-WRITE-CHILD
   FS-TEST-ROOT s" beta.txt" s" " FS-TEST-WRITE-CHILD
   FS-TEST-ROOT-GIT s" ignored.txt" s" " FS-TEST-WRITE-CHILD
   FS-TEST-ROOT-JJ s" ignored.txt" s" " FS-TEST-WRITE-CHILD
   FS-TEST-ROOT-DOTS s" ignored.txt" s" " FS-TEST-WRITE-CHILD
   FS-TEST-ALPHA-GIT s" ignored.txt" s" " FS-TEST-WRITE-CHILD
   FS-TEST-CHILD-DOTS s" ignored.txt" s" " FS-TEST-WRITE-CHILD ;

: FS-TEST-DEEP-COMP ( n -- ptr u8 n )
   SB-RESET
   s" d" SB-APPEND
   FS-MUT-SB-U
   SB$ ;

: FS-TEST-DEEP-STEP ( n -- ) {: idx :}
   idx FS-TEST-DEEP-COMP {: ca:ptr cu :}
   FS-TEST-DEEP-CUR ca cu FS-TEST-OUT JOIN-PATH {: u :}
   FS-TEST-OUT u FS-TEST-DEEP-CUR-BUF FS-TEST-DEEP-CUR-U FS-TEST-COPY!
   FS-TEST-DEEP-CUR FS-TEST-MAKE-REGISTERED-DIR ;

: FS-TEST-MAKE-DEEP ( -- )
   FS-TEST-DEEP FS-TEST-MAKE-REGISTERED-DIR
   FS-TEST-DEEP FS-TEST-DEEP-CUR-BUF FS-TEST-DEEP-CUR-U FS-TEST-COPY!
   0 begin dup FS-TEST-DEEP-LIMIT < while
      dup FS-TEST-DEEP-STEP
      1+
   repeat drop
   FS-TEST-DEEP-CUR s" leaf.txt" s" " FS-TEST-WRITE-CHILD ;

: FS-TEST-PREPARE-FIXTURE ( -- )
   CLEANUP-RESET
   FS-TEST-BASE!
   FS-TEST-PATHS!
   FS-TEST-BASE CLEANUP-DIR+
   FS-TEST-MAKE-WALK-DIRS
   FS-TEST-WRITE-WALK-FILES
   FS-TEST-BIG-PATH s" abcd" WRITE-ALL
   FS-TEST-BIG-PATH CLEANUP+
   FS-TEST-EMPTY-PATH s" " WRITE-ALL
   FS-TEST-EMPTY-PATH CLEANUP+
   FS-TEST-IO-PATH CLEANUP+
   FS-TEST-MAKE-DEEP ;

: FS-TEST-PATH-TOO-LONG ( -- )
   FS-TEST-LONG FS-PATH-CAP 1 + FS-PATHZ drop ;

: FS-TEST-JOIN-TOO-LONG ( -- )
   FS-TEST-LONG FS-PATH-CAP s" z" FS-TEST-OUT JOIN-PATH drop ;

: FS-TEST-JOIN-NEG-PARENT ( -- )
   s" root" drop -1 s" child" FS-TEST-OUT JOIN-PATH drop ;

: FS-TEST-CAP-TOO-LONG ( -- )
   FS-PATH-CAP 1 + FS-CHECK-JOIN-CAP ;

: FS-TEST-MISSING-STAT ( -- )
   s" no-such-habu-fs-path-for-stdlib-test" STAT-MODE drop ;

: FS-TEST-MISSING-FILE-SIZE ( -- )
   s" no-such-habu-fs-path-for-stdlib-test" FILE-SIZE drop ;

: FS-TEST-DIR-FILE-SIZE ( -- )
   FS-TEST-ROOT FILE-SIZE drop ;

: FS-TEST-MISSING-WALK ( -- )
   s" no-such-habu-fs-path-for-stdlib-test" [: 2drop ;] WALK-FILES ;

: FS-TEST-DEEP-WALK ( -- )
   FS-TEST-DEEP [: 2drop ;] WALK-FILES ;

\ The walk root is copied into the depth-0 walk slot, a span of FS-PATH-CAP: a
\ root that does not fit is refused by the copy (E-SPAN-CAPACITY) where the
\ module used to compare the length by hand.
: FS-TEST-WALK-ROOT-TOO-LONG ( -- )
   FS-TEST-LONG FS-PATH-CAP 1 + [: 2drop ;] WALK-FILES ;

: FS-TEST-MISSING-READ ( -- )
   s" no-such-habu-fs-read-file" FS-TEST-READ-BUF FS-TEST-READ-CAP READ-ALL drop ;

: FS-TEST-READ-TOO-LARGE ( -- )
   FS-TEST-BIG-PATH FS-TEST-READ-BUF FS-TEST-WRITE-LEN READ-ALL drop ;

: FS-TEST-WRITE-DIR ( -- )
   FS-TEST-ROOT s" x" WRITE-ALL ;

: FS-TEST-APPEND-DIR ( -- )
   FS-TEST-ROOT s" x" APPEND-FILE ;

: FS-TEST-WRITE-MISSING-PARENT ( -- )
   FS-TEST-ROOT s" missing-parent/out.txt" FS-TEST-OUT JOIN-PATH
   FS-TEST-OUT swap s" x" WRITE-ALL ;

: FS-TEST-APPEND-MISSING-PARENT ( -- )
   FS-TEST-ROOT s" missing-parent/out.txt" FS-TEST-OUT JOIN-PATH
   FS-TEST-OUT swap s" x" APPEND-FILE ;

: FS-TEST-WRITE-READONLY ( -- )
   FS-TEST-IO-PATH FS-TEST-MODE-READONLY CHMOD-MODE
   FS-TEST-IO-PATH s" x" WRITE-ALL ;

: FS-TEST-SETUP ( -- )
   FS-TEST-LONG FS-PATH-CAP 1 + FS-TEST-FILL-C FS-TEST-FILL
   0 FS-TEST-WALK-COUNT !
   0 FS-TEST-DEEP-IDX !
   0 FS-TEST-AFTER-IDX !
   0 FS-TEST-BETA-IDX !
   0 FS-TEST-FILE-COUNT ! ;

: FS-TEST-PATHS ( -- )
   s" AGENTS.md" EXISTS? FS-TEST-TRUE
   s" no-such-habu-fs-path-for-stdlib-test" EXISTS? FS-TEST-FALSE
   s" no-such-habu-fs-path-for-stdlib-test" FILE? FS-TEST-FALSE
   s" no-such-habu-fs-path-for-stdlib-test" DIR? FS-TEST-FALSE ;

: FS-TEST-FILE-DIR ( -- )
   s" AGENTS.md" FILE? FS-TEST-TRUE
   s" AGENTS.md" DIR? FS-TEST-FALSE
   s" src" DIR? FS-TEST-TRUE
   s" src" FILE? FS-TEST-FALSE
   s" AGENTS.md" STAT-MODE S-IFMT and S-IFREG = FS-TEST-TRUE
   s" src" STAT-MODE S-IFMT and S-IFDIR = FS-TEST-TRUE ;

: FS-TEST-EXECUTABLE ( -- )
   s" bin/hb" EXECUTABLE? FS-TEST-TRUE
   s" AGENTS.md" EXECUTABLE? FS-TEST-FALSE
   s" no-such-habu-fs-path-for-stdlib-test" EXECUTABLE? FS-TEST-FALSE ;

: FS-TEST-FILE-SIZE ( -- )
   FS-TEST-BIG-PATH FILE-SIZE FS-TEST-EXACT-CAP FS-TEST=
   FS-TEST-EMPTY-PATH FILE-SIZE 0 FS-TEST= ;

: FS-TEST-FILE-META ( -- )
   FS-TEST-BIG-PATH FILE-META {: sz:n mt:n mn:n ct:n cn:n :}
   sz FS-TEST-EXACT-CAP FS-TEST=
   mt 0 > FS-TEST-TRUE
   mn 0 >= FS-TEST-TRUE
   ct 0 > FS-TEST-TRUE
   cn 0 >= FS-TEST-TRUE ;

: FS-TEST-BASENAME ( -- )
   s" file.f" BASENAME s" file.f" FS-TEST$=
   s" src/core/checker.f" BASENAME s" checker.f" FS-TEST$=
   s" src/" BASENAME s" " FS-TEST$=
   s" /" BASENAME s" " FS-TEST$=
   s" " BASENAME s" " FS-TEST$= ;

: FS-TEST-JOIN ( -- )
   s" src" s" core" FS-TEST-JOIN$ s" src/core" FS-TEST$=
   s" src/" s" core" FS-TEST-JOIN$ s" src/core" FS-TEST$=
   s" /" s" tmp" FS-TEST-JOIN$ s" /tmp" FS-TEST$=
   FS-TEST-LONG FS-PATH-CAP 2 - s" z" FS-TEST-OUT JOIN-PATH
   FS-PATH-CAP FS-TEST= ;

: FS-TEST-WALK-CB ( ptr u8 n -- ) {: a:ptr u :}
   FS-TEST-WALK-COUNT @ 1 + FS-TEST-WALK-COUNT !
   a u s" /alpha/aa-child/deep.txt" ENDS-WITH? if
      FS-TEST-WALK-COUNT @ FS-TEST-DEEP-IDX !
   then
   a u s" /alpha/zz-after.txt" ENDS-WITH? if
      FS-TEST-WALK-COUNT @ FS-TEST-AFTER-IDX !
   then
   a u s" /beta.txt" ENDS-WITH? if
      FS-TEST-WALK-COUNT @ FS-TEST-BETA-IDX !
   then
   a u s" /.git/ignored.txt" ENDS-WITH? FS-TEST-FALSE
   a u s" /.jj/ignored.txt" ENDS-WITH? FS-TEST-FALSE
   a u s" /.dots/ignored.txt" ENDS-WITH? FS-TEST-FALSE ;

: FS-TEST-FILE-CB ( ptr u8 n -- ) {: a:ptr u :}
   FS-TEST-FILE-COUNT @ 1 + FS-TEST-FILE-COUNT !
   a u s" /beta.txt" ENDS-WITH? FS-TEST-TRUE ;

: FS-TEST-WALK ( -- )
   FS-TEST-ROOT [: FS-TEST-WALK-CB ;] WALK-FILES
   FS-TEST-WALK-COUNT @ 3 FS-TEST=
   FS-TEST-DEEP-IDX @ 0 > FS-TEST-TRUE
   FS-TEST-AFTER-IDX @ 0 > FS-TEST-TRUE
   FS-TEST-BETA-IDX @ 0 > FS-TEST-TRUE
   FS-TEST-ROOT s" beta.txt" FS-TEST-OUT JOIN-PATH
   FS-TEST-OUT swap [: FS-TEST-FILE-CB ;] WALK-FILES
   FS-TEST-FILE-COUNT @ 1 FS-TEST= ;

: FS-TEST-IO ( -- )
   FS-TEST-IO-PATH s" abc" WRITE-ALL
   FS-TEST-IO-PATH FS-TEST-READ-BUF FS-TEST-READ-CAP READ-ALL FS-TEST-WRITE-LEN FS-TEST=
   FS-TEST-READ-BUF FS-TEST-WRITE-LEN s" abc" FS-TEST$=
   FS-TEST-IO-PATH FS-PATHZ FS-O-RDWR 0 open FS-TEST-FD !
   FS-TEST-FD @ 2 > FS-TEST-TRUE
   FS-TEST-FD @ FS-TEST-READ-BUF 1 read 1 FS-TEST=
   FS-TEST-READ-BUF 1 s" a" FS-TEST$=
   FS-TEST-FD @ close
   FS-TEST-IO-PATH s" xy" WRITE-ALL
   FS-TEST-IO-PATH s" z" APPEND-FILE
   FS-TEST-IO-PATH OPEN-APPEND-FD FS-TEST-FD !
   FS-TEST-FD @ s" !" write 1 FS-TEST=
   FS-TEST-FD @ close
   FS-TEST-IO-PATH FS-TEST-READ-BUF FS-TEST-READ-CAP READ-ALL 4 FS-TEST=
   FS-TEST-READ-BUF 4 s" xyz!" FS-TEST$=
   FS-TEST-BIG-PATH FS-TEST-READ-BUF FS-TEST-EXACT-CAP READ-ALL
   FS-TEST-EXACT-CAP FS-TEST=
   FS-TEST-READ-BUF FS-TEST-EXACT-CAP s" abcd" FS-TEST$= ;

: FS-TEST-THROWS ( -- )
   T-RESET
   [: FS-TEST-PATH-TOO-LONG ;] E-FS-PATH TTHROWSQ
   [: FS-TEST-JOIN-TOO-LONG ;] E-FS-CAPACITY TTHROWSQ
   [: FS-TEST-JOIN-NEG-PARENT ;] E-FS-PATH TTHROWSQ
   [: FS-TEST-CAP-TOO-LONG ;] E-FS-CAPACITY TTHROWSQ
   [: FS-TEST-MISSING-STAT ;] E-FS-STAT TTHROWSQ
   [: FS-TEST-MISSING-FILE-SIZE ;] E-FS-STAT TTHROWSQ
   [: FS-TEST-DIR-FILE-SIZE ;] E-FS-STAT TTHROWSQ
   [: FS-TEST-MISSING-WALK ;] E-FS-STAT TTHROWSQ
   [: FS-TEST-DEEP-WALK ;] E-FS-DEPTH TTHROWSQ
   [: FS-TEST-WALK-ROOT-TOO-LONG ;] E-SPAN-CAPACITY TTHROWSQ
   [: FS-TEST-MISSING-READ ;] E-FS-OPEN TTHROWSQ
   [: FS-TEST-READ-TOO-LARGE ;] E-FS-CAPACITY TTHROWSQ
   [: FS-TEST-WRITE-DIR ;] E-FS-OPEN TTHROWSQ
   [: FS-TEST-APPEND-DIR ;] E-FS-OPEN TTHROWSQ
   [: FS-TEST-WRITE-MISSING-PARENT ;] E-FS-OPEN TTHROWSQ
   [: FS-TEST-APPEND-MISSING-PARENT ;] E-FS-OPEN TTHROWSQ
   [: FS-TEST-WRITE-READONLY ;] E-FS-OPEN TTHROWSQ
   T-FAILURES 0 FS-TEST= ;

: FS-TEST-INTERNALS ( -- )
   FS-FALSE FS-TEST-FALSE
   FS-TRUE FS-TEST-TRUE
   FS-TEST-U16 FS-U16@ $1234 FS-TEST=
   FS-TEST-U64 FS-U64@ FS-TEST-U64-VALUE FS-TEST=
   FS-PATH-CAP FS-CHECK-JOIN-CAP
   FS-TEST-BIG-PATH FS-TRY-STAT FS-TEST-TRUE
   FS-STAT-MODE@ S-IFMT and S-IFREG = FS-TEST-TRUE
   FS-STAT-SIZE@ FS-TEST-EXACT-CAP FS-TEST=
   s" no-such-habu-fs-path-for-stdlib-test" FS-TRY-STAT FS-TEST-FALSE
   s" no-such-habu-fs-path-for-stdlib-test" FS-TRY-STAT-MODE MATCH option
     none OF FS-TRUE ENDOF
     some OF drop FS-FALSE ENDOF
   ;MATCH FS-TEST-TRUE
   s" AGENTS.md" FS-TRY-STAT-MODE MATCH option
     none OF FS-FALSE ENDOF
     some OF S-IFMT and S-IFREG = ENDOF
   ;MATCH FS-TEST-TRUE
   s" no-such-habu-fs-path-for-stdlib-test" FS-TRY-LSTAT-MODE MATCH option
     none OF FS-TRUE ENDOF
     some OF drop FS-FALSE ENDOF
   ;MATCH FS-TEST-TRUE
   s" AGENTS.md" FS-TRY-LSTAT-MODE MATCH option
     none OF FS-FALSE ENDOF
     some OF S-IFMT and S-IFREG = ENDOF
   ;MATCH FS-TEST-TRUE ;

: FS-TEST-REPORT ( -- )
   FS-TEST-FAIL @ 0 = if s" fs-test: ok" type cr exit then
   FS-TEST-FAIL @ . s" fs-test: failures" type cr
   s" fs-test: failures" FS-TEST-EX-FAIL die ;

: FS-TEST-CLEANUP ( -- )
   CLEANUP-RUN
   FS-TEST-BASE EXISTS? FS-TEST-FALSE ;

\ TWO TASKS THROUGH READ-ALL AND FILE-SIZE AT ONCE. The per-call slots are the
\ FS-ABI band of each task's own region now, so each task threads its own
\ descriptor, length and path. While they were process-wide this did not merely
\ return wrong bytes: one task's `0 FS-IO-LEN !` reset the other's progress and
\ the pair did not terminate, so a regression here shows up as this file
\ hanging rather than failing. The two files differ in both length and content
\ so a crossed descriptor is caught either way.
TASK:MIN-STACK TASK:TASK FS-TEST-PAR-A-TASK
TASK:MIN-STACK TASK:TASK FS-TEST-PAR-B-TASK

: FS-TEST-PAR-A ( -- ptr u8 n )
   FS-TEST-PAR-A-BUF FS-TEST-PAR-A-U @ ;

: FS-TEST-PAR-B ( -- ptr u8 n )
   FS-TEST-PAR-B-BUF FS-TEST-PAR-B-U @ ;

: FS-TEST-PAR-BAD+ ( -- )
   1 FS-TEST-PAR-BAD atomic-add drop ;

: FS-TEST-PAR-ROUND ( ptr u8 n ptr u8 n n -- ) {: pa:ptr pu buf:ptr want:n c :}
   pa pu FILE-SIZE want <> if FS-TEST-PAR-BAD+ exit then
   pa pu buf FS-TEST-PAR-CAP READ-ALL want <> if FS-TEST-PAR-BAD+ exit then
   want 0 do buf i + c@ c <> if FS-TEST-PAR-BAD+ unloop exit then loop ;

: FS-TEST-PAR-WORK ( ptr u8 n ptr u8 n n -- ) {: pa:ptr pu buf:ptr want:n c :}
   1 FS-TEST-PAR-READY atomic-add drop
   begin FS-TEST-PAR-READY atomic@ 2 < while TASK:PAUSE repeat
   FS-TEST-PAR-ITERS 0 do pa pu buf want c FS-TEST-PAR-ROUND loop
   1 FS-TEST-PAR-DONE atomic-add drop ;

: FS-TEST-PAR-WORK-A ( -- )
   FS-TEST-PAR-A FS-TEST-PAR-A-DATA FS-TEST-PAR-A-N FS-TEST-PAR-A-C FS-TEST-PAR-WORK ;

: FS-TEST-PAR-WORK-B ( -- )
   FS-TEST-PAR-B FS-TEST-PAR-B-DATA FS-TEST-PAR-B-N FS-TEST-PAR-B-C FS-TEST-PAR-WORK ;

: FS-TEST-PAR-WRITE ( ptr u8 n n n -- ) {: pa:ptr pu n c :}
   FS-TEST-PAR-SEED n c FS-TEST-FILL
   pa pu FS-TEST-PAR-SEED n WRITE-ALL
   pa pu CLEANUP+ ;

: FS-TEST-PARALLEL ( -- )
   FS-TEST-BASE s" par-a.txt" FS-TEST-PAR-A-BUF FS-TEST-PAR-A-U FS-TEST-PATH!
   FS-TEST-BASE s" par-b.txt" FS-TEST-PAR-B-BUF FS-TEST-PAR-B-U FS-TEST-PATH!
   FS-TEST-PAR-A FS-TEST-PAR-A-N FS-TEST-PAR-A-C FS-TEST-PAR-WRITE
   FS-TEST-PAR-B FS-TEST-PAR-B-N FS-TEST-PAR-B-C FS-TEST-PAR-WRITE
   0 FS-TEST-PAR-BAD !  0 FS-TEST-PAR-DONE !  0 FS-TEST-PAR-READY !
   ['] FS-TEST-PAR-WORK-A FS-TEST-PAR-A-TASK TASK:ACTIVATE
   ['] FS-TEST-PAR-WORK-B FS-TEST-PAR-B-TASK TASK:ACTIVATE
   begin FS-TEST-PAR-DONE atomic@ 2 < while TASK:PAUSE repeat
   FS-TEST-PAR-A-TASK TASK:KILL
   FS-TEST-PAR-B-TASK TASK:KILL
   FS-TEST-PAR-BAD @ 0= FS-TEST-TRUE ;

\ Replace a symlink between the two FILE? checks. A refused write that has
\ already opened its target must close it; the lowest free fd stays unchanged.
create FS-TEST-RACE-LIVE-BUF FS-PATH-CAP allot
create FS-TEST-RACE-NEXT-BUF FS-PATH-CAP allot
variable FS-TEST-RACE-LIVE-U
variable FS-TEST-RACE-NEXT-U
variable FS-TEST-RACE-STOP
variable FS-TEST-RACE-SWAPS
variable FS-TEST-RACE-REFUSED
variable FS-TEST-RACE-BAD
TASK:MIN-STACK TASK:TASK FS-TEST-RACE-TASK

: FS-TEST-RACE-LIVE ( -- ptr u8 n )
   FS-TEST-RACE-LIVE-BUF FS-TEST-RACE-LIVE-U @ ;
: FS-TEST-RACE-NEXT ( -- ptr u8 n )
   FS-TEST-RACE-NEXT-BUF FS-TEST-RACE-NEXT-U @ ;
: FS-TEST-RACE-REPLACE ( ptr u8 n -- )
   FS-TEST-RACE-NEXT MAKE-SYMLINK
   FS-TEST-RACE-NEXT FS-TEST-RACE-LIVE RENAME-FILE ;
: FS-TEST-RACE-WORK ( -- )
   begin FS-TEST-RACE-STOP atomic@ 0= while
      FS-TEST-IO-PATH FS-TEST-RACE-REPLACE
      s" /dev/null" FS-TEST-RACE-REPLACE
      1 FS-TEST-RACE-SWAPS atomic-add drop
   repeat
   0 TASK:RETURN ;
: FS-TEST-FREE-FD ( -- n )
   S\" /dev/null\z" drop open-rd dup close ;

: FS-TEST-RACED-REFUSAL ( -- )
   FS-TEST-BASE s" race-link" FS-TEST-RACE-LIVE-BUF FS-TEST-RACE-LIVE-U FS-TEST-PATH!
   FS-TEST-BASE s" race-next" FS-TEST-RACE-NEXT-BUF FS-TEST-RACE-NEXT-U FS-TEST-PATH!
   FS-TEST-IO-PATH FS-TEST-RACE-LIVE MAKE-SYMLINK
   0 FS-TEST-RACE-STOP !  0 FS-TEST-RACE-SWAPS !
   0 FS-TEST-RACE-REFUSED !  0 FS-TEST-RACE-BAD !
   FS-TEST-FREE-FD {: before:n :}
   before 0 >= FS-TEST-TRUE
   ['] FS-TEST-RACE-WORK FS-TEST-RACE-TASK TASK:ACTIVATE
   4000 0 do
      i 1 and 0<> if [: FS-TEST-RACE-LIVE s" x" APPEND-FILE ;] catch
      else [: FS-TEST-RACE-LIVE s" x" WRITE-ALL ;] catch then
      dup E-FS-OPEN = if 1 FS-TEST-RACE-REFUSED +! drop
      else 0<> if 1 FS-TEST-RACE-BAD +! then then
   loop
   1 FS-TEST-RACE-STOP atomic!
   FS-TEST-RACE-TASK TASK:JOIN
   MATCH result
      ok OF 0 FS-TEST= ENDOF
      err OF 0 FS-TEST= ENDOF
   ;MATCH
   FS-TEST-RACE-SWAPS atomic@ 0 > FS-TEST-TRUE
   FS-TEST-RACE-REFUSED @ 0 > FS-TEST-TRUE
   FS-TEST-RACE-BAD @ 0 FS-TEST=
   FS-TEST-FREE-FD before FS-TEST=
   FS-TEST-RACE-LIVE REMOVE-FILE ;

: FS-TEST-MAIN ( -- )
   FS-TEST-SETUP
   FS-TEST-PREPARE-FIXTURE
   FS-TEST-INTERNALS
   FS-TEST-PATHS
   FS-TEST-FILE-DIR
   FS-TEST-EXECUTABLE
   FS-TEST-FILE-SIZE
   FS-TEST-FILE-META
   FS-TEST-BASENAME
   FS-TEST-JOIN
   FS-TEST-WALK
   FS-TEST-IO
   FS-TEST-PARALLEL
   FS-TEST-RACED-REFUSAL
   FS-TEST-THROWS
   FS-TEST-CLEANUP
   FS-TEST-REPORT ;

FS-TEST-MAIN

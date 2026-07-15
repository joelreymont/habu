\ fs-test.f - focused tests for checked stdlib filesystem helpers.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f lib/fs-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require test/checker-assert.f

1 constant FS-TEST-EX-FAIL
-61 constant FS-TEST-STAT-RC
-62 constant FS-TEST-READ-RC
-63 constant FS-TEST-CLOSE-RC
$34 constant FS-TEST-U16-LO
$12 constant FS-TEST-U16-HI
$0807060504030201 constant FS-TEST-U64-VALUE
$78 constant FS-TEST-FILL-C
3 constant FS-TEST-WRITE-LEN
4 constant FS-TEST-EXACT-CAP
8 constant FS-TEST-READ-CAP
40 constant FS-TEST-DEEP-LIMIT
292 constant FS-TEST-MODE-READONLY

variable FS-TEST-CASE
variable FS-TEST-FAIL
variable FS-TEST-WALK-COUNT
variable FS-TEST-DEEP-IDX
variable FS-TEST-AFTER-IDX
variable FS-TEST-BETA-IDX
variable FS-TEST-FILE-COUNT
variable FS-TEST-FD
variable FS-TEST-BASE-U
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
variable FS-TEST-LINK-U
variable FS-TEST-BROKEN-U
variable FS-TEST-MISSING-U
variable FS-TEST-RACE-U
variable FS-TEST-STREAM-U
variable FS-TEST-STREAM-N
variable FS-TEST-CLOSE-N

create FS-TEST-OUT FS-PATH-CAP allot
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
create FS-TEST-LINK-BUF FS-PATH-CAP allot
create FS-TEST-BROKEN-BUF FS-PATH-CAP allot
create FS-TEST-MISSING-BUF FS-PATH-CAP allot
create FS-TEST-RACE-BUF FS-PATH-CAP allot
create FS-TEST-STREAM-BUF FS-TEST-READ-CAP allot
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

: FS-TEST-FILL ( ptr u8 n n -- ) {: dst:ptr u c :}
   0 begin dup u < while
      c dst over + c!
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

: FS-TEST-LINK-PATH ( -- ptr u8 n )
   FS-TEST-LINK-BUF FS-TEST-LINK-U @ ;

: FS-TEST-BROKEN-PATH ( -- ptr u8 n )
   FS-TEST-BROKEN-BUF FS-TEST-BROKEN-U @ ;

: FS-TEST-MISSING-PATH ( -- ptr u8 n )
   FS-TEST-MISSING-BUF FS-TEST-MISSING-U @ ;

: FS-TEST-RACE-PATH ( -- ptr u8 n )
   FS-TEST-RACE-BUF FS-TEST-RACE-U @ ;

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
   FS-TEST-BASE s" empty.txt" FS-TEST-EMPTY-BUF FS-TEST-EMPTY-U FS-TEST-PATH!
   FS-TEST-BASE s" file-link" FS-TEST-LINK-BUF FS-TEST-LINK-U FS-TEST-PATH!
   FS-TEST-BASE s" broken-link" FS-TEST-BROKEN-BUF FS-TEST-BROKEN-U FS-TEST-PATH!
   FS-TEST-BASE s" missing-target" FS-TEST-MISSING-BUF FS-TEST-MISSING-U FS-TEST-PATH!
   FS-TEST-BASE s" race.txt" FS-TEST-RACE-BUF FS-TEST-RACE-U FS-TEST-PATH! ;

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
   FS-TEST-BASE CLEANUP-TREE+
   FS-TEST-MAKE-WALK-DIRS
   FS-TEST-WRITE-WALK-FILES
   FS-TEST-BIG-PATH s" abcd" WRITE-ALL
   FS-TEST-BIG-PATH CLEANUP+
   FS-TEST-EMPTY-PATH s" " WRITE-ALL
   FS-TEST-EMPTY-PATH CLEANUP+
   FS-TEST-BIG-PATH FS-TEST-LINK-PATH MAKE-SYMLINK
   FS-TEST-MISSING-PATH FS-TEST-BROKEN-PATH MAKE-SYMLINK
   FS-TEST-RACE-PATH s" old" WRITE-ALL
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

\ Test-only package reopen: exercise the private injected operation row without
\ publishing it from lib/fs.f.
package FS
private

: TEST-READ-FAIL ( fd ptr u8 n -- n )
   2drop drop FS-TEST-READ-RC ;

: TEST-STAT-FAIL ( fd ptr u8 -- rc )
   2drop FS-TEST-STAT-RC >RC ;

: TEST-CLOSE-FAIL ( fd -- rc )
   CLOSE-FD drop FS-TEST-CLOSE-RC >RC ;

: TEST-CLOSE-COUNT ( fd -- rc )
   FS-TEST-CLOSE-N @ 1 + FS-TEST-CLOSE-N !
   CLOSE-FD ;

: TEST-CLOSE-FAIL-COUNT ( fd -- rc )
   FS-TEST-CLOSE-N @ 1 + FS-TEST-CLOSE-N !
   TEST-CLOSE-FAIL ;

: TEST-RACE-OPEN ( ptr u8 -- fd )
   drop
   FS-TEST-RACE-PATH REMOVE-FILE
   FS-TEST-BIG-PATH FS-TEST-RACE-PATH MAKE-SYMLINK
   FS-TEST-RACE-PATH FS-PATHZ OPEN-NOFOLLOW ;

public

: TEST-INJECT-STAT ( -- stream-outcome )
   FS-TEST-BIG-PATH FS-TEST-READ-BUF 2
   [: 2drop ;]
   [: OPEN-NOFOLLOW ;]
   [: TEST-STAT-FAIL ;]
   [: READ-FD ;]
   [: CLOSE-FD ;]
   STREAM-WITH ;

: TEST-INJECT-STAT-CLOSE ( -- stream-outcome )
   FS-TEST-BIG-PATH FS-TEST-READ-BUF 2
   [: 2drop ;]
   [: OPEN-NOFOLLOW ;]
   [: TEST-STAT-FAIL ;]
   [: READ-FD ;]
   [: TEST-CLOSE-FAIL ;]
   STREAM-WITH ;

: TEST-INJECT-READ ( -- stream-outcome )
   FS-TEST-BIG-PATH FS-TEST-READ-BUF 2
   [: 2drop ;]
   [: OPEN-NOFOLLOW ;]
   [: FSTAT-FD ;]
   [: TEST-READ-FAIL ;]
   [: CLOSE-FD ;]
   STREAM-WITH ;

: TEST-INJECT-READ-CLOSE ( -- stream-outcome )
   FS-TEST-BIG-PATH FS-TEST-READ-BUF 2
   [: 2drop ;]
   [: OPEN-NOFOLLOW ;]
   [: FSTAT-FD ;]
   [: TEST-READ-FAIL ;]
   [: TEST-CLOSE-FAIL ;]
   STREAM-WITH ;

: TEST-INJECT-CLOSE ( -- stream-outcome )
   FS-TEST-BIG-PATH FS-TEST-READ-BUF 2
   [: 2drop ;]
   [: OPEN-NOFOLLOW ;]
   [: FSTAT-FD ;]
   [: READ-FD ;]
   [: TEST-CLOSE-FAIL ;]
   STREAM-WITH ;

: TEST-INJECT-CALLBACK ( -- stream-outcome )
   0 FS-TEST-CLOSE-N !
   FS-TEST-BIG-PATH FS-TEST-READ-BUF 2
   [: 2drop FS-TEST-EX-FAIL throw ;]
   [: OPEN-NOFOLLOW ;]
   [: FSTAT-FD ;]
   [: READ-FD ;]
   [: TEST-CLOSE-COUNT ;]
   STREAM-WITH ;

: TEST-INJECT-CALLBACK-CLOSE ( -- stream-outcome )
   0 FS-TEST-CLOSE-N !
   FS-TEST-BIG-PATH FS-TEST-READ-BUF 2
   [: 2drop FS-TEST-EX-FAIL throw ;]
   [: OPEN-NOFOLLOW ;]
   [: FSTAT-FD ;]
   [: READ-FD ;]
   [: TEST-CLOSE-FAIL-COUNT ;]
   STREAM-WITH ;

: TEST-OPEN-RACE ( -- stream-outcome )
   FS-TEST-RACE-PATH FS-TEST-READ-BUF 2
   [: 2drop ;]
   [: TEST-RACE-OPEN ;]
   [: FSTAT-FD ;]
   [: READ-FD ;]
   [: CLOSE-FD ;]
   STREAM-WITH ;

;package

: FS-TEST-EXPECT-OK ( FS:stream-outcome -- )
   MATCH FS:stream-outcome
      ok OF FS-TRUE ENDOF
      failed OF drop FS-FALSE ENDOF
      close-failed OF drop FS-FALSE ENDOF
      failed-close OF 2drop FS-FALSE ENDOF
   ;MATCH FS-TEST-TRUE ;

: FS-TEST-CONSUME ( FS:stream-outcome -- )
   MATCH FS:stream-outcome
      ok OF ENDOF
      failed OF drop ENDOF
      close-failed OF drop ENDOF
      failed-close OF 2drop ENDOF
   ;MATCH ;

: FS-TEST-EXPECT-FAILED ( FS:stream-outcome n -- ) {: want:n :}
   MATCH FS:stream-outcome
      ok OF FS-FALSE ENDOF
      failed OF want = ENDOF
      close-failed OF drop FS-FALSE ENDOF
      failed-close OF 2drop FS-FALSE ENDOF
   ;MATCH FS-TEST-TRUE ;

: FS-TEST-EXPECT-CLOSE ( FS:stream-outcome n -- ) {: want:n :}
   MATCH FS:stream-outcome
      ok OF FS-FALSE ENDOF
      failed OF drop FS-FALSE ENDOF
      close-failed OF RC>N want = ENDOF
      failed-close OF 2drop FS-FALSE ENDOF
   ;MATCH FS-TEST-TRUE ;

: FS-TEST-EXPECT-BOTH ( FS:stream-outcome n n -- )
   {: primary:n close:n :}
   MATCH FS:stream-outcome
      ok OF FS-FALSE ENDOF
      failed OF drop FS-FALSE ENDOF
      close-failed OF drop FS-FALSE ENDOF
      failed-close OF
         {: got:n crc:rc :}
         got primary = crc RC>N close = and
      ENDOF
   ;MATCH FS-TEST-TRUE ;

: FS-TEST-STREAM-RESET ( -- )
   0 FS-TEST-STREAM-U !
   0 FS-TEST-STREAM-N ! ;

\ typed-local-lint: allow-bare-local - src keeps the callback's ptr u8 role.
: FS-TEST-STREAM-CHUNK ( ptr u8 n -- ) {: src u:n :}
   FS-TEST-STREAM-U @ u + FS-TEST-READ-CAP > if E-FS-CAPACITY throw then
   src FS-TEST-STREAM-BUF FS-TEST-STREAM-U @ + u BYTE-COPY
   FS-TEST-STREAM-U @ u + FS-TEST-STREAM-U !
   FS-TEST-STREAM-N @ 1 + FS-TEST-STREAM-N ! ;

: FS-TEST-STREAM-MISSING ( -- )
   FS-TEST-MISSING-PATH FS-TEST-READ-BUF 2
   [: 2drop ;] FS:STREAM-REGULAR FS-TEST-CONSUME ;

: FS-TEST-STREAM-LINK ( -- )
   FS-TEST-LINK-PATH FS-TEST-READ-BUF 2
   [: 2drop ;] FS:STREAM-REGULAR FS-TEST-CONSUME ;

: FS-TEST-STREAM-BROKEN ( -- )
   FS-TEST-BROKEN-PATH FS-TEST-READ-BUF 2
   [: 2drop ;] FS:STREAM-REGULAR FS-TEST-CONSUME ;

: FS-TEST-STREAM-DIR ( -- FS:stream-outcome )
   FS-TEST-ROOT FS-TEST-READ-BUF 2 [: 2drop ;] FS:STREAM-REGULAR ;

: FS-TEST-STREAM-ZERO-BUF ( -- )
   FS-TEST-BIG-PATH FS-TEST-READ-BUF 0
   [: 2drop ;] FS:STREAM-REGULAR FS-TEST-CONSUME ;

: FS-TEST-OPEN-RACE ( -- )
   FS:TEST-OPEN-RACE FS-TEST-CONSUME ;

: FS-TEST-STREAMING ( -- )
   FS-TEST-STREAM-RESET
   FS-TEST-BIG-PATH FS-TEST-READ-BUF 2
   [: FS-TEST-STREAM-CHUNK ;] FS:STREAM-REGULAR FS-TEST-EXPECT-OK
   FS-TEST-STREAM-U @ FS-TEST-EXACT-CAP FS-TEST=
   FS-TEST-STREAM-N @ 1 > FS-TEST-TRUE
   FS-TEST-STREAM-BUF FS-TEST-STREAM-U @ s" abcd" FS-TEST$=
   FS-TEST-STREAM-RESET
   FS-TEST-EMPTY-PATH FS-TEST-READ-BUF 2
   [: FS-TEST-STREAM-CHUNK ;] FS:STREAM-REGULAR FS-TEST-EXPECT-OK
   FS-TEST-STREAM-U @ 0 FS-TEST=
   FS-TEST-STREAM-N @ 0 FS-TEST= ;

: FS-TEST-NOFOLLOW-ERRORS ( -- )
   T-RESET
   [: FS-TEST-STREAM-LINK ;] E-FS-OPEN TTHROWSQ
   [: FS-TEST-STREAM-BROKEN ;] E-FS-OPEN TTHROWSQ
   FS-TEST-STREAM-DIR E-FS-STAT FS-TEST-EXPECT-FAILED
   [: FS-TEST-STREAM-MISSING ;] E-FS-OPEN TTHROWSQ
   [: FS-TEST-STREAM-ZERO-BUF ;] E-FS-CAPACITY TTHROWSQ
   FS:TEST-INJECT-STAT E-FS-STAT FS-TEST-EXPECT-FAILED
   FS:TEST-INJECT-READ E-FS-IO FS-TEST-EXPECT-FAILED
   FS:TEST-INJECT-CALLBACK FS-TEST-EX-FAIL FS-TEST-EXPECT-FAILED
   FS-TEST-CLOSE-N @ 1 FS-TEST=
   FS:TEST-INJECT-CLOSE FS-TEST-CLOSE-RC FS-TEST-EXPECT-CLOSE
   FS:TEST-INJECT-STAT-CLOSE E-FS-STAT FS-TEST-CLOSE-RC FS-TEST-EXPECT-BOTH
   FS:TEST-INJECT-READ-CLOSE E-FS-IO FS-TEST-CLOSE-RC FS-TEST-EXPECT-BOTH
   FS:TEST-INJECT-CALLBACK-CLOSE
      FS-TEST-EX-FAIL FS-TEST-CLOSE-RC FS-TEST-EXPECT-BOTH
   FS-TEST-CLOSE-N @ 1 FS-TEST=
   [: FS-TEST-OPEN-RACE ;] E-FS-OPEN TTHROWSQ
   FS-TEST-RACE-PATH SYMLINK? FS-TEST-TRUE
   T-FAILURES 0 FS-TEST= ;

: FS-TEST-NOFOLLOW-TYPES ( -- )
   s" FSP ( ptr u8 n ptr u8 n [ ptr u8 n -- ] -- FS:stream-outcome ) FS:STREAM-REGULAR"
      CHECK-QUIET-CANDIDATE! -1 FS-TEST=
   s" FSN ( ptr u8 n ptr u8 n [ ptr u8 -- ] -- FS:stream-outcome ) FS:STREAM-REGULAR"
      CHECK-QUIET-CANDIDATE! 0 FS-TEST= ;

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
   FS-TEST-STREAMING
   FS-TEST-NOFOLLOW-ERRORS
   FS-TEST-NOFOLLOW-TYPES
   FS-TEST-THROWS
   FS-TEST-CLEANUP
   FS-TEST-REPORT ;

FS-TEST-MAIN

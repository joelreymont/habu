\ fs-test.f - focused tests for checked stdlib filesystem helpers.
\ Run: bin/hb --load src/lib/error.f src/lib/string.f src/lib/test.f
\   src/lib/fs.f src/lib/fs-mutate.f src/lib/fs-test.f
\ bin/hb is this tree's engine (docs/bootstrap.md): an engine carries fs.f and
\ fs-mutate.f, so --load under another engine keeps that engine's FS.
\ The FIFO rows require mkfifo and cat on PATH.

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/memory.f                \ MEM-MAX-N for the join overflow rows
require src/lib/task.f                  \ two tasks through FS:READ-ALL / FS:FILE-SIZE at once
require src/lib/adt/result.f            \ the join's answer is MATCHed here
require src/lib/fmt.f                   \ FMT:.INT - report numbers inline
require src/lib/process.f               \ the FIFO rows' reader child and its pipe
require src/lib/process-argv.f
require src/lib/process-env.f           \ RESOLVE-EXECUTABLE finds mkfifo and cat
require src/lib/fd.f                    \ FD:TRY-FIFO-WRITER, WRITE-FULL, READ-EXACT
require src/lib/ffi-abi.f

PROCESS-SYMBOLS
FUNCTION: ACCESS-CALL access ( ptr u8 n -- i32 ) ;FUNCTION

$4000 constant FS-TEST-S-IFDIR             \ S_IFDIR on both targets
2 constant FS-TEST-ENOENT                   \ ENOENT on both targets
1 constant FS-TEST-EX-FAIL
$78 constant FS-TEST-FILL-C
3 constant FS-TEST-WRITE-LEN
4 constant FS-TEST-EXACT-CAP
8 constant FS-TEST-READ-CAP
40 constant FS-TEST-DEEP-LIMIT
292 constant FS-TEST-MODE-READONLY
420 constant FS-TEST-MODE-UNSEARCHABLE   \ 0644: read, no search
493 constant FS-TEST-MODE-DIR            \ 0755
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
variable FS-TEST-LEAF-N
variable FS-TEST-LEAF-U
variable FS-TEST-NEST-N
variable FS-TEST-AFTER-IDX
variable FS-TEST-BETA-IDX
variable FS-TEST-FILE-COUNT
variable FS-TEST-GONE-N
variable FS-TEST-FD
variable FS-TEST-BASE-U
variable FS-TEST-PAR-A-U
variable FS-TEST-PAR-B-U
variable FS-TEST-PAR-BAD
variable FS-TEST-PAR-READY
variable FS-TEST-WA-U
variable FS-TEST-WB-U
variable FS-TEST-WB-SUB-U
variable FS-TEST-ROOT-U
variable FS-TEST-ALPHA-U
variable FS-TEST-CHILD-U
variable FS-TEST-ROOT-GIT-U
variable FS-TEST-ROOT-JJ-U
variable FS-TEST-ROOT-DOTS-U
variable FS-TEST-ALPHA-GIT-U
variable FS-TEST-CHILD-OUT-U
variable FS-TEST-DEEP-U
variable FS-TEST-DEEP-CUR-U
variable FS-TEST-IO-U
variable FS-TEST-BIG-U
variable FS-TEST-EMPTY-U
variable FS-TEST-RANGE-U
variable FS-TEST-LINKS-U
variable FS-TEST-CAP-U
variable FS-TEST-CAP-ROOT-U
variable FS-TEST-CAP-SEEN

create FS-TEST-OUT PATH-CAP allot
create FS-TEST-PAR-A-BUF PATH-CAP allot
create FS-TEST-PAR-B-BUF PATH-CAP allot
create FS-TEST-PAR-SEED FS-TEST-PAR-CAP allot
create FS-TEST-PAR-A-DATA FS-TEST-PAR-CAP allot
create FS-TEST-PAR-B-DATA FS-TEST-PAR-CAP allot
create FS-TEST-BASE-BUF PATH-CAP allot
create FS-TEST-WA-BUF PATH-CAP allot
create FS-TEST-WB-BUF PATH-CAP allot
create FS-TEST-WB-SUB-BUF PATH-CAP allot
create FS-TEST-ROOT-BUF PATH-CAP allot
create FS-TEST-ALPHA-BUF PATH-CAP allot
create FS-TEST-CHILD-BUF PATH-CAP allot
create FS-TEST-ROOT-GIT-BUF PATH-CAP allot
create FS-TEST-ROOT-JJ-BUF PATH-CAP allot
create FS-TEST-ROOT-DOTS-BUF PATH-CAP allot
create FS-TEST-ALPHA-GIT-BUF PATH-CAP allot
create FS-TEST-CHILD-OUT-BUF PATH-CAP allot
create FS-TEST-DEEP-BUF PATH-CAP allot
create FS-TEST-DEEP-CUR-BUF PATH-CAP allot
create FS-TEST-IO-BUF PATH-CAP allot
create FS-TEST-BIG-BUF PATH-CAP allot
create FS-TEST-EMPTY-BUF PATH-CAP allot
create FS-TEST-RANGE-BUF PATH-CAP allot
create FS-TEST-LINKS-BUF PATH-CAP allot
create FS-TEST-LONG PATH-CAP 1 + allot
create FS-TEST-CAP-PATH PATH-CAP 1 + allot
create FS-TEST-OS-PATH PATH-CAP 2 + allot
create FS-TEST-READ-BUF FS-TEST-READ-CAP allot

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

\ Fills dst[0,u) with c; FS-TEST-PARALLEL compares the bytes, so a wrong fill
\ fails there.
: FS-TEST-FILL ( ptr u8 n n -- ) {: dst:ptr u c :}
   0 begin dup u < while
      c over dst + c!
      1+
   repeat drop ;

: FS-TEST-JOIN$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: pa:ptr pu na:ptr nu :}
   pa pu na nu FS-TEST-OUT FS:JOIN-PATH
   FS-TEST-OUT swap ;

: FS-TEST-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: FS-TEST-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu na:ptr nu dst:ptr lenp:ptr :}
   pa pu na nu dst FS:JOIN-PATH lenp ! ;

: FS-TEST-BASE ( -- ptr u8 n )
   FS-TEST-BASE-BUF FS-TEST-BASE-U @ ;

: FS-TEST-ROOT ( -- ptr u8 n )
   FS-TEST-ROOT-BUF FS-TEST-ROOT-U @ ;

\ The two trees the two-context rows walk: separate roots, so a path from one
\ walk can never pass the other's prefix check.
: FS-TEST-WA ( -- ptr u8 n )
   FS-TEST-WA-BUF FS-TEST-WA-U @ ;

: FS-TEST-WB ( -- ptr u8 n )
   FS-TEST-WB-BUF FS-TEST-WB-U @ ;

: FS-TEST-WB-SUB ( -- ptr u8 n )
   FS-TEST-WB-SUB-BUF FS-TEST-WB-SUB-U @ ;

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

: FS-TEST-CHILD-OUT ( -- ptr u8 n )
   FS-TEST-CHILD-OUT-BUF FS-TEST-CHILD-OUT-U @ ;

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

: FS-TEST-RANGE-PATH ( -- ptr u8 n )
   FS-TEST-RANGE-BUF FS-TEST-RANGE-U @ ;

: FS-TEST-LINKS-PATH ( -- ptr u8 n )
   FS-TEST-LINKS-BUF FS-TEST-LINKS-U @ ;

: FS-TEST-BASE! ( -- )
   s" habu-fs" FS:MKTEMP-DIR {: a:ptr u :}
   a u FS-TEST-BASE-BUF FS-TEST-BASE-U FS-TEST-COPY! ;

: FS-TEST-PATHS! ( -- )
   FS-TEST-BASE s" root" FS-TEST-ROOT-BUF FS-TEST-ROOT-U FS-TEST-PATH!
   FS-TEST-ROOT s" alpha" FS-TEST-ALPHA-BUF FS-TEST-ALPHA-U FS-TEST-PATH!
   FS-TEST-ALPHA s" aa-child" FS-TEST-CHILD-BUF FS-TEST-CHILD-U FS-TEST-PATH!
   FS-TEST-ROOT s" .git" FS-TEST-ROOT-GIT-BUF FS-TEST-ROOT-GIT-U FS-TEST-PATH!
   FS-TEST-ROOT s" .jj" FS-TEST-ROOT-JJ-BUF FS-TEST-ROOT-JJ-U FS-TEST-PATH!
   FS-TEST-ROOT s" .dots" FS-TEST-ROOT-DOTS-BUF FS-TEST-ROOT-DOTS-U FS-TEST-PATH!
   FS-TEST-ALPHA s" .git" FS-TEST-ALPHA-GIT-BUF FS-TEST-ALPHA-GIT-U FS-TEST-PATH!
   FS-TEST-CHILD s" .out" FS-TEST-CHILD-OUT-BUF FS-TEST-CHILD-OUT-U FS-TEST-PATH!
   FS-TEST-BASE s" walk-a" FS-TEST-WA-BUF FS-TEST-WA-U FS-TEST-PATH!
   FS-TEST-BASE s" walk-b" FS-TEST-WB-BUF FS-TEST-WB-U FS-TEST-PATH!
   FS-TEST-WB s" sub" FS-TEST-WB-SUB-BUF FS-TEST-WB-SUB-U FS-TEST-PATH!
   FS-TEST-BASE s" deep" FS-TEST-DEEP-BUF FS-TEST-DEEP-U FS-TEST-PATH!
   FS-TEST-BASE s" io.txt" FS-TEST-IO-BUF FS-TEST-IO-U FS-TEST-PATH!
   FS-TEST-BASE s" big.txt" FS-TEST-BIG-BUF FS-TEST-BIG-U FS-TEST-PATH!
   FS-TEST-BASE s" empty.txt" FS-TEST-EMPTY-BUF FS-TEST-EMPTY-U FS-TEST-PATH!
   FS-TEST-BASE s" range.txt" FS-TEST-RANGE-BUF FS-TEST-RANGE-U FS-TEST-PATH!
   FS-TEST-BASE s" links.txt" FS-TEST-LINKS-BUF FS-TEST-LINKS-U FS-TEST-PATH! ;

: FS-TEST-MAKE-REGISTERED-DIR ( ptr u8 n -- )
   2dup FS:MAKE-DIR FS:CLEANUP-DIR+ ;

: FS-TEST-WRITE-PLAIN ( ptr u8 n ptr u8 n ptr u8 n -- n ) {: pa:ptr pu na:ptr nu data:ptr datau :}
   pa pu na nu FS-TEST-OUT FS:JOIN-PATH {: pathu :}
   FS-TEST-OUT pathu data datau FS:WRITE-ALL
   pathu ;

\ Each child file is registered for removal at exit. The two walk trees the
\ concurrent rows own are not: FS-TEST-CLEANUP removes them by hand.
: FS-TEST-WRITE-CHILD ( ptr u8 n ptr u8 n ptr u8 n -- )
   FS-TEST-WRITE-PLAIN FS-TEST-OUT swap FS:CLEANUP+ ;

: FS-TEST-MAKE-WALK-DIRS ( -- )
   FS-TEST-ROOT FS-TEST-MAKE-REGISTERED-DIR
   FS-TEST-ALPHA FS-TEST-MAKE-REGISTERED-DIR
   FS-TEST-CHILD FS-TEST-MAKE-REGISTERED-DIR
   FS-TEST-ROOT-GIT FS-TEST-MAKE-REGISTERED-DIR
   FS-TEST-ROOT-JJ FS-TEST-MAKE-REGISTERED-DIR
   FS-TEST-ROOT-DOTS FS-TEST-MAKE-REGISTERED-DIR
   FS-TEST-ALPHA-GIT FS-TEST-MAKE-REGISTERED-DIR
   FS-TEST-CHILD-OUT FS-TEST-MAKE-REGISTERED-DIR ;

: FS-TEST-MAKE-WALK-TREES ( -- )
   FS-TEST-WA FS:MAKE-DIR
   FS-TEST-WB FS:MAKE-DIR
   FS-TEST-WB-SUB FS:MAKE-DIR ;

: FS-TEST-WRITE-WALK-FILES ( -- )
   FS-TEST-CHILD s" deep.txt" s" " FS-TEST-WRITE-CHILD
   FS-TEST-ALPHA s" zz-after.txt" s" " FS-TEST-WRITE-CHILD
   FS-TEST-ROOT s" beta.txt" s" " FS-TEST-WRITE-CHILD
   FS-TEST-ROOT-GIT s" ignored.txt" s" " FS-TEST-WRITE-CHILD
   FS-TEST-ROOT-JJ s" ignored.txt" s" " FS-TEST-WRITE-CHILD
   FS-TEST-ROOT-DOTS s" ignored.txt" s" " FS-TEST-WRITE-CHILD
   FS-TEST-ALPHA-GIT s" ignored.txt" s" " FS-TEST-WRITE-CHILD
   FS-TEST-CHILD-OUT s" ignored.txt" s" " FS-TEST-WRITE-CHILD
   FS-TEST-ALPHA s" dangling" FS-TEST-OUT FS:JOIN-PATH {: linku :}
   s" no-target" FS-TEST-OUT linku FS:MAKE-SYMLINK
   FS-TEST-OUT linku FS:CLEANUP+
   FS-TEST-ALPHA s" loop" FS-TEST-OUT FS:JOIN-PATH {: loopu :}
   s" loop" FS-TEST-OUT loopu FS:MAKE-SYMLINK
   FS-TEST-OUT loopu FS:CLEANUP+ ;

: FS-TEST-WRITE-WALK-TREE-FILES ( -- )
   FS-TEST-WA s" a1.txt" s" " FS-TEST-WRITE-PLAIN drop
   FS-TEST-WA s" a2.txt" s" " FS-TEST-WRITE-PLAIN drop
   FS-TEST-WB s" b1.txt" s" " FS-TEST-WRITE-PLAIN drop
   FS-TEST-WB s" b2.txt" s" " FS-TEST-WRITE-PLAIN drop
   FS-TEST-WB-SUB s" b3.txt" s" " FS-TEST-WRITE-PLAIN drop ;

: FS-TEST-DEEP-COMP ( n -- ptr u8 n )
   SB-RESET
   s" d" SB-APPEND
   FMT:SB-U
   SB$ ;

: FS-TEST-DEEP-STEP ( n -- ) {: idx :}
   idx FS-TEST-DEEP-COMP {: ca:ptr cu :}
   FS-TEST-DEEP-CUR ca cu FS-TEST-OUT FS:JOIN-PATH {: u :}
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
   FS:CLEANUP-RESET
   FS-TEST-BASE!
   FS-TEST-PATHS!
   FS-TEST-BASE FS:CLEANUP-DIR+
   FS-TEST-MAKE-WALK-DIRS
   FS-TEST-WRITE-WALK-FILES
   FS-TEST-MAKE-WALK-TREES
   FS-TEST-WRITE-WALK-TREE-FILES
   FS-TEST-BIG-PATH s" abcd" FS:WRITE-ALL
   FS-TEST-BIG-PATH FS:CLEANUP+
   FS-TEST-EMPTY-PATH s" " FS:WRITE-ALL
   FS-TEST-EMPTY-PATH FS:CLEANUP+
   FS-TEST-IO-PATH FS:CLEANUP+
   FS-TEST-RANGE-PATH FS:CLEANUP+
   FS-TEST-LINKS-PATH FS:CLEANUP+
   FS-TEST-MAKE-DEEP ;

: FS-TEST-PATH-TOO-LONG ( -- )
   FS-TEST-LONG PATH-CAP 1 + FS:>PATHZ drop ;

: FS-TEST-JOIN-TOO-LONG ( -- )
   FS-TEST-LONG PATH-CAP s" z" FS-TEST-OUT FS:JOIN-PATH drop ;

: FS-TEST-JOIN-NEG-PARENT ( -- )
   s" root" drop -1 s" child" FS-TEST-OUT FS:JOIN-PATH drop ;

: FS-TEST-JOIN-NEG-NAME ( -- )
   s" root" s" child" drop -1 FS-TEST-OUT FS:JOIN-PATH drop ;

\ A long name joined to a short parent, `extra` bytes past an exact fill.
: FS-TEST-JOIN-NAME-FILL ( n -- n ) {: extra :}
   s" root" {: pa:ptr pu :}
   pa pu FS-TEST-LONG PATH-CAP pu - 1 - extra + FS-TEST-OUT FS:JOIN-PATH ;

\ A negative length is malformed whatever the other length is. Each pair sums
\ to exactly PATH-CAP, so only the sign can refuse it.
: FS-TEST-JOIN-NEG-PARENT-LONG-NAME ( -- )
   s" root" drop -2 FS-TEST-LONG PATH-CAP 1 + FS-TEST-OUT FS:JOIN-PATH drop ;

: FS-TEST-JOIN-LONG-PARENT-NEG-NAME ( -- )
   FS-TEST-LONG PATH-CAP 1 + s" child" drop -2 FS-TEST-OUT FS:JOIN-PATH drop ;

\ A length near the maximum cell must meet the join's own capacity rule, not
\ wrap the summed length back under PATH-CAP.
: FS-TEST-JOIN-HUGE-NAME ( -- )
   s" root" FS-TEST-LONG MEM-MAX-N FS-TEST-OUT FS:JOIN-PATH drop ;

: FS-TEST-JOIN-HUGE-PARENT ( -- )
   FS-TEST-LONG MEM-MAX-N s" z" FS-TEST-OUT FS:JOIN-PATH drop ;

: FS-TEST-JOIN-LONG-PARENT ( -- )
   FS-TEST-LONG PATH-CAP 1 + s" z" FS-TEST-OUT FS:JOIN-PATH drop ;

\ "ab/cd" is five bytes, one past the span.
: FS-TEST-JOIN-INTO-SHORT ( -- )
   s" ab" s" cd" FS-TEST-OUT 4 SPAN:MAKE FS:JOIN-PATH-INTO drop ;

: FS-TEST-MISSING-STAT ( -- )
   s" no-such-habu-fs-path-for-stdlib-test" FS:STAT-MODE drop ;

: FS-TEST-MISSING-FILE-SIZE ( -- )
   s" no-such-habu-fs-path-for-stdlib-test" FS:FILE-SIZE drop ;

: FS-TEST-DIR-FILE-SIZE ( -- )
   FS-TEST-ROOT FS:FILE-SIZE drop ;

: FS-TEST-MISSING-WALK ( -- )
   s" no-such-habu-fs-path-for-stdlib-test" [: 2drop ;] FS:WALK-FILES ;

: FS-TEST-UNDER$ ( ptr u8 n -- ptr u8 n )
   {: na:ptr nu :}
   FS-TEST-BASE na nu FS-TEST-JOIN$ ;

\ kept.txt's stat in a directory that can be read but not searched is EACCES.
: FS-TEST-UNSEARCHABLE-WALK ( -- )
   s" locked-walk" FS-TEST-UNDER$ 2dup FS:MAKE-DIR FS:CLEANUP-TREE+
   s" locked-walk/locked" FS-TEST-UNDER$ FS:MAKE-DIR
   s" locked-walk/locked/kept.txt" FS-TEST-UNDER$ s" k" FS:WRITE-ALL
   s" locked-walk/locked" FS-TEST-UNDER$
   FS-TEST-MODE-UNSEARCHABLE FS:CHMOD-MODE
   [: s" locked-walk" FS-TEST-UNDER$ [: 2drop ;] FS:WALK-FILES ;]
   E-FS-STAT TTHROWSQ
   FS:ERRNO FS:EACCES FS-TEST=
   s" locked-walk/locked" FS-TEST-UNDER$ FS-TEST-MODE-DIR FS:CHMOD-MODE ;

: FS-TEST-WALK-ROOT-TOO-LONG ( -- )
   FS-TEST-LONG PATH-CAP 1 + [: 2drop ;] FS:WALK-FILES ;

: FS-TEST-MISSING-READ ( -- )
   s" no-such-habu-fs-read-file" FS-TEST-READ-BUF FS-TEST-READ-CAP FS:READ-ALL drop ;

: FS-TEST-READ-TOO-LARGE ( -- )
   FS-TEST-BIG-PATH FS-TEST-READ-BUF FS-TEST-WRITE-LEN FS:READ-ALL drop ;

: FS-TEST-WRITE-DIR ( -- )
   FS-TEST-ROOT s" x" FS:WRITE-ALL ;

: FS-TEST-APPEND-DIR ( -- )
   FS-TEST-ROOT s" x" FS:APPEND-FILE ;

: FS-TEST-WRITE-MISSING-PARENT ( -- )
   FS-TEST-ROOT s" missing-parent/out.txt" FS-TEST-OUT FS:JOIN-PATH
   FS-TEST-OUT swap s" x" FS:WRITE-ALL ;

: FS-TEST-APPEND-MISSING-PARENT ( -- )
   FS-TEST-ROOT s" missing-parent/out.txt" FS-TEST-OUT FS:JOIN-PATH
   FS-TEST-OUT swap s" x" FS:APPEND-FILE ;

: FS-TEST-WRITE-READONLY ( -- )
   FS-TEST-IO-PATH FS-TEST-MODE-READONLY FS:CHMOD-MODE
   FS-TEST-IO-PATH s" x" FS:WRITE-ALL ;

: FS-TEST-SETUP ( -- )
   FS-TEST-LONG PATH-CAP 1 + FS-TEST-FILL-C FS-TEST-FILL
   0 FS-TEST-WALK-COUNT !
   0 FS-TEST-DEEP-IDX !
   0 FS-TEST-AFTER-IDX !
   0 FS-TEST-BETA-IDX !
   0 FS-TEST-FILE-COUNT ! ;

: FS-TEST-PATHS ( -- )
   s" AGENTS.md" FS:EXISTS? FS-TEST-TRUE
   s" no-such-habu-fs-path-for-stdlib-test" FS:EXISTS? FS-TEST-FALSE
   s" no-such-habu-fs-path-for-stdlib-test" FS:FILE? FS-TEST-FALSE
   s" no-such-habu-fs-path-for-stdlib-test" FS:DIR? FS-TEST-FALSE ;

\ The OS takes a path of PATH-CAP bytes and refuses one byte more: ENAMETOOLONG.
: FS-TEST-OS-PATH-CAP ( -- )
   FS-TEST-OS-PATH PATH-CAP 2 + [char] / FS-TEST-FILL
   PATH-CAP 2 + 0 ?do [char] . FS-TEST-OS-PATH i + c! 2 +loop
   FS-TEST-OS-PATH PATH-CAP + c@
   0 FS-TEST-OS-PATH PATH-CAP + c!
   FS-TEST-OS-PATH 0 ACCESS-CALL 0 FS-TEST=
   FS-TEST-OS-PATH PATH-CAP + c!
   0 FS-TEST-OS-PATH PATH-CAP 1 + + c!
   FS-TEST-OS-PATH 0 ACCESS-CALL -1 FS-TEST=
   FFI:ERRNO FS:ENAMETOOLONG FS-TEST= ;

: FS-TEST-FILE-DIR ( -- )
   s" AGENTS.md" FS:FILE? FS-TEST-TRUE
   s" AGENTS.md" FS:DIR? FS-TEST-FALSE
   s" src" FS:DIR? FS-TEST-TRUE
   s" src" FS:FILE? FS-TEST-FALSE
   s" AGENTS.md" FS:STAT-MODE FS:S-IFMT and FS:S-IFREG = FS-TEST-TRUE
   s" src" FS:STAT-MODE FS:S-IFMT and FS-TEST-S-IFDIR = FS-TEST-TRUE ;

: FS-TEST-EXECUTABLE ( -- )
   s" bin/hb" FS:EXECUTABLE? FS-TEST-TRUE
   s" AGENTS.md" FS:EXECUTABLE? FS-TEST-FALSE
   s" no-such-habu-fs-path-for-stdlib-test" FS:EXECUTABLE? FS-TEST-FALSE ;

: FS-TEST-FILE-SIZE ( -- )
   FS-TEST-BIG-PATH FS:FILE-SIZE FS-TEST-EXACT-CAP FS-TEST=
   FS-TEST-EMPTY-PATH FS:FILE-SIZE 0 FS-TEST= ;

: FS-TEST-FILE-META ( -- )
   FS-TEST-BIG-PATH FS:FILE-META {: sz:n mt:n mn:n ct:n cn:n :}
   sz FS-TEST-EXACT-CAP FS-TEST=
   mt 0 > FS-TEST-TRUE
   mn 0 >= FS-TEST-TRUE
   ct 0 > FS-TEST-TRUE
   cn 0 >= FS-TEST-TRUE ;

: FS-TEST-BASENAME ( -- )
   s" file.f" FS:BASENAME s" file.f" FS-TEST$=
   s" src/core/checker.f" FS:BASENAME s" checker.f" FS-TEST$=
   s" src/" FS:BASENAME s" " FS-TEST$=
   s" /" FS:BASENAME s" " FS-TEST$=
   s" " FS:BASENAME s" " FS-TEST$= ;

: FS-TEST-JOIN ( -- )
   s" src" s" core" FS-TEST-JOIN$ s" src/core" FS-TEST$=
   s" src/" s" core" FS-TEST-JOIN$ s" src/core" FS-TEST$=
   s" /" s" tmp" FS-TEST-JOIN$ s" /tmp" FS-TEST$=
   FS-TEST-LONG PATH-CAP 2 - s" z" FS-TEST-OUT FS:JOIN-PATH
   PATH-CAP FS-TEST=
   0 FS-TEST-JOIN-NAME-FILL PATH-CAP FS-TEST= ;

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
   a u s" /.dots/ignored.txt" ENDS-WITH? FS-TEST-FALSE
   a u s" /.out/ignored.txt" ENDS-WITH? FS-TEST-FALSE ;

: FS-TEST-FILE-CB ( ptr u8 n -- ) {: a:ptr u :}
   FS-TEST-FILE-COUNT @ 1 + FS-TEST-FILE-COUNT !
   a u s" /beta.txt" ENDS-WITH? FS-TEST-TRUE ;

: FS-TEST-WALK ( -- )
   FS-TEST-ROOT [: FS-TEST-WALK-CB ;] FS:WALK-FILES
   FS-TEST-WALK-COUNT @ 3 FS-TEST=
   FS-TEST-DEEP-IDX @ 0 > FS-TEST-TRUE
   FS-TEST-AFTER-IDX @ 0 > FS-TEST-TRUE
   FS-TEST-BETA-IDX @ 0 > FS-TEST-TRUE
   FS-TEST-ROOT s" beta.txt" FS-TEST-OUT FS:JOIN-PATH
   FS-TEST-OUT swap [: FS-TEST-FILE-CB ;] FS:WALK-FILES
   FS-TEST-FILE-COUNT @ 1 FS-TEST= ;

: FS-TEST-GONE-CB ( ptr u8 n -- ) {: a:ptr u :}
   FS-TEST-GONE-N @ 1 + FS-TEST-GONE-N !
   FS-TEST-GONE-N @ 1 = if
      a u s" /one.txt" ENDS-WITH? if s" gone-walk/two.txt"
      else s" gone-walk/one.txt" then
      FS-TEST-UNDER$ FS:REMOVE-FILE
   then ;

\ An entry removed after its directory was read is skipped, not E-FS-STAT.
: FS-TEST-WALK-GONE ( -- )
   0 FS-TEST-GONE-N !
   s" gone-walk" FS-TEST-UNDER$ 2dup FS:MAKE-DIR FS:CLEANUP-TREE+
   s" gone-walk/one.txt" FS-TEST-UNDER$ s" 1" FS:WRITE-ALL
   s" gone-walk/two.txt" FS-TEST-UNDER$ s" 2" FS:WRITE-ALL
   [: s" gone-walk" FS-TEST-UNDER$ [: FS-TEST-GONE-CB ;] FS:WALK-FILES ;]
   catch 0 FS-TEST=
   FS-TEST-GONE-N @ 1 FS-TEST= ;

: FS-TEST-WALK-THROW-CB ( ptr u8 n -- )
   2drop E-A-BOUNDS throw ;       \ no walk path raises the callback's code

: FS-TEST-WALK-THROW ( -- )
   FS-TEST-ROOT [: FS-TEST-WALK-THROW-CB ;] FS:WALK-FILES ;

: FS-TEST-NEST-INNER-CB ( ptr u8 n -- )
   2drop FS-TEST-NEST-N @ 1 + FS-TEST-NEST-N ! ;

: FS-TEST-WALK-NEST-CB ( ptr u8 n -- )
   2drop FS-TEST-ROOT [: FS-TEST-NEST-INNER-CB ;] FS:WALK-FILES ;

: FS-TEST-WALK-NEST ( -- )
   0 FS-TEST-NEST-N !
   FS-TEST-ROOT [: FS-TEST-WALK-NEST-CB ;] FS:WALK-FILES ;

: FS-TEST-WALK-NEST-THROW-CB ( ptr u8 n -- )
   2drop FS-TEST-ROOT [: FS-TEST-WALK-THROW-CB ;] FS:WALK-FILES ;

: FS-TEST-WALK-NEST-THROW ( -- )
   FS-TEST-ROOT [: FS-TEST-WALK-NEST-THROW-CB ;] FS:WALK-FILES ;

\ FS-TEST-DEEP-LIMIT levels is past WALK-FRAMES0, so the walk and the
\ removal both grow their frames while a descriptor is open at every depth
\ above. The leaf's whole path is checked, so every depth the frames moved
\ under still joined its name onto the right parent.
: FS-TEST-DEEP-CB ( ptr u8 n -- ) {: a:ptr u :}
   FS-TEST-LEAF-N @ 1 + FS-TEST-LEAF-N !
   a u FS-TEST-OUT FS-TEST-LEAF-U @ FS-TEST$= ;

: FS-TEST-DEEP-TREE ( -- )
   0 FS-TEST-LEAF-N !
   FS-TEST-DEEP-CUR s" leaf.txt" FS-TEST-OUT FS:JOIN-PATH FS-TEST-LEAF-U !
   FS-TEST-DEEP [: FS-TEST-DEEP-CB ;] FS:WALK-FILES
   FS-TEST-LEAF-N @ 1 FS-TEST=
   FS-TEST-DEEP FS:REMOVE-TREE
   FS-TEST-DEEP FS:EXISTS? FS-TEST-FALSE ;

\ PATH-CAP bounds the path string, not the tree; past PATH-CAP's lstat limit a
\ failed lstat reads as a vanished entry.
: FS-TEST-CAP ( -- ptr u8 n )
   FS-TEST-CAP-PATH FS-TEST-CAP-U @ ;

: FS-TEST-CAP-OVER ( -- ptr u8 n )
   FS-TEST-CAP-PATH FS-TEST-CAP-U @ 1 + ;

: FS-TEST-CAP-ROOT ( -- ptr u8 n )
   FS-TEST-CAP-PATH FS-TEST-CAP-ROOT-U @ ;

: FS-TEST-CAP-NAME+ ( n -- ) {: k :}
   FS-TEST-CAP-U @ {: u :}
   FS:SLASH FS-TEST-CAP-PATH u + c!
   FS-TEST-CAP-PATH u 1 + + k FS-TEST-FILL-C FS-TEST-FILL
   u 1 + k + FS-TEST-CAP-U ! ;

: FS-TEST-CAP-DOT+ ( -- )
   FS-TEST-CAP-U @ {: u :}
   FS:SLASH FS-TEST-CAP-PATH u + c!
   [char] . FS-TEST-CAP-PATH u 1 + + c!
   u 2 + FS-TEST-CAP-U ! ;

\ `/.` pairs in the root's spelling carry the length, so the walk's joined path
\ is the cap too and the real tree is one directory holding a 1-byte name. The
\ root's name takes 1 or 2 bytes, so the pairs get an even number of bytes.
: FS-TEST-CAP-BUILD ( -- )
   FS-TEST-BASE FS-TEST-CAP-PATH FS-TEST-CAP-U FS-TEST-COPY!
   PATH-CAP FS-TEST-CAP-U @ - 3 - {: m :}
   2 m 1 and - {: r :}
   m r - 2 / 0 ?do FS-TEST-CAP-DOT+ loop
   r FS-TEST-CAP-NAME+
   FS-TEST-CAP-U @ FS-TEST-CAP-ROOT-U !
   1 FS-TEST-CAP-NAME+
   FS-TEST-FILL-C FS-TEST-CAP-PATH FS-TEST-CAP-U @ + c! ;

: FS-TEST-CAP-CB ( ptr u8 n -- )
   FS-TEST-CAP-SEEN @ 1 + FS-TEST-CAP-SEEN !
   FS-TEST-CAP FS-TEST$= ;

: FS-TEST-PATH-CAP-EDGE ( -- )
   T-RESET
   FS-TEST-CAP-BUILD
   FS-TEST-CAP-U @ PATH-CAP FS-TEST=
   FS-TEST-CAP-ROOT FS:MAKE-DIRS
   FS-TEST-CAP FS:TRY-LSTAT FS-TEST-FALSE  FS:ERRNO FS-TEST-ENOENT FS-TEST=
   FS-TEST-CAP s" cap" FS:WRITE-ALL
   FS-TEST-CAP FS:TRY-LSTAT FS-TEST-TRUE  FS:ERRNO 0 FS-TEST=
   FS-TEST-CAP FS:FILE? FS-TEST-TRUE
   FS-TEST-CAP FS-TEST-READ-BUF FS-TEST-READ-CAP FS:READ-ALL FS-TEST-WRITE-LEN FS-TEST=
   FS-TEST-READ-BUF FS-TEST-WRITE-LEN s" cap" FS-TEST$=
   0 FS-TEST-CAP-SEEN !
   FS-TEST-CAP-ROOT [: FS-TEST-CAP-CB ;] FS:WALK-FILES
   FS-TEST-CAP-SEEN @ 1 FS-TEST=
   [: FS-TEST-CAP-OVER FS:TRY-LSTAT drop ;] E-FS-PATH TTHROWSQ  FS:ERRNO 0 FS-TEST=
   [: FS-TEST-CAP-OVER s" cap" FS:WRITE-ALL ;] E-FS-PATH TTHROWSQ  FS:ERRNO 0 FS-TEST=
   [: FS-TEST-CAP-OVER FS:MAKE-DIRS ;] E-FS-PATH TTHROWSQ  FS:ERRNO 0 FS-TEST=
   FS-TEST-CAP-ROOT FS:REMOVE-TREE
   FS-TEST-CAP-ROOT FS:EXISTS? FS-TEST-FALSE
   T-FAILURES 0 FS-TEST= ;

\ Two tasks walk two trees at once, and a callback walks a second tree while its
\ own walk stands, each through plain FS:WALK-FILES. A crossed path slot is what
\ the prefix check catches: every path a callback is handed starts with its root.
20 constant FS-TEST-WALK-ROUNDS
2 constant FS-TEST-WA-FILES
3 constant FS-TEST-WB-FILES

variable FS-TEST-WA-N
variable FS-TEST-WB-N
variable FS-TEST-WALK-BAD
variable FS-TEST-WALK-READY
TASK:MIN-STACK TASK:TASK FS-TEST-WALK-A-TASK
TASK:MIN-STACK TASK:TASK FS-TEST-WALK-B-TASK

: FS-TEST-UNDER? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u ra:ptr ru :}
   u ru <= if false exit then
   a ru ra ru STR= ;

: FS-TEST-WA-CB ( ptr u8 n -- ) {: a:ptr u :}
   a u FS-TEST-WA FS-TEST-UNDER? 0= if 1 FS-TEST-WALK-BAD atomic-add drop then
   1 FS-TEST-WA-N atomic-add drop ;

: FS-TEST-WB-CB ( ptr u8 n -- ) {: a:ptr u :}
   a u FS-TEST-WB FS-TEST-UNDER? 0= if 1 FS-TEST-WALK-BAD atomic-add drop then
   1 FS-TEST-WB-N atomic-add drop ;

\ Both tasks are JOINED rather than watched through a done counter: a walk that
\ throws then ends the row red with its code instead of hanging the suite on a
\ counter that never arrives.
: FS-TEST-WALK-WORK-A ( -- )
   1 FS-TEST-WALK-READY atomic-add drop
   begin FS-TEST-WALK-READY atomic@ 2 < while TASK:PAUSE repeat
   FS-TEST-WALK-ROUNDS 0 do
      FS-TEST-WA [: FS-TEST-WA-CB ;] FS:WALK-FILES
   loop
   0 TASK:RETURN ;

: FS-TEST-WALK-WORK-B ( -- )
   1 FS-TEST-WALK-READY atomic-add drop
   begin FS-TEST-WALK-READY atomic@ 2 < while TASK:PAUSE repeat
   FS-TEST-WALK-ROUNDS 0 do
      FS-TEST-WB [: FS-TEST-WB-CB ;] FS:WALK-FILES
   loop
   0 TASK:RETURN ;

: FS-TEST-WALK-COUNTS-RESET ( -- )
   0 FS-TEST-WA-N !  0 FS-TEST-WB-N !  0 FS-TEST-WALK-BAD ! ;

: FS-TEST-JOIN-OK ( ptr n -- )
   TASK:JOIN MATCH result
      ok OF 0 FS-TEST= ENDOF
      err OF dup 0 FS-TEST= . ENDOF
   ;MATCH ;

: FS-TEST-WALK-PARALLEL ( -- )
   FS-TEST-WALK-COUNTS-RESET
   0 FS-TEST-WALK-READY !
   ['] FS-TEST-WALK-WORK-A FS-TEST-WALK-A-TASK TASK:ACTIVATE
   ['] FS-TEST-WALK-WORK-B FS-TEST-WALK-B-TASK TASK:ACTIVATE
   FS-TEST-WALK-A-TASK FS-TEST-JOIN-OK
   FS-TEST-WALK-B-TASK FS-TEST-JOIN-OK
   FS-TEST-WALK-BAD @ 0 FS-TEST=
   FS-TEST-WA-N @ FS-TEST-WA-FILES FS-TEST-WALK-ROUNDS * FS-TEST=
   FS-TEST-WB-N @ FS-TEST-WB-FILES FS-TEST-WALK-ROUNDS * FS-TEST= ;

: FS-TEST-WALK-SECOND-CB ( ptr u8 n -- )
   2drop FS-TEST-WB [: FS-TEST-WB-CB ;] FS:WALK-FILES ;

: FS-TEST-WALK-SECOND ( -- )
   FS-TEST-WALK-COUNTS-RESET
   FS-TEST-WA [: FS-TEST-WALK-SECOND-CB ;] FS:WALK-FILES
   FS-TEST-WALK-BAD @ 0 FS-TEST=
   FS-TEST-WB-N @ FS-TEST-WA-FILES FS-TEST-WB-FILES * FS-TEST= ;

: FS-TEST-NUL-EXISTS ( -- )
   S\" /tmp/habu-fs-unsafe\x00tail" FS:EXISTS? drop ;

: FS-TEST-NUL-WRITE ( -- )
   S\" /tmp/habu-fs-unsafe\x00tail" s" x" FS:WRITE-ALL ;

: FS-TEST-NUL-APPEND ( -- )
   S\" /tmp/habu-fs-unsafe\x00tail" s" x" FS:APPEND-FILE ;

: FS-TEST-IO ( -- )
   FS-TEST-IO-PATH s" abc" FS:WRITE-ALL
   FS-TEST-IO-PATH FS-TEST-READ-BUF FS-TEST-READ-CAP FS:READ-ALL FS-TEST-WRITE-LEN FS-TEST=
   FS-TEST-READ-BUF FS-TEST-WRITE-LEN s" abc" FS-TEST$=
   FS-TEST-IO-PATH FS:>PATHZ FS:O-RDWR 0 open FS-TEST-FD !
   FS-TEST-FD @ 2 > FS-TEST-TRUE
   FS-TEST-FD @ FS-TEST-READ-BUF 1 read 1 FS-TEST=
   FS-TEST-READ-BUF 1 s" a" FS-TEST$=
   FS-TEST-FD @ close
   FS-TEST-IO-PATH s" xy" FS:WRITE-ALL
   FS-TEST-IO-PATH s" z" FS:APPEND-FILE
   FS-TEST-IO-PATH FS:OPEN-APPEND-FD FS-TEST-FD !
   FS-TEST-FD @ s" !" write 1 FS-TEST=
   FS-TEST-FD @ close
   FS-TEST-IO-PATH FS-TEST-READ-BUF FS-TEST-READ-CAP FS:READ-ALL 4 FS-TEST=
   FS-TEST-READ-BUF 4 s" xyz!" FS-TEST$=
   FS-TEST-BIG-PATH FS-TEST-READ-BUF FS-TEST-EXACT-CAP FS:READ-ALL
   FS-TEST-EXACT-CAP FS-TEST=
   FS-TEST-READ-BUF FS-TEST-EXACT-CAP s" abcd" FS-TEST$= ;

: FS-TEST-THROWS ( -- )
   T-RESET
   [: FS-TEST-PATH-TOO-LONG ;] E-FS-PATH TTHROWSQ
   [: FS-TEST-JOIN-TOO-LONG ;] E-FS-CAPACITY TTHROWSQ
   [: FS-TEST-JOIN-NEG-PARENT ;] E-FS-PATH TTHROWSQ
   [: FS-TEST-JOIN-NEG-NAME ;] E-FS-PATH TTHROWSQ
   [: 1 FS-TEST-JOIN-NAME-FILL drop ;] E-FS-CAPACITY TTHROWSQ
   [: FS-TEST-JOIN-NEG-PARENT-LONG-NAME ;] E-FS-PATH TTHROWSQ
   [: FS-TEST-JOIN-LONG-PARENT-NEG-NAME ;] E-FS-PATH TTHROWSQ
   [: FS-TEST-JOIN-HUGE-NAME ;] E-FS-CAPACITY TTHROWSQ
   [: FS-TEST-JOIN-HUGE-PARENT ;] E-FS-CAPACITY TTHROWSQ
   [: FS-TEST-JOIN-LONG-PARENT ;] E-FS-CAPACITY TTHROWSQ
   [: FS-TEST-JOIN-INTO-SHORT ;] E-FS-CAPACITY TTHROWSQ
   [: FS-TEST-MISSING-STAT ;] E-FS-STAT TTHROWSQ
   [: FS-TEST-MISSING-FILE-SIZE ;] E-FS-STAT TTHROWSQ
   [: FS-TEST-DIR-FILE-SIZE ;] E-FS-STAT TTHROWSQ
   [: FS-TEST-MISSING-WALK ;] E-FS-STAT TTHROWSQ
   FS-TEST-UNSEARCHABLE-WALK
   [: FS-TEST-WALK-ROOT-TOO-LONG ;] E-FS-PATH TTHROWSQ
   [: FS-TEST-NUL-EXISTS ;] E-FFI-NUL TTHROWSQ
   [: FS-TEST-NUL-WRITE ;] E-FFI-NUL TTHROWSQ
   [: FS-TEST-NUL-APPEND ;] E-FFI-NUL TTHROWSQ
   [: FS-TEST-MISSING-READ ;] E-FS-OPEN TTHROWSQ
   [: FS-TEST-READ-TOO-LARGE ;] E-FS-CAPACITY TTHROWSQ
   [: FS-TEST-WRITE-DIR ;] E-FS-OPEN TTHROWSQ
   [: FS-TEST-APPEND-DIR ;] E-FS-OPEN TTHROWSQ
   [: FS-TEST-WRITE-MISSING-PARENT ;] E-FS-OPEN TTHROWSQ
   [: FS-TEST-APPEND-MISSING-PARENT ;] E-FS-OPEN TTHROWSQ
   [: FS-TEST-WRITE-READONLY ;] E-FS-OPEN TTHROWSQ
   T-FAILURES 0 FS-TEST= ;

: FS-TEST-TRY-STAT ( -- )
   FS-TEST-BIG-PATH FS:TRY-STAT FS-TEST-TRUE
   FS:STAT-MODE@ FS:S-IFMT and FS:S-IFREG = FS-TEST-TRUE
   FS:STAT-SIZE@ FS-TEST-EXACT-CAP FS-TEST=
   s" no-such-habu-fs-path-for-stdlib-test" FS:TRY-STAT FS-TEST-FALSE
   s" no-such-habu-fs-path-for-stdlib-test" FS:TRY-LSTAT FS-TEST-FALSE
   s" AGENTS.md" FS:TRY-LSTAT FS-TEST-TRUE
   FS:STAT-MODE@ FS:S-IFMT and FS:S-IFREG = FS-TEST-TRUE ;

: FS-TEST-REPORT ( -- )
   FS-TEST-FAIL @ 0 = if s" fs-test: ok" type cr exit then
   s" fs-test: " type FS-TEST-FAIL @ FMT:.INT s"  failure(s)" type cr
   s" fs-test: failures" FS-TEST-EX-FAIL die ;

: FS-TEST-CLEANUP ( -- )
   FS-TEST-WA FS:REMOVE-TREE
   FS-TEST-WB FS:REMOVE-TREE
   FS:CLEANUP-RUN
   FS-TEST-BASE FS:EXISTS? FS-TEST-FALSE ;

\ Two tasks through FS:READ-ALL and FS:FILE-SIZE at once: the per-call slots are
\ the FS-ABI band of each task's own region. Shared slots make the pair hang
\ rather than fail (one task's `0 IO-LEN !` resets the other's progress). The two
\ files differ in length and content, so a crossed descriptor is caught either way.
TASK:MIN-STACK TASK:TASK FS-TEST-PAR-A-TASK
TASK:MIN-STACK TASK:TASK FS-TEST-PAR-B-TASK

: FS-TEST-PAR-A ( -- ptr u8 n )
   FS-TEST-PAR-A-BUF FS-TEST-PAR-A-U @ ;

: FS-TEST-PAR-B ( -- ptr u8 n )
   FS-TEST-PAR-B-BUF FS-TEST-PAR-B-U @ ;

: FS-TEST-PAR-BAD+ ( -- )
   1 FS-TEST-PAR-BAD atomic-add drop ;

: FS-TEST-PAR-ROUND ( ptr u8 n ptr u8 n n -- ) {: pa:ptr pu buf:ptr want:n c :}
   pa pu FS:FILE-SIZE want <> if FS-TEST-PAR-BAD+ exit then
   pa pu buf FS-TEST-PAR-CAP FS:READ-ALL want <> if FS-TEST-PAR-BAD+ exit then
   want 0 do buf i + c@ c <> if FS-TEST-PAR-BAD+ unloop exit then loop ;

: FS-TEST-PAR-WORK ( ptr u8 n ptr u8 n n -- ) {: pa:ptr pu buf:ptr want:n c :}
   1 FS-TEST-PAR-READY atomic-add drop
   begin FS-TEST-PAR-READY atomic@ 2 < while TASK:PAUSE repeat
   FS-TEST-PAR-ITERS 0 do pa pu buf want c FS-TEST-PAR-ROUND loop
   0 TASK:RETURN ;

: FS-TEST-PAR-WORK-A ( -- )
   FS-TEST-PAR-A FS-TEST-PAR-A-DATA FS-TEST-PAR-A-N FS-TEST-PAR-A-C FS-TEST-PAR-WORK ;

: FS-TEST-PAR-WORK-B ( -- )
   FS-TEST-PAR-B FS-TEST-PAR-B-DATA FS-TEST-PAR-B-N FS-TEST-PAR-B-C FS-TEST-PAR-WORK ;

: FS-TEST-PAR-WRITE ( ptr u8 n n n -- ) {: pa:ptr pu n c :}
   FS-TEST-PAR-SEED n c FS-TEST-FILL
   pa pu FS-TEST-PAR-SEED n FS:WRITE-ALL
   pa pu FS:CLEANUP+ ;

: FS-TEST-PARALLEL ( -- )
   FS-TEST-BASE s" par-a.txt" FS-TEST-PAR-A-BUF FS-TEST-PAR-A-U FS-TEST-PATH!
   FS-TEST-BASE s" par-b.txt" FS-TEST-PAR-B-BUF FS-TEST-PAR-B-U FS-TEST-PATH!
   FS-TEST-PAR-A FS-TEST-PAR-A-N FS-TEST-PAR-A-C FS-TEST-PAR-WRITE
   FS-TEST-PAR-B FS-TEST-PAR-B-N FS-TEST-PAR-B-C FS-TEST-PAR-WRITE
   0 FS-TEST-PAR-BAD !  0 FS-TEST-PAR-READY !
   ['] FS-TEST-PAR-WORK-A FS-TEST-PAR-A-TASK TASK:ACTIVATE
   ['] FS-TEST-PAR-WORK-B FS-TEST-PAR-B-TASK TASK:ACTIVATE
   FS-TEST-PAR-A-TASK FS-TEST-JOIN-OK
   FS-TEST-PAR-B-TASK FS-TEST-JOIN-OK
   FS-TEST-PAR-BAD @ 0= FS-TEST-TRUE ;

\ Replace a symlink between the two FS:FILE? checks. A refused write that has
\ already opened its target must close it; the lowest free fd stays unchanged.
create FS-TEST-RACE-LIVE-BUF PATH-CAP allot
create FS-TEST-RACE-NEXT-BUF PATH-CAP allot
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
   FS-TEST-RACE-NEXT FS:MAKE-SYMLINK
   FS-TEST-RACE-NEXT FS-TEST-RACE-LIVE FS:RENAME-FILE ;
: FS-TEST-RACE-WORK ( -- )
   begin FS-TEST-RACE-STOP atomic@ 0= while
      FS-TEST-IO-PATH FS-TEST-RACE-REPLACE
      s" /dev/null" FS-TEST-RACE-REPLACE
      1 FS-TEST-RACE-SWAPS atomic-add drop
   repeat
   0 TASK:RETURN ;
: FS-TEST-FREE-FD ( -- n )
   S\" /dev/null\z" drop open-rd {: fd:n :}
   fd close
   fd ;

: FS-TEST-WALK-THROW-CLOSES ( -- )
   FS-TEST-FREE-FD {: before:n :}
   [: FS-TEST-WALK-THROW ;] E-A-BOUNDS TTHROWSQ
   FS-TEST-FREE-FD before FS-TEST=
   \ A callback walks the same tree again on a context of its own: every
   \ inner walk sees all three files, and both give their descriptors back.
   FS-TEST-FREE-FD {: nested:n :}
   FS-TEST-WALK-NEST
   FS-TEST-NEST-N @ 3 3 * FS-TEST=
   FS-TEST-FREE-FD nested FS-TEST=
   \ An inner callback's throw unwinds both walks, and each closes its own.
   FS-TEST-FREE-FD {: inner:n :}
   [: FS-TEST-WALK-NEST-THROW ;] E-A-BOUNDS TTHROWSQ
   FS-TEST-FREE-FD inner FS-TEST= ;

\ FS:READ-AT over a file of ten known bytes: a range inside it and a range
\ ending at its last byte read those bytes; one byte past the end is E-FS-IO
\ with no cause and gives its descriptor back; a negative length or offset is
\ E-FS-CAPACITY, len 0 at the end succeeds, and a missing path is E-FS-OPEN.
: FS-TEST-READ-AT ( -- )
   T-RESET
   FS-TEST-RANGE-PATH s" 0123456789" FS:WRITE-ALL
   FS-TEST-RANGE-PATH 3 FS-TEST-READ-BUF 4 FS:READ-AT
   FS-TEST-READ-BUF 4 s" 3456" FS-TEST$=
   FS-TEST-RANGE-PATH 6 FS-TEST-READ-BUF 4 FS:READ-AT
   FS-TEST-READ-BUF 4 s" 6789" FS-TEST$=
   FS-TEST-FREE-FD {: before:n :}
   [: FS-TEST-RANGE-PATH 7 FS-TEST-READ-BUF 4 FS:READ-AT ;] E-FS-IO TTHROWSQ
   FS:ERRNO 0 FS-TEST=
   FS-TEST-FREE-FD before FS-TEST=
   [: FS-TEST-RANGE-PATH 0 FS-TEST-READ-BUF -1 FS:READ-AT ;]
   E-FS-CAPACITY TTHROWSQ
   [: FS-TEST-RANGE-PATH -1 FS-TEST-READ-BUF 1 FS:READ-AT ;]
   E-FS-CAPACITY TTHROWSQ
   FS-TEST-RANGE-PATH 10 FS-TEST-READ-BUF 0 FS:READ-AT
   [: s" no-such-habu-fs-read-at-file" 0 FS-TEST-READ-BUF 1 FS:READ-AT ;]
   E-FS-OPEN TTHROWSQ
   T-FAILURES 0 FS-TEST= ;

: FS-TEST-FD-LINKS ( -- )
   FS-TEST-LINKS-PATH s" abc" FS:WRITE-ALL
   FS-TEST-LINKS-PATH FS:OPEN-SIZED {: file:fd size:n ok :}
   ok FS-TEST-ASSERT
   file FS:FD-LINKS 1 FS-TEST=
   FS-TEST-LINKS-PATH FS:REMOVE-FILE
   file FS:FD-LINKS 0 FS-TEST=
   file FD>N FS:CLOSE-FD 0 FS-TEST=
   T-RESET
   [: file FS:FD-LINKS drop ;] E-FS-STAT TTHROWSQ
   FS:ERRNO 9 FS-TEST=
   T-FAILURES 0 FS-TEST= ;


: FS-TEST-RACED-REFUSAL ( -- )
   FS-TEST-BASE s" race-link" FS-TEST-RACE-LIVE-BUF FS-TEST-RACE-LIVE-U FS-TEST-PATH!
   FS-TEST-BASE s" race-next" FS-TEST-RACE-NEXT-BUF FS-TEST-RACE-NEXT-U FS-TEST-PATH!
   FS-TEST-IO-PATH FS-TEST-RACE-LIVE FS:MAKE-SYMLINK
   0 FS-TEST-RACE-STOP !  0 FS-TEST-RACE-SWAPS !
   0 FS-TEST-RACE-REFUSED !  0 FS-TEST-RACE-BAD !
   FS-TEST-FREE-FD {: before:n :}
   before 0 >= FS-TEST-TRUE
   ['] FS-TEST-RACE-WORK FS-TEST-RACE-TASK TASK:ACTIVATE
   4000 0 do
      i 1 and 0<> if [: FS-TEST-RACE-LIVE s" x" FS:APPEND-FILE ;] catch
      else [: FS-TEST-RACE-LIVE s" x" FS:WRITE-ALL ;] catch then
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
   FS-TEST-RACE-LIVE FS:REMOVE-FILE ;

package FS-TEST-FIFO
private

$20 constant OUT-CAP
3 constant F-GETFL

\ The open needs O_NONBLOCK; the write end it answers must not keep it, or a
\ write that fills the FIFO would fail instead of waiting for the reader.
: NONBLOCK ( -- n )
   HB-TARGET-MACOS? if 4 else $800 then ;

create DIR-BUF PATH-CAP allot
create FIFO-BUF PATH-CAP allot
create LINK-BUF PATH-CAP allot
create TOOL-BUF PATH-CAP allot
create OUT-BUF OUT-CAP allot
variable DIR-U
variable FIFO-U
variable LINK-U
variable WRITER
variable VISITS

: DIR$ ( -- ptr u8 n )
   DIR-BUF DIR-U @ ;

: FIFO$ ( -- ptr u8 n )
   FIFO-BUF FIFO-U @ ;

: LINK$ ( -- ptr u8 n )
   LINK-BUF LINK-U @ ;

: PAYLOAD$ ( -- ptr u8 n )
   s" through the fifo" ;

\ A system tool's path, resolved through PATH into TOOL-BUF.
: TOOL ( ptr u8 n -- ptr u8 len )
   >LEN TOOL-BUF RESOLVE-EXECUTABLE TOOL-BUF swap ;

: MAKE-FIFO ( -- )
   PROC-ARGV-RESET
   FIFO$ >LEN PROC-ARGV+
   s" mkfifo" TOOL -1 >FD -1 >FD -1 >FD PROC-RUN-ARGV-IO-RC
   MATCH result
      ok OF drop ENDOF
      err OF drop E-FS-IO throw ENDOF
   ;MATCH ;

: SPAWN-CAT ( fd -- pid )
   {: out:fd :}
   PROC-ARGV-RESET
   FIFO$ >LEN PROC-ARGV+
   s" cat" TOOL -1 >FD out -1 >FD PROC-SPAWN-ARGV-IO ;

\ True when the attempt opened the write end, which this closes again.
: OPENED? ( option<fd> -- bool )
   MATCH option
      none OF false ENDOF
      some OF FD>N close true ENDOF
   ;MATCH ;

\ One attempt, keeping in WRITER the write end it opened.
: ATTEMPT ( -- bool )
   FIFO$ FD:TRY-FIFO-WRITER MATCH option
      none OF false ENDOF
      some OF FD>N WRITER ! true ENDOF
   ;MATCH ;

\ A zero-window probe, which reaps a cat that has ended.
: ENDED? ( pid -- bool )
   0 >MS PROCESS:WAIT-BOUNDED MATCH PROCESS:ending
      timeout OF false ENDOF
      exited OF drop true ENDOF
      signaled OF drop true ENDOF
   ;MATCH ;

\ The caller's retry loop: an attempt a millisecond until one opens, false
\ once cat has ended without opening the read end.
: AWAIT-READER ( pid -- bool )
   {: child:pid :}
   begin
      ATTEMPT if true exit then
      child ENDED? if false exit then
      1 >MS TASK:SLEEP
   again ;

: PREDICATES ( -- )
   FIFO$ FS:FIFO? FS-TEST-TRUE
   LINK$ FS:SYMLINK? FS-TEST-TRUE
   LINK$ FS:FIFO? FS-TEST-TRUE
   FS-TEST-BIG-PATH FS:FIFO? FS-TEST-FALSE
   FS-TEST-BASE FS:FIFO? FS-TEST-FALSE
   s" no-such-habu-fs-path-for-stdlib-test" FS:FIFO? FS-TEST-FALSE ;

: NO-READER ( -- )
   FS-TEST-FREE-FD {: before:n :}
   FIFO$ FD:TRY-FIFO-WRITER OPENED? FS-TEST-FALSE
   FS-TEST-FREE-FD before FS-TEST= ;

: REFUSALS ( -- )
   T-RESET
   [: FS-TEST-BIG-PATH FD:TRY-FIFO-WRITER OPENED? drop ;] E-FS-OPEN TTHROWSQ
   [: s" no-such-habu-fs-path-for-stdlib-test" FD:TRY-FIFO-WRITER OPENED? drop ;]
   E-FS-OPEN TTHROWSQ
   T-FAILURES 0 FS-TEST= ;

\ Everything cat printed: the payload, then end of file.
: OUTPUT ( fd -- )
   OUT-BUF PAYLOAD$ nip 1+ SPAN:MAKE FD:READ-EXACT MATCH FD:fill
      full OF false FS-TEST-TRUE ENDOF
      eof OF PAYLOAD$ nip FS-TEST= ENDOF
   ;MATCH
   OUT-BUF PAYLOAD$ nip PAYLOAD$ FS-TEST$= ;

: REAP ( pid -- )
   PROCESS:WAIT PROCESS:STATUS>RC RC>N 0 FS-TEST= ;

: READER ( -- )
   FD:PIPE-PAIR {: r:fd w:fd :}
   w SPAWN-CAT {: child:pid :}
   w FD>N close
   child AWAIT-READER {: opened:bool :}
   opened FS-TEST-TRUE
   opened if
      WRITER @ F-GETFL 0 fcntl NONBLOCK and 0 FS-TEST=
      WRITER @ >FD PAYLOAD$ FD:WRITE-FULL
      WRITER @ close
      child REAP
      r OUTPUT
   then
   r FD>N close ;

: VISIT ( ptr u8 n -- )
   2drop VISITS @ 1 + VISITS ! ;

: WALK-DIR ( -- )
   0 VISITS !
   DIR$ [: VISIT ;] FS:WALK-FILES
   VISITS @ 0 FS-TEST= ;

public

: RUN ( -- )
   FS-TEST-BASE s" fifo-dir" DIR-BUF DIR-U FS-TEST-PATH!
   DIR$ s" fifo" FIFO-BUF FIFO-U FS-TEST-PATH!
   DIR$ s" fifo-link" LINK-BUF LINK-U FS-TEST-PATH!
   DIR$ FS:MAKE-DIR
   DIR$ FS:CLEANUP-TREE+
   MAKE-FIFO
   FIFO$ LINK$ FS:MAKE-SYMLINK
   WALK-DIR
   PREDICATES
   NO-READER
   REFUSALS
   READER ;

;package

: FS-TEST-MAIN ( -- )
   FS-TEST-SETUP
   FS-TEST-PREPARE-FIXTURE
   FS-TEST-TRY-STAT
   FS-TEST-PATHS
   FS-TEST-OS-PATH-CAP
   FS-TEST-FILE-DIR
   FS-TEST-EXECUTABLE
   FS-TEST-FILE-SIZE
   FS-TEST-FILE-META
   FS-TEST-BASENAME
   FS-TEST-JOIN
   FS-TEST-WALK
   FS-TEST-WALK-GONE
   FS-TEST-PATH-CAP-EDGE
   FS-TEST-DEEP-TREE
   FS-TEST-WALK-SECOND
   FS-TEST-WALK-PARALLEL
   FS-TEST-WALK-THROW-CLOSES
   FS-TEST-IO
   FS-TEST-READ-AT
   FS-TEST-FD-LINKS
   FS-TEST-PARALLEL
   FS-TEST-RACED-REFUSAL
   FS-TEST-FIFO:RUN
   FS-TEST-THROWS
   FS-TEST-CLEANUP
   FS-TEST-REPORT ;

FS-TEST-MAIN

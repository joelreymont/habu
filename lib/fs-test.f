\ fs-test.f - focused tests for checked stdlib filesystem helpers.
\ Run: cat lib/errors.f lib/string.f lib/fs.f lib/fs-test.f | bin/hb

1 constant FS-TEST-EX-FAIL
$34 constant FS-TEST-U16-LO
$12 constant FS-TEST-U16-HI
$78 constant FS-TEST-FILL-C

variable FS-TEST-CASE
variable FS-TEST-FAIL

create FS-TEST-OUT FS-PATH-CAP allot
create FS-TEST-LONG FS-PATH-CAP 1 + allot
create FS-TEST-U16
   FS-TEST-U16-LO c, FS-TEST-U16-HI c,

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

: FS-TEST-SETUP ( -- )
   FS-TEST-LONG FS-PATH-CAP 1 + FS-TEST-FILL-C FS-TEST-FILL ;

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

: FS-TEST-THROWS ( -- )
   ['] FS-TEST-PATH-TOO-LONG catch E-FS-PATH FS-TEST=
   ['] FS-TEST-JOIN-TOO-LONG catch E-FS-CAPACITY FS-TEST=
   ['] FS-TEST-JOIN-NEG-PARENT catch E-FS-PATH FS-TEST=
   ['] FS-TEST-CAP-TOO-LONG catch E-FS-CAPACITY FS-TEST=
   ['] FS-TEST-MISSING-STAT catch E-FS-STAT FS-TEST= ;

: FS-TEST-INTERNALS ( -- )
   FS-FALSE FS-TEST-FALSE
   FS-TRUE FS-TEST-TRUE
   FS-TEST-U16 FS-U16@ $1234 FS-TEST=
   FS-PATH-CAP FS-CHECK-JOIN-CAP
   s" no-such-habu-fs-path-for-stdlib-test" FS-TRY-STAT-MODE -1 FS-TEST= ;

: FS-TEST-REPORT ( -- )
   FS-TEST-FAIL @ 0 = if s" fs-test: ok" type cr exit then
   FS-TEST-FAIL @ . s" fs-test: failures" type cr
   s" fs-test: failures" FS-TEST-EX-FAIL die ;

: FS-TEST-MAIN ( -- )
   FS-TEST-SETUP
   FS-TEST-INTERNALS
   FS-TEST-PATHS
   FS-TEST-FILE-DIR
   FS-TEST-BASENAME
   FS-TEST-JOIN
   FS-TEST-THROWS
   FS-TEST-REPORT ;

FS-TEST-MAIN

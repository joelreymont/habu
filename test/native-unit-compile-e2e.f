\ A selected source still runs through the real native loader. The generated
\ source tree and its verdict are repeatable with:
\ bin/hb --load test/native-unit-compile-e2e.f

require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require src/compiler/native/compiler.f
require tools/native-unit-compile.f

package UNIT-COMPILE
public
TRUSTED: BORROWED-CLEAR? ( -- bool )
   BODY-XT @ 0= ;
;package

package NATIVE-UNIT-COMPILE-TEST

create ROOT FS-PATH-CAP allot variable ROOT-U
create PATH FS-PATH-CAP allot
variable BODY-SEEN
variable GOOD-SOURCE-N
variable SKIP-SOURCE-N
public
variable SIDE-EFFECT

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;

: PUT ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n body:ptr bodyu:n :}
   ROOT$ name nameu PATH JOIN-PATH PATH swap body bodyu WRITE-ALL ;

: SOURCES ( -- )
   s" good.f"
      S\" require lib/string.f\npackage UCX\nprivate\n41 constant SEED\nget-current prot-wid-add\npublic\n: VALUE ( -- n ) SEED 1 + ;\n;package\n" PUT
   s" skip.f"
      S\" package UCX\npublic\n: SKIPPED ( -- n ) 99 ;\n;package\n" PUT
   s" wrong.f"
      S\" package WRONG\npublic\n: NEVER ( -- n ) 99 ;\n;package\n" PUT
   s" tail.f"
      S\" package UCX\npublic\n: TAIL ( -- n ) 7 ;\n;package\n0 set-tier\n" PUT
   s" store.f"
      S\" package UCX\npublic\n: STORE ( -- n ) 8 ;\n;package\n0 NATIVE-UNIT-COMPILE-TEST:SIDE-EFFECT !\n" PUT
   s" dep.f"
      S\" 0 NATIVE-UNIT-COMPILE-TEST:SIDE-EFFECT !\n" PUT
   s" unavailable.f"
      S\" require dep.f\npackage UCX\npublic\n: UNAVAILABLE-VALUE ( -- n ) 9 ;\n;package\n" PUT
   s" immediate.f"
      S\" package UCX\npublic\n: IMMEDIATE-VALUE ( -- n ) NATIVE-UNIT-COMPILE-TEST:SIDE 42 ;\n;package\n" PUT
   s" shadow-require.f"
      S\" require lib/string.f\npackage USR\n;package\n" PUT
   s" fresh-require.f"
      S\" require lib/string.f\npackage UFR\npublic\n: VALUE ( -- n ) 23 ;\n;package\n" PUT
   s" shadow-current.f"
      S\" package UCG\n: get-current ( -- n ) 1 NATIVE-UNIT-COMPILE-TEST:SIDE-EFFECT ! wordlist ;\nget-current prot-wid-add\n;package\n" PUT
   s" qualified-colon.f"
      S\" package UQC\npublic\n: QCO:VALUE ( -- n ) 11 ;\n;package\n" PUT
   s" qualified-constant.f"
      S\" package UQK\npublic\n7 constant QKO:VALUE\n;package\n" PUT
   s" missing-close.f"
      S\" package UCM\npublic\n: VALUE ( -- n ) 4 ;\n" PUT
   s" retry.f"
      S\" package UCR\npublic\n: VALUE ( -- n ) 7 ;\n;package\n" PUT ;

: ON-BODY ( -- bool ) 1 BODY-SEEN ! false ;
: ON-SKIP ( -- bool ) 2 BODY-SEEN ! true ;
: LOAD-GOOD ( -- ) 1 GOOD-SOURCE-N +! s" good.f" included ;
: LOAD-SKIP ( -- ) 1 SKIP-SOURCE-N +! s" skip.f" included ;
: LOAD-WRONG ( -- ) s" wrong.f" included ;
: LOAD-TAIL ( -- ) s" tail.f" included ;
: LOAD-STORE ( -- ) s" store.f" included ;
: LOAD-UNAVAILABLE ( -- ) s" unavailable.f" included ;
: LOAD-IMMEDIATE ( -- ) s" immediate.f" included ;
: LOAD-SHADOW-REQUIRE ( -- ) s" shadow-require.f" included ;
: LOAD-FRESH-REQUIRE ( -- ) s" fresh-require.f" included ;
: LOAD-SHADOW-CURRENT ( -- ) s" shadow-current.f" included ;
: LOAD-QUALIFIED-COLON ( -- ) s" qualified-colon.f" included ;
: LOAD-QUALIFIED-CONSTANT ( -- ) s" qualified-constant.f" included ;
: LOAD-MISSING-CLOSE ( -- ) s" missing-close.f" included ;
: LOAD-RETRY ( -- ) s" retry.f" included ;

: GOOD ( -- ) s" UCX" [: ON-BODY ;] [: LOAD-GOOD ;] UNIT-COMPILE:WITH ;
: SKIP ( -- ) s" UCX" [: ON-SKIP ;] [: LOAD-SKIP ;] UNIT-COMPILE:WITH ;
: WRONG ( -- ) s" UCX" [: ON-BODY ;] [: LOAD-WRONG ;] UNIT-COMPILE:WITH ;
: TAIL ( -- ) s" UCX" [: ON-BODY ;] [: LOAD-TAIL ;] UNIT-COMPILE:WITH ;
: STORE ( -- ) s" UCX" [: ON-BODY ;] [: LOAD-STORE ;] UNIT-COMPILE:WITH ;
: UNAVAILABLE ( -- ) s" UCX" [: ON-BODY ;] [: LOAD-UNAVAILABLE ;] UNIT-COMPILE:WITH ;
: IMMEDIATE-LOAD ( -- ) s" UCX" [: ON-BODY ;] [: LOAD-IMMEDIATE ;] UNIT-COMPILE:WITH ;
: SHADOW-REQUIRE ( -- ) s" USR" [: ON-BODY ;] [: LOAD-SHADOW-REQUIRE ;] UNIT-COMPILE:WITH ;
: FRESH-REQUIRE ( -- ) s" UFR" [: ON-BODY ;] [: LOAD-FRESH-REQUIRE ;] UNIT-COMPILE:WITH ;
: SHADOW-CURRENT ( -- ) s" UCG" [: ON-BODY ;] [: LOAD-SHADOW-CURRENT ;] UNIT-COMPILE:WITH ;
: QUALIFIED-COLON ( -- ) s" UQC" [: ON-BODY ;] [: LOAD-QUALIFIED-COLON ;] UNIT-COMPILE:WITH ;
: QUALIFIED-CONSTANT ( -- ) s" UQK" [: ON-BODY ;] [: LOAD-QUALIFIED-CONSTANT ;] UNIT-COMPILE:WITH ;
: MISSING-CLOSE ( -- ) s" UCM" [: ON-BODY ;] [: LOAD-MISSING-CLOSE ;] UNIT-COMPILE:WITH ;
: RETRY ( -- ) s" UCR" [: ON-BODY ;] [: LOAD-RETRY ;] UNIT-COMPILE:WITH ;

: SIDE ( -- ) 1 SIDE-EFFECT ! ; immediate
s" NATIVE-UNIT-COMPILE-TEST:SIDE" 0 parse-imm

: RUN-CASES ( -- )
   0 BODY-SEEN !
   0 GOOD-SOURCE-N ! 0 SKIP-SOURCE-N !
   GOOD
   UNIT-COMPILE:BORROWED-CLEAR? TTRUE
   BODY-SEEN @ 1 T=
   GOOD-SOURCE-N @ 1 T=
   SKIP-SOURCE-N @ 0 T=
   SKIP
   UNIT-COMPILE:BORROWED-CLEAR? TTRUE
   BODY-SEEN @ 2 T=
   GOOD-SOURCE-N @ 1 T=
   SKIP-SOURCE-N @ 1 T=
   ndict@ {: before:n :}
   [: WRONG ;] catch 70 T=
   UNIT-COMPILE:BORROWED-CLEAR? TTRUE
   ndict@ before T=
   BODY-SEEN @ 2 T=
   [: TAIL ;] catch 70 T=
   tier@ 1 T=
   17 SIDE-EFFECT !
   [: STORE ;] catch 70 T=
   SIDE-EFFECT @ 17 T=
   [: UNAVAILABLE ;] catch 70 T=
   SIDE-EFFECT @ 17 T=
   0 SIDE-EFFECT !
   [: IMMEDIATE-LOAD ;] catch 70 T=
   SIDE-EFFECT @ 0 T= ;

: RUN ( -- )
   T-RESET
   s" bounded native package evaluator" T-LABEL
   s" native-unit-compile-e2e" HB-TMP-MKDIR {: root:ptr u:n :}
   root ROOT u BYTE-COPY u ROOT-U !
   SOURCES
   ROOT$ [: RUN-CASES ;] SOURCE-ROOT:WITH
   s" native unit compile tree: " type ROOT$ type cr ;

: SHADOW-REQUIRE-CHECK ( -- )
   [: SHADOW-REQUIRE ;] catch 70 T= ;

: SHADOW-REQUIRE-CASE ( -- )
   17 SIDE-EFFECT !
   ROOT$ [: SHADOW-REQUIRE-CHECK ;] SOURCE-ROOT:WITH
   SIDE-EFFECT @ 17 T= ;

TRUSTED: ORIGINAL-REQUIRE-XT ( -- n ) ['] require ;

: FRESH-REQUIRE-CASE ( n -- )
   UNIT-COMPILE:BIND-REQUIRE
   ROOT$ [: FRESH-REQUIRE ;] SOURCE-ROOT:WITH
   ORIGINAL-REQUIRE-XT UNIT-COMPILE:BIND-REQUIRE ;

: REVIEW-BODY ( -- )
   ndict@ {: before:n :}
   [: QUALIFIED-COLON ;] catch 70 T=
   ndict@ before T=
   [: QUALIFIED-CONSTANT ;] catch 70 T=
   ndict@ before T=
   17 SIDE-EFFECT !
   [: SHADOW-CURRENT ;] catch 70 T=
   SIDE-EFFECT @ 17 T=
   ndict@ {: missing-before:n :}
   [: MISSING-CLOSE ;] catch 70 T=
   ndict@ missing-before T=
   [: RETRY ;] catch 0 T= ;

: REVIEW-CASES ( -- )
   ROOT$ [: REVIEW-BODY ;] SOURCE-ROOT:WITH ;

;package

' require UNIT-COMPILE:BIND-REQUIRE
1 set-tier
NATIVE-UNIT-COMPILE-TEST:RUN
UCX:VALUE 42 T=
package SREQ
: require ( -- ) 1 NATIVE-UNIT-COMPILE-TEST:SIDE-EFFECT ! ;
NATIVE-UNIT-COMPILE-TEST:SHADOW-REQUIRE-CASE
;package
NATIVE-UNIT-COMPILE-TEST:REVIEW-CASES
undefine require
: require ( -- ) parse-name 2drop ;
' require NATIVE-UNIT-COMPILE-TEST:FRESH-REQUIRE-CASE
UFR:VALUE 23 T=
T-REPORT

\ A selected source still runs through the real native loader. The generated
\ source tree and its verdict are repeatable with:
\ bin/hb --load test/native-unit-compile-e2e.f

require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/test/subject.f
require src/compiler/native/compiler.f
require tools/native-unit-compile.f

package UNIT-COMPILE
public
TRUSTED: BORROWED-CLEAR? ( -- bool )
   BODY-XT @ 0= ;
TRUSTED: NESTED-RUN ( [ -- ] -- n )
   ['] GUARD swap unit-compile-run ;
;package

package NATIVE-UNIT-COMPILE-TEST

create ROOT FS-PATH-CAP allot variable ROOT-U
create PATH FS-PATH-CAP allot
variable BODY-SEEN
variable GOOD-SOURCE-N
variable SKIP-SOURCE-N
variable CONTINUED
create OUT 1024 allot
create ERR 1024 allot
variable ERR-U
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
   s" open-definition.f"
      S\" package UCO\npublic\n: VALUE ( -- n ) 4\n" PUT
   s" retry.f"
      S\" package UCR\npublic\n: VALUE ( -- n ) 7 ;\n;package\n" PUT
   s" throw.f"
      S\" package UCT\npublic\n: VALUE ( -- n ) 6 ;\n;package\n" PUT
   s" after-throw.f"
      S\" package UCAFT\npublic\n: VALUE ( -- n ) 43 ;\n;package\n" PUT
   s" nested.f"
      S\" package UCNT\npublic\n: VALUE ( -- n ) 44 ;\n;package\n0 NATIVE-UNIT-COMPILE-TEST:SIDE-EFFECT !\n" PUT ;

: ON-BODY ( -- bool ) 1 BODY-SEEN ! false ;
: ON-SKIP ( -- bool ) 2 BODY-SEEN ! true ;
: CONTINUE ( -- ) 1 CONTINUED +! ;
TRUSTED: BAD-HOOK ( [ -- ] -- n )
   0 swap unit-compile-run ;
: ON-THROW ( -- bool ) E-STR-BOUNDS throw ;
: ON-NESTED ( -- bool )
   [: CONTINUE ;] UNIT-COMPILE:NESTED-RUN 70 T=
   CONTINUED @ 0 T=
   false ;
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
: LOAD-OPEN-DEFINITION ( -- ) s" open-definition.f" included ;
: LOAD-RETRY ( -- ) s" retry.f" included ;
: LOAD-THROW ( -- ) s" throw.f" included ;
: LOAD-AFTER-THROW ( -- ) s" after-throw.f" included ;
: LOAD-NESTED ( -- ) s" nested.f" included ;

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
: OPEN-DEFINITION ( -- ) s" UCO" [: ON-BODY ;] [: LOAD-OPEN-DEFINITION ;] UNIT-COMPILE:WITH ;
: RETRY ( -- ) s" UCR" [: ON-BODY ;] [: LOAD-RETRY ;] UNIT-COMPILE:WITH ;
: THROWN ( -- ) s" UCT" [: ON-THROW ;] [: LOAD-THROW ;] UNIT-COMPILE:WITH ;
: AFTER-THROW ( -- ) s" UCAFT" [: ON-BODY ;] [: LOAD-AFTER-THROW ;] UNIT-COMPILE:WITH ;
: NESTED ( -- ) s" UCNT" [: ON-NESTED ;] [: LOAD-NESTED ;] UNIT-COMPILE:WITH ;

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
   SIDE-EFFECT @ 0 T=
   s" hook refuses malformed and nested use without running a continuation" T-LABEL
   0 CONTINUED !
   [: CONTINUE ;] BAD-HOOK 70 T=
   CONTINUED @ 0 T=
   17 SIDE-EFFECT !
   [: NESTED ;] catch 70 T=
   CONTINUED @ 0 T=
   SIDE-EFFECT @ 17 T=
   s" arbitrary callback throw releases hook for the next source" T-LABEL
   [: THROWN ;] catch E-STR-BOUNDS T=
   UNIT-COMPILE:BORROWED-CLEAR? TTRUE
   AFTER-THROW ;

: REFUSED ( ptr u8 n -- )
   OUT 1024 >LEN ERR 1024 >LEN 10000 >MS SUBJECT:RUN
   MATCH outcome
      exited OF 70 T= ENDOF
      signaled OF drop false TTRUE ENDOF
      timeout OF false TTRUE ENDOF
   ;MATCH
   LEN>N ERR-U ! LEN>N drop
   ERR ERR-U @ s" unit-compile-run" CONTAINS? TTRUE ;

: HOOK-GATE ( -- )
   s" scoped hook is inaccessible to ordinary source and tick" T-LABEL
   s" unit-compile-run" REFUSED
   s" ' unit-compile-run" REFUSED
   s" : HOOK-ESCAPE ( [ ptr u8 n n n -- n ] [ -- ] -- n ) unit-compile-run ;" REFUSED ;

: RUN ( -- )
   T-RESET
   s" bounded native package evaluator" T-LABEL
   s" native-unit-compile-e2e" HB-TMP-MKDIR {: root:ptr u:n :}
   root ROOT u BYTE-COPY u ROOT-U !
   SOURCES
   ROOT$ [: RUN-CASES ;] SOURCE-ROOT:WITH
   HOOK-GATE
   s" native unit compile tree: " type ROOT$ type cr ;

: SHADOW-REQUIRE-CHECK ( -- )
   [: SHADOW-REQUIRE ;] catch 70 T= ;

: SHADOW-REQUIRE-CASE ( -- )
   17 SIDE-EFFECT !
   ROOT$ [: SHADOW-REQUIRE-CHECK ;] SOURCE-ROOT:WITH
   SIDE-EFFECT @ 17 T= ;

\ BIND-REQUIRE stores the original require's execution token as an integer.
CAST: XT>N ( [ -- ] -- n )
: ORIGINAL-REQUIRE-XT ( -- n ) ['] require XT>N ;

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
   \ A unit that ends inside a definition is refused as any source is, rc 74.
   [: OPEN-DEFINITION ;] catch 74 T=
   ndict@ missing-before T=
   [: RETRY ;] catch 0 T= ;

: REVIEW-CASES ( -- )
   ROOT$ [: REVIEW-BODY ;] SOURCE-ROOT:WITH ;

;package

' require UNIT-COMPILE:BIND-REQUIRE
1 set-tier

package UNIT-HOOK-SHAPE
variable CONTINUED
TRUSTED: CALL ( [ ptr u8 n n n -- n ] [ -- ] -- n )
   unit-compile-run ;
: GUARD ( ptr u8 n n n -- n ) 2drop 2drop 0 ;
: CONTINUE ( -- ) 1 CONTINUED +! ;
public
: RUN ( -- )
   ['] GUARD [: CONTINUE ;] CALL 0 T=
   CONTINUED @ 1 T= ;
;package

NATIVE-UNIT-COMPILE-TEST:RUN
UNIT-HOOK-SHAPE:RUN
UCX:VALUE 42 T=
UCAFT:VALUE 43 T=
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

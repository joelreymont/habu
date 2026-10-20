\ Small applications exercise ordinary source loading around cached definitions.

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require src/lib/process-cwd.f
require src/lib/test/subject.f
require tools/image-size-lib.f
require test/native-file-app-lib.f

package NATIVE-FILE-FORMS-TEST

$8000 constant CAP
create ROOT FS-PATH-CAP allot  variable ROOT-U
create AT-BUF FS-PATH-CAP allot
create AT-A-BUF FS-PATH-CAP allot
create AT-B-BUF FS-PATH-CAP allot
create PUT-BUF FS-PATH-CAP allot
create OUT CAP allot  variable OUT-U
create ERR CAP allot  variable ERR-U
create PROGRAM CAP allot  variable PROGRAM-U
create CUT CAP allot  variable CUT-U
DYNAMIC-BUFFER IMAGE-A u8
DYNAMIC-BUFFER IMAGE-B u8

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: AT ( ptr u8 n -- ptr u8 n )
   {: name:ptr u:n :}
   ROOT$ name u AT-BUF JOIN-PATH AT-BUF swap ;
: AT-A ( ptr u8 n -- ptr u8 n )
   {: name:ptr u:n :}
   ROOT$ name u AT-A-BUF JOIN-PATH AT-A-BUF swap ;
: AT-B ( ptr u8 n -- ptr u8 n )
   {: name:ptr u:n :}
   ROOT$ name u AT-B-BUF JOIN-PATH AT-B-BUF swap ;
: PUT ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n body:ptr bodyu:n :}
   ROOT$ name nameu PUT-BUF JOIN-PATH PUT-BUF swap body bodyu WRITE-ALL ;
: CAPTURE ( ptr u8 n ptr u8 n -- n )
   {: exe:ptr exeu:n stdin:ptr stdinu:n :}
   0 PROGRAM-U ! s" cwd: " PROGRAM CAP PROGRAM-U BUF-APPEND
   ROOT$ PROGRAM CAP PROGRAM-U BUF-APPEND STR-LF PROGRAM CAP PROGRAM-U BUF-APPEND-C
   exe exeu stdin stdinu PROGRAM CAP PROGRAM-U PROC-ARGV-PROGRAM+
   exe exeu >LEN ROOT$ >LEN stdin stdinu >LEN
   OUT CAP >LEN ERR CAP >LEN 600000 >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U !
   PROGRAM PROGRAM-U @ OUT OUT-U @ ERR ERR-U @ rc SUBJECT:RC-TIMED-OUT
   rc ;
: SUCCESS ( n -- )
   dup 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   0 T= ;

: BUILD ( ptr u8 n -- n )
   {: product:ptr productu:n :}
   ROOT$ s" app.f" product productu NATIVE-FILE-APP:BUILD-AT {: rc:n :}
   NATIVE-FILE-APP:OUT$ {: out:ptr outu:n :}
   NATIVE-FILE-APP:ERR$ {: err:ptr erru:n :}
   out OUT outu BYTE-COPY outu OUT-U !
   err ERR erru BYTE-COPY erru ERR-U !
   rc ;

: RUN-PRODUCT ( ptr u8 n ptr u8 n -- )
   {: product:ptr productu:n expected:ptr expectu:n :}
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   product productu AT s" " CAPTURE SUCCESS
   OUT OUT-U @ expected expectu CONTAINS? TTRUE ;

: PREFIX ( -- )
   s" dep.f" S\" package FORMS-DEP\npublic\n: NUMBER ( -- n ) 6 ;\n;package\n" PUT
   s" provided.f" S\" package FORMS-PROVIDED\npublic\n: NUMBER ( -- n ) 99 ;\n;package\n" PUT
   s" prefix.f"
   S\" require dep.f\npackage FORMS-PREFIX\nprivate\nvariable COUNT-CELL\npublic\n: COUNT! ( n -- ) COUNT-CELL ! ;\n: COUNT@ ( -- n ) COUNT-CELL @ ;\n: VALUE ( -- n ) 10 ;\n;package\npackage FORMS-PROVIDED\npublic\n: NUMBER ( -- n ) 13 ;\n;package\n" PUT ;

: DOWNSTREAM ( ptr u8 n -- ) {: body:ptr bodyu:n :}
   s" downstream.f" body bodyu PUT
   s" app.f" S\" require prefix.f\nrequire unit.f\nrequire downstream.f\n: MAIN ( -- ) FORMS-CLIENT:RESULT . ;\n" PUT ;

variable CODE-A-O  variable CODE-A-U
variable CODE-B-O  variable CODE-B-U

: SAME-CODE? ( ptr u8 n ptr u8 n -- bool )
   {: a:ptr au:n b:ptr bu:n :}
   a au IMAGE-SIZE:MEASURE
   IMAGE-SIZE:CODE-BLOB-RANGE CODE-A-U ! CODE-A-O !
   b bu IMAGE-SIZE:MEASURE
   IMAGE-SIZE:CODE-BLOB-RANGE CODE-B-U ! CODE-B-O !
   CODE-A-U @ CODE-B-U @ <> if false exit then
   a au FILE-SIZE {: asize:n :}
   b bu FILE-SIZE {: bsize:n :}
   asize IMAGE-A-RESERVE
   bsize IMAGE-B-RESERVE
   a au 0 IMAGE-A BYTE-VIEW asize READ-ALL asize <> if E-FS-IO throw then
   b bu 0 IMAGE-B BYTE-VIEW bsize READ-ALL bsize <> if E-FS-IO throw then
   0 IMAGE-A BYTE-VIEW CODE-A-O @ BYTE+ CODE-A-U @
   0 IMAGE-B BYTE-VIEW CODE-B-O @ BYTE+ CODE-B-U @ STR= ;

: SAME-CUT? ( ptr u8 n ptr u8 n -- bool )
   {: a:ptr au:n b:ptr bu:n :}
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   a au s" cp@ dbase@ - . ndict@ . wordlist . here data-base - ." CAPTURE SUCCESS
   OUT CUT OUT-U @ BYTE-COPY OUT-U @ CUT-U !
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   b bu s" cp@ dbase@ - . ndict@ . wordlist . here data-base - ." CAPTURE SUCCESS
   CUT CUT-U @ OUT OUT-U @ STR= ;

: BUILD-PAIR ( ptr u8 n ptr u8 n -- )
   {: export:ptr exportu:n imported:ptr importu:n :}
   export exportu BUILD SUCCESS
   OUT OUT-U @ s" definition hits 0" CONTAINS? TTRUE
   imported importu BUILD SUCCESS
   OUT OUT-U @ s" definition hits " CONTAINS? TTRUE
   OUT OUT-U @ s" definition hits 0" CONTAINS? TFALSE
   export exportu AT-A {: a:ptr au:n :}
   imported importu AT-B {: b:ptr bu:n :}
   \ Compare emitted code and live extents, then run both applications below.
   a au b bu SAME-CODE? TTRUE
   a au b bu SAME-CUT? TTRUE ;

public
: START ( ptr u8 n -- )
   T-RESET
   T-LABEL
   s" native-file-forms" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   s" cache" AT MAKE-DIR
   PREFIX ;

: FINISH ( -- )
   T-REPORT
   s" native file forms artifacts: " type ROOT$ type cr ;

: ZERO ( -- )
   s" source actions retain dependency, provided, and DATA state" START
   s" unit.f" S\" require dep.f\ns\" provided.f\" provided\n27 FORMS-PREFIX:COUNT!\n" PUT
   S\" require provided.f\npackage FORMS-CLIENT\npublic\n: RESULT ( -- n ) FORMS-PREFIX:COUNT@ FORMS-DEP:NUMBER + FORMS-PROVIDED:NUMBER + ;\n;package\n" DOWNSTREAM
   s" zero-export" s" zero-import" BUILD-PAIR
   s" zero-export" s" 46" RUN-PRODUCT
   s" zero-import" s" 46" RUN-PRODUCT
   FINISH ;

: NAMESPACE ( -- )
   s" namespace initialization remains visible to cached definitions" START
   s" unit.f" S\" package FORMS-EMPTY\n;package\n" PUT
   S\" package FORMS-EMPTY\npublic\n: VALUE ( -- n ) 7 ;\n;package\npackage FORMS-CLIENT\npublic\n: RESULT ( -- n ) FORMS-PREFIX:VALUE FORMS-EMPTY:VALUE + ;\n;package\n" DOWNSTREAM
   s" namespace-export" s" namespace-import" BUILD-PAIR
   s" namespace-export" s" 17" RUN-PRODUCT
   s" namespace-import" s" 17" RUN-PRODUCT
   FINISH ;

: GLOBAL ( -- )
   s" ordinary global definition and downstream checker" START
   s" unit.f" S\" : NUMBER ( -- n ) 42 ;\ns\" global-unit-source-ran\" type cr\n" PUT
   S\" package FORMS-CLIENT\npublic\n: RESULT ( -- n ) NUMBER 1+ ;\n;package\n" DOWNSTREAM
   s" global-export" s" global-import" BUILD-PAIR
   s" global-export" s" 43" RUN-PRODUCT
   s" global-import" s" 43" RUN-PRODUCT
   S\" package FORMS-CLIENT\npublic\n: RESULT ( -- n ) NUMBER @ ;\n;package\n" DOWNSTREAM
   s" global-bad" BUILD 70 T=
   OUT OUT-U @ s" global-unit-source-ran" CONTAINS? TTRUE
   ERR ERR-U @ s" expected: ptr n actual: n" CONTAINS? TTRUE
   FINISH ;

: REPEATED-FILE ( -- )
   s" selected FILE repeated by a downstream include" START
   s" unit.f" S\" 1 FORMS-PREFIX:COUNT@ + FORMS-PREFIX:COUNT!\n" PUT
   S\" s\" unit.f\" included\npackage FORMS-CLIENT\npublic\n: RESULT ( -- n ) FORMS-PREFIX:COUNT@ ;\n;package\n" DOWNSTREAM
   s" repeat-export" s" repeat-import" BUILD-PAIR
   s" repeat-export" s" 2" RUN-PRODUCT
   s" repeat-import" s" 2" RUN-PRODUCT
   FINISH ;
;package

\ DATA, prior writes, typed cells, literals, reopened packages and created
\ words survive cold compilation and warm import in small applications.
\ bin/hb --load test/native-file-state-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require tools/image-size-lib.f
require test/native-file-app-lib.f

package NATIVE-FILE-STATE-TEST

$8000 constant CAP
create ROOT FS-PATH-CAP allot  variable ROOT-U
create AT-BUF FS-PATH-CAP allot
create AT-A-BUF FS-PATH-CAP allot
create AT-B-BUF FS-PATH-CAP allot
create EXPORT-PATH FS-PATH-CAP allot
create IMPORT-PATH FS-PATH-CAP allot
create OUT CAP allot  variable OUT-U
create ERR CAP allot  variable ERR-U
create CUT CAP allot  variable CUT-U
DYNAMIC-BUFFER IMAGE-A u8
DYNAMIC-BUFFER IMAGE-B u8
variable EXPORT-U
variable IMPORT-U
variable CODE-A-O  variable CODE-A-U
variable CODE-B-O  variable CODE-B-U

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
   name nameu AT body bodyu WRITE-ALL ;

: CAPTURE ( ptr u8 n ptr u8 n -- n )
   {: exe:ptr exeu:n stdin:ptr stdinu:n :}
   exe exeu >LEN stdin stdinu >LEN
   OUT CAP >LEN ERR CAP >LEN 600000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U !
   rc ;

: SUCCESS ( n -- )
   dup 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   0 T= ;

: BUILD ( ptr u8 n -- n ) {: product:ptr productu:n :}
   s" app.f" AT-A product productu AT-B s" cache" AT
      NATIVE-FILE-APP:BUILD {: rc:n :}
   NATIVE-FILE-APP:OUT$ {: out:ptr outu:n :}
   NATIVE-FILE-APP:ERR$ {: err:ptr erru:n :}
   out OUT outu BYTE-COPY outu OUT-U !
   err ERR erru BYTE-COPY erru ERR-U !
   rc ;

: APP-SOURCE ( -- )
   s" app.f"
   S\" require prefix.f\nrequire unit.f\nrequire downstream.f\n: MAIN ( -- ) FILE-STATE-CLIENT:VALUE . FILE-STATE-CLIENT:LONG . FILE-STATE-CLIENT:DEPENDENCY . FILE-STATE-A:LINK@ c@ . FILE-STATE-A:LETTERS$ drop c@ . FILE-STATE-A:EMPTY$ nip . FILE-STATE-A:WORDS$ dup . drop c@ . FILE-STATE-A:BEFORE-SPILL$ dup . drop c@ . FILE-STATE-A:AFTER-SPILL$ dup . drop c@ . 35 FILE-STATE-A:QUOTE@ execute . ;\n" PUT ;

: PREPARE ( -- )
   s" cache" AT MAKE-DIR
   s" test/fixtures/native-file-state-prefix.f" s" prefix.f" AT 1
      NATIVE-FILE-APP:COPY-SKIP-LINES
   s" test/fixtures/native-file-state-unit.f" s" unit.f" AT 1
      NATIVE-FILE-APP:COPY-SKIP-LINES
   s" unit.f" AT S\" s\" state-unit-source-ran\" type cr\n" APPEND-FILE
   s" test/fixtures/native-file-state-downstream.f" s" downstream.f" AT 1
      NATIVE-FILE-APP:COPY-SKIP-LINES
   s" test/fixtures/native-file-state-rejected.f" s" bad.f" AT 0
      NATIVE-FILE-APP:COPY-SKIP-LINES
   APP-SOURCE ;

: FRESH-DOWNSTREAM ( -- )
   s" test/fixtures/native-file-state-downstream.f" s" fresh.f" AT 1
      NATIVE-FILE-APP:COPY-SKIP-LINES
   s" app.f" S\" require prefix.f\nrequire unit.f\nrequire fresh.f\n: MAIN ( -- ) FILE-STATE-CLIENT:VALUE . FILE-STATE-CLIENT:LONG . FILE-STATE-CLIENT:DEPENDENCY . FILE-STATE-A:LINK@ c@ . FILE-STATE-A:LETTERS$ drop c@ . FILE-STATE-A:EMPTY$ nip . FILE-STATE-A:WORDS$ dup . drop c@ . FILE-STATE-A:BEFORE-SPILL$ dup . drop c@ . FILE-STATE-A:AFTER-SPILL$ dup . drop c@ . 35 FILE-STATE-A:QUOTE@ execute . ;\n" PUT ;

: RUN-STATE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   path pathu s" " CAPTURE SUCCESS
   OUT OUT-U @ S\" 101\n35\n14\n66\n65\n0\n11\n102\n12\n98\n11\n97\n42\n" CONTAINS? TTRUE ;

: REJECT-TYPED-CLIENT ( -- )
   s" app.f" S\" require prefix.f\nrequire unit.f\nrequire bad.f\n: MAIN ( -- ) ;\n" PUT
   s" bad-hb" BUILD 70 T=
   OUT OUT-U @ s" state-unit-source-ran" CONTAINS? 0= TTRUE
   ERR ERR-U @ s" expected: ptr a actual: ptr u8" CONTAINS? TTRUE ;

: PAIR-PATHS ( -- )
   s" cold-hb" AT {: a:ptr u:n :}
   a EXPORT-PATH u BYTE-COPY
   u EXPORT-U !
   s" warm-hb" AT {: b:ptr v:n :}
   b IMPORT-PATH v BYTE-COPY
   v IMPORT-U ! ;

: SAME-CODE? ( -- bool )
   EXPORT-PATH EXPORT-U @ IMAGE-SIZE:MEASURE
   IMAGE-SIZE:CODE-BLOB-RANGE CODE-A-U ! CODE-A-O !
   IMPORT-PATH IMPORT-U @ IMAGE-SIZE:MEASURE
   IMAGE-SIZE:CODE-BLOB-RANGE CODE-B-U ! CODE-B-O !
   CODE-A-U @ CODE-B-U @ <> if false exit then
   EXPORT-PATH EXPORT-U @ FILE-SIZE {: asize:n :}
   IMPORT-PATH IMPORT-U @ FILE-SIZE {: bsize:n :}
   asize IMAGE-A-RESERVE
   bsize IMAGE-B-RESERVE
   EXPORT-PATH EXPORT-U @ 0 IMAGE-A BYTE-VIEW asize READ-ALL asize <> if E-FS-IO throw then
   IMPORT-PATH IMPORT-U @ 0 IMAGE-B BYTE-VIEW bsize READ-ALL bsize <> if E-FS-IO throw then
   0 IMAGE-A BYTE-VIEW CODE-A-O @ BYTE+ CODE-A-U @
   0 IMAGE-B BYTE-VIEW CODE-B-O @ BYTE+ CODE-B-U @ STR= ;

: SAME-CUT? ( -- bool )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   EXPORT-PATH EXPORT-U @ s" cp@ dbase@ - . ndict@ . wordlist . here data-base - ."
      CAPTURE SUCCESS
   OUT CUT OUT-U @ BYTE-COPY OUT-U @ CUT-U !
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   IMPORT-PATH IMPORT-U @ s" cp@ dbase@ - . ndict@ . wordlist . here data-base - ."
      CAPTURE SUCCESS
   CUT CUT-U @ OUT OUT-U @ STR= ;

public
: MAIN ( -- )
   T-RESET
   s" full file object restores owned storage and checker state" T-LABEL
   s" native-file-state" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   PREPARE
   s" cold-hb" BUILD SUCCESS
   OUT OUT-U @ s" file unit hits 0" CONTAINS? TTRUE
   OUT OUT-U @ s" state-unit-source-ran" CONTAINS? TTRUE
   s" warm-hb" BUILD SUCCESS
   OUT OUT-U @ s" file unit hits " CONTAINS? TTRUE
   OUT OUT-U @ s" file unit hits 0" CONTAINS? 0= TTRUE
   OUT OUT-U @ s" compiled 0" CONTAINS? TTRUE
   OUT OUT-U @ s" state-unit-source-ran" CONTAINS? 0= TTRUE
   PAIR-PATHS
   SAME-CODE? TTRUE
   SAME-CUT? TTRUE
   EXPORT-PATH EXPORT-U @ RUN-STATE
   IMPORT-PATH IMPORT-U @ RUN-STATE
   FRESH-DOWNSTREAM
   s" fresh-hb" BUILD SUCCESS
   OUT OUT-U @ s" state-unit-source-ran" CONTAINS? 0= TTRUE
   s" fresh-hb" AT RUN-STATE
   REJECT-TYPED-CLIENT
   T-REPORT
   s" native file unit artifacts: " type ROOT$ type cr ;
;package

NATIVE-FILE-STATE-TEST:MAIN

\ Each engine keeps one checksummed object per loaded file under its own
\ directory of the out root (tools/build-out.f).

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-list.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-env.f
require src/lib/process-cwd.f
require src/lib/engine-candidate.f
require test/whitebox-child.f
require test/native-file-e2e.f
require test/native-file-app-lib.f

using NATIVE-FILE-E2E
package NATIVE-FILE-STORAGE-TEST

$10000 constant STREAM-CAP
create CASE-PATH PATH-CAP allot
create OUT-PATH PATH-CAP allot  variable OUT-U
create DIR-PATH PATH-CAP allot  variable DIR-U
create ENTRY-PATH PATH-CAP allot
create CWD-OUT STREAM-CAP allot
create CWD-ERR STREAM-CAP allot
DYNAMIC-BUFFER BYTES u8
DYNAMIC-BUFFER PRODUCT-A u8  variable PRODUCT-A-U
DYNAMIC-BUFFER PRODUCT-B u8  variable PRODUCT-B-U
variable FLIP-N
variable OBJECT-N

: LIB-V1$ ( -- ptr u8 n )
   S\" package STORE-LIB\npublic\n: LIB-X ( -- n ) 1 ;\n: LIB-Y ( -- n ) 2 ;\n: LIB-Z ( -- n ) 3 ;\n;package\n" ;
: LIB-V3$ ( -- ptr u8 n )
   S\" package STORE-LIB\npublic\n: LIB-X ( -- n ) 1 ;\n: LIB-W ( -- n ) 5 ;\n: LIB-Z ( -- n ) 3 ;\n;package\n" ;
: APP$ ( -- ptr u8 n )
   S\" require lib.f\n: MAIN ( -- ) STORE-LIB:LIB-X STORE-LIB:LIB-Z + . ;\n" ;
: APP-NO-LIB$ ( -- ptr u8 n )
   S\" : MAIN ( -- ) 7 . ;\n" ;
: APP-EDITED$ ( -- ptr u8 n )
   S\" require lib.f\n\\ edited\n: MAIN ( -- ) STORE-LIB:LIB-X STORE-LIB:LIB-Z + . ;\n" ;
: LIB-A$ ( -- ptr u8 n )
   S\" package STORE-A\npublic\n: A-ONE ( -- n ) 10 ;\n: A-TWO ( -- n ) A-ONE 1+ ;\n;package\n" ;
: LIB-B$ ( -- ptr u8 n )
   S\" package STORE-B\npublic\n: B-ONE ( -- n ) 20 ;\n: B-TWO ( -- n ) B-ONE 2 + ;\n: B-THREE ( -- n ) B-TWO 3 + ;\n;package\n" ;
: SAVED-SOURCE$ ( -- ptr u8 n )
   S\" 1 set-tier\nAPP-BUILD:RUN-SAVED-REPORT\n" ;

: IN ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: dir:ptr diru:n name:ptr nameu:n :}
   dir diru AT name nameu CASE-PATH FS:JOIN-PATH CASE-PATH swap ;

: WRITE-IN ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: dir:ptr diru:n name:ptr nameu:n body:ptr bodyu:n :}
   dir diru name nameu IN body bodyu FS:WRITE-ALL ;

: OUT-ROOT$ ( -- ptr u8 n ) OUT-PATH OUT-U @ ;

: OUT-IN ( ptr u8 n ptr u8 n -- )
   IN {: a:ptr u:n :}
   a OUT-PATH u BYTE-COPY u OUT-U ! ;

: CASE-DIR ( ptr u8 n -- )
   {: dir:ptr diru:n :}
   dir diru AT FS:MAKE-DIR
   dir diru s" out" OUT-IN ;

\ An empty out root leaves HABU_OUT empty: the build uses its cwd's .out.
: STAGE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: dir:ptr diru:n app:ptr appu:n out:ptr outu:n engine:ptr engineu:n :}
   PROC-ARGV-ENV-RESET
   s" HABU_OUT" >LEN OUT-ROOT$ >LEN PROC-ENV+
   s" HABU_UNDER_TEST" >LEN engine engineu >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN engine engineu >LEN PROC-ENV+
   s" HABU_NATIVE_CACHE_DIAG" >LEN s" 1" >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --" ARG
   dir diru app appu IN ARG
   dir diru out outu IN ARG ;

\ As test/native-file-auto-e2e.f RUN-SAVED-IN.
: SAVED ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: dir:ptr diru:n app:ptr appu:n out:ptr outu:n :}
   NATIVE-FILE-APP:BUILDER$ {: builder:ptr builderu:n :}
   dir diru app appu out outu ENGINE-CANDIDATE:PATH$ STAGE
   builder builderu SAVED-SOURCE$ CAPTURE ;

: SAVED-APP ( ptr u8 n -- n ) s" app.f" s" app-hb" SAVED ;

: SAVED-APP-IN ( ptr u8 n -- n )
   {: dir:ptr diru:n :}
   NATIVE-FILE-APP:BUILDER$ {: builder:ptr builderu:n :}
   dir diru s" app.f" s" app-hb" ENGINE-CANDIDATE:PATH$ STAGE
   builder builderu >LEN dir diru AT >LEN SAVED-SOURCE$ >LEN
      CWD-OUT STREAM-CAP >LEN CWD-ERR STREAM-CAP >LEN 600000 >MS
      PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE-OUTCOME
      PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   CWD-OUT outu LEN>N CWD-ERR erru LEN>N STREAMS!
   rc ;

: DIRECT ( ptr u8 n ptr u8 n -- n )
   {: dir:ptr diru:n engine:ptr engineu:n :}
   dir diru s" app.f" s" app-hb" engine engineu STAGE
   engine engineu S\" require tools/app-build.f\nAPP-BUILD:RUN\n" CAPTURE ;

: RUN-IN ( ptr u8 n ptr u8 n -- n )
   IN {: a:ptr u:n :}
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   a u s" " CAPTURE ;

\ -1 when the report lacks label.
: COUNT ( ptr u8 n -- n )
   {: label:ptr labelu:n :}
   OUT$ {: out:ptr outu:n :}
   out outu label labelu FIND-SUB MATCH option
      none OF -1 ENDOF
      some OF IDX>N labelu + ENDOF
   ;MATCH {: at:n :}
   at 0 < if -1 exit then
   out at + outu at - 10 0 SPLIT-NEXT 2drop
      {: digits:ptr digitsu:n :}
   digits digitsu TRIM STR>NUMBER? MATCH option
      none OF -1 ENDOF
      some OF ENDOF
   ;MATCH ;

: REUSED ( -- n ) s" files reused " COUNT ;
: COMPILED ( -- n ) s" compiled " COUNT ;

: REFUSED? ( -- bool ) ERR$ s" native-cache: entry refused " CONTAINS? ;

: LOAD ( ptr u8 n -- n )
   {: path:ptr pathu:n :}
   path pathu FS:FILE-SIZE {: size:n :}
   size BYTES-RESERVE
   path pathu 0 BYTES size FS:READ-ALL size T=
   size ;

: KEEP-A ( ptr u8 n -- )
   LOAD {: size:n :}
   size PRODUCT-A-RESERVE
   0 BYTES 0 PRODUCT-A size BYTE-COPY size PRODUCT-A-U ! ;

: KEEP-B ( ptr u8 n -- )
   LOAD {: size:n :}
   size PRODUCT-B-RESERVE
   0 BYTES 0 PRODUCT-B size BYTE-COPY size PRODUCT-B-U ! ;

: SAME-A? ( ptr u8 n -- bool )
   LOAD {: size:n :}
   0 BYTES size 0 PRODUCT-A PRODUCT-A-U @ STR= ;

: SAME-B? ( ptr u8 n -- bool )
   LOAD {: size:n :}
   0 BYTES size 0 PRODUCT-B PRODUCT-B-U @ STR= ;

: DIR$ ( -- ptr u8 n ) DIR-PATH DIR-U @ ;

: DIR! ( ptr u8 n -- )
   {: a:ptr u:n :}
   a DIR-PATH u BYTE-COPY u DIR-U ! ;

: ENTRY$ ( ptr u8 n -- ptr u8 n )
   {: name:ptr nameu:n :}
   DIR$ name nameu ENTRY-PATH FS:JOIN-PATH ENTRY-PATH swap ;

\ 64 hex digits and .hfo.
: OBJECT? ( ptr u8 n -- bool )
   {: name:ptr nameu:n :}
   name nameu s" .hfo" ENDS-WITH? nameu 68 = and ;

: TALLY ( ptr u8 n -- )
   OBJECT? if 1 OBJECT-N +! then ;

: TALLY-ENGINE ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   OUT-ROOT$ name nameu ENTRY-PATH FS:JOIN-PATH {: u:n :}
   ENTRY-PATH u s" obj" DIR-PATH FS:JOIN-PATH DIR-U !
   DIR$ [: TALLY ;] FS-LIST:EACH ;

: TALLY-ROOT ( -- n )
   0 OBJECT-N !
   OUT-ROOT$ [: TALLY-ENGINE ;] FS-LIST:EACH
   OBJECT-N @ ;

\ The object's checksum covers its last byte.
: FLIP ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   name nameu OBJECT? 0= if exit then
   name nameu ENTRY$ {: path:ptr u:n :}
   path u LOAD {: size:n :}
   size 1- BYTES dup c@ $FF xor swap c!
   path u 0 BYTES size FS:WRITE-ALL
   1 FLIP-N +! ;

: FLIP-ALL ( -- n )
   0 FLIP-N !
   OUT-ROOT$ NATIVE-FILE-APP:OBJ-DIR$ DIR!
   DIR$ [: FLIP ;] FS-LIST:EACH
   FLIP-N @ ;

\ 1. A file a build does not load keeps its object for the next build that
\ loads it. HABU_OUT is empty, so the objects live in the tree's .out.
: KEEP-CASE ( -- )
   s" an object survives a build that does not use it" T-LABEL
   s" keep" AT FS:MAKE-DIR
   0 OUT-U !
   s" keep" s" lib.f" LIB-V1$ WRITE-IN
   s" keep" s" app.f" APP$ WRITE-IN
   s" keep" SAVED-APP-IN SUCCESS
   s" keep" s" app.f" APP-NO-LIB$ WRITE-IN
   s" keep" SAVED-APP-IN SUCCESS
   s" keep" s" app.f" APP$ WRITE-IN
   s" keep" SAVED-APP-IN SUCCESS
   ERR$ s" name=LIB-" CONTAINS? TFALSE
   \ A file key names the file's place in the load, not its text, so only
   \ MAIN compiles.
   REUSED 1 T=
   COMPILED 1 T=
   s" keep" s" .out" OUT-IN
   TALLY-ROOT 2 T=
   s" keep" s" app-hb" RUN-IN SUCCESS
   OUT$ s" 4" CONTAINS? TTRUE ;

\ 2. Another engine (different checker and kernel identity) writes its objects
\ in its own directory and leaves this engine's.
: ALTERNATE-CASE ( -- )
   s" another engine's build keeps this engine's objects" T-LABEL
   s" alternate" CASE-DIR
   s" alternate" s" lib.f" LIB-V1$ WRITE-IN
   s" alternate" s" app.f" APP$ WRITE-IN
   s" alternate" ENGINE-CANDIDATE:PATH$ DIRECT SUCCESS
   s" alternate" ENGINE-CANDIDATE:PATH$ DIRECT SUCCESS
   REUSED {: warm:n :}
   COMPILED {: warm-compiled:n :}
   warm 0 > TTRUE
   TALLY-ROOT {: first-objects:n :}
   s" alternate" WHITEBOX-CHILD:ENGINE$ DIRECT SUCCESS
   TALLY-ROOT first-objects > TTRUE
   s" alternate" ENGINE-CANDIDATE:PATH$ DIRECT SUCCESS
   REUSED warm T=
   COMPILED warm-compiled T=
   s" alternate" s" app-hb" RUN-IN SUCCESS
   OUT$ s" 4" CONTAINS? TTRUE ;

: ALTERNATE-CASES ( -- )
   [: s" native-file-storage-engine" WHITEBOX-CHILD:PROVIDE
      ALTERNATE-CASE ;]
   [: WHITEBOX-CHILD:REMOVE ;] finally ;

: SPAWN ( ptr u8 n ptr u8 n ptr u8 n -- pid )
   {: app:ptr appu:n out:ptr outu:n log:ptr logu:n :}
   NATIVE-FILE-APP:BUILDER$ {: builder:ptr builderu:n :}
   s" race" s" drive" IN FS:>PATHZ open-rd {: in:n :}
   in 0 < if E-FS-OPEN throw then
   s" race" log logu IN FS:OPEN-APPEND-FD {: fd:n :}
   s" race" app appu out outu ENGINE-CANDIDATE:PATH$ STAGE
   builder builderu >LEN in >FD fd >FD fd >FD PROC-SPAWN-ARGV-ENV-IO
   in close
   fd close ;

: EXITED ( pid -- n )
   PROC-WAIT-RC MATCH result
      ok OF ENDOF
      err OF ENDOF
   ;MATCH ;

\ 3. Two builds racing into one root leave every object each made: a key
\ covers the requiring file. A rebuild of each reuses all of its own, and the
\ products match a serial pair's in a fresh root.
: CONCURRENT-CASE ( -- )
   s" concurrent builds share one root" T-LABEL
   s" race" CASE-DIR
   s" race" s" lib-a.f" LIB-A$ WRITE-IN
   s" race" s" lib-b.f" LIB-B$ WRITE-IN
   s" race" s" app-a.f"
      S\" require lib-a.f\nrequire lib-b.f\n: MAIN ( -- ) STORE-A:A-TWO . ;\n" WRITE-IN
   s" race" s" app-b.f"
      S\" require lib-a.f\nrequire lib-b.f\n: MAIN ( -- ) STORE-B:B-THREE . ;\n" WRITE-IN
   s" race" s" drive" SAVED-SOURCE$ WRITE-IN
   s" app-a.f" s" a-hb" s" log-a" SPAWN {: pa:pid :}
   s" app-b.f" s" b-hb" s" log-b" SPAWN {: pb:pid :}
   pa EXITED 0 T=
   pb EXITED 0 T=
   s" race" s" a-hb" IN KEEP-A
   s" race" s" b-hb" IN KEEP-B
   s" race" s" app-a.f" s" a-hb" SAVED SUCCESS
   REUSED 3 T=
   COMPILED 0 T=
   s" race" s" a-hb" RUN-IN SUCCESS
   OUT$ s" 11" CONTAINS? TTRUE
   s" race" s" app-b.f" s" b-hb" SAVED SUCCESS
   REUSED 3 T=
   COMPILED 0 T=
   s" race" s" b-hb" RUN-IN SUCCESS
   OUT$ s" 25" CONTAINS? TTRUE
   s" race" s" serial-out" OUT-IN
   s" race" s" app-a.f" s" a-hb" SAVED SUCCESS
   s" race" s" app-b.f" s" b-hb" SAVED SUCCESS
   s" race" s" a-hb" IN SAME-A? TTRUE
   s" race" s" b-hb" IN SAME-B? TTRUE ;

\ 4. A flipped byte in every object is refused and rebuilt.
: BYTE-CASE ( -- )
   s" a corrupt object is refused and rebuilt" T-LABEL
   s" flip" CASE-DIR
   s" flip" s" lib.f" LIB-V1$ WRITE-IN
   s" flip" s" app.f" APP$ WRITE-IN
   s" flip" SAVED-APP SUCCESS
   COMPILED {: cold:n :}
   s" flip" SAVED-APP SUCCESS
   REUSED 2 T=
   FLIP-ALL 2 T=
   s" flip" s" app.f" APP-EDITED$ WRITE-IN
   s" flip" SAVED-APP SUCCESS
   REFUSED? TTRUE
   ERR$ s" : checksum" CONTAINS? TTRUE
   COMPILED cold T=
   s" flip" s" app-hb" RUN-IN SUCCESS
   OUT$ s" 4" CONTAINS? TTRUE
   s" flip" SAVED-APP SUCCESS
   REFUSED? TFALSE
   REUSED 2 T=
   COMPILED 0 T= ;

public
: MAIN ( -- )
   T-RESET
   s" native-file-storage" TREE
   KEEP-CASE
   ALTERNATE-CASES
   CONCURRENT-CASE
   BYTE-CASE
   T-REPORT ;
;package
;using

NATIVE-FILE-STORAGE-TEST:MAIN

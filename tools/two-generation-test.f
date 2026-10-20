require src/lib/string.f
require src/lib/test.f
require src/lib/engine-candidate.f
require src/lib/fmt.f
require tools/two-generation-core.f

package TWO-GEN
private

create TGT-ROOT FS-PATH-CAP allot
variable TGT-ROOT-U
create TGT-A FS-PATH-CAP allot
variable TGT-A-U
create TGT-B FS-PATH-CAP allot
variable TGT-B-U
create TGT-M FS-PATH-CAP allot
variable TGT-M-U
create TGT-HOST FS-PATH-CAP allot
variable TGT-HOST-U
create TGT-INHERITED FS-PATH-CAP allot
variable TGT-INHERITED-U
create TGT-CACHE-A FS-PATH-CAP allot
variable TGT-CACHE-A-U
create TGT-CACHE-B FS-PATH-CAP allot
variable TGT-CACHE-B-U
67 constant TGT-UNCAUGHT-RC                 \ hb's exit status for an uncaught throw

: TGT-A$ ( -- ptr u8 n ) TGT-A TGT-A-U @ ;
: TGT-B$ ( -- ptr u8 n ) TGT-B TGT-B-U @ ;
: TGT-M$ ( -- ptr u8 n ) TGT-M TGT-M-U @ ;
: TGT-ROOT$ ( -- ptr u8 n ) TGT-ROOT TGT-ROOT-U @ ;
: TGT-HOST$ ( -- ptr u8 n ) TGT-HOST TGT-HOST-U @ ;
: TGT-INHERITED$ ( -- ptr u8 n ) TGT-INHERITED TGT-INHERITED-U @ ;
: TGT-CACHE-A$ ( -- ptr u8 n ) TGT-CACHE-A TGT-CACHE-A-U @ ;
: TGT-CACHE-B$ ( -- ptr u8 n ) TGT-CACHE-B TGT-CACHE-B-U @ ;

: TGT-PREP ( -- )
   CLEANUP-RESET
   s" habu-two-generation" HB-TMP-MKDIR {: a:ptr u:n :}
   a TGT-ROOT u BYTE-COPY u TGT-ROOT-U !
   TGT-ROOT$ CLEANUP-TREE+
   TGT-ROOT$ s" a" TGT-A JOIN-PATH TGT-A-U !
   TGT-ROOT$ s" b" TGT-B JOIN-PATH TGT-B-U !
   TGT-ROOT$ s" missing" TGT-M JOIN-PATH TGT-M-U !
   TGT-ROOT$ s" host" TGT-HOST JOIN-PATH TGT-HOST-U !
   TGT-ROOT$ s" inherited" TGT-INHERITED JOIN-PATH TGT-INHERITED-U ! ;

: TGT-COMPARE ( n n -- ) {: count:n first:n :}
   TGT-A$ TGT-B$ TG-BYTE-DIFF count T=
   TG-FIRST-DIFF @ first T= ;

: TGT-BYTES ( -- )
   s" whole-byte comparison covers empty, unequal length and chunk boundaries" T-LABEL
   TGT-A$ s" " WRITE-ALL TGT-B$ s" " WRITE-ALL
   0 -1 TGT-COMPARE
   TGT-B$ s" x" WRITE-ALL -1 0 TGT-COMPARE
   TGT-A$ s" abcd" WRITE-ALL TGT-B$ s" abXd" WRITE-ALL
   1 2 TGT-COMPARE
   TGT-B$ s" aX" WRITE-ALL -1 1 TGT-COMPARE
   TG-CMP-CAP 0 ?do 0 TG-CMP-A i + c! loop
   TGT-A$ TG-CMP-A TG-CMP-CAP WRITE-ALL
   TGT-B$ TG-CMP-A TG-CMP-CAP WRITE-ALL
   TGT-A$ s" tail" APPEND-FILE TGT-B$ s" taiX" APPEND-FILE
   1 TG-CMP-CAP 3 + TGT-COMPARE
   TG-CMP-CAP 0 ?do 0 TG-CMP-A i + c! loop
   TGT-B$ TG-CMP-A TG-CMP-CAP WRITE-ALL
   -1 TG-CMP-CAP TGT-COMPARE ;

: TGT-MAP$ ( -- ptr u8 n )
   TGT-ROOT$ s" a.map" TG-MAP-PATH JOIN-PATH
   TG-MAP-PATH swap ;

: TGT-NAMES ( -- )
   s" sidecar owners use column names and half-open code spans" T-LABEL
   TGT-A$ 12 TG-MAP-NAME$ s" " T$=
   TGT-MAP$
   S\" habu-names 1\ncolumns rec named start len wid name\n0 1 0 16 0 FIRST\n1 0 24 2147483656 3 HIDDEN\n2 1 4 200 -1 PACKAGE\n" WRITE-ALL
   TGT-A$ 0 TG-MAP-NAME$ s" FIRST" T$=
   TGT-A$ 19 TG-MAP-NAME$ s" FIRST" T$=
   TGT-A$ 20 TG-MAP-NAME$ s" " T$=
   TGT-A$ 24 TG-MAP-NAME$ s" HIDDEN" T$=
   TGT-A$ 32 TG-MAP-NAME$ s" " T$=
   TGT-MAP$
   S\" habu-names 1\ncolumns name extra len wid start\nMOVED ignored 2147483656 3 24\n" WRITE-ALL
   TGT-A$ 27 TG-MAP-NAME$ s" MOVED" T$=
   TGT-MAP$ S\" habu-names 2\ncolumns start len wid name\n0 8 0 NEW\n" WRITE-ALL
   TGT-A$ 0 TG-MAP-NAME$ s" " T$=
   TGT-MAP$ S\" habu-names 1\ncolumns start len name\n0 8 BAD\n" WRITE-ALL
   TGT-A$ 0 TG-MAP-NAME$ s" " T$=
   TGT-MAP$ S\" habu-names 1\ncolumns start len wid name\n0 invalid 0 BAD\n" WRITE-ALL
   TGT-A$ 0 TG-MAP-NAME$ s" " T$= ;

: TGT-IMAGE ( -- )
   s" image reader supplies the sidecar's file-coordinate base" T-LABEL
   ENGINE-CANDIDATE:PATH$ IMAGE-SIZE:MEASURE
   IMAGE-SIZE:CODE-BLOB-RANGE {: at:n len:n :}
   at 0 > TTRUE len 0 > TTRUE
   at len + ENGINE-CANDIDATE:PATH$ FILE-SIZE < TTRUE ;

\ --compare of file a against the given path: stdout and stderr lengths, rc.
: TGT-CLI-RUN ( ptr u8 n -- len len n ) {: b:ptr bu:n :}
   PROC-ARGV-RESET PROC-ENV-RESET PROC-ENV-INHERIT-MISSING
   s" --load" >LEN PROC-ARGV+
   s" tools/two-generation-build.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+ s" --compare" >LEN PROC-ARGV+
   TGT-A$ >LEN PROC-ARGV+ b bu >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN
   TG-OUT TG-CAP-OUT >LEN TG-ERR TG-CAP-OUT >LEN 10000 >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N ;

: TGT-CLI ( n -- ) {: want:n :}
   TGT-B$ TGT-CLI-RUN {: outu:len erru:len rc:n :}
   rc want T= erru LEN>N 0 T=
   want 0= if
      TG-OUT outu LEN>N s" 0 differing bytes" CONTAINS? TTRUE
   else
      TG-OUT outu LEN>N s" length mismatch; first differing offset 32768" CONTAINS? TTRUE
   then ;

\ The line the engine prints for an uncaught code, rendered from the name so the
\ needle follows src/lib/errors.f instead of repeating its number.
: TGT-FAILED$ ( -- ptr u8 n )
   SB-RESET
   s" hb: uncaught throw code " SB-APPEND
   E-FS-OPEN FMT:SB-INT
   SB$ ;

\ A missing file is no verdict: MAIN rethrows the step's code, so the engine
\ names it on stderr and exits with its status for an uncaught throw.
: TGT-CLI-MISSING ( -- )
   s" a failed step's throw reaches the engine on stderr" T-LABEL
   TGT-M$ TGT-CLI-RUN {: outu:len erru:len rc:n :}
   rc TGT-UNCAUGHT-RC T= outu LEN>N 0 T=
   TG-ERR erru LEN>N TGT-FAILED$ CONTAINS? TTRUE ;

\ The host launcher only invokes this Habu child. The child's file records the
\ cache selected for an actual TG-BUILD spawn; its marker makes reuse visible.
: TGT-CACHE-CHILD ( -- )
   s" HABU_BUILD_CACHE" GETENV {: root:ptr rootu:n :}
   rootu 0= if s" two-gen-test: no cache" 1 die then
   root rootu s" seen" TG-PATH JOIN-PATH {: pathu:n :}
   TG-PATH pathu EXISTS? if s" two-gen-test: dirty cache" 1 die then
   root rootu MAKE-DIRS
   TG-PATH pathu s" seen" WRITE-ALL
   4 SCRIPT-ARGV$ root rootu WRITE-ALL ;

: TGT-CACHE-HOST ( -- )
   SB-RESET
   S\" #!/bin/sh\nexec \q" SB-APPEND
   ENGINE-CANDIDATE:PATH$ SB-APPEND
   S\" \q --load tools/two-generation-test.f -- cache-child \q$@\q\n" SB-APPEND
   TGT-HOST$ SB$ WRITE-ALL
   TGT-HOST$ CHMOD-X ;

: TGT-CACHE-READ ( n ptr u8 ptr n -- ) {: g:n buf:ptr size:ptr :}
   g TG-GEN$ buf FS-PATH-CAP READ-ALL size ! ;

: TGT-CACHE ( -- )
   s" each build child starts in a private empty cache" T-LABEL
   TGT-CACHE-HOST
   TGT-ROOT$ {: root:ptr rootu:n :}
   root TG-ROOT rootu BYTE-COPY
   rootu TG-ROOT-U !
   TGT-HOST$ TG-HOST!
   TGT-INHERITED$ MAKE-DIR
   TGT-INHERITED$ s" seen" TG-PATH JOIN-PATH {: pathu:n :}
   TG-PATH pathu s" old" WRITE-ALL
   PROC-ENV-DEFAULT-RESET
   s" HABU_BUILD_CACHE" >LEN TGT-INHERITED$ >LEN PROC-ENV-DEFAULT+
   1 TG-BUILD {: err1:len rc1:n :}
   rc1 0 T=
   rc1 0<> if PROC-ENV-DEFAULT-RESET exit then
   err1 LEN>N 0 T=
   2 TG-BUILD {: err2:len rc2:n :}
   rc2 0 T=
   rc2 0<> if PROC-ENV-DEFAULT-RESET exit then
   err2 LEN>N 0 T=
   PROC-ENV-DEFAULT-RESET
   1 TGT-CACHE-A TGT-CACHE-A-U TGT-CACHE-READ
   2 TGT-CACHE-B TGT-CACHE-B-U TGT-CACHE-READ
   TGT-CACHE-A$ TGT-ROOT$ STARTS-WITH? TTRUE
   TGT-CACHE-B$ TGT-ROOT$ STARTS-WITH? TTRUE
   TGT-CACHE-A$ TGT-CACHE-B$ STR= TFALSE
   TGT-CACHE-A$ DIR? TTRUE
   TGT-CACHE-B$ DIR? TTRUE ;

: TGT-RUN ( -- )
   T-RESET TGT-PREP TGT-BYTES TGT-NAMES TGT-IMAGE
   s" the command refuses unequal products and accepts equal ones" T-LABEL
   1 TGT-CLI TGT-A$ TGT-B$ COPY-FILE-STREAM 0 TGT-CLI
   TGT-CLI-MISSING TGT-CACHE
   TG-MAP-RELEASE CLEANUP-RUN T-REPORT ;

: TGT-MAIN ( -- )
   SCRIPT-ARGC 0 > if
      0 SCRIPT-ARGV$ s" cache-child" STR= if TGT-CACHE-CHILD exit then
   then
   TGT-RUN ;

TGT-MAIN
;package

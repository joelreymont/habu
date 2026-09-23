require lib/test.f
require lib/engine-candidate.f
require tools/two-generation-core.f

package TWO-GEN
private

create TGT-ROOT FS-PATH-CAP allot
variable TGT-ROOT-U
create TGT-A FS-PATH-CAP allot
variable TGT-A-U
create TGT-B FS-PATH-CAP allot
variable TGT-B-U

: TGT-A$ ( -- ptr u8 n ) TGT-A TGT-A-U @ ;
: TGT-B$ ( -- ptr u8 n ) TGT-B TGT-B-U @ ;
: TGT-ROOT$ ( -- ptr u8 n ) TGT-ROOT TGT-ROOT-U @ ;

: TGT-PREP ( -- )
   CLEANUP-RESET
   s" habu-two-generation" HB-TMP-MKDIR {: a:ptr u:n :}
   a TGT-ROOT u BYTE-COPY u TGT-ROOT-U !
   TGT-ROOT$ CLEANUP-TREE+
   TGT-ROOT$ s" a" TGT-A JOIN-PATH TGT-A-U !
   TGT-ROOT$ s" b" TGT-B JOIN-PATH TGT-B-U ! ;

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
   TGT-ROOT$ s" a.names" TG-MAP-PATH JOIN-PATH
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
   s" bin/hb" IMAGE-SIZE:MEASURE
   IMAGE-SIZE:CODE-BLOB-RANGE {: at:n len:n :}
   at 0 > TTRUE len 0 > TTRUE
   at len + s" bin/hb" FILE-SIZE < TTRUE ;

: TGT-CLI ( n -- ) {: want:n :}
   PROC-ARGV-RESET PROC-ENV-RESET PROC-ENV-INHERIT-MISSING
   s" --load" >LEN PROC-ARGV+
   s" tools/two-generation-build.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+ s" --compare" >LEN PROC-ARGV+
   TGT-A$ >LEN PROC-ARGV+ TGT-B$ >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN
   TG-OUT TG-CAP-OUT >LEN TG-ERR TG-CAP-OUT >LEN 10000 >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   rc want T= erru LEN>N 0 T=
   want 0= if
      TG-OUT outu LEN>N s" 0 differing bytes" CONTAINS? TTRUE
   else
      TG-OUT outu LEN>N s" length mismatch; first differing offset 32768" CONTAINS? TTRUE
   then ;

: TGT-RUN ( -- )
   T-RESET TGT-PREP TGT-BYTES TGT-NAMES TGT-IMAGE
   s" the command refuses unequal products and accepts equal ones" T-LABEL
   1 TGT-CLI TGT-A$ TGT-B$ COPY-FILE-STREAM 0 TGT-CLI
   TG-MAP-RELEASE CLEANUP-RUN T-REPORT ;

TGT-RUN
;package

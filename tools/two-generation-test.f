require src/lib/string.f
require src/lib/test.f
require src/lib/engine-candidate.f
require tools/two-generation-core.f

package TWO-GEN
private

create TGT-ROOT PATH-CAP allot
variable TGT-ROOT-U
create TGT-A PATH-CAP allot
variable TGT-A-U
create TGT-B PATH-CAP allot
variable TGT-B-U

: TGT-A$ ( -- ptr u8 n ) TGT-A TGT-A-U @ ;
: TGT-B$ ( -- ptr u8 n ) TGT-B TGT-B-U @ ;
: TGT-ROOT$ ( -- ptr u8 n ) TGT-ROOT TGT-ROOT-U @ ;

: TGT-PREP ( -- )
   FS:CLEANUP-RESET
   s" habu-two-generation" FS:MKTEMP-DIR {: a:ptr u:n :}
   a TGT-ROOT u BYTE-COPY u TGT-ROOT-U !
   TGT-ROOT$ FS:CLEANUP-TREE+
   TGT-ROOT$ s" a" TGT-A FS:JOIN-PATH TGT-A-U !
   TGT-ROOT$ s" b" TGT-B FS:JOIN-PATH TGT-B-U ! ;

: TGT-COMPARE ( n n -- ) {: count:n first:n :}
   TGT-A$ TGT-B$ TG-BYTE-DIFF count T=
   TG-FIRST-DIFF @ first T= ;

: TGT-BYTES ( -- )
   s" whole-byte comparison covers empty, unequal length and chunk boundaries" T-LABEL
   TGT-A$ s" " FS:WRITE-ALL TGT-B$ s" " FS:WRITE-ALL
   0 -1 TGT-COMPARE
   TGT-B$ s" x" FS:WRITE-ALL -1 0 TGT-COMPARE
   TGT-A$ s" abcd" FS:WRITE-ALL TGT-B$ s" abXd" FS:WRITE-ALL
   1 2 TGT-COMPARE
   TGT-B$ s" aX" FS:WRITE-ALL -1 1 TGT-COMPARE
   TG-CMP-CAP 0 ?do 0 TG-CMP-A i + c! loop
   TGT-A$ TG-CMP-A TG-CMP-CAP FS:WRITE-ALL
   TGT-B$ TG-CMP-A TG-CMP-CAP FS:WRITE-ALL
   TGT-A$ s" tail" FS:APPEND-FILE TGT-B$ s" taiX" FS:APPEND-FILE
   1 TG-CMP-CAP 3 + TGT-COMPARE
   TG-CMP-CAP 0 ?do 0 TG-CMP-A i + c! loop
   TGT-B$ TG-CMP-A TG-CMP-CAP FS:WRITE-ALL
   -1 TG-CMP-CAP TGT-COMPARE ;

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
      TG-OUT outu LEN>N s" two-gen: same image" CONTAINS? TTRUE
   else
      TG-OUT outu LEN>N s" length mismatch; first differing offset 32768" CONTAINS? TTRUE
   then ;

: TGT-RUN ( -- )
   T-RESET TGT-PREP TGT-BYTES
   s" the command refuses unequal products and accepts equal ones" T-LABEL
   1 TGT-CLI TGT-A$ TGT-B$ FS:COPY-FILE-STREAM 0 TGT-CLI
   FS:CLEANUP-RUN T-REPORT ;

TGT-RUN
;package

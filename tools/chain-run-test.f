\ chain-run-test.f - hash comparison and refusal boundary for chain-run.

require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require tools/chain-run.f

package CHAIN-RUN-TEST
using SOURCE-ROOT

create ROOT FS-PATH-CAP allot
variable ROOT-U
create A FS-PATH-CAP allot
variable A-U
create B FS-PATH-CAP allot
variable B-U
create BIG-A 32769 allot
create BIG-B 32769 allot

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: A$ ( -- ptr u8 n ) A A-U @ ;
: B$ ( -- ptr u8 n ) B B-U @ ;

: COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   a dst u BYTE-COPY u up ! ;

: PREP ( -- )
   CLEANUP-RESET
   s" chain-run-test" HB-TMP-MKDIR ROOT ROOT-U COPY!
   ROOT$ CLEANUP-TREE+
   ROOT$ s" a" JOIN A A-U COPY!
   ROOT$ s" b" JOIN B B-U COPY!
   A$ s" same" WRITE-ALL
   B$ s" same" WRITE-ALL ;

: BIG-PREP ( -- )
   32769 0 ?do i 255 and BIG-A i + c! i 255 and BIG-B i + c! loop ;

: MAIN ( -- )
   T-RESET PREP
   s" equal files share a digest" T-LABEL
   A$ B$ CHAIN-RUN:SAME-FILES? TTRUE
   B$ s" changed" WRITE-ALL
   s" changed files do not share a digest" T-LABEL
   A$ B$ CHAIN-RUN:SAME-FILES? 0= TTRUE
   BIG-PREP
   A$ BIG-A 32769 WRITE-ALL
   B$ BIG-B 32769 WRITE-ALL
   s" exact comparison crosses a chunk boundary" T-LABEL
   A$ B$ CHAIN-RUN:SAME-FILES? TTRUE
   1 BIG-B 32768 + c!
   B$ BIG-B 32769 WRITE-ALL
   A$ B$ CHAIN-RUN:SAME-FILES? 0= TTRUE
   ROOT$ REMOVE-TREE
   T-REPORT ;

MAIN
;package

\ bootstrap-strip-test.f - lexer-aware recovery-source compaction.

require lib/test.f
require tools/bootstrap-strip-lib.f

package BOOTSTRIP-TEST
private

create OUT 4096 allot

: CHECK ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n want:ptr wantu:n :}
   a u OUT 4096 BOOTSTRIP:STRIP-BYTES {: got:n :}
   OUT got want wantu T$= ;

: CASES ( -- )
   s" " s" " CHECK
   s" \" s" " CHECK
   s" \word" 2dup CHECK
   s\" \\ comment\n\n: A ( -- ) 1 ;\n" s\" : A ( -- ) 1 ;\n" CHECK
   s\" : A s\" text\n\\ kept\n\" drop drop ;\n" 2dup CHECK
   s\" : A s\" text\n\n\" drop drop ;\n" 2dup CHECK
   s\" : A (\n\\ kept\n) 1 ;\n" 2dup CHECK
   s\" PRIM: 2swap PE-A PE-IN\n\\ kept\nPE-B PE-IN PRIM;\n" 2dup CHECK
   s\" : A ( -- ) 1 ; \\ trailing\n\\ drop\n: B ( -- ) 2 ;\n" s\" : A ( -- ) 1 ; \\ trailing\n: B ( -- ) 2 ;\n" CHECK ;

public
: MAIN ( -- ) T-RESET CASES T-REPORT ;
;package

BOOTSTRIP-TEST:MAIN

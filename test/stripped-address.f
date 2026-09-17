\ Address-chain admission in the real native stripped linker. Process children
\ test die paths; the executable regression lives in stripped-quotation.f.
require lib/test.f
require test/gate-common.f
require tools/aot-build.f

package AOT-LINK

create SAT-CHAIN 16 allot
get-current constant SAT-WID

: SAT-W! ( n ptr u8 -- ) {: w:n a:ptr :}
   w a c! w 8 rshift a 1+ c!
   w 16 rshift a 2 + c! w 24 rshift a 3 + c! ;

: SAT-RESET ( n -- ) {: rd:n :}
   $D2800000 rd or SAT-CHAIN SAT-W!
   $F2A00000 rd or SAT-CHAIN 4 + SAT-W!
   $F2C00000 rd or SAT-CHAIN 8 + SAT-W!
   $F2E00000 rd or SAT-CHAIN 12 + SAT-W! ;

: SAT-DECODE ( -- n )
   SAT-CHAIN SAT-CHAIN 16 + ADDRESS-VALUE ;

\ A real record and a real recorded cell inside the code region, so the refusals
\ below name a site the way the linker's own callers do.
: SAT-REC ( -- ptr n ) 0 REC ;
: SAT-SITE ( -- ptr u8 ) SAT-REC REC-CODE-PTR@ ;

: SAT-OWNER ( -- )
   s" SAT-DECODE" SAT-WID XREF-FIND-WL {: r:ptr :}
   r XREF-FOUND? TTRUE
   r XREF-START ADDRESS-OWNER r = TTRUE
   r XREF-START 4 + ADDRESS-OWNER r = TTRUE
   s" instruction interiors are not callable addresses" T-LABEL
   r XREF-START 1+ ADDRESS-OWNER XREF-FOUND? TFALSE ;

: SAT-VALID ( -- )
   SAT-OWNER
   DATA-VA VA>N DATA-ADDRESS? TTRUE
   DATA-VA VA>N DATA-SIZE + DATA-ADDRESS? TTRUE
   DATA-VA VA>N DATA-SIZE + 1+ DATA-ADDRESS? TFALSE
   s" namespace wordlist IDs are not code owners" T-LABEL
   s" AOT-LINK" XREF-NAMESPACE-WL XREF-FIND-WL XREF-START
   ADDRESS-OWNER XREF-FOUND? TFALSE
   0 SAT-RESET SAT-DECODE 0 T=
   9 SAT-RESET SAT-DECODE 0 T=
   30 SAT-RESET SAT-DECODE 0 T=
   SAT-CHAIN SAT-CHAIN 15 + ADDRESS-CHAIN? TFALSE
   SAT-CHAIN SAT-CHAIN ADDRESS-CHAIN? TFALSE
   SAT-CHAIN 1+ SAT-CHAIN ADDRESS-CHAIN? TFALSE
   \ The validator proves a stable address, including an empty buffer's end. A
   \ refusal names the site, so every call carries the owning record and the
   \ recorded cell; an admitted address reads neither.
   100 BLOB-SRC ! 116 BLOB-END !
   SAT-REC SAT-SITE 100 DATA-ADDRESS!
   SAT-REC SAT-SITE 115 DATA-ADDRESS!
   SAT-REC SAT-SITE 116 DATA-ADDRESS!
   100 BLOB-END ! SAT-REC SAT-SITE 100 DATA-ADDRESS! ;

: SAT-REFUSED ( ptr u8 n ptr u8 n -- )
   {: body:ptr bodyu:n message:ptr messageu:n :}
   GE-SRC-RESET
   s" package AOT-LINK" GE-SRC-LINE
   s" 9 SAT-RESET" GE-SRC-LINE
   body bodyu GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   74 message messageu body bodyu GE-EVAL-FORK-BAD ;

: SAT-BAD-CHAIN ( ptr u8 n -- )
   s" aot: malformed recorded address chain" SAT-REFUSED ;

: SAT-REFUSALS ( -- )
   s" $F2A0000A SAT-CHAIN 4 + SAT-W! SAT-DECODE drop" SAT-BAD-CHAIN
   s" $F2C00009 SAT-CHAIN 4 + SAT-W! SAT-DECODE drop" SAT-BAD-CHAIN
   s" $D503201F SAT-CHAIN 12 + SAT-W! SAT-DECODE drop" SAT-BAD-CHAIN
   s" SAT-CHAIN SAT-CHAIN 15 + ADDRESS-VALUE drop" SAT-BAD-CHAIN
   s" 100 BLOB-SRC ! 116 BLOB-END ! SAT-REC SAT-SITE 99 DATA-ADDRESS!"
      s" aot: address refers to data outside the restored span caller=" SAT-REFUSED
   s" 100 BLOB-SRC ! 116 BLOB-END ! SAT-REC SAT-SITE 117 DATA-ADDRESS!"
      s" aot: address refers to data outside the restored span caller=" SAT-REFUSED
   s" 0 31 EMIT-CODE-ADDRESS"
      s" aot: code address cannot use the zero register" SAT-REFUSED ;

public
: SAT-RUN ( -- )
   T-RESET
   SAT-VALID
   SAT-REFUSALS
   T-REPORT ;

;package
AOT-LINK:SAT-RUN

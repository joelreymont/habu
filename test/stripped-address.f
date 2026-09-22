\ Address-chain admission in the real native stripped linker. Process children
\ test die paths; the executable regression lives in stripped-quotation.f.
require lib/test.f
require lib/string.f
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

\ A numeric DATA address, the domain every recorded chain value and every claim
\ lives in: the byte distance from the dictionary base plus that base as a
\ number, which is how the linker turns a site pointer back into an offset.
: SAT-DATA-N ( ptr u8 -- n ) {: a:ptr :}
   a AOT-DBASE@ BYTE-VIEW - AOT-DBASE-N + ;

\ The site of a member NOTHING NAMED, which is what a span member carries when
\ the build stripped the word's name, paired with a real code address inside a
\ word whose record is right here.
: SAT-UNNAMED-SITE ( -- ptr n ptr u8 )
   XREF-NULL  s" SAT-DECODE" SAT-WID XREF-FIND-WL REC-CODE-PTR@ 4 + ;

\ ... and an address inside a buffer, which no record spells: the chain the
\ file's own code compiles spells SAT-CHAIN and adds the offset at run time.
: SAT-INTERIOR ( -- n ) SAT-CHAIN SAT-DATA-N 8 + ;

: SAT-OWNER ( -- )
   s" SAT-DECODE" SAT-WID XREF-FIND-WL {: r:ptr :}
   r XREF-FOUND? TTRUE
   r XREF-START ADDRESS-OWNER r = TTRUE
   r XREF-START 4 + ADDRESS-OWNER r = TTRUE
   s" instruction interiors are not callable addresses" T-LABEL
   r XREF-START 1+ ADDRESS-OWNER XREF-FOUND? TFALSE ;

\ THE TWO NEIGHBOUR RULES, which name a site and a target no record spells. They
\ are reading aids for the span refusal alone: the ownership answers above keep
\ their exact rule, and the tests for them stand beside these.
: SAT-NEIGHBOUR ( -- )
   s" SAT-DECODE" SAT-WID XREF-FIND-WL {: r:ptr :}
   r REC-CODE-PTR@ CODE-NEIGHBOUR r = TTRUE
   r REC-CODE-PTR@ 4 + CODE-NEIGHBOUR r = TTRUE
   s" the byte below an entry belongs to the record before it" T-LABEL
   r REC-CODE-PTR@ 1- CODE-NEIGHBOUR r <> TTRUE
   s" SAT-CHAIN" SAT-WID XREF-FIND-WL {: c:ptr :}
   SAT-CHAIN SAT-DATA-N {: at:n :}
   at DATA-CELL-OWNER c = TTRUE
   s" a byte inside the buffer is spelled by no record" T-LABEL
   SAT-INTERIOR DATA-CELL-OWNER XREF-FOUND? TFALSE
   SAT-INTERIOR DATA-NEIGHBOUR {: near:ptr nat:n :}
   near c = TTRUE
   nat at = TTRUE
   s" a value below every spelled address has no neighbour" T-LABEL
   1 DATA-NEIGHBOUR {: low:ptr lat:n :}
   low XREF-FOUND? TFALSE
   lat -1 T= ;

: SAT-VALID ( -- )
   SAT-OWNER
   SAT-NEIGHBOUR
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

\ A numeric code address back to the pointer every code writer carries, the
\ inverse of aot-closure.f CODE-N and the code-domain twin of SAT-DATA-N above.
\ It is compared, never read: MAP-TARGET! answers for addresses its closure has
\ no member for, and one below every member is one of them.
: SAT-CODE-PTR ( n -- ptr u8 ) {: at:n :}
   AOT-DBASE@ BYTE-VIEW  at AOT-DBASE-N -  + ;

\ ONE SYNTHETIC CLOSURE MEMBER, filled the way test/gate-aot-positive-lib.f's
\ AMAP case fills the walk's own tables - the rows asked for through CLO-TABLES
\ and PLAN-TABLES, then the entry order MAP-TARGET searches built by the real
\ MEMBER-ORDER - so the refusals below come out of the real writer. Member 0 is
\ SAT-REC's code, four bytes of it, so every target below names a code address
\ the member does not cover and MAP-TARGET has nothing to map it to. It
\ scribbles on the live tables and so runs in a forked child only, which is what
\ SAT-REFUSED gives each body.
: SAT-ONE-MEMBER ( -- )
   1 CLO-TABLES 1 PLAN-TABLES
   SAT-SITE 0 CLO !  4 0 CLO-LEN !  SAT-REC 0 CLO-REC !
   0 0 NEWOFF !  1 NCLO ! MEMBER-ORDER ;

\ THE THREE ANSWERS A REFUSED CODE TARGET CAN HAVE: the record that owns it, the
\ record below it as `NAME+off`, and `<unknown>`. The neighbour case needs an
\ address no record owns with a record below it, and the unaligned one is what
\ this tree can pin: a record's span ends exactly where the next record's entry
\ begins (measured - SAT-DECODE's entry plus its REC-BYTES is owned by the next
\ record), so the only aligned address no record owns lies past the LAST
\ record's span, at an offset that is that word's compiled length and moves with
\ any edit. ADDRESS-OWNER refuses an unaligned address outright (SAT-OWNER
\ above), and an ADR is the writer that meets one, its delta being in bytes
\ where a branch target is 4-byte aligned (aot-lib.f ADR-TARGET!).
: SAT-MAP-TARGET ( ptr u8 -- ) {: t:ptr :}
   SAT-ONE-MEMBER  0 t MAP-TARGET! ;
: SAT-TARGET-OWNED ( -- )
   s" SAT-DECODE" SAT-WID XREF-FIND-WL REC-CODE-PTR@ 4 + SAT-MAP-TARGET ;
: SAT-TARGET-NEAR ( -- )
   s" SAT-DECODE" SAT-WID XREF-FIND-WL REC-CODE-PTR@ 1+ SAT-MAP-TARGET ;
: SAT-TARGET-LOW ( -- )
   1 SAT-CODE-PTR SAT-MAP-TARGET ;
\ `<unknown>` for the OTHER reason it is given: a target ABOVE every record,
\ which is what a DATA address is. SAT-CHAIN is a buffer gigabytes above the
\ last word's code, and the record below it would be named with a region-sized
\ offset (test/gate-aot-positive-lib.f ADR-MEMBER caught exactly that).
: SAT-TARGET-DATA ( -- )
   SAT-CHAIN SAT-MAP-TARGET ;

\ `target-word=` ends the refusal's line, so a needle that stops at the name
\ matches a `NAME+off` answer too. The exact-owner case asks for the name AND
\ the newline behind it, which is the only thing that says no `+off` was
\ printed.
: SAT-EOL$ ( ptr u8 n -- ptr u8 n )
   SB-RESET  SB-APPEND  GE-SB-LF  SB$ ;

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
   s" 100 BLOB-SRC ! 116 BLOB-END ! SAT-UNNAMED-SITE 99 DATA-ADDRESS!"
      s" caller=SAT-DECODE+4" SAT-REFUSED
   s" 100 BLOB-SRC ! 116 BLOB-END ! SAT-REC SAT-SITE 99 DATA-ADDRESS!"
      s" target=<unknown>" SAT-REFUSED
   s" 100 BLOB-SRC ! 116 BLOB-END ! SAT-REC SAT-SITE SAT-INTERIOR DATA-ADDRESS!"
      s" target=SAT-CHAIN+8" SAT-REFUSED
   s" 0 31 EMIT-CODE-ADDRESS"
      s" aot: code address cannot use the zero register" SAT-REFUSED
   s" SAT-TARGET-OWNED"
      s" aot: PC-relative target removed or outside closure site=" SAT-REFUSED
   s" SAT-TARGET-OWNED"
      s" target-word=SAT-DECODE" SAT-EOL$ SAT-REFUSED
   s" SAT-TARGET-NEAR"
      s" target-word=SAT-DECODE+1" SAT-REFUSED
   s" SAT-TARGET-LOW"
      s" target-word=<unknown>" SAT-REFUSED
   s" SAT-TARGET-DATA"
      s" target-word=<unknown>" SAT-REFUSED ;

public
: SAT-RUN ( -- )
   T-RESET
   SAT-VALID
   SAT-REFUSALS
   T-REPORT ;

;package
AOT-LINK:SAT-RUN

\ memory-test.f - focused tests for OS-backed memory buffers.

require lib/errors.f
require lib/string.f
require lib/test.f
require test/checker-assert.f
require lib/memory.f
require lib/test/outcome.f
require lib/process-fork.f
require lib/test/subject.f
require lib/test/mmap-exhaust.f

64 constant MEMT-BUFS
16 constant MEMT-SPAN-BUFS
32 constant MEMT-SPANS
65 constant MEMT-MARK-A
90 constant MEMT-MARK-Z

variable MEMT-HERE

: MEMT-TOTAL ( -- n )
   MEMT-BUFS MEM-64K * ;

: MEMT-SPAN-TOTAL ( -- n )
   MEMT-SPAN-BUFS MEM-64K * ;

: MEMT-ZERO-BYTES ( -- )
   0 MEM-ALLOC-BYTES 2drop ;

: MEMT-NEG-BYTES ( -- )
   -1 MEM-ALLOC-BYTES 2drop ;

: MEMT-ZERO-64K ( -- )
   0 MEM-ALLOC-64K-BUFFERS 2drop ;

: MEMT-TOO-MANY-64K ( -- )
   MEM-MAX-64K-BUFFERS 1 + MEM-64K-BYTES drop ;

: MEMT-TOO-LARGE-SPAN ( -- )
   MEM-MAX-N MEM-ALLOC-64K-SPAN 2drop ;

: MEMT-ZERO-CELLS ( -- )
   0 >COUNT MEM-ALLOC-CELLS drop ;

: MEMT-TOO-MANY-CELLS ( -- )
   MEM-MAX-CELLS 1 + >COUNT MEM-CELLS>BYTES drop ;

: MEMT-END ( ptr u8 n -- ptr u8 ) {: a:ptr u :}
   a u 1 - + ;

: MEMT-TOUCH-ENDS ( ptr u8 n -- ) {: a:ptr u :}
   MEMT-MARK-A a c!
   MEMT-MARK-Z a u MEMT-END c!
   a c@ MEMT-MARK-A T=
   a u MEMT-END c@ MEMT-MARK-Z T= ;

: MEMT-TOUCH-SLOTS ( ptr u8 n n -- ) {: a:ptr u count :}
   count MEM-64K * u T=
   count 0 ?do
      MEMT-MARK-A i + a i MEM-64K * + c!
      a i MEM-64K * + c@ MEMT-MARK-A i + T=
   loop ;

: MEMT-TOUCH-64K-SLOTS ( ptr u8 n -- )
   MEMT-BUFS MEMT-TOUCH-SLOTS ;

: MEMT-TOUCH-SPAN-SLOTS ( ptr u8 n -- )
   MEMT-SPAN-BUFS MEMT-TOUCH-SLOTS ;

: MEMT-CELL-SPAN ( -- )
   4 >COUNT MEM-ALLOC-CELLS {: a:ptr :}
   111 a !
   222 a 1 cells + !
   a @ 111 T=
   a 1 cells + @ 222 T= ;

: MEMT-SINGLE-64K ( -- )
   MEM-ALLOC-64K
   dup MEM-64K T=
   MEMT-TOUCH-ENDS ;

: MEMT-MANY-64K ( -- )
   MEMT-BUFS MEM-ALLOC-64K-BUFFERS
   MEMT-TOUCH-64K-SLOTS ;

: MEMT-DATA-UNCHANGED ( -- )
   here data-base - MEMT-HERE !
   MEM-ALLOC-64K 2drop
   here data-base - MEMT-HERE @ T= ;

: MEMT-LIVE-SPAN-FRAME ( ptr u8 n n -- ) {: a:ptr u remaining :}
   u MEMT-SPAN-TOTAL T=
   a u MEMT-TOUCH-SPAN-SLOTS
   remaining 0 > if
      MEMT-SPAN-BUFS MEM-ALLOC-64K-BUFFERS
      remaining 1 - recurse
   then
   a u MEMT-TOUCH-SPAN-SLOTS ;

: MEMT-MANY-LIVE-SPANS ( -- )
   here data-base - MEMT-HERE !
   MEMT-SPAN-BUFS MEM-ALLOC-64K-BUFFERS
   MEMT-SPANS 1 - MEMT-LIVE-SPAN-FRAME
   here data-base - MEMT-HERE @ T= ;

T-RESET
MEM-64K $10000 T=
MEM-MAP-SHARED 1 T=
1 MEM-64K-BYTES MEM-64K T=
1 MEM-64K-COUNT-FOR 1 T=
MEM-64K MEM-64K-COUNT-FOR 1 T=
MEM-64K 1 + MEM-64K-COUNT-FOR 2 T=
MEM-CELL-BYTES 1 cells T=
1 >COUNT MEM-CELLS>BYTES MEM-CELL-BYTES T=
1 MEM-64K-SPAN-BYTES MEM-64K T=
MEM-64K 1 + MEM-64K-SPAN-BYTES MEM-64K 2 * T=
MEMT-CELL-SPAN
MEMT-SINGLE-64K
MEMT-MANY-64K
MEMT-DATA-UNCHANGED
MEMT-MANY-LIVE-SPANS
' MEMT-ZERO-BYTES E-MEM-SIZE TTHROWS
' MEMT-NEG-BYTES E-MEM-SIZE TTHROWS
' MEMT-ZERO-64K E-MEM-SIZE TTHROWS
' MEMT-TOO-MANY-64K E-MEM-SIZE TTHROWS
' MEMT-TOO-LARGE-SPAN E-MEM-SIZE TTHROWS
' MEMT-ZERO-CELLS E-MEM-SIZE TTHROWS
' MEMT-TOO-MANY-CELLS E-MEM-SIZE TTHROWS
T-REPORT

\ ---- B5 package MEM: typed allocation roles (MODEL-CAD-V2-PLAN.md B5.5) --------
\ numeric-result<a> is a layout value with no polymorphic eliminator yet, so each
\ classifier MATCHes the concrete role it holds and maps ok -> 0 / refusal ->
\ E-CADNUM-* (the cad-num-types-test.f idiom). The scalar words admit zero; only
\ the alloc-* sinks reject zero/overflow, and that refusal precedes any mmap.

: MEMT-BL-CODE ( CAD-NUM:numeric-result<CAD-NUM:byte-len> -- n )
   MATCH CAD-NUM:numeric-result
      ok OF drop 0 ENDOF                    negative OF E-CADNUM-NEGATIVE ENDOF
      zero OF E-CADNUM-ZERO ENDOF           overflow OF E-CADNUM-OVERFLOW ENDOF
      underflow OF E-CADNUM-UNDERFLOW ENDOF bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF
      misaligned OF E-CADNUM-MISALIGNED ENDOF
   ;MATCH ;
: MEMT-IC-CODE ( CAD-NUM:numeric-result<CAD-NUM:item-count> -- n )
   MATCH CAD-NUM:numeric-result
      ok OF drop 0 ENDOF                    negative OF E-CADNUM-NEGATIVE ENDOF
      zero OF E-CADNUM-ZERO ENDOF           overflow OF E-CADNUM-OVERFLOW ENDOF
      underflow OF E-CADNUM-UNDERFLOW ENDOF bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF
      misaligned OF E-CADNUM-MISALIGNED ENDOF
   ;MATCH ;
: MEMT-AB-CODE ( CAD-NUM:numeric-result<CAD-NUM:alloc-byte-len> -- n )
   MATCH CAD-NUM:numeric-result
      ok OF drop 0 ENDOF                    negative OF E-CADNUM-NEGATIVE ENDOF
      zero OF E-CADNUM-ZERO ENDOF           overflow OF E-CADNUM-OVERFLOW ENDOF
      underflow OF E-CADNUM-UNDERFLOW ENDOF bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF
      misaligned OF E-CADNUM-MISALIGNED ENDOF
   ;MATCH ;
: MEMT-AC-CODE ( CAD-NUM:numeric-result<CAD-NUM:alloc-cell-count> -- n )
   MATCH CAD-NUM:numeric-result
      ok OF drop 0 ENDOF                    negative OF E-CADNUM-NEGATIVE ENDOF
      zero OF E-CADNUM-ZERO ENDOF           overflow OF E-CADNUM-OVERFLOW ENDOF
      underflow OF E-CADNUM-UNDERFLOW ENDOF bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF
      misaligned OF E-CADNUM-MISALIGNED ENDOF
   ;MATCH ;

\ scalar sizing: raw n -> zero-admitting role -> MEM word -> class
: MEMT-CELLS>BYTES# ( n -- n ) CAD-NUM:CELL-COUNT
   MATCH CAD-NUM:numeric-result ok OF MEM:CELLS>BYTES MEMT-BL-CODE ENDOF
      negative OF E-CADNUM-NEGATIVE ENDOF zero OF E-CADNUM-ZERO ENDOF
      overflow OF E-CADNUM-OVERFLOW ENDOF underflow OF E-CADNUM-UNDERFLOW ENDOF
      bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF misaligned OF E-CADNUM-MISALIGNED ENDOF ;MATCH ;
: MEMT-64K-BYTES# ( n -- n ) CAD-NUM:ITEM-COUNT
   MATCH CAD-NUM:numeric-result ok OF MEM:64K-BYTES MEMT-BL-CODE ENDOF
      negative OF E-CADNUM-NEGATIVE ENDOF zero OF E-CADNUM-ZERO ENDOF
      overflow OF E-CADNUM-OVERFLOW ENDOF underflow OF E-CADNUM-UNDERFLOW ENDOF
      bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF misaligned OF E-CADNUM-MISALIGNED ENDOF ;MATCH ;
: MEMT-64K-COUNT-FOR# ( n -- n ) CAD-NUM:BYTE-LEN
   MATCH CAD-NUM:numeric-result ok OF MEM:64K-COUNT-FOR MEMT-IC-CODE ENDOF
      negative OF E-CADNUM-NEGATIVE ENDOF zero OF E-CADNUM-ZERO ENDOF
      overflow OF E-CADNUM-OVERFLOW ENDOF underflow OF E-CADNUM-UNDERFLOW ENDOF
      bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF misaligned OF E-CADNUM-MISALIGNED ENDOF ;MATCH ;
: MEMT-64K-SPAN# ( n -- n ) CAD-NUM:BYTE-LEN
   MATCH CAD-NUM:numeric-result ok OF MEM:64K-SPAN-BYTES MEMT-BL-CODE ENDOF
      negative OF E-CADNUM-NEGATIVE ENDOF zero OF E-CADNUM-ZERO ENDOF
      overflow OF E-CADNUM-OVERFLOW ENDOF underflow OF E-CADNUM-UNDERFLOW ENDOF
      bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF misaligned OF E-CADNUM-MISALIGNED ENDOF ;MATCH ;

\ allocation narrowing: raw n -> role -> AS-ALLOC-* -> class (never allocates on refuse)
: MEMT-BYTES-ALLOC# ( n -- n ) CAD-NUM:BYTE-LEN
   MATCH CAD-NUM:numeric-result ok OF CAD-NUM:AS-ALLOC-BYTE-LEN MEMT-AB-CODE ENDOF
      negative OF E-CADNUM-NEGATIVE ENDOF zero OF E-CADNUM-ZERO ENDOF
      overflow OF E-CADNUM-OVERFLOW ENDOF underflow OF E-CADNUM-UNDERFLOW ENDOF
      bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF misaligned OF E-CADNUM-MISALIGNED ENDOF ;MATCH ;
: MEMT-CELLS-ALLOC# ( n -- n ) CAD-NUM:CELL-COUNT
   MATCH CAD-NUM:numeric-result ok OF CAD-NUM:AS-ALLOC-CELL-COUNT MEMT-AC-CODE ENDOF
      negative OF E-CADNUM-NEGATIVE ENDOF zero OF E-CADNUM-ZERO ENDOF
      overflow OF E-CADNUM-OVERFLOW ENDOF underflow OF E-CADNUM-UNDERFLOW ENDOF
      bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF misaligned OF E-CADNUM-MISALIGNED ENDOF ;MATCH ;

\ real typed allocation via the shared MEM narrowing helpers: MEM:BYTES-ALLOC-LEN
\ / MEM:CELLS-ALLOC-COUNT take a raw n straight to the validated alloc role and
\ throw E-MEM-SIZE on any refusal before an mmap (TTHROWSQ cases drive the
\ zero/negative/overflow refusals in MEM's TEST-ALLOC-ROLES).

\ The returned alloc role's raw projection is MEM-private, so the tests touch
\ memory over the KNOWN raw extent and drop the role.
\ EXEMPLAR MIGRATION: a bounded-lifetime scratch mapping that formerly leaked
\ (`MEM:ALLOC-BYTES drop {: a:ptr :}` used the buffer then dropped its length, so it
\ could never be released) now scopes the mapping in MEM:WITH-BYTES - written+read end
\ to end inside the body, released on scope exit. The write/read body is a named word
\ because a quotation cannot hold locals.
: MEMT-TYPED-BYTES-BODY ( ptr u8 CAD-NUM:alloc-byte-len -- ) {: a:ptr len :}
   MEMT-MARK-A a c!  MEMT-MARK-Z a MEM-64K 1 - + c!
   a c@ MEMT-MARK-A T=  a MEM-64K 1 - + c@ MEMT-MARK-Z T= ;
: MEMT-TYPED-BYTES ( -- )                    \ typed byte alloc is writable end to end, scoped (no leak)
   MEM-64K MEM:BYTES-ALLOC-LEN [: MEMT-TYPED-BYTES-BODY ;] MEM:WITH-BYTES ;
: MEMT-TYPED-64K ( -- )                      \ typed single-64K alloc is writable end to end
   MEM:ALLOC-64K drop {: a:ptr :}
   MEMT-MARK-A a c!  MEMT-MARK-Z a MEM-64K 1 - + c!
   a c@ MEMT-MARK-A T=  a MEM-64K 1 - + c@ MEMT-MARK-Z T= ;
: MEMT-TYPED-CELLS ( -- )                    \ typed cell alloc stores/fetches two cells
   4 MEM:CELLS-ALLOC-COUNT MEM:ALLOC-CELLS {: a:ptr :}
   111 a !  222 a 1 cells + !
   a @ 111 T=  a 1 cells + @ 222 T= ;

\ ---- purified MEM:64K-COUNT-FOR: byte-identical parity with the legacy raw word -
\ MEM:64K-COUNT-FOR now composes CAD-NUM:DIV-BYTES-CEIL and reads no raw cell.
\ Pin its EXACT item-count at the boundary inputs 1 / 64K / 64K+1 / MAX-N so the
\ purification stays behavior-identical: the ceil counts match the legacy raw
\ MEM-64K-COUNT-FOR at 1 / 64K / 64K+1, and at MAX-N the pure scalar word returns
\ the true ceil (MEM-MAX-64K-BUFFERS+1), where the raw word instead throws its
\ own bundled buffer ceiling (that allocation ceiling now lives at the alloc sink).
package CAD-NUM
public
: IC>RAW ( CAD-NUM:item-count -- n ) ITEM-COUNT>N ;   \ test-only white-box reader
;package

: MEMT-64K-COUNT ( n -- n ) CAD-NUM:BYTE-LEN          \ raw bytes -> typed ceil count value
   MATCH CAD-NUM:numeric-result ok OF MEM:64K-COUNT-FOR
      MATCH CAD-NUM:numeric-result ok OF CAD-NUM:IC>RAW ENDOF
         negative OF E-MEM-SIZE throw ENDOF zero OF E-MEM-SIZE throw ENDOF
         overflow OF E-MEM-SIZE throw ENDOF underflow OF E-MEM-SIZE throw ENDOF
         bad-alignment OF E-MEM-SIZE throw ENDOF misaligned OF E-MEM-SIZE throw ENDOF ;MATCH ENDOF
      negative OF E-MEM-SIZE throw ENDOF zero OF E-MEM-SIZE throw ENDOF overflow OF E-MEM-SIZE throw ENDOF
      underflow OF E-MEM-SIZE throw ENDOF bad-alignment OF E-MEM-SIZE throw ENDOF misaligned OF E-MEM-SIZE throw ENDOF ;MATCH ;

: MEMT-COUNT-PARITY ( -- )                            \ exact-count regression at 1/64K/64K+1/MAX-N
   1 MEMT-64K-COUNT 1 T=
   MEM-64K MEMT-64K-COUNT 1 T=
   MEM-64K 1 + MEMT-64K-COUNT 2 T=
   MEM-MAX-N MEMT-64K-COUNT MEM-MAX-64K-BUFFERS 1 + T= ;

\ ---- typed release and range unmap ---------------------------------------------
\ Package-local fixtures exercise MEM only through its public seams.
package MEM-TEST
private

$86 constant MEMT-FAULT-RC
$1000 constant RBT-CAP
1000 constant RBT-TIMEOUT-MS
-7777 constant E-PRIMARY

create RBT-OUT RBT-CAP allot
create RBT-ERR RBT-CAP allot

: RBT-LEN ( n -- CAD-NUM:byte-len )
   CAD-NUM:BYTE-LEN MATCH CAD-NUM:numeric-result
      ok OF ENDOF
      negative OF E-PRIMARY throw ENDOF
      zero OF E-PRIMARY throw ENDOF
      overflow OF E-PRIMARY throw ENDOF
      underflow OF E-PRIMARY throw ENDOF
      bad-alignment OF E-PRIMARY throw ENDOF
      misaligned OF E-PRIMARY throw ENDOF
   ;MATCH ;

: RBT-SUB-OK? ( CAD-NUM:byte-len CAD-NUM:byte-len -- bool )
   CAD-NUM:SUB-BYTES MATCH CAD-NUM:numeric-result
      ok OF drop true ENDOF
      negative OF false ENDOF
      zero OF true ENDOF
      overflow OF false ENDOF
      underflow OF false ENDOF
      bad-alignment OF false ENDOF
      misaligned OF false ENDOF
   ;MATCH ;

: RBT-BYTES= ( CAD-NUM:byte-len CAD-NUM:byte-len -- bool )
   2dup RBT-SUB-OK? >r swap RBT-SUB-OK? r> and ;

: MEMT-EXIT0 ( -- )
   s" " 0 die ;

: MEMT-FORK ( [ -- ] -- outcome )
   {: body :} \ typed-local-lint: allow-bare-local - body keeps the quotation effect.
   PROC-FORK:CHECKED {: pid:pid :}
   pid PID>N 0= if
      body execute
      MEMT-EXIT0
   then
   pid PROC-WAIT-OUTCOME ;

: MBT-OPEN ( n -- MEM:block )
   MEM:BYTES-ALLOC-LEN MEM:OPEN
   MATCH result
      ok OF ENDOF
      err OF throw ENDOF
   ;MATCH ;

: MBT-ENDS ( -- )
   MEM-64K 1 + {: want:n :}
   want MBT-OPEN
   MEM:BORROW {: buf:ptr len:CAD-NUM:byte-len :}
   len want RBT-LEN RBT-BYTES= 0= if E-PRIMARY throw then
   MEMT-MARK-A buf c!
   MEMT-MARK-Z buf want 1 - + c!
   buf c@ MEMT-MARK-A <> if E-PRIMARY throw then
   buf want 1 - + c@ MEMT-MARK-Z <> if E-PRIMARY throw then
   MEM:RELEASE ;

: MBT-BASE-CHILD ( -- )
   MEM-64K 2 * MBT-OPEN
   MEM:BORROW {: buf:ptr len:CAD-NUM:byte-len :}
   len drop
   buf MEM-64K + MEM-64K munmap
   dup 0<> if E-MEM-UNMAP throw then
   drop
   MEM:RELEASE
   MEMT-EXIT0 ;

: MBT-UNMAP-CHILD ( -- )
   MEM-64K MBT-OPEN
   MEM:BORROW {: buf:ptr len:CAD-NUM:byte-len :}
   len drop
   MEM:RELEASE
   2 close
   buf c@ drop
   MEMT-EXIT0 ;

: MBT-OVERFLOW ( -- )
   MEM-MAX-N MEM-CELL-BYTES - MEM-CELL-BYTES / MEM-CELL-BYTES * 1 +
   MEM:BYTES-ALLOC-LEN MEM:OPEN
   MATCH result
      err OF E-MEM-SIZE <> if E-PRIMARY throw then ENDOF
      ok OF MEM:RELEASE E-PRIMARY throw ENDOF
   ;MATCH ;

: MBT-MAP-ERR-CHILD ( -- )
   MEM-64K MEM-CELL-BYTES + MMAP-TEST:EXHAUST-CHILD
   MEM-64K MEM:BYTES-ALLOC-LEN MEM:OPEN
   MATCH result
      err OF E-MEM-MAP <> if E-PRIMARY throw then ENDOF
      ok OF MEM:RELEASE E-PRIMARY throw ENDOF
   ;MATCH
   MEMT-EXIT0 ;

: TEST-BLOCK ( -- )
   T-RESET
   MBT-ENDS
   MBT-OVERFLOW
   [: MBT-BASE-CHILD ;] MEMT-FORK 0 T-OUTCOME-EXITED=
   [: MBT-MAP-ERR-CHILD ;] MEMT-FORK 0 T-OUTCOME-EXITED=
   [: MBT-UNMAP-CHILD ;] MEMT-FORK MEMT-FAULT-RC T-OUTCOME-EXITED=
   T-REPORT ;

: MBT-SEALED-SRC ( -- ptr u8 n )
   s" package MEM private EXPORT B-MINT ;package" ;

: TEST-SEALED ( -- )
   T-RESET
   MBT-SEALED-SRC RBT-OUT RBT-CAP >LEN RBT-ERR RBT-CAP >LEN
   RBT-TIMEOUT-MS >MS SUBJECT:RUN
   ENGINE-ERROR:SEAL-PACKAGE T-OUTCOME-EXITED=
   LEN>N drop
   LEN>N drop
   T-REPORT ;

: MBT-SEAL-ACTION ( -- [ -- ] )
   [: TEST-SEALED ;] ;

: RBT-RELEASED-BASE ( -- ptr u8 )
   MEM-64K 2 * MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES
   over {: base:ptr :}
   MEMT-MARK-A base c!
   MEMT-MARK-Z base MEM-64K 2 * 1 - + c!
   MEM:RELEASE-BYTES
   base ;

: RBT-RELEASE-FIRST ( -- )
   RBT-RELEASED-BASE c@ drop ;

: RBT-RELEASE-LAST ( -- )
   RBT-RELEASED-BASE MEM-64K 2 * 1 - + c@ drop ;

: RBT-RELEASE-OUTERS ( ptr u8 -- ) {: base:ptr :}
   base MEM-64K RBT-LEN MEM:UNMAP
   base MEM-64K 3 * + MEM-64K RBT-LEN MEM:UNMAP ;

: RBT-ONE-CLEANUP ( ptr u8 -- ) {: base:ptr :}
   base MEM-64K RBT-LEN MEM:UNMAP
   base MEM-64K 2 * + MEM-64K RBT-LEN MEM:UNMAP ;

: RBT-ONE-MAP ( -- ptr u8 )
   MEM-64K 3 * MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES
   drop {: base:ptr :}
   MEMT-MARK-A base MEM-64K 1 - + c!
   MEMT-MARK-Z base MEM-64K 2 * + c!
   base MEM-64K + MEM-64K RBT-LEN MEM:UNMAP
   base ;

: RBT-ONE-GUARDS ( -- )
   RBT-ONE-MAP {: base:ptr :}
   base MEM-64K 1 - + c@ MEMT-MARK-A <> if E-PRIMARY throw then
   base MEM-64K 2 * + c@ MEMT-MARK-Z <> if E-PRIMARY throw then
   base RBT-ONE-CLEANUP ;

: RBT-ONE-FAULT ( -- )
   RBT-ONE-MAP MEM-64K + c@ drop ;

: RBT-UNMAP-MID ( -- ptr u8 )
   MEM-64K 4 * MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES
   drop {: base:ptr :}
   MEMT-MARK-A base MEM-64K 1 - + c!
   MEMT-MARK-Z base MEM-64K 3 * + c!
   base MEM-64K + MEM-64K 2 * RBT-LEN MEM:UNMAP
   base ;

: RBT-UNMAP-LEFT ( -- )
   RBT-UNMAP-MID {: base:ptr :}
   base MEM-64K 1 - + c@ MEMT-MARK-A <> if E-PRIMARY throw then
   base RBT-RELEASE-OUTERS ;

: RBT-UNMAP-RIGHT ( -- )
   RBT-UNMAP-MID {: base:ptr :}
   base MEM-64K 3 * + c@ MEMT-MARK-Z <> if E-PRIMARY throw then
   base RBT-RELEASE-OUTERS ;

: RBT-UNMAPPED-BASE ( -- ptr u8 )
   RBT-UNMAP-MID
   dup RBT-RELEASE-OUTERS ;

: RBT-UNMAP-FIRST ( -- )
   RBT-UNMAPPED-BASE MEM-64K + c@ drop ;

: RBT-UNMAP-LAST ( -- )
   RBT-UNMAPPED-BASE MEM-64K 3 * 1 - + c@ drop ;

: RBT-RELEASE-FIRST-SRC ( -- ptr u8 n )
   s" package MEM-TEST RBT-RELEASE-FIRST ;package" ;

: RBT-RELEASE-LAST-SRC ( -- ptr u8 n )
   s" package MEM-TEST RBT-RELEASE-LAST ;package" ;

: RBT-UNMAP-FIRST-SRC ( -- ptr u8 n )
   s" package MEM-TEST RBT-UNMAP-FIRST ;package" ;

: RBT-UNMAP-LAST-SRC ( -- ptr u8 n )
   s" package MEM-TEST RBT-UNMAP-LAST ;package" ;

: RBT-UNMAP-LEFT-SRC ( -- ptr u8 n )
   s" package MEM-TEST RBT-UNMAP-LEFT ;package" ;

: RBT-UNMAP-RIGHT-SRC ( -- ptr u8 n )
   s" package MEM-TEST RBT-UNMAP-RIGHT ;package" ;

: RBT-ONE-GUARDS-SRC ( -- ptr u8 n )
   s" package MEM-TEST RBT-ONE-GUARDS ;package" ;

: RBT-ONE-FAULT-SRC ( -- ptr u8 n )
   s" package MEM-TEST RBT-ONE-FAULT ;package" ;

: RBT-EXPECT-FAULT ( ptr u8 n -- ) {: src:ptr srcu:n :}
   src srcu RBT-OUT RBT-CAP >LEN RBT-ERR RBT-CAP >LEN
   RBT-TIMEOUT-MS >MS SUBJECT:RUN
   MEMT-FAULT-RC T-OUTCOME-EXITED=
   LEN>N 0 > TTRUE
   LEN>N 0 T= ;

: RBT-EXPECT-CLEAN ( ptr u8 n -- ) {: src:ptr srcu:n :}
   src srcu RBT-OUT RBT-CAP >LEN RBT-ERR RBT-CAP >LEN
   RBT-TIMEOUT-MS >MS SUBJECT:RUN
   0 T-OUTCOME-EXITED=
   LEN>N 0 T=
   LEN>N 0 T= ;

: RBT-EXACT-SPANS ( -- )
   T-RESET
   RBT-RELEASE-FIRST-SRC RBT-EXPECT-FAULT
   RBT-RELEASE-LAST-SRC RBT-EXPECT-FAULT
   RBT-UNMAP-LEFT-SRC RBT-EXPECT-CLEAN
   RBT-UNMAP-RIGHT-SRC RBT-EXPECT-CLEAN
   RBT-UNMAP-FIRST-SRC RBT-EXPECT-FAULT
   RBT-UNMAP-LAST-SRC RBT-EXPECT-FAULT
   RBT-ONE-GUARDS-SRC RBT-EXPECT-CLEAN
   RBT-ONE-FAULT-SRC RBT-EXPECT-FAULT
   T-REPORT ;

: RBT-ACTION ( -- [ -- ] )
   [: RBT-EXACT-SPANS ;] ;

: TEST-ALLOC-ROLES ( -- )
   T-RESET
   MEMT-COUNT-PARITY
   \ scalar sizing admits zero and positive (zero is a valid scalar answer)
   0 MEMT-CELLS>BYTES# 0 T=      1 MEMT-CELLS>BYTES# 0 T=
   0 MEMT-64K-BYTES# 0 T=        3 MEMT-64K-BYTES# 0 T=
   0 MEMT-64K-COUNT-FOR# 0 T=    1 MEMT-64K-COUNT-FOR# 0 T=   MEM-64K 1 + MEMT-64K-COUNT-FOR# 0 T=
   0 MEMT-64K-SPAN# 0 T=         MEM-64K 1 + MEMT-64K-SPAN# 0 T=
   \ allocation sinks reject zero (never allocate) and reject negative
   0 MEMT-BYTES-ALLOC# E-CADNUM-ZERO T=      -1 MEMT-BYTES-ALLOC# E-CADNUM-NEGATIVE T=
   0 MEMT-CELLS-ALLOC# E-CADNUM-ZERO T=      -1 MEMT-CELLS-ALLOC# E-CADNUM-NEGATIVE T=
   1 MEMT-BYTES-ALLOC# 0 T=                  1 MEMT-CELLS-ALLOC# 0 T=
   \ over-allocation fails at validation, BEFORE any mmap primitive is reachable
   MEM-MAX-CELLS 1 + MEMT-CELLS-ALLOC# E-CADNUM-OVERFLOW T=
   \ real typed allocations touch OS-backed memory
   MEMT-TYPED-BYTES
   MEMT-TYPED-64K
   MEMT-TYPED-CELLS
   \ shared narrowing helpers refuse zero/negative/overflow with E-MEM-SIZE
   \ before any mmap primitive is reachable
   [: 0 MEM:BYTES-ALLOC-LEN drop ;] E-MEM-SIZE TTHROWSQ
   [: -1 MEM:BYTES-ALLOC-LEN drop ;] E-MEM-SIZE TTHROWSQ
   [: 0 MEM:CELLS-ALLOC-COUNT drop ;] E-MEM-SIZE TTHROWSQ
   [: -1 MEM:CELLS-ALLOC-COUNT drop ;] E-MEM-SIZE TTHROWSQ
   [: MEM-MAX-CELLS 1 + MEM:CELLS-ALLOC-COUNT drop ;] E-MEM-SIZE TTHROWSQ
   T-REPORT ;

TEST-ALLOC-ROLES
TEST-BLOCK
MBT-SEAL-ACTION
RBT-ACTION

;package

execute
execute

\ Fatal syscall failures bypass catch and cannot print the survival byte.
package MEM-FATAL-TEST

$100 constant CAP
1000 constant TIMEOUT-MS
$47 constant FATAL-RC

create CAP-OUT CAP allot
create CAP-ERR CAP allot

: EXPECT-FATAL ( ptr u8 n -- ) {: src:ptr srcu:n :}
   src srcu CAP-OUT CAP >LEN CAP-ERR CAP >LEN TIMEOUT-MS >MS SUBJECT:RUN
   FATAL-RC T-OUTCOME-EXITED=
   LEN>N {: erru:n :}
   LEN>N {: outu:n :}
   outu 0 T=
   CAP-ERR erru s" memory: unmap failed" T$= ;

: RELEASE-SRC ( -- ptr u8 n )
   s" package MFR : RUN ( -- ) MEM:ALLOC-64K swap 1 + swap MEM:RELEASE-BYTES ; ' RUN ;package catch $53 emit" ;

: UNMAP-SRC ( -- ptr u8 n )
   s" package MFU : RUN ( -- ) MEM:ALLOC-64K drop 1 + MEM-64K STR:LENGTH MEM:UNMAP ; ' RUN ;package catch $53 emit" ;

: ZERO-SRC ( -- ptr u8 n )
   s" package MFZ : RUN ( -- ) MEM:ALLOC-64K drop 0 STR:LENGTH MEM:UNMAP ; ' RUN ;package catch $53 emit" ;

public

: RUN ( -- )
   T-RESET
   RELEASE-SRC EXPECT-FATAL
   UNMAP-SRC EXPECT-FATAL
   ZERO-SRC EXPECT-FATAL
   T-REPORT ;

;package

MEM-FATAL-TEST:RUN

\ ---- MEM:WITH-BYTES: quotation-scoped mapped memory (RAII) --------------------
\ Child processes prove release through OS-enforced mapping accessibility.
package MEM-TEST
private
PTR-VARIABLE WBT-SAVED
PTR-VARIABLE WBT-OUTER
DEFLINEAR MEM:wbt-own

s" WBT-OWN" s" ptr u8 CAD-NUM:alloc-byte-len -- MEM:wbt-own" TRUST
s" WBT-DONE" s" MEM:wbt-own --" TRUST

: WBT-PROBE-RELEASED ( -- )
   2 close
   WBT-SAVED @ c@ drop
   MEMT-EXIT0 ;

: WBT-RELEASED ( -- )
   [: WBT-PROBE-RELEASED ;] MEMT-FORK
   MATCH outcome
      exited OF MEMT-FAULT-RC <> if E-PRIMARY throw then ENDOF
      signaled OF drop E-PRIMARY throw ENDOF
      timeout OF E-PRIMARY throw ENDOF
   ;MATCH ;

: WBT-RESULT-BODY ( ptr u8 CAD-NUM:alloc-byte-len -- n )
   drop {: buf:ptr :}
   buf WBT-SAVED !
   MEMT-MARK-A buf c!
   buf c@ ;

: WBT-THROW-BODY ( ptr u8 CAD-NUM:alloc-byte-len -- )
   drop {: buf:ptr :}
   buf WBT-SAVED !
   E-PRIMARY throw ;

: WBT-SAVE-INNER ( ptr u8 CAD-NUM:alloc-byte-len -- )
   drop {: buf:ptr :}
   buf WBT-SAVED !
   MEMT-MARK-Z buf c! ;

: WBT-TOUCH-INNER ( ptr u8 CAD-NUM:alloc-byte-len -- )
   drop {: buf:ptr :}
   MEMT-MARK-Z buf c!
   buf c@ MEMT-MARK-Z <> if E-PRIMARY throw then ;

: WBT-INNER-RELEASE ( ptr u8 CAD-NUM:alloc-byte-len -- )
   drop {: outer:ptr :}
   outer WBT-OUTER !
   MEMT-MARK-A outer c!
   MEM-64K MEM:BYTES-ALLOC-LEN [: WBT-SAVE-INNER ;] MEM:WITH-BYTES
   WBT-OUTER @ c@ MEMT-MARK-A <> if E-PRIMARY throw then
   WBT-RELEASED ;

: WBT-OUTER-RELEASE ( ptr u8 CAD-NUM:alloc-byte-len -- )
   drop {: outer:ptr :}
   outer WBT-SAVED !
   MEMT-MARK-A outer c!
   MEM-64K MEM:BYTES-ALLOC-LEN [: WBT-TOUCH-INNER ;] MEM:WITH-BYTES
   outer c@ MEMT-MARK-A <> if E-PRIMARY throw then ;

: WBT-RESULT ( -- n )
   MEM-64K MEM:BYTES-ALLOC-LEN [: WBT-RESULT-BODY ;] MEM:WITH-BYTES ;

: WBT-THROW ( -- )
   MEM-64K MEM:BYTES-ALLOC-LEN [: WBT-THROW-BODY ;] MEM:WITH-BYTES ;

: WBT-NORMAL-CHILD ( -- )
   WBT-RESULT MEMT-MARK-A <> if E-PRIMARY throw then
   WBT-RELEASED
   MEMT-EXIT0 ;

: WBT-THROW-CHILD ( -- )
   [: WBT-THROW ;] catch E-PRIMARY <> if E-PRIMARY throw then
   WBT-RELEASED
   MEMT-EXIT0 ;

: WBT-INNER-CHILD ( -- )
   MEM-64K MEM:BYTES-ALLOC-LEN [: WBT-INNER-RELEASE ;] MEM:WITH-BYTES
   MEMT-EXIT0 ;

: WBT-OUTER-CHILD ( -- )
   MEM-64K MEM:BYTES-ALLOC-LEN [: WBT-OUTER-RELEASE ;] MEM:WITH-BYTES
   WBT-RELEASED
   MEMT-EXIT0 ;

: WBT-RETAINED-CHILD ( -- )
   MEM-64K MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES
   drop {: buf:ptr :}
   buf WBT-SAVED !
   MEMT-MARK-A buf c!
   WBT-PROBE-RELEASED ;

: TEST-WITH-BYTES ( -- )
   T-RESET
   [: WBT-NORMAL-CHILD ;] MEMT-FORK 0 T-OUTCOME-EXITED=
   [: WBT-THROW-CHILD ;] MEMT-FORK 0 T-OUTCOME-EXITED=
   [: WBT-INNER-CHILD ;] MEMT-FORK 0 T-OUTCOME-EXITED=
   [: WBT-OUTER-CHILD ;] MEMT-FORK 0 T-OUTCOME-EXITED=
   [: WBT-RETAINED-CHILD ;] MEMT-FORK 0 T-OUTCOME-EXITED=
   T-REPORT ;

TEST-WITH-BYTES

;package

\ ---- static rejection matrix: generic pointers accept; role swaps reject --------
\ CHECK-QUIET-CANDIDATE!: -1 accepted, 0 rejected (type error), 1 uncheckable.
package MEM-TEST
private

: STAT ( -- )
   T-RESET
   \ Real WITH-BYTES calls may mint or consume one linear owner through the
   \ quotation's result row. WBT-OWN/WBT-DONE are checker-only fixture effects.
   s" WBT-MINT ( -- MEM:wbt-own ) MEM-64K MEM:BYTES-ALLOC-LEN [: WBT-OWN ;] MEM:WITH-BYTES"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" WBT-CONSUME ( MEM:wbt-own -- ) MEM-64K MEM:BYTES-ALLOC-LEN [: 2drop WBT-DONE ;] MEM:WITH-BYTES"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" WBB-MINT ( MEM:block -- MEM:wbt-own ) [: WBT-OWN ;] MEM:WITH-BLOCK"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" WBB-CONSUME ( MEM:wbt-own MEM:block -- ) [: 2drop WBT-DONE ;] MEM:WITH-BLOCK"
      CHECK-QUIET-CANDIDATE! -1 T=
   \ positive signature controls: the exact B5.5-frozen effects resolve.
   s" G-CELLS>BYTES ( CAD-NUM:cell-count -- CAD-NUM:numeric-result<CAD-NUM:byte-len> ) MEM:CELLS>BYTES"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" G-64K-BYTES ( CAD-NUM:item-count -- CAD-NUM:numeric-result<CAD-NUM:byte-len> ) MEM:64K-BYTES"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" G-64K-COUNT-FOR ( CAD-NUM:byte-len -- CAD-NUM:numeric-result<CAD-NUM:item-count> ) MEM:64K-COUNT-FOR"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" G-64K-SPAN ( CAD-NUM:byte-len -- CAD-NUM:numeric-result<CAD-NUM:byte-len> ) MEM:64K-SPAN-BYTES"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" G-ALLOC ( CAD-NUM:alloc-byte-len -- ptr u8 CAD-NUM:alloc-byte-len ) MEM:ALLOC-BYTES"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" G-ALLOC-CELLS ( CAD-NUM:alloc-cell-count -- ptr a ) MEM:ALLOC-CELLS"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" G-AC-U8 ( CAD-NUM:alloc-cell-count -- ptr u8 ) MEM:ALLOC-CELLS"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" G-AC-BOOL ( CAD-NUM:alloc-cell-count -- ptr bool ) MEM:ALLOC-CELLS"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" G-AC-PU8 ( CAD-NUM:alloc-cell-count -- ptr ptr u8 ) MEM:ALLOC-CELLS"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" G-AC-DUP ( CAD-NUM:alloc-cell-count -- ptr u8 ptr u8 ) MEM:ALLOC-CELLS dup"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" B-AC-SPLIT ( CAD-NUM:alloc-cell-count -- ptr u8 ptr bool ) MEM:ALLOC-CELLS dup"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" G-ALLOC-64K ( -- ptr u8 CAD-NUM:alloc-byte-len ) MEM:ALLOC-64K"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" G-BYTES-ALLOC-LEN ( n -- CAD-NUM:alloc-byte-len ) MEM:BYTES-ALLOC-LEN"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" G-CELLS-ALLOC-COUNT ( n -- CAD-NUM:alloc-cell-count ) MEM:CELLS-ALLOC-COUNT"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" G-BLOCK-OPEN ( CAD-NUM:alloc-byte-len -- result<MEM:block,n> ) MEM:OPEN"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" G-BLOCK-BORROW ( MEM:block -- MEM:block ptr u8 CAD-NUM:byte-len ) MEM:BORROW"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" G-BLOCK-RELEASE ( MEM:block -- ) MEM:RELEASE"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" B-BLOCK-DUP ( MEM:block -- MEM:block MEM:block ) dup"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" B-BLOCK-DROP ( MEM:block -- ) drop"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" B-BLOCK-FORGE ( ptr u8 -- MEM:block )"
      CHECK-QUIET-CANDIDATE! 0 T=
   \ negatives: a zero-admitting role at the sink, byte<->cell role swaps, raw n.
   s" B-ZEROABLE-ALLOC ( CAD-NUM:byte-len -- ptr u8 CAD-NUM:alloc-byte-len ) MEM:ALLOC-BYTES"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" B-CELL-ROLE-BYTES ( CAD-NUM:alloc-cell-count -- ptr u8 CAD-NUM:alloc-byte-len ) MEM:ALLOC-BYTES"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" B-BYTE-ROLE-CELLS ( CAD-NUM:alloc-byte-len -- ptr a ) MEM:ALLOC-CELLS"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" B-RAW-CELLS>BYTES ( n -- CAD-NUM:numeric-result<CAD-NUM:byte-len> ) MEM:CELLS>BYTES"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" B-BYTE-ROLE-64K-BYTES ( CAD-NUM:byte-len -- CAD-NUM:numeric-result<CAD-NUM:byte-len> ) MEM:64K-BYTES"
      CHECK-QUIET-CANDIDATE! 0 T=
   \ RELEASE-BYTES accepts any typed mapping pointer with an exact allocation
   \ extent. Raw addresses and every other length role are rejected.
   s" G-REL-U8 ( ptr u8 CAD-NUM:alloc-byte-len -- ) MEM:RELEASE-BYTES"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" G-REL-BOOL ( ptr bool CAD-NUM:alloc-byte-len -- ) MEM:RELEASE-BYTES"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" G-REL-PU8 ( ptr ptr u8 CAD-NUM:alloc-byte-len -- ) MEM:RELEASE-BYTES"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" B-RELEASE-RAW-PTR ( n CAD-NUM:alloc-byte-len -- ) MEM:RELEASE-BYTES"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" B-RELEASE-RAW-LEN ( ptr u8 n -- ) MEM:RELEASE-BYTES"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" B-RELEASE-ZEROABLE-LEN ( ptr u8 CAD-NUM:byte-len -- ) MEM:RELEASE-BYTES"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" B-RELEASE-CELL-LEN ( ptr u8 CAD-NUM:alloc-cell-count -- ) MEM:RELEASE-BYTES"
      CHECK-QUIET-CANDIDATE! 0 T=
   \ UNMAP likewise accepts every typed pointer, but only with a mapped byte
   \ extent. The primitive is pointer-polymorphic and still rejects raw addresses.
   s" G-UM-U8 ( ptr u8 CAD-NUM:byte-len -- ) MEM:UNMAP"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" G-UM-BOOL ( ptr bool CAD-NUM:byte-len -- ) MEM:UNMAP"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" G-UM-PU8 ( ptr ptr u8 CAD-NUM:byte-len -- ) MEM:UNMAP"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" G-MUN-BOOL ( ptr bool n -- n ) munmap"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" B-UNMAP-RAW-PTR ( n CAD-NUM:byte-len -- ) MEM:UNMAP"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" B-MUN-RAW ( n n -- n ) munmap"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" B-UNMAP-RAW-LEN ( ptr u8 n -- ) MEM:UNMAP"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" B-UNMAP-ALLOC-LEN ( ptr u8 CAD-NUM:alloc-byte-len -- ) MEM:UNMAP"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" B-UNMAP-CELL-LEN ( ptr u8 CAD-NUM:cell-count -- ) MEM:UNMAP"
      CHECK-QUIET-CANDIDATE! 0 T=
   \ WITH-BYTES types the quotation body row: the frozen signature (row-polymorphic S
   \ threaded from the body) resolves, while a raw-n length or a cell-count role in the
   \ scoped length + body row are checker rejects, so a caller cannot scope a mapping
   \ over an unvalidated size or a byte/cell role swap.
   s" G-WITH-BYTES ( R CAD-NUM:alloc-byte-len [ R ptr u8 CAD-NUM:alloc-byte-len -- S ] -- S ) MEM:WITH-BYTES"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" B-WB-RAW-LEN ( R n [ R ptr u8 CAD-NUM:alloc-byte-len -- S ] -- S ) MEM:WITH-BYTES"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" B-WB-CELL-LEN ( R CAD-NUM:alloc-cell-count [ R ptr u8 CAD-NUM:alloc-cell-count -- S ] -- S ) MEM:WITH-BYTES"
      CHECK-QUIET-CANDIDATE! 0 T=
   T-REPORT ;

STAT

;package

s" memory-test: ok" type cr

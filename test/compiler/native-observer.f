\ native-observer.f - frozen HIR facts bound to successfully published routines.
require lib/test.f
require lib/test/outcome.f
require lib/process.f
require lib/process-argv.f
require lib/engine-candidate.f
require src/compiler/native/compiler.f
require src/core/generated-declaration.f
require src/arch/x86-64/abi.f
require src/arch/x86-64/passes.f

1 set-tier

variable NOBS-SCALAR
TYPED-VARIABLE NOBS-ENTRY NCOMP:code-entry

package NOBS-TEST
private

8 constant FUN-MAX
16 constant OWNER-MAX
64 constant ROW-MAX
8171 constant E-OBSERVE
8172 constant E-LATE

$4000 constant PUB-CAP
create PUB-OUT PUB-CAP allot
create PUB-ERR PUB-CAP allot

variable PENDING
variable P-IDX
variable P-CTX
variable P-N
FUN-MAX TYPED-BUFFER P-MUL n
1 TYPED-BUFFER HELD IR-BUILD:module
variable REFUSE
variable IGNORE
variable SEEN
variable OWNER-N
variable ROW-N
variable INVALIDATED
variable LAST-STALE
variable REPL-BEFORE
variable CLEARED-OWNER
OWNER-MAX TYPED-BUFFER OWNER-XT NCOMP:code-entry
ROW-MAX TYPED-BUFFER ROW-OWNER n
ROW-MAX TYPED-BUFFER ROW-ORD n
ROW-MAX TYPED-BUFFER ROW-OFF n
ROW-MAX TYPED-BUFFER ROW-MUL n
ROW-MAX TYPED-BUFFER ROW-LIVE n

CAST: HELD-RAW ( ptr IR-BUILD:module -- ptr n )

\ All anchors are declared before compilation; publication writes only the
\ already-declared CODE cell and preallocated scalar rows.
: DECLARE-ANCHORS ( -- )
   NOBS-ENTRY NCOMP:DECLARE-ENTRY
   OWNER-MAX 0 ?do i OWNER-XT NCOMP:DECLARE-ENTRY loop ;

: REFUSALS ( -- )
   s" raw callback effect cannot install" T-LABEL
   [: s" : NOBS-RAW-INSTALL ( [ n IR-CTX:ctx n n n -- ] -- ) NCOMP:PUBLISHED! ;" evaluate-closed ;]
   70 TTHROWSQ
   s" entry cannot enter scalar storage" T-LABEL
   [: s" : NOBS-ENTRY-SCALAR ( NCOMP:code-entry -- ) NOBS-SCALAR ! ;" evaluate-closed ;]
   70 TTHROWSQ
   s" offset cannot enter code anchor" T-LABEL
   [: s" : NOBS-OFF-ANCHOR ( n -- ) NOBS-ENTRY ! ;" evaluate-closed ;]
   70 TTHROWSQ
   s" opaque parent cannot execute" T-LABEL
   [: s" : NOBS-ENTRY-EXEC ( NCOMP:code-entry -- ) execute ;" evaluate-closed ;]
   70 TTHROWSQ ;

: FUNCTION ( IR-BUILD:module n -- IR-ID:ir-fun-id )
   {: m:IR-BUILD:module k:n :}
   m IR-BUILD:FKEY k IR-ID:PACK-FUN ;

: BLOCK ( IR-BUILD:module IR-ID:ir-fun-id n -- IR-ID:ir-block-id )
   {: m:IR-BUILD:module f:IR-ID:ir-fun-id i:n :}
   m IR-BUILD:FFUN-ROWS m IR-BUILD:FBLOCK-ROWS m IR-BUILD:FKEY f i
   IR-FUN:FBLOCK@ ;

: OP ( IR-BUILD:module IR-ID:ir-block-id n -- IR-ID:ir-op-id )
   {: m:IR-BUILD:module blk:IR-ID:ir-block-id i:n :}
   m IR-BUILD:FBLOCK-ROWS m IR-BUILD:FOP-ROWS m IR-BUILD:FKEY blk i
   IR-FUN:FOP@ ;

: MUL? ( IR-BUILD:module IR-ID:ir-op-id -- bool )
   {: m:IR-BUILD:module op:IR-ID:ir-op-id :}
   m IR-BUILD:FSYM-POOL m IR-BUILD:FSYM-ROWS
   m IR-BUILD:FOP-ROWS m IR-BUILD:FKEY op IR-OP:FOPCODE@
   s" hir.mul" IR-SYM:FEQ? ;

: FUNCTION-MUL? ( IR-BUILD:module n -- bool )
   {: m:IR-BUILD:module k:n :}
   m k FUNCTION {: f:IR-ID:ir-fun-id :}
   m IR-BUILD:FFUN-ROWS f IR-FUN:FBLOCK-COUNT 0 ?do
      m f i BLOCK {: blk:IR-ID:ir-block-id :}
      m IR-BUILD:FBLOCK-ROWS blk IR-FUN:FOP-COUNT 0 ?do
         m m blk i OP MUL? if true unloop unloop exit then
      loop
   loop
   false ;

: OBSERVE ( n IR-CTX:ctx IR-BUILD:module -- )
   {: idx:n c:IR-CTX:ctx m:IR-BUILD:module :}
   1 SEEN +!
   REFUSE @ 0<> if E-OBSERVE throw then
   0 PENDING !
   IGNORE @ 0<> if exit then
   m IR-BUILD:FFUN-ROWS IR-FUN:FFUNS {: count:n :}
   count FUN-MAX > if E-OBSERVE throw then
   idx P-IDX !
   c IR-CTX:SERIAL P-CTX !
   count P-N !
   m 0 HELD !
   count 0 ?do m i FUNCTION-MUL? if 1 else 0 then i P-MUL ! loop
   1 PENDING ! ;

: PARENT@ ( n -- NCOMP:code-entry )
   OWNER-XT @ ;

\ This callback has no allocator or failure path. ROW-N is advanced only after
\ the final ordinal, so a partially filled association is never visible.
: PUBLISHED ( n IR-CTX:ctx NCOMP:code-entry n n -- )
   {: idx:n c:IR-CTX:ctx parent:NCOMP:code-entry ord:n off:n :}
   PENDING @ 0= if exit then
   idx P-IDX @ <> c IR-CTX:SERIAL P-CTX @ <> or if
      s" native-observer: publication context mismatch" 76 die
   then
   ord 0= if
      parent OWNER-N @ OWNER-XT !
   then
   ROW-N @ ord + {: row:n :}
   OWNER-N @ row ROW-OWNER !
   ord row ROW-ORD !
   off row ROW-OFF !
   ord P-MUL @ row ROW-MUL !
   1 row ROW-LIVE !
   ord P-N @ 1- = if
      P-N @ ROW-N +!
      1 OWNER-N +!
      0 PENDING !
   then ;

: INVALIDATE ( n -- )
   {: floor:n :}
   1 INVALIDATED +!
   ROW-N @ 0 ?do
      i ROW-OWNER @ PARENT@ NCOMP:ENTRY>N floor >= if
         i ROW-ORD @ 0 > if
            i ROW-OWNER @ PARENT@ NCOMP:ENTRY>N i ROW-OFF @ + LAST-STALE !
         then
         0 i ROW-LIVE !
      then
   loop
   OWNER-N @ 0 ?do
      i PARENT@ NCOMP:ENTRY>N floor >= if i OWNER-XT NCOMP:CLEAR-ENTRY then
   loop
   0 PENDING ! ;

: ADMIT? ( n -- bool )
   {: q:n :}
   ROW-N @ 0 ?do
      i ROW-LIVE @ 0<> i ROW-MUL @ 0= and if
         i ROW-OWNER @ PARENT@ NCOMP:ENTRY>N i ROW-OFF @ + q = if true unloop exit then
      then
   loop
   false ;

TRUSTED: EV ( ptr u8 n -- ) evaluate ;
TRUSTED: EV-N ( ptr u8 n -- n ) evaluate ;

: SOURCE-QUOTES ( -- )
   s" : NOBS-TWO ( -- [ n -- n ] [ n -- n ] ) [: [: 1 + ;] execute ;] [: 3 * ;] ;" EV ;

: EXACT-CASE ( -- )
   SOURCE-QUOTES
   s" observer saw parent and three quotation functions" T-LABEL
   P-N @ 4 T=
   ROW-N @ 4 T=
   s" NOBS-TWO" XREF-FIND XREF-START 0 PARENT@ NCOMP:ENTRY>N T=
   s" NOBS-TWO drop" EV-N ADMIT? TTRUE
   s" NOBS-TWO nip" EV-N ADMIT? TFALSE
   s" 41 NOBS-TWO drop execute" EV-N 42 T=
   s" 14 NOBS-TWO nip execute" EV-N 42 T=
   s" retired frozen HIR reader refuses" T-LABEL
   [: 0 HELD @ IR-BUILD:FFUN-ROWS IR-FUN:FFUNS drop ;] catch 0<> TTRUE ;

: THROW-OBSERVE ( -- )
   s" : NOBS-RETRY ( -- [ n -- n ] ) [: 5 * ;] ;" EV ;

: RETRY-CASE ( -- )
   1 REFUSE !
   [: THROW-OBSERVE ;] E-OBSERVE TTHROWSQ
   0 REFUSE !
   s" observation refusal publishes no rows" T-LABEL
   ROW-N @ 4 T=
   s" : NOBS-RETRY ( -- [ n -- n ] ) [: 2 + ;] ;" EV
   s" NOBS-RETRY" EV-N ADMIT? TTRUE
   s" 40 NOBS-RETRY execute" EV-N 42 T= ;

: LATE ( n n n -- )
   2drop drop
   NSHADOW:OPEN? 0= if s" native-observer: shadow was not open" 76 die then
   E-LATE throw ;

: THROW-LATE ( -- )
   [: LATE ;] [: s" : NOBS-LATE ( -- [ n -- n ] ) [: 7 * ;] ;" EV ;]
   NPUB:WITH-UNIT ;

: LATE-CASE ( -- )
   ROW-N @ {: before:n :}
   X64ABI:BINDING NSHADOW:OPEN
   [: [: THROW-LATE ;] E-LATE TTHROWSQ ;]
   [: NSHADOW:CLOSE ;] finally
   s" a later refusal publishes no association" T-LABEL
   ROW-N @ before T=
   s" : NOBS-LATE ( -- [ n -- n ] ) [: 4 + ;] ;" EV
   s" NOBS-LATE" EV-N ADMIT? TTRUE ;

: FAIL-OUTER ( -- )
   s" NOBS-TEST:NESTED-CHILD 8181 throw" EV ;

: FAIL-GEN-BODY ( -- )
   s" : NOBS-GEN ( -- [ n -- n ] ) [: 11 + ;] ;" EV
   8182 throw ;

: FAIL-GEN ( -- )
   [: FAIL-GEN-BODY ;] GENERATED-DECL:RUN ;

: OUTER-CASE ( -- )
   s" a published child of a failed source is invalidated" T-LABEL
   OWNER-N @ CLEARED-OWNER !
   [: FAIL-OUTER ;] 8181 TTHROWSQ
   INVALIDATED @ 0 > TTRUE
   CLEARED-OWNER @ PARENT@ NCOMP:ENTRY>N 0 T=
   1 IGNORE !
   s" : NOBS-REUSE ( -- [ n -- n ] ) [: 9 + ;] ;" EV
   0 IGNORE !
   s" NOBS-REUSE" EV-N LAST-STALE @ T=
   s" NOBS-REUSE" EV-N ADMIT? TFALSE ;

: GENERATED-CASE ( -- )
   INVALIDATED @ {: before:n :}
   [: FAIL-GEN ;] 8182 TTHROWSQ
   s" generated declaration rollback invalidated its published child" T-LABEL
   INVALIDATED @ before > TTRUE
   1 IGNORE !
   s" : NOBS-GEN-REUSE ( -- [ n -- n ] ) [: 11 + ;] ;" EV
   0 IGNORE !
   s" NOBS-GEN-REUSE" EV-N LAST-STALE @ T=
   s" NOBS-GEN-REUSE" EV-N ADMIT? TFALSE ;

: DOES-CASE ( -- )
   OWNER-N @ {: owner:n :}
   ROW-N @ {: row:n :}
   s" : NOBS-DOES ( n -- ) create , does> ( -- n ) @ ;" EV
   s" completed definer and does clause share one parent publication" T-LABEL
   OWNER-N @ owner 1+ T=
   ROW-N @ row 2 + T=
   s" NOBS-DOES" XREF-FIND XREF-START owner PARENT@ NCOMP:ENTRY>N T=
   row ROW-ORD @ 0 T=
   row 1+ ROW-ORD @ 1 T=
   row 1+ ROW-OFF @ 0 > TTRUE
   s" 7 NOBS-DOES NOBS-MADE NOBS-MADE" EV-N 7 T= ;

: PUBLISH-THROW-SRC$ ( -- ptr u8 n )
   S\" require src/compiler/native/compiler.f 1 set-tier package PUBFAIL public : THROW-PUBLISHED ( n IR-CTX:ctx NCOMP:code-entry n n -- ) {: idx:n c:IR-CTX:ctx parent:NCOMP:code-entry ord:n off:n :} s\q PUBFAIL-DOES\q XREF-FIND XREF-START parent NCOMP:ENTRY>N = if s\q committed-does-visible\q type cr then 8173 throw ; ;package ' PUBFAIL:THROW-PUBLISHED NCOMP:PUBLISHED! : PUBFAIL-DOES ( n -- ) create , does> ( -- n ) @ ;" ;

: PUBLISH-THROW-CASE ( -- )
   s" escaping publication callback is fatal after DOES commit" T-LABEL
   PROC-ARGV-RESET
   ENGINE-CANDIDATE:PATH$ >LEN PUBLISH-THROW-SRC$ >LEN
   PUB-OUT PUB-CAP >LEN PUB-ERR PUB-CAP >LEN 10000 >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME {: outu:len erru:len oc :}
   PUBLISH-THROW-SRC$ PUB-OUT outu LEN>N PUB-ERR erru LEN>N oc 76
   T-OUTCOME-EXITED=
   PUB-OUT outu LEN>N s" committed-does-visible" CONTAINS? TTRUE
   PUB-ERR erru LEN>N s" ncomp: publication callback threw" CONTAINS? TTRUE
   PUB-ERR erru LEN>N s" ncomp: cannot compile" CONTAINS? TFALSE ;

public

: NESTED-CHILD ( -- )
   s" : NOBS-CHILD ( -- [ n -- n ] ) [: 9 + ;] ;" EV ;

: REPL-CHILD ( -- )
   s" : NOBS-REPL-CHILD ( -- [ n -- n ] ) [: 9 + ;] ;" EV ;

: RUN ( -- )
   T-RESET
   DECLARE-ANCHORS
   REFUSALS
   ['] OBSERVE NBACK:OBSERVE!
   ['] PUBLISHED NCOMP:PUBLISHED!
   ['] INVALIDATE CODE-RECLAIM:INVALIDATE!
   EXACT-CASE RETRY-CASE LATE-CASE OUTER-CASE GENERATED-CASE DOES-CASE
   PUBLISH-THROW-CASE
   T-REPORT s" native-observer: ok" type cr ;

: IMAGE-PREPARE ( -- )
   s" native-observer-before: " type 0 PARENT@ NCOMP:ENTRY>N . cr
   0 0 HELD HELD-RAW !
   0 P-IDX ! 0 P-CTX ! 0 LAST-STALE ! ;

: REPL-PREPARE ( -- ) INVALIDATED @ REPL-BEFORE ! ;

: REPL-CHECK ( -- )
   T-RESET
   s" an uncaught REPL throw invalidates code published on its line" T-LABEL
   INVALIDATED @ REPL-BEFORE @ > TTRUE
   1 IGNORE !
   s" : NOBS-REPL-REUSE ( -- [ n -- n ] ) [: 9 + ;] ;" EV
   0 IGNORE !
   s" NOBS-REPL-REUSE" EV-N LAST-STALE @ T=
   s" NOBS-REPL-REUSE" EV-N ADMIT? TFALSE
   T-REPORT s" native-observer-repl: ok" type cr ;

: IMAGE-CHECK ( -- )
   T-RESET
   s" named parent anchor and function offsets survived image relocation" T-LABEL
   s" native-observer-after: " type 0 PARENT@ NCOMP:ENTRY>N . cr
   s" NOBS-TWO" XREF-FIND XREF-START 0 PARENT@ NCOMP:ENTRY>N T=
   CLEARED-OWNER @ PARENT@ NCOMP:ENTRY>N 0 T=
   s" NOBS-TWO drop" EV-N ADMIT? TTRUE
   s" NOBS-TWO nip" EV-N ADMIT? TFALSE
   s" 41 NOBS-TWO drop execute" EV-N 42 T=
   SEEN @ {: before:n :}
   s" : NOBS-IMAGE-LATE ( -- [ n -- n ] ) [: 2 + ;] ;" EV
   SEEN @ before > TTRUE
   s" NOBS-IMAGE-LATE" EV-N ADMIT? TTRUE
   s" NOBS-IMAGE-LATE" XREF-FIND XREF-START
   OWNER-N @ 1- PARENT@ NCOMP:ENTRY>N T=
   T-REPORT s" native-observer-image: ok" type cr ;
;package

NOBS-TEST:RUN

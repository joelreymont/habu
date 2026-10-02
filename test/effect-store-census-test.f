\ effect-store-census-test.f — the effect-store census, on windows whose
\ composition is known before it is asked.
\
\     bin/hb --load test/effect-store-census-test.f
\
\ WHY A CENSUS NEEDS FIXTURES. tools/effect-store-census.f is the acceptance
\ instrument for dot habu-the-effect-store-45bdc561: the claim "the store lost
\ 82% of its bytes" is only as good as the walk that counted them. A walk that
\ missed a node kind, or charged one twice, would publish a smaller number and
\ look like a better result. So the tool is run over three windows built here,
\ where what it must answer is fixed by construction rather than by inspection.
\
\ THE WINDOWS.
\   empty     nothing loaded between MARK and RUN: every counter zero. Catches a
\             walk that reads past the store's end or charges the terminator.
\   repeat    definitions whose signature the store already holds: the window is
\             record headers and NOTHING else, so NODES is zero and SHARES is
\             not. Catches a walk that charges shared nodes to every reader, and
\             it is the composition the interner exists to produce.
\   fresh     a signature the store has never seen, plus a definition the checker
\             rejects: nodes appear, the rejected one leaves nothing behind, and
\             the arithmetic still closes.
\
\ AND THE IDENTITY EVERY WINDOW MUST SATISFY: window-bytes = final + dup, i.e.
\ ORPHAN-BYTES is zero — the walk saw every byte in the window exactly once. It
\ is checked on all three and on the whole store, because a census that does not
\ balance cannot be read at all.

require lib/errors.f
require lib/string.f
require lib/memory.f
require tools/effect-store-census.f
require lib/fmt.f                        \ FMT:.INT - one-line number text

package EFFCENSUS-TEST

variable #FAIL
variable #CASE

: T-FAIL ( -- )
   [char] F emit #CASE @ .
   #FAIL @ 1 + #FAIL ! ;

: T= ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want <> if
      T-FAIL s" assert: expected " type want FMT:.INT s"  got " type got FMT:.INT cr
   then ;

: T<> ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want = if
      T-FAIL s" assert: expected anything but " type want FMT:.INT cr
   then ;

: TTRUE ( bool -- ) if -1 else 0 then -1 T= ;

TRUSTED: CT-EVAL ( ptr u8 n -- ) evaluate ;

variable MK   variable TC

\ BALANCED ( -- ) : the identity that makes every other number readable.
: BALANCED ( -- )
   EFF-CENSUS:ORPHAN-BYTES 0 T=
   EFF-CENSUS:WINDOW-BYTES
   EFF-CENSUS:FINAL-BYTES EFF-CENSUS:DUP-BYTES + T= ;

\ ---------------------------------------------------------------------------
\ window 1: empty
\ ---------------------------------------------------------------------------
EFF-CENSUS:MARK MK !
MK @ EFF-CENSUS:RUN
EFF-CENSUS:WINDOW-BYTES 0 T=
EFF-CENSUS:RECORDS 0 T=
EFF-CENSUS:NODES 0 T=
EFF-CENSUS:SHAPES 0 T=
EFF-CENSUS:NODE-TOTAL-BYTES 0 T=
BALANCED

\ ---------------------------------------------------------------------------
\ window 2: repeats only — headers, no nodes
\ ---------------------------------------------------------------------------
\ seed the shapes OUTSIDE the window, so the window itself can only repeat them
s" : CTSEED ( n n -- n ) drop ;" CT-EVAL

EFF-CENSUS:MARK MK !
s" : CTR1 ( n n -- n ) drop ;" CT-EVAL
s" : CTR2 ( n n -- n ) drop ;" CT-EVAL
s" : CTR3 ( n n -- n ) drop ;" CT-EVAL
MK @ EFF-CENSUS:RUN
EFF-CENSUS:RECORDS 0 T<>                       \ the window is not empty
EFF-CENSUS:NODES 0 T=                          \ ... and holds no node of its own
EFF-CENSUS:NODE-TOTAL-BYTES 0 T=
EFF-CENSUS:SHAPES 0 T<>                        \ it does name rows
EFF-CENSUS:BELOW-WINDOW 0 T<>                  \ ... and every one of them is older
EFF-CENSUS:WINDOW-BYTES EFF-CENSUS:HEADER-BYTES T=
BALANCED

\ ---------------------------------------------------------------------------
\ window 3: a fresh shape, and a rejected definition
\ ---------------------------------------------------------------------------
TRUSTED: CT-BAD-DEF ( -- ) s" : CTBAD ( n -- n ) drop ;" evaluate ;

\ A family nothing else names: its EN-PARAM node carries name bytes the store has
\ never held, so the window is guaranteed to hold nodes of its own. An ordinary
\ scalar row would not be — the boot store already carries every short chain of
\ `n`, which is how this window first came out with zero nodes.
EFF-CENSUS:MARK MK !
s" enum ctfresh alpha beta ;enum" CT-EVAL
s" : CTF1 ( ctfresh -- ) drop ;" CT-EVAL
' CT-BAD-DEF catch TC !
MK @ EFF-CENSUS:RUN
TC @ 0 T<>                                     \ the bad definition really was rejected
EFF-CENSUS:NODES 0 T<>                         \ the fresh shape cost real nodes
EFF-CENSUS:NODE-TOTAL-BYTES 0 T<>
EFF-CENSUS:WINDOW-BYTES
EFF-CENSUS:HEADER-BYTES EFF-CENSUS:CONTENT-TOTAL-BYTES +
EFF-CENSUS:NODE-TOTAL-BYTES + T=
BALANCED

\ ---------------------------------------------------------------------------
\ window 4: a swept window — the bindings of retired symbols name nothing
\ ---------------------------------------------------------------------------
\ The capture's sweep (src/core/checker.f CHECKER-SWEEP) retires what its policy
\ drops and clears the symbol of every binding it leaves in the store. This runs
\ it on a policy that drops two of the window's three words, so the census has a
\ known answer: two bindings keyed on nothing, none on a retired symbol, and the
\ window's bytes unchanged, because nothing was compacted.
: CTS-DROP? ( ptr u8 n bool ptr u8 n -- bool ) {: pa:ptr pu:n pub:bool na:ptr nu:n :}
   na nu s" cts-drop" STARTS-WITH? 0= ;
TRUSTED: CT-SWEEP ( -- ) [: CTS-DROP? ;] CHECKER-SWEEP:RUN ;

EFF-CENSUS:MARK MK !
s" : CTS-KEEP ( n -- n ) 1 + ;" CT-EVAL
s" : CTS-DROP1 ( n -- n ) 2 + ;" CT-EVAL
s" : CTS-DROP2 ( n n -- n ) + ;" CT-EVAL
MK @ EFF-CENSUS:RUN
EFF-CENSUS:WINDOW-BYTES TC !
EFF-CENSUS:UNKEYED 0 T=
CT-SWEEP
MK @ EFF-CENSUS:RUN
EFF-CENSUS:RETIRED-KEYED 0 T=                  \ no binding still names a retired symbol
EFF-CENSUS:UNKEYED 2 T=                        \ ... the dropped words' two name nothing
EFF-CENSUS:WINDOW-BYTES TC @ T=
BALANCED
s" CTS-KEEP" EFFECT-QUERY TTRUE                \ the kept word still resolves
s" CTS-DROP1" EFFECT-QUERY 0= TTRUE            \ a retired one resolves to nothing
s" undefine CTS-DROP1" CT-EVAL                \ ... and its name certifies afresh
s" : CTS-DROP1 ( n -- n ) 3 + ;" CT-EVAL
s" CTS-DROP1" EFFECT-QUERY TTRUE

\ ---------------------------------------------------------------------------
\ the whole store: one node per shape, which is the interner's whole claim
\ ---------------------------------------------------------------------------
0 EFF-CENSUS:RUN
BALANCED
EFF-CENSUS:NODES 0 T<>
EFF-CENSUS:NODES EFF-CENSUS:SHAPES T=
EFF-CENSUS:BELOW-WINDOW 0 T=                   \ nothing is reachable below offset zero

: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" effect-store-census-test: failures" 1 die ;
REPORT

;package

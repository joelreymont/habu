\ effect-store-census-test.f — the effect-store census, on windows whose
\ composition is known before it is asked.
\
\     bin/hb --load test/effect-store-census-test.f
\
\ WHY A CENSUS NEEDS FIXTURES. tools/effect-store-census.f is the acceptance
\ instrument for dot habu-the-effect-store-45bdc561: the claim "the store lost
\ 82% of its bytes" is only as good as the walk that counted them. A walk that
\ missed a node kind, or charged one twice, would publish a smaller number and
\ look like a better result. So the tool is run over four windows built here,
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
\   swept     a capture's sweep that retires six words, beside kept words that
\             share what they reached: nothing moves, the dropped records keep
\             only their chain link, the bytes the walk stops reaching are
\             exactly the ones the sweep zeroed, and the checker and the source
\             pre-verifier answer every kept word as they did.
\
\ AND THE IDENTITY EVERY WINDOW MUST SATISFY: window-bytes = final + dup + dead,
\ i.e. ORPHAN-BYTES is zero — the walk saw every byte in the window exactly once,
\ and every byte it did not see is one a sweep zeroed. It is checked on every
\ window and on the whole store, because a census that does not balance cannot
\ be read at all.

require lib/errors.f
require lib/string.f
require lib/memory.f
require src/habu/verify-source.f
require tools/effect-store-census.f

package EFFCENSUS-TEST

variable #FAIL
variable #CASE

: T-FAIL ( -- )
   [char] F emit #CASE @ .
   #FAIL @ 1 + #FAIL ! ;

: T= ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want <> if
      T-FAIL s" assert: expected " type want . s" got " type got . cr
   then ;

: T<> ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want = if
      T-FAIL s" assert: expected anything but " type want . cr
   then ;

TRUSTED: CT-EVAL ( ptr u8 n -- ) evaluate ;

variable MK   variable TC

\ BALANCED ( -- ) : the identity that makes every other number readable.
: BALANCED ( -- )
   EFF-CENSUS:ORPHAN-BYTES 0 T=
   EFF-CENSUS:WINDOW-BYTES
   EFF-CENSUS:FINAL-BYTES EFF-CENSUS:DUP-BYTES + EFF-CENSUS:DEAD-BYTES + T= ;

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
\ window 4: a swept store — a retired binding keeps only its chain link
\ ---------------------------------------------------------------------------
\ The capture's sweep (src/core/checker.f CHECKER-SWEEP) retires what its policy
\ drops and zeroes, in place, every byte of the effect store that only a retired
\ binding reached; the binding keeps its link to the next record, so nothing
\ moves. The policy here drops the cts-drop words, and two kept words stand
\ where a sweep that zeroed too much would reach them. CTS-KEEPQT repeats the
\ fresh quotation CTS-DROPQT took just before it, so its content and nodes lie
\ in a dropped record's span. CTS-KEEPWD wraps the dropped definer CTS-DROPMK,
\ so the record its control row's CREATES cell names, the effect of a word it
\ creates, is reached only through that kept row. A word created at run time
\ never reads that record - the engine publishes its effect from the definer's
\ clause (src/habu/habu2.f LASTC-TRUST:PUBLISH) - so the window asks the reader
\ that does: the source pre-verifier registers a word a source creates through
\ CTS-KEEPWD from that record (src/core/checker.f CHECKER-RECORD-CREATED), and
\ the word must type as ( -- n ), and not as ( -- n n ), on both sides of the
\ sweep. Only dropped words follow the last MARK: CTS-DROP1 and
\ CTS-DROP2 repeat shapes the store already holds, CTS-DROP3 takes a family
\ twice, a row no other word has, so its record owns a content and a node, and
\ the effect CTS-DROPMK2 gives the words it creates is that window's one
\ record keyed on nothing. The census reads the whole store on both sides: the
\ same bytes and the same records, and the six dropped bindings keyed on
\ nothing. Swept, the last window is chain links alone, the bytes it stopped
\ reaching are every byte the sweep zeroed - an earlier sweep may have zeroed
\ others before it - and the checker and the pre-verifier answer each kept word
\ as they did.
: CTS-POLICY? ( ptr u8 n bool ptr u8 n -- bool ) {: pa:ptr pu:n pub:bool na:ptr nu:n :}
   na nu s" cts-drop" STARTS-WITH? 0= ;
TRUSTED: CT-SWEEP ( -- ) [: CTS-POLICY? ;] CHECKER-SWEEP:RUN ;

70 constant REJECT-RC   \ a refused definition (src/habu/habu2.f RC-REJECT)

\ CREATED-USE$ / CREATED-MISUSE$ ( -- ptr u8 n ) : a source that creates
\ CTS-MADE3 through CTS-KEEPWD and calls it, under an effect the record
\ CTS-KEEPWD's CREATES cell names must admit and one it must refuse.
: CREATED-USE$ ( -- ptr u8 n )
   s" 9 CTS-KEEPWD CTS-MADE3 : CTS-USE3 ( -- n ) CTS-MADE3 ;" ;
: CREATED-MISUSE$ ( -- ptr u8 n )
   s" 9 CTS-KEEPWD CTS-MADE3 : CTS-USE3 ( -- n n ) CTS-MADE3 ;" ;

\ VERDICTS ( -- ) : what the checker answers for code that calls each kept
\ word, one effect that must certify and one that must not, and what the source
\ pre-verifier answers for a word a source creates through CTS-KEEPWD.
: VERDICTS ( -- )
   s" CTV ( n -- n ) CTS-KEEP" CHECK-QUIET-CANDIDATE! -1 T=
   s" CTV ( n n -- n ) CTS-KEEP" CHECK-QUIET-CANDIDATE! 0 T=
   s" CTV ( [ ptr ctswept -- ] -- ) CTS-KEEPQT" CHECK-QUIET-CANDIDATE! -1 T=
   s" CTV ( [ ctswept -- ] -- ) CTS-KEEPQT" CHECK-QUIET-CANDIDATE! 0 T=
   s" CTV ( n -- ) CTS-KEEPWD" CHECK-QUIET-CANDIDATE! -1 T=
   s" CTV ( -- n ) CTS-MADE1" CHECK-QUIET-CANDIDATE! -1 T=
   s" CTV ( -- n n ) CTS-MADE1" CHECK-QUIET-CANDIDATE! 0 T=
   [: CREATED-USE$ VERIFY:SOURCE-BUF ;] catch 0 T=
   [: CREATED-MISUSE$ VERIFY:SOURCE-BUF ;] catch REJECT-RC T= ;

variable RECS0   variable KEYLESS0   variable WINDOW0   variable DEAD0
variable ZEROED

s" enum ctswept ctsw-on ctsw-off ;enum" CT-EVAL
s" : CTS-KEEP ( n -- n ) 1 + ;" CT-EVAL
EFF-CENSUS:MARK MK !
s" : CTS-DROPQT ( [ ptr ctswept -- ] -- ) drop ;" CT-EVAL
MK @ EFF-CENSUS:RUN
EFF-CENSUS:NODES 0 T<>                         \ the quotation is fresh
EFF-CENSUS:MARK MK !
s" : CTS-KEEPQT ( [ ptr ctswept -- ] -- ) drop ;" CT-EVAL
MK @ EFF-CENSUS:RUN
EFF-CENSUS:RECORDS 1 T=                        \ ... and CTS-KEEPQT's content and nodes
EFF-CENSUS:CONTENTS 0 T=                       \ are the ones in CTS-DROPQT's span
EFF-CENSUS:NODES 0 T=
s" : CTS-DROPMK ( n -- ) create , does> ( -- n ) @ ;" CT-EVAL
s" : CTS-KEEPWD ( n -- ) CTS-DROPMK ;" CT-EVAL
s" 7 CTS-KEEPWD CTS-MADE1" CT-EVAL
EFF-CENSUS:MARK MK !
s" : CTS-DROP1 ( n -- n ) 2 + ;" CT-EVAL
s" : CTS-DROP2 ( n n -- n ) + ;" CT-EVAL
s" : CTS-DROP3 ( ctswept ctswept -- ) 2drop ;" CT-EVAL
s" : CTS-DROPMK2 ( n -- ) create , does> ( -- n ) @ ;" CT-EVAL
MK @ EFF-CENSUS:RUN
EFF-CENSUS:NODES 0 T<>                         \ the dropped words own a node
EFF-CENSUS:UNKEYED 1 T=                        \ CTS-DROPMK2's created effect names nothing
VERDICTS
s" CTV ( n -- n ) CTS-DROP1" CHECK-QUIET-CANDIDATE! -1 T=   \ a dropped word certifies until swept
0 EFF-CENSUS:RUN
BALANCED
EFF-CENSUS:RECORDS RECS0 !
EFF-CENSUS:UNKEYED KEYLESS0 !
EFF-CENSUS:WINDOW-BYTES WINDOW0 !
EFF-CENSUS:DEAD-BYTES DEAD0 !
CT-SWEEP
0 EFF-CENSUS:RUN
BALANCED
EFF-CENSUS:WINDOW-BYTES WINDOW0 @ T=           \ nothing moved
EFF-CENSUS:RECORDS RECS0 @ T=                  \ ... and every record is still on the chain
EFF-CENSUS:UNKEYED KEYLESS0 @ 6 + T=           \ the six dropped bindings name nothing
EFF-CENSUS:RETIRED-KEYED 0 T=                  \ ... and none names a retired symbol
EFF-CENSUS:DEAD-BYTES DEAD0 @ - ZEROED !
MK @ EFF-CENSUS:RUN                            \ the last window is chain links alone:
BALANCED
EFF-CENSUS:CONTENTS 0 T=                       \ no record holds a content
EFF-CENSUS:BELOW-WINDOW 0 T=                   \ ... and no row reaches an older node
EFF-CENSUS:DEAD-BYTES ZEROED @ T=              \ what they reached is all the sweep zeroed
s" effect-store-census swept zeroed-bytes " type ZEROED @ . cr
VERDICTS                                       \ every kept word answers as it did
s" 8 CTS-KEEPWD CTS-MADE2" CT-EVAL             \ ... and the wrapper still creates
s" CTV ( -- n ) CTS-MADE2" CHECK-QUIET-CANDIDATE! -1 T=
s" CTV ( -- n n ) CTS-MADE2" CHECK-QUIET-CANDIDATE! 0 T=
s" CTV ( n -- n ) CTS-DROP1" CHECK-QUIET-CANDIDATE! 1 T=   \ a retired word resolves to nothing
s" undefine CTS-DROP3" CT-EVAL                \ ... and its name certifies afresh, on
s" : CTS-DROP3 ( ctswept ctswept -- ) 2drop ;" CT-EVAL     \ the row the sweep zeroed
s" CTV ( ctswept ctswept -- ) CTS-DROP3" CHECK-QUIET-CANDIDATE! -1 T=

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

\ type-linear-suite.f — whole-bundle linear accounting for layout values
\ (PLAN item 11, docs/type-families.md §19). Run BY THE ENGINE over stdin:
\     bin/hb < test/type-linear-suite.f
\ A sum whose type args include a linear con is ONE linear unit: constructors
\ consume linear payloads and mint the bundle (empty-payload variants mint
\ without input, the option-NONE shape), pass-through and deeper-row flow
\ conserve it, and every copy/drop/transport/local-capture/unconsumed path
\ rejects (v1: construct + flow only; move-class transports and MATCH
\ consumption land with item 9). Width stays truthful: linear-arg layouts
\ expand to hidden fields exactly like non-linear ones (LAYOUT-ARGS-OPEN?),
\ so checker rows and runtime cells agree.
\
\ The closing TC/TK/TM section (package TLIN) covers the OTHER way a bundle
\ becomes one linear unit: not through its type arguments but through a declared
\ field or variant payload that names a family which itself owns a linear value
\ (dot habu-checker-enum-payload-9e1ae6cc). Inside it, TC21-TC30 pin that typed
\ memory stays closed to a linear value carried under a generic layer, each with
\ a width-identical non-linear control, and the TA block records that the atomic
\ memory operations transport no layout at all.

require test/checker-assert.f

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

deflinear ltok
SUMTYPE lq2 2
  VARIANT ok  a ;VARIANT
  VARIANT err b ;VARIANT
;SUMTYPE
SUMTYPE lqmix 2
  VARIANT small a ;VARIANT
  VARIANT big a b n ;VARIANT
;SUMTYPE

\ ---------------------------------------------------------------------------
\ accepted: construction, minting, pass-through, deeper-row flow, wrappers.
\ ---------------------------------------------------------------------------
s" A1=" type s" C1 ( ltok -- lq2<ltok,n> ) LQ2:OK" CHECK-QUIET-CANDIDATE! -1 T=
s" A2=" type s" C2 ( n -- lq2<ltok,n> ) LQ2:ERR" CHECK-QUIET-CANDIDATE! -1 T=
s" A3=" type s" C3 ( lq2<ltok,n> -- lq2<ltok,n> )" CHECK-QUIET-CANDIDATE! -1 T=
\ transports beside the bundle stay legal: the reject targets the bundle only.
s" A4=" type s" C4 ( lq2<ltok,n> n -- lq2<ltok,n> ) drop" CHECK-QUIET-CANDIDATE! -1 T=
\ a generic wrapper flows the linear through construction (1-in/1-out var).
: LWRAP ( a -- lq2<a,b> ) LQ2:OK ;
s" A5=" type s" C5 ( ltok -- lq2<ltok,n> ) LWRAP" CHECK-QUIET-CANDIDATE! -1 T=
\ wider payloads: multi-cell construction with a linear arg mints one unit.
s" A6=" type s" C6 ( ltok n n -- lqmix<ltok,n> ) LQMIX:BIG" CHECK-QUIET-CANDIDATE! -1 T=
\ narrow variant with zero padding still consumes the linear payload exactly once.
s" A7=" type s" C7 ( ltok -- lqmix<ltok,n> ) LQMIX:SMALL" CHECK-QUIET-CANDIDATE! -1 T=
cr

\ ---------------------------------------------------------------------------
\ move-class transports (swap/rot/-rot/2swap and >r/2>r round trips) reorder the
\ bundle without copy or drop, so the linear count is CONSERVED -> accept. The
\ whole M+1-cell group moves atomically (XG-READ-HID), and LIN-CHECK conservation
\ classifies: a permutation keeps before=after, so it certifies.
\ ---------------------------------------------------------------------------
s" M1=" type s" M1 ( lq2<ltok,n> n -- n lq2<ltok,n> ) swap" CHECK-QUIET-CANDIDATE! -1 T=
s" M2=" type s" M2 ( lq2<ltok,n> n n -- n n lq2<ltok,n> ) rot" CHECK-QUIET-CANDIDATE! -1 T=
s" M3=" type s" M3 ( n n lq2<ltok,n> -- lq2<ltok,n> n n ) -rot" CHECK-QUIET-CANDIDATE! -1 T=
s" M4=" type s" M4 ( lq2<ltok,n> n n n -- n n lq2<ltok,n> n ) 2swap" CHECK-QUIET-CANDIDATE! -1 T=
s" M5=" type s" M5 ( lq2<ltok,n> -- lq2<ltok,n> ) >r r>" CHECK-QUIET-CANDIDATE! -1 T=
s" M6=" type s" M6 ( lq2<ltok,n> n -- lq2<ltok,n> n ) 2>r 2r>" CHECK-QUIET-CANDIDATE! -1 T=
\ two linear bundles swapped: both conserved -> accept.
s" M7=" type s" M7 ( lq2<ltok,n> lq2<ltok,n> -- lq2<ltok,n> lq2<ltok,n> ) swap" CHECK-QUIET-CANDIDATE! -1 T=
\ a move that then loses the bundle at the boundary, or strands it on the return
\ stack, still rejects: conservation is per-step, boundary balance catches the loss.
s" MR1=" type s" MR1 ( lq2<ltok,n> n -- n ) swap" CHECK-QUIET-CANDIDATE! 0 T=
s" MR2=" type s" MR2 ( lq2<ltok,n> -- ) >r" CHECK-QUIET-CANDIDATE! 0 T=
cr

\ ---------------------------------------------------------------------------
\ rejected: copy, drop, locals, return-stack copy, unconsumed, loss. Copy-class
\ (dup/over/tuck/2dup) raises the count, drop-class (drop/nip/2drop) lowers it,
\ and r@/2r@ re-push (copy) the group -> LIN-CHECK conservation fires.
\ ---------------------------------------------------------------------------
s" R1=" type s" B1 ( lq2<ltok,n> -- lq2<ltok,n> lq2<ltok,n> ) dup" CHECK-QUIET-CANDIDATE! 0 T=
s" R2=" type s" B2 ( lq2<ltok,n> -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=
s" R3=" type s" B3 ( lq2<ltok,n> -- )" CHECK-QUIET-CANDIDATE! 0 T=
s" R4=" type s" B4 ( lq2<ltok,n> n -- lq2<ltok,n> n lq2<ltok,n> ) over" CHECK-QUIET-CANDIDATE! 0 T=
s" R5=" type s" B5 ( lq2<ltok,n> -- lq2<ltok,n> ) {: x :} x" CHECK-QUIET-CANDIDATE! 0 T=
s" R6=" type s" B6 ( n lq2<ltok,n> -- lq2<ltok,n> n lq2<ltok,n> ) tuck" CHECK-QUIET-CANDIDATE! 0 T=
s" R7=" type s" B7 ( lq2<ltok,n> -- lq2<ltok,n> lq2<ltok,n> ) >r r@ r>" CHECK-QUIET-CANDIDATE! 0 T=
s" R8=" type s" B8 ( ltok -- lq2<ltok,n> ) LQ2:OK LQ2:OK" CHECK-QUIET-CANDIDATE! 0 T=   \ payload reuse
s" R9=" type s" B9 ( ltok f -- lq2<ltok,n> ) if LQ2:OK then" CHECK-QUIET-CANDIDATE! 0 T=  \ branch loss
s" R10=" type s" B10 ( ltok n -- lq2<ltok,n> ) nip 0 0" CHECK-QUIET-CANDIDATE! 0 T=       \ raw forge
s" R11=" type s" B11 ( ltok n -- lq2<ltok,n> ) nip LQ2:ERR" CHECK-QUIET-CANDIDATE! 0 T=   \ payload dropped
s" R12=" type s" B12 ( lq2<ltok,n> n -- n ) nip" CHECK-QUIET-CANDIDATE! 0 T=              \ bundle dropped by nip
s" R13=" type s" B13 ( lq2<ltok,n> n -- lq2<ltok,n> n lq2<ltok,n> n ) 2dup" CHECK-QUIET-CANDIDATE! 0 T=  \ 2dup copies bundle
cr

\ ---------------------------------------------------------------------------
\ scalar linear discipline unchanged next to the bundle machinery.
\ ---------------------------------------------------------------------------
s" S1=" type s" D1 ( ltok -- ltok )" CHECK-QUIET-CANDIDATE! -1 T=
s" S2=" type s" D2 ( ltok -- ltok ltok ) dup" CHECK-QUIET-CANDIDATE! 0 T=
s" S3=" type s" D3 ( ltok -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=
cr

\ A cell-family argument is phantom even inside a layout.
NEWTYPE lcell 1
s" LCOPY" s" lq2<lcell<a>,n> -- lq2<lcell<a>,n> lq2<lcell<a>,n>" TRUST
s" P2=" type s" P2 ( lq2<lcell<ltok>,n> -- lq2<lcell<ltok>,n> lq2<lcell<ltok>,n> ) LCOPY" CHECK-QUIET-CANDIDATE! -1 T=
cr

\ ---------------------------------------------------------------------------
\ item 9 slice 2: the `construct` form conserves exactly like the generated
\ constructor words — same CHECKER-STEP accounting, no separate linear rules.
\ Consume (A1 parity), mint (A2), padded/multi-cell (A6/A7), and the reuse/
\ loss/transport rejects hold through the inline form.
\ ---------------------------------------------------------------------------
s" K1=" type s" KC1 ( ltok -- lq2<ltok,n> ) construct lq2 ok" CHECK-QUIET-CANDIDATE! -1 T=
s" K2=" type s" KC2 ( n -- lq2<ltok,n> ) construct lq2 err" CHECK-QUIET-CANDIDATE! -1 T=
s" K3=" type s" KC3 ( ltok n n -- lqmix<ltok,n> ) construct lqmix big" CHECK-QUIET-CANDIDATE! -1 T=
s" K4=" type s" KC4 ( ltok -- lqmix<ltok,n> ) construct lqmix small" CHECK-QUIET-CANDIDATE! -1 T=
cr
s" KR1=" type s" KB1 ( ltok n -- lq2<ltok,n> ) construct lq2 err" CHECK-QUIET-CANDIDATE! 0 T=   \ unconsumed linear payload
s" KR2=" type s" KB2 ( ltok -- lq2<ltok,n> lq2<ltok,n> ) construct lq2 ok dup" CHECK-QUIET-CANDIDATE! 0 T=
s" KR3=" type s" KB3 ( ltok n -- lq2<ltok,n> ) nip construct lq2 err" CHECK-QUIET-CANDIDATE! 0 T=   \ payload dropped before construction
cr

\ ---------------------------------------------------------------------------
\ layout-cap slice 5 (dot habu-checker-capability-layout-9b8540bd): linearity is
\ TRANSITIVE through a NESTED layout arg. A linear con buried inside a nested
\ instantiation (the ltok inside lopt<lq2<ltok,n>>) makes the OUTER bundle one
\ linear unit — LAYOUT-ARG-LIN-N / LAYOUT-ARG-LINEARISH? recurse through the arg
\ tree, so LIN-TYPE-COUNT samples the one unit at the outer tag. Before slice 5
\ the outer bundle read as non-linear and could be dup/dropped, laundering the
\ resource (a soundness hole). Sound rule: a linear moving INTO the constructed
\ payload consumes it once (NL1), MATCH re-introduces it once and the arm must
\ consume it — re-mint (NL6) or free — and any copy (NL2), loss (NL3/NL4), or
\ match-then-drop (NL7) rejects. Identity/transport still flows (NL5).
\ ---------------------------------------------------------------------------
SUMTYPE lopt 1 VARIANT none ;VARIANT VARIANT some a ;VARIANT ;SUMTYPE
s" NL1=" type s" NC1 ( lq2<ltok,n> -- lopt<lq2<ltok,n>> ) LOPT:SOME" CHECK-QUIET-CANDIDATE! -1 T=
s" NL2=" type s" NC2 ( lq2<ltok,n> -- lopt<lq2<ltok,n>> lopt<lq2<ltok,n>> ) LOPT:SOME dup" CHECK-QUIET-CANDIDATE! 0 T=
s" NL3=" type s" NC3 ( lopt<lq2<ltok,n>> -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=
s" NL4=" type s" NC4 ( lq2<ltok,n> -- lopt<lq2<ltok,n>> ) LOPT:NONE" CHECK-QUIET-CANDIDATE! 0 T=   \ NONE drops the linear payload
s" NL5=" type s" NC5 ( lopt<lq2<ltok,n>> -- lopt<lq2<ltok,n>> )" CHECK-QUIET-CANDIDATE! -1 T=
s" NL6=" type s" NC6 ( lopt<lq2<ltok,n>> -- lopt<lq2<ltok,n>> ) MATCH lopt none OF LOPT:NONE ENDOF some OF LOPT:SOME ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
s" NL7=" type s" NC7 ( lopt<lq2<ltok,n>> -- n ) MATCH lopt none OF 0 ENDOF some OF drop 1 ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
cr

\ ---------------------------------------------------------------------------
\ LINEARITY BY CONTAINMENT through a declared field or payload (dot
\ habu-checker-enum-payload-9e1ae6cc).
\
\ The sections above make a bundle linear through its TYPE ARGUMENTS. This one
\ makes it linear through its DECLARED MEMBERS: a structure field or a variant
\ payload that names a family which itself owns a linear value. Until this dot
\ both unified declarers rejected that spelling outright, so `FIELD res
\ WSTORE:resident` was legal but `FIELD m gpt2-model` — the same resource one
\ structure deeper — rejected as an "unknown" type. Refusing it never protected
\ anything, because the obligation is carried by the value either way; it only
\ made the resource unnameable one level out, which is what blocked the GPT-2
\ checkpoint load from answering with a loaded model.
\
\ What decides linearity is TFAM-CONCRETE-LINEAR?, which walks a family's field
\ and payload schemas and follows an application node into the family it names.
\ It already recursed, so accepting the spelling was enough: TCn below prove the
\ containment reaches an arbitrary depth and through a sum, TKn that the sealed
\ MAKE/UNMAKE pair conserves the resource, and TMn that MATCH hands the payload
\ to the arm as a linear value the arm must discharge exactly once.
\
\ FREE-MODEL / MINT-TOK are the abstract consumer and producer (test-fixture
\ boundary, the FREE-MTOK pattern of test/type-match-suite.f) so an arm can fully
\ discharge or fully re-supply the resource without a runtime.
\
\ The whole section lives in package TLIN, which is also how production writes
\ these types (maki/infer/weight-store.f owns `WSTORE:resident` the same way).
\ The candidates are checked from inside the package because `construct` resolves
\ its family in the ACTIVE package, so a fixture that constructs has to be
\ written where a real caller of these types would be written.
\ ---------------------------------------------------------------------------
package TLIN
public

DEFLINEAR TLIN:tok                                             \ the linear owner everything below nests
STRUCTURE box 0 FIELD t TLIN:tok FIELD k n ;STRUCTURE          \ depth 1: names the con
STRUCTURE model 0 FIELD inner box FIELD z n ;STRUCTURE         \ depth 2: names that family
STRUCTURE deep 0 FIELD d model ;STRUCTURE                      \ depth 3: and again
STRUCTURE lone 0 FIELD t TLIN:tok ;STRUCTURE                   \ one cell, linear
STRUCTURE plain 0 FIELD v n ;STRUCTURE                         \ one cell, its non-linear twin
STRUCTURE plainer 0 FIELD inner plain ;STRUCTURE               \ control: nothing linear anywhere
ENUM load-result 0
  VARIANT loaded   FIELD model model ;VARIANT
  VARIANT rejected FIELD code n ;VARIANT
;ENUM
STRUCTURE via-load 0 FIELD result load-result ;STRUCTURE       \ a structure reaching a linear ENUM
s" TLIN:FREE-MODEL" s" TLIN:model --" TRUST
s" TLIN:MINT-TOK" s" -- TLIN:tok" TRUST

\ the value itself: identity and permutation conserve it, every copy or loss does not.
s" TC1=" type s" TC1 ( model -- model )" CHECK-QUIET-CANDIDATE! -1 T=
s" TC2=" type s" TC2 ( model -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=
s" TC3=" type s" TC3 ( model -- model model ) dup" CHECK-QUIET-CANDIDATE! 0 T=
s" TC4=" type s" TC4 ( model model -- model ) nip" CHECK-QUIET-CANDIDATE! 0 T=
s" TC5=" type s" TC5 ( model n -- n model ) swap" CHECK-QUIET-CANDIDATE! -1 T=
s" TC6=" type s" TC6 ( model -- ) >r" CHECK-QUIET-CANDIDATE! 0 T=
s" TC7=" type s" TC7 ( model -- model model ) >r r@ r>" CHECK-QUIET-CANDIDATE! 0 T=
s" TC8=" type s" TC8 ( model -- ) FREE-MODEL" CHECK-QUIET-CANDIDATE! -1 T=
\ one more level of nesting is still one linear unit, not zero.
s" TC9=" type s" TC9 ( deep -- deep )" CHECK-QUIET-CANDIDATE! -1 T=
s" TC10=" type s" TC10 ( deep -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=
s" TC11=" type s" TC11 ( deep -- deep deep ) dup" CHECK-QUIET-CANDIDATE! 0 T=
\ a structure reaching the resource through an ENUM counts the same way.
s" TC12=" type s" TC12 ( via-load -- via-load )" CHECK-QUIET-CANDIDATE! -1 T=
s" TC13=" type s" TC13 ( via-load -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=
s" TC14=" type s" TC14 ( via-load -- via-load via-load ) dup" CHECK-QUIET-CANDIDATE! 0 T=
\ the control: an identically nested chain with no linear member copies and drops
\ freely, so the rejects above answer the chain and not the nesting.
s" TC15=" type s" TC15 ( plainer -- plainer plainer ) dup" CHECK-QUIET-CANDIDATE! -1 T=
s" TC16=" type s" TC16 ( plainer -- ) drop" CHECK-QUIET-CANDIDATE! -1 T=
\ a local capture of the model hides its linear use, so it stays rejected.
s" TC17=" type s" TC17 ( model -- model ) {: v:model :} v" CHECK-QUIET-CANDIDATE! 0 T=
\ typed memory stays closed to the resource, so no address round trip can launder
\ it. `lone` and `plain` are both one cell and differ only in owning a linear
\ value, so the load that certifies for one and rejects the other is answering
\ linearity and not width. The address is not the resource and still copies.
s" TC18=" type s" TC18 ( ptr lone -- lone ) @" CHECK-QUIET-CANDIDATE! 0 T=
s" TC19=" type s" TC19 ( ptr plain -- plain ) @" CHECK-QUIET-CANDIDATE! -1 T=
s" TC20=" type s" TC20 ( ptr lone -- ptr lone ptr lone ) dup" CHECK-QUIET-CANDIDATE! -1 T=
cr

\ ---------------------------------------------------------------------------
\ typed memory has to stay closed to a linear value buried under a GENERIC layer
\ as well, and that is a DIFFERENT question in the checker from the one TC18 asks.
\ TC18 rejects `ptr lone` because the pointee family is itself linear; the checker
\ answers that from the family. `opt<lone>` is linear for another reason — its
\ type ARGUMENT is — and the checker answers that by walking the argument list and
\ asking each argument whether it reaches a linear value. Nothing above exercised
\ that walk through memory, so deleting its containment answer left every case in
\ this file green while a whole linear bundle could cross typed memory through
\ `( ptr opt<lone> -- opt<lone> ) @`.
\
\ `opt<lone>` and `opt<plain>` are the same width — one tag cell and one payload
\ cell — and differ only in owning a linear value, so a load that rejects one
\ and certifies for the other is answering linearity and not width. Repeating the
\ pair at two generic layers pins the recursion instead of one fixed depth, and
\ the store direction is pinned beside the load because `!` reaches the same guard
\ from the other side.
\ ---------------------------------------------------------------------------
ENUM opt 1
  VARIANT none ;VARIANT
  VARIANT some FIELD v a ;VARIANT
;ENUM
s" TC21=" type s" TC21 ( ptr opt<lone> -- opt<lone> ) @" CHECK-QUIET-CANDIDATE! 0 T=
s" TC22=" type s" TC22 ( ptr opt<plain> -- opt<plain> ) @" CHECK-QUIET-CANDIDATE! -1 T=
s" TC23=" type s" TC23 ( opt<lone> ptr opt<lone> -- ) !" CHECK-QUIET-CANDIDATE! 0 T=
s" TC24=" type s" TC24 ( opt<plain> ptr opt<plain> -- ) !" CHECK-QUIET-CANDIDATE! -1 T=
\ two layers: the walk recurses rather than looking one argument deep.
s" TC25=" type s" TC25 ( ptr opt<opt<lone>> -- opt<opt<lone>> ) @" CHECK-QUIET-CANDIDATE! 0 T=
s" TC26=" type s" TC26 ( ptr opt<opt<plain>> -- opt<opt<plain>> ) @" CHECK-QUIET-CANDIDATE! -1 T=
s" TC27=" type s" TC27 ( opt<opt<lone>> ptr opt<opt<lone>> -- ) !" CHECK-QUIET-CANDIDATE! 0 T=
s" TC28=" type s" TC28 ( opt<opt<plain>> ptr opt<opt<plain>> -- ) !" CHECK-QUIET-CANDIDATE! -1 T=
\ the store direction for the bare family too, so the pair above is not the only
\ evidence that `!` consults the same guard `@` does.
s" TC29=" type s" TC29 ( lone ptr lone -- ) !" CHECK-QUIET-CANDIDATE! 0 T=
s" TC30=" type s" TC30 ( plain ptr plain -- ) !" CHECK-QUIET-CANDIDATE! -1 T=
cr

\ ---------------------------------------------------------------------------
\ The ATOMIC memory operations: a CLOSED-SURFACE TRIPWIRE, not a linearity proof.
\
\ atomic@ and atomic! carry the same polymorphic pointer effect as @ and !, and
\ atomic-cas and atomic-add take a pointer too, so all four look like more doors a
\ linear bundle could cross. They are not doors at all today. The layout-aware
\ transport step is selected by TOKEN NAME and only `@` and `!` have that name, so
\ a layout value of ANY kind cannot be captured here and every case below rejects — the
\ linear one and its width-identical non-linear twin alike.
\
\ That is exactly why these are labelled a tripwire and not evidence. A negative
\ whose control also rejects says nothing about linearity; what these cases pin is
\ that the surface is CLOSED. The `ptr n` cases at the end certify, which is what
\ keeps the block honest: the four words exist and work, so the rejections above
\ are about layouts and not about an unknown word.
\
\ If layouts are ever opened to the atomic operations, the non-linear controls go
\ red first, and whoever opens them has to add real linearity pins in the same
\ change rather than inherit a silent hole.
\ ---------------------------------------------------------------------------
s" TA1=" type s" TA1 ( ptr opt<lone> -- opt<lone> ) atomic@" CHECK-QUIET-CANDIDATE! 0 T=
s" TA2=" type s" TA2 ( ptr opt<plain> -- opt<plain> ) atomic@" CHECK-QUIET-CANDIDATE! 0 T=   \ control also rejected: surface closed
s" TA3=" type s" TA3 ( opt<lone> ptr opt<lone> -- ) atomic!" CHECK-QUIET-CANDIDATE! 0 T=
s" TA4=" type s" TA4 ( opt<plain> ptr opt<plain> -- ) atomic!" CHECK-QUIET-CANDIDATE! 0 T=
s" TA5=" type s" TA5 ( ptr opt<opt<lone>> -- opt<opt<lone>> ) atomic@" CHECK-QUIET-CANDIDATE! 0 T=
s" TA6=" type s" TA6 ( ptr opt<opt<plain>> -- opt<opt<plain>> ) atomic@" CHECK-QUIET-CANDIDATE! 0 T=
s" TA7=" type s" TA7 ( opt<lone> opt<lone> ptr opt<lone> -- opt<lone> ) atomic-cas" CHECK-QUIET-CANDIDATE! 0 T=
s" TA8=" type s" TA8 ( opt<plain> opt<plain> ptr opt<plain> -- opt<plain> ) atomic-cas" CHECK-QUIET-CANDIDATE! 0 T=
s" TA9=" type s" TA9 ( opt<lone> ptr opt<lone> -- opt<lone> ) atomic-add" CHECK-QUIET-CANDIDATE! 0 T=
s" TA10=" type s" TA10 ( opt<plain> ptr opt<plain> -- opt<plain> ) atomic-add" CHECK-QUIET-CANDIDATE! 0 T=
\ the bare families close the same way.
s" TA11=" type s" TA11 ( ptr lone -- lone ) atomic@" CHECK-QUIET-CANDIDATE! 0 T=
s" TA12=" type s" TA12 ( ptr plain -- plain ) atomic@" CHECK-QUIET-CANDIDATE! 0 T=
\ and the four words themselves certify over a plain cell, so nothing above is
\ passing merely because an atomic word went missing.
s" TA13=" type s" TA13 ( ptr n -- n ) atomic@" CHECK-QUIET-CANDIDATE! -1 T=
s" TA14=" type s" TA14 ( n ptr n -- ) atomic!" CHECK-QUIET-CANDIDATE! -1 T=
s" TA15=" type s" TA15 ( n ptr n -- n ) atomic-add" CHECK-QUIET-CANDIDATE! -1 T=
s" TA16=" type s" TA16 ( n n ptr n -- n ) atomic-cas" CHECK-QUIET-CANDIDATE! -1 T=
cr

\ A second lock now stands in front of the shape above: a record can no longer
\ even NAME a pointer to a linear value, because both unified declarers reject
\ `FIELD p ptr <linear>` outright. That door is pinned where it lives, in
\ test/structure-decl-suite.f and test/enum-decl-suite.f; this file stays about
\ what the checker does with such a value once it is on a row.

\ the sealed MAKE/UNMAKE pair moves the resource without copying or losing it.
s" TK1=" type s" TK1 ( box n -- model ) TLIN-MODEL:MAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" TK2=" type s" TK2 ( model -- box n ) TLIN-MODEL:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" TK3=" type s" TK3 ( box n -- box n ) TLIN-MODEL:MAKE TLIN-MODEL:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" TK4=" type s" TK4 ( model -- n ) TLIN-MODEL:UNMAKE nip" CHECK-QUIET-CANDIDATE! 0 T=
s" TK5=" type s" TK5 ( box n -- model model ) TLIN-MODEL:MAKE dup" CHECK-QUIET-CANDIDATE! 0 T=
s" TK6=" type s" TK6 ( box n -- ) TLIN-MODEL:MAKE drop" CHECK-QUIET-CANDIDATE! 0 T=
cr

\ construction consumes the model exactly once and creates one load-result.
s" TM1=" type s" TM1 ( model -- load-result ) construct load-result loaded" CHECK-QUIET-CANDIDATE! -1 T=
s" TM2=" type s" TM2 ( n -- load-result ) construct load-result rejected" CHECK-QUIET-CANDIDATE! -1 T=
s" TM3=" type s" TM3 ( model -- load-result model ) dup construct load-result loaded swap" CHECK-QUIET-CANDIDATE! 0 T=
s" TM4=" type s" TM4 ( model n -- load-result ) nip construct load-result rejected" CHECK-QUIET-CANDIDATE! 0 T=
\ load-result is one linear value.
s" TM5=" type s" TM5 ( load-result -- load-result )" CHECK-QUIET-CANDIDATE! -1 T=
s" TM6=" type s" TM6 ( load-result -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=
s" TM7=" type s" TM7 ( load-result -- load-result load-result ) dup" CHECK-QUIET-CANDIDATE! 0 T=
\ MATCH hands the payload to the branch as a linear value: discharging it once
\ certifies, and copying, dropping, stranding or leaving it does not.
s" TM8=" type s" TM8 ( load-result -- n ) MATCH load-result loaded OF FREE-MODEL 0 ENDOF rejected OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
s" TM9=" type s" TM9 ( load-result -- n ) MATCH load-result loaded OF drop 0 ENDOF rejected OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
s" TM10=" type s" TM10 ( load-result -- n ) MATCH load-result loaded OF dup FREE-MODEL FREE-MODEL 0 ENDOF rejected OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
s" TM11=" type s" TM11 ( load-result -- n ) MATCH load-result loaded OF 0 ENDOF rejected OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
s" TM12=" type s" TM12 ( load-result -- n ) MATCH load-result loaded OF >r 0 ENDOF rejected OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
s" TM13=" type s" TM13 ( load-result -- n ) MATCH load-result loaded OF exit ENDOF rejected OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
\ reconstructing load-result is a legitimate discharge, and so is moving the model
\ out through the join — both consume the branch's value exactly once.
s" TM14=" type s" TM14 ( load-result -- load-result ) MATCH load-result loaded OF construct load-result loaded ENDOF rejected OF construct load-result rejected ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
s" TM15=" type s" TM15 ( load-result -- model ) MATCH load-result loaded OF ENDOF rejected OF drop MINT-TOK 0 TLIN-BOX:MAKE 0 TLIN-MODEL:MAKE ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! -1 T=
\ unpacking the payload inside the branch keeps the obligation on the parts.
s" TM16=" type s" TM16 ( load-result -- n ) MATCH load-result loaded OF TLIN-MODEL:UNMAKE drop TLIN-BOX:UNMAKE drop drop 0 ENDOF rejected OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T=
cr

;package

\ ---------------------------------------------------------------------------
\ report: "ok" on success, nonzero exit on any failure.
\ ---------------------------------------------------------------------------
: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" type-linear-suite: failures" 1 die ;
REPORT

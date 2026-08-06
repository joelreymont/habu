\ mint-neg-test.f - committed negatives for the checked-mint output-provenance
\ seal (src/core/checker.f NP-MINT-CHECK) and for the output-family pinning of
\ the MINTING combinators (lib/ptx/rep.f: PTXREP:MINT-LOAD / MINT-ROW-SPAN /
\ MINT-ROW-LOAD). Every case drives the real checker through PTXN-CHECK.
\
\ WHERE THE SEAL BITES. NP-CHECK walks the whole declared quantifier inventory
\ first, rejecting a quantifier that got specialized to a concrete type or
\ aliased onto another quantifier, and NP-MINT-CHECK never runs once either has
\ fired. A candidate reaches the mint clause only when every declared quantifier
\ still resolves to its own distinct variable, so the variable the seal names has
\ to be one the BODY minted fresh - never one the declaration aliased.
\
\ THE MINT-* COMBINATORS CANNOT REACH IT, and that is layer 1 doing its job:
\ every argument of their declared output is PROJECTED from an operand
\ (tile<t,b,m> from span<s,t,e> + gridctx<b,e,m>; span<s,t,k> from
\ matrix<s,t,e,k>). A wrapper's output argument therefore resolves either to
\ another declared quantifier - aliased - or to the concrete type the operand
\ pinned - specialized. Sourcing an operand from GRID-CTX / ROW-CTX does hand the
\ block position a genuinely fresh variable, but those rows' `fresh-mask-live`
\ argument is GENERATIVE - a distinct nominal per occurrence, so no declaration
\ can name it - and it specializes a declared quantifier first. Measured here:
\   ( span<space-global,t,e> gridctx<b,e,m> -- tile<u,b,m> )   -> 'u'/'t' aliased
\   ( span<space-global,f32,e> gridctx<b,e,m> -- tile<u,b,m> ) -> 'u' specialized
\   ( span<space-global,t,e> -- tile<t,b,m> ) dup GRID-CTX ... -> 'm' specialized
\ The retired M1..M4 asserted exactly those shapes, so they rejected without ever
\ reaching the seal and pinned nothing about provenance. MZX keeps that fact as
\ an executable record rather than a comment: if a MINT-* output ever stops being
\ fully projected, MZX flips and this file has to be revisited.
\
\ WHAT THE SEAL DOES REACH is a checked `:` wrapper over a TRUSTED: row whose own
\ declared output introduces an input-unbound single-cell type variable. Those
\ rows are the ones the PTX stdlib still has to trust, and re-declaring one as
\ checked code is the real temptation the seal exists to refuse:
\
\   MZ1 ACC-ZERO  - free variable at argument 0 while its siblings ARE input
\                   bound: binding part of an output family launders nothing.
\   MZ2 BROADCAST - free variable PAST a concrete argument: every argument index
\                   is scanned, and a concrete argument is skipped, not a stop.
\   MZ3 ROW       - an EMPTY input row: the seal needs no input to compare with.
\
\ Each is paired with a control that runs the SAME body word and declares the
\ SAME output term, differing only by a witness operand that binds the minted
\ variable - so the reject is about provenance and about nothing else. MZA is the
\ anti-fool case: it certifies with an EMPTY diagnostic while its own source
\ carries the needle text, which is what makes a matched needle the checker's
\ word instead of the candidate's.
\
\ Scratch checker builds (mutations of src/core/checker.f, never committed) show
\ the three carry independent content, so this is not one result under three
\ names: neutering NP-MINT-CHECK certifies all three; exempting an output family
\ that projects at least one operand certifies MZ1 alone; scanning only argument
\ 0 certifies MZ2 alone; skipping an empty input row certifies MZ3 alone. Under
\ every one of those the retired M1..M4 stayed rejected, by the aliasing clause.
\ Each needle also names the variable the seal chose, so a build that scanned
\ only the LAST argument still rejected MZ2 - and MZ2 still failed, because the
\ diagnostic then named 'm' where the fixture requires 'b'. First-wins argument
\ order is part of what these cases pin.
\
\ MF1 / MF2 are layer 1 itself: MINT-LOAD's declared `tile` output and
\ MINT-ROW-SPAN's declared `span` output reject an acc / matrix relabel by
\ unification, so a wrapper cannot re-tag the minted family. Their needles are
\ the rendered expected/actual pair, not the combinator's name, so an undefined
\ word or an unrelated failure at the same call site cannot satisfy them.

require lib/ptx/neg-test-lib.f

package MINT-NEG
private

\ CHECK-CANDIDATE! answers -1 certified / 0 rejected / 1 uncheckable. lib/ptx
\ shares PTXN-REJECTS but has no resolving-control assertion yet; this file owns
\ one until neg-test-lib.f itself moves into a package and can publish it.
-1 constant CERT

\ The resolving control for a PTXN-REJECTS case: the same premise minus the one
\ property under test. The verdict is asserted EXACTLY certified, because an
\ unresolvable or mistyped control answers 1, and 1 would otherwise read as
\ "not rejected". The diagnostic must be EMPTY as well - that is what proves a
\ needle matched in the paired negative is the CHECKER's text and not the
\ candidate's own source echoed back.
: ACCEPTS ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n label:ptr labelu:n :}
   src srcu PTXN-CHECK {: diag:n verdict:n rc:n :}
   label labelu T-LABEL
   rc 0 T=
   label labelu T-LABEL
   verdict CERT T=
   label labelu T-LABEL
   diag 0 T= ;

: MAIN ( -- )
   T-RESET
   256 %BLOCK

   \ --- the mint-provenance seal ------------------------------------------
   s" MZ1 ( gridctx<b,e,m> -- acc<t,b,m> ) ACC-ZERO"
   s" 't' is minted into family 'acc' but is unbound in the inputs"
   s" ACC-ZERO as checked code: free argument 0 beside bound siblings" PTXN-REJECTS
   s" MZ1C ( gridctx<b,e,m> acc<t,b,m> -- acc<t,b,m> ) drop ACC-ZERO"
   s" control: the same ACC-ZERO mint with 't' bound by a witness operand" ACCEPTS
   s" NEG: ACC-ZERO cannot become checked code; binding b and m does not free t" type cr

   s" MZ2 ( uniform<f32> -- tile<f32,b,m> ) BROADCAST"
   s" 'b' is minted into family 'tile' but is unbound in the inputs"
   s" BROADCAST as checked code: free argument past a concrete one" PTXN-REJECTS
   s" MZ2C ( uniform<f32> tile<f32,b,m> -- tile<f32,b,m> ) drop BROADCAST"
   s" control: the same BROADCAST mint with 'b' bound by a witness operand" ACCEPTS
   s" NEG: BROADCAST cannot become checked code; a concrete f32 does not end the scan" type cr

   s" MZ3 ( -- rowidx<e> ) ROW"
   s" 'e' is minted into family 'rowidx' but is unbound in the inputs"
   s" ROW as checked code: an empty input row binds nothing" PTXN-REJECTS
   s" MZ3C ( rowidx<e> -- rowidx<e> ) drop ROW"
   s" control: the same ROW mint with 'e' bound by a witness operand" ACCEPTS
   s" NEG: ROW cannot become checked code; with no inputs nothing can bind e" type cr

   \ --- the needle is the checker's word, not the candidate's --------------
   s" MZA ( gridctx<b,e,m> acc<t,b,m> -- acc<t,b,m> ) drop ( 't' is minted into family 'acc' but is unbound in the inputs ) ACC-ZERO"
   s" anti-fool: a certifying candidate whose own source carries the needle" ACCEPTS
   s" NEG: source text cannot satisfy a needle - only the rendered diagnostic can" type cr

   \ --- why M1..M4 were re-aimed: this shape never reaches the seal --------
   s" MZX ( span<space-global,t,e> gridctx<b,e,m> -- tile<u,b,m> ) [: EMIT-LOAD ;] PTXREP:MINT-LOAD"
   s" declared quantifiers 'u' and 't' are unified"
   s" a free MINT-LOAD output aliases its projected operand, seal unreached" PTXN-REJECTS
   s" NEG: MINT-LOAD's output is fully projected, so a free element aliases first" type cr

   \ --- layer 1: the minted output family is pinned ------------------------
   s" MF1 ( span<space-global,t,e> gridctx<b,e,m> -- acc<t,b,m> ) [: EMIT-LOAD ;] PTXREP:MINT-LOAD"
   s" expected: acc<a,b,c> actual: tile<a,b,c>"
   s" MINT-LOAD family relabel (tile->acc) reject" PTXN-REJECTS
   s" NEG: MINT-LOAD output family is pinned; acc relabel rejects (family forge)" type cr

   s" MF2 ( matrix<space-global,t,e,k> rowidx<e> -- matrix<space-global,t,e,k> ) [: EMIT-ROW-SPAN ;] PTXREP:MINT-ROW-SPAN"
   s" expected: matrix<space-global,a,b,c> actual: span<space-global,a,c>"
   s" MINT-ROW-SPAN family relabel (span->matrix) reject" PTXN-REJECTS
   s" NEG: MINT-ROW-SPAN output family is pinned; matrix relabel rejects (family forge)" type cr

   T-REPORT ;

MAIN

;package

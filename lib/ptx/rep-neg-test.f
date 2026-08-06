\ rep-neg-test.f - committed negative regressions for the phantom-preserving
\ register-emitter capability (lib/ptx/rep.f, package PTXREP).
\
\ Each body is a source-equivalent op the checker MUST reject fail-closed, pinning
\ the three soundness rules the mission requires (parallel to tile-v4a-neg-test.f):
\
\   N1 forge (cross-family)   - REP2 preserves ONE operand phantom, so relabeling
\                               an mmaslice as an mmbslice cannot unify (the two
\                               operands are one `a`): the emitter cannot forge a
\                               different kernel family.
\   N2 forge (REPMIX2 output) - REPMIX2 preserves the FIRST operand's phantom, so
\                               an mmaslice-in / mmbslice-out relabel rejects even
\                               though the second operand differs.
\   N3 arity drift            - a unary ( n -- n ) emitter routed through the
\                               binary REP2 rejects on the quotation effect.
\   N4 kind mismatch          - a wide multi-cell layout family cannot bind the
\                               single-cell rep var `a`, so the register-emitter
\                               combinators never apply to a non-scalar kind.
\   N5 SINK3 mint (forge)     - a phantom-consuming SINK3 returns NOTHING, so a
\                               wrapper that declares an output phantom cannot
\                               certify: a sink can neither mint nor forge a value.
\   N6 SINK3 arity            - a 4-consuming emitter ( n n n n -- ) routed through
\                               SINK3 ( [ n n n -- ] ) rejects on the quotation.
\   N7 SINK4 mint (forge)     - as N5 for the 4-operand indexed sink.
\   N8 SINK4 arity            - a 3-consuming emitter ( n n n -- ) routed through
\                               SINK4 ( [ n n n n -- ] ) rejects on the quotation.
\   N9 REPMIX3 forge          - REPMIX3 preserves the FIRST operand, so an
\                               acc-in / tile-out relabel (output != first) rejects.
\   N10 REPMIX3B forge        - REPMIX3B preserves the SECOND operand, so a
\                               uniform-out relabel (output != second) rejects.
\
\ WHAT EACH NEEDLE IS. Every one of these rejects by unification at the combinator
\ call, which renders no rule sentence - only the mismatching terms. The needle is
\ therefore the whole rendered `at 'CALLSITE' expected: ... actual: ...` span, so
\ it pins WHY the candidate rejected and not merely that some message named the
\ combinator. The needles these cases used to carry were bare combinator names
\ (`REP2`, `SINK3`), and a bare name survives a changed rejection reason: the
\ candidate still fails at the same call site, the diagnostic still prints the
\ name, and the case stays green with the soundness rule it claims to pin gone.
\ Measured, not argued - relaxing REP2's declared operand discipline from
\ ( a a [ n n -- n ] -- a ) to ( a b [ n n -- n ] -- a ) deletes the one-phantom
\ rule N1 exists for and still leaves the whole PTX library certifying, and under
\ that mutation the bare-name version of this file reported `test: ok`. The spans
\ below turn the same mutation red on N1, N3 and N4.
\
\ WHY THE CALL SITE IS PART OF THE SPAN. N5 and N7 render byte-identical term
\ spans - `expected: tile<a,b,c> actual: ` with an EMPTY actual, which is exactly
\ the sink-mint rule: the sink produced nothing where a tile was declared. Only
\ the call site tells the SINK3 case from the SINK4 case, so dropping it would let
\ either be satisfied by the other's diagnostic. The same span is kept on all ten
\ so one rule covers the file.
\
\ REPECHO IS THE ANTI-FOOL CASE. It is a VALID REP2 wrapper that certifies with an
\ EMPTY diagnostic while carrying N1's entire needle verbatim in its own source, in
\ a Forth comment. That is what makes a matched needle the CHECKER's word: the
\ buffer the assertions read stays empty for a source that contains the text, so it
\ cannot be an echo of the candidate. (Same role as MZA in lib/ptx/mint-neg-test.f.)

require lib/ptx/neg-test-lib.f

package REP-NEG
using PTXN
private

\ a deliberately WIDE (2-payload variant => width 2) family for the kind reject.
SUMTYPE repwide 0 VARIANT both n n ;VARIANT ;SUMTYPE

: MAIN ( -- )
   T-RESET
   256 %BLOCK

   s" FORGE ( mmaslice<t,b,l,w,p> mmbslice<t,b,l,w,p> -- mmbslice<t,b,l,w,p> ) [: EMIT-ADD ;] PTXREP:REP2"
   s" at 'PTXREP:REP2' expected: mmbslice<a,b,c,d,e> mmbslice<a,b,c,d,e> [ n n-- n] actual: mmaslice<a,b,c,d,e> mmbslice<a,b,c,d,e> [ n n-- n]"
   s" phantom forge cross-family (mmaslice->mmbslice) reject" REJECTS
   s" NEG: REP2 cannot relabel an mmaslice operand as an mmbslice (forge)" type cr

   s" FORGEMIX ( mmaslice<t,b,l,w,p> uniform<f32> -- mmbslice<t,b,l,w,p> ) [: EMIT-SCALE ;] PTXREP:REPMIX2"
   s" at 'PTXREP:REPMIX2' expected: mmbslice<a,b,c,d,e> actual: mmaslice<a,b,c,d,e>"
   s" phantom forge REPMIX2 output reject" REJECTS
   s" NEG: REPMIX2 output must equal the FIRST operand phantom (forge)" type cr

   s" ARITYBAD ( tile<t,b,m> tile<t,b,m> -- tile<t,b,m> ) [: EMIT-RELU ;] PTXREP:REP2"
   s" at 'PTXREP:REP2' expected: tile<a,b,c> tile<a,b,c> [ n n-- n] actual: tile<a,b,c> tile<a,b,c> [ n-- n]"
   s" emitter arity drift (unary into binary) reject" REJECTS
   s" NEG: a unary ( n -- n ) emitter cannot satisfy binary REP2 (arity)" type cr

   s" KINDBAD ( repwide repwide -- repwide ) [: EMIT-ADD ;] PTXREP:REP2"
   s" at 'PTXREP:REP2' expected: a a [ n n-- n] actual: repwide<> repwide<> [ n n-- n]"
   s" wide-layout kind mismatch reject" REJECTS
   s" NEG: a wide multi-cell family cannot bind the single-cell rep var (kind)" type cr

   s" SINK3MINT ( tile<t,b,m> span<space-global,t,e> gridctx<b,e,m> -- tile<t,b,m> ) [: EMIT-STORE ;] PTXREP:SINK3"
   s" at 'PTXREP:SINK3' expected: tile<a,b,c> actual:"
   s" SINK3 cannot mint an output phantom reject" REJECTS
   s" NEG: a SINK3 store returns nothing, so it cannot mint/forge a phantom (mint)" type cr

   s" SINK3AR ( tile<t,b,m> span<space-global,u32,i> span<space-global,t,e> uniqidxctx<b,i,e,m> -- ) [: EMIT-INDEX-STORE ;] PTXREP:SINK3"
   s" at 'PTXREP:SINK3' expected: a b c [ n n n-- ] actual: tile<d,e,f> span<space-global,u32,g> span<space-global,d,h> uniqidxctx<e,g,h,f> [ n n n n-- ]"
   s" SINK3 emitter arity drift (4-ary into 3-ary) reject" REJECTS
   s" NEG: a 4-consuming emitter cannot satisfy ternary SINK3 (arity)" type cr

   s" SINK4MINT ( tile<t,b,m> span<space-global,u32,i> span<space-global,t,e> uniqidxctx<b,i,e,m> -- tile<t,b,m> ) [: EMIT-INDEX-STORE ;] PTXREP:SINK4"
   s" at 'PTXREP:SINK4' expected: tile<a,b,c> actual:"
   s" SINK4 cannot mint an output phantom reject" REJECTS
   s" NEG: a SINK4 store returns nothing, so it cannot mint/forge a phantom (mint)" type cr

   s" SINK4AR ( tile<t,b,m> span<space-global,t,e> gridctx<b,e,m> x -- ) [: EMIT-STORE ;] PTXREP:SINK4"
   s" at 'PTXREP:SINK4' expected: a b c d [ n n n n-- ] actual: tile<e,f,g> span<space-global,e,h> gridctx<f,h,g> i [ n n n-- ]"
   s" SINK4 emitter arity drift (3-ary into 4-ary) reject" REJECTS
   s" NEG: a 3-consuming emitter cannot satisfy quaternary SINK4 (arity)" type cr

   s" R3FORGE ( acc<t,b,m> tile<t,b,m> tile<t,b,m> -- tile<t,b,m> ) [: EMIT-ACC-FMA ;] PTXREP:REPMIX3"
   s" at 'PTXREP:REPMIX3' expected: tile<a,b,c> actual: acc<a,b,c>"
   s" REPMIX3 output must equal the FIRST operand phantom reject" REJECTS
   s" NEG: REPMIX3 output must equal the FIRST operand phantom (forge)" type cr

   s" R3BFORGE ( uniform<t> tile<t,b,m> tile<t,b,m> -- uniform<t> ) [: EMIT-FMA ;] PTXREP:REPMIX3B"
   s" at 'PTXREP:REPMIX3B' expected: uniform<a> actual: tile<a,b,c>"
   s" REPMIX3B output must equal the SECOND operand phantom reject" REJECTS
   s" NEG: REPMIX3B output must equal the SECOND operand phantom (forge)" type cr

   \ --- the needle is the checker's word, not the candidate's ------------------
   s" REPECHO ( mmbslice<t,b,l,w,p> mmbslice<t,b,l,w,p> -- mmbslice<t,b,l,w,p> ) ( at 'PTXREP:REP2' expected: mmbslice<a,b,c,d,e> mmbslice<a,b,c,d,e> [ n n-- n] actual: mmaslice<a,b,c,d,e> mmbslice<a,b,c,d,e> [ n n-- n] ) [: EMIT-ADD ;] PTXREP:REP2"
   s" anti-fool: a certifying REP2 whose own source carries N1's whole needle" ACCEPTS
   s" NEG: source text cannot satisfy a needle - only the rendered diagnostic can" type cr

   T-REPORT ;

MAIN

;using
;package

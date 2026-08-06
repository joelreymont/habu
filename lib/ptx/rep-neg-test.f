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

require lib/ptx/neg-test-lib.f

package REP-NEG
private

\ a deliberately WIDE (2-payload variant => width 2) family for the kind reject.
SUMTYPE repwide 0 VARIANT both n n ;VARIANT ;SUMTYPE

: MAIN ( -- )
   T-RESET
   256 %BLOCK

   s" FORGE ( mmaslice<t,b,l,w,p> mmbslice<t,b,l,w,p> -- mmbslice<t,b,l,w,p> ) [: EMIT-ADD ;] PTXREP:REP2"
   s" REP2" s" phantom forge cross-family (mmaslice->mmbslice) reject" PTXN:REJECTS
   s" NEG: REP2 cannot relabel an mmaslice operand as an mmbslice (forge)" type cr

   s" FORGEMIX ( mmaslice<t,b,l,w,p> uniform<f32> -- mmbslice<t,b,l,w,p> ) [: EMIT-SCALE ;] PTXREP:REPMIX2"
   s" REPMIX2" s" phantom forge REPMIX2 output reject" PTXN:REJECTS
   s" NEG: REPMIX2 output must equal the FIRST operand phantom (forge)" type cr

   s" ARITYBAD ( tile<t,b,m> tile<t,b,m> -- tile<t,b,m> ) [: EMIT-RELU ;] PTXREP:REP2"
   s" REP2" s" emitter arity drift (unary into binary) reject" PTXN:REJECTS
   s" NEG: a unary ( n -- n ) emitter cannot satisfy binary REP2 (arity)" type cr

   s" KINDBAD ( repwide repwide -- repwide ) [: EMIT-ADD ;] PTXREP:REP2"
   s" REP2" s" wide-layout kind mismatch reject" PTXN:REJECTS
   s" NEG: a wide multi-cell family cannot bind the single-cell rep var (kind)" type cr

   s" SINK3MINT ( tile<t,b,m> span<space-global,t,e> gridctx<b,e,m> -- tile<t,b,m> ) [: EMIT-STORE ;] PTXREP:SINK3"
   s" SINK3" s" SINK3 cannot mint an output phantom reject" PTXN:REJECTS
   s" NEG: a SINK3 store returns nothing, so it cannot mint/forge a phantom (mint)" type cr

   s" SINK3AR ( tile<t,b,m> span<space-global,u32,i> span<space-global,t,e> uniqidxctx<b,i,e,m> -- ) [: EMIT-INDEX-STORE ;] PTXREP:SINK3"
   s" SINK3" s" SINK3 emitter arity drift (4-ary into 3-ary) reject" PTXN:REJECTS
   s" NEG: a 4-consuming emitter cannot satisfy ternary SINK3 (arity)" type cr

   s" SINK4MINT ( tile<t,b,m> span<space-global,u32,i> span<space-global,t,e> uniqidxctx<b,i,e,m> -- tile<t,b,m> ) [: EMIT-INDEX-STORE ;] PTXREP:SINK4"
   s" SINK4" s" SINK4 cannot mint an output phantom reject" PTXN:REJECTS
   s" NEG: a SINK4 store returns nothing, so it cannot mint/forge a phantom (mint)" type cr

   s" SINK4AR ( tile<t,b,m> span<space-global,t,e> gridctx<b,e,m> x -- ) [: EMIT-STORE ;] PTXREP:SINK4"
   s" SINK4" s" SINK4 emitter arity drift (3-ary into 4-ary) reject" PTXN:REJECTS
   s" NEG: a 3-consuming emitter cannot satisfy quaternary SINK4 (arity)" type cr

   s" R3FORGE ( acc<t,b,m> tile<t,b,m> tile<t,b,m> -- tile<t,b,m> ) [: EMIT-ACC-FMA ;] PTXREP:REPMIX3"
   s" REPMIX3" s" REPMIX3 output must equal the FIRST operand phantom reject" PTXN:REJECTS
   s" NEG: REPMIX3 output must equal the FIRST operand phantom (forge)" type cr

   s" R3BFORGE ( uniform<t> tile<t,b,m> tile<t,b,m> -- uniform<t> ) [: EMIT-FMA ;] PTXREP:REPMIX3B"
   s" REPMIX3B" s" REPMIX3B output must equal the SECOND operand phantom reject" PTXN:REJECTS
   s" NEG: REPMIX3B output must equal the SECOND operand phantom (forge)" type cr

   T-REPORT ;

MAIN

;package

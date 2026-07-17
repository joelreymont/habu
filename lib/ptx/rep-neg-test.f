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

require lib/ptx/neg-test-lib.f

\ a deliberately WIDE (2-payload variant => width 2) family for the kind reject.
SUMTYPE repwide 0 VARIANT both n n ;VARIANT ;SUMTYPE

: RN-MAIN ( -- )
   T-RESET
   256 %BLOCK

   s" FORGE ( mmaslice<t,b,l,w,p> mmbslice<t,b,l,w,p> -- mmbslice<t,b,l,w,p> ) [: EMIT-ADD ;] PTXREP:REP2"
   s" REP2" s" phantom forge cross-family (mmaslice->mmbslice) reject" PTXN-REJECTS
   s" NEG: REP2 cannot relabel an mmaslice operand as an mmbslice (forge)" type cr

   s" FORGEMIX ( mmaslice<t,b,l,w,p> uniform<f32> -- mmbslice<t,b,l,w,p> ) [: EMIT-SCALE ;] PTXREP:REPMIX2"
   s" REPMIX2" s" phantom forge REPMIX2 output reject" PTXN-REJECTS
   s" NEG: REPMIX2 output must equal the FIRST operand phantom (forge)" type cr

   s" ARITYBAD ( tile<t,b,m> tile<t,b,m> -- tile<t,b,m> ) [: EMIT-RELU ;] PTXREP:REP2"
   s" REP2" s" emitter arity drift (unary into binary) reject" PTXN-REJECTS
   s" NEG: a unary ( n -- n ) emitter cannot satisfy binary REP2 (arity)" type cr

   s" KINDBAD ( repwide repwide -- repwide ) [: EMIT-ADD ;] PTXREP:REP2"
   s" REP2" s" wide-layout kind mismatch reject" PTXN-REJECTS
   s" NEG: a wide multi-cell family cannot bind the single-cell rep var (kind)" type cr

   T-REPORT ;

RN-MAIN

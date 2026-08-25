\ mint-neg-test.f - committed negative regressions for the checked-mint
\ output-provenance capability (src/core/checker.f NP-MINT-CHECK) and the MINTING
\ combinators (lib/ptx/rep.f: PTXREP:MINT-LOAD / MINT-ROW-SPAN / MINT-ROW-LOAD).
\
\ A MINTING wrapper REPACKAGES register operands into a NEW register-phantom
\ family whose type arguments are all PROJECTED from the operands. Each body below
\ is a source-equivalent op the checker MUST reject fail-closed, pinning the two
\ soundness layers (parallel to rep-neg-test.f):
\
\   M1 free element (mint forge) - a wrapper that declares an output element var
\                                  unbound in its inputs (`tile<u,b,m>`) mints a
\                                  phantom of input-unrelated type: NP-MINT-CHECK
\                                  rejects (E-NONPARAMETRIC-EFFECT, "minted into").
\   M2 free block  (mint forge)  - as M1 for the block position (`tile<t,c,m>`).
\   M3 free elem via ROW-SPAN    - the span-minting combinator obeys the same seal
\                                  (`span<u,k>` with u unbound rejects).
\   M4 free elem via ROW-LOAD    - the row-load combinator likewise.
\   M5 wrong family              - MINT-LOAD's declared `tile` output pins the
\                                  family: an `acc`-out relabel rejects by
\                                  unification, so a wrapper cannot re-tag the
\                                  minted family.
\   M6 wrong family via ROW-SPAN - MINT-ROW-SPAN pins `span`: a `matrix`-out
\                                  relabel rejects.
\
\ NB the register<->phantom mint stays available ONLY behind the audited TRUSTED:
\ combinators; a plain `:` wrapper cannot forge a free-typed phantom. The
\ fresh-mask CONTEXT mints (GRID-CTX / ROW-CTX / …) keep their own TRUSTED: rows —
\ their block/mask outputs are genuinely fresh (unbound in inputs), so the same
\ seal is exactly why they cannot be rewritten as checked callers.

require lib/ptx/neg-test-lib.f

: MN-MAIN ( -- )
   T-RESET
   256 %BLOCK

   s" MLDF ( span<space-global,t,e> gridctx<b,e,m> -- tile<u,b,m> ) [: EMIT-LOAD ;] PTXREP:MINT-LOAD"
   s" minted into" s" MINT-LOAD free output element reject" PTXN-REJECTS
   s" NEG: MINT-LOAD cannot mint a free-typed element (mint forge)" type cr

   s" MLDB ( span<space-global,t,e> gridctx<b,e,m> -- tile<t,c,m> ) [: EMIT-LOAD ;] PTXREP:MINT-LOAD"
   s" minted into" s" MINT-LOAD free output block reject" PTXN-REJECTS
   s" NEG: MINT-LOAD cannot mint a free-typed block (mint forge)" type cr

   s" MRSF ( matrix<space-global,t,e,k> rowidx<e> -- span<space-global,u,k> ) [: EMIT-ROW-SPAN ;] PTXREP:MINT-ROW-SPAN"
   s" minted into" s" MINT-ROW-SPAN free output element reject" PTXN-REJECTS
   s" NEG: MINT-ROW-SPAN cannot mint a free-typed element (mint forge)" type cr

   s" MRLF ( span<space-global,t,k> rowctx<b,k,m> -- tile<u,b,m> ) [: EMIT-ROW-LOAD ;] PTXREP:MINT-ROW-LOAD"
   s" minted into" s" MINT-ROW-LOAD free output element reject" PTXN-REJECTS
   s" NEG: MINT-ROW-LOAD cannot mint a free-typed element (mint forge)" type cr

   s" MLDA ( span<space-global,t,e> gridctx<b,e,m> -- acc<t,b,m> ) [: EMIT-LOAD ;] PTXREP:MINT-LOAD"
   s" MINT-LOAD" s" MINT-LOAD family relabel (tile->acc) reject" PTXN-REJECTS
   s" NEG: MINT-LOAD output family is pinned; acc relabel rejects (family forge)" type cr

   s" MRSM ( matrix<space-global,t,e,k> rowidx<e> -- matrix<space-global,t,e,k> ) [: EMIT-ROW-SPAN ;] PTXREP:MINT-ROW-SPAN"
   s" MINT-ROW-SPAN" s" MINT-ROW-SPAN family relabel (span->matrix) reject" PTXN-REJECTS
   s" NEG: MINT-ROW-SPAN output family is pinned; matrix relabel rejects (family forge)" type cr

   T-REPORT ;

MN-MAIN

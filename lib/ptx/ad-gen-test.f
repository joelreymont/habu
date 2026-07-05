\ ad-gen-test.f - the composed reverse pass and the generated-body lowering.
\
\ AD-BACKWARD$ (reverse + substitute from the vjp.f table + simplify) is
\ asserted on exact generated text, including the XSUBSUM body the device gate
\ launches and the NEG NEG collapse through the composed pipeline. Control flow
\ stays a named fail-closed reject across token classes. The ADG lowering
\ contract rejects before any emit: unknown tokens, saved-value tokens (no
\ lowering yet), cotangent-stack underflow, and unbalanced bodies. Emitting
\ rejects (double load, dangling value) are asserted under capture in
\ tools/ptx/saxpy-test.f.

require lib/ptx/test-prelude.f
require lib/ptx/ad-gen.f

T-RESET

\ ---- the composed pass: generated backward text ----------------------------------
s" ROW-LOAD DUP BLOCK-SUM PTX:B- ROW-STORE" AD-BACKWARD$
   s" ROW-LOAD DUP BLOCK-SUM NEG BROADCAST +. ROW-SCATTER-ADD" T$=
\ the pipeline simplifies what the raw reversal leaves behind (NEG NEG cancels)
s" LOAD NEG NEG STORE" AD-REVERSE s" LOAD NEG NEG SCATTER-ADD" T$=
s" LOAD NEG NEG STORE" AD-BACKWARD$ s" LOAD SCATTER-ADD" T$=
\ nonlinear expansions flow through the composed pass unchanged
s" LOAD EXP. STORE" AD-BACKWARD$ s" LOAD SAVED-Y *. SCATTER-ADD" T$=

\ ---- control flow is a named reject across token classes -------------------------
: ADGT-IF ( -- )    s" LOAD if STORE then" AD-BACKWARD$ 2drop ;
' ADGT-IF E-PTX-AD-CONTROL TTHROWS
: ADGT-LOOP ( -- )  s" LOAD begin +. repeat STORE" AD-BACKWARD$ 2drop ;
' ADGT-LOOP E-PTX-AD-CONTROL TTHROWS
: ADGT-DO ( -- )    s" LOAD ?do +. loop STORE" AD-BACKWARD$ 2drop ;
' ADGT-DO E-PTX-AD-CONTROL TTHROWS
: ADGT-REC ( -- )   s" LOAD recurse STORE" AD-BACKWARD$ 2drop ;
' ADGT-REC E-PTX-AD-CONTROL TTHROWS

\ ---- lowering contract: pre-emit fail-closed rejects ------------------------------
: ADGT-UNKNOWN ( -- )   s" FROB" 1 2 3 ADG-LOWER ;
' ADGT-UNKNOWN E-PTX-AD-UNKNOWN TTHROWS
: ADGT-SAVED ( -- )     s" SAVED-X" 1 2 3 ADG-LOWER ;
' ADGT-SAVED E-PTX-NOIMPL TTHROWS
: ADGT-UNDERFLOW ( -- ) s" +." 1 2 3 ADG-LOWER ;
' ADGT-UNDERFLOW E-PTX-AD-UNDERFLOW TTHROWS
: ADGT-NEG-EMPTY ( -- ) s" NEG" 1 2 3 ADG-LOWER ;
' ADGT-NEG-EMPTY E-PTX-AD-UNDERFLOW TTHROWS
: ADGT-STORE-EMPTY ( -- ) s" ROW-STORE" 1 2 3 ADG-LOWER ;
' ADGT-STORE-EMPTY E-PTX-AD-UNDERFLOW TTHROWS
: ADGT-EMPTY ( -- )     s" " 1 2 3 ADG-LOWER ;
' ADGT-EMPTY E-PTX-AD-OUTPUT TTHROWS

\ ---- saved-value resolution (habu-ad-thread-saved) --------------------------------
\ The saves-op scan drives the recompute lowering: 0 saves-ops skip the
\ recompute, exactly 1 binds its SAVED-* registers, more than one is a named
\ fail-closed reject BEFORE any emit (multi-save threading is DAG work).
s" ROW-LOAD DUP BLOCK-SUM PTX:B- ROW-STORE" ADG-SAVES-OP# 0 T=
s" ROW-LOAD EXP. ROW-STORE" ADG-SAVES-OP# 1 T=
s" ROW-LOAD DUP BLOCK-MAX PTX:B- EXP. DUP BLOCK-SUM PTX:B/ ROW-STORE" ADG-SAVES-OP# 3 T=

: ADGT-MULTI-SAVES ( -- )
   s" ROW-LOAD EXP. EXP. ROW-STORE"
   s" ROW-LOAD SAVED-Y *. SAVED-Y *. ROW-SCATTER-ADD"
   1 2 3 4 ADG-LOWER-BWD ;
' ADGT-MULTI-SAVES E-PTX-NOIMPL TTHROWS

\ the lowering consults the cost model (AD-SAVE?): a collective-heavy slice
\ whose saved value the model chooses to SAVE fail-closes BEFORE any emit -
\ the save lowering is the materialized-output/closed-form route
\ (SOFTMAX_BWD_ROWS), not the generated-kernel recompute.
: ADGT-SAVE-CHOSEN ( -- )
   s" ROW-LOAD DUP BLOCK-MAX PTX:B- ROW-STORE"
   s" ROW-LOAD DUP BLOCK-SUM NEG SAVED-X SAVED-MX BLOCK-MAX-SELECT +. ROW-SCATTER-ADD"
   1 2 3 4 ADG-LOWER-BWD ;
' ADGT-SAVE-CHOSEN E-PTX-NOIMPL TTHROWS

T-REPORT

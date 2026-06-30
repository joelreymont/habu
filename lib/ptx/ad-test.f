\ ptx-ad-test.f - runnable tests for the reverse-mode AD pass v0.

T-RESET

\ VJP table: each linear primitive maps to its mutual adjoint
s" LOAD"      VJP-ADJOINT s" STORE"     STR= TTRUE
s" STORE"     VJP-ADJOINT s" LOAD"      STR= TTRUE
s" DUP"       VJP-ADJOINT s" +."        STR= TTRUE
s" +."        VJP-ADJOINT s" DUP"       STR= TTRUE
s" BLOCK-SUM" VJP-ADJOINT s" BROADCAST" STR= TTRUE
s" BROADCAST" VJP-ADJOINT s" BLOCK-SUM" STR= TTRUE

\ the reverse pass: forward body -> backward body (reverse order + VJP substitution)
\ forward  LOAD DUP BLOCK-SUM   =>   VJP[BLOCK-SUM] VJP[DUP] VJP[LOAD]
s" LOAD DUP BLOCK-SUM" AD-REVERSE s" BROADCAST +. STORE" STR= TTRUE
\ single word
s" +." AD-REVERSE s" DUP" STR= TTRUE
\ a longer linear pipeline
s" LOAD LOAD +. STORE" AD-REVERSE s" LOAD DUP STORE STORE" STR= TTRUE
\ row pipeline (ROW-LOAD/ROW-STORE mutual adjoints; NEG self-adjoint)
s" ROW-LOAD" VJP-ADJOINT s" ROW-STORE" STR= TTRUE
s" NEG" VJP-ADJOINT s" NEG" STR= TTRUE
s" ROW-LOAD BLOCK-SUM BROADCAST ROW-STORE" AD-REVERSE
   s" ROW-LOAD BLOCK-SUM BROADCAST ROW-STORE" STR= TTRUE

\ a forward word with no registered adjoint fails closed
: BAD-VJP ( -- )  s" SCALE" VJP-ADJOINT 2drop ;
' BAD-VJP E-PTX-NOVJP TTHROWS

\ control flow is a named straight-line-boundary reject, not a generic missing VJP
: BAD-AD-CONTROL ( -- )
   s" LOAD if STORE then" AD-REVERSE 2drop ;
' BAD-AD-CONTROL E-PTX-AD-CONTROL TTHROWS

\ save-vs-recompute: nonlinear ops save primals/outputs, linear ones save nothing
s" EXP."      VJP-SAVES 1 T=
s" *."        VJP-SAVES 2 T=
s" BLOCK-MAX" VJP-SAVES 2 T=
s" +."        VJP-SAVES 0 T=
s" LOAD"      VJP-SAVES 0 T=
s" EXP." VJP-NONLINEAR? TTRUE
s" +."   VJP-NONLINEAR? TFALSE
\ recompute chosen only when cheaper than the save round-trip
10 3  AD-RECOMPUTE? TTRUE      \ recompute(3) < save(10)
3 10  AD-RECOMPUTE? TFALSE     \ recompute(10) !< save(3)

\ nonlinear automation: a unary nonlinear op auto-derives its adjoint EXPANSION
\ (with saved-value references) inside the reversed backward
s" LOAD EXP. STORE" AD-REVERSE  s" LOAD SAVED-Y *. STORE" STR= TTRUE
s" ROW-LOAD BLOCK-MAX ROW-STORE" AD-REVERSE
   s" ROW-LOAD SAVED-X SAVED-MX BLOCK-MAX-SELECT ROW-STORE" STR= TTRUE
\ binary nonlinear ops: 2-output adjoints expand with stack-threaded cotangents
s" LOAD *. STORE" AD-REVERSE
   s" LOAD DUP SAVED-Y *. SWAP SAVED-X *. STORE" STR= TTRUE
s" LOAD PTX:B- STORE" AD-REVERSE
   s" LOAD DUP BLOCK-SUM NEG STORE" STR= TTRUE
\ PTX:B/ (z=x/s): dx=dz/s, ds=-Sum(dz*z)/s - the last softmax op's 2-output adjoint
s" LOAD PTX:B/ STORE" AD-REVERSE
   s" LOAD DUP SAVED-S PTX:B/ SWAP SAVED-Z *. BLOCK-SUM NEG SAVED-S PTX:U/ STORE" STR= TTRUE
\ the FULL softmax forward now derives a complete backward (every op covered, incl. PTX:B/) -
\ AD-REVERSE does not throw E-PTX-NOVJP and produces a non-empty backward body
s" LOAD DUP BLOCK-MAX PTX:B- EXP. DUP BLOCK-SUM PTX:B/" AD-REVERSE nip 0 > TTRUE

\ algebraic-simplify: adjacent NEG NEG cancels (double negation = identity)
s" NEG NEG"          AD-SIMPLIFY s" "       STR= TTRUE
s" DUP NEG NEG +."   AD-SIMPLIFY s" DUP +." STR= TTRUE
s" NEG"              AD-SIMPLIFY s" NEG"     STR= TTRUE
s" +. STORE"         AD-SIMPLIFY s" +. STORE" STR= TTRUE

T-REPORT

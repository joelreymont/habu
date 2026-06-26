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

\ a forward word with no registered adjoint fails closed
: BAD-VJP ( -- )  s" SCALE" VJP-ADJOINT 2drop ;
' BAD-VJP E-PTX-NOVJP TTHROWS

T-REPORT

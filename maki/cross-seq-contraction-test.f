\ cross-seq-contraction-test.f - BTC-5 soundness closer (dot habu-cross-seq-
\ contraction-34a6265f, docs/batch-sequence-design.md §5 BTC-5).
\
\ The (B,T,C) fold packs B sequences of length T into B*T rows. The named risk
\ (§4 counterargument 2): nothing in the 2D tensor type stops a plain MATMUL over
\ the folded B*T rows from contracting ACROSS sequences, where only the within-
\ sequence #T axis is a legal summation axis. BTC-5 is the regression proving that,
\ on the landed extent-role substrate (BTC-7 extprod/redx + BTC-2 free extents),
\ that bad program is a LOAD-TIME checker reject (exit 70) with a named diagnostic
\ — NOT a runtime error, and NOT a runtime guard. The folded row is typed over the
\ ordered product former extprod<free,inner> (maki/extent.f EXTPROD:); a contraction
\ (redx) accepts the inner #T and REJECTS the whole product (checker.f
\ EXT-REDX-BAD-ARG? at SIG-END-PARAM, checker.f:2716,2728).
\
\ Both directions:
\   LEGAL (loads AND runs): split the fold into free #XSB x inner #XST, contract
\     over the inner #XST only. XS-LEGAL's declared signature carries redx<extxst>
\     (an inner contraction) — ACCEPTED at load — and the body runs the real split
\     arithmetic (#XSR-SPLIT: b = r / T, t = r mod T).
\   BAD (rejects at LOAD, exit 70, named): a word whose signature contracts the
\     WHOLE fold (redx over the extprod). Proven with the reduced-bad-program idiom
\     of test/load-reject-diag-test.f — a spawned child engine LOADS the bad program
\     and this suite asserts the child's process exit code (70) + the named stderr
\     diagnostic — so the committed suite stays green while proving the reject. Spawn
\     (fork+exec), not bare fork: safe inside the maki gate where an earlier suite may
\     have initialized CUDA (maki/eval/device-fault-test.f:8-12).
\
\ Overlap with BTC-7 (honest): the CHECKER RULE this fixture exercises — redx over a
\ whole product rejects — is the one BTC-7 landed, and BTC-7's in-process candidate
\ scores already cover that verdict (maki/extent-test.f:100 PCP `redx<extprod<..>> >RED`
\ scored 0; test/extent-product-test.f:74 XCP scored 0). BTC-5 adds the SPECIFIC named
\ regression the contract pins: the folded-B*T-rows misuse proven as a genuine
\ LOAD-TIME process reject (exit 70 + named diagnostic) — not merely an in-process
\ candidate verdict — with the legal split-and-contract running alongside it.

require lib/test.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require maki/extent.f

T-RESET

package MAKI

\ ---- the folded (B,T) row extent, factored free #XSB (batch #B) x inner #XST (seq #T) ----
8   EXTENT: #XSB     \ free / batch factor #B
16  EXTENT: #XST     \ in-block / sequence factor #T (the ONLY legal contraction axis)
128 EXTENT: #XSR     \ folded rows = B*T = 8*16
EXTPROD: #XSR ( #XSB #XST )

\ ===== LEGAL direction (loads AND runs): split the fold, contract over inner #T only =====
\ The declared signature carries redx<extxst> (an INNER contraction) — ACCEPTED at load —
\ and the body runs the real split arithmetic: row r -> free b = r / 16, inner t = r mod 16.
: XS-LEGAL ( ix<extprod<extxsb,extxst>> -- ix<extxsb> redx<extxst> ) #XSR-SPLIT >RED ;
: XS-BATCH ( n -- n )  #XSR-FOLD XS-LEGAL drop IX>N ;   \ raw row -> batch index (inner #T contracted away)
50 XS-BATCH 3 T=       \ row 50 -> batch 3 (50 / 16), inner 2 contracted
17 XS-BATCH 1 T=       \ row 17 -> batch 1 (17 / 16)

\ ===== BAD direction (rejects at LOAD, exit 70, named): contract the WHOLE fold =====
$1000 constant CS-CAP
70 constant CS-REJECT-RC
60000 constant CS-TIMEOUT-MS
create CS-OUT CS-CAP allot
create CS-ERR CS-CAP allot
variable CS-RC
variable CS-OUTU
variable CS-ERRU
variable CS-EXITED

\ the self-contained reduced bad program (a fresh child engine reads it on stdin): fold the
\ B*T rows, then declare a word whose signature contracts the WHOLE fold (redx over the
\ product) — the cross-sequence leak, which the checker must reject at load.
: CS-BAD$ ( -- ptr u8 n )
   s" require maki/extent.f package MAKI 128 EXTENT: #XCR 8 EXTENT: #XCB 16 EXTENT: #XCT EXTPROD: #XCR ( #XCB #XCT ) : XS-CROSS ( ix<extprod<extxcb,extxct>> -- redx<extprod<extxcb,extxct>> ) >RED ; ;package" ;

\ the child engine: the gate's HABU_UNDER_TEST when set, else bin/hb (load-reject-diag pattern).
: CS-HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if 2drop s" bin/hb" exit then ;

: CS-STORE! ( len len outcome -- )
   MATCH outcome
     exited   OF CS-RC ! 0 0=    CS-EXITED ! ENDOF
     signaled OF CS-RC ! 0 0= 0= CS-EXITED ! ENDOF
     timeout  OF 0 CS-RC ! 0 0= 0= CS-EXITED ! ENDOF
   ;MATCH
   LEN>N CS-ERRU !  LEN>N CS-OUTU ! ;

: CS-RUN ( -- )   \ spawn the child engine, feed the bad program on stdin, capture the outcome
   PROC-ARGV-RESET
   CS-HB$ >LEN  CS-BAD$ >LEN  CS-OUT CS-CAP >LEN  CS-ERR CS-CAP >LEN  CS-TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME  CS-STORE! ;

: CS-ERR$ ( -- ptr u8 n )  CS-ERR CS-ERRU @ ;

CS-RUN
CS-EXITED @ TTRUE                        \ the child EXITED (not signaled / timed out)
CS-RC @ CS-REJECT-RC T=                  \ ...at process exit 70 — the load-time checker reject
CS-OUTU @ 0 T=                           \ empty stdout (the reject printed nothing to stdout)
CS-ERR$ s" in xs-cross" CONTAINS? TTRUE  \ stderr names the rejected definition
CS-ERR$ s" at 'redx'" CONTAINS? TTRUE    \ ...at the redx contraction over the whole fold

;package

T-REPORT

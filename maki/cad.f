\ maki/cad.f - Model CAD REPL command skeleton (dot cad-0b).
\
\ The REPL surface for docs/model-cad.md Phase 0. `MODEL:` defines a named toy
\ composition over the Phase-1 op set; every inspection command returns a
\ structured cad-0a report (maki/report.f) that honestly states what is not yet
\ computed -- conservative truth, never fabricated numbers:
\
\   * fusion plan = one region per op, no fusion (the planner is cad-2);
\   * byte/coalescing estimates = unknown (they need shapes, cad-3);
\   * schedule = a single host-reference candidate (the autotuner is cad-4);
\   * CERTIFY = model-level static legality only (kernel legality is cad-1/cad-5);
\   * GOLDEN / GRADCHECK / PROFILE = not-run on a host without a GPU -- a named
\     not-run verdict, never a fake pass and never an error mask.
\
\ PROMOTE refuses with a named throw (E-CAD-GATE) unless every gate passes.
\ OPTIMIZE composes the whole loop into one aggregate report and records the
\ promotion decision as a warning instead of throwing. EXPLAIN renders the
\ repair-packet-discipline failure packets (tools/repair-packet-core.f discipline).
\
\ Commands return the report handle ( -- report ) so they compose and are
\ testable; CAD-SHOW renders one to stdout for interactive use. maki -> habu only;
\ cad owns the error range -5020..-5029.

require lib/string.f
require maki/report.f

-5020 constant E-CAD-NOMODEL   \ command issued with no model defined
-5021 constant E-CAD-OP        \ unknown op token in a MODEL: composition
-5022 constant E-CAD-EMPTY     \ MODEL: with no name or no ops
-5023 constant E-CAD-GATE      \ PROMOTE refused: required gates did not pass
-5024 constant E-CAD-SYNTAX    \ malformed MODEL: (unterminated / over-long)

package MAKI
public

\ ---- Phase-1 op set (docs/model-cad.md) ------------------------------------
0  constant OP-ADD
1  constant OP-MUL
2  constant OP-SCALE
3  constant OP-BIAS
4  constant OP-RELU
5  constant OP-GELU
6  constant OP-LAYERNORM
7  constant OP-RMSNORM
8  constant OP-SOFTMAX-ROW
9  constant OP-MATMUL
10 constant OP-LINEAR
11 constant OP-RESIDUAL-ADD
12 constant OP-CAST
13 constant OP-N

private

64 constant MODEL-NAME-CAP
create MODEL-NAME MODEL-NAME-CAP allot   variable MODEL-NAME-U
32 constant MODEL-MAX-OPS
create MODEL-OPS MODEL-MAX-OPS cells allot   variable MODEL-NOPS
variable MODEL-SET?                        \ 0 until a MODEL: succeeds

\ ---- op-name -> op-kind (fail closed: unknown op is rejected, never guessed) -
: OP-KIND ( ptr u8 n -- n )
   2dup s" ADD"          STR= if 2drop OP-ADD          exit then
   2dup s" MUL"          STR= if 2drop OP-MUL          exit then
   2dup s" SCALE"        STR= if 2drop OP-SCALE        exit then
   2dup s" BIAS"         STR= if 2drop OP-BIAS         exit then
   2dup s" RELU"         STR= if 2drop OP-RELU         exit then
   2dup s" GELU"         STR= if 2drop OP-GELU         exit then
   2dup s" LAYERNORM"    STR= if 2drop OP-LAYERNORM    exit then
   2dup s" RMSNORM"      STR= if 2drop OP-RMSNORM      exit then
   2dup s" SOFTMAX-ROW"  STR= if 2drop OP-SOFTMAX-ROW  exit then
   2dup s" MATMUL"       STR= if 2drop OP-MATMUL       exit then
   2dup s" LINEAR"       STR= if 2drop OP-LINEAR       exit then
   2dup s" RESIDUAL-ADD" STR= if 2drop OP-RESIDUAL-ADD exit then
   2dup s" CAST"         STR= if 2drop OP-CAST         exit then
   2drop E-CAD-OP throw ;

\ ---- model store -----------------------------------------------------------
: MODEL-RESET ( -- )  0 MODEL-NOPS !  0 MODEL-NAME-U !  0 MODEL-SET? ! ;

: MODEL-NAME! ( ptr u8 n -- )                  \ copy the transient parse-name token
   {: a:ptr u:n :}
   u MODEL-NAME-CAP > if E-CAD-SYNTAX throw then
   0 begin dup u < while  dup a + c@  over MODEL-NAME + c!  1+  repeat drop
   u MODEL-NAME-U ! ;

: MODEL-OP+ ( n -- )
   {: op:n :}
   MODEL-NOPS @ MODEL-MAX-OPS >= if E-CAD-SYNTAX throw then
   op MODEL-NOPS @ cells MODEL-OPS + !
   MODEL-NOPS @ 1+ MODEL-NOPS ! ;

: MODEL-NAME$ ( -- ptr u8 n )  MODEL-NAME MODEL-NAME-U @ ;
: MODEL-K ( -- n )  MODEL-NOPS @ ;

\ skip an optional `( ... )` stack-effect comment inside a MODEL: line
: SKIP-PAREN ( -- )
   begin
      parse-name dup 0= if 2drop E-CAD-SYNTAX throw then
      2dup s" )" STR= if 2drop exit then
      2drop
   again ;

public

\ MODEL: NAME [ ( ... ) ] OP OP ... ;   (single line; multi-line refill is cad-1)
: MODEL: ( -- )
   MODEL-RESET
   parse-name dup 0= if 2drop E-CAD-EMPTY throw then
   MODEL-NAME!
   begin
      parse-name dup 0= if 2drop E-CAD-SYNTAX throw then
      2dup s" ;" STR= if
         2drop  MODEL-NOPS @ 0= if E-CAD-EMPTY throw then  -1 MODEL-SET? !  exit
      then
      2dup s" (" STR= if 2drop SKIP-PAREN else OP-KIND MODEL-OP+ then
   again ;

: MODEL-CLEAR ( -- )  MODEL-RESET ;
: MODEL-DEFINED? ( -- bool )  MODEL-SET? @ 0= 0= ;

private

\ ---- conservative section builders (read model state, write the report) -----
: CAD-BASE ( report -- report )                \ model name + no-fusion plan (K ops)
   MODEL-SET? @ 0= if E-CAD-NOMODEL throw then
   MODEL-NAME$ RPT-MODEL!
   MODEL-K MODEL-K RPT-OPS!
   MODEL-K RPT-REGIONS!
   MODEL-K RPT-MATERIALIZED! ;

: LOWER-INTO ( report -- report )
   CAD-BASE
   s" lowering: conservative op-per-node; typed IR lands in cad-1-ir" RPT-WARN+ ;

: FUSE-INTO ( report -- report )
   s" phase0: no fusion planner; one region per op (cad-2)" RPT-SPLIT+
   s" fusion: no regions merged this phase" RPT-WARN+ ;

: MEMORY-INTO ( report -- report )
   s" memory: byte + coalescing estimates need shapes (cad-3)" RPT-WARN+ ;

: TILE-INTO ( report -- report )
   s" host-reference-v0" RPT-CAND+  0 RPT-SELECT!
   s" schedule: single host-reference candidate; autotuner in cad-4" RPT-WARN+ ;

: TUNE-INTO ( report -- report )
   TILE-INTO
   s" tune: no measurement history yet (cad-4)" RPT-WARN+ ;

: CERTIFY-INTO ( report -- report )            \ static, no GPU: model-level legality
   s" " V-PASS G-CERTIFY RPT-GATE!
   s" certify: model-level legality only; kernel legality in cad-1/cad-5" RPT-WARN+ ;

: GOLDEN-INTO ( report -- report )
   s" no-device" V-NOTRUN G-GOLDEN RPT-GATE! ;

: GRADCHECK-INTO ( report -- report )
   s" no-device" V-NOTRUN G-GRADCHECK RPT-GATE! ;

: PROFILE-INTO ( report -- report )
   s" no-device" V-NOTRUN G-PROFILE RPT-GATE! ;

\ full conservative report over every phase (PROMOTE / OPTIMIZE / EXPLAIN)
: FULL-REPORT ( -- report )
   RPT-NEW LOWER-INTO FUSE-INTO MEMORY-INTO TILE-INTO
   CERTIFY-INTO GOLDEN-INTO GRADCHECK-INTO PROFILE-INTO ;

\ ---- promotion gate --------------------------------------------------------
: GATE-PASS? ( report n -- report bool )
   over swap RPT-GATE-TAG@ V-PASS = ;

: PROMOTE-OK? ( report -- report bool )        \ all four gates pass
   G-CERTIFY   GATE-PASS? >r
   G-GOLDEN    GATE-PASS? r> and >r
   G-GRADCHECK GATE-PASS? r> and >r
   G-PROFILE   GATE-PASS? r> and ;

: CACHE-KEY-INTO ( report -- report )          \ artifact key (model-scoped in phase 0)
   MODEL-NAME$ RPT-CACHE! ;

: PROMOTE-REPORT ( report -- report )
   PROMOTE-OK? 0= if E-CAD-GATE throw then
   CACHE-KEY-INTO ;

: OPTIMIZE-PROMOTE ( report -- report )        \ record the decision, never throw
   PROMOTE-OK? if
      CACHE-KEY-INTO  s" promote: gates pass; artifact cached" RPT-WARN+
   else
      s" promote: refused; required device gates not run on host" RPT-WARN+
   then ;

public

\ ---- inspection commands (each returns a structured cad-0a report) ----------
: LOWER ( -- report )      RPT-NEW LOWER-INTO ;
: FUSE ( -- report )       RPT-NEW LOWER-INTO FUSE-INTO ;
: MEMORY ( -- report )     RPT-NEW LOWER-INTO MEMORY-INTO ;
: TILE ( -- report )       RPT-NEW LOWER-INTO TILE-INTO ;
: CERTIFY ( -- report )    RPT-NEW LOWER-INTO CERTIFY-INTO ;
: GOLDEN ( -- report )     RPT-NEW LOWER-INTO GOLDEN-INTO ;
: GRADCHECK ( -- report )  RPT-NEW LOWER-INTO GRADCHECK-INTO ;
: PROFILE ( -- report )    RPT-NEW LOWER-INTO PROFILE-INTO ;
: TUNE ( -- report )       RPT-NEW LOWER-INTO TUNE-INTO ;

\ PROMOTE refuses (named throw) unless every gate passes; on success caches.
: PROMOTE ( -- report )  FULL-REPORT PROMOTE-REPORT ;

\ OPTIMIZE composes lower -> fuse -> memory -> tile -> gates -> promote decision.
: OPTIMIZE ( -- report )  FULL-REPORT OPTIMIZE-PROMOTE ;

\ EXPLAIN emits repair-packet-discipline failure lines for every non-pass gate.
: EXPLAIN ( -- ptr u8 n )  FULL-REPORT RPT-RENDER-PACKETS ;

\ CAD-SHOW renders a report's machine view to stdout (interactive convenience).
: CAD-SHOW ( report -- )  RPT-RENDER type cr ;

end-package

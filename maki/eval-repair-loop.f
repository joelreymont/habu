\ maki/eval-repair-loop.f - the repair-loop metric engine shared by the eval-repair
\ tests (maki/eval-repair.f) and the EXPLAIN-packet A/B ablation
\ (maki/eval-repair-ab-test.f). Reopens package EVAL: the REPAIR- module.
\
\ Feed candidate kernel sources one at a time with EVAL:REPAIR-STEP: it counts a
\ repair round per checker rejection and sums authored tokens until the checker
\ certifies (green). repair-rounds = number of checker-guided fixes; tokens-to-green
\ = total kernel-source tokens authored until certification. The checker
\ (EVAL:CHECK-PASSES?) is the in-environment judge.
\
\ This engine is ARM-AGNOSTIC: the difference between rich EXPLAIN-packet feedback
\ and minimal status-quo feedback is expressed purely by WHICH candidate trajectory
\ the caller feeds (a shorter, targeted path vs a longer, floundering one), never by
\ any branch in here. Keeping it in one owned file avoids duplicating the metric.

require lib/ptx/test-prelude.f
require maki/eval.f

package EVAL

variable ER-NTOK  variable ER-ST
\ count whitespace-separated tokens in a kernel source string
\ typed-local-lint: allow-bare-local  (a keeps the `ptr u8` role SPLIT-NEXT needs)
: COUNT-TOKS ( ptr u8 n -- n ) {: a u :}
   0 ER-NTOK !  0 ER-ST !
   begin  a u $20 ER-ST @ SPLIT-NEXT  while      ( tokptr toklen nextstart )
      ER-ST !                                    ( tokptr toklen )
      dup 0 > if  ER-NTOK @ 1+ ER-NTOK !  then  2drop
   repeat
   2drop drop  ER-NTOK @ ;

variable ER-ROUND  variable ER-TOKENS  variable ER-GREEN

public

: REPAIR-RESET ( -- )  0 ER-ROUND !  0 ER-TOKENS !  0 ER-GREEN ! ;
\ one authoring step: count its tokens, check it; reject -> +1 repair round,
\ certify -> green. Steps after green are ignored (the loop has converged).
: REPAIR-STEP ( ptr u8 n -- )
   ER-GREEN @ if 2drop exit then
   2dup COUNT-TOKS  ER-TOKENS @ +  ER-TOKENS !
   CHECK-PASSES? if  -1 ER-GREEN !  else  ER-ROUND @ 1+ ER-ROUND !  then ;
: REPAIR-ROUNDS@ ( -- n )  ER-ROUND @ ;
: REPAIR-TOKENS@ ( -- n )  ER-TOKENS @ ;
: REPAIR-GREEN? ( -- bool )  ER-GREEN @ ;      \ -1 (green) / 0 (not), already canonical

;package

\ maki/eval-repair-loop.f - the repair-loop metric engine shared by the eval-repair
\ tests (maki/eval-repair.f) and the EXPLAIN-packet A/B ablation
\ (maki/eval-repair-ab-test.f). Reopens package EVAL: the REPAIR- module.
\
\ Feed candidate kernel sources one at a time with EVAL:REPAIR-STEP: it counts a
\ repair round per checker rejection and sums authored tokens until the checker
\ certifies (green). repair-rounds = number of checker-guided fixes; tokens-to-green
\ = total kernel-source tokens authored until certification. The checker
\ (EVAL:CHECK-PASSES?) is the in-environment judge. Each step also sums the
\ deterministic model-token estimate (maki/eval-tokest.f GEN-TOK-EST) so every
\ run reports BOTH units: raw whitespace source tokens (REPAIR-TOKENS@) and the
\ tokenizer-approximation estimate (REPAIR-EST@).
\
\ This engine is ARM-AGNOSTIC: the difference between rich EXPLAIN-packet feedback
\ and minimal status-quo feedback is expressed purely by WHICH candidate trajectory
\ the caller feeds (a shorter, targeted path vs a longer, floundering one), never by
\ any branch in here. Keeping it in one owned file avoids duplicating the metric.

require lib/ptx/test-prelude.f
require lib/string.f                 \ STR:SPLIT-NEXT typed token scan + CAD-NUM roles
require maki/eval.f
require maki/eval-tokest.f

\ ---- audited split-result projections (STR:SPLIT-NEXT slices -> raw offsets) -----
\ COUNT-TOKS scans whitespace tokens on the byte-len/byte-off STR:SPLIT-NEXT reports
\ as CAD-NUM roles; the count test and cursor variable want raw offsets. Reopen the
\ unsealed CAD-NUM package for checked bridges over its private projections (no new
\ TRUSTED). dot habu-migrate-maki-str-6e5cabd4.
package CAD-NUM public
: ERL-BL>N ( CAD-NUM:byte-len -- n )  BYTE-LEN>N ;
: ERL-BO>N ( CAD-NUM:byte-off -- n )  BYTE-OFF>N ;
;package

package EVAL

variable ER-NTOK  variable ER-ST
\ count whitespace-separated tokens in a kernel source string
\ typed-local-lint: allow-bare-local  (a keeps the `ptr u8` role the typed split needs)
: COUNT-TOKS ( ptr u8 n -- n ) {: a u :}
   0 ER-NTOK !  0 ER-ST !
   begin  a u STR:LENGTH $20 ER-ST @ STR:OFFSET STR:SPLIT-NEXT MATCH option   ( SOME tokptr byte-len byte-off )
     none OF STR-FALSE ENDOF                              \ no more tokens: end the scan
     some OF STR-SPLIT:UNMAKE                             ( tokptr byte-len byte-off )
        CAD-NUM:ERL-BO>N ER-ST !                          ( tokptr byte-len )
        CAD-NUM:ERL-BL>N dup 0 > if  ER-NTOK @ 1+ ER-NTOK !  then  2drop
        STR-TRUE ENDOF
   ;MATCH
   while repeat
   ER-NTOK @ ;

variable ER-ROUND  variable ER-TOKENS  variable ER-GREEN  variable ER-EST

public

: REPAIR-RESET ( -- )  0 ER-ROUND !  0 ER-TOKENS !  0 ER-GREEN !  0 ER-EST ! ;
\ one authoring step: count its tokens (both units), check it; reject -> +1
\ repair round, certify -> green. Steps after green are ignored (converged).
: REPAIR-STEP ( ptr u8 n -- )
   ER-GREEN @ if 2drop exit then
   2dup COUNT-TOKS  ER-TOKENS +!
   2dup GEN-TOK-EST  ER-EST +!
   CHECK-PASSES? if  -1 ER-GREEN !  else  ER-ROUND @ 1+ ER-ROUND !  then ;
: REPAIR-ROUNDS@ ( -- n )  ER-ROUND @ ;
: REPAIR-TOKENS@ ( -- n )  ER-TOKENS @ ;
: REPAIR-EST@ ( -- n )  ER-EST @ ;
: REPAIR-GREEN? ( -- bool )  ER-GREEN @ ;      \ -1 (green) / 0 (not), already canonical

;package

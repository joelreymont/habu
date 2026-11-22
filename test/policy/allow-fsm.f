\ allow-fsm.f - the policy harness for a sealed FSM table: admit FSM and PFSM
\ and the top-level `require`, seal, load the design named after `--`.
require src/lib/vocabulary.f
require src/lib/fsm.f
require test/policy/fsm-dep.f
using VOCABULARY

: RUN ( -- )
   s" test/policy" >str SOURCE-ROOT!
   s" FSM" >str ALLOW-PACKAGE  s" PFSM" >str ALLOW-PACKAGE
   RESTRICT 0 SCRIPT-ARGV$ included ;
RUN
;using

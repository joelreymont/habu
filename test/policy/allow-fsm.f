\ allow-fsm.f - the policy harness for a sealed FSM table: admit FSM and PFSM
\ and the top-level `require`, seal, load the design named after `--`.
require src/lib/vocabulary.f
require src/lib/fsm.f
require test/policy/fsm-dep.f

: RUN ( -- )
   s" test/policy" VOCABULARY:SOURCE-ROOT!
   s" FSM" VOCABULARY:ALLOW-PACKAGE  s" PFSM" VOCABULARY:ALLOW-PACKAGE
   VOCABULARY:RESTRICT 0 SCRIPT-ARGV$ included ;
RUN

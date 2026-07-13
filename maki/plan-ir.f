\ maki/plan-ir.f - the plan-IR -> execution handoff over the typed async DAG.
\
\ V2 sections 10.4 / 22.3: today the host executor sequences nodes by hidden
\ index order (EX-EXEC's flat loop), exactly as the CUDA driver sequences one
\ stream - an implicit order no plan artifact represents. This file makes that
\ sequencing a TYPED artifact: PIR-BUILD lowers the adopted model IR into the
\ async DAG (one kernel node per IR node on one compute stream; every IR
\ node-operand edge an explicit ADAG-DEP+ dependency), seals it, and PIR-RUN
\ replays the SEALED deterministic topological order through the host executor
\ (EX-PLAN-N once, then EX-STEP per kernel node). DAG node i is IR node i
\ (appended in index order onto the empty DAG), so the handoff adds no second
\ node-identity table.
\
\ Event-record / event-wait nodes are ordering-only on the host (the replay
\ order already honors their edges); copy / memset nodes have no host buffer
\ target yet and fail closed (E-PIR-UNSUP) - executing the same schema on
\ device streams is the Orin-gated leg (dot habu-v2-checked-async-8d460576).
\
\ One concern: the IR -> DAG handoff + the replay driver. The DAG schema is
\ maki/async-dag.f; numeric execution stays maki/executor.f. maki -> habu
\ only; plan-ir owns -5300..-5309.

require maki/model-ir.f
require maki/executor.f
require maki/async-dag.f

-5300 constant E-PIR-EMPTY   \ handoff of an empty model IR
-5301 constant E-PIR-STATE   \ replay driver invoked without a sealed DAG
-5302 constant E-PIR-UNSUP   \ replayed node kind has no host execution (copy / memset)

package MAKI
private

\ one IR operand edge: a node-ref operand is an explicit DAG dependency
\ (input-slot operands are host-ready bound buffers; they carry no edge).
: PIR-DEP1 ( CAD-KIND:node-id MIR:input-index -- )
   {: nd:CAD-KIND:node-id k:MIR:input-index :}
   nd k MIR-IN@ {: ref:MIR:operand-ref :}
   ref MIR-REF-INPUT? if exit then
   ref MIR-REF-NODE NODE>RAW ADAG-NODE-ID
   nd NODE>RAW ADAG-NODE-ID
   ADAG-DEP+ ;

: PIR-DEPS ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd MIR-IN-COUNT@ 0 ?do  nd i MIR-INPUT-IDX PIR-DEP1  loop ;

\ replay one DAG node on the host: kernels execute, events are ordering-only,
\ transfers are the device leg (fail closed).
: PIR-STEP ( ADAG:node-id -- ) {: an:ADAG:node-id :}
   an ADAG-KIND@ MATCH akind
      kernel       OF an ADAG-KERNEL-IR@ EX-STEP ENDOF
      event-record OF ENDOF
      event-wait   OF ENDOF
      copy         OF E-PIR-UNSUP throw ENDOF
      memset       OF E-PIR-UNSUP throw ENDOF
   ;MATCH ;

public

\ lower the adopted model IR into a sealed one-stream async DAG.
: PIR-BUILD ( -- )
   MIR-N@ 0= if E-PIR-EMPTY throw then
   ADAG-RESET
   ADAG-STREAM+ {: st:ADAG:stream-id :}
   MIR-N@ 0 ?do  i MIR-NODE-ID st ADAG-KERNEL+ drop  loop
   MIR-N@ 0 ?do  i MIR-NODE-ID PIR-DEPS  loop
   ADAG-SEAL ;

\ deterministic replay of the sealed DAG through the host executor: plan the
\ node buffers once, then execute in the sealed topological order.
: PIR-RUN ( -- )
   ADAG-SEALED? 0= if E-PIR-STATE throw then
   MIR-N@ EX-PLAN-N
   ADAG-N@ 0 ?do  i ADAG-ORDER@ PIR-STEP  loop ;

;package

\ eval-author.f - unified model-driven authoring grader.
\
\ GRADE-AUTHOR ( a u task -- verdict ) grades a candidate kernel source against its TASK's
\ device golden, returning the author verdict: 0 = checker-rejected (does not certify),
\ 1 = device-wrong (certifies but the device output != the FP32 reference - a semantic bug
\ the checker cannot catch), 2 = GREEN (certifies AND device-correct). It dispatches to the
\ committed device-golden graders: maki/eval-device.f GRADE-CANDIDATE (the SAXPY task) and
\ maki/eval-device-sm.f GRADE-SM (the softmax-rows task). An unknown task fails closed.
\
\ This retires the throwaway /tmp grade_habu*.sh / grade_one.py scripts (docs/eval-triton.md):
\ the model-driven pass@k authoring matrix is now reproducible from the committed tree,
\ run by bin/hb. The Triton baseline and the subagent generation arm stay external + documented
\ (Habu Only). Load after maki/eval-device.f and maki/eval-device-sm.f.

-5003 constant E-MK-EVAL          \ unknown eval task (fail-closed)

package MAKI
public

0 constant TASK-SAXPY
1 constant TASK-SOFTMAX

: GRADE-AUTHOR ( ptr u8 n n -- n ) {: a u task :}
   task case
      TASK-SAXPY   of a u EVAL:GRADE-CANDIDATE endof
      TASK-SOFTMAX of a u EVAL:GRADE-SM        endof
      E-MK-EVAL throw
   endcase ;

end-package

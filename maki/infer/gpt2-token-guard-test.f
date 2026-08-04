\ Spawn-isolated guard-page proofs for production GPT2:ENCODE.

require lib/test.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-candidate.f
require maki/infer/gpt2-generate.f

package GPT2
private

1024 constant TG-CAP
240000 constant TG-MS

create TG-OUT TG-CAP allot
create TG-ERR TG-CAP allot
variable TG-RC
variable TG-OUTU
variable TG-ERRU
variable TG-EXITED

: TG-STORE! ( len len outcome -- )
   MATCH outcome
      exited OF TG-RC ! 0 0= TG-EXITED ! ENDOF
      signaled OF TG-RC ! 0 0= 0= TG-EXITED ! ENDOF
      timeout OF 0 TG-RC ! 0 0= 0= TG-EXITED ! ENDOF
   ;MATCH
   LEN>N TG-ERRU !
   LEN>N TG-OUTU ! ;

: TG-RUN ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" maki/infer/gpt2-token-guard-child.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   0 SCRIPT-ARGV$ >LEN PROC-ARGV+
   PROC-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN
   TG-OUT TG-CAP >LEN TG-ERR TG-CAP >LEN TG-MS T-BUDGET-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME TG-STORE! ;

: TG-TEST ( -- )
   SCRIPT-ARGC 1 <> if E-STR-BOUNDS throw then
   T-RESET
   s" terminal apostrophe and all truncated UTF-8 widths stay in bounds" T-LABEL
   TG-RUN
   TG-EXITED @ TTRUE
   TG-RC @ 0 T=
   TG-OUTU @ 0 T=
   TG-ERRU @ 0 T=
   T-REPORT ;

TG-TEST

;package

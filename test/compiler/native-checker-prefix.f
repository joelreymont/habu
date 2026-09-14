\ Rebuild the checker through the current native owner handoff, then exercise
\ its actual schema rewind and pointer-pool boundaries before sealing.
require lib/test.f
require lib/process-argv.f
require lib/engine-candidate.f

package CHECKER-PREFIX-TEST
$4000 constant CAP
create OUT CAP allot
create ERR CAP allot

: RUN ( -- )
   T-RESET
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/compiler/aot-mode.f" >LEN PROC-ARGV+
   s" test/native-window-owner-child.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" test/compiler/native-checker-storage.f" >LEN PROC-ARGV+
   s" test/compiler/native-prefix-rollback.f" >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN
   OUT CAP >LEN ERR CAP >LEN 180000 >MS RUN-ARGV-CAPTURE
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: ou:len eu:len :}
         OUT ou LEN>N s" window: 0" CONTAINS? 0= eu LEN>N 0<> or if
            OUT ou LEN>N type ERR eu LEN>N type
         then
         OUT ou LEN>N s" window: 0" CONTAINS? TTRUE
         eu LEN>N 0 T=
      ENDOF
      err OF PCAP-FAILED:UNMAKE {: ou:len eu:len rc:rc :}
         OUT ou LEN>N type ERR eu LEN>N type
         rc RC>N 0 T=
      ENDOF
   ;MATCH
   T-REPORT ;
public
RUN
;package

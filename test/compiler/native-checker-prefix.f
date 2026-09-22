\ Rebuild the checker through the current native owner handoff, then exercise
\ its actual schema rewind and pointer-pool boundaries before sealing.
require lib/test.f
require lib/fs-mutate.f
require lib/process-argv.f
require lib/process-env.f
require test/whitebox-child.f

package CHECKER-PREFIX-TEST
$4000 constant CAP
create OUT CAP allot
create ERR CAP allot

\ The child reopens the native build window and hands the checker its own
\ prefix, which the sealed product refuses: `hb: internal engine word:
\ DECLARATIONS`, exit 70. So it runs on the engine test/whitebox-child.f names.
: PREPARE ( -- )
   CLEANUP-RESET
   s" native-checker-prefix" WHITEBOX-CHILD:PROVIDE ;

: CHECK ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/compiler/aot-mode.f" >LEN PROC-ARGV+
   s" test/native-window-owner-child.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" test/compiler/native-checker-storage.f" >LEN PROC-ARGV+
   s" test/compiler/native-prefix-rollback.f" >LEN PROC-ARGV+
   WHITEBOX-CHILD:ENV!
   WHITEBOX-CHILD:ENGINE$ >LEN
   OUT CAP >LEN ERR CAP >LEN 180000 >MS RUN-ARGV-ENV-CAPTURE
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
   ;MATCH ;

: RUN ( -- )
   T-RESET
   [: PREPARE CHECK ;] [: CLEANUP-RUN ;] finally
   T-REPORT ;
public
RUN
;package

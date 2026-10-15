\ A real OS allocation refusal after successful growth to the inline boundary.
\ The child denies every further mapping (Linux: RLIMIT_AS set to zero); the
\ allocation under test is the engine's.
require src/lib/test.f
require src/lib/process-argv.f
require src/lib/engine-candidate.f
package ADDRESS-CELL-STORAGE-OOM
create OUT 1024 allot
create ERR 4096 allot
: RUN ( -- )
   T-RESET
   HB-TARGET-LINUX? 0= if
      s" address-cell-storage-oom: the address-space limit applies to Linux" type cr
   else
      PROC-ARGV-RESET
      s" --load" >LEN PROC-ARGV+
      s" test/address-cell-oom-child.f" >LEN PROC-ARGV+
      ENGINE-CANDIDATE:PATH$ >LEN OUT 1024 >LEN ERR 4096 >LEN 60000 >MS
      RUN-ARGV-CAPTURE
      MATCH result
         ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
            OUT outu LEN>N type ERR erru LEN>N type
            s" the child dies at the denied mapping" T-LABEL
            0 96 T=
         ENDOF
         err OF PCAP-FAILED:UNMAKE {: outu:len erru:len rc:rc :}
            s" the child dies at the denied mapping" T-LABEL
            rc RC>N 96 T=
            s" duplicate marks at capacity allocate nothing" T-LABEL
            OUT outu LEN>N S\" address-cell-oom: ready\naddress-cell-oom: duplicate-ok\n" T$=
            s" the engine names the refused storage allocation" T-LABEL
            ERR erru LEN>N S\" hb: address-cell storage allocation failed\n" T$=
         ENDOF
      ;MATCH
   then
   T-REPORT ;
RUN
;package

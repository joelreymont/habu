\ A real OS allocation refusal, after successful growth to the inline boundary.
\ Python supplies only Linux prlimit; the allocation under test is the engine's.
require lib/test.f
require lib/engine-candidate.f
package ADDRESS-CELL-STORAGE-OOM
create OUT 1024 allot
create ERR 4096 allot
: RUN ( -- )
   T-RESET
   HB-TARGET-LINUX? if
      PROC-ARGV-RESET
      s" test/address-cell-storage-oom.py" >LEN PROC-ARGV+
      ENGINE-CANDIDATE:PATH$ >LEN PROC-ARGV+
      s" /usr/bin/python3" >LEN OUT 1024 >LEN ERR 4096 >LEN 60000 >MS
      RUN-ARGV-CAPTURE
      MATCH result
         ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
            erru LEN>N 0 T=
            OUT outu LEN>N S\" address-cell-oom: ok\n" T$=
         ENDOF
         err OF PCAP-FAILED:UNMAKE {: outu:len erru:len rc:rc :}
            OUT outu LEN>N type ERR erru LEN>N type
            rc RC>N 0 T=
         ENDOF
      ;MATCH
   then
   T-REPORT ;
RUN
;package

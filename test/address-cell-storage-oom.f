\ A real OS refusal to grow the address-cell table runs in a child: its zero
\ address-space limit is process-wide and its uncaught throw ends it.
require src/lib/string.f
require src/lib/test.f
require src/lib/process-argv.f
require src/lib/engine-candidate.f
package ADDRESS-CELL-STORAGE-OOM
create OUT 1024 allot
create ERR 4096 allot
: RUN ( -- )
   T-RESET
   HB-TARGET-LINUX-KERNEL? 0= if
      s" address-cell-storage-oom: the address-space limit applies to Linux" type cr
   else
      PROC-ARGV-RESET
      s" --load" >LEN PROC-ARGV+
      s" test/address-cell-oom-child.f" >LEN PROC-ARGV+
      ENGINE-CANDIDATE:PATH$ >LEN OUT 1024 >LEN ERR 4096 >LEN PROCESS:WAIT-FOREVER
      RUN-ARGV-CAPTURE
      MATCH result
         ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
            OUT outu LEN>N type ERR erru LEN>N type
            s" the uncaught throw ends the child" T-LABEL
            0 67 T=
         ENDOF
         err OF PCAP-FAILED:UNMAKE {: outu:len erru:len rc:rc :}
            s" a duplicate allocates nothing; a caught grow leaves the table usable" T-LABEL
            OUT outu LEN>N
            S\" address-cell-oom: ready\naddress-cell-oom: duplicate-ok\naddress-cell-oom: caught -9855\naddress-cell-oom: after-catch-ok\n" T$=
            s" the uncaught throw prints the E-XTCELL-GROW record" T-LABEL
            ERR erru LEN>N s" error[E-XTCELL-GROW]" STARTS-WITH? TTRUE
            s" the uncaught throw ends the child" T-LABEL
            rc RC>N 67 T=
         ENDOF
      ;MATCH
   then
   T-REPORT ;
RUN
;package

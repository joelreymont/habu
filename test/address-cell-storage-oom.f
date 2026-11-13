\ A real OS allocation refusal after successful growth to the inline boundary.
\ The child denies every further mapping (Linux: RLIMIT_AS set to zero); the
\ allocation under test is the engine's.
require src/lib/string.f
require src/lib/test.f
require src/lib/process-argv.f
require src/lib/engine-candidate.f
package ADDRESS-CELL-STORAGE-OOM
create OUT 1024 allot
create ERR 4096 allot
\ The exit's whole line up to the lr's 16 hex digits and its newline.
: LINE$ ( -- ptr u8 n ) s" E-XTCELL habu: address-cell storage allocation failed lr " ;
: RUN ( -- )
   T-RESET
   HB-TARGET-LINUX? 0= if
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
            s" the child dies at the denied mapping" T-LABEL
            0 96 T=
         ENDOF
         err OF PCAP-FAILED:UNMAKE {: outu:len erru:len rc:rc :}
            s" the child dies at the denied mapping" T-LABEL
            rc RC>N 96 T=
            s" duplicate marks at capacity allocate nothing" T-LABEL
            OUT outu LEN>N S\" address-cell-oom: ready\naddress-cell-oom: duplicate-ok\n" T$=
            s" the engine names the refused storage allocation" T-LABEL
            erru LEN>N {: errn:n :}
            errn LINE$ nip 17 + T=
            ERR errn LINE$ STARTS-WITH? TTRUE
            true errn 1- LINE$ nip ?do ERR i + c@ STR-HEX-DIGIT? and loop TTRUE
            ERR errn + 1- c@ 10 T=
         ENDOF
      ;MATCH
   then
   T-REPORT ;
RUN
;package

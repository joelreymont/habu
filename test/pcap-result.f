\ pcap-result.f - a process capture result read as numbers for a test: the
\ stdout and stderr byte counts and the exit code, 0 for a clean exit. A test
\ that keeps the counts stores the three cells it answers:
\    … RUN-ARGV-CAPTURE PCAP-RESULT:>N RC ! ERR-U ! OUT-U !

require src/lib/process.f

package PCAP-RESULT
public

: >N ( result<pcap:captured,pcap:failed> -- n n n )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N erru LEN>N 0 ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len rc:rc :}
         outu LEN>N erru LEN>N rc RC>N ENDOF
   ;MATCH ;

;package

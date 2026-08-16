\ host-run-lib.f - run one fixture on the capture host and relay its verdict.
\
\ THE PARTITION RULE this file carries (dot habu-seeded-words-invisible-
\ c7505a49): a fixture whose SUBJECT is source-loading the chain - a capture,
\ a load-time delta, a family-count bracket - cannot run on the product,
\ because the product already provides every chain file and the load the
\ fixture exists to measure becomes a no-op. Such a fixture runs on the
\ CAPTURE HOST the install keeps beside the product, obtained BY PATH and
\ never rebuilt (tools/build-fixpoint.f BF-INSTALL-HOST). A fixture whose
\ subject is chain BEHAVIOR keeps running on the product directly.
\
\ Each driver states its fixture's subject in its own header and hands the
\ path here. The child's verdict is the verdict: rc 0 passes, anything else
\ fails the driver's one assert with the child's stderr and stdout relayed,
\ so the schedule sees exactly the red the host saw.

require lib/errors.f
require lib/string.f
require lib/process.f
require lib/process-env.f
require lib/test.f

package HOST-RUN

$20000 constant IO-CAP
600000 constant CHILD-TIMEOUT-MS

create OUT IO-CAP allot   variable OUT-U
create ERR IO-CAP allot   variable ERR-U
variable RC

: HOST$ ( -- ptr u8 n ) s" bin/hb-host" ;

: CAPTURE! ( result<pcap:captured,pcap:failed> -- )
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  0 RC ! ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  c RC>N RC ! ENDOF
   ;MATCH ;

: RELAY ( -- )
   ERR ERR-U @ type
   OUT OUT-U @ type ;

public

: HOST-RUN ( ptr u8 n -- ) {: p:ptr pu:n :}
   PROC-ARGV-RESET
   PROC-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load" >LEN PROC-ARGV+
   p pu >LEN PROC-ARGV+
   HOST$ >LEN  OUT IO-CAP >LEN  ERR IO-CAP >LEN  CHILD-TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE CAPTURE!
   p pu T-LABEL
   RC @ 0 <> if RELAY then
   RC @ 0 T=
   T-REPORT
   s" host-run: ok" type cr ;

;package

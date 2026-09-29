\ aot-chain-capture-lib.f - the fixture the AOT chain capture gate rows share.
\ Loaded by test/aot-chain-capture-suite.f, test/aot-chain-producer-suite.f and,
\ inside the producer's private host, test/aot-chain-row-checks.f: the scratch
\ tree, the child runner and its captured output, the output assertions and the
\ row driver. It runs nothing; each file reopens package AOT-CHAIN-SUITE and
\ runs the probes it owns.

require lib/string.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f

package AOT-CHAIN-SUITE

$8000 constant CAP
60000 constant CHILD-TIMEOUT-MS

\ tools/aot-chain-capture.f's refusal code and the sentence the product must die
\ with, which is a different exit from every undefined-word death.
$4A constant REFUSE-RC

create OUT CAP allot     variable OUT-U
create ERR CAP allot     variable ERR-U
create EMPTY 1 allot                          \ zero-length stdin
variable RC

create ROOT-BUF FS-PATH-CAP allot   variable ROOT-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: OUT$ ( -- ptr u8 n ) OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;

\ One tree per run, registered for cleanup, so "the artifact exists" is a statement
\ about the capture that just ran and never about a leftover.
: SETUP ( -- )
   s" habu-aot-chain" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY  u ROOT-U !
   ROOT$ CLEANUP-TREE+ ;

: RUN-ENGINE ( ptr u8 n -- )
   >LEN  EMPTY 0 >LEN  OUT CAP >LEN  ERR CAP >LEN  CHILD-TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  0 RC ! ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  c RC>N RC ! ENDOF
   ;MATCH ;

: SAID? ( ptr u8 n -- ) {: m:ptr mu:n :}
   m mu T-LABEL
   OUT$ m mu CONTAINS? TTRUE ;

: ERR-SAID? ( ptr u8 n -- ) {: m:ptr mu:n :}
   m mu T-LABEL
   ERR$ m mu CONTAINS? TTRUE ;

: ROW-RC ( n -- )
   {: want:n :}
   RC @ want <> if
      s" artifact-row child stdout:" type cr OUT$ type cr
      s" artifact-row child stderr:" type cr ERR$ type cr
   then
   RC @ want T= ;

\ A row's probes, with every tree SETUP registered removed whether they pass or
\ throw.
: RUN-PROBES ( [ -- ] -- )
   T-RESET
   CLEANUP-RESET
   catch {: code:n :}
   CLEANUP-RUN
   code 0 <> if code throw then
   T-REPORT ;

;package

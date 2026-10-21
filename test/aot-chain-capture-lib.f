\ aot-chain-capture-lib.f - the fixture the AOT chain capture gate rows share.
\ Loaded by test/aot-chain-capture-suite.f and test/aot-chain-storage-suite.f:
\ the scratch tree, the child runner and its captured output, the output
\ assertions and the row driver. It runs nothing; the suite reopens package AOT-CHAIN-SUITE and
\ runs the probes it owns.

require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require test/pcap-result.f

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
   PCAP-RESULT:>N RC ! ERR-U ! OUT-U ! ;

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

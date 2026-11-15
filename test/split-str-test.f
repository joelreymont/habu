\ split-str-test.f - the split-str marker: a file that carries it reads each
\ str in the rows its tokens are checked against as ^u8 n; the marker ends with
\ its file, and the verifier reads it as the load does.
\     bin/hb < test/split-str-test.f

require src/habu/verify-source.f
require src/lib/test.f

T-RESET

\ Silences the expected rejection diagnostics.
package SPLIT-DIAG
create BUF 65536 allot
BUF 65536 REJECTION:BUFFER!
;package

\ Unmarked: str is one value.
: SPL ( str -- n ) str> nip ;
: SPQ ( str [ str -- n ] -- n ) execute ;

include test/split-str-marked.f

\ The marker ended with its file: str is one value again.
s" : SPX ( ^u8 n -- n ) SPL ;" TEST-EVAL:RC 70 T=
s" abc" >str SPL 3 T=

\ split-str anywhere but a file's first line is refused.
s" split-str" TEST-EVAL:RC 75 T=

\ The verifier reads the marker as the load does.
: SPW-ON ( -- )
   s\" split-str\n: SPW ( ^u8 n -- n ) SPL ;" VERIFY:SOURCE-BUF ;
: SPW-OFF ( -- )
   s" : SPW ( ^u8 n -- n ) SPL ;" VERIFY:SOURCE-BUF ;
' SPW-ON catch 0 T=
CHECKER-SPLIT-STR @ 0 T=
' SPW-OFF catch 70 T=

\ A file the verifier scans ends its marker with it:
\ VERIFY:SOURCE-COMPOSE-IN-SCOPE scans the subject and the file it includes,
\ each through COMPOSE-FILE-SCAN.
: SPZ-RUN ( -- )
   s\" include test/split-str-nested.f\n: SPZ ( ^u8 n -- n ) SPL ;\n"
   s" test/spz-subject.f" VERIFY:SOURCE-COMPOSE-IN-SCOPE ;
' SPZ-RUN catch 70 T=

T-REPORT

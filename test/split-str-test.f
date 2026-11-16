\ split-str-test.f - the split-str marker: a file that carries it reads each
\ str in the rows its tokens are checked against as ^u8 n; the marker ends with
\ its file, and the verifier reads it as the load does.
\     bin/hb < test/split-str-test.f

require src/habu/verify-source.f
require src/lib/test.f
require src/lib/process-argv.f
require src/lib/engine-candidate.f
require src/lib/fs-mutate.f
require test/e2e-tree.f

T-RESET

\ Silences the expected rejection diagnostics.
REJECTION:CAPTURE

\ Unmarked: str is one value.
: SPL ( str -- n ) str> nip ;
: SPQ ( str [ str -- n ] -- n ) execute ;

include test/split-str-marked.f

\ The marker ended with its file: str is one value again.
s" : SPX ( ^u8 n -- n ) SPL ;" TEST-EVAL:RC 70 T=
s" abc" >str SPL 3 T=

\ Its own definitions are published as written.
s" abc" >str SPB 3 T=
s" abcd" >str SPR 4 T=
s" ab" SPO str> nip 2 T=

\ split-str anywhere but a file's first line is rejected at its place.
REJECTION:CAPTURE
s" split-str" TEST-EVAL:RC 70 T=
REJECTION:BUFFER$ s" error[E-SPLIT-STR-LINE]: `split-str` is not allowed in this context" CONTAINS? TTRUE
REJECTION:BUFFER$ s" split-str-test.f" 15 s\" s\" split-str\" TEST-EVAL:RC" T-PLACED
REJECTION:BUFFER$ s\"   = evaluated: split-str\n" CONTAINS? TTRUE
REJECTION:BUFFER$ s" = help: write `split-str` in lower case, alone on its file's first line, or delete it" CONTAINS? TTRUE

\ tools/check.f's verifier places it at split-str with its help, as the load does.
: SPC-RUN ( -- )
   s" split-str-check" E2E-TREE:TREE
   E2E-TREE:ROOT$ FS:CLEANUP-TREE+
   s" test/split-str-late.f" E2E-TREE:AT-A s\" : SPC ( -- ) ;\nsplit-str\n" FS:WRITE-ALL
   s" tools/check.f" E2E-TREE:ARGS
   s" --" >LEN PROC-ARGV+
   s" test/split-str-late.f" >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ E2E-TREE:ROOT$ E2E-TREE:RUN-ON ;
SPC-RUN
E2E-TREE:RC@ 70 T=
E2E-TREE:ERR$ s" error[E-SPLIT-STR-LINE]: `split-str` is not allowed in this context" CONTAINS? TTRUE
E2E-TREE:ERR$ s" split-str-late.f" 1 s" split-str" T-PLACED
E2E-TREE:ERR$ s" ^^^^^^^^^ its file's first line is not `split-str` alone" CONTAINS? TTRUE
E2E-TREE:ERR$ s" = help: write `split-str` in lower case, alone on its file's first line, or delete it" CONTAINS? TTRUE

\ The verifier reads the marker as the load does.
: SPW-ON ( -- )
   s\" split-str\n: SPW ( ^u8 n -- n ) SPL ;" VERIFY:SOURCE-BUF ;
: SPW-OFF ( -- )
   s" : SPW ( ^u8 n -- n ) SPL ;" VERIFY:SOURCE-BUF ;
' SPW-ON catch 0 T=
' SPW-OFF catch 70 T=
\ The verifier walks a marked definition's body on the split rows.
: SPW2-RUN ( -- )
   s\" split-str\n: SPW2 ( str -- n ) nip ;" VERIFY:SOURCE-BUF ;
' SPW2-RUN catch 0 T=

\ A file the verifier scans ends its marker with it: a composition scans the
\ subject and the file it includes.
: SPZ-RUN ( -- )
   s\" include test/split-str-nested.f\n: SPZ ( ^u8 n -- n ) SPL ;\n"
   s" test/spz-subject.f" 2dup VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;
' SPZ-RUN catch 70 T=

T-REPORT

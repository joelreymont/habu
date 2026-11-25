\ split-str-test.f - the split-str marker: a file that carries it reads each
\ str in the rows its tokens are checked against as ^u8 n; the marker ends with
\ its file, and the verifier reads it as the load does.
\     bin/hb --load test/run.f -- split-str </dev/null

require src/habu/verify-source.f
require src/lib/test/assert.f
require src/lib/test/eval.f
require src/lib/process-argv.f
require src/lib/engine-candidate.f
require src/lib/fs-mutate.f
require test/e2e-tree.f
require src/lib/string.f
require src/lib/test/subject.f
require src/lib/test/ending.f
require src/lib/fs.f
require src/lib/process.f

T-RESET

\ Silences the expected rejection diagnostics.
REJECTION:CAPTURE

\ Unmarked: str is one value.
: SPL ( str -- n ) str> nip ;
: SPQ ( str [ str -- n ] -- n ) execute ;

include test/split-str-marked.f

\ The marker ended with its file: str is one value again.
s" : SPX ( ^u8 n -- n ) SPL ;" TEST-EVAL:RC 70 T=
s" abc" SPL 3 T=
s" : SPSX ( ^u8 n -- n ) SPSV @ execute ;" TEST-EVAL:RC 70 T=
s" abcd" SPSV @ execute 4 T=
s" : SPSQX ( ^u8 n -- n ) 0 SPSQB SPSQ:Q @ execute ;" TEST-EVAL:RC 70 T=
s" abcd" 0 SPSQB SPSQ:Q @ execute 4 T=
: SPJ-W ( spj -- n )
   MATCH spj text OF str> nip ENDOF none OF 0 ENDOF ;MATCH ;
s" ab" SPJ:TEXT SPJ-W 2 T=

\ Its own definitions are published as written.
s" abc" SPB 3 T=
s" abcd" SPR 4 T=
s" ab" str> SPO str> nip 2 T=

\ Unmarked, a literal is one str: a str row takes it, and `>str` after it is
\ rejected. split-str-marked.f hands one to `^u8 n` rows.
s"  a" STR:LTRIM str> nip 1 T=
REJECTION:CAPTURE
S\" : SPLX ( -- ) s\q a\q >str drop ;" TEST-EVAL:RC 70 T=
REJECTION:BUFFER$ s" error[E-MISMATCH]" STR:CONTAINS? TTRUE

\ split-str anywhere but a file's first line is rejected at its place.
REJECTION:CAPTURE
s" split-str" TEST-EVAL:RC 70 T=
REJECTION:BUFFER$ s" error[E-SPLIT-STR-LINE]: `split-str` is not allowed in this context" STR:CONTAINS? TTRUE
REJECTION:BUFFER$ s" split-str-test.f" 15 s\" s\" split-str\" TEST-EVAL:RC" T-PLACED
REJECTION:BUFFER$ s\"   = evaluated: split-str\n" STR:CONTAINS? TTRUE
REJECTION:BUFFER$ s" = help: move `split-str`, in lower case, alone to the first line of a file loaded by name, or delete it" STR:CONTAINS? TTRUE

\ tools/check.f's verifier places it at split-str with its help, as the load does.
: SPC-RUN ( -- )
   s" split-str-check" E2E-TREE:TREE
   E2E-TREE:ROOT$ FS:CLEANUP-TREE+
   s" test/split-str-late.f" E2E-TREE:AT-A s\" : SPC ( -- ) ;\nsplit-str\n" FS:WRITE-ALL
   s" tools/check.f" E2E-TREE:ARGS
   s" --" PROC-ARGV+
   s" test/split-str-late.f" PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ E2E-TREE:ROOT$ E2E-TREE:RUN-ON ;
SPC-RUN
E2E-TREE:RC@ 70 T=
E2E-TREE:ERR$ s" error[E-SPLIT-STR-LINE]: `split-str` is not allowed in this context" STR:CONTAINS? TTRUE
E2E-TREE:ERR$ s" split-str-late.f" 1 s" split-str" T-PLACED
E2E-TREE:ERR$ s" ^^^^^^^^^ it takes effect only alone on the first line of a file loaded by name" STR:CONTAINS? TTRUE
E2E-TREE:ERR$ s" = help: move `split-str`, in lower case, alone to the first line of a file loaded by name, or delete it" STR:CONTAINS? TTRUE

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
   s" test/spz-subject.f" {: path :}
   s\" include test/split-str-nested.f\n: SPZ ( ^u8 n -- n ) SPL ;\n"
   path path VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;
' SPZ-RUN catch 70 T=

include test/split-str-deep.f

T-REPORT

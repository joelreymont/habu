split-str
\ split-str-marked.f - included by test/split-str-test.f: from the marker on,
\ the str rows of SPL and SPQ read as ^u8 n.

\ A call at top level.
s" abc" SPL 3 T=

\ A call in a definition.
: SPM ( ^u8 n -- n ) SPL ;
s" abcd" SPM 4 T=

\ A tick.
: SPT ( ^u8 n -- n ) ['] SPL execute ;

\ A quotation row nested in a called row.
: SPV ( ^u8 n -- n ) [: nip ;] SPQ ;

\ A file it includes starts unmarked, and its marker holds after it.
include test/split-str-plain.f
: SPP ( ^u8 n -- n ) SPL ;
s" ab" SPP 2 T=

\ A string the file evaluates reads with its marker.
s" : SPE ( ^u8 n -- n ) SPL ;" evaluate
s" abc" SPE 3 T=

\ A source the verifier scans starts unmarked and leaves the file's marker set.
: SPY-RUN ( -- ) s" : SPY ( -- n ) 0 ;" VERIFY:SOURCE-BUF ;
' SPY-RUN catch 0 T=
CHECKER-SPLIT-STR @ 0<> TTRUE

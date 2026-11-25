split-str
\ split-str-marked.f - included by test/split-str-test.f: from the marker on,
\ the str rows of SPL and SPQ read as ^u8 n.

require src/lib/test/assert.f
require src/lib/test/eval.f

\ A call at top level.
s" abc" SPL 3 T=

\ A call in a definition.
: SPM ( ^u8 n -- n ) SPL ;
s" abcd" SPM 4 T=

\ A tick.
: SPT ( ^u8 n -- n ) ['] SPL execute ;

\ A quotation row nested in a called row.
: SPV ( ^u8 n -- n ) [: nip ;] SPQ ;

\ A quotation row behind a pointer or in a family's argument.
TYPED-VARIABLE SPSV [ str -- n ]
: SPS ( -- ) [: SPL ;] SPSV ! ;
SPS
: SPSF ( ^u8 n -- n ) SPSV @ execute ;
s" abc" SPSF 3 T=
: SPSP ( ^u8 n ^[ str -- n ] -- n ) @ execute ;
STRUCTURE spsh<a> FIELD v a ;STRUCTURE
: SPSHF ( ^u8 n spsh<[ str -- n ]> -- n ) SPSH:UNMAKE execute ;
\ A quotation row a structure's field accessor hands back.
STRUCTURE spsq DERIVE addr FIELD q [ str -- n ] ;STRUCTURE
1 LAYOUT-BUFFER SPSQB spsq
: SPSQS ( -- ) [: SPL ;] 0 SPSQB SPSQ:Q ! ;
SPSQS
: SPSQF ( ^u8 n -- n ) 0 SPSQB SPSQ:Q @ execute ;
s" abc" SPSQF 3 T=

\ A str stored in a typed variable and fetched back, then passed on.
TYPED-VARIABLE SPST str
: SPSTS ( -- ) s" abc" SPST ! ;
SPSTS
: SPSTF ( -- ^u8 n n ) SPST @ 2dup SPL ;
SPSTF 3 T= s" abc" T$=

\ A MATCH arm takes a str payload split.
SUMTYPE spj VARIANT text str ;VARIANT VARIANT none ;VARIANT ;SUMTYPE
: SPJ-OF ( spj -- n u8 )
   MATCH spj text OF swap c@ ENDOF none OF 0 0 ENDOF ;MATCH ;
: SPJ-RUN ( -- n u8 ) s" abc" SPJ:TEXT SPJ-OF ;
SPJ-RUN char a T= 3 T=

\ A file it includes starts unmarked, and its marker holds after it.
include test/split-str-plain.f
: SPP ( ^u8 n -- n ) SPL ;
s" ab" SPP 2 T=

\ A string the file evaluates reads with its marker.
s" : SPE ( ^u8 n -- n ) SPL ;" evaluate
s" abc" SPE 3 T=

\ A source the verifier scans leaves the file's marker set.
: SPY-RUN ( -- ) s" : SPY ( -- n ) 0 ;" VERIFY:SOURCE-BUF ;
' SPY-RUN catch 0 T=

\ The file's own definitions are published as written, str, and their bodies
\ walk the split rows.
: SPB ( str -- n ) nip ;
: SPR ( str -- n ) dup 0= IF nip EXIT THEN 1 - swap 1 + swap RECURSE 1 + ;
: SPO ( ^u8 n -- str ) ;

\ A cast keeps its written rows: split, its rows would differ in width.
package SPC
public
STRUCTURE spp OPAQUE FIELD v str ;STRUCTURE
private
s" cast: >SPP ( str -- spp )" TEST-EVAL:RC 0 T=
;package

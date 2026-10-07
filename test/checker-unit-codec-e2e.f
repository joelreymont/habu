\ A package's checker facts survive an artifact boundary. The dictionary stays
\ resident while CHECKER-RESET-SOURCE makes the importing checker source-fresh.
\ Run on a build containing the unit owner callbacks:
\ bin/hb --load test/checker-unit-codec-e2e.f

require lib/test.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f

package CHECKER-UNIT-CODEC-TEST

$40000 constant ART-CAP
7159 constant E-UNIT-FORMAT   \ src/core/checker.f CHECKER-REG's, private there
7160 constant E-UNIT-STATE
create ART ART-CAP allot
create BAD ART-CAP allot
variable ART-U
variable ART-I
variable MARK-SERIAL
variable BASE-SERIAL
PTR-VARIABLE SAVED-OWNER
variable SAVED-SERIAL
variable SAVED-REGIME
create ROOT FS-PATH-CAP allot
variable ROOT-U
create PATH FS-PATH-CAP allot

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: OWNER ( -- ptr u8 ) data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ ;

\ The checker owner's record holds its unit callbacks as code address integers.
CAST: MARK-XT ( n -- [ -- ] )
CAST: EXPORT-XT ( n -- [ -- ptr u8 n ] )
CAST: IMPORT-XT ( n -- [ ptr u8 n -- ] )

: MARK ( -- )
   OWNER CHECKER-OWNER-ABI:UNIT-MARK-OFF + CELL-VIEW @ MARK-XT execute ;

: UNIT-EXPORT ( -- ptr u8 n )
   OWNER CHECKER-OWNER-ABI:UNIT-EXPORT-OFF + CELL-VIEW @ EXPORT-XT execute ;

: IMPORT ( ptr u8 n -- )
   OWNER CHECKER-OWNER-ABI:UNIT-IMPORT-OFF + CELL-VIEW @ IMPORT-XT execute ;

TRUSTED: RESET-SOURCE ( -- ) CHECKER-RESET-SOURCE ;
: LOAD-UNIT ( -- )
   s" package UNIT-NBR public : BL-ALPHA ( -- n ) 1 ; : BL-OMEGA ( -- n ) 2 ; : BL-WORD ( n n -- n ) + ; : BL-TARGET ( n n -- n ) + ; : BL? ( n -- bool ) 0= ; : BL-SCHEME ( forall<p,[ R n -- R n | U -- U ]> -- ) drop ; : BL-INFER BL-SCHEME ; ;package" evaluate-closed ;
: SHADOW ( -- )
   s" package UNIT-NBR public : UNIT-SHADOW ( n -- n ) 1+ ; undefine UNIT-SHADOW : UNIT-SHADOW ( n n -- n ) + ; ;package" evaluate-closed ;
: CLIENT ( -- n )
   s" 4096 8 UNIT-NBR:BL-WORD" TEST-EVAL:N ;
: DEFER-CHANGE ( -- )
   s" package UNIT-NBR public defer UNIT-DEFER ( -- n ) ;package" evaluate-closed ;

: SERIAL ( -- n )
   CHECKER-OWNER:BINDING-WINDOW {: owner:ptr serial:n regime:bool :}
   serial ;

: SAVE-WINDOW ( -- )
   CHECKER-OWNER:BINDING-WINDOW
   SAVED-REGIME ! SAVED-SERIAL ! SAVED-OWNER ! ;

: CHECK-STALE ( -- )
   [: SAVED-OWNER @ SAVED-SERIAL @ SAVED-REGIME @
      CHECKER-OWNER:BINDING-WINDOW-CK ;]
   CHECKER-OWNER-ABI:BINDING-RC TTHROWSQ ;

: SAVE ( -- )
   UNIT-EXPORT {: a:ptr u:n :}
   u ART-CAP <= TTRUE
   a ART u BYTE-COPY
   u ART-U !
   ART 4 cells + CELL-VIEW @ 10 T=
   ART 6 cells + CELL-VIEW @ SERIAL MARK-SERIAL @ - T=
   s" checker-unit-codec-e2e" HB-TMP-MKDIR {: root:ptr len:n :}
   root ROOT len BYTE-COPY len ROOT-U !
   ROOT$ s" unit-nbr.checker-unit" PATH JOIN-PATH
   PATH swap ART ART-U @ WRITE-ALL ;

: CHECK-CLIENT ( -- )
   CLIENT 4104 T=
   s" UNIT-GOOD ( n n -- n ) UNIT-NBR:UNIT-SHADOW" CHECK-CANDIDATE! -1 T=
   s" UNIT-SHADOW-OLD ( n -- n ) UNIT-NBR:UNIT-SHADOW" CHECK-CANDIDATE! 0 T=
   s" UNIT-BAD ( ptr u8 -- bool ) UNIT-NBR:BL?" CHECK-CANDIDATE! 0 T=
   s" UNIT-SCHEME ( forall<q,[ R n -- R n | U -- U ]> -- ) UNIT-NBR:BL-SCHEME" CHECK-CANDIDATE! -1 T=
   s" UNIT-INFER ( forall<s,[ R n -- R n | U -- U ]> -- ) UNIT-NBR:BL-INFER" CHECK-CANDIDATE! -1 T= ;

\ The wire header has seven cells. Each symbol has a visibility cell and two
\ length-prefixed, cell-aligned strings; an effect begins with three cells.
: STRING-END ( n -- n ) dup ART + CELL-VIEW @ 7 + -8 and + CELL + ;
: NAME-OFF ( n -- n )
   {: idx:n :}
   7 cells ART-I !
   idx 0 ?do
      CELL ART-I +!
      ART-I @ STRING-END ART-I !
      ART-I @ STRING-END ART-I !
   loop
   CELL ART-I +!
   ART-I @ STRING-END ART-I !
   ART-I @ CELL + ;
: FIRST-GRAPH-OFF ( -- n )
   7 cells ART-I !
   ART 3 cells + CELL-VIEW @ 0 ?do
      CELL ART-I +!
      ART-I @ STRING-END ART-I !
      ART-I @ STRING-END ART-I !
   loop
   ART-I @ 3 cells + ;

: FIRST-CONTROL-OFF ( -- n )
   FIRST-GRAPH-OFF 3 cells - ART-I !
   ART 4 cells + CELL-VIEW @ 0 ?do
      ART-I @ 2 cells + ART + CELL-VIEW @ 7 + -8 and
      3 cells + ART-I +!
   loop
   ART-I @ ;

: CHECK-DEFER-GRAPH ( -- )
   ART BAD ART-U @ BYTE-COPY
   \ EW.SYM is graph-header cell nine; $10000 marks defer metadata.
   BAD FIRST-GRAPH-OFF + 9 cells + CELL-VIEW
   dup @ $10000 or swap !
   [: BAD ART-U @ IMPORT ;] catch E-UNIT-FORMAT T= ;

: CHECK-ROOT-GRAPH ( -- )
   ART BAD ART-U @ BYTE-COPY
   \ A private root bit cannot be imported as graph control metadata.
   BAD FIRST-GRAPH-OFF + 9 cells + CELL-VIEW
   dup @ $20000 or swap !
   [: BAD ART-U @ IMPORT ;] catch E-UNIT-FORMAT T= ;

: CHECK-DEFER-CONTROL ( -- )
   ART 5 cells + CELL-VIEW @ 0 > TTRUE
   ART BAD ART-U @ BYTE-COPY
   \ A control row stores its symbol ordinal, then packed flags and masks.
   BAD FIRST-CONTROL-OFF + CELL + CELL-VIEW
   dup @ $10000 or swap !
   [: BAD ART-U @ IMPORT ;] catch E-UNIT-FORMAT T= ;

: CHECK-FAILED-IMPORTS ( -- )
   SERIAL BASE-SERIAL !
   ART BAD ART-U @ BYTE-COPY
   1 BAD CELL + CELL-VIEW !
   [: BAD ART-U @ IMPORT ;] E-UNIT-FORMAT TTHROWSQ
   SERIAL BASE-SERIAL @ T=
   ART BAD ART-U @ BYTE-COPY
   6 cells BAD 2 cells + CELL-VIEW !
   [: BAD 6 cells IMPORT ;] E-UNIT-FORMAT TTHROWSQ
   SERIAL BASE-SERIAL @ T=
   ART BAD ART-U @ BYTE-COPY
   -1 BAD 6 cells + CELL-VIEW !
   [: BAD ART-U @ IMPORT ;] E-UNIT-FORMAT TTHROWSQ
   SERIAL BASE-SERIAL @ T=
   ART BAD ART-U @ BYTE-COPY
   0 NAME-OFF {: first:n :}
   1 NAME-OFF {: second:n :}
   ART first CELL - + CELL-VIEW @
      ART second CELL - + CELL-VIEW @ T=
   ART first + BAD second + ART first CELL - + CELL-VIEW @ BYTE-COPY
   [: BAD ART-U @ IMPORT ;] E-UNIT-FORMAT TTHROWSQ
   SERIAL BASE-SERIAL @ T= ;

: CHECK-BOUNDARIES ( -- )
   RESET-SOURCE MARK
   SERIAL BASE-SERIAL !
   ART BAD ART-U @ BYTE-COPY
   0 BAD 6 cells + CELL-VIEW !
   BAD ART-U @ IMPORT
   SERIAL BASE-SERIAL @ T=
   RESET-SOURCE MARK
   SERIAL BASE-SERIAL !
   ART BAD ART-U @ BYTE-COPY
   $8000000000000000 BAD 6 cells + CELL-VIEW !
   BAD ART-U @ IMPORT
   SERIAL BASE-SERIAL @ $8000000000000000 + T= ;

public

: RUN ( -- )
   T-RESET
   s" ordered package checker facts import into source-fresh checker" T-LABEL
   MARK SERIAL MARK-SERIAL !
   LOAD-UNIT SHADOW
   s" UNIT-EXTRA ( -- n ) UNIT-NBR:BL-ALPHA" CHECK-CANDIDATE! -1 T=
   SAVE
   RESET-SOURCE
   MARK
   s" imported unit refuses defer graph metadata" T-LABEL
   CHECK-DEFER-GRAPH
   s" imported unit refuses root graph metadata" T-LABEL
   CHECK-ROOT-GRAPH
   s" imported unit refuses defer control metadata" T-LABEL
   CHECK-DEFER-CONTROL
   s" failed imports preserve the binding serial" T-LABEL
   CHECK-FAILED-IMPORTS
   SAVE-WINDOW
   SERIAL BASE-SERIAL !
   ART ART-U @ IMPORT
   SERIAL BASE-SERIAL @ ART 6 cells + CELL-VIEW @ + T=
   CHECK-STALE
   s" a consumed mark cannot reserve twice" T-LABEL
   [: ART ART-U @ IMPORT ;] E-UNIT-STATE TTHROWSQ
   SERIAL BASE-SERIAL @ ART 6 cells + CELL-VIEW @ + T=
   CHECK-CLIENT
   s" unit exporter refuses changed defer state" T-LABEL
   MARK DEFER-CHANGE
   [: UNIT-EXPORT 2drop ;] catch 0<> TTRUE
   s" zero and sign-bit deltas retain full cell semantics" T-LABEL
   CHECK-BOUNDARIES
   T-REPORT
   s" checker unit artifact: " type ROOT$ type
   s" /unit-nbr.checker-unit" type cr ;

;package

CHECKER-UNIT-CODEC-TEST:RUN

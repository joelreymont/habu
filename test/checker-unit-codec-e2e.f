\ A package's checker facts survive an artifact boundary. The dictionary stays
\ resident while CHECKER-RESET-SOURCE makes the importing checker source-fresh.
\ Run on a build containing the unit owner callbacks:
\ bin/hb --load test/checker-unit-codec-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require test/checker-owner-reset.f
require src/habu/xref.f

package CHECKER-UNIT-CODEC-TEST

$40000 constant ART-CAP
create ART ART-CAP allot
create BAD ART-CAP allot
variable ART-U
variable ART-I
variable MARK-SERIAL
variable BASE-SERIAL
variable LINEAR-HITS
PTR-VARIABLE SAVED-OWNER
variable SAVED-SERIAL
variable SAVED-REGIME
create ROOT PATH-CAP allot
variable ROOT-U
create PATH PATH-CAP allot

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: OWNER ( -- ptr u8 ) data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ ;

\ The checker owner's record holds its unit callbacks as code address integers.
CAST: MARK-XT ( n -- [ -- ] )
CAST: EXPORT-XT ( n -- [ -- ptr u8 n ] )
CAST: IMPORT-XT ( n -- [ ptr u8 n -- ] )
CAST: PREPARE-BEGIN-XT ( n -- [ -- ] )
CAST: PREPARE-XT ( n -- [ ptr u8 n n n -- n ] )
CAST: INSTALL-XT ( n -- [ n -- ] )
CAST: PREPARE-END-XT ( n -- [ -- ] )
CAST: CONTRACT-XT ( n -- [ n -- ptr u8 n bool ] )

: MARK ( -- )
   OWNER CHECKER-OWNER-ABI:UNIT-MARK-OFF + CELL-VIEW @ MARK-XT execute ;

: UNIT-EXPORT ( -- ptr u8 n )
   OWNER CHECKER-OWNER-ABI:UNIT-EXPORT-OFF + CELL-VIEW @ EXPORT-XT execute ;

: IMPORT ( ptr u8 n -- )
   OWNER CHECKER-OWNER-ABI:UNIT-IMPORT-OFF + CELL-VIEW @ IMPORT-XT execute ;

: PREPARE-BEGIN ( -- )
   OWNER CHECKER-OWNER-ABI:UNIT-PREPARE-BEGIN-OFF + CELL-VIEW @ PREPARE-BEGIN-XT execute ;
: PREPARE ( ptr u8 n n n -- n )
   OWNER CHECKER-OWNER-ABI:UNIT-PREPARE-OFF + CELL-VIEW @ PREPARE-XT execute ;
: INSTALL ( n -- )
   OWNER CHECKER-OWNER-ABI:UNIT-INSTALL-OFF + CELL-VIEW @ INSTALL-XT execute ;
: PREPARE-END ( -- )
   OWNER CHECKER-OWNER-ABI:UNIT-PREPARE-END-OFF + CELL-VIEW @ PREPARE-END-XT execute ;

: CONTRACT ( n -- ptr u8 n bool )
   OWNER CHECKER-OWNER-ABI:RECORD-CONTRACT-OFF + CELL-VIEW @ CONTRACT-XT execute ;

: RECORD-HISTORY ( -- )
   s" package UNIT-HISTORY public : WORD ( n -- n ) 1+ ; ;package" evaluate-closed
   s" UNIT-HISTORY:WORD" XREF-FIND-INDEX {: old:n :}
   old CONTRACT {: first:ptr size:n found:bool :}
   found TTRUE
   size ART-CAP <= TTRUE
   first BAD size BYTE-COPY
   s" package UNIT-HISTORY public undefine WORD : WORD ( n n -- n ) + ; ;package" evaluate-closed
   old CONTRACT {: prior:ptr prioru:n still:bool :}
   still TTRUE prioru size T=
   prior prioru BAD size STR= TTRUE
   s" UNIT-HISTORY:WORD" XREF-FIND-INDEX {: current:n :}
   current old <> TTRUE
   current CONTRACT {: later:ptr lateru:n has:bool :}
   has TTRUE
   later lateru BAD size STR= 0= TTRUE ;

: CREATED-CONTRACT ( -- )
   s" package UNIT-HISTORY public : MAKE ( n -- ) create , does> ( -- n ) @ ; 19 MAKE MADE ;package"
      evaluate-closed
   s" UNIT-HISTORY:MADE" XREF-FIND-INDEX CONTRACT
      {: bytes:ptr size:n found:bool :}
   found TTRUE
   size CHECKER-OWNER-ABI:RECORD-CONTRACT-MIN-BYTES >= TTRUE
   bytes CHECKER-OWNER-ABI:RECORD-FIXED-KIND-OFF + CELL-VIEW @ 0 T= ;

: FIXED-HISTORY ( -- )
   s" package UNIT-HISTORY public 41 constant NUM ;package" evaluate-closed
   s" UNIT-HISTORY:NUM" XREF-FIND-INDEX {: old:n :}
   old CONTRACT {: first:ptr size:n found:bool :}
   found TTRUE
   size CHECKER-OWNER-ABI:RECORD-CONTRACT-MIN-BYTES >= TTRUE
   first CHECKER-OWNER-ABI:RECORD-FIXED-KIND-OFF + CELL-VIEW @
      CHECKER-OWNER-ABI:RECORD-FIXED-VAL T=
   first CHECKER-OWNER-ABI:RECORD-FIXED-VALUE-OFF + CELL-VIEW @ 41 T=
   s" package UNIT-HISTORY public undefine NUM 77 constant NUM ;package" evaluate-closed
   old CONTRACT {: prior:ptr prioru:n still:bool :}
   still TTRUE
   prioru CHECKER-OWNER-ABI:RECORD-CONTRACT-MIN-BYTES >= TTRUE
   prior CHECKER-OWNER-ABI:RECORD-FIXED-VALUE-OFF + CELL-VIEW @ 41 T=
   s" UNIT-HISTORY:NUM" XREF-FIND-INDEX CONTRACT
      {: later:ptr lateru:n has:bool :}
   has TTRUE
   lateru CHECKER-OWNER-ABI:RECORD-CONTRACT-MIN-BYTES >= TTRUE
   later CHECKER-OWNER-ABI:RECORD-FIXED-VALUE-OFF + CELL-VIEW @ 77 T= ;

: INFERRED-CONTRACT ( -- )
   s" UNIT-NBR:USE-INF" XREF-FIND-INDEX CONTRACT rot drop
      {: size:n found:bool :}
   found TTRUE
   size CHECKER-OWNER-ABI:RECORD-CONTRACT-MIN-BYTES >= TTRUE ;

: LOAD-UNIT ( -- )
   s" package UNIT-NBR public : BL-ALPHA ( -- n ) 1 ; : BL-OMEGA ( -- n ) 2 ; : BL-WORD ( n n -- n ) + ; : BL-TARGET ( n n -- n ) + ; : BL? ( n -- bool ) 0= ; : BL-SCHEME ( forall<p,[ R n -- R n | U -- U ]> -- ) drop ; : BL-INFER BL-SCHEME ; ;package" evaluate-closed
   s" package UNIT-NBR public : SAME-ID ( x x -- ) 2drop ; defer RG-ONE ( -- fresh-region-a ) defer RG-PAIR ( -- fresh-region-a fresh-region-a ) defer RG-DIFF ( -- fresh-region-a fresh-region-b ) : USE-INF RG-ONE ; : USE-PAIR RG-PAIR ; ;package" evaluate-closed
   s" package UNIT-NBR public defer EX-ONE ( -- fresh-extent-a ) defer EX-PAIR ( -- fresh-extent-a fresh-extent-a ) ;package" evaluate-closed
   s" package UNIT-NBR public defer GN-ONE ( -- fresh-gen-a ) defer GN-PAIR ( -- fresh-gen-a fresh-gen-a ) ;package" evaluate-closed
   s" package UNIT-NBR public defer MS-ONE ( -- fresh-mask-a ) defer MS-PAIR ( -- fresh-mask-a fresh-mask-a ) ;package" evaluate-closed ;
: SHADOW ( -- )
   s" package UNIT-NBR public : UNIT-SHADOW ( n -- n ) 1+ ; undefine UNIT-SHADOW : UNIT-SHADOW ( n n -- n ) + ; ;package" evaluate-closed ;
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
   ART 6 cells + CELL-VIEW @ SERIAL MARK-SERIAL @ - T=
   s" checker-unit-codec-e2e" FS:MKTEMP-DIR {: root:ptr len:n :}
   root ROOT len BYTE-COPY len ROOT-U !
   ROOT$ s" unit-nbr.checker-unit" PATH FS:JOIN-PATH
   PATH swap ART ART-U @ FS:WRITE-ALL ;

: CHECK-FRESH ( -- )
   s" FRESH-RG-TWO ( -- ) UNIT-NBR:RG-ONE UNIT-NBR:RG-ONE UNIT-NBR:SAME-ID" CHECK-CANDIDATE! 0 T=
   s" FRESH-RG-PAIR ( -- ) UNIT-NBR:RG-PAIR UNIT-NBR:SAME-ID" CHECK-CANDIDATE! -1 T=
   s" FRESH-RG-DIFF ( -- ) UNIT-NBR:RG-DIFF UNIT-NBR:SAME-ID" CHECK-CANDIDATE! 0 T=
   s" FRESH-INF-TWO ( -- ) UNIT-NBR:USE-INF UNIT-NBR:USE-INF UNIT-NBR:SAME-ID" CHECK-CANDIDATE! 0 T=
   s" FRESH-INF-PAIR ( -- ) UNIT-NBR:USE-PAIR UNIT-NBR:SAME-ID" CHECK-CANDIDATE! -1 T=
   s" FRESH-EX-TWO ( -- ) UNIT-NBR:EX-ONE UNIT-NBR:EX-ONE UNIT-NBR:SAME-ID" CHECK-CANDIDATE! 0 T=
   s" FRESH-EX-PAIR ( -- ) UNIT-NBR:EX-PAIR UNIT-NBR:SAME-ID" CHECK-CANDIDATE! -1 T=
   s" FRESH-GN-TWO ( -- ) UNIT-NBR:GN-ONE UNIT-NBR:GN-ONE UNIT-NBR:SAME-ID" CHECK-CANDIDATE! 0 T=
   s" FRESH-GN-PAIR ( -- ) UNIT-NBR:GN-PAIR UNIT-NBR:SAME-ID" CHECK-CANDIDATE! -1 T=
   s" FRESH-MS-TWO ( -- ) UNIT-NBR:MS-ONE UNIT-NBR:MS-ONE UNIT-NBR:SAME-ID" CHECK-CANDIDATE! 0 T=
   s" FRESH-MS-PAIR ( -- ) UNIT-NBR:MS-PAIR UNIT-NBR:SAME-ID" CHECK-CANDIDATE! -1 T= ;

: CHECK-CLIENT ( -- )
   \ Reset retired the test helper's source effects too. Compile the client
   \ against the imported facts and check its value inside that fresh body.
   S\" : UNIT-CLIENT ( -- ) 4096 8 UNIT-NBR:BL-WORD 4104 <> if s\" checker unit client value\" 1 die then ; UNIT-CLIENT" evaluate-closed
   CHECK-FRESH
   s" UNIT-GOOD ( n n -- n ) UNIT-NBR:UNIT-SHADOW" CHECK-CANDIDATE! -1 T=
   s" UNIT-SHADOW-OLD ( n -- n ) UNIT-NBR:UNIT-SHADOW" CHECK-CANDIDATE! 0 T=
   s" UNIT-BAD ( ptr u8 -- bool ) UNIT-NBR:BL?" CHECK-CANDIDATE! 0 T=
   s" UNIT-SCHEME ( forall<q,[ R n -- R n | U -- U ]> -- ) UNIT-NBR:BL-SCHEME" CHECK-CANDIDATE! -1 T=
   s" UNIT-INFER ( forall<s,[ R n -- R n | U -- U ]> -- ) UNIT-NBR:BL-INFER" CHECK-CANDIDATE! -1 T= ;

\ The wire header has ten cells. Family and constructor payloads precede symbols.
\ Each symbol has a visibility cell and two
\ length-prefixed, cell-aligned strings; an effect begins with seven cells.
: STRING-END ( n -- n ) dup ART + CELL-VIEW @ 7 + -8 and + CELL + ;
: ROWS-START ( -- n )
   10 cells ART 7 cells + CELL-VIEW @ +
   ART 8 cells + CELL-VIEW @ 0 ?do
      STRING-END STRING-END
   loop ;
: NAME-OFF ( n -- n )
   {: idx:n :}
   ROWS-START ART-I !
   idx 0 ?do
      CELL ART-I +!
      ART-I @ STRING-END ART-I !
      ART-I @ STRING-END ART-I !
   loop
   CELL ART-I +!
   ART-I @ STRING-END ART-I !
   ART-I @ CELL + ;
: FIRST-GRAPH-OFF ( -- n )
   ROWS-START ART-I !
   ART 3 cells + CELL-VIEW @ 0 ?do
      CELL ART-I +!
      ART-I @ STRING-END ART-I !
      ART-I @ STRING-END ART-I !
   loop
   ART-I @ 7 cells + ;

: FIRST-CONTROL-OFF ( -- n )
   FIRST-GRAPH-OFF 7 cells - ART-I !
   ART 4 cells + CELL-VIEW @ 0 ?do
      ART-I @ 6 cells + ART + CELL-VIEW @ 7 + -8 and
      7 cells + ART-I +!
   loop
   ART-I @ ;

: CHECK-DEFER-GRAPH ( -- )
   ART BAD ART-U @ BYTE-COPY
   \ EW.SYM is graph-header cell nine; $10000 marks defer metadata.
   BAD FIRST-GRAPH-OFF + 9 cells + CELL-VIEW
   dup @ $10000 or swap !
   [: BAD ART-U @ IMPORT ;] catch CHECKER-REG:E-UNIT-FORMAT T= ;

: CHECK-ROOT-GRAPH ( -- )
   ART BAD ART-U @ BYTE-COPY
   \ A root header bit outside the graph flags (src/core/checker.f
   \ CTL-GRAPH-FLAGS) cannot be imported as graph control metadata.
   BAD FIRST-GRAPH-OFF + 9 cells + CELL-VIEW
   dup @ CTL-GRAPH-FLAGS invert dup negate and or swap !
   [: BAD ART-U @ IMPORT ;] catch CHECKER-REG:E-UNIT-FORMAT T= ;

: CHECK-DEFER-CONTROL ( -- )
   ART 5 cells + CELL-VIEW @ 0 > TTRUE
   ART BAD ART-U @ BYTE-COPY
   \ A control row stores its symbol ordinal, then packed flags and masks.
   BAD FIRST-CONTROL-OFF + CELL + CELL-VIEW
   dup @ $10000 or swap !
   [: BAD ART-U @ IMPORT ;] catch CHECKER-REG:E-UNIT-FORMAT T= ;

: CHECK-FAILED-IMPORTS ( -- )
   SERIAL BASE-SERIAL !
   ART BAD ART-U @ BYTE-COPY
   1 BAD CELL + CELL-VIEW !
   [: BAD ART-U @ IMPORT ;] CHECKER-REG:E-UNIT-FORMAT TTHROWSQ
   SERIAL BASE-SERIAL @ T=
   ART BAD ART-U @ BYTE-COPY
   6 cells BAD 2 cells + CELL-VIEW !
   [: BAD 6 cells IMPORT ;] CHECKER-REG:E-UNIT-FORMAT TTHROWSQ
   SERIAL BASE-SERIAL @ T=
   ART BAD ART-U @ BYTE-COPY
   -1 BAD 6 cells + CELL-VIEW !
   [: BAD ART-U @ IMPORT ;] CHECKER-REG:E-UNIT-FORMAT TTHROWSQ
   SERIAL BASE-SERIAL @ T=
   ART BAD ART-U @ BYTE-COPY
   0 NAME-OFF {: first:n :}
   1 NAME-OFF {: second:n :}
   ART first CELL - + CELL-VIEW @
      ART second CELL - + CELL-VIEW @ T=
   ART first + BAD second + ART first CELL - + CELL-VIEW @ BYTE-COPY
   [: BAD ART-U @ IMPORT ;] CHECKER-REG:E-UNIT-FORMAT TTHROWSQ
   SERIAL BASE-SERIAL @ T=
   ART BAD ART-U @ BYTE-COPY
   ART 3 cells + CELL-VIEW @
      BAD FIRST-GRAPH-OFF 7 cells - + CELL-VIEW !
   [: BAD ART-U @ IMPORT ;] CHECKER-REG:E-UNIT-FORMAT TTHROWSQ
   SERIAL BASE-SERIAL @ T= ;

: CHECK-BOUNDARIES ( -- )
   OWNER-RESET:SOURCE MARK
   SERIAL BASE-SERIAL !
   ART BAD ART-U @ BYTE-COPY
   0 BAD 6 cells + CELL-VIEW !
   BAD ART-U @ IMPORT
   SERIAL BASE-SERIAL @ T=
   OWNER-RESET:SOURCE MARK
   SERIAL BASE-SERIAL !
   ART BAD ART-U @ BYTE-COPY
   $8000000000000000 BAD 6 cells + CELL-VIEW !
   BAD ART-U @ IMPORT
   SERIAL BASE-SERIAL @ $8000000000000000 + T= ;

\ Prepared trees must validate every section before the first section publishes.
\ A later malformed range here changes neither the live binding window nor the
\ checker's symbol facts; a valid range consumes the retained plan once.
: CHECK-PREPARED ( -- )
   OWNER-RESET:SOURCE MARK
   SERIAL BASE-SERIAL !
   PREPARE-BEGIN
   ART ART-U @ 0 0 PREPARE drop
   ART BAD ART-U @ BYTE-COPY
   1 BAD CELL + CELL-VIEW !
   [: BAD ART-U @ 0 0 PREPARE drop ;] CHECKER-REG:E-UNIT-FORMAT TTHROWSQ
   SERIAL BASE-SERIAL @ T=
   PREPARE-END
   SERIAL BASE-SERIAL @ T=
   UNIT-EXPORT drop {: delta:ptr :}
   delta 3 cells + CELL-VIEW @ 0 T=
   delta 4 cells + CELL-VIEW @ 0 T=
   delta 5 cells + CELL-VIEW @ 0 T=
   PREPARE-BEGIN
   ART ART-U @ 0 0 PREPARE INSTALL
   PREPARE-END
   SERIAL BASE-SERIAL @ ART 6 cells + CELL-VIEW @ + T= ;

: PATCH-LINEAR ( -- )
   s" CODEC-LIN:tok" {: name:ptr len:n :}
   0 LINEAR-HITS !
   ART-U @ len - 1+ FIRST-GRAPH-OFF 7 cells - ?do
      BAD i + len name len STR= IF
         [char] x BAD i + len 1- + c!
         1 LINEAR-HITS +!
      THEN
   loop
   LINEAR-HITS @ 1 T= ;

: CHECK-LINEAR-PREP ( -- )
   OWNER-RESET:SOURCE
   s" package CODEC-LIN public DEFLINEAR CODEC-LIN:tok ;package" evaluate-closed
   MARK
   s" package CODEC-LIN public : CL-ALPHA ( -- n ) 1 ; : CL-ID ( CODEC-LIN:tok -- CODEC-LIN:tok ) ; ;package" evaluate-closed
   UNIT-EXPORT {: src:ptr size:n :}
   size ART-CAP <= TTRUE
   src ART size BYTE-COPY size ART-U !
   ART 8 cells + CELL-VIEW @ 0 T=
   ART BAD ART-U @ BYTE-COPY PATCH-LINEAR
   OWNER-RESET:SOURCE MARK
   SERIAL BASE-SERIAL !
   PREPARE-BEGIN
   [: BAD ART-U @ 0 0 PREPARE drop ;] CHECKER-REG:E-UNIT-FORMAT TTHROWSQ
   PREPARE-END
   SERIAL BASE-SERIAL @ T=
   UNIT-EXPORT drop {: delta:ptr :}
   delta 3 cells + CELL-VIEW @ 0 T=
   delta 4 cells + CELL-VIEW @ 0 T=
   PREPARE-BEGIN
   ART ART-U @ 0 0 PREPARE INSTALL
   PREPARE-END
   SERIAL BASE-SERIAL @ ART 6 cells + CELL-VIEW @ + T=
   ROOT$ s" unknown-linear.checker-unit" PATH FS:JOIN-PATH
   PATH swap BAD ART-U @ FS:WRITE-ALL ;

public

: RUN ( -- )
   T-RESET
   s" ordered package checker facts import into source-fresh checker" T-LABEL
   MARK SERIAL MARK-SERIAL !
   LOAD-UNIT SHADOW
   s" retired record keeps its own effect across a redefinition" T-LABEL
   RECORD-HISTORY
   s" a does> word binds its reissued record occurrence" T-LABEL
   CREATED-CONTRACT
   s" a fixed value is retained by actual record" T-LABEL
   FIXED-HISTORY
   s" an inferred fresh-region producer has a record contract" T-LABEL
   INFERRED-CONTRACT
   s" UNIT-EXTRA ( -- n ) UNIT-NBR:BL-ALPHA" CHECK-CANDIDATE! -1 T=
   s" fresh identity templates certify cold calls" T-LABEL
   CHECK-FRESH
   SAVE
   OWNER-RESET:SOURCE
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
   [: ART ART-U @ IMPORT ;] CHECKER-REG:E-UNIT-STATE TTHROWSQ
   SERIAL BASE-SERIAL @ ART 6 cells + CELL-VIEW @ + T=
   CHECK-CLIENT
   s" unit exporter accepts deferred declaration state" T-LABEL
   MARK DEFER-CHANGE
   [: UNIT-EXPORT 2drop ;] catch 0 T=
   s" zero and sign-bit deltas retain full cell semantics" T-LABEL
   CHECK-BOUNDARIES
   s" prepared checker sections reject a late malformed range before publication" T-LABEL
   CHECK-PREPARED
   s" prepared graph resolves every linear constructor before publication" T-LABEL
   CHECK-LINEAR-PREP
   T-REPORT
   s" checker unit artifact: " type ROOT$ type
   s" /unit-nbr.checker-unit and /unknown-linear.checker-unit" type cr ;

;package

CHECKER-UNIT-CODEC-TEST:RUN

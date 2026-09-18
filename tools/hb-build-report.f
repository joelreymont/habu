\ hb-build-report.f - typed in-process and wire-format hb-build reports.
\ Load after lib/json-write.f and lib/build-cache.f.
\
\ The JSON writer keeps no state of its own, so this module owns the report
\ bytes: REPORT-W writes into REPORT-BUF, and a span answered by one report word
\ stays valid until the next one runs.

require lib/build-cache.f
require lib/memory.f
require lib/json-write.f

package HB-BUILD

ENUM state empty complete ;ENUM

create CACHE-ROOT-BUF FS-PATH-CAP allot
1 LAYOUT-BUFFER CACHE-SOURCE-BUF BUILD-CACHE:source
1 LAYOUT-BUFFER STATE-BUF state

TYPED-VARIABLE TEXT-A ptr u8
variable TEXT-CAP
variable TEXT-U
variable CACHE-ROOT-U
variable ARTIFACT-HIT-FLAG
variable OBJECT-HIT-FLAG
variable MAKER-HIT-FLAG
variable MAKER-BUILT-FLAG
variable MAKER-RAN-FLAG
variable ELAPSED-VALUE
\ Where the bytes of the image this build wrote went: the same six numbers the
\ text summary prints, plus the file's own length. tools/image-size-lib.f
\ measures them and refuses unless its classes sum to that length, so `other`
\ is what no class claimed and the seven add up exactly.
variable SIZE-TOTAL
variable SIZE-CODE
variable SIZE-NAMES
variable SIZE-DATA
variable SIZE-DATA-ZERO
variable SIZE-PAD
variable SIZE-OTHER

\ Worst case one cache_root byte becomes \u00XX, plus the fixed keys and values.
6 constant REPORT-ESCAPE-MAX
FS-PATH-CAP REPORT-ESCAPE-MAX * 512 + constant REPORT-CAP
REPORT-CAP BUFFER: REPORT-BUF
TYPED-VARIABLE REPORT-W JSON-WRITE:writer

: REPORT-OPEN ( -- ptr JSON-WRITE:writer )
   REPORT-W REPORT-BUF REPORT-CAP JSON-WRITE:OPEN ;

: CACHE-SOURCE-PTR ( -- ptr BUILD-CACHE:source )
   0 CACHE-SOURCE-BUF ;

: CACHE-SOURCE! ( BUILD-CACHE:source -- )
   CACHE-SOURCE-PTR ! ;

: CACHE-SOURCE-VALUE ( -- BUILD-CACHE:source )
   CACHE-SOURCE-PTR @ ;

: STATE-PTR ( -- ptr state )
   0 STATE-BUF ;

: STATE! ( state -- )
   STATE-PTR ! ;

: STATE-VALUE ( -- state )
   STATE-PTR @ ;

: COMPLETE? ( -- bool )
   STATE-VALUE MATCH state
      empty OF 0 0= 0= ENDOF
      complete OF 0 0= ENDOF
   ;MATCH ;

: REQUIRE-COMPLETE ( -- )
   COMPLETE? 0= if E-BUILD-STATUS throw then ;

: COPY-ROOT! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < if E-STR-BOUNDS throw then
   u FS-PATH-CAP > if E-STR-CAPACITY throw then
   a CACHE-ROOT-BUF u BYTE-COPY
   u CACHE-ROOT-U ! ;

: TEXT-A@ ( -- ptr u8 )
   TEXT-A @ ;

: TEXT-A! ( ptr u8 -- )
   TEXT-A ! ;

: TEXT-ALLOC ( n -- ptr u8 )
   MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop ;

: TEXT-NEED ( n -- n ) {: add:n :}
   add 0 < if E-STR-BOUNDS throw then
   add MEM-MAX-N TEXT-U @ - > if E-STR-CAPACITY throw then
   TEXT-U @ add + ;

: TEXT-BUF ( n -- ptr u8 ) {: need:n :}
   TEXT-CAP @ need < if
      need TEXT-ALLOC {: dst:ptr :}
      TEXT-U @ 0 > if TEXT-A@ dst TEXT-U @ BYTE-COPY then
      dst TEXT-A!
      need TEXT-CAP !
   then
   TEXT-A@ ;

: TEXT-RESET ( -- )
   0 TEXT-U ! ;

: TEXT+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u TEXT-NEED {: need:n :}
   need TEXT-BUF {: dst:ptr :}
   a dst TEXT-U @ + u BYTE-COPY
   need TEXT-U ! ;

: TEXT-BOOL+ ( bool -- )
   if s" true" else s" false" then TEXT+ ;

: TEXT-QUOTED+ ( ptr u8 n -- ) {: a:ptr u:n :}
   REPORT-OPEN a u JSON-WRITE:STRING
   JSON-WRITE:$ TEXT+ ;

: TEXT$ ( -- ptr u8 n )
   TEXT-A@ TEXT-U @ ;

public

: RESET ( -- )
   construct state empty STATE!
   0 CACHE-ROOT-U !
   0 ARTIFACT-HIT-FLAG !
   0 OBJECT-HIT-FLAG !
   0 MAKER-HIT-FLAG !
   0 MAKER-BUILT-FLAG !
   0 MAKER-RAN-FLAG !
   0 ELAPSED-VALUE !
   0 SIZE-TOTAL !
   0 SIZE-CODE !
   0 SIZE-NAMES !
   0 SIZE-DATA !
   0 SIZE-DATA-ZERO !
   0 SIZE-PAD !
   0 SIZE-OTHER ! ;

: VALID? ( -- bool )
   COMPLETE? ;

: CAPTURE ( ptr u8 n BUILD-CACHE:source bool bool bool bool bool n -- )
   {: root:ptr rootu:n source:BUILD-CACHE:source artifact:bool object:bool maker:bool built:bool ran:bool elapsed:n :}
   construct state empty STATE!
   root rootu COPY-ROOT!
   source CACHE-SOURCE!
   artifact ARTIFACT-HIT-FLAG !
   object OBJECT-HIT-FLAG !
   maker MAKER-HIT-FLAG !
   built MAKER-BUILT-FLAG !
   ran MAKER-RAN-FLAG !
   elapsed ELAPSED-VALUE !
   construct state complete STATE! ;

: CAPTURE-SIZE ( n n n n n n n -- )
   {: total:n code:n names:n data:n dzero:n pad:n other:n :}
   \ The six terms plus `other` ARE the file, which is the property that lets a
   \ build system read one of them without reading the table.
   code names + data + dzero + pad + other + total <> if
      E-BUILD-STATUS throw
   then
   total SIZE-TOTAL !
   code SIZE-CODE !
   names SIZE-NAMES !
   data SIZE-DATA !
   dzero SIZE-DATA-ZERO !
   pad SIZE-PAD !
   other SIZE-OTHER ! ;

: CACHE-ROOT$ ( -- ptr u8 n )
   REQUIRE-COMPLETE
   CACHE-ROOT-BUF CACHE-ROOT-U @ ;

: CACHE-SOURCE ( -- BUILD-CACHE:source )
   REQUIRE-COMPLETE
   CACHE-SOURCE-VALUE ;

: ARTIFACT-HIT? ( -- bool )
   REQUIRE-COMPLETE
   ARTIFACT-HIT-FLAG @ 0 <> ;

: OBJECT-HIT? ( -- bool )
   REQUIRE-COMPLETE
   OBJECT-HIT-FLAG @ 0 <> ;

: MAKER-HIT? ( -- bool )
   REQUIRE-COMPLETE
   MAKER-HIT-FLAG @ 0 <> ;

: MAKER-BUILT? ( -- bool )
   REQUIRE-COMPLETE
   MAKER-BUILT-FLAG @ 0 <> ;

: MAKER-RAN? ( -- bool )
   REQUIRE-COMPLETE
   MAKER-RAN-FLAG @ 0 <> ;

: ELAPSED-NS ( -- n )
   REQUIRE-COMPLETE
   ELAPSED-VALUE @ ;

: REPORT$ ( -- ptr u8 n )
   REPORT-OPEN
   JSON-WRITE:OBJECT-START
   s" schema" s" hb-build-report" JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" version" 1 JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" cache_root" CACHE-ROOT$ JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" cache_source" CACHE-SOURCE BUILD-CACHE:SOURCE$ JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" artifact_hit" ARTIFACT-HIT? JSON-WRITE:FIELD-BOOL JSON-WRITE:COMMA
   s" object_hit" OBJECT-HIT? JSON-WRITE:FIELD-BOOL JSON-WRITE:COMMA
   s" maker_hit" MAKER-HIT? JSON-WRITE:FIELD-BOOL JSON-WRITE:COMMA
   s" maker_built" MAKER-BUILT? JSON-WRITE:FIELD-BOOL JSON-WRITE:COMMA
   s" maker_ran" MAKER-RAN? JSON-WRITE:FIELD-BOOL JSON-WRITE:COMMA
   s" elapsed_ns" ELAPSED-NS JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" size" JSON-WRITE:KEY
   JSON-WRITE:OBJECT-START
   s" total" SIZE-TOTAL @ JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" code" SIZE-CODE @ JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" names" SIZE-NAMES @ JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" data_written" SIZE-DATA @ JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" data_zero_filled" SIZE-DATA-ZERO @ JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" padding" SIZE-PAD @ JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" other" SIZE-OTHER @ JSON-WRITE:FIELD-U
   JSON-WRITE:OBJECT-END
   JSON-WRITE:OBJECT-END
   JSON-WRITE:$ ;

: PATH-ERROR$ ( -- ptr u8 n )
   REPORT-OPEN
   JSON-WRITE:OBJECT-START
   s" schema" s" hb-build-error" JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" version" 1 JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" code" s" E-BUILD-PATH" JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" cache_selected" BUILD-CACHE:SELECTED? JSON-WRITE:FIELD-BOOL JSON-WRITE:COMMA
   s" cache_root" BUILD-CACHE:SELECTED-ROOT$ JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" cache_source" BUILD-CACHE:SELECTED-SOURCE BUILD-CACHE:SOURCE$ JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" cause" BUILD-CACHE:CAUSE$ JSON-WRITE:FIELD-S
   JSON-WRITE:OBJECT-END
   JSON-WRITE:$ ;

: PATH-ERROR-TEXT$ ( -- ptr u8 n )
   TEXT-RESET
   s" hb-build: schema=hb-build-error version=1 code=E-BUILD-PATH cache_selected=" TEXT+
   BUILD-CACHE:SELECTED? TEXT-BOOL+
   s"  cache_root=" TEXT+
   BUILD-CACHE:SELECTED-ROOT$ TEXT-QUOTED+
   s"  cache_source=" TEXT+
   BUILD-CACHE:SELECTED-SOURCE BUILD-CACHE:SOURCE$ TEXT+
   s"  cause=" TEXT+
   BUILD-CACHE:CAUSE$ TEXT+
   TEXT$ ;

;package

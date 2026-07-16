\ hb-build-report.f - typed in-process and wire-format hb-build reports.
\ Load after lib/json-write.f and lib/build-cache.f.

require lib/build-cache.f
require lib/memory.f
require lib/json-write.f

package HB-BUILD

ENUM state empty complete ;ENUM

create CACHE-ROOT-BUF FS-PATH-CAP allot
1 LAYOUT-BUFFER CACHE-SOURCE-BUF BUILD-CACHE:source
1 LAYOUT-BUFFER STATE-BUF state

variable TEXT-A
variable TEXT-CAP
variable TEXT-U
variable CACHE-ROOT-U
variable ARTIFACT-HIT-FLAG
variable OBJECT-HIT-FLAG
variable MAKER-HIT-FLAG
variable MAKER-BUILT-FLAG
variable MAKER-RAN-FLAG
variable ELAPSED-VALUE

: FIELD-COMMA ( -- )
   JW-COMMA ;

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

: TEXT-A-FIELD ( -- ptr ptr u8 )
   TEXT-A 0 ptr-field ;

: TEXT-A@ ( -- ptr u8 )
   TEXT-A-FIELD @ ;

: TEXT-A! ( ptr u8 -- )
   TEXT-A-FIELD ! ;

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

: TEXT-QUOTED+ ( ptr u8 n -- )
   JW-RESET
   JW-STRING
   JW$ TEXT+ ;

: TEXT$ ( -- ptr u8 n )
   TEXT-A@ TEXT-U @ ;

: RESET ( -- )
   construct state empty STATE!
   0 CACHE-ROOT-U !
   0 ARTIFACT-HIT-FLAG !
   0 OBJECT-HIT-FLAG !
   0 MAKER-HIT-FLAG !
   0 MAKER-BUILT-FLAG !
   0 MAKER-RAN-FLAG !
   0 ELAPSED-VALUE ! ;

public

: VALID? ( -- bool )
   COMPLETE? ;

private

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

public

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
   JW-RESET
   JW-OBJECT-START
   s" schema" s" hb-build-report" JW-FIELD-S FIELD-COMMA
   s" version" 1 JW-FIELD-U FIELD-COMMA
   s" cache_root" CACHE-ROOT$ JW-FIELD-S FIELD-COMMA
   s" cache_source" CACHE-SOURCE BUILD-CACHE:SOURCE$ JW-FIELD-S FIELD-COMMA
   s" artifact_hit" ARTIFACT-HIT? JW-FIELD-BOOL FIELD-COMMA
   s" object_hit" OBJECT-HIT? JW-FIELD-BOOL FIELD-COMMA
   s" maker_hit" MAKER-HIT? JW-FIELD-BOOL FIELD-COMMA
   s" maker_built" MAKER-BUILT? JW-FIELD-BOOL FIELD-COMMA
   s" maker_ran" MAKER-RAN? JW-FIELD-BOOL FIELD-COMMA
   s" elapsed_ns" ELAPSED-NS JW-FIELD-U
   JW-OBJECT-END
   JW$ ;

: PATH-ERROR$ ( -- ptr u8 n )
   JW-RESET
   JW-OBJECT-START
   s" schema" s" hb-build-error" JW-FIELD-S FIELD-COMMA
   s" version" 1 JW-FIELD-U FIELD-COMMA
   s" code" s" E-BUILD-PATH" JW-FIELD-S FIELD-COMMA
   s" cache_selected" BUILD-CACHE:SELECTED? JW-FIELD-BOOL FIELD-COMMA
   s" cache_root" BUILD-CACHE:SELECTED-ROOT$ JW-FIELD-S FIELD-COMMA
   s" cache_source" BUILD-CACHE:SELECTED-SOURCE BUILD-CACHE:SOURCE$ JW-FIELD-S FIELD-COMMA
   s" cause" BUILD-CACHE:CAUSE$ JW-FIELD-S
   JW-OBJECT-END
   JW$ ;

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

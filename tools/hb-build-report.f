\ hb-build-report.f - typed in-process and wire-format hb-build reports.
\ The JSON writer keeps no state, so this module owns the report bytes: REPORT-W
\ writes into REPORT-BUF, and a span one report word answers stays valid until the
\ next one runs.

require src/lib/errors.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/memory.f
require src/lib/json-write.f

package HB-BUILD

ENUM state empty complete ;ENUM

1 LAYOUT-BUFFER STATE-BUF state

TYPED-VARIABLE TEXT-A ptr u8
variable TEXT-CAP
variable TEXT-U
variable MAKER-HIT-FLAG
variable MAKER-BUILT-FLAG
variable MAKER-RAN-FLAG
variable ELAPSED-VALUE
variable FILES-REUSED-N
variable FILES-COMPILED-N
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

\ Worst case one quoted path byte becomes \u00XX, plus the fixed keys and values.
6 constant REPORT-ESCAPE-MAX
PATH-CAP REPORT-ESCAPE-MAX * 512 + constant REPORT-CAP
REPORT-CAP BUFFER: REPORT-BUF
TYPED-VARIABLE REPORT-W JSON-WRITE:writer

: REPORT-OPEN ( -- ptr JSON-WRITE:writer )
   REPORT-W REPORT-BUF REPORT-CAP JSON-WRITE:OPEN ;

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

: TEXT-QUOTED+ ( ptr u8 n -- ) {: a:ptr u:n :}
   REPORT-OPEN a u JSON-WRITE:STRING
   JSON-WRITE:$ TEXT+ ;

: TEXT$ ( -- ptr u8 n )
   TEXT-A@ TEXT-U @ ;

: FILES-REUSED ( -- n )
   REQUIRE-COMPLETE
   FILES-REUSED-N @ ;

: FILES-COMPILED ( -- n )
   REQUIRE-COMPLETE
   FILES-COMPILED-N @ ;

public

: RESET ( -- )
   construct state empty STATE!
   0 MAKER-HIT-FLAG !
   0 MAKER-BUILT-FLAG !
   0 MAKER-RAN-FLAG !
   0 ELAPSED-VALUE !
   0 FILES-REUSED-N !
   0 FILES-COMPILED-N !
   0 SIZE-TOTAL !
   0 SIZE-CODE !
   0 SIZE-NAMES !
   0 SIZE-DATA !
   0 SIZE-DATA-ZERO !
   0 SIZE-PAD !
   0 SIZE-OTHER ! ;

: VALID? ( -- bool )
   COMPLETE? ;

: CAPTURE ( bool bool bool n -- )
   {: maker:bool built:bool ran:bool elapsed:n :}
   construct state empty STATE!
   maker MAKER-HIT-FLAG !
   built MAKER-BUILT-FLAG !
   ran MAKER-RAN-FLAG !
   elapsed ELAPSED-VALUE !
   construct state complete STATE! ;

: CAPTURE-SIZE ( n n n n n n n -- )
   {: total:n code:n names:n data:n dzero:n pad:n other:n :}
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

\ files_reused and files_compiled count the maker's application-load session (the
\ engine source load for --engine) once per file load: installed from file
\ objects, and compiled from source, resumed replays among them
\ (tools/native-definition-cache.f FILES-N, REUSED-N). Link-support and writer
\ sessions are not counted; a build that launched no maker reports 0 and 0, as
\ RESET leaves them.
: CAPTURE-FILES ( n n -- )
   {: reused:n compiled:n :}
   reused FILES-REUSED-N !
   compiled FILES-COMPILED-N ! ;

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
   s" maker_hit" MAKER-HIT? JSON-WRITE:FIELD-BOOL JSON-WRITE:COMMA
   s" maker_built" MAKER-BUILT? JSON-WRITE:FIELD-BOOL JSON-WRITE:COMMA
   s" maker_ran" MAKER-RAN? JSON-WRITE:FIELD-BOOL JSON-WRITE:COMMA
   s" files_reused" FILES-REUSED JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" files_compiled" FILES-COMPILED JSON-WRITE:FIELD-U JSON-WRITE:COMMA
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
   s" code" s" E-BUILD-PATH" JSON-WRITE:FIELD-S
   JSON-WRITE:OBJECT-END
   JSON-WRITE:$ ;

: PATH-ERROR-TEXT$ ( -- ptr u8 n )
   s" hb-build: schema=hb-build-error version=1 code=E-BUILD-PATH" ;

: TIMEOUT-ENV-ERROR$ ( -- ptr u8 n )
   REPORT-OPEN
   JSON-WRITE:OBJECT-START
   s" schema" s" hb-build-error" JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" version" 1 JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" code" s" E-BUILD-COMMAND" JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" env" s" HB_BUILD_TIMEOUT_MS" JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" expected" s" integer from 1 to 2147483647" JSON-WRITE:FIELD-S
   JSON-WRITE:OBJECT-END
   JSON-WRITE:$ ;

: TIMEOUT-ENV-ERROR-TEXT$ ( -- ptr u8 n )
   s" hb-build: HB_BUILD_TIMEOUT_MS must be an integer from 1 to 2147483647" ;

: MAKER-TIMEOUT-ERROR$ ( ptr u8 n ptr u8 n n ptr u8 n ptr u8 n -- ptr u8 n )
   {: maker:ptr makeru:n source:ptr sourceu:n limit:n out:ptr outu:n err:ptr erru:n :}
   sourceu outu + erru + 6 * 512 + {: cap:n :}
   TEXT-RESET
   REPORT-W cap TEXT-BUF cap JSON-WRITE:OPEN
   JSON-WRITE:OBJECT-START
   s" schema" s" hb-build-error" JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" version" 1 JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" code" s" E-PROC-TIMEOUT" JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" maker" maker makeru JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" source" source sourceu JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" timeout_ms" limit JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" stdout" out outu JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" stderr" err erru JSON-WRITE:FIELD-S
   JSON-WRITE:OBJECT-END
   JSON-WRITE:$ ;

: MAKER-TIMEOUT-ERROR-TEXT$ ( ptr u8 n ptr u8 n n -- ptr u8 n )
   {: maker:ptr makeru:n source:ptr sourceu:n limit:n :}
   TEXT-RESET
   s" hb-build: code=E-PROC-TIMEOUT maker=" TEXT+
   maker makeru TEXT+
   s"  source=" TEXT+
   source sourceu TEXT-QUOTED+
   s"  timeout_ms=" TEXT+
   REPORT-OPEN limit JSON-WRITE:U JSON-WRITE:$ TEXT+
   TEXT$ ;

;package

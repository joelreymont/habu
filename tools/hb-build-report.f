\ hb-build-report.f - typed in-process and wire-format hb-build reports.
\ The JSON writer keeps no state, so this module owns the report bytes: REPORT-W
\ writes into REPORT-OUT, and a span one report word answers stays valid until the
\ next one runs.

require src/lib/error.f
require src/lib/string.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/json.f
require src/lib/json/write.f

package HB-BUILD

ENUM state empty complete ;ENUM

1 LAYOUT-BUFFER STATE-BUF state

variable ELAPSED-VALUE
variable FILES-REUSED-N
variable FILES-COMPILED-N
\ Where the bytes of the program this build wrote went: the six classes the
\ text summary prints, which sum to the file's own length, and that length.
variable SIZE-TOTAL
variable SIZE-CODE
variable SIZE-NAMES
variable SIZE-DATA
variable SIZE-DATA-ZERO
variable SIZE-PAD
variable SIZE-OTHER

\ Made by the first report and kept for the process.
TYPED-VARIABLE REPORT-OUT ^VEC:vector<u8>
TYPED-VARIABLE REPORT-W JSON:writer

: REPORT-OPEN ( -- ^JSON:writer )
   REPORT-OUT @ NULL-PTR = if
      HEAP:ALLOCATOR 0 [type] u8 VEC:NEW REPORT-OUT !
   then
   REPORT-W REPORT-OUT @ JSON:OPEN ;

: STATE-PTR ( -- ^state )
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

: FILES-REUSED ( -- n )
   REQUIRE-COMPLETE
   FILES-REUSED-N @ ;

: FILES-COMPILED ( -- n )
   REQUIRE-COMPLETE
   FILES-COMPILED-N @ ;

public

: RESET ( -- )
   construct state empty STATE!
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

: CAPTURE ( n -- )
   {: elapsed:n :}
   construct state empty STATE!
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

\ files_reused and files_compiled count the build's file loads: linked from the
\ file objects, and compiled from source (src/habu/linker.f LINKED, COMPILED).
: CAPTURE-FILES ( n n -- )
   {: reused:n compiled:n :}
   reused FILES-REUSED-N !
   compiled FILES-COMPILED-N ! ;

: ELAPSED-NS ( -- n )
   REQUIRE-COMPLETE
   ELAPSED-VALUE @ ;

: REPORT$ ( -- str )
   REPORT-OPEN
   JSON:WRITE-OBJECT-START
   s" schema" s" hb-build-report" JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" version" 1 JSON:WRITE-FIELD-U JSON:WRITE-COMMA
   s" files_reused" FILES-REUSED JSON:WRITE-FIELD-U JSON:WRITE-COMMA
   s" files_compiled" FILES-COMPILED JSON:WRITE-FIELD-U JSON:WRITE-COMMA
   s" elapsed_ns" ELAPSED-NS JSON:WRITE-FIELD-U JSON:WRITE-COMMA
   s" size" JSON:WRITE-KEY
   JSON:WRITE-OBJECT-START
   s" total" SIZE-TOTAL @ JSON:WRITE-FIELD-U JSON:WRITE-COMMA
   s" code" SIZE-CODE @ JSON:WRITE-FIELD-U JSON:WRITE-COMMA
   s" names" SIZE-NAMES @ JSON:WRITE-FIELD-U JSON:WRITE-COMMA
   s" data_written" SIZE-DATA @ JSON:WRITE-FIELD-U JSON:WRITE-COMMA
   s" data_zero_filled" SIZE-DATA-ZERO @ JSON:WRITE-FIELD-U JSON:WRITE-COMMA
   s" padding" SIZE-PAD @ JSON:WRITE-FIELD-U JSON:WRITE-COMMA
   s" other" SIZE-OTHER @ JSON:WRITE-FIELD-U
   JSON:WRITE-OBJECT-END
   JSON:WRITE-OBJECT-END
   JSON:WRITTEN$ ;

: PATH-ERROR$ ( -- str )
   REPORT-OPEN
   JSON:WRITE-OBJECT-START
   s" schema" s" hb-build-error" JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" version" 1 JSON:WRITE-FIELD-U JSON:WRITE-COMMA
   s" code" s" E-BUILD-PATH" JSON:WRITE-FIELD-S
   JSON:WRITE-OBJECT-END
   JSON:WRITTEN$ ;

: PATH-ERROR-TEXT$ ( -- str )
   s" hb-build: schema=hb-build-error version=1 code=E-BUILD-PATH" ;

;package

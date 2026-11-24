\ Standard SPARQL Results JSON and N-Triples from SQL row batches.
\ Private to SPARQL. A batch's temporary arena is released before the next
\ READ; the caller's WRITE can refuse a byte limit without retaining rows.

require src/lib/sparql/compile.f
require src/lib/json.f

package SPARQL
public

-10000 s" invalid SPARQL backend result" ERROR:CODE E-QUERY

STRUCTURE call OPAQUE DERIVE addr
   FIELD plan ^plan
   FIELD context ^u8
   FIELD read [ ^u8 ^u8 n -- ^u8 n ]
   FIELD write [ ^u8 ^u8 n -- ]
   FIELD rows n
;STRUCTURE

private

CAST: >CURSOR ( ^u8 -- ^JSON:cursor )

: CHECK-RESULT ( ^call -- ) CALL-PLAN @ TREE CHECK-BUDGET ;

: OUT ( ^call ^u8 n -- )
   {: call bytes size:n :}
   call CHECK-RESULT
   call CALL-CONTEXT @ bytes size call CALL-WRITE @ execute ;

: FORM ( ^call -- n )
   CALL-PLAN @ TREE dup SYNTAX-ROOT @ NODE-K ;

: READ-ROWS ( ^call ^u8 n -- ^u8 n )
   {: call request size:n :}
   call CHECK-RESULT
   call CALL-CONTEXT @ request size call CALL-READ @ execute
   dup 0 <= if E-QUERY throw then ;

: BEGIN-RESULT ( ^call -- )
   {: call :}
   call FORM N-SELECT = if
      call CALL-PLAN @ {: plan :}
      plan TREE SYNTAX-ARENA @ ARENA:ALLOCATOR {: alloc :}
      alloc [type] JSON:writer ALLOC:NEW {: writer :}
      writer alloc plan TREE SYNTAX-LENGTH @ 6 * 128 + ALLOC:BYTES
         JSON:WRITE-OPEN JSON:WRITE-OBJECT-START
      s" head" JSON:WRITE-KEY JSON:WRITE-OBJECT-START
      s" vars" JSON:WRITE-KEY plan WRITE-NAMES JSON:WRITE-OBJECT-END
      JSON:WRITE-COMMA s" results" JSON:WRITE-KEY JSON:WRITE-OBJECT-START
      s" bindings" JSON:WRITE-KEY JSON:WRITE-ARRAY-START
      JSON:WRITTEN$ call -rot OUT
      writer JSON:WRITE-CLOSE
   else call FORM N-ASK = if
      call s\" {\"head\":{},\"boolean\":" OUT
   then then ;

\ The arena closes a reader even when malformed backend JSON throws.
: CURSOR ( ^ARENA:arena -- ^JSON:cursor )
   [type] JSON:cursor width>n
   [: >CURSOR JSON:READ-DISPOSE ;]
      ARENA:ALLOC-DISPOSE drop >CURSOR ;

: MEMBER$ ( ^u8 n ^u8 n ^ARENA:arena -- ^u8 n )
   {: bytes size:n key ku:n arena :}
   arena CURSOR bytes size JSON:READ-INIT
   JSON:READ-NEXT JSON:T-OBJ <> if E-QUERY throw then
   key ku JSON:READ-FIND-KEY if
      JSON:READ-ANY-SPAN$ {: out count:n :}
      JSON:READ-CLOSE out count
   else JSON:READ-CLOSE s" " then ;

: STRING$ ( ^u8 n ^ARENA:arena -- ^u8 n )
   {: bytes size:n arena :}
   arena ARENA:ALLOCATOR size ALLOC:BYTES {: out capacity:n :}
   arena CURSOR bytes size JSON:READ-INIT
   JSON:READ-NEXT JSON:T-STR <> if E-QUERY throw then
   out capacity JSON:READ-STR {: count:n :}
   JSON:READ-FINISH out count ;

: TEXT-MEMBER$ ( ^u8 n ^u8 n ^ARENA:arena -- ^u8 n )
   {: bytes size:n key ku:n arena :}
   bytes size key ku arena MEMBER$ dup 0= if exit then
   arena STRING$ ;

: HEX ( n -- u8 ) s" 0123456789abcdef" drop + c@ ;

: ESCAPE ( ^call n ^u8 -- )
   {: call byte:n out :}
   92 out c! 117 out 1+ c! 48 out 2 + c! 48 out 3 + c!
   byte 4 rshift HEX out 4 + c! byte 15 and HEX out 5 + c!
   call out 6 OUT ;

: HEX-ESCAPE? ( n bool -- bool )
   {: byte:n iri:bool :}
   byte 32 < byte 127 = or if true exit then
   iri 0= if false exit then
   byte 32 = byte 60 = or byte 62 = or byte 123 = or
   byte 125 = or byte 124 = or byte 94 = or byte 96 = or
   byte 34 = or byte 92 = or ;

: PLAIN ( ^call ^u8 n n -- )
   {: call text start:n end:n :}
   end start > if call text start + end start - OUT then ;

: QUOTED ( ^call ^u8 n bool ^u8 -- )
   {: call text size:n iri:bool scratch :}
   0 size 0 ?do
      {: start:n :}
      i 4095 and 0= if call CHECK-RESULT then
      text i + c@ {: byte:n :}
      byte iri HEX-ESCAPE? if
         call text start i PLAIN call byte scratch ESCAPE i 1+
      else byte 34 = byte 92 = or if
         call text start i PLAIN call s" \" OUT i
      else start then then
   loop {: start:n :} call text start size PLAIN ;

: IRI ( ^call ^u8 n ^u8 -- )
   {: call text size:n scratch :}
   call s" <" OUT call text size true scratch QUOTED call s" >" OUT ;

: BLANK ( ^call ^u8 n ^ARENA:arena -- )
   {: call text size:n arena :}
   arena ARENA:ALLOCATOR size 2 * 3 + ALLOC:BYTES {: out count:n :}
   s" _:b" drop out 3 BYTE-COPY
   size 0 ?do
      i 4095 and 0= if call CHECK-RESULT then
      out 3 + i 2 * + {: at :}
      text i + c@ dup 4 rshift HEX at c!
      15 and HEX at 1+ c!
   loop call out count OUT ;

: TERM ( ^call ^u8 n ^ARENA:arena -- )
   {: call bytes size:n arena :}
   arena ARENA:ALLOCATOR 8 ALLOC:BYTES drop {: scratch :}
   bytes size s" type" arena TEXT-MEMBER$ {: kind ku:n :}
   bytes size s" value" arena MEMBER$ dup 0= if E-QUERY throw then
      arena STRING$ {: value vu:n :}
   kind ku s" uri" STR= if call value vu scratch IRI exit then
   kind ku s" bnode" STR= if call value vu arena BLANK exit then
   kind ku s" literal" STR= 0= if E-QUERY throw then
   call s\" \"" OUT call value vu false scratch QUOTED call s\" \"" OUT
   bytes size s" xml:lang" arena TEXT-MEMBER$ dup 0<> if
      call s" @" OUT call -rot OUT exit
   then 2drop
   bytes size s" datatype" arena TEXT-MEMBER$ dup 0<> if
      call s" ^^" OUT call -rot scratch IRI
   else 2drop then ;

: GRAPH-ROW ( ^call ^u8 n ^ARENA:arena -- )
   {: call bytes size:n arena :}
   call bytes size s" s" arena MEMBER$ arena TERM
   call s"  " OUT
   call bytes size s" p" arena MEMBER$ arena TERM
   call s"  " OUT
   call bytes size s" o" arena MEMBER$ arena TERM
   call s\"  .\n" OUT ;

: ROW ( ^call ^u8 n ^ARENA:arena -- )
   {: call bytes size:n arena :}
   call FORM case
      N-SELECT of
         call CALL-ROWS @ 0<> if call s" ," OUT then
         call bytes size OUT
      endof
      N-ASK of
         call CALL-ROWS @ 0<> if E-QUERY throw then
         bytes size s" true" STR= bytes size s" false" STR= or 0= if E-QUERY throw then
         call bytes size OUT
      endof
      call bytes size arena GRAPH-ROW
   endcase
   1 call CALL-ROWS +! ;

: BATCH-IN ( ^call ^u8 n ^ARENA:arena -- bool )
   {: call bytes size:n arena :}
   call CALL-ROWS @ {: before:n :}
   arena CURSOR bytes size JSON:READ-INIT
   JSON:READ-NEXT JSON:T-ARR <> if E-QUERY throw then
   begin JSON:READ-NEXT JSON:T-ARR-END <> while
      JSON:READ-ANY-SPAN$ {: row ru:n :}
      call row ru arena ROW
   repeat JSON:READ-FINISH
   call CALL-ROWS @ before <> ;

: BATCH ( ^call ^u8 n -- bool ) [: BATCH-IN ;] ARENA:WITH ;

: END-RESULT ( ^call -- )
   {: call :}
   call FORM N-SELECT = if call s" ]}}" OUT exit then
   call FORM N-ASK = if
      call CALL-ROWS @ 1 <> if E-QUERY throw then
      call s" }" OUT
   then ;

;package

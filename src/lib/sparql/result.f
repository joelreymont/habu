\ Standard SPARQL Results JSON and N-Triples from SQL row batches.
\ Private to SPARQL. A batch's temporary arena is released before the next
\ READ; the caller's WRITE can refuse a byte limit without retaining rows.

require src/lib/sparql/compile.f
require src/lib/json.f

package SPARQL
public

-10000 s" invalid SPARQL backend result" ERROR:CODE E-QUERY

private

STRUCTURE call DERIVE addr
   FIELD plan ptr plan
   FIELD context ptr u8
   FIELD read [ ptr u8 ptr u8 n -- ptr u8 n ]
   FIELD write [ ptr u8 ptr u8 n -- ]
   FIELD rows n
;STRUCTURE

CAST: >CURSOR ( ptr u8 -- ptr JSON:cursor )

: CHECK-RESULT ( ptr call -- ) CALL-PLAN @ TREE CHECK-BUDGET ;

: OUT ( ptr call ptr u8 n -- )
   {: call bytes size:n :}
   call CHECK-RESULT
   call CALL-CONTEXT @ bytes size call CALL-WRITE @ execute ;

: FORM ( ptr call -- n )
   CALL-PLAN @ TREE dup SYNTAX-ROOT @ NODE-K ;

: READ-ROWS ( ptr call ptr u8 n -- ptr u8 n )
   {: call request size:n :}
   call CHECK-RESULT
   call CALL-CONTEXT @ request size call CALL-READ @ execute
   dup 0 <= if E-QUERY throw then ;

: BEGIN-RESULT ( ptr call -- )
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
: CURSOR ( ptr ARENA:arena -- ptr JSON:cursor )
   JSON:CURSOR-BYTES [: >CURSOR JSON:READ-DISPOSE ;]
      ARENA:ALLOC-DISPOSE drop >CURSOR ;

: MEMBER$ ( ptr u8 n ptr u8 n ptr ARENA:arena -- ptr u8 n )
   {: bytes size:n key ku:n arena :}
   arena CURSOR bytes size JSON:READ-INIT
   JSON:READ-NEXT JSON:T-OBJ <> if E-QUERY throw then
   key ku JSON:READ-FIND-KEY if
      JSON:READ-ANY-SPAN$ {: reader out count:n :}
      reader JSON:READ-CLOSE out count
   else JSON:READ-CLOSE s" " then ;

: STRING$ ( ptr u8 n ptr ARENA:arena -- ptr u8 n )
   {: bytes size:n arena :}
   arena ARENA:ALLOCATOR size ALLOC:BYTES {: out capacity:n :}
   arena CURSOR bytes size JSON:READ-INIT
   JSON:READ-NEXT JSON:T-STR <> if E-QUERY throw then
   out capacity JSON:READ-STR {: reader count:n :}
   reader JSON:READ-FINISH out count ;

: TEXT-MEMBER$ ( ptr u8 n ptr u8 n ptr ARENA:arena -- ptr u8 n )
   {: bytes size:n key ku:n arena :}
   bytes size key ku arena MEMBER$ dup 0= if exit then
   arena STRING$ ;

: HEX ( n -- u8 ) s" 0123456789abcdef" drop + c@ ;

: ESCAPE ( ptr call n ptr u8 -- )
   {: call byte:n out :}
   92 out c! 117 out 1+ c! 48 out 2 + c! 48 out 3 + c!
   byte 4 rshift HEX out 4 + c! byte 15 and HEX out 5 + c!
   call out 6 OUT ;

: QUOTED ( ptr call ptr u8 n bool ptr u8 -- )
   {: call text size:n iri:bool scratch :}
   size 0 ?do
      text i + c@ {: byte:n :}
      byte 32 < byte 127 = or
      iri if byte 32 = or byte 60 = or byte 62 = or byte 123 = or
         byte 125 = or byte 124 = or byte 94 = or byte 96 = or byte 34 = or byte 92 = or then if
         call byte scratch ESCAPE
      else
         byte 34 = byte 92 = or if call s" \" OUT then
         call text i + 1 OUT
      then
   loop ;

: IRI ( ptr call ptr u8 n ptr u8 -- )
   {: call text size:n scratch :}
   call s" <" OUT call text size true scratch QUOTED call s" >" OUT ;

: BLANK ( ptr call ptr u8 n ptr u8 -- )
   {: call text size:n scratch :}
   call s" _:b" OUT
   size 0 ?do
      text i + c@ dup 4 rshift HEX scratch c!
      15 and HEX scratch 1+ c! call scratch 2 OUT
   loop ;

: TERM ( ptr call ptr u8 n ptr ARENA:arena -- )
   {: call bytes size:n arena :}
   arena ARENA:ALLOCATOR 8 ALLOC:BYTES drop {: scratch :}
   bytes size s" type" arena TEXT-MEMBER$ {: kind ku:n :}
   bytes size s" value" arena MEMBER$ dup 0= if E-QUERY throw then
      arena STRING$ {: value vu:n :}
   kind ku s" uri" STR= if call value vu scratch IRI exit then
   kind ku s" bnode" STR= if call value vu scratch BLANK exit then
   kind ku s" literal" STR= 0= if E-QUERY throw then
   call s\" \"" OUT call value vu false scratch QUOTED call s\" \"" OUT
   bytes size s" xml:lang" arena TEXT-MEMBER$ dup 0<> if
      call s" @" OUT call -rot OUT exit
   then 2drop
   bytes size s" datatype" arena TEXT-MEMBER$ dup 0<> if
      call s" ^^" OUT call -rot scratch IRI
   else 2drop then ;

: GRAPH-ROW ( ptr call ptr u8 n ptr ARENA:arena -- )
   {: call bytes size:n arena :}
   call bytes size s" s" arena MEMBER$ arena TERM
   call s"  " OUT
   call bytes size s" p" arena MEMBER$ arena TERM
   call s"  " OUT
   call bytes size s" o" arena MEMBER$ arena TERM
   call s\"  .\n" OUT ;

: ROW ( ptr call ptr u8 n ptr ARENA:arena -- )
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

: BATCH-IN ( ptr call ptr u8 n ptr ARENA:arena -- bool )
   {: call bytes size:n arena :}
   call CALL-ROWS @ {: before:n :}
   arena CURSOR bytes size JSON:READ-INIT
   JSON:READ-NEXT JSON:T-ARR <> if E-QUERY throw then
   begin JSON:READ-NEXT JSON:T-ARR-END <> while
      JSON:READ-ANY-SPAN$ {: reader row ru:n :}
      call row ru arena ROW reader
   repeat JSON:READ-FINISH
   call CALL-ROWS @ before <> ;

: BATCH ( ptr call ptr u8 n -- bool ) [: BATCH-IN ;] ARENA:WITH ;

: END-RESULT ( ptr call -- )
   {: call :}
   call FORM N-SELECT = if call s" ]}}" OUT exit then
   call FORM N-ASK = if
      call CALL-ROWS @ 1 <> if E-QUERY throw then
      call s" }" OUT
   then ;

;package

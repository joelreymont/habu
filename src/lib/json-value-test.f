split-str
\ json-value-test.f - JSON:PARSE and JSON:TEXT$: the value as written without
\ the white space around it, the reader's rejections with their place, a
\ repeated member name rejected at any depth but not across objects, and a
\ cursor's depth mapping released. JSON:GET, JSON:FIND and JSON:AT: lookups,
\ their rejections placed in the document, and pointers that name nothing. The
\ kind tests, the walks and the scalar readers, and their wrong-kind rejections
\ with the document's place. JSON:REJECT's rejection of a value at its place.
\ Run: bin/hb --load src/lib/json-value-test.f

require src/lib/error.f
require src/lib/heap.f
require src/lib/memory.f
require src/lib/reason.f
require src/lib/span.f
require src/lib/test.f
require src/lib/vector.f
require src/lib/json.f
require src/lib/test/mapped.f
require src/lib/test/counting-allocator.f

package JSON-VALUE-TEST
private

: SAID ( n str -- ) {: code:n wa:^ wu:n :}
   code REASON:TEXT$ wa wu T$=
   REASON:RELEASE ;

: TEXT-OF ( str -- str )
   JSON:PARSE JSON:TEXT$ ;

: PARSED ( str -- )
   TEXT-OF 2drop ;

: TEST-TEXT ( -- )
   s" TEXT$ answers the value as written, without the white space around it" T-LABEL
   s\"   {\"a\":[1,2]}  " TEXT-OF s\" {\"a\":[1,2]}" T$=
   s\" \"x\"" TEXT-OF s\" \"x\"" T$= ;

: TEST-MALFORMED ( -- )
   s" PARSE rejects a text that is not one JSON value with the reader's code" T-LABEL
   [: s\" {\"a\":}" PARSED ;] E-JSON-READ-MALFORMED TTHROWSQ
   E-JSON-READ-MALFORMED
   s" 1:6: expected a value, found `}`; write the member's value"
   SAID
   [: s" 1 2" PARSED ;] E-JSON-READ-TRAILING TTHROWSQ
   [: s" " PARSED ;] E-JSON-READ-EOF TTHROWSQ
   [: s"  " PARSED ;] E-JSON-READ-EOF TTHROWSQ ;

: TEST-REPEAT ( -- )
   s" PARSE rejects a repeated member name at the repeat, at any depth" T-LABEL
   [: s\" {\"a\":1,\"b\":{\"c\":1,\"c\":2}}" PARSED ;]
   JSON:E-MEMBER TTHROWSQ
   JSON:E-MEMBER
   s\" 1:19: expected a member name not used before in this object, found `\"c\"`; remove the repeated member"
   SAID
   [: s\" {\"a\":1,\"a\":2}" PARSED ;] JSON:E-MEMBER TTHROWSQ
   JSON:E-MEMBER
   s\" 1:8: expected a member name not used before in this object, found `\"a\"`; remove the repeated member"
   SAID
   s" PARSE takes one name in two objects" T-LABEL
   s\" [{\"a\":1},{\"a\":2}]" TEXT-OF s\" [{\"a\":1},{\"a\":2}]" T$=
   s\" {\"a\":{\"a\":1}}" TEXT-OF s\" {\"a\":{\"a\":1}}" T$=
   s" PARSE rejects the repeat earliest in the text, not the first found" T-LABEL
   [: s\" {\"a\":1,\"a\":{\"x\":1,\"x\":2}}" PARSED ;] JSON:E-MEMBER TTHROWSQ
   JSON:E-MEMBER
   s\" 1:8: expected a member name not used before in this object, found `\"a\"`; remove the repeated member"
   SAID
   [: s\" {\"a\":{\"x\":1,\"x\":2},\"a\":3}" PARSED ;] JSON:E-MEMBER TTHROWSQ
   JSON:E-MEMBER
   s\" 1:13: expected a member name not used before in this object, found `\"x\"`; remove the repeated member"
   SAID
   [: s\" {\"a\":1,\"b\":{\"c\":1,\"d\":2,\"c\":3}}" PARSED ;] JSON:E-MEMBER TTHROWSQ
   JSON:E-MEMBER
   s\" 1:25: expected a member name not used before in this object, found `\"c\"`; remove the repeated member"
   SAID ;

: GOT ( str str -- str ) {: t:^ tu:n n:^ nu:n :}
   t tu JSON:PARSE n nu JSON:GET JSON:TEXT$ ;

\ The text of a found value, or `none`, which no JSON text is.
: OPTION$ ( option<JSON:value> -- str )
   MATCH option
     none OF s" none" ENDOF
     some OF JSON:TEXT$ ENDOF
   ;MATCH ;

: FOUND$ ( str str -- str ) {: t:^ tu:n n:^ nu:n :}
   t tu JSON:PARSE n nu JSON:FIND OPTION$ ;

: POINTED$ ( str -- str ) {: p:^ u:n :}
   s\" {\"a\":[1,2],\"a/b\":3}" JSON:PARSE p u JSON:AT OPTION$ ;

: NESTED$ ( -- str )
   s\" {\n \"a\": {\n  \"b\": {\"c\":1}\n }\n}" ;

: DEEP-MISSING ( -- )
   NESTED$ JSON:PARSE s" a" JSON:GET s" b" JSON:GET s" x" JSON:GET
   JSON:TEXT$ 2drop ;

: TEST-GET ( -- )
   s" GET answers a member's value, through two objects" T-LABEL
   s\" {\"a\":1,\"b\":2}" s" b" GOT s" 2" T$=
   s\" {\"a\":{\"b\":[true]}}" JSON:PARSE s" a" JSON:GET s" b" JSON:GET
   JSON:TEXT$ s" [true]" T$=
   s" GET rejects a missing member at its object's place" T-LABEL
   [: s\" {\"a\":{}}" JSON:PARSE s" a" JSON:GET s" x" JSON:GET
      JSON:TEXT$ 2drop ;] JSON:E-MEMBER TTHROWSQ
   JSON:E-MEMBER
   s" 1:6: expected a member `x`, found none; add the member or read it with JSON:FIND"
   SAID
   s" GET counts a deep value's place from the document's first line" T-LABEL
   [: DEEP-MISSING ;] JSON:E-MEMBER TTHROWSQ
   JSON:E-MEMBER
   s" 3:8: expected a member `x`, found none; add the member or read it with JSON:FIND"
   SAID
   s" GET rejects a member of an array" T-LABEL
   [: s\" {\"a\":[1]}" JSON:PARSE s" a" JSON:GET s" b" JSON:GET
      JSON:TEXT$ 2drop ;] JSON:E-MEMBER TTHROWSQ
   JSON:E-MEMBER
   s" 1:6: expected an object, found an array; read a member only from an object"
   SAID ;

\ 31 ASCII bytes, then a two-byte character across REASON:QUOTE-MOST.
: LONG-NAME$ ( -- str )  s" abcdefghijklmnopqrstuvwxyz01234é56789" ;

: TEST-LONG-NAME ( -- )
   s" GET shows a long missing name cut on a character's first byte" T-LABEL
   [: s" {}" LONG-NAME$ GOT 2drop ;] JSON:E-MEMBER TTHROWSQ
   JSON:E-MEMBER
   s" 1:1: expected a member `abcdefghijklmnopqrstuvwxyz01234...`, found none; add the member or read it with JSON:FIND"
   SAID ;

: TEST-FIND ( -- )
   s" FIND answers some member or none" T-LABEL
   s\" {\"a\":1}" s" a" FOUND$ s" 1" T$=
   s\" {\"a\":1}" s" b" FOUND$ s" none" T$=
   s" FIND rejects a member of an array" T-LABEL
   [: s" [1]" s" a" FOUND$ 2drop ;] JSON:E-MEMBER TTHROWSQ
   JSON:E-MEMBER
   s" 1:1: expected an object, found an array; read a member only from an object"
   SAID ;

: TEST-AT ( -- )
   s" AT answers the value a pointer names, or none" T-LABEL
   s" " POINTED$ s\" {\"a\":[1,2],\"a/b\":3}" T$=
   s" /a/1" POINTED$ s" 2" T$=
   s" /a~1b" POINTED$ s" 3" T$=
   s" /x" POINTED$ s" none" T$=
   s" /a/9" POINTED$ s" none" T$=
   s" /a/-" POINTED$ s" none" T$=
   s" AT rejects a malformed pointer" T-LABEL
   [: s" a" POINTED$ 2drop ;] E-JSON-POINTER TTHROWSQ ;

\ 100 levels: 64 inline, the rest in the mapping.
100 constant DEPTH
create DEEP-BUF DEPTH 2 * allot

: DEEP$ ( -- str )
   DEPTH 0 ?do [char] [ DEEP-BUF i + c! loop
   DEPTH 0 ?do [char] ] DEEP-BUF DEPTH + i + c! loop
   DEEP-BUF DEPTH 2 * ;

: DEEP ( -- )
   DEEP$ TEXT-OF DEEP$ T$= ;

: TEST-DEEP ( -- )
   s" PARSE reads 100 levels and releases the depth mapping" T-LABEL
   \ The first call maps the scope's pooled chunk, which the pool keeps.
   DEEP
   [: DEEP ;] MAPPED:RELEASED? TTRUE ;

: SEEN-NEW ( -- ^VEC:vector<u8> )
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW ;

\ Appends the text and a `;`.
: SEE ( ^VEC:vector<u8> str -- ^VEC:vector<u8> ) {: seen a:^ u:n :}
   a u seen VEC:APPEND  s" ;" seen VEC:APPEND  seen ;

\ Checks the text seen, then deletes the vector.
: SEEN= ( ^VEC:vector<u8> str -- ) {: seen a:^ u:n :}
   seen VEC:SPAN SPAN:$ a u T$=  seen VEC:DELETE ;

: SEE-TEXT ( ^VEC:vector<u8> JSON:value -- ^VEC:vector<u8> )
   JSON:TEXT$ SEE ;

: DROP-VALUE ( JSON:value -- ) {: v :} ;

: SEE-MEMBER
   ( ^VEC:vector<u8> JSON:value JSON:value -- ^VEC:vector<u8> )
   {: name value :}
   name SEE-TEXT value SEE-TEXT ;

: TEST-KINDS ( -- )
   s" the kind tests read a value's first byte" T-LABEL
   s"  {} " JSON:PARSE JSON:OBJECT? TTRUE
   s" []" JSON:PARSE JSON:OBJECT? TFALSE
   s" []" JSON:PARSE JSON:ARRAY? TTRUE
   s" {}" JSON:PARSE JSON:ARRAY? TFALSE
   s\" \"a\"" JSON:PARSE JSON:STRING? TTRUE
   s" 1" JSON:PARSE JSON:STRING? TFALSE
   s" -1" JSON:PARSE JSON:NUMBER? TTRUE
   s" 0.5" JSON:PARSE JSON:NUMBER? TTRUE
   s\" \"1\"" JSON:PARSE JSON:NUMBER? TFALSE
   s" null" JSON:PARSE JSON:NULL? TTRUE
   s" false" JSON:PARSE JSON:NULL? TFALSE
   s" true" JSON:PARSE JSON:BOOL? TTRUE
   s" false" JSON:PARSE JSON:BOOL? TTRUE
   s" null" JSON:PARSE JSON:BOOL? TFALSE
   s\" \"true\"" JSON:PARSE JSON:BOOL? TFALSE
   s" 0" JSON:PARSE JSON:BOOL? TFALSE ;

: EACH-OBJECT ( -- )
   s\" \n {\"b\":2}" JSON:PARSE [: DROP-VALUE ;] JSON:EACH ;

: TEST-EACH ( -- )
   s" EACH hands each element of an array, in order" T-LABEL
   SEEN-NEW s\" [1,\"a\",{\"b\":2}]" JSON:PARSE [: SEE-TEXT ;] JSON:EACH
   s\" 1;\"a\";{\"b\":2};" SEEN=
   s" EACH carries the stack under its body" T-LABEL
   0 s" [1,2,3]" JSON:PARSE [: JSON:>N + ;] JSON:EACH 6 T=
   s" EACH rejects an object at its place in the document" T-LABEL
   [: EACH-OBJECT ;] JSON:E-MEMBER TTHROWSQ
   JSON:E-MEMBER
   s" 2:2: expected an array, found an object; walk only an array with JSON:EACH"
   SAID ;

: TEST-EACH-MEMBER ( -- )
   s" EACH-MEMBER hands each member's name as written and its value" T-LABEL
   SEEN-NEW s\" {\"a\":1,\"b\\\"c\":[2]}" JSON:PARSE
   [: SEE-MEMBER ;] JSON:EACH-MEMBER
   s\" \"a\";1;\"b\\\"c\";[2];" SEEN=
   s" EACH-MEMBER carries the stack under its body" T-LABEL
   0 s\" {\"a\":1,\"b\":2}" JSON:PARSE
   [: DROP-VALUE DROP-VALUE 1+ ;] JSON:EACH-MEMBER 2 T=
   [: s" [1]" JSON:PARSE [: DROP-VALUE DROP-VALUE ;] JSON:EACH-MEMBER ;]
   JSON:E-MEMBER TTHROWSQ
   JSON:E-MEMBER
   s" 1:1: expected an object, found an array; walk only an object with JSON:EACH-MEMBER"
   SAID ;

: UNTIL-2 ( n JSON:value -- n bool ) {: k:n v :}
   k 1+  v JSON:>N 2 <> ;

: TEST-EACH-WHILE ( -- )
   s" EACH-WHILE stops after the body that answers false" T-LABEL
   0 s" [1,2,3]" JSON:PARSE [: UNTIL-2 ;] JSON:EACH-WHILE 2 T=
   s" EACH-WHILE walks to the end while the body answers true" T-LABEL
   0 s" [1,3,4]" JSON:PARSE [: UNTIL-2 ;] JSON:EACH-WHILE 3 T=
   s" EACH-WHILE rejects an object, naming itself" T-LABEL
   [: s" {}" JSON:PARSE [: DROP-VALUE true ;] JSON:EACH-WHILE ;]
   JSON:E-MEMBER TTHROWSQ
   JSON:E-MEMBER
   s" 1:1: expected an array, found an object; walk only an array with JSON:EACH-WHILE"
   SAID ;

: UNTIL-B ( n JSON:value JSON:value -- n bool )
   {: k:n name v :}
   k 1+  name s" b" JSON:STRING= 0= ;

: TEST-EACH-MEMBER-WHILE ( -- )
   s" EACH-MEMBER-WHILE stops after the body that answers false" T-LABEL
   0 s\" {\"a\":1,\"b\":2,\"c\":3}" JSON:PARSE
   [: UNTIL-B ;] JSON:EACH-MEMBER-WHILE 2 T=
   s" EACH-MEMBER-WHILE walks to the end while the body answers true" T-LABEL
   0 s\" {\"a\":1,\"c\":3,\"d\":4}" JSON:PARSE
   [: UNTIL-B ;] JSON:EACH-MEMBER-WHILE 3 T=
   s" EACH-MEMBER-WHILE rejects an array, naming itself" T-LABEL
   [: s" [1]" JSON:PARSE
      [: DROP-VALUE DROP-VALUE true ;] JSON:EACH-MEMBER-WHILE ;]
   JSON:E-MEMBER TTHROWSQ
   JSON:E-MEMBER
   s" 1:1: expected an object, found an array; walk only an object with JSON:EACH-MEMBER-WHILE"
   SAID ;

: TEST-STRING= ( -- )
   s" STRING= compares a string's decoded bytes" T-LABEL
   s\" \"aé\"" JSON:PARSE s" aé" JSON:STRING= TTRUE
   s\" \"a\\u00e9\"" JSON:PARSE s" aé" JSON:STRING= TTRUE
   s\" \"aé\"" JSON:PARSE s" a" JSON:STRING= TFALSE
   s" 1" JSON:PARSE s" 1" JSON:STRING= TFALSE ;

: WITH-NUMBER ( -- )
   s" 42" JSON:PARSE [: 2drop ;] JSON:WITH-STRING ;

: TEST-WITH-STRING ( -- )
   s" WITH-STRING lends a string's decoded text" T-LABEL
   s\" \"plain\"" JSON:PARSE [: s" plain" T$= ;] JSON:WITH-STRING
   s\" \"a\\nb\\u00e9\"" JSON:PARSE
   [: s\" a\nbé" T$= ;] JSON:WITH-STRING
   [: WITH-NUMBER ;] JSON:E-MEMBER TTHROWSQ
   JSON:E-MEMBER
   s" 1:1: expected a string, found an integer; write a string" SAID ;

\ Appends the string to a vector holding `x`, then checks the vector.
: APPENDED ( str str -- ) {: t:^ tu:n w:^ wu:n :}
   SEEN-NEW {: out :}
   s" x" out VEC:APPEND
   t tu JSON:PARSE out JSON:APPEND-STRING
   out w wu SEEN= ;

: APPEND-NUMBER ( -- )
   SEEN-NEW {: out :}
   [: s" 42" JSON:PARSE out JSON:APPEND-STRING ;]
   [: out VEC:DELETE ;] finally ;

\ The vector holds its own JSON text with no room to spare, so the growth for
\ the escaped string moves the text and fills the old block.
: APPEND-OWN ( -- )
   ALLOC-TEST:MOVING 0 [type] u8 VEC:NEW {: out :}
   s\" \"\\nabcdefghijklmnop\"" out VEC:APPEND
   out VEC:SPAN SPAN:$ JSON:PARSE out JSON:APPEND-STRING
   out s\" \"\\nabcdefghijklmnop\"\nabcdefghijklmnop" SEEN= ;

: TEST-APPEND-STRING ( -- )
   s" APPEND-STRING appends a string's decoded bytes" T-LABEL
   s\" \"plain\"" s" xplain" APPENDED
   s\" \"a\\nb\\u00e9\\ud83d\\ude00\""
   s\" xa\nbé😀" APPENDED
   s\" \"\"" s" x" APPENDED
   APPEND-OWN
   [: APPEND-NUMBER ;] JSON:E-MEMBER TTHROWSQ
   JSON:E-MEMBER
   s" 1:1: expected a string, found an integer; write a string" SAID ;

: N-OF ( str -- n )  JSON:PARSE JSON:>N ;

\ INTEGER? is true and >N answers want.
: WHOLE ( str n -- )
   {: t:^ u:n want:n :}
   t u JSON:PARSE JSON:INTEGER? TTRUE
   t u N-OF want T= ;

\ INTEGER? is false and >N rejects the value.
: NOT-WHOLE ( str -- )
   {: t:^ u:n :}
   t u JSON:PARSE JSON:INTEGER? TFALSE
   [: t u N-OF drop ;] JSON:E-MEMBER TTHROWSQ ;

: TEST->N ( -- )
   s" >N reads a whole number that fits in a cell, as INTEGER? says" T-LABEL
   s" 42" 42 WHOLE
   s" 1.0" 1 WHOLE
   s" 1e2" 100 WHOLE
   s" 10E-1" 1 WHOLE
   s" -0" 0 WHOLE
   s" 0e99999999999999999999" 0 WHOLE
   s" 9223372036854775807" MEM-MAX-N WHOLE
   s" -9223372036854775808" MEM-MAX-N negate 1- WHOLE
   s" 9.223372036854775807e18" MEM-MAX-N WHOLE
   s" 1.5" NOT-WHOLE
   JSON:E-MEMBER
   s" 1:1: expected a whole number from -9223372036854775808 to 9223372036854775807, found `1.5`; write a whole number in that range"
   SAID
   s" 9223372036854775808" NOT-WHOLE
   s" -9223372036854775809" NOT-WHOLE
   s" 1e19" NOT-WHOLE
   s" 1e99999999999999999999" NOT-WHOLE
   s\" \"1\"" NOT-WHOLE
   s" true" NOT-WHOLE ;

: INTEGRAL ( str -- )  JSON:PARSE JSON:INTEGRAL? TTRUE ;
: NOT-INTEGRAL ( str -- )  JSON:PARSE JSON:INTEGRAL? TFALSE ;

: TEST-INTEGRAL? ( -- )
   s" INTEGRAL? is true for a number with no fraction, any size" T-LABEL
   s" 12" INTEGRAL
   s" -0" INTEGRAL
   s" 1.0" INTEGRAL
   s" 1e30" INTEGRAL
   s" 0e5" INTEGRAL
   s" 123456789012345678901234" INTEGRAL
   s" 1e99999999999999999999" INTEGRAL
   s" 1.5" NOT-INTEGRAL
   s" 1e-1" NOT-INTEGRAL
   s" 1e-99999999999999999999" NOT-INTEGRAL
   s\" \"1\"" NOT-INTEGRAL
   s" true" NOT-INTEGRAL ;

: BOOL-OF ( str -- bool )  JSON:PARSE JSON:>BOOL ;

: TEST->BOOL ( -- )
   s" >BOOL reads true and false" T-LABEL
   s" true" BOOL-OF TTRUE
   s" false" BOOL-OF TFALSE
   [: s" null" BOOL-OF drop ;] JSON:E-MEMBER TTHROWSQ
   JSON:E-MEMBER
   s" 1:1: expected true or false, found `null`; write true or false" SAID ;

\ The `1` on the third line of NESTED$.
: NESTED-REJECT ( -- )
   NESTED$ JSON:PARSE s" a" JSON:GET s" b" JSON:GET s" c" JSON:GET
   s" a count above 1" s" write a count of 2 or more" JSON:REJECT ;

: OBJECT-REJECT ( -- )
   s\" {\"a\":{\"b\":1}}" JSON:PARSE s" a" JSON:GET
   s" a number" s" write the limit as a number" JSON:REJECT ;

: TEST-REJECT ( -- )
   s" REJECT rejects a value at its place in the document" T-LABEL
   [: NESTED-REJECT ;] JSON:E-MEMBER TTHROWSQ
   JSON:E-MEMBER
   s" 3:13: expected a count above 1, found `1`; write a count of 2 or more"
   SAID
   s" REJECT shows an object's `{` as found" T-LABEL
   [: OBJECT-REJECT ;] JSON:E-MEMBER TTHROWSQ
   JSON:E-MEMBER
   s" 1:6: expected a number, found `{`; write the limit as a number" SAID ;

public

: RUN ( -- )
   TEST-TEXT
   TEST-MALFORMED
   TEST-REPEAT
   TEST-DEEP
   TEST-GET
   TEST-LONG-NAME
   TEST-FIND
   TEST-AT
   TEST-KINDS
   TEST-EACH
   TEST-EACH-MEMBER
   TEST-EACH-WHILE
   TEST-EACH-MEMBER-WHILE
   TEST-STRING=
   TEST-WITH-STRING
   TEST-APPEND-STRING
   TEST->N
   TEST-INTEGRAL?
   TEST->BOOL
   TEST-REJECT ;

;package

T-RESET
JSON-VALUE-TEST:RUN
T-REPORT
s" json-value-test: ok" type cr

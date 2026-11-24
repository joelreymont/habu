split-str
\ json-schema-test.f - schemas through JSON-SCHEMA:COMPILE: the shapes the
\ consumers register and each rejected schema's text; values through
\ JSON-SCHEMA:VALID?, each violation's text.
\ Run: bin/hb --load src/lib/json-schema-test.f

require src/lib/error.f
require src/lib/test.f
require src/lib/arena.f
require src/lib/heap.f
require src/lib/span.f
require src/lib/vector.f
require src/lib/reason.f
require src/lib/json.f
require src/lib/json-schema.f

package JSON-SCHEMA-TEST

: PARSED ( ^u8 n ^ARENA:arena -- JSON-SCHEMA:schema ) {: t:^ u:n a:^ :}
   t u JSON:PARSE a JSON-SCHEMA:COMPILE ;

: COMPILE-ONLY ( ^u8 n -- )
   [: PARSED drop ;] ARENA:WITH ;

: ACCEPTS ( ^u8 n -- ) {: t:^ u:n :}
   [: t u COMPILE-ONLY ;] catch 0 T= ;

\ The schema is rejected with exactly the text given.
: REJECTS ( ^u8 n ^u8 n -- ) {: t:^ u:n w:^ wu:n :}
   [: t u COMPILE-ONLY ;] JSON-SCHEMA:E-SCHEMA TTHROWSQ
   JSON-SCHEMA:E-SCHEMA REASON:TEXT$ w wu T$=
   REASON:RELEASE ;

: TAKEN ( -- )
   s" COMPILE takes the sapd shape" T-LABEL
   s\" {\"type\":\"object\",\"properties\":{\"filter\":{\"type\":\"string\"},\"top\":{\"type\":\"integer\",\"minimum\":0,\"maximum\":500},\"key\":{\"type\":\"object\",\"additionalProperties\":{\"type\":\"string\"}}},\"required\":[\"filter\"],\"additionalProperties\":false}" ACCEPTS
   s" COMPILE takes a Tender shape: a type array, enum, items, minimum" T-LABEL
   s\" {\"type\":\"object\",\"properties\":{\"limit\":{\"type\":[\"integer\",\"null\"],\"minimum\":1},\"state\":{\"type\":\"string\",\"enum\":[\"open\",\"closed\"]},\"tags\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}}},\"required\":[\"state\"]}" ACCEPTS
   s" COMPILE takes a Maki shape: anyOf, pattern, maxProperties, lengths, number" T-LABEL
   s\" {\"type\":\"object\",\"properties\":{\"name\":{\"type\":\"string\",\"pattern\":\"^[a-z][a-z0-9-]*$\",\"minLength\":1,\"maxLength\":64},\"weight\":{\"type\":\"number\",\"minimum\":0.5,\"maximum\":1e3},\"target\":{\"anyOf\":[{\"type\":\"string\"},{\"type\":\"integer\"}]},\"labels\":{\"type\":\"object\",\"maxProperties\":8}},\"additionalProperties\":false}" ACCEPTS
   s" COMPILE takes a Zen shape: $schema, oneOf, format, uniqueItems, {}, enum with null" T-LABEL
   s\" {\"$schema\":\"https://json-schema.org/draft/2020-12/schema\",\"type\":\"object\",\"description\":\"A step\",\"properties\":{\"when\":{\"type\":\"string\",\"format\":\"date-time\"},\"step\":{\"oneOf\":[{\"type\":\"string\"},{\"type\":\"null\"}]},\"ids\":{\"type\":\"array\",\"items\":{\"type\":\"string\"},\"uniqueItems\":true,\"minItems\":1,\"maxItems\":10},\"extra\":{},\"mode\":{\"enum\":[\"a\",null]}}}" ACCEPTS ;

: REJECTED ( -- )
   s" COMPILE rejects an unknown keyword, listing the keywords" T-LABEL
   s\" {\"type\":\"integer\",\"multipleOf\":2}"
   s\" 1:19: expected one of `$schema`, `type`, `enum`, `minimum`, `maximum`, `minLength`, `maxLength`, `pattern`, `properties`, `required`, `additionalProperties`, `maxProperties`, `items`, `minItems`, `maxItems`, `uniqueItems`, `anyOf`, `oneOf`, `description`, `title`, `default`, `examples`, `deprecated`, `readOnly`, `writeOnly`, `$comment`, `format`, found `\"multipleOf\"`; rename the member to one of those, or remove it"
   REJECTS
   s" COMPILE rejects `$schema` in a sub-schema" T-LABEL
   s\" {\"items\":{\"$schema\":\"https://json-schema.org/draft/2020-12/schema\"}}"
   s\" 1:11: expected one of `type`, `enum`, `minimum`, `maximum`, `minLength`, `maxLength`, `pattern`, `properties`, `required`, `additionalProperties`, `maxProperties`, `items`, `minItems`, `maxItems`, `uniqueItems`, `anyOf`, `oneOf`, `description`, `title`, `default`, `examples`, `deprecated`, `readOnly`, `writeOnly`, `$comment`, `format`, found `\"$schema\"`; rename the member to one of those, or remove it"
   REJECTS
   s" COMPILE rejects `$schema` naming another dialect" T-LABEL
   s\" {\"$schema\":\"http://json-schema.org/draft-07/schema#\"}"
   s\" 1:12: expected `$schema` as `https://json-schema.org/draft/2020-12/schema`, found `\"http://json-schema.org/draft-07...`; name the 2020-12 dialect, or remove `$schema`"
   REJECTS
   s" COMPILE rejects an unknown type name" T-LABEL
   s\" {\"type\":\"text\"}"
   s\" 1:9: expected `type` as one of `null`, `boolean`, `object`, `array`, `number`, `string` and `integer`, found `\"text\"`; write one of those type names"
   REJECTS
   s" COMPILE rejects an empty type array at its `[`" T-LABEL
   s\" {\"type\":[]}"
   s\" 1:9: expected `type` as an array of one type name or more, found `[`; name a type, or remove `type`"
   REJECTS
   s" COMPILE rejects a repeated type at its second copy" T-LABEL
   s\" {\"type\":[\"string\",\"null\",\"string\"]}"
   s\" 1:26: expected `type` naming each type once, found `\"string\"`; remove the repeated type name"
   REJECTS
   s" COMPILE rejects an empty enum at its `[`" T-LABEL
   s\" {\"enum\":[ ]}"
   s\" 1:9: expected `enum` as an array of one value or more, found `[`; list the values allowed in an array"
   REJECTS
   s" COMPILE rejects a negative minLength" T-LABEL
   s\" {\"minLength\":-1}"
   s\" 1:14: expected `minLength` as an integer from 0 to 9223372036854775807, found `-1`; write an integer in that range"
   REJECTS
   s" COMPILE rejects a fractional minLength" T-LABEL
   s\" {\"minLength\":1.5}"
   s\" 1:14: expected `minLength` as an integer from 0 to 9223372036854775807, found `1.5`; write an integer in that range"
   REJECTS
   s" COMPILE rejects a minimum written as a string" T-LABEL
   s\" {\"minimum\":\"0\"}"
   s\" 1:12: expected `minimum` as a number, found `\"0\"`; write a number, with no quotes"
   REJECTS
   s" COMPILE rejects a pattern REGEX rejects, with REGEX's text" T-LABEL
   s\" {\"type\":\"string\",\"pattern\":\"(a\"}"
   s\" 1:28: expected a pattern the dialect takes, found `\"(a\"`; pattern column 3: expected `)` to close a group, found the end of the pattern; close each `(` with `)`; rewrite the pattern as its text says"
   REJECTS
   s" COMPILE rejects properties that are no object" T-LABEL
   s\" {\"properties\":[\"a\"]}"
   s\" 1:15: expected `properties` as an object of schema objects, found `[`; write an object mapping each property name to its schema"
   REJECTS
   s" COMPILE rejects a required name that is no string" T-LABEL
   s\" {\"required\":[\"a\",1]}"
   s\" 1:18: expected `required` as an array of property names, found `1`; write the names as strings in an array"
   REJECTS
   s" COMPILE rejects a required name repeated at its second copy" T-LABEL
   s\" {\"required\":[\"a\",\"b\",\"a\"]}"
   s\" 1:22: expected `required` naming each property once, found `\"a\"`; remove the repeated name"
   REJECTS
   s" COMPILE rejects additionalProperties a number" T-LABEL
   s\" {\"additionalProperties\":1}"
   s\" 1:25: expected `additionalProperties` as `true`, `false` or a schema object, found `1`; write a schema object, `{}` for any value"
   REJECTS
   s" COMPILE rejects items `true`" T-LABEL
   s\" {\"type\":\"array\",\"items\":true}"
   s\" 1:25: expected `items` as a schema object, found `true`; write a schema object, `{}` for any value"
   REJECTS
   s" COMPILE rejects an empty anyOf at its `[`" T-LABEL
   s\" {\"anyOf\":[]}"
   s\" 1:10: expected `anyOf` as an array of one schema object or more, found `[`; list the alternatives' schemas in an array"
   REJECTS
   s" COMPILE rejects uniqueItems beside object items, at its `true`" T-LABEL
   s\" {\"type\":\"array\",\"uniqueItems\":true,\"items\":{\"type\":\"object\"}}"
   s\" 1:31: expected `uniqueItems` beside `items` whose `type` names scalar types only, found `true`; remove `uniqueItems`, or give `items` a `type` of scalars only"
   REJECTS
   s" COMPILE rejects a minimum whose exponent leaves a cell" T-LABEL
   s\" {\"minimum\":1e99999999999999999999}"
   s\" 1:12: expected `minimum` as a number whose exponent a signed 64-bit cell holds, found `1e99999999999999999999`; write the bound with a smaller exponent"
   REJECTS
   s" COMPILE rejects an enum number whose exponent leaves a cell" T-LABEL
   s\" {\"enum\":[1e99999999999999999999]}"
   s\" 1:10: expected `enum` values whose exponents a signed 64-bit cell holds, found `1e99999999999999999999`; write each number with a smaller exponent"
   REJECTS
   s" COMPILE rejects an enum number nested in a value, at the number" T-LABEL
   s\" {\"enum\":[{\"a\":[1,1e99999999999999999999]}]}"
   s\" 1:18: expected `enum` values whose exponents a signed 64-bit cell holds, found `1e99999999999999999999`; write each number with a smaller exponent"
   REJECTS ;

\ Each cell holds a vector, NULL-PTR while it is not made.
TYPED-VARIABLE VIOLATION ^VEC:vector<u8>
TYPED-VARIABLE NESTED ^VEC:vector<u8>

: MAKE ( ^^VEC:vector<u8> -- ) {: c:^ :}
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW c ! ;

: UNMAKE ( ^^VEC:vector<u8> -- ) {: c:^ :}
   c @ VEC:DELETE  NULL-PTR c ! ;

: VIOLATION$ ( -- ^u8 n ) VIOLATION @ VEC:SPAN SPAN:$ ;

: CHECKED ( ^u8 n ^u8 n ^ARENA:arena -- bool )
   {: v:^ vu:n s:^ su:n a:^ :}
   s su a PARSED {: sc :}
   v vu sc VIOLATION @ JSON-SCHEMA:VALID? ;

\ The value v checked against the schema s, the vector cleared first.
: VERDICT ( ^u8 n ^u8 n -- bool ) {: s:^ su:n v:^ vu:n :}
   VIOLATION @ 0 VEC:RESIZE
   v vu s su [: CHECKED ;] ARENA:WITH ;

: PASSES ( ^u8 n ^u8 n -- )
   VERDICT TTRUE  VIOLATION$ s" " T$= ;

\ The value fails with exactly the text given.
: FAILS ( ^u8 n ^u8 n ^u8 n -- ) {: s:^ su:n v:^ vu:n w:^ wu:n :}
   s su v vu VERDICT TFALSE  VIOLATION$ w wu T$= ;

: SAPD$ ( -- ^u8 n )
   s\" {\"type\":\"object\",\"properties\":{\"filter\":{\"type\":\"string\"},\"top\":{\"type\":\"integer\",\"minimum\":0,\"maximum\":500},\"key\":{\"type\":\"object\",\"additionalProperties\":{\"type\":\"string\"}}},\"required\":[\"filter\"],\"additionalProperties\":false}" ;

: SAPD ( -- )
   s" VALID? takes sapd's arguments" T-LABEL
   SAPD$ s\" {\"filter\":\"x\",\"top\":5,\"key\":{\"a\":\"b\"}}" PASSES
   s" VALID? rejects a root of the wrong type" T-LABEL
   SAPD$ s" []"
   s" : expected a value of type `object`, found `[]`; send a value of that type"
   FAILS
   s" VALID? rejects a member properties does not name, listing them" T-LABEL
   SAPD$ s\" {\"filters\":\"x\"}"
   s" : expected only the members `filter`, `top` and `key`, found `filters`; rename it to one of those or remove it"
   FAILS
   s" VALID? rejects a fraction where an integer belongs" T-LABEL
   SAPD$ s\" {\"filter\":\"x\",\"top\":1.5}"
   s" .top: expected a value of type `integer`, found `1.5`; send a whole number"
   FAILS
   s" VALID? rejects a number above the maximum" T-LABEL
   SAPD$ s\" {\"filter\":\"x\",\"top\":501}"
   s" .top: expected a number of at most `500`, found `501`; send a number in that range"
   FAILS
   s" VALID? rejects a number below the minimum" T-LABEL
   SAPD$ s\" {\"filter\":\"x\",\"top\":-1}"
   s" .top: expected a number of at least `0`, found `-1`; send a number in that range"
   FAILS
   s" VALID? rejects a member additionalProperties' schema rejects" T-LABEL
   SAPD$ s\" {\"filter\":\"x\",\"key\":{\"a\":1}}"
   s" .key.a: expected a value of type `string`, found `1`; send a value of that type"
   FAILS
   s" VALID? rejects a missing required member" T-LABEL
   SAPD$ s\" {\"top\":5}"
   s" : expected a member `filter`, found none; add `filter`"
   FAILS
   s" VALID? finds nine members and required names past a map's growth" T-LABEL
   s\" {\"properties\":{\"p1\":{},\"p2\":{},\"p3\":{},\"p4\":{},\"p5\":{},\"p6\":{},\"p7\":{},\"p8\":{},\"p9\":{}},\"required\":[\"p9\",\"p8\",\"p7\",\"p6\",\"p5\",\"p4\",\"p3\",\"p2\",\"p1\"],\"additionalProperties\":false}"
   s\" {\"p7\":1,\"p2\":1,\"p9\":1,\"p1\":1,\"p6\":1,\"p3\":1,\"p8\":1,\"p5\":1}"
   s" : expected a member `p4`, found none; add `p4`"
   FAILS ;

: VALUES ( -- )
   s" VALID? rejects an exponent past a cell at a bound" T-LABEL
   s\" {\"type\":\"number\",\"minimum\":0}" s" 1e99999999999999999999"
   s" : expected a number whose exponent a signed 64-bit cell holds, found `1e99999999999999999999`; write the number with a smaller exponent"
   FAILS
   s" VALID? types a whole number a cell holds as integer, whatever its spelling" T-LABEL
   s\" {\"type\":\"integer\"}" s" 1.0" PASSES
   s\" {\"type\":\"integer\"}" s" 1e2" PASSES
   s\" {\"type\":\"integer\"}" s" -9223372036854775808" PASSES
   s\" {\"type\":\"integer\"}" s" 9223372036854775807" PASSES
   s\" {\"type\":\"integer\"}" s" 12345678901234567890"
   s" : expected a value of type `integer`, found `12345678901234567890`; send a whole number from -9223372036854775808 to 9223372036854775807"
   FAILS
   s\" {\"type\":\"integer\"}" s" 1e99999999999999999999"
   s" : expected a value of type `integer`, found `1e99999999999999999999`; send a whole number from -9223372036854775808 to 9223372036854775807"
   FAILS
   s\" {\"type\":\"integer\"}" s" 1e-1"
   s" : expected a value of type `integer`, found `1e-1`; send a whole number"
   FAILS
   s" VALID? names two types with or" T-LABEL
   s\" {\"type\":[\"integer\",\"null\"]}" s\" \"x\""
   s\" : expected a value of type `integer` or `null`, found `\"x\"`; send a value of one of those types"
   FAILS
   s" VALID? names three types in the schema's order" T-LABEL
   s\" {\"type\":[\"string\",\"number\",\"null\"]}" s" true"
   s" : expected a value of type `string`, `number` or `null`, found `true`; send a value of one of those types"
   FAILS
   s" VALID? takes a value the enum lists" T-LABEL
   s\" {\"enum\":[\"open\",\"closed\"]}" s\" \"open\"" PASSES
   s" VALID? rejects a value the enum does not list" T-LABEL
   s\" {\"enum\":[\"open\",\"closed\"]}" s\" \"shut\""
   s\" : expected one of `[\"open\",\"closed\"]`, found `\"shut\"`; send one of those values"
   FAILS
   s" VALID? checks enum before an object's members" T-LABEL
   s\" {\"enum\":[{\"a\":1}],\"properties\":{\"a\":{\"type\":\"string\"}}}"
   s\" {\"a\":2}"
   s\" : expected one of `[{\"a\":1}]`, found `{\"a\":2}`; send one of those values"
   FAILS
   s" VALID? checks an object's members after enum takes it" T-LABEL
   s\" {\"enum\":[{\"a\":1}],\"properties\":{\"a\":{\"type\":\"string\"}}}"
   s\" {\"a\":1}"
   s" .a: expected a value of type `string`, found `1`; send a value of that type"
   FAILS
   s" VALID? compares enum numbers by value" T-LABEL
   s\" {\"enum\":[1]}" s" 1.0" PASSES
   s" VALID? counts minLength in characters" T-LABEL
   s\" {\"minLength\":2}" s\" \"é\""
   s\" : expected a string of at least 2 characters, found `\"é\"`; send a longer string"
   FAILS
   s" VALID? counts maxLength in characters" T-LABEL
   s\" {\"maxLength\":1}" s\" \"é\"" PASSES
   s" VALID? rejects a string over maxLength, one character" T-LABEL
   s\" {\"maxLength\":1}" s\" \"éé\""
   s\" : expected a string of at most 1 character, found `\"éé\"`; send a shorter string"
   FAILS
   s" VALID? takes a string the pattern matches" T-LABEL
   s\" {\"pattern\":\"^[a-z]+$\"}" s\" \"abc\"" PASSES
   s" VALID? rejects a string the pattern does not match" T-LABEL
   s\" {\"pattern\":\"^[a-z]+$\"}" s\" \"Abc\""
   s\" : expected a string the pattern `^[a-z]+$` matches, found `\"Abc\"`; send a string it matches"
   FAILS
   s" VALID? rejects any member when additionalProperties is false alone" T-LABEL
   s\" {\"additionalProperties\":false}" s\" {\"a\":1}"
   s" : expected no members, found `a`; remove it"
   FAILS
   s" VALID? shows a hidden character in a member name" T-LABEL
   s\" {\"additionalProperties\":false}" s\" {\"a\\u202eb\":1}"
   s" : expected no members, found `aU+202Eb`; remove it"
   FAILS
   s" VALID? names the one member properties allows" T-LABEL
   s\" {\"properties\":{\"filter\":{}},\"additionalProperties\":false}"
   s\" {\"filtre\":1}"
   s" : expected only the member `filter`, found `filtre`; rename it to `filter` or remove it"
   FAILS
   s" VALID? rejects members past maxProperties" T-LABEL
   s\" {\"maxProperties\":1}" s\" {\"a\":1,\"b\":2}"
   s" : expected at most 1 member, found 2 members; remove a member"
   FAILS
   s" VALID? cuts a long value at 32 bytes" T-LABEL
   s\" {\"type\":\"integer\"}" s\" \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\""
   s\" : expected a value of type `integer`, found `\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa...`; send a value of that type"
   FAILS
   s" VALID? cuts a long member name in the place" T-LABEL
   s\" {\"properties\":{\"abcdefghijklmnopqrstuvwxyz0123456789ABCD\":{\"type\":\"string\"}}}"
   s\" {\"abcdefghijklmnopqrstuvwxyz0123456789ABCD\":1}"
   s" .abcdefghijklmnopqrstuvwxyz012345...: expected a value of type `string`, found `1`; send a value of that type"
   FAILS ;

: STRINGS$ ( -- ^u8 n )
   s\" {\"type\":\"array\",\"items\":{\"type\":\"string\"}}" ;

: UNIQUE-STRINGS$ ( -- ^u8 n )
   s\" {\"type\":\"array\",\"items\":{\"type\":\"string\"},\"uniqueItems\":true}" ;

: ARRAYS ( -- )
   s" VALID? rejects an element items rejects, at its index" T-LABEL
   STRINGS$ s\" [\"a\",1]"
   s" [1]: expected a value of type `string`, found `1`; send a value of that type"
   FAILS
   s" VALID? places an element below a member" T-LABEL
   s\" {\"properties\":{\"tags\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}}}}"
   s\" {\"tags\":[1]}"
   s" .tags[0]: expected a value of type `string`, found `1`; send a value of that type"
   FAILS
   s" VALID? takes an array of minItems" T-LABEL
   s\" {\"minItems\":1}" s" [0]" PASSES
   s" VALID? rejects an array under minItems" T-LABEL
   s\" {\"minItems\":1}" s" []"
   s" : expected at least 1 item, found 0 items; add an item"
   FAILS
   s" VALID? takes an array of maxItems" T-LABEL
   s\" {\"maxItems\":2}" s" [0,1]" PASSES
   s" VALID? rejects an array over maxItems" T-LABEL
   s\" {\"maxItems\":2}" s" [0,1,2]"
   s" : expected at most 2 items, found 3 items; remove an item"
   FAILS
   s" VALID? reports a broken count after the array's path, at each level" T-LABEL
   s\" {\"properties\":{\"p\":{\"items\":{\"minItems\":2,\"items\":{\"maxItems\":0,\"items\":{\"type\":\"string\"}}}}}}"
   s\" {\"p\":[[[1]]]}"
   s" .p[0]: expected at least 2 items, found 1 item; add an item"
   FAILS
   s" VALID? takes elements that differ in kind or value" T-LABEL
   s\" {\"items\":{\"type\":[\"string\",\"number\",\"boolean\",\"null\"]},\"uniqueItems\":true}"
   s\" [\"1\",1,1.5,true,false,null,\"a\"]" PASSES
   s" VALID? rejects strings equal once decoded" T-LABEL
   UNIQUE-STRINGS$ s\" [\"a\",\"\\u0061\"]"
   s\" : expected items that differ, found `\"\\u0061\"` at [0] and [1]; remove one of the two"
   FAILS
   s" VALID? rejects numbers equal by value" T-LABEL
   s\" {\"items\":{\"type\":\"number\"},\"uniqueItems\":true}" s" [1,1.0]"
   s" : expected items that differ, found `1.0` at [0] and [1]; remove one of the two"
   FAILS
   s" VALID? reports the repeat whose second copy comes first" T-LABEL
   UNIQUE-STRINGS$ s\" [\"b\",\"a\",\"a\",\"b\"]"
   s\" : expected items that differ, found `\"a\"` at [1] and [2]; remove one of the two"
   FAILS
   s" VALID? rejects an exponent past a cell under uniqueItems" T-LABEL
   s\" {\"items\":{\"type\":\"number\"},\"uniqueItems\":true}"
   s" [1,1e99999999999999999999]"
   s" [1]: expected a number whose exponent a signed 64-bit cell holds, found `1e99999999999999999999`; write the number with a smaller exponent"
   FAILS ;

100000 constant DEPTH

: ADD-TIMES ( ^u8 n n -- ) {: s:^ u:n k:n :}
   k 0 ?do s u NESTED @ VEC:APPEND loop ;

\ o DEPTH times, m, then c DEPTH times; answers the vector's count after.
: NEST ( ^u8 n ^u8 n ^u8 n -- n ) {: o:^ ou:n m:^ mu:n c:^ cu:n :}
   o ou DEPTH ADD-TIMES  m mu 1 ADD-TIMES  c cu DEPTH ADD-TIMES
   NESTED @ VEC:COUNT ;

\ The schema, the value it takes and the two it rejects, one after another
\ in one vector: spans into it hold only once it stops growing.
: DEEP ( -- )
   s" COMPILE and VALID? walk a schema and a value 100000 levels deep" T-LABEL
   NESTED MAKE
   s\" {\"maxItems\":1,\"items\":" s\" {\"type\":\"string\"}" s" }" NEST {: a:n :}
   s" [" s\" \"a\"" s" ]" NEST {: b:n :}
   s" [" s" 1" s" ]" NEST {: c:n :}
   s" [" s" 1" s" ,0]" NEST drop
   NESTED @ VEC:SPAN SPAN:$ {: t:^ u:n :}
   t a  t a +  b a -  PASSES
   t a  t b +  c b -  VERDICT TFALSE
   VIOLATION$ {: w:^ wu:n :}
   w 6 s" [0][0]" T$=  wu 300073 T=
   t a  t c +  u c -
   s" : expected at most 1 item, found 2 items; remove an item" FAILS
   NESTED UNMAKE ;

: CHOICES ( -- )
   s" VALID? takes anyOf's second alternative after the first fails" T-LABEL
   s\" {\"anyOf\":[{\"type\":\"string\"},{\"type\":\"integer\"}]}" s" 5"
   PASSES
   s" VALID? takes a container's second anyOf alternative after the first fails deep" T-LABEL
   s\" {\"anyOf\":[{\"items\":{\"items\":{\"type\":\"string\"}}},{\"items\":{\"type\":\"array\"}}]}"
   s" [[1]]" PASSES
   s" VALID? lists each anyOf alternative's violation" T-LABEL
   s\" {\"anyOf\":[{\"type\":\"string\"},{\"type\":\"object\",\"required\":[\"a\"]}]}"
   s" {}"
   s\" : expected a value one schema of `anyOf` takes, found `{}`; meet one of them:\n  [0]: expected a value of type `string`, found `{}`; send a value of that type\n  [1]: expected a member `a`, found none; add `a`"
   FAILS
   s" VALID? places an anyOf member's alternatives below it" T-LABEL
   s\" {\"properties\":{\"x\":{\"anyOf\":[{\"type\":\"string\"},{\"properties\":{\"y\":{\"type\":\"integer\"}}}]}}}"
   s\" {\"x\":{\"y\":\"z\"}}"
   s\" .x: expected a value one schema of `anyOf` takes, found `{\"y\":\"z\"}`; meet one of them:\n  [0]: expected a value of type `string`, found `{\"y\":\"z\"}`; send a value of that type\n  [1].y: expected a value of type `integer`, found `\"z\"`; send a value of that type"
   FAILS
   s" VALID? lists each oneOf alternative's violation when none passes" T-LABEL
   s\" {\"oneOf\":[{\"type\":\"string\"},{\"type\":\"null\"}]}" s" 1"
   s\" : expected a value exactly one schema of `oneOf` takes, found `1`; meet one of them:\n  [0]: expected a value of type `string`, found `1`; send a value of that type\n  [1]: expected a value of type `null`, found `1`; send a value of that type"
   FAILS
   s" VALID? takes a value one oneOf alternative takes" T-LABEL
   s\" {\"oneOf\":[{\"type\":\"string\"},{\"type\":\"null\"}]}" s" null"
   PASSES
   s" VALID? names the oneOf alternatives that both take a value" T-LABEL
   s\" {\"oneOf\":[{\"type\":\"integer\"},{\"type\":\"number\"},{\"type\":\"string\"}]}"
   s" 1"
   s" : expected a value exactly one schema of `oneOf` takes, found `1`, which schemas [0] and [1] take; send a value only one of them takes"
   FAILS ;

: RUN ( -- )
   T-RESET
   TAKEN REJECTED
   VIOLATION MAKE
   SAPD VALUES ARRAYS DEEP CHOICES
   VIOLATION UNMAKE
   T-REPORT ;

RUN

;package

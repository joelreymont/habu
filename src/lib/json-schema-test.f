split-str
\ json-schema-test.f - schemas through JSON-SCHEMA:COMPILE: the shapes the
\ consumers register, and each rejected schema's text.
\ Run: bin/hb --load src/lib/json-schema-test.f

require src/lib/error.f
require src/lib/test.f
require src/lib/arena.f
require src/lib/reason.f
require src/lib/json-schema.f

package JSON-SCHEMA-TEST

: COMPILE-ONLY ( ^u8 n -- )
   [: JSON-SCHEMA:COMPILE drop ;] ARENA:WITH ;

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
   s" COMPILE rejects a keyword repeated in one schema at its second key" T-LABEL
   s\" {\"type\":\"string\",\"type\":\"integer\"}"
   s\" 1:18: expected each member once, found `\"type\"`; remove the repeated member, keeping one"
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
   s" COMPILE rejects an empty type array at its `]`" T-LABEL
   s\" {\"type\":[]}"
   s\" 1:10: expected `type` as an array of one type name or more, found `]`; name a type, or remove `type`"
   REJECTS
   s" COMPILE rejects a repeated type at its second copy" T-LABEL
   s\" {\"type\":[\"string\",\"null\",\"string\"]}"
   s\" 1:26: expected `type` naming each type once, found `\"string\"`; remove the repeated type name"
   REJECTS
   s" COMPILE rejects an empty enum at its `]`" T-LABEL
   s\" {\"enum\":[ ]}"
   s\" 1:11: expected `enum` as an array of one value or more, found `]`; list the values allowed in an array"
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
   s\" 1:28: expected a pattern the dialect takes, found pattern column 3: expected `)` to close a group, found the end of the pattern; close each `(` with `)`; rewrite the pattern as its text says"
   REJECTS
   s" COMPILE rejects properties that are no object" T-LABEL
   s\" {\"properties\":[\"a\"]}"
   s\" 1:15: expected `properties` as an object of schema objects, found `[`; write an object mapping each property name to its schema"
   REJECTS
   s" COMPILE rejects a property name repeated at its second copy" T-LABEL
   s\" {\"properties\":{\"a\":{},\"a\":{\"type\":\"string\"}}}"
   s\" 1:23: expected `properties` naming each property once, found `\"a\"`; remove the repeated property, keeping one"
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
   s" COMPILE rejects an empty anyOf at its `]`" T-LABEL
   s\" {\"anyOf\":[]}"
   s\" 1:11: expected `anyOf` as an array of one schema object or more, found `]`; list the alternatives' schemas in an array"
   REJECTS
   s" COMPILE rejects uniqueItems beside object items, at its `true`" T-LABEL
   s\" {\"type\":\"array\",\"uniqueItems\":true,\"items\":{\"type\":\"object\"}}"
   s\" 1:31: expected `uniqueItems` beside `items` whose `type` names scalar types only, found `true`; remove `uniqueItems`, or give `items` a `type` of scalars only"
   REJECTS
   s" COMPILE rejects text that ends inside the schema" T-LABEL
   s\" {\"type\":"
   s\" 1:9: expected a value, found the end of the input; write the rest of the document, closing each open array with `]` and object with `}`"
   REJECTS ;

: RUN ( -- )
   T-RESET
   TAKEN REJECTED
   T-REPORT ;

RUN

;package

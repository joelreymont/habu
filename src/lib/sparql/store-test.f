\ The store description's rejections through READ-STORE, each with its
\ place and fix.

require src/lib/test/assert.f
require src/lib/reason.f
require src/lib/sparql/store.f
require src/lib/vector.f
require src/lib/json/read.f

package SPARQL

: PART ( ^VEC:vector<u8> str -- ) {: out:^ text:str :}
   text out VEC:APPEND ;

: TEXT-OF ( ^VEC:vector<u8> -- str ) {: out:^ :}
   out 0 VEC:AT out VEC:COUNT >str ;

: NEW-TEXT ( ^ARENA:arena -- ^VEC:vector<u8> )
   ARENA:ALLOCATOR 0 [type] u8 VEC:NEW ;

: READ-REJECTS ( str str ^ARENA:arena -- )
   {: text:str want:str arena :}
   [: text arena READ-STORE drop ;] JSON:E-MEMBER TTHROWSQ
   JSON:E-MEMBER REASON:TEXT$ want T$=
   REASON:RELEASE ;

\ The source text inside a description with one shape, `id`.
: SOURCE-REJECTS ( str str ^ARENA:arena -- )
   {: text:str want:str arena :}
   arena NEW-TEXT {: out:^ :}
   out s\" {\"bytes\": 1, \"shapes\": {\"id\": {\"in\": \"i\"," PART
   out s\" \"out\": \"o\"}}, \"sources\": [" PART
   out text PART  out s" ]}" PART
   out TEXT-OF want arena READ-REJECTS ;

: TEST-STORE-REJECTS ( ^ARENA:arena -- ) {: arena :}
   s" a missing member is rejected by JSON:GET" T-LABEL
   s\" {\"shapes\": {}, \"sources\": []}"
   s" 1:1: expected a member `bytes`, found none; add the member to the object"
   arena READ-REJECTS
   s" bytes is a whole number above 0" T-LABEL
   s\" {\"bytes\": 0, \"shapes\": {}, \"sources\": []}"
   s" 1:11: expected a whole number of bytes above 0, found `0`; give the largest answer in bytes"
   arena READ-REJECTS
   s\" {\"bytes\": \"1\", \"shapes\": {}, \"sources\": []}"
   s\" 1:11: expected a whole number of bytes above 0, found `\"1\"`; give the largest answer in bytes"
   arena READ-REJECTS
   s" a shape's in is a string" T-LABEL
   s\" {\"bytes\": 1, \"shapes\": {\"id\": {\"in\": 1, \"out\": \"o\"}}, \"sources\": []}"
   s" 1:38: expected a string, found `1`; write the value as a JSON string"
   arena READ-REJECTS ;

\ The shapes text inside a description with no sources.
: SHAPES-REJECT ( str str ^ARENA:arena -- )
   {: text:str want:str arena :}
   arena NEW-TEXT {: out:^ :}
   out s\" {\"bytes\": 1, \"shapes\": {" PART
   out text PART  out s\" }, \"sources\": []}" PART
   out TEXT-OF want arena READ-REJECTS ;

: TEST-HOLDS-REJECTS ( ^ARENA:arena -- ) {: arena :}
   s" holds is uri or an array of datatype IRIs" T-LABEL
   s\" \"id\": {\"in\": \"i\", \"out\": \"o\", \"holds\": \"iri\"}"
   s\" 1:64: expected `uri` or an array of datatype IRIs, found `\"iri\"`; write \"uri\" or list the datatypes the shape holds"
   arena SHAPES-REJECT ;

: TEST-WITHIN-REJECTS ( ^ARENA:arena -- ) {: arena :}
   s" a part is within a declared shape" T-LABEL
   s\" \"p\": {\"within\": \"no\"}"
   s\" 1:41: expected the name of a shape in `shapes`, found `\"no\"`; declare the shape in `shapes` or name a declared one"
   arena SHAPES-REJECT
   s" a chain of within ends" T-LABEL
   s\" \"p\": {\"within\": \"q\"}, \"q\": {\"within\": \"p\"}"
   s\" 1:41: expected the name of a shape whose chain of `within` ends, found `\"q\"`; break the cycle of `within` the name leads into"
   arena SHAPES-REJECT
   s\" \"p\": {\"within\": \"p\"}"
   s\" 1:41: expected the name of a shape whose chain of `within` ends, found `\"p\"`; break the cycle of `within` the name leads into"
   arena SHAPES-REJECT ;

\ Parts named before their parents.
: TEST-NESTED ( ^ARENA:arena -- ) {: arena :}
   s" a part may be within a part" T-LABEL
   s\" {\"bytes\": 1, \"shapes\": {\"a\": {\"within\": \"b\"}, \"b\": {\"within\": \"c\"}, \"c\": {\"in\": \"i\", \"out\": \"o\", \"type\": \"t\"}}, \"sources\": []}"
   arena READ-STORE STORE-SHAPES @ {: shapes:^ :}
   shapes 0 VEC:AT {: a:^ :}
   a SHAPE-PARENT @ 1 T=  a SHAPE-ROOT @ 2 T=
   a SHAPE-IN @ s" i" T$=  a SHAPE-OUT @ s" o" T$=
   a SHAPE-TYPE @ s" t" T$=
   shapes 1 VEC:AT SHAPE-ROOT @ 2 T=
   shapes 2 VEC:AT SHAPE-PARENT @ -1 T= ;

: TEST-SLOT-REJECTS ( ^ARENA:arena -- ) {: arena :}
   s" a source's from is a string" T-LABEL
   s\" {\"from\": 1}"
   s" 1:77: expected a string, found `1`; write the value as a JSON string"
   arena SOURCE-REJECTS
   s" a slot has one of col, term and cols" T-LABEL
   s\" {\"from\": \"f\", \"graph\": null, \"s\": {}}"
   s" 1:102: expected one of `col`, `term` and `cols`, found `{`; give a column with `col` and `shape`, a constant `term`, or columns in `cols`"
   arena SOURCE-REJECTS
   s\" {\"from\": \"f\", \"graph\": null, \"s\": {\"col\": \"c\", \"shape\": \"id\", \"term\": 1}}"
   s" 1:102: expected one of `col`, `term` and `cols`, found `{`; give a column with `col` and `shape`, a constant `term`, or columns in `cols`"
   arena SOURCE-REJECTS
   s" a column has a shape" T-LABEL
   s\" {\"from\": \"f\", \"graph\": null, \"s\": {\"col\": \"c\"}}"
   s" 1:102: expected a `shape` beside `col`, found `{`; name the column's shape in `shape`"
   arena SOURCE-REJECTS
   s" a column is a string" T-LABEL
   s\" {\"from\": \"f\", \"graph\": null, \"s\": {\"col\": 1, \"shape\": \"id\"}}"
   s" 1:110: expected a string, found `1`; write the value as a JSON string"
   arena SOURCE-REJECTS
   s" a slot's shape is declared" T-LABEL
   s\" {\"from\": \"f\", \"graph\": null, \"s\": {\"col\": \"c\", \"shape\": \"no\"}}"
   s\" 1:124: expected the name of a shape in `shapes`, found `\"no\"`; declare the shape in `shapes` or name a declared one"
   arena SOURCE-REJECTS
   s" a slot's unique is a boolean" T-LABEL
   s\" {\"from\": \"f\", \"graph\": null, \"s\": {\"col\": \"c\", \"shape\": \"id\", \"unique\": 1}}"
   s" 1:140: expected true or false, found `1`; write the value as a JSON boolean"
   arena SOURCE-REJECTS
   s" a slot's notnull is a boolean" T-LABEL
   s\" {\"from\": \"f\", \"graph\": null, \"s\": {\"col\": \"c\", \"shape\": \"id\", \"notnull\": 1}}"
   s" 1:141: expected true or false, found `1`; write the value as a JSON boolean"
   arena SOURCE-REJECTS
   s" a slot is an object" T-LABEL
   s\" {\"from\": \"f\", \"graph\": null, \"s\": 1}"
   s" 1:102: expected an object, found `1`; write it as a JSON object"
   arena SOURCE-REJECTS ;

: TEST-COLS-REJECTS ( ^ARENA:arena -- ) {: arena :}
   s" cols has no col or term beside it" T-LABEL
   s\" {\"from\": \"f\", \"graph\": null, \"s\": {\"col\": \"c\", \"cols\": []}}"
   s\" 1:110: expected no `col` or `term` beside `cols`, found `\"c\"`; remove it: `cols` names each column"
   arena SOURCE-REJECTS
   s" cols has no key beside it" T-LABEL
   s\" {\"from\": \"f\", \"graph\": null, \"s\": {\"cols\": [], \"key\": 1}}"
   s" 1:122: expected no `key` beside `cols`, found `1`; remove `key`: a key belongs on a column slot"
   arena SOURCE-REJECTS
   s" cols has no unique beside it" T-LABEL
   s\" {\"from\": \"f\", \"graph\": null, \"s\": {\"cols\": [], \"unique\": true}}"
   s" 1:125: expected no `unique` beside `cols`, found `true`; remove `unique`: it belongs on a column slot or a column of `cols`"
   arena SOURCE-REJECTS
   s" cols has no notnull beside it" T-LABEL
   s\" {\"from\": \"f\", \"graph\": null, \"s\": {\"cols\": [], \"notnull\": true}}"
   s" 1:126: expected no `notnull` beside `cols`, found `true`; remove `notnull`: it belongs on a column slot"
   arena SOURCE-REJECTS
   s" cols is an array of two columns or more" T-LABEL
   s\" {\"from\": \"f\", \"graph\": null, \"s\": {\"cols\": 1}}"
   s" 1:111: expected an array of columns, found `1`; list the columns as objects with `col` and `shape`"
   arena SOURCE-REJECTS
   s\" {\"from\": \"f\", \"graph\": null, \"s\": {\"cols\": [{\"col\": \"c\", \"shape\": \"id\"}]}}"
   s" 1:111: expected at least two columns, found `[`; give one column as a slot with `col` and `shape`"
   arena SOURCE-REJECTS
   s" a column of cols has col and shape" T-LABEL
   s\" {\"from\": \"f\", \"graph\": null, \"s\": {\"cols\": [{\"col\": \"c\"}, 1]}}"
   s\" 1:112: expected a column with `col` and `shape`, found `{`; write the column as {\"col\": c, \"shape\": name}"
   arena SOURCE-REJECTS
   s" a column of cols has no key" T-LABEL
   s\" {\"from\": \"f\", \"graph\": null, \"s\": {\"cols\": [{\"col\": \"c\", \"shape\": \"id\", \"key\": 1}, 1]}}"
   s" 1:147: expected no `key` in a column of `cols`, found `1`; remove `key`: a key belongs on a column slot"
   arena SOURCE-REJECTS
   s" a column of cols has no notnull" T-LABEL
   s\" {\"from\": \"f\", \"graph\": null, \"s\": {\"cols\": [{\"col\": \"c\", \"shape\": \"id\", \"notnull\": true}, 1]}}"
   s" 1:151: expected no `notnull` in a column of `cols`, found `true`; remove `notnull`: a column of `cols` is NULL where another holds the value"
   arena SOURCE-REJECTS
   s" a column slot has no only" T-LABEL
   s\" {\"from\": \"f\", \"graph\": null, \"s\": {\"col\": \"c\", \"shape\": \"id\", \"only\": 1}}"
   s" 1:138: expected no `only` outside a column of `cols`, found `1`; remove `only`: it belongs on a column of `cols`"
   arena SOURCE-REJECTS
   s" cols has no only beside it" T-LABEL
   s\" {\"from\": \"f\", \"graph\": null, \"s\": {\"cols\": [], \"only\": 1}}"
   s" 1:123: expected no `only` beside `cols`, found `1`; remove `only`: it belongs on a column of `cols`"
   arena SOURCE-REJECTS
   s" a column's only names a declared key set" T-LABEL
   s\" {\"bytes\": 1, \"shapes\": {\"id\": {\"in\": \"i\", \"out\": \"o\"}}, \"keys\": {\"k\": {\"type\": \"t\", \"values\": {}}}, \"sources\": [{\"from\": \"f\", \"graph\": null, \"s\": {\"cols\": [{\"col\": \"a\", \"shape\": \"id\", \"only\": {\"col\": \"k\", \"set\": \"no\"}}, {\"col\": \"b\", \"shape\": \"id\"}]}}]}"
   s\" 1:213: expected the name of a key set in `keys`, found `\"no\"`; declare the key set in `keys` or name a declared one"
   arena READ-REJECTS ;

: TEST-NOTNULL ( ^ARENA:arena -- ) {: arena :}
   s" a column's notnull is read" T-LABEL
   s\" {\"bytes\": 1, \"shapes\": {\"id\": {\"in\": \"i\", \"out\": \"o\"}}, \"sources\": [{\"from\": \"f\", \"graph\": null, \"s\": {\"col\": \"c\", \"shape\": \"id\", \"notnull\": true}, \"p\": {\"col\": \"p\", \"shape\": \"id\"}, \"o\": {\"col\": \"o\", \"shape\": \"id\", \"notnull\": false}}]}"
   arena READ-STORE STORE-SOURCES @ 0 VEC:AT {: src:^ :}
   src SOURCE-S @ SLOT-NOTNULL? TTRUE
   src SOURCE-O @ SLOT-NOTNULL? TFALSE ;

: TEST-TERM-REJECTS ( ^ARENA:arena -- ) {: arena :}
   s" a graph is null or a uri term" T-LABEL
   s\" {\"from\": \"f\", \"graph\": 1}"
   s" 1:91: expected null or a term of type `uri`, found `1`; write null for the default graph or a uri term"
   arena SOURCE-REJECTS
   s\" {\"from\": \"f\", \"graph\": {\"type\": \"literal\", \"value\": \"g\"}}"
   s" 1:91: expected null or a term of type `uri`, found `{`; write null for the default graph or a uri term"
   arena SOURCE-REJECTS
   s" a term is a uri or a literal" T-LABEL
   s\" {\"from\": \"f\", \"graph\": {\"type\": \"bnode\", \"value\": \"g\"}}"
   s\" 1:100: expected `uri` or `literal`, found `\"bnode\"`; write a uri or a literal term"
   arena SOURCE-REJECTS
   s" a term's value is a string" T-LABEL
   s\" {\"from\": \"f\", \"graph\": {\"type\": \"uri\", \"value\": 1}}"
   s" 1:116: expected a string, found `1`; write the value as a JSON string"
   arena SOURCE-REJECTS
   s" a term's string holds no U+0000" T-LABEL
   s\" {\"from\": \"f\", \"graph\": {\"type\": \"uri\", \"value\": \"a\\u0000\"}}"
   s\" 1:116: expected a string without U+0000, found `\"a\\u0000\"`; remove U+0000 from the string: PostgreSQL text cannot hold it"
   arena SOURCE-REJECTS
   s" a literal has one of datatype and xml:lang" T-LABEL
   s\" {\"from\": \"f\", \"graph\": null, \"s\": {\"term\": {\"type\": \"literal\", \"value\": \"v\", \"datatype\": \"d\", \"xml:lang\": \"en\"}}}"
   s" 1:111: expected a literal with one of `datatype` and `xml:lang`, found `{`; remove one of the two"
   arena SOURCE-REJECTS
   s" a literal's datatype is a string" T-LABEL
   s\" {\"from\": \"f\", \"graph\": null, \"s\": {\"term\": {\"type\": \"literal\", \"value\": \"v\", \"datatype\": 1}}}"
   s" 1:157: expected a string, found `1`; write the value as a JSON string"
   arena SOURCE-REJECTS
   s" a literal's datatype is not xsd:string" T-LABEL
   s\" {\"from\": \"f\", \"graph\": null, \"s\": {\"term\": {\"type\": \"literal\", \"value\": \"v\", \"datatype\": \"http://www.w3.org/2001/XMLSchema#string\"}}}"
   s\" 1:157: expected a datatype other than xsd:string, found `\"http://www.w3.org/2001/XMLSchem...`; remove `datatype`: a plain literal is an xsd:string"
   arena SOURCE-REJECTS
   s" a literal's xml:lang is in lower case" T-LABEL
   s\" {\"from\": \"f\", \"graph\": null, \"s\": {\"term\": {\"type\": \"literal\", \"value\": \"v\", \"xml:lang\": \"EN\"}}}"
   s\" 1:157: expected a language tag in lower case, found `\"EN\"`; write it in lower case"
   arena SOURCE-REJECTS
   s" a literal's datatype is not rdf:langString" T-LABEL
   s\" {\"from\": \"f\", \"graph\": null, \"s\": {\"term\": {\"type\": \"literal\", \"value\": \"v\", \"datatype\": \"http://www.w3.org/1999/02/22-rdf-syntax-ns#langString\"}}}"
   s\" 1:157: expected a datatype other than rdf:langString, found `\"http://www.w3.org/1999/02/22-rd...`; replace `datatype` with `xml:lang`: a literal with a language has no datatype"
   arena SOURCE-REJECTS
   s" a literal's xml:lang is a language tag" T-LABEL
   s\" {\"from\": \"f\", \"graph\": null, \"s\": {\"term\": {\"type\": \"literal\", \"value\": \"v\", \"xml:lang\": \"e1-us\"}}}"
   s\" 1:157: expected a language tag: letters, then letters or digits after each -, found `\"e1-us\"`; write a tag such as en or en-gb"
   arena SOURCE-REJECTS
   s\" {\"from\": \"f\", \"graph\": null, \"s\": {\"term\": {\"type\": \"literal\", \"value\": \"v\", \"xml:lang\": \"en-\"}}}"
   s\" 1:157: expected a language tag: letters, then letters or digits after each -, found `\"en-\"`; write a tag such as en or en-gb"
   arena SOURCE-REJECTS
   s" a uri has no datatype or xml:lang" T-LABEL
   s\" {\"from\": \"f\", \"graph\": null, \"s\": {\"term\": {\"type\": \"uri\", \"value\": \"v\", \"datatype\": \"d\"}}}"
   s\" 1:153: expected no `datatype` or `xml:lang` on a uri, found `\"d\"`; remove it: a literal has one, a uri none"
   arena SOURCE-REJECTS
   s" a term has no other member" T-LABEL
   s\" {\"from\": \"f\", \"graph\": {\"type\": \"uri\", \"value\": \"v\", \"note\": \"x\"}}"
   s\" 1:121: expected `type`, `value`, `datatype` or `xml:lang`, found `\"note\"`; remove the member: a term has no other"
   arena SOURCE-REJECTS ;

: TEST-KEY-REJECTS ( ^ARENA:arena -- ) {: arena :}
   s" keys is an object" T-LABEL
   s\" {\"bytes\": 1, \"shapes\": {}, \"keys\": 1, \"sources\": []}"
   s" 1:36: expected an object of key sets, found `1`; name each key set in `keys`"
   arena READ-REJECTS
   s" a key set is an object" T-LABEL
   s\" {\"bytes\": 1, \"shapes\": {}, \"keys\": {\"k\": 1}, \"sources\": []}"
   s" 1:42: expected a key set, found `1`; give the key set as an object with `type` and `values`"
   arena READ-REJECTS
   s" a key set has a type and values" T-LABEL
   s\" {\"bytes\": 1, \"shapes\": {}, \"keys\": {\"k\": {\"values\": {}}}, \"sources\": []}"
   s" 1:42: expected a member `type`, found none; add the member to the object"
   arena READ-REJECTS
   s\" {\"bytes\": 1, \"shapes\": {}, \"keys\": {\"k\": {\"type\": \"t\"}}, \"sources\": []}"
   s" 1:42: expected a member `values`, found none; add the member to the object"
   arena READ-REJECTS
   s" a key set's values is an object of strings" T-LABEL
   s\" {\"bytes\": 1, \"shapes\": {}, \"keys\": {\"k\": {\"type\": \"t\", \"values\": 1}}, \"sources\": []}"
   s" 1:66: expected an object of IRIs and their keys, found `1`; map each IRI to the SQL array literal of its keys"
   arena READ-REJECTS
   s\" {\"bytes\": 1, \"shapes\": {}, \"keys\": {\"k\": {\"type\": \"t\", \"values\": {\"urn:a\": 1}}}, \"sources\": []}"
   s" 1:76: expected a string, found `1`; write the value as a JSON string"
   arena READ-REJECTS
   s\" {\"bytes\": 1, \"shapes\": {}, \"keys\": {\"k\": {\"type\": \"t\", \"values\": {\"urn:a\": \"{\\u0000}\"}}}, \"sources\": []}"
   s\" 1:76: expected a string without U+0000, found `\"{\\u0000}\"`; remove U+0000 from the string: PostgreSQL text cannot hold it"
   arena READ-REJECTS
   s" a key's set is declared" T-LABEL
   s\" {\"bytes\": 1, \"shapes\": {\"id\": {\"in\": \"i\", \"out\": \"o\"}}, \"keys\": {\"k\": {\"type\": \"t\", \"values\": {}}}, \"sources\": [{\"from\": \"f\", \"graph\": null, \"s\": {\"col\": \"c\", \"shape\": \"id\", \"key\": {\"col\": \"k\", \"set\": \"no\"}}}]}"
   s\" 1:202: expected the name of a key set in `keys`, found `\"no\"`; declare the key set in `keys` or name a declared one"
   arena READ-REJECTS ;

: TEST-STORE ( -- )
   [: TEST-STORE-REJECTS ;] ARENA:WITH
   [: TEST-HOLDS-REJECTS ;] ARENA:WITH
   [: TEST-WITHIN-REJECTS ;] ARENA:WITH
   [: TEST-NESTED ;] ARENA:WITH
   [: TEST-SLOT-REJECTS ;] ARENA:WITH
   [: TEST-COLS-REJECTS ;] ARENA:WITH
   [: TEST-NOTNULL ;] ARENA:WITH
   [: TEST-TERM-REJECTS ;] ARENA:WITH
   [: TEST-KEY-REJECTS ;] ARENA:WITH ;

TEST-STORE

;package

T-REPORT

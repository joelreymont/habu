split-str
\ json-schema.f - JSON Schema, package JSON-SCHEMA: COMPILE reads a schema, a
\ JSON:value, into nodes that state what a JSON value must be; VALID? checks a
\ value against them.
\
\ CONTRACT. COMPILE takes one schema object of the 2020-12 dialect. Its
\ keywords with a rule are type, enum, minimum, maximum, minLength,
\ maxLength, pattern, properties, required, additionalProperties,
\ maxProperties, items, minItems, maxItems, uniqueItems, anyOf and oneOf;
\ description, title, default, examples, deprecated, readOnly, writeOnly,
\ $comment and format are read past. `$schema` stands only at the root and
\ names https://json-schema.org/draft/2020-12/schema. A sub-schema is an
\ object; additionalProperties may also be true or false. uniqueItems is
\ true only beside an `items` whose `type` names scalar types only. Another
\ keyword, a value its keyword does not take, or a bound or enum number whose
\ exponent a signed cell does not hold throws E-SCHEMA through REASON:THROW
\ with the text `L:C: expected X, found Y; fix`, line and byte column in the
\ text the schema lies in. COMPILE makes no nested call per sub-schema:
\ JSON:WALK gives the schema's events in one pass, and each open schema
\ object is a level on a stack in the scratch arena, so memory, not the
\ machine stack, bounds a schema's depth.
\ VALID? ( str schema ^VEC:vector<u8> -- bool ) answers whether a JSON value text the
\ reader admits satisfies the schema, and false appends the first
\ violation, `P: expected X, found Y; fix` with P `.name` per member and
\ `[i]` per array element below the root, to the vector; text the reader
\ does not admit, or memory that cannot be had, throws its code, leaving the
\ vector as it was. Type integer takes a whole number a signed cell holds,
\ whatever its spelling; a larger one is of type number only. A failed anyOf
\ or oneOf lists each alternative's violation on a line of its own, `[i]` and
\ the violation, its place below the value's.
\ VALID? makes no nested call per value: one reader passes over the text,
\ and each value being judged is a place on a stack in the scratch arena,
\ so memory, not the machine stack, bounds a value's depth. A place judges
\ type, enum and its kind's rules, pushes a place for each member or
\ element a schema judges, then each anyOf and oneOf alternative, rooted, on
\ its value read again from its first token; it closes with the reader on
\ its value's last token, whatever its verdict, which the place below
\ reads. enum reads a container to its end before its members, then again;
\ uniqueItems reads its array again at the `]`. A container's violation
\ that quotes it is written once its closing token is read.
\ STORAGE CLASS. CALLER-OWNED: the nodes, their name maps, the decoded names
\ and each pattern's decoded text and program lie in the arena COMPILE was
\ given, read-only after COMPILE; they borrow the schema text, which must
\ outlive the schema. VALID?'s scratch is one ARENA:WITH, so calls on several
\ threads at once are safe.

require src/lib/error.f
require src/lib/string.f
require src/lib/span.f
require src/lib/fmt.f
require src/lib/adt/option.f
require src/lib/arena.f
require src/lib/reason.f
require src/lib/json.f
require src/lib/sort.f
require src/lib/map.f
require src/lib/regex.f
require src/lib/vector.f
require src/lib/utf8-scalar.f
require src/core/rejection.f
require src/lib/unicode/visible.f

package JSON-SCHEMA
private

-9910 constant E-FIRST
-9919 constant E-LAST

public

-9910 s" a JSON Schema the checker does not take: correct it where its reason text says"
ERROR:CODE E-SCHEMA

private

\ How a node takes the members its properties do not name: any, none, or
\ each judged by the node `additional`.
ENUM extras open closed ruled ;ENUM

STRUCTURE entry DERIVE addr
   FIELD next ^entry
   FIELD name SPAN:span<u8>
;STRUCTURE

\ types: bit i for TYPE-NAMES$'s i-th name, 0 for none; type-order: each i + 1
\ in the schema's order, 4 bits apiece. An empty span, -1 or null is no rule.
\ property-map: each property's name to its address; required-map: each
\ required name to its index in required. A map is null until its first name.
MUTUAL node property branch
STRUCTURE node DERIVE addr
   FIELD types n
   FIELD type-order n
   FIELD values SPAN:span<u8>
   FIELD minimum SPAN:span<u8>
   FIELD maximum SPAN:span<u8>
   FIELD min-length n
   FIELD max-length n
   FIELD max-properties n
   FIELD min-items n
   FIELD max-items n
   FIELD patterned bool
   FIELD pattern REGEX:regex
   FIELD pattern-text SPAN:span<u8>
   FIELD properties ^property
   FIELD property-map ^n
   FIELD required ^entry
   FIELD required-map ^n
   FIELD extras extras
   FIELD additional ^node
   FIELD items ^node
   FIELD unique bool
   FIELD any-of ^branch
   FIELD one-of ^branch
;STRUCTURE
STRUCTURE property DERIVE addr
   FIELD next ^property
   FIELD name SPAN:span<u8>
   FIELD node ^node
;STRUCTURE
STRUCTURE branch DERIVE addr
   FIELD next ^branch
   FIELD node ^node
;STRUCTURE
;MUTUAL

public

STRUCTURE schema OPAQUE
   FIELD root ^node
;STRUCTURE

private

\ What a level reads next: a keyword's name or value, a `properties` member's
\ name or schema, or an element of its anyOf or oneOf array.
ENUM reading keywords value properties member choice ;ENUM

\ build: one COMPILE call. top is the innermost open level, null outside the
\ root; skip counts the containers open in a value judged whole at its close.
\ level: one open schema object. Root indexes: base of names' first name,
\ keyword of the keyword or choice being read. name's schema is read next.
MUTUAL build level
STRUCTURE build DERIVE addr
   FIELD arena ^ARENA:arena
   FIELD scratch ^ARENA:arena
   FIELD root ^node
   FIELD top ^level
   FIELD skip n
;STRUCTURE
STRUCTURE level DERIVE addr
   FIELD build ^build
   FIELD outer ^level
   FIELD node ^node
   FIELD property-tail ^^property
   FIELD entry-tail ^^entry
   FIELD branch-tail ^^branch
   FIELD phase reading
   FIELD names SPAN:span<u8>
   FIELD base n
   FIELD keyword n
   FIELD name SPAN:span<u8>
;STRUCTURE
;MUTUAL

ARENA NODES node
ARENA PROPERTIES property
ARENA BRANCHES branch
ARENA ENTRIES entry
ARENA BUILDS build
ARENA LEVELS level

\ ---- rejections ------------------------------------------------------------

\ Rejects the value at its first byte: JSON:REJECT's E-MEMBER text leaves as
\ E-SCHEMA.
: REJECT ( JSON:value str str -- )
   {: v ea:^ eu:n fa:^ fu:n :}
   [: v ea eu fa fu JSON:REJECT ;] catch {: code:n :}
   code JSON:E-MEMBER = if
      JSON:E-MEMBER REASON:TEXT$ E-SCHEMA REASON:THROW
   then
   code throw ;

: PUT ( ^u8 str -- ^u8 ) {: dst:^ a:^ u:n :}
   a dst u BYTE-COPY  dst u + ;

: JOIN ( str str ^ARENA:arena -- str ) {: a:^ u:n b:^ bu:n s:^ :}
   s u bu + 1 max ARENA:ALLOC drop {: dst:^ :}
   dst a u PUT b bu PUT drop
   dst u bu + ;

\ Puts the blank-separated list's name at start in backticks, after a
\ comma unless it is the first; answers the next name's start.
: PUT-NAME ( ^u8 n str -- ^u8 n ) {: dst:^ start:n a:^ u:n :}
   a u STR:SPACE start STR:SPLIT-NEXT drop {: na:^ nu:n next:n :}
   dst  start 0<> if s" , " PUT then
   s" `" PUT na nu PUT s" `" PUT  next ;

\ "one of `a`, `b`" for the list's names, in the arena.
: ONE-OF ( str ^ARENA:arena -- str ) {: a:^ u:n s:^ :}
   s u 3 * 16 + ARENA:ALLOC drop {: dst:^ :}
   dst s" one of " PUT
   0 begin dup u < while a u PUT-NAME repeat drop
   dst - dst swap ;

\ found, or idx when the list's name at start is w; the next start and idx.
: NAME-STEP ( n n n str str -- n n n )
   {: found:n start:n idx:n w:^ wu:n a:^ u:n :}
   a u STR:SPACE start STR:SPLIT-NEXT drop {: na:^ nu:n next:n :}
   found 0<  w wu na nu STR:=  and if idx else found then
   next idx 1+ ;

\ The index of name w in the blank-separated list, or -1.
: NAME-INDEX ( str str -- n ) {: w:^ wu:n a:^ u:n :}
   -1 0 0 begin over u < while w wu a u NAME-STEP repeat
   2drop ;

\ ---- values ----------------------------------------------------------------

: COPIED ( ^ARENA:arena str -- str ) {: a:^ t:^ u:n :}
   a u 1 max ARENA:ALLOC drop {: dst:^ :}
   dst t u PUT drop  dst u ;

\ The string's decoded text, in the build's arena.
: DECODED ( JSON:value ^build -- str ) {: v b:^ :}
   b BUILD-ARENA @ v [: COPIED ;] JSON:WITH-STRING ;

\ The number's lexeme; e is the expected text for another value.
: LEXEME ( JSON:value str -- SPAN:span<u8> ) {: v e:^ eu:n :}
   v JSON:NUMBER? 0= if
      v e eu s" write a number, with no quotes" REJECT
   then
   v JSON:TEXT$ SPAN:MAKE ;

\ A bound's lexeme, as LEXEME; ex is the expected text for one whose
\ exponent NUMBER-COMPARE cannot order.
: BOUND-LEXEME ( JSON:value str str -- SPAN:span<u8> )
   {: v e:^ eu:n ex:^ exu:n :}
   v e eu LEXEME {: b :}
   b SPAN:$ 2dup JSON:NUMBER-COMPARE MATCH option
      none OF
         v ex exu s" write the bound with a smaller exponent" REJECT
      ENDOF
      some OF drop ENDOF
   ;MATCH b ;

\ The integer, from 0 to the largest a cell holds, written with no fraction
\ or exponent; e is the expected text for another value.
: LIMIT ( JSON:value str -- n ) {: v e:^ eu:n :}
   v JSON:NUMBER? if
      v JSON:TEXT$ STR:>NUMBER? MATCH option
         none OF -1 ENDOF
         some OF ENDOF
      ;MATCH
   else -1 then
   dup 0 >= if exit then
   drop v e eu s" write an integer in that range" REJECT ;

: DIALECT$ ( -- str ) s" https://json-schema.org/draft/2020-12/schema" ;

: DIALECT ( JSON:value -- ) {: v :}
   v DIALECT$ JSON:STRING= if exit then
   v s" `$schema` as `https://json-schema.org/draft/2020-12/schema`"
   s" name the 2020-12 dialect, or remove `$schema`" REJECT ;

: TYPE-NAMES$ ( -- str )
   s" null boolean object array number string integer" ;
\ The bits of null, boolean, number, string and integer.
$73 constant SCALARS

\ The bit of the type the name names, 0 for none.
: TYPE-BIT ( str -- n )
   TYPE-NAMES$ NAME-INDEX dup 0< if drop 0 exit then
   1 swap lshift ;

\ The bit of the type the value names.
: NAMED-TYPE ( JSON:value -- n ) {: v :}
   v JSON:STRING? if v [: TYPE-BIT ;] JSON:WITH-STRING else 0 then
   dup 0<> if exit then
   drop v
   s" `type` as one of `null`, `boolean`, `object`, `array`, `number`, `string` and `integer`"
   s" write one of those type names" REJECT ;

: BIT-INDEX ( n -- n )
   0 begin over 1 and 0= while 1+ swap 1 rshift swap repeat nip ;

: ADD-ORDER ( n ^node -- ) {: bit:n nd:^ :}
   nd NODE-TYPE-ORDER @ {: o:n :}
   0 begin o over rshift 0<> while 4 + repeat {: at:n :}
   bit BIT-INDEX 1+ at lshift o or nd NODE-TYPE-ORDER ! ;

: ADD-TYPE ( JSON:value ^node -- ) {: v nd:^ :}
   v NAMED-TYPE {: bit:n :}
   nd NODE-TYPES @ bit and 0<> if
      v s" `type` naming each type once" s" remove the repeated type name"
      REJECT
   then
   nd NODE-TYPES @ bit or nd NODE-TYPES !
   bit nd ADD-ORDER ;

: TYPE-RULE ( JSON:value ^node -- ) {: v nd:^ :}
   v JSON:ARRAY? 0= if v nd ADD-TYPE exit then
   v [: nd ADD-TYPE ;] JSON:EACH
   nd NODE-TYPES @ 0<> if exit then
   v s" `type` as an array of one type name or more"
   s" name a type, or remove `type`" REJECT ;

\ Whether an array's text holds only white space between its brackets.
: EMPTY? ( str -- bool ) {: a:^ u:n :}
   u 1- 1 ?do
      a i + c@ STR:SPACE > if unloop false exit then
   loop true ;

: ENUM-FIX$ ( -- str ) s" list the values allowed in an array" ;

\ Rejects a number whose exponent NUMBER-COMPARE cannot order.
: ENUM-NUMBER ( JSON:value -- ) {: v :}
   v JSON:NUMBER? 0= if exit then
   v JSON:TEXT$ 2dup JSON:NUMBER-COMPARE MATCH option
      none OF
         v s" `enum` values whose exponents a signed 64-bit cell holds"
         s" write each number with a smaller exponent" REJECT
      ENDOF
      some OF drop ENDOF
   ;MATCH ;

: DROP-VALUE ( JSON:value -- ) {: v :} ;

: ENUM-EVENT ( JSON:event -- bool ) {: e :}
   e MATCH JSON:event
      open-object OF ENDOF
      open-array OF ENDOF
      name OF DROP-VALUE ENDOF
      scalar OF ENUM-NUMBER ENDOF
      close OF DROP-VALUE ENDOF
   ;MATCH true ;

: ENUM-EXPECTED$ ( -- str ) s" `enum` as an array of one value or more" ;

: ENUM-RULE ( JSON:value ^node -- ) {: v nd:^ :}
   v JSON:ARRAY? 0= if v ENUM-EXPECTED$ ENUM-FIX$ REJECT then
   v [: ENUM-EVENT ;] JSON:WALK
   v JSON:TEXT$ EMPTY? if v ENUM-EXPECTED$ ENUM-FIX$ REJECT then
   v JSON:TEXT$ SPAN:MAKE nd NODE-VALUES ! ;

\ Rejects the pattern with REGEX's text before the fix.
: BAD-PATTERN ( JSON:value ^build -- ) {: v b:^ :}
   b BUILD-SCRATCH @ {: s:^ :}
   s" pattern column " REGEX:E-PATTERN REASON:TEXT$ s JOIN
   s" ; rewrite the pattern as its text says" s JOIN {: fx:^ fu:n :}
   v s" a pattern the dialect takes" fx fu REJECT ;

: PATTERN-RULE ( JSON:value ^level -- ) {: v lv:^ :}
   v JSON:STRING? 0= if
      v s" `pattern` as a string" s" write the pattern as a JSON string"
      REJECT
   then
   lv LEVEL-BUILD @ {: b:^ :}  lv LEVEL-NODE @ {: nd:^ :}
   v b DECODED {: p:^ pu:n :}
   p pu SPAN:MAKE nd NODE-PATTERN-TEXT !  true nd NODE-PATTERNED !
   [: p pu b BUILD-ARENA @ REGEX:COMPILE nd NODE-PATTERN ! ;] catch
   {: code:n :}
   code REGEX:E-PATTERN = if v b BAD-PATTERN then
   code 0<> if code throw then ;

\ Keyword idx's whole value; idx 0 is `$schema`.
: VALUE-KEYWORD ( JSON:value n ^level -- ) {: v idx:n lv:^ :}
   lv LEVEL-NODE @ {: nd:^ :}
   idx case
      0 of v DIALECT endof
      1 of v nd TYPE-RULE endof
      2 of v nd ENUM-RULE endof
      3 of v s" `minimum` as a number"
         s" `minimum` as a number whose exponent a signed 64-bit cell holds"
         BOUND-LEXEME nd NODE-MINIMUM ! endof
      4 of v s" `maximum` as a number"
         s" `maximum` as a number whose exponent a signed 64-bit cell holds"
         BOUND-LEXEME nd NODE-MAXIMUM ! endof
      5 of v s" `minLength` as an integer from 0 to 9223372036854775807"
         LIMIT nd NODE-MIN-LENGTH ! endof
      6 of v s" `maxLength` as an integer from 0 to 9223372036854775807"
         LIMIT nd NODE-MAX-LENGTH ! endof
      7 of v lv PATTERN-RULE endof
   endcase ;

\ ---- objects ---------------------------------------------------------------

\ The keywords a root schema takes; KEYWORD reads by the index in this list.
\ A sub-schema takes the list past `$schema `.
: ROOT-KEYWORDS$ ( -- str )
   s" $schema type enum minimum maximum minLength maxLength pattern properties required additionalProperties maxProperties items minItems maxItems uniqueItems anyOf oneOf description title default examples deprecated readOnly writeOnly $comment format" ;

: KEYWORDS$ ( -- str )
   ROOT-KEYWORDS$ {: a:^ u:n :}  s" $schema " nip {: k:n :}
   a k +  u k - ;

: NEW-NODE ( ^build -- ^node )
   BUILD-ARENA @ NODES-NEW {: nd:^ :}
   -1 nd NODE-MIN-LENGTH !  -1 nd NODE-MAX-LENGTH !
   -1 nd NODE-MAX-PROPERTIES !
   -1 nd NODE-MIN-ITEMS !  -1 nd NODE-MAX-ITEMS !
   nd ;

\ The node of the schema object the walk just opened, its level pushed to
\ read it over the list, whose first name has root index base.
: OPEN ( ^build str n -- ^node ) {: b:^ list:^ lu:n base:n :}
   b BUILD-SCRATCH @ LEVELS-NEW {: lv:^ :}
   b lv LEVEL-BUILD !  b BUILD-TOP @ lv LEVEL-OUTER !
   b NEW-NODE {: nd:^ :}  nd lv LEVEL-NODE !
   nd NODE-PROPERTIES lv LEVEL-PROPERTY-TAIL !
   nd NODE-REQUIRED lv LEVEL-ENTRY-TAIL !  READING-KEYWORDS lv LEVEL-PHASE !
   list lu SPAN:MAKE lv LEVEL-NAMES !  base lv LEVEL-BASE !
   lv b BUILD-TOP !
   nd ;

: SUB ( ^build -- ^node ) KEYWORDS$ 1 OPEN ;

: SUB-FIX$ ( -- str ) s" write a schema object, `{}` for any value" ;

\ A property's address a map holds.
CAST: >PROPERTY ( n -- ^property )

: NAME-GET ( str ^n -- option<n> ) {: na:^ nu:n m:^ :}
   m NULL-PTR = if OPTION:NONE exit then
   m na nu MAP:GET ;

: NAME-COUNT ( ^n -- n ) {: m:^ :}
   m NULL-PTR = if 0 exit then
   m MAP:COUNT@ COUNT>N ;

\ na nu must live as long as arena a, which holds the map.
: NAME-PUT ( n str ^^n ^ARENA:arena -- )
   {: v:n na:^ nu:n mp:^ a:^ :}
   mp a MAP:ROOM
   v mp @ na nu MAP:SET ;

: APPEND-PROPERTY ( ^property ^level -- ) {: p:^ lv:^ :}
   p lv LEVEL-PROPERTY-TAIL @ !  p PROPERTY-NEXT lv LEVEL-PROPERTY-TAIL ! ;

\ The `{` of the schema of the member named last; its level is pushed.
: OPEN-MEMBER ( ^level -- ) {: lv:^ :}
   lv LEVEL-BUILD @ {: b:^ :}
   READING-PROPERTIES lv LEVEL-PHASE !
   b SUB {: child:^ :}
   b BUILD-ARENA @ PROPERTIES-NEW {: p:^ :}
   lv LEVEL-NAME @ p PROPERTY-NAME !  child p PROPERTY-NODE !
   p lv APPEND-PROPERTY
   p NULL-PTR -  lv LEVEL-NAME @ SPAN:$
   lv LEVEL-NODE @ NODE-PROPERTY-MAP  b BUILD-ARENA @ NAME-PUT ;

: APPEND-ENTRY ( ^entry ^level -- ) {: en:^ lv:^ :}
   en lv LEVEL-ENTRY-TAIL @ !  en ENTRY-NEXT lv LEVEL-ENTRY-TAIL ! ;

: REQUIRED-EXPECTED$ ( -- str )
   s" `required` as an array of property names" ;
: REQUIRED-FIX$ ( -- str ) s" write the names as strings in an array" ;

: REQUIRED-NAME ( JSON:value ^level -- ) {: v lv:^ :}
   lv LEVEL-NODE @ {: nd:^ :}  lv LEVEL-BUILD @ {: b:^ :}
   v JSON:STRING? 0= if
      v REQUIRED-EXPECTED$ REQUIRED-FIX$ REJECT
   then
   v b DECODED {: na:^ nu:n :}
   na nu nd NODE-REQUIRED-MAP @ NAME-GET
   MATCH option
      none OF ENDOF
      some OF
         drop v s" `required` naming each property once"
         s" remove the repeated name" REJECT
      ENDOF
   ;MATCH
   b BUILD-ARENA @ ENTRIES-NEW {: en:^ :}
   na nu SPAN:MAKE en ENTRY-NAME !
   en lv APPEND-ENTRY
   nd NODE-REQUIRED-MAP @ NAME-COUNT  na nu
   nd NODE-REQUIRED-MAP b BUILD-ARENA @ NAME-PUT ;

: REQUIRED-RULE ( JSON:value ^level -- ) {: v lv:^ :}
   v JSON:ARRAY? 0= if
      v REQUIRED-EXPECTED$ REQUIRED-FIX$ REJECT
   then
   v [: lv REQUIRED-NAME ;] JSON:EACH ;

\ A schema object opens a level of its own.
: EXTRAS-RULE ( JSON:value ^level -- ) {: v lv:^ :}
   v JSON:BOOL? if
      v JSON:>BOOL if EXTRAS-OPEN else EXTRAS-CLOSED then
      lv LEVEL-NODE @ NODE-EXTRAS ! exit
   then
   v s" `additionalProperties` as `true`, `false` or a schema object"
   SUB-FIX$ REJECT ;

\ Keywords 8 to 11 whose value opens no schema or `properties` object.
: OBJECT-KEYWORD ( JSON:value n ^level -- ) {: v idx:n lv:^ :}
   idx case
      8 of v s" `properties` as an object of schema objects"
         s" write an object mapping each property name to its schema"
         REJECT endof
      9 of v lv REQUIRED-RULE endof
      10 of v lv EXTRAS-RULE endof
      11 of v s" `maxProperties` as an integer from 0 to 9223372036854775807"
         LIMIT lv LEVEL-NODE @ NODE-MAX-PROPERTIES ! endof
   endcase ;

\ ---- arrays and choices ----------------------------------------------------

: UNIQUE-RULE ( JSON:value ^level -- ) {: v lv:^ :}
   v JSON:BOOL? if v JSON:>BOOL lv LEVEL-NODE @ NODE-UNIQUE ! exit then
   v s" `uniqueItems` as `true` or `false`" s" write `true` or `false`"
   REJECT ;

\ Keywords 12 to 15 whose value opens no schema.
: ARRAY-KEYWORD ( JSON:value n ^level -- ) {: v idx:n lv:^ :}
   lv LEVEL-NODE @ {: nd:^ :}
   idx case
      12 of v s" `items` as a schema object" SUB-FIX$ REJECT endof
      13 of v s" `minItems` as an integer from 0 to 9223372036854775807"
         LIMIT nd NODE-MIN-ITEMS ! endof
      14 of v s" `maxItems` as an integer from 0 to 9223372036854775807"
         LIMIT nd NODE-MAX-ITEMS ! endof
      15 of v lv UNIQUE-RULE endof
   endcase ;

: APPEND-BRANCH ( ^branch ^level -- ) {: br:^ lv:^ :}
   br lv LEVEL-BRANCH-TAIL @ !  br BRANCH-NEXT lv LEVEL-BRANCH-TAIL ! ;

\ The `{` of an alternative; its level is pushed and its branch appended.
: OPEN-BRANCH ( ^level -- ) {: lv:^ :}
   lv LEVEL-BUILD @ {: b:^ :}
   b SUB {: child:^ :}
   b BUILD-ARENA @ BRANCHES-NEW {: br:^ :}
   child br BRANCH-NODE !
   br lv APPEND-BRANCH ;

: CHOICE-FIX$ ( -- str ) s" list the alternatives' schemas in an array" ;

\ The expected text for the value of choice keyword idx, 16 anyOf or 17
\ oneOf.
: CHOICE-EXPECTED$ ( n -- str )
   16 = if s" `anyOf` as an array of one schema object or more" exit then
   s" `oneOf` as an array of one schema object or more" ;

: ELEMENT-EXPECTED$ ( n -- str )
   16 = if s" an `anyOf` element as a schema object" exit then
   s" a `oneOf` element as a schema object" ;

: CHOICE-LIST ( ^level -- ^^branch ) {: lv:^ :}
   lv LEVEL-NODE @ {: nd:^ :}
   lv LEVEL-KEYWORD @ 16 = if nd NODE-ANY-OF exit then
   nd NODE-ONE-OF ;

: OPEN-CHOICE ( ^level -- ) {: lv:^ :}
   lv CHOICE-LIST lv LEVEL-BRANCH-TAIL !
   READING-CHOICE lv LEVEL-PHASE ! ;

\ At the choice array's `]`, the whole array.
: CLOSE-CHOICE ( JSON:value ^level -- ) {: v lv:^ :}
   lv CHOICE-LIST @ NULL-PTR = if
      v lv LEVEL-KEYWORD @ CHOICE-EXPECTED$ CHOICE-FIX$ REJECT
   then
   READING-KEYWORDS lv LEVEL-PHASE ! ;

\ ---- schemas ---------------------------------------------------------------

\ Keyword idx's value when it opens no level, `properties` object or
\ choice array; keywords from 18 on state no rule.
: KEYWORD ( JSON:value n ^level -- ) {: v idx:n lv:^ :}
   idx 8 < if v idx lv VALUE-KEYWORD exit then
   idx 12 < if v idx lv OBJECT-KEYWORD exit then
   idx 16 < if v idx lv ARRAY-KEYWORD exit then
   idx 18 < if v idx CHOICE-EXPECTED$ CHOICE-FIX$ REJECT then ;

\ Whether the node's items state scalar types only.
: SCALAR-ITEMS? ( ^node -- bool )
   NODE-ITEMS @ dup NULL-PTR = if drop false exit then
   NODE-TYPES @ {: t:n :}
   t 0= if false exit then
   t SCALARS invert and 0= ;

\ At the schema's `}`, the whole schema: uniqueItems true needs items of
\ scalar types.
: CHECK-UNIQUE ( JSON:value ^level -- ) {: v lv:^ :}
   lv LEVEL-NODE @ {: nd:^ :}
   nd NODE-UNIQUE @ 0= if exit then
   nd SCALAR-ITEMS? if exit then
   v s" uniqueItems" JSON:GET
   s" `uniqueItems` beside `items` whose `type` names scalar types only"
   s" remove `uniqueItems`, or give `items` a `type` of scalars only"
   REJECT ;

: SKIP ( ^build -- ) 1 swap BUILD-SKIP ! ;

\ The `{` of keyword idx's value.
: OPEN-VALUE-OBJECT ( ^level -- ) {: lv:^ :}
   lv LEVEL-BUILD @ {: b:^ :}  lv LEVEL-NODE @ {: nd:^ :}
   lv LEVEL-KEYWORD @ {: idx:n :}
   idx 8 = if READING-PROPERTIES lv LEVEL-PHASE ! exit then
   idx 10 <> idx 12 <> and if b SKIP exit then
   READING-KEYWORDS lv LEVEL-PHASE !
   b SUB {: child:^ :}
   idx 10 = if
      child nd NODE-ADDITIONAL !  EXTRAS-RULED nd NODE-EXTRAS ! exit
   then
   child nd NODE-ITEMS ! ;

: OPEN-VALUE-ARRAY ( ^level -- ) {: lv:^ :}
   lv LEVEL-KEYWORD @ {: idx:n :}
   idx 16 = idx 17 = or if lv OPEN-CHOICE exit then
   lv LEVEL-BUILD @ SKIP ;

\ A `{` comes only after a name or in an array: never in the keywords or
\ properties phase.
: OPENED-OBJECT ( ^build -- ) {: b:^ :}
   b BUILD-TOP @ {: lv:^ :}
   lv LEVEL-PHASE @ MATCH reading
      keywords OF ENDOF
      value OF lv OPEN-VALUE-OBJECT ENDOF
      properties OF ENDOF
      member OF lv OPEN-MEMBER ENDOF
      choice OF lv OPEN-BRANCH ENDOF
   ;MATCH ;

: OPENED-ARRAY ( ^build -- ) {: b:^ :}
   b BUILD-TOP @ {: lv:^ :}
   lv LEVEL-PHASE @ MATCH reading
      keywords OF ENDOF
      value OF lv OPEN-VALUE-ARRAY ENDOF
      properties OF ENDOF
      member OF b SKIP ENDOF
      choice OF b SKIP ENDOF
   ;MATCH ;

\ The index in the level's list of the name, or -1.
: LISTED-INDEX ( ^level str -- n ) {: lv:^ w:^ wu:n :}
   w wu lv LEVEL-NAMES @ SPAN:$ NAME-INDEX ;

: KEYWORD-NAME ( JSON:value ^level -- ) {: v lv:^ :}
   lv v [: LISTED-INDEX ;] JSON:WITH-STRING {: idx:n :}
   idx 0< if
      v lv LEVEL-NAMES @ SPAN:$ lv LEVEL-BUILD @ BUILD-SCRATCH @ ONE-OF
      s" rename the member to one of those, or remove it" REJECT
   then
   idx lv LEVEL-BASE @ + lv LEVEL-KEYWORD !
   READING-VALUE lv LEVEL-PHASE ! ;

: MEMBER-NAME ( JSON:value ^level -- ) {: v lv:^ :}
   v lv LEVEL-BUILD @ DECODED SPAN:MAKE lv LEVEL-NAME !
   READING-MEMBER lv LEVEL-PHASE ! ;

\ Names come only in the keywords and properties phases.
: NAMED ( JSON:value ^build -- ) {: v b:^ :}
   b BUILD-TOP @ {: lv:^ :}
   lv LEVEL-PHASE @ MATCH reading
      keywords OF v lv KEYWORD-NAME ENDOF
      value OF ENDOF
      properties OF v lv MEMBER-NAME ENDOF
      member OF ENDOF
      choice OF ENDOF
   ;MATCH ;

\ A scalar, or a container closed while skipped, in the phase that reads a
\ value; names come in the others.
: WHOLE ( JSON:value ^level -- ) {: v lv:^ :}
   lv LEVEL-PHASE @ MATCH reading
      keywords OF ENDOF
      value OF
         v lv LEVEL-KEYWORD @ lv KEYWORD
         READING-KEYWORDS lv LEVEL-PHASE !
      ENDOF
      properties OF ENDOF
      member OF v s" a property's schema as an object" SUB-FIX$ REJECT ENDOF
      choice OF
         v lv LEVEL-KEYWORD @ ELEMENT-EXPECTED$ SUB-FIX$ REJECT
      ENDOF
   ;MATCH ;

: POP ( JSON:value ^level -- ) {: v lv:^ :}
   v lv CHECK-UNIQUE
   lv LEVEL-OUTER @ lv LEVEL-BUILD @ BUILD-TOP ! ;

\ A close never ends a value or a member's schema before it began.
: CLOSED ( JSON:value ^build -- ) {: v b:^ :}
   b BUILD-TOP @ {: lv:^ :}
   lv LEVEL-PHASE @ MATCH reading
      keywords OF v lv POP ENDOF
      value OF ENDOF
      properties OF READING-KEYWORDS lv LEVEL-PHASE ! ENDOF
      member OF ENDOF
      choice OF v lv CLOSE-CHOICE ENDOF
   ;MATCH ;

: SKIPPED ( JSON:value ^build -- ) {: v b:^ :}
   -1 b BUILD-SKIP +!
   b BUILD-SKIP @ 0= if v b BUILD-TOP @ WHOLE then ;

: SKIP-STEP ( JSON:event ^build -- ) {: e b:^ :}
   e MATCH JSON:event
      open-object OF 1 b BUILD-SKIP +! ENDOF
      open-array OF 1 b BUILD-SKIP +! ENDOF
      name OF DROP-VALUE ENDOF
      scalar OF DROP-VALUE ENDOF
      close OF b SKIPPED ENDOF
   ;MATCH ;

\ Takes one event of the walk; false once the root's `}` is read. The root
\ is an object, so the first event opens it.
: STEP ( JSON:event ^build -- bool ) {: e b:^ :}
   b BUILD-TOP @ NULL-PTR = if
      b ROOT-KEYWORDS$ 0 OPEN b BUILD-ROOT !  true exit
   then
   b BUILD-SKIP @ 0<> if e b SKIP-STEP true exit then
   e MATCH JSON:event
      open-object OF b OPENED-OBJECT ENDOF
      open-array OF b OPENED-ARRAY ENDOF
      name OF b NAMED ENDOF
      scalar OF b BUILD-TOP @ WHOLE ENDOF
      close OF b CLOSED ENDOF
   ;MATCH
   b BUILD-TOP @ NULL-PTR <> ;

\ s is the scratch arena.
: READ-ROOT ( JSON:value ^ARENA:arena ^ARENA:arena -- schema )
   {: root a:^ s:^ :}
   root JSON:OBJECT? 0= if
      root s" a schema object" s" write the schema as a JSON object" REJECT
   then
   s BUILDS-NEW {: b:^ :}
   a b BUILD-ARENA !  s b BUILD-SCRATCH !
   root [: b STEP ;] JSON:WALK
   b BUILD-ROOT @ SCHEMA-MAKE ;

\ ---- checking values -------------------------------------------------------

: NUMBER? ( n -- bool ) dup JSON:T-INT = swap JSON:T-FLOAT = or ;
: OPENER? ( n -- bool ) dup JSON:T-OBJ = swap JSON:T-ARR = or ;

\ What a place reads next: its value's first token, its object's next
\ member, its array's next element, the verdict of the place it pushed, or
\ its anyOf's or oneOf's alternatives; close pops it.
ENUM judging value members member elements element any-of any-alt one-of one-alt close ;ENUM

\ The index of an alternative a oneOf value meets.
STRUCTURE pass DERIVE addr
   FIELD next ^pass
   FIELD index n
;STRUCTURE

\ walk: one VALID? call. values reads an enum's array; spare chains closed
\ places by outer; verdict is the last closed place's.
\ place: one value judged against node; a container's text is its start until
\ its close is read. Text counts: mark at the push, path-end after its path.
MUTUAL walk place
STRUCTURE walk DERIVE addr
   FIELD buf ^VEC:vector<u8>
   FIELD scratch ^ARENA:arena
   FIELD cursor JSON:cursor
   FIELD values JSON:cursor
   FIELD top ^place
   FIELD spare ^place
   FIELD verdict bool
;STRUCTURE
STRUCTURE place DERIVE addr
   FIELD walk ^walk
   FIELD outer ^place
   FIELD inner ^place
   FIELD rooted bool
   FIELD name SPAN:span<u8>
   FIELD index n
   FIELD node ^node
   FIELD phase judging
   FIELD kind n
   FIELD text SPAN:span<u8>
   FIELD mark n
   FIELD path-end n
   FIELD count n
   FIELD marks ^u8
   FIELD branch ^branch
   FIELD choice n
   FIELD passes ^pass
   FIELD tail ^pass
;STRUCTURE
;MUTUAL

ARENA WALKS walk
ARENA PLACES place
ARENA PASSES pass

: MARK ( ^place -- n ) PLACE-WALK @ WALK-BUF @ VEC:COUNT ;

: BACK ( n ^place -- ) PLACE-WALK @ WALK-BUF @ swap VEC:RESIZE ;

: SPARE-PLACE ( ^walk -- ^place ) {: w:^ :}
   w WALK-SPARE @ {: pl:^ :}
   pl NULL-PTR = if w WALK-SCRATCH @ PLACES-NEW exit then
   pl PLACE-OUTER @ w WALK-SPARE !  pl ;

\ A place for the value whose first token is current, judged against nd,
\ pushed above top; rooted when it is the root.
: OPEN-PLACE ( ^node ^walk -- ^place ) {: nd:^ w:^ :}
   w SPARE-PLACE {: pl:^ :}
   w WALK-TOP @ {: up:^ :}
   w pl PLACE-WALK !  up pl PLACE-OUTER !  NULL-PTR pl PLACE-INNER !
   up NULL-PTR = pl PLACE-ROOTED !
   s" " SPAN:MAKE pl PLACE-NAME !  -1 pl PLACE-INDEX !
   nd pl PLACE-NODE !  JUDGING-VALUE pl PLACE-PHASE !
   pl MARK pl PLACE-MARK !  0 pl PLACE-COUNT !
   up NULL-PTR <> if pl up PLACE-INNER ! then
   pl w WALK-TOP !  pl ;

: POP-PLACE ( ^walk -- ) {: w:^ :}
   w WALK-TOP @ {: pl:^ :}
   pl PLACE-OUTER @ {: up:^ :}
   up w WALK-TOP !
   up NULL-PTR <> if NULL-PTR up PLACE-INNER ! then
   w WALK-SPARE @ pl PLACE-OUTER !  pl w WALK-SPARE ! ;

\ The place's verdict; its next step pops it.
: DECIDE ( bool ^place -- ) {: ok:bool pl:^ :}
   ok pl PLACE-WALK @ WALK-VERDICT !  JUDGING-CLOSE pl PLACE-PHASE ! ;

: DECIDED ( ^place -- bool ) PLACE-WALK @ WALK-VERDICT @ ;

: TEXT$ ( ^place -- str ) PLACE-TEXT @ SPAN:$ ;

: CONTAINER? ( ^place -- bool ) PLACE-KIND @ OPENER? ;

\ The container's closing token is current.
: ENDED ( JSON:reader ^place -- JSON:reader ) {: pl:^ :}
   pl TEXT$ drop {: a:^ :}
   JSON:READ-SPAN$ + a -  a swap SPAN:MAKE pl PLACE-TEXT ! ;

\ A container's first token is current: the reader moves to its last.
: SPANNED ( JSON:reader ^place -- JSON:reader ) {: pl:^ :}
   pl CONTAINER? 0= if exit then
   JSON:READ-ANY-SPAN$ SPAN:MAKE pl PLACE-TEXT ! ;

\ The current string or key, decoded into the scratch arena.
: DECODE ( JSON:reader ^place -- JSON:reader str ) {: pl:^ :}
   JSON:READ-SPAN$ nip {: raw:n :}
   pl PLACE-WALK @ WALK-SCRATCH @ raw 1 max ARENA:ALLOC drop {: dst:^ :}
   dst raw JSON:READ-STR  dst swap ;

: ADD ( str ^place -- ) {: s:^ u:n pl:^ :}
   s u pl PLACE-WALK @ WALK-BUF @ VEC:APPEND ;

\ Text quoted from the value or the schema is cut at CUT bytes.
32 constant CUT

: ADD-CUT ( str ^place -- ) {: s:^ u:n pl:^ :}
   s u CUT UTF8:FIT {: k:n :}
   s k pl PLACE-WALK @ WALK-BUF @ UNICODE-VISIBLE:APPEND
   k u < if s" ..." pl ADD then ;

: QUOTE ( str ^place -- ) {: s:^ u:n pl:^ :}
   s" `" pl ADD  s u pl ADD-CUT  s" `" pl ADD ;

\ `n things`, word naming one thing with a leading space.
: COUNTED ( n str ^place -- ) {: k:n w:^ wu:n pl:^ :}
   k FMT:INT$ pl ADD  w wu pl ADD  k 1 <> if s" s" pl ADD then ;

: SAY-INDEX ( n ^place -- ) {: idx:n pl:^ :}
   s" [" pl ADD  idx FMT:INT$ pl ADD  s" ]" pl ADD ;

: SAY-STEP ( ^place -- ) {: pl:^ :}
   pl PLACE-INDEX @ {: idx:n :}
   idx 0 >= if idx pl SAY-INDEX exit then
   s" ." pl ADD  pl PLACE-NAME @ SPAN:$ pl ADD-CUT ;

\ `.name` for each member and `[i]` for each element from the rooted place
\ down to this one.
: PATH ( ^place -- ) {: pl:^ :}
   pl begin dup PLACE-ROOTED @ 0= while PLACE-OUTER @ repeat
   dup MARK over PLACE-PATH-END !
   begin dup pl <> while
      PLACE-INNER @ dup SAY-STEP  dup MARK over PLACE-PATH-END !
   repeat drop ;

: EXPECTED ( ^place -- ) {: pl:^ :}
   pl PATH  s" : expected " pl ADD ;

: FOUND ( ^place -- ) {: pl:^ :}
   s" , found " pl ADD  pl TEXT$ pl QUOTE ;

: FIX ( str ^place -- ) {: s:^ u:n pl:^ :}
   s" ; " pl ADD  s u pl ADD ;

\ ---- types -----------------------------------------------------------------

$10 constant NUMBER-BIT
$40 constant INTEGER-BIT

: NUMBER-TYPES ( JSON:reader -- JSON:reader n )
   JSON:READ-SPAN$ JSON:PARSE JSON:INTEGER? if
      NUMBER-BIT INTEGER-BIT or exit
   then
   NUMBER-BIT ;

\ The bits of the types the current value has.
: VALUE-TYPES ( JSON:reader -- JSON:reader n )
   JSON:READ-TOKEN case
      JSON:T-NULL of 1 endof
      JSON:T-TRUE of 2 endof
      JSON:T-FALSE of 2 endof
      JSON:T-OBJ of 4 endof
      JSON:T-ARR of 8 endof
      JSON:T-FLOAT of NUMBER-TYPES endof
      JSON:T-STR of $20 endof
      JSON:T-INT of NUMBER-TYPES endof
      0 swap
   endcase ;

: NAME-AT ( n -- str n ) {: start:n :}
   TYPE-NAMES$ STR:SPACE start STR:SPLIT-NEXT drop ;

: TYPE-NAME ( n -- str ) {: idx:n :}
   0 idx 0 ?do NAME-AT nip nip loop  NAME-AT drop ;

: ORDER-LENGTH ( n -- n )
   0 begin over 0<> while 1+ swap 4 rshift swap repeat nip ;

\ The node's type names in the schema's order: `a`, `b` or `c`.
: SAY-TYPES ( ^node ^place -- ) {: nd:^ pl:^ :}
   nd NODE-TYPE-ORDER @ {: o:n :}
   o ORDER-LENGTH {: k:n :}
   k 0 ?do
      i 0 > if i k 1- = if s"  or " else s" , " then pl ADD then
      o i 4 * rshift 15 and 1- TYPE-NAME pl QUOTE
   loop ;

\ The fix for a number that is not of type integer.
: WHOLE-FIX$ ( ^place -- str )
   TEXT$ JSON:PARSE JSON:INTEGRAL? if
      s" send a whole number from -9223372036854775808 to 9223372036854775807" exit
   then
   s" send a whole number" ;

: TYPE-FIX$ ( ^node ^place -- str ) {: nd:^ pl:^ :}
   nd NODE-TYPES @ {: t:n :}
   pl PLACE-KIND @ NUMBER? t INTEGER-BIT and 0<> and if
      pl WHOLE-FIX$ exit
   then
   t dup 1- and 0= if s" send a value of that type" exit then
   s" send a value of one of those types" ;

: WRONG-TYPE ( ^node ^place -- ) {: nd:^ pl:^ :}
   pl EXPECTED  s" a value of type " pl ADD  nd pl SAY-TYPES
   pl FOUND  nd pl TYPE-FIX$ pl FIX ;

\ ---- enum and numbers ------------------------------------------------------

: HUGE ( ^place -- ) {: pl:^ :}
   pl EXPECTED
   s" a number whose exponent a signed 64-bit cell holds" pl ADD
   pl FOUND  s" write the number with a smaller exponent" pl FIX ;

\ Whether an element of the array text e equals the value text v.
: ELEMENT? ( str str ^JSON:cursor -- bool )
   {: v:^ vu:n e:^ eu:n c:^ :}
   c e eu JSON:READ-INIT JSON:READ-NEXT drop
   begin JSON:READ-NEXT JSON:T-ARR-END <> while
      JSON:READ-ANY-SPAN$ v vu JSON:VALUE= if
         JSON:READ-FINISH true exit
      then
   repeat
   JSON:READ-FINISH false ;

: UNLISTED ( ^node ^place -- ) {: nd:^ pl:^ :}
   pl EXPECTED  s" one of " pl ADD  nd NODE-VALUES @ SPAN:$ pl QUOTE
   pl FOUND  s" send one of those values" pl FIX ;

: ENUM? ( ^node -- bool ) NODE-VALUES @ SPAN:$ nip 0<> ;

\ The place's text against the node's enum. A number VALUE= cannot read,
\ COMPILE having read the enum's, is the value's.
: ENUM-OK? ( ^node ^place -- bool ) {: nd:^ pl:^ :}
   pl TEXT$ {: v:^ vu:n :}
   nd NODE-VALUES @ SPAN:$ {: e:^ eu:n :}
   pl PLACE-WALK @ WALK-VALUES {: c:^ :}
   [: v vu e eu c ELEMENT? ;] catch {: in code:n :}
   code 0= if
      in if true exit then
      nd pl UNLISTED false exit
   then
   code E-JSON-READ-NUMBER = if pl HUGE false exit then
   code throw ;

\ A bound's lexeme is quoted in the text: `at least` for a minimum.
: OUTSIDE ( SPAN:span<u8> bool ^place -- ) {: b most:bool pl:^ :}
   pl EXPECTED
   most if s" a number of at most " else s" a number of at least " then
   pl ADD  b SPAN:$ pl QUOTE
   pl FOUND  s" send a number in that range" pl FIX ;

\ Whether the number keeps to the bound b, a maximum when most; an empty b
\ states none.
: BOUND? ( SPAN:span<u8> bool ^place -- bool ) {: b most:bool pl:^ :}
   b SPAN:$ nip 0= if true exit then
   most if 1 else -1 then {: wrong:n :}
   pl TEXT$ b SPAN:$ JSON:NUMBER-COMPARE MATCH option
      none OF pl HUGE false ENDOF
      some OF wrong = if b most pl OUTSIDE false else true then ENDOF
   ;MATCH ;

: NUMBER-OK? ( ^node ^place -- bool ) {: nd:^ pl:^ :}
   nd NODE-MINIMUM @ false pl BOUND? 0= if false exit then
   nd NODE-MAXIMUM @ true pl BOUND? ;

\ ---- strings ---------------------------------------------------------------

\ The text's Unicode characters; a byte that starts none counts one.
: CHARACTERS ( str -- n ) {: s:^ u:n :}
   0 0 begin dup u < while
      s u rot UTF8:NEXT MATCH UTF8:scalar-step
         scalar OF nip ENDOF
         raw-byte OF nip ENDOF
      ;MATCH
      swap 1+ swap
   repeat drop ;

\ A string of k characters breaks the length n, a most when most.
: BAD-LENGTH ( n bool ^place -- ) {: k:n most:bool pl:^ :}
   pl EXPECTED
   most if s" a string of at most " else s" a string of at least " then
   pl ADD  k s"  character" pl COUNTED
   pl FOUND
   most if s" send a shorter string" else s" send a longer string" then
   pl FIX ;

: UNMATCHED ( ^node ^place -- ) {: nd:^ pl:^ :}
   pl EXPECTED  s" a string the pattern " pl ADD
   nd NODE-PATTERN-TEXT @ SPAN:$ pl QUOTE  s"  matches" pl ADD
   pl FOUND  s" send a string it matches" pl FIX ;

: STRING-OK? ( JSON:reader ^node ^place -- JSON:reader bool )
   {: nd:^ pl:^ :}
   nd NODE-MIN-LENGTH @ {: least:n :}  nd NODE-MAX-LENGTH @ {: most:n :}
   least 0< most 0< and nd NODE-PATTERNED @ 0= and if true exit then
   pl DECODE {: s:^ su:n :}
   s su CHARACTERS {: k:n :}
   k least < if least false pl BAD-LENGTH false exit then
   most 0 >= k most > and if most true pl BAD-LENGTH false exit then
   nd NODE-PATTERNED @ 0= if true exit then
   s su nd NODE-PATTERN @ REGEX:MATCH? if true exit then
   nd pl UNMATCHED false ;

\ ---- objects ---------------------------------------------------------------

\ The quoted names of the properties from p on: `a`, `b` and `c`.
: SAY-NAMES ( ^property ^place -- ) {: p:^ pl:^ :}
   p begin dup NULL-PTR <> while
      dup PROPERTY-NAME @ SPAN:$ pl QUOTE
      PROPERTY-NEXT @ dup NULL-PTR <> if
         dup PROPERTY-NEXT @ NULL-PTR = if s"  and " else s" , " then
         pl ADD
      then
   repeat drop ;

\ The member named na that additionalProperties false rejects in the object
\ at pl.
: EXTRA ( str ^node ^place -- ) {: na:^ nu:n nd:^ pl:^ :}
   nd NODE-PROPERTIES @ {: p:^ :}
   pl EXPECTED
   p NULL-PTR = if s" no members" else s" only the member" then pl ADD
   p NULL-PTR <> if
      p PROPERTY-NEXT @ NULL-PTR <> if s" s" pl ADD then
      s"  " pl ADD  p pl SAY-NAMES
   then
   s" , found " pl ADD  na nu pl QUOTE
   p NULL-PTR = if s" remove it" pl FIX exit then
   p PROPERTY-NEXT @ NULL-PTR <> if
      s" rename it to one of those or remove it" pl FIX exit
   then
   s" ; rename it to " pl ADD  p PROPERTY-NAME @ SPAN:$ pl QUOTE
   s"  or remove it" pl ADD ;

: ABSENT ( ^entry ^place -- ) {: en:^ pl:^ :}
   pl EXPECTED  s" a member " pl ADD  en ENTRY-NAME @ SPAN:$ pl QUOTE
   s" , found none; add " pl ADD  en ENTRY-NAME @ SPAN:$ pl QUOTE ;

: TOO-MANY ( n n ^place -- ) {: most:n k:n pl:^ :}
   pl EXPECTED  s" at most " pl ADD  most s"  member" pl COUNTED
   s" , found " pl ADD  k s"  member" pl COUNTED
   s" remove a member" pl FIX ;

: OPEN-OBJECT ( ^place -- ) {: pl:^ :}
   pl PLACE-NODE @ NODE-REQUIRED-MAP @ NAME-COUNT {: k:n :}
   pl PLACE-WALK @ WALK-SCRATCH @ k 1 max ARENA:ALLOC drop
   pl PLACE-MARKS !
   JUDGING-MEMBERS pl PLACE-PHASE ! ;

\ Marks the name, when it is required.
: MARK-REQUIRED ( str ^place -- ) {: na:^ nu:n pl:^ :}
   pl PLACE-NODE @ {: nd:^ :}
   na nu nd NODE-REQUIRED-MAP @ NAME-GET
   MATCH option
      none OF ENDOF
      some OF pl PLACE-MARKS @ swap + 1 swap c! ENDOF
   ;MATCH ;

\ The first required name no member held, else NULL-PTR.
: UNREAD-REQUIRED ( ^place -- ^entry ) {: pl:^ :}
   pl PLACE-MARKS @ {: marks:^ :}
   pl PLACE-NODE @ NODE-REQUIRED @ 0
   begin over NULL-PTR <> while
      marks over + c@ 0= if drop exit then
      1+ swap ENTRY-NEXT @ swap
   repeat drop ;

\ Reads past the members after the current one to the object's `}`.
: SKIP-MEMBERS ( JSON:reader -- JSON:reader )
   begin JSON:READ-NEXT JSON:T-OBJ-END <> while
      JSON:READ-NEXT drop  JSON:READ-SKIP-VALUE
   repeat ;

: PUSH-MEMBER ( ^node str ^place -- ) {: nd:^ na:^ nu:n pl:^ :}
   JUDGING-MEMBER pl PLACE-PHASE !
   nd pl PLACE-WALK @ OPEN-PLACE {: kid:^ :}
   na nu SPAN:MAKE kid PLACE-NAME ! ;

: FIND-PROPERTY ( str ^node -- ^property ) {: na:^ nu:n nd:^ :}
   na nu nd NODE-PROPERTY-MAP @ NAME-GET
   MATCH option
      none OF NULL-PTR ENDOF
      some OF >PROPERTY ENDOF
   ;MATCH ;

\ The key is current: its value is pushed against its property's or
\ additionalProperties' schema, skipped, or rejected.
: KEY-STEP ( JSON:reader ^place -- JSON:reader ) {: pl:^ :}
   pl PLACE-NODE @ {: nd:^ :}
   pl DECODE {: na:^ nu:n :}
   na nu nd FIND-PROPERTY {: p:^ :}
   na nu pl MARK-REQUIRED
   JSON:READ-NEXT drop
   p NULL-PTR <> if p PROPERTY-NODE @ na nu pl PUSH-MEMBER exit then
   nd NODE-EXTRAS @ MATCH extras
      open OF JSON:READ-SKIP-VALUE  1 pl PLACE-COUNT +! ENDOF
      closed OF
         na nu nd pl EXTRA
         JSON:READ-SKIP-VALUE SKIP-MEMBERS pl ENDED  false pl DECIDE
      ENDOF
      ruled OF nd NODE-ADDITIONAL @ na nu pl PUSH-MEMBER ENDOF
   ;MATCH ;

\ At the `}`: the first required name unread, then maxProperties.
: OBJECT-END ( ^place -- ) {: pl:^ :}
   pl UNREAD-REQUIRED {: en:^ :}
   en NULL-PTR <> if en pl ABSENT false pl DECIDE exit then
   pl PLACE-NODE @ NODE-MAX-PROPERTIES @ {: most:n :}
   pl PLACE-COUNT @ {: k:n :}
   most 0 >= k most > and if most k pl TOO-MANY false pl DECIDE exit then
   JUDGING-ANY-OF pl PLACE-PHASE ! ;

: MEMBERS-STEP ( JSON:reader ^place -- JSON:reader ) {: pl:^ :}
   JSON:READ-NEXT JSON:T-OBJ-END = if pl ENDED pl OBJECT-END exit then
   pl KEY-STEP ;

\ A member that fails fails its object.
: MEMBER-DONE ( JSON:reader ^place -- JSON:reader ) {: pl:^ :}
   pl DECIDED if
      1 pl PLACE-COUNT +!  JUDGING-MEMBERS pl PLACE-PHASE ! exit
   then
   SKIP-MEMBERS pl ENDED  false pl DECIDE ;

\ ---- arrays ----------------------------------------------------------------

\ One array under uniqueItems. A row is an element's rank, its offset in text
\ and length, and its decoded string's offset in decoded and length.
STRUCTURE table DERIVE addr
   FIELD text ^u8
   FIELD rows ^n
   FIELD decoded ^u8
   FIELD used n
;STRUCTURE

ARENA TABLES table

5 constant ROW-CELLS
3 constant NUMBER-RANK
4 constant STRING-RANK

\ decoded is as long as the array text, and no string decodes longer than
\ it is written.
: NEW-TABLE ( str n ^place -- ^table ) {: v:^ vu:n k:n pl:^ :}
   pl PLACE-WALK @ WALK-SCRATCH @ {: s:^ :}
   s TABLES-NEW {: tb:^ :}
   v tb TABLE-TEXT !
   s k ROW-CELLS * cells 1 max ARENA:ALLOC drop CELL-VIEW
   tb TABLE-ROWS !
   s vu 1 max ARENA:ALLOC drop tb TABLE-DECODED !
   tb ;

: ROW ( n ^table -- ^n ) {: idx:n tb:^ :}
   tb TABLE-ROWS @  idx ROW-CELLS * cells + ;

: RANK ( n ^table -- n ) ROW @ ;

\ The element's text as written.
: ELEMENT$ ( n ^table -- str ) {: idx:n tb:^ :}
   idx tb ROW {: r:^ :}
   tb TABLE-TEXT @ r 1 cells + @ +  r 2 cells + @ ;

: DECODED$ ( n ^table -- str ) {: idx:n tb:^ :}
   idx tb ROW {: r:^ :}
   tb TABLE-DECODED @ r 3 cells + @ +  r 4 cells + @ ;

\ The rank uniqueItems sorts a scalar's kind by; a kind not listed is a number.
: KIND-RANK ( n -- n )
   case
      JSON:T-NULL of 0 endof
      JSON:T-FALSE of 1 endof
      JSON:T-TRUE of 2 endof
      JSON:T-STR of STRING-RANK endof
      NUMBER-RANK swap
   endcase ;

\ The element whose token is current, its text e, as row idx.
: RECORD ( JSON:reader str n ^table -- JSON:reader )
   {: e:^ eu:n idx:n tb:^ :}
   idx tb ROW {: r:^ :}
   JSON:READ-TOKEN KIND-RANK r !
   e tb TABLE-TEXT @ - r 1 cells + !  eu r 2 cells + !
   JSON:READ-TOKEN JSON:T-STR <> if exit then
   tb TABLE-USED @ {: at:n :}
   tb TABLE-DECODED @ at +  eu JSON:READ-STR {: du:n :}
   at r 3 cells + !  du r 4 cells + !
   at du + tb TABLE-USED ! ;

: INT-ORDER ( n n -- n )
   2dup < if 2drop -1 exit then
   > if 1 else 0 then ;

: BYTE-ORDER ( str str -- n ) {: a:^ u:n b:^ w:n :}
   a u b w STR:< if -1 exit then
   b w a u STR:< if 1 else 0 then ;

\ -1, 0 or 1 as element i's value is below, equal to or above element j's.
\ NUMBER-COMPARE answers some: ORDERABLE? has rejected what it cannot order.
: VALUE-ORDER ( n n ^table -- n ) {: i:n j:n tb:^ :}
   i tb RANK {: r:n :}
   j tb RANK {: s:n :}
   r s <> if r s INT-ORDER exit then
   r NUMBER-RANK = if
      i tb ELEMENT$ j tb ELEMENT$ JSON:NUMBER-COMPARE MATCH option
         none OF 0 ENDOF
         some OF ENDOF
      ;MATCH exit
   then
   r STRING-RANK = if i tb DECODED$ j tb DECODED$ BYTE-ORDER exit then
   0 ;

\ By value, then by index.
: BEFORE? ( n n ^table -- bool ) {: i:n j:n tb:^ :}
   i j tb VALUE-ORDER {: o:n :}
   o 0<> if o 0< exit then
   i j < ;

: HUGE? ( n ^table -- bool ) {: idx:n tb:^ :}
   idx tb RANK NUMBER-RANK <> if false exit then
   idx tb ELEMENT$ 2dup JSON:NUMBER-COMPARE MATCH option
      none OF true ENDOF
      some OF drop false ENDOF
   ;MATCH ;

\ Element idx rejected at a place pushed for it and popped.
: HUGE-AT ( n ^table ^place -- ) {: idx:n tb:^ pl:^ :}
   pl PLACE-WALK @ {: w:^ :}
   pl PLACE-NODE @ NODE-ITEMS @ w OPEN-PLACE {: kid:^ :}
   idx kid PLACE-INDEX !  idx tb ELEMENT$ SPAN:MAKE kid PLACE-TEXT !
   kid HUGE  w POP-PLACE ;

\ The first number NUMBER-COMPARE cannot order, if any, rejected at its
\ place.
: ORDERABLE? ( n ^table ^place -- bool ) {: k:n tb:^ pl:^ :}
   k 0 ?do
      i tb HUGE? if i tb pl HUGE-AT unloop false exit then
   loop true ;

\ Whether the elements at positions p - 1 and p of order are equal and
\ the first of them starts its run.
: RUN-START? ( ^n n ^table -- bool ) {: order:^ p:n tb:^ :}
   order p 1- SORT:FX@ {: a:n :}
   a order p SORT:FX@ tb VALUE-ORDER 0<> if false exit then
   p 1 = if true exit then
   order p 2 - SORT:FX@ a tb VALUE-ORDER 0<> ;

\ The pair (bi, bj) or (a, b), whichever's second index is less; bj is -1
\ for no pair.
: LESSER ( n n n n -- n n ) {: bi:n bj:n a:n b:n :}
   bj 0< b bj < or if a b else bi bj then ;

\ Of each run of equal elements in order, its first two indexes: the pair
\ whose second is least, -1 -1 when no two are equal.
: REPEATED ( ^n n ^table -- n n ) {: order:^ k:n tb:^ :}
   -1 -1  k 1 ?do
      order i tb RUN-START? if
         order i 1- SORT:FX@ order i SORT:FX@ LESSER
      then
   loop ;

: DUPLICATE ( n n ^table ^place -- ) {: a:n b:n tb:^ pl:^ :}
   pl EXPECTED  s" items that differ, found " pl ADD
   b tb ELEMENT$ pl QUOTE
   s"  at " pl ADD  a pl SAY-INDEX  s"  and " pl ADD  b pl SAY-INDEX
   s" remove one of the two" pl FIX ;

\ uniqueItems over the k recorded elements: sorted by kind, value and
\ index, equal elements are neighbours.
: DISTINCT? ( n ^table ^place -- bool ) {: k:n tb:^ pl:^ :}
   k tb pl ORDERABLE? 0= if false exit then
   pl PLACE-WALK @ WALK-SCRATCH @ k cells 1 max ARENA:ALLOC drop
   CELL-VIEW {: order:^ :}
   k 0 ?do i order i cells + ! loop
   order k [: tb BEFORE? ;] SORT:SORT!
   order k tb REPEATED {: a:n b:n :}
   b 0< if true exit then
   a b tb pl DUPLICATE false ;

\ The array read again from its `[` to its `]`, each element recorded; items
\ took them all, so they are scalars.
: UNIQUE? ( JSON:reader ^place -- JSON:reader bool ) {: pl:^ :}
   pl PLACE-COUNT @ {: k:n :}
   pl TEXT$ {: v:^ vu:n :}
   v vu k pl NEW-TABLE {: tb:^ :}
   v vu JSON:READ-REWIND
   0 begin
      >r JSON:READ-NEXT JSON:T-ARR-END <> r> swap
   while
      >r JSON:READ-ANY-SPAN$ r@ tb RECORD r> 1+
   repeat drop
   k tb pl DISTINCT? ;

\ Reads past the elements after the current one to the array's `]`, adding
\ them to k.
: SKIP-ELEMENTS ( JSON:reader n -- JSON:reader n )
   begin
      >r JSON:READ-NEXT JSON:T-ARR-END <> r> swap
   while
      >r JSON:READ-SKIP-VALUE r> 1+
   repeat ;

\ k elements break the count lim, a most when most: the text after the path.
: COUNT-SAYS ( n n bool ^place -- ) {: lim:n k:n most:bool pl:^ :}
   s" : expected " pl ADD
   most if s" at most " else s" at least " then pl ADD
   lim s"  item" pl COUNTED
   s" , found " pl ADD  k s"  item" pl COUNTED
   most if s" remove an item" else s" add an item" then pl FIX ;

\ The bound lim k elements break, a most when most; bad when they break one.
: BROKEN-BOUND ( n ^node -- n bool bool ) {: k:n nd:^ :}
   nd NODE-MIN-ITEMS @ {: least:n :}  nd NODE-MAX-ITEMS @ {: most:n :}
   k least < if least false true exit then
   most 0 >= k most > and if most true true exit then
   0 false false ;

: COUNT-OK? ( n ^node ^place -- bool ) {: k:n nd:^ pl:^ :}
   k nd BROKEN-BOUND {: lim:n most:bool bad:bool :}
   bad if pl PATH  lim k most pl COUNT-SAYS then  bad 0= ;

\ At the `]`: the count, then uniqueItems.
: ARRAY-END ( JSON:reader ^place -- JSON:reader ) {: pl:^ :}
   pl PLACE-NODE @ {: nd:^ :}
   pl PLACE-COUNT @ nd pl COUNT-OK? 0= if false pl DECIDE exit then
   nd NODE-UNIQUE @ nd NODE-ITEMS @ NULL-PTR <> and if
      pl UNIQUE? 0= if false pl DECIDE exit then
   then
   JUDGING-ANY-OF pl PLACE-PHASE ! ;

\ The next element is pushed against items, or skipped and counted.
: ELEMENTS-STEP ( JSON:reader ^place -- JSON:reader ) {: pl:^ :}
   JSON:READ-NEXT JSON:T-ARR-END = if pl ENDED pl ARRAY-END exit then
   pl PLACE-NODE @ NODE-ITEMS @ {: items:^ :}
   items NULL-PTR = if JSON:READ-SKIP-VALUE 1 pl PLACE-COUNT +! exit then
   JUDGING-ELEMENT pl PLACE-PHASE !
   items pl PLACE-WALK @ OPEN-PLACE {: kid:^ :}
   pl PLACE-COUNT @ kid PLACE-INDEX ! ;

\ A failing element fails its array; a broken count's text replaces the
\ element's after the array's path, which starts the element's violation.
: ELEMENT-DONE ( JSON:reader ^place -- JSON:reader ) {: pl:^ :}
   1 pl PLACE-COUNT +!
   pl DECIDED if JUDGING-ELEMENTS pl PLACE-PHASE ! exit then
   pl PLACE-COUNT @ SKIP-ELEMENTS {: k:n :}
   pl ENDED
   pl PLACE-NODE @ {: nd:^ :}
   k nd BROKEN-BOUND {: lim:n most:bool bad:bool :}
   bad if pl PLACE-PATH-END @ pl BACK  lim k most pl COUNT-SAYS then
   false pl DECIDE ;

\ ---- anyOf and oneOf --------------------------------------------------------

: ANY-OF$ ( -- str ) s" one schema of `anyOf` takes" ;
: ONE-OF$ ( -- str ) s" exactly one schema of `oneOf` takes" ;

\ `P: expected a value s, found `v``.
: CHOICE-HEAD ( str ^place -- ) {: s:^ su:n pl:^ :}
   pl EXPECTED  s" a value " pl ADD  s su pl ADD  pl FOUND ;

\ `[k]` on a line of its own, then alternative br, k, pushed rooted on the
\ value's first token.
: ALTERNATIVE ( JSON:reader ^branch n ^place -- JSON:reader )
   {: br:^ k:n pl:^ :}
   br pl PLACE-BRANCH !  k pl PLACE-CHOICE !
   s\" \n  [" pl ADD  k FMT:INT$ pl ADD  s" ]" pl ADD
   pl CONTAINER? if pl TEXT$ JSON:READ-REWIND then
   br BRANCH-NODE @ pl PLACE-WALK @ OPEN-PLACE {: alt:^ :}
   true alt PLACE-ROOTED ! ;

: NEXT-ALTERNATIVE ( JSON:reader ^place -- JSON:reader ) {: pl:^ :}
   pl PLACE-BRANCH @ BRANCH-NEXT @  pl PLACE-CHOICE @ 1+  pl ALTERNATIVE ;

: LAST-ALTERNATIVE? ( ^place -- bool )
   PLACE-BRANCH @ BRANCH-NEXT @ NULL-PTR = ;

: ANY-OF-STEP ( JSON:reader ^place -- JSON:reader ) {: pl:^ :}
   pl PLACE-NODE @ NODE-ANY-OF @ {: br:^ :}
   br NULL-PTR = if JUDGING-ONE-OF pl PLACE-PHASE ! exit then
   ANY-OF$ pl CHOICE-HEAD  s" meet one of them:" pl FIX
   JUDGING-ANY-ALT pl PLACE-PHASE !
   br 0 pl ALTERNATIVE ;

\ An alternative that passes drops the alternatives' texts.
: ANY-ALT-DONE ( JSON:reader ^place -- JSON:reader ) {: pl:^ :}
   pl DECIDED if
      pl PLACE-MARK @ pl BACK  JUDGING-ONE-OF pl PLACE-PHASE ! exit
   then
   pl LAST-ALTERNATIVE? if false pl DECIDE exit then
   pl NEXT-ALTERNATIVE ;

: PASSED ( ^place -- ) {: pl:^ :}
   pl PLACE-WALK @ WALK-SCRATCH @ PASSES-NEW {: p:^ :}
   pl PLACE-CHOICE @ p PASS-INDEX !
   pl PLACE-TAIL @ {: t:^ :}
   t NULL-PTR = if p pl PLACE-PASSES ! else p t PASS-NEXT ! then
   p pl PLACE-TAIL ! ;

\ The indexes from p on: `[0], [1] and [2]`.
: SAY-PASSES ( ^pass ^place -- ) {: p:^ pl:^ :}
   p begin dup NULL-PTR <> while
      s" [" pl ADD  dup PASS-INDEX @ FMT:INT$ pl ADD  s" ]" pl ADD
      PASS-NEXT @ dup NULL-PTR <> if
         dup PASS-NEXT @ NULL-PTR = if s"  and " else s" , " then
         pl ADD
      then
   repeat drop ;

: AMBIGUOUS ( ^pass ^place -- ) {: p:^ pl:^ :}
   ONE-OF$ pl CHOICE-HEAD
   s" , which schemas " pl ADD  p pl SAY-PASSES  s"  take" pl ADD
   s" send a value only one of them takes" pl FIX ;

: ONE-OF-STEP ( JSON:reader ^place -- JSON:reader ) {: pl:^ :}
   pl PLACE-NODE @ NODE-ONE-OF @ {: br:^ :}
   br NULL-PTR = if true pl DECIDE exit then
   ONE-OF$ pl CHOICE-HEAD  s" meet one of them:" pl FIX
   NULL-PTR pl PLACE-PASSES !  NULL-PTR pl PLACE-TAIL !
   JUDGING-ONE-ALT pl PLACE-PHASE !
   br 0 pl ALTERNATIVE ;

\ After the last alternative: none passed keeps their texts; one passed
\ drops them; more names them.
: ONE-ALT-DONE ( JSON:reader ^place -- JSON:reader ) {: pl:^ :}
   pl DECIDED if pl PASSED then
   pl LAST-ALTERNATIVE? 0= if pl NEXT-ALTERNATIVE exit then
   pl PLACE-PASSES @ {: p:^ :}
   p NULL-PTR = if false pl DECIDE exit then
   pl PLACE-MARK @ pl BACK
   p PASS-NEXT @ NULL-PTR = if true pl DECIDE exit then
   p pl AMBIGUOUS  false pl DECIDE ;

\ ---- judging ---------------------------------------------------------------

\ A number's and a string's rules at once; an object's members and an
\ array's elements step by step.
: KIND-STEP ( JSON:reader ^place -- JSON:reader ) {: pl:^ :}
   pl PLACE-NODE @ {: nd:^ :}
   pl PLACE-KIND @ {: kind:n :}
   kind NUMBER? if
      nd pl NUMBER-OK? 0= if false pl DECIDE exit then
   then
   kind JSON:T-STR = if
      nd pl STRING-OK? 0= if false pl DECIDE exit then
   then
   kind JSON:T-OBJ = if pl OPEN-OBJECT exit then
   kind JSON:T-ARR = if JUDGING-ELEMENTS pl PLACE-PHASE ! exit then
   JUDGING-ANY-OF pl PLACE-PHASE ! ;

\ The value's first token is current: type, enum, then its kind's rules.
: VALUE-STEP ( JSON:reader ^place -- JSON:reader ) {: pl:^ :}
   pl PLACE-NODE @ {: nd:^ :}
   JSON:READ-TOKEN pl PLACE-KIND !
   pl CONTAINER? if JSON:READ-SPAN$ drop 0 else JSON:READ-ANY-SPAN$ then
   SPAN:MAKE pl PLACE-TEXT !
   VALUE-TYPES {: bits:n :}  nd NODE-TYPES @ {: t:n :}
   t 0<> t bits and 0= and if
      pl SPANNED  nd pl WRONG-TYPE  false pl DECIDE exit
   then
   nd ENUM? if
      pl SPANNED
      nd pl ENUM-OK? 0= if false pl DECIDE exit then
      pl CONTAINER? if pl TEXT$ JSON:READ-REWIND then
   then
   pl KIND-STEP ;

: JUDGE-STEP ( JSON:reader ^walk -- JSON:reader ) {: w:^ :}
   w WALK-TOP @ {: pl:^ :}
   pl PLACE-PHASE @ MATCH judging
      value OF pl VALUE-STEP ENDOF
      members OF pl MEMBERS-STEP ENDOF
      member OF pl MEMBER-DONE ENDOF
      elements OF pl ELEMENTS-STEP ENDOF
      element OF pl ELEMENT-DONE ENDOF
      any-of OF pl ANY-OF-STEP ENDOF
      any-alt OF pl ANY-ALT-DONE ENDOF
      one-of OF pl ONE-OF-STEP ENDOF
      one-alt OF pl ONE-ALT-DONE ENDOF
      close OF w POP-PLACE ENDOF
   ;MATCH ;

\ s is the scratch arena.
: WALK-ROOT ( str schema ^VEC:vector<u8> ^ARENA:arena -- bool )
   {: v:^ vu:n sc buf:^ s:^ :}
   s [: dup WALK-CURSOR JSON:READ-DISPOSE WALK-VALUES JSON:READ-DISPOSE ;]
   WALKS-NEW-DISPOSE {: w:^ :}
   buf w WALK-BUF !  s w WALK-SCRATCH !
   w WALK-CURSOR v vu JSON:READ-INIT JSON:READ-NEXT drop
   sc SCHEMA-UNMAKE w OPEN-PLACE drop
   begin w WALK-TOP @ NULL-PTR <> while w JUDGE-STEP repeat
   JSON:READ-FINISH  w WALK-VERDICT @ ;

public

\ The schema value's nodes, in arena a; a schema outside the contract throws
\ E-SCHEMA with its text.
: COMPILE ( JSON:value ^ARENA:arena -- schema )
   {: v a:^ :}
   v a [: READ-ROOT ;] ARENA:WITH ;

\ Whether the value text satisfies the schema; false appends the first
\ violation's text to the vector.
: VALID? ( str schema ^VEC:vector<u8> -- bool ) {: v:^ vu:n sc buf:^ :}
   buf VEC:COUNT {: at:n :}
   false [: drop v vu sc buf [: WALK-ROOT ;] ARENA:WITH ;] catch
   {: ok code:n :}
   code 0= if ok exit then
   buf at VEC:RESIZE  code throw ;

;package

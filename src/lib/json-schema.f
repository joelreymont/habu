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
\ VALID? answers whether the value satisfies the schema, and false appends
\ the first violation, `P: expected X, found Y; fix` with P `.name` per
\ member and `[i]` per array element below the root, to the vector; memory
\ that cannot be had throws its code, leaving the vector as it was. Type
\ integer takes a whole number a signed cell holds, whatever its spelling; a
\ larger one is of type number only. A failed anyOf or oneOf lists each
\ alternative's violation on a line of its own, `[i]` and the violation, its
\ place below the value's.
\ VALID? makes no nested call per value: JSON:WALK gives the value's events
\ in one pass, and each value being judged is a place on a lane, a stack in
\ the scratch arena with its own violation text, so memory, not the machine
\ stack, bounds a value's depth. A place judges a scalar at its event. A
\ container's members or elements are judged as their events come, a place
\ pushed for each one a schema judges; its type, enum, count and
\ uniqueItems are written at its close, where its text is whole, and an
\ enum violation replaces what its members or elements wrote. A place that
\ fails before its container's close reads past the events up to it. Each
\ event goes to every live lane. A place whose value has a type it takes
\ opens a lane for each anyOf and oneOf alternative, rooted at the value;
\ the lanes take the value's events beside it and end at its last, before
\ the place reads their verdicts. A failed anyOf or oneOf takes each
\ failed lane's text into its own, the lane's pieces linked, not copied.
\ Without $ref each schema node is live at most once per value, so memory
\ bounds the live lanes.
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
   FIELD name str
;STRUCTURE

\ types: bit i for TYPE-NAMES$'s i-th name, 0 for none; type-order: each i + 1
\ in the schema's order, 4 bits apiece. An empty string or value, -1 or null is
\ no rule.
\ property-map: each property's name to its address; required-map: each
\ required name to its index in required. A map is null until its first name.
MUTUAL node property branch
STRUCTURE node DERIVE addr
   FIELD types n
   FIELD type-order n
   FIELD values JSON:value
   FIELD minimum str
   FIELD maximum str
   FIELD min-length n
   FIELD max-length n
   FIELD max-properties n
   FIELD min-items n
   FIELD max-items n
   FIELD patterned bool
   FIELD pattern REGEX:regex
   FIELD pattern-text str
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
   FIELD name str
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
   FIELD names str
   FIELD base n
   FIELD keyword n
   FIELD name str
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
   {: v es:str fs:str :}
   [: v es fs JSON:REJECT ;] catch {: code:n :}
   code JSON:E-MEMBER = if
      JSON:E-MEMBER REASON:TEXT$ E-SCHEMA REASON:THROW
   then
   code throw ;

: PUT ( ^u8 str -- ^u8 ) {: dst:^ s:str :}
   s STR:DATA dst s STR:LEN BYTE-COPY  dst s STR:LEN + ;

: JOIN ( str str ^ARENA:arena -- str ) {: a:str b:str s:^ :}
   a STR:LEN b STR:LEN + {: len:n :}
   s len 1 max ARENA:ALLOC drop {: dst:^ :}
   dst a PUT b PUT drop
   dst len >str ;

\ Puts the blank-separated list's name at start in backticks, after a
\ comma unless it is the first; answers the next name's start.
: PUT-NAME ( ^u8 n str -- ^u8 n ) {: dst:^ start:n s:str :}
   s STR:SPACE start STR:SPLIT-NEXT drop {: name:str next:n :}
   dst  start 0<> if s" , " PUT then
   s" `" PUT name PUT s" `" PUT  next ;

\ "one of `a`, `b`" for the list's names, in the arena.
: ONE-OF ( str ^ARENA:arena -- str ) {: list:str s:^ :}
   s list STR:LEN 3 * 16 + ARENA:ALLOC drop {: dst:^ :}
   dst s" one of " PUT
   0 begin dup list STR:LEN < while list PUT-NAME repeat drop
   dst - dst swap >str ;

\ found, or idx when the list's name at start is w; the next start and idx.
: NAME-STEP ( n n n str str -- n n n )
   {: found:n start:n idx:n w:str s:str :}
   s STR:SPACE start STR:SPLIT-NEXT drop {: name:str next:n :}
   found 0<  w name STR:=  and if idx else found then
   next idx 1+ ;

\ The index of name w in the blank-separated list, or -1.
: NAME-INDEX ( str str -- n ) {: w:str s:str :}
   -1 0 0 begin over s STR:LEN < while w s NAME-STEP repeat
   2drop ;

\ ---- values ----------------------------------------------------------------

: COPIED ( ^ARENA:arena str -- str ) {: a:^ s:str :}
   a s STR:LEN 1 max ARENA:ALLOC drop {: dst:^ :}
   dst s PUT drop  dst s STR:LEN >str ;

\ The string's decoded text, in the build's arena.
: DECODED ( JSON:value ^build -- str ) {: v b:^ :}
   b BUILD-ARENA @ v [: COPIED ;] JSON:WITH-STRING ;

\ The number's lexeme; e is the expected text for another value.
: LEXEME ( JSON:value str -- str ) {: v e:str :}
   v JSON:NUMBER? 0= if
      v e s" write a number, with no quotes" REJECT
   then
   v JSON:TEXT$ ;

\ A bound's lexeme, as LEXEME; ex is the expected text for one whose
\ exponent NUMBER-COMPARE cannot order.
: BOUND-LEXEME ( JSON:value str str -- str )
   {: v e:str ex:str :}
   v e LEXEME {: b :}
   b dup JSON:NUMBER-COMPARE MATCH option
      none OF
         v ex s" write the bound with a smaller exponent" REJECT
      ENDOF
      some OF drop ENDOF
   ;MATCH b ;

\ The integer, from 0 to the largest a cell holds, written with no fraction
\ or exponent; e is the expected text for another value.
: LIMIT ( JSON:value str -- n ) {: v e:str :}
   v JSON:NUMBER? if
      v JSON:TEXT$ STR:>NUMBER? MATCH option
         none OF -1 ENDOF
         some OF ENDOF
      ;MATCH
   else -1 then
   dup 0 >= if exit then
   drop v e s" write an integer in that range" REJECT ;

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
: EMPTY? ( str -- bool ) {: s:str :}
   s STR:LEN 1- 1 ?do
      s STR:DATA i + c@ STR:SPACE > if unloop false exit then
   loop true ;

: ENUM-FIX$ ( -- str ) s" list the values allowed in an array" ;

\ Rejects a number whose exponent NUMBER-COMPARE cannot order.
: ENUM-NUMBER ( JSON:value -- ) {: v :}
   v JSON:NUMBER? 0= if exit then
   v JSON:TEXT$ dup JSON:NUMBER-COMPARE MATCH option
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
   v nd NODE-VALUES ! ;

\ Rejects the pattern with REGEX's text before the fix.
: BAD-PATTERN ( JSON:value ^build -- ) {: v b:^ :}
   b BUILD-SCRATCH @ {: s:^ :}
   s" pattern column " REGEX:E-PATTERN REASON:TEXT$ s JOIN
   s" ; rewrite the pattern as its text says" s JOIN {: fx:str :}
   v s" a pattern the dialect takes" fx REJECT ;

: PATTERN-RULE ( JSON:value ^level -- ) {: v lv:^ :}
   v JSON:STRING? 0= if
      v s" `pattern` as a string" s" write the pattern as a JSON string"
      REJECT
   then
   lv LEVEL-BUILD @ {: b:^ :}  lv LEVEL-NODE @ {: nd:^ :}
   v b DECODED {: p:str :}
   p nd NODE-PATTERN-TEXT !  true nd NODE-PATTERNED !
   [: p b BUILD-ARENA @ REGEX:COMPILE nd NODE-PATTERN ! ;] catch
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
   ROOT-KEYWORDS$ {: all:str :}  s" $schema " STR:LEN {: k:n :}
   all STR:DATA k +  all STR:LEN k - >str ;

: NEW-NODE ( ^build -- ^node )
   BUILD-ARENA @ NODES-NEW {: nd:^ :}
   -1 nd NODE-MIN-LENGTH !  -1 nd NODE-MAX-LENGTH !
   -1 nd NODE-MAX-PROPERTIES !
   -1 nd NODE-MIN-ITEMS !  -1 nd NODE-MAX-ITEMS !
   nd ;

\ The node of the schema object the walk just opened, its level pushed to
\ read it over the list, whose first name has root index base.
: OPEN ( ^build str n -- ^node ) {: b:^ list:str base:n :}
   b BUILD-SCRATCH @ LEVELS-NEW {: lv:^ :}
   b lv LEVEL-BUILD !  b BUILD-TOP @ lv LEVEL-OUTER !
   b NEW-NODE {: nd:^ :}  nd lv LEVEL-NODE !
   nd NODE-PROPERTIES lv LEVEL-PROPERTY-TAIL !
   nd NODE-REQUIRED lv LEVEL-ENTRY-TAIL !  READING-KEYWORDS lv LEVEL-PHASE !
   list lv LEVEL-NAMES !  base lv LEVEL-BASE !
   lv b BUILD-TOP !
   nd ;

: SUB ( ^build -- ^node ) KEYWORDS$ 1 OPEN ;

: SUB-FIX$ ( -- str ) s" write a schema object, `{}` for any value" ;

\ A property's address a map holds.
CAST: >PROPERTY ( n -- ^property )

: NAME-GET ( str ^n -- option<n> ) {: ns:str m:^ :}
   m NULL-PTR = if OPTION:NONE exit then
   m ns MAP:GET ;

: NAME-COUNT ( ^n -- n ) {: m:^ :}
   m NULL-PTR = if 0 exit then
   m MAP:COUNT@ COUNT>N ;

\ ns must live as long as arena a, which holds the map.
: NAME-PUT ( n str ^^n ^ARENA:arena -- )
   {: v:n ns:str mp:^ a:^ :}
   mp a MAP:ROOM
   v mp @ ns MAP:SET ;

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
   p NULL-PTR -  lv LEVEL-NAME @
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
   v b DECODED {: ns:str :}
   ns nd NODE-REQUIRED-MAP @ NAME-GET
   MATCH option
      none OF ENDOF
      some OF
         drop v s" `required` naming each property once"
         s" remove the repeated name" REJECT
      ENDOF
   ;MATCH
   b BUILD-ARENA @ ENTRIES-NEW {: en:^ :}
   ns en ENTRY-NAME !
   en lv APPEND-ENTRY
   nd NODE-REQUIRED-MAP @ NAME-COUNT  ns
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
: LISTED-INDEX ( ^level str -- n ) {: lv:^ w:str :}
   w lv LEVEL-NAMES @ NAME-INDEX ;

: KEYWORD-NAME ( JSON:value ^level -- ) {: v lv:^ :}
   lv v [: LISTED-INDEX ;] JSON:WITH-STRING {: idx:n :}
   idx 0< if
      v lv LEVEL-NAMES @ lv LEVEL-BUILD @ BUILD-SCRATCH @ ONE-OF
      s" rename the member to one of those, or remove it" REJECT
   then
   idx lv LEVEL-BASE @ + lv LEVEL-KEYWORD !
   READING-VALUE lv LEVEL-PHASE ! ;

: MEMBER-NAME ( JSON:value ^level -- ) {: v lv:^ :}
   v lv LEVEL-BUILD @ DECODED lv LEVEL-NAME !
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

\ What a place takes next. member and element await the pushed place's
\ verdict; pass reads an unjudged value; skip and mistyped read to the close.
ENUM judging value members member elements element pass skip mistyped
   any-of one-of close ;ENUM

\ walk: one VALID? call. lanes to last chains the live lanes by next, each
\ after the lane whose place opened it; spare chains closed places by outer.
\ idle chains ended lanes to reuse by next.
\ lane: a stack of places and the text its pieces hold, first to last; before
\ counts the bytes before last. verdict is the last closed place's.
\ piece: the first of lane's pieces, which ADD-LANE may link after another's.
\ place: one value judged against node; a container's value and text are set
\ at its close. Text counts: mark at the push, path-end after its path.
MUTUAL walk lane piece place
STRUCTURE walk DERIVE addr
   FIELD scratch ^ARENA:arena
   FIELD lanes ^lane
   FIELD last ^lane
   FIELD spare ^place
   FIELD idle ^lane
;STRUCTURE
STRUCTURE lane DERIVE addr
   FIELD walk ^walk
   FIELD top ^place
   FIELD verdict bool
   FIELD first ^piece
   FIELD last ^piece
   FIELD before n
   FIELD next ^lane
   FIELD prev ^lane
   FIELD sibling ^lane
;STRUCTURE
STRUCTURE piece DERIVE addr
   FIELD next ^piece
   FIELD prev ^piece
   FIELD text ^VEC:vector<u8>
   FIELD lane ^lane
;STRUCTURE
STRUCTURE place DERIVE addr
   FIELD walk ^walk
   FIELD lane ^lane
   FIELD outer ^place
   FIELD inner ^place
   FIELD rooted bool
   FIELD name str
   FIELD index n
   FIELD node ^node
   FIELD phase judging
   FIELD kind n
   FIELD value JSON:value
   FIELD text str
   FIELD mark n
   FIELD path-end n
   FIELD count n
   FIELD depth n
   FIELD marks ^u8
   FIELD any-of ^lane
   FIELD one-of ^lane
;STRUCTURE
;MUTUAL

ARENA WALKS walk
ARENA LANES lane
ARENA PIECES piece
ARENA PLACES place

\ The vector the place's lane writes its text to.
: OUT ( ^place -- ^VEC:vector<u8> ) PLACE-LANE @ LANE-LAST @ PIECE-TEXT @ ;

: LANE-SIZE ( ^lane -- n ) {: ln:^ :}
   ln LANE-BEFORE @  ln LANE-LAST @ PIECE-TEXT @ VEC:COUNT + ;

: MARK ( ^place -- n ) PLACE-LANE @ LANE-SIZE ;

\ A piece cut from another lane idles its own. Left set, its next leads
\ EMPTY-LANE into a live lane, and its prev keeps UNLINK from idling the lane.
: IDLE-PIECE ( ^piece -- ) {: p:^ :}
   p PIECE-LANE @ {: ln:^ :}  ln LANE-WALK @ {: w:^ :}
   NULL-PTR p PIECE-PREV !  NULL-PTR p PIECE-NEXT !
   w WALK-IDLE @ ln LANE-NEXT !  ln w WALK-IDLE ! ;

: DROP-PIECE ( ^lane -- ) {: ln:^ :}
   ln LANE-LAST @ {: q:^ :}  q PIECE-PREV @ {: p:^ :}
   p ln LANE-LAST !  NULL-PTR p PIECE-NEXT !
   p PIECE-TEXT @ VEC:COUNT negate ln LANE-BEFORE +!
   q IDLE-PIECE ;

\ The text cut to its first n bytes.
: BACK ( n ^place -- ) {: k:n pl:^ :}
   pl PLACE-LANE @ {: ln:^ :}
   begin k ln LANE-BEFORE @ < while ln DROP-PIECE repeat
   pl OUT  k ln LANE-BEFORE @ - VEC:RESIZE ;

\ The lane's text after the place's lane's, its pieces moved.
: ADD-LANE ( ^lane ^place -- ) {: ln:^ pl:^ :}
   pl PLACE-LANE @ {: to:^ :}
   to LANE-LAST @ {: p:^ :}  ln LANE-FIRST @ {: q:^ :}
   to LANE-SIZE ln LANE-BEFORE @ + to LANE-BEFORE !
   q p PIECE-NEXT !  p q PIECE-PREV !
   ln LANE-LAST @ to LANE-LAST ! ;

\ The lane last of the live lanes.
: LINK ( ^lane ^walk -- ) {: ln:^ w:^ :}
   w WALK-LAST @ {: up:^ :}  up ln LANE-PREV !  NULL-PTR ln LANE-NEXT !
   up NULL-PTR = if ln w WALK-LANES ! else ln up LANE-NEXT ! then
   ln w WALK-LAST ! ;

\ A lane whose text goes on in the vector, last of the live lanes.
: NEW-LANE ( ^VEC:vector<u8> ^walk -- ^lane ) {: v:^ w:^ :}
   w WALK-SCRATCH @ {: s:^ :}
   s PIECES-NEW {: p:^ :}  v p PIECE-TEXT !
   s LANES-NEW {: ln:^ :}  ln p PIECE-LANE !
   w ln LANE-WALK !  p ln LANE-FIRST !  p ln LANE-LAST !
   ln w LINK  ln ;

\ The lane's text and places emptied, its first piece kept.
: EMPTY-LANE ( ^lane -- ) {: ln:^ :}
   ln LANE-FIRST @ {: p:^ :}
   p PIECE-NEXT @ begin dup NULL-PTR <> while
      dup PIECE-NEXT @ swap IDLE-PIECE
   repeat drop
   p PIECE-TEXT @ 0 VEC:RESIZE  NULL-PTR p PIECE-NEXT !
   p ln LANE-LAST !  0 ln LANE-BEFORE !  false ln LANE-VERDICT !
   NULL-PTR ln LANE-TOP !  NULL-PTR ln LANE-SIBLING ! ;

\ An idle lane emptied, else a new one; last of the live lanes.
: SPARE-LANE ( ^walk -- ^lane ) {: w:^ :}
   w WALK-IDLE @ {: ln:^ :}
   ln NULL-PTR = if
      w WALK-SCRATCH @ ARENA:ALLOCATOR 0 [type] u8 VEC:NEW
      w NEW-LANE exit
   then
   ln LANE-NEXT @ w WALK-IDLE !
   ln EMPTY-LANE  ln w LINK  ln ;

\ The lane leaves the live lanes, idle unless ADD-LANE moved its text. Only a
\ step pops a place that opened lanes, so FEED's walk by next meets no idle one.
: UNLINK ( ^lane -- ) {: ln:^ :}
   ln LANE-WALK @ {: w:^ :}
   ln LANE-PREV @ {: up:^ :}  ln LANE-NEXT @ {: dn:^ :}
   up NULL-PTR = if dn w WALK-LANES ! else dn up LANE-NEXT ! then
   dn NULL-PTR = if up w WALK-LAST ! else up dn LANE-PREV ! then
   ln LANE-FIRST @ PIECE-PREV @ NULL-PTR <> if exit then
   w WALK-IDLE @ ln LANE-NEXT !  ln w WALK-IDLE ! ;

\ The lanes from ln on, chained by sibling, leave the live lanes.
: UNLINK-ALL ( ^lane -- )
   begin dup NULL-PTR <> while dup UNLINK LANE-SIBLING @ repeat drop ;

: SPARE-PLACE ( ^walk -- ^place ) {: w:^ :}
   w WALK-SPARE @ {: pl:^ :}
   pl NULL-PTR = if w WALK-SCRATCH @ PLACES-NEW exit then
   pl PLACE-OUTER @ w WALK-SPARE !  pl ;

\ A place for the value whose first event comes next, judged against nd,
\ pushed on the lane; rooted at the lane's bottom.
: OPEN-PLACE ( ^node ^lane -- ^place ) {: nd:^ ln:^ :}
   ln LANE-WALK @ {: w:^ :}
   w SPARE-PLACE {: pl:^ :}
   ln LANE-TOP @ {: up:^ :}
   w pl PLACE-WALK !  ln pl PLACE-LANE !
   up pl PLACE-OUTER !  NULL-PTR pl PLACE-INNER !
   up NULL-PTR = pl PLACE-ROOTED !
   s" " pl PLACE-NAME !  -1 pl PLACE-INDEX !
   nd pl PLACE-NODE !  JUDGING-VALUE pl PLACE-PHASE !
   pl MARK pl PLACE-MARK !  0 pl PLACE-COUNT !  0 pl PLACE-DEPTH !
   NULL-PTR pl PLACE-ANY-OF !  NULL-PTR pl PLACE-ONE-OF !
   up NULL-PTR <> if pl up PLACE-INNER ! then
   pl ln LANE-TOP !  pl ;

\ The top place popped; the lanes it opened end with it.
: POP-PLACE ( ^lane -- ) {: ln:^ :}
   ln LANE-TOP @ {: pl:^ :}
   pl PLACE-ANY-OF @ UNLINK-ALL  pl PLACE-ONE-OF @ UNLINK-ALL
   pl PLACE-OUTER @ {: up:^ :}
   up ln LANE-TOP !
   up NULL-PTR <> if NULL-PTR up PLACE-INNER ! then
   pl PLACE-WALK @ {: w:^ :}
   w WALK-SPARE @ pl PLACE-OUTER !  pl w WALK-SPARE ! ;

\ The place's verdict; its next step pops it.
: DECIDE ( bool ^place -- ) {: ok:bool pl:^ :}
   ok pl PLACE-LANE @ LANE-VERDICT !  JUDGING-CLOSE pl PLACE-PHASE ! ;

: DECIDED ( ^place -- bool ) PLACE-LANE @ LANE-VERDICT @ ;

: TEXT$ ( ^place -- str ) PLACE-TEXT @ ;

: KEEP ( JSON:value ^place -- ) {: v pl:^ :}
   v pl PLACE-VALUE !  v JSON:TEXT$ pl PLACE-TEXT ! ;

\ The member name, decoded into the scratch arena.
: DECODE ( JSON:value ^place -- str ) {: v pl:^ :}
   pl PLACE-WALK @ WALK-SCRATCH @ v [: COPIED ;] JSON:WITH-STRING ;

: ADD ( str ^place -- ) {: s:str pl:^ :}
   s pl OUT VEC:APPEND ;

\ Text quoted from the value or the schema is cut at CUT bytes.
32 constant CUT

: ADD-CUT ( str ^place -- ) {: s:str pl:^ :}
   s CUT UTF8:FIT {: k:n :}
   s STR:DATA k >str pl OUT UNICODE-VISIBLE:APPEND
   k s STR:LEN < if s" ..." pl ADD then ;

: QUOTE ( str ^place -- ) {: s:str pl:^ :}
   s" `" pl ADD  s pl ADD-CUT  s" `" pl ADD ;

\ `n things`, word naming one thing with a leading space.
: COUNTED ( n str ^place -- ) {: k:n w:str pl:^ :}
   k FMT:INT$ pl ADD  w pl ADD  k 1 <> if s" s" pl ADD then ;

: SAY-INDEX ( n ^place -- ) {: idx:n pl:^ :}
   s" [" pl ADD  idx FMT:INT$ pl ADD  s" ]" pl ADD ;

: SAY-STEP ( ^place -- ) {: pl:^ :}
   pl PLACE-INDEX @ {: idx:n :}
   idx 0 >= if idx pl SAY-INDEX exit then
   s" ." pl ADD  pl PLACE-NAME @ pl ADD-CUT ;

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

: FIX ( str ^place -- ) {: s:str pl:^ :}
   s" ; " pl ADD  s pl ADD ;

\ ---- types -----------------------------------------------------------------

4 constant OBJECT-BIT
8 constant ARRAY-BIT
$10 constant NUMBER-BIT
$20 constant STRING-BIT
$40 constant INTEGER-BIT

\ The bits of the types the scalar has.
: SCALAR-TYPES ( JSON:value -- n ) {: v :}
   v JSON:NULL? if 1 exit then
   v JSON:BOOL? if 2 exit then
   v JSON:STRING? if STRING-BIT exit then
   v JSON:INTEGER? if NUMBER-BIT INTEGER-BIT or exit then
   NUMBER-BIT ;

\ Whether a value with the type bits has a type the node names.
: TYPED? ( n ^node -- bool ) {: bits:n nd:^ :}
   nd NODE-TYPES @ {: t:n :}
   t 0= t bits and 0<> or ;

: NAME-AT ( n -- str n ) {: start:n :}
   TYPE-NAMES$ STR:SPACE start STR:SPLIT-NEXT drop ;

: TYPE-NAME ( n -- str ) {: idx:n :}
   0 idx 0 ?do NAME-AT nip loop  NAME-AT drop ;

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
   PLACE-VALUE @ JSON:INTEGRAL? if
      s" send a whole number from -9223372036854775808 to 9223372036854775807" exit
   then
   s" send a whole number" ;

: TYPE-FIX$ ( ^node ^place -- str ) {: nd:^ pl:^ :}
   nd NODE-TYPES @ {: t:n :}
   pl PLACE-KIND @ NUMBER-BIT and 0<> t INTEGER-BIT and 0<> and if
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

\ Whether an element of the enum e equals the value v.
: LISTED? ( JSON:value JSON:value -- bool ) {: v e :}
   v JSON:TEXT$ {: t:str :}
   false e [: JSON:TEXT$ t JSON:VALUE= nip dup 0= ;] JSON:EACH-WHILE ;

: UNLISTED ( ^node ^place -- ) {: nd:^ pl:^ :}
   pl EXPECTED  s" one of " pl ADD  nd NODE-VALUES @ JSON:TEXT$ pl QUOTE
   pl FOUND  s" send one of those values" pl FIX ;

: ENUM? ( ^node -- bool ) NODE-VALUES @ JSON:TEXT$ STR:LEN 0<> ;

\ The value against the node's enum; a violation replaces the place's text.
\ COMPILE read the enum's numbers, so one VALUE= cannot read is the value's.
: ENUM-OK? ( ^node ^place -- bool ) {: nd:^ pl:^ :}
   nd ENUM? 0= if true exit then
   [: pl PLACE-VALUE @ nd NODE-VALUES @ LISTED? ;] catch {: in code:n :}
   code 0= if
      in if true exit then
      pl PLACE-MARK @ pl BACK  nd pl UNLISTED false exit
   then
   code E-JSON-READ-NUMBER <> if code throw then
   pl PLACE-MARK @ pl BACK  pl HUGE false ;

\ A bound's lexeme is quoted in the text: `at least` for a minimum.
: OUTSIDE ( str bool ^place -- ) {: b most:bool pl:^ :}
   pl EXPECTED
   most if s" a number of at most " else s" a number of at least " then
   pl ADD  b pl QUOTE
   pl FOUND  s" send a number in that range" pl FIX ;

\ Whether the number keeps to the bound b, a maximum when most; an empty b
\ states none.
: BOUND? ( str bool ^place -- bool ) {: b most:bool pl:^ :}
   b STR:LEN 0= if true exit then
   most if 1 else -1 then {: wrong:n :}
   pl TEXT$ b JSON:NUMBER-COMPARE MATCH option
      none OF pl HUGE false ENDOF
      some OF wrong = if b most pl OUTSIDE false else true then ENDOF
   ;MATCH ;

: NUMBER-OK? ( ^node ^place -- bool ) {: nd:^ pl:^ :}
   nd NODE-MINIMUM @ false pl BOUND? 0= if false exit then
   nd NODE-MAXIMUM @ true pl BOUND? ;

\ ---- strings ---------------------------------------------------------------

\ The text's Unicode characters; a byte that starts none counts one.
: CHARACTERS ( str -- n ) {: s:str :}
   0 0 begin dup s STR:LEN < while
      s swap UTF8:NEXT MATCH UTF8:scalar-step
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
   nd NODE-PATTERN-TEXT @ pl QUOTE  s"  matches" pl ADD
   pl FOUND  s" send a string it matches" pl FIX ;

\ The decoded string against the node's lengths and pattern.
: TEXT-OK? ( str ^node ^place -- bool ) {: s:str nd:^ pl:^ :}
   nd NODE-MIN-LENGTH @ {: least:n :}  nd NODE-MAX-LENGTH @ {: most:n :}
   s CHARACTERS {: k:n :}
   k least < if least false pl BAD-LENGTH false exit then
   most 0 >= k most > and if most true pl BAD-LENGTH false exit then
   nd NODE-PATTERNED @ 0= if true exit then
   s nd NODE-PATTERN @ REGEX:MATCH? if true exit then
   nd pl UNMATCHED false ;

: STRING-OK? ( ^node ^place -- bool ) {: nd:^ pl:^ :}
   nd NODE-MIN-LENGTH @ 0<  nd NODE-MAX-LENGTH @ 0< and
   nd NODE-PATTERNED @ 0= and if true exit then
   pl PLACE-VALUE @ [: nd pl TEXT-OK? ;] JSON:WITH-STRING ;

\ ---- objects ---------------------------------------------------------------

\ The quoted names of the properties from p on: `a`, `b` and `c`.
: SAY-NAMES ( ^property ^place -- ) {: p:^ pl:^ :}
   p begin dup NULL-PTR <> while
      dup PROPERTY-NAME @ pl QUOTE
      PROPERTY-NEXT @ dup NULL-PTR <> if
         dup PROPERTY-NEXT @ NULL-PTR = if s"  and " else s" , " then
         pl ADD
      then
   repeat drop ;

\ The member named ns that additionalProperties false rejects in the object
\ at pl.
: EXTRA ( str ^node ^place -- ) {: ns:str nd:^ pl:^ :}
   nd NODE-PROPERTIES @ {: p:^ :}
   pl EXPECTED
   p NULL-PTR = if s" no members" else s" only the member" then pl ADD
   p NULL-PTR <> if
      p PROPERTY-NEXT @ NULL-PTR <> if s" s" pl ADD then
      s"  " pl ADD  p pl SAY-NAMES
   then
   s" , found " pl ADD  ns pl QUOTE
   p NULL-PTR = if s" remove it" pl FIX exit then
   p PROPERTY-NEXT @ NULL-PTR <> if
      s" rename it to one of those or remove it" pl FIX exit
   then
   s" ; rename it to " pl ADD  p PROPERTY-NAME @ pl QUOTE
   s"  or remove it" pl ADD ;

: ABSENT ( ^entry ^place -- ) {: en:^ pl:^ :}
   pl EXPECTED  s" a member " pl ADD  en ENTRY-NAME @ pl QUOTE
   s" , found none; add " pl ADD  en ENTRY-NAME @ pl QUOTE ;

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
: MARK-REQUIRED ( str ^place -- ) {: ns:str pl:^ :}
   pl PLACE-NODE @ {: nd:^ :}
   ns nd NODE-REQUIRED-MAP @ NAME-GET
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

\ A violation is written: the place reads past its container's events to
\ its close.
: FAIL ( ^place -- ) {: pl:^ :}
   JUDGING-SKIP pl PLACE-PHASE ! ;

: PUSH-MEMBER ( ^node str ^place -- ) {: nd:^ ns:str pl:^ :}
   JUDGING-MEMBER pl PLACE-PHASE !
   nd pl PLACE-LANE @ OPEN-PLACE {: kid:^ :}
   ns kid PLACE-NAME ! ;

: FIND-PROPERTY ( str ^node -- ^property ) {: ns:str nd:^ :}
   ns nd NODE-PROPERTY-MAP @ NAME-GET
   MATCH option
      none OF NULL-PTR ENDOF
      some OF >PROPERTY ENDOF
   ;MATCH ;

\ A member's name: its value is pushed against its property's or
\ additionalProperties' schema, read past, or rejected.
: KEY-STEP ( JSON:value ^place -- ) {: v pl:^ :}
   pl PLACE-NODE @ {: nd:^ :}
   v pl DECODE {: ns:str :}
   ns nd FIND-PROPERTY {: p:^ :}
   ns pl MARK-REQUIRED
   p NULL-PTR <> if p PROPERTY-NODE @ ns pl PUSH-MEMBER exit then
   nd NODE-EXTRAS @ MATCH extras
      open OF JUDGING-PASS pl PLACE-PHASE ! ENDOF
      closed OF ns nd pl EXTRA  pl FAIL ENDOF
      ruled OF nd NODE-ADDITIONAL @ ns pl PUSH-MEMBER ENDOF
   ;MATCH ;

\ At the `}`: the first required name unread, then maxProperties.
: OBJECT-END ( ^place -- bool ) {: pl:^ :}
   pl UNREAD-REQUIRED {: en:^ :}
   en NULL-PTR <> if en pl ABSENT false exit then
   pl PLACE-NODE @ NODE-MAX-PROPERTIES @ {: most:n :}
   pl PLACE-COUNT @ {: k:n :}
   most 0 >= k most > and if most k pl TOO-MANY false exit then
   true ;

\ A member that fails fails its object.
: MEMBER-DONE ( ^place -- ) {: pl:^ :}
   pl DECIDED if
      1 pl PLACE-COUNT +!  JUDGING-MEMBERS pl PLACE-PHASE ! exit
   then
   pl FAIL ;

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
: NEW-TABLE ( str n ^place -- ^table ) {: v:str k:n pl:^ :}
   pl PLACE-WALK @ WALK-SCRATCH @ {: s:^ :}
   s TABLES-NEW {: tb:^ :}
   v STR:DATA tb TABLE-TEXT !
   s k ROW-CELLS * cells 1 max ARENA:ALLOC drop CELL-VIEW
   tb TABLE-ROWS !
   s v STR:LEN 1 max ARENA:ALLOC drop tb TABLE-DECODED !
   tb ;

: ROW ( n ^table -- ^n ) {: idx:n tb:^ :}
   tb TABLE-ROWS @  idx ROW-CELLS * cells + ;

: RANK ( n ^table -- n ) ROW @ ;

\ The element's text as written.
: ELEMENT$ ( n ^table -- str ) {: idx:n tb:^ :}
   idx tb ROW {: r:^ :}
   tb TABLE-TEXT @ r 1 cells + @ +  r 2 cells + @ >str ;

: DECODED$ ( n ^table -- str ) {: idx:n tb:^ :}
   idx tb ROW {: r:^ :}
   tb TABLE-DECODED @ r 3 cells + @ +  r 4 cells + @ >str ;

\ The rank uniqueItems sorts a scalar's kind by.
: KIND-RANK ( JSON:value -- n ) {: v :}
   v JSON:NULL? if 0 exit then
   v JSON:BOOL? if v JSON:>BOOL if 2 else 1 then exit then
   v JSON:STRING? if STRING-RANK exit then
   NUMBER-RANK ;

\ The string's decoded text after the decoded texts row r's table holds.
: DECODE-ROW ( JSON:value ^n ^table -- ) {: e r:^ tb:^ :}
   tb TABLE-USED @ {: at:n :}
   tb TABLE-DECODED @ at + {: dst:^ :}
   dst e [: PUT ;] JSON:WITH-STRING dst - {: du:n :}
   at r 3 cells + !  du r 4 cells + !
   at du + tb TABLE-USED ! ;

\ The element e as row idx.
: RECORD ( ^table n JSON:value -- ^table n ) {: tb:^ idx:n e :}
   idx tb ROW {: r:^ :}
   e KIND-RANK r !
   e JSON:TEXT$ {: t:str :}
   t STR:DATA tb TABLE-TEXT @ - r 1 cells + !
   t STR:LEN r 2 cells + !
   e JSON:STRING? if e r tb DECODE-ROW then
   tb idx 1+ ;

: INT-ORDER ( n n -- n )
   2dup < if 2drop -1 exit then
   > if 1 else 0 then ;

: BYTE-ORDER ( str str -- n ) {: a:str b:str :}
   a b STR:< if -1 exit then
   b a STR:< if 1 else 0 then ;

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
   idx tb ELEMENT$ dup JSON:NUMBER-COMPARE MATCH option
      none OF true ENDOF
      some OF drop false ENDOF
   ;MATCH ;

\ Element idx rejected at a place pushed for it and popped.
: HUGE-AT ( n ^table ^place -- ) {: idx:n tb:^ pl:^ :}
   pl PLACE-LANE @ {: ln:^ :}
   pl PLACE-NODE @ NODE-ITEMS @ ln OPEN-PLACE {: kid:^ :}
   idx kid PLACE-INDEX !  idx tb ELEMENT$ kid PLACE-TEXT !
   kid HUGE  ln POP-PLACE ;

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

\ Each element of the array recorded; items took them all, so they are
\ scalars.
: UNIQUE? ( ^place -- bool ) {: pl:^ :}
   pl PLACE-COUNT @ {: k:n :}
   pl TEXT$ k pl NEW-TABLE {: tb:^ :}
   tb 0 pl PLACE-VALUE @ [: RECORD ;] JSON:EACH drop drop
   k tb pl DISTINCT? ;

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
: ARRAY-END ( ^place -- bool ) {: pl:^ :}
   pl PLACE-NODE @ {: nd:^ :}
   pl PLACE-COUNT @ nd pl COUNT-OK? 0= if false exit then
   nd NODE-UNIQUE @ nd NODE-ITEMS @ NULL-PTR <> and 0= if true exit then
   pl UNIQUE? ;

\ The next element is pushed against items, or read past and counted.
: START-ELEMENT ( ^place -- ) {: pl:^ :}
   pl PLACE-NODE @ NODE-ITEMS @ {: items:^ :}
   items NULL-PTR = if JUDGING-PASS pl PLACE-PHASE ! exit then
   JUDGING-ELEMENT pl PLACE-PHASE !
   items pl PLACE-LANE @ OPEN-PLACE {: kid:^ :}
   pl PLACE-COUNT @ kid PLACE-INDEX ! ;

\ A failing element fails its array.
: ELEMENT-DONE ( ^place -- ) {: pl:^ :}
   1 pl PLACE-COUNT +!
   pl DECIDED if JUDGING-ELEMENTS pl PLACE-PHASE ! exit then
   pl FAIL ;

\ At the `]` of an array an element failed: a broken count's text replaces
\ the element's after the array's path, which the element's began with.
: RECOUNT ( ^place -- ) {: pl:^ :}
   pl PLACE-COUNT @ {: k:n :}
   k pl PLACE-NODE @ BROKEN-BOUND {: lim:n most:bool bad:bool :}
   bad if pl PLACE-PATH-END @ pl BACK  lim k most pl COUNT-SAYS then ;

\ ---- anyOf and oneOf --------------------------------------------------------

: ANY-OF$ ( -- str ) s" one schema of `anyOf` takes" ;
: ONE-OF$ ( -- str ) s" exactly one schema of `oneOf` takes" ;

\ `P: expected a value s, found `v``.
: CHOICE-HEAD ( str ^place -- ) {: s:str pl:^ :}
   pl EXPECTED  s" a value " pl ADD  s pl ADD  pl FOUND ;

\ A lane rooted at the place's value, judging it against the alternative.
: ALTERNATIVE ( ^branch ^place -- ^lane ) {: br:^ pl:^ :}
   pl PLACE-WALK @ SPARE-LANE {: ln:^ :}
   br BRANCH-NODE @ ln OPEN-PLACE drop  ln ;

\ A lane for each alternative from br on, chained by sibling.
: ALTERNATIVES ( ^branch ^place -- ^lane ) {: br:^ pl:^ :}
   br NULL-PTR = if NULL-PTR exit then
   br pl ALTERNATIVE {: head:^ :}
   head br BRANCH-NEXT @
   begin dup NULL-PTR <> while
      swap over pl ALTERNATIVE tuck swap LANE-SIBLING !
      swap BRANCH-NEXT @
   repeat 2drop head ;

\ At the value's first event, once its type holds: the lanes of its
\ anyOf's and oneOf's alternatives, which take the event next.
: OPEN-LANES ( ^place -- ) {: pl:^ :}
   pl PLACE-NODE @ {: nd:^ :}
   nd NODE-ANY-OF @ pl ALTERNATIVES pl PLACE-ANY-OF !
   nd NODE-ONE-OF @ pl ALTERNATIVES pl PLACE-ONE-OF ! ;

\ The lanes from ln on whose verdict is true.
: PASSING ( ^lane -- n )
   0 swap begin dup NULL-PTR <> while
      dup LANE-VERDICT @ if swap 1+ swap then  LANE-SIBLING @
   repeat drop ;

\ `[k]` on a line of its own, then lane k's violation.
: SAY-ALTERNATIVE ( ^lane n ^place -- ) {: ln:^ k:n pl:^ :}
   s\" \n  " pl ADD  k pl SAY-INDEX  ln pl ADD-LANE ;

\ Each lane's from ln on, all of which failed.
: SAY-ALTERNATIVES ( ^lane ^place -- ) {: ln:^ pl:^ :}
   s" meet one of them:" pl FIX
   ln 0 begin over NULL-PTR <> while
      2dup pl SAY-ALTERNATIVE  1+ swap LANE-SIBLING @ swap
   repeat 2drop ;

\ The anyOf's lanes all failing fails the place.
: ANY-OF-STEP ( ^place -- ) {: pl:^ :}
   pl PLACE-ANY-OF @ {: ln:^ :}
   ln NULL-PTR = ln PASSING 0<> or if
      JUDGING-ONE-OF pl PLACE-PHASE ! exit
   then
   ANY-OF$ pl CHOICE-HEAD  ln pl SAY-ALTERNATIVES  false pl DECIDE ;

\ `[i]` for lane i when it passes, then `, ` or ` and ` when more pass
\ after it, r passing from lane i on; the next lane, its index and r.
: SAY-PASS ( ^lane n n ^place -- ^lane n n ) {: ln:^ idx:n r:n pl:^ :}
   ln LANE-SIBLING @  idx 1+
   ln LANE-VERDICT @ 0= if r exit then
   idx pl SAY-INDEX  r 1- {: left:n :}
   left 1 = if s"  and " pl ADD then
   left 1 > if s" , " pl ADD then  left ;

\ The indexes of the lanes from ln on that pass: `[0], [1] and [2]`.
: SAY-PASSES ( ^lane ^place -- ) {: ln:^ pl:^ :}
   ln 0 ln PASSING begin dup 0<> while pl SAY-PASS repeat
   drop 2drop ;

: AMBIGUOUS ( ^lane ^place -- ) {: ln:^ pl:^ :}
   ONE-OF$ pl CHOICE-HEAD
   s" , which schemas " pl ADD  ln pl SAY-PASSES  s"  take" pl ADD
   s" send a value only one of them takes" pl FIX ;

\ Exactly one of the oneOf's lanes passing passes the place; none lists
\ their violations, more names them.
: ONE-OF-STEP ( ^place -- ) {: pl:^ :}
   pl PLACE-ONE-OF @ {: ln:^ :}
   ln PASSING {: k:n :}
   ln NULL-PTR = k 1 = or if true pl DECIDE exit then
   k 0= if ONE-OF$ pl CHOICE-HEAD  ln pl SAY-ALTERNATIVES
   else ln pl AMBIGUOUS then
   false pl DECIDE ;

\ ---- judging ---------------------------------------------------------------

\ A number's bounds, or a string's lengths and pattern.
: KIND-OK? ( n ^node ^place -- bool ) {: bits:n nd:^ pl:^ :}
   bits NUMBER-BIT and 0<> if nd pl NUMBER-OK? exit then
   bits STRING-BIT = if nd pl STRING-OK? exit then
   true ;

\ A scalar's type, enum, then its kind's rules.
: SCALAR-STEP ( JSON:value ^place -- ) {: v pl:^ :}
   pl PLACE-NODE @ {: nd:^ :}
   v pl KEEP  v SCALAR-TYPES {: bits:n :}  bits pl PLACE-KIND !
   bits nd TYPED? 0= if nd pl WRONG-TYPE false pl DECIDE exit then
   nd pl ENUM-OK? 0= if false pl DECIDE exit then
   bits nd pl KIND-OK? 0= if false pl DECIDE exit then
   pl OPEN-LANES  JUDGING-ANY-OF pl PLACE-PHASE ! ;

\ A container's `{` or `[`: a type the node does not name is written at
\ the close, where the container's text is whole.
: OPENED ( n ^place -- ) {: bits:n pl:^ :}
   bits pl PLACE-KIND !
   bits pl PLACE-NODE @ TYPED? 0= if
      JUDGING-MISTYPED pl PLACE-PHASE ! exit
   then
   pl OPEN-LANES
   bits OBJECT-BIT = if pl OPEN-OBJECT exit then
   JUDGING-ELEMENTS pl PLACE-PHASE ! ;

\ A container's close, ok when its members' or elements' rules held: enum,
\ then anyOf and oneOf.
: FINISH ( bool ^place -- ) {: ok:bool pl:^ :}
   pl PLACE-NODE @ pl ENUM-OK? 0= if false pl DECIDE exit then
   ok 0= if false pl DECIDE exit then
   JUDGING-ANY-OF pl PLACE-PHASE ! ;

\ An event in what the place reads past: 1 when a value at depth 0 ends, 2
\ at the place's own close, whose value is kept, else 0.
: INNER ( JSON:event ^place -- n ) {: e pl:^ :}
   pl PLACE-DEPTH {: d:^ :}
   e MATCH JSON:event
      open-object OF 1 d +! 0 ENDOF
      open-array OF 1 d +! 0 ENDOF
      name OF DROP-VALUE 0 ENDOF
      scalar OF DROP-VALUE d @ 0= if 1 else 0 then ENDOF
      close OF
         d @ 0= if pl KEEP 2 else
            DROP-VALUE -1 d +!  d @ 0= if 1 else 0 then
         then
      ENDOF
   ;MATCH ;

\ A value's first event opens it or is a scalar, and an object gives a
\ member's name or its close; the walk gives no other event there.
: VALUE-EVENT ( JSON:event ^place -- ) {: e pl:^ :}
   e MATCH JSON:event
      open-object OF OBJECT-BIT pl OPENED ENDOF
      open-array OF ARRAY-BIT pl OPENED ENDOF
      name OF DROP-VALUE ENDOF
      scalar OF pl SCALAR-STEP ENDOF
      close OF DROP-VALUE ENDOF
   ;MATCH ;

: MEMBERS-EVENT ( JSON:event ^place -- ) {: e pl:^ :}
   e MATCH JSON:event
      open-object OF ENDOF
      open-array OF ENDOF
      name OF pl KEY-STEP ENDOF
      scalar OF DROP-VALUE ENDOF
      close OF pl KEEP  pl OBJECT-END pl FINISH ENDOF
   ;MATCH ;

\ True when the place takes the event; an element's first event is left
\ for the place pushed for it, or for the pass over it.
: ELEMENTS-EVENT ( JSON:event ^place -- bool ) {: e pl:^ :}
   e MATCH JSON:event
      open-object OF false ENDOF
      open-array OF false ENDOF
      name OF DROP-VALUE true ENDOF
      scalar OF DROP-VALUE false ENDOF
      close OF pl KEEP  pl ARRAY-END pl FINISH  true ENDOF
   ;MATCH
   dup if exit then  pl START-ELEMENT ;

\ A member's or element's value no schema judges, counted at its end.
: PASS-EVENT ( JSON:event ^place -- ) {: e pl:^ :}
   e pl INNER 1 <> if exit then
   1 pl PLACE-COUNT +!
   pl PLACE-KIND @ OBJECT-BIT = if JUDGING-MEMBERS else JUDGING-ELEMENTS then
   pl PLACE-PHASE ! ;

\ After a violation each value at depth 0 is counted, for an array's
\ count at its close.
: SKIP-EVENT ( JSON:event ^place -- ) {: e pl:^ :}
   e pl INNER {: r:n :}
   r 1 = if 1 pl PLACE-COUNT +! exit then
   r 2 <> if exit then
   pl PLACE-KIND @ ARRAY-BIT = if pl RECOUNT then
   false pl FINISH ;

: MISTYPED-EVENT ( JSON:event ^place -- ) {: e pl:^ :}
   e pl INNER 2 <> if exit then
   pl PLACE-NODE @ pl WRONG-TYPE  false pl DECIDE ;

\ True when the place takes the event; a place that waits for none never
\ meets one.
: TAKE ( JSON:event ^place -- bool ) {: e pl:^ :}
   pl PLACE-PHASE @ MATCH judging
      value OF e pl VALUE-EVENT true ENDOF
      members OF e pl MEMBERS-EVENT true ENDOF
      member OF true ENDOF
      elements OF e pl ELEMENTS-EVENT ENDOF
      element OF true ENDOF
      pass OF e pl PASS-EVENT true ENDOF
      skip OF e pl SKIP-EVENT true ENDOF
      mistyped OF e pl MISTYPED-EVENT true ENDOF
      any-of OF true ENDOF
      one-of OF true ENDOF
      close OF true ENDOF
   ;MATCH ;

\ Steps the lane's top place; false when it waits for an event.
: JUDGE-STEP ( ^lane -- bool ) {: ln:^ :}
   ln LANE-TOP @ {: pl:^ :}
   pl PLACE-PHASE @ MATCH judging
      value OF false ENDOF
      members OF false ENDOF
      member OF pl MEMBER-DONE true ENDOF
      elements OF false ENDOF
      element OF pl ELEMENT-DONE true ENDOF
      pass OF false ENDOF
      skip OF false ENDOF
      mistyped OF false ENDOF
      any-of OF pl ANY-OF-STEP true ENDOF
      one-of OF pl ONE-OF-STEP true ENDOF
      close OF ln POP-PLACE true ENDOF
   ;MATCH ;

\ Steps the lane's places until one waits for an event or the last pops.
: SETTLE ( ^lane -- ) {: ln:^ :}
   begin ln LANE-TOP @ NULL-PTR <> while
      ln JUDGE-STEP 0= if exit then
   repeat ;

\ The event taken by the lane's top place or the place it pushes for it.
: HAND ( JSON:event ^lane -- ) {: e ln:^ :}
   begin e ln LANE-TOP @ TAKE until ;

\ Each live lane takes the event, first to last, a lane opened at it too;
\ then each steps, last to first, a place's lanes before the place.
: FEED ( ^walk JSON:event -- ^walk bool ) {: w:^ e :}
   w WALK-LANES @ begin dup NULL-PTR <> while
      e over HAND  LANE-NEXT @
   repeat drop
   w WALK-LAST @ begin dup NULL-PTR <> while
      dup SETTLE  LANE-PREV @
   repeat drop
   w true ;

\ The pieces after the lane's first appended to it.
: FLATTEN ( ^lane -- ) {: ln:^ :}
   ln LANE-FIRST @ {: p:^ :}
   p PIECE-NEXT @ begin dup NULL-PTR <> while
      dup PIECE-TEXT @ VEC:SPAN SPAN:$ p PIECE-TEXT @ VEC:APPEND
      PIECE-NEXT @
   repeat drop ;

\ s is the scratch arena.
: WALK-ROOT ( JSON:value schema ^VEC:vector<u8> ^ARENA:arena -- bool )
   {: v sc buf:^ s:^ :}
   s WALKS-NEW {: w:^ :}
   s w WALK-SCRATCH !
   buf w NEW-LANE {: ln:^ :}
   sc SCHEMA-UNMAKE ln OPEN-PLACE drop
   w v [: FEED ;] JSON:WALK drop
   ln FLATTEN  ln LANE-VERDICT @ ;

public

: COMPILE ( JSON:value ^ARENA:arena -- schema )
   {: v a:^ :}
   v a [: READ-ROOT ;] ARENA:WITH ;

: VALID? ( JSON:value schema ^VEC:vector<u8> -- bool ) {: v sc buf:^ :}
   buf VEC:COUNT {: at:n :}
   false [: drop v sc buf [: WALK-ROOT ;] ARENA:WITH ;] catch
   {: ok code:n :}
   code 0= if ok exit then
   buf at VEC:RESIZE  code throw ;

;package

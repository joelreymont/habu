split-str
\ json-schema.f - JSON Schema, package JSON-SCHEMA: COMPILE reads a schema's
\ text into nodes that state what a JSON value must be; VALID? checks a
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
\ keyword, a keyword repeated in one schema, a value its keyword does not
\ take, a bound or enum number whose exponent a signed cell does not hold,
\ or text that is no JSON throws E-SCHEMA through REASON:THROW with the
\ text `L:C: expected X, found Y; fix`, line and byte column in the schema
\ text. COMPILE makes no nested call per sub-schema: each open schema object
\ is a level on a stack in the scratch arena, read one member or element a
\ step, so memory, not the machine stack, bounds a schema's depth.
\ VALID? ( ^u8 n schema ^a -- bool ) answers whether a JSON value text the
\ reader admits satisfies the schema, and false appends the first
\ violation, `P: expected X, found Y; fix` with P `.name` per member and
\ `[i]` per array element below the root, to the BUF ^a; text the reader
\ does not admit, or memory that cannot be had, throws its code, leaving the
\ BUF as it was. A failed anyOf or oneOf lists each alternative's violation
\ on a line of its own, `[i]` and the violation, its place below the value's.
\ STORAGE CLASS. CALLER-OWNED: the nodes, the decoded names and each
\ pattern's decoded text and program lie in the arena COMPILE was given,
\ read-only after COMPILE; they borrow the schema text, which must outlive
\ the schema. VALID?'s scratch is one ARENA:WITH, so calls on several
\ threads at once are safe.

require src/lib/error.f
require src/lib/string.f
require src/lib/span.f
require src/lib/fmt.f
require src/lib/adt/option.f
require src/lib/arena.f
require src/lib/reason.f
require src/lib/json.f
require src/lib/regex.f
require src/lib/byte-buffer.f
require src/lib/utf8-scalar.f
require src/core/rejection.f

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
   FIELD required ^entry
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

\ What a level reads next: its own keywords, the members of its
\ `properties` object, or the elements of its anyOf or oneOf array.
ENUM reading keywords properties choice ;ENUM

\ build is one COMPILE call: the arena the nodes go in, the scratch arena,
\ the schema text, whose start rejections are placed from, and top, the
\ innermost open level, NULL-PTR once the root's `}` is read.
\ level is one schema object being read: outer is the level that opened it;
\ seen holds a byte per name of the keyword list for READ-LISTED, and base
\ is the root index of the list's first name; choice is the open anyOf's or
\ oneOf's keyword index; unique-at is the offset of uniqueItems' `true`, 0
\ when it states none.
MUTUAL build level
STRUCTURE build DERIVE addr
   FIELD arena ^ARENA:arena
   FIELD scratch ^ARENA:arena
   FIELD text ^u8
   FIELD root ^node
   FIELD top ^level
;STRUCTURE
STRUCTURE level DERIVE addr
   FIELD build ^build
   FIELD outer ^level
   FIELD node ^node
   FIELD phase reading
   FIELD names SPAN:span<u8>
   FIELD base n
   FIELD seen ^u8
   FIELD choice n
   FIELD unique-at n
;STRUCTURE
;MUTUAL

ARENA NODES node
ARENA PROPERTIES property
ARENA BRANCHES branch
ARENA ENTRIES entry
ARENA BUILDS build
ARENA LEVELS level
ARENA CURSORS JSON:cursor

\ ---- rejections ------------------------------------------------------------

\ Rejects the current token: READ-REJECT's E-MEMBER text leaves as
\ E-SCHEMA.
: REJECT ( JSON:reader ^u8 n ^u8 n -- )
   {: ea:^ eu:n fa:^ fu:n :}
   [: ea eu fa fu JSON:READ-REJECT ;] catch {: code:n :}
   code JSON:E-MEMBER = if
      JSON:E-MEMBER REASON:TEXT$ E-SCHEMA REASON:THROW
   then
   code throw ;

\ Room for a rejection's place and separators beside its texts.
64 constant TEXT-SLACK

: PUT ( ^u8 ^u8 n -- ^u8 ) {: dst:^ src:^ u:n :}
   src dst u BYTE-COPY  dst u + ;

\ `L:C: expected e, found ` for the byte at offset at of the schema text.
: PUT-PLACE ( ^u8 ^build n ^u8 n -- ^u8 ) {: dst:^ b:^ at:n e:^ eu:n :}
   b BUILD-TEXT @ dup at + 1 1 0 REJECTION:POSITION drop
   {: line:n col:n :}
   dst line FMT:INT$ PUT s" :" PUT col FMT:INT$ PUT
   s" : expected " PUT e eu PUT s" , found " PUT ;

\ Throws E-SCHEMA for the byte at offset at, which the reader has left,
\ with the expected text, the found text f as given and the fix.
: REJECT-AT ( ^build n ^u8 n ^u8 n ^u8 n -- )
   {: b:^ at:n e:^ eu:n f:^ fu:n x:^ xu:n :}
   b BUILD-SCRATCH @ eu fu + xu + TEXT-SLACK + ARENA:ALLOC drop
   {: base:^ :}
   base b at e eu PUT-PLACE f fu PUT s" ; " PUT x xu PUT
   base - {: tu:n :}
   base tu E-SCHEMA REASON:THROW ;

\ A code the JSON reader threw leaves as E-SCHEMA with its text; a source
\ COMPILE's caller gave wrong, and every other code, as it is.
: RETHROW ( n -- ) {: code:n :}
   code E-JSON-READ-FIRST <=  code E-JSON-READ-LAST >=  and
   code JSON:E-READ-SOURCE <> and if
      code REASON:TEXT$ E-SCHEMA REASON:THROW
   then
   code throw ;

\ ---- values ----------------------------------------------------------------

\ The offset of the current token's first byte, a string's opening quote.
: TOKEN-AT ( JSON:reader ^build -- JSON:reader n ) {: b:^ :}
   JSON:READ-TOKEN {: kind:n :}
   JSON:READ-SPAN$ drop  b BUILD-TEXT @ -
   kind JSON:T-STR = if 1- then ;

\ The current string or key, decoded into the arena.
: DECODED ( JSON:reader ^build -- JSON:reader ^u8 n ) {: b:^ :}
   JSON:READ-SPAN$ nip {: raw:n :}
   b BUILD-ARENA @ raw 1 max ARENA:ALLOC drop {: dst:^ :}
   dst raw JSON:READ-STR  dst swap ;

: NUMBER? ( n -- bool ) dup JSON:T-INT = swap JSON:T-FLOAT = or ;

\ The current number's lexeme; e is the expected text for another token.
: LEXEME ( JSON:reader ^u8 n -- JSON:reader SPAN:span<u8> ) {: e:^ eu:n :}
   JSON:READ-TOKEN NUMBER? 0= if
      e eu s" write a number, with no quotes" REJECT
   then
   JSON:READ-SPAN$ SPAN:MAKE ;

\ A bound's lexeme, as LEXEME; ex is the expected text for one whose
\ exponent NUMBER-COMPARE cannot order.
: BOUND-LEXEME ( JSON:reader ^u8 n ^u8 n -- JSON:reader SPAN:span<u8> )
   {: e:^ eu:n ex:^ exu:n :}
   e eu LEXEME {: b :}
   b SPAN:$ 2dup JSON:NUMBER-COMPARE MATCH option
      none OF
         ex exu s" write the bound with a smaller exponent" REJECT
      ENDOF
      some OF drop ENDOF
   ;MATCH b ;

\ The current integer, from 0 to the largest a cell holds; e is the
\ expected text for another token.
: LIMIT ( JSON:reader ^u8 n -- JSON:reader n ) {: e:^ eu:n :}
   JSON:READ-TOKEN JSON:T-INT = if
      JSON:READ-SPAN$ STR>NUMBER? MATCH option
         none OF -1 ENDOF
         some OF ENDOF
      ;MATCH
   else -1 then
   dup 0 >= if exit then
   drop e eu s" write an integer in that range" REJECT ;

: DIALECT$ ( -- ^u8 n ) s" https://json-schema.org/draft/2020-12/schema" ;

: DIALECT ( JSON:reader -- JSON:reader )
   JSON:READ-TOKEN JSON:T-STR = if
      DIALECT$ JSON:READ-STR-EQ? if exit then
   then
   s" `$schema` as `https://json-schema.org/draft/2020-12/schema`"
   s" name the 2020-12 dialect, or remove `$schema`" REJECT ;

: TYPE-NAMES$ ( -- ^u8 n )
   s" null boolean object array number string integer" ;
7 constant TYPE-COUNT
\ The bits of null, boolean, number, string and integer.
$73 constant SCALARS

\ found, or the bit of name idx, which starts at start, when the current
\ string is that name; and the next name's start.
: TYPE-STEP ( JSON:reader n n n -- JSON:reader n n )
   {: found:n start:n idx:n :}
   TYPE-NAMES$ STR-SPACE start SPLIT-NEXT drop {: na:^ nu:n next:n :}
   na nu JSON:READ-STR-EQ? if 1 idx lshift else found then  next ;

\ The bit of the type the current string names, 0 for none.
: TYPE-BIT ( JSON:reader -- JSON:reader n )
   0 0 TYPE-COUNT 0 ?do i TYPE-STEP loop drop ;

\ The bit of the type the current token names.
: NAMED-TYPE ( JSON:reader -- JSON:reader n )
   JSON:READ-TOKEN JSON:T-STR = if TYPE-BIT else 0 then
   dup 0<> if exit then
   drop
   s" `type` as one of `null`, `boolean`, `object`, `array`, `number`, `string` and `integer`"
   s" write one of those type names" REJECT ;

: BIT-INDEX ( n -- n )
   0 begin over 1 and 0= while 1+ swap 1 rshift swap repeat nip ;

: ADD-ORDER ( n ^node -- ) {: bit:n nd:^ :}
   nd NODE-TYPE-ORDER @ {: o:n :}
   0 begin o over rshift 0<> while 4 + repeat {: at:n :}
   bit BIT-INDEX 1+ at lshift o or nd NODE-TYPE-ORDER ! ;

: ADD-TYPE ( JSON:reader ^node -- JSON:reader ) {: nd:^ :}
   NAMED-TYPE {: bit:n :}
   nd NODE-TYPES @ bit and 0<> if
      s" `type` naming each type once" s" remove the repeated type name"
      REJECT
   then
   nd NODE-TYPES @ bit or nd NODE-TYPES !
   bit nd ADD-ORDER ;

: TYPE-LIST ( JSON:reader ^node -- JSON:reader ) {: nd:^ :}
   begin JSON:READ-NEXT JSON:T-ARR-END <> while nd ADD-TYPE repeat
   nd NODE-TYPES @ 0<> if exit then
   s" `type` as an array of one type name or more"
   s" name a type, or remove `type`" REJECT ;

: TYPE-RULE ( JSON:reader ^node -- JSON:reader ) {: nd:^ :}
   JSON:READ-TOKEN JSON:T-ARR = if nd TYPE-LIST exit then
   nd ADD-TYPE ;

\ Whether an array's text holds only white space between its brackets.
: EMPTY? ( ^u8 n -- bool ) {: a:^ u:n :}
   u 1- 1 ?do
      a i + c@ STR-SPACE > if unloop false exit then
   loop true ;

: ENUM-EXPECTED$ ( -- ^u8 n )
   s" `enum` as an array of one value or more" ;
: ENUM-FIX$ ( -- ^u8 n ) s" list the values allowed in an array" ;

\ Rejects the current token, of kind kind, when it is a number whose
\ exponent NUMBER-COMPARE cannot order.
: ENUM-NUMBER ( JSON:reader n -- JSON:reader ) {: kind:n :}
   kind NUMBER? 0= if exit then
   JSON:READ-SPAN$ 2dup JSON:NUMBER-COMPARE MATCH option
      none OF
         s" `enum` values whose exponents a signed 64-bit cell holds"
         s" write each number with a smaller exponent" REJECT
      ENDOF
      some OF drop ENDOF
   ;MATCH ;

: OPENER? ( n -- bool ) dup JSON:T-OBJ = swap JSON:T-ARR = or ;
: CLOSER? ( n -- bool )
   dup JSON:T-OBJ-END = swap JSON:T-ARR-END = or ;

\ Reads the next token of an enum array at depth d; the depth after it.
: ENUM-STEP ( JSON:reader n -- JSON:reader n ) {: d:n :}
   JSON:READ-NEXT {: kind:n :}
   kind ENUM-NUMBER
   kind OPENER? if d 1+ exit then
   kind CLOSER? if d 1- exit then
   d ;

\ The text of the array whose `[` is current, the reader left on its `]`.
: ENUM-VALUES ( JSON:reader -- JSON:reader ^u8 n )
   JSON:READ-SPAN$ drop {: a:^ :}
   1 begin dup 0 > while ENUM-STEP repeat drop
   JSON:READ-SPAN$ + a -  a swap ;

: ENUM-RULE ( JSON:reader ^node -- JSON:reader ) {: nd:^ :}
   JSON:READ-TOKEN JSON:T-ARR <> if
      ENUM-EXPECTED$ ENUM-FIX$ REJECT
   then
   ENUM-VALUES {: a:^ u:n :}
   a u EMPTY? if ENUM-EXPECTED$ ENUM-FIX$ REJECT then
   a u SPAN:MAKE nd NODE-VALUES ! ;

\ Rejects the pattern string at offset at with REGEX's text as found.
: BAD-PATTERN ( ^build n -- ) {: b:^ at:n :}
   REGEX:E-PATTERN REASON:TEXT$ {: t:^ tu:n :}
   s" pattern column " {: p:^ pu:n :}
   b BUILD-SCRATCH @ pu tu + ARENA:ALLOC drop {: f:^ :}
   f p pu PUT t tu PUT drop
   b at s" a pattern the dialect takes" f pu tu +
   s" rewrite the pattern as its text says" REJECT-AT ;

: PATTERN-RULE ( JSON:reader ^level -- JSON:reader ) {: lv:^ :}
   JSON:READ-TOKEN JSON:T-STR <> if
      s" `pattern` as a string" s" write the pattern as a JSON string"
      REJECT
   then
   lv LEVEL-BUILD @ {: b:^ :}  lv LEVEL-NODE @ {: nd:^ :}
   b TOKEN-AT {: at:n :}
   b DECODED {: p:^ pu:n :}
   p pu SPAN:MAKE nd NODE-PATTERN-TEXT !  true nd NODE-PATTERNED !
   [: p pu b BUILD-ARENA @ REGEX:COMPILE nd NODE-PATTERN ! ;] catch
   {: code:n :}
   code REGEX:E-PATTERN = if b at BAD-PATTERN then
   code 0<> if code throw then ;

\ The first token of keyword idx's value is current; idx 0 is `$schema`.
: VALUE-KEYWORD ( JSON:reader n ^level -- JSON:reader ) {: idx:n lv:^ :}
   lv LEVEL-NODE @ {: nd:^ :}
   idx case
      0 of DIALECT endof
      1 of nd TYPE-RULE endof
      2 of nd ENUM-RULE endof
      3 of s" `minimum` as a number"
         s" `minimum` as a number whose exponent a signed 64-bit cell holds"
         BOUND-LEXEME nd NODE-MINIMUM ! endof
      4 of s" `maximum` as a number"
         s" `maximum` as a number whose exponent a signed 64-bit cell holds"
         BOUND-LEXEME nd NODE-MAXIMUM ! endof
      5 of s" `minLength` as an integer from 0 to 9223372036854775807"
         LIMIT nd NODE-MIN-LENGTH ! endof
      6 of s" `maxLength` as an integer from 0 to 9223372036854775807"
         LIMIT nd NODE-MAX-LENGTH ! endof
      7 of lv PATTERN-RULE endof
   endcase ;

\ ---- objects ---------------------------------------------------------------

\ The keywords READ-LISTED takes at the root, each optional; KEYWORD reads
\ by the index in this list. A sub-schema takes the list past `$schema? `.
: ROOT-KEYWORDS$ ( -- ^u8 n )
   s" $schema? type? enum? minimum? maximum? minLength? maxLength? pattern? properties? required? additionalProperties? maxProperties? items? minItems? maxItems? uniqueItems? anyOf? oneOf? description? title? default? examples? deprecated? readOnly? writeOnly? $comment? format?" ;

: KEYWORDS$ ( -- ^u8 n )
   ROOT-KEYWORDS$ {: a:^ u:n :}  s" $schema? " nip {: k:n :}
   a k +  u k - ;

: NEW-NODE ( ^build -- ^node )
   BUILD-ARENA @ NODES-NEW {: nd:^ :}
   -1 nd NODE-MIN-LENGTH !  -1 nd NODE-MAX-LENGTH !
   -1 nd NODE-MAX-PROPERTIES !
   -1 nd NODE-MIN-ITEMS !  -1 nd NODE-MAX-ITEMS !
   nd ;

\ The node of the schema object whose `{` is current, its level pushed to
\ read it over the keyword list; base is the root index of the list's first
\ name.
: OPEN ( ^build ^u8 n n -- ^node ) {: b:^ list:^ lu:n base:n :}
   b BUILD-SCRATCH @ {: s:^ :}
   s LEVELS-NEW {: lv:^ :}
   b lv LEVEL-BUILD !  b BUILD-TOP @ lv LEVEL-OUTER !
   b NEW-NODE lv LEVEL-NODE !  READING-KEYWORDS lv LEVEL-PHASE !
   list lu SPAN:MAKE lv LEVEL-NAMES !  base lv LEVEL-BASE !
   s list lu JSON:READ-NAME-COUNT ARENA:ALLOC drop lv LEVEL-SEEN !
   lv b BUILD-TOP !
   lv LEVEL-NODE @ ;

\ The sub-schema whose first token is current, its level pushed; e is the
\ expected text for a token that opens no object.
: SUB ( JSON:reader ^build ^u8 n -- JSON:reader ^node ) {: b:^ e:^ eu:n :}
   JSON:READ-TOKEN JSON:T-OBJ <> if
      e eu s" write a schema object, `{}` for any value" REJECT
   then
   b KEYWORDS$ 1 OPEN ;

\ Whether the property is named as the current key; p stays.
: SAME-NAME? ( JSON:reader ^property -- JSON:reader ^property bool )
   {: p:^ :}
   p PROPERTY-NAME @ SPAN:$ JSON:READ-STR-EQ? p swap ;

\ The first property from p on named as the current key, else NULL-PTR.
: PROPERTY-NAMED ( JSON:reader ^property -- JSON:reader ^property )
   begin dup NULL-PTR <> while
      SAME-NAME? if exit then PROPERTY-NEXT @
   repeat ;

: NAMED? ( JSON:reader ^property -- JSON:reader bool )
   PROPERTY-NAMED NULL-PTR <> ;

: APPEND-PROPERTY ( ^property ^node -- )
   NODE-PROPERTIES
   begin dup @ NULL-PTR <> while @ PROPERTY-NEXT repeat ! ;

\ The key is current; its value's level is pushed.
: PROPERTY ( JSON:reader ^level -- JSON:reader ) {: lv:^ :}
   lv LEVEL-NODE @ {: nd:^ :}  lv LEVEL-BUILD @ {: b:^ :}
   nd NODE-PROPERTIES @ NAMED? if
      s" `properties` naming each property once"
      s" remove the repeated property, keeping one" REJECT
   then
   b DECODED {: na:^ nu:n :}
   JSON:READ-NEXT drop
   b s" a property's schema as an object" SUB {: child:^ :}
   b BUILD-ARENA @ PROPERTIES-NEW {: p:^ :}
   na nu SPAN:MAKE p PROPERTY-NAME !  child p PROPERTY-NODE !
   p nd APPEND-PROPERTY ;

: PROPERTIES-RULE ( JSON:reader ^level -- JSON:reader ) {: lv:^ :}
   JSON:READ-TOKEN JSON:T-OBJ <> if
      s" `properties` as an object of schema objects"
      s" write an object mapping each property name to its schema" REJECT
   then
   READING-PROPERTIES lv LEVEL-PHASE ! ;

: PROPERTIES-STEP ( JSON:reader ^level -- JSON:reader ) {: lv:^ :}
   JSON:READ-NEXT JSON:T-OBJ-END = if
      READING-KEYWORDS lv LEVEL-PHASE ! exit
   then
   lv PROPERTY ;

: SAME-ENTRY? ( JSON:reader ^entry -- JSON:reader ^entry bool )
   {: en:^ :}
   en ENTRY-NAME @ SPAN:$ JSON:READ-STR-EQ? en swap ;

\ Whether an entry from en on holds the current string.
: LISTED? ( JSON:reader ^entry -- JSON:reader bool )
   begin dup NULL-PTR <> while
      SAME-ENTRY? if drop true exit then ENTRY-NEXT @
   repeat drop false ;

: APPEND-ENTRY ( ^entry ^node -- )
   NODE-REQUIRED begin dup @ NULL-PTR <> while @ ENTRY-NEXT repeat ! ;

: REQUIRED-EXPECTED$ ( -- ^u8 n )
   s" `required` as an array of property names" ;
: REQUIRED-FIX$ ( -- ^u8 n ) s" write the names as strings in an array" ;

: REQUIRED-NAME ( JSON:reader ^level -- JSON:reader ) {: lv:^ :}
   lv LEVEL-NODE @ {: nd:^ :}  lv LEVEL-BUILD @ {: b:^ :}
   JSON:READ-TOKEN JSON:T-STR <> if
      REQUIRED-EXPECTED$ REQUIRED-FIX$ REJECT
   then
   nd NODE-REQUIRED @ LISTED? if
      s" `required` naming each property once" s" remove the repeated name"
      REJECT
   then
   b DECODED {: na:^ nu:n :}
   b BUILD-ARENA @ ENTRIES-NEW {: en:^ :}
   na nu SPAN:MAKE en ENTRY-NAME !
   en nd APPEND-ENTRY ;

: REQUIRED-RULE ( JSON:reader ^level -- JSON:reader ) {: lv:^ :}
   JSON:READ-TOKEN JSON:T-ARR <> if
      REQUIRED-EXPECTED$ REQUIRED-FIX$ REJECT
   then
   begin JSON:READ-NEXT JSON:T-ARR-END <> while lv REQUIRED-NAME repeat ;

: EXTRAS-RULE ( JSON:reader ^level -- JSON:reader ) {: lv:^ :}
   lv LEVEL-NODE @ {: nd:^ :}
   JSON:READ-TOKEN {: kind:n :}
   kind JSON:T-TRUE = if EXTRAS-OPEN nd NODE-EXTRAS ! exit then
   kind JSON:T-FALSE = if EXTRAS-CLOSED nd NODE-EXTRAS ! exit then
   lv LEVEL-BUILD @
   s" `additionalProperties` as `true`, `false` or a schema object" SUB
   nd NODE-ADDITIONAL !  EXTRAS-RULED nd NODE-EXTRAS ! ;

: OBJECT-KEYWORD ( JSON:reader n ^level -- JSON:reader ) {: idx:n lv:^ :}
   idx case
      8 of lv PROPERTIES-RULE endof
      9 of lv REQUIRED-RULE endof
      10 of lv EXTRAS-RULE endof
      11 of s" `maxProperties` as an integer from 0 to 9223372036854775807"
         LIMIT lv LEVEL-NODE @ NODE-MAX-PROPERTIES ! endof
   endcase ;

\ ---- arrays and choices ----------------------------------------------------

: UNIQUE-RULE ( JSON:reader ^level -- JSON:reader ) {: lv:^ :}
   JSON:READ-TOKEN {: kind:n :}
   kind JSON:T-FALSE = if exit then
   kind JSON:T-TRUE <> if
      s" `uniqueItems` as `true` or `false`" s" write `true` or `false`"
      REJECT
   then
   lv LEVEL-BUILD @ TOKEN-AT lv LEVEL-UNIQUE-AT !
   true lv LEVEL-NODE @ NODE-UNIQUE ! ;

: ARRAY-KEYWORD ( JSON:reader n ^level -- JSON:reader ) {: idx:n lv:^ :}
   lv LEVEL-NODE @ {: nd:^ :}
   idx case
      12 of lv LEVEL-BUILD @ s" `items` as a schema object" SUB
         nd NODE-ITEMS ! endof
      13 of s" `minItems` as an integer from 0 to 9223372036854775807"
         LIMIT nd NODE-MIN-ITEMS ! endof
      14 of s" `maxItems` as an integer from 0 to 9223372036854775807"
         LIMIT nd NODE-MAX-ITEMS ! endof
      15 of lv UNIQUE-RULE endof
   endcase ;

: APPEND-BRANCH ( ^branch ^^branch -- )
   begin dup @ NULL-PTR <> while @ BRANCH-NEXT repeat ! ;

\ An alternative whose first token is current, appended at head.
: BRANCH ( JSON:reader ^build ^^branch ^u8 n -- JSON:reader )
   {: b:^ head:^ e:^ eu:n :}
   b e eu SUB {: child:^ :}
   b BUILD-ARENA @ BRANCHES-NEW {: br:^ :}
   child br BRANCH-NODE !
   br head APPEND-BRANCH ;

: CHOICE-FIX$ ( -- ^u8 n ) s" list the alternatives' schemas in an array" ;

\ The expected text for the value of choice keyword idx, 16 anyOf or 17
\ oneOf.
: CHOICE-EXPECTED$ ( n -- ^u8 n )
   16 = if s" `anyOf` as an array of one schema object or more" exit then
   s" `oneOf` as an array of one schema object or more" ;

: ELEMENT-EXPECTED$ ( n -- ^u8 n )
   16 = if s" an `anyOf` element as a schema object" exit then
   s" a `oneOf` element as a schema object" ;

: CHOICE-KEYWORD ( JSON:reader n ^level -- JSON:reader ) {: idx:n lv:^ :}
   JSON:READ-TOKEN JSON:T-ARR <> if
      idx CHOICE-EXPECTED$ CHOICE-FIX$ REJECT
   then
   idx lv LEVEL-CHOICE !  READING-CHOICE lv LEVEL-PHASE ! ;

: CHOICE-STEP ( JSON:reader ^level -- JSON:reader ) {: lv:^ :}
   lv LEVEL-CHOICE @ {: idx:n :}  lv LEVEL-NODE @ {: nd:^ :}
   idx 16 = if nd NODE-ANY-OF else nd NODE-ONE-OF then {: head:^ :}
   JSON:READ-NEXT JSON:T-ARR-END <> if
      lv LEVEL-BUILD @ head idx ELEMENT-EXPECTED$ BRANCH exit
   then
   head @ NULL-PTR = if idx CHOICE-EXPECTED$ CHOICE-FIX$ REJECT then
   READING-KEYWORDS lv LEVEL-PHASE ! ;

\ ---- schemas ---------------------------------------------------------------

\ Keywords from 18 on state no rule.
: KEYWORD ( JSON:reader n ^level -- JSON:reader ) {: idx:n lv:^ :}
   idx 8 < if idx lv VALUE-KEYWORD exit then
   idx 12 < if idx lv OBJECT-KEYWORD exit then
   idx 16 < if idx lv ARRAY-KEYWORD exit then
   idx 18 < if idx lv CHOICE-KEYWORD exit then
   JSON:READ-SKIP-VALUE ;

\ Whether the node's items state scalar types only.
: SCALAR-ITEMS? ( ^node -- bool )
   NODE-ITEMS @ dup NULL-PTR = if drop false exit then
   NODE-TYPES @ {: t:n :}
   t 0= if false exit then
   t SCALARS invert and 0= ;

\ At the schema's `}`: uniqueItems true needs items of scalar types.
: CHECK-UNIQUE ( ^level -- ) {: lv:^ :}
   lv LEVEL-UNIQUE-AT @ 0= if exit then
   lv LEVEL-NODE @ SCALAR-ITEMS? if exit then
   lv LEVEL-BUILD @ lv LEVEL-UNIQUE-AT @
   s" `uniqueItems` beside `items` whose `type` names scalar types only"
   s" `true`"
   s" remove `uniqueItems`, or give `items` a `type` of scalars only"
   REJECT-AT ;

\ The next keyword; at the schema's `}` its level is popped.
: KEYWORDS-STEP ( JSON:reader ^level -- JSON:reader ) {: lv:^ :}
   lv LEVEL-NAMES @ SPAN:$ lv LEVEL-SEEN @ JSON:READ-LISTED {: idx:n :}
   idx 0< if
      lv CHECK-UNIQUE
      lv LEVEL-OUTER @ lv LEVEL-BUILD @ BUILD-TOP ! exit
   then
   idx lv LEVEL-BASE @ + lv KEYWORD ;

\ Reads the next member or element of the innermost open level.
: STEP ( JSON:reader ^build -- JSON:reader )
   BUILD-TOP @ {: lv:^ :}
   lv LEVEL-PHASE @ MATCH reading
      keywords OF lv KEYWORDS-STEP ENDOF
      properties OF lv PROPERTIES-STEP ENDOF
      choice OF lv CHOICE-STEP ENDOF
   ;MATCH ;

: ROOT ( JSON:reader ^build -- JSON:reader ^node ) {: b:^ :}
   JSON:READ-TOKEN JSON:T-OBJ <> if
      s" a schema object" s" write the schema as a JSON object" REJECT
   then
   b ROOT-KEYWORDS$ 0 OPEN ;

\ s is the scratch arena; a throw leaves the cursor owning nothing.
: READ-ROOT ( ^u8 n ^ARENA:arena ^ARENA:arena -- schema )
   {: src:^ u:n a:^ s:^ :}
   s BUILDS-NEW {: b:^ :}
   a b BUILD-ARENA !  s b BUILD-SCRATCH !  src b BUILD-TEXT !
   s CURSORS-NEW {: c:^ :}
   [: c src u JSON:READ-INIT JSON:READ-NEXT drop
      b ROOT b BUILD-ROOT !
      begin b BUILD-TOP @ NULL-PTR <> while b STEP repeat
      JSON:READ-FINISH ;] catch {: code:n :}
   code 0<> if c JSON:READ-DISPOSE code RETHROW then
   b BUILD-ROOT @ SCHEMA-MAKE ;

\ ---- checking values -------------------------------------------------------

\ One VALID? call: the BUF a violation's text goes to, the scratch arena
\ and the cursor an enum's array is read with.
STRUCTURE walk DERIVE addr
   FIELD buf ^n
   FIELD scratch ^ARENA:arena
   FIELD values JSON:cursor
;STRUCTURE

\ A value's place under the one at up: element index when not negative, else
\ member name; the root when up is NULL-PTR. cursor reads the values here.
STRUCTURE place DERIVE addr
   FIELD up ^place
   FIELD name SPAN:span<u8>
   FIELD index n
   FIELD walk ^walk
   FIELD cursor JSON:cursor
;STRUCTURE

ARENA WALKS walk
ARENA PLACES place

: NEW-PLACE ( ^place ^walk -- ^place ) {: up:^ w:^ :}
   w WALK-SCRATCH @ [: PLACE-CURSOR JSON:READ-DISPOSE ;]
   PLACES-NEW-DISPOSE {: pl:^ :}
   up pl PLACE-UP !  w pl PLACE-WALK !  -1 pl PLACE-INDEX !  pl ;

: CHILD ( ^place -- ^place ) dup PLACE-WALK @ NEW-PLACE ;

\ The current string or key, decoded into the scratch arena.
: DECODE ( JSON:reader ^place -- JSON:reader ^u8 n ) {: pl:^ :}
   JSON:READ-SPAN$ nip {: raw:n :}
   pl PLACE-WALK @ WALK-SCRATCH @ raw 1 max ARENA:ALLOC drop {: dst:^ :}
   dst raw JSON:READ-STR  dst swap ;

: ADD ( ^u8 n ^place -- ) {: s:^ u:n pl:^ :}
   s u BUF:N>BLEN pl PLACE-WALK @ WALK-BUF @ BUF:APPEND-SPAN ;

\ Text quoted from the value or the schema is cut at CUT bytes.
32 constant CUT

: ADD-CUT ( ^u8 n ^place -- ) {: s:^ u:n pl:^ :}
   s u CUT UTF8:FIT {: k:n :}
   s k pl ADD  k u < if s" ..." pl ADD then ;

: QUOTE ( ^u8 n ^place -- ) {: s:^ u:n pl:^ :}
   s" `" pl ADD  s u pl ADD-CUT  s" `" pl ADD ;

\ `n things`, word naming one thing with a leading space.
: COUNTED ( n ^u8 n ^place -- ) {: k:n w:^ wu:n pl:^ :}
   k FMT:INT$ pl ADD  w wu pl ADD  k 1 <> if s" s" pl ADD then ;

: SAY-INDEX ( n ^place -- ) {: idx:n pl:^ :}
   s" [" pl ADD  idx FMT:INT$ pl ADD  s" ]" pl ADD ;

\ `.name` for each member and `[i]` for each element from the root down to
\ the place.
: PATH ( ^place -- ) {: pl:^ :}
   pl PLACE-UP @ {: up:^ :}
   up NULL-PTR = if exit then
   up RECURSE
   pl PLACE-INDEX @ {: idx:n :}
   idx 0 >= if idx pl SAY-INDEX exit then
   s" ." pl ADD  pl PLACE-NAME @ SPAN:$ pl ADD-CUT ;

: EXPECTED ( ^place -- ) {: pl:^ :}
   pl PATH  s" : expected " pl ADD ;

\ `, found` and the current value's text; the reader may move to the
\ value's last token.
: FOUND ( JSON:reader ^place -- JSON:reader ) {: pl:^ :}
   s" , found " pl ADD  JSON:READ-ANY-SPAN$ pl QUOTE ;

: FIX ( ^u8 n ^place -- ) {: s:^ u:n pl:^ :}
   s" ; " pl ADD  s u pl ADD ;

\ ---- types -----------------------------------------------------------------

$10 constant NUMBER-BIT
$40 constant INTEGER-BIT

\ An integer lexeme is of type integer when a signed cell holds it.
: INTEGER-TYPES ( JSON:reader -- JSON:reader n )
   JSON:READ-SPAN$ STR>NUMBER? MATCH option
      none OF NUMBER-BIT ENDOF
      some OF drop NUMBER-BIT INTEGER-BIT or ENDOF
   ;MATCH ;

\ The bits of the types the current value has.
: VALUE-TYPES ( JSON:reader -- JSON:reader n )
   JSON:READ-TOKEN case
      JSON:T-NULL of 1 endof
      JSON:T-TRUE of 2 endof
      JSON:T-FALSE of 2 endof
      JSON:T-OBJ of 4 endof
      JSON:T-ARR of 8 endof
      JSON:T-FLOAT of NUMBER-BIT endof
      JSON:T-STR of $20 endof
      JSON:T-INT of INTEGER-TYPES endof
      0 swap
   endcase ;

: NAME-AT ( n -- ^u8 n n ) {: start:n :}
   TYPE-NAMES$ STR-SPACE start SPLIT-NEXT drop ;

: TYPE-NAME ( n -- ^u8 n ) {: idx:n :}
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

: TYPE-FIX$ ( ^node bool -- ^u8 n ) {: nd:^ num:bool :}
   nd NODE-TYPES @ {: t:n :}
   num t INTEGER-BIT and 0<> and if
      s" send a whole number from -9223372036854775808 to 9223372036854775807, with no fraction or exponent"
      exit
   then
   t dup 1- and 0= if s" send a value of that type" exit then
   s" send a value of one of those types" ;

: WRONG-TYPE ( JSON:reader ^node ^place -- JSON:reader ) {: nd:^ pl:^ :}
   JSON:READ-TOKEN NUMBER? {: num:bool :}
   pl EXPECTED  s" a value of type " pl ADD  nd pl SAY-TYPES
   pl FOUND  nd num TYPE-FIX$ pl FIX ;

\ ---- enum and numbers ------------------------------------------------------

: HUGE ( JSON:reader ^place -- JSON:reader ) {: pl:^ :}
   pl EXPECTED
   s" a number whose exponent a signed 64-bit cell holds" pl ADD
   pl FOUND  s" write the number with a smaller exponent" pl FIX ;

\ Whether an element of the array text e equals the value text v.
: ELEMENT? ( ^u8 n ^u8 n ^JSON:cursor -- bool )
   {: v:^ vu:n e:^ eu:n c:^ :}
   c e eu JSON:READ-INIT JSON:READ-NEXT drop
   begin JSON:READ-NEXT JSON:T-ARR-END <> while
      JSON:READ-ANY-SPAN$ v vu JSON:VALUE= if
         JSON:READ-FINISH true exit
      then
   repeat
   JSON:READ-FINISH false ;

: UNLISTED ( JSON:reader ^node ^place -- JSON:reader ) {: nd:^ pl:^ :}
   pl EXPECTED  s" one of " pl ADD  nd NODE-VALUES @ SPAN:$ pl QUOTE
   pl FOUND  s" send one of those values" pl FIX ;

\ The value text v against the node's enum. A number VALUE= cannot read,
\ COMPILE having read the enum's, is the value's.
: ENUM-OK? ( JSON:reader ^u8 n ^node ^place -- JSON:reader bool )
   {: v:^ vu:n nd:^ pl:^ :}
   nd NODE-VALUES @ SPAN:$ {: e:^ eu:n :}
   eu 0= if true exit then
   pl PLACE-WALK @ WALK-VALUES {: c:^ :}
   false [: drop v vu e eu c ELEMENT? ;] catch {: in code:n :}
   code 0= if
      in if true exit then
      nd pl UNLISTED false exit
   then
   code E-JSON-READ-NUMBER = if pl HUGE false exit then
   code throw ;

\ A bound's lexeme is quoted in the text: `at least` for a minimum.
: OUTSIDE ( JSON:reader SPAN:span<u8> bool ^place -- JSON:reader )
   {: b most:bool pl:^ :}
   pl EXPECTED
   most if s" a number of at most " else s" a number of at least " then
   pl ADD  b SPAN:$ pl QUOTE
   pl FOUND  s" send a number in that range" pl FIX ;

\ Whether the current number keeps to the bound b, a maximum when most;
\ an empty b states none.
: BOUND? ( JSON:reader SPAN:span<u8> bool ^place -- JSON:reader bool )
   {: b most:bool pl:^ :}
   b SPAN:$ nip 0= if true exit then
   most if 1 else -1 then {: wrong:n :}
   JSON:READ-SPAN$ b SPAN:$ JSON:NUMBER-COMPARE MATCH option
      none OF pl HUGE false ENDOF
      some OF wrong = if b most pl OUTSIDE false else true then ENDOF
   ;MATCH ;

: NUMBER-OK? ( JSON:reader ^node ^place -- JSON:reader bool )
   {: nd:^ pl:^ :}
   nd NODE-MINIMUM @ false pl BOUND? 0= if false exit then
   nd NODE-MAXIMUM @ true pl BOUND? ;

\ ---- strings ---------------------------------------------------------------

\ The text's Unicode characters; a byte that starts none counts one.
: CHARACTERS ( ^u8 n -- n ) {: s:^ u:n :}
   0 0 begin dup u < while
      s u rot UTF8:NEXT MATCH UTF8:scalar-step
         scalar OF nip ENDOF
         raw-byte OF nip ENDOF
      ;MATCH
      swap 1+ swap
   repeat drop ;

\ A string of k characters breaks the length n, a most when most.
: BAD-LENGTH ( JSON:reader n bool ^place -- JSON:reader )
   {: k:n most:bool pl:^ :}
   pl EXPECTED
   most if s" a string of at most " else s" a string of at least " then
   pl ADD  k s"  character" pl COUNTED
   pl FOUND
   most if s" send a shorter string" else s" send a longer string" then
   pl FIX ;

: UNMATCHED ( JSON:reader ^node ^place -- JSON:reader ) {: nd:^ pl:^ :}
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

defer CHECK ( ^u8 n ^node ^place -- bool )

\ The quoted names of the properties from p on: `a`, `b` and `c`.
: SAY-NAMES ( ^property ^place -- ) {: p:^ pl:^ :}
   p begin dup NULL-PTR <> while
      dup PROPERTY-NAME @ SPAN:$ pl QUOTE
      PROPERTY-NEXT @ dup NULL-PTR <> if
         dup PROPERTY-NEXT @ NULL-PTR = if s"  and " else s" , " then
         pl ADD
      then
   repeat drop ;

\ A member additionalProperties false rejects; kid is its place.
: EXTRA ( ^node ^place -- ) {: nd:^ kid:^ :}
   kid PLACE-UP @ {: pl:^ :}  nd NODE-PROPERTIES @ {: p:^ :}
   pl EXPECTED
   p NULL-PTR = if s" no members" else s" only the member" then pl ADD
   p NULL-PTR <> if
      p PROPERTY-NEXT @ NULL-PTR <> if s" s" pl ADD then
      s"  " pl ADD  p pl SAY-NAMES
   then
   s" , found " pl ADD  kid PLACE-NAME @ SPAN:$ pl QUOTE
   p NULL-PTR = if s" remove it" pl FIX exit then
   p PROPERTY-NEXT @ NULL-PTR <> if
      s" rename it to one of those or remove it" pl FIX exit
   then
   s" ; rename it to " pl ADD  p PROPERTY-NAME @ SPAN:$ pl QUOTE
   s"  or remove it" pl ADD ;

\ The key is current; kid is the member's place.
: MEMBER ( JSON:reader ^node ^place -- JSON:reader bool )
   {: nd:^ kid:^ :}
   nd NODE-PROPERTIES @ PROPERTY-NAMED {: p:^ :}
   kid DECODE SPAN:MAKE kid PLACE-NAME !
   JSON:READ-NEXT drop  JSON:READ-ANY-SPAN$ {: v:^ vu:n :}
   p NULL-PTR <> if v vu p PROPERTY-NODE @ kid CHECK exit then
   nd NODE-EXTRAS @ MATCH extras
      open OF true ENDOF
      closed OF nd kid EXTRA false ENDOF
      ruled OF v vu nd NODE-ADDITIONAL @ kid CHECK ENDOF
   ;MATCH ;

\ Each member of the object whose `{` is current, and their count.
: MEMBERS ( JSON:reader ^node ^place -- JSON:reader n bool )
   {: nd:^ kid:^ :}
   0 begin
      >r JSON:READ-NEXT JSON:T-OBJ-END <> r> swap
   while
      >r nd kid MEMBER r> swap
      0= if false exit then
      1+
   repeat true ;

\ Whether the object text o holds a member the entry names.
: HAS? ( ^u8 n ^entry ^JSON:cursor -- bool ) {: o:^ ou:n en:^ c:^ :}
   c o ou JSON:READ-INIT JSON:READ-NEXT drop
   en ENTRY-NAME @ SPAN:$ JSON:READ-FIND-KEY >r JSON:READ-CLOSE r> ;

\ The first entry from en on the object text o lacks, else NULL-PTR.
: MISSING ( ^u8 n ^entry ^JSON:cursor -- ^entry )
   {: o:^ ou:n en:^ c:^ :}
   en NULL-PTR = if en exit then
   o ou en c HAS? 0= if en exit then
   o ou en ENTRY-NEXT @ c RECURSE ;

: ABSENT ( ^entry ^place -- ) {: en:^ pl:^ :}
   pl EXPECTED  s" a member " pl ADD  en ENTRY-NAME @ SPAN:$ pl QUOTE
   s" , found none; add " pl ADD  en ENTRY-NAME @ SPAN:$ pl QUOTE ;

: TOO-MANY ( n n ^place -- ) {: most:n k:n pl:^ :}
   pl EXPECTED  s" at most " pl ADD  most s"  member" pl COUNTED
   s" , found " pl ADD  k s"  member" pl COUNTED
   s" remove a member" pl FIX ;

\ The object text o, whose `{` is current, against the node.
: OBJECT-OK? ( JSON:reader ^u8 n ^node ^place -- JSON:reader bool )
   {: o:^ ou:n nd:^ pl:^ :}
   pl CHILD {: kid:^ :}
   nd kid MEMBERS {: k:n ok:bool :}
   ok 0= if false exit then
   o ou nd NODE-REQUIRED @ kid PLACE-CURSOR MISSING {: en:^ :}
   en NULL-PTR <> if en pl ABSENT false exit then
   nd NODE-MAX-PROPERTIES @ {: most:n :}
   most 0 >= k most > and if most k pl TOO-MANY false exit then
   true ;

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
: NEW-TABLE ( ^u8 n n ^place -- ^table ) {: v:^ vu:n k:n pl:^ :}
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
: ELEMENT$ ( n ^table -- ^u8 n ) {: idx:n tb:^ :}
   idx tb ROW {: r:^ :}
   tb TABLE-TEXT @ r 1 cells + @ +  r 2 cells + @ ;

: DECODED$ ( n ^table -- ^u8 n ) {: idx:n tb:^ :}
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
: RECORD ( JSON:reader ^u8 n n ^table -- JSON:reader )
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

: BYTE-ORDER ( ^u8 n ^u8 n -- n ) {: a:^ u:n b:^ w:n :}
   a u b w STR< if -1 exit then
   b w a u STR< if 1 else 0 then ;

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

: HUGE-AT ( n ^table ^place -- ) {: idx:n tb:^ kid:^ :}
   idx kid PLACE-INDEX !
   kid PLACE-CURSOR idx tb ELEMENT$ JSON:READ-INIT JSON:READ-NEXT drop
   kid HUGE JSON:READ-FINISH ;

\ The first number NUMBER-COMPARE cannot order, if any, rejected at its
\ place kid.
: ORDERABLE? ( n ^table ^place -- bool ) {: k:n tb:^ kid:^ :}
   k 0 ?do
      i tb HUGE? if i tb kid HUGE-AT unloop false exit then
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
: DISTINCT? ( n ^table ^place ^place -- bool ) {: k:n tb:^ pl:^ kid:^ :}
   k tb kid ORDERABLE? 0= if false exit then
   pl PLACE-WALK @ WALK-SCRATCH @ k cells 1 max ARENA:ALLOC drop
   CELL-VIEW {: order:^ :}
   k 0 ?do i order i cells + ! loop
   order k [: tb BEFORE? ;] SORT:SORT!
   order k tb REPEATED {: a:n b:n :}
   b 0< if true exit then
   a b tb pl DUPLICATE false ;

\ The elements of the array text v.
: COUNT-ELEMENTS ( ^u8 n ^JSON:cursor -- n ) {: v:^ vu:n c:^ :}
   c v vu JSON:READ-INIT JSON:READ-NEXT drop
   0 begin
      >r JSON:READ-NEXT JSON:T-ARR-END <> r> swap
   while
      >r JSON:READ-SKIP-VALUE r> 1+
   repeat
   >r JSON:READ-FINISH r> ;

\ k elements break the count lim, a most when most.
: BAD-COUNT ( n n bool ^place -- ) {: lim:n k:n most:bool pl:^ :}
   pl EXPECTED
   most if s" at most " else s" at least " then pl ADD
   lim s"  item" pl COUNTED
   s" , found " pl ADD  k s"  item" pl COUNTED
   most if s" remove an item" else s" add an item" then pl FIX ;

: COUNT-OK? ( n ^node ^place -- bool ) {: k:n nd:^ pl:^ :}
   nd NODE-MIN-ITEMS @ {: least:n :}  nd NODE-MAX-ITEMS @ {: most:n :}
   k least < if least k false pl BAD-COUNT false exit then
   most 0 >= k most > and if most k true pl BAD-COUNT false exit then
   true ;

\ The element whose first token is current, at index idx, recorded in tb
\ unless tb is NULL-PTR.
: ELEMENT-OK? ( JSON:reader n ^node ^table ^place -- JSON:reader bool )
   {: idx:n items:^ tb:^ kid:^ :}
   idx kid PLACE-INDEX !
   JSON:READ-ANY-SPAN$ {: e:^ eu:n :}
   e eu items kid CHECK 0= if false exit then
   tb NULL-PTR <> if e eu idx tb RECORD then
   true ;

\ Each element of the array whose `[` is current against items.
: ELEMENTS-OK? ( JSON:reader ^node ^table ^place -- JSON:reader bool )
   {: items:^ tb:^ kid:^ :}
   0 begin
      >r JSON:READ-NEXT JSON:T-ARR-END <> r> swap
   while
      >r r@ items tb kid ELEMENT-OK? r> swap
      0= if drop false exit then
      1+
   repeat drop true ;

\ The array text v, whose `[` is current, against the node.
: ARRAY-OK? ( JSON:reader ^u8 n ^node ^place -- JSON:reader bool )
   {: v:^ vu:n nd:^ pl:^ :}
   pl CHILD {: kid:^ :}
   v vu kid PLACE-CURSOR COUNT-ELEMENTS {: k:n :}
   k nd pl COUNT-OK? 0= if false exit then
   nd NODE-ITEMS @ {: items:^ :}
   items NULL-PTR = if true exit then
   nd NODE-UNIQUE @ if v vu k pl NEW-TABLE else NULL-PTR then
   {: tb:^ :}
   items tb kid ELEMENTS-OK? 0= if false exit then
   tb NULL-PTR = if true exit then
   k tb pl kid DISTINCT? ;

\ ---- anyOf and oneOf --------------------------------------------------------

\ The index of an alternative a oneOf value meets.
STRUCTURE pass DERIVE addr
   FIELD next ^pass
   FIELD index n
;STRUCTURE

ARENA PASSES pass

: MARK ( ^place -- NUM:byte-len ) PLACE-WALK @ WALK-BUF @ BUF:LEN@ ;

: BACK ( NUM:byte-len ^place -- ) PLACE-WALK @ WALK-BUF @ BUF:LEN! ;

\ A root place of the place's walk.
: FRESH-ROOT ( ^place -- ^place ) NULL-PTR swap PLACE-WALK @ NEW-PLACE ;

: ANY-OF$ ( -- ^u8 n ) s" one schema of `anyOf` takes" ;
: ONE-OF$ ( -- ^u8 n ) s" exactly one schema of `oneOf` takes" ;

\ `P: expected a value s, found `v``; v may hold the root's blanks.
: CHOICE-HEAD ( ^u8 n ^u8 n ^place -- ) {: v:^ vu:n s:^ su:n pl:^ :}
   pl EXPECTED  s" a value " pl ADD  s su pl ADD
   s" , found " pl ADD  v vu TRIM pl QUOTE ;

\ Alternative k checked at alt, its text after `[k]` on a line of its own.
: ALTERNATIVE ( ^u8 n ^branch n ^place -- bool )
   {: v:^ vu:n br:^ k:n alt:^ :}
   s\" \n  [" alt ADD  k FMT:INT$ alt ADD  s" ]" alt ADD
   v vu br BRANCH-NODE @ alt CHECK ;

\ Whether v meets an alternative from br on, k br's index.
: ANY? ( ^u8 n ^branch n ^place -- bool ) {: v:^ vu:n br:^ k:n alt:^ :}
   br NULL-PTR = if false exit then
   v vu br k alt ALTERNATIVE if true exit then
   v vu br BRANCH-NEXT @ k 1+ alt RECURSE ;

: ANY-OF-OK? ( ^u8 n ^node ^place -- bool ) {: v:^ vu:n nd:^ pl:^ :}
   nd NODE-ANY-OF @ {: br:^ :}
   br NULL-PTR = if true exit then
   pl MARK {: at :}
   v vu ANY-OF$ pl CHOICE-HEAD  s" meet one of them:" pl FIX
   v vu br 0 pl FRESH-ROOT ANY? 0= if false exit then
   at pl BACK true ;

\ The indexes of the alternatives from br on that v meets, ascending.
: PASSING ( ^u8 n ^branch n ^place -- ^pass ) {: v:^ vu:n br:^ k:n alt:^ :}
   br NULL-PTR = if NULL-PTR exit then
   v vu br k alt ALTERNATIVE {: ok:bool :}
   v vu br BRANCH-NEXT @ k 1+ alt RECURSE {: rest:^ :}
   ok 0= if rest exit then
   alt PLACE-WALK @ WALK-SCRATCH @ PASSES-NEW {: p:^ :}
   rest p PASS-NEXT !  k p PASS-INDEX !  p ;

\ The indexes from p on: `[0], [1] and [2]`.
: SAY-PASSES ( ^pass ^place -- ) {: p:^ pl:^ :}
   p begin dup NULL-PTR <> while
      s" [" pl ADD  dup PASS-INDEX @ FMT:INT$ pl ADD  s" ]" pl ADD
      PASS-NEXT @ dup NULL-PTR <> if
         dup PASS-NEXT @ NULL-PTR = if s"  and " else s" , " then
         pl ADD
      then
   repeat drop ;

: AMBIGUOUS ( ^u8 n ^pass ^place -- ) {: v:^ vu:n p:^ pl:^ :}
   v vu ONE-OF$ pl CHOICE-HEAD
   s" , which schemas " pl ADD  p pl SAY-PASSES  s"  take" pl ADD
   s" send a value only one of them takes" pl FIX ;

: ONE-OF-OK? ( ^u8 n ^node ^place -- bool ) {: v:^ vu:n nd:^ pl:^ :}
   nd NODE-ONE-OF @ {: br:^ :}
   br NULL-PTR = if true exit then
   pl MARK {: at :}
   v vu ONE-OF$ pl CHOICE-HEAD  s" meet one of them:" pl FIX
   v vu br 0 pl FRESH-ROOT PASSING {: p:^ :}
   p NULL-PTR = if false exit then
   at pl BACK
   p PASS-NEXT @ NULL-PTR = if true exit then
   v vu p pl AMBIGUOUS false ;

\ ---- judging ---------------------------------------------------------------

\ The rules of the value's kind: a number's, a string's or an object's.
: KIND-OK? ( JSON:reader ^u8 n ^node ^place -- JSON:reader bool )
   {: v:^ vu:n nd:^ pl:^ :}
   JSON:READ-TOKEN {: kind:n :}
   kind NUMBER? if nd pl NUMBER-OK? exit then
   kind JSON:T-STR = if nd pl STRING-OK? exit then
   kind JSON:T-OBJ = if v vu nd pl OBJECT-OK? exit then
   kind JSON:T-ARR = if v vu nd pl ARRAY-OK? exit then
   true ;

\ The value text v, whose first token is current, against the node; the
\ reader ends anywhere in the value.
: JUDGE ( JSON:reader ^u8 n ^node ^place -- JSON:reader bool )
   {: v:^ vu:n nd:^ pl:^ :}
   VALUE-TYPES {: bits:n :}  nd NODE-TYPES @ {: t:n :}
   t 0<> t bits and 0= and if nd pl WRONG-TYPE false exit then
   v vu nd pl ENUM-OK? 0= if false exit then
   v vu nd pl KIND-OK? 0= if false exit then
   v vu nd pl ANY-OF-OK? 0= if false exit then
   v vu nd pl ONE-OF-OK? ;

\ The value text v against the node, read with the place's cursor.
: CHECK-VALUE ( ^u8 n ^node ^place -- bool ) {: v:^ vu:n nd:^ pl:^ :}
   pl PLACE-CURSOR v vu JSON:READ-INIT JSON:READ-NEXT drop
   v vu nd pl JUDGE  >r JSON:READ-FINISH r> ;

: INSTALL-CHECK ( -- ) [: CHECK-VALUE ;] is CHECK ;
INSTALL-CHECK

\ s is the scratch arena.
: WALK-ROOT ( ^u8 n schema ^n ^ARENA:arena -- bool )
   {: v:^ vu:n sc buf:^ s:^ :}
   s [: WALK-VALUES JSON:READ-DISPOSE ;] WALKS-NEW-DISPOSE {: w:^ :}
   buf w WALK-BUF !  s w WALK-SCRATCH !
   v vu sc SCHEMA-UNMAKE NULL-PTR w NEW-PLACE CHECK ;

public

\ The schema's nodes, in arena a; a schema outside the contract throws
\ E-SCHEMA with its text.
: COMPILE ( ^u8 n ^ARENA:arena -- schema )
   {: src:^ u:n a:^ :}
   src u a [: READ-ROOT ;] ARENA:WITH ;

\ Whether the value text satisfies the schema; false appends the first
\ violation's text to the BUF.
: VALID? ( ^u8 n schema ^a -- bool ) {: v:^ vu:n sc buf:^ :}
   buf BUF:LEN@ {: at :}
   buf BYTE-VIEW CELL-VIEW {: cells:^ :}
   false [: drop v vu sc cells [: WALK-ROOT ;] ARENA:WITH ;] catch
   {: ok code:n :}
   code 0= if ok exit then
   at buf BUF:LEN!  code throw ;

;package

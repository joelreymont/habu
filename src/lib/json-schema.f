split-str
\ json-schema.f - JSON Schema, package JSON-SCHEMA: COMPILE reads a schema's
\ text into nodes that state what a JSON value must be.
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
\ take, or text that is no JSON throws E-SCHEMA through REASON:THROW with
\ the text `L:C: expected X, found Y; fix`, line and byte column in the
\ schema text. Each sub-schema is read by a nested call, so the depth of
\ the recursion is the schema's own.
\ STORAGE CLASS. CALLER-OWNED: the nodes lie in the arena COMPILE was given,
\ read-only after COMPILE; they borrow the schema text, which must outlive
\ the schema.

require src/lib/error.f
require src/lib/string.f
require src/lib/span.f
require src/lib/fmt.f
require src/lib/adt/option.f
require src/lib/arena.f
require src/lib/reason.f
require src/lib/json.f
require src/lib/regex.f
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

\ A node is one schema object. types holds bit i for the i-th name of
\ TYPE-NAMES$, 0 when the schema states no type; an empty span, a -1
\ count or a null node states no rule. values (enum's array), minimum and
\ maximum borrow the schema text; names and pattern-text, the pattern as
\ it decodes, lie in the arena.
MUTUAL node property branch
STRUCTURE node DERIVE addr
   FIELD types n
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

\ One COMPILE call: the arena the nodes go in, the scratch arena and the
\ schema text, whose start rejections are placed from.
STRUCTURE build DERIVE addr
   FIELD arena ^ARENA:arena
   FIELD scratch ^ARENA:arena
   FIELD text ^u8
   FIELD root ^node
;STRUCTURE

\ One schema object being read; unique-at is the offset of uniqueItems'
\ `true`, 0 when it states none.
STRUCTURE level DERIVE addr
   FIELD build ^build
   FIELD node ^node
   FIELD unique-at n
;STRUCTURE

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

: ADD-TYPE ( JSON:reader ^node -- JSON:reader ) {: nd:^ :}
   NAMED-TYPE {: bit:n :}
   nd NODE-TYPES @ bit and 0<> if
      s" `type` naming each type once" s" remove the repeated type name"
      REJECT
   then
   nd NODE-TYPES @ bit or nd NODE-TYPES ! ;

: TYPE-LIST ( JSON:reader ^node -- JSON:reader ) {: nd:^ :}
   begin JSON:READ-NEXT JSON:T-ARR-END <> while nd ADD-TYPE repeat
   nd NODE-TYPES @ 0<> if exit then
   s" `type` as an array of one type name or more"
   s" name a type, or remove `type`" REJECT ;

: TYPE-RULE ( JSON:reader ^node -- JSON:reader ) {: nd:^ :}
   JSON:READ-TOKEN JSON:T-ARR = if nd TYPE-LIST exit then
   NAMED-TYPE nd NODE-TYPES ! ;

\ Whether an array's text holds only white space between its brackets.
: EMPTY? ( ^u8 n -- bool ) {: a:^ u:n :}
   u 1- 1 ?do
      a i + c@ STR-SPACE > if unloop false exit then
   loop true ;

: ENUM-EXPECTED$ ( -- ^u8 n )
   s" `enum` as an array of one value or more" ;
: ENUM-FIX$ ( -- ^u8 n ) s" list the values allowed in an array" ;

: ENUM-RULE ( JSON:reader ^node -- JSON:reader ) {: nd:^ :}
   JSON:READ-TOKEN JSON:T-ARR <> if
      ENUM-EXPECTED$ ENUM-FIX$ REJECT
   then
   JSON:READ-VALUE-SPAN$ {: a:^ u:n :}
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
      3 of s" `minimum` as a number" LEXEME nd NODE-MINIMUM ! endof
      4 of s" `maximum` as a number" LEXEME nd NODE-MAXIMUM ! endof
      5 of s" `minLength` as an integer from 0 to 9223372036854775807"
         LIMIT nd NODE-MIN-LENGTH ! endof
      6 of s" `maxLength` as an integer from 0 to 9223372036854775807"
         LIMIT nd NODE-MAX-LENGTH ! endof
      7 of lv PATTERN-RULE endof
   endcase ;

\ ---- objects ---------------------------------------------------------------

defer READ-SCHEMA ( JSON:reader ^build -- JSON:reader ^node )

\ The sub-schema whose first token is current; e is the expected text for
\ a token that opens no object.
: SUB ( JSON:reader ^build ^u8 n -- JSON:reader ^node ) {: b:^ e:^ eu:n :}
   JSON:READ-TOKEN JSON:T-OBJ <> if
      e eu s" write a schema object, `{}` for any value" REJECT
   then
   b READ-SCHEMA ;

\ Whether the property is named as the current key; p stays.
: SAME-NAME? ( JSON:reader ^property -- JSON:reader ^property bool )
   {: p:^ :}
   p PROPERTY-NAME @ SPAN:$ JSON:READ-STR-EQ? p swap ;

\ Whether a property from p on is named as the current key.
: NAMED? ( JSON:reader ^property -- JSON:reader bool )
   begin dup NULL-PTR <> while
      SAME-NAME? if drop true exit then PROPERTY-NEXT @
   repeat drop false ;

: APPEND-PROPERTY ( ^property ^node -- )
   NODE-PROPERTIES
   begin dup @ NULL-PTR <> while @ PROPERTY-NEXT repeat ! ;

\ The key is current; the reader ends on its value's last token.
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
   begin JSON:READ-NEXT JSON:T-OBJ-END <> while lv PROPERTY repeat ;

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

\ e is the expected text for the value, el for an element.
: CHOICE-RULE ( JSON:reader ^build ^^branch ^u8 n ^u8 n -- JSON:reader )
   {: b:^ head:^ e:^ eu:n el:^ elu:n :}
   JSON:READ-TOKEN JSON:T-ARR <> if e eu CHOICE-FIX$ REJECT then
   begin JSON:READ-NEXT JSON:T-ARR-END <> while
      b head el elu BRANCH
   repeat
   head @ NULL-PTR <> if exit then
   e eu CHOICE-FIX$ REJECT ;

: CHOICE-KEYWORD ( JSON:reader n ^level -- JSON:reader ) {: idx:n lv:^ :}
   lv LEVEL-BUILD @ {: b:^ :}  lv LEVEL-NODE @ {: nd:^ :}
   idx 16 = if
      b nd NODE-ANY-OF s" `anyOf` as an array of one schema object or more"
      s" an `anyOf` element as a schema object" CHOICE-RULE exit
   then
   b nd NODE-ONE-OF s" `oneOf` as an array of one schema object or more"
   s" a `oneOf` element as a schema object" CHOICE-RULE ;

\ ---- schemas ---------------------------------------------------------------

\ The keywords READ-MEMBERS takes at the root, each optional; KEYWORD reads
\ by the index in this list. A sub-schema takes the list past `$schema? `.
: ROOT-KEYWORDS$ ( -- ^u8 n )
   s" $schema? type? enum? minimum? maximum? minLength? maxLength? pattern? properties? required? additionalProperties? maxProperties? items? minItems? maxItems? uniqueItems? anyOf? oneOf? description? title? default? examples? deprecated? readOnly? writeOnly? $comment? format?" ;

: KEYWORDS$ ( -- ^u8 n )
   ROOT-KEYWORDS$ {: a:^ u:n :}  s" $schema? " nip {: k:n :}
   a k +  u k - ;

\ Keywords from 18 on state no rule.
: KEYWORD ( JSON:reader n ^level -- JSON:reader ) {: idx:n lv:^ :}
   idx 8 < if idx lv VALUE-KEYWORD exit then
   idx 12 < if idx lv OBJECT-KEYWORD exit then
   idx 16 < if idx lv ARRAY-KEYWORD exit then
   idx 18 < if idx lv CHOICE-KEYWORD exit then
   JSON:READ-SKIP-VALUE ;

: NEW-NODE ( ^build -- ^node )
   BUILD-ARENA @ NODES-NEW {: nd:^ :}
   -1 nd NODE-MIN-LENGTH !  -1 nd NODE-MAX-LENGTH !
   -1 nd NODE-MAX-PROPERTIES !
   -1 nd NODE-MIN-ITEMS !  -1 nd NODE-MAX-ITEMS !
   nd ;

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

\ The schema object whose `{` is current, read over the keyword list; base
\ is the root index of the list's first keyword.
: OBJECT ( JSON:reader ^build ^u8 n n -- JSON:reader ^node )
   {: b:^ list:^ lu:n base:n :}
   b BUILD-SCRATCH @ LEVELS-NEW {: lv:^ :}
   b lv LEVEL-BUILD !  b NEW-NODE lv LEVEL-NODE !
   list lu [: base + lv KEYWORD ;] JSON:READ-MEMBERS
   lv CHECK-UNIQUE
   lv LEVEL-NODE @ ;

: SUB-SCHEMA ( JSON:reader ^build -- JSON:reader ^node ) {: b:^ :}
   b KEYWORDS$ 1 OBJECT ;

: INSTALL ( -- ) [: SUB-SCHEMA ;] is READ-SCHEMA ;
INSTALL

: ROOT ( JSON:reader ^build -- JSON:reader ^node ) {: b:^ :}
   JSON:READ-TOKEN JSON:T-OBJ <> if
      s" a schema object" s" write the schema as a JSON object" REJECT
   then
   b ROOT-KEYWORDS$ 0 OBJECT ;

\ s is the scratch arena; a throw leaves the cursor owning nothing.
: READ-ROOT ( ^u8 n ^ARENA:arena ^ARENA:arena -- schema )
   {: src:^ u:n a:^ s:^ :}
   s BUILDS-NEW {: b:^ :}
   a b BUILD-ARENA !  s b BUILD-SCRATCH !  src b BUILD-TEXT !
   s CURSORS-NEW {: c:^ :}
   [: c src u JSON:READ-INIT JSON:READ-NEXT drop
      b ROOT b BUILD-ROOT ! JSON:READ-FINISH ;] catch {: code:n :}
   code 0<> if c JSON:READ-DISPOSE code RETHROW then
   b BUILD-ROOT @ SCHEMA-MAKE ;

public

\ The schema's nodes, in arena a; a schema outside the contract throws
\ E-SCHEMA with its text.
: COMPILE ( ^u8 n ^ARENA:arena -- schema )
   {: src:^ u:n a:^ :}
   src u a [: READ-ROOT ;] ARENA:WITH ;

;package

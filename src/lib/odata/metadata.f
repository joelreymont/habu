\ metadata.f - METADATA and the model's accessors; part of package ODATA
\ (src/lib/odata.f), which states the contract. METADATA reads the document
\ in one pass of phases; an element it does not read is skipped whole,
\ counting the open elements, and reading resumes in the phase it left.
\ While METADATA reads, every name it looks up or checks for a duplicate is
\ found in one MAP of keys, each the name's kind as one byte and what makes
\ the name unique (a record's address, a namespace, the name). Where several
\ records answer one key, the map holds a binding's first two, and a term's
\ copy with the least offset seen on one target: a target's annotations are in
\ no order, so each repeat makes the later of it and the held copy a candidate,
\ and the least candidate, where some term's count first reaches two, is
\ rejected. One walk of the base types, depth first
\ without a stack, numbers the types from 1 and gives each type the last
\ number of the types under it: U is T or a base type of T exactly when U's
\ rank <= T's rank <= U's rank-end. A member is redeclared when a type the
\ walk is in already holds its name. The model's index lists each name's
\ members in their owners' rank order, so T's member of a name is the last
\ whose owner's rank is at most T's, when T's rank is within that owner's.
\ A set's navigation leads where the model's `targets` map says, under the
\ set and the navigation (V4) or its from end (V2), when the navigation's
\ owner is the set's type or a base of it.

require src/lib/error.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/heap.f
require src/lib/arena.f
require src/lib/fmt.f
require src/lib/reason.f
require src/core/rejection.f
require src/lib/xml.f
require src/lib/map.f
require src/lib/adt/option.f
require src/lib/adt/nullable.f
require src/lib/sort.f
require src/lib/vector.f
require src/lib/map-core.f
require src/lib/xml/read.f
require src/lib/xml/state.f

package ODATA
public

-9930 s" a $metadata document the library rejects: correct it where its reason text says"
   ERROR:CODE E-METADATA
-9931 s" a response body the library rejects: correct it where its reason text says"
   ERROR:CODE E-RESPONSE
-9932 s" a key value that does not fit its property's type: correct it as its reason text says"
   ERROR:CODE E-KEY

private
-9930 constant E-FIRST
-9939 constant E-LAST

ENUM version v2 v4 ;ENUM

\ An annotation's namespace and name, and its value's text.
STRUCTURE annotation DERIVE addr
   FIELD namespace str
   FIELD name str
   FIELD text str
   FIELD offset n
   FIELD next ^annotation
;STRUCTURE

STRUCTURE path DERIVE addr
   FIELD text str
   FIELD next ^path
;STRUCTURE

\ A V4 Capabilities.FilterRestrictions: its term's namespace, an alias resolved
\ when read, its Filterable and its NonFilterableProperties paths.
STRUCTURE filter DERIVE addr
   FIELD namespace str
   FIELD filterable bool
   FIELD paths ^path
   FIELD offset n
   FIELD next ^filter
;STRUCTURE

\ A V2 Association End: many when its Multiplicity is `*`.
STRUCTURE association-end DERIVE addr
   FIELD role str
   FIELD type str
   FIELD many bool
   FIELD offset n
   FIELD next ^association-end
;STRUCTURE

STRUCTURE association DERIVE addr
   FIELD name str
   FIELD namespace str
   FIELD ends ^association-end
   FIELD offset n
   FIELD next ^association
;STRUCTURE

\ A key property's type: an Edm type V2, V4 or both write in a key, a V4
\ enumeration type, or `undeclared`, a V4 type the document does not declare.
ENUM key-kind DERIVE eq
   string boolean guid binary byte sbyte int16 int32 int64 decimal double
   single date-time date-time-offset time date time-of-day duration
   enumeration undeclared
;ENUM

STRUCTURE enum-member DERIVE addr
   FIELD name str
   FIELD next ^enum-member
;STRUCTURE

\ A V4 EnumType; a value of a `flags` one names one or more members.
STRUCTURE enum-type DERIVE addr
   FIELD name str
   FIELD namespace str
   FIELD flags bool
   FIELD members ^enum-member
;STRUCTURE

\ A V4 TypeDefinition's UnderlyingType.
STRUCTURE type-definition DERIVE addr
   FIELD underlying str
;STRUCTURE

\ `names` maps the name of a set, a singleton or a type's member to its
\ `named`; `targets` maps to a binding's target (V4) or an association set (V2).
\ A set's `filters` are the FilterRestrictions read for it, `filter` the one
\ kept, null for none.
\ A key-ref's `enum-type` is set when its `kind` is `enumeration`.
\ An entity type's `rank-end` is 0 until the walk of the base types leaves it.
\ `types` heads the ENTITY-TYPE-NEXT chain of every type. No set of a
\ `hidden` type is served, and every navigation to one has a hidden name.
\ A property's `complex-type` is the complex type its Type names, or names in
\ `Collection(...)` with `many` set; null for any other.
public
MUTUAL metadata entity-set entity-type property navigation key-ref
STRUCTURE metadata OPAQUE DERIVE addr
   FIELD version version
   FIELD sets ^entity-set
   FIELD names ^n
   FIELD targets ^n
   FIELD types ^entity-type
;STRUCTURE
STRUCTURE entity-set OPAQUE DERIVE addr
   FIELD name str
   FIELD type-name str
   FIELD type ^entity-type
   FIELD filters ^filter
   FIELD filter ^filter
   FIELD offset n
   FIELD next ^entity-set
;STRUCTURE
STRUCTURE entity-type OPAQUE DERIVE addr
   FIELD name str
   FIELD namespace str
   FIELD complex bool
   FIELD hidden bool
   FIELD version version
   FIELD base-name str
   FIELD base ^entity-type
   FIELD derived ^entity-type
   FIELD sibling ^entity-type
   FIELD rank n
   FIELD rank-end n
   FIELD depth n
   FIELD key-holder ^entity-type
   FIELD property-base ^entity-type
   FIELD navigation-base ^entity-type
   FIELD model ^metadata
   FIELD properties ^property
   FIELD navigations ^navigation
   FIELD key ^key-ref
   FIELD annotations ^annotation
   FIELD offset n
   FIELD next ^entity-type
;STRUCTURE
STRUCTURE property OPAQUE DERIVE addr
   FIELD name str
   FIELD type str
   FIELD nullable bool
   FIELD complex-type ^entity-type
   FIELD many bool
   FIELD owner ^entity-type
   FIELD annotations ^annotation
   FIELD offset n
   FIELD next ^property
;STRUCTURE
STRUCTURE navigation OPAQUE DERIVE addr
   FIELD name str
   FIELD type str
   FIELD relationship str
   FIELD from-role str
   FIELD to-role str
   FIELD association ^association
   FIELD from ^association-end
   FIELD to ^association-end
   FIELD target-type ^entity-type
   FIELD many bool
   FIELD owner ^entity-type
   FIELD offset n
   FIELD next ^navigation
;STRUCTURE
STRUCTURE key-ref OPAQUE DERIVE addr
   FIELD name str
   FIELD property ^property
   FIELD kind key-kind
   FIELD enum-type ^enum-type
   FIELD offset n
   FIELD next ^key-ref
;STRUCTURE
;MUTUAL

private

\ A `hidden` name hides every property and navigation of it, on every type;
\ `singleton` is the type of the singleton of the name, null for none.
STRUCTURE named DERIVE addr
   FIELD set ^entity-set
   FIELD singleton ^entity-type
   FIELD properties ^^property
   FIELD property-count n
   FIELD navigations ^^navigation
   FIELD navigation-count n
   FIELD hidden bool
;STRUCTURE

STRUCTURE set-end DERIVE addr
   FIELD role str
   FIELD set-name str
   FIELD end ^association-end
   FIELD set ^entity-set
   FIELD offset n
   FIELD next ^set-end
;STRUCTURE

STRUCTURE association-set DERIVE addr
   FIELD name str
   FIELD association-name str
   FIELD association ^association
   FIELD ends ^set-end
   FIELD offset n
   FIELD next ^association-set
;STRUCTURE

\ A singleton as read, until its type resolves.
STRUCTURE singleton DERIVE addr
   FIELD name str
   FIELD type-name str
   FIELD offset n
   FIELD next ^singleton
;STRUCTURE

STRUCTURE binding DERIVE addr
   FIELD set ^entity-set
   FIELD path str
   FIELD target-name str
   FIELD target ^entity-set
   FIELD offset n
   FIELD next ^binding
;STRUCTURE

\ An `<Annotations Target>` block, attached at the end of the read.
STRUCTURE block DERIVE addr
   FIELD target str
   FIELD annotations ^annotation
   FIELD filters ^filter
   FIELD next ^block
;STRUCTURE

\ The namespace an edmx:Include's or a Schema's Alias names.
STRUCTURE alias DERIVE addr
   FIELD namespace str
;STRUCTURE

\ Keys of the names map. `member-name`: a container child but an entity set;
\ `visible`: a member's name alone, held by the last type the walk entered with it.
ENUM kind DERIVE tag
   type-name association-name property-name navigation-name end-role
   set-name member-name alias-name visible keyed term binding-path
   binding-second link-second singleton-name enum-name definition-name
;ENUM

\ The open CSDL element; `skip` is any element the table does not name.
ENUM phase DERIVE eq
   document edmx reference data-services schema type-body key property-body
   container set-body association-body set-pairs annotations enum-body
   filter-body record paths-value paths path-text bool-value bool-text skip
;ENUM

\ An element's namespace: EDMX of V2 or V4, CSDL of V2 (1.0, 1.1, 2.0) or
\ V4, or another, V3's CSDL among them.
ENUM space DERIVE eq edmx2 edmx4 edm2 edm4 other ;ENUM

\ One METADATA call's state: names holds the map, keys, arrays, scratch and
\ the cursor; `home` is the phase a filter's end returns to.
STRUCTURE doc DERIVE addr
   FIELD model ^metadata
   FIELD arena ^ARENA:arena
   FIELD cursor ^XML:cursor
   FIELD scratch ^VEC:vector<u8>
   FIELD phase phase
   FIELD space space
   FIELD depth n
   FIELD resume phase
   FIELD offset n
   FIELD namespace str
   FIELD container str
   FIELD container-namespace str
   FIELD containers n
   FIELD type ^entity-type
   FIELD property ^property
   FIELD association ^association
   FIELD set ^entity-set
   FIELD association-set ^association-set
   FIELD block ^block
   FIELD filter ^filter
   FIELD home phase
   FIELD enum-type ^enum-type
   FIELD set-last ^entity-set
   FIELD property-last ^property
   FIELD navigation-last ^navigation
   FIELD types ^entity-type
   FIELD type-count n
   FIELD ranked ^^entity-type
   FIELD associations ^association
   FIELD association-sets ^association-set
   FIELD singletons ^singleton
   FIELD bindings ^binding
   FIELD blocks ^block
   FIELD key-ref-last ^key-ref
   FIELD names ^ARENA:arena
   FIELD map ^n
   FIELD key ^u8
   FIELD key-room n
   FIELD key-len n
   FIELD seconds ^^set-end
   FIELD second-count n
   FIELD source str
;STRUCTURE

ARENA DOCS doc
ARENA MODELS metadata
ARENA SETS entity-set
ARENA TYPES entity-type
ARENA PROPERTIES property
ARENA NAVIGATIONS navigation
ARENA ANNOTATIONS annotation
ARENA FILTERS filter
ARENA PATHS path
ARENA ENDS association-end
ARENA ASSOCIATIONS association
ARENA KEY-REFS key-ref
ARENA SET-ENDS set-end
ARENA ASSOCIATION-SETS association-set
ARENA SINGLETONS singleton
ARENA BINDINGS binding
ARENA BLOCKS block
ARENA ALIASES alias
ARENA ENUM-TYPES enum-type
ARENA ENUM-MEMBERS enum-member
ARENA DEFINITIONS type-definition
ARENA NAMEDS named

CAST: >PROPS ( ^u8 -- ^^property )
CAST: >NAVS ( ^u8 -- ^^navigation )
CAST: >SET-ENDS ( ^u8 -- ^^set-end )
CAST: >TYPES ( ^u8 -- ^^entity-type )
\ A record's address a map holds.
CAST: >RECORD ( n -- ^t )

: EDMX2$ ( -- str ) s" http://schemas.microsoft.com/ado/2007/06/edmx" ;
: EDMX4$ ( -- str ) s" http://docs.oasis-open.org/odata/ns/edmx" ;
: EDM4$ ( -- str ) s" http://docs.oasis-open.org/odata/ns/edm" ;

: EDM2? ( str -- bool )
   {: uri:str :}
   uri s" http://schemas.microsoft.com/ado/2008/09/edm" STR:=
   uri s" http://schemas.microsoft.com/ado/2007/05/edm" STR:= or
   uri s" http://schemas.microsoft.com/ado/2006/04/edm" STR:= or ;

: SPACE-OF ( str -- space )
   {: uri:str :}
   uri EDMX2$ STR:= if SPACE-EDMX2 exit then
   uri EDMX4$ STR:= if SPACE-EDMX4 exit then
   uri EDM4$ STR:= if SPACE-EDM4 exit then
   uri EDM2? if SPACE-EDM2 exit then
   SPACE-OTHER ;

: V2-VERSION? ( version -- bool )
   MATCH version
      v2 OF true ENDOF
      v4 OF false ENDOF
   ;MATCH ;

: TYPE-V2? ( ^entity-type -- bool ) ENTITY-TYPE-VERSION @ V2-VERSION? ;
: V2? ( ^doc -- bool ) DOC-MODEL @ METADATA-VERSION @ V2-VERSION? ;

: CSDL ( ^doc -- space ) V2? if SPACE-EDM2 else SPACE-EDM4 then ;
: EDMX ( ^doc -- space ) V2? if SPACE-EDMX2 else SPACE-EDMX4 then ;
: ENTER ( ^doc phase -- ) swap DOC-PHASE ! ;

\ The text before the last dot and the text after it; all and none without one.
: QUALIFIER ( str -- str str )
   {: text:str :}
   text STR:LEN begin dup 0 > while
      1- dup text STR:DATA + c@ [char] . = if
         {: dot:n :} text STR:DATA dot >str text STR:DATA dot + 1+ text STR:LEN dot - 1- >str exit
      then
   repeat drop
   text text STR:DATA 0 >str ;

\ The index of the first slash, -1 for none.
: SLASH ( str -- n )
   {: text:str :}
   text STR:LEN 0 ?do
      text STR:DATA i + c@ [char] / = if i unloop exit then
   loop
   -1 ;

\ The text before index I and the text after it.
: SPLIT ( str n -- str str )
   {: text:str i:n :}
   text STR:DATA i >str text STR:DATA i + 1+ text STR:LEN i - 1- >str ;

: HAS-SLASH? ( str -- bool ) SLASH 0 >= ;

\ CUT keeps the part of a name or value from the document a rejection shows:
\ REASON:QUOTE-MOST bytes back to a character's first byte, and true for `...`
\ after it.
: CUT ( str -- str bool )
   {: text:str :}
   text STR:LEN REASON:QUOTE-MOST <= if text false exit then
   REASON:QUOTE-MOST begin
      dup 0 > if dup text STR:DATA + c@ $C0 and $80 = else false then
   while 1- repeat
   text STR:DATA swap >str true ;

: QUOTE ( str -- )  CUT REASON:QUOTE ;

\ PLACE opens a rejection at a byte of the document, the caller adds what was
\ expected, and FOUND or FOUND-QUOTED adds what was found and the fix and
\ throws.
: PLACE ( ^doc n -- )
   {: d:^ at:n :}
   d DOC-SOURCE @ STR:DATA {: src:^ :}
   REASON:OPEN
   src src at + 1 1 0 REJECTION:POSITION drop REASON:AT
   REASON:EXPECTED ;

: FIX ( str -- )  REASON:FIX REASON:APPEND E-METADATA REASON:STOP ;

\ Found text as given: none, a count, a description.
: FOUND ( str str -- )
   {: found:str fix:str :}
   REASON:FOUND found REASON:APPEND fix FIX ;

: FOUND-QUOTED ( str str -- )
   {: found:str fix:str :}
   REASON:FOUND found QUOTE fix FIX ;

: PLACE-HERE ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d d DOC-OFFSET @ PLACE ;

: NONE$ ( -- str ) s" none" ;

\ Attributes. Each answer is a view of scratch, overwritten by the next.
: SCRATCH$ ( ^doc -- str ) DOC-SCRATCH @ VEC:BYTES ;

: CLEARED ( ^doc -- ^VEC:vector<u8> ) DOC-SCRATCH @ dup 0 VEC:RESIZE ;

: FIND-ATTR ( XML:reader str -- XML:reader bool )
   {: name:str :}
   XML:ATTR-RESET
   begin XML:ATTR-NEXT while
      XML:ATTR-NAME$ name STR:= if true exit then
   repeat
   false ;

: ATTR-TEXT$ ( XML:reader ^doc -- XML:reader str )
   {: d:^ :}
   d CLEARED XML:ATTR-TEXT
   d SCRATCH$ ;

: ATTR$ ( XML:reader ^doc str -- XML:reader str bool )
   {: d:^ name:str :}
   name FIND-ATTR if d ATTR-TEXT$ true exit then
   d SCRATCH$ STR:DATA 0 >str false ;

: NEED$ ( XML:reader ^doc str -- XML:reader str )
   {: d:^ name:str :}
   d name ATTR$ if exit then drop
   d PLACE-HERE XML:LOCAL$ {: el:str :}
   s" attribute `" REASON:APPEND name REASON:APPEND
   s" ` of `" REASON:APPEND el REASON:APPEND
   s" `" REASON:APPEND NONE$ s" add the attribute" FOUND ;

: KEEP ( ^doc str -- str )
   {: d:^ src:str :}
   d DOC-ARENA @ src STR:LEN ARENA:ALLOC {: dst:^ got:n :}
   src STR:DATA dst src STR:LEN BYTE-COPY
   dst got >str ;

: NEED ( XML:reader ^doc str -- XML:reader str )
   {: d:^ name:str :}
   d name NEED$ {: v:str :} d v KEEP ;

: OPTIONAL ( XML:reader ^doc str -- XML:reader str )
   {: d:^ name:str :}
   d name ATTR$ drop {: v:str :} d v KEEP ;

\ Whether the current start tag is NAME in the namespace SPACE.
: IS? ( XML:reader ^doc space str -- XML:reader bool )
   {: d:^ sp:space name:str :}
   d DOC-SPACE @ sp SPACE-EQ 0= if false exit then
   XML:LOCAL$ name STR:= ;

\ Skipping: `skip` counts the elements it enters until the one it began at
\ ends.
: SKIP-START ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d DOC-PHASE @ d DOC-RESUME !
   1 d DOC-DEPTH !
   d PHASE-SKIP ENTER ;

: DEEPER ( XML:reader ^doc -- XML:reader ) 1 swap DOC-DEPTH +! ;

: SHALLOWER ( ^doc -- )
   {: d:^ :}
   -1 d DOC-DEPTH +!
   d DOC-DEPTH @ 0= if d d DOC-RESUME @ ENTER then ;

\ The phase an element's end returns to.
: PARENT ( ^doc phase -- phase )
   {: d:^ p:phase :}
   p MATCH phase
      document OF PHASE-DOCUMENT ENDOF
      edmx OF PHASE-DOCUMENT ENDOF
      reference OF PHASE-EDMX ENDOF
      data-services OF PHASE-EDMX ENDOF
      schema OF PHASE-DATA-SERVICES ENDOF
      type-body OF PHASE-SCHEMA ENDOF
      key OF PHASE-TYPE-BODY ENDOF
      property-body OF PHASE-TYPE-BODY ENDOF
      container OF PHASE-SCHEMA ENDOF
      set-body OF PHASE-CONTAINER ENDOF
      association-body OF PHASE-SCHEMA ENDOF
      set-pairs OF PHASE-CONTAINER ENDOF
      annotations OF PHASE-SCHEMA ENDOF
      enum-body OF PHASE-SCHEMA ENDOF
      filter-body OF d DOC-HOME @ ENDOF
      record OF PHASE-FILTER-BODY ENDOF
      paths-value OF PHASE-RECORD ENDOF
      paths OF PHASE-PATHS-VALUE ENDOF
      path-text OF PHASE-PATHS ENDOF
      bool-value OF PHASE-RECORD ENDOF
      bool-text OF PHASE-BOOL-VALUE ENDOF
      skip OF PHASE-SKIP ENDOF
   ;MATCH ;

\ Keys: each is built in the doc's key buffer, which grows by doubling.
: KEY-ROOM ( ^doc n -- )
   {: d:^ more:n :}
   d DOC-KEY-LEN @ more + {: need:n :}
   need d DOC-KEY-ROOM @ <= if exit then
   need d DOC-KEY-ROOM @ 2 * max {: room:n :}
   d DOC-NAMES @ room ARENA:ALLOC drop {: buf:^ :}
   d DOC-KEY @ buf d DOC-KEY-LEN @ BYTE-COPY
   buf d DOC-KEY !
   room d DOC-KEY-ROOM ! ;

: KEY+ ( ^doc str -- )
   {: d:^ src:str :}
   d src STR:LEN KEY-ROOM
   src STR:DATA d DOC-KEY @ d DOC-KEY-LEN @ + src STR:LEN BYTE-COPY
   src STR:LEN d DOC-KEY-LEN +! ;

: KEY-CELL+ ( ^doc n -- )
   {: d:^ v:n :}
   d 1 cells KEY-ROOM
   v d DOC-KEY @ d DOC-KEY-LEN @ + CELL-VIEW !
   1 cells d DOC-KEY-LEN +! ;

: KEY-AT+ ( ^doc ^t -- ) NULL-PTR - KEY-CELL+ ;

: KEY-NEW ( ^doc kind -- )
   {: d:^ k:kind :}
   0 d DOC-KEY-LEN !
   d 1 KEY-ROOM
   k KIND-TAG d DOC-KEY @ c!
   1 d DOC-KEY-LEN ! ;

: KEY$ ( ^doc -- str ) dup DOC-KEY @ swap DOC-KEY-LEN @ >str ;

\ A name in a namespace.
: QUALIFIED-KEY ( ^doc kind str str -- )
   {: d:^ k:kind ns:str name:str :}
   d k KEY-NEW d ns STR:LEN KEY-CELL+ d ns KEY+ d name KEY+ ;

\ A name of the record at an address.
: OWNED-KEY ( ^doc kind ^t str -- )
   {: d:^ k:kind at:^ name:str :}
   d k KEY-NEW d at KEY-AT+ d name KEY+ ;

: NAME-KEY ( ^doc kind str -- )
   {: d:^ k:kind name:str :}
   d k KEY-NEW d name KEY+ ;

\ Two records' addresses.
: PAIR-KEY ( ^doc kind ^a ^b -- )
   {: d:^ k:kind a:^ b:^ :}
   d k KEY-NEW d a KEY-AT+ d b KEY-AT+ ;

\ The names map.
: MAP-OF ( ^doc -- ^n ) DOC-MAP @ ;

: MAP-NEW ( ^doc n -- )
   {: d:^ cap:n :}
   d DOC-NAMES @ cap >COUNT MAP:IN d DOC-MAP ! ;

: HELD? ( ^doc -- bool )
   {: d:^ :}
   d MAP-OF d KEY$ MAP:HAS? ;

\ The record KEY names in the map, null for none.
: MAP-RECORD ( ^n str -- ^t )
   MAP:GET MATCH option
      none OF 0 >RECORD ENDOF
      some OF >RECORD ENDOF
   ;MATCH ;

: FIND ( ^doc -- ^t ) dup MAP-OF swap KEY$ MAP-RECORD ;

\ Sets the key to V; a new key is copied into the names arena.
: REMEMBER ( ^doc n -- )
   {: d:^ v:n :}
   d HELD? if v d MAP-OF d KEY$ MAP:SET exit then
   d DOC-MAP d DOC-NAMES @ MAP:ROOM
   d DOC-NAMES @ d DOC-KEY-LEN @ ARENA:ALLOC {: copy:^ len:n :}
   d DOC-KEY @ copy len BYTE-COPY
   v d MAP-OF copy len >str MAP:SET ;

: REMEMBER-AT ( ^doc ^t -- ) NULL-PTR - REMEMBER ;

\ Names.
: ALIAS ( ^doc str -- str )
   {: d:^ name:str :}
   d KIND-ALIAS-NAME name NAME-KEY
   d FIND {: a:^alias :}
   a NULL-PTR = if name exit then
   a ALIAS-NAMESPACE @ ;

\ A qualified name, its qualifier an alias or a namespace, as a key of K.
: REFERENCE-KEY ( ^doc kind str -- )
   {: d:^ k:kind name:str :}
   name QUALIFIER {: q:str tail:str :}
   d q ALIAS {: ns:str :}
   d k ns tail QUALIFIED-KEY ;

: FIND-TYPE ( ^doc str -- ^entity-type )
   {: d:^ name:str :}
   d KIND-TYPE-NAME name REFERENCE-KEY d FIND ;

: FIND-ENUM ( ^doc str -- ^enum-type )
   {: d:^ name:str :}
   d KIND-ENUM-NAME name REFERENCE-KEY d FIND ;

: FIND-DEFINITION ( ^doc str -- ^type-definition )
   {: d:^ name:str :}
   d KIND-DEFINITION-NAME name REFERENCE-KEY d FIND ;

: FIND-ASSOCIATION ( ^doc str -- ^association )
   {: d:^ name:str :}
   d KIND-ASSOCIATION-NAME name REFERENCE-KEY d FIND ;

: FIND-END ( ^doc ^association str -- ^association-end )
   {: d:^ a:^ role:str :}
   d KIND-END-ROLE a role OWNED-KEY d FIND ;

: FIND-SET ( ^doc str -- ^entity-set )
   {: d:^ name:str :}
   d KIND-SET-NAME name NAME-KEY d FIND ;

: MEMBER? ( ^doc str -- bool )
   {: d:^ name:str :}
   d KIND-MEMBER-NAME name NAME-KEY d HELD? ;

: SINGLETON? ( ^doc str -- bool )
   {: d:^ name:str :}
   d KIND-SINGLETON-NAME name NAME-KEY d HELD? ;

\ The model's index of names.
: NAMES-OF ( ^metadata -- ^n ) METADATA-NAMES @ ;

\ The `named` of NAME, null for none.
: INDEXED ( ^metadata str -- ^named ) swap NAMES-OF swap MAP-RECORD ;

: HIDDEN? ( ^metadata str -- bool )
   INDEXED {: x:^named :}
   x NULL-PTR = if false exit then
   x NAMED-HIDDEN @ ;

\ The set NAME, null for none.
: INDEXED-SET ( ^metadata str -- ^entity-set )
   INDEXED {: x:^named :}
   x NULL-PTR = if NULL-PTR exit then
   x NAMED-SET @ ;

\ The last index below COUNT whose owner's rank, as Q answers it, is at most
\ RANK, -1 for none; the ranks rise with the index.
: AT-OR-BELOW ( n n [ n -- n ] -- n )
   {: rank:n count:n q :}
   -1 count begin 2dup swap 1+ > while
      {: lo:n hi:n :}
      lo hi + 2 / {: mid:n :}
      mid q execute rank <= if mid hi else lo mid then
   repeat drop ;

\ T's or a base type's member NAME, null for none.
: FIND-PROPERTY ( ^entity-type str -- ^property )
   {: t:^ name:str :}
   t ENTITY-TYPE-MODEL @ name INDEXED {: x:^named :}
   x NULL-PTR = if NULL-PTR exit then
   x NAMED-PROPERTIES @ {: ps:^ :}
   t ENTITY-TYPE-RANK @ x NAMED-PROPERTY-COUNT @
   [: cells ps + @ PROPERTY-OWNER @ ENTITY-TYPE-RANK @ ;]
   AT-OR-BELOW {: i:n :}
   i 0< if NULL-PTR exit then
   ps i cells + @ {: p:^ :}
   t ENTITY-TYPE-RANK @ p PROPERTY-OWNER @ ENTITY-TYPE-RANK-END @ > if
      NULL-PTR exit
   then
   p ;

: FIND-NAVIGATION ( ^entity-type str -- ^navigation )
   {: t:^ name:str :}
   t ENTITY-TYPE-MODEL @ name INDEXED {: x:^named :}
   x NULL-PTR = if NULL-PTR exit then
   x NAMED-NAVIGATIONS @ {: ns:^ :}
   t ENTITY-TYPE-RANK @ x NAMED-NAVIGATION-COUNT @
   [: cells ns + @ NAVIGATION-OWNER @ ENTITY-TYPE-RANK @ ;]
   AT-OR-BELOW {: i:n :}
   i 0< if NULL-PTR exit then
   ns i cells + @ {: n:^ :}
   t ENTITY-TYPE-RANK @ n NAVIGATION-OWNER @ ENTITY-TYPE-RANK-END @ > if
      NULL-PTR exit
   then
   n ;

\ Annotations: a V2 attribute outside the CSDL, XML and xmlns namespaces, or
\ a V4 Annotation whose value is one constant or path attribute.
: XMLNS? ( str -- bool )
   {: name:str :}
   name s" xmlns" STR:=
   name s" xmlns:" STR:STARTS-WITH? or
   name s" xml:" STR:STARTS-WITH? or ;

: PREFIXED? ( str -- bool )
   {: name:str :}
   name STR:LEN 0 ?do name STR:DATA i + c@ [char] : = if true unloop exit then loop
   false ;

: ADD-ANNOTATION ( ^doc ^^annotation str str str -- )
   {: d:^ list:^ ns:str nm:str tx:str :}
   d DOC-ARENA @ ANNOTATIONS-NEW {: a:^ :}
   ns a ANNOTATION-NAMESPACE !
   nm a ANNOTATION-NAME !
   tx a ANNOTATION-TEXT !
   d DOC-OFFSET @ a ANNOTATION-OFFSET !
   list @ a ANNOTATION-NEXT !
   a list ! ;

: FOREIGN? ( str -- bool )
   SPACE-OF {: sp:space :}
   sp SPACE-OTHER SPACE-EQ ;

\ The current attribute, when it is an annotation, onto LIST.
: ATTR-ANNOTATION ( XML:reader ^doc ^^annotation -- XML:reader )
   {: d:^ list:^ :}
   XML:ATTR-NAME$ {: name:str :}
   name PREFIXED? 0= name XMLNS? or if exit then
   d CLEARED XML:ATTR-URI
   d SCRATCH$ FOREIGN? 0= if exit then
   d d SCRATCH$ KEEP {: ns:str :}
   XML:ATTR-LOCAL$ {: local:str :}
   d local KEEP {: nm:str :}
   d ATTR-TEXT$ {: v:str :}
   d list ns nm d v KEEP ADD-ANNOTATION ;

: ATTR-ANNOTATIONS ( XML:reader ^doc ^^annotation -- XML:reader )
   {: d:^ list:^ :}
   d V2? 0= if exit then
   XML:ATTR-RESET
   begin XML:ATTR-NEXT while
      d list ATTR-ANNOTATION
   repeat ;

: VALUE-NAME? ( str -- bool )
   {: name:str :}
   name s" String" STR:= name s" Bool" STR:= or
   name s" Int" STR:= or name s" Decimal" STR:= or
   name s" Float" STR:= or name s" Date" STR:= or
   name s" DateTimeOffset" STR:= or name s" TimeOfDay" STR:= or
   name s" Duration" STR:= or name s" Guid" STR:= or
   name s" Binary" STR:= or name s" EnumMember" STR:= or
   name s" Path" STR:= or name s" PropertyPath" STR:= or
   name s" NavigationPropertyPath" STR:= or
   name s" AnnotationPath" STR:= or
   name s" ModelElementPath" STR:= or ;

: FIND-VALUE ( XML:reader -- XML:reader bool )
   XML:ATTR-RESET
   begin XML:ATTR-NEXT while
      XML:ATTR-NAME$ VALUE-NAME? if true exit then
   repeat
   false ;

\ The Annotation's Term, kept, as its qualifier and its name; rejected
\ unless qualified.
: TERM-OF ( XML:reader ^doc -- XML:reader str str )
   {: d:^ :}
   d s" Term" NEED {: term:str :}
   term QUALIFIER {: q:str tail:str :}
   tail STR:LEN 0= if
      d PLACE-HERE s" `Term` of `Annotation` to be a qualified name" REASON:APPEND
      term s" write the term's namespace or alias, a dot and its name"
      FOUND-QUOTED
   then
   q tail ;

\ A V4 Annotation onto LIST; one with a Qualifier or no value attribute is
\ not kept.
: ANNOTATION-START ( XML:reader ^doc ^^annotation -- XML:reader )
   {: d:^ list:^ :}
   s" Qualifier" FIND-ATTR if d SKIP-START exit then
   d TERM-OF {: q:str tail:str :}
   FIND-VALUE if
      d ATTR-TEXT$ {: v:str :}
      d list q tail d v KEEP ADD-ANNOTATION
   then
   d SKIP-START ;

\ The document's root fixes the version.
: VERSION-OK? ( ^doc str -- bool )
   {: d:^ s:str :}
   d V2? if s s" 1.0" STR:= exit then
   s s" 4.0" STR:= s s" 4.01" STR:= or ;

: EDMX-START ( XML:reader ^doc version -- XML:reader )
   {: d:^ ver:version :}
   ver d DOC-MODEL @ METADATA-VERSION !
   d s" Version" NEED$ {: v:str :}
   d v VERSION-OK? 0= if
      d PLACE-HERE
      d V2? if
         s" `Version` of `Edmx` to be 1.0 for OData V2" REASON:APPEND
         v s" set `Version` to 1.0" FOUND-QUOTED
      then
      s" `Version` of `Edmx` to be 4.0 or 4.01 for OData V4" REASON:APPEND
      v s" set `Version` to 4.0 or 4.01" FOUND-QUOTED
   then
   d PHASE-EDMX ENTER ;

\ A namespace URI in backticks, whole and made visible, as CUT would make
\ V2's and V3's the same; or no namespace.
: NAMESPACE+ ( str -- )
   {: uri:str :}
   uri STR:LEN 0= if s" no namespace" REASON:APPEND exit then
   uri false REASON:QUOTE ;

\ The element's namespace as found.
: NAMESPACE-FOUND ( str str -- )
   {: uri:str fix:str :}
   REASON:FOUND uri NAMESPACE+ fix FIX ;

: IN-DOCUMENT ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d SPACE-EDMX2 s" Edmx" IS? if d VERSION-V2 EDMX-START exit then
   d SPACE-EDMX4 s" Edmx" IS? if d VERSION-V4 EDMX-START exit then
   d PLACE-HERE
   s" `edmx:Edmx` in the EDMX namespace of OData V2 or V4" REASON:APPEND
   XML:LOCAL$ {: el:str :}
   REASON:FOUND el QUOTE s"  in " REASON:APPEND d SCRATCH$ NAMESPACE+
   s" make the root element `edmx:Edmx` in that namespace" FIX ;

\ The element's Alias, when it has one, for the namespace NS.
: ADD-ALIAS ( XML:reader ^doc str -- XML:reader )
   {: d:^ ns:str :}
   d s" Alias" ATTR$ 0= if drop exit then
   {: nm:str :}
   d DOC-NAMES @ ALIASES-NEW {: a:^ :}
   ns a ALIAS-NAMESPACE !
   d KIND-ALIAS-NAME nm NAME-KEY d a REMEMBER-AT ;

: INCLUDE-START ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d s" Namespace" NEED {: ns:str :}
   d ns ADD-ALIAS
   d SKIP-START ;

: IN-EDMX ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d d EDMX s" DataServices" IS? if d PHASE-DATA-SERVICES ENTER exit then
   d SPACE-EDMX4 s" Reference" IS? if d PHASE-REFERENCE ENTER exit then
   d SKIP-START ;

: IN-REFERENCE ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d SPACE-EDMX4 s" Include" IS? if d INCLUDE-START exit then
   d SKIP-START ;

: SCHEMA-START ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d s" Namespace" NEED {: ns:str :}
   ns d DOC-NAMESPACE !
   d ns ADD-ALIAS
   d PHASE-SCHEMA ENTER ;

: IN-DATA-SERVICES ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   XML:LOCAL$ s" Schema" STR:= 0= if d SKIP-START exit then
   d d CSDL s" Schema" IS? if d SCHEMA-START exit then
   d PLACE-HERE
   d V2? if
      s" `Schema` in a CSDL namespace of OData V2 (1.0, 1.1 or 2.0)"
   else
      s" `Schema` in the CSDL namespace of OData V4"
   then
   REASON:APPEND
   d SCRATCH$ s" put the `Schema` in that namespace" NAMESPACE-FOUND ;

\ Duplicate names are rejected where the second is read.
: NOT-NEW ( str str -- )
   {: el:str name:str :}
   s" `" REASON:APPEND el REASON:APPEND
   s" ` to have a name not declared before" REASON:APPEND
   name s" rename or remove it" FOUND-QUOTED ;

: REPEATED ( XML:reader ^doc str -- XML:reader )
   {: d:^ name:str :}
   d PLACE-HERE XML:LOCAL$ {: el:str :} el name NOT-NEW ;

: NAME-BYTE? ( u8 -- bool )
   dup 127 > if drop true exit then
   dup [char] _ = if drop true exit then
   STR:ALNUM? ;

\ The index after the name bytes from I on in TEXT.
: RUN-END ( str n -- n )
   {: text:str i:n :}
   i begin
      dup text STR:LEN < if dup text STR:DATA + c@ NAME-BYTE? else false then
   while 1+ repeat ;

\ A name a query can spell is one run of name bytes.
: IDENTIFIER ( XML:reader ^doc str -- XML:reader )
   {: d:^ text:str :}
   text STR:LEN 0 > text 0 RUN-END text STR:LEN = and if exit then
   d PLACE-HERE XML:LOCAL$ {: el:str :}
   s" `" REASON:APPEND el REASON:APPEND
   s" ` to have a name of letters, digits, `_` or bytes above 127"
   REASON:APPEND
   text s" rename it to an OData identifier" FOUND-QUOTED ;

: DECLARED-KEY ( ^doc kind str -- )
   {: d:^ k:kind name:str :}
   d k d DOC-NAMESPACE @ name QUALIFIED-KEY ;

\ A schema's entity, complex, enumeration and defined types share one name
\ space.
: TYPE-TAKEN? ( ^doc str -- bool )
   {: d:^ name:str :}
   d KIND-TYPE-NAME name DECLARED-KEY d HELD?
   d KIND-ENUM-NAME name DECLARED-KEY d HELD? or
   d KIND-DEFINITION-NAME name DECLARED-KEY d HELD? or ;

: TYPE-START ( XML:reader ^doc bool -- XML:reader )
   {: d:^ complex:bool :}
   d s" Name" NEED {: name:str :}
   d name TYPE-TAKEN? if d name REPEATED then
   d DOC-ARENA @ TYPES-NEW {: t:^ :}
   name t ENTITY-TYPE-NAME !
   d DOC-NAMESPACE @ t ENTITY-TYPE-NAMESPACE !
   complex t ENTITY-TYPE-COMPLEX !
   d DOC-MODEL @ METADATA-VERSION @ t ENTITY-TYPE-VERSION !
   d DOC-OFFSET @ t ENTITY-TYPE-OFFSET !
   d s" BaseType" OPTIONAL t ENTITY-TYPE-BASE-NAME !
   d t ENTITY-TYPE-ANNOTATIONS ATTR-ANNOTATIONS
   d DOC-TYPES @ t ENTITY-TYPE-NEXT ! t d DOC-TYPES !
   d KIND-TYPE-NAME name DECLARED-KEY d t REMEMBER-AT
   d DOC-MODEL @ t ENTITY-TYPE-MODEL !
   1 d DOC-TYPE-COUNT +!
   t d DOC-TYPE !
   NULL-PTR d DOC-PROPERTY-LAST !
   NULL-PTR d DOC-NAVIGATION-LAST !
   NULL-PTR d DOC-KEY-REF-LAST !
   d PHASE-TYPE-BODY ENTER ;

\ T's own members only: a base type's are checked when the walk holds them.
: MEMBER-TAKEN? ( ^doc ^entity-type str -- bool )
   {: d:^ t:^ name:str :}
   d KIND-PROPERTY-NAME t name OWNED-KEY d HELD?
   d KIND-NAVIGATION-NAME t name OWNED-KEY d HELD? or ;

\ V, the value of NAME on the element EL at AT, as a boolean; rejected unless
\ true or false.
: BOOL-OF ( ^doc n str str str -- bool )
   {: d:^ at:n name:str el:str s:str :}
   s s" true" STR:= if true exit then
   s s" false" STR:= if false exit then
   d at PLACE
   s" `" REASON:APPEND name REASON:APPEND
   s" ` of `" REASON:APPEND el REASON:APPEND
   s" ` to be true or false" REASON:APPEND
   REASON:FOUND s QUOTE
   REASON:FIX s" set `" REASON:APPEND name REASON:APPEND
   s" ` to true or false" REASON:APPEND E-METADATA REASON:STOP ;

\ The boolean attribute NAME of the current element, DEFAULT when absent.
: BOOL-ATTR ( XML:reader ^doc str bool -- XML:reader bool )
   {: d:^ name:str default:bool :}
   d name ATTR$ 0= if drop default exit then
   {: v:str :}
   XML:LOCAL$ {: el:str :}
   d d DOC-OFFSET @ name el v BOOL-OF ;

: NULLABLE ( XML:reader ^doc -- XML:reader bool )
   s" Nullable" true BOOL-ATTR ;

: ADD-PROPERTY ( ^doc ^property -- )
   {: d:^ p:^ :}
   d DOC-PROPERTY-LAST @ dup NULL-PTR = if
      drop p d DOC-TYPE @ ENTITY-TYPE-PROPERTIES !
   else PROPERTY-NEXT p swap ! then
   p d DOC-PROPERTY-LAST !
   d KIND-PROPERTY-NAME d DOC-TYPE @ p PROPERTY-NAME @ OWNED-KEY
   d p REMEMBER-AT ;

: PROPERTY-START ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d s" Name" NEED {: name:str :}
   d name IDENTIFIER
   d d DOC-TYPE @ name MEMBER-TAKEN? if d name REPEATED then
   d DOC-ARENA @ PROPERTIES-NEW {: p:^ :}
   name p PROPERTY-NAME !
   d DOC-TYPE @ p PROPERTY-OWNER !
   d DOC-OFFSET @ p PROPERTY-OFFSET !
   d s" Type" NEED p PROPERTY-TYPE !
   d NULLABLE p PROPERTY-NULLABLE !
   d p PROPERTY-ANNOTATIONS ATTR-ANNOTATIONS
   d p ADD-PROPERTY
   p d DOC-PROPERTY !
   d PHASE-PROPERTY-BODY ENTER ;

: ADD-NAVIGATION ( ^doc ^navigation -- )
   {: d:^ n:^ :}
   d DOC-NAVIGATION-LAST @ dup NULL-PTR = if
      drop n d DOC-TYPE @ ENTITY-TYPE-NAVIGATIONS !
   else NAVIGATION-NEXT n swap ! then
   n d DOC-NAVIGATION-LAST !
   d KIND-NAVIGATION-NAME d DOC-TYPE @ n NAVIGATION-NAME @ OWNED-KEY
   d n REMEMBER-AT ;

: NAVIGATION-START ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d s" Name" NEED {: name:str :}
   d name IDENTIFIER
   d d DOC-TYPE @ name MEMBER-TAKEN? if d name REPEATED then
   d DOC-ARENA @ NAVIGATIONS-NEW {: n:^ :}
   name n NAVIGATION-NAME !
   d DOC-TYPE @ n NAVIGATION-OWNER !
   d DOC-OFFSET @ n NAVIGATION-OFFSET !
   d V2? if
      d s" Relationship" NEED n NAVIGATION-RELATIONSHIP !
      d s" FromRole" NEED n NAVIGATION-FROM-ROLE !
      d s" ToRole" NEED n NAVIGATION-TO-ROLE !
   else
      d s" Type" NEED n NAVIGATION-TYPE !
   then
   d n ADD-NAVIGATION
   d SKIP-START ;

: KEY-REF-START ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d DOC-ARENA @ KEY-REFS-NEW {: k:^ :}
   d s" Name" NEED k KEY-REF-NAME !
   d DOC-OFFSET @ k KEY-REF-OFFSET !
   d DOC-KEY-REF-LAST @ dup NULL-PTR = if
      drop k d DOC-TYPE @ ENTITY-TYPE-KEY !
   else KEY-REF-NEXT k swap ! then
   k d DOC-KEY-REF-LAST !
   d SKIP-START ;

: IN-TYPE ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d d CSDL s" Key" IS? if d PHASE-KEY ENTER exit then
   d d CSDL s" Property" IS? if d PROPERTY-START exit then
   d d CSDL s" NavigationProperty" IS? if d NAVIGATION-START exit then
   d SPACE-EDM4 s" Annotation" IS? if
      d d DOC-TYPE @ ENTITY-TYPE-ANNOTATIONS ANNOTATION-START exit
   then
   d SKIP-START ;

: IN-KEY ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d d CSDL s" PropertyRef" IS? if d KEY-REF-START exit then
   d SKIP-START ;

: IN-PROPERTY ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d SPACE-EDM4 s" Annotation" IS? if
      d d DOC-PROPERTY @ PROPERTY-ANNOTATIONS ANNOTATION-START exit
   then
   d SKIP-START ;

: ASSOCIATION-TAKEN? ( ^doc str -- bool )
   {: d:^ name:str :}
   d KIND-ASSOCIATION-NAME name DECLARED-KEY d HELD? ;

: ASSOCIATION-START ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d s" Name" NEED {: name:str :}
   d name ASSOCIATION-TAKEN? if d name REPEATED then
   d DOC-ARENA @ ASSOCIATIONS-NEW {: a:^ :}
   name a ASSOCIATION-NAME !
   d DOC-NAMESPACE @ a ASSOCIATION-NAMESPACE !
   d DOC-OFFSET @ a ASSOCIATION-OFFSET !
   d DOC-ASSOCIATIONS @ a ASSOCIATION-NEXT ! a d DOC-ASSOCIATIONS !
   d KIND-ASSOCIATION-NAME name DECLARED-KEY d a REMEMBER-AT
   a d DOC-ASSOCIATION !
   d PHASE-ASSOCIATION-BODY ENTER ;

: MULTIPLICITY ( XML:reader ^doc -- XML:reader bool )
   {: d:^ :}
   d s" Multiplicity" NEED$ {: v:str :}
   v s" *" STR:= if true exit then
   v s" 1" STR:= v s" 0..1" STR:= or if false exit then
   d PLACE-HERE s" `Multiplicity` of `End` to be 1, 0..1 or *" REASON:APPEND
   v s" set `Multiplicity` to 1, 0..1 or *" FOUND-QUOTED ;

\ The two `End` elements of EL NAME share ROLE.
: ROLE-TWICE ( str str str -- )
   {: el:str name:str role:str :}
   s" the two `End` elements of `" REASON:APPEND el REASON:APPEND s" ` " REASON:APPEND
   name QUOTE s"  to have different roles" REASON:APPEND
   role s" give one `End` the other role" FOUND-QUOTED ;

: END-START ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d s" Role" NEED {: role:str :}
   d DOC-ASSOCIATION @ {: a:^ :}
   d a role FIND-END NULL-PTR <> if
      d PLACE-HERE
      s" Association" a ASSOCIATION-NAME @ role ROLE-TWICE
   then
   d DOC-ARENA @ ENDS-NEW {: e:^ :}
   role e ASSOCIATION-END-ROLE !
   d s" Type" NEED e ASSOCIATION-END-TYPE !
   d MULTIPLICITY e ASSOCIATION-END-MANY !
   d DOC-OFFSET @ e ASSOCIATION-END-OFFSET !
   a ASSOCIATION-ENDS @ e ASSOCIATION-END-NEXT ! e a ASSOCIATION-ENDS !
   d KIND-END-ROLE a role OWNED-KEY d e REMEMBER-AT
   d SKIP-START ;

: IN-ASSOCIATION ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d d CSDL s" End" IS? if d END-START exit then
   d SKIP-START ;

: CONTAINER-START ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d s" Name" NEED {: name:str :}
   d DOC-CONTAINERS @ 0<> if
      d PLACE-HERE s" no `EntityContainer` after the first" REASON:APPEND
      name s" move its contents into the first and remove it" FOUND-QUOTED
   then
   1 d DOC-CONTAINERS !
   name d DOC-CONTAINER !
   d DOC-NAMESPACE @ d DOC-CONTAINER-NAMESPACE !
   d PHASE-CONTAINER ENTER ;

: NAME-TAKEN? ( ^doc str -- bool )
   {: d:^ name:str :}
   d name FIND-SET NULL-PTR <>
   d name MEMBER? or ;

: ADD-MEMBER ( ^doc str -- )
   {: d:^ name:str :}
   d KIND-MEMBER-NAME name NAME-KEY d 0 REMEMBER ;

: ADD-SINGLETON ( XML:reader ^doc str -- XML:reader )
   {: d:^ name:str :}
   d KIND-SINGLETON-NAME name NAME-KEY d 0 REMEMBER
   d DOC-ARENA @ SINGLETONS-NEW {: s:^ :}
   name s SINGLETON-NAME !
   d s" Type" NEED s SINGLETON-TYPE-NAME !
   d DOC-OFFSET @ s SINGLETON-OFFSET !
   d DOC-SINGLETONS @ s SINGLETON-NEXT !
   s d DOC-SINGLETONS ! ;

: MEMBER-START ( XML:reader ^doc bool -- XML:reader )
   {: d:^ singleton:bool :}
   d s" Name" NEED {: name:str :}
   singleton if d name IDENTIFIER then
   d name NAME-TAKEN? if d name REPEATED then
   d name ADD-MEMBER
   singleton if d name ADD-SINGLETON then
   d SKIP-START ;

: ADD-SET ( ^doc ^entity-set -- )
   {: d:^ s:^ :}
   d DOC-SET-LAST @ dup NULL-PTR = if
      drop s d DOC-MODEL @ METADATA-SETS !
   else ENTITY-SET-NEXT s swap ! then
   s d DOC-SET-LAST !
   d KIND-SET-NAME s ENTITY-SET-NAME @ NAME-KEY d s REMEMBER-AT ;

: SET-START ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d s" Name" NEED {: name:str :}
   d name IDENTIFIER
   d name NAME-TAKEN? if d name REPEATED then
   d DOC-ARENA @ SETS-NEW {: s:^ :}
   name s ENTITY-SET-NAME !
   d s" EntityType" NEED s ENTITY-SET-TYPE-NAME !
   d DOC-OFFSET @ s ENTITY-SET-OFFSET !
   d s ADD-SET
   s d DOC-SET !
   d PHASE-SET-BODY ENTER ;

: ASSOCIATION-SET-START ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d s" Name" NEED {: name:str :}
   d name NAME-TAKEN? if d name REPEATED then
   d name ADD-MEMBER
   d DOC-ARENA @ ASSOCIATION-SETS-NEW {: a:^ :}
   name a ASSOCIATION-SET-NAME !
   d s" Association" NEED a ASSOCIATION-SET-ASSOCIATION-NAME !
   d DOC-OFFSET @ a ASSOCIATION-SET-OFFSET !
   d DOC-ASSOCIATION-SETS @ a ASSOCIATION-SET-NEXT !
   a d DOC-ASSOCIATION-SETS !
   a d DOC-ASSOCIATION-SET !
   d PHASE-SET-PAIRS ENTER ;

: IN-CONTAINER ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d d CSDL s" EntitySet" IS? if d SET-START exit then
   d d CSDL s" AssociationSet" IS? if d ASSOCIATION-SET-START exit then
   d d CSDL s" FunctionImport" IS? if d false MEMBER-START exit then
   d d CSDL s" ActionImport" IS? if d false MEMBER-START exit then
   d d CSDL s" Singleton" IS? if d true MEMBER-START exit then
   d SKIP-START ;

\ The set's binding of a path under `binding-path` is the newest, under
\ `binding-second` the one before it: the first two in DOC-BINDINGS.
: ADD-BINDING ( ^doc ^binding -- )
   {: d:^ b:^ :}
   b BINDING-SET @ b BINDING-PATH @ {: s:^ p:str :}
   d KIND-BINDING-PATH s p OWNED-KEY d FIND {: old:^binding :}
   old NULL-PTR <> if
      d KIND-BINDING-SECOND s p OWNED-KEY d old REMEMBER-AT
   then
   d KIND-BINDING-PATH s p OWNED-KEY d b REMEMBER-AT ;

: BINDING-START ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d DOC-ARENA @ BINDINGS-NEW {: b:^ :}
   d DOC-SET @ b BINDING-SET !
   d s" Path" NEED b BINDING-PATH !
   d s" Target" NEED b BINDING-TARGET-NAME !
   d DOC-OFFSET @ b BINDING-OFFSET !
   d DOC-BINDINGS @ b BINDING-NEXT ! b d DOC-BINDINGS !
   d b ADD-BINDING
   d SKIP-START ;

: FILTER-NAME? ( str -- bool ) s" FilterRestrictions" STR:= ;

: FILTER-TERM? ( XML:reader ^doc -- XML:reader bool )
   {: d:^ :}
   d s" Term" ATTR$ 0= if drop false exit then
   QUALIFIER {: q:str tail:str :} tail FILTER-NAME? ;

\ A FilterRestrictions with no Qualifier onto LIST, read in filter-body; any
\ other Annotation is read past.
: FILTER-START ( XML:reader ^doc ^^filter -- XML:reader )
   {: d:^ list:^ :}
   s" Qualifier" FIND-ATTR if d SKIP-START exit then
   d TERM-OF {: q:str tail:str :}
   tail FILTER-NAME? 0= if d SKIP-START exit then
   d DOC-ARENA @ FILTERS-NEW {: f:^ :}
   q f FILTER-NAMESPACE !
   true f FILTER-FILTERABLE !
   d DOC-OFFSET @ f FILTER-OFFSET !
   list @ f FILTER-NEXT ! f list !
   f d DOC-FILTER !
   d DOC-PHASE @ d DOC-HOME !
   d PHASE-FILTER-BODY ENTER ;

: IN-SET ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d SPACE-EDM4 s" NavigationPropertyBinding" IS? if d BINDING-START exit then
   d SPACE-EDM4 s" Annotation" IS? if
      d d DOC-SET @ ENTITY-SET-FILTERS FILTER-START exit
   then
   d SKIP-START ;

: SET-END-START ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d DOC-ARENA @ SET-ENDS-NEW {: e:^ :}
   d s" Role" NEED e SET-END-ROLE !
   d s" EntitySet" NEED e SET-END-SET-NAME !
   d DOC-OFFSET @ e SET-END-OFFSET !
   d DOC-ASSOCIATION-SET @ {: a:^ :}
   a ASSOCIATION-SET-ENDS @ e SET-END-NEXT ! e a ASSOCIATION-SET-ENDS !
   d SKIP-START ;

: IN-SET-PAIRS ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d d CSDL s" End" IS? if d SET-END-START exit then
   d SKIP-START ;

: BLOCK-START ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   s" Qualifier" FIND-ATTR if d SKIP-START exit then
   d DOC-ARENA @ BLOCKS-NEW {: b:^ :}
   d s" Target" NEED b BLOCK-TARGET !
   d DOC-BLOCKS @ b BLOCK-NEXT ! b d DOC-BLOCKS !
   b d DOC-BLOCK !
   d PHASE-ANNOTATIONS ENTER ;

: IN-ANNOTATIONS ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d SPACE-EDM4 s" Annotation" IS? if
      d FILTER-TERM? if d d DOC-BLOCK @ BLOCK-FILTERS FILTER-START exit then
      d d DOC-BLOCK @ BLOCK-ANNOTATIONS ANNOTATION-START exit
   then
   d SKIP-START ;

: ENUM-START ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d s" Name" NEED {: name:str :}
   d name TYPE-TAKEN? if d name REPEATED then
   d DOC-ARENA @ ENUM-TYPES-NEW {: e:^ :}
   name e ENUM-TYPE-NAME !
   d DOC-NAMESPACE @ e ENUM-TYPE-NAMESPACE !
   d s" IsFlags" false BOOL-ATTR e ENUM-TYPE-FLAGS !
   d KIND-ENUM-NAME name DECLARED-KEY d e REMEMBER-AT
   e d DOC-ENUM-TYPE !
   d PHASE-ENUM-BODY ENTER ;

: ENUM-MEMBER-START ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d DOC-ARENA @ ENUM-MEMBERS-NEW {: m:^ :}
   d s" Name" NEED m ENUM-MEMBER-NAME !
   d DOC-ENUM-TYPE @ {: e:^ :}
   e ENUM-TYPE-MEMBERS @ m ENUM-MEMBER-NEXT ! m e ENUM-TYPE-MEMBERS !
   d SKIP-START ;

: IN-ENUM ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d SPACE-EDM4 s" Member" IS? if d ENUM-MEMBER-START exit then
   d SKIP-START ;

: DEFINITION-START ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d s" Name" NEED {: name:str :}
   d name TYPE-TAKEN? if d name REPEATED then
   d DOC-NAMES @ DEFINITIONS-NEW {: t:^ :}
   d s" UnderlyingType" NEED t TYPE-DEFINITION-UNDERLYING !
   d KIND-DEFINITION-NAME name DECLARED-KEY d t REMEMBER-AT
   d SKIP-START ;

: IN-SCHEMA ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d d CSDL s" EntityType" IS? if d false TYPE-START exit then
   d d CSDL s" ComplexType" IS? if d true TYPE-START exit then
   d d CSDL s" Association" IS? if d ASSOCIATION-START exit then
   d d CSDL s" EntityContainer" IS? if d CONTAINER-START exit then
   d SPACE-EDM4 s" Annotations" IS? if d BLOCK-START exit then
   d SPACE-EDM4 s" EnumType" IS? if d ENUM-START exit then
   d SPACE-EDM4 s" TypeDefinition" IS? if d DEFINITION-START exit then
   d SKIP-START ;

: IN-FILTER ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d SPACE-EDM4 s" Record" IS? if d PHASE-RECORD ENTER exit then
   d SKIP-START ;

\ Filterable as a Bool attribute is read here, as a Bool element in bool-value.
: FILTERABLE-START ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   s" Bool" FIND-ATTR 0= if d PHASE-BOOL-VALUE ENTER exit then
   d s" Bool" true BOOL-ATTR d DOC-FILTER @ FILTER-FILTERABLE !
   d SKIP-START ;

: PROPERTY-VALUE-START ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d s" Property" ATTR$ drop {: name:str :}
   name s" NonFilterableProperties" STR:= if
      d PHASE-PATHS-VALUE ENTER exit
   then
   name s" Filterable" STR:= if d FILTERABLE-START exit then
   d SKIP-START ;

: IN-FILTER-RECORD ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d SPACE-EDM4 s" PropertyValue" IS? if d PROPERTY-VALUE-START exit then
   d SKIP-START ;

: IN-PATHS-VALUE ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d SPACE-EDM4 s" Collection" IS? if d PHASE-PATHS ENTER exit then
   d SKIP-START ;

: TEXT-START ( XML:reader ^doc phase -- XML:reader )
   {: d:^ p:phase :}
   d CLEARED drop
   d p ENTER ;

: IN-PATHS ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d SPACE-EDM4 s" PropertyPath" IS? if d PHASE-PATH-TEXT TEXT-START exit then
   d SKIP-START ;

: IN-BOOL-VALUE ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d SPACE-EDM4 s" Bool" IS? if d PHASE-BOOL-TEXT TEXT-START exit then
   d SKIP-START ;

\ Rejects an element inside EL, whose value is text.
: NOT-TEXT ( XML:reader ^doc str -- XML:reader )
   {: d:^ el:str :}
   d PLACE-HERE s" text in `" REASON:APPEND el REASON:APPEND s" `" REASON:APPEND
   s" an element" s" write the value as text" FOUND ;

\ A start tag: its namespace is classified, then the open phase takes it;
\ scratch holds the URI until the tag's first attribute is decoded.
: CLASSIFY ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d CLEARED XML:URI
   d SCRATCH$ SPACE-OF d DOC-SPACE ! ;

: ON-START ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d CLASSIFY
   d DOC-PHASE @ MATCH phase
      document OF d IN-DOCUMENT ENDOF
      edmx OF d IN-EDMX ENDOF
      reference OF d IN-REFERENCE ENDOF
      data-services OF d IN-DATA-SERVICES ENDOF
      schema OF d IN-SCHEMA ENDOF
      type-body OF d IN-TYPE ENDOF
      key OF d IN-KEY ENDOF
      property-body OF d IN-PROPERTY ENDOF
      container OF d IN-CONTAINER ENDOF
      set-body OF d IN-SET ENDOF
      association-body OF d IN-ASSOCIATION ENDOF
      set-pairs OF d IN-SET-PAIRS ENDOF
      annotations OF d IN-ANNOTATIONS ENDOF
      enum-body OF d IN-ENUM ENDOF
      filter-body OF d IN-FILTER ENDOF
      record OF d IN-FILTER-RECORD ENDOF
      paths-value OF d IN-PATHS-VALUE ENDOF
      paths OF d IN-PATHS ENDOF
      path-text OF d s" PropertyPath" NOT-TEXT ENDOF
      bool-value OF d IN-BOOL-VALUE ENDOF
      bool-text OF d s" Bool" NOT-TEXT ENDOF
      skip OF d DEEPER ENDOF
   ;MATCH ;

\ A PropertyPath's or Bool's text, which may come in several text and CDATA
\ events, is collected in the scratch; other text is read past.
: ON-TEXT ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d DOC-PHASE @ {: p:phase :}
   p PHASE-PATH-TEXT PHASE-EQ p PHASE-BOOL-TEXT PHASE-EQ or 0= if exit then
   d DOC-SCRATCH @ XML:TEXT ;

: PATH-END ( ^doc -- )
   {: d:^ :}
   d DOC-ARENA @ PATHS-NEW {: p:^ :}
   d d SCRATCH$ KEEP p PATH-TEXT !
   d DOC-FILTER @ {: f:^ :}
   f FILTER-PATHS @ p PATH-NEXT ! p f FILTER-PATHS ! ;

: BOOL-END ( ^doc -- )
   {: d:^ :}
   d d DOC-OFFSET @ s" Bool" s" PropertyValue" d SCRATCH$ BOOL-OF
   d DOC-FILTER @ FILTER-FILTERABLE ! ;

: ON-END ( ^doc -- )
   {: d:^ :}
   d DOC-PHASE @ {: p:phase :}
   p PHASE-SKIP PHASE-EQ if d SHALLOWER exit then
   p PHASE-PATH-TEXT PHASE-EQ if d PATH-END then
   p PHASE-BOOL-TEXT PHASE-EQ if d BOOL-END then
   d d p PARENT ENTER ;

\ One event; false at the end of the document.
: STEP ( XML:reader ^doc -- XML:reader bool )
   {: d:^ :}
   XML:NEXT {: k:XML:kind :}
   XML:RAW drop OFF>N d DOC-OFFSET !
   k MATCH XML:kind
      start OF d ON-START true ENDOF
      end OF d ON-END true ENDOF
      text OF d ON-TEXT true ENDOF
      comment OF true ENDOF
      pi OF true ENDOF
      cdata OF d ON-TEXT true ENDOF
      eof OF false ENDOF
   ;MATCH ;

: EVENTS ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   begin d STEP while repeat ;

\ RESOLVE: every name the document gives resolves to its record, or the
\ document is rejected at the element that holds the name.
: TYPE-ELEMENT$ ( ^entity-type -- str )
   ENTITY-TYPE-COMPLEX @ if s" ComplexType" exit then s" EntityType" ;

\ Opens the text at the type T with its `BaseType`.
: BASE-PLACE ( ^doc ^entity-type -- )
   {: d:^ t:^ :}
   d t ENTITY-TYPE-OFFSET @ PLACE
   s" `BaseType` of `" REASON:APPEND t TYPE-ELEMENT$ REASON:APPEND s" ` " REASON:APPEND
   t ENTITY-TYPE-NAME @ QUOTE ;

: BASE ( ^doc ^entity-type -- )
   {: d:^ t:^ :}
   t ENTITY-TYPE-BASE-NAME @ {: bn:str :}
   bn STR:LEN 0= if exit then
   d bn FIND-TYPE dup NULL-PTR = if
      drop d t BASE-PLACE
      s"  to name a type the document declares" REASON:APPEND
      bn s" declare that type, or name one the document declares" FOUND-QUOTED
   then
   t ENTITY-TYPE-BASE ! ;

: REDECLARED ( ^doc n str str -- )
   {: d:^ at:n name:str el:str :}
   d at PLACE el name NOT-NEW ;

\ The open type holding NAME, null for none.
: HOLDER ( ^doc str -- ^entity-type )
   {: d:^ name:str :}
   d KIND-VISIBLE name NAME-KEY d FIND {: h:^entity-type :}
   h NULL-PTR = if h exit then
   h ENTITY-TYPE-RANK-END @ 0= if h exit then
   NULL-PTR ;

: HOLD ( ^doc ^entity-type str n str -- )
   {: d:^ t:^ name:str at:n el:str :}
   d name HOLDER NULL-PTR <> if d at name el REDECLARED then
   d KIND-VISIBLE name NAME-KEY d t REMEMBER-AT ;

: HOLD-MEMBERS ( ^doc ^entity-type -- )
   {: d:^ t:^ :}
   t ENTITY-TYPE-PROPERTIES @ begin dup NULL-PTR <> while
      {: p:^ :}
      d t p PROPERTY-NAME @ p PROPERTY-OFFSET @ s" Property" HOLD
      p PROPERTY-NEXT @
   repeat drop
   t ENTITY-TYPE-NAVIGATIONS @ begin dup NULL-PTR <> while
      {: n:^ :}
      d t n NAVIGATION-NAME @ n NAVIGATION-OFFSET @ s" NavigationProperty" HOLD
      n NAVIGATION-NEXT @
   repeat drop ;

\ Each takes an Edm type's name without `Edm.` and answers its kind,
\ none for a name it does not list.
: SHARED-KIND ( str -- option<key-kind> )
   {: s:str :}
   s s" String" STR:= if KEY-KIND-STRING OPTION:SOME exit then
   s s" Boolean" STR:= if KEY-KIND-BOOLEAN OPTION:SOME exit then
   s s" Guid" STR:= if KEY-KIND-GUID OPTION:SOME exit then
   s s" Binary" STR:= if KEY-KIND-BINARY OPTION:SOME exit then
   s s" Byte" STR:= if KEY-KIND-BYTE OPTION:SOME exit then
   s s" SByte" STR:= if KEY-KIND-SBYTE OPTION:SOME exit then
   s s" Int16" STR:= if KEY-KIND-INT16 OPTION:SOME exit then
   s s" Int32" STR:= if KEY-KIND-INT32 OPTION:SOME exit then
   s s" Int64" STR:= if KEY-KIND-INT64 OPTION:SOME exit then
   s s" Decimal" STR:= if KEY-KIND-DECIMAL OPTION:SOME exit then
   s s" Double" STR:= if KEY-KIND-DOUBLE OPTION:SOME exit then
   s s" Single" STR:= if KEY-KIND-SINGLE OPTION:SOME exit then
   s s" DateTimeOffset" STR:= if
      KEY-KIND-DATE-TIME-OFFSET OPTION:SOME exit
   then
   OPTION:NONE ;

: V2-KIND ( str -- option<key-kind> )
   {: s:str :}
   s s" DateTime" STR:= if KEY-KIND-DATE-TIME OPTION:SOME exit then
   s s" Time" STR:= if KEY-KIND-TIME OPTION:SOME exit then
   s SHARED-KIND ;

: V4-KIND ( str -- option<key-kind> )
   {: s:str :}
   s s" Date" STR:= if KEY-KIND-DATE OPTION:SOME exit then
   s s" TimeOfDay" STR:= if KEY-KIND-TIME-OF-DAY OPTION:SOME exit then
   s s" Duration" STR:= if KEY-KIND-DURATION OPTION:SOME exit then
   s SHARED-KIND ;

: PRIMITIVE? ( str -- bool )
   {: s:str :}
   s STR:LEN 4 < if false exit then
   s STR:DATA 4 >str s" Edm." STR:= ;

\ The kind of key the type TYPE is in VERSION's key predicate, none for a
\ type that version writes in no key.
: TYPE-KIND ( str version -- option<key-kind> )
   {: s:str v:version :}
   s PRIMITIVE? 0= if OPTION:NONE exit then
   s STR:DATA 4 + s STR:LEN 4 - >str v V2-VERSION? if V2-KIND else V4-KIND then ;

: UNKEYED ( ^doc ^key-ref ^property -- )
   {: d:^ k:^ p:^ :}
   d V2? if s" 2" else s" 4" then {: ver:str :}
   d k KEY-REF-OFFSET @ PLACE
   s" `Type` of key property " REASON:APPEND k KEY-REF-NAME @ QUOTE
   s"  to be a type OData V" REASON:APPEND ver REASON:APPEND
   s"  writes in a key" REASON:APPEND
   REASON:FOUND p PROPERTY-TYPE @ QUOTE
   REASON:FIX s" give the key property a type OData V" REASON:APPEND
   ver REASON:APPEND
   s"  writes in a key, or take it out of the `Key`" REASON:APPEND
   E-METADATA REASON:STOP ;

\ Sets K's kind from the Edm type TYPE, which P's type is or is defined
\ over, rejecting a type the document's version writes in no key.
: EDM-KIND ( ^doc ^key-ref ^property str -- )
   {: d:^ k:^ p:^ s:str :}
   s d DOC-MODEL @ METADATA-VERSION @ TYPE-KIND
   MATCH option
      none OF d k p UNKEYED ENDOF
      some OF k KEY-REF-KIND ! ENDOF
   ;MATCH ;

\ Whether TYPE is one namespace-qualified name, which another document can
\ declare; `Collection(...)` is not.
: QUALIFIED? ( str -- bool )
   {: s:str :}
   s s" Collection(" STR:STARTS-WITH? if false exit then
   s QUALIFIER {: q:str n:str :} n STR:LEN 0 > ;

: ENUM-KIND ( ^doc ^key-ref ^property -- )
   {: d:^ k:^ p:^ :}
   p PROPERTY-TYPE @ {: t:str :}
   d t FIND-ENUM {: e:^enum-type :}
   e NULL-PTR = if
      t QUALIFIED? 0= d t FIND-TYPE NULL-PTR <> or if d k p UNKEYED then
      KEY-KIND-UNDECLARED k KEY-REF-KIND ! exit
   then
   KEY-KIND-ENUMERATION k KEY-REF-KIND !
   e k KEY-REF-ENUM-TYPE ! ;

\ Sets K's kind from its property P's type: an Edm type, a V4 type definition
\ over one or enumeration type, else `undeclared`, rejecting any other.
: KEY-REF-TYPE ( ^doc ^key-ref ^property -- )
   {: d:^ k:^ p:^ :}
   p PROPERTY-TYPE @ {: t:str :}
   t PRIMITIVE? if d k p t EDM-KIND exit then
   d V2? if d k p UNKEYED then
   d t FIND-DEFINITION {: def:^type-definition :}
   def NULL-PTR = if d k p ENUM-KIND exit then
   d k p def TYPE-DEFINITION-UNDERLYING @ EDM-KIND ;

\ A property in T's Key is marked under `keyed`.
: KEYED? ( ^doc ^entity-type ^property -- bool )
   {: d:^ t:^ p:^ :}
   d KIND-KEYED t p PAIR-KEY d HELD? ;

: KEY-REF ( ^doc ^entity-type ^key-ref -- )
   {: d:^ t:^ k:^ :}
   k KEY-REF-NAME @ {: name:str :}
   d name HOLDER {: h:^entity-type :}
   h NULL-PTR = if NULL-PTR else
      d KIND-PROPERTY-NAME h name OWNED-KEY d FIND
   then {: p:^property :}
   p NULL-PTR = p NULL-PTR <> if d t p KEYED? or then if
      d k KEY-REF-OFFSET @ PLACE
      s" `Name` of `PropertyRef` to name a property of " REASON:APPEND
      t ENTITY-TYPE-NAME @ QUOTE s"  once" REASON:APPEND
      k KEY-REF-NAME @
      s" name a property the type declares or inherits, each once" FOUND-QUOTED
   then
   d KIND-KEYED t p PAIR-KEY d 0 REMEMBER
   p k KEY-REF-PROPERTY !
   d k p KEY-REF-TYPE ;

\ Sets T's fields from its base's, then holds its members and resolves its
\ Key.
: ENTER-TYPE ( ^doc ^entity-type n -- )
   {: d:^ t:^ rank:n :}
   rank t ENTITY-TYPE-RANK !
   t d DOC-RANKED @ rank 1- cells + !
   t t ENTITY-TYPE-KEY-HOLDER !
   t ENTITY-TYPE-BASE @ {: b:^ :}
   b NULL-PTR <> if
      b ENTITY-TYPE-DEPTH @ 1+ t ENTITY-TYPE-DEPTH !
      t ENTITY-TYPE-KEY @ NULL-PTR = if
         b ENTITY-TYPE-KEY-HOLDER @ t ENTITY-TYPE-KEY-HOLDER !
      then
      b ENTITY-TYPE-PROPERTIES @ NULL-PTR <> if b
      else b ENTITY-TYPE-PROPERTY-BASE @ then t ENTITY-TYPE-PROPERTY-BASE !
      b ENTITY-TYPE-NAVIGATIONS @ NULL-PTR <> if b
      else b ENTITY-TYPE-NAVIGATION-BASE @ then t ENTITY-TYPE-NAVIGATION-BASE !
   then
   d t HOLD-MEMBERS
   t ENTITY-TYPE-KEY @ begin dup NULL-PTR <> while
      {: k:^ :} d t k KEY-REF k KEY-REF-NEXT @
   repeat drop ;

\ After T, of rank LAST: its first derived type, else the sibling of T or of
\ its nearest base with one, each type left getting rank-end LAST; null at end.
: NEXT-TYPE ( ^entity-type n -- ^entity-type )
   {: t:^ last:n :}
   t ENTITY-TYPE-DERIVED @ dup NULL-PTR <> if exit then drop
   t begin
      last over ENTITY-TYPE-RANK-END !
      dup ENTITY-TYPE-SIBLING @ dup NULL-PTR <> if nip exit then drop
      ENTITY-TYPE-BASE @ dup NULL-PTR =
   until ;

\ Links T into its base's `derived` list, or ROOTS when it has none; linking
\ the doc's list, last declared first, leaves each list in declaration order.
: ADOPT ( ^entity-type ^entity-type -- ^entity-type )
   {: roots:^ t:^ :}
   t ENTITY-TYPE-BASE @ {: b:^ :}
   b NULL-PTR = if roots t ENTITY-TYPE-SIBLING ! t exit then
   b ENTITY-TYPE-DERIVED @ t ENTITY-TYPE-SIBLING !
   t b ENTITY-TYPE-DERIVED !
   roots ;

\ Enters each type whose base chain ends, depth first from the first of
\ ROOTS: down `derived`, across `sibling`, back up `base`.
: WALK-BASES ( ^doc ^entity-type -- )
   {: d:^ roots:^ :}
   0 roots begin dup NULL-PTR <> while
      {: last:n t:^ :}
      last 1+ {: rank:n :}
      d t rank ENTER-TYPE
      rank t rank NEXT-TYPE
   repeat 2drop ;

\ A type no walk reaches is in a cycle of base types or leads to one; the
\ first declared, the last such in the doc's list, is rejected.
: RESOLVE-BASES ( ^doc -- )
   {: d:^ :}
   d DOC-TYPES @ begin dup NULL-PTR <> while
      d over BASE ENTITY-TYPE-NEXT @
   repeat drop
   NULL-PTR d DOC-TYPES @ begin dup NULL-PTR <> while
      dup ENTITY-TYPE-NEXT @ -rot ADOPT swap
   repeat drop
   d DOC-NAMES @ d DOC-TYPE-COUNT @ cells ARENA:ALLOC drop >TYPES
   d DOC-RANKED !
   d swap WALK-BASES
   NULL-PTR d DOC-TYPES @ begin dup NULL-PTR <> while
      dup ENTITY-TYPE-RANK @ 0= if nip dup then ENTITY-TYPE-NEXT @
   repeat drop
   dup NULL-PTR = if drop exit then
   {: t:^ :}
   d t BASE-PLACE
   s"  to lead to a type with none" REASON:APPEND
   s" a cycle" s" remove a `BaseType` from the cycle" FOUND ;

\ The model's index of names, made once the walk has ranked the types.
: OWN-MEMBERS ( ^entity-type -- n )
   {: t:^ :}
   0 t ENTITY-TYPE-PROPERTIES @ begin dup NULL-PTR <> while
      swap 1+ swap PROPERTY-NEXT @
   repeat drop
   t ENTITY-TYPE-NAVIGATIONS @ begin dup NULL-PTR <> while
      swap 1+ swap NAVIGATION-NEXT @
   repeat drop ;

\ The model's sets, the singletons and every type's members.
: NAME-COUNT ( ^doc -- n )
   {: d:^ :}
   0 d DOC-MODEL @ METADATA-SETS @ begin dup NULL-PTR <> while
      swap 1+ swap ENTITY-SET-NEXT @
   repeat drop
   d DOC-SINGLETONS @ begin dup NULL-PTR <> while
      swap 1+ swap SINGLETON-NEXT @
   repeat drop
   d DOC-TYPES @ begin dup NULL-PTR <> while
      dup OWN-MEMBERS rot + swap ENTITY-TYPE-NEXT @
   repeat drop ;

\ NAME's `named`, made in the caller's arena under the name's own bytes
\ when the name is new.
: INDEX-ENTRY ( ^doc str -- ^named )
   {: d:^ name:str :}
   d DOC-MODEL @ name INDEXED {: x:^named :}
   x NULL-PTR <> if x exit then
   d DOC-ARENA @ NAMEDS-NEW {: y:^ :}
   y NULL-PTR - d DOC-MODEL @ NAMES-OF name MAP:SET
   y ;

\ Records each set under its name and counts each name's members.
: TALLY-NAMES ( ^doc -- )
   {: d:^ :}
   d DOC-MODEL @ METADATA-SETS @ begin dup NULL-PTR <> while
      dup d over ENTITY-SET-NAME @ INDEX-ENTRY NAMED-SET ! ENTITY-SET-NEXT @
   repeat drop
   d DOC-TYPES @ begin dup NULL-PTR <> while
      dup ENTITY-TYPE-PROPERTIES @ begin dup NULL-PTR <> while
         {: p:^ :}
         1 d p PROPERTY-NAME @ INDEX-ENTRY NAMED-PROPERTY-COUNT +!
         p PROPERTY-NEXT @
      repeat drop
      dup ENTITY-TYPE-NAVIGATIONS @ begin dup NULL-PTR <> while
         {: n:^ :}
         1 d n NAVIGATION-NAME @ INDEX-ENTRY NAMED-NAVIGATION-COUNT +!
         n NAVIGATION-NEXT @
      repeat drop
      ENTITY-TYPE-NEXT @
   repeat drop ;

\ The first append to a name's array makes it, of the counted cells, and
\ counts again from 0.
: FILE-PROPERTY ( ^doc ^property -- )
   {: d:^ p:^ :}
   d DOC-MODEL @ p PROPERTY-NAME @ INDEXED {: x:^named :}
   x NAMED-PROPERTIES @ NULL-PTR = if
      d DOC-ARENA @ x NAMED-PROPERTY-COUNT @ cells ARENA:ALLOC drop >PROPS
      x NAMED-PROPERTIES !
      0 x NAMED-PROPERTY-COUNT !
   then
   p x NAMED-PROPERTIES @ x NAMED-PROPERTY-COUNT @ cells + !
   1 x NAMED-PROPERTY-COUNT +! ;

: FILE-NAVIGATION ( ^doc ^navigation -- )
   {: d:^ n:^ :}
   d DOC-MODEL @ n NAVIGATION-NAME @ INDEXED {: x:^named :}
   x NAMED-NAVIGATIONS @ NULL-PTR = if
      d DOC-ARENA @ x NAMED-NAVIGATION-COUNT @ cells ARENA:ALLOC drop >NAVS
      x NAMED-NAVIGATIONS !
      0 x NAMED-NAVIGATION-COUNT !
   then
   n x NAMED-NAVIGATIONS @ x NAMED-NAVIGATION-COUNT @ cells + !
   1 x NAMED-NAVIGATION-COUNT +! ;

\ The map holds at most half its capacity, so it never grows.
: INDEX-NAMES ( ^doc -- )
   {: d:^ :}
   d NAME-COUNT 2 * {: cap:n :}
   d DOC-ARENA @ cap >COUNT MAP:IN d DOC-MODEL @ METADATA-NAMES !
   d TALLY-NAMES
   d DOC-TYPE-COUNT @ 0 ?do
      d DOC-RANKED @ i cells + @ {: t:^ :}
      t ENTITY-TYPE-PROPERTIES @ begin dup NULL-PTR <> while
         d over FILE-PROPERTY PROPERTY-NEXT @
      repeat drop
      t ENTITY-TYPE-NAVIGATIONS @ begin dup NULL-PTR <> while
         d over FILE-NAVIGATION NAVIGATION-NEXT @
      repeat drop
   loop ;

: ROOT-OF ( ^entity-type -- ^entity-type )
   begin dup ENTITY-TYPE-BASE @ dup NULL-PTR <> while nip repeat drop ;

\ Whether S's type is in T's inheritance tree.
: IN-TREE? ( ^entity-set ^entity-type -- bool )
   ROOT-OF swap ENTITY-SET-TYPE @ ROOT-OF = ;

\ The expected text of a set in the inheritance tree of the type NAME.
: TREE-EXPECTED ( str -- )
   s"  to name an entity set of a type in the inheritance tree of "
   REASON:APPEND QUOTE ;

\ The found and fix of S, a set outside the tree expected.
: TREE-FOUND ( ^entity-set -- )
   {: s:^ :}
   REASON:FOUND s ENTITY-SET-NAME @ QUOTE
   s"  of `EntityType` " REASON:APPEND s ENTITY-SET-TYPE-NAME @ QUOTE
   s" name an entity set of a type in that tree" FIX ;

: ENTITY? ( ^entity-type -- bool )
   {: t:^ :}
   t NULL-PTR = if false exit then
   t ENTITY-TYPE-COMPLEX @ 0= ;

\ The found and fix of NAME where an entity type is expected, T what it
\ resolves to.
: ENTITY-FOUND ( ^entity-type str -- )
   {: t:^ name:str :}
   t NULL-PTR = if
      name
      s" declare that entity type, or name one the document declares"
      FOUND-QUOTED
   else
      REASON:FOUND name QUOTE s" , a `ComplexType`" REASON:APPEND
      s" name an `EntityType` instead" FIX
   then ;

: SET-TYPE ( ^doc ^entity-set -- )
   {: d:^ s:^ :}
   s ENTITY-SET-TYPE-NAME @ {: name:str :}
   d name FIND-TYPE {: t:^ :}
   t ENTITY? 0= if
      d s ENTITY-SET-OFFSET @ PLACE
      s" `EntityType` of `EntitySet` " REASON:APPEND s ENTITY-SET-NAME @ QUOTE
      s"  to name an entity type the document declares" REASON:APPEND
      t name ENTITY-FOUND
   then
   t ENTITY-TYPE-KEY-HOLDER @ ENTITY-TYPE-KEY @ NULL-PTR = if
      d s ENTITY-SET-OFFSET @ PLACE
      s" `EntityType` " REASON:APPEND s ENTITY-SET-TYPE-NAME @ QUOTE
      s"  of `EntitySet` " REASON:APPEND s ENTITY-SET-NAME @ QUOTE
      s"  to have a `Key`" REASON:APPEND
      NONE$ s" give the entity type a `Key`" FOUND
   then
   t s ENTITY-SET-TYPE ! ;

\ S's name's `named` holds the entity type S's Type names.
: SINGLETON-RESOLVES ( ^doc ^singleton -- )
   {: d:^ s:^ :}
   s SINGLETON-TYPE-NAME @ {: name:str :}
   d name FIND-TYPE {: t:^ :}
   t ENTITY? 0= if
      d s SINGLETON-OFFSET @ PLACE
      s" `Type` of `Singleton` " REASON:APPEND s SINGLETON-NAME @ QUOTE
      s"  to name an entity type the document declares" REASON:APPEND
      t name ENTITY-FOUND
   then
   t d s SINGLETON-NAME @ INDEX-ENTRY NAMED-SINGLETON ! ;

: RESOLVE-CONTAINER-TYPES ( ^doc -- )
   {: d:^ :}
   d DOC-MODEL @ METADATA-SETS @ begin dup NULL-PTR <> while
      d over SET-TYPE ENTITY-SET-NEXT @
   repeat drop
   d DOC-SINGLETONS @ begin dup NULL-PTR <> while
      d over SINGLETON-RESOLVES SINGLETON-NEXT @
   repeat drop ;

: END-RESOLVES ( ^doc ^association-end -- )
   {: d:^ e:^ :}
   e ASSOCIATION-END-TYPE @ {: name:str :}
   d name FIND-TYPE {: t:^ :}
   t ENTITY? if exit then
   d e ASSOCIATION-END-OFFSET @ PLACE
   s" `Type` of `End` " REASON:APPEND e ASSOCIATION-END-ROLE @ QUOTE
   s"  to name an entity type the document declares" REASON:APPEND
   t name ENTITY-FOUND ;

\ What EL named NAME is expected to hold: an End for each of two roles.
: ENDS ( str str -- )
   {: el:str name:str :}
   s" two `End` elements in `" REASON:APPEND el REASON:APPEND s" ` " REASON:APPEND
   name QUOTE s" , one for each role" REASON:APPEND ;

: ENDS-FOUND ( n -- )
   FMT:INT$ s" give it one `End` for each of the two roles" FOUND ;

: ASSOCIATION-RESOLVES ( ^doc ^association -- )
   {: d:^ a:^ :}
   0 a ASSOCIATION-ENDS @ begin dup NULL-PTR <> while
      d over END-RESOLVES ASSOCIATION-END-NEXT @ swap 1+ swap
   repeat drop {: count:n :}
   count 2 = if exit then
   d a ASSOCIATION-OFFSET @ PLACE
   s" Association" a ASSOCIATION-NAME @ ENDS count ENDS-FOUND ;

: RESOLVE-ENDS ( ^doc -- )
   {: d:^ :}
   d DOC-ASSOCIATIONS @ begin dup NULL-PTR <> while
      d over ASSOCIATION-RESOLVES ASSOCIATION-NEXT @
   repeat drop ;

\ Opens the text with ATTR of the navigation property N.
: NAVIGATION-PLACE ( ^doc ^navigation str -- )
   {: d:^ n:^ attr:str :}
   d n NAVIGATION-OFFSET @ PLACE
   s" `" REASON:APPEND attr REASON:APPEND s" ` of `NavigationProperty` " REASON:APPEND
   n NAVIGATION-NAME @ QUOTE ;

: ROLE ( ^doc ^navigation ^association str str -- ^association-end )
   {: d:^ n:^ a:^ role:str attr:str :}
   d a role FIND-END dup NULL-PTR <> if exit then drop
   d n attr NAVIGATION-PLACE
   s"  to name a role an `End` of `Association` " REASON:APPEND
   a ASSOCIATION-NAME @ QUOTE s"  declares" REASON:APPEND
   role s" name a role an `End` of the `Association` declares"
   FOUND-QUOTED ;

: V2-NAVIGATION ( ^doc ^navigation -- )
   {: d:^ n:^ :}
   d n NAVIGATION-RELATIONSHIP @ FIND-ASSOCIATION {: a:^ :}
   a NULL-PTR = if
      d n s" Relationship" NAVIGATION-PLACE
      s"  to name an `Association` the document declares" REASON:APPEND
      n NAVIGATION-RELATIONSHIP @
      s" declare that `Association`, or name one the document declares"
      FOUND-QUOTED
   then
   a n NAVIGATION-ASSOCIATION !
   d n a n NAVIGATION-FROM-ROLE @ s" FromRole" ROLE n NAVIGATION-FROM !
   d n a n NAVIGATION-TO-ROLE @ s" ToRole" ROLE {: e:^ :}
   e n NAVIGATION-TO !
   d e ASSOCIATION-END-TYPE @ FIND-TYPE n NAVIGATION-TARGET-TYPE !
   e ASSOCIATION-END-MANY @ n NAVIGATION-MANY ! ;

\ `Collection(X)` is to-many X.
: COLLECTION ( str -- str bool )
   {: text:str :}
   text s" Collection(" STR:STARTS-WITH?
   text STR:LEN 0 > if text STR:DATA text STR:LEN + 1- c@ [char] ) = and then
   if s" Collection(" STR:LEN {: open:n :} text STR:DATA open + text STR:LEN open - 1- >str true exit then
   text false ;

: V4-NAVIGATION ( ^doc ^navigation -- )
   {: d:^ n:^ :}
   n NAVIGATION-TYPE @ COLLECTION n NAVIGATION-MANY !
   {: tn:str :}
   d tn FIND-TYPE {: t:^ :}
   t ENTITY? 0= if
      d n s" Type" NAVIGATION-PLACE
      s"  to name an entity type the document declares" REASON:APPEND
      t n NAVIGATION-TYPE @ ENTITY-FOUND
   then
   t n NAVIGATION-TARGET-TYPE ! ;

: VALUE-TYPE ( ^doc ^property -- )
   {: d:^ p:^ :}
   p PROPERTY-TYPE @ COLLECTION {: tn:str many:bool :}
   d tn FIND-TYPE {: t:^ :}
   t NULL-PTR = if exit then
   t ENTITY-TYPE-COMPLEX @ 0= if exit then
   t p PROPERTY-COMPLEX-TYPE !
   many p PROPERTY-MANY ! ;

: RESOLVE-VALUE-TYPES ( ^doc -- )
   {: d:^ :}
   d DOC-TYPES @ begin dup NULL-PTR <> while
      dup ENTITY-TYPE-PROPERTIES @ begin dup NULL-PTR <> while
         d over VALUE-TYPE PROPERTY-NEXT @
      repeat drop
      ENTITY-TYPE-NEXT @
   repeat drop ;

: RESOLVE-NAVIGATIONS ( ^doc -- )
   {: d:^ :}
   d DOC-TYPES @ begin dup NULL-PTR <> while
      dup ENTITY-TYPE-NAVIGATIONS @ begin dup NULL-PTR <> while
         d V2? if d over V2-NAVIGATION else d over V4-NAVIGATION then
         NAVIGATION-NEXT @
      repeat drop
      ENTITY-TYPE-NEXT @
   repeat drop ;

\ Rejects E when its set is outside its End's Type's inheritance tree.
: END-FITS ( ^doc ^set-end -- )
   {: d:^ e:^ :}
   e SET-END-SET @ {: s:^ :}
   e SET-END-END @ ASSOCIATION-END-TYPE @ {: tn:str :}
   s d tn FIND-TYPE IN-TREE? if exit then
   d e SET-END-OFFSET @ PLACE
   s" `EntitySet` of `End` " REASON:APPEND e SET-END-ROLE @ QUOTE
   tn TREE-EXPECTED s TREE-FOUND ;

: SET-END ( ^set-end ^doc ^association -- )
   {: e:^ d:^ a:^ :}
   d a e SET-END-ROLE @ FIND-END {: ae:^ :}
   ae NULL-PTR = if
      d e SET-END-OFFSET @ PLACE
      s" `Role` of `End` to name a role of `Association` " REASON:APPEND
      a ASSOCIATION-NAME @ QUOTE
      e SET-END-ROLE @ s" name a role an `End` of the `Association` declares"
      FOUND-QUOTED
   then
   ae e SET-END-END !
   d e SET-END-SET-NAME @ FIND-SET {: s:^ :}
   s NULL-PTR = if
      d e SET-END-OFFSET @ PLACE
      s" `EntitySet` of `End` " REASON:APPEND e SET-END-ROLE @ QUOTE
      s"  to name an entity set the document declares" REASON:APPEND
      e SET-END-SET-NAME @
      s" declare that entity set, or name one the document declares" FOUND-QUOTED
   then
   s e SET-END-SET !
   d e END-FITS ;

: TARGETS-OF ( ^metadata -- ^n ) METADATA-TARGETS @ ;

\ A targets key: S's address, then R's, with no kind.
: TARGET-KEY ( ^doc ^entity-set ^t -- )
   {: d:^ s:^ r:^ :}
   0 d DOC-KEY-LEN ! d s KEY-AT+ d r KEY-AT+ ;

: TARGET-HELD? ( ^doc -- bool )
   {: d:^ :}
   d DOC-MODEL @ TARGETS-OF d KEY$ MAP:HAS? ;

: TARGET-FIND ( ^doc -- ^t ) dup DOC-MODEL @ TARGETS-OF swap KEY$ MAP-RECORD ;

\ Sets the key to V, the key copied into the caller's arena.
: ADD-TARGET ( ^doc n -- )
   {: d:^ v:n :}
   d DOC-ARENA @ d DOC-KEY-LEN @ ARENA:ALLOC {: copy:^ len:n :}
   d DOC-KEY @ copy len BYTE-COPY
   v d DOC-MODEL @ TARGETS-OF copy len >str MAP:SET ;

\ The bindings and the association sets' Ends: a targets entry each at most.
: TARGET-COUNT ( ^doc -- n )
   {: d:^ :}
   0 d DOC-BINDINGS @ begin dup NULL-PTR <> while
      swap 1+ swap BINDING-NEXT @
   repeat drop
   d DOC-ASSOCIATION-SETS @ begin dup NULL-PTR <> while
      dup ASSOCIATION-SET-ENDS @ begin dup NULL-PTR <> while
         rot 1+ -rot SET-END-NEXT @
      repeat drop
      ASSOCIATION-SET-NEXT @
   repeat drop ;

\ `targets` holds at most half its capacity, so it never grows.
: TARGETS-NEW ( ^doc -- )
   {: d:^ :}
   d TARGET-COUNT {: count:n :}
   count 2 * {: cap:n :}
   d DOC-ARENA @ cap >COUNT MAP:IN d DOC-MODEL @ METADATA-TARGETS !
   d DOC-NAMES @ count cells ARENA:ALLOC drop >SET-ENDS d DOC-SECONDS ! ;

: NOTE-SECOND ( ^doc ^set-end -- )
   {: d:^ e:^ :}
   e d DOC-SECONDS @ d DOC-SECOND-COUNT @ cells + !
   1 d DOC-SECOND-COUNT +! ;

\ The first association set to put a set at a role is in `targets`, the
\ second under `link-second`, its End in `seconds`.
: ADD-LINK ( ^doc ^association-set ^set-end -- )
   {: d:^ x:^ e:^ :}
   e SET-END-END @ e SET-END-SET @ {: ae:^ s:^ :}
   d s ae TARGET-KEY
   d TARGET-HELD? 0= if d x NULL-PTR - ADD-TARGET exit then
   d KIND-LINK-SECOND ae s PAIR-KEY
   d HELD? if exit then
   d x REMEMBER-AT
   d e NOTE-SECOND ;

: ASSOCIATION-SET-PLACE ( ^doc ^association-set -- )
   {: d:^ x:^ :}
   d x ASSOCIATION-SET-OFFSET @ PLACE ;

: ASSOCIATION-SET ( ^doc ^association-set -- )
   {: d:^ x:^ :}
   d x ASSOCIATION-SET-ASSOCIATION-NAME @ FIND-ASSOCIATION {: a:^ :}
   a NULL-PTR = if
      d x ASSOCIATION-SET-PLACE
      s" `Association` of `AssociationSet` " REASON:APPEND x ASSOCIATION-SET-NAME @ QUOTE
      s"  to name an `Association` the document declares" REASON:APPEND
      x ASSOCIATION-SET-ASSOCIATION-NAME @
      s" declare that `Association`, or name one the document declares"
      FOUND-QUOTED
   then
   a x ASSOCIATION-SET-ASSOCIATION !
   0 x ASSOCIATION-SET-ENDS @ begin dup NULL-PTR <> while
      SET-END-NEXT @ swap 1+ swap
   repeat drop {: count:n :}
   count 2 <> if
      d x ASSOCIATION-SET-PLACE
      s" AssociationSet" x ASSOCIATION-SET-NAME @ ENDS count ENDS-FOUND
   then
   x ASSOCIATION-SET-ENDS @ {: e:^ :}
   e SET-END-NEXT @ {: f:^ :}
   e d a SET-END f d a SET-END
   e SET-END-END @ f SET-END-END @ = if
      d x ASSOCIATION-SET-PLACE
      s" AssociationSet" x ASSOCIATION-SET-NAME @ e SET-END-ROLE @
      ROLE-TWICE
   then
   d x e ADD-LINK d x f ADD-LINK ;

: RESOLVE-ASSOCIATION-SETS ( ^doc -- )
   {: d:^ :}
   d DOC-ASSOCIATION-SETS @ begin dup NULL-PTR <> while
      d over ASSOCIATION-SET ASSOCIATION-SET-NEXT @
   repeat drop ;

\ A binding Target is a set or singleton, alone or after the container's name;
\ a containment path (a slash and navigations after it) leads to no set.
: CONTAINER-PATH? ( ^doc str -- bool )
   {: d:^ s:str :}
   s d DOC-CONTAINER @ STR:= if true exit then
   s QUALIFIER {: q:str tail:str :}
   tail d DOC-CONTAINER @ STR:= 0= if false exit then
   d q ALIAS d DOC-CONTAINER-NAMESPACE @ STR:= ;

: TARGET-PLACE ( ^doc ^binding -- )
   {: d:^ b:^ :}
   d b BINDING-OFFSET @ PLACE
   s" `Target` of `NavigationPropertyBinding` " REASON:APPEND b BINDING-PATH @ QUOTE ;

: TARGET-RESOLVES ( ^doc ^binding -- )
   {: d:^ b:^ :}
   b BINDING-TARGET-NAME @ {: text:str :}
   text SLASH {: i:n :}
   i 0 < if
      text
   else
      text i SPLIT {: head:str tail:str :}
      d head CONTAINER-PATH? if tail else text then
   then {: name:str :}
   name SLASH {: j:n :}
   j 0 >= if
      name j SPLIT drop {: owner:str :}
      d owner FIND-SET NULL-PTR = d owner SINGLETON? 0= and if
         d b TARGET-PLACE
         s"  to name an entity set or singleton of " REASON:APPEND
         d DOC-CONTAINER @ QUOTE
         b BINDING-TARGET-NAME @
         s" name an entity set or singleton the container declares" FOUND-QUOTED
      then
      NULL-PTR b BINDING-TARGET ! exit
   then
   d name FIND-SET dup b BINDING-TARGET !
   NULL-PTR <> if exit then
   d name SINGLETON? if exit then
   d b TARGET-PLACE
   s"  to name an entity set or singleton the document declares" REASON:APPEND
   b BINDING-TARGET-NAME @
   s" declare that entity set or singleton, or name one the document declares"
   FOUND-QUOTED ;

\ The navigation a Path without a slash names; null for a Path with one.
: PATH-RESOLVES ( ^doc ^binding -- ^navigation )
   {: d:^ b:^ :}
   b BINDING-PATH @ HAS-SLASH? if NULL-PTR exit then
   b BINDING-SET @ ENTITY-SET-TYPE @
   b BINDING-PATH @ FIND-NAVIGATION dup NULL-PTR <> if exit then
   drop d b BINDING-OFFSET @ PLACE
   s" `Path` of `NavigationPropertyBinding` to name a navigation property of " REASON:APPEND
   b BINDING-SET @ ENTITY-SET-TYPE @ ENTITY-TYPE-NAME @ QUOTE
   b BINDING-PATH @
   s" name a navigation property the type declares or inherits" FOUND-QUOTED ;

\ Rejects B when the set it targets along N is outside N's target type's
\ inheritance tree.
: TARGET-FITS ( ^doc ^binding ^navigation -- )
   {: d:^ b:^ n:^ :}
   b BINDING-TARGET @ {: s:^ :}
   n NULL-PTR = s NULL-PTR = or if exit then
   s n NAVIGATION-TARGET-TYPE @ IN-TREE? if exit then
   d b TARGET-PLACE
   n NAVIGATION-TYPE @ COLLECTION drop TREE-EXPECTED s TREE-FOUND ;

\ V4: a second binding of N on the set is rejected; the target is kept.
: ADD-BINDING-TARGET ( ^doc ^binding ^navigation -- )
   {: d:^ b:^ n:^ :}
   n NULL-PTR = d V2? or if exit then
   b BINDING-SET @ {: s:^ :}
   d KIND-BINDING-SECOND s b BINDING-PATH @ OWNED-KEY
   d FIND {: c:^binding :}
   c NULL-PTR <> if
      d c BINDING-OFFSET @ PLACE
      s" a `Path` no other `NavigationPropertyBinding` of `EntitySet` " REASON:APPEND
      s ENTITY-SET-NAME @ QUOTE s"  has" REASON:APPEND
      c BINDING-PATH @ s" remove one of the two bindings" FOUND-QUOTED
   then
   d s n TARGET-KEY
   d b BINDING-TARGET @ NULL-PTR - ADD-TARGET ;

: RESOLVE-BINDINGS ( ^doc -- )
   {: d:^ :}
   d DOC-BINDINGS @ begin dup NULL-PTR <> while
      {: b:^ :}
      d b TARGET-RESOLVES
      d b PATH-RESOLVES {: n:^ :}
      d b n TARGET-FITS
      d b n ADD-BINDING-TARGET
      b BINDING-NEXT @
   repeat drop ;

\ Seconds sort by their End's role end, then by their set's type's rank.
: SECOND-KEY ( ^set-end -- n n )
   dup SET-END-END @ NULL-PTR -
   swap SET-END-SET @ ENTITY-SET-TYPE @ ENTITY-TYPE-RANK @ ;

: KEY<? ( n n n n -- bool )
   {: a:n r:n b:n q:n :}
   a b = if r q < exit then
   a b < ;

: SECOND<? ( ^set-end ^set-end -- bool )
   swap SECOND-KEY rot SECOND-KEY KEY<? ;

\ The second association set at E's role and set is rejected for N.
: SECOND-REJECTED ( ^doc ^set-end ^navigation -- )
   {: d:^ e:^ n:^ :}
   e SET-END-END @ e SET-END-SET @ {: ae:^ s:^ :}
   d KIND-LINK-SECOND ae s PAIR-KEY d FIND {: y:^association-set :}
   d s ae TARGET-KEY d TARGET-FIND {: x:^association-set :}
   d y ASSOCIATION-SET-PLACE
   s" `AssociationSet` " REASON:APPEND y ASSOCIATION-SET-NAME @ QUOTE
   s"  to be the only one for `EntitySet` " REASON:APPEND s ENTITY-SET-NAME @ QUOTE
   s"  and `NavigationProperty` " REASON:APPEND n NAVIGATION-NAME @ QUOTE
   x ASSOCIATION-SET-NAME @ s" remove one of the two association sets"
   FOUND-QUOTED ;

\ N reaches a second at its FromRole end whose set's type ranks in its owner's
\ interval; the first second at or above the owner's rank decides.
: REACHES-SECOND ( ^doc ^navigation -- )
   {: d:^ n:^ :}
   n NAVIGATION-FROM @ NULL-PTR - {: from:n :}
   n NAVIGATION-OWNER @ {: o:^ :}
   d DOC-SECONDS @ {: es:^ :}
   d DOC-SECOND-COUNT @ {: count:n :}
   0 count
   [: cells es + @ SECOND-KEY from o ENTITY-TYPE-RANK @ KEY<? ;]
   SORT:PARTITION-POINT {: i:n :}
   i count >= if exit then
   es i cells + @ {: e:^ :}
   e SECOND-KEY {: end:n rank:n :}
   end from = rank o ENTITY-TYPE-RANK-END @ <= and 0= if exit then
   d e n SECOND-REJECTED ;

\ V2: a role and a set two association sets share are rejected when a
\ navigation from that role is the set's type's or a base type's.
: RESOLVE-SECONDS ( ^doc -- )
   {: d:^ :}
   d V2? 0= d DOC-SECOND-COUNT @ 0= or if exit then
   d DOC-SECONDS @ d DOC-SECOND-COUNT @ [: SECOND<? ;] SORT:SORT!
   d DOC-TYPES @ begin dup NULL-PTR <> while
      dup ENTITY-TYPE-NAVIGATIONS @ begin dup NULL-PTR <> while
         d over REACHES-SECOND NAVIGATION-NEXT @
      repeat drop
      ENTITY-TYPE-NEXT @
   repeat drop ;

\ Moves A's list onto LIST's.
: ATTACH ( ^annotation ^^annotation -- )
   {: a:^ list:^ :}
   a begin dup NULL-PTR <> while
      {: b:^annotation :}
      b ANNOTATION-NEXT @
      list @ b ANNOTATION-NEXT !
      b list !
   repeat drop ;

: ATTACH-FILTERS ( ^filter ^^filter -- )
   {: f:^ list:^ :}
   f begin dup NULL-PTR <> while
      {: g:^filter :}
      g FILTER-NEXT @
      list @ g FILTER-NEXT !
      g list !
   repeat drop ;

\ A Target naming a type or property takes the block's annotations, a set of
\ the container its filters; others, as SAP's navigations, are read past.
: BLOCK ( ^doc ^block -- )
   {: d:^ b:^ :}
   b BLOCK-TARGET @ {: text:str :}
   text SLASH {: i:n :}
   i 0 < if
      d text FIND-TYPE dup NULL-PTR = if drop exit then
      b BLOCK-ANNOTATIONS @ swap ENTITY-TYPE-ANNOTATIONS ATTACH exit
   then
   text i SPLIT {: head:str tail:str :}
   d head CONTAINER-PATH? if
      d tail FIND-SET dup NULL-PTR = if drop exit then
      b BLOCK-FILTERS @ swap ENTITY-SET-FILTERS ATTACH-FILTERS exit
   then
   d head FIND-TYPE {: t:^ :}
   t NULL-PTR = if exit then
   d KIND-PROPERTY-NAME t tail OWNED-KEY d FIND {: p:^property :}
   p NULL-PTR = if exit then
   b BLOCK-ANNOTATIONS @ p PROPERTY-ANNOTATIONS ATTACH ;

: RESOLVE-BLOCKS ( ^doc -- )
   {: d:^ :}
   d DOC-BLOCKS @ begin dup NULL-PTR <> while
      d over BLOCK BLOCK-NEXT @
   repeat drop ;

\ An alias becomes its namespace.
: TERM ( ^doc ^annotation -- )
   {: d:^ a:^ :}
   d a ANNOTATION-NAMESPACE @ ALIAS a ANNOTATION-NAMESPACE ! ;

\ A's term on the target whose list starts at LIST.
: TERM-KEY ( ^doc ^annotation ^annotation -- )
   {: d:^ list:^ a:^ :}
   a ANNOTATION-NAMESPACE @ {: ns:str :}
   d KIND-TERM KEY-NEW d list KEY-AT+ d ns STR:LEN KEY-CELL+ d ns KEY+
   d a ANNOTATION-NAME @ KEY+ ;

\ The earlier of A and B in the document; either may be null.
: EARLIER ( ^annotation ^annotation -- ^annotation )
   {: a:^ b:^ :}
   a NULL-PTR = if b exit then
   b NULL-PTR = if a exit then
   a ANNOTATION-OFFSET @ b ANNOTATION-OFFSET @ < if a else b then ;

\ The candidate A makes when an annotation in LIST has its term, else null.
: UNIQUE ( ^doc ^annotation ^annotation -- ^annotation )
   {: d:^ list:^ a:^ :}
   d list a TERM-KEY
   d HELD? 0= if d a REMEMBER-AT NULL-PTR exit then
   d FIND {: held:^annotation :}
   held ANNOTATION-OFFSET @ a ANNOTATION-OFFSET @ < if a exit then
   d a REMEMBER-AT held ;

\ The Annotation at AT is rejected as a second of the term NAMESPACE, NAME.
: SECOND-TERM ( ^doc n str str -- )
   {: d:^ at:n ns:str nm:str :}
   d at PLACE
   s" one `Annotation` of term " REASON:APPEND nm QUOTE
   s"  of " REASON:APPEND ns QUOTE s"  per target" REASON:APPEND
   s" a second" s" remove one of the two" FOUND ;

\ A, unless null, is rejected as a second annotation of its term.
: DUPLICATE ( ^doc ^annotation -- )
   {: d:^ a:^ :}
   a NULL-PTR = if exit then
   d a ANNOTATION-OFFSET @ a ANNOTATION-NAMESPACE @ a ANNOTATION-NAME @
   SECOND-TERM ;

\ Rejects a second annotation of one term on one target, at its later copy.
: TERMS ( ^doc ^annotation -- )
   {: d:^ list:^ :}
   NULL-PTR list begin dup NULL-PTR <> while
      {: least:^annotation a:^annotation :}
      d a TERM
      least d list a UNIQUE EARLIER
      a ANNOTATION-NEXT @
   repeat drop
   d swap DUPLICATE ;

: TYPE-TERMS ( ^doc ^entity-type -- )
   {: d:^ t:^ :}
   d t ENTITY-TYPE-ANNOTATIONS @ TERMS
   t ENTITY-TYPE-PROPERTIES @ begin dup NULL-PTR <> while
      d over PROPERTY-ANNOTATIONS @ TERMS PROPERTY-NEXT @
   repeat drop ;

: RESOLVE-TERMS ( ^doc -- )
   {: d:^ :}
   d DOC-TYPES @ begin dup NULL-PTR <> while
      d over TYPE-TERMS ENTITY-TYPE-NEXT @
   repeat drop ;

: CAPABILITIES$ ( -- str ) s" Org.OData.Capabilities.V1" ;

\ The earlier of F and G in the document; either may be null.
: EARLIER-FILTER ( ^filter ^filter -- ^filter )
   {: f:^ g:^ :}
   f NULL-PTR = if g exit then
   g NULL-PTR = if f exit then
   f FILTER-OFFSET @ g FILTER-OFFSET @ < if f else g then ;

\ Whether F's term, its alias resolved, is in Org.OData.Capabilities.V1.
: CAPABILITIES? ( ^doc ^filter -- bool )
   {: d:^ f:^ :}
   d f FILTER-NAMESPACE @ ALIAS CAPABILITIES$ STR:= ;

\ The earliest CAPABILITIES? filter on LIST other than BUT, null for none.
: EARLIEST ( ^doc ^filter ^filter -- ^filter )
   {: d:^ list:^ but:^ :}
   NULL-PTR list begin dup NULL-PTR <> while
      {: least:^filter f:^filter :}
      least
      f but <> d f CAPABILITIES? and if f EARLIER-FILTER then
      f FILTER-NEXT @
   repeat drop ;

\ The earliest is the set's filter; a second is rejected.
: SET-FILTER ( ^doc ^entity-set -- )
   {: d:^ s:^ :}
   s ENTITY-SET-FILTERS @ {: list:^filter :}
   d list NULL-PTR EARLIEST {: first:^filter :}
   first s ENTITY-SET-FILTER !
   d list first EARLIEST {: second:^filter :}
   second NULL-PTR = if exit then
   d second FILTER-OFFSET @ CAPABILITIES$
   s" FilterRestrictions" SECOND-TERM ;

: RESOLVE-FILTERS ( ^doc -- )
   {: d:^ :}
   d DOC-MODEL @ METADATA-SETS @ begin dup NULL-PTR <> while
      d over SET-FILTER ENTITY-SET-NEXT @
   repeat drop ;

: RESOLVE ( ^doc -- )
   {: d:^ :}
   d RESOLVE-BASES
   d INDEX-NAMES
   d RESOLVE-CONTAINER-TYPES
   d RESOLVE-ENDS
   d RESOLVE-NAVIGATIONS
   d RESOLVE-VALUE-TYPES
   d TARGETS-NEW
   d RESOLVE-ASSOCIATION-SETS
   d RESOLVE-BINDINGS
   d RESOLVE-SECONDS
   d RESOLVE-BLOCKS
   d RESOLVE-FILTERS
   d RESOLVE-TERMS ;

: PARSE ( ^doc str -- )
   {: d:^ src:str :}
   src d DOC-SOURCE !
   d DOC-NAMES @ ARENA:ALLOCATOR 0 [type] u8 VEC:NEW d DOC-SCRATCH !
   d 16 MAP-NEW
   d DOC-CURSOR @ HEAP:ALLOCATOR src XML:INIT
   d EVENTS XML:CLOSE
   d RESOLVE ;

\ The types from T up that declare properties, collected in BUF, a cell per
\ base type and T, so the callback runs over them from the top down.
: PROPERTIES-IN ( ^entity-type [ ^property -- ] ^u8 NUM:alloc-byte-len -- )
   drop >TYPES {: t:^ q buf:^ :}
   0 t ENTITY-TYPE-PROPERTIES @ NULL-PTR <> if t
   else t ENTITY-TYPE-PROPERTY-BASE @ then
   begin dup NULL-PTR <> while
      2dup swap cells buf swap + ! ENTITY-TYPE-PROPERTY-BASE @ swap 1+ swap
   repeat drop
   begin dup 0 > while
      1- dup cells buf swap + @ ENTITY-TYPE-PROPERTIES @
      begin dup NULL-PTR <> while dup q execute PROPERTY-NEXT @ repeat drop
   repeat drop ;

: PROPERTIES-OF ( ^entity-type [ ^property -- ] -- )
   over ENTITY-TYPE-DEPTH @ 1+ cells MEM:BYTES-ALLOC-LEN
   [: PROPERTIES-IN ;] MEM:WITH-BYTES ;

: NAVIGATIONS-IN ( ^entity-type [ ^navigation -- ] ^u8 NUM:alloc-byte-len -- )
   drop >TYPES {: t:^ q buf:^ :}
   0 t ENTITY-TYPE-NAVIGATIONS @ NULL-PTR <> if t
   else t ENTITY-TYPE-NAVIGATION-BASE @ then
   begin dup NULL-PTR <> while
      2dup swap cells buf swap + ! ENTITY-TYPE-NAVIGATION-BASE @ swap 1+ swap
   repeat drop
   begin dup 0 > while
      1- dup cells buf swap + @ ENTITY-TYPE-NAVIGATIONS @
      begin dup NULL-PTR <> while dup q execute NAVIGATION-NEXT @ repeat drop
   repeat drop ;

: NAVIGATIONS-OF ( ^entity-type [ ^navigation -- ] -- )
   over ENTITY-TYPE-DEPTH @ 1+ cells MEM:BYTES-ALLOC-LEN
   [: NAVIGATIONS-IN ;] MEM:WITH-BYTES ;

\ Whether T is the navigation's owner or derives from it.
: HAS-NAVIGATION? ( ^entity-type ^navigation -- bool )
   NAVIGATION-OWNER @ {: t:^ o:^ :}
   t ENTITY-TYPE-RANK @ {: r:n :}
   o ENTITY-TYPE-RANK @ r <= r o ENTITY-TYPE-RANK-END @ <= and ;

: TARGET-IN ( ^metadata n n ^u8 NUM:alloc-byte-len -- option<n> )
   drop {: m:^ a:n b:n key:^ :}
   a key CELL-VIEW !
   b key 1 cells + CELL-VIEW !
   m TARGETS-OF key 2 cells >str MAP:GET ;

\ The targets entry under the addresses A and B, as distances from null.
: TARGET-OF ( ^metadata n n -- option<n> )
   2 cells MEM:BYTES-ALLOC-LEN [: TARGET-IN ;] MEM:WITH-BYTES ;

\ The set at X's End for the role E plays.
: END-SET ( ^association-set ^association-end -- ^entity-set )
   {: x:^ e:^ :}
   x ASSOCIATION-SET-ENDS @ begin dup NULL-PTR <> while
      dup SET-END-END @ e = if SET-END-SET @ exit then
      SET-END-NEXT @
   repeat drop NULL-PTR ;

: V2-TARGET ( ^entity-set ^navigation -- nullable<entity-set> )
   {: s:^ n:^ :}
   n NAVIGATION-OWNER @ ENTITY-TYPE-MODEL @
   s NULL-PTR - n NAVIGATION-FROM @ NULL-PTR - TARGET-OF MATCH option
      none OF NULLABLE:NONE ENDOF
      some OF >RECORD n NAVIGATION-TO @ END-SET NULLABLE:SOME ENDOF
   ;MATCH ;

: V4-TARGET ( ^entity-set ^navigation -- nullable<entity-set> )
   {: s:^ n:^ :}
   n NAVIGATION-OWNER @ ENTITY-TYPE-MODEL @
   s NULL-PTR - n NULL-PTR - TARGET-OF MATCH option
      none OF NULLABLE:NONE ENDOF
      some OF >RECORD NULLABLE:SOME ENDOF
   ;MATCH ;

: ANNOTATION ( ^annotation str str -- str bool )
   {: a:^ ns:str name:str :}
   a begin dup NULL-PTR <> while
      dup ANNOTATION-NAME @ name STR:=
      over ANNOTATION-NAMESPACE @ ns STR:= and if
         ANNOTATION-TEXT @ true exit
      then
      ANNOTATION-NEXT @
   repeat drop name STR:DATA 0 >str false ;

: KEY-REFS-OF ( ^entity-type [ ^key-ref -- ] -- )
   {: t:^ q :}
   t ENTITY-TYPE-KEY-HOLDER @ ENTITY-TYPE-KEY @ begin dup NULL-PTR <> while
      dup q execute KEY-REF-NEXT @
   repeat drop ;

\ HIDE-TYPE through SETTLE-PASS answer true when they hid what was not hidden.
: HIDE-TYPE ( ^entity-type -- bool )
   {: t:^ :}
   t ENTITY-TYPE-HIDDEN @ if false exit then
   true t ENTITY-TYPE-HIDDEN ! true ;

: HIDE-NAMED ( ^named -- bool )
   {: x:^named :}
   x NAMED-HIDDEN @ if false exit then
   true x NAMED-HIDDEN ! true ;

\ Hides every type of T's inheritance tree.
: HIDE-FAMILY ( ^entity-type -- bool )
   ROOT-OF {: r:^ :}
   false r ENTITY-TYPE-MODEL @ METADATA-TYPES @
   begin dup NULL-PTR <> while
      {: u:^ :}
      u ROOT-OF r = if u HIDE-TYPE or then
      u ENTITY-TYPE-NEXT @
   repeat drop ;

\ Hides T, not yet hidden, when its key, its own or inherited, holds a
\ hidden name.
: HIDE-KEYED ( ^entity-type -- bool )
   {: t:^ :}
   t ENTITY-TYPE-MODEL @ {: m:^ :}
   t [: KEY-REF-PROPERTY @ PROPERTY-NAME @
      m swap HIDDEN? if true t ENTITY-TYPE-HIDDEN ! then ;] KEY-REFS-OF
   t ENTITY-TYPE-HIDDEN @ ;

\ Hides the name of each navigation T declares to a hidden type.
: HIDE-LEADING ( ^entity-type -- bool )
   {: t:^ :}
   t ENTITY-TYPE-MODEL @ {: m:^ :}
   false t ENTITY-TYPE-NAVIGATIONS @ begin dup NULL-PTR <> while
      {: n:^ :}
      n NAVIGATION-TARGET-TYPE @ ENTITY-TYPE-HIDDEN @ if
         m n NAVIGATION-NAME @ INDEXED HIDE-NAMED or
      then
      n NAVIGATION-NEXT @
   repeat drop ;

: SETTLE-PASS ( ^metadata -- bool )
   false swap METADATA-TYPES @ begin dup NULL-PTR <> while
      {: t:^ :}
      t ENTITY-TYPE-HIDDEN @ if t HIDE-FAMILY else t HIDE-KEYED then or
      t HIDE-LEADING or
      t ENTITY-TYPE-NEXT @
   repeat drop ;

\ Applies the hide rules until a pass hides nothing; a rule only sets flags,
\ so the passes end.
: SETTLE ( ^metadata -- ) begin dup SETTLE-PASS 0= until drop ;

: SERVED? ( ^entity-set -- bool ) ENTITY-SET-TYPE @ ENTITY-TYPE-HIDDEN @ 0= ;

\ Whether NAME is hidden or names a set or singleton of a hidden type.
: QUERY-HIDDEN? ( ^metadata str -- bool )
   INDEXED {: x:^named :}
   x NULL-PTR = if false exit then
   x NAMED-HIDDEN @ if true exit then
   x NAMED-SINGLETON @ {: t:^entity-type :}
   t NULL-PTR <> if t ENTITY-TYPE-HIDDEN @ if true exit then then
   x NAMED-SET @ {: s:^entity-set :}
   s NULL-PTR = if false exit then
   s SERVED? 0= ;

: V2-FILTERABLE? ( ^property -- bool )
   PROPERTY-ANNOTATIONS @ s" http://www.sap.com/Protocols/SAPData"
   s" filterable" ANNOTATION if s" false" STR:= 0= exit then
   drop true ;

\ Whether F makes no property filterable or lists P's name.
: RESTRICTS? ( ^filter ^property -- bool )
   {: f:^ p:^ :}
   f FILTER-FILTERABLE @ 0= if true exit then
   f FILTER-PATHS @ begin dup NULL-PTR <> while
      dup PATH-TEXT @ p PROPERTY-NAME @ STR:= if drop true exit then
      PATH-NEXT @
   repeat drop false ;

public

: METADATA ( ^ARENA:arena str -- ^metadata )
   {: arena:^ src:str :}
   arena DOCS-NEW {: d:^ :}
   arena MODELS-NEW d DOC-MODEL !
   arena d DOC-ARENA !
   ARENA:NEW d DOC-NAMES !
   d DOC-NAMES @ ARENA:ALLOCATOR [type] XML:cursor ALLOC:NEW d DOC-CURSOR !
   [: d src PARSE ;] catch {: code:n :}
   d DOC-CURSOR @ XML:DISPOSE
   d DOC-NAMES @ ARENA:RELEASE
   code 0<> if code throw then
   d DOC-TYPES @ d DOC-MODEL @ METADATA-TYPES !
   d DOC-MODEL @ ;

: ENTITY-SET ( ^metadata str -- nullable<entity-set> )
   INDEXED-SET {: s:^ :}
   s NULL-PTR = if NULLABLE:NONE exit then
   s SERVED? 0= if NULLABLE:NONE exit then
   s NULLABLE:SOME ;

: EACH-ENTITY-SET ( ^metadata [ ^entity-set -- ] -- )
   {: m:^ q :}
   m METADATA-SETS @ begin dup NULL-PTR <> while
      dup SERVED? if dup q execute then ENTITY-SET-NEXT @
   repeat drop ;

: ENTITY-SET-NAME$ ( ^entity-set -- str ) ENTITY-SET-NAME @ ;

: ENTITY-TYPE ( ^entity-set -- ^entity-type ) ENTITY-SET-TYPE @ ;

\ The base type's properties first, then the type's own, in declared order.
: EACH-PROPERTY ( ^entity-type [ ^property -- ] -- )
   {: t:^ q :}
   t ENTITY-TYPE-MODEL @ {: m:^ :}
   t [: dup PROPERTY-NAME @ m swap HIDDEN?
      if drop else q execute then ;] PROPERTIES-OF ;

\ The key in declared order: the type's own, else its nearest base type's.
: EACH-KEY ( ^entity-type [ ^property -- ] -- )
   {: q :}
   [: KEY-REF-PROPERTY @ q execute ;] KEY-REFS-OF ;

: PROPERTY-NAME$ ( ^property -- str ) PROPERTY-NAME @ ;
: PROPERTY-TYPE$ ( ^property -- str ) PROPERTY-TYPE @ ;
: NULLABLE? ( ^property -- bool ) PROPERTY-NULLABLE @ ;

\ The base type's navigation properties first, then the type's own.
: EACH-NAVIGATION ( ^entity-type [ ^navigation -- ] -- )
   {: t:^ q :}
   t ENTITY-TYPE-MODEL @ {: m:^ :}
   t [: dup NAVIGATION-NAME @ m swap HIDDEN?
      if drop else q execute then ;] NAVIGATIONS-OF ;

: NAVIGATION-NAME$ ( ^navigation -- str ) NAVIGATION-NAME @ ;
: TO-MANY? ( ^navigation -- bool ) NAVIGATION-MANY @ ;

\ The set the navigation leads to from SET; none when SET's type lacks it or no
\ association set or binding names a set.
: NAVIGATION-TARGET ( ^entity-set ^navigation -- nullable<entity-set> )
   {: s:^ n:^ :}
   s ENTITY-SET-TYPE @ n HAS-NAVIGATION? 0= if NULLABLE:NONE exit then
   n NAVIGATION-OWNER @ TYPE-V2? if s n V2-TARGET exit then
   s n V4-TARGET ;

\ The text of the type's annotation of the term NAMESPACE, NAME, and whether
\ it has one.
: TYPE-ANNOTATION$ ( ^entity-type str str -- str bool )
   {: t:^ ns:str name:str :}
   t ENTITY-TYPE-ANNOTATIONS @ ns name ANNOTATION ;

: PROPERTY-ANNOTATION$ ( ^property str str -- str bool )
   {: p:^ ns:str name:str :}
   p PROPERTY-ANNOTATIONS @ ns name ANNOTATION ;

\ Whether a $filter on SET may name PROP, a property of SET's type: false when
\ PROP's sap:filterable or SET's FilterRestrictions says not, else true.
: FILTERABLE? ( ^entity-set ^property -- bool )
   {: s:^ p:^ :}
   p V2-FILTERABLE? 0= if false exit then
   s ENTITY-SET-FILTER @ {: f:^filter :}
   f NULL-PTR = if true exit then
   f p RESTRICTS? 0= ;

\ Hides the type of the set NAME and SETTLEs; false, changing nothing, when
\ the model has no such set.
: HIDE-SET ( ^metadata str -- bool )
   {: m:^ name:str :}
   m name INDEXED-SET {: s:^ :}
   s NULL-PTR = if false exit then
   true s ENTITY-SET-TYPE @ ENTITY-TYPE-HIDDEN !
   m SETTLE true ;

\ Hides every property and navigation NAME and SETTLEs; false, changing
\ nothing, when the model has none.
: HIDE-PROPERTY ( ^metadata str -- bool )
   {: m:^ name:str :}
   m name INDEXED {: x:^named :}
   x NULL-PTR = if false exit then
   x NAMED-PROPERTY-COUNT @ x NAMED-NAVIGATION-COUNT @ + 0= if
      false exit
   then
   true x NAMED-HIDDEN ! m SETTLE true ;

\ The first hidden name in TEXT, borrowed from it, and true; else an empty
\ str and false.
: HIDDEN-NAME$ ( ^metadata str -- str bool )
   {: m:^ text:str :}
   0 begin dup text STR:LEN < while
      dup text STR:DATA + c@ NAME-BYTE? 0= if 1+ else
         dup text swap RUN-END {: at:n end:n :}
         m text STR:DATA at + end at - >str QUERY-HIDDEN? if
            text STR:DATA at + end at - >str true exit
         then
         end
      then
   repeat drop text STR:DATA 0 >str false ;

;package

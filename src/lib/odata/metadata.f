\ metadata.f - METADATA and the model's accessors; part of package ODATA
\ (src/lib/odata.f), which states the contract. METADATA reads the document
\ in one pass of phases; an element it does not read is skipped whole,
\ counting the open elements, and reading resumes in the phase it left.
\ While METADATA reads, every name it looks up or checks for a duplicate is
\ found in one MAP of keys, each the name's kind as one byte and what makes
\ the name unique (a record's address, a namespace, the name). Where several
\ records answer one key (bindings, a target's terms), the map holds the first
\ two or a count. One walk of the base types, depth first
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
require src/lib/span.f
require src/lib/memory.f
require src/lib/arena.f
require src/lib/log.f
require src/lib/xml.f
require src/lib/map.f
require src/lib/adt/option.f
require src/lib/adt/nullable.f
require src/lib/sort.f

package ODATA
public

-9930 s" a $metadata document the library rejects: correct it where the odata log line says"
   ERROR:CODE E-METADATA
-9931 s" a response body the library rejects: correct it where the odata log line says"
   ERROR:CODE E-RESPONSE
-9932 s" a key value that does not fit its property's Edm type: give it in that type's form"
   ERROR:CODE E-KEY

private
-9930 constant E-FIRST
-9939 constant E-LAST

ENUM version v2 v4 ;ENUM

\ An annotation's namespace and name, and its value's text.
STRUCTURE annotation DERIVE addr
   FIELD namespace SPAN:span<u8>
   FIELD name SPAN:span<u8>
   FIELD text SPAN:span<u8>
   FIELD offset n
   FIELD next ^annotation
;STRUCTURE

\ A V2 Association End: many when its Multiplicity is `*`.
STRUCTURE association-end DERIVE addr
   FIELD role SPAN:span<u8>
   FIELD type SPAN:span<u8>
   FIELD many bool
   FIELD offset n
   FIELD next ^association-end
;STRUCTURE

STRUCTURE association DERIVE addr
   FIELD name SPAN:span<u8>
   FIELD namespace SPAN:span<u8>
   FIELD ends ^association-end
   FIELD offset n
   FIELD next ^association
;STRUCTURE

\ `names` maps a set or member name to its `named`; `targets` maps to a
\ binding's target (V4) or an association set (V2).
\ An entity type's `rank-end` is 0 until the walk of the base types leaves it.
\ A property's `complex-type` is the complex type its Type names, or names in
\ `Collection(...)` with `many` set; null for any other.
public
MUTUAL metadata entity-set entity-type property navigation key-ref
STRUCTURE metadata OPAQUE DERIVE addr
   FIELD version version
   FIELD sets ^entity-set
   FIELD names ^n
   FIELD names-cap n
   FIELD targets ^n
   FIELD targets-cap n
;STRUCTURE
STRUCTURE entity-set OPAQUE DERIVE addr
   FIELD name SPAN:span<u8>
   FIELD type-name SPAN:span<u8>
   FIELD type ^entity-type
   FIELD offset n
   FIELD next ^entity-set
;STRUCTURE
STRUCTURE entity-type OPAQUE DERIVE addr
   FIELD name SPAN:span<u8>
   FIELD namespace SPAN:span<u8>
   FIELD complex bool
   FIELD version version
   FIELD base-name SPAN:span<u8>
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
   FIELD name SPAN:span<u8>
   FIELD type SPAN:span<u8>
   FIELD nullable bool
   FIELD complex-type ^entity-type
   FIELD many bool
   FIELD owner ^entity-type
   FIELD annotations ^annotation
   FIELD offset n
   FIELD next ^property
;STRUCTURE
STRUCTURE navigation OPAQUE DERIVE addr
   FIELD name SPAN:span<u8>
   FIELD type SPAN:span<u8>
   FIELD relationship SPAN:span<u8>
   FIELD from-role SPAN:span<u8>
   FIELD to-role SPAN:span<u8>
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
   FIELD name SPAN:span<u8>
   FIELD property ^property
   FIELD offset n
   FIELD next ^key-ref
;STRUCTURE
;MUTUAL

private

STRUCTURE named DERIVE addr
   FIELD set ^entity-set
   FIELD properties ^^property
   FIELD property-count n
   FIELD navigations ^^navigation
   FIELD navigation-count n
;STRUCTURE

STRUCTURE set-end DERIVE addr
   FIELD role SPAN:span<u8>
   FIELD set-name SPAN:span<u8>
   FIELD end ^association-end
   FIELD set ^entity-set
   FIELD offset n
   FIELD next ^set-end
;STRUCTURE

STRUCTURE association-set DERIVE addr
   FIELD name SPAN:span<u8>
   FIELD association-name SPAN:span<u8>
   FIELD association ^association
   FIELD ends ^set-end
   FIELD offset n
   FIELD next ^association-set
;STRUCTURE

STRUCTURE binding DERIVE addr
   FIELD set ^entity-set
   FIELD path SPAN:span<u8>
   FIELD target-name SPAN:span<u8>
   FIELD target ^entity-set
   FIELD offset n
   FIELD next ^binding
;STRUCTURE

\ An `<Annotations Target>` block, attached at the end of the read.
STRUCTURE block DERIVE addr
   FIELD target SPAN:span<u8>
   FIELD annotations ^annotation
   FIELD next ^block
;STRUCTURE

\ The namespace an edmx:Include's or a Schema's Alias names.
STRUCTURE alias DERIVE addr
   FIELD namespace SPAN:span<u8>
;STRUCTURE

\ Keys of the names map. `member-name`: a container child but an entity set;
\ `visible`: a member's name alone, held by the last type the walk entered with it.
ENUM kind DERIVE tag
   type-name association-name property-name navigation-name end-role
   set-name member-name alias-name visible keyed term binding-path
   binding-second link-second singleton-name
;ENUM

\ The open CSDL element; `skip` is any element the table does not name.
ENUM phase DERIVE eq
   document edmx reference data-services schema type-body key property-body
   container set-body association-body set-pairs annotations skip
;ENUM

\ An element's namespace: EDMX of V2 or V4, CSDL of V2 (1.0, 1.1, 2.0) or
\ V4, or another, V3's CSDL among them.
ENUM space DERIVE eq edmx2 edmx4 edm2 edm4 other ;ENUM

\ One METADATA call's state. Scratch holds the element's namespace URI until
\ its first attribute is decoded; the names arena holds the map, keys, arrays.
STRUCTURE doc DERIVE addr
   FIELD model ^metadata
   FIELD arena ^ARENA:arena
   FIELD cursor ^XML:cursor
   FIELD scratch ^u8
   FIELD room n
   FIELD phase phase
   FIELD space space
   FIELD uri-len n
   FIELD depth n
   FIELD resume phase
   FIELD offset n
   FIELD end n
   FIELD namespace SPAN:span<u8>
   FIELD container SPAN:span<u8>
   FIELD container-namespace SPAN:span<u8>
   FIELD containers n
   FIELD type ^entity-type
   FIELD property ^property
   FIELD association ^association
   FIELD set ^entity-set
   FIELD association-set ^association-set
   FIELD block ^block
   FIELD set-last ^entity-set
   FIELD property-last ^property
   FIELD navigation-last ^navigation
   FIELD types ^entity-type
   FIELD type-count n
   FIELD ranked ^^entity-type
   FIELD associations ^association
   FIELD association-sets ^association-set
   FIELD bindings ^binding
   FIELD blocks ^block
   FIELD key-ref-last ^key-ref
   FIELD names ^ARENA:arena
   FIELD map ^n
   FIELD map-cap n
   FIELD key ^u8
   FIELD key-room n
   FIELD key-len n
   FIELD seconds ^^set-end
   FIELD second-count n
;STRUCTURE

ARENA DOCS doc
ARENA MODELS metadata
ARENA SETS entity-set
ARENA TYPES entity-type
ARENA PROPERTIES property
ARENA NAVIGATIONS navigation
ARENA ANNOTATIONS annotation
ARENA ENDS association-end
ARENA ASSOCIATIONS association
ARENA KEY-REFS key-ref
ARENA SET-ENDS set-end
ARENA ASSOCIATION-SETS association-set
ARENA BINDINGS binding
ARENA BLOCKS block
ARENA ALIASES alias
ARENA NAMEDS named

CAST: >PROPS ( ^u8 -- ^^property )
CAST: >NAVS ( ^u8 -- ^^navigation )
CAST: >SET-ENDS ( ^u8 -- ^^set-end )
CAST: >TYPES ( ^u8 -- ^^entity-type )
\ A record's address a map holds.
CAST: >RECORD ( n -- ^t )

: EDMX2$ ( -- ^u8 n ) s" http://schemas.microsoft.com/ado/2007/06/edmx" ;
: EDMX4$ ( -- ^u8 n ) s" http://docs.oasis-open.org/odata/ns/edmx" ;
: EDM4$ ( -- ^u8 n ) s" http://docs.oasis-open.org/odata/ns/edm" ;

: EDM2? ( ^u8 n -- bool )
   {: uri:^ len:n :}
   uri len s" http://schemas.microsoft.com/ado/2008/09/edm" STR=
   uri len s" http://schemas.microsoft.com/ado/2007/05/edm" STR= or
   uri len s" http://schemas.microsoft.com/ado/2006/04/edm" STR= or ;

: SPACE-OF ( ^u8 n -- space )
   {: uri:^ len:n :}
   uri len EDMX2$ STR= if SPACE-EDMX2 exit then
   uri len EDMX4$ STR= if SPACE-EDMX4 exit then
   uri len EDM4$ STR= if SPACE-EDM4 exit then
   uri len EDM2? if SPACE-EDM2 exit then
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
: QUALIFIER ( ^u8 n -- ^u8 n ^u8 n )
   {: text:^ len:n :}
   len begin dup 0 > while
      1- dup text + c@ [char] . = if
         {: dot:n :} text dot text dot + 1+ len dot - 1- exit
      then
   repeat drop
   text len text 0 ;

\ The index of the first slash, -1 for none.
: SLASH ( ^u8 n -- n )
   {: text:^ len:n :}
   len 0 ?do
      text i + c@ [char] / = if i unloop exit then
   loop
   -1 ;

\ The text before index I and the text after it.
: SPLIT ( ^u8 n n -- ^u8 n ^u8 n )
   {: text:^ len:n i:n :}
   text i text i + 1+ len i - 1- ;

: HAS-SLASH? ( ^u8 n -- bool ) SLASH 0 >= ;

\ Rejections: one log line, then E-METADATA. PLACE opens the line up to
\ "expected "; the caller adds what was expected; FOUND ends the line.

\ CUT+ writes a piece of a line taken from the document or a body, cut at
\ CUT-MAX bytes back to a character's first byte, with `...` after it.
32 constant CUT-MAX

: CUT+ ( ^u8 n -- )
   {: text:^ len:n :}
   len CUT-MAX <= if text len LOG:TEXT+ exit then
   CUT-MAX begin
      dup 0 > if dup text + c@ $C0 and $80 = else false then
   while 1- repeat
   text swap LOG:TEXT+ s" ..." LOG:TEXT+ ;

: FOUND+ ( ^u8 n -- ) s" ; found " LOG:TEXT+ CUT+ ;

: PLACE ( n ^u8 n ^u8 n -- )
   {: at:n el:^ el-len:n nm:^ nm-len:n :}
   LOG:ERROR s" odata" LOG:LINE
   s" $metadata byte " LOG:TEXT+ at LOG:INT+
   s" , " LOG:TEXT+ el el-len CUT+
   nm-len 0 > if s"  " LOG:TEXT+ nm nm-len CUT+ then
   s" : expected " LOG:TEXT+ ;

: FOUND ( ^u8 n -- )
   FOUND+ LOG:;LINE E-METADATA throw ;

: PLACE-HERE ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   XML:LOCAL$ {: el:^ el-len:n :}
   d DOC-OFFSET @ el el-len el 0 PLACE ;

: NONE$ ( -- ^u8 n ) s" none" ;
: SAME? ( ^t ^t -- bool ) NULL-PTR - swap NULL-PTR - = ;
: TEXT+ ( SPAN:span<u8> -- ) SPAN:$ CUT+ ;
: SPAN= ( SPAN:span<u8> ^u8 n -- bool )
   {: s:SPAN:span<u8> t:^ tl:n :}
   s SPAN:$ t tl STR= ;

\ Attributes. Each answer is a view of scratch, overwritten by the next.
: FIND-ATTR ( XML:reader ^u8 n -- XML:reader bool )
   {: name:^ len:n :}
   XML:ATTR-RESET
   begin XML:ATTR-NEXT while
      XML:ATTR-NAME$ name len STR= if true exit then
   repeat
   false ;

: ATTR-TEXT$ ( XML:reader ^doc -- XML:reader ^u8 n )
   {: d:^ :}
   d DOC-SCRATCH @ d DOC-ROOM @ XML:ATTR-TEXT {: got:n :}
   d DOC-SCRATCH @ got ;

: ATTR$ ( XML:reader ^doc ^u8 n -- XML:reader ^u8 n bool )
   {: d:^ name:^ len:n :}
   name len FIND-ATTR if d ATTR-TEXT$ true exit then
   d DOC-SCRATCH @ 0 false ;

: NEED$ ( XML:reader ^doc ^u8 n -- XML:reader ^u8 n )
   {: d:^ name:^ len:n :}
   d name len ATTR$ if exit then 2drop
   d PLACE-HERE
   s" attribute " LOG:TEXT+ name len LOG:TEXT+ NONE$ FOUND ;

: KEEP ( ^doc ^u8 n -- SPAN:span<u8> )
   {: d:^ src:^ len:n :}
   d DOC-ARENA @ len ARENA:ALLOC {: dst:^ got:n :}
   src dst len BYTE-COPY
   dst got SPAN:MAKE ;

: NEED ( XML:reader ^doc ^u8 n -- XML:reader SPAN:span<u8> )
   {: d:^ name:^ len:n :}
   d name len NEED$ {: v:^ vl:n :} d v vl KEEP ;

: OPTIONAL ( XML:reader ^doc ^u8 n -- XML:reader SPAN:span<u8> )
   {: d:^ name:^ len:n :}
   d name len ATTR$ drop {: v:^ vl:n :} d v vl KEEP ;

\ Whether the current start tag is NAME in the namespace SPACE.
: IS? ( XML:reader ^doc space ^u8 n -- XML:reader bool )
   {: d:^ sp:space name:^ len:n :}
   d DOC-SPACE @ sp SPACE-EQ 0= if false exit then
   XML:LOCAL$ name len STR= ;

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
: PARENT ( phase -- phase )
   MATCH phase
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

: KEY+ ( ^doc ^u8 n -- )
   {: d:^ src:^ len:n :}
   d len KEY-ROOM
   src d DOC-KEY @ d DOC-KEY-LEN @ + len BYTE-COPY
   len d DOC-KEY-LEN +! ;

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

: KEY$ ( ^doc -- ^u8 n ) dup DOC-KEY @ swap DOC-KEY-LEN @ ;

\ A name in a namespace.
: QUALIFIED-KEY ( ^doc kind ^u8 n ^u8 n -- )
   {: d:^ k:kind ns:^ nl:n name:^ len:n :}
   d k KEY-NEW d nl KEY-CELL+ d ns nl KEY+ d name len KEY+ ;

\ A name of the record at an address.
: OWNED-KEY ( ^doc kind ^t ^u8 n -- )
   {: d:^ k:kind at:^ name:^ len:n :}
   d k KEY-NEW d at KEY-AT+ d name len KEY+ ;

: NAME-KEY ( ^doc kind ^u8 n -- )
   {: d:^ k:kind name:^ len:n :}
   d k KEY-NEW d name len KEY+ ;

\ Two records' addresses.
: PAIR-KEY ( ^doc kind ^a ^b -- )
   {: d:^ k:kind a:^ b:^ :}
   d k KEY-NEW d a KEY-AT+ d b KEY-AT+ ;

\ The names map.
: MAP-OF ( ^doc -- ^n count ) dup DOC-MAP @ swap DOC-MAP-CAP @ >COUNT ;

: MAP-IN ( ^ARENA:arena n -- ^n )
   {: a:^ cap:n :}
   cap >COUNT MAP:CELL-COUNT COUNT>N cells {: bytes:n :}
   a bytes ARENA:ALLOC drop CELL-VIEW {: m:^ :}
   m cap >COUNT MAP:INIT
   m ;

: MAP-NEW ( ^doc n -- )
   {: d:^ cap:n :}
   d DOC-NAMES @ cap MAP-IN d DOC-MAP !
   cap d DOC-MAP-CAP ! ;

: MOVE-ENTRY ( ^u8 len n ^n count -- )
   {: key:^ len:len v:n m:^ cap:count :}
   v m cap key len MAP:SET ;

\ Doubles the map before an entry would fill half of it.
: MAP-GROW ( ^doc -- )
   {: d:^ :}
   d MAP-OF {: old:^ cap:count :}
   old MAP:COUNT@ COUNT>N 1+ 2 * cap COUNT>N <= if exit then
   d cap COUNT>N 2 * MAP-NEW
   d MAP-OF {: new:^ ncap:count :}
   old cap [: new ncap MOVE-ENTRY ;] MAP:EACH ;

: LOOKUP ( ^doc -- option<n> )
   {: d:^ :}
   d MAP-OF d KEY$ >LEN MAP:GET ;

: HELD? ( ^doc -- bool )
   {: d:^ :}
   d MAP-OF d KEY$ >LEN MAP:HAS? ;

\ The record KEY names in the map, null for none.
: MAP-RECORD ( ^n count ^u8 n -- ^t )
   >LEN MAP:GET MATCH option
      none OF 0 >RECORD ENDOF
      some OF >RECORD ENDOF
   ;MATCH ;

: FIND ( ^doc -- ^t ) dup MAP-OF rot KEY$ MAP-RECORD ;

\ Sets the key to V; a new key is copied into the names arena.
: REMEMBER ( ^doc n -- )
   {: d:^ v:n :}
   d HELD? if v d MAP-OF d KEY$ >LEN MAP:SET exit then
   d MAP-GROW
   d DOC-NAMES @ d DOC-KEY-LEN @ ARENA:ALLOC {: copy:^ len:n :}
   d DOC-KEY @ copy len BYTE-COPY
   v d MAP-OF copy len >LEN MAP:SET ;

: REMEMBER-AT ( ^doc ^t -- ) NULL-PTR - REMEMBER ;

\ Names.
: ALIAS ( ^doc ^u8 n -- ^u8 n )
   {: d:^ name:^ len:n :}
   d KIND-ALIAS-NAME name len NAME-KEY
   d FIND {: a:^alias :}
   a NULL-PTR = if name len exit then
   a ALIAS-NAMESPACE @ SPAN:$ ;

\ A qualified name, its qualifier an alias or a namespace, as a key of K.
: REFERENCE-KEY ( ^doc kind ^u8 n -- )
   {: d:^ k:kind name:^ len:n :}
   name len QUALIFIER {: q:^ ql:n tail:^ tl:n :}
   d q ql ALIAS {: ns:^ nl:n :}
   d k ns nl tail tl QUALIFIED-KEY ;

: FIND-TYPE ( ^doc ^u8 n -- ^entity-type )
   {: d:^ name:^ len:n :}
   d KIND-TYPE-NAME name len REFERENCE-KEY d FIND ;

: FIND-ASSOCIATION ( ^doc ^u8 n -- ^association )
   {: d:^ name:^ len:n :}
   d KIND-ASSOCIATION-NAME name len REFERENCE-KEY d FIND ;

: FIND-END ( ^doc ^association SPAN:span<u8> -- ^association-end )
   {: d:^ a:^ role:SPAN:span<u8> :}
   d KIND-END-ROLE a role SPAN:$ OWNED-KEY d FIND ;

: FIND-SET ( ^doc ^u8 n -- ^entity-set )
   {: d:^ name:^ len:n :}
   d KIND-SET-NAME name len NAME-KEY d FIND ;

: MEMBER? ( ^doc ^u8 n -- bool )
   {: d:^ name:^ len:n :}
   d KIND-MEMBER-NAME name len NAME-KEY d HELD? ;

: SINGLETON? ( ^doc ^u8 n -- bool )
   {: d:^ name:^ len:n :}
   d KIND-SINGLETON-NAME name len NAME-KEY d HELD? ;

\ The model's index of names.
: NAMES-OF ( ^metadata -- ^n count )
   dup METADATA-NAMES @ swap METADATA-NAMES-CAP @ >COUNT ;

\ The `named` of NAME, null for none.
: INDEXED ( ^metadata ^u8 n -- ^named ) rot NAMES-OF 2swap MAP-RECORD ;

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
: FIND-PROPERTY ( ^entity-type ^u8 n -- ^property )
   {: t:^ name:^ len:n :}
   t ENTITY-TYPE-MODEL @ name len INDEXED {: x:^named :}
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

: FIND-NAVIGATION ( ^entity-type ^u8 n -- ^navigation )
   {: t:^ name:^ len:n :}
   t ENTITY-TYPE-MODEL @ name len INDEXED {: x:^named :}
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
: XMLNS? ( ^u8 n -- bool )
   {: name:^ len:n :}
   name len s" xmlns" STR=
   name len s" xmlns:" STARTS-WITH? or
   name len s" xml:" STARTS-WITH? or ;

: PREFIXED? ( ^u8 n -- bool )
   {: name:^ len:n :}
   len 0 ?do name i + c@ [char] : = if true unloop exit then loop
   false ;

: ADD-ANNOTATION ( ^doc ^^annotation SPAN:span<u8> SPAN:span<u8> SPAN:span<u8> -- )
   {: d:^ list:^ ns:SPAN:span<u8> nm:SPAN:span<u8> tx:SPAN:span<u8> :}
   d DOC-ARENA @ ANNOTATIONS-NEW {: a:^ :}
   ns a ANNOTATION-NAMESPACE !
   nm a ANNOTATION-NAME !
   tx a ANNOTATION-TEXT !
   d DOC-OFFSET @ a ANNOTATION-OFFSET !
   list @ a ANNOTATION-NEXT !
   a list ! ;

: FOREIGN? ( ^u8 n -- bool )
   SPACE-OF {: sp:space :}
   sp SPACE-OTHER SPACE-EQ ;

\ The current attribute, when it is an annotation, onto LIST.
: ATTR-ANNOTATION ( XML:reader ^doc ^^annotation -- XML:reader )
   {: d:^ list:^ :}
   XML:ATTR-NAME$ {: name:^ len:n :}
   name len PREFIXED? 0= name len XMLNS? or if exit then
   d DOC-SCRATCH @ d DOC-ROOM @ XML:ATTR-URI {: ul:n :}
   d DOC-SCRATCH @ ul FOREIGN? 0= if exit then
   d d DOC-SCRATCH @ ul KEEP {: ns:SPAN:span<u8> :}
   XML:ATTR-LOCAL$ {: local:^ ll:n :}
   d local ll KEEP {: nm:SPAN:span<u8> :}
   d ATTR-TEXT$ {: v:^ vl:n :}
   d list ns nm d v vl KEEP ADD-ANNOTATION ;

: ATTR-ANNOTATIONS ( XML:reader ^doc ^^annotation -- XML:reader )
   {: d:^ list:^ :}
   d V2? 0= if exit then
   XML:ATTR-RESET
   begin XML:ATTR-NEXT while
      d list ATTR-ANNOTATION
   repeat ;

: VALUE-NAME? ( ^u8 n -- bool )
   {: name:^ len:n :}
   name len s" String" STR= name len s" Bool" STR= or
   name len s" Int" STR= or name len s" Decimal" STR= or
   name len s" Float" STR= or name len s" Date" STR= or
   name len s" DateTimeOffset" STR= or name len s" TimeOfDay" STR= or
   name len s" Duration" STR= or name len s" Guid" STR= or
   name len s" Binary" STR= or name len s" EnumMember" STR= or
   name len s" Path" STR= or name len s" PropertyPath" STR= or
   name len s" NavigationPropertyPath" STR= or
   name len s" AnnotationPath" STR= or
   name len s" ModelElementPath" STR= or ;

: FIND-VALUE ( XML:reader -- XML:reader bool )
   XML:ATTR-RESET
   begin XML:ATTR-NEXT while
      XML:ATTR-NAME$ VALUE-NAME? if true exit then
   repeat
   false ;

\ A V4 Annotation onto LIST; one with a Qualifier or no value attribute is
\ not kept.
: ANNOTATION-START ( XML:reader ^doc ^^annotation -- XML:reader )
   {: d:^ list:^ :}
   s" Qualifier" FIND-ATTR if d SKIP-START exit then
   d s" Term" NEED {: term:SPAN:span<u8> :}
   term SPAN:$ QUALIFIER {: q:^ ql:n tail:^ tl:n :}
   tl 0= if
      d PLACE-HERE
      s" Term to be a qualified name" LOG:TEXT+ term SPAN:$ FOUND
   then
   FIND-VALUE if
      d ATTR-TEXT$ {: v:^ vl:n :}
      d list q ql SPAN:MAKE tail tl SPAN:MAKE d v vl KEEP ADD-ANNOTATION
   then
   d SKIP-START ;

\ The document's root fixes the version.
: VERSION-OK? ( ^doc ^u8 n -- bool )
   {: d:^ v:^ vl:n :}
   d V2? if v vl s" 1.0" STR= exit then
   v vl s" 4.0" STR= v vl s" 4.01" STR= or ;

: EDMX-START ( XML:reader ^doc version -- XML:reader )
   {: d:^ ver:version :}
   ver d DOC-MODEL @ METADATA-VERSION !
   d s" Version" NEED$ {: v:^ vl:n :}
   d v vl VERSION-OK? 0= if
      d PLACE-HERE
      d V2? if s" Version 1.0 of OData V2" else s" Version 4.0 or 4.01 of OData V4" then
      LOG:TEXT+ v vl FOUND
   then
   d PHASE-EDMX ENTER ;

: URI$ ( ^doc -- ^u8 n ) dup DOC-SCRATCH @ swap DOC-URI-LEN @ ;

: IN-DOCUMENT ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d SPACE-EDMX2 s" Edmx" IS? if d VERSION-V2 EDMX-START exit then
   d SPACE-EDMX4 s" Edmx" IS? if d VERSION-V4 EDMX-START exit then
   d PLACE-HERE
   s" edmx:Edmx in the EDMX namespace of OData V2 or V4" LOG:TEXT+
   d URI$ FOUND ;

\ The element's Alias, when it has one, for the namespace NS.
: ADD-ALIAS ( XML:reader ^doc SPAN:span<u8> -- XML:reader )
   {: d:^ ns:SPAN:span<u8> :}
   d s" Alias" ATTR$ 0= if 2drop exit then
   {: nm:^ nl:n :}
   d DOC-NAMES @ ALIASES-NEW {: a:^ :}
   ns a ALIAS-NAMESPACE !
   d KIND-ALIAS-NAME nm nl NAME-KEY d a REMEMBER-AT ;

: INCLUDE-START ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d s" Namespace" NEED {: ns:SPAN:span<u8> :}
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
   d s" Namespace" NEED {: ns:SPAN:span<u8> :}
   ns d DOC-NAMESPACE !
   d ns ADD-ALIAS
   d PHASE-SCHEMA ENTER ;

: IN-DATA-SERVICES ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   XML:LOCAL$ s" Schema" STR= 0= if d SKIP-START exit then
   d d CSDL s" Schema" IS? if d SCHEMA-START exit then
   d PLACE-HERE
   d V2? if
      s" Schema in a CSDL namespace of OData V2 (1.0, 1.1 or 2.0)"
   else
      s" Schema in the CSDL namespace of OData V4"
   then
   LOG:TEXT+ d URI$ FOUND ;

\ Duplicate names are rejected where the second is read.
: NOT-NEW ( SPAN:span<u8> -- )
   s" a name not declared before" LOG:TEXT+ SPAN:$ FOUND ;

: REPEATED ( XML:reader ^doc SPAN:span<u8> -- XML:reader )
   {: d:^ name:SPAN:span<u8> :}
   d PLACE-HERE name NOT-NEW ;

: DECLARED-KEY ( ^doc kind SPAN:span<u8> -- )
   {: d:^ k:kind name:SPAN:span<u8> :}
   d k d DOC-NAMESPACE @ SPAN:$ name SPAN:$ QUALIFIED-KEY ;

: TYPE-TAKEN? ( ^doc SPAN:span<u8> -- bool )
   {: d:^ name:SPAN:span<u8> :}
   d KIND-TYPE-NAME name DECLARED-KEY d HELD? ;

: TYPE-START ( XML:reader ^doc bool -- XML:reader )
   {: d:^ complex:bool :}
   d s" Name" NEED {: name:SPAN:span<u8> :}
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
: MEMBER-TAKEN? ( ^doc ^entity-type SPAN:span<u8> -- bool )
   {: d:^ t:^ name:SPAN:span<u8> :}
   d KIND-PROPERTY-NAME t name SPAN:$ OWNED-KEY d HELD?
   d KIND-NAVIGATION-NAME t name SPAN:$ OWNED-KEY d HELD? or ;

: NULLABLE ( XML:reader ^doc -- XML:reader bool )
   {: d:^ :}
   d s" Nullable" ATTR$ 0= if 2drop true exit then
   {: v:^ vl:n :}
   v vl s" true" STR= if true exit then
   v vl s" false" STR= if false exit then
   d PLACE-HERE s" Nullable true or false" LOG:TEXT+ v vl FOUND ;

: ADD-PROPERTY ( ^doc ^property -- )
   {: d:^ p:^ :}
   d DOC-PROPERTY-LAST @ dup NULL-PTR = if
      drop p d DOC-TYPE @ ENTITY-TYPE-PROPERTIES !
   else PROPERTY-NEXT p swap ! then
   p d DOC-PROPERTY-LAST !
   d KIND-PROPERTY-NAME d DOC-TYPE @ p PROPERTY-NAME @ SPAN:$ OWNED-KEY
   d p REMEMBER-AT ;

: PROPERTY-START ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d s" Name" NEED {: name:SPAN:span<u8> :}
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
   d KIND-NAVIGATION-NAME d DOC-TYPE @ n NAVIGATION-NAME @ SPAN:$ OWNED-KEY
   d n REMEMBER-AT ;

: NAVIGATION-START ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d s" Name" NEED {: name:SPAN:span<u8> :}
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

: ASSOCIATION-TAKEN? ( ^doc SPAN:span<u8> -- bool )
   {: d:^ name:SPAN:span<u8> :}
   d KIND-ASSOCIATION-NAME name DECLARED-KEY d HELD? ;

: ASSOCIATION-START ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d s" Name" NEED {: name:SPAN:span<u8> :}
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
   d s" Multiplicity" NEED$ {: v:^ vl:n :}
   v vl s" *" STR= if true exit then
   v vl s" 1" STR= v vl s" 0..1" STR= or if false exit then
   d PLACE-HERE s" Multiplicity 1, 0..1 or *" LOG:TEXT+ v vl FOUND ;

: END-START ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d s" Role" NEED {: role:SPAN:span<u8> :}
   d DOC-ASSOCIATION @ {: a:^ :}
   d a role FIND-END NULL-PTR <> if d role REPEATED then
   d DOC-ARENA @ ENDS-NEW {: e:^ :}
   role e ASSOCIATION-END-ROLE !
   d s" Type" NEED e ASSOCIATION-END-TYPE !
   d MULTIPLICITY e ASSOCIATION-END-MANY !
   d DOC-OFFSET @ e ASSOCIATION-END-OFFSET !
   a ASSOCIATION-ENDS @ e ASSOCIATION-END-NEXT ! e a ASSOCIATION-ENDS !
   d KIND-END-ROLE a role SPAN:$ OWNED-KEY d e REMEMBER-AT
   d SKIP-START ;

: IN-ASSOCIATION ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d d CSDL s" End" IS? if d END-START exit then
   d SKIP-START ;

: CONTAINER-START ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d DOC-CONTAINERS @ 0<> if
      d PLACE-HERE s" one EntityContainer" LOG:TEXT+ s" a second" FOUND
   then
   1 d DOC-CONTAINERS !
   d s" Name" NEED d DOC-CONTAINER !
   d DOC-NAMESPACE @ d DOC-CONTAINER-NAMESPACE !
   d PHASE-CONTAINER ENTER ;

: NAME-TAKEN? ( ^doc SPAN:span<u8> -- bool )
   {: d:^ name:SPAN:span<u8> :}
   d name SPAN:$ FIND-SET NULL-PTR <>
   d name SPAN:$ MEMBER? or ;

: ADD-MEMBER ( ^doc SPAN:span<u8> -- )
   {: d:^ name:SPAN:span<u8> :}
   d KIND-MEMBER-NAME name SPAN:$ NAME-KEY d 0 REMEMBER ;

: MEMBER-START ( XML:reader ^doc bool -- XML:reader )
   {: d:^ singleton:bool :}
   d s" Name" NEED {: name:SPAN:span<u8> :}
   d name NAME-TAKEN? if d name REPEATED then
   d name ADD-MEMBER
   singleton if d KIND-SINGLETON-NAME name SPAN:$ NAME-KEY d 0 REMEMBER then
   d SKIP-START ;

: ADD-SET ( ^doc ^entity-set -- )
   {: d:^ s:^ :}
   d DOC-SET-LAST @ dup NULL-PTR = if
      drop s d DOC-MODEL @ METADATA-SETS !
   else ENTITY-SET-NEXT s swap ! then
   s d DOC-SET-LAST !
   d KIND-SET-NAME s ENTITY-SET-NAME @ SPAN:$ NAME-KEY d s REMEMBER-AT ;

: SET-START ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d s" Name" NEED {: name:SPAN:span<u8> :}
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
   d s" Name" NEED {: name:SPAN:span<u8> :}
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
   b BINDING-SET @ b BINDING-PATH @ SPAN:$ {: s:^ p:^ pl:n :}
   d KIND-BINDING-PATH s p pl OWNED-KEY d FIND {: old:^binding :}
   old NULL-PTR <> if
      d KIND-BINDING-SECOND s p pl OWNED-KEY d old REMEMBER-AT
   then
   d KIND-BINDING-PATH s p pl OWNED-KEY d b REMEMBER-AT ;

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

: IN-SET ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d SPACE-EDM4 s" NavigationPropertyBinding" IS? if d BINDING-START exit then
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
      d d DOC-BLOCK @ BLOCK-ANNOTATIONS ANNOTATION-START exit
   then
   d SKIP-START ;

: IN-SCHEMA ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d d CSDL s" EntityType" IS? if d false TYPE-START exit then
   d d CSDL s" ComplexType" IS? if d true TYPE-START exit then
   d d CSDL s" Association" IS? if d ASSOCIATION-START exit then
   d d CSDL s" EntityContainer" IS? if d CONTAINER-START exit then
   d SPACE-EDM4 s" Annotations" IS? if d BLOCK-START exit then
   d SKIP-START ;

\ A start tag: its namespace is classified, then the open phase takes it.
: CLASSIFY ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   d DOC-SCRATCH @ d DOC-ROOM @ XML:URI {: ul:n :}
   ul d DOC-URI-LEN !
   d DOC-SCRATCH @ ul SPACE-OF d DOC-SPACE ! ;

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
      skip OF d DEEPER ENDOF
   ;MATCH ;

: ON-END ( ^doc -- )
   {: d:^ :}
   d DOC-PHASE @ PHASE-SKIP PHASE-EQ if d SHALLOWER exit then
   d d DOC-PHASE @ PARENT ENTER ;

\ One event; false at the end of the document.
: STEP ( XML:reader ^doc -- XML:reader bool )
   {: d:^ :}
   XML:NEXT {: k:XML:kind :}
   XML:RAW {: off len :}
   off OFF>N d DOC-OFFSET !
   off OFF>N len LEN>N + d DOC-END !
   k MATCH XML:kind
      start OF d ON-START true ENDOF
      end OF d ON-END true ENDOF
      text OF true ENDOF
      comment OF true ENDOF
      pi OF true ENDOF
      cdata OF true ENDOF
      eof OF false ENDOF
   ;MATCH ;

: EVENTS ( XML:reader ^doc -- XML:reader )
   {: d:^ :}
   begin d STEP while repeat ;

\ RESOLVE: every name the document gives resolves to its record, or the
\ document is rejected at the element that holds the name.
: BASE ( ^doc ^entity-type -- )
   {: d:^ t:^ :}
   t ENTITY-TYPE-BASE-NAME @ SPAN:$ {: bn:^ bl:n :}
   bl 0= if exit then
   d bn bl FIND-TYPE dup NULL-PTR = if
      drop t ENTITY-TYPE-OFFSET @ s" EntityType" t ENTITY-TYPE-NAME @ SPAN:$ PLACE
      s" BaseType to name a type the document declares" LOG:TEXT+ bn bl FOUND
   then
   t ENTITY-TYPE-BASE ! ;

: REDECLARED ( n SPAN:span<u8> ^u8 n -- )
   {: at:n name:SPAN:span<u8> el:^ el-len:n :}
   at el el-len el 0 PLACE name NOT-NEW ;

\ The open type holding NAME, null for none.
: HOLDER ( ^doc ^u8 n -- ^entity-type )
   {: d:^ name:^ len:n :}
   d KIND-VISIBLE name len NAME-KEY d FIND {: h:^entity-type :}
   h NULL-PTR = if h exit then
   h ENTITY-TYPE-RANK-END @ 0= if h exit then
   NULL-PTR ;

: HOLD ( ^doc ^entity-type SPAN:span<u8> n ^u8 n -- )
   {: d:^ t:^ name:SPAN:span<u8> at:n el:^ el-len:n :}
   d name SPAN:$ HOLDER NULL-PTR <> if at name el el-len REDECLARED then
   d KIND-VISIBLE name SPAN:$ NAME-KEY d t REMEMBER-AT ;

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

\ A property in T's Key is marked under `keyed`.
: KEYED? ( ^doc ^entity-type ^property -- bool )
   {: d:^ t:^ p:^ :}
   d KIND-KEYED t p PAIR-KEY d HELD? ;

: KEY-REF ( ^doc ^entity-type ^key-ref -- )
   {: d:^ t:^ k:^ :}
   k KEY-REF-NAME @ SPAN:$ {: name:^ len:n :}
   d name len HOLDER {: h:^entity-type :}
   h NULL-PTR = if NULL-PTR else
      d KIND-PROPERTY-NAME h name len OWNED-KEY d FIND
   then {: p:^property :}
   p NULL-PTR = p NULL-PTR <> if d t p KEYED? or then if
      k KEY-REF-OFFSET @ s" PropertyRef" k KEY-REF-NAME @ SPAN:$ PLACE
      s" Name to name a property of " LOG:TEXT+ t ENTITY-TYPE-NAME @ TEXT+
      s"  once" LOG:TEXT+ k KEY-REF-NAME @ SPAN:$ FOUND
   then
   d KIND-KEYED t p PAIR-KEY d 0 REMEMBER
   p k KEY-REF-PROPERTY ! ;

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
   t ENTITY-TYPE-OFFSET @ s" EntityType" t ENTITY-TYPE-NAME @ SPAN:$ PLACE
   s" BaseType to lead to a type with none" LOG:TEXT+ s" a cycle" FOUND ;

\ The model's index of names, made once the walk has ranked the types.
: OWN-MEMBERS ( ^entity-type -- n )
   {: t:^ :}
   0 t ENTITY-TYPE-PROPERTIES @ begin dup NULL-PTR <> while
      swap 1+ swap PROPERTY-NEXT @
   repeat drop
   t ENTITY-TYPE-NAVIGATIONS @ begin dup NULL-PTR <> while
      swap 1+ swap NAVIGATION-NEXT @
   repeat drop ;

\ The model's sets and every type's members.
: NAME-COUNT ( ^doc -- n )
   {: d:^ :}
   0 d DOC-MODEL @ METADATA-SETS @ begin dup NULL-PTR <> while
      swap 1+ swap ENTITY-SET-NEXT @
   repeat drop
   d DOC-TYPES @ begin dup NULL-PTR <> while
      dup OWN-MEMBERS rot + swap ENTITY-TYPE-NEXT @
   repeat drop ;

\ NAME's `named`, made in the caller's arena under the name's own bytes
\ when the name is new.
: INDEX-ENTRY ( ^doc SPAN:span<u8> -- ^named )
   {: d:^ name:SPAN:span<u8> :}
   d DOC-MODEL @ name SPAN:$ INDEXED {: x:^named :}
   x NULL-PTR <> if x exit then
   d DOC-ARENA @ NAMEDS-NEW {: y:^ :}
   y NULL-PTR - d DOC-MODEL @ NAMES-OF name SPAN:$ >LEN MAP:SET
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
   d DOC-MODEL @ p PROPERTY-NAME @ SPAN:$ INDEXED {: x:^named :}
   x NAMED-PROPERTIES @ NULL-PTR = if
      d DOC-ARENA @ x NAMED-PROPERTY-COUNT @ cells ARENA:ALLOC drop >PROPS
      x NAMED-PROPERTIES !
      0 x NAMED-PROPERTY-COUNT !
   then
   p x NAMED-PROPERTIES @ x NAMED-PROPERTY-COUNT @ cells + !
   1 x NAMED-PROPERTY-COUNT +! ;

: FILE-NAVIGATION ( ^doc ^navigation -- )
   {: d:^ n:^ :}
   d DOC-MODEL @ n NAVIGATION-NAME @ SPAN:$ INDEXED {: x:^named :}
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
   d NAME-COUNT 2 * 1 max {: cap:n :}
   d DOC-ARENA @ cap MAP-IN d DOC-MODEL @ METADATA-NAMES !
   cap d DOC-MODEL @ METADATA-NAMES-CAP !
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

: SET-TYPE ( ^doc ^entity-set -- )
   {: d:^ s:^ :}
   d s ENTITY-SET-TYPE-NAME @ SPAN:$ FIND-TYPE {: t:^ :}
   t NULL-PTR = t NULL-PTR <> if t ENTITY-TYPE-COMPLEX @ or then if
      s ENTITY-SET-OFFSET @ s" EntitySet" s ENTITY-SET-NAME @ SPAN:$ PLACE
      s" EntityType to name an entity type the document declares" LOG:TEXT+
      s ENTITY-SET-TYPE-NAME @ SPAN:$ FOUND
   then
   t ENTITY-TYPE-KEY-HOLDER @ ENTITY-TYPE-KEY @ NULL-PTR = if
      s ENTITY-SET-OFFSET @ s" EntitySet" s ENTITY-SET-NAME @ SPAN:$ PLACE
      s" EntityType " LOG:TEXT+ s ENTITY-SET-TYPE-NAME @ TEXT+
      s"  to have a Key" LOG:TEXT+ NONE$ FOUND
   then
   t s ENTITY-SET-TYPE ! ;

: RESOLVE-SET-TYPES ( ^doc -- )
   {: d:^ :}
   d DOC-MODEL @ METADATA-SETS @ begin dup NULL-PTR <> while
      d over SET-TYPE ENTITY-SET-NEXT @
   repeat drop ;

: END-RESOLVES ( ^doc ^association-end -- )
   {: d:^ e:^ :}
   d e ASSOCIATION-END-TYPE @ SPAN:$ FIND-TYPE NULL-PTR <> if exit then
   e ASSOCIATION-END-OFFSET @ s" End" e ASSOCIATION-END-ROLE @ SPAN:$ PLACE
   s" Type to name a type the document declares" LOG:TEXT+
   e ASSOCIATION-END-TYPE @ SPAN:$ FOUND ;

: ENDS-FOUND ( n -- )
   s" two End elements, one for each role" LOG:TEXT+
   s" ; found " LOG:TEXT+ LOG:INT+ LOG:;LINE E-METADATA throw ;

: ASSOCIATION-RESOLVES ( ^doc ^association -- )
   {: d:^ a:^ :}
   0 a ASSOCIATION-ENDS @ begin dup NULL-PTR <> while
      d over END-RESOLVES ASSOCIATION-END-NEXT @ swap 1+ swap
   repeat drop {: count:n :}
   count 2 = if exit then
   a ASSOCIATION-OFFSET @ s" Association" a ASSOCIATION-NAME @ SPAN:$ PLACE
   count ENDS-FOUND ;

: RESOLVE-ENDS ( ^doc -- )
   {: d:^ :}
   d DOC-ASSOCIATIONS @ begin dup NULL-PTR <> while
      d over ASSOCIATION-RESOLVES ASSOCIATION-NEXT @
   repeat drop ;

: NAVIGATION-PLACE ( ^navigation -- )
   {: n:^ :}
   n NAVIGATION-OFFSET @ s" NavigationProperty" n NAVIGATION-NAME @ SPAN:$ PLACE ;

: ROLE ( ^doc ^navigation ^association SPAN:span<u8> -- ^association-end )
   {: d:^ n:^ a:^ role:SPAN:span<u8> :}
   d a role FIND-END dup NULL-PTR <> if exit then drop
   n NAVIGATION-PLACE
   s" a role of Association " LOG:TEXT+ a ASSOCIATION-NAME @ TEXT+
   role SPAN:$ FOUND ;

: V2-NAVIGATION ( ^doc ^navigation -- )
   {: d:^ n:^ :}
   d n NAVIGATION-RELATIONSHIP @ SPAN:$ FIND-ASSOCIATION {: a:^ :}
   a NULL-PTR = if
      n NAVIGATION-PLACE
      s" Relationship to name an Association the document declares" LOG:TEXT+
      n NAVIGATION-RELATIONSHIP @ SPAN:$ FOUND
   then
   a n NAVIGATION-ASSOCIATION !
   d n a n NAVIGATION-FROM-ROLE @ ROLE n NAVIGATION-FROM !
   d n a n NAVIGATION-TO-ROLE @ ROLE {: e:^ :}
   e n NAVIGATION-TO !
   d e ASSOCIATION-END-TYPE @ SPAN:$ FIND-TYPE n NAVIGATION-TARGET-TYPE !
   e ASSOCIATION-END-MANY @ n NAVIGATION-MANY ! ;

\ `Collection(X)` is to-many X.
: COLLECTION ( ^u8 n -- ^u8 n bool )
   {: text:^ len:n :}
   text len s" Collection(" STARTS-WITH?
   len 0 > if text len + 1- c@ [char] ) = and then
   if s" Collection(" nip {: open:n :} text open + len open - 1- true exit then
   text len false ;

: V4-NAVIGATION ( ^doc ^navigation -- )
   {: d:^ n:^ :}
   n NAVIGATION-TYPE @ SPAN:$ COLLECTION n NAVIGATION-MANY !
   {: tn:^ tl:n :}
   d tn tl FIND-TYPE {: t:^ :}
   t NULL-PTR = t NULL-PTR <> if t ENTITY-TYPE-COMPLEX @ or then if
      n NAVIGATION-PLACE
      s" Type to name an entity type the document declares" LOG:TEXT+
      n NAVIGATION-TYPE @ SPAN:$ FOUND
   then
   t n NAVIGATION-TARGET-TYPE ! ;

: VALUE-TYPE ( ^doc ^property -- )
   {: d:^ p:^ :}
   p PROPERTY-TYPE @ SPAN:$ COLLECTION {: tn:^ tl:n many:bool :}
   d tn tl FIND-TYPE {: t:^ :}
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

: SET-END ( ^set-end ^doc ^association -- )
   {: e:^ d:^ a:^ :}
   d a e SET-END-ROLE @ FIND-END {: ae:^ :}
   ae NULL-PTR = if
      e SET-END-OFFSET @ s" End" e SET-END-ROLE @ SPAN:$ PLACE
      s" Role to name a role of Association " LOG:TEXT+ a ASSOCIATION-NAME @ TEXT+
      e SET-END-ROLE @ SPAN:$ FOUND
   then
   ae e SET-END-END !
   d e SET-END-SET-NAME @ SPAN:$ FIND-SET {: s:^ :}
   s NULL-PTR = if
      e SET-END-OFFSET @ s" End" e SET-END-ROLE @ SPAN:$ PLACE
      s" EntitySet to name an entity set the document declares" LOG:TEXT+
      e SET-END-SET-NAME @ SPAN:$ FOUND
   then
   s e SET-END-SET ! ;

: TARGETS-OF ( ^metadata -- ^n count )
   dup METADATA-TARGETS @ swap METADATA-TARGETS-CAP @ >COUNT ;

\ A targets key: S's address, then R's, with no kind.
: TARGET-KEY ( ^doc ^entity-set ^t -- )
   {: d:^ s:^ r:^ :}
   0 d DOC-KEY-LEN ! d s KEY-AT+ d r KEY-AT+ ;

: TARGET-HELD? ( ^doc -- bool )
   {: d:^ :}
   d DOC-MODEL @ TARGETS-OF d KEY$ >LEN MAP:HAS? ;

: TARGET-FIND ( ^doc -- ^t ) dup DOC-MODEL @ TARGETS-OF rot KEY$ MAP-RECORD ;

\ Sets the key to V, the key copied into the caller's arena.
: ADD-TARGET ( ^doc n -- )
   {: d:^ v:n :}
   d DOC-ARENA @ d DOC-KEY-LEN @ ARENA:ALLOC {: copy:^ len:n :}
   d DOC-KEY @ copy len BYTE-COPY
   v d DOC-MODEL @ TARGETS-OF copy len >LEN MAP:SET ;

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
   count 2 * 1 max {: cap:n :}
   d DOC-ARENA @ cap MAP-IN d DOC-MODEL @ METADATA-TARGETS !
   cap d DOC-MODEL @ METADATA-TARGETS-CAP !
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

: ASSOCIATION-SET-PLACE ( ^association-set -- )
   {: x:^ :}
   x ASSOCIATION-SET-OFFSET @ s" AssociationSet" x ASSOCIATION-SET-NAME @ SPAN:$ PLACE ;

: ROLE-TWICE ( SPAN:span<u8> -- )
   s" two End elements, one for each role" LOG:TEXT+
   s" ; found role " LOG:TEXT+ TEXT+ s"  twice" LOG:TEXT+
   LOG:;LINE E-METADATA throw ;

: ASSOCIATION-SET ( ^doc ^association-set -- )
   {: d:^ x:^ :}
   d x ASSOCIATION-SET-ASSOCIATION-NAME @ SPAN:$ FIND-ASSOCIATION {: a:^ :}
   a NULL-PTR = if
      x ASSOCIATION-SET-PLACE
      s" Association to name an Association the document declares" LOG:TEXT+
      x ASSOCIATION-SET-ASSOCIATION-NAME @ SPAN:$ FOUND
   then
   a x ASSOCIATION-SET-ASSOCIATION !
   0 x ASSOCIATION-SET-ENDS @ begin dup NULL-PTR <> while
      SET-END-NEXT @ swap 1+ swap
   repeat drop {: count:n :}
   count 2 <> if x ASSOCIATION-SET-PLACE count ENDS-FOUND then
   x ASSOCIATION-SET-ENDS @ {: e:^ :}
   e SET-END-NEXT @ {: f:^ :}
   e d a SET-END f d a SET-END
   e SET-END-END @ f SET-END-END @ SAME? if
      x ASSOCIATION-SET-PLACE e SET-END-ROLE @ ROLE-TWICE
   then
   d x e ADD-LINK d x f ADD-LINK ;

: RESOLVE-ASSOCIATION-SETS ( ^doc -- )
   {: d:^ :}
   d DOC-ASSOCIATION-SETS @ begin dup NULL-PTR <> while
      d over ASSOCIATION-SET ASSOCIATION-SET-NEXT @
   repeat drop ;

\ A binding Target is a set or singleton, alone or after the container's name;
\ a containment path (a slash and navigations after it) leads to no set.
: CONTAINER-PATH? ( ^doc ^u8 n -- bool )
   {: d:^ p:^ pl:n :}
   p pl d DOC-CONTAINER @ SPAN:$ STR= if true exit then
   p pl QUALIFIER {: q:^ ql:n tail:^ tl:n :}
   tail tl d DOC-CONTAINER @ SPAN:$ STR= 0= if false exit then
   d q ql ALIAS d DOC-CONTAINER-NAMESPACE @ SPAN:$ STR= ;

: BINDING-PLACE ( ^binding -- )
   {: b:^ :}
   b BINDING-OFFSET @ s" NavigationPropertyBinding" b BINDING-PATH @ SPAN:$ PLACE ;

: TARGET-RESOLVES ( ^doc ^binding -- )
   {: d:^ b:^ :}
   b BINDING-TARGET-NAME @ SPAN:$ {: text:^ len:n :}
   text len SLASH {: i:n :}
   i 0 < if
      text len
   else
      text len i SPLIT {: head:^ hl:n tail:^ tl:n :}
      d head hl CONTAINER-PATH? if tail tl else text len then
   then {: name:^ nl:n :}
   name nl SLASH {: j:n :}
   j 0 >= if
      name nl j SPLIT 2drop {: owner:^ ol:n :}
      d owner ol FIND-SET NULL-PTR = d owner ol SINGLETON? 0= and if
         b BINDING-PLACE
         s" Target to name an entity set or singleton of " LOG:TEXT+
         d DOC-CONTAINER @ TEXT+ b BINDING-TARGET-NAME @ SPAN:$ FOUND
      then
      NULL-PTR b BINDING-TARGET ! exit
   then
   d name nl FIND-SET dup b BINDING-TARGET !
   NULL-PTR <> if exit then
   d name nl SINGLETON? if exit then
   b BINDING-PLACE
   s" Target to name an entity set or singleton the document declares" LOG:TEXT+
   b BINDING-TARGET-NAME @ SPAN:$ FOUND ;

\ The navigation a Path without a slash names; null for a Path with one.
: PATH-RESOLVES ( ^doc ^binding -- ^navigation )
   {: d:^ b:^ :}
   b BINDING-PATH @ SPAN:$ HAS-SLASH? if NULL-PTR exit then
   b BINDING-SET @ ENTITY-SET-TYPE @
   b BINDING-PATH @ SPAN:$ FIND-NAVIGATION dup NULL-PTR <> if exit then
   drop b BINDING-PLACE
   s" Path to name a navigation property of " LOG:TEXT+
   b BINDING-SET @ ENTITY-SET-TYPE @ ENTITY-TYPE-NAME @ TEXT+
   b BINDING-PATH @ SPAN:$ FOUND ;

\ V4: a second binding of N on the set is rejected; the target is kept.
: ADD-BINDING-TARGET ( ^doc ^binding ^navigation -- )
   {: d:^ b:^ n:^ :}
   n NULL-PTR = d V2? or if exit then
   b BINDING-SET @ {: s:^ :}
   d KIND-BINDING-SECOND s b BINDING-PATH @ SPAN:$ OWNED-KEY
   d FIND {: c:^binding :}
   c NULL-PTR <> if
      c BINDING-PLACE
      s" one NavigationPropertyBinding per Path" LOG:TEXT+ s" a second" FOUND
   then
   d s n TARGET-KEY
   d b BINDING-TARGET @ NULL-PTR - ADD-TARGET ;

: RESOLVE-BINDINGS ( ^doc -- )
   {: d:^ :}
   d DOC-BINDINGS @ begin dup NULL-PTR <> while
      {: b:^ :}
      d b TARGET-RESOLVES
      d b d b PATH-RESOLVES ADD-BINDING-TARGET
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
   y ASSOCIATION-SET-PLACE
   s" one AssociationSet for EntitySet " LOG:TEXT+ s ENTITY-SET-NAME @ TEXT+
   s"  and NavigationProperty " LOG:TEXT+ n NAVIGATION-NAME @ TEXT+
   x ASSOCIATION-SET-NAME @ SPAN:$ FOUND ;

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

\ A block whose Target names a type or a property it declares attaches; any
\ other is read past: SAP's services target navigation properties.
: ATTACH ( ^annotation ^^annotation -- )
   {: a:^ list:^ :}
   a begin dup NULL-PTR <> while
      dup ANNOTATION-NEXT @ swap
      list @ over ANNOTATION-NEXT ! list !
   repeat drop ;

: BLOCK ( ^doc ^block -- )
   {: d:^ b:^ :}
   b BLOCK-TARGET @ SPAN:$ {: text:^ len:n :}
   text len SLASH {: i:n :}
   i 0 < if
      d text len FIND-TYPE dup NULL-PTR = if drop exit then
      b BLOCK-ANNOTATIONS @ swap ENTITY-TYPE-ANNOTATIONS ATTACH exit
   then
   text len i SPLIT {: head:^ hl:n tail:^ tl:n :}
   d head hl FIND-TYPE {: t:^ :}
   t NULL-PTR = if exit then
   d KIND-PROPERTY-NAME t tail tl OWNED-KEY d FIND {: p:^property :}
   p NULL-PTR = if exit then
   b BLOCK-ANNOTATIONS @ p PROPERTY-ANNOTATIONS ATTACH ;

: RESOLVE-BLOCKS ( ^doc -- )
   {: d:^ :}
   d DOC-BLOCKS @ begin dup NULL-PTR <> while
      d over BLOCK BLOCK-NEXT @
   repeat drop ;

\ Terms: an alias becomes its namespace; a second annotation of one term on
\ one target is rejected.
: TERM ( ^doc ^annotation -- )
   {: d:^ a:^ :}
   d a ANNOTATION-NAMESPACE @ SPAN:$ ALIAS SPAN:MAKE a ANNOTATION-NAMESPACE ! ;

\ A's term on the target whose list starts at LIST; kept as a count.
: TERM-KEY ( ^doc ^annotation ^annotation -- )
   {: d:^ list:^ a:^ :}
   a ANNOTATION-NAMESPACE @ SPAN:$ {: ns:^ nl:n :}
   d KIND-TERM KEY-NEW d list KEY-AT+ d nl KEY-CELL+ d ns nl KEY+
   d a ANNOTATION-NAME @ SPAN:$ KEY+ ;

: TERM-COUNT ( ^doc ^annotation ^annotation -- n )
   {: d:^ list:^ a:^ :}
   d list a TERM-KEY d LOOKUP MATCH option
      none OF 0 ENDOF
      some OF ENDOF
   ;MATCH ;

: COUNT-TERM ( ^doc ^annotation ^annotation -- )
   {: d:^ list:^ a:^ :}
   d list a TERM-COUNT 1+ {: c:n :}
   d list a TERM-KEY d c REMEMBER ;

\ The first annotation in LIST whose term appears again is rejected.
: UNIQUE ( ^doc ^annotation ^annotation -- )
   {: d:^ list:^ a:^ :}
   d list a TERM-COUNT 2 < if exit then
   a ANNOTATION-OFFSET @ s" Annotation" a ANNOTATION-NAME @ SPAN:$ PLACE
   s" one annotation of " LOG:TEXT+ a ANNOTATION-NAMESPACE @ TEXT+
   s"  per target" LOG:TEXT+ s" a second" FOUND ;

: TERMS ( ^doc ^annotation -- )
   {: d:^ list:^ :}
   list begin dup NULL-PTR <> while d over TERM ANNOTATION-NEXT @ repeat drop
   list begin dup NULL-PTR <> while
      dup d list rot COUNT-TERM ANNOTATION-NEXT @
   repeat drop
   list begin dup NULL-PTR <> while
      dup d list rot UNIQUE ANNOTATION-NEXT @
   repeat drop ;

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

: RESOLVE ( ^doc -- )
   {: d:^ :}
   d RESOLVE-BASES
   d INDEX-NAMES
   d RESOLVE-SET-TYPES
   d RESOLVE-ENDS
   d RESOLVE-NAVIGATIONS
   d RESOLVE-VALUE-TYPES
   d TARGETS-NEW
   d RESOLVE-ASSOCIATION-SETS
   d RESOLVE-BINDINGS
   d RESOLVE-SECONDS
   d RESOLVE-BLOCKS
   d RESOLVE-TERMS ;

: PARSE ( ^doc ^u8 n -- )
   {: d:^ src:^ len:n :}
   ARENA:NEW d DOC-NAMES !
   d 16 MAP-NEW
   d DOC-CURSOR @ src len XML:INIT
   d EVENTS XML:CLOSE
   d RESOLVE ;

\ A read the XML reader or the system stopped: one line with the end of the
\ last event read and the code, which METADATA throws on.
: STOPPED ( ^doc n -- )
   {: d:^ code:n :}
   code E-METADATA = if exit then
   LOG:ERROR s" odata" LOG:LINE
   s" $metadata byte " LOG:TEXT+ d DOC-END @ LOG:INT+
   s" , the end of the last event read: the read stopped with " LOG:TEXT+
   code LOG:CODE+ LOG:;LINE ;

: SCRATCH ( ^doc n -- SPAN:span<u8> )
   {: d:^ len:n :}
   len XML:CURSOR-BYTES + MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: buf:SPAN:span<u8> :}
   buf SPAN:$ drop {: base:^ :}
   base XML:CURSOR-BYTES XML:EMPTY-CURSOR d DOC-CURSOR !
   base XML:CURSOR-BYTES + d DOC-SCRATCH !
   len d DOC-ROOM !
   buf ;

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
   m TARGETS-OF key 2 cells >LEN MAP:GET ;

\ The targets entry under the addresses A and B, as distances from null.
: TARGET-OF ( ^metadata n n -- option<n> )
   2 cells MEM:BYTES-ALLOC-LEN [: TARGET-IN ;] MEM:WITH-BYTES ;

\ The set at X's End for the role E plays.
: END-SET ( ^association-set ^association-end -- ^entity-set )
   {: x:^ e:^ :}
   x ASSOCIATION-SET-ENDS @ begin dup NULL-PTR <> while
      dup SET-END-END @ e SAME? if SET-END-SET @ exit then
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

: ANNOTATION ( ^annotation ^u8 n ^u8 n -- ^u8 n bool )
   {: a:^ ns:^ nl:n name:^ len:n :}
   a begin dup NULL-PTR <> while
      dup ANNOTATION-NAME @ name len SPAN=
      over ANNOTATION-NAMESPACE @ ns nl SPAN= and if
         ANNOTATION-TEXT @ SPAN:$ true exit
      then
      ANNOTATION-NEXT @
   repeat drop name 0 false ;

public

: METADATA ( ^ARENA:arena ^u8 n -- ^metadata )
   {: arena:^ src:^ len:n :}
   arena DOCS-NEW {: d:^ :}
   arena MODELS-NEW d DOC-MODEL !
   arena d DOC-ARENA !
   d len SCRATCH {: buf:SPAN:span<u8> :}
   [: d src len PARSE ;] catch {: code:n :}
   d DOC-CURSOR @ XML:DISPOSE
   buf MEM:FREE-SPAN
   d DOC-NAMES @ ARENA:RELEASE
   code 0<> if d code STOPPED code throw then
   d DOC-MODEL @ ;

: ENTITY-SET ( ^metadata ^u8 n -- nullable<entity-set> )
   INDEXED {: x:^named :}
   x NULL-PTR = if NULLABLE:NONE exit then
   x NAMED-SET @ NULLABLE:SOME ;

: EACH-ENTITY-SET ( ^metadata [ ^entity-set -- ] -- )
   {: m:^ q :}
   m METADATA-SETS @ begin dup NULL-PTR <> while
      dup q execute ENTITY-SET-NEXT @
   repeat drop ;

: ENTITY-SET-NAME$ ( ^entity-set -- ^u8 n ) ENTITY-SET-NAME @ SPAN:$ ;

: ENTITY-TYPE ( ^entity-set -- ^entity-type ) ENTITY-SET-TYPE @ ;

\ The base type's properties first, then the type's own, in declared order.
: EACH-PROPERTY ( ^entity-type [ ^property -- ] -- ) PROPERTIES-OF ;

\ The key in declared order: the type's own, else its nearest base type's.
: EACH-KEY ( ^entity-type [ ^property -- ] -- )
   {: t:^ q :}
   t ENTITY-TYPE-KEY-HOLDER @ ENTITY-TYPE-KEY @ begin dup NULL-PTR <> while
      dup KEY-REF-PROPERTY @ q execute KEY-REF-NEXT @
   repeat drop ;

: PROPERTY-NAME$ ( ^property -- ^u8 n ) PROPERTY-NAME @ SPAN:$ ;
: PROPERTY-TYPE$ ( ^property -- ^u8 n ) PROPERTY-TYPE @ SPAN:$ ;
: NULLABLE? ( ^property -- bool ) PROPERTY-NULLABLE @ ;

\ The base type's navigation properties first, then the type's own.
: EACH-NAVIGATION ( ^entity-type [ ^navigation -- ] -- ) NAVIGATIONS-OF ;

: NAVIGATION-NAME$ ( ^navigation -- ^u8 n ) NAVIGATION-NAME @ SPAN:$ ;
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
: TYPE-ANNOTATION$ ( ^entity-type ^u8 n ^u8 n -- ^u8 n bool )
   {: t:^ ns:^ nl:n name:^ len:n :}
   t ENTITY-TYPE-ANNOTATIONS @ ns nl name len ANNOTATION ;

: PROPERTY-ANNOTATION$ ( ^property ^u8 n ^u8 n -- ^u8 n bool )
   {: p:^ ns:^ nl:n name:^ len:n :}
   p PROPERTY-ANNOTATIONS @ ns nl name len ANNOTATION ;

;package

\ response.f - PAGE, RECORD and COPY-ENTITY; part of package ODATA
\ (src/lib/odata.f), which states the contract.
\ COPY-ENTITY uses no recursion: JSON:WALK gives the record's events in text
\ order, and each open container is a frame on a stack that starts in the
\ call's scratch and doubles into a mapping. A name sets the innermost frame's
\ slot, what the member's value is read as; an array's frame has one slot for
\ every element. A value is handled by the slot it fills: a record's member
\ by the record's type, an expanded navigation by frames of its target type, a
\ complex value by frames of its complex type, and other values are copied.
\ A container a slot rejects is read past in a frame that rejects it at its
\ close, the first event that carries the container's value.

require src/lib/error.f
require src/lib/string.f
require src/lib/string-option.f
require src/lib/span.f
require src/lib/memory.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/date.f
require src/lib/adt/option.f
require src/lib/reason.f
require src/lib/json.f
require src/lib/odata/metadata.f
require src/lib/json/value.f
require src/lib/json/read.f
require src/lib/json/write.f
require src/lib/json/copy.f

package ODATA
private

\ Rejects the value as E-RESPONSE.
: RESPONSE-REJECT ( JSON:value str str -- )
   {: v es:str fs:str :}
   [: v es fs JSON:REJECT ;] catch {: code:n :}
   code JSON:E-MEMBER = if
      JSON:E-MEMBER REASON:TEXT$ E-RESPONSE REASON:THROW
   then
   code throw ;

: KIND-FIX$ ( -- str ) s" write that kind of value here" ;
: REPEAT-FIX$ ( -- str ) s" remove the repeated member, keeping one" ;
: MISSING-FIX$ ( -- str ) s" add the member to the object" ;

\ RESPONSE-REJECT with the four texts joined as the expected text.
: JOINED-REJECT ( JSON:value str str str str str -- )
   {: v a:str b:str c:str d:str f:str :}
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: out:^ :}
   [: a out VEC:APPEND  b out VEC:APPEND
      c out VEC:APPEND  d out VEC:APPEND
      v out VEC:BYTES f RESPONSE-REJECT ;]
   [: out VEC:DELETE ;] finally ;

\ The value is not WANT, which WHAT needs.
: UNEXPECTED ( JSON:value str str -- )
   {: v what:str want:str :}
   v want s"  for " what s" " KIND-FIX$ JOINED-REJECT ;

\ The name is a second member of WHAT, under another of its names.
: TWICE ( JSON:value str -- )
   {: name what:str :}
   name what s"  once" s" " s" " REPEAT-FIX$ JOINED-REJECT ;

\ The object, which needs the member named, lacks it.
: ABSENT ( JSON:value str -- )
   {: v name:str :}
   v s" the member `" name s" ` in the envelope" s" "
   MISSING-FIX$ JOINED-REJECT ;

\ One PAGE call's state: `records` holds a value once `found`.
STRUCTURE envelope DERIVE addr
   FIELD version version
   FIELD records JSON:value
   FIELD found bool
   FIELD link ^VEC:vector<u8>
   FIELD linked bool
   FIELD count n
   FIELD counted bool
;STRUCTURE

CAST: >ENVELOPE ( ^u8 -- ^envelope )

: TAKE ( JSON:value ^envelope -- )
   {: v e:^ :}
   v e ENVELOPE-RECORDS !
   true e ENVELOPE-FOUND ! ;

: NEED-RECORDS ( ^envelope JSON:value str -- )
   {: e:^ v name:str :}
   e ENVELOPE-FOUND @ if exit then
   v name ABSENT ;

\ The value of one to 18 decimal digits, which no cell overflows.
: DIGITS ( str -- option<n> )
   {: s:str :}
   s STR:LEN 18 > if OPTION:NONE exit then
   s STR:PARSE-POS ;

\ The count's text, read from the value V.
: TAKE-COUNT ( str ^envelope JSON:value str -- )
   {: text:str e:^ v what:str :}
   text DIGITS MATCH option
      none OF
         v s" a non-negative integer for " what s" " s" "
         s" write the count as one to 18 decimal digits" JOINED-REJECT
      ENDOF
      some OF
         e ENVELOPE-COUNT ! true e ENVELOPE-COUNTED !
      ENDOF
   ;MATCH ;

: COUNT-ONCE ( ^envelope JSON:value str -- )
   {: e:^ name what:str :}
   e ENVELOPE-COUNTED @ if name what TWICE then ;

: V2-COUNT ( JSON:value ^envelope -- )
   {: v e:^ :}
   v JSON:STRING? 0= if
      v s" member `__count`" s" a string of digits" UNEXPECTED
   then
   v [: e v s" member `__count`" TAKE-COUNT ;] JSON:WITH-STRING ;

: FLOAT-MARK? ( n -- bool )
   dup [char] . = over [char] e = or swap [char] E = or ;

\ A number written with no fraction and no exponent.
: INT? ( JSON:value -- bool )
   {: v :}
   v JSON:NUMBER? 0= if false exit then
   v JSON:TEXT$ {: t:str :}
   t STR:LEN 0 ?do t STR:DATA i + c@ FLOAT-MARK? if unloop false exit then loop
   true ;

: V4-COUNT ( JSON:value JSON:value ^envelope str -- )
   {: name v e:^ what:str :}
   e name what COUNT-ONCE
   v INT? 0= if v what s" an integer" UNEXPECTED then
   v JSON:TEXT$ e v what TAKE-COUNT ;

: NEXT-LINK ( JSON:value JSON:value ^envelope str -- )
   {: name v e:^ what:str :}
   e ENVELOPE-LINKED @ if name what TWICE then
   true e ENVELOPE-LINKED !
   v JSON:STRING? 0= if v what s" a string" UNEXPECTED then
   v e ENVELOPE-LINK @ JSON:APPEND-STRING ;

: ARRAY-MEMBER ( JSON:value ^envelope str -- )
   {: v e:^ what:str :}
   v JSON:ARRAY? 0= if v what s" an array of records" UNEXPECTED then
   v e TAKE ;

: RESULTS-MEMBER ( JSON:value JSON:value ^envelope -- )
   {: name v e:^ :}
   name s" results" JSON:STRING= if
      v e s" member `results`" ARRAY-MEMBER exit
   then
   name s" __count" JSON:STRING= if v e V2-COUNT exit then
   name s" __next" JSON:STRING= if
      name v e s" member `__next`" NEXT-LINK
   then ;

: PAGE-D ( JSON:value ^envelope -- )
   {: v e:^ :}
   v JSON:ARRAY? if v e TAKE exit then
   v JSON:OBJECT? if
      v [: e RESULTS-MEMBER ;] JSON:EACH-MEMBER
      e v s" results" NEED-RECORDS exit
   then
   v s" member `d`" s" an array or an object" UNEXPECTED ;

: V2-PAGE-MEMBER ( JSON:value JSON:value ^envelope -- )
   {: name v e:^ :}
   name s" d" JSON:STRING= if v e PAGE-D then ;

: V4-COUNT-MEMBER ( JSON:value JSON:value ^envelope -- )
   {: name v e:^ :}
   name s" @odata.count" JSON:STRING= if
      name v e s" member `@odata.count`" V4-COUNT exit
   then
   name s" @count" JSON:STRING= if
      name v e s" member `@count`" V4-COUNT
   then ;

: V4-LINK-MEMBER ( JSON:value JSON:value ^envelope -- )
   {: name v e:^ :}
   name s" @odata.nextLink" JSON:STRING= if
      name v e s" member `@odata.nextLink`" NEXT-LINK exit
   then
   name s" @nextLink" JSON:STRING= if
      name v e s" member `@nextLink`" NEXT-LINK
   then ;

: V4-PAGE-MEMBER ( JSON:value JSON:value ^envelope -- )
   {: name v e:^ :}
   name s" value" JSON:STRING= if
      v e s" member `value`" ARRAY-MEMBER exit
   then
   name v e V4-COUNT-MEMBER
   name v e V4-LINK-MEMBER ;

: TOP-OBJECT ( JSON:value -- )
   {: v :}
   v JSON:OBJECT? 0= if v s" the body" s" an object" UNEXPECTED then ;

: ENVELOPE-V2? ( ^envelope -- bool ) ENVELOPE-VERSION @ V2-VERSION? ;

: NEW-ENVELOPE ( ^u8 ^metadata ^VEC:vector<u8> -- ^envelope )
   {: room:^ m:^ link:^ :}
   room >ENVELOPE {: e:^ :}
   m METADATA-VERSION @ e ENVELOPE-VERSION !
   false e ENVELOPE-FOUND !
   link e ENVELOPE-LINK !
   false e ENVELOPE-LINKED !
   0 e ENVELOPE-COUNT !
   false e ENVELOPE-COUNTED !
   e ;

: COUNT-OF ( ^envelope -- option<n> )
   {: e:^ :}
   e ENVELOPE-COUNTED @ if e ENVELOPE-COUNT @ OPTION:SOME exit then
   OPTION:NONE ;

: PAGE-IN ( ^u8 ^metadata JSON:value ^VEC:vector<u8> -- JSON:value option<n> )
   {: room:^ m:^ body link:^ :}
   room m link NEW-ENVELOPE {: e:^ :}
   body TOP-OBJECT
   e ENVELOPE-V2? if
      body [: e V2-PAGE-MEMBER ;] JSON:EACH-MEMBER
      e body s" d" NEED-RECORDS
   else
      body [: e V4-PAGE-MEMBER ;] JSON:EACH-MEMBER
      e body s" value" NEED-RECORDS
   then
   e ENVELOPE-RECORDS @ e COUNT-OF ;

\ V2's `d`, an object.
: RECORD-D ( JSON:value -- JSON:value )
   {: body :}
   body s" d" JSON:FIND MATCH option
      none OF body s" d" ABSENT ENDOF
      some OF
         {: d :}
         d JSON:OBJECT? 0= if d s" member `d`" s" an object" UNEXPECTED then
         d
      ENDOF
   ;MATCH ;

\ An open container: a record, an array of records, a V2 results object, a V2
\ navigation's object not yet named, a copy, one read past or one rejected.
ENUM nest entity entities results expanded object array skip wrong ;ENUM

\ What a value is read as: the root, a copied member, an element, a dropped,
\ dated, complex or navigation member, `results`, or an array's record.
ENUM slot root plain element dropped dated complex nav results record
;ENUM

\ A wrong frame rejects its container as its `fault`; `done` is set at a
\ results object's `results` member.
STRUCTURE frame DERIVE addr
   FIELD nest nest
   FIELD type ^entity-type
   FIELD slot slot
   FIELD fault slot
   FIELD property ^property
   FIELD navigation ^navigation
   FIELD done bool
;STRUCTURE

STRUCTURE copy DERIVE addr
   FIELD frames ^u8
   FIELD room n
   FIELD depth n
   FIELD mapping SPAN:span<u8>
   FIELD type ^entity-type
   FIELD model ^metadata
   BUFFER text 24
;STRUCTURE

16 constant FIRST-FRAMES

CAST: >COPY ( ^u8 -- ^copy )
CAST: >FRAMES ( ^u8 -- ^frame )

: NTH-FRAME ( ^copy n -- ^frame )
   {: c:^ i:n :}
   c COPY-FRAMES @ i FRAME-CELLS * cells + >FRAMES ;

: TOP ( ^copy -- ^frame ) dup COPY-DEPTH @ 1- NTH-FRAME ;

\ A new mapping of WANT bytes holding the HAVE bytes at FROM; the old
\ mapping is freed.
: MOVED ( str n SPAN:span<u8> -- SPAN:span<u8> )
   {: from:str want:n old:SPAN:span<u8> :}
   want MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: grown:SPAN:span<u8> :}
   from grown SPAN:COPY
   old MEM:FREE-SPAN
   grown ;

: GROW ( ^copy -- )
   {: c:^ :}
   c COPY-ROOM @ FRAME-CELLS * cells {: have:n :}
   c COPY-FRAMES @ have >str have 2 * c COPY-MAPPING @ MOVED
   {: grown:SPAN:span<u8> :}
   grown c COPY-MAPPING !
   grown SPAN:$ STR:DATA c COPY-FRAMES !
   c COPY-ROOM @ 2 * c COPY-ROOM ! ;

: DROP-FRAMES ( ^copy -- ) COPY-MAPPING @ MEM:FREE-SPAN ;

\ A frame read after a PUSH is read afresh: a PUSH may move the frames.
: PUSH ( ^copy nest ^entity-type slot -- )
   {: c:^ kind:nest t:^ s:slot :}
   c COPY-DEPTH @ c COPY-ROOM @ = if c GROW then
   1 c COPY-DEPTH +!
   c TOP {: f:^ :}
   kind f FRAME-NEST !  t f FRAME-TYPE !  s f FRAME-SLOT !
   false f FRAME-DONE ! ;

: POP ( ^copy -- ) -1 swap COPY-DEPTH +! ;

\ The innermost frame's slot, the root's before any.
: SLOT@ ( ^copy -- slot )
   {: c:^ :}
   c COPY-DEPTH @ 0= if SLOT-ROOT exit then
   c TOP FRAME-SLOT @ ;

: OPEN-RECORD ( ^JSON:writer ^copy ^entity-type -- ^JSON:writer )
   {: c:^ t:^ :}
   JSON:WRITE-OBJECT-START c NEST-ENTITY t SLOT-DROPPED PUSH ;

: OPEN-RECORDS ( ^JSON:writer ^copy ^entity-type -- ^JSON:writer )
   {: c:^ t:^ :}
   JSON:WRITE-ARRAY-START c NEST-ENTITIES t SLOT-RECORD PUSH ;

: OPEN-OBJECT ( ^JSON:writer ^copy -- ^JSON:writer )
   {: c:^ :}
   JSON:WRITE-OBJECT-START c NEST-OBJECT NULL-PTR SLOT-DROPPED PUSH ;

: OPEN-ARRAY ( ^JSON:writer ^copy -- ^JSON:writer )
   {: c:^ :}
   JSON:WRITE-ARRAY-START c NEST-ARRAY NULL-PTR SLOT-ELEMENT PUSH ;

: PUSH-SKIP ( ^copy -- ) NEST-SKIP NULL-PTR SLOT-DROPPED PUSH ;

\ Reads the container past, to reject it as FAULT at its close; the
\ innermost frame holds its member.
: PUSH-WRONG ( ^copy slot -- )
   {: c:^ s:slot :}
   c TOP FRAME-PROPERTY @ {: p:^ :}
   c TOP FRAME-NAVIGATION @ {: n:^ :}
   c PUSH-SKIP
   c TOP {: f:^ :}
   NEST-WRONG f FRAME-NEST !  s f FRAME-FAULT !
   p f FRAME-PROPERTY !  n f FRAME-NAVIGATION ! ;

: PUSH-EXPANDED ( ^copy ^navigation -- )
   {: c:^ n:^ :}
   c NEST-EXPANDED n NAVIGATION-TARGET-TYPE @ SLOT-DROPPED PUSH
   n c TOP FRAME-NAVIGATION ! ;

\ V2's `__metadata` and every V4 member whose name holds `@`.
: DROPPED? ( ^entity-type str -- bool )
   {: t:^ name:str :}
   t TYPE-V2? if name s" __metadata" STR:= exit then
   name STR:LEN 0 ?do name STR:DATA i + c@ [char] @ = if unloop true exit then loop
   false ;

\ A declared V2 Edm.DateTime or Edm.DateTimeOffset.
: DATED? ( ^entity-type ^property -- bool )
   {: t:^ p:^ :}
   p NULL-PTR = if false exit then
   t TYPE-V2? 0= if false exit then
   p PROPERTY-TYPE @ {: ty:str :}
   ty s" Edm.DateTime" STR:= ty s" Edm.DateTimeOffset" STR:= or ;

: COMPLEX? ( ^property -- bool )
   {: p:^ :}
   p NULL-PTR = if false exit then
   p PROPERTY-COMPLEX-TYPE @ NULL-PTR <> ;

86400000 constant MS-DAY
0 1 1 DATE:YMD>DAYS MS-DAY * constant MS-FIRST
10000 1 1 DATE:YMD>DAYS MS-DAY * constant MS-END

\ Ends in a sign and four digits, with ticks before them.
: ZONED? ( str -- bool )
   {: s:str :}
   s STR:LEN 6 < if false exit then
   s STR:DATA s STR:LEN + 5 - c@ {: sign:n :}
   sign [char] + = sign [char] - = or 0= if false exit then
   s STR:DATA s STR:LEN + 4 - 4 >str STR:DIGITS? ;

\ The offset of `+mmmm` or `-mmmm` in milliseconds, 0 when there is none.
: ZONE-MS ( str -- n )
   {: s:str :}
   s ZONED? 0= if 0 exit then
   s STR:DATA s STR:LEN + 4 - 4 >str DIGITS MATCH option
      none OF 0 ENDOF
      some OF 60000 * ENDOF
   ;MATCH
   s STR:DATA s STR:LEN + 5 - c@ [char] - = if negate then ;

\ The UTC milliseconds of `ms`, `-ms`, `ms+mmmm` or `ms-mmmm`, the offset's
\ minutes taken off; none outside the years 0 to 9999.
: TICKS ( str -- option<n> )
   {: s:str :}
   s ZONED? if s STR:LEN 5 - else s STR:LEN then {: v:n :}
   v 0 > if s STR:DATA c@ [char] - = else false then {: neg:bool :}
   neg if 1 else 0 then {: lead:n :}
   s STR:DATA lead + v lead - >str DIGITS MATCH option
      none OF OPTION:NONE ENDOF
      some OF
         neg if negate then s ZONE-MS - {: ms:n :}
         ms MS-FIRST >= ms MS-END < and if ms OPTION:SOME
         else OPTION:NONE then
      ENDOF
   ;MATCH ;

: MS ( str -- option<n> )
   {: s:str :}
   s s" /Date(" STR:STARTS-WITH? s s" )/" STR:ENDS-WITH? and 0= if
      OPTION:NONE exit
   then
   s" /Date(" STR:LEN {: open:n :}
   s STR:DATA open + s STR:LEN open - 2 - >str TICKS ;

\ Quotient and remainder, the remainder in 0 to d - 1.
: FLOORED ( n n -- n n )
   {: n:n d:n :}
   n d mod dup 0 < if d + then {: r:n :}
   n r - d / r ;

\ ISO 8601 UTC: the time of day formatted on day 0, then the day over it.
: ISO ( n ^copy -- str )
   {: ms:n c:^ :}
   c COPY-TEXT SPAN:$ STR:DATA {: out:^ :}
   ms MS-DAY FLOORED {: day:n rest:n :}
   rest 1000 / out DATE:TIME-LEN DATE:FORMAT-EPOCH-UTC drop
   day out DATE:LEN DATE:FORMAT-YMD drop
   rest 1000 mod {: milli:n :}
   DATE:TIME-LEN 1- {: zone:n :}
   milli 0= if out DATE:TIME-LEN >str exit then
   [char] . out zone + c!
   milli 3 out zone 1+ DATE:WIDTH!
   [char] Z out zone + 4 + c!
   out zone 5 + >str ;

: DATE-REJECT ( JSON:value ^property -- )
   {: v p:^ :}
   v s" /Date(ms)/ or null for member `" p PROPERTY-NAME @
   s" ` of type " p PROPERTY-TYPE @
   s" write the value as /Date(ms)/, /Date(ms+mmmm)/ or /Date(ms-mmmm)/ in the years 0 to 9999, or null"
   JOINED-REJECT ;

: NAV-KEY ( ^JSON:writer ^navigation -- ^JSON:writer )
   {: n:^ :}
   JSON:WRITE-COMMA n NAVIGATION-NAME @ JSON:WRITE-KEY ;

: NAV-WANT ( ^navigation -- str )
   {: n:^ :}
   n TO-MANY? 0= if s" null or a record" exit then
   n NAVIGATION-TARGET-TYPE @ TYPE-V2? 0= if
      s" null or an array of records" exit
   then
   s" null, an array of records or an object with `results`" ;

: NAV-REJECT ( JSON:value ^navigation -- )
   {: v n:^ :}
   v n NAV-WANT s"  for navigation `" n NAVIGATION-NAME @ s" `"
   KIND-FIX$ JOINED-REJECT ;

: COMPLEX-WANT ( ^property -- str )
   PROPERTY-MANY @ if s" null or an array of records" exit then
   s" null or a record" ;

: COMPLEX-REJECT ( JSON:value ^property -- )
   {: v p:^ :}
   v p COMPLEX-WANT s"  for member `" p PROPERTY-NAME @ s" `"
   KIND-FIX$ JOINED-REJECT ;

\ Rejects the value read as the slot, whose member the frame holds.
: WRONG ( JSON:value slot ^frame -- )
   {: v s:slot f:^ :}
   s MATCH slot
      dated OF v f FRAME-PROPERTY @ DATE-REJECT ENDOF
      complex OF v f FRAME-PROPERTY @ COMPLEX-REJECT ENDOF
      nav OF v f FRAME-NAVIGATION @ NAV-REJECT ENDOF
      results OF
         v s" an array of records for member `results`" KIND-FIX$
         RESPONSE-REJECT
      ENDOF
      record OF
         v s" a record object in an array of records" KIND-FIX$
         RESPONSE-REJECT
      ENDOF
      root OF ENDOF
      plain OF ENDOF
      element OF ENDOF
      dropped OF ENDOF
   ;MATCH ;

\ The decoded string of the value, a date in a dated slot.
: DATE-TEXT ( ^JSON:writer str JSON:value ^frame ^copy -- ^JSON:writer )
   {: text:str v f:^ c:^ :}
   text MS MATCH option
      none OF v SLOT-DATED f WRONG ENDOF
      some OF c ISO JSON:WRITE-STRING ENDOF
   ;MATCH ;

: DATED ( ^JSON:writer JSON:value ^frame ^copy -- ^JSON:writer )
   {: v f:^ c:^ :}
   v JSON:NULL? if JSON:WRITE-NULL exit then
   v JSON:STRING? 0= if v SLOT-DATED f WRONG then
   v [: v f c DATE-TEXT ;] JSON:WITH-STRING ;

: COPIED ( ^JSON:writer JSON:value -- ^JSON:writer )
   {: w v :}
   v w JSON:COPY-VALUE ;

\ A scalar, read as the innermost frame's slot.
: SCALAR ( ^JSON:writer JSON:value ^copy -- ^JSON:writer )
   {: v c:^ :}
   c TOP {: f:^ :}
   f FRAME-SLOT @ {: s:slot :}
   s MATCH slot
      plain OF v COPIED ENDOF
      element OF JSON:WRITE-COMMA v COPIED ENDOF
      dated OF v f c DATED ENDOF
      complex OF
         v JSON:NULL? if JSON:WRITE-NULL else v s f WRONG then
      ENDOF
      nav OF
         v JSON:NULL? 0= if v s f WRONG then
         f FRAME-NAVIGATION @ NAV-KEY JSON:WRITE-NULL
      ENDOF
      results OF v s f WRONG ENDOF
      record OF v s f WRONG ENDOF
      root OF ENDOF
      dropped OF ENDOF
   ;MATCH ;

: NAV-OBJECT ( ^JSON:writer ^copy -- ^JSON:writer )
   {: c:^ :}
   c TOP FRAME-NAVIGATION @ {: n:^ :}
   n NAVIGATION-TARGET-TYPE @ {: t:^ :}
   t TYPE-V2? if c n PUSH-EXPANDED exit then
   n TO-MANY? if c SLOT-NAV PUSH-WRONG exit then
   n NAV-KEY c t OPEN-RECORD ;

: NAV-ARRAY ( ^JSON:writer ^copy -- ^JSON:writer )
   {: c:^ :}
   c TOP FRAME-NAVIGATION @ {: n:^ :}
   n TO-MANY? 0= if c SLOT-NAV PUSH-WRONG exit then
   n NAV-KEY c n NAVIGATION-TARGET-TYPE @ OPEN-RECORDS ;

: COMPLEX-OBJECT ( ^JSON:writer ^copy -- ^JSON:writer )
   {: c:^ :}
   c TOP FRAME-PROPERTY @ {: p:^ :}
   p PROPERTY-COMPLEX-TYPE @ {: t:^ :}
   p PROPERTY-MANY @ 0= if c t OPEN-RECORD exit then
   t TYPE-V2? if c NEST-RESULTS t SLOT-DROPPED PUSH exit then
   c SLOT-COMPLEX PUSH-WRONG ;

: COMPLEX-ARRAY ( ^JSON:writer ^copy -- ^JSON:writer )
   {: c:^ :}
   c TOP FRAME-PROPERTY @ {: p:^ :}
   p PROPERTY-MANY @ 0= if c SLOT-COMPLEX PUSH-WRONG exit then
   c p PROPERTY-COMPLEX-TYPE @ OPEN-RECORDS ;

: TOP-TYPE ( ^copy -- ^entity-type ) TOP FRAME-TYPE @ ;

\ An object opened as the innermost frame's slot.
: OBJECT-OPENED ( ^JSON:writer ^copy -- ^JSON:writer )
   {: c:^ :}
   c SLOT@ MATCH slot
      root OF c c COPY-TYPE @ OPEN-RECORD ENDOF
      plain OF c OPEN-OBJECT ENDOF
      element OF JSON:WRITE-COMMA c OPEN-OBJECT ENDOF
      dropped OF c PUSH-SKIP ENDOF
      dated OF c SLOT-DATED PUSH-WRONG ENDOF
      complex OF c COMPLEX-OBJECT ENDOF
      nav OF c NAV-OBJECT ENDOF
      results OF c SLOT-RESULTS PUSH-WRONG ENDOF
      record OF JSON:WRITE-COMMA c c TOP-TYPE OPEN-RECORD ENDOF
   ;MATCH ;

\ An array opened as the innermost frame's slot; the root is an object.
: ARRAY-OPENED ( ^JSON:writer ^copy -- ^JSON:writer )
   {: c:^ :}
   c SLOT@ MATCH slot
      root OF ENDOF
      plain OF c OPEN-ARRAY ENDOF
      element OF JSON:WRITE-COMMA c OPEN-ARRAY ENDOF
      dropped OF c PUSH-SKIP ENDOF
      dated OF c SLOT-DATED PUSH-WRONG ENDOF
      complex OF c COMPLEX-ARRAY ENDOF
      nav OF c NAV-ARRAY ENDOF
      results OF c c TOP-TYPE OPEN-RECORDS ENDOF
      record OF c SLOT-RECORD PUSH-WRONG ENDOF
   ;MATCH ;

\ The frame takes the slot of the member named, its key written.
: KEYED ( ^JSON:writer str ^frame slot -- ^JSON:writer )
   {: name:str f:^ s:slot :}
   s f FRAME-SLOT !
   JSON:WRITE-COMMA name JSON:WRITE-KEY ;

: PROPERTY-KEYED ( ^JSON:writer ^frame slot -- ^JSON:writer )
   {: f:^ s:slot :}
   f FRAME-PROPERTY @ PROPERTY-NAME @ f s KEYED ;

\ A member of a record of the frame's type, by its decoded name.
: MEMBER ( ^JSON:writer str ^copy ^frame -- ^JSON:writer )
   {: name:str c:^ f:^ :}
   f FRAME-TYPE @ {: t:^ :}
   SLOT-DROPPED f FRAME-SLOT !
   c COPY-MODEL @ name HIDDEN? if exit then
   t name DROPPED? if exit then
   t name FIND-PROPERTY {: p:^ :}
   p f FRAME-PROPERTY !
   t p DATED? if f SLOT-DATED PROPERTY-KEYED exit then
   p COMPLEX? if f SLOT-COMPLEX PROPERTY-KEYED exit then
   t name FIND-NAVIGATION {: n:^ :}
   n f FRAME-NAVIGATION !
   n NULL-PTR <> if SLOT-NAV f FRAME-SLOT ! exit then
   name f SLOT-PLAIN KEYED ;

\ A member of a copied object: dropped when hidden, else copied.
: OBJECT-MEMBER ( ^JSON:writer str ^copy ^frame -- ^JSON:writer )
   {: name:str c:^ f:^ :}
   c COPY-MODEL @ name HIDDEN? if
      SLOT-DROPPED f FRAME-SLOT ! exit
   then
   name f SLOT-PLAIN KEYED ;

\ A V2 results object's member: `results` is read, others dropped.
: RESULTS-NAME ( JSON:value ^frame -- )
   {: v f:^ :}
   v s" results" JSON:STRING= 0= if SLOT-DROPPED f FRAME-SLOT ! exit then
   SLOT-RESULTS f FRAME-SLOT !
   true f FRAME-DONE ! ;

\ A V2 navigation's object becomes, after its key, a results object or a
\ record.
: EXPAND ( ^JSON:writer ^copy -- ^JSON:writer )
   {: c:^ :}
   c TOP {: f:^ :}
   f FRAME-NAVIGATION @ {: n:^ :}
   n NAV-KEY
   n TO-MANY? if NEST-RESULTS f FRAME-NEST ! exit then
   JSON:WRITE-OBJECT-START NEST-ENTITY f FRAME-NEST ! ;

\ A first member named `__deferred` drops the navigation.
: EXPANDED-NAME ( ^JSON:writer JSON:value ^copy -- ^JSON:writer )
   {: v c:^ :}
   c TOP {: f:^ :}
   v s" __deferred" JSON:STRING= if NEST-SKIP f FRAME-NEST ! exit then
   c EXPAND
   f FRAME-NAVIGATION @ TO-MANY? if v f RESULTS-NAME exit then
   v [: c f MEMBER ;] JSON:WITH-STRING ;

: NAMED ( ^JSON:writer JSON:value ^copy -- ^JSON:writer )
   {: v c:^ :}
   c TOP {: f:^ :}
   f FRAME-NEST @ MATCH nest
      entity OF v [: c f MEMBER ;] JSON:WITH-STRING ENDOF
      object OF v [: c f OBJECT-MEMBER ;] JSON:WITH-STRING ENDOF
      results OF v f RESULTS-NAME ENDOF
      expanded OF v c EXPANDED-NAME ENDOF
      entities OF ENDOF
      array OF ENDOF
      skip OF ENDOF
      wrong OF ENDOF
   ;MATCH ;

: RESULTS-END ( JSON:value ^frame -- )
   {: v f:^ :}
   f FRAME-DONE @ if exit then
   v s" the member `results` in a results object" MISSING-FIX$
   RESPONSE-REJECT ;

: EXPANDED-END ( ^JSON:writer JSON:value ^copy -- ^JSON:writer )
   {: v c:^ :}
   c EXPAND
   c TOP {: f:^ :}
   f FRAME-NAVIGATION @ TO-MANY? if v f RESULTS-END exit then
   JSON:WRITE-OBJECT-END ;

\ The innermost frame's container, whose value is V, closes.
: CLOSED ( ^JSON:writer JSON:value ^copy -- ^JSON:writer )
   {: v c:^ :}
   c TOP {: f:^ :}
   f FRAME-NEST @ MATCH nest
      entity OF JSON:WRITE-OBJECT-END ENDOF
      object OF JSON:WRITE-OBJECT-END ENDOF
      entities OF JSON:WRITE-ARRAY-END ENDOF
      array OF JSON:WRITE-ARRAY-END ENDOF
      results OF v f RESULTS-END ENDOF
      expanded OF v c EXPANDED-END ENDOF
      skip OF ENDOF
      wrong OF v f FRAME-FAULT @ f WRONG ENDOF
   ;MATCH
   c POP ;

\ One event of the walk, handled by the innermost frame.
: COPY-EVENT ( ^JSON:writer JSON:event ^copy -- ^JSON:writer bool )
   {: e c:^ :}
   e MATCH JSON:event
      open-object OF c OBJECT-OPENED ENDOF
      open-array OF c ARRAY-OPENED ENDOF
      name OF c NAMED ENDOF
      scalar OF c SCALAR ENDOF
      close OF c CLOSED ENDOF
   ;MATCH
   true ;

: NEW-COPY ( ^u8 ^entity-type -- ^copy )
   {: room:^ t:^ :}
   room >COPY {: c:^ :}
   room COPY-CELLS cells + c COPY-FRAMES !
   FIRST-FRAMES c COPY-ROOM !
   0 c COPY-DEPTH !
   room 0 SPAN:MAKE c COPY-MAPPING !
   t c COPY-TYPE !
   t ENTITY-TYPE-MODEL @ c COPY-MODEL !
   c ;

: COPY-IN ( ^JSON:writer ^u8 JSON:value ^entity-type -- ^JSON:writer )
   {: room:^ v t:^ :}
   v JSON:OBJECT? 0= if
      v s" a record object" KIND-FIX$ RESPONSE-REJECT
   then
   room t NEW-COPY {: c:^ :}
   [: v [: c COPY-EVENT ;] JSON:WALK ;] [: c DROP-FRAMES ;] finally ;

public

\ The records array of a collection response, the decoded next link written
\ into the vector, left empty for none, and the count, none when absent.
: PAGE ( ^metadata JSON:value ^VEC:vector<u8> -- JSON:value option<n> )
   {: m:^ body link:^ :}
   link 0 VEC:RESIZE
   ENVELOPE-CELLS cells MEM:BYTES-ALLOC-LEN
   [: drop m body link PAGE-IN ;] MEM:WITH-BYTES ;

\ The query option that asks the service for the count PAGE answers.
: COUNT-OPTION$ ( ^metadata -- str )
   METADATA-VERSION @ MATCH version
      v2 OF s" $inlinecount=allpages" ENDOF
      v4 OF s" $count=true" ENDOF
   ;MATCH ;

\ The record of a single-entity response.
: RECORD ( ^metadata JSON:value -- JSON:value )
   {: m:^ body :}
   body TOP-OBJECT
   m METADATA-VERSION @ V2-VERSION? 0= if body exit then
   body RECORD-D ;

\ Writes the plain form of the record, one of the type.
: COPY-ENTITY ( JSON:value ^JSON:writer ^entity-type -- ^JSON:writer )
   {: v w t:^ :}
   w COPY-CELLS FIRST-FRAMES FRAME-CELLS * + cells MEM:BYTES-ALLOC-LEN
   [: drop v t COPY-IN ;] MEM:WITH-BYTES ;

;package

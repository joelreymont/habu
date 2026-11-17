\ response.f - PAGE, RECORD$ and COPY-ENTITY; part of package ODATA
\ (src/lib/odata.f), which states the contract.
\ PAGE and RECORD$ walk the envelope's members, every token through NEXT,
\ then read the rest of the body, so they answer only a body that reads
\ whole.
\ COPY-ENTITY uses no recursion: each open container is a frame on a stack
\ that starts in the call's scratch and doubles into a mapping. The
\ innermost frame handles each token and answers the token to handle next:
\ a record frame rewrites its members by its type, an expanded navigation
\ opens frames of its target type, a complex value frames of its complex
\ type, and other containers are copied.

require src/lib/error.f
require src/lib/string.f
require src/lib/span.f
require src/lib/memory.f
require src/lib/log.f
require src/lib/date.f
require src/lib/adt/option.f
require src/lib/json.f
require src/lib/odata/metadata.f

package ODATA
private

\ Rejections: one log line, then E-RESPONSE. LINE-AT opens the line; the
\ caller names the subject, EXPECT adds what was expected, REJECT ends it.
: LINE-AT ( ^u8 n n -- )
   {: from:^ fl:n at:n :}
   LOG:ERROR s" odata" LOG:LINE
   from fl LOG:TEXT+ s"  byte " LOG:TEXT+ at LOG:INT+
   s" , " LOG:TEXT+ ;

: EXPECT ( ^u8 n -- ) s" : expected " LOG:TEXT+ LOG:TEXT+ ;

: REJECT ( ^u8 n -- )
   FOUND+ LOG:;LINE E-RESPONSE throw ;

: QUOTED? ( n -- bool ) dup JSON:T-STR = swap JSON:T-KEY = or ;
: OPENER? ( n -- bool ) dup JSON:T-OBJ = swap JSON:T-ARR = or ;
: CLOSER? ( n -- bool ) dup JSON:T-OBJ-END = swap JSON:T-ARR-END = or ;
\ How far the second address lies past the first.
: OFFSET ( ^u8 ^u8 -- n ) NULL-PTR - swap NULL-PTR - - ;

\ The offset from BASE of the current token's first byte, a string's
\ opening quote included.
: TOKEN-AT ( JSON:reader ^u8 -- JSON:reader n )
   {: base:^ :}
   JSON:READ-TOKEN {: kind:n :}
   JSON:READ-SPAN$ drop base swap OFFSET
   kind QUOTED? if 1- then ;

\ The offset just past the current token, a string's closing quote included.
: TOKEN-END ( JSON:reader ^u8 -- JSON:reader n )
   {: base:^ :}
   JSON:READ-TOKEN {: kind:n :}
   JSON:READ-SPAN$ {: at:^ len:n :}
   base at OFFSET len +
   kind QUOTED? if 1+ then ;

\ One PAGE or RECORD$ call's state: `end` is the offset just past the last
\ token read; `records` is empty until the records are read.
STRUCTURE envelope DERIVE addr
   FIELD version version
   FIELD body ^u8
   FIELD end n
   FIELD records SPAN:span<u8>
   FIELD link SPAN:span<u8>
   FIELD link-len n
   FIELD linked bool
   FIELD count n
   FIELD counted bool
;STRUCTURE

CAST: >ENVELOPE ( ^u8 -- ^envelope )

: NEXT ( JSON:reader ^envelope -- JSON:reader n )
   {: e:^ :}
   JSON:READ-NEXT {: kind:n :}
   kind JSON:T-END <> if
      e ENVELOPE-BODY @ TOKEN-END e ENVELOPE-END !
   then
   kind ;

\ Past the value whose first token, KIND, is current.
: SKIP ( JSON:reader ^envelope n -- JSON:reader )
   {: e:^ kind:n :}
   kind OPENER? 0= if exit then
   1 begin dup 0 > while
      >r e NEXT r> swap
      dup OPENER? if drop 1+ else CLOSER? if 1- then then
   repeat drop ;

: SKIP-MEMBER ( JSON:reader ^envelope -- JSON:reader )
   {: e:^ :}
   e NEXT {: kind:n :}
   e kind SKIP ;

: ENVELOPE-LINE ( JSON:reader ^envelope ^u8 n -- JSON:reader )
   {: e:^ what:^ wl:n :}
   e ENVELOPE-BODY @ TOKEN-AT {: at:n :}
   s" response" at LINE-AT what wl LOG:TEXT+ ;

\ The current token, of KIND, is not what WHAT needs.
: UNEXPECTED ( JSON:reader ^envelope n ^u8 n ^u8 n -- JSON:reader )
   {: e:^ kind:n what:^ wl:n want:^ nl:n :}
   e what wl ENVELOPE-LINE
   want nl EXPECT kind JSON:KIND$ REJECT ;

: TWICE ( JSON:reader ^envelope ^u8 n -- JSON:reader )
   ENVELOPE-LINE
   s" it once" EXPECT s" it again" REJECT ;

: RECORDS? ( ^envelope -- bool ) ENVELOPE-RECORDS @ SPAN:LEN 0 > ;

\ The records are the current container, of KIND, through its close.
: TAKE ( JSON:reader ^envelope n -- JSON:reader )
   {: e:^ kind:n :}
   e ENVELOPE-BODY @ TOKEN-AT {: at:n :}
   e kind SKIP
   e ENVELOPE-BODY @ at + e ENVELOPE-END @ at - SPAN:MAKE
   e ENVELOPE-RECORDS ! ;

: NEED-RECORDS ( ^envelope ^u8 n -- )
   {: e:^ name:^ nl:n :}
   e RECORDS? if exit then
   s" response" e ENVELOPE-END @ LINE-AT
   s" the envelope" LOG:TEXT+
   s" member " EXPECT name nl LOG:TEXT+ s" none" REJECT ;

\ The value of one to 18 decimal digits, which no cell overflows.
: DIGITS ( ^u8 n -- option<n> )
   {: a:^ u:n :}
   u 18 > if OPTION:NONE exit then
   a u STR-PARSE-POS ;

\ The current string, decoded into scratch as long as its source, which
\ decoding never outgrows, handed to Q.
: DECODED ( JSON:reader [ JSON:reader ^u8 n -- JSON:reader ] -- JSON:reader )
   {: q :}
   JSON:READ-SPAN$ nip 1 max MEM:BYTES-ALLOC-LEN
   [: MEM:ALLOCATION>SPAN SPAN:$ over >r JSON:READ-STR
      r> swap q execute ;] MEM:WITH-BYTES ;

: TAKE-COUNT ( JSON:reader ^u8 n ^envelope ^u8 n -- JSON:reader )
   {: text:^ len:n e:^ what:^ wl:n :}
   text len DIGITS MATCH option
      none OF
         e what wl ENVELOPE-LINE
         s" a non-negative integer" EXPECT text len REJECT
      ENDOF
      some OF
         e ENVELOPE-COUNT ! true e ENVELOPE-COUNTED !
      ENDOF
   ;MATCH ;

: COUNT-ONCE ( JSON:reader ^envelope ^u8 n -- JSON:reader n )
   {: e:^ what:^ wl:n :}
   e ENVELOPE-COUNTED @ if e what wl TWICE then
   e NEXT ;

: V2-COUNT ( JSON:reader ^envelope -- JSON:reader )
   {: e:^ :}
   e s" member __count" COUNT-ONCE {: kind:n :}
   kind JSON:T-STR = if
      [: e s" member __count" TAKE-COUNT ;] DECODED
   else
      e kind s" member __count" s" a string of digits" UNEXPECTED
   then ;

: V4-COUNT ( JSON:reader ^envelope ^u8 n -- JSON:reader )
   {: e:^ what:^ wl:n :}
   e what wl COUNT-ONCE {: kind:n :}
   kind JSON:T-INT = if
      JSON:READ-SPAN$ e what wl TAKE-COUNT
   else
      e kind what wl s" an integer" UNEXPECTED
   then ;

: LINK-TEXT ( JSON:reader ^u8 n ^envelope -- JSON:reader )
   {: text:^ len:n e:^ :}
   e ENVELOPE-LINK @ {: link:SPAN:span<u8> :}
   len link SPAN:LEN > if E-SPAN-CAPACITY throw then
   text len link SPAN:COPY
   len e ENVELOPE-LINK-LEN ! ;

: NEXT-LINK ( JSON:reader ^envelope ^u8 n -- JSON:reader )
   {: e:^ what:^ wl:n :}
   e ENVELOPE-LINKED @ if e what wl TWICE then
   true e ENVELOPE-LINKED !
   e NEXT {: kind:n :}
   kind JSON:T-STR = if
      [: e LINK-TEXT ;] DECODED
   else
      e kind what wl s" a string" UNEXPECTED
   then ;

: ARRAY-MEMBER ( JSON:reader ^envelope ^u8 n -- JSON:reader )
   {: e:^ what:^ wl:n :}
   e RECORDS? if e what wl TWICE then
   e NEXT {: kind:n :}
   kind JSON:T-ARR = if
      e kind TAKE
   else
      e kind what wl s" an array of records" UNEXPECTED
   then ;

\ The members of the current object, each handed to Q on its key.
: MEMBERS ( JSON:reader ^envelope [ JSON:reader -- JSON:reader ] -- JSON:reader )
   {: e:^ q :}
   begin e NEXT JSON:T-KEY = while q execute repeat ;

: RESULTS-MEMBER ( JSON:reader ^envelope -- JSON:reader )
   {: e:^ :}
   s" results" JSON:READ-STR-EQ? if
      e s" member results" ARRAY-MEMBER exit
   then
   s" __count" JSON:READ-STR-EQ? if e V2-COUNT exit then
   s" __next" JSON:READ-STR-EQ? if
      e s" member __next" NEXT-LINK exit
   then
   e SKIP-MEMBER ;

: PAGE-D ( JSON:reader ^envelope -- JSON:reader )
   {: e:^ :}
   e RECORDS? if e s" member d" TWICE then
   e NEXT {: kind:n :}
   kind JSON:T-ARR = if e kind TAKE exit then
   kind JSON:T-OBJ = if
      e [: e RESULTS-MEMBER ;] MEMBERS
      e s" results" NEED-RECORDS exit
   then
   e kind s" member d" s" an array or an object" UNEXPECTED ;

: V2-PAGE-MEMBER ( JSON:reader ^envelope -- JSON:reader )
   {: e:^ :}
   s" d" JSON:READ-STR-EQ? if e PAGE-D else e SKIP-MEMBER then ;

: V4-PAGE-MEMBER ( JSON:reader ^envelope -- JSON:reader )
   {: e:^ :}
   s" value" JSON:READ-STR-EQ? if
      e s" member value" ARRAY-MEMBER exit
   then
   s" @odata.count" JSON:READ-STR-EQ? if
      e s" member @odata.count" V4-COUNT exit
   then
   s" @count" JSON:READ-STR-EQ? if e s" member @count" V4-COUNT exit then
   s" @odata.nextLink" JSON:READ-STR-EQ? if
      e s" member @odata.nextLink" NEXT-LINK exit
   then
   s" @nextLink" JSON:READ-STR-EQ? if
      e s" member @nextLink" NEXT-LINK exit
   then
   e SKIP-MEMBER ;

: RECORD-D ( JSON:reader ^envelope -- JSON:reader )
   {: e:^ :}
   e RECORDS? if e s" member d" TWICE then
   e NEXT {: kind:n :}
   kind JSON:T-OBJ = if
      e kind TAKE
   else
      e kind s" member d" s" an object" UNEXPECTED
   then ;

: V2-RECORD-MEMBER ( JSON:reader ^envelope -- JSON:reader )
   {: e:^ :}
   s" d" JSON:READ-STR-EQ? if e RECORD-D else e SKIP-MEMBER then ;

: TOP-OBJECT ( JSON:reader ^envelope -- JSON:reader n )
   {: e:^ :}
   e NEXT {: kind:n :}
   kind JSON:T-OBJ <> if
      e kind s" the body" s" an object" UNEXPECTED
   then
   kind ;

: FINISH ( JSON:reader ^envelope -- )
   {: e:^ :}
   begin e NEXT JSON:T-END = until
   JSON:READ-CLOSE ;

: ENVELOPE-V2? ( ^envelope -- bool ) ENVELOPE-VERSION @ V2-VERSION? ;

: PAGE-WALK ( ^envelope ^JSON:cursor ^u8 n -- )
   {: e:^ cur:^ body:^ len:n :}
   cur body len JSON:READ-INIT
   e TOP-OBJECT drop
   e ENVELOPE-V2? if
      e [: e V2-PAGE-MEMBER ;] MEMBERS e s" d" NEED-RECORDS
   else
      e [: e V4-PAGE-MEMBER ;] MEMBERS e s" value" NEED-RECORDS
   then
   e FINISH ;

: RECORD-WALK ( ^envelope ^JSON:cursor ^u8 n -- )
   {: e:^ cur:^ body:^ len:n :}
   cur body len JSON:READ-INIT
   e TOP-OBJECT {: kind:n :}
   e ENVELOPE-V2? if
      e [: e V2-RECORD-MEMBER ;] MEMBERS e s" d" NEED-RECORDS
   else
      e kind TAKE
   then
   e FINISH ;

\ A read the JSON reader stopped: one line with the end of the last token
\ read and the code, which the caller throws on.
: READ-STOPPED ( ^envelope n -- )
   {: e:^ code:n :}
   code E-JSON-READ-LAST < code E-JSON-READ-FIRST > or if exit then
   s" response" e ENVELOPE-END @ LINE-AT
   s" the end of the last token read: the read stopped with " LOG:TEXT+
   code LOG:CODE+ LOG:;LINE ;

: GUARDED ( ^envelope ^JSON:cursor [ -- ] -- )
   {: e:^ cur:^ q :}
   q catch {: code:n :}
   code 0= if exit then
   cur JSON:READ-DISPOSE
   e code READ-STOPPED
   code throw ;

: NEW-ENVELOPE ( ^u8 ^metadata ^u8 SPAN:span<u8> -- ^envelope )
   {: room:^ m:^ body:^ link:SPAN:span<u8> :}
   room >ENVELOPE {: e:^ :}
   m METADATA-VERSION @ e ENVELOPE-VERSION !
   body e ENVELOPE-BODY !
   0 e ENVELOPE-END !
   body 0 SPAN:MAKE e ENVELOPE-RECORDS !
   link e ENVELOPE-LINK !
   0 e ENVELOPE-LINK-LEN !
   false e ENVELOPE-LINKED !
   0 e ENVELOPE-COUNT !
   false e ENVELOPE-COUNTED !
   e ;

: COUNT-OF ( ^envelope -- option<n> )
   {: e:^ :}
   e ENVELOPE-COUNTED @ if e ENVELOPE-COUNT @ OPTION:SOME exit then
   OPTION:NONE ;

: PAGE-IN ( ^u8 ^metadata ^u8 n ^JSON:cursor SPAN:span<u8> -- ^u8 n n option<n> )
   {: room:^ m:^ body:^ len:n cur:^ link:SPAN:span<u8> :}
   room m body link NEW-ENVELOPE {: e:^ :}
   e cur [: e cur body len PAGE-WALK ;] GUARDED
   e ENVELOPE-RECORDS @ SPAN:$
   e ENVELOPE-LINK-LEN @ e COUNT-OF ;

: RECORD-IN ( ^u8 ^metadata ^u8 n ^JSON:cursor -- ^u8 n )
   {: room:^ m:^ body:^ len:n cur:^ :}
   room m body body 0 SPAN:MAKE NEW-ENVELOPE {: e:^ :}
   e cur [: e cur body len RECORD-WALK ;] GUARDED
   e ENVELOPE-RECORDS @ SPAN:$ ;

\ A frame is one open container: `results` a V2 results object, `done` once its
\ array is read; for an entity, `open` is its `{` and `names` its names' start.
ENUM nest entity entities results object array ;ENUM

STRUCTURE frame DERIVE addr
   FIELD nest nest
   FIELD type ^entity-type
   FIELD done bool
   FIELD open ^u8
   FIELD names n
;STRUCTURE

\ Offsets count from `start`, the record's `{`. `names` holds each open entity's
\ names, a length cell and the bytes padded to a cell; mappings start empty.
STRUCTURE copy DERIVE addr
   FIELD frames ^u8
   FIELD room n
   FIELD depth n
   FIELD mapping SPAN:span<u8>
   FIELD names ^u8
   FIELD names-room n
   FIELD names-used n
   FIELD names-mapping SPAN:span<u8>
   FIELD start ^u8
   FIELD dst ^u8
   FIELD cap n
   BUFFER text 24
;STRUCTURE

16 constant FIRST-FRAMES
512 constant FIRST-NAMES

CAST: >COPY ( ^u8 -- ^copy )
CAST: >FRAMES ( ^u8 -- ^frame )

: NTH-FRAME ( ^copy n -- ^frame )
   {: c:^ i:n :}
   c COPY-FRAMES @ i FRAME-CELLS * cells + >FRAMES ;

: TOP ( ^copy -- ^frame ) dup COPY-DEPTH @ 1- NTH-FRAME ;

: FREE-MAPPING ( SPAN:span<u8> -- )
   {: map:SPAN:span<u8> :}
   map SPAN:LEN 0 > if map MEM:FREE-SPAN then ;

\ A new mapping of WANT bytes holding the HAVE bytes at FROM; the old
\ mapping is freed.
: MOVED ( ^u8 n n SPAN:span<u8> -- SPAN:span<u8> )
   {: from:^ have:n want:n old:SPAN:span<u8> :}
   want MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: grown:SPAN:span<u8> :}
   from have grown SPAN:COPY
   old FREE-MAPPING
   grown ;

: GROW ( ^copy -- )
   {: c:^ :}
   c COPY-ROOM @ FRAME-CELLS * cells {: have:n :}
   c COPY-FRAMES @ have have 2 * c COPY-MAPPING @ MOVED
   {: grown:SPAN:span<u8> :}
   grown c COPY-MAPPING !
   grown SPAN:$ drop c COPY-FRAMES !
   c COPY-ROOM @ 2 * c COPY-ROOM ! ;

\ Room for MORE bytes past the names, the room doubled until they fit.
: NAMES-ROOM ( ^copy n -- )
   {: c:^ more:n :}
   c COPY-NAMES-USED @ {: used:n :}
   used more + {: need:n :}
   c COPY-NAMES-ROOM @ need >= if exit then
   c COPY-NAMES-ROOM @ begin dup need < while 2 * repeat {: want:n :}
   c COPY-NAMES @ used want c COPY-NAMES-MAPPING @ MOVED
   {: grown:SPAN:span<u8> :}
   grown c COPY-NAMES-MAPPING !
   grown SPAN:$ drop c COPY-NAMES !
   want c COPY-NAMES-ROOM ! ;

: DROP-FRAMES ( ^copy -- )
   {: c:^ :}
   c COPY-MAPPING @ FREE-MAPPING
   c COPY-NAMES-MAPPING @ FREE-MAPPING ;

\ A frame read after a PUSH is read afresh: a PUSH may move the frames.
: PUSH ( ^copy nest ^entity-type -- )
   {: c:^ kind:nest t:^ :}
   c COPY-DEPTH @ c COPY-ROOM @ = if c GROW then
   1 c COPY-DEPTH +!
   c TOP {: f:^ :}
   kind f FRAME-NEST ! t f FRAME-TYPE ! false f FRAME-DONE ! ;

: PUSH-ENTITY ( ^copy ^entity-type ^u8 -- )
   {: c:^ t:^ open:^ :}
   c NEST-ENTITY t PUSH
   c TOP {: f:^ :}
   open f FRAME-OPEN !
   c COPY-NAMES-USED @ f FRAME-NAMES ! ;

\ The bytes a name of LEN takes in the names, its length cell included.
: NAME-SIZE ( n -- n ) CELL 1- + CELL / 1+ cells ;

: NAME$ ( ^u8 n -- ^u8 n )
   + {: at:^ :}
   at cell+ at CELL-VIEW @ ;

: ADD-NAME ( ^copy ^u8 n -- )
   {: c:^ name:^ len:n :}
   len NAME-SIZE {: size:n :}
   c size NAMES-ROOM
   c COPY-NAMES @ c COPY-NAMES-USED @ + {: at:^ :}
   len at CELL-VIEW !
   name at cell+ len BYTE-COPY
   size c COPY-NAMES-USED +! ;

\ Writes into ORDER the offset of each name from FROM to PAST in the names
\ at BASE, and answers their count.
: SLOTS ( ^n ^u8 n n -- n )
   {: order:^ base:^ from:n past:n :}
   0 from begin dup past < while
      2dup swap cells order + !
      base over NAME$ nip NAME-SIZE +
      swap 1+ swap
   repeat drop ;

: NAME< ( n n ^u8 -- bool )
   {: i:n j:n base:^ :}
   base i NAME$ base j NAME$ STR< ;

: SAME-NEIGHBOUR? ( ^n n ^u8 -- bool )
   {: order:^ k:n base:^ :}
   k 1 ?do
      base order i 1- cells + @ NAME$
      base order i cells + @ NAME$ STR=
      if true unloop exit then
   loop false ;

\ Whether the top entity repeats a member: its names sorted by their bytes
\ in an order of their slots, past the names, equal names are neighbours.
: REPEATED? ( ^copy -- bool )
   {: c:^ :}
   c TOP FRAME-NAMES @ {: from:n :}
   c COPY-NAMES-USED @ {: past:n :}
   c past from - NAMES-ROOM
   c COPY-NAMES @ {: base:^ :}
   base past + CELL-VIEW {: order:^ :}
   order base from past SLOTS {: k:n :}
   order k [: base NAME< ;] SORT:SORT!
   order k base SAME-NEIGHBOUR? ;

: OPEN-AT ( JSON:reader ^JSON:writer -- JSON:reader ^JSON:writer ^u8 )
   swap JSON:READ-SPAN$ drop {: at:^ :} swap at ;

: ADVANCE ( JSON:reader ^JSON:writer -- JSON:reader ^JSON:writer n )
   swap JSON:READ-NEXT rot swap ;

\ The current string or key, decoded into the caller's scratch.
: DECODE ( JSON:reader ^JSON:writer ^copy -- JSON:reader ^JSON:writer ^u8 n )
   {: c:^ :}
   swap c COPY-DST @ c COPY-CAP @ JSON:READ-STR {: len:n :} swap
   c COPY-DST @ len ;

: SKIP-VALUE ( JSON:reader ^JSON:writer -- JSON:reader ^JSON:writer )
   ADVANCE OPENER? if swap JSON:READ-SKIP-VALUE swap then ;

\ On a key: past that member and the rest of its object.
: SKIP-REST ( JSON:reader ^JSON:writer -- JSON:reader ^JSON:writer )
   begin SKIP-VALUE ADVANCE JSON:T-KEY <> until ;

\ The bottom frame's close answers T-END and leaves the reader on it.
: CLOSE ( JSON:reader ^JSON:writer ^copy -- JSON:reader ^JSON:writer n )
   {: c:^ :}
   -1 c COPY-DEPTH +!
   c COPY-DEPTH @ 0= if JSON:T-END exit then
   ADVANCE ;

: RECORD-LINE ( JSON:reader ^JSON:writer ^copy ^u8 n -- JSON:reader ^JSON:writer )
   {: c:^ what:^ wl:n :}
   swap c COPY-START @ TOKEN-AT {: at:n :} swap
   s" response record" at LINE-AT what wl LOG:TEXT+ ;

\ A value written as it stands; a container opens its frame.
: PLAIN ( JSON:reader ^JSON:writer n ^copy -- JSON:reader ^JSON:writer )
   {: kind:n c:^ :}
   kind JSON:T-STR = if c DECODE JSON:WRITE-STRING exit then
   kind JSON:T-INT = kind JSON:T-FLOAT = or if
      swap JSON:READ-SPAN$ {: a:^ u:n :} swap a u JSON:WRITE-RAW exit
   then
   kind JSON:T-TRUE = if true JSON:WRITE-BOOL exit then
   kind JSON:T-FALSE = if false JSON:WRITE-BOOL exit then
   kind JSON:T-NULL = if JSON:WRITE-NULL exit then
   kind JSON:T-OBJ = if
      JSON:WRITE-OBJECT-START c NEST-OBJECT NULL-PTR PUSH exit
   then
   JSON:WRITE-ARRAY-START c NEST-ARRAY NULL-PTR PUSH ;

: IN-OBJECT ( JSON:reader ^JSON:writer n ^copy -- JSON:reader ^JSON:writer n )
   {: kind:n c:^ :}
   kind JSON:T-OBJ-END = if JSON:WRITE-OBJECT-END c CLOSE exit then
   JSON:WRITE-COMMA c DECODE JSON:WRITE-KEY
   ADVANCE c PLAIN ADVANCE ;

: IN-ARRAY ( JSON:reader ^JSON:writer n ^copy -- JSON:reader ^JSON:writer n )
   {: kind:n c:^ :}
   kind JSON:T-ARR-END = if JSON:WRITE-ARRAY-END c CLOSE exit then
   JSON:WRITE-COMMA kind c PLAIN ADVANCE ;

: IN-RECORDS ( JSON:reader ^JSON:writer n ^copy ^entity-type -- JSON:reader ^JSON:writer n )
   {: kind:n c:^ t:^ :}
   kind JSON:T-ARR-END = if JSON:WRITE-ARRAY-END c CLOSE exit then
   kind JSON:T-OBJ <> if
      c s" an array of records" RECORD-LINE
      s" a record object" EXPECT kind JSON:KIND$ REJECT
   then
   JSON:WRITE-COMMA JSON:WRITE-OBJECT-START
   OPEN-AT {: open:^ :} c t open PUSH-ENTITY ADVANCE ;

: RESULTS-ARRAY ( JSON:reader ^JSON:writer ^copy ^frame -- JSON:reader ^JSON:writer n )
   {: c:^ f:^ :}
   f FRAME-DONE @ if
      c s" a results object" RECORD-LINE
      s" member results once" EXPECT s" it again" REJECT
   then
   true f FRAME-DONE !
   f FRAME-TYPE @ {: t:^ :}
   ADVANCE {: kind:n :}
   kind JSON:T-ARR <> if
      c s" member results" RECORD-LINE
      s" an array of records" EXPECT kind JSON:KIND$ REJECT
   then
   JSON:WRITE-ARRAY-START c NEST-ENTITIES t PUSH ADVANCE ;

\ A V2 results object: its results array is written in its place.
: IN-RESULTS ( JSON:reader ^JSON:writer n ^copy ^frame -- JSON:reader ^JSON:writer n )
   {: kind:n c:^ f:^ :}
   kind JSON:T-OBJ-END = if
      f FRAME-DONE @ 0= if
         c s" a results object" RECORD-LINE
         s" member results" EXPECT s" none" REJECT
      then
      c CLOSE exit
   then
   swap s" results" JSON:READ-STR-EQ? {: hit:bool :} swap
   hit if c f RESULTS-ARRAY exit then
   SKIP-VALUE ADVANCE ;

\ V2's `__metadata` and every V4 member whose name holds `@`.
: DROPPED? ( ^entity-type ^u8 n -- bool )
   {: t:^ name:^ len:n :}
   t TYPE-V2? if name len s" __metadata" STR= exit then
   len 0 ?do name i + c@ [char] @ = if unloop true exit then loop
   false ;

: DATED? ( ^entity-type ^property -- bool )
   {: t:^ p:^ :}
   t TYPE-V2? 0= if false exit then
   p PROPERTY-TYPE @ SPAN:$ {: ty:^ tl:n :}
   ty tl s" Edm.DateTime" STR= ty tl s" Edm.DateTimeOffset" STR= or ;

86400000 constant MS-DAY
0 1 1 DATE:YMD>DAYS MS-DAY * constant MS-FIRST
10000 1 1 DATE:YMD>DAYS MS-DAY * constant MS-END

\ Ends in a sign and four digits, with ticks before them.
: ZONED? ( ^u8 n -- bool )
   {: a:^ u:n :}
   u 6 < if false exit then
   a u + 5 - c@ {: sign:n :}
   sign [char] + = sign [char] - = or 0= if false exit then
   a u + 4 - 4 STR-DIGITS? ;

\ The offset of `+mmmm` or `-mmmm` in milliseconds, 0 when there is none.
: ZONE-MS ( ^u8 n -- n )
   {: a:^ u:n :}
   a u ZONED? 0= if 0 exit then
   a u + 4 - 4 DIGITS MATCH option
      none OF 0 ENDOF
      some OF 60000 * ENDOF
   ;MATCH
   a u + 5 - c@ [char] - = if negate then ;

\ The UTC milliseconds of `ms`, `-ms`, `ms+mmmm` or `ms-mmmm`, the offset's
\ minutes taken off; none outside the years 0 to 9999.
: TICKS ( ^u8 n -- option<n> )
   {: a:^ u:n :}
   a u ZONED? if u 5 - else u then {: v:n :}
   v 0 > if a c@ [char] - = else false then {: neg:bool :}
   neg if 1 else 0 then {: lead:n :}
   a lead + v lead - DIGITS MATCH option
      none OF OPTION:NONE ENDOF
      some OF
         neg if negate then a u ZONE-MS - {: ms:n :}
         ms MS-FIRST >= ms MS-END < and if ms OPTION:SOME
         else OPTION:NONE then
      ENDOF
   ;MATCH ;

: MS ( ^u8 n -- option<n> )
   {: a:^ u:n :}
   a u s" /Date(" STARTS-WITH? a u s" )/" ENDS-WITH? and 0= if
      OPTION:NONE exit
   then
   s" /Date(" nip {: open:n :}
   a open + u open - 2 - TICKS ;

\ Quotient and remainder, the remainder in 0 to d - 1.
: FLOORED ( n n -- n n )
   {: n:n d:n :}
   n d mod dup 0 < if d + then {: r:n :}
   n r - d / r ;

\ ISO 8601 UTC: the time of day formatted on day 0, then the day over it.
: ISO ( n ^copy -- ^u8 n )
   {: ms:n c:^ :}
   c COPY-TEXT SPAN:$ drop {: out:^ :}
   ms MS-DAY FLOORED {: day:n rest:n :}
   rest 1000 / out DATE:TIME-LEN DATE:FORMAT-EPOCH-UTC 2drop
   day out DATE:LEN DATE:FORMAT-YMD 2drop
   rest 1000 mod {: milli:n :}
   DATE:TIME-LEN 1- {: zone:n :}
   milli 0= if out DATE:TIME-LEN exit then
   [char] . out zone + c!
   milli 3 out zone 1+ DATE:WIDTH!
   [char] Z out zone + 4 + c!
   out zone 5 + ;

: DATE-REJECT ( JSON:reader ^JSON:writer ^copy ^property ^u8 n -- JSON:reader ^JSON:writer )
   {: c:^ p:^ found:^ fl:n :}
   c s" member " RECORD-LINE p PROPERTY-NAME @ TEXT+
   s" /Date(ms)/ or null for " EXPECT p PROPERTY-TYPE @ TEXT+
   found fl REJECT ;

: DATE-MEMBER ( JSON:reader ^JSON:writer ^property ^copy -- JSON:reader ^JSON:writer )
   {: p:^ c:^ :}
   JSON:WRITE-COMMA p PROPERTY-NAME @ SPAN:$ JSON:WRITE-KEY
   ADVANCE {: kind:n :}
   kind JSON:T-NULL = if JSON:WRITE-NULL exit then
   kind JSON:T-STR <> if c p kind JSON:KIND$ DATE-REJECT then
   c DECODE {: text:^ len:n :}
   text len MS MATCH option
      none OF c p text len DATE-REJECT ENDOF
      some OF c ISO JSON:WRITE-STRING ENDOF
   ;MATCH ;

: NAV-KEY ( ^JSON:writer ^navigation -- ^JSON:writer )
   {: n:^ :}
   JSON:WRITE-COMMA n NAVIGATION-NAME @ SPAN:$ JSON:WRITE-KEY ;

: NAV-REJECT ( JSON:reader ^JSON:writer ^copy ^navigation n ^u8 n -- JSON:reader ^JSON:writer )
   {: c:^ n:^ kind:n want:^ wl:n :}
   c s" navigation " RECORD-LINE n NAVIGATION-NAME @ TEXT+
   want wl EXPECT kind JSON:KIND$ REJECT ;

\ A complex value: null, one record or, for a collection, an array of
\ records or a V2 results object.
: COMPLEX-MEMBER ( JSON:reader ^JSON:writer ^property ^copy -- JSON:reader ^JSON:writer n )
   {: p:^ c:^ :}
   p PROPERTY-COMPLEX-TYPE @ {: t:^ :}
   JSON:WRITE-COMMA p PROPERTY-NAME @ SPAN:$ JSON:WRITE-KEY
   ADVANCE {: kind:n :}
   kind JSON:T-NULL = if JSON:WRITE-NULL ADVANCE exit then
   p PROPERTY-MANY @ if
      kind JSON:T-ARR = if
         JSON:WRITE-ARRAY-START c NEST-ENTITIES t PUSH ADVANCE exit
      then
      kind JSON:T-OBJ = t TYPE-V2? and if c NEST-RESULTS t PUSH ADVANCE exit then
      c s" member " RECORD-LINE p PROPERTY-NAME @ TEXT+
      s" null or an array of records" EXPECT kind JSON:KIND$ REJECT
   then
   kind JSON:T-OBJ <> if
      c s" member " RECORD-LINE p PROPERTY-NAME @ TEXT+
      s" null or a record" EXPECT kind JSON:KIND$ REJECT
   then
   JSON:WRITE-OBJECT-START OPEN-AT {: open:^ :} c t open PUSH-ENTITY ADVANCE ;

\ A V2 navigation's object: deferred, a results object or one record.
: V2-NAV-OBJECT ( JSON:reader ^JSON:writer ^navigation ^copy -- JSON:reader ^JSON:writer n )
   {: n:^ c:^ :}
   n NAVIGATION-TARGET-TYPE @ {: t:^ :}
   OPEN-AT {: open:^ :}
   ADVANCE {: first:n :}
   first JSON:T-KEY = if
      swap s" __deferred" JSON:READ-STR-EQ? {: deferred:bool :} swap
      deferred if SKIP-REST ADVANCE exit then
   then
   n NAV-KEY
   n TO-MANY? if c NEST-RESULTS t PUSH first exit then
   JSON:WRITE-OBJECT-START c t open PUSH-ENTITY first ;

: NAV-MEMBER ( JSON:reader ^JSON:writer ^navigation ^copy -- JSON:reader ^JSON:writer n )
   {: n:^ c:^ :}
   n NAVIGATION-TARGET-TYPE @ {: t:^ :}
   ADVANCE {: kind:n :}
   kind JSON:T-NULL = if n NAV-KEY JSON:WRITE-NULL ADVANCE exit then
   kind JSON:T-ARR = if
      n NAV-KEY JSON:WRITE-ARRAY-START c NEST-ENTITIES t PUSH ADVANCE exit
   then
   kind JSON:T-OBJ <> if
      c n kind s" null, a record or an array of records" NAV-REJECT
   then
   t TYPE-V2? if n c V2-NAV-OBJECT exit then
   n TO-MANY? if c n kind s" an array of records" NAV-REJECT then
   n NAV-KEY JSON:WRITE-OBJECT-START
   OPEN-AT {: open:^ :} c t open PUSH-ENTITY ADVANCE ;

\ On the record's `}`: its names are checked and dropped.
: ONCE ( JSON:reader ^JSON:writer ^copy -- JSON:reader ^JSON:writer )
   {: c:^ :}
   c TOP {: f:^ :}
   c REPEATED? 0= if f FRAME-NAMES @ c COPY-NAMES-USED ! exit then
   s" response record" c COPY-START @ f FRAME-OPEN @ OFFSET LINE-AT
   s" a record" LOG:TEXT+
   s" each member once" EXPECT s" a member repeated" REJECT ;

: IN-RECORD ( JSON:reader ^JSON:writer n ^copy ^entity-type -- JSON:reader ^JSON:writer n )
   {: kind:n c:^ t:^ :}
   kind JSON:T-OBJ-END = if
      c ONCE JSON:WRITE-OBJECT-END c CLOSE exit
   then
   c DECODE {: name:^ len:n :}
   c name len ADD-NAME
   t name len DROPPED? if SKIP-VALUE ADVANCE exit then
   t name len FIND-PROPERTY {: p:^ :}
   p NULL-PTR <> if t p DATED? else false then if
      p c DATE-MEMBER ADVANCE exit
   then
   p NULL-PTR <> if p PROPERTY-COMPLEX-TYPE @ NULL-PTR <> else false then if
      p c COMPLEX-MEMBER exit
   then
   t name len FIND-NAVIGATION {: n:^ :}
   n NULL-PTR <> if n c NAV-MEMBER exit then
   JSON:WRITE-COMMA name len JSON:WRITE-KEY ADVANCE c PLAIN ADVANCE ;

\ The innermost frame handles the token and answers the next one.
: HANDLE ( JSON:reader ^JSON:writer n ^copy -- JSON:reader ^JSON:writer n )
   {: kind:n c:^ :}
   c TOP {: f:^ :}
   f FRAME-TYPE @ {: t:^ :}
   kind f FRAME-NEST @ MATCH nest
      entity OF c t IN-RECORD ENDOF
      entities OF c t IN-RECORDS ENDOF
      results OF c f IN-RESULTS ENDOF
      object OF c IN-OBJECT ENDOF
      array OF c IN-ARRAY ENDOF
   ;MATCH ;

: WALK ( JSON:reader ^JSON:writer ^copy -- JSON:reader ^JSON:writer )
   {: c:^ :}
   ADVANCE
   begin c COPY-DEPTH @ 0 > while c HANDLE repeat drop ;

: COPY-IN ( JSON:reader ^JSON:writer ^u8 ^entity-type ^u8 n -- JSON:reader ^JSON:writer )
   {: room:^ t:^ dst:^ cap:n :}
   room >COPY {: c:^ :}
   room COPY-CELLS cells + c COPY-FRAMES !
   FIRST-FRAMES c COPY-ROOM !
   0 c COPY-DEPTH !
   room 0 SPAN:MAKE c COPY-MAPPING !
   room COPY-CELLS FIRST-FRAMES FRAME-CELLS * + cells + c COPY-NAMES !
   FIRST-NAMES c COPY-NAMES-ROOM !
   0 c COPY-NAMES-USED !
   room 0 SPAN:MAKE c COPY-NAMES-MAPPING !
   dst c COPY-DST ! cap c COPY-CAP !
   swap JSON:READ-TOKEN {: kind:n :}
   kind JSON:T-OBJ <> if E-JSON-READ-STATE throw then
   JSON:READ-SPAN$ drop c COPY-START ! swap
   JSON:WRITE-OBJECT-START
   c t c COPY-START @ PUSH-ENTITY
   [: c WALK ;] [: c DROP-FRAMES ;] finally ;

public

\ The records array of a collection response, the next link's length
\ written into SPAN, 0 for none, and the count, none when absent.
: PAGE ( ^metadata ^u8 n ^JSON:cursor SPAN:span<u8> -- ^u8 n n option<n> )
   {: m:^ body:^ len:n cur:^ link:SPAN:span<u8> :}
   ENVELOPE-CELLS cells MEM:BYTES-ALLOC-LEN
   [: drop m body len cur link PAGE-IN ;] MEM:WITH-BYTES ;

\ The record of a single-entity response.
: RECORD$ ( ^metadata ^u8 n ^JSON:cursor -- ^u8 n )
   {: m:^ body:^ len:n cur:^ :}
   ENVELOPE-CELLS cells MEM:BYTES-ALLOC-LEN
   [: drop m body len cur RECORD-IN ;] MEM:WITH-BYTES ;

\ The plain form of one record of the type: the reader is on its `{` and
\ ends on its `}`; the span is scratch for one decoded string or key.
: COPY-ENTITY ( JSON:reader ^JSON:writer ^entity-type ^u8 n -- JSON:reader ^JSON:writer )
   {: t:^ dst:^ cap:n :}
   COPY-CELLS FIRST-FRAMES FRAME-CELLS * + cells FIRST-NAMES +
   MEM:BYTES-ALLOC-LEN
   [: drop t dst cap COPY-IN ;] MEM:WITH-BYTES ;

;package

\ record.f - immutable schema-tagged records over the pools' pages, for the
\ browser runtime (docs/browser-runtime.md §3.1, §6, §7.1).
\
\ STORAGE CLASS. POOL-OWNED for records: a record and the chunks of its values
\ are objects of the page pool of the pools that built them, and RT-POOL's
\ reference counts hold them. PROCESS-WIDE for schemas: SCHEMA adds to one
\ registry for the image while files load, and every set of pools reads it.
\
\ A schema is an Id128, a version and its fields: ints, then floats, then
\ references, then values, the ordinals running from 0 in that order. No two
\ schemas share an id and a version. A record is one page: a header naming its
\ schema and whether it is sealed, then one cell per field, zero until written.
\
\ BUILD reserves a page and answers a builder, a linear token, so the checker
\ never lets a caller copy or drop one. INT!, FLOAT!, REF! and VALUE! write a
\ field by its ordinal. SEAL consumes the builder and answers the record, held
\ once and frozen; ABORT consumes it and releases the page. No word writes
\ through a record, and every word that takes a builder refuses one whose
\ record SEAL has sealed, which only a copy the checker never saw reaches.
\ REF! takes a record, which only SEAL answers, so a record references only
\ records sealed before it and its owning edges form a DAG by construction. A
\ refused write leaves the builder's fields unchanged and the builder where it
\ was, so a caller that catches it by name keeps the builder (docs/forth.md,
\ "A LINEAR handle cannot cross a quotation-literal catch"); a VALUE! the
\ pools refuse partway, building its chunks or holding them in the field, has
\ queued the chunks it built for RECLAIM-STEP, and the pools charge them until
\ it reclaims them.
\
\ A value is bytes. VALUE! copies them into a chain of chunks built from its
\ end, so that each chunk references only a chunk already complete. A chunk
\ holds CHUNK-BYTES of them, so a value larger than a page spans several.
\ CURSOR opens a value field for reading, and each READ copies at most what is
\ left of one chunk. A cursor holds no reference: read it while its record is
\ held. A read names the reader's schema and is refused unless the record was
\ built under it, by E-RT-RECORD-VERSION for another version of the same id
\ and by E-RT-RECORD-FOREIGN for another id. A float field refuses NaN (§6).
\ RECLAIM-STEP releases a record's references and values when it reclaims the
\ record.
\
\ BOUNDARY. Outside the package no typed route leads from a schema, a builder
\ or a cursor to its handle or cells, and none from a number to a record, a
\ schema, a builder or a cursor: the converters of records and schemas are
\ private and a cast from a number to either is E-CAST-OWNER; a builder is
\ minted and erased only by the private LINEAR: pair beside its generation
\ check, and CAST: refuses it (E-CAST-LINEAR); a cursor's parts are a private
\ type, so the public MAKE and UNMAKE of a cursor only take cursors apart and
\ put parts back together, two cursors' parts crossed included, which READ
\ refuses once the place passes its chunk. A record, though, is a number to
\ any package's `CAST: ( RT-RECORD:record -- n )`, and RT-HANDLE:HANDLE makes
\ the handle of its page from that number's halves, through which RT-POOL's
\ words reach the record and the chunks of its values. They refuse to write
\ either (E-RT-POOL-FROZEN): CHAIN freezes each chunk as soon as it is
\ complete, after its bytes, its length and its next chunk, and SEAL freezes
\ the record. So no route, checked or typed, writes a sealed record or its
\ values without a private pointer mint. Raw storage passes them all: a private
\ pointer mint (docs/forth.md, CAST:), such as any package's own
\ `CAST: ( ptr RT-RECORD:record -- ptr n )`, reads and writes a record held in
\ storage as a number, and the public byte and cell views over a cell that
\ holds one store any number there, a forged record included. A handle names
\ no set of pools, so a record or builder is given only with the pools that
\ built it: the runtime context holds one set. What follows holds while only
\ this package's words make records and builders and each is given with its
\ own pools.

require lib/errors.f
require lib/ieee754.f
require lib/type/deftype.f
require lib/runtime/id.f
require lib/runtime/handle.f
require lib/runtime/pool.f

package RT-RECORD

private

DEFTYPE CURSOR-PART

public

\ The decade -9530..-9539.
-9530 constant E-RT-RECORD-FIRST
-9539 constant E-RT-RECORD-LAST
-9530 constant E-RT-RECORD-NULL     \ the zero record token where a word reads through it
-9531 constant E-RT-RECORD-SCHEMA   \ the zero schema token, SCHEMA with every schema taken, or a declaration with a count below 0, more fields than a record holds, a version outside 0 .. 2^32 - 1 or an id and version declared before
-9532 constant E-RT-RECORD-FOREIGN  \ a read of a record whose schema's id is not the reader's
-9533 constant E-RT-RECORD-VERSION  \ a read of a record built under another version of the reader's schema
-9534 constant E-RT-RECORD-FIELD    \ an ordinal that is not a field of the word's kind
-9535 constant E-RT-RECORD-NAN      \ FLOAT! of a NaN
-9536 constant E-RT-RECORD-SEALED   \ a builder whose record SEAL has sealed, which only an unchecked copy reaches
-9537 constant E-RT-RECORD-OOM      \ BUILD or VALUE! when the pools answer oom
-9538 constant E-RT-RECORD-SPAN     \ VALUE! or READ of a negative length or a null address with bytes, VALUE! of a length above 2^63 - CHUNK-BYTES, or READ of a cursor past its chunk

DEFTYPE RECORD
undefine >RECORD
undefine RECORD>N

DEFTYPE SCHEMA
undefine >SCHEMA
undefine SCHEMA>N

DEFLINEAR RT-RECORD:builder

STRUCTURE cursor 0
   FIELD chunk cursor-part
   FIELD at cursor-part
;STRUCTURE

private

\ A schema's id and version, then where each kind's ordinals end: the ints
\ before INTS, the floats before FLOATS, the references before REFS and the
\ values before VALUES, the schema's field count.
STRUCTURE entry 0 DERIVE addr
   FIELD id RT-ID:id128
   FIELD version n
   FIELD ints n
   FIELD floats n
   FIELD refs n
   FIELD values n
;STRUCTURE

CAST: >RECORD ( n -- record )
CAST: RECORD>N ( record -- n )
CAST: >SCHEMA ( n -- schema )
CAST: SCHEMA>N ( schema -- n )

$FFFFFFFF constant U32-MAX
$7FFFFFFFFFFFFFFF constant MAX-N
\ A header holds its record's schema number in bits 0-31, and SEALED once SEAL
\ has sealed the record.
$100000000 constant SEALED
\ A record's header is its payload cell 0 and its field 0 is cell 1. A chunk's
\ next chunk is its cell 0, the value's bytes from it to the end are its cell
\ 1, and its own bytes start at cell 2.
1 constant FIELD-BASE
1 constant LEFT-CELL
2 constant DATA-BASE

RT--POOL-KIND:pages RT-POOL:PAYLOAD-CELLS constant PAGE-CELLS

public

PAGE-CELLS FIELD-BASE - constant MAX-FIELDS
PAGE-CELLS DATA-BASE - cells constant CHUNK-BYTES
256 constant MAX-SCHEMAS

private

\ ---- schemas ----------------------------------------------------------------

MAX-SCHEMAS TYPED-BUFFER ENTRIES entry
variable SCHEMAS

\ The entry of the schema numbered n, from 1.
: ENTRY ( n -- ptr entry )
   1 - ENTRIES ;

\ The schema's number, refusing the zero token and any number SCHEMA has not
\ answered before the registry is read.
: SCHEMA# ( schema -- n )
   SCHEMA>N {: s:n :}
   s 1 < s SCHEMAS @ > or if E-RT-RECORD-SCHEMA throw then
   s ;

: COUNT? ( n -- bool )
   {: c:n :}
   c 0 >= c MAX-FIELDS <= and ;

\ Whether a schema has the id and version.
: DECLARED? ( RT-ID:id128 n -- bool )
   {: id:RT-ID:id128 v:n :}
   false
   SCHEMAS @ 0 ?do
      i ENTRIES {: en:ptr :}
      en ENTRY-ID @ id RT-ID:EQUAL? en ENTRY-VERSION @ v = and or
   loop ;

public

\ A schema of the id and version whose fields are `ints` ints, then `floats`
\ floats, then `refs` references, then `values` values: a version of 0 ..
\ 2^32 - 1, no count below 0 and at most MAX-FIELDS fields.
: SCHEMA ( RT-ID:id128 n n n n n -- schema )
   {: id:RT-ID:id128 v:n ints:n floats:n refs:n values:n :}
   v 0 < v U32-MAX > or if E-RT-RECORD-SCHEMA throw then
   ints COUNT? floats COUNT? and refs COUNT? and values COUNT? and 0= if E-RT-RECORD-SCHEMA throw then
   ints floats + refs + values + MAX-FIELDS > if E-RT-RECORD-SCHEMA throw then
   id v DECLARED? if E-RT-RECORD-SCHEMA throw then
   SCHEMAS @ {: s:n :}
   s MAX-SCHEMAS >= if E-RT-RECORD-SCHEMA throw then
   ints floats + {: fe:n :}
   fe refs + {: re:n :}
   id v ints fe re re values + ENTRY-MAKE s ENTRIES !
   s 1 + SCHEMAS !
   s 1 + >SCHEMA ;

private

\ ---- records and builders -----------------------------------------------------

TYPED-VARIABLE RECORDS RT-POOL:schema
TYPED-VARIABLE CHUNKS RT-POOL:schema

\ The handle whose bits these are.
: BITS>HANDLE ( n -- RT-HANDLE:handle )
   {: v:n :}
   v U32-MAX and v 32 rshift RT-HANDLE:HANDLE ;

\ The bits of this handle, which BITS>HANDLE takes back.
: HANDLE>BITS ( RT-HANDLE:handle -- n )
   {: h:RT-HANDLE:handle :}
   h RT-HANDLE:GENERATION 32 lshift h RT-HANDLE:SLOT or ;

\ A payload cell of the object whose bits these are.
: AT@ ( n n RT-POOL:pools -- n )
   {: v:n c:n p:RT-POOL:pools :}
   v BITS>HANDLE c p RT-POOL:CELL@ ;

: AT! ( n n n RT-POOL:pools -- )
   {: x:n v:n c:n p:RT-POOL:pools :}
   x v BITS>HANDLE c p RT-POOL:CELL! ;

\ The bits of the object a reference cell holds, 0 when it holds none.
: REF-BITS ( n n RT-POOL:pools -- n )
   {: v:n c:n p:RT-POOL:pools :}
   v BITS>HANDLE c p RT-POOL:REF@ HANDLE>BITS ;

\ The record's bits, refusing the zero token before anything reads through it.
: BITS-OF ( record -- n )
   RECORD>N {: v:n :}
   v 0= if E-RT-RECORD-NULL throw then
   v ;

\ The payload cell of ordinal o, refused unless lo <= o < hi.
: FIELD-CELL ( idx n n -- n )
   {: o:idx lo:n hi:n :}
   o IDX>N {: j:n :}
   j lo < j hi >= or if E-RT-RECORD-FIELD throw then
   j FIELD-BASE + ;

LINEAR: >BUILDER ( n -- RT-RECORD:builder )
LINEAR: BUILDER> ( RT-RECORD:builder -- n )

\ The generation check: the bits of the builder's record and the entry of its
\ schema. RT-POOL refuses a page whose generation is no longer the builder's,
\ and a record SEAL has sealed is refused here; either refusal leaves the
\ builder where it was.
: OPENED ( RT-RECORD:builder RT-POOL:pools -- RT-RECORD:builder n ptr entry )
   {: p:RT-POOL:pools :}
   BUILDER> dup >BUILDER swap {: v:n :}
   v 0 p AT@ {: hd:n :}
   hd SEALED and 0<> if E-RT-RECORD-SEALED throw then
   v hd U32-MAX and ENTRY ;

\ The bits of a NaN: every exponent bit set and a fraction.
$7FF0000000000000 constant EXPONENT
$000FFFFFFFFFFFFF constant FRACTION

: NAN? ( n -- bool )
   {: b:n :}
   b EXPONENT and EXPONENT = b FRACTION and 0<> and ;

public

\ A builder of a record of the schema, its fields zero: a page of the pools.
: BUILD ( schema RT-POOL:pools -- RT-RECORD:builder )
   {: sc:schema p:RT-POOL:pools :}
   sc SCHEMA# {: s:n :}
   RECORDS @ p RT-POOL:RESERVE
   MATCH RT-POOL:reservation
      granted OF ENDOF
      oom OF E-RT-RECORD-OOM throw ENDOF
   ;MATCH
   HANDLE>BITS {: v:n :}
   s v 0 p AT!
   v >BUILDER ;

\ Write the int field of ordinal o.
: INT! ( RT-RECORD:builder n idx RT-POOL:pools -- RT-RECORD:builder )
   {: x:n o:idx p:RT-POOL:pools :}
   p OPENED {: v:n en:ptr :}
   o 0 en ENTRY-INTS @ FIELD-CELL {: c:n :}
   x v c p AT! ;

\ Write the float field of ordinal o. A NaN is refused (§6).
: FLOAT! ( RT-RECORD:builder r idx RT-POOL:pools -- RT-RECORD:builder )
   {: x:r o:idx p:RT-POOL:pools :}
   x IEEE754:F64>BITS {: b:n :}
   b NAN? if E-RT-RECORD-NAN throw then
   p OPENED {: v:n en:ptr :}
   o en ENTRY-INTS @ en ENTRY-FLOATS @ FIELD-CELL {: c:n :}
   b v c p AT! ;

\ Hold the record in the reference field of ordinal o, releasing the one it
\ replaces.
: REF! ( RT-RECORD:builder record idx RT-POOL:pools -- RT-RECORD:builder )
   {: r:record o:idx p:RT-POOL:pools :}
   r BITS-OF {: rv:n :}
   p OPENED {: v:n en:ptr :}
   o en ENTRY-FLOATS @ en ENTRY-REFS @ FIELD-CELL {: c:n :}
   rv BITS>HANDLE v BITS>HANDLE c p RT-POOL:REF! ;

\ The builder's record, sealed, frozen and held once.
: SEAL ( RT-RECORD:builder RT-POOL:pools -- record )
   {: p:RT-POOL:pools :}
   p OPENED drop {: v:n :}
   v 0 p AT@ SEALED or v 0 p AT!
   v BITS>HANDLE p RT-POOL:FREEZE
   BUILDER> >RECORD ;

\ Release the builder's record unsealed, for RECLAIM-STEP to reclaim with what
\ its fields hold.
: ABORT ( RT-RECORD:builder RT-POOL:pools -- )
   {: p:RT-POOL:pools :}
   p OPENED drop {: v:n :}
   BUILDER> drop
   v BITS>HANDLE p RT-POOL:RELEASE ;

private

\ ---- values -------------------------------------------------------------------

\ Refuse a negative length, and bytes at the null address.
: SPAN ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 < if E-RT-RECORD-SPAN throw then
   u 0 > a 0= and if E-RT-RECORD-SPAN throw then ;

\ The k bytes at the address, k at most 8, as one cell, the first byte lowest.
: PACK ( ptr u8 n -- n )
   {: a:ptr k:n :}
   0 k 0 ?do a i + c@ i 8 * lshift or loop ;

\ Copy the k bytes at the address into the chunk's bytes.
: FILL ( ptr u8 n n RT-POOL:pools -- )
   {: a:ptr k:n v:n p:RT-POOL:pools :}
   k 7 + 8 / 0 ?do
      a i 8 * + k i 8 * - 8 min PACK v i DATA-BASE + p AT!
   loop ;

\ A chunk, held once. When the pools answer oom, the chain built so far, whose
\ head these bits are (0 for none), is released and E-RT-RECORD-OOM thrown.
: CHUNK ( n RT-POOL:pools -- n )
   {: next:n p:RT-POOL:pools :}
   CHUNKS @ p RT-POOL:RESERVE
   MATCH RT-POOL:reservation
      granted OF HANDLE>BITS ENDOF
      oom OF
         next 0<> if next BITS>HANDLE p RT-POOL:RELEASE then
         E-RT-RECORD-OOM throw
      ENDOF
   ;MATCH ;

\ The span's bytes as a chain of chunks built from its end, each chunk holding
\ the next and frozen once complete: the head's bits, held once, or 0 for no
\ bytes.
: CHAIN ( ptr u8 n RT-POOL:pools -- n )
   {: a:ptr u:n p:RT-POOL:pools :}
   u CHUNK-BYTES 1 - + CHUNK-BYTES / {: k:n :}
   0
   k 0 ?do
      {: next:n :}
      k 1 - i - CHUNK-BYTES * {: off:n :}
      next p CHUNK {: v:n :}
      a off + u off - CHUNK-BYTES min v p FILL
      u off - v LEFT-CELL p AT!
      next 0<> if
         next BITS>HANDLE v BITS>HANDLE 0 p RT-POOL:REF!
         next BITS>HANDLE p RT-POOL:RELEASE
      then
      v BITS>HANDLE p RT-POOL:FREEZE
      v
   loop ;

public

\ Copy the bytes into the value field of ordinal o, releasing the value it
\ replaces. When the pools run out partway, the chunks built so far are
\ released, the field keeps what it held and E-RT-RECORD-OOM is thrown. The
\ chain's own hold is released whether the field takes the chain or RT-POOL
\ refuses it, whose code is thrown again. A length CHAIN cannot round up to
\ whole chunks within a cell is refused.
: VALUE! ( RT-RECORD:builder ptr u8 n idx RT-POOL:pools -- RT-RECORD:builder )
   {: a:ptr u:n o:idx p:RT-POOL:pools :}
   a u SPAN
   u MAX-N CHUNK-BYTES 1 - - > if E-RT-RECORD-SPAN throw then
   p OPENED {: v:n en:ptr :}
   o en ENTRY-REFS @ en ENTRY-VALUES @ FIELD-CELL {: c:n :}
   a u p CHAIN {: head:n :}
   head BITS>HANDLE v BITS>HANDLE c p [: 2over 2over RT-POOL:REF! ;] catch
   {: code:n :} 2drop 2drop
   head 0<> if head BITS>HANDLE p RT-POOL:RELEASE then
   code 0<> if code throw then ;

private

\ ---- reading ------------------------------------------------------------------

\ The bits of a record built under the reader's schema, and that schema's
\ entry. A record of another version of the schema's id is refused by
\ E-RT-RECORD-VERSION, and one of another id by E-RT-RECORD-FOREIGN.
: READABLE ( record schema RT-POOL:pools -- n ptr entry )
   {: r:record sc:schema p:RT-POOL:pools :}
   r BITS-OF {: v:n :}
   sc SCHEMA# {: want:n :}
   v 0 p AT@ U32-MAX and {: got:n :}
   got want <> if
      got ENTRY ENTRY-ID @ want ENTRY ENTRY-ID @ RT-ID:EQUAL? if
         E-RT-RECORD-VERSION throw
      then
      E-RT-RECORD-FOREIGN throw
   then
   v want ENTRY ;

\ The bytes of the chunk, at most CHUNK-BYTES of those left from it.
: CHUNK-LENGTH ( n RT-POOL:pools -- n )
   {: v:n p:RT-POOL:pools :}
   v LEFT-CELL p AT@ CHUNK-BYTES min ;

\ Copy the chunk's k bytes from byte `at` on to the address.
: TAKE ( n n n ptr u8 RT-POOL:pools -- )
   {: v:n at:n k:n dst:ptr p:RT-POOL:pools :}
   0
   k 0 ?do
      at i + {: q:n :}
      q 7 and 0= i 0= or if drop v q 3 rshift DATA-BASE + p AT@ then
      dup q 7 and 3 lshift rshift $FF and dst i + c!
   loop
   drop ;

public

\ Whether the record is the zero token, which REF@ answers for an empty field.
: NULL? ( record -- bool )
   RECORD>N 0= ;

: RETAIN ( record RT-POOL:pools -- )
   {: r:record p:RT-POOL:pools :}
   r BITS-OF BITS>HANDLE p RT-POOL:RETAIN ;

\ The last release leaves the record to RECLAIM-STEP, which releases what its
\ fields hold.
: RELEASE ( record RT-POOL:pools -- )
   {: r:record p:RT-POOL:pools :}
   r BITS-OF BITS>HANDLE p RT-POOL:RELEASE ;

\ The int field of ordinal o.
: INT@ ( record schema idx RT-POOL:pools -- n )
   {: r:record sc:schema o:idx p:RT-POOL:pools :}
   r sc p READABLE {: v:n en:ptr :}
   v o 0 en ENTRY-INTS @ FIELD-CELL p AT@ ;

\ The float field of ordinal o.
: FLOAT@ ( record schema idx RT-POOL:pools -- r )
   {: r:record sc:schema o:idx p:RT-POOL:pools :}
   r sc p READABLE {: v:n en:ptr :}
   v o en ENTRY-INTS @ en ENTRY-FLOATS @ FIELD-CELL p AT@ IEEE754:BITS>F64 ;

\ The record the reference field of ordinal o holds, the zero token when it is
\ empty. The parent holds it: RETAIN it to keep it past the parent.
: REF@ ( record schema idx RT-POOL:pools -- record )
   {: r:record sc:schema o:idx p:RT-POOL:pools :}
   r sc p READABLE {: v:n en:ptr :}
   v o en ENTRY-FLOATS @ en ENTRY-REFS @ FIELD-CELL p REF-BITS >RECORD ;

\ A cursor at the first byte of the value field of ordinal o.
: CURSOR ( record schema idx RT-POOL:pools -- cursor )
   {: r:record sc:schema o:idx p:RT-POOL:pools :}
   r sc p READABLE {: v:n en:ptr :}
   v o en ENTRY-REFS @ en ENTRY-VALUES @ FIELD-CELL p REF-BITS {: head:n :}
   head >CURSOR-PART 0 >CURSOR-PART RT--RECORD-CURSOR:MAKE ;

\ Copy at most k bytes of the value from the cursor to the address, and no more
\ than its chunk has left: the cursor past them and how many, which is 0 for a
\ positive k only at the value's end.
: READ ( cursor ptr u8 n RT-POOL:pools -- cursor n )
   {: c:cursor dst:ptr k:n p:RT-POOL:pools :}
   dst k SPAN
   c RT--RECORD-CURSOR:UNMAKE {: ch:cursor-part at:cursor-part :}
   ch CURSOR-PART>N {: v:n :}
   at CURSOR-PART>N {: off:n :}
   v 0= if c 0 exit then
   v p CHUNK-LENGTH {: len:n :}
   off len >= if E-RT-RECORD-SPAN throw then
   len off - k min {: t:n :}
   v off t dst p TAKE
   off t + len = if v 0 p REF-BITS 0 else v off t + then
   {: nv:n noff:n :}
   nv >CURSOR-PART noff >CURSOR-PART RT--RECORD-CURSOR:MAKE t ;

private

\ ---- reclaiming ---------------------------------------------------------------

\ A record's child at an edge: each record and value its fields hold, in
\ ordinal order and the empty fields skipped, then the null handle.
: RECORD-EDGE ( RT-HANDLE:handle n RT-POOL:pools -- RT-HANDLE:handle )
   {: h e:n p:RT-POOL:pools :}
   h 0 p RT-POOL:CELL@ U32-MAX and ENTRY {: en:ptr :}
   e
   en ENTRY-VALUES @ FIELD-BASE + en ENTRY-FLOATS @ FIELD-BASE + ?do
      h i p RT-POOL:REF@ {: c:RT-HANDLE:handle :}
      c RT-HANDLE:NULL? 0= if
         dup 0= if drop c unloop exit then
         1 -
      then
   loop
   drop RT-HANDLE:NULL ;

\ A chunk's child: the next chunk, then the null handle.
: CHUNK-EDGE ( RT-HANDLE:handle n RT-POOL:pools -- RT-HANDLE:handle )
   {: h e:n p:RT-POOL:pools :}
   e 0<> if RT-HANDLE:NULL exit then
   h 0 p RT-POOL:REF@ ;

: REGISTER ( -- )
   RT--POOL-KIND:pages [: RECORD-EDGE ;] RT-POOL:SCHEMA RECORDS !
   RT--POOL-KIND:pages [: CHUNK-EDGE ;] RT-POOL:SCHEMA CHUNKS ! ;

REGISTER

;package

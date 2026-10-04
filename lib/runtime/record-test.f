\ record-test.f - RT-RECORD through its load path: schemas declared once per
\ id and version, each kind of field written by ordinal and read back, a
\ builder whose fields no refused write changes, NaN refused in a float
\ field, a read under another version or schema refused by code, a 1 MiB
\ value round-tripped through chunks by a bounded cursor, records reclaimed
\ with their children through RECLAIM-STEP, a sealed record and its value's
\ chunks that RT-POOL refuses to write through a handle rebuilt from the
\ record's bits, a value refused for a builder whose page FREEZE froze that
\ leaves nothing charged, oom and lengths past what a cell counts that leave the
\ builder, and the typed routes each token refuses, the duplicated and the
\ dropped builder among them. The white-box rows at the end drive what only a
\ copy of a builder the checker never saw reaches, and a full registry.
\ Run: bin/hb --load lib/runtime/record-test.f

require lib/errors.f
require lib/ieee754.f
require lib/string.f
require lib/test.f
require lib/test/outcome.f
require lib/runtime/id.f
require lib/runtime/handle.f
require lib/runtime/pool.f
require lib/runtime/record.f

package RRT

\ ---- fixtures ---------------------------------------------------------------

16 BUFFER: ID-BYTES

\ The Id128 whose bytes are zero but for the last, which holds v.
: ID ( n -- RT-ID:id128 )
   {: v:n :}
   16 0 do 0 ID-BYTES i + c! loop
   v ID-BYTES 15 + c!
   ID-BYTES 16 RT-ID:BYTES>ID128 ;

: SCHEMA-CODE ( -- n )
   RT-RECORD:E-RT-RECORD-SCHEMA ;

: FIELD-CODE ( -- n )
   RT-RECORD:E-RT-RECORD-FIELD ;

: SPAN-CODE ( -- n )
   RT-RECORD:E-RT-RECORD-SPAN ;

: NULL-CODE ( -- n )
   RT-RECORD:E-RT-RECORD-NULL ;

: BITS ( r -- n )
   IEEE754:F64>BITS ;

RT-POOL:POOLS-CELLS TYPED-BUFFER CELLS-A n
TYPED-VARIABLE PA RT-POOL:pools

\ The pages the pools charge.
: PAGES ( -- n )
   RT--POOL-KIND:pages PA @ RT-POOL:CHARGED 4096 / ;

\ Reclaim every record and chunk released for good: how many edges that took.
: RECLAIM ( -- n )
   0 begin 1 PA @ RT-POOL:RECLAIM-STEP dup 0<> while + repeat drop ;

\ ---- schemas ----------------------------------------------------------------

\ PART: ints 0 and 1, float 2, references 3 and 4 and value 5. PART2 is
\ version 2 of PART's id with the same fields. LEAF has one int, BLOB one value
\ and TREE three references and a value.
TYPED-VARIABLE PART RT-RECORD:schema
TYPED-VARIABLE PART2 RT-RECORD:schema
TYPED-VARIABLE LEAF RT-RECORD:schema
TYPED-VARIABLE BLOB RT-RECORD:schema
TYPED-VARIABLE TREE RT-RECORD:schema
TYPED-VARIABLE WIDE RT-RECORD:schema
\ Never written: the zero schema and record tokens.
TYPED-VARIABLE NO-SCHEMA RT-RECORD:schema
TYPED-VARIABLE NO-RECORD RT-RECORD:record

: SCHEMAS ( -- )
   1 ID 1 2 1 2 1 RT-RECORD:SCHEMA PART !
   1 ID 2 2 1 2 1 RT-RECORD:SCHEMA PART2 !
   2 ID 1 1 0 0 0 RT-RECORD:SCHEMA LEAF !
   3 ID 1 0 0 0 1 RT-RECORD:SCHEMA BLOB !
   4 ID 1 0 0 3 1 RT-RECORD:SCHEMA TREE ! ;

SCHEMAS

: DECLARATIONS ( -- )
   s" a version outside 0 .. 2^32 - 1 is refused" T-LABEL
   [: 9 ID -1 1 0 0 0 RT-RECORD:SCHEMA drop ;] SCHEMA-CODE TTHROWSQ
   [: 9 ID $100000000 1 0 0 0 RT-RECORD:SCHEMA drop ;] SCHEMA-CODE TTHROWSQ
   s" and so is a count below 0, of each kind" T-LABEL
   [: 9 ID 1 -1 1 0 0 RT-RECORD:SCHEMA drop ;] SCHEMA-CODE TTHROWSQ
   [: 9 ID 1 1 -1 0 0 RT-RECORD:SCHEMA drop ;] SCHEMA-CODE TTHROWSQ
   [: 9 ID 1 0 1 -1 0 RT-RECORD:SCHEMA drop ;] SCHEMA-CODE TTHROWSQ
   [: 9 ID 1 0 0 1 -1 RT-RECORD:SCHEMA drop ;] SCHEMA-CODE TTHROWSQ
   s" and more fields than a record holds" T-LABEL
   [: 9 ID 1 RT-RECORD:MAX-FIELDS 0 0 1 RT-RECORD:SCHEMA drop ;] SCHEMA-CODE TTHROWSQ
   [: 9 ID 1 0 0 1 RT-RECORD:MAX-FIELDS RT-RECORD:SCHEMA drop ;] SCHEMA-CODE TTHROWSQ
   s" counted without wrapping" T-LABEL
   [: 9 ID 1 $4000000000000000 dup dup dup RT-RECORD:SCHEMA drop ;] SCHEMA-CODE TTHROWSQ
   s" an id and version declared before are refused" T-LABEL
   [: 1 ID 1 0 0 0 0 RT-RECORD:SCHEMA drop ;] SCHEMA-CODE TTHROWSQ
   [: 1 ID 2 2 1 2 1 RT-RECORD:SCHEMA drop ;] SCHEMA-CODE TTHROWSQ
   s" while MAX-FIELDS fields at version 2^32 - 1 are declared" T-LABEL
   9 ID $FFFFFFFF RT-RECORD:MAX-FIELDS 0 0 0 RT-RECORD:SCHEMA WIDE ! ;

\ ---- values -----------------------------------------------------------------

$100000 constant MIB
MIB BUFFER: SRC
MIB BUFFER: DST
variable READS
variable LONGEST

\ SRC holds a byte sequence that repeats nowhere a chunk boundary could hide.
: FILL-SRC ( -- )
   12345 MIB 0 do 1103515245 * 12345 + dup 16 rshift $FF and SRC i + c! loop drop ;

FILL-SRC

\ Read the value field of ordinal o into DST, at most k bytes a READ, until a
\ READ answers 0: how many bytes it held. READS counts the READs that answered
\ bytes and LONGEST is the most one answered.
: READ-VALUE ( RT-RECORD:record RT-RECORD:schema n n -- n )
   {: r:RT-RECORD:record sc:RT-RECORD:schema o:n k:n :}
   0 READS ! 0 LONGEST !
   r sc o >IDX PA @ RT-RECORD:CURSOR
   0
   begin
      {: c:RT-RECORD:cursor got:n :}
      c DST got + k PA @ RT-RECORD:READ {: next:RT-RECORD:cursor t:n :}
      t 0<> if 1 READS +! then
      t LONGEST @ max LONGEST !
      next got t +
      t 0=
   until
   {: c:RT-RECORD:cursor got:n :}
   got ;

\ Whether DST's first u bytes are SRC's.
: SAME? ( n -- bool )
   {: u:n :}
   true u 0 ?do SRC i + c@ DST i + c@ = and loop ;

\ ---- fields -----------------------------------------------------------------

TYPED-VARIABLE R-LEAF RT-RECORD:record
TYPED-VARIABLE R-PART RT-RECORD:record
TYPED-VARIABLE R-EMPTY RT-RECORD:record
TYPED-VARIABLE R-X RT-RECORD:record

: LEAF-OF ( n -- RT-RECORD:record )
   {: v:n :}
   LEAF @ PA @ RT-RECORD:BUILD
   v 0 >IDX PA @ RT-RECORD:INT!
   PA @ RT-RECORD:SEAL ;

\ R-PART holds 42 and -1, -2.5, R-LEAF and nothing, and "hello".
: BUILD-PART ( -- )
   7 LEAF-OF R-LEAF !
   PART @ PA @ RT-RECORD:BUILD
   42 0 >IDX PA @ RT-RECORD:INT!
   -1 1 >IDX PA @ RT-RECORD:INT!
   2.5 fnegate 2 >IDX PA @ RT-RECORD:FLOAT!
   R-LEAF @ 3 >IDX PA @ RT-RECORD:REF!
   s" hello" 5 >IDX PA @ RT-RECORD:VALUE!
   PA @ RT-RECORD:SEAL R-PART ! ;

: FIELDS ( -- )
   0 CELLS-A RT-POOL:INIT PA !
   BUILD-PART
   s" each int field reads back as written" T-LABEL
   R-PART @ PART @ 0 >IDX PA @ RT-RECORD:INT@ 42 T=
   R-PART @ PART @ 1 >IDX PA @ RT-RECORD:INT@ -1 T=
   s" and the float field" T-LABEL
   R-PART @ PART @ 2 >IDX PA @ RT-RECORD:FLOAT@ BITS 2.5 fnegate BITS T=
   s" a reference field holds the record REF! gave it" T-LABEL
   R-PART @ PART @ 3 >IDX PA @ RT-RECORD:REF@ LEAF @ 0 >IDX PA @ RT-RECORD:INT@ 7 T=
   s" and one never written holds the zero record" T-LABEL
   R-PART @ PART @ 4 >IDX PA @ RT-RECORD:REF@ RT-RECORD:NULL? TTRUE
   s" a value field reads back whole" T-LABEL
   R-PART @ PART @ 5 1000 READ-VALUE 5 T=
   DST 5 s" hello" T$=
   s" the record, its child and its value's one chunk take a page each" T-LABEL
   PAGES 3 T=
   s" every field of a record built with no writes reads zero" T-LABEL
   PART @ PA @ RT-RECORD:BUILD PA @ RT-RECORD:SEAL R-EMPTY !
   R-EMPTY @ PART @ 0 >IDX PA @ RT-RECORD:INT@ 0 T=
   R-EMPTY @ PART @ 2 >IDX PA @ RT-RECORD:FLOAT@ BITS 0 T=
   R-EMPTY @ PART @ 3 >IDX PA @ RT-RECORD:REF@ RT-RECORD:NULL? TTRUE
   R-EMPTY @ PART @ 5 1000 READ-VALUE 0 T=
   s" an empty value takes no chunk" T-LABEL
   PAGES 4 T=
   s" a float field holds either infinity" T-LABEL
   PART @ PA @ RT-RECORD:BUILD
   1.0 0.0 f/ 2 >IDX PA @ RT-RECORD:FLOAT!
   PA @ RT-RECORD:SEAL R-X !
   R-X @ PART @ 2 >IDX PA @ RT-RECORD:FLOAT@ BITS $7FF0000000000000 T=
   R-X @ PA @ RT-RECORD:RELEASE
   PART @ PA @ RT-RECORD:BUILD
   -1.0 0.0 f/ 2 >IDX PA @ RT-RECORD:FLOAT!
   PA @ RT-RECORD:SEAL R-X !
   R-X @ PART @ 2 >IDX PA @ RT-RECORD:FLOAT@ BITS $FFF0000000000000 T=
   R-X @ PA @ RT-RECORD:RELEASE
   s" the last ordinal of MAX-FIELDS holds a field and the next is refused" T-LABEL
   WIDE @ PA @ RT-RECORD:BUILD
   99 RT-RECORD:MAX-FIELDS 1 - >IDX PA @ RT-RECORD:INT!
   PA @ RT-RECORD:SEAL R-X !
   R-X @ WIDE @ RT-RECORD:MAX-FIELDS 1 - >IDX PA @ RT-RECORD:INT@ 99 T=
   [: R-X @ WIDE @ RT-RECORD:MAX-FIELDS >IDX PA @ RT-RECORD:INT@ drop ;] FIELD-CODE TTHROWSQ
   R-X @ PA @ RT-RECORD:RELEASE
   RECLAIM drop ;

\ ---- refused writes ---------------------------------------------------------

\ Each takes a builder of PART and makes a write PART refuses.
: INT-AT-FLOAT ( RT-RECORD:builder -- RT-RECORD:builder )
   1 2 >IDX PA @ RT-RECORD:INT! ;

: INT-BELOW ( RT-RECORD:builder -- RT-RECORD:builder )
   1 -1 >IDX PA @ RT-RECORD:INT! ;

: FLOAT-AT-INT ( RT-RECORD:builder -- RT-RECORD:builder )
   1.0 1 >IDX PA @ RT-RECORD:FLOAT! ;

: REF-AT-VALUE ( RT-RECORD:builder -- RT-RECORD:builder )
   R-LEAF @ 5 >IDX PA @ RT-RECORD:REF! ;

: VALUE-AT-REF ( RT-RECORD:builder -- RT-RECORD:builder )
   s" x" 4 >IDX PA @ RT-RECORD:VALUE! ;

: VALUE-PAST ( RT-RECORD:builder -- RT-RECORD:builder )
   s" x" 6 >IDX PA @ RT-RECORD:VALUE! ;

: NAN-QUOTIENT ( RT-RECORD:builder -- RT-RECORD:builder )
   0.0 0.0 f/ 2 >IDX PA @ RT-RECORD:FLOAT! ;

: NAN-LEAST ( RT-RECORD:builder -- RT-RECORD:builder )
   $7FF0000000000001 IEEE754:BITS>F64 2 >IDX PA @ RT-RECORD:FLOAT! ;

: NAN-NEGATIVE ( RT-RECORD:builder -- RT-RECORD:builder )
   -1 IEEE754:BITS>F64 2 >IDX PA @ RT-RECORD:FLOAT! ;

: NULL-REF ( RT-RECORD:builder -- RT-RECORD:builder )
   NO-RECORD @ 4 >IDX PA @ RT-RECORD:REF! ;

: NEGATIVE-SPAN ( RT-RECORD:builder -- RT-RECORD:builder )
   s" x" drop -1 5 >IDX PA @ RT-RECORD:VALUE! ;

: NULL-SPAN ( RT-RECORD:builder -- RT-RECORD:builder )
   NULL-PTR 1 5 >IDX PA @ RT-RECORD:VALUE! ;

TYPED-VARIABLE R-KEPT RT-RECORD:record

\ A builder holding 5, 1.5, R-LEAF and "kept" is caught by name through every
\ refused write, and the record it seals holds just those.
: WRITES ( -- )
   PART @ PA @ RT-RECORD:BUILD
   5 0 >IDX PA @ RT-RECORD:INT!
   1.5 2 >IDX PA @ RT-RECORD:FLOAT!
   R-LEAF @ 3 >IDX PA @ RT-RECORD:REF!
   s" kept" 5 >IDX PA @ RT-RECORD:VALUE!
   s" a write at an ordinal outside its kind is refused" T-LABEL
   ['] INT-AT-FLOAT catch FIELD-CODE T=
   ['] INT-BELOW catch FIELD-CODE T=
   ['] FLOAT-AT-INT catch FIELD-CODE T=
   ['] REF-AT-VALUE catch FIELD-CODE T=
   ['] VALUE-AT-REF catch FIELD-CODE T=
   ['] VALUE-PAST catch FIELD-CODE T=
   s" and so is a NaN in a float field, whatever its sign and payload" T-LABEL
   ['] NAN-QUOTIENT catch RT-RECORD:E-RT-RECORD-NAN T=
   ['] NAN-LEAST catch RT-RECORD:E-RT-RECORD-NAN T=
   ['] NAN-NEGATIVE catch RT-RECORD:E-RT-RECORD-NAN T=
   s" and the zero record in a reference field" T-LABEL
   ['] NULL-REF catch NULL-CODE T=
   s" and a negative length or bytes at the null address in a value field" T-LABEL
   ['] NEGATIVE-SPAN catch SPAN-CODE T=
   ['] NULL-SPAN catch SPAN-CODE T=
   PA @ RT-RECORD:SEAL R-KEPT !
   s" none of them changed the record the builder went on to seal" T-LABEL
   R-KEPT @ PART @ 0 >IDX PA @ RT-RECORD:INT@ 5 T=
   R-KEPT @ PART @ 1 >IDX PA @ RT-RECORD:INT@ 0 T=
   R-KEPT @ PART @ 2 >IDX PA @ RT-RECORD:FLOAT@ BITS 1.5 BITS T=
   R-KEPT @ PART @ 3 >IDX PA @ RT-RECORD:REF@ LEAF @ 0 >IDX PA @ RT-RECORD:INT@ 7 T=
   R-KEPT @ PART @ 4 >IDX PA @ RT-RECORD:REF@ RT-RECORD:NULL? TTRUE
   R-KEPT @ PART @ 5 1000 READ-VALUE 4 T=
   DST 4 s" kept" T$=
   s" while no bytes at the null address are an empty value" T-LABEL
   PART @ PA @ RT-RECORD:BUILD
   NULL-PTR 0 5 >IDX PA @ RT-RECORD:VALUE!
   PA @ RT-RECORD:SEAL R-X !
   R-X @ PART @ 5 1000 READ-VALUE 0 T=
   R-X @ PA @ RT-RECORD:RELEASE
   RECLAIM drop ;

\ ---- versions and schemas ----------------------------------------------------

: CURSOR-PART2 ( -- )
   R-PART @ PART2 @ 5 >IDX PA @ RT-RECORD:CURSOR {: c:RT-RECORD:cursor :} ;

: CURSOR-LEAF ( -- )
   R-PART @ LEAF @ 5 >IDX PA @ RT-RECORD:CURSOR {: c:RT-RECORD:cursor :} ;

: VERSION-CODE ( -- n )
   RT-RECORD:E-RT-RECORD-VERSION ;

: FOREIGN-CODE ( -- n )
   RT-RECORD:E-RT-RECORD-FOREIGN ;

: VERSIONS ( -- )
   s" each read of a record under another version of its schema is refused" T-LABEL
   [: R-PART @ PART2 @ 0 >IDX PA @ RT-RECORD:INT@ drop ;] VERSION-CODE TTHROWSQ
   [: R-PART @ PART2 @ 2 >IDX PA @ RT-RECORD:FLOAT@ drop ;] VERSION-CODE TTHROWSQ
   [: R-PART @ PART2 @ 3 >IDX PA @ RT-RECORD:REF@ drop ;] VERSION-CODE TTHROWSQ
   ['] CURSOR-PART2 VERSION-CODE TTHROWSQ
   s" and under a schema of another id" T-LABEL
   [: R-PART @ LEAF @ 0 >IDX PA @ RT-RECORD:INT@ drop ;] FOREIGN-CODE TTHROWSQ
   [: R-LEAF @ PART @ 0 >IDX PA @ RT-RECORD:INT@ drop ;] FOREIGN-CODE TTHROWSQ
   [: R-PART @ TREE @ 0 >IDX PA @ RT-RECORD:REF@ drop ;] FOREIGN-CODE TTHROWSQ
   ['] CURSOR-LEAF FOREIGN-CODE TTHROWSQ
   s" a record of version 2 reads under version 2 and not under 1" T-LABEL
   PART2 @ PA @ RT-RECORD:BUILD
   11 0 >IDX PA @ RT-RECORD:INT!
   PA @ RT-RECORD:SEAL R-X !
   R-X @ PART2 @ 0 >IDX PA @ RT-RECORD:INT@ 11 T=
   [: R-X @ PART @ 0 >IDX PA @ RT-RECORD:INT@ drop ;] VERSION-CODE TTHROWSQ
   R-X @ PA @ RT-RECORD:RELEASE
   RECLAIM drop ;

\ ---- zero tokens ------------------------------------------------------------

\ Never written: the zero pools token.
TYPED-VARIABLE PZ RT-POOL:pools

: CURSOR-ZERO ( -- )
   NO-RECORD @ PART @ 5 >IDX PZ @ RT-RECORD:CURSOR {: c:RT-RECORD:cursor :} ;

: BUILD-ZERO ( -- )
   NO-SCHEMA @ PZ @ RT-RECORD:BUILD PZ @ RT-RECORD:ABORT ;

\ Given the zero pools, the refusal proves no pool was read first.
: ZEROS ( -- )
   s" the zero record is refused before anything reads through it" T-LABEL
   [: NO-RECORD @ PZ @ RT-RECORD:RETAIN ;] NULL-CODE TTHROWSQ
   [: NO-RECORD @ PZ @ RT-RECORD:RELEASE ;] NULL-CODE TTHROWSQ
   [: NO-RECORD @ PART @ 0 >IDX PZ @ RT-RECORD:INT@ drop ;] NULL-CODE TTHROWSQ
   [: NO-RECORD @ PART @ 2 >IDX PZ @ RT-RECORD:FLOAT@ drop ;] NULL-CODE TTHROWSQ
   [: NO-RECORD @ PART @ 3 >IDX PZ @ RT-RECORD:REF@ drop ;] NULL-CODE TTHROWSQ
   ['] CURSOR-ZERO NULL-CODE TTHROWSQ
   s" and so is the zero schema" T-LABEL
   ['] BUILD-ZERO SCHEMA-CODE TTHROWSQ
   [: R-PART @ NO-SCHEMA @ 0 >IDX PZ @ RT-RECORD:INT@ drop ;] SCHEMA-CODE TTHROWSQ ;

\ ---- values through chunks ---------------------------------------------------

TYPED-VARIABLE R-BLOB RT-RECORD:record
TYPED-VARIABLE R-SMALL RT-RECORD:record
TYPED-VARIABLE R-TRIP RT-RECORD:record

: CHUNK-BYTES ( -- n )
   RT-RECORD:CHUNK-BYTES ;

\ A record of BLOB whose value is SRC's first u bytes.
: BLOB-OF ( n -- RT-RECORD:record )
   {: u:n :}
   BLOB @ PA @ RT-RECORD:BUILD
   SRC u 0 >IDX PA @ RT-RECORD:VALUE!
   PA @ RT-RECORD:SEAL ;

\ The chunks a value of u bytes takes.
: CHUNKS ( n -- n )
   CHUNK-BYTES 1 - + CHUNK-BYTES / ;

\ A value of u bytes takes a page per chunk beside its record's, reads back
\ whole at k bytes a READ, and gives every page back once released.
: ROUND-TRIP ( n n -- )
   {: u:n k:n :}
   PAGES {: before:n :}
   u BLOB-OF R-TRIP !
   PAGES before - u CHUNKS 1 + T=
   R-TRIP @ BLOB @ 0 k READ-VALUE u T=
   u SAME? TTRUE
   R-TRIP @ PA @ RT-RECORD:RELEASE
   RECLAIM drop
   PAGES before T= ;

: READ-NEGATIVE ( -- )
   R-SMALL @ BLOB @ 0 >IDX PA @ RT-RECORD:CURSOR
   DST -1 PA @ RT-RECORD:READ {: c:RT-RECORD:cursor t:n :} ;

: READ-NULL ( -- )
   R-SMALL @ BLOB @ 0 >IDX PA @ RT-RECORD:CURSOR
   NULL-PTR 1 PA @ RT-RECORD:READ {: c:RT-RECORD:cursor t:n :} ;

\ R-SMALL's chunk with the place of a cursor that read k bytes of R-BLOB.
: READ-CROSSED ( n -- )
   {: k:n :}
   R-SMALL @ BLOB @ 0 >IDX PA @ RT-RECORD:CURSOR RT--RECORD-CURSOR:UNMAKE {: small-chunk small-at :}
   R-BLOB @ BLOB @ 0 >IDX PA @ RT-RECORD:CURSOR
   DST k PA @ RT-RECORD:READ {: c:RT-RECORD:cursor t:n :}
   c RT--RECORD-CURSOR:UNMAKE {: blob-chunk blob-at :}
   small-chunk blob-at RT--RECORD-CURSOR:MAKE
   DST 1 PA @ RT-RECORD:READ {: d:RT-RECORD:cursor u:n :} ;

: READ-CROSSED-10 ( -- )
   10 READ-CROSSED ;

: READ-CROSSED-50 ( -- )
   50 READ-CROSSED ;

\ A cursor R-SMALL's record had before it was reclaimed.
TYPED-VARIABLE C-SMALL RT-RECORD:cursor

: READ-RECLAIMED ( -- )
   C-SMALL @ DST 1 PA @ RT-RECORD:READ {: c:RT-RECORD:cursor t:n :} ;

: VALUES ( -- )
   s" a 1 MiB value round-trips through chunks, 1000 bytes a READ" T-LABEL
   MIB 1000 ROUND-TRIP
   s" which takes five READs a full chunk and one for the last" T-LABEL
   MIB BLOB-OF R-BLOB !
   R-BLOB @ BLOB @ 0 1000 READ-VALUE drop
   READS @ MIB CHUNK-BYTES / 5 * 1 + T=
   s" a READ that asks for the whole value gets one chunk's bytes at most" T-LABEL
   R-BLOB @ BLOB @ 0 MIB READ-VALUE MIB T=
   READS @ MIB CHUNKS T=
   LONGEST @ CHUNK-BYTES T=
   MIB SAME? TTRUE
   s" values at and just past a chunk's bytes, and of one byte, round-trip" T-LABEL
   CHUNK-BYTES MIB ROUND-TRIP
   CHUNK-BYTES 1 + MIB ROUND-TRIP
   1 MIB ROUND-TRIP
   s" a READ of no bytes answers 0 and leaves the cursor" T-LABEL
   10 BLOB-OF R-SMALL !
   R-SMALL @ BLOB @ 0 >IDX PA @ RT-RECORD:CURSOR
   DST 0 PA @ RT-RECORD:READ 0 T=
   DST 10 PA @ RT-RECORD:READ 10 T=
   DST 10 PA @ RT-RECORD:READ 0 T=
   {: c:RT-RECORD:cursor :}
   10 SAME? TTRUE
   s" a READ of a negative length or into the null address is refused" T-LABEL
   ['] READ-NEGATIVE SPAN-CODE TTHROWSQ
   ['] READ-NULL SPAN-CODE TTHROWSQ
   s" and so is a cursor whose place lies at or past its chunk's bytes" T-LABEL
   ['] READ-CROSSED-10 SPAN-CODE TTHROWSQ
   ['] READ-CROSSED-50 SPAN-CODE TTHROWSQ
   R-SMALL @ BLOB @ 0 >IDX PA @ RT-RECORD:CURSOR C-SMALL !
   R-SMALL @ PA @ RT-RECORD:RELEASE
   R-BLOB @ PA @ RT-RECORD:RELEASE
   RECLAIM drop
   s" a cursor read after its record is reclaimed is refused" T-LABEL
   ['] READ-RECLAIMED RT-HANDLE:E-RT-HANDLE-STALE TTHROWSQ ;

\ ---- reclaiming -------------------------------------------------------------

TYPED-VARIABLE R-L1 RT-RECORD:record
TYPED-VARIABLE R-L2 RT-RECORD:record
TYPED-VARIABLE R-P RT-RECORD:record

: STALE ( -- n )
   RT-HANDLE:E-RT-HANDLE-STALE ;

\ R-P references R-L1 at 0, nothing at 1 and R-L2 at 2, and holds a value of
\ three chunks.
: BUILD-TREE ( -- )
   1 LEAF-OF R-L1 !
   2 LEAF-OF R-L2 !
   TREE @ PA @ RT-RECORD:BUILD
   R-L1 @ 0 >IDX PA @ RT-RECORD:REF!
   R-L2 @ 2 >IDX PA @ RT-RECORD:REF!
   SRC CHUNK-BYTES 2 * 1 + 3 >IDX PA @ RT-RECORD:VALUE!
   PA @ RT-RECORD:SEAL R-P ! ;

: RECLAIMS ( -- )
   PAGES {: before:n :}
   BUILD-TREE
   R-L1 @ PA @ RT-RECORD:RELEASE
   R-L2 @ PA @ RT-RECORD:RELEASE
   s" children a record holds outlive their own release" T-LABEL
   R-L1 @ LEAF @ 0 >IDX PA @ RT-RECORD:INT@ 1 T=
   PAGES before - 6 T=
   s" releasing the record frees nothing before RECLAIM-STEP" T-LABEL
   R-P @ PA @ RT-RECORD:RELEASE
   PAGES before - 6 T=
   s" which visits each child and chunk once, the empty reference skipped" T-LABEL
   RECLAIM 11 T=
   s" and gives back every page" T-LABEL
   PAGES before T=
   [: R-L1 @ LEAF @ 0 >IDX PA @ RT-RECORD:INT@ drop ;] STALE TTHROWSQ
   [: R-L2 @ PA @ RT-RECORD:RETAIN ;] STALE TTHROWSQ
   s" a child two records hold outlives the first" T-LABEL
   3 LEAF-OF R-L1 !
   TREE @ PA @ RT-RECORD:BUILD R-L1 @ 0 >IDX PA @ RT-RECORD:REF! PA @ RT-RECORD:SEAL R-P !
   TREE @ PA @ RT-RECORD:BUILD R-L1 @ 1 >IDX PA @ RT-RECORD:REF! PA @ RT-RECORD:SEAL R-X !
   R-L1 @ PA @ RT-RECORD:RELEASE
   R-P @ PA @ RT-RECORD:RELEASE
   RECLAIM 2 T=
   R-L1 @ LEAF @ 0 >IDX PA @ RT-RECORD:INT@ 3 T=
   s" and goes with the second" T-LABEL
   R-X @ PA @ RT-RECORD:RELEASE
   RECLAIM 3 T=
   [: R-L1 @ PA @ RT-RECORD:RETAIN ;] STALE TTHROWSQ
   s" a record retained outlives a release" T-LABEL
   4 LEAF-OF R-X !
   R-X @ PA @ RT-RECORD:RETAIN
   R-X @ PA @ RT-RECORD:RELEASE
   RECLAIM 0 T=
   R-X @ LEAF @ 0 >IDX PA @ RT-RECORD:INT@ 4 T=
   R-X @ PA @ RT-RECORD:RELEASE
   RECLAIM 1 T=
   s" an aborted record goes with its value and releases its references" T-LABEL
   5 LEAF-OF R-L1 !
   TREE @ PA @ RT-RECORD:BUILD
   R-L1 @ 0 >IDX PA @ RT-RECORD:REF!
   SRC 10 3 >IDX PA @ RT-RECORD:VALUE!
   PA @ RT-RECORD:ABORT
   RECLAIM 4 T=
   R-L1 @ LEAF @ 0 >IDX PA @ RT-RECORD:INT@ 5 T=
   s" a reference or value written again releases what it replaced" T-LABEL
   6 LEAF-OF R-L2 !
   TREE @ PA @ RT-RECORD:BUILD
   R-L1 @ 0 >IDX PA @ RT-RECORD:REF!
   R-L2 @ 0 >IDX PA @ RT-RECORD:REF!
   SRC CHUNK-BYTES 1 + 3 >IDX PA @ RT-RECORD:VALUE!
   SRC 10 3 >IDX PA @ RT-RECORD:VALUE!
   PA @ RT-RECORD:SEAL R-P !
   R-L1 @ PA @ RT-RECORD:RELEASE
   R-L2 @ PA @ RT-RECORD:RELEASE
   RECLAIM 4 T=
   R-P @ TREE @ 0 >IDX PA @ RT-RECORD:REF@ LEAF @ 0 >IDX PA @ RT-RECORD:INT@ 6 T=
   R-P @ TREE @ 3 1000 READ-VALUE 10 T=
   R-P @ PA @ RT-RECORD:RELEASE
   RECLAIM drop
   PAGES before T= ;

\ ---- the pools' own words ----------------------------------------------------

\ The oracle's route: a record's bits through a cast any package may declare,
\ and the handle of its page rebuilt from them for RT-POOL's words. A page
\ holds the header in payload cell 0 and field o in cell o + 1; a chunk holds
\ its next chunk in cell 0 and its bytes from cell 2.
CAST: LEAK ( RT-RECORD:record -- n )

: PAGE-OF ( RT-RECORD:record -- RT-HANDLE:handle )
   LEAK {: v:n :}
   v $FFFFFFFF and v 32 rshift RT-HANDLE:HANDLE ;

: FROZEN-CODE ( -- n )
   RT-POOL:E-RT-POOL-FROZEN ;

\ A NaN stored by CELL! in R-PART's float field, ordinal 2.
: NAN-BY-CELL ( -- )
   $7FF8000000000000 R-PART @ PAGE-OF 3 PA @ RT-POOL:CELL! ;

\ R-LEAF stored by REF! in R-PART's empty reference field, ordinal 4.
: LEAF-BY-REF ( -- )
   R-LEAF @ PAGE-OF R-PART @ PAGE-OF 5 PA @ RT-POOL:REF! ;

\ A record of BLOB whose value takes two chunks.
TYPED-VARIABLE R-TWO RT-RECORD:record

: HEAD-CHUNK ( -- RT-HANDLE:handle )
   R-TWO @ PAGE-OF 1 PA @ RT-POOL:REF@ ;

: HEAD-BY-CELL ( -- )
   0 HEAD-CHUNK 2 PA @ RT-POOL:CELL! ;

: NEXT-BY-CELL ( -- )
   0 HEAD-CHUNK 0 PA @ RT-POOL:REF@ 2 PA @ RT-POOL:CELL! ;

: LEAKS ( -- )
   s" a sealed record's page, rebuilt from its bits, refuses CELL! of a NaN" T-LABEL
   ['] NAN-BY-CELL FROZEN-CODE TTHROWSQ
   R-PART @ PART @ 2 >IDX PA @ RT-RECORD:FLOAT@ BITS 2.5 fnegate BITS T=
   s" and REF! of a reference field, which stays empty" T-LABEL
   ['] LEAF-BY-REF FROZEN-CODE TTHROWSQ
   R-PART @ PART @ 4 >IDX PA @ RT-RECORD:REF@ RT-RECORD:NULL? TTRUE
   s" each chunk of its value refuses CELL! too" T-LABEL
   CHUNK-BYTES 1 + BLOB-OF R-TWO !
   ['] HEAD-BY-CELL FROZEN-CODE TTHROWSQ
   ['] NEXT-BY-CELL FROZEN-CODE TTHROWSQ
   R-TWO @ BLOB @ 0 1000 READ-VALUE CHUNK-BYTES 1 + T=
   CHUNK-BYTES 1 + SAME? TTRUE
   R-TWO @ PA @ RT-RECORD:RELEASE
   RECLAIM drop ;

RT-POOL:POOLS-CELLS TYPED-BUFFER CELLS-F n
TYPED-VARIABLE PF RT-POOL:pools

\ The first page fresh pools issue: slot 1 at generation 1.
: FIRST-PAGE ( -- RT-HANDLE:handle )
   1 1 RT-HANDLE:HANDLE ;

: VALUE-X ( RT-RECORD:builder -- RT-RECORD:builder )
   s" x" 0 >IDX PF @ RT-RECORD:VALUE! ;

\ The open builder's page, frozen through FREEZE, which takes any held handle.
: FROZEN-BUILDER ( -- )
   0 CELLS-F RT-POOL:INIT PF !
   BLOB @ PF @ RT-RECORD:BUILD
   FIRST-PAGE PF @ RT-POOL:FREEZE
   s" a value for a builder whose page FREEZE froze is refused" T-LABEL
   ['] VALUE-X catch FROZEN-CODE T=
   s" and the chain built for it is released, so nothing stays charged" T-LABEL
   PF @ RT-RECORD:ABORT
   begin 1 PF @ RT-POOL:RECLAIM-STEP 0= until
   RT--POOL-KIND:pages PF @ RT-POOL:CHARGED 0 T=
   PF @ RT-POOL:SHUTDOWN ;

\ ---- oom --------------------------------------------------------------------

RT-POOL:POOLS-CELLS TYPED-BUFFER CELLS-Q n
TYPED-VARIABLE PQ RT-POOL:pools

: OOM-CODE ( -- n )
   RT-RECORD:E-RT-RECORD-OOM ;

: CHARGED-Q ( -- n )
   RT--POOL-KIND:pages PQ @ RT-POOL:CHARGED 4096 / ;

: BUILD-Q ( -- )
   PART @ PQ @ RT-RECORD:BUILD PQ @ RT-RECORD:ABORT ;

\ A value of three chunks.
: VALUE-3 ( RT-RECORD:builder -- RT-RECORD:builder )
   SRC CHUNK-BYTES 2 * 1 + 5 >IDX PQ @ RT-RECORD:VALUE! ;

\ The longest length VALUE! takes, 2^63 - CHUNK-BYTES: CHAIN adds
\ CHUNK-BYTES - 1 to it to count its chunks, which makes the largest cell.
$7FFFFFFFFFFFFFFF CHUNK-BYTES 1 - - constant MOST-BYTES

: VALUE-MOST ( RT-RECORD:builder -- RT-RECORD:builder )
   SRC MOST-BYTES 5 >IDX PQ @ RT-RECORD:VALUE! ;

: VALUE-PAST-MOST ( RT-RECORD:builder -- RT-RECORD:builder )
   SRC MOST-BYTES 1 + 5 >IDX PQ @ RT-RECORD:VALUE! ;

: VALUE-MAX-N ( RT-RECORD:builder -- RT-RECORD:builder )
   SRC $7FFFFFFFFFFFFFFF 5 >IDX PQ @ RT-RECORD:VALUE! ;

: OOMS ( -- )
   0 CELLS-Q RT-POOL:INIT PQ !
   0 RT--POOL-KIND:pages PQ @ RT-POOL:QUOTA!
   s" BUILD with no page left is refused, nothing charged" T-LABEL
   ['] BUILD-Q OOM-CODE TTHROWSQ
   CHARGED-Q 0 T=
   2 4096 * RT--POOL-KIND:pages PQ @ RT-POOL:QUOTA!
   PART @ PQ @ RT-RECORD:BUILD
   s" ok" 5 >IDX PQ @ RT-RECORD:VALUE!
   s" a value with no page for its first chunk is refused, the builder kept" T-LABEL
   ['] VALUE-3 catch OOM-CODE T=
   CHARGED-Q 2 T=
   s" a length past 2^63 - CHUNK-BYTES is refused before the pools are asked" T-LABEL
   ['] VALUE-MAX-N catch SPAN-CODE T=
   ['] VALUE-PAST-MOST catch SPAN-CODE T=
   s" while 2^63 - CHUNK-BYTES goes on to the pools, which have no page for it" T-LABEL
   ['] VALUE-MOST catch OOM-CODE T=
   CHARGED-Q 2 T=
   3 4096 * RT--POOL-KIND:pages PQ @ RT-POOL:QUOTA!
   s" and one the pools run out of partway too" T-LABEL
   ['] VALUE-3 catch OOM-CODE T=
   s" the chunk built before is released, for RECLAIM-STEP to give back" T-LABEL
   CHARGED-Q 3 T=
   1 PQ @ RT-POOL:RECLAIM-STEP 1 T=
   CHARGED-Q 2 T=
   s" and the field keeps what it held through every refusal" T-LABEL
   PQ @ RT-RECORD:SEAL R-X !
   R-X @ PART @ 5 >IDX PQ @ RT-RECORD:CURSOR
   DST 100 PQ @ RT-RECORD:READ 2 T=
   {: c:RT-RECORD:cursor :}
   DST 2 s" ok" T$=
   PQ @ RT-POOL:SHUTDOWN ;

\ ---- routes the checker refuses ---------------------------------------------

4096 BUFFER: CHECK-TEXT

\ The checker refuses the candidate, and its diagnostic holds the text.
: REFUSED ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n want:ptr wantu:n :}
   CHECK-TEXT 4096 DIAG-BUFFER!
   src srcu CHECK-CANDIDATE! 0 T=
   DIAG-BUFFER$ want wantu CONTAINS? TTRUE
   DIAG-BUFFER-OFF ;

: CERTIFIED ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

: CHECKER ( -- )
   s" duplicating a builder is refused by the checker" T-LABEL
   s" RRT-DUP ( RT-RECORD:builder -- RT-RECORD:builder RT-RECORD:builder ) dup"
   s" at 'dup'" REFUSED
   s" and so is dropping one" T-LABEL
   s" RRT-DROP ( RT-RECORD:builder -- ) drop"
   s" at 'drop'" REFUSED
   s" while a record, which is shared, is duplicated and dropped" T-LABEL
   s" RRT-DUP-RECORD ( RT-RECORD:record -- RT-RECORD:record RT-RECORD:record ) dup" CERTIFIED
   s" RRT-DROP-RECORD ( RT-RECORD:record -- ) drop" CERTIFIED
   s" and a builder is sealed or aborted" T-LABEL
   s" RRT-SEAL ( RT-RECORD:builder RT-POOL:pools -- RT-RECORD:record ) RT-RECORD:SEAL" CERTIFIED
   s" RRT-ABORT ( RT-RECORD:builder RT-POOL:pools -- ) RT-RECORD:ABORT" CERTIFIED
   s" a builder where a reference field wants a record is refused" T-LABEL
   s" RRT-OPEN-REF ( RT-RECORD:builder RT-RECORD:builder RT-POOL:pools -- RT-RECORD:builder ) {: p:RT-POOL:pools :} 0 >IDX p RT-RECORD:REF!"
   s" actual: RT-RECORD:builder RT-RECORD:builder idx rt-pool:pools" REFUSED
   s" a number where a record or a schema is expected is refused" T-LABEL
   s" RRT-N-AS-RECORD ( n RT-POOL:pools -- ) RT-RECORD:RETAIN"
   s" actual: n rt-pool:pools" REFUSED
   s" RRT-N-AS-SCHEMA ( n RT-POOL:pools -- RT-RECORD:builder ) RT-RECORD:BUILD"
   s" actual: n rt-pool:pools" REFUSED
   s" and a number where an ordinal is" T-LABEL
   s" RRT-N-AS-ORDINAL ( RT-RECORD:builder n n RT-POOL:pools -- RT-RECORD:builder ) RT-RECORD:INT!"
   s" actual: RT-RECORD:builder n n rt-pool:pools" REFUSED ;

\ A cast is declared at top level, where the text runs.
: CASTS ( -- )
   s" no cast turns a number into a record or a schema outside RT-RECORD" T-LABEL
   s" CAST: RRT-FORGE-RECORD ( n -- RT-RECORD:record )" TEST-EVAL:RC E-CAST-OWNER T=
   s" CAST: RRT-FORGE-SCHEMA ( n -- RT-RECORD:schema )" TEST-EVAL:RC E-CAST-OWNER T=
   s" nor into a builder, there or anywhere" T-LABEL
   s" CAST: RRT-FORGE-BUILDER ( n -- RT-RECORD:builder )" TEST-EVAL:RC E-CAST-LINEAR T=
   s" and no LINEAR: row outside RT-RECORD mints one" T-LABEL
   s" LINEAR: RRT-MINT ( n -- RT-RECORD:builder )" TEST-EVAL:RC E-LINEAR-OWNER T= ;

$400 constant CHILD-CAP
10000 constant CHILD-MS
CHILD-CAP BUFFER: CHILD-OUT
CHILD-CAP BUFFER: CHILD-ERR

\ A forked child of this image loads the text and exits 70, having named the
\ word undefined.
: UNDEFINED ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n want:ptr wantu:n :}
   src srcu CHILD-OUT CHILD-CAP >LEN CHILD-ERR CHILD-CAP >LEN CHILD-MS >MS SUBJECT:RUN
   {: outu:len erru:len oc :}
   src srcu CHILD-OUT outu LEN>N CHILD-ERR erru LEN>N oc CHECKER-REJECT-RC T-OUTCOME-EXITED=
   CHILD-ERR erru LEN>N want wantu CONTAINS? TTRUE ;

: CONVERTERS ( -- )
   s" the converters of records and schemas are undefined outside RT-RECORD" T-LABEL
   s" : RRT-U1 ( n -- RT-RECORD:record ) RT-RECORD:>RECORD ;" s" E-UNDEFINED: RT-RECORD:>RECORD" UNDEFINED
   s" : RRT-U2 ( RT-RECORD:record -- n ) RT-RECORD:RECORD>N ;" s" E-UNDEFINED: RT-RECORD:RECORD>N" UNDEFINED
   s" : RRT-U3 ( n -- RT-RECORD:schema ) RT-RECORD:>SCHEMA ;" s" E-UNDEFINED: RT-RECORD:>SCHEMA" UNDEFINED
   s" : RRT-U4 ( RT-RECORD:schema -- n ) RT-RECORD:SCHEMA>N ;" s" E-UNDEFINED: RT-RECORD:SCHEMA>N" UNDEFINED
   s" and so are the builder's mint and erase" T-LABEL
   s" : RRT-U5 ( n -- RT-RECORD:builder ) RT-RECORD:>BUILDER ;" s" E-UNDEFINED: RT-RECORD:>BUILDER" UNDEFINED
   s" : RRT-U6 ( RT-RECORD:builder -- n ) RT-RECORD:BUILDER> ;" s" E-UNDEFINED: RT-RECORD:BUILDER>" UNDEFINED
   s" and the maker of a cursor's parts" T-LABEL
   s" : RRT-U7 ( -- ) 0 RT-RECORD:>CURSOR-PART drop ;" s" E-UNDEFINED: RT-RECORD:>CURSOR-PART" UNDEFINED ;

: MAIN ( -- )
   T-RESET
   DECLARATIONS
   FIELDS
   WRITES
   VERSIONS
   ZEROS
   VALUES
   RECLAIMS
   LEAKS
   FROZEN-BUILDER
   OOMS
   CHECKER
   CASTS
   CONVERTERS ;

MAIN

;package

\ ---- white-box --------------------------------------------------------------
\ What only a copy of a builder the checker never saw reaches, a copy that
\ outlives SEAL or ABORT, then a schema number SCHEMA never answered and a full
\ registry. Only RT-RECORD mints a builder or a schema, so these rows run
\ inside it.
package RT-RECORD

RT-POOL:POOLS-CELLS TYPED-BUFFER RRT-CELLS-W n
TYPED-VARIABLE RRT-PW RT-POOL:pools
TYPED-VARIABLE RRT-S schema
TYPED-VARIABLE RRT-R record
TYPED-VARIABLE RRT-CHILD record
TYPED-VARIABLE RRT-FIRST record
16 BUFFER: RRT-IDB

\ An Id128 no other row declares: $FF, zeros, then n in the last two bytes.
: RRT-ID ( n -- RT-ID:id128 )
   {: v:n :}
   16 0 do 0 RRT-IDB i + c! loop
   $FF RRT-IDB c!
   v 8 rshift $FF and RRT-IDB 14 + c!
   v $FF and RRT-IDB 15 + c!
   RRT-IDB 16 RT-ID:BYTES>ID128 ;

: RRT-SETUP ( -- )
   0 RRT-CELLS-W RT-POOL:INIT RRT-PW !
   0 RRT-ID 1 1 1 1 1 SCHEMA RRT-S ! ;

RRT-SETUP

: RRT-RECLAIM ( -- n )
   0 begin 1 RRT-PW @ RT-POOL:RECLAIM-STEP dup 0<> while + repeat drop ;

\ Two builders of one record, as a copy the checker never saw makes them.
: RRT-TWIN ( RT-RECORD:builder -- RT-RECORD:builder RT-RECORD:builder )
   BUILDER> dup >BUILDER swap >BUILDER ;

\ Each writes through the builder it is given.
: RRT-INT ( RT-RECORD:builder -- RT-RECORD:builder )
   9 0 >IDX RRT-PW @ INT! ;

: RRT-FLOAT ( RT-RECORD:builder -- RT-RECORD:builder )
   9.0 1 >IDX RRT-PW @ FLOAT! ;

: RRT-REF ( RT-RECORD:builder -- RT-RECORD:builder )
   RRT-CHILD @ 2 >IDX RRT-PW @ REF! ;

: RRT-VALUE ( RT-RECORD:builder -- RT-RECORD:builder )
   s" changed" 3 >IDX RRT-PW @ VALUE! ;

: RRT-SEAL-COPY ( -- )
   RRT-S @ RRT-PW @ BUILD RRT-TWIN
   RRT-PW @ SEAL RRT-FIRST !
   RRT-PW @ SEAL drop ;

: RRT-ABORT-COPY ( -- )
   RRT-S @ RRT-PW @ BUILD RRT-TWIN
   RRT-PW @ SEAL RRT-FIRST !
   RRT-PW @ ABORT ;

: RRT-SEALED ( -- )
   RRT-S @ RRT-PW @ BUILD RRT-PW @ SEAL RRT-CHILD !
   RRT-S @ RRT-PW @ BUILD
   1 0 >IDX RRT-PW @ INT!
   s" kept" 3 >IDX RRT-PW @ VALUE!
   RRT-TWIN RRT-PW @ SEAL RRT-R !
   s" a copy of a builder SEAL consumed is refused by each word that writes" T-LABEL
   ['] RRT-INT catch E-RT-RECORD-SEALED T=
   ['] RRT-FLOAT catch E-RT-RECORD-SEALED T=
   ['] RRT-REF catch E-RT-RECORD-SEALED T=
   ['] RRT-VALUE catch E-RT-RECORD-SEALED T=
   BUILDER> drop
   s" so the sealed record keeps what it held" T-LABEL
   RRT-R @ RRT-S @ 0 >IDX RRT-PW @ INT@ 1 T=
   RRT-R @ RRT-S @ 1 >IDX RRT-PW @ FLOAT@ IEEE754:F64>BITS 0 T=
   RRT-R @ RRT-S @ 2 >IDX RRT-PW @ REF@ NULL? TTRUE
   RRT-R @ RRT-S @ 3 >IDX RRT-PW @ CURSOR
   RRT-IDB 16 RRT-PW @ READ 4 T=
   {: c:cursor :}
   RRT-IDB 4 s" kept" T$=
   s" and neither SEAL nor ABORT takes a copy whose record is sealed" T-LABEL
   [: RRT-SEAL-COPY ;] E-RT-RECORD-SEALED TTHROWSQ
   RRT-FIRST @ RRT-PW @ RELEASE
   [: RRT-ABORT-COPY ;] E-RT-RECORD-SEALED TTHROWSQ
   RRT-FIRST @ RRT-PW @ RELEASE
   RRT-R @ RRT-PW @ RELEASE
   RRT-CHILD @ RRT-PW @ RELEASE
   RRT-RECLAIM drop ;

: RRT-STALE ( -- )
   RRT-S @ RRT-PW @ BUILD RRT-TWIN
   RRT-PW @ ABORT
   s" a copy of a builder ABORT released is refused, dead and then stale" T-LABEL
   ['] RRT-INT catch RT-POOL:E-RT-POOL-DEAD T=
   RRT-RECLAIM drop
   ['] RRT-INT catch RT-HANDLE:E-RT-HANDLE-STALE T=
   BUILDER> drop ;

: RRT-UNKNOWN ( -- )
   s" a schema number SCHEMA never answered is refused" T-LABEL
   [: SCHEMAS @ 1 + >SCHEMA RRT-PW @ BUILD RRT-PW @ ABORT ;] E-RT-RECORD-SCHEMA TTHROWSQ ;

: RRT-FULL ( -- )
   MAX-SCHEMAS SCHEMAS @ - 0 ?do i 1 + RRT-ID 1 0 0 0 0 SCHEMA drop loop
   s" a schema past MAX-SCHEMAS is refused" T-LABEL
   [: 1000 RRT-ID 1 0 0 0 0 SCHEMA drop ;] E-RT-RECORD-SCHEMA TTHROWSQ ;

RRT-SEALED
RRT-STALE
RRT-UNKNOWN
RRT-FULL

;package

T-REPORT

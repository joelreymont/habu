split-str
\ A document opens a runtime-sized set of wire parts, retains only owners and
\ lexical ranges, closes its parsing cursors, then edits those parts later.
require src/lib/test.f
require src/lib/memory.f
require src/lib/heap.f
require src/lib/span.f
require src/lib/vector.f
require src/lib/xml.f
require src/lib/arena.f
require src/lib/byte-edit.f
require src/lib/fs.f
require src/os/script-argv.f

package XML-OWNER-CONSUMER
private

512 constant WIRE-CAP
1024 constant OUTPUT-CAP
create WIRE WIRE-CAP allot
create INPUT WIRE-CAP allot
create EDIT-STORE 3 EDIT:STORAGE-BYTES allot
create WRITTEN OUTPUT-CAP allot
TYPED-VARIABLE DECODED ^VEC:vector<u8>
1 SPAN-BUFFER: STREAM-CHUNK
variable STREAM-POS

DYNAMIC-BUFFER OWNERS XML:source-owner
DYNAMIC-BUFFER ORIGINALS ^u8
DYNAMIC-BUFFER ORIGINAL-SIZES NUM:alloc-byte-len
DYNAMIC-BUFFER ATTR-OFF off
DYNAMIC-BUFFER ATTR-LEN len
DYNAMIC-BUFFER TEXT-OFF off
DYNAMIC-BUFFER TEXT-LEN len

TYPED-VARIABLE RD XML:cursor
variable COUNT
variable ORIGINAL-COUNT
variable OWNER-COUNT
variable FIRST-OWNER
variable READING
variable BAD-LAST
variable INVALID-LAST

CAST: KIND>N ( XML:kind -- n )
CAST: ENCODING>N ( XML:encoding -- n )

: UTF16$ ( str bool -- str )
   {: ascii size:n little:bool :}
   size WIRE-CAP 2 - 2 / > if XML:E-CAPACITY throw then
   little if $FF WIRE c! $FE WIRE 1+ c!
   else $FE WIRE c! $FF WIRE 1+ c! then
   size 0 ?do
      ascii i + c@ {: byte:n :}
      little if
         byte WIRE i 2 * 2 + + c! 0 WIRE i 2 * 3 + + c!
      else
         0 WIRE i 2 * 2 + + c! byte WIRE i 2 * 3 + + c!
      then
   loop
   WIRE size 2 * 2 + ;

: PART$ ( n bool -- str )
   {: index:n malformed:bool :}
   index COUNT @ 1- = INVALID-LAST @ 0<> and if
      s\" <r>\xC0\x80</r>" exit
   then
   malformed if s" <r q='a&amp;b'>Ax&#x1F600;Y</wrong>" exit then
   index 3 mod {: form:n :}
   form 0= if s\" \xEF\xBB\xBF<r q='a&amp;b'>Ax😀Y</r><!--keep-->" exit then
   s" <r q='a&amp;b'>Ax&#x1F600;Y</r><!--keep-->"
   form 1 = UTF16$ ;

: NEXT= ( XML:reader XML:kind -- XML:reader )
   KIND>N {: want:n :}
   XML:NEXT KIND>N want T= ;

: STREAM$ ( -- str )
   s" <a xmlns:p='urn:x'><p:b q='&amp;'>Aé&amp;Z</p:b></a>" ;

: STREAM-FILL ( ^n SPAN:span<u8> -- n )
   {: pos:^ output :}
   STREAM$ {: source size:n :}
   pos @ size >= if 0 exit then
   source pos @ + c@ output 0 SPAN:U8!
   1 pos +! 1 ;

: CLEARED ( -- ^VEC:vector<u8> ) DECODED @ dup 0 VEC:RESIZE ;
: DECODED$ ( -- str ) DECODED @ VEC:SPAN SPAN:$ ;

: STREAM-READ ( -- )
   0 STREAM-POS !
   RD STREAM-POS ['] STREAM-FILL STREAM-CHUNK XML:INIT-INPUT
   XML-KIND:START NEXT=
   XML-KIND:START NEXT=
   XML:NAME$ s" p:b" T$=
   CLEARED XML:URI DECODED$ s" urn:x" T$=
   XML:ATTR-NEXT TTRUE
   CLEARED XML:ATTR-TEXT DECODED$ s" &" T$=
   CLEARED drop
   begin XML:NEXT KIND>N XML-KIND:TEXT KIND>N = while
      DECODED @ XML:TEXT
   repeat
   DECODED$ s" Aé&Z" T$=
   XML:KIND KIND>N XML-KIND:END KIND>N T=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT=
   XML:CLOSE ;

: READ-PART ( XML:reader -- XML:reader )
   XML-KIND:START NEXT=
   XML:ATTR-NEXT TTRUE
   XML:ATTR-VALUE {: start:off size:len :}
   start READING @ ATTR-OFF ! size READING @ ATTR-LEN !
   XML-KIND:TEXT NEXT=
   XML:CONTENT {: start:off size:len :}
   start READING @ TEXT-OFF ! size READING @ TEXT-LEN !
   begin XML:NEXT KIND>N XML-KIND:EOF KIND>N <> while repeat ;

: PARSE-PART ( n -- )
   dup READING ! OWNERS @ RD swap XML:OWNER-READER
   ['] READ-PART catch swap XML:CLOSE
   dup 0<> if throw then drop ;

: COPY-PART ( n -- )
   {: index:n :}
   index 1+ OWNERS-RESERVE index 1+ ORIGINALS-RESERVE
   index 1+ ORIGINAL-SIZES-RESERVE index 1+ ATTR-OFF-RESERVE
   index 1+ ATTR-LEN-RESERVE index 1+ TEXT-OFF-RESERVE
   index 1+ TEXT-LEN-RESERVE
   index index COUNT @ 1- = BAD-LAST @ 0<> and PART$
   {: wire size:n :}
   size MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES
   {: original original-size:NUM:alloc-byte-len :}
   wire original size >LEN BYTE-COPY-LEN
   original index ORIGINALS ! original-size index ORIGINAL-SIZES !
   ORIGINAL-COUNT @ 1+ ORIGINAL-COUNT !
   HEAP:ALLOCATOR original size XML:SOURCE-INIT XML:SOURCE-OWN index OWNERS !
   OWNER-COUNT @ 1+ OWNER-COUNT !
   index PARSE-PART ;

: OPEN-PARTS ( -- )
   COUNT @ 0 ?do i COPY-PART loop ;

: CLOSE-PARTS ( -- )
   OWNER-COUNT @ FIRST-OWNER @ ?do i OWNERS @ XML:OWNER-CLOSE loop
   ORIGINAL-COUNT @ 0 ?do i ORIGINALS @ i ORIGINAL-SIZES @ MEM:RELEASE-BYTES loop
   OWNERS-RELEASE ORIGINALS-RELEASE ORIGINAL-SIZES-RELEASE
   ATTR-OFF-RELEASE ATTR-LEN-RELEASE TEXT-OFF-RELEASE TEXT-LEN-RELEASE
   0 OWNER-COUNT ! 0 ORIGINAL-COUNT ! 0 FIRST-OWNER ! ;

: OPEN ( n bool -- )
   {: count:n bad:bool :}
   count COUNT ! bad BAD-LAST !
   0 OWNER-COUNT ! 0 ORIGINAL-COUNT ! 0 FIRST-OWNER !
   ['] OPEN-PARTS catch
   dup 0<> if
      {: failure:n :}
      ['] CLOSE-PARTS catch drop
      failure throw
   then drop ;

: CHECK-OWNER ( n -- )
   {: index:n :}
   index OWNERS @ {: owner :}
   owner XML:OWNER-BOM$ nip 2 index 3 mod 0= if 1+ then T=
   owner XML:OWNER-ORIGINAL$ nip 0 > TTRUE
   owner XML:OWNER-LEXICAL$ nip 0 > TTRUE
   owner XML:OWNER-ENCODING ENCODING>N
   index 3 mod 0= if XML-ENCODING:UTF8 else
      index 3 mod 1 = if XML-ENCODING:UTF16-LE else XML-ENCODING:UTF16-BE then
   then ENCODING>N T=
   index ATTR-LEN @ LEN>N 7 T=
   index TEXT-LEN @ LEN>N index 3 mod 0= if 7 else 12 then T= ;

: REPARSE ( n -- )
   {: written:n :}
   HEAP:ALLOCATOR WRITTEN written XML:SOURCE-INIT
   RD XML:INIT-SOURCE
   XML-KIND:START NEXT=
   XML:ATTR-NEXT TTRUE
   CLEARED XML:ATTR-TEXT DECODED$ s" aQb" T$=
   XML-KIND:TEXT NEXT=
   CLEARED XML:TEXT DECODED$ s" Ax🚗Y" T$=
   XML-KIND:END NEXT= XML-KIND:COMMENT NEXT=
   XML-KIND:COMMENT NEXT= XML-KIND:EOF NEXT=
   XML:CLOSE XML:SOURCE-CLOSE ;

: WRITE-PART ( n -- )
   {: index:n :}
   index OWNERS @ {: owner :}
   owner index ATTR-OFF @ OFF>N 1+ >OFF 5 >LEN XML:OWNER-RANGE
   {: attr-off:off attr-len:len :}
   owner index TEXT-OFF @ OFF>N 2 + >OFF
   index 3 mod 0= if 4 else 9 then >LEN
   XML:OWNER-RANGE {: text-off:off text-len:len :}
   owner owner XML:OWNER-LEXICAL$ nip >OFF 0 >LEN XML:OWNER-RANGE
   {: end-off:off end-len:len :}
   end-len LEN>N 0 T=
   owner s" Q" XML:OWNER-ENCODE-SIZE
   index 3 mod 0= if 1 else 2 then T=
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: out:^ :}
   owner s" Q" out XML:OWNER-ENCODE
   out VEC:COUNT {: attr-size:n :}
   owner s" 🚗" XML:OWNER-ENCODE-SIZE {: wanted:n :}
   owner s" 🚗" out XML:OWNER-ENCODE
   out VEC:COUNT attr-size - dup wanted T=
   {: text-size:n :}
   owner s" <!--late-->" out XML:OWNER-ENCODE
   out VEC:COUNT attr-size - text-size - {: tail-size:n :}
   out VEC:SPAN SPAN:$ drop {: encoded :}
   owner XML:OWNER-ORIGINAL$ {: original original-size:n :}
   end-off OFF>N original-size T=
   EDIT-STORE 3 EDIT:STORAGE-BYTES original original-size EDIT:INIT
   attr-off attr-len encoded attr-size EDIT:REPLACE
   text-off text-len encoded attr-size + text-size EDIT:REPLACE
   end-off end-len encoded attr-size + text-size + tail-size EDIT:REPLACE
   WRITTEN OUTPUT-CAP EDIT:WRITE {: written:n :}
   EDIT:CLOSE
   out VEC:DELETE
   WRITTEN attr-off OFF>N original attr-off OFF>N T$=
   WRITTEN BYTE-VIEW index 3 mod 0= if 3 else 2 then
   original index 3 mod 0= if 3 else 2 then T$=
   attr-off OFF>N {: a:n :} attr-len LEN>N {: ar:n :}
   text-off OFF>N {: t:n :} text-len LEN>N {: tr:n :}
   WRITTEN a attr-size + + t a ar + -
   original a ar + + t a ar + - T$=
   WRITTEN t attr-size ar - + text-size + + original-size t tr + -
   original t tr + + original-size t tr + - T$=
   written REPARSE
   index 3 mod 0= if s" part-utf8.xml" else
      index 3 mod 1 = if s" part-utf16le.xml" else s" part-utf16be.xml" then
   then
   0 SCRIPT-ARGV$ 2swap INPUT FS:JOIN-PATH INPUT swap
   WRITTEN written FS:WRITE-ALL ;

: LIFETIME ( -- )
   0 OWNERS @ XML:OWNER-CLOSE
   1 FIRST-OWNER ! ;

: DOCUMENT ( -- )
   7 false OPEN
   7 0 ?do i CHECK-OWNER loop
   6 WRITE-PART 0 WRITE-PART
   6 1 ?do i WRITE-PART loop
   LIFETIME
   6 OWNERS @ XML:OWNER-LEXICAL$ nip 0 > TTRUE
   CLOSE-PARTS ;

\ The source takes its storage from the arena, between the blocks asked for
\ before and after it; the arena's release ends the owner nobody closed.
: ARENA-OWNER ( -- )
   ARENA:NEW {: a:^ :}
   a 1 ARENA:ALLOC drop NULL-PTR - {: before:n :}
   a ARENA:ALLOCATOR s" <r q='a&amp;b'>Ax&#x1F600;Y</r>" true UTF16$
   XML:SOURCE-INIT XML:SOURCE-OWN {: owner :}
   a 1 ARENA:ALLOC drop NULL-PTR - {: after:n :}
   owner XML:OWNER-LEXICAL$ {: lexical size:n :}
   lexical NULL-PTR - before > TTRUE
   lexical NULL-PTR - size + after <= TTRUE
   RD owner XML:OWNER-READER
   XML-KIND:START NEXT=
   XML:ATTR-NEXT TTRUE
   CLEARED XML:ATTR-TEXT DECODED$ s" a&b" T$=
   XML-KIND:TEXT NEXT=
   CLEARED XML:TEXT DECODED$ s" Ax😀Y" T$=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT=
   XML:CLOSE
   a ARENA:RELEASE ;

: BAD-OPEN ( -- ) 4 true OPEN ;
: BAD-WIRE ( -- ) true INVALID-LAST ! 4 false OPEN ;

public
: MAIN ( -- )
   T-RESET
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW DECODED !
   s" bounded callback XML survives source and native capture" T-LABEL
   STREAM-READ
   s" runtime-sized owners survive parser scratch reuse and later edits" T-LABEL
   DOCUMENT
   s" an owner on an arena's allocator ends with the arena" T-LABEL
   ARENA-OWNER
   s" a malformed later part preserves the parser error and frees prior parts" T-LABEL
   ['] BAD-OPEN catch XML:E-MALFORMED T=
   s" a failed source initialization publishes no owner" T-LABEL
   ['] BAD-WIRE catch XML:E-UTF8 T=
   false INVALID-LAST !
   2 false OPEN
   2 0 ?do i CHECK-OWNER loop
   CLOSE-PARTS
   DECODED @ VEC:DELETE
   T-REPORT
   s" xml-owner-consumer: ok" type cr ;
;package

: MAIN ( -- ) XML-OWNER-CONSUMER:MAIN ;

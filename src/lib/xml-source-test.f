split-str
require src/lib/test.f
require src/lib/xml.f
require src/lib/byte-edit.f
require src/lib/allocator.f
require src/lib/heap.f
require src/lib/span.f
require src/lib/vector.f
require src/lib/reason.f
require src/lib/vector.f
require src/lib/heap.f

package XML-SOURCE-TEST

create EDIT-STORE 4 EDIT:STORAGE-BYTES allot
TYPED-VARIABLE ENCODED-OUT ^VEC:vector<u8>
create WRITTEN 256 allot
TYPED-VARIABLE DECODED ^VEC:vector<u8>
create BOMLESS 256 allot
create BOM-INPUT 256 allot
TYPED-VARIABLE ASCII-OWNER XML:source-owner
TYPED-VARIABLE RD XML:cursor
1024 constant MARKED-CAP
create MARKED MARKED-CAP allot
create MARKED-AT MARKED-CAP cells allot
variable MARKED-LEX
variable MARKED-WIRE
variable PROBE
100000 constant CHILDREN
variable WIDE-BAD

CAST: KIND>N ( XML:kind -- n )
CAST: ENCODING>N ( XML:encoding -- n )

: LE$ ( -- str )
   s\" \xFF\xFE<\za\z \zq\z=\z'\zx\z'\z>\z&\za\zm\zp\z;\z<\z/\za\z>\z" ;

: BE$ ( -- str )
   s\" \xFE\xFF\z<\za\z>\zx\z<\z/\za\z>" ;

: ASCII>UTF16 ( str bool -- str )
   {: ascii size:n little:bool :}
   size 0 ?do
      ascii i + c@ {: byte:n :}
      little if
         byte BOMLESS i 2 * + c!
         0 BOMLESS i 2 * 1+ + c!
      else
         0 BOMLESS i 2 * + c!
         byte BOMLESS i 2 * 1+ + c!
      then
   loop
   BOMLESS size 2 * ;

: BOM-ASCII ( str bool -- str )
   {: ascii size:n little:bool :}
   ascii size little ASCII>UTF16 {: encoded count:n :}
   little if
      $FF BOM-INPUT c! $FE BOM-INPUT 1+ c!
   else
      $FE BOM-INPUT c! $FF BOM-INPUT 1+ c!
   then
   encoded BOM-INPUT 2 + count BYTE-COPY
   BOM-INPUT count 2 + ;

: OPEN ( str -- XML:source )
   HEAP:ALLOCATOR -rot XML:SOURCE-INIT ;

: READER ( XML:source -- XML:source XML:reader ) RD XML:INIT-SOURCE ;

: DECODED-NEW ( -- ) HEAP:ALLOCATOR 0 [type] u8 VEC:NEW DECODED ! ;
: CLEARED ( -- ^VEC:vector<u8> ) DECODED @ dup 0 VEC:RESIZE ;
: DECODED$ ( -- str ) DECODED @ VEC:SPAN SPAN:$ ;

: NEXT= ( XML:reader XML:kind -- XML:reader )
   KIND>N {: want:n :}
   XML:NEXT KIND>N want T= ;

: DRAIN ( XML:reader -- XML:reader )
   begin XML:NEXT KIND>N XML-KIND:EOF KIND>N <> while repeat ;

: UTF16-READ ( -- )
   LE$ OPEN
   XML:SOURCE-ENCODING ENCODING>N XML-ENCODING:UTF16-LE ENCODING>N T=
   XML:SOURCE-BOM$ s\" \xFF\xFE" T$=
   XML:SOURCE-LEXICAL$ s" <a q='x'>&amp;</a>" T$=
   READER
   XML-KIND:START NEXT=
   XML:RAW LEN>N 9 T= OFF>N 0 T=
   XML:ATTR-NEXT TTRUE
   XML:ATTR-VALUE$ s" x" T$=
   XML-KIND:TEXT NEXT=
   CLEARED XML:TEXT DECODED$ s" &" T$=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT=
   XML:CLOSE XML:SOURCE-CLOSE
   BE$ OPEN
   XML:SOURCE-ENCODING ENCODING>N XML-ENCODING:UTF16-BE ENCODING>N T=
   XML:SOURCE-LEXICAL$ s" <a>x</a>" T$=
   READER XML-KIND:START NEXT= XML-KIND:TEXT NEXT=
   CLEARED XML:TEXT DECODED$ s" x" T$=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT=
   XML:CLOSE XML:SOURCE-CLOSE ;

: UTF8-READ ( -- )
   s\" \xEF\xBB\xBF<a>é😀</a>" OPEN
   XML:SOURCE-ENCODING ENCODING>N XML-ENCODING:UTF8 ENCODING>N T=
   XML:SOURCE-BOM$ s\" \xEF\xBB\xBF" T$=
   XML:SOURCE-LEXICAL$ s" <a>é😀</a>" T$=
   READER XML-KIND:START NEXT= XML-KIND:TEXT NEXT=
   CLEARED XML:TEXT DECODED$ s" é😀" T$=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT=
   XML:CLOSE XML:SOURCE-CLOSE ;

: ASCII-READ ( -- )
   s\" <?xml version='1.0' encoding='us-ascii'?><a q=\q&#x4E2D;\q>&#x4E2D;</a>"
   OPEN
   XML:SOURCE-ENCODING ENCODING>N XML-ENCODING:US-ASCII ENCODING>N T=
   XML:SOURCE-BOM$ nip 0 T=
   XML:SOURCE-LEXICAL$ {: lexical size:n :}
   lexical size s\" <?xml version='1.0' encoding='us-ascii'?><a q=\q&#x4E2D;\q>&#x4E2D;</a>" T$=
   READER XML-KIND:PI NEXT= XML-KIND:START NEXT=
   XML:ATTR-NEXT TTRUE
   CLEARED XML:ATTR-TEXT DECODED$ s" 中" T$=
   XML-KIND:TEXT NEXT=
   CLEARED XML:TEXT DECODED$ s" 中" T$=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT=
   XML:CLOSE XML:SOURCE-CLOSE
   s\" <?xml version='1.0' encoding=\quS-aScIi\q?><a/>" OPEN
   XML:SOURCE-ENCODING ENCODING>N XML-ENCODING:US-ASCII ENCODING>N T=
   XML:SOURCE-CLOSE
   s" <?xml-stylesheet encoding='US-ASCII'?><a>é</a>" OPEN
   XML:SOURCE-ENCODING ENCODING>N XML-ENCODING:UTF8 ENCODING>N T=
   READER XML-KIND:PI NEXT= XML-KIND:START NEXT=
   XML-KIND:TEXT NEXT= CLEARED XML:TEXT DECODED$ s" é" T$=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT=
   XML:CLOSE XML:SOURCE-CLOSE ;

: ASCII-OUTPUT ( -- )
   s" <?xml version='1.0' encoding='US-ASCII'?><a>old</a>" OPEN
   XML:SOURCE-OWN dup ASCII-OWNER ! {: owner :}
   owner XML:OWNER-ENCODING ENCODING>N XML-ENCODING:US-ASCII ENCODING>N T=
   RD owner XML:OWNER-READER
   XML-KIND:PI NEXT= XML-KIND:START NEXT=
   owner XML:OWNER-ENCODING ENCODING>N XML-ENCODING:US-ASCII ENCODING>N T=
   XML-KIND:TEXT NEXT= XML:CONTENT {: text-off:off text-len:len :}
   XML-KIND:END NEXT= XML-KIND:EOF NEXT= XML:CLOSE
   owner text-off text-len XML:OWNER-RANGE
   {: edit-off:off edit-len:len :}
   edit-len LEN>N 3 T= edit-off OFF>N 44 T=
   owner s" &#x4E2D;" XML:OWNER-ENCODE-SIZE 8 T=
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: out:^ :}
   out ENCODED-OUT !
   owner s" &#x4E2D;" out XML:OWNER-ENCODE
   out VEC:SPAN SPAN:$ s" &#x4E2D;" T$=
   [: ASCII-OWNER @ s" 中" XML:OWNER-ENCODE-SIZE drop ;]
   XML:E-ENCODING TTHROWSQ
   [: ASCII-OWNER @ s" 中" ENCODED-OUT @ XML:OWNER-ENCODE ;]
   XML:E-ENCODING TTHROWSQ
   [: ASCII-OWNER @ s\" \z" XML:OWNER-ENCODE-SIZE drop ;]
   XML:E-SCALAR TTHROWSQ
   out VEC:SPAN SPAN:$ s" &#x4E2D;" T$=
   owner XML:OWNER-ORIGINAL$ {: original size:n :}
   EDIT-STORE 1 EDIT:STORAGE-BYTES original size EDIT:INIT
   edit-off edit-len out VEC:SPAN SPAN:$ EDIT:REPLACE
   WRITTEN 256 EDIT:WRITE {: written:n :}
   EDIT:CLOSE
   out VEC:DELETE
   WRITTEN written s" <?xml version='1.0' encoding='US-ASCII'?><a>&#x4E2D;</a>" T$=
   WRITTEN written OPEN
   READER XML-KIND:PI NEXT= XML-KIND:START NEXT=
   XML-KIND:TEXT NEXT= CLEARED XML:TEXT DECODED$ s" 中" T$=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT= XML:CLOSE XML:SOURCE-CLOSE
   owner XML:OWNER-CLOSE ;

: DECLARED-BOM ( -- )
   s\" <?xml version=\q1.0\q encoding='UTF-16'?><p:a xmlns:p=\qu\q q='x&amp;y'>a\r\n<![CDATA[<b>]]><?go v?></p:a><!--keep-->"
   false BOM-ASCII OPEN
   XML:SOURCE-LEXICAL$
   s\" <?xml version=\q1.0\q encoding='UTF-16'?><p:a xmlns:p=\qu\q q='x&amp;y'>a\r\n<![CDATA[<b>]]><?go v?></p:a><!--keep-->"
   T$=
   READER
   XML-KIND:PI NEXT=
   XML-KIND:START NEXT=
   CLEARED XML:URI DECODED$ s\" u" T$=
   s" p" CLEARED XML:PREFIX-URI TTRUE
   DECODED$ s" u" T$=
   XML:ATTR-NEXT TTRUE XML:ATTR-NEXT TTRUE
   XML:ATTR-VALUE$ s\" x&amp;y" T$=
   CLEARED XML:ATTR-TEXT DECODED$ s\" x&y" T$=
   XML-KIND:TEXT NEXT=
   CLEARED XML:TEXT DECODED$ s\" a\n" T$=
   XML-KIND:CDATA NEXT=
   CLEARED XML:TEXT DECODED$ s\" <b>" T$=
   XML-KIND:PI NEXT= XML-KIND:END NEXT=
   XML-KIND:COMMENT NEXT=
   CLEARED XML:TEXT DECODED$ s\" keep" T$=
   XML-KIND:EOF NEXT= XML:CLOSE XML:SOURCE-CLOSE
   s\" <?xml version='1.0' encoding='UTF-16'?><a/>"
   true BOM-ASCII OPEN READER
   XML-KIND:PI NEXT= XML-KIND:START NEXT= XML-KIND:END NEXT=
   XML-KIND:EOF NEXT= XML:CLOSE XML:SOURCE-CLOSE ;

: UNICODE-UTF16 ( -- )
   s\" \xFF\xFE<\z\xE9\z>\z\x3D\xD8\z\xDE<\z/\z\xE9\z>\z" OPEN
   READER XML-KIND:START NEXT=
   XML:NAME$ s\" é" T$=
   XML-KIND:TEXT NEXT=
   CLEARED XML:TEXT DECODED$ s\" 😀" T$=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT=
   XML:CLOSE XML:SOURCE-CLOSE
   s\" \xFE\xFF\z<\z\xE9\z>\xD8\x3D\xDE\z\z<\z/\z\xE9\z>" OPEN
   READER XML-KIND:START NEXT=
   XML:NAME$ s\" é" T$=
   XML-KIND:TEXT NEXT=
   CLEARED XML:TEXT DECODED$ s\" 😀" T$=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT=
   XML:CLOSE XML:SOURCE-CLOSE ;

: INTERLEAVED ( -- )
   LE$ OPEN
   BE$ OPEN
   XML:SOURCE-ENCODING ENCODING>N
   XML-ENCODING:UTF16-BE ENCODING>N T=
   swap XML:SOURCE-ENCODING ENCODING>N
   XML-ENCODING:UTF16-LE ENCODING>N T=
   swap XML:SOURCE-CLOSE XML:SOURCE-CLOSE ;

: BOMLESS-READ ( -- )
   s\" <?xml version='1.0' encoding='UTF-16LE'?><a>x</a>"
   true ASCII>UTF16 OPEN
   XML:SOURCE-BOM$ nip 0 T=
   XML:SOURCE-LEXICAL$
   s\" <?xml version='1.0' encoding='UTF-16LE'?><a>x</a>" T$=
   READER
   XML-KIND:PI NEXT= XML-KIND:START NEXT= XML-KIND:TEXT NEXT=
   CLEARED XML:TEXT DECODED$ s\" x" T$=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT=
   XML:CLOSE XML:SOURCE-CLOSE
   s\" <?xml version='1.0' encoding='utf-16be'?><a>x</a>"
   false ASCII>UTF16 OPEN
   XML:SOURCE-BOM$ nip 0 T=
   READER
   XML-KIND:PI NEXT= XML-KIND:START NEXT= XML-KIND:TEXT NEXT=
   CLEARED XML:TEXT DECODED$ s\" x" T$=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT=
   XML:CLOSE XML:SOURCE-CLOSE ;

: BAD-NEXT ( str n -- )
   {: want:n :}
   OPEN READER
   ['] DRAIN catch want T=
   ['] DRAIN catch XML:E-STATE T=
   XML:CLOSE XML:SOURCE-CLOSE ;

: BAD-INIT ( str n -- )
   {: want:n :}
   [: 2dup OPEN XML:SOURCE-CLOSE ;]
   catch want T= 2drop ;

\ A rejection's text: the place, what was expected, the token found and the fix.
: SAID ( n str -- ) {: code:n wa:^ wu:n :}
   code REASON:TEXT$ wa wu T$=
   REASON:RELEASE ;

: ASCII-REJECTIONS ( -- )
   s" <?xml version='1.0' encoding='ISO-8859-1'?><a/>"
      XML:E-ENCODING BAD-NEXT
   s\" <?xml version='1.0' encoding='US-ASCII'?><a>é</a>"
      XML:E-ENCODING BAD-INIT
   s\" <?xml version='1.0' encoding='US-ASCII'?><é/>"
      XML:E-ENCODING BAD-INIT
   s\" <?xml version='1.0' encoding='US-ASCII'?><a><!--é--></a>"
      XML:E-ENCODING BAD-INIT
   s\" <?xml version='1.0' encoding='US-ASCII'?><a><![CDATA[é]]></a>"
      XML:E-ENCODING BAD-INIT
   s\" <?xml version='1.0' encoding='US-ASCII'?><a/><?p é?>"
      XML:E-ENCODING BAD-INIT
   s\" <?xml version='1.0' encoding='US-ASCII'?><a/>é"
      XML:E-ENCODING BAD-INIT
   s\" \xEF\xBB\xBF<?xml version='1.0' encoding='US-ASCII'?><a/>"
      XML:E-ENCODING BAD-INIT
   XML:E-ENCODING s" 1:34: expected an encoding that matches the document's bytes, such as `UTF-8` or `UTF-16`, found `US-ASCII`; declare the encoding the document is saved in, or save it as US-ASCII without a byte order mark" SAID
   s\" <?xml version='1.0' encoding='US-ASCII'?><a/>"
      true ASCII>UTF16 XML:E-ENCODING BAD-INIT
   XML:E-ENCODING s" 1:31: expected an encoding that matches the document's bytes, such as `UTF-8` or `UTF-16`, found unit $0055; declare the encoding the document is saved in, or save it as US-ASCII without a byte order mark" SAID
   s\" <?xml version='1.0' encoding='US-ASCII'?><a/>"
      false ASCII>UTF16 XML:E-ENCODING BAD-INIT ;

: BOMLESS-REJECTIONS ( -- )
   s\" <a/>" true ASCII>UTF16 XML:E-ENCODING BAD-INIT
   XML:E-ENCODING s" 1:1: expected a byte order mark or `<?xml` at the start of a UTF-16 document, found unit $003C; begin the document with a byte order mark or an XML declaration, or save it as UTF-8" SAID
   s\" <a/>" false ASCII>UTF16 XML:E-ENCODING BAD-INIT
   s\" <?xml version='1.0'?><a/>"
   true ASCII>UTF16 XML:E-ENCODING BAD-NEXT
   s\" <?xml-stylesheet x?><a/>"
   false ASCII>UTF16 XML:E-ENCODING BAD-NEXT
   s\" <?xml version='1.0' encoding='UTF-16BE'?><a/>"
   true ASCII>UTF16 XML:E-ENCODING BAD-NEXT
   s\" <?xml version='1.0' encoding='UTF-16LE'?><a/>"
   false ASCII>UTF16 XML:E-ENCODING BAD-NEXT
   s\" <?xml version='1.0' encoding='UTF-16'?><a/>"
   true ASCII>UTF16 XML:E-ENCODING BAD-NEXT
   s\" <?xml version='1.0' encoding='UTF-16LE'"
   true ASCII>UTF16 XML:E-TRUNCATED BAD-NEXT
   s\" <?xml version '1.0' encoding='UTF-16LE'?><a/>"
   true ASCII>UTF16 XML:E-MALFORMED BAD-NEXT
   s\"  <?xml version='1.0' encoding='UTF-16LE'?><a/>"
   true ASCII>UTF16 XML:E-SCALAR BAD-INIT ;

: BOMLESS-EDIT ( bool -- )
   {: little:bool :}
   little if
      s\" <?xml version='1.0' encoding='UTF-16LE'?><a>x|KEEP</a>"
   else
      s\" <?xml version='1.0' encoding='UTF-16BE'?><a>x|KEEP</a>"
   then
   little ASCII>UTF16 OPEN
   READER XML-KIND:PI NEXT= XML-KIND:START NEXT= XML-KIND:TEXT NEXT=
   XML:CONTENT {: text-off:off text-len:len :}
   XML:CLOSE
   text-len LEN>N 6 T=
   text-off 1 >LEN XML:SOURCE-RANGE
   {: edit-off:off edit-len:len :}
   edit-len LEN>N 2 T=
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: out:^ :}
   s\" A🚗" out XML:ENCODE
   out VEC:SPAN SPAN:$ {: encoded added:n :}
   added 6 T=
   little if
      encoded added s\" A\z\x3D\xD8\x97\xDE" T$=
   else
      encoded added s\" \zA\xD8\x3D\xDE\x97" T$=
   then
   XML:SOURCE-ORIGINAL$ {: original original-len:n :}
   EDIT-STORE 2 EDIT:STORAGE-BYTES original original-len EDIT:INIT
   edit-off edit-len encoded added EDIT:REPLACE
   WRITTEN 256 EDIT:WRITE {: written:n :}
   EDIT:CLOSE XML:SOURCE-CLOSE
   out VEC:DELETE
   edit-off OFF>N {: start:n :}
   edit-len LEN>N {: removed:n :}
   WRITTEN BYTE-VIEW start original start T$=
   WRITTEN BYTE-VIEW start added + + written start added + -
   original start removed + + original-len start removed + -
   T$=
   WRITTEN written OPEN
   XML:SOURCE-BOM$ nip 0 T=
   READER XML-KIND:PI NEXT= XML-KIND:START NEXT= XML-KIND:TEXT NEXT=
   CLEARED XML:TEXT DECODED$ s\" A🚗|KEEP" T$=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT=
   XML:CLOSE XML:SOURCE-CLOSE ;

: COMPOSED-EDIT ( bool -- )
   {: little:bool :}
   s\" <?xml version='1.0' encoding='UTF-16'?><a q='old'>KEEP<![CDATA[old]]>tail</a><!--keep-->"
   little BOM-ASCII OPEN
   READER XML-KIND:PI NEXT= XML-KIND:START NEXT=
   XML:ATTR-NEXT TTRUE
   XML:ATTR-VALUE {: attr-off:off attr-len:len :}
   XML-KIND:TEXT NEXT=
   XML:CONTENT {: insert-off:off keep-len:len :}
   keep-len LEN>N 4 T=
   XML-KIND:CDATA NEXT=
   XML:CONTENT {: delete-off:off delete-len:len :}
   XML-KIND:TEXT NEXT= XML-KIND:END NEXT=
   XML-KIND:COMMENT NEXT= XML-KIND:EOF NEXT=
   XML:CLOSE
   attr-off attr-len XML:SOURCE-RANGE
   {: attr-wire:off attr-old:len :}
   insert-off 0 >LEN XML:SOURCE-RANGE
   {: insert-wire:off insert-old:len :}
   insert-old LEN>N 0 T=
   delete-off delete-len XML:SOURCE-RANGE
   {: delete-wire:off delete-old:len :}
   XML:SOURCE-LEXICAL$ nip >OFF 0 >LEN XML:SOURCE-RANGE
   {: eof-wire:off eof-old:len :}
   eof-old LEN>N 0 T=
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: out:^ :}
   s\" A🚗" out XML:ENCODE
   out VEC:COUNT {: attr-added:n :}
   s\" pre-" out XML:ENCODE
   out VEC:COUNT attr-added - {: insert-added:n :}
   s\" " out XML:ENCODE
   out VEC:COUNT attr-added insert-added + T=
   out VEC:SPAN SPAN:$ drop {: encoded :}
   little if
      encoded attr-added s\" A\z\x3D\xD8\x97\xDE" T$=
   else
      encoded attr-added s\" \zA\xD8\x3D\xDE\x97" T$=
   then
   XML:SOURCE-ORIGINAL$ {: original original-len:n :}
   EDIT-STORE 4 EDIT:STORAGE-BYTES original original-len EDIT:INIT
   attr-wire attr-old encoded attr-added EDIT:REPLACE
   insert-wire insert-old encoded attr-added + insert-added EDIT:REPLACE
   delete-wire delete-old encoded 0 EDIT:REPLACE
   eof-wire eof-old encoded 0 EDIT:REPLACE
   WRITTEN 256 EDIT:WRITE {: written:n :}
   EDIT:CLOSE XML:SOURCE-CLOSE
   out VEC:DELETE
   attr-wire OFF>N {: a:n :}
   attr-old LEN>N {: ar:n :}
   insert-wire OFF>N {: b:n :}
   delete-wire OFF>N {: c:n :}
   delete-old LEN>N {: cr:n :}
   attr-added ar T=
   WRITTEN BYTE-VIEW a original a T$=
   WRITTEN BYTE-VIEW a attr-added + + b a ar + -
   original a ar + + b a ar + - T$=
   WRITTEN BYTE-VIEW b insert-added + + c b -
   original b + c b - T$=
   WRITTEN BYTE-VIEW c insert-added + + written c insert-added + -
   original c cr + + original-len c cr + - T$=
   WRITTEN written OPEN
   XML:SOURCE-BOM$ nip 2 T=
   READER XML-KIND:PI NEXT= XML-KIND:START NEXT=
   XML:ATTR-NEXT TTRUE
   CLEARED XML:ATTR-TEXT DECODED$ s\" A🚗" T$=
   XML-KIND:TEXT NEXT=
   CLEARED XML:TEXT DECODED$ s\" pre-KEEP" T$=
   XML-KIND:CDATA NEXT=
   CLEARED XML:TEXT DECODED @ VEC:COUNT 0 T=
   XML-KIND:TEXT NEXT=
   CLEARED XML:TEXT DECODED$ s\" tail" T$=
   XML-KIND:END NEXT= XML-KIND:COMMENT NEXT=
   CLEARED XML:TEXT DECODED$ s\" keep" T$=
   XML-KIND:EOF NEXT= XML:CLOSE XML:SOURCE-CLOSE ;

: WRITE-TINY ( EDIT:editor -- EDIT:editor )
   WRITTEN 1 EDIT:WRITE drop ;

: EDIT-LE ( -- )
   s\" \xFF\xFE<\za\z>\zx\z<\z/\za\z>\z" OPEN
   READER XML-KIND:START NEXT= XML-KIND:TEXT NEXT=
   XML:CONTENT {: text-off:off text-len:len :}
   XML:CLOSE
   text-off text-len XML:SOURCE-RANGE {: edit-off:off edit-len:len :}
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: out:^ :}
   s" é" out XML:ENCODE
   out VEC:SPAN SPAN:$ {: encoded added:n :}
   encoded added s\" \xE9\z" T$=
   XML:SOURCE-ORIGINAL$ {: original original-len:n :}
   EDIT-STORE 2 EDIT:STORAGE-BYTES original original-len EDIT:INIT
   edit-off edit-len encoded added EDIT:REPLACE
   $5A WRITTEN c!
   ['] WRITE-TINY catch EDIT:E-CAPACITY T=
   WRITTEN c@ $5A T=
   WRITTEN 256 EDIT:WRITE {: written:n :}
   EDIT:CLOSE XML:SOURCE-CLOSE
   out VEC:DELETE
   WRITTEN written s\" \xFF\xFE<\za\z>\z\xE9\z<\z/\za\z>\z" T$=
   WRITTEN written OPEN
   READER XML-KIND:START NEXT= XML-KIND:TEXT NEXT=
   CLEARED XML:TEXT DECODED$ s" é" T$=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT=
   XML:CLOSE XML:SOURCE-CLOSE ;

: MID-RANGE ( XML:source -- XML:source )
   4 >OFF 0 >LEN XML:SOURCE-RANGE 2drop ;

: END-MID-RANGE ( XML:source -- XML:source )
   3 >OFF 1 >LEN XML:SOURCE-RANGE 2drop ;

: NEGATIVE-RANGE ( XML:source -- XML:source )
   -1 >OFF 0 >LEN XML:SOURCE-RANGE 2drop ;

: HUGE-RANGE ( XML:source -- XML:source )
   $7FFFFFFFFFFFFFFF >OFF 1 >LEN XML:SOURCE-RANGE 2drop ;

: BOUNDARIES-AND-RECOVERY ( -- )
   s\" \xFF\xFE<\za\z>\z\x3D\xD8\z\xDE<\z/\za\z>\z" OPEN
   0 >OFF 0 >LEN XML:SOURCE-RANGE
   LEN>N 0 T= OFF>N 2 T=
   3 >OFF 4 >LEN XML:SOURCE-RANGE
   LEN>N 4 T= OFF>N 8 T=
   11 >OFF 0 >LEN XML:SOURCE-RANGE
   LEN>N 0 T= OFF>N 20 T=
   ['] MID-RANGE catch XML:E-BOUNDARY T=
   ['] END-MID-RANGE catch XML:E-BOUNDARY T=
   ['] NEGATIVE-RANGE catch XML:E-RANGE T=
   ['] HUGE-RANGE catch XML:E-RANGE T=
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: out:^ :}
   s\" A🚗" out XML:ENCODE
   out VEC:COUNT 6 T=
   out VEC:DELETE
   READER XML-KIND:START NEXT= XML-KIND:TEXT NEXT=
   CLEARED XML:TEXT DECODED$ s\" 😀" T$=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT=
   XML:CLOSE XML:SOURCE-CLOSE ;

: UTF8-RANGES ( -- )
   s\" \xEF\xBB\xBF<a>é😀</a>" OPEN
   0 >OFF 0 >LEN XML:SOURCE-RANGE
   LEN>N 0 T= OFF>N 3 T=
   3 >OFF 6 >LEN XML:SOURCE-RANGE
   LEN>N 6 T= OFF>N 6 T=
   5 >OFF 4 >LEN XML:SOURCE-RANGE
   LEN>N 4 T= OFF>N 8 T=
   13 >OFF 0 >LEN XML:SOURCE-RANGE
   LEN>N 0 T= OFF>N 16 T=
   ['] MID-RANGE catch XML:E-BOUNDARY T=
   ['] END-MID-RANGE catch XML:E-BOUNDARY T=
   ['] NEGATIVE-RANGE catch XML:E-RANGE T=
   ['] HUGE-RANGE catch XML:E-RANGE T=
   XML:SOURCE-CLOSE
   s" <a>é😀</a>" OPEN
   3 >OFF 6 >LEN XML:SOURCE-RANGE
   LEN>N 6 T= OFF>N 3 T=
   13 >OFF 0 >LEN XML:SOURCE-RANGE
   LEN>N 0 T= OFF>N 13 T=
   XML:SOURCE-CLOSE ;

\ An allocator bumping through POOL whose resize moves the block and fills
\ the old one with $A5, so a read of the old block shows.
256 constant POOL-BYTES
POOL-BYTES BUFFER: POOL
variable TOP
TYPED-VARIABLE MOVER ALLOC:allocator

: P-ALLOC ( str -- ^u8 )
   {: st:^ len:n :}
   TOP @ len + POOL-BYTES > if E-MEM-SIZE throw then
   st TOP @ +  len 7 + -8 and TOP +! ;

: P-RESIZE ( ^u8 str n -- ^u8 )
   {: st:^ block:^ old:n new:n :}
   st new P-ALLOC {: moved:^ :}
   block moved old new min BYTE-COPY
   $A5 block old SPAN:MAKE SPAN:FILL  moved ;

: P-FREE ( ^u8 ^u8 -- ) 2drop ;

: APPENDS-KEEP ( -- )
   s" <a/>" OPEN
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: out:^ :}
   s" ab" out VEC:APPEND
   s" cd" out XML:ENCODE
   s" <e" out XML:ESCAPE-TEXT
   s" 'f" out XML:ESCAPE-ATTR
   out VEC:SPAN SPAN:$ s" abcd&lt;e&apos;f" T$=
   out VEC:DELETE
   XML:SOURCE-CLOSE ;

\ The vector's own bytes escaped into it while its block moves: TRIM leaves
\ no room, so the escape grows the block.
: ESCAPE-OWN ( -- )
   POOL [: P-ALLOC ;] [: P-RESIZE ;] [: P-FREE ;] MOVER ALLOC:INIT
   MOVER 0 [type] u8 VEC:NEW {: v:^ :}
   s" a<b" v VEC:APPEND
   v 3 VEC:TRIM
   v VEC:SPAN SPAN:$ v XML:ESCAPE-TEXT
   v VEC:SPAN SPAN:$ s" a<ba&lt;b" T$=
   v VEC:DELETE ;

\ An allocator over the heap that counts the blocks it gives and frees.
variable TAKEN
variable FREED
TYPED-VARIABLE COUNTER ALLOC:allocator

: C-ALLOC ( ^u8 n -- ^u8 )
   {: st:^ len:n :}
   1 TAKEN +!  HEAP:ALLOCATOR len ALLOC:BYTES drop ;

: C-RESIZE ( ^u8 ^u8 n n -- ^u8 )
   {: st:^ block:^ old:n new:n :}
   HEAP:ALLOCATOR block old new ALLOC:RESIZE drop ;

: C-FREE ( ^u8 ^u8 -- )
   {: st:^ block:^ :}
   1 FREED +!  HEAP:ALLOCATOR block ALLOC:FREE ;

: COUNTED-STORAGE ( -- )
   NULL-PTR [: C-ALLOC ;] [: C-RESIZE ;] [: C-FREE ;] COUNTER ALLOC:INIT
   s" a rejected document takes no storage" T-LABEL
   [: COUNTER s\" \xFF\xFE\x00\xDC" XML:SOURCE-INIT XML:SOURCE-CLOSE ;]
   catch XML:E-UTF16 T=
   TAKEN @ 0 T=
   s" a source's close frees the storage it took" T-LABEL
   COUNTER LE$ XML:SOURCE-INIT XML:SOURCE-CLOSE
   TAKEN @ 1 T= FREED @ 1 T=
   COUNTER s" <a/>" XML:SOURCE-INIT XML:SOURCE-OWN XML:OWNER-CLOSE
   TAKEN @ 2 T= FREED @ 2 T= ;

\ MARKED holds a UTF-16 text built scalar by scalar; MARKED-AT holds, per
\ lexical offset, the original offset that scalar was written at, or -1 inside
\ one.
: MARK-UNIT ( n bool -- )
   {: unit:n little:bool :}
   MARKED-WIRE @ {: at:n :}
   little if
      unit $FF and MARKED at + c!
      unit 8 rshift MARKED at 1+ + c!
   else
      unit 8 rshift MARKED at + c!
      unit $FF and MARKED at 1+ + c!
   then
   at 2 + MARKED-WIRE ! ;


\ Append scalar, width bytes of lexical UTF-8, as one unit or a surrogate pair.
: MARK-SCALAR ( n n bool -- )
   {: scalar:n width:n little:bool :}
   MARKED-LEX @ {: lex:n :}
   MARKED-WIRE @ MARKED-AT lex cells + !
   width 1 ?do -1 MARKED-AT lex i + cells + ! loop
   lex width + MARKED-LEX !
   scalar $10000 < if scalar little MARK-UNIT exit then
   scalar $10000 - {: pair:n :}
   pair 10 rshift $D800 + little MARK-UNIT
   pair $3FF and $DC00 + little MARK-UNIT ;


: MARK-ASCII ( str bool -- )
   {: text size:n little:bool :}
   size 0 ?do text i + c@ 1 little MARK-SCALAR loop ;


\ <a>, 64 turns of 😀éx中x, </a>: 711 lexical bytes. A turn is 11 bytes, so
\ each scalar of each width starts at every residue modulo any power of two up
\ to 64, and the multibyte ones straddle every such boundary.
: MARKED-BUILD ( bool -- str )
   {: little:bool :}
   0 MARKED-LEX ! 0 MARKED-WIRE !
   $FEFF little MARK-UNIT
   s" <a>" little MARK-ASCII
   64 0 ?do
      $1F600 4 little MARK-SCALAR
      $E9 2 little MARK-SCALAR
      $78 1 little MARK-SCALAR
      $4E2D 3 little MARK-SCALAR
      $78 1 little MARK-SCALAR
   loop
   s" </a>" little MARK-ASCII
   MARKED-WIRE @ MARKED-AT MARKED-LEX @ cells + !
   MARKED MARKED-WIRE @ ;

: PROBE-RANGE ( XML:source -- XML:source )
   PROBE @ >OFF 0 >LEN XML:SOURCE-RANGE 2drop ;


: MARKED-RANGES ( bool -- )
   {: little:bool :}
   little MARKED-BUILD {: original size:n :}
   original size OPEN
   XML:SOURCE-LEXICAL$ nip MARKED-LEX @ T=
   READER DRAIN XML:CLOSE
   0
   MARKED-LEX @ 1+ 0 ?do
      {: prev:n :}
      MARKED-AT i cells + @ {: want:n :}
      want -1 = if
         i PROBE ! ['] PROBE-RANGE catch XML:E-BOUNDARY T=
         prev
      else
         MARKED-AT prev cells + @ {: from:n :}
         i >OFF 0 >LEN XML:SOURCE-RANGE LEN>N 0 T= OFF>N want T=
         prev >OFF i prev - >LEN XML:SOURCE-RANGE
         LEN>N want from - T= OFF>N from T=
         i
      then
   loop
   drop
   XML:SOURCE-CLOSE ;


: WIDE-PUT ( ^u8 n str -- n )
   {: wire at:n text size:n :}
   size 0 ?do
      text i + c@ wire at i 2 * + + c!
      0 wire at i 2 * + 1+ + c!
   loop
   at size 2 * + ;


\ Mapping costs time bounded whatever the offset, or the row overruns
\ its CPU budget.
: WIDE-OWNER-RANGES ( -- )
   CHILDREN 4 * 7 + 2 * 2 + {: size:n :}
   size MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES {: wire wire-alloc :}
   $FF wire c! $FE wire 1+ c!
   wire 2 s" <r>" WIDE-PUT
   CHILDREN 0 ?do wire swap s" <a/>" WIDE-PUT loop
   wire swap s" </r>" WIDE-PUT size T=
   wire size OPEN XML:SOURCE-OWN {: owner :}
   0 WIDE-BAD !
   CHILDREN 0 ?do
      owner i 4 * 3 + >OFF 4 >LEN XML:OWNER-RANGE
      LEN>N 8 <> swap OFF>N i 4 * 3 + 2 * 2 + <> or if 1 WIDE-BAD +! then
   loop
   WIDE-BAD @ 0 T=
   owner XML:OWNER-CLOSE
   wire wire-alloc MEM:RELEASE-BYTES ;

: REJECTIONS ( -- )
   s\" \xFF\xFE<" XML:E-UTF16 BAD-INIT
   s\" \xFF\xFE\x00\xDC" XML:E-UTF16 BAD-INIT
   s\" \xFF\xFE\x00\xD8" XML:E-UTF16 BAD-INIT
   s\" \xFE\xFF\xDC\x00" XML:E-UTF16 BAD-INIT
   s\" \xFF\xFE\x01\z" XML:E-SCALAR BAD-INIT
   s\" \xFF\xFE\z\xD8A\z" XML:E-UTF16 BAD-INIT
   s\" \xFF\xFE\z\z\z\z" XML:E-ENCODING BAD-INIT
   s\" \z\z\xFE\xFF" XML:E-ENCODING BAD-INIT
   s\" \z\z\xFE\xFF<a/>" XML:E-ENCODING BAD-INIT
   XML:E-ENCODING s" 1:1: expected a UTF-8 or UTF-16 document, found byte $00; save the document as UTF-8 or UTF-16" SAID
   s\" <\za\z/\z>\z" XML:E-ENCODING BAD-INIT
   s\" <a>\xC0\x80</a>" XML:E-UTF8 BAD-INIT
   s\" <a>\xED\xA0\x80</a>" XML:E-UTF8 BAD-INIT
   s\" \xFF\xFE<\za\z>\z\x01\z<\z/\za\z>\z" XML:E-SCALAR BAD-INIT
   s\" <?xml version='1.0' encoding='UTF-16LE'?><a/>"
   true BOM-ASCII XML:E-ENCODING BAD-NEXT
   s\" \xEF\xBB\xBF\xEF\xBB\xBF<a/>" OPEN READER
   ['] DRAIN catch XML:E-MALFORMED T=
   XML:CLOSE XML:SOURCE-CLOSE ;


\ A source reader's rejection: its code and its text, read at the catch.
: SAID-NEXT ( str n str -- )
   {: want:n wa:^ wu:n :}
   OPEN READER
   ['] DRAIN catch want T=
   want REASON:TEXT$ wa wu T$=
   REASON:RELEASE
   XML:CLOSE XML:SOURCE-CLOSE ;

: DECLARATION-TEXTS ( -- )
   s" UTF-16 without a byte order mark names the encoding it is missing" T-LABEL
   s" <?xml version='1.0'?><a/>" true ASCII>UTF16 XML:E-ENCODING
   s\" 1:20: expected `encoding` after `version`, found `?`; declare `encoding=\qUTF-16LE\q` or `encoding=\qUTF-16BE\q`, as the document's bytes are"
   SAID-NEXT
   s" an attribute in the encoding's place is named" T-LABEL
   s" <?xml version='1.0' standalone='yes'?><a/>" true ASCII>UTF16 XML:E-ENCODING
   s\" 1:21: expected `encoding` after `version`, found `standalone`; declare `encoding=\qUTF-16LE\q` or `encoding=\qUTF-16BE\q`, as the document's bytes are"
   SAID-NEXT
   s" a byte order mark takes UTF-16" T-LABEL
   s" <?xml version='1.0' encoding='UTF-8'?><a/>" true BOM-ASCII XML:E-ENCODING
   s\" 1:32: expected `UTF-16`, as the byte order mark shows, found `UTF-8`; declare `encoding=\qUTF-16\q`"
   SAID-NEXT
   s" little-endian UTF-16 without a byte order mark takes UTF-16LE" T-LABEL
   s" <?xml version='1.0' encoding='UTF-16BE'?><a/>" true ASCII>UTF16 XML:E-ENCODING
   s\" 1:31: expected `UTF-16LE`, the encoding of the document's bytes, found `UTF-16BE`; declare `encoding=\qUTF-16LE\q`"
   SAID-NEXT
   s" big-endian UTF-16 without a byte order mark takes UTF-16BE" T-LABEL
   s" <?xml version='1.0' encoding='UTF-16LE'?><a/>" false ASCII>UTF16 XML:E-ENCODING
   s\" 1:31: expected `UTF-16BE`, the encoding of the document's bytes, found `UTF-16LE`; declare `encoding=\qUTF-16BE\q`"
   SAID-NEXT
   s" UTF-16 without a byte order mark begins with its declaration" T-LABEL
   s" <?xml-stylesheet x?><a/>" false ASCII>UTF16 XML:E-ENCODING
   s\" 1:1: expected an XML declaration, which a UTF-16 document without a byte order mark begins with, found `<`; begin the document with `<?xml version=\q1.0\q encoding=\qUTF-16LE\q?>` or its `UTF-16BE` form, or with a byte order mark"
   SAID-NEXT ;

: UNIT-TEXTS ( -- )
   s" a UTF-16 line ends at an LF unit, not at an LF byte of another unit" T-LABEL
   s\" \xFF\xFE<\za\z>\z\x0A\z\x0A\x01\z\xDC<\z/\za\z>\z" XML:E-UTF16 BAD-INIT
   XML:E-UTF16 s" 2:2: expected a character, or a high surrogate before a low one, found unit $DC00; write the high surrogate before the low one, or remove the low surrogate" SAID
   s" a lone last byte of UTF-16 is placed after the last unit" T-LABEL
   s\" \xFF\xFE<\za\z/\z>\zA" XML:E-UTF16 BAD-INIT
   XML:E-UTF16 s" 1:6: expected a UTF-16 unit of two bytes, found byte $41; write the last character whole, or remove the stray byte" SAID
   s" a big-endian high surrogate before another unit is placed at that unit" T-LABEL
   s\" \xFE\xFF\z<\za\z>\xD8\z\zA\z<\z/\za\z>" XML:E-UTF16 BAD-INIT
   XML:E-UTF16 s" 1:6: expected a low surrogate after the high surrogate, found unit $0041; write the low surrogate after the high one, or remove the high surrogate" SAID
   s" a UTF-16 character XML 1.0 does not allow is found as its unit" T-LABEL
   s\" \xFF\xFE<\za\z>\z\xFE\xFF<\z/\za\z>\z" XML:E-SCALAR BAD-INIT
   XML:E-SCALAR s" 1:5: expected a character XML 1.0 allows, found unit $FFFE; remove the character, or write one XML 1.0 allows" SAID
   s" a byte no UTF-8 character starts is placed by lines" T-LABEL
   s\" <a>\x0A<b/>\x0A\xFF</a>" XML:E-UTF8 BAD-INIT
   XML:E-UTF8 s" 3:1: expected a UTF-8 character, found byte $FF; write the text in UTF-8" SAID ;

\ Each source-stage case and the reader case after it reject documents that
\ differ only at the rejected place, and give it one line and column.
: PLACE-TEXTS ( -- )
   s" a source stage counts a UTF-8 byte order mark on line 1" T-LABEL
   s\" \xEF\xBB\xBF<a><b>xxxxxxxx</b><c>yyyyyyyy</c>\x01</a>" XML:E-SCALAR BAD-INIT
   XML:E-SCALAR s" 1:37: expected a character XML 1.0 allows, found byte $01; remove the character, or write one XML 1.0 allows" SAID
   s" a source reader counts a UTF-8 byte order mark on line 1" T-LABEL
   s\" \xEF\xBB\xBF<a><b>xxxxxxxx</b><c>yyyyyyyy</c>&foo;</a>" XML:E-ENTITY
   s" 1:37: expected `&amp;`, `&lt;`, `&gt;`, `&quot;`, `&apos;` or a character reference such as `&#65;`, found `&foo;`; write `&amp;` for a literal `&`, and end each reference with `;`"
   SAID-NEXT
   s" a source stage counts UTF-16LE in units" T-LABEL
   s\" \xFF\xFE<\za\z>\z\n\z<\zb\z>\z\xE9\z\x3D\xD8\z\xDE\x01\z<\z/\zb\z>\z<\z/\za\z>\z" XML:E-SCALAR BAD-INIT
   XML:E-SCALAR s" 2:7: expected a character XML 1.0 allows, found unit $0001; remove the character, or write one XML 1.0 allows" SAID
   s" a source reader counts UTF-16LE in units" T-LABEL
   s\" \xFF\xFE<\za\z>\z\n\z<\zb\z>\z\xE9\z\x3D\xD8\z\xDE&\zf\zo\zo\z;\z<\z/\zb\z>\z<\z/\za\z>\z" XML:E-ENTITY
   s" 2:7: expected `&amp;`, `&lt;`, `&gt;`, `&quot;`, `&apos;` or a character reference such as `&#65;`, found `&foo;`; write `&amp;` for a literal `&`, and end each reference with `;`"
   SAID-NEXT
   s" a source stage counts UTF-16BE in units" T-LABEL
   s\" \xFE\xFF\z<\za\z>\z\n\z<\zb\z>\z\xE9\xD8\x3D\xDE\z\z\x01\z<\z/\zb\z>\z<\z/\za\z>" XML:E-SCALAR BAD-INIT
   XML:E-SCALAR s" 2:7: expected a character XML 1.0 allows, found unit $0001; remove the character, or write one XML 1.0 allows" SAID
   s" a source reader counts UTF-16BE in units" T-LABEL
   s\" \xFE\xFF\z<\za\z>\z\n\z<\zb\z>\z\xE9\xD8\x3D\xDE\z\z&\zf\zo\zo\z;\z<\z/\zb\z>\z<\z/\za\z>" XML:E-ENTITY
   s" 2:7: expected `&amp;`, `&lt;`, `&gt;`, `&quot;`, `&apos;` or a character reference such as `&#65;`, found `&foo;`; write `&amp;` for a literal `&`, and end each reference with `;`"
   SAID-NEXT ;

DECODED-NEW
T-RESET
UTF16-READ
UTF8-READ
ASCII-READ
ASCII-OUTPUT
ASCII-REJECTIONS
DECLARED-BOM
UNICODE-UTF16
INTERLEAVED
BOMLESS-READ
BOMLESS-REJECTIONS
true BOMLESS-EDIT
false BOMLESS-EDIT
true COMPOSED-EDIT
false COMPOSED-EDIT
EDIT-LE
BOUNDARIES-AND-RECOVERY
UTF8-RANGES
true MARKED-RANGES
false MARKED-RANGES
WIDE-OWNER-RANGES
APPENDS-KEEP
ESCAPE-OWN
COUNTED-STORAGE
REJECTIONS
DECLARATION-TEXTS
UNIT-TEXTS
PLACE-TEXTS
DECODED @ VEC:DELETE
T-REPORT
;package

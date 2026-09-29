require lib/test.f
require lib/xml.f
require lib/byte-edit.f

package XML-SOURCE-TEST

16 constant READER-CAP
256 constant SOURCE-CAP
create SOURCE-STORE SOURCE-CAP allot
create SECOND-STORE SOURCE-CAP allot
create READER-STORE READER-CAP XML:STORAGE-BYTES allot
create EDIT-STORE 4 EDIT:STORAGE-BYTES allot
create ENCODED 64 allot
create ATTR-OUT 64 allot
create INSERT-OUT 64 allot
create WRITTEN 256 allot
create DECODED 64 allot
create BOMLESS 256 allot
create BOM-INPUT 256 allot
create WIRE 64 allot

CAST: KIND>N ( XML:kind -- n )
CAST: ENCODING>N ( XML:encoding -- n )

: LE$ ( -- ptr u8 n )
   s\" \xFF\xFE<\za\z \zq\z=\z'\zx\z'\z>\z&\za\zm\zp\z;\z<\z/\za\z>\z" ;

: BE$ ( -- ptr u8 n )
   s\" \xFE\xFF\z<\za\z>\zx\z<\z/\za\z>" ;

: ASCII>UTF16 ( ptr u8 n bool -- ptr u8 n )
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

: BOM-ASCII ( ptr u8 n bool -- ptr u8 n )
   {: ascii size:n little:bool :}
   ascii size little ASCII>UTF16 {: encoded count:n :}
   little if
      $FF BOM-INPUT c! $FE BOM-INPUT 1+ c!
   else
      $FE BOM-INPUT c! $FF BOM-INPUT 1+ c!
   then
   encoded BOM-INPUT 2 + count BYTE-COPY
   BOM-INPUT count 2 + ;

: OPEN ( ptr u8 n -- XML:source )
   SOURCE-STORE SOURCE-CAP 2swap XML:SOURCE-INIT ;

: READER ( XML:source -- XML:source XML:reader )
   READER-STORE READER-CAP XML:STORAGE-BYTES XML:INIT-SOURCE ;

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
   DECODED 64 XML:TEXT DECODED swap s" &" T$=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT=
   XML:CLOSE XML:SOURCE-CLOSE
   BE$ OPEN
   XML:SOURCE-ENCODING ENCODING>N XML-ENCODING:UTF16-BE ENCODING>N T=
   XML:SOURCE-LEXICAL$ s" <a>x</a>" T$=
   READER XML-KIND:START NEXT= XML-KIND:TEXT NEXT=
   DECODED 64 XML:TEXT DECODED swap s" x" T$=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT=
   XML:CLOSE XML:SOURCE-CLOSE ;

: UTF8-READ ( -- )
   s\" \xEF\xBB\xBF<a>é😀</a>" OPEN
   XML:SOURCE-ENCODING ENCODING>N XML-ENCODING:UTF8 ENCODING>N T=
   XML:SOURCE-BOM$ s\" \xEF\xBB\xBF" T$=
   XML:SOURCE-LEXICAL$ s" <a>é😀</a>" T$=
   READER XML-KIND:START NEXT= XML-KIND:TEXT NEXT=
   DECODED 64 XML:TEXT DECODED swap s" é😀" T$=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT=
   XML:CLOSE XML:SOURCE-CLOSE ;

: DECLARED-BOM ( -- )
   s\" <?xml version=\q1.0\q encoding='UTF-16'?><p:a xmlns:p=\qu\q q='x&amp;y'>a\r\n<![CDATA[<b>]]><?go v?></p:a><!--keep-->"
   false BOM-ASCII OPEN
   XML:SOURCE-LEXICAL$
   s\" <?xml version=\q1.0\q encoding='UTF-16'?><p:a xmlns:p=\qu\q q='x&amp;y'>a\r\n<![CDATA[<b>]]><?go v?></p:a><!--keep-->"
   T$=
   READER
   XML-KIND:PI NEXT=
   XML-KIND:START NEXT=
   DECODED 64 XML:URI DECODED swap s\" u" T$=
   XML:ATTR-NEXT TTRUE XML:ATTR-NEXT TTRUE
   XML:ATTR-VALUE$ s\" x&amp;y" T$=
   DECODED 64 XML:ATTR-TEXT DECODED swap s\" x&y" T$=
   XML-KIND:TEXT NEXT=
   DECODED 64 XML:TEXT DECODED swap s\" a\n" T$=
   XML-KIND:CDATA NEXT=
   DECODED 64 XML:TEXT DECODED swap s\" <b>" T$=
   XML-KIND:PI NEXT= XML-KIND:END NEXT=
   XML-KIND:COMMENT NEXT=
   DECODED 64 XML:TEXT DECODED swap s\" keep" T$=
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
   DECODED 64 XML:TEXT DECODED swap s\" 😀" T$=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT=
   XML:CLOSE XML:SOURCE-CLOSE
   s\" \xFE\xFF\z<\z\xE9\z>\xD8\x3D\xDE\z\z<\z/\z\xE9\z>" OPEN
   READER XML-KIND:START NEXT=
   XML:NAME$ s\" é" T$=
   XML-KIND:TEXT NEXT=
   DECODED 64 XML:TEXT DECODED swap s\" 😀" T$=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT=
   XML:CLOSE XML:SOURCE-CLOSE ;

: INTERLEAVED ( -- )
   LE$ OPEN
   SECOND-STORE SOURCE-CAP BE$ XML:SOURCE-INIT
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
   DECODED 64 XML:TEXT DECODED swap s\" x" T$=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT=
   XML:CLOSE XML:SOURCE-CLOSE
   s\" <?xml version='1.0' encoding='utf-16be'?><a>x</a>"
   false ASCII>UTF16 OPEN
   XML:SOURCE-BOM$ nip 0 T=
   READER
   XML-KIND:PI NEXT= XML-KIND:START NEXT= XML-KIND:TEXT NEXT=
   DECODED 64 XML:TEXT DECODED swap s\" x" T$=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT=
   XML:CLOSE XML:SOURCE-CLOSE ;

: BAD-NEXT ( ptr u8 n n -- )
   {: want:n :}
   OPEN READER
   ['] DRAIN catch want T=
   ['] DRAIN catch XML:E-STATE T=
   XML:CLOSE XML:SOURCE-CLOSE ;

: BAD-INIT ( ptr u8 n n -- )
   {: want:n :}
   [: 2dup SOURCE-STORE SOURCE-CAP 2swap XML:SOURCE-INIT XML:SOURCE-CLOSE ;]
   catch want T= 2drop ;

: BOMLESS-REJECTIONS ( -- )
   s\" <a/>" true ASCII>UTF16 XML:E-ENCODING BAD-INIT
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
   s\" A🚗" ENCODED 64 XML:ENCODE {: added:n :}
   added 6 T=
   little if
      ENCODED added s\" A\z\x3D\xD8\x97\xDE" T$=
   else
      ENCODED added s\" \zA\xD8\x3D\xDE\x97" T$=
   then
   XML:SOURCE-ORIGINAL$ {: original original-len:n :}
   EDIT-STORE 2 EDIT:STORAGE-BYTES original original-len EDIT:INIT
   edit-off edit-len ENCODED added EDIT:REPLACE
   WRITTEN 256 EDIT:WRITE {: written:n :}
   EDIT:CLOSE XML:SOURCE-CLOSE
   edit-off OFF>N {: start:n :}
   edit-len LEN>N {: removed:n :}
   WRITTEN BYTE-VIEW start original start T$=
   WRITTEN BYTE-VIEW start added + + written start added + -
   original start removed + + original-len start removed + -
   T$=
   SECOND-STORE SOURCE-CAP WRITTEN written XML:SOURCE-INIT
   XML:SOURCE-BOM$ nip 0 T=
   READER XML-KIND:PI NEXT= XML-KIND:START NEXT= XML-KIND:TEXT NEXT=
   DECODED 64 XML:TEXT DECODED swap s\" A🚗|KEEP" T$=
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
   s\" A🚗" ATTR-OUT 64 XML:ENCODE {: attr-added:n :}
   little if
      ATTR-OUT attr-added s\" A\z\x3D\xD8\x97\xDE" T$=
   else
      ATTR-OUT attr-added s\" \zA\xD8\x3D\xDE\x97" T$=
   then
   s\" pre-" INSERT-OUT 64 XML:ENCODE {: insert-added:n :}
   s\" " ENCODED 0 XML:ENCODE 0 T=
   XML:SOURCE-ORIGINAL$ {: original original-len:n :}
   EDIT-STORE 4 EDIT:STORAGE-BYTES original original-len EDIT:INIT
   attr-wire attr-old ATTR-OUT attr-added EDIT:REPLACE
   insert-wire insert-old INSERT-OUT insert-added EDIT:REPLACE
   delete-wire delete-old ENCODED 0 EDIT:REPLACE
   eof-wire eof-old ENCODED 0 EDIT:REPLACE
   WRITTEN 256 EDIT:WRITE {: written:n :}
   EDIT:CLOSE XML:SOURCE-CLOSE
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
   SECOND-STORE SOURCE-CAP WRITTEN written XML:SOURCE-INIT
   XML:SOURCE-BOM$ nip 2 T=
   READER XML-KIND:PI NEXT= XML-KIND:START NEXT=
   XML:ATTR-NEXT TTRUE
   DECODED 64 XML:ATTR-TEXT DECODED swap s\" A🚗" T$=
   XML-KIND:TEXT NEXT=
   DECODED 64 XML:TEXT DECODED swap s\" pre-KEEP" T$=
   XML-KIND:CDATA NEXT=
   DECODED 64 XML:TEXT 0 T=
   XML-KIND:TEXT NEXT=
   DECODED 64 XML:TEXT DECODED swap s\" tail" T$=
   XML-KIND:END NEXT= XML-KIND:COMMENT NEXT=
   DECODED 64 XML:TEXT DECODED swap s\" keep" T$=
   XML-KIND:EOF NEXT= XML:CLOSE XML:SOURCE-CLOSE ;

: WRITE-TINY ( EDIT:editor -- EDIT:editor )
   WRITTEN 1 EDIT:WRITE drop ;

: EDIT-LE ( -- )
   s\" \xFF\xFE<\za\z>\zx\z<\z/\za\z>\z" OPEN
   READER XML-KIND:START NEXT= XML-KIND:TEXT NEXT=
   XML:CONTENT {: text-off:off text-len:len :}
   XML:CLOSE
   text-off text-len XML:SOURCE-RANGE {: edit-off:off edit-len:len :}
   s" é" ENCODED 64 XML:ENCODE {: added:n :}
   ENCODED added s\" \xE9\z" T$=
   XML:SOURCE-ORIGINAL$ {: original original-len:n :}
   EDIT-STORE 2 EDIT:STORAGE-BYTES original original-len EDIT:INIT
   edit-off edit-len ENCODED added EDIT:REPLACE
   $5A WRITTEN c!
   ['] WRITE-TINY catch EDIT:E-CAPACITY T=
   WRITTEN c@ $5A T=
   WRITTEN 256 EDIT:WRITE {: written:n :}
   EDIT:CLOSE XML:SOURCE-CLOSE
   WRITTEN written s\" \xFF\xFE<\za\z>\z\xE9\z<\z/\za\z>\z" T$=
   SECOND-STORE SOURCE-CAP WRITTEN written XML:SOURCE-INIT
   READER XML-KIND:START NEXT= XML-KIND:TEXT NEXT=
   DECODED 64 XML:TEXT DECODED swap s" é" T$=
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

: TINY-ENCODE ( XML:source -- XML:source )
   s\" A" ENCODED 1 XML:ENCODE drop ;

: ALIAS-ENCODE ( XML:source -- XML:source )
   XML:SOURCE-ORIGINAL$ {: original size:n :}
   s\" A" original size XML:ENCODE drop ;

: BAD-READER-CAP ( XML:source -- XML:source )
   READER-STORE 0 XML:INIT-SOURCE XML:CLOSE ;

: ALIAS-READER ( XML:source -- XML:source )
   SOURCE-STORE READER-CAP XML:STORAGE-BYTES XML:INIT-SOURCE
   XML:CLOSE ;

: TEXT-WIRE ( XML:reader -- XML:reader )
   WIRE BYTE-VIEW 1 XML:TEXT drop ;

: TEXT-SOURCE-HEADER ( XML:reader -- XML:reader )
   SOURCE-STORE CELL + BYTE-VIEW 1 XML:TEXT drop ;

: SOURCE-OUTPUT-ALIAS ( -- )
   s\" \xFF\xFE<\za\z>\zx\z<\z/\za\z>\z" {: original size:n :}
   original WIRE size BYTE-COPY
   WIRE BYTE-VIEW size OPEN
   READER XML-KIND:START NEXT= XML-KIND:TEXT NEXT=
   ['] TEXT-WIRE catch XML:E-ALIAS T=
   WIRE c@ $FF T=
   ['] TEXT-SOURCE-HEADER catch XML:E-ALIAS T=
   XML:CLOSE
   XML:SOURCE-ORIGINAL$ nip size T=
   XML:SOURCE-CLOSE ;

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
   $5A ENCODED c!
   ['] TINY-ENCODE catch XML:E-CAPACITY T=
   ENCODED c@ $5A T=
   ['] ALIAS-ENCODE catch XML:E-ALIAS T=
   ['] BAD-READER-CAP catch XML:E-CAPACITY T=
   ['] ALIAS-READER catch XML:E-ALIAS T=
   s\" A🚗" ENCODED 64 XML:ENCODE
   {: added:n :}
   added 6 T=
   READER XML-KIND:START NEXT= XML-KIND:TEXT NEXT=
   DECODED 64 XML:TEXT DECODED swap s\" 😀" T$=
   XML-KIND:END NEXT= XML-KIND:EOF NEXT=
   XML:CLOSE XML:SOURCE-CLOSE ;

: OPEN-SMALL ( ptr u8 n -- )
   2dup XML:SOURCE-BYTES 1- {: cap:n :}
   SOURCE-STORE cap 2swap XML:SOURCE-INIT XML:SOURCE-CLOSE ;

: SMALL-SOURCE ( ptr u8 n -- )
   [: 2dup OPEN-SMALL ;] catch XML:E-CAPACITY T= 2drop ;

: SOURCE-CAPACITY ( -- )
   s\" \xFF\xFE<\za\z/\z>\z" SMALL-SOURCE
   s\" \xFF\xFE<\za\z/\z>\z" OPEN
   READER XML-KIND:START NEXT= XML-KIND:END NEXT=
   XML-KIND:EOF NEXT= XML:CLOSE XML:SOURCE-CLOSE ;

: REJECTIONS ( -- )
   s\" \xFF\xFE<" XML:E-UTF16 BAD-INIT
   s\" \xFF\xFE\x00\xDC" XML:E-UTF16 BAD-INIT
   s\" \xFF\xFE\x00\xD8" XML:E-UTF16 BAD-INIT
   s\" \xFE\xFF\xDC\x00" XML:E-UTF16 BAD-INIT
   s\" \xFF\xFE\x01\z" XML:E-SCALAR BAD-INIT
   s\" \xFF\xFE\z\xD8A\z" XML:E-UTF16 BAD-INIT
   s\" \xFF\xFE\z\z\z\z" XML:E-ENCODING BAD-INIT
   s\" \z\z\xFE\xFF" XML:E-ENCODING BAD-INIT
   s\" <\za\z/\z>\z" XML:E-ENCODING BAD-INIT
   s\" <a>\xC0\x80</a>" XML:E-UTF8 BAD-INIT
   s\" <a>\xED\xA0\x80</a>" XML:E-UTF8 BAD-INIT
   s\" \xFF\xFE<\za\z>\z\x01\z<\z/\za\z>\z" XML:E-SCALAR BAD-INIT
   s\" <?xml version='1.0' encoding='UTF-16LE'?><a/>"
   true BOM-ASCII XML:E-ENCODING BAD-NEXT
   s\" \xEF\xBB\xBF\xEF\xBB\xBF<a/>" OPEN READER
   ['] DRAIN catch XML:E-MALFORMED T=
   XML:CLOSE XML:SOURCE-CLOSE ;

T-RESET
UTF16-READ
UTF8-READ
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
SOURCE-CAPACITY
SOURCE-OUTPUT-ALIAS
REJECTIONS
T-REPORT
;package

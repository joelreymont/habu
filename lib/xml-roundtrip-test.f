require lib/test.f
require lib/xml.f
require lib/byte-edit.f

package XML-ROUNDTRIP-TEST
using XML
8 constant XML-CAP
$1000 constant BYTE-CAP
here CELL 1- and CELL swap - CELL 1- and allot
create PARSER XML-CAP STORAGE-BYTES allot
create EDITOR 2 EDIT:STORAGE-BYTES allot
create ATTRIBUTE-BUF BYTE-CAP allot
create TEXT-BUF BYTE-CAP allot
create DOCUMENT BYTE-CAP allot
create READBACK BYTE-CAP allot
create SINGLE 1 allot

: ORIGINAL$ ( -- ptr u8 n )
   s" <?xml version='1.0'?><p:a xmlns:p='urn:p' q='old'>old</p:a><!--keep-->" ;

: OPEN ( ptr u8 n -- XML:reader )
   PARSER XML-CAP STORAGE-BYTES 2swap INIT ;

: ATTRIBUTE-RANGE ( -- off len )
   ORIGINAL$ OPEN NEXT drop NEXT drop
   ATTR-NEXT TTRUE ATTR-NEXT TTRUE
   ATTR-VALUE {: start:off size:len :}
   XML:CLOSE start size ;

: TEXT-RANGE ( -- off len )
   ORIGINAL$ OPEN NEXT drop NEXT drop NEXT drop
   CONTENT {: start:off size:len :}
   XML:CLOSE start size ;

: APPLY ( ptr u8 n -- n )
   {: value size:n :}
   value size ATTRIBUTE-BUF BYTE-CAP ESCAPE-ATTR {: attr-size:n :}
   value size TEXT-BUF BYTE-CAP ESCAPE-TEXT {: text-size:n :}
   ATTRIBUTE-RANGE {: attr-start:off attr-old:len :}
   TEXT-RANGE {: text-start:off text-old:len :}
   EDITOR 2 EDIT:STORAGE-BYTES ORIGINAL$ EDIT:INIT
   attr-start attr-old ATTRIBUTE-BUF attr-size EDIT:REPLACE
   text-start text-old TEXT-BUF text-size EDIT:REPLACE
   DOCUMENT BYTE-CAP EDIT:WRITE {: document-size:n :}
   EDIT:CLOSE document-size ;

: VERIFY ( ptr u8 n n -- )
   {: value size:n document-size:n :}
   DOCUMENT document-size OPEN
   NEXT drop NEXT drop
   ATTR-NEXT TTRUE ATTR-NEXT TTRUE
   READBACK BYTE-CAP ATTR-TEXT READBACK swap value size T$=
   NEXT drop
   READBACK BYTE-CAP TEXT READBACK swap value size T$=
   NEXT drop NEXT drop
   READBACK BYTE-CAP TEXT READBACK swap s" keep" T$=
   NEXT drop XML:CLOSE ;

: ROUNDTRIP ( ptr u8 n -- )
   {: value size:n :}
   value size value size APPLY VERIFY ;

: ASCII-ROUNDTRIPS ( -- )
   $7F $20 ?do
      i SINGLE c!
      SINGLE 1 ROUNDTRIP
   loop ;

T-RESET
ASCII-ROUNDTRIPS
s\" \t\n\r" ROUNDTRIP
s" <&>'é名😀🚀" ROUNDTRIP
T-REPORT
;using
;package

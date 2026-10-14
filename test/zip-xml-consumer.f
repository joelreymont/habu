\ A public ZIP input supplies an XML reader through a caller-owned context cell.
require src/lib/zip.f
require src/lib/xml.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f

package ZIP-XML-CONSUMER
private

136 constant ZIP-SIZE
38 constant XML-OFFSET
16 constant XML-CAP
create ROOT FS-PATH-CAP allot
variable ROOT-LEN
create PATH FS-PATH-CAP allot
create IMAGE ZIP-SIZE allot
create CONTEXT 0 ,
create CHUNK 7 allot
create READER XML-CAP XML:STORAGE-BYTES allot
create TEXT-BUF 32 allot

CAST: KIND>N ( XML:kind -- n )

: ZIP$ ( -- ptr u8 n )
   S\" PK\x03\x04\x14\x00\x00\x00\x00\x00\x00\x00!\x00\x22+\xAD\xD2\x16\x00\x00\x00\x16\x00\x00\x00\x08\x00\x00\x00part.xml<r>hello&amp;world</r>PK\x01\x02\x14\x03\x14\x00\x00\x00\x00\x00\x00\x00!\x00\x22+\xAD\xD2\x16\x00\x00\x00\x16\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x00\x00\x00part.xmlPK\x05\x06\x00\x00\x00\x00\x01\x00\x01\x006\x00\x00\x00<\x00\x00\x00\x00\x00" ;

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-LEN @ ;
: FILE$ ( ptr u8 n -- ptr u8 n )
   ROOT$ 2swap PATH JOIN-PATH PATH swap ;

: FILL ( ptr n SPAN:span<u8> -- n )
   {: ctx:ptr dst :}
   ctx ZIP:INPUT-SLOT @ dst ZIP:READ-IN ;

: OPEN-XML ( ZIP:input -- XML:reader )
   CONTEXT ZIP:INPUT-SLOT !
   CONTEXT ['] FILL CHUNK 7 SPAN:MAKE
   READER XML-CAP XML:STORAGE-BYTES XML:INIT-INPUT ;

: NEXT= ( XML:reader XML:kind -- XML:reader )
   KIND>N {: expected:n :}
   XML:NEXT KIND>N expected T= ;

: READ-PART ( -- )
   s" sample.zip" FILE$ ZIP:OPEN-FILE {: archive:ZIP:archive :}
   archive archive 0 ZIP:ENTRY ZIP:OPEN-ENTRY {: input:ZIP:input :}
   input OPEN-XML
   XML-KIND:START NEXT=
   XML:NAME$ s" r" T$=
   0 begin
      >r XML:NEXT KIND>N XML-KIND:TEXT KIND>N =
   while
      r> {: used:n :}
      TEXT-BUF used + 32 used - XML:TEXT used +
   repeat
   r> {: used:n :}
   TEXT-BUF used s" hello&world" T$=
   XML:KIND KIND>N XML-KIND:END KIND>N T=
   XML-KIND:EOF NEXT=
   XML:CLOSE
   input ZIP:CLOSE-ENTRY
   archive ZIP:CLOSE ;

: DRAIN ( XML:reader -- XML:reader )
   begin XML:NEXT KIND>N XML-KIND:EOF KIND>N = until ;

: STALE-READ ( ZIP:input SPAN:span<u8> -- ZIP:input SPAN:span<u8> )
   2dup ZIP:READ-IN drop ;

: STALE-CLOSE ( ZIP:input -- ZIP:input )
   dup ZIP:CLOSE-ENTRY ;

: READ-CORRUPT ( -- )
   s" corrupt.zip" FILE$ ZIP:OPEN-FILE {: archive:ZIP:archive :}
   archive archive 0 ZIP:ENTRY ZIP:OPEN-ENTRY {: input:ZIP:input :}
   input OPEN-XML ['] DRAIN catch swap XML:CLOSE
   ZIP:E-READ T=
   input CHUNK 7 SPAN:MAKE ['] STALE-READ catch
   >r 2drop r> ZIP:E-HANDLE T=
   input ['] STALE-CLOSE catch ZIP:E-HANDLE T= drop
   archive ZIP:CLOSE ;

: SETUP ( -- )
   s" habu-zip-xml" HB-TMP-MKDIR {: root:ptr len:n :}
   root ROOT len BYTE-COPY len ROOT-LEN !
   s" sample.zip" FILE$ ZIP$ WRITE-ALL
   ZIP$ IMAGE swap BYTE-COPY
   $48 IMAGE XML-OFFSET 3 + + c!
   s" corrupt.zip" FILE$ IMAGE ZIP-SIZE WRITE-ALL ;

public
: RUN ( -- )
   T-RESET SETUP
   s" public ZIP callback streams XML and closes both owners" T-LABEL
   READ-PART
   s" callback read failure retires its ZIP handle" T-LABEL
   READ-CORRUPT
   T-REPORT
   ROOT$ type cr ;
;package

: MAIN ( -- ) ZIP-XML-CONSUMER:RUN ;

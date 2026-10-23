\ aot-shadow-key.f - what test/aot-shadow-capture.f's writer and its reader
\ child, test/aot-shadow-reader.f, agree on: the producer key and the chain
\ sources the header digests.
require src/habu/aot-ident.f

package AOTSH-KEY
public

32 constant KEY-BYTES
create KEY KEY-BYTES allot           \ the producer key: any 32 bytes READ is handed back

: IDENT! ( -- )
   KEY-BYTES 0 ?do 0 KEY i + c! loop
   AOT-IDENT:RESET
   s" src/habu/aot-decl.f" AOT-IDENT:PATH+
   s" src/habu/aot-shadow.f" AOT-IDENT:PATH+ ;

;package

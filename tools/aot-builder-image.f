\ A keyed saved image of the fixed AOT builder support, selected by a producer
\ key the caller has validated.
require src/lib/errors.f
require src/lib/fs.f
require src/lib/process-argv.f
require src/lib/content-key.f
require tools/build-support.f
require tools/keyed-image.f

package AOT-BUILDER-IMAGE

create KEY-HEX KEYED-IMAGE:KEY-HEX-LEN allot
create PATH-BUF FS-PATH-CAP allot
create SELECTED-BUF FS-PATH-CAP allot
variable PATH-U
variable SELECTED-U

: FAMILY$ ( -- ptr u8 n ) s" aot-builder" ;
: PATH-BYTES ( -- ptr u8 n ) PATH-BUF PATH-U @ ;

: PROGRAM$ ( -- ptr u8 n )
   S\" require tools/aot-builder.f\n0 SCRIPT-ARGV$ AOT-BUILDER:SAVE\n" ;

: COPY-ENTRY! ( ptr u8 n ptr u8 ptr n -- )
   {: a:ptr u:n dst:ptr up:ptr :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a dst u BYTE-COPY
   u up ! ;

: OUT-ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

public

\ The caller has validated this KEY-HEX-LEN-byte producer key against the
\ selected engine, target and resolved support closure. Preserve the image's
\ own recipe and source-root identity without discovering that closure again.
: SELECT-PRODUCER$ ( ptr u8 n -- ptr u8 n bool )
   {: key:ptr timeout:n :}
   BUILD-FIXPOINT:BF-ENGINE$ SOURCE-ROOT:ENGINE-FOR$
      SELECTED-BUF SELECTED-U COPY-ENTRY!
   CONTENT-KEY:OPEN
   s" aot-builder-producer-v1" CONTENT-KEY:TEXT+
   key KEYED-IMAGE:KEY-HEX-LEN CONTENT-KEY:TEXT+
   PROGRAM$ CONTENT-KEY:TEXT+
   SELECTED-BUF SELECTED-U @ CONTENT-KEY:TEXT+
   KEY-HEX CONTENT-KEY:FINAL-HEX
   KEY-HEX FAMILY$ PATH-BUF PATH-U KEYED-IMAGE:PATH!
   FAMILY$ PATH-BYTES BUILD-FIXPOINT:BF-ENGINE$ PROGRAM$ ['] OUT-ARG timeout
   KEYED-IMAGE:ENSURE-WITH {: hit:bool :}
   PATH-BYTES hit ;

;package

\ A keyed saved image of the fixed AOT builder support. The selected engine,
\ source root and support closure are resolved before the caller stages argv.
require src/lib/errors.f
require src/lib/fs.f
require src/lib/process-argv.f
require src/lib/content-key.f
require tools/build-support.f
require tools/keyed-image.f

package AOT-BUILDER-IMAGE

create KEY-HEX KEYED-IMAGE:KEY-HEX-LEN allot
create PATH-BUF FS-PATH-CAP allot
create ENTRY-BUF FS-PATH-CAP allot
create ROOT-BUF FS-PATH-CAP allot
create SELECTED-BUF FS-PATH-CAP allot
create RUNNING-BUF FS-PATH-CAP allot
variable PATH-U
variable ENTRY-U
variable ROOT-U
variable SELECTED-U
variable RUNNING-U

: FAMILY$ ( -- ptr u8 n ) s" aot-builder" ;
: PATH-BYTES ( -- ptr u8 n ) PATH-BUF PATH-U @ ;

: PROGRAM$ ( -- ptr u8 n )
   S\" require tools/aot-builder.f\n0 SCRIPT-ARGV$ AOT-BUILDER:SAVE\n" ;

: COPY-ENTRY! ( ptr u8 n ptr u8 ptr n -- )
   {: a:ptr u:n dst:ptr up:ptr :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a dst u BYTE-COPY
   u up ! ;

: FILL-KEY ( CONTENT-KEY:fold -- CONTENT-KEY:fold )
   s" aot-builder-image-v1" CONTENT-KEY:TEXT+
   BUILD-FIXPOINT:BF-ENGINE$ s" host-engine" CONTENT-KEY:FILE-NAMED+
   SELECTED-BUF SELECTED-U @ CONTENT-KEY:TEXT+
   PROGRAM$ CONTENT-KEY:TEXT+
   ENTRY-BUF ENTRY-U @ ROOT-BUF ROOT-U @ KEYED-IMAGE:CLOSURE-IN+ ;

: RESOLVE-CONTENT ( -- )
   s" tools/aot-builder.f" SOURCE-ROOT:RESOLVE drop
   ENTRY-BUF ENTRY-U COPY-ENTRY!
   SOURCE-ROOT:RESOLVED-ROOT$ ROOT-BUF ROOT-U COPY-ENTRY!
   CONTENT-KEY:OPEN {: fold:CONTENT-KEY:fold :}
   fold [: FILL-KEY ;] catch {: rc:n :} drop
   rc 0<> if fold CONTENT-KEY:DISCARD rc throw then
   fold KEY-HEX CONTENT-KEY:FINAL-HEX
   KEY-HEX FAMILY$ PATH-BUF PATH-U KEYED-IMAGE:PATH! ;

: RESOLVE ( -- )
   SOURCE-ROOT:ENGINE$ RUNNING-BUF RUNNING-U COPY-ENTRY!
   BUILD-FIXPOINT:BF-ENGINE$ SOURCE-ROOT:ENGINE-FOR$
   SELECTED-BUF SELECTED-U COPY-ENTRY!
   SELECTED-BUF SELECTED-U @ SOURCE-ROOT:ENGINE!
   [: ['] RESOLVE-CONTENT SOURCE-ROOT:WITH-CWD ;] catch {: rc:n :}
   RUNNING-BUF RUNNING-U @ SOURCE-ROOT:ENGINE!
   rc 0<> if rc throw then ;

: OUT-ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: ENSURE ( n -- ptr u8 n bool )
   {: timeout:n :}
   FAMILY$ PATH-BYTES BUILD-FIXPOINT:BF-ENGINE$ PROGRAM$ ['] OUT-ARG timeout
   KEYED-IMAGE:ENSURE-WITH {: hit:bool :}
   PATH-BYTES hit ;

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
   timeout ENSURE ;

: SELECT$ ( n -- ptr u8 n bool )
   RESOLVE ENSURE ;

: PATH$ ( n -- ptr u8 n )
   SELECT$ drop ;

;package

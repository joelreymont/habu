\ Cache the explicit FILE driver as a saved image. The key includes the chosen
\ compiler and the ordered support closure, including source paths: a changed
\ driver or a different source tree must build a different image.

require src/lib/errors.f
require src/lib/fs.f
require src/lib/engine-id.f
require src/lib/process-argv.f
require src/lib/content-key.f
require tools/keyed-image.f

package NATIVE-FILE-DRIVER-IMAGE

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

: FAMILY$ ( -- ptr u8 n ) s" native-file-driver" ;
: PATH-BYTES ( -- ptr u8 n ) PATH-BUF PATH-U @ ;

\ The saver runs from outer stdin after the driver require returns. The driver
\ records its live source-producer digest before capture resets that history.
: PROGRAM$ ( -- ptr u8 n )
   S\" require tools/native-file-explicit.f\nNATIVE-FILE-EXPLICIT:CAPTURE-PRODUCER\nNATIVE-FILE-EXPLICIT:SAVE-ROOT\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" ;

: COPY-ENTRY! ( ptr u8 n ptr u8 ptr n -- )
   {: a:ptr u:n dst:ptr up:ptr :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a dst u BYTE-COPY
   u up ! ;

: FILL-KEY ( CONTENT-KEY:fold -- CONTENT-KEY:fold )
   s" native-file-driver-image-v1" CONTENT-KEY:TEXT+
   ENGINE-ID:PATH$ s" host-engine" CONTENT-KEY:FILE-NAMED+
   SELECTED-BUF SELECTED-U @ CONTENT-KEY:TEXT+
   PROGRAM$ CONTENT-KEY:TEXT+
   ENTRY-BUF ENTRY-U @ ROOT-BUF ROOT-U @ KEYED-IMAGE:CLOSURE-IN+ ;

: RESOLVE-CONTENT ( -- )
   s" tools/native-file-explicit.f" SOURCE-ROOT:RESOLVE drop
   ENTRY-BUF ENTRY-U COPY-ENTRY!
   SOURCE-ROOT:RESOLVED-ROOT$ ROOT-BUF ROOT-U COPY-ENTRY!
   CONTENT-KEY:OPEN {: fold:CONTENT-KEY:fold :}
   fold [: FILL-KEY ;] catch {: rc:n :} drop
   rc 0<> if fold CONTENT-KEY:DISCARD rc throw then
   fold
   KEY-HEX CONTENT-KEY:FINAL-HEX
   KEY-HEX FAMILY$ PATH-BUF PATH-U KEYED-IMAGE:PATH! ;

: RESOLVE ( -- )
   SOURCE-ROOT:ENGINE$ RUNNING-BUF RUNNING-U COPY-ENTRY!
   ENGINE-ID:PATH$ SOURCE-ROOT:ENGINE-FOR$
   SELECTED-BUF SELECTED-U COPY-ENTRY!
   SELECTED-BUF SELECTED-U @ SOURCE-ROOT:ENGINE!
   [: ['] RESOLVE-CONTENT SOURCE-ROOT:WITH-CWD ;] catch {: rc:n :}
   RUNNING-BUF RUNNING-U @ SOURCE-ROOT:ENGINE!
   rc 0<> if rc throw then ;

: OUT-ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

public

: PATH$ ( -- ptr u8 n )
   RESOLVE
   FAMILY$ PATH-BYTES ENGINE-ID:PATH$ PROGRAM$ ['] OUT-ARG 180000
   KEYED-IMAGE:ENSURE-WITH
   PATH-BYTES ;

;package

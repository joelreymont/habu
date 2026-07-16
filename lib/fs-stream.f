\ fs-stream.f - checked no-follow regular-file streaming.

require lib/errors.f
require lib/string.f
require lib/fs.f

package FS
public

$90 constant STREAM-STAT-BYTES

ENUM stream-stage validate walk open stat read callback ;ENUM

ENUM stream-result 0
   VARIANT ok
      FIELD mode n
   ;VARIANT
   VARIANT failed
      FIELD at stream-stage
      FIELD code n
   ;VARIANT
   VARIANT close-failed
      FIELD close n
   ;VARIANT
   VARIANT failed-close
      FIELD at stream-stage
      FIELD code n
      FIELD close n
   ;VARIANT
;ENUM

private

defer OPEN-OP ( ptr u8 n n -- n )
defer OPENAT-OP ( fd ptr u8 n n -- n )
defer FSTAT-OP ( fd ptr u8 -- rc )
defer READ-OP ( fd ptr u8 n -- n )
defer CLOSE-OP ( fd -- rc )

: INSTALL-SYSTEM ( -- )
   ['] open-errno is OPEN-OP
   ['] openat is OPENAT-OP
   ['] fstat64 is FSTAT-OP
   ['] read-fd is READ-OP
   ['] close-rc is CLOSE-OP ;

: VALIDATE ( ptr u8 n n n -- ) {: path:ptr pathu:n compcap:n cap:n :}
   path pathu CHECK-SAFE-PATH
   path pathu PATH-START pathu = if E-FS-PATH throw then
   compcap 2 < if E-FS-CAPACITY throw then
   cap 0 <= if E-FS-CAPACITY throw then ;

: VALIDATE-CODE ( ptr u8 n n n -- n ) {: path:ptr pathu:n compcap:n cap:n :}
   [: path pathu compcap cap VALIDATE ;] catch ;

: FAILED ( stream-stage n -- stream-result )
   construct stream-result failed ;

: CLOSE-FAILURE ( fd stream-stage n -- stream-result )
   {: fd:fd at:stream-stage code:n :}
   fd CLOSE-OP RC>N dup 0= if
      drop at code construct stream-result failed
      exit
   then
   at code rot construct stream-result failed-close ;

: CLOSE-SUCCESS ( fd n -- stream-result ) {: fd:fd mode:n :}
   CLOSE-OP RC>N dup 0= if
      drop mode construct stream-result ok
      exit
   then
   construct stream-result close-failed ;

\ typed-local-lint: allow-bare-local - q preserves its quotation effect.
: CALLBACK-CODE ( ptr n ptr u8 n [ ptr n ptr u8 n -- ] -- n )
   {: user:ptr a:ptr u:n q :}
   [: user a u q execute ;] catch ;

\ typed-local-lint: allow-bare-local - q preserves its quotation effect.
: OPENED ( fd ptr u8 n ptr u8 ptr n [ ptr n ptr u8 n -- ] -- stream-result )
   {: fd:fd buf:ptr cap:n statbuf:ptr user:ptr q :}
   fd statbuf FSTAT-OP RC>N dup 0 <> if
      construct stream-stage stat swap fd -rot CLOSE-FAILURE
      exit
   then
   drop
   statbuf FS-STAT:REGULAR? 0= if
      fd construct stream-stage stat E-FS-STAT CLOSE-FAILURE
      exit
   then
   statbuf FS-STAT:MODE@ {: mode:n :}
   begin
      fd buf cap READ-OP {: got:n :}
      got 0 < if
         fd construct stream-stage read got CLOSE-FAILURE
         exit
      then
      got cap > if
         fd construct stream-stage read E-FS-IO CLOSE-FAILURE
         exit
      then
      got 0= if fd mode CLOSE-SUCCESS exit then
      user buf got q CALLBACK-CODE {: code:n :}
      code 0 <> if
         fd construct stream-stage callback code CLOSE-FAILURE
         exit
      then
   again ;

create DOT-Z FS-DOT c, 0 c,
create ROOT-Z FS-SLASH c, 0 c,

: BASE-Z ( ptr u8 n -- ptr u8 ) {: path:ptr pathu:n :}
   path pathu PATH-START if ROOT-Z else DOT-Z then ;

: COPY-COMPONENT ( ptr u8 n n ptr u8 n -- ptr u8 )
   {: path:ptr start:n end:n comp:ptr compcap:n :}
   end start - {: u:n :}
   u 1+ compcap > if E-FS-CAPACITY throw then
   path start + comp u BYTE-COPY
   0 comp u + c!
   comp ;

\ typed-local-lint: allow-bare-local - q preserves its quotation effect.
: OPEN-FROM
   ( fd ptr u8 n n n ptr u8 n ptr u8 n ptr u8 ptr n [ ptr n ptr u8 n -- ] -- stream-result )
   {: dir:fd path:ptr pathu:n start:n depth:n comp:ptr compcap:n buf:ptr cap:n statbuf:ptr user:ptr q :}
   depth FS-MAX-DEPTH >= if
      dir construct stream-stage walk E-FS-DEPTH CLOSE-FAILURE
      exit
   then
   path pathu start COMPONENT-END {: end:n :}
   [: path start end comp compcap COPY-COMPONENT drop ;] catch {: copy-code:n :}
   copy-code 0 <> if
      dir construct stream-stage validate copy-code CLOSE-FAILURE
      exit
   then
   end pathu = if
      dir comp O-NOFOLLOW 0 OPENAT-OP {: raw:n :}
      raw 0 < if
         dir construct stream-stage open raw CLOSE-FAILURE
         exit
      then
      raw >FD {: file:fd :}
      dir CLOSE-OP RC>N {: close:n :}
      close 0 <> if
         file construct stream-stage walk close CLOSE-FAILURE
         exit
      then
      file buf cap statbuf user q OPENED
      exit
   then
   dir comp O-DIRECTORY O-NOFOLLOW or 0 OPENAT-OP {: raw:n :}
   raw 0 < if
      dir construct stream-stage walk raw CLOSE-FAILURE
      exit
   then
   raw >FD {: next:fd :}
   dir CLOSE-OP RC>N {: close:n :}
   close 0 <> if
      next construct stream-stage walk close CLOSE-FAILURE
      exit
   then
   next path pathu end 1+ depth 1+ comp compcap buf cap statbuf user q recurse ;

public

\ typed-local-lint: allow-bare-local - q preserves its quotation effect.
: STREAM-REGULAR
   ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr n [ ptr n ptr u8 n -- ] -- stream-result )
   {: path:ptr pathu:n comp:ptr compcap:n buf:ptr cap:n statbuf:ptr statcap:n user:ptr q :}
   path pathu compcap cap VALIDATE-CODE dup 0 <> if
      construct stream-stage validate swap FAILED
      exit
   then
   drop
   statcap STREAM-STAT-BYTES < if
      construct stream-stage validate E-FS-CAPACITY FAILED
      exit
   then
   path pathu BASE-Z O-DIRECTORY O-NOFOLLOW or 0 OPEN-OP {: raw:n :}
   raw 0 < if construct stream-stage walk raw FAILED exit then
   raw >FD path pathu path pathu PATH-START 0 comp compcap
   buf cap statbuf user q OPEN-FROM ;

private

INSTALL-SYSTEM

;package

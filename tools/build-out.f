\ build-out.f - where a build writes what it makes: <out>/<engine-key>/, the key
\ the 64-hex SHA-256 of an engine binary (KEY-OF!). <out> is $HABU_OUT when set and
\ non-empty (relative to SOURCE-ROOT:CWD$), else .out under the working directory.
\ obj/<key>.hfo holds one object per file key; img/<family> lives under the engine
\ that builds the image (tools/keyed-image.f). Outputs a later engine must find
\ live under <out> itself (BASE$). PROCESS-WIDE: cleared before image capture.

require src/lib/errors.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/image-lifecycle.f
require src/lib/engine-id.f
require src/os/env-base.f
require src/core/include.f
require src/core/sha256.f

package BUILD-OUT

private

64 constant KEY-LEN

create OUT-BUF PATH-CAP allot     variable OUT-U
create ROOT-BUF PATH-CAP allot    variable ROOT-U
create OBJ-BUF PATH-CAP allot     variable OBJ-U
create NAME-BUF PATH-CAP allot
create PATH-BUF PATH-CAP allot
create IMG-BUF PATH-CAP allot
create IMG-PATH-BUF PATH-CAP allot
create KEY-CTX SHA256-FILE-CTX-BYTES allot
variable CLEANUP-LIVE

: CLEAR-SPAN ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 ?do 0 a i + c! loop ;

: CLEAR ( -- )
   OUT-BUF PATH-CAP CLEAR-SPAN
   ROOT-BUF PATH-CAP CLEAR-SPAN
   OBJ-BUF PATH-CAP CLEAR-SPAN
   IMG-BUF PATH-CAP CLEAR-SPAN
   IMG-PATH-BUF PATH-CAP CLEAR-SPAN
   NAME-BUF PATH-CAP CLEAR-SPAN
   PATH-BUF PATH-CAP CLEAR-SPAN
   KEY-CTX SHA256-FILE-CTX-BYTES CLEAR-SPAN
   0 OUT-U ! 0 ROOT-U ! 0 OBJ-U ! 0 CLEANUP-LIVE ! ;

: REGISTER-CLEANUP ( -- )
   CLEANUP-LIVE @ 0= if
      [: CLEAR ;] IMAGE-LIFECYCLE:REGISTER
      -1 CLEANUP-LIVE !
   then ;

\ The out root in OUT-BUF. HABU_OUT is read unobserved, so it never becomes a
\ build input.
: OUT-ROOT ( -- n )
   s" HABU_OUT" ENV-INPUT:CURRENT$ {: v:ptr vu:n :}
   vu 0= if SOURCE-ROOT:CWD$ s" .out" OUT-BUF FS:JOIN-PATH exit then
   v c@ FS:SLASH <> if SOURCE-ROOT:CWD$ v vu OUT-BUF FS:JOIN-PATH exit then
   vu PATH-CAP > if E-FS-CAPACITY throw then
   v OUT-BUF vu BYTE-COPY
   vu ;

public

: BASE$ ( -- ptr u8 n )
   OUT-U @ 0= if
      REGISTER-CLEANUP
      OUT-ROOT {: outu:n :}
      OUT-BUF outu FS:MAKE-DIRS
      outu OUT-U !
   then
   OUT-BUF OUT-U @ ;

: ROOT$ ( -- ptr u8 n )
   ROOT-U @ 0= if
      BASE$ ENGINE-ID:KEY$ ROOT-BUF FS:JOIN-PATH {: rootu:n :}
      ROOT-BUF rootu FS:MAKE-DIRS
      rootu ROOT-U !
   then
   ROOT-BUF ROOT-U @ ;

: OBJ$ ( ptr u8 n -- ptr u8 n )
   {: key:ptr keyu:n :}
   keyu 0 <= if E-FS-PATH throw then
   keyu PATH-CAP 4 - > if E-FS-CAPACITY throw then
   OBJ-U @ 0= if
      ROOT$ s" obj" OBJ-BUF FS:JOIN-PATH {: obju:n :}
      OBJ-BUF obju FS:MAKE-DIRS
      obju OBJ-U !
   then
   key NAME-BUF keyu BYTE-COPY
   s" .hfo" drop NAME-BUF keyu + 4 BYTE-COPY
   OBJ-BUF OBJ-U @ NAME-BUF keyu 4 + PATH-BUF FS:JOIN-PATH
   PATH-BUF swap ;


\ The engine key of the binary at the path into a KEY-LEN-byte buffer: the
\ lowercase hex SHA-256 of its bytes, the string ENGINE-ID:KEY$ gives for the
\ running engine's own file. An unopenable file throws E-FS-OPEN and a failed
\ read E-FS-IO.
: KEY-OF! ( ptr u8 n ptr u8 -- )
   {: path:ptr pathu:n dst:ptr :}
   REGISTER-CLEANUP
   KEY-CTX path pathu dst SHA256-FILE-HEX-IN {: rc:n :}
   rc SHA-E-OPEN = if E-FS-OPEN throw then
   rc 0<> if E-FS-IO throw then ;


\ The path of a family's image, or of a record named after it, under the img/
\ directory of the engine whose key is given; the directory is made.
: IMG$ ( ptr u8 n ptr u8 -- ptr u8 n )
   {: fam:ptr famu:n key:ptr :}
   famu 0 <= if E-FS-PATH throw then
   BASE$ key KEY-LEN IMG-PATH-BUF FS:JOIN-PATH {: rootu:n :}
   IMG-PATH-BUF rootu s" img" IMG-BUF FS:JOIN-PATH {: imgu:n :}
   IMG-BUF imgu FS:MAKE-DIRS
   IMG-BUF imgu fam famu IMG-PATH-BUF FS:JOIN-PATH
   IMG-PATH-BUF swap ;

;package

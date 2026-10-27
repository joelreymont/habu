\ build-out.f - where a build writes what it makes.
\ A build's file objects live under one out root, one directory per engine:
\ <out>/<engine-key>/, where <engine-key> is the running binary's content key
\ (ENGINE-ID:KEY$). <out> is $HABU_OUT when that is set and non-empty, a
\ relative value resolved against the canonical working directory
\ (SOURCE-ROOT:CWD$); otherwise it is .out under that directory, so outputs go
\ with the tree. obj/<key>.hfo holds one compiled file object per file key,
\ replaced under its own key; img/<family> holds a family's saved image and the
\ records beside it (tools/keyed-image.f). Outputs that a later engine must
\ still find, such as the fixpoint refresh stamp, live directly under <out>
\ (BASE$), since a refresh replaces the engine that keys <out>/<engine-key>/.
\ No other variable, root or fallback exists, and a directory that cannot be
\ made throws the filesystem cause (E-FS-IO with FFI:ERRNO, or E-FS-CAPACITY
\ for a path past FS-PATH-CAP).
\
\ STORAGE CLASS. PROCESS-WIDE: the paths are resolved once per process and
\ cleared before image capture through a one-shot lifecycle hook, so a saved
\ image resolves its own out root and engine when it first asks.

require src/lib/errors.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/image-lifecycle.f
require src/lib/engine-id.f
require src/os/env-base.f
require src/core/include.f

package BUILD-OUT

private

create OUT-BUF FS-PATH-CAP allot     variable OUT-U
create ROOT-BUF FS-PATH-CAP allot    variable ROOT-U
create OBJ-BUF FS-PATH-CAP allot     variable OBJ-U
create IMG-BUF FS-PATH-CAP allot     variable IMG-U
create NAME-BUF FS-PATH-CAP allot
create PATH-BUF FS-PATH-CAP allot
create IMG-PATH-BUF FS-PATH-CAP allot
variable CLEANUP-LIVE

: CLEAR-SPAN ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 ?do 0 a i + c! loop ;

: CLEAR ( -- )
   OUT-BUF FS-PATH-CAP CLEAR-SPAN
   ROOT-BUF FS-PATH-CAP CLEAR-SPAN
   OBJ-BUF FS-PATH-CAP CLEAR-SPAN
   IMG-BUF FS-PATH-CAP CLEAR-SPAN
   IMG-PATH-BUF FS-PATH-CAP CLEAR-SPAN
   NAME-BUF FS-PATH-CAP CLEAR-SPAN
   PATH-BUF FS-PATH-CAP CLEAR-SPAN
   0 OUT-U ! 0 ROOT-U ! 0 OBJ-U ! 0 IMG-U ! 0 CLEANUP-LIVE ! ;

: REGISTER-CLEANUP ( -- )
   CLEANUP-LIVE @ 0= if
      [: CLEAR ;] IMAGE-LIFECYCLE:REGISTER
      -1 CLEANUP-LIVE !
   then ;

\ The out root in OUT-BUF. HABU_OUT is read unobserved, so it never becomes a
\ build input.
: OUT-ROOT ( -- n )
   s" HABU_OUT" ENV-INPUT:CURRENT$ {: v:ptr vu:n :}
   vu 0= if SOURCE-ROOT:CWD$ s" .out" OUT-BUF JOIN-PATH exit then
   v c@ FS-SLASH <> if SOURCE-ROOT:CWD$ v vu OUT-BUF JOIN-PATH exit then
   vu FS-PATH-CAP > if E-FS-CAPACITY throw then
   v OUT-BUF vu BYTE-COPY
   vu ;

public

\ The out root itself, for outputs not keyed by engine.
: BASE$ ( -- ptr u8 n )
   OUT-U @ 0= if
      REGISTER-CLEANUP
      OUT-ROOT {: outu:n :}
      OUT-BUF outu MAKE-DIRS
      outu OUT-U !
   then
   OUT-BUF OUT-U @ ;

: ROOT$ ( -- ptr u8 n )
   ROOT-U @ 0= if
      BASE$ ENGINE-ID:KEY$ ROOT-BUF JOIN-PATH {: rootu:n :}
      ROOT-BUF rootu MAKE-DIRS
      rootu ROOT-U !
   then
   ROOT-BUF ROOT-U @ ;

\ The object file for a file key.
: OBJ$ ( ptr u8 n -- ptr u8 n )
   {: key:ptr keyu:n :}
   keyu 0 <= if E-FS-PATH throw then
   keyu FS-PATH-CAP 4 - > if E-FS-CAPACITY throw then
   OBJ-U @ 0= if
      ROOT$ s" obj" OBJ-BUF JOIN-PATH {: obju:n :}
      OBJ-BUF obju MAKE-DIRS
      obju OBJ-U !
   then
   key NAME-BUF keyu BYTE-COPY
   s" .hfo" drop NAME-BUF keyu + 4 BYTE-COPY
   OBJ-BUF OBJ-U @ NAME-BUF keyu 4 + PATH-BUF JOIN-PATH
   PATH-BUF swap ;


\ The path of a family's image, or of a record named after it, under img/.
: IMG$ ( ptr u8 n -- ptr u8 n )
   {: fam:ptr famu:n :}
   famu 0 <= if E-FS-PATH throw then
   IMG-U @ 0= if
      ROOT$ s" img" IMG-BUF JOIN-PATH {: imgu:n :}
      IMG-BUF imgu MAKE-DIRS
      imgu IMG-U !
   then
   IMG-BUF IMG-U @ fam famu IMG-PATH-BUF JOIN-PATH
   IMG-PATH-BUF swap ;

;package

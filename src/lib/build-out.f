\ build-out.f - where a build writes what it makes, under <out>: $HABU_OUT when set
\ and non-empty (relative to SOURCE-ROOT:CWD$), else .out under the working
\ directory. <out>/obj/<hb's stamp>/ holds the objects that hb wrote, each under the
\ name its writer gives (OBJ$). Outputs a later engine must find live under <out>
\ itself (BASE$).
\ PROCESS-WIDE: cleared before image capture.
require src/lib/error.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/image-lifecycle.f
require src/lib/engine-id.f
require src/os/env-base.f
require src/core/include.f

package BUILD-OUT

private

create OUT-BUF PATH-CAP allot     variable OUT-U
create OBJ-BUF PATH-CAP allot     variable OBJ-U
create PATH-BUF PATH-CAP allot
variable CLEANUP-LIVE

: CLEAR-SPAN ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 ?do 0 a i + c! loop ;

: CLEAR ( -- )
   OUT-BUF PATH-CAP CLEAR-SPAN
   OBJ-BUF PATH-CAP CLEAR-SPAN
   PATH-BUF PATH-CAP CLEAR-SPAN
   0 OUT-U ! 0 OBJ-U ! 0 CLEANUP-LIVE ! ;

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

: OBJ$ ( ptr u8 n -- ptr u8 n )
   {: name:ptr nameu:n :}
   nameu 0 <= if E-FS-PATH throw then
   OBJ-U @ 0= if
      BASE$ s" obj" PATH-BUF FS:JOIN-PATH {: obju:n :}
      PATH-BUF obju ENGINE-ID:KEY$ OBJ-BUF FS:JOIN-PATH {: stampu:n :}
      OBJ-BUF stampu FS:MAKE-DIRS
      stampu OBJ-U !
   then
   OBJ-BUF OBJ-U @ name nameu PATH-BUF FS:JOIN-PATH
   PATH-BUF swap ;

;package

\ build-out.f - where a build writes what it makes, under <out>: $HABU_OUT when set
\ and non-empty (relative to SOURCE-ROOT:CWD$), else .out under the working
\ directory. <out>/obj/<hb's stamp>/ holds the objects that hb wrote, each under the
\ name its writer gives (OBJ$). Outputs a later engine must find live under <out>
\ itself (BASE$).
\ PROCESS-WIDE: the caches are scratch, so a build leaves them out.
require src/lib/error.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/engine-id.f
require src/os/env-base.f
require src/core/include.f

package BUILD-OUT

private

\ The caches belong to the process that filled them; zero lengths mean nothing
\ is cached yet.
PATH-CAP SCRATCH-BUFFER: OUT-BUF
SCRATCH-VARIABLE OUT-U
PATH-CAP SCRATCH-BUFFER: OBJ-BUF
SCRATCH-VARIABLE OBJ-U
PATH-CAP SCRATCH-BUFFER: PATH-BUF

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

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

PATH-CAP SCRATCH-BUFFER: OUT-BUF
SCRATCH-VARIABLE OUT-U
PATH-CAP SCRATCH-BUFFER: OBJ-BUF
SCRATCH-VARIABLE OBJ-U
PATH-CAP SCRATCH-BUFFER: PATH-BUF

\ The out root in OUT-BUF. HABU_OUT is read through ENV-INPUT:CURRENT$, so its
\ value is no build input.
: OUT-ROOT ( -- n )
   s" HABU_OUT" ENV-INPUT:CURRENT$ {: v:str :}
   v STR:LEN 0= if SOURCE-ROOT:CWD$ s" .out" OUT-BUF FS:JOIN-PATH exit then
   v STR:DATA c@ FS:SLASH <> if SOURCE-ROOT:CWD$ v OUT-BUF FS:JOIN-PATH exit then
   v STR:LEN PATH-CAP > if E-FS-CAPACITY throw then
   v STR:DATA OUT-BUF v STR:LEN BYTE-COPY
   v STR:LEN ;

public

: BASE$ ( -- str )
   OUT-U @ 0= if
      OUT-ROOT {: outu:n :}
      OUT-BUF outu >str FS:MAKE-DIRS
      outu OUT-U !
   then
   OUT-BUF OUT-U @ >str ;

: OBJ$ ( str -- str )
   {: name:str :}
   name STR:LEN 0 <= if E-FS-PATH throw then
   OBJ-U @ 0= if
      BASE$ s" obj" PATH-BUF FS:JOIN-PATH {: obju:n :}
      PATH-BUF obju >str ENGINE-ID:KEY$ OBJ-BUF FS:JOIN-PATH {: stampu:n :}
      OBJ-BUF stampu >str FS:MAKE-DIRS
      stampu OBJ-U !
   then
   OBJ-BUF OBJ-U @ >str name PATH-BUF FS:JOIN-PATH
   PATH-BUF swap >str ;

;package

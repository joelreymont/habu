\ build-cache.f - canonical checked build-cache root selection.
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/fs-root.f, and
\ lib/fs-mutate.f.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-root.f
require lib/fs-mutate.f

package BUILD-CACHE
public

ENUM source explicit xdg home tmp ;ENUM

private

create ROOT-BUF FS-PATH-CAP allot
1 LAYOUT-BUFFER SOURCE-BUF source

variable ROOT-U
variable OVERRIDE?
variable READY?

: TRUE ( -- bool )
   0 0= ;

: FALSE ( -- bool )
   TRUE 0= ;

: ROOT-BYTES ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: ROOT-COPY! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= if E-BUILD-PATH throw then
   u FS-PATH-CAP > if E-BUILD-PATH throw then
   a ROOT-BUF u BYTE-COPY
   u ROOT-U ! ;

: ROOT-JOIN! ( ptr u8 n ptr u8 n -- )
   ROOT-BUF JOIN-PATH ROOT-U ! ;

: SOURCE-PTR ( -- ptr source )
   0 SOURCE-BUF ;

: SOURCE! ( source -- )
   SOURCE-PTR ! ;

: SOURCE-VALUE ( -- source )
   SOURCE-PTR @ ;

: PREPARE-ROOT ( -- )
   ROOT-BYTES 2dup EXISTS? if
      2dup DIR? 0= if 2drop E-BUILD-PATH throw then
   else
      2dup MAKE-DIRS
   then
   2dup FS:WRITABLE-ROOT? 0= if 2drop E-BUILD-PATH throw then
   2drop
   TRUE READY? ! ;

: SELECT-EXPLICIT ( ptr u8 n -- )
   ROOT-COPY!
   construct source explicit SOURCE!
   PREPARE-ROOT ;

: SELECT-SUFFIX ( ptr u8 n ptr u8 n source -- )
   {: a:ptr u:n suffix:ptr suffixu:n source:source :}
   a u suffix suffixu ROOT-JOIN!
   source SOURCE!
   PREPARE-ROOT ;

: NONEMPTY? ( ptr u8 n -- bool )
   nip 0 > ;

: SELECT-ENV ( -- )
   s" HABU_BUILD_CACHE" GETENV 2dup NONEMPTY? if SELECT-EXPLICIT exit then 2drop
   s" XDG_CACHE_HOME" GETENV 2dup NONEMPTY? if
      s" habu-build" construct source xdg SELECT-SUFFIX exit
   then 2drop
   s" HOME" GETENV 2dup NONEMPTY? if
      s" .cache/habu-build" construct source home SELECT-SUFFIX exit
   then 2drop
   s" TMPDIR" GETENV 2dup NONEMPTY? if
      s" habu-build" construct source tmp SELECT-SUFFIX exit
   then 2drop
   E-BUILD-PATH throw ;

: ENSURE ( -- )
   READY? @ 0 <> if exit then
   OVERRIDE? @ 0 <> if
      construct source explicit SOURCE!
      PREPARE-ROOT
      exit
   then
   SELECT-ENV ;

public

: RESET ( -- )
   0 ROOT-U !
   FALSE OVERRIDE? !
   FALSE READY? ! ;

: ROOT! ( ptr u8 n -- )
   ROOT-COPY!
   TRUE OVERRIDE? !
   FALSE READY? ! ;

: ROOT$ ( -- ptr u8 n )
   ENSURE
   ROOT-BYTES ;

: SOURCE ( -- source )
   ENSURE
   SOURCE-VALUE ;

: RESOLVE ( -- ptr u8 n source )
   ENSURE
   ROOT-BYTES SOURCE-VALUE ;

: SOURCE$ ( source -- ptr u8 n )
   MATCH source
      explicit OF s" explicit" ENDOF
      xdg OF s" xdg" ENDOF
      home OF s" home" ENDOF
      tmp OF s" tmp" ENDOF
   ;MATCH ;

;package

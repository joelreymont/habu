\ Fresh engine in a checkout whose lib and test directories are symlinks.
require lib/test.f

using SOURCE-ROOT
package SOURCE-ROOT-ALIAS-CHILD

variable LOADED
create PATH-BUF INCLUDE-PATH-CAP allot
variable PATH-U

public
: BUMP ( n -- ) LOADED +! ;
private

: ROOT$ ( -- ptr u8 n ) 0 SCRIPT-ARGV$ ;

: PATH! ( ptr u8 n -- ) {: a:ptr u:n :}
   u INCLUDE-PATH-CAP > if E-FS-CAPACITY throw then
   a PATH-BUF u BYTE-COPY u PATH-U ! ;

: PATH$ ( -- ptr u8 n ) PATH-BUF PATH-U @ ;

\ JOIN storage is resolver scratch; each query may overwrite it. Retain the
\ requested spelling so every consumer sees the same original bytes.
: KNOWN ( ptr u8 n -- )
   PATH!
   PATH$ ENGINE-PROVIDES? TTRUE
   PATH$ RESOLVE TTRUE 2drop
   PATH$ ENTRY-RESOLVE TTRUE 2drop
   PATH$ required ;

: UNKNOWN ( ptr u8 n -- )
   PATH!
   PATH$ ENGINE-PROVIDES? TFALSE
   PATH$ RESOLVE TFALSE 2drop ;

: PORTABLE ( -- )
   REQUIRE-N @ {: before:n :}
   s" lib/errors.f" KNOWN
   s" ./lib/errors.f" KNOWN
   s" lib/../lib/errors.f" KNOWN
   CWD$ s" lib/errors.f" JOIN KNOWN
   CWD$ s" ./lib/../lib/errors.f" JOIN KNOWN
   \ No src directory exists here: a missing compiled file is still provided.
   s" src/core/util.f" KNOWN
   REQUIRE-N @ before T=
   REQUIRE-SNAPSHOT
   s" lib/errors.f" ENGINE-PROVIDES? TTRUE
   s" lib/errors.f" RESOLVE TFALSE 2drop
   REQUIRE-RESTORE ;

: OWNED ( ptr u8 n -- )
   [: s" lib/errors.f" required ;] WITH ;

: DISTINCT ( -- )
   ROOT$ s" a/lib/errors.f" JOIN UNKNOWN
   ROOT$ s" a" JOIN OWNED
   ROOT$ s" b/lib/errors.f" JOIN UNKNOWN
   ROOT$ s" b" JOIN OWNED
   LOADED @ 11 T=
   \ Lexical branch/.. is CWD, but physical branch/.. is ROOT/other.
   s" branch/../lib/errors.f" UNKNOWN
   CWD$ s" branch/../lib/errors.f" JOIN UNKNOWN
   s" branch/../lib/errors.f" required
   CWD$ s" branch/../lib/errors.f" JOIN required
   LOADED @ 111 T= ;

: RUN ( -- )
   T-RESET CWD$ [: PORTABLE DISTINCT ;] WITH T-REPORT
   s" source root aliases: ok" type cr ;

RUN
;package
;using

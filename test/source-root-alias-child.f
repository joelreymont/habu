\ Fresh engine in a checkout whose lib and test directories are symlinks.
require lib/test.f

using SOURCE-ROOT
package SOURCE-ROOT-ALIAS-CHILD

variable LOADED

public
: BUMP ( n -- ) LOADED +! ;
private

: ROOT$ ( -- ptr u8 n ) 0 SCRIPT-ARGV$ ;

: KNOWN ( ptr u8 n -- )
   2dup ENGINE-PROVIDES? TTRUE
   2dup RESOLVE TTRUE 2drop
   2dup ENTRY-RESOLVE TTRUE 2drop
   required ;

: UNKNOWN ( ptr u8 n -- )
   2dup ENGINE-PROVIDES? TFALSE
   RESOLVE TFALSE 2drop ;

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

\ Fresh engine in a checkout whose lib, src and test directories are symlinks.
require src/lib/test.f

using SOURCE-ROOT
package SOURCE-ROOT-ALIAS-CHILD

variable LOADED
create PATH-BUF PATH-CAP allot
variable PATH-U

public
: BUMP ( n -- ) LOADED +! ;
private

: ROOT$ ( -- str ) 0 SCRIPT-ARGV$ ;

: PATH! ( str -- ) {: s:str :}
   s STR:LEN PATH-CAP > if E-FS-CAPACITY throw then
   s STR:DATA PATH-BUF s STR:LEN BYTE-COPY s STR:LEN PATH-U ! ;

: PATH$ ( -- str ) PATH-BUF PATH-U @ >str ;

\ JOIN storage is resolver scratch; each query may overwrite it. Retain the
\ requested spelling so every consumer sees the same original bytes.
: KNOWN ( str -- )
   PATH!
   PATH$ REQUIRE-REG:ENGINE-PROVIDES? TTRUE
   PATH$ RESOLVE TTRUE drop
   PATH$ ENTRY-RESOLVE TTRUE drop
   PATH$ required ;

: UNKNOWN ( str -- )
   PATH!
   PATH$ REQUIRE-REG:ENGINE-PROVIDES? TFALSE
   PATH$ RESOLVE TFALSE drop ;

: PORTABLE ( -- )
   REQUIRE-REG:COUNT {: before:n :}
   s" src/lib/error.f" KNOWN
   s" ./src/lib/error.f" KNOWN
   s" src/lib/../lib/error.f" KNOWN
   CWD$ s" src/lib/error.f" JOIN KNOWN
   CWD$ s" ./src/lib/../lib/error.f" JOIN KNOWN
   \ src is a link too, so this checkout is a tree and the engine root.
   s" src/core/util.f" KNOWN
   REQUIRE-REG:COUNT before T=
   REQUIRE-REG:SNAPSHOT
   s" src/lib/error.f" REQUIRE-REG:ENGINE-PROVIDES? TTRUE
   s" src/lib/error.f" RESOLVE TFALSE drop
   REQUIRE-REG:RESTORE ;

: OWNED ( str -- )
   [: s" src/lib/error.f" required ;] WITH ;

: DISTINCT ( -- )
   ROOT$ s" a/src/lib/error.f" JOIN UNKNOWN
   ROOT$ s" a" JOIN OWNED
   ROOT$ s" b/src/lib/error.f" JOIN UNKNOWN
   ROOT$ s" b" JOIN OWNED
   LOADED @ 11 T=
   \ Lexical branch/.. is CWD, but physical branch/.. is ROOT/other.
   s" branch/../src/lib/error.f" UNKNOWN
   CWD$ s" branch/../src/lib/error.f" JOIN UNKNOWN
   s" branch/../src/lib/error.f" required
   CWD$ s" branch/../src/lib/error.f" JOIN required
   LOADED @ 111 T= ;

: RUN ( -- )
   T-RESET CWD$ [: PORTABLE DISTINCT ;] WITH T-REPORT
   s" source root aliases: ok" type cr ;

RUN
;package
;using

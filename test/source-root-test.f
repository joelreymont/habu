\ Named entries own root-relative dependencies; canonical identity deduplicates.
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require tools/source-discovery.f
require tools/event-closure-lib.f

using SOURCE-ROOT
using DISCOVER
using EC
package SOURCE-ROOT-TEST

FS-PATH-CAP 1+ constant CAP
create ROOT CAP allot
create APP-A CAP allot
create APP-B CAP allot
create FALLBACK CAP allot
create SAVED-ROOT CAP allot
create TARGET CAP allot
create LINK CAP allot
variable ROOT-U
variable APP-A-U
variable APP-B-U
variable FALLBACK-U
variable SAVED-ROOT-U
variable TARGET-U
variable LINK-U
variable LOADED
variable SAVED-DEPTH

public
: BUMP ( n -- ) LOADED +! ;
private

: COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY u lenp ! ;

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: A$ ( -- ptr u8 n ) APP-A APP-A-U @ ;
: B$ ( -- ptr u8 n ) APP-B APP-B-U @ ;
: FALLBACK$ ( -- ptr u8 n ) FALLBACK FALLBACK-U @ ;
: A-PATH ( ptr u8 n -- ptr u8 n ) A$ 2swap JOIN ;
: B-PATH ( ptr u8 n -- ptr u8 n ) B$ 2swap JOIN ;
: FALLBACK-PATH ( ptr u8 n -- ptr u8 n ) FALLBACK$ 2swap JOIN ;

: RESTORED ( -- )
   CURRENT$ SAVED-ROOT SAVED-ROOT-U @ T$=
   INCLUDE-DEPTH @ SAVED-DEPTH @ T= ;

: PREP ( -- )
   CLEANUP-RESET 0 LOADED !
   CURRENT$ SAVED-ROOT SAVED-ROOT-U COPY!
   INCLUDE-DEPTH @ SAVED-DEPTH !
   s" habu-source-root" TMPDIR-MKDIR CANONICAL TTRUE ROOT ROOT-U COPY!
   ROOT$ CLEANUP-TREE+
   ROOT$ s" a" JOIN APP-A APP-A-U COPY!
   ROOT$ s" b" JOIN APP-B APP-B-U COPY!
   s" src/nested" A-PATH MAKE-DIRS
   s" src" B-PATH MAKE-DIRS
   s" src/leaf.f" A-PATH s" 1 SOURCE-ROOT-TEST:BUMP" WRITE-ALL
   s" src/nested/dep.f" A-PATH s" require src/leaf.f" WRITE-ALL
   s" src/leaf.f" B-PATH s" 10 SOURCE-ROOT-TEST:BUMP" WRITE-ALL
   s" main.f" B-PATH s" require src/leaf.f" WRITE-ALL
   s" boom.f" A-PATH s" 9132 throw" WRITE-ALL
   s" ." s" habu-root-fallback" MAKE-TEMP-DIR FALLBACK FALLBACK-U COPY!
   FALLBACK$ CLEANUP-TREE+
   s" leaf.f" FALLBACK-PATH s" 100 SOURCE-ROOT-TEST:BUMP" WRITE-ALL
   SB-RESET s" require " SB-APPEND
   s" leaf.f" FALLBACK-PATH SB-APPEND
   s" shared.f" FALLBACK-PATH SB$ WRITE-ALL
   FALLBACK$ A-PATH MAKE-DIRS
   s" leaf.f" FALLBACK-PATH A-PATH s" 1000 SOURCE-ROOT-TEST:BUMP" WRITE-ALL
   SB-RESET S\" require src/nested/dep.f\nrequire src/leaf.f\nrequire " SB-APPEND
   s" shared.f" FALLBACK-PATH SB-APPEND
   s" main.f" A-PATH SB$ WRITE-ALL ;

: LOAD-ENTRIES ( -- )
   s" main.f" A-PATH script-required
   LOADED @ 101 T= RESTORED
   s" main.f" B-PATH script-required
   LOADED @ 111 T= RESTORED ;

: ALIASES ( -- )
   s" src/leaf.f" A-PATH TARGET TARGET-U COPY!
   s" src/alias.f" A-PATH LINK LINK-U COPY!
   TARGET TARGET-U @ LINK LINK-U @ MAKE-SYMLINK
   s" src/./nested/../leaf.f" A-PATH required
   LINK LINK-U @ required
   TARGET TARGET-U @ required
   LOADED @ 111 T=
   LINK LINK-U @ included
   TARGET TARGET-U @ included
   LOADED @ 113 T= RESTORED ;

: PROVIDED-MISSING ( -- )
   s" absent.f" A-PATH provided
   s" ./absent.f" A-PATH required
   RESTORED ;

: THROW-LOAD ( -- ) s" boom.f" A-PATH included ;

: THROW-RESTORES ( -- )
   [: THROW-LOAD ;] 9132 TTHROWSQ
   RESTORED ;

: DISCOVERY ( -- )
   s" main.f" A-PATH DISCOVER:RUN
   EVENT-COUNT 3 T=
   0 EVENT-PATH@ s" src/nested/dep.f" A-PATH T$=
   0 SOURCE-EVENT:ROOT@ A$ T$=
   2 EVENT-PATH@ s" shared.f" FALLBACK-PATH CANONICAL TTRUE T$=
   2 SOURCE-EVENT:ROOT@ CWD$ T$=
   s" src/nested/dep.f" A-PATH A$ RUN-IN
   EVENT-COUNT 1 T=
   0 EVENT-PATH@ s" src/leaf.f" A-PATH T$=
   0 SOURCE-EVENT:ROOT@ A$ T$=
   RESTORED ;

: CLOSURE ( -- )
   s" main.f" A-PATH BUILD
   EC:COUNT 5 T=
   0 PATH$ s" main.f" A-PATH T$=
   4 PATH$ s" leaf.f" FALLBACK-PATH CANONICAL TTRUE T$=
   RESTORED ;

: RUN ( -- )
   T-RESET PREP
   LOAD-ENTRIES ALIASES PROVIDED-MISSING THROW-RESTORES DISCOVERY CLOSURE
   CLEANUP-RUN T-REPORT ;

RUN
;package
;using
;using
;using

\ dynamic-tail-manifest.f - declared dynamic-tail boundaries for discovery.
\
\ The whole-file discovery pass (tools/source-discovery.f) rejects fail-closed
\ any source whose loader dataflow is not statically visible: a dynamic
\ (non-literal) loader path at top level, a shadowed/undefined/retired loader
\ word, or an unsupported string opener before a top-level loader. A file
\ listed here is a reviewed boundary whose loader dataflow the pass cannot
\ read: discovery tolerates (skips) exactly those forms in it and records only
\ its statically-visible loader events, so the event log is a lower bound on
\ what that file loads. The one entry is the loader's own definition site,
\ whose definitions of the loader words are no shadows. Only the walk consults
\ this table: the engine provides that file, so no check composes it. Keep this
\ table minimal; every entry carries a one-line reason, and an entry is retired
\ when its unreadable form is replaced by static loader forms.
\
\ An entry is a file of the tree this file was loaded from, by its canonical
\ path there (CANON$), whatever working directory the check runs in or
\ spelling names the file: the tree's root is fixed when this file loads.

require lib/errors.f
require lib/string.f

package DTM
using SOURCE-ROOT

create CANON-PATH PATH-CAP 1 + allot
variable CANON-U
create ROOT PATH-CAP allot
variable ROOT-U

\ While this file loads, CURRENT$ is the root that resolved it, at most
\ PATH-CAP bytes as every root is. It is fixed here, before a working directory
\ in another tree can stand in for it.
CURRENT$ dup ROOT-U ! ROOT swap BYTE-COPY

: ROOT$ ( -- ptr u8 n )
   ROOT ROOT-U @ ;

public

1 constant COUNT

: PATH$ ( n -- ptr u8 n ) {: i:n :}
   i 0 = if s" src/core/include.f" exit then
   E-TBL-BOUNDS throw ;

: REASON$ ( n -- ptr u8 n ) {: i:n :}
   i 0 = if s" it defines the loader words, so their names stand as definition names and inside each other's bodies; it loads no source of its own" exit then
   E-TBL-BOUNDS throw ;

\ Entry I's canonical absolute path, PATH$ in the tree this file was loaded
\ from. The string lasts until the next CANONICAL.
: CANON$ ( n -- ptr u8 n ) {: i:n :}
   ROOT$ i PATH$ JOIN CANONICAL drop ;

\ Whether the file at PATH, a relative one read from the working directory, is
\ an entry: its canonical path is one's CANON$.
: KNOWN? ( ptr u8 n -- bool )
   CANONICAL drop {: a:ptr u:n :}
   a CANON-PATH u BYTE-COPY u CANON-U !
   0 begin dup COUNT < while
      dup CANON$
      CANON-PATH CANON-U @ STR= if drop STR-TRUE exit then
      1+
   repeat drop STR-FALSE ;

;using
;package

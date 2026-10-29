\ code-owner.f - turn a live code address into the records that own it. Runs inside
\ the engine under study: JIT-region addresses have no external symbols, and each
\ record holds its routine's start and length. Every owner is reported, as an
\ `EXPORT` alias or `;does` companion can cover one routine twice. REGION-OWNER.
\ takes a region offset, which is stable across ASLR runs.
\   <engine> --load tools/code-owner-main.f -- <hex-or-decimal region offset>

require src/lib/fmt.f                        \ FMT:.INT

package CODE-OWNER

private

variable HITS

: SPAN? ( n ptr n -- bool ) {: a:n r:ptr :}
   r XREF-WORDLIST XREF-NAMESPACE-WL = if 0 0= 0= exit then   \ a package row's [0] is a wid, not code
   r XREF-RETIRED? if 0 0= 0= exit then
   a r XREF-START >=  a r XREF-START r XREF-CODE-BYTES + <  and ;

: ROW. ( n ptr n -- ) {: a:n r:ptr :}
   s" owner=" type r XREF-NAME$ type
   s"  wid=" type r XREF-WORDLIST FMT:.INT
   s"  start=" type r XREF-START FMT:.INT
   s"  len=" type r XREF-CODE-BYTES FMT:.INT
   s"  off=" type a r XREF-START - FMT:.INT cr ;

public

\ The live region base this run got, so a caller can turn its own offsets into
\ addresses the way REGION-OWNER. does.
: BASE ( -- n ) dbase@ ;

\ Every record whose routine contains `a`. Prints one line each and answers how
\ many there were, so a caller can tell "no owner" (a primitive in the loaded
\ __text, or an address in no routine at all) from a silent success.
: AT ( n -- n ) {: a:n :}
   0 HITS !
   ndict@ 0 ?do
      i XREF-REC {: r:ptr :}
      a r SPAN? if  a r ROW.  1 HITS +!  then
   loop
   HITS @ ;

: AT. ( n -- ) {: a:n :}
   s" addr=" type a FMT:.INT
   s"  region-off=" type a BASE - FMT:.INT cr
   a AT 0= if s" no record owns it - a primitive in __text, or not in a routine" type cr then ;

: REGION-OWNER. ( n -- ) {: off:n :}
   BASE off + AT. ;

;package

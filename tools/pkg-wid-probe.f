\ pkg-wid-probe.f - which wordlist ids this engine's package records claim.
\
\ A package record carries DICT-WL's package marker in its wordlist cell and its
\ two wordlist ids in the cells an ordinary record uses for code start and length:
\ [0] is the public id, [8] the private one (0 when the package has none). Every
\ id belongs to exactly one package, so two records claiming one id means two
\ packages share a wordlist - which is what registering a captured wid into
\ another engine's wid space produces (dot habu-rebase-captured-wids-54dec421).

require src/habu/xref.f

package PKG-WID-PROBE

-1 constant PKG-MARK             \ the wordlist cell of a package record

variable HI

: PKG-REC? ( ptr a -- bool ) XREF-WORDLIST PKG-MARK = ;

\ Does this record claim `wid` as either of its two wordlists? A private cell of
\ 0 means the package has no private wordlist, so it claims nothing.
: CLAIMS? ( ptr a n -- bool ) {: r w:n :}   \ typed-local-lint: allow-bare-local - r keeps the ptr a record role
   r PKG-REC? 0= if 0 0= 0= exit then
   r XREF-START w = if 0 0= exit then
   w 0= if 0 0= 0= exit then
   r XREF-LEN w = ;

public

\ The public wordlist id the package record spelled `name` claims, or 0.
: WID-OF ( ptr u8 n -- n )
   {: a u:n :}   \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   0
   ndict@ 0 ?do
      i XREF-REC PKG-REC? if
         i XREF-REC a u XREF-MATCH? if
            drop i XREF-REC XREF-START
         then
      then
   loop ;

\ How many package records claim `wid`, in either role. One is the engine's own
\ answer for every id it handed out; two is a collision.
: OWNERS ( n -- n ) {: wid:n :}
   0
   ndict@ 0 ?do
      i XREF-REC wid CLAIMS? if 1+ then
   loop ;

\ The highest wordlist id any package record claims. Every id a record names must
\ already have been handed out, so an engine whose WIDN sits at or below this has
\ ids it will allocate a second time.
: HIGH ( -- n )
   0 HI !
   ndict@ 0 ?do
      i XREF-REC PKG-REC? if
         i XREF-REC XREF-START HI @ > if i XREF-REC XREF-START HI ! then
         i XREF-REC XREF-LEN   HI @ > if i XREF-REC XREF-LEN   HI ! then
      then
   loop
   HI @ ;

;package

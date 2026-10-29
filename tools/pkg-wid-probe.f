\ pkg-wid-probe.f - which wordlist ids this engine's package records claim.
\ A package record holds DICT-WL's package marker in its wordlist cell, its public
\ id at [0] and its private id at [8] (0 for none). Two records claiming one id is
\ a collision, as registering a captured wid into another engine's wid space makes.

require src/habu/xref.f

package PKG-WID-PROBE

-1 constant PKG-MARK             \ the wordlist cell of a package record

variable HI

: PKG-REC? ( ptr n -- bool ) XREF-WORDLIST PKG-MARK = ;

\ Does this record claim `wid` as either of its two wordlists? A private cell of
\ 0 means the package has no private wordlist, so it claims nothing.
: CLAIMS? ( ptr n n -- bool ) {: r w:n :}
   r PKG-REC? 0= if 0 0= 0= exit then
   r XREF-PKG-PUBLIC w = if 0 0= exit then
   w 0= if 0 0= 0= exit then
   r XREF-PKG-PRIVATE w = ;

public

\ The public wordlist id the package record spelled `name` claims, or 0.
: WID-OF ( ptr u8 n -- n )
   {: a u:n :}
   0
   ndict@ 0 ?do
      i XREF-REC PKG-REC? if
         i XREF-REC a u XREF-MATCH? if
            drop i XREF-REC XREF-PKG-PUBLIC
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
         i XREF-REC XREF-PKG-PUBLIC  HI @ > if i XREF-REC XREF-PKG-PUBLIC  HI ! then
         i XREF-REC XREF-PKG-PRIVATE HI @ > if i XREF-REC XREF-PKG-PRIVATE HI ! then
      then
   loop
   HI @ ;

;package

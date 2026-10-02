\ outer-find.f - OUTER:FIND (src/habu/outer.f) asked for every record of the
\ booted dictionary, against each wordlist asked in turn along the order the
\ engine's interpreter searches (habu1.f EMIT-FIND, habu2.f EMIT-FIND-USED).
\
\ The model. A spelling with no colon, or whose first colon is its first or last
\ byte, is bare; one with a single inner colon is NAME:tail; one with more
\ misses. A bare spelling is asked of the open package's private, public and
\ global wordlists in turn (with no package open, of the global one alone);
\ when all of those answer 0 and it has no colon, of every used public, where
\ two distinct records are ambiguous. NAME:tail is asked of the public wid the
\ namespace row NAME carries, then of the global wordlist when that wid is the
\ open package's own public one.
\
\ The comparison. At every wordlist of the open chain or of NAME:tail that FIND
\ passed, search-wl must answer 0. At the one holding FIND's record it must
\ answer that record's xt, unless it hides the record (DNAME-INT, the
\ engine-helper wordlist, no start): then it answers 0, and the record FIND
\ returned must carry the spelling's name. The used publics are asked through
\ XREF-FIND-WL (xref.f), a linear walk of the records apart from FIND's hash
\ index, and compared by record: one body EXPORTed under a second record is two
\ records with one xt, which the engine counts as two. A miss must be a miss
\ everywhere.
\
\ What FIND can get wrong, and what sees it:
\ - the order: every record asked by its own name in its own scope (SELF), a
\   private and a public tail shadowing globals, a global shadowing a used tail;
\ - the probe: search-wl hides the DNAME-INT rows FIND must return;
\ - the used publics: two packages exporting one tail, one body EXPORTed under
\   a second record, one package used twice, a colon-bearing tail;
\ - NAME:tail: every public record spelled so, NAME:a:b, the open package's
\   fall-through to the global wordlist, a leading or trailing colon;
\ - retired and namespace rows: never answered;
\ - case: every spelling with a letter, its case flipped, gets the same answer.
\ The walk runs at top level, under three usings, under an EXPORT twin's two
\ usings, and inside a package.

require lib/test.f
require lib/string.f
require lib/fmt.f
require src/core/bytes.f
require src/habu/layout.f
require src/habu/xref.f
require src/compiler/native/dict.f
require src/habu/outer.f

\ ---- fixtures -----------------------------------------------------------------
package OUTER-FIND-FXA
public
: OFX-TWIN ( -- n ) 1 ;           \ FXB exports it too: ambiguous under both
: OFX-SHADOW ( -- n ) 2 ;         \ a global too, which answers first
: OFX-USED ( -- n ) 3 ;           \ only here: the using answers
: OFX-USED: ( -- n ) 4 ;          \ a colon: never a used tail
;package

package OUTER-FIND-FXB
public
: OFX-TWIN ( -- n ) 5 ;
;package

package OUTER-FIND-FXC
public
EXPORT OUTER-FIND-FXA:OFX-USED    \ FXA's body under a second record
;package

: OFX-SHADOW ( -- n ) 6 ;
: OFX-GLOBAL ( -- n ) 7 ;         \ no package exports it
: OFX-PRIVATE ( -- n ) 8 ;        \ OUTER-FIND-TEST's private tail shadows it
: OFX-PUBLIC ( -- n ) 9 ;         \ OUTER-FIND-TEST's public tail shadows it
: :OFX-LEAD:COLON ( -- n ) 10 ;   \ a leading colon: bare, whatever follows
: OFX-TRAIL: ( -- n ) 11 ;        \ a trailing colon: bare
: OFX-GONE ( -- n ) 12 ;
undefine OFX-GONE                 \ retired, and nothing answers it
: OFX-AGAIN ( -- n ) 13 ;
undefine OFX-AGAIN
: OFX-AGAIN ( -- n ) 14 ;         \ retired, and a newer record answers it

variable OFX-GLOBAL-XT
' OFX-SHADOW OFX-GLOBAL-XT !

package OUTER-FIND-TEST

: OFX-PRIVATE ( -- n ) 15 ;

public

: OFX-PUBLIC ( -- n ) 16 ;

: ASK-TWIN ( -- )
   s" OFX-TWIN" OUTER:FIND drop ;

: ASK-USED ( -- )
   s" OFX-USED" OUTER:FIND drop ;

private

\ ---- counts ---------------------------------------------------------------------
0 constant #RECORDS
1 constant #SPELLINGS
2 constant #SELF
3 constant #INTERNAL
4 constant #RETIRED
5 constant #NAMESPACE
6 constant #QUALIFIED
7 constant #FOLDED
8 constant #USED
9 constant #AMBIGUOUS
10 constant #COUNTS

#COUNTS TYPED-BUFFER COUNTS n

: COUNT@ ( n -- n )
   COUNTS @ ;

: COUNT+ ( n -- ) {: k:n :}
   k COUNT@ 1+ k COUNTS ! ;

: COUNTS-RESET ( -- )
   #COUNTS 0 ?do 0 i COUNTS ! loop ;

: COUNT. ( ptr u8 n n -- ) {: a:ptr u:n k:n :}
   space a u type space k COUNT@ FMT:.INT ;

\ ---- the spelling asked, and FIND's answer -------------------------------------
TYPED-VARIABLE SP-A ptr u8
variable SP-U
TYPED-VARIABLE GOT ptr n
variable RC

: SPELLING ( -- ptr u8 n )
   SP-A @ SP-U @ ;

: ASK-GO ( -- )
   SPELLING OUTER:FIND GOT ! ;

\ FIND's record lands in GOT, or its throw code in RC with GOT null.
: ASK ( ptr u8 n -- )
   SP-U ! SP-A !
   XREF-NULL GOT !
   [: ASK-GO ;] catch RC ! ;

: EXPECT ( bool ptr u8 n -- ) {: ok:bool why:ptr wu:n :}
   T-NEXT
   ok 0= if
      SPELLING T-LABEL  why wu T-ASSERT-DETAIL  T-LABEL-CLEAR
   then ;

\ FIND's answer is a miss.
: NOTHING ( -- )
   RC @ 0=  GOT @ XREF-FOUND? 0= and
   s" FIND answered what search-wl misses" EXPECT ;

\ ---- the model ------------------------------------------------------------------
-1 constant SHAPE-BARE
-2 constant SHAPE-BAD

: FIRST-COLON ( ptr u8 n -- n )
   $3A INDEX-OF MATCH option
      none OF -1 ENDOF
      some OF IDX>N ENDOF
   ;MATCH ;

: COLON? ( ptr u8 n -- bool )
   $3A COUNT-CHAR 0<> ;

\ The qualifier's index, SHAPE-BARE or SHAPE-BAD.
: SHAPE ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u FIRST-COLON {: first:n :}
   first 1 < if SHAPE-BARE exit then
   first u 1- = if SHAPE-BARE exit then
   a u $3A COUNT-CHAR 1 > if SHAPE-BAD exit then
   first ;

4 TYPED-BUFFER TIERS n
variable #TIERS

: TIER ( n -- n )
   TIERS @ ;

: TIER+ ( n -- )
   #TIERS @ TIERS !  #TIERS @ 1+ #TIERS ! ;

: OPEN-TIERS ( -- )
   0 #TIERS !
   NDICT:OPEN-PRI 0= if 0 TIER+ exit then
   NDICT:OPEN-PRI TIER+  NDICT:OPEN-PUB TIER+  0 TIER+ ;

: QUAL-TIERS ( n -- ) {: pub:n :}
   0 #TIERS !  pub TIER+
   NDICT:OPEN-PRI 0<> pub NDICT:OPEN-PUB = and if 0 TIER+ then ;

\ The tier that holds a wid, or -1.
: TIER-OF ( n -- n ) {: wid:n :}
   #TIERS @ 0 ?do
      i TIER wid = if i unloop exit then
   loop
   -1 ;

\ search-wl answers 0 for these records inside their own wordlist.
: HIDDEN? ( ptr n -- bool ) {: rec:ptr :}
   rec XREF-FLAGS DNAME-INT and 0<>
   rec XREF-WORDLIST OWNER-API-PRI-WID = or
   rec XREF-START 0= or ;

\ One tier of the chain; true when FIND's record is in it.
: AT-TIER ( ptr u8 n n -- bool ) {: a:ptr u:n wid:n :}
   a u wid search-wl {: x:n :}
   GOT @ {: got:ptr :}
   got XREF-FOUND? if
      got XREF-WORDLIST wid = if
         got HIDDEN? if
            x 0= s" search-wl answered a record it hides" EXPECT
            got XREF-FLAGS DNAME-INT and 0<> if #INTERNAL COUNT+ then
         else
            x got XREF-START = s" FIND answered another record than search-wl" EXPECT
         then
         got XREF-NAME$ a u XREF-STR=CI s" FIND answered another name" EXPECT
         true exit
      then
   then
   x 0= s" FIND passed a wordlist holding the name" EXPECT
   false ;

: TIERS-AGREE ( ptr u8 n -- bool ) {: a:ptr u:n :}
   #TIERS @ 0 ?do
      a u i TIER AT-TIER if true unloop exit then
   loop
   false ;

: USE-DEPTH ( -- n )
   data-base USE-DEPTH-CELL + @ ;

: USE-WID ( n -- n )
   cells data-base USE-WIDS-OFF + + @ ;

\ Fold one used public's record into (first record, distinct records up to 2).
: MERGE ( ptr n n ptr n -- ptr n n ) {: first:ptr count:n rec:ptr :}
   rec XREF-FOUND? 0= if first count exit then
   count 0= if rec 1 exit then
   rec first = if first count exit then
   first 2 ;

\ By record, not xt: an EXPORT twin is two records with one xt.
: USED-ANSWER ( ptr u8 n -- ptr n n ) {: a:ptr u:n :}
   XREF-NULL 0
   USE-DEPTH 0 ?do
      a u i USE-WID XREF-FIND-WL MERGE
   loop ;

: USED-AGREE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u USED-ANSWER {: first:ptr count:n :}
   RC @ E-USING-AMBIGUOUS = if
      count 2 = s" FIND refused a tail the used publics agree on" EXPECT
      #AMBIGUOUS COUNT+ exit
   then
   RC @ 0= s" FIND threw" EXPECT
   GOT @ {: got:ptr :}
   got XREF-FOUND? 0= if
      count 0= s" FIND missed a used tail" EXPECT exit
   then
   count 1 = s" FIND answered a tail the used publics disagree on" EXPECT
   first got = s" FIND answered another used record" EXPECT
   #USED COUNT+ ;

: BARE-AGREE ( ptr u8 n -- ) {: a:ptr u:n :}
   OPEN-TIERS
   a u TIERS-AGREE if exit then
   a u COLON? if NOTHING exit then
   a u USED-AGREE ;

: QUAL-AGREE ( ptr u8 n n -- ) {: a:ptr u:n q:n :}
   a q XREF-NAMESPACE-WL search-wl {: pub:n :}
   pub 0= if NOTHING exit then
   pub QUAL-TIERS
   a q 1+ + u q - 1- TIERS-AGREE if exit then
   NOTHING ;

\ FIND's answer for one spelling against the model's.
: CHECK ( ptr u8 n -- ) {: a:ptr u:n :}
   a u ASK
   #SPELLINGS COUNT+
   a u SHAPE {: q:n :}
   q SHAPE-BAD = if NOTHING exit then
   q SHAPE-BARE = if a u BARE-AGREE exit then
   a u q QUAL-AGREE ;

\ ---- a record's classes ---------------------------------------------------------
\ A record in the open chain answers its own name, unless a wordlist earlier
\ in the chain holds the name too.
: SELF ( ptr n -- ) {: rec:ptr :}
   rec XREF-NAME$ SHAPE SHAPE-BARE <> if exit then
   OPEN-TIERS
   rec XREF-WORDLIST TIER-OF {: k:n :}
   k 0 < if exit then
   GOT @ {: got:ptr :}
   got XREF-FOUND? 0= if false s" FIND missed a record of the open chain" EXPECT exit then
   got XREF-WORDLIST TIER-OF {: j:n :}
   j 0 >= j k <= and s" FIND answered past the record's own wordlist" EXPECT
   j k = if
      got rec = s" FIND answered another record of the same wordlist" EXPECT
      #SELF COUNT+
   then ;

: NOT-ANSWERED ( ptr n -- ) {: rec:ptr :}
   GOT @ rec <> s" FIND answered a retired or namespace row" EXPECT ;

: CLASSES ( ptr n -- ) {: rec:ptr :}
   rec XREF-WORDLIST XREF-RETIRED-WL = if rec NOT-ANSWERED  #RETIRED COUNT+ then
   rec XREF-WORDLIST XREF-NAMESPACE-WL = if rec NOT-ANSWERED  #NAMESPACE COUNT+ then ;

256 constant SPELL-CAP
SPELL-CAP BUFFER: FLIPPED
SPELL-CAP BUFFER: QUALIFIED

: FLIP ( n -- n ) {: c:n :}
   c $41 >= c $5A <= and if c $20 or exit then
   c $61 >= c $7A <= and if c $20 xor exit then
   c ;

\ The spelling FIND just answered, its letters' case flipped, gets that answer.
: FOLDED ( ptr u8 n -- ) {: a:ptr u:n :}
   u SPELL-CAP > if false s" spelling too long" EXPECT exit then
   GOT @ RC @ {: got:ptr rc:n :}
   0 u 0 ?do
      a i + c@ dup FLIP dup FLIPPED i + c!
      <> if 1+ then
   loop
   0= if exit then
   FLIPPED u ASK
   RC @ rc =  GOT @ got = and s" the other case got another answer" EXPECT
   #FOLDED COUNT+ ;

\ Every record of NS's public wordlist, spelled NS:tail, answers that record.
: QUALIFIED-ONE ( ptr n ptr n -- ) {: ns:ptr rec:ptr :}
   ns XREF-NAME$ {: na:ptr nu:n :}
   rec XREF-NAME$ {: ra:ptr ru:n :}
   nu 1+ ru + {: qu:n :}
   qu SPELL-CAP > if false s" spelling too long" EXPECT exit then
   na QUALIFIED nu BYTE-COPY
   $3A QUALIFIED nu + c!
   ra QUALIFIED nu 1+ + ru BYTE-COPY
   QUALIFIED qu CHECK
   ra ru COLON? if exit then
   GOT @ rec = s" NS:tail did not answer the tail's record" EXPECT
   #QUALIFIED COUNT+
   QUALIFIED qu FOLDED ;

: QUALIFIED-WALK ( ptr n -- ) {: ns:ptr :}
   ns XREF-PKG-PUBLIC {: pub:n :}
   ndict@ 0 ?do
      i XREF-REC XREF-WORDLIST pub = if ns i XREF-REC QUALIFIED-ONE then
   loop ;

: RECORD ( n -- )
   XREF-REC {: rec:ptr :}
   #RECORDS COUNT+
   rec XREF-NAME$ CHECK
   rec SELF
   rec CLASSES
   rec XREF-NAME$ FOLDED
   rec XREF-WORDLIST XREF-NAMESPACE-WL = if rec QUALIFIED-WALK then ;

: REPORT ( ptr u8 n -- ) {: a:ptr u:n :}
   s" outer-find: " type a u type cr
   s" records" #RECORDS COUNT.
   s" spellings" #SPELLINGS COUNT.
   s" self" #SELF COUNT.
   s" internal" #INTERNAL COUNT.
   s" retired" #RETIRED COUNT.
   s" namespace" #NAMESPACE COUNT.
   s" qualified" #QUALIFIED COUNT.
   s" folded" #FOLDED COUNT.
   s" used" #USED COUNT.
   s" ambiguous" #AMBIGUOUS COUNT. cr ;

public

\ Every record, in the scope open when this runs. Each class every scope holds
\ must have been met.
: WALK ( ptr u8 n -- ) {: a:ptr u:n :}
   COUNTS-RESET
   ndict@ 0 ?do i RECORD loop
   a u REPORT
   a u T-LABEL #SELF COUNT@ 0 > TTRUE
   a u T-LABEL #INTERNAL COUNT@ 0 > TTRUE
   a u T-LABEL #RETIRED COUNT@ 0 > TTRUE
   a u T-LABEL #NAMESPACE COUNT@ 0 > TTRUE
   a u T-LABEL #QUALIFIED COUNT@ 0 > TTRUE
   a u T-LABEL #FOLDED COUNT@ 0 > TTRUE ;

\ FIND's xt for a spelling, 0 for a miss; a throw is a failed case.
: XT-OF ( ptr u8 n -- n )
   ASK
   RC @ 0= s" FIND threw" EXPECT
   GOT @ XREF-FOUND? if GOT @ XREF-START exit then
   0 ;

: USED# ( -- n )
   #USED COUNT@ ;

: AMBIGUOUS# ( -- n )
   #AMBIGUOUS COUNT@ ;

;package

T-RESET

\ ---- pinned cases: FIND against the engine itself (tick, or the token run) ----
\ A leading or trailing colon leaves the token bare.
s" :OFX-LEAD:COLON" OUTER-FIND-TEST:XT-OF  ' :OFX-LEAD:COLON  T=
:OFX-LEAD:COLON 10 T=
s" OFX-TRAIL:" OUTER-FIND-TEST:XT-OF  ' OFX-TRAIL:  T=
\ NAME:tail reaches NAME's public tail; a second colon misses.
s" OUTER-FIND-FXA:OFX-USED" OUTER-FIND-TEST:XT-OF  ' OUTER-FIND-FXA:OFX-USED  T=
s" OUTER-FIND-FXA:OFX-USED:X" OUTER-FIND-TEST:XT-OF 0 T=
\ Retired rows answer nothing; a newer record answers the name.
s" OFX-GONE" OUTER-FIND-TEST:XT-OF 0 T=
s" OFX-AGAIN" OUTER-FIND-TEST:XT-OF  ' OFX-AGAIN  T=
OFX-AGAIN 14 T=
\ Outside the package its own qualified name does not fall through.
s" OUTER-FIND-TEST:OFX-GLOBAL" OUTER-FIND-TEST:XT-OF 0 T=

using OUTER-FIND-FXA
\ The global wordlist answers before the used publics.
s" OFX-SHADOW" OUTER-FIND-TEST:XT-OF  OFX-GLOBAL-XT @  T=
s" OFX-USED" OUTER-FIND-TEST:XT-OF  ' OFX-USED  T=
\ A colon-bearing token never reaches the used publics.
s" OFX-USED:" OUTER-FIND-TEST:XT-OF 0 T=
using OUTER-FIND-FXA
\ One package used twice is one record.
s" OFX-USED" OUTER-FIND-TEST:XT-OF  ' OFX-USED  T=
using OUTER-FIND-FXB
\ Two packages exporting one tail: E-USING-AMBIGUOUS, which the engine dies on.
' OUTER-FIND-TEST:ASK-TWIN E-USING-AMBIGUOUS TTHROWS
s" three usings" OUTER-FIND-TEST:WALK
OUTER-FIND-TEST:USED# 0 > TTRUE
OUTER-FIND-TEST:AMBIGUOUS# 0 > TTRUE
;using
;using
;using

using OUTER-FIND-FXA
using OUTER-FIND-FXC
\ One body under two records is two records: ambiguous, as the engine counts.
' OUTER-FIND-FXC:OFX-USED  ' OUTER-FIND-FXA:OFX-USED  T=
' OUTER-FIND-TEST:ASK-USED E-USING-AMBIGUOUS TTHROWS
\ FXC holds nothing but the second record, so every ambiguity here is that one.
s" an EXPORT twin" OUTER-FIND-TEST:WALK
OUTER-FIND-TEST:USED# 0 > TTRUE
OUTER-FIND-TEST:AMBIGUOUS# 0 > TTRUE
;using
;using

s" top level" OUTER-FIND-TEST:WALK
OUTER-FIND-TEST:USED# 0 T=
OUTER-FIND-TEST:AMBIGUOUS# 0 T=

package OUTER-FIND-TEST
\ The private tail, then the public one, answers before the global.
s" OFX-PRIVATE" XT-OF  ' OFX-PRIVATE  T=
OFX-PRIVATE 15 T=
s" OFX-PUBLIC" XT-OF  ' OFX-PUBLIC  T=
OFX-PUBLIC 16 T=
\ The open package's own NAME:tail falls through to a global tail it lacks.
s" OUTER-FIND-TEST:OFX-GLOBAL" XT-OF  ' OFX-GLOBAL  T=
OUTER-FIND-TEST:OFX-GLOBAL 7 T=
using OUTER-FIND-FXA
s" inside a package" WALK
USED# 0 > TTRUE
AMBIGUOUS# 0 T=
;using
;package

s" outer-find: cases " type T-CASES FMT:.INT cr
T-REPORT

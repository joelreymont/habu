\ c2-reopen-refusals.f - the checker rules that bind code written inside C2-MEM.
\
\ Run: the WHITEBOX-SUITE row c2-reopen-refusals (test/gate-stdlib-cases.f),
\ which hands it the unsealed engine (test/whitebox-engine.f).
\
\ Every product bakes C2-MEM (src/habu/native-runtime.f requires
\ lib/c2-owner.f), and the native build seals every package it bakes
\ (src/core/internal-mark.f SEAL-PACKAGES). So on a product `package C2-MEM`
\ exits 84 with the package name before the checker sees a body;
\ test/c2-memory-refusals.f and test/c2-owner-producer-refusals.f pin that on
\ the admitted C2 image. A reopened C2-MEM is the owner: its private words are
\ its own, and package privacy refuses none of them inside it. The whitebox
\ image keeps engine packages open, so the rules that still bind C2-MEM's own
\ code run there, each case in a disposable fork of the whitebox engine
\ (lib/test/subject.f). The image class is asserted first, because on a sealed
\ engine every case exits 84. The STOW-WITH-LOOP compile below is a probe of
\ the native loop rewrite, and SECTION-VIEWS runs the view representation casts
\ that only C2-MEM's private section may declare. SECTION-OWNER calls the
\ generated private owner constructor from a reopened C2-MEM.

require lib/test.f
require lib/test/subject.f
require lib/c2-owner.f
require src/compiler/native/compiler.f

\ Rewriting a foldable loop must also copy the later C2 transfer's kind.
\ This uses the owner's real carrier and primitive; compilation is the probe.
1 set-tier
package C2-MEM
private
TRUSTED: STOW-WITH-LOOP ( R ptr u8 n n [ R ptr u8 n -- S ptr u8 n | U -- U ] stow-layout | U -- R ptr u8 n [ R ptr u8 n -- S ptr u8 n | U -- U ] | U )
   0 10 0 ?do 1 + loop drop
   HEAD-FRAME c2-init-stow
   TASK:DEFER-LEAVE ;
;package
0 set-tier

package C2-REOPEN-REFUSALS

$4000 constant CAP
create OUT CAP allot
create ERR CAP allot

: STATUS? ( ptr u8 n n -- bool ) {: expected:n :}
   OUT CAP >LEN ERR CAP >LEN 10000 >MS SUBJECT:RUN
   MATCH outcome
      exited OF expected = ENDOF
      signaled OF drop false ENDOF
      timeout OF false ENDOF
   ;MATCH
   >r 2drop r> ;

: SECTION-CLASS ( -- )
   s" the engine under test keeps engine packages open" T-LABEL
   ENGINE-INTERNAL:IMAGE-CLASS ENGINE-INTERNAL:IMAGE-WHITEBOX T= ;

\ The view representation casts are C2-MEM's (src/core/checker.f
\ VIEW-CAST-CERTIFY): its private section packs ( ptr u8 n -- V ) and unpacks a
\ mutable view, and a checked word that unpacks and packs again binds the
\ pack's fresh scope and region variables to its own declared row. Its public
\ section refuses the pack with E-CAST-SCOPE, which no catch takes here: exit
\ 67 naming the code. A `:` still cannot declare raw cells a view. A qualified
\ name selects another package's public wordlist, here this one's, so the
\ private section's pack is E-CAST-SCOPE under one too. That refusal is caught
\ in this engine, which the throw leaves outside C2-MEM, and the wordlist the
\ name selects is shown to hold no such name after it.
67 constant UNCAUGHT-RC          \ hb's exit status for an uncaught throw
public get-current private constant QUAL-WID

: SCOPE-REFUSED? ( ptr u8 n -- bool )
   OUT CAP >LEN ERR CAP >LEN 10000 >MS SUBJECT:RUN
   MATCH outcome
      exited OF UNCAUGHT-RC = ENDOF
      signaled OF drop false ENDOF
      timeout OF false ENDOF
   ;MATCH
   {: outu:len erru:len refused:bool :}
   refused outu LEN>N 0= and
   ERR erru LEN>N s" uncaught throw code 7151" CONTAINS? and ;

: SECTION-VIEWS ( -- )
   s" C2-MEM's private section packs raw cells into a view" T-LABEL
   s" package C2-MEM private CAST: C2RR-PACK ( ptr u8 n -- read-view<p,q,u8> ) ;package" 0 STATUS? TTRUE
   s" a checked C2-MEM word unpacks a mutable view and packs it again" T-LABEL
   s" package C2-MEM private CAST: C2RR-MUN ( mut-view<p,q,a,u8> -- ptr u8 n ) CAST: C2RR-MPK ( ptr u8 n -- mut-view<p,q,a,u8> ) : C2RR-ROUND ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> ) C2RR-MUN C2RR-MPK ; ;package" 0 STATUS? TTRUE
   s" C2-MEM's public section cannot pack a view" T-LABEL
   s" package C2-MEM public CAST: C2RR-PUBLIC-PACK ( ptr u8 n -- read-view<p,q,u8> ) ;package" SCOPE-REFUSED? TTRUE
   s" a reopened C2-MEM definition cannot declare raw cells a view" T-LABEL
   s" package C2-MEM private : C2RR-RAW-VIEW ( ptr u8 n -- read-view<p,q,u8> ) ; ;package" 70 STATUS? TTRUE
   s" C2-MEM's private section cannot pack under another package's name" T-LABEL
   s" package C2-MEM private CAST: C2-REOPEN-REFUSALS:QUAL-PACK ( ptr u8 n -- read-view<p,q,u8> ) ;package" TEST-EVAL:RC E-CAST-SCOPE T=
   s" QUAL-PACK" QUAL-WID search-wl 0= TTRUE ;

: SECTION-OWNER ( -- )
   s" a reopened C2-MEM calls the generated owner constructor" T-LABEL
   s" package C2-MEM private : C2RR-CTOR ( mut-view<p,i,a,init<i,owner-state>> -- owner<p,i,a> ) OWNER-MAKE ; ;package" 0 STATUS? TTRUE ;

public

: RUN ( -- )
   T-RESET
   SECTION-CLASS
   SECTION-VIEWS
   SECTION-OWNER
   T-REPORT ;

;package

C2-REOPEN-REFUSALS:RUN

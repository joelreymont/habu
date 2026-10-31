\ c2-reopen-refusals.f - the checker rules that bind code written inside a
\ reopened C2-MEM. Run as the WHITEBOX-SUITE row c2-reopen-refusals
\ (test/gate-stdlib-cases.f) on the unsealed engine (test/whitebox-engine.f):
\ a product seals C2-MEM, so `package C2-MEM` exits 84 there
\ (test/c2-memory-refusals.f). The image class is asserted first; each case
\ runs in a disposable fork (src/lib/test/subject.f).

require src/lib/test.f
require src/lib/test/subject.f
require src/lib/c2-owner.f
require src/compiler/native/compiler.f

\ Rewriting a foldable loop must also copy the later C2 transfer's kind; this
\ uses the owner's real carrier and primitive, and compilation is the probe.
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

: SECTION-CLASS ( -- )
   s" the engine under test keeps engine packages open" T-LABEL
   ENGINE-INTERNAL:IMAGE-CLASS ENGINE-INTERNAL:IMAGE-WHITEBOX T= ;

\ The view casts are src/core/checker.f VIEW-CAST-CERTIFY's. The public
\ section's refusal is uncaught (exit 67). The qualified-name refusal is caught
\ in this engine, which the throw leaves outside C2-MEM, and QUAL-WID is shown
\ to hold no such name.
67 constant UNCAUGHT-RC
public get-current private constant QUAL-WID

: SECTION-VIEWS ( -- )
   s" C2-MEM's private section packs raw cells into a view" T-LABEL
   s" package C2-MEM private CAST: C2RR-PACK ( ptr u8 n -- read-view<p,q,u8> ) ;package" 0 SUBJECT:EXITED? TTRUE
   s" a checked C2-MEM word unpacks a mutable view and packs it again" T-LABEL
   s" package C2-MEM private CAST: C2RR-MUN ( mut-view<p,q,a,u8> -- ptr u8 n ) CAST: C2RR-MPK ( ptr u8 n -- mut-view<p,q,a,u8> ) : C2RR-ROUND ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> ) C2RR-MUN C2RR-MPK ; ;package" 0 SUBJECT:EXITED? TTRUE
   s" C2-MEM's private section retypes a view callback as a callback on its cells" T-LABEL
   s" package C2-MEM private CAST: C2RR-CB-PACK ( [ mut-view<p,q,a,u8> -- ] -- [ ptr u8 n -- ] ) : C2RR-CB-USE ( [ mut-view<p,p,fresh-region-b,u8> -- ] -- [ ptr u8 n -- ] ) C2RR-CB-PACK ; ;package" 0 SUBJECT:EXITED? TTRUE
   s" a callback on raw cells cannot become a view callback" T-LABEL
   s" package C2-MEM private CAST: C2RR-CB-UNPACK ( [ ptr u8 n -- ] -- [ mut-view<p,q,a,u8> -- ] ) ;package"
   UNCAUGHT-RC s" uncaught throw code 7151" SUBJECT:REFUSED? TTRUE
   s" a callback pack refuses a callback over a named tail" T-LABEL
   s" package C2-MEM private CAST: C2RR-CB-NAMED ( [ R mut-view<p,q,a,u8> -- R ] -- [ S ptr u8 n -- S ] ) ;package"
   UNCAUGHT-RC s" uncaught throw code 7151" SUBJECT:REFUSED? TTRUE
   s" a callback pack refuses a callback that leaves a result" T-LABEL
   s" package C2-MEM private CAST: C2RR-CB-RESULT ( [ mut-view<p,q,a,u8> -- n ] -- [ ptr u8 n -- n ] ) ;package"
   UNCAUGHT-RC s" uncaught throw code 7151" SUBJECT:REFUSED? TTRUE
   s" C2-MEM's public section cannot pack a view" T-LABEL
   s" package C2-MEM public CAST: C2RR-PUBLIC-PACK ( ptr u8 n -- read-view<p,q,u8> ) ;package"
   UNCAUGHT-RC s" uncaught throw code 7151" SUBJECT:REFUSED? TTRUE
   s" a reopened C2-MEM definition cannot declare raw cells a view" T-LABEL
   s" package C2-MEM private : C2RR-RAW-VIEW ( ptr u8 n -- read-view<p,q,u8> ) ; ;package" 70 SUBJECT:EXITED? TTRUE
   s" C2-MEM's private section cannot pack under another package's name" T-LABEL
   s" package C2-MEM private CAST: C2-REOPEN-REFUSALS:QUAL-PACK ( ptr u8 n -- read-view<p,q,u8> ) ;package" TEST-EVAL:RC E-CAST-SCOPE T=
   s" QUAL-PACK" QUAL-WID search-wl 0= TTRUE ;

: SECTION-OWNER ( -- )
   s" a reopened C2-MEM calls the generated owner constructor" T-LABEL
   s" package C2-MEM private : C2RR-CTOR ( mut-view<p,i,a,init<i,owner-state>> -- owner<p,i,a> ) OWNER-MAKE ; ;package" 0 SUBJECT:EXITED? TTRUE ;

public

: RUN ( -- )
   T-RESET
   SECTION-CLASS
   SECTION-VIEWS
   SECTION-OWNER
   T-REPORT ;

;package

C2-REOPEN-REFUSALS:RUN

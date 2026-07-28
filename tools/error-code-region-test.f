\ error-code-region-test.f - checked fixtures for the compiler growth region.
\ Run: bin/hb --load tools/error-code-region-test.f
\
\ The compiler's first error block, -6600..-6699, is full: ten owners spent all
\ 100 codes and the subsystem still has ten substrate stages, eight dialects,
\ and two back ends to build. lib/errors.f therefore reserves a second, larger
\ region, E-COMP-FIRST..E-COMP-LAST, and tools/error-code-lint.f is what keeps
\ it exclusive. These fixtures prove three things about that region: the owning
\ file can mint anywhere inside it, every other file is refused inside it, and
\ the live ledger really does carry the reservation with room left in it.
\
\ Every fixture builds its source text from the two live bound constants rather
\ than from copied digits. The lint scans this file along with the rest of the
\ tree, so a literal `-8xxx constant E-NAME` written here would be
\ indistinguishable from a genuine claim inside the very region under test.
\ Composing the text also means the fixtures follow the bounds if they ever
\ move, instead of silently testing a range nobody uses any more.
\
\ Load after lib/test.f and tools/error-code-lint-core.f.

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/test.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/intern.f
require tools/lint/token.f
require tools/error-code-lint-core.f

package ECL-REGION-TEST
private

128 constant SRC-CAP           \ one composed source is held while the next is built

create SRC SRC-CAP allot
variable SRC#

: KEEP ( ptr u8 n -- ) {: a:ptr u:n :}
   u SRC-CAP > if E-TEST-CAPACITY throw then
   a SRC u LINT-BMOVE  u SRC# ! ;

: KEPT$ ( -- ptr u8 n )  SRC SRC# @ ;

: BOUNDS$ ( -- ptr u8 n )   \ the owning file's two range sentinels
   SB-RESET
   E-COMP-FIRST FMT:SB-INT  s"  constant E-COMP-FIRST  " SB-APPEND
   E-COMP-LAST  FMT:SB-INT  s"  constant E-COMP-LAST" SB-APPEND
   SB$ ;

: MEMBER+ ( n -- ) {: code:n :}   \ append one member claim to the open builder
   code FMT:SB-INT  s"  constant E-CMEMBER" SB-APPEND ;

: MEMBER$ ( n -- ptr u8 n ) {: code:n :}   \ one member claim, alone
   SB-RESET  code MEMBER+  SB$ ;

: OWNED ( n -- n ) {: code:n :}   \ findings when the owning file mints the code
   BOUNDS$ KEEP
   SB-RESET
   KEPT$ SB-APPEND  s"   " SB-APPEND  code MEMBER+
   SB$ ECL-COUNT ;

: FOREIGN ( n -- n ) {: code:n :}   \ findings when another file mints the code
   BOUNDS$ KEEP
   KEPT$ code MEMBER$ ECL-COUNT2 ;

: MID ( -- n )    E-COMP-FIRST E-COMP-LAST + 2 / ;

: WIDTH ( -- n )  E-COMP-FIRST E-COMP-LAST - 1+ ;   \ codes the region holds

: ROWS ( -- n )   \ live reservations whose two bounds are the region's bounds
   0  0 begin dup ECL-RES# @ < while
      dup ECL-RES-FIRST@ E-COMP-FIRST =
      over ECL-RES-LAST@ E-COMP-LAST = and if swap 1+ swap then
      1+
   repeat drop ;

: CLAIMS ( -- n )   \ live E- codes claimed inside the region
   0  0 begin dup ECL-CLAIM# @ < while
      dup ECL-CODE@ E-COMP-FIRST E-COMP-LAST ECL-INRANGE? if swap 1+ swap then
      1+
   repeat drop ;

: USABLE ( -- )
   \ the owning file may mint at either bound and anywhere between them
   E-COMP-FIRST OWNED 0 T=
   MID OWNED 0 T=
   E-COMP-LAST OWNED 0 T= ;

: RESERVED ( -- )
   \ the whole region is exclusive from the moment its two bounds exist, before
   \ any member code inside it has been minted
   E-COMP-FIRST FOREIGN 1 T=
   MID FOREIGN 1 T=
   E-COMP-LAST FOREIGN 1 T=
   \ and it reserves nothing past its bounds: both neighbours stay free
   E-COMP-FIRST 1+ FOREIGN 0 T=
   E-COMP-LAST 1- FOREIGN 0 T= ;

: LIVE ( -- )
   \ the real ledger reserves the region exactly once, and the region still has
   \ free codes for the stages that have not been built yet. Running out of
   \ codes in the middle of the subsystem is what forced this second region to
   \ exist, so this fails while there is still room to react rather than after
   \ the next stage has nowhere to mint.
   ECL-REPORT? @ {: report:bool :}
   ECL-REPORT-OFF  ECL-RUN  report ECL-REPORT!
   ROWS 1 T=
   CLAIMS WIDTH < TTRUE ;

: MAIN ( -- )
   T-RESET
   USABLE
   RESERVED
   LIVE
   T-REPORT ;

MAIN

;package

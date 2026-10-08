\ engine-writers-prepare.f - the checked owner words test/engine-writers-child.f
\ reaches the engine's writers through, the pre-seal sources its libraries need,
\ then the production seal.
\
\ Loaded in test/native-window-owner-child.f's window once the include words are
\ live, ahead of test/engine-writers-child.f. A checked caller reaches each
\ writer only inside the package whose private row types it (src/habu/prims.f):
\ OUTER for the definition writers and xref-search-wl, CHECKER-OVERLAY for the
\ replay writers. Each caller is a checked `:` in the public section of a
\ package named for its owner, which the child imports: CHECKER-OVERLAY reopens
\ the window's record; OUTER is fresh, since the window holds no OUTER record and
\ the rows bind by owner name. The product refuses `package CHECKER-OVERLAY`
\ (exit 84) and holds no OUTER namespace record, so `package OUTER` opens a
\ fresh package there too; nothing here relies on either. The words come
\ before the seal, as test/prim-owner-scope-prepare.f's owner case does:
\ src/core/internal-mark.f classifies every record the window holds once, these
\ words' included, and the child judges each writer as that pass leaves it.
\ trust-sig! has no OUTER row, so the child keeps its own caller for it.

package OUTER
public
: EW-RECORD ( ptr u8 n n -- ptr n ) xref-search-wl ;
: EW-NS ( ptr u8 n bool -- n ) namespace-record ;
: EW-PRIVATE ( n -- ) namespace-private ;
: EW-ALIAS ( ptr u8 n n n -- ) alias-record ;
: EW-SCOPE ( n n -- ) package-scope! ;
: EW-OPEN ( ptr u8 n n n -- ) def-open ;
: EW-APPEND ( ptr u8 n -- ) body-append ;
: EW-CSIG ( ptr u8 n -- ) created-sig! ;
: EW-CLOSE ( -- ) def-close ;
;package

package CHECKER-OVERLAY
public
: EWR-OPEN ( -- ) replay-open ;
: EWR-CLOSE ( -- ) replay-close ;
: EWR-WIDN ( n -- ) replay-widn! ;
: EWR-REC ( ptr u8 n n -- ) replay-record ;
: EWR-WID ( n n -- ) record-wid! ;
: EWR-PRI ( n bool -- ) replay-private ;
;package

\ The child's libraries stand on pre-seal sources the participants stop short
\ of. They load here in src/habu/native-runtime.f's order, before the seal, as
\ on the product. The last declaration source seals registration: until it
\ loads, lib/string.f's ENUM is refused with E-REGISTRATION-SEALED.
require src/habu/xref.f
require src/core/generated-declaration-dictionary.f
require src/core/generated-declaration-protection.f
require src/core/dynamic-storage.f
require lib/prelude.f

require src/core/prefix-boundary.f
include src/core/internal-mark.f

\ The product's seal (src/habu/native-runtime.f CHECKER-REG:SEAL) ends with
\ SEAL-CAPTURE, which records the watermark that arms the protected-wid refusals
\ the child checks (src/habu/habu2.f OPEN-WID,). The window stops short of it.
SEAL-CAPTURE

\ The saved AOT builder (tools/aot-builder-image.f) built on the linker image,
\ for rows whose children link on PRELOADED-ENGINE:LINKER$. Resolve it before
\ staging argv or env: the image supplier uses those same tables. The linker is
\ selected as the build engine only for this build, so it never stays the
\ process default. This is the image an hb-build child on the linker selects
\ (tools/hb-build-lib.f HBB-MAKER-IMAGE$), so one settle serves the direct
\ maker runs and every hb-build child in the same out root.
require tools/hb-build-lib.f
require test/preloaded-engine.f
require test/image-grant.f
require test/suite-budget.f

package LINKER-BUILDER-ENGINE

public

: PATH$ ( -- ptr u8 n )
   s" linker-builder" IMAGE-GRANT:CHECK
   PRELOADED-ENGINE:LINKER$ BUILD-FIXPOINT:BF-ENGINE!
   [: SUITE-BUDGET:CHILD-MS HB-BUILD-CLI:HBB-MAKER-IMAGE$ ;]
   [: BUILD-FIXPOINT:BF-ENGINE-RESET ;] finally ;

: ENSURE ( -- )
   PATH$ 2drop ;

\ The engine the builder image was built on, which a row hands its children.
: HOST$ ( -- ptr u8 n )
   PRELOADED-ENGINE:LINKER$ ;

;package

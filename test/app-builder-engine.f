\ The keyed REPL builder (tools/app-build-image.f) on the product engine.
\ Resolve it before staging argv or env: the image supplier uses those same
\ tables. The product engine is selected as the build engine only for this
\ build, so it never stays the process default.
require src/lib/engine-candidate.f
require tools/app-build-image.f
require test/image-grant.f
require src/lib/process.f

package APP-BUILDER-ENGINE

public

: PATH$ ( -- ptr u8 n )
   s" app-builder" IMAGE-GRANT:CHECK
   ENGINE-CANDIDATE:PATH$ BUILD-FIXPOINT:BF-ENGINE!
   [: PROCESS:WAIT-FOREVER MS>N APP-BUILD-IMAGE:PATH$ ;]
   [: BUILD-FIXPOINT:BF-ENGINE-RESET ;] finally ;

: ENSURE ( -- )
   PATH$ 2drop ;

;package

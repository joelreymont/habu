\ Compatibility command for the engine target in tools/hb-build.f.
\ Orchestration stays at the caller's tier; the driver selects its target's.
\ An earlier --load entry can alter the donor compiler or install a backend.
\ Such a process keeps its live source-loaded driver and its exact state.
package NATIVE-BUILD-ENTRY
\ The current script-required row is already present while this file runs.
REQUIRE-REG:COUNT REQUIRE-BOOT-LIMIT 1+ > constant PRELOADED?
;package
\ A refused command line stops here, before BUILD below loads the whole native
\ compiler to reach the driver's own check.
require tools/native-build-args.f
NATIVE-BUILD:BUILD-ARGS!
require src/lib/executable-build.f

package NATIVE-BUILD-ENTRY

\ An explicit target can depend on a backend loaded earlier in this process.
: SOURCE-ENTRY? ( -- bool )
   PRELOADED? if true exit then
   NATIVE-BUILD:EXPLICIT-TARGET? ;

\ The required driver is loaded inside the protected executable-build scope.
\ The entry is named in a text because it resolves only after require returns.
: BUILD ( -- )
   [: s" tools/native-build-core.f" required ;] EXECUTABLE-BUILD:WITH
   s" NATIVE-BUILD:RUN" evaluate-closed ;

\ The public command owns the ordinary saved-support selection, source-input
\ manifest, artifact cache and output installation. Keep the legacy spelling
\ and result line here while using that build session.
: RUN-HB-BUILD ( -- )
   s" tools/hb-build-lib.f" required
   s" 0 SCRIPT-ARGV$ NATIVE-BUILD:WHITEBOX? NATIVE-BUILD:EXPLICIT-TARGET? HB-BUILD-CLI:HBB-ENGINE-RUN-PATH" evaluate-closed
   s" " 0 die ;

: RUN ( -- )
   SOURCE-ENTRY? if BUILD exit then
   RUN-HB-BUILD ;

' RUN
;package
execute

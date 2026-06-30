\ warm-image-gate-stats.f - gate stats hook for warm-image-lib.f.
\
\ Load after test/gate-stats.f and tools/warm-image-lib.f.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/source.f
require lib/codesign.f
require test/gate-stats.f
require tools/warm-image-lib.f

: WIGS-EVENT ( ptr u8 n -- )
   GS-EVENT ;

: WIGS-INSTALL ( -- )
   [: WIGS-EVENT ;] is WI-EVENT ;

WIGS-INSTALL

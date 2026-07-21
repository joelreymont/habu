\ build-fixpoint-refresh.f - self-contained native refresh entry.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/codesign.f
require tools/build-fixpoint.f

package BUILD-FIXPOINT-REFRESH

public

: RUN ( -- )
   BF-CLI ;

;package

BUILD-FIXPOINT-REFRESH:RUN

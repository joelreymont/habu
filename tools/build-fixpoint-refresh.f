\ build-fixpoint-refresh.f - self-contained native refresh entry.

require src/lib/errors.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/codesign.f
require tools/build-fixpoint.f

package BUILD-FIXPOINT-REFRESH

public

: RUN ( -- )
   BUILD-FIXPOINT:BF-CLI ;

;package

BUILD-FIXPOINT-REFRESH:RUN

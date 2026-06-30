\ warm-image.f - CLI wrapper for warm snapshot images.
\
\ Run: bin/hb --load tools/warm-image.f -- OUT [SUPPORT...]

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
require tools/warm-image-lib.f

WI-MAIN

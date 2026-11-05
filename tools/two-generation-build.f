\ Uncached build identity and generation-chain checks. The core also supplies
\ the focused byte-comparison fixtures without starting builds.
\ Run: bin/hb --load tools/two-generation-build.f -- [seed]
\      bin/hb --load tools/two-generation-build.f -- --same-host [seed]
\      bin/hb --load tools/two-generation-build.f -- --compare file-a file-b
require tools/two-generation-core.f
TWO-GEN:MAIN

\ build-fixpoint-main.f - CLI entrypoint for tools/build-fixpoint.f.
\ Load after tools/build-fixpoint.f. BF-CLI is the fail-closed boundary:
\ any escaped throw is reported on stderr and exits BF-BUILD-RC.

\ Load-discipline guard, mirroring tools/build-fixpoint.f. Loading this CLI entry
\ without its full chain (lib preamble + tools/build-fixpoint.f) otherwise dies
\ with a bare `E-UNDEFINED: BF-CLI` (or FS-PATH-CAP) on the first missing word.
\ BF-CLI is the sentinel: it is defined only once build-fixpoint.f loaded past its
\ own preamble guard, so its presence proves the whole chain is here. Self-contained
\ (BF-USAGE-RC lives in build-fixpoint.f, which is absent in exactly this failure).
64 constant BFM-USAGE-RC
: BFM-NEED-PREAMBLE ( -- )
   s" BF-CLI" CHECKER-DEFINED? if exit then
   S\" build-fixpoint: missing required load; --load lib/errors.f lib/string.f lib/memory.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/codesign.f tools/build-fixpoint.f tools/build-fixpoint-main.f before the build verb\n" BFM-USAGE-RC die ;
BFM-NEED-PREAMBLE

BF-CLI

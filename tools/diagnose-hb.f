\ diagnose-hb.f - CLI entry: report why bin/hb fails to start outside the repo.
\
\ Run: bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/fs.f \
\   tools/diagnose-hb-core.f tools/diagnose-hb.f -- [ROOT]
\ ROOT precedence: argv[0], else $HABU_ROOT, else the current directory.

require tools/diagnose-hb-core.f

DIAGNOSE:MAIN

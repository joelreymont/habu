\ hb-build.f - CLI entrypoint for tools/hb-build-lib.f.
\ Load after tools/hb-build-lib.f.
\
\ One call, so it names the package rather than importing it, and it runs at
\ top level with every package closed - a build resolves names in whatever
\ package scope is open when it runs.

HB-BUILD-CLI:HBB-MAIN

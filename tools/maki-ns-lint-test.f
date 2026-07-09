\ maki-ns-lint-test.f - checked fixtures for the maki wordlist-namespace lint.
\ Load after lib/test.f and tools/maki-ns-lint-core.f.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/token.f
require tools/maki-ns-lint-core.f

: MNLT-EXT ( -- )
   s" maki/gpu.f"   MNL-SRC? TTRUE
   s" maki/eval.f"  MNL-SRC? TTRUE
   s" docs/x.md"    MNL-SRC? TFALSE ;

: MNLT-DETECT ( -- )
   \ RED-FIRST: a top-level definition outside package MAKI is caught
   s" : FOO dup ;"             MNL-COUNT 1 T=
   s" variable GX"             MNL-COUNT 1 T=
   s" create BUF 4 allot"      MNL-COUNT 1 T=
   s" TRUSTED: RAW dup ;"      MNL-COUNT 1 T=
   \ several unwrapped defs -> several findings
   s" : A dup ; : B drop ;"    MNL-COUNT 2 T= ;

: MNLT-WRAPPED-OK ( -- )
   \ inside package MAKI -> no finding
   s" package MAKI : FOO dup ; end-package"     MNL-COUNT 0 T=
   s" package MAKI variable GX end-package"      MNL-COUNT 0 T=
   \ a private helper before `public` is still inside MAKI -> ok
   s" package MAKI : H dup ; public : G H ; end-package"  MNL-COUNT 0 T=
   \ cross-cutting error constants stay global (exempt)
   s" -5000 constant E-MK-DTYPE"                 MNL-COUNT 0 T=
   s" -5002 constant E-FUSE"                     MNL-COUNT 0 T=
   \ a non-MAKI package with NO marker (bare string) IS flagged, not silently OK
   s" package CUDA : BAR dup ; end-package"      MNL-COUNT 1 T=
   \ comments never trip (TOKENIZE strips them)
   s" \ defines FOO in maki prose"               MNL-COUNT 0 T= ;

: MNLT-MARKER ( -- )
   s" \ maki-ns-lint: boundary CUDA - subsystem" MNL-MARKER? TTRUE
   s" \ ordinary comment"                         MNL-MARKER? TFALSE ;

: MNLT-STALE ( -- )
   \ fresh marker: <PKG> matches the file's `package` token -> not stale
   S\" \\ maki-ns-lint: boundary CUDA - x\npackage CUDA\n: FOO dup ;\n"   MNL-STALE? TFALSE
   \ stale marker: names FUSION but the file declares CUDA -> stale finding
   S\" \\ maki-ns-lint: boundary FUSION - x\npackage CUDA\n: FOO dup ;\n"  MNL-STALE? TTRUE ;

: MNLT-LIVE ( -- )
   \ the real maki/ tree must be namespace-clean -> MAKI-NS-LINT returns clean
   MAKI-NS-LINT ;

: MNLT-MAIN ( -- )
   T-RESET
   MNLT-EXT
   MNLT-DETECT
   MNLT-WRAPPED-OK
   MNLT-MARKER
   MNLT-STALE
   MNLT-LIVE
   T-REPORT ;

MNLT-MAIN

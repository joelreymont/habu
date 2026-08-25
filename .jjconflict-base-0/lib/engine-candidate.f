\ engine-candidate.f - the validated engine binary nested tooling should trust and spawn.
\
\ One concern: resolve WHICH engine a nested tool (a supervised child, a replay
\ clone, a candidate-under-test worker) must run. The answer is either an explicit
\ HABU_UNDER_TEST override a parent set, or - with no override - the running
\ engine's own identity, so a standalone tool replays through the very binary that
\ launched it. Every resolved path is validated as a real executable before it is
\ handed out, so a bad override fails closed here (E-FS-OPEN) rather than deep
\ inside a later spawn.
\
\ Resolution order (highest priority first):
\   1. HABU_UNDER_TEST in the child-env default table a gate exports to its children
\      (PROC-ENV-DEFAULT$?);
\   2. HABU_UNDER_TEST in the live process environment (GETENV);
\   3. no override -> the running engine's own executable path (ENGINE-ID:PATH$).
\
\ This is the ONE shared resolver: the maki replay child (maki/cad-test.f) and a
\ PTY-supervised engine-under-test both take their engine path from here instead of
\ each re-deriving the same override/self rule.

require lib/errors.f
require lib/fs.f
require lib/process-env.f
require lib/engine-id.f

\ GETENV (src/os/env-base.f) and the >LEN/LEN>N role coercions (src/core/roles.f)
\ are engine-provided; PROC-ENV-DEFAULT$? is lib/process-env.f, EXECUTABLE? is
\ lib/fs.f, ENGINE-ID:PATH$ is lib/engine-id.f.

package ENGINE-CANDIDATE

private

\ The environment variable a parent sets to redirect nested tooling at a specific
\ engine binary; a gate names the freshly built candidate here.
: NAME$ ( -- ptr u8 n )  s" HABU_UNDER_TEST" ;

\ The unvalidated resolved path: the override if one is in force (child-env default
\ table first, then the live environment), else the running engine's own path.
: RESOLVE$ ( -- ptr u8 n )
   NAME$ >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   NAME$ GETENV dup 0 > if exit then
   2drop ENGINE-ID:PATH$ ;

\ Fail closed unless the path names a real executable.
: VALIDATE$ ( ptr u8 n -- ptr u8 n )
   2dup EXECUTABLE? 0= if E-FS-OPEN throw then ;

public

\ The engine binary nested tooling should trust and spawn, validated executable.
: PATH$ ( -- ptr u8 n )
   RESOLVE$ VALIDATE$ ;

;package

\ engine-candidate.f - the validated engine binary nested tooling trusts and
\ spawns: HABU_UNDER_TEST from the child-env default table a gate exports, then
\ from the live environment, else the running engine's own path
\ (ENGINE-ID:PATH$), so a standalone tool replays through the binary that
\ launched it. The path is validated as an executable here, so a bad override
\ fails closed (E-FS-OPEN) before any spawn.

require src/lib/errors.f
require src/lib/fs.f
require src/lib/process-env.f
require src/lib/engine-id.f

\ GETENV (src/os/env-base.f) and the >LEN/LEN>N role coercions (src/core/roles.f)
\ are engine-provided; PROC-ENV-DEFAULT$? is src/lib/process-env.f, FS:EXECUTABLE? is
\ src/lib/fs.f, ENGINE-ID:PATH$ is src/lib/engine-id.f.

package ENGINE-CANDIDATE

private

\ A gate names the freshly built candidate here.
: NAME$ ( -- ptr u8 n )  s" HABU_UNDER_TEST" ;

: RESOLVE$ ( -- ptr u8 n )
   NAME$ >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   NAME$ GETENV dup 0 > if exit then
   2drop ENGINE-ID:PATH$ ;

: VALIDATE$ ( ptr u8 n -- ptr u8 n )
   2dup FS:EXECUTABLE? 0= if E-FS-OPEN throw then ;

public

: PATH$ ( -- ptr u8 n )
   RESOLVE$ VALIDATE$ ;

;package

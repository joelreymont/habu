\ engine-candidate.f - the validated engine binary nested tooling trusts and
\ spawns: HABU_UNDER_TEST from the child-env default table a gate exports, then
\ from the live environment, else the running engine's own path
\ (ENGINE-ID:PATH$), so a standalone tool replays through the binary that
\ launched it. The path is absolute, a relative override joined to this
\ process's working directory, so it names the same file to a child spawned in
\ another directory. It is validated as an executable here, so a bad override
\ fails closed (E-FS-OPEN) before any spawn.

require src/lib/errors.f
require src/lib/fs.f
require src/lib/process-env.f
require src/lib/engine-id.f

\ GETENV (src/os/env-base.f), the >LEN/LEN>N role coercions (src/core/roles.f)
\ and SOURCE-ROOT:CWD$ (src/core/include.f) are engine-provided;
\ PROC-ENV-DEFAULT$? is src/lib/process-env.f, FS:EXECUTABLE? and FS:JOIN-PATH
\ are src/lib/fs.f, ENGINE-ID:PATH$ is src/lib/engine-id.f.

package ENGINE-CANDIDATE

private

\ A gate names the freshly built candidate here.
: NAME$ ( -- ptr u8 n )  s" HABU_UNDER_TEST" ;

PATH-CAP BUFFER: JOINED

: RESOLVE$ ( -- ptr u8 n )
   NAME$ >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   NAME$ GETENV dup 0 > if exit then
   2drop ENGINE-ID:PATH$ ;

\ An empty override stays empty for VALIDATE$ to refuse; joined, it would name
\ the working directory.
: ABSOLUTE$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u 0= if a u exit then
   a c@ [char] / = if a u exit then
   SOURCE-ROOT:CWD$ a u JOINED FS:JOIN-PATH {: ju:n :}
   JOINED ju ;

: VALIDATE$ ( ptr u8 n -- ptr u8 n )
   2dup FS:EXECUTABLE? 0= if E-FS-OPEN throw then ;

public

: PATH$ ( -- ptr u8 n )
   RESOLVE$ ABSOLUTE$ VALIDATE$ ;

;package

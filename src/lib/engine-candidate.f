\ engine-candidate.f - the validated engine binary nested tooling trusts and
\ spawns: HABU_UNDER_TEST from the child-env default table a gate exports, then
\ from the live environment, else the running program's own path, so a
\ standalone tool replays through the binary that launched it. The path is
\ absolute, a relative override joined to this process's working directory, so
\ it names the same file to a child spawned in another directory. It is
\ validated as an executable here, so a bad override is rejected with its record
\ (E-ENGINE-NOT-EXECUTABLE) before any spawn.

require src/core/exit-status.f
require src/lib/fs.f
require src/lib/process-env.f
require src/lib/engine-id.f

package ENGINE-CANDIDATE

private

\ A gate names the freshly built candidate here.
: NAME$ ( -- ^u8 n )  s" HABU_UNDER_TEST" ;

PATH-CAP BUFFER: JOINED

: RESOLVE$ ( -- ^u8 n )
   NAME$ >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   NAME$ GETENV dup 0 > if exit then
   2drop ENGINE-ID:OWN-PATH ;

\ An empty override stays empty for VALIDATE$ to reject; joined, it would name
\ the working directory.
: ABSOLUTE$ ( ^u8 n -- ^u8 n ) {: a:^ u:n :}
   u 0= if a u exit then
   a c@ [char] / = if a u exit then
   SOURCE-ROOT:CWD$ a u JOINED FS:JOIN-PATH {: ju:n :}
   JOINED ju ;

\ The running engine is always executable, so a path that is not is the
\ override's.
: REJECT ( ^u8 n -- ) {: a:^ u:n :}
   s" E-ENGINE-NOT-EXECUTABLE" REJECTION:OPEN
   NAME$ REJECTION:EMIT-STR  s"  names `" REJECTION:EMIT-STR
   a u REJECTION:EMIT-STR  s" `, which is not an executable file" REJECTION:EMIT-STR
   REJECTION:HELP
   s" point " REJECTION:EMIT-STR  NAME$ REJECTION:EMIT-STR
   s"  at an executable hb, or unset it" REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

: VALIDATE$ ( ^u8 n -- ^u8 n )
   2dup FS:EXECUTABLE? 0= if 2dup REJECT then ;

public

: PATH$ ( -- ^u8 n )
   RESOLVE$ ABSOLUTE$ VALIDATE$ ;

;package

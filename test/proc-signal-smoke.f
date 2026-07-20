\ proc-signal-smoke.f - focused proof of the kill-errno and execve
\ process-control primitives: a signal-0 probe to the running process itself
\ reports success (0); a signal to a positive but non-existent pid reports the
\ ESRCH errno class (-3), not a bare -1; and execve on a missing path reports the
\ ENOENT errno class (-2) through its failure-only return path. Both primitives
\ normalize the per-OS error convention (Linux's direct -errno, macOS's carry
\ flag plus positive errno) to 0 on success or -errno on failure, so the
\ supervisor can tell ESRCH from EPERM.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f \
\      lib/process.f lib/process-argv.f test/proc-signal-smoke.f

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/process.f
require lib/process-argv.f

package PROC-SIGNAL-SMOKE

0 constant SIG-PROBE               \ the null signal: validate the pid, deliver nothing
3 constant ESRCH#                  \ "no such process" (POSIX, identical on Linux/macOS)
2 constant ENOENT#                 \ "no such file or directory" (POSIX, identical both OSes)
$7FFF0000 constant DEAD-PID        \ a positive pid guaranteed not to exist

create EXEC-ENVP 1 cells allot     \ a single-slot, NULL-terminated (empty) environment

\ ---- checks ------------------------------------------------------------------
\ Signal 0 to our own live pid: the process exists, so the probe succeeds.
: CHECK-SELF-PROBE ( -- )
   getpid SIG-PROBE kill-errno 0 T= ;

\ Signal 0 to a positive but non-existent pid must report -ESRCH, not a bare -1,
\ so the caller can distinguish it from -EPERM.
: CHECK-DEAD-PID ( -- )
   DEAD-PID SIG-PROBE kill-errno ESRCH# negate T= ;

\ execve only returns on failure; a missing absolute path fails path resolution
\ before touching argv/envp, so the primitive returns -ENOENT and the running
\ process is left intact. PROC-ARGV-PREPARE builds the [pathz, NULL] argv; the
\ NULL-terminated EXEC-ENVP is the empty environment.
: CHECK-EXEC-FAIL ( -- )
   0 EXEC-ENVP !
   PROC-ARGV-RESET
   s" /no/such/hb-execve-probe" >LEN PROC-ARGV-PREPARE
   EXEC-ENVP execve ENOENT# negate T= ;

: RUN ( -- )
   T-RESET
   CHECK-SELF-PROBE
   CHECK-DEAD-PID
   CHECK-EXEC-FAIL
   T-REPORT
   s" proc-signal-smoke: ok" type cr ;

RUN

;package

\ why-threw.f - throw-site diagnostic for opaque capacity throws.
\
\ A bare throw code (e.g. E-STR-CAPACITY -2201) loses the buffer that overflowed.
\ This reports the code plus the live fill of every shared string builder (SB,
\ content-key CK and CK-ROW) so the overflowing buffer names itself. It matters
\ most in fork-worker / parallel-gate captures, where the code is all the log
\ shows. Globals are not unwound by throw, so the reported fill reflects the
\ state AT the throw; each field is its own `WHY-THREW:` line so it greps cleanly
\ out of an interleaved capture file.
\
\ `WHY-THREW-DUMP ( code -- )` prints the report; the gate fork-throw handler
\ (test/gate-pool.f) calls it so any parallel worker's throw self-identifies.
\ `WHY-THREW ( [ -- ] -- )` runs a quotation under catch, dumps, and re-throws
\ for standalone use.
\
\ Run: bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/fs.f
\   lib/content-key.f tools/why-threw.f

require lib/errors.f
require lib/string.f
require lib/content-key.f

\ Each value ends its line: the engine's `.` emits the number and a newline, so a
\ line reads e.g. `WHY-THREW: SB fill=975`.
: WHY-THREW-DUMP ( n -- ) {: rc:n :}
   s" WHY-THREW: fork-worker throw code=" type rc .
   s" WHY-THREW: SB fill=" type SB-LEN @ .
   s" WHY-THREW: SB cap=" type SB-CAP .
   s" WHY-THREW: CK fill=" type CK-U @ .
   s" WHY-THREW: CK cap=" type CK-CAP .
   s" WHY-THREW: CK-ROW fill=" type CK-ROW-U @ .
   s" WHY-THREW: CK-ROW cap=" type CK-ROW-CAP . ;

\ typed-local-lint: allow-bare-local - q keeps the suspect quotation effect.
: WHY-THREW ( [ -- ] -- ) {: q :}
   q catch {: rc:n :}
   rc 0= if exit then
   rc WHY-THREW-DUMP
   rc throw ;

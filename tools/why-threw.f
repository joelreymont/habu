\ why-threw.f - throw-site diagnostic for opaque capacity throws.
\
\ A bare throw code (e.g. E-STR-CAPACITY -2201) loses the buffer that overflowed.
\ This reports the code plus the live fill of the shared string builder (SB).
\ It matters most in fork-worker / parallel-gate captures, where the code is
\ all the log shows. Globals are not unwound by throw, so the reported fill
\ reflects the state AT the throw; each field is its own `WHY-THREW:` line so
\ it greps cleanly out of an interleaved capture file.
\
\ The module is `package WHY-THREW`; its two entries keep their historic
\ spellings, which repeat the module name in the tail. That is pre-existing
\ prefix debt, not a new pattern: shortening them would edit the body of an
\ unpackaged 1155-line consumer (test/gate-pool.f) and pull its own packaging
\ into this change.
\
\ `WHY-THREW-DUMP ( code -- )` prints the report; the gate fork-throw handler
\ (test/gate-pool.f) calls it so any parallel worker's throw self-identifies.
\ `WHY-THREW ( [ -- ] -- )` runs a quotation under catch, dumps, and re-throws
\ for standalone use.
\
\ Run: bin/hb --load tools/why-threw.f

require src/lib/errors.f
require src/lib/string.f

package WHY-THREW

public

\ Each value ends its line: the engine's `.` emits the number and a newline, so a
\ line reads e.g. `WHY-THREW: SB fill=975`.
: WHY-THREW-DUMP ( n -- ) {: rc:n :}
   s" WHY-THREW: fork-worker throw code=" type rc .
   s" WHY-THREW: SB fill=" type SB-LEN @ .
   s" WHY-THREW: SB cap=" type SB-CAP . ;

: WHY-THREW ( [ -- ] -- ) {: q :}
   q catch {: rc:n :}
   rc 0= if exit then
   rc WHY-THREW-DUMP
   rc throw ;

;package

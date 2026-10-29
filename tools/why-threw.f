\ why-threw.f - throw-site diagnostic for opaque capacity throws: the code plus
\ the live fill of the shared string builder (SB), one `WHY-THREW:` line per field
\ so it greps out of an interleaved capture. Globals are not unwound by throw, so
\ the fill is the state at the throw. WHY-THREW-DUMP is the gate fork-throw
\ handler's (test/gate-pool.f); WHY-THREW runs a quotation under catch, dumps and
\ rethrows. Run: bin/hb --load tools/why-threw.f

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

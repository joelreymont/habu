\ why-threw.f - throw-site diagnostic for opaque capacity throws.
\
\ A bare throw code (e.g. E-STR-CAPACITY -2201) loses the buffer that overflowed.
\ This reports the code plus the live fill of the shared string builder (SB),
\ every open content-key fold, and the content-key row builder, so the
\ overflowing buffer names itself. A fold's fill is reported per slot because
\ this runs from a throw handler, which holds no fold handle; a free slot reads
\ 0, so the line that is not 0 is the fold that was being built. It matters
\ most in fork-worker / parallel-gate captures, where the code is all the log
\ shows. Globals are not unwound by throw, so the reported fill reflects the
\ state AT the throw; each field is its own `WHY-THREW:` line so it greps cleanly
\ out of an interleaved capture file.
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
\ Run: bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/fs.f
\   lib/content-key.f tools/why-threw.f

require lib/errors.f
require lib/string.f
require lib/content-key.f

package WHY-THREW

variable WT-I

: FOLD-FILLS ( -- )
   0 WT-I !
   begin WT-I @ CONTENT-KEY:FOLDS < while
      s" WHY-THREW: CK fill=" type WT-I @ CONTENT-KEY:FOLD-FILL .
      WT-I @ 1+ WT-I !
   repeat ;

public

\ Each value ends its line: the engine's `.` emits the number and a newline, so a
\ line reads e.g. `WHY-THREW: SB fill=975`.
: WHY-THREW-DUMP ( n -- ) {: rc:n :}
   s" WHY-THREW: fork-worker throw code=" type rc .
   s" WHY-THREW: SB fill=" type SB-LEN @ .
   s" WHY-THREW: SB cap=" type SB-CAP .
   FOLD-FILLS
   s" WHY-THREW: CK cap=" type CONTENT-KEY:BUF-CAP .
   s" WHY-THREW: CK-ROW fill=" type CONTENT-KEY:ROW$ nip .
   s" WHY-THREW: CK-ROW cap=" type CONTENT-KEY:ROW-CAP . ;

\ typed-local-lint: allow-bare-local - q keeps the suspect quotation effect.
: WHY-THREW ( [ -- ] -- ) {: q :}
   q catch {: rc:n :}
   rc 0= if exit then
   rc WHY-THREW-DUMP
   rc throw ;

;package

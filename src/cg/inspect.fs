\ inspect.fs — reliable codegen inspection. EVERY word here is a colon
\ definition, so it is safe to call from the gforth REPL. (At the interpret
\ level, compile-only words — `[']`, `i` inside a loop, `{: :}` locals, and
\ `>r`/`r@` straddling `do`/`loop` — silently read garbage. Do NOT hand-roll
\ diagnostics out of those; call these instead. See LESSONS.md § Debugging.)
\
\ Drives the ICode assembler (asm.fs) + disassembler (disasm.fs). Assemble the
\ *current* ICODE buffer into a private scratch area so SCODE/CODELEN (used by
\ the real build) are never clobbered.

require asm.fs
require disasm.fs

create ICODEBUF 65536 allot   variable ICODELEN
: IC-ASM ( -- addr u )  ICODEBUF ASSEMBLE dup ICODELEN !  ICODEBUF swap ;

: ICDUMP ( -- )    IC-ASM 4 / DISASM ;          \ disassemble the current ICODE program

variable SCAN-W
: ICSCAN ( u32 -- )                              \ byte offsets where a word == u32
   SCAN-W !  IC-ASM 2drop
   ICODELEN @ 4 / 0 ?do
      ICODEBUF i 4 * + l@ SCAN-W @ = if cr ."   @ " i 4 * . then
   loop ;

: ?LBL ( id -- )   cr ." L" dup . ." @ "  cells LBLPOS + @ . ;   \ a label's byte offset

: ICAT ( byteoff -- )  cr ICODEBUF + 1 DISASM ;   \ decode one already-assembled word

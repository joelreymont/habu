\ Inspect complete Intel code spans through the bounded production decoder.
require src/habu/code-bytes.f
require src/arch/x86-64/disasm.f

package X64CODEGEN-PROBE
private

: CODE ( ptr u8 n -- ptr u8 n n ) {: name:ptr size:n :}
   name size XREF-FIND dup XREF-FOUND? 0= if
      drop s" codegen-x64-probe: record not found" 76 die
   then {: rec:ptr :}
   rec XREF-START {: pc:n :}
   rec XREF-CODE-BYTES {: bytes:n :}
   pc bytes CODE-BYTES:AT drop bytes pc ;

public

\ The final decoded instruction must leave this word for a direct target.
: TAIL-BRANCH? ( ptr u8 n -- bool )
   CODE {: code:ptr bytes:n pc:n :}
   0 begin dup bytes < while
      {: at:n :}
      code at + bytes at - pc at + X64DIS:STEP
         {: step:n flow:n target:n :}
      at step + dup bytes = if
         drop
         flow X64DIS:FLOW-JUMP =
         target pc < target pc bytes + >= or and exit
      then
   repeat drop false ;

\ Count direct calls to the exact published entry, not matching byte patterns
\ inside an operand of another instruction.
: CALLS-TO ( ptr u8 n n -- n ) {: name:ptr size:n wanted:n :}
   name size CODE {: code:ptr bytes:n pc:n :}
   0 0 begin dup bytes < while
      {: at:n :}
      code at + bytes at - pc at + X64DIS:STEP
         {: step:n flow:n target:n :}
      flow X64DIS:FLOW-CALL = target wanted = and if 1+ then
      at step +
   repeat drop ;

;package

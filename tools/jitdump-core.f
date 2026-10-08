\ jitdump-core.f - reusable JIT code disassembly words.

require src/arch/arm64/disasm.f
require src/habu/code-bytes.f

\ CLI: bin/hb --load tools/jitdump.f -- '<program>' WORD
\ Inline usage when disasm.f is already loaded: <program> ' WORD JITDUMP:JD
\ Walks from the xt to the first RET (inclusive), capped at 512 instructions.
\
\ The word reader below, `W32@`, is private to this package, as the breakpoint
\ debugger's fetch of the same name is to package DEBUG (src/habu/debug.f), so
\ the two never collide in one dictionary. The public spellings do not move:
\ the one test that calls three of them imports the package with `using` and
\ keeps its own definitions untouched.
package JITDUMP
private

512 constant MAX-INSTR
4 constant INSN-BYTES

: W32@ ( ptr u8 -- n ) {: p:ptr :}
   p c@
   p 1 + c@ 8 lshift or
   p 2 + c@ 16 lshift or
   p 3 + c@ 24 lshift or ;
\ The cursor is an address the engine handed back as a number - an xt - and it
\ stays one: CODE-BYTES:AT is where each step becomes bytes, so a walk that runs
\ off the end of the emitted code is refused instead of decoding whatever
\ follows it.
variable JDP  variable JDN

: JD-INSN@ ( -- n )
   JDP @ INSN-BYTES CODE-BYTES:AT drop W32@ ;

: JIT-USAGE ( -- )
   s" usage: bin/hb --load src/arch/arm64/disasm.f tools/jitdump.f -- '<program>' WORD" 64 die ;

public

: JD ( n -- ) {: xt:n :}
   xt JDP !  0 JDN !
   BEGIN
     JD-INSN@ DIS1
     JDN @ 1 + JDN !
     JD-INSN@ $D65F03C0 =  JDN @ MAX-INSTR 1 - > or
     JDP @ INSN-BYTES + JDP !
   UNTIL ;

: JIT-FIND ( ptr u8 n -- n )
   get-current search-wl dup 0= if s" jitdump: target word not found" 74 die then ;

\ Evaluates caller source through the real compiler before lookup. The program
\ defines words and leaves nothing: a cell it leaves is E-EVAL-RESIDUE.
: JIT-EVALUATE ( ptr u8 n -- )
   evaluate-closed ;

: JIT-MAIN ( -- )
   SCRIPT-ARGC 2 <> if JIT-USAGE then
   0 SCRIPT-ARGV$ JIT-EVALUATE
   1 SCRIPT-ARGV$ JIT-FIND JD ;

: JIT-AUTO ( -- )
   SCRIPT-ARGC 0 > if JIT-MAIN then ;

;package

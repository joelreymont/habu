\ jitdump-core.f - reusable JIT code disassembly words.

require src/arch/arm64/disasm.f
require src/habu/code-bytes.f

\ CLI: bin/hb --load tools/jitdump.f -- '<program>' WORD
\ Inline usage when disasm.f is already loaded: <program> ' WORD JITDUMP:JD
\ Walks from the xt to the first RET (inclusive), capped at 512 instructions.
\
\ The word reader below used to be the global `W32@`, which is also the 32-bit
\ fetch the engine's baked breakpoint debugger (src/habu/debug.f) publishes.
\ The AOT seed now runs at the end of the engine prefix on every boot (dot
\ habu-decide-arm-the-5234727b), so both would land in one dictionary and this
\ file would die `duplicate definition` at load. Every other reader of that
\ shape in the tree already carries an owner prefix (AOT-W32@, ACAP-W32@); this
\ one gets a package, which is what docs/forth.md asks for anyway. The public
\ spellings do not move: the one test that calls three of them imports the
\ package with `using` and keeps its own definitions untouched.
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

\ Evaluates caller source through the real compiler before lookup; retire with
\ habu-builder-trust-rows-c5d41af6 when dynamic evaluation is checker-typed.
TRUSTED: JIT-EVALUATE ( ptr u8 n -- )
   evaluate ;

: JIT-MAIN ( -- )
   SCRIPT-ARGC 2 <> if JIT-USAGE then
   0 SCRIPT-ARGV$ JIT-EVALUATE
   1 SCRIPT-ARGV$ JIT-FIND JD ;

: JIT-AUTO ( -- )
   SCRIPT-ARGC 0 > if JIT-MAIN then ;

;package

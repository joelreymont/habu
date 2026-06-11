\ walk-demo.fs — compile the Forth body "7 dup *" to native code with GEN-BODY and
\ emit a self-signed binary to /tmp/sh-walk-bin (exit 49). Needs full Mach-O builder (macho.fs+sign2.fs)
\ +asm+icode+walk. Proves the standalone COMPILES source (not hand-assembled/copied).
: GO
  ASM-INIT
  s" 7 dup *" GEN-BODY
  BUILD-MACHO  s" sh" SET-SIGID CODESIG2
  s" /tmp/sh-walk-bin" PATH0 1537 493 open dup MBUF MLEN @ write drop close ;
GO

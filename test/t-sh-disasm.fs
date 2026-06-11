\ t-sh-disasm.fs — the standalone's own ARM64 disassembler (src/arch/arm64/disasm.f) decodes
\ a known instruction buffer to mnemonics + operands (self-hosted debugging of generated
\ code, zero gforth/python). Run: gforth test/t-sh-disasm.fs -e bye
require sh-driver.fs
: DIS-OUT ( -- a u )
   0 CL !
   s" src/arch/arm64/disasm.f"      slurp-file +B   s"  " +B
   s" test/demos/disasm-demo.f" slurp-file +B
   CBUF CL @ NF-RUN  NFOUT 2@ ;
\ '.' prints "<n>\n"; mnemonics via type+space. movz x5,#42 / add x1,x2,x3 / ret / svc #0:
T{ DIS-OUT s\" movz 5\n42\nadd 1\n2\n3\nret svc 0\n" compare 0= -> true }T

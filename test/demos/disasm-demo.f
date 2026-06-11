\ disasm-demo.fs — disassemble a known instruction buffer (movz x5,#42; add x1,x2,x3;
\ ret; svc #0) with the standalone disassembler. Driven by test/t-sh-disasm.fs.
create P 69 c, 5 c, 128 c, 210 c, 65 c, 0 c, 3 c, 139 c, 192 c, 3 c, 95 c, 214 c, 1 c, 0 c, 0 c, 212 c, 
: GO P 4 DISASM ;
GO

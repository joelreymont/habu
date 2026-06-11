\ sign2-demo.fs — drive the FULL-builder self-signer: assemble exit(42), build the
\ unsigned Mach-O (macho.fs), apply the ad-hoc CodeDirectory post-pass (sign2.fs),
\ write /tmp/se-signed. Apple's codesign validates it; it runs (exit 42).
: GO  ASM-INIT  0 42 MOVZ,  16 1 MOVZ,  $80 SVC,
   BUILD-MACHO  s" se-out" SET-SIGID  CODESIG2
   s" /tmp/se-signed" PATH0 1537 493 open dup MBUF MLEN @ write drop close ;
GO

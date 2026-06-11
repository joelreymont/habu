\ sign-demo.fs — driver: build the exit(42) image (macho-min.fs), self-sign it
\ (sign.fs), and write the signed binary to /tmp/se-signed. Concatenated after
\ sha256.fs + macho-min.fs + sign.fs by test/t-sh-sign.fs.
: GO
  s" /tmp/se-signed" PSET
  BUILD CODESIG
  PB 1537 493 open {: fd :}
  fd MSTART @ MOFF write drop
  fd close ;
GO

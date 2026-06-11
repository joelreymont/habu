\ t-sh-fp-enc.fs — FP encoders (fp-a): habu-side values match the bootstrap
\ encode-pass constants, formula-golden. Run: gforth test/t-sh-fp-enc.fs -e bye
require sh-driver.fs
create EB 4096 allot  variable EL
: e+ ( a u -- )  bounds ?do i c@ EB EL @ + c! 1 EL +! loop ;
: n+ ( n -- )  0 <# 10 hold #s #> e+ ;
: REF ( -- )  0 EL !
   $1E604000 1 or 2 5 lshift or n+          \ fmov d1,d2
   $1E601800 1 or 2 5 lshift or 3 16 lshift or n+   \ fdiv d1,d2,d3
   $1E614000 1 or 2 5 lshift or n+          \ fneg
   $1E60C000 1 or 2 5 lshift or n+          \ fabs
   $1E61C000 1 or 2 5 lshift or n+          \ fsqrt
   $1E602000 1 5 lshift or 2 16 lshift or n+ \ fcmp d1,d2
   $1E602008 1 5 lshift or n+               \ fcmp d1,#0
   $9E620000 1 or 2 5 lshift or n+          \ scvtf d1,x2
   $9E780000 1 or 2 5 lshift or n+ ;        \ fcvtzs x1,d2
: GEN ( -- a u )
   0 CL !  s" src/arch/arm64/asm.f" +F
   s" : GO 1 2 ENC-FMOVDD . 1 2 3 ENC-FDIV . 1 2 ENC-FNEG . 1 2 ENC-FABS . " +B
   s" 1 2 ENC-FSQRT . 1 2 ENC-FCMP . 1 ENC-FCMP0 . 1 2 ENC-SCVTF . 1 2 ENC-FCVTZS . ; GO" +B
   CBUF CL @ NF-RUN  NFOUT 2@ ;
REF
T{ GEN  EB EL @ compare 0= -> true }T

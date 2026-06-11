\ walk.fs — token-driven code GENERATOR in the standalone (caf's memory-path):
\ compiles a Forth body string to native ARM64 using asm.fs encoders + icode.fs.
\ Data stack lives below sp in the generated code: x19=DSP (grows down, top at [x19]);
\ x9/x10 scratch. Each op's fixed instruction sequence is a DATA template (GTAB) so
\ the dispatcher stays small (the standalone inlines colon words -> a big switch would
\ overflow). Literals are emitted directly. Needs asm.fs + icode.fs loaded first.
create GTAB 3 c, 100 c, 117 c, 112 c, 3 c, 105 c, 2 c, 64 c, 249 c, 115 c, 34 c, 0 c, 209 c, 105 c, 2 c, 0 c, 249 c, 4 c, 100 c, 114 c, 111 c, 112 c, 1 c, 115 c, 34 c, 0 c, 145 c, 4 c, 115 c, 119 c, 97 c, 112 c, 4 c, 105 c, 2 c, 64 c, 249 c, 106 c, 6 c, 64 c, 249 c, 106 c, 2 c, 0 c, 249 c, 105 c, 6 c, 0 c, 249 c, 4 c, 111 c, 118 c, 101 c, 114 c, 3 c, 105 c, 6 c, 64 c, 249 c, 115 c, 34 c, 0 c, 209 c, 105 c, 2 c, 0 c, 249 c, 1 c, 43 c, 5 c, 106 c, 2 c, 64 c, 249 c, 115 c, 34 c, 0 c, 145 c, 105 c, 2 c, 64 c, 249 c, 41 c, 1 c, 10 c, 139 c, 105 c, 2 c, 0 c, 249 c, 1 c, 45 c, 5 c, 106 c, 2 c, 64 c, 249 c, 115 c, 34 c, 0 c, 145 c, 105 c, 2 c, 64 c, 249 c, 41 c, 1 c, 10 c, 203 c, 105 c, 2 c, 0 c, 249 c, 1 c, 42 c, 5 c, 106 c, 2 c, 64 c, 249 c, 115 c, 34 c, 0 c, 145 c, 105 c, 2 c, 64 c, 249 c, 41 c, 125 c, 10 c, 155 c, 105 c, 2 c, 0 c, 249 c, 3 c, 97 c, 110 c, 100 c, 5 c, 106 c, 2 c, 64 c, 249 c, 115 c, 34 c, 0 c, 145 c, 105 c, 2 c, 64 c, 249 c, 41 c, 1 c, 10 c, 138 c, 105 c, 2 c, 0 c, 249 c, 2 c, 111 c, 114 c, 5 c, 106 c, 2 c, 64 c, 249 c, 115 c, 34 c, 0 c, 145 c, 105 c, 2 c, 64 c, 249 c, 41 c, 1 c, 10 c, 170 c, 105 c, 2 c, 0 c, 249 c, 3 c, 120 c, 111 c, 114 c, 5 c, 106 c, 2 c, 64 c, 249 c, 115 c, 34 c, 0 c, 145 c, 105 c, 2 c, 64 c, 249 c, 41 c, 1 c, 10 c, 202 c, 105 c, 2 c, 0 c, 249 c, 0 c, 
\ read a little-endian u32 from byte addr p
variable RDP
: RD32 {: p :}  p c@  p 1 + c@ 8 lshift or  p 2 + c@ 16 lshift or  p 3 + c@ 24 lshift or ;
\ GFIND ( a u -- ) : if token (a,u) is in GTAB, emit its template words; sets GHIT.
variable GHIT  variable GP  variable GNL  variable GNP  variable GCNT  variable GWI
: GFIND {: a u :}  0 GHIT !  GTAB GP !
   BEGIN GP @ c@ dup WHILE                          \ nlen != 0
     GNL !  GP @ 1 + GNP !
     GNP @ GNL @ +  dup c@ GCNT !  1 +  RDP !        \ count, then words ptr in RDP
     a u GNP @ GNL @ STR= IF
       1 GHIT !
       0 GWI ! BEGIN GWI @ GCNT @ < WHILE  RDP @ GWI @ 4 * + RD32 EMITW  GWI @ 1 + GWI !  REPEAT
     THEN
     RDP @ GCNT @ 4 * +  GP !                        \ next entry
   REPEAT drop ;
\ parse a decimal token to a value (optional leading -)
variable PNV  variable PNI  variable PNN
: PARSE-NUM {: a u :}  0 PNV !  0 PNI !  1 PNN !
   a c@ 45 = IF -1 PNN !  1 PNI ! THEN
   BEGIN PNI @ u < WHILE  PNV @ 10 *  a PNI @ + c@ 48 -  +  PNV !  PNI @ 1 + PNI !  REPEAT
   PNV @ PNN @ * ;
\ emit a literal push (32-bit): sub x19,#8 ; movz/movk x9 ; str x9,[x19]
: GEN-LIT {: n :}
   19 19 8 ENC-SUBI EMITW
   9 n 65535 and 0 MOVZHW EMITW
   9 n 16 rshift 65535 and 1 MOVKHW EMITW
   9 19 0 ENC-STR EMITW ;
: DIG? {: c :} c 47 > c 58 < and ;
variable AD2
: ALLDIG2? {: a u :} u 0= IF 0 AD2 ! ELSE -1 AD2 ! 0 BEGIN dup u < WHILE dup a + c@ DIG? 0= IF 0 AD2 ! THEN 1 + REPEAT drop THEN AD2 @ ;
: GEN-TOK {: a u :}  a u ALLDIG2? IF a u PARSE-NUM GEN-LIT ELSE a u GFIND THEN ;
\ tokenize body and generate; wrap with prologue (x19=sp) and epilogue (exit top)
variable GB  variable GL  variable GI  variable GS
: GEN-BODY {: a u :}
   a GB !  u GL !
   19 31 0 ENC-ADDI EMITW                            \ add x19, sp, #0  (DSP = sp)
   0 GI !
   BEGIN GI @ GL @ < WHILE
     BEGIN GI @ GL @ < GB @ GI @ + c@ 32 = and WHILE GI @ 1 + GI ! REPEAT
     GI @ GL @ < IF
       GB @ GI @ + GS !
       BEGIN GI @ GL @ < GB @ GI @ + c@ 32 <> and WHILE GI @ 1 + GI ! REPEAT
       GS @ GB @ GI @ + GS @ - GEN-TOK
     THEN
   REPEAT
   0 19 0 ENC-LDR EMITW                              \ ldr x0,[x19]  (exit code = TOS)
   16 1 0 MOVZHW EMITW  0 ENC-SVC EMITW ;            \ movz x16,#1 ; svc 0

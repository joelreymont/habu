\ walk.fs — token-driven code GENERATOR in the standalone (habu's memory-path):
\ compiles a Forth body string to native ARM64 using asm.fs encoders + icode.fs.
\ Data stack lives below sp in the generated code: x19=DSP (grows down, top at [x19]);
\ x9/x10 scratch. Each op's fixed instruction sequence is a DATA template (GTAB) so
\ the dispatcher stays small (the standalone inlines colon words -> a big switch would
\ overflow). Literals are emitted directly. Needs asm.fs + icode.fs loaded first.
\ GTAB: records of [nlen, name, count, u32-words(LE)], 0-terminated. Built at load time
\ from the encoders (asm.fs) instead of a baked byte blob: GT< starts a record, GW+
\ appends one encoded instruction (count byte and 0 terminator maintained as it goes).
create GTAB 400 allot  variable GTP  variable GTN
: GT< {: a u :}
   u GTP @ c!
   0 BEGIN dup u < WHILE  dup a + c@  over GTP @ + 1 + c!  1 + REPEAT drop
   GTP @ 1 + u +  GTN !  0 GTN @ c!  GTN @ 1 + GTP !  0 GTP @ c! ;
: GW+ {: w :}
   w $FF and GTP @ c!  w 8 rshift $FF and GTP @ 1 + c!
   w 16 rshift $FF and GTP @ 2 + c!  w 24 rshift $FF and GTP @ 3 + c!
   GTP @ 4 + GTP !  GTN @ c@ 1 + GTN @ c!  0 GTP @ c! ;
: GPOP9   10 19 0 ENC-LDR GW+  19 19 8 ENC-ADDI GW+  9 19 0 ENC-LDR GW+ ;  \ x10=top, x9=next
: GPUSH9  9 19 0 ENC-STR GW+ ;                                             \ [x19]=x9 (in place)
: GTABLES  GTAB GTP !  0 GTAB c!
   s" dup"  GT<  9 19 0 ENC-LDR GW+  19 19 8 ENC-SUBI GW+  9 19 0 ENC-STR GW+
   s" drop" GT<  19 19 8 ENC-ADDI GW+
   s" swap" GT<  9 19 0 ENC-LDR GW+  10 19 8 ENC-LDR GW+
                 10 19 0 ENC-STR GW+  9 19 8 ENC-STR GW+
   s" over" GT<  9 19 8 ENC-LDR GW+  19 19 8 ENC-SUBI GW+  9 19 0 ENC-STR GW+
   s" +"    GT<  GPOP9  9 9 10 ENC-ADD GW+  GPUSH9
   s" -"    GT<  GPOP9  9 9 10 ENC-SUB GW+  GPUSH9
   s" *"    GT<  GPOP9  9 9 10 ENC-MUL GW+  GPUSH9
   s" and"  GT<  GPOP9  9 9 10 ENC-AND GW+  GPUSH9
   s" or"   GT<  GPOP9  9 9 10 ENC-ORR GW+  GPUSH9
   s" xor"  GT<  GPOP9  9 9 10 ENC-EOR GW+  GPUSH9 ;
GTABLES
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

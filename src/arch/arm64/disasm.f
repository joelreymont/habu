\ disasm.fs — a small ARM64 disassembler in the STANDALONE's Forth: decode a u32 to a
\ mnemonic + operands (printed via type/.). Data-table driven (the standalone inlines
\ colon words, so a big switch would overflow). Self-hosted debugging of generated code.
\ NROWS rows of [mask:8][val:8][kind:1][nlen:1][name]. kind: 0 RRR 1 RRI 2 MOVW 3 LS
\ 4 NONE 5 SVC 6 CMPR 8 CMPI 7 BR.
20 constant NROWS
create DTB 0 c, 0 c, 128 c, 255 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 128 c, 210 c, 0 c, 0 c, 0 c, 0 c, 2 c, 4 c, 109 c, 111 c, 118 c, 122 c, 0 c, 0 c, 128 c, 255 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 128 c, 242 c, 0 c, 0 c, 0 c, 0 c, 2 c, 4 c, 109 c, 111 c, 118 c, 107 c, 0 c, 0 c, 128 c, 255 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 128 c, 146 c, 0 c, 0 c, 0 c, 0 c, 2 c, 4 c, 109 c, 111 c, 118 c, 110 c, 0 c, 252 c, 224 c, 255 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 139 c, 0 c, 0 c, 0 c, 0 c, 0 c, 3 c, 97 c, 100 c, 100 c, 0 c, 252 c, 224 c, 255 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 203 c, 0 c, 0 c, 0 c, 0 c, 0 c, 3 c, 115 c, 117 c, 98 c, 0 c, 252 c, 224 c, 255 c, 0 c, 0 c, 0 c, 0 c, 0 c, 124 c, 0 c, 155 c, 0 c, 0 c, 0 c, 0 c, 0 c, 3 c, 109 c, 117 c, 108 c, 0 c, 252 c, 224 c, 255 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 138 c, 0 c, 0 c, 0 c, 0 c, 0 c, 3 c, 97 c, 110 c, 100 c, 0 c, 252 c, 224 c, 255 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 170 c, 0 c, 0 c, 0 c, 0 c, 0 c, 3 c, 111 c, 114 c, 114 c, 0 c, 252 c, 224 c, 255 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 202 c, 0 c, 0 c, 0 c, 0 c, 0 c, 3 c, 101 c, 111 c, 114 c, 0 c, 0 c, 128 c, 255 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 145 c, 0 c, 0 c, 0 c, 0 c, 1 c, 3 c, 97 c, 100 c, 100 c, 0 c, 0 c, 128 c, 255 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 209 c, 0 c, 0 c, 0 c, 0 c, 1 c, 3 c, 115 c, 117 c, 98 c, 0 c, 0 c, 192 c, 255 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 64 c, 249 c, 0 c, 0 c, 0 c, 0 c, 3 c, 3 c, 108 c, 100 c, 114 c, 0 c, 0 c, 192 c, 255 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 249 c, 0 c, 0 c, 0 c, 0 c, 3 c, 3 c, 115 c, 116 c, 114 c, 31 c, 252 c, 224 c, 255 c, 0 c, 0 c, 0 c, 0 c, 31 c, 0 c, 0 c, 235 c, 0 c, 0 c, 0 c, 0 c, 6 c, 3 c, 99 c, 109 c, 112 c, 255 c, 3 c, 128 c, 255 c, 0 c, 0 c, 0 c, 0 c, 31 c, 0 c, 0 c, 241 c, 0 c, 0 c, 0 c, 0 c, 8 c, 3 c, 99 c, 109 c, 112 c, 0 c, 0 c, 0 c, 255 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 180 c, 0 c, 0 c, 0 c, 0 c, 7 c, 3 c, 99 c, 98 c, 122 c, 0 c, 0 c, 0 c, 255 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 181 c, 0 c, 0 c, 0 c, 0 c, 7 c, 4 c, 99 c, 98 c, 110 c, 122 c, 0 c, 0 c, 0 c, 252 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 20 c, 0 c, 0 c, 0 c, 0 c, 7 c, 1 c, 98 c, 255 c, 255 c, 255 c, 255 c, 0 c, 0 c, 0 c, 0 c, 192 c, 3 c, 95 c, 214 c, 0 c, 0 c, 0 c, 0 c, 4 c, 3 c, 114 c, 101 c, 116 c, 31 c, 0 c, 224 c, 255 c, 0 c, 0 c, 0 c, 0 c, 1 c, 0 c, 0 c, 212 c, 0 c, 0 c, 0 c, 0 c, 5 c, 3 c, 115 c, 118 c, 99 c, 

: U64@ {: p :} p c@  p 1 + c@ 8 lshift or  p 2 + c@ 16 lshift or  p 3 + c@ 24 lshift or
   p 4 + c@ 32 lshift or  p 5 + c@ 40 lshift or  p 6 + c@ 48 lshift or  p 7 + c@ 56 lshift or ;

: FRD {: w :} w 31 and ;

: FRN {: w :} w 5 rshift 31 and ;

: FRM {: w :} w 16 rshift 31 and ;

: FI12 {: w :} w 10 rshift 4095 and ;

: FI16 {: w :} w 5 rshift 65535 and ;
create CHB 4 allot

: EMITC {: c :} c CHB c! CHB 1 type ;

: SPC 32 EMITC ;

: DROW-EMIT {: w kind :}
   kind 0 = IF w FRD . w FRN . w FRM . THEN
   kind 1 = IF w FRD . w FRN . w FI12 . THEN
   kind 2 = IF w FRD . w FI16 . THEN
   kind 3 = IF w FRD . w FRN . w FI12 8 * . THEN
   kind 6 = IF w FRN . w FRM . THEN
   kind 8 = IF w FRN . w FI12 . THEN
   kind 5 = IF w FI16 . THEN ;
\ NB: no locals inside the loop (corrupts the frame) — row fields go through variables.
variable DDONE  variable DRI  variable DRP  variable DMSK  variable DVAL  variable DKIND  variable DNL

: DIS1 {: w :}  DTB DRP !  0 DDONE !  0 DRI !
   BEGIN DRI @ NROWS < WHILE
     DRP @ U64@ DMSK !  DRP @ 8 + U64@ DVAL !
     DRP @ 16 + c@ DKIND !  DRP @ 17 + c@ DNL !
     DDONE @ 0= w DMSK @ and DVAL @ = and IF
       DRP @ 18 + DNL @ type SPC  w DKIND @ DROW-EMIT  1 DDONE !  THEN
     DRP @ 18 + DNL @ + DRP !  DRI @ 1 + DRI !
   REPEAT
   DDONE @ 0= IF 63 EMITC THEN ;

: DISASM {: a n :}  0 BEGIN dup n < WHILE  dup 4 * a + U64@ 4294967295 and DIS1  1 + REPEAT drop ;

\ icode.fs — minimal single-pass assembler in the STANDALONE's Forth: emit ARM64
\ words (asm.fs encoders) into a code buffer, define labels, and resolve B/CBZ/CBNZ
\ branches (backward immediately, forward via backpatch). Next codegen-port step
\ after the encoders. CP counts WORDS; deltas are word-relative (ARM64 PC-relative).
create CODE 16384 allot   variable CP
: ARESET 0 CP ! ;
: CW@ {: w :}  CODE w 4 * + ;                      \ byte addr of word w
\ NB: the standalone mis-reads a SECOND {: :} locals group, so these use a variable
\ for the byte pointer instead of a 2nd local (cf. VAR-OF / BR-EMIT bugs).
variable EP
: EMITW {: u :}  CP @ CW@ EP !                       \ store u LE at CODE[CP], CP++
   u 255 and EP @ c!  u 8 rshift 255 and EP @ 1 + c!  u 16 rshift 255 and EP @ 2 + c!  u 24 rshift 255 and EP @ 3 + c!
   CP @ 1 + CP ! ;
: PATCH {: u w :}  w CW@ EP !                        \ OR u into the word already at w (delta bits)
   u 255 and EP @ c@ or EP @ c!  u 8 rshift 255 and EP @ 1 + c@ or EP @ 1 + c!
   u 16 rshift 255 and EP @ 2 + c@ or EP @ 2 + c!  u 24 rshift 255 and EP @ 3 + c@ or EP @ 3 + c! ;
\ labels: LBLP[id] = defining word pos, or -1 if pending
create LBLP 256 cells allot   variable NLBL
\ fixups: site word-pos, target label, kind (0=B26, 1=cond/CBZ 19-bit)
create FXS 256 cells allot   create FXL 256 cells allot   create FXK 256 cells allot   variable NFX
: ASM-INIT  ARESET  0 NLBL !  0 NFX !  0 BEGIN dup cells LBLP + -1 swap ! 1 + dup 255 > UNTIL drop ;
: NEWLBL  NLBL @ dup 1 + NLBL ! ;                    \ -- id
: FX+ {: site lbl kind :}                            \ record a forward fixup
   site NFX @ cells FXS + !  lbl NFX @ cells FXL + !  kind NFX @ cells FXK + !  NFX @ 1 + NFX ! ;
\ encode a word delta into the branch word for a kind
: D26  16777215 and ;                                \ B/BL: bits 0..25
: D19  524287 and 5 lshift ;                         \ cond/CBZ: bits 5..23
\ emit a branch (base already encoded with delta=0) to a label; resolve or defer
variable BBASE  variable BKIND
: BR-EMIT {: lbl :}                                  \ BBASE/BKIND set; emits + records if fwd
   lbl cells LBLP + @  dup 0 < IF                     \ pos on stack (0< isn't a standalone prim)
     drop  CP @ lbl BKIND @ FX+  BBASE @ EMITW
   ELSE  CP @ -  BKIND @ 0= IF D26 ELSE D19 THEN  BBASE @ or EMITW  THEN ;
: B,    {: lbl :}  335544320  BBASE !  0 BKIND !  lbl BR-EMIT ;
: BL,   {: lbl :}  2483027968 BBASE !  0 BKIND !  lbl BR-EMIT ;
: BCOND, {: cond lbl :}  1409286144 cond or BBASE !  1 BKIND !  lbl BR-EMIT ;
: CBZ,  {: rt lbl :}  3019898880 rt or BBASE !  1 BKIND !  lbl BR-EMIT ;
: CBNZ, {: rt lbl :}  3036676096 rt or BBASE !  1 BKIND !  lbl BR-EMIT ;
\ define a label here; backpatch all pending fixups that target it
variable LBI
: LBL, {: lbl :}  CP @ lbl cells LBLP + !
   0 LBI ! BEGIN LBI @ NFX @ < WHILE
     LBI @ cells FXL + @ lbl = IF
       CP @ LBI @ cells FXS + @ -                    \ delta = here - site
       LBI @ cells FXK + @ 0= IF D26 ELSE D19 THEN
       LBI @ cells FXS + @ PATCH
     THEN
     LBI @ 1 + LBI !
   REPEAT ;
: ASM-LEN  CP @ 4 * ;

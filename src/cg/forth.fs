\ forth.fs — emit a STANDALONE native Forth (no gforth, no C). Subroutine-threaded,
\ PC-relative (PIE-safe). Stage 1: a dictionary of native primitives + an outer
\ interpreter that number-pushes / FINDs+EXECUTEs tokens from an embedded source.
\ Stage 2 (this file): a runtime `:`/`;` compiler that JITs new words into an
\ mmap'd region by INLINING stencils — each token's machine code (a primitive's
\ body, or a prior word's body, both minus their trailing RET) is memcpy'd into
\ the new word, so compiled words are fully flattened/leaf and need no calls.
\ Literals compile to a movz/movk + push stencil. W^X: the region is mmap'd RW,
\ toggled RW->RX at `;` (mprotect + DC CVAU / IC IVAU flush) so the word is
\ callable, and back to RW at the next `:`.  See docs/forth.md, LESSONS.md.
\
\ Registers (emitted program):
\   x19=XDS data stack   x20=RBASE __TEXT base   x21=INP  x22=INE  (input cursor/end)
\   x23=TKA  x24=TKL (current token)   x25=PEND (dict slot being compiled; 0=interpret)
\   x26=DBASE mmap region = runtime dict base   x27=NDICT live word count   x28=CP compile ptr
\   x9-x15,x5-x8,x0-x4,x16 scratch.

require exec.fs
require templ.fs           \ g-push, XDS(=19)
require rt.fs              \ g-print9 (shared signed-decimal printer)

20 constant RBASE   21 constant INP    22 constant INE   23 constant TKA   24 constant TKL
25 constant PEND    26 constant DBASE  27 constant NDICT  28 constant CP

$100000 constant REGION       \ mmap region size (1 MB)
$10000  constant DICT-SIZE     \ dict area at region+0 (64 KB); code area follows
40      constant DREC          \ dict record: addr(8) clen(8) namelen(8) name(16)
$F000   constant CFSTK-OFF     \ control-flow stack: cell[0]=CFSP, cells[1..]=addrs
$80000  constant DATA-SIZE     \ data-space mmap (always RW, separate from the RX code region)
$100000 constant IBUFSZ        \ stdin read buffer (1 MB)
\ x20 (RBASE) is dead after startup, so it doubles as DATA: the data-space base.
\ [x20] holds DP (next-free pointer); usable space is [x20+8 .. x20+DATA-SIZE).
20 constant DATA
create SQ-KW  115 c, 34 c,      \ build-time bytes for the keyword  s"  (s=115, "=34)
variable STDIN?   STDIN? off   \ source mode: baked Lsrc (off) vs read from stdin (on)

\ runtime instruction-word constants the JIT compiler stamps out (verified encodings)
$D65F03C0 constant W-RET
$F9000269 constant W-PUSH0     \ str  x9,[x19,#0]
$91002273 constant W-PUSH1     \ add  x19,x19,#8
$D2800009 constant W-MOVZ0     \ movz x9,#0
$F2A00009 constant W-MOVK1     \ movk x9,#0,lsl#16
$F2C00009 constant W-MOVK2     \ movk x9,#0,lsl#32
$F2E00009 constant W-MOVK3     \ movk x9,#0,lsl#48

\ --- primitive registry (host-side, to build the seed dictionary) ---
create PLBL 64 cells allot   create PEL 64 cells allot
create PLEN 64 cells allot   create PNAM 64 cells allot
create PNPOOL 1024 chars allot   variable PNP   variable #PL
: reg-prim {: na nu lbl elbl -- :}
   lbl  #PL @ cells PLBL + !
   elbl #PL @ cells PEL  + !
   nu   #PL @ cells PLEN + !
   PNPOOL PNP @ +  {: dst :}   dst #PL @ cells PNAM + !
   na dst nu move   nu PNP +!   1 #PL +! ;
: FPRIM {: na nu xt -- :}            \ define+register a primitive (start..RET..end labels)
   NEWLBL {: lbl :}  NEWLBL {: elbl :}
   na nu lbl elbl reg-prim
   lbl LBL,  xt execute  RET,  elbl LBL, ;

\ shared label ids (forward refs)
variable Lanchor  variable Lfind  variable Lnum  variable Ldict  variable Lsrc  variable SRCN
variable Lcemit   variable Ltok   variable Lprot  variable Lflush variable Lncount
\ control-flow JIT helpers + keyword data labels (self-host 1b)
variable Lcfpush  variable Lcfpop  variable Lpat   variable Lkwcmp
variable Lkwif    variable Lkwthen variable Lkwelse variable Lkwbegin
variable Lkwuntil variable Lkwagain variable Lkwwhile variable Lkwrepeat
variable Lkwcreate variable Lkwvar variable Lkwsq

9 constant A   10 constant B   11 constant C
\ ---- primitive bodies (ICode operating on the x19 data stack) ----
: b+   B g-pop  A g-pop  A A B ADD,  A g-push ;
: b-   B g-pop  A g-pop  A A B SUB,  A g-push ;
: b*   B g-pop  A g-pop  A A B MUL,  A g-push ;
: bdup  A g-pop  A g-push  A g-push ;
: bdrop XDS XDS 8 SUBI, ;
: bswap A g-pop  B g-pop  A g-push  B g-push ;
: bdot  A g-pop  g-print9 ;          \ pop x9, print signed decimal + newline

\ comparisons -> Forth flag 0/-1 (CSET 0/1 then negate via the zero register SP)
: (cmp) {: cond -- :}  B g-pop  A g-pop  A B CMP,  A cond CSET,  A SP A SUB,  A g-push ;
: b=  C-EQ (cmp) ;   : b<> C-NE (cmp) ;   : b<  C-LT (cmp) ;   : b>  C-GT (cmp) ;
: b<= C-LE (cmp) ;   : b>= C-GE (cmp) ;
: b0= A g-pop  A 0 CMPI,  A C-EQ CSET,  A SP A SUB,  A g-push ;
\ bitwise / logic
: band B g-pop A g-pop  A A B AND, A g-push ;
: bor  B g-pop A g-pop  A A B ORR, A g-push ;
: bxor B g-pop A g-pop  A A B EOR, A g-push ;
: binv A g-pop  B 0 MOVN,  A A B EOR,  A g-push ;     \ A ^ -1
: bneg A g-pop  A SP A SUB,  A g-push ;               \ 0 - A
\ shifts (variable count); /, mod via SDIV/MUL
: blsh B g-pop A g-pop  A A B LSLV, A g-push ;
: brsh B g-pop A g-pop  A A B LSRV, A g-push ;
: bdiv B g-pop A g-pop  A A B SDIV, A g-push ;
: bmod B g-pop A g-pop  C A B SDIV,  C C B MUL,  A A C SUB,  A g-push ;
\ stack shuffles (memory on x19)
: bnip  A g-pop  XDS XDS 8 SUBI,  A g-push ;
: bover B g-pop A g-pop  A g-push B g-push A g-push ;
: btuck B g-pop A g-pop  B g-push A g-push B g-push ;
: brot  C g-pop B g-pop A g-pop  B g-push C g-push A g-push ;
: bmrot C g-pop B g-pop A g-pop  C g-push A g-push B g-push ;
: b2dup B g-pop A g-pop  A g-push B g-push A g-push B g-push ;
: b2drop XDS XDS 16 SUBI, ;
\ memory access (absolute addresses on the stack)
: bfetch  A g-pop  A A 0 LDR,  A g-push ;
: bstore  B g-pop A g-pop  A B 0 STR, ;               \ ( val addr -- )
: bcfetch A g-pop  A A 0 LDRB, A g-push ;
: bcstore B g-pop A g-pop  A B 0 STRB, ;
: bcells  A g-pop  A A 3 LSLI, A g-push ;             \ n*8
\ data space: DP cell is [x20]; HERE/ALLOT/,/C, bump it (x20 region is always RW)
: bhere   7 DATA 0 LDR,  7 g-push ;
: ballot  A g-pop  7 DATA 0 LDR,  7 7 A ADD,  7 DATA 0 STR, ;
: bcomma  A g-pop  7 DATA 0 LDR,  A 7 0 STR,  7 7 8 ADDI,  7 DATA 0 STR, ;
: bccomma A g-pop  7 DATA 0 LDR,  A 7 0 STRB, 7 7 1 ADDI,  7 DATA 0 STR, ;
: btype   2 g-pop  1 g-pop  0 1 MOVZ,  16 4 MOVZ,  $80 SVC, ;   \ ( addr len -- ) write(1,..)

: emit-prims ( -- )
   s" +"    ['] b+    FPRIM   s" -"    ['] b-    FPRIM   s" *"    ['] b*    FPRIM
   s" dup"  ['] bdup  FPRIM   s" drop" ['] bdrop FPRIM   s" swap" ['] bswap FPRIM
   s" ."    ['] bdot  FPRIM
   s" ="    ['] b=    FPRIM   s" <>"   ['] b<>   FPRIM   s" <"    ['] b<    FPRIM
   s" >"    ['] b>    FPRIM   s" <="   ['] b<=   FPRIM   s" >="   ['] b>=   FPRIM
   s" 0="   ['] b0=   FPRIM
   s" and"  ['] band  FPRIM   s" or"   ['] bor   FPRIM   s" xor"  ['] bxor  FPRIM
   s" invert" ['] binv FPRIM  s" negate" ['] bneg FPRIM
   s" lshift" ['] blsh FPRIM  s" rshift" ['] brsh FPRIM
   s" /"    ['] bdiv  FPRIM   s" mod"  ['] bmod  FPRIM
   s" nip"  ['] bnip  FPRIM   s" over" ['] bover FPRIM   s" tuck" ['] btuck FPRIM
   s" rot"  ['] brot  FPRIM   s" -rot" ['] bmrot FPRIM
   s" 2dup" ['] b2dup FPRIM   s" 2drop" ['] b2drop FPRIM
   s" @"    ['] bfetch FPRIM   s" !"    ['] bstore FPRIM
   s" c@"   ['] bcfetch FPRIM  s" c!"   ['] bcstore FPRIM
   s" cells" ['] bcells FPRIM
   s" here" ['] bhere  FPRIM   s" allot" ['] ballot FPRIM
   s" ,"    ['] bcomma FPRIM   s" c,"   ['] bccomma FPRIM
   s" type" ['] btype  FPRIM ;

\ ---- CEMIT ( x9=word -- ) : str w9,[x28] ; CP += 4 ----
: emit-cemit ( -- )
   Lcemit @ LBL,  9 28 0 STRW,  28 28 4 ADDI,  RET, ;

\ ---- TOK ( -- x0=have? ) : skip spaces, scan one token into TKA/TKL, advance INP ----
: emit-tok ( -- )
   Ltok @ LBL,
   NEWLBL {: tskip :}  NEWLBL {: thas :}  NEWLBL {: tscan :}
   NEWLBL {: tgot :}   NEWLBL {: tnone :}
   tskip LBL,                                          \ skip whitespace (any byte <= 32)
      INP INE CMP,  C-GE tnone BCOND,
      9 INP 0 LDRB,  9 32 CMPI,  C-HI thas BCOND,      \ c > 32 -> token start
      INP INP 1 ADDI,  tskip B,
   thas LBL,  TKA INP 0 ADDI,
   tscan LBL,                                          \ scan to next whitespace
      INP INE CMP,  C-GE tgot BCOND,
      9 INP 0 LDRB,  9 32 CMPI,  C-LS tgot BCOND,      \ c <= 32 -> token end
      INP INP 1 ADDI,  tscan B,
   tgot LBL,  TKL INP TKA SUB,  0 1 MOVZ,  RET,
   tnone LBL,  0 0 MOVZ,  RET, ;

\ ---- PROT ( x2=prot -- ) : mprotect(region, REGION, prot) ----
: emit-prot ( -- )
   Lprot @ LBL,
   0 DBASE 0 ADDI,  1 REGION LIT64,  16 74 MOVZ,  $80 SVC,  RET, ;

\ ---- FLUSH ( -- ) : DC CVAU + IC IVAU over the code area [region+DICT-SIZE, CP) ----
: emit-flush ( -- )
   Lflush @ LBL,
   9 DBASE 0 ADDI,  5 DICT-SIZE LIT64,  9 9 5 ADD,          \ x9 = code start
   NEWLBL {: fdl :}  NEWLBL {: fdd :}  NEWLBL {: fil :}  NEWLBL {: fid :}
   10 9 0 ADDI,
   fdl LBL,  10 CP CMP,  C-GE fdd BCOND,  10 DCCVAU,  10 10 64 ADDI,  fdl B,
   fdd LBL,  DSB-ISH,
   10 9 0 ADDI,
   fil LBL,  10 CP CMP,  C-GE fid BCOND,  10 ICIVAU,  10 10 64 ADDI,  fil B,
   fid LBL,  DSB-ISH,  ISB,  RET, ;

\ ---- FIND ( x9=tka x10=tkl -- x11=addr x12=clen x13=found ) over 40-byte records ----
: emit-find ( -- )
   Lfind @ LBL,
   5 DBASE 0 ADDI,  6 NDICT 0 ADDI,  13 0 MOVZ,           \ rec, remaining, found=0
   NEWLBL {: floop :}  NEWLBL {: fdone :}  NEWLBL {: fnext :}
   NEWLBL {: fcmp :}   NEWLBL {: fmatch :}
   floop LBL,
      6 fdone CBZ,
      14 5 16 LDR,  14 10 CMP,  C-NE fnext BCOND,         \ namelen != tkl
      7 0 MOVZ,                                            \ i=0
      fcmp LBL,
         7 10 CMP,  C-GE fmatch BCOND,
         15 5 24 ADDI,  15 15 7 ADD,  15 15 0 LDRB,        \ rec.name[i]
         3 15 $41 SUBI,  3 26 CMPI,  3 C-CC CSET,  3 3 5 LSLI,  15 15 3 ORR,  \ fold A-Z->a-z
         4 9 7 ADD,     4 4 0 LDRB,                         \ tok[i]
         3 4 $41 SUBI,   3 26 CMPI,  3 C-CC CSET,  3 3 5 LSLI,  4 4 3 ORR,     \ fold A-Z->a-z
         15 4 CMP,  C-NE fnext BCOND,
         7 7 1 ADDI,  fcmp B,
      fmatch LBL,
         11 5 0 LDR,  12 5 8 LDR,  13 1 MOVZ,  fdone B,    \ addr, clen, found=1
      fnext LBL,  5 5 DREC ADDI,  6 6 1 SUBI,  floop B,
   fdone LBL,  RET, ;

\ ---- NUMBER? ( x9=tka x10=tkl -- x11=val x12=ok ) ----
: emit-num ( -- )
   Lnum @ LBL,
   11 0 MOVZ,  13 1 MOVZ,  14 0 MOVZ,  12 0 MOVZ,
   NEWLBL {: ldone :}
   10 ldone CBZ,
   15 9 0 LDRB,  15 45 CMPI,
   NEWLBL {: lloop :}  C-NE lloop BCOND,
   13 0 MOVN,  14 1 MOVZ,  14 10 CMP,  C-EQ ldone BCOND,    \ "-" only -> fail
   lloop LBL,
   NEWLBL {: lok :}
   14 10 CMP,  C-GE lok BCOND,
   5 9 14 ADD,  15 5 0 LDRB,
   15 48 CMPI,  C-LT ldone BCOND,
   15 57 CMPI,  C-GT ldone BCOND,
   15 15 48 SUBI,  5 10 MOVZ,  11 11 5 MUL,  11 11 15 ADD,
   14 14 1 ADDI,  lloop B,
   lok LBL,  11 11 13 MUL,  12 1 MOVZ,
   ldone LBL,  RET, ;

\ ---- seed dictionary: NPRIMS records of [startoff(8) endoff(8) namelen(8) name(16)] ----
: emit-dict ( -- )
   Lncount @ LBL,  #PL @ DCQ,                              \ live count, read at startup
   Ldict @ LBL,
   #PL @ 0 ?do
      i cells PLBL + @ DLBL,                                \ +0  start byte-offset
      i cells PEL  + @ DLBL,                                \ +8  end   byte-offset
      i cells PLEN + @ DCQ,                                 \ +16 name length
      i cells PNAM + @  i cells PLEN + @  BYTES,            \ +24 name (padded to 4)
      16  i cells PLEN + @  3 + -4 and  -  ?dup if  PNPOOL  swap BYTES, then
   loop ;

\ ---- compile-mode literal: emit movz/movk x9=val then the push stencil ----
: c-lit ( -- )   \ val in x11 at runtime; T0 register in JIT code is x9
   6 11 0 ADDI,  5 $FFFF MOVZ,
   7 6 5 AND,    7 7 5 LSLI,  8 W-MOVZ0 LIT64,  9 8 7 ORR,  Lcemit @ BL,
   7 6 16 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK1 LIT64,  9 8 7 ORR,  Lcemit @ BL,
   7 6 32 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK2 LIT64,  9 8 7 ORR,  Lcemit @ BL,
   7 6 48 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK3 LIT64,  9 8 7 ORR,  Lcemit @ BL,
   9 W-PUSH0 LIT64,  Lcemit @ BL,  9 W-PUSH1 LIT64,  Lcemit @ BL, ;

\ ---- compile-mode inline copy: CCOPY( x9=src, x10=nbytes ) word-at-a-time ----
: c-copy ( -- )
   NEWLBL {: cl :}  NEWLBL {: cd :}
   10 cd CBZ,
   cl LBL,  11 9 0 LDRW,  11 28 0 STRW,  9 9 4 ADDI,  28 28 4 ADDI,  10 10 4 SUBI,
            10 cl CBNZ,
   cd LBL, ;

\ ---- source setup: point INP/INE at either the baked Lsrc or stdin ----
\ stdin mode reads all of fd 0 into a fresh RW mmap buffer, then interprets it
\ (batch REPL: `echo ': SQ DUP * ; 5 SQ .' | ./forth`). Clobbers x0-x5,x9,x11,x16.
: emit-source ( -- )
   STDIN? @ if
      0 0 MOVZ,  1 IBUFSZ LIT64,  2 3 MOVZ,  3 $1002 LIT64,  4 0 MOVN,  5 0 MOVZ,
      16 197 MOVZ,  $80 SVC,                       \ mmap RW input buffer -> x0
      11 0 0 ADDI,  9 0 0 ADDI,                    \ x11 = base, x9 = write ptr
      NEWLBL {: rl :}  NEWLBL {: rd :}
      rl LBL,
         0 0 MOVZ,  1 9 0 ADDI,                    \ read(fd=0, buf=ptr, …)
         2 11 0 ADDI,  5 IBUFSZ LIT64,  2 2 5 ADD,  2 2 9 SUB,   \ count = base+SZ-ptr
         2 rd CBZ,                                 \ buffer full -> done
         16 3 MOVZ,  $80 SVC,                      \ -> x0 = n
         0 rd CBZ,                                 \ EOF (n=0) -> done
         9 9 0 ADD,  rl B,                         \ ptr += n
      rd LBL,
      INP 11 0 ADDI,  INE 9 0 ADDI,                \ INP=base, INE=ptr
   else
      INP Lsrc @ ADR,  INE Lsrc @ ADR,  INE INE SRCN @ ADDI,
   then ;

\ ---- control-flow JIT: a CF stack (region+CFSTK-OFF) of placeholder branch
\ addresses; THEN/ELSE/REPEAT patch the recorded branch's relative offset. ----
\ Lcfpush(x9=val), Lcfpop(->x9), Lpat(x9=addr: patch CBZ/B to current CP),
\ Lkwcmp(x0=kwaddr x1=kwlen -> x0=match? vs TKA/TKL, case-folded).
: emit-cf-helpers ( -- )
   Lcfpush @ LBL,
      5 CFSTK-OFF LIT64,  10 DBASE 5 ADD,  11 10 0 LDR,
      12 11 3 LSLI,  12 12 10 ADD,  12 12 8 ADDI,  9 12 0 STR,
      11 11 1 ADDI,  11 10 0 STR,  RET,
   Lcfpop @ LBL,
      5 CFSTK-OFF LIT64,  10 DBASE 5 ADD,  11 10 0 LDR,  11 11 1 SUBI,  11 10 0 STR,
      12 11 3 LSLI,  12 12 10 ADD,  12 12 8 ADDI,  9 12 0 LDR,  RET,
   Lpat @ LBL,                                       \ patch imm19 (CBZ) / imm26 (B)
      11 9 0 LDRW,  10 CP 9 SUB,  10 10 2 ASRI,
      5 $80000000 LIT64,  13 11 5 AND,
      NEWLBL {: pisb :}  NEWLBL {: pdone :}
      13 pisb CBZ,                                    \ bit31==0 -> B (imm26)
         5 $7FFFF LIT64,  10 10 5 AND,  10 10 5 LSLI,  pdone B,
      pisb LBL,  5 $3FFFFFF LIT64,  10 10 5 AND,
      pdone LBL,  11 11 10 ORR,  11 9 0 STRW,  RET,
   Lkwcmp @ LBL,
      NEWLBL {: kno :}  NEWLBL {: kyes :}  NEWLBL {: kchk :}
      TKL 1 CMP,  C-NE kno BCOND,
      2 0 MOVZ,  3 $20 MOVZ,
      kchk LBL,
         2 1 CMP,  C-GE kyes BCOND,
         4 TKA 2 ADD,  4 4 0 LDRB,  4 4 3 ORR,        \ fold token byte to lower
         5 0 2 ADD,    5 5 0 LDRB,                    \ keyword byte (stored lower)
         4 5 CMP,  C-NE kno BCOND,
         2 2 1 ADDI,  kchk B,
      kyes LBL,  0 1 MOVZ,  RET,
      kno  LBL,  0 0 MOVZ,  RET, ;

\ keyword bytes (lower-case) at known labels; ADR reaches them PC-relative
: emit-kwdata ( -- )
   Lkwif @ LBL,     s" if"     BYTES,    Lkwthen @ LBL,   s" then"   BYTES,
   Lkwelse @ LBL,   s" else"   BYTES,    Lkwbegin @ LBL,  s" begin"  BYTES,
   Lkwuntil @ LBL,  s" until"  BYTES,    Lkwagain @ LBL,  s" again"  BYTES,
   Lkwwhile @ LBL,  s" while"  BYTES,    Lkwrepeat @ LBL, s" repeat" BYTES,
   Lkwcreate @ LBL, s" create" BYTES,    Lkwvar @ LBL,    s" variable" BYTES,
   Lkwsq @ LBL,     SQ-KW 2 BYTES, ;                       \ the 2 bytes  s "

\ compile-time handler emitters (run at BUILD time, append JIT-emitter ICode)
: c-emitw  ( word -- )  9 swap LIT64,  Lcemit @ BL, ;          \ emit one fixed instr word
: c-popflag ( -- )  $D1002273 c-emitw  $F9400269 c-emitw ;     \ sub x19,#8 ; ldr x9,[x19]
: c-pushcp ( -- )   9 CP 0 ADDI,  Lcfpush @ BL, ;              \ push current CP
: c-bback {: opc mask -- :}                                    \ branch opc back to x9 target
   10 9 CP SUB,  10 10 2 ASRI,  5 mask LIT64,  10 10 5 AND,  9 opc LIT64,  9 9 10 ORR,  Lcemit @ BL, ;
: c-if    c-popflag  c-pushcp  $B4000009 c-emitw ;             \ pop flag; cbz fwd (patched by THEN)
: c-then  Lcfpop @ BL,  Lpat @ BL, ;
: c-else  Lcfpop @ BL,  14 9 0 ADDI,  c-pushcp  $14000000 c-emitw  9 14 0 ADDI,  Lpat @ BL, ;
: c-begin c-pushcp ;
: c-again Lcfpop @ BL,  $14000000 $3FFFFFF c-bback ;
: c-until Lcfpop @ BL,  14 9 0 ADDI,  c-popflag
   10 14 CP SUB,  10 10 2 ASRI,  5 $7FFFF LIT64,  10 10 5 AND,  10 10 5 LSLI,
   9 $B4000009 LIT64,  9 9 10 ORR,  Lcemit @ BL, ;
: c-while c-popflag  c-pushcp  $B4000009 c-emitw ;
: c-repeat Lcfpop @ BL,  14 9 0 ADDI,  Lcfpop @ BL,  $14000000 $3FFFFFF c-bback
   9 14 0 ADDI,  Lpat @ BL, ;

\ CREATE/VARIABLE (interpret-mode defining words): make a dict word whose body
\ pushes the current DP (a data-space address). Reuses the `:` slot pattern + the
\ c-lit emitter (with x11 = DP) for the literal-push body.
: c-create ( -- )
   2 3 MOVZ,  Lprot @ BL,                               \ region -> RW
   Ltok @ BL,                                            \ read NAME
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,   \ slot
   CP 9 0 STR,  TKL 9 16 STR,                            \ slot.addr=CP, namelen
   10 9 24 ADDI,  11 TKA 0 ADDI,  12 TKL 0 ADDI,         \ copy name
   NEWLBL {: ncp :}  NEWLBL {: ncpd :}
   ncp LBL,  12 ncpd CBZ,  13 11 0 LDRB,  13 10 0 STRB,
      10 10 1 ADDI,  11 11 1 ADDI,  12 12 1 SUBI,  ncp B,
   ncpd LBL,
   11 DATA 0 LDR,                                        \ x11 = DP (body pushes it)
   c-lit                                                 \ emit movz/movk x9=DP + push
   9 W-RET LIT64,  Lcemit @ BL,                          \ emit RET
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,   \ slot again
   10 9 0 LDR,  10 CP 10 SUB,  10 10 4 SUBI,  10 9 8 STR,        \ clen = CP-addr-4
   NDICT NDICT 1 ADDI,
   2 5 MOVZ,  Lprot @ BL,  Lflush @ BL, ;               \ region -> RX + flush
: c-variable ( -- )  c-create
   7 DATA 0 LDR,  7 7 8 ADDI,  7 DATA 0 STR, ;          \ reserve 1 cell

\ S" string" (compile mode): emit  B over the bytes ; <bytes> ; push abs-addr ;
\ push len. Bytes live in the RX code image; the absolute address is known at
\ compile time, so c-lit pushes it (no PC-relative ADR needed).
: c-sdq ( -- )
   INP INP 1 ADDI,  13 INP 0 ADDI,                      \ skip one space; x13 = start
   NEWLBL {: sl :}  NEWLBL {: sd :}
   sl LBL,  9 INP 0 LDRB,  9 $22 CMPI,  C-EQ sd BCOND,  INP INP 1 ADDI,  sl B,
   sd LBL,  10 INP 13 SUB,  INP INP 1 ADDI,             \ x10 = len; skip closing "
   15 CP 0 ADDI,  9 $14000000 LIT64,  Lcemit @ BL,      \ x15 = B addr; emit B placeholder
   12 CP 0 ADDI,                                        \ x12 = byte addr (after the B)
   11 13 0 ADDI,  9 10 0 ADDI,                          \ copy x10 bytes start->CP
   NEWLBL {: cl :}  NEWLBL {: cd :}
   cl LBL,  9 cd CBZ,
      14 11 0 LDRB,  14 28 0 STRB,  28 28 1 ADDI,  11 11 1 ADDI,  9 9 1 SUBI,  cl B,
   cd LBL,
   28 28 3 ADDI,  5 -4 LIT64,  28 28 5 AND,             \ pad CP to 4
   9 15 0 ADDI,  15 10 0 ADDI,  Lpat @ BL,              \ x9=B addr; save len in x15; patch B->here
   11 12 0 ADDI,  c-lit                                 \ push byte addr (x12)
   11 15 0 ADDI,  c-lit ;                               \ push len (x15)

\ emit one compile-mode keyword case: if TKA/TKL == kw, run handler then back to lmain
: cf-entry {: lmainlbl kwvar kwlen hxt -- :}
   0 kwvar @ ADR,  1 kwlen MOVZ,  Lkwcmp @ BL,
   NEWLBL {: skip :}  0 skip CBZ,
   hxt execute  lmainlbl B,
   skip LBL, ;

\ ---- MAIN: startup (data stack + mmap + seed dict) then the outer interpreter ----
: emit-main ( -- )
   Lanchor @ LBL,
   RBASE Lanchor @ ADR,                              \ x20 = __TEXT base
   SP SP 2048 SUBI,  XDS SP 0 ADDI,                  \ data stack on machine sp
   \ mmap(0, REGION, PROT_READ|WRITE=3, MAP_ANON|MAP_PRIVATE=0x1002, -1, 0)
   0 0 MOVZ,  1 REGION LIT64,  2 3 MOVZ,  3 $1002 LIT64,  4 0 MOVN,  5 0 MOVZ,
   16 197 MOVZ,  $80 SVC,
   DBASE 0 0 ADDI,                                    \ x26 = region
   CP DBASE 0 ADDI,  5 DICT-SIZE LIT64,  CP CP 5 ADD, \ x28 = region + DICT-SIZE
   \ seed runtime dict from build-time dict (convert offsets -> absolute addr + clen)
   11 Lncount @ ADR,  11 11 0 LDR,  NDICT 11 0 ADDI,  \ x27 = NDICT = seed count
   9 Ldict @ ADR,  10 DBASE 0 ADDI,  12 11 0 ADDI,    \ src, dst, i
   NEWLBL {: scopy :}  NEWLBL {: scdone :}
   scopy LBL,
      12 scdone CBZ,
      5 9 0 LDR,  6 9 8 LDR,                          \ startoff, endoff
      7 RBASE 5 ADD,  7 10 0 STR,                     \ addr = RBASE+startoff
      6 6 5 SUB,  6 6 4 SUBI,  6 10 8 STR,            \ clen = endoff-startoff-4
      5 9 16 LDR,  5 10 16 STR,                       \ namelen
      5 9 24 LDR,  5 10 24 STR,  5 9 32 LDR,  5 10 32 STR,  \ name[0..15]
      9 9 DREC ADDI,  10 10 DREC ADDI,  12 12 1 SUBI,  scopy B,
   scdone LBL,
   \ separate always-RW data region (x20 is free after the seed copy); [x20]=DP=x20+8
   0 0 MOVZ,  1 DATA-SIZE LIT64,  2 3 MOVZ,  3 $1002 LIT64,  4 0 MOVN,  5 0 MOVZ,
   16 197 MOVZ,  $80 SVC,  DATA 0 0 ADDI,
   7 DATA 8 ADDI,  7 DATA 0 STR,
   emit-source                                        \ INP/INE <- baked Lsrc or stdin
   PEND 0 MOVZ,                                       \ interpret mode
   NEWLBL {: lmain :}  NEWLBL {: lexit :}  NEWLBL {: lcompile :}
   lmain LBL,
      Ltok @ BL,  0 lexit CBZ,
      PEND lcompile CBNZ,
      \ ---------------- INTERPRET ----------------
      NEWLBL {: lnotcolon :}
      TKL 1 CMPI,  C-NE lnotcolon BCOND,
      9 TKA 0 LDRB,  9 58 CMPI,  C-NE lnotcolon BCOND,     \ ':'
         2 3 MOVZ,  Lprot @ BL,                             \ region -> RW *before* any write
         Ltok @ BL,                                         \ read NAME
         9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,  \ slot
         PEND 9 0 ADDI,
         CP 9 0 STR,  TKL 9 16 STR,                         \ slot.addr=CP, slot.namelen
         10 9 24 ADDI,  11 TKA 0 ADDI,  12 TKL 0 ADDI,      \ copy name
         NEWLBL {: ncopy :}  NEWLBL {: ncd :}
         ncopy LBL,  12 ncd CBZ,
            13 11 0 LDRB,  13 10 0 STRB,
            10 10 1 ADDI,  11 11 1 ADDI,  12 12 1 SUBI,  ncopy B,
         ncd LBL,
         5 CFSTK-OFF LIT64,  11 DBASE 5 ADD,  12 0 MOVZ,  12 11 0 STR,   \ reset CFSP
         lmain B,
      lnotcolon LBL,
      \ interpret-mode defining words
      lmain Lkwcreate 6 ['] c-create   cf-entry
      lmain Lkwvar    8 ['] c-variable cf-entry
      9 TKA 0 ADDI,  10 TKL 0 ADDI,  Lnum @ BL,             \ NUMBER?
      NEWLBL {: lnotnum :}
      12 lnotnum CBZ,  11 g-push  lmain B,
      lnotnum LBL,
      9 TKA 0 ADDI,  10 TKL 0 ADDI,  Lfind @ BL,            \ FIND
      13 lmain CBZ,                                          \ unknown -> skip
      11 BLR,  lmain B,                                      \ EXECUTE
      \ ---------------- COMPILE ----------------
   lcompile LBL,
      NEWLBL {: lnotsemi :}
      TKL 1 CMPI,  C-NE lnotsemi BCOND,
      9 TKA 0 LDRB,  9 59 CMPI,  C-NE lnotsemi BCOND,       \ ';'
         9 W-RET LIT64,  Lcemit @ BL,                       \ emit RET
         9 PEND 0 LDR,  10 CP 9 SUB,  10 10 4 SUBI,  10 PEND 8 STR,  \ clen
         NDICT NDICT 1 ADDI,                                \ publish word
         PEND 0 MOVZ,                                       \ leave compile mode
         2 5 MOVZ,  Lprot @ BL,  Lflush @ BL,               \ region -> RX + flush
         lmain B,
      lnotsemi LBL,
      \ control-flow keywords (compile-only): emit/patch JIT branches, then loop
      lmain Lkwif     2 ['] c-if     cf-entry
      lmain Lkwthen   4 ['] c-then   cf-entry
      lmain Lkwelse   4 ['] c-else   cf-entry
      lmain Lkwbegin  5 ['] c-begin  cf-entry
      lmain Lkwuntil  5 ['] c-until  cf-entry
      lmain Lkwagain  5 ['] c-again  cf-entry
      lmain Lkwwhile  5 ['] c-while  cf-entry
      lmain Lkwrepeat 6 ['] c-repeat cf-entry
      lmain Lkwsq     2 ['] c-sdq    cf-entry            \ S" string"
      9 TKA 0 ADDI,  10 TKL 0 ADDI,  Lnum @ BL,             \ NUMBER? -> literal
      NEWLBL {: lcnotnum :}
      12 lcnotnum CBZ,  c-lit  lmain B,
      lcnotnum LBL,
      9 TKA 0 ADDI,  10 TKL 0 ADDI,  Lfind @ BL,            \ FIND -> inline stencil
      13 lmain CBZ,
      9 11 0 ADDI,  10 12 0 ADDI,  c-copy  lmain B,
   lexit LBL,
      0 0 MOVZ,  16 1 MOVZ,  $80 SVC, ;                     \ exit(0)

: EMIT-FORTH ( src-a src-u -- )
   SRCN !  >r
   ICODE-RESET  cf-reset  0 #PL !  0 PNP !
   NEWLBL Lanchor !  NEWLBL Lfind !  NEWLBL Lnum !  NEWLBL Ldict !  NEWLBL Lsrc !
   NEWLBL Lcemit !  NEWLBL Ltok !  NEWLBL Lprot !  NEWLBL Lflush !  NEWLBL Lncount !
   NEWLBL Lcfpush !  NEWLBL Lcfpop !  NEWLBL Lpat !  NEWLBL Lkwcmp !
   NEWLBL Lkwif !  NEWLBL Lkwthen !  NEWLBL Lkwelse !  NEWLBL Lkwbegin !
   NEWLBL Lkwuntil !  NEWLBL Lkwagain !  NEWLBL Lkwwhile !  NEWLBL Lkwrepeat !
   NEWLBL Lkwcreate !  NEWLBL Lkwvar !  NEWLBL Lkwsq !
   emit-main                                              \ entry @ offset 0
   emit-prims  emit-cemit  emit-tok  emit-prot  emit-flush  emit-find  emit-num
   emit-cf-helpers  emit-kwdata
   emit-dict                                              \ after #PL is final
   Lsrc @ LBL,  r> SRCN @ BYTES, ;

\ Build a standalone native Forth that interprets `src`, write it to `outfile`.
: FORTH-EXE ( src-a src-u out-a out-u -- )
   2>r  EMIT-FORTH  2r> EMIT-EXE ;

\ Build a standalone native Forth that reads its program from STDIN (batch REPL),
\ write it to `outfile`:  echo ': SQ DUP * ; 5 SQ .' | ./outfile
: FORTH-REPL-EXE ( out-a out-u -- )
   STDIN? on  s" "  ['] EMIT-FORTH catch  STDIN? off  throw  \ restore mode even on error
   EMIT-EXE ;

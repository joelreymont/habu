\ forth.fs — emit a STANDALONE native Forth (no gforth). Subroutine-threaded,
\ PC-relative (PIE-safe): primitives are native routines, the dictionary maps
\ names to code byte-offsets, the outer interpreter parses an embedded source
\ string and number-pushes / FINDs+EXECUTEs each token. Stage 1: interpret a
\ fixed program. Registers: x19=DSP, x20=RBASE(code base), x21=INP, x22=INE,
\ x23=tok-addr, x24=tok-len; x9-x15,x5-x7 scratch.

require exec.fs
require templ.fs           \ g-push/g-pop, XDS(=19)

20 constant RBASE   21 constant INP   22 constant INE   23 constant TKA   24 constant TKL

\ --- primitive registry (host-side, to build the dictionary) ---
create PLBL 64 cells allot   create PLEN 64 cells allot   create PNAM 64 cells allot
create PNPOOL 1024 chars allot   variable PNP   variable #PL
: reg-prim {: na nu lbl -- :}
   lbl  #PL @ cells PLBL + !
   nu   #PL @ cells PLEN + !
   PNPOOL PNP @ +  {: dst :}   dst #PL @ cells PNAM + !
   na dst nu move   nu PNP +!   1 #PL +! ;
: FPRIM {: na nu xt -- :}            \ define+register a primitive
   NEWLBL {: lbl :}  na nu lbl reg-prim  lbl LBL,  xt execute  RET, ;

\ shared label ids (forward refs)
variable Lanchor  variable Lfind  variable Lnum  variable Ldict  variable Lsrc  variable SRCN

\ ---- primitive bodies ( emit ICode operating on x19 data stack ) ----
9 constant A   10 constant B   11 constant C
: b+   B g-pop  A g-pop  A A B ADD,  A g-push ;
: b-   B g-pop  A g-pop  A A B SUB,  A g-push ;
: b*   B g-pop  A g-pop  A A B MUL,  A g-push ;
: bdup  A g-pop  A g-push  A g-push ;
: bdrop XDS XDS 8 SUBI, ;
: bswap A g-pop  B g-pop  A g-push  B g-push ;
\ `.` : pop, print signed decimal + newline (itoa + write), then RET
: bdot
   A g-pop  SP SP 32 SUBI,  12 SP 32 ADDI,
   13 10 MOVZ,  12 12 1 SUBI,  13 12 0 STRB,        \ '\n'
   14 0 MOVZ,  A 0 CMPI,
   NEWLBL {: lp :}  C-GE lp BCOND,
   14 1 MOVZ,  A SP A SUB,  lp LBL,
   10 10 MOVZ,
   NEWLBL {: ll :}  ll LBL,
   11 A 10 SDIV,  13 11 10 MUL,  13 A 13 SUB,  13 13 48 ADDI,
   12 12 1 SUBI,  13 12 0 STRB,  A 11 0 ADDI,  A ll CBNZ,
   NEWLBL {: ls :}  14 ls CBZ,  13 45 MOVZ,  12 12 1 SUBI,  13 12 0 STRB,  ls LBL,
   0 1 MOVZ,  1 12 0 ADDI,  2 SP 32 ADDI,  2 2 12 SUB,  16 4 MOVZ,  $80 SVC,
   SP SP 32 ADDI, ;

: emit-prims ( -- )
   s" +"    ['] b+    FPRIM   s" -"    ['] b-    FPRIM   s" *"    ['] b*    FPRIM
   s" dup"  ['] bdup  FPRIM   s" drop" ['] bdrop FPRIM   s" swap" ['] bswap FPRIM
   s" ."    ['] bdot  FPRIM ;

\ ---- FIND ( x9=tka x10=tkl -- x11=coff x12=found ) ----
: emit-find ( -- )
   Lfind @ LBL,
   13 Ldict @ ADR,  14 #PL @ MOVZ,  12 0 MOVZ,
   NEWLBL {: lloop :}  NEWLBL {: ldone :}  NEWLBL {: lnext :}
   lloop LBL,
   14 ldone CBZ,
   15 13 8 LDR,  15 10 CMP,  C-NE lnext BCOND,
   5 0 MOVZ,
   NEWLBL {: lcmp :}  NEWLBL {: lmatch :}
   lcmp LBL,
   5 10 CMP,  C-GE lmatch BCOND,
   6 13 5 ADD,  6 6 16 ADDI,  6 6 0 LDRB,
   7 9 5 ADD,   7 7 0 LDRB,
   6 7 CMP,  C-NE lnext BCOND,
   5 5 1 ADDI,  lcmp B,
   lmatch LBL,  11 13 0 LDR,  12 1 MOVZ,  ldone B,
   lnext LBL,  13 13 32 ADDI,  14 14 1 SUBI,  lloop B,
   ldone LBL,  RET, ;

\ ---- NUMBER? ( x9=tka x10=tkl -- x11=val x12=ok ) ----
: emit-num ( -- )
   Lnum @ LBL,
   11 0 MOVZ,  13 1 MOVZ,  14 0 MOVZ,  12 0 MOVZ,
   NEWLBL {: ldone :}
   10 ldone CBZ,                                  \ empty -> fail
   15 9 0 LDRB,  15 45 CMPI,
   NEWLBL {: lloop :}  C-NE lloop BCOND,
   13 0 MOVN,  14 1 MOVZ,  14 10 CMP,  C-EQ ldone BCOND,   \ "-" only -> fail
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

\ ---- dictionary: NPRIMS records of [coff(8) | namelen(8) | name(16)] ----
: emit-dict ( -- )
   Ldict @ LBL,
   #PL @ 0 ?do
      i cells PLBL + @ DLBL,                       \ +0 code byte-offset
      i cells PLEN + @ DCQ,                         \ +8 name length
      i cells PNAM + @  i cells PLEN + @  BYTES,    \ +16 name (padded to 4)
      \ pad name field to 16 bytes
      16  i cells PLEN + @  3 + -4 and  -  ?dup if  PNPOOL  swap BYTES, then
   loop ;

\ ---- MAIN: startup + outer interpreter loop (entry at offset 0) ----
: emit-main ( -- )
   Lanchor @ LBL,
   RBASE Lanchor @ ADR,                  \ x20 = runtime code base
   SP SP 2048 SUBI,  XDS SP 0 ADDI,      \ data stack
   INP Lsrc @ ADR,                        \ x21 = &SRC
   INE Lsrc @ ADR,  INE INE SRCN @ ADDI,  \ x22 = &SRC + len
   NEWLBL {: lskip :}   NEWLBL {: lhastok :}  NEWLBL {: lscan :}
   NEWLBL {: lgot :}    NEWLBL {: lnotnum :} NEWLBL {: ldone :}
   lskip LBL,
      INP INE CMP,  C-GE ldone BCOND,
      9 INP 0 LDRB,  9 32 CMPI,  C-NE lhastok BCOND,
      INP INP 1 ADDI,  lskip B,
   lhastok LBL,
      TKA INP 0 ADDI,
   lscan LBL,
      INP INE CMP,  C-GE lgot BCOND,
      9 INP 0 LDRB,  9 32 CMPI,  C-EQ lgot BCOND,
      INP INP 1 ADDI,  lscan B,
   lgot LBL,
      TKL INP TKA SUB,
      9 TKA 0 ADDI,  10 TKL 0 ADDI,  Lnum @ BL,        \ NUMBER?
      12 lnotnum CBZ,
      11 g-push  lskip B,
   lnotnum LBL,
      9 TKA 0 ADDI,  10 TKL 0 ADDI,  Lfind @ BL,       \ FIND
      12 lskip CBZ,                                     \ unknown -> skip
      9 RBASE 11 ADD,  9 BLR,  lskip B,                 \ EXECUTE
   ldone LBL,
      0 0 MOVZ,  16 1 MOVZ,  $80 SVC, ;

: EMIT-FORTH ( src-a src-u -- )
   SRCN !  >r
   ICODE-RESET  cf-reset  0 #PL !  0 PNP !
   NEWLBL Lanchor !  NEWLBL Lfind !  NEWLBL Lnum !  NEWLBL Ldict !  NEWLBL Lsrc !
   emit-main  emit-prims  emit-find  emit-num  emit-dict   \ prims first: emit-find bakes #PL
   Lsrc @ LBL,  r> SRCN @ BYTES, ;

\ Build a standalone native Forth that interprets `src`, write it to `outfile`.
: FORTH-EXE ( src-a src-u out-a out-u -- )
   2>r  EMIT-FORTH  2r> EMIT-EXE ;

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
require crash.fs           \ in-binary crash handler (register dump on signal)

20 constant RBASE   21 constant INP    22 constant INE   23 constant TKA   24 constant TKL
25 constant PEND    26 constant DBASE  27 constant NDICT  28 constant CP

$100000 constant REGION       \ mmap region size (1 MB)
$10000  constant DICT-SIZE     \ dict area at region+0 (64 KB); code area follows
48      constant DREC          \ dict record: addr(8) clen(8) namelen(8) name(16) wid(8)
$F000   constant CFSTK-OFF     \ control-flow stack: cell[0]=CFSP, cells[1..]=addrs
$80000  constant DATA-SIZE     \ data-space mmap (always RW, separate from the RX code region)
$100000 constant IBUFSZ        \ stdin read buffer (1 MB)
\ x20 (RBASE) is dead after startup, so it doubles as DATA: the data-space base.
\ [x20] holds DP (next-free pointer); usable space is [x20+8 .. x20+DATA-SIZE).
20 constant DATA
\ data-region header (all at [x20]): DP, HND (catch chain), and the locals table
\ for the word being compiled — LOC-N count, LOC-F frame bytes, then 16 name slots
\ (len + up to 16 name bytes, 24 B each). User data (DP) starts past the header.
0   constant DP-CELL    8  constant HND-CELL
16  constant LOCN-CELL   24 constant LOCF-CELL    32 constant LOCNAMES
24  constant LOC-REC      \ bytes per local name record (len + 16 name)
$1A0 constant CUR-CELL    \ get/set-current wordlist id (new defs go here)
$1A8 constant WIDN-CELL   \ next fresh wordlist id (WORDLIST hands these out)
$1B0 constant HOOK-CELL   \ check hook: a word addr run on each : body (0 = none)
$1B8 constant BODYLEN-CELL \ length of the captured body of the def in progress
$1C0 constant RBASE-CELL  \ saved __TEXT load base (RBASE) for the self-rebuild
$1C8 constant LOOPSP-CELL \ DO/LOOP frame stack depth
$1D0 constant S0-CELL     \ saved data-stack base (initial XDS) for the `.s` inspector
$1D8 constant SSCR-CELL   \ `.s` loop-pointer scratch (survives g-print9's x9..x15 clobber)
$200 constant BODYBUF-OFF \ captured body text (space-joined tokens), 1 KB
$600 constant LOOP-STK-OFF \ DO/LOOP frames (index,limit) — 32 nested, 16 B each
$800 constant DATA-START  \ DP initial offset (past header + body buffer + loop stack)
create SQ-KW  115 c, 34 c,      \ build-time bytes for the keyword  s"  (s=115, "=34)
create TICK-KW   39 c,          \ '  (0x27)
create BTICK-KW  91 c, 39 c, 93 c,   \ ['] = [ ' ]  (0x5b 0x27 0x5d)
create LBRACE-KW 123 c, 58 c,   \ {:  (0x7b 0x3a)
create ENDLOC-KW 58 c, 125 c,   \ :}  (0x3a 0x7d)
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
   lbl LBL,  SP SP 16 SUBI,  30 SP 0 STR,    \ prologue: save x30 (calls now nest, not inline)
   xt execute  30 SP 0 LDR,  SP SP 16 ADDI,  RET,  elbl LBL, ;

\ shared label ids (forward refs)
variable Lanchor  variable Lfind  variable Lnum  variable Ldict  variable Lsrc  variable SRCN
variable Lcemit   variable Ltok   variable Lprot  variable Lflush variable Lncount
\ control-flow JIT helpers + keyword data labels (self-host 1b)
variable Lcfpush  variable Lcfpop  variable Lpat   variable Lkwcmp
variable Lkwif    variable Lkwthen variable Lkwelse variable Lkwbegin
variable Lkwuntil variable Lkwagain variable Lkwwhile variable Lkwrepeat
variable Lkwcreate variable Lkwvar variable Lkwsq variable Lkwtick variable Lkwbtick
variable Lkwlbrace variable Lkwendloc variable Lloc-find variable Lkwconst
variable Lkwdo variable Lkwloop variable Lkwi

9 constant A   10 constant B   11 constant C
\ ---- primitive bodies (ICode operating on the x19 data stack) ----
: b+   B g-pop  A g-pop  A A B ADD,  A g-push ;
: b-   B g-pop  A g-pop  A A B SUB,  A g-push ;
: b*   B g-pop  A g-pop  A A B MUL,  A g-push ;
: bdup  A g-pop  A g-push  A g-push ;
: bdrop XDS XDS 8 SUBI, ;
: bswap A g-pop  B g-pop  A g-push  B g-push ;
: bdot  A g-pop  g-print9 ;          \ pop x9, print signed decimal + newline
\ .s — print the whole data stack (base..top), one signed decimal per line, WITHOUT
\ consuming it. The loop pointer lives in a DATA cell because g-print9 clobbers x9..x15.
: b.s
   9 DATA S0-CELL LDR,  9 DATA SSCR-CELL STR,
   NEWLBL {: sl :}  NEWLBL {: sd :}
   sl LBL,
      9 DATA SSCR-CELL LDR,  9 XDS CMP,  C-GE sd BCOND,
      9 9 0 LDR,  g-print9
      9 DATA SSCR-CELL LDR,  9 9 8 ADDI,  9 DATA SSCR-CELL STR,
      sl B,
   sd LBL, ;

\ comparisons -> Forth flag 0/-1 (CSET 0/1 then negate via the zero register SP)
: (cmp) {: cond -- :}  B g-pop  A g-pop  A B CMP,  A cond CSET,  A SP A SUB,  A g-push ;
: b=  C-EQ (cmp) ;   : b<> C-NE (cmp) ;   : b<  C-LT (cmp) ;   : b>  C-GT (cmp) ;
: b<= C-LE (cmp) ;   : b>= C-GE (cmp) ;
: b0= A g-pop  A 0 CMPI,  A C-EQ CSET,  A SP A SUB,  A g-push ;
: b0< A g-pop  A 0 CMPI,  A C-LT CSET,  A SP A SUB,  A g-push ;
: b1+ A g-pop  A A 1 ADDI,  A g-push ;
: b1- A g-pop  A A 1 SUBI,  A g-push ;
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
\ file I/O (path must be NUL-terminated by the caller)
: bopen   2 g-pop  1 g-pop  0 g-pop  16 5 MOVZ,  $80 SVC,  0 g-push ;   \ ( pathz flags mode -- fd )
: bwrite  2 g-pop  1 g-pop  0 g-pop  16 4 MOVZ,  $80 SVC,  0 g-push ;   \ ( fd buf len -- n )
: bclose  0 g-pop  16 6 MOVZ,  $80 SVC, ;                               \ ( fd -- )
: brbase  9 DATA RBASE-CELL LDR,  9 g-push ;                            \ ( -- rbase ) __TEXT load base
: bexec   A g-pop  SP SP 16 SUBI,  30 SP 0 STR,  A BLR,  30 SP 0 LDR,  SP SP 16 ADDI, ;  \ ( xt -- )
\ catch ( xt -- exc ) / throw ( exc -- ). Handler frames chain through [x20+8]
\ (=HND). A frame (48 B on the machine stack) saves: prev-HND, data-sp(x19),
\ machine-sp, resume-pc (an ADR within this stencil — PC-relative, survives the
\ memcpy that inlines the stencil), and the link register.
: bcatch
   A g-pop                               \ xt -> x9
   SP SP 48 SUBI,
   30 SP 32 STR,                         \ save link
   11 DATA 8 LDR,  11 SP 0 STR,          \ prev HND
   19 SP 8 STR,                          \ data sp
   13 SP 48 ADDI,  13 SP 16 STR,         \ machine sp to restore (= frame+48)
   NEWLBL {: lres :}  NEWLBL {: lpush :}
   12 lres ADR,  12 SP 24 STR,           \ resume pc
   14 SP 0 ADDI,  14 DATA 8 STR,         \ HND = this frame
   9 BLR,                                \ run xt (may throw)
   11 SP 0 LDR,  11 DATA 8 STR,          \ normal: HND = prev
   30 SP 32 LDR,  SP SP 48 ADDI,         \ restore link, pop frame
   9 0 MOVZ,  lpush B,                   \ exc = 0
   lres LBL,                             \ throw lands here (x9=exc, sp/HND/lr restored)
   lpush LBL,  9 g-push ;                \ push exc (0 normal / exc on throw)
: bthrow
   A g-pop                               \ exc -> x9
   11 DATA 8 LDR,                        \ HND
   NEWLBL {: lnoh :}  11 lnoh CBZ,
   19 11 8 LDR,                          \ restore data sp
   10 11 0 LDR,  10 DATA 8 STR,          \ HND = prev
   30 11 32 LDR,  12 11 24 LDR,  13 11 16 LDR,   \ link, resume pc, machine sp
   SP 13 0 ADDI,  12 BR,                 \ restore sp; jump to catch's resume
   lnoh LBL,  0 9 0 ADDI,  16 1 MOVZ,  $80 SVC, ;   \ no handler -> exit(exc)
\ wordlists: each dict record carries a wid (offset 40). New defs take CURRENT.
: bwordlist  9 DATA WIDN-CELL LDR,  9 g-push  9 9 1 ADDI,  9 DATA WIDN-CELL STR, ;  \ ( -- wid )
: bgetcur    9 DATA CUR-CELL LDR,  9 g-push ;                                       \ ( -- wid )
: bsetcur    A g-pop  A DATA CUR-CELL STR, ;                                        \ ( wid -- )
: bsetcheck  A g-pop  A DATA HOOK-CELL STR, ;                                       \ ( xt -- ): install check hook
\ search-wl ( a u wid -- addr|0 ): find name (a,u) in wordlist wid (case-folded)
: bswl
   2 g-pop  1 g-pop  0 g-pop                      \ wid=x2, u=x1, a=x0
   3 $20 MOVZ,  5 DBASE 0 ADDI,  6 NDICT 0 ADDI,  11 0 MOVZ,   \ fold mask, rec, count, result
   NEWLBL {: wl :} NEWLBL {: wend :} NEWLBL {: wnext :} NEWLBL {: wcmp :}
   NEWLBL {: wmatch :} NEWLBL {: wf1 :} NEWLBL {: wf2 :}
   wl LBL,  6 wend CBZ,
      9 5 40 LDR,  9 2 CMP,  C-NE wnext BCOND,    \ wid mismatch
      9 5 16 LDR,  9 1 CMP,  C-NE wnext BCOND,    \ namelen mismatch
      7 0 MOVZ,
      wcmp LBL,  7 1 CMP,  C-GE wmatch BCOND,
         9 5 24 ADDI,  9 9 7 ADD,  9 9 0 LDRB,    \ rec.name[j]
         9 $41 CMPI,  C-LT wf1 BCOND,  9 $5A CMPI,  C-GT wf1 BCOND,  9 9 3 ORR,
         wf1 LBL,
         10 0 7 ADD,  10 10 0 LDRB,               \ a[j]
         10 $41 CMPI,  C-LT wf2 BCOND,  10 $5A CMPI,  C-GT wf2 BCOND,  10 10 3 ORR,
         wf2 LBL,
         9 10 CMP,  C-NE wnext BCOND,
         7 7 1 ADDI,  wcmp B,
      wmatch LBL,  11 5 0 LDR,  wnext B,          \ keep scanning -> LAST (newest) match wins
      wnext LBL,  5 5 DREC ADDI,  6 6 1 SUBI,  wl B,
   wend LBL,  11 g-push ;

: emit-prims ( -- )
   s" +"    ['] b+    FPRIM   s" -"    ['] b-    FPRIM   s" *"    ['] b*    FPRIM
   s" dup"  ['] bdup  FPRIM   s" drop" ['] bdrop FPRIM   s" swap" ['] bswap FPRIM
   s" ."    ['] bdot  FPRIM   s" .s"   ['] b.s   FPRIM
   s" ="    ['] b=    FPRIM   s" <>"   ['] b<>   FPRIM   s" <"    ['] b<    FPRIM
   s" >"    ['] b>    FPRIM   s" <="   ['] b<=   FPRIM   s" >="   ['] b>=   FPRIM
   s" 0="   ['] b0=   FPRIM   s" 0<"   ['] b0<   FPRIM
   s" 1+"   ['] b1+   FPRIM   s" 1-"   ['] b1-   FPRIM
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
   s" type" ['] btype  FPRIM   s" execute" ['] bexec FPRIM
   s" open" ['] bopen FPRIM   s" write" ['] bwrite FPRIM   s" close" ['] bclose FPRIM
   s" rbase" ['] brbase FPRIM
   s" catch" ['] bcatch FPRIM   s" throw" ['] bthrow FPRIM
   s" wordlist" ['] bwordlist FPRIM   s" get-current" ['] bgetcur FPRIM
   s" set-current" ['] bsetcur FPRIM  s" search-wl" ['] bswl FPRIM
   s" set-check" ['] bsetcheck FPRIM ;

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
      fmatch LBL,                                          \ keep scanning: take the LAST
         11 5 0 LDR,  12 5 8 LDR,  13 1 MOVZ,  fnext B,    \ (newest) match -> redefs shadow
      fnext LBL,  5 5 DREC ADDI,  6 6 1 SUBI,  floop B,
   fdone LBL,  RET, ;

\ ---- NUMBER? ( x9=tka x10=tkl -- x11=val x12=ok ) ----
\ Accepts decimal and $hex, each with an optional leading '-'.  x6=base, x7=digit.
: emit-num ( -- )
   Lnum @ LBL,
   11 0 MOVZ,  13 1 MOVZ,  14 0 MOVZ,  12 0 MOVZ,  6 10 MOVZ,   \ val sign idx ok base=10
   NEWLBL {: ldone :}
   10 ldone CBZ,                                                \ empty token -> fail
   15 9 0 LDRB,  15 45 CMPI,  NEWLBL {: ndoll :}  C-NE ndoll BCOND,  \ leading '-'
      13 0 MOVN,  14 1 MOVZ,
   ndoll LBL,
   14 10 CMP,  C-GE ldone BCOND,                                \ "-" only -> fail (before probe!)
   5 9 14 ADD,  15 5 0 LDRB,  15 36 CMPI,  NEWLBL {: nohex :}  C-NE nohex BCOND,  \ '$' prefix
      6 16 MOVZ,  14 14 1 ADDI,
   nohex LBL,
   14 10 CMP,  C-GE ldone BCOND,                                \ nothing after sign/$ -> fail
   NEWLBL {: lloop :}  NEWLBL {: lok :}  NEWLBL {: gotd :}  NEWLBL {: nd :}  NEWLBL {: nuc :}
   lloop LBL,
   14 10 CMP,  C-GE lok BCOND,
   5 9 14 ADD,  15 5 0 LDRB,                                    \ c = next byte
   15 48 CMPI,  C-LT ldone BCOND,                               \ < '0' -> fail
   15 57 CMPI,  C-GT nd BCOND,
      7 15 48 SUBI,  gotd B,                                    \ '0'..'9' -> c-48
   nd LBL,
   6 16 CMPI,  C-NE ldone BCOND,                                \ non-hex base -> fail
   15 97 CMPI,  C-LT nuc BCOND,  15 102 CMPI,  C-GT ldone BCOND,
      7 15 87 SUBI,  gotd B,                                    \ 'a'..'f' -> c-87
   nuc LBL,
   15 65 CMPI,  C-LT ldone BCOND,  15 70 CMPI,  C-GT ldone BCOND,
      7 15 55 SUBI,                                             \ 'A'..'F' -> c-55
   gotd LBL,
   11 11 6 MUL,  11 11 7 ADD,                                   \ val = val*base + digit
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
      0 DCQ,                                               \ +40 wid (seed prims = 0 = FORTH)
   loop ;

\ ---- compile-mode literal: emit movz/movk x9=val then the push stencil ----
: c-lit ( -- )   \ val in x11 at runtime; T0 register in JIT code is x9
   6 11 0 ADDI,  5 $FFFF MOVZ,
   7 6 5 AND,    7 7 5 LSLI,  8 W-MOVZ0 LIT64,  9 8 7 ORR,  Lcemit @ BL,
   7 6 16 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK1 LIT64,  9 8 7 ORR,  Lcemit @ BL,
   7 6 32 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK2 LIT64,  9 8 7 ORR,  Lcemit @ BL,
   7 6 48 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK3 LIT64,  9 8 7 ORR,  Lcemit @ BL,
   9 W-PUSH0 LIT64,  Lcemit @ BL,  9 W-PUSH1 LIT64,  Lcemit @ BL, ;

\ ---- compile-mode CALL: emit `movz/movk x16,target + blr x16` (x11=target addr).
\ Replaces inlining (which flattened bodies and exploded code size); every word now
\ saves/restores x30, so arbitrary call nesting is safe. Absolute, not BL: the JIT
\ region is a kernel-placed mmap and prims live in __TEXT — BL's +-128MB imm26 would
\ silently truncate if they land far apart. x16 is IP0, the ABI call-scratch register.
: c-call ( -- )   \ x11 = target addr (48-bit VA)
   5 $FFFF MOVZ,
   7 11 5 AND,    7 7 5 LSLI,  8 $D2800010 LIT64,  9 8 7 ORR,  Lcemit @ BL,  \ movz x16,lo
   7 11 16 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 $F2A00010 LIT64,  9 8 7 ORR,  Lcemit @ BL,
   7 11 32 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 $F2C00010 LIT64,  9 8 7 ORR,  Lcemit @ BL,
   9 $D63F0200 LIT64,  Lcemit @ BL, ;                                        \ blr x16

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
      INP Lsrc @ ADR,  INE Lsrc @ ADR,  5 SRCN @ LIT64,  INE INE 5 ADD,
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
         4 TKA 2 ADD,  4 4 0 LDRB,                    \ token byte
         NEWLBL {: knf :}                             \ fold ONLY A-Z (symbols stay literal)
         4 $41 CMPI,  C-LT knf BCOND,  4 $5A CMPI,  C-GT knf BCOND,  4 4 3 ORR,
         knf LBL,
         5 0 2 ADD,    5 5 0 LDRB,                    \ keyword byte (stored lower-case / literal)
         4 5 CMP,  C-NE kno BCOND,
         2 2 1 ADDI,  kchk B,
      kyes LBL,  0 1 MOVZ,  RET,
      kno  LBL,  0 0 MOVZ,  RET, ;

\ Lloc-find ( -- x0 = local slot index, or -1 ) : exact-match TKA/TKL against the
\ locals table ([x20+LOCNAMES], LOC-N records of {len, 16 name bytes}).
: emit-loc-find ( -- )
   Lloc-find @ LBL,
   9 DATA LOCN-CELL LDR,  10 0 MOVZ,                 \ x9=N  x10=i
   NEWLBL {: ll :}  NEWLBL {: lmiss :}  NEWLBL {: lhit :}
   NEWLBL {: lcmp :}  NEWLBL {: lnext :}
   ll LBL,  10 9 CMP,  C-GE lmiss BCOND,
      12 LOC-REC MOVZ,  11 10 12 MUL,  11 11 LOCNAMES ADDI,  11 DATA 11 ADD,   \ entry
      12 11 0 LDR,  12 TKL CMP,  C-NE lnext BCOND,   \ len mismatch
      13 0 MOVZ,                                     \ j
      lcmp LBL,  13 TKL CMP,  C-GE lhit BCOND,
         14 11 13 ADD,  14 14 8 ADDI,  14 14 0 LDRB, \ entry.name[j]
         15 TKA 13 ADD,  15 15 0 LDRB,               \ tok[j]
         14 15 CMP,  C-NE lnext BCOND,
         13 13 1 ADDI,  lcmp B,
      lhit LBL,  0 10 0 ADDI,  RET,                  \ slot = i
      lnext LBL,  10 10 1 ADDI,  ll B,
   lmiss LBL,  0 0 MOVN,  RET, ;                     \ -1

\ keyword bytes (lower-case) at known labels; ADR reaches them PC-relative
: emit-kwdata ( -- )
   Lkwif @ LBL,     s" if"     BYTES,    Lkwthen @ LBL,   s" then"   BYTES,
   Lkwelse @ LBL,   s" else"   BYTES,    Lkwbegin @ LBL,  s" begin"  BYTES,
   Lkwuntil @ LBL,  s" until"  BYTES,    Lkwagain @ LBL,  s" again"  BYTES,
   Lkwwhile @ LBL,  s" while"  BYTES,    Lkwrepeat @ LBL, s" repeat" BYTES,
   Lkwcreate @ LBL, s" create" BYTES,    Lkwvar @ LBL,    s" variable" BYTES,
   Lkwsq @ LBL,     SQ-KW 2 BYTES,                         \ the 2 bytes  s "
   Lkwtick @ LBL,   TICK-KW 1 BYTES,    Lkwbtick @ LBL,  BTICK-KW 3 BYTES,
   Lkwlbrace @ LBL, LBRACE-KW 2 BYTES,  Lkwendloc @ LBL, ENDLOC-KW 2 BYTES,
   Lkwconst @ LBL,  s" constant" BYTES,
   Lkwdo @ LBL,  s" do" BYTES,    Lkwloop @ LBL,  s" loop" BYTES,    Lkwi @ LBL,  s" i" BYTES, ;

\ compile-time handler emitters (run at BUILD time, append JIT-emitter ICode)
: c-emitw  ( word -- )  9 swap LIT64,  Lcemit @ BL, ;          \ emit one fixed instr word
: c-popflag ( -- )  $D1002273 c-emitw  $F9400269 c-emitw ;     \ sub x19,#8 ; ldr x9,[x19]
: c-pushcp ( -- )   9 CP 0 ADDI,  Lcfpush @ BL, ;              \ push current CP
: c-bback {: opc mask -- :}                                    \ branch opc back to x9 target
   10 9 CP SUB,  10 10 2 ASRI,  5 mask LIT64,  10 10 5 AND,  9 opc LIT64,  9 9 10 ORR,  Lcemit @ BL, ;
: j-if    c-popflag  c-pushcp  $B4000009 c-emitw ;             \ pop flag; cbz fwd (patched by THEN)
: j-then  Lcfpop @ BL,  Lpat @ BL, ;
: j-else  Lcfpop @ BL,  14 9 0 ADDI,  c-pushcp  $14000000 c-emitw  9 14 0 ADDI,  Lpat @ BL, ;
: j-begin c-pushcp ;
: j-again Lcfpop @ BL,  $14000000 $3FFFFFF c-bback ;
: j-until Lcfpop @ BL,  14 9 0 ADDI,  c-popflag
   10 14 CP SUB,  10 10 2 ASRI,  5 $7FFFF LIT64,  10 10 5 AND,  10 10 5 LSLI,
   9 $B4000009 LIT64,  9 9 10 ORR,  Lcemit @ BL, ;
: j-while c-popflag  c-pushcp  $B4000009 c-emitw ;
: j-repeat Lcfpop @ BL,  14 9 0 ADDI,  Lcfpop @ BL,  $14000000 $3FFFFFF c-bback
   9 14 0 ADDI,  Lpat @ BL, ;

\ DO/LOOP/I — loop index/limit live in a data-region frame stack ([x20+LOOP-STK-OFF],
\ depth [x20+LOOPSP-CELL]) since x27/x28 are the compiler's NDICT/CP. Fixed encodings
\ (computed offline). j-do pushes a frame + records loop-top; j-loop increments the
\ index, compares, b.lt back, then pops the frame on exit; j-i pushes the index.
: j-do  ( limit start DO )
   3506446963 c-emitw  4181721705 c-emitw  3506446963 c-emitw  4181721706 c-emitw
   4181780107 c-emitw  3548179820 c-emitw  2434269580 c-emitw  2333344140 c-emitw
   4177527177 c-emitw  4177528202 c-emitw  2432697707 c-emitw  4177585803 c-emitw
   c-pushcp ;
: j-loop
   4181780107 c-emitw  3506439531 c-emitw  3548179820 c-emitw  2434269580 c-emitw  2333344140 c-emitw
   4181721481 c-emitw  4181722506 c-emitw  2432697641 c-emitw  4177527177 c-emitw  3943301439 c-emitw
   Lcfpop @ BL,                                        \ x9 = loop-top
   10 9 CP SUB,  10 10 2 ASRI,  5 $7FFFF LIT64,  10 10 5 AND,  10 10 5 LSLI,
   9 $5400000B LIT64,  9 9 10 ORR,  Lcemit @ BL,       \ b.lt loop-top
   4181780107 c-emitw  3506439531 c-emitw  4177585803 c-emitw ;   \ pop frame
: j-i
   4181780107 c-emitw  3506439531 c-emitw  3548179820 c-emitw  2434269580 c-emitw  2333344140 c-emitw
   4181721481 c-emitw  4177527401 c-emitw  2432705139 c-emitw ;

\ CREATE/VARIABLE (interpret-mode defining words): make a dict word whose body
\ pushes the current DP (a data-space address). Reuses the `:` slot pattern + the
\ c-lit emitter (with x11 = DP) for the literal-push body.
: c-create ( -- )
   2 3 MOVZ,  Lprot @ BL,                               \ region -> RW
   Ltok @ BL,                                            \ read NAME
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,   \ slot
   CP 9 0 STR,  TKL 9 16 STR,                            \ slot.addr=CP, namelen
   14 DATA CUR-CELL LDR,  14 9 40 STR,                   \ slot.wid = CURRENT
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

\ CONSTANT ( n -- ) "name": define a word that pushes n. Pop n first (x15
\ survives the name copy), then emit a literal-push body via c-lit (x11=n).
: c-constant ( -- )
   15 g-pop                                             \ n -> x15 (consumed)
   2 3 MOVZ,  Lprot @ BL,  Ltok @ BL,
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   CP 9 0 STR,  TKL 9 16 STR,  14 DATA CUR-CELL LDR,  14 9 40 STR,
   10 9 24 ADDI,  11 TKA 0 ADDI,  12 TKL 0 ADDI,
   NEWLBL {: kcp :}  NEWLBL {: kcd :}
   kcp LBL,  12 kcd CBZ,  13 11 0 LDRB,  13 10 0 STRB,
      10 10 1 ADDI,  11 11 1 ADDI,  12 12 1 SUBI,  kcp B,
   kcd LBL,
   11 15 0 ADDI,  c-lit                                 \ body: push n
   9 W-RET LIT64,  Lcemit @ BL,
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   10 9 0 LDR,  10 CP 10 SUB,  10 10 4 SUBI,  10 9 8 STR,
   NDICT NDICT 1 ADDI,  2 5 MOVZ,  Lprot @ BL,  Lflush @ BL, ;

\ ' NAME (interpret): find NAME, push its code address. ['] NAME (compile): bake
\ the address as a literal push into the word being compiled (via c-lit, x11=addr).
: c-tick ( -- )
   Ltok @ BL,  9 TKA 0 ADDI,  10 TKL 0 ADDI,  Lfind @ BL,
   NEWLBL {: tk :}  13 tk CBZ,  11 g-push  tk LBL, ;
: c-btick ( -- )
   Ltok @ BL,  9 TKA 0 ADDI,  10 TKL 0 ADDI,  Lfind @ BL,
   NEWLBL {: bk :}  13 bk CBZ,  c-lit  bk LBL, ;

\ {: a b :} (compile): record the names in the locals table, carve a machine-stack
\ frame, and pop the declared values into slots (slot 0 = first/deepest name). The
\ frame is torn down at ';'. Local references are resolved by Lloc-find -> a load.
: c-lbrace ( -- )
   \ first {: of the word carves a fixed 16-slot (128-byte) frame; later blocks
   \ append to the locals table and pop into the next slots (no second carve).
   12 DATA LOCF-CELL LDR,  NEWLBL {: havef :}  12 havef CBNZ,
      9 $D10203FF LIT64,  Lcemit @ BL,        \ sub sp,sp,#128
      9 128 MOVZ,  9 DATA LOCF-CELL STR,      \ LOC-F = 128
   havef LBL,
   6 DATA LOCN-CELL LDR,                      \ x6 = start slot for this block (= current N)
   NEWLBL {: nl :}  NEWLBL {: nd :}  NEWLBL {: nstore :}  NEWLBL {: ncp :}  NEWLBL {: ncd :}
   nl LBL,
      Ltok @ BL,  0 nd CBZ,
      0 Lkwendloc @ ADR,  1 2 MOVZ,  Lkwcmp @ BL,  0 nstore CBZ,  nd B,   \ ":}" -> done
      nstore LBL,
      11 DATA LOCN-CELL LDR,  12 LOC-REC MOVZ,  11 11 12 MUL,  11 11 LOCNAMES ADDI,  11 DATA 11 ADD,
      TKL 11 0 STR,                           \ entry.len
      12 11 8 ADDI,  13 TKA 0 ADDI,  14 TKL 0 ADDI,    \ copy name bytes
      ncp LBL,  14 ncd CBZ,  15 13 0 LDRB, 15 12 0 STRB, 12 12 1 ADDI, 13 13 1 ADDI, 14 14 1 SUBI, ncp B,
      ncd LBL,
      11 DATA LOCN-CELL LDR,  11 11 1 ADDI,  11 DATA LOCN-CELL STR,   \ N++
      nl B,
   nd LBL,
   \ pop this block's values into slots [start .. N-1] (top -> highest slot)
   13 DATA LOCN-CELL LDR,  13 13 1 SUBI,      \ i = N-1
   NEWLBL {: pl :}  NEWLBL {: pd :}
   pl LBL,
      13 6 CMP,  C-LT pd BCOND,               \ i < start -> done
      9 $D1002273 LIT64,  Lcemit @ BL,        \ sub x19,#8
      9 $F9400269 LIT64,  Lcemit @ BL,        \ ldr x9,[x19]
      9 $F90003E9 LIT64,  14 13 10 LSLI,  9 9 14 ORR,  Lcemit @ BL,   \ str x9,[sp,#i*8]
      13 13 1 SUBI,  pl B,
   pd LBL, ;

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
      5 9 40 LDR,  5 10 40 STR,                       \ wid
      9 9 DREC ADDI,  10 10 DREC ADDI,  12 12 1 SUBI,  scopy B,
   scdone LBL,
   \ separate always-RW data region (x20 is free after the seed copy); [x20]=DP=x20+8
   0 0 MOVZ,  1 DATA-SIZE LIT64,  2 3 MOVZ,  3 $1002 LIT64,  4 0 MOVN,  5 0 MOVZ,
   16 197 MOVZ,  $80 SVC,
   20 0 RBASE-CELL STR,                               \ save RBASE (x20=__TEXT base) into the data region
   DATA 0 0 ADDI,
   XDS DATA S0-CELL STR,                              \ save data-stack base for `.s`
   7 DATA DATA-START ADDI,  7 DATA DP-CELL STR,       \ DP = base + header
   9 0 MOVZ,  9 DATA HND-CELL STR,                    \ HND (catch handler chain) = 0
   9 0 MOVZ,  9 DATA CUR-CELL STR,                    \ CURRENT wordlist = 0 (FORTH)
   9 1 MOVZ,  9 DATA WIDN-CELL STR,                   \ next fresh wid = 1
   9 0 MOVZ,  9 DATA HOOK-CELL STR,                   \ check hook = none
   9 0 MOVZ,  9 DATA LOOPSP-CELL STR,                 \ DO/LOOP frame depth = 0
   g-install-crash                                    \ self-diagnosing crash (register dump)
   emit-source                                        \ INP/INE <- baked Lsrc or stdin
   PEND 0 MOVZ,                                       \ interpret mode
   NEWLBL {: lmain :}  NEWLBL {: lexit :}  NEWLBL {: lcompile :}  NEWLBL {: lundef :}
   lmain LBL,
      Ltok @ BL,  0 lexit CBZ,
      \ skip comments (both modes): \ to end-of-line, ( to ')'
      NEWLBL {: notcom :}  NEWLBL {: skln :}  NEWLBL {: skpar :}
      TKL 1 CMPI,  C-NE notcom BCOND,
      9 TKA 0 LDRB,
      9 92 CMPI,  C-EQ skln BCOND,                       \ '\'
      9 40 CMPI,  C-NE notcom BCOND,                     \ '('
      skpar LBL,  INP INE CMP,  C-GE lmain BCOND,
         9 INP 0 LDRB,  INP INP 1 ADDI,  9 41 CMPI,  C-NE skpar BCOND,  lmain B,
      skln LBL,   INP INE CMP,  C-GE lmain BCOND,
         9 INP 0 LDRB,  INP INP 1 ADDI,  9 10 CMPI,  C-NE skln BCOND,  lmain B,
      notcom LBL,
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
         14 DATA CUR-CELL LDR,  14 9 40 STR,                \ slot.wid = CURRENT
         10 9 24 ADDI,  11 TKA 0 ADDI,  12 TKL 0 ADDI,      \ copy name
         NEWLBL {: ncopy :}  NEWLBL {: ncd :}
         ncopy LBL,  12 ncd CBZ,
            13 11 0 LDRB,  13 10 0 STRB,
            10 10 1 ADDI,  11 11 1 ADDI,  12 12 1 SUBI,  ncopy B,
         ncd LBL,
         5 CFSTK-OFF LIT64,  11 DBASE 5 ADD,  12 0 MOVZ,  12 11 0 STR,   \ reset CFSP
         12 0 MOVZ,  12 DATA LOCN-CELL STR,  12 DATA LOCF-CELL STR,      \ reset locals
         12 0 MOVZ,  12 DATA BODYLEN-CELL STR,                           \ reset body capture
         9 $D10043FF LIT64,  Lcemit @ BL,                  \ prologue: sub sp,sp,#16
         9 $F90003FE LIT64,  Lcemit @ BL,                  \   str x30,[sp]  (slot.addr points here)
         lmain B,
      lnotcolon LBL,
      \ interpret-mode defining words + tick
      lmain Lkwcreate 6 ['] c-create   cf-entry
      lmain Lkwvar    8 ['] c-variable cf-entry
      lmain Lkwconst  8 ['] c-constant cf-entry
      lmain Lkwtick   1 ['] c-tick     cf-entry
      9 TKA 0 ADDI,  10 TKL 0 ADDI,  Lnum @ BL,             \ NUMBER?
      NEWLBL {: lnotnum :}
      12 lnotnum CBZ,  11 g-push  lmain B,
      lnotnum LBL,
      9 TKA 0 ADDI,  10 TKL 0 ADDI,  Lfind @ BL,            \ FIND
      13 lundef CBZ,                                         \ unknown -> error (exit 70)
      11 BLR,  lmain B,                                      \ EXECUTE
      \ ---------------- COMPILE ----------------
   lcompile LBL,
      NEWLBL {: lnotsemi :}
      TKL 1 CMPI,  C-NE lnotsemi BCOND,
      9 TKA 0 LDRB,  9 59 CMPI,  C-NE lnotsemi BCOND,       \ ';'
         12 DATA LOCF-CELL LDR,  NEWLBL {: notd :}  12 notd CBZ,   \ tear down locals frame
            9 $910003FF LIT64,  14 12 10 LSLI,  9 9 14 ORR,  Lcemit @ BL,   \ add sp,sp,#frame
         notd LBL,
         9 $F94003FE LIT64,  Lcemit @ BL,                   \ epilogue: ldr x30,[sp]
         9 $910043FF LIT64,  Lcemit @ BL,                   \   add sp,sp,#16
         9 W-RET LIT64,  Lcemit @ BL,                       \ emit RET
         9 PEND 0 LDR,  10 CP 9 SUB,  10 10 4 SUBI,  10 PEND 8 STR,  \ clen
         2 5 MOVZ,  Lprot @ BL,  Lflush @ BL,               \ region -> RX + flush (callable now)
         \ run the check hook on the captured body; publish only if it returns nonzero
         NEWLBL {: nohook :}  NEWLBL {: rejected :}
         9 DATA HOOK-CELL LDR,  9 nohook CBZ,
            10 DATA BODYBUF-OFF ADDI,  10 g-push           \ ( body-addr )
            10 DATA BODYLEN-CELL LDR,  10 g-push           \ ( body-len )
            SP SP 16 SUBI,  30 SP 0 STR,  9 BLR,  30 SP 0 LDR,  SP SP 16 ADDI,
            10 g-pop  10 rejected CBZ,                     \ ok==0 -> don't publish
         nohook LBL,
            NDICT NDICT 1 ADDI,                            \ publish word
         rejected LBL,
         PEND 0 MOVZ,                                      \ leave compile mode
         lmain B,
      lnotsemi LBL,
      \ capture the token into the body buffer (for the check hook); space-joined
      14 DATA BODYLEN-CELL LDR,  NEWLBL {: bovf :}  14 900 CMPI,  C-GE bovf BCOND,
         15 DATA BODYBUF-OFF ADDI,  15 15 14 ADD,           \ dst = buf + len
         11 TKA 0 ADDI,  12 TKL 0 ADDI,                     \ src, count
         NEWLBL {: bcp :}  NEWLBL {: bcd :}
         bcp LBL,  12 bcd CBZ,  13 11 0 LDRB,  13 15 0 STRB,
            15 15 1 ADDI,  11 11 1 ADDI,  12 12 1 SUBI,  bcp B,
         bcd LBL,  13 32 MOVZ,  13 15 0 STRB,               \ space separator
         14 14 TKL ADD,  14 14 1 ADDI,  14 DATA BODYLEN-CELL STR,   \ len += TKL+1
      bovf LBL,
      \ control-flow keywords (compile-only): emit/patch JIT branches, then loop
      lmain Lkwif     2 ['] j-if     cf-entry
      lmain Lkwthen   4 ['] j-then   cf-entry
      lmain Lkwelse   4 ['] j-else   cf-entry
      lmain Lkwbegin  5 ['] j-begin  cf-entry
      lmain Lkwuntil  5 ['] j-until  cf-entry
      lmain Lkwagain  5 ['] j-again  cf-entry
      lmain Lkwwhile  5 ['] j-while  cf-entry
      lmain Lkwrepeat 6 ['] j-repeat cf-entry
      lmain Lkwsq     2 ['] c-sdq    cf-entry            \ S" string"
      lmain Lkwbtick  3 ['] c-btick  cf-entry            \ ['] NAME
      lmain Lkwdo     2 ['] j-do     cf-entry            \ DO
      lmain Lkwloop   4 ['] j-loop   cf-entry            \ LOOP
      lmain Lkwi      1 ['] j-i      cf-entry            \ I
      lmain Lkwlbrace 2 ['] c-lbrace cf-entry            \ {: a b :} locals
      \ local-name reference -> load from its frame slot, push
      Lloc-find @ BL,  NEWLBL {: notloc :}  0 0 CMPI,  C-LT notloc BCOND,
         9 $F94003E9 LIT64,  14 0 10 LSLI,  9 9 14 ORR,  Lcemit @ BL,   \ ldr x9,[sp,#slot*8]
         9 W-PUSH0 LIT64,  Lcemit @ BL,  9 W-PUSH1 LIT64,  Lcemit @ BL,
         lmain B,
      notloc LBL,
      9 TKA 0 ADDI,  10 TKL 0 ADDI,  Lnum @ BL,             \ NUMBER? -> literal
      NEWLBL {: lcnotnum :}
      12 lcnotnum CBZ,  c-lit  lmain B,
      lcnotnum LBL,
      9 TKA 0 ADDI,  10 TKL 0 ADDI,  Lfind @ BL,            \ FIND -> inline stencil
      13 lundef CBZ,                                         \ undefined word in a : body -> error
      c-call  lmain B,                                      \ x11=addr -> emit BL (no longer inline)
   \ undefined word during compilation: write the name to stderr and exit(70). Silently
   \ skipping it (the old behaviour) hid real bugs (e.g. `0<`, `STR=` -> no-op).
   lundef LBL,
      0 2 MOVZ,  1 TKA 0 ADDI,  2 TKL 0 ADDI,  16 4 MOVZ,  $80 SVC,   \ write(2, name)
      0 70 MOVZ,  16 1 MOVZ,  $80 SVC,                       \ exit(70)
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
   NEWLBL Lkwtick !  NEWLBL Lkwbtick !
   NEWLBL Lkwlbrace !  NEWLBL Lkwendloc !  NEWLBL Lloc-find !  NEWLBL Lkwconst !
   NEWLBL Lkwdo !  NEWLBL Lkwloop !  NEWLBL Lkwi !
   NEWLBL Lcrashh !  NEWLBL Lhex !  NEWLBL Lhdr !
   emit-main                                              \ entry @ offset 0
   emit-prims  emit-cemit  emit-tok  emit-prot  emit-flush  emit-find  emit-num
   emit-cf-helpers  emit-loc-find  emit-kwdata  emit-crash-handler  emit-hex
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

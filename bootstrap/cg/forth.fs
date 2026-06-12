\ forth.fs — emit a STANDALONE native Forth (no gforth, no C). Subroutine-threaded,
\ PC-relative (PIE-safe). Stage 1: a dictionary of native primitives + an outer
\ interpreter that number-pushes / FINDs+EXECUTEs tokens from an embedded source.
\ Stage 2 (this file): a runtime `:`/`;` compiler that JITs new words into an
\ mmap'd region. Every word carries an x30 frame (prologue/epilogue); a token
\ compiles to an absolute `movz/movk x16 + blr x16` CALL, except small leaf
\ bodies (no BL/BLR/BR/RET/ADR inside, meat <= INL-MAX) which are inlined.
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
require rt.fs              \ G-PRINT9 (shared signed-decimal printer)
require crash.fs           \ in-binary crash handler (register dump on signal)
require treeshake.fs       \ KEEP? prim/keyword gating (hb-build's maker arms it)

20 constant RBASE   21 constant INP    22 constant INE   23 constant TKA   24 constant TKL
26 constant DBASE  27 constant NDICT  28 constant CP

$100000 constant REGION       \ mmap region size (1 MB)
$300000000 constant RBASE-VA \ FIXED region VA: baked addresses survive re-runs (AOT)
$340000000 constant DATA-VA  \ FIXED data VA
$48425350414E5321 constant SNAP-MAGIC \ AOT snapshot trailer marker
$10000  constant DICT-SIZE     \ dict area at region+0 (64 KB); code area follows
48      constant DREC          \ dict record: addr(8) clen(8) namelen(8) name(16) wid(8)
$F000   constant CFSTK-OFF     \ control-flow stack: cell[0]=CFSP, cells[1..]=addrs
$200000 constant DATA-SIZE     \ data-space mmap (always RW, separate from the RX code region)
$100000 constant IBUFSZ        \ stdin read buffer (1 MB)
\ x20 (RBASE) is dead after startup, so it doubles as DATA: the data-space base.
\ [x20] holds DP (next-free pointer); usable space is [x20+8 .. x20+DATA-SIZE).
20 constant DATA
\ data-region header (all at [x20]): DP, HND (catch chain), and the locals table
\ for the word being compiled — LOC-N count, LOC-F frame bytes, then 16 name slots
\ (len + up to 16 name bytes, 24 B each). User data (DP) starts past the header.
0   constant DP-CELL    8  constant HND-CELL
16  constant LOCN-CELL   24 constant LOCF-CELL
$3000 constant LOCNAMES   \ 64 records x 24 B ($3000-$3600); was 16 at DATA+32
24  constant LOC-REC      \ bytes per local name record (len + 16 name)
$1A0 constant CUR-CELL    \ get/set-current wordlist id (new defs go here)
$1A8 constant WIDN-CELL   \ next fresh wordlist id (WORDLIST hands these out)
$1B0 constant HOOK-CELL   \ check hook: a word addr run on each : body (0 = none)
$1B8 constant BODYLEN-CELL \ length of the captured body of the def in progress
$1C0 constant RBASE-CELL  \ saved __TEXT load base (RBASE) for the self-rebuild
$1C8 constant LOOPSP-CELL \ DO/LOOP frame stack depth
$1D0 constant S0-CELL     \ saved data-stack base (initial XDS) for the `.s` inspector
$1D8 constant SSCR-CELL   \ `.s` loop-pointer scratch (survives g-print9's x9..x15 clobber)
$3640 constant REPLH-CELL  \ REPL line-reader xt (0 = batch; repl.f INSTALL sets it)
$3648 constant RSAVCP-CELL \ line-start CP    (REPL error rollback)
$3650 constant RSAVND-CELL \ line-start NDICT
$3658 constant RSAVDP-CELL \ line-start DP
$3660 constant RSAVSP-CELL \ loop-level machine SP (throw recovery unwinds to it)
$3668 constant RRECP-CELL  \ runtime addr of the REPL recovery entry (EMIT-MAIN stores it)
$3670 constant ARGC-CELL   \ dyld main(argc,argv,envp): x0-x2, captured at entry
$3678 constant ARGV-CELL
$3680 constant ENVP-CELL
$3688 constant PEND-CELL   \ pending dict record ptr (0 = interpret mode; was x25)
$600 constant LOOP-STK-OFF \ DO/LOOP frames (index,limit) — 32 nested, 16 B each
                           \ (baked into the j-do/j-loop/j-i precomputed words — don't move)
$800 constant BODYBUF-OFF \ captured body text (space-joined tokens), 8 KB
8000 constant BODYBUF-CAP \ fatal above this (truncation would let the checker certify unseen code)
$568 constant RSP-CELL    \ user return-stack depth (>r r> r@)
$570 constant EXITH-CELL  \ EXIT placeholder chain head (code offset; 0 = none)
$578 constant LVD-CELL    \ compile-time DO nesting depth (LEAVE chains)
$580 constant LVH-OFF     \ LEAVE chain head per nesting level — 16 levels
$560 constant LASTC-CELL  \ last CREATEd slot addr (DOES> patches it)
$1F0 constant DOESP-CELL  \ runtime address of LDOESPATCH (stored at startup)
$230 constant CREATEP-CELL \ runtime address of LCREATE (prims must not name labels)
$238 constant QPATCH-CELL \ [: b-over patch site (0 = not inside a quotation)
$240 constant QENT-CELL   \ [: nested entry address (the xt ;] pushes)
$248 constant QXH-CELL    \ saved EXIT chain head across the quotation
$2800 constant RSTK-OFF   \ user return stack — 256 cells, below DATA-START
$3800 constant DATA-START \ DP initial offset (past header + loop stack + body buf + rstack)
create SQ-KW  115 c, 34 c,      \ build-time bytes for the keyword  s"  (s=115, "=34)
create BCHAR-KW 91 c, 99 c, 104 c, 97 c, 114 c, 93 c,   \ [char]
create QUOT-KW 91 c, 58 c,      \ [:
create SEMIQ-KW 59 c, 93 c,     \ ;]
create QNL-KW 63 c, 10 c,       \ ?\n  (REPL reject)
create OKS-KW 32 c, 111 c, 107 c, 10 c,   \ \x20ok\n (REPL accept)
create TICK-KW   39 c,          \ '  (0x27)
create BTICK-KW  91 c, 39 c, 93 c,   \ ['] = [ ' ]  (0x5b 0x27 0x5d)
create LBRACE-KW 123 c, 58 c,   \ {:  (0x7b 0x3a)
create ENDLOC-KW 58 c, 125 c,   \ :}  (0x3a 0x7d)
variable STDIN?   STDIN? off   \ source mode: baked LSRC (off) vs read from stdin (on)

\ runtime instruction-word constants the JIT compiler stamps out (verified encodings)
$D65F03C0 constant W-RET
$F9000269 constant W-PUSH0     \ str  x9,[x19,#0]
$91002273 constant W-PUSH1     \ add  x19,x19,#8
$D2800009 constant W-MOVZ0     \ movz x9,#0
$F2A00009 constant W-MOVK1     \ movk x9,#0,lsl#16
$F2C00009 constant W-MOVK2     \ movk x9,#0,lsl#32
$F2E00009 constant W-MOVK3     \ movk x9,#0,lsl#48

\ --- primitive registry (host-side, to build the seed dictionary) ---
create PLBL 96 cells allot   create PEL 96 cells allot
create PLEN 96 cells allot   create PNAM 96 cells allot
create PNPOOL 1024 chars allot   variable PNP   variable #PL


: REG-PRIM {: na nu lbl elbl -- :}
   lbl  #PL @ cells PLBL + !
   elbl #PL @ cells PEL  + !
   nu   #PL @ cells PLEN + !
   PNPOOL PNP @ +  {: dst :}   dst #PL @ cells PNAM + !
   na dst nu move   nu PNP +!   1 #PL +! ;

: FPRIM {: na nu xt -- :}            \ define+register a primitive (start..RET..end labels)
   na nu KEEP? 0 = IF EXIT THEN
   NEWLBL {: lbl :}  NEWLBL {: elbl :}
   na nu lbl elbl REG-PRIM
   lbl LBL,  SP SP 16 SUBI,  30 SP 0 STR,    \ prologue: save x30 (calls now nest, not inline)
   xt execute  30 SP 0 LDR,  SP SP 16 ADDI,  RET,  elbl LBL, ;

: FPRIM-L {: na nu xt -- :}          \ LEAF primitive: no BL/BLR in the body, so no
   na nu KEEP? 0 = IF EXIT THEN          \ x30 frame — 2x cheaper calls, fully inlineable
   NEWLBL {: lbl :}  NEWLBL {: elbl :}
   na nu lbl elbl REG-PRIM
   lbl LBL,  xt execute  RET,  elbl LBL, ;

\ shared label ids (forward refs)
variable LANCHOR  variable LFIND  variable LNUM  variable LDICT  variable LSRC  variable SRCN
variable LCEMIT   variable LTOK   variable LPROT  variable LFLUSH variable LNCOUNT
\ control-flow JIT helpers + keyword data labels (self-host 1b)
variable LCFPUSH  variable LCFPOP  variable LPAT   variable LKWCMP  variable LBCAP  variable LBCS
variable LBCHAIN  variable LCREATE  variable LDOESPATCH
variable LKWIF    variable LKWTHEN variable LKWELSE variable LKWBEGIN
variable LKWUNTIL variable LKWAGAIN variable LKWWHILE variable LKWREPEAT
variable LKWCREATE variable LKWVAR variable LKWSQ variable LKWTICK variable LKWBTICK
variable LREAD  variable LRBYE  variable LRDIE  variable LRREC  variable LQNL  variable LOKS
variable LKWLBRACE variable LKWENDLOC variable LLOC-FIND variable LKWCONST
variable LKWDO variable LKWLOOP variable LKWI
variable LKWTOR variable LKWRFROM variable LKWRFET
variable LKWEXIT variable LKWREC
variable LKWQDO variable LKWPLOOP variable LKWJ variable LKWLEAVE variable LKWUNLOOP
variable LKWCHAR variable LKWBCHAR
variable LKWIMM variable LKWPOST variable LKWCOMPC
variable LKWDOES variable LKWQUOT variable LKWSEMIQ

9 constant A   10 constant B   11 constant C
require prof.fs           \ in-binary sampling profiler (emitters + prims)
require jit.fs          \ runtime abstract value stack for the : compiler

\ ---- primitive bodies (ICode operating on the x19 data stack) ----
: B+   B G-POP  A G-POP  A A B ADD,  A G-PUSH ;

: B-   B G-POP  A G-POP  A A B SUB,  A G-PUSH ;

: B*   B G-POP  A G-POP  A A B MUL,  A G-PUSH ;

: BDUP  A G-POP  A G-PUSH  A G-PUSH ;

: BDROP XDS XDS 8 SUBI, ;

: BSWAP A G-POP  B G-POP  A G-PUSH  B G-PUSH ;

: BDOT  A G-POP  G-PRINT9 ;          \ pop x9, print signed decimal + newline

: BU.   A G-POP  G-PRINTU9 ;         \ pop x9, print unsigned decimal + newline

: BRUNRC  A G-POP                    \ ( pathz -- rc ) spawn+wait; -1 = spawn failed
   NEWLBL {: spok :}  NEWLBL {: spdn :}  NEWLBL {: spw :}
   SP SP 64 SUBI,
   9 SP 16 STR,                      \ argv[0] = path
   10 0 MOVZ,  10 SP 24 STR,         \ argv[1] = 0
   10 SP 48 STR,                     \ envp[0] = 0
   0 SP 0 ADDI,                      \ &pid
   1 9 0 ADDI,
   2 0 MOVZ,                         \ adesc = 0 (kernel API: 5 args, not libc's 6)
   3 SP 16 ADDI,  4 SP 48 ADDI,      \ argv, envp
   NR-SPAWN SYS,
   9 2 CSET,  9 9 0 ORR,             \ error = carry set OR errno in x0
   9 spok CBZ,                       \ either -> rc -1
      9 0 MOVN,  spdn B,
   spok LBL,
   0 SP 0 LDR,                       \ pid
   1 SP 8 ADDI,  2 0 MOVZ,  3 0 MOVZ,
   NR-WAIT4 SYS,
   9 2 CSET,  9 spw CBZ,             \ wait4 error (no child) -> rc -1
      9 0 MOVN,  spdn B,
   spw LBL,
   9 SP 8 LDRW,
   9 9 8 LSRI,  9 9 $FF ANDI,        \ WEXITSTATUS
   spdn LBL,
   9 G-PUSH
   SP SP 64 ADDI, ;
: BCPFETCH    9 CP 0 ADDI,  A G-PUSH ;     \ ( -- addr ) live CP (snapshot writer)
: BNDICTFETCH 9 NDICT 0 ADDI,  A G-PUSH ;  \ ( -- n ) live dict count
: BDBASEFETCH 9 DBASE 0 ADDI,  A G-PUSH ;  \ ( -- addr ) region base

: BCREATE  15 0 MOVZ,  16 20 CREATEP-CELL LDR,  16 BLR, ;   \ ( "name" -- ) runtime CREATE via the
                                     \ startup-stored cell: subsets emit prims w/o labels

: BCOMPILE  A G-POP  11 9 0 ADDI,    \ ( xt -- ) append `movz-chain x16 ; blr x16` at CP
   SP SP 16 SUBI,  11 SP 8 STR,
   2 3 MOVZ,  LPROT @ BL,             \ run with region RX (immediate caller) — flip RW
   11 SP 8 LDR,
   5 $FFFF MOVZ,
   7 11 5 AND,    7 7 5 LSLI,  8 $D2800010 LIT64,  9 8 7 ORR,  LCEMIT @ BL,
   7 11 16 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 $F2A00010 LIT64,  9 8 7 ORR,  LCEMIT @ BL,
   7 11 32 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 $F2C00010 LIT64,  9 8 7 ORR,  LCEMIT @ BL,
   9 $D63F0200 LIT64,  LCEMIT @ BL,
   2 5 MOVZ,  LPROT @ BL,             \ back to RX for the caller
   SP SP 16 ADDI, ;

: BEMIT A G-POP  13 9 0 ADDI,  G-EMITC ;   \ ( c -- ) write one byte

: BCR   13 10 MOVZ,  G-EMITC ;

: BSPACE 13 32 MOVZ,  G-EMITC ;

\ .s — print the whole data stack (base..top), one signed decimal per line, WITHOUT
\ consuming it. The loop pointer lives in a DATA cell because G-PRINT9 clobbers x9..x15.
: B.S
   9 DATA S0-CELL LDR,  9 DATA SSCR-CELL STR,
   NEWLBL {: sl :}  NEWLBL {: sd :}
   sl LBL,
      9 DATA SSCR-CELL LDR,  9 XDS CMP,  C-GE sd BCOND,
      9 9 0 LDR,  G-PRINT9
      9 DATA SSCR-CELL LDR,  9 9 8 ADDI,  9 DATA SSCR-CELL STR,
      sl B,
   sd LBL, ;

\ comparisons -> Forth flag 0/-1 (CSET 0/1 then negate via the zero register SP)
: (CMP) {: cond -- :}  B G-POP  A G-POP  A B CMP,  A cond CSET,  A SP A SUB,  A G-PUSH ;

: B=  C-EQ (CMP) ;

: B<> C-NE (CMP) ;

: B<  C-LT (CMP) ;

: B>  C-GT (CMP) ;

: B<= C-LE (CMP) ;

: B>= C-GE (CMP) ;

: B0= A G-POP  A 0 CMPI,  A C-EQ CSET,  A SP A SUB,  A G-PUSH ;

: B0< A G-POP  A 0 CMPI,  A C-LT CSET,  A SP A SUB,  A G-PUSH ;

: B1+ A G-POP  A A 1 ADDI,  A G-PUSH ;

: B1- A G-POP  A A 1 SUBI,  A G-PUSH ;

\ bitwise / logic
: BAND B G-POP A G-POP  A A B AND, A G-PUSH ;

: BOR  B G-POP A G-POP  A A B ORR, A G-PUSH ;

: BXOR B G-POP A G-POP  A A B EOR, A G-PUSH ;

: BINV A G-POP  B 0 MOVN,  A A B EOR,  A G-PUSH ;     \ A ^ -1

: BNEG A G-POP  A SP A SUB,  A G-PUSH ;               \ 0 - A

\ shifts (variable count); /, mod via SDIV/MUL
: BLSH B G-POP A G-POP  A A B LSLV, A G-PUSH ;

: BRSH B G-POP A G-POP  A A B LSRV, A G-PUSH ;

: BDIV B G-POP A G-POP  A A B SDIV, A G-PUSH ;

: BMOD B G-POP A G-POP  C A B SDIV,  C C B MUL,  A A C SUB,  A G-PUSH ;

\ stack shuffles (memory on x19)
: BNIP  A G-POP  XDS XDS 8 SUBI,  A G-PUSH ;

: BOVER B G-POP A G-POP  A G-PUSH B G-PUSH A G-PUSH ;

: BTUCK B G-POP A G-POP  B G-PUSH A G-PUSH B G-PUSH ;

: BROT  C G-POP B G-POP A G-POP  B G-PUSH C G-PUSH A G-PUSH ;

: BMROT C G-POP B G-POP A G-POP  C G-PUSH A G-PUSH B G-PUSH ;

: B2DUP B G-POP A G-POP  A G-PUSH B G-PUSH A G-PUSH B G-PUSH ;

: B2DROP XDS XDS 16 SUBI, ;

\ memory access (absolute addresses on the stack)
: BFETCH  A G-POP  A A 0 LDR,  A G-PUSH ;

: BSTORE  B G-POP A G-POP  A B 0 STR, ;               \ ( val addr -- )

: BCFETCH A G-POP  A A 0 LDRB, A G-PUSH ;

: BCSTORE B G-POP A G-POP  A B 0 STRB, ;

: BCELLS  A G-POP  A A 3 LSLI, A G-PUSH ;             \ n*8

\ data space: DP cell is [x20]; HERE/ALLOT/,/C, bump it (x20 region is always RW)
: BHERE   7 DATA 0 LDR,  7 G-PUSH ;

: BALLOT  A G-POP  7 DATA 0 LDR,  7 7 A ADD,  7 DATA 0 STR, ;

: BCOMMA  A G-POP  7 DATA 0 LDR,  A 7 0 STR,  7 7 8 ADDI,  7 DATA 0 STR, ;

: BCCOMMA A G-POP  7 DATA 0 LDR,  A 7 0 STRB, 7 7 1 ADDI,  7 DATA 0 STR, ;

: BTYPE   2 G-POP  1 G-POP  0 1 MOVZ,  NR-WRITE SYS, ;   \ ( addr len -- ) write(1,..)

\ die ( a u code -- noreturn ): msg to stderr, exit(code). The in-subset abort for
\ compiler invariant violations — better a loud death than silent memory corruption.
: BDIE    7 G-POP  2 G-POP  1 G-POP  0 2 MOVZ,  NR-WRITE SYS,
          0 7 0 ADDI,  NR-EXIT SYS, ;

\ file I/O (path must be NUL-terminated by the caller)
: BOPEN   2 G-POP  1 G-POP  0 G-POP  NR-OPEN SYS,  0 G-PUSH ;   \ ( pathz flags mode -- fd )

: BWRITE  2 G-POP  1 G-POP  0 G-POP  NR-WRITE SYS,  0 G-PUSH ;   \ ( fd buf len -- n )

: BREAD   2 G-POP  1 G-POP  0 G-POP  NR-READ SYS,  0 G-PUSH ;   \ ( fd buf len -- n )

: BIOCTL  2 G-POP  1 G-POP  0 G-POP  NR-IOCTL SYS,  0 G-PUSH ;  \ ( fd req buf -- rc )

: BCLOSE  0 G-POP  NR-CLOSE SYS, ;                               \ ( fd -- )

: BRBASE  9 DATA RBASE-CELL LDR,  9 G-PUSH ;                            \ ( -- rbase ) __TEXT load base

: BEXEC   A G-POP  SP SP 16 SUBI,  30 SP 0 STR,  A BLR,  30 SP 0 LDR,  SP SP 16 ADDI, ;  \ ( xt -- )

\ catch ( xt -- exc ) / throw ( exc -- ). Handler frames chain through [x20+8]
\ (=HND). A frame (48 B on the machine stack) saves: prev-HND, data-sp(x19),
\ machine-sp, resume-pc (an ADR within this stencil — PC-relative, survives the
\ memcpy that inlines the stencil), and the link register.
: BCATCH
   A G-POP                               \ xt -> x9
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
   lpush LBL,  9 G-PUSH ;                \ push exc (0 normal / exc on throw)

: BTHROW
   A G-POP                               \ exc -> x9
   11 DATA 8 LDR,                        \ HND
   NEWLBL {: lnoh :}  11 lnoh CBZ,
   19 11 8 LDR,                          \ restore data sp
   10 11 0 LDR,  10 DATA 8 STR,          \ HND = prev
   30 11 32 LDR,  12 11 24 LDR,  13 11 16 LDR,   \ link, resume pc, machine sp
   SP 13 0 ADDI,  12 BR,                 \ restore sp; jump to catch's resume
   lnoh LBL,
   10 DATA REPLH-CELL LDR,  NEWLBL {: lnorec :}  10 lnorec CBZ,
   10 DATA RRECP-CELL LDR,  10 BR,                                \ tty REPL: recover instead of dying
   lnorec LBL,  0 9 0 ADDI,  NR-EXIT SYS, ;   \ no handler -> exit(exc)

\ wordlists: each dict record carries a wid (offset 40). New defs take CURRENT.
: BWORDLIST  9 DATA WIDN-CELL LDR,  9 G-PUSH  9 9 1 ADDI,  9 DATA WIDN-CELL STR, ;  \ ( -- wid )

: BGETCUR    9 DATA CUR-CELL LDR,  9 G-PUSH ;                                       \ ( -- wid )

: BSETCUR    A G-POP  A DATA CUR-CELL STR, ;                                        \ ( wid -- )

: BSETCHECK  A G-POP  A DATA HOOK-CELL STR, ;                                       \ ( xt -- ): install check hook

\ search-wl ( a u wid -- addr|0 ): find name (a,u) in wordlist wid (case-folded)
: BSWL
   2 G-POP  1 G-POP  0 G-POP                      \ wid=x2, u=x1, a=x0
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
   wend LBL,  11 G-PUSH ;

: EMIT-PRIMS ( -- )
   s" +"    ['] B+    FPRIM-L   s" -"    ['] B-    FPRIM-L   s" *"    ['] B*    FPRIM-L
   s" dup"  ['] BDUP  FPRIM-L   s" drop" ['] BDROP FPRIM-L   s" swap" ['] BSWAP FPRIM-L
   s" ."    ['] BDOT  FPRIM-L   s" .s"   ['] B.S   FPRIM-L
   s" u."   ['] BU.   FPRIM-L   s" emit" ['] BEMIT FPRIM-L
   s" cr"   ['] BCR   FPRIM-L   s" space" ['] BSPACE FPRIM-L
   s" ="    ['] B=    FPRIM-L   s" <>"   ['] B<>   FPRIM-L   s" <"    ['] B<    FPRIM-L
   s" >"    ['] B>    FPRIM-L   s" <="   ['] B<=   FPRIM-L   s" >="   ['] B>=   FPRIM-L
   s" 0="   ['] B0=   FPRIM-L   s" 0<"   ['] B0<   FPRIM-L
   s" 1+"   ['] B1+   FPRIM-L   s" 1-"   ['] B1-   FPRIM-L
   s" and"  ['] BAND  FPRIM-L   s" or"   ['] BOR   FPRIM-L   s" xor"  ['] BXOR  FPRIM-L
   s" invert" ['] BINV FPRIM-L  s" negate" ['] BNEG FPRIM-L
   s" lshift" ['] BLSH FPRIM-L  s" rshift" ['] BRSH FPRIM-L
   s" /"    ['] BDIV  FPRIM-L   s" mod"  ['] BMOD  FPRIM-L
   s" nip"  ['] BNIP  FPRIM-L   s" over" ['] BOVER FPRIM-L   s" tuck" ['] BTUCK FPRIM-L
   s" rot"  ['] BROT  FPRIM-L   s" -rot" ['] BMROT FPRIM-L
   s" 2dup" ['] B2DUP FPRIM-L   s" 2drop" ['] B2DROP FPRIM-L
   s" @"    ['] BFETCH FPRIM-L   s" !"    ['] BSTORE FPRIM-L
   s" c@"   ['] BCFETCH FPRIM-L  s" c!"   ['] BCSTORE FPRIM-L
   s" cells" ['] BCELLS FPRIM-L
   s" here" ['] BHERE  FPRIM-L   s" allot" ['] BALLOT FPRIM-L
   s" ,"    ['] BCOMMA FPRIM-L   s" c,"   ['] BCCOMMA FPRIM-L
   s" type" ['] BTYPE  FPRIM-L   s" execute" ['] BEXEC FPRIM
   s" compile," ['] BCOMPILE FPRIM
   s" create" ['] BCREATE FPRIM
   s" run-rc" ['] BRUNRC FPRIM-L
   s" cp@" ['] BCPFETCH FPRIM-L   s" dbase@" ['] BDBASEFETCH FPRIM-L
   s" ndict@" ['] BNDICTFETCH FPRIM-L
   s" die"  ['] BDIE   FPRIM-L
   s" open" ['] BOPEN FPRIM-L   s" write" ['] BWRITE FPRIM-L   s" read" ['] BREAD FPRIM-L   s" ioctl" ['] BIOCTL FPRIM-L
   s" close" ['] BCLOSE FPRIM-L
   s" rbase" ['] BRBASE FPRIM-L
   s" catch" ['] BCATCH FPRIM   s" throw" ['] BTHROW FPRIM-L
   s" wordlist" ['] BWORDLIST FPRIM-L   s" get-current" ['] BGETCUR FPRIM-L
   s" set-current" ['] BSETCUR FPRIM-L  s" search-wl" ['] BSWL FPRIM-L
   s" set-check" ['] BSETCHECK FPRIM-L ;

\ ---- CEMIT ( x9=word -- ) : str w9,[x28] ; CP += 4 ----
\ FP: doubles as raw IEEE754 bit-cells on the data stack; FMOV through D0/D1.
\ Compare conds per FP flag semantics: < MI, > GT, = EQ (NaN compares false).
: BF+    B G-POP  A G-POP  0 A FMOVXD,  1 B FMOVXD,  0 0 1 FADD,  A 0 FMOVDX,  A G-PUSH ;

: BF-    B G-POP  A G-POP  0 A FMOVXD,  1 B FMOVXD,  0 0 1 FSUB,  A 0 FMOVDX,  A G-PUSH ;

: BF*    B G-POP  A G-POP  0 A FMOVXD,  1 B FMOVXD,  0 0 1 FMUL,  A 0 FMOVDX,  A G-PUSH ;

: BF/    B G-POP  A G-POP  0 A FMOVXD,  1 B FMOVXD,  0 0 1 FDIV,  A 0 FMOVDX,  A G-PUSH ;

: BFNEG  A G-POP  0 A FMOVXD,  0 0 FNEG,   A 0 FMOVDX,  A G-PUSH ;

: BFABS  A G-POP  0 A FMOVXD,  0 0 FABS,   A 0 FMOVDX,  A G-PUSH ;

: BFSQRT A G-POP  0 A FMOVXD,  0 0 FSQRT,  A 0 FMOVDX,  A G-PUSH ;

: (FCMP) {: cond :}  B G-POP  A G-POP  0 A FMOVXD,  1 B FMOVXD,  0 1 FCMP,
   A cond CSET,  A SP A SUB,  A G-PUSH ;

: BF<  C-MI (FCMP) ;

: BF>  C-GT (FCMP) ;

: BF=  C-EQ (FCMP) ;

: (FCMP0) {: cond :}  A G-POP  0 A FMOVXD,  0 FCMP0,
   A cond CSET,  A SP A SUB,  A G-PUSH ;

: BF0< C-MI (FCMP0) ;

: BF0= C-EQ (FCMP0) ;

: BS>F  A G-POP  0 A SCVTF,   A 0 FMOVDX,  A G-PUSH ;

: BF>S  A G-POP  0 A FMOVXD,  A 0 FCVTZS,  A G-PUSH ;

: BFDOT
   NEWLBL NEWLBL NEWLBL {: fl il sd :}
   A G-POP  15 A 0 ADDI,                               \ bits (sign test later)
   SP SP 48 SUBI,
   12 SP 48 ADDI,
   13 10 MOVZ,  12 12 1 SUBI,  13 12 0 STRB,           \ newline
   0 15 FMOVXD,  1 0 FABS,                             \ d1 = |x|
   9 1 FCVTZS,                                         \ x9 = int part
   2 9 SCVTF,  3 1 2 FSUB,                             \ d3 = frac
   14 $F4240 LIT64,  2 14 SCVTF,  3 3 2 FMUL,
   14 3 FCVTZS,                                        \ x14 = frac * 1e6
   10 10 MOVZ,  5 6 MOVZ,
   fl LBL,                                             \ six zero-padded frac digits
     11 14 10 SDIV,  13 11 10 MUL,  13 14 13 SUB,
     13 13 48 ADDI,  12 12 1 SUBI,  13 12 0 STRB,
     14 11 0 ADDI,  5 5 1 SUBI,  5 fl CBNZ,
   13 46 MOVZ,  12 12 1 SUBI,  13 12 0 STRB,           \ '.'
   il LBL,                                             \ int digits (do-while)
     11 9 10 SDIV,  13 11 10 MUL,  13 9 13 SUB,
     13 13 48 ADDI,  12 12 1 SUBI,  13 12 0 STRB,
     9 11 0 ADDI,  9 il CBNZ,
   15 15 63 LSRI,  15 sd CBZ,
     13 45 MOVZ,  12 12 1 SUBI,  13 12 0 STRB,         \ '-'
   sd LBL,
   0 1 MOVZ,  1 12 0 ADDI,  2 SP 48 ADDI,  2 2 12 SUB,
   NR-WRITE SYS,
   SP SP 48 ADDI, ;

: EMIT-FP-PRIMS ( -- )
   s" f+" ['] BF+ FPRIM-L   s" f-" ['] BF- FPRIM-L   s" f*" ['] BF* FPRIM-L
   s" f/" ['] BF/ FPRIM-L   s" fnegate" ['] BFNEG FPRIM-L
   s" fabs" ['] BFABS FPRIM-L  s" fsqrt" ['] BFSQRT FPRIM-L
   s" f<" ['] BF< FPRIM-L   s" f>" ['] BF> FPRIM-L   s" f=" ['] BF= FPRIM-L
   s" f0<" ['] BF0< FPRIM-L  s" f0=" ['] BF0= FPRIM-L
   s" s>f" ['] BS>F FPRIM-L  s" f>s" ['] BF>S FPRIM-L
   s" f." ['] BFDOT FPRIM-L ;

: EMIT-CEMIT ( -- )
   LCEMIT @ LBL,  9 28 0 STRW,  28 28 4 ADDI,  RET, ;

\ LBCAP ( -- ) : append TKA/TKL + ' ' to the body capture. LBCS ( x11=a x12=u )
\ is the general entry (defining-word kind tokens). FATAL (exit 71) on overflow —
\ truncation would let the check hook certify code it never saw.
: EMIT-BCAP
   LBCAP @ LBL,
   11 TKA 0 ADDI,  12 TKL 0 ADDI,
   LBCS @ LBL,
   NEWLBL NEWLBL NEWLBL {: bok bcp bcd :}
   17 12 0 ADDI,                  \ len in x17 (IP1): callers keep state in x5-x8
   14 DATA BODYLEN-CELL LDR,
   5 BODYBUF-CAP MOVZ,  14 5 CMP,  C-LT bok BCOND,
      0 2 MOVZ,  1 11 0 ADDI,  2 12 0 ADDI,  NR-WRITE SYS,
      0 71 MOVZ,  NR-EXIT SYS,
   bok LBL,
   15 DATA BODYBUF-OFF ADDI,  15 15 14 ADD,
   bcp LBL,  12 bcd CBZ,  13 11 0 LDRB,  13 15 0 STRB,
      15 15 1 ADDI,  11 11 1 ADDI,  12 12 1 SUBI,  bcp B,
   bcd LBL,  13 32 MOVZ,  13 15 0 STRB,
   14 14 17 ADD,  14 14 1 ADDI,  14 DATA BODYLEN-CELL STR,
   RET, ;

\ ---- TOK ( -- x0=have? ) : skip spaces, scan one token into TKA/TKL, advance INP ----
: EMIT-TOK ( -- )
   LTOK @ LBL,
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
: EMIT-PROT ( -- )
   LPROT @ LBL,
   0 DBASE 0 ADDI,  1 REGION LIT64,  NR-MPROTECT SYS,  RET, ;

\ ---- FLUSH ( x9=start -- ) : DC CVAU + IC IVAU over [x9, CP) — just the words
\ emitted since the last flush, not the whole code area (that walk made every
\ `;` cost O(total code), O(n^2) over a program build) ----
: EMIT-FLUSH ( -- )
   LFLUSH @ LBL,
   NEWLBL {: fdl :}  NEWLBL {: fdd :}  NEWLBL {: fil :}  NEWLBL {: fid :}
   9 9 6 LSRI,  9 9 6 LSLI,                                 \ align start down to the
   10 9 0 ADDI,                                             \ line, or the 64-byte
                                                            \ stride skips the last one
   fdl LBL,  10 CP CMP,  C-GE fdd BCOND,  10 DCCVAU,  10 10 64 ADDI,  fdl B,
   fdd LBL,  DSB-ISH,
   10 9 0 ADDI,
   fil LBL,  10 CP CMP,  C-GE fid BCOND,  10 ICIVAU,  10 10 64 ADDI,  fil B,
   fid LBL,  DSB-ISH,  ISB,  RET, ;

\ ---- FIND ( x9=tka x10=tkl -- x11=addr x12=clen x13=found|imm<<1 ) over 40-byte records ----
: EMIT-FIND ( -- )
   LFIND @ LBL,
   5 DBASE 0 ADDI,  6 NDICT 0 ADDI,  13 0 MOVZ,           \ rec, remaining, found=0
   NEWLBL {: floop :}  NEWLBL {: fdone :}  NEWLBL {: fnext :}
   NEWLBL {: fcmp :}   NEWLBL {: fmatch :}
   floop LBL,
      6 fdone CBZ,
      14 5 16 LDR,  14 14 $FF ANDI,  14 10 CMP,  C-NE fnext BCOND,         \ namelen != tkl
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
         11 5 0 LDR,  12 5 8 LDR,
         14 5 16 LDR,  14 14 $100 ANDI,  14 14 7 LSRI,   \ immediate bit -> 2
         13 1 MOVZ,  13 13 14 ORR,  fnext B,    \ (newest) match -> redefs shadow
      fnext LBL,  5 5 DREC ADDI,  6 6 1 SUBI,  floop B,
   fdone LBL,  RET, ;

\ ---- NUMBER? ( x9=tka x10=tkl -- x11=val x12=ok ) ----
\ Accepts decimal and $hex, each with an optional leading '-'.  x6=base, x7=digit.
: EMIT-NUM ( -- )
   LNUM @ LBL,
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
   2 0 MOVZ,                                                    \ frac mode off
   14 10 CMP,  C-GE ldone BCOND,                                \ nothing after sign/$ -> fail
   NEWLBL {: lloop :}  NEWLBL {: lok :}  NEWLBL {: gotd :}  NEWLBL {: nd :}  NEWLBL {: nuc :}
   NEWLBL {: ndot :}  NEWLBL {: isfrac :}  NEWLBL {: lint :}  NEWLBL {: fpos :}
   lloop LBL,
   14 10 CMP,  C-GE lok BCOND,
   5 9 14 ADD,  15 5 0 LDRB,                                    \ c = next byte
   15 46 CMPI,  C-NE ndot BCOND,                                \ '.' -> frac mode
      6 10 CMPI,  C-NE ldone BCOND,
      2 ldone CBNZ,
      2 1 MOVZ,  4 0 MOVZ,  3 1 MOVZ,
      14 14 1 ADDI,  lloop B,
   ndot LBL,
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
   2 isfrac CBNZ,
   11 11 6 MUL,  11 11 7 ADD,                                   \ val = val*base + digit
   14 14 1 ADDI,  lloop B,
   isfrac LBL,                                                  \ frac digit: f=f*10+d, k*=10
   5 10 MOVZ,  4 4 5 MUL,  4 4 7 ADD,  3 3 5 MUL,
   14 14 1 ADDI,  lloop B,
   lok LBL,
   2 lint CBZ,
   3 1 CMPI,  C-EQ ldone BCOND,                                 \ "1." (no frac digits) -> fail
   0 11 SCVTF,  1 4 SCVTF,  2 3 SCVTF,                          \ int, frac, scale
   1 1 2 FDIV,  0 0 1 FADD,
   13 0 CMPI,  C-GE fpos BCOND,  0 0 FNEG,
   fpos LBL,  11 0 FMOVDX,  12 1 MOVZ,  RET,
   lint LBL,  11 11 13 MUL,  12 1 MOVZ,
   ldone LBL,  RET, ;

\ ---- seed dictionary: NPRIMS records of [startoff(8) endoff(8) namelen(8) name(16)] ----
: EMIT-DICT ( -- )
   LNCOUNT @ LBL,  #PL @ DCQ,                              \ live count, read at startup
   LDICT @ LBL,
   #PL @ 0 ?do
      i cells PLBL + @ DLBL,                                \ +0  start byte-offset
      i cells PEL  + @ DLBL,                                \ +8  end   byte-offset
      i cells PLEN + @ DCQ,                                 \ +16 name length
      i cells PNAM + @  i cells PLEN + @  BYTES,            \ +24 name (padded to 4)
      16  i cells PLEN + @  3 + -4 and  -  ?dup if  PNPOOL  swap BYTES, then
      0 DCQ,                                               \ +40 wid (seed prims = 0 = FORTH)
   loop ;

\ ---- compile-mode literal: emit movz/movk x9=val then the push stencil ----
: C-LIT ( -- )   \ val in x11 at runtime; T0 register in JIT code is x9
   6 11 0 ADDI,  5 $FFFF MOVZ,
   7 6 5 AND,    7 7 5 LSLI,  8 W-MOVZ0 LIT64,  9 8 7 ORR,  LCEMIT @ BL,
   7 6 16 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK1 LIT64,  9 8 7 ORR,  LCEMIT @ BL,
   7 6 32 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK2 LIT64,  9 8 7 ORR,  LCEMIT @ BL,
   7 6 48 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK3 LIT64,  9 8 7 ORR,  LCEMIT @ BL,
   9 W-PUSH0 LIT64,  LCEMIT @ BL,  9 W-PUSH1 LIT64,  LCEMIT @ BL, ;

\ ---- compile-mode CALL-or-INLINE (x11=target addr, x12=clen from FIND) ----
\ Small leaf bodies are inlined (copy the meat between the x30 prologue/epilogue);
\ everything else gets an absolute `movz/movk x16 + blr x16` call. Absolute, not BL:
\ the JIT region is a kernel-placed mmap and prims live in __TEXT — BL's +-128MB imm26
\ would silently truncate if they land far apart. x16 is IP0, the ABI call scratch.
\ Inline criteria: meat <= INL-MAX bytes AND no BL/BLR/BR/RET/ADR/ADRP word in it
\ (calls need the frame; ADR is PC-relative). Internal label branches are relative and
\ copy safely. Bodies without the prologue (CREATE/VARIABLE/CONSTANT literal-pushes)
\ inline whole. Dict clen: prim = end-start-4, user word = set at `;` — both excl RET.
$28 constant INL-MAX   \ 40 bytes = 10 instructions of meat

: C-CALL ( -- )
   NEWLBL {: lcall :}  NEWLBL {: lcopy :}  NEWLBL {: lscan :}  NEWLBL {: lsbody :}
   NEWLBL {: lnopro :}  NEWLBL {: linl :}  NEWLBL {: ldone :}
   9 11 0 LDRW,  8 $D10043FF LIT64,  9 8 CMP,  C-NE lnopro BCOND,
      12 INL-MAX 16 + CMPI,  C-GT lcall BCOND,
      13 11 8 ADDI,  14 11 12 ADD,  14 14 8 SUBI,  lscan B,   \ meat [addr+8, addr+clen-8)
   lnopro LBL,
      12 INL-MAX CMPI,  C-GT lcall BCOND,
      13 11 0 ADDI,  14 11 12 ADD,                            \ whole body [addr, addr+clen)
      9 14 0 LDRW,  8 $D65F03C0 LIT64,  9 8 CMP,  C-NE lcall BCOND,   \ ret slot patched
                                                               \ (does>) -> never inline
   lscan LBL,
      15 13 0 ADDI,
   lsbody LBL,  15 14 CMP,  C-GE lcopy BCOND,
      9 15 0 LDRW,  15 15 4 ADDI,
      8 $FC000000 LIT64,  10 9 8 AND,  8 $94000000 LIT64,  10 8 CMP,  C-EQ lcall BCOND,
      8 $FFFFFC1F LIT64,  10 9 8 AND,
         8 $D63F0000 LIT64,  10 8 CMP,  C-EQ lcall BCOND,                                \ BLR
         8 $D61F0000 LIT64,  10 8 CMP,  C-EQ lcall BCOND,                                \ BR
      8 $D65F03C0 LIT64,  9 8 CMP,  C-EQ lcall BCOND,                                    \ RET
      8 $1F000000 LIT64,  10 9 8 AND,  8 $10000000 LIT64,  10 8 CMP,  C-EQ lcall BCOND,  \ ADR/ADRP
      lsbody B,
   lcopy LBL,
      15 13 0 ADDI,
   linl LBL,  15 14 CMP,  C-GE ldone BCOND,
      9 15 0 LDRW,  15 15 4 ADDI,  LCEMIT @ BL,  linl B,
   lcall LBL,
      5 $FFFF MOVZ,
      7 11 5 AND,    7 7 5 LSLI,  8 $D2800010 LIT64,  9 8 7 ORR,  LCEMIT @ BL,  \ movz x16,lo
      7 11 16 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 $F2A00010 LIT64,  9 8 7 ORR,  LCEMIT @ BL,
      7 11 32 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 $F2C00010 LIT64,  9 8 7 ORR,  LCEMIT @ BL,
      9 $D63F0200 LIT64,  LCEMIT @ BL,                                          \ blr x16
   ldone LBL, ;

\ ---- source setup: point INP/INE at either the baked LSRC or stdin ----
\ stdin mode reads all of fd 0 into a fresh RW mmap buffer, then interprets it
\ (batch REPL: `echo ': SQ DUP * ; 5 SQ .' | ./forth`). Clobbers x0-x5,x9,x11,x16.
: EMIT-SOURCE ( -- )
   STDIN? @ if
      NEWLBL {: rpipe :}  NEWLBL {: rgo :}
      \ a tty? run the BAKED source (the REPL bootstrap) instead of a blocking
      \ read-to-EOF; a pipe keeps the classic batch read-all below.
      0 0 MOVZ,  1 $40487413 LIT64,  2 DATA BODYBUF-OFF ADDI,  NR-IOCTL SYS,
      0 rpipe CBNZ,
      INP LSRC @ ADR,  INE LSRC @ ADR,  5 SRCN @ LIT64,  INE INE 5 ADD,  rgo B,
      rpipe LBL,
      0 0 MOVZ,  1 IBUFSZ LIT64,  2 3 MOVZ,  3 $1002 LIT64,  4 0 MOVN,  5 0 MOVZ,
      NR-MMAP SYS,                       \ mmap RW input buffer -> x0
      11 0 0 ADDI,  9 0 0 ADDI,                    \ x11 = base, x9 = write ptr
      NEWLBL {: rl :}  NEWLBL {: RD :}
      rl LBL,
         0 0 MOVZ,  1 9 0 ADDI,                    \ read(fd=0, buf=ptr, …)
         2 11 0 ADDI,  5 IBUFSZ LIT64,  2 2 5 ADD,  2 2 9 SUB,   \ count = base+SZ-ptr
         2 RD CBZ,                                 \ buffer full -> done
         NR-READ SYS,                      \ -> x0 = n
         0 RD CBZ,                                 \ EOF (n=0) -> done
         9 9 0 ADD,  rl B,                         \ ptr += n
      RD LBL,
      INP 11 0 ADDI,  INE 9 0 ADDI,                \ INP=base, INE=ptr
      rgo LBL,
   else
      INP LSRC @ ADR,  INE LSRC @ ADR,  5 SRCN @ LIT64,  INE INE 5 ADD,
   then ;

\ ---- control-flow JIT: a CF stack (region+CFSTK-OFF) of placeholder branch
\ addresses; THEN/ELSE/REPEAT patch the recorded branch's relative offset. ----
\ Lcfpush(x9=val), Lcfpop(->x9), Lpat(x9=addr: patch CBZ/B to current CP),
\ Lkwcmp(x0=kwaddr x1=kwlen -> x0=match? vs TKA/TKL, case-folded).
: EMIT-CF-HELPERS ( -- )
   LCFPUSH @ LBL,
      5 CFSTK-OFF LIT64,  10 DBASE 5 ADD,  11 10 0 LDR,
      12 11 3 LSLI,  12 12 10 ADD,  12 12 8 ADDI,  9 12 0 STR,
      11 11 1 ADDI,  11 10 0 STR,  RET,
   LCFPOP @ LBL,
      5 CFSTK-OFF LIT64,  10 DBASE 5 ADD,  11 10 0 LDR,  11 11 1 SUBI,  11 10 0 STR,
      12 11 3 LSLI,  12 12 10 ADD,  12 12 8 ADDI,  9 12 0 LDR,  RET,
   LPAT @ LBL,                                       \ patch imm19 (CBZ) / imm26 (B)
      11 9 0 LDRW,  10 CP 9 SUB,  10 10 2 ASRI,
      5 $80000000 LIT64,  13 11 5 AND,
      NEWLBL {: pisb :}  NEWLBL {: pdone :}
      13 pisb CBZ,                                    \ bit31==0 -> B (imm26)
         5 $7FFFF LIT64,  10 10 5 AND,  10 10 5 LSLI,  pdone B,
      pisb LBL,  5 $3FFFFFF LIT64,  10 10 5 AND,
      pdone LBL,  11 11 10 ORR,  11 9 0 STRW,  RET,
   LKWCMP @ LBL,
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
      kno  LBL,  0 0 MOVZ,  RET,
   LBCHAIN @ LBL,                                    \ patch a B-placeholder chain:
      NEWLBL {: bcl :}  NEWLBL {: bcd :}             \ x9=head offset, x14=target;
      bcl LBL,  9 bcd CBZ,                           \ clobbers x5,x10-x12
         10 DBASE 9 ADD,  11 10 0 LDRW,
         12 14 10 SUB,  12 12 2 ASRI,
         5 $3FFFFFF LIT64,  12 12 5 AND,
         5 $14000000 LIT64,  12 12 5 ORR,
         12 10 0 STRW,
         9 11 0 ADDI,  bcl B,
      bcd LBL,  RET, ;

\ LLOC-FIND ( -- x0 = local slot index, or -1 ) : exact-match TKA/TKL against the
\ locals table ([x20+LOCNAMES], LOC-N records of {len, 16 name bytes}).
: EMIT-LOC-FIND ( -- )
   LLOC-FIND @ LBL,
   9 DATA LOCN-CELL LDR,  10 0 MOVZ,                 \ x9=N  x10=i
   NEWLBL {: ll :}  NEWLBL {: lmiss :}  NEWLBL {: lhit :}
   NEWLBL {: lcmp :}  NEWLBL {: lnext :}
   ll LBL,  10 9 CMP,  C-GE lmiss BCOND,
      12 LOC-REC MOVZ,  11 10 12 MUL,  5 LOCNAMES LIT64,  11 11 5 ADD,  11 DATA 11 ADD,   \ entry
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
: EMIT-KWDATA ( -- )
   LKWIF @ LBL,     s" if"     BYTES,    LKWTHEN @ LBL,   s" then"   BYTES,
   LKWELSE @ LBL,   s" else"   BYTES,    LKWBEGIN @ LBL,  s" begin"  BYTES,
   LKWUNTIL @ LBL,  s" until"  BYTES,    LKWAGAIN @ LBL,  s" again"  BYTES,
   LKWWHILE @ LBL,  s" while"  BYTES,    LKWREPEAT @ LBL, s" repeat" BYTES,
   LKWCREATE @ LBL, s" create" BYTES,    LKWVAR @ LBL,    s" variable" BYTES,
   LKWSQ @ LBL,     SQ-KW 2 BYTES,                         \ the 2 bytes  s "
   LKWTICK @ LBL,   TICK-KW 1 BYTES,    LKWBTICK @ LBL,  BTICK-KW 3 BYTES,
   LKWLBRACE @ LBL, LBRACE-KW 2 BYTES,  LKWENDLOC @ LBL, ENDLOC-KW 2 BYTES,
   LKWCONST @ LBL,  s" constant" BYTES,
   LQNL @ LBL,  QNL-KW 2 BYTES,   LOKS @ LBL,  OKS-KW 4 BYTES,
   LKWDO @ LBL,  s" do" BYTES,    LKWLOOP @ LBL,  s" loop" BYTES,    LKWI @ LBL,  s" i" BYTES,
   LKWTOR @ LBL,  s" >r" BYTES,   LKWRFROM @ LBL,  s" r>" BYTES,   LKWRFET @ LBL,  s" r@" BYTES,
   LKWEXIT @ LBL,  s" exit" BYTES,   LKWREC @ LBL,  s" recurse" BYTES,
   LKWQDO @ LBL,  s" ?do" BYTES,   LKWPLOOP @ LBL,  s" +loop" BYTES,   LKWJ @ LBL,  s" j" BYTES,
   LKWLEAVE @ LBL,  s" leave" BYTES,   LKWUNLOOP @ LBL,  s" unloop" BYTES,
   LKWCHAR @ LBL,  s" char" BYTES,   LKWBCHAR @ LBL,  BCHAR-KW 6 BYTES,
   LKWIMM @ LBL,  s" immediate" BYTES,   LKWPOST @ LBL,  s" postpone" BYTES,
   LKWCOMPC @ LBL,  s" compile," BYTES,
   LKWDOES @ LBL,  s" does>" BYTES,
   LKWQUOT @ LBL,  QUOT-KW 2 BYTES,   LKWSEMIQ @ LBL,  SEMIQ-KW 2 BYTES, ;

\ compile-time handler emitters (run at BUILD time, append JIT-emitter ICode)
: C-EMITW  ( word -- )  9 swap LIT64,  LCEMIT @ BL, ;          \ emit one fixed instr word

: C-POPFLAG ( -- )  $D1002273 C-EMITW  $F9400269 C-EMITW ;     \ sub x19,#8 ; ldr x9,[x19]

: C-PUSHCP ( -- )   9 CP 0 ADDI,  LCFPUSH @ BL, ;              \ push current CP

: C-BBACK {: opc mask -- :}                                    \ branch opc back to x9 target
   10 9 CP SUB,  10 10 2 ASRI,  5 mask LIT64,  10 10 5 AND,  9 opc LIT64,  9 9 10 ORR,  LCEMIT @ BL, ;

: J-IF    C-POPFLAG  C-PUSHCP  $B4000009 C-EMITW ;             \ pop flag; cbz fwd (patched by THEN)

: J-THEN  LCFPOP @ BL,  LPAT @ BL, ;

: J-ELSE  LCFPOP @ BL,  14 9 0 ADDI,  C-PUSHCP  $14000000 C-EMITW  9 14 0 ADDI,  LPAT @ BL, ;

\ BEGIN loops are register-resident: J-BEGIN snapshots the VS into registers
\ (Lvsnap), the back edges reconcile to that snapshot (Lvrecon) and branch on
\ x17 — never a VS register, so the reconcile reload can't clobber the flag.
: J-BEGIN  LVSNAP @ BL,  C-PUSHCP ;

: J-AGAIN  LVRECON @ BL,  LCFPOP @ BL,  $14000000 $3FFFFFF C-BBACK ;

: J-UNTILX ( -- )                          \ shared tail: reconcile + cbz x17,top
   LVRECON @ BL,
   LCFPOP @ BL,
   10 9 CP SUB,  10 10 2 ASRI,  5 $7FFFF LIT64,  10 10 5 AND,  10 10 5 LSLI,
   9 $B4000011 LIT64,  9 9 10 ORR,  LCEMIT @ BL, ;

: J-UNTIL  $D1002273 C-EMITW  $F9400271 C-EMITW  J-UNTILX ;   \ pop flag -> x17

: J-WHILE C-POPFLAG  C-PUSHCP  $B4000009 C-EMITW ;

: J-REPEAT LVRECON @ BL,  LCFPOP @ BL,  14 9 0 ADDI,  LCFPOP @ BL,  $14000000 $3FFFFFF C-BBACK
   12 0 MOVZ,  12 DATA VSP-CELL STR,                  \ exit path arrives from
   12 VRALL MOVZ,  12 DATA VRFREE-CELL STR,           \ WHILE's spilled state
   9 14 0 ADDI,  LPAT @ BL, ;

\ DO/LOOP/I — loop index/limit live in a data-region frame stack ([x20+LOOP-STK-OFF],
\ depth [x20+LOOPSP-CELL]) since x27/x28 are the compiler's NDICT/CP. Fixed encodings
\ (computed offline). J-DO pushes a frame + records loop-top; J-LOOP increments the
\ index, compares, b.lt back, then pops the frame on exit; J-I pushes the index.
: J-FRAME  ( -- )                       \ pop limit/start, push a loop frame
   3506446963 C-EMITW  4181721705 C-EMITW  3506446963 C-EMITW  4181721706 C-EMITW
   4181780107 C-EMITW  3548179820 C-EMITW  2434269580 C-EMITW  2333344140 C-EMITW
   4177527177 C-EMITW  4177528202 C-EMITW  2432697707 C-EMITW  4177585803 C-EMITW ;

: J-LVOPEN  ( -- )                       \ open a LEAVE-chain level: LVH[LVD]=0, LVD++
   9 DATA LVD-CELL LDR,
   10 9 3 LSLI,  10 10 LVH-OFF ADDI,  10 DATA 10 ADD,
   12 0 MOVZ,  12 10 0 STR,
   9 9 1 ADDI,  9 DATA LVD-CELL STR, ;

: J-LVLEAVE  ( -- )                      \ chain a B placeholder on the current level
   9 DATA LVD-CELL LDR,  9 9 1 SUBI,
   10 9 3 LSLI,  10 10 LVH-OFF ADDI,  10 DATA 10 ADD,
   9 10 0 LDR,
   11 CP DBASE SUB,  11 10 0 STR,
   LCEMIT @ BL, ;

: J-DO  ( limit start DO )
   J-FRAME  J-LVOPEN  C-PUSHCP ;

: J-?DO ( limit start ?DO )              \ DO, but skip the loop when limit = start
   J-FRAME  J-LVOPEN
   $EB0A013F C-EMITW                     \ cmp x9,x10  (start/limit still live)
   $54000041 C-EMITW                     \ b.ne +8 (over the skip placeholder)
   J-LVLEAVE
   C-PUSHCP ;

: J-LEAVE  J-LVLEAVE ;

: J-UNLOOP                               \ pop one loop frame, no branch
   4181780107 C-EMITW  3506439531 C-EMITW  4177585803 C-EMITW ;

: J-LOOPEND  ( -- )                      \ shared LOOP/+LOOP tail: pop frame, patch
   14 CP 0 ADDI,                         \ LEAVE/?DO skips to the pop point, LVD--
   4181780107 C-EMITW  3506439531 C-EMITW  4177585803 C-EMITW
   9 DATA LVD-CELL LDR,  9 9 1 SUBI,  9 DATA LVD-CELL STR,
   10 9 3 LSLI,  10 10 LVH-OFF ADDI,  10 DATA 10 ADD,  9 10 0 LDR,
   LBCHAIN @ BL, ;

: J-LOOP
   4181780107 C-EMITW  3506439531 C-EMITW  3548179820 C-EMITW  2434269580 C-EMITW  2333344140 C-EMITW
   4181721481 C-EMITW  4181722506 C-EMITW  2432697641 C-EMITW  4177527177 C-EMITW  3943301439 C-EMITW
   LCFPOP @ BL,                                        \ x9 = loop-top
   10 9 CP SUB,  10 10 2 ASRI,  5 $7FFFF LIT64,  10 10 5 AND,  10 10 5 LSLI,
   9 $5400000B LIT64,  9 9 10 ORR,  LCEMIT @ BL,       \ b.lt loop-top
   J-LOOPEND ;

: J-+LOOP  ( n +LOOP )                   \ index += n; loop while (old-limit) and
   $D1002273 C-EMITW  $F9400269 C-EMITW  \ (new-limit) agree in sign (ANS crossing)
   4181780107 C-EMITW  3506439531 C-EMITW  3548179820 C-EMITW  2434269580 C-EMITW  2333344140 C-EMITW
   $F940018D C-EMITW                     \ ldr x13,[x12]      index
   4181722506 C-EMITW                    \ ldr x10,[x12,#8]   limit
   $CB0A01AF C-EMITW                     \ sub x15,x13,x10    old
   $8B0901AD C-EMITW                     \ add x13,x13,x9
   $F900018D C-EMITW                     \ str x13,[x12]
   $CB0A01B0 C-EMITW                     \ sub x16,x13,x10    new
   $CA1001EF C-EMITW                     \ eor x15,x15,x16
   $F10001FF C-EMITW                     \ cmp x15,#0
   LCFPOP @ BL,
   10 9 CP SUB,  10 10 2 ASRI,  5 $7FFFF LIT64,  10 10 5 AND,  10 10 5 LSLI,
   9 $5400000A LIT64,  9 9 10 ORR,  LCEMIT @ BL,       \ b.ge loop-top
   J-LOOPEND ;

: J-I
   4181780107 C-EMITW  3506439531 C-EMITW  3548179820 C-EMITW  2434269580 C-EMITW  2333344140 C-EMITW
   4181721481 C-EMITW  4177527401 C-EMITW  2432705139 C-EMITW ;

: J-J                                    \ outer loop index: frame[LOOPSP-2]
   4181780107 C-EMITW  $D100096B C-EMITW 3548179820 C-EMITW  2434269580 C-EMITW  2333344140 C-EMITW
   4181721481 C-EMITW  4177527401 C-EMITW  2432705139 C-EMITW ;

\ >R R> R@ — the user return stack lives in a data-region stack ([x20+RSTK-OFF],
\ depth at [x20+RSP-CELL]), like the DO/LOOP frames: x25/x28 belong to the
\ compiler, and word frames on the machine stack would unbalance the epilogue.
: W-LDRX {: rt RN off -- w :}                          \ ldr rt,[rn,#off]
   $F9400000  off 8 / 10 lshift or  RN 5 lshift or  rt or ;

: W-STRX {: rt RN off -- w :}                          \ str rt,[rn,#off]
   $F9000000  off 8 / 10 lshift or  RN 5 lshift or  rt or ;

: J-TOR                                                \ pop data -> push RSTK
   $D1002273 C-EMITW  $F9400269 C-EMITW                \ sub x19,#8 ; ldr x9,[x19]
   10 20 RSP-CELL W-LDRX C-EMITW
   $8B0A0E8B C-EMITW                                   \ add x11,x20,x10,lsl#3
   9 11 RSTK-OFF W-STRX C-EMITW
   $9100054A C-EMITW                                   \ add x10,x10,#1
   10 20 RSP-CELL W-STRX C-EMITW ;

: J-RPOP                                               \ x9 = RSTK top, x10 = RSP-1
   10 20 RSP-CELL W-LDRX C-EMITW
   $D100054A C-EMITW                                   \ sub x10,x10,#1
   $8B0A0E8B C-EMITW                                   \ add x11,x20,x10,lsl#3
   9 11 RSTK-OFF W-LDRX C-EMITW ;

: J-RFROM  J-RPOP                                      \ pop RSTK -> push data
   10 20 RSP-CELL W-STRX C-EMITW
   $F9000269 C-EMITW  $91002273 C-EMITW ;              \ str x9,[x19] ; add x19,#8

: J-RFETCH  J-RPOP                                     \ peek RSTK -> push data
   $F9000269 C-EMITW  $91002273 C-EMITW ;

\ EXIT: emit a placeholder word holding the PREVIOUS chain offset (0 = end);
\ `;` walks the chain and patches each into `b epilogue`. RECURSE: bl back to
\ the current word's entry (PEND slot.addr) — every word has the standard
\ prologue/epilogue, so calling into the open definition is well-formed.
: J-EXIT
   9 DATA EXITH-CELL LDR,                              \ x9 = prev chain offset
   10 CP DBASE SUB,  10 DATA EXITH-CELL STR,           \ head := this placeholder
   LCEMIT @ BL, ;

: J-RECURSE
   9 DATA PEND-CELL LDR,  9 9 0 LDR,  $94000000 $3FFFFFF C-BBACK ;   \ bl entry

\ DOES> — the defining word patches its LAST create into `push dfield ; b D`,
\ then exits; D (the does-body) follows with its own prologue and shares `;`'s
\ epilogue. The patch itself runs in LDOESPATCH (ENGINE text): flipping the
\ region to RW would un-map EXECUTE from the page the defining word runs on.
\ Locals BEFORE does> are refused (the shared teardown wouldn't match).
: J-DOES ( -- )
   NEWLBL {: dok :}
   12 DATA LOCF-CELL LDR,  12 dok CBZ,
      0 2 MOVZ,  1 TKA 0 ADDI,  2 TKL 0 ADDI,  NR-WRITE SYS,
      0 75 MOVZ,  NR-EXIT SYS,
   dok LBL,
   $1000008A C-EMITW                     \ adr x10, #+16 = D (4 words ahead)
   16 20 DOESP-CELL W-LDRX C-EMITW       \ x16 = LDOESPATCH runtime addr
   $D63F0200 C-EMITW                     \ blr x16
   J-EXIT                                \ word 4: the defining word ends here
   9 $D10043FF LIT64,  LCEMIT @ BL,      \ D: fresh prologue for the does-body
   9 $F90003FE LIT64,  LCEMIT @ BL, ;

\ [: ... ;] — an anonymous nested definition: [: jumps over the body, gives it
\ its own prologue; ;] closes it (epilogue + patch) and pushes its address as a
\ literal in the OUTER word (xt on the stack at outer runtime). One level; the
\ EXIT chain is scoped to the quotation; locals inside are refused.
: J-QUOT ( -- )
   NEWLBL {: qok :}
   9 DATA QPATCH-CELL LDR,  9 qok CBZ,
      0 2 MOVZ,  1 TKA 0 ADDI,  2 TKL 0 ADDI,  NR-WRITE SYS,
      0 75 MOVZ,  NR-EXIT SYS,
   qok LBL,
   9 CP 0 ADDI,  9 DATA QPATCH-CELL STR,
   9 $14000000 LIT64,  LCEMIT @ BL,               \ b-over placeholder
   9 CP 0 ADDI,  9 DATA QENT-CELL STR,            \ the quotation's entry
   9 DATA EXITH-CELL LDR,  9 DATA QXH-CELL STR,   \ scope the EXIT chain
   12 0 MOVZ,  12 DATA EXITH-CELL STR,
   9 $D10043FF LIT64,  LCEMIT @ BL,               \ its own prologue
   9 $F90003FE LIT64,  LCEMIT @ BL, ;

: J-SEMIQUOT ( -- )
   NEWLBL {: sqok :}
   9 DATA QPATCH-CELL LDR,  9 sqok CBNZ,
      0 2 MOVZ,  1 TKA 0 ADDI,  2 TKL 0 ADDI,  NR-WRITE SYS,
      0 75 MOVZ,  NR-EXIT SYS,
   sqok LBL,
   14 CP 0 ADDI,  9 DATA EXITH-CELL LDR,  LBCHAIN @ BL,   \ exits -> this epilogue
   9 DATA QXH-CELL LDR,  9 DATA EXITH-CELL STR,
   9 $F94003FE LIT64,  LCEMIT @ BL,                \ epilogue: ldr x30,[sp]
   9 $910043FF LIT64,  LCEMIT @ BL,                \ add sp,#16
   9 W-RET LIT64,  LCEMIT @ BL,
   9 DATA QPATCH-CELL LDR,  LPAT @ BL,             \ b-over lands here
   11 DATA QENT-CELL LDR,  C-LIT                   \ push the xt in the outer word
   12 0 MOVZ,  12 DATA QPATCH-CELL STR, ;

\ LDOESPATCH ( x10=D ): patch the last-created word's RET into `b D`.
\ Runs from engine text, so the region RW/RX flips are safe mid-execution.
: EMIT-DOESPATCH ( -- )
   LDOESPATCH @ LBL,
   SP SP 32 SUBI,  30 SP 0 STR,  10 SP 8 STR,
   2 3 MOVZ,  LPROT @ BL,                                \ region -> RW
   10 SP 8 LDR,
   11 DATA LASTC-CELL LDR,                               \ created slot
   12 11 0 LDR,  13 11 8 LDR,  12 12 13 ADD,             \ x12 = RET addr
   14 10 12 SUB,  14 14 2 ASRI,                          \ delta words (negative)
   5 $3FFFFFF LIT64,  14 14 5 AND,
   5 $14000000 LIT64,  14 14 5 ORR,                      \ b D
   14 12 0 STRW,
   12 SP 16 STR,
   2 5 MOVZ,  LPROT @ BL,                                \ region -> RX
   12 SP 16 LDR,
   12 DCCVAU,  DSB-ISH,  12 ICIVAU,  DSB-ISH,  ISB,      \ flush the patched line
   30 SP 0 LDR,  SP SP 32 ADDI,  RET, ;

\ CREATE/VARIABLE (interpret-mode defining words): make a dict word whose body
\ pushes the current DP (a data-space address). Reuses the `:` slot pattern + the
\ C-LIT emitter (with x11 = DP) for the literal-push body.
\ record defining words for the checker: append the kind token + run the hook
\ (verdict ignored — create/variable/constant always publish).
: C-DEFHOOK  NEWLBL {: kwv klen nohk :}
   11 kwv @ ADR,  12 klen MOVZ,  LBCS @ BL,
   9 DATA HOOK-CELL LDR,  9 nohk CBZ,
   10 DATA BODYBUF-OFF ADDI,  10 G-PUSH
   10 DATA BODYLEN-CELL LDR,  10 G-PUSH
   SP SP 16 SUBI,  30 SP 0 STR,  9 BLR,  30 SP 0 LDR,  SP SP 16 ADDI,
   10 G-POP
   nohk LBL, ;

\ CREATE as a BL-able routine: the interpret keyword AND the runtime `create`
\ prim share it, so defining words (`: CONST create , does> @ ;`) work.
\ LCREATE ( x15=top-level? ): the hook KIND record (`NAME create` -> sig -- n)
\ only applies to top-level creates — a word created INSIDE a defining word may
\ be does>-patched to any effect, so it publishes unrecorded; the author
\ declares it with `trust`.
: EMIT-CREATE ( -- )
   NEWLBL {: nokind :}
   LCREATE @ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,  15 SP 8 STR,
   2 3 MOVZ,  LPROT @ BL,                               \ region -> RW
   LTOK @ BL,                                            \ read NAME
   12 0 MOVZ,  12 DATA BODYLEN-CELL STR,  LBCAP @ BL,   \ seed "NAME " for the hook
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,   \ slot
   CP 9 0 STR,  TKL 9 16 STR,                            \ slot.addr=CP, namelen
   14 DATA CUR-CELL LDR,  14 9 40 STR,                   \ slot.wid = CURRENT
   10 9 24 ADDI,  11 TKA 0 ADDI,  12 TKL 0 ADDI,         \ copy name
   NEWLBL {: ncp :}  NEWLBL {: ncpd :}
   ncp LBL,  12 ncpd CBZ,  13 11 0 LDRB,  13 10 0 STRB,
      10 10 1 ADDI,  11 11 1 ADDI,  12 12 1 SUBI,  ncp B,
   ncpd LBL,
   11 DATA 0 LDR,                                        \ x11 = DP (body pushes it)
   C-LIT                                                 \ emit movz/movk x9=DP + push
   9 W-RET LIT64,  LCEMIT @ BL,                          \ emit RET
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,   \ slot again
   10 9 0 LDR,  10 CP 10 SUB,  10 10 4 SUBI,  10 9 8 STR,        \ clen = CP-addr-4
   9 DATA LASTC-CELL STR,                               \ DOES> patches this slot
   NDICT NDICT 1 ADDI,  9 9 0 LDR,                      \ x9 = body start for the flush
   2 5 MOVZ,  LPROT @ BL,  LFLUSH @ BL,                 \ region -> RX + flush
   15 SP 8 LDR,  15 nokind CBZ,
   LKWCREATE 6 C-DEFHOOK
   nokind LBL,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

: C-CREATE ( -- )  15 1 MOVZ,  LCREATE @ BL, ;

: C-VARIABLE ( -- )  C-CREATE
   7 DATA 0 LDR,  7 7 8 ADDI,  7 DATA 0 STR, ;          \ reserve 1 cell

\ CONSTANT ( n -- ) "name": define a word that pushes n. Pop n first (x15
\ survives the name copy), then emit a literal-push body via C-LIT (x11=n).
: C-CONSTANT ( -- )
   2 3 MOVZ,  LPROT @ BL,  LTOK @ BL,
   12 0 MOVZ,  12 DATA BODYLEN-CELL STR,  LBCAP @ BL,   \ seed "NAME " for the hook
   15 G-POP                                             \ n -> x15 AFTER LBCAP (it clobbers x15)
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   CP 9 0 STR,  TKL 9 16 STR,  14 DATA CUR-CELL LDR,  14 9 40 STR,
   10 9 24 ADDI,  11 TKA 0 ADDI,  12 TKL 0 ADDI,
   NEWLBL {: kcp :}  NEWLBL {: kcd :}
   kcp LBL,  12 kcd CBZ,  13 11 0 LDRB,  13 10 0 STRB,
      10 10 1 ADDI,  11 11 1 ADDI,  12 12 1 SUBI,  kcp B,
   kcd LBL,
   11 15 0 ADDI,  C-LIT                                 \ body: push n
   9 W-RET LIT64,  LCEMIT @ BL,
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   10 9 0 LDR,  10 CP 10 SUB,  10 10 4 SUBI,  10 9 8 STR,
   NDICT NDICT 1 ADDI,  9 9 0 LDR,                      \ x9 = body start for the flush
   2 5 MOVZ,  LPROT @ BL,  LFLUSH @ BL,
   LKWCONST 8 C-DEFHOOK ;

\ IMMEDIATE: mark the LAST defined word — the compile loop EXECUTES immediate
\ words instead of compiling calls (flag = bit $100 of slot.namelen).
: C-IMMEDIATE ( -- )
   2 3 MOVZ,  LPROT @ BL,                               \ dict lives in the RX region
   9 NDICT 0 ADDI,  9 9 1 SUBI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   10 9 16 LDR,  10 10 $100 ORRI,  10 9 16 STR,
   2 5 MOVZ,  LPROT @ BL, ;

\ POSTPONE NAME (compile): immediate -> compile the call; ordinary -> bake the
\ xt and compile a call to the `compile,` prim (appends the call at ITS runtime).
: C-POSTPONE ( -- )
   NEWLBL {: pok :}  NEWLBL {: pnimm :}  NEWLBL {: pdone :}
   LTOK @ BL,  9 TKA 0 ADDI,  10 TKL 0 ADDI,  LFIND @ BL,
   13 pok CBNZ,
      0 2 MOVZ,  1 TKA 0 ADDI,  2 TKL 0 ADDI,  NR-WRITE SYS,
      0 70 MOVZ,  NR-EXIT SYS,
   pok LBL,
   14 13 2 ANDI,  14 pnimm CBZ,
      C-CALL  pdone B,
   pnimm LBL,
      C-LIT                                              \ bake the xt (x11)
      9 LKWCOMPC @ ADR,  10 8 MOVZ,  LFIND @ BL,         \ find `compile,`
      C-CALL
   pdone LBL, ;

\ CHAR NAME (interpret): push NAME's first byte. [CHAR] NAME (compile): bake it
\ as a VS constant (folds like any literal).
: C-CHAR  ( -- )   LTOK @ BL,  9 TKA 0 LDRB,  9 G-PUSH ;

: C-BCHAR ( -- )   LTOK @ BL,  11 TKA 0 LDRB,  LVPUSHC @ BL, ;

\ ' NAME (interpret): find NAME, push its code address. ['] NAME (compile): bake
\ the address as a literal push into the word being compiled (via c-lit, x11=addr).
: C-TICK ( -- )
   LTOK @ BL,  9 TKA 0 ADDI,  10 TKL 0 ADDI,  LFIND @ BL,
   NEWLBL {: tk :}  13 tk CBZ,  11 G-PUSH  tk LBL, ;

: C-BTICK ( -- )
   LTOK @ BL,  9 TKA 0 ADDI,  10 TKL 0 ADDI,  LFIND @ BL,
   NEWLBL {: bk :}  13 bk CBZ,  C-LIT  bk LBL, ;

\ {: a b :} (compile): record the names in the locals table, carve a machine-stack
\ frame, and pop the declared values into slots (slot 0 = first/deepest name). The
\ frame is torn down at ';'. Local references are resolved by LLOC-FIND -> a load.
: C-LBRACE ( -- )
   \ FOOTGUN GUARD 1: {: inside IF/BEGIN/DO corrupts the frame (the CF stack is
   \ non-empty while compiling control flow) — refuse loudly: token + exit(75).
   NEWLBL {: cfok :}
   5 CFSTK-OFF LIT64,  10 DBASE 5 ADD,  11 10 0 LDR,  11 cfok CBZ,
      0 2 MOVZ,  1 TKA 0 ADDI,  2 TKL 0 ADDI,  NR-WRITE SYS,
      0 75 MOVZ,  NR-EXIT SYS,
   cfok LBL,
   \ FOOTGUN GUARD 1c: {: inside [: ;] — the locals frame belongs to the OUTER
   \ word; the quotation's epilogue would not tear it down. Refuse: exit(75).
   NEWLBL {: qlok :}
   11 DATA QPATCH-CELL LDR,  11 qlok CBZ,
      0 2 MOVZ,  1 TKA 0 ADDI,  2 TKL 0 ADDI,  NR-WRITE SYS,
      0 75 MOVZ,  NR-EXIT SYS,
   qlok LBL,
   \ FOOTGUN GUARD 1b: {: after EXIT — the patched epilogue would tear down a
   \ frame the exit path never carved. Refuse loudly: token + exit(75).
   NEWLBL {: xok :}
   11 DATA EXITH-CELL LDR,  11 xok CBZ,
      0 2 MOVZ,  1 TKA 0 ADDI,  2 TKL 0 ADDI,  NR-WRITE SYS,
      0 75 MOVZ,  NR-EXIT SYS,
   xok LBL,
   \ each {: :} group carves EXACTLY its own slots at ':}' — no fixed frame, no
   \ slot cap from the frame. A slot's sp offset is LOCF - 8*(slot+1): earlier
   \ slots shift UP by each later carve, and LOCF tracks the running total, so
   \ the offset stays compile-time computable. Teardown stays `add sp,#LOCF`.
   6 DATA LOCN-CELL LDR,                      \ x6 = start slot for this block (= current N)
   NEWLBL {: nl :}  NEWLBL {: nd :}  NEWLBL {: nstore :}  NEWLBL {: ncp :}  NEWLBL {: ncd :}
   nl LBL,
      LTOK @ BL,  0 nd CBZ,
      LBCAP @ BL,                                          \ locals reach the checker too
      0 LKWENDLOC @ ADR,  1 2 MOVZ,  LKWCMP @ BL,  0 nstore CBZ,  nd B,   \ ":}" -> done
      nstore LBL,
      \ cap: the LOCNAMES table holds 64 records — die loudly past it
      NEWLBL {: nlok :}
      11 DATA LOCN-CELL LDR,  11 64 CMPI,  C-LT nlok BCOND,
         0 2 MOVZ,  1 TKA 0 ADDI,  2 TKL 0 ADDI,  NR-WRITE SYS,
         0 75 MOVZ,  NR-EXIT SYS,
      nlok LBL,
      \ FOOTGUN GUARD 2: a local named i/I is shadowed by the loop-index keyword
      NEWLBL {: noti :}
      TKL 1 CMPI,  C-NE noti BCOND,
      13 TKA 0 LDRB,  14 $20 MOVZ,  13 13 14 ORR,  13 105 CMPI,  C-NE noti BCOND,
         0 2 MOVZ,  1 TKA 0 ADDI,  2 TKL 0 ADDI,  NR-WRITE SYS,
         0 75 MOVZ,  NR-EXIT SYS,
      noti LBL,
      11 DATA LOCN-CELL LDR,  12 LOC-REC MOVZ,  11 11 12 MUL,  5 LOCNAMES LIT64,  11 11 5 ADD,  11 DATA 11 ADD,
      \ typed local a:n — references use the BARE name; the :type suffix is
      \ checker-only (it reaches the hook via the body capture). x14 = bare len.
      NEWLBL {: tsl :}  NEWLBL {: tsd :}
      14 0 MOVZ,
      tsl LBL,  14 TKL CMP,  C-GE tsd BCOND,
         15 TKA 14 ADD,  15 15 0 LDRB,  15 58 CMPI,  C-EQ tsd BCOND,
         14 14 1 ADDI,  tsl B,
      tsd LBL,
      14 11 0 STR,                            \ entry.len = bare len
      12 11 8 ADDI,  13 TKA 0 ADDI,           \ copy bare bytes (x14 already the count)
      ncp LBL,  14 ncd CBZ,  15 13 0 LDRB, 15 12 0 STRB, 12 12 1 ADDI, 13 13 1 ADDI, 14 14 1 SUBI, ncp B,
      ncd LBL,
      11 DATA LOCN-CELL LDR,  11 11 1 ADDI,  11 DATA LOCN-CELL STR,   \ N++
      nl B,
   nd LBL,
   \ carve exactly this group's slots, bump LOCF, then pop top -> highest
   \ NEW slot at offset LOCF - 8*(i+1)
   13 DATA LOCN-CELL LDR,  14 13 6 SUB,       \ n = N - start
   5 14 3 LSLI,  5 5 15 ADDI,  5 5 $FFFFFFFFFFFFFFF0 ANDI,   \ carve = align16(n*8):
   9 $D10003FF LIT64,  15 5 10 LSLI,  9 9 15 ORR,  LCEMIT @ BL,   \ SP must stay 16-aligned
   15 DATA LOCF-CELL LDR,  15 15 5 ADD,  15 DATA LOCF-CELL STR,   \ (pad sits below the slots)
   12 DATA LOCF-CELL LDR,  12 12 3 LSRI,      \ x12 = total slots in the frame
   13 DATA LOCN-CELL LDR,  13 13 1 SUBI,      \ i = N-1
   NEWLBL {: pl :}  NEWLBL {: pd :}
   pl LBL,
      13 6 CMP,  C-LT pd BCOND,               \ i < start -> done
      9 $D1002273 LIT64,  LCEMIT @ BL,        \ sub x19,#8
      9 $F9400269 LIT64,  LCEMIT @ BL,        \ ldr x9,[x19]
      5 12 13 SUB,  5 5 1 SUBI,               \ scaled off = total - i - 1
      9 $F90003E9 LIT64,  5 5 10 LSLI,  9 9 5 ORR,  LCEMIT @ BL,   \ str x9,[sp,#off]
      13 13 1 SUBI,  pl B,
   pd LBL, ;

\ S" (interpret mode): copy the string to HERE (transient — no allot) and push
\ ( addr len ). Compile mode bakes bytes into the code image instead (c-sdq).
: C-ISDQ ( -- )
   INP INP 1 ADDI,  13 INP 0 ADDI,                      \ skip one space; x13 = start
   NEWLBL {: sl :}  NEWLBL {: sd :}
   sl LBL,  9 INP 0 LDRB,  9 $22 CMPI,  C-EQ sd BCOND,  INP INP 1 ADDI,  sl B,
   sd LBL,  10 INP 13 SUB,  INP INP 1 ADDI,             \ x10 = len; skip closing "
   12 DATA 0 LDR,  15 12 0 ADDI,                        \ x12 = DP, x15 = string base
   11 13 0 ADDI,  9 10 0 ADDI,
   NEWLBL {: cl :}  NEWLBL {: cd :}
   cl LBL,  9 cd CBZ,
      14 11 0 LDRB,  14 12 0 STRB,  12 12 1 ADDI,  11 11 1 ADDI,  9 9 1 SUBI,  cl B,
   cd LBL,
   12 DATA 0 STR,                                       \ allot: DP advances past the copy
   15 G-PUSH  10 G-PUSH ;

\ S" string" (compile mode): emit  B over the bytes ; <bytes> ; push abs-addr ;
\ push len. Bytes live in the RX code image; the absolute address is known at
\ compile time, so C-LIT pushes it (no PC-relative ADR needed).
: C-SDQ ( -- )
   INP INP 1 ADDI,  13 INP 0 ADDI,                      \ skip one space; x13 = start
   NEWLBL {: sl :}  NEWLBL {: sd :}
   sl LBL,  9 INP 0 LDRB,  9 $22 CMPI,  C-EQ sd BCOND,  INP INP 1 ADDI,  sl B,
   sd LBL,  10 INP 13 SUB,  INP INP 1 ADDI,             \ x10 = len; skip closing "
   15 CP 0 ADDI,  9 $14000000 LIT64,  LCEMIT @ BL,      \ x15 = B addr; emit B placeholder
   12 CP 0 ADDI,                                        \ x12 = byte addr (after the B)
   11 13 0 ADDI,  9 10 0 ADDI,                          \ copy x10 bytes start->CP
   NEWLBL {: cl :}  NEWLBL {: cd :}
   cl LBL,  9 cd CBZ,
      14 11 0 LDRB,  14 28 0 STRB,  28 28 1 ADDI,  11 11 1 ADDI,  9 9 1 SUBI,  cl B,
   cd LBL,
   28 28 3 ADDI,  5 -4 LIT64,  28 28 5 AND,             \ pad CP to 4
   9 15 0 ADDI,  15 10 0 ADDI,  LPAT @ BL,              \ x9=B addr; save len in x15; patch B->here
   11 12 0 ADDI,  C-LIT                                 \ push byte addr (x12)
   11 15 0 ADDI,  C-LIT ;                               \ push len (x15)

\ emit one compile-mode keyword case: if TKA/TKL == kw, run handler then back to lmain
: CF-ENTRY {: lmainlbl kwvar kwlen hxt -- :}
   0 kwvar @ ADR,  1 kwlen MOVZ,  LKWCMP @ BL,
   NEWLBL {: skip :}  0 skip CBZ,
   LVSPILL @ BL,
   hxt execute  lmainlbl B,
   skip LBL, ;

\ cfn-entry: keyword case WITHOUT the spill — loop words manage the VS
\ themselves (BEGIN snapshots it, AGAIN/REPEAT reconcile to the snapshot).
: CFN-ENTRY {: lmainlbl kwvar kwlen hxt -- :}
   0 kwvar @ ADR,  1 kwlen MOVZ,  LKWCMP @ BL,
   NEWLBL {: skip :}  0 skip CBZ,
   hxt execute  lmainlbl B,
   skip LBL, ;

variable CFSK
variable CFSK2

\ cfb-entry: branch keywords (if/until/while) with the condition on the VS —
\ a REGISTER top branches directly (no spill + memory pop); con or empty falls
\ back to the spill + pop path. hxtr gets the condition reg in x14.
: CFB-ENTRY {: lmainlbl kwvar kwlen hxtm hxtr :}
   NEWLBL CFSK !  NEWLBL CFSK2 !
   0 kwvar @ ADR,  1 kwlen MOVZ,  LKWCMP @ BL,
   0 CFSK @ CBZ,
   6 DATA VSP-CELL LDR,  6 CFSK2 @ CBZ,
   5 6 1 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   7 CFSK2 @ CBNZ,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  14 8 0 LDR,
   SP SP 16 SUBI,  14 SP 8 STR,
   LVDROP @ BL,  LVSPILL @ BL,
   14 SP 8 LDR,  SP SP 16 ADDI,
   hxtr execute
   lmainlbl B,
   CFSK2 @ LBL,
   LVSPILL @ BL,
   hxtm execute
   lmainlbl B,
   CFSK @ LBL, ;

\ cfbn-entry: like CFB-ENTRY but the register path neither spills nor saves —
\ UNTIL reconciles to the BEGIN snapshot itself; the condition reg x14 survives
\ LVDROP (which only relabels the VS, no emission).
: CFBN-ENTRY {: lmainlbl kwvar kwlen hxtm hxtr :}
   NEWLBL CFSK !  NEWLBL CFSK2 !
   0 kwvar @ ADR,  1 kwlen MOVZ,  LKWCMP @ BL,
   0 CFSK @ CBZ,
   6 DATA VSP-CELL LDR,  6 CFSK2 @ CBZ,
   5 6 1 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   7 CFSK2 @ CBNZ,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  14 8 0 LDR,
   LVDROP @ BL,
   hxtr execute
   lmainlbl B,
   CFSK2 @ LBL,
   LVSPILL @ BL,
   hxtm execute
   lmainlbl B,
   CFSK @ LBL, ;

: J-IFR  C-PUSHCP  8 $B4000000 LIT64,  9 8 14 ORR,  LCEMIT @ BL, ;

: J-WHILER  J-IFR ;

: J-UNTILR                                 \ reg flag -> x17 first: the reconcile
   8 $AA0003F1 LIT64,  7 14 16 LSLI,  9 8 7 ORR,  LCEMIT @ BL,   \ may reload into it
   J-UNTILX ;

\ ---- MAIN: startup (data stack + mmap + seed dict) then the outer interpreter ----
: EMIT-MAIN ( -- )
   LANCHOR @ LBL,
   13 0 0 ADDI,  14 1 0 ADDI,  15 2 0 ADDI,          \ main(argc,argv,envp) from dyld
   RBASE LANCHOR @ ADR,                              \ x20 = __TEXT base
   SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,  XDS SP 0 ADDI,                  \ data stack on machine sp
   \ mmap(0, REGION, PROT_READ|WRITE=3, MAP_ANON|MAP_PRIVATE=0x1002, -1, 0)
   0 RBASE-VA LIT64,  1 REGION LIT64,  2 3 MOVZ,  3 $1012 LIT64,  4 0 MOVN,  5 0 MOVZ,
   NR-MMAP SYS,
   5 RBASE-VA LIT64,  0 5 CMP,
   NEWLBL {: rvok :}  C-EQ rvok BCOND,
      0 78 MOVZ,  NR-EXIT SYS,                         \ fixed VA taken: die loudly
   rvok LBL,
   DBASE 0 0 ADDI,                                    \ x26 = region
   CP DBASE 0 ADDI,  5 DICT-SIZE LIT64,  CP CP 5 ADD, \ x28 = region + DICT-SIZE
   \ seed runtime dict from build-time dict (convert offsets -> absolute addr + clen)
   11 LNCOUNT @ ADR,  11 11 0 LDR,  NDICT 11 0 ADDI,  \ x27 = NDICT = seed count
   9 LDICT @ ADR,  10 DBASE 0 ADDI,  12 11 0 ADDI,    \ src, dst, i
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
   0 DATA-VA LIT64,  1 DATA-SIZE LIT64,  2 3 MOVZ,  3 $1012 LIT64,  4 0 MOVN,  5 0 MOVZ,
   NR-MMAP SYS,
   5 DATA-VA LIT64,  0 5 CMP,
   NEWLBL {: dvok :}  C-EQ dvok BCOND,
      0 78 MOVZ,  NR-EXIT SYS,
   dvok LBL,
   20 0 RBASE-CELL STR,                               \ save RBASE (x20=__TEXT base) into the data region
   DATA 0 0 ADDI,
   XDS DATA S0-CELL STR,                              \ save data-stack base for `.s`
   13 DATA ARGC-CELL STR,  14 DATA ARGV-CELL STR,  15 DATA ENVP-CELL STR,
   5 DATA-START MOVZ,  7 DATA 5 ADD,  7 DATA DP-CELL STR,   \ DP = base + header ($2800 > imm12)
   \ ---- AOT snapshot? (trailer at the end of our own __text). If present:
   \ restore both regions verbatim (fixed VAs keep region addresses valid),
   \ relocate engine-text call chains (the only ASLR-movers), boot WARM. ----
   NEWLBL {: snomag :}  NEWLBL {: sc1 :}  NEWLBL {: sc1d :}
   NEWLBL {: sc2 :}  NEWLBL {: sc2d :}
   NEWLBL {: srl :}  NEWLBL {: srn :}  NEWLBL {: srx :}  NEWLBL {: snapdone :}
   24 0 MOVZ,                                       \ x24 = snapshot flag
   9 DATA RBASE-CELL LDR,  25 9 0 ADDI,             \ x25 = live text CONTENT base
   10 9 0 ADDI,  5 $1000 LIT64,  10 10 5 SUB,
   11 10 216 LDR,                                   \ S = our __text size
   12 9 11 ADD,  12 12 40 SUBI,                     \ trailer
   13 12 0 LDR,  5 SNAP-MAGIC LIT64,  13 5 CMP,  C-NE snomag BCOND,
   21 12 8 LDR,                                     \ x21 = snapshot-time text base
   15 12 16 LDR,                                    \ x15 = ndict
   6 12 24 LDR,                                     \ x6 = region payload len
   7 12 32 LDR,                                     \ x7 = data payload len
   \ corrupt/truncated trailer must never smear the regions: exit 79
   NEWLBL {: snbad :}  NEWLBL {: snokz :}
   5 REGION LIT64,  6 5 CMP,  C-GT snbad BCOND,
   5 DATA-SIZE LIT64,  7 5 CMP,  C-GT snbad BCOND,
   5 1280 MOVZ,  15 5 CMP,  C-GT snbad BCOND,
   snokz B,
   snbad LBL,  0 79 MOVZ,  NR-EXIT SYS,
   snokz LBL,
   9 DATA ARGC-CELL LDR,  10 DATA ARGV-CELL LDR,  0 DATA ENVP-CELL LDR,
   22 11 6 SUB,  22 22 7 SUB,  22 22 40 SUBI,       \ x22 = engine text len then
   8 12 7 SUB,  8 8 6 SUB,                          \ region payload src
   13 DBASE 0 ADDI,  14 0 MOVZ,
   sc1 LBL,  14 6 CMP,  C-GE sc1d BCOND,
      3 8 14 ADD,  3 3 0 LDRB,  4 13 14 ADD,  3 4 0 STRB,
      14 14 1 ADDI,  sc1 B,
   sc1d LBL,
   8 12 7 SUB,  13 DATA 0 ADDI,  14 0 MOVZ,
   sc2 LBL,  14 7 CMP,  C-GE sc2d BCOND,
      3 8 14 ADD,  3 3 0 LDRB,  4 13 14 ADD,  3 4 0 STRB,
      14 14 1 ADDI,  sc2 B,
   sc2d LBL,
   25 DATA RBASE-CELL STR,                          \ live values over stale copies
   XDS DATA S0-CELL STR,
   9 DATA ARGC-CELL STR,  10 DATA ARGV-CELL STR,  0 DATA ENVP-CELL STR,
   NDICT 15 0 ADDI,
   CP DBASE 6 ADD,
   NEWLBL {: sdl2 :}  NEWLBL {: sdn2 :}  NEWLBL {: sds2 :}
   \ rebase seed-prim dict entries (slot.addr in the old engine text)
   9 DBASE 0 ADDI,  10 0 MOVZ,
   sdl2 LBL,  10 NDICT CMP,  C-GE sdn2 BCOND,
      13 9 0 LDR,
      13 21 CMP,  C-LT sds2 BCOND,
      14 21 22 ADD,  13 14 CMP,  C-GE sds2 BCOND,
      13 13 21 SUB,  13 13 25 ADD,  13 9 0 STR,
      sds2 LBL,  9 9 DREC ADDI,  10 10 1 ADDI,  sdl2 B,
   sdn2 LBL,
   \ relocation: movz/movk/movk x16 + blr x16 whose value sat in the OLD text
   9 DBASE 0 ADDI,  5 DICT-SIZE LIT64,  9 9 5 ADD,
   srl LBL,  9 CP CMP,  C-GE srx BCOND,
      10 9 0 LDRW,  5 $FFE0001F LIT64,  10 10 5 AND,
      5 $D2800010 LIT64,  10 5 CMP,  C-NE srn BCOND,
      10 9 4 LDRW,  5 $FFE0001F LIT64,  10 10 5 AND,
      5 $F2A00010 LIT64,  10 5 CMP,  C-NE srn BCOND,
      10 9 8 LDRW,  5 $FFE0001F LIT64,  10 10 5 AND,
      5 $F2C00010 LIT64,  10 5 CMP,  C-NE srn BCOND,
      10 9 12 LDRW,  5 $D63F0200 LIT64,  10 5 CMP,  C-NE srn BCOND,
      10 9 0 LDRW,  10 10 5 LSRI,  5 $FFFF LIT64,  10 10 5 AND,  13 10 0 ADDI,
      10 9 4 LDRW,  10 10 5 LSRI,  5 $FFFF LIT64,  10 10 5 AND,  10 10 16 LSLI,  13 13 10 ORR,
      10 9 8 LDRW,  10 10 5 LSRI,  5 $FFFF LIT64,  10 10 5 AND,  10 10 32 LSLI,  13 13 10 ORR,
      13 21 CMP,  C-LT srn BCOND,
      14 21 22 ADD,  13 14 CMP,  C-GE srn BCOND,
      13 13 21 SUB,  13 13 25 ADD,                  \ rebase into the live text
      10 9 0 LDRW,  5 $FFE0001F LIT64,  10 10 5 AND,
        14 13 0 ADDI,  5 $FFFF LIT64,  14 14 5 AND,  14 14 5 LSLI,  10 10 14 ORR,  10 9 0 STRW,
      10 9 4 LDRW,  5 $FFE0001F LIT64,  10 10 5 AND,
        14 13 16 LSRI,  5 $FFFF LIT64,  14 14 5 AND,  14 14 5 LSLI,  10 10 14 ORR,  10 9 4 STRW,
      10 9 8 LDRW,  5 $FFE0001F LIT64,  10 10 5 AND,
        14 13 32 LSRI,  5 $FFFF LIT64,  14 14 5 AND,  14 14 5 LSLI,  10 10 14 ORR,  10 9 8 STRW,
      9 9 12 ADDI,
   srn LBL,  9 9 4 ADDI,  srl B,
   srx LBL,
   2 5 MOVZ,  LPROT @ BL,                           \ region RX +
   9 DBASE 0 ADDI,  5 DICT-SIZE LIT64,  9 9 5 ADD,  LFLUSH @ BL,   \ coherent
   24 1 MOVZ,
   snomag LBL,
   9 0 MOVZ,  9 DATA HND-CELL STR,                    \ HND (catch handler chain) = 0
   NEWLBL {: cwok :}  24 cwok CBNZ,                   \ snapshot keeps warm CUR/WIDN/HOOK
   9 0 MOVZ,  9 DATA CUR-CELL STR,                    \ CURRENT wordlist = 0 (FORTH)
   9 1 MOVZ,  9 DATA WIDN-CELL STR,                   \ next fresh wid = 1
   9 0 MOVZ,  9 DATA HOOK-CELL STR,                   \ check hook = none
   cwok LBL,
   9 0 MOVZ,  9 DATA LOOPSP-CELL STR,                 \ DO/LOOP frame depth = 0
   G-INSTALL-CRASH                                    \ self-diagnosing crash (register dump)
   9 LDOESPATCH @ ADR,  9 DATA DOESP-CELL STR,
   9 LCREATE @ ADR,  9 DATA CREATEP-CELL STR,        \ DOES> patch routine addr
   9 LRREC @ ADR,  9 DATA RRECP-CELL STR,             \ throw's REPL recovery entry
   LVRINIT @ BL,                                     \ fill VRTAB/VRITAB from VRPACK
   EMIT-SOURCE                                        \ INP/INE <- baked LSRC or stdin
   9 0 MOVZ,  9 DATA PEND-CELL STR,                   \ interpret mode
   NEWLBL {: LMAIN :}  NEWLBL {: LEXIT :}  NEWLBL {: LCOMPILE :}  NEWLBL {: LUNDEF :}
   LMAIN LBL,
      LTOK @ BL,  0 LEXIT CBZ,
      \ skip comments (both modes): \ to end-of-line, ( to ')'
      NEWLBL {: notcom :}  NEWLBL {: skln :}  NEWLBL {: skpar :}
      TKL 1 CMPI,  C-NE notcom BCOND,
      9 TKA 0 LDRB,
      9 92 CMPI,  C-EQ skln BCOND,                       \ '\'
      9 40 CMPI,  C-NE notcom BCOND,                     \ '('
      skpar LBL,  INP INE CMP,  C-GE LMAIN BCOND,
         9 INP 0 LDRB,  INP INP 1 ADDI,  9 41 CMPI,  C-NE skpar BCOND,  LMAIN B,
      skln LBL,   INP INE CMP,  C-GE LMAIN BCOND,
         9 INP 0 LDRB,  INP INP 1 ADDI,  9 10 CMPI,  C-NE skln BCOND,  LMAIN B,
      notcom LBL,
      9 DATA PEND-CELL LDR,  9 LCOMPILE CBNZ,
      \ ---------------- INTERPRET ----------------
      NEWLBL {: lnotcolon :}
      TKL 1 CMPI,  C-NE lnotcolon BCOND,
      9 TKA 0 LDRB,  9 58 CMPI,  C-NE lnotcolon BCOND,     \ ':'
         2 3 MOVZ,  LPROT @ BL,                             \ region -> RW *before* any write
         NEWLBL {: cpok :}  NEWLBL {: ndok :}
         9 REGION $4000 - LIT64,  9 DBASE 9 ADD,  CP 9 CMP,  C-LT cpok BCOND,
            0 2 MOVZ,  1 TKA 0 ADDI,  2 TKL 0 ADDI,  NR-WRITE SYS,
            0 76 MOVZ,  NR-EXIT SYS,                    \ code region full
         cpok LBL,
         9 1280 MOVZ,  NDICT 9 CMP,  C-LT ndok BCOND,      \ slot 1280 = CFSTK-OFF
            0 2 MOVZ,  1 TKA 0 ADDI,  2 TKL 0 ADDI,  NR-WRITE SYS,
            0 77 MOVZ,  NR-EXIT SYS,                    \ dictionary full
         ndok LBL,
         LTOK @ BL,                                         \ read NAME
         9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,  \ slot
         9 DATA PEND-CELL STR,
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
         LBCAP @ BL,             \ seed with the NAME (checker records certified sigs)
         12 0 MOVZ,  12 DATA VSP-CELL STR,  12 DATA SNAPSP-CELL STR,     \ reset the VS
         12 DATA EXITH-CELL STR,  12 DATA LVD-CELL STR,                  \ reset EXIT/LEAVE chains
         12 DATA QPATCH-CELL STR,                                        \ reset quotation state
         12 VRALL MOVZ,  12 DATA VRFREE-CELL STR,
         9 $D10043FF LIT64,  LCEMIT @ BL,                  \ prologue: sub sp,sp,#16
         9 $F90003FE LIT64,  LCEMIT @ BL,                  \   str x30,[sp]  (slot.addr points here)
         LMAIN B,
      lnotcolon LBL,
      \ interpret-mode defining words + tick
      LMAIN LKWCREATE 6 ['] C-CREATE   CF-ENTRY
      LMAIN LKWVAR    8 ['] C-VARIABLE CF-ENTRY
      LMAIN LKWCONST  8 ['] C-CONSTANT CF-ENTRY
      LMAIN LKWTICK   1 ['] C-TICK     CF-ENTRY
      LMAIN LKWCHAR   4 ['] C-CHAR     CF-ENTRY
      LMAIN LKWIMM    9 ['] C-IMMEDIATE CF-ENTRY
      LMAIN LKWSQ     2 ['] C-ISDQ     CF-ENTRY
      9 TKA 0 ADDI,  10 TKL 0 ADDI,  LNUM @ BL,             \ NUMBER?
      NEWLBL {: lnotnum :}
      12 lnotnum CBZ,  11 G-PUSH  LMAIN B,
      lnotnum LBL,
      9 TKA 0 ADDI,  10 TKL 0 ADDI,  LFIND @ BL,            \ FIND
      13 LUNDEF CBZ,                                         \ unknown -> error (exit 70)
      11 BLR,  LMAIN B,                                      \ EXECUTE
      \ ---------------- COMPILE ----------------
   LCOMPILE LBL,
      NEWLBL {: lnotsemi :}
      TKL 1 CMPI,  C-NE lnotsemi BCOND,
      9 TKA 0 LDRB,  9 59 CMPI,  C-NE lnotsemi BCOND,       \ ';'
         LVSPILL @ BL,                                       \ VS -> real pushes first
         \ patch every EXIT placeholder to `b here` (epilogue, incl. teardown)
         14 CP 0 ADDI,  9 DATA EXITH-CELL LDR,  LBCHAIN @ BL,
         12 DATA LOCF-CELL LDR,  NEWLBL {: notd :}  12 notd CBZ,   \ tear down locals frame
            9 $910003FF LIT64,  14 12 10 LSLI,  9 9 14 ORR,  LCEMIT @ BL,   \ add sp,sp,#frame
         notd LBL,
         9 $F94003FE LIT64,  LCEMIT @ BL,                   \ epilogue: ldr x30,[sp]
         9 $910043FF LIT64,  LCEMIT @ BL,                   \   add sp,sp,#16
         9 W-RET LIT64,  LCEMIT @ BL,                       \ emit RET
         11 DATA PEND-CELL LDR,  9 11 0 LDR,  10 CP 9 SUB,  10 10 4 SUBI,  10 11 8 STR,  \ clen
         2 5 MOVZ,  LPROT @ BL,  LFLUSH @ BL,               \ region -> RX + flush (callable now)
         \ run the check hook on the captured body; publish only if it returns nonzero
         NEWLBL {: nohook :}  NEWLBL {: rejected :}
         9 DATA HOOK-CELL LDR,  9 nohook CBZ,
            10 DATA BODYBUF-OFF ADDI,  10 G-PUSH           \ ( body-addr )
            10 DATA BODYLEN-CELL LDR,  10 G-PUSH           \ ( body-len )
            SP SP 16 SUBI,  30 SP 0 STR,  9 BLR,  30 SP 0 LDR,  SP SP 16 ADDI,
            10 G-POP  10 rejected CBZ,                     \ ok==0 -> don't publish
         nohook LBL,
            NDICT NDICT 1 ADDI,                            \ publish word
         rejected LBL,
         9 0 MOVZ,  9 DATA PEND-CELL STR,                  \ leave compile mode
         LMAIN B,
      lnotsemi LBL,
      \ capture the token into the body buffer (for the check hook); space-joined.
      LBCAP @ BL,
      \ control-flow keywords (compile-only): emit/patch JIT branches, then loop
      LMAIN LKWIF     2 ['] J-IF   ['] J-IFR    CFB-ENTRY
      LMAIN LKWTHEN   4 ['] J-THEN   CF-ENTRY
      LMAIN LKWELSE   4 ['] J-ELSE   CF-ENTRY
      LMAIN LKWBEGIN  5 ['] J-BEGIN  CFN-ENTRY
      LMAIN LKWUNTIL  5 ['] J-UNTIL ['] J-UNTILR CFBN-ENTRY
      LMAIN LKWAGAIN  5 ['] J-AGAIN  CFN-ENTRY
      LMAIN LKWWHILE  5 ['] J-WHILE ['] J-WHILER CFB-ENTRY
      LMAIN LKWREPEAT 6 ['] J-REPEAT CFN-ENTRY
      LMAIN LKWSQ     2 ['] C-SDQ    CF-ENTRY            \ S" string"
      LMAIN LKWBTICK  3 ['] C-BTICK  CF-ENTRY            \ ['] NAME
      LMAIN LKWBCHAR  6 ['] C-BCHAR  CF-ENTRY            \ [CHAR] X
      LMAIN LKWPOST   8 ['] C-POSTPONE CF-ENTRY           \ POSTPONE NAME
      LMAIN LKWDOES   5 ['] J-DOES     CF-ENTRY           \ DOES>
      LMAIN LKWQUOT   2 ['] J-QUOT     CF-ENTRY           \ [:
      LMAIN LKWSEMIQ  2 ['] J-SEMIQUOT CF-ENTRY           \ ;]
      LMAIN LKWDO     2 ['] J-DO     CF-ENTRY            \ DO
      LMAIN LKWLOOP   4 ['] J-LOOP   CF-ENTRY            \ LOOP
      LMAIN LKWI      1 ['] J-I      CF-ENTRY            \ I
      LMAIN LKWTOR    2 ['] J-TOR    CF-ENTRY            \ >R
      LMAIN LKWRFROM  2 ['] J-RFROM  CF-ENTRY            \ R>
      LMAIN LKWRFET   2 ['] J-RFETCH CF-ENTRY            \ R@
      LMAIN LKWEXIT   4 ['] J-EXIT    CF-ENTRY            \ EXIT
      LMAIN LKWREC    7 ['] J-RECURSE CF-ENTRY            \ RECURSE
      LMAIN LKWQDO    3 ['] J-?DO     CF-ENTRY            \ ?DO
      LMAIN LKWPLOOP  5 ['] J-+LOOP   CF-ENTRY            \ +LOOP
      LMAIN LKWJ      1 ['] J-J       CF-ENTRY            \ J
      LMAIN LKWLEAVE  5 ['] J-LEAVE   CF-ENTRY            \ LEAVE
      LMAIN LKWUNLOOP 6 ['] J-UNLOOP  CF-ENTRY            \ UNLOOP
      LMAIN LKWLBRACE 2 ['] C-LBRACE CF-ENTRY            \ {: a b :} locals
      \ local-name reference -> load from its frame slot, push
      LLOC-FIND @ BL,  NEWLBL {: notloc :}  NEWLBL {: lmem :}  0 0 CMPI,  C-LT notloc BCOND,
         LVRALLOC @ BL,  14 lmem CBZ,                  \ local -> straight into a register
         7 DATA LOCF-CELL LDR,  7 7 3 LSRI,  7 7 0 SUB,  7 7 1 SUBI,   \ off = total-slot-1
         9 $F94003E0 LIT64,  9 9 14 ORR,  7 7 10 LSLI,  9 9 7 ORR,  LCEMIT @ BL,
         LVPUSHR @ BL,
         LMAIN B,
         lmem LBL,                                     \ no free reg: classic memory push
         LVSPILL @ BL,
         7 DATA LOCF-CELL LDR,  7 7 3 LSRI,  7 7 0 SUB,  7 7 1 SUBI,
         9 $F94003E9 LIT64,  7 7 10 LSLI,  9 9 7 ORR,  LCEMIT @ BL,   \ ldr x9,[sp,#off]
         9 W-PUSH0 LIT64,  LCEMIT @ BL,  9 W-PUSH1 LIT64,  LCEMIT @ BL,
         LMAIN B,
      notloc LBL,
      9 TKA 0 ADDI,  10 TKL 0 ADDI,  LNUM @ BL,             \ NUMBER? -> literal
      NEWLBL {: lcnotnum :}
      12 lcnotnum CBZ,  LVPUSHC @ BL,  LMAIN B,
      lcnotnum LBL,
      LMAIN LKWPLUS  1 ['] VF+ ['] E+ VOP-ENTRY
      LMAIN LKWMINUS 1 ['] VF- ['] E- VOP-ENTRY
      LMAIN LKWSTAR  1 ['] VF* ['] E* VOP-ENTRY
      LMAIN LKWAND2  3 ['] FAND ['] EAND VOP-ENTRY
      LMAIN LKWOR2   2 ['] FOR2 ['] EOR2 VOP-ENTRY
      LMAIN LKWXOR2  3 ['] FXOR2 ['] EXOR VOP-ENTRY
      LMAIN LKWDUP2  3 1 ['] XDUP  VSHUF-ENTRY
      LMAIN LKWDROP2 4 1 ['] XDROP VSHUF-ENTRY
      LMAIN LKWSWAP2 4 2 ['] XSWAP VSHUF-ENTRY
      LMAIN LKWOVER2 4 2 ['] XOVER VSHUF-ENTRY
      LMAIN LKWNIP2  3 2 ['] XNIP  VSHUF-ENTRY
      LMAIN LKWEQ2 1 0 VCMP-ENTRY
      LMAIN LKWNE2 2 1 VCMP-ENTRY
      LMAIN LKWLT2 1 11 VCMP-ENTRY
      LMAIN LKWGT2 1 12 VCMP-ENTRY
      LMAIN LKWLE2 2 13 VCMP-ENTRY
      LMAIN LKWGE2 2 10 VCMP-ENTRY
      LMAIN LKWINC  2 ['] FU1+ ['] EU1+ VUN-ENTRY
      LMAIN LKWDEC  2 ['] FU1- ['] EU1- VUN-ENTRY
      LMAIN LKWZEQ  2 ['] FU0= ['] EU0= VUN-ENTRY
      LMAIN LKWZLT  2 ['] FU0< ['] EU0< VUN-ENTRY
      LMAIN LKWNEG2 6 ['] FUNEG ['] EUNEG VUN-ENTRY
      LMAIN LKWINV2 6 ['] FUINV ['] EUINV VUN-ENTRY

      LVSPILL @ BL,                                          \ VS -> memory before a call
      9 TKA 0 ADDI,  10 TKL 0 ADDI,  LFIND @ BL,            \ FIND -> inline stencil
      13 LUNDEF CBZ,                                         \ undefined word in a : body -> error
      NEWLBL {: notimm :}
      14 13 2 ANDI,  14 notimm CBZ,                          \ IMMEDIATE: execute NOW
         SP SP 16 SUBI,  30 SP 0 STR,  11 SP 8 STR,
         2 5 MOVZ,  LPROT @ BL,                              \ region RX to run it
         11 SP 8 LDR,  11 BLR,
         2 3 MOVZ,  LPROT @ BL,                              \ back to RW (still compiling)
         30 SP 0 LDR,  SP SP 16 ADDI,
         LMAIN B,
      notimm LBL,
      C-CALL  LMAIN B,                                      \ x11=addr -> emit BL (no longer inline)
   \ undefined word during compilation: write the name to stderr and exit(70). Silently
   \ skipping it (the old behaviour) hid real bugs (e.g. `0<`, `STR=` -> no-op).
   LUNDEF LBL,
      0 2 MOVZ,  1 TKA 0 ADDI,  2 TKL 0 ADDI,  NR-WRITE SYS,   \ write(2, name)
      9 DATA REPLH-CELL LDR,  9 LRDIE @ CBZ,
   LRREC @ LBL,
      \ REPL recovery (also throw's no-handler target): "?", roll back the
      \ line's compile state, reset stacks AND the machine SP, read again
      0 2 MOVZ,  1 LQNL @ ADR,  2 2 MOVZ,  NR-WRITE SYS,
      CP DATA RSAVCP-CELL LDR,
      NDICT DATA RSAVND-CELL LDR,
      9 DATA RSAVDP-CELL LDR,  9 DATA DP-CELL STR,
      9 DATA S0-CELL LDR,  XDS 9 0 ADDI,
      9 0 MOVZ,
      9 DATA RSP-CELL STR,  9 DATA HND-CELL STR,  9 DATA LOOPSP-CELL STR,
      9 DATA LVD-CELL STR,  9 DATA VSP-CELL STR,  9 DATA QPATCH-CELL STR,
      9 DATA LOCN-CELL STR,  9 DATA BODYLEN-CELL STR,  9 DATA EXITH-CELL STR,
      9 DATA PEND-CELL STR,
      9 VRALL MOVZ,  9 DATA VRFREE-CELL STR,
      9 DATA RSAVSP-CELL LDR,  SP 9 0 ADDI,
      LREAD @ B,
   LRDIE @ LBL,
      0 70 MOVZ,  NR-EXIT SYS,                       \ exit(70)
   LEXIT LBL,
      9 DATA REPLH-CELL LDR,  9 LRBYE @ CBZ,
      0 1 MOVZ,  1 LOKS @ ADR,  2 4 MOVZ,  NR-WRITE SYS,        \ " ok"
   LREAD @ LBL,
      \ save line-start compile state, then call RD-LINE ( -- a u )
      9 SP 0 ADDI,  9 DATA RSAVSP-CELL STR,
      CP DATA RSAVCP-CELL STR,
      NDICT DATA RSAVND-CELL STR,
      9 DATA DP-CELL LDR,  9 DATA RSAVDP-CELL STR,
      9 DATA REPLH-CELL LDR,  9 BLR,
      XDS XDS 8 SUBI,  10 XDS 0 LDR,
      XDS XDS 8 SUBI,  11 XDS 0 LDR,
      10 LRBYE @ CBZ,                                 \ empty = EOF
      INP 11 0 ADDI,  INE 11 10 ADD,  LMAIN B,
   LRBYE @ LBL,
      0 0 MOVZ,  NR-EXIT SYS, ;                     \ exit(0)

: EMIT-FORTH ( src-a src-u -- )
   SRCN !  >r
   ICODE-RESET  CF-RESET  0 #PL !  0 PNP !
   NEWLBL LANCHOR !  NEWLBL LFIND !  NEWLBL LNUM !  NEWLBL LDICT !  NEWLBL LSRC !
   NEWLBL LCEMIT !  NEWLBL LTOK !  NEWLBL LPROT !  NEWLBL LFLUSH !  NEWLBL LNCOUNT !
   NEWLBL LBCAP !  NEWLBL LBCS !
   NEWLBL LCFPUSH !  NEWLBL LCFPOP !  NEWLBL LPAT !  NEWLBL LKWCMP !
   NEWLBL LBCHAIN !  NEWLBL LCREATE !  NEWLBL LDOESPATCH !
   NEWLBL LREAD !  NEWLBL LRBYE !  NEWLBL LRDIE !  NEWLBL LRREC !  NEWLBL LQNL !  NEWLBL LOKS !
   NEWLBL LKWIF !  NEWLBL LKWTHEN !  NEWLBL LKWELSE !  NEWLBL LKWBEGIN !
   NEWLBL LKWUNTIL !  NEWLBL LKWAGAIN !  NEWLBL LKWWHILE !  NEWLBL LKWREPEAT !
   NEWLBL LKWCREATE !  NEWLBL LKWVAR !  NEWLBL LKWSQ !
   NEWLBL LKWTICK !  NEWLBL LKWBTICK !
   NEWLBL LKWLBRACE !  NEWLBL LKWENDLOC !  NEWLBL LLOC-FIND !  NEWLBL LKWCONST !
   NEWLBL LKWDO !  NEWLBL LKWLOOP !  NEWLBL LKWI !
   NEWLBL LKWTOR !  NEWLBL LKWRFROM !  NEWLBL LKWRFET !
   NEWLBL LKWEXIT !  NEWLBL LKWREC !
   NEWLBL LKWQDO !  NEWLBL LKWPLOOP !  NEWLBL LKWJ !  NEWLBL LKWLEAVE !  NEWLBL LKWUNLOOP !
   NEWLBL LKWCHAR !  NEWLBL LKWBCHAR !
   NEWLBL LKWIMM !  NEWLBL LKWPOST !  NEWLBL LKWCOMPC !  NEWLBL LKWDOES !
   NEWLBL LKWQUOT !  NEWLBL LKWSEMIQ !
   NEWLBL LCRASHH !  NEWLBL LHEX !  NEWLBL LHDR !
   NEWLBL LPROFH !  NEWLBL LPROFDUMP !
   NEWLBL LVSPILL !  NEWLBL LVLITPUSH !  NEWLBL LVPUSHC !
   NEWLBL LVTOP2C !  NEWLBL LVFOLDPUT !
   NEWLBL LVRALLOC !  NEWLBL LVBIT !  NEWLBL LVRINIT !  NEWLBL LVMOVK !  NEWLBL LVFORCEK !  NEWLBL LVBINPREP !  NEWLBL LVPUSHR !
   NEWLBL LVDROP !  NEWLBL LVSWAPX !  NEWLBL LVNIPX !  NEWLBL LVCOPY !
   NEWLBL LVSNAP !  NEWLBL LVRECON !
   NEWLBL LKWPLUS !  NEWLBL LKWMINUS !  NEWLBL LKWSTAR !
   NEWLBL LKWAND2 !  NEWLBL LKWOR2 !  NEWLBL LKWXOR2 !
   NEWLBL LKWDUP2 !  NEWLBL LKWDROP2 !  NEWLBL LKWSWAP2 !
   NEWLBL LKWOVER2 !  NEWLBL LKWNIP2 !
   NEWLBL LKWEQ2 !  NEWLBL LKWNE2 !  NEWLBL LKWLT2 !
   NEWLBL LKWGT2 !  NEWLBL LKWLE2 !  NEWLBL LKWGE2 !
   NEWLBL LKWINC !  NEWLBL LKWDEC !  NEWLBL LKWZEQ !
   NEWLBL LKWZLT !  NEWLBL LKWNEG2 !  NEWLBL LKWINV2 !
   EMIT-MAIN                                              \ entry @ offset 0
   EMIT-PRIMS  EMIT-PROF-PRIMS  EMIT-FP-PRIMS  EMIT-CEMIT  EMIT-BCAP  EMIT-TOK  EMIT-PROT  EMIT-FLUSH  EMIT-FIND  EMIT-NUM
   EMIT-CREATE  EMIT-DOESPATCH
   EMIT-CF-HELPERS  EMIT-LOC-FIND  EMIT-KWDATA  EMIT-FOLDKW  EMIT-SHUFKW  EMIT-CMPKW  EMIT-UNKW  EMIT-CRASH-HANDLER  EMIT-HEX
   EMIT-PROFDUMP  EMIT-PROF  EMIT-JIT
   EMIT-DICT                                              \ after #PL is final
   LSRC @ LBL,  r> SRCN @ BYTES, ;

\ Build a standalone native Forth that interprets `src`, write it to `outfile`.
: FORTH-EXE ( src-a src-u out-a out-u -- )
   2>r  EMIT-FORTH  2r> EMIT-EXE ;

\ Build a standalone native Forth that reads its program from STDIN (batch REPL),
\ write it to `outfile`:  echo ': SQ DUP * ; 5 SQ .' | ./outfile
: FORTH-REPL-EXE ( out-a out-u -- )
   STDIN? on  s" "  ['] EMIT-FORTH catch  STDIN? off  throw  \ restore mode even on error
   EMIT-EXE ;

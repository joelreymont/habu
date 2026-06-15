\ habu1.f — the ENGINE BUILDER ported to the subset (from bootstrap/cg/forth.fs):
\ emits the standalone native Forth's primitives, helper routines, and seed
\ dictionary. Golden word-for-word vs habu in test/t-sh-habu1.fs. Needs asm.fs +
\ icode.fs + mnem.fs + rt.fs (g-push/g-pop/g-print9) + crash.fs + macho.fs.
\ Part 1: prims + tok/find/num/prot/flush/cemit + dict. The interpreter main
\ loop, keyword JIT and EMIT-FORTH follow in part 2 (habu2.f).
20 constant RBASE
26 constant DBASE  27 constant NDICT  28 constant CP
$100000 constant REGION
$300000000 constant RBASE-VA \ FIXED region VA: baked addresses survive re-runs (AOT)
$340000000 constant DATA-VA  \ FIXED data VA
$48425350414E5321 constant SNAP-MAGIC \ AOT snapshot trailer marker
$1C000  constant DICT-SIZE
48      constant DREC
$1B000  constant CFSTK-OFF
$200000 constant DATA-SIZE
$100000 constant IBUFSZ
20 constant DATA
0   constant DP-CELL    8  constant HND-CELL
16  constant LOCN-CELL   24 constant LOCF-CELL
$3000 constant LOCNAMES   \ 64 records x 24 B ($3000-$3600); was 16 at DATA+32
24  constant LOC-REC
$1A0 constant CUR-CELL
$1A8 constant WIDN-CELL
$1B0 constant HOOK-CELL
$1B8 constant BODYLEN-CELL
$1C0 constant RBASE-CELL
$1C8 constant LOOPSP-CELL
$1D0 constant S0-CELL
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
$3690 constant TKA-CELL    \ current token addr (was x23)
$3698 constant TKL-CELL    \ current token len  (was x24)
$36A0 constant INP-CELL    \ input cursor (was x21)
$36A8 constant INE-CELL    \ input end    (was x22)
$36C0 constant BPA-CELL    \ one-shot breakpoint addr (0 = none; debug.f sets)
$36C8 constant BPI-CELL    \ (legacy single-BP; unused)
$36D0 constant BPTAB-OFF   \ 16 breakpoints: (addr, saved-instr) 16 B each, addr 0 = empty
$2740 constant EVAL-FRAME  \ re-entrant evaluate save frame, 8 cells ($2740-$277F: the free hole
                           \ between BODYBUF end ($800+8000=$2740) and RSTK ($2800)). NOT $3600 —
                           \ that COLLIDED with VRTAB-OFF/$3600 + VRITAB-OFF/$3620 (regalloc.f), so every
                           \ evaluate clobbered the reg-alloc tables -> LVRALLOC returned saved stack-
                           \ pointer bytes as register numbers -> illegal SUB encodings (nondeterm SIGILL).
                           \ +0 INP +8 INE +16 RET +24 SP +32 XDS +40 CP +48 NDICT +56 DP
$37D0 constant EVALD-CELL  \ evaluate nesting depth (0 = top-level REPL/batch; gates the nested paths)
$37D8 constant EVALERR-CELL \ result of the last evaluate: 0 = clean, 1 = recovered from an error
$37E0 constant LMAINP-CELL  \ runtime addr of the interpret loop top (EM-STARTUP stores it; B-EVAL branches there)
$1D8 constant SSCR-CELL
$600 constant LOOP-STK-OFF
$800 constant BODYBUF-OFF
8000 constant BODYBUF-CAP
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
$3800 constant DATA-START
variable STDIN?   0 STDIN? !
\ runtime instruction-word constants the JIT compiler stamps out
$D65F03C0 constant W-RET
$F9000269 constant W-PUSH0
$91002273 constant W-PUSH1
$D2800009 constant W-MOVZ0
$F2A00009 constant W-MOVK1
$F2C00009 constant W-MOVK2
$F2E00009 constant W-MOVK3
\ --- primitive registry (build-side, for the seed dictionary) ---
128 constant PRIM-CAP
2048 constant PRIM-NAME-CAP
create PLBL PRIM-CAP cells allot   create PEL PRIM-CAP cells allot
create PLEN PRIM-CAP cells allot   create PNAM PRIM-CAP cells allot
create PNPOOL PRIM-NAME-CAP allot   variable PNP   variable #PL
variable RPD

: ?PRIM-SPACE {: na nu :} ( na nu -- )
   #PL @ PRIM-CAP >= IF s" primitive registry full" 76 die THEN
   PNP @ nu + PRIM-NAME-CAP > IF s" primitive name pool full" 76 die THEN ;

: REG-PRIM {: na nu lbl elbl :}
   na nu ?PRIM-SPACE
   lbl  #PL @ cells PLBL + !
   elbl #PL @ cells PEL  + !
   nu   #PL @ cells PLEN + !
   PNPOOL PNP @ + RPD !  RPD @ #PL @ cells PNAM + !
   0 BEGIN dup nu < WHILE  dup na + c@  over RPD @ + c!  1 + REPEAT drop
   PNP @ nu + PNP !  #PL @ 1 + #PL ! ;
variable FPL  variable FPE

: FPRIM {: na nu xt :}
   na nu KEEP? 0 = IF EXIT THEN
   LBL FPL !  LBL FPE !
   na nu FPL @ FPE @ REG-PRIM
   FPL @ LBL,  SP SP 16 SUBI,  30 SP 0 STR,
   xt execute  30 SP 0 LDR,  SP SP 16 ADDI,  RET,  FPE @ LBL, ;
s" fprim" s" n n n --" TRUST

: FPRIM-L {: na nu xt :}               \ LEAF prim: no BL/BLR in body -> no x30 frame
   na nu KEEP? 0 = IF EXIT THEN
   LBL FPL !  LBL FPE !
   na nu FPL @ FPE @ REG-PRIM
   FPL @ LBL,  xt execute  RET,  FPE @ LBL, ;
s" fprim-l" s" n n n --" TRUST
\ shared label ids (forward refs)
variable LANCHOR  variable LFIND  variable LNUM  variable LDICT  variable LSRC  variable SRCN
variable LCEMIT   variable LTOK   variable LPROT  variable LFLUSH variable LNCOUNT
variable LCFPUSH  variable LCFPOP  variable LPAT   variable LKWCMP  variable LBCAP  variable LBCS
variable LBCHAIN  variable LCREATE  variable LDOESPATCH
variable LKWIF    variable LKWTHEN variable LKWELSE variable LKWBEGIN
variable LKWUNTIL variable LKWAGAIN variable LKWWHILE variable LKWREPEAT
variable LKWCREATE variable LKWVAR variable LKWSQ variable LKWTICK variable LKWBTICK
variable LKWLBRACE variable LKWENDLOC variable LLOC-FIND variable LKWCONST
variable LKWDO variable LKWLOOP variable LKWI
variable LKWTOR variable LKWRFROM variable LKWRFET
variable LKWEXIT variable LKWREC
variable LKWQDO variable LKWPLOOP variable LKWJ variable LKWLEAVE variable LKWUNLOOP
variable LKWCHAR variable LKWBCHAR
variable LKWIMM variable LKWPOST variable LKWCOMPC
variable LKWDOES variable LKWQUOT variable LKWSEMIQ
9 constant A   10 constant B   11 constant C

\ ---- primitive bodies (operate on the x19 data stack) ----
: B+   B G-POP  A G-POP  A A B ADD,  A G-PUSH ;

: B-   B G-POP  A G-POP  A A B SUB,  A G-PUSH ;

: B*   B G-POP  A G-POP  A A B MUL,  A G-PUSH ;

: BDUP  A G-POP  A G-PUSH  A G-PUSH ;

: BDROP XDS XDS 8 SUBI, ;

: BSWAP A G-POP  B G-POP  A G-PUSH  B G-PUSH ;

: BDOT  A G-POP  G-PRINT9 ;

: BU.   A G-POP  G-PRINTU9 ;

: BRUNRC  A G-POP                    \ ( pathz -- rc ) spawn+wait; -1 = spawn failed
   LBL LBL LBL {: spok spdn spw :}
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

: BPIPE                              \ ( -- rfd wfd rc ) rc=0, or -1 -1 -1
   LBL LBL {: pok pdn :}
   NR-PIPE SYS,
   9 C-CS CSET,  9 pok CBZ,
      9 0 MOVN,  9 G-PUSH  9 G-PUSH  9 G-PUSH  pdn B,
   pok LBL,
   0 G-PUSH  1 G-PUSH  9 0 MOVZ,  9 G-PUSH
   pdn LBL, ;

: BDUP2  B G-POP  A G-POP            \ ( oldfd newfd -- rc ) rc=newfd or -1
   LBL LBL {: dok ddn :}
   NR-DUP2 SYS,
   9 C-CS CSET,  9 dok CBZ,
      0 0 MOVN,  ddn B,
   dok LBL,
   ddn LBL,
   0 G-PUSH ;

: BFCNTL  2 G-POP  1 G-POP  0 G-POP  \ ( fd cmd arg -- rc ) rc=sysret or -1
   LBL LBL {: fok fdn :}
   NR-FCNTL SYS,
   9 C-CS CSET,  9 fok CBZ,
      0 0 MOVN,  fdn B,
   fok LBL,
   fdn LBL,
   0 G-PUSH ;

: BPOLL  2 G-POP  1 G-POP  0 G-POP   \ ( fds nfds timeout -- rc ) rc=nready/0 or -1
   LBL LBL {: pok pdn :}
   NR-POLL SYS,
   9 C-CS CSET,  9 pok CBZ,
      0 0 MOVN,  pdn B,
   pok LBL,
   pdn LBL,
   0 G-PUSH ;

: BWAITRC  A G-POP                    \ ( pid -- rc ) wait4; -1 = wait failed
   LBL LBL {: wok wdn :}
   SP SP 16 SUBI,
   0 9 0 ADDI,
   1 SP 0 ADDI,  2 0 MOVZ,  3 0 MOVZ,
   NR-WAIT4 SYS,
   9 C-CS CSET,  9 wok CBZ,
      9 0 MOVN,  wdn B,
   wok LBL,
   9 SP 0 LDRW,
   9 9 8 LSRI,  9 9 $FF ANDI,
   wdn LBL,
   9 G-PUSH
   SP SP 16 ADDI, ;

\ Emit one PSFA_DUP2 record into the runtime file-actions blob at x13.
: SPAWN-DUP2-ACTION {: fdreg newfd :}
   LBL {: skip :}
   fdreg 0 CMPI,  C-LT skip BCOND,
   14 13 4 LDRW,  15 1040 MOVZ,  14 14 15 MUL,
   14 14 8 ADDI,  14 14 13 ADD,
   15 2 MOVZ,  15 14 0 STRW,
   fdreg 14 4 STRW,
   15 newfd MOVZ,  15 14 8 STRW,
   14 13 4 LDRW,  14 14 1 ADDI,  14 13 4 STRW,
   skip LBL, ;
s" spawn-dup2-action" s" n n --" TRUST

: BSPAWNIO                            \ ( pathz stdinfd stdoutfd stderrfd -- pid|-1 )
   12 G-POP  11 G-POP  10 G-POP  9 G-POP
   LBL LBL {: spok spdn :}
   SP SP 3584 SUBI,
   9 SP 16 STR,                       \ argv[0] = path
   14 0 MOVZ,  14 SP 24 STR,          \ argv[1] = 0
   14 SP 32 STR,                      \ envp[0] = 0
   13 SP 176 ADDI,                    \ file actions
   14 3 MOVZ,  14 13 0 STRW,
   14 0 MOVZ,  14 13 4 STRW,
   10 0 SPAWN-DUP2-ACTION
   11 1 SPAWN-DUP2-ACTION
   12 2 SPAWN-DUP2-ACTION
   14 0 MOVZ,                         \ zero the descriptor
   14 SP 48 STR,  14 SP 56 STR,  14 SP 64 STR,  14 SP 72 STR,
   14 SP 80 STR,  14 SP 88 STR,  14 SP 96 STR,  14 SP 104 STR,
   14 SP 112 STR,  14 SP 120 STR,  14 SP 128 STR,  14 SP 136 STR,
   14 SP 144 STR,  14 SP 152 STR,  14 SP 160 STR,  14 SP 168 STR,
   14 13 4 LDRW,  15 1040 MOVZ,  14 14 15 MUL,  14 14 8 ADDI,
   14 SP 64 STR,                      \ adesc.file_actions_size
   13 SP 72 STR,                      \ adesc.file_actions
   0 SP 0 ADDI,                       \ &pid
   1 9 0 ADDI,                        \ path
   14 13 4 LDRW,  2 SP 48 ADDI,
   LBL {: sad :}
   14 sad CBNZ,
      2 0 MOVZ,                       \ no actions: XNU rejects an empty blob
   sad LBL,
   3 SP 16 ADDI,  4 SP 32 ADDI,       \ argv, envp
   NR-SPAWN SYS,
   9 C-CS CSET,  9 9 0 ORR,  9 spok CBZ,
      9 0 MOVN,  spdn B,
   spok LBL,
   9 SP 0 LDR,
   spdn LBL,
   9 G-PUSH
   SP SP 3584 ADDI, ;

: BCPFETCH    9 CP 0 ADDI,  A G-PUSH ;     \ ( -- addr ) live CP (snapshot writer)
: BNDICTFETCH 9 NDICT 0 ADDI,  A G-PUSH ;  \ ( -- n ) live dict count
: BDBASEFETCH 9 DBASE 0 ADDI,  A G-PUSH ;  \ ( -- addr ) region base
: BCPSET   A G-POP  CP A 0 ADDI, ;         \ ( addr -- ) set CP — forget code back to a mark
: BNDSET   A G-POP  NDICT A 0 ADDI, ;      \ ( n -- ) set NDICT — forget dict entries past a mark

: BEPOCHSECONDS ( -- )
   LBL {: ok :}
   SP SP 16 SUBI,
   0 SP 0 ADDI,  1 0 MOVZ,  NR-GETTIMEOFDAY SYS,
   9 C-CS CSET,  9 9 0 ORR,  9 0 CMPI,  C-EQ ok BCOND,  BRK,
   ok LBL,
   9 SP 0 LDR,  9 G-PUSH
   SP SP 16 ADDI, ;

\ Monotonic nanoseconds for benchmarks. Darwin exposes `clock_gettime` and
\ `mach_absolute_time` through libSystem/commpage APIs, not this raw-syscall
\ engine. On arm64 macOS, EL0 can read CNTVCT_EL0 and CNTFRQ_EL0 directly; use
\ quotient/remainder conversion so the tick*1e9 multiply cannot overflow.
: BMONONS ( -- )
   LBL {: ok :}
   $D53BE049 EMITW  $D53BE00A EMITW         \ mrs x9,CNTVCT_EL0 ; mrs x10,CNTFRQ_EL0
   10 ok CBNZ,  BRK,  ok LBL,
   11 9 10 UDIV,                            \ q = ticks / freq
   12 11 10 MUL,  9 9 12 SUB,               \ r = ticks % freq
   13 $3B9ACA00 LIT64,                      \ 1_000_000_000 ns/s
   11 11 13 MUL,
   9 9 13 MUL,  9 9 10 UDIV,
   9 11 9 ADD,  9 G-PUSH ;

\ ( a u -- ) re-entrant interpret of the string a/u in this process: save the
\ outer input cursor + compile state, point INP/INE at a/u, bump EVALD, and jump
\ to the interpret loop top (its runtime addr in LMAINP-CELL — prims can't name
\ labels). End-of-buffer (LEXIT) and an error (LUNDEF), when EVALD>0, restore the
\ frame and return here. Sets EVALERR-CELL: 0 = clean, 1 = recovered from an error.
: B-EVAL
   B G-POP  A G-POP                                  \ x10 = u, x9 = a
   14 EVAL-FRAME LIT64,  14 DATA 14 ADD,             \ x14 = &frame
   11 DATA INP-CELL LDR,  11 14 0 STR,
   12 DATA INE-CELL LDR,  12 14 8 STR,
   30 14 16 STR,                                     \ leaf prim: x30 = caller return
   11 SP 0 ADDI,  11 14 24 STR,
   XDS 14 32 STR,  CP 14 40 STR,  NDICT 14 48 STR,
   11 DATA DP-CELL LDR,  11 14 56 STR,
   11 DATA EVALD-CELL LDR,  11 11 1 ADDI,  11 DATA EVALD-CELL STR,
   9 DATA INP-CELL STR,                              \ INP = a
   11 9 10 ADD,  11 DATA INE-CELL STR,               \ INE = a + u
   9 DATA LMAINP-CELL LDR,  9 BR, ;

: BCREATE  15 0 MOVZ,  16 20 CREATEP-CELL LDR,  16 BLR, ;   \ ( "name" -- ) runtime CREATE via the
                                     \ startup-stored cell: subsets emit prims w/o labels

: BCOMPILE  A G-POP  11 9 0 ADDI,
   SP SP 16 SUBI,  11 SP 8 STR,
   2 3 MOVZ,  LPROT @ BL,
   11 SP 8 LDR,
   5 $FFFF MOVZ,
   7 11 5 AND,    7 7 5 LSLI,  8 $D2800010 LIT64,  9 8 7 ORR,  LCEMIT @ BL,
   7 11 16 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 $F2A00010 LIT64,  9 8 7 ORR,  LCEMIT @ BL,
   7 11 32 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 $F2C00010 LIT64,  9 8 7 ORR,  LCEMIT @ BL,
   9 $D63F0200 LIT64,  LCEMIT @ BL,
   2 5 MOVZ,  LPROT @ BL,
   SP SP 16 ADDI, ;

: BEMIT A G-POP  13 9 0 ADDI,  G-EMITC ;

: BCR   13 10 MOVZ,  G-EMITC ;

: BSPACE 13 32 MOVZ,  G-EMITC ;

: B.S
   LBL LBL {: sl sd :}
   9 DATA S0-CELL LDR,  9 DATA SSCR-CELL STR,
   sl LBL,
      9 DATA SSCR-CELL LDR,  9 XDS CMP,  C-GE sd BCOND,
      9 9 0 LDR,  G-PRINT9
      9 DATA SSCR-CELL LDR,  9 9 8 ADDI,  9 DATA SSCR-CELL STR,
      sl B,
   sd LBL, ;

: (CMP) {: cond :}  B G-POP  A G-POP  A B CMP,  A cond CSET,  A SP A SUB,  A G-PUSH ;

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

: BAND B G-POP A G-POP  A A B AND, A G-PUSH ;

: BOR  B G-POP A G-POP  A A B ORR, A G-PUSH ;

: BXOR B G-POP A G-POP  A A B EOR, A G-PUSH ;

: BINV A G-POP  B 0 MOVN,  A A B EOR,  A G-PUSH ;

: BNEG A G-POP  A SP A SUB,  A G-PUSH ;

: BLSH B G-POP A G-POP  A A B LSLV, A G-PUSH ;

: BRSH B G-POP A G-POP  A A B LSRV, A G-PUSH ;

: BDIV0? LBL {: lok :} B lok CBNZ, BRK, lok LBL, ;   \ SDIV by 0 silently yields 0; trap a zero divisor (B)

: BDIV B G-POP A G-POP  BDIV0?  A A B SDIV, A G-PUSH ;

: BMOD B G-POP A G-POP  BDIV0?  C A B SDIV,  C C B MUL,  A A C SUB,  A G-PUSH ;

: BNIP  A G-POP  XDS XDS 8 SUBI,  A G-PUSH ;

: BOVER B G-POP A G-POP  A G-PUSH B G-PUSH A G-PUSH ;

: BTUCK B G-POP A G-POP  B G-PUSH A G-PUSH B G-PUSH ;

: BROT  C G-POP B G-POP A G-POP  B G-PUSH C G-PUSH A G-PUSH ;

: BMROT C G-POP B G-POP A G-POP  C G-PUSH A G-PUSH B G-PUSH ;

: B2DUP B G-POP A G-POP  A G-PUSH B G-PUSH A G-PUSH B G-PUSH ;

: B2DROP XDS XDS 16 SUBI, ;

: BFETCH  A G-POP  A A 0 LDR,  A G-PUSH ;

: BSTORE  B G-POP A G-POP  A B 0 STR, ;

: BCFETCH A G-POP  A A 0 LDRB, A G-PUSH ;

: BCSTORE B G-POP A G-POP  A B 0 STRB, ;

: BCELLS  A G-POP  A A 3 LSLI, A G-PUSH ;

: BHERE   7 DATA 0 LDR,  7 G-PUSH ;

: BALLOT  A G-POP  7 DATA 0 LDR,  7 7 A ADD,  7 DATA 0 STR, ;

: BCOMMA  A G-POP  7 DATA 0 LDR,  A 7 0 STR,  7 7 8 ADDI,  7 DATA 0 STR, ;

: BCCOMMA A G-POP  7 DATA 0 LDR,  A 7 0 STRB, 7 7 1 ADDI,  7 DATA 0 STR, ;

: BTYPE   2 G-POP  1 G-POP  0 1 MOVZ,  NR-WRITE SYS, ;

: BDIE    7 G-POP  2 G-POP  1 G-POP  0 2 MOVZ,  NR-WRITE SYS,
          0 7 0 ADDI,  NR-EXIT SYS, ;

: SYS-PUSH                         \ ( -- ) push x0, or -1 when the syscall carry is set
   LBL LBL {: ok done :}
   9 C-CS CSET,  9 ok CBZ,
      0 0 MOVN,  done B,
   ok LBL,
   done LBL,
   0 G-PUSH ;

: BOPEN   2 G-POP  1 G-POP  0 G-POP  NR-OPEN SYS,  0 G-PUSH ;

: BWRITE  2 G-POP  1 G-POP  0 G-POP  NR-WRITE SYS,  0 G-PUSH ;

: BREAD   2 G-POP  1 G-POP  0 G-POP  NR-READ SYS,  0 G-PUSH ;

: BIOCTL  2 G-POP  1 G-POP  0 G-POP  NR-IOCTL SYS,  0 G-PUSH ;

: BOPENRD A G-POP  0 9 0 ADDI,  1 0 MOVZ,  2 0 MOVZ,  NR-OPEN SYS,  SYS-PUSH ;

: BACCESS 1 G-POP  0 G-POP  NR-ACCESS SYS,  SYS-PUSH ;

: BSTAT64 1 G-POP  0 G-POP  NR-STAT64 SYS,  SYS-PUSH ;

: BGETDIRENTRIES64
   3 G-POP  2 G-POP  1 G-POP  0 G-POP  NR-GETDIRENTRIES64 SYS,  SYS-PUSH ;

: BPATCH32                       \ ( w addr -- ): RW-flip, store, RX, cache-sync —
   A G-POP  B G-POP              \ all inside ENGINE text (a JIT-resident caller
   SP SP 32 SUBI,                \ flipping the region would unmap ITSELF)
   A SP 8 STR,  B SP 16 STR,
   2 3 MOVZ,  LPROT @ BL,
   9 SP 8 LDR,  10 SP 16 LDR,  10 9 0 STRW,
   2 5 MOVZ,  LPROT @ BL,
   9 SP 8 LDR,  LFLUSH @ BL,
   SP SP 32 ADDI, ;

: BCLOSE  0 G-POP  NR-CLOSE SYS, ;

: BRBASE  9 DATA RBASE-CELL LDR,  9 G-PUSH ;

: BEXEC   A G-POP  SP SP 16 SUBI,  30 SP 0 STR,  A BLR,  30 SP 0 LDR,  SP SP 16 ADDI, ;

: BCATCH
   LBL LBL {: lres lpush :}
   A G-POP
   SP SP 48 SUBI,
   30 SP 32 STR,
   11 DATA 8 LDR,  11 SP 0 STR,
   19 SP 8 STR,
   13 SP 48 ADDI,  13 SP 16 STR,
   12 lres ADR,  12 SP 24 STR,
   14 SP 0 ADDI,  14 DATA 8 STR,
   9 BLR,
   11 SP 0 LDR,  11 DATA 8 STR,
   30 SP 32 LDR,  SP SP 48 ADDI,
   9 0 MOVZ,  lpush B,
   lres LBL,
   lpush LBL,  9 G-PUSH ;

: BTHROW
   LBL {: lnoh :}
   A G-POP
   11 DATA 8 LDR,
   11 lnoh CBZ,
   19 11 8 LDR,
   10 11 0 LDR,  10 DATA 8 STR,
   30 11 32 LDR,  12 11 24 LDR,  13 11 16 LDR,
   SP 13 0 ADDI,  12 BR,
   lnoh LBL,
   10 DATA REPLH-CELL LDR,  LBL {: lnorec :}  10 lnorec CBZ,
   10 DATA RRECP-CELL LDR,  10 BR,
   lnorec LBL,  0 9 0 ADDI,  NR-EXIT SYS, ;

: BWORDLIST  9 DATA WIDN-CELL LDR,  9 G-PUSH  9 9 1 ADDI,  9 DATA WIDN-CELL STR, ;

: BGETCUR    9 DATA CUR-CELL LDR,  9 G-PUSH ;

: BSETCUR    A G-POP  A DATA CUR-CELL STR, ;

: BSETCHECK  A G-POP  A DATA HOOK-CELL STR, ;

: BSWL
   LBL LBL LBL LBL LBL LBL LBL {: wl wend wnext wcmp wmatch wf1 wf2 :}
   2 G-POP  1 G-POP  0 G-POP
   3 $20 MOVZ,  5 DBASE 0 ADDI,  6 NDICT 0 ADDI,  11 0 MOVZ,
   wl LBL,  6 wend CBZ,
      9 5 40 LDR,  9 2 CMP,  C-NE wnext BCOND,
      9 5 16 LDR,  9 1 CMP,  C-NE wnext BCOND,
      7 0 MOVZ,
      wcmp LBL,  7 1 CMP,  C-GE wmatch BCOND,
         9 5 24 ADDI,  9 9 7 ADD,  9 9 0 LDRB,
         9 $41 CMPI,  C-LT wf1 BCOND,  9 $5A CMPI,  C-GT wf1 BCOND,  9 9 3 ORR,
         wf1 LBL,
         10 0 7 ADD,  10 10 0 LDRB,
         10 $41 CMPI,  C-LT wf2 BCOND,  10 $5A CMPI,  C-GT wf2 BCOND,  10 10 3 ORR,
         wf2 LBL,
         9 10 CMP,  C-NE wnext BCOND,
         7 7 1 ADDI,  wcmp B,
      wmatch LBL,  11 5 0 LDR,  wnext B,
      wnext LBL,  5 5 DREC ADDI,  6 6 1 SUBI,  wl B,
   wend LBL,  11 G-PUSH ;

: EMIT-PRIMS
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
   s" pipe" ['] BPIPE FPRIM-L   s" dup2" ['] BDUP2 FPRIM-L
   s" fcntl" ['] BFCNTL FPRIM-L   s" poll" ['] BPOLL FPRIM-L
   s" spawn-io" ['] BSPAWNIO FPRIM-L   s" wait-rc" ['] BWAITRC FPRIM-L
   s" cp@" ['] BCPFETCH FPRIM-L   s" dbase@" ['] BDBASEFETCH FPRIM-L
   s" ndict@" ['] BNDICTFETCH FPRIM-L
   s" cp!" ['] BCPSET FPRIM-L   s" ndict!" ['] BNDSET FPRIM-L
   s" epoch-seconds" ['] BEPOCHSECONDS FPRIM-L
   s" mono-ns" ['] BMONONS FPRIM-L
   s" evaluate" ['] B-EVAL FPRIM-L
   s" die"  ['] BDIE   FPRIM-L
   s" open" ['] BOPEN FPRIM-L   s" write" ['] BWRITE FPRIM-L   s" read" ['] BREAD FPRIM-L   s" ioctl" ['] BIOCTL FPRIM-L
   s" open-rd" ['] BOPENRD FPRIM-L
   s" access" ['] BACCESS FPRIM-L   s" stat64" ['] BSTAT64 FPRIM-L
   s" getdirentries64" ['] BGETDIRENTRIES64 FPRIM-L
   s" patch32" ['] BPATCH32 FPRIM
   s" close" ['] BCLOSE FPRIM-L
   s" rbase" ['] BRBASE FPRIM-L
   s" catch" ['] BCATCH FPRIM   s" throw" ['] BTHROW FPRIM-L
   s" wordlist" ['] BWORDLIST FPRIM-L   s" get-current" ['] BGETCUR FPRIM-L
   s" set-current" ['] BSETCUR FPRIM-L  s" search-wl" ['] BSWL FPRIM-L
   s" set-check" ['] BSETCHECK FPRIM-L ;
s" emit-prims" s" --" TRUST

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
   LBL LBL LBL {: fl il sd :}
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

: EMIT-FP-PRIMS
   s" f+" ['] BF+ FPRIM-L   s" f-" ['] BF- FPRIM-L   s" f*" ['] BF* FPRIM-L
   s" f/" ['] BF/ FPRIM-L   s" fnegate" ['] BFNEG FPRIM-L
   s" fabs" ['] BFABS FPRIM-L  s" fsqrt" ['] BFSQRT FPRIM-L
   s" f<" ['] BF< FPRIM-L   s" f>" ['] BF> FPRIM-L   s" f=" ['] BF= FPRIM-L
   s" f0<" ['] BF0< FPRIM-L  s" f0=" ['] BF0= FPRIM-L
   s" s>f" ['] BS>F FPRIM-L  s" f>s" ['] BF>S FPRIM-L
   s" f." ['] BFDOT FPRIM-L ;
s" emit-fp-prims" s" --" TRUST

: EMIT-CEMIT
   LCEMIT @ LBL,  9 28 0 STRW,  28 28 4 ADDI,  RET, ;

\ LBCAP ( -- ) : append TKA/TKL + ' ' to the body capture. LBCS ( x11=a x12=u )
\ is the general entry (defining-word kind tokens). FATAL (exit 71) on overflow —
\ truncation would let the check hook certify code it never saw.
: EMIT-BCAP
   LBCAP @ LBL,
   11 DATA TKA-CELL LDR,  12 DATA TKL-CELL LDR,
   LBCS @ LBL,
   LBL LBL LBL {: bok bcp bcd :}
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

: EMIT-TOK
   LTOK @ LBL,
   LBL LBL LBL LBL LBL {: tskip thas tscan tgot tnone :}
   11 DATA INP-CELL LDR,  12 DATA INE-CELL LDR,
   tskip LBL,
      11 12 CMP,  C-GE tnone BCOND,
      9 11 0 LDRB,  9 32 CMPI,  C-HI thas BCOND,
      11 11 1 ADDI,  tskip B,
   thas LBL,  11 DATA TKA-CELL STR,
   tscan LBL,
      11 12 CMP,  C-GE tgot BCOND,
      9 11 0 LDRB,  9 32 CMPI,  C-LS tgot BCOND,
      11 11 1 ADDI,  tscan B,
   tgot LBL,  9 DATA TKA-CELL LDR,  9 11 9 SUB,  9 DATA TKL-CELL STR,
      11 DATA INP-CELL STR,  0 1 MOVZ,  RET,
   tnone LBL,  11 DATA INP-CELL STR,  0 0 MOVZ,  RET, ;

: EMIT-PROT
   LPROT @ LBL,
   0 DBASE 0 ADDI,  1 REGION LIT64,  NR-MPROTECT SYS,  RET, ;

: EMIT-FLUSH
   LFLUSH @ LBL,
   LBL LBL LBL LBL {: fdl fdd fil fid :}
   9 9 6 LSRI,  9 9 6 LSLI,                                 \ align start down to the
   10 9 0 ADDI,                                             \ line, or the 64-byte
                                                            \ stride skips the last one
   fdl LBL,  10 CP CMP,  C-GE fdd BCOND,  10 DCCVAU,  10 10 64 ADDI,  fdl B,
   fdd LBL,  DSB-ISH,
   10 9 0 ADDI,
   fil LBL,  10 CP CMP,  C-GE fid BCOND,  10 ICIVAU,  10 10 64 ADDI,  fil B,
   fid LBL,  DSB-ISH,  ISB,  RET, ;

: EMIT-FIND
   LFIND @ LBL,
   LBL LBL LBL LBL LBL {: floop fdone fnext fcmp fmatch :}
   5 DBASE 0 ADDI,  6 NDICT 0 ADDI,  13 0 MOVZ,
   floop LBL,
      6 fdone CBZ,
      14 5 16 LDR,  14 14 $FF ANDI,  14 10 CMP,  C-NE fnext BCOND,
      7 0 MOVZ,
      fcmp LBL,
         7 10 CMP,  C-GE fmatch BCOND,
         15 5 24 ADDI,  15 15 7 ADD,  15 15 0 LDRB,
         3 15 $41 SUBI,  3 26 CMPI,  3 C-CC CSET,  3 3 5 LSLI,  15 15 3 ORR,
         4 9 7 ADD,     4 4 0 LDRB,
         3 4 $41 SUBI,   3 26 CMPI,  3 C-CC CSET,  3 3 5 LSLI,  4 4 3 ORR,
         15 4 CMP,  C-NE fnext BCOND,
         7 7 1 ADDI,  fcmp B,
      fmatch LBL,
         11 5 0 LDR,  12 5 8 LDR,
         14 5 16 LDR,  14 14 $100 ANDI,  14 14 7 LSRI,   \ immediate bit -> 2
         13 1 MOVZ,  13 13 14 ORR,  fnext B,
      fnext LBL,  5 5 DREC ADDI,  6 6 1 SUBI,  floop B,
   fdone LBL,  RET, ;

: EMIT-NUM
   LNUM @ LBL,
   LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL
   {: ldone ndoll nohex lloop lok gotd nd nuc ndot isfrac lint fpos :}
   11 0 MOVZ,  13 1 MOVZ,  14 0 MOVZ,  12 0 MOVZ,  6 10 MOVZ,
   10 ldone CBZ,
   15 9 0 LDRB,  15 45 CMPI,  C-NE ndoll BCOND,
      13 0 MOVN,  14 1 MOVZ,
   ndoll LBL,
   14 10 CMP,  C-GE ldone BCOND,
   5 9 14 ADD,  15 5 0 LDRB,  15 36 CMPI,  C-NE nohex BCOND,
      6 16 MOVZ,  14 14 1 ADDI,
   nohex LBL,
   2 0 MOVZ,                                                    \ frac mode off
   14 10 CMP,  C-GE ldone BCOND,
   lloop LBL,
   14 10 CMP,  C-GE lok BCOND,
   5 9 14 ADD,  15 5 0 LDRB,
   15 46 CMPI,  C-NE ndot BCOND,                                \ '.' -> frac mode
      6 10 CMPI,  C-NE ldone BCOND,                             \ only base 10
      2 ldone CBNZ,                                             \ second dot -> fail
      2 1 MOVZ,  4 0 MOVZ,  3 1 MOVZ,                           \ frac=0 scale=1
      14 14 1 ADDI,  lloop B,
   ndot LBL,
   15 48 CMPI,  C-LT ldone BCOND,
   15 57 CMPI,  C-GT nd BCOND,
      7 15 48 SUBI,  gotd B,
   nd LBL,
   6 16 CMPI,  C-NE ldone BCOND,
   15 97 CMPI,  C-LT nuc BCOND,  15 102 CMPI,  C-GT ldone BCOND,
      7 15 87 SUBI,  gotd B,
   nuc LBL,
   15 65 CMPI,  C-LT ldone BCOND,  15 70 CMPI,  C-GT ldone BCOND,
      7 15 55 SUBI,
   gotd LBL,
   2 isfrac CBNZ,
   11 11 6 MUL,  11 11 7 ADD,
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

: EMIT-DICT
   LNCOUNT @ LBL,  #PL @ DCQ,
   LDICT @ LBL,
   0 BEGIN dup #PL @ < WHILE
      dup cells PLBL + @ DLBL,
      dup cells PEL  + @ DLBL,
      dup cells PLEN + @ DCQ,
      dup cells PNAM + @  over cells PLEN + @  BYTES,
      16  over cells PLEN + @  3 + -4 and  -  dup 0 > IF PNPOOL swap BYTES, ELSE drop THEN
      0 DCQ,
      1 + REPEAT drop ;

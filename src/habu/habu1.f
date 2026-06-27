\ habu1.f — the ENGINE BUILDER part 1: emits the standalone native Forth's
\ primitives, helper routines, and seed dictionary. Needs asm.fs +
\ icode.fs + mnem.fs + rt.fs (g-push/g-pop/g-print9) + crash.fs + macho.fs.
\ Part 1: prims + tok/find/num/prot/flush/cemit + dict. The interpreter main
\ loop, keyword JIT and EMIT-FORTH follow in part 2 (habu2.f).
variable STDIN?   0 0= 0= STDIN? !
s" STDIN?" s" -- ptr bool" TRUST
\ runtime instruction-word constants the JIT compiler stamps out
$D65F03C0 constant W-RET
$F9000269 constant W-PUSH0
$91002273 constant W-PUSH1
$D2800009 constant W-MOVZ0
$F2A00009 constant W-MOVK1
$F2C00009 constant W-MOVK2
$F2E00009 constant W-MOVK3
\ --- primitive registry (build-side, for the seed dictionary) ---
160 constant PRIM-CAP
2048 constant PRIM-NAME-CAP
create PLBL PRIM-CAP cells allot   create PEL PRIM-CAP cells allot
create PLEN PRIM-CAP cells allot   create PNAM PRIM-CAP cells allot
create PNLBL PRIM-CAP cells allot
create PNPOOL PRIM-NAME-CAP allot   variable PNP   variable #PL
variable RPD
variable PR-A  variable PR-U  variable PR-L  variable PR-E
variable FP-A  variable FP-U  variable FP-XT
: RPD@ ( -- ptr u8 ) RPD 0 ptr-field @ ;
: PR-A@ ( -- ptr u8 ) PR-A 0 ptr-field @ ;
: FP-A@ ( -- ptr u8 ) FP-A 0 ptr-field @ ;

: PR-SPACE ( -- )
   #PL @ PRIM-CAP >= IF s" primitive registry full" 76 die THEN
   PNP @ PR-U @ + PRIM-NAME-CAP > IF s" primitive name pool full" 76 die THEN ;

: PR-ARGS ( ptr u8 n n n -- )
   PR-E !  PR-L !  PR-U !  PR-A ! ;

: PR-COPY-NAME ( -- )
   0 BEGIN dup PR-U @ < WHILE  dup PR-A@ + c@  over RPD@ + c!  1 + REPEAT drop ;

: REG-PRIM ( ptr u8 n n n -- )
   PR-ARGS
   PR-SPACE
   PR-L @ #PL @ cells PLBL + !
   PR-E @ #PL @ cells PEL  + !
   PR-U @ #PL @ cells PLEN + !
   PNPOOL PNP @ + RPD !  RPD@ #PL @ cells PNAM + !
   PR-COPY-NAME
   PNP @ PR-U @ + PNP !  #PL @ 1 + #PL ! ;
variable FPL  variable FPE

: FP-ARGS ( ptr u8 n n -- )
   FP-XT !  FP-U !  FP-A ! ;

: FP-KEEP? ( -- bool )
   FP-A@ FP-U @ KEEP? ;

: FP-REG ( -- )
   FP-A@ FP-U @ FPL @ FPE @ REG-PRIM ;

: FPRIM ( ptr u8 n n -- )
   FP-ARGS
   FP-KEEP? 0= IF EXIT THEN
   LBL FPL !  LBL FPE !
   FP-REG
   FPL LABEL@ LBL,  SP SP 16 SUBI,  30 SP 0 STR,
   FP-XT @ execute  30 SP 0 LDR,  SP SP 16 ADDI,  RET,  FPE LABEL@ LBL, ;
s" fprim" s" ptr u8 n n --" TRUST

: FPRIM-L ( ptr u8 n n -- )           \ LEAF prim: no BL/BLR in body -> no x30 frame
   FP-ARGS
   FP-KEEP? 0= IF EXIT THEN
   LBL FPL !  LBL FPE !
   FP-REG
   FPL LABEL@ LBL,  FP-XT @ execute  RET,  FPE LABEL@ LBL, ;
s" fprim-l" s" ptr u8 n n --" TRUST
\ shared label ids (forward refs)
variable LANCHOR  variable LFIND  variable LNUM  variable LDICT  variable LSRC  variable SRCN
variable LCEMIT   variable LTOK   variable LPROT  variable LFLUSH variable LNCOUNT
variable LCFPUSH  variable LCFPOP  variable LPAT   variable LKWCMP  variable LBCAP  variable LBCS
variable LBCHAIN  variable LCREATE  variable LDOESPATCH
variable LKWIF    variable LKWTHEN variable LKWELSE variable LKWBEGIN
variable LKWUNTIL variable LKWAGAIN variable LKWWHILE variable LKWREPEAT
variable LKWCREATE variable LKWVAR variable LKWSQ variable LKWCQ variable LKWDOTQ
variable LKWTICK variable LKWBTICK
variable LKWTYPE
variable LKWLBRACE variable LKWENDLOC variable LLOC-FIND variable LKWCONST
variable LKWDO variable LKWLOOP variable LKWI
variable LKWTOR variable LKWRFROM variable LKWRFET
variable LKWEXIT variable LKWREC
variable LKWQDO variable LKWPLOOP variable LKWJ variable LKWLEAVE variable LKWUNLOOP
variable LKWCHAR variable LKWBCHAR
variable LKWIMM variable LKWPOST variable LKWCOMPC
variable LKWDOES variable LKWQUOT variable LKWSEMIQ variable LKWPACKAGE variable LKWPUBLIC
variable LKWTRUSTED variable LKWTRUST variable LKWCHKDOES variable LKWKERNEL variable LKWPRIVATE variable LKWENDPACKAGE variable LKWDUPDEF variable LCHKPACKAGE variable LCHKPUB variable LCHKPRI variable LCHKENDPKG
9 constant A   10 constant B   11 constant C
12 constant DREG  13 constant EREG

\ ---- primitive bodies (operate on the x19 data stack) ----
: B+ ( -- )
   B G-POP  A G-POP  A A B ADD,  A G-PUSH ;

: B- ( -- )
   B G-POP  A G-POP  A A B SUB,  A G-PUSH ;

: B* ( -- )
   B G-POP  A G-POP  A A B MUL,  A G-PUSH ;

: BDUP ( -- )
   A G-POP  A G-PUSH  A G-PUSH ;

: BDROP ( -- )
   XDS XDS 8 SUBI, ;

: BSWAP ( -- )
   A G-POP  B G-POP  A G-PUSH  B G-PUSH ;

: BDOT ( -- )
   A G-POP  G-PRINT9 ;

: BU. ( -- )
   A G-POP  G-PRINTU9 ;

56 constant LINUX-SPAWN-PIPE-R-OFF
60 constant LINUX-SPAWN-PIPE-W-OFF
64 constant LINUX-SPAWN-ERR-OFF
72 constant LINUX-SPAWN-PID-OFF
80 constant LINUX-SPAWN-STATUS-OFF
96 constant LINUX-SPAWN-FRAME
3 constant LINUX-SPAWN-MIN-ERRFD
1024 constant LINUX-F-LINUX-SPECIFIC-BASE
6 constant LINUX-F-DUPFD-CLOEXEC-OFF
2 constant LINUX-F-SETFD
1 constant LINUX-FD-CLOEXEC
$80000 constant LINUX-O-CLOEXEC
LINUX-F-LINUX-SPECIFIC-BASE LINUX-F-DUPFD-CLOEXEC-OFF + constant LINUX-F-DUPFD-CLOEXEC

: LINUX-SPAWN-FAIL ( reg -- )
   {: errfd :}
   0 errfd REG>N 0 ADDI,
   15 1 MOVZ,  15 SP LINUX-SPAWN-ERR-OFF STRB,
   1 SP LINUX-SPAWN-ERR-OFF ADDI,  2 1 MOVZ,  NR-WRITE SYS,
   0 127 MOVZ,  NR-EXIT-GROUP SYS, ;
s" linux-spawn-fail" s" reg --" TRUST

: LINUX-DUP2-FD ( reg fd reg -- )
   {: fdreg newfd errfd :}
   LBL LBL {: skip ok :}
   fdreg REG>N 0 CMPI,  C-LT skip BCOND,
   fdreg REG>N newfd FD>N CMPI,  C-EQ skip BCOND,
   0 fdreg REG>N 0 ADDI,  1 newfd FD>N MOVZ,  2 0 MOVZ,  NR-DUP2 SYS,
   9 C-CS CSET,  9 ok CBZ,
      errfd LINUX-SPAWN-FAIL
   ok LBL,
   skip LBL, ;
s" linux-dup2-fd" s" reg fd reg --" TRUST

: LINUX-CHDIR-FD ( reg reg -- )
   {: cwdreg errfd :}
   LBL LBL {: skip ok :}
   cwdreg REG>N 0 CMPI,  C-LT skip BCOND,
   0 cwdreg REG>N 0 ADDI,  NR-CHDIR SYS,
   9 C-CS CSET,  9 ok CBZ,
      errfd LINUX-SPAWN-FAIL
   ok LBL,
   skip LBL, ;
s" linux-chdir-fd" s" reg reg --" TRUST

: LINUX-SPAWN-CLOSE-R ( -- )
   0 SP LINUX-SPAWN-PIPE-R-OFF LDRW,  NR-CLOSE SYS, ;
s" linux-spawn-close-r" s" --" TRUST

: LINUX-SPAWN-CLOSE-W ( -- )
   0 SP LINUX-SPAWN-PIPE-W-OFF LDRW,  NR-CLOSE SYS, ;
s" linux-spawn-close-w" s" --" TRUST

: LINUX-SPAWN-CLOSE-PIPE ( -- )
   LINUX-SPAWN-CLOSE-R
   LINUX-SPAWN-CLOSE-W ;
s" linux-spawn-close-pipe" s" --" TRUST

: LINUX-SPAWN-PREP-W ( -- )
   LBL LBL LBL {: high fail done :}
   9 0 MOVZ,
   0 SP LINUX-SPAWN-PIPE-W-OFF LDRW,
   0 LINUX-SPAWN-MIN-ERRFD 1- CMPI,  C-GT done BCOND,
      1 LINUX-F-DUPFD-CLOEXEC MOVZ,  2 LINUX-SPAWN-MIN-ERRFD MOVZ,
      NR-FCNTL SYS,
      9 C-CS CSET,  9 fail CBNZ,
      14 0 0 ADDI,
      0 SP LINUX-SPAWN-PIPE-W-OFF LDRW,  NR-CLOSE SYS,
      14 SP LINUX-SPAWN-PIPE-W-OFF STRW,
      9 0 MOVZ,  done B,
   fail LBL,
      9 1 MOVZ,
   done LBL, ;
s" linux-spawn-prep-w" s" --" TRUST

: LINUX-SPAWN-WAIT-STORED ( -- )
   0 SP LINUX-SPAWN-PID-OFF LDR,
   1 SP LINUX-SPAWN-STATUS-OFF ADDI,  2 0 MOVZ,  3 0 MOVZ,
   NR-WAIT4 SYS, ;
s" linux-spawn-wait-stored" s" --" TRUST

: LINUX-SPAWN-PARENT ( -- )
   LBL LBL LBL {: ok fail done :}
   0 SP LINUX-SPAWN-PID-OFF STR,
   LINUX-SPAWN-CLOSE-W
   0 SP LINUX-SPAWN-PIPE-R-OFF LDRW,
   1 SP LINUX-SPAWN-ERR-OFF ADDI,  2 1 MOVZ,  NR-READ SYS,
   9 C-CS CSET,  9 fail CBNZ,
   0 0 CMPI,  C-EQ ok BCOND,
   fail LBL,
      LINUX-SPAWN-CLOSE-R
      LINUX-SPAWN-WAIT-STORED
      9 0 MOVN,  done B,
   ok LBL,
      LINUX-SPAWN-CLOSE-R
      9 SP LINUX-SPAWN-PID-OFF LDR,
   done LBL, ;
s" linux-spawn-parent" s" --" TRUST

: LINUX-SPAWN-CHILD ( -- )
   LINUX-SPAWN-CLOSE-R
   14 SP LINUX-SPAWN-PIPE-W-OFF LDRW,
   9 SP 24 LDR,  9 >REG 14 >REG LINUX-CHDIR-FD
   14 SP LINUX-SPAWN-PIPE-W-OFF LDRW,
   9 SP 32 LDR,  9 >REG 0 >FD 14 >REG LINUX-DUP2-FD
   14 SP LINUX-SPAWN-PIPE-W-OFF LDRW,
   9 SP 40 LDR,  9 >REG 1 >FD 14 >REG LINUX-DUP2-FD
   14 SP LINUX-SPAWN-PIPE-W-OFF LDRW,
   9 SP 48 LDR,  9 >REG 2 >FD 14 >REG LINUX-DUP2-FD
   0 SP 0 LDR,  1 SP 8 LDR,  2 SP 16 LDR,
   NR-EXECVE SYS,
   14 SP LINUX-SPAWN-PIPE-W-OFF LDRW,
   14 >REG LINUX-SPAWN-FAIL ;
s" linux-spawn-child" s" --" TRUST

: LINUX-SPAWN ( reg reg reg reg reg reg reg -- )
   {: pathreg argvreg envreg cwdreg infd outfd errfd :}
   LBL LBL LBL LBL {: child closefail fail done :}
   SP SP LINUX-SPAWN-FRAME SUBI,
   pathreg REG>N SP 0 STR,  argvreg REG>N SP 8 STR,  envreg REG>N SP 16 STR,  cwdreg REG>N SP 24 STR,
   infd REG>N SP 32 STR,  outfd REG>N SP 40 STR,  errfd REG>N SP 48 STR,
   0 SP LINUX-SPAWN-PIPE-R-OFF ADDI,  1 LINUX-O-CLOEXEC LIT64,  NR-PIPE SYS,
   9 C-CS CSET,  9 fail CBNZ,
   LINUX-SPAWN-PREP-W
   9 closefail CBNZ,
   0 17 MOVZ,  1 0 MOVZ,  2 0 MOVZ,  3 0 MOVZ,  4 0 MOVZ,
   NR-SPAWN SYS,
   9 C-CS CSET,  9 closefail CBNZ,
   0 child CBZ,
      LINUX-SPAWN-PARENT
      done B,
   child LBL,
      LINUX-SPAWN-CHILD
   closefail LBL,
      LINUX-SPAWN-CLOSE-PIPE
   fail LBL,
      9 0 MOVN,
   done LBL,
   SP SP LINUX-SPAWN-FRAME ADDI,
   9 G-PUSH ;
s" linux-spawn" s" reg reg reg reg reg reg reg --" TRUST

: BRUNRC ( -- )                    \ ( pathz -- rc ) spawn+wait; -1 = spawn failed
   A G-POP
   LBL LBL LBL {: spok spdn spw :}
   HB-TARGET-LINUX? IF
      SP SP 64 SUBI,
      9 SP 16 STR,
      10 0 MOVZ,  10 SP 24 STR,
      10 SP 48 STR,
      10 SP 16 ADDI,
      11 SP 48 ADDI,
      13 0 MOVN,
      9 >REG 10 >REG 11 >REG 13 >REG 13 >REG 13 >REG 13 >REG LINUX-SPAWN
      9 G-POP
      9 0 CMPI,  C-LT spdn BCOND,
      0 9 0 ADDI,
      1 SP 8 ADDI,  2 0 MOVZ,  3 0 MOVZ,
      NR-WAIT4 SYS,
      10 C-CS CSET,  10 spw CBZ,
         9 0 MOVN,  spdn B,
      spw LBL,
      9 SP 8 LDRW,
      9 9 8 LSRI,  9 9 $FF ANDI,
      spdn LBL,
      9 G-PUSH
      SP SP 64 ADDI,
      exit
   THEN
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

: BPIPE ( -- )                     \ ( -- rfd wfd rc ) rc=0, or -1 -1 -1
   LBL LBL {: pok pdn :}
   HB-TARGET-LINUX? IF
      SP SP 16 SUBI,
      0 SP 0 ADDI,  1 0 MOVZ,  NR-PIPE SYS,
      9 C-CS CSET,  9 pok CBZ,
         9 0 MOVN,  9 G-PUSH  9 G-PUSH  9 G-PUSH  pdn B,
      pok LBL,
      0 SP 0 LDRW,  1 SP 4 LDRW,
      0 G-PUSH  1 G-PUSH  9 0 MOVZ,  9 G-PUSH
      pdn LBL,
      SP SP 16 ADDI,
      exit
   THEN
   NR-PIPE SYS,
   9 C-CS CSET,  9 pok CBZ,
      9 0 MOVN,  9 G-PUSH  9 G-PUSH  9 G-PUSH  pdn B,
   pok LBL,
   0 G-PUSH  1 G-PUSH  9 0 MOVZ,  9 G-PUSH
   pdn LBL, ;

: BDUP2 ( -- )                     \ ( oldfd newfd -- rc ) rc=newfd or -1
   1 G-POP  0 G-POP
   LBL LBL {: dok ddn :}
   HB-TARGET-LINUX? IF 2 0 MOVZ, THEN
   NR-DUP2 SYS,
   9 C-CS CSET,  9 dok CBZ,
      0 0 MOVN,  ddn B,
   dok LBL,
   ddn LBL,
   0 G-PUSH ;

13 constant LINUX-SIGPIPE
1 constant LINUX-SIG-IGN
8 constant LINUX-SIGSET-SIZE

: LINUX-IGNORE-SIGPIPE ( -- )
   LBL LBL {: ok done :}
   SP SP 64 SUBI,
   9 LINUX-SIG-IGN MOVZ,  9 SP 0 STR,
   9 0 MOVZ,  9 SP 8 STR,  9 SP 16 STR,  9 SP 24 STR,
   0 LINUX-SIGPIPE MOVZ,  1 SP 0 ADDI,  2 0 MOVZ,  3 LINUX-SIGSET-SIZE MOVZ,
   NR-SIGACTION SYS,
   9 C-CS CSET,  9 ok CBZ,
      0 0 MOVN,  done B,
   ok LBL,
      0 0 MOVZ,
   done LBL,
   SP SP 64 ADDI, ;
s" linux-ignore-sigpipe" s" --" TRUST

: BFCNTL ( -- )                    \ ( fd cmd arg -- rc ) rc=sysret or -1
   2 G-POP  1 G-POP  0 G-POP
   LBL LBL LBL {: fok fdn freal :}
   HB-TARGET-LINUX? IF
      1 73 CMPI,  C-NE freal BCOND,
         LINUX-IGNORE-SIGPIPE
         fdn B,
      freal LBL,
   THEN
   NR-FCNTL SYS,
   9 C-CS CSET,  9 fok CBZ,
      0 0 MOVN,  fdn B,
   fok LBL,
   fdn LBL,
   0 G-PUSH ;

: BPOLL ( -- )                     \ ( fds nfds timeout -- rc ) rc=nready/0 or -1
   2 G-POP  1 G-POP  0 G-POP
   LBL LBL LBL LBL {: pok pdn pneg pcall :}
   HB-TARGET-LINUX? IF
      SP SP 32 SUBI,
      2 0 CMPI,  C-LT pneg BCOND,
         5 1000 MOVZ,  6 2 5 UDIV,
         7 6 5 MUL,  7 2 7 SUB,
         5 1000 MOVZ,  7 7 5 MUL,  5 1000 MOVZ,  7 7 5 MUL,
         6 SP 0 STR,  7 SP 8 STR,
         2 SP 0 ADDI,  pcall B,
      pneg LBL,
         2 0 MOVZ,
      pcall LBL,
      3 0 MOVZ,  4 0 MOVZ,
      NR-POLL SYS,
      9 C-CS CSET,  9 pok CBZ,
         0 0 MOVN,  pdn B,
      pok LBL,
      pdn LBL,
      0 G-PUSH
      SP SP 32 ADDI,
      exit
   THEN
   NR-POLL SYS,
   9 C-CS CSET,  9 pok CBZ,
      0 0 MOVN,  pdn B,
   pok LBL,
   pdn LBL,
   0 G-PUSH ;

: BKILL ( -- )                     \ ( pid sig -- rc ) rc=0 or -1
   1 G-POP  0 G-POP
   LBL LBL {: kok kdn :}
   NR-KILL SYS,
   9 C-CS CSET,  9 kok CBZ,
      0 0 MOVN,  kdn B,
   kok LBL,
   kdn LBL,
   0 G-PUSH ;

: BWAITRC ( -- )                   \ ( pid -- rc ) wait4; -1 = wait failed
   A G-POP
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

: BWAITSTATUS ( -- )               \ ( pid -- status ) wait4 raw status; -1 = wait failed
   A G-POP
   LBL LBL {: wok wdn :}
   SP SP 16 SUBI,
   0 9 0 ADDI,
   1 SP 0 ADDI,  2 0 MOVZ,  3 0 MOVZ,
   NR-WAIT4 SYS,
   9 C-CS CSET,  9 wok CBZ,
      9 0 MOVN,  wdn B,
   wok LBL,
   9 SP 0 LDRW,
   wdn LBL,
   9 G-PUSH
   SP SP 16 ADDI, ;

1040 constant SPAWN-ACTION-SIZE
3584 constant SPAWN-FRAME3
2048 constant SPAWN-FRAME4-A
2048 constant SPAWN-FRAME4-B
256 constant SPAWN-FRAME4-C
0 constant SPAWN-PID-OFF
16 constant SPAWN-ARGV-OFF
24 constant SPAWN-ARGV-END-OFF
32 constant SPAWN-ENVP-OFF
176 constant SPAWN-ACTIONS-OFF
0 constant SPAWN-FA-CAP-OFF
4 constant SPAWN-FA-COUNT-OFF
8 constant SPAWN-FA-ACTS-OFF
2 constant PSFA-DUP2
5 constant PSFA-CHDIR
8 constant SPAWN-CHDIR-PATH-OFF
SPAWN-ACTION-SIZE SPAWN-CHDIR-PATH-OFF - constant SPAWN-CHDIR-PATH-CAP
48 constant SPAWN-ADESC-OFF
64 constant SPAWN-ADESC-FA-SIZE-OFF
72 constant SPAWN-ADESC-FA-PTR-OFF
128 constant SPAWN-ADESC-SIZE

variable SDA-FD  variable SDA-NEW  variable SDA-SKIP
variable SCA-CWD  variable SCA-FAIL
variable SCA-COPY  variable SCA-OVER  variable SCA-DONE
variable SACT-CAP  variable SAD-HAS
variable SPD-PATH  variable SAE-ARGV  variable SAE-ENVP
variable SDEF-PATH  variable SADV-ARGV
variable SFIN-OK  variable SFIN-FAIL
variable BSP-OK  variable BSP-DN  variable BSP-SAD
variable SZA-I

\ Emit one PSFA_DUP2 record into the runtime file-actions blob at x13.
: SPAWN-DUP2-ARGS ( reg fd -- )
   SDA-NEW !  SDA-FD ! ;

: SPAWN-DUP2-ACTION ( reg fd -- )
   SPAWN-DUP2-ARGS
   LBL SDA-SKIP !
   SDA-FD @ 0 CMPI,  C-LT SDA-SKIP LABEL@ BCOND,
   14 13 SPAWN-FA-COUNT-OFF LDRW,  15 SPAWN-ACTION-SIZE MOVZ,  14 14 15 MUL,
   14 14 SPAWN-FA-ACTS-OFF ADDI,  14 14 13 ADD,
   15 PSFA-DUP2 MOVZ,  15 14 0 STRW,
   SDA-FD @ 14 4 STRW,
   15 SDA-NEW @ MOVZ,  15 14 8 STRW,
   14 13 SPAWN-FA-COUNT-OFF LDRW,  14 14 1 ADDI,  14 13 SPAWN-FA-COUNT-OFF STRW,
   SDA-SKIP LABEL@ LBL, ;
s" spawn-dup2-action" s" reg fd --" TRUST

\ Emit one PSFA_CHDIR record into the runtime file-actions blob at x13.
: SPAWN-CHDIR-ARGS ( reg label -- )
   SCA-FAIL !  SCA-CWD ! ;

: SPAWN-CHDIR-LABELS ( -- )
   LBL SCA-COPY !  LBL SCA-OVER !  LBL SCA-DONE ! ;

: SPAWN-CHDIR-ACTION ( reg label -- )
   SPAWN-CHDIR-ARGS
   SPAWN-CHDIR-LABELS
   14 13 SPAWN-FA-COUNT-OFF LDRW,  15 SPAWN-ACTION-SIZE MOVZ,  14 14 15 MUL,
   14 14 SPAWN-FA-ACTS-OFF ADDI,  14 14 13 ADD,
   15 PSFA-CHDIR MOVZ,  15 14 0 STRW,
   16 SCA-CWD @ 0 ADDI,
   17 14 SPAWN-CHDIR-PATH-OFF ADDI,
   18 SPAWN-CHDIR-PATH-CAP MOVZ,
   SCA-COPY LABEL@ LBL,
      18 0 CMPI,  C-EQ SCA-OVER LABEL@ BCOND,
      15 16 0 LDRB,
      15 17 0 STRB,
      16 16 1 ADDI,
      17 17 1 ADDI,
      18 18 1 SUBI,
      15 SCA-COPY LABEL@ CBNZ,
   14 13 SPAWN-FA-COUNT-OFF LDRW,  14 14 1 ADDI,  14 13 SPAWN-FA-COUNT-OFF STRW,
   SCA-DONE LABEL@ B,
   SCA-OVER LABEL@ LBL,
   9 0 MOVN,  SCA-FAIL LABEL@ B,
   SCA-DONE LABEL@ LBL, ;
s" spawn-chdir-action" s" reg label --" TRUST

: SPAWN-DARWIN-FRAME3-ENTER ( -- )
   SP SP SPAWN-FRAME3 SUBI, ;
s" spawn-darwin-frame3-enter" s" --" TRUST

: SPAWN-DARWIN-FRAME3-LEAVE ( -- )
   SP SP SPAWN-FRAME3 ADDI, ;
s" spawn-darwin-frame3-leave" s" --" TRUST

: SPAWN-DARWIN-FRAME4-ENTER ( -- )
   SP SP SPAWN-FRAME4-A SUBI,
   SP SP SPAWN-FRAME4-B SUBI,
   SP SP SPAWN-FRAME4-C SUBI, ;
s" spawn-darwin-frame4-enter" s" --" TRUST

: SPAWN-DARWIN-FRAME4-LEAVE ( -- )
   SP SP SPAWN-FRAME4-C ADDI,
   SP SP SPAWN-FRAME4-B ADDI,
   SP SP SPAWN-FRAME4-A ADDI, ;
s" spawn-darwin-frame4-leave" s" --" TRUST

: SPAWN-DARWIN-ACTIONS-RESET ( count -- )
   SACT-CAP !
   13 SP SPAWN-ACTIONS-OFF ADDI,
   14 SACT-CAP @ MOVZ,  14 13 SPAWN-FA-CAP-OFF STRW,
   14 0 MOVZ,  14 13 SPAWN-FA-COUNT-OFF STRW, ;
s" spawn-darwin-actions-reset" s" count --" TRUST

: SPAWN-DARWIN-STDIO-ACTIONS ( -- )
   10 >REG 0 >FD SPAWN-DUP2-ACTION
   11 >REG 1 >FD SPAWN-DUP2-ACTION
   12 >REG 2 >FD SPAWN-DUP2-ACTION ;
s" spawn-darwin-stdio-actions" s" --" TRUST

: SPAWN-DARWIN-ZERO-ADESC ( -- )
   14 0 MOVZ,
   0 SZA-I !
   BEGIN SZA-I @ SPAWN-ADESC-SIZE < WHILE
      14 SP SPAWN-ADESC-OFF SZA-I @ + STR,
      SZA-I @ 8 + SZA-I !
   REPEAT ;
s" spawn-darwin-zero-adesc" s" --" TRUST

: SPAWN-DARWIN-FILL-ADESC ( -- )
   14 13 SPAWN-FA-COUNT-OFF LDRW,
   15 SPAWN-ACTION-SIZE MOVZ,
   14 14 15 MUL,
   14 14 SPAWN-FA-ACTS-OFF ADDI,
   14 SP SPAWN-ADESC-FA-SIZE-OFF STR,
   13 SP SPAWN-ADESC-FA-PTR-OFF STR, ;
s" spawn-darwin-fill-adesc" s" --" TRUST

: SPAWN-DARWIN-NULLABLE-ADESC ( label -- )
   SAD-HAS !
   14 13 SPAWN-FA-COUNT-OFF LDRW,
   2 SP SPAWN-ADESC-OFF ADDI,
   14 SAD-HAS LABEL@ CBNZ,
      2 0 MOVZ,
   SAD-HAS LABEL@ LBL, ;
s" spawn-darwin-nullable-adesc" s" label --" TRUST

: SPAWN-DARWIN-USE-ADESC ( -- )
   2 SP SPAWN-ADESC-OFF ADDI, ;
s" spawn-darwin-use-adesc" s" --" TRUST

: SPAWN-DARWIN-PID-PATH ( reg -- )
   SPD-PATH !
   0 SP SPAWN-PID-OFF ADDI,
   1 SPD-PATH @ 0 ADDI, ;
s" spawn-darwin-pid-path" s" reg --" TRUST

: SPAWN-DARWIN-ARGV-ENVP ( reg reg -- )
   SAE-ENVP !  SAE-ARGV !
   3 SAE-ARGV @ 0 ADDI,
   4 SAE-ENVP @ 0 ADDI, ;
s" spawn-darwin-argv-envp" s" reg reg --" TRUST

: SPAWN-DARWIN-DEFAULT-ARGV-ENVP ( reg -- )
   SDEF-PATH !
   SDEF-PATH @ SP SPAWN-ARGV-OFF STR,
   14 0 MOVZ,
   14 SP SPAWN-ARGV-END-OFF STR,
   14 SP SPAWN-ENVP-OFF STR, ;
s" spawn-darwin-default-argv-envp" s" reg --" TRUST

: SPAWN-DARWIN-DEFAULT-ENVP ( -- )
   14 0 MOVZ,
   14 SP SPAWN-ARGV-OFF STR, ;
s" spawn-darwin-default-envp" s" --" TRUST

: SPAWN-DARWIN-USE-DEFAULT-ARGV-ENVP ( -- )
   3 SP SPAWN-ARGV-OFF ADDI,
   4 SP SPAWN-ENVP-OFF ADDI, ;
s" spawn-darwin-use-default-argv-envp" s" --" TRUST

: SPAWN-DARWIN-ARGV-DEFAULT-ENVP ( reg -- )
   SADV-ARGV !
   3 SADV-ARGV @ 0 ADDI,
   4 SP SPAWN-ARGV-OFF ADDI, ;
s" spawn-darwin-argv-default-envp" s" reg --" TRUST

: SPAWN-DARWIN-FINISH ( label label -- )
   SFIN-FAIL !  SFIN-OK !
   NR-SPAWN SYS,
   9 C-CS CSET,  9 9 0 ORR,  9 SFIN-OK LABEL@ CBZ,
      9 0 MOVN,  SFIN-FAIL LABEL@ B,
   SFIN-OK LABEL@ LBL,
   9 SP SPAWN-PID-OFF LDR,
   SFIN-FAIL LABEL@ LBL,
   9 G-PUSH ;
s" spawn-darwin-finish" s" label label --" TRUST

: BSP-LABELS3 ( -- )
   LBL BSP-OK !  LBL BSP-DN !  LBL BSP-SAD ! ;

: BSP-LABELS2 ( -- )
   LBL BSP-OK !  LBL BSP-DN ! ;

: BSPAWNIO ( -- )                  \ ( pathz stdinfd stdoutfd stderrfd -- pid|-1 )
   BSP-LABELS3
   12 G-POP  11 G-POP  10 G-POP  9 G-POP
   HB-TARGET-LINUX? IF
      SP SP 64 SUBI,
      9 SP 16 STR,
      13 0 MOVZ,  13 SP 24 STR,
      13 SP 32 STR,
      13 0 MOVN,
      14 SP 16 ADDI,  15 SP 32 ADDI,
      9 >REG 14 >REG 15 >REG 13 >REG 10 >REG 11 >REG 12 >REG LINUX-SPAWN
      SP SP 64 ADDI,
      exit
   THEN
   SPAWN-DARWIN-FRAME3-ENTER
   9 >REG SPAWN-DARWIN-DEFAULT-ARGV-ENVP
   3 >COUNT SPAWN-DARWIN-ACTIONS-RESET
   SPAWN-DARWIN-STDIO-ACTIONS
   SPAWN-DARWIN-ZERO-ADESC
   SPAWN-DARWIN-FILL-ADESC
   9 >REG SPAWN-DARWIN-PID-PATH
   BSP-SAD @ >LABEL SPAWN-DARWIN-NULLABLE-ADESC
   SPAWN-DARWIN-USE-DEFAULT-ARGV-ENVP
   BSP-OK @ >LABEL BSP-DN @ >LABEL SPAWN-DARWIN-FINISH
   SPAWN-DARWIN-FRAME3-LEAVE ;

: BSPAWNARGVIO ( -- )              \ ( pathz argvp stdinfd stdoutfd stderrfd -- pid|-1 )
   BSP-LABELS3
   12 G-POP  11 G-POP  10 G-POP  9 G-POP  8 G-POP
   HB-TARGET-LINUX? IF
      SP SP 16 SUBI,
      13 0 MOVZ,  13 SP 0 STR,
      13 0 MOVN,
      15 SP 0 ADDI,
      8 >REG 9 >REG 15 >REG 13 >REG 10 >REG 11 >REG 12 >REG LINUX-SPAWN
      SP SP 16 ADDI,
      exit
   THEN
   SPAWN-DARWIN-FRAME3-ENTER
   SPAWN-DARWIN-DEFAULT-ENVP
   3 >COUNT SPAWN-DARWIN-ACTIONS-RESET
   SPAWN-DARWIN-STDIO-ACTIONS
   SPAWN-DARWIN-ZERO-ADESC
   SPAWN-DARWIN-FILL-ADESC
   8 >REG SPAWN-DARWIN-PID-PATH
   BSP-SAD @ >LABEL SPAWN-DARWIN-NULLABLE-ADESC
   9 >REG SPAWN-DARWIN-ARGV-DEFAULT-ENVP
   BSP-OK @ >LABEL BSP-DN @ >LABEL SPAWN-DARWIN-FINISH
   SPAWN-DARWIN-FRAME3-LEAVE ;

: BSPAWNARGVENVIO ( -- )           \ ( pathz argvp envp stdinfd stdoutfd stderrfd -- pid|-1 )
   BSP-LABELS3
   12 G-POP  11 G-POP  10 G-POP  7 G-POP  9 G-POP  8 G-POP
   HB-TARGET-LINUX? IF
      13 0 MOVN,
      8 >REG 9 >REG 7 >REG 13 >REG 10 >REG 11 >REG 12 >REG LINUX-SPAWN
      exit
   THEN
   SPAWN-DARWIN-FRAME3-ENTER
   3 >COUNT SPAWN-DARWIN-ACTIONS-RESET
   SPAWN-DARWIN-STDIO-ACTIONS
   SPAWN-DARWIN-ZERO-ADESC
   SPAWN-DARWIN-FILL-ADESC
   8 >REG SPAWN-DARWIN-PID-PATH
   BSP-SAD @ >LABEL SPAWN-DARWIN-NULLABLE-ADESC
   9 >REG 7 >REG SPAWN-DARWIN-ARGV-ENVP
   BSP-OK @ >LABEL BSP-DN @ >LABEL SPAWN-DARWIN-FINISH
   SPAWN-DARWIN-FRAME3-LEAVE ;

: BSPAWNARGVENVCWDIO ( -- )        \ ( pathz argvp envp cwdz stdinfd stdoutfd stderrfd -- pid|-1 )
   BSP-LABELS2
   12 G-POP  11 G-POP  10 G-POP  6 G-POP  7 G-POP  9 G-POP  8 G-POP
   HB-TARGET-LINUX? IF
      8 >REG 9 >REG 7 >REG 6 >REG 10 >REG 11 >REG 12 >REG LINUX-SPAWN
      exit
   THEN
   SPAWN-DARWIN-FRAME4-ENTER
   4 >COUNT SPAWN-DARWIN-ACTIONS-RESET
   6 >REG BSP-DN @ >LABEL SPAWN-CHDIR-ACTION
   SPAWN-DARWIN-STDIO-ACTIONS
   SPAWN-DARWIN-ZERO-ADESC
   SPAWN-DARWIN-FILL-ADESC
   8 >REG SPAWN-DARWIN-PID-PATH
   SPAWN-DARWIN-USE-ADESC
   9 >REG 7 >REG SPAWN-DARWIN-ARGV-ENVP
   BSP-OK @ >LABEL BSP-DN @ >LABEL SPAWN-DARWIN-FINISH
   SPAWN-DARWIN-FRAME4-LEAVE ;

: BCPFETCH ( -- ) 9 CP 0 ADDI,  A G-PUSH ;     \ ( -- addr ) live CP (snapshot writer)
: BNDICTFETCH ( -- ) 9 NDICT 0 ADDI,  A G-PUSH ;  \ ( -- n ) live dict count
: BDBASEFETCH ( -- ) 9 DBASE 0 ADDI,  A G-PUSH ;  \ ( -- addr ) region base
: BDATAFETCH ( -- ) 9 DATA 0 ADDI,  A G-PUSH ;   \ ( -- addr ) live DATA base
: BCPSET ( -- ) A G-POP  CP A 0 ADDI, ;         \ ( addr -- ) set CP — forget code back to a mark
: BNDSET ( -- ) A G-POP  NDICT A 0 ADDI, ;      \ ( n -- ) set NDICT — forget dict entries past a mark

: BEPOCHSECONDS ( -- )
   LBL {: ok :}
   0 DATA GTOD-SCRATCH ADDI,  1 0 MOVZ,  2 0 MOVZ,  NR-GETTIMEOFDAY SYS,
   9 C-CS CSET,  9 9 0 ORR,  9 0 CMPI,  C-EQ ok BCOND,  BRK,
   ok LBL,
   9 DATA GTOD-SCRATCH LDR,  9 G-PUSH ;

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
\ depth-indexed frame and return here. Sets EVALERR-CELL: 0 = clean, 1 = recovered from an error.
: C-EVAL-FRAME-ADDR ( n n n -- ) {: depth dst scratch :}
   dst EVAL-FRAME LIT64,
   scratch depth EVAL-FRAME-SHIFT LSLI,
   dst dst scratch ADD,
   dst DATA dst ADD, ;

: B-EVAL ( -- )
   LBL {: ok :}
   B G-POP  A G-POP                                  \ x10 = u, x9 = a
   11 DATA EVALD-CELL LDR,
   12 EVAL-MAX-DEPTH MOVZ,  11 12 CMP,  C-LT ok BCOND,
      BRK,
   ok LBL,
   11 14 15 C-EVAL-FRAME-ADDR                        \ x14 = &frame[EVALD]
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

: BCREATE ( -- )
   15 0 MOVZ,  16 20 CREATEP-CELL LDR,  16 BLR, ;   \ ( "name" -- ) runtime CREATE via the
                                     \ startup-stored cell: subsets emit prims w/o labels

: BCOMPILE ( -- )
   A G-POP  11 9 0 ADDI,
   SP SP 16 SUBI,  11 SP 8 STR,
   2 3 MOVZ,  LPROT LABEL@ BL,
   11 SP 8 LDR,
   5 $FFFF MOVZ,
   7 11 5 AND,    7 7 5 LSLI,  8 $D2800010 LIT64,  9 8 7 ORR,  LCEMIT LABEL@ BL,
   7 11 16 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 $F2A00010 LIT64,  9 8 7 ORR,  LCEMIT LABEL@ BL,
   7 11 32 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 $F2C00010 LIT64,  9 8 7 ORR,  LCEMIT LABEL@ BL,
   9 $D63F0200 LIT64,  LCEMIT LABEL@ BL,
   2 5 MOVZ,  LPROT LABEL@ BL,
   SP SP 16 ADDI, ;

: BEMIT ( -- )
   A G-POP  13 9 0 ADDI,  G-EMITC ;

: BCR ( -- )
   13 10 MOVZ,  G-EMITC ;

: BSPACE ( -- )
   13 32 MOVZ,  G-EMITC ;

: B.S ( -- )
   LBL LBL {: sl sd :}
   9 DATA S0-CELL LDR,  9 DATA SSCR-CELL STR,
   sl LBL,
      9 DATA SSCR-CELL LDR,  9 XDS CMP,  C-GE sd BCOND,
      9 9 0 LDR,  G-PRINT9
      9 DATA SSCR-CELL LDR,  9 9 8 ADDI,  9 DATA SSCR-CELL STR,
      sl B,
   sd LBL, ;

: BDEPTH ( -- )
   A DATA S0-CELL LDR,
   A XDS A SUB,
   A A 3 ASRI,
   A G-PUSH ;

: (CMP) ( n -- )
   {: cond :}  B G-POP  A G-POP  A B CMP,  A cond CSET,  A SP A SUB,  A G-PUSH ;

: B= ( -- )
   C-EQ (CMP) ;

: B<> ( -- )
   C-NE (CMP) ;

: B< ( -- )
   C-LT (CMP) ;

: B> ( -- )
   C-GT (CMP) ;

: B<= ( -- )
   C-LE (CMP) ;

: B>= ( -- )
   C-GE (CMP) ;

: B0= ( -- )
   A G-POP  A 0 CMPI,  A C-EQ CSET,  A SP A SUB,  A G-PUSH ;

: B0< ( -- )
   A G-POP  A 0 CMPI,  A C-LT CSET,  A SP A SUB,  A G-PUSH ;

: B1+ ( -- )
   A G-POP  A A 1 ADDI,  A G-PUSH ;

: B1- ( -- )
   A G-POP  A A 1 SUBI,  A G-PUSH ;

: BAND ( -- )
   B G-POP A G-POP  A A B AND, A G-PUSH ;

: BOR ( -- )
   B G-POP A G-POP  A A B ORR, A G-PUSH ;

: BXOR ( -- )
   B G-POP A G-POP  A A B EOR, A G-PUSH ;

: BINV ( -- )
   A G-POP  B 0 MOVN,  A A B EOR,  A G-PUSH ;

: BNEG ( -- )
   A G-POP  A SP A SUB,  A G-PUSH ;

: BLSH ( -- )
   B G-POP A G-POP  A A B LSLV, A G-PUSH ;

: BRSH ( -- )
   B G-POP A G-POP  A A B LSRV, A G-PUSH ;

: BDIV0? ( -- )
   LBL {: lok :} B lok CBNZ, BRK, lok LBL, ;   \ SDIV by 0 silently yields 0; trap a zero divisor (B)

: BDIV ( -- )
   B G-POP A G-POP  BDIV0?  A A B SDIV, A G-PUSH ;

: BMOD ( -- )
   B G-POP A G-POP  BDIV0?  C A B SDIV,  C C B MUL,  A A C SUB,  A G-PUSH ;

: BDIVMOD ( -- )
   B G-POP A G-POP  BDIV0?  C A B SDIV,  DREG C B MUL,  A A DREG SUB,  A G-PUSH C G-PUSH ;

: BABS ( -- )
   A G-POP  A 0 CMPI,  LBL {: done :}  C-GE done BCOND,  A SP A SUB,  done LBL,  A G-PUSH ;

: BMIN ( -- )
   B G-POP A G-POP  A B CMP,  LBL {: done :}  C-LE done BCOND,  A B 0 ADDI,  done LBL,  A G-PUSH ;

: BMAX ( -- )
   B G-POP A G-POP  A B CMP,  LBL {: done :}  C-GE done BCOND,  A B 0 ADDI,  done LBL,  A G-PUSH ;

: BNIP ( -- )
   A G-POP  XDS XDS 8 SUBI,  A G-PUSH ;

: BOVER ( -- )
   B G-POP A G-POP  A G-PUSH B G-PUSH A G-PUSH ;

: BTUCK ( -- )
   B G-POP A G-POP  B G-PUSH A G-PUSH B G-PUSH ;

: BROT ( -- )
   C G-POP B G-POP A G-POP  B G-PUSH C G-PUSH A G-PUSH ;

: BMROT ( -- )
   C G-POP B G-POP A G-POP  C G-PUSH A G-PUSH B G-PUSH ;

: B2DUP ( -- )
   B G-POP A G-POP  A G-PUSH B G-PUSH A G-PUSH B G-PUSH ;

: B2DROP ( -- )
   XDS XDS 16 SUBI, ;

: B2SWAP ( -- )
   EREG G-POP DREG G-POP C G-POP A G-POP  DREG G-PUSH EREG G-PUSH A G-PUSH C G-PUSH ;

: B2OVER ( -- )
   EREG G-POP DREG G-POP C G-POP A G-POP  A G-PUSH C G-PUSH DREG G-PUSH EREG G-PUSH A G-PUSH C G-PUSH ;

: BQDUP ( -- )
   A G-POP  A G-PUSH  LBL {: done :}  A done CBZ,  A G-PUSH  done LBL, ;

: BFETCH ( -- )
   A G-POP  A A 0 LDR,  A G-PUSH ;

: BSTORE ( -- )
   B G-POP A G-POP  A B 0 STR, ;

: BPTRFIELD ( -- )
   B G-POP  A G-POP  B B 3 LSLI,  A A B ADD,  A G-PUSH ;

: BPLUSSTORE ( -- )
   B G-POP A G-POP  C B 0 LDR,  C C A ADD,  C B 0 STR, ;

: BCFETCH ( -- )
   A G-POP  A A 0 LDRB, A G-PUSH ;

: BCSTORE ( -- )
   B G-POP A G-POP  A B 0 STRB, ;

: BCELLS ( -- )
   A G-POP  A A 3 LSLI, A G-PUSH ;

: BCELLPLUS ( -- )
   A G-POP  A A 8 ADDI, A G-PUSH ;

: BCHARS ( -- ) ;

: BCHARPLUS ( -- )
   A G-POP  A A 1 ADDI, A G-PUSH ;

: BCOUNT ( -- )
   A G-POP  B A 0 LDRB,  A A 1 ADDI,  A G-PUSH  B G-PUSH ;

: RSTK-PUSH ( n -- )
   {: reg :}
   14 DATA RSP-CELL LDR,
   15 14 3 LSLI,  15 DATA 15 ADD,
   reg 15 RSTK-OFF STR,
   14 14 1 ADDI,  14 DATA RSP-CELL STR, ;

: RSTK-POP ( n -- )
   {: reg :}
   14 DATA RSP-CELL LDR,
   14 14 1 SUBI,
   15 14 3 LSLI,  15 DATA 15 ADD,
   reg 15 RSTK-OFF LDR,
   14 DATA RSP-CELL STR, ;

: B2TOR ( -- )
   B G-POP A G-POP  A RSTK-PUSH  B RSTK-PUSH ;

: B2RFROM ( -- )
   B RSTK-POP  A RSTK-POP  A G-PUSH  B G-PUSH ;

: B2RFETCH ( -- )
   B RSTK-POP  A RSTK-POP  A RSTK-PUSH  B RSTK-PUSH  A G-PUSH  B G-PUSH ;

: BHERE ( -- )
   7 DATA 0 LDR,  7 G-PUSH ;

: DP-CHECK ( n -- )
   {: reg :}
   LBL LBL {: low-ok high-ok :}
   5 DATA-START MOVZ,  5 DATA 5 ADD,
   reg 5 CMP,  C-GE low-ok BCOND,
      0 76 MOVZ,  NR-EXIT SYS,
   low-ok LBL,
   5 DATA-SIZE LIT64,  5 DATA 5 ADD,
   reg 5 CMP,  C-LE high-ok BCOND,
      0 76 MOVZ,  NR-EXIT SYS,
   high-ok LBL, ;

: BALLOT ( -- )
   A G-POP  7 DATA 0 LDR,  7 7 A ADD,  7 DP-CHECK  7 DATA 0 STR, ;

: BCOMMA ( -- )
   A G-POP  7 DATA 0 LDR,  C 7 8 ADDI,  C DP-CHECK  A 7 0 STR,  C DATA 0 STR, ;

: BCCOMMA ( -- )
   A G-POP  7 DATA 0 LDR,  C 7 1 ADDI,  C DP-CHECK  A 7 0 STRB, C DATA 0 STR, ;

: BTYPE ( -- )
   2 G-POP  1 G-POP  0 1 MOVZ,  NR-WRITE SYS, ;

: BDIE ( -- )
   7 G-POP  2 G-POP  1 G-POP  0 2 MOVZ,  NR-WRITE SYS,
          0 7 0 ADDI,  NR-EXIT SYS, ;

: SYS-PUSH ( -- )                  \ push x0, or -1 when the syscall carry is set
   LBL LBL {: ok done :}
   9 C-CS CSET,  9 ok CBZ,
      0 0 MOVN,  done B,
   ok LBL,
   done LBL,
   0 G-PUSH ;

: BOPEN ( -- )
   2 G-POP  1 G-POP  0 G-POP
   HB-TARGET-LINUX? IF
      3 2 0 ADDI,
      OS-OPEN-FLAGS
      1 0 0 ADDI,
      0 99 MOVN,
   THEN
   NR-OPEN SYS,  SYS-PUSH ;

: BWRITE ( -- )
   2 G-POP  1 G-POP  0 G-POP  NR-WRITE SYS,  SYS-PUSH ;

: BREAD ( -- )
   2 G-POP  1 G-POP  0 G-POP  NR-READ SYS,  SYS-PUSH ;

: BIOCTL ( -- )
   2 G-POP  1 G-POP  0 G-POP  NR-IOCTL SYS,  SYS-PUSH ;

: BMMAP ( -- )
   5 G-POP  4 G-POP  3 G-POP  2 G-POP  1 G-POP  0 G-POP
   HB-TARGET-LINUX? IF OS-MMAP-FLAGS THEN
   NR-MMAP SYS,  SYS-PUSH ; \ ( addr len prot flags fd off -- addr|-1 )

\ ---- FFI: AAPCS64 trampolines ----
\ `ffi-call` keeps the old fast path: load 8 cells from argbuf into x0-x7,
\ BLR fn, push x0. `ffi-call-abi`/`ffi-call-abi-r` add x8, d0-d7, caller-packed
\ stack spill, and integer/float return variants for the checked lib/ffi.f API.
\ argbuf must be a >=8-cell (64-byte) buffer; trailing cells are ignored by a
\ callee that takes fewer args. XDS (x19) is AAPCS64 callee-saved so the C call
\ preserves the data stack; x30 is framed by FPRIM (these prims have a BLR).
: BFFI-LOAD-X0-X7 ( -- )
   0 15 0  LDR,   1 15 8  LDR,   2 15 16 LDR,   3 15 24 LDR,
   4 15 32 LDR,   5 15 40 LDR,   6 15 48 LDR,   7 15 56 LDR, ;

: BFFI-CALL ( -- )
   16 G-POP                                            \ x16 = fn
   15 G-POP                                            \ x15 = argbuf
   BFFI-LOAD-X0-X7
   16 BLR,
   0 G-PUSH ;

: BFFI-LOAD-DREG ( n n -- ) {: d:n off:n :}
   9 17 off LDR,  d 9 FMOVXD, ;

: BFFI-LOAD-D0-D7 ( -- )
   0 0 BFFI-LOAD-DREG    1 $8 BFFI-LOAD-DREG
   2 $10 BFFI-LOAD-DREG  3 $18 BFFI-LOAD-DREG
   4 $20 BFFI-LOAD-DREG  5 $28 BFFI-LOAD-DREG
   6 $30 BFFI-LOAD-DREG  7 $38 BFFI-LOAD-DREG ;

: BFFI-COPY-ABI-STACK ( -- )
   LBL {: lskip:label :}  LBL {: lloop:label :}  LBL {: ldone:label :}
   14 0 CMPI,  C-LE lskip BCOND,                      \ stackcells <= 0 -> no spill
   10 14 0 ADDI,                                      \ x10 = cells left
   11 10 3 LSLI,  11 11 $F ADDI,  11 11 4 LSRI,  11 11 4 LSLI,
   12 SP 0 ADDI,  12 12 11 SUB,  SP 12 0 ADDI,        \ sp -= align(cells*8,16)
   12 13 0 ADDI,  13 SP 0 ADDI,                       \ x12=src, x13=dst
   lloop LBL,
      10 ldone CBZ,
      9 12 0 LDR,  9 13 0 STR,
      12 12 $8 ADDI,  13 13 $8 ADDI,
      10 10 1 SUBI,  lloop B,
   ldone LBL,
   lskip LBL, ;

: BFFI-CALL-ABI-CORE ( -- )
   16 G-POP                                            \ x16 = fn
   14 G-POP                                            \ x14 = stack cell count
   13 G-POP                                            \ x13 = prepacked stack cells
   17 G-POP                                            \ x17 = FP argbuf
   15 G-POP                                            \ x15 = integer argbuf
   20 SP $8 STR,                                       \ park caller x20 in frame slot
   20 SP 0 ADDI,                                       \ x20 = frame sp
   BFFI-COPY-ABI-STACK
   BFFI-LOAD-X0-X7
   8 15 $40 LDR,                                       \ x8 = indirect-result address
   BFFI-LOAD-D0-D7
   16 BLR,
   SP 20 0 ADDI,
   20 SP $8 LDR, ;

: BFFI-CALL-ABI ( -- )
   BFFI-CALL-ABI-CORE
   0 G-PUSH ;

: BFFI-CALL-ABI-R ( -- )
   BFFI-CALL-ABI-CORE
   9 0 FMOVDX,  9 G-PUSH ;

\ ---- FFI: general AAPCS64 trampoline, any integer/pointer arity ----
\ ( argbuf nargs fn -- ret ) : x0-x7 from argbuf[0..7]; args 9..nargs spilled to
\ the stack (16-byte aligned per the ABI) by an exact runtime loop -- no arity
\ cap, no garbage slots. argbuf must hold max(8,nargs) cells. The BLR clobbers
\ caller-saved regs, so x20 (callee-saved) carries the frame sp across the call
\ to restore it afterward; the caller's x20 parks in the FPRIM frame's free
\ [sp,#8] slot. Shifted-register SUB treats r31 as XZR not SP, so sp is lowered
\ via a temp. Integer/pointer args only.
: BFFI-CALL-N ( -- )
   16 G-POP                                            \ x16 = fn
   14 G-POP                                            \ x14 = nargs
   15 G-POP                                            \ x15 = argbuf
   20 SP $8 STR,                                       \ park caller x20 in frame slot
   20 SP 0 ADDI,                                       \ x20 = frame sp
   LBL {: lskip :}  LBL {: lloop :}  LBL {: ldone :}
   14 8 CMPI,  C-LE lskip BCOND,                       \ nargs <= 8 -> no spill
      10 14 8 SUBI,                                    \ x10 = extra = nargs - 8
      11 10 3 LSLI,  11 11 $F ADDI,  11 11 4 LSRI,  11 11 4 LSLI,  \ salloc = (extra*8+$F)&~$F
      12 SP 0 ADDI,  12 12 11 SUB,  SP 12 0 ADDI,      \ sp -= salloc
      12 15 $40 ADDI,                                  \ x12 = src = argbuf + 8 cells
      13 SP 0 ADDI,                                    \ x13 = dst = sp
      lloop LBL,
      10 ldone CBZ,                                    \ extra == 0 -> done
         9 12 0 LDR,  9 13 0 STR,                      \ [dst] = [src]
         12 12 $8 ADDI,  13 13 $8 ADDI,               \ src++, dst++
         10 10 1 SUBI,  lloop B,                       \ extra--, loop
      ldone LBL,
   lskip LBL,
   BFFI-LOAD-X0-X7
   16 BLR,
   SP 20 0 ADDI,                                       \ restore sp from x20
   20 SP $8 LDR,                                       \ restore caller x20
   0 G-PUSH ;

: BOPENRD ( -- )
   A G-POP  A OS-OPEN-RD  SYS-PUSH ;

: BACCESS ( -- )
   1 G-POP  0 G-POP
   HB-TARGET-LINUX? IF
      2 1 0 ADDI,  1 0 0 ADDI,  0 99 MOVN,  3 0 MOVZ,
   THEN
   NR-ACCESS SYS,  SYS-PUSH ;

: BUNLINK ( -- )
   0 G-POP
   HB-TARGET-LINUX? IF
      1 0 0 ADDI,  0 99 MOVN,  2 0 MOVZ,
   THEN
   NR-UNLINK SYS,  SYS-PUSH ;

: BRENAME ( -- )
   1 G-POP  0 G-POP
   HB-TARGET-LINUX? IF
      3 1 0 ADDI,  1 0 0 ADDI,  0 99 MOVN,  2 99 MOVN,
   THEN
   NR-RENAME SYS,  SYS-PUSH ;

: BCHMOD ( -- )
   1 G-POP  0 G-POP
   HB-TARGET-LINUX? IF
      2 1 0 ADDI,  1 0 0 ADDI,  0 99 MOVN,  3 0 MOVZ,
   THEN
   NR-CHMOD SYS,  SYS-PUSH ;

: BSYMLINK ( -- )
   2 G-POP  0 G-POP
   HB-TARGET-LINUX? IF 1 99 MOVN, ELSE 1 1 MOVN, THEN
   3 0 MOVZ,  4 0 MOVZ,  5 0 MOVZ,
   NR-SYMLINKAT SYS,  SYS-PUSH ;

: BREADLINK ( -- )
   3 G-POP  2 G-POP  1 G-POP
   HB-TARGET-LINUX? IF 0 99 MOVN, ELSE 0 1 MOVN, THEN
   4 0 MOVZ,  5 0 MOVZ,
   NR-READLINKAT SYS,  SYS-PUSH ;

: BMKDIR ( -- )
   1 G-POP  0 G-POP
   HB-TARGET-LINUX? IF
      2 1 0 ADDI,  1 0 0 ADDI,  0 99 MOVN,
   THEN
   NR-MKDIR SYS,  SYS-PUSH ;

: BRMDIR ( -- )
   0 G-POP
   HB-TARGET-LINUX? IF
      1 0 0 ADDI,  0 99 MOVN,  2 $200 MOVZ,
   THEN
   NR-RMDIR SYS,  SYS-PUSH ;

: LINUX-STAT-FIX ( n -- )
   {: bufreg :}
   5 bufreg 16 LDRW,  5 bufreg 4 STRW,
   5 bufreg 48 LDR,   5 bufreg 96 STR, ;
s" linux-stat-fix" s" n --" TRUST

: BSTAT64 ( -- )
   1 G-POP  0 G-POP
   LBL LBL {: ok done :}
   HB-TARGET-LINUX? IF
      2 1 0 ADDI,  1 0 0 ADDI,  0 99 MOVN,  3 0 MOVZ,
      NR-STAT64 SYS,
      9 C-CS CSET,  9 ok CBZ,
         0 0 MOVN,  done B,
      ok LBL,
      2 LINUX-STAT-FIX
      done LBL,
      0 G-PUSH
      exit
   THEN
   NR-STAT64 SYS,  SYS-PUSH ;

: BLSTAT64 ( -- )
   1 G-POP  0 G-POP  2 0 MOVZ,  3 0 MOVZ,  4 0 MOVZ,  5 0 MOVZ,
   LBL LBL {: ok done :}
   HB-TARGET-LINUX? IF
      2 1 0 ADDI,  1 0 0 ADDI,  0 99 MOVN,  3 AT-SYMLINK-NOFOLLOW MOVZ,
      NR-LSTAT64 SYS,
      9 C-CS CSET,  9 ok CBZ,
         0 0 MOVN,  done B,
      ok LBL,
      2 LINUX-STAT-FIX
      done LBL,
      0 G-PUSH
      exit
   THEN
   NR-LSTAT64 SYS,  SYS-PUSH ;

: BGETDIRENTRIES64 ( -- )
   3 G-POP  2 G-POP  1 G-POP  0 G-POP
   NR-GETDIRENTRIES64 SYS,  SYS-PUSH ;

: C-FLUSH-X9-LINE ( -- )
   9 DCCVAU,  DSB-ISH,  9 ICIVAU,  DSB-ISH,  ISB, ;

: BPATCH32 ( -- )                \ ( w addr -- ): RW-flip, store, RX, cache-sync —
   A G-POP  B G-POP              \ all inside ENGINE text (a JIT-resident caller
   SP SP 32 SUBI,                \ flipping the region would unmap ITSELF)
   A SP 8 STR,  B SP 16 STR,
   2 3 MOVZ,  LPROT LABEL@ BL,
   9 SP 8 LDR,  10 SP 16 LDR,  10 9 0 STRW,
   2 5 MOVZ,  LPROT LABEL@ BL,
   9 SP 8 LDR,  C-FLUSH-X9-LINE
   SP SP 32 ADDI, ;

: BCLOSE ( -- )
   0 G-POP  NR-CLOSE SYS, ;

: BRBASE ( -- )
   9 DATA RBASE-CELL LDR,  9 G-PUSH ;

: BEXEC ( -- )
   A G-POP  SP SP 16 SUBI,  30 SP 0 STR,  A BLR,  30 SP 0 LDR,  SP SP 16 ADDI, ;

: BCATCH ( -- )
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

: BTHROW ( -- )
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

: BWORDLIST ( -- )
   9 DATA WIDN-CELL LDR,  9 G-PUSH  9 9 1 ADDI,  9 DATA WIDN-CELL STR, ;

: BGETCUR ( -- )
   9 DATA CUR-CELL LDR,  9 G-PUSH ;

: BSETCUR ( -- )
   A G-POP  A DATA CUR-CELL STR, ;

: BSETCHECK ( -- )
   A G-POP  A DATA HOOK-CELL STR, ;

: BSWL ( -- )
   LBL LBL LBL LBL LBL LBL LBL LBL {: wl wend wnext wcmp wmatch wf1 wf2 winl :}
   2 G-POP  1 G-POP  0 G-POP
   3 $20 MOVZ,  5 DBASE 0 ADDI,  6 NDICT 0 ADDI,  11 0 MOVZ,
   wl LBL,  6 wend CBZ,
      9 5 40 LDR,  9 2 CMP,  C-NE wnext BCOND,
      9 5 16 LDR,  9 9 4 LSLI,  9 9 4 LSRI,  9 1 CMP,  C-NE wnext BCOND,
      16 5 24 ADDI,
      9 5 16 LDR,  9 9 DNAME-EXT ANDI,  9 winl CBZ,
         16 5 24 LDR,
      winl LBL,
      7 0 MOVZ,
      wcmp LBL,  7 1 CMP,  C-GE wmatch BCOND,
         9 16 7 ADD,  9 9 0 LDRB,
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

: BPARSE-NAME ( -- )
   LBL LBL {: none done :}
   LTOK LABEL@ BL,
   0 none CBZ,
      9 DATA TKA-CELL LDR,  9 G-PUSH
      9 DATA TKL-CELL LDR,  9 G-PUSH
      done B,
   none LBL,
      9 DATA INP-CELL LDR,  9 G-PUSH
      9 0 MOVZ,  9 G-PUSH
   done LBL, ;

: EMIT-ARITH-PRIMS ( -- )
   s" +"    ['] B+    FPRIM-L   s" -"    ['] B-    FPRIM-L   s" *"    ['] B*    FPRIM-L
   s" /"    ['] BDIV  FPRIM-L   s" mod"  ['] BMOD  FPRIM-L   s" /mod" ['] BDIVMOD FPRIM-L
   s" abs"  ['] BABS  FPRIM-L   s" min"  ['] BMIN  FPRIM-L   s" max"  ['] BMAX FPRIM-L ;

: EMIT-COMPARE-PRIMS ( -- )
   s" ="    ['] B=    FPRIM-L   s" <>"   ['] B<>   FPRIM-L   s" <"    ['] B<    FPRIM-L
   s" >"    ['] B>    FPRIM-L   s" <="   ['] B<=   FPRIM-L   s" >="   ['] B>=   FPRIM-L
   s" 0="   ['] B0=   FPRIM-L   s" 0<"   ['] B0<   FPRIM-L
   s" 1+"   ['] B1+   FPRIM-L   s" 1-"   ['] B1-   FPRIM-L
   s" and"  ['] BAND  FPRIM-L   s" or"   ['] BOR   FPRIM-L   s" xor"  ['] BXOR  FPRIM-L
   s" invert" ['] BINV FPRIM-L  s" negate" ['] BNEG FPRIM-L
   s" lshift" ['] BLSH FPRIM-L  s" rshift" ['] BRSH FPRIM-L ;

: EMIT-STACK-PRIMS ( -- )
   s" dup"  ['] BDUP  FPRIM-L   s" drop" ['] BDROP FPRIM-L   s" swap" ['] BSWAP FPRIM-L
   s" nip"  ['] BNIP  FPRIM-L   s" over" ['] BOVER FPRIM-L   s" tuck" ['] BTUCK FPRIM-L
   s" rot"  ['] BROT  FPRIM-L   s" -rot" ['] BMROT FPRIM-L
   s" 2dup" ['] B2DUP FPRIM-L   s" 2drop" ['] B2DROP FPRIM-L
   s" 2swap" ['] B2SWAP FPRIM-L  s" 2over" ['] B2OVER FPRIM-L  s" ?dup" ['] BQDUP FPRIM-L
   s" 2>r" ['] B2TOR FPRIM-L  s" 2r>" ['] B2RFROM FPRIM-L  s" 2r@" ['] B2RFETCH FPRIM-L ;

: EMIT-MEMORY-PRIMS ( -- )
   s" @"    ['] BFETCH FPRIM-L   s" !"    ['] BSTORE FPRIM-L   s" ptr-field" ['] BPTRFIELD FPRIM-L
   s" +!" ['] BPLUSSTORE FPRIM-L
   s" c@"   ['] BCFETCH FPRIM-L  s" c!"   ['] BCSTORE FPRIM-L
   s" cells" ['] BCELLS FPRIM-L  s" cell+" ['] BCELLPLUS FPRIM-L
   s" chars" ['] BCHARS FPRIM-L  s" char+" ['] BCHARPLUS FPRIM-L  s" count" ['] BCOUNT FPRIM-L ;

: EMIT-OUTPUT-PRIMS ( -- )
   s" ."    ['] BDOT  FPRIM-L   s" .s"   ['] B.S   FPRIM-L   s" depth" ['] BDEPTH FPRIM-L
   s" u."   ['] BU.   FPRIM-L   s" emit" ['] BEMIT FPRIM-L
   s" cr"   ['] BCR   FPRIM-L   s" space" ['] BSPACE FPRIM-L
   s" type" ['] BTYPE  FPRIM-L ;

: EMIT-DICT-PRIMS ( -- )
   s" here" ['] BHERE  FPRIM-L   s" allot" ['] BALLOT FPRIM-L
   s" ,"    ['] BCOMMA FPRIM-L   s" c,"   ['] BCCOMMA FPRIM-L
   s" execute" ['] BEXEC FPRIM
   s" compile," ['] BCOMPILE FPRIM
   s" create" ['] BCREATE FPRIM
   s" parse-name" ['] BPARSE-NAME FPRIM
   s" evaluate" ['] B-EVAL FPRIM-L ;

: EMIT-PROCESS-PRIMS ( -- )
   s" run-rc" ['] BRUNRC FPRIM-L
   s" pipe" ['] BPIPE FPRIM-L   s" dup2" ['] BDUP2 FPRIM-L
   s" fcntl" ['] BFCNTL FPRIM-L   s" poll" ['] BPOLL FPRIM-L
   s" kill" ['] BKILL FPRIM-L
   s" spawn-io" ['] BSPAWNIO FPRIM-L
   s" spawn-argv-io" ['] BSPAWNARGVIO FPRIM-L
   s" spawn-argv-env-io" ['] BSPAWNARGVENVIO FPRIM-L
   s" spawn-argv-env-cwd-io" ['] BSPAWNARGVENVCWDIO FPRIM-L
   s" wait-rc" ['] BWAITRC FPRIM-L
   s" wait-status" ['] BWAITSTATUS FPRIM-L ;

: EMIT-ENGINE-PRIMS ( -- )
   s" cp@" ['] BCPFETCH FPRIM-L   s" dbase@" ['] BDBASEFETCH FPRIM-L
   s" data-base" ['] BDATAFETCH FPRIM-L
   s" ndict@" ['] BNDICTFETCH FPRIM-L
   s" cp!" ['] BCPSET FPRIM-L   s" ndict!" ['] BNDSET FPRIM-L
   s" epoch-seconds" ['] BEPOCHSECONDS FPRIM-L
   s" mono-ns" ['] BMONONS FPRIM-L
   s" die"  ['] BDIE   FPRIM-L ;

: EMIT-FS-PRIMS ( -- )
   s" open" ['] BOPEN FPRIM-L   s" write" ['] BWRITE FPRIM-L   s" read" ['] BREAD FPRIM-L   s" ioctl" ['] BIOCTL FPRIM-L
   s" mmap" ['] BMMAP FPRIM-L
   s" ffi-call" ['] BFFI-CALL FPRIM
   s" ffi-call-n" ['] BFFI-CALL-N FPRIM
   s" ffi-call-abi" ['] BFFI-CALL-ABI FPRIM
   s" ffi-call-abi-r" ['] BFFI-CALL-ABI-R FPRIM
   s" open-rd" ['] BOPENRD FPRIM-L
   s" access" ['] BACCESS FPRIM-L
   s" unlink" ['] BUNLINK FPRIM-L   s" rename" ['] BRENAME FPRIM-L   s" chmod" ['] BCHMOD FPRIM-L
   s" symlink" ['] BSYMLINK FPRIM-L   s" readlink" ['] BREADLINK FPRIM-L
   s" mkdir" ['] BMKDIR FPRIM-L     s" rmdir" ['] BRMDIR FPRIM-L
   s" stat64" ['] BSTAT64 FPRIM-L   s" lstat64" ['] BLSTAT64 FPRIM-L
   s" getdirentries64" ['] BGETDIRENTRIES64 FPRIM-L
   s" patch32" ['] BPATCH32 FPRIM
   s" close" ['] BCLOSE FPRIM-L
   s" rbase" ['] BRBASE FPRIM-L ;

: EMIT-CHECKER-PRIMS ( -- )
   s" catch" ['] BCATCH FPRIM   s" throw" ['] BTHROW FPRIM-L
   s" wordlist" ['] BWORDLIST FPRIM-L   s" get-current" ['] BGETCUR FPRIM-L
   s" set-current" ['] BSETCUR FPRIM-L  s" search-wl" ['] BSWL FPRIM-L
   s" set-check" ['] BSETCHECK FPRIM-L ;

: EMIT-PRIMS ( -- )
   EMIT-ARITH-PRIMS  EMIT-COMPARE-PRIMS  EMIT-STACK-PRIMS
   EMIT-MEMORY-PRIMS  EMIT-OUTPUT-PRIMS  EMIT-DICT-PRIMS
   EMIT-PROCESS-PRIMS  EMIT-ENGINE-PRIMS  EMIT-FS-PRIMS
   EMIT-CHECKER-PRIMS ;
s" emit-prims" s" --" TRUST

\ FP: doubles as raw IEEE754 bit-cells on the data stack; FMOV through D0/D1.
\ Compare conds per FP flag semantics: < MI, > GT, = EQ (NaN compares false).
: BF+ ( -- )
   B G-POP  A G-POP  0 A FMOVXD,  1 B FMOVXD,  0 0 1 FADD,  A 0 FMOVDX,  A G-PUSH ;

: BF- ( -- )
   B G-POP  A G-POP  0 A FMOVXD,  1 B FMOVXD,  0 0 1 FSUB,  A 0 FMOVDX,  A G-PUSH ;

: BF* ( -- )
   B G-POP  A G-POP  0 A FMOVXD,  1 B FMOVXD,  0 0 1 FMUL,  A 0 FMOVDX,  A G-PUSH ;

: BF/ ( -- )
   B G-POP  A G-POP  0 A FMOVXD,  1 B FMOVXD,  0 0 1 FDIV,  A 0 FMOVDX,  A G-PUSH ;

: BFNEG ( -- )
   A G-POP  0 A FMOVXD,  0 0 FNEG,   A 0 FMOVDX,  A G-PUSH ;

: BFABS ( -- )
   A G-POP  0 A FMOVXD,  0 0 FABS,   A 0 FMOVDX,  A G-PUSH ;

: BFSQRT ( -- )
   A G-POP  0 A FMOVXD,  0 0 FSQRT,  A 0 FMOVDX,  A G-PUSH ;

: (FCMP) ( n -- )
   {: cond :}  B G-POP  A G-POP  0 A FMOVXD,  1 B FMOVXD,  0 1 FCMP,
   A cond CSET,  A SP A SUB,  A G-PUSH ;

: BF< ( -- )
   C-MI (FCMP) ;

: BF> ( -- )
   C-GT (FCMP) ;

: BF= ( -- )
   C-EQ (FCMP) ;

: (FCMP0) ( n -- )
   {: cond :}  A G-POP  0 A FMOVXD,  0 FCMP0,
   A cond CSET,  A SP A SUB,  A G-PUSH ;

: BF0< ( -- )
   C-MI (FCMP0) ;

: BF0= ( -- )
   C-EQ (FCMP0) ;

: BS>F ( -- )
   A G-POP  0 A SCVTF,   A 0 FMOVDX,  A G-PUSH ;

: BF>S ( -- )
   A G-POP  0 A FMOVXD,  A 0 FCVTZS,  A G-PUSH ;

: BFDOT ( -- )
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

: EMIT-FP-PRIMS ( -- )
   s" f+" ['] BF+ FPRIM-L   s" f-" ['] BF- FPRIM-L   s" f*" ['] BF* FPRIM-L
   s" f/" ['] BF/ FPRIM-L   s" fnegate" ['] BFNEG FPRIM-L
   s" fabs" ['] BFABS FPRIM-L  s" fsqrt" ['] BFSQRT FPRIM-L
   s" f<" ['] BF< FPRIM-L   s" f>" ['] BF> FPRIM-L   s" f=" ['] BF= FPRIM-L
   s" f0<" ['] BF0< FPRIM-L  s" f0=" ['] BF0= FPRIM-L
   s" s>f" ['] BS>F FPRIM-L  s" f>s" ['] BF>S FPRIM-L
   s" f." ['] BFDOT FPRIM-L ;
s" emit-fp-prims" s" --" TRUST

: EMIT-CEMIT ( -- )
   LCEMIT LABEL@ LBL,  9 28 0 STRW,  28 28 4 ADDI,  RET, ;

\ LBCAP ( -- ) : append TKA/TKL + ' ' to the body capture. LBCS ( x11=a x12=u )
\ is the general entry (defining-word kind tokens). FATAL (exit 71) on overflow —
\ truncation would let the check hook certify code it never saw.
: EMIT-BCAP ( -- )
   LBCAP LABEL@ LBL,
   11 DATA TKA-CELL LDR,  12 DATA TKL-CELL LDR,
   LBCS LABEL@ LBL,
   LBL LBL LBL {: bok bcp bcd :}
   17 12 0 ADDI,                  \ len in x17 (IP1): callers keep state in x5-x8
   14 DATA BODYLEN-CELL LDR,
   16 14 17 ADD,  16 16 1 ADDI,
   5 BODYBUF-CAP MOVZ,  16 5 CMP,  C-LE bok BCOND,
      0 2 MOVZ,  1 11 0 ADDI,  2 12 0 ADDI,  NR-WRITE SYS,
      0 71 MOVZ,  NR-EXIT SYS,
   bok LBL,
   15 DATA BODYBUF-OFF ADDI,  15 15 14 ADD,
   bcp LBL,  12 bcd CBZ,  13 11 0 LDRB,  13 15 0 STRB,
      15 15 1 ADDI,  11 11 1 ADDI,  12 12 1 SUBI,  bcp B,
   bcd LBL,  13 32 MOVZ,  13 15 0 STRB,
   14 14 17 ADD,  14 14 1 ADDI,  14 DATA BODYLEN-CELL STR,
   RET, ;

: EMIT-TOK ( -- )
   LTOK LABEL@ LBL,
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

: EMIT-PROT ( -- )
   LPROT LABEL@ LBL,
   0 DBASE 0 ADDI,  1 REGION LIT64,  NR-MPROTECT SYS,  RET, ;

: EMIT-FLUSH ( -- )
   LFLUSH LABEL@ LBL,
   LBL LBL LBL LBL {: fdl fdd fil fid :}
   9 9 6 LSRI,  9 9 6 LSLI,                                 \ align start down to the
   10 9 0 ADDI,                                             \ line, or the 64-byte
                                                            \ stride skips the last one
   fdl LBL,  10 CP CMP,  C-GE fdd BCOND,  10 DCCVAU,  10 10 64 ADDI,  fdl B,
   fdd LBL,  DSB-ISH,
   10 9 0 ADDI,
   fil LBL,  10 CP CMP,  C-GE fid BCOND,  10 ICIVAU,  10 10 64 ADDI,  fil B,
   fid LBL,  DSB-ISH,  ISB,  RET, ;

: EMIT-FIND ( -- )
   LFIND LABEL@ LBL,
   LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL
   {: qscan qnone qhas qbad qtail qtailok nloop nnext ncmp nmatch nend ninl
      fstart floop fdone fnext fcmp fmatch finl fmiss ftryglobal ffound :}
   13 0 MOVZ,
   17 0 MOVZ,
   qscan LBL,
      17 10 CMP,  C-GE qnone BCOND,
      14 9 17 ADD,  14 14 0 LDRB,  14 $3A CMPI,  C-EQ qhas BCOND,
      17 17 1 ADDI,  qscan B,
   qnone LBL,
      2 DATA PKG-PRI-CELL LDR,  fstart B,
   qhas LBL,
      17 0 CMPI,  C-EQ qnone BCOND,
      14 17 1 ADDI,  14 10 CMP,  C-GE qnone BCOND,
      14 17 1 ADDI,
   qtail LBL,
      14 10 CMP,  C-GE qtailok BCOND,
      15 9 14 ADD,  15 15 0 LDRB,  15 $3A CMPI,  C-EQ qbad BCOND,
      14 14 1 ADDI,  qtail B,
   qtailok LBL,
      5 DBASE 0 ADDI,  6 NDICT 0 ADDI,
   nloop LBL,
      6 nend CBZ,
      14 5 40 LDR,  15 0 MOVN,  14 15 CMP,  C-NE nnext BCOND,
      14 5 16 LDR,  14 14 4 LSLI,  14 14 4 LSRI,  14 17 CMP,  C-NE nnext BCOND,
      16 5 24 ADDI,
      14 5 16 LDR,  14 14 DNAME-EXT ANDI,  14 ninl CBZ,
         16 5 24 LDR,
      ninl LBL,
      7 0 MOVZ,
      ncmp LBL,
         7 17 CMP,  C-GE nmatch BCOND,
         15 16 7 ADD,  15 15 0 LDRB,
         3 15 $41 SUBI,  3 $1A CMPI,  3 C-CC CSET,  3 3 5 LSLI,  15 15 3 ORR,
         4 9 7 ADD,     4 4 0 LDRB,
         3 4 $41 SUBI,   3 $1A CMPI,  3 C-CC CSET,  3 3 5 LSLI,  4 4 3 ORR,
         15 4 CMP,  C-NE nnext BCOND,
         7 7 1 ADDI,  ncmp B,
      nmatch LBL,
         2 5 0 LDR,
         9 9 17 ADD,  9 9 1 ADDI,
         10 10 17 SUB,  10 10 1 SUBI,
         fstart B,
      nnext LBL,  5 5 DREC ADDI,  6 6 1 SUBI,  nloop B,
   nend LBL,  RET,
   qbad LBL,  RET,
   fstart LBL,
      5 DBASE 0 ADDI,  6 NDICT 0 ADDI,
   floop LBL,
      6 fdone CBZ,
      14 5 40 LDR,  14 2 CMP,  C-NE fnext BCOND,
      14 5 16 LDR,  14 14 4 LSLI,  14 14 4 LSRI,  14 10 CMP,  C-NE fnext BCOND,
      16 5 24 ADDI,
      14 5 16 LDR,  14 14 DNAME-EXT ANDI,  14 finl CBZ,
         16 5 24 LDR,
      finl LBL,
      7 0 MOVZ,
      fcmp LBL,
         7 10 CMP,  C-GE fmatch BCOND,
         15 16 7 ADD,  15 15 0 LDRB,
         3 15 $41 SUBI,  3 $1A CMPI,  3 C-CC CSET,  3 3 5 LSLI,  15 15 3 ORR,
         4 9 7 ADD,     4 4 0 LDRB,
         3 4 $41 SUBI,   3 $1A CMPI,  3 C-CC CSET,  3 3 5 LSLI,  4 4 3 ORR,
         15 4 CMP,  C-NE fnext BCOND,
         7 7 1 ADDI,  fcmp B,
      fmatch LBL,
         11 5 0 LDR,  12 5 8 LDR,
         14 5 16 LDR,  14 14 DNAME-IMM ANDI,  14 14 59 LSRI,   \ immediate bit -> 2
         13 1 MOVZ,  13 13 14 ORR,  fnext B,
      fnext LBL,  5 5 DREC ADDI,  6 6 1 SUBI,  floop B,
   fdone LBL,
      13 ffound CBNZ,
      14 DATA PKG-PRI-CELL LDR,  14 fmiss CBZ,
      14 2 CMP,  C-NE ftryglobal BCOND,
         2 DATA PKG-PUB-CELL LDR,  fstart B,
      ftryglobal LBL,
      14 DATA PKG-PUB-CELL LDR,  14 2 CMP,  C-NE fmiss BCOND,
         2 0 MOVZ,  fstart B,
      ffound LBL,
      fmiss LBL,  RET, ;

: C-NUM-INIT-REGS ( -- )
   11 0 MOVZ,  13 1 MOVZ,  14 0 MOVZ,  12 0 MOVZ,  6 10 MOVZ, ;

: C-NUM-SIGN ( label label -- ) {: ldone:label ndoll:label :}
   10 ldone CBZ,
   15 9 0 LDRB,  15 45 CMPI,  C-NE ndoll BCOND,
      13 0 MOVN,  14 1 MOVZ,
   ndoll LBL,
   14 10 CMP,  C-GE ldone BCOND, ;

: C-NUM-BASE ( label label -- ) {: ldone:label nohex:label :}
   5 9 14 ADD,  15 5 0 LDRB,  15 36 CMPI,  C-NE nohex BCOND,
      6 16 MOVZ,  14 14 1 ADDI,
   nohex LBL,
   2 0 MOVZ,                                                    \ frac mode off
   14 10 CMP,  C-GE ldone BCOND, ;

: C-NUM-DOT ( label label label -- ) {: ldone:label lloop:label ndot:label :}
   15 46 CMPI,  C-NE ndot BCOND,                                \ '.' -> frac mode
      6 10 CMPI,  C-NE ldone BCOND,                             \ only base 10
      2 ldone CBNZ,                                             \ second dot -> fail
      2 1 MOVZ,  4 0 MOVZ,  3 1 MOVZ,                           \ frac=0 scale=1
      14 14 1 ADDI,  lloop B,
   ndot LBL, ;

: C-NUM-DIGIT ( label label label label -- ) {: ldone:label gotd:label nd:label nuc:label :}
   15 48 CMPI,  C-LT ldone BCOND,
   15 57 CMPI,  C-GT nd BCOND,
      7 15 48 SUBI,  gotd B,
   nd LBL,
   6 16 CMPI,  C-NE ldone BCOND,
   15 97 CMPI,  C-LT nuc BCOND,  15 102 CMPI,  C-GT ldone BCOND,
      7 15 87 SUBI,  gotd B,
   nuc LBL,
   15 65 CMPI,  C-LT ldone BCOND,  15 70 CMPI,  C-GT ldone BCOND,
      7 15 55 SUBI, ;

: C-NUM-INT-STEP ( label -- ) {: lloop:label :}
   11 11 6 MUL,  11 11 7 ADD,
   14 14 1 ADDI,  lloop B, ;

: C-NUM-FRAC-STEP ( label -- ) {: lloop:label :}
   5 10 MOVZ,  4 4 5 MUL,  4 4 7 ADD,  3 3 5 MUL,
   14 14 1 ADDI,  lloop B, ;

: C-NUM-FLOAT-FINISH ( label label -- ) {: ldone:label fpos:label :}
   3 1 CMPI,  C-EQ ldone BCOND,                                 \ "1." (no frac digits) -> fail
   0 11 SCVTF,  1 4 SCVTF,  2 3 SCVTF,                          \ int, frac, scale
   1 1 2 FDIV,  0 0 1 FADD,
   13 0 CMPI,  C-GE fpos BCOND,  0 0 FNEG,
   fpos LBL,  11 0 FMOVDX,  12 1 MOVZ,  RET, ;

: C-NUM-INT-FINISH ( -- )
   11 11 13 MUL,  12 1 MOVZ, ;

: EMIT-NUM ( -- )
   LNUM LABEL@ LBL,
   LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL
   {: ldone ndoll nohex lloop lok gotd nd nuc ndot isfrac lint fpos :}
   C-NUM-INIT-REGS
   ldone ndoll C-NUM-SIGN
   ldone nohex C-NUM-BASE
   lloop LBL,
   14 10 CMP,  C-GE lok BCOND,
   5 9 14 ADD,  15 5 0 LDRB,
   ldone lloop ndot C-NUM-DOT
   ldone gotd nd nuc C-NUM-DIGIT
   gotd LBL,
   2 isfrac CBNZ,
   lloop C-NUM-INT-STEP
   isfrac LBL,                                                  \ frac digit: f=f*10+d, k*=10
   lloop C-NUM-FRAC-STEP
   lok LBL,
   2 lint CBZ,
   ldone fpos C-NUM-FLOAT-FINISH
   lint LBL,  C-NUM-INT-FINISH
   ldone LBL,  RET, ;

: EMIT-DICT ( -- )
   0 BEGIN dup #PL @ < WHILE
      dup cells PLEN + @ DNAME-INL > IF
         LBL over cells PNLBL + !
         dup cells PNLBL + LABEL@ LBL,
         dup cells PNAM + @ over cells PLEN + @ BYTES,
      ELSE
         -1 over cells PNLBL + !
      THEN
      1 + REPEAT drop
   LNCOUNT LABEL@ LBL,  #PL @ DCQ,
   LDICT LABEL@ LBL,
   0 BEGIN dup #PL @ < WHILE
      dup cells PLBL + LABEL@ DLBL,
      dup cells PEL  + LABEL@ DLBL,
      dup cells PLEN + @ DNAME-INL > IF
         dup cells PLEN + @ DNAME-EXT or DCQ,
         dup cells PNLBL + LABEL@ DLBL,
         0 DCQ,
      ELSE
         dup cells PLEN + @ DCQ,
         dup cells PNAM + @  over cells PLEN + @  BYTES,
         16  over cells PLEN + @  3 + -4 and  -  dup 0 > IF PNPOOL swap BYTES, ELSE drop THEN
      THEN
      0 DCQ,
      1 + REPEAT drop ;

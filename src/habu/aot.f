\ aot.f — AOT linker driver. The MAKER compiles the program in-process (we point
\ INP/INE at the program text + an AOT-LINK sentinel and return, so the maker's
\ own interpret loop compiles the definitions into its JIT region), then AOT-LINK
\ serializes ONLY the native-reachable closure of MAIN into a standalone binary:
\ a minimal runtime entry (reserve the value stack, x19=sp) + `BL MAIN; exit`,
\ with every other word, the interpreter, the compiler and the parser stripped.
\ All reachable blobs are copied into the output __text and their absolute
\ inter-word calls relocated. The program MUST define `: MAIN ;`.
\ tools/hb-aot.sh owns the I/O paths. A DRIVER (appended last, like build.f).

variable PB  variable PN  variable PFD  variable PRD
$40000 constant PMAX
: AOT-IN   s" hb-aot-src" TMP-PATH ;
: AOT-OUT  s" hb-aot-got" TMP-PATH ;

: READ-PROG
   AOT-IN PATH0  0 0 open PFD !
   here PB !  PMAX allot  0 PN !
   BEGIN  PFD @  PB @ PN @ +  PMAX PN @ -  read PRD !  PRD @ 0 > WHILE  PN @ PRD @ + PN !  REPEAT
   PFD @ close
   PN @ 0 > 0= IF s" aot: empty source" 74 die THEN ;
: SENTSET  s"  AOT-LINK " {: sa su :}  su 0 ?do  sa i + c@  PB @ PN @ + i + c!  loop  PN @ su + PN ! ;

\ --- read 32-bit words; recognize the compiled call (movz/movk/movk x16 + blr x16)
: W32@ {: a :} a c@  a 1+ c@ 8 lshift or  a 2 + c@ 16 lshift or  a 3 + c@ 24 lshift or ;
: W32! {: w a :} w a c!  w 8 rshift a 1+ c!  w 16 rshift a 2 + c!  w 24 rshift a 3 + c! ;
: TGT {: p :} p W32@ 5 rshift $FFFF and
   p 4 + W32@ 5 rshift $FFFF and 16 lshift or
   p 8 + W32@ 5 rshift $FFFF and 32 lshift or ;
: CALL? {: p :} p W32@ $FFE0001F and $D2800010 =
   p 4 + W32@ $FFE0001F and $F2A00010 = and
   p 8 + W32@ $FFE0001F and $F2C00010 = and
   p 12 + W32@ $D63F0200 = and ;

: REC {: k :}  dbase@ k 48 * + ;          \ dict record k  (0:addr 8:len 16:nlen 24:name)
: FOLD {: c :}  c 64 > c 91 < and IF c 32 + ELSE c THEN ;
: MAIN? {: r :}  r 16 + @ $FF and 4 =
   r 24 + c@ FOLD 109 = and  r 25 + c@ FOLD 97 = and
   r 26 + c@ FOLD 105 = and  r 27 + c@ FOLD 110 = and ;

variable FX
: FINDADDR {: t :}  0 FX !
   BEGIN FX @ ndict@ < WHILE  FX @ REC @ t = IF FX @ REC exit THEN  FX @ 1+ FX ! REPEAT  0 ;
: FINDMAIN  0 FX !
   BEGIN FX @ ndict@ < WHILE  FX @ REC MAIN? IF FX @ REC exit THEN  FX @ 1+ FX ! REPEAT  0 ;

\ --- closure: BFS from MAIN over the native call graph.
create CLO 256 cells allot   variable NCLO  variable CX
: IN-CLO? {: r :}  0 CX ! BEGIN CX @ NCLO @ < WHILE CX @ cells CLO + @ r = IF -1 exit THEN CX @ 1+ CX ! REPEAT 0 ;
: ADD-CLO {: r :}  r IN-CLO? IF exit THEN  r NCLO @ cells CLO + !  NCLO @ 1+ NCLO ! ;
variable SP2  variable SEND
: SCAN-REC {: r :}
   r @ SP2 !  r @ r 8 + @ + SEND !
   BEGIN SP2 @ SEND @ < WHILE
      SP2 @ CALL? IF  SP2 @ TGT FINDADDR dup IF ADD-CLO ELSE drop THEN  SP2 @ 16 + SP2 !
      ELSE SP2 @ 4 + SP2 ! THEN
   REPEAT ;
variable WI
: CLOSURE  0 NCLO !  FINDMAIN dup 0= IF drop s" aot: no MAIN" 74 die THEN  ADD-CLO
   0 WI ! BEGIN WI @ NCLO @ < WHILE  WI @ cells CLO + @ SCAN-REC  WI @ 1+ WI ! REPEAT ;

\ --- emit the image: minimal entry + copied blobs, then relocate the calls.
variable MLBL  variable REC2
create OLDA 256 cells allot   create NEWOFF 256 cells allot   create BLEN 256 cells allot
: EMIT-ENTRY
   SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,
   SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,
   XDS SP 0 ADDI,
   MLBL @ BL,                              \ bl MAIN (resolved when MLBL is placed)
   0 0 MOVZ,  NR-EXIT SYS, ;               \ exit(0)
: COPY-BLOBS
   0 WI ! BEGIN WI @ NCLO @ < WHILE
      WI @ cells CLO + @ REC2 !
      WI @ 0= IF MLBL @ LBL, THEN          \ MAIN is closure word 0 -> place its label
      REC2 @ @         OLDA   WI @ cells + !
      ASM-LEN          NEWOFF WI @ cells + !
      REC2 @ 8 + @ 4 + BLEN   WI @ cells + !   \ dict len excludes the trailing RET — add it back
      REC2 @ @  REC2 @ 8 + @ 4 +  BYTES,       \ copy the blob (incl. RET) into CODE
      WI @ 1+ WI ! REPEAT ;
\ code offset (in CODE) of the blob whose OLD addr is t, or -1. The binary is PIE
\ (arm64 macOS requires it), so absolute call targets would be wrong under the
\ ASLR slide — instead we rewrite each abs call to a PC-RELATIVE bl, whose offset
\ within __text is slide-independent. No runtime relocation needed.
: CLO-OFF {: t :}  0 CX !
   BEGIN CX @ NCLO @ < WHILE
      OLDA CX @ cells + @ t = IF  NEWOFF CX @ cells + @  exit THEN
      CX @ 1+ CX ! REPEAT  -1 ;
\ replace the 4-instr `movz/movk/movk x16; blr x16` at CODE byte offset `site`
\ with `nop; nop; nop; bl target` (target = the callee's CODE offset).
: PATCH-BL {: site target :}
   $D503201F CODE site +     W32!
   $D503201F CODE site 4 + + W32!
   $D503201F CODE site 8 + + W32!
   target  site 12 + -  4 /  $3FFFFFF and  $94000000 or  CODE site 12 + + W32! ;
variable RP  variable RE  variable RV
: RELOCATE
   0 WI ! BEGIN WI @ NCLO @ < WHILE
      NEWOFF WI @ cells + @ RP !
      RP @ BLEN WI @ cells + @ + RE !
      BEGIN RP @ RE @ < WHILE
         CODE RP @ + CALL? IF
            CODE RP @ + TGT CLO-OFF RV !
            RV @ -1 <> IF RP @ RV @ PATCH-BL THEN
            RP @ 16 + RP !
         ELSE RP @ 4 + RP ! THEN
      REPEAT
      WI @ 1+ WI ! REPEAT ;

: AOT-LINK
   CLOSURE  ASM-INIT  LBL MLBL !
   EMIT-ENTRY  COPY-BLOBS  RELOCATE
   ASM-CODE  BUILD-IMAGE  s" hb-aot" SET-SIGID  CODESIG2
   AOT-OUT PATH0  1537 493 open  dup MBUF MLEN @ write drop  close ;

: GO  READ-PROG  SENTSET
   PB @ DATA-VA INP-CELL + !
   PB @ PN @ + DATA-VA INE-CELL + ! ;
GO

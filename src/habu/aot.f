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

: USER-HOOK
   CHECK!  dup -1 <> IF s" hb-build: check did not certify" 70 die THEN ;

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

: REC-NAME= {: r a u :}
   r 16 + @ $FF and u = IF
      0 BEGIN dup u < WHILE
         dup r 24 + swap + c@ FOLD
         over a + c@ FOLD = 0= IF drop 0 EXIT THEN
         1 +
      REPEAT drop -1
   ELSE 0 THEN ;
: AOT-UNSAFE? {: r :}
   r s" @" REC-NAME= IF -1 EXIT THEN
   r s" !" REC-NAME= IF -1 EXIT THEN
   r s" c@" REC-NAME= IF -1 EXIT THEN
   r s" c!" REC-NAME= IF -1 EXIT THEN
   r s" here" REC-NAME= IF -1 EXIT THEN
   r s" allot" REC-NAME= IF -1 EXIT THEN
   r s" ," REC-NAME= IF -1 EXIT THEN
   r s" c," REC-NAME= IF -1 EXIT THEN
   r s" create" REC-NAME= IF -1 EXIT THEN
   r s" compile," REC-NAME= IF -1 EXIT THEN
   r s" patch32" REC-NAME= IF -1 EXIT THEN
   0 ;
create AECH 1 allot
: AE1 {: c :}  c AECH c!  2 AECH 1 write drop ;
: AETXT {: a u :}  2 a u write drop ;
: AEJCHAR {: c :}
   c 10 = IF 92 AE1 110 AE1 EXIT THEN
   c 13 = IF 92 AE1 114 AE1 EXIT THEN
   c 9 = IF 92 AE1 116 AE1 EXIT THEN
   c 34 =  c 92 = or IF 92 AE1 THEN  c AE1 ;
: AEJSTR {: a u :}  34 AE1  0 BEGIN dup u < WHILE dup a + c@ AEJCHAR 1 + REPEAT drop 34 AE1 ;
: AEJKEY {: a u :}  a u AEJSTR 58 AE1 ;
: AOT-UNSAFE-JSON {: caller callee :}
   123 AE1
   s" code" AEJKEY s" E-AOT-UNSUPPORTED" AEJSTR 44 AE1
   s" verdict" AEJKEY s" rejected" AEJSTR 44 AE1
   s" word" AEJKEY caller 24 + caller 16 + @ $FF and AEJSTR 44 AE1
   s" token" AEJKEY callee 24 + callee 16 + @ $FF and AEJSTR 44 AE1
   s" suggestion" AEJKEY
   s" stripped AOT has no persistent data region; use --repl/snapshot for data-space words or remove the runtime data access" AEJSTR
   125 AE1 10 AE1 ;
: AOT-UNSAFE-PROSE {: caller callee :}
   s" hb-build: stripped AOT unsupported word '" AETXT
   callee 24 + callee 16 + @ $FF and AETXT
   s" ' called by '" AETXT
   caller 24 + caller 16 + @ $FF and AETXT
   s" '" AETXT 10 AE1 ;
: AOT-UNSAFE-DIE {: caller callee :}
   JSON-DIAGS @ IF caller callee AOT-UNSAFE-JSON ELSE caller callee AOT-UNSAFE-PROSE THEN
   s" hb-build: AOT unsupported word" 70 die ;

variable FX
: FINDADDR {: t :}  0 FX !
   BEGIN FX @ ndict@ < WHILE  FX @ REC @ t = IF FX @ REC exit THEN  FX @ 1+ FX ! REPEAT  0 ;
: FINDMAIN  0 FX !
   BEGIN FX @ ndict@ < WHILE  FX @ REC MAIN? IF FX @ REC exit THEN  FX @ 1+ FX ! REPEAT  0 ;

\ --- closure: BFS from MAIN over the native call graph. CLO and the parallel
\ COPY/RELOCATE arrays (OLDA/NEWOFF/BLEN) are all sized by MAX-CLO; ADD-CLO fails
\ closed at the cap so a large closure can never write past the tables.
1024 constant MAX-CLO
create CLO MAX-CLO cells allot   variable NCLO  variable CX
: IN-CLO? {: r :}  0 CX ! BEGIN CX @ NCLO @ < WHILE CX @ cells CLO + @ r = IF -1 exit THEN CX @ 1+ CX ! REPEAT 0 ;
: ADD-CLO {: r :}  r IN-CLO? IF exit THEN
   NCLO @ MAX-CLO >= IF s" aot: closure exceeds MAX-CLO" 74 die THEN
   r NCLO @ cells CLO + !  NCLO @ 1+ NCLO ! ;
variable SP2  variable SEND
: SCAN-REC {: r :}
   r @ SP2 !  r @ r 8 + @ + SEND !
   BEGIN SP2 @ SEND @ < WHILE
      SP2 @ CALL? IF
         SP2 @ TGT FINDADDR dup IF
            dup AOT-UNSAFE? IF r swap AOT-UNSAFE-DIE THEN
            ADD-CLO
         ELSE drop THEN
         SP2 @ 16 + SP2 !
      ELSE SP2 @ 4 + SP2 ! THEN
   REPEAT ;
variable WI
: CLOSURE  0 NCLO !  FINDMAIN dup 0= IF drop s" aot: no MAIN" 74 die THEN  ADD-CLO
   0 WI ! BEGIN WI @ NCLO @ < WHILE  WI @ cells CLO + @ SCAN-REC  WI @ 1+ WI ! REPEAT ;

\ --- emit the image: minimal entry + copied blobs, then relocate the calls.
variable MLBL  variable REC2
create OLDA MAX-CLO cells allot   create NEWOFF MAX-CLO cells allot   create BLEN MAX-CLO cells allot
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
   ['] USER-HOOK set-check
   PB @ DATA-VA INP-CELL + !
   PB @ PN @ + DATA-VA INE-CELL + ! ;
GO

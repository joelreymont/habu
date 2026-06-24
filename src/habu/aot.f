\ aot.f — AOT linker driver. The MAKER compiles the program in-process (we point
\ INP/INE at the program text + an AOT-LINK sentinel and return, so the maker's
\ own interpret loop compiles the definitions into its JIT region), then AOT-LINK
\ serializes ONLY the native-reachable closure of MAIN into a standalone binary:
\ a minimal runtime entry (reserve the value stack, x19=sp) + `BL MAIN; exit`,
\ with every other word, the interpreter, the compiler and the parser stripped.
\ All reachable blobs are copied into the output __text and their absolute
\ inter-word calls relocated. The program MUST define `: MAIN ;`.
\ tools/hb-build.f owns the I/O paths. A DRIVER (appended last, like build.f).

\ Audited driver boundary: the toolchain hook is on when this file is appended;
\ the driver installs USER-HOOK below for user source only.
0 set-check

variable PB  variable PN  variable PFD  variable PRD
variable SI
$40000 constant PMAX
: AOT-PB@ PB @ ;
s" AOT-PB@" s" -- ptr u8" TRUST
: AOT-DBASE@ dbase@ ;
s" AOT-DBASE@" s" -- ptr a" TRUST
: AOT-PTR@ {: a:ptr :} ( ptr a -- ptr a )
   a @ ;
s" AOT-PTR@" s" ptr a -- ptr a" TRUST
: AOT-IN   s" hb-aot-src" TMP-PATH ;
: AOT-OUT  s" hb-aot-got" TMP-PATH ;

: USER-HOOK
   CHECK!  dup -1 <> IF s" hb-build: check did not certify" 70 die THEN ;

: READ-PROG
   AOT-IN PATH0  0 0 open PFD !
   here PB !  PMAX allot  0 PN !
   BEGIN  PFD @  AOT-PB@ PN @ +  PMAX PN @ -  read PRD !  PRD @ 0 > WHILE  PN @ PRD @ + PN !  REPEAT
   PFD @ close
   PN @ 0 > 0= IF s" aot: empty source" 74 die THEN ;
: SENTSET  s"  AOT-LINK " {: sa:ptr su :}
   0 SI !
   BEGIN SI @ su < WHILE
      sa SI @ + c@  AOT-PB@ PN @ + SI @ + c!
      SI @ 1 + SI !
   REPEAT
   PN @ su + PN ! ;

\ --- read 32-bit words; recognize the compiled call (movz/movk/movk x16 + blr x16)
: W32@ {: a:ptr :} a c@  a 1+ c@ 8 lshift or  a 2 + c@ 16 lshift or  a 3 + c@ 24 lshift or ;
: W32! {: w a:ptr :} w a c!  w 8 rshift a 1+ c!  w 16 rshift a 2 + c!  w 24 rshift a 3 + c! ;
: TGT {: p:ptr :} p W32@ 5 rshift $FFFF and
   p 4 + W32@ 5 rshift $FFFF and 16 lshift or
   p 8 + W32@ 5 rshift $FFFF and 32 lshift or ;
: CALL? {: p:ptr :} p W32@ $FFE0001F and $D2800010 =
   p 4 + W32@ $FFE0001F and $F2A00010 = and
   p 8 + W32@ $FFE0001F and $F2C00010 = and
   p 12 + W32@ $D63F0200 = and ;
: CALL-AT? {: p:ptr e:ptr :}  p 16 + e <= IF p CALL? ELSE 0 0= 0= THEN ;

: REC {: k :} ( n -- ptr a )
   AOT-DBASE@ k 48 * + ;          \ dict record k  (0:addr 8:len 16:name-len|flags 24:name|ptr)
: AOT-FOLD {: c :}  c 64 > c 91 < and IF c 32 + ELSE c THEN ;
: REC-NAME-LEN {: r:ptr :} ( ptr a -- n )
   r 16 + @ DNAME-LEN-MASK and ;
: REC-NAME-PTR {: r:ptr :} ( ptr a -- ptr a )
   r 16 + @ DNAME-EXT and 0= IF r 24 + ELSE r 24 + AOT-PTR@ THEN ;
: REC-NAME@ {: r:ptr :} ( ptr a -- ptr a n )
   r REC-NAME-PTR  r REC-NAME-LEN ;
: REC-NAME-C@ {: r:ptr idx :} ( ptr a n -- n )
   r REC-NAME-PTR idx + c@ ;

: REC-NAME= {: r:ptr a:ptr u :} ( ptr a ptr u8 n -- bool )
   r REC-NAME-LEN u = IF
      0 BEGIN dup u < WHILE
         dup r swap REC-NAME-C@ AOT-FOLD
         over a + c@ AOT-FOLD = 0= IF drop 0 0= 0= EXIT THEN
         1 +
      REPEAT drop 0 0=
   ELSE 0 0= 0= THEN ;
: MAIN? {: r:ptr :} ( ptr a -- bool )
   r s" MAIN" REC-NAME= ;
: AOT-UNSAFE? {: r:ptr :} ( ptr a -- bool )
   r s" @" REC-NAME= IF 0 0= EXIT THEN
   r s" !" REC-NAME= IF 0 0= EXIT THEN
   r s" c@" REC-NAME= IF 0 0= EXIT THEN
   r s" c!" REC-NAME= IF 0 0= EXIT THEN
   r s" here" REC-NAME= IF 0 0= EXIT THEN
   r s" allot" REC-NAME= IF 0 0= EXIT THEN
   r s" ," REC-NAME= IF 0 0= EXIT THEN
   r s" c," REC-NAME= IF 0 0= EXIT THEN
   r s" create" REC-NAME= IF 0 0= EXIT THEN
   r s" compile," REC-NAME= IF 0 0= EXIT THEN
   r s" patch32" REC-NAME= IF 0 0= EXIT THEN
   0 0= 0= ;
create AECH 1 allot
: AE1 {: c :}  c AECH c!  2 AECH 1 write drop ;
: AETXT {: a:ptr u :} ( ptr u8 n -- )
   2 a u write drop ;
: AEREC-TXT {: r:ptr :} ( ptr a -- )
   r 0= IF s" <unknown>" AETXT ELSE r REC-NAME@ AETXT THEN ;
: AEJCHAR {: c :}
   c 10 = IF 92 AE1 110 AE1 EXIT THEN
   c 13 = IF 92 AE1 114 AE1 EXIT THEN
   c 9 = IF 92 AE1 116 AE1 EXIT THEN
   c 34 =  c 92 = or IF 92 AE1 THEN  c AE1 ;
: AEJSTR {: a:ptr u :} ( ptr u8 n -- )
   34 AE1  0 BEGIN dup u < WHILE dup a + c@ AEJCHAR 1 + REPEAT drop 34 AE1 ;
: AEJKEY {: a:ptr u :} ( ptr u8 n -- )
   a u AEJSTR 58 AE1 ;
: AEJREC {: r:ptr :} ( ptr a -- )
   r 0= IF s" <unknown>" AEJSTR ELSE r REC-NAME@ AEJSTR THEN ;
create AENB 20 allot  variable AENV  variable AENN
: AEJNUM
   AENV !  0 AENN !
   AENV @ 0= IF 48 AE1 EXIT THEN
   BEGIN AENV @ 0 > WHILE
      AENV @ 10 mod 48 +  AENB AENN @ + c!
      AENN @ 1 + AENN !
      AENV @ 10 / AENV !
   REPEAT
   AENN @ BEGIN dup 0 > WHILE 1 - dup AENB + c@ AE1 REPEAT drop ;
: AOT-UNSAFE-JSON {: caller:ptr callee:ptr :} ( ptr a ptr a -- )
   123 AE1
   s" schema_version" AEJKEY 1 AEJNUM 44 AE1
   s" code" AEJKEY s" E-AOT-UNSUPPORTED" AEJSTR 44 AE1
   s" verdict" AEJKEY s" rejected" AEJSTR 44 AE1
   s" word" AEJKEY caller REC-NAME@ AEJSTR 44 AE1
   s" token" AEJKEY callee REC-NAME@ AEJSTR 44 AE1
   s" reason" AEJKEY s" stripped AOT has no persistent data region" AEJSTR 44 AE1
   s" suggestion" AEJKEY
   s" stripped AOT has no persistent data region; use --repl/snapshot for data-space words or remove the runtime data access" AEJSTR
   125 AE1 10 AE1 ;
: AOT-UNSAFE-PROSE {: caller:ptr callee:ptr :} ( ptr a ptr a -- )
   s" hb-build: stripped AOT unsupported word '" AETXT
   callee REC-NAME@ AETXT
   s" ' called by '" AETXT
   caller REC-NAME@ AETXT
   s" '" AETXT 10 AE1 ;
: AOT-UNSAFE-DIE {: caller:ptr callee:ptr :} ( ptr a ptr a -- )
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
variable ROOTREC
variable CLO-LIMIT
: CLO-LIMIT! {: n :}
   n 1 < IF s" aot: CLO-LIMIT below 1" 74 die THEN
   n MAX-CLO > IF s" aot: CLO-LIMIT above MAX-CLO" 74 die THEN
   n CLO-LIMIT ! ;
MAX-CLO CLO-LIMIT!
: IN-CLO? {: r:ptr :} ( ptr a -- bool )
   0 CX ! BEGIN CX @ NCLO @ < WHILE CX @ cells CLO + @ r = IF 0 0= exit THEN CX @ 1+ CX ! REPEAT 0 0= 0= ;
: CALL-IN-CLO? {: p:ptr :} ( ptr u8 -- bool )
   p TGT FINDADDR dup 0= 0= IF IN-CLO? ELSE drop 0 0= 0= THEN ;
: CLO-OVERFLOW-JSON {: r:ptr :} ( ptr a -- )
   123 AE1
   s" schema_version" AEJKEY 1 AEJNUM 44 AE1
   s" code" AEJKEY s" E-AOT-CLOSURE-LIMIT" AEJSTR 44 AE1
   s" verdict" AEJKEY s" rejected" AEJSTR 44 AE1
   s" reachable_count" AEJKEY NCLO @ AEJNUM 44 AE1
   s" max_closure" AEJKEY CLO-LIMIT @ AEJNUM 44 AE1
   s" root_word" AEJKEY ROOTREC @ AEJREC 44 AE1
   s" last_added_word" AEJKEY r AEJREC 44 AE1
   s" suggestion" AEJKEY
   s" split program, use --repl/snapshot, or raise MAX-CLO with a gate that proves the larger closure" AEJSTR
   125 AE1 10 AE1 ;
: CLO-OVERFLOW-PROSE {: r:ptr :} ( ptr a -- )
   s" aot: closure exceeds MAX-CLO reachable_count=" AETXT NCLO @ AEJNUM
   s"  max_closure=" AETXT CLO-LIMIT @ AEJNUM
   s"  root_word='" AETXT ROOTREC @ AEREC-TXT
   s" ' last_added_word='" AETXT r AEREC-TXT
   s" ' suggestion='split program, use --repl/snapshot, or raise MAX-CLO with a gate that proves the larger closure'" AETXT
   10 AE1 ;
: CLO-OVERFLOW-DIE {: r:ptr :} ( ptr a -- )
   JSON-DIAGS @ IF r CLO-OVERFLOW-JSON ELSE r CLO-OVERFLOW-PROSE THEN
   s" aot: closure exceeds MAX-CLO" 74 die ;
: ADD-CLO {: r:ptr :} ( ptr a -- )
   r IN-CLO? IF exit THEN
   NCLO @ CLO-LIMIT @ >= IF r CLO-OVERFLOW-DIE THEN
   r NCLO @ cells CLO + !  NCLO @ 1+ NCLO ! ;
variable SP2  variable SEND
: SCAN-REC {: r:ptr :} ( ptr a -- )
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
: CLOSURE  0 NCLO !  FINDMAIN dup 0= IF drop s" aot: no MAIN" 74 die THEN  dup ROOTREC !  ADD-CLO
   0 WI ! BEGIN WI @ NCLO @ < WHILE  WI @ cells CLO + @ SCAN-REC  WI @ 1+ WI ! REPEAT ;

\ --- emit the image: minimal entry + compacted, relocated blobs.
variable MLBL  variable REC2
create OLDA MAX-CLO cells allot   create NEWOFF MAX-CLO cells allot   create BLEN MAX-CLO cells allot
: EMIT-ENTRY
   SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,
   SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,
   XDS SP 0 ADDI,
   MLBL @ BL,                              \ bl MAIN (resolved when MLBL is placed)
   0 0 MOVZ,  NR-EXIT SYS, ;               \ exit(0)
variable CP2  variable CEND  variable CLEN  variable NEXT-OFF
: BIMM? {: w :}  w $7C000000 and $14000000 = ;
: BCOND? {: w :}  w $FF000010 and $54000000 = ;
: CBZIMM? {: w :}  w $7E000000 and $34000000 = ;
: TBZIMM? {: w :}  w $7E000000 and $36000000 = ;
: ADR? {: w :}  w $9F000000 and $10000000 = ;
: ADRP? {: w :}  w $9F000000 and $90000000 = ;
: RAW-LEN {: r:ptr :} ( ptr a -- n )
   r 8 + @ 4 + ;
: REC-END {: r:ptr :} ( ptr a -- ptr u8 )
   r @ r RAW-LEN + ;
: COMPACT-LEN {: r:ptr :} ( ptr a -- n )
   0 CLEN !  r @ CP2 !  r @ r RAW-LEN + CEND !
   BEGIN CP2 @ CEND @ < WHILE
      CP2 @ CEND @ CALL-AT? IF
         CP2 @ CALL-IN-CLO? IF
            CLEN @ 4 + CLEN !  CP2 @ 16 + CP2 !
         ELSE
            CLEN @ 4 + CLEN !  CP2 @ 4 + CP2 !
         THEN
      ELSE
         CLEN @ 4 + CLEN !  CP2 @ 4 + CP2 !
      THEN
   REPEAT CLEN @ ;
: PLAN-BLOBS
   ASM-LEN NEXT-OFF !
   0 WI ! BEGIN WI @ NCLO @ < WHILE
      WI @ cells CLO + @ REC2 !
      REC2 @ @         OLDA   WI @ cells + !
      NEXT-OFF @       NEWOFF WI @ cells + !
      REC2 @ COMPACT-LEN dup BLEN WI @ cells + !
      NEXT-OFF @ + NEXT-OFF !
      WI @ 1+ WI ! REPEAT ;
\ code offset (in CODE) of the blob whose OLD addr is t, or -1. The binary is PIE
\ (arm64 macOS requires it), so absolute call targets would be wrong under the
\ ASLR slide — instead we rewrite each abs call to a PC-RELATIVE bl, whose offset
\ within __text is slide-independent. No runtime relocation needed.
: CLO-OFF {: t :}  0 CX !
   BEGIN CX @ NCLO @ < WHILE
      OLDA CX @ cells + @ t = IF  NEWOFF CX @ cells + @  exit THEN
      CX @ 1+ CX ! REPEAT  -1 ;
\ encode a compact PC-relative BL. The AOT __text is small today, but range
\ checking makes linker corruption fail at build time if that ever changes.
variable BDELTA  variable TNEW
: FIELD {: w lo width :}  w lo rshift  1 width lshift 1 - and ;
: SX {: f width :}  f 1 width 1 - lshift xor  1 width 1 - lshift - ;
: REL26 {: site target :}
   target site - BDELTA !
   BDELTA @ 3 and 0 <> IF s" aot: branch target not 4-byte aligned" 74 die THEN
   BDELTA @ 4 / BDELTA !
   BDELTA @ -33554432 <  BDELTA @ 33554431 > or IF s" aot: B/BL target out of range" 74 die THEN
   BDELTA @ $3FFFFFF and ;
: REL19 {: site target :}
   target site - BDELTA !
   BDELTA @ 3 and 0 <> IF s" aot: branch target not 4-byte aligned" 74 die THEN
   BDELTA @ 4 / BDELTA !
   BDELTA @ -262144 <  BDELTA @ 262143 > or IF s" aot: rel19 target out of range" 74 die THEN
   BDELTA @ $7FFFF and ;
: REL14 {: site target :}
   target site - BDELTA !
   BDELTA @ 3 and 0 <> IF s" aot: branch target not 4-byte aligned" 74 die THEN
   BDELTA @ 4 / BDELTA !
   BDELTA @ -8192 <  BDELTA @ 8191 > or IF s" aot: rel14 target out of range" 74 die THEN
   BDELTA @ $3FFF and ;
: BL32 {: site target :}  site target REL26 $94000000 or ;
: ADRD32 {: site target :}
   target site - BDELTA !
   BDELTA @ -1048576 <  BDELTA @ 1048575 > or IF s" aot: ADR target out of range" 74 die THEN
   BDELTA @ 3 and 29 lshift  BDELTA @ 2 rshift $7FFFF and 5 lshift or ;
\ replace the 4-instr `movz/movk/movk x16; blr x16` at CODE byte offset `site`
\ with `nop; nop; nop; bl target` (target = the callee's CODE offset).
: PATCH-BL {: site target :}
   $D503201F CODE site +     W32!
   $D503201F CODE site 4 + + W32!
   $D503201F CODE site 8 + + W32!
   site 12 + target BL32  CODE site 12 + + W32! ;

variable MAPOUT  variable MAPP  variable MAPE
: REC-NEWOFF {: r:ptr :} ( ptr a -- n )
   0 CX !
   BEGIN CX @ NCLO @ < WHILE
      CX @ cells CLO + @ r = IF NEWOFF CX @ cells + @ EXIT THEN
      CX @ 1+ CX ! REPEAT  -1 ;
: MAP-IN-BLOB {: r:ptr t:ptr :} ( ptr a ptr u8 -- n )
   t r @ < IF -1 EXIT THEN
   t r REC-END > IF -1 EXIT THEN
   0 MAPOUT !  r @ MAPP !  r REC-END MAPE !
   BEGIN MAPP @ MAPE @ < WHILE
      MAPP @ MAPE @ CALL-AT?  MAPP @ CALL-IN-CLO? and IF
         t MAPP @ = IF r REC-NEWOFF MAPOUT @ + EXIT THEN
         t MAPP @ 16 + < IF -1 EXIT THEN
         MAPOUT @ 4 + MAPOUT !  MAPP @ 16 + MAPP !
      ELSE
         t MAPP @ 4 + < IF r REC-NEWOFF MAPOUT @ +  t MAPP @ - + EXIT THEN
         MAPOUT @ 4 + MAPOUT !  MAPP @ 4 + MAPP !
      THEN
   REPEAT
   t MAPE @ = IF r REC-NEWOFF MAPOUT @ + ELSE -1 THEN ;
: OLD>NEW {: t:ptr :} ( ptr u8 -- n )
   0 CX !
   BEGIN CX @ NCLO @ < WHILE
      CX @ cells CLO + @ t MAP-IN-BLOB dup -1 <> IF EXIT THEN drop
      CX @ 1+ CX ! REPEAT  -1 ;
: MAP-TARGET {: r:ptr t:ptr :} ( ptr a ptr u8 -- n )
   r t MAP-IN-BLOB dup -1 <> IF EXIT THEN drop  t OLD>NEW ;
: MAP-TARGET! {: r:ptr t:ptr :} ( ptr a ptr u8 -- )
   r t MAP-TARGET TNEW !
   TNEW @ -1 = IF s" aot: PC-relative target removed or outside closure" 74 die THEN ;
: BTGT26 {: p:ptr w :} ( ptr u8 n -- ptr u8 )
   p  w 0 26 FIELD 26 SX 4 * + ;
: BTGT19 {: p:ptr w :} ( ptr u8 n -- ptr u8 )
   p  w 5 19 FIELD 19 SX 4 * + ;
: BTGT14 {: p:ptr w :} ( ptr u8 n -- ptr u8 )
   p  w 5 14 FIELD 14 SX 4 * + ;
: ADRTGT {: p:ptr w :} ( ptr u8 n -- ptr u8 )
   p  w 5 19 FIELD 2 lshift  w 29 2 FIELD or 21 SX + ;
: RELOC-W32 {: r:ptr p:ptr w :} ( ptr a ptr u8 n -- n )
   w BIMM? IF
      r p w BTGT26 MAP-TARGET!
      w $FC000000 and  ASM-LEN TNEW @ REL26 or EXIT THEN
   w BCOND? IF
      r p w BTGT19 MAP-TARGET!
      w $FF00001F and  ASM-LEN TNEW @ REL19 5 lshift or EXIT THEN
   w CBZIMM? IF
      r p w BTGT19 MAP-TARGET!
      w $FF00001F and  ASM-LEN TNEW @ REL19 5 lshift or EXIT THEN
   w TBZIMM? IF
      r p w BTGT14 MAP-TARGET!
      w $FFF8001F and  ASM-LEN TNEW @ REL14 5 lshift or EXIT THEN
   w ADR? IF
      r p w ADRTGT MAP-TARGET!
      w $9F00001F and  ASM-LEN TNEW @ ADRD32 or EXIT THEN
   w ADRP? IF s" aot: ADRP relocation unsupported" 74 die THEN
   w ;

variable DENSE-RV
: COPY-COMPACT-BLOB {: r:ptr :} ( ptr a -- )
   r @ CP2 !  r @ r RAW-LEN + CEND !
   BEGIN CP2 @ CEND @ < WHILE
      CP2 @ CEND @ CALL-AT? IF
         CP2 @ TGT CLO-OFF DENSE-RV !
         DENSE-RV @ -1 <> IF
            ASM-LEN DENSE-RV @ BL32 EMITW  CP2 @ 16 + CP2 !
         ELSE
            r CP2 @ CP2 @ W32@ RELOC-W32 EMITW  CP2 @ 4 + CP2 !
         THEN
      ELSE
         r CP2 @ CP2 @ W32@ RELOC-W32 EMITW  CP2 @ 4 + CP2 !
      THEN
   REPEAT ;
: COPY-PLANNED-BLOBS
   0 WI ! BEGIN WI @ NCLO @ < WHILE
      WI @ cells CLO + @ REC2 !
      WI @ 0= IF MLBL @ LBL, THEN          \ MAIN is closure word 0 -> place its label
      REC2 @ COPY-COMPACT-BLOB
      WI @ 1+ WI ! REPEAT ;
: COPY-BLOBS  PLAN-BLOBS  COPY-PLANNED-BLOBS ;
variable RP  variable RE  variable RV
: RELOCATE
   0 WI ! BEGIN WI @ NCLO @ < WHILE
      NEWOFF WI @ cells + @ RP !
      RP @ BLEN WI @ cells + @ + RE !
      BEGIN RP @ RE @ < WHILE
         CODE RP @ + CODE RE @ + CALL-AT? IF
            CODE RP @ + TGT CLO-OFF RV !
            RV @ -1 <> IF RP @ RV @ PATCH-BL THEN
            RP @ 16 + RP !
         ELSE RP @ 4 + RP ! THEN
      REPEAT
      WI @ 1+ WI ! REPEAT ;

: AOT-LINK
   CLOSURE  ASM-INIT  LBL MLBL !
   EMIT-ENTRY  COPY-BLOBS  RELOCATE
   ASM-CODE  BUILD-IMAGE  s" hb-prog" SET-SIGID  CODESIG2
   AOT-OUT PATH0  1537 493 open  dup MBUF MLEN @ write drop  close ;

: GO  READ-PROG  SENTSET
   ['] USER-HOOK set-check
   AOT-PB@ DATA-VA INP-CELL + !
   AOT-PB@ PN @ + DATA-VA INE-CELL + ! ;
GO

\ aot-lib.f - stripped AOT linker words. Load after src/habu/aot-closure.f.
\
\ The MAKER compiles the program in-process (we point
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
variable AOT-SI
$40000 constant PMAX
: AOT-PB@ PB @ ;
s" AOT-PB@" s" -- ptr u8" TRUST
: AOT-IN   s" hb-aot-src" TMP-PATH ;
: AOT-OUT  s" hb-aot-got" TMP-PATH ;

: AOT-FALSE ( -- bool ) 0 0= 0= ;
: AOT-JSON-ARG? ( -- bool )
   ARGC 2 <= IF AOT-FALSE EXIT THEN
   2 ARGV$ dup 1 = IF drop c@ 49 = ELSE 2drop AOT-FALSE THEN ;
: AOT-RUNTIME-ARGS ( -- )
   ARGC 1 > IF 1 ARGV$ DIAG-FILE! THEN
   AOT-JSON-ARG? IF -1 JSON-DIAGS ! THEN ;

: USER-HOOK
   CHECK!  dup -1 <> IF s" hb-build: check did not certify" 70 die THEN ;

: READ-PROG
   AOT-IN PATH0  0 0 open PFD !
   PFD @ 0 < IF s" aot: cannot open source" 74 die THEN
   here PB !  PMAX allot  0 PN !
   BEGIN
      PFD @  AOT-PB@ PN @ +  PMAX PN @ -  read PRD !
      PRD @ 0 >
   WHILE
      PN @ PRD @ + PN !
   REPEAT
   PFD @ close
   PRD @ 0 < IF s" aot: source read failed" 74 die THEN
   PN @ 0 > 0= IF s" aot: empty source" 74 die THEN
   PN @ PMAX = IF s" aot: source exceeds buffer" 74 die THEN ;

: SENT-ROOM ( n -- )
   PN @ + PMAX > IF s" aot: source exceeds buffer" 74 die THEN ;

: SENTSET  s"  AOT-LINK " {: sa:ptr su:n :}
   su SENT-ROOM
   0 AOT-SI !
   BEGIN AOT-SI @ su < WHILE
      sa AOT-SI @ + c@  AOT-PB@ PN @ + AOT-SI @ + c!
      AOT-SI @ 1 + AOT-SI !
   REPEAT
   PN @ su + PN ! ;

\ --- emit the image: minimal entry + compacted, relocated blobs.
variable MLBL  variable REC2
create OLDA MAX-CLO cells allot   create NEWOFF MAX-CLO cells allot   create BLEN MAX-CLO cells allot

: AOT-W32! ( n ptr u8 -- ) {: w:n a:ptr :}
   w a c!  w 8 rshift a 1+ c!  w 16 rshift a 2 + c!  w 24 rshift a 3 + c! ;

\ --- persistent data region: the program's compile-time create/variable/allot/
\ ,/s" data lives contiguously in the maker's fixed DATA region at
\ [PB+PMAX, here) (the source buffer occupies [PB,PB+PMAX)); the assembler CODE
\ buffer is a separate mmap so AOT-LINK never allots into DATA. We emit that span
\ into __text and the entry maps DATA-VA and copies it back to the SAME absolute
\ VA (DATA-VA is a fixed MAP_FIXED VA, so those addresses are load-stable). All
\ other runtime cells stay zero from the fresh anonymous mmap; only x20, S0-CELL,
\ and DP-CELL need explicit init.
variable BLOB-SRC  variable BLOB-END  variable BLOB-LEN  variable BLOB-LBL
variable DSCAN
$F0000 constant AOT-DATA-BLOB-MAX          \ keep the blob within ADR ±1MB range

: AOT-DATA-SPAN ( -- )
   PB @ PMAX +  BLOB-SRC !
   here  BLOB-END !
   BLOB-END @ BLOB-SRC @ - dup 0 < IF s" aot: negative data span" 74 die THEN BLOB-LEN ! ;

: CELL-TEXTPTR? ( n -- bool )               \ true if a code/dict pointer (RBASE-VA window)
   dup RBASE-VA >=  swap RBASE-VA REGION + < and ;
: AOT-DATA-TEXTPTR? ( -- bool )
   BLOB-SRC @ DSCAN !
   BEGIN DSCAN @ 8 + BLOB-END @ <= WHILE
      DSCAN @ @ CELL-TEXTPTR? IF 0 0= EXIT THEN
      DSCAN @ 8 + DSCAN !
   REPEAT  0 0= 0= ;
: AOT-DATA-TEXTPTR-JSON ( -- )
   123 AE1
   s" schema_version" AEJKEY 1 AEJNUM 44 AE1
   s" code" AEJKEY s" E-AOT-UNSUPPORTED" AEJSTR 44 AE1
   s" verdict" AEJKEY s" rejected" AEJSTR 44 AE1
   s" reason" AEJKEY s" stripped AOT persistent data holds a code/dict pointer" AEJSTR 44 AE1
   s" suggestion" AEJKEY
   s" stripped AOT cannot rebase code/dict pointers in data (defer or ' word ,); use --repl or remove the code pointer from data" AEJSTR
   125 AE1 10 AE1 ;
: AOT-DATA-TEXTPTR-DIE ( -- )
   JSON-DIAGS @ IF AOT-DATA-TEXTPTR-JSON ELSE
      s" hb-build: stripped AOT persistent data holds a code/dict pointer (defer or ' word ,)" AETXT 10 AE1
   THEN
   s" hb-build: AOT unsupported persistent data" 70 die ;

: EMIT-DATA-REGION-MAP ( -- )
   LBL {: dvok:label :}
   0 DATA-VA LIT64,  1 DATA-SIZE LIT64,  2 3 MOVZ,  3 MAP-ANON-PRIVATE-FIXED LIT64,  4 0 MOVN,  5 0 MOVZ,
   NR-MMAP SYS,
   5 DATA-VA LIT64,  0 5 CMP,
   C-EQ dvok BCOND,
      0 78 MOVZ,  NR-EXIT-GROUP SYS,
   dvok LBL,
   DATA 0 0 ADDI,                                \ x20 = DATA-VA (mmap result, verified)
   XDS DATA S0-CELL STR, ;                        \ S0-CELL = value-stack base

: EMIT-DATA-COPY ( -- )
   BLOB-LEN @ 0= IF
      7 BLOB-END @ LIT64,  7 DATA DP-CELL STR,  exit         \ DP = data base (no user data)
   THEN
   9 BLOB-LBL LABEL@ ADR,                         \ x9 = blob src in __text
   10 BLOB-SRC @ LIT64,                           \ x10 = dst (absolute stable VA)
   11 BLOB-LEN @ LIT64,                           \ x11 = byte count
   12 0 MOVZ,                                     \ x12 = i
   LBL LBL {: cp:label cpd:label :}
   cp LBL,
      12 11 CMP,  C-GE cpd BCOND,
      13 9 12 ADD,  13 13 0 LDRB,
      14 10 12 ADD,  13 14 0 STRB,
      12 12 1 ADDI,  cp B,
   cpd LBL,
   7 BLOB-END @ LIT64,  7 DATA DP-CELL STR, ;      \ DP = user-end (runtime here/allot base)

: EMIT-DATA-BLOB ( -- )                            \ place the blob after all code
   BLOB-LEN @ 0= IF exit THEN
   ASM-LEN AOT-DATA-BLOB-MAX > IF
      s" aot: data blob too far for ADR (program too large); split program" 74 die THEN
   BLOB-LBL LABEL@ LBL,
   BLOB-SRC @ BLOB-LEN @ BYTES, ;

: EMIT-ENTRY
   SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,
   SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,
   XDS SP 0 ADDI,
   EMIT-DATA-REGION-MAP                          \ map DATA-VA, set x20/S0
   EMIT-DATA-COPY                                \ restore persistent data + DP
   MLBL LABEL@ BL,                              \ bl MAIN (resolved when MLBL is placed)
   0 0 MOVZ,  NR-EXIT-GROUP SYS, ;               \ exit(0)
variable CP2  variable CEND  variable CLEN  variable NEXT-OFF
: BIMM? {: w:n :}  w $7C000000 and $14000000 = ;
: BCOND? {: w:n :}  w $FF000010 and $54000000 = ;
: CBZIMM? {: w:n :}  w $7E000000 and $34000000 = ;
: TBZIMM? {: w:n :}  w $7E000000 and $36000000 = ;
: ADR? {: w:n :}  w $9F000000 and $10000000 = ;
: ADRP? {: w:n :}  w $9F000000 and $90000000 = ;
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
: CLO-OFF {: t:ptr :}  0 CLO-CX !
   BEGIN CLO-CX @ NCLO @ < WHILE
      OLDA CLO-CX @ cells + @ t = IF  NEWOFF CLO-CX @ cells + @  exit THEN
      CLO-CX @ 1+ CLO-CX ! REPEAT  -1 ;
\ encode a compact PC-relative BL. The AOT __text is small today, but range
\ checking makes linker corruption fail at build time if that ever changes.
variable BDELTA  variable TNEW
: FIELD {: w:n lo:n width:n :}  w lo rshift  1 width lshift 1 - and ;
: SX {: f:n width:n :}  f 1 width 1 - lshift xor  1 width 1 - lshift - ;
: REL26 {: site:n target:n :}
   target site - BDELTA !
   BDELTA @ 3 and 0 <> IF s" aot: branch target not 4-byte aligned" 74 die THEN
   BDELTA @ 4 / BDELTA !
   BDELTA @ -33554432 <  BDELTA @ 33554431 > or IF s" aot: B/BL target out of range" 74 die THEN
   BDELTA @ $3FFFFFF and ;
: REL19 {: site:n target:n :}
   target site - BDELTA !
   BDELTA @ 3 and 0 <> IF s" aot: branch target not 4-byte aligned" 74 die THEN
   BDELTA @ 4 / BDELTA !
   BDELTA @ -262144 <  BDELTA @ 262143 > or IF s" aot: rel19 target out of range" 74 die THEN
   BDELTA @ $7FFFF and ;
: REL14 {: site:n target:n :}
   target site - BDELTA !
   BDELTA @ 3 and 0 <> IF s" aot: branch target not 4-byte aligned" 74 die THEN
   BDELTA @ 4 / BDELTA !
   BDELTA @ -8192 <  BDELTA @ 8191 > or IF s" aot: rel14 target out of range" 74 die THEN
   BDELTA @ $3FFF and ;
: BL32 {: site:n target:n :}  site target REL26 $94000000 or ;
: ADRD32 {: site:n target:n :}
   target site - BDELTA !
   BDELTA @ -1048576 <  BDELTA @ 1048575 > or IF s" aot: ADR target out of range" 74 die THEN
   BDELTA @ 3 and 29 lshift  BDELTA @ 2 rshift $7FFFF and 5 lshift or ;
\ replace the 4-instr `movz/movk/movk x16; blr x16` at CODE byte offset `site`
\ with `nop; nop; nop; bl target` (target = the callee's CODE offset).
: PATCH-BL {: site:n target:n :}
   $D503201F CODE site +     AOT-W32!
   $D503201F CODE site 4 + + AOT-W32!
   $D503201F CODE site 8 + + AOT-W32!
   site 12 + target BL32  CODE site 12 + + AOT-W32! ;

variable MAPOUT  variable MAPP  variable MAPE
: REC-NEWOFF {: r:ptr :} ( ptr a -- n )
   0 CLO-CX !
   BEGIN CLO-CX @ NCLO @ < WHILE
      CLO-CX @ cells CLO + @ r = IF NEWOFF CLO-CX @ cells + @ EXIT THEN
      CLO-CX @ 1+ CLO-CX ! REPEAT  -1 ;
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
   0 CLO-CX !
   BEGIN CLO-CX @ NCLO @ < WHILE
      CLO-CX @ cells CLO + @ t MAP-IN-BLOB dup -1 <> IF EXIT THEN drop
      CLO-CX @ 1+ CLO-CX ! REPEAT  -1 ;
: MAP-TARGET {: r:ptr t:ptr :} ( ptr a ptr u8 -- n )
   r t MAP-IN-BLOB dup -1 <> IF EXIT THEN drop  t OLD>NEW ;
: MAP-TARGET! {: r:ptr t:ptr :} ( ptr a ptr u8 -- )
   r t MAP-TARGET TNEW !
   TNEW @ -1 = IF s" aot: PC-relative target removed or outside closure" 74 die THEN ;
: BTGT26 {: p:ptr w:n :} ( ptr u8 n -- ptr u8 )
   p  w 0 26 FIELD 26 SX 4 * + ;
: BTGT19 {: p:ptr w:n :} ( ptr u8 n -- ptr u8 )
   p  w 5 19 FIELD 19 SX 4 * + ;
: BTGT14 {: p:ptr w:n :} ( ptr u8 n -- ptr u8 )
   p  w 5 14 FIELD 14 SX 4 * + ;
: ADRTGT {: p:ptr w:n :} ( ptr u8 n -- ptr u8 )
   p  w 5 19 FIELD 2 lshift  w 29 2 FIELD or 21 SX + ;
: RELOC-W32 {: r:ptr p:ptr w:n :} ( ptr a ptr u8 n -- n )
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
            r CP2 @ CP2 @ AOT-W32@ RELOC-W32 EMITW  CP2 @ 4 + CP2 !
         THEN
      ELSE
         r CP2 @ CP2 @ AOT-W32@ RELOC-W32 EMITW  CP2 @ 4 + CP2 !
      THEN
   REPEAT ;
: COPY-PLANNED-BLOBS
   0 WI ! BEGIN WI @ NCLO @ < WHILE
      WI @ cells CLO + @ REC2 !
      WI @ 0= IF MLBL LABEL@ LBL, THEN          \ MAIN is closure word 0 -> place its label
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
   AOT-DATA-SPAN
   AOT-DATA-TEXTPTR? IF AOT-DATA-TEXTPTR-DIE THEN
   CLOSURE  ASM-INIT  LBL MLBL !  LBL BLOB-LBL !
   EMIT-ENTRY  COPY-BLOBS  RELOCATE  EMIT-DATA-BLOB
   s" hb-prog" AOT-OUT DRV-EMIT-IMAGE ;

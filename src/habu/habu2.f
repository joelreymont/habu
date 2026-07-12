\ habu2.f — engine-builder part 2: the JIT compiler
\ emitters (literal/call/keywords/locals/strings/do-loop), the outer-interpreter
\ main loop, and EMIT-FORTH. Needs habu1.f (part 1). EMIT-MAIN is split into
\ phase words sharing label VARIABLES (a giant single word would need dozens of
\ locals); emission order is stable so the self-rebuild reaches a fixpoint.
\ ---- compile-mode literal: emit movz/movk x9=val then the push stencil ----
: C-LIT ( -- )
   6 11 0 ADDI,  5 $FFFF MOVZ,
   7 6 5 AND,    7 7 5 LSLI,  8 W-MOVZ0 LIT64,  9 8 7 ORR,  LCEMIT LABEL@ BL,
   7 6 16 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK1 LIT64,  9 8 7 ORR,  LCEMIT LABEL@ BL,
   7 6 32 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK2 LIT64,  9 8 7 ORR,  LCEMIT LABEL@ BL,
   7 6 48 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK3 LIT64,  9 8 7 ORR,  LCEMIT LABEL@ BL,
   9 W-PUSH0 LIT64,  LCEMIT LABEL@ BL,  9 W-PUSH1 LIT64,  LCEMIT LABEL@ BL, ;
\ compile-mode raw literal materialization: emit movz/movk x9=val.  `val` is in
\ the compiler's x11 at definition time; unlike C-LIT this does not push it.
: C-X9-LIT ( -- )
   6 11 0 ADDI,  5 $FFFF MOVZ,
   7 6 5 AND,    7 7 5 LSLI,  8 W-MOVZ0 LIT64,  9 8 7 ORR,  LCEMIT LABEL@ BL,
   7 6 16 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK1 LIT64,  9 8 7 ORR,  LCEMIT LABEL@ BL,
   7 6 32 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK2 LIT64,  9 8 7 ORR,  LCEMIT LABEL@ BL,
   7 6 48 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK3 LIT64,  9 8 7 ORR,  LCEMIT LABEL@ BL, ;
\ ---- compile-mode CALL-or-INLINE (x11=target addr, x12=clen from FIND) ----
$28 constant INL-MAX
$D10043FF constant C-CALL-PROLOGUE-INSTR
$D65F03C0 constant C-CALL-RET-INSTR
$FC000000 constant C-CALL-B-IMM-MASK
$94000000 constant C-CALL-BL-IMM
$14000000 constant C-CALL-B-IMM
$FF000010 constant C-CALL-B-COND-MASK
$54000000 constant C-CALL-B-COND
$7E000000 constant C-CALL-CBZ-TBZ-MASK
$34000000 constant C-CALL-CBZ
$36000000 constant C-CALL-TBZ
$FFFFFC1F constant C-CALL-BR-MASK
$D63F0000 constant C-CALL-BLR
$D61F0000 constant C-CALL-BR
$1F000000 constant C-CALL-ADR-MASK
$10000000 constant C-CALL-ADR
$D2800010 constant C-CALL-MOVZ-X16
$F2A00010 constant C-CALL-MOVK-X16-16
$F2C00010 constant C-CALL-MOVK-X16-32
$D63F0200 constant C-CALL-BLR-X16

: C-CALL-BRANCH-NO-PROLOGUE ( label -- ) {: lnopro:label :}
   9 11 0 LDRW,  8 C-CALL-PROLOGUE-INSTR LIT64,
   9 8 CMP,  C-NE lnopro BCOND, ;

: C-CALL-PROLOGUE-SPAN ( label -- ) {: lcall:label :}
   12 INL-MAX 16 + CMPI,  C-GT lcall BCOND,
   13 11 8 ADDI,  14 11 12 ADD,  14 14 8 SUBI, ;

: C-CALL-REQUIRE-RET-SLOT ( label -- ) {: lcall:label :}
   9 14 0 LDRW,  8 C-CALL-RET-INSTR LIT64,
   9 8 CMP,  C-NE lcall BCOND, ;

: C-CALL-PLAIN-SPAN ( label -- ) {: lcall:label :}
   12 INL-MAX CMPI,  C-GT lcall BCOND,
   13 11 0 ADDI,  14 11 12 ADD,
   lcall C-CALL-REQUIRE-RET-SLOT ;   \ ret slot patched (does>) -> never inline

: C-CALL-REJECT-MASKED ( n n label -- ) {: mask:n op:n lcall:label :}
   8 mask LIT64,  10 9 8 AND,
   8 op LIT64,  10 8 CMP,  C-EQ lcall BCOND, ;

: C-CALL-REJECT-EXACT ( n label -- ) {: op:n lcall:label :}
   8 op LIT64,  9 8 CMP,  C-EQ lcall BCOND, ;

: C-CALL-REJECT-UNSAFE ( label -- ) {: lcall:label :}
   C-CALL-B-IMM-MASK C-CALL-BL-IMM lcall C-CALL-REJECT-MASKED
   C-CALL-B-IMM-MASK C-CALL-B-IMM lcall C-CALL-REJECT-MASKED
   C-CALL-B-COND-MASK C-CALL-B-COND lcall C-CALL-REJECT-MASKED
   C-CALL-CBZ-TBZ-MASK C-CALL-CBZ lcall C-CALL-REJECT-MASKED
   C-CALL-CBZ-TBZ-MASK C-CALL-TBZ lcall C-CALL-REJECT-MASKED
   C-CALL-BR-MASK C-CALL-BLR lcall C-CALL-REJECT-MASKED
   C-CALL-BR-MASK C-CALL-BR lcall C-CALL-REJECT-MASKED
   C-CALL-RET-INSTR lcall C-CALL-REJECT-EXACT
   C-CALL-ADR-MASK C-CALL-ADR lcall C-CALL-REJECT-MASKED ;

: C-CALL-SCAN-SAFE ( label label label -- ) {: lcopy:label lcall:label lsbody:label :}
   15 13 0 ADDI,
   lsbody LBL,  15 14 CMP,  C-GE lcopy BCOND,
      9 15 0 LDRW,  15 15 4 ADDI,
      lcall C-CALL-REJECT-UNSAFE
      lsbody B, ;

: C-CALL-COPY-INLINE ( label label -- ) {: linl:label ldone:label :}
   15 13 0 ADDI,
   linl LBL,  15 14 CMP,  C-GE ldone BCOND,
      9 15 0 LDRW,  15 15 4 ADDI,  LCEMIT LABEL@ BL,  linl B, ;

: C-CALL-EMIT-MOVZ-X16 ( -- )
   5 $FFFF MOVZ,
   7 11 5 AND,  7 7 5 LSLI,
   8 C-CALL-MOVZ-X16 LIT64,  9 8 7 ORR,  LCEMIT LABEL@ BL, ;

: C-CALL-EMIT-MOVK-X16 ( n n -- ) {: sh op :}
   7 11 sh LSRI,  7 7 5 AND,  7 7 5 LSLI,
   8 op LIT64,  9 8 7 ORR,  LCEMIT LABEL@ BL, ;

: C-CALL-EMIT-ABSOLUTE ( -- )
   C-CALL-EMIT-MOVZ-X16
   16 C-CALL-MOVK-X16-16 C-CALL-EMIT-MOVK-X16
   32 C-CALL-MOVK-X16-32 C-CALL-EMIT-MOVK-X16
   9 C-CALL-BLR-X16 LIT64,  LCEMIT LABEL@ BL, ;

: C-CALL ( -- )
   LBL LBL LBL LBL LBL LBL LBL {: lcall lcopy lscan lsbody lnopro linl ldone :}
   lnopro C-CALL-BRANCH-NO-PROLOGUE
      lcall C-CALL-PROLOGUE-SPAN
      lscan B,
   lnopro LBL,
      lcall C-CALL-PLAIN-SPAN
   lscan LBL,
      lcopy lcall lsbody C-CALL-SCAN-SAFE
   lcopy LBL,
      linl ldone C-CALL-COPY-INLINE
   lcall LBL,
      C-CALL-EMIT-ABSOLUTE
   ldone LBL, ;

\ ---- source setup: baked LSRC or stdin ----
variable LTRAPH   variable LBPH   variable LBPSH   variable LBPWH   variable LBADLOC
variable LSRCRD   variable LSHBANG   variable LOPENERR   variable LOPENNL
variable LUNCAUGHT   variable LUNCMSG   \ uncaught-top-level-throw reporter + its fd-2 message
variable LUNCRPT   variable LUNCPOS   variable LUNCLOOP   variable LUNCDONE   \ reporter branch + itoa labels
24 constant UNCMSG-LEN   \ byte length of "hb: uncaught throw code " (LUNCMSG)
variable LWIDE   variable LWIDEMSG   variable LDIAGRET   \ interpret-mode wide-effect reject + its message + shared recovery tail
33 constant WIDEMSG-LEN   \ byte length of "hb: interpret-mode layout value: " (LWIDEMSG)
variable LINTERNAL   variable LINTMSG   \ interpret-mode internal-word reject (DNAME-INT) + its message
26 constant INTMSG-LEN    \ byte length of "hb: internal engine word: " (LINTMSG)
31 constant CONFMSG-LEN   \ byte length of "hb: construct: unknown family: " (EM-COMPILE-ADT-MODE)
32 constant CONVMSG-LEN   \ byte length of "hb: construct: unknown variant: " (EM-COMPILE-ADT-MODE)
27 constant MFAMMSG-LEN   \ byte length of "hb: match: unknown family: " (EM-ADT-MATCH-FAM)
28 constant MVARMSG-LEN   \ byte length of "hb: match: unknown variant: " (EM-ADT-MATCH-VAR)
24 constant MOFMSG-LEN    \ byte length of "hb: match: expected of: " (EM-ADT-MATCH-OF)
variable LDICTFULL   variable LCODEFULL   \ definer capacity-exit labels (dict-record / code-region full)
24 constant CAPMSG-LEN    \ byte length of "hb: dictionary full at: " and "hb: code space full at: "
variable LSNAPBAD   variable LSNAPVER   \ snapshot-loader labeled-exit messages (corrupt trailer 79 / unsupported version 80)
29 constant SNAPBAD-MSG-LEN   \ byte length of "hb: snapshot trailer corrupt\n" (LSNAPBAD)
40 constant SNAPVER-MSG-LEN   \ byte length of "hb: snapshot format version unsupported\n" (LSNAPVER)
variable LSRCFULL   variable LSRCREAD   variable LBADSTR   \ boot source labeled rc-74 exits (prefix overflow / read error / string literal)
30 constant SRCFULL-MSG-LEN   \ byte length of "hb: source prefix buffer full\n" (LSRCFULL; SRC-SFAIL/SRC-BFAIL IBUFSZ overflow)
23 constant SRCREAD-MSG-LEN   \ byte length of "hb: cannot read source\n" (LSRCREAD; source read syscall error)
23 constant BADSTR-MSG-LEN    \ byte length of "hb: bad string literal\n" (LBADSTR; unterminated or bad-escape string literal)
variable LPROTPUB   variable LPROTAOT   \ protected-WID seal labeled rc-84 exits (publish-into-protected / AOT boot-pass gate reject)
40 constant PROTPUB-MSG-LEN   \ byte length of "hb: cannot publish into protected word: " (LPROTPUB; C-STORE-DEF-NAME appends the def name + newline)
34 constant PROTAOT-MSG-LEN   \ byte length of "hb: AOT protected-WID gate reject\n" (LPROTAOT; EM-AOTWIDGATE boot-pass reject)
variable LFLAGMATCH  variable LSRCBADFLAG  variable LFLAGTAB
variable LBADFLAG    variable LUSAGE1      variable LUSAGE2     variable LSPC
variable LPLINUXTARGET  variable LPMACOSTARGET
variable LPLINUXLAYOUT  variable LPMACOSLAYOUT
variable LPUTIL         variable LPSTRUCTURES   variable LPBYTES        variable LPCHECKER      variable LPRENDER
variable LPLOWERCERTBASE
variable LPTYPESCHEMA   variable LPTYPEFAM      variable LPSUMTYPE      variable LPLAYOUTBUF  variable LPLAYOUTVALID
variable LPHOOK         variable LPSTRUCTEFF    variable LPHABULAYOUT   variable LPENVBASE      variable LPINCLUDE
variable LPSCRIPTARGV   variable LPINTMARK
variable LPROLES
variable LPENUMS        variable LPEXECVECTOR   variable LPSHA256       variable LPTFAMSHA
variable LPCOMBINATORS  variable LPXREF  variable LPLAYOUTSEAL  variable LPLOWERCERTSEAL
create BPH-KW 104 c, 97 c, 98 c, 117 c, 45 c, 98 c, 112 c, 58 c, 10 c,   \ habu-bp:\n
create BPS-KW 104 c, 97 c, 98 c, 117 c, 45 c, 98 c, 112 c, 45 c, 115 c, 116 c, 97 c, 99 c, 107 c, 58 c, 10 c,
create BPW-KW 104 c, 97 c, 98 c, 117 c, 45 c, 98 c, 112 c, 45 c, 119 c, 97 c, 116 c, 99 c, 104 c, 58 c, 10 c,
\ "habu: local cannot be inside quotation\n" ($27 bytes)
create BADLOC-KW $68 c, $61 c, $62 c, $75 c, $3A c, $20 c, $6C c, $6F c, $63 c, $61 c, $6C c, $20 c, $63 c, $61 c, $6E c, $6E c, $6F c, $74 c, $20 c, $62 c, $65 c, $20 c, $69 c, $6E c, $73 c, $69 c, $64 c, $65 c, $20 c, $71 c, $75 c, $6F c, $74 c, $61 c, $74 c, $69 c, $6F c, $6E c, $0A c,
create ZBYTE 0 c,
create NL-KW 10 c,   \ single newline for the open-failure diagnostic

\ ---- CLI flag table: one source of truth for the matcher and the usage line ----
1 constant MODE-LOAD   2 constant MODE-SEP   3 constant MODE-FILE
4 constant MODE-UNKNOWN   5 constant MODE-BUILD
create FLAGTAB-DATA                        \ record = len, mode, name-bytes; 0-len terminator
   6 c, MODE-LOAD c,  45 c, 45 c, 108 c, 111 c, 97 c, 100 c,   \ "--load" -> MODE-LOAD
   7 c, MODE-BUILD c, 45 c, 45 c, 98 c, 117 c, 105 c, 108 c, 100 c, \ "--build" -> MODE-BUILD
   2 c, MODE-SEP  c,  45 c, 45 c,                              \ "--"     -> MODE-SEP
   0 c,                                                        \ terminator
22 constant FLAGTAB-LEN
create ONESP 32 c,   \ one space, written between usage flag names

: ZBYTES, ( ptr u8 n -- )
   BYTES, ZBYTE 1 BYTES, ;

: C-TRAP-MCTX>R9 ( -- )
   HB-TARGET-LINUX? IF 9 2 LINUX-UC-MCTX-OFF ADDI, exit THEN
   9 4 MCTX-OFF LDR, ;
s" c-trap-mctx>r9" s" --" TRUST

: C-MCTX-PC>R10 ( -- )
   HB-TARGET-LINUX? IF 10 9 LINUX-MCTX-PC-OFF LDR, exit THEN
   10 9 MACOS-MCTX-PC-OFF LDR, ;
s" c-mctx-pc>r10" s" --" TRUST

: C-MCTX-X19>R12 ( -- )
   HB-TARGET-LINUX? IF 12 9 LINUX-MCTX-X19-OFF LDR, exit THEN
   12 9 MACOS-MCTX-X19-OFF LDR, ;
s" c-mctx-x19>r12" s" --" TRUST

: C-MCTX-SP-16! ( -- )
   HB-TARGET-LINUX? IF
      12 9 LINUX-MCTX-SP-OFF LDR,  12 12 16 SUBI,  12 9 LINUX-MCTX-SP-OFF STR, exit
   THEN
   12 9 MACOS-MCTX-SP-OFF LDR,  12 12 16 SUBI,  12 9 MACOS-MCTX-SP-OFF STR, ;
s" c-mctx-sp-16!" s" --" TRUST

: C-MCTX-PC+4! ( -- )
   HB-TARGET-LINUX? IF
      12 9 LINUX-MCTX-PC-OFF LDR,  12 12 4 ADDI,  12 9 LINUX-MCTX-PC-OFF STR, exit
   THEN
   12 9 MACOS-MCTX-PC-OFF LDR,  12 12 4 ADDI,  12 9 MACOS-MCTX-PC-OFF STR, ;
s" c-mctx-pc+4!" s" --" TRUST

: C-BP-HIT-SAVE ( -- )
   SP SP 80 SUBI,
   1 SP 0 STR,  4 SP 8 STR,  5 SP 16 STR,
   9 SP 24 STR,  10 SP 32 STR,  8 SP 40 STR,
   14 8 16 LDR,  14 14 1 ADDI,  14 8 16 STR,
   15 8 24 LDR,  12 15 1 LSRI, ;
s" c-bp-hit-save" s" --" TRUST

: C-BP-PRINT-HIT ( -- )
   1 LBPH LABEL@ ADR,  0 2 MOVZ,  2 9 MOVZ,  NR-WRITE SYS,
   9 SP 32 LDR,  LHEX LABEL@ BL,
   9 SP 24 LDR,  C-MCTX-X19>R12
   9 12 8 SUBI,  9 9 0 LDR,  LHEX LABEL@ BL,
   1 LBPSH LABEL@ ADR,  0 2 MOVZ,  2 15 MOVZ,  NR-WRITE SYS, ;
s" c-bp-print-hit" s" --" TRUST

: C-BP-STACK-RANGE ( -- )
   17 DATA S0-CELL LDR,
   9 SP 24 LDR,  C-MCTX-X19>R12
   12 SP 56 STR, ;
s" c-bp-stack-range" s" --" TRUST

: C-BP-WATCH-HEAD ( -- )
   1 LBPWH LABEL@ ADR,  0 2 MOVZ,  2 15 MOVZ,  NR-WRITE SYS,
   6 DATA BPWN-CELL LDR,  7 DATA BPWBASE-CELL LDR,
   17 0 MOVZ, ;
s" c-bp-watch-head" s" --" TRUST

: C-BP-WATCH-ROW ( -- )
   22 17 3 LSLI,  22 7 22 ADD,  23 22 0 LDR,
   9 23 0 ADDI,  LHEX LABEL@ BL,
   9 23 0 LDR,  LHEX LABEL@ BL,
   17 17 1 ADDI, ;
s" c-bp-watch-row" s" --" TRUST

: C-BP-RESTORE-ONESHOT ( -- )
   2 3 MOVZ,  LPROT LABEL@ BL,
   8 SP 40 LDR,  11 8 0 LDR,  12 8 8 LDR,  12 11 0 STRW,
   2 5 MOVZ,  LPROT LABEL@ BL,
   9 11 0 ADDI,  LFLUSH LABEL@ BL,
   8 SP 40 LDR,  12 0 MOVZ,  12 8 0 STR, ;
s" c-bp-restore-oneshot" s" --" TRUST

: C-BP-EMULATE ( -- )
   9 SP 24 LDR,
   C-MCTX-SP-16!
   C-MCTX-PC+4! ;
s" c-bp-emulate" s" --" TRUST

: C-BP-SCAN ( label label label label -- )
   {: tno:label bscan:label bnext:label bhit:label :}
   6 8 MOVZ,  7 0 MOVZ,
   bscan LBL,
      7 6 CMP,  C-GE tno BCOND,
      8 7 5 LSLI,  14 BPTAB-OFF LIT64,  8 8 14 ADD,  8 DATA 8 ADD,
      13 8 0 LDR,  13 bnext CBZ,
      10 13 CMP,  C-EQ bhit BCOND,
      bnext LBL,  7 7 1 ADDI,  bscan B, ;
s" c-bp-scan" s" label label label label --" TRUST

: C-BP-STACK-DUMP ( label label -- )
   {: sdump:label sdone:label :}
   sdump LBL,
      14 SP 56 LDR,  17 14 CMP,  C-GE sdone BCOND,
      9 17 0 LDR,  17 SP 48 STR,  LHEX LABEL@ BL,
      17 SP 48 LDR,  17 17 8 ADDI,  sdump B,
   sdone LBL, ;
s" c-bp-stack-dump" s" label label --" TRUST

: C-BP-WATCH-DUMP ( label label -- )
   {: wloop:label wdone:label :}
   6 DATA BPWN-CELL LDR,  6 wdone CBZ,
   7 DATA BPWBASE-CELL LDR,  7 wdone CBZ,
   C-BP-WATCH-HEAD
   wloop LBL,
      17 6 CMP,  C-GE wdone BCOND,
      C-BP-WATCH-ROW  wloop B,
   wdone LBL, ;
s" c-bp-watch-dump" s" label label --" TRUST

\ LTRAPH: target signal entry. A one-shot
\ breakpoint at [BPA-CELL]: print habu-bp, pc, data-stack, and watch cells;
\ restore the original instruction, clear the bp, sigreturn to re-execute the word.
\ Any other trap falls through to the crash dump (x2/x4 untouched).
: EMIT-TRAPH ( -- )
   LTRAPH LABEL@ LBL,
   LBL {: tno :}
   C-TRAP-MCTX>R9                                    \ x9 = mcontext
   C-MCTX-PC>R10                                     \ x10 = pc
   LBL {: bscan :}  LBL {: bnext :}  LBL {: bhit :}
   LBL {: sdump :}  LBL {: sdone :}  LBL {: wloop :}  LBL {: wdone :}
   LBL {: emu :}  LBL {: fin :}
   tno bscan bnext bhit C-BP-SCAN                    \ scan BPTAB[0..8)
   \ slot layout: +0 addr  +8 saved-instr  +16 hits  +24 ctrl(skip<<1 | persist)
   bhit LBL,                                         \ x8=&slot x9=mctx x10=pc
   C-BP-HIT-SAVE                                     \ x15=ctrl  x12=skip
   14 12 CMP,  C-LS emu BCOND,                       \ hits <= skip -> silent, just emulate
   C-BP-PRINT-HIT
   C-BP-STACK-RANGE                                  \ x17=base x18=x19
   sdump sdone C-BP-STACK-DUMP
   wloop wdone C-BP-WATCH-DUMP
   8 SP 40 LDR,  15 8 24 LDR,  15 15 1 ANDI,  15 emu CBNZ,   \ persistent -> emulate, keep BRK
   C-BP-RESTORE-ONESHOT                              \ clear slot addr; resume re-runs orig
   fin B,
   emu LBL,                                          \ emulate the entry prologue, keep BRK:
   C-BP-EMULATE
   fin LBL,
   0 SP 8 LDR,  1 SP 0 LDR,  2 SP 16 LDR,  SP SP 80 ADDI,
   NR-SIGRETURN SYS,                                 \ sigreturn(uctx, infostyle, token)
   tno LBL,
   LCRASHH LABEL@ B,
   LBPH LABEL@ LBL,  BPH-KW 9 BYTES,
   LBPSH LABEL@ LBL, BPS-KW 15 BYTES,
   LBPWH LABEL@ LBL, BPW-KW 15 BYTES,
   LBADLOC LABEL@ LBL, BADLOC-KW $50 BYTES,
   LOPENERR LABEL@ LBL, s" hb: cannot open " BYTES,
   LOPENNL LABEL@ LBL, NL-KW 1 BYTES,
   LUNCMSG LABEL@ LBL, s" hb: uncaught throw code " BYTES,     \ UNCMSG-LEN bytes; LUNCAUGHT appends the signed code + newline
   LWIDEMSG LABEL@ LBL, s" hb: interpret-mode layout value: " BYTES,     \ WIDEMSG-LEN bytes; LWIDE appends the token + newline
   LINTMSG LABEL@ LBL, s" hb: internal engine word: " BYTES,             \ INTMSG-LEN bytes; LINTERNAL appends the token + newline
   LDICTFULL LABEL@ LBL, s" hb: dictionary full at: " BYTES,             \ CAPMSG-LEN bytes; capacity arms append the token + newline
   LCODEFULL LABEL@ LBL, s" hb: code space full at: " BYTES,             \ CAPMSG-LEN bytes
   LSNAPBAD LABEL@ LBL, s" hb: snapshot trailer corrupt" BYTES,  NL-KW 1 BYTES,               \ SNAPBAD-MSG-LEN bytes incl. newline
   LSNAPVER LABEL@ LBL, s" hb: snapshot format version unsupported" BYTES,  NL-KW 1 BYTES,     \ SNAPVER-MSG-LEN bytes incl. newline
   LSRCFULL LABEL@ LBL, s" hb: source prefix buffer full" BYTES,  NL-KW 1 BYTES,               \ SRCFULL-MSG-LEN bytes incl. newline
   LSRCREAD LABEL@ LBL, s" hb: cannot read source" BYTES,  NL-KW 1 BYTES,                       \ SRCREAD-MSG-LEN bytes incl. newline
   LBADSTR  LABEL@ LBL, s" hb: bad string literal" BYTES,  NL-KW 1 BYTES,                       \ BADSTR-MSG-LEN bytes incl. newline
   LPROTPUB LABEL@ LBL, s" hb: cannot publish into protected word: " BYTES,                     \ PROTPUB-MSG-LEN bytes; C-STORE-DEF-NAME appends the def name + newline
   LPROTAOT LABEL@ LBL, s" hb: AOT protected-WID gate reject" BYTES,  NL-KW 1 BYTES, ;          \ PROTAOT-MSG-LEN bytes incl. newline

\ override SIGTRAP(5) to the resuming handler (G-INSTALL-CRASH pointed all four
\ at the dumper; this repoints just TRAP once LTRAPH is bound).
: G-INSTALL-TRAP ( -- )
   9 LTRAPH LABEL@ ADR,  9 C-SIGACTION-FRAME
   5 INSTALL-SIGACT
   C-SIGACTION-FRAME-DONE ;

: EMIT-SHEBANG-COMMENT ( -- )
   LSHBANG LABEL@ LBL,
   LBL {: done :}
   4 9 17 SUB,  4 2 CMPI,  C-LT done BCOND,
   4 17 0 LDRB,  4 $23 CMPI,  C-NE done BCOND,
   4 17 1 LDRB,  4 $21 CMPI,  C-NE done BCOND,
   4 92 MOVZ,  4 17 0 STRB,
   4 32 MOVZ,  4 17 1 STRB,
   done LBL,
   RET, ;

: EMIT-SOURCE-READ ( -- )
   LSRCRD LABEL@ LBL,
   LBL LBL LBL LBL {: srl sdone sreaderr sopenerr :}
   LBL LBL LBL {: sscan:label sscandone:label sbufull:label :}
   12 OS-OPEN-RD
   13 C-CS CSET,  13 sopenerr CBNZ,
   12 0 0 ADDI,
   17 9 0 ADDI,
   srl LBL,
      0 12 0 ADDI,  1 9 0 ADDI,
      2 11 0 ADDI,  5 IBUFSZ LIT64,  2 2 5 ADD,  2 2 9 SUB,
      2 sbufull CBZ,                                \ no room left: IBUFSZ overflow, not a read fault
      NR-READ SYS,
      13 C-CS CSET,  13 sreaderr CBNZ,
      0 sdone CBZ,
      9 9 0 ADD,  srl B,
   sdone LBL,
   0 12 0 ADDI,  NR-CLOSE SYS,
   SP SP 16 SUBI,  30 SP 0 STR,
   LSHBANG LABEL@ BL,
   30 SP 0 LDR,  SP SP 16 ADDI,
   RET,
   sreaderr LBL,  0 12 0 ADDI,  NR-CLOSE SYS,     \ read() fault: x12 is the fd, no path to name; label the cause on fd 2
   1 LSRCREAD LABEL@ ADR,  0 2 MOVZ,  2 SRCREAD-MSG-LEN MOVZ,  NR-WRITE SYS,
   0 74 MOVZ,  NR-EXIT-GROUP SYS,
   sbufull LBL,  0 12 0 ADDI,  NR-CLOSE SYS,      \ source outgrew IBUFSZ mid-read: name the buffer, not a read fault
   1 LSRCFULL LABEL@ ADR,  0 2 MOVZ,  2 SRCFULL-MSG-LEN MOVZ,  NR-WRITE SYS,
   0 74 MOVZ,  NR-EXIT-GROUP SYS,
   sopenerr LBL,                                  \ x12 = NUL-terminated source path (untouched since open)
   1 LOPENERR LABEL@ ADR,  0 2 MOVZ,  2 16 MOVZ,  NR-WRITE SYS,   \ write(2,"hb: cannot open ",16)
   13 12 0 ADDI,                                  \ cursor := path
   sscan LBL,
      14 13 0 LDRB,  14 sscandone CBZ,             \ stop at the NUL terminator
      13 13 1 ADDI,  sscan B,
   sscandone LBL,
   2 13 12 SUB,  1 12 0 ADDI,  0 2 MOVZ,  NR-WRITE SYS,           \ write(2, path, len)
   1 LOPENNL LABEL@ ADR,  0 2 MOVZ,  2 1 MOVZ,  NR-WRITE SYS,     \ write(2,"\n",1)
   0 74 MOVZ,  NR-EXIT-GROUP SYS, ;

\ ---- CLI flag classifier ----------------------------------------------------
\ LFLAGMATCH: BL-callable, register-transparent (SP-frames x1..x7, only x0 set).
\ Input x12 = argv c-string. Output x0 = MODE-LOAD/MODE-SEP/MODE-FILE/MODE-UNKNOWN.
\ Walks FLAGTAB-DATA with a full-string compare (len + bytes + trailing NUL); a
\ leading '-' that matches no row is MODE-UNKNOWN, no leading '-' is MODE-FILE.
: EMIT-FLAG-MATCH ( -- )
   LFLAGMATCH LABEL@ LBL,
   LBL LBL LBL LBL LBL LBL LBL {: isfile:label recloop:label cmploop:label checknul:label nextrec:label unknown:label done:label :}
   SP SP 64 SUBI,                                  \ save x1..x7 (register-transparent)
   1 SP 0 STR,  2 SP 8 STR,  3 SP 16 STR,  4 SP 24 STR,
   5 SP 32 STR,  6 SP 40 STR,  7 SP 48 STR,
   1 12 0 LDRB,  1 $2D CMPI,  C-NE isfile BCOND,   \ no leading '-' -> file argument
   3 LFLAGTAB LABEL@ ADR,                          \ x3 = record cursor
   recloop LBL,
      4 3 0 LDRB,  4 unknown CBZ,                  \ len==0 terminator -> unknown flag
      5 3 1 LDRB,                                  \ x5 = mode
      6 3 2 ADDI,                                  \ x6 = name ptr (record+2)
      7 12 0 ADDI,                                 \ x7 = argv cursor
      cmploop LBL,
         4 checknul CBZ,                           \ all name bytes matched -> check NUL
         1 7 0 LDRB,  2 6 0 LDRB,  1 2 CMP,  C-NE nextrec BCOND,
         7 7 1 ADDI,  6 6 1 ADDI,  4 4 1 SUBI,  cmploop B,
      checknul LBL,
         1 7 0 LDRB,  1 nextrec CBNZ,              \ argv longer than flag -> mismatch
         0 5 0 ADDI,  done B,                      \ full match: x0 = mode
      nextrec LBL,
         1 3 0 LDRB,  3 3 1 ADD,  3 3 2 ADDI,  recloop B,   \ cursor += 2 + len
   isfile LBL,   0 MODE-FILE MOVZ,   done B,
   unknown LBL,  0 MODE-UNKNOWN MOVZ,
   done LBL,
   1 SP 0 LDR,  2 SP 8 LDR,  3 SP 16 LDR,  4 SP 24 LDR,
   5 SP 32 LDR,  6 SP 40 LDR,  7 SP 48 LDR,
   SP SP 64 ADDI,
   RET, ;

\ LSRCBADFLAG: reached with x12 = offending argv; the handler exits so it may
\ freely clobber callee-saved x21/x22 (both survive the write syscalls). Writes
\ the offending flag then a usage line built by iterating FLAGTAB-DATA, exit 64.
: EMIT-FLAG-REJECT ( -- )
   LSRCBADFLAG LABEL@ LBL,
   LBL LBL LBL LBL {: bscan:label bdone:label uloop:label udone:label :}
   21 12 0 ADDI,                                   \ x21 = offending arg
   1 LBADFLAG LABEL@ ADR,  0 2 MOVZ,  2 18 MOVZ,  NR-WRITE SYS,   \ "hb: unknown flag: "
   22 21 0 ADDI,                                   \ x22 = strlen cursor
   bscan LBL,  14 22 0 LDRB,  14 bdone CBZ,  22 22 1 ADDI,  bscan B,
   bdone LBL,  2 22 21 SUB,  1 21 0 ADDI,  0 2 MOVZ,  NR-WRITE SYS,   \ write(2, arg, len)
   1 LOPENNL LABEL@ ADR,  0 2 MOVZ,  2 1 MOVZ,  NR-WRITE SYS,         \ "\n"
   1 LUSAGE1 LABEL@ ADR,  0 2 MOVZ,  2 13 MOVZ,  NR-WRITE SYS,        \ "usage: bin/hb"
   22 LFLAGTAB LABEL@ ADR,                         \ x22 = table cursor
   uloop LBL,
      14 22 0 LDRB,  14 udone CBZ,                 \ len==0 terminator -> usage tail
      1 LSPC LABEL@ ADR,  0 2 MOVZ,  2 1 MOVZ,  NR-WRITE SYS,         \ " "
      1 22 2 ADDI,  2 22 0 LDRB,  0 2 MOVZ,  NR-WRITE SYS,            \ write(2, name, len)
      14 22 0 LDRB,  22 22 14 ADD,  22 22 2 ADDI,  uloop B,           \ cursor += 2 + len
   udone LBL,
   1 LUSAGE2 LABEL@ ADR,  0 2 MOVZ,  2 28 MOVZ,  NR-WRITE SYS,        \ " [file.f]  (source on stdin)"
   1 LOPENNL LABEL@ ADR,  0 2 MOVZ,  2 1 MOVZ,  NR-WRITE SYS,         \ "\n"
   0 64 MOVZ,  NR-EXIT-GROUP SYS, ;

\ Flag byte table (read only via ADR) plus the reject message strings.
: EMIT-FLAG-TABLE ( -- )
   LBADFLAG LABEL@ LBL,  s" hb: unknown flag: " BYTES,
   LUSAGE1 LABEL@ LBL,   s" usage: bin/hb" BYTES,
   LUSAGE2 LABEL@ LBL,   s"  [file.f]  (source on stdin)" BYTES,
   LSPC LABEL@ LBL,      ONESP 1 BYTES,
   LFLAGTAB LABEL@ LBL,  FLAGTAB-DATA FLAGTAB-LEN BYTES, ;

: EMIT-FLAGS ( -- )
   EMIT-FLAG-MATCH
   EMIT-FLAG-REJECT
   EMIT-FLAG-TABLE ;

: C-TARGET-UNKNOWN ( -- )
   s" hb: unknown target" 76 die ;

0 constant PFX-COMMON
1 constant PFX-LINUX
2 constant PFX-MACOS

: PFX-TARGET-OK ( -- )
   HB-TARGET-LINUX? if exit then
   HB-TARGET-MACOS? if exit then
   C-TARGET-UNKNOWN ;

: PFX-LOAD? ( n -- bool )
   case PFX-COMMON of 0 0= endof PFX-LINUX of HB-TARGET-LINUX? endof
      PFX-MACOS of HB-TARGET-MACOS? endof
      0 0= 0= swap endcase ;

: PFX-LOAD-ROW ( n ptr n ptr u8 n -- ) {: kind var a u :}
   kind PFX-LOAD? if 12 var LABEL@ ADR,  LSRCRD LABEL@ BL, then ;

: PFX-PATH-ROW ( n ptr n ptr u8 n -- ) {: kind var a u :}
   var LABEL@ LBL,  a u ZBYTES, ;

: PFX-LOAD-BASE-FILES ( -- )
   PFX-COMMON LPUTIL         s" src/core/util.f"        PFX-LOAD-ROW
   PFX-COMMON LPSTRUCTURES   s" src/core/structures.f"  PFX-LOAD-ROW
   PFX-COMMON LPCHECKER      s" src/core/checker.f"     PFX-LOAD-ROW
   PFX-COMMON LPLOWERCERTBASE s" src/core/lower-cert-base.f" PFX-LOAD-ROW
   PFX-COMMON LPTYPESCHEMA   s" src/core/type-schema.f" PFX-LOAD-ROW
   PFX-COMMON LPTYPEFAM      s" src/core/type-family.f" PFX-LOAD-ROW
   PFX-COMMON LPRENDER       s" src/core/render.f"      PFX-LOAD-ROW
   PFX-COMMON LPSUMTYPE      s" src/core/sumtype.f"     PFX-LOAD-ROW
   PFX-COMMON LPLAYOUTBUF    s" src/core/layout-buffer.f" PFX-LOAD-ROW
   PFX-COMMON LPLAYOUTVALID  s" src/core/layout-valid.f" PFX-LOAD-ROW
   PFX-COMMON LPHOOK         s" src/core/check-hook.f"  PFX-LOAD-ROW
   PFX-COMMON LPSTRUCTEFF    s" src/core/structures-effects.f" PFX-LOAD-ROW
   PFX-COMMON LPROLES        s" src/core/roles.f"       PFX-LOAD-ROW
   PFX-COMMON LPBYTES        s" src/core/bytes.f"       PFX-LOAD-ROW
   PFX-LINUX  LPLINUXTARGET  s" src/os/linux/target.f"  PFX-LOAD-ROW
   PFX-MACOS  LPMACOSTARGET  s" src/os/macos/target.f"  PFX-LOAD-ROW
   PFX-LINUX  LPLINUXLAYOUT  s" src/os/linux/layout.f"  PFX-LOAD-ROW
   PFX-MACOS  LPMACOSLAYOUT  s" src/os/macos/layout.f"  PFX-LOAD-ROW
   PFX-COMMON LPHABULAYOUT   s" src/habu/layout.f"      PFX-LOAD-ROW
   PFX-COMMON LPENVBASE      s" src/os/env-base.f"      PFX-LOAD-ROW
   PFX-COMMON LPINCLUDE      s" src/core/include.f"     PFX-LOAD-ROW
   PFX-COMMON LPENUMS        s" src/core/enums.f"       PFX-LOAD-ROW
   PFX-COMMON LPEXECVECTOR   s" src/core/exec-vector.f" PFX-LOAD-ROW
   PFX-COMMON LPSHA256       s" src/core/sha256.f"      PFX-LOAD-ROW
   PFX-COMMON LPTFAMSHA      s" src/core/type-family-sha.f" PFX-LOAD-ROW
   PFX-COMMON LPCOMBINATORS  s" src/core/combinators.f" PFX-LOAD-ROW
   PFX-COMMON LPXREF         s" src/habu/xref.f"        PFX-LOAD-ROW
   PFX-COMMON LPLAYOUTSEAL   s" src/core/layout-buffer-seal.f" PFX-LOAD-ROW
   PFX-COMMON LPLOWERCERTSEAL s" src/core/lower-cert-seal.f" PFX-LOAD-ROW ;

: PFX-LOAD-SCRIPT-ARGV ( -- )
   PFX-COMMON LPSCRIPTARGV   s" src/os/script-argv.f"   PFX-LOAD-ROW ;

: PFX-LOAD-SCRIPT-ARGV-COLD ( -- )
   LBL {: done :}
   12 DATA SNAP-CELL LDR,
   12 done CBNZ,
   PFX-LOAD-SCRIPT-ARGV
   done LBL, ;

\ internal-mark.f is the LAST prefix source (dot habu-hb-crash-bare-c5be6634):
\ its marking pass must see every prefix definition and every recorded effect
\ (structures-effects.f TRUST rows, xref/script-argv signatures) before it
\ classifies the remaining sig-less prefix words as DNAME-INT internal.
: PFX-LOAD-INTMARK ( -- )
   PFX-COMMON LPINTMARK      s" src/core/internal-mark.f" PFX-LOAD-ROW ;

: PFX-LOAD-INTMARK-COLD ( -- )
   LBL {: done:label :}
   12 DATA SNAP-CELL LDR,
   12 done CBNZ,
   PFX-LOAD-INTMARK
   done LBL, ;

: PFX-LOAD-FILES ( -- )
   PFX-LOAD-BASE-FILES
   PFX-LOAD-SCRIPT-ARGV
   PFX-LOAD-INTMARK ;

: PFX-PATH-FILES ( -- )
   PFX-COMMON LPUTIL         s" src/core/util.f"        PFX-PATH-ROW
   PFX-COMMON LPSTRUCTURES   s" src/core/structures.f"  PFX-PATH-ROW
   PFX-COMMON LPCHECKER      s" src/core/checker.f"     PFX-PATH-ROW
   PFX-COMMON LPLOWERCERTBASE s" src/core/lower-cert-base.f" PFX-PATH-ROW
   PFX-COMMON LPTYPESCHEMA   s" src/core/type-schema.f" PFX-PATH-ROW
   PFX-COMMON LPTYPEFAM      s" src/core/type-family.f" PFX-PATH-ROW
   PFX-COMMON LPRENDER       s" src/core/render.f"      PFX-PATH-ROW
   PFX-COMMON LPSUMTYPE      s" src/core/sumtype.f"     PFX-PATH-ROW
   PFX-COMMON LPLAYOUTBUF    s" src/core/layout-buffer.f" PFX-PATH-ROW
   PFX-COMMON LPLAYOUTVALID  s" src/core/layout-valid.f" PFX-PATH-ROW
   PFX-COMMON LPHOOK         s" src/core/check-hook.f"  PFX-PATH-ROW
   PFX-COMMON LPSTRUCTEFF    s" src/core/structures-effects.f" PFX-PATH-ROW
   PFX-COMMON LPROLES        s" src/core/roles.f"       PFX-PATH-ROW
   PFX-COMMON LPBYTES        s" src/core/bytes.f"       PFX-PATH-ROW
   PFX-LINUX  LPLINUXTARGET  s" src/os/linux/target.f"  PFX-PATH-ROW
   PFX-MACOS  LPMACOSTARGET  s" src/os/macos/target.f"  PFX-PATH-ROW
   PFX-LINUX  LPLINUXLAYOUT  s" src/os/linux/layout.f"  PFX-PATH-ROW
   PFX-MACOS  LPMACOSLAYOUT  s" src/os/macos/layout.f"  PFX-PATH-ROW
   PFX-COMMON LPHABULAYOUT   s" src/habu/layout.f"      PFX-PATH-ROW
   PFX-COMMON LPENVBASE      s" src/os/env-base.f"      PFX-PATH-ROW
   PFX-COMMON LPINCLUDE      s" src/core/include.f"     PFX-PATH-ROW
   PFX-COMMON LPSCRIPTARGV   s" src/os/script-argv.f"   PFX-PATH-ROW
   PFX-COMMON LPINTMARK      s" src/core/internal-mark.f" PFX-PATH-ROW
   PFX-COMMON LPENUMS        s" src/core/enums.f"       PFX-PATH-ROW
   PFX-COMMON LPEXECVECTOR   s" src/core/exec-vector.f" PFX-PATH-ROW
   PFX-COMMON LPSHA256       s" src/core/sha256.f"      PFX-PATH-ROW
   PFX-COMMON LPTFAMSHA      s" src/core/type-family-sha.f" PFX-PATH-ROW
   PFX-COMMON LPCOMBINATORS  s" src/core/combinators.f" PFX-PATH-ROW
   PFX-COMMON LPXREF         s" src/habu/xref.f"        PFX-PATH-ROW
   PFX-COMMON LPLAYOUTSEAL   s" src/core/layout-buffer-seal.f" PFX-PATH-ROW
   PFX-COMMON LPLOWERCERTSEAL s" src/core/lower-cert-seal.f" PFX-PATH-ROW ;

: EMIT-HOST-LOAD-PREFIX ( -- )
   16 0 MOVZ,  16 DATA HOOK-CELL STR,
   PFX-TARGET-OK
   PFX-LOAD-BASE-FILES ;

: EMIT-COLD-PREFIX ( -- )
   LBL {: done :}
   12 DATA SNAP-CELL LDR,
   12 done CBNZ,
   EMIT-HOST-LOAD-PREFIX
   done LBL, ;

\ Seal the friend arena (TFAM 2b-i): latch := FRIEND-ARENA-LEN. Emitted at the
\ END of the cold prefix — after the engine's own canonical source is loaded and
\ before ANY user token (--load file, stdin pipe, baked LSRC, or REPL) is
\ evaluated — so every raw write into the arena from user source is trapped
\ fail-closed. Self-sealing: once the latch is set, clearing it is a protected
\ write and traps, so the seal is one-way. Uses x5 (a cold-prefix scratch reg);
\ x9=cursor and x11=buffer base are live across this point and preserved.
\ The latch is set BEFORE the engine's own checker/xref/stdlib source is
\ evaluated (that source runs post-latch via guard-bypassing DATA stores), so the
\ seal-time ndict is NOT the engine boundary; the truncation watermark is instead
\ captured by SEAL-CAPTURE (habu1.f) tokens in the source stream - a baseline at
\ the end of xref.f plus the cold-prefix assembler's token at the true prefix
\ end (EMIT-SEAL-CAPTURE-TOKEN, after script-argv.f and the provide rows).
\ MODE-BUILD is not a user-source entry: build-fixpoint first certifies its
\ compiler payload, LCOLDPFXB leaves the latch open for that compiler prefix,
\ and the payload executes SEAL-FRIEND before its driver.
: C-EMIT-TTY-PROBE ( -- )
   0 0 MOVZ,
   HB-TARGET-LINUX? if 1 $5401 LIT64, else
      HB-TARGET-MACOS? if 1 $40487413 LIT64, else C-TARGET-UNKNOWN then
   then
   2 DATA BODYBUF-OFF ADDI,
   NR-IOCTL SYS, ;
s" c-emit-tty-probe" s" --" TRUST

variable SRC-TTY  variable SRC-FILE  variable SRC-SFAIL
variable SRC-RL   variable SRC-RD    variable SRC-PIPEOK
variable SRC-REPL variable SRC-DONE  variable SRC-FSCAN
variable SRC-FNEXT variable SRC-FREADY variable SRC-FPLAIN
variable SRC-FLOOP variable SRC-SHLOOP variable SRC-STDINPROG
variable SRC-BLOOP variable SRC-BDONE  variable SRC-BFAIL
variable LCOLDPFX variable LCOLDPFXB variable LAPPPROV

: C-SOURCE-LABELS ( -- )
   LBL SRC-TTY !   LBL SRC-FILE !  LBL SRC-SFAIL !
   LBL SRC-RL !    LBL SRC-RD !    LBL SRC-PIPEOK !
   LBL SRC-REPL !  LBL SRC-DONE !  LBL SRC-FSCAN !
   LBL SRC-FNEXT ! LBL SRC-FREADY ! LBL SRC-FPLAIN !
   LBL SRC-FLOOP ! LBL SRC-SHLOOP ! LBL SRC-STDINPROG !
   LBL SRC-BLOOP ! LBL SRC-BDONE ! LBL SRC-BFAIL !
   LBL LCOLDPFX !  LBL LCOLDPFXB !  LBL LAPPPROV ! ;

: C-SOURCE-MMAP ( label -- ) {: fail:label :}
   0 0 MOVZ,  1 IBUFSZ LIT64,  2 3 MOVZ,
   3 MAP-ANON-PRIVATE LIT64,  4 0 MOVN,  5 0 MOVZ,
   NR-MMAP SYS,
   13 C-CS CSET,  13 fail CBNZ, ;

: C-SOURCE-SKIP-SHEBANG ( -- )
   12 9 11 SUB,  12 2 CMPI,  C-LT SRC-DONE LABEL@ BCOND,
   4 11 0 LDRB,  4 $23 CMPI,  C-NE SRC-DONE LABEL@ BCOND,
   4 11 1 LDRB,  4 $21 CMPI,  C-NE SRC-DONE LABEL@ BCOND,
   11 11 2 ADDI,
   SRC-SHLOOP LABEL@ LBL,
      11 9 CMP,  C-GE SRC-DONE LABEL@ BCOND,
      4 11 0 LDRB,  11 11 1 ADDI,
      11 DATA INP-CELL STR,
      4 10 CMPI,  C-EQ SRC-DONE LABEL@ BCOND,
      SRC-SHLOOP LABEL@ B, ;

: C-SOURCE-FIND-SEP ( -- )
   SRC-FSCAN LABEL@ LBL,
      13 10 CMP,  C-GE SRC-FREADY LABEL@ BCOND,
      12 DATA ARGV-CELL LDR,  5 13 3 LSLI,  12 12 5 ADD,  12 12 0 LDR,
      LFLAGMATCH LABEL@ BL,  0 MODE-SEP CMPI,  C-NE SRC-FNEXT LABEL@ BCOND,
      15 13 0 ADDI,  SRC-FREADY LABEL@ B,
   SRC-FNEXT LABEL@ LBL,  13 13 1 ADDI,  SRC-FSCAN LABEL@ B, ;

: C-SOURCE-ARGV1 ( -- )
   12 DATA ARGV-CELL LDR,  12 12 8 LDR, ;

: C-SOURCE-FILE-MAP ( -- )
   SRC-SFAIL @ C-SOURCE-MMAP
   11 0 0 ADDI, ;

: C-SOURCE-APPEND-X4-TO ( label -- ) {: fail:label :}
   2 11 0 ADDI,
   5 IBUFSZ LIT64,
   2 2 5 ADD,
   9 2 CMP,
   C-GE fail BCOND,
   4 9 0 STRB,
   9 9 1 ADDI, ;

: C-SOURCE-APPEND-X4 ( -- )
   SRC-SFAIL LABEL@ C-SOURCE-APPEND-X4-TO ;

: C-SOURCE-APPEND-CHAR ( n -- ) {: c:n :}
   4 c MOVZ,
   C-SOURCE-APPEND-X4 ;

: C-SOURCE-APPEND-CHAR-TO ( n label -- ) {: c:n fail:label :}
   4 c MOVZ,
   fail C-SOURCE-APPEND-X4-TO ;

: EMIT-SEAL-FRIEND-TOKEN ( label -- ) {: fail:label :}
   ENGINE-BUILD:BUILDING? if exit then
   LBL {: done:label :}
   12 DATA SNAP-CELL LDR,
   12 done CBNZ,
   $53 fail C-SOURCE-APPEND-CHAR-TO
   $45 fail C-SOURCE-APPEND-CHAR-TO
   $41 fail C-SOURCE-APPEND-CHAR-TO
   $4C fail C-SOURCE-APPEND-CHAR-TO
   $2D fail C-SOURCE-APPEND-CHAR-TO
   $46 fail C-SOURCE-APPEND-CHAR-TO
   $52 fail C-SOURCE-APPEND-CHAR-TO
   $49 fail C-SOURCE-APPEND-CHAR-TO
   $45 fail C-SOURCE-APPEND-CHAR-TO
   $4E fail C-SOURCE-APPEND-CHAR-TO
   $44 fail C-SOURCE-APPEND-CHAR-TO
   $0A fail C-SOURCE-APPEND-CHAR-TO
   done LBL, ;

: C-SOURCE-APPEND-Z12 ( -- )
   LBL LBL {: loop:label done:label :}
   loop LBL,
      4 12 0 LDRB,
      4 done CBZ,
      C-SOURCE-APPEND-X4
      12 12 1 ADDI,
      loop B,
   done LBL, ;

: C-SOURCE-APPEND-PROVIDED ( -- )
   $73 C-SOURCE-APPEND-CHAR
   $22 C-SOURCE-APPEND-CHAR
   $20 C-SOURCE-APPEND-CHAR
   C-SOURCE-APPEND-Z12
   $22 C-SOURCE-APPEND-CHAR
   $20 C-SOURCE-APPEND-CHAR
   $70 C-SOURCE-APPEND-CHAR
   $72 C-SOURCE-APPEND-CHAR
   $6F C-SOURCE-APPEND-CHAR
   $76 C-SOURCE-APPEND-CHAR
   $69 C-SOURCE-APPEND-CHAR
   $64 C-SOURCE-APPEND-CHAR
   $65 C-SOURCE-APPEND-CHAR
   $64 C-SOURCE-APPEND-CHAR
   $0A C-SOURCE-APPEND-CHAR ;

\ TFAM 2b-iii (dot habu-tfam-2b-iii-5d25b52f): append `SEAL-CAPTURE` as the
\ LAST engine-prefix source token, after every engine file and the provide
\ rows. xref.f's in-file call is only the baseline: src/os/script-argv.f loads
\ after xref.f, so a watermark frozen there left the argv tail FORGET/HIDEable.
\ The capture must be a SOURCE token, not a native store: at cold-prefix
\ assembly time nothing has evaluated yet - ndict is still the native-prim
\ boundary and only grows as the interpret loop chews the concatenated source.
\ Cold boots only: a snapshot restores its bake-time watermark from the
\ persisted friend band (same gate as PFX-LOAD-SCRIPT-ARGV-COLD).
: EMIT-SEAL-CAPTURE-TOKEN ( -- )
   LBL {: done:label :}
   12 DATA SNAP-CELL LDR,
   12 done CBNZ,
   $53 C-SOURCE-APPEND-CHAR
   $45 C-SOURCE-APPEND-CHAR
   $41 C-SOURCE-APPEND-CHAR
   $4C C-SOURCE-APPEND-CHAR
   $2D C-SOURCE-APPEND-CHAR
   $43 C-SOURCE-APPEND-CHAR
   $41 C-SOURCE-APPEND-CHAR
   $50 C-SOURCE-APPEND-CHAR
   $54 C-SOURCE-APPEND-CHAR
   $55 C-SOURCE-APPEND-CHAR
   $52 C-SOURCE-APPEND-CHAR
   $45 C-SOURCE-APPEND-CHAR
   $0A C-SOURCE-APPEND-CHAR
   done LBL, ;

: PFX-PROVIDE-ROW ( n ptr n ptr u8 n -- ) {: kind:n var:ptr a:ptr u:n :}
   kind PFX-LOAD? if
      12 var LABEL@ ADR,
      LAPPPROV LABEL@ BL,
   then ;

: PFX-PROVIDE-FILES ( -- )
   PFX-COMMON LPUTIL         s" src/core/util.f"        PFX-PROVIDE-ROW
   PFX-COMMON LPSTRUCTURES   s" src/core/structures.f"  PFX-PROVIDE-ROW
   PFX-COMMON LPCHECKER      s" src/core/checker.f"     PFX-PROVIDE-ROW
   PFX-COMMON LPLOWERCERTBASE s" src/core/lower-cert-base.f" PFX-PROVIDE-ROW
   PFX-COMMON LPTYPESCHEMA   s" src/core/type-schema.f" PFX-PROVIDE-ROW
   PFX-COMMON LPTYPEFAM      s" src/core/type-family.f" PFX-PROVIDE-ROW
   PFX-COMMON LPRENDER       s" src/core/render.f"      PFX-PROVIDE-ROW
   PFX-COMMON LPSUMTYPE      s" src/core/sumtype.f"     PFX-PROVIDE-ROW
   PFX-COMMON LPLAYOUTBUF    s" src/core/layout-buffer.f" PFX-PROVIDE-ROW
   PFX-COMMON LPLAYOUTVALID  s" src/core/layout-valid.f" PFX-PROVIDE-ROW
   PFX-COMMON LPHOOK         s" src/core/check-hook.f"  PFX-PROVIDE-ROW
   PFX-COMMON LPSTRUCTEFF    s" src/core/structures-effects.f" PFX-PROVIDE-ROW
   PFX-COMMON LPROLES        s" src/core/roles.f"       PFX-PROVIDE-ROW
   PFX-COMMON LPBYTES        s" src/core/bytes.f"       PFX-PROVIDE-ROW
   PFX-LINUX  LPLINUXTARGET  s" src/os/linux/target.f"  PFX-PROVIDE-ROW
   PFX-MACOS  LPMACOSTARGET  s" src/os/macos/target.f"  PFX-PROVIDE-ROW
   PFX-LINUX  LPLINUXLAYOUT  s" src/os/linux/layout.f"  PFX-PROVIDE-ROW
   PFX-MACOS  LPMACOSLAYOUT  s" src/os/macos/layout.f"  PFX-PROVIDE-ROW
   PFX-COMMON LPHABULAYOUT   s" src/habu/layout.f"      PFX-PROVIDE-ROW
   PFX-COMMON LPENVBASE      s" src/os/env-base.f"      PFX-PROVIDE-ROW
   PFX-COMMON LPINCLUDE      s" src/core/include.f"     PFX-PROVIDE-ROW
   PFX-COMMON LPSCRIPTARGV   s" src/os/script-argv.f"   PFX-PROVIDE-ROW
   PFX-COMMON LPINTMARK      s" src/core/internal-mark.f" PFX-PROVIDE-ROW
   PFX-COMMON LPENUMS        s" src/core/enums.f"       PFX-PROVIDE-ROW
   PFX-COMMON LPEXECVECTOR   s" src/core/exec-vector.f" PFX-PROVIDE-ROW
   PFX-COMMON LPSHA256       s" src/core/sha256.f"      PFX-PROVIDE-ROW
   PFX-COMMON LPTFAMSHA      s" src/core/type-family-sha.f" PFX-PROVIDE-ROW
   PFX-COMMON LPCOMBINATORS  s" src/core/combinators.f" PFX-PROVIDE-ROW
   PFX-COMMON LPXREF         s" src/habu/xref.f"        PFX-PROVIDE-ROW
   PFX-COMMON LPLAYOUTSEAL   s" src/core/layout-buffer-seal.f" PFX-PROVIDE-ROW
   PFX-COMMON LPLOWERCERTSEAL s" src/core/lower-cert-seal.f" PFX-PROVIDE-ROW ;

: C-SOURCE-PIPE ( -- )
   SRC-STDINPROG LABEL@ LBL,
   SRC-SFAIL @ C-SOURCE-MMAP
   11 0 0 ADDI,  9 0 0 ADDI,
   LCOLDPFX LABEL@ BL,
   17 9 0 ADDI,
   SRC-RL LABEL@ LBL,
      0 0 MOVZ,  1 9 0 ADDI,
      2 11 0 ADDI,  5 IBUFSZ LIT64,  2 2 5 ADD,  2 2 9 SUB,
      2 SRC-SFAIL LABEL@ CBZ,
      NR-READ SYS,
      13 C-CS CSET,  13 SRC-SFAIL LABEL@ CBNZ,
      0 SRC-RD LABEL@ CBZ,
      9 9 0 ADD,  SRC-RL LABEL@ B,
   SRC-RD LABEL@ LBL,
   LSHBANG LABEL@ BL,
   9 17 CMP,  C-NE SRC-PIPEOK LABEL@ BCOND,
   10 DATA ARGC-CELL LDR,  10 1 CMPI,  C-GT SRC-FILE LABEL@ BCOND,
   SRC-PIPEOK LABEL@ LBL,
   11 DATA INP-CELL STR,  9 DATA INE-CELL STR,
   C-SOURCE-SKIP-SHEBANG ;

: C-SOURCE-FILE-INIT ( -- )
   9 11 0 ADDI,
   10 DATA ARGC-CELL LDR,
   14 1 MOVZ,  15 2 MOVZ,
   C-SOURCE-ARGV1 ;

: C-SOURCE-FILE-PREFIX ( -- )
   LBL LBL LBL {: load:label build:label multi:label :}
   LFLAGMATCH LABEL@ BL,                                   \ x12 = argv[1] -> x0 = mode
   0 MODE-UNKNOWN CMPI,  C-EQ LSRCBADFLAG LABEL@ BCOND,    \ unknown flag -> reject rc 64
   0 MODE-BUILD CMPI,    C-EQ build BCOND,
   0 MODE-LOAD CMPI,     C-EQ load BCOND,
   SRC-FPLAIN LABEL@ B,
   load LBL,
   14 2 MOVZ,  15 10 0 ADDI,  13 2 MOVZ,
   LCOLDPFX LABEL@ BL,
   multi B,
   build LBL,
   14 2 MOVZ,  15 10 0 ADDI,  13 2 MOVZ,
   LCOLDPFXB LABEL@ BL,
   multi LBL,
   C-SOURCE-FIND-SEP
   SRC-FPLAIN LABEL@ LBL,
   LCOLDPFX LABEL@ BL,
   SRC-FREADY LABEL@ LBL, ;

: C-SOURCE-ARGV14 ( -- )
   12 DATA ARGV-CELL LDR,  5 14 3 LSLI,
   12 12 5 ADD,  12 12 0 LDR, ;

: C-SOURCE-APPEND-ARG ( -- )
   C-SOURCE-ARGV14
   LAPPPROV LABEL@ BL,
   C-SOURCE-ARGV14
   LSRCRD LABEL@ BL,
   14 14 1 ADDI, ;

: C-SOURCE-APPEND-LF ( -- )
   2 11 0 ADDI,  5 IBUFSZ LIT64,  2 2 5 ADD,
   9 2 CMP,  C-GE SRC-SFAIL LABEL@ BCOND,
   5 10 MOVZ,  5 9 0 STRB,  9 9 1 ADDI, ;

: C-SOURCE-FILE-LOOP ( -- )
   SRC-FLOOP LABEL@ LBL,
      14 15 CMP,  C-GE SRC-PIPEOK LABEL@ BCOND,
      C-SOURCE-APPEND-ARG
      14 15 CMP,  C-GE SRC-PIPEOK LABEL@ BCOND,
      C-SOURCE-APPEND-LF
      SRC-FLOOP LABEL@ B, ;

: C-SOURCE-APPEND-LSRC ( -- )
   LBL LBL {: loop:label done:label :}
   12 LSRC LABEL@ ADR,  5 SRCN @ LIT64,  13 12 5 ADD,
   loop LBL,
      12 13 CMP,  C-GE done BCOND,
      2 11 0 ADDI,  5 IBUFSZ LIT64,  2 2 5 ADD,  9 2 CMP,  C-GE SRC-SFAIL LABEL@ BCOND,
      4 12 0 LDRB,  4 9 0 STRB,
      12 12 1 ADDI,  9 9 1 ADDI,
      loop B,
   done LBL, ;

: C-SOURCE-FAIL-REPL-DONE ( -- )
   SRC-SFAIL LABEL@ LBL,                                          \ IBUFSZ source-prefix overflow: label fd 2 before exit 74
   1 LSRCFULL LABEL@ ADR,  0 2 MOVZ,  2 SRCFULL-MSG-LEN MOVZ,  NR-WRITE SYS,
   0 74 MOVZ,  NR-EXIT-GROUP SYS,
   SRC-REPL LABEL@ LBL,
   SRC-SFAIL @ C-SOURCE-MMAP
   11 0 0 ADDI,  9 11 0 ADDI,
   LCOLDPFX LABEL@ BL,
   C-SOURCE-APPEND-LSRC
   11 DATA INP-CELL STR,  9 DATA INE-CELL STR,
   12 1 MOVZ,  12 DATA AOT-SEED-ARM-CELL STR,     \ arm the AOT REPL seed: interactive repl only
   SRC-DONE LABEL@ B,
   SRC-DONE LABEL@ LBL, ;

: C-SOURCE-FILE-LIST ( -- )
   9 DATA ARGC-CELL LDR,  9 1 CMPI,  C-LE SRC-REPL LABEL@ BCOND,
   C-SOURCE-FILE-MAP
   SRC-FILE LABEL@ LBL,
   C-SOURCE-FILE-INIT
   C-SOURCE-FILE-PREFIX
   14 15 CMP,  C-GE SRC-SFAIL LABEL@ BCOND,
   C-SOURCE-FILE-LOOP
   C-SOURCE-FAIL-REPL-DONE ;

: C-SOURCE-STDIN ( -- )
   C-EMIT-TTY-PROBE
   0 SRC-TTY LABEL@ CBZ,
   10 DATA ARGC-CELL LDR,  10 1 CMPI,  C-LE SRC-STDINPROG LABEL@ BCOND,
   C-SOURCE-ARGV1
   LFLAGMATCH LABEL@ BL,                                   \ x12 = argv[1] -> x0 = mode
   0 MODE-UNKNOWN CMPI,  C-EQ LSRCBADFLAG LABEL@ BCOND,    \ unknown flag -> reject rc 64
   0 MODE-LOAD CMPI,     C-EQ SRC-TTY LABEL@ BCOND,        \ --load -> file-list path
   0 MODE-BUILD CMPI,    C-EQ SRC-TTY LABEL@ BCOND,        \ --build -> verified compiler-source path
   C-SOURCE-PIPE                                           \ SEP/FILE -> read stdin as program
   SRC-TTY LABEL@ LBL,
   C-SOURCE-FILE-LIST ;

: C-SOURCE-BAKED ( -- )
   SRC-BFAIL @ C-SOURCE-MMAP
   11 0 0 ADDI,  9 0 0 ADDI,
   EMIT-COLD-PREFIX
   SRC-BFAIL LABEL@ EMIT-SEAL-FRIEND-TOKEN         \ seal after the cold prefix, before baked user source
   17 9 0 ADDI,
   12 LSRC LABEL@ ADR,  5 SRCN @ LIT64,  13 12 5 ADD,
   SRC-BLOOP LABEL@ LBL,
      12 13 CMP,  C-GE SRC-BDONE LABEL@ BCOND,
      2 11 0 ADDI,  5 IBUFSZ LIT64,  2 2 5 ADD,  9 2 CMP,  C-GE SRC-BFAIL LABEL@ BCOND,
      4 12 0 LDRB,  4 9 0 STRB,
      12 12 1 ADDI,  9 9 1 ADDI,
      SRC-BLOOP LABEL@ B,
   SRC-BDONE LABEL@ LBL,
   LSHBANG LABEL@ BL,
   11 DATA INP-CELL STR,  9 DATA INE-CELL STR,  SRC-DONE LABEL@ B,
   SRC-BFAIL LABEL@ LBL,                                          \ IBUFSZ baked-prefix overflow: label fd 2 before exit 74
   1 LSRCFULL LABEL@ ADR,  0 2 MOVZ,  2 SRCFULL-MSG-LEN MOVZ,  NR-WRITE SYS,
   0 74 MOVZ,  NR-EXIT-GROUP SYS,
   SRC-DONE LABEL@ LBL, ;

\ Shared cold-prefix routines: the stdin engine builds the checker/stdlib
\ provided-files prefix at the user entries plus the certified MODE-BUILD entry.
\ Two routines, emitted once each and
\ branched over so the fall-through startup path skips their bodies:
\
\ LAPPPROV ( x12 = string ptr, x9 = cursor -> appends `s" <str>" provided\n`,
\ x9 advanced ). Leaf: the append sequence uses only STRB/branch (no BL), so no
\ x30 frame. Replaces the ~552-byte per-row inline C-SOURCE-APPEND-PROVIDED,
\ which was emitted ~19 times in PFX-PROVIDE-FILES plus once in the argv loop.
\
\ LCOLDPFX/LCOLDPFXB load the same prefix. LCOLDPFX seals before ordinary
\ source; LCOLDPFXB is the build-only entry used for a statically certified
\ compiler payload that contains its own SEAL-FRIEND boundary before its
\ driver. x30 and the entry mode survive the internal LSRCRD/LAPPPROV calls.
: EMIT-COLD-PREFIX-SHARED ( -- )
   LBL LBL LBL {: skip:label body:label noseal:label :}
   skip B,
   LAPPPROV LABEL@ LBL,
      C-SOURCE-APPEND-PROVIDED  RET,
   LCOLDPFX LABEL@ LBL,
      SP SP 16 SUBI,  30 SP 0 STR,
      12 0 MOVZ,  12 SP 8 STR,  body B,
   LCOLDPFXB LABEL@ LBL,
      SP SP 16 SUBI,  30 SP 0 STR,
      12 1 MOVZ,  12 SP 8 STR,
   body LBL,
      EMIT-COLD-PREFIX
      PFX-LOAD-SCRIPT-ARGV-COLD
      PFX-LOAD-INTMARK-COLD                        \ LAST prefix source: seal-time internal-word marking pass
      PFX-PROVIDE-FILES
      EMIT-SEAL-CAPTURE-TOKEN                      \ watermark token at the true engine-prefix end
      12 SP 8 LDR,  12 noseal CBNZ,
      SRC-SFAIL LABEL@ EMIT-SEAL-FRIEND-TOKEN      \ seal before any appended user source
   noseal LBL,
      30 SP 0 LDR,  SP SP 16 ADDI,  RET,
   skip LBL, ;

: EMIT-SOURCE ( -- )
   C-SOURCE-LABELS
   STDIN? @ IF EMIT-COLD-PREFIX-SHARED C-SOURCE-STDIN ELSE C-SOURCE-BAKED THEN ;

\ ---- control-flow JIT helpers ----
: C-EMIT-DROP-X12 ( -- )
   LBL {: done:label :}
   12 done CBZ,
      9 $910003FF LIT64,  14 12 10 LSLI,  9 9 14 ORR,  LCEMIT LABEL@ BL,
   done LBL, ;
s" c-emit-drop-x12" s" --" TRUST

: EMIT-CF-HELPERS ( -- )
   LBL LBL LBL LBL LBL LBL {: pisb pdone kno kyes kchk knf :}
   LCFPUSH LABEL@ LBL,
      5 CFSTK-OFF LIT64,  10 DBASE 5 ADD,  11 10 0 LDR,
      12 CF-REC MOVZ,  12 11 12 MUL,  12 12 10 ADD,  12 12 8 ADDI,
      9 12 0 STR,
      13 DATA LOCN-CELL LDR,  13 12 CF-LOCN STR,
      13 DATA LOCF-CELL LDR,  13 12 CF-LOCF STR,
      11 11 1 ADDI,  11 10 0 STR,  RET,
   LCFPOP LABEL@ LBL,
      SP SP 16 SUBI,  30 SP 0 STR,
      5 CFSTK-OFF LIT64,  10 DBASE 5 ADD,  11 10 0 LDR,  11 11 1 SUBI,  11 10 0 STR,
      12 CF-REC MOVZ,  12 11 12 MUL,  12 12 10 ADD,  12 12 8 ADDI,
      16 12 0 ADDI,
      15 DATA LOCF-CELL LDR,  14 16 CF-LOCF LDR,  12 15 14 SUB,
      C-EMIT-DROP-X12
      13 16 CF-LOCN LDR,  13 DATA LOCN-CELL STR,
      14 16 CF-LOCF LDR,  14 DATA LOCF-CELL STR,
      9 16 0 LDR,
      30 SP 0 LDR,  SP SP 16 ADDI,  RET,
   LPAT LABEL@ LBL,
      11 9 0 LDRW,  10 CP 9 SUB,  10 10 2 ASRI,
      5 $80000000 LIT64,  13 11 5 AND,
      13 pisb CBZ,
         5 $7FFFF LIT64,  10 10 5 AND,  10 10 5 LSLI,  pdone B,
      pisb LBL,  5 $3FFFFFF LIT64,  10 10 5 AND,
      pdone LBL,  11 11 10 ORR,  11 9 0 STRW,  RET,
   LKWCMP LABEL@ LBL,
      2 DATA TKL-CELL LDR,  2 1 CMP,  C-NE kno BCOND,
      2 0 MOVZ,  3 $20 MOVZ,
      kchk LBL,
         2 1 CMP,  C-GE kyes BCOND,
         4 DATA TKA-CELL LDR,  4 4 2 ADD,  4 4 0 LDRB,
         4 $41 CMPI,  C-LT knf BCOND,  4 $5A CMPI,  C-GT knf BCOND,  4 4 3 ORR,
         knf LBL,
         5 0 2 ADD,    5 5 0 LDRB,
         4 5 CMP,  C-NE kno BCOND,
         2 2 1 ADDI,  kchk B,
      kyes LBL,  0 1 MOVZ,  RET,
      kno  LBL,  0 0 MOVZ,  RET,
   LBCHAIN LABEL@ LBL,                                    \ patch a B-placeholder chain:
      LBL LBL {: bcl bcd :}                    \ x9=head offset, x14=target;
      bcl LBL,  9 bcd CBZ,                           \ clobbers x5,x10-x12
         10 DBASE 9 ADD,  11 10 0 LDRW,
         12 14 10 SUB,  12 12 2 ASRI,
         5 $3FFFFFF LIT64,  12 12 5 AND,
         5 $14000000 LIT64,  12 12 5 ORR,
         12 10 0 STRW,
         9 11 0 ADDI,  bcl B,
      bcd LBL,  RET, ;

: EMIT-LOC-FIND ( -- )
   LBL LBL LBL LBL LBL {: ll lmiss lhit lcmp lnext :}
   LLOC-FIND LABEL@ LBL,
   9 DATA LOCN-CELL LDR,  10 0 MOVZ,
   6 DATA TKL-CELL LDR,  7 DATA TKA-CELL LDR,
   ll LBL,  10 9 CMP,  C-GE lmiss BCOND,
      12 LOC-REC MOVZ,  11 10 12 MUL,  5 LOCNAMES LIT64,  11 11 5 ADD,  11 DATA 11 ADD,
      12 11 0 LDR,  12 6 CMP,  C-NE lnext BCOND,
      13 0 MOVZ,
      lcmp LBL,  13 6 CMP,  C-GE lhit BCOND,
         14 11 13 ADD,  14 14 8 ADDI,  14 14 0 LDRB,
         15 7 13 ADD,  15 15 0 LDRB,
         14 15 CMP,  C-NE lnext BCOND,
         13 13 1 ADDI,  lcmp B,
      lhit LBL,  0 10 0 ADDI,  RET,
      lnext LBL,  10 10 1 ADDI,  ll B,
   lmiss LBL,  0 0 MOVN,  RET, ;
\ keyword bytes (lower-case / literal) at known labels
create SQ-KW  115 c, 34 c,
create CQ-KW  99 c, 34 c,
create DOTQ-KW 46 c, 34 c,
create ESQ-KW  115 c, 92 c, 34 c,
create ECQ-KW  99 c, 92 c, 34 c,
create EDOTQ-KW 46 c, 92 c, 34 c,
create BCHAR-KW 91 c, 99 c, 104 c, 97 c, 114 c, 93 c,   \ [char]
create QUOT-KW 91 c, 58 c,      \ [:
create SEMIQ-KW 59 c, 93 c,     \ ;]
variable LREAD  variable LRBYE  variable LRDIE  variable LRREC  variable LQNL  variable LOKS
create QNL-KW 63 c, 10 c,
create OKS-KW 32 c, 111 c, 107 c, 10 c,
create BADTAG-SFX-KW 32 c, 116 c, 97 c, 103 c, 10 c,   \ " tag\n" for the MATCH bad-tag die
create TICK-KW   39 c,
create BTICK-KW  91 c, 39 c, 93 c,
create LBRACE-KW 123 c, 58 c,
create ENDLOC-KW 58 c, 125 c,
$4842444546455201 constant DEFER-MAGIC
variable LKWDEFER  variable LKWIS  variable LKWDEFERUNSET
variable LCHKDEFER  variable LSIGPTRA  variable LSIGA  variable LRECWPUB  variable LP2DOESW
\ ADT lowering keywords (TFAM 10, docs §16): `construct` dispatches through the
\ CMM-CELL mode machine (J-CONSTRUCT + EM-COMPILE-ADT-MODE, slice 2; execution
\ gate-pinned by GE-CONSTRUCT-EXEC). The MATCH rows are data-only until slice 3
\ wires the eliminator states — those tokens still fail closed as undefined.
variable LKWCONSTRUCT  variable LKWMATCH  variable LKWSEMIMATCH
variable LTFLCONFAM  variable LTFLCVAR   \ TFL lowering-surface bridge names (C-FIND-GLOBAL)
variable LTFLMATCHFAM  variable LTFLNAME \ MATCH bridge names: tfl-match-fam? / tfam-name$
variable LBADTAGPFX    variable LBADTAGSFX  \ bad-tag die message spans (C-DIE-BAD-TAG)
variable LRESTAB    \ sealed system-package name table (TFAM 2b-ii)
\ Sealed system-package names (TFAM 2b-ii). Records are [u8 len][len bytes] in
\ lowercase (CHECKER-FOLD-C canonical form), terminated by a 0-length record.
\ This ONE native table is the reserved-name set: the guards fold each candidate
\ byte and compare against it. It lives in the compiler (habu1/habu2 CHECK-OFF
\ region) rather than the checker because the guards must resolve it during the
\ sealed self-hosting stage build and checker-boot recompile, where a checker
\ word is neither reachably kept nor safely callable from mid C-QUALIFY-DEF.
create RESTAB-BUF
   4 c, $74 c, $66 c, $61 c, $6D c,               \ "tfam"
   4 c, $74 c, $79 c, $70 c, $65 c,               \ "type"
   5 c, $6D c, $61 c, $74 c, $63 c, $68 c,        \ "match"
  12 c, $63 c, $68 c, $65 c, $63 c, $6B c, $65 c, \ "checker-cert"
         $72 c, $2D c, $63 c, $65 c, $72 c, $74 c,
  10 c, $6C c, $6F c, $77 c, $65 c, $72 c, $2D c, \ "lower-cert"
         $63 c, $65 c, $72 c, $74 c,
  15 c, $6C c, $6F c, $77 c, $65 c, $72 c, $2D c, \ "lower-cert-hook"
         $63 c, $65 c, $72 c, $74 c, $2D c, $68 c, $6F c, $6F c, $6B c,
   0 c,                                           \ terminator
here RESTAB-BUF - constant RESTAB-LEN

: EMIT-KWDATA ( -- )
   LKWIF LABEL@ LBL,     s" if"     BYTES,    LKWTHEN LABEL@ LBL,   s" then"   BYTES,
   LKWELSE LABEL@ LBL,   s" else"   BYTES,    LKWBEGIN LABEL@ LBL,  s" begin"  BYTES,
   LKWUNTIL LABEL@ LBL,  s" until"  BYTES,    LKWAGAIN LABEL@ LBL,  s" again"  BYTES,
   LKWWHILE LABEL@ LBL,  s" while"  BYTES,    LKWREPEAT LABEL@ LBL, s" repeat" BYTES,
   LKWCASE LABEL@ LBL,   s" case"   BYTES,    LKWOF LABEL@ LBL,     s" of"     BYTES,
   LKWENDOF LABEL@ LBL,  s" endof"  BYTES,    LKWENDCASE LABEL@ LBL, s" endcase" BYTES,
   LKWCREATE LABEL@ LBL, s" create" BYTES,    LKWVAR LABEL@ LBL,    s" variable" BYTES,
   LKWSQ LABEL@ LBL,     SQ-KW 2 BYTES,
   LKWCQ LABEL@ LBL,     CQ-KW 2 BYTES,
   LKWDOTQ LABEL@ LBL,   DOTQ-KW 2 BYTES,
   LKWESQ LABEL@ LBL,    ESQ-KW 3 BYTES,
   LKWECQ LABEL@ LBL,    ECQ-KW 3 BYTES,
   LKWEDOTQ LABEL@ LBL,  EDOTQ-KW 3 BYTES,
   LKWTYPE LABEL@ LBL,   s" type" BYTES,
   LKWTICK LABEL@ LBL,   TICK-KW 1 BYTES,    LKWBTICK LABEL@ LBL,  BTICK-KW 3 BYTES,
   LKWLBRACE LABEL@ LBL, LBRACE-KW 2 BYTES,  LKWENDLOC LABEL@ LBL, ENDLOC-KW 2 BYTES,
   LKWCONST LABEL@ LBL,  s" constant" BYTES,
   LQNL LABEL@ LBL,  QNL-KW 2 BYTES,   LOKS LABEL@ LBL,  OKS-KW 4 BYTES,
   LKWDO LABEL@ LBL,  s" do" BYTES,    LKWLOOP LABEL@ LBL,  s" loop" BYTES,    LKWI LABEL@ LBL,  s" i" BYTES,
   LKWTOR LABEL@ LBL,  s" >r" BYTES,   LKWRFROM LABEL@ LBL,  s" r>" BYTES,   LKWRFET LABEL@ LBL,  s" r@" BYTES,
   LKWEXIT LABEL@ LBL,  s" exit" BYTES,   LKWREC LABEL@ LBL,  s" recurse" BYTES,
   LKWQDO LABEL@ LBL,  s" ?do" BYTES,   LKWPLOOP LABEL@ LBL,  s" +loop" BYTES,   LKWJ LABEL@ LBL,  s" j" BYTES,
   LKWLEAVE LABEL@ LBL,  s" leave" BYTES,   LKWUNLOOP LABEL@ LBL,  s" unloop" BYTES,
   LKWCHAR LABEL@ LBL,  s" char" BYTES,   LKWBCHAR LABEL@ LBL,  BCHAR-KW 6 BYTES,
   LKWIMM LABEL@ LBL,  s" immediate" BYTES,   LKWPOST LABEL@ LBL,  s" postpone" BYTES,
   LKWCOMPC LABEL@ LBL,  s" compile," BYTES,
   LKWDOES LABEL@ LBL,  s" does>" BYTES,
   LKWTRUSTED LABEL@ LBL, s" trusted:" BYTES,
   LKWKERNEL LABEL@ LBL, s" kernel:" BYTES,
   LKWTRUST LABEL@ LBL, s" trust" BYTES,      LKWCHKDOES LABEL@ LBL, s" check-does!" BYTES,  LKWPACKAGE LABEL@ LBL, s" package" BYTES,  LKWPUBLIC LABEL@ LBL, s" public" BYTES,
   LKWPRIVATE LABEL@ LBL, s" private" BYTES,  LKWENDPACKAGE LABEL@ LBL, s" end-package" BYTES,  LKWDUPDEF LABEL@ LBL, s" duplicate definition: " BYTES,  LKWQUOT LABEL@ LBL,  QUOT-KW 2 BYTES,   LKWSEMIQ LABEL@ LBL,  SEMIQ-KW 2 BYTES,  LKWDEFER LABEL@ LBL, s" defer" BYTES,  LKWIS LABEL@ LBL, s" is" BYTES,  LKWDEFERUNSET LABEL@ LBL, s" defer-unset" BYTES,  LCHKPACKAGE LABEL@ LBL, s" checker-package" BYTES,  LCHKPUB LABEL@ LBL, s" checker-public" BYTES,  LCHKPRI LABEL@ LBL, s" checker-private" BYTES,  LCHKENDPKG LABEL@ LBL, s" checker-end-package" BYTES,  LCHKDEFER LABEL@ LBL, s" checker-defer" BYTES,  LRESTAB LABEL@ LBL, RESTAB-BUF RESTAB-LEN BYTES,  LSIGPTRA LABEL@ LBL, s" -- ptr a" BYTES,  LSIGA LABEL@ LBL, s" -- a" BYTES,  LRECWPUB LABEL@ LBL, s" rec-wide-publish" BYTES,  LP2DOESW LABEL@ LBL, s" hb: does>-split cannot lower layout width facts: " BYTES,
   LKWEXPORT LABEL@ LBL, s" export" BYTES,  LCHKEXPORT LABEL@ LBL, s" checker-export" BYTES,
   LKWCONSTRUCT LABEL@ LBL, s" construct" BYTES,  LKWMATCH LABEL@ LBL, s" match" BYTES,  LKWSEMIMATCH LABEL@ LBL, s" ;match" BYTES,
   LTFLCONFAM LABEL@ LBL, s" tfl-con-fam?" BYTES,  LTFLCVAR LABEL@ LBL, s" tfl-cvar?" BYTES,
   LTFLMATCHFAM LABEL@ LBL, s" tfl-match-fam?" BYTES,  LTFLNAME LABEL@ LBL, s" tfam-name$" BYTES,
   LBADTAGPFX LABEL@ LBL, s" hb: bad " BYTES,  LBADTAGSFX LABEL@ LBL, BADTAG-SFX-KW 5 BYTES,
   PFX-PATH-FILES ;

\ ---- compile-time keyword handlers (append JIT-emitter code at BUILD time) ----
: C-EMITW ( n -- ) {: w:n :}  9 w LIT64,  LCEMIT LABEL@ BL, ;

: C-POPFLAG ( -- )  $D1002273 C-EMITW  $F9400269 C-EMITW ;

: C-POP-X16 ( -- )  $D1002273 C-EMITW  $F9400270 C-EMITW ;

: C-PUSHCP ( -- )   9 CP 0 ADDI,  LCFPUSH LABEL@ BL, ;

: C-BBACK ( n n -- ) {: opc mask :}
   10 9 CP SUB,  10 10 2 ASRI,  5 mask LIT64,  10 10 5 AND,  9 opc LIT64,  9 9 10 ORR,  LCEMIT LABEL@ BL, ;

: J-IF ( -- )    C-POPFLAG  C-PUSHCP  $B4000009 C-EMITW ;

: J-THEN ( -- )  LCFPOP LABEL@ BL,  LPAT LABEL@ BL, ;

: J-ELSE ( -- )
   LCFPOP LABEL@ BL,
   14 9 0 ADDI,
   C-PUSHCP
   $14000000 C-EMITW
   9 14 0 ADDI,
   LPAT LABEL@ BL, ;

: J-CASE ( -- )
   9 0 MOVZ,  LCFPUSH LABEL@ BL, ;

\ construct keyword (TFAM 10 slice 2): arm the operand capture (CMM=1). CFN
\ entry — no spill: construct only ADDS VS constants on top, exactly like the
\ literal tokens of a generated-constructor body; tracked payload cells stay
\ wherever the VS has them.
: J-CONSTRUCT ( -- )
   12 1 MOVZ,  12 DATA CMM-CELL STR, ;

\ match keyword (TFAM 10 slice 3, docs §16): CF-ENTRY spills the width-expanded
\ scrutinee bundle to the physical stack first, then J-MATCH pushes a CF sentinel
\ (x9=0, J-CASE shape, so ;MATCH's J-ENDCASE-style join loop stops at it), opens a
\ match-frame nesting level (the family id is filled at the family token), and
\ arms CMM=3 (want family). No emission — the bundle is already spilled and each
\ OF peeks the tag in place.
: J-MATCH ( -- )
   9 0 MOVZ,  LCFPUSH LABEL@ BL,
   9 DATA CMFRD-CELL LDR,  9 9 1 ADDI,  9 DATA CMFRD-CELL STR,
   12 3 MOVZ,  12 DATA CMM-CELL STR, ;

\ J-OF pushes a CASE-arm marker (bit 0) onto the CMBK branch-kind bitstack so the
\ shared J-ENDOF can tell a CASE arm from a MATCH variant branch (which pushes a 1
\ in EM-ADT-MATCH-OF). The emitted CASE compare/branch code is unchanged.
: J-OF ( -- )
   14 DATA CMBK-CELL LDR,  14 14 1 LSLI,  14 DATA CMBK-CELL STR,
   C-POP-X16
   $F85F8269 C-EMITW
   $EB10013F C-EMITW
   $9A9F17E9 C-EMITW
   C-PUSHCP
   $B4000009 C-EMITW
   $D1002273 C-EMITW ;

\ J-ENDOF: identical branch-placeholder codegen for CASE and MATCH (= J-ELSE);
\ then pop the CMBK branch-kind bit — a MATCH variant branch (1) re-arms the token
\ machine to CMM=4 (want variant-or-;match), a CASE arm or nested case/match ENDOF
\ (0) leaves CMM untouched. This is the compiler analogue of the checker's
\ CF-ENDOF-DISPATCH frame-kind routing, with no per-frame kind on the CF stack.
: J-ENDOF ( -- )
   LBL {: nm:label :}
   J-ELSE
   14 DATA CMBK-CELL LDR,  15 14 1 ANDI,  14 14 1 LSRI,  14 DATA CMBK-CELL STR,
   15 nm CBZ,  12 4 MOVZ,  12 DATA CMM-CELL STR,  nm LBL, ;

: J-ENDCASE ( -- )
   LBL LBL {: cloop:label done:label :}
   $D1002273 C-EMITW
   cloop LBL,
      LCFPOP LABEL@ BL,
      9 done CBZ,
      LPAT LABEL@ BL,
      cloop B,
   done LBL, ;

\ BEGIN loops are register-resident: J-BEGIN snapshots the VS into registers
\ (Lvsnap), the back edges reconcile to that snapshot (Lvrecon) and branch on
\ x17 — never a VS register, so the reconcile reload can't clobber the flag.
: J-BEGIN ( -- )  LVSNAP LABEL@ BL,  C-PUSHCP ;

: J-AGAIN ( -- )  LVRECON LABEL@ BL,  LCFPOP LABEL@ BL,  $14000000 $3FFFFFF C-BBACK ;

: J-UNTILX ( -- )                                 \ shared tail: reconcile + cbz x17,top
   LVRECON LABEL@ BL,
   LCFPOP LABEL@ BL,
   10 9 CP SUB,  10 10 2 ASRI,  5 $7FFFF LIT64,  10 10 5 AND,  10 10 5 LSLI,
   9 $B4000011 LIT64,  9 9 10 ORR,  LCEMIT LABEL@ BL, ;

: J-UNTIL ( -- )  $D1002273 C-EMITW  $F9400271 C-EMITW  J-UNTILX ;   \ pop flag -> x17

: J-WHILE ( -- ) C-POPFLAG  C-PUSHCP  $B4000009 C-EMITW ;

: J-REPEAT ( -- ) LVRECON LABEL@ BL,  LCFPOP LABEL@ BL,
   SP SP 16 SUBI,  9 SP 0 STR,  14 SP 8 STR,
   LCFPOP LABEL@ BL,  $14000000 $3FFFFFF C-BBACK
   12 0 MOVZ,  12 DATA VSP-CELL STR,                  \ exit path arrives from
   12 VRALL MOVZ,  12 DATA VRFREE-CELL STR,           \ WHILE's spilled state
   12 FRALL MOVZ,  12 DATA FRFREE-CELL STR,
   9 SP 0 LDR,  LPAT LABEL@ BL,
   14 SP 8 LDR,  15 DATA LOCF-CELL LDR,  12 14 15 SUB,  C-EMIT-DROP-X12
   SP SP 16 ADDI, ;

: J-FRAME ( -- )                                \ pop limit/start, push a loop frame
   3506446963 C-EMITW  4181721705 C-EMITW  3506446963 C-EMITW  4181721706 C-EMITW
   4181780107 C-EMITW  3548179820 C-EMITW  2434269580 C-EMITW  2333344140 C-EMITW
   4177527177 C-EMITW  4177528202 C-EMITW  2432697707 C-EMITW  4177585803 C-EMITW ;

: J-LVOPEN ( -- )                               \ open a LEAVE-chain level: LVH[LVD]=0, LVD++
   9 DATA LVD-CELL LDR,
   10 9 3 LSLI,  10 10 LVH-OFF ADDI,  10 DATA 10 ADD,
   12 0 MOVZ,  12 10 0 STR,
   10 9 3 LSLI,  10 10 LVF-OFF ADDI,  10 DATA 10 ADD,
   12 DATA LOCF-CELL LDR,  12 10 0 STR,
   9 9 1 ADDI,  9 DATA LVD-CELL STR, ;

: J-LVLEAVE ( -- )                              \ chain a B placeholder on the current level
   9 DATA LVD-CELL LDR,  9 9 1 SUBI,
   10 9 3 LSLI,  10 10 LVF-OFF ADDI,  10 DATA 10 ADD,
   14 10 0 LDR,  15 DATA LOCF-CELL LDR,  12 15 14 SUB,  C-EMIT-DROP-X12
   9 DATA LVD-CELL LDR,  9 9 1 SUBI,
   10 9 3 LSLI,  10 10 LVH-OFF ADDI,  10 DATA 10 ADD,
   9 10 0 LDR,
   11 CP DBASE SUB,  11 10 0 STR,
   LCEMIT LABEL@ BL, ;

: J-DO ( -- )
   J-FRAME  J-LVOPEN  C-PUSHCP ;

: J-?DO ( -- )                                  \ DO, but skip the loop when limit = start
   J-FRAME  J-LVOPEN
   $EB0A013F C-EMITW                     \ cmp x9,x10  (start/limit still live)
   $54000041 C-EMITW                     \ b.ne +8 (over the skip placeholder)
   J-LVLEAVE
   C-PUSHCP ;

: J-LEAVE ( -- )  J-LVLEAVE ;

: J-UNLOOP ( -- )                               \ pop one loop frame, no branch
   4181780107 C-EMITW  3506439531 C-EMITW  4177585803 C-EMITW ;

: J-LOOPEND ( -- )                              \ shared LOOP/+LOOP tail: pop frame, patch
   14 CP 0 ADDI,                         \ LEAVE/?DO skips to the pop point, LVD--
   4181780107 C-EMITW  3506439531 C-EMITW  4177585803 C-EMITW
   9 DATA LVD-CELL LDR,  9 9 1 SUBI,  9 DATA LVD-CELL STR,
   10 9 3 LSLI,  10 10 LVH-OFF ADDI,  10 DATA 10 ADD,  9 10 0 LDR,
   LBCHAIN LABEL@ BL, ;

: J-LOOP ( -- )
   4181780107 C-EMITW  3506439531 C-EMITW  3548179820 C-EMITW  2434269580 C-EMITW  2333344140 C-EMITW
   4181721481 C-EMITW  4181722506 C-EMITW  2432697641 C-EMITW  4177527177 C-EMITW  3943301439 C-EMITW
   LCFPOP LABEL@ BL,
   10 9 CP SUB,  10 10 2 ASRI,  5 $7FFFF LIT64,  10 10 5 AND,  10 10 5 LSLI,
   9 $5400000B LIT64,  9 9 10 ORR,  LCEMIT LABEL@ BL,
   J-LOOPEND ;

: J-+LOOP ( -- )                                \ index += n; loop while (old-limit) and
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
   LCFPOP LABEL@ BL,
   10 9 CP SUB,  10 10 2 ASRI,  5 $7FFFF LIT64,  10 10 5 AND,  10 10 5 LSLI,
   9 $5400000A LIT64,  9 9 10 ORR,  LCEMIT LABEL@ BL,       \ b.ge loop-top
   J-LOOPEND ;

: J-I ( -- )
   4181780107 C-EMITW  3506439531 C-EMITW  3548179820 C-EMITW  2434269580 C-EMITW  2333344140 C-EMITW
   4181721481 C-EMITW  4177527401 C-EMITW  2432705139 C-EMITW ;

: J-J ( -- )                                    \ outer loop index: frame[LOOPSP-2]
   4181780107 C-EMITW  $D100096B C-EMITW 3548179820 C-EMITW  2434269580 C-EMITW  2333344140 C-EMITW
   4181721481 C-EMITW  4177527401 C-EMITW  2432705139 C-EMITW ;

\ >R R> R@ — the user return stack lives in a data-region stack ([x20+RSTK-OFF],
\ depth at [x20+RSP-CELL]), like the DO/LOOP frames: x25/x28 belong to the
\ compiler, and word frames on the machine stack would unbalance the epilogue.
: W-LDRX ( n n n -- n ) {: rt RN off :}                               \ ldr rt,[rn,#off]
   $F9400000  off 8 / 10 lshift or  RN 5 lshift or  rt or ;

: W-STRX ( n n n -- n ) {: rt RN off :}                               \ str rt,[rn,#off]
   $F9000000  off 8 / 10 lshift or  RN 5 lshift or  rt or ;

: C-FIND-TRUST ( -- )  LBL {: ok :}
   9 LKWTRUST LABEL@ ADR,  10 5 MOVZ,  LFIND LABEL@ BL,
   13 ok CBNZ,
      0 2 MOVZ,  1 LKWTRUST LABEL@ ADR,  2 5 MOVZ,  NR-WRITE SYS,
      0 70 MOVZ,  NR-EXIT-GROUP SYS,
   ok LBL, ;

: C-TASK-LIVE-GUARD ( -- )
   LBL {: ok:label :}
   9 DATA TASKS-LIVE-CELL LDR,  9 ok CBZ,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 $4F MOVZ,  NR-EXIT-GROUP SYS,
   ok LBL, ;
s" c-task-live-guard" s" --" TRUST

\ C-PUSH-DREC-NAME: push the definition's ORIGINAL qualified spelling for the
\ engine->checker record calls (trust/defer rows). It is the FIRST TOKEN of the
\ body buffer -- LBCAP seeds "NAME " there before C-QUALIFY-DEF rewrites the
\ token, and that is byte-for-byte the spelling the CHECK cert path reads as NMA.
\ Two properties matter: (1) it is a fixed engine DATA buffer, so it survives a
\ multi-line body, unlike the raw source pointer DEF-TKA (stale once the source
\ reader refills); (2) it is the QUALIFIED `PKG:tail`, unlike the published bare
\ dictionary-record tail. CHECKER-RECORD-SYM then records a qualified def under
\ ( pkg, tail ) PUBLIC -- matching the certify record -- instead of leaking a
\ bare-global `tail` effect row that shadowed core prims and certified bare-tail
\ calls the engine rejects (dot habu-qualified-defs-leak-aadeb5c9). Unqualified
\ defs are unchanged (first token == whole name). The Gforth stage0 mirror keeps
\ the record-name form: it has no package system, so its name cannot diverge.
\ Scratch stays off x11: C-FIND-TRUST/C-FIND-GLOBAL leave the record XT in x11
\ for the caller's later C-CALL-X11-SAVED, so this word must preserve it.
: C-PUSH-DREC-NAME ( -- )
   LBL LBL {: scan:label done:label :}
   9 DATA BODYBUF-OFF ADDI,             \ x9 = name start (body buffer base)
   10 0 MOVZ,                           \ x10 = name length
   12 DATA BODYLEN-CELL LDR,            \ x12 = body length bound (fail-closed)
   scan LBL,
      10 12 CMP,  C-GE done BCOND,      \ hit body end without a space -> stop
      13 9 10 ADD,  13 13 0 LDRB,       \ x13 = name[x10]
      13 $20 CMPI,  C-EQ done BCOND,    \ the seeded trailing space ends the name
      13 done CBZ,                      \ NUL also ends it (safety)
      10 10 1 ADDI,  scan B,
   done LBL,
   9 G-PUSH
   10 G-PUSH ;

: C-PUSH-DATA-CELL ( n -- ) {: off :}
   9 DATA off LDR,  9 G-PUSH ;

: C-PUSH-TRUST-SIG ( n n -- ) {: aoff uoff :}
   aoff C-PUSH-DATA-CELL
   uoff C-PUSH-DATA-CELL ;

: C-CALL-X11-SAVED ( -- )
   SP SP 16 SUBI,  30 SP 0 STR,
   11 BLR,
   30 SP 0 LDR,  SP SP 16 ADDI, ;

: C-CALL-TRUST-PEND ( -- )
   C-FIND-TRUST
   C-PUSH-DREC-NAME
   TSIG-A-CELL TSIG-U-CELL C-PUSH-TRUST-SIG
   C-CALL-X11-SAVED ;

: C-CALL-TRUST-LASTC ( -- )
   LBL {: nohook:label :}
   9 DATA HOOK-CELL LDR,  9 nohook CBZ,
   C-FIND-TRUST
   C-PUSH-DREC-NAME
   CRSIG-A-CELL CRSIG-U-CELL C-PUSH-TRUST-SIG
   C-CALL-X11-SAVED
   nohook LBL, ;

: C-CALL-TRUST-LASTC-PTR-A ( -- )
   LBL {: nohook:label :}
   9 DATA HOOK-CELL LDR,  9 nohook CBZ,
   C-FIND-TRUST
   C-PUSH-DREC-NAME
   9 LSIGPTRA LABEL@ ADR,  9 G-PUSH
   9 8 MOVZ,  9 G-PUSH
   C-CALL-X11-SAVED
   nohook LBL, ;

: C-CALL-TRUST-LASTC-A ( -- )
   LBL {: nohook:label :}
   9 DATA HOOK-CELL LDR,  9 nohook CBZ,
   C-FIND-TRUST
   C-PUSH-DREC-NAME
   9 LSIGA LABEL@ ADR,  9 G-PUSH
   9 4 MOVZ,  9 G-PUSH
   C-CALL-X11-SAVED
   nohook LBL, ;

: C-FIND-GLOBAL ( ptr n n -- ) {: name:ptr len:n :}
   LBL {: ok:label :}
   SP SP 16 SUBI,
   14 DATA PKG-PUB-CELL LDR,  14 SP 0 STR,
   14 DATA PKG-PRI-CELL LDR,  14 SP 8 STR,
   14 0 MOVZ,  14 DATA PKG-PUB-CELL STR,  14 DATA PKG-PRI-CELL STR,
   9 name LABEL@ ADR,  10 len MOVZ,  LFIND LABEL@ BL,
   14 SP 0 LDR,  14 DATA PKG-PUB-CELL STR,
   14 SP 8 LDR,  14 DATA PKG-PRI-CELL STR,
   SP SP 16 ADDI,
   13 ok CBNZ,
      0 2 MOVZ,  1 name LABEL@ ADR,  2 len MOVZ,  NR-WRITE SYS,
      0 70 MOVZ,  NR-EXIT-GROUP SYS,
   ok LBL, ;
s" c-find-global" s" ptr n n --" TRUST

: C-CALL-CHECKER-DEFER ( -- )
   LCHKDEFER 13 C-FIND-GLOBAL
   C-PUSH-DREC-NAME
   C-CALL-X11-SAVED ;
s" c-call-checker-defer" s" --" TRUST

\ Interpret-gate marking (habu-tfam-12-interpret checker half): after ndict++
\ the checker's rec-wide-publish consumes the RECW latch its record choke
\ point stored for THIS definition's effect and, when wide, `wide-mark`s the
\ just-published record (ndict-1) DNAME-WIDE. Hook-guarded: during the engine
\ prefix self-load (no checker hook) nothing marks — the prefix has no
\ wide-effect rows (verified) and user source only loads post-hook.
: EM-REC-WIDE-PUBLISH ( -- )
   LBL {: nohook:label :}
   9 DATA HOOK-CELL LDR,  9 nohook CBZ,
   LRECWPUB 16 C-FIND-GLOBAL
   C-CALL-X11-SAVED
   nohook LBL, ;

: C-DIE-DOES ( -- )
   0 2 MOVZ,  1 LKWDOES LABEL@ ADR,  2 5 MOVZ,  NR-WRITE SYS,
   0 70 MOVZ,  NR-EXIT-GROUP SYS, ;

: C-CALL-CHECK-DOES ( -- )
   LBL LBL {: found good :}
   9 LKWCHKDOES LABEL@ ADR,  10 11 MOVZ,  LFIND LABEL@ BL,
   13 found CBNZ,
      0 2 MOVZ,  1 LKWCHKDOES LABEL@ ADR,  2 11 MOVZ,  NR-WRITE SYS,
      0 70 MOVZ,  NR-EXIT-GROUP SYS,
   found LBL,
   9 DATA BODYBUF-OFF ADDI,
   10 DATA DOESB-CELL LDR,
   9 9 10 ADD,  9 G-PUSH
   12 DATA BODYLEN-CELL LDR,  12 12 10 SUB,  12 G-PUSH
   9 DATA TCSIG-A-CELL LDR,  9 G-PUSH
   9 DATA TCSIG-U-CELL LDR,  9 G-PUSH
   SP SP 16 SUBI,  30 SP 0 STR,  11 BLR,  30 SP 0 LDR,  SP SP 16 ADDI,
   10 G-POP  11 0 MOVN,  10 11 CMP,  C-EQ good BCOND,
      C-DIE-DOES
   good LBL, ;

: C-CALL-CHECK-DEFINER ( -- )
   LBL LBL LBL LBL {: nohook fulllen lenok good :}
   9 DATA HOOK-CELL LDR,  9 nohook CBZ,
   10 DATA BODYBUF-OFF ADDI,  10 G-PUSH
   10 DATA DOESB-CELL LDR,  10 fulllen CBZ,
      10 10 6 SUBI,  lenok B,
   fulllen LBL,
      10 DATA BODYLEN-CELL LDR,
   lenok LBL,
   10 G-PUSH
   9 DATA HOOK-CELL LDR,
   SP SP 16 SUBI,  30 SP 0 STR,  9 BLR,  30 SP 0 LDR,  SP SP 16 ADDI,
   10 G-POP  10 good CBNZ,
      C-DIE-DOES
   good LBL,
   nohook LBL, ;

: C-EMIT-DATA-X9! ( n -- ) {: off :}
   9 20 off W-STRX C-EMITW ;

: C-EMIT-CRSIG-PART! ( n n -- ) {: src dst :}
   11 DATA src LDR,  C-X9-LIT
   dst C-EMIT-DATA-X9! ;

: C-EMIT-CRSIG-A! ( -- )
   TCSIG-A-CELL CRSIG-A-CELL C-EMIT-CRSIG-PART! ;

: C-EMIT-CRSIG-U! ( -- )
   TCSIG-U-CELL CRSIG-U-CELL C-EMIT-CRSIG-PART! ;

: C-EMIT-CRSIG-SET ( -- )
   LBL {: none :}
   9 DATA TCSIG-U-CELL LDR,  9 none CBZ,
      C-EMIT-CRSIG-A!
      C-EMIT-CRSIG-U!
   none LBL, ;

: C-RUNTIME-CRSIG-CLEAR ( -- )
   9 0 MOVZ,
   9 DATA CRSIG-A-CELL STR,
   9 DATA CRSIG-U-CELL STR, ;

: J-TOR ( -- )                                                \ pop data -> push RSTK
   $D1002273 C-EMITW  $F9400269 C-EMITW                \ sub x19,#8 ; ldr x9,[x19]
   10 20 RSP-CELL W-LDRX C-EMITW
   $8B0A0E8B C-EMITW                                   \ add x11,x20,x10,lsl#3
   9 11 RSTK-OFF W-STRX C-EMITW
   $9100054A C-EMITW                                   \ add x10,x10,#1
   10 20 RSP-CELL W-STRX C-EMITW ;

: J-RPOP ( -- )                                               \ x9 = RSTK top, x10 = RSP-1
   10 20 RSP-CELL W-LDRX C-EMITW
   $D100054A C-EMITW                                   \ sub x10,x10,#1
   $8B0A0E8B C-EMITW                                   \ add x11,x20,x10,lsl#3
   9 11 RSTK-OFF W-LDRX C-EMITW ;

: J-RFROM ( -- )  J-RPOP                                      \ pop RSTK -> push data
   10 20 RSP-CELL W-STRX C-EMITW
   $F9000269 C-EMITW  $91002273 C-EMITW ;              \ str x9,[x19] ; add x19,#8

: J-RFETCH ( -- )  J-RPOP                                     \ peek RSTK -> push data
   $F9000269 C-EMITW  $91002273 C-EMITW ;

\ EXIT: emit a placeholder word holding the PREVIOUS chain offset (0 = end);
\ `;` walks the chain and patches each into `b epilogue`. RECURSE: bl back to
\ the current word's entry (PEND slot.addr) — every word has the standard
\ prologue/epilogue, so calling into the open definition is well-formed.
: J-EXIT ( -- )
   LBL {: qexit:label :}
   9 DATA QPATCH-CELL LDR,  9 qexit CBNZ,
      12 DATA LOCF-CELL LDR,  C-EMIT-DROP-X12
   qexit LBL,
   9 DATA EXITH-CELL LDR,                              \ x9 = prev chain offset
   10 CP DBASE SUB,  10 DATA EXITH-CELL STR,           \ head := this placeholder
   LCEMIT LABEL@ BL, ;

: J-RECURSE ( -- )
   9 DATA PEND-CELL LDR,  9 9 0 LDR,  $94000000 $3FFFFFF C-BBACK ;   \ bl entry

: C-SIG-START ( label -- ) {: lmiss:label :}
   LBL LBL {: ws got :}
   11 DATA INP-CELL LDR,  12 DATA INE-CELL LDR,
   ws LBL,  11 12 CMP,  C-GE lmiss BCOND,
      13 11 0 LDRB,  13 32 CMPI,  C-HI got BCOND,
      11 11 1 ADDI,  ws B,
   got LBL,  13 40 CMPI,  C-NE lmiss BCOND,
   14 11 0 ADDI,  15 11 0 ADDI, ;

: C-SIG-END ( label -- ) {: lmiss:label :}
   LBL {: scan :}
   scan LBL,  15 12 CMP,  C-GE lmiss BCOND,
      13 15 0 LDRB,  15 15 1 ADDI,  13 41 CMPI,  C-NE scan BCOND, ;

: C-SIG-INNER$ ( -- )
   11 14 1 ADDI,  12 15 14 SUB,  12 12 2 SUBI, ;

: C-SIG-FULL$ ( -- )
   11 14 0 ADDI,  12 15 14 SUB, ;

: C-SIG-CAPTURE-TSIG ( -- )
   15 DATA INP-CELL STR,
   C-SIG-INNER$
   11 DATA TSIG-A-CELL STR,  12 DATA TSIG-U-CELL STR,
   C-SIG-FULL$  LBCS LABEL@ BL, ;

: C-SIG-BAD ( -- )
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 76 MOVZ,  NR-EXIT-GROUP SYS, ;

: C-PARSE-CREATED-SIG ( -- )
   LBL LBL LBL LBL {: cpy cpd done bad :}
   bad C-SIG-START
   bad C-SIG-END
   15 DATA INP-CELL STR,
   C-SIG-INNER$
   10 12 0 ADDI,
   12 DATA 0 LDR,  15 12 0 ADDI,
   14 12 10 ADD,  14 DP-CHECK
   9 10 0 ADDI,
   cpy LBL,  9 cpd CBZ,
      13 11 0 LDRB,  13 12 0 STRB,
      12 12 1 ADDI,  11 11 1 ADDI,  9 9 1 SUBI,  cpy B,
   cpd LBL,
   12 DATA 0 STR,
   15 DATA TCSIG-A-CELL STR,  10 DATA TCSIG-U-CELL STR,
   done B,
   bad LBL,  C-SIG-BAD
   done LBL, ;

: J-DOES ( -- )
   LBL {: dok :}
   12 DATA LOCF-CELL LDR,  12 dok CBZ,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 75 MOVZ,  NR-EXIT-GROUP SYS,
   dok LBL,
   9 DATA BODYLEN-CELL LDR,  9 DATA DOESB-CELL STR,
   C-PARSE-CREATED-SIG
   C-EMIT-CRSIG-SET
   $1000008A C-EMITW                     \ adr x10, #+16 = D (4 words ahead)
   16 20 DOESP-CELL W-LDRX C-EMITW       \ x16 = LDOESPATCH runtime addr
   $D63F0200 C-EMITW                     \ blr x16
   J-EXIT                                \ word 4: the defining word ends here
   9 $D10043FF LIT64,  LCEMIT LABEL@ BL,      \ D: fresh prologue for the does-body
   9 $F90003FE LIT64,  LCEMIT LABEL@ BL, ;

: J-QUOT ( -- )
   LBL {: qok :}
   9 DATA QPATCH-CELL LDR,  9 qok CBZ,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 75 MOVZ,  NR-EXIT-GROUP SYS,
   qok LBL,
   9 CP 0 ADDI,  9 DATA QPATCH-CELL STR,
   9 $14000000 LIT64,  LCEMIT LABEL@ BL,               \ b-over placeholder
   9 CP 0 ADDI,  9 DATA QENT-CELL STR,            \ the quotation's entry
   9 DATA EXITH-CELL LDR,  9 DATA QXH-CELL STR,   \ scope the EXIT chain
   12 0 MOVZ,  12 DATA EXITH-CELL STR,
   9 $D10043FF LIT64,  LCEMIT LABEL@ BL,               \ its own prologue
   9 $F90003FE LIT64,  LCEMIT LABEL@ BL, ;

: J-SEMIQUOT ( -- )
   LBL {: sqok :}
   9 DATA QPATCH-CELL LDR,  9 sqok CBNZ,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 75 MOVZ,  NR-EXIT-GROUP SYS,
   sqok LBL,
   14 CP 0 ADDI,  9 DATA EXITH-CELL LDR,  LBCHAIN LABEL@ BL,   \ exits -> this epilogue
   9 DATA QXH-CELL LDR,  9 DATA EXITH-CELL STR,
   9 $F94003FE LIT64,  LCEMIT LABEL@ BL,                \ epilogue: ldr x30,[sp]
   9 $910043FF LIT64,  LCEMIT LABEL@ BL,                \ add sp,#16
   9 W-RET LIT64,  LCEMIT LABEL@ BL,
   9 DATA QPATCH-CELL LDR,  LPAT LABEL@ BL,             \ b-over lands here
   11 DATA QENT-CELL LDR,  C-LIT                   \ push the xt in the outer word
   12 0 MOVZ,  12 DATA QPATCH-CELL STR, ;

: EMIT-DOESPATCH ( -- )
   LBL {: nocr :}
   LDOESPATCH LABEL@ LBL,
   SP SP 32 SUBI,  30 SP 0 STR,  10 SP 8 STR,
   2 3 MOVZ,  LPROT LABEL@ BL,                                \ region -> RW
   10 SP 8 LDR,
   11 DATA LASTC-CELL LDR,                               \ created slot
   12 11 0 LDR,  13 11 8 LDR,  12 12 13 ADD,             \ x12 = RET addr
   14 10 12 SUB,  14 14 2 ASRI,                          \ delta words (negative)
   5 $3FFFFFF LIT64,  14 14 5 AND,
   5 $14000000 LIT64,  14 14 5 ORR,                      \ b D
   14 12 0 STRW,
   12 SP 16 STR,
   2 5 MOVZ,  LPROT LABEL@ BL,                                \ region -> RX
   12 SP 16 LDR,
   12 DCCVAU,  DSB-ISH,  12 ICIVAU,  DSB-ISH,  ISB,      \ flush the patched line
   9 DATA CRSIG-U-CELL LDR,  9 nocr CBZ,
      C-CALL-TRUST-LASTC
      C-RUNTIME-CRSIG-CLEAR
   nocr LBL,
   30 SP 0 LDR,  SP SP 32 ADDI,  RET, ;

\ ---- interpret-mode defining words ----
\ record defining words for the checker: append the kind token + run the hook
\ (verdict ignored — create/variable/constant always publish).
: C-DEFHOOK ( ptr u8 n -- )  LBL {: kwv klen nohk :}
   11 kwv LABEL@ ADR,  12 klen MOVZ,  LBCS LABEL@ BL,
   9 DATA HOOK-CELL LDR,  9 nohk CBZ,
   10 DATA BODYBUF-OFF ADDI,  10 G-PUSH
   10 DATA BODYLEN-CELL LDR,  10 G-PUSH
   SP SP 16 SUBI,  30 SP 0 STR,  9 BLR,  30 SP 0 LDR,  SP SP 16 ADDI,
   10 G-POP
   nohk LBL, ;

: C-STORE-NAME ( -- )
   LBL LBL LBL LBL LBL LBL LBL LBL {: short fail capok lcopy lcd scopy scd done :}
   12 DATA TKL-CELL LDR,
   13 12 0 ADDI,
   12 DNAME-INL CMPI,  C-LE short BCOND,
      14 DNAME-EXT LIT64,  13 13 14 ORR,  13 9 16 STR,
      15 12 3 ADDI,  15 15 2 LSRI,  15 15 2 LSLI,
      16 CP 15 ADD,
      10 REGION $4000 - LIT64,  10 DBASE 10 ADD,  16 10 CMP,  C-LT capok BCOND,
         fail B,
      capok LBL,
      CP 9 24 STR,
      10 DATA TKA-CELL LDR,
      11 CP 0 ADDI,
      14 12 0 ADDI,
      lcopy LBL,  14 lcd CBZ,
         15 10 0 LDRB,  15 11 0 STRB,
         10 10 1 ADDI,  11 11 1 ADDI,  14 14 1 SUBI,  lcopy B,
      lcd LBL,
      CP 16 0 ADDI,
      done B,
   short LBL,
      13 9 16 STR,
      11 9 24 ADDI,  10 DATA TKA-CELL LDR,  14 12 0 ADDI,
      scopy LBL,  14 scd CBZ,
         15 10 0 LDRB,  15 11 0 STRB,
         10 10 1 ADDI,  11 11 1 ADDI,  14 14 1 SUBI,  scopy B,
      scd LBL,
      done B,
   fail LBL,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 76 MOVZ,  NR-EXIT-GROUP SYS,
   done LBL, ;

: C-QUALIFY-FAIL ( n -- ) {: rc:n :}
   0 2 MOVZ,  1 DATA DEF-TKA-CELL LDR,  2 DATA DEF-TKL-CELL LDR,  NR-WRITE SYS,
   0 2 MOVZ,  1 LQNL LABEL@ ADR,  1 1 1 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
   0 rc MOVZ,  NR-EXIT-GROUP SYS, ;

\ Capacity exits (dot habu-gate-runner-entry-81c84af0): a definer hitting the
\ dict-record or code-region capacity used to write only the CURRENT TOKEN to
\ fd 2 (a lone ':' at a colon definition) and exit_group - label-free and
\ unattributable. Emit the fixed label first, then the token + newline; the
\ exit codes stay the deterministic contracts (dict full $4D=77, code space
\ full $4C=76).
: C-CAP-LABEL ( ptr a -- )
   0 2 MOVZ,
   LABEL@ 1 swap ADR,
   2 CAPMSG-LEN MOVZ,  NR-WRITE SYS, ;

: C-DIE-TOKEN-NL ( n -- ) {: rc:n :}
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 2 MOVZ,  1 LQNL LABEL@ ADR,  1 1 1 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
   0 rc MOVZ,  NR-EXIT-GROUP SYS, ;

: C-DIE-DICT-FULL ( -- )
   LDICTFULL C-CAP-LABEL
   $4D C-DIE-TOKEN-NL ;

: C-DIE-CODE-FULL ( -- )
   LCODEFULL C-CAP-LABEL
   $4C C-DIE-TOKEN-NL ;

: C-QUALIFY-CAP ( -- )
   LBL {: room :}
   14 DICT-CAP MOVZ,  NDICT 14 CMP,  C-LT room BCOND,
      LDICTFULL C-CAP-LABEL
      $4D C-QUALIFY-FAIL
   room LBL, ;

: C-DUP-DEF-FAIL ( -- )
   0 2 MOVZ,  1 LKWDUPDEF LABEL@ ADR,  2 22 MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 DATA DEF-TKA-CELL LDR,  2 DATA DEF-TKL-CELL LDR,  NR-WRITE SYS,
   0 $4E MOVZ,  NR-EXIT-GROUP SYS, ;
s" c-dup-def-fail" s" --" TRUST

: C-REJECT-DUP-DEF ( -- )
   LBL LBL LBL LBL LBL LBL LBL LBL {: nloop:label nnext:label ncmp:label nmatch:label nend:label ninl:label done:label nlin:label :}
   14 DATA HIDXP-CELL LDR,  14 nlin CBZ,  C-HIDX-DUP?  13 nmatch CBNZ,  done B,  nlin LBL,  5 DBASE 0 ADDI,  6 NDICT 0 ADDI,
   nloop LBL,
      6 nend CBZ,
      14 5 40 LDR,  15 DATA DEF-WL-CELL LDR,  14 15 CMP,  C-NE nnext BCOND,
      14 5 16 LDR,  14 14 4 LSLI,  14 14 4 LSRI,
      15 DATA TKL-CELL LDR,  14 15 CMP,  C-NE nnext BCOND,
      16 5 24 ADDI,
      14 5 16 LDR,  14 14 DNAME-EXT ANDI,  14 ninl CBZ,
         16 5 24 LDR,
      ninl LBL,
      7 0 MOVZ,
      ncmp LBL,
         15 DATA TKL-CELL LDR,  7 15 CMP,  C-GE nmatch BCOND,
         15 16 7 ADD,  15 15 0 LDRB,
         3 15 $41 SUBI,  3 $1A CMPI,  3 C-CC CSET,  3 3 5 LSLI,  15 15 3 ORR,
         4 DATA TKA-CELL LDR,  4 4 7 ADD,  4 4 0 LDRB,
         3 4 $41 SUBI,  3 $1A CMPI,  3 C-CC CSET,  3 3 5 LSLI,  4 4 3 ORR,
         15 4 CMP,  C-NE nnext BCOND,
         7 7 1 ADDI,  ncmp B,
      nmatch LBL,
         C-DUP-DEF-FAIL
      nnext LBL,  5 5 DREC ADDI,  6 6 1 SUBI,  nloop B,
   nend LBL,
   done LBL, ;
s" c-reject-dup-def" s" --" TRUST

\ TFAM 2b-ii: sealed system-package guard. The offending token sits in
\ TKA/TKL when either emitter runs, so the shared fail writes it and exits with
\ the distinct named code E-SEAL-PACKAGE. Both guards read the REAL friend latch
\ (FRIEND-LATCH-CELL) natively: latch 0 = engine cold load (friend) allows the
\ reserved name; sealed = user source rejects fail-closed. The reserved-name set
\ (RESTAB above) and the A-Z fold are native, NOT checker words: the guards must
\ resolve during the sealed self-hosting stage build and checker-boot recompile,
\ where a checker word is neither reachably kept nor safely callable.
: C-SEAL-PACKAGE-FAIL ( -- )   \ write the offending package token, exit E-SEAL-PACKAGE
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 E-SEAL-PACKAGE MOVZ,  NR-EXIT-GROUP SYS, ;
s" c-seal-package-fail" s" --" TRUST

: C-SEAL-MATCH ( -- )   \ if TKA[0,x24) folds to a reserved name (RESTAB), exit E-SEAL-PACKAGE
   LBL LBL LBL LBL LBL {: tabloop:label cmploop:label matched:label tabnext:label done:label :}
   13 LRESTAB LABEL@ ADR,                               \ x13 = reserved-name table cursor
   tabloop LBL,
      14 13 0 LDRB,                                     \ x14 = entry length
      14 done CBZ,                                      \ 0 terminator -> no match
      14 24 CMP,  C-NE tabnext BCOND,                   \ length mismatch -> next entry
      15 0 MOVZ,                                        \ x15 = byte index
      cmploop LBL,
         15 24 CMP,  C-GE matched BCOND,                \ all bytes matched -> reserved
         16 DATA TKA-CELL LDR,  16 16 15 ADD,  16 16 0 LDRB,   \ x16 = candidate byte TKA[x15]
         3 16 $41 SUBI,  3 $1A CMPI,  3 C-CC CSET,  3 3 5 LSLI,  16 16 3 ORR,   \ fold A-Z -> a-z
         17 13 15 ADD,  17 17 1 ADDI,  17 17 0 LDRB,    \ x17 = entry byte [1+x15]
         16 17 CMP,  C-NE tabnext BCOND,                \ byte mismatch -> next entry
         15 15 1 ADDI,  cmploop B,
      matched LBL,
         C-SEAL-PACKAGE-FAIL
   tabnext LBL,
      13 13 14 ADD,  13 13 1 ADDI,                      \ advance past [len][bytes]
      tabloop B,
   done LBL, ;
s" c-seal-match" s" --" TRUST

: C-QUALIFY-SEAL-GUARD ( -- )   \ reject `NAME:tail` defs into a sealed system package
   LBL LBL LBL {: scan:label have:label ok:label :}
   9 DATA FRIEND-LATCH-CELL LDR,  9 ok CBZ,             \ friend/open -> no guard
   \ Only a NAME:tail token (first ':' not at an edge, matching CHECKER-QUALIFIED?)
   \ can name a package; a leading/trailing ':' (e.g. `PRIM:`) is an ordinary name.
   \ The whole check is native (RESTAB + fold), so it is safe during the sealed
   \ self-hosting stage build and checker-boot recompile of the engine's own defs.
   25 DATA TKL-CELL LDR,  24 0 MOVZ,
   scan LBL,
      24 25 CMP,  C-GE ok BCOND,                        \ no ':' -> ordinary -> skip
      9 DATA TKA-CELL LDR,  9 9 24 ADD,  9 9 0 LDRB,
      9 $3A CMPI,  C-EQ have BCOND,                      \ first ':' at index x24
      24 24 1 ADDI,  scan B,
   have LBL,
      24 ok CBZ,                                         \ leading ':' -> ordinary -> skip
      9 24 1 ADDI,  9 25 CMP,  C-GE ok BCOND,            \ trailing ':' -> ordinary -> skip
      C-SEAL-MATCH                                       \ prefix len = x24; fail if reserved
   ok LBL, ;
s" c-qualify-seal-guard" s" --" TRUST

: C-QUALIFY-DEF ( -- )
   LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL
   {: qscan qnone qhas qbad qtail qlookup qapply nloop nnext ncmp nmatch nend ninl done :}
   C-QUALIFY-SEAL-GUARD
   11 DATA TKA-CELL LDR,  11 DATA DEF-TKA-CELL STR,
   12 DATA TKL-CELL LDR,  12 DATA DEF-TKL-CELL STR,
   14 DATA CUR-CELL LDR,  14 DATA DEF-WL-CELL STR,
   9 11 0 ADDI,  10 12 0 ADDI,  17 0 MOVZ,
   qscan LBL,
      17 10 CMP,  C-GE qnone BCOND,
      14 9 17 ADD,  14 14 0 LDRB,  14 $3A CMPI,  C-EQ qhas BCOND,
      17 17 1 ADDI,  qscan B,
   qnone LBL,
      C-QUALIFY-CAP
      C-REJECT-DUP-DEF
      done B,
   qhas LBL,
      17 0 CMPI,  C-EQ qnone BCOND,
      14 17 1 ADDI,  14 10 CMP,  C-GE qnone BCOND,
      14 0 MOVZ,  14 DATA DEF-WL-CELL STR,
      14 17 1 ADDI,
   qtail LBL,
      14 10 CMP,  C-GE qlookup BCOND,
      15 9 14 ADD,  15 15 0 LDRB,  15 $3A CMPI,  C-EQ qbad BCOND,
      14 14 1 ADDI,  qtail B,
   qlookup LBL,
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
         14 5 0 LDR,  14 DATA DEF-WL-CELL STR,
         nend B,
      nnext LBL,  5 5 DREC ADDI,  6 6 1 SUBI,  nloop B,
   nend LBL,
      14 DATA DEF-WL-CELL LDR,  14 0 CMPI,  C-NE qapply BCOND,
      C-QUALIFY-CAP
      14 DATA WIDN-CELL LDR,  14 DATA DEF-WL-CELL STR,
      15 14 1 ADDI,  15 DATA WIDN-CELL STR,
      9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
      11 DATA DEF-TKA-CELL LDR,  11 DATA TKA-CELL STR,
      17 DATA TKL-CELL STR,
      C-STORE-NAME
      14 DATA DEF-WL-CELL LDR,  14 9 0 STR,
      15 0 MOVZ,  15 9 8 STR,
      15 0 MOVN,  15 9 40 STR,
      NDICT NDICT 1 ADDI,  LHIDXADD LABEL@ BL,
   qapply LBL,
      11 DATA DEF-TKA-CELL LDR,  11 11 17 ADD,  11 11 1 ADDI,  11 DATA TKA-CELL STR,
      12 DATA DEF-TKL-CELL LDR,  12 12 17 SUB,  12 12 1 SUBI,  12 DATA TKL-CELL STR,
      C-QUALIFY-CAP
      C-REJECT-DUP-DEF
      done B,
   qbad LBL,
      $4B C-QUALIFY-FAIL
   done LBL, ;
s" c-qualify-def" s" --" TRUST

\ Publish guard (TFAM 2b-v): a new record's WID is DEF-WL-CELL (from CUR-CELL, which
\ a user can redirect with `set-current`, or a resolved package WID). Reject
\ publishing into a protected WID once the friend latch is sealed -- so user source
\ cannot `<protected-wid> set-current : FOO ;` or `: RESULT:BOGUS ;` into a sealed
\ system / generated constructor package. Friend/cold-load (latch 0) is exempt.
\ x9 (the record pointer, live for the [40] store below) is preserved across the
\ LPROTWIDQ call; x30 is already caller-saved on this publish path.
: C-STORE-DEF-NAME ( -- )
   LBL {: pgok:label :}
   14 DATA FRIEND-LATCH-CELL LDR,  14 pgok CBZ,          \ open -> no guard
   SP SP 16 SUBI,  9 SP 0 STR,                           \ save record ptr
   9 DATA DEF-WL-CELL LDR,  LPROTWIDQ LABEL@ BL,         \ x9 = target wid; x13 = protected?
   9 SP 0 LDR,  SP SP 16 ADDI,                           \ restore record ptr
   13 pgok CBZ,                                          \ not protected -> allow
      1 LPROTPUB LABEL@ ADR,  0 2 MOVZ,  2 PROTPUB-MSG-LEN MOVZ,  NR-WRITE SYS,       \ protected + sealed: name the guard on fd 2 before exit 84
      0 2 MOVZ,  1 DATA DEF-TKA-CELL LDR,  2 DATA DEF-TKL-CELL LDR,  NR-WRITE SYS,     \ + the offending def name
      1 LOPENNL LABEL@ ADR,  0 2 MOVZ,  2 1 MOVZ,  NR-WRITE SYS,                       \ + newline
      0 E-SEAL-PACKAGE MOVZ,  NR-EXIT-GROUP SYS,
   pgok LBL,
   C-STORE-NAME
   14 DATA DEF-WL-CELL LDR,  14 9 40 STR,
   11 DATA DEF-TKA-CELL LDR,  11 DATA TKA-CELL STR,
   12 DATA DEF-TKL-CELL LDR,  12 DATA TKL-CELL STR, ;
s" c-store-def-name" s" --" TRUST

: EMIT-CREATE ( -- )
   LBL {: nokind :}
   LCREATE LABEL@ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,  15 SP 8 STR,
   2 3 MOVZ,  LPROT LABEL@ BL,
   LTOK LABEL@ BL,
   12 0 MOVZ,  12 DATA BODYLEN-CELL STR,  LBCAP LABEL@ BL,   \ seed "NAME " for the hook
   C-QUALIFY-DEF
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   C-STORE-DEF-NAME
   CP 9 0 STR,
   11 DATA 0 LDR,
   C-LIT
   9 W-RET LIT64,  LCEMIT LABEL@ BL,
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   10 9 0 LDR,  10 CP 10 SUB,  10 10 4 SUBI,  10 9 8 STR,
   9 DATA LASTC-CELL STR,
   NDICT NDICT 1 ADDI,  LHIDXADD LABEL@ BL,  9 9 0 LDR,   \ publish record NDICT-1; x9 = body start for the flush
   2 5 MOVZ,  LPROT LABEL@ BL,  LFLUSH LABEL@ BL,
   15 SP 8 LDR,  15 nokind CBZ,
   LKWCREATE 6 C-DEFHOOK
   nokind LBL,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

: C-CREATE ( -- )
   C-TASK-LIVE-GUARD
   15 1 MOVZ,  LCREATE LABEL@ BL,
   C-CALL-TRUST-LASTC-PTR-A ;

: C-VARIABLE ( -- )  C-CREATE
   7 DATA 0 LDR,  7 7 8 ADDI,  7 DP-CHECK  7 DATA 0 STR, ;

\ `constant` pops ONE physical cell and records the one-cell `-- a` trust
\ (C-CALL-TRUST-LASTC-A) — the permanent contract (TFAM 12 verdict 2026-07-09):
\ the interpret stack is untyped, so no shape source exists here, and a
\ wider-than-cell layout value never lands on it (DNAME-WIDE dispatch gate).
\ Parity with verify-source / public-signatures / all-errors is locked by the
\ check-all-errors-test const-layout-narrow fixture.
: C-CONSTANT ( -- )
   C-TASK-LIVE-GUARD
   2 3 MOVZ,  LPROT LABEL@ BL,  LTOK LABEL@ BL,
   12 0 MOVZ,  12 DATA BODYLEN-CELL STR,  LBCAP LABEL@ BL,   \ seed "NAME " for the hook
   C-QUALIFY-DEF
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   C-STORE-DEF-NAME
   15 G-POP                                             \ n -> x15 after name storage (clobbers x15)
   CP 9 0 STR,
   11 15 0 ADDI,  C-LIT
   9 W-RET LIT64,  LCEMIT LABEL@ BL,
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   10 9 0 LDR,  10 CP 10 SUB,  10 10 4 SUBI,  10 9 8 STR,
   9 DATA LASTC-CELL STR,
   NDICT NDICT 1 ADDI,  LHIDXADD LABEL@ BL,  9 9 0 LDR,   \ publish record NDICT-1; x9 = body start for the flush
   2 5 MOVZ,  LPROT LABEL@ BL,  LFLUSH LABEL@ BL,
   LKWCONST 8 C-DEFHOOK
   C-CALL-TRUST-LASTC-A ;

: C-CLEAR-TRUSTED-STATE ( -- )
   9 0 MOVZ,
   9 DATA TSIG-A-CELL STR,   9 DATA TSIG-U-CELL STR,
   9 DATA TCSIG-A-CELL STR,  9 DATA TCSIG-U-CELL STR,
   9 DATA DOESB-CELL STR,
   9 DATA TRUSTED-CELL STR, ;

: C-PARSE-REQUIRED-SIG ( -- )
   LBL LBL {: done bad :}
   bad C-SIG-START
   bad C-SIG-END
   C-SIG-CAPTURE-TSIG
   done B,
   bad LBL,  C-SIG-BAD
   done LBL, ;

: C-PARSE-TRUST-SIG ( -- )
   C-PARSE-REQUIRED-SIG ;

: C-COLON-MAYBE-SIG ( -- )
   LBL LBL {: nsig scd :}
   nsig C-SIG-START
   scd C-SIG-END
   scd LBL,
   C-SIG-CAPTURE-TSIG
   nsig LBL, ;

: C-TRUSTED ( -- )
   C-TASK-LIVE-GUARD
   LBL LBL LBL {: cpok ndok done :}
   2 3 MOVZ,  LPROT LABEL@ BL,
   9 REGION $4000 - LIT64,  9 DBASE 9 ADD,  CP 9 CMP,  C-LT cpok BCOND,
      C-DIE-CODE-FULL
   cpok LBL,
   9 DICT-CAP MOVZ,  NDICT 9 CMP,  C-LT ndok BCOND,
      C-DIE-DICT-FULL
   ndok LBL,
   LTOK LABEL@ BL,  0 done CBZ,
   12 0 MOVZ,  12 DATA BODYLEN-CELL STR,
   LBCAP LABEL@ BL,
   C-QUALIFY-DEF
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   9 DATA PEND-CELL STR,
   C-STORE-DEF-NAME
   CP 9 0 STR,
   5 CFSTK-OFF LIT64,  11 DBASE 5 ADD,  12 0 MOVZ,  12 11 0 STR,
   12 DATA LOCN-CELL STR,  12 DATA LOCF-CELL STR,
   12 DATA CMM-CELL STR,  12 DATA CMFRD-CELL STR,  12 DATA CMBK-CELL STR,
   C-CLEAR-TRUSTED-STATE
   12 1 MOVZ,  12 DATA TRUSTED-CELL STR,
   C-PARSE-TRUST-SIG
   12 0 MOVZ,  12 DATA VSP-CELL STR,  12 DATA SNAPSP-CELL STR,
   12 DATA EXITH-CELL STR,  12 DATA LVD-CELL STR,
   12 DATA QPATCH-CELL STR,
   12 VRALL MOVZ,  12 DATA VRFREE-CELL STR,
   12 FRALL MOVZ,  12 DATA FRFREE-CELL STR,
   9 $D10043FF LIT64,  LCEMIT LABEL@ BL,
   9 $F90003FE LIT64,  LCEMIT LABEL@ BL,
   done LBL, ;

: C-DEFER-DIE-TOKEN ( n -- ) {: rc :}
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 rc MOVZ,  NR-EXIT-GROUP SYS, ;
s" c-defer-die-token" s" n --" TRUST

: C-DEFER-FIND-UNSET ( -- )
   LBL {: found :}
   9 LKWDEFERUNSET LABEL@ ADR,  10 11 MOVZ,  LFIND LABEL@ BL,
   13 found CBNZ,
      $46 C-DEFER-DIE-TOKEN
   found LBL, ;
s" c-defer-find-unset" s" --" TRUST

: C-DEFER-CELL ( -- )
   C-DEFER-FIND-UNSET
   7 DATA DP-CELL LDR,
   7 7 7 ADDI,  7 7 3 LSRI,  7 7 3 LSLI,
   11 7 0 STR,
   7 DATA DEFER-XT-CELL STR,
   7 7 8 ADDI,  7 DP-CHECK  7 DATA DP-CELL STR, ;
s" c-defer-cell" s" --" TRUST

: C-DEFER-EMIT-CODE ( -- )
   $D10043FF C-EMITW
   $F90003FE C-EMITW
   11 DATA DEFER-XT-CELL LDR,
   C-X9-LIT
   16 9 0 W-LDRX C-EMITW
   C-CALL-BLR-X16 C-EMITW
   $F94003FE C-EMITW
   $910043FF C-EMITW
   W-RET C-EMITW ;
s" c-defer-emit-code" s" --" TRUST

: C-DEFER-META-WRITE ( -- )
   11 DEFER-MAGIC LIT64,  11 28 0 STR,  28 28 8 ADDI,
   11 DATA DEFER-XT-CELL LDR,  11 28 0 STR,  28 28 8 ADDI, ;
s" c-defer-meta-write" s" --" TRUST

: C-DEFER-ROOM ( -- )
   LBL LBL {: cpok ndok :}
   9 REGION $4000 - LIT64,  9 DBASE 9 ADD,  CP 9 CMP,  C-LT cpok BCOND,
      C-DIE-CODE-FULL
   cpok LBL,
   9 DICT-CAP MOVZ,  NDICT 9 CMP,  C-LT ndok BCOND,
      C-DIE-DICT-FULL
   ndok LBL, ;
s" c-defer-room" s" --" TRUST

: C-DEFER ( -- )
   C-TASK-LIVE-GUARD
   LBL {: named :}
   2 3 MOVZ,  LPROT LABEL@ BL,
   C-DEFER-ROOM
   LTOK LABEL@ BL,  0 named CBNZ,
      $4A C-DEFER-DIE-TOKEN
   named LBL,
   12 0 MOVZ,  12 DATA BODYLEN-CELL STR,  LBCAP LABEL@ BL,
   C-CLEAR-TRUSTED-STATE
   C-PARSE-REQUIRED-SIG
   C-DEFER-CELL
   C-QUALIFY-DEF
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   9 DATA PEND-CELL STR,
   C-STORE-DEF-NAME
   CP 9 0 STR,
   C-DEFER-EMIT-CODE
   9 DATA PEND-CELL LDR,
   10 9 0 LDR,  10 CP 10 SUB,  10 9 8 STR,
   C-DEFER-META-WRITE
   NDICT NDICT 1 ADDI,  LHIDXADD LABEL@ BL,
   9 DATA PEND-CELL LDR,  9 9 0 LDR,
   2 5 MOVZ,  LPROT LABEL@ BL,  LFLUSH LABEL@ BL,
   C-CALL-TRUST-PEND
   C-CALL-CHECKER-DEFER
   EM-REC-WIDE-PUBLISH
   C-CLEAR-TRUSTED-STATE
   9 0 MOVZ,  9 DATA PEND-CELL STR, ;
s" c-defer" s" --" TRUST

: C-DEFER-TARGET-META ( -- )
   LBL LBL LBL {: named found ok :}
   LTOK LABEL@ BL,  0 named CBNZ,
      $4A C-DEFER-DIE-TOKEN
   named LBL,
   LBCAP LABEL@ BL,
   9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFIND LABEL@ BL,
   13 found CBNZ,
      $46 C-DEFER-DIE-TOKEN
   found LBL,
   14 11 12 ADD,
   15 14 0 LDR,
   5 DEFER-MAGIC LIT64,
   15 5 CMP,  C-EQ ok BCOND,
      $4C C-DEFER-DIE-TOKEN
   ok LBL,
   14 14 8 ADDI,  14 14 0 LDR,
   14 DATA DEFER-META-CELL STR, ;
s" c-defer-target-meta" s" --" TRUST

: J-IS ( -- )
   C-DEFER-TARGET-META
   LVSPILL LABEL@ BL,
   C-POP-X16
   11 DATA DEFER-META-CELL LDR,
   C-X9-LIT
   16 9 0 W-STRX C-EMITW ;
s" j-is" s" --" TRUST

: C-IMMEDIATE ( -- )
   2 3 MOVZ,  LPROT LABEL@ BL,
   9 NDICT 0 ADDI,  9 9 1 SUBI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   10 9 16 LDR,  10 10 DNAME-IMM ORRI,  10 9 16 STR,
   2 5 MOVZ,  LPROT LABEL@ BL, ;

: C-POSTPONE ( -- )
   LBL LBL LBL {: pok pnimm pdone :}
   LTOK LABEL@ BL,  C-QUALIFY-SEAL-GUARD                 \ reject `postpone RESERVED:tail` once sealed (TFAM 2b-iii)
   9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFIND LABEL@ BL,
   13 pok CBNZ,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 70 MOVZ,  NR-EXIT-GROUP SYS,
   pok LBL,
   14 13 2 ANDI,  14 pnimm CBZ,
      C-CALL  pdone B,
   pnimm LBL,
      C-LIT
      9 LKWCOMPC LABEL@ ADR,  10 8 MOVZ,  LFIND LABEL@ BL,
      C-CALL
   pdone LBL, ;

: C-QUOTE-START ( -- )
   12 DATA INP-CELL LDR,  12 12 1 ADDI,  13 12 0 ADDI, ;

: C-QUOTE-EOF ( -- )                               \ unterminated or bad-escape string literal: label fd 2 before exit 74
   1 LBADSTR LABEL@ ADR,  0 2 MOVZ,  2 BADSTR-MSG-LEN MOVZ,  NR-WRITE SYS,
   0 74 MOVZ,  NR-EXIT-GROUP SYS, ;

: C-QUOTE-SCAN ( -- )
   LBL LBL LBL {: sl sd eof :}
   sl LBL,
      14 DATA INE-CELL LDR,
      12 14 CMP,  C-GE eof BCOND,
      9 12 0 LDRB,  9 $22 CMPI,  C-EQ sd BCOND,
      12 12 1 ADDI,  sl B,
   eof LBL,  C-QUOTE-EOF
   sd LBL, ;

: C-QUOTE-CONSUME ( -- )
   10 12 13 SUB,  16 13 0 ADDI,  12 12 1 ADDI,  12 DATA INP-CELL STR, ;

: C-QUOTE-LEN ( -- )
   10 12 13 SUB, ;

: C-QUOTE-CONSUME-DONE ( -- )
   16 13 0 ADDI,  12 12 1 ADDI,  12 DATA INP-CELL STR, ;

: C-QUOTE-SAVE ( -- )
   SP SP 16 SUBI,  16 SP 0 STR,  10 SP 8 STR, ;

: C-QUOTE-RESTORE ( -- )
   16 SP 0 LDR,  10 SP 8 LDR, ;

: C-QUOTE-SAVED-DROP ( -- )
   SP SP 16 ADDI, ;

: C-ESC-HEX-X9 ( label -- ) {: bad:label :}
   LBL LBL LBL {: lower:label upper:label done:label :}
   9 $30 CMPI,  C-LT lower BCOND,
   9 $39 CMPI,  C-GT lower BCOND,
   9 9 $30 SUBI,  done B,
   lower LBL,
   9 $61 CMPI,  C-LT upper BCOND,
   9 $66 CMPI,  C-GT upper BCOND,
   9 9 $57 SUBI,  done B,
   upper LBL,
   9 $41 CMPI,  C-LT bad BCOND,
   9 $46 CMPI,  C-GT bad BCOND,
   9 9 $37 SUBI,
   done LBL, ;

: C-ESC-DECODE-BASIC ( label label -- ) {: hex:label bad:label :}
   LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL
   {: dq:label bs:label bel:label bs8:label esc:label lf:label
      ff:label cr:label tab:label vt:label nul:label done:label :}
   9 $22 CMPI,  C-EQ dq BCOND,
   9 $71 CMPI,  C-EQ dq BCOND,
   9 $5C CMPI,  C-EQ bs BCOND,
   9 $61 CMPI,  C-EQ bel BCOND,
   9 $62 CMPI,  C-EQ bs8 BCOND,
   9 $65 CMPI,  C-EQ esc BCOND,
   9 $6C CMPI,  C-EQ lf BCOND,
   9 $66 CMPI,  C-EQ ff BCOND,
   9 $6E CMPI,  C-EQ lf BCOND,
   9 $72 CMPI,  C-EQ cr BCOND,
   9 $74 CMPI,  C-EQ tab BCOND,
   9 $76 CMPI,  C-EQ vt BCOND,
   9 $7A CMPI,  C-EQ nul BCOND,
   9 $78 CMPI,  C-EQ hex BCOND,
   9 $58 CMPI,  C-EQ hex BCOND,
   bad B,
   dq LBL,   9 $22 MOVZ,  done B,
   bs LBL,   9 $5C MOVZ,  done B,
   bel LBL,  9 $07 MOVZ,  done B,
   bs8 LBL,  9 $08 MOVZ,  done B,
   esc LBL,  9 $1B MOVZ,  done B,
   lf LBL,   9 $0A MOVZ,  done B,
   ff LBL,   9 $0C MOVZ,  done B,
   cr LBL,   9 $0D MOVZ,  done B,
   tab LBL,  9 $09 MOVZ,  done B,
   vt LBL,   9 $0B MOVZ,  done B,
   nul LBL,  9 $00 MOVZ,
   done LBL, ;

variable LESCDEC  variable LESCHEX  variable LESCSCAN  variable LESCCOPY
variable LSNAPRBD  variable LSNAPRBC
variable LAOTWIDGATE   \ AOT boot sealed-WID reject routine (TFAM 2b-v)

\ Escape decoder, emitted once by EMIT-ESC-DECODE, BL-called from the scan and
\ copy loops; entries clobber only x9/x10 (and LR). LESCDEC: x9 escape char ->
\ x9 byte, x10 0=ok 1=hex 2=bad. LESCHEX: x9 hex digit -> x9 nibble, x10 0=ok 2=bad.
: EMIT-ESC-DECODE ( -- )
   LBL LBL LBL {: hex:label bad:label hbad:label :}
   LESCDEC LABEL@ LBL,  hex bad C-ESC-DECODE-BASIC  10 0 MOVZ,  RET,
   hex LBL,  10 1 MOVZ,  RET,
   bad LBL,  10 2 MOVZ,  RET,
   LESCHEX LABEL@ LBL,  hbad C-ESC-HEX-X9  10 0 MOVZ,  RET,
   hbad LBL,  10 2 MOVZ,  RET, ;

\ Escaped-literal scan, emitted once, BL-called (x12 cursor in/out, x10 count
\ out, x11/x14/x15 scratch; saves LR around the inner decoder BLs).
: EMIT-ESC-SCAN ( -- )
   LBL LBL LBL LBL LBL {: scan:label done:label esc:label hex:label bad:label :}
   LESCSCAN LABEL@ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   11 0 MOVZ,
   scan LBL,  14 DATA INE-CELL LDR,  12 14 CMP,  C-GE bad BCOND,
      9 12 0 LDRB,  9 $22 CMPI,  C-EQ done BCOND,
      9 $5C CMPI,  C-EQ esc BCOND,
      12 12 1 ADDI,  11 11 1 ADDI,  scan B,
   esc LBL,  12 12 1 ADDI,  12 14 CMP,  C-GE bad BCOND,
      9 12 0 LDRB,  LESCDEC LABEL@ BL,
      10 1 CMPI,  C-EQ hex BCOND,  C-GT bad BCOND,
      12 12 1 ADDI,  11 11 1 ADDI,  scan B,
   hex LBL,
      15 12 3 ADDI,  15 14 CMP,  C-GT bad BCOND,
      9 12 1 LDRB,  LESCHEX LABEL@ BL,  10 bad CBNZ,
      9 12 2 LDRB,  LESCHEX LABEL@ BL,  10 bad CBNZ,
      12 15 0 ADDI,  11 11 1 ADDI,  scan B,
   bad LBL,  C-QUOTE-EOF
   done LBL,  10 11 0 ADDI,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ Escaped-literal copy, emitted once, BL-called (x11 src, x12 end, x17 dst
\ in/out; incoming x10 decoded count preserved; saves LR).
: EMIT-ESC-COPY ( -- )
   LBL LBL LBL LBL LBL {: copy:label done:label esc:label hex:label bad:label :}
   LESCCOPY LABEL@ LBL,
   SP SP 16 SUBI,  10 SP 0 STR,  30 SP 8 STR,
   copy LBL,  11 12 CMP,  C-GE done BCOND,
      9 11 0 LDRB,  9 $5C CMPI,  C-EQ esc BCOND,
      9 17 0 STRB,  17 17 1 ADDI,  11 11 1 ADDI,  copy B,
   esc LBL,
      11 11 1 ADDI,  11 12 CMP,  C-GE bad BCOND,
      9 11 0 LDRB,  LESCDEC LABEL@ BL,
      10 1 CMPI,  C-EQ hex BCOND,  C-GT bad BCOND,
      11 11 1 ADDI,  9 17 0 STRB,  17 17 1 ADDI,  copy B,
   hex LBL,
      15 11 3 ADDI,  15 12 CMP,  C-GT bad BCOND,
      9 11 1 LDRB,  LESCHEX LABEL@ BL,  10 bad CBNZ,  14 9 4 LSLI,
      9 11 2 LDRB,  LESCHEX LABEL@ BL,  10 bad CBNZ,  9 14 9 ORR,
      11 15 0 ADDI,  9 17 0 STRB,  17 17 1 ADDI,  copy B,
   bad LBL,  C-QUOTE-EOF
   done LBL,  10 SP 0 LDR,  30 SP 8 LDR,  SP SP 16 ADDI,  RET, ;

: C-ESC-QUOTE-SCAN ( -- )
   LESCSCAN LABEL@ BL, ;

: C-ESC-QUOTE-CONSUME ( -- )
   15 12 13 SUB,  16 13 0 ADDI,  12 12 1 ADDI,  12 DATA INP-CELL STR, ;

: C-ESC-QUOTE-SAVE ( -- )
   SP SP 32 SUBI,  16 SP 0 STR,  15 SP 8 STR,  10 SP 16 STR, ;

: C-ESC-QUOTE-RESTORE ( -- )
   16 SP 0 LDR,  15 SP 8 LDR,  10 SP 16 LDR, ;

: C-ESC-QUOTE-SAVED-DROP ( -- )
   SP SP 32 ADDI, ;

: C-ESC-COPY-X17 ( -- )
   LESCCOPY LABEL@ BL, ;

: C-ISDQ ( -- )
   C-QUOTE-START
   C-QUOTE-SCAN
   C-QUOTE-CONSUME
   LBL LBL {: cl cd :}
   12 DATA 0 LDR,  15 12 0 ADDI,                        \ x12 = DP, x15 = string base
   14 12 10 ADD,  14 DP-CHECK
   11 13 0 ADDI,  9 10 0 ADDI,
   cl LBL,  9 cd CBZ,
      14 11 0 LDRB,  14 12 0 STRB,  12 12 1 ADDI,  11 11 1 ADDI,  9 9 1 SUBI,  cl B,
   cd LBL,
   12 DATA 0 STR,                                       \ allot: DP advances past the copy
   15 G-PUSH  10 G-PUSH ;

: C-ICQ ( -- )
   C-QUOTE-START
   C-QUOTE-SCAN
   C-QUOTE-CONSUME
   LBL LBL LBL {: capok cl cd :}
   10 255 CMPI,  C-LE capok BCOND,  0 76 MOVZ,  NR-EXIT-GROUP SYS,
   capok LBL,
   12 DATA 0 LDR,  15 12 0 ADDI,                       \ x15 = counted string base
   14 12 10 ADD,  14 14 1 ADDI,  14 DP-CHECK
   10 12 0 STRB,  12 12 1 ADDI,
   11 13 0 ADDI,  9 10 0 ADDI,
   cl LBL,  9 cd CBZ,
      14 11 0 LDRB,  14 12 0 STRB,  12 12 1 ADDI,  11 11 1 ADDI,  9 9 1 SUBI,  cl B,
   cd LBL,
   12 DATA 0 STR,
   15 G-PUSH ;

: C-IDOTQ ( -- )
   C-QUOTE-START
   C-QUOTE-SCAN
   C-QUOTE-CONSUME
   0 1 MOVZ,  1 13 0 ADDI,  2 10 0 ADDI,  NR-WRITE SYS, ;

: C-EISDQ ( -- )
   C-QUOTE-START
   C-ESC-QUOTE-SCAN
   C-ESC-QUOTE-CONSUME
   11 16 0 ADDI,  12 16 15 ADD,
   17 DATA 0 LDR,
   14 17 10 ADD,  14 DP-CHECK
   C-ESC-COPY-X17
   17 DATA 0 STR,  11 17 10 SUB,
   11 G-PUSH  10 G-PUSH ;

: C-EICQ ( -- )
   LBL {: capok:label :}
   C-QUOTE-START
   C-ESC-QUOTE-SCAN
   10 255 CMPI,  C-LE capok BCOND,  0 76 MOVZ,  NR-EXIT-GROUP SYS,
   capok LBL,
   C-ESC-QUOTE-CONSUME
   11 16 0 ADDI,  12 16 15 ADD,
   17 DATA 0 LDR,
   14 17 10 ADD,  14 14 1 ADDI,  14 DP-CHECK
   10 17 0 STRB,  17 17 1 ADDI,
   C-ESC-COPY-X17
   17 DATA 0 STR,  11 17 10 SUB,  11 11 1 SUBI,
   11 G-PUSH ;

: C-EIDOTQ ( -- )
   C-QUOTE-START
   C-ESC-QUOTE-SCAN
   C-ESC-QUOTE-CONSUME
   11 16 0 ADDI,  12 16 15 ADD,
   17 DATA 0 LDR,
   14 17 10 ADD,  14 DP-CHECK
   C-ESC-COPY-X17
   17 DATA 0 STR,
   0 1 MOVZ,  1 17 10 SUB,  2 10 0 ADDI,  NR-WRITE SYS, ;

: C-CHAR ( -- )
   LTOK LABEL@ BL,  LBCAP LABEL@ BL,
   9 DATA TKA-CELL LDR,  9 9 0 LDRB,  9 G-PUSH ;

: C-BCHAR ( -- )
   LTOK LABEL@ BL,  LBCAP LABEL@ BL,
   11 DATA TKA-CELL LDR,  11 11 0 LDRB,  LVPUSHC LABEL@ BL, ;

: C-TICK ( -- )
   LBL {: tk :}
   LTOK LABEL@ BL,  C-QUALIFY-SEAL-GUARD                 \ reject `' RESERVED:tail` once sealed (TFAM 2b-iii)
   9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFIND LABEL@ BL,
   13 tk CBZ,
   14 13 8 ANDI,  14 LWIDE LABEL@ CBNZ,                  \ `' <wide-effect word>` would launder the bundle past the dispatch gate
   14 13 16 ANDI,  14 LINTERNAL LABEL@ CBNZ,             \ `' <internal word>` would launder the xt past the dispatch gate (execute)
   11 G-PUSH  tk LBL, ;

: C-BTICK ( -- )
   LBL {: bk :}
   LTOK LABEL@ BL,  C-QUALIFY-SEAL-GUARD                 \ reject `['] RESERVED:tail` once sealed (TFAM 2b-iii)
   9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFIND LABEL@ BL,
   13 bk CBZ,  C-LIT  bk LBL, ;

\ ---- item 12 slice 3b: pass-2 width-aware recompile, certificate side ------
\ A definition whose certified check recorded any wider-than-cell width fact is
\ re-run from a sealed copy of its source with width-aware transport lowering.
\ These meta words emit the compile-loop plumbing shared by transport ops and
\ locals carve/reference paths. Width, bind, and fetch facts come only from the
\ immutable LOWER-CERT artifact copied into the lowering transaction; pass 2
\ never calls the checker or reads its mutable tables.
variable LCERTBYTES  variable LP2CWAT  variable LP2CDESC
variable LKWTUCK3  variable LKWROT3  variable LKWMROT3
variable LKW2DUP3  variable LKW2DROP3  variable LKW2SWAP3  variable LKW2OVER3
variable LKW2TOR3  variable LKW2RFROM3  variable LKW2RFET3
variable LKWAT2  variable LKWSTORE2
variable LP2COPY  variable LP2DROPN  variable LP2REV  variable LP2ROT  variable LP2RS
variable LP2FETCH  variable LP2STORE  variable LP2VEXEC  variable LP2VEMIT
variable LVPBADMSG  variable LP2NEST
package LOWER-TXN-CODE
public
variable BAD  variable MEM  variable FULL  variable DRIFT
variable VDESC  variable DRIFT-FAIL
4095 constant MAX-WIDTH

: FAIL ( ptr a n -- ) {: msg:ptr len:n :}
   0 2 MOVZ,  1 msg LABEL@ ADR,  2 len MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 LQNL LABEL@ ADR,  1 1 1 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
   0 76 MOVZ,  NR-EXIT-GROUP SYS, ;
end-package

: EM-P2-SLOT-DIE ( -- )            \ frame slot beyond the scaled ldr/str range: fail closed
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 $4B MOVZ,  NR-EXIT-GROUP SYS, ;

: EM-P2-LOCAL-GUARD ( n label -- ) {: reg:n ok:label :}
   reg 64 CMPI,  C-CC ok BCOND,
   EM-P2-SLOT-DIE
   ok LBL, ;

: EM-P2-CARVE-W ( -- )             \ x10 := next certified bind width; bind it to live local [SP]
   LBL LBL {: localok:label bindok:label :}
   9 SP 0 LDR,  9 localok EM-P2-LOCAL-GUARD
   11 DATA TXN-CERT-A-CELL LDR,
   12 11 LOWER-CERT:WF-COUNT-CELL cells LDR,       \ certified WF row count
   13 11 LOWER-CERT:BIND-COUNT-CELL cells LDR,     \ certified bind count
   14 DATA TXN-BIND-I-CELL LDR,
   14 13 CMP,  C-CC bindok BCOND,
      0 76 MOVZ,  NR-EXIT-GROUP SYS,
   bindok LBL,
   13 LOWER-CERT:WF-CELLS cells MOVZ,
   12 12 13 MUL,
   11 11 LOWER-CERT:HEADER-CELLS cells ADDI,
   11 11 12 ADD,
   12 14 3 LSLI,  11 11 12 ADD,  10 11 0 LDR,
   12 TXN-LIVE-W-OFF MOVZ,  12 DATA 12 ADD,
   13 9 3 LSLI,  12 12 13 ADD,  10 12 0 STR,
   14 14 1 ADDI,  14 DATA TXN-BIND-I-CELL STR, ;

: EM-P2-LIVE-W ( -- )              \ x10 := certified width of live local [SP]
   LBL {: ok:label :}
   9 SP 0 LDR,  9 ok EM-P2-LOCAL-GUARD
   12 TXN-LIVE-W-OFF MOVZ,  12 DATA 12 ADD,
   13 9 3 LSLI,  12 12 13 ADD,  10 12 0 LDR, ;

: EM-P2-LIVE-CUM ( -- )            \ x10 := frame cells through live local [SP]
   LBL LBL LBL {: ok:label loop:label done:label :}
   9 SP 0 LDR,  9 ok EM-P2-LOCAL-GUARD
   10 0 MOVZ,  11 0 MOVZ,
   12 TXN-LIVE-W-OFF MOVZ,  12 DATA 12 ADD,
   loop LBL,  11 9 CMP,  C-HI done BCOND,
      13 11 3 LSLI,  14 12 13 ADD,  13 14 0 LDR,
      10 10 13 ADD,  11 11 1 ADDI,  loop B,
   done LBL, ;

\ pass-2 locals carve: each local occupies its logical width in frame cells,
\ packed from the frame top downward in declaration order (base cell of local i
\ = LOCF/8 - certified-live-cum(i)); the scalar case reproduces the pass-1 slot
\ formula LOCF/8-1-i exactly. Capture pops each local's cells tag-first (stack
\ order), bottom cell landing at its base slot, so a reference re-pushes
\ ascending. The width pass (ql loop) consumes one bind seq per group local
\ in bind order, filling the compiler-owned live table the placement pass (pl
\ loop) and EM-P2-LOCREF read back by live index. A second width-validation
\ loop walks operand positions in certificate sort order and cross-checks each
\ width row against the corresponding declaration-order bind width.
: EM-P2-CARVE ( -- )
   LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL
   {: ql:label qd:label vl:label vd:label vok:label pl:label pd:label jl:label jd:label sok:label :}
   SP SP 32 SUBI,
   9 DATA P2LOC0-CELL LDR,  9 SP 0 STR,               \ i := group start
   9 0 MOVZ,  9 SP 8 STR,                             \ total := 0
   ql LBL,
      9 SP 0 LDR,  10 DATA LOCN-CELL LDR,  9 10 CMP,  C-GE qd BCOND,
      EM-P2-CARVE-W
      9 SP 8 LDR,  9 9 10 ADD,  9 SP 8 STR,
      9 SP 0 LDR,  9 9 1 ADDI,  9 SP 0 STR,
      ql B,
   qd LBL,
   9 0 MOVZ,  9 SP 0 STR,                             \ operand pos := 0
   vl LBL,
      9 SP 0 LDR,
      10 DATA LOCN-CELL LDR,  11 DATA P2LOC0-CELL LDR,  10 10 11 SUB,
      9 10 CMP,  C-GE vd BCOND,
      9 SP 24 STR,
      9 DATA TKA-CELL LDR,  11 DATA TXN-SRC-A-CELL LDR,  9 9 11 SUB,
      10 SP 24 LDR,  LP2CWAT LABEL@ BL,  10 SP 16 STR,
      9 DATA LOCN-CELL LDR,  9 9 1 SUBI,
      11 SP 24 LDR,  9 9 11 SUB,  9 SP 0 STR,
      EM-P2-LIVE-W
      11 SP 16 LDR,  10 11 CMP,  C-EQ vok BCOND,
         LOWER-TXN-CODE:DRIFT-FAIL LABEL@ B,
      vok LBL,
      9 SP 24 LDR,  9 9 1 ADDI,  9 SP 0 STR,
      vl B,
   vd LBL,
   9 SP 8 LDR,  5 9 3 LSLI,  5 5 15 ADDI,  5 5 $FFFFFFFFFFFFFFF0 ANDI,
   9 $D10003FF LIT64,  15 5 10 LSLI,  9 9 15 ORR,  LCEMIT LABEL@ BL,
   15 DATA LOCF-CELL LDR,  15 15 5 ADD,  15 DATA LOCF-CELL STR,
   9 DATA LOCN-CELL LDR,  9 9 1 SUBI,  9 SP 0 STR,    \ i := last local
   pl LBL,
      9 SP 0 LDR,  10 DATA P2LOC0-CELL LDR,  9 10 CMP,  C-LT pd BCOND,
      EM-P2-LIVE-W  10 SP 8 STR,                      \ w
      EM-P2-LIVE-CUM  10 SP 16 STR,                   \ cum
      12 DATA LOCF-CELL LDR,  12 12 3 LSRI,
      10 SP 16 LDR,  12 12 10 SUB,                    \ x12 = base slot
      10 SP 8 LDR,  10 10 1 SUBI,                     \ j := w-1 (tag cell first)
      jl LBL,
         10 0 CMPI,  C-LT jd BCOND,
         9 $D1002273 LIT64,  LCEMIT LABEL@ BL,        \ sub x19,x19,#8
         9 $F9400269 LIT64,  LCEMIT LABEL@ BL,        \ ldr x9,[x19]
         5 12 10 ADD,
         5 4095 CMPI,  C-LE sok BCOND,
            EM-P2-SLOT-DIE
         sok LBL,
         9 $F90003E9 LIT64,  15 5 10 LSLI,  9 9 15 ORR,  LCEMIT LABEL@ BL,   \ str x9,[sp,#slot]
         10 10 1 SUBI,  jl B,
      jd LBL,
      9 SP 0 LDR,  9 9 1 SUBI,  9 SP 0 STR,
      pl B,
   pd LBL,
   SP SP 32 ADDI, ;

\ pass-2 local reference: spill, then push the local's whole group ascending
\ from its frame base. Scalar locals take this memory path too when a wide
\ local exists anywhere in the frame — their base comes from the same width-
\ aware formula, so offsets stay correct after a wide neighbor.
: EM-P2-LOCREF ( -- )
   LBL LBL LBL {: rl:label rd:label sok2:label :}
   SP SP 32 SUBI,
   0 SP 0 STR,                                        \ idx from LLOC-FIND
   EM-P2-LIVE-W  10 SP 8 STR,                         \ w
   EM-P2-LIVE-CUM  10 SP 16 STR,                      \ cum
   LVSPILL LABEL@ BL,
   12 DATA LOCF-CELL LDR,  12 12 3 LSRI,
   10 SP 16 LDR,  12 12 10 SUB,                       \ x12 = base slot
   10 0 MOVZ,                                         \ j := 0 (bottom cell first)
   rl LBL,
      11 SP 8 LDR,  10 11 CMP,  C-GE rd BCOND,
      5 12 10 ADD,
      5 4095 CMPI,  C-LE sok2 BCOND,
         EM-P2-SLOT-DIE
      sok2 LBL,
      9 $F94003E9 LIT64,  15 5 10 LSLI,  9 9 15 ORR,  LCEMIT LABEL@ BL,   \ ldr x9,[sp,#slot]
      9 W-PUSH0 LIT64,  LCEMIT LABEL@ BL,
      9 W-PUSH1 LIT64,  LCEMIT LABEL@ BL,
      10 10 1 ADDI,  rl B,
   rd LBL,
   SP SP 32 ADDI, ;

: C-LBRACE-DIE ( -- )   \ B2: emit the locals-placement diagnostic, then exit 75
   1 LBADLOC LABEL@ ADR,  0 2 MOVZ,  2 $27 MOVZ,  NR-WRITE SYS,
   0 $4B MOVZ,  NR-EXIT-GROUP SYS, ;
s" c-lbrace-die" s" --" TRUST

: C-LBRACE-GUARDS ( -- )
   LBL {: qlok:label :}
   11 DATA QPATCH-CELL LDR,  11 qlok CBZ,
      C-LBRACE-DIE
   qlok LBL, ;

: C-LBRACE-STORE-ONE ( -- )
   LBL LBL LBL LBL LBL {: nlok ncp ncd tsl tsd :}
   11 DATA LOCN-CELL LDR,  11 $40 CMPI,  C-LT nlok BCOND,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 $4B MOVZ,  NR-EXIT-GROUP SYS,
   nlok LBL,
   11 DATA LOCN-CELL LDR,  12 LOC-REC MOVZ,  11 11 12 MUL,  5 LOCNAMES LIT64,  11 11 5 ADD,  11 DATA 11 ADD,
   14 0 MOVZ,  8 DATA TKL-CELL LDR,  10 DATA TKA-CELL LDR,
   tsl LBL,  14 8 CMP,  C-GE tsd BCOND,
      15 10 14 ADD,  15 15 0 LDRB,  15 58 CMPI,  C-EQ tsd BCOND,
      14 14 1 ADDI,  tsl B,
   tsd LBL,
   14 11 0 STR,
   12 11 8 ADDI,  13 DATA TKA-CELL LDR,
   ncp LBL,  14 ncd CBZ,  15 13 0 LDRB, 15 12 0 STRB, 12 12 1 ADDI, 13 13 1 ADDI, 14 14 1 SUBI, ncp B,
   ncd LBL,
   11 DATA LOCN-CELL LDR,  11 11 1 ADDI,  11 DATA LOCN-CELL STR, ;

: C-LBRACE-PARSE-NAMES ( -- )
   LBL LBL LBL {: nl nd nstore :}
   6 DATA LOCN-CELL LDR,
   nl LBL,
      LTOK LABEL@ BL,  0 nd CBZ,
      LBCAP LABEL@ BL,                                          \ locals reach the checker too
      0 LKWENDLOC LABEL@ ADR,  1 2 MOVZ,  LKWCMP LABEL@ BL,  0 nstore CBZ,  nd B,
      nstore LBL,
      C-LBRACE-STORE-ONE
      nl B,
   nd LBL, ;

: C-LBRACE-CARVE-FRAME ( -- )
   LBL LBL {: pl pd :}
   LBL LBL {: p1c:label pjoin:label :}
   9 DATA P2-CELL LDR,  9 p1c CBZ,                    \ pass 2: width-aware carve
      EM-P2-CARVE
      pjoin B,
   p1c LBL,
   13 DATA LOCN-CELL LDR,  14 13 6 SUB,
   5 14 3 LSLI,  5 5 15 ADDI,  5 5 $FFFFFFFFFFFFFFF0 ANDI,
   9 $D10003FF LIT64,  15 5 10 LSLI,  9 9 15 ORR,  LCEMIT LABEL@ BL,
   15 DATA LOCF-CELL LDR,  15 15 5 ADD,  15 DATA LOCF-CELL STR,
   12 DATA LOCF-CELL LDR,  12 12 3 LSRI,
   13 DATA LOCN-CELL LDR,  13 13 1 SUBI,
   pl LBL,
      13 6 CMP,  C-LT pd BCOND,
      9 $D1002273 LIT64,  LCEMIT LABEL@ BL,
      9 $F9400269 LIT64,  LCEMIT LABEL@ BL,
      5 12 13 SUB,  5 5 1 SUBI,
      9 $F90003E9 LIT64,  5 5 10 LSLI,  9 9 5 ORR,  LCEMIT LABEL@ BL,
      13 13 1 SUBI,  pl B,
   pd LBL,
   pjoin LBL, ;

: C-LBRACE ( -- )
   9 DATA LOCN-CELL LDR,  9 DATA P2LOC0-CELL STR,     \ group start for the pass-2 carve
   C-LBRACE-GUARDS
   C-LBRACE-PARSE-NAMES
   C-LBRACE-CARVE-FRAME ;

\ compile-mode PC-RELATIVE address push: emit `adr x9, target` then the push
\ stencil. Unlike C-LIT's absolute movz/movk, the offset survives the AOT blob
\ copy and the ASLR slide, because the target (an embedded S" body) moves WITH
\ this instruction. target in x11; CP (the emit cursor / future ADR pc) is x28.
: C-ADR ( -- )
   5 11 28 SUB,                                                       \ x5 = d = target - CP
   8 $10000009 LIT64,                                                 \ ADR opcode | Rd=x9
   6 3 MOVZ,  7 5 6 AND,  7 7 29 LSLI,  8 8 7 ORR,                    \ | (d & 3) << 29
   7 5 2 LSRI,  6 $7FFFF LIT64,  7 7 6 AND,  7 7 5 LSLI,  8 8 7 ORR,  \ | ((d>>2) & 0x7FFFF) << 5
   9 8 0 ADDI,  LCEMIT LABEL@ BL,                                          \ emit the ADR word
   9 W-PUSH0 LIT64,  LCEMIT LABEL@ BL,  9 W-PUSH1 LIT64,  LCEMIT LABEL@ BL, ;

: C-SDQ ( -- )
   LBL LBL {: cl cd :}
   C-QUOTE-START
   C-QUOTE-SCAN
   C-QUOTE-CONSUME
   C-QUOTE-SAVE
   C-QUOTE-RESTORE
   11 16 0 ADDI,  12 10 1 ADDI,  LBCS LABEL@ BL,
   15 CP 0 ADDI,  9 $14000000 LIT64,  LCEMIT LABEL@ BL,
   12 CP 0 ADDI,
   C-QUOTE-RESTORE
   11 16 0 ADDI,  9 10 0 ADDI,
   cl LBL,  9 cd CBZ,
      14 11 0 LDRB,  14 28 0 STRB,  28 28 1 ADDI,  11 11 1 ADDI,  9 9 1 SUBI,  cl B,
   cd LBL,
   28 28 3 ADDI,  5 -4 LIT64,  28 28 5 AND,
   9 15 0 ADDI,  15 10 0 ADDI,  LPAT LABEL@ BL,
   11 12 0 ADDI,  C-ADR                                \ push byte addr PC-relative (AOT/ASLR-safe)
   11 15 0 ADDI,  C-LIT                                \ push len (a value, absolute is fine)
   C-QUOTE-SAVED-DROP ;

: C-CQ ( -- )
   LBL LBL LBL {: capok cl cd :}
   C-QUOTE-START
   C-QUOTE-SCAN
   C-QUOTE-LEN
   10 255 CMPI,  C-LE capok BCOND,  0 76 MOVZ,  NR-EXIT-GROUP SYS,
   capok LBL,
   C-QUOTE-CONSUME-DONE
   C-QUOTE-SAVE
   C-QUOTE-RESTORE
   11 16 0 ADDI,  12 10 1 ADDI,  LBCS LABEL@ BL,
   15 CP 0 ADDI,  9 $14000000 LIT64,  LCEMIT LABEL@ BL,
   12 CP 0 ADDI,
   C-QUOTE-RESTORE
   10 28 0 STRB,  28 28 1 ADDI,
   11 16 0 ADDI,  9 10 0 ADDI,
   cl LBL,  9 cd CBZ,
      14 11 0 LDRB,  14 28 0 STRB,  28 28 1 ADDI,  11 11 1 ADDI,  9 9 1 SUBI,  cl B,
   cd LBL,
   28 28 3 ADDI,  5 -4 LIT64,  28 28 5 AND,
   9 15 0 ADDI,  15 10 1 ADDI,  LPAT LABEL@ BL,
   11 12 0 ADDI,  C-ADR
   C-QUOTE-SAVED-DROP ;

: C-DOTQ ( -- )
   LBL {: ok :}
   C-SDQ
   9 LKWTYPE LABEL@ ADR,  10 4 MOVZ,  LFIND LABEL@ BL,
   13 ok CBNZ,  0 70 MOVZ,  NR-EXIT-GROUP SYS,
   ok LBL,
   C-CALL ;

: C-ESDQ ( -- )
   C-QUOTE-START
   C-ESC-QUOTE-SCAN
   C-ESC-QUOTE-CONSUME
   C-ESC-QUOTE-SAVE
   C-ESC-QUOTE-RESTORE
   11 16 0 ADDI,  12 15 1 ADDI,  LBCS LABEL@ BL,
   9 $14000000 LIT64,  LCEMIT LABEL@ BL,
   13 CP 0 ADDI,
   C-ESC-QUOTE-RESTORE
   11 16 0 ADDI,  12 16 15 ADD,  17 28 0 ADDI,
   C-ESC-COPY-X17
   28 17 0 ADDI,
   28 28 3 ADDI,  5 -4 LIT64,  28 28 5 AND,
   12 13 0 ADDI,
   9 13 4 SUBI,  15 10 0 ADDI,  LPAT LABEL@ BL,
   11 12 0 ADDI,  C-ADR
   11 15 0 ADDI,  C-LIT
   C-ESC-QUOTE-SAVED-DROP ;

: C-ECQ ( -- )
   LBL {: capok:label :}
   C-QUOTE-START
   C-ESC-QUOTE-SCAN
   10 255 CMPI,  C-LE capok BCOND,  0 76 MOVZ,  NR-EXIT-GROUP SYS,
   capok LBL,
   C-ESC-QUOTE-CONSUME
   C-ESC-QUOTE-SAVE
   C-ESC-QUOTE-RESTORE
   11 16 0 ADDI,  12 15 1 ADDI,  LBCS LABEL@ BL,
   9 $14000000 LIT64,  LCEMIT LABEL@ BL,
   13 CP 0 ADDI,
   C-ESC-QUOTE-RESTORE
   10 28 0 STRB,  28 28 1 ADDI,
   11 16 0 ADDI,  12 16 15 ADD,  17 28 0 ADDI,
   C-ESC-COPY-X17
   28 17 0 ADDI,
   28 28 3 ADDI,  5 -4 LIT64,  28 28 5 AND,
   12 13 0 ADDI,
   9 13 4 SUBI,  15 10 1 ADDI,  LPAT LABEL@ BL,
   11 12 0 ADDI,  C-ADR
   C-ESC-QUOTE-SAVED-DROP ;

: C-EDOTQ ( -- )
   LBL {: ok:label :}
   C-ESDQ
   9 LKWTYPE LABEL@ ADR,  10 4 MOVZ,  LFIND LABEL@ BL,
   13 ok CBNZ,  0 70 MOVZ,  NR-EXIT-GROUP SYS,
   ok LBL,
   C-CALL ;
variable CFSK

TRUSTED: EM-HXT-EXECUTE ( n -- )
   execute ;

: CF-ENTRY ( label ptr a n n -- ) {: lmainlbl:label kwvar:ptr kwlen:n hxt:n :}
   LBL CFSK !
   0 kwvar LABEL@ ADR,  1 kwlen MOVZ,  LKWCMP LABEL@ BL,
   0 CFSK LABEL@ CBZ,
   LVSPILL LABEL@ BL,
   hxt EM-HXT-EXECUTE  lmainlbl B,
   CFSK LABEL@ LBL, ;

\ cfn-entry: keyword case WITHOUT the spill — loop words manage the VS
\ themselves (BEGIN snapshots it, AGAIN/REPEAT reconcile to the snapshot).
: CFN-ENTRY ( label ptr a n n -- ) {: lmainlbl:label kwvar:ptr kwlen:n hxt:n :}
   LBL CFSK !
   0 kwvar LABEL@ ADR,  1 kwlen MOVZ,  LKWCMP LABEL@ BL,
   0 CFSK LABEL@ CBZ,
   hxt EM-HXT-EXECUTE  lmainlbl B,
   CFSK LABEL@ LBL, ;
\ ---- MAIN, split into emission-ordered phases sharing label variables ----
variable LMAIN  variable LEXIT  variable LCOMPILE  variable LUNDEF
variable LUNDERFLOW           \ top-level data-stack underflow diagnostic entry (guard at LMAIN boundary)
variable LARITY               \ pre-exec arity guard for deref/execute prims (interpret-find BL -> EMIT-ARITY-GUARD)
variable LEX0  variable LUN0   \ re-entrant evaluate: original-path continuations of LEXIT / LUNDEF
variable LEVALREC             \ re-entrant evaluate: throw-escape recovery entry (BTHROW branches here)
variable LEVLL  variable LEVLP  variable LEVLD  variable LEVLN  variable LEVLR   \ LEVALREC internal labels
variable CLOC-MAIN  variable CLOC-NOT
variable CLOC-MEM   variable CLOC-QOK   variable CLOC-P1
variable CFSK2

\ cfb-entry: branch keywords (if/until/while) with the condition on the VS —
\ a REGISTER top branches directly (no spill + memory pop); con or empty falls
\ back to the spill + pop path. hxtr gets the condition reg in x14.
: CFB-ENTRY ( label ptr a n n n -- ) {: lmainlbl:label kwvar:ptr kwlen:n hxtm:n hxtr:n :}
   LBL CFSK !  LBL CFSK2 !
   0 kwvar LABEL@ ADR,  1 kwlen MOVZ,  LKWCMP LABEL@ BL,
   0 CFSK LABEL@ CBZ,
   6 DATA VSP-CELL LDR,  6 CFSK2 LABEL@ CBZ,
   5 6 1 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   7 CFSK2 LABEL@ CBNZ,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  14 8 0 LDR,
   SP SP 16 SUBI,  14 SP 8 STR,
   LVDROP LABEL@ BL,  LVSPILL LABEL@ BL,
   14 SP 8 LDR,  SP SP 16 ADDI,
   hxtr EM-HXT-EXECUTE
   lmainlbl B,
   CFSK2 LABEL@ LBL,
   LVSPILL LABEL@ BL,
   hxtm EM-HXT-EXECUTE
   lmainlbl B,
   CFSK LABEL@ LBL, ;

\ cfbn-entry: like CFB-ENTRY but the register path neither spills nor saves —
\ UNTIL reconciles to the BEGIN snapshot itself; the condition reg x14 survives
\ LVDROP (which only relabels the VS, no emission).
: CFBN-ENTRY ( label ptr a n n n -- ) {: lmainlbl:label kwvar:ptr kwlen:n hxtm:n hxtr:n :}
   LBL CFSK !  LBL CFSK2 !
   0 kwvar LABEL@ ADR,  1 kwlen MOVZ,  LKWCMP LABEL@ BL,
   0 CFSK LABEL@ CBZ,
   6 DATA VSP-CELL LDR,  6 CFSK2 LABEL@ CBZ,
   5 6 1 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   7 CFSK2 LABEL@ CBNZ,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  14 8 0 LDR,
   LVDROP LABEL@ BL,
   hxtr EM-HXT-EXECUTE
   lmainlbl B,
   CFSK2 LABEL@ LBL,
   LVSPILL LABEL@ BL,
   hxtm EM-HXT-EXECUTE
   lmainlbl B,
   CFSK LABEL@ LBL, ;

: J-IFR ( -- )  C-PUSHCP  8 $B4000000 LIT64,  9 8 14 ORR,  LCEMIT LABEL@ BL, ;

: J-WHILER ( -- )  J-IFR ;

: J-UNTILR ( -- )                                 \ reg flag -> x17 first: the reconcile
   8 $AA0003F1 LIT64,  7 14 16 LSLI,  9 8 7 ORR,  LCEMIT LABEL@ BL,   \ may reload into it
   J-UNTILX ;

: C-LOCAL-REF-LABELS ( -- )
   LBL CLOC-MEM !  LBL CLOC-QOK ! ;

: C-LOCAL-REF-ARGS ( label label -- )
   CLOC-NOT !  CLOC-MAIN ! ;

: C-LOCAL-REF ( label label -- )
   C-LOCAL-REF-ARGS
   C-LOCAL-REF-LABELS
   LLOC-FIND LABEL@ BL,  0 0 CMPI,  C-LT CLOC-NOT LABEL@ BCOND,
   LBCAP LABEL@ BL,
   11 DATA QPATCH-CELL LDR,  11 CLOC-QOK LABEL@ CBZ,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 75 MOVZ,  NR-EXIT-GROUP SYS,
   CLOC-QOK LABEL@ LBL,
   LBL CLOC-P1 !
   9 DATA P2-CELL LDR,  9 CLOC-P1 LABEL@ CBZ,         \ pass 2: width-aware reference
      EM-P2-LOCREF
      CLOC-MAIN LABEL@ B,
   CLOC-P1 LABEL@ LBL,
   LVRALLOC LABEL@ BL,  14 CLOC-MEM LABEL@ CBZ,
   7 DATA LOCF-CELL LDR,  7 7 3 LSRI,  7 7 0 SUB,  7 7 1 SUBI,
   9 $F94003E0 LIT64,  9 9 14 ORR,  7 7 10 LSLI,  9 9 7 ORR,  LCEMIT LABEL@ BL,
   LVPUSHR LABEL@ BL,
   CLOC-MAIN LABEL@ B,
   CLOC-MEM LABEL@ LBL,
   LVSPILL LABEL@ BL,
   7 DATA LOCF-CELL LDR,  7 7 3 LSRI,  7 7 0 SUB,  7 7 1 SUBI,
   9 $F94003E9 LIT64,  7 7 10 LSLI,  9 9 7 ORR,  LCEMIT LABEL@ BL,
   9 W-PUSH0 LIT64,  LCEMIT LABEL@ BL,  9 W-PUSH1 LIT64,  LCEMIT LABEL@ BL,
   CLOC-MAIN LABEL@ B, ;
s" c-local-ref" s" label label --" TRUST

: EM-ENTRY-ARGS ( -- )
   HB-TARGET-LINUX? IF
      13 SP 0 LDR,  14 SP 8 ADDI,
      15 13 1 ADDI,  15 15 3 LSLI,  15 14 15 ADD,
      exit
   THEN
   HB-TARGET-MACOS? IF
      13 0 0 ADDI,  14 1 0 ADDI,  15 2 0 ADDI,
      exit
   THEN
   C-TARGET-UNKNOWN ;

: EM-RUNTIME-STACK ( -- )
   XREG-RBASE LANCHOR LABEL@ ADR,
   SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,
   SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,
   XDS SP 0 ADDI, ;

: EM-MMAP-CODE-REGION ( -- )
   LBL {: rvok :}
   0 RBASE-VA LIT64,  1 REGION LIT64,  2 3 MOVZ,  3 MAP-ANON-PRIVATE-FIXED LIT64,  4 0 MOVN,  5 0 MOVZ,
   NR-MMAP SYS,
   5 RBASE-VA LIT64,  0 5 CMP,
   C-EQ rvok BCOND,
      0 78 MOVZ,  NR-EXIT-GROUP SYS,
   rvok LBL, ;

\ ---- AOT seed: register the metabuild-compiled words baked in the LAOTCODE
\ blob + LAOTDICT records. Runs after EM-SEED-DICT (region mapped, DBASE/NDICT/CP
\ live) and before the DATA region is mapped, so it only touches the code region
\ and the pinned registers. The blob is copied to CP (the current code-area top),
\ each record's [0]/[8] (text-blob-relative offsets) are rebased against that base,
\ and NDICT/CP are advanced so the cold-prefix compile lands after the blob. Words
\ with no external calls are position-independent, so no call relocation is needed
\ for the one-word milestone. LAOTCODELEN = 0 (stage/maker builds) skips it whole.
\ Registers x13/x14/x15 still hold argc/argv/envp until EM-DATA-INIT stores them,
\ so this pass (which runs earlier) must stay off them, exactly like EM-SEED-DICT.
\ Copy the baked LAOTCODE blob to CP (x11 = blob byte length on entry).
: EM-AOT-COPY-BLOB ( -- )
   LBL LBL {: acopy:label adone:label :}
   9 LAOTCODE LABEL@ ADR,  12 0 MOVZ,                \ x9 = blob src (__text), x12 = i
   acopy LBL,  12 11 CMP,  C-GE adone BCOND,
      3 9 12 ADD,  3 3 0 LDRB,  4 CP 12 ADD,  3 4 0 STRB,
      12 12 1 ADDI,  acopy B,
   adone LBL, ;

\ Register the LAOTNREC baked records at &dict[NDICT], rebasing each [0] xt from a
\ blob offset to CP+offset, and hash-indexing it (LHIDXADD). All records first, so
\ EM-AOT-PATCH-SITES can resolve sibling calls by name.
\ Each source record is a compact 12 bytes (word0 = blob-off u16 | end u16<<16;
\ word1 = name-off u16 | flags u8<<16 | pad u8<<24; word2 = wid u32); expand it to
\ the full 48B dict record, rebasing [0] xt to CP+blob-off and reconstructing [16]
\ flags|len, the [24..40) inline name (from the deduped LAOTNAMES pool, zero-padded),
\ and [40] wid (full u32 so wordlist IDs above 255 survive) -- the EXACT inverse of
\ the build-time ACAP-COMPACT-RECS, proven byte-identical to the source-compiled
\ record by ACAP-PROVE-RECS. As each record is registered WIDN is advanced above its
\ wid, so a post-seed wordlist allocation cannot collide with a restored wordlist.
\ x2..x7 are LHIDXADD's saved set; x9/x11/x12 survive it. Records are 4B-aligned so
\ each 32-bit word loads with LDRW.
: EM-AOT-REGISTER-RECS ( -- )
   LBL LBL LBL LBL LBL {: rloop:label rdone:label nloop:label ndone:label widok:label :}
   9 LAOTDICT LABEL@ ADR,  12 0 MOVZ,               \ x9 = compact record src (12B stride), x12 = k
   11 LAOTNREC LABEL@ ADR,  11 11 0 LDR,            \ x11 = N (survives LHIDXADD)
   rloop LBL,  12 11 CMP,  C-GE rdone BCOND,
      10 DREC MOVZ,  10 NDICT 10 MUL,  10 DBASE 10 ADD,   \ x10 = &dict[NDICT]
      3 9 0 LDRW,                                   \ x3 = word0 = blob-off | end<<16
      5 $FFFF LIT64,  4 3 5 AND,  4 CP 4 ADD,  4 10 0 STR,  \ [0] xt = CP + blob-off (u16)
      3 3 16 LSRI,  3 10 8 STR,                     \ [8] end = word0>>16 (u16, hi=0)
      6 9 4 LDRW,                                   \ x6 = word1 = name-off | flags<<16 | pad<<24
      5 $FFFF LIT64,  4 6 5 AND,                     \ x4 = name-off
      7 LAOTNAMES LABEL@ ADR,  4 7 4 ADD,           \ x4 = pool entry ptr (len byte)
      5 4 0 LDRB,                                   \ x5 = name length = pool[entry]
      7 6 16 LSRI,  3 $FF LIT64,  7 7 3 AND,        \ x7 = flags = (word1>>16)&0xFF
      7 7 60 LSLI,  7 7 5 ORR,  7 10 16 STR,        \ [16] = flags<<60 | len
      2 0 MOVZ,  2 10 24 STR,  2 10 32 STR,         \ zero [24..40)
      4 4 1 ADDI,                                   \ x4 = name src (entry+1)
      3 0 MOVZ,                                     \ x3 = i
      nloop LBL,  3 5 CMP,  C-GE ndone BCOND,
         2 4 3 ADD,  2 2 0 LDRB,                    \ x2 = name[i]
         7 10 24 ADDI,  7 7 3 ADD,  2 7 0 STRB,     \ dict[24+i] = name[i]
         3 3 1 ADDI,  nloop B,
      ndone LBL,
      6 9 8 LDRW,  6 10 40 STR,                     \ [40] wid = word2 (full u32, hi=0)
      4 6 1 ADDI,  5 DATA WIDN-CELL LDR,  4 5 CMP,  C-LE widok BCOND,   \ WIDN = max(WIDN, wid+1)
         4 DATA WIDN-CELL STR,                       \ advance so post-seed allocs clear restored wids
      widok LBL,
      NDICT NDICT 1 ADDI,  LHIDXADD LABEL@ BL,      \ publish + index (x9/x11/x12 preserved)
      9 9 12 ADDI,  12 12 1 ADDI,  rloop B,
   rdone LBL, ;

\ Restore the baked protected-WID registry (TFAM 2b-v). Copies the LAOTPWID u32
\ WIDs into the friend-arena registry table (direct STR into the sealed band, same
\ as the WIDN advance below -- the AOT seed pass is trusted boot machinery), sets
\ PROT-WID-N-CELL to the restored count, and advances WIDN past each restored WID so
\ a post-restore wordlist/package allocation cannot reuse a protected WID. Full u32
\ per entry: a WID above 255 restores without truncation. N (bounded by PROT-WID-MAX
\ at capture) needs no runtime cap check. Runs after EM-AOT-REGISTER-RECS.
: EM-AOT-REGISTER-PROT-WIDS ( -- )
   LBL LBL LBL {: ploop:label pdone:label pwok:label :}
   9 LAOTPWID LABEL@ ADR,                           \ x9 = baked u32 WID src
   11 LAOTNPWID LABEL@ ADR,  11 11 0 LDR,           \ x11 = restored count N
   11 DATA PROT-WID-N-CELL STR,                     \ registry count := N
   10 PROT-WID-OFF MOVZ,  10 DATA 10 ADD,           \ x10 = &registry[0] (offset > imm12: materialize + add)
   12 0 MOVZ,                                       \ x12 = i
   ploop LBL,  12 11 CMP,  C-GE pdone BCOND,
      3 9 0 LDRW,                                   \ x3 = baked wid (full u32)
      3 10 0 STRW,                                  \ registry[i] = wid
      4 3 1 ADDI,  5 DATA WIDN-CELL LDR,  4 5 CMP,  C-LE pwok BCOND,   \ WIDN = max(WIDN, wid+1)
         4 DATA WIDN-CELL STR,
      pwok LBL,
      9 9 4 ADDI,  10 10 4 ADDI,  12 12 1 ADDI,  ploop B,
   pdone LBL, ;

\ For each baked call site (packed 4B row = blob-off u16 | name-off u16<<16 into the
\ deduped [len][bytes] name pool at LAOTNAMES) resolve the callee by NAME in THIS
\ engine (LFIND over primitives, cold-prefix words, and the just-registered
\ siblings) and re-encode the three movz/movk x16 immediates at CP+blob-offset to
\ that address. A missing name is a build/seed inconsistency: fail closed. Rows are
\ 4B-aligned so each loads with a single LDRW.
: EM-AOT-PATCH-SITES ( -- )
   LBL LBL LBL {: ploop:label pdone:label pnf:label :}
   21 LAOTSITES LABEL@ ADR,                          \ x21 = row cursor (4B rows)
   23 LAOTNSITE LABEL@ ADR,  23 23 0 LDR,            \ x23 = site count M
   22 0 MOVZ,                                        \ x22 = site index
   ploop LBL,  22 23 CMP,  C-GE pdone BCOND,
      24 21 0 LDRW,                                  \ x24 = row = blob-off | name-off<<16
      4 24 16 LSRI,                                  \ x4 = name-off (row>>16)
      5 $FFFF LIT64,  24 24 5 AND,                   \ x24 = blob offset (row & 0xFFFF; survives LFIND)
      5 LAOTNAMES LABEL@ ADR,  9 5 4 ADD,            \ x9 = pool entry ptr = LAOTNAMES + name-off
      10 9 0 LDRB,                                   \ x10 = name length = pool[entry]
      9 9 1 ADDI,                                    \ x9 = name ptr = entry + 1
      LFIND LABEL@ BL,                               \ x11 = xt, x13 = found?
      13 pnf CBZ,
      LAOTWIDGATE LABEL@ BL,                         \ TFAM 2b-v: reject reloc into a protected WID (x24 survives)
      9 CP 24 ADD,                                   \ x9 = site addr = CP + blob offset
      10 9 0 LDRW,  5 $FFE0001F LIT64,  10 10 5 AND,
        14 11 0 ADDI,  5 $FFFF LIT64,  14 14 5 AND,  14 14 5 LSLI,  10 10 14 ORR,  10 9 0 STRW,
      10 9 4 LDRW,  5 $FFE0001F LIT64,  10 10 5 AND,
        14 11 16 LSRI,  5 $FFFF LIT64,  14 14 5 AND,  14 14 5 LSLI,  10 10 14 ORR,  10 9 4 STRW,
      10 9 8 LDRW,  5 $FFE0001F LIT64,  10 10 5 AND,
        14 11 32 LSRI,  5 $FFFF LIT64,  14 14 5 AND,  14 14 5 LSLI,  10 10 14 ORR,  10 9 8 STRW,
      21 21 4 ADDI,  22 22 1 ADDI,  ploop B,
   pnf LBL,  0 $51 MOVZ,  NR-EXIT-GROUP SYS,
   pdone LBL, ;

\ DATA-literal relocation (third relocation class): reserve the REPL's DATA span
\ at the current DP (the region is fixed MAP_FIXED, anon-mmap => zeroed => identical
\ to a source-compiled all-allot/variable region), then rebase every captured DATA
\ address literal (movz/movk x9 chain) by delta = seedDP - captureD0, so create/
\ variable buffer refs point at the seeded DATA. No name lookup; single delta.
: EM-AOT-RELOC-DATA ( -- )
   LBL LBL {: dloop:label drdone:label :}
   3 DATA DP-CELL LDR,                              \ x3 = seed DP (abs) = REPL DATA base at boot
   5 LAOTDATAD0 LABEL@ ADR,  5 5 0 LDR,             \ x5 = capture-time REPL DATA base
   6 3 5 SUB,                                       \ x6 = delta (survives the loop)
   5 LAOTDATASIZE LABEL@ ADR,  5 5 0 LDR,           \ x5 = REPL DATA span
   3 3 5 ADD,  3 DATA DP-CELL STR,                  \ reserve: DP += span (zeroed by anon mmap)
   21 LAOTDSITES LABEL@ ADR,                        \ x21 = DATA-site cursor (u16 offsets)
   23 LAOTNDSITE LABEL@ ADR,  23 23 0 LDR,          \ x23 = DATA-site count
   22 0 MOVZ,
   dloop LBL,  22 23 CMP,  C-GE drdone BCOND,
      24 21 0 LDRB,  4 21 1 LDRB,  4 4 8 LSLI,  24 24 4 ORR,   \ x24 = blob offset (u16 LE)
      9 CP 24 ADD,                                  \ x9 = literal addr = CP + blob offset
      10 9 0 LDRW,   10 10 5 LSRI,  5 $FFFF LIT64,  10 10 5 AND,  11 10 0 ADDI,
      10 9 4 LDRW,   10 10 5 LSRI,  5 $FFFF LIT64,  10 10 5 AND,  10 10 16 LSLI,  11 11 10 ORR,
      10 9 8 LDRW,   10 10 5 LSRI,  5 $FFFF LIT64,  10 10 5 AND,  10 10 32 LSLI,  11 11 10 ORR,
      10 9 12 LDRW,  10 10 5 LSRI,  5 $FFFF LIT64,  10 10 5 AND,  10 10 48 LSLI,  11 11 10 ORR,
      11 11 6 ADD,                                  \ x11 = value + delta
      10 9 0 LDRW,   5 $FFE0001F LIT64,  10 10 5 AND,  14 11 0 ADDI,   5 $FFFF LIT64,  14 14 5 AND,  14 14 5 LSLI,  10 10 14 ORR,  10 9 0 STRW,
      10 9 4 LDRW,   5 $FFE0001F LIT64,  10 10 5 AND,  14 11 16 LSRI,  5 $FFFF LIT64,  14 14 5 AND,  14 14 5 LSLI,  10 10 14 ORR,  10 9 4 STRW,
      10 9 8 LDRW,   5 $FFE0001F LIT64,  10 10 5 AND,  14 11 32 LSRI,  5 $FFFF LIT64,  14 14 5 AND,  14 14 5 LSLI,  10 10 14 ORR,  10 9 8 STRW,
      10 9 12 LDRW,  5 $FFE0001F LIT64,  10 10 5 AND,  14 11 48 LSRI,  5 $FFFF LIT64,  14 14 5 AND,  14 14 5 LSLI,  10 10 14 ORR,  10 9 12 STRW,
      21 21 2 ADDI,  22 22 1 ADDI,  dloop B,
   drdone LBL, ;

\ CODE-literal relocation (fourth relocation class): rebase every captured movz/movk
\ x9 literal whose value pointed into the capture-time code blob [B0,B1) (anonymous
\ quotation entry addresses) by the code delta = seedCP - captureB0. The blob was
\ copied verbatim to CP, so a single delta maps each in-blob code address to its
\ seeded location. Runs while the region is RW (before RX), same as the call patch.
: EM-AOT-RELOC-CODE ( -- )
   LBL LBL {: cloop:label crdone:label :}
   5 LAOTCODEB0 LABEL@ ADR,  5 5 0 LDR,            \ x5 = capture-time code base (B0)
   6 CP 5 SUB,                                     \ x6 = code delta = seedCP - B0 (survives loop)
   21 LAOTCSITES LABEL@ ADR,                       \ x21 = CODE-site cursor (u16 offsets)
   23 LAOTNCSITE LABEL@ ADR,  23 23 0 LDR,         \ x23 = CODE-site count
   22 0 MOVZ,
   cloop LBL,  22 23 CMP,  C-GE crdone BCOND,
      24 21 0 LDRB,  4 21 1 LDRB,  4 4 8 LSLI,  24 24 4 ORR,   \ x24 = blob offset (u16 LE)
      9 CP 24 ADD,                                 \ x9 = literal addr = CP + blob offset
      10 9 0 LDRW,   10 10 5 LSRI,  5 $FFFF LIT64,  10 10 5 AND,  11 10 0 ADDI,
      10 9 4 LDRW,   10 10 5 LSRI,  5 $FFFF LIT64,  10 10 5 AND,  10 10 16 LSLI,  11 11 10 ORR,
      10 9 8 LDRW,   10 10 5 LSRI,  5 $FFFF LIT64,  10 10 5 AND,  10 10 32 LSLI,  11 11 10 ORR,
      10 9 12 LDRW,  10 10 5 LSRI,  5 $FFFF LIT64,  10 10 5 AND,  10 10 48 LSLI,  11 11 10 ORR,
      11 11 6 ADD,                                 \ x11 = value + code delta
      10 9 0 LDRW,   5 $FFE0001F LIT64,  10 10 5 AND,  14 11 0 ADDI,   5 $FFFF LIT64,  14 14 5 AND,  14 14 5 LSLI,  10 10 14 ORR,  10 9 0 STRW,
      10 9 4 LDRW,   5 $FFE0001F LIT64,  10 10 5 AND,  14 11 16 LSRI,  5 $FFFF LIT64,  14 14 5 AND,  14 14 5 LSLI,  10 10 14 ORR,  10 9 4 STRW,
      10 9 8 LDRW,   5 $FFE0001F LIT64,  10 10 5 AND,  14 11 32 LSRI,  5 $FFFF LIT64,  14 14 5 AND,  14 14 5 LSLI,  10 10 14 ORR,  10 9 8 STRW,
      10 9 12 LDRW,  5 $FFE0001F LIT64,  10 10 5 AND,  14 11 48 LSRI,  5 $FFFF LIT64,  14 14 5 AND,  14 14 5 LSLI,  10 10 14 ORR,  10 9 12 STRW,
      21 21 2 ADDI,  22 22 1 ADDI,  cloop B,
   crdone LBL, ;

\ Boot-run the captured top-level entry words (INSTALL/BPW-INSTALL/S-INSTALL) once
\ the seeded blob is RX + icache-flushed: walk the 0-terminated [len][name] list,
\ LFIND each in the now-registered dict, and blr its xt. This replaces the embedded
\ install-tail source -- the engine installs the REPL with zero baked source. Runs
\ at LEX0 on every boot; the entry words self-guard on TTY? so pipe/script boots are
\ no-ops. A missing name is a build/seed bug -> panic exit $52.
: EM-AOT-BOOTRUN ( -- )
   LBL LBL LBL {: bloop:label bdone:label bnf:label :}
   21 LAOTBOOTRUN LABEL@ ADR,                        \ x21 = list cursor
   bloop LBL,
      10 21 0 LDRB,  10 bdone CBZ,                   \ x10 = name len; 0 -> done
      SP SP 16 SUBI,  21 SP 0 STR,  10 SP 8 STR,     \ preserve cursor + len across the call
      9 21 1 ADDI,                                   \ x9 = name ptr = cursor + 1
      LFIND LABEL@ BL,                               \ x11 = xt, x13 = found?
      13 bnf CBZ,
      LAOTWIDGATE LABEL@ BL,                         \ TFAM 2b-v: reject bootrun into a protected WID
      11 BLR,                                        \ call the entry word
      21 SP 0 LDR,  10 SP 8 LDR,  SP SP 16 ADDI,
      21 21 1 ADDI,  21 21 10 ADD,                   \ advance past [len][name]
      bloop B,
   bnf LBL,  0 $52 MOVZ,  NR-EXIT-GROUP SYS,
   bdone LBL, ;

\ Seed the metabuild-captured AOT words at LEXIT: copy the blob, register N dict
\ records, name-relocate the call sites, relocate DATA-address literals, advance CP.
\ Region is RX at LEXIT so the pass toggles RW around all region writes and flushes
\ the icache. LAOTNREC = 0 (stage2/maker/snap: nothing captured) skips the pass.
: EM-SEED-AOT ( -- )
   LBL {: askip:label :}
   11 LAOTNREC LABEL@ ADR,  11 11 0 LDR,            \ x11 = N
   11 askip CBZ,                                    \ nothing captured -> skip
   2 3 MOVZ,  LPROT LABEL@ BL,                       \ region -> RW
   11 LAOTCODELEN LABEL@ ADR,  11 11 0 LDR,         \ x11 = blob length (for the copy)
   EM-AOT-COPY-BLOB
   EM-AOT-REGISTER-RECS
   EM-AOT-REGISTER-PROT-WIDS
   EM-AOT-PATCH-SITES
   EM-AOT-RELOC-DATA
   EM-AOT-RELOC-CODE
   9 CP 0 ADDI,                                     \ x9 = blob base (= CP before advance) for the flush
   11 LAOTCODELEN LABEL@ ADR,  11 11 0 LDR,         \ x11 = blob length again
   CP CP 11 ADD,                                    \ code area top past the blob
   2 5 MOVZ,  LPROT LABEL@ BL,                       \ region -> RX
   LFLUSH LABEL@ BL,                                \ flush icache over [blob base, CP)
   EM-AOT-BOOTRUN                                   \ install the REPL (no source): LFIND+blr the entry words
   askip LBL, ;

: EM-SEED-DICT ( -- )
   LBL LBL {: scopy scdone :}
   DBASE 0 0 ADDI,
   CP DBASE 0 ADDI,  5 DICT-SIZE LIT64,  CP CP 5 ADD,
   11 LNCOUNT LABEL@ ADR,  11 11 0 LDR,  NDICT 11 0 ADDI,
   9 LDICT LABEL@ ADR,  10 DBASE 0 ADDI,  12 11 0 ADDI,
   scopy LBL,
      12 scdone CBZ,
      5 9 0 LDR,  6 9 8 LDR,
      7 XREG-RBASE 5 ADD,  7 10 0 STR,
      6 6 5 SUB,  6 6 4 SUBI,  6 10 8 STR,
      5 9 16 LDR,  5 10 16 STR,
      6 9 24 LDR,
      LBL {: inl-name :}
      8 DNAME-EXT LIT64,  8 5 8 AND,  8 inl-name CBZ,
         6 XREG-RBASE 6 ADD,
      inl-name LBL,
      6 10 24 STR,  5 9 32 LDR,  5 10 32 STR,
      5 9 40 LDR,  5 10 40 STR,
      9 9 DREC ADDI,  10 10 DREC ADDI,  12 12 1 SUBI,  scopy B,
   scdone LBL, ;

TRUSTED: EM-DATA-VA>N ( -- n ) DATA-VA ;

: EM-MMAP-DATA-REGION ( -- )
   LBL {: dvok :}
   0 EM-DATA-VA>N LIT64,  1 DATA-SIZE LIT64,  2 3 MOVZ,  3 MAP-ANON-PRIVATE-FIXED LIT64,  4 0 MOVN,  5 0 MOVZ,
   NR-MMAP SYS,
   5 EM-DATA-VA>N LIT64,  0 5 CMP,
   C-EQ dvok BCOND,
      0 78 MOVZ,  NR-EXIT-GROUP SYS,
   dvok LBL, ;

: EM-DATA-INIT ( -- )
   20 0 RBASE-CELL STR,
   DATA 0 0 ADDI,
   XDS DATA S0-CELL STR,
   13 DATA ARGC-CELL STR,  14 DATA ARGV-CELL STR,  15 DATA ENVP-CELL STR,
   5 DATA-START LIT64,  7 DATA 5 ADD,  7 DATA DP-CELL STR, ;

: EM-SNAPSHOT-COPY-CODE ( -- )
   LBL LBL {: sc1 sc1d :}
   13 DBASE 0 ADDI,  14 0 MOVZ,
   sc1 LBL,  14 6 CMP,  C-GE sc1d BCOND,
      3 8 14 ADD,  3 3 0 LDRB,  4 13 14 ADD,  3 4 0 STRB,
      14 14 1 ADDI,  sc1 B,
   sc1d LBL, ;

: EM-SNAPSHOT-COPY-DATA ( -- )
   LBL LBL {: sc2 sc2d :}
   8 12 7 SUB,  13 DATA 0 ADDI,  14 0 MOVZ,
   sc2 LBL,  14 7 CMP,  C-GE sc2d BCOND,
      3 8 14 ADD,  3 3 0 LDRB,  4 13 14 ADD,  3 4 0 STRB,
      14 14 1 ADDI,  sc2 B,
   sc2d LBL, ;

\ Region walks are BL-callable and parameterized so the loader (live
\ region) and the snapshot writer (scratch copy) share ONE implementation:
\ x8 = region base, x15 = record count, x16 = region code end,
\ x21 = detect base, x22 = detect len, x25 = rebase target base (value - x21 + x25).
: EM-SNAPSHOT-REBASE-DICT ( -- )
   LBL LBL LBL LBL {: sdl2 sdn2 sds2 srn :}
   LSNAPRBD LABEL@ LBL,
   9 8 0 ADDI,  10 0 MOVZ,
   sdl2 LBL,  10 15 CMP,  C-GE sdn2 BCOND,
      13 9 0 LDR,
      13 21 CMP,  C-LT sds2 BCOND,
      14 21 22 ADD,  13 14 CMP,  C-GE sds2 BCOND,
      13 13 21 SUB,  13 13 25 ADD,  13 9 0 STR,
      sds2 LBL,
      13 9 16 LDR,  13 13 DNAME-EXT ANDI,  13 srn CBZ,
      13 9 24 LDR,
      13 21 CMP,  C-LT srn BCOND,
      14 21 22 ADD,  13 14 CMP,  C-GE srn BCOND,
      13 13 21 SUB,  13 13 25 ADD,  13 9 24 STR,
      srn LBL,  9 9 DREC ADDI,  10 10 1 ADDI,  sdl2 B,
   sdn2 LBL,  RET, ;

\ Sealed-WID reject for the AOT boot passes (TFAM 2b-v). x11 = resolved xt on entry;
\ re-derive its record WID (scan dict for [0]==xt, read [40]) and, if that WID is in
\ the protected-WID registry, fail-closed (exit E-SEAL-PACKAGE) -- so a captured
\ relocation callee or boot-run entry name that resolves into a sealed system /
\ generated constructor package is rejected before the call immediate is rewritten
\ or the entry word is executed. Preserves x11; clobbers x5/x6/x9/x13/x14; saves x30
\ for the nested LPROTWIDQ. A not-found xt (no record) skips the guard.
: EM-AOTWIDGATE ( -- )
   LBL LBL LBL {: wscan:label wfound:label wdone:label :}
   LAOTWIDGATE LABEL@ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,  11 SP 8 STR,           \ save return + xt
   5 DBASE 0 ADDI,  6 NDICT 0 ADDI,
   wscan LBL,  6 wdone CBZ,
      14 5 0 LDR,  14 11 CMP,  C-EQ wfound BCOND,        \ record[0] == xt ?
      5 5 DREC ADDI,  6 6 1 SUBI,  wscan B,
   wfound LBL,
      9 5 40 LDR,                                        \ x9 = record WID
      LPROTWIDQ LABEL@ BL,                               \ x13 = protected?
      13 wdone CBZ,
         1 LPROTAOT LABEL@ ADR,  0 2 MOVZ,  2 PROTAOT-MSG-LEN MOVZ,  NR-WRITE SYS,    \ protected WID at AOT boot gate: name it on fd 2 before exit 84
         0 E-SEAL-PACKAGE MOVZ,  NR-EXIT-GROUP SYS,
   wdone LBL,
      30 SP 0 LDR,  11 SP 8 LDR,  SP SP 16 ADDI,  RET, ;

: EM-SNAPSHOT-REBASE-CALLS ( -- )
   LBL LBL LBL {: srl srn srx :}
   LSNAPRBC LABEL@ LBL,
   9 8 0 ADDI,  5 DICT-SIZE LIT64,  9 9 5 ADD,
   srl LBL,  9 16 CMP,  C-GE srx BCOND,
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
      13 13 21 SUB,  13 13 25 ADD,
      10 9 0 LDRW,  5 $FFE0001F LIT64,  10 10 5 AND,
        14 13 0 ADDI,  5 $FFFF LIT64,  14 14 5 AND,  14 14 5 LSLI,  10 10 14 ORR,  10 9 0 STRW,
      10 9 4 LDRW,  5 $FFE0001F LIT64,  10 10 5 AND,
        14 13 16 LSRI,  5 $FFFF LIT64,  14 14 5 AND,  14 14 5 LSLI,  10 10 14 ORR,  10 9 4 STRW,
      10 9 8 LDRW,  5 $FFE0001F LIT64,  10 10 5 AND,
        14 13 32 LSRI,  5 $FFFF LIT64,  14 14 5 AND,  14 14 5 LSLI,  10 10 14 ORR,  10 9 8 STRW,
      9 9 12 ADDI,
   srn LBL,  9 9 4 ADDI,  srl B,
   srx LBL,  RET, ;

\ snap-rebase ( base end count dbase dlen newbase -- ): run both relocation
\ walks over an arbitrary region copy [base,end). Pool registers are spilled before
\ any prim call, so clobbering x8/x15/x16/x21/x22/x25 is safe here. x8=base,
\ x16=end are the write-region endpoints. The half-open span guard rejects every
\ protected-band intersection, including a region that straddles a whole band.
\ The legitimate snapshot builder uses a high scratch mmap copy outside DATA.
: BSNAPREBASE ( -- )
   25 G-POP  22 G-POP  21 G-POP  15 G-POP  16 G-POP  8 G-POP
   11 16 8 SUB,  8 11 GUARD-SPAN
   LSNAPRBD LABEL@ BL,
   LSNAPRBC LABEL@ BL, ;

: EM-SNAPSHOT-RX-FLUSH ( -- )
   2 5 MOVZ,  LPROT LABEL@ BL,
   9 DBASE 0 ADDI,  5 DICT-SIZE LIT64,  9 9 5 ADD,  LFLUSH LABEL@ BL, ;

\ ---- AOT snapshot? (trailer at the end of our own __text). If present:
\ restore both regions verbatim (fixed VAs keep region addresses valid),
\ relocate engine-text call chains (the only ASLR-movers), boot WARM. ----
: EM-SNAPSHOT-RESTORE ( -- )
   LBL LBL LBL LBL LBL LBL {: snomag:label snbad:label snok:label snnew:label snhave:label snbadver:label :}
   24 0 MOVZ,                                       \ x24 = snapshot flag
   9 DATA RBASE-CELL LDR,  25 9 0 ADDI,             \ x25 = live text CONTENT base
   10 9 0 ADDI,  5 $1000 LIT64,  10 10 5 SUB,
   11 10 IMAGE-TEXT-SIZE-OFF LDR,                   \ S = our executable text size
   12 10 11 ADD,  5 IMAGE-TEXT-TRAILER-ADJ LIT64,  12 12 5 ADD,   \ x12 = trailer END (base+SNL+ADJ)
   \ Two-probe trailer detection (dot habu-snapshot-format-ver, item 12 3b): a
   \ 48-byte format-versioned trailer sits at END-48 with the version cell at
   \ +40; a legacy 40-byte trailer at END-40 (version implicitly 0). SNL grows
   \ with the trailer, so END-size lands on the same magic cell in both formats.
   \ A pre-3b engine (fixed END-40 probe) reading a 48-byte image lands on the
   \ text-base=0 field, misses the magic, and cold-boots: fail-closed, never a
   \ hidden-field misread. An image version newer than we support exits 80
   \ (E-SNAP-VERSION), mirroring the snbad rc-79 corrupt-trailer path.
   5 SNAP-MAGIC LIT64,
   13 12 48 SUBI,  14 13 0 LDR,  14 5 CMP,  C-EQ snnew BCOND,      \ x13 = 48-byte trailer base?
   13 12 40 SUBI,  14 13 0 LDR,  14 5 CMP,  C-NE snomag BCOND,     \ x13 = 40-byte trailer base? else cold boot
   snhave B,                                                       \ legacy v0 trailer: no version check
   snnew LBL,
      14 13 40 LDR,                                                \ x14 = image format version
      5 1 MOVZ,  14 5 CMP,  C-GT snbadver BCOND,                   \ x5 = max supported version (SNAP-FORMAT-VERSION)
   snhave LBL,
      12 13 0 ADDI,                                                \ x12 = resolved trailer base
   5 IMAGE-TEXT-CONTENT-ADJ LIT64,  11 11 5 SUB,
   21 12 8 LDR,                                     \ x21 = snapshot-time text base
   15 12 16 LDR,                                    \ x15 = ndict
   6 12 24 LDR,                                     \ x6 = region payload len
   7 12 32 LDR,                                     \ x7 = data payload len
   \ corrupt/truncated trailer must never smear the regions: exit 79
   5 REGION LIT64,  6 5 CMP,  C-GT snbad BCOND,
   5 DATA-SIZE LIT64,  7 5 CMP,  C-GT snbad BCOND,
   5 DICT-CAP MOVZ,  15 5 CMP,  C-GT snbad BCOND,
   snok B,
   snbad LBL,                                                              \ corrupt/truncated trailer: label the fd-2 diagnostic before exit 79
      1 LSNAPBAD LABEL@ ADR,  0 2 MOVZ,  2 SNAPBAD-MSG-LEN MOVZ,  NR-WRITE SYS,
      0 79 MOVZ,  NR-EXIT-GROUP SYS,
   snbadver LBL,                                                           \ E-SNAP-VERSION: image format newer than engine supports; label before exit 80
      1 LSNAPVER LABEL@ ADR,  0 2 MOVZ,  2 SNAPVER-MSG-LEN MOVZ,  NR-WRITE SYS,
      0 80 MOVZ,  NR-EXIT-GROUP SYS,
   snok LBL,
   9 DATA ARGC-CELL LDR,  10 DATA ARGV-CELL LDR,  0 DATA ENVP-CELL LDR,
   22 11 6 SUB,  22 22 7 SUB,  22 22 40 SUBI,       \ x22 = engine text len then
   8 12 7 SUB,  8 8 6 SUB,                          \ region payload src
   EM-SNAPSHOT-COPY-CODE
   EM-SNAPSHOT-COPY-DATA
   25 DATA RBASE-CELL STR,                          \ live values over stale copies
   XDS DATA S0-CELL STR,
   9 DATA ARGC-CELL STR,  10 DATA ARGV-CELL STR,  0 DATA ENVP-CELL STR,
   NDICT 15 0 ADDI,
   CP DBASE 6 ADD,
   8 DBASE 0 ADDI,  16 CP 0 ADDI,
   LSNAPRBD LABEL@ BL,
   LSNAPRBC LABEL@ BL,
   EM-SNAPSHOT-RX-FLUSH
   24 1 MOVZ,
   24 DATA SNAP-CELL STR,
   snomag LBL, ;

: EM-STARTUP-RUNTIME-STATE ( -- )
   LBL {: cwok :}
   9 0 MOVZ,  9 DATA HND-CELL STR,
   9 DATA SNAP-CELL LDR,
   9 cwok CBNZ,

   9 0 MOVZ,  9 DATA CUR-CELL STR,
   9 1 MOVZ,  9 DATA WIDN-CELL STR,
   9 0 MOVZ,  9 DATA HOOK-CELL STR,
   9 0 MOVZ,  9 DATA PROT-WID-N-CELL STR,          \ protected-WID registry starts empty (TFAM 2b-v)
   cwok LBL,  9 0 MOVZ,
   9 DATA PKG-PUB-CELL STR,  9 DATA PKG-PRI-CELL STR,  9 DATA PKG-PARENT-CELL STR,  9 DATA PKG-REC-CELL STR,  9 DATA LOOPSP-CELL STR,
   9 DATA P2-CELL STR,  9 DATA P2BODY0-CELL STR,
   9 DATA P2INP-CELL STR,  9 DATA P2INE-CELL STR,  9 DATA P2DP-CELL STR,
   9 DATA P2W0-CELL STR,  9 DATA P2W1-CELL STR,  9 DATA P2W2-CELL STR,  9 DATA P2W3-CELL STR,
   9 DATA P2LOC0-CELL STR,
   9 DATA TXN-ACTIVE-CELL STR,  9 DATA TXN-SRC-A-CELL STR,  9 DATA TXN-SRC-U-CELL STR,
   9 DATA TXN-CERT-A-CELL STR,  9 DATA TXN-CERT-U-CELL STR,  9 DATA TXN-BIND-I-CELL STR,
   9 DATA TXN-WF-I-CELL STR,  9 DATA TXN-FETCH-I-CELL STR,
   9 DATA TXN-BLOB-A-CELL STR,  9 DATA TXN-BLOB-CAP-CELL STR,
   G-INSTALL-CRASH
   G-INSTALL-TRAP
   9 LDOESPATCH LABEL@ ADR,  9 DATA DOESP-CELL STR,
   9 LCREATE LABEL@ ADR,  9 DATA CREATEP-CELL STR,
   9 LRREC LABEL@ ADR,  9 DATA RRECP-CELL STR,
   9 LMAIN LABEL@ ADR,  9 DATA LMAINP-CELL STR,            \ interpret-loop top (B-EVAL branches here)
   9 LEVALREC LABEL@ ADR,  9 DATA EVALREC-CELL STR,       \ evaluate throw-recovery entry (BTHROW branches here)
   9 LUNCAUGHT LABEL@ ADR,  9 DATA UNCGH-CELL STR,        \ uncaught top-level throw reporter (BTHROW THROW-NOREC branches here)
   LVRINIT LABEL@ BL,  LHIDXBUILD LABEL@ BL,             \ VRTAB/VRITAB fill + dict hash table (data mapped, NDICT final)
   EMIT-SOURCE
   9 0 MOVZ,  9 DATA PEND-CELL STR,
   9 DATA TSIG-A-CELL STR,   9 DATA TSIG-U-CELL STR,
   9 DATA TCSIG-A-CELL STR,  9 DATA TCSIG-U-CELL STR,
   9 DATA CRSIG-A-CELL STR,  9 DATA CRSIG-U-CELL STR,
   9 DATA DOESB-CELL STR,
   9 DATA TRUSTED-CELL STR, ;

: EM-STARTUP ( -- )
   LANCHOR LABEL@ LBL,
   EM-ENTRY-ARGS
   EM-RUNTIME-STACK
   EM-MMAP-CODE-REGION
   EM-SEED-DICT
   \ EM-SEED-AOT moved to EM-COMPILE-EXIT (LEXIT): the AOT words are seeded
   \ post-cold-prefix so name-relocated calls (M2) can resolve cold-prefix words.
   EM-MMAP-DATA-REGION
   EM-DATA-INIT
   EM-SNAPSHOT-RESTORE
   EM-STARTUP-RUNTIME-STATE ;

: EM-COMMENT ( -- )
   LBL LBL LBL {: notcom skln skpar :}
   LMAIN LABEL@ LBL,
      \ depth-floor guard: the just-interpreted top-level word must never leave
      \ the data stack below its base (S0). XDS < S0 is a proven underflow — name
      \ the offending word and fail closed instead of running on garbage / a signal.
      9 DATA S0-CELL LDR,  XDS 9 CMP,  C-LT LUNDERFLOW LABEL@ BCOND,
      LTOK LABEL@ BL,  0 LEXIT LABEL@ CBZ,
      9 DATA TKL-CELL LDR,  9 1 CMPI,  C-NE notcom BCOND,
      9 DATA TKA-CELL LDR,  9 9 0 LDRB,
      9 92 CMPI,  C-EQ skln BCOND,
      9 40 CMPI,  C-NE notcom BCOND,
      skpar LBL,  11 DATA INP-CELL LDR,  12 DATA INE-CELL LDR,  11 12 CMP,  C-GE LMAIN LABEL@ BCOND,
         9 11 0 LDRB,  11 11 1 ADDI,  11 DATA INP-CELL STR,  9 41 CMPI,  C-NE skpar BCOND,  LMAIN LABEL@ B,
      skln LBL,   11 DATA INP-CELL LDR,  12 DATA INE-CELL LDR,  11 12 CMP,  C-GE LMAIN LABEL@ BCOND,
         9 11 0 LDRB,  11 11 1 ADDI,  11 DATA INP-CELL STR,  9 10 CMPI,  C-NE skln BCOND,  LMAIN LABEL@ B,
      notcom LBL,
      9 DATA PEND-CELL LDR,  9 LCOMPILE LABEL@ CBNZ, ;

: EM-INTERPRET-COLON ( label -- ) {: lnotcolon:label :}
   LBL LBL LBL LBL {: cpok ndok kcolon ktry :}
   9 DATA TKL-CELL LDR,  9 1 CMPI,  C-NE ktry BCOND,
   9 DATA TKA-CELL LDR,  9 9 0 LDRB,  9 $3A CMPI,  C-NE ktry BCOND,
   kcolon LBL,
      LBL {: p2ok:label :}
      9 DATA P2-CELL LDR,  9 p2ok CBZ,
         0 2 MOVZ,  1 LP2NEST LABEL@ ADR,  2 33 MOVZ,  NR-WRITE SYS,
         76 C-DIE-TOKEN-NL
      p2ok LBL,
      C-TASK-LIVE-GUARD
      2 3 MOVZ,  LPROT LABEL@ BL,
      9 REGION $4000 - LIT64,  9 DBASE 9 ADD,  CP 9 CMP,  C-LT cpok BCOND,
         C-DIE-CODE-FULL
      cpok LBL,
      9 DICT-CAP MOVZ,  NDICT 9 CMP,  C-LT ndok BCOND,      \ slots end at CFSTK-OFF
         C-DIE-DICT-FULL
      ndok LBL,
      LTOK LABEL@ BL,
      12 0 MOVZ,  12 DATA BODYLEN-CELL STR,
      LBCAP LABEL@ BL,             \ seed with the NAME (checker records certified sigs)
      C-QUALIFY-DEF
      9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
      9 DATA PEND-CELL STR,
      C-STORE-DEF-NAME
      CP 9 0 STR,
      5 CFSTK-OFF LIT64,  11 DBASE 5 ADD,  12 0 MOVZ,  12 11 0 STR,
      12 0 MOVZ,  12 DATA LOCN-CELL STR,  12 DATA LOCF-CELL STR,
      12 DATA CMM-CELL STR,  12 DATA CMFRD-CELL STR,  12 DATA CMBK-CELL STR,
      C-CLEAR-TRUSTED-STATE
      C-COLON-MAYBE-SIG
         9 DATA DP-CELL LDR,  9 DATA P2DP-CELL STR,          \ pass-2 DP watermark
         9 DATA BODYLEN-CELL LDR,  9 DATA P2BODY0-CELL STR,  \ body starts after name+sig
         12 0 MOVZ,  12 DATA VSP-CELL STR,  12 DATA SNAPSP-CELL STR,
         12 DATA EXITH-CELL STR,  12 DATA LVD-CELL STR,
         12 DATA QPATCH-CELL STR,
         12 VRALL MOVZ,  12 DATA VRFREE-CELL STR,
         12 FRALL MOVZ,  12 DATA FRFREE-CELL STR,
         9 $D10043FF LIT64,  LCEMIT LABEL@ BL,
         9 $F90003FE LIT64,  LCEMIT LABEL@ BL,
         LMAIN LABEL@ B,
   ktry LBL,
   0 LKWKERNEL LABEL@ ADR,  1 7 MOVZ,  LKWCMP LABEL@ BL,  0 lnotcolon CBZ,
   kcolon B,
   lnotcolon LBL, ;
s" em-interpret-colon" s" label --" TRUST

: C-CALL-CHECKER-PACKAGE ( -- )
   LCHKPACKAGE 15 C-FIND-GLOBAL
   9 DATA TKA-CELL LDR,  9 G-PUSH
   9 DATA TKL-CELL LDR,  9 G-PUSH
   C-CALL-X11-SAVED ;
s" c-call-checker-package" s" --" TRUST

: C-CALL-CHECKER-PUBLIC ( -- )
   LCHKPUB 14 C-FIND-GLOBAL
   C-CALL-X11-SAVED ;
s" c-call-checker-public" s" --" TRUST

: C-CALL-CHECKER-PRIVATE ( -- )
   LCHKPRI 15 C-FIND-GLOBAL
   C-CALL-X11-SAVED ;
s" c-call-checker-private" s" --" TRUST

: C-CALL-CHECKER-END-PACKAGE ( -- )
   LCHKENDPKG 19 C-FIND-GLOBAL
   C-CALL-X11-SAVED ;
s" c-call-checker-end-package" s" --" TRUST

: C-PACKAGE-FAIL ( n -- ) {: rc:n :}
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 rc MOVZ,  NR-EXIT-GROUP SYS, ;
s" c-package-fail" s" n --" TRUST

: C-PACKAGE-NAME-GUARD ( -- )
   LBL LBL LBL {: scan:label bad:label done:label :}
   14 0 MOVZ,
   scan LBL,
      15 DATA TKL-CELL LDR,  14 15 CMP,  C-GE done BCOND,
      15 DATA TKA-CELL LDR,  15 15 14 ADD,  15 15 0 LDRB,
      15 $3A CMPI,  C-EQ bad BCOND,
      14 14 1 ADDI,  scan B,
   bad LBL,  $4B C-PACKAGE-FAIL
   done LBL, ;
s" c-package-name-guard" s" --" TRUST

: C-PACKAGE-NEW-PRIVATE-WID ( -- )
   12 DATA WIDN-CELL LDR,
   13 12 1 ADDI,  13 DATA WIDN-CELL STR, ;
s" c-package-new-private-wid" s" --" TRUST

: C-PACKAGE-ALLOC-WIDS ( -- )
   17 DATA WIDN-CELL LDR,
   16 17 1 ADDI,
   15 17 2 ADDI,  15 DATA WIDN-CELL STR, ;
s" c-package-alloc-wids" s" --" TRUST

: C-PACKAGE-NEW-RECORD ( -- )
   C-QUALIFY-CAP
   C-PACKAGE-ALLOC-WIDS
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   C-STORE-NAME
   11 DATA WIDN-CELL LDR,  11 11 2 SUBI,
   12 11 1 ADDI,
   11 9 0 STR,  12 9 8 STR,
   15 0 MOVN,  15 9 40 STR,
   NDICT NDICT 1 ADDI,  LHIDXADD LABEL@ BL,
   5 9 0 ADDI, ;
s" c-package-new-record" s" --" TRUST

: C-PACKAGE-EXISTING-PRIVATE ( label -- ) {: done:label :}
   LBL {: havepri:label :}
   12 havepri CBNZ,
      C-PACKAGE-NEW-PRIVATE-WID
      12 5 8 STR,
   havepri LBL,
   done B, ;
s" c-package-existing-private" s" label --" TRUST

: C-PACKAGE-RECORD-MATCH ( label label -- ) {: hit:label miss:label :}
   LBL LBL {: cmp:label inline:label :}
   14 5 40 LDR,  15 0 MOVN,  14 15 CMP,  C-NE miss BCOND,
   14 5 16 LDR,  14 14 4 LSLI,  14 14 4 LSRI,
   15 DATA TKL-CELL LDR,  14 15 CMP,  C-NE miss BCOND,
   16 5 24 ADDI,
   14 5 16 LDR,  14 14 DNAME-EXT ANDI,  14 inline CBZ,
      16 5 24 LDR,
   inline LBL,
   7 0 MOVZ,
   cmp LBL,
      15 DATA TKL-CELL LDR,  7 15 CMP,  C-GE hit BCOND,
      15 16 7 ADD,  15 15 0 LDRB,
      3 15 $41 SUBI,  3 $1A CMPI,  3 C-CC CSET,  3 3 5 LSLI,  15 15 3 ORR,
      4 DATA TKA-CELL LDR,  4 4 7 ADD,  4 4 0 LDRB,
      3 4 $41 SUBI,  3 $1A CMPI,  3 C-CC CSET,  3 3 5 LSLI,  4 4 3 ORR,
      15 4 CMP,  C-NE miss BCOND,
      7 7 1 ADDI,  cmp B, ;
s" c-package-record-match" s" label label --" TRUST

: C-PACKAGE-ENSURE ( -- )
   LBL LBL LBL LBL LBL
   {: loop:label miss:label hit:label make:label done:label :}
   C-PACKAGE-NAME-GUARD
   5 DBASE 0 ADDI,  6 NDICT 0 ADDI,
   loop LBL,
      6 make CBZ,
      hit miss C-PACKAGE-RECORD-MATCH
      hit LBL,
         11 5 0 LDR,  12 5 8 LDR,
         done C-PACKAGE-EXISTING-PRIVATE
      miss LBL,  5 5 DREC ADDI,  6 6 1 SUBI,  loop B,
   make LBL,
      C-PACKAGE-NEW-RECORD
   done LBL, ;
s" c-package-ensure" s" --" TRUST

: C-PACKAGE-PROT-GUARD ( -- )
   LBL LBL LBL LBL {: loop:label miss:label hit:label done:label :}
   5 DBASE 0 ADDI,  6 NDICT 0 ADDI,
   loop LBL,
      6 done CBZ,
      hit miss C-PACKAGE-RECORD-MATCH
      hit LBL,
         9 5 0 LDR,  LPROTWIDQ LABEL@ BL,
         13 done CBZ,
         C-SEAL-PACKAGE-FAIL
      miss LBL,
         5 5 DREC ADDI,  6 6 1 SUBI,  loop B,
   done LBL, ;
s" c-package-prot-guard" s" --" TRUST

: C-PACKAGE-SEAL-GUARD ( -- )   \ reject `package NAME` open/reopen of a sealed system package
   LBL {: ok:label :}
   9 DATA FRIEND-LATCH-CELL LDR,  9 ok CBZ,             \ friend/open -> allow (engine cold load)
   24 DATA TKL-CELL LDR,  C-SEAL-MATCH                  \ candidate len = TKL; fail if reserved
   C-PACKAGE-PROT-GUARD
   ok LBL, ;
s" c-package-seal-guard" s" --" TRUST

: C-PACKAGE ( -- )
   C-TASK-LIVE-GUARD
   LBL LBL {: inactive:label hastok:label :}
   9 DATA PKG-PUB-CELL LDR,  9 inactive CBZ,
      $4B C-PACKAGE-FAIL
   inactive LBL,
   LTOK LABEL@ BL,  0 hastok CBNZ,
      $4A C-PACKAGE-FAIL
   hastok LBL,
   C-CALL-CHECKER-PACKAGE
   C-PACKAGE-SEAL-GUARD
   2 3 MOVZ,  LPROT LABEL@ BL,
   C-PACKAGE-ENSURE
   2 5 MOVZ,  LPROT LABEL@ BL,
   9 DATA CUR-CELL LDR,  9 DATA PKG-PARENT-CELL STR,
   11 DATA PKG-PUB-CELL STR,  12 DATA PKG-PRI-CELL STR,
   5 DATA PKG-REC-CELL STR,
   12 DATA CUR-CELL STR, ;
s" c-package" s" --" TRUST

: C-PUBLIC ( -- )
   C-TASK-LIVE-GUARD
   LBL {: active:label :}
   9 DATA PKG-PUB-CELL LDR,  9 active CBNZ,
      $4B C-PACKAGE-FAIL
   active LBL,
   C-CALL-CHECKER-PUBLIC
   9 DATA PKG-PUB-CELL LDR,
   9 DATA CUR-CELL STR, ;
s" c-public" s" --" TRUST

: C-PRIVATE ( -- )
   C-TASK-LIVE-GUARD
   LBL {: active:label :}
   9 DATA PKG-PRI-CELL LDR,  9 active CBNZ,
      $4B C-PACKAGE-FAIL
   active LBL,
   C-CALL-CHECKER-PRIVATE
   9 DATA PKG-PRI-CELL LDR,
   9 DATA CUR-CELL STR, ;
s" c-private" s" --" TRUST

: C-END-PACKAGE ( -- )
   C-TASK-LIVE-GUARD
   LBL {: active:label :}
   9 DATA PKG-PUB-CELL LDR,  9 active CBNZ,
      $4B C-PACKAGE-FAIL
   active LBL,
   C-CALL-CHECKER-END-PACKAGE
   9 DATA PKG-PARENT-CELL LDR,  9 DATA CUR-CELL STR,
   9 0 MOVZ,
   9 DATA PKG-PUB-CELL STR,  9 DATA PKG-PRI-CELL STR,
   9 DATA PKG-PARENT-CELL STR,  9 DATA PKG-REC-CELL STR, ;
s" c-end-package" s" --" TRUST

: C-CALL-CHECKER-EXPORT ( -- )
   LCHKEXPORT 14 C-FIND-GLOBAL
   9 DATA TKA-CELL LDR,  9 G-PUSH
   9 DATA TKL-CELL LDR,  9 G-PUSH
   C-CALL-X11-SAVED ;
s" c-call-checker-export" s" --" TRUST

\ Rewrite TKA/TKL to the tail of a NAME:tail token (DEF-TKA/DEF-TKL hold the
\ original spelling). A leading/trailing first colon keeps the token an
\ ordinary name (FIND parity); a malformed double-colon token never reaches
\ here (LFIND already rejected it as undefined).
: C-EXPORT-TAIL! ( -- )
   LBL LBL LBL {: scan:label have:label done:label :}
   9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  17 0 MOVZ,
   scan LBL,
      17 10 CMP,  C-GE done BCOND,                     \ no ':' -> bare name
      14 9 17 ADD,  14 14 0 LDRB,  14 $3A CMPI,  C-EQ have BCOND,
      17 17 1 ADDI,  scan B,
   have LBL,
      17 done CBZ,                                     \ leading ':' -> ordinary
      14 17 1 ADDI,  14 10 CMP,  C-GE done BCOND,      \ trailing ':' -> ordinary
      11 9 17 ADD,  11 11 1 ADDI,  11 DATA TKA-CELL STR,
      12 10 17 SUB,  12 12 1 SUBI,  12 DATA TKL-CELL STR,
   done LBL, ;
s" c-export-tail!" s" --" TRUST

\ EXPORT keyword (dot habu-compiler-pkg-re-688212c1). Two documented roles
\ split by package context, mirrored 1:1 by verify-source RECORD-EXPORT:
\ - INSIDE an open package: publish an EXISTING word under its own tail into
\   the CURRENT section wordlist — same code pointer, same body span,
\   immediate/wide name bits copied — no forwarding body, zero runtime cost.
\ - TOP LEVEL: the pre-existing hb-build --repl export directive surface
\   (`EXPORT word…` keeps extra words callable; lib/prelude.f rides it).
\   COMMENT-EXPORTS strips these lines before hb-build compiles, and a plain
\   load consumes the name as a no-op so directive-carrying programs stay
\   directly loadable.
\ Checker sync mirrors C-PACKAGE (CHECKER-EXPORT sees the ORIGINAL spelling
\ and throws on unsigned/prim sources). Native walls that hold without the
\ hook: missing name ($4A), sealed source prefix (C-QUALIFY-SEAL-GUARD,
\ E-SEAL-PACKAGE), undefined word (token + rc 70), duplicate tail
\ (C-REJECT-DUP-DEF $4E, checked BEFORE the checker call so the labeled
\ native diagnosis wins), protected-WID target (C-STORE-DEF-NAME publish
\ guard). The checker call runs OUTSIDE the RW code window (checked code must
\ execute RX); the record publish sits inside the 3/5 LPROT window because
\ C-STORE-NAME spills long names at CP.
: C-EXPORT ( -- )
   C-TASK-LIVE-GUARD
   LBL LBL LBL LBL LBL {: active:label dnamed:label named:label found:label done:label :}
   9 DATA PKG-PUB-CELL LDR,  9 active CBNZ,
      LTOK LABEL@ BL,  0 dnamed CBNZ,
         $4A C-PACKAGE-FAIL
      dnamed LBL,
      done B,
   active LBL,
   LTOK LABEL@ BL,  0 named CBNZ,
      $4A C-PACKAGE-FAIL
   named LBL,
   C-QUALIFY-SEAL-GUARD
   9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFIND LABEL@ BL,
   13 found CBNZ,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 70 MOVZ,  NR-EXIT-GROUP SYS,
   found LBL,
   SP SP 32 SUBI,  11 SP 0 STR,  12 SP 8 STR,  13 SP 16 STR,
   11 DATA TKA-CELL LDR,  11 DATA DEF-TKA-CELL STR,
   12 DATA TKL-CELL LDR,  12 DATA DEF-TKL-CELL STR,
   14 DATA CUR-CELL LDR,  14 DATA DEF-WL-CELL STR,
   C-EXPORT-TAIL!
   C-QUALIFY-CAP
   C-REJECT-DUP-DEF                                      \ native dup wall first: labeled diagnosis + $4E
   11 DATA DEF-TKA-CELL LDR,  11 DATA TKA-CELL STR,      \ checker sees the ORIGINAL spelling
   12 DATA DEF-TKL-CELL LDR,  12 DATA TKL-CELL STR,
   C-CALL-CHECKER-EXPORT
   2 3 MOVZ,  LPROT LABEL@ BL,
   C-EXPORT-TAIL!                                        \ tail again for the publish (pure rescan)
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   C-STORE-DEF-NAME
   14 SP 0 LDR,  14 9 0 STR,                            \ [0] = source code ptr
   14 SP 8 LDR,  14 9 8 STR,                            \ [8] = source body len
   14 SP 16 LDR,                                        \ x14 = LFIND flags
   15 9 16 LDR,
   16 14 2 ANDI,  16 16 59 LSLI,  15 15 16 ORR,         \ flag bit1 -> DNAME-IMM
   16 14 8 ANDI,  16 16 59 LSLI,  15 15 16 ORR,         \ flag bit3 -> DNAME-WIDE
   15 9 16 STR,
   NDICT NDICT 1 ADDI,  LHIDXADD LABEL@ BL,
   2 5 MOVZ,  LPROT LABEL@ BL,
   SP SP 32 ADDI,
   done LBL, ;
s" c-export" s" --" TRUST

: EM-INTERPRET-DEFINE-KEYWORDS ( -- )
   s" package" KEEP? IF LMAIN LABEL@ LKWPACKAGE 7 ['] C-PACKAGE CF-ENTRY THEN
   s" public" KEEP? IF LMAIN LABEL@ LKWPUBLIC 6 ['] C-PUBLIC CF-ENTRY THEN
   s" private" KEEP? IF LMAIN LABEL@ LKWPRIVATE 7 ['] C-PRIVATE CF-ENTRY THEN
   s" end-package" KEEP? IF LMAIN LABEL@ LKWENDPACKAGE 11 ['] C-END-PACKAGE CF-ENTRY THEN
   s" export" KEEP? IF LMAIN LABEL@ LKWEXPORT 6 ['] C-EXPORT CF-ENTRY THEN
   s" trusted:" KEEP? IF LMAIN LABEL@ LKWTRUSTED 8 ['] C-TRUSTED CF-ENTRY THEN
   s" defer" KEEP? IF LMAIN LABEL@ LKWDEFER 5 ['] C-DEFER CF-ENTRY THEN
   s" create" KEEP? IF LMAIN LABEL@ LKWCREATE 6 ['] C-CREATE   CF-ENTRY THEN
   s" variable" KEEP? IF LMAIN LABEL@ LKWVAR    8 ['] C-VARIABLE CF-ENTRY THEN
   s" constant" KEEP? IF LMAIN LABEL@ LKWCONST  8 ['] C-CONSTANT CF-ENTRY THEN
   s" '" KEEP? IF LMAIN LABEL@ LKWTICK   1 ['] C-TICK     CF-ENTRY THEN
   s" char" KEEP? IF LMAIN LABEL@ LKWCHAR   4 ['] C-CHAR     CF-ENTRY THEN
   s" immediate" KEEP? IF LMAIN LABEL@ LKWIMM    9 ['] C-IMMEDIATE CF-ENTRY THEN ;
s" em-interpret-define-keywords" s" --" TRUST

: EM-INTERPRET-STRING-KEYWORDS ( -- )
   LMAIN LABEL@ LKWSQ     2 ['] C-ISDQ     CF-ENTRY
   LMAIN LABEL@ LKWCQ     2 ['] C-ICQ      CF-ENTRY
   LMAIN LABEL@ LKWDOTQ   2 ['] C-IDOTQ    CF-ENTRY
   LMAIN LABEL@ LKWESQ    3 ['] C-EISDQ    CF-ENTRY
   LMAIN LABEL@ LKWECQ    3 ['] C-EICQ     CF-ENTRY
   LMAIN LABEL@ LKWEDOTQ  3 ['] C-EIDOTQ   CF-ENTRY ;
s" em-interpret-string-keywords" s" --" TRUST

: EM-INTERPRET-NUMBER ( label -- ) {: lnotnum:label :}
   9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LNUM LABEL@ BL,
   12 lnotnum CBZ,  11 G-PUSH  LMAIN LABEL@ B, ;
s" em-interpret-number" s" label --" TRUST

: EM-INTERPRET-FIND ( -- )
   9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFIND LABEL@ BL,
   13 LUNDEF LABEL@ CBZ,
   14 13 8 ANDI,  14 LWIDE LABEL@ CBNZ,                \ DNAME-WIDE effect (TFAM): fail closed, never land a bundle on the interpret stack (x13 still holds the LFIND dict flags)
   14 13 16 ANDI,  14 LINTERNAL LABEL@ CBNZ,           \ DNAME-INT: engine-internal word with no checker-known effect - fail closed before the body runs on the untyped interpret stack
   LARITY LABEL@ BL,          \ pre-exec arity guard (fable): deref/execute prims fault on a shallow
   11 BLR,  LMAIN LABEL@ B, ; \ stack before the LMAIN depth-floor guard sees it; diverts to LUNDERFLOW
s" em-interpret-find" s" --" TRUST

: EM-INTERPRET-WORDS ( -- )
   LBL {: lnotnum :}
   EM-INTERPRET-DEFINE-KEYWORDS
   EM-INTERPRET-STRING-KEYWORDS
   lnotnum EM-INTERPRET-NUMBER
   lnotnum LBL,
   EM-INTERPRET-FIND ;
s" em-interpret-words" s" --" TRUST

: EM-INTERPRET ( -- )
   LBL {: lnotcolon :}
   lnotcolon EM-INTERPRET-COLON
   EM-INTERPRET-WORDS ;
s" em-interpret" s" --" TRUST

: EM-COMPILE-DROP-LOCALS ( -- )
   LBL {: done :}
   12 DATA LOCF-CELL LDR,  12 done CBZ,
      9 $910003FF LIT64,  14 12 10 LSLI,  9 9 14 ORR,  LCEMIT LABEL@ BL,
   done LBL, ;
s" em-compile-drop-locals" s" --" TRUST

: EM-COMPILE-RET ( -- )
   9 $F94003FE LIT64,  LCEMIT LABEL@ BL,
   9 $910043FF LIT64,  LCEMIT LABEL@ BL,
   9 W-RET LIT64,  LCEMIT LABEL@ BL, ;
s" em-compile-ret" s" --" TRUST

\ ---- item 12 slice 3b: pass-2 width-aware transport lowering ---------------
\ ONE mechanism for every transport tier (register shuffle, inline rs keyword,
\ dictionary leaf prim): at a pass-2 transport token whose operands include a
\ wider-than-cell group, force LVSPILL and emit fixed-shape memory cell loops
\ on the live stack ([x19], top at -8). Widths are compile-time constants, so
\ every loop shape is constant and each branch displacement is a meta-time
\ constant — no runtime patching, no new dictionary prims, no scratch region.
\ The loop bodies are emitted by BL-able engine helpers (LP2COPY/LP2DROPN/
\ LP2REV/LP2ROT/LP2RS below); each op is a group-permutation composition of
\ push-copy, pop, span-rotation (three in-place reversals), and rstk block move.

\ LP2COPY ( x5=len-cells x6=src-off-cells ) : emit a push of len cells starting
\ src-off cells below the top — a whole-group copy, bottom cell first.
: EMIT-P2-COPY ( -- )
   LP2COPY LABEL@ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   8 $D2800009 LIT64,  7 5 5 LSLI,  9 8 7 ORR,  LCEMIT LABEL@ BL,   \ movz x9,#len
   8 $D100026A LIT64,  7 6 13 LSLI,  9 8 7 ORR,  LCEMIT LABEL@ BL,  \ sub x10,x19,#off*8
   $F940014B C-EMITW                                                \ ldr x11,[x10]
   $9100214A C-EMITW                                                \ add x10,x10,#8
   $F900026B C-EMITW                                                \ str x11,[x19]
   W-PUSH1 C-EMITW                                                  \ add x19,x19,#8
   $F1000529 C-EMITW                                                \ subs x9,x9,#1
   $54FFFF61 C-EMITW                                                \ b.ne loop (-5)
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ LP2DROPN ( x5=cells ) : emit a pop of the top span.
: EMIT-P2-DROPN ( -- )
   LP2DROPN LABEL@ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   8 $D1000273 LIT64,  7 5 13 LSLI,  9 8 7 ORR,  LCEMIT LABEL@ BL,  \ sub x19,x19,#n*8
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ LP2REV ( x5=lo-off-bytes x6=hi-off-bytes ) : emit an in-place reversal of the
\ cells from x19-lo up to x19-hi (two-pointer swap; empty/1-cell span is a noop).
: EMIT-P2-REV ( -- )
   LP2REV LABEL@ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   8 $D100026A LIT64,  7 5 10 LSLI,  9 8 7 ORR,  LCEMIT LABEL@ BL,  \ sub x10,x19,#lo
   8 $D100026B LIT64,  7 6 10 LSLI,  9 8 7 ORR,  LCEMIT LABEL@ BL,  \ sub x11,x19,#hi
   $EB0B015F C-EMITW                                                \ cmp x10,x11
   $54000102 C-EMITW                                                \ b.hs done (+8)
   $F940014C C-EMITW                                                \ ldr x12,[x10]
   $F940016D C-EMITW                                                \ ldr x13,[x11]
   $F900014D C-EMITW                                                \ str x13,[x10]
   $F900016C C-EMITW                                                \ str x12,[x11]
   $9100214A C-EMITW                                                \ add x10,x10,#8
   $D100216B C-EMITW                                                \ sub x11,x11,#8
   $17FFFFF8 C-EMITW                                                \ b cmp (-8)
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ LP2ROT ( x5=T-cells x6=k-cells ) : emit a left-rotation of the top T cells
\ moving the bottom k cells to the top — triple reversal, whole-group order
\ preserving (== the checker's group permutation for swap/rot/-rot/2swap).
: EMIT-P2-ROT ( -- )
   LP2ROT LABEL@ LBL,
   SP SP 32 SUBI,  30 SP 0 STR,  5 SP 8 STR,  6 SP 16 STR,
   5 5 3 LSLI,
   7 SP 16 LDR,  6 SP 8 LDR,  6 6 7 SUB,  6 6 3 LSLI,  6 6 8 ADDI,
   LP2REV LABEL@ BL,                                  \ reverse the bottom k cells
   7 SP 16 LDR,  5 SP 8 LDR,  5 5 7 SUB,  5 5 3 LSLI,  6 8 MOVZ,
   LP2REV LABEL@ BL,                                  \ reverse the top T-k cells
   5 SP 8 LDR,  5 5 3 LSLI,  6 8 MOVZ,
   LP2REV LABEL@ BL,                                  \ reverse the whole span
   30 SP 0 LDR,  SP SP 32 ADDI,  RET, ;

\ LP2RS ( x5=T-cells x6=mode ) : emit a block transfer between the data stack
\ and the return-stack region ([x20+RSTK-OFF], depth at [x20+RSP-CELL]),
\ ascending order preserved. mode 0 = data->rstk pop (>r/2>r), 1 = rstk->data
\ pop (r>/2r>), 2 = rstk->data copy (r@/2r@). T=1 reproduces J-TOR/J-RFROM/
\ J-RFETCH cell order; T=2 reproduces B2TOR/B2RFROM/B2RFETCH.
: EMIT-P2-RS ( -- )
   LBL LBL {: rsto:label rsdone:label :}
   LP2RS LABEL@ LBL,
   SP SP 32 SUBI,  30 SP 0 STR,  5 SP 8 STR,  6 SP 16 STR,
   10 20 RSP-CELL W-LDRX C-EMITW                      \ ldr x10,[x20,#RSP-CELL]
   6 rsto CBZ,
   \ modes 1/2: x10 -= T; x11 = block base; copy T cells rstk->data
   8 $D100014A LIT64,  5 SP 8 LDR,  7 5 10 LSLI,  9 8 7 ORR,  LCEMIT LABEL@ BL,
   $8B0A0E8B C-EMITW                                  \ add x11,x20,x10,lsl#3
   8 $D2800009 LIT64,  5 SP 8 LDR,  7 5 5 LSLI,  9 8 7 ORR,  LCEMIT LABEL@ BL,
   13 11 RSTK-OFF W-LDRX C-EMITW                      \ ldr x13,[x11,#RSTK-OFF]
   $9100216B C-EMITW                                  \ add x11,x11,#8
   $F900026D C-EMITW                                  \ str x13,[x19]
   W-PUSH1 C-EMITW                                    \ add x19,x19,#8
   $F1000529 C-EMITW                                  \ subs x9,x9,#1
   $54FFFF61 C-EMITW                                  \ b.ne loop (-5)
   6 SP 16 LDR,  6 2 CMPI,  C-EQ rsdone BCOND,        \ r@/2r@ keep the depth
   10 20 RSP-CELL W-STRX C-EMITW                      \ str x10,[x20,#RSP-CELL]
   rsdone B,
   rsto LBL,
   \ mode 0: x11 = rstk top; copy T cells data->rstk, pop, depth += T
   $8B0A0E8B C-EMITW
   8 $D100026C LIT64,  5 SP 8 LDR,  7 5 13 LSLI,  9 8 7 ORR,  LCEMIT LABEL@ BL,   \ sub x12,x19,#T*8
   8 $D2800009 LIT64,  5 SP 8 LDR,  7 5 5 LSLI,  9 8 7 ORR,  LCEMIT LABEL@ BL,
   $F940018D C-EMITW                                  \ ldr x13,[x12]
   $9100218C C-EMITW                                  \ add x12,x12,#8
   13 11 RSTK-OFF W-STRX C-EMITW                      \ str x13,[x11,#RSTK-OFF]
   $9100216B C-EMITW                                  \ add x11,x11,#8
   $F1000529 C-EMITW                                  \ subs x9,x9,#1
   $54FFFF61 C-EMITW                                  \ b.ne loop (-5)
   8 $D1000273 LIT64,  5 SP 8 LDR,  7 5 13 LSLI,  9 8 7 ORR,  LCEMIT LABEL@ BL,   \ sub x19,x19,#T*8
   8 $9100014A LIT64,  5 SP 8 LDR,  7 5 10 LSLI,  9 8 7 ORR,  LCEMIT LABEL@ BL,   \ add x10,x10,#T
   10 20 RSP-CELL W-STRX C-EMITW
   rsdone LBL,
   30 SP 0 LDR,  SP SP 32 ADDI,  RET, ;

\ LP2FETCH ( x5=width-cells ) : emit `@` for one layout bundle. Runtime pops
\ the typed base address, reads memory in ascending slot order, and pushes
\ slot0..tag so the tag is again the top physical cell.
: EMIT-P2-FETCH ( -- )
   LP2FETCH LABEL@ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   $D1002273 C-EMITW                                  \ sub x19,x19,#8
   $F940026A C-EMITW                                  \ ldr x10,[x19] : base
   8 $D2800009 LIT64,  7 5 5 LSLI,  9 8 7 ORR,  LCEMIT LABEL@ BL,
   $F940014B C-EMITW                                  \ ldr x11,[x10]
   $9100214A C-EMITW                                  \ add x10,x10,#8
   $F900026B C-EMITW                                  \ str x11,[x19]
   W-PUSH1 C-EMITW
   $F1000529 C-EMITW                                  \ subs x9,x9,#1
   $54FFFF61 C-EMITW                                  \ b.ne loop (-5)
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ LP2VEXEC is called by a compiled typed fetch before its address is popped.
\ LR points at the B-over-descriptor instruction, so the descriptor begins at
\ LR+4. Every active CHECK validates an unsigned declaration-order tag domain;
\ guard mismatches skip that nested CHECK while still consuming its rows.
: EMIT-P2-VALID-EXEC ( -- )
   LBL LBL LBL LBL LBL LBL {: outer:label guard:label active:label inactive:label invalid:label done:label :}
   LP2VEXEC LABEL@ LBL,
   15 30 4 ADDI,                                      \ descriptor follows return-site B
   9 15 0 LDR,  15 15 8 ADDI,
   19 19 8 SUBI,  10 19 0 LDR,  19 19 8 ADDI,        \ typed base; stack unchanged
   outer LBL,
      9 done CBZ,
      11 15 0 LDR,                                    \ tag cell offset
      12 15 8 LDR,                                    \ exclusive domain limit
      13 15 16 LDR,
      15 15 LOWER-CERT:CHECK-CELLS cells ADDI,       \ guard count; next row
   guard LBL,
      13 active CBZ,
      16 15 0 LDR,  17 15 8 LDR,  14 15 16 LDR,
      15 15 LOWER-CERT:GUARD-CELLS cells ADDI,       \ guard off, tag, certified domain
      13 13 1 SUBI,
      16 16 3 LSLI,  16 10 16 ADD,  16 16 0 LDR,
      16 14 CMP,
      C-CS invalid BCOND,
      16 17 CMP,
      C-NE inactive BCOND,
      guard B,
   active LBL,
      16 11 3 LSLI,  16 10 16 ADD,  16 16 0 LDR,
      16 12 CMP,
      C-CS invalid BCOND,
      9 9 1 SUBI,
      outer B,
   inactive LBL,
      16 LOWER-CERT:GUARD-CELLS cells MOVZ,  16 13 16 MUL,
      15 15 16 ADD,                                   \ consume remaining guard triples
      9 9 1 SUBI,
      outer B,
   invalid LBL,
   0 2 MOVZ,
   1 LVPBADMSG LABEL@ ADR,  2 18 MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 LOPENNL LABEL@ ADR,  2 1 MOVZ,  NR-WRITE SYS,
   0 E-BAD-TAG MOVZ,  NR-EXIT-GROUP SYS,
   done LBL,
   RET, ;

\ LP2VEMIT reads the current fetch descriptor from the frozen lowering
\ certificate, copies its cell stream inline after a B-over-data instruction,
\ and emits an absolute call to LP2VEXEC. No live checker state is consulted.
: EMIT-P2-VALID-EMIT ( -- )
   LBL LBL LBL {: copy:label patch:label done:label :}
   LP2VEMIT LABEL@ LBL,
   SP SP 48 SUBI,  30 SP 0 STR,
   9 0 MOVZ,  9 SP 24 STR,
   9 DATA TKA-CELL LDR,
   10 DATA TXN-SRC-A-CELL LDR,
   9 9 10 SUB,
   LP2CDESC LABEL@ BL,
   11 SP 8 STR,  12 SP 16 STR,
   12 done CBZ,
   11 LP2VEXEC LABEL@ ADR,
   C-CALL-EMIT-ABSOLUTE
   14 CP 0 ADDI,  14 SP 24 STR,
   $14000000 C-EMITW
   11 SP 8 LDR,  12 SP 16 LDR,
   copy LBL,
      12 patch CBZ,
      9 11 0 LDRW,  LCEMIT LABEL@ BL,
      11 11 4 ADDI,  12 12 4 SUBI,
      copy B,
   patch LBL,
   9 SP 24 LDR,
   LPAT LABEL@ BL,
   done LBL,
   30 SP 0 LDR,  SP SP 48 ADDI,  RET, ;

\ LP2STORE ( x5=width-cells ) emits one layout-bundle store. The generated word
\ calls LPROTSPAN once with x10=destination and x11=width*8 before any mutation;
\ that ABI preserves the live bundle registers and rejects whole-span crossing.
: EMIT-P2-STORE ( -- )
   LP2STORE LABEL@ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,  5 SP 8 STR,
   $D1002273 C-EMITW                                  \ sub x19,x19,#8
   $F940026A C-EMITW                                  \ ldr x10,[x19] : dst
   8 $D100026E LIT64,  7 5 13 LSLI,  9 8 7 ORR,  LCEMIT LABEL@ BL,   \ sub x14,x19,#W*8
   8 $D2800009 LIT64,  7 5 5 LSLI,  9 8 7 ORR,  LCEMIT LABEL@ BL,
   8 $D280000B LIT64,  7 5 8 LSLI,  9 8 7 ORR,  LCEMIT LABEL@ BL,    \ mov x11,#W*8
   11 LPROTSPAN LABEL@ ADR,
   C-CALL-EMIT-ABSOLUTE
   $F94001CF C-EMITW                                  \ ldr x15,[x14]
   $F900014F C-EMITW                                  \ str x15,[x10]
   $910021CE C-EMITW                                  \ add x14,x14,#8
   $9100214A C-EMITW                                  \ add x10,x10,#8
   $F1000529 C-EMITW                                  \ subs x9,x9,#1
   $54FFFF61 C-EMITW                                  \ b.ne store (-5)
   5 SP 8 LDR,
   8 $D1000273 LIT64,  7 5 13 LSLI,  9 8 7 ORR,  LCEMIT LABEL@ BL,   \ sub x19,x19,#W*8
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

package LOWER-TXN-LOOKUP
private

: EMIT-WIDTH-LOOKUP ( -- )
   LBL LBL LBL LBL {: missing:label keyeq:label found:label drift:label :}
   LP2CWAT LABEL@ LBL,                              \ x9=source offset, x10=operand; x10=width
   14 10 0 ADDI,
   11 DATA TXN-CERT-A-CELL LDR,
   12 11 LOWER-CERT:WF-COUNT-CELL cells LDR,
   13 DATA TXN-WF-I-CELL LDR,
   13 12 CMP,  C-EQ missing BCOND,  C-HI drift BCOND,
   15 LOWER-CERT:WF-CELLS cells MOVZ,
   15 13 15 MUL,
   11 11 LOWER-CERT:HEADER-CELLS cells ADDI,
   11 11 15 ADD,
   15 11 0 LDR,  9 15 CMP,  C-EQ keyeq BCOND,  C-HI drift BCOND,
   missing LBL,  10 1 MOVZ,  RET,
   keyeq LBL,
      15 11 1 cells LDR,  14 15 CMP,  C-EQ found BCOND,  C-HI drift BCOND,
      10 1 MOVZ,  RET,
   found LBL,
      10 11 2 cells LDR,
      13 13 1 ADDI,  13 DATA TXN-WF-I-CELL STR,  RET,
   drift LBL,  LOWER-TXN-CODE:DRIFT 37 LOWER-TXN-CODE:FAIL ;

: EMIT-DESC-LOOKUP ( -- )
   LBL LBL LBL {: missing:label found:label drift:label :}
   LP2CDESC LABEL@ LBL,                             \ x9=source offset; x11=descriptor, x12=bytes
   10 DATA TXN-CERT-A-CELL LDR,
   13 10 LOWER-CERT:WF-COUNT-CELL cells LDR,        \ WF rows
   14 10 LOWER-CERT:BIND-COUNT-CELL cells LDR,      \ bind widths
   15 10 LOWER-CERT:FETCH-COUNT-CELL cells LDR,     \ fetch rows
   11 10 LOWER-CERT:HEADER-CELLS cells ADDI,
   12 LOWER-CERT:WF-CELLS cells MOVZ,
   13 13 12 MUL,  11 11 13 ADD,
   14 14 3 LSLI,  11 11 14 ADD,
   14 DATA TXN-FETCH-I-CELL LDR,
   14 15 CMP,  C-EQ missing BCOND,  C-HI drift BCOND,
   13 LOWER-CERT:FETCH-CELLS cells MOVZ,
   13 14 13 MUL,  11 11 13 ADD,
   13 11 0 LDR,  9 13 CMP,  C-EQ found BCOND,  C-HI drift BCOND,
   missing LBL,  11 0 MOVZ,  12 0 MOVZ,  RET,
   found LBL,
      13 11 1 cells LDR,  12 11 2 cells LDR,
      13 13 3 LSLI,  11 10 13 ADD,
      12 12 3 LSLI,
      14 14 1 ADDI,  14 DATA TXN-FETCH-I-CELL STR,  RET,
   drift LBL,  LOWER-TXN-CODE:DRIFT 37 LOWER-TXN-CODE:FAIL ;

public

: EMIT-LOOKUPS ( -- )
   EMIT-WIDTH-LOOKUP
   EMIT-DESC-LOOKUP ;

end-package

: EMIT-P2-HELPERS ( -- )
   EMIT-P2-COPY  EMIT-P2-DROPN  EMIT-P2-REV  EMIT-P2-ROT  EMIT-P2-RS
   LOWER-TXN-LOOKUP:EMIT-LOOKUPS
   EMIT-P2-VALID-EXEC  EMIT-P2-VALID-EMIT  EMIT-P2-FETCH  EMIT-P2-STORE ;

\ keyword/certificate-name bytes for the pass-2 dispatch (the shuffle-op and rs
\ keyword names reuse the jit.f/loop-keyword labels).
: EMIT-P2KW ( -- )
   LCERTBYTES LABEL@ LBL, s" lower-cert:bytes" BYTES,
   LVPBADMSG LABEL@ LBL, s" hb: bad layout tag" BYTES,
   LP2NEST LABEL@ LBL, s" hb: nested definition in pass 2: " BYTES,
   LOWER-TXN-CODE:BAD LABEL@ LBL, s" hb: malformed lowering certificate" BYTES,
   LOWER-TXN-CODE:MEM LABEL@ LBL, s" hb: lowering transaction memory failed" BYTES,
   LOWER-TXN-CODE:FULL LABEL@ LBL, s" hb: lowering transaction full" BYTES,
   LOWER-TXN-CODE:DRIFT LABEL@ LBL, s" hb: lowering certificate replay drift" BYTES,
   LKWTUCK3 LABEL@ LBL,   s" tuck" BYTES,   LKWROT3 LABEL@ LBL,   s" rot" BYTES,
   LKWMROT3 LABEL@ LBL,   s" -rot" BYTES,   LKW2DUP3 LABEL@ LBL,  s" 2dup" BYTES,
   LKW2DROP3 LABEL@ LBL,  s" 2drop" BYTES,  LKW2SWAP3 LABEL@ LBL, s" 2swap" BYTES,
   LKW2OVER3 LABEL@ LBL,  s" 2over" BYTES,  LKW2TOR3 LABEL@ LBL,  s" 2>r" BYTES,
   LKW2RFROM3 LABEL@ LBL, s" 2r>" BYTES,    LKW2RFET3 LABEL@ LBL, s" 2r@" BYTES,
   LKWAT2 LABEL@ LBL, s" @" BYTES,  LKWSTORE2 LABEL@ LBL, s" !" BYTES, ;

: EM-P2-W-CELL ( n -- n )          \ DATA offset holding operand pos's width
   dup 0 = IF drop P2W0-CELL EXIT THEN
   dup 1 = IF drop P2W1-CELL EXIT THEN
   2 = IF P2W2-CELL EXIT THEN
   P2W3-CELL ;

: EM-P2-QUERY-1 ( n -- ) {: pos:n :}   \ P2W[pos] := frozen certificate width(source-offset,pos)
   pos EM-P2-W-CELL {: wcell:n :}
   9 DATA TKA-CELL LDR,
   10 DATA TXN-SRC-A-CELL LDR,
   9 9 10 SUB,
   10 pos MOVZ,
   LP2CWAT LABEL@ BL,
   10 DATA wcell STR, ;

: EM-P2-QUERY-WIDTHS ( n -- ) {: k:n :}   \ emit: query k widths; x13 = any wider than 1
   LBL {: scal:label :}
   0 BEGIN dup k < WHILE
      dup EM-P2-QUERY-1
      1 +
   REPEAT drop
   9 DATA P2W0-CELL LDR,
   k 1 > IF 10 DATA P2W1-CELL LDR,  9 9 10 ADD, THEN
   k 2 > IF 10 DATA P2W2-CELL LDR,  9 9 10 ADD, THEN
   k 3 > IF 10 DATA P2W3-CELL LDR,  9 9 10 ADD, THEN
   13 0 MOVZ,  9 k CMPI,  C-LE scal BCOND,  13 1 MOVZ,
   scal LBL, ;

variable P2SK
: P2W-ENTRY ( label ptr a n n n -- ) {: lmainlbl:label kwvar:ptr kwlen:n k:n ext:n :}
   LBL P2SK !
   0 kwvar LABEL@ ADR,  1 kwlen MOVZ,  LKWCMP LABEL@ BL,
   0 P2SK LABEL@ CBZ,
   k EM-P2-QUERY-WIDTHS
   13 P2SK LABEL@ CBZ,                                \ all-scalar: normal lowering
   LVSPILL LABEL@ BL,
   ext JIT-XT-EXECUTE
   lmainlbl B,
   P2SK LABEL@ LBL, ;
s" p2w-entry" s" label ptr a n n n --" TRUST

: P2F-ENTRY ( label ptr a n n -- ) {: lmainlbl:label kwvar:ptr kwlen:n ext:n :}
   LBL P2SK !
   0 kwvar LABEL@ ADR,  1 kwlen MOVZ,  LKWCMP LABEL@ BL,
   0 P2SK LABEL@ CBZ,
   1 EM-P2-QUERY-WIDTHS
   LVSPILL LABEL@ BL,
   ext JIT-XT-EXECUTE
   lmainlbl B,
   P2SK LABEL@ LBL, ;
s" p2f-entry" s" label ptr a n n --" TRUST

\ op bodies: read the operand widths (P2W cells, pos 0 = stack top), compose
\ sums into the helper args, BL the emit helper(s).
: EM-P2X-DUP ( -- )                \ ( g0 -- g0 g0 )
   5 DATA P2W0-CELL LDR,  6 5 0 ADDI,  LP2COPY LABEL@ BL, ;
: EM-P2X-DROP ( -- )
   5 DATA P2W0-CELL LDR,  LP2DROPN LABEL@ BL, ;
: EM-P2X-SWAP ( -- )               \ ( g1 g0 -- g0 g1 )
   9 DATA P2W0-CELL LDR,  10 DATA P2W1-CELL LDR,
   5 9 10 ADD,  6 10 0 ADDI,  LP2ROT LABEL@ BL, ;
: EM-P2X-OVER ( -- )               \ ( g1 g0 -- g1 g0 g1 )
   9 DATA P2W0-CELL LDR,  10 DATA P2W1-CELL LDR,
   5 10 0 ADDI,  6 9 10 ADD,  LP2COPY LABEL@ BL, ;
: EM-P2X-NIP ( -- )                \ ( g1 g0 -- g0 )
   EM-P2X-SWAP
   5 DATA P2W1-CELL LDR,  LP2DROPN LABEL@ BL, ;
: EM-P2X-TUCK ( -- )               \ ( g1 g0 -- g0 g1 g0 )
   EM-P2X-SWAP
   9 DATA P2W0-CELL LDR,  10 DATA P2W1-CELL LDR,
   5 9 0 ADDI,  6 9 10 ADD,  LP2COPY LABEL@ BL, ;
: EM-P2X-ROT ( -- )                \ ( g2 g1 g0 -- g1 g0 g2 )
   9 DATA P2W0-CELL LDR,  10 DATA P2W1-CELL LDR,  11 DATA P2W2-CELL LDR,
   5 9 10 ADD,  5 5 11 ADD,  6 11 0 ADDI,  LP2ROT LABEL@ BL, ;
: EM-P2X-MROT ( -- )               \ ( g2 g1 g0 -- g0 g2 g1 )
   9 DATA P2W0-CELL LDR,  10 DATA P2W1-CELL LDR,  11 DATA P2W2-CELL LDR,
   5 9 10 ADD,  5 5 11 ADD,  6 10 11 ADD,  LP2ROT LABEL@ BL, ;
: EM-P2X-2DUP ( -- )               \ ( g1 g0 -- g1 g0 g1 g0 )
   9 DATA P2W0-CELL LDR,  10 DATA P2W1-CELL LDR,
   5 9 10 ADD,  6 5 0 ADDI,  LP2COPY LABEL@ BL, ;
: EM-P2X-2DROP ( -- )
   9 DATA P2W0-CELL LDR,  10 DATA P2W1-CELL LDR,
   5 9 10 ADD,  LP2DROPN LABEL@ BL, ;
: EM-P2X-2SWAP ( -- )              \ ( g3 g2 g1 g0 -- g1 g0 g3 g2 )
   9 DATA P2W0-CELL LDR,  10 DATA P2W1-CELL LDR,  11 DATA P2W2-CELL LDR,  12 DATA P2W3-CELL LDR,
   5 9 10 ADD,  5 5 11 ADD,  5 5 12 ADD,  6 11 12 ADD,  LP2ROT LABEL@ BL, ;
: EM-P2X-2OVER ( -- )              \ ( g3 g2 g1 g0 -- g3 g2 g1 g0 g3 g2 )
   9 DATA P2W0-CELL LDR,  10 DATA P2W1-CELL LDR,  11 DATA P2W2-CELL LDR,  12 DATA P2W3-CELL LDR,
   5 11 12 ADD,  6 9 10 ADD,  6 6 11 ADD,  6 6 12 ADD,  LP2COPY LABEL@ BL, ;
: EM-P2X-TOR ( -- )
   5 DATA P2W0-CELL LDR,  6 0 MOVZ,  LP2RS LABEL@ BL, ;
: EM-P2X-RFROM ( -- )
   5 DATA P2W0-CELL LDR,  6 1 MOVZ,  LP2RS LABEL@ BL, ;
: EM-P2X-RFETCH ( -- )
   5 DATA P2W0-CELL LDR,  6 2 MOVZ,  LP2RS LABEL@ BL, ;
: EM-P2X-2TOR ( -- )
   9 DATA P2W0-CELL LDR,  10 DATA P2W1-CELL LDR,  5 9 10 ADD,  6 0 MOVZ,  LP2RS LABEL@ BL, ;
: EM-P2X-2RFROM ( -- )
   9 DATA P2W0-CELL LDR,  10 DATA P2W1-CELL LDR,  5 9 10 ADD,  6 1 MOVZ,  LP2RS LABEL@ BL, ;
: EM-P2X-2RFET ( -- )
   9 DATA P2W0-CELL LDR,  10 DATA P2W1-CELL LDR,  5 9 10 ADD,  6 2 MOVZ,  LP2RS LABEL@ BL, ;
: EM-P2X-FETCH ( -- )
   LP2VEMIT LABEL@ BL,
   5 DATA P2W0-CELL LDR,  LP2FETCH LABEL@ BL, ;
: EM-P2X-STORE ( -- )
   5 DATA P2W0-CELL LDR,  LP2STORE LABEL@ BL, ;

\ the pass-2 width dispatch: sits between the local-reference dispatch (locals
\ shadow op names, checker parity) and the keyword tiers, so a wide fact at any
\ transport tier is intercepted before its scalar lowering. Facts are recorded
\ only at transport/locals tokens; everything else falls through byte-identical.
: EM-COMPILE-P2WIDE ( -- )
   LBL {: notp2:label :}
   9 DATA P2-CELL LDR,  9 notp2 CBZ,
   LMAIN LABEL@ LKWDUP2    3 1 ['] EM-P2X-DUP     P2W-ENTRY
   LMAIN LABEL@ LKWDROP2   4 1 ['] EM-P2X-DROP    P2W-ENTRY
   LMAIN LABEL@ LKWSWAP2   4 2 ['] EM-P2X-SWAP    P2W-ENTRY
   LMAIN LABEL@ LKWOVER2   4 2 ['] EM-P2X-OVER    P2W-ENTRY
   LMAIN LABEL@ LKWNIP2    3 2 ['] EM-P2X-NIP     P2W-ENTRY
   LMAIN LABEL@ LKWTUCK3   4 2 ['] EM-P2X-TUCK    P2W-ENTRY
   LMAIN LABEL@ LKWROT3    3 3 ['] EM-P2X-ROT     P2W-ENTRY
   LMAIN LABEL@ LKWMROT3   4 3 ['] EM-P2X-MROT    P2W-ENTRY
   LMAIN LABEL@ LKW2DUP3   4 2 ['] EM-P2X-2DUP    P2W-ENTRY
   LMAIN LABEL@ LKW2DROP3  5 2 ['] EM-P2X-2DROP   P2W-ENTRY
   LMAIN LABEL@ LKW2SWAP3  5 4 ['] EM-P2X-2SWAP   P2W-ENTRY
   LMAIN LABEL@ LKW2OVER3  5 4 ['] EM-P2X-2OVER   P2W-ENTRY
   LMAIN LABEL@ LKWTOR     2 1 ['] EM-P2X-TOR     P2W-ENTRY
   LMAIN LABEL@ LKWRFROM   2 1 ['] EM-P2X-RFROM   P2W-ENTRY
   LMAIN LABEL@ LKWRFET    2 1 ['] EM-P2X-RFETCH  P2W-ENTRY
   LMAIN LABEL@ LKW2TOR3   3 2 ['] EM-P2X-2TOR    P2W-ENTRY
   LMAIN LABEL@ LKW2RFROM3 3 2 ['] EM-P2X-2RFROM  P2W-ENTRY
   LMAIN LABEL@ LKW2RFET3  3 2 ['] EM-P2X-2RFET   P2W-ENTRY
   LMAIN LABEL@ LKWAT2     1 ['] EM-P2X-FETCH   P2F-ENTRY
   LMAIN LABEL@ LKWSTORE2  1 1 ['] EM-P2X-STORE   P2W-ENTRY
   notp2 LBL, ;
s" em-compile-p2wide" s" --" TRUST

package LOWER-TXN
private

: EMIT-DESC-VALIDATOR ( -- )
   LBL LBL LBL LBL LBL {: outer:label guards:label next:label done:label bad:label :}
   LOWER-TXN-CODE:VDESC LABEL@ LBL,               \ x11=cert, x12=off cells, x13=len cells, x14=width
   15 12 3 LSLI,  15 11 15 ADD,
   16 13 3 LSLI,  16 15 16 ADD,
   12 15 0 LDR,  12 0 CMPI,  C-LT bad BCOND,
   15 15 1 cells ADDI,
   outer LBL,  12 done CBZ,
      10 15 LOWER-CERT:CHECK-CELLS cells ADDI,  10 15 CMP,  C-CC bad BCOND,
      10 16 CMP,  C-HI bad BCOND,
      17 15 0 LDR,  17 14 CMP,  C-CS bad BCOND,
      17 15 1 cells LDR,  17 0 CMPI,  C-LE bad BCOND,
      13 15 2 cells LDR,  13 0 CMPI,  C-LT bad BCOND,
      15 15 LOWER-CERT:CHECK-CELLS cells ADDI,
   guards LBL,  13 next CBZ,
      10 15 LOWER-CERT:GUARD-CELLS cells ADDI,  10 15 CMP,  C-CC bad BCOND,
      10 16 CMP,  C-HI bad BCOND,
      17 15 0 LDR,  17 14 CMP,  C-CS bad BCOND,
      10 15 1 cells LDR,  10 0 CMPI,  C-LT bad BCOND,
      17 15 2 cells LDR,  17 0 CMPI,  C-LE bad BCOND,
      10 17 CMP,  C-CS bad BCOND,
      15 15 LOWER-CERT:GUARD-CELLS cells ADDI,  13 13 1 SUBI,  guards B,
   next LBL,
      12 12 1 SUBI,  outer B,
   done LBL,
      15 16 CMP,  C-NE bad BCOND,  RET,
   bad LBL,  LOWER-TXN-CODE:BAD 34 LOWER-TXN-CODE:FAIL ;

: EMIT-DRIFT-FAIL ( -- )
   LOWER-TXN-CODE:DRIFT-FAIL LABEL@ LBL,
   LOWER-TXN-CODE:DRIFT 37 LOWER-TXN-CODE:FAIL ;

: PROTECT ( n -- ) {: prot:n :}
   LBL {: ok:label :}
   0 DATA TXN-BLOB-A-CELL LDR,
   1 DATA TXN-BLOB-CAP-CELL LDR,
   2 prot MOVZ,  NR-MPROTECT SYS,
   0 0 CMPI,  C-EQ ok BCOND,
   LOWER-TXN-CODE:MEM 38 LOWER-TXN-CODE:FAIL
   ok LBL, ;

: MAP ( n -- ) {: cap:n :}
   LBL LBL LBL {: clear:label mapped:label ok:label :}
   12 DATA TXN-BLOB-A-CELL LDR,
   13 DATA TXN-BLOB-CAP-CELL LDR,  12 12 13 ORR,
   13 DATA TXN-SRC-A-CELL LDR,  12 12 13 ORR,
   13 DATA TXN-SRC-U-CELL LDR,  12 12 13 ORR,
   13 DATA TXN-CERT-A-CELL LDR,  12 12 13 ORR,
   13 DATA TXN-CERT-U-CELL LDR,  12 12 13 ORR,
   12 clear CBZ,
      LOWER-TXN-CODE:MEM 38 LOWER-TXN-CODE:FAIL
   clear LBL,
   0 0 MOVZ,  1 cap 0 ADDI,  2 3 MOVZ,
   3 MAP-ANON-PRIVATE LIT64,  4 0 MOVN,  5 0 MOVZ,
   NR-MMAP SYS,
   13 C-CS CSET,  13 mapped CBZ,
      LOWER-TXN-CODE:MEM 38 LOWER-TXN-CODE:FAIL
   mapped LBL,
   0 ok CBNZ,
      1 cap 0 ADDI,  NR-MUNMAP SYS,
      LOWER-TXN-CODE:MEM 38 LOWER-TXN-CODE:FAIL
   ok LBL,
   0 DATA TXN-BLOB-A-CELL STR,
   cap DATA TXN-BLOB-CAP-CELL STR, ;

: RELEASE-MAP ( -- )
   LBL LBL LBL {: empty:label mapped:label ok:label :}
   0 DATA TXN-BLOB-A-CELL LDR,
   1 DATA TXN-BLOB-CAP-CELL LDR,
   0 mapped CBNZ,
      1 empty CBZ,
      LOWER-TXN-CODE:MEM 38 LOWER-TXN-CODE:FAIL
   mapped LBL,
   1 ok CBNZ,
      LOWER-TXN-CODE:MEM 38 LOWER-TXN-CODE:FAIL
   ok LBL,
   NR-MUNMAP SYS,
   0 0 CMPI,  C-EQ empty BCOND,
      LOWER-TXN-CODE:MEM 38 LOWER-TXN-CODE:FAIL
   empty LBL,
   0 0 MOVZ,
   0 DATA TXN-BLOB-A-CELL STR,
   0 DATA TXN-BLOB-CAP-CELL STR,
   0 DATA TXN-SRC-A-CELL STR,
   0 DATA TXN-SRC-U-CELL STR,
   0 DATA TXN-CERT-A-CELL STR,
   0 DATA TXN-CERT-U-CELL STR, ;

: COPY-BYTES ( n n n -- ) {: src:n len:n dst:n :}
   LBL LBL {: loop:label done:label :}
   loop LBL,  len done CBZ,
      17 src 0 LDRB,  17 dst 0 STRB,
      src src 1 ADDI,  dst dst 1 ADDI,  len len 1 SUBI,
      loop B,
   done LBL, ;

: ROW-END ( n n n n label -- ) {: count:n stride:n cur:n total:n bad:label :}
   16 stride MOVZ,
   15 count 16 MUL,
   17 15 16 UDIV,  17 count CMP,  C-NE bad BCOND,
   16 cur 0 ADDI,  cur cur 15 ADD,
   cur 16 CMP,  C-CC bad BCOND,
   cur total CMP,  C-HI bad BCOND, ;

: SOURCE-LEN ( n -- ) {: dst:n :}
   LBL LBL {: full:label done:label :}
   dst DATA DOESB-CELL LDR,
   dst full CBZ,
      dst dst 6 SUBI,  done B,
   full LBL,
      dst DATA BODYLEN-CELL LDR,
   done LBL, ;

: HASH-BODY ( -- )
   LBL LBL {: loop:label done:label :}
   12 DATA BODYBUF-OFF ADDI,
   13 SOURCE-LEN
   14 LOWER-CERT:FNV-OFFSET LIT64,
   15 LOWER-CERT:FNV-PRIME LIT64,
   loop LBL,  13 done CBZ,
      17 12 0 LDRB,
      14 14 17 EOR,
      14 14 15 MUL,
      12 12 1 ADDI,
      13 13 1 SUBI,
      loop B,
   done LBL, ;

: VALIDATE-WF ( label -- ) {: bad:label :}
   LBL LBL LBL LBL LBL LBL {: loop:label ordered:label flags:label fetch:label advance:label done:label :}
   5 11 LOWER-CERT:WF-COUNT-CELL cells LDR,
   13 11 LOWER-CERT:HEADER-CELLS cells ADDI,
   14 0 MOVZ,  9 0 MOVZ,  8 0 MOVZ,  6 0 MOVZ,  7 0 MOVZ,
   loop LBL,  14 5 CMP,  C-GE done BCOND,
      15 13 0 LDR,  17 SOURCE-LEN  15 17 CMP,  C-CS bad BCOND,
      17 13 1 cells LDR,  17 TXN-LIVE-W-CAP CMPI,  C-CS bad BCOND,
      14 ordered CBZ,
      15 6 CMP,  C-CC bad BCOND,  C-NE ordered BCOND,
      17 7 CMP,  C-LS bad BCOND,
   ordered LBL,
      6 15 0 ADDI,  7 17 0 ADDI,
      15 13 2 cells LDR,  15 bad CBZ,
      15 LOWER-TXN-CODE:MAX-WIDTH CMPI,  C-HI bad BCOND,
      15 1 CMPI,  C-LS flags BCOND,
      9 1 MOVZ,
   flags LBL,
      17 13 3 cells LDR,
      15 17 3 ANDI,  15 17 CMP,  C-NE bad BCOND,
      17 3 CMPI,  C-EQ bad BCOND,
      15 17 LOWER-CERT:FETCH-FLAG ANDI,  15 fetch CBNZ,
      advance B,
   fetch LBL,
      8 8 1 ADDI,
   advance LBL,
      13 13 LOWER-CERT:WF-CELLS cells ADDI,
      14 14 1 ADDI,  loop B,
   done LBL,
   15 11 LOWER-CERT:FETCH-COUNT-CELL cells LDR,  15 8 CMP,  C-NE bad BCOND, ;

: VALIDATE-BINDS ( label -- ) {: bad:label :}
   LBL LBL {: loop:label done:label :}
   5 11 LOWER-CERT:WF-COUNT-CELL cells LDR,
   6 11 LOWER-CERT:BIND-COUNT-CELL cells LDR,
   13 11 LOWER-CERT:HEADER-CELLS cells ADDI,
   12 LOWER-CERT:WF-CELLS cells MOVZ,
   15 5 12 MUL,  13 13 15 ADD,
   14 0 MOVZ,
   loop LBL,  14 6 CMP,  C-GE done BCOND,
      15 13 0 LDR,  15 bad CBZ,
      15 LOWER-TXN-CODE:MAX-WIDTH CMPI,  C-HI bad BCOND,
      13 13 1 cells ADDI,  14 14 1 ADDI,  loop B,
   done LBL, ;

: VALIDATE-FETCHES ( label -- ) {: bad:label :}
   LBL LBL LBL LBL LBL {: loop:label scan:label linked:label scalar:label done:label :}
   5 11 LOWER-CERT:WF-COUNT-CELL cells LDR,
   6 11 LOWER-CERT:BIND-COUNT-CELL cells LDR,
   7 11 LOWER-CERT:FETCH-COUNT-CELL cells LDR,
   8 11 LOWER-CERT:FETCH-DATA-CELLS-CELL cells LDR,
   0 11 LOWER-CERT:HEADER-CELLS cells ADDI,          \ WF cursor
   1 5 0 ADDI,                                      \ WF rows remaining
   12 LOWER-CERT:WF-CELLS cells MOVZ,
   13 5 12 MUL,  2 0 13 ADD,
   13 6 3 LSLI,  2 2 13 ADD,                         \ fetch-row cursor
   3 LOWER-CERT:HEADER-CELLS MOVZ,
   12 LOWER-CERT:WF-CELLS MOVZ,
   13 5 12 MUL,  3 3 13 ADD,  3 3 6 ADD,
   12 LOWER-CERT:FETCH-CELLS MOVZ,
   13 7 12 MUL,  3 3 13 ADD,                         \ next descriptor cell
   4 10 3 LSRI,                                      \ total certificate cells
   5 7 0 ADDI,
   loop LBL,  5 done CBZ,
   scan LBL,  1 bad CBZ,
      12 0 3 cells LDR,
      13 12 LOWER-CERT:FETCH-FLAG ANDI,
      13 linked CBNZ,
      0 0 LOWER-CERT:WF-CELLS cells ADDI,
      1 1 1 SUBI,  scan B,
   linked LBL,
      12 0 0 LDR,  13 2 0 LDR,  12 13 CMP,  C-NE bad BCOND,
      12 0 1 cells LDR,  12 bad CBNZ,
      14 0 2 cells LDR,
      0 0 LOWER-CERT:WF-CELLS cells ADDI,
      1 1 1 SUBI,
      12 2 1 cells LDR,  12 3 CMP,  C-NE bad BCOND,
      13 2 2 cells LDR,  13 bad CBZ,
      8 3 13 ADD,  8 3 CMP,  C-CC bad BCOND,
      8 4 CMP,  C-HI bad BCOND,
      15 12 3 LSLI,  15 11 15 ADD,
      15 15 0 LDR,  15 scalar CBZ,
      9 1 MOVZ,
   scalar LBL,
      LOWER-TXN-CODE:VDESC LABEL@ BL,
      3 8 0 ADDI,
      2 2 LOWER-CERT:FETCH-CELLS cells ADDI,
      5 5 1 SUBI,  loop B,
   done LBL,
   3 4 CMP,  C-NE bad BCOND,
   12 11 LOWER-CERT:NEEDS-CELL cells LDR,  12 9 CMP,  C-NE bad BCOND, ;

: VALIDATE ( -- )
   LBL LBL {: bad:label done:label :}
   10 LOWER-CERT:HEADER-CELLS cells CMPI,  C-CC bad BCOND,
   13 11 LOWER-CERT:MAGIC-CELL cells LDR,
   14 LOWER-CERT:MAGIC LIT64,  13 14 CMP,  C-NE bad BCOND,
   13 11 LOWER-CERT:VERSION-CELL cells LDR,
   13 LOWER-CERT:VERSION CMPI,  C-NE bad BCOND,
   13 11 LOWER-CERT:TOTAL-BYTES-CELL cells LDR,
   13 10 CMP,  C-NE bad BCOND,
   13 10 7 ANDI,  13 bad CBNZ,
   13 11 LOWER-CERT:BODY-LEN-CELL cells LDR,
   14 SOURCE-LEN
   13 14 CMP,  C-NE bad BCOND,
   HASH-BODY
   13 11 LOWER-CERT:BODY-HASH-CELL cells LDR,
   13 14 CMP,  C-NE bad BCOND,
   5 11 LOWER-CERT:WF-COUNT-CELL cells LDR,
   6 11 LOWER-CERT:BIND-COUNT-CELL cells LDR,
   7 11 LOWER-CERT:FETCH-COUNT-CELL cells LDR,
   8 11 LOWER-CERT:FETCH-DATA-CELLS-CELL cells LDR,
   13 LOWER-CERT:HEADER-CELLS cells MOVZ,
   5 LOWER-CERT:WF-CELLS cells 13 10 bad ROW-END
   6 1 cells 13 10 bad ROW-END
   7 LOWER-CERT:FETCH-CELLS cells 13 10 bad ROW-END
   8 1 cells 13 10 bad ROW-END
   13 10 CMP,  C-NE bad BCOND,
   bad VALIDATE-WF
   bad VALIDATE-BINDS
   bad VALIDATE-FETCHES
   done B,
   bad LBL,  LOWER-TXN-CODE:BAD 34 LOWER-TXN-CODE:FAIL
   done LBL, ;

: ZERO-LIVE ( -- )
   LBL LBL {: loop:label done:label :}
   11 TXN-LIVE-W-OFF MOVZ,  11 DATA 11 ADD,
   12 64 MOVZ,  13 0 MOVZ,
   loop LBL,  12 done CBZ,
      13 11 0 STR,  11 11 1 cells ADDI,  12 12 1 SUBI,  loop B,
   done LBL, ;

: VERIFY-COUNT ( n n -- ) {: state:n header:n :}
   LBL {: ok:label :}
   11 DATA TXN-CERT-A-CELL LDR,
   12 11 header cells LDR,
   13 DATA state LDR,
   12 13 CMP,  C-EQ ok BCOND,
      LOWER-TXN-CODE:DRIFT 37 LOWER-TXN-CODE:FAIL
   ok LBL, ;

public

: EMIT-VALIDATOR ( -- )
   EMIT-DESC-VALIDATOR
   EMIT-DRIFT-FAIL ;
: SEAL ( -- ) 1 PROTECT ;
: RELEASE ( -- ) RELEASE-MAP ;

: VERIFY-DONE ( -- )
   TXN-WF-I-CELL LOWER-CERT:WF-COUNT-CELL VERIFY-COUNT
   TXN-BIND-I-CELL LOWER-CERT:BIND-COUNT-CELL VERIFY-COUNT
   TXN-FETCH-I-CELL LOWER-CERT:FETCH-COUNT-CELL VERIFY-COUNT ;

: FREEZE ( -- )
   LBL LBL LBL LBL {: skip:label copy:label room:label alloc:label :}
   LCERTBYTES 16 C-FIND-GLOBAL
   C-CALL-X11-SAVED
   10 G-POP  11 G-POP                              \ x11=certificate, x10=bytes
   VALIDATE
   10 11 LOWER-CERT:TOTAL-BYTES-CELL cells LDR,      \ validator helpers use x10 as scratch
   13 11 LOWER-CERT:NEEDS-CELL cells LDR,  13 copy CBNZ,
      13 0 MOVZ,  13 DATA TXN-ACTIVE-CELL STR,  skip B,
   copy LBL,
   14 SOURCE-LEN
   15 14 9 ADDI,  15 15 $FFFFFFFFFFFFFFF8 ANDI,     \ aligned source bytes incl. `; `
   16 15 10 ADD,  16 15 CMP,  C-CS room BCOND,
      LOWER-TXN-CODE:FULL 29 LOWER-TXN-CODE:FAIL
   room LBL,
   17 PROT-PAGE-MAX 1 - LIT64,  17 16 17 ADD,
   17 16 CMP,  C-CS alloc BCOND,
      LOWER-TXN-CODE:FULL 29 LOWER-TXN-CODE:FAIL
   alloc LBL,
   5 PROT-PAGE-MAX negate LIT64,  17 17 5 AND,
   SP SP 32 SUBI,
   11 SP 0 STR,  10 SP 8 STR,  14 SP 16 STR,
   17 MAP
   11 DATA BODYBUF-OFF ADDI,  12 14 0 ADDI,
   13 DATA TXN-BLOB-A-CELL LDR,
   13 DATA TXN-SRC-A-CELL STR,
   11 12 13 COPY-BYTES
   17 $3B MOVZ,  17 13 0 STRB,  13 13 1 ADDI,
   17 32 MOVZ,  17 13 0 STRB,  13 13 1 ADDI,
   13 13 7 ADDI,  13 13 $FFFFFFFFFFFFFFF8 ANDI,
   13 DATA TXN-CERT-A-CELL STR,
   11 SP 0 LDR,  12 SP 8 LDR,
   12 DATA TXN-CERT-U-CELL STR,
   11 12 13 COPY-BYTES
   14 SP 16 LDR,  14 14 2 ADDI,  14 DATA TXN-SRC-U-CELL STR,
   14 0 MOVZ,
   14 DATA TXN-BIND-I-CELL STR,
   14 DATA TXN-WF-I-CELL STR,
   14 DATA TXN-FETCH-I-CELL STR,
   ZERO-LIVE
   14 1 MOVZ,  14 DATA TXN-ACTIVE-CELL STR,
   SEAL
   SP SP 32 ADDI,
   skip LBL, ;

: NEEDS? ( -- )
   10 DATA TXN-ACTIVE-CELL LDR, ;

end-package

\ pass-2 entry: the hook produced a sealed lowering transaction. Save the live
\ input, repoint the tokenizer at its frozen source span, rewind CP to
\ the colon entry (the name bytes stay) and DP to the definition watermark,
\ reset the per-definition compile state exactly as EM-INTERPRET-COLON does,
\ re-emit the prologue, and re-run the compile loop width-aware.
: EM-P2-START ( -- )
   2 3 MOVZ,  LPROT LABEL@ BL,
   LOWER-TXN:SEAL
   9 DATA INP-CELL LDR,  9 DATA P2INP-CELL STR,
   9 DATA INE-CELL LDR,  9 DATA P2INE-CELL STR,
   10 DATA TXN-SRC-A-CELL LDR,
   9 DATA P2BODY0-CELL LDR,  9 10 9 ADD,  9 DATA INP-CELL STR,
   9 DATA TXN-SRC-U-CELL LDR,  9 10 9 ADD,  9 DATA INE-CELL STR,
   11 DATA PEND-CELL LDR,  CP 11 0 LDR,
   9 DATA P2DP-CELL LDR,  9 DATA DP-CELL STR,
   5 CFSTK-OFF LIT64,  11 DBASE 5 ADD,  12 0 MOVZ,  12 11 0 STR,
   12 DATA LOCN-CELL STR,  12 DATA LOCF-CELL STR,
   12 DATA VSP-CELL STR,  12 DATA SNAPSP-CELL STR,
   12 DATA EXITH-CELL STR,  12 DATA LVD-CELL STR,
   12 DATA QPATCH-CELL STR,
   12 VRALL MOVZ,  12 DATA VRFREE-CELL STR,
   12 FRALL MOVZ,  12 DATA FRFREE-CELL STR,
   9 $D10043FF LIT64,  LCEMIT LABEL@ BL,
   9 $F90003FE LIT64,  LCEMIT LABEL@ BL,
   9 1 MOVZ,  9 DATA P2-CELL STR,
   9 0 MOVZ,
   9 DATA TXN-BIND-I-CELL STR, ;
s" em-p2-start" s" --" TRUST

\ EM-P2-TRIGGER: emitted right after the publish path freezes a certified
\ definition. An active transaction enters the pass-2 re-run; wide facts inside
\ a does> split body fail
\ closed: the two-phase body check indexes tokens differently, so a width-aware
\ re-run cannot align). Pass 2 (the re-run's own ';') falls through to the
\ normal publish.
: EM-P2-TRIGGER ( -- )
   LBL LBL {: nowide:label p2ok:label :}
   9 DATA P2-CELL LDR,  9 nowide CBNZ,
   LOWER-TXN:NEEDS?
   10 nowide CBZ,
   10 DATA DOESB-CELL LDR,  10 p2ok CBZ,
      0 2 MOVZ,  1 LP2DOESW LABEL@ ADR,  2 49 MOVZ,  NR-WRITE SYS,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 $4B MOVZ,  NR-EXIT-GROUP SYS,
   p2ok LBL,
   EM-P2-START
   LMAIN LABEL@ B,
   nowide LBL, ;
s" em-p2-trigger" s" --" TRUST

\ EM-P2-CHECK-DEFINER: the sig'd-definition publish gate. Pass 1 runs the hook
\ (which registers the certified signature) and then the pass-2 trigger. The
\ pass-2 second ';' must NOT re-run the hook — a second CHECK! of the same name
\ hits the checker's certified-duplicate guard (CHECKER-DUP-DEFINITION, throw
\ $4E) — so it skips straight to the normal TRUST-PEND publish tail, giving the
\ exact pass-1 registration sequence (one certify add + one trust row).
: EM-P2-CHECK-DEFINER ( -- )
   LBL {: p2sk:label :}
   9 DATA P2-CELL LDR,  9 p2sk CBNZ,
   9 DATA HOOK-CELL LDR,  9 p2sk CBZ,
      C-CALL-CHECK-DEFINER
      LOWER-TXN:FREEZE
      EM-P2-TRIGGER
   p2sk LBL, ;
s" em-p2-check-definer" s" --" TRUST

\ EM-P2-FINISH: emitted on the publish tail — the pass-2 second ';' published
\ through the ordinary trusted tail (hook re-check skipped), so resume the
\ saved real input and clear the pass-2 state (dead pointers zeroed for image
\ determinism).
: EM-P2-FINISH ( -- )
   LBL {: nop2:label :}
   9 DATA P2-CELL LDR,  9 nop2 CBZ,
   LOWER-TXN:VERIFY-DONE
   9 DATA P2INP-CELL LDR,  9 DATA INP-CELL STR,
   9 DATA P2INE-CELL LDR,  9 DATA INE-CELL STR,
   LOWER-TXN:RELEASE
   9 0 MOVZ,  9 DATA P2-CELL STR,
   9 DATA P2INP-CELL STR,  9 DATA P2INE-CELL STR,
   9 DATA TXN-ACTIVE-CELL STR,
   9 DATA TXN-WF-I-CELL STR,  9 DATA TXN-BIND-I-CELL STR,
   9 DATA TXN-FETCH-I-CELL STR,
   nop2 LBL, ;
s" em-p2-finish" s" --" TRUST

: EM-COMPILE-FLUSH-PEND ( -- )
   11 DATA PEND-CELL LDR,
   9 11 0 LDR,  10 CP 9 SUB,  10 10 4 SUBI,  10 11 8 STR,
   2 5 MOVZ,  LPROT LABEL@ BL,  LFLUSH LABEL@ BL, ;
s" em-compile-flush-pend" s" --" TRUST

: EM-COMPILE-PUBLISH-TRUSTED ( -- )
   LBL LBL LBL {: ttrusted ndhas ndchk :}
   10 DATA TRUSTED-CELL LDR,  10 ttrusted CBNZ,
      \ hook-certified sig'd definition (TSIG holds the captured signature, so
      \ every checked `: NAME ( .. )` publishes HERE): a wider-than-cell width
      \ fact triggers the pass-2 width-aware re-run (item 12 slice 3b); the
      \ pass-2 second ';' skips the re-check and publishes below.
      EM-P2-CHECK-DEFINER
   ttrusted LBL,
   10 DATA TCSIG-U-CELL LDR,  10 ndhas CBNZ,
   10 DATA DOESB-CELL LDR,  10 ndchk CBZ,
      C-DIE-DOES
   ndhas LBL,
   10 DATA DOESB-CELL LDR,  10 ndchk CBZ,
      C-CALL-CHECK-DOES
   ndchk LBL,
   C-CALL-TRUST-PEND
   NDICT NDICT 1 ADDI,  LHIDXADD LABEL@ BL,
   EM-REC-WIDE-PUBLISH
   EM-P2-FINISH
   C-CLEAR-TRUSTED-STATE
   9 0 MOVZ,  9 DATA PEND-CELL STR,
   LMAIN LABEL@ B, ;
s" em-compile-publish-trusted" s" --" TRUST

: EM-COMPILE-PUBLISH-HOOKED ( -- )
   LBL LBL LBL LBL {: nohook:label rejected:label inl:label done:label :}
   9 DATA P2-CELL LDR,  9 nohook CBNZ,                \ pass-2 second ';': no hook re-check
   9 DATA HOOK-CELL LDR,  9 nohook CBZ,
      10 DATA BODYBUF-OFF ADDI,  10 G-PUSH
      10 DATA BODYLEN-CELL LDR,  10 G-PUSH
      SP SP 16 SUBI,  30 SP 0 STR,  9 BLR,  30 SP 0 LDR,  SP SP 16 ADDI,
      10 G-POP  10 rejected CBZ,
      \ certified sig-less definition: same pass-2 dispatch as the sig'd
      \ publish path (item 12 slice 3b).
      LOWER-TXN:FREEZE
      EM-P2-TRIGGER
   nohook LBL,  NDICT NDICT 1 ADDI,  LHIDXADD LABEL@ BL,
   EM-REC-WIDE-PUBLISH
   done B,
   rejected LBL,  11 DATA PEND-CELL LDR,  12 11 16 LDR,  12 12 DNAME-EXT ANDI,  12 inl CBZ,
      CP 11 24 LDR,  done B,                           \ ext name in code space: CP := pre-name CP
   inl LBL,  CP 11 0 LDR,                              \ inline name: CP := colon entry
   done LBL,
   EM-P2-FINISH
   C-CLEAR-TRUSTED-STATE
   9 0 MOVZ,  9 DATA PEND-CELL STR,  LMAIN LABEL@ B, ;
s" em-compile-publish-hooked" s" --" TRUST

: EM-COMPILE-PUBLISH ( -- )
   LBL LBL {: checked unsigned :}
   9 DATA HOOK-CELL LDR,  9 checked CBNZ,
      EM-COMPILE-PUBLISH-HOOKED
   checked LBL,
   9 DATA TSIG-U-CELL LDR,  9 unsigned CBZ,
      EM-COMPILE-PUBLISH-TRUSTED
   unsigned LBL,
   EM-COMPILE-PUBLISH-HOOKED ;
s" em-compile-publish" s" --" TRUST

: EM-COMPILE-SEMI ( label -- ) {: lnotsemi:label :}
   9 DATA TKL-CELL LDR,  9 1 CMPI,  C-NE lnotsemi BCOND,
   9 DATA TKA-CELL LDR,  9 9 0 LDRB,  9 59 CMPI,  C-NE lnotsemi BCOND,
      LVSPILL LABEL@ BL,
      EM-COMPILE-DROP-LOCALS
      14 CP 0 ADDI,  9 DATA EXITH-CELL LDR,  LBCHAIN LABEL@ BL,
      EM-COMPILE-RET
      EM-COMPILE-FLUSH-PEND
      EM-COMPILE-PUBLISH
   lnotsemi LBL, ;
s" em-compile-semi" s" label --" TRUST

: EM-COMPILE-CONTROL-KEYWORDS ( -- )
   s" if" KEEP? IF LMAIN LABEL@ LKWIF     2 ['] J-IF   ['] J-IFR    CFB-ENTRY THEN
   s" then" KEEP? IF LMAIN LABEL@ LKWTHEN   4 ['] J-THEN   CF-ENTRY THEN
   s" else" KEEP? IF LMAIN LABEL@ LKWELSE   4 ['] J-ELSE   CF-ENTRY THEN
   s" begin" KEEP? IF LMAIN LABEL@ LKWBEGIN  5 ['] J-BEGIN  CFN-ENTRY THEN
   s" until" KEEP? IF LMAIN LABEL@ LKWUNTIL  5 ['] J-UNTIL ['] J-UNTILR CFBN-ENTRY THEN
   s" again" KEEP? IF LMAIN LABEL@ LKWAGAIN  5 ['] J-AGAIN  CFN-ENTRY THEN
   s" while" KEEP? IF LMAIN LABEL@ LKWWHILE  5 ['] J-WHILE ['] J-WHILER CFB-ENTRY THEN
   s" repeat" KEEP? IF LMAIN LABEL@ LKWREPEAT 6 ['] J-REPEAT CFN-ENTRY THEN
   s" case" KEEP? IF LMAIN LABEL@ LKWCASE 4 ['] J-CASE CFN-ENTRY THEN
   s" of" KEEP? IF LMAIN LABEL@ LKWOF 2 ['] J-OF CF-ENTRY THEN
   s" endof" KEEP? IF LMAIN LABEL@ LKWENDOF 5 ['] J-ENDOF CF-ENTRY THEN
   s" endcase" KEEP? IF LMAIN LABEL@ LKWENDCASE 7 ['] J-ENDCASE CF-ENTRY THEN
   s" construct" KEEP? IF LMAIN LABEL@ LKWCONSTRUCT 9 ['] J-CONSTRUCT CFN-ENTRY THEN
   s" match" KEEP? IF LMAIN LABEL@ LKWMATCH 5 ['] J-MATCH CF-ENTRY THEN ;
s" em-compile-control-keywords" s" --" TRUST

: EM-COMPILE-STRING-KEYWORDS ( -- )
   LMAIN LABEL@ LKWSQ     2 ['] C-SDQ    CF-ENTRY
   LMAIN LABEL@ LKWCQ     2 ['] C-CQ     CF-ENTRY
   LMAIN LABEL@ LKWDOTQ   2 ['] C-DOTQ   CF-ENTRY
   LMAIN LABEL@ LKWESQ    3 ['] C-ESDQ   CF-ENTRY
   LMAIN LABEL@ LKWECQ    3 ['] C-ECQ    CF-ENTRY
   LMAIN LABEL@ LKWEDOTQ  3 ['] C-EDOTQ  CF-ENTRY ;
s" em-compile-string-keywords" s" --" TRUST

: EM-COMPILE-META-KEYWORDS ( -- )
   s" [']" KEEP? IF LMAIN LABEL@ LKWBTICK  3 ['] C-BTICK  CF-ENTRY THEN
   s" [char]" KEEP? IF LMAIN LABEL@ LKWBCHAR  6 ['] C-BCHAR  CF-ENTRY THEN
   s" postpone" KEEP? IF LMAIN LABEL@ LKWPOST   8 ['] C-POSTPONE CF-ENTRY THEN
   s" does>" KEEP? IF LMAIN LABEL@ LKWDOES   5 ['] J-DOES     CF-ENTRY THEN
   s" [:" KEEP? IF LMAIN LABEL@ LKWQUOT   2 ['] J-QUOT     CF-ENTRY THEN
   s" is" KEEP? IF LMAIN LABEL@ LKWIS 2 ['] J-IS CF-ENTRY THEN
   s" ;]" KEEP? IF LMAIN LABEL@ LKWSEMIQ  2 ['] J-SEMIQUOT CF-ENTRY THEN ;
s" em-compile-meta-keywords" s" --" TRUST

: EM-COMPILE-LOOP-KEYWORDS ( -- )
   s" do" KEEP? IF LMAIN LABEL@ LKWDO     2 ['] J-DO     CF-ENTRY THEN
   s" loop" KEEP? IF LMAIN LABEL@ LKWLOOP   4 ['] J-LOOP   CF-ENTRY THEN
   s" i" KEEP? IF LMAIN LABEL@ LKWI      1 ['] J-I      CF-ENTRY THEN
   s" >r" KEEP? IF LMAIN LABEL@ LKWTOR    2 ['] J-TOR    CF-ENTRY THEN
   s" r>" KEEP? IF LMAIN LABEL@ LKWRFROM  2 ['] J-RFROM  CF-ENTRY THEN
   s" r@" KEEP? IF LMAIN LABEL@ LKWRFET   2 ['] J-RFETCH CF-ENTRY THEN
   s" exit" KEEP? IF LMAIN LABEL@ LKWEXIT   4 ['] J-EXIT    CF-ENTRY THEN
   s" recurse" KEEP? IF LMAIN LABEL@ LKWREC    7 ['] J-RECURSE CF-ENTRY THEN
   s" ?do" KEEP? IF LMAIN LABEL@ LKWQDO    3 ['] J-?DO     CF-ENTRY THEN
   s" +loop" KEEP? IF LMAIN LABEL@ LKWPLOOP  5 ['] J-+LOOP   CF-ENTRY THEN
   s" j" KEEP? IF LMAIN LABEL@ LKWJ      1 ['] J-J       CF-ENTRY THEN
   s" leave" KEEP? IF LMAIN LABEL@ LKWLEAVE  5 ['] J-LEAVE   CF-ENTRY THEN
   s" unloop" KEEP? IF LMAIN LABEL@ LKWUNLOOP 6 ['] J-UNLOOP  CF-ENTRY THEN
   s" {:" KEEP? IF LMAIN LABEL@ LKWLBRACE 2 ['] C-LBRACE CF-ENTRY THEN ;
s" em-compile-loop-keywords" s" --" TRUST

: EM-COMPILE-KEYWORDS ( -- )
   LBCAP LABEL@ BL,
   EM-COMPILE-CONTROL-KEYWORDS
   EM-COMPILE-STRING-KEYWORDS
   EM-COMPILE-META-KEYWORDS
   EM-COMPILE-LOOP-KEYWORDS ;
s" em-compile-keywords" s" --" TRUST

: EM-COMPILE-LOCAL ( -- )
   LBL {: notloc :}
   LMAIN LABEL@ notloc C-LOCAL-REF
   notloc LBL, ;
s" em-compile-local" s" --" TRUST

: EM-COMPILE-LITERAL ( -- )
   LBL LBL {: lcnotnum lcflt :}
   9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LNUM LABEL@ BL,
   12 lcnotnum CBZ,
   2 lcflt CBNZ,
      LVPUSHC LABEL@ BL,  LMAIN LABEL@ B,
   lcflt LBL,
      LVPUSHF LABEL@ BL,  LMAIN LABEL@ B,
   lcnotnum LBL, ;
s" em-compile-literal" s" --" TRUST

: EM-COMPILE-ARITH-OPS ( -- )
   s" +" KEEP? IF LMAIN LABEL@ LKWPLUS  1 ['] VF+ ['] E+ ['] EI+ VOPI-ENTRY THEN
   s" -" KEEP? IF LMAIN LABEL@ LKWMINUS 1 ['] VF- ['] E- ['] EI- VOPI-ENTRY THEN
   s" *" KEEP? IF LMAIN LABEL@ LKWSTAR  1 ['] VF* ['] E* VOP-ENTRY THEN
   s" and" KEEP? IF LMAIN LABEL@ LKWAND2  3 ['] FAND ['] EAND VOP-ENTRY THEN
   s" or" KEEP? IF LMAIN LABEL@ LKWOR2   2 ['] FOR2 ['] EOR2 VOP-ENTRY THEN
   s" xor" KEEP? IF LMAIN LABEL@ LKWXOR2  3 ['] FXOR2 ['] EXOR VOP-ENTRY THEN ;
s" em-compile-arith-ops" s" --" TRUST

: EM-COMPILE-SHUFFLE-OPS ( -- )
   s" dup" KEEP? IF LMAIN LABEL@ LKWDUP2  3 1 ['] XDUP  VSHUF-ENTRY THEN
   s" drop" KEEP? IF LMAIN LABEL@ LKWDROP2 4 1 ['] XDROP VSHUF-ENTRY THEN
   s" swap" KEEP? IF LMAIN LABEL@ LKWSWAP2 4 2 ['] XSWAP VSHUF-ENTRY THEN
   s" over" KEEP? IF LMAIN LABEL@ LKWOVER2 4 2 ['] XOVER VSHUF-ENTRY THEN
   s" nip" KEEP? IF LMAIN LABEL@ LKWNIP2  3 2 ['] XNIP  VSHUF-ENTRY THEN ;
s" em-compile-shuffle-ops" s" --" TRUST

: EM-COMPILE-COMPARE-OPS ( -- )
   s" =" KEEP? IF LMAIN LABEL@ LKWEQ2 1 0 VCMP-ENTRY THEN
   s" <>" KEEP? IF LMAIN LABEL@ LKWNE2 2 1 VCMP-ENTRY THEN
   s" <" KEEP? IF LMAIN LABEL@ LKWLT2 1 11 VCMP-ENTRY THEN
   s" >" KEEP? IF LMAIN LABEL@ LKWGT2 1 12 VCMP-ENTRY THEN
   s" <=" KEEP? IF LMAIN LABEL@ LKWLE2 2 13 VCMP-ENTRY THEN
   s" >=" KEEP? IF LMAIN LABEL@ LKWGE2 2 10 VCMP-ENTRY THEN ;
s" em-compile-compare-ops" s" --" TRUST

: EM-COMPILE-UNARY-OPS ( -- )
   s" 1+" KEEP? IF LMAIN LABEL@ LKWINC  2 ['] FU1+ ['] EU1+ VUN-ENTRY THEN
   s" 1-" KEEP? IF LMAIN LABEL@ LKWDEC  2 ['] FU1- ['] EU1- VUN-ENTRY THEN
   s" 0=" KEEP? IF LMAIN LABEL@ LKWZEQ  2 ['] FU0= ['] EU0= VUN-ENTRY THEN
   s" 0<" KEEP? IF LMAIN LABEL@ LKWZLT  2 ['] FU0< ['] EU0< VUN-ENTRY THEN
   s" negate" KEEP? IF LMAIN LABEL@ LKWNEG2 6 ['] FUNEG ['] EUNEG VUN-ENTRY THEN
   s" invert" KEEP? IF LMAIN LABEL@ LKWINV2 6 ['] FUINV ['] EUINV VUN-ENTRY THEN ;
s" em-compile-unary-ops" s" --" TRUST

: EM-COMPILE-FLOAT-OPS ( -- )
   s" f+" KEEP? IF LMAIN LABEL@ LKWFPLUS  2 $1E602800 FOP-ENTRY THEN
   s" f-" KEEP? IF LMAIN LABEL@ LKWFMINUS 2 $1E603800 FOP-ENTRY THEN
   s" f*" KEEP? IF LMAIN LABEL@ LKWFSTAR  2 $1E600800 FOP-ENTRY THEN
   s" f/" KEEP? IF LMAIN LABEL@ LKWFSLASH 2 $1E601800 FOP-ENTRY THEN ;
s" em-compile-float-ops" s" --" TRUST

: EM-COMPILE-OPS ( -- )
   EM-COMPILE-ARITH-OPS
   EM-COMPILE-SHUFFLE-OPS
   EM-COMPILE-COMPARE-OPS
   EM-COMPILE-UNARY-OPS
   EM-COMPILE-FLOAT-OPS ;
s" em-compile-ops" s" --" TRUST

: EM-COMPILE-CALL ( -- )
   LBL {: notimm :}
   LVSPILL LABEL@ BL,
   9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFIND LABEL@ BL,
   13 LUNDEF LABEL@ CBZ,
   14 13 2 ANDI,  14 notimm CBZ,
      SP SP 16 SUBI,  30 SP 0 STR,  11 SP 8 STR,
      2 5 MOVZ,  LPROT LABEL@ BL,
      11 SP 8 LDR,  11 BLR,
      2 3 MOVZ,  LPROT LABEL@ BL,
      30 SP 0 LDR,  SP SP 16 ADDI,
      LMAIN LABEL@ B,
   notimm LBL,
   C-CALL  LMAIN LABEL@ B, ;
s" em-compile-call" s" --" TRUST

: EM-RESET-COMPILE-STATE ( -- )
   9 0 MOVZ,
   9 DATA RSP-CELL STR,  9 DATA HND-CELL STR,  9 DATA LOOPSP-CELL STR,
   9 DATA LVD-CELL STR,  9 DATA VSP-CELL STR,  9 DATA QPATCH-CELL STR,
   9 DATA LOCN-CELL STR,  9 DATA BODYLEN-CELL STR,  9 DATA EXITH-CELL STR,
   9 DATA PEND-CELL STR,  9 DATA CMM-CELL STR,
   9 DATA CMFRD-CELL STR,  9 DATA CMBK-CELL STR,
   9 0 MOVZ,
   9 DATA TSIG-A-CELL STR,   9 DATA TSIG-U-CELL STR,
   9 DATA TCSIG-A-CELL STR,  9 DATA TCSIG-U-CELL STR,
   9 DATA CRSIG-A-CELL STR,  9 DATA CRSIG-U-CELL STR,
   9 DATA DOESB-CELL STR,
   9 DATA TRUSTED-CELL STR,
   9 VRALL MOVZ,  9 DATA VRFREE-CELL STR, ;
s" em-reset-compile-state" s" --" TRUST

: EM-EVAL-UNDEF-ROLLBACK ( -- )
   9 DATA EVALD-CELL LDR,  9 9 1 SUBI,  9 DATA EVALD-CELL STR,
   9 14 15 C-EVAL-FRAME-ADDR
   CP 14 40 LDR,  NDICT 14 48 LDR,  XDS 14 32 LDR,
   9 14 56 LDR,  9 DATA DP-CELL STR,
   EM-RESET-COMPILE-STATE
   9 14 0 LDR,  9 DATA INP-CELL STR,
   9 14 8 LDR,  9 DATA INE-CELL STR,
   9 1 MOVZ,  9 DATA EVALERR-CELL STR,
   9 14 24 LDR,  SP 9 0 ADDI,
   9 14 16 LDR,  9 BR, ;
s" em-eval-undef-rollback" s" --" TRUST

\ A throw whose nearest handler lies beyond one or more active evaluate boundaries
\ lands here (BTHROW branch via EVALREC-CELL), x15 = throw code. Each escaped eval
\ frame is rolled back — input cursor, dictionary top (CP/NDICT), data-stack base
\ (XDS), data pointer (DP), and compile state — and EVALERR-CELL records the code,
\ so the handler resumes with clean state. Popping stops as soon as EVALD reaches 0
\ or the nearest handler (x11, read once because EM-RESET-COMPILE-STATE zeroes the
\ HND-CELL copy) is inside the current eval frame; then the throw is delivered to
\ that handler / REPL / process exit exactly as the non-evaluate path does.
: EM-EVAL-THROW-RECOVER ( -- )
   LEVALREC LABEL@ LBL,
   LBL LEVLL !  LBL LEVLP !  LBL LEVLD !  LBL LEVLN !  LBL LEVLR !
   11 DATA 8 LDR,                                     \ x11 = nearest handler (read once)
   LEVLL LABEL@ LBL,
      12 DATA EVALD-CELL LDR,  12 LEVLD LABEL@ CBZ,   \ no eval frame left → unwind to handler
      12 12 1 SUBI,  12 13 14 C-EVAL-FRAME-ADDR       \ x13 = &frame[EVALD-1]
      14 13 24 LDR,                                   \ x14 = eval-entry SP (boundary); x13 stays &frame
      11 LEVLP LABEL@ CBZ,                            \ no handler → pop (escape)
      11 14 CMP,  C-LS LEVLD LABEL@ BCOND,            \ handler inside this frame → unwind to it
   LEVLP LABEL@ LBL,
      12 13 0 LDR,   12 DATA INP-CELL STR,
      12 13 8 LDR,   12 DATA INE-CELL STR,
      CP 13 40 LDR,  NDICT 13 48 LDR,  XDS 13 32 LDR,
      12 13 56 LDR,  12 DATA DP-CELL STR,
      15 DATA EVALERR-CELL STR,                       \ EVALERR = code
      12 DATA EVALD-CELL LDR,  12 12 1 SUBI,  12 DATA EVALD-CELL STR,
      EM-RESET-COMPILE-STATE                          \ clobbers x9 only; x11,x15 preserved
      LEVLL LABEL@ B,
   LEVLD LABEL@ LBL,
   9 15 0 ADDI,                                       \ x9 = code
   11 LEVLN LABEL@ CBZ,
   19 11 8 LDR,  10 11 0 LDR,  10 DATA 8 STR,
   30 11 32 LDR,  12 11 24 LDR,  13 11 16 LDR,
   SP 13 0 ADDI,  12 BR,
   LEVLN LABEL@ LBL,
   10 DATA REPLH-CELL LDR,  10 LEVLR LABEL@ CBZ,
   10 DATA RRECP-CELL LDR,  10 BR,
   \ No handler and no REPL: fall into the shared uncaught-throw exit (x9 = code).
   \ LEVLR (eval-frame path) and LUNCAUGHT (BTHROW THROW-NOREC path, reached via
   \ UNCGH-CELL - stored at boot since a leaf prim cannot name this label) share one
   \ address. A code in [1,255] is kernel-representable and is an established
   \ deliberate exit contract (lib/argv.f usage 64, check hook 70, lint findings 1),
   \ so it exits byte-identically to before: exit_group(code), no extra output. Any
   \ other code would be kernel-masked to `code & 0xFF` - the fail-open class this
   \ closes (-2816 exited 0 SILENTLY, -2802 exited an aliased 14) - so it is instead
   \ reported as "hb: uncaught throw code <n>\n" on fd 2 (signed itoa mirrors
   \ G-PRINT9) and exits the deterministic UNCAUGHT-RC. x15 keeps the code across
   \ the message write (the kernel preserves x2-x15, as EMIT-SOURCE-READ's open-error
   \ path relies on for x12). Never returns - no RET, keeps FPRIM-L throw leaf-safe.
   LEVLR LABEL@ LBL,  LUNCAUGHT LABEL@ LBL,
   LBL LUNCRPT !  LBL LUNCPOS !  LBL LUNCLOOP !  LBL LUNCDONE !
   15 9 0 ADDI,                                        \ x15 = code (survives writes; x9-x14 are itoa scratch)
   0 9 0 ADDI,                                         \ x0 = code for the passthrough exit
   9 1 CMPI,    C-LT LUNCRPT LABEL@ BCOND,
   9 255 CMPI,  C-GT LUNCRPT LABEL@ BCOND,
   NR-EXIT-GROUP SYS,                                  \ representable deliberate code: exit(code) as before
   LUNCRPT LABEL@ LBL,                                 \ out-of-range: report, then exit UNCAUGHT-RC
   1 LUNCMSG LABEL@ ADR,  0 2 MOVZ,  2 UNCMSG-LEN MOVZ,  NR-WRITE SYS,   \ write(2,"hb: uncaught throw code ",24)
   9 15 0 ADDI,                                        \ x9 = code for the itoa
   SP SP $20 SUBI,  12 SP $20 ADDI,
   13 $A MOVZ,  12 12 1 SUBI,  13 12 0 STRB,           \ trailing newline
   14 0 MOVZ,  9 0 CMPI,
   C-GE LUNCPOS LABEL@ BCOND,
   14 1 MOVZ,  9 SP 9 SUB,                             \ x9 = -x9 (abs); x14 = sign flag
   LUNCPOS LABEL@ LBL,
   10 $A MOVZ,
   LUNCLOOP LABEL@ LBL,
   11 9 10 SDIV,  13 11 10 MUL,  13 9 13 SUB,
   13 13 $30 ADDI,  12 12 1 SUBI,  13 12 0 STRB,
   9 11 0 ADDI,  9 LUNCLOOP LABEL@ CBNZ,
   14 LUNCDONE LABEL@ CBZ,
   13 $2D MOVZ,  12 12 1 SUBI,  13 12 0 STRB,          \ leading '-'
   LUNCDONE LABEL@ LBL,
   0 2 MOVZ,  1 12 0 ADDI,  2 SP $20 ADDI,  2 2 12 SUB,
   NR-WRITE SYS,                                       \ write(2, digits, len)
   0 UNCAUGHT-RC MOVZ,  NR-EXIT-GROUP SYS, ;
s" em-eval-throw-recover" s" --" TRUST

: EM-REPL-RECOVER ( -- )
   LRREC LABEL@ LBL,
   0 2 MOVZ,  1 LQNL LABEL@ ADR,  2 2 MOVZ,  NR-WRITE SYS,
   CP DATA RSAVCP-CELL LDR,
   NDICT DATA RSAVND-CELL LDR,
   9 DATA RSAVDP-CELL LDR,  9 DATA DP-CELL STR,
   9 DATA S0-CELL LDR,  XDS 9 0 ADDI,
   EM-RESET-COMPILE-STATE
   9 DATA RSAVSP-CELL LDR,  SP 9 0 ADDI,
   LREAD LABEL@ B, ;
s" em-repl-recover" s" --" TRUST

\ Reject rc: an undefined word in a `:`-body is a rejected definition. Used both
\ as the catchable throw code delivered to an enclosing catch and as the
\ fail-closed process exit code, so caught -> code 70 and uncaught -> rc70 are the
\ same value (matches the top-level LRDIE exit and driver-io's "check reject 70").
70 constant RC-REJECT

\ Interpret-level reject diagnostics. LUNDEF: undefined token. LWIDE: a found
\ word whose DNAME-WIDE dict flag is set (recorded effect carries a
\ wider-than-cell layout value) was executed or ticked at interpret level -
\ running it would land a multi-cell bundle on the untyped interpret stack,
\ where scalar dup/drop/swap silently corrupt it (dot
\ habu-tfam-12-interpret-10b385b1); checked definitions own bundle work. Both
\ print their diagnostic + the token, then share the LDIAGRET recovery tail:
\ inside evaluate the aborted compile unwinds as a catchable RC-REJECT (70) throw
\ via the eval throw-recovery (restoring RX first), else tty REPL recovery, else exit 70.
: EM-COMPILE-UNDEF ( -- )
   LUNDEF LABEL@ LBL,
   SP SP 16 SUBI,  9 $494645444E552D45 LIT64,  9 SP 0 STR,  9 $000000203A44454E LIT64,  9 SP 8 STR,  0 2 MOVZ,  1 SP 0 ADDI,  2 13 MOVZ,  NR-WRITE SYS,  SP SP 16 ADDI,  0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,  0 2 MOVZ,  1 LQNL LABEL@ ADR,  1 1 1 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
   LDIAGRET LABEL@ LBL,                                     \ shared LUNDEF/LWIDE recovery tail (TFAM)
   9 DATA EVALD-CELL LDR,  9 LUN0 LABEL@ CBZ,               \ EVALD==0 -> top-level path (LUN0), unchanged
      \ Inside evaluate: the aborted nested :-compile unwinds as a catchable throw
      \ (RC-REJECT) via the eval throw-recovery (the same LEVALREC path BTHROW uses),
      \ which rolls back every escaped eval frame -- dropping the partial definition
      \ (CP/NDICT/XDS/DP) -- and delivers to the enclosing catch, or fails closed with
      \ rc70 when no handler exists (exactly like LRDIE). We abort mid-compile with the
      \ dict region RW; restore RX before re-entering executable (EV/handler) code.
      2 5 MOVZ,  LPROT LABEL@ BL,                           \ region -> RX
      15 RC-REJECT MOVZ,                                    \ x15 = throw code
      10 DATA EVALREC-CELL LDR,  10 BR,                     \ -> LEVALREC (frame unwind + deliver)
   LUN0 LABEL@ LBL,
   9 DATA REPLH-CELL LDR,  9 LRDIE LABEL@ CBZ,
   EM-REPL-RECOVER
   LRDIE LABEL@ LBL,
   0 70 MOVZ,  NR-EXIT-GROUP SYS,
   LWIDE LABEL@ LBL,                                   \ branch target: TKA/TKL still hold the token
   0 2 MOVZ,  1 LWIDEMSG LABEL@ ADR,  2 WIDEMSG-LEN MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 2 MOVZ,  1 LQNL LABEL@ ADR,  1 1 1 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
   LDIAGRET LABEL@ B,
   LINTERNAL LABEL@ LBL,                               \ branch target: TKA/TKL still hold the token (DNAME-INT reject)
   0 2 MOVZ,  1 LINTMSG LABEL@ ADR,  2 INTMSG-LEN MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 2 MOVZ,  1 LQNL LABEL@ ADR,  1 1 1 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
   LDIAGRET LABEL@ B, ;
s" em-compile-undef" s" --" TRUST

: EM-EVAL-CLEAN-EXIT ( -- )
   9 DATA EVALD-CELL LDR,  9 9 1 SUBI,  9 DATA EVALD-CELL STR,
   9 14 15 C-EVAL-FRAME-ADDR
   9 14 0 LDR,  9 DATA INP-CELL STR,
   9 14 8 LDR,  9 DATA INE-CELL STR,
   9 0 MOVZ,  9 DATA EVALERR-CELL STR,
   9 14 24 LDR,  SP 9 0 ADDI,
   9 14 16 LDR,  9 BR, ;
s" em-eval-clean-exit" s" --" TRUST

: EM-REPL-READ ( -- )
   LREAD LABEL@ LBL,
   9 SP 0 ADDI,  9 DATA RSAVSP-CELL STR,
   CP DATA RSAVCP-CELL STR,
   NDICT DATA RSAVND-CELL STR,
   9 DATA DP-CELL LDR,  9 DATA RSAVDP-CELL STR,
   9 DATA REPLH-CELL LDR,  9 BLR,
   XDS XDS 8 SUBI,  10 XDS 0 LDR,
   XDS XDS 8 SUBI,  11 XDS 0 LDR,
   10 LRBYE LABEL@ CBZ,
   11 DATA INP-CELL STR,  11 11 10 ADD,  11 DATA INE-CELL STR,  LMAIN LABEL@ B, ;
s" em-repl-read" s" --" TRUST

: EM-COMPILE-EXIT ( -- )
   LBL {: aoskip:label :}
   LEXIT LABEL@ LBL,
   9 DATA EVALD-CELL LDR,  9 LEX0 LABEL@ CBZ,
      EM-EVAL-CLEAN-EXIT
   LEX0 LABEL@ LBL,                                          \ top-level source exhausted (EVALD==0), cp@ clean here
   9 DATA AOT-SEED-DONE-CELL LDR,  9 aoskip CBNZ,            \ already seeded -> skip
   9 DATA AOT-SEED-ARM-CELL LDR,  9 aoskip CBZ,              \ armed only on the interactive repl entry
      EM-SEED-AOT                                            \ seed the AOT REPL once, post-cold-prefix
      9 1 MOVZ,  9 DATA AOT-SEED-DONE-CELL STR,
   aoskip LBL,
   9 DATA REPLH-CELL LDR,  9 LRBYE LABEL@ CBZ,
   0 1 MOVZ,  1 LOKS LABEL@ ADR,  2 4 MOVZ,  NR-WRITE SYS,
   EM-REPL-READ
   LRBYE LABEL@ LBL,
   0 0 MOVZ,  NR-EXIT-GROUP SYS, ;
s" em-compile-exit" s" --" TRUST

\ Top-level data-stack underflow diagnostic. Reached from the LMAIN depth-floor
\ guard when the just-interpreted word left XDS below S0 (proven underflow). Print
\ `E-UNDERFLOW: <word>` naming the offending token (TKA/TKL still hold it, LTOK has
\ not overwritten them this iteration), then recover exactly like the undefined-word
\ path: roll back one evaluate frame if inside EVALUATE, else recover + re-read in
\ the REPL, else fail closed with exit 70 in a batch/--load load. Never a signal.
: EM-INTERPRET-UNDERFLOW ( -- )
   LBL LBL {: uf0:label ufdie:label :}
   LUNDERFLOW LABEL@ LBL,
   SP SP 16 SUBI,  9 $465245444E552D45 LIT64,  9 SP 0 STR,  9 $000000203A574F4C LIT64,  9 SP 8 STR,  0 2 MOVZ,  1 SP 0 ADDI,  2 13 MOVZ,  NR-WRITE SYS,  SP SP 16 ADDI,  0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,  0 2 MOVZ,  1 LQNL LABEL@ ADR,  1 1 1 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
   9 DATA EVALD-CELL LDR,  9 uf0 CBZ,
      EM-EVAL-UNDEF-ROLLBACK
   uf0 LBL,
   9 DATA REPLH-CELL LDR,  9 ufdie CBZ,
   LRREC LABEL@ B,
   ufdie LBL,
   0 70 MOVZ,  NR-EXIT-GROUP SYS, ;
s" em-interpret-underflow" s" --" TRUST

\ construct operand steps (TFAM 10 slice 2, docs §16 Constructors). CMM=1: the
\ token is the FAMILY operand — resolve eagerly through tfl-con-fam? (owner-only
\ scope, sum/enum gate) so no operand string must survive a REPL line refill;
\ stash the id in CMFAM-CELL, arm CMM=2. CMM=2: the token is the VARIANT operand
\ — tfl-cvar? returns ( tag pads ok ); emit pads x VS-constant 0 pushes + one
\ tag push (LVPUSHC, byte-identical to how the item-8 generated-constructor body
\ literals compile), clear the mode. Either resolution failure dies fail-closed
\ at ITS token with a named message + exit 70, the same class as E-UNDEFINED;
\ a foreign-package or wrong-kind family is "unknown" by owner-scope design, and
\ the checker's richer E-CONSTRUCT-* diagnostics live on the check-only paths,
\ which never reach this engine leg. `;` (or any closer) arriving mid-form is
\ consumed as the operand and dies the same way (MD-CON-TRUNC's engine mirror).
\ Region protection: the compile loop holds the code REGION RW between the
\ colon start and the `;` flush, but the TFL bridge targets are checker words
\ COMPILED INTO that region — executing them from a writable page SIGBUSes
\ under W^X. Each leg therefore opens an RX window (LPROT 5) around the find +
\ call and returns to RW (LPROT 3) before any emission resumes; DATA stores
\ (CMFAM/CMM) are outside the flipped region and legal in either state, and
\ the die legs exit the process, so their protection state is irrelevant.
: EM-ADT-CON-FAM ( -- )                 \ CMM=1 leg: resolve family, arm state 2
   LBL LBL {: fmsg:label fok:label :}
   LBCAP LABEL@ BL,                     \ operand reaches the checker's body too
   2 5 MOVZ,  LPROT LABEL@ BL,          \ region -> RX: checker-call window
   LTFLCONFAM 12 C-FIND-GLOBAL
   9 DATA TKA-CELL LDR,  9 G-PUSH
   9 DATA TKL-CELL LDR,  9 G-PUSH
   C-CALL-X11-SAVED
   10 G-POP                             \ ok flag (top)
   9 G-POP                              \ family id
   10 fok CBNZ,
      0 2 MOVZ,  1 fmsg ADR,  2 CONFMSG-LEN MOVZ,  NR-WRITE SYS,
      70 C-DIE-TOKEN-NL
   fmsg LBL,  s" hb: construct: unknown family: " BYTES,
   fok LBL,
   9 DATA CMFAM-CELL STR,
   12 2 MOVZ,  12 DATA CMM-CELL STR,
   2 3 MOVZ,  LPROT LABEL@ BL,          \ region -> RW: resume emission
   LMAIN LABEL@ B, ;
s" em-adt-con-fam" s" --" TRUST

: EM-ADT-CON-PUSHES ( -- )              \ pads x 0 + tag as VS constants (x12=pads, x13=tag)
   LBL LBL {: ploop:label pdone:label :}
   SP SP 16 SUBI,  12 SP 0 STR,  13 SP 8 STR,   \ frame the counters: LVPUSHC may
   2 3 MOVZ,  LPROT LABEL@ BL,                  \ spill (emission -> region RW first)
   ploop LBL,
      12 SP 0 LDR,  12 pdone CBZ,
      11 0 MOVZ,  LVPUSHC LABEL@ BL,
      12 SP 0 LDR,  12 12 1 SUBI,  12 SP 0 STR,  ploop B,
   pdone LBL,
   11 SP 8 LDR,  LVPUSHC LABEL@ BL,
   SP SP 16 ADDI, ;
s" em-adt-con-pushes" s" --" TRUST

: EM-ADT-CON-VAR ( -- )                 \ CMM=2 leg: resolve variant, emit, mode off
   LBL LBL {: vmsg:label vok:label :}
   LBCAP LABEL@ BL,                     \ operand reaches the checker's body too
   2 5 MOVZ,  LPROT LABEL@ BL,          \ region -> RX: checker-call window
   LTFLCVAR 9 C-FIND-GLOBAL
   9 DATA TKA-CELL LDR,  9 G-PUSH
   9 DATA TKL-CELL LDR,  9 G-PUSH
   9 DATA CMFAM-CELL LDR,  9 G-PUSH
   C-CALL-X11-SAVED
   10 G-POP                             \ ok flag (top)
   12 G-POP                             \ pads
   13 G-POP                             \ tag
   10 vok CBNZ,
      0 2 MOVZ,  1 vmsg ADR,  2 CONVMSG-LEN MOVZ,  NR-WRITE SYS,
      70 C-DIE-TOKEN-NL
   vmsg LBL,  s" hb: construct: unknown variant: " BYTES,
   vok LBL,
   EM-ADT-CON-PUSHES                    \ frames the counters, then flips back to RW
   12 0 MOVZ,  12 DATA CMM-CELL STR,
   LMAIN LABEL@ B, ;
s" em-adt-con-var" s" --" TRUST

\ MATCH lowering legs (TFAM 10 slice 3, docs §16). CMM=3/4/5 continue the token
\ machine J-MATCH armed at `match`: 3 = want family, 4 = want variant-or-`;match`,
\ 5 = want `of`. Like the construct legs each opens an RX window (LPROT 5) only
\ around the checker-friend bridge call (the bridge targets are checker words
\ compiled INTO the mid-body RW code region — a BLR from a writable page SIGBUSes
\ under W^X) and flips back to RW (LPROT 3) before any emission; each LBCAPs its
\ consumed operand so the checker body sees it (the keyword-phase LBCAP only sees
\ `match`/`endof`); resolution failure dies fail-closed at ITS token, exit 70.
\ The scrutinee is the width-expanded bundle spilled to the physical stack by
\ J-MATCH's CF-ENTRY; each variant peeks the tag at [x19,#-8] in place.

\ C-DIE-BAD-TAG ( x11=family-name-addr  x12=family-name-len -- )
\   Emit the invalid-tag die INLINE into the user word with no normal
\   continuation: a jump over the message, the message "hb: bad <family> tag\n"
\   copied inline (the NAME BYTES travel with the word — never a live pointer into
\   the mmap-relocatable TF-STR pool), then a self-contained write(2,msg,len) +
\   exit_group(E-BAD-TAG). Modeled on C-DIE-TOKEN-NL's write/exit tail and C-SDQ's
\   inline-byte copy, but every instruction is C-EMITW'd into the user code region
\   (x28=CP), not the engine. Runs with the region RW. Clobbers x5-x16.
: C-DIE-BAD-TAG ( -- )
   LBL LBL LBL LBL LBL LBL {: p1:label p2:label s1:label s2:label t1:label t2:label :}
   SP SP $20 SUBI,  11 SP 0 STR,  12 SP 8 STR,        \ frame name addr/len (loops clobber x11)
   15 CP 0 ADDI,  15 SP 16 STR,                        \ B-over addr (patch target)
   9 $14000000 LIT64,  LCEMIT LABEL@ BL,               \ emit B placeholder
   16 CP 0 ADDI,  16 SP 24 STR,                        \ msg start addr
   11 LBADTAGPFX LABEL@ ADR,  9 8 MOVZ,                \ copy "hb: bad " (8)
   p1 LBL,  9 p2 CBZ,  14 11 0 LDRB,  14 28 0 STRB,  28 28 1 ADDI,  11 11 1 ADDI,  9 9 1 SUBI,  p1 B,
   p2 LBL,
   11 SP 0 LDR,  9 SP 8 LDR,                           \ copy family name
   s1 LBL,  9 s2 CBZ,  14 11 0 LDRB,  14 28 0 STRB,  28 28 1 ADDI,  11 11 1 ADDI,  9 9 1 SUBI,  s1 B,
   s2 LBL,
   11 LBADTAGSFX LABEL@ ADR,  9 5 MOVZ,                \ copy " tag\n" (5)
   t1 LBL,  9 t2 CBZ,  14 11 0 LDRB,  14 28 0 STRB,  28 28 1 ADDI,  11 11 1 ADDI,  9 9 1 SUBI,  t1 B,
   t2 LBL,
   28 28 3 ADDI,  5 -4 LIT64,  28 28 5 AND,            \ align CP to 4
   9 SP 16 LDR,  LPAT LABEL@ BL,                       \ patch B-over -> past the bytes
   $D2800040 C-EMITW                                   \ movz x0, #2  (fd)
   11 SP 24 LDR,  5 11 28 SUB,  8 $10000001 LIT64,     \ adr x1, msg : d = msg - CP, Rd=x1
      6 3 MOVZ,  7 5 6 AND,  7 7 29 LSLI,  8 8 7 ORR,
      7 5 2 LSRI,  6 $7FFFF LIT64,  7 7 6 AND,  7 7 5 LSLI,  8 8 7 ORR,
      9 8 0 ADDI,  LCEMIT LABEL@ BL,
   14 SP 8 LDR,  14 14 13 ADDI,                        \ x14 = 8 + name-len + 5 = name-len + 13
   9 $D2800002 LIT64,  14 14 5 LSLI,  9 9 14 ORR,  LCEMIT LABEL@ BL,   \ movz x2, #len
   9 SYS-EMIT-WRITE LIT64,  LCEMIT LABEL@ BL,          \ movz x_sys, #NR-WRITE
   SYS-EMIT-SVC C-EMITW                                \ svc
   9 $D2800000 E-BAD-TAG 32 * + LIT64,  LCEMIT LABEL@ BL,   \ movz x0, #E-BAD-TAG
   9 SYS-EMIT-EXIT LIT64,  LCEMIT LABEL@ BL,           \ movz x_sys, #NR-EXIT-GROUP
   SYS-EMIT-SVC C-EMITW                                \ svc
   SP SP $20 ADDI, ;
s" c-die-bad-tag" s" --" TRUST

: EM-MATCH-SEMI ( -- )                  \ ;match: invalid-tag die + join + pop frame
   LBL LBL {: jl:label jd:label :}
   2 5 MOVZ,  LPROT LABEL@ BL,          \ region -> RX: checker-friend call window
   LTFLNAME 10 C-FIND-GLOBAL
   14 DATA CMFRD-CELL LDR,  14 14 1 SUBI,  14 14 3 LSLI,  14 14 CMFR-OFF ADDI,  15 DATA 14 ADD,  9 15 0 LDR,
   9 G-PUSH                             \ top match-frame family id
   C-CALL-X11-SAVED
   12 G-POP                             \ family name length
   11 G-POP                             \ family name address
   SP SP 16 SUBI,  11 SP 0 STR,  12 SP 8 STR,    \ frame the name span across the RW flip
   2 3 MOVZ,  LPROT LABEL@ BL,          \ region -> RW: emission
   11 SP 0 LDR,  12 SP 8 LDR,  SP SP 16 ADDI,
   C-DIE-BAD-TAG                        \ emit the inline bad-tag die (no continuation)
   jl LBL,                              \ J-ENDCASE-style: patch every ENDOF B to the join
      LCFPOP LABEL@ BL,
      9 jd CBZ,
      LPAT LABEL@ BL,
      jl B,
   jd LBL,
   14 DATA CMFRD-CELL LDR,  14 14 1 SUBI,  14 DATA CMFRD-CELL STR,   \ pop match-frame level
   12 0 MOVZ,  12 DATA CMM-CELL STR,
   LMAIN LABEL@ B, ;
s" em-match-semi" s" --" TRUST

: EM-ADT-MATCH-FAM ( -- )               \ CMM=3: resolve match family (signature scope)
   LBL LBL {: fmsg:label fok:label :}
   LBCAP LABEL@ BL,
   2 5 MOVZ,  LPROT LABEL@ BL,
   LTFLMATCHFAM 14 C-FIND-GLOBAL
   9 DATA TKA-CELL LDR,  9 G-PUSH
   9 DATA TKL-CELL LDR,  9 G-PUSH
   C-CALL-X11-SAVED
   10 G-POP                             \ ok flag
   9 G-POP                              \ family id
   10 fok CBNZ,
      0 2 MOVZ,  1 fmsg ADR,  2 MFAMMSG-LEN MOVZ,  NR-WRITE SYS,
      70 C-DIE-TOKEN-NL
   fmsg LBL,  s" hb: match: unknown family: " BYTES,
   fok LBL,
   14 DATA CMFRD-CELL LDR,  14 14 1 SUBI,  14 14 3 LSLI,  14 14 CMFR-OFF ADDI,  15 DATA 14 ADD,  9 15 0 STR,
   12 4 MOVZ,  12 DATA CMM-CELL STR,
   2 3 MOVZ,  LPROT LABEL@ BL,
   LMAIN LABEL@ B, ;
s" em-adt-match-fam" s" --" TRUST

: EM-ADT-MATCH-VAR ( -- )               \ CMM=4: ;match -> semi, else resolve variant
   LBL LBL LBL {: notsemi:label vmsg:label vok:label :}
   LBCAP LABEL@ BL,
   0 LKWSEMIMATCH LABEL@ ADR,  1 6 MOVZ,  LKWCMP LABEL@ BL,
   0 notsemi CBZ,
      EM-MATCH-SEMI
   notsemi LBL,
   2 5 MOVZ,  LPROT LABEL@ BL,
   LTFLCVAR 9 C-FIND-GLOBAL
   9 DATA TKA-CELL LDR,  9 G-PUSH
   9 DATA TKL-CELL LDR,  9 G-PUSH
   14 DATA CMFRD-CELL LDR,  14 14 1 SUBI,  14 14 3 LSLI,  14 14 CMFR-OFF ADDI,  15 DATA 14 ADD,  9 15 0 LDR,  9 G-PUSH
   C-CALL-X11-SAVED
   10 G-POP                             \ ok flag
   12 G-POP                             \ pads (M-p)
   13 G-POP                             \ tag
   10 vok CBNZ,
      0 2 MOVZ,  1 vmsg ADR,  2 MVARMSG-LEN MOVZ,  NR-WRITE SYS,
      70 C-DIE-TOKEN-NL
   vmsg LBL,  s" hb: match: unknown variant: " BYTES,
   vok LBL,
   13 DATA CMTAG-CELL STR,
   12 DATA CMPADS-CELL STR,
   12 5 MOVZ,  12 DATA CMM-CELL STR,
   2 3 MOVZ,  LPROT LABEL@ BL,
   LMAIN LABEL@ B, ;
s" em-adt-match-var" s" --" TRUST

: EM-ADT-MATCH-OF ( -- )                \ CMM=5: require `of`, emit compare + prologue
   LBL LBL {: emsg:label eok:label :}
   LBCAP LABEL@ BL,
   0 LKWOF LABEL@ ADR,  1 2 MOVZ,  LKWCMP LABEL@ BL,
   0 eok CBNZ,
      0 2 MOVZ,  1 emsg ADR,  2 MOFMSG-LEN MOVZ,  NR-WRITE SYS,
      70 C-DIE-TOKEN-NL
   emsg LBL,  s" hb: match: expected of: " BYTES,
   eok LBL,
   14 DATA CMTAG-CELL LDR,
   9 C-CALL-MOVZ-X16 LIT64,  14 14 5 LSLI,  9 9 14 ORR,  LCEMIT LABEL@ BL,   \ movz x16, #tag
   $F85F8269 C-EMITW                    \ ldur x9,[x19,#-8]  peek tag
   $EB10013F C-EMITW                    \ cmp x9,x16
   $9A9F17E9 C-EMITW                    \ cset x9,eq
   C-PUSHCP
   $B4000009 C-EMITW                    \ cbz x9,+0  (skip to next variant if no match)
   14 DATA CMPADS-CELL LDR,  14 14 1 ADDI,  14 14 3 LSLI,
   9 $D1000273 LIT64,  14 14 10 LSLI,  9 9 14 ORR,  LCEMIT LABEL@ BL,   \ sub x19,x19,#(8*(1+pads))
   14 DATA CMBK-CELL LDR,  14 14 1 LSLI,  14 14 1 ORRI,  14 DATA CMBK-CELL STR,   \ push match-branch marker (1)
   12 0 MOVZ,  12 DATA CMM-CELL STR,
   LMAIN LABEL@ B, ;
s" em-adt-match-of" s" --" TRUST

\ EM-COMPILE-ADT-MODE (TFAM 10 slices 1-3): fail-closed head of the compile
\ dispatch. CMM-CELL mirrors the checker's construct+match token machine; the
\ operand tokens are consumed HERE — before the semi/local/keyword/literal/call/
\ undefined path — so captured family/variant/of/;match tokens never reach
\ dictionary lookup (the checker's capture discipline). 1/2 = construct, 3/4/5 =
\ match; each leg branches to LMAIN so the legs never fall into one another.
: EM-COMPILE-ADT-MODE ( -- )
   LBL LBL LBL LBL LBL {: s2:label s3:label s4:label s5:label off:label :}
   9 DATA CMM-CELL LDR,  9 off CBZ,
   9 2 CMPI,  C-EQ s2 BCOND,
   9 3 CMPI,  C-EQ s3 BCOND,
   9 4 CMPI,  C-EQ s4 BCOND,
   9 5 CMPI,  C-EQ s5 BCOND,
   EM-ADT-CON-FAM
   s2 LBL,  EM-ADT-CON-VAR
   s3 LBL,  EM-ADT-MATCH-FAM
   s4 LBL,  EM-ADT-MATCH-VAR
   s5 LBL,  EM-ADT-MATCH-OF
   off LBL, ;
s" em-compile-adt-mode" s" --" TRUST
: EM-COMPILE ( -- )
   LBL {: lnotsemi :}
   LCOMPILE LABEL@ LBL,
   EM-COMPILE-ADT-MODE
   lnotsemi EM-COMPILE-SEMI
   EM-COMPILE-LOCAL
   EM-COMPILE-P2WIDE
   EM-COMPILE-KEYWORDS
   EM-COMPILE-LITERAL
   EM-COMPILE-OPS
   EM-COMPILE-CALL
   EM-COMPILE-UNDEF
   EM-COMPILE-EXIT
   EM-EVAL-THROW-RECOVER ;    \ branch-target only (reached via EVALREC-CELL); ends by branching
s" em-compile" s" --" TRUST

: EMIT-MAIN ( -- )
   LBL LMAIN !  LBL LEXIT !  LBL LCOMPILE !  LBL LUNDEF !  LBL LUNDERFLOW !  LBL LARITY !
   EM-STARTUP  EM-COMMENT  EM-INTERPRET  EM-COMPILE  EM-INTERPRET-UNDERFLOW ;
s" emit-main" s" --" TRUST

\ Pre-execution arity guard (LARITY). A deref/execute primitive (@ ! +! c@ c!
\ atomic@ atomic! atomic-add atomic-cas count type execute run-in-stack) as the
\ LITERAL FIRST top-level token faults inside the primitive body (SIGSEGV, crash
\ handler exit 134) BEFORE the post-token LMAIN depth-floor guard (EM-COMMENT /
\ EM-INTERPRET-UNDERFLOW) can observe XDS < S0. The interpret-find dispatch BLs here
\ after a successful dictionary find: x11 = the resolved xt, x10 is loaded with the current
\ data-stack depth in cells, and each guarded prim's baked entry-label address is
\ compared against x11. If x11 matches and depth < the prim's minimum input count,
\ we divert to LUNDERFLOW (TKA/TKL still name the token) for a clean E-UNDERFLOW
\ instead of a signal; otherwise LARITY returns and the interpret BLR proceeds
\ unchanged. Every non-guarded word runs the compare chain and falls through
\ (effective min-in 0): no behavior change. The prim entry labels are resolved by
\ EMIT-ARITY-GUARD after EMIT-PRIMS, so this routine is emitted post-prims.
: ARITY-EMIT ( n n -- )               \ ( prim-entry-label min ) : x10=depth cells, x11=xt
   LBL {: glbl:n min:n nxt:label :}
   14 glbl >LABEL ADR,  11 14 CMP,  C-NE nxt BCOND,   \ x11 != this prim's xt -> skip
      10 min CMPI,  C-LT LUNDERFLOW LABEL@ BCOND,     \ depth < min-in -> underflow diagnostic
      RET,                                            \ matched, depth ok -> proceed to BLR
   nxt LBL, ;

: EMIT-ARITY-GUARD ( -- )             \ bake LARITY: depth into x10, then a compare row per guarded prim
   LARITY LABEL@ LBL,
   9 DATA S0-CELL LDR,  10 XDS 9 SUB,  10 10 3 ASRI,   \ x10 = (XDS - S0) / 8 = depth in cells
   0 BEGIN dup GDR-N @ < WHILE
      dup cells GDR-LBL + @  over cells GDR-MIN + @  ARITY-EMIT
      1+
   REPEAT drop
   RET, ;
variable SRCA
: SRCA@ ( -- ptr u8 )
   SRCA @ ;
s" SRCA@" s" -- ptr u8" TRUST

: EMIT-RESET-BUILDER ( ptr u8 n -- )
   SRCN !  SRCA !
   ASM-INIT  0 #PL !  0 PNP ! ;

: EMIT-LABEL-CORE ( -- )
   LBL LANCHOR !  LBL LFIND !  LBL LNUM !  LBL LDICT !  LBL LSRC !
   LBL LCEMIT !  LBL LTOK !  LBL LPROT !  LBL LFLUSH !  LBL LNCOUNT !
   LBL LAOTCODE !  LBL LAOTDICT !  LBL LAOTCODELEN !
   LBL LAOTNREC !  LBL LAOTNSITE !  LBL LAOTSITES !  LBL LAOTNAMES !
   LBL LAOTNDSITE !  LBL LAOTDSITES !  LBL LAOTDATAD0 !  LBL LAOTDATASIZE !
   LBL LAOTNCSITE !  LBL LAOTCSITES !  LBL LAOTCODEB0 !
   LBL LAOTBOOTRUN !
   LBL LAOTNPWID !  LBL LAOTPWID !  LBL LPROTWIDQ !
   LBL LBCAP !  LBL LBCS !  LBL LESCDEC !  LBL LESCHEX !  LBL LESCSCAN !  LBL LESCCOPY !
   LBL LSNAPRBD !  LBL LSNAPRBC !  LBL LHIDXADD !  LBL LHIDXBUILD !
   LBL LAOTWIDGATE !
   LBL LCFPUSH !  LBL LCFPOP !  LBL LPAT !  LBL LKWCMP ! ;

: EMIT-LABEL-CONTROL ( -- )
   LBL LKWIF !  LBL LKWTHEN !  LBL LKWELSE !  LBL LKWBEGIN !
   LBL LKWUNTIL !  LBL LKWAGAIN !  LBL LKWWHILE !  LBL LKWREPEAT !
   LBL LKWCASE !  LBL LKWOF !  LBL LKWENDOF !  LBL LKWENDCASE !
   LBL LKWCREATE !  LBL LKWVAR !  LBL LKWSQ !  LBL LKWCQ !  LBL LKWDOTQ !
   LBL LKWESQ !  LBL LKWECQ !  LBL LKWEDOTQ !
   LBL LKWTYPE !
   LBL LKWTICK !  LBL LKWBTICK !
   LBL LKWLBRACE !  LBL LKWENDLOC !  LBL LLOC-FIND !  LBL LKWCONST !
   LBL LKWDO !  LBL LKWLOOP !  LBL LKWI !
   LBL LKWTOR !  LBL LKWRFROM !  LBL LKWRFET !
   LBL LKWEXIT !  LBL LKWREC !
   LBL LKWQDO !  LBL LKWPLOOP !  LBL LKWJ !  LBL LKWLEAVE !  LBL LKWUNLOOP !
   LBL LKWCHAR !  LBL LKWBCHAR !
   LBL LKWIMM !  LBL LKWPOST !  LBL LKWCOMPC !  LBL LKWDOES !
   LBL LKWTRUSTED !  LBL LKWTRUST !  LBL LKWCHKDOES !  LBL LKWKERNEL !
   LBL LKWPACKAGE !  LBL LKWPUBLIC !  LBL LKWPRIVATE !  LBL LKWENDPACKAGE !
   LBL LKWDUPDEF !
   LBL LCHKPACKAGE !  LBL LCHKPUB !  LBL LCHKPRI !  LBL LCHKENDPKG !
   LBL LCHKDEFER !  LBL LRESTAB !  LBL LRECWPUB !  LBL LP2DOESW !
   LBL LKWEXPORT !  LBL LCHKEXPORT !
   LBL LKWQUOT !  LBL LKWSEMIQ !  LBL LKWDEFER !  LBL LKWIS !  LBL LKWDEFERUNSET !
   LBL LSIGPTRA !  LBL LSIGA !
   LBL LKWCONSTRUCT !  LBL LKWMATCH !  LBL LKWSEMIMATCH !
   LBL LTFLCONFAM !  LBL LTFLCVAR !
   LBL LTFLMATCHFAM !  LBL LTFLNAME !  LBL LBADTAGPFX !  LBL LBADTAGSFX ! ;

: EMIT-LABEL-RUNTIME ( -- )
   LBL LBCHAIN !  LBL LCREATE !  LBL LDOESPATCH !
   LBL LREAD !  LBL LRBYE !  LBL LRDIE !  LBL LRREC !  LBL LQNL !  LBL LOKS !
   LBL LEX0 !  LBL LUN0 !  LBL LEVALREC !
   LBL LCRASHH !  LBL LHEX !  LBL LHDR !  LBL LTRAPH !  LBL LBPH !  LBL LBPSH !  LBL LBPWH !  LBL LBADLOC !
   LBL LSRCRD !  LBL LSHBANG !  LBL LOPENERR !  LBL LOPENNL !
   LBL LUNCAUGHT !  LBL LUNCMSG !
   LBL LWIDE !  LBL LWIDEMSG !  LBL LDIAGRET !
   LBL LINTERNAL !  LBL LINTMSG !
   LBL LDICTFULL !  LBL LCODEFULL !
   LBL LSNAPBAD !  LBL LSNAPVER !
   LBL LSRCFULL !  LBL LSRCREAD !  LBL LBADSTR !
   LBL LPROTPUB !  LBL LPROTAOT !
   LBL LFLAGMATCH !  LBL LSRCBADFLAG !  LBL LFLAGTAB !
   LBL LBADFLAG !  LBL LUSAGE1 !  LBL LUSAGE2 !  LBL LSPC ! ;

: EMIT-LABEL-SOURCES ( -- )
   LBL LPLINUXTARGET !  LBL LPMACOSTARGET !
   LBL LPLINUXLAYOUT !  LBL LPMACOSLAYOUT !
   LBL LPUTIL !  LBL LPSTRUCTURES !  LBL LPBYTES !  LBL LPCHECKER !  LBL LPLOWERCERTBASE !  LBL LPRENDER !  LBL LPHOOK !
   LBL LPTYPESCHEMA !  LBL LPTYPEFAM !  LBL LPSUMTYPE !  LBL LPLAYOUTBUF !  LBL LPLAYOUTVALID !
   LBL LPSTRUCTEFF !  LBL LPHABULAYOUT !
   LBL LPENVBASE !  LBL LPINCLUDE !  LBL LPSCRIPTARGV !  LBL LPINTMARK !  LBL LPROLES !
   LBL LPENUMS !  LBL LPEXECVECTOR !  LBL LPSHA256 !  LBL LPTFAMSHA !
   LBL LPCOMBINATORS !  LBL LPXREF !  LBL LPLAYOUTSEAL !  LBL LPLOWERCERTSEAL ! ;

: EMIT-LABEL-JIT ( -- )
   LBL LPROFH !  LBL LPROFDUMP !
   LBL LVSPILL !  LBL LVLITPUSH !  LBL LVPUSHC !
   LBL LVTOP2C !  LBL LVFOLDPUT !
   LBL LVRALLOC !  LBL LVBIT !  LBL LVRINIT !  LBL LVMOVK !  LBL LVFORCEK !  LBL LVBINPREP !  LBL LVBINIPREP !  LBL LVPUSHR !
   LBL LVPUSHF !  LBL LFRALLOC !  LBL LFFORCEK !  LBL LFBINPREP !
   LBL LKWFPLUS !  LBL LKWFMINUS !  LBL LKWFSTAR !  LBL LKWFSLASH !
   LBL LVDROP !  LBL LVSWAPX !  LBL LVNIPX !  LBL LVCOPY !
   LBL LVSNAP !  LBL LVRECON ! ;

: EMIT-LABEL-OPS ( -- )
   LBL LKWPLUS !  LBL LKWMINUS !  LBL LKWSTAR !
   LBL LKWAND2 !  LBL LKWOR2 !  LBL LKWXOR2 !
   LBL LKWDUP2 !  LBL LKWDROP2 !  LBL LKWSWAP2 !
   LBL LKWOVER2 !  LBL LKWNIP2 !
   LBL LKWEQ2 !  LBL LKWNE2 !  LBL LKWLT2 !
   LBL LKWGT2 !  LBL LKWLE2 !  LBL LKWGE2 !
   LBL LKWINC !  LBL LKWDEC !  LBL LKWZEQ !
   LBL LKWZLT !  LBL LKWNEG2 !  LBL LKWINV2 ! ;

: EMIT-LABEL-P2 ( -- )
   LBL LCERTBYTES !  LBL LP2CWAT !  LBL LP2CDESC !
   LBL LKWTUCK3 !  LBL LKWROT3 !  LBL LKWMROT3 !
   LBL LKW2DUP3 !  LBL LKW2DROP3 !  LBL LKW2SWAP3 !  LBL LKW2OVER3 !
   LBL LKW2TOR3 !  LBL LKW2RFROM3 !  LBL LKW2RFET3 !
   LBL LKWAT2 !  LBL LKWSTORE2 !
   LBL LP2COPY !  LBL LP2DROPN !  LBL LP2REV !  LBL LP2ROT !  LBL LP2RS !
   LBL LP2FETCH !  LBL LP2STORE !  LBL LP2VEXEC !  LBL LP2VEMIT !  LBL LVPBADMSG !  LBL LP2NEST !
   LBL LOWER-TXN-CODE:BAD !  LBL LOWER-TXN-CODE:MEM !
   LBL LOWER-TXN-CODE:FULL !  LBL LOWER-TXN-CODE:DRIFT !
   LBL LOWER-TXN-CODE:VDESC !  LBL LOWER-TXN-CODE:DRIFT-FAIL ! ;

: EMIT-LABELS ( -- )
   EMIT-LABEL-CORE
   EMIT-LABEL-CONTROL
   EMIT-LABEL-RUNTIME
   EMIT-LABEL-SOURCES
   EMIT-LABEL-JIT
   EMIT-LABEL-OPS
   EMIT-LABEL-P2 ;

\ ---- AOT M2: N-word capture buffers (host-only build scratch; `allot` DATA, NOT
\ baked into bin/hb). aot-capture.f fills them from the metabuild host's compiled
\ words; EMIT-AOT-SEED bakes blob + N dict records + a call-site relocation table
\ (site blob-offset -> callee dict NAME); EM-SEED-AOT copies/registers/relocates at
\ boot. Site rows and dict records are stored as cells here for @/! access; the bake
\ packs sites to u32 triples.
$10000 constant AOT-BLOB-CAP
create AOT-BLOB-BUF AOT-BLOB-CAP allot    variable AOT-BLOB-LEN
256 constant AOT-REC-MAX
\ AOT-REC-BUF holds three regions (all viewed via AOT-REC-BUF@, no extra TRUST):
\   [0 .. MAX*48)          verbatim 48B dict records (capture source of truth)
\   [MAX*48 .. +MAX*12)    compact 12B records (baked; blob-off u16 + end u16 + name-off u16 + flags u8 + pad u8 + wid u32)
\   [+MAX*12 .. +48)       48B scratch for the build-time expand==verbatim proof
create AOT-REC-BUF AOT-REC-MAX 48 * AOT-REC-MAX 12 * + 48 + allot    variable AOT-REC-N
2048 constant AOT-SITE-MAX
create AOT-SITE-BUF AOT-SITE-MAX 4 * allot    variable AOT-SITE-N   \ packed 4B rows: blob-off u16 + name-off u16
$4000 constant AOT-NAMES-CAP
create AOT-NAMES-BUF AOT-NAMES-CAP allot    variable AOT-NAMES-LEN
\ DATA-literal relocation table (third relocation class): blob offsets of the
\ movz/movk x9 DATA-address literals (create/variable buffer refs). AOT-DATA-D0 =
\ the capture engine's REPL-DATA base (abs); AOT-DATA-SIZE = the REPL DATA span
\ (all allot/variable => zero content). EM-SEED-AOT reserves DATA and rebases each
\ literal by (seed-DP - AOT-DATA-D0).
512 constant AOT-DSITE-MAX
create AOT-DSITE-BUF AOT-DSITE-MAX 2 * allot    variable AOT-DSITE-N   \ packed u16 blob offsets (DATA then CODE)
variable AOT-DATA-D0    variable AOT-DATA-SIZE
\ CODE-literal relocation table (fourth relocation class): blob offsets of the
\ movz/movk x9 literals whose value lands in the captured code range [B0,B1) --
\ anonymous quotation-body entry addresses (J-SEMIQUOT `C-LIT QENT`). Rebased by
\ the code delta (seedCP - captureB0); no name (quotations are anonymous). Stored in
\ the DATA-site buffer right after the AOT-DSITE-N DATA offsets (one fewer scratch
\ view), and baked as its own contiguous LAOTCSITES section.
variable AOT-CSITE-N
variable AOT-CODE-B0
\ boot-run name list: 0-terminated [len][name-bytes] records of the top-level entry
\ words (INSTALL/BPW-INSTALL/S-INSTALL) the metabuild ran at the tail of the REPL
\ source. With the source dropped, EM-SEED-AOT LFINDs + calls each after RX/flush so
\ the seeded engine installs the REPL with no embedded source.
$400 constant AOT-BOOTRUN-CAP
create AOT-BOOTRUN-BUF AOT-BOOTRUN-CAP allot    variable AOT-BOOTRUN-LEN

\ protected-WID registry AOT image (TFAM 2b-v): the u32 WIDs of sealed system /
\ generated constructor packages, captured from the live friend-arena registry and
\ baked so EM-AOT-REGISTER-PROT-WIDS can restore them at boot -- advancing WIDN past
\ each so a post-restore wordlist alloc cannot collide with a restored protected WID.
\ u32 entries (matching the registry's checked u32 domain) so wordlist IDs above 255
\ round-trip through the seed with no u8 truncation. Capacity = PROT-WID-MAX so a full
\ registry always fits.
PROT-WID-MAX constant AOT-PWID-MAX
create AOT-PWID-BUF AOT-PWID-MAX 4 * allot    variable AOT-PWID-N

\ Raw emitter-boundary views (same pattern as SRCA@): expose the build-scratch
\ buffers as `ptr` for the checked copy/BYTES, sites below.
: AOT-BLOB-BUF@ ( -- ptr u8 ) AOT-BLOB-BUF ;
s" AOT-BLOB-BUF@" s" -- ptr u8" TRUST
: AOT-REC-BUF@ ( -- ptr a ) AOT-REC-BUF ;
s" AOT-REC-BUF@" s" -- ptr a" TRUST
: AOT-SITE-BUF@ ( -- ptr u8 ) AOT-SITE-BUF ;
s" AOT-SITE-BUF@" s" -- ptr u8" TRUST
: AOT-NAMES-BUF@ ( -- ptr u8 ) AOT-NAMES-BUF ;
s" AOT-NAMES-BUF@" s" -- ptr u8" TRUST
: AOT-DSITE-BUF@ ( -- ptr u8 ) AOT-DSITE-BUF ;
s" AOT-DSITE-BUF@" s" -- ptr u8" TRUST
: AOT-BOOTRUN-BUF@ ( -- ptr u8 ) AOT-BOOTRUN-BUF ;
s" AOT-BOOTRUN-BUF@" s" -- ptr u8" TRUST
: AOT-PWID-BUF@ ( -- ptr u8 ) AOT-PWID-BUF ;
s" AOT-PWID-BUF@" s" -- ptr u8" TRUST

\ Bake the AOT section: blob length + blob, record count + N 48-byte dict records
\ (xt/end blob-relative, inline name), site count + M u32 triples (blob-off,
\ name-off, name-len), then the name pool. Placed last so it never shifts engine
\ offsets. LAOTNREC = 0 makes EM-SEED-AOT skip the whole pass (stage2/maker/snap).
: EMIT-AOT-SITES ( -- )   \ packed 4B rows (blob-off u16 + name-off u16)
   AOT-SITE-N @ 0 > IF AOT-SITE-BUF@ AOT-SITE-N @ 4 * BYTES, THEN ;
: EMIT-AOT-DSITES ( -- )   \ packed u16 DATA-site offsets
   AOT-DSITE-N @ 0 > IF AOT-DSITE-BUF@ AOT-DSITE-N @ 2 * BYTES, THEN ;
: EMIT-AOT-CSITES ( -- )   \ packed u16 CODE-site offsets (after the AOT-DSITE-N DATA u16s)
   AOT-CSITE-N @ 0 > IF AOT-DSITE-BUF@ AOT-DSITE-N @ 2 * + AOT-CSITE-N @ 2 * BYTES, THEN ;
: EMIT-AOT-SEED ( -- )
   LAOTCODELEN LABEL@ LBL,  AOT-BLOB-LEN @ DCQ,
   LAOTCODE LABEL@ LBL,
   AOT-BLOB-LEN @ 0 > IF AOT-BLOB-BUF@ AOT-BLOB-LEN @ BYTES, THEN
   LAOTNREC LABEL@ LBL,  AOT-REC-N @ DCQ,
   LAOTDICT LABEL@ LBL,                          \ compact 12B records (EM-AOT-REGISTER-RECS expands to 48B)
   AOT-REC-N @ 0 > IF AOT-REC-BUF@ AOT-REC-MAX 48 * + AOT-REC-N @ 12 * BYTES, THEN
   LAOTNSITE LABEL@ LBL,  AOT-SITE-N @ DCQ,
   LAOTSITES LABEL@ LBL,  EMIT-AOT-SITES
   LAOTNAMES LABEL@ LBL,
   AOT-NAMES-LEN @ 0 > IF AOT-NAMES-BUF@ AOT-NAMES-LEN @ BYTES, THEN
   LAOTDATASIZE LABEL@ LBL,  AOT-DATA-SIZE @ DCQ,
   LAOTDATAD0 LABEL@ LBL,  AOT-DATA-D0 @ DCQ,
   LAOTNDSITE LABEL@ LBL,  AOT-DSITE-N @ DCQ,
   LAOTDSITES LABEL@ LBL,  EMIT-AOT-DSITES
   LAOTCODEB0 LABEL@ LBL,  AOT-CODE-B0 @ DCQ,
   LAOTNCSITE LABEL@ LBL,  AOT-CSITE-N @ DCQ,
   LAOTCSITES LABEL@ LBL,  EMIT-AOT-CSITES
   LAOTBOOTRUN LABEL@ LBL,  AOT-BOOTRUN-BUF@ AOT-BOOTRUN-LEN @ 1 + BYTES,   \ +1 = live 0 terminator
   LAOTNPWID LABEL@ LBL,  AOT-PWID-N @ DCQ,                                  \ protected-WID registry: count
   LAOTPWID LABEL@ LBL,                                                      \ then N u32 WIDs (TFAM 2b-v)
   AOT-PWID-N @ 0 > IF AOT-PWID-BUF@ AOT-PWID-N @ 4 * BYTES, THEN ;

: EMIT-PRIMITIVE-SECTIONS ( -- )
   EMIT-PRIMS
   EMIT-ARITY-GUARD             \ bake LARITY after prims so guarded entry labels resolve
   s" snap-rebase" ['] BSNAPREBASE FPRIM
   EMIT-PROF-PRIMS
   EMIT-FP-PRIMS
   EMIT-CEMIT
   EMIT-BCAP
   EMIT-TOK
   EMIT-PROT
   EMIT-PROTWID
   EMIT-FLUSH
   EMIT-FIND
   EMIT-HIDX
   EMIT-NUM ;

: EMIT-DICTIONARY-SECTIONS ( -- )
   EMIT-CREATE
   EMIT-DOESPATCH
   EMIT-CF-HELPERS  EMIT-ESC-DECODE  EMIT-ESC-SCAN  EMIT-ESC-COPY
   EM-SNAPSHOT-REBASE-DICT  EM-SNAPSHOT-REBASE-CALLS  EM-AOTWIDGATE
   EMIT-LOC-FIND
   EMIT-KWDATA
   EMIT-FOLDKW
   EMIT-SHUFKW
   EMIT-CMPKW
   EMIT-UNKW
   EMIT-P2KW ;

: EMIT-RUNTIME-SECTIONS ( -- )
   EMIT-CRASH-HANDLER
   EMIT-TRAPH
   EMIT-HEX
   EMIT-PROFDUMP
   EMIT-PROF
   EMIT-SHEBANG-COMMENT
   EMIT-SOURCE-READ
   EMIT-FLAGS
   EMIT-JIT
   EMIT-P2-HELPERS
   LOWER-TXN:EMIT-VALIDATOR ;

: EMIT-CODE-SECTIONS ( -- )
   EMIT-MAIN
   EMIT-PRIMITIVE-SECTIONS
   EMIT-DICTIONARY-SECTIONS
   EMIT-RUNTIME-SECTIONS
   EMIT-DICT
   EMIT-AOT-SEED ;

: EMIT-SOURCE-BYTES ( -- )
   LSRC LABEL@ LBL,  SRCA@ SRCN @ BYTES, ;

: EMIT-FORTH ( ptr u8 n -- )
   EMIT-RESET-BUILDER
   EMIT-LABELS
   EMIT-CODE-SECTIONS
   EMIT-SOURCE-BYTES ;
s" emit-forth" s" ptr u8 n --" TRUST

package ENGINE-BUILD
public
\ An escaped emitter throw is caught only by the internal driver, which exits;
\ the process-local build flag is therefore never observed after that edge.
: BUILD ( ptr u8 n -- )
   ARM
   EMIT-FORTH
   DISARM
   ;
end-package

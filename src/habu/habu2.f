\ habu2.f — engine-builder part 2: the JIT compiler
\ emitters (literal/call/keywords/locals/strings/do-loop), the outer-interpreter
\ main loop, and ENGINE-EMIT:FORTH. Needs habu1.f (part 1). EMIT-MAIN is split into
\ phase words sharing label VARIABLES (a giant single word would need dozens of
\ locals); emission order is stable so the self-rebuild reaches a fixpoint.
\ The ARM64 encoders are package A64ASM's public surface (src/arch/arm64/asm.f).
using A64ASM
\ The AOT capture buffers, their caps and the section budget are src/habu/aot-decl.f,
\ loaded immediately before this file so a capture running in bin/hb and this
\ emitter share one declaration of the format. The import spans the file - the
\ seed emitters at the end read the buffers, the relocation emitters near the top
\ read the window - and closes beside the A64ASM one.
using AOT-BUF

\ ---- literal emitters: scalars vs relocatable addresses ---------------------------
\ A relocatable address must never be emitted through the scalar path. Scalars use the
\ shared minimal MOVZ/MOVN+MOVK synthesizer (LVMOVK, via LVLITPUSH) and materialize into
\ x16 -- never the fixed four-instruction x9 chain the AOT relocation recognises -- so a
\ scalar whose value numerically lands inside a DATA/CODE address range can never be
\ mistaken for an address. DATA/CODE addresses keep the fixed four-instruction x9 chain
\ (constant width preserves the boot-reloc patch space) and flow only through the
\ dedicated C-DATA-ADDR* / C-CODE-ADDR words, which name the relocation kind at the site.

\ scalar-push: minimal chain + push (LVLITPUSH synthesizes x16, then pushes it).
: C-LIT ( -- )
   LVLITPUSH LABEL@ BL, ;
\ raw scalar into x16 (no push). x11 holds the value; LVMOVK emits the minimal chain.
: C-RAW-LIT ( -- )
   14 16 MOVZ,  LVMOVK LABEL@ BL, ;

\ fixed four-instruction MOVZ/MOVK x9 chain (no push): the relocatable-address form the
\ AOT boot pass rewrites in place. `val` is in x11 at emit time.
: C-ADDR-RAW ( -- )
   6 11 0 ADDI,  5 $FFFF MOVZ,
   7 6 5 AND,    7 7 5 LSLI,  8 W-MOVZ0 LIT64,  9 8 7 ORR,  LCEMIT LABEL@ BL,
   7 6 16 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK1 LIT64,  9 8 7 ORR,  LCEMIT LABEL@ BL,
   7 6 32 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK2 LIT64,  9 8 7 ORR,  LCEMIT LABEL@ BL,
   7 6 48 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK3 LIT64,  9 8 7 ORR,  LCEMIT LABEL@ BL, ;
\ the same fixed chain followed by the push stencil.
: C-ADDR-PUSH ( -- )
   C-ADDR-RAW
   9 W-PUSH0 LIT64,  LCEMIT LABEL@ BL,  9 W-PUSH1 LIT64,  LCEMIT LABEL@ BL, ;
\ The three words that NAME the relocation kind of a chain, and the compile-mode
\ CALL-or-INLINE emitter, are defined further down, right after the snapshot-
\ relocation labels. All four record a site through SNAP-RELOC:MARK-SITE: the
\ three because they are creating a chain, the inliner because it is copying one.
\ ---- addressing a baked label the ADR field cannot reach -------------------
\ ADR, reaches ADR-HI (1 MiB) from its own site. That is the whole allowance of
\ the ENGINE code part, and the ADR, that reaches LSRC from the startup region is
\ what MEASURES it -- so those two sites keep using ADR, on purpose and must not
\ be converted; converting them would leave the window's first term with nothing
\ enforcing it. What lies past LSRC is a different matter: the AOT payload
\ section runs to its own buffer caps, megabytes once the compiler chain is
\ captured, and a reader of it must not use ADR, however close a particular label
\ happens to land today. The rule is the SECTION, not the distance -- see the
\ three-part note in src/arch/arm64/icode.f, which is where the window is derived,
\ and tools/aot-section-reach-lint.f, which refuses an ADR, into the section.
\
\ WHERE THE BASE COMES FROM, AND WHY IT IS NOT A REGISTER. EM-STARTUP takes the
\ runtime text base into XREG-RBASE with an ADR, planted at the image's first
\ instruction, then EM-DATA-INIT stores it into RBASE-CELL and repurposes that
\ same register (layout.f names x20 both XREG-RBASE and DATA) for the DATA base.
\ RBASE-CELL is therefore the live text CONTENT base for the whole rest of the
\ boot, which is exactly what EM-SNAPSHOT-RESTORE and snap-lib.f SNAP:WRITE-BYTES
\ already read it as. This word is valid only AFTER EM-DATA-INIT; a site before
\ it still holds the base in XREG-RBASE and keeps using ADR,.
\
\ rt is a scratch register the caller names, because there is no free one to
\ assume at these sites; it is clobbered. Naming the same register twice would
\ load the base over the offset and then double the base - a wrong address in a
\ correctly assembled image, which is the worst shape a fault can take - so the
\ pair is refused at emit time instead of being asked for in a comment. The
\ refusal is reachable by a caller's typo and by nothing else, so its evidence is
\ a mutation: passing `9 9 LAOTCODE LABEL@ TADR,` stops the build with this
\ message rather than producing an engine.
\ LIMGEND belongs here because it is the same subject from the other end: the
\ label bound past every baked part, which only this form can address.
package IMGREF

public

variable LIMGEND                                  \ the emitted image's end = the cold text CONTENT length

: TADR, ( n n label -- )                          \ ( rd rt label -- ) rd = runtime address of label
   {: rd:n rt:n l:label :}
   rd rt = if s" habu2: TADR, destination and scratch are one register" ICODE-EXIT-RC die then
   rd l LOFF,                                     \ rd = label's byte offset from the text base
   rt DATA RBASE-CELL LDR,                        \ rt = this boot's text CONTENT base
   rd rt rd ADD, ;

;package

\ The emitter calls both by their bare names, so the import spans the file and
\ closes beside the A64ASM one at the end.
using IMGREF

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
variable LMININ   variable LMINMSG   \ interpret-mode certified-word underdepth reject (DNAME-MIN-IN) + its message
32 constant MINMSG-LEN    \ byte length of "hb: interpret stack underdepth: " (LMINMSG)
variable LPREFMISS   variable LPREFMISSMSG   \ armed checker without its compile-immediate preflight
35 constant PREFMISSMSG-LEN   \ byte length of "hb: compile preflight hook missing\n"
variable LORPHAN   variable LORPHANMSG   \ orphan control-flow closer reject (empty CFSTK pop) + its message
40 constant ORPHANMSG-LEN  \ byte length of "hb: control-flow closer without opener: " (LORPHANMSG)
variable LCFCAP   variable LCFCAPMSG   \ control-flow stack overflow reject (LCFPUSH at CFSTK-DEPTH-MAX) + its message
35 constant CFCAPMSG-LEN   \ byte length of "hb: control-flow nesting too deep: " (LCFCAPMSG)
variable LCSTR   variable LCSTRMSG   \ counted-string >255 reject (C-ICQ/C-EICQ/C-CQ/C-ECQ) + its fd-2 message (dot habu-recovery-pkg-scope-e0bd98e2)
37 constant CSTRMSG-LEN    \ byte length of "hb: counted string too long (max 255)" (LCSTRMSG); previously a bare silent 76 shared with C-SIG-BAD
variable LDPBADMSG   \ DP-heap bound reject (habu1.f DP-CHECK, allot/,/c,/definer sinks) fd-2 message (dot habu-dictionary-allot-past-4e5c3c2b); label LDPBAD declared in habu1.f forward-ref block
27 constant DPBADMSG-LEN    \ byte length of "hb: data space out of range" (LDPBADMSG); LDPBAD appends a newline. Was a bare silent exit_group(76)
variable LCOMPILEDIE   \ shared recoverable compile-error tail (dot habu-raw-exit-compile): a die site that already wrote its diagnostic branches here with x0 = its sysexits exit code. Inside evaluate (EVALD>0) the aborted compile unwinds as a catchable throw of that SAME code via LEVALREC (RSP/CP/NDICT/XDS/DP + compile-state rollback, HIDX tolerates the stale records); at top level (EVALD==0) it exit_group(x0) byte-identically to the old raw exit.
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
\ Snapshot relocation: the labels, the message lengths and the one instruction
\ constant its two relocation passes need. The band offsets and exit statuses of
\ the same subsystem are declared in src/habu/layout.f, which opens this package
\ first; src/habu/snap-lib.f reopens it again for the writer's half.
package SNAP-RELOC
public
variable LCALLMSG   \ a recorded call site does not hold a call instruction (CALLMAP-RC)
31 constant CALLMSG-LEN   \ byte length of "hb: snapshot call map mismatch\n" (LCALLMSG)
variable LXTMSG     \ more address cells were declared than the table can hold (XTCELL-RC)
32 constant XTMSG-LEN     \ byte length of "hb: snapshot address table full\n" (LXTMSG)
variable LADDRMSG   \ a recorded address-literal site does not hold the MOVZ/MOVK chain (ADDRMAP-RC)
34 constant ADDRMSG-LEN   \ byte length of "hb: snapshot address map mismatch\n" (LADDRMSG)
variable LXTBANDMSG \ a declared address cell is not a cell-aligned address inside DATA (XTBAND-RC)
39 constant XTBANDMSG-LEN \ byte length of "hb: snapshot address cell out of range\n" (LXTBANDMSG)
variable LCALLS     \ region-to-text call relocation routine (snapshot write + restore)
variable LXT        \ declared-address-cell relocation routine (snapshot restore)
variable LMARK      \ declare one DATA cell as holding a region address
variable LADDRS     \ address-literal relocation routine (snapshot write + restore)
variable LADDRSITE  \ record the chain about to be emitted at CP as an address literal
\ The six opcode bits of an AArch64 BL, i.e. $94000000 >> 26. The call relocation
\ pass reads them with one shift rather than a mask-and-compare against a 64-bit
\ literal.
$25 constant BL-OP-HI

\ Emit "declare the engine cell at OFFSET as holding a region address". The three
\ engine hook cells are named this way at cold boot; the dispatch cell of a
\ deferred word is declared from a register instead, because its address is only
\ known while `defer` or `is` is running. Defined here beside the labels so the
\ compile handlers further down this file can already use it.
: MARK-CELL ( n -- ) {: cell:n :}
   9 cell LIT64,  9 DATA 9 ADD,  LMARK LABEL@ BL, ;

\ Emit "the four-instruction chain that starts at the current CP is an address of
\ code". The recorder reads CP itself, so the compile handler has nothing to pass
\ and the site can never be off by an instruction.
: MARK-SITE ( -- )
   LADDRSITE LABEL@ BL, ;

\ Carry one copied word's address-literal record across with its bytes, for the
\ compile-mode inliner's copy loop below - the one thing in the engine that
\ REPRODUCES a chain instead of creating one.
\ AN INLINED BODY IS A SECOND COPY OF ITS CODE, AND ONLY ITS BYTES USED TO TRAVEL.
\ This band is keyed by region offset, so the record a chain got when it was
\ created describes the ONE place it was created; the copy lands somewhere else, at
\ an offset nothing has recorded. The chain's VALUE needs no fixing - an absolute
\ address means the same thing wherever the four words sit - so the record is the
\ only thing that has to be reissued. The caller has the word it is about to copy
\ in x15; if that word carries the bit, MARK-SITE marks the word at CP, which is
\ exactly where the copy is about to land.
\ It must run BEFORE the caller loads and emits the word, because MARK-SITE
\ clobbers x9 - the register the copy carries the word in - and because emitting
\ advances CP past the destination.
\ A chain is the only relocation-meaningful thing an inlined body can carry: the
\ inliner's safety scan has already refused every BL, B, B.cond, CBZ, TBZ, BLR, BR,
\ RET and ADR, so no call site and no PC-relative material ever reaches here. That
\ same scan now also refuses a body whose chain an OPEN capture window could not
\ describe (AOT-WINDOW:EMIT-OUTSIDE below), so a record reissued here is one the
\ capture in progress can classify.
\ THE BAND IS ASKED ONLY INSIDE ITS OWN DOMAIN, and that is the region's extent
\ rather than a guess about a value, exactly as in EMIT-CEMITBL. It covers the JIT
\ region and nothing else, while the body being inlined is very often in the
\ engine's loaded __text - `dup` and `drop` both live there and both inline - whose
\ region offset is a large unsigned number. Nothing is lost by skipping those:
\ every writer of this band records at CP, so a recorded word is a region word by
\ construction.
\ Clobbers x5, x6, x9 and x10, all dead at the inliner's copy step; x13, x14 and
\ x15 carry the copy and are untouched here and by MARK-SITE.
: CARRY-SITE ( -- )
   LBL {: lnosite:label :}
   6 15 DBASE SUB,  5 REGION LIT64,  6 5 CMP,          \ source word inside the band's domain?
   C-CS lnosite BCOND,                                 \ no: engine __text carries no records
   9 6 0 ADDI,  9 9 2 LSRI,  9 9 7 ANDI,               \ x9 = bit number = word index & 7
   6 6 5 LSRI,                                         \ x6 = map byte index = offset >> 5
   5 ADDRMAP-OFF LIT64,  6 6 5 ADD,  6 DATA 6 ADD,     \ x6 = map byte address
   10 6 0 LDRB,  10 10 9 LSRV,  10 10 1 ANDI,          \ x10 = this word's recorded bit
   10 lnosite CBZ,
      MARK-SITE                                        \ records the destination word, which is CP
   lnosite LBL, ;

;package

\ ---- the three words that name a chain's relocation kind ---------------------
\ Same instruction shape for all three; what differs is what the address means,
\ which is why the kind is decided here, at the emit site, and never later by
\ looking at the bytes.
\
\ ALL THREE RECORD THEIR SITE, AND THE MAP ANSWERS ONE QUESTION: WHERE A CHAIN
\ STARTS. It used to answer only for code addresses, and the DATA ones were left
\ out on the argument that DATA is mapped at a fixed address in every run, so a
\ persisted DATA literal is already correct in the run that restores it. That
\ argument is TRUE and it was an argument about the wrong consumer. A snapshot
\ restore does not have to move a DATA chain; an AOT CAPTURE has to FIND one -
\ the captured blob is copied to a different DP in the seeded engine, so the
\ address a chain-compiled word holds is the metabuild host's and is wrong there.
\ Capture used to find them by scanning the blob for the chain's shape and
\ testing the value against the window's DATA span, which is a value heuristic:
\ an ordinary integer may hold any value at all, and a compiled word may carry
\ inline data that decodes as a move-wide chain. So the record is written where
\ the kind is KNOWN, which is here, and the two consumers read the one map.
\
\ AND WHY ONE BAND STILL SERVES BOTH KINDS. SNAP-RELOC:EMIT-ADDRS is called ONCE
\ PER BAND with the band it is moving (x21 = base, x22 = length) and rewrites a
\ chain only when the address it spells out lies inside that band; a chain naming
\ neither band is left alone rather than guessed at. Its two calls are the live
\ JIT region and the engine's loaded __text. A DATA address is in neither, in
\ every run and at both ends of a snapshot: DATA is mapped MAP_FIXED at DATA-VA
\ ($340000000 on Linux, $44000000000 on macOS), the writer canonicalises the
\ region to the RBASE-VA sentinel ($300000000, span REGION = $800000, so the band
\ ends at $300800000) and the loader maps the sentinel onto the live region base,
\ which sits REGION-OFF above __text. Every one of those bands is far below
\ DATA-VA and none of them can reach it. So a DATA site recorded here is VISITED
\ by both calls and rewritten by NEITHER - which is exactly right, because DATA
\ does not move - and what the record buys is a capture that no longer has to
\ recognise a chain by what it contains.
\
\ AND THE BAND DOES NOT SAY WHICH KIND A SITE IS, because nothing has to store
\ that. A consumer already holds the two spans of the window it is working on -
\ the AOT capture takes them as arguments - and a recorded site's value lands in
\ exactly one of them: the DATA span sits inside the region mapped at DATA-VA and
\ the code span inside the JIT region, which are disjoint by construction. So the
\ kind is DERIVED from the value of a site already known to be a real address,
\ which is a total classification and not a recognition - the guess the old
\ value-range scans made was about WHETHER a word was a site at all, and that
\ question is now answered by this bit and by nothing else. A second band, or a
\ per-window kind list, would have cost storage to answer a question the spans
\ already answer.
\ push a DATA-region address (create/variable data field).
: C-DATA-ADDR ( -- )
   SNAP-RELOC:MARK-SITE
   C-ADDR-PUSH ;
\ raw DATA-region address into x9, no push (the defer dispatch-cell address).
: C-DATA-ADDR-RAW ( -- )
   SNAP-RELOC:MARK-SITE
   C-ADDR-RAW ;
\ push a CODE address (quotation entry xt, ['] / postpone target xt). The word it
\ names lives in the JIT region or in the engine's loaded __text, and neither of
\ those is at the same address in the run that restores a snapshot image, so the
\ relocation pass rewrites this one where it leaves a DATA chain alone.
: C-CODE-ADDR ( -- )
   SNAP-RELOC:MARK-SITE
   C-ADDR-PUSH ;

\ ---- compile-mode CALL-or-INLINE (x11=target addr, x12=clen from FIND) ----
\ LOUTSIDE: the routine the safety scan below asks about one candidate body word -
\ is the address chain this word starts outside the capture window the metabuild
\ has open? It belongs to the window, so it joins the package src/habu/layout.f
\ opens for the window's two cells; its body is emitted once, further down, beside
\ the other AOT-pass routines. The label is declared HERE because the scan that
\ calls it is compiled from this block.
package AOT-WINDOW
public
variable LOUTSIDE
;package
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
$D2800010 constant C-CALL-MOVZ-X16    \ movz x16 scaffold: ADT-match tag compare (not a call)
$D63F0200 constant C-CALL-BLR-X16     \ blr x16: kept for the deferred-word indirect call (C-DEFER-EMIT-CODE)

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

\ The two rejections the scan makes, in the order their inputs die. Both take the
\ lcall exit, which is the universal fallback: the target is a defined word with
\ its own `ret`, so calling it and copying it are the same answer in two encodings,
\ and every rejection here picks the encoding that stays correct.
\ x15 is the word's ADDRESS and is what LOUTSIDE reads, so the cursor advances
\ after both rejections rather than between the load and them.
: C-CALL-SCAN-SAFE ( label label label -- ) {: lcopy:label lcall:label lsbody:label :}
   15 13 0 ADDI,
   lsbody LBL,  15 14 CMP,  C-GE lcopy BCOND,
      9 15 0 LDRW,
      lcall C-CALL-REJECT-UNSAFE
      AOT-WINDOW:LOUTSIDE LABEL@ BL,  10 lcall CBNZ,   \ a chain the open capture window cannot describe
      15 15 4 ADDI,
      lsbody B, ;

: C-CALL-COPY-INLINE ( label label -- ) {: linl:label ldone:label :}
   15 13 0 ADDI,
   linl LBL,  15 14 CMP,  C-GE ldone BCOND,
      SNAP-RELOC:CARRY-SITE
      9 15 0 LDRW,  15 15 4 ADDI,  LCEMIT LABEL@ BL,  linl B, ;

: C-CALL ( -- )
   LBL LBL LBL LBL LBL LBL LBL
   {: lcall:label lcopy:label lscan:label lsbody:label lnopro:label
      linl:label ldone:label :}
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
      LCEMITBL LABEL@ BL,       \ one direct BL imm26 to the statically known target in x11
   ldone LBL, ;

variable LSRCFULL   variable LSRCREAD   variable LBADSTR   \ boot source labeled rc-74 exits (prefix overflow / read error / string literal)
30 constant SRCFULL-MSG-LEN   \ byte length of "hb: source prefix buffer full\n" (LSRCFULL; SRC-SFAIL/SRC-BFAIL IBUFSZ overflow)
23 constant SRCREAD-MSG-LEN   \ byte length of "hb: cannot read source\n" (LSRCREAD; source read syscall error)
23 constant BADSTR-MSG-LEN    \ byte length of "hb: bad string literal\n" (LBADSTR; unterminated or bad-escape string literal)
variable LPROTPUB   variable LPROTAOT   \ protected-WID seal labeled rc-84 exits (publish-into-protected / AOT boot-pass gate reject)
40 constant PROTPUB-MSG-LEN   \ byte length of "hb: cannot publish into protected word: " (LPROTPUB; C-STORE-DEF-NAME appends the def name + newline)
35 constant PROTAOT-MSG-LEN   \ byte length of "hb: AOT protected-WID gate reject: " (LPROTAOT; EM-AOTWIDGATE appends the callee name + newline)
variable LMMAPCODE   variable LMMAPDATA   \ region mmap-fail/collision labeled rc-78 exits (code region / data region); message bytes live in the loaded __text image, so the write is valid even though the region being mapped does not exist yet
33 constant MMAPCODE-MSG-LEN   \ byte length of "hb: cannot map fixed code region\n" (LMMAPCODE)
33 constant MMAPDATA-MSG-LEN   \ byte length of "hb: cannot map fixed data region\n" (LMMAPDATA)
variable LBLRANGE   \ boot BL-range assertion labeled rc-81 exit: the mapped region fell outside BL's +/-128 MiB of __text
32 constant BLRANGE-MSG-LEN   \ byte length of "hb: code region out of BL range\n" (LBLRANGE)
variable LFLAGMATCH  variable LSRCBADFLAG  variable LFLAGTAB
variable LBADFLAG    variable LUSAGE1      variable LUSAGE2     variable LSPC
variable LPLINUXTARGET  variable LPMACOSTARGET
variable LPLINUXLAYOUT  variable LPMACOSLAYOUT
variable LPUTIL         variable LPCELL         variable LPPTRSTORAGE  variable LPSTRUCTURES
variable LPENGINEERROR  variable LPENGINEERROREFFECTS
variable LPBYTES        variable LPCHECKER      variable LPRENDER
variable LPLOWERCERTBASE
variable LPTYPESCHEMA   variable LPTYPEFAM      variable LPSUMTYPE      variable LPLAYOUTBUF  variable LPLAYOUTVALID
variable LPHOOK         variable LPCELLEFF      variable LPPTRSTORAGEEFF
variable LPHABULAYOUT   variable LPENVBASE      variable LPINCLUDE
variable LPSCRIPTARGV   variable LPINTMARK
variable LPROLES
variable LPDECLTXN      variable LPGENDECL      variable LPDECLEVENT    variable LPSTRUCTMAKE
variable LPSTRUCTDECL   variable LPENUMDECL
variable LPENUMS        variable LPEXECVECTOR   variable LPSHA256       variable LPTFAMSHA
variable LPCOMBINATORS  variable LPXREF  variable LPGENDECLDICT  variable LPGENDECLPROT
variable LPLAYOUTSEAL  variable LPLOWERCERTSEAL
variable LPTOPROW
\ Boot stdlib block (dot habu-seed-the-stdlib-d8e3a757): the checked stdlib the
\ whole tree already requires, loaded once by the engine instead of once per
\ program.
variable LPPRELUDE      variable LPERRORS       variable LPOPTION
variable LPCADTYPES     variable LPCADARITH     variable LPSTRING
variable LPMEMORY       variable LPVECTOR
variable LCHKSNAPTOKEN
variable SRC-SFAIL

61 constant CHKSNAPTOKEN-LEN
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

\ Breakpoint-handler rows emit ABI-specific mcontext access, register save/print,
\ watched-cell display, one-shot restore, and instruction emulation.
\ Retirement: habu-builder-trust-rows-c5d41af6.
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

\ ---- naming a breakpoint's caller --------------------------------------------
\ The three pieces of the `habu-bp-lr:` line: its label, its bytes, and the read
\ that gets the interrupted thread's x30 out of the signal mcontext. In a package
\ because they are new - the label variables and message buffers around them are
\ this file's recorded packaging debt and a new name has no reason to join it.
\ MEASURED, NOT INFERRED: this handler's own x30 is long gone by the time the
\ first number reaches fd 2 (LHEX is a BL), so the value has to come from the
\ same mcontext the pc does.
package BP-CALLER
public
variable LBPLH
create BPL-KW 104 c, 97 c, 98 c, 117 c, 45 c, 98 c, 112 c, 45 c, 108 c, 114 c, 58 c, 10 c,   \ habu-bp-lr:\n
12 constant BPL-LEN

: C-MCTX-LR>R9 ( -- )
   HB-TARGET-LINUX? IF 9 9 LINUX-MCTX-LR-OFF LDR, exit THEN
   9 9 MACOS-MCTX-LR-OFF LDR, ;
s" BP-CALLER:C-MCTX-LR>R9" s" --" TRUST
;package

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

\ WHO CALLED THIS, which is the question a breakpoint in a seeded engine is
\ usually asked and could not answer. The pc and the top of the data stack say
\ where execution is and what it is holding; the interrupted x30 says where it
\ came back to, and tools/code-owner.f turns that into a record name. It is read
\ out of the same mcontext the pc comes from, so it is the value the trapped
\ thread held - not this handler's own link register, which the BL to LHEX has
\ already overwritten by the time the first number is printed.
: C-BP-PRINT-HIT ( -- )
   1 LBPH LABEL@ ADR,  0 2 MOVZ,  2 9 MOVZ,  NR-WRITE SYS,
   9 SP 32 LDR,  LHEX LABEL@ BL,
   9 SP 24 LDR,  C-MCTX-X19>R12
   9 12 8 SUBI,  9 9 0 LDR,  LHEX LABEL@ BL,
   1 BP-CALLER:LBPLH LABEL@ ADR,  0 2 MOVZ,  2 BP-CALLER:BPL-LEN MOVZ,  NR-WRITE SYS,
   9 SP 24 LDR,  BP-CALLER:C-MCTX-LR>R9  LHEX LABEL@ BL,
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
   1 CP 4 ADDI,  PROT:LOPEN LABEL@ BL,
   8 SP 40 LDR,  11 8 0 LDR,  12 8 8 LDR,  12 11 0 STRW,
   PROT:LCLOSE LABEL@ BL,
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
   BP-CALLER:LBPLH LABEL@ LBL, BP-CALLER:BPL-KW BP-CALLER:BPL-LEN BYTES,
   LBPSH LABEL@ LBL, BPS-KW 15 BYTES,
   LBPWH LABEL@ LBL, BPW-KW 15 BYTES,
   LBADLOC LABEL@ LBL, BADLOC-KW $50 BYTES,
   LOPENERR LABEL@ LBL, s" hb: cannot open " BYTES,
   LOPENNL LABEL@ LBL, NL-KW 1 BYTES,
   LUNCMSG LABEL@ LBL, s" hb: uncaught throw code " BYTES,     \ UNCMSG-LEN bytes; LUNCAUGHT appends the signed code + newline
   LWIDEMSG LABEL@ LBL, s" hb: interpret-mode layout value: " BYTES,     \ WIDEMSG-LEN bytes; LWIDE appends the token + newline
   LINTMSG LABEL@ LBL, s" hb: internal engine word: " BYTES,             \ INTMSG-LEN bytes; LINTERNAL appends the token + newline
   LMINMSG LABEL@ LBL, s" hb: interpret stack underdepth: " BYTES,       \ MINMSG-LEN bytes; LMININ appends the token + newline
   LPREFMISSMSG LABEL@ LBL, S\" hb: compile preflight hook missing\n" BYTES, \ PREFMISSMSG-LEN contiguous bytes
   LORPHANMSG LABEL@ LBL, s" hb: control-flow closer without opener: " BYTES,   \ ORPHANMSG-LEN bytes; LORPHAN appends the closer token + newline
   LCFCAPMSG LABEL@ LBL, s" hb: control-flow nesting too deep: " BYTES,          \ CFCAPMSG-LEN bytes; LCFCAP appends the opener token + newline
   LCSTRMSG LABEL@ LBL, s" hb: counted string too long (max 255)" BYTES,        \ CSTRMSG-LEN bytes; LCSTR appends a newline (fixed label: names the constraint at a glance)
   LDPBADMSG LABEL@ LBL, s" hb: data space out of range" BYTES,                 \ DPBADMSG-LEN bytes; LDPBAD appends a newline (DP-heap allot bound)
   LDICTFULL LABEL@ LBL, s" hb: dictionary full at: " BYTES,             \ CAPMSG-LEN bytes; capacity arms append the token + newline
   LCODEFULL LABEL@ LBL, s" hb: code space full at: " BYTES,             \ CAPMSG-LEN bytes
   LSNAPBAD LABEL@ LBL, s" hb: snapshot trailer corrupt" BYTES,  NL-KW 1 BYTES,               \ SNAPBAD-MSG-LEN bytes incl. newline
   LSNAPVER LABEL@ LBL, s" hb: snapshot format version unsupported" BYTES,  NL-KW 1 BYTES,     \ SNAPVER-MSG-LEN bytes incl. newline
   SNAP-RELOC:LCALLMSG LABEL@ LBL, s" hb: snapshot call map mismatch" BYTES,  NL-KW 1 BYTES,     \ SNAP-RELOC:CALLMSG-LEN bytes incl. newline
   SNAP-RELOC:LXTMSG LABEL@ LBL, s" hb: snapshot address table full" BYTES,  NL-KW 1 BYTES,      \ SNAP-RELOC:XTMSG-LEN bytes incl. newline
   SNAP-RELOC:LADDRMSG LABEL@ LBL, s" hb: snapshot address map mismatch" BYTES,  NL-KW 1 BYTES,   \ SNAP-RELOC:ADDRMSG-LEN bytes incl. newline
   SNAP-RELOC:LXTBANDMSG LABEL@ LBL, s" hb: snapshot address cell out of range" BYTES,  NL-KW 1 BYTES,  \ SNAP-RELOC:XTBANDMSG-LEN bytes incl. newline
   LSRCFULL LABEL@ LBL, s" hb: source prefix buffer full" BYTES,  NL-KW 1 BYTES,               \ SRCFULL-MSG-LEN bytes incl. newline
   LSRCREAD LABEL@ LBL, s" hb: cannot read source" BYTES,  NL-KW 1 BYTES,                       \ SRCREAD-MSG-LEN bytes incl. newline
   LBADSTR  LABEL@ LBL, s" hb: bad string literal" BYTES,  NL-KW 1 BYTES,                       \ BADSTR-MSG-LEN bytes incl. newline
   LPROTPUB LABEL@ LBL, s" hb: cannot publish into protected word: " BYTES,                     \ PROTPUB-MSG-LEN bytes; C-STORE-DEF-NAME appends the def name + newline
   LPROTAOT LABEL@ LBL, s" hb: AOT protected-WID gate reject: " BYTES,                          \ PROTAOT-MSG-LEN bytes; EM-AOTWIDGATE appends the callee name + newline
   LMMAPCODE LABEL@ LBL, s" hb: cannot map fixed code region" BYTES,  NL-KW 1 BYTES,            \ MMAPCODE-MSG-LEN bytes incl. newline
   LMMAPDATA LABEL@ LBL, s" hb: cannot map fixed data region" BYTES,  NL-KW 1 BYTES,            \ MMAPDATA-MSG-LEN bytes incl. newline
   LBLRANGE LABEL@ LBL, s" hb: code region out of BL range" BYTES,  NL-KW 1 BYTES, ;            \ BLRANGE-MSG-LEN bytes incl. newline

\ LCEMITBL ( x11 = absolute target ) : emit ONE direct BL imm26 to x11 at CP, then CP += 4.
\ The single call-emit primitive for every statically known native call — dictionary words
\ (C-CALL), the runtime compiler (compile,/BCOMPILE), and the registered engine helpers
\ (LP2VEXEC/(PROT-SPAN)) alike. disp = target - CP; the region maps at __text + REGION-OFF
\ so every call site and callee sit within BL's +/-128 MiB reach, and the boot assertion
\ (EM-MMAP-CODE-REGION, exit BL-RANGE-RC) guarantees the whole region stays in range. Each
\ emission still fails closed on that window: a future layout that broke it would otherwise
\ encode a wild displacement into live code. Builds x9 = BL opcode | imm26, then tails into
\ LCEMIT (CP guard + word store + CP += 4).
\ Before encoding it records the site in the region-to-text call map when the callee lies
\ OUTSIDE the mapped region (layout.f CALLMAP-OFF). That test is the region's own extent,
\ not a guess about the value: a callee either is inside [DBASE, DBASE+REGION) or it is in
\ the engine's loaded __text. Only the second kind changes distance between the run that
\ writes a snapshot image and the run that restores it, so only the second kind is recorded
\ and later relocated (EM-SNAPSHOT-REBASE-CALLS).
\ x11 (the callee) is dead once the map bit is set, so it doubles as scratch; x5/x6/x9/x10
\ are the registers this primitive already clobbers.
: EMIT-CEMITBL ( -- )
   LBL LBL {: rok:label nomark:label :}
   LCEMITBL LABEL@ LBL,
   10 11 CP SUB,                                       \ x10 = disp = target - CP (signed byte distance)
   5 BL-REACH LIT64,  6 10 5 ADD,  5 5 1 LSLI,  6 5 CMP,    \ (disp + BL-REACH) vs 2*BL-REACH (unsigned window)
   C-CC rok BCOND,                                     \ disp in [-BL-REACH, BL-REACH) -> encode; else fail closed
      1 LBLRANGE LABEL@ ADR,  0 2 MOVZ,  2 BLRANGE-MSG-LEN MOVZ,  NR-WRITE SYS,
      0 BL-RANGE-RC MOVZ,  NR-EXIT-GROUP SYS,
   rok LBL,
   6 11 DBASE SUB,  5 REGION LIT64,  6 5 CMP,          \ callee inside the region?
   C-CC nomark BCOND,                                  \ yes: distance is position independent, nothing to record
      11 CP DBASE SUB,                                 \ x11 = this site's byte offset within the region
      9 11 0 ADDI,  9 9 2 LSRI,  9 9 7 ANDI,           \ x9 = bit number = word index & 7
      6 11 0 ADDI,  6 6 5 LSRI,                        \ x6 = map byte index = offset >> 5
      5 SNAP-RELOC:CALLMAP-OFF LIT64,  6 6 5 ADD,  6 DATA 6 ADD,  \ x6 = map byte address
      5 1 MOVZ,  5 5 9 LSLV,  9 6 0 LDRB,  9 9 5 ORR,  9 6 0 STRB,
   nomark LBL,
   10 10 2 ASRI,  5 $03FFFFFF LIT64,  10 10 5 AND,      \ imm26 = (disp >> 2) & 0x03FFFFFF (sign-preserving)
   9 $94000000 LIT64,  9 9 10 ORR,                     \ x9 = BL opcode | imm26
   LCEMIT LABEL@ B, ;                                  \ tail: LCEMIT guards CP, stores x9, advances CP += 4, RET

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

: C-SOURCE-APPEND-X4-TO ( label -- ) {: fail:label :}
   2 11 0 ADDI,
   5 IBUFSZ LIT64,
   2 2 5 ADD,
   9 2 CMP,
   C-GE fail BCOND,
   4 9 0 STRB,
   9 9 1 ADDI, ;

: PFX-APPEND-ENGINE-SNAP-HOOK ( -- )
   LBL LBL {: loop:label done:label :}
   12 LCHKSNAPTOKEN LABEL@ ADR,
   13 CHKSNAPTOKEN-LEN MOVZ,
   loop LBL,
   13 done CBZ,
   4 12 0 LDRB,
   SRC-SFAIL LABEL@ C-SOURCE-APPEND-X4-TO
   12 12 1 ADDI,  13 13 1 SUBI,
   loop B,
   done LBL, ;

: PFX-APPEND-ENGINE-SNAP-HOOK-BUILD ( -- )
   LBL {: done:label :}
   12 SP 8 LDR,
   12 done CBZ,
   PFX-APPEND-ENGINE-SNAP-HOOK
   done LBL, ;

: PFX-LOAD-CHECKER-FILES ( -- )
   PFX-COMMON LPUTIL         s" src/core/util.f"        PFX-LOAD-ROW
   PFX-COMMON LPCELL         s" src/core/cell.f"        PFX-LOAD-ROW
   PFX-COMMON LPPTRSTORAGE   s" src/core/pointer-storage.f" PFX-LOAD-ROW
   PFX-COMMON LPENGINEERROR  s" src/core/engine-error.f" PFX-LOAD-ROW
   PFX-COMMON LPEXECVECTOR   s" src/core/exec-vector.f" PFX-LOAD-ROW
   PFX-COMMON LPCHECKER      s" src/core/checker.f"     PFX-LOAD-ROW
   PFX-COMMON LPENGINEERROREFFECTS s" src/core/engine-error-effects.f" PFX-LOAD-ROW
   PFX-COMMON LPLOWERCERTBASE s" src/core/lower-cert-base.f" PFX-LOAD-ROW
   PFX-COMMON LPTYPESCHEMA   s" src/core/type-schema.f" PFX-LOAD-ROW
   PFX-COMMON LPTYPEFAM      s" src/core/type-family.f" PFX-LOAD-ROW
   PFX-COMMON LPRENDER       s" src/core/render.f"      PFX-LOAD-ROW
   PFX-COMMON LPSUMTYPE      s" src/core/sumtype.f"     PFX-LOAD-ROW
   PFX-COMMON LPLAYOUTBUF    s" src/core/layout-buffer.f" PFX-LOAD-ROW
   PFX-COMMON LPLAYOUTVALID  s" src/core/layout-valid.f" PFX-LOAD-ROW
   PFX-COMMON LPHOOK         s" src/core/check-hook.f"  PFX-LOAD-ROW
   PFX-COMMON LPCELLEFF      s" src/core/cell-effects.f" PFX-LOAD-ROW
   PFX-COMMON LPPTRSTORAGEEFF s" src/core/pointer-storage-effects.f" PFX-LOAD-ROW
   PFX-COMMON LPDECLTXN      s" src/core/declaration-transaction.f" PFX-LOAD-ROW
   PFX-COMMON LPGENDECL      s" src/core/generated-declaration.f" PFX-LOAD-ROW ;

: PFX-LOAD-DECL-FILES ( -- )
   \ The shared declaration-event transaction loads first, then the STRUCTURE
   \ constructor generator, then the STRUCTURE declarer (which calls the
   \ generator, so it must load after it), then the ENUM declarer (a pure
   \ event-driven leaf) — all after the checker hook.
   PFX-COMMON LPDECLEVENT    s" src/core/decl-event.f"     PFX-LOAD-ROW
   PFX-COMMON LPSTRUCTMAKE   s" src/core/structure-make.f" PFX-LOAD-ROW
   PFX-COMMON LPSTRUCTDECL   s" src/core/structure-decl.f" PFX-LOAD-ROW
   PFX-COMMON LPENUMDECL     s" src/core/enum-decl.f"      PFX-LOAD-ROW ;

: PFX-LOAD-CORE-FILES ( -- )
   PFX-COMMON LPSTRUCTURES   s" src/core/structures.f"  PFX-LOAD-ROW
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
   PFX-COMMON LPSHA256       s" src/core/sha256.f"      PFX-LOAD-ROW
   PFX-COMMON LPTFAMSHA      s" src/core/type-family-sha.f" PFX-LOAD-ROW
   PFX-COMMON LPCOMBINATORS  s" src/core/combinators.f" PFX-LOAD-ROW
   PFX-COMMON LPXREF         s" src/habu/xref.f"        PFX-LOAD-ROW
   PFX-COMMON LPGENDECLDICT  s" src/core/generated-declaration-dictionary.f" PFX-LOAD-ROW
   PFX-COMMON LPGENDECLPROT  s" src/core/generated-declaration-protection.f" PFX-LOAD-ROW
   PFX-COMMON LPLAYOUTSEAL   s" src/core/layout-buffer-seal.f" PFX-LOAD-ROW
   PFX-COMMON LPLOWERCERTSEAL s" src/core/lower-cert-seal.f" PFX-LOAD-ROW ;

: PFX-LOAD-BASE-FILES ( -- )
   PFX-LOAD-CHECKER-FILES
   PFX-LOAD-DECL-FILES
   PFX-LOAD-CORE-FILES ;

\ The boot stdlib (dot habu-seed-the-stdlib-d8e3a757). These eight files are the
\ checked surface the tree already requires everywhere - lib/string.f alone has
\ 548 requiring files, lib/errors.f 472 - so every program used to pay for its
\ own copy. Loading them once here makes each of those requires a registry
\ no-op and gives every program the same pre-seal copy.
\
\ Order is the files' own require graph, read from the files: prelude and
\ errors have no requires; adt/option.f is a bare ENUM; cad-num-types.f is the
\ role NEWTYPEs and cad-num-arithmetic.f requires it; string.f requires errors,
\ adt/option and cad-num-arithmetic; memory.f requires errors and
\ cad-num-arithmetic; vector.f requires errors and memory. Three of them spell
\ the dependency as `s" path" required` rather than the `require` keyword, so
\ the graph has to be read from the files and not grepped for one spelling.
\
\ Unlike every other prefix file these DO carry require lines, so the provide
\ rows for the eight must reach the registry BEFORE the first file text is read
\ - otherwise the first `required` inside a file re-reads a file this table
\ already loaded and the boot dies on a duplicate definition. That is what
\ PFX-LOAD-STDLIB-COLD orders, and why the block cannot simply be appended to
\ PFX-LOAD-CORE-FILES.
\
\ src/arch/arm64/asm.f is not here YET, and the reason it could not be is gone.
\ It used to publish 146 global names, so loading it at every boot made a bare
\ `enc-b` under `using onnx` ambiguous against the global ENC-B it defined
\ (E-USING-SHADOW-GLOBAL, maki/onnx/proto-test.f). It is package A64ASM now (dot
\ habu-pkg-the-arm64-ce972795) and publishes nothing globally, which
\ test/compiler/asm-package-test.f measures from both sides. Adding the row is
\ the seed work's, not the packaging's - dot habu-seed-the-chain-e98b03d4.
: PFX-LOAD-STDLIB-FILES ( -- )
   PFX-COMMON LPPRELUDE      s" lib/prelude.f"            PFX-LOAD-ROW
   PFX-COMMON LPERRORS       s" lib/errors.f"             PFX-LOAD-ROW
   PFX-COMMON LPOPTION       s" lib/adt/option.f"         PFX-LOAD-ROW
   PFX-COMMON LPCADTYPES     s" lib/cad-num-types.f"      PFX-LOAD-ROW
   PFX-COMMON LPCADARITH     s" lib/cad-num-arithmetic.f" PFX-LOAD-ROW
   PFX-COMMON LPSTRING       s" lib/string.f"             PFX-LOAD-ROW
   PFX-COMMON LPMEMORY       s" lib/memory.f"             PFX-LOAD-ROW
   PFX-COMMON LPVECTOR       s" lib/vector.f"             PFX-LOAD-ROW ;

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
\ (cell/pointer-storage effect rows, checked structure words, xref/script-argv
\ signatures) before it
\ classifies the remaining sig-less prefix words as DNAME-INT internal.
: PFX-LOAD-INTMARK ( -- )
   PFX-COMMON LPINTMARK      s" src/core/internal-mark.f" PFX-LOAD-ROW ;

: PFX-LOAD-INTMARK-COLD ( -- )
   LBL {: done:label :}
   12 DATA SNAP-CELL LDR,
   12 done CBNZ,
   PFX-LOAD-INTMARK
   done LBL, ;

\ top-row.f installs the tier-1 top-level row tracker (dot
\ habu-typed-top-checker-82cf8b84) as the LAST cold-prefix source: after
\ internal-mark's own top-level `0 set-check` window (whose re-arm lives inside a
\ word body and is invisible to a token hook), so the tracker starts armed and
\ empty on the first user token. Snapshot boots skip it (SNAP-CELL guard), like
\ internal-mark above; snapshot/AOT re-install is docs/typed-top-level.md §5.6.
: PFX-LOAD-TOPROW ( -- )
   PFX-COMMON LPTOPROW       s" src/core/top-row.f"     PFX-LOAD-ROW ;

: PFX-LOAD-TOPROW-COLD ( -- )
   LBL {: done:label :}
   12 DATA SNAP-CELL LDR,
   12 done CBNZ,
   PFX-LOAD-TOPROW
   done LBL, ;

: PFX-LOAD-FILES ( -- )
   PFX-LOAD-BASE-FILES
   PFX-LOAD-SCRIPT-ARGV
   PFX-LOAD-INTMARK ;

: PFX-PATH-CHECKER-FILES ( -- )
   PFX-COMMON LPUTIL         s" src/core/util.f"        PFX-PATH-ROW
   PFX-COMMON LPCELL         s" src/core/cell.f"        PFX-PATH-ROW
   PFX-COMMON LPPTRSTORAGE   s" src/core/pointer-storage.f" PFX-PATH-ROW
   PFX-COMMON LPENGINEERROR  s" src/core/engine-error.f" PFX-PATH-ROW
   PFX-COMMON LPEXECVECTOR   s" src/core/exec-vector.f" PFX-PATH-ROW
   PFX-COMMON LPCHECKER      s" src/core/checker.f"     PFX-PATH-ROW
   PFX-COMMON LPENGINEERROREFFECTS s" src/core/engine-error-effects.f" PFX-PATH-ROW
   PFX-COMMON LPLOWERCERTBASE s" src/core/lower-cert-base.f" PFX-PATH-ROW
   PFX-COMMON LPTYPESCHEMA   s" src/core/type-schema.f" PFX-PATH-ROW
   PFX-COMMON LPTYPEFAM      s" src/core/type-family.f" PFX-PATH-ROW
   PFX-COMMON LPRENDER       s" src/core/render.f"      PFX-PATH-ROW
   PFX-COMMON LPSUMTYPE      s" src/core/sumtype.f"     PFX-PATH-ROW
   PFX-COMMON LPLAYOUTBUF    s" src/core/layout-buffer.f" PFX-PATH-ROW
   PFX-COMMON LPLAYOUTVALID  s" src/core/layout-valid.f" PFX-PATH-ROW
   PFX-COMMON LPHOOK         s" src/core/check-hook.f"  PFX-PATH-ROW
   PFX-COMMON LPCELLEFF      s" src/core/cell-effects.f" PFX-PATH-ROW
   PFX-COMMON LPPTRSTORAGEEFF s" src/core/pointer-storage-effects.f" PFX-PATH-ROW
   PFX-COMMON LPDECLTXN      s" src/core/declaration-transaction.f" PFX-PATH-ROW
   PFX-COMMON LPGENDECL      s" src/core/generated-declaration.f" PFX-PATH-ROW ;

: PFX-PATH-DECL-FILES ( -- )
   PFX-COMMON LPDECLEVENT    s" src/core/decl-event.f"     PFX-PATH-ROW
   PFX-COMMON LPSTRUCTMAKE   s" src/core/structure-make.f" PFX-PATH-ROW
   PFX-COMMON LPSTRUCTDECL   s" src/core/structure-decl.f" PFX-PATH-ROW
   PFX-COMMON LPENUMDECL     s" src/core/enum-decl.f"      PFX-PATH-ROW ;

: PFX-PATH-CORE-FILES ( -- )
   PFX-COMMON LPSTRUCTURES   s" src/core/structures.f"  PFX-PATH-ROW
   PFX-COMMON LPROLES        s" src/core/roles.f"       PFX-PATH-ROW
   PFX-COMMON LPBYTES        s" src/core/bytes.f"       PFX-PATH-ROW
   PFX-LINUX  LPLINUXTARGET  s" src/os/linux/target.f"  PFX-PATH-ROW
   PFX-MACOS  LPMACOSTARGET  s" src/os/macos/target.f"  PFX-PATH-ROW
   PFX-LINUX  LPLINUXLAYOUT  s" src/os/linux/layout.f"  PFX-PATH-ROW
   PFX-MACOS  LPMACOSLAYOUT  s" src/os/macos/layout.f"  PFX-PATH-ROW
   PFX-COMMON LPHABULAYOUT   s" src/habu/layout.f"      PFX-PATH-ROW
   PFX-COMMON LPENVBASE      s" src/os/env-base.f"      PFX-PATH-ROW
   PFX-COMMON LPINCLUDE      s" src/core/include.f"     PFX-PATH-ROW
   PFX-COMMON LPENUMS        s" src/core/enums.f"       PFX-PATH-ROW
   PFX-COMMON LPSHA256       s" src/core/sha256.f"      PFX-PATH-ROW
   PFX-COMMON LPTFAMSHA      s" src/core/type-family-sha.f" PFX-PATH-ROW
   PFX-COMMON LPCOMBINATORS  s" src/core/combinators.f" PFX-PATH-ROW
   PFX-COMMON LPXREF         s" src/habu/xref.f"        PFX-PATH-ROW
   PFX-COMMON LPGENDECLDICT  s" src/core/generated-declaration-dictionary.f" PFX-PATH-ROW
   PFX-COMMON LPGENDECLPROT  s" src/core/generated-declaration-protection.f" PFX-PATH-ROW
   PFX-COMMON LPLAYOUTSEAL   s" src/core/layout-buffer-seal.f" PFX-PATH-ROW
   PFX-COMMON LPLOWERCERTSEAL s" src/core/lower-cert-seal.f" PFX-PATH-ROW
   PFX-COMMON LPSCRIPTARGV   s" src/os/script-argv.f"   PFX-PATH-ROW
   PFX-COMMON LPINTMARK      s" src/core/internal-mark.f" PFX-PATH-ROW
   PFX-COMMON LPTOPROW       s" src/core/top-row.f"     PFX-PATH-ROW ;

: PFX-PATH-STDLIB-FILES ( -- )
   PFX-COMMON LPPRELUDE      s" lib/prelude.f"            PFX-PATH-ROW
   PFX-COMMON LPERRORS       s" lib/errors.f"             PFX-PATH-ROW
   PFX-COMMON LPOPTION       s" lib/adt/option.f"         PFX-PATH-ROW
   PFX-COMMON LPCADTYPES     s" lib/cad-num-types.f"      PFX-PATH-ROW
   PFX-COMMON LPCADARITH     s" lib/cad-num-arithmetic.f" PFX-PATH-ROW
   PFX-COMMON LPSTRING       s" lib/string.f"             PFX-PATH-ROW
   PFX-COMMON LPMEMORY       s" lib/memory.f"             PFX-PATH-ROW
   PFX-COMMON LPVECTOR       s" lib/vector.f"             PFX-PATH-ROW ;

: PFX-PATH-FILES ( -- )
   PFX-PATH-CHECKER-FILES
   PFX-PATH-DECL-FILES
   PFX-PATH-CORE-FILES
   PFX-PATH-STDLIB-FILES ;

\ The prefix reload below zeroes HOOK-CELL, so the boot-time load of the
\ checker/core prefix is itself unchecked. The staged fixpoint pre-pass
\ closes that trust hole for every installed engine: the refresh's stage-N
\ binary certifies the FULL assembled stage-N+1 source (this prefix included)
\ before any stage compile (tools/build-fixpoint.f BF-CERTIFY-*, blocking),
\ and the BF-PIN content hashes close the mid-build reload TOCTOU. Remaining
\ gap: a post-install disk edit is reloaded unchecked at next boot until the
\ pin is baked into the image (dot habu-boot-pin-bake-8b284046).
: EMIT-HOST-LOAD-PREFIX ( -- )
   16 0 MOVZ,  16 DATA HOOK-CELL STR,  16 DATA COMPILE-PREFLIGHT-CELL STR,
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
\ Raw target ioctl emitter for startup TTY detection.
\ Retirement: habu-builder-trust-rows-c5d41af6.
: C-EMIT-TTY-PROBE ( -- )
   0 0 MOVZ,
   HB-TARGET-LINUX? if 1 $5401 LIT64, else
      HB-TARGET-MACOS? if 1 $40487413 LIT64, else C-TARGET-UNKNOWN then
   then
   2 DATA BODYBUF-OFF ADDI,
   NR-IOCTL SYS, ;
s" c-emit-tty-probe" s" --" TRUST

variable SRC-TTY  variable SRC-FILE
variable SRC-RL   variable SRC-RD    variable SRC-PIPEOK
variable SRC-REPL variable SRC-DONE  variable SRC-FSCAN
variable SRC-FNEXT variable SRC-FREADY variable SRC-FPLAIN
variable SRC-FLOOP variable SRC-SHLOOP variable SRC-STDINPROG
variable SRC-BLOOP variable SRC-BDONE  variable SRC-BFAIL
variable LCOLDPFX variable LCOLDPFXB variable LAPPPROV variable LAPPREQ

: C-SOURCE-LABELS ( -- )
   LBL SRC-TTY !   LBL SRC-FILE !  LBL SRC-SFAIL !
   LBL SRC-RL !    LBL SRC-RD !    LBL SRC-PIPEOK !
   LBL SRC-REPL !  LBL SRC-DONE !  LBL SRC-FSCAN !
   LBL SRC-FNEXT ! LBL SRC-FREADY ! LBL SRC-FPLAIN !
   LBL SRC-FLOOP ! LBL SRC-SHLOOP ! LBL SRC-STDINPROG !
   LBL SRC-BLOOP ! LBL SRC-BDONE ! LBL SRC-BFAIL !
   LBL LCOLDPFX !  LBL LCOLDPFXB !  LBL LAPPPROV !  LBL LAPPREQ ! ;

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

\ `s" <x12>" ` - the head both loader lines below share.
: C-SOURCE-APPEND-QUOTED ( -- )
   $73 C-SOURCE-APPEND-CHAR
   $22 C-SOURCE-APPEND-CHAR
   $20 C-SOURCE-APPEND-CHAR
   C-SOURCE-APPEND-Z12
   $22 C-SOURCE-APPEND-CHAR
   $20 C-SOURCE-APPEND-CHAR ;

: C-SOURCE-APPEND-PROVIDED ( -- )
   C-SOURCE-APPEND-QUOTED
   $70 C-SOURCE-APPEND-CHAR
   $72 C-SOURCE-APPEND-CHAR
   $6F C-SOURCE-APPEND-CHAR
   $76 C-SOURCE-APPEND-CHAR
   $69 C-SOURCE-APPEND-CHAR
   $64 C-SOURCE-APPEND-CHAR
   $65 C-SOURCE-APPEND-CHAR
   $64 C-SOURCE-APPEND-CHAR
   $0A C-SOURCE-APPEND-CHAR ;

: C-SOURCE-APPEND-REQUIRED ( -- )
   C-SOURCE-APPEND-QUOTED
   $73 C-SOURCE-APPEND-CHAR
   $63 C-SOURCE-APPEND-CHAR
   $72 C-SOURCE-APPEND-CHAR
   $69 C-SOURCE-APPEND-CHAR
   $70 C-SOURCE-APPEND-CHAR
   $74 C-SOURCE-APPEND-CHAR
   $2D C-SOURCE-APPEND-CHAR
   $72 C-SOURCE-APPEND-CHAR
   $65 C-SOURCE-APPEND-CHAR
   $71 C-SOURCE-APPEND-CHAR
   $75 C-SOURCE-APPEND-CHAR
   $69 C-SOURCE-APPEND-CHAR
   $72 C-SOURCE-APPEND-CHAR
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
\ Freeze the count of paths the ENGINE provides, as the last prefix token
\ alongside SEAL-CAPTURE and for the same reason: everything recorded up to
\ here is the engine's own surface, everything after it belongs to whoever is
\ running. src/core/include.f ENGINE-PROVIDES? reads the frozen count, so a
\ tool can tell "the engine carries this file" from "this process required it".
\ Cold boots only - a snapshot restores the count with the rest of its data.
: EMIT-REQUIRE-FREEZE-TOKEN ( -- )
   LBL {: done:label :}
   12 DATA SNAP-CELL LDR,
   12 done CBNZ,
   $52 C-SOURCE-APPEND-CHAR
   $45 C-SOURCE-APPEND-CHAR
   $51 C-SOURCE-APPEND-CHAR
   $55 C-SOURCE-APPEND-CHAR
   $49 C-SOURCE-APPEND-CHAR
   $52 C-SOURCE-APPEND-CHAR
   $45 C-SOURCE-APPEND-CHAR
   $2D C-SOURCE-APPEND-CHAR
   $42 C-SOURCE-APPEND-CHAR
   $4F C-SOURCE-APPEND-CHAR
   $4F C-SOURCE-APPEND-CHAR
   $54 C-SOURCE-APPEND-CHAR
   $2D C-SOURCE-APPEND-CHAR
   $46 C-SOURCE-APPEND-CHAR
   $52 C-SOURCE-APPEND-CHAR
   $45 C-SOURCE-APPEND-CHAR
   $45 C-SOURCE-APPEND-CHAR
   $5A C-SOURCE-APPEND-CHAR
   $45 C-SOURCE-APPEND-CHAR
   $0A C-SOURCE-APPEND-CHAR
   done LBL, ;

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

: PFX-PROVIDE-CHECKER-FILES ( -- )
   PFX-COMMON LPUTIL         s" src/core/util.f"        PFX-PROVIDE-ROW
   PFX-COMMON LPCELL         s" src/core/cell.f"        PFX-PROVIDE-ROW
   PFX-COMMON LPPTRSTORAGE   s" src/core/pointer-storage.f" PFX-PROVIDE-ROW
   PFX-COMMON LPENGINEERROR  s" src/core/engine-error.f" PFX-PROVIDE-ROW
   PFX-COMMON LPEXECVECTOR   s" src/core/exec-vector.f" PFX-PROVIDE-ROW
   PFX-COMMON LPCHECKER      s" src/core/checker.f"     PFX-PROVIDE-ROW
   PFX-COMMON LPENGINEERROREFFECTS s" src/core/engine-error-effects.f" PFX-PROVIDE-ROW
   PFX-COMMON LPLOWERCERTBASE s" src/core/lower-cert-base.f" PFX-PROVIDE-ROW
   PFX-COMMON LPTYPESCHEMA   s" src/core/type-schema.f" PFX-PROVIDE-ROW
   PFX-COMMON LPTYPEFAM      s" src/core/type-family.f" PFX-PROVIDE-ROW
   PFX-COMMON LPRENDER       s" src/core/render.f"      PFX-PROVIDE-ROW
   PFX-COMMON LPSUMTYPE      s" src/core/sumtype.f"     PFX-PROVIDE-ROW
   PFX-COMMON LPLAYOUTBUF    s" src/core/layout-buffer.f" PFX-PROVIDE-ROW
   PFX-COMMON LPLAYOUTVALID  s" src/core/layout-valid.f" PFX-PROVIDE-ROW
   PFX-COMMON LPHOOK         s" src/core/check-hook.f"  PFX-PROVIDE-ROW
   PFX-COMMON LPCELLEFF      s" src/core/cell-effects.f" PFX-PROVIDE-ROW
   PFX-COMMON LPPTRSTORAGEEFF s" src/core/pointer-storage-effects.f" PFX-PROVIDE-ROW
   PFX-COMMON LPDECLTXN      s" src/core/declaration-transaction.f" PFX-PROVIDE-ROW
   PFX-COMMON LPGENDECL      s" src/core/generated-declaration.f" PFX-PROVIDE-ROW ;

: PFX-PROVIDE-DECL-FILES ( -- )
   PFX-COMMON LPDECLEVENT    s" src/core/decl-event.f"     PFX-PROVIDE-ROW
   PFX-COMMON LPSTRUCTMAKE   s" src/core/structure-make.f" PFX-PROVIDE-ROW
   PFX-COMMON LPSTRUCTDECL   s" src/core/structure-decl.f" PFX-PROVIDE-ROW
   PFX-COMMON LPENUMDECL     s" src/core/enum-decl.f"      PFX-PROVIDE-ROW ;

: PFX-PROVIDE-CORE-FILES ( -- )
   PFX-COMMON LPSTRUCTURES   s" src/core/structures.f"  PFX-PROVIDE-ROW
   PFX-COMMON LPROLES        s" src/core/roles.f"       PFX-PROVIDE-ROW
   PFX-COMMON LPBYTES        s" src/core/bytes.f"       PFX-PROVIDE-ROW
   PFX-LINUX  LPLINUXTARGET  s" src/os/linux/target.f"  PFX-PROVIDE-ROW
   PFX-MACOS  LPMACOSTARGET  s" src/os/macos/target.f"  PFX-PROVIDE-ROW
   PFX-LINUX  LPLINUXLAYOUT  s" src/os/linux/layout.f"  PFX-PROVIDE-ROW
   PFX-MACOS  LPMACOSLAYOUT  s" src/os/macos/layout.f"  PFX-PROVIDE-ROW
   PFX-COMMON LPHABULAYOUT   s" src/habu/layout.f"      PFX-PROVIDE-ROW
   PFX-COMMON LPENVBASE      s" src/os/env-base.f"      PFX-PROVIDE-ROW
   PFX-COMMON LPINCLUDE      s" src/core/include.f"     PFX-PROVIDE-ROW
   PFX-COMMON LPENUMS        s" src/core/enums.f"       PFX-PROVIDE-ROW
   PFX-COMMON LPSHA256       s" src/core/sha256.f"      PFX-PROVIDE-ROW
   PFX-COMMON LPTFAMSHA      s" src/core/type-family-sha.f" PFX-PROVIDE-ROW
   PFX-COMMON LPCOMBINATORS  s" src/core/combinators.f" PFX-PROVIDE-ROW
   PFX-COMMON LPXREF         s" src/habu/xref.f"        PFX-PROVIDE-ROW
   PFX-COMMON LPGENDECLDICT  s" src/core/generated-declaration-dictionary.f" PFX-PROVIDE-ROW
   PFX-COMMON LPGENDECLPROT  s" src/core/generated-declaration-protection.f" PFX-PROVIDE-ROW
   PFX-COMMON LPLAYOUTSEAL   s" src/core/layout-buffer-seal.f" PFX-PROVIDE-ROW
   PFX-COMMON LPLOWERCERTSEAL s" src/core/lower-cert-seal.f" PFX-PROVIDE-ROW
   PFX-COMMON LPSCRIPTARGV   s" src/os/script-argv.f"   PFX-PROVIDE-ROW
   PFX-COMMON LPINTMARK      s" src/core/internal-mark.f" PFX-PROVIDE-ROW
   PFX-COMMON LPTOPROW       s" src/core/top-row.f"     PFX-PROVIDE-ROW ;

\ The nine boot-stdlib provide rows. They are NOT part of PFX-PROVIDE-FILES:
\ that word runs on snapshot boots too (it is outside the SNAP-CELL guard), and
\ the dev snapshot's keep surface (tools/build-fixpoint.f BF-APPEND-SNAP-KEEP)
\ does not carry lib/, so a row there would tell a snapshot engine that
\ lib/string.f is already loaded when it is not - a silent no-op require and a
\ missing STR:. On a cold boot these run inside PFX-LOAD-STDLIB-COLD, ahead of
\ the file texts, which is exactly where the nine need them.
: PFX-PROVIDE-STDLIB-FILES ( -- )
   PFX-COMMON LPPRELUDE      s" lib/prelude.f"            PFX-PROVIDE-ROW
   PFX-COMMON LPERRORS       s" lib/errors.f"             PFX-PROVIDE-ROW
   PFX-COMMON LPOPTION       s" lib/adt/option.f"         PFX-PROVIDE-ROW
   PFX-COMMON LPCADTYPES     s" lib/cad-num-types.f"      PFX-PROVIDE-ROW
   PFX-COMMON LPCADARITH     s" lib/cad-num-arithmetic.f" PFX-PROVIDE-ROW
   PFX-COMMON LPSTRING       s" lib/string.f"             PFX-PROVIDE-ROW
   PFX-COMMON LPMEMORY       s" lib/memory.f"             PFX-PROVIDE-ROW
   PFX-COMMON LPVECTOR       s" lib/vector.f"             PFX-PROVIDE-ROW ;

\ Cold boots only, like PFX-LOAD-SCRIPT-ARGV-COLD above: a snapshot restores its
\ bake-time dictionary and must not reload anything.
: PFX-LOAD-STDLIB-COLD ( -- )
   LBL {: done:label :}
   12 DATA SNAP-CELL LDR,
   12 done CBNZ,
   PFX-PROVIDE-STDLIB-FILES
   PFX-LOAD-STDLIB-FILES
   done LBL, ;

\ ---------------------------------------------------------------------------
\ The merged chain's provide rows.
\
\ Baking the chain puts its 43 files' definitions in the engine and tells
\ src/core/include.f nothing, so a program's `require
\ src/compiler/native/migrate.f` would read the file a second time and die on a
\ duplicate definition - the seeded engine would be WORSE for the chain's own
\ consumers than the source one. These rows are what make that require the
\ registry no-op it already is for src/core/checker.f.
\
\ THE LIST IS THE ARTIFACT'S, so nothing here can disagree with it: the merge
\ latches the closure the capture recorded and re-derives its digest from disk
\ (src/habu/aot-file.f RESTORE-CLOSURE, then the chain-digest refusal), and this
\ reads that same table. An emit with no artifact merged holds none, which is why
\ the capture host carries no row and is unchanged by construction.
package PFX-CHAIN
public

\ The label the table is bound to; LABELS:INIT mints it with the rest.
variable LTAB

private

\ ONE TABLE, NOT ONE LABEL PER FILE: the file set belongs to the artifact, so its
\ size is a runtime fact here, and a walk over a terminated table is what the boot
\ code can express without knowing it.
\ ASSEMBLED HERE AND EMITTED IN ONE BYTES, - measured, not assumed: every BYTES,
\ pads to the four-byte grain, so a path per call leaves up to three zero bytes
\ between rows and the walk reads the first of them as the table's end. The first
\ build of this table marked one file of the 43 for exactly that reason.
\ The buffer is what AOT-IDENT's own two bounds allow, so it cannot be the
\ narrower of two budgets.
AOT-IDENT:MAX AOT-IDENT:PATH-CAP 1 + * 1 + constant TAB-CAP
create TAB TAB-CAP allot
variable TAB-U

: TAB+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a  TAB TAB-U @ +  u BYTE-COPY
   0  TAB TAB-U @ + u +  c!
   TAB-U @ u + 1 + TAB-U ! ;

\ Every path zero-terminated, closed by an empty one.
: TAB! ( -- )
   0 TAB-U !
   AOT-IDENT:COUNT 0 ?do i AOT-IDENT:PATH$ TAB+ loop
   0 TAB TAB-U @ + c!
   TAB-U @ 1 + TAB-U ! ;

\ LAPPPROV leaves x12 on the terminator of the path it just wrote (its walk stops
\ at the zero byte without stepping over it), so one ADDI is the whole step to the
\ next path and an empty path ends the table.
: WALK ( -- )
   LBL LBL {: loop:label done:label :}
   12 LTAB LABEL@ ADR,
   loop LBL,
      4 12 0 LDRB,
      4 done CBZ,
      LAPPPROV LABEL@ BL,
      12 12 1 ADDI,
      loop B,
   done LBL, ;

public

\ The table itself, emitted with the other baked path strings.
: TABLE ( -- )
   AOT-IDENT:COUNT 0= if exit then
   TAB!
   LTAB LABEL@ LBL,
   TAB TAB-U @ BYTES, ;

\ Cold boots only, like the stdlib rows above and for the same reason: a snapshot
\ restores the registry it was baked with, and the dev snapshot's keep surface
\ (tools/build-fixpoint.f BF-APPEND-SNAP-KEEP) carries no chain at all.
: ROWS ( -- )
   AOT-IDENT:COUNT 0= if exit then
   LBL {: done:label :}
   12 DATA SNAP-CELL LDR,
   12 done CBNZ,
   WALK
   done LBL, ;

;package

: PFX-PROVIDE-FILES ( -- )
   PFX-PROVIDE-CHECKER-FILES
   PFX-PROVIDE-DECL-FILES
   PFX-PROVIDE-CORE-FILES ;

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
   11 DATA INP-CELL STR,  9 DATA BOOT-SRC:USER-END STR,   \ INE is the prefix end (LCOLDPFX); the program is the second stream
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
   16 1 MOVZ,                                              \ --load: hand argv files to the registry
   multi B,
   build LBL,
   14 2 MOVZ,  15 10 0 ADDI,  13 2 MOVZ,
   LCOLDPFXB LABEL@ BL,
   16 0 MOVZ,                                              \ --build: one certified payload, read raw
   multi LBL,
   C-SOURCE-FIND-SEP
   SRC-FPLAIN LABEL@ LBL,
   LCOLDPFX LABEL@ BL,
   16 0 MOVZ,                                              \ plain `hb prog.f`: the program, read raw
   SRC-FREADY LABEL@ LBL, ;

: C-SOURCE-ARGV14 ( -- )
   12 DATA ARGV-CELL LDR,  5 14 3 LSLI,
   12 12 5 ADD,  12 12 0 LDR, ;

\ One argv source file (dot habu-make-load-consult-85c88fb3).
\
\ `--load` means "make these files loaded", so it appends `s" <path>" required`
\ and lets src/core/include.f decide: a path the registry already holds is
\ skipped, anything else is read and recorded. The old row wrote the `provided`
\ marker and then inlined the text ANYWAY - it maintained a registry it never
\ read, so `--load lib/errors.f` against an engine that already carries
\ lib/errors.f died on a duplicate definition while `require lib/errors.f` from
\ inside a file no-opped cleanly. Same request, two answers; the loader now
\ consults the registry it writes.
\
\ The other two argv modes stay raw. `--build` receives ONE statically certified
\ payload, assembled and hashed as a single stream and far past INCLUDE-BUF-CAP;
\ plain `hb prog.f` receives the program itself, not a dependency. x16 carries
\ which mode we are in, set on each of the three exits of C-SOURCE-FILE-PREFIX
\ right after that mode's cold-prefix call - the same entry-mode-bit shape
\ LCOLDPFX/LCOLDPFXB already use for their own seal decision. The only other
\ write to x16 in the startup path is inside the cold prefix itself, which
\ always runs before these three stores.
: C-SOURCE-APPEND-ARG ( -- )
   LBL LBL {: raw:label done:label :}
   16 raw CBZ,
      C-SOURCE-ARGV14
      LAPPREQ LABEL@ BL,
      done B,
   raw LBL,
      C-SOURCE-ARGV14
      LAPPPROV LABEL@ BL,
      C-SOURCE-ARGV14
      LSRCRD LABEL@ BL,
   done LBL,
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
   11 DATA INP-CELL STR,  9 DATA BOOT-SRC:USER-END STR,   \ INE is the prefix end; the baked source is the second stream
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
   9 DATA INE-CELL STR,                            \ the prefix is this boot's first stream and it ends here
   12 LSRC LABEL@ ADR,  5 SRCN @ LIT64,  13 12 5 ADD,
   SRC-BLOOP LABEL@ LBL,
      12 13 CMP,  C-GE SRC-BDONE LABEL@ BCOND,
      2 11 0 ADDI,  5 IBUFSZ LIT64,  2 2 5 ADD,  9 2 CMP,  C-GE SRC-BFAIL LABEL@ BCOND,
      4 12 0 LDRB,  4 9 0 STRB,
      12 12 1 ADDI,  9 9 1 ADDI,
      SRC-BLOOP LABEL@ B,
   SRC-BDONE LABEL@ LBL,
   LSHBANG LABEL@ BL,
   11 DATA INP-CELL STR,  9 DATA BOOT-SRC:USER-END STR,  SRC-DONE LABEL@ B,   \ baked source: the second stream
   SRC-BFAIL LABEL@ LBL,                                          \ IBUFSZ baked-prefix overflow: label fd 2 before exit 74
   1 LSRCFULL LABEL@ ADR,  0 2 MOVZ,  2 SRCFULL-MSG-LEN MOVZ,  NR-WRITE SYS,
   0 74 MOVZ,  NR-EXIT-GROUP SYS,
   SRC-DONE LABEL@ LBL, ;

\ Shared cold-prefix routines: the stdin engine builds the checker/stdlib
\ provided-files prefix at the user entries plus the certified MODE-BUILD entry.
\ Three routines, emitted once each and
\ branched over so the fall-through startup path skips their bodies:
\
\ LAPPPROV ( x12 = string ptr, x9 = cursor -> appends `s" <str>" provided\n`,
\ x9 advanced ). Leaf: the append sequence uses only STRB/branch (no BL), so no
\ x30 frame. Replaces the ~552-byte per-row inline C-SOURCE-APPEND-PROVIDED,
\ which was emitted ~19 times in PFX-PROVIDE-FILES plus once in the argv loop.
\
\ LAPPREQ is the same leaf with `required` in place of `provided`: the --load
\ argv row, which asks the registry to load a path rather than asserting the
\ path is already loaded and inlining it regardless.
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
   LAPPREQ LABEL@ LBL,
      C-SOURCE-APPEND-REQUIRED  RET,
   LCOLDPFX LABEL@ LBL,
      SP SP 16 SUBI,  30 SP 0 STR,
      12 0 MOVZ,  12 SP 8 STR,  body B,
   LCOLDPFXB LABEL@ LBL,
      SP SP 16 SUBI,  30 SP 0 STR,
      12 1 MOVZ,  12 SP 8 STR,
   body LBL,
      EMIT-COLD-PREFIX
      PFX-LOAD-STDLIB-COLD                         \ the checked stdlib, after the core files
      PFX-APPEND-ENGINE-SNAP-HOOK-BUILD
      PFX-LOAD-SCRIPT-ARGV-COLD
      PFX-PROVIDE-FILES
      PFX-LOAD-INTMARK-COLD                        \ LAST prefix definition/marking pass
      PFX-LOAD-TOPROW-COLD                         \ tier-1 top-row tracker: armed on the first user token
      PFX-CHAIN:ROWS                               \ the merged chain's files, ahead of the freeze
      EMIT-REQUIRE-FREEZE-TOKEN                    \ engine-provided path count, same boundary
      EMIT-SEAL-CAPTURE-TOKEN                      \ watermark token at the true engine-prefix end
      12 SP 8 LDR,  12 noseal CBNZ,
      SRC-SFAIL LABEL@ EMIT-SEAL-FRIEND-TOKEN      \ seal before any appended user source
   noseal LBL,
      9 DATA INE-CELL STR,                         \ the prefix is the boot's FIRST stream and it ends here
      30 SP 0 LDR,  SP SP 16 ADDI,  RET,
   skip LBL, ;

: EMIT-SOURCE ( -- )
   C-SOURCE-LABELS
   STDIN? @ IF EMIT-COLD-PREFIX-SHARED C-SOURCE-STDIN ELSE C-SOURCE-BAKED THEN ;

\ ---- control-flow JIT helpers ----
\ Raw-register helper emits a variable-width value-stack drop.
\ Retirement: habu-builder-trust-rows-c5d41af6.
: C-EMIT-DROP-X12 ( -- )
   LBL {: done:label :}
   12 done CBZ,
      9 $910003FF LIT64,  14 12 10 LSLI,  9 9 14 ORR,  LCEMIT LABEL@ BL,
   done LBL, ;
s" c-emit-drop-x12" s" --" TRUST

: EMIT-CF-HELPERS ( -- )
   LBL LBL LBL LBL LBL LBL {: pisb pdone kno kyes kchk knf :}
   LCFPUSH LABEL@ LBL,
      \ The control-flow stack lives inside the protected region, so a push must
      \ declare its band before it writes. PROT:LCF is register-transparent and
      \ costs a load and a branch once the band is already open, which it is for
      \ every push after the first in a bracket; only x30 has to be framed,
      \ because the branch itself is what takes it.
      SP SP 16 SUBI,  30 SP 0 STR,
      PROT:LCF LABEL@ BL,
      30 SP 0 LDR,  SP SP 16 ADDI,
      5 CFSTK-OFF LIT64,  10 DBASE 5 ADD,  11 10 0 LDR,    \ x11 = control-flow depth
      12 CFSTK-DEPTH-MAX MOVZ,  11 12 CMP,                 \ depth vs region cap (x12 = internal scratch, reloaded below; callers like J-ELSE preserve x14 across this call)
      C-GE LCFCAP LABEL@ BCOND,                            \ at the cap: fail-closed reject BEFORE the write (never overflow into the code area above CFSTK)
      12 CF-REC MOVZ,  12 11 12 MUL,  12 12 10 ADD,  12 12 8 ADDI,
      9 12 0 STR,
      13 DATA LOCN-CELL LDR,  13 12 CF-LOCN STR,
      13 DATA LOCF-CELL LDR,  13 12 CF-LOCF STR,
      11 11 1 ADDI,  11 10 0 STR,  RET,
   LCFPOP LABEL@ LBL,
      5 CFSTK-OFF LIT64,  10 DBASE 5 ADD,  11 10 0 LDR,      \ x11 = control-flow depth (peek before pop)
      11 LORPHAN LABEL@ CBZ,                                 \ empty stack: orphan closer, fail-closed reject (never underflow into a bogus branch origin)
      SP SP 16 SUBI,  30 SP 0 STR,
      PROT:LCF LABEL@ BL,                                    \ the depth store below is a control-flow band write
      11 11 1 SUBI,  11 10 0 STR,
      12 CF-REC MOVZ,  12 11 12 MUL,  12 12 10 ADD,  12 12 8 ADDI,
      16 12 0 ADDI,
      15 DATA LOCF-CELL LDR,  14 16 CF-LOCF LDR,  12 15 14 SUB,
      C-EMIT-DROP-X12
      13 16 CF-LOCN LDR,  13 DATA LOCN-CELL STR,
      14 16 CF-LOCF LDR,  14 DATA LOCF-CELL STR,
      9 16 0 LDR,
      30 SP 0 LDR,  SP SP 16 ADDI,  RET,
   LPAT LABEL@ LBL,
      \ A patch writes BELOW CP - the branch it fills was emitted earlier in this
      \ body - so the code band opened at the bracket's CP need not reach it: the
      \ compile loop reopens at the CP it holds after every checker call, and pass
      \ 2 rewinds CP outright. The target says so itself. Only x30 is framed; the
      \ declaration leaves x9 and x11 alone.
      SP SP 16 SUBI,  30 SP 0 STR,
      1 9 0 ADDI,  2 4 MOVZ,  PROT:LSPAN LABEL@ BL,
      30 SP 0 LDR,  SP SP 16 ADDI,
      11 9 0 LDRW,  10 CP 9 SUB,  10 10 2 ASRI,
      \ Select the immediate field by branch CLASS, not by bit 31: an
      \ unconditional B ($14000000, opcode bits[31:26]=000101) carries imm26
      \ (bits 0..25); every other forward placeholder patched through here —
      \ CBZ/CBNZ ($B4/$B5, bit31=1) AND B.cond ($54000000, bit31=0, the MATCH-arm
      \ mismatch-skip b.ne) — carries imm19 (bits 5..23). Bit 31 alone would
      \ misfile a B.cond as imm26 and corrupt its cond+opcode fields.
      5 $FC000000 LIT64,  13 11 5 AND,  5 $14000000 LIT64,  13 5 CMP,  C-EQ pisb BCOND,
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
      SP SP 16 SUBI,  30 SP 0 STR,                 \ clobbers x0-x2 (the declaration), x5, x10-x12
      bcl LBL,  9 bcd CBZ,
         10 DBASE 9 ADD,  11 10 0 LDRW,
         1 10 0 ADDI,  2 4 MOVZ,  PROT:LSPAN LABEL@ BL,   \ each link is a write below CP
         12 14 10 SUB,  12 12 2 ASRI,
         5 $3FFFFFF LIT64,  12 12 5 AND,
         5 $14000000 LIT64,  12 12 5 ORR,
         12 10 0 STRW,
         9 11 0 ADDI,  bcl B,
      bcd LBL,  30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

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
\ DEFER-MAGIC is src/habu/layout.f's, beside the dispatch-cell offset it belongs
\ with: this file writes the trailer and checks it, and the native chain's
\ src/compiler/native/dict.f checks it too, so the number has one definition.
variable LKWDEFER  variable LKWIS  variable LKWDEFERUNSET
\ The defer/is misuse diagnostics own one concern between them - telling the
\ source what it got wrong with `defer` or `is` - so they live in their own
\ package, on the HOLD-EMIT model below, rather than joining the global emitter
\ surface around them. The label cells are public because KWDATA:EMIT bakes
\ their strings and the LABELS allocator mints their ids; each message's byte
\ length is private, stated once beside the string it measures. The emitters
\ that write them reopen this package further down, next to the defer rows.
package DEFER-DIAG
34 constant NOTOKEN-LEN    \ byte length of "hb: is: missing target word after " (LDEFNOTOKEN)
31 constant NOTFOUND-LEN   \ byte length of "hb: is: no deferred word named " (LDEFNOTFOUND)
29 constant NOTDEFER-LEN   \ byte length of "hb: is: not a deferred word: " (LDEFNOTDEFER)
30 constant NONAME-LEN     \ byte length of "hb: defer: missing name after " (LDEFNONAME)
72 constant HINT-LEN       \ byte length of "hb: is: parsing words resolve outside using-imports; qualify the target\n" (LDEFHINT)
public
variable LDEFNOTOKEN  variable LDEFNOTFOUND  variable LDEFNOTDEFER  variable LDEFNONAME
variable LDEFHINT
;package
variable LCHKDEFER  variable LSIGPTRA  variable LSIGA  variable LRECWPUB  variable LRECMIQ  variable LP2DOESW

\ The held-publication seam's own two names: the label cell holding the spelling
\ the publish tail looks up, and the emitter that asks the question. They own one
\ concern between them - withholding a certified definition for the native chain
\ - so they live in their own package rather than joining the global emitter
\ surface around them, which is pre-existing debt and not a pattern to extend.
package HOLD-EMIT
public
variable LHOLDQ
;package
\ ADT lowering keywords (TFAM 10, docs §16): `construct` dispatches through the
\ CMM-CELL mode machine (J-CONSTRUCT + EM-COMPILE-ADT-MODE, slice 2; execution
\ gate-pinned by GE-CONSTRUCT-EXEC). The MATCH rows are data-only until slice 3
\ wires the eliminator states — those tokens still fail closed as undefined.
variable LKWCONSTRUCT  variable LKWMATCH  variable LKWSEMIMATCH
variable LTFLCONFAM  variable LTFLCVAR   \ TFL lowering-surface bridge names (C-FIND-GLOBAL)
variable LTFLMATCHFAM  variable LTFLNAME \ MATCH bridge names: tfl-match-fam? / tfam-name$
variable LBADTAGPFX    variable LBADTAGSFX  \ bad-tag die message spans (C-DIE-BAD-TAG)
variable LRESTAB    \ sealed system-package name table (TFAM 2b-ii)

\ Keyword string data section: the baked name table every LKW*/LCHK* label
\ points into, plus the sealed reserved-name table it embeds.
package KWDATA

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
  12 c, $65 c, $6E c, $67 c, $69 c, $6E c, $65 c, \ "engine-error"
         $2D c, $65 c, $72 c, $72 c, $6F c, $72 c,
   0 c,                                           \ terminator
here RESTAB-BUF - constant RESTAB-LEN

public

\ Label cell for the baked `trust-raw` keyword string. Declared here rather
\ than in the habu1.f global label chain because the raw-storage seal is a
\ habu2.f concern: the keyword row below bakes the string, LASTC-TRUST:FIND-RAW
\ resolves it, and the LABELS allocator mints its id.
variable LKWTRUSTRAW

\ The same, for the baked `trust-decl` keyword string: which registrar the
\ engine's publish tail reaches is a habu2.f concern too, DEF-TRUST:FIND
\ resolves it, and the row below bakes it.
variable LKWTRUSTDECL

: EMIT ( -- )
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
   LKWTRUSTDECL LABEL@ LBL, s" trust-decl" BYTES,      LKWTRUSTRAW LABEL@ LBL, s" trust-raw" BYTES,      LKWCHKDOES LABEL@ LBL, s" check-does!" BYTES,  LKWPACKAGE LABEL@ LBL, s" package" BYTES,  LKWPUBLIC LABEL@ LBL, s" public" BYTES,
   LKWPRIVATE LABEL@ LBL, s" private" BYTES,  LKWSEMIPACKAGE LABEL@ LBL, s" ;package" BYTES,  LKWDUPDEF LABEL@ LBL, s" duplicate definition: " BYTES,  LKWQUOT LABEL@ LBL,  QUOT-KW 2 BYTES,   LKWSEMIQ LABEL@ LBL,  SEMIQ-KW 2 BYTES,  LKWDEFER LABEL@ LBL, s" defer" BYTES,  LKWIS LABEL@ LBL, s" is" BYTES,  LKWDEFERUNSET LABEL@ LBL, s" defer-unset" BYTES,  DEFER-DIAG:LDEFNOTOKEN LABEL@ LBL, s" hb: is: missing target word after " BYTES,  DEFER-DIAG:LDEFNOTFOUND LABEL@ LBL, s" hb: is: no deferred word named " BYTES,  DEFER-DIAG:LDEFNOTDEFER LABEL@ LBL, s" hb: is: not a deferred word: " BYTES,  DEFER-DIAG:LDEFHINT LABEL@ LBL, S\" hb: is: parsing words resolve outside using-imports; qualify the target\n" BYTES,  DEFER-DIAG:LDEFNONAME LABEL@ LBL, s" hb: defer: missing name after " BYTES,  LCHKPACKAGE LABEL@ LBL, s" checker-package" BYTES,  LCHKPUB LABEL@ LBL, s" checker-public" BYTES,  LCHKPRI LABEL@ LBL, s" checker-private" BYTES,  LCHKENDPKG LABEL@ LBL, s" checker-end-package" BYTES,  LCHKDEFER LABEL@ LBL, s" checker-defer" BYTES,  LRESTAB LABEL@ LBL, RESTAB-BUF RESTAB-LEN BYTES,  LSIGPTRA LABEL@ LBL, s" -- ptr a" BYTES,  LSIGA LABEL@ LBL, s" -- a" BYTES,  LRECWPUB LABEL@ LBL, s" rec-wide-publish" BYTES,  LRECMIQ LABEL@ LBL, s" rec-min-in@" BYTES,  HOLD-EMIT:LHOLDQ LABEL@ LBL, s" checker-hold?" BYTES,  LP2DOESW LABEL@ LBL, s" hb: does>-split cannot lower layout width facts: " BYTES,
   LKWEXPORT LABEL@ LBL, s" export" BYTES,  LCHKEXPORT LABEL@ LBL, s" checker-export" BYTES,
   LKWUSING LABEL@ LBL, s" using" BYTES,  LKWSEMIUSING LABEL@ LBL, s" ;using" BYTES,  LCHKUSING LABEL@ LBL, s" checker-using" BYTES,
   LKWCONSTRUCT LABEL@ LBL, s" construct" BYTES,  LKWMATCH LABEL@ LBL, s" match" BYTES,  LKWSEMIMATCH LABEL@ LBL, s" ;match" BYTES,
   LTFLCONFAM LABEL@ LBL, s" tfl-con-fam?" BYTES,  LTFLCVAR LABEL@ LBL, s" tfl-cvar?" BYTES,
   LTFLMATCHFAM LABEL@ LBL, s" tfl-match-fam?" BYTES,  LTFLNAME LABEL@ LBL, s" tfam-name$" BYTES,
   LBADTAGPFX LABEL@ LBL, s" hb: bad " BYTES,  LBADTAGSFX LABEL@ LBL, BADTAG-SFX-KW 5 BYTES,
   LCHKSNAPTOKEN LABEL@ LBL,
   s" ' CHECKER-SNAPSHOT-PREPARE data-base ENGINE-SNAP-XT-CELL + !" BYTES,
   NL-KW 1 BYTES,
   PFX-PATH-FILES
   PFX-CHAIN:TABLE ;

;package

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
\ arms CMM=3 (want family). Emission (reduction 2): the bundle is already spilled,
\ so peek the tag ONCE here (ldur x9,[x19,#-8]) — no token emits code between this
\ point and the first arm's compare, and the mismatch-skip chain never touches x19
\ or x9 before the next compare, so every arm's cmp reuses this one load.
: J-MATCH ( -- )
   9 0 MOVZ,  LCFPUSH LABEL@ BL,
   9 DATA CMFRD-CELL LDR,  9 9 1 ADDI,  9 DATA CMFRD-CELL STR,
   12 3 MOVZ,  12 DATA CMM-CELL STR,
   $F85F8269 C-EMITW ;                  \ ldur x9,[x19,#-8]: single per-dispatch tag peek

\ J-OF pushes a CASE-arm marker (bit 0) onto the CMBK branch-kind bitstack so the
\ shared J-ENDOF can tell a CASE arm from a MATCH variant branch (which pushes a 1
\ in EM-ADT-MATCH-OF). Dispatch (mirroring the MATCH slimming): compare the peeked
\ scrutinee against the popped arm value and skip straight to the next arm on a
\ b.ne, instead of materializing a cset boolean only to test it with a cbz. The
\ cmp sets NZ and the very next emitted word is the branch (C-PUSHCP is
\ compile-time and emits nothing), so the flags reach the b.ne untouched. LPAT
\ already patches the b.ne placeholder via its imm19 branch-class path, exactly
\ like the shared exit->join B and the former cbz. Saves one word (4 bytes) and
\ one executed instruction per arm.
: J-OF ( -- )
   14 DATA CMBK-CELL LDR,  14 14 1 LSLI,  14 DATA CMBK-CELL STR,
   C-POP-X16                             \ pop arm value into x16
   $F85F8269 C-EMITW                     \ ldur x9,[x19,#-8]  peek scrutinee
   $EB10013F C-EMITW                     \ cmp x9,x16
   C-PUSHCP
   $54000001 C-EMITW                     \ b.ne +0  (skip to next arm on mismatch)
   $D1002273 C-EMITW ;                   \ sub x19,x19,#8  drop scrutinee on match

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

\ The loop-emit family: the DO/LEAVE level stack (open a level, prove one is
\ open, chain a LEAVE placeholder onto the innermost one) and the loop keyword
\ handlers built on it. The loop keyword dispatch rows reopen this package
\ further down so they resolve the handlers bare.
package LOOP-EMIT

: LVOPEN ( -- )                                 \ open a LEAVE-chain level: LVH[LVD]=0, LVD++
   9 DATA LVD-CELL LDR,
   10 9 3 LSLI,  10 10 LVH-OFF ADDI,  10 DATA 10 ADD,
   12 0 MOVZ,  12 10 0 STR,
   10 9 3 LSLI,  10 10 LVF-OFF ADDI,  10 DATA 10 ADD,
   12 DATA LOCF-CELL LDR,  12 10 0 STR,
   9 9 1 ADDI,  9 DATA LVD-CELL STR, ;

\ The DO/LEAVE level stack (LVD-CELL depth + the LVH/LVF level arrays) is the
\ loop family's opener record: `do`/`?do` open a level in LVOPEN, `loop`/
\ `+loop` close one in J-LOOPEND, and `leave` chains onto the innermost open
\ one. LCFPOP's orphan guard covers only the CF stack, so it does not see this
\ stack at all: a loop-family word with no open `do` indexed level -1, and
\ LVH-OFF's cell -1 IS LVD-CELL itself (they are adjacent, $578/$580). `loop`
\ and `+loop` then handed that junk head offset to LBCHAIN, which dereferenced
\ DBASE + junk and took SIGSEGV (rc 134); `leave` silently stored a code offset
\ into LVD-CELL, forging a non-zero "open level" count out of nothing. Every
\ consumer of the level stack now proves a level exists BEFORE reading or
\ writing it, and otherwise takes the shared closer-without-opener reject, the
\ exact sibling of LCFPOP's underflow guard (dot habu-fix-loop-closer-9e5d012e).
\ Guarding `leave` is what keeps LVD-CELL a sole-writer count of open `do`
\ levels, so the `loop`/`+loop` guard is an existence check and not a value
\ test an earlier stray `leave` could defeat.
\ Clobbers x9 only; every call site reloads or overwrites x9 immediately after.
: LVREQUIRE ( -- )                              \ reject a loop-family word with no open DO level
   LBL {: ok:label :}
   9 DATA LVD-CELL LDR,  9 ok CBNZ,
      LORPHAN LABEL@ B,                  \ TKA/TKL still hold the offending token
   ok LBL, ;

: LVLEAVE ( -- )                                \ chain a B placeholder on the current level
   LVREQUIRE
   9 DATA LVD-CELL LDR,  9 9 1 SUBI,
   10 9 3 LSLI,  10 10 LVF-OFF ADDI,  10 DATA 10 ADD,
   14 10 0 LDR,  15 DATA LOCF-CELL LDR,  12 15 14 SUB,  C-EMIT-DROP-X12
   9 DATA LVD-CELL LDR,  9 9 1 SUBI,
   10 9 3 LSLI,  10 10 LVH-OFF ADDI,  10 DATA 10 ADD,
   9 10 0 LDR,
   11 CP DBASE SUB,  11 10 0 STR,
   LCEMIT LABEL@ BL, ;

: J-DO ( -- )
   J-FRAME  LVOPEN  C-PUSHCP ;

: J-?DO ( -- )                                  \ DO, but skip the loop when limit = start
   J-FRAME  LVOPEN
   $EB0A013F C-EMITW                     \ cmp x9,x10  (start/limit still live)
   $54000041 C-EMITW                     \ b.ne +8 (over the skip placeholder)
   LVLEAVE
   C-PUSHCP ;

: J-LEAVE ( -- )  LVLEAVE ;

: J-UNLOOP ( -- )                               \ pop one loop frame, no branch
   4181780107 C-EMITW  3506439531 C-EMITW  4177585803 C-EMITW ;

: J-LOOPEND ( -- )                              \ shared LOOP/+LOOP tail: pop frame, patch
   14 CP 0 ADDI,                         \ LEAVE/?DO skips to the pop point, LVD--
   4181780107 C-EMITW  3506439531 C-EMITW  4177585803 C-EMITW
   9 DATA LVD-CELL LDR,  9 9 1 SUBI,  9 DATA LVD-CELL STR,
   10 9 3 LSLI,  10 10 LVH-OFF ADDI,  10 DATA 10 ADD,  9 10 0 LDR,
   LBCHAIN LABEL@ BL, ;

: J-LOOP ( -- )
   LVREQUIRE                             \ no open DO level: reject before emitting or popping
   4181780107 C-EMITW  3506439531 C-EMITW  3548179820 C-EMITW  2434269580 C-EMITW  2333344140 C-EMITW
   4181721481 C-EMITW  4181722506 C-EMITW  2432697641 C-EMITW  4177527177 C-EMITW  3943301439 C-EMITW
   LCFPOP LABEL@ BL,
   10 9 CP SUB,  10 10 2 ASRI,  5 $7FFFF LIT64,  10 10 5 AND,  10 10 5 LSLI,
   9 $5400000B LIT64,  9 9 10 ORR,  LCEMIT LABEL@ BL,
   J-LOOPEND ;

: J-+LOOP ( -- )                                \ index += n; loop while (old-limit) and
   LVREQUIRE                             \ no open DO level: reject before emitting or popping
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

;package

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

\ Native task guard and dictionary/checker lookup bridge operate on generated
\ registers and dynamically found checker words.
\ Retirement: habu-builder-trust-rows-c5d41af6.
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
\ Scratch stays off x11: DEF-TRUST:FIND/C-FIND-GLOBAL leave the record XT in x11
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

\ ---- reaching the checker's definer-facing registrar --------------------------
\ One concern: how the engine hands the checker a signature a DEFINER just
\ declared. It is the sibling of LASTC-TRUST below, which does the same job for
\ a created word's raw storage cell, and it is a package for the same reason.
\
\ WHY IT IS `trust-decl` AND NOT `trust`. Both spellings register the same row.
\ The difference is what the caller may be asked to prove. A definer publishing
\ a record has not made it findable yet - measured, a definition is absent from
\ `search-wl` at its own publish tail and present one statement later - so a
\ registrar reached from here must never consult the dictionary. Source writing
\ `s" NAME" s" SIG" trust` is in the opposite position: it claims a word exists,
\ and that claim is exactly what used to go unchecked, minting a global checker
\ symbol for a stale row in silence (dot habu-make-trust-refuse-cc8e19de). Two
\ questions, two words, so neither has to guess which caller it has.
package DEF-TRUST

public

\ Resolve `trust-decl` (checker.f TRUST-DECL) into x11. Same fail-closed shape
\ as LASTC-TRUST:FIND-RAW: a missing registrar names itself on fd 2 and exits 70
\ rather than publishing a definition whose effect nothing recorded. Public
\ because BDRAINPRETRUST needs the XT for its own push sequence.
: FIND ( -- )  LBL {: ok:label :}
   9 KWDATA:LKWTRUSTDECL LABEL@ ADR,  10 10 MOVZ,  LFIND LABEL@ BL,
   13 ok CBNZ,
      0 2 MOVZ,  1 KWDATA:LKWTRUSTDECL LABEL@ ADR,  2 10 MOVZ,  NR-WRITE SYS,
      0 70 MOVZ,  NR-EXIT-GROUP SYS,
   ok LBL, ;

\ Register the pending definition's declared signature: name from the body
\ buffer, signature from the TSIG cells. NOT named `CALL`: an upper-case
\ `PKG:CALL` is a reserved shape - tools/lint/clobber-lint.f reads it as a
\ register-move prelude plus a branch to a shared engine helper (today only
\ PROT-GUARD:CALL) and fails closed on any other, which is the right refusal
\ and not a model to widen.
: REGISTER ( -- )
   FIND
   C-PUSH-DREC-NAME
   TSIG-A-CELL TSIG-U-CELL C-PUSH-TRUST-SIG
   C-CALL-X11-SAVED ;

;package

\ ---- created-word effect publication -----------------------------------------
\ The three public words below are every place the engine gives a defining
\ word's creation an effect the checker will believe. All three publish a cell
\ of raw dictionary storage, so all three register through `trust-raw`: the
\ type variables in the published effect are minted raw and cannot bind a
\ nominal family, which is what stops a plain integer stored into the cell from
\ being read back out as a sealed nominal with no converter in between.
\
\ LASTC-TRUST:PUBLISH publishes the runtime effect a `does>` clause declared
\ for the words its defining word creates, so it covers user-written defining
\ words as well as PTR-VARIABLE. LASTC-TRUST:PUBLISH-PTR-A publishes `-- ptr a`
\ for create and variable, and LASTC-TRUST:PUBLISH-A publishes `-- a` for
\ constant.
package LASTC-TRUST

\ Resolve `trust-raw`, the checker's raw-storage effect registrar (checker.f
\ TRUST-RAW). Every word the engine publishes from a defining word owns a cell
\ of raw dictionary storage, so its effect must be registered with raw type
\ variables that cannot bind a nominal family; the publish words below all
\ route here instead of at `trust-decl`. Same shape as DEF-TRUST:FIND: a
\ missing registrar names itself on fd 2 and exits 70 rather than publishing
\ the word unsealed.
: FIND-RAW ( -- )  LBL {: ok:label :}
   9 KWDATA:LKWTRUSTRAW LABEL@ ADR,  10 9 MOVZ,  LFIND LABEL@ BL,
   13 ok CBNZ,
      0 2 MOVZ,  1 KWDATA:LKWTRUSTRAW LABEL@ ADR,  2 9 MOVZ,  NR-WRITE SYS,
      0 70 MOVZ,  NR-EXIT-GROUP SYS,
   ok LBL, ;

public

: PUBLISH ( -- )
   LBL {: nohook:label :}
   9 DATA HOOK-CELL LDR,  9 nohook CBZ,
   FIND-RAW
   C-PUSH-DREC-NAME
   CRSIG-A-CELL CRSIG-U-CELL C-PUSH-TRUST-SIG
   C-CALL-X11-SAVED
   nohook LBL, ;

: PUBLISH-PTR-A ( -- )
   LBL {: nohook:label :}
   9 DATA HOOK-CELL LDR,  9 nohook CBZ,
   FIND-RAW
   C-PUSH-DREC-NAME
   9 LSIGPTRA LABEL@ ADR,  9 G-PUSH
   9 8 MOVZ,  9 G-PUSH
   C-CALL-X11-SAVED
   nohook LBL, ;

: PUBLISH-A ( -- )
   LBL {: nohook:label :}
   9 DATA HOOK-CELL LDR,  9 nohook CBZ,
   FIND-RAW
   C-PUSH-DREC-NAME
   9 LSIGA LABEL@ ADR,  9 G-PUSH
   9 4 MOVZ,  9 G-PUSH
   C-CALL-X11-SAVED
   nohook LBL, ;

;package

: C-FIND-GLOBAL? ( ptr n n -- ) {: name:ptr len:n :}
   SP SP 16 SUBI,
   14 DATA PKG-PUB-CELL LDR,  14 SP 0 STR,
   14 DATA PKG-PRI-CELL LDR,  14 SP 8 STR,
   14 0 MOVZ,  14 DATA PKG-PUB-CELL STR,  14 DATA PKG-PRI-CELL STR,
   9 name LABEL@ ADR,  10 len MOVZ,  LFIND LABEL@ BL,
   14 SP 0 LDR,  14 DATA PKG-PUB-CELL STR,
   14 SP 8 LDR,  14 DATA PKG-PRI-CELL STR,
   SP SP 16 ADDI, ;
s" C-FIND-GLOBAL?" s" ptr n n --" TRUST

: C-FIND-GLOBAL ( ptr n n -- ) {: name:ptr len:n :}
   LBL {: ok:label :}
   name len C-FIND-GLOBAL?
   13 ok CBNZ,
      0 2 MOVZ,  1 name LABEL@ ADR,  2 len MOVZ,  NR-WRITE SYS,
      0 70 MOVZ,  NR-EXIT-GROUP SYS,
   ok LBL, ;
s" C-FIND-GLOBAL" s" ptr n n --" TRUST

: C-FIND-CHECKER ( ptr n n label -- ) {: name:ptr len:n done:label :}
   LBL {: ready:label :}
   name len C-FIND-GLOBAL?
   13 ready CBNZ,
   9 DATA FRIEND-LATCH-CELL LDR,  9 done CBZ,
   name len C-FIND-GLOBAL
   ready LBL, ;
s" C-FIND-CHECKER" s" ptr n n label --" TRUST

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
\ wide-effect rows (verified) and user source only loads post-hook; the
\ engine-prefix records certified between check-hook.f and the seal are
\ re-poked by the seal-time pass (src/core/internal-mark.f).
\ Certified min-in (dot habu-habu-certified-words-84e84eaf): the same tail
\ then drains the checker's RECMI latch through rec-min-in@ (checker.f, loads
\ before check-hook.f, so it is always resolvable once the hook is live) and
\ pokes DNAME-MIN-IN (flags bits 52-59) of the just-published record inside
\ the LPROT RW window, so the interpret-find dispatch can fail closed on a
\ bare underdepth call. min-in 0 (unchecked/no-signature boundary) pokes nothing.
: EM-REC-WIDE-PUBLISH ( -- )
   LBL LBL {: nohook:label nomark:label :}
   9 DATA HOOK-CELL LDR,  9 nohook CBZ,
   LRECWPUB 16 C-FIND-GLOBAL
   C-CALL-X11-SAVED
   LRECMIQ 11 C-FIND-GLOBAL
   C-CALL-X11-SAVED
   10 G-POP                                            \ x10 = latched min-in cells (0 = none)
   10 nomark CBZ,
      9 NDICT 0 ADDI,  9 9 1 SUBI,  12 DREC MOVZ,  9 9 12 MUL,  9 DBASE 9 ADD,   \ x9 = &record[NDICT-1] (x10 latch survives via kernel x2-x15)
      2 3 MOVZ,  LPROTREC LABEL@ BL,
      10 10 $FF ANDI,  10 10 52 LSLI,                  \ min-in byte -> DNAME-MIN-IN bits 52-59
      12 9 16 LDR,  12 12 10 ORR,  12 9 16 STR,
      2 5 MOVZ,  LPROTREC LABEL@ BL,
   nomark LBL,
   nohook LBL, ;

: C-DIE-DOES ( -- )                                   \ does>-body checker reject: recoverable inside evaluate (rc 70), fail-closed exit 70 at top level
   0 2 MOVZ,  1 LKWDOES LABEL@ ADR,  2 5 MOVZ,  NR-WRITE SYS,
   0 70 MOVZ,  LCOMPILEDIE LABEL@ B, ;

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

: C-EMIT-DATA-X16! ( n -- ) {: off :}
   16 20 off W-STRX C-EMITW ;

: C-EMIT-CRSIG-PART! ( n n -- ) {: src dst :}
   11 DATA src LDR,  C-RAW-LIT
   dst C-EMIT-DATA-X16! ;

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

: C-SIG-BAD ( -- )                                   \ malformed created/required stack-sig: recoverable inside evaluate (rc 76), fail-closed exit 76 at top level. Fires before any dict publish (does/defer/trust name not yet counted into NDICT); rollback drops only compile-state + region prot.
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 76 MOVZ,  LCOMPILEDIE LABEL@ B, ;

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

\ J-DOES is emitted further down, past the capacity exits its clause record
\ reaches for (C-DIE-DICT-FULL / C-DIE-CODE-FULL).

: J-QUOT ( -- )
   LBL {: qok :}
   9 DATA QPATCH-CELL LDR,  9 qok CBZ,                    \ nested [: quotation opener: recoverable inside evaluate (rc 75), fail-closed exit 75 at top level. Fires before J-QUOT touches QPATCH/emit; rollback drops compile-state
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 75 MOVZ,  LCOMPILEDIE LABEL@ B,
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
   9 DATA QPATCH-CELL LDR,  9 sqok CBNZ,                  \ ;] with no open quotation: recoverable inside evaluate (rc 75), fail-closed exit 75 at top level. Fires before J-SEMIQUOT emits; rollback drops compile-state
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 75 MOVZ,  LCOMPILEDIE LABEL@ B,
   sqok LBL,
   14 CP 0 ADDI,  9 DATA EXITH-CELL LDR,  LBCHAIN LABEL@ BL,   \ exits -> this epilogue
   9 DATA QXH-CELL LDR,  9 DATA EXITH-CELL STR,
   9 $F94003FE LIT64,  LCEMIT LABEL@ BL,                \ epilogue: ldr x30,[sp]
   9 $910043FF LIT64,  LCEMIT LABEL@ BL,                \ add sp,#16
   9 W-RET LIT64,  LCEMIT LABEL@ BL,
   9 DATA QPATCH-CELL LDR,  LPAT LABEL@ BL,             \ b-over lands here
   11 DATA QENT-CELL LDR,  C-CODE-ADDR             \ push the xt in the outer word (relocatable code addr)
   12 0 MOVZ,  12 DATA QPATCH-CELL STR, ;

\ The does>-patch runtime routine emitter (LDOESPATCH): patches the created
\ word's RET into a branch and publishes the declared runtime effect.
\
\ AND CLEARS THE CREATED RECORD'S DEFINER KIND, which is the one place that fact
\ can stop being true. `create` stamped the record DKIND:ADDR because the body it
\ emitted pushes the word's data address and returns; the patch below replaces
\ that return with a branch into a clause body, so from here the word runs
\ whatever the clause says and its address is no longer what a mention of it
\ means. A reader that folded the stale stamp would compile the address of the
\ storage in place of a call to the behaviour - the miscompile the stamp exists
\ to make impossible - so the clear happens in the same protected window as the
\ patch, on the same record, and never a step apart from it.
package DOESPATCH

public

: EMIT ( -- )
   LBL {: nocr :}
   LDOESPATCH LABEL@ LBL,
   SP SP 32 SUBI,  30 SP 0 STR,  10 SP 8 STR,
   1 CP 4 ADDI,  PROT:LOPEN LABEL@ BL,                  \ code band -> RW
   10 SP 8 LDR,
   11 DATA LASTC-CELL LDR,                               \ created slot
   12 11 0 LDR,  13 11 8 LDR,  12 12 13 ADD,             \ x12 = RET addr
   1 11 0 ADDI,  2 DREC MOVZ,  PROT:LSPAN LABEL@ BL,     \ the created record, whose kind stamp clears below
   1 12 0 ADDI,  2 4 MOVZ,  PROT:LSPAN LABEL@ BL,        \ the created body's RET, below CP
   14 10 12 SUB,  14 14 2 ASRI,                          \ delta words (negative)
   5 $3FFFFFF LIT64,  14 14 5 AND,
   5 $14000000 LIT64,  14 14 5 ORR,                      \ b D
   14 12 0 STRW,
   13 11 16 LDR,  5 DKIND:MASK -1 xor LIT64,  13 13 5 AND,  13 11 16 STR,   \ the body is a clause now, not a push
   12 SP 16 STR,
   PROT:LCLOSE LABEL@ BL,                                \ region -> RX
   12 SP 16 LDR,
   12 DCCVAU,  DSB-ISH,  12 ICIVAU,  DSB-ISH,  ISB,      \ flush the patched line
   9 DATA CRSIG-U-CELL LDR,  9 nocr CBZ,
      LASTC-TRUST:PUBLISH
      C-RUNTIME-CRSIG-CLEAR
   nocr LBL,
   30 SP 0 LDR,  SP SP 32 ADDI,  RET, ;

;package

\ ---- interpret-mode defining words ----
\ record defining words for the checker: append the kind token + run the hook
\ (verdict ignored — create/variable/constant always publish).
\ kwv is the keyword's LABEL CELL (LKWCREATE / LKWCONST), read by LABEL@, so its
\ element type is a cell — not the keyword's bytes.
: C-DEFHOOK ( ptr n n -- )  LBL {: kwv:ptr klen:n nohk:label :}
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
      1 15 0 ADDI,  PROT:RESERVE                 \ a long name is a direct write at CP
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
   fail LBL,                                          \ long name would overflow the code region: recoverable inside evaluate (rc 76), fail-closed exit 76 at top level
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 76 MOVZ,  LCOMPILEDIE LABEL@ B,
   done LBL, ;

: C-QUALIFY-FAIL ( n -- ) {: rc:n :}                  \ qualified-name misuse ($4B) / dict-full ($4D) during C-QUALIFY-DEF: recoverable inside evaluate, fail-closed exit rc at top level
   0 2 MOVZ,  1 DATA DEF-TKA-CELL LDR,  2 DATA DEF-TKL-CELL LDR,  NR-WRITE SYS,
   0 2 MOVZ,  1 LQNL LABEL@ ADR,  1 1 1 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
   0 rc MOVZ,  LCOMPILEDIE LABEL@ B, ;

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

: C-DIE-TOKEN-NL ( n -- ) {: rc:n :}                  \ definer capacity die (dict full $4D / code full $4C): recoverable inside evaluate (rollback frees the aborted definition), fail-closed exit rc at top level
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 2 MOVZ,  1 LQNL LABEL@ ADR,  1 1 1 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
   0 rc MOVZ,  LCOMPILEDIE LABEL@ B, ;

: C-DIE-DICT-FULL ( -- )
   LDICTFULL C-CAP-LABEL
   $4D C-DIE-TOKEN-NL ;

: C-DIE-CODE-FULL ( -- )
   LCODEFULL C-CAP-LABEL
   $4C C-DIE-TOKEN-NL ;

\ ---- the does>-clause's own dictionary record --------------------------------
\ WHY THE CLAUSE NEEDS A NAME. LDOESPATCH patches the created word's RET into
\ `b D`, and D is the clause entry — an address INSIDE the defining word's
\ compiled body. That branch is the one call in the system the AOT seed cannot
\ relocate: it travels as a raw PC-relative word, so a capture whose defining
\ word lies OUTSIDE the window carries a displacement measured against code no
\ target has, and the seeded engine branches into the middle of another word
\ (dot habu-merged-engine-nmigrate-c970bf04: three chain words, one SIGSEGV).
\ Giving the clause a record of its own makes D a record ENTRY, so the
\ name-keyed call-site path carries it and the seed resolves it in the engine it
\ is booting exactly as it resolves every BL. Carrying a name plus a byte delta
\ instead was refused: a captured branch must never depend on a callee's
\ interior layout.
\
\ IT OVERLAPS ITS PARENT AND TRIMS NOTHING. The defining word's length is
\ written at `;`, past the clause body, so the parent's span already covers the
\ clause. Overlapping spans are ordinary here — EXPORT publishes a second record
\ over one body and CODE-RECLAIM republishes — and trimming the parent would
\ move a span the inliner, the snapshot and the size census all read.
\
\ THE NAME IS THE PARENT'S PLUS `;does`, derived and not invented, because the
\ seed looks it up in ANOTHER engine and the only identity two engines spell the
\ same way is the parent's own name. Every byte above $20 is a legal name byte;
\ `:` is the one to avoid, since a colon inside a name makes the name
\ package-qualified. The bytes are stored out of line (DNAME-EXT) whatever their
\ length, in the gap between the defining word's own exit and the clause entry —
\ code the parent branches over and never executes, which is why the `adr` that
\ computes D is assembled here rather than being the constant it was.
\
\ THE WID IS THE PARENT'S, which is also the answer the sealed-WID gate needs:
\ the clause is exactly as public or private as the word it belongs to. The slot
\ is &dict[NDICT+1], one above the parent's own pending slot, and both are
\ counted by the one publish; every abandon path leaves both uncounted.
package DOES-REC

5 constant SUF-LEN                                     \ ";does"

public

\ x11 = the parent's pending record, x13 = the clause name's length, x14 = the
\ parent's name bytes, x15 = the clause name's length rounded up to a word.
: NAME$ ( -- )
   LBL {: inl:label :}
   11 DATA PEND-CELL LDR,
   12 11 16 LDR,
   13 12 0 ADDI,  13 13 14 LSLI,  13 13 14 LSRI,       \ the parent's own name length
   14 11 24 ADDI,
   12 12 DNAME-EXT ANDI,  12 inl CBZ,
      14 11 24 LDR,                                    \ ... which is a pointer when it is long
   inl LBL,
   13 13 SUF-LEN ADDI,
   15 13 3 ADDI,  15 15 2 LSRI,  15 15 2 LSLI, ;

\ The two capacities the clause adds: one dictionary slot above the parent's,
\ and the name bytes at CP. Refused here, before anything is written.
: ROOM ( -- )
   LBL LBL {: ndok:label cpok:label :}
   9 DICT-CAP 1 - MOVZ,  NDICT 9 CMP,  C-LT ndok BCOND,
      C-DIE-DICT-FULL
   ndok LBL,
   9 CP 15 ADD,
   10 REGION $4000 - LIT64,  10 DBASE 10 ADD,  9 10 CMP,  C-LT cpok BCOND,
      C-DIE-CODE-FULL
   cpok LBL, ;

\ `adr x10, D`: four words down and then past the name bytes. The displacement
\ is a multiple of four, so only immhi is ever set.
: ENTRY-ADR ( -- )
   9 15 16 ADDI,  9 9 2 LSRI,  9 9 5 LSLI,
   10 $1000000A LIT64,  9 9 10 ORR,
   LCEMIT LABEL@ BL, ;

\ The clause's name bytes at CP, then the record that names them, then CP up to
\ the clause entry. The pad between the two is written zero rather than left:
\ these bytes are copied into the AOT blob verbatim, so they have to be a value
\ and not whatever the last abandoned definition put there.
: MAKE ( -- )
   LBL LBL LBL LBL {: cpy:label cpd:label pad:label pend:label :}
   NAME$
   1 15 0 ADDI,  PROT:RESERVE
   10 CP 0 ADDI,                                       \ the write cursor
   12 13 SUF-LEN SUBI,
   cpy LBL,  12 cpd CBZ,
      9 14 0 LDRB,  9 10 0 STRB,
      14 14 1 ADDI,  10 10 1 ADDI,  12 12 1 SUBI,  cpy B,
   cpd LBL,
   9 $3B MOVZ,  9 10 0 STRB,  10 10 1 ADDI,            \ ';'
   9 $64 MOVZ,  9 10 0 STRB,  10 10 1 ADDI,            \ 'd'
   9 $6F MOVZ,  9 10 0 STRB,  10 10 1 ADDI,            \ 'o'
   9 $65 MOVZ,  9 10 0 STRB,  10 10 1 ADDI,            \ 'e'
   9 $73 MOVZ,  9 10 0 STRB,  10 10 1 ADDI,            \ 's'
   12 15 13 SUB,  9 0 MOVZ,
   pad LBL,  12 pend CBZ,
      9 10 0 STRB,  10 10 1 ADDI,  12 12 1 SUBI,  pad B,
   pend LBL,
   14 NDICT 1 ADDI,  9 DREC MOVZ,  14 14 9 MUL,  14 DBASE 14 ADD,
   1 14 0 ADDI,  2 DREC MOVZ,  PROT:LSPAN LABEL@ BL,
   9 0 MOVZ,                                           \ a slot an abandoned definition may have written
   9 14 0 STR,  9 14 8 STR,  9 14 16 STR,
   9 14 24 STR,  9 14 32 STR,  9 14 40 STR,
   9 CP 15 ADD,  9 14 0 STR,                           \ [0] = the clause entry
   9 DNAME-EXT LIT64,  9 13 9 ORR,  9 14 16 STR,       \ [16] = its name length, out of line
   9 CP 0 ADDI,  9 14 24 STR,                          \ [24] = the name bytes
   11 DATA PEND-CELL LDR,  9 11 40 LDR,  9 14 40 STR,  \ [40] = the parent's wordlist
   CP CP 15 ADD, ;

\ The clause record's own length, by the parent's rule and inside the parent's
\ protection span: both bodies end at the shared epilogue. It runs BEFORE the
\ parent's write, which leaves x9 live for the cache flush.
: FLUSH ( -- )
   LBL {: none:label :}
   9 DATA DOESB-CELL LDR,  9 none CBZ,
   11 NDICT 1 ADDI,  12 DREC MOVZ,  11 11 12 MUL,  11 DBASE 11 ADD,
   1 11 0 ADDI,  2 DREC MOVZ,  PROT:LSPAN LABEL@ BL,
   9 11 0 LDR,  10 CP 9 SUB,  10 10 4 SUBI,  10 11 8 STR,
   none LBL, ;

\ Counted and indexed with its parent, after the record facts the publish pokes
\ into &record[NDICT-1] — which is the parent's slot until this bump moves it.
: PUBLISH ( -- )
   LBL {: none:label :}
   9 DATA DOESB-CELL LDR,  9 none CBZ,
   NDICT NDICT 1 ADDI,  LHIDXADD LABEL@ BL,
   none LBL, ;

;package

: J-DOES ( -- )
   LBL {: dok:label :}
   12 DATA LOCF-CELL LDR,  12 dok CBZ,                    \ does> with locals active: recoverable inside evaluate (rc 75), fail-closed exit 75 at top level. Fires before J-DOES emits any does-patch code; only DOESB/compile-state set -> rollback drops it
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 75 MOVZ,  LCOMPILEDIE LABEL@ B,
   dok LBL,
   9 DATA BODYLEN-CELL LDR,  9 DATA DOESB-CELL STR,
   C-PARSE-CREATED-SIG
   C-EMIT-CRSIG-SET
   DOES-REC:NAME$  DOES-REC:ROOM
   DOES-REC:ENTRY-ADR                    \ adr x10, D = past word 4 and the name bytes
   16 20 DOESP-CELL W-LDRX C-EMITW       \ x16 = LDOESPATCH runtime addr
   $D63F0200 C-EMITW                     \ blr x16
   J-EXIT                                \ word 4: the defining word ends here
   DOES-REC:MAKE                         \ the name bytes, then the clause's record
   9 $D10043FF LIT64,  LCEMIT LABEL@ BL,      \ D: fresh prologue for the does-body
   9 $F90003FE LIT64,  LCEMIT LABEL@ BL, ;

: C-QUALIFY-CAP ( -- )
   LBL {: room :}
   14 DICT-CAP MOVZ,  NDICT 14 CMP,  C-LT room BCOND,
      LDICTFULL C-CAP-LABEL
      $4D C-QUALIFY-FAIL
   room LBL, ;

: C-DUP-DEF-FAIL ( -- )                               \ duplicate definition: recoverable inside evaluate (rc $4E), fail-closed exit $4E at top level
   0 2 MOVZ,  1 LKWDUPDEF LABEL@ ADR,  2 22 MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 DATA DEF-TKA-CELL LDR,  2 DATA DEF-TKL-CELL LDR,  NR-WRITE SYS,
   0 $4E MOVZ,  LCOMPILEDIE LABEL@ B, ;
s" c-dup-def-fail" s" --" TRUST

\ C-DUP-DEF-FAIL and C-REJECT-DUP-DEF emit duplicate-definition rejection.
\ Retirement: habu-builder-trust-rows-c5d41af6.

: C-REJECT-DUP-DEF ( -- )
   LBL LBL LBL LBL LBL LBL LBL LBL {: nloop:label nnext:label ncmp:label nmatch:label nend:label ninl:label done:label nlin:label :}
   14 DATA HIDXP-CELL LDR,  14 nlin CBZ,  C-HIDX-DUP?  13 nmatch CBNZ,  done B,  nlin LBL,  5 DBASE 0 ADDI,  6 NDICT 0 ADDI,
   nloop LBL,
      6 nend CBZ,
      14 5 40 LDR,  15 DATA DEF-WL-CELL LDR,  14 15 CMP,  C-NE nnext BCOND,
      14 5 16 LDR,  14 14 14 LSLI,  14 14 14 LSRI,
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
\ the distinct named code ENGINE-ERROR:SEAL-PACKAGE. Both guards read the REAL friend latch
\ (FRIEND-LATCH-CELL) natively: latch 0 = engine cold load (friend) allows the
\ reserved name; sealed = user source rejects fail-closed. The reserved-name set
\ (RESTAB above) and the A-Z fold are native, NOT checker words: the guards must
\ resolve during the sealed self-hosting stage build and checker-boot recompile,
\ where a checker word is neither reachably kept nor safely callable.
: C-SEAL-PACKAGE-FAIL ( -- )   \ write the offending package token, exit ENGINE-ERROR:SEAL-PACKAGE
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 ENGINE-ERROR:SEAL-PACKAGE MOVZ,  NR-EXIT-GROUP SYS, ;
s" c-seal-package-fail" s" --" TRUST

: C-SEAL-MATCH ( -- )   \ if TKA[0,x24) folds to a reserved name (RESTAB), exit ENGINE-ERROR:SEAL-PACKAGE
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

variable LQUALIFYDEF      \ shared qualification helper entry (dot habu-emit-one-shared)
variable LSTOREDEFNAME    \ shared guarded-name-publication helper entry

\ One shared qualification/publication helper (dot habu-emit-one-shared).
\ Formerly this whole body was inlined at every definer (CREATE/CONSTANT/TRUSTED:/
\ DEFER/colon), copying the qualification scan, package lookup/create path,
\ duplicate wall, capacity guard, name storage, and state restoration into each
\ handler. It is now emitted once at LQUALIFYDEF and reached by branch-with-link.
\ Register ABI: pure DATA-cell (memory) in/out — it reads TKA/TKL/CUR, writes the
\ DEF-* mirror + TKA/TKL. It clobbers the general scratch set (x3-x17); every
\ caller already recomputes those from memory after the call, so nothing is live
\ across it. The one internal branch-with-link (LHIDXADD) clobbers the link
\ register, so the body saves x30 across itself. Fail exits (seal/dup/qualified-
\ name misuse/dict-full) either exit_group or B LCOMPILEDIE, whose recovery
\ restores SP from the eval-frame / REPL snapshot, so the unbalanced frame on a
\ die path is harmless. W^X: emitted into the RW code region like every other
\ primitive, flushed RX with the rest of the image; it takes no data-region
\ store outside the DATA cells its callers already touch.
\ EMIT-QUALIFY-DEF/C-QUALIFY-DEF and EMIT-STORE-DEF-NAME/C-STORE-DEF-NAME
\ centralize qualification and raw dictionary-name publication.
\ Retirement: habu-builder-trust-rows-c5d41af6.
: EMIT-QUALIFY-DEF ( -- )
   LQUALIFYDEF LABEL@ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,                       \ save link register across the internal LHIDXADD BL
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
      14 5 16 LDR,  14 14 14 LSLI,  14 14 14 LSRI,  14 17 CMP,  C-NE nnext BCOND,
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
      1 9 0 ADDI,  2 DREC MOVZ,  PROT:LSPAN LABEL@ BL,   \ the record this publishes
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
   done LBL,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;              \ restore link register + return
s" emit-qualify-def" s" --" TRUST

: C-QUALIFY-DEF ( -- )  LQUALIFYDEF LABEL@ BL, ;      \ call the one shared helper
s" c-qualify-def" s" --" TRUST

\ One shared guarded-name-publication helper (dot habu-emit-one-shared).
\ Formerly inlined at every definer plus EXPORT; now emitted once at
\ LSTOREDEFNAME and reached by branch-with-link.
\ Publish guard (TFAM 2b-v): a new record's WID is DEF-WL-CELL (from CUR-CELL, which
\ a user can redirect with `set-current`, or a resolved package WID). Reject
\ publishing into a protected WID once the friend latch is sealed -- so user source
\ cannot `<protected-wid> set-current : FOO ;` or `: RESULT:BOGUS ;` into a sealed
\ system / generated constructor package. Friend/cold-load (latch 0) is exempt.
\ Register ABI: x9 = record pointer, live in AND out — the body saves it across
\ the internal LPROTWIDQ call (SP-relative) and restores it, then uses it for the
\ [40] wid store; every caller reads x9 after the call, so the save/restore is the
\ preservation contract. The body also saves x30 across the LPROTWIDQ
\ branch-with-link; the protected-publish exit is exit_group, so its unbalanced
\ frame never returns. W^X: no data-region store outside the DATA cells its
\ callers already touch; emitted RW, flushed RX with the image.
: EMIT-STORE-DEF-NAME ( -- )
   LSTOREDEFNAME LABEL@ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,                          \ save link register across the internal LPROTWIDQ BL
   LBL {: pgok:label :}
   14 DATA FRIEND-LATCH-CELL LDR,  14 pgok CBZ,          \ open -> no guard
   SP SP 16 SUBI,  9 SP 0 STR,                           \ save record ptr
   9 DATA DEF-WL-CELL LDR,  LPROTWIDQ LABEL@ BL,         \ x9 = target wid; x13 = protected?
   9 SP 0 LDR,  SP SP 16 ADDI,                           \ restore record ptr
   13 pgok CBZ,                                          \ not protected -> allow
      1 LPROTPUB LABEL@ ADR,  0 2 MOVZ,  2 PROTPUB-MSG-LEN MOVZ,  NR-WRITE SYS,       \ protected + sealed: name the guard on fd 2 before exit 84
      0 2 MOVZ,  1 DATA DEF-TKA-CELL LDR,  2 DATA DEF-TKL-CELL LDR,  NR-WRITE SYS,     \ + the offending def name
      1 LOPENNL LABEL@ ADR,  0 2 MOVZ,  2 1 MOVZ,  NR-WRITE SYS,                       \ + newline
      0 ENGINE-ERROR:SEAL-PACKAGE MOVZ,  NR-EXIT-GROUP SYS,
   pgok LBL,
   C-STORE-NAME
   14 DATA DEF-WL-CELL LDR,  14 9 40 STR,
   11 DATA DEF-TKA-CELL LDR,  11 DATA TKA-CELL STR,
   12 DATA DEF-TKL-CELL LDR,  12 DATA TKL-CELL STR,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;                 \ restore link register + return
s" emit-store-def-name" s" --" TRUST

: C-STORE-DEF-NAME ( -- )  LSTOREDEFNAME LABEL@ BL, ;    \ call the one shared helper
s" c-store-def-name" s" --" TRUST

: EMIT-CREATE ( -- )
   LBL {: nokind :}
   LCREATE LABEL@ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,  15 SP 8 STR,
   1 CP 4 ADDI,  PROT:LOPEN LABEL@ BL,
   LTOK LABEL@ BL,
   12 0 MOVZ,  12 DATA BODYLEN-CELL STR,  LBCAP LABEL@ BL,   \ seed "NAME " for the hook
   C-QUALIFY-DEF
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   1 9 0 ADDI,  2 DREC MOVZ,  PROT:LSPAN LABEL@ BL,   \ the record this create publishes
   C-STORE-DEF-NAME
   CP 9 0 STR,
   11 DATA 0 LDR,
   C-DATA-ADDR
   9 W-RET LIT64,  LCEMIT LABEL@ BL,
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   10 9 0 LDR,  10 CP 10 SUB,  10 10 4 SUBI,  10 9 8 STR,
   10 9 16 LDR,  10 10 DKIND:ADDR ORRI,  10 9 16 STR,     \ this record's body pushes its DATA address
   9 DATA LASTC-CELL STR,
   NDICT NDICT 1 ADDI,  LHIDXADD LABEL@ BL,  9 9 0 LDR,   \ publish record NDICT-1; x9 = body start for the flush
   PROT:LCLOSE LABEL@ BL,  LFLUSH LABEL@ BL,
   15 SP 8 LDR,  15 nokind CBZ,
   LKWCREATE 6 C-DEFHOOK
   nokind LBL,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ Interpret-mode defining-word handlers. The interpret define-keyword dispatch
\ rows reopen this package further down and resolve them bare.
package INTERP-EMIT

: C-CREATE ( -- )
   C-TASK-LIVE-GUARD
   15 1 MOVZ,  LCREATE LABEL@ BL,
   LASTC-TRUST:PUBLISH-PTR-A ;

: C-VARIABLE ( -- )  C-CREATE
   7 DATA 0 LDR,  7 7 8 ADDI,  7 DP-CHECK  7 DATA 0 STR, ;

\ `constant` pops ONE physical cell and records the one-cell `-- a` trust
\ (LASTC-TRUST:PUBLISH-A) — the permanent contract (TFAM 12 verdict 2026-07-09):
\ the interpret stack is untyped, so no shape source exists here, and a
\ wider-than-cell layout value never lands on it (DNAME-WIDE dispatch gate).
\ Parity with verify-source / public-signatures / all-errors is locked by the
\ check-all-errors-test const-layout-narrow fixture.
: C-CONSTANT ( -- )
   C-TASK-LIVE-GUARD
   1 CP 4 ADDI,  PROT:LOPEN LABEL@ BL,  LTOK LABEL@ BL,
   12 0 MOVZ,  12 DATA BODYLEN-CELL STR,  LBCAP LABEL@ BL,   \ seed "NAME " for the hook
   C-QUALIFY-DEF
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   1 9 0 ADDI,  2 DREC MOVZ,  PROT:LSPAN LABEL@ BL,     \ the record this constant publishes
   C-STORE-DEF-NAME
   15 G-POP                                             \ n -> x15 after name storage (clobbers x15)
   CP 9 0 STR,
   11 15 0 ADDI,  C-LIT
   9 W-RET LIT64,  LCEMIT LABEL@ BL,
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   10 9 0 LDR,  10 CP 10 SUB,  10 10 4 SUBI,  10 9 8 STR,
   10 9 16 LDR,  10 10 DKIND:VAL ORRI,  10 9 16 STR,      \ this record's body pushes a decided number
   9 DATA LASTC-CELL STR,
   NDICT NDICT 1 ADDI,  LHIDXADD LABEL@ BL,  9 9 0 LDR,   \ publish record NDICT-1; x9 = body start for the flush
   PROT:LCLOSE LABEL@ BL,  LFLUSH LABEL@ BL,
   LKWCONST 8 C-DEFHOOK
   LASTC-TRUST:PUBLISH-A ;

;package

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
   1 CP 4 ADDI,  PROT:LOPEN LABEL@ BL,
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
   1 9 0 ADDI,  2 DREC MOVZ,  PROT:LSPAN LABEL@ BL,   \ the record this body will publish
   C-STORE-DEF-NAME
   CP 9 0 STR,
   PROT:LCF LABEL@ BL,                                \ the control-flow depth reset below
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

: C-DEFER-DIE-TOKEN ( n -- ) {: rc:n :}               \ boot-integrity backstop: the bare token, then exit rc; recoverable inside evaluate, fail-closed exit rc at top level. Its ONE caller is C-DEFER-FIND-UNSET ($46), which fires before its DP alloc, so the eval-frame / REPL rollback drops only compile-state + region prot. It stays token-only on purpose: the token there is the engine's own `defer-unset` primitive name, not a name the source wrote, so there is no source misuse to explain. The four causes that ARE source misuse each name themselves through DEFER-DIAG below. Distinct from the pre-trust boot-integrity backstops (C-PD-DIE-FULL exit 72, BSEALCAP exit 73) which do NOT route here and STAY hard exits.
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 rc MOVZ,  LCOMPILEDIE LABEL@ B, ;
s" c-defer-die-token" s" n --" TRUST

\ The defer/is misuse causes each get their own line: what went wrong, then the
\ token it went wrong on, then a newline. They used to share C-DEFER-DIE-TOKEN,
\ which wrote the bare token and nothing else - a build that failed here printed
\ `HOOK` and exited 70, with no code, no cause and no hint (dot
\ habu-a-failed-defer-b83bcfa5). The codes were already distinct at the call
\ sites; only the message was missing.
\ Recovery is unchanged by the move: every one of these fires before the defer
\ record is counted into NDICT - C-DEFER pre-publish, and the two
\ C-DEFER-TARGET-META sites before DEFER-META-CELL is written - so an abort
\ inside `evaluate` rolls back compile state and region protection and nothing
\ else, and C-PD-CAPTURE runs only on C-DEFER's success path, after the NDICT
\ bump, so a failure here never leaves a stale pending-table entry.
package DEFER-DIAG

: DIE-HEAD ( label n -- ) {: msg:label len:n :}
   0 2 MOVZ,  1 msg ADR,  2 len MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 2 MOVZ,  1 LOPENNL LABEL@ ADR,  2 1 MOVZ,  NR-WRITE SYS, ;
s" die-head" s" label n --" TRUST

: DIE-MSG ( label n n -- ) {: msg:label len:n rc:n :}
   msg len DIE-HEAD
   0 rc MOVZ,  LCOMPILEDIE LABEL@ B, ;
s" die-msg" s" label n n --" TRUST

public

\ One word per cause, so a die site names the cause and nothing else: a message,
\ its length and its exit code are one fact about that cause and are stated once,
\ here, instead of being spelled out again at every site that can fail.
: DIE-NO-NAME ( -- )                       \ `defer` with no following token
   LDEFNONAME LABEL@ NONAME-LEN $4A DIE-MSG ;
s" die-no-name" s" --" TRUST

: DIE-NO-TARGET ( -- )                     \ `is` with no following token
   LDEFNOTOKEN LABEL@ NOTOKEN-LEN $4A DIE-MSG ;
s" die-no-target" s" --" TRUST

: DIE-NOT-DEFER ( -- )                     \ `is` target exists but is not deferred
   LDEFNOTDEFER LABEL@ NOTDEFER-LEN $4C DIE-MSG ;
s" die-not-defer" s" --" TRUST

\ `is` parses its target and resolves it through the engine's own lookup, which
\ does not consult the packages a `using` imported - so a bare tail that only a
\ used public exports is genuinely not found here, and the reader needs telling
\ that qualifying it is the fix. The advice is checked, not guessed: with a
\ public `defer` in DPKG, `is DPKG:DHOOK` compiles and runs where the bare
\ `is DHOOK` under `using DPKG` dies here. A tail that a global AND a live used
\ public both claim is a different case and already refused - the checker stops
\ it at the reference site with E-USING-SHADOW-GLOBAL (throw 7141), naming both
\ candidates - which is why this dot's repair is the message and not a new rule.
: DIE-NOT-FOUND ( -- )                     \ `is` target resolves to nothing
   LDEFNOTFOUND LABEL@ NOTFOUND-LEN DIE-HEAD
   0 2 MOVZ,  1 LDEFHINT LABEL@ ADR,  2 HINT-LEN MOVZ,  NR-WRITE SYS,
   0 $46 MOVZ,  LCOMPILEDIE LABEL@ B, ;
s" die-not-found" s" --" TRUST

;package

\ This and the following defer rows emit dispatch cells/code/metadata, pending
\ pre-trust capture, IS retargeting, and capacity and misuse failures.
\ Retirement: habu-builder-trust-rows-c5d41af6.

: C-DEFER-FIND-UNSET ( -- )
   LBL {: found :}
   9 LKWDEFERUNSET LABEL@ ADR,  10 11 MOVZ,  LFIND LABEL@ BL,
   13 found CBNZ,
      $46 C-DEFER-DIE-TOKEN
   found LBL, ;
s" c-defer-find-unset" s" --" TRUST

\ The dispatch cell of a deferred word holds an execution token, which is an
\ address in the JIT region, and it lives in the DP heap, which a snapshot
\ persists byte for byte. Declare it here, where the cell is created and its kind
\ is therefore known, so the snapshot writer and loader can move it with the
\ region (dot habu-relocate-persisted-defer-7aa681c4).
: C-DEFER-CELL ( -- )
   C-DEFER-FIND-UNSET
   7 DATA DP-CELL LDR,
   7 7 7 ADDI,  7 7 3 LSRI,  7 7 3 LSLI,
   11 7 0 STR,
   7 DATA DEFER-XT-CELL STR,
   7 7 8 ADDI,  7 DP-CHECK  7 DATA DP-CELL STR,
   9 DATA DEFER-XT-CELL LDR,  SNAP-RELOC:LMARK LABEL@ BL, ;
s" c-defer-cell" s" --" TRUST

: C-DEFER-EMIT-CODE ( -- )
   $D10043FF C-EMITW
   $F90003FE C-EMITW
   11 DATA DEFER-XT-CELL LDR,
   C-DATA-ADDR-RAW                                 \ movz/movk x9 = dispatch-cell addr (relocatable data addr)
   16 9 0 W-LDRX C-EMITW
   C-CALL-BLR-X16 C-EMITW
   $F94003FE C-EMITW
   $910043FF C-EMITW
   W-RET C-EMITW ;
s" c-defer-emit-code" s" --" TRUST

: C-DEFER-META-WRITE ( -- )
   1 16 MOVZ,  PROT:RESERVE                        \ two cells written straight at CP
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

\ --- Pre-trust defer pending registration (dot habu-engine-pre-trust-77410827) ---
\ A `defer NAME ( E )` declared before checker.f defines `trust`/`checker-defer`
\ cannot run DEF-TRUST:REGISTER / C-CALL-CHECKER-DEFER (they LFIND those words and
\ die exit 70). C-DEFER instead copies the defer's qualified name + effect
\ signature into the pending table (src/habu/layout.f PD-*); DRAIN-PRETRUST
\ replays both registrations after `: TRUST`.

: C-PD-COPY ( -- )                                   \ copy x5 bytes [x9..)->[x16..); advances x9/x16, x5->0
   LBL LBL {: top done :}   \ typed-local-lint: allow-bare-local
   top LBL,  5 done CBZ,
      6 9 0 LDRB,  6 16 0 STRB,
      9 9 1 ADDI,  16 16 1 ADDI,  5 5 1 SUBI,  top B,
   done LBL, ;
s" c-pd-copy" s" --" TRUST

: C-PD-DIE-FULL ( -- )                               \ table full or name/sig over cap: name the offending defer token, exit 72
   SP SP 32 SUBI,  9 $2D657270203A6268 LIT64,  9 SP 0 STR,  9 $6564207473757274 LIT64,  9 SP 8 STR,  9 $6C62617420726566 LIT64,  9 SP 16 STR,  9 $203A6C6C75662065 LIT64,  9 SP 24 STR,
   0 2 MOVZ,  1 SP 0 ADDI,  2 32 MOVZ,  NR-WRITE SYS,  SP SP 32 ADDI,
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 2 MOVZ,  1 LQNL LABEL@ ADR,  1 1 1 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
   0 72 MOVZ,  NR-EXIT-GROUP SYS, ;
s" c-pd-die-full" s" --" TRUST

: C-PD-CAPTURE ( -- )                                \ record this defer's (name,sig) into the next pending slot
   LBL LBL LBL {: capok nameok sigok :}   \ typed-local-lint: allow-bare-local
   12 PD-TABLE-OFF LIT64,  12 DATA 12 ADD,           \ x12 = &band
   13 12 0 LDR,  14 PD-CAP MOVZ,  13 14 CMP,  C-LT capok BCOND,
      C-PD-DIE-FULL
   capok LBL,
   15 PD-SLOT MOVZ,  15 13 15 MUL,                   \ x15 = count*PD-SLOT
   14 12 PD-SLOTS-REL ADDI,  14 14 15 ADD,           \ x14 = slot base (survives C-PUSH-DREC-NAME/copies)
   C-PUSH-DREC-NAME                                  \ G: name-addr, name-len (clobbers x9,x10,x12,x13)
   10 G-POP  9 G-POP                                 \ x10=name-len  x9=name-addr
   16 PD-NAME-CAP MOVZ,  10 16 CMP,  C-LS nameok BCOND,
      C-PD-DIE-FULL
   nameok LBL,
   10 14 PD-NLEN-OFF STR,
   16 14 PD-NAME-OFF ADDI,  5 10 0 ADDI,  C-PD-COPY
   9 DATA TSIG-A-CELL LDR,  10 DATA TSIG-U-CELL LDR, \ x9=sig-addr x10=sig-len
   16 PD-SIG-CAP MOVZ,  10 16 CMP,  C-LS sigok BCOND,
      C-PD-DIE-FULL
   sigok LBL,
   10 14 PD-SLEN-OFF STR,
   16 14 PD-SIG-OFF ADDI,  5 10 0 ADDI,  C-PD-COPY
   12 PD-TABLE-OFF LIT64,  12 DATA 12 ADD,           \ reload &band (C-PUSH-DREC-NAME clobbered x12)
   13 12 0 LDR,  13 13 1 ADDI,  13 12 0 STR, ;       \ count++
s" c-pd-capture" s" --" TRUST

: C-PRETRUST-READY? ( -- )                           \ x13 <- both `trust-decl` and `checker-defer` are defined (non-dying)
   LBL {: done :}   \ typed-local-lint: allow-bare-local
   9 KWDATA:LKWTRUSTDECL LABEL@ ADR,  10 10 MOVZ,  LFIND LABEL@ BL,
   13 done CBZ,                                      \ trust-decl absent -> x13=0
   LCHKDEFER 13 C-FIND-GLOBAL?                       \ x13 = checker-defer found? (global scope)
   done LBL, ;
s" c-pretrust-ready?" s" --" TRUST

: C-DEFER ( -- )
   C-TASK-LIVE-GUARD
   LBL {: named :}
   1 CP 4 ADDI,  PROT:LOPEN LABEL@ BL,
   C-DEFER-ROOM
   LTOK LABEL@ BL,  0 named CBNZ,
      DEFER-DIAG:DIE-NO-NAME
   named LBL,
   12 0 MOVZ,  12 DATA BODYLEN-CELL STR,  LBCAP LABEL@ BL,
   C-CLEAR-TRUSTED-STATE
   C-PARSE-REQUIRED-SIG
   C-DEFER-CELL
   C-QUALIFY-DEF
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   9 DATA PEND-CELL STR,
   1 9 0 ADDI,  2 DREC MOVZ,  PROT:LSPAN LABEL@ BL,   \ the record this defer publishes
   C-STORE-DEF-NAME
   CP 9 0 STR,
   C-DEFER-EMIT-CODE
   9 DATA PEND-CELL LDR,
   10 9 0 LDR,  10 CP 10 SUB,  10 9 8 STR,
   C-DEFER-META-WRITE
   NDICT NDICT 1 ADDI,  LHIDXADD LABEL@ BL,
   9 DATA PEND-CELL LDR,  9 9 0 LDR,
   PROT:LCLOSE LABEL@ BL,  LFLUSH LABEL@ BL,
   LBL LBL {: ready pdone :}                          \ typed-local-lint: allow-bare-local — pre-trust defer capability (dot habu-engine-pre-trust-77410827)
   C-PRETRUST-READY?  13 ready CBNZ,
      C-PD-CAPTURE  pdone B,                          \ trust/checker-defer absent: record into the pending table
   ready LBL,
      DEF-TRUST:REGISTER
      C-CALL-CHECKER-DEFER
   pdone LBL,
   EM-REC-WIDE-PUBLISH
   C-CLEAR-TRUSTED-STATE
   9 0 MOVZ,  9 DATA PEND-CELL STR, ;
s" c-defer" s" --" TRUST

: C-DEFER-TARGET-META ( -- )
   LBL LBL LBL {: named found ok :}
   LTOK LABEL@ BL,  0 named CBNZ,
      DEFER-DIAG:DIE-NO-TARGET
   named LBL,
   LBCAP LABEL@ BL,
   9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFIND LABEL@ BL,
   13 found CBNZ,
      DEFER-DIAG:DIE-NOT-FOUND
   found LBL,
   14 11 12 ADD,
   15 14 0 LDR,
   5 DEFER-MAGIC LIT64,
   15 5 CMP,  C-EQ ok BCOND,
      DEFER-DIAG:DIE-NOT-DEFER
   ok LBL,
   14 14 8 ADDI,  14 14 0 LDR,
   14 DATA DEFER-META-CELL STR, ;
s" c-defer-target-meta" s" --" TRUST

\ `is` is the store site for a deferred word's execution token, so it is also the
\ point at which that cell's kind is declared: whatever else the cell has held, it
\ holds a JIT-region address from here on, and a snapshot must move it with the
\ region (dot habu-relocate-persisted-defer-7aa681c4). The declaration is made
\ once per `is`, not once per store, and the table refuses duplicates, so a defer
\ that is re-pointed a thousand times is still one row.
: J-IS ( -- )
   C-DEFER-TARGET-META
   LVSPILL LABEL@ BL,
   C-POP-X16
   11 DATA DEFER-META-CELL LDR,
   9 11 0 ADDI,  SNAP-RELOC:LMARK LABEL@ BL,
   C-DATA-ADDR-RAW                                  \ movz/movk x9 = dispatch-cell addr (relocatable data addr)
   16 9 0 W-STRX C-EMITW ;
s" j-is" s" --" TRUST

\ DRAIN-PRETRUST ( -- ): replay DEF-TRUST:REGISTER + C-CALL-CHECKER-DEFER
\ for every pending pre-trust defer, then empty the table. Called once from
\ src/core/checker.f right after `: TRUST` (the earliest point both `trust-decl`
\ and `checker-defer`:5208 exist). Drains from the top down using the band count
\ itself as the loop state so it survives across the checker BLR calls; each slot
\ supplies the copied name+sig, so no body-buffer/record read is needed at drain
\ time. Registered as a framed prim (BLRs into the checker) via FPRIM below.
: BDRAINPRETRUST ( -- )
   LBL LBL {: loop done :}   \ typed-local-lint: allow-bare-local
   loop LBL,
      12 PD-TABLE-OFF LIT64,  12 DATA 12 ADD,  13 12 0 LDR,  13 done CBZ,   \ remaining==0 -> done
      DEF-TRUST:FIND                                  \ x11 = trust-decl XT (clobbers x12-x16)
      12 PD-TABLE-OFF LIT64,  12 DATA 12 ADD,  13 12 0 LDR,  13 13 1 SUBI,  \ index = remaining-1
      15 PD-SLOT MOVZ,  15 13 15 MUL,  14 12 PD-SLOTS-REL ADDI,  14 14 15 ADD,   \ x14 = slot base (x11 preserved)
      9 14 PD-NAME-OFF ADDI,  9 G-PUSH   9 14 PD-NLEN-OFF LDR,  9 G-PUSH        \ push name addr,len
      9 14 PD-SIG-OFF ADDI,   9 G-PUSH   9 14 PD-SLEN-OFF LDR,  9 G-PUSH        \ push sig addr,len
      C-CALL-X11-SAVED                                \ trust-decl( na nu sa su )
      LCHKDEFER 13 C-FIND-GLOBAL                      \ x11 = checker-defer XT (clobbers x12-x16)
      12 PD-TABLE-OFF LIT64,  12 DATA 12 ADD,  13 12 0 LDR,  13 13 1 SUBI,
      15 PD-SLOT MOVZ,  15 13 15 MUL,  14 12 PD-SLOTS-REL ADDI,  14 14 15 ADD,
      9 14 PD-NAME-OFF ADDI,  9 G-PUSH   9 14 PD-NLEN-OFF LDR,  9 G-PUSH        \ push name addr,len
      C-CALL-X11-SAVED                                \ checker-defer( ptr-u8 n )
      12 PD-TABLE-OFF LIT64,  12 DATA 12 ADD,  13 12 0 LDR,  13 13 1 SUBI,  13 12 0 STR,   \ remaining--
      loop B,
   done LBL, ;
s" bdrainpretrust" s" --" TRUST

\ `immediate` writes ONE flag cell of the record just published and emits nothing,
\ so it is the stateless-flip shape, not a bracket: the target is fixed across the
\ pair and the word owns no other span. It now reads exactly like its two
\ siblings, habu1.f BWIDEMARK and BINTMARK, whose prose already claimed the
\ mirror. Interpret-mode only, so no compile bracket is open around it.
: C-IMMEDIATE ( -- )
   9 NDICT 0 ADDI,  9 9 1 SUBI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   2 3 MOVZ,  LPROTREC LABEL@ BL,
   10 9 16 LDR,  10 10 DNAME-IMM ORRI,  10 9 16 STR,
   2 5 MOVZ,  LPROTREC LABEL@ BL, ;

: C-POSTPONE ( -- )
   LBL LBL LBL {: pok pnimm pdone :}
   LTOK LABEL@ BL,  C-QUALIFY-SEAL-GUARD                 \ reject `postpone RESERVED:tail` once sealed (TFAM 2b-iii)
   9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFIND LABEL@ BL,
   13 pok CBNZ,                                          \ postpone <undefined>: recoverable inside evaluate (rc 70), fail-closed exit 70 at top level. Only LTOK + LFIND ran; nothing published/emitted -> clean rollback
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 70 MOVZ,  LCOMPILEDIE LABEL@ B,
   pok LBL,
   14 13 2 ANDI,  14 pnimm CBZ,
      C-CALL  pdone B,
   pnimm LBL,
      C-CODE-ADDR
      9 LKWCOMPC LABEL@ ADR,  10 8 MOVZ,  LFIND LABEL@ BL,
      C-CALL
   pdone LBL, ;

: C-QUOTE-START ( -- )
   12 DATA INP-CELL LDR,  12 12 1 ADDI,  13 12 0 ADDI, ;

: C-QUOTE-EOF ( -- )                               \ unterminated or bad-escape string literal: label fd 2, then recoverable inside evaluate (rc 74), fail-closed exit 74 at top level. Emitted into the scan/copy runtime routines (LESCSCAN/LESCCOPY) too; those run at compile-time and their local SP save is discarded by the LCOMPILEDIE recovery (SP reset from the catch/REPL boundary). INP not yet consumed at the die -> clean rollback
   1 LBADSTR LABEL@ ADR,  0 2 MOVZ,  2 BADSTR-MSG-LEN MOVZ,  NR-WRITE SYS,
   0 74 MOVZ,  LCOMPILEDIE LABEL@ B, ;

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
variable LSNAPRBD
variable LAOTWIDGATE   \ AOT boot sealed-WID reject routine (TFAM 2b-v)
variable LAOTPROT      \ cold-start baked protected-WID restore

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

\ ---- top-row hook events (dot habu-typed-top-engine-2b2e88aa) --------------
\ With a hook installed through `set-top-check` (layout.f TOP-HOOK-CELL), each
\ interpret-dispatch class emits one event through the shared LTOPHOOK routine
\ (EMIT-TOPHOOK): x15 = TOP-EV-* class, x16 = LFIND flags (0 for literals);
\ LTOPHOOK adds the TKA/TKL token bytes and runs the hook. The site guard is
\ one load + CBZ, so the uninstalled path is today's dispatch unchanged.
variable LTOPHOOK

: C-TOPHOOK-LIT ( n -- ) {: class:n :}     \ literal event: flags 0
   LBL {: nohk:label :}
   9 DATA TOP-HOOK-CELL LDR,  9 nohk CBZ,
   15 class MOVZ,  16 0 MOVZ,
   LTOPHOOK LABEL@ BL,
   nohk LBL, ;

: C-TOPHOOK-FLAGS ( n -- ) {: class:n :}   \ word/tick event: flags from x13 (LFIND)
   LBL {: nohk:label :}
   9 DATA TOP-HOOK-CELL LDR,  9 nohk CBZ,
   15 class MOVZ,  16 13 0 ADDI,
   LTOPHOOK LABEL@ BL,
   nohk LBL, ;

: C-TOPHOOK-CALL ( -- )                    \ pre-BLR word event: x11 (xt) preserved
   LBL {: nohk:label :}
   9 DATA TOP-HOOK-CELL LDR,  9 nohk CBZ,
   SP SP 16 SUBI,  11 SP 0 STR,
   15 TOP-EV-WORD MOVZ,  16 13 0 ADDI,
   LTOPHOOK LABEL@ BL,
   11 SP 0 LDR,  SP SP 16 ADDI,
   nohk LBL, ;

\ LTOPHOOK: shared top-row event trampoline, BL-called from the guarded event
\ sites. Entry: x15 = class, x16 = flags, hook known installed. Pushes
\ ( token-a token-u class flags ) and BLRs the hook word — the hook consumes
\ exactly those four cells; a hook throw unwinds through BTHROW like any
\ executed word's. Clobbers x9-x17.
: EMIT-TOPHOOK ( -- )
   LTOPHOOK LABEL@ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   9 DATA TKA-CELL LDR,  9 G-PUSH
   9 DATA TKL-CELL LDR,  9 G-PUSH
   15 G-PUSH
   16 G-PUSH
   9 DATA TOP-HOOK-CELL LDR,
   9 BLR,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

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
   15 G-PUSH  10 G-PUSH
   TOP-EV-STR C-TOPHOOK-LIT ;

: C-ICQ ( -- )
   C-QUOTE-START
   C-QUOTE-SCAN
   C-QUOTE-CONSUME
   LBL LBL LBL {: capok cl cd :}
   10 255 CMPI,  C-LE capok BCOND,  LCSTR LABEL@ B,   \ c" over 255 bytes: named fd-2 label (LCSTR) then rc 76 via LCOMPILEDIE -- recoverable inside evaluate, fail-closed exit 76 at top level. Fires before the DP copy -> clean rollback
   capok LBL,
   12 DATA 0 LDR,  15 12 0 ADDI,                       \ x15 = counted string base
   14 12 10 ADD,  14 14 1 ADDI,  14 DP-CHECK
   10 12 0 STRB,  12 12 1 ADDI,
   11 13 0 ADDI,  9 10 0 ADDI,
   cl LBL,  9 cd CBZ,
      14 11 0 LDRB,  14 12 0 STRB,  12 12 1 ADDI,  11 11 1 ADDI,  9 9 1 SUBI,  cl B,
   cd LBL,
   12 DATA 0 STR,
   15 G-PUSH
   TOP-EV-CSTR C-TOPHOOK-LIT ;

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
   11 G-PUSH  10 G-PUSH
   TOP-EV-STR C-TOPHOOK-LIT ;

: C-EICQ ( -- )
   LBL {: capok:label :}
   C-QUOTE-START
   C-ESC-QUOTE-SCAN
   10 255 CMPI,  C-LE capok BCOND,  LCSTR LABEL@ B,   \ c\" over 255 bytes: named fd-2 label (LCSTR) then rc 76 via LCOMPILEDIE -- recoverable inside evaluate, fail-closed exit 76 at top level. Fires before C-ESC-QUOTE-CONSUME (INP not consumed) -> clean rollback
   capok LBL,
   C-ESC-QUOTE-CONSUME
   11 16 0 ADDI,  12 16 15 ADD,
   17 DATA 0 LDR,
   14 17 10 ADD,  14 14 1 ADDI,  14 DP-CHECK
   10 17 0 STRB,  17 17 1 ADDI,
   C-ESC-COPY-X17
   17 DATA 0 STR,  11 17 10 SUB,  11 11 1 SUBI,
   11 G-PUSH
   TOP-EV-CSTR C-TOPHOOK-LIT ;

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
   9 DATA TKA-CELL LDR,  9 9 0 LDRB,  9 G-PUSH
   TOP-EV-CHAR C-TOPHOOK-LIT ;

: C-BCHAR ( -- )
   LTOK LABEL@ BL,  LBCAP LABEL@ BL,
   11 DATA TKA-CELL LDR,  11 11 0 LDRB,  LVPUSHC LABEL@ BL, ;

: C-TICK ( -- )
   LBL LBL LBL {: tk:label usedtry:label found:label :}
   LTOK LABEL@ BL,  C-QUALIFY-SEAL-GUARD                 \ reject `' RESERVED:tail` once sealed (TFAM 2b-iii)
   9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFIND LABEL@ BL,
   13 usedtry CBZ,                                       \ open-scope + global miss -> try used publics (` ' SUITE` under a `using`)
   found LBL,
   14 13 8 ANDI,  14 LWIDE LABEL@ CBNZ,                  \ `' <wide-effect word>` would launder the bundle past the dispatch gate
   14 13 16 ANDI,  14 LINTERNAL LABEL@ CBNZ,             \ `' <internal word>` would launder the xt past the dispatch gate (execute)
   11 G-PUSH
   TOP-EV-TICK C-TOPHOOK-FLAGS
   tk B,
   usedtry LBL,
      9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFINDUSED LABEL@ BL,
      13 tk CBZ,                                         \ undefined `' X` stays the pre-existing no-op
      found B,
   tk LBL, ;

: C-BTICK ( -- )
   LBL {: bk :}
   LTOK LABEL@ BL,  C-QUALIFY-SEAL-GUARD                 \ reject `['] RESERVED:tail` once sealed (TFAM 2b-iii)
   9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFIND LABEL@ BL,
   13 bk CBZ,  C-CODE-ADDR  bk LBL, ;

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
variable LP2NEST
package LOWER-TXN-CODE
public
variable BAD  variable MEM  variable FULL  variable DRIFT
variable VDESC  variable DRIFT-FAIL
4095 constant MAX-WIDTH

: FAIL ( ptr a n -- ) {: msg:ptr len:n :}
   0 2 MOVZ,  1 msg LABEL@ ADR,  2 len MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 LQNL LABEL@ ADR,  1 1 1 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
   0 76 MOVZ,  NR-EXIT-GROUP SYS, ;
;package

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

\ Locals rows reject quotation-scoped declarations and emit local-reference loads;
\ the control dispatcher executes data-driven emitter xts.
\ Retirement: habu-builder-trust-rows-c5d41af6.
: C-LBRACE-DIE ( -- )   \ B2: locals opener inside a quotation: recoverable inside evaluate (rc $4B), fail-closed exit $4B at top level
   1 LBADLOC LABEL@ ADR,  0 2 MOVZ,  2 $27 MOVZ,  NR-WRITE SYS,
   0 $4B MOVZ,  LCOMPILEDIE LABEL@ B, ;
s" c-lbrace-die" s" --" TRUST

: C-LBRACE-GUARDS ( -- )
   LBL {: qlok:label :}
   11 DATA QPATCH-CELL LDR,  11 qlok CBZ,
      C-LBRACE-DIE
   qlok LBL, ;

: C-LBRACE-STORE-ONE ( -- )
   LBL LBL LBL LBL LBL {: nlok ncp ncd tsl tsd :}
   11 DATA LOCN-CELL LDR,  11 $40 CMPI,  C-LT nlok BCOND,   \ more than 64 locals in one definition: recoverable inside evaluate (rc $4B), fail-closed exit $4B at top level. Only LOCN/LOCNAMES compile-state mutated (EM-RESET-COMPILE-STATE zeroes LOCN) -> clean rollback
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 $4B MOVZ,  LCOMPILEDIE LABEL@ B,
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
   1 10 0 ADDI,  PROT:RESERVE                      \ the literal's bytes go straight at CP
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
   10 255 CMPI,  C-LE capok BCOND,  LCSTR LABEL@ B,   \ c" over 255 bytes (compile mode): named fd-2 label (LCSTR) then rc 76 via LCOMPILEDIE -- recoverable inside evaluate, fail-closed exit 76 at top level. Fires before C-QUOTE-CONSUME-DONE (INP not consumed, nothing emitted) -> clean rollback
   capok LBL,
   C-QUOTE-CONSUME-DONE
   C-QUOTE-SAVE
   C-QUOTE-RESTORE
   11 16 0 ADDI,  12 10 1 ADDI,  LBCS LABEL@ BL,
   15 CP 0 ADDI,  9 $14000000 LIT64,  LCEMIT LABEL@ BL,
   12 CP 0 ADDI,
   C-QUOTE-RESTORE
   1 10 1 ADDI,  PROT:RESERVE                      \ count byte + the literal's bytes, at CP
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
   1 10 0 ADDI,  PROT:RESERVE                      \ the decoded bytes go straight at CP
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
   10 255 CMPI,  C-LE capok BCOND,  LCSTR LABEL@ B,   \ c\" over 255 bytes (compile mode): named fd-2 label (LCSTR) then rc 76 via LCOMPILEDIE -- recoverable inside evaluate, fail-closed exit 76 at top level. Fires before C-ESC-QUOTE-CONSUME (INP not consumed) -> clean rollback
   capok LBL,
   C-ESC-QUOTE-CONSUME
   C-ESC-QUOTE-SAVE
   C-ESC-QUOTE-RESTORE
   11 16 0 ADDI,  12 15 1 ADDI,  LBCS LABEL@ BL,
   9 $14000000 LIT64,  LCEMIT LABEL@ BL,
   13 CP 0 ADDI,
   C-ESC-QUOTE-RESTORE
   1 10 1 ADDI,  PROT:RESERVE                      \ count byte + the decoded bytes, at CP
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
variable LEVCORRUPT  variable LEVCORRUPTMSG   \ eval-cross forged/corrupt handler-frame fail-closed path + its message
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
   11 DATA QPATCH-CELL LDR,  11 CLOC-QOK LABEL@ CBZ,        \ local referenced inside a quotation: recoverable inside evaluate (rc 75), fail-closed exit 75 at top level
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 75 MOVZ,  LCOMPILEDIE LABEL@ B,
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
   LBL LBL {: rok:label rvok:label :}
   \ Hint the region at the runtime-discovered __text base (XREG-RBASE/x20) + REGION-OFF
   \ and then take whatever base the kernel gives back.
   \ It used to demand that exact address and exit 78 otherwise, because a snapshot
   \ image's region-to-text call displacements were baked in by the writing run and only
   \ survived if the reading run reproduced the same distance. That was never reliable:
   \ the address is not ours to reserve. Measured on macOS 26.5.1 over 200 bare runs of a
   \ snapshot image, the runtime places about 1.1 MiB of its own mappings at a randomised
   \ offset after the loaded image -- observed starts +0xE90000 through +0x1214000 against
   \ a hint at about +0x100B000 -- so the hint was occupied in 182 of the 200 runs and the
   \ engine refused to start. No fixed offset from __text avoids that, because the cluster
   \ simply re-straddles the new one.
   \ Those displacements are now relocated instead (SNAP-RELOC:EMIT-CALLS), so the base
   \ no longer has to be any particular value -- it only has to be near enough to __text
   \ that BL can reach, which the assertion below still requires and which is a property of
   \ the address rather than a wish about it.
   \ We ask for PROT-PAGE-MAX extra bytes and round the base up, so the region base keeps
   \ the 64 KiB alignment LPROT flips protection at even when the kernel moves us to an
   \ address that is only page aligned.
   6 REGION-OFF LIT64,  6 XREG-RBASE 6 ADD,  9 PROT-PAGE-MAX 1 - MOVZ,  6 6 9 ADD,  6 6 16 LSRI,  6 6 16 LSLI,   \ x6 = 64KB-aligned hint
   0 6 0 ADDI,  1 REGION PROT-PAGE-MAX + LIT64,  2 3 MOVZ,  3 MAP-ANON-PRIVATE LIT64,  4 0 MOVN,  5 0 MOVZ,
   NR-MMAP SYS,
   \ SYS, leaves the same syscall-error convention on both targets -- Darwin sets the
   \ carry flag, and the Linux emitter reconciles -errno into it -- so a failed mapping
   \ is read from the flag the kernel set, not guessed from the shape of x0. Name the
   \ fault on fd 2 before exit 78, exactly as the DATA region does.
   C-CC rok BCOND,
      1 LMMAPCODE LABEL@ ADR,  0 2 MOVZ,  2 MMAPCODE-MSG-LEN MOVZ,  NR-WRITE SYS,
      0 78 MOVZ,  NR-EXIT-GROUP SYS,
   rok LBL,
   9 PROT-PAGE-MAX 1 - MOVZ,  0 0 9 ADD,  0 0 16 LSRI,  0 0 16 LSLI,   \ x0 = 64KB-aligned region base inside the mapping
   \ Boot BL-range assertion (permanent guarantee stage C's direct BL relies on): the
   \ whole region must lie within +/-128 MiB of __text. region_end - __text base is the
   \ farthest displacement; die named if it reaches BL-REACH. x0 (region base) survives.
   9 REGION LIT64,  9 0 9 ADD,  9 9 XREG-RBASE SUB,
   5 BL-REACH LIT64,  9 5 CMP,
   C-CC rvok BCOND,
      1 LBLRANGE LABEL@ ADR,  0 2 MOVZ,  2 BLRANGE-MSG-LEN MOVZ,  NR-WRITE SYS,
      0 BL-RANGE-RC MOVZ,  NR-EXIT-GROUP SYS,
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
   9 5 LAOTCODE LABEL@ TADR,  12 0 MOVZ,            \ x9 = blob src (__text), x12 = i
   acopy LBL,  12 11 CMP,  C-GE adone BCOND,
      3 9 12 ADD,  3 3 0 LDRB,  4 CP 12 ADD,  3 4 0 STRB,
      12 12 1 ADDI,  acopy B,
   adone LBL, ;

\ Register the LAOTNREC baked records at &dict[NDICT], rebasing each [0] xt from a
\ blob offset to CP+offset, and hash-indexing it (LHIDXADD). All records first, so
\ EM-AOT-PATCH-SITES can resolve sibling calls by name.
\ Each source record is a compact 20 bytes (word0 = blob-off-or-package-public u32;
\ word1 = code-len-or-package-private u32; word2 = name-off u32; word3 = flags u8 |
\ min-in u8<<8; word4 = wid u32). Expand it to the full 48B dict record, rebasing
\ ordinary [0] to CP+blob-off while package [0]/[8] remain raw u32 WID roles, and reconstructing [16]
\ flags|len, the [24..40) inline name (from the deduped LAOTNAMES pool, zero-padded),
\ and [40] wid (full u32 so wordlist IDs above 255 survive) -- the EXACT inverse of
\ the build-time ACAP-COMPACT-RECS, proven byte-identical to the source-compiled
\ record by ACAP-PROVE-RECS. Every wid is rebased out of the capture's window and
\ into this engine's space first (AOT-WINDOW:REBASE-WID,), and WIDN moves once, past
\ the whole window, after the loop.
\ x2..x7 are LHIDXADD's saved set; x9/x11/x12 survive it. Records are 4B-aligned so
\ each 32-bit word loads with LDRW.
package AOT-WINDOW
public

\ Rebase one captured wid in xw into the wid space of the engine being booted, or
\ branch to `bad`. A captured wid is a WINDOW coordinate: 0 is the global
\ wordlist, which means the same thing in every process and passes through, and
\ every other one must lie in [W0, W0+span) and becomes WIDN + (wid - W0). WIDN is
\ read per wid because the pass advances it exactly once, after the loop, so every
\ read in the loop answers the same number: the target's base.
: REBASE-WID, ( n n n label -- ) {: w:n s1:n s2:n bad:label :}
   LBL {: zero:label :}
   w zero CBZ,
   s1 s2 LWIDW0 LABEL@ TADR,  s1 s1 0 LDR,
   w w s1 SUB,                                   \ window-relative
   s1 s2 LWIDSPAN LABEL@ TADR,  s1 s1 0 LDR,
   \ One unsigned test refuses both sides: a wid below the base wraps high.
   w s1 CMP,  C-CS bad BCOND,
   s1 DATA WIDN-CELL LDR,
   w w s1 ADD,
   zero LBL, ;

\ Seal the wordlists the window sealed, now that the rebase has said where they
\ landed. EMIT-AOT-PROT-RESTORE cannot carry these: it runs before the cold prefix
\ and so before the base they are relative to exists.
: SEAL-WIDS, ( label -- ) {: bad:label :}
   LBL LBL {: ploop:label pdone:label :}
   3 4 LNPWIN LABEL@ TADR,  3 3 0 LDR,          \ x3 = row count
   7 0 MOVZ,                                    \ x7 = i
   ploop LBL,  7 3 CMP,  C-GE pdone BCOND,
      4 5 LPWIN LABEL@ TADR,
      6 7 2 LSLI,  4 4 6 ADD,  4 4 0 LDRW,       \ x4 = window-relative WID
      5 DATA WIDN-CELL LDR,  4 4 5 ADD,          \ x4 = this engine's WID for it
      5 PROT-WID-MAX MOVZ,  4 5 CMP,  C-CS bad BCOND,
      DATA 4 5 6 2 ENGINE-EMIT:PROT-BITS-ADDR,   \ x5 = &word, x6 = mask
      2 5 LDAR,  2 2 6 ORR,  2 5 STLR,           \ release-publish the set bit
      7 7 1 ADDI,  ploop B,
   pdone LBL, ;

;package

: EM-AOT-REGISTER-RECS ( -- )
   LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL
   {: rloop:label rdone:label pkg:label fields:label nloop:label ndone:label
      pkg-wid:label next:label extname:label nameok:label
      bad:label msg:label done:label :}
   9 5 LAOTDICT LABEL@ TADR,  12 0 MOVZ,           \ x9 = compact record src (AOT-CREC-ROW stride), x12 = k
   11 5 LAOTNREC LABEL@ TADR,  11 11 0 LDR,         \ x11 = N (survives LHIDXADD)
   \ T0, before a single wid is handed out: the base every window coordinate below
   \ rebases against, and the number the sealed-WID gate reads on all three passes
   \ to tell a wordlist this seed created from one the engine already had. Latched
   \ here rather than recomputed there, because WIDN stops answering it the moment
   \ a boot-run entry word allocates a wordlist of its own.
   5 DATA WIDN-CELL LDR,  5 DATA AOT-WINDOW:T0-CELL STR,
   \ N records in ONE bracket, from the watermark upward: the seed knows the whole
   \ extent before the loop starts and says so once, instead of declaring a record
   \ per iteration. This is the writer that makes "a fixed window of pages around
   \ &dict[NDICT]" unusable as a rule.
   1 DREC MOVZ,  1 NDICT 1 MUL,  1 DBASE 1 ADD,
   2 DREC MOVZ,  2 11 2 MUL,
   PROT:LSPAN LABEL@ BL,
   rloop LBL,  12 11 CMP,  C-GE rdone BCOND,
      10 DREC MOVZ,  10 NDICT 10 MUL,  10 DBASE 10 ADD,   \ x10 = &dict[NDICT]
      6 9 16 LDRW,  5 $FFFFFFFF LIT64,  6 5 CMP,  C-EQ pkg BCOND,
      3 9 0 LDRW,  3 CP 3 ADD,  3 10 0 STR,         \ ordinary [0] = CP + blob-off u32
      3 9 4 LDRW,  3 10 8 STR,                      \ ordinary [8] = code length u32
      fields B,
      pkg LBL,
      3 9 0 LDRW,  3 4 5 bad AOT-WINDOW:REBASE-WID,  3 10 0 STR,   \ package [0] = public WID
      3 9 4 LDRW,  3 4 5 bad AOT-WINDOW:REBASE-WID,  3 10 8 STR,   \ package [8] = private WID
      fields LBL,
      4 9 8 LDRW,                                   \ x4 = word2 = name-off u32
      7 5 LAOTNAMES LABEL@ TADR,  4 7 4 ADD,        \ x4 = pool entry ptr (len byte)
      5 4 0 LDRB,                                   \ x5 = name length = pool[entry]
      6 9 12 LDRW,                                  \ x6 = word3 = flags | min-in<<8
      7 6 $FF ANDI,                                 \ x7 = flags
      7 7 60 LSLI,  7 7 5 ORR,                      \ flags<<60 | len
      3 6 8 LSRI,  3 3 $FF ANDI,  3 3 52 LSLI,      \ min-in byte = word3>>8 -> DNAME-MIN-IN bits 52-59
      7 7 3 ORR,
      3 6 16 LSRI,  3 3 3 ANDI,  3 3 50 LSLI,       \ definer-kind byte = word3>>16 -> DKIND bits 50-51
      7 7 3 ORR,  7 10 16 STR,                      \ [16] = flags<<60 | min-in<<52 | dkind<<50 | len
      2 0 MOVZ,  2 10 24 STR,  2 10 32 STR,         \ zero [24..40)
      4 4 1 ADDI,                                   \ x4 = name src (entry+1)
      \ A name past DNAME-INL does not fit the record, and the engine's own
      \ definer keeps such a name out of line with [24] pointing at its bytes
      \ (C-STORE-NAME). The pool entry in __text IS those bytes and outlives the
      \ boot, so the record points there -- exactly how the baked cold-prefix
      \ records carry their long names (ENGINE-EMIT:EMIT-DICT + EM-SEED-DICT).
      \ The snapshot walk already knows this shape: LSNAPRBD's text-band pass
      \ folds a dictionary [24] that lands in __text to a canonical 0 on the way
      \ out and back to the live text base on the way in.
      6 6 2 ANDI,  6 extname CBNZ,                  \ word3 flags & DNAME-EXT
      3 0 MOVZ,                                     \ x3 = i
      nloop LBL,  3 5 CMP,  C-GE ndone BCOND,
         2 4 3 ADD,  2 2 0 LDRB,                    \ x2 = name[i]
         7 10 24 ADDI,  7 7 3 ADD,  2 7 0 STRB,     \ dict[24+i] = name[i]
         3 3 1 ADDI,  nloop B,
      ndone LBL,  nameok B,
      extname LBL,  4 10 24 STR,                    \ [24] = the pool entry's bytes
      nameok LBL,
      6 9 16 LDRW,  5 $FFFFFFFF LIT64,  6 5 CMP,  C-EQ pkg-wid BCOND,
      6 4 5 bad AOT-WINDOW:REBASE-WID,
      6 10 40 STR,                                  \ ordinary [40] wid = word4 (full u32, hi=0)
      next B,
      pkg-wid LBL,
      6 0 MOVN,  6 10 40 STR,                       \ package marker is signed -1, not a wid
      next LBL,
      NDICT NDICT 1 ADDI,  LHIDXADD LABEL@ BL,      \ publish + index (x9/x11/x12 preserved)
      9 9 AOT-CREC-ROW ADDI,  12 12 1 ADDI,  rloop B,
   rdone LBL,
   bad AOT-WINDOW:SEAL-WIDS,                          \ still WIDN = the target's base
   \ WIDN = base + span, once. The span and not the highest wid registered: a
   \ window may allocate a wordlist no record names, and a later allocation must
   \ not be handed one of those either.
   4 5 AOT-WINDOW:LWIDSPAN LABEL@ TADR,  4 4 0 LDR,
   5 DATA WIDN-CELL LDR,  4 4 5 ADD,  4 DATA WIDN-CELL STR,
   done B,
   bad LBL,
      1 msg ADR,  0 2 MOVZ,  2 39 MOVZ,  NR-WRITE SYS,
      0 ENGINE-ERROR:AOT-SEED MOVZ,  NR-EXIT-GROUP SYS,
   msg LBL,  s" hb: AOT wid outside the capture window" BYTES,  NL-KW 1 BYTES,
   done LBL, ;

\ Validate and restore the baked protected-WID bitmap (TFAM 2b-v) immediately
\ after cold startup clears the live band, before the cold prefix can register its
\ constructor families. Warm snapshot startup skips both clear and restore.
\ Validation is two facts, because the bitmap shape carries the rest: the baked
\ frame must be tagged a bitmap, and bit 0 must be clear, since WID 0 is not a
\ wordlist. The row array this replaced needed a bound check and an O(rows^2)
\ duplicate scan -- a bit has one index and the index cannot leave
\ [0, PROT-WID-MAX).
\ Then copy the fixed PROT-BITS-BYTES blob into the friend-arena band (direct STR
\ into the sealed band -- the AOT seed pass is trusted boot machinery), advance
\ WIDN past the highest protected WID so a post-restore wordlist/package
\ allocation cannot reuse one, and release-publish PROT-REG-TAG last so nothing
\ observes a half-copied band as a bitmap. The blob is a fixed-size image of a
\ SET, so the restored bytes do not depend on the order the writing run protected
\ its WIDs -- which is what lets install --force reach a byte-identical fixpoint
\ across the table-era/bitmap-era changeover.
: EMIT-AOT-PROT-RESTORE ( -- )
   LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL
   {: cloop:label cdone:label sloop:label sdone:label shi:label
      bloop:label bdone:label tagpub:label bad:label msg:label :}
   LAOTPROT LABEL@ LBL,
   11 5 LAOTNPWID LABEL@ TADR,  11 11 0 LDR,
   5 PROT-REG-TAG LIT64,  11 5 CMP,  C-NE bad BCOND,  \ the baked frame must be a bitmap
   9 5 LAOTPWID LABEL@ TADR,                        \ x9  = baked bitmap src
   2 9 0 LDR,  2 2 1 ANDI,  2 bad CBNZ,             \ WID 0 is never a wordlist
   10 PROT-BITS-OFF MOVZ,  10 DATA 10 ADD,          \ x10 = &band[0] (offset > imm12: materialize + add)
   11 PROT-BITS-BYTES MOVZ,                         \ x11 = bytes left
   cloop LBL,  11 cdone CBZ,
      3 9 0 LDR,  3 10 0 STR,
      9 9 8 ADDI,  10 10 8 ADDI,  11 11 8 SUBI,  cloop B,
   cdone LBL,
   \ WIDN = max(WIDN, highest protected WID + 1). Scan words downward for the top
   \ non-zero one, then shift its bits out to find the highest index within it.
   10 PROT-BITS-OFF MOVZ,  10 DATA 10 ADD,
   11 PROT-BITS-BYTES MOVZ,
   sloop LBL,  11 sdone CBZ,
      11 11 8 SUBI,
      3 10 11 ADD,  3 3 0 LDR,
      3 shi CBNZ,
      sloop B,
   sdone LBL,  tagpub B,                            \ nothing protected: leave WIDN alone
   shi LBL,
      4 11 3 LSLI,                                  \ x4 = WID of bit 0 of that word
      5 0 MOVZ,
      bloop LBL,  3 bdone CBZ,
         3 3 1 LSRI,  5 5 1 ADDI,  bloop B,
      bdone LBL,                                    \ x5 = highest set bit index + 1
      4 4 5 ADD,                                    \ x4 = highest protected WID + 1
      5 DATA WIDN-CELL LDR,  4 5 CMP,  C-LS tagpub BCOND,
         4 DATA WIDN-CELL STR,
   tagpub LBL,
   5 PROT-REG-TAG-CELL MOVZ,  5 DATA 5 ADD,
   4 PROT-REG-TAG LIT64,
   4 5 STLR,                                        \ release-publish the shape tag last
   RET,
   bad LBL,
      1 msg ADR,  0 2 MOVZ,  2 30 MOVZ,  NR-WRITE SYS,
      0 ENGINE-ERROR:AOT-SEED MOVZ,  NR-EXIT-GROUP SYS,
   msg LBL,
      s" hb: AOT protected-WID co" BYTES,
      $00000A7470757272 DCQ, ;                      \ "rrupt\n" + two unwritten pad bytes

\ Validate the baked name pool before the AOT seed reads it.
: EM-AOT-VALIDATE ( label -- ) {: bad:label :}
   LBL LBL
   {: pool-loop:label pool-done:label :}
   21 5 LAOTNAMESLEN LABEL@ TADR,  21 21 0 LDR,
   5 AOT-NAMES-CAP LIT64,  21 5 CMP,  C-HI bad BCOND,
   22 5 LAOTNAMES LABEL@ TADR,
   4 0 MOVZ,
   pool-loop LBL,
      4 21 CMP,  C-GE pool-done BCOND,
      5 22 4 ADD,  2 5 0 LDRB,
      4 4 1 ADDI,  4 4 2 ADD,
      4 21 CMP,  C-HI bad BCOND,
      pool-loop B,
   pool-done LBL,
      4 21 CMP,  C-NE bad BCOND, ;

\ For each baked call site (blob-off u32, name-off u32 into the deduped
\ [len][bytes] name pool at LAOTNAMES, callee scope u32) resolve the callee in
\ THIS engine and re-encode ONLY the BL imm26 at CP+blob-offset to that address
\ (disp = callee - site, in BL range by the boot region assertion); the BL opcode
\ (the callee is named, not embedded) is preserved. A missing name is a build/seed
\ inconsistency: fail closed. Rows are 4B-aligned so each field loads with a
\ single LDRW.
\ THE SCOPE IS WHAT MAKES THE NAME AN IDENTITY. A bare name is not one - the
\ compiler chain puts SLOT@ in five wordlists and in none of them globally, so a
\ global lookup answered "absent" for 82% of its call sites (dot 9d7d8e72). Four
\ scopes and no more: a wid below FIRST-DYNAMIC-WID is a layout constant and means
\ the same wordlist here; a window coordinate becomes T0 + (wid - W0), the rebase
\ the records take; WID-QUAL says the pooled name is qualified and LFIND's own
\ qualifier path resolves it; anything else is refused by name.
\ AND EVERY CALLEE GOES THROUGH THE SEALED-WID GATE, unconditionally. This pass
\ used to skip it for a wordlist the seed had just created, which was the right
\ verdict reached in the wrong place: a rule about which callees may be wired is
\ the gate's, and splitting it between a caller-side skip and a callee-side test
\ is how the gate came to be stated more broadly than the seal it protects.
\ EM-AOTWIDGATE now carries that clause itself, as the first of its four, so all
\ three baked-name passes decide alike.
\ The AOT seed writes call immediates straight into the region, so it is the second
\ producer of region-to-text calls and records its sites in the same map as
\ EMIT-CEMITBL, by the same region-extent test. It runs from EM-COMPILE-EXIT, after
\ the DATA region is mapped, so the map is writable here.
: EM-AOT-PATCH-SITES ( -- )
   LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL
   s" hb: AOT call site unresolved"  s" hb: AOT call site names a wordlist outside the window"
   {: ploop:label pdone:label pnf:label pnomark:label
      pqual:label pscope:label pgate:label pbad:label
      nfmsg:label badmsg:label
      nfa nfu bada badu :}                           \ typed-local-lint: allow-bare-local
   21 5 LAOTSITES LABEL@ TADR,                       \ x21 = row cursor
   23 5 LAOTNSITE LABEL@ TADR,  23 23 0 LDR,         \ x23 = site count M
   22 0 MOVZ,                                        \ x22 = site index
   ploop LBL,  22 23 CMP,  C-GE pdone BCOND,
      24 21 0 LDRW,                                  \ x24 = blob offset u32 (survives the lookup)
      4 21 4 LDRW,                                   \ x4 = name-off u32
      5 10 LAOTNAMES LABEL@ TADR,  9 5 4 ADD,        \ x9 = pool entry ptr = LAOTNAMES + name-off
      10 9 0 LDRB,                                   \ x10 = name length = pool[entry]
      9 9 1 ADDI,                                    \ x9 = name ptr = entry + 1
      25 21 8 LDRW,                                  \ x25 = callee scope u32 (survives the lookup)
      5 WID-QUAL LIT64,  25 5 CMP,  C-EQ pqual BCOND,
      25 FIRST-DYNAMIC-WID CMPI,  C-CC pscope BCOND, \ layout constant: this number in every engine
      6 7 AOT-WINDOW:LWIDW0 LABEL@ TADR,  6 6 0 LDR,
      25 25 6 SUB,                                   \ window-relative
      6 7 AOT-WINDOW:LWIDSPAN LABEL@ TADR,  6 6 0 LDR,
      25 6 CMP,  C-CS pbad BCOND,                    \ one unsigned test refuses both sides
      7 DATA WIDN-CELL LDR,  7 7 6 SUB,              \ x7 = T0; the records pass moved WIDN past the window
      25 25 7 ADD,
   pscope LBL,
      0 9 0 ADDI,  1 10 0 ADDI,  2 25 0 ADDI,
      WLFIND:LENTRY LABEL@ BL,                       \ x11 = xt or 0, x12 = the row
      11 pnf CBZ,
      5 12 0 ADDI,                                   \ the row the gate asks about
      pgate B,
   pqual LBL,
      LFIND LABEL@ BL,                               \ x11 = xt, x13 = found?, x5 = record
      13 pnf CBZ,
   pgate LBL,
      LAOTWIDGATE LABEL@ BL,                         \ TFAM 2b-v: the one sealed-WID rule, shared with the other two passes (reads x5/x11; x24 survives)
      9 CP 24 ADD,                                   \ x9 = site addr = CP + blob offset
      10 11 9 SUB,  10 10 2 ASRI,  5 $3FFFFFF LIT64,  10 10 5 AND,   \ x10 = imm26 = (callee - site) >> 2, masked
      14 9 0 LDRW,  5 $FC000000 LIT64,  14 14 5 AND,  14 14 10 ORR,  14 9 0 STRW,   \ keep BL opcode, write imm26
      13 11 DBASE SUB,  5 REGION LIT64,  13 5 CMP,     \ callee inside the region?
      C-CC pnomark BCOND,                              \ yes: position independent, nothing to record
         14 9 DBASE SUB,                               \ x14 = site byte offset within the region
         4 14 0 ADDI,  4 4 2 LSRI,  4 4 7 ANDI,        \ x4 = bit number = word index & 7
         14 14 5 LSRI,                                 \ x14 = map byte index = offset >> 5
         5 SNAP-RELOC:CALLMAP-OFF LIT64,  14 14 5 ADD,  14 DATA 14 ADD,
         5 1 MOVZ,  5 5 4 LSLV,  13 14 0 LDRB,  13 13 5 ORR,  13 14 0 STRB,
      pnomark LBL,
      21 21 SITE-ROW ADDI,  22 22 1 ADDI,  ploop B,
   pnf LBL,
      1 nfmsg ADR,  0 2 MOVZ,  2 nfu 1+ MOVZ,  NR-WRITE SYS,
      0 ENGINE-ERROR:AOT-SEED MOVZ,  NR-EXIT-GROUP SYS,
   nfmsg LBL,  nfa nfu BYTES,  NL-KW 1 BYTES,        \ unreachable: the leg above exit_groups
   pbad LBL,
      1 badmsg ADR,  0 2 MOVZ,  2 badu 1+ MOVZ,  NR-WRITE SYS,
      0 ENGINE-ERROR:AOT-SEED MOVZ,  NR-EXIT-GROUP SYS,
   badmsg LBL,  bada badu BYTES,  NL-KW 1 BYTES,
   pdone LBL, ;

\ DATA-literal relocation (third relocation class): reserve the REPL's DATA span
\ at the current DP (the region is fixed MAP_FIXED, anon-mmap => zeroed => identical
\ to a source-compiled all-allot/variable region), then rebase every captured DATA
\ address literal (movz/movk x9 chain) by delta = seedDP - captureD0, so create/
\ variable buffer refs point at the seeded DATA. No name lookup; single delta.
\ The span is the only DP-advancing sink not routed through DP-CHECK -- it is read
\ straight from the AOT image -- so bound it BEFORE it advances DP: the new DP must
\ satisfy seedDP + span <= data-base + DATA-SIZE (the DATA region top), i.e. span
\ must fit the headroom top - seedDP. A legitimate REPL span is a few KB, well
\ within headroom; a forged/oversized span (image tampered past its sha / codesign
\ cover) fails closed with a named boot diagnostic. No eval frame exists at seed
\ time, so this uses the boot-path die idiom (named fd-2 message + exit
\ ENGINE-ERROR:AOT-SEED=82, the AOT seed-pass boot-integrity code), not LCOMPILEDIE. The die
\ is inlined between the check and the reserve; the pass path branches over it (ok)
\ so this word still falls through to EM-AOT-RELOC-CODE (it is inlined, not a call).
\ Copy the captured window's DATA content to the seed DP. x3 = destination (seed
\ DP), x5 = span; both survive. Byte at a time, like EM-AOT-COPY-BLOB - a few KB
\ once at boot, and the span has no alignment guarantee.
package AOT-WINDOW
public
: COPY-DATA ( -- )
   LBL LBL {: dcopy:label dcdone:label :}
   9 10 LDATA LABEL@ TADR,  12 0 MOVZ,              \ x9 = content src (__text), x12 = i
   dcopy LBL,  12 5 CMP,  C-GE dcdone BCOND,
      13 9 12 ADD,  13 13 0 LDRB,  14 3 12 ADD,  13 14 0 STRB,
      12 12 1 ADDI,  dcopy B,
   dcdone LBL, ;

\ Put the trap xt into every declared address cell the window carried.
\ WHAT THE BYTES COULD NOT SAY. The capture zeroed these cells because the value
\ they held was a code address in the BUILDING host, which must not enter the
\ image (aot-capture.f ACAP-BAKE-DATA states the invariant: a declared address
\ cell's value is owned by whatever declares it, never by the window's bytes).
\ Zero is not what "no implementation yet" means here, though - a dispatch cell is
\ read and branched to (`ldr x16,[x9]; blr x16`), so a zero cell is a jump to
\ address 0. What it means is the xt of `defer-unset`, which is exactly what a
\ freshly declared cell holds, and it is found the same way C-DEFER-FIND-UNSET
\ finds it at compile time: LFIND on the keyword this engine already carries. One
\ authority for what unset means, resolved in the engine being booted.
\ The boot-run list installs the real vectors immediately after the seed and that
\ is what owns them; this is the value a cell keeps if the boot-run has no entry
\ for it, and then the first call dies "defer: unset execution vector" instead of
\ branching into whatever the bytes happened to hold.
\ Runs after the copy and the DP advance, so the window base is DP - span. LFIND
\ clobbers x3-x16, hence the reload.
28 constant TRAP-MSG-LEN   \ byte length of "hb: AOT defer-unset missing\n"

: TRAP-XTCELLS ( -- )
   LBL LBL LBL LBL {: xloop:label xdone:label xbad:label msg:label :}
   23 10 LNXTOFF LABEL@ TADR,  23 23 0 LDR,         \ x23 = declared-cell count
   23 xdone CBZ,
   9 LKWDEFERUNSET LABEL@ ADR,  10 11 MOVZ,  LFIND LABEL@ BL,   \ x11 = trap xt, x13 = found?
   13 xbad CBZ,
   3 DATA DP-CELL LDR,                              \ x3 = DP, now past the window
   5 10 LAOTDATASIZE LABEL@ TADR,  5 5 0 LDR,  3 3 5 SUB,   \ x3 = window base
   21 10 LXTOFFS LABEL@ TADR,
   23 10 LNXTOFF LABEL@ TADR,  23 23 0 LDR,
   22 0 MOVZ,
   xloop LBL,  22 23 CMP,  C-GE xdone BCOND,
      24 21 0 LDRW,                                 \ x24 = window offset u32
      9 3 24 ADD,  11 9 0 STR,                      \ the cell traps until the boot-run installs it
      21 21 4 ADDI,  22 22 1 ADDI,  xloop B,
   xbad LBL,
      1 msg ADR,  0 2 MOVZ,  2 TRAP-MSG-LEN MOVZ,  NR-WRITE SYS,
      0 ENGINE-ERROR:AOT-SEED MOVZ,  NR-EXIT-GROUP SYS,
   msg LBL,  s" hb: AOT defer-unset missing" BYTES,  NL-KW 1 BYTES,
   xdone LBL, ;
;package

\ THE DELTA IS A WHOLE NUMBER OF CELLS, and that is what keeps an ALIGNMENT.
\ Every captured DATA address moves by one delta, so a delta that is not a
\ multiple of eight changes each address's 8-residue - and the atomics do not
\ survive that: LDAXR and STLR fault on a misaligned address (dot
\ habu-merged-data-window-b8fec035, measured: a merged engine died SIGBUS in
\ FIND-B on a cell four bytes off). Nothing aligns DP for the seed - `variable`
\ allots a cell wherever DP stands (C-VARIABLE) - so the seed advances DP to the
\ next address carrying the capture base's OWN residue, and from there the whole
\ window arrives with every cell exactly as aligned as it was captured. It costs
\ at most seven bytes of DATA once per boot.
: EM-AOT-RELOC-DATA ( -- )
   LBL LBL LBL LBL {: dloop:label drdone:label ok:label msg:label :}
   3 DATA DP-CELL LDR,                              \ x3 = seed DP (abs) = REPL DATA base at boot
   5 10 LAOTDATAD0 LABEL@ TADR,  5 5 0 LDR,         \ x5 = capture-time REPL DATA base
   6 5 3 SUB,  6 6 7 ANDI,  3 3 6 ADD,              \ ... and DP up to that base's own 8-residue
   6 3 5 SUB,                                       \ x6 = delta (survives the loop)
   5 10 LAOTDATASIZE LABEL@ TADR,  5 5 0 LDR,       \ x5 = REPL DATA span
   7 DATA-SIZE LIT64,  7 DATA 7 ADD,  7 7 3 SUB,    \ x7 = headroom = (data-base + DATA-SIZE) - seed DP
   5 7 CMP,  C-LS ok BCOND,                         \ span <= headroom -> ok; else fall into the boot die
      1 msg ADR,  0 2 MOVZ,  2 31 MOVZ,  NR-WRITE SYS,
      0 ENGINE-ERROR:AOT-SEED MOVZ,  NR-EXIT-GROUP SYS,
   msg LBL,  s" hb: AOT data span out of range" BYTES,  NL-KW 1 BYTES,
   ok LBL,
   AOT-WINDOW:COPY-DATA                             \ the window's own bytes, not a zeroed reserve
   3 3 5 ADD,  3 DATA DP-CELL STR,                  \ DP += span (bounded)
   21 10 LAOTDSITES LABEL@ TADR,                    \ x21 = DATA-site cursor (u32 offsets)
   23 10 LAOTNDSITE LABEL@ TADR,  23 23 0 LDR,      \ x23 = DATA-site count
   22 0 MOVZ,
   dloop LBL,  22 23 CMP,  C-GE drdone BCOND,
      24 21 0 LDRW,                                 \ x24 = blob offset u32
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
      21 21 4 ADDI,  22 22 1 ADDI,  dloop B,
   drdone LBL,
   AOT-WINDOW:TRAP-XTCELLS ;

\ CODE-literal relocation (fourth relocation class): rebase every captured movz/movk
\ x9 literal whose value pointed into the capture-time code blob [B0,B1) (anonymous
\ quotation entry addresses) by the code delta = seedCP - captureB0. The blob was
\ copied verbatim to CP, so a single delta maps each in-blob code address to its
\ seeded location. Runs while the region is RW (before RX), same as the call patch.
: EM-AOT-RELOC-CODE ( -- )
   LBL LBL {: cloop:label crdone:label :}
   5 10 LAOTCODEB0 LABEL@ TADR,  5 5 0 LDR,        \ x5 = capture-time code base (B0)
   6 CP 5 SUB,                                     \ x6 = code delta = seedCP - B0 (survives loop)
   21 10 LAOTCSITES LABEL@ TADR,                   \ x21 = CODE-site cursor (u32 offsets)
   23 10 LAOTNCSITE LABEL@ TADR,  23 23 0 LDR,     \ x23 = CODE-site count
   22 0 MOVZ,
   cloop LBL,  22 23 CMP,  C-GE crdone BCOND,
      24 21 0 LDRW,                                \ x24 = blob offset u32
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
      \ This pass is the second producer of code-address chains, so it records its
      \ sites in the same map C-CODE-ADDR writes. It knows the literal is an
      \ address of code without decoding anything: the offset came off the captured
      \ CODE-site list, and the DATA-site list is walked by a different word.
      14 9 DBASE SUB,                                \ x14 = chain byte offset within the region
      4 14 0 ADDI,  4 4 2 LSRI,  4 4 7 ANDI,         \ x4 = bit number = word index & 7
      14 14 5 LSRI,                                  \ x14 = map byte index = offset >> 5
      5 SNAP-RELOC:ADDRMAP-OFF LIT64,  14 14 5 ADD,  14 DATA 14 ADD,
      5 1 MOVZ,  5 5 4 LSLV,  13 14 0 LDRB,  13 13 5 ORR,  13 14 0 STRB,
      21 21 4 ADDI,  22 22 1 ADDI,  cloop B,
   crdone LBL, ;

\ NAMED code-literal relocation (fifth relocation class): for each baked row
\ (blob-off u32, name-off u32 into the same deduped pool the call sites use)
\ resolve the word by NAME in THIS engine and write its xt into the four immediate
\ lanes of the movz/movk chain at CP + blob-off, keeping each instruction's opcode
\ and destination register. It is the call-site pass's answer for a site that is
\ not a BL: a call site is patched by name because the callee's address differs
\ between the building host and this engine, and a code literal naming a word has
\ exactly the same problem and exactly the same answer.
\ The capture zeroes the lanes, so the value written here is the only address the
\ chain can carry - there is no host address underneath to fall back to.
\ Like EM-AOT-RELOC-CODE this is a producer of code-address chains in the region,
\ so it records each site in the map C-CODE-ADDR writes; and like
\ EM-AOT-PATCH-SITES it puts the resolved xt through the sealed-WID gate before
\ using it. Runs while the region is RW, after the records are registered.
\ A missing name is a build/seed inconsistency and cannot be recovered from at
\ boot: it names the failure on fd 2 and exits ENGINE-ERROR:AOT-SEED, the same
\ shape EM-SEED-AOT's metadata check uses.
package AOT-XTSITE
public
36 constant MSG-LEN   \ byte length of "hb: AOT named code site unresolved\n"
: PATCH-CHAINS ( -- )
   LBL LBL LBL LBL {: xloop:label xdone:label xnf:label msg:label :}
   21 10 LROWS LABEL@ TADR,                       \ x21 = row cursor (8B rows)
   23 10 LCOUNT LABEL@ TADR,  23 23 0 LDR,        \ x23 = row count
   22 0 MOVZ,
   xloop LBL,  22 23 CMP,  C-GE xdone BCOND,
      24 21 0 LDRW,                               \ x24 = blob offset u32 (survives LFIND)
      4 21 4 LDRW,                                \ x4 = name-off u32
      5 10 LAOTNAMES LABEL@ TADR,  9 5 4 ADD,     \ x9 = pool entry ptr
      10 9 0 LDRB,                                \ x10 = name length
      9 9 1 ADDI,                                 \ x9 = name ptr
      LFIND LABEL@ BL,                            \ x11 = xt, x13 = found?, x5 = record
      13 xnf CBZ,
      LAOTWIDGATE LABEL@ BL,                      \ reject a resolve into a protected WID (reads LFIND's x5/x11; x24 survives)
      9 CP 24 ADD,                                \ x9 = chain addr = CP + blob offset
      10 9 0 LDRW,   5 $FFE0001F LIT64,  10 10 5 AND,  14 11 0 ADDI,   5 $FFFF LIT64,  14 14 5 AND,  14 14 5 LSLI,  10 10 14 ORR,  10 9 0 STRW,
      10 9 4 LDRW,   5 $FFE0001F LIT64,  10 10 5 AND,  14 11 16 LSRI,  5 $FFFF LIT64,  14 14 5 AND,  14 14 5 LSLI,  10 10 14 ORR,  10 9 4 STRW,
      10 9 8 LDRW,   5 $FFE0001F LIT64,  10 10 5 AND,  14 11 32 LSRI,  5 $FFFF LIT64,  14 14 5 AND,  14 14 5 LSLI,  10 10 14 ORR,  10 9 8 STRW,
      10 9 12 LDRW,  5 $FFE0001F LIT64,  10 10 5 AND,  14 11 48 LSRI,  5 $FFFF LIT64,  14 14 5 AND,  14 14 5 LSLI,  10 10 14 ORR,  10 9 12 STRW,
      14 9 DBASE SUB,                             \ x14 = chain byte offset within the region
      4 14 0 ADDI,  4 4 2 LSRI,  4 4 7 ANDI,      \ x4 = bit number = word index & 7
      14 14 5 LSRI,                               \ x14 = map byte index = offset >> 5
      5 SNAP-RELOC:ADDRMAP-OFF LIT64,  14 14 5 ADD,  14 DATA 14 ADD,
      5 1 MOVZ,  5 5 4 LSLV,  13 14 0 LDRB,  13 13 5 ORR,  13 14 0 STRB,
      21 21 8 ADDI,  22 22 1 ADDI,  xloop B,
   xnf LBL,
      1 msg ADR,  0 2 MOVZ,  2 MSG-LEN MOVZ,  NR-WRITE SYS,
      0 ENGINE-ERROR:AOT-SEED MOVZ,  NR-EXIT-GROUP SYS,
   msg LBL,  s" hb: AOT named code site unresolved" BYTES,  NL-KW 1 BYTES,
   xdone LBL, ;
;package

\ Boot-run the captured top-level entry words (INSTALL/BPW-INSTALL/S-INSTALL) once
\ the seeded blob is RX + icache-flushed: walk the 0-terminated [len][name] list,
\ LFIND each in the now-registered dict, and blr its xt. This replaces the embedded
\ install-tail source -- the engine installs the REPL with zero baked source. Runs
\ at LEX0 on every boot; the entry words self-guard on TTY? so pipe/script boots are
\ no-ops. A missing name is a build/seed bug -> panic exit $52.
: EM-AOT-BOOTRUN ( -- )
   LBL LBL LBL {: bloop:label bdone:label bnf:label :}
   21 10 LAOTBOOTRUN LABEL@ TADR,                     \ x21 = list cursor
   bloop LBL,
      10 21 0 LDRB,  10 bdone CBZ,                   \ x10 = name len; 0 -> done
      SP SP 16 SUBI,  21 SP 0 STR,  10 SP 8 STR,     \ preserve cursor + len across the call
      9 21 1 ADDI,                                   \ x9 = name ptr = cursor + 1
      LFIND LABEL@ BL,                               \ x11 = xt, x13 = found?, x5 = record
      13 bnf CBZ,
      LAOTWIDGATE LABEL@ BL,                         \ TFAM 2b-v: reject bootrun into a protected WID (reads LFIND's x5/x11)
      11 BLR,                                        \ call the entry word
      21 SP 0 LDR,  10 SP 8 LDR,  SP SP 16 ADDI,
      21 21 1 ADDI,  21 21 10 ADD,                   \ advance past [len][name]
      bloop B,
   bnf LBL,  0 $52 MOVZ,  NR-EXIT-GROUP SYS,
   bdone LBL, ;

\ Publish the checker payload: where it is and how long. Two DATA cells, written
\ once and never again, which is what makes them readable by ordinary checked
\ code (src/core/checker.f CK-AOT-SIG-POOL / CK-AOT-SIG-LEN) with no relocation
\ of their own: the span is __text, mapped for the life of the process, and an
\ offset into it means the same thing at every boot.
\
\ IT RUNS AFTER THE RECORDS ARE REGISTERED and before the boot-run list, because
\ a boot-run entry word is a definition like any other and may name a seeded
\ word: the checker has to be able to see the pool by the time one is compiled.
\ A build that captured nothing never reaches here - EM-SEED-AOT skips the whole
\ pass - so both cells stay the zero the DATA region boots with, which is exactly
\ "no pool" to the intake.
\ In the package that owns the two cells, the way AOT-WINDOW's boot passes sit in
\ the package that owns its label ids: src/habu/layout.f declares POOL-CELL and
\ LEN-CELL, and this is the one writer of them.
package AOT-SIG
public
: PUBLISH, ( -- )
   9 5 LSPAN LABEL@ TADR,
   9 DATA POOL-CELL STR,
   9 5 LLEN LABEL@ TADR,  9 9 0 LDR,
   9 DATA LEN-CELL STR, ;

\ Put the payload's TYPE REGISTRY in, here and nowhere later. The carried
\ signature rows name families by ABSOLUTE id, so the ids are only the families
\ they mean while the live registry still ends where the capture's did. This
\ point is the one moment that is true by construction: the engine prefix has
\ run and no user token has, so nothing outside the engine can have declared a
\ type yet, and no checker rollback frame is open to undo the install. Doing it
\ at the first signature intake instead - which is where it used to happen -
\ made ordinary source refuse: declare a NEWTYPE, then name a chain word, and
\ the high-water has already moved past the base the capture recorded.
\
\ The SIGNATURE POOL stays lazy. That laziness was priced for 124 KB of text and
\ 6798 parses; the registry is 46 KB of pointer-free rows, and a boot pays for
\ it whether or not it ever names a chain word.
\
\ Resolved by name through LFIND, the way EM-AOT-BOOTRUN resolves an entry word,
\ and refused the same way: the widgate rules the callee's wordlist and a
\ missing name is a seed built against a checker that no longer has it, which is
\ a panic and not a miss.
: INSTALL-NAME$ ( -- ptr u8 n )
   s" CK-AOT-REG-INSTALL" ;

\ The emitted immediate comes from the emitted bytes, so the two cannot drift.
: INSTALL-NAME-LEN ( -- n )
   INSTALL-NAME$ {: a:ptr u:n :} u ;

: INSTALL, ( -- )
   LBL LBL {: nf:label done:label :}
   9 5 LNAME LABEL@ TADR,
   10 INSTALL-NAME-LEN MOVZ,
   LFIND LABEL@ BL,
   13 nf CBZ,
   LAOTWIDGATE LABEL@ BL,
   11 BLR,
   done B,
   nf LBL,  0 $52 MOVZ,  NR-EXIT-GROUP SYS,
   done LBL, ;
;package

\ Seed the metabuild-captured AOT words at LEXIT: copy the blob, register N dict
\ records, name-relocate the call sites, relocate DATA-address literals, advance CP.
\ Region is RX at LEXIT so the pass toggles RW around all region writes and flushes
\ the icache. LAOTNREC = 0 (stage2/maker/snap: nothing captured) skips the pass.
: EM-SEED-AOT ( -- )
   LBL LBL LBL {: askip:label bad:label msg:label :}
   11 5 LAOTNREC LABEL@ TADR,  11 11 0 LDR,        \ x11 = N
   11 askip CBZ,                                    \ nothing captured -> skip
   bad EM-AOT-VALIDATE
   11 5 LAOTCODELEN LABEL@ TADR,  11 11 0 LDR,     \ x11 = blob length, read before the flip
   1 CP 11 ADD,  PROT:LOPEN LABEL@ BL,              \ region -> RW over the blob's landing span
   EM-AOT-COPY-BLOB                                 \ x11 rides the kernel-preserved x2-x15 band
   EM-AOT-REGISTER-RECS
   EM-AOT-PATCH-SITES
   EM-AOT-RELOC-DATA
   EM-AOT-RELOC-CODE
   AOT-XTSITE:PATCH-CHAINS                          \ named code literals, after the rebased ones
   9 CP 0 ADDI,                                     \ x9 = blob base (= CP before advance) for the flush
   11 5 LAOTCODELEN LABEL@ TADR,  11 11 0 LDR,     \ x11 = blob length again
   CP CP 11 ADD,                                    \ code area top past the blob
   PROT:LCLOSE LABEL@ BL,                           \ region -> RX
   LFLUSH LABEL@ BL,                                \ flush icache over [blob base, CP)
   AOT-SIG:PUBLISH,                                 \ the checker payload's address and length
   AOT-SIG:INSTALL,                                 \ then its type registry, before any user token can declare a family
   EM-AOT-BOOTRUN                                   \ install the REPL (no source): LFIND+blr the entry words
   askip B,
   bad LBL,
      1 msg ADR,  0 2 MOVZ,  2 25 MOVZ,  NR-WRITE SYS,
      0 ENGINE-ERROR:AOT-SEED MOVZ,  NR-EXIT-GROUP SYS,
   msg LBL,  s" hb: AOT metadata corrupt" BYTES,  NL-KW 1 BYTES,
   askip LBL, ;

: EM-SEED-DICT ( -- )
   LBL LBL LBL LBL {: scopy:label scdone:label pkg:label fields:label :}
   DBASE 0 0 ADDI,
   CP DBASE 0 ADDI,  5 DICT-SIZE LIT64,  CP CP 5 ADD,
   11 LNCOUNT LABEL@ ADR,  11 11 0 LDR,  NDICT 11 0 ADDI,
   9 LDICT LABEL@ ADR,  10 DBASE 0 ADDI,  12 11 0 ADDI,
   scopy LBL,
      12 scdone CBZ,
      5 9 0 LDR,  6 9 8 LDR,
      7 9 40 LDR,  8 0 MOVN,  7 8 CMP,  C-EQ pkg BCOND,
      7 XREG-RBASE 5 ADD,  7 10 0 STR,
      6 6 5 SUB,  6 6 4 SUBI,  6 10 8 STR,
      fields B,
      pkg LBL,
      5 10 0 STR,  6 10 8 STR,
      fields LBL,
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

: EM-MMAP-DATA-REGION ( -- )
   LBL {: dvok :}
   0 DATA-VA VA>N LIT64,  1 DATA-SIZE LIT64,  2 3 MOVZ,  3 MAP-ANON-PRIVATE-FIXED LIT64,  4 0 MOVN,  5 0 MOVZ,
   NR-MMAP SYS,
   5 DATA-VA VA>N LIT64,  0 5 CMP,
   C-EQ dvok BCOND,
      1 LMMAPDATA LABEL@ ADR,  0 2 MOVZ,  2 MMAPDATA-MSG-LEN MOVZ,  NR-WRITE SYS,   \ name the fault on fd 2 (bytes in loaded __text) before exit; code region is already mapped, argc/argv/envp in x13/x14/x15 untouched
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
\ x8 = region base, x15 = record count,
\ x21 = detect base, x22 = detect len, x25 = rebase target base (value - x21 + x25).
: EM-SNAPSHOT-REBASE-DICT ( -- )
   LBL LBL LBL LBL {: sdl2 sdn2 sds2 srn :}
   LSNAPRBD LABEL@ LBL,
   9 8 0 ADDI,  10 0 MOVZ,
   sdl2 LBL,  10 15 CMP,  C-GE sdn2 BCOND,
      13 9 40 LDR,  14 0 MOVN,  13 14 CMP,  C-EQ sds2 BCOND, \ package [0]/[8] are raw WID roles, never text pointers
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

\ ---- snapshot relocation: the two emitted passes -----------------------------
\ Reopens the package src/habu/layout.f declares the bands in and this file
\ declares the labels in.
package SNAP-RELOC
public

\ Region-to-text call relocation, the BL-shaped counterpart of the dictionary walk
\ above and the other half of what makes a snapshot image portable.
\ A call that stays inside the region keeps its distance wherever the region lands,
\ so it needs nothing. A call from the region into the engine's loaded __text does
\ not, because the run that writes an image and the run that restores it get their
\ region base from the kernel and their image base from the loader independently.
\ The sites are not searched for; they were recorded when the calls were created
\ (CALLMAP-OFF, written by EMIT-CEMITBL and EM-AOT-PATCH-SITES), because a compiled
\ word may carry inline non-instruction data and no decode of region bytes could
\ tell a call from a data word that happens to look like one.
\ Parameterized exactly like the dictionary walk so the writer (scratch copy, live
\ distance -> canonical distance) and the loader (live region, canonical distance
\ -> live distance) share ONE implementation:
\   x8  = base of the region image being patched
\   x11 = its byte length (the map is scanned only that far)
\   x10 = signed byte amount to add to every recorded call displacement
\ The added amount is always a multiple of four, so it is applied in instruction
\ units and re-masked into imm26; the boot assertion in EM-MMAP-CODE-REGION is what
\ guarantees the result is still inside BL's reach.
\ Clobbers x3..x7, x9, x12..x14; x8/x10/x11 survive for the caller.
: EMIT-CALLS ( -- )
   LBL LBL LBL LBL LBL LBL {: scl:label scdone:label scnext:label
                              scbit:label scbnext:label scbad:label :}
   LCALLS LABEL@ LBL,
   6 CALLMAP-OFF LIT64,  6 DATA 6 ADD,              \ x6 = call-site map base
   13 11 31 ADDI,  13 13 5 LSRI,                    \ x13 = map bytes covering the image (rounded up)
   12 0 MOVZ,                                       \ x12 = map byte index
   7 10 2 ASRI,                                     \ x7 = the delta in instruction units
   scl LBL,  12 13 CMP,  C-GE scdone BCOND,
      5 6 12 ADD,  5 5 0 LDRB,                      \ x5 = map byte, consumed one bit at a time
      5 scnext CBZ,                                 \ no site recorded in these eight words
      4 12 3 LSLI,                                  \ x4 = region word index of this byte's first bit
      scbit LBL,  5 scnext CBZ,                     \ every remaining bit is clear
         14 5 1 ANDI,  14 scbnext CBZ,
            14 4 2 LSLI,                            \ x14 = site byte offset within the region
            14 11 CMP,  C-GE scbnext BCOND,         \ past this payload: not part of the image being patched
            14 8 14 ADD,                            \ x14 = site address
            9 14 0 LDRW,
            \ a recorded site must still be a call. Anything else means the map and
            \ the region disagree, which is a corrupt image, so refuse to run it.
            3 9 26 LSRI,  3 BL-OP-HI CMPI,  C-NE scbad BCOND,
            3 9 38 LSLI,  3 3 38 LSRI,              \ x3 = current displacement, in instructions
            3 3 7 ADD,  3 3 38 LSLI,  3 3 38 LSRI,  \ shifted by the delta, back into 26 bits
            9 9 26 LSRI,  9 9 26 LSLI,  9 9 3 ORR,  9 14 0 STRW,
         scbnext LBL,
         5 5 1 LSRI,  4 4 1 ADDI,  scbit B,
   scnext LBL,  12 12 1 ADDI,  scl B,
   scbad LBL,
      1 LCALLMSG LABEL@ ADR,  0 2 MOVZ,  2 CALLMSG-LEN MOVZ,  NR-WRITE SYS,
      0 CALLMAP-RC MOVZ,  NR-EXIT-GROUP SYS,
   scdone LBL,  RET, ;

\ Declare one persisted DATA cell as holding a JIT-region address, so the snapshot
\ writer canonicalises it and the loader maps it back onto the live region.
\   x9 = the cell's address, unchanged on return.
\ Membership is a set: `defer` registers a dispatch cell when it allocates it,
\ `is` registers the cell it is about to store into, and the `xt!` primitive
\ registers a cell whose address its caller worked out at run time, so the same
\ cell arrives here more than once and must be listed once. The table is short
\ (one row per deferred word plus one per declared table cell in the image), the
\ walk runs while `defer`/`is` is being compiled or while a dispatch table is
\ being filled in, and the alternative -- letting a cell in twice -- would apply
\ the relocation twice and produce an address that points nowhere.
\ Overflow is a hard stop: dropping a row would leave a stale writer-run address in
\ a restored image, which is exactly the failure this table exists to prevent.
\ A cell that does not lie inside DATA is a hard stop for the same reason and one
\ step earlier. This is the single point every producer of a row goes through --
\ `defer`, `is` and `xt!` alike -- so validating here covers all three, and each
\ of them now declares BEFORE it stores, so a refused cell is refused before
\ anything has been written to it. A cell outside DATA is not a near miss: the
\ table's three consumers would index DATA by that offset and read or write eight
\ bytes of whatever lives there. Containment is the entire rule -- see the
\ XTCELL-OFF-MAX note in src/habu/layout.f for why alignment is not part of it.
\ Every register it uses is saved and restored, because the compile-handler call
\ sites are in the middle of a handler with its own live values and the `xt!` call
\ site is in the middle of a running checked word.
: EMIT-MARK ( -- )
   LBL LBL LBL LBL LBL {: scan:label add:label full:label band:label ret:label :}
   LMARK LABEL@ LBL,
   SP SP 48 SUBI,
   5 SP 0 STR,  6 SP 8 STR,  12 SP 16 STR,  13 SP 24 STR,  14 SP 32 STR,
   12 9 DATA SUB,                                   \ x12 = the cell's offset within DATA
   6 XTCELL-OFF-MAX LIT64,  12 6 CMP,  C-HI band BCOND,   \ unsigned: the cell would run past DATA, or start below it
   5 XTCELL-ROWS-OFF LIT64,  5 DATA 5 ADD,          \ x5 = row base
   6 XTCELL-N-CELL LIT64,  6 DATA 6 ADD,  13 6 0 LDR,   \ x13 = rows in use
   14 0 MOVZ,                                       \ x14 = row index
   scan LBL,  14 13 CMP,  C-GE add BCOND,
      6 14 3 LSLI,  6 5 6 ADD,  6 6 0 LDR,
      6 12 CMP,  C-EQ ret BCOND,                    \ already declared: nothing to do
      14 14 1 ADDI,  scan B,
   add LBL,
      6 XTCELL-CAP MOVZ,  13 6 CMP,  C-GE full BCOND,
      6 13 3 LSLI,  6 5 6 ADD,  12 6 0 STR,
      13 13 1 ADDI,
      6 XTCELL-N-CELL LIT64,  6 DATA 6 ADD,  13 6 0 STR,
      ret B,
   full LBL,
      1 LXTMSG LABEL@ ADR,  0 2 MOVZ,  2 XTMSG-LEN MOVZ,  NR-WRITE SYS,
      0 XTCELL-RC MOVZ,  NR-EXIT-GROUP SYS,
   band LBL,
      1 LXTBANDMSG LABEL@ ADR,  0 2 MOVZ,  2 XTBANDMSG-LEN MOVZ,  NR-WRITE SYS,
      0 XTBAND-RC MOVZ,  NR-EXIT-GROUP SYS,
   ret LBL,
   5 SP 0 LDR,  6 SP 8 LDR,  12 SP 16 LDR,  13 SP 24 LDR,  14 SP 32 LDR,
   SP SP 48 ADDI,  RET, ;

\ xt! ( q ptr q -- ): store an execution token into a persisted cell and declare
\ that cell to the table above, in one step.
\ `defer` and `is` can only declare a cell whose address is known while they are
\ being COMPILED. A TABLE of dispatch cells picks its cell at RUN time, from a
\ base pointer and a row index, so no compile handler ever sees that address, and
\ the store word is the only code that knows a JIT-region address is going into a
\ persisted cell (dot habu-declare-persisted-cb-b150b5d5). Storing and declaring
\ are one primitive rather than two words so neither half can be done without the
\ other and the declaration cannot drift away from the store it describes.
\ The store itself is habu1.f BSTORE's, protection guard and all: x10 is the cell,
\ x9 the token, and the guard leaves both alone.
\ The declaration runs BEFORE the store, which is the ordering `defer` and `is`
\ have always had and the reason the band check above can be called a
\ precondition: LMARK refuses a cell that is not a cell-aligned address inside
\ DATA by terminating, so a refused cell is never written. Doing the store first
\ would leave the caller's token in a cell the engine then declines to declare.
\ The swap below is what that ordering costs: LMARK wants the cell in x9, so the
\ token is parked in x11 across the call. x11 is scratch in a primitive body, and
\ LMARK preserves x9 and saves every other register it touches, so both survive.
\ Registered framed, because the guard and LMARK are both BL.
: BXTSTORE ( -- )
   B G-POP  A G-POP
   7 8 MOVZ,  B 7 PROT-GUARD:CALL
   C A 0 ADDI,                                      \ x11 = the token
   A B 0 ADDI,                                      \ x9 = the cell, for LMARK
   LMARK LABEL@ BL,
   C B 0 STR, ;

\ Move every declared address cell by one signed amount.
\   x10 = the amount to add
\ The loader is the only caller: the writer's half of this pass is checked Habu in
\ src/habu/snap-lib.f, because it works on a scratch copy of DATA rather than on
\ the live region. A zero cell means "nothing installed here yet" and stays zero,
\ which is why a cleared hook survives a snapshot as a cleared hook.
\ Every row is re-validated here rather than trusted because the rows arrive from
\ an IMAGE FILE. The engine that wrote it checked what it declared, but this run
\ did not write it: a damaged or forged image can carry any offset at all, and
\ relocating through one would add the region delta to eight bytes chosen by the
\ image. The check is the same band the declaration enforces, so a row this
\ engine wrote always passes and only a corrupt image can fail it.
\ Clobbers x5, x6, x9, x13, x14, x12.
: EMIT-XT ( -- )
   LBL LBL LBL LBL {: loop:label skip:label band:label done:label :}
   LXT LABEL@ LBL,
   5 XTCELL-ROWS-OFF LIT64,  5 DATA 5 ADD,
   6 XTCELL-N-CELL LIT64,  6 DATA 6 ADD,  13 6 0 LDR,
   14 0 MOVZ,
   loop LBL,  14 13 CMP,  C-GE done BCOND,
      6 14 3 LSLI,  6 5 6 ADD,  6 6 0 LDR,          \ x6 = the cell's offset within DATA
      12 XTCELL-OFF-MAX LIT64,  6 12 CMP,  C-HI band BCOND,  \ unsigned: the cell would run past DATA, or start below it
      6 DATA 6 ADD,
      9 6 0 LDR,  9 skip CBZ,
         9 9 10 ADD,  9 6 0 STR,
      skip LBL,
      14 14 1 ADDI,  loop B,
   band LBL,
      1 LXTBANDMSG LABEL@ ADR,  0 2 MOVZ,  2 XTBANDMSG-LEN MOVZ,  NR-WRITE SYS,
      0 XTBAND-RC MOVZ,  NR-EXIT-GROUP SYS,
   done LBL,  RET, ;

\ Record the four-instruction MOVZ/MOVK chain that is about to be written at CP as
\ an address of code, so the two relocation passes can find it again without ever
\ decoding region bytes. Called from C-CODE-ADDR before the chain is emitted, so
\ CP is exactly the chain's first word.
\ x11 holds the address the caller is about to compile in and must survive; x30 is
\ already dead here, because the chain emission itself calls LCEMIT four times.
\ Clobbers x5, x6, x9.
: EMIT-ADDR-SITE ( -- )
   LADDRSITE LABEL@ LBL,
   6 CP DBASE SUB,                                  \ x6 = chain's byte offset within the region
   9 6 0 ADDI,  9 9 2 LSRI,  9 9 7 ANDI,            \ x9 = bit number = word index & 7
   6 6 5 LSRI,                                      \ x6 = map byte index = offset >> 5
   5 ADDRMAP-OFF LIT64,  6 6 5 ADD,  6 DATA 6 ADD,  \ x6 = map byte address
   5 1 MOVZ,  5 5 9 LSLV,  9 6 0 LDRB,  9 9 5 ORR,  9 6 0 STRB,
   RET, ;

\ Address-literal relocation: the MOVZ/MOVK-shaped counterpart of the dictionary
\ walk and of the call pass, and the third and last thing that has to move when a
\ snapshot image is restored at addresses the writing run never saw.
\ The chain a `[: ... ;]`, a `[']` or a `postpone` pushes names a word's code, and
\ that code sits either inside the JIT region or in the engine's loaded __text.
\ Both move independently between the writing and the restoring run, so the pass
\ is parameterized exactly like the dictionary walk and is CALLED ONCE PER BAND,
\ with the same triple that walk is given:
\   x21 = base of the band being moved
\   x22 = its length
\   x25 = the base those addresses are moving to  (value - x21 + x25)
\ and, like the call pass, with the image it is scanning:
\   x8  = base of the region image being patched
\   x11 = its byte length (the map is scanned only that far)
\ The two bands are disjoint at both write and restore time -- the live region sits
\ REGION-OFF above __text, and canonical text offsets are far below the RBASE-VA
\ sentinel -- so a chain is rewritten by exactly one of the two calls, and a chain
\ that names neither band (there is no such kind today) is left alone rather than
\ guessed at.
\ The sites are not searched for. They were recorded when the chain was created
\ (ADDRMAP-OFF, written by EMIT-ADDR-SITE from C-CODE-ADDR and by the AOT seed's
\ code-literal rebase), because a compiled word may carry inline non-instruction
\ data and the sibling DATA literals share the chain's exact shape, so no decode of
\ region bytes could tell the three apart.
\ Clobbers x2, x3, x4, x5, x6, x7, x9, x10, x12, x13, x14; x8, x11, x21, x22 and
\ x25 survive for the caller.
: EMIT-ADDRS ( -- )
   LBL LBL LBL LBL LBL LBL {: sal:label sadone:label sanext:label
                              sabit:label sabnext:label sabad:label :}
   LADDRS LABEL@ LBL,
   6 ADDRMAP-OFF LIT64,  6 DATA 6 ADD,              \ x6 = address-literal map base
   13 11 31 ADDI,  13 13 5 LSRI,                    \ x13 = map bytes covering the image (rounded up)
   12 0 MOVZ,                                       \ x12 = map byte index
   7 ADDR-OPC-MASK LIT64,                           \ x7 = an instruction word minus its immediate
   2 ADDR-IMM-MASK LIT64,                           \ x2 = one 16-bit immediate field
   sal LBL,  12 13 CMP,  C-GE sadone BCOND,
      5 6 12 ADD,  5 5 0 LDRB,                      \ x5 = map byte, consumed one bit at a time
      5 sanext CBZ,                                 \ no site recorded in these eight words
      4 12 3 LSLI,                                  \ x4 = region word index of this byte's first bit
      sabit LBL,  5 sanext CBZ,                     \ every remaining bit is clear
         14 5 1 ANDI,  14 sabnext CBZ,
            14 4 2 LSLI,                            \ x14 = chain's byte offset within the region
            3 14 ADDR-CHAIN-BYTES ADDI,             \ the whole chain has to be inside the payload
            3 11 CMP,  C-HI sabnext BCOND,          \ past this payload: not part of the image being patched
            14 8 14 ADD,                            \ x14 = chain address
            \ a recorded site must still be the chain. Anything else means the map
            \ and the region disagree, which is a corrupt image, so refuse to run it.
            \ Word 0 supplies the register the whole chain names (x3), and the other
            \ three lanes are compared against their own scaffold carrying THAT
            \ register, so four move-wide words that name four different registers are
            \ refused as firmly as four words that are not move-wide at all.
            9 14 0 LDRW,   9 9 7 AND,
            3 9 ADDR-RD-MASK ANDI,
            10 W-MOVZ0 LIT64,  10 10 ADDR-RD-BITS LSRI,  10 10 ADDR-RD-BITS LSLI,  10 10 3 ORR,
            9 10 CMP,  C-NE sabad BCOND,
            9 14 4 LDRW,   9 9 7 AND,
            10 W-MOVK1 LIT64,  10 10 ADDR-RD-BITS LSRI,  10 10 ADDR-RD-BITS LSLI,  10 10 3 ORR,
            9 10 CMP,  C-NE sabad BCOND,
            9 14 8 LDRW,   9 9 7 AND,
            10 W-MOVK2 LIT64,  10 10 ADDR-RD-BITS LSRI,  10 10 ADDR-RD-BITS LSLI,  10 10 3 ORR,
            9 10 CMP,  C-NE sabad BCOND,
            9 14 12 LDRW,  9 9 7 AND,
            10 W-MOVK3 LIT64,  10 10 ADDR-RD-BITS LSRI,  10 10 ADDR-RD-BITS LSLI,  10 10 3 ORR,
            9 10 CMP,  C-NE sabad BCOND,
            \ x3 = the address the four immediates spell out
            9 14 0 LDRW,   9 9 5 LSRI,  9 9 2 AND,  3 9 0 ADDI,
            9 14 4 LDRW,   9 9 5 LSRI,  9 9 2 AND,  9 9 16 LSLI,  3 3 9 ORR,
            9 14 8 LDRW,   9 9 5 LSRI,  9 9 2 AND,  9 9 32 LSLI,  3 3 9 ORR,
            9 14 12 LDRW,  9 9 5 LSRI,  9 9 2 AND,  9 9 48 LSLI,  3 3 9 ORR,
            3 21 CMP,  C-CC sabnext BCOND,          \ below this band: the other call owns it
            10 21 22 ADD,  3 10 CMP,  C-CS sabnext BCOND,
            3 3 21 SUB,  3 3 25 ADD,                \ x3 = the same code at its new base
            9 14 0 LDRW,   9 9 7 AND,  10 3 0 ADDI,   10 10 2 AND,  10 10 5 LSLI,  9 9 10 ORR,  9 14 0 STRW,
            9 14 4 LDRW,   9 9 7 AND,  10 3 16 LSRI,  10 10 2 AND,  10 10 5 LSLI,  9 9 10 ORR,  9 14 4 STRW,
            9 14 8 LDRW,   9 9 7 AND,  10 3 32 LSRI,  10 10 2 AND,  10 10 5 LSLI,  9 9 10 ORR,  9 14 8 STRW,
            9 14 12 LDRW,  9 9 7 AND,  10 3 48 LSRI,  10 10 2 AND,  10 10 5 LSLI,  9 9 10 ORR,  9 14 12 STRW,
         sabnext LBL,
         5 5 1 LSRI,  4 4 1 ADDI,  sabit B,
   sanext LBL,  12 12 1 ADDI,  sal B,
   sabad LBL,
      1 LADDRMSG LABEL@ ADR,  0 2 MOVZ,  2 ADDRMSG-LEN MOVZ,  NR-WRITE SYS,
      0 ADDRMAP-RC MOVZ,  NR-EXIT-GROUP SYS,
   sadone LBL,  RET, ;

;package

\ ---- the open capture window's question, asked of one body word ---------------
\ WHAT IT ANSWERS. The compile-mode inliner is about to copy a candidate body into
\ the word being compiled. If that body carries an address chain, the copy carries
\ the chain's VALUE, and a captured window has to be able to say what that value
\ means: an address inside the window's own DATA moves with the window and is
\ rebased at boot, an address inside the window's own code moves with the blob and
\ is rebased too, and an address that is in NEITHER is one the window cannot
\ describe at all. The last kind used to be copied in and then refused by the
\ capture (aot-capture.f ACAP-UNCLASSIFIED, exit 74) after the whole build had run.
\ This routine says so BEFORE the copy, and the inliner then emits its BL instead -
\ which the capture records as an ordinary call site and the seed relocates BY NAME
\ in the engine being built, so the pre-window address never travels at all.
\
\ IT IS THE SAME CLASSIFICATION ACAP-SCAN-DSITES PERFORMS, on the live watermarks.
\ There the spans are [d0,d1) and [b0,b1), both ends latched once the window has
\ closed; here the window is still open, so the ends are the current DP and CP.
\ Those are the same intervals restricted to what exists yet, because DP and CP
\ only grow and no chain can name a cell not yet allotted or a body not yet
\ compiled. Membership is decided by two unsigned range tests, which reject an
\ address below the base in the same comparison.
\
\ EXISTENCE IS THE RECORD'S TO ANSWER, exactly as it is where the copy loop
\ reissues a record, whose band read this repeats: a word is the start of a chain
\ if and only if the address-literal map says so, and that map covers the JIT
\ region and nothing else, so a body in the engine's loaded __text is passed
\ through untested. The value is decoded only for a word the map has already
\ called a chain, so nothing here recognises an address by what it looks like.
\
\ AND AN ENGINE WITH NO WINDOW OPEN DECLINES NOTHING, by arithmetic rather than by
\ a flag: both cells read zero, the DATA test becomes "is the value below the
\ current DP" and every address a recorded chain can hold is, so the first test
\ accepts and the routine returns 0. That is why there is no armed/unarmed branch
\ to keep in step, and why this path is exercised by every compile in every engine
\ instead of only inside a metabuild.
\
\   x15 = the candidate body word's ADDRESS (not its contents)
\   x10 = 0 to go on copying, nonzero to decline the body and call it
\ Clobbers x5, x6, x9 and x10 - the same four the inliner's copy step and LCEMITBL
\ already clobber, so every path out of C-CALL destroys them anyway.
package AOT-WINDOW
public
: EMIT-OUTSIDE ( -- )
   LBL LBL {: ok:label ret:label :}
   LOUTSIDE LABEL@ LBL,
   6 15 DBASE SUB,  5 REGION LIT64,  6 5 CMP,          \ inside the band the map covers?
   C-CS ok BCOND,                                      \ no: engine __text carries no records
   9 6 0 ADDI,  9 9 2 LSRI,  9 9 7 ANDI,               \ x9 = bit number = word index & 7
   6 6 5 LSRI,                                         \ x6 = map byte index = offset >> 5
   5 SNAP-RELOC:ADDRMAP-OFF LIT64,  6 6 5 ADD,  6 DATA 6 ADD,
   10 6 0 LDRB,  10 10 9 LSRV,  10 10 1 ANDI,          \ x10 = this word's recorded bit
   10 ok CBZ,                                          \ not a chain: nothing to classify
   6 SNAP-RELOC:ADDR-IMM-MASK LIT64,                   \ x6 = one 16-bit immediate field
   9 15 0 LDRW,   9 9 5 LSRI,   9 9 6 AND,             \ x9 = the address the four lanes spell out
   10 15 4 LDRW,  10 10 5 LSRI,  10 10 6 AND,  10 10 16 LSLI,  9 9 10 ORR,
   10 15 8 LDRW,  10 10 5 LSRI,  10 10 6 AND,  10 10 32 LSLI,  9 9 10 ORR,
   10 15 12 LDRW, 10 10 5 LSRI,  10 10 6 AND,  10 10 48 LSLI,  9 9 10 ORR,
   5 DATA D0-CELL LDR,  6 DATA DP-CELL LDR,            \ the window's DATA span, open at DP
   10 9 5 SUB,  6 6 5 SUB,  10 6 CMP,  C-CC ok BCOND,
   5 DATA B0-CELL LDR,  6 CP 0 ADDI,                   \ its code span, open at CP
   10 9 5 SUB,  6 6 5 SUB,  10 6 CMP,  C-CC ok BCOND,
   10 1 MOVZ,  ret B,                                  \ in neither: the window cannot describe it
   ok LBL,  10 0 MOVZ,
   ret LBL,  RET, ;
;package

\ Sealed-WID reject for the AOT boot passes (TFAM 2b-v). Entry is LFIND's answer
\ as LFIND left it: x5 = the record the baked name resolved to, x11 = its xt. A
\ callee the engine's own rules would not let checked source CALL fails the boot
\ closed (exit ENGINE-ERROR:SEAL-PACKAGE), so a captured relocation callee or
\ boot-run entry name is rejected before the call immediate is rewritten or the
\ entry word is executed.
\ Preserves x5 and x11; clobbers x6/x9/x13/x14; saves x30 for the nested LPROTWIDQ.
\
\ WHAT IT REFUSES IS A PRIVATE WORD OF A SEALED PRE-EXISTING PACKAGE, and that is
\ far narrower than "any protected WID", which is what this asked until
\ 2026-08-16. The rule is four layers and every one of them was forced by a
\ measurement on the real compiler chain (18893 sites); they are tested in this
\ order because the first is one subtraction and answers for most of them.
\
\ (1) A WORDLIST THIS SEED CREATED IS NOT ONE THE ENGINE ALREADY HAD. The gate
\ exists to stop a baked name reaching into a pre-existing sealed wordlist; the
\ window's own private call to its own private word is the window calling itself,
\ and refusing it refuses the whole chain - 10648 of its sites resolve into
\ in-window PRIVATE wordlists that the chain's own packages sealed with the
\ ordinary `private get-current prot-wid-add` idiom, and AOT-WINDOW:SEAL-WIDS,
\ has already set those bits by the time the call-site pass runs.
\ T0 is READ FROM ITS CELL, not recomputed: `WIDN - span` is T0 only until
\ something else allocates a wordlist, which a boot-run entry word may do before
\ the last name on the list has been through this gate.
\
\ (2) THE TWO ENGINE-RESERVED WORDLISTS ARE ADMITTED, and the tree already ruled
\ why. OWNER-API-PRI-WID carries the registered engine helpers ((PROT-SPAN),
\ (LP2VEXEC)) and habu1.f's BSWL states it: the refusal of that wordlist belongs
\ to the `search-wl` PRIMITIVE and not to the shared routine, "because the AOT
\ seed relocates calls to those helpers by name and must search that wordlist".
\ The pins in LPROTWIDQ exist so no post-seal PUBLISH can forge a record into
\ them - a publish-side fact this gate was inheriting by accident. Nothing else
\ can ever be in there: EMIT-STORE-DEF-NAME refuses the publish (test/seal.f pins
\ `2 set-current : X ;` at exit 84), so a callee resolved in wid 2 IS one of the
\ engine's own resident helpers. The chain calls (LP2VEXEC) from 12 sites - every
\ compiled typed fetch emits one.
\
\ (4) AND THE SEAL'S OWN DISCRIMINATOR IS PUBLIC-VERSUS-PRIVATE, which is in the
\ engine rather than in a category name: EMIT-STORE-DEF-NAME refuses DEFINING
\ into a protected wid and C-PACKAGE-SEAL-GUARD refuses OPENING one, while
\ CALLING a public word of a sealed package is what checked source does every
\ day. A gate stated more broadly than the invariant it protects refuses
\ legitimate work the first time a real workload arrives, and one did: the
\ chain's 3 CODE-RECLAIM:WATCH sites and its A64RAV:DKEEP-HOOK-DEFAULT boot-run
\ entry are all public words of sealed packages, and all four died here.
\ A package's PUBLIC wordlist is its record's [0] and its PRIVATE one is [8], so
\ the question is answered by asking the namespace rows rather than by any
\ property of the number. The scan reaches only a wid that survived the three
\ layers above, which on the merged chain is four callees a boot.
\
\ ITS REFUSE LEG HAS NO PRODUCER TODAY, AND IS KEPT ANYWAY. Nothing an artifact
\ can carry reaches it, because two accept-sets in two other files already cover
\ the whole input space:
\   - a CALL SITE's scope is one of four shapes (EM-AOT-PATCH-SITES). A raw
\     pre-window wid is not one of them - it dies `pbad`, exit
\     ENGINE-ERROR:AOT-SEED, which test/aot-wid-suite.f PROBE-WID-FORGED already
\     pins - and a window coordinate is rebased to T0 + (wid - W0), which layer
\     (1) admits BY CONSTRUCTION: the rebase and the layer read the same span.
\   - a BOOT-RUN or CODE-LITERAL name is resolved by LFIND, whose qualifier path
\     (habu1.f FIND-NMATCH) takes the search wid from the package record's [0] -
\     its PUBLIC slot - so a qualified name cannot land in a private wordlist, and
\     a bare one at seed time has no open package and lands in wid 0. The one way
\     to open a sealed package is refused by C-PACKAGE-SEAL-GUARD.
\ So this leg guards an EDIT rather than an input: widen either accept-set - carry
\ a verbatim pre-window wid in the site row, say, which is dot 9d7d8e72's refuted
\ option (b) - and a private callee of a sealed package arrives here and is
\ refused by name instead of being wired in. A defence whose threat lives in
\ another file cannot be justified by a fixture in this one; the layers that DO
\ decide are each falsified by mutation on the real chain (deleting one reds
\ SERIAL-NEXT, (LP2VEXEC), HS-FIELD and WATCH respectively), and test/aot-wid-suite.f
\ PROBE-BOOT-GATE holds the public-slot admit with a two-mode pair.
\
\ IT ASKS THE RECORD THE LOOKUP MATCHED, WHICH IS THE ONLY RECORD THAT ANSWERS.
\ This used to take the xt alone and scan the whole dictionary for the first
\ record whose [0] equalled it. That is not the same question and it is the
\ weaker one: `EXPORT` gives one body a second record under a second name in a
\ second wordlist, so an xt can sit in several rows, and the scan reports the
\ LOWEST-indexed of them - measurably a different wid from the one the name
\ resolved in. A protected wordlist reached through such a name would have been
\ read as its unprotected sibling and admitted. It was also the whole per-boot
\ cost of the pass, O(sites x dictionary): 2.4us per site at 7187 records,
\ measured, which is tens of milliseconds a boot at compiler-chain scale.
\
\ THE POINTER IS CHECKED, NOT TRUSTED. A register handed between two engine
\ routines is worth exactly what the receiver proves about it, so the gate
\ re-establishes here what the old scan got by construction: the record is
\ present, it lies inside the live record array, and its [0] is the xt LFIND
\ returned. Anything else is an engine that no longer agrees with itself, and it
\ dies named rather than resolving the guard away - which the old "no record
\ found, skip the guard" leg would have done, silently, had it ever been
\ reachable.
\ The self-consistency message is lexical, like AOT-XTSITE:PATCH-CHAINS's own:
\ the bytes are emitted after the exit that names them, and the write length is
\ READ OFF the same string rather than counted by hand, so the two cannot drift.
: EM-AOTWIDGATE ( -- )
   LBL LBL LBL LBL LBL LBL LBL  s" hb: AOT gate: lookup record unusable"
   {: wbad:label wdone:label wmsg:label winl:label
      wploop:label wpnext:label wrej:label ma mu :}   \ typed-local-lint: allow-bare-local
   LAOTWIDGATE LABEL@ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,  11 SP 8 STR,           \ save return + xt
   5 wbad CBZ,                                          \ LFIND reported a hit, so a record must be here
   6 5 DBASE SUB,                                       \ x6 = byte offset into the record array
   14 DREC MOVZ,  14 14 NDICT MUL,                      \ x14 = live array length in bytes
   6 14 CMP,  C-CS wbad BCOND,                          \ unsigned: below DBASE wraps high, so one test covers both ends
   14 5 0 LDR,  14 11 CMP,  C-NE wbad BCOND,            \ the record must carry the xt LFIND returned
   9 5 40 LDR,                                          \ x9 = record WID
   \ (1) a wordlist THIS SEED created. One unsigned test refuses both sides, and it
   \ goes first because it answers for 14769 of the chain's 18893 sites.
   \ x13 stays untouched here: it is LPROTWIDQ's result register below, and a value
   \ of this routine's own parked in it would read as carried across that call.
   14 6 AOT-WINDOW:LWIDSPAN LABEL@ TADR,  14 14 0 LDR,  \ x14 = span (x6 = TADR scratch)
   6 DATA AOT-WINDOW:T0-CELL LDR,                       \ x6 = T0, latched by the records pass
   6 9 6 SUB,  6 14 CMP,  C-CC wdone BCOND,
   \ (2) the engine's own API wordlists: no source can publish into either, and
   \ the seed relocates a registered helper's call by name through wid 2.
   9 OWNER-API-PUB-WID CMPI,  C-EQ wdone BCOND,
   9 OWNER-API-PRI-WID CMPI,  C-EQ wdone BCOND,
   \ (3) an ordinary pre-window wordlist.
   LPROTWIDQ LABEL@ BL,                                 \ x13 = protected?
   13 wdone CBZ,
   \ (4) sealed and pre-window: admit it if this wid is some package's PUBLIC
   \ wordlist. A namespace row carries -1 where an ordinary record carries its wid,
   \ its [0] is the package's public wid and its [8] the private one, so the roles
   \ are read off the rows that define them.
   6 DBASE 0 ADDI,  14 NDICT 0 ADDI,
   wploop LBL,
      14 wrej CBZ,
      13 6 40 LDR,  13 13 1 ADDI,  13 wpnext CBNZ,      \ [40] == -1 <=> a namespace row
      13 6 0 LDR,  13 9 CMP,  C-EQ wdone BCOND,         \ its public wordlist: checked source calls it
   wpnext LBL,
      6 6 DREC ADDI,  14 14 1 SUBI,  wploop B,
   wrej LBL,
      1 LPROTAOT LABEL@ ADR,  0 2 MOVZ,  2 PROTAOT-MSG-LEN MOVZ,  NR-WRITE SYS,    \ private word of a sealed package: name it on fd 2 before exit 84
      \ + the callee's own name, read off the record the lookup matched, the way
      \ EMIT-STORE-DEF-NAME appends the def name to its sibling exit. Without it the
      \ gate names a category and no reader can tell which of thousands of baked
      \ names it refused. [16] carries the length in its low bits and DNAME-EXT says
      \ the bytes live outside the record.
      14 5 16 LDR,
      6 DNAME-EXT LIT64,  6 14 6 AND,
      2 14 0 ADDI,  2 2 14 LSLI,  2 2 14 LSRI,          \ x2 = name length
      1 5 24 ADDI,                                      \ x1 = the inline bytes
      6 winl CBZ,  1 5 24 LDR,                          \ EXT: [24] points at them
      winl LBL,
      0 2 MOVZ,  NR-WRITE SYS,
      1 LOPENNL LABEL@ ADR,  0 2 MOVZ,  2 1 MOVZ,  NR-WRITE SYS,
      0 ENGINE-ERROR:SEAL-PACKAGE MOVZ,  NR-EXIT-GROUP SYS,
   wbad LBL,
      1 wmsg ADR,  0 2 MOVZ,  2 mu 1+ MOVZ,  NR-WRITE SYS,   \ +1 = the newline emitted with the bytes
      0 ENGINE-ERROR:AOT-SEED MOVZ,  NR-EXIT-GROUP SYS,
   wmsg LBL,  ma mu BYTES,  NL-KW 1 BYTES,              \ unreachable: both legs above exit_group
   wdone LBL,
      30 SP 0 LDR,  11 SP 8 LDR,  SP SP 16 ADDI,  RET, ;

\ snap-rebase ( base end count dbase dlen newbase -- ): canonicalize a region copy
\ [base,end) in TWO passes so the image is byte-identical whatever VAs this run got.
\ Pass 1 uses the caller's (dbase,dlen,newbase) to fold engine-text pointers to their
\ canonical base (0). Pass 2 folds the live JIT region [DBASE, DBASE+span) -- the only
\ other absolute address class in the copy -- to the RBASE-VA sentinel; span = end-base
\ is the region-payload length. Both bands are disjoint (REGION-OFF >> text size), so
\ the passes never re-touch each other's output. The loader (EM-SNAPSHOT-RESTORE)
\ inverts both: sentinel -> live region base, 0 -> live text base. Pool registers are
\ spilled before any prim call, so clobbering x8/x15/x16/x21/x22/x25 is safe here.
\ x8=base, x16=end are the write-region endpoints. The half-open span guard rejects
\ every protected-band intersection. The legitimate builder uses a high scratch copy.
: BSNAPREBASE ( -- )
   25 G-POP  22 G-POP  21 G-POP  15 G-POP  16 G-POP  8 G-POP
   11 16 8 SUB,  8 11 PROT-GUARD:CALL
   LSNAPRBD LABEL@ BL,
   \ address-literal pass, text band: an xt a `[']` or a `postpone` compiled in may
   \ name a primitive, whose code is in __text, so the same (x21,x22,x25) triple
   \ pass 1 just used for dictionary cells applies to those chains too.
   11 16 8 SUB,                                         \ x11 = region payload length
   SNAP-RELOC:LADDRS LABEL@ BL,
   \ call pass: rewrite every recorded region-to-text call from the distance this
   \ run happens to have to the distance it would have if the region sat exactly
   \ REGION-OFF above __text. x21 still holds the live text base from pass 1.
   10 DBASE 21 SUB,  9 REGION-OFF LIT64,  10 10 9 SUB,  \ x10 = live distance - REGION-OFF
   11 16 8 SUB,                                         \ x11 = region payload length
   SNAP-RELOC:LCALLS LABEL@ BL,
   21 DBASE 0 ADDI,  22 16 8 SUB,  25 RBASE-VA LIT64,   \ pass 2: live region -> RBASE-VA sentinel
   LSNAPRBD LABEL@ BL,
   \ address-literal pass, region band: the quotation entries and the xts that name
   \ a word compiled into the region, folded to the same sentinel pass 2 uses.
   11 16 8 SUB,
   SNAP-RELOC:LADDRS LABEL@ BL, ;

\ The region at rest, established once at the end of EM-STARTUP on BOTH boot
\ paths. It is the only whole-region flip left and the only caller of the
\ unbounded LPROT: everything else flips the window it opened. The cold path
\ needs it because EM-MMAP-CODE-REGION maps the region RW and the narrow closes
\ would then never touch the pages above the seeded code; the snapshot path needs
\ it because the restore writes the whole region. Clearing every band's record
\ belongs here with it: "whole region RX" and "no band open" are one state, and a
\ restored image's DATA copy can otherwise carry a band's range from the run that
\ wrote it.
: EM-SNAPSHOT-RX-FLUSH ( -- )
   2 5 MOVZ,  0 DBASE 0 ADDI,  1 REGION LIT64,  LPROT LABEL@ BL,
   9 0 MOVZ,  9 DATA PROT:WINDOW STR,  9 DATA PROT:WLO STR,
   9 DATA PROT:RLO STR,  9 DATA PROT:RHI STR,  9 DATA PROT:CF STR,
   9 DBASE 0 ADDI,  5 DICT-SIZE LIT64,  9 9 5 ADD,  LFLUSH LABEL@ BL, ;

: EM-SNAPSHOT-VALIDATE-WIDS ( label -- ) {: bad:label :}
   LBL LBL LBL LBL LBL
   {: prot-loop:label prot-max:label prot-inner:label prot-next:label widn:label :}
   5 PROT-BITS-END MOVZ,  7 5 CMP,  C-CC bad BCOND,
   10 12 7 SUB,                                     \ x10 = snapshot DATA source
   11 10 PROT-REG-TAG-CELL LDR,
   5 PROT-REG-TAG LIT64,  11 5 CMP,  C-NE bad BCOND, \ the image's band must be a bitmap
   12 PROT-BITS-OFF MOVZ,  12 10 12 ADD,            \ x12 = &image bitmap[0]
   2 12 0 LDR,  2 2 1 ANDI,  2 bad CBNZ,            \ WID 0 is never a wordlist
   \ x17 = highest protected WID (0 if none): find the top non-zero word, then shift
   \ its bits out. Replaces a per-row running max plus an O(rows^2) duplicate scan --
   \ a bitmap cannot repeat an index, so only the maximum is still worth computing.
   17 0 MOVZ,  8 PROT-BITS-BYTES MOVZ,
   prot-loop LBL,  8 widn CBZ,
      8 8 8 SUBI,
      9 12 8 ADD,  9 9 0 LDR,
      9 prot-max CBNZ,
      prot-loop B,
   prot-max LBL,
      17 8 3 LSLI,  5 0 MOVZ,
   prot-inner LBL,  9 prot-next CBZ,
      9 9 1 LSRI,  5 5 1 ADDI,  prot-inner B,
   prot-next LBL,
      17 17 5 ADD,  17 17 1 SUBI,

   widn LBL,
   6 10 WIDN-CELL LDR,
   14 6 32 LSRI,  14 bad CBNZ,
   6 FIRST-DYNAMIC-WID CMPI,  C-LT bad BCOND,
   6 17 CMP,  C-LS bad BCOND, ;

\ ---- AOT snapshot? (trailer at the end of our own __text). If present:
\ restore both regions verbatim (fixed VAs keep region addresses valid),
\ relocate engine-text call chains (the only ASLR-movers), boot WARM. ----
: EM-SNAPSHOT-RESTORE ( -- )
   LBL LBL LBL LBL LBL LBL LBL
   {: snomag:label snbad:label snok:label snnew:label snhave:label
      snbadver:label snpresent:label :}
   24 0 MOVZ,                                       \ x24 = snapshot flag
   9 DATA RBASE-CELL LDR,  25 9 0 ADDI,             \ x25 = live text CONTENT base
   10 9 0 ADDI,  5 $1000 LIT64,  10 10 5 SUB,
   11 10 IMAGE-TEXT-SIZE-OFF LDR,                   \ S = our executable text size
   12 10 11 ADD,  5 IMAGE-TEXT-TRAILER-ADJ LIT64,  12 12 5 ADD,   \ x12 = trailer END (base+SNL+ADJ)
   \ The executable header is consumed by the OS loader and therefore owns the
   \ snapshot-presence contract. A cold image ends where the emitter stopped, and
   \ LIMGEND is bound there with nothing after it, so its offset from the text
   \ base IS that length (page-rounded on ELF, which pads its text). A larger
   \ authenticated text extent means a snapshot exists, so missing trailer magic
   \ is corruption.
   \ This used to add up the layout instead - LSRC plus the padded baked source -
   \ which was true only while the source was the last thing emitted. It stopped
   \ being true when the AOT payload moved after it (dot
   \ habu-reach-the-seed-d1326596) and the reconstruction read a cold image as a
   \ corrupt snapshot. A label cannot go stale that way.
   14 LIMGEND LABEL@ LOFF,
   HB-TARGET-LINUX? IF
      5 $FFF MOVZ,  14 14 5 ADD,
      5 -$1000 LIT64,  14 14 5 AND,
   THEN
   5 IMAGE-TEXT-CONTENT-ADJ LIT64,  11 11 5 SUB,
   11 14 CMP,  C-LT snbad BCOND,  C-GT snpresent BCOND,
   snomag B,
   snpresent LBL,
   \ The owner-role registry is required for checked qualified lookup. A v0-v2
   \ image has no trustworthy owner semantics, so the legacy trailer and every
   \ version other than the current format fail closed as unsupported.
   5 SNAP-MAGIC LIT64,
   13 12 SNAP-TRL-BYTES SUBI,  14 13 0 LDR,  14 5 CMP,  C-EQ snnew BCOND,       \ x13 = versioned trailer base?
   13 12 SNAP-TRL-LEGACY-BYTES SUBI,  14 13 0 LDR,  14 5 CMP,  C-NE snbad BCOND, \ authenticated snapshot payload without magic is corrupt
   snbadver B,                                                     \ legacy v0 cannot carry owner roles
   snnew LBL,
      14 13 SNAP-TRL-VERSION LDR,                                  \ x14 = image format version
      5 SNAP-FORMAT-VERSION MOVZ,  14 5 CMP,  C-NE snbadver BCOND,
   snhave LBL,
      12 13 0 ADDI,                                                \ x12 = resolved trailer base
   21 12 SNAP-TRL-TBASE LDR,                        \ x21 = snapshot-time text base
   15 12 SNAP-TRL-NDICT LDR,                        \ x15 = ndict
   6 12 SNAP-TRL-REGLEN LDR,                        \ x6 = region payload len
   7 12 SNAP-TRL-DATALEN LDR,                       \ x7 = data payload len
   \ corrupt/truncated trailer must never smear the regions: exit 79
   5 REGION LIT64,  6 5 CMP,  C-GT snbad BCOND,
   5 DICT-SIZE LIT64,  6 5 CMP,  C-LT snbad BCOND,
   5 DATA-SIZE LIT64,  7 5 CMP,  C-GT snbad BCOND,
   5 DICT-CAP MOVZ,  15 5 CMP,  C-GT snbad BCOND,
   SP SP 64 SUBI,
   6 SP 0 STR,  7 SP 8 STR,  11 SP 16 STR,  12 SP 24 STR,
   15 SP 32 STR,  21 SP 40 STR,  25 SP 48 STR,
   snbad EM-SNAPSHOT-VALIDATE-WIDS
   6 SP 0 LDR,  7 SP 8 LDR,  11 SP 16 LDR,  12 SP 24 LDR,
   15 SP 32 LDR,  21 SP 40 LDR,  25 SP 48 LDR,
   SP SP 64 ADDI,
   snok B,
   snbad LBL,                                                              \ corrupt/truncated trailer: label the fd-2 diagnostic before exit 79
      1 LSNAPBAD LABEL@ ADR,  0 2 MOVZ,  2 SNAPBAD-MSG-LEN MOVZ,  NR-WRITE SYS,
      0 79 MOVZ,  NR-EXIT-GROUP SYS,
   snbadver LBL,                                                           \ E-SNAP-VERSION: image format newer than engine supports; label before exit 80
      1 LSNAPVER LABEL@ ADR,  0 2 MOVZ,  2 SNAPVER-MSG-LEN MOVZ,  NR-WRITE SYS,
      0 80 MOVZ,  NR-EXIT-GROUP SYS,
   snok LBL,
   9 DATA ARGC-CELL LDR,  10 DATA ARGV-CELL LDR,  0 DATA ENVP-CELL LDR,
   8 12 7 SUB,  8 8 6 SUB,                          \ region payload src
   EM-SNAPSHOT-COPY-CODE
   EM-SNAPSHOT-COPY-DATA
   25 DATA RBASE-CELL STR,                          \ live values over stale copies
   XDS DATA S0-CELL STR,
   9 DATA ARGC-CELL STR,  10 DATA ARGV-CELL STR,  0 DATA ENVP-CELL STR,
   NDICT 15 0 ADDI,
   CP DBASE 6 ADD,
   8 DBASE 0 ADDI,
   \ region pass FIRST: the RBASE-VA sentinel -> live region base (DBASE). Running it
   \ before the text pass keeps each pass's live output out of the other's canonical
   \ detection band, correct on both non-PIE (Linux) and PIE (macOS, where the region
   \ slides with __text). x6 = region payload len.
   21 RBASE-VA LIT64,  22 6 0 ADDI,  25 DBASE 0 ADDI,
   LSNAPRBD LABEL@ BL,
   \ The address-literal pass runs once per band, right behind the dictionary walk
   \ that band belongs to, and takes the same (x21,x22,x25) triple. It scans the
   \ live region, so it needs x8 and the payload length in x11 -- and x11 still
   \ holds the snapshot text size the text pass below has to have, so the three
   \ values that pass clobbers are parked on the machine stack across both calls.
   SP SP 32 SUBI,  6 SP 0 STR,  7 SP 8 STR,  11 SP 16 STR,
   11 6 0 ADDI,
   SNAP-RELOC:LADDRS LABEL@ BL,
   6 SP 0 LDR,  7 SP 8 LDR,  11 SP 16 LDR,
   \ text pass: canonical base 0 -> live text base. x22 = engine text len; x25 reloaded.
   21 0 MOVZ,  22 11 6 SUB,  22 22 7 SUB,  22 22 SNAP-TRL-BYTES SUBI,  25 DATA RBASE-CELL LDR,
   LSNAPRBD LABEL@ BL,
   11 6 0 ADDI,  8 DBASE 0 ADDI,
   SNAP-RELOC:LADDRS LABEL@ BL,
   6 SP 0 LDR,  7 SP 8 LDR,  11 SP 16 LDR,
   SP SP 32 ADDI,
   \ call pass, the inverse of the writer's: the image carries every region-to-text
   \ call at the canonical REGION-OFF distance, so add back the difference between
   \ that and the distance this run actually got. The call map itself arrived with
   \ the DATA copy above and is indexed by region offset, so it needs no rebasing.
   10 REGION-OFF LIT64,  9 DBASE 25 SUB,  10 10 9 SUB,  \ x10 = REGION-OFF - live distance
   11 6 0 ADDI,  8 DBASE 0 ADDI,                        \ x11 = region payload length, x8 = live region
   SNAP-RELOC:LCALLS LABEL@ BL,
   \ address-cell pass: every DATA cell that was declared to hold a region address
   \ arrived relative to the RBASE-VA sentinel (snap-lib.f canonicalised it on the
   \ way out), so map it onto the region this run actually got. DATA is copied
   \ verbatim, so nothing else would ever move these cells.
   10 DBASE 0 ADDI,  9 RBASE-VA LIT64,  10 10 9 SUB,
   SNAP-RELOC:LXT LABEL@ BL,
   24 1 MOVZ,
   24 DATA SNAP-CELL STR,
   snomag LBL, ;

: EM-STARTUP-RUNTIME-STATE ( -- )
   LBL LBL LBL {: cwok:label pwclr:label pwclrd:label :}
   9 0 MOVZ,  9 DATA HND-CELL STR,
   9 DATA SNAP-CELL LDR,
   9 cwok CBNZ,

   9 0 MOVZ,  9 DATA CUR-CELL STR,
   9 FIRST-DYNAMIC-WID MOVZ,  9 DATA WIDN-CELL STR,
   9 0 MOVZ,  9 DATA HOOK-CELL STR,  9 DATA COMPILE-PREFLIGHT-CELL STR,
   9 DATA TOP-HOOK-CELL STR,
   \ The three engine hook cells hold execution tokens once something installs
   \ them, so they are address cells like a deferred word's dispatch cell. They are
   \ declared here, by name, on the cold path only: a restored image already
   \ carries the declarations the writing run made, and re-declaring is not the
   \ same thing as declaring twice only because the table refuses duplicates.
   HOOK-CELL SNAP-RELOC:MARK-CELL
   COMPILE-PREFLIGHT-CELL SNAP-RELOC:MARK-CELL
   TOP-HOOK-CELL SNAP-RELOC:MARK-CELL
   \ Constructor registry starts empty: clear the whole bitmap, then publish the shape
   \ tag. The old count cell made "empty" a single store; a bitmap has to be zeroed in
   \ full, and a cold boot is the only path that may do it (a restored image carries
   \ the declarations its writing run made).
   9 0 MOVZ,
   10 PROT-BITS-OFF MOVZ,  10 DATA 10 ADD,
   11 PROT-BITS-BYTES MOVZ,
   pwclr LBL,  11 pwclrd CBZ,
      9 10 0 STR,  10 10 8 ADDI,  11 11 8 SUBI,  pwclr B,
   pwclrd LBL,
   9 PROT-REG-TAG LIT64,  9 DATA PROT-REG-TAG-CELL STR,
   LAOTPROT LABEL@ BL,                       \ baked entries precede cold-prefix registrations
   cwok LBL,  9 0 MOVZ,
   9 DATA ENGINE-SNAP-XT-CELL STR,
   9 DATA REPLH-CELL STR,
   9 DATA AOT-SEED-DONE-CELL STR,
   9 DATA BOOT-SRC:USER-END STR,
   9 DATA PKG-PUB-CELL STR,  9 DATA PKG-PRI-CELL STR,  9 DATA PKG-PARENT-CELL STR,  9 DATA PKG-REC-CELL STR,  9 DATA LOOPSP-CELL STR,
   9 DATA PKGRESYNC-CELL STR,                            \ checker resync latch clear at boot (dot habu-recovery-pkg-scope)
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
   EM-STARTUP-RUNTIME-STATE
   EM-SNAPSHOT-RX-FLUSH ;                \ the region at rest: whole region RX, no window open

\ Checker package-scope resync drain (dot habu-recovery-pkg-scope-e0bd98e2). A
\ compile-error recovery rolls the ENGINE package scope back to the boundary
\ (PKGSNAP / RPKG); the checker keeps its OWN package scope (CHECKER-PACKAGE-MODE/
\ NAME/U). Rolling back one without the other would introduce an engine/checker
\ desync (today both dangle together). The recovery legs arm PKGRESYNC-CELL; this
\ runs once at the LMAIN loop top and, when the restored engine scope is GLOBAL
\ (PKG-PUB==0), resets the checker to NONE via the existing global checker-end-package
\ so the pair stays consistent. (Exotic residual, dotted: a package legitimately open
\ ACROSS the boundary whose failing input also changed package scope leaves the
\ checker at the inner scope; the engine still restores the boundary correctly.)
\ Hot path is one LDR + CBZ per token when the flag is clear.
: EM-PKG-RESYNC ( -- )
   LBL {: nosync:label :}
   9 DATA PKGRESYNC-CELL LDR,  9 nosync CBZ,
      9 0 MOVZ,  9 DATA PKGRESYNC-CELL STR,
      9 DATA PKG-PUB-CELL LDR,  9 nosync CBNZ,        \ engine scope non-global -> leave the checker as is
         LCHKENDPKG 19 nosync C-FIND-CHECKER          \ x11 = checker-end-package XT (nosync if absent: cold load / no checker)
         C-CALL-X11-SAVED
   nosync LBL, ;

: EM-COMMENT ( -- )
   LBL LBL LBL {: notcom skln skpar :}
   LMAIN LABEL@ LBL,
      EM-PKG-RESYNC
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

\ EM-INTERPRET-COLON and the checker-callback rows bridge token dispatch to
\ dynamically found checker package words.
\ Retirement: habu-builder-trust-rows-c5d41af6.
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
      1 CP 4 ADDI,  PROT:LOPEN LABEL@ BL,
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
      1 9 0 ADDI,  2 DREC MOVZ,  PROT:LSPAN LABEL@ BL,   \ the record this body will publish
      C-STORE-DEF-NAME
      CP 9 0 STR,
      PROT:LCF LABEL@ BL,                                \ the control-flow depth reset below
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
   LBL {: done:label :}
   LCHKPACKAGE 15 done C-FIND-CHECKER
   9 DATA TKA-CELL LDR,  9 G-PUSH
   9 DATA TKL-CELL LDR,  9 G-PUSH
   C-CALL-X11-SAVED
   done LBL, ;
s" c-call-checker-package" s" --" TRUST

: C-CALL-CHECKER-PUBLIC ( -- )
   LBL {: done:label :}
   LCHKPUB 14 done C-FIND-CHECKER
   C-CALL-X11-SAVED
   done LBL, ;
s" c-call-checker-public" s" --" TRUST

: C-CALL-CHECKER-PRIVATE ( -- )
   LBL {: done:label :}
   LCHKPRI 15 done C-FIND-CHECKER
   C-CALL-X11-SAVED
   done LBL, ;
s" c-call-checker-private" s" --" TRUST

: C-CALL-CHECKER-END-PACKAGE ( -- )
   LBL {: done:label :}
   LCHKENDPKG 19 done C-FIND-CHECKER
   C-CALL-X11-SAVED
   done LBL, ;
s" c-call-checker-end-package" s" --" TRUST

\ Mirror `using NAME` into the checker so its resolution (checker.f
\ CHECKER-FIND-ACTIVE-SYM) sees the same used publics as the engine. Passes the
\ package name token (TKA/TKL); checker-using records it into CHK-USE-NAMES at the
\ current shared USE-DEPTH. Skips (no-op) when the checker word is absent, exactly
\ like the package hooks — the engine still owns USE-DEPTH / USE-WIDS.
: C-CALL-CHECKER-USING ( -- )
   LBL {: done:label :}
   LCHKUSING 13 done C-FIND-CHECKER               \ 13 = len "checker-using"
   9 DATA TKA-CELL LDR,  9 G-PUSH
   9 DATA TKL-CELL LDR,  9 G-PUSH
   C-CALL-X11-SAVED
   done LBL, ;
s" c-call-checker-using" s" --" TRUST

\ C-PACKAGE-FAIL through C-PACKAGE-ENSURE validate names and allocate or reopen
\ the native package record and its public/private wordlists.
\ Retirement: habu-builder-trust-rows-c5d41af6.
: C-PACKAGE-FAIL ( n -- ) {: rc:n :}                  \ package keyword misuse ($4A no name / $4B wrong context): recoverable inside evaluate, fail-closed exit rc at top level
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 rc MOVZ,  LCOMPILEDIE LABEL@ B, ;
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
   1 9 0 ADDI,  2 DREC MOVZ,  PROT:LSPAN LABEL@ BL,   \ the record this package publishes
   C-STORE-NAME
   11 DATA WIDN-CELL LDR,  11 11 2 SUBI,
   12 11 1 ADDI,
   11 9 0 STR,  12 9 8 STR,
   15 0 MOVN,  15 9 40 STR,
   NDICT NDICT 1 ADDI,  LHIDXADD LABEL@ BL,
   5 9 0 ADDI, ;
s" c-package-new-record" s" --" TRUST

\ The record this writes is one the SCAN found: `package NAME` reopening an
\ existing package rewrites that record's private-WID cell, and the record can sit
\ anywhere below the watermark. It is the reason a record band keyed on NDICT
\ would be a lucky value rather than a rule, so it declares the record it found.
: C-PACKAGE-EXISTING-PRIVATE ( label -- ) {: done:label :}
   LBL {: havepri:label :}
   12 havepri CBNZ,
      C-PACKAGE-NEW-PRIVATE-WID
      1 5 0 ADDI,  2 DREC MOVZ,  PROT:LSPAN LABEL@ BL,
      12 5 8 STR,
   havepri LBL,
   done B, ;
s" c-package-existing-private" s" label --" TRUST

: C-PACKAGE-RECORD-MATCH ( label label -- ) {: hit:label miss:label :}
   LBL LBL {: cmp:label inline:label :}
   14 5 40 LDR,  15 0 MOVN,  14 15 CMP,  C-NE miss BCOND,
   14 5 16 LDR,  14 14 14 LSLI,  14 14 14 LSRI,
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

\ C-PACKAGE and the following package/use/export/interpreter rows emit namespace
\ state transitions, token dispatch, and definition return paths.
\ Retirement: habu-builder-trust-rows-c5d41af6.
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
   1 CP 4 ADDI,  PROT:LOPEN LABEL@ BL,
   C-PACKAGE-ENSURE
   PROT:LCLOSE LABEL@ BL,
   \ remember the using-scope depth at package open; `;package` restores it so usings
   \ opened inside this package block end at ;package (x9=addr, x10=value scratch;
   \ x11/x12/x5 hold the package wids/record and stay live for the stores below).
   9 USE-DEPTH-CELL LIT64,  9 DATA 9 ADD,  10 9 0 LDR,
   9 USE-PKG-SAVE-CELL LIT64,  9 DATA 9 ADD,  10 9 0 STR,
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
   9 DATA PKG-PARENT-CELL STR,  9 DATA PKG-REC-CELL STR,
   \ restore the using-scope depth to the package-open boundary: usings opened inside
   \ this package block end here (the checker reads the same shared USE-DEPTH).
   9 USE-PKG-SAVE-CELL LIT64,  9 DATA 9 ADD,  10 9 0 LDR,
   9 USE-DEPTH-CELL LIT64,  9 DATA 9 ADD,  10 9 0 STR, ;
s" c-end-package" s" --" TRUST

\ ---- `using NAME` / `;using`: consumer-side package import (dot habu-using-import-pkg-a07dd7ba) ----
\ `using NAME` makes package NAME's PUBLIC wordlist visible to bare lookup until the
\ matching `;using`, the enclosing `;package`, or the end of the load file. Only NAME's
\ public WID joins the search (privates stay invisible), definitions still target the
\ current scope's wordlist (nothing lands in NAME), and qualified NAME:WORD is unchanged.
\ The used-publics search (EMIT-FIND-USED) runs only after the open-scope + global chain
\ misses, so `using` is purely additive and can never silently shadow an existing binding;
\ a bare tail resolving in two used packages is a hard error.

: C-USING-NAME-GUARD ( -- )   \ consume the next token; reject a missing name / a ':' in it
   LBL LBL LBL LBL LBL LBL {: mmiss:label hastok:label cscan:label cbad:label cmsg:label cok:label :}
   LTOK LABEL@ BL,  0 hastok CBNZ,
      0 2 MOVZ,  1 mmiss ADR,  2 31 MOVZ,  NR-WRITE SYS,
      0 2 MOVZ,  1 LQNL LABEL@ ADR,  1 1 1 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
      0 89 MOVZ,  LCOMPILEDIE LABEL@ B,               \ 89 = ENGINE-ERROR:USING-NO-NAME
   mmiss LBL,  s" hb: using: missing package name" BYTES,
   hastok LBL,
   14 0 MOVZ,
   cscan LBL,
      15 DATA TKL-CELL LDR,  14 15 CMP,  C-GE cok BCOND,
      15 DATA TKA-CELL LDR,  15 15 14 ADD,  15 15 0 LDRB,  15 $3A CMPI,  C-EQ cbad BCOND,
      14 14 1 ADDI,  cscan B,
   cbad LBL,
      0 2 MOVZ,  1 cmsg ADR,  2 46 MOVZ,  NR-WRITE SYS,
      90 C-DIE-TOKEN-NL                               \ 90 = ENGINE-ERROR:USING-BAD-NAME
   cmsg LBL,  s" hb: using: package name must not contain ':': " BYTES,
   cok LBL, ;
s" c-using-name-guard" s" --" TRUST

: C-USING-WID ( -- )   \ TKA/TKL name a package -> x2 = its public WID; die unknown otherwise
   \ The namespace rows (wid DICT-WL:NAMESPACE) are ordinary published records,
   \ inserted by the same LHIDXADD as every definition and dup-walled by
   \ C-PACKAGE-ENSURE (one record per package name), so `using` resolves its
   \ package through the same one-wordlist hash probe the name lookup uses
   \ (CG-26): hash the token once, walk the (name XOR -1) chain, and let
   \ C-PACKAGE-RECORD-MATCH - the same word the scan judges a candidate with -
   \ judge each probed record. An empty slot is the scan's own "no such
   \ package". The scan below stays authoritative for the pre-LHIDXBUILD
   \ window and the full-wrap chain, exactly as in LFIND.
   LBL LBL LBL LBL LBL LBL LBL LBL LBL
   {: loop:label miss:label hit:label notfound:label umsg:label done:label
      lscan:label plp:label pnx:label :}
   14 DATA HIDXP-CELL LDR,  14 lscan CBZ,          \ no table -> the scan answers
   16 DATA TKA-CELL LDR,  15 DATA TKL-CELL LDR,
   16 15 3 4 5 7 C-HIDX-HASH                       \ x3 = folded-name hash
   4 0 MOVN,
   6 3 4 EOR,  5 HIDX-SLOTS 1 - LIT64,  6 6 5 AND, \ x6 = slot = (hash XOR NAMESPACE) & mask
   8 HIDX-SLOTS LIT64,                             \ x8 = step budget
   plp LBL,
      5 6 2 LSLI,  5 14 5 ADD,  7 5 0 LDRW,        \ x7 = slot value (index+1)
      7 notfound CBZ,                              \ empty slot -> no such package
      7 7 1 SUBI,  7 NDICT CMP,  C-GE pnx BCOND,   \ stale (truncated) index
      5 DREC MOVZ,  5 7 5 MUL,  5 DBASE 5 ADD,     \ x5 = candidate record
      hit pnx C-PACKAGE-RECORD-MATCH               \ hit iff wid==-1 && name==token
   pnx LBL,
      14 DATA HIDXP-CELL LDR,                      \ the match word used x14 as scratch
      8 8 1 SUBI,  8 lscan CBZ,                    \ full wrap -> the scan answers
      6 6 1 ADDI,  5 HIDX-SLOTS 1 - LIT64,  6 6 5 AND,  plp B,
   lscan LBL,
   5 DBASE 0 ADDI,  6 NDICT 0 ADDI,
   loop LBL,
      6 notfound CBZ,
      hit miss C-PACKAGE-RECORD-MATCH            \ x5 = candidate; hit iff wid==-1 && name==token
      hit LBL,  2 5 0 LDR,  done B,              \ x2 = record[0] = package public WID
      miss LBL,  5 5 DREC ADDI,  6 6 1 SUBI,  loop B,
   notfound LBL,
      0 2 MOVZ,  1 umsg ADR,  2 28 MOVZ,  NR-WRITE SYS,
      91 C-DIE-TOKEN-NL                               \ 91 = ENGINE-ERROR:USING-UNKNOWN
   umsg LBL,  s" hb: using: unknown package: " BYTES,
   done LBL, ;
s" c-using-wid" s" --" TRUST

: C-USING-PUSH ( -- )   \ x2 = public WID; push it onto the using stack (overflow -> die)
   LBL LBL {: fmsg:label pushok:label :}
   7 USE-DEPTH-CELL LIT64,  7 DATA 7 ADD,  8 7 0 LDR,       \ x8 = live using depth (overflow test)
   14 USE-MAX MOVZ,  8 14 CMP,  C-LT pushok BCOND,
      0 2 MOVZ,  1 fmsg ADR,  2 39 MOVZ,  NR-WRITE SYS,
      92 C-DIE-TOKEN-NL                               \ 92 = ENGINE-ERROR:USING-OVERFLOW
   fmsg LBL,  s" hb: using: too many concurrent usings: " BYTES,
   pushok LBL,
      7 USE-DEPTH-CELL LIT64,  7 DATA 7 ADD,  8 7 0 LDR,   \ reload depth on the accepted path (no cross-syscall live reg)
      14 USE-WIDS-OFF LIT64,  14 DATA 14 ADD,  15 8 3 LSLI,  14 14 15 ADD,  2 14 0 STR,   \ USE-WIDS[depth] = wid
      C-CALL-CHECKER-USING                                 \ mirror name into CHK-USE-NAMES[depth] (reads pre-increment depth)
      7 USE-DEPTH-CELL LIT64,  7 DATA 7 ADD,  8 7 0 LDR,  8 8 1 ADDI,  8 7 0 STR, ;      \ depth++
s" c-using-push" s" --" TRUST

: C-USING ( -- )
   C-TASK-LIVE-GUARD
   C-USING-NAME-GUARD
   C-USING-WID
   C-USING-PUSH ;
s" c-using" s" --" TRUST

: C-END-USING ( -- )
   C-TASK-LIVE-GUARD
   LBL LBL {: ok:label umsg:label :}
   7 USE-DEPTH-CELL LIT64,  7 DATA 7 ADD,  8 7 0 LDR,       \ x8 = live using depth (underflow test)
   8 ok CBNZ,
      0 2 MOVZ,  1 umsg ADR,  2 32 MOVZ,  NR-WRITE SYS,
      0 2 MOVZ,  1 LQNL LABEL@ ADR,  1 1 1 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
      0 93 MOVZ,  LCOMPILEDIE LABEL@ B,               \ 93 = ENGINE-ERROR:USING-UNBALANCED
   umsg LBL,  s" hb: ;using without an open using" BYTES,
   ok LBL,
   7 USE-DEPTH-CELL LIT64,  7 DATA 7 ADD,  8 7 0 LDR,  8 8 1 SUBI,  8 7 0 STR, ;   \ reload depth (no cross-syscall live reg), depth--
s" c-end-using" s" --" TRUST

\ EMIT-FIND-USED (LFINDUSED leaf): searched only after the open-scope + global chain
\ misses. Input x9=token addr, x10=token len; output x13=0 (miss) or FIND-shaped flags
\ (hit) with x11=code x12=body-len. The token is hashed once and each live
\ USE-WIDS[0..depth) public wordlist is asked through the same one-wordlist hash
\ probe LFIND and search-wl answer from - O(depth) chain walks instead of the
\ O(NDICT * depth) whole-dictionary scan this leaf used to make (CG-26). The
\ probe reproduces the scan exactly on these wids for BSWL's reason: a package
\ public wordlist is guarded by the definer's duplicate wall, so it holds at
\ most one live row per folded name. The same package used twice probes the
\ same wid to the same record, which the record-identity dedupe counts once -
\ the scan's own rule. A second DISTINCT record is the ambiguity hard error,
\ exactly as in the scan. The scan is kept below as the authoritative answer
\ for the two windows the probe cannot serve: no table yet (pre-LHIDXBUILD
\ startup), and a chain walked through every slot without meeting an empty one.
\ Qualified tokens (containing ':') never resolve here. Register/name-fold
\ conventions mirror EMIT-FIND's authoritative linear scan; preserves
\ x9/x10/x19(XDS)/x20(DATA). It publishes LFIND's record output too - x5 = the
\ matched record on a hit, 0 on every miss - because a caller that reaches this
\ leaf reaches it INSTEAD of LFIND, and one of the two answering a question the
\ other cannot is how the two shapes drift apart.
: EMIT-FIND-USED ( -- )
   LFINDUSED LABEL@ LBL,
   LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL
   LBL LBL LBL LBL LBL LBL LBL LBL
   {: qscan:label qnone:label ret:label uloop:label mloop:label member:label
      ninl:label ncmp:label nmatch:label unext:label udone:label amb:label ambmsg:label
      lscan:label kloop:label knext:label ploop:label pnext:label pinl:label
      pcmp:label pmatch:label :}
   13 0 MOVZ,
   17 0 MOVZ,
   qscan LBL,
      17 10 CMP,  C-GE qnone BCOND,
      14 9 17 ADD,  14 14 0 LDRB,  14 $3A CMPI,  C-EQ ret BCOND,   \ colon -> not a bare tail; miss
      17 17 1 ADDI,  qscan B,
   qnone LBL,
   14 USE-DEPTH-CELL LIT64,  14 DATA 14 ADD,  8 14 0 LDR,          \ x8 = depth
   8 ret CBZ,                                                       \ no usings -> miss
   7 USE-WIDS-OFF LIT64,  7 DATA 7 ADD,                            \ x7 = &USE-WIDS[0]
   14 DATA HIDXP-CELL LDR,  14 lscan CBZ,                          \ no table -> the scan answers
   9 10 15 4 5 17 C-HIDX-HASH                                      \ x15 = folded-name hash, once
   16 0 MOVZ,  6 0 MOVZ,                                            \ x16 = matched record (0=none), x6 = match count
   3 0 MOVZ,                                                        \ x3 = used-wid index
   kloop LBL,
      3 8 CMP,  C-GE udone BCOND,                                  \ every wid asked -> shared verdict
      2 3 3 LSLI,  2 7 2 ADD,  2 2 0 LDR,                          \ x2 = USE-WIDS[x3]
      5 15 2 EOR,  4 HIDX-SLOTS 1 - LIT64,  5 5 4 AND,             \ x5 = slot = (hash XOR wid) & mask
      4 HIDX-SLOTS LIT64,                                          \ x4 = step budget
   ploop LBL,
      12 5 2 LSLI,  12 14 12 ADD,  11 12 0 LDRW,                   \ x11 = slot value (index+1)
      11 knext CBZ,                                                \ empty slot -> absent from this wid
      11 11 1 SUBI,  11 NDICT CMP,  C-GE pnext BCOND,              \ stale (truncated) index
      12 DREC MOVZ,  12 11 12 MUL,  12 DBASE 12 ADD,               \ x12 = record ptr
      11 12 40 LDR,  11 2 CMP,  C-NE pnext BCOND,                  \ wid mismatch
      11 12 16 LDR,  11 11 14 LSLI,  11 11 14 LSRI,  11 10 CMP,  C-NE pnext BCOND,  \ name-len mismatch
      17 12 24 ADDI,
      11 12 16 LDR,  11 11 DNAME-EXT ANDI,  11 pinl CBZ,
         17 12 24 LDR,
      pinl LBL,
      13 0 MOVZ,
      pcmp LBL,
         \ x2 and x11 carry the two bytes, x12 is the shared fold scratch; the
         \ record pointer is recomputed from the slot at pmatch and the wid is
         \ reloaded from USE-WIDS[x3] at pnext, so both survive by re-derivation
         13 10 CMP,  C-GE pmatch BCOND,
         11 17 13 ADD,  11 11 0 LDRB,
         12 11 $41 SUBI,  12 $1A CMPI,  12 C-CC CSET,  12 12 5 LSLI,  11 11 12 ORR,
         2 9 13 ADD,  2 2 0 LDRB,
         12 2 $41 SUBI,  12 $1A CMPI,  12 C-CC CSET,  12 12 5 LSLI,  2 2 12 ORR,
         11 2 CMP,  C-NE pnext BCOND,
         13 13 1 ADDI,  pcmp B,
      pmatch LBL,
         12 5 2 LSLI,  12 14 12 ADD,  12 12 0 LDRW,                \ x12 = this slot's record again
         12 12 1 SUBI,  11 DREC MOVZ,  12 12 11 MUL,  12 DBASE 12 ADD,
         12 16 CMP,  C-EQ knext BCOND,                             \ same record again (same package used twice)
         16 12 0 ADDI,
         6 6 1 ADDI,  6 2 CMPI,  C-GE amb BCOND,                   \ 2nd distinct record -> ambiguous
   knext LBL,
      3 3 1 ADDI,  kloop B,
   pnext LBL,
      2 3 3 LSLI,  2 7 2 ADD,  2 2 0 LDR,                          \ reload x2 = USE-WIDS[x3]
      4 4 1 SUBI,  4 lscan CBZ,                                    \ full wrap -> the scan answers
      5 5 1 ADDI,  12 HIDX-SLOTS 1 - LIT64,  5 5 12 AND,  ploop B,
   lscan LBL,
   16 0 MOVZ,  6 0 MOVZ,                                            \ x16 = matched record (0=none), x6 = match count
   5 DBASE 0 ADDI,  4 NDICT 0 ADDI,
   uloop LBL,
      4 udone CBZ,
      14 5 40 LDR,                                                 \ x14 = record wid
      3 0 MOVZ,
      mloop LBL,
         3 8 CMP,  C-GE unext BCOND,                               \ record wid not among used wids -> skip
         2 3 3 LSLI,  2 7 2 ADD,  15 2 0 LDR,                      \ x15 = USE-WIDS[x3]
         15 14 CMP,  C-EQ member BCOND,
         3 3 1 ADDI,  mloop B,
      member LBL,
         15 5 16 LDR,  15 15 14 LSLI,  15 15 14 LSRI,  15 10 CMP,  C-NE unext BCOND,   \ name-len mismatch
         2 5 24 ADDI,
         14 5 16 LDR,  14 14 DNAME-EXT ANDI,  14 ninl CBZ,
            2 5 24 LDR,
         ninl LBL,
         17 0 MOVZ,
         ncmp LBL,
            17 10 CMP,  C-GE nmatch BCOND,
            15 2 17 ADD,  15 15 0 LDRB,
            3 15 $41 SUBI,  3 $1A CMPI,  3 C-CC CSET,  3 3 5 LSLI,  15 15 3 ORR,
            14 9 17 ADD,  14 14 0 LDRB,
            3 14 $41 SUBI,  3 $1A CMPI,  3 C-CC CSET,  3 3 5 LSLI,  14 14 3 ORR,
            15 14 CMP,  C-NE unext BCOND,
            17 17 1 ADDI,  ncmp B,
         nmatch LBL,
            6 6 1 ADDI,  16 5 0 ADDI,                              \ count++, remember record
            6 2 CMPI,  C-GE amb BCOND,                             \ 2nd distinct match -> ambiguous
      unext LBL,  5 5 DREC ADDI,  4 4 1 SUBI,  uloop B,
   udone LBL,
   13 0 MOVZ,                                                      \ the probes used x13 as a byte index
   16 ret CBZ,                                                     \ no match -> x13=0 miss
   5 16 0 ADDI,
   11 5 0 LDR,  12 5 8 LDR,
   14 5 16 LDR,
   15 14 DNAME-WIDE ANDI,  15 15 59 LSRI,
   16 14 DNAME-INT ANDI,  16 16 59 LSRI,
   15 15 16 ORR,
   16 14 DNAME-MIN-IN-MASK ANDI,  16 16 44 LSRI,
   15 15 16 ORR,
   14 14 DNAME-IMM ANDI,  14 14 59 LSRI,
   14 14 15 ORR,
   13 1 MOVZ,  13 13 14 ORR,
   RET,                                                            \ hit: x5 is the matched record, LFIND's own output shape
   ret LBL,  5 0 MOVZ,  RET,                                       \ every miss leaves x5 = 0, including the two that never wrote it
   amb LBL,
      0 2 MOVZ,  1 ambmsg ADR,  2 60 MOVZ,  NR-WRITE SYS,
      PROT:LCLOSE LABEL@ BL,                                  \ region -> RX (idempotent; a compile-path caller is RW here)
      94 C-DIE-TOKEN-NL                               \ 94 = ENGINE-ERROR:USING-AMBIGUOUS
   ambmsg LBL,  s" hb: ambiguous bare word resolves in multiple used packages: " BYTES, ;
s" emit-find-used" s" --" TRUST

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
\ ENGINE-ERROR:SEAL-PACKAGE), undefined word (token + rc 70), duplicate tail
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
   13 found CBNZ,                                          \ EXPORT <undefined>: recoverable inside evaluate (rc 70), fail-closed exit 70 at top level. C-EXPORT's own state is clean (only LTOK + LFIND ran; no SP push, no publish). Caveat (universal, not new): if the failing string ALSO opened the package (package X public export NOPE), the recovery restores the dictionary + compile state but not package scope, so X stays open — identical to the landed LUNDEF/dup-def in-package recoveries; the session is usable for fresh top-level defines. Package-scope rollback is a separate follow-up.
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 70 MOVZ,  LCOMPILEDIE LABEL@ B,
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
   1 CP 4 ADDI,  PROT:LOPEN LABEL@ BL,
   C-EXPORT-TAIL!                                        \ tail again for the publish (pure rescan)
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   1 9 0 ADDI,  2 DREC MOVZ,  PROT:LSPAN LABEL@ BL,      \ the record this export publishes
   C-STORE-DEF-NAME
   14 SP 0 LDR,  14 9 0 STR,                            \ [0] = source code ptr
   14 SP 8 LDR,  14 9 8 STR,                            \ [8] = source body len
   14 SP 16 LDR,                                        \ x14 = LFIND flags
   15 9 16 LDR,
   16 14 2 ANDI,  16 16 59 LSLI,  15 15 16 ORR,         \ flag bit1 -> DNAME-IMM
   16 14 8 ANDI,  16 16 59 LSLI,  15 15 16 ORR,         \ flag bit3 -> DNAME-WIDE
   16 14 $FF00 ANDI,  16 16 44 LSLI,  15 15 16 ORR,     \ flag bits 8-15 -> DNAME-MIN-IN (same body, same certified arity)
   15 9 16 STR,
   NDICT NDICT 1 ADDI,  LHIDXADD LABEL@ BL,
   PROT:LCLOSE LABEL@ BL,
   SP SP 32 ADDI,
   done LBL, ;
s" c-export" s" --" TRUST

\ Interpret-state dispatch assembly: the keyword/number/find emitters and the
\ EM-INTERPRET entry that EMIT-MAIN wires into the main loop.
package INTERP-EMIT

: EM-INTERPRET-DEFINE-KEYWORDS ( -- )
   s" package" KEEP? IF LMAIN LABEL@ LKWPACKAGE 7 ['] C-PACKAGE CF-ENTRY THEN
   s" public" KEEP? IF LMAIN LABEL@ LKWPUBLIC 6 ['] C-PUBLIC CF-ENTRY THEN
   s" private" KEEP? IF LMAIN LABEL@ LKWPRIVATE 7 ['] C-PRIVATE CF-ENTRY THEN
   s" ;package" KEEP? IF LMAIN LABEL@ LKWSEMIPACKAGE 8 ['] C-END-PACKAGE CF-ENTRY THEN
   s" using" KEEP? IF LMAIN LABEL@ LKWUSING 5 ['] C-USING CF-ENTRY THEN
   s" ;using" KEEP? IF LMAIN LABEL@ LKWSEMIUSING 6 ['] C-END-USING CF-ENTRY THEN
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
   12 lnotnum CBZ,  11 G-PUSH
   TOP-EV-NUM C-TOPHOOK-LIT
   LMAIN LABEL@ B, ;
s" em-interpret-number" s" label --" TRUST

: EM-INTERPRET-FIND ( -- )
   LBL LBL LBL {: depthok:label usedtry:label found:label :}
   9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFIND LABEL@ BL,
   13 usedtry CBZ,                                     \ open-scope + global miss -> try used publics
   found LBL,
   14 13 8 ANDI,  14 LWIDE LABEL@ CBNZ,                \ DNAME-WIDE effect (TFAM): fail closed, never land a bundle on the interpret stack (x13 still holds the LFIND dict flags)
   14 13 16 ANDI,  14 LINTERNAL LABEL@ CBNZ,           \ DNAME-INT: engine-internal word with no checker-known effect - fail closed before the body runs on the untyped interpret stack
   14 13 $FF00 ANDI,  14 depthok CBZ,                  \ DNAME-MIN-IN (x13 bits 8-15): certified min input arity; 0 = unguarded boundary
      14 14 8 LSRI,                                    \ x14 = min-in cells
      9 DATA S0-CELL LDR,  10 XDS 9 SUB,  10 10 3 ASRI, \ x10 = interpret depth in cells
      10 14 CMP,  C-LT LMININ LABEL@ BCOND,            \ depth < declared inputs -> named reject BEFORE the body can read below base
   depthok LBL,
   LARITY LABEL@ BL,          \ pre-exec arity guard (fable): deref/execute prims fault on a shallow
                              \ stack before the LMAIN depth-floor guard sees it; diverts to LUNDERFLOW
   C-TOPHOOK-CALL             \ every native gate passed: emit the pre-BLR word event (x13 = LFIND flags)
   11 BLR,  LMAIN LABEL@ B,
   usedtry LBL,
      9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFINDUSED LABEL@ BL,   \ used-publics resolution (ambiguity dies)
      13 LUNDEF LABEL@ CBZ,                            \ still nothing -> undefined
      found B, ;                                       \ resolved via a used package: rejoin the normal dispatch
s" em-interpret-find" s" --" TRUST

: EM-INTERPRET-WORDS ( -- )
   LBL {: lnotnum :}
   EM-INTERPRET-DEFINE-KEYWORDS s" interpret/define" ENGINE-SIZE:MARK
   EM-INTERPRET-STRING-KEYWORDS s" interpret/string" ENGINE-SIZE:MARK
   lnotnum EM-INTERPRET-NUMBER
   lnotnum LBL,               s" interpret/number" ENGINE-SIZE:MARK
   EM-INTERPRET-FIND          s" interpret/find" ENGINE-SIZE:MARK ;
s" em-interpret-words" s" --" TRUST

public

: EM-INTERPRET ( -- )
   LBL {: lnotcolon :}
   lnotcolon EM-INTERPRET-COLON s" interpret/colon" ENGINE-SIZE:MARK
   EM-INTERPRET-WORDS ;
s" em-interpret" s" --" TRUST

;package

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
\ Registered as the sealed (LP2VEXEC) engine helper, exactly like (PROT-SPAN):
\ the direct BL that LP2VEMIT plants in a compiled fetch is resolved by exact
\ record code-entry in an ahead-of-time image (aot-closure.f FINDADDR-PTR) and
\ relocated as an in-image branch, instead of shipping the build-time engine
\ address and crashing a stripped image. The invalid-tag diagnostic is inlined
\ inside the record so its ADR stays record-relative and is relocated with the
\ body when the closure copies it (no reference to engine message data that a
\ stripped image does not carry).
19 constant LVP-BADTAG-LEN   \ "hb: bad layout tag\n" (18-byte message + newline)
: EMIT-P2-VALID-EXEC ( -- )
   LBL LBL LBL LBL LBL LBL {: outer:label guard:label active:label inactive:label invalid:label done:label :}
   LBL LBL {: badmsg:label end:label :}
   LP2VEXEC LABEL@ {: start:label :}
   s" (LP2VEXEC)" start LABEL>N end LABEL>N ENGINE-HELPER:REGISTER
   start LBL,
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
   1 badmsg ADR,  2 LVP-BADTAG-LEN MOVZ,  NR-WRITE SYS,            \ write(2,"hb: bad layout tag\n",19)
   0 ENGINE-ERROR:BAD-TAG MOVZ,  NR-EXIT-GROUP SYS,
   done LBL,
   RET,
   badmsg LBL,  S\" hb: bad layout tag\n" BYTES,                   \ inline data (msg+newline, contiguous): relocation-safe within the registered record
   end LBL, ;

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
   LCEMITBL LABEL@ BL,                                \ direct BL to the registered (LP2VEXEC) helper
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
   LCEMITBL LABEL@ BL,                                \ direct BL to the registered (PROT-SPAN) helper
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
   missing LBL,  10 1 MOVZ,  11 0 MOVZ,  RET,          \ x11=0: not found (default width 1)
   keyeq LBL,
      15 11 1 cells LDR,  14 15 CMP,  C-EQ found BCOND,  C-HI drift BCOND,
      10 1 MOVZ,  11 0 MOVZ,  RET,                     \ pos below cursor row: not found
   found LBL,
      10 11 2 cells LDR,
      13 13 1 ADDI,  13 DATA TXN-WF-I-CELL STR,  11 1 MOVZ,  RET,   \ x11=1: found (layout-cap slice 4 xpad legs read the flag)
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

;package

: EMIT-P2-HELPERS ( -- )
   EMIT-P2-COPY  EMIT-P2-DROPN  EMIT-P2-REV  EMIT-P2-ROT  EMIT-P2-RS
   LOWER-TXN-LOOKUP:EMIT-LOOKUPS
   EMIT-P2-VALID-EXEC  EMIT-P2-VALID-EMIT  EMIT-P2-FETCH  EMIT-P2-STORE ;

\ keyword/certificate-name bytes for the pass-2 dispatch (the shuffle-op and rs
\ keyword names reuse the jit.f/loop-keyword labels).
: EMIT-P2KW ( -- )
   LCERTBYTES LABEL@ LBL, s" lower-cert:bytes" BYTES,
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

\ Pass-2 and compiler rows dispatch width-aware emitters, rerun/freeze publication,
\ and emit the control, literal, operator, call, and reset paths.
\ Retirement: habu-builder-trust-rows-c5d41af6.
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
   LBL LBL LBL LBL LBL LBL LBL {: loop:label ordered:label flags:label fetch:label advance:label done:label xdone:label :}
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
      15 17 7 ANDI,  15 17 CMP,  C-NE bad BCOND,      \ flags subset {fetch,store,xpad}
      17 3 CMPI,  C-EQ bad BCOND,
      15 17 LOWER-CERT:XPAD-FLAG ANDI,  15 xdone CBZ, \ layout-cap slice 4: xpad row marks needs-p2
         9 1 MOVZ,
   xdone LBL,
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

;package

\ pass-2 entry: the hook produced a sealed lowering transaction. Save the live
\ input, repoint the tokenizer at its frozen source span, rewind CP to
\ the colon entry (the name bytes stay) and DP to the definition watermark,
\ reset the per-definition compile state exactly as EM-INTERPRET-COLON does,
\ re-emit the prologue, and re-run the compile loop width-aware.
\
\ THE REWIND OWNS EVERY RECORD KEYED ON THE CURSOR IT REWINDS, and two are: the
\ region-to-text call map and the address-literal map, both indexed by region
\ offset and both written AT CP as pass 1 emitted (SNAP-RELOC:MARK-SITE from
\ C-CODE-ADDR / C-DATA-ADDR / C-DATA-ADDR-RAW, the record the inliner's copy loop
\ reissues for a chain it duplicates, and EMIT-CEMITBL for a call into the
\ engine's loaded __text). Pass 2 lowers the same body with the widths it now
\ knows, so its stream has different lengths and its records land at different
\ offsets; a bit pass 1 set at a word pass 2 fills with something else is a
\ record of code that no longer exists.
\ Both maps' readers treat a bit as authoritative - EMIT-ADDRS
\ requires the four move-wide words and EMIT-CALLS requires a BL, and each
\ refuses the image outright when the bytes disagree - so the stale bit is not a
\ wasted bit, it is an image this engine will not load, and the AOT capture
\ refuses the same site one build step earlier (aot-capture.f ACAP-UNCLASSIFIED).
\ So the span pass 1 emitted is cleared in both maps in the same breath as the
\ two cursors are put back. It is DP's watermark's counterpart for CP.
\
\ The span is exactly [entry, pass-1 CP): the entry is the pending definition's
\ own code field, read from PEND-CELL, which is where CP is being put back to,
\ and CP is still where pass 1 left it. Nothing below the entry is touched, so a
\ record any earlier definition owns is untouched by construction.
\ The maps live in DATA, which is a separate fixed mapping from the JIT region
\ the protection bands cover, so this write needs no band declaration - the same
\ reason EMIT-ADDR-SITE and EMIT-CEMITBL need none.
: EM-P2-START ( -- )
   1 CP 4 ADDI,  PROT:LOPEN LABEL@ BL,
   LOWER-TXN:SEAL
   9 DATA INP-CELL LDR,  9 DATA P2INP-CELL STR,
   9 DATA INE-CELL LDR,  9 DATA P2INE-CELL STR,
   10 DATA TXN-SRC-A-CELL LDR,
   9 DATA P2BODY0-CELL LDR,  9 10 9 ADD,  9 DATA INP-CELL STR,
   9 DATA TXN-SRC-U-CELL LDR,  9 10 9 ADD,  9 DATA INE-CELL STR,
   11 DATA PEND-CELL LDR,  10 11 0 LDR,               \ x10 = the colon entry
   9 CP 10 SUB,                                       \ x9 = the span pass 1 emitted from it
   SNAP-RELOC:CALLMAP-OFF SNAP-RELOC:CLEAR-SPAN,      \ x9/x10 survive the clear
   SNAP-RELOC:ADDRMAP-OFF SNAP-RELOC:CLEAR-SPAN,
   CP 10 0 ADDI,                                      \ CP back to the colon entry: below the band
   9 DATA P2DP-CELL LDR,  9 DATA DP-CELL STR,
   PROT:LCF LABEL@ BL,                                \ the control-flow depth reset below
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

\ The body length lands in the record at the SECOND end of the definition, and a
\ declaration does not survive that far: every checker call inside the body closes
\ the bracket, and a close clears every band. So the record is declared again
\ here, where it is written, rather than once at the colon.
: EM-COMPILE-FLUSH-PEND ( -- )
   DOES-REC:FLUSH
   11 DATA PEND-CELL LDR,
   1 11 0 ADDI,  2 DREC MOVZ,  PROT:LSPAN LABEL@ BL,
   9 11 0 LDR,  10 CP 9 SUB,  10 10 4 SUBI,  10 11 8 STR,
   PROT:LCLOSE LABEL@ BL,  LFLUSH LABEL@ BL, ;
s" em-compile-flush-pend" s" --" TRUST

: EM-COMPILE-PUBLISH-TRUSTED ( label -- ) {: publish:label :}
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
   DEF-TRUST:REGISTER
   publish B, ;
s" em-compile-publish-trusted" s" label --" TRUST

\ The hook's answer is a SIGN and not a set of codes. Zero has always meant
\ "refused, unpublish it"; every non-zero value has always meant "certified,
\ publish it". The third outcome splits that second half rather than adding a
\ magic number the engine and src/core/check-hook.f would both have to spell:
\ NEGATIVE certifies and publishes, POSITIVE certifies and HOLDS. The engine
\ therefore asks a structural question about the answer - which side of zero is
\ it on - and never has to know which hold code the checked world chose, so
\ there is no constant here that a second file has to keep in step.
\
\ WHAT HOLDING IS FOR. A definition the native chain is about to recompile from
\ the tape the checker just filled (src/compiler/native/migrate.f). Everything
\ the publication does is exactly what must NOT happen yet: the count must not
\ move, the record must not enter the name index, and the checker's width and
\ minimum-arity facts must not be poked into a record no caller can reach. The
\ chain's own publisher does all four, after its emission has been validated,
\ so that nothing is reachable under this name until code the chain accepted is
\ what stands behind it.
\
\ AND HOLDING IS NOT REJECTING, WHICH IS THE TRAP IN THIS BLOCK. Both skip the
\ publish label and both give the emission's code space back, so the two legs
\ look interchangeable. They are not. A name longer than DNAME-INL is not stored
\ in the record: C-STORE-NAME writes those bytes into CODE space and keeps the
\ PRE-NAME code pointer at record offset 24. Rejecting rewinds to that pre-name
\ pointer, which is right when the whole record is being thrown away and fatal
\ when it is being kept - the chain would emit its routine straight over the
\ bytes that spell the held word's own name. So the held leg rewinds to the
\ COLON ENTRY, record cell 0, for inline and ext names alike, with no DNAME-EXT
\ test. That erases every instruction the old emitter wrote and stops one byte
\ above the name.
\
\ WHICH ALSO LEAVES THE RECORD POINTING AT ITS OWN NEXT CODE. After the rewind
\ the code pointer IS the record's start cell, so the slot the chain is handed
\ (src/compiler/native/publish.f NEXT-SLOT) is the slot the record already
\ names, and the commit has no start cell to invent.
\
\ THE TEST SITS BEFORE THE PASS-2 DISPATCH DELIBERATELY. A held definition never
\ enters the width-aware re-run: its emission is being discarded, so re-emitting
\ it more carefully is work for nobody. Skipping the freeze strands nothing -
\ TXN-ACTIVE-CELL is set in exactly one place and the trigger follows it in the
\ same instruction stream, so it is never left set across definitions.
: EM-COMPILE-PUBLISH-HOOKED ( label label -- )
   {: publish:label finish:label :}
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
   nohook LBL,  publish B,
   rejected LBL,  11 DATA PEND-CELL LDR,  12 11 16 LDR,  12 12 DNAME-EXT ANDI,  12 inl CBZ,
      CP 11 24 LDR,  done B,                           \ ext name in code space: CP := pre-name CP
   inl LBL,  CP 11 0 LDR,                              \ inline name: CP := colon entry
   done LBL,
   finish B, ;
s" em-compile-publish-hooked" s" label label --" TRUST

\ Ask, at the one point both publish tails reach, whether this certified
\ definition is being withheld for the native chain (src/core/checker.f
\ CHECKER-HOLD?). A non-zero answer takes the held exit: no count, no name
\ index, no record facts, and the code pointer back to the colon entry.
\
\ IT IS ASKED HERE AND NOT IN EITHER TAIL. Only the sig-less tail has a hook
\ return value; a definition WITH a declared signature publishes through
\ EM-COMPILE-PUBLISH-TRUSTED, which calls the checker directly and never reads a
\ verdict. Anything carried on the hook's answer would therefore have reached
\ half the definitions in the system and silently missed the half that matter.
\ Both tails converge on `publish`, so one question here is one mechanism for
\ every definition, whichever route certified it.
\
\ ABSENT MEANS NOT HELD. The lookup is the non-fatal form, so the cold prefix -
\ every definition compiled before checker.f exists - takes exactly the path it
\ took before this question was asked.
\
\ AND THE HELD EXIT REWINDS TO THE COLON ENTRY, NEVER THE WAY THE REJECT LEG
\ DOES. A name longer than DNAME-INL is not stored in the record: C-STORE-NAME
\ writes those bytes into CODE space and keeps the PRE-NAME code pointer at
\ record offset 24. Rejecting rewinds to that pre-name pointer, which is right
\ when the record is being thrown away and fatal when it is being KEPT - the
\ chain would emit its routine straight over the bytes that spell the held
\ word's own name. Rewinding to record cell 0 instead erases every instruction
\ the old emitter wrote and stops one byte above the name, which also leaves the
\ record already pointing at the slot the chain is about to fill.
package HOLD-EMIT
public

: EM-COMPILE-HELD? ( label -- ) {: finish:label :}
   LBL {: nohold:label :}
   LHOLDQ 13 C-FIND-GLOBAL?
   13 nohold CBZ,
   C-CALL-X11-SAVED
   10 G-POP
   10 nohold CBZ,
      11 DATA PEND-CELL LDR,  CP 11 0 LDR,             \ CP := colon entry, above the name
      finish B,
   nohold LBL, ;
s" HOLD-EMIT:EM-COMPILE-HELD?" s" label --" TRUST

;package

: EM-COMPILE-PUBLISH ( -- )
   LBL LBL LBL {: hooked:label publish:label finish:label :}
   9 DATA HOOK-CELL LDR,  9 hooked CBZ,
   9 DATA TSIG-U-CELL LDR,  9 hooked CBZ,
   publish EM-COMPILE-PUBLISH-TRUSTED
   hooked LBL,
   publish finish EM-COMPILE-PUBLISH-HOOKED
   publish LBL,
   finish HOLD-EMIT:EM-COMPILE-HELD?
   NDICT NDICT 1 ADDI,  LHIDXADD LABEL@ BL,
   EM-REC-WIDE-PUBLISH
   DOES-REC:PUBLISH
   finish LBL,
   EM-P2-FINISH
   C-CLEAR-TRUSTED-STATE
   9 0 MOVZ,  9 DATA PEND-CELL STR,
   LMAIN LABEL@ B, ;
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

\ The loop keyword dispatch rows belong to the loop-emit family above: they
\ resolve its handlers bare, so the package is reopened here.
package LOOP-EMIT
public

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

;package

\ Compile-state dispatch assembly: the keyword-table aggregator and the
\ EM-COMPILE entry that EMIT-MAIN wires into the main loop (reopened below).
package COMPILE-EMIT

: EM-COMPILE-KEYWORDS ( -- )
   LBCAP LABEL@ BL,
   EM-COMPILE-CONTROL-KEYWORDS
   EM-COMPILE-STRING-KEYWORDS
   EM-COMPILE-META-KEYWORDS
   LOOP-EMIT:EM-COMPILE-LOOP-KEYWORDS ;
s" em-compile-keywords" s" --" TRUST

;package

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

\ The arithmetic rows hand their fold/emit/immediate emitters to the jit.f
\ VOPI-ENTRY/VOP-ENTRY registrars, which are private to the emitter package, so
\ this row block is inside it.
package ENGINE-EMIT

: EM-COMPILE-ARITH-OPS ( -- )
   s" +" KEEP? IF LMAIN LABEL@ LKWPLUS  1 ['] VF+ ['] E+ ['] EI+ 4095 VOPI-ENTRY THEN
   s" -" KEEP? IF LMAIN LABEL@ LKWMINUS 1 ['] VF- ['] E- ['] EI- 4095 VOPI-ENTRY THEN
   s" *" KEEP? IF LMAIN LABEL@ LKWSTAR  1 ['] VF* ['] E* VOP-ENTRY THEN
   s" and" KEEP? IF LMAIN LABEL@ LKWAND2  3 ['] FAND ['] EAND VOP-ENTRY THEN
   s" or" KEEP? IF LMAIN LABEL@ LKWOR2   2 ['] FOR2 ['] EOR2 VOP-ENTRY THEN
   s" xor" KEEP? IF LMAIN LABEL@ LKWXOR2  3 ['] FXOR2 ['] EXOR VOP-ENTRY THEN
   s" lshift" KEEP? IF LMAIN LABEL@ LKWLSH 6 ['] FLSH ['] ELSH ['] EILSH 63 VOPI-ENTRY THEN
   s" rshift" KEEP? IF LMAIN LABEL@ LKWRSH 6 ['] FRSH ['] ERSH ['] EIRSH 63 VOPI-ENTRY THEN ;
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

public

\ The one export of the row block: COMPILE-EMIT wires it into the main loop.
: EM-COMPILE-OPS ( -- )
   EM-COMPILE-ARITH-OPS
   EM-COMPILE-SHUFFLE-OPS
   EM-COMPILE-COMPARE-OPS
   EM-COMPILE-UNARY-OPS
   EM-COMPILE-FLOAT-OPS ;
s" em-compile-ops" s" --" TRUST

;package

: EM-COMPILE-CALL ( -- )
   LBL LBL LBL LBL LBL LBL LBL LBL {: notimm:label depthok:label callimm:label noxc:label ploop:label pdone:label usedtry:label found:label :}
   9 DATA P2-CELL LDR,  9 noxc CBZ,               \ layout-cap slice 4: pass-2 wide generated-ctor call adds extra pads
      9 DATA TKA-CELL LDR,  10 DATA TXN-SRC-A-CELL LDR,  9 9 10 SUB,  10 0 MOVZ,
      LP2CWAT LABEL@ BL,                          \ x10 = extra pads, x11 = found
      11 noxc CBZ,                                \ ordinary call (no fact): normal lowering
      SP SP 16 SUBI,  10 SP 0 STR,                \ frame the count across LVPUSHC spills
      1 CP 4 ADDI,  PROT:LOPEN LABEL@ BL,                 \ region -> RW for emission
      ploop LBL,
         10 SP 0 LDR,  10 pdone CBZ,
         11 0 MOVZ,  LVPUSHC LABEL@ BL,           \ push one extra zero pad below the declared body's pads
         10 SP 0 LDR,  10 10 1 SUBI,  10 SP 0 STR,  ploop B,
      pdone LBL,
      SP SP 16 ADDI,
   noxc LBL,
   LVSPILL LABEL@ BL,
   9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFIND LABEL@ BL,
   13 usedtry CBZ,                                        \ open-scope + global miss -> try used publics
   found LBL,
   14 13 2 ANDI,  14 notimm CBZ,
      14 13 $FF00 ANDI,  14 depthok CBZ,                  \ DNAME-MIN-IN (x13 bits 8-15): the immediate's certified min input arity; 0 = unguarded boundary (compile path floor beneath the p5 checker/hook reject, reached under 0 set-check)
         14 14 8 LSRI,                                    \ x14 = min-in cells
         9 DATA S0-CELL LDR,  10 XDS 9 SUB,  10 10 3 ASRI, \ x10 = interpret depth in cells
         10 14 CMP,  C-LT LMININ LABEL@ BCOND,            \ compile-time depth < declared inputs -> named reject BEFORE the immediate BLRs below the interpret base (dict region still RW here; LDIAGRET restores RX)
      depthok LBL,
      SP SP 32 SUBI,  30 SP 0 STR,  11 SP 8 STR,
      PROT:LCLOSE LABEL@ BL,
      9 DATA HOOK-CELL LDR,  9 callimm CBZ,
      9 DATA COMPILE-PREFLIGHT-CELL LDR,  9 LPREFMISS LABEL@ CBZ,  9 SP 16 STR,
      9 DATA BODYBUF-OFF ADDI,  9 G-PUSH
      9 DATA BODYLEN-CELL LDR,  9 G-PUSH
      9 DATA TKA-CELL LDR,  9 G-PUSH
      9 DATA TKL-CELL LDR,  9 G-PUSH
      9 DATA TRUSTED-CELL LDR,  9 G-PUSH
      9 SP 16 LDR,  9 BLR,
      callimm LBL,
      11 SP 8 LDR,  11 BLR,
      1 CP 4 ADDI,  PROT:LOPEN LABEL@ BL,
      30 SP 0 LDR,  SP SP 32 ADDI,
      LMAIN LABEL@ B,
   notimm LBL,
   C-CALL  LMAIN LABEL@ B,
   usedtry LBL,
      9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFINDUSED LABEL@ BL,   \ used-publics resolution (ambiguity dies; region-flip handled inside)
      13 LUNDEF LABEL@ CBZ,                            \ still nothing -> undefined in this body
      found B, ;                                       \ resolved via a used package: rejoin the normal compile path
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

\ A throw whose nearest handler lies beyond one or more active evaluate boundaries
\ lands here (BTHROW branch via EVALREC-CELL), x15 = throw code. Each escaped eval
\ frame is rolled back — input cursor, dictionary top (CP/NDICT), data-stack base
\ (XDS), data pointer (DP), and compile state — and EVALERR-CELL records the code,
\ so the handler resumes with clean state. Popping stops as soon as EVALD reaches 0
\ or the nearest handler (x11, read once because EM-RESET-COMPILE-STATE zeroes the
\ HND-CELL copy) is inside the current eval frame; then the throw is delivered to
\ that handler / REPL / process exit exactly as the non-evaluate path does.
\ Recovery, undefined/exit handling, ADT construction/matching, and main-loop rows
\ emit raw machine state transitions and diagnostics.
\ Retirement: habu-builder-trust-rows-c5d41af6.
: EM-EVAL-THROW-RECOVER ( -- )
   LEVALREC LABEL@ LBL,
   LBL LEVLL !  LBL LEVLP !  LBL LEVLD !  LBL LEVLN !  LBL LEVLR !
   LBL LEVCORRUPT !  LBL LEVCORRUPTMSG !
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
      \ roll the open-package scope back to this frame's boundary (PKGSNAP[EVALD-1]),
      \ parallel to CP/NDICT/DP above: an in-package define aborted inside evaluate must
      \ not leave the package dangling. x11(handler)/x13(&frame)/x15(code) preserved; x9/x10 scratch.
      9 DATA EVALD-CELL LDR,  9 9 1 SUBI,                    \ x9 = EVALD-1 (pre-decrement index)
      10 PKGSNAP-OFF LIT64,  9 9 PKGSNAP-SHIFT LSLI,  10 10 9 ADD,  10 DATA 10 ADD,   \ x10 = &PKGSNAP[EVALD-1]
      9 10 PKGSNAP-CUR LDR,     9 DATA CUR-CELL STR,
      9 10 PKGSNAP-PUB LDR,     9 DATA PKG-PUB-CELL STR,
      9 10 PKGSNAP-PRI LDR,     9 DATA PKG-PRI-CELL STR,
      9 10 PKGSNAP-PARENT LDR,  9 DATA PKG-PARENT-CELL STR,
      9 10 PKGSNAP-REC LDR,     9 DATA PKG-REC-CELL STR,
      9 10 PKGSNAP-USE LDR,     12 USE-DEPTH-CELL LIT64,  12 DATA 12 ADD,  9 12 0 STR,   \ roll the using-scope depth back too (x12 reloaded below)
      9 1 MOVZ,  9 DATA PKGRESYNC-CELL STR,                  \ arm the checker resync (drained at next LMAIN)
      15 DATA EVALERR-CELL STR,                       \ EVALERR = code
      12 DATA EVALD-CELL LDR,  12 12 1 SUBI,  12 DATA EVALD-CELL STR,
      EM-RESET-COMPILE-STATE                          \ clobbers x9 only; x11,x15 preserved
      LEVLL LABEL@ B,
   LEVLD LABEL@ LBL,
   9 15 0 ADDI,                                       \ x9 = code
   11 LEVLN LABEL@ CBZ,
   \ handler-frame integrity (same contract as habu1.f BTHROW): validate before any restore store
   14 CATCH-FRAME-MAGIC LIT64,  10 11 56 LDR,  10 14 CMP,  C-NE LEVCORRUPT LABEL@ BCOND,   \ forged/adjacent-mutated frame
   10 11 40 LDR,  10 0 CMPI,  C-LT LEVCORRUPT LABEL@ BCOND,            \ saved RSP underflow
   14 RSTK-CELLS MOVZ,  10 14 CMP,  C-GT LEVCORRUPT LABEL@ BCOND,      \ saved RSP past region
   10 11 48 LDR,  10 0 CMPI,  C-LT LEVCORRUPT LABEL@ BCOND,            \ saved LOOPSP underflow
   14 LOOP-STK-FRAMES MOVZ,  10 14 CMP,  C-GT LEVCORRUPT LABEL@ BCOND, \ saved LOOPSP past region
   19 11 8 LDR,
   10 11 40 LDR,  10 DATA RSP-CELL STR,               \ restore user return-stack depth
   10 11 48 LDR,  10 DATA LOOPSP-CELL STR,            \ restore loop-stack depth
   10 11 0 LDR,  10 DATA 8 STR,                       \ HND = prev
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
   0 UNCAUGHT-RC MOVZ,  NR-EXIT-GROUP SYS,
   LEVCORRUPT LABEL@ LBL,                              \ eval-cross forged/corrupt handler frame: fail closed before any restore
   0 2 MOVZ,  1 LEVCORRUPTMSG LABEL@ ADR,  2 23 MOVZ,  NR-WRITE SYS,
   0 ENGINE-ERROR:CATCH-STACK MOVZ,  NR-EXIT-GROUP SYS,
   LEVCORRUPTMSG LABEL@ LBL,  s" hb: catch frame corrupt" BYTES, ;
s" em-eval-throw-recover" s" --" TRUST

: EM-REPL-RECOVER ( -- )
   LRREC LABEL@ LBL,
   0 2 MOVZ,  1 LQNL LABEL@ ADR,  2 2 MOVZ,  NR-WRITE SYS,
   CP DATA RSAVCP-CELL LDR,
   NDICT DATA RSAVND-CELL LDR,
   9 DATA RSAVDP-CELL LDR,  9 DATA DP-CELL STR,
   9 DATA S0-CELL LDR,  XDS 9 0 ADDI,
   EM-RESET-COMPILE-STATE
   \ roll the open-package scope back to this REPL line's boundary (RPKG snapshot),
   \ alongside the RSAVCP/RSAVND/RSAVDP/RSAVSP rollback above, and arm the checker resync.
   9 DATA RPKG-CUR LDR,      9 DATA CUR-CELL STR,
   9 DATA RPKG-PUB LDR,      9 DATA PKG-PUB-CELL STR,
   9 DATA RPKG-PRI LDR,      9 DATA PKG-PRI-CELL STR,
   9 DATA RPKG-PARENT LDR,   9 DATA PKG-PARENT-CELL STR,
   9 DATA RPKG-REC LDR,      9 DATA PKG-REC-CELL STR,
   10 USE-RPKG-SAVE-CELL LIT64,  10 DATA 10 ADD,  9 10 0 LDR,     \ roll the using-scope depth back to this REPL line's snapshot
   10 USE-DEPTH-CELL LIT64,  10 DATA 10 ADD,  9 10 0 STR,
   9 1 MOVZ,  9 DATA PKGRESYNC-CELL STR,
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
      PROT:LCLOSE LABEL@ BL,                           \ region -> RX
      15 RC-REJECT MOVZ,                                    \ x15 = throw code
      10 DATA EVALREC-CELL LDR,  10 BR,                     \ -> LEVALREC (frame unwind + deliver)
   LUN0 LABEL@ LBL,
   9 DATA REPLH-CELL LDR,  9 LRDIE LABEL@ CBZ,
   \ Restore RX before re-entering the REPL read/handler code (dot habu-recovery-pkg-scope-e0bd98e2):
   \ an undefined word (LUNDEF), orphan closer (LORPHAN), or cf-cap (LCFCAP) that aborts a `:`-body
   \ at the tty REPL leaves the JIT code region RW; EM-REPL-RECOVER -> LREAD -> RD-LINE then executes
   \ compiled code in a writable region -> W^X SIGBUS. This is the same RX restore the EVALD>0 leg
   \ above and LCOMPILEDIE's ldie leg already do; idempotent for the interpret-level diagnostics
   \ (LWIDE/LINTERNAL/LMININ) that share this LUN0 tail with the region already RX. Pre-existing
   \ crash surfaced by the REPL package-scope fixture (in-package `: FOO NOPEWORD ;` at the tty).
   PROT:LCLOSE LABEL@ BL,
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
   LDIAGRET LABEL@ B,
   LMININ LABEL@ LBL,                                  \ branch target: TKA/TKL still hold the token (DNAME-MIN-IN underdepth reject)
   0 2 MOVZ,  1 LMINMSG LABEL@ ADR,  2 MINMSG-LEN MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 2 MOVZ,  1 LQNL LABEL@ ADR,  1 1 1 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
   LDIAGRET LABEL@ B,
   LPREFMISS LABEL@ LBL,
   0 2 MOVZ,  1 LPREFMISSMSG LABEL@ ADR,  2 PREFMISSMSG-LEN MOVZ,  NR-WRITE SYS,
   LDIAGRET LABEL@ B,
   LORPHAN LABEL@ LBL,                                 \ branch target from LCFPOP: closer with an empty control-flow stack (TKA/TKL hold the closer token)
   0 2 MOVZ,  1 LORPHANMSG LABEL@ ADR,  2 ORPHANMSG-LEN MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 2 MOVZ,  1 LQNL LABEL@ ADR,  1 1 1 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
   LDIAGRET LABEL@ B,
   LCFCAP LABEL@ LBL,                                 \ branch target from LCFPUSH: opener at the control-flow stack depth cap (TKA/TKL hold the opener token)
   0 2 MOVZ,  1 LCFCAPMSG LABEL@ ADR,  2 CFCAPMSG-LEN MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 2 MOVZ,  1 LQNL LABEL@ ADR,  1 1 1 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
   LDIAGRET LABEL@ B, ;
s" em-compile-undef" s" --" TRUST

\ Shared recoverable compile-error tail (dot habu-raw-exit-compile). The
\ parametric sibling of LDIAGRET: LDIAGRET delivers a fixed RC-REJECT (70), this
\ carries the code in x0 so duplicate-definition / dict+code overflow / package
\ misuse / locals-in-quotation / does>-check rejects each keep their own
\ sysexits-style exit code (positive, shared across sites by design per
\ tools/error-code-lint-core.f). Each die site writes its diagnostic to fd 2, sets
\ x0 = its code, then `LCOMPILEDIE LABEL@ B,` here. Inside evaluate (EVALD>0) the
\ aborted nested compile unwinds as a catchable throw of that code via the eval
\ throw-recovery (the same LEVALREC path BTHROW/LDIAGRET use) — rolling back every
\ escaped eval frame (input cursor, CP/NDICT truncation drops the partial record,
\ XDS/DP, compile state; HIDX transparently skips any rolled-back record index) —
\ and delivers to the enclosing catch, else fails closed with exit_group(x0). At
\ top level (EVALD==0) it exit_group(x0), byte-identical to the old raw exit. We may
\ abort mid-compile with the code region RW, so restore RX (idempotent when already
\ RX) before re-entering executable EV/handler code. x15 carries the code across the
\ LPROT syscall (kernel preserves x2-x15, as the LUNCAUGHT reporter already relies on).
: EM-COMPILE-DIE ( -- )
   LBL LBL {: ldie:label rdie:label :}
   LCOMPILEDIE LABEL@ LBL,                                  \ entry: x0 = sysexits exit/throw code (die site just wrote its diagnostic)
   15 0 0 ADDI,                                             \ x15 = code (survives LPROT; LEVALREC delivers it as the throw code)
   9 DATA EVALD-CELL LDR,  9 ldie CBZ,                      \ EVALD==0 -> no eval frame (top level / tty REPL); EVALD>0 -> unwind escaped eval frames
      PROT:LCLOSE LABEL@ BL,                           \ region -> RX
      10 DATA EVALREC-CELL LDR,  10 BR,                     \ -> LEVALREC (escaped-frame unwind + deliver x15)
   ldie LBL,
   9 DATA REPLH-CELL LDR,  9 rdie CBZ,                      \ tty-REPL parity (dot habu-convert-residual-compile-f460b9f2): REPLH!=0 -> recover the interactive session exactly like LUNDEF/LDIAGRET's LUN0 leg instead of exiting; REPLH==0 (script/--load/stdin) -> fail-closed exit byte-identically
      PROT:LCLOSE LABEL@ BL,                           \ region -> RX (idempotent) before re-entering REPL read/handler code: a compile die mid-:-body leaves the region RW
      LRREC LABEL@ B,                                       \ -> EM-REPL-RECOVER: roll back to the REPL line-start snapshot (same CP/NDICT/DP/XDS/compile-state surface + HIDX stale-skip as the eval-frame recovery) and re-read
   rdie LBL,
   NR-EXIT-GROUP SYS,                                       \ x0 still = code (REPLH==0): fail-closed exit unchanged
   \ Counted-string-too-long diagnostic (dot habu-recovery-pkg-scope-e0bd98e2). The
   \ four c"/c\" cap sites (C-ICQ/C-EICQ/C-CQ/C-ECQ) used a bare `0 76 MOVZ,
   \ LCOMPILEDIE B,` that was byte-identically SILENT and overloaded with C-SIG-BAD's
   \ 76. This branch target writes a named fd-2 label first, then routes through the
   \ SAME LCOMPILEDIE tail with x0=76 (catchable throw inside evaluate, fail-closed
   \ exit 76 at top level). No new exit code, no new TRUST site (emitted inside this
   \ pre-existing untrusted tail word).
   LCSTR LABEL@ LBL,
   0 2 MOVZ,  1 LCSTRMSG LABEL@ ADR,  2 CSTRMSG-LEN MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 LQNL LABEL@ ADR,  1 1 1 ADDI,  2 1 MOVZ,  NR-WRITE SYS,   \ LQNL[1] = newline
   0 76 MOVZ,  LCOMPILEDIE LABEL@ B,
   \ DP-heap out-of-range diagnostic (dot habu-dictionary-allot-past-4e5c3c2b). Every
   \ allot/,/c,/definer DP advance runs habu1.f DP-CHECK, which used a bare silent
   \ `0 76 MOVZ, NR-EXIT-GROUP SYS,` when the new DP left [DATA-START, DATA-SIZE] — a
   \ capacity kill with NO diagnostic (the split-K lane's "silent corruption": ~28 MB of
   \ host buffers exit_group'd with no message). This target writes a named fd-2 label
   \ then routes through the SAME LCOMPILEDIE tail with x0=76: a catchable throw inside
   \ evaluate (DP rolled back, no partial write — DP-CHECK precedes the DP store), a
   \ fail-closed exit 76 at top level. No new exit code (76 is the pre-existing raw code).
   LDPBAD LABEL@ LBL,
   0 2 MOVZ,  1 LDPBADMSG LABEL@ ADR,  2 DPBADMSG-LEN MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 LQNL LABEL@ ADR,  1 1 1 ADDI,  2 1 MOVZ,  NR-WRITE SYS,   \ LQNL[1] = newline
   0 76 MOVZ,  LCOMPILEDIE LABEL@ B, ;

: EM-EVAL-CLEAN-EXIT ( -- )
   9 DATA EVALD-CELL LDR,  9 9 1 SUBI,  9 DATA EVALD-CELL STR,
   9 14 15 C-EVAL-FRAME-ADDR
   9 14 0 LDR,  9 DATA INP-CELL STR,
   9 14 8 LDR,  9 DATA INE-CELL STR,
   9 0 MOVZ,  9 DATA EVALERR-CELL STR,
   \ end of load file: restore the using-scope depth to this frame's entry snapshot
   \ (PKGSNAP[EVALD].PKGSNAP-USE) so usings opened in the evaluated source do not leak
   \ to the caller. Package scope deliberately persists across a clean include; usings
   \ are file-local. x15 = &PKGSNAP[idx]; x9 = value.
   9 DATA EVALD-CELL LDR,
   15 PKGSNAP-OFF LIT64,  9 9 PKGSNAP-SHIFT LSLI,  15 15 9 ADD,  15 DATA 15 ADD,
   9 15 PKGSNAP-USE LDR,
   15 USE-DEPTH-CELL LIT64,  15 DATA 15 ADD,  9 15 0 STR,
   9 14 24 LDR,  SP 9 0 ADDI,
   9 14 16 LDR,  9 BR, ;
s" em-eval-clean-exit" s" --" TRUST

: EM-REPL-READ ( -- )
   LREAD LABEL@ LBL,
   9 SP 0 ADDI,  9 DATA RSAVSP-CELL STR,
   CP DATA RSAVCP-CELL STR,
   NDICT DATA RSAVND-CELL STR,
   9 DATA DP-CELL LDR,  9 DATA RSAVDP-CELL STR,
   \ snapshot the open-package scope for this REPL line (EM-REPL-RECOVER restores it)
   9 DATA CUR-CELL LDR,       9 DATA RPKG-CUR STR,
   9 DATA PKG-PUB-CELL LDR,   9 DATA RPKG-PUB STR,
   9 DATA PKG-PRI-CELL LDR,   9 DATA RPKG-PRI STR,
   9 DATA PKG-PARENT-CELL LDR, 9 DATA RPKG-PARENT STR,
   9 DATA PKG-REC-CELL LDR,   9 DATA RPKG-REC STR,
   10 USE-DEPTH-CELL LIT64,  10 DATA 10 ADD,  9 10 0 LDR,        \ snapshot the using-scope depth for this REPL line
   10 USE-RPKG-SAVE-CELL LIT64,  10 DATA 10 ADD,  9 10 0 STR,
   9 DATA REPLH-CELL LDR,  9 BLR,
   XDS XDS 8 SUBI,  10 XDS 0 LDR,
   XDS XDS 8 SUBI,  11 XDS 0 LDR,
   10 LRBYE LABEL@ CBZ,
   11 DATA INP-CELL STR,  11 11 10 ADD,  11 DATA INE-CELL STR,  LMAIN LABEL@ B, ;
s" em-repl-read" s" --" TRUST

\ Top-level source exhausted. A boot arrives here TWICE: first at the end of the
\ ENGINE PREFIX stream, then at the end of the USER stream (a piped program, the
\ `required` rows `--load` wrote, or the baked LSRC) - see BOOT-SRC:USER-END in
\ layout.f for why the two are separate streams in one buffer.
\
\ The first arrival is the only moment in a boot when the engine is complete and
\ no user token has run, so it is where the AOT seed belongs: the captured code
\ resolves its call sites by name against the cold-prefix dictionary, which now
\ exists, and every mode - pty REPL, piped stdin, `--load`, `--build`, a baked
\ driver - reaches it before its program. The done cell is the seed's only guard;
\ the second arrival and any REPL re-entry find it set. (Dot
\ habu-decide-arm-the-5234727b, USER RULING 2026-08-11: one dictionary surface for
\ every boot mode. The rejected shapes are on that leaf: a new `AOT-SEED` prefix
\ token would have put an engine-internal name in every program's dictionary, and
\ hanging the seed on the existing SEAL-FRIEND token would have made a security
\ primitive moonlight as dictionary construction.)
\
\ Installing the user stream is EM-REPL-READ's move: the new stream starts where
\ the old one ended (INE), and re-entering LMAIN is a branch. The cell is cleared
\ as it is consumed, so nothing can install it twice.
: EM-COMPILE-EXIT ( -- )
   LBL LBL {: aoskip:label nousrc:label :}
   LEXIT LABEL@ LBL,
   9 DATA EVALD-CELL LDR,  9 LEX0 LABEL@ CBZ,
      EM-EVAL-CLEAN-EXIT
   LEX0 LABEL@ LBL,                                          \ top-level source exhausted (EVALD==0), cp@ clean here
   9 DATA AOT-SEED-DONE-CELL LDR,  9 aoskip CBNZ,            \ already seeded -> skip
      EM-SEED-AOT                                            \ seed the baked words once, post-cold-prefix
      9 1 MOVZ,  9 DATA AOT-SEED-DONE-CELL STR,
   aoskip LBL,
   9 DATA BOOT-SRC:USER-END LDR,  9 nousrc CBZ,                  \ no second stream -> repl or exit
      10 0 MOVZ,  10 DATA BOOT-SRC:USER-END STR,                 \ one-shot: consume it before installing
      10 DATA INE-CELL LDR,  10 DATA INP-CELL STR,           \ the user stream begins where the prefix ended
      9 DATA INE-CELL STR,
      LMAIN LABEL@ B,
   nousrc LBL,
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
\ path: inside EVALUATE the failure unwinds as a catchable RC-REJECT throw via the
\ eval throw-recovery (LEVALREC, restoring RX first), else recover + re-read in the
\ REPL, else fail closed with exit 70 in a batch/--load load. Never a signal. The
\ old rollback-and-return leg (EVALERR only) was fail-open: catch read 0 and a
\ handlerless caller kept interpreting past the failed evaluate.
: EM-INTERPRET-UNDERFLOW ( -- )
   LBL LBL {: uf0:label ufdie:label :}
   LUNDERFLOW LABEL@ LBL,
   SP SP 16 SUBI,  9 $465245444E552D45 LIT64,  9 SP 0 STR,  9 $000000203A574F4C LIT64,  9 SP 8 STR,  0 2 MOVZ,  1 SP 0 ADDI,  2 13 MOVZ,  NR-WRITE SYS,  SP SP 16 ADDI,  0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,  0 2 MOVZ,  1 LQNL LABEL@ ADR,  1 1 1 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
   9 DATA EVALD-CELL LDR,  9 uf0 CBZ,
      \ Inside evaluate: mirror LDIAGRET — an immediate word can underflow
      \ mid-compile with the dict region RW, so restore RX before the unwind
      \ re-enters executable (handler) code; mprotect is idempotent when the
      \ guard fired at interpret level with the region already RX.
      PROT:LCLOSE LABEL@ BL,                           \ region -> RX
      15 RC-REJECT MOVZ,                                \ x15 = throw code
      10 DATA EVALREC-CELL LDR,  10 BR,                 \ -> LEVALREC (frame unwind + deliver)
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
\ Shared ADT-dispatch stencils (dot habu-factor-repeated-match). The CMM=1/2/3/4/5
\ legs repeat three byte-identical engine sequences; factored into BL/B-shared
\ engine routines (bodies + labels both placed inline in EM-COMPILE-ADT-MODE).
\ Emitted USER code is untouched — none of these touch
\ C-EMITW/LCEMIT — so only the compiler's own __text shrinks.
\   LADTPUSHTOK: push captured token addr+len as checker-bridge args (x9/XDS;
\                preserves x11=XT). Leaf, RET.
\   LMFRTOP:     x15 = &top match-frame family-id slot (x14 scratch). Leaf, RET.
\   LADTDIE:     operand-resolution die tail entered with x1=msg x2=len live:
\                write(2) then C-DIE-TOKEN-NL(70). Never returns (B, not BL).
variable LADTPUSHTOK  variable LMFRTOP  variable LADTDIE

: EM-ADT-CON-FAM ( -- )                 \ CMM=1 leg: resolve family, arm state 2
   LBL LBL {: fmsg:label fok:label :}
   LBCAP LABEL@ BL,                     \ operand reaches the checker's body too
   PROT:LCLOSE LABEL@ BL,          \ region -> RX: checker-call window
   LTFLCONFAM 12 C-FIND-GLOBAL
   LADTPUSHTOK LABEL@ BL,
   C-CALL-X11-SAVED
   10 G-POP                             \ ok flag (top)
   9 G-POP                              \ family id
   10 fok CBNZ,
      1 fmsg ADR,  2 CONFMSG-LEN MOVZ,  LADTDIE LABEL@ B,
   fmsg LBL,  s" hb: construct: unknown family: " BYTES,
   fok LBL,
   9 DATA CMFAM-CELL STR,
   12 2 MOVZ,  12 DATA CMM-CELL STR,
   1 CP 4 ADDI,  PROT:LOPEN LABEL@ BL,          \ region -> RW: resume emission
   LMAIN LABEL@ B, ;
s" em-adt-con-fam" s" --" TRUST

: EM-ADT-CON-PUSHES ( -- )              \ pads x 0 + tag as VS constants (x12=pads, x13=tag)
   LBL LBL {: ploop:label pdone:label :}
   SP SP 16 SUBI,  12 SP 0 STR,  13 SP 8 STR,   \ frame the counters: LVPUSHC may
   1 CP 4 ADDI,  PROT:LOPEN LABEL@ BL,                  \ spill (emission -> region RW first)
   ploop LBL,
      12 SP 0 LDR,  12 pdone CBZ,
      11 0 MOVZ,  LVPUSHC LABEL@ BL,
      12 SP 0 LDR,  12 12 1 SUBI,  12 SP 0 STR,  ploop B,
   pdone LBL,
   11 SP 8 LDR,  LVPUSHC LABEL@ BL,
   SP SP 16 ADDI, ;
s" em-adt-con-pushes" s" --" TRUST

: EM-ADT-CON-VAR ( -- )                 \ CMM=2 leg: resolve variant, emit, mode off
   LBL LBL LBL {: vmsg:label vok:label nox:label :}
   LBCAP LABEL@ BL,                     \ operand reaches the checker's body too
   PROT:LCLOSE LABEL@ BL,          \ region -> RX: checker-call window
   LTFLCVAR 9 C-FIND-GLOBAL
   LADTPUSHTOK LABEL@ BL,
   9 DATA CMFAM-CELL LDR,  9 G-PUSH
   C-CALL-X11-SAVED
   10 G-POP                             \ ok flag (top)
   12 G-POP                             \ pads
   13 G-POP                             \ tag
   10 vok CBNZ,
      1 vmsg ADR,  2 CONVMSG-LEN MOVZ,  LADTDIE LABEL@ B,
   vmsg LBL,  s" hb: construct: unknown variant: " BYTES,
   vok LBL,
   9 DATA P2-CELL LDR,  9 nox CBZ,      \ layout-cap slice 4: pass-2 wide construct adds extra pads
      SP SP 16 SUBI,  12 SP 0 STR,  13 SP 8 STR,      \ save declared pads + tag across the query
      9 DATA TKA-CELL LDR,  10 DATA TXN-SRC-A-CELL LDR,  9 9 10 SUB,  10 0 MOVZ,
      LP2CWAT LABEL@ BL,                              \ x10 = extra pads, x11 = found
      12 SP 0 LDR,  13 SP 8 LDR,  SP SP 16 ADDI,
      11 nox CBZ,
         12 12 10 ADD,                                \ x12 = declared + extra pads
   nox LBL,
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
\ J-MATCH's CF-ENTRY; J-MATCH peeks the tag at [x19,#-8] once into x9 and every
\ variant compares that live x9 (reduction 2).

\ C-DIE-BAD-TAG ( x11=family-name-addr  x12=family-name-len -- )
\   Emit the invalid-tag die INLINE into the user word with no normal
\   continuation: a jump over the message, the message "hb: bad <family> tag\n"
\   copied inline (the NAME BYTES travel with the word — never a live pointer into
\   the mmap-relocatable TF-STR pool), then a self-contained write(2,msg,len) +
\   exit_group(ENGINE-ERROR:BAD-TAG). Modeled on C-DIE-TOKEN-NL's write/exit tail and C-SDQ's
\   inline-byte copy, but every instruction is C-EMITW'd into the user code region
\   (x28=CP), not the engine. Runs with the region RW. Clobbers x5-x16.
: C-DIE-BAD-TAG ( -- )
   LBL LBL LBL LBL LBL LBL {: p1:label p2:label s1:label s2:label t1:label t2:label :}
   SP SP $20 SUBI,  11 SP 0 STR,  12 SP 8 STR,        \ frame name addr/len (loops clobber x11)
   15 CP 0 ADDI,  15 SP 16 STR,                        \ B-over addr (patch target)
   9 $14000000 LIT64,  LCEMIT LABEL@ BL,               \ emit B placeholder
   16 CP 0 ADDI,  16 SP 24 STR,                        \ msg start addr
   1 12 13 ADDI,  PROT:RESERVE                      \ the whole message, copied at CP
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
   9 $D2800000 ENGINE-ERROR:BAD-TAG 32 * + LIT64,  LCEMIT LABEL@ BL,   \ movz x0, #ENGINE-ERROR:BAD-TAG
   9 SYS-EMIT-EXIT LIT64,  LCEMIT LABEL@ BL,           \ movz x_sys, #NR-EXIT-GROUP
   SYS-EMIT-SVC C-EMITW                                \ svc
   SP SP $20 ADDI, ;
s" c-die-bad-tag" s" --" TRUST

: EM-MATCH-SEMI ( -- )                  \ ;match: invalid-tag die + join + pop frame
   LBL LBL {: jl:label jd:label :}
   PROT:LCLOSE LABEL@ BL,          \ region -> RX: checker-friend call window
   LTFLNAME 10 C-FIND-GLOBAL
   LMFRTOP LABEL@ BL,  9 15 0 LDR,
   9 G-PUSH                             \ top match-frame family id
   C-CALL-X11-SAVED
   12 G-POP                             \ family name length
   11 G-POP                             \ family name address
   SP SP 16 SUBI,  11 SP 0 STR,  12 SP 8 STR,    \ frame the name span across the RW flip
   1 CP 4 ADDI,  PROT:LOPEN LABEL@ BL,          \ region -> RW: emission
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
   PROT:LCLOSE LABEL@ BL,
   LTFLMATCHFAM 14 C-FIND-GLOBAL
   LADTPUSHTOK LABEL@ BL,
   C-CALL-X11-SAVED
   10 G-POP                             \ ok flag
   9 G-POP                              \ family id
   10 fok CBNZ,
      1 fmsg ADR,  2 MFAMMSG-LEN MOVZ,  LADTDIE LABEL@ B,
   fmsg LBL,  s" hb: match: unknown family: " BYTES,
   fok LBL,
   LMFRTOP LABEL@ BL,  9 15 0 STR,
   12 4 MOVZ,  12 DATA CMM-CELL STR,
   1 CP 4 ADDI,  PROT:LOPEN LABEL@ BL,
   LMAIN LABEL@ B, ;
s" em-adt-match-fam" s" --" TRUST

: EM-ADT-MATCH-VAR ( -- )               \ CMM=4: ;match -> semi, else resolve variant
   LBL LBL LBL {: notsemi:label vmsg:label vok:label :}
   LBCAP LABEL@ BL,
   0 LKWSEMIMATCH LABEL@ ADR,  1 6 MOVZ,  LKWCMP LABEL@ BL,
   0 notsemi CBZ,
      EM-MATCH-SEMI
   notsemi LBL,
   PROT:LCLOSE LABEL@ BL,
   LTFLCVAR 9 C-FIND-GLOBAL
   LADTPUSHTOK LABEL@ BL,
   LMFRTOP LABEL@ BL,  9 15 0 LDR,  9 G-PUSH
   C-CALL-X11-SAVED
   10 G-POP                             \ ok flag
   12 G-POP                             \ pads (M-p)
   13 G-POP                             \ tag
   10 vok CBNZ,
      1 vmsg ADR,  2 MVARMSG-LEN MOVZ,  LADTDIE LABEL@ B,
   vmsg LBL,  s" hb: match: unknown variant: " BYTES,
   vok LBL,
   13 DATA CMTAG-CELL STR,
   12 DATA CMPADS-CELL STR,
   12 5 MOVZ,  12 DATA CMM-CELL STR,
   1 CP 4 ADDI,  PROT:LOPEN LABEL@ BL,
   LMAIN LABEL@ B, ;
s" em-adt-match-var" s" --" TRUST

: EM-ADT-MATCH-OF ( -- )                \ CMM=5: require `of`, emit compare + prologue
   LBL LBL LBL LBL LBL {: emsg:label eok:label noxm:label bigtag:label tagdone:label :}
   LBCAP LABEL@ BL,
   0 LKWOF LABEL@ ADR,  1 2 MOVZ,  LKWCMP LABEL@ BL,
   0 eok CBNZ,
      1 emsg ADR,  2 MOFMSG-LEN MOVZ,  LADTDIE LABEL@ B,
   emsg LBL,  s" hb: match: expected of: " BYTES,
   eok LBL,
   9 DATA P2-CELL LDR,  9 noxm CBZ,     \ layout-cap slice 4: pass-2 wide match arm adds extra pads to CMPADS
      9 DATA TKA-CELL LDR,  10 DATA TXN-SRC-A-CELL LDR,  9 9 10 SUB,  10 0 MOVZ,
      LP2CWAT LABEL@ BL,                \ x10 = extra pads, x11 = found
      11 noxm CBZ,
         14 DATA CMPADS-CELL LDR,  14 14 10 ADD,  14 DATA CMPADS-CELL STR,
   noxm LBL,
   \ Dispatch compare+skip (reductions 1/2/3). The tag is already live in x9
   \ (loaded once by J-MATCH). Compare it with an immediate cmp #tag when the tag
   \ fits AArch64's 12-bit unsigned cmp field ($F100013F | tag<<10), else fall
   \ back to movz x16,#tag + cmp x9,x16. Then a b.ne placeholder ($54000001,
   \ imm19) skips to the next variant, patched by J-ENDOF->LPAT — replacing the
   \ former movz+ldur+cmp+cset+cbz (7 arm words down to 2 + one shared ldur).
   14 DATA CMTAG-CELL LDR,
   14 $FFF CMPI,  C-HI bigtag BCOND,    \ tag > 2^12-1: outside cmp's imm12 field
      9 $F100013F LIT64,  14 14 10 LSLI,  9 9 14 ORR,  LCEMIT LABEL@ BL,  tagdone B,   \ cmp x9,#tag
   bigtag LBL,
      9 C-CALL-MOVZ-X16 LIT64,  14 14 5 LSLI,  9 9 14 ORR,  LCEMIT LABEL@ BL,   \ movz x16,#tag
      $EB10013F C-EMITW                 \ cmp x9,x16
   tagdone LBL,
   C-PUSHCP
   $54000001 C-EMITW                    \ b.ne +0  (skip to next variant on tag mismatch)
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
   LBL LADTPUSHTOK !  LBL LMFRTOP !  LBL LADTDIE !   \ allocate shared-routine labels
   9 DATA CMM-CELL LDR,  9 off CBZ,
   9 2 CMPI,  C-EQ s2 BCOND,
   9 3 CMPI,  C-EQ s3 BCOND,
   9 4 CMPI,  C-EQ s4 BCOND,
   9 5 CMPI,  C-EQ s5 BCOND,
   EM-ADT-CON-FAM
   \ Shared routine bodies, emitted after CON-FAM's unconditional LMAIN B, and
   \ before the s2 dispatch label: reached only via BL/B, never by fall-through,
   \ and each is self-terminating (RET, or C-DIE-TOKEN-NL's tail branch).
   LADTPUSHTOK LABEL@ LBL,              \ push captured token addr+len as checker-bridge args
      9 DATA TKA-CELL LDR,  9 G-PUSH
      9 DATA TKL-CELL LDR,  9 G-PUSH  RET,
   LMFRTOP LABEL@ LBL,                  \ x15 = &top match-frame family-id slot (x14 scratch)
      14 DATA CMFRD-CELL LDR,  14 14 1 SUBI,  14 14 3 LSLI,  14 14 CMFR-OFF ADDI,  15 DATA 14 ADD,  RET,
   LADTDIE LABEL@ LBL,                  \ x1=msg x2=len live: write(2) diagnostic then fail-closed exit 70
      0 2 MOVZ,  NR-WRITE SYS,  70 C-DIE-TOKEN-NL
   s2 LBL,  EM-ADT-CON-VAR
   s3 LBL,  EM-ADT-MATCH-FAM
   s4 LBL,  EM-ADT-MATCH-VAR
   s5 LBL,  EM-ADT-MATCH-OF
   off LBL, ;
s" em-compile-adt-mode" s" --" TRUST
package COMPILE-EMIT
public

: EM-COMPILE ( -- )
   LBL {: lnotsemi :}
   LCOMPILE LABEL@ LBL,
   EM-COMPILE-ADT-MODE        s" compile/adt" ENGINE-SIZE:MARK
   lnotsemi EM-COMPILE-SEMI   s" compile/semi" ENGINE-SIZE:MARK
   EM-COMPILE-LOCAL           s" compile/local" ENGINE-SIZE:MARK
   EM-COMPILE-P2WIDE          s" compile/p2wide" ENGINE-SIZE:MARK
   EM-COMPILE-KEYWORDS        s" compile/keywords" ENGINE-SIZE:MARK
   EM-COMPILE-LITERAL         s" compile/literal" ENGINE-SIZE:MARK
   ENGINE-EMIT:EM-COMPILE-OPS s" compile/ops" ENGINE-SIZE:MARK
   EM-COMPILE-CALL            s" compile/call" ENGINE-SIZE:MARK
   EM-COMPILE-UNDEF           s" compile/undef" ENGINE-SIZE:MARK
   EM-COMPILE-DIE             s" compile/die" ENGINE-SIZE:MARK
   EM-COMPILE-EXIT            s" compile/exit" ENGINE-SIZE:MARK
   EM-EVAL-THROW-RECOVER      s" compile/eval-recover" ENGINE-SIZE:MARK ;
s" em-compile" s" --" TRUST

;package

\ The main-loop emitter entry belongs to the emitter package habu1.f opens for
\ the primitive sections: its caller EMIT-CODE-SECTIONS resolves it bare in the
\ reopened block below.
package ENGINE-EMIT

: EMIT-MAIN ( -- )
   LBL LMAIN !  LBL LEXIT !  LBL LCOMPILE !  LBL LUNDEF !  LBL LUNDERFLOW !  LBL LARITY !
   EM-STARTUP                 s" main/startup" ENGINE-SIZE:MARK
   EM-COMMENT                 s" main/comment" ENGINE-SIZE:MARK
   INTERP-EMIT:EM-INTERPRET
   COMPILE-EMIT:EM-COMPILE
   EM-INTERPRET-UNDERFLOW     s" main/underflow" ENGINE-SIZE:MARK ;
s" emit-main" s" --" TRUST

;package

\ Pre-execution arity guard (LARITY). A deref/execute/dispatch primitive (@ !
\ +! c@ c! atomic@ atomic! atomic-add atomic-cas count type execute
\ run-in-stack int-mark min-in-mark, plus the census additions of
\ dot habu-habu-certified-words-84e84eaf: evaluate catch ffi-call* patch32
\ search-wl set-check cp! ndict! - see the habu1.f GDEREF table) as the
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
\ SRCA@ refines the raw source-buffer cell for the final byte copy.
\ Retirement: habu-builder-trust-rows-c5d41af6.
variable SRCA
: SRCA@ ( -- ptr u8 )
   SRCA @ ;
s" SRCA@" s" -- ptr u8" TRUST

: EMIT-RESET-BUILDER ( ptr u8 n -- )
   SRCN !  SRCA !
   ASM-INIT  0 #PL !  0 PNP ! ;

\ Label allocation for the emitter: one private allocator per engine region,
\ one public entry that reserves every label a build uses.
package LABELS

: CORE ( -- )
   LBL LANCHOR !  LBL LFIND !  LBL LNUM !  LBL LDICT !  LBL LSRC !  LBL LIMGEND !
   LBL LCEMIT !  LBL LCEMITBL !  LBL LTOK !  LBL LPROT !  LBL LPROTREC !  LBL LPROTSPAN !  LBL LFLUSH !  LBL LNCOUNT !
   LBL PROT:LOPEN !  LBL PROT:LCLOSE !  LBL PROT:LGROW !  LBL PROT:LSPAN !  LBL PROT:LCF !
   LBL LAOTCODE !  LBL LAOTDICT !  LBL LAOTCODELEN !
   LBL LAOTNREC !  LBL LAOTNSITE !  LBL LAOTSITES !  LBL LAOTNAMES !  LBL LAOTNAMESLEN !
   LBL LAOTNDSITE !  LBL LAOTDSITES !  LBL LAOTDATAD0 !  LBL LAOTDATASIZE !
   LBL AOT-WINDOW:LDATA !  LBL AOT-WINDOW:LNXTOFF !  LBL AOT-WINDOW:LXTOFFS !
   LBL LAOTNCSITE !  LBL LAOTCSITES !  LBL LAOTCODEB0 !
   LBL AOT-XTSITE:LCOUNT !  LBL AOT-XTSITE:LROWS !
   LBL LAOTBOOTRUN !
   LBL AOT-SIG:LLEN !  LBL AOT-SIG:LSPAN !  LBL AOT-SIG:LNAME !
   LBL LAOTNPWID !  LBL LAOTPWID !  LBL LAOTPROT !  LBL LPROTWIDQ !
   LBL AOT-WINDOW:LWIDW0 !  LBL AOT-WINDOW:LWIDSPAN !  LBL AOT-WINDOW:LNPWIN !  LBL AOT-WINDOW:LPWIN !
   LBL LBCAP !  LBL LBCS !  LBL LESCDEC !  LBL LESCHEX !  LBL LESCSCAN !  LBL LESCCOPY !
   LBL LSNAPRBD !  LBL LHIDXADD !  LBL LHIDXBUILD !
   LBL HIDX:LREBUILD !  LBL HIDX:LFULL !  LBL WLFIND:LENTRY !
   LBL SNAP-RELOC:LCALLS !  LBL SNAP-RELOC:LXT !  LBL SNAP-RELOC:LMARK !
   LBL SNAP-RELOC:LADDRS !  LBL SNAP-RELOC:LADDRSITE !
   LBL LQUALIFYDEF !  LBL LSTOREDEFNAME !
   LBL LAOTWIDGATE !  LBL AOT-WINDOW:LOUTSIDE !
   LBL LCFPUSH !  LBL LCFPOP !  LBL LPAT !  LBL LKWCMP !  LBL LTOPHOOK ! ;

: CONTROL ( -- )
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
   LBL LKWTRUSTED !  LBL KWDATA:LKWTRUSTDECL !  LBL KWDATA:LKWTRUSTRAW !  LBL LKWCHKDOES !  LBL LKWKERNEL !
   LBL LKWPACKAGE !  LBL LKWPUBLIC !  LBL LKWPRIVATE !  LBL LKWSEMIPACKAGE !
   LBL LKWDUPDEF !
   LBL LCHKPACKAGE !  LBL LCHKPUB !  LBL LCHKPRI !  LBL LCHKENDPKG !
   LBL LCHKDEFER !  LBL LRESTAB !  LBL LRECWPUB !  LBL LRECMIQ !  LBL HOLD-EMIT:LHOLDQ !  LBL LP2DOESW !
   LBL LKWEXPORT !  LBL LCHKEXPORT !
   LBL LKWUSING !  LBL LKWSEMIUSING !  LBL LCHKUSING !  LBL LFINDUSED !
   LBL LKWQUOT !  LBL LKWSEMIQ !  LBL LKWDEFER !  LBL LKWIS !  LBL LKWDEFERUNSET !
   LBL DEFER-DIAG:LDEFNOTOKEN !  LBL DEFER-DIAG:LDEFNOTFOUND !
   LBL DEFER-DIAG:LDEFNOTDEFER !  LBL DEFER-DIAG:LDEFNONAME !  LBL DEFER-DIAG:LDEFHINT !
   LBL LSIGPTRA !  LBL LSIGA !
   LBL LKWCONSTRUCT !  LBL LKWMATCH !  LBL LKWSEMIMATCH !
   LBL LTFLCONFAM !  LBL LTFLCVAR !
   LBL LTFLMATCHFAM !  LBL LTFLNAME !  LBL LBADTAGPFX !  LBL LBADTAGSFX ! ;

: RUNTIME ( -- )
   LBL LBCHAIN !  LBL LCREATE !  LBL LDOESPATCH !
   LBL LREAD !  LBL LRBYE !  LBL LRDIE !  LBL LRREC !  LBL LQNL !  LBL LOKS !
   LBL LEX0 !  LBL LUN0 !  LBL LEVALREC !
   LBL LCRASHH !  LBL LHEX !  LBL LHDR !  LBL LTRAPH !  LBL LBPH !  LBL BP-CALLER:LBPLH !  LBL LBPSH !  LBL LBPWH !  LBL LBADLOC !
   LBL LSRCRD !  LBL LSHBANG !  LBL LOPENERR !  LBL LOPENNL !
   LBL LUNCAUGHT !  LBL LUNCMSG !
   LBL LWIDE !  LBL LWIDEMSG !  LBL LDIAGRET !
   LBL LINTERNAL !  LBL LINTMSG !
   LBL LMININ !  LBL LMINMSG !
   LBL LPREFMISS !  LBL LPREFMISSMSG !
   LBL LORPHAN !  LBL LORPHANMSG !
   LBL LCFCAP !  LBL LCFCAPMSG !
   LBL LCSTR !  LBL LCSTRMSG !
   LBL LDPBAD !  LBL LDPBADMSG !
   LBL LCOMPILEDIE !
   LBL LDICTFULL !  LBL LCODEFULL !
   LBL LSNAPBAD !  LBL LSNAPVER !
   LBL SNAP-RELOC:LCALLMSG !  LBL SNAP-RELOC:LXTMSG !  LBL SNAP-RELOC:LADDRMSG !
   LBL SNAP-RELOC:LXTBANDMSG !
   LBL LSRCFULL !  LBL LSRCREAD !  LBL LBADSTR !
   LBL LPROTPUB !  LBL LPROTAOT !
   LBL LMMAPCODE !  LBL LMMAPDATA !  LBL LBLRANGE !
   LBL LFLAGMATCH !  LBL LSRCBADFLAG !  LBL LFLAGTAB !
   LBL LBADFLAG !  LBL LUSAGE1 !  LBL LUSAGE2 !  LBL LSPC ! ;

: SOURCES ( -- )
   LBL LPLINUXTARGET !  LBL LPMACOSTARGET !
   LBL LPLINUXLAYOUT !  LBL LPMACOSLAYOUT !
   LBL LPUTIL !  LBL LPCELL !  LBL LPPTRSTORAGE !
   LBL LPSTRUCTURES !  LBL LPBYTES !  LBL LPENGINEERROR !  LBL LPCHECKER !  LBL LPENGINEERROREFFECTS !
   LBL LPLOWERCERTBASE !  LBL LPRENDER !  LBL LPHOOK !
   LBL LPCELLEFF !  LBL LPPTRSTORAGEEFF !  LBL LPDECLTXN !  LBL LPGENDECL !
   LBL LPTYPESCHEMA !  LBL LPTYPEFAM !  LBL LPSUMTYPE !  LBL LPLAYOUTBUF !  LBL LPLAYOUTVALID !
   LBL LPHABULAYOUT !
   LBL LPENVBASE !  LBL LPINCLUDE !  LBL LPSCRIPTARGV !  LBL LPINTMARK !  LBL LPROLES !
   LBL LPDECLEVENT !  LBL LPSTRUCTMAKE !  LBL LPSTRUCTDECL !  LBL LPENUMDECL !
   LBL LPENUMS !  LBL LPEXECVECTOR !  LBL LPSHA256 !  LBL LPTFAMSHA !
   LBL LPCOMBINATORS !  LBL LPXREF !  LBL LPGENDECLDICT !  LBL LPGENDECLPROT !
   LBL LPLAYOUTSEAL !  LBL LPLOWERCERTSEAL !
   LBL LPTOPROW !
   LBL LPPRELUDE !  LBL LPERRORS !  LBL LPOPTION !
   LBL LPCADTYPES !  LBL LPCADARITH !  LBL LPSTRING !
   LBL LPMEMORY !  LBL LPVECTOR !
   LBL PFX-CHAIN:LTAB !
   LBL LCHKSNAPTOKEN ! ;

: JIT ( -- )
   LBL PROF:LPROFH !  LBL PROF:LPROFDUMP !
   LBL LVSPILL !  LBL LVLITPUSH !  LBL LVPUSHC !
   LBL LVTOP2C !  LBL LVFOLDPUT !
   LBL LVRALLOC !  LBL LVBIT !  LBL LVRINIT !  LBL LVMOVK !  LBL LVFORCEK !  LBL LVBINPREP !  LBL LVBINIPREP !  LBL LVPUSHR !
   LBL LVPUSHF !  LBL LFRALLOC !  LBL LFFORCEK !  LBL LFBINPREP !
   LBL LKWFPLUS !  LBL LKWFMINUS !  LBL LKWFSTAR !  LBL LKWFSLASH !
   LBL LVDROP !  LBL LVSWAPX !  LBL LVNIPX !  LBL LVCOPY !
   LBL LVSNAP !  LBL LVRECON ! ;

: OPS ( -- )
   LBL LKWPLUS !  LBL LKWMINUS !  LBL LKWSTAR !
   LBL LKWAND2 !  LBL LKWOR2 !  LBL LKWXOR2 !  LBL LKWLSH !  LBL LKWRSH !
   LBL LKWDUP2 !  LBL LKWDROP2 !  LBL LKWSWAP2 !
   LBL LKWOVER2 !  LBL LKWNIP2 !
   LBL LKWEQ2 !  LBL LKWNE2 !  LBL LKWLT2 !
   LBL LKWGT2 !  LBL LKWLE2 !  LBL LKWGE2 !
   LBL LKWINC !  LBL LKWDEC !  LBL LKWZEQ !
   LBL LKWZLT !  LBL LKWNEG2 !  LBL LKWINV2 ! ;

: P2 ( -- )
   LBL LCERTBYTES !  LBL LP2CWAT !  LBL LP2CDESC !
   LBL LKWTUCK3 !  LBL LKWROT3 !  LBL LKWMROT3 !
   LBL LKW2DUP3 !  LBL LKW2DROP3 !  LBL LKW2SWAP3 !  LBL LKW2OVER3 !
   LBL LKW2TOR3 !  LBL LKW2RFROM3 !  LBL LKW2RFET3 !
   LBL LKWAT2 !  LBL LKWSTORE2 !
   LBL LP2COPY !  LBL LP2DROPN !  LBL LP2REV !  LBL LP2ROT !  LBL LP2RS !
   LBL LP2FETCH !  LBL LP2STORE !  LBL LP2VEXEC !  LBL LP2VEMIT !  LBL LP2NEST !
   LBL LOWER-TXN-CODE:BAD !  LBL LOWER-TXN-CODE:MEM !
   LBL LOWER-TXN-CODE:FULL !  LBL LOWER-TXN-CODE:DRIFT !
   LBL LOWER-TXN-CODE:VDESC !  LBL LOWER-TXN-CODE:DRIFT-FAIL ! ;

public

: INIT ( -- )
   CORE
   CONTROL
   RUNTIME
   SOURCES
   JIT
   OPS
   P2 ;

;package

\ Bake the AOT section: blob length + blob, record count + N compact AOT-CREC-ROW
\ records (blob-relative code span, name-pool reference, flags, wid), call-site
\ count + M 8-byte rows (blob-off u32, name-off u32), then the name pool and the
\ DATA/CODE/window offset tables, all u32. Emitted LAST of the three image parts
\ (ENGINE-EMIT:FORTH says why), after the baked source, so it shifts nothing and
\ no reach depends on its size.
\ EVERY LABEL THIS WORD BINDS IS ADDRESSED WITH TADR,, NEVER ADR,. That is a rule
\ about the SECTION, not about how far away a particular label happens to land: a
\ blob of a few hundred bytes and a blob of a megabyte are the same kind of thing
\ and the boot code cannot tell them apart. Reordering the rows below is
\ therefore free, and an ADR, into this section is a defect whatever today's
\ measurement says (tools/aot-section-reach-lint.f refuses one).
\ LAOTNREC = 0 makes EM-SEED-AOT skip the whole pass (stage2/maker/snap).
: EMIT-AOT-SITES ( -- )   \ packed rows (blob-off u32 + name-off u32 + scope u32)
   AOT-SITE-N @ 0 > IF AOT-SITE-BUF@ AOT-SITE-N @ SITE-ROW * BYTES, THEN ;
: EMIT-AOT-DSITES ( -- )   \ packed u32 DATA-site offsets
   AOT-DSITE-N @ 0 > IF AOT-DSITE-BUF@ AOT-DSITE-N @ 4 * BYTES, THEN ;
: EMIT-AOT-CSITES ( -- )   \ packed u32 CODE-site offsets (after the AOT-DSITE-N DATA u32s)
   AOT-CSITE-N @ 0 > IF AOT-DSITE-BUF@ AOT-DSITE-N @ 4 * + AOT-CSITE-N @ 4 * BYTES, THEN ;
package AOT-XTSITE
public
: EMIT-ROWS ( -- )   \ packed 8B rows (blob-off u32 + name-off u32)
   N @ 0 > IF BUF@ N @ 8 * BYTES, THEN ;
;package
package AOT-WINDOW
public
: EMIT-DATA ( -- )   \ the window's DATA content, declared address cells zeroed
   AOT-DATA-SIZE @ 0 > IF DATA-BUF@ AOT-DATA-SIZE @ BYTES, THEN ;
: EMIT-XTOFFS ( -- )   \ packed u32 window offsets of the declared address cells
   XTOFF-N @ 0 > IF XTOFF-BUF@ XTOFF-N @ 4 * BYTES, THEN ;
: EMIT-PWIN ( -- )   \ packed u32 window-relative protected WIDs
   AOT-PWIN-N @ 0 > IF AOT-PWIN-BUF@ AOT-PWIN-N @ 4 * BYTES, THEN ;
;package
\ ---- the checker payload: one span, its own table, three sections -------------
\ WHY IT IS ONE SPAN. The seed publishes it with the two DATA cells layout.f set
\ aside (AOT-SIG:POOL-CELL and LEN-CELL), and the checker needs three: the
\ signature rows, the strings they name, and the type registry the signatures
\ resolve against. So the span carries its own table - a section count, then a
\ row of (offset, length) each, then the sections - in the shape this file's own
\ artifact reader uses, and src/core/checker.f refuses every malformation of it
\ by name before it believes a byte.
\
\ IT IS ASSEMBLED INTO A BUFFER AND EMITTED ONCE. BYTES, pads its run to the
\ four-byte grain, so three separate runs would leave gaps the table would have
\ to describe; one run over one contiguous buffer leaves the offsets meaning
\ exactly what they say. The buffer is the payload's own, sized from the three
\ caps it concatenates.
package AOT-SIG-PAYLOAD
using AOT-BUF
public

3 constant SEC-N
SEC-N 16 * 8 + constant TBL-BYTES
TBL-BYTES  AOT-SIG-MAX SIG-ROW * +  AOT-SIG-STR-CAP +  AOT-REG-CAP +  constant CAP
create BUF CAP allot
variable LEN
variable CUR

: BUF@ ( -- ptr u8 ) BUF ;
s" AOT-SIG-PAYLOAD:BUF@" s" -- ptr u8" TRUST

: U64! ( n n -- ) {: v:n at:n :}
   8 0 ?do  v i 8 * rshift $FF and  BUF@ at i + + c!  loop ;

: SEC-PTR ( n -- ptr u8 ) {: k:n :}
   k 0 = if AOT-SIG-BUF@ exit then
   k 1 = if AOT-SIG-STR-BUF@ exit then
   AOT-REG-BUF@ ;

: SEC-LEN ( n -- n ) {: k:n :}
   k 0 = if AOT-SIG-N @ SIG-ROW * exit then
   k 1 = if AOT-SIG-STR-LEN @ exit then
   AOT-REG-LEN @ ;

\ Nothing to publish when nothing was captured, and that is the whole of the
\ "an engine with no seed behaves exactly as it did" story: LEN stays 0, the
\ seed skips, both cells stay 0 and the checker's intake is a cell read.
: BUILD ( -- )
   0 LEN !
   AOT-SIG-N @ 0= AOT-SIG-STR-LEN @ 0= and AOT-REG-LEN @ 0= and if exit then
   SEC-N 0 U64!
   TBL-BYTES CUR !
   SEC-N 0 ?do
      CUR @  i 16 * 8 +  U64!
      i SEC-LEN  i 16 * 16 +  U64!
      CUR @ i SEC-LEN + CUR !
   loop
   CUR @ CAP > if
      s" habu2: the checker payload exceeds its own buffer" ICODE-EXIT-RC die
   then
   TBL-BYTES CUR !
   SEC-N 0 ?do
      i SEC-PTR  BUF@ CUR @ +  i SEC-LEN  BYTE-COPY
      CUR @ i SEC-LEN + CUR !
   loop
   CUR @ LEN ! ;

\ BUILD is the caller's, so the length label and the bytes read one assembly
\ rather than two: EMIT-AOT-SEED builds once, emits the length, then emits this.
: EMIT ( -- )
   LEN @ 0 > if BUF@ LEN @ BYTES, then ;

;package

: EMIT-AOT-SEED ( -- )
   LAOTCODELEN LABEL@ LBL,  AOT-BLOB-LEN @ DCQ,
   LAOTCODE LABEL@ LBL,
   AOT-BLOB-LEN @ 0 > IF AOT-BLOB-BUF@ AOT-BLOB-LEN @ BYTES, THEN
   LAOTNREC LABEL@ LBL,  AOT-REC-N @ DCQ,
   LAOTDICT LABEL@ LBL,                          \ compact 16B records (EM-AOT-REGISTER-RECS expands to 48B)
   AOT-REC-N @ 0 > IF AOT-REC-BUF@ AOT-REC-MAX 48 * + AOT-REC-N @ AOT-CREC-ROW * BYTES, THEN
   LAOTNSITE LABEL@ LBL,  AOT-SITE-N @ DCQ,
   LAOTSITES LABEL@ LBL,  EMIT-AOT-SITES
   LAOTNAMESLEN LABEL@ LBL,  AOT-NAMES-LEN @ DCQ,
   LAOTNAMES LABEL@ LBL,
   AOT-NAMES-LEN @ 0 > IF AOT-NAMES-BUF@ AOT-NAMES-LEN @ BYTES, THEN
   LAOTDATASIZE LABEL@ LBL,  AOT-DATA-SIZE @ DCQ,
   LAOTDATAD0 LABEL@ LBL,  AOT-DATA-D0 @ DCQ,
   LAOTNDSITE LABEL@ LBL,  AOT-DSITE-N @ DCQ,
   LAOTDSITES LABEL@ LBL,  EMIT-AOT-DSITES
   AOT-WINDOW:LNXTOFF LABEL@ LBL,  AOT-WINDOW:XTOFF-N @ DCQ,
   AOT-WINDOW:LXTOFFS LABEL@ LBL,  AOT-WINDOW:EMIT-XTOFFS
   AOT-WINDOW:LDATA LABEL@ LBL,  AOT-WINDOW:EMIT-DATA
   LAOTCODEB0 LABEL@ LBL,  AOT-CODE-B0 @ DCQ,
   LAOTNCSITE LABEL@ LBL,  AOT-CSITE-N @ DCQ,
   LAOTCSITES LABEL@ LBL,  EMIT-AOT-CSITES
   AOT-XTSITE:LCOUNT LABEL@ LBL,  AOT-XTSITE:N @ DCQ,
   AOT-XTSITE:LROWS LABEL@ LBL,  AOT-XTSITE:EMIT-ROWS
   LAOTBOOTRUN LABEL@ LBL,  AOT-BOOTRUN-BUF@ AOT-BOOTRUN-LEN @ 1 + BYTES,   \ +1 = live 0 terminator
   LAOTNPWID LABEL@ LBL,  PROT-REG-TAG DCQ,                                  \ protected-WID frame: shape tag
   LAOTPWID LABEL@ LBL,                                                      \ then the fixed-width bitmap (TFAM 2b-v)
   AOT-PWID-BUF@ PROT-BITS-BYTES BYTES,
   AOT-WINDOW:LWIDW0 LABEL@ LBL,  AOT-WID-W0 @ DCQ,
   AOT-WINDOW:LWIDSPAN LABEL@ LBL,  AOT-WID-SPAN @ DCQ,
   AOT-WINDOW:LNPWIN LABEL@ LBL,  AOT-PWIN-N @ DCQ,
   AOT-WINDOW:LPWIN LABEL@ LBL,  AOT-WINDOW:EMIT-PWIN
   AOT-SIG-PAYLOAD:BUILD                          \ before the length label reads it
   AOT-SIG:LLEN LABEL@ LBL,  AOT-SIG-PAYLOAD:LEN @ DCQ,
   AOT-SIG:LNAME LABEL@ LBL,  AOT-SIG:INSTALL-NAME$ BYTES,
   AOT-SIG:LSPAN LABEL@ LBL,  AOT-SIG-PAYLOAD:EMIT ;

\ tok-imm? ( ptr u8 n -- n ): live-dictionary immediate probe for the checker
\ (dot habu-checker-fitting-arity-70dc94e4). Pops a token name, runs the same
\ LFIND the interpret dispatch uses, and pushes flags&2 - the DNAME-IMM bit
\ (habu1.f FIND-HMATCH folds it to bit 1) - so DO-TOK1 can reject a live
\ immediate as a checked body step. LFIND clobbers x3-x16 and preserves XDS
\ (x19)/DATA (x20); the FPRIM frame (GDEREF-F) holds our x30 across the BL.
: BTOKIMM ( -- )
   10 G-POP  9 G-POP                   \ x10 = len (TOS), x9 = addr: LFIND's token registers
   LFIND LABEL@ BL,                    \ x13 = found | imm<<1 | min-in byte | int flags
   9 13 2 ANDI,
   A G-PUSH ;

\ Build sequencing: the section emitters join the emitter package habu1.f opens
\ for EMIT-PRIMS, EMIT-PROTWID and EMIT-DICT, which they call bare, and FORTH is
\ this package's one public word -- what src/habu/build.f, src/habu/stdin.f and
\ ENGINE-BUILD:BUILD below all call.
package ENGINE-EMIT

: EMIT-PRIMITIVE-SECTIONS ( -- )
   EMIT-PRIMS                    s" primitives/base" ENGINE-SIZE:MARK
   EMIT-ARITY-GUARD             s" primitives/arity" ENGINE-SIZE:MARK
   s" snap-rebase" ['] BSNAPREBASE FPRIM
   s" DRAIN-PRETRUST" ['] BDRAINPRETRUST FPRIM
   s" tok-imm?" ['] BTOKIMM 2 GDEREF-F
   s" xt!" ['] SNAP-RELOC:BXTSTORE 2 GDEREF-F  s" primitives/extra" ENGINE-SIZE:MARK
   PROF:EMIT-PROF-PRIMS       s" primitives/prof" ENGINE-SIZE:MARK
   EMIT-FP-PRIMS              s" primitives/float" ENGINE-SIZE:MARK
   EMIT-CEMIT                 s" primitives/cemit" ENGINE-SIZE:MARK
   EMIT-CEMITBL               s" primitives/cemitbl" ENGINE-SIZE:MARK
   EMIT-BCAP                  s" primitives/capture" ENGINE-SIZE:MARK
   EMIT-TOK                   s" primitives/token" ENGINE-SIZE:MARK
   EMIT-PROT                  s" primitives/protect" ENGINE-SIZE:MARK
   EMIT-PROTWID                s" primitives/protected-wid" ENGINE-SIZE:MARK
   EMIT-FLUSH                 s" primitives/flush" ENGINE-SIZE:MARK
   EMIT-FIND                  s" primitives/find" ENGINE-SIZE:MARK
   WLFIND:EMIT                s" primitives/find-wl" ENGINE-SIZE:MARK
   EMIT-FIND-USED             s" primitives/find-used" ENGINE-SIZE:MARK
   EMIT-HIDX                  s" primitives/hash-index" ENGINE-SIZE:MARK
   EMIT-QUALIFY-DEF           s" primitives/qualify-def" ENGINE-SIZE:MARK
   EMIT-STORE-DEF-NAME        s" primitives/store-def-name" ENGINE-SIZE:MARK
   EMIT-NUM                   s" primitives/number" ENGINE-SIZE:MARK
   EMIT-TOPHOOK               s" primitives/top-hook" ENGINE-SIZE:MARK ;

: EMIT-DICTIONARY-SECTIONS ( -- )
   EMIT-CREATE
   DOESPATCH:EMIT
   EMIT-CF-HELPERS  EMIT-ESC-DECODE  EMIT-ESC-SCAN  EMIT-ESC-COPY
   EM-SNAPSHOT-REBASE-DICT  EM-AOTWIDGATE  AOT-WINDOW:EMIT-OUTSIDE  EMIT-AOT-PROT-RESTORE
   SNAP-RELOC:EMIT-CALLS  SNAP-RELOC:EMIT-MARK  SNAP-RELOC:EMIT-XT
   SNAP-RELOC:EMIT-ADDR-SITE  SNAP-RELOC:EMIT-ADDRS
   EMIT-LOC-FIND
   KWDATA:EMIT
   EMIT-FOLDKW
   EMIT-SHUFKW
   EMIT-CMPKW
   EMIT-UNKW
   EMIT-P2KW ;

: EMIT-RUNTIME-SECTIONS ( -- )
   EMIT-CRASH-HANDLER
   EMIT-TRAPH
   EMIT-HEX
   PROF:EMIT-PROFDUMP
   PROF:EMIT-PROF
   EMIT-SHEBANG-COMMENT
   EMIT-SOURCE-READ
   EMIT-FLAGS
   EMIT-JIT
   EMIT-P2-HELPERS
   LOWER-TXN:EMIT-VALIDATOR ;

: EMIT-CODE-SECTIONS ( -- )
   EMIT-MAIN
   EMIT-PRIMITIVE-SECTIONS
   EMIT-DICTIONARY-SECTIONS   s" dictionary-code" ENGINE-SIZE:MARK
   EMIT-RUNTIME-SECTIONS      s" runtime" ENGINE-SIZE:MARK
   EMIT-DICT                  s" seed-dictionary" ENGINE-SIZE:MARK ;

: EMIT-SOURCE-BYTES ( -- )
   LSRC LABEL@ LBL,  SRCA@ SRCN @ BYTES, ;

public

\ Records one region row per emitter phase; the container rows (header, page pad,
\ and the target tail) and the HABU_ENGINE_SIZE_MAP report are added post-sign by
\ src/habu/driver-io.f DRV-SIZE-MAP, once the image length is final and the map
\ can reconcile to the exact file size.
\ Balanced whole-engine emission boundary.
\ Retirement: habu-builder-trust-rows-c5d41af6.
\ THE THREE PARTS, IN THIS ORDER, AND WHY THE ORDER IS THE CONTRACT.
\ An image is engine code, then the baked source, then the AOT payload, and each
\ part is bounded by a different owner (src/arch/arm64/icode.f derives the code
\ window from all three). The order is what keeps the FIRST bound real: the boot
\ path reaches LSRC with an ADR, planted in the startup region, so the distance
\ from the image's first instruction to LSRC is the engine code half, and ?ADR
\ refuses at ADR-HI. Emitting the AOT payload BEFORE the source, as this did
\ until 2026-08-14, put the payload's megabytes inside that distance and the ADR,
\ then refused images whose engine code was a tenth of its allowance -- the wrong
\ file refusing first, and the reason dot habu-reach-the-seed-d1326596 exists.
\ The payload is last and is addressed only through TADR,, so its size cannot
\ enter any reach.
: FORTH ( ptr u8 n -- )
   ENGINE-SIZE:RESET
   EMIT-RESET-BUILDER
   LABELS:INIT
   EMIT-CODE-SECTIONS
   EMIT-SOURCE-BYTES          s" baked-source" ENGINE-SIZE:MARK
   EMIT-AOT-SEED              s" aot-seed" ENGINE-SIZE:MARK
   LIMGEND LABEL@ LBL, ;                 \ nothing follows: this label IS the content length
s" forth" s" ptr u8 n --" TRUST

;package

package ENGINE-BUILD
public
\ An escaped emitter throw is caught only by the internal driver, which exits;
\ the process-local build flag is therefore never observed after that edge.
: BUILD ( ptr u8 n -- )
   ARM
   ENGINE-EMIT:FORTH
   DISARM
   ;
;package

;using
;using
;using

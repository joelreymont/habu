\ habu2.f — engine-builder part 2: the JIT compiler
\ emitters (literal/call/keywords/locals/strings/do-loop), the outer-interpreter
\ main loop and kernel emission. Needs habu1.f (part 1). Emission is split into
\ phase words sharing label VARIABLES (a giant single word would need dozens of
\ locals); emission order is stable so the self-rebuild reaches a fixpoint.
require src/lib/fmt.f
require src/habu/address-cells.f
require src/habu/address-carrier.f
require src/habu/snapshot-format.f
require src/habu/cell-grid.f
require src/habu/code-span.f
require src/habu/aot-runtime.f
require src/habu/native-observer-cells.f
require src/habu/native-observer-extension.f
require src/habu/native-body-abi.f
require src/habu/native-host-cells.f
require src/habu/boot-files.f
require src/core/sha256.f
require src/compiler/fixed-body.f
\ The ARM64 encoders are package A64ASM's public surface (src/arch/arm64/asm.f).
require src/arch/arm64/icode.f
using A64ASM
using A64ICODE
\ The AOT capture buffers, their caps and the section budget are src/habu/aot-decl.f,
\ loaded before this file so bin/hb's capture and this emitter share one
\ declaration of the format.
using AOT-BUF
\ The register allocator's DATA cells, whose free masks this file resets at
\ REPEAT and at each new definition, are package REGALLOC-ABI's
\ (src/habu/regalloc-abi.f).
using REGALLOC-ABI
using CRASH
using REGALLOC
using RT
using SYSCALL

package ENGINE-EMIT
\ Emitter selection only. Chosen offsets are baked as instruction literals;
\ no emitted data cell can change a selected code entry's scope kind.
variable EM-SCOPE-CHILD-OFF
variable EM-C2-MUT-OFF
variable EM-C2-MUT-LOAN-OFF
variable EM-C2-INIT-OFF
variable EM-C2-RECORDS-OFF
variable EM-C2-RECORD-OFF
variable EM-C2-FIELD-OFF
public

\ A relocatable address never takes the scalar path. Scalars use the minimal
\ MOVZ/MOVN+MOVK chain (LVMOVK) into x16, never the fixed x9 chain the AOT
\ relocation recognises, so a scalar whose value lands in a DATA or CODE range
\ is never taken for an address. CODE addresses are four halves, DATA three;
\ both go through C-DATA-ADDR* / C-CODE-ADDR, which name the relocation kind.

: C-LIT ( -- )
   LVLITPUSH LABEL@ BL, ;
private
\ x11 holds the value.
: C-RAW-LIT ( -- )
   14 16 MOVZ,  LVMOVK LABEL@ BL, ;

\ A carrier word with its destination field cleared: the chain emitters OR in
\ the register x14 names.
: RD-FREE ( n -- n )
   ADDRESS-CARRIER:ADDR-RD-BITS rshift ADDRESS-CARRIER:ADDR-RD-BITS lshift ;

\ Fixed four-instruction MOVZ/MOVK chain of x11 into the register x14 names: the
\ relocatable form the AOT boot pass rewrites in place.
: C-ADDR-RAW ( -- )
   6 11 0 ADDI,  5 $FFFF MOVZ,
   7 6 5 AND,    7 7 5 LSLI,  8 W-MOVZ0 RD-FREE LIT64,  8 8 14 ORR,  9 8 7 ORR,  LCEMIT LABEL@ BL,
   7 6 16 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK1 RD-FREE LIT64,  8 8 14 ORR,  9 8 7 ORR,  LCEMIT LABEL@ BL,
   7 6 32 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK2 RD-FREE LIT64,  8 8 14 ORR,  9 8 7 ORR,  LCEMIT LABEL@ BL,
   7 6 48 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK3 RD-FREE LIT64,  8 8 14 ORR,  9 8 7 ORR,  LCEMIT LABEL@ BL, ;
: C-ADDR-PUSH ( -- )
   14 9 MOVZ,  C-ADDR-RAW
   9 W-PUSH9 LIT64,  LCEMIT LABEL@ BL, ;

\ Shared DATA must not use x20, which changes per task. Both fixed DATA mappings
\ fit three halves; high-to-low order distinguishes these from full addresses.
\ The chain loads the register x14 names.
: C-DATA-RAW ( -- )
   DATA-VA VA>N DATA-SIZE + $FFFFFFFFFFFF > if C-ADDR-RAW exit then
   LBL LBL {: absolute:label done:label :}
   5 DATA-VA VA>N LIT64,  6 11 5 SUB,  5 DATA-SIZE LIT64,
   6 5 CMP,  C-HI absolute BCOND,
   5 $FFFF MOVZ,
   7 11 32 LSRI,  7 7 5 AND,  7 7 5 LSLI,  8 ADDRESS-CARRIER:DATA-MOVZ2 LIT64,  8 8 14 ORR,  9 8 7 ORR,  LCEMIT LABEL@ BL,
   7 11 16 LSRI,  7 7 5 AND,  7 7 5 LSLI,  8 W-MOVK1 RD-FREE LIT64,  8 8 14 ORR,  9 8 7 ORR,  LCEMIT LABEL@ BL,
   7 11 5 AND,  7 7 5 LSLI,  8 ADDRESS-CARRIER:DATA-MOVK0 LIT64,  8 8 14 ORR,  9 8 7 ORR,  LCEMIT LABEL@ BL,
   done B,
   absolute LBL, C-ADDR-RAW
   done LBL, ;
;package
using ENGINE-EMIT
\ The three relocation-kind words and the CALL-or-INLINE emitter sit after the
\ snapshot-relocation labels; all four record a site through RELOC-EMIT:MARK-SITE.

package ENGINE-EMIT
public
\ A write of a literal's bytes takes its length from that literal (a -LEN word
\ or `nip`), and a `c,` byte table's from `here TABLE -` after its last byte,
\ never from a hand-counted byte count.
variable LTRAPH   variable LBPH   variable LBPSH   variable LBPWH   variable LBADLOC
variable LOPENNL
public
variable LUNCAUGHT   variable LUNCMSG   \ uncaught-top-level-throw reporter + its fd-2 message
private
variable LUNCRPT   variable LUNCPOS   variable LUNCLOOP   variable LUNCDONE   \ reporter branch + itoa labels
: UNCMSG$ ( -- ptr u8 n )   s" hb: uncaught throw code " ;
: UNCMSG-LEN ( -- n )   UNCMSG$ nip ;
public
variable LUNDEF   \ undefined-token reject; declared with its fellow diagnostics because C-BTICK, emitted long before the MAIN label block, branches to it
variable LWIDE   variable LWIDEMSG   variable LDIAGRET   \ interpret-mode wide-effect reject + its message + shared recovery tail
private
: WIDEMSG$ ( -- ptr u8 n )   s" hb: interpret-mode layout value: " ;
: WIDEMSG-LEN ( -- n )   WIDEMSG$ nip ;
public
variable LINTERNAL   variable LINTMSG   \ interpret-mode internal-word reject (DNAME-INT) + its message
private
: INTMSG$ ( -- ptr u8 n )   s" hb: internal engine word: " ;
: INTMSG-LEN ( -- n )   INTMSG$ nip ;
public
variable LTRUSTTICK   variable LTRUSTTICKMSG
private
: TRUSTTICKMSG$ ( -- ptr u8 n )   s" hb: trusted-only tick: " ;
: TRUSTTICKMSG-LEN ( -- n )   TRUSTTICKMSG$ nip ;
public
\ LPOLICY is the one refusal every sealed site shares: a keyword outside the
\ KWDATA design span [LKWIF, LKWDESIGNEND) and an undefined token end there.
\ It is declared with the diagnostics because LKWCMP, emitted long before the
\ MAIN label block, branches to it.
variable LPOLICY   variable LPOLICYMSG   variable LKWDESIGNEND
private
: POLICY-MSG$ ( -- ptr u8 n )   s" hb: not in vocabulary: " ;
: POLICY-MSG-LEN ( -- n )       POLICY-MSG$ nip ;
public
variable LMINMSG   \ the underdepth message LFLOORREC writes
private
: MINMSG$ ( -- ptr u8 n )   s" hb: interpret stack underdepth: " ;
: MINMSG-LEN ( -- n )   MINMSG$ nip ;
public
variable LFLOORREC   \ the underdepth throw for a data access below the base (crash.f C-CRASH-DATA-RECOVER resumes here)
variable LORPHAN   variable LORPHANMSG   \ orphan control-flow closer reject (empty CFSTK pop) + its message
private
: ORPHANMSG$ ( -- ptr u8 n )   s" hb: control-flow closer without opener: " ;
: ORPHANMSG-LEN ( -- n )   ORPHANMSG$ nip ;
public
variable LCFKIND   variable LCFKINDMSG   \ control-flow word whose open entry is another structure's (C-CF-AT, C-CF-NONE) + its message
private
: CFKINDMSG$ ( -- ptr u8 n )   s" hb: control-flow word does not match the open structure: " ;
: CFKINDMSG-LEN ( -- n )   CFKINDMSG$ nip ;
public
variable LCFCAP   variable LCFCAPMSG   \ control-flow stack overflow reject (LCFPUSH at CFSTK-DEPTH-MAX) + its message
variable LLOCWIDE   variable LLOCWIDEMSG   \ local name wider than LOC-NAME-CAP reject (C-LBRACE-STORE-ONE) + its message
private
: LOCWIDEMSG$ ( -- ptr u8 n )   s" hb: local name over 16 bytes: " ;
: LOCWIDEMSG-LEN ( -- n )   LOCWIDEMSG$ nip ;
public
variable LLOCMANY   variable LLOCMANYMSG   \ more than LOC-RECS locals in one definition reject (C-LBRACE-STORE-ONE) + its message
private
: LOCMANYMSG$ ( -- ptr u8 n )   s" hb: more than 64 locals in one definition: " ;
: LOCMANYMSG-LEN ( -- n )   LOCMANYMSG$ nip ;
: CFCAPMSG$ ( -- ptr u8 n )   s" hb: control-flow nesting too deep: " ;
: CFCAPMSG-LEN ( -- n )   CFCAPMSG$ nip ;
public
variable LCSTR   variable LCSTRMSG   \ counted-string >255 reject (C-ICQ/C-EICQ/C-CQ/C-ECQ) + its fd-2 message
private
: CSTRMSG$ ( -- ptr u8 n )   s" hb: counted string too long (max 255)" ;
: CSTRMSG-LEN ( -- n )   CSTRMSG$ nip ;
public
variable LDEFKWMSG
private
: DEFKWMSG$ ( -- ptr u8 n )   s" hb: compile keyword cannot be a definition name: " ;
public
\ Capacity refusals that state a count: one line naming what filled up, the
\ ceiling and the count. LDIAGU writes a number on fd 2.
variable LDIAGU     \ ( x9 = value ) unsigned decimal on fd 2, no newline
variable LBGROWMSG   variable LBGROWUNIT   \ the body text block could not grow (habu1.f BCAP-GROW); label LBGROWFAIL declared in habu1.f forward-ref block
variable LDPBADMSG   variable LDPBADOF   variable LDPBADUNIT   \ DP-heap bound reject (habu1.f DP-CHECK, allot/,/c,/definer sinks): the head, the " of " before the ceiling and the " bytes" that ends the line; label LDPBAD declared in habu1.f forward-ref block
variable LQNEST \ quotation-nesting overflow (J-QUOT at JIT-QUOT:LEVELS open); placed nowhere: only row-walk code branches to it
variable LSRCEND \ a source buffer ended inside a definition it opened; placed nowhere: only row-walk code branches to it

private
: DPBAD-MSG$ ( -- ptr u8 n )     s" hb: data space out of range: DP " ;
: DPBAD-OF$ ( -- ptr u8 n )      s"  of " ;
: DPBAD-UNIT$ ( -- ptr u8 n )    s"  bytes" ;
: BGROW-MSG$ ( -- ptr u8 n )     s" hb: cannot grow definition body text to " ;
: BGROW-UNIT$ ( -- ptr u8 n )    s"  bytes" ;
public
: UNSET-MSG$ ( -- ptr u8 n )     S\" hb: native compiler dispatch unset\n" ;
private
: P2SLOT-MSG$ ( -- ptr u8 n )    s" hb: locals frame over 4096 cells: " ;

: DPBADMSG-LEN ( -- n )      DPBAD-MSG$ nip ;
: DPBAD-OF-LEN ( -- n )      DPBAD-OF$ nip ;
: DPBAD-UNIT-LEN ( -- n )    DPBAD-UNIT$ nip ;
: BGROW-MSG-LEN ( -- n )     BGROW-MSG$ nip ;
: BGROW-UNIT-LEN ( -- n )    BGROW-UNIT$ nip ;
private

public
variable LCOMPILEDIE   \ shared recoverable compile-error tail: a die site that already wrote its diagnostic branches here with x0 = its sysexits exit code. Inside evaluate (EVALD>0) the aborted compile unwinds as a catchable throw of that SAME code via LEVALREC (RSP/CP/NDICT/XDS/DP + compile-state rollback, HIDX tolerates the stale records); at top level (EVALD==0) it calls exit_group(x0).
variable LSRCAT        \ the LCOMPILEDIE tail's location writer, a leaf a fail-closed die site calls before it exits
public
variable LDICTFULL   variable LCODEFULL   \ definer capacity-exit labels (dict-record / code-region full)
private
: DICTFULL-MSG$ ( -- ptr u8 n )   s" hb: dictionary full at: " ;
: CODEFULL-MSG$ ( -- ptr u8 n )   s" hb: code space full at: " ;
private
;package
\ Snapshot relocation labels, messages and instruction constant; the band offsets
\ and exit statuses are src/habu/layout.f package SNAP-RELOC. This file compiles
\ on a product engine, where no source reopens a baked package (docs/forth-card.md
\ § 6), so the emitter's half is its own package; HIDX-EMIT is the same split
\ for HIDX.
package RELOC-EMIT
public
variable LCALLMSG   \ a recorded call site does not hold a call instruction (CALLMAP-RC)
: CALLMSG$ ( -- ptr u8 n )   S\" hb: snapshot call map mismatch\n" ;
: CALLMSG-LEN ( -- n )   CALLMSG$ nip ;
variable LXTMSG     \ checked growth could not secure storage (XTCELL-RC)
: XTMSG$ ( -- ptr u8 n )   S\" hb: address-cell storage allocation failed\n" ;
: XTMSG-LEN ( -- n )   XTMSG$ nip ;
variable LXTHEADERMSG
: XTHEADERMSG$ ( -- ptr u8 n )   S\" hb: invalid address-cell storage header\n" ;
: XTHEADERMSG-LEN ( -- n )   XTHEADERMSG$ nip ;
variable LADDRMSG   \ a recorded address-literal site does not hold the MOVZ/MOVK chain (ADDRMAP-RC)
: ADDRMSG$ ( -- ptr u8 n )   S\" hb: snapshot address map mismatch\n" ;
: ADDRMSG-LEN ( -- n )   ADDRMSG$ nip ;
variable LXTBANDMSG \ a declared address cell is not a cell-aligned address inside DATA (XTBAND-RC)
: XTBANDMSG$ ( -- ptr u8 n )   S\" hb: snapshot address cell out of range\n" ;
: XTBANDMSG-LEN ( -- n )   XTBANDMSG$ nip ;
variable LXTKINDMSG \ one cell was declared as both an XT and a DATA pointer (XTKIND-RC)
: XTKINDMSG$ ( -- ptr u8 n )   S\" hb: snapshot address cell kind mismatch\n" ;
: XTKINDMSG-LEN ( -- n )   XTKINDMSG$ nip ;
variable LCALLS     \ region-to-text call relocation routine (snapshot write + restore)
variable LMARK      \ declare one DATA cell as holding a region address
variable LPTRMARK   \ declare one DATA cell as holding a DATA pointer
variable LSCRATCHMARK \ declare one DATA cell as a scratch pointer, saved as null
variable LROLLBACK  \ retire address rows at or above the saved DATA cut
variable LADDRS     \ address-literal relocation routine (snapshot write + restore)
variable LADDRSITE  \ record the address-literal chain whose first word is in x9
\ $94000000 >> 26, the opcode bits of an AArch64 BL: the call relocation pass
\ reads them with one shift instead of a mask against a 64-bit literal.
$25 constant BL-OP-HI

\ Record an address chain whose first instruction is held in the named register.
\ Compile handlers use CP; boot relocation uses the captured site's address.
: MARK-SITE-AT ( n -- ) {: site:n :}
   9 site 0 ADDI,
   LADDRSITE LABEL@ BL, ;

\ The compile handler records CP before emitting the chain's first instruction.
: MARK-SITE ( -- )
   CP MARK-SITE-AT ;

: EMIT-FULL-MESSAGE ( -- )
   LXTMSG LABEL@ LBL, XTMSG$ BYTES,
   LXTHEADERMSG LABEL@ LBL, XTHEADERMSG$ BYTES, ;

;package

package ENGINE-EMIT
public
\ The kind is decided at the emit site: DATA uses its three-half carrier, CODE
\ the absolute one. All three record their site, so the map says where every
\ chain starts. An AOT capture copies the blob to a different DP and must find
\ DATA chains too; recognising a chain by shape or value is refused, since an
\ integer or inline data can look like one.
\ RELOC-EMIT:EMIT-ADDRS runs once per band (x21 = base, x22 = length: the JIT
\ region, then the loaded __text) and rewrites a chain only when its address
\ lies in that band. DATA sits at the fixed DATA-VA far above both bands, so a
\ DATA site is visited and left alone: DATA does not move. The map stores no
\ kind: a consumer derives it from which of its window's disjoint DATA and code
\ spans the value lands in.
: C-DATA-ADDR ( -- )
   RELOC-EMIT:MARK-SITE
   14 9 MOVZ,  C-DATA-RAW
   9 W-PUSH9 LIT64,  LCEMIT LABEL@ BL, ;
private
\ x9, no push: the defer dispatch-cell address.
: C-DATA-ADDR-RAW ( -- )
   RELOC-EMIT:MARK-SITE
   14 9 MOVZ,  C-DATA-RAW ;
\ The target lives in the JIT region or the loaded __text, neither at the same
\ address after a snapshot restore, so the relocation pass rewrites this chain.
: C-CODE-ADDR ( -- )
   RELOC-EMIT:MARK-SITE
   C-ADDR-PUSH ;

$D63F0200 constant C-CALL-BLR-X16     \ blr x16: deferred-word indirect call

\ Tier 0 calls the resolved target in x11. Callees from either compiler keep
\ their own frames, literal pools and relocation records.
: C-CALL ( -- )
   LCEMITBL LABEL@ BL, ;

public
variable LBADSTR   \ boot source labeled rc-74 exits (prefix overflow / read error / string literal)
private
\ LBADSTR: an unterminated or bad-escape string literal. NO newline: the
\ LCOMPILEDIE tail appends ` at <path>:<line>` and the newline.
: BADSTR-MSG$ ( -- ptr u8 n )   s" hb: bad string literal" ;
: BADSTR-MSG-LEN ( -- n )   BADSTR-MSG$ nip ;
public
variable LSIGBAD              \ C-SIG-BAD's code; the token and the LCOMPILEDIE tail follow it
private
: SIGBAD-MSG$ ( -- ptr u8 n )   s" E-MISSING-SIGNATURE: " ;
: SIGBAD-MSG-LEN ( -- n )   SIGBAD-MSG$ nip ;
public
variable LSEALED              \ the exit-84 refusal's code (C-SEAL-PACKAGE-FAIL); the name and the LSRCAT location follow it
private
: SEALED-MSG$ ( -- ptr u8 n )   s" E-SEALED-PACKAGE: " ;
: SEALED-MSG-LEN ( -- n )   SEALED-MSG$ nip ;
public
variable LATMSG               \ " at " — the refusal-location separator LSRCAT writes before <path>:<line>
private
: ATMSG$ ( -- ptr u8 n )   s"  at " ;
: ATMSG-LEN ( -- n )   ATMSG$ nip ;
public
variable LMMAPCODE   variable LMMAPDATA   \ region mmap-fail/collision labeled rc-78 exits (code region / data region); message bytes live in the loaded __text image, so the write is valid even though the region being mapped does not exist yet
private
: MMAPCODE-MSG$ ( -- ptr u8 n )   S\" hb: cannot map fixed code region\n" ;
: MMAPCODE-MSG-LEN ( -- n )   MMAPCODE-MSG$ nip ;
: MMAPDATA-MSG$ ( -- ptr u8 n )   S\" hb: cannot map fixed data region\n" ;
: MMAPDATA-MSG-LEN ( -- n )   MMAPDATA-MSG$ nip ;
public
variable LBLRANGE   \ boot BL-range assertion labeled rc-81 exit: the mapped region fell outside BL's +/-128 MiB of __text
variable LKERNELDIGEST
private
: BLRANGE-MSG$ ( -- ptr u8 n )   S\" hb: code region out of BL range\n" ;
: BLRANGE-MSG-LEN ( -- n )   BLRANGE-MSG$ nip ;
public
variable LADDSUBIMM   \ the shared add/sub-immediate emitter (EMIT-ADDSUB-IMM): every transfer offset folds through it
variable LADDSUBBIG   \ transfer immediate past the two-halves range: labeled rc-75 compile refusal
private
: ADDSUBBIG-MSG$ ( -- ptr u8 n )   S\" hb: transfer immediate out of range\n" ;
: ADDSUBBIG-MSG-LEN ( -- n )   ADDSUBBIG-MSG$ nip ;

create BPH-KW 104 c, 97 c, 98 c, 117 c, 45 c, 98 c, 112 c, 58 c, 10 c,   \ habu-bp:\n
here BPH-KW - constant BPH-LEN
create BPS-KW 104 c, 97 c, 98 c, 117 c, 45 c, 98 c, 112 c, 45 c, 115 c, 116 c, 97 c, 99 c, 107 c, 58 c, 10 c,
here BPS-KW - constant BPS-LEN
create BPW-KW 104 c, 97 c, 98 c, 117 c, 45 c, 98 c, 112 c, 45 c, 119 c, 97 c, 116 c, 99 c, 104 c, 58 c, 10 c,
here BPW-KW - constant BPW-LEN
\ "habu: local cannot be inside quotation\n"
create BADLOC-KW $68 c, $61 c, $62 c, $75 c, $3A c, $20 c, $6C c, $6F c, $63 c, $61 c, $6C c, $20 c, $63 c, $61 c, $6E c, $6E c, $6F c, $74 c, $20 c, $62 c, $65 c, $20 c, $69 c, $6E c, $73 c, $69 c, $64 c, $65 c, $20 c, $71 c, $75 c, $6F c, $74 c, $61 c, $74 c, $69 c, $6F c, $6E c, $0A c,
here BADLOC-KW - constant BADLOC-LEN
create NL-KW 10 c,   \ single newline for the open-failure diagnostic
public
here NL-KW - constant NL-LEN

private
: C-TRAP-MCTX>R9 ( -- )
   HB-TARGET-LINUX? IF 9 2 LINUX-UC-MCTX-OFF ADDI, exit THEN
   9 4 MCTX-OFF LDR, ;

: C-MCTX-PC>R10 ( -- )
   HB-TARGET-LINUX? IF 10 9 LINUX-MCTX-PC-OFF LDR, exit THEN
   10 9 MACOS-MCTX-PC-OFF LDR, ;

: C-MCTX-X19>R12 ( -- )
   HB-TARGET-LINUX? IF 12 9 LINUX-MCTX-X19-OFF LDR, exit THEN
   12 9 MACOS-MCTX-X19-OFF LDR, ;
;package

\ The `habu-bp-lr:` line: its label, bytes and the read of the interrupted x30
\ from the signal mcontext; tools/code-owner.f turns the value into a record name.
\ Measured: the handler's own x30 is overwritten (LHEX is a BL) before the first
\ number reaches fd 2.
package BP-CALLER
public
variable LBPLH
create BPL-KW 104 c, 97 c, 98 c, 117 c, 45 c, 98 c, 112 c, 45 c, 108 c, 114 c, 58 c, 10 c,   \ habu-bp-lr:\n
here BPL-KW - constant BPL-LEN

: C-MCTX-LR>R9 ( -- )
   HB-TARGET-LINUX? IF 9 9 LINUX-MCTX-LR-OFF LDR, exit THEN
   9 9 MACOS-MCTX-LR-OFF LDR, ;
;package

package ENGINE-EMIT
: C-MCTX-SP-16! ( -- )
   HB-TARGET-LINUX? IF
      12 9 LINUX-MCTX-SP-OFF LDR,  12 12 16 SUBI,  12 9 LINUX-MCTX-SP-OFF STR, exit
   THEN
   12 9 MACOS-MCTX-SP-OFF LDR,  12 12 16 SUBI,  12 9 MACOS-MCTX-SP-OFF STR, ;

: C-MCTX-PC+4! ( -- )
   HB-TARGET-LINUX? IF
      12 9 LINUX-MCTX-PC-OFF LDR,  12 12 4 ADDI,  12 9 LINUX-MCTX-PC-OFF STR, exit
   THEN
   12 9 MACOS-MCTX-PC-OFF LDR,  12 12 4 ADDI,  12 9 MACOS-MCTX-PC-OFF STR, ;

: C-BP-HIT-SAVE ( -- )
   SP SP 80 SUBI,
   1 SP 0 STR,  4 SP 8 STR,  5 SP 16 STR,
   9 SP 24 STR,  10 SP 32 STR,  8 SP 40 STR,
   14 8 16 LDR,  14 14 1 ADDI,  14 8 16 STR,
   15 8 24 LDR,  12 15 1 LSRI, ;

: C-BP-STACK-RANGE ( -- )
   LBL LBL {: bad:label ready:label :}
   14 DATA STACK-ABI:BASE-CELL LDR,  10 DATA STACK-ABI:CAP-CELL LDR,
   9 SP 24 LDR,  C-MCTX-X19>R12
   0 bad STACK-GUARD:CHECK-CURSOR
   12 14 12 ADD,  12 SP 56 STR,  17 14 0 ADDI,
   ready B,
   bad LBL, STACK-GUARD:EXIT-BOUNDS
   ready LBL, ;

: C-BP-PRINT-HIT ( -- )
   LBL {: empty:label :}
   1 LBPH LABEL@ ADR,  0 2 MOVZ,  2 BPH-LEN MOVZ,  NR-WRITE SYS,
   9 SP 32 LDR,  LHEX LABEL@ BL,
   \ An empty interrupted stack has no top cell to print. Validate the saved
   \ cursor against the active allocation before either diagnostic reads it.
   C-BP-STACK-RANGE
   12 17 CMP, C-EQ empty BCOND,
   9 12 8 SUBI,  9 9 0 LDR,  LHEX LABEL@ BL,
   empty LBL,
   1 BP-CALLER:LBPLH LABEL@ ADR,  0 2 MOVZ,  2 BP-CALLER:BPL-LEN MOVZ,  NR-WRITE SYS,
   9 SP 24 LDR,  BP-CALLER:C-MCTX-LR>R9  LHEX LABEL@ BL,
   1 LBPSH LABEL@ ADR,  0 2 MOVZ,  2 BPS-LEN MOVZ,  NR-WRITE SYS, ;

: C-BP-WATCH-HEAD ( -- )
   1 LBPWH LABEL@ ADR,  0 2 MOVZ,  2 BPW-LEN MOVZ,  NR-WRITE SYS,
   6 DATA BPWN-CELL LDR,  7 DATA BPWBASE-CELL LDR,
   17 0 MOVZ, ;

: C-BP-WATCH-ROW ( -- )
   22 17 3 LSLI,  22 7 22 ADD,  23 22 0 LDR,
   9 23 0 ADDI,  LHEX LABEL@ BL,
   9 23 0 LDR,  LHEX LABEL@ BL,
   17 17 1 ADDI, ;

: C-BP-RESTORE-ONESHOT ( -- )
   8 SP 40 LDR,  11 8 0 LDR,  12 8 8 LDR,
   13 DATA REGION-ALIAS:DELTA-CELL LDR,  13 11 13 ADD,  12 13 0 STRW,   \ through the alias
   9 11 0 ADDI,  LFLUSH LABEL@ BL,
   8 SP 40 LDR,  12 0 MOVZ,  12 8 0 STR, ;

\ Store the interrupted thread's link register at its (already lowered) stack
\ pointer: the second half of the pre-indexed save, in the ucontext.
: C-MCTX-LINK>SLOT ( -- )
   HB-TARGET-LINUX? IF
      12 9 LINUX-MCTX-SP-OFF LDR,  14 9 LINUX-MCTX-LR-OFF LDR,  14 12 0 STR, exit
   THEN
   12 9 MACOS-MCTX-SP-OFF LDR,  14 9 MACOS-MCTX-LR-OFF LDR,  14 12 0 STR, ;

\ The opcode classes of an instruction that reads or sets PC, each as
\ (word >> shift) [& mask] = value. The breakpoint handler below refuses to run
\ such an instruction away from its address.
0 ENC-B 26 rshift                constant CINL-B-OP
0 C-EQ ENC-BCOND 24 rshift       constant CINL-BCOND-OP
0 0 ENC-CBZ 25 rshift $3F and    constant CINL-CBZ-OP    \ cbz, cbnz, either width
0 0 ENC-ADR 24 rshift $1F and    constant CINL-ADR-OP    \ adr, adrp
$1B constant CINL-TBZ-OP         \ tbz, tbnz, either width: bits 30:25 = 011011
$3B constant CINL-LDRLIT-MASK    \ ldr, ldrsw, prfm (literal), any register file:
$18 constant CINL-LDRLIT-OP      \   bits 29:27 = 011 and 25:24 = 00

: C-MCTX-PC<R12 ( -- )
   HB-TARGET-LINUX? IF 12 9 LINUX-MCTX-PC-OFF STR, exit THEN
   12 9 MACOS-MCTX-PC-OFF STR, ;

\ Branch to `rel` when x13, the displaced instruction, reads or sets PC: the
\ PC-relative classes, and blr, whose return address would be the trampoline's.
\ Clobbers x14, x15.
: C-BP-PCREL ( label -- ) {: rel:label :}
   14 13 26 LSRI,  14 14 $1F ANDI,  14 CINL-B-OP CMPI,  C-EQ rel BCOND,   \ b, bl
   14 13 24 LSRI,  14 CINL-BCOND-OP CMPI,  C-EQ rel BCOND,
   14 13 25 LSRI,  14 14 $3F ANDI,  14 CINL-CBZ-OP CMPI,  C-EQ rel BCOND,
   14 CINL-TBZ-OP CMPI,  C-EQ rel BCOND,
   14 13 24 LSRI,  14 14 $1F ANDI,  14 CINL-ADR-OP CMPI,  C-EQ rel BCOND,
   14 13 24 LSRI,  15 CINL-LDRLIT-MASK MOVZ,  14 14 15 AND,
   14 CINL-LDRLIT-OP CMPI,  C-EQ rel BCOND,
   15 $FFFFFC1F LIT64,  14 13 15 AND,  15 0 ENC-BLR LIT64,  14 15 CMP,  C-EQ rel BCOND, ;

\ Resume at the slot's trampoline: [BPTRAMP-CELL] + index * 8, and the index * 8
\ is the slot's offset in BPTAB, 32 bytes a slot, shifted right by two.
: C-BP-TRAMP ( -- )
   14 8 DATA SUB,  15 BPTAB-OFF LIT64,  14 14 15 SUB,  14 14 2 LSRI,
   12 DATA BPTRAMP-CELL LDR,  12 12 14 ADD,
   C-MCTX-PC<R12 ;

\ A persistent breakpoint keeps its BRK, so the handler runs the displaced
\ instruction. The link-register save and the nop EM-COMPILE-RET leaves in a
\ leaf's slot are emulated; any other instruction runs in the slot's trampoline
\ (debug.f writes it when it plants the BRK: the instruction, then `b` back).
\ One that reads or sets PC (C-BP-PCREL) degrades the breakpoint to one-shot.
\ The emulated `str x30,[sp,#-16]!` lands in this handler's signal frame, over
\ the arm64 frame_record {fp, lr}: nothing reads it back (sigreturn restores
\ from uc_mcontext, lower in the frame; the handler addresses locals off sp,
\ never x29), and that address is the slot the resumed word's epilogue reads.
: C-BP-EMULATE ( -- )
   LBL LBL LBL LBL {: framed:label step:label oneshot:label done:label :}
   9 SP 24 LDR,                                       \ the interrupted mcontext
   8 SP 40 LDR,  13 8 8 LDR,                          \ the word the BRK replaced
   14 W-LINKSAVE LIT64,  13 14 CMP,  C-EQ framed BCOND,
   14 W-NOP LIT64,  13 14 CMP,  C-EQ step BCOND,
   oneshot C-BP-PCREL
      C-BP-TRAMP
      done B,
   oneshot LBL,
      \ Give the word its instruction back and resume ON it: the breakpoint
      \ degrades to one-shot, a visible loss, where running the instruction at
      \ the wrong address is a wrong result.
      C-BP-RESTORE-ONESHOT
      done B,
   framed LBL,
      C-MCTX-SP-16!
      C-MCTX-LINK>SLOT
   step LBL,
      C-MCTX-PC+4!
   done LBL, ;

: C-BP-SCAN ( label label label label -- )
   {: tno:label bscan:label bnext:label bhit:label :}
   6 8 MOVZ,  7 0 MOVZ,
   bscan LBL,
      7 6 CMP,  C-GE tno BCOND,
      8 7 5 LSLI,  14 BPTAB-OFF LIT64,  8 8 14 ADD,  8 DATA 8 ADD,
      13 8 0 LDR,  13 bnext CBZ,
      10 13 CMP,  C-EQ bhit BCOND,
      bnext LBL,  7 7 1 ADDI,  bscan B, ;

: C-BP-STACK-DUMP ( label label -- )
   {: sdump:label sdone:label :}
   sdump LBL,
      14 SP 56 LDR,  17 14 CMP,  C-CS sdone BCOND,
      9 17 0 LDR,  17 SP 48 STR,  LHEX LABEL@ BL,
      17 SP 48 LDR,  17 17 8 ADDI,  sdump B,
   sdone LBL, ;

: C-BP-WATCH-DUMP ( label label -- )
   {: wloop:label wdone:label :}
   6 DATA BPWN-CELL LDR,  6 wdone CBZ,
   7 DATA BPWBASE-CELL LDR,  7 wdone CBZ,
   C-BP-WATCH-HEAD
   wloop LBL,
      17 6 CMP,  C-GE wdone BCOND,
      C-BP-WATCH-ROW  wloop B,
   wdone LBL, ;

\ LTRAPH, the trap signal entry: a BPTAB hit is reported and resumed (one-shot
\ restores the instruction, persistent runs C-BP-EMULATE); any other trap falls
\ through to the crash dump with x2/x4 untouched.
: EMIT-TRAPH ( -- )
   LTRAPH LABEL@ LBL,
   LBL {: tno :}
   C-TRAP-MCTX>R9
   C-MCTX-PC>R10
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
   LBPH LABEL@ LBL,  BPH-KW BPH-LEN BYTES,
   BP-CALLER:LBPLH LABEL@ LBL, BP-CALLER:BPL-KW BP-CALLER:BPL-LEN BYTES,
   LBPSH LABEL@ LBL, BPS-KW BPS-LEN BYTES,
   LBPWH LABEL@ LBL, BPW-KW BPW-LEN BYTES,
   LBADLOC LABEL@ LBL, BADLOC-KW BADLOC-LEN BYTES,
   LOPENNL LABEL@ LBL, NL-KW NL-LEN BYTES,
   LUNCMSG LABEL@ LBL, UNCMSG$ BYTES,                  \ LUNCAUGHT appends the signed code + newline
   LWIDEMSG LABEL@ LBL, WIDEMSG$ BYTES,                \ LWIDE appends the token + newline
   LINTMSG LABEL@ LBL, INTMSG$ BYTES,                  \ LINTERNAL appends the token + newline
   LTRUSTTICKMSG LABEL@ LBL, TRUSTTICKMSG$ BYTES,
   LPOLICYMSG LABEL@ LBL, POLICY-MSG$ BYTES,          \ LPOLICY appends the token; the LCOMPILEDIE tail appends the location + newline
   LMINMSG LABEL@ LBL, MINMSG$ BYTES,                  \ LFLOORREC appends the token + newline
   LDEFKWMSG LABEL@ LBL, DEFKWMSG$ BYTES,
   LORPHANMSG LABEL@ LBL, ORPHANMSG$ BYTES,            \ LORPHAN appends the closer token + newline
   LCFCAPMSG LABEL@ LBL, CFCAPMSG$ BYTES,              \ LCFCAP appends the opener token + newline
   LCFKINDMSG LABEL@ LBL, CFKINDMSG$ BYTES,            \ LCFKIND appends the token + newline
   LLOCWIDEMSG LABEL@ LBL, LOCWIDEMSG$ BYTES,          \ LLOCWIDE appends the declaration token + newline
   LLOCMANYMSG LABEL@ LBL, LOCMANYMSG$ BYTES,          \ LLOCMANY appends the declaration token + newline
   LCSTRMSG LABEL@ LBL, CSTRMSG$ BYTES,                \ LCSTR appends a newline
   LDPBADMSG LABEL@ LBL, DPBAD-MSG$ BYTES,                               \ LDPBAD appends the refused DP, " of ", the ceiling and " bytes" (DP-heap allot bound)
   LDPBADOF LABEL@ LBL, DPBAD-OF$ BYTES,
   LDPBADUNIT LABEL@ LBL, DPBAD-UNIT$ BYTES,
   LDICTFULL LABEL@ LBL, DICTFULL-MSG$ BYTES,          \ capacity arms append the token + newline
   LCODEFULL LABEL@ LBL, CODEFULL-MSG$ BYTES,
   LBGROWMSG LABEL@ LBL, BGROW-MSG$ BYTES,                               \ EM-BODY-GROW-DIE appends the capacity tried and the unit; LCOMPILEDIE the place and newline
   LBGROWUNIT LABEL@ LBL, BGROW-UNIT$ BYTES,
   RELOC-EMIT:LCALLMSG LABEL@ LBL, RELOC-EMIT:CALLMSG$ BYTES,
   RELOC-EMIT:EMIT-FULL-MESSAGE
   RELOC-EMIT:LADDRMSG LABEL@ LBL, RELOC-EMIT:ADDRMSG$ BYTES,
   RELOC-EMIT:LXTBANDMSG LABEL@ LBL, RELOC-EMIT:XTBANDMSG$ BYTES,
   RELOC-EMIT:LXTKINDMSG LABEL@ LBL, RELOC-EMIT:XTKINDMSG$ BYTES,
   LBADSTR  LABEL@ LBL, BADSTR-MSG$ BYTES,             \ no newline: the LCOMPILEDIE tail appends the location + newline
   LSIGBAD  LABEL@ LBL, SIGBAD-MSG$ BYTES,
   LSEALED  LABEL@ LBL, SEALED-MSG$ BYTES,
   LATMSG   LABEL@ LBL, ATMSG$ BYTES,
   LMMAPCODE LABEL@ LBL, MMAPCODE-MSG$ BYTES,
   LMMAPDATA LABEL@ LBL, MMAPDATA-MSG$ BYTES,
   LBLRANGE LABEL@ LBL, BLRANGE-MSG$ BYTES,
   LADDSUBBIG LABEL@ LBL, ADDSUBBIG-MSG$ BYTES, ;

\ Count a call in FRAME-CELL, through scratch registers a and b: bit 0 says the
\ body holds a call, bit 1 that it holds a second. That count, saturated at two,
\ is all the close needs: with one call, a call left as the body's last word is
\ that call, and turning it into a tail branch leaves no call at all. A cell opened as already having called (the does> clause)
\ counts its first real call as the second.
: FRAME-CALL, ( n n -- ) {: a:n b:n :}
   a DATA FRAME-CELL LDR,  b a 1 ANDI,  b b 1 LSLI,  a a b ORR,  a a 1 ORRI,
   a DATA FRAME-CELL STR, ;

\ LADDSUBIMM ( x8 = add/sub-immediate word with Rd and Rn set and imm12 = 0,
\ x7 = unsigned byte operand ): emit it with the operand folded in, through
\ LCEMIT; every variable-width stack transfer offset of the pass-2 emitters.
\ Below 4096 it is one word (test/type-layout-lower-pending.f pins that shape);
\ below 2^24 two (the #12-shifted half against the source, then the low half
\ against the destination). Folding the operand's bit 12 into the sh field is
\ the failure this prevents (a 513-cell bundle as `sub x14,x19,#32768`). An
\ operand at or above 2^24 cannot come from a checker-certified width and is
\ refused by name. Clobbers x7, x8, x9 and x30 only, so a caller may hold a
\ second operand live across the call.
: EMIT-ADDSUB-IMM ( -- )
   LBL LBL LBL {: ok:label low:label done:label :}
   LADDSUBIMM LABEL@ LBL,
   SP SP 32 SUBI,  30 SP 0 STR,  7 SP 8 STR,  8 SP 16 STR,
   9 7 24 LSRI,  9 ok CBZ,
      1 LADDSUBBIG LABEL@ ADR,  0 2 MOVZ,  2 ADDSUBBIG-MSG-LEN MOVZ,  NR-WRITE SYS,
      0 75 MOVZ,  NR-EXIT-GROUP SYS,
   ok LBL,
   9 7 12 LSRI,  9 low CBZ,                           \ x9 = operand >> 12
      7 $400000 LIT64,  8 8 7 ORR,                    \ sh = 1: this half scales by 4096
      9 9 10 LSLI,  9 9 8 ORR,  LCEMIT LABEL@ BL,
      7 SP 8 LDR,  9 7 $FFF ANDI,  9 done CBZ,        \ no low half: the shifted word carried it all
      8 SP 16 LDR,  9 8 $1F ANDI,  9 9 5 LSLI,        \ the low half reads back the destination
      7 $FFFFFFFFFFFFFC1F LIT64,  8 8 7 AND,  8 8 9 ORR,
      7 SP 8 LDR,  7 7 $FFF ANDI,  7 7 10 LSLI,  9 8 7 ORR,
      LCEMIT LABEL@ BL,
      done B,
   low LBL,
      9 7 10 LSLI,  9 9 8 ORR,  LCEMIT LABEL@ BL,     \ the single folded word
   done LBL,
   30 SP 0 LDR,  SP SP 32 ADDI,  RET, ;

: C-TARGET-UNKNOWN ( -- )
   s" hb: unknown target" 76 die ;

private

\ (GENIO-OUT): the device half of src/habu/rt.f's output funnel, reached when
\ the task's output device index is non-zero (x0 = index, x1 = span address,
\ x2 = length). A sealed engine helper for the reason (PROT-SPAN) is: prim bodies
\ reach it by direct BL (src/habu/rt.f G-OUT).
\ ACTIVE-CELL holds the device during the call, so a write shared by several
\ devices finds its own; the caller's value is restored from the frame.
\ It falls back to write(1) when the re-entrancy guard is set, the index is
\ outside 1..DEVICES or the row is empty: output keeps working while a device is
\ built or gone. A device reports a failure by latching it for its next checked
\ operation (src/lib/genio-core.f); a throw out of `emit` would unwind through
\ engine internals.
: EMIT-GENIO-OUT ( -- )
   LBL LBL LBL {: term:label done:label end:label :}
   LGENIOOUT LABEL@ {: start:label :}
   s" (GENIO-OUT)" start LABEL>N end LABEL>N ENGINE-PRIMS:HELPER-REGISTER
   start LBL,
   SP SP $10 SUBI,  30 SP 0 STR,
   9 DATA GENIO-ABI:BUSY-CELL LDR,  9 term CBNZ,
   0 GENIO-ABI:DEVICES CMPI,  C-HI term BCOND,          \ index outside 1..DEVICES
   9 GENIO-ABI:WRITE-OFF LIT64,  9 DATA 9 ADD,
   10 0 0 ADDI,  10 10 1 SUBI,  10 10 3 LSLI,  9 9 10 ADD,
   9 9 0 LDR,  9 term CBZ,                              \ row never registered
   10 DATA GENIO-ABI:ACTIVE-CELL LDR,  10 SP 8 STR,     \ save the caller's active device
   0 DATA GENIO-ABI:ACTIVE-CELL STR,                    \ the write runs for this one
   10 1 MOVZ,  10 DATA GENIO-ABI:BUSY-CELL STR,
   1 G-PUSH  2 G-PUSH
   9 BLR,
   10 0 MOVZ,  10 DATA GENIO-ABI:BUSY-CELL STR,
   10 SP 8 LDR,  10 DATA GENIO-ABI:ACTIVE-CELL STR,
   done B,
   term LBL,
   0 1 MOVZ,  NR-WRITE SYS,
   done LBL,
   30 SP 0 LDR,  SP SP $10 ADDI,
   RET,
   end LBL, ;

;package

package ENGINE-EMIT

public
\ Release an exact byte count from the machine-stack locals frame in one add
\ (imm12) naming no scratch: x9..x15 (regalloc.f VRPACK) may hold live values.
: C-EMIT-DROP-X12 ( -- )
   LBL {: done:label :}
   12 done CBZ,
      8 $910003FF LIT64,  7 12 0 ADDI,  LADDSUBIMM LABEL@ BL,   \ add sp,sp,#bytes
   done LBL, ;

private
: EMIT-CF-HELPERS ( -- )
   LBL LBL LBL LBL LBL LBL LBL {: pisb pdone kno kyes kchk knf kok :}
   LCFPUSH LABEL@ LBL,
      5 CFSTK-OFF LIT64,  10 DBASE 5 ADD,
      5 DATA REGION-ALIAS:DELTA-CELL LDR,  10 10 5 ADD,    \ x10 = the stack through the alias
      11 10 0 LDR,                                         \ x11 = control-flow depth
      12 CFSTK-DEPTH-MAX MOVZ,  11 12 CMP,                 \ depth vs region cap (x12 = internal scratch, reloaded below; callers like J-ELSE preserve x14 across this call)
      C-GE LCFCAP LABEL@ BCOND,                            \ at the cap: fail-closed reject BEFORE the write (never overflow into the code area above CFSTK)
      12 0 MOVZ,  12 DATA LIDX-CELL STR,                   \ a join may be reached from code that wrote x29
      12 CF-REC MOVZ,  12 11 12 MUL,  12 12 10 ADD,  12 12 8 ADDI,
      9 12 0 STR,
      13 DATA LOCN-CELL LDR,  13 12 CF-LOCN STR,
      13 DATA LOCF-CELL LDR,  13 12 CF-LOCF STR,
      15 12 CF-KIND STR,                                   \ x15 = the entry's kind (C-CF-PUSH)
      11 11 1 ADDI,  11 10 0 STR,  RET,
   LCFPOP LABEL@ LBL,
      5 CFSTK-OFF LIT64,  10 DBASE 5 ADD,  11 10 0 LDR,      \ x11 = control-flow depth (peek before pop)
      11 LORPHAN LABEL@ CBZ,                                 \ empty stack: orphan closer, fail-closed reject (never underflow into a bogus branch origin)
      12 0 MOVZ,  12 DATA LIDX-CELL STR,                     \ a join may be reached from code that wrote x29
      SP SP 16 SUBI,  30 SP 0 STR,
      5 DATA REGION-ALIAS:DELTA-CELL LDR,  10 10 5 ADD,      \ x10 = the stack through the alias
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
      \ A patch writes BELOW CP: the branch it fills was emitted earlier in this
      \ body. The target is recorded in FWDTGT-CELL: a closer that emits
      \ nothing after its patch leaves it at CP.
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
      pdone LBL,  11 11 10 ORR,
      5 DATA REGION-ALIAS:DELTA-CELL LDR,  5 9 5 ADD,  11 5 0 STRW,   \ through the alias
      10 CP DBASE SUB,  10 DATA FWDTGT-CELL STR,  RET,
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
      \ The token IS this keyword. Under the design seal (layout.f
      \ POLICY-NDICT-CELL) only a spelling baked in KWDATA's design span
      \ [LKWIF, LKWDESIGNEND) is in the vocabulary, and x0 still holds the
      \ spelling the row passed. Both ends are tested because the op rows bake
      \ theirs outside KWDATA (jit.f), on whichever side the emitter put them.
      \ Every row matches through here, so every row inherits the refusal.
      kyes LBL,
         2 DATA POLICY-NDICT-CELL LDR,  2 kok CBZ,
         2 LKWIF LABEL@ ADR,  0 2 CMP,  C-CC LPOLICY LABEL@ BCOND,
         2 LKWDESIGNEND LABEL@ ADR,  0 2 CMP,  C-CS LPOLICY LABEL@ BCOND,
      kok  LBL,  0 1 MOVZ,  RET,
      kno  LBL,  0 0 MOVZ,  RET,
   LBCHAIN LABEL@ LBL,                                    \ patch a B-placeholder chain:
      LBL LBL {: bcl bcd :}                    \ x9=head offset, x14=target;
      bcl LBL,  9 bcd CBZ,                         \ clobbers x5, x9-x12
         10 DBASE 9 ADD,  11 10 0 LDRW,
         12 14 10 SUB,  12 12 2 ASRI,
         5 $3FFFFFF LIT64,  12 12 5 AND,
         5 $14000000 LIT64,  12 12 5 ORR,
         5 DATA REGION-ALIAS:DELTA-CELL LDR,  5 10 5 ADD,  12 5 0 STRW,   \ through the alias
         9 11 0 ADDI,  bcl B,
      bcd LBL,  RET, ;

public
\ keyword bytes (lower-case / literal) at known labels
create SQ-KW  115 c, 34 c,
create CQ-KW  99 c, 34 c,
create DOTQ-KW 46 c, 34 c,
create ESQ-KW  115 c, 92 c, 34 c,
create ECQ-KW  99 c, 92 c, 34 c,
create EDOTQ-KW 46 c, 92 c, 34 c,
create BCHAR-KW 91 c, 99 c, 104 c, 97 c, 114 c, 93 c,   \ [char]
here BCHAR-KW - constant BCHAR-LEN
create QUOT-KW 91 c, 58 c,      \ [:
create SEMIQ-KW 59 c, 93 c,     \ ;]
variable LRREC
create TICK-KW   39 c,
here TICK-KW - constant TICK-LEN
create BTICK-KW  91 c, 39 c, 93 c,
here BTICK-KW - constant BTICK-LEN
create LBRACE-KW 123 c, 58 c,
create ENDLOC-KW 58 c, 125 c,
\ DEFER-MAGIC is src/habu/layout.f's: this file writes and checks the trailer,
\ and src/compiler/native/dict.f checks it too.
variable LKWDEFER  variable LKWIS  variable LKWDEFERUNSET
;package
\ The defer/is misuse diagnostics. The label cells and messages are public:
\ KWDATA:EMIT bakes the strings and the LABELS allocator mints the ids. Their
\ emitters reopen this package beside the defer rows.
package DEFER-DIAG
public
variable LDEFNOTOKEN  variable LDEFNOTFOUND  variable LDEFNOTDEFER  variable LDEFNONAME
variable LDEFHINT
: NOTOKEN$ ( -- ptr u8 n )    s" hb: is: missing target word after " ;
: NOTFOUND$ ( -- ptr u8 n )   s" hb: is: no deferred word named " ;
: NOTDEFER$ ( -- ptr u8 n )   s" hb: is: not a deferred word: " ;
: NONAME$ ( -- ptr u8 n )     s" hb: defer: missing name after " ;
\ No newline: the LCOMPILEDIE tail appends the location and it.
: HINT$ ( -- ptr u8 n )   s" hb: is: parsing words resolve outside using-imports; qualify the target" ;
;package
package ENGINE-EMIT
public
variable LSIGA  variable LP2CLAUSE
variable LNSTRINTERN
;package

\ The native compiler seam's labels hold its qualified spelling, its
\ boot-integrity diagnostic, and the capture-only compiler entry.
package NCOMP-EMIT
public
variable LUNSET
;package
package ENGINE-EMIT
public
\ ADT lowering keywords: `construct` and MATCH dispatch through the CMM-CELL
\ mode machine.
variable LKWCONSTRUCT  variable LKWMATCH  variable LKWSEMIMATCH
variable LRESTAB    \ sealed system-package name table

\ Keyword spellings a diagnostic writes: the row below that bakes each one and
\ every write of it take the bytes and the length from the same literal.
: KWCREATE$ ( -- ptr u8 n )     s" create" ;
: KWCONST$ ( -- ptr u8 n )      s" constant" ;
: KWCHAR$ ( -- ptr u8 n )       s" char" ;
: KWDOES$ ( -- ptr u8 n )       s" does>" ;
: KWTRUSTED$ ( -- ptr u8 n )    s" trusted:" ;
: KWDUPDEF$ ( -- ptr u8 n )     s" duplicate definition: " ;
private
: RECWPUB$ ( -- ptr u8 n )      s" rec-wide-publish" ;
: RECMIQ$ ( -- ptr u8 n )       s" rec-min-in@" ;
public
: CHKEXPORT$ ( -- ptr u8 n )    s" checker-export" ;
;package

\ Keyword string data section: the baked name table every LKW*/LCHK* label
\ points into, plus the sealed reserved-name table it embeds.
package KWDATA

\ Sealed system-package names. Records are [u8 len][len bytes] in
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
   0 c,
here RESTAB-BUF - constant RESTAB-LEN

public

\ Label cell for the baked `trust-raw` string: the keyword row below bakes it,
\ LASTC-TRUST:FIND-RAW names it in diagnostics, LABELS mints its id.
variable LKWTRUSTRAW
: TRUSTRAW$ ( -- ptr u8 n )   s" trust-raw" ;

\ The same for `trust-decl` (DEF-TRUST:FIND names it in diagnostics).
variable LKWTRUSTDECL
: TRUSTDECL$ ( -- ptr u8 n )   s" trust-decl" ;

\ The `cast:` declarer's three baked strings: the keyword the interpret dispatch
\ matches, the checker registrar's diagnostic spelling, and the one error before
\ the signature parser takes over.
variable LKWCAST
variable LCASTNONAME
variable LKEYNONAME
\ The `linear:` declarer's keyword. A missing name takes the shared reader
\ keyword diagnostic, so its one string is the keyword itself.
variable LKWLINEAR
: CASTNONAME$ ( -- ptr u8 n )    s" hb: cast: missing name after " ;
: KEYNONAME$ ( -- ptr u8 n )     s" hb: reader keyword needs a name: " ;
: CASTNONAME-LEN ( -- n )    CASTNONAME$ nip ;
: KEYNONAME-LEN ( -- n )     KEYNONAME$ nip ;

private

\ The design span: the fourteen keyword spellings a sealed source may still use
\ (layout.f POLICY-NDICT-CELL). It is baked first and as one run, LKWIF opening
\ it and LKWDESIGNEND closing it, because EMIT-CF-HELPERS' LKWCMP admits a
\ matched keyword under the seal by ADDRESS: its baked spelling lies in
\ [LKWIF, LKWDESIGNEND) or the token is refused. A spelling baked in this word
\ is therefore in every sealed design's vocabulary, and one baked anywhere else
\ is not.
: DESIGN-SPAN ( -- )
   LKWIF LABEL@ LBL,     s" if"     BYTES,    LKWELSE LABEL@ LBL,   s" else"   BYTES,
   LKWTHEN LABEL@ LBL,   s" then"   BYTES,
   LKWLBRACE LABEL@ LBL, LBRACE-KW 2 BYTES,  LKWENDLOC LABEL@ LBL, ENDLOC-KW 2 BYTES,
   LKWSQ LABEL@ LBL,     SQ-KW 2 BYTES,
   LKWESQ LABEL@ LBL,    ESQ-KW 3 BYTES,
   LKWPACKAGE LABEL@ LBL, s" package" BYTES,  LKWPUBLIC LABEL@ LBL, s" public" BYTES,
   LKWPRIVATE LABEL@ LBL, s" private" BYTES,  LKWSEMIPACKAGE LABEL@ LBL, s" ;package" BYTES,
   LKWUSING LABEL@ LBL, s" using" BYTES,  LKWSEMIUSING LABEL@ LBL, s" ;using" BYTES,
   LKWBPACKAGE LABEL@ LBL, s" [package]" BYTES,
   LKWDESIGNEND LABEL@ LBL, ;

public

: EMIT ( -- )
   DESIGN-SPAN
   LKWSEALPACKAGE LABEL@ LBL, s" seal-package" BYTES,
   LKWBEGIN LABEL@ LBL,  s" begin"  BYTES,
   LKWUNTIL LABEL@ LBL,  s" until"  BYTES,    LKWAGAIN LABEL@ LBL,  s" again"  BYTES,
   LKWWHILE LABEL@ LBL,  s" while"  BYTES,    LKWREPEAT LABEL@ LBL, s" repeat" BYTES,
   LKWCASE LABEL@ LBL,   s" case"   BYTES,    LKWOF LABEL@ LBL,     s" of"     BYTES,
   LKWENDOF LABEL@ LBL,  s" endof"  BYTES,    LKWENDCASE LABEL@ LBL, s" endcase" BYTES,
   LKWCREATE LABEL@ LBL, KWCREATE$ BYTES,    LKWVAR LABEL@ LBL,    s" variable" BYTES,
   LKWCQ LABEL@ LBL,     CQ-KW 2 BYTES,
   LKWDOTQ LABEL@ LBL,   DOTQ-KW 2 BYTES,
   LKWECQ LABEL@ LBL,    ECQ-KW 3 BYTES,
   LKWEDOTQ LABEL@ LBL,  EDOTQ-KW 3 BYTES,
   LKWTYPE LABEL@ LBL,   s" type" BYTES,
   LKWTICK LABEL@ LBL,   TICK-KW TICK-LEN BYTES,    LKWBTICK LABEL@ LBL,  BTICK-KW BTICK-LEN BYTES,
   LKWCONST LABEL@ LBL,  KWCONST$ BYTES,
   LKWDO LABEL@ LBL,  s" do" BYTES,    LKWLOOP LABEL@ LBL,  s" loop" BYTES,    LKWI LABEL@ LBL,  s" i" BYTES,
   LKWTOR LABEL@ LBL,  s" >r" BYTES,   LKWRFROM LABEL@ LBL,  s" r>" BYTES,   LKWRFET LABEL@ LBL,  s" r@" BYTES,
   LKWEXIT LABEL@ LBL,  s" exit" BYTES,   LKWREC LABEL@ LBL,  s" recurse" BYTES,
   LKWQDO LABEL@ LBL,  s" ?do" BYTES,   LKWPLOOP LABEL@ LBL,  s" +loop" BYTES,   LKWJ LABEL@ LBL,  s" j" BYTES,
   LKWLEAVE LABEL@ LBL,  s" leave" BYTES,   LKWUNLOOP LABEL@ LBL,  s" unloop" BYTES,
   LKWCHAR LABEL@ LBL,  KWCHAR$ BYTES,   LKWBCHAR LABEL@ LBL,  BCHAR-KW BCHAR-LEN BYTES,
   LKWIMM LABEL@ LBL,  s" immediate" BYTES,
   LKWDOES LABEL@ LBL,  KWDOES$ BYTES,
   LKWTRUSTED LABEL@ LBL, KWTRUSTED$ BYTES,
   LKWCAST LABEL@ LBL, s" cast:" BYTES,
   LKWLINEAR LABEL@ LBL, s" linear:" BYTES,
   LCASTNONAME LABEL@ LBL, CASTNONAME$ BYTES,
   LKEYNONAME LABEL@ LBL, KEYNONAME$ BYTES,
   LKWKERNEL LABEL@ LBL, s" kernel:" BYTES,
   LKWTRUSTDECL LABEL@ LBL, TRUSTDECL$ BYTES,      LKWTRUSTRAW LABEL@ LBL, TRUSTRAW$ BYTES,
   LKWDUPDEF LABEL@ LBL, KWDUPDEF$ BYTES,  LKWQUOT LABEL@ LBL,  QUOT-KW 2 BYTES,   LKWSEMIQ LABEL@ LBL,  SEMIQ-KW 2 BYTES,  LKWDEFER LABEL@ LBL, s" defer" BYTES,  LKWIS LABEL@ LBL, s" is" BYTES,  LKWDEFERUNSET LABEL@ LBL, s" defer-unset" BYTES,  DEFER-DIAG:LDEFNOTOKEN LABEL@ LBL, DEFER-DIAG:NOTOKEN$ BYTES,  DEFER-DIAG:LDEFNOTFOUND LABEL@ LBL, DEFER-DIAG:NOTFOUND$ BYTES,  DEFER-DIAG:LDEFNOTDEFER LABEL@ LBL, DEFER-DIAG:NOTDEFER$ BYTES,  DEFER-DIAG:LDEFHINT LABEL@ LBL, DEFER-DIAG:HINT$ BYTES,  DEFER-DIAG:LDEFNONAME LABEL@ LBL, DEFER-DIAG:NONAME$ BYTES,            LRESTAB LABEL@ LBL, RESTAB-BUF RESTAB-LEN BYTES,    LSIGA LABEL@ LBL, s" -- a" BYTES,    NCOMP-EMIT:LUNSET LABEL@ LBL, UNSET-MSG$ BYTES,
   LKWEXPORT LABEL@ LBL, s" export" BYTES,  LCHKEXPORT LABEL@ LBL, CHKEXPORT$ BYTES,
   LKWCONSTRUCT LABEL@ LBL, s" construct" BYTES,  LKWMATCH LABEL@ LBL, s" match" BYTES,  LKWSEMIMATCH LABEL@ LBL, s" ;match" BYTES, ;

;package

package ENGINE-EMIT
\ Compile-time keyword handlers append JIT-emitter code at BUILD time.
: C-POPFLAG ( -- )
   W-POP9 C-EMITW ;

: C-POP-X16 ( -- )
   W-POP16 C-EMITW ;

: C-CF-PUSH ( n -- )
   {: kind:n :}
   15 kind MOVZ,  LCFPUSH LABEL@ BL, ;   \ x9 = the entry's origin

public
: C-PUSHCP ( n -- )   9 CP 0 ADDI,  C-CF-PUSH ;

private
\ A refusal caught inside an immediate word's evaluate leaves the outer
\ definition compiling, restored to the refused token's start mark (layout.f
\ TOKBODY-CELL): body text, locals count, CP, virtual-stack depth and register
\ masks. So a control word may spill or emit code before it refuses, but refuses
\ before it pushes or pops the control-flow stack, opens a DO level, takes or
\ reconciles to a snapshot frame, shifts CMBK or patches code below CP: none of
\ those is in the mark. The refusal depends only on the control-flow depth and
\ entry kinds, known before the word touches anything.
\ C-CF-ROOM refuses an opener at the depth cap (LCFPUSH's check, made first).
\ C-CF-AT refuses a word whose entry pos below the top is missing (LORPHAN) or
\ of another kind (LCFKIND: a depth check alone let `if until` pop the IF entry
\ as a loop top); C-CF-NONE refuses a word that ends a body while an entry is
\ open. All clobber x5 and x10-x12, as LCFPUSH does.
: C-CF-DEPTH ( -- )
   5 CFSTK-OFF LIT64,  10 DBASE 5 ADD,  11 10 0 LDR, ;

public
: C-CF-ROOM ( -- )
   LBL {: ok:label :}
   C-CF-DEPTH
   12 CFSTK-DEPTH-MAX MOVZ,  11 12 CMP,  C-LT ok BCOND,
      LCFCAP LABEL@ B,
   ok LBL, ;

private
\ x12 = the kind of entry x11 (an index from the bottom).
: C-CF-KIND@ ( -- )
   12 CF-REC MOVZ,  12 11 12 MUL,  12 12 10 ADD,  12 12 CF-KIND 8 + LDR, ;

: C-CF-AT2 ( n n n -- )
   {: pos:n k1:n k2:n :}
   LBL LBL {: have:label ok:label :}
   C-CF-DEPTH
   11 pos 1 + CMPI,  C-GE have BCOND,
      LORPHAN LABEL@ B,
   have LBL,
   11 11 pos 1 + SUBI,  C-CF-KIND@
   12 k1 CMPI,  C-EQ ok BCOND,
   k2 k1 <> if  12 k2 CMPI,  C-EQ ok BCOND,  then
      LCFKIND LABEL@ B,
   ok LBL, ;

public
: C-CF-AT ( n n -- )  dup C-CF-AT2 ;

private
: C-CF-NONE ( -- )
   LBL {: ok:label :}
   C-CF-DEPTH  11 ok CBZ,
      LCFKIND LABEL@ B,
   ok LBL, ;

: C-BBACK ( n n -- ) {: opc mask :}
   10 9 CP SUB,  10 10 2 ASRI,  5 mask LIT64,  10 10 5 AND,  9 opc LIT64,  9 9 10 ORR,  LCEMIT LABEL@ BL, ;

: J-IF ( -- )    C-POPFLAG  CFK-IF C-PUSHCP  $B4000009 C-EMITW ;

\ `then` reconciles the fall-through arm to the frame its `if` or `else`
\ recorded, then patches the pending branch past that code: the branch arrives
\ in the frame's layout already.
: J-THEN ( -- )  0 CFK-IF C-CF-AT  LVRECON LABEL@ BL,  LCFPOP LABEL@ BL,  LPAT LABEL@ BL, ;

\ Pop the top entry, open a kind entry at a forward B and patch the popped
\ branch past that B: the shared tail of else and endof.
: C-ELSE ( n -- )
   {: kind:n :}
   LCFPOP LABEL@ BL,
   14 9 0 ADDI,
   kind C-PUSHCP
   $14000000 C-EMITW
   9 14 0 ADDI,
   LPAT LABEL@ BL, ;

\ `else` records the true arm's end as the join frame and starts the false arm
\ in the frame `if` recorded, the layout its branch arrives with (LVRESNAP).
: J-ELSE ( -- )  0 CFK-IF C-CF-AT  LVRESNAP LABEL@ BL,  CFK-IF C-ELSE ;

: J-CASE ( -- )
   9 0 MOVZ,  CFK-CASE C-CF-PUSH ;

\ construct: arm the operand capture (CMM=1). No spill: construct only adds VS
\ constants on top, like a generated constructor's literal tokens.
: J-CONSTRUCT ( -- )
   12 1 MOVZ,  12 DATA CMM-CELL STR, ;

\ match: CF-ENTRY spills the width-expanded scrutinee bundle first; J-MATCH
\ pushes a CF sentinel (x9=0, the J-CASE shape ;MATCH's join loop stops at),
\ opens a match-frame level and arms CMM=3 (want family). The tag is peeked once
\ (ldur x9,[x19,#-8]): no token emits code before the first arm's compare and
\ the mismatch-skip chain never touches x19 or x9, so every arm's cmp reuses it.
: J-MATCH ( -- )
   9 0 MOVZ,  CFK-MATCH C-CF-PUSH
   9 DATA CMFRD-CELL LDR,  9 9 1 ADDI,  9 DATA CMFRD-CELL STR,
   12 3 MOVZ,  12 DATA CMM-CELL STR,
   $F85F8269 C-EMITW ;                  \ ldur x9,[x19,#-8]: single per-dispatch tag peek

\ J-OF pushes a CASE-arm bit (0) on the CMBK bitstack so the shared J-ENDOF
\ tells a CASE arm from a MATCH variant branch (1). The arm
\ compares the peeked scrutinee and skips on b.ne: C-PUSHCP emits nothing, so
\ the cmp's flags reach the b.ne untouched, and LPAT patches it through its
\ imm19 branch class.
: J-OF ( -- )
   0 CFK-CASE CFK-ENDOF C-CF-AT2         \ an arm continues its case
   C-CF-ROOM
   14 DATA CMBK-CELL LDR,  14 14 1 LSLI,  14 DATA CMBK-CELL STR,
   C-POP-X16
   $F85F8269 C-EMITW                     \ ldur x9,[x19,#-8]  peek scrutinee
   $EB10013F C-EMITW                     \ cmp x9,x16
   CFK-OF C-PUSHCP
   $54000001 C-EMITW                     \ b.ne +0  (skip to next arm on mismatch)
   $D1002273 C-EMITW ;                   \ sub x19,x19,#8  drop scrutinee on match

\ J-ENDOF: the J-ELSE placeholder for CASE and MATCH, then it pops the CMBK bit:
\ a MATCH variant branch (1) re-arms CMM=4 (want variant or ;match), a CASE arm
\ (0) leaves CMM alone. Mirrors the checker's CF-ENDOF-DISPATCH; both arms
\ leave a CFK-OF entry.
: J-ENDOF ( -- )
   LBL {: nm:label :}
   0 CFK-OF C-CF-AT  CFK-ENDOF C-ELSE
   14 DATA CMBK-CELL LDR,  15 14 1 ANDI,  14 14 1 LSRI,  14 DATA CMBK-CELL STR,
   15 nm CBZ,  12 4 MOVZ,  12 DATA CMM-CELL STR,  nm LBL, ;

: J-ENDCASE ( -- )
   LBL LBL LBL LBL LBL {: cloop:label done:label scan:label more:label found:label :}
   \ The loop below pops and patches down to the case marker (J-CASE's 0), so
   \ the entries above it must be its arms' ENDOFs and the marker a case's,
   \ proved before it pops anything: x11 walks the depth down.
   C-CF-DEPTH
   scan LBL,
      11 more CBNZ,
         LORPHAN LABEL@ B,
      more LBL,
      11 11 1 SUBI,  C-CF-KIND@
      12 CFK-ENDOF CMPI,  C-EQ scan BCOND,
      12 CFK-CASE CMPI,  C-EQ found BCOND,
         LCFKIND LABEL@ B,
   found LBL,
   $D1002273 C-EMITW
   cloop LBL,
      LCFPOP LABEL@ BL,
      9 done CBZ,
      LPAT LABEL@ BL,
      cloop B,
   done LBL, ;

\ A back edge branches on x17, never a VS register.
: J-BEGIN ( -- )  C-CF-ROOM  LVSNAP LABEL@ BL,  CFK-BEGIN C-PUSHCP ;

: J-AGAIN ( -- )  0 CFK-BEGIN C-CF-AT  LVRECON LABEL@ BL,  LCFPOP LABEL@ BL,  $14000000 $3FFFFFF C-BBACK ;

\ Shared tail of `until`: reconcile, then the back branch whose word (imm19 0)
\ the caller put in x16. LCFPOP writes x16, so the word is framed across it.
: J-UNTILX ( -- )
   SP SP 16 SUBI,  16 SP 0 STR,
   LVRECON LABEL@ BL,
   LCFPOP LABEL@ BL,
   16 SP 0 LDR,  SP SP 16 ADDI,
   10 9 CP SUB,  10 10 2 ASRI,  5 $7FFFF LIT64,  10 10 5 AND,  10 10 5 LSLI,
   9 16 10 ORR,  LCEMIT LABEL@ BL, ;

: J-UNTIL ( -- )                                      \ pop flag -> x17, cbz x17
   W-POP17 C-EMITW  16 $B4000011 LIT64,  J-UNTILX ;

\ The refusals `until` and `while` make before either path (CFB-ENTRY).
: C-UNTIL-CHECK ( -- )  0 CFK-BEGIN C-CF-AT ;

: C-WHILE-CHECK ( -- )  0 CFK-BEGIN C-CF-AT  C-CF-ROOM ;

: J-WHILE ( -- ) C-POPFLAG  CFK-WHILE C-PUSHCP  $B4000009 C-EMITW ;

\ `repeat` pops the WHILE frame above the BEGIN frame, reconciles the back
\ edge to the BEGIN frame and starts the exit path in the WHILE frame, the
\ layout `while`'s branch arrives with. Machine frame: 0 the WHILE origin,
\ 8 its LOCF, 16 k, 24 p0, 32 p1 of the WHILE frame.
: J-REPEAT ( -- ) 0 CFK-WHILE C-CF-AT  1 CFK-BEGIN C-CF-AT
   SP SP 48 SUBI,
   LVFPOP LABEL@ BL,  13 SP 16 STR,  12 SP 24 STR,  14 SP 32 STR,
   LVRECON LABEL@ BL,
   LCFPOP LABEL@ BL,  9 SP 0 STR,  14 SP 8 STR,
   LCFPOP LABEL@ BL,  $14000000 $3FFFFFF C-BBACK
   13 SP 16 LDR,  12 SP 24 LDR,  14 SP 32 LDR,  LVSETF LABEL@ BL,
   9 SP 0 LDR,  LPAT LABEL@ BL,
   14 SP 8 LDR,  15 DATA LOCF-CELL LDR,  12 14 15 SUB,  C-EMIT-DROP-X12
   SP SP 48 ADDI, ;

\ Instruction words the JIT stamps into generated code. The loop and return
\ stacks are guarded mappings (STACK-ABI), so their bases are runtime values
\ loaded from the DATA header; stamping through these encoders keeps a
\ cell-offset change from silently re-encoding. Neither stack lives in a
\ register: x25/x28 belong to the compiler, and word frames on the machine stack
\ would unbalance the epilogue.
: W-LDRX ( n n n -- n ) {: rt RN off :}                               \ ldr rt,[rn,#off]
   $F9400000  off 8 / 10 lshift or  RN 5 lshift or  rt or ;

: W-STRX ( n n n -- n ) {: rt RN off :}                               \ str rt,[rn,#off]
   $F9000000  off 8 / 10 lshift or  RN 5 lshift or  rt or ;

: W-ADDX ( n n n -- n ) {: rd RN RM :}                                \ add rd,rn,rm
   $8B000000  RM 16 lshift or  RN 5 lshift or  rd or ;

: W-ADDX-LSL3 ( n n n -- n ) {: rd RN RM :}                           \ add rd,rn,rm,lsl#3
   $8B000000  RM 16 lshift or  3 10 lshift or  RN 5 lshift or  rd or ;

\ LOOP-FRAME-ADDR, : x12 += the loop frame band's base, x13 scratch: the frame
\ address from x12 holding its byte offset. The base is a header cell, so this
\ loads it. `j` emits it after the dispatch spill, where x11-x13 hold no value.
: LOOP-FRAME-ADDR, ( -- )
   13 20 STACK-ABI:LOOP-BASE-CELL W-LDRX C-EMITW
   12 12 13 W-ADDX C-EMITW ;

public
\ The innermost loop's index lives in its frame cell, which J-FRAME, J-LOOP and
\ J-+LOOP store on every step, and in x29 (regalloc.f LIDX), which they write
\ beside it. `j`, throw and catch, tasks, the crash reporter and the stepper
\ read the frame, so memory stays the index's home and x29 is a cache.
\ LIDX-CELL says at compile time whether the next code may read x29: `do` and
\ `?do` set it, and so does `i` when it reloads; a call (RECURSE included), a
\ control-flow push or pop (LCFPUSH, LCFPOP: a join or the code after a loop
\ may be reached from code that wrote x29), `unloop`, a definition start and
\ a refused token's rollback clear it. While it is clear, `i`, `loop` and
\ `+loop` load x29 from the frame first.
\ The loop rows below emit code while the virtual stack keeps values in pool
\ registers, so their scratch is x0, x1, x16 and x17: x0 and x1 are free scratch
\ in emitted row code because no pool value lives in them and a call clobbers
\ them; x16 and x17 are never pooled.
\ J-FRAME : push the loop frame (index x29, limit x17); x29 is its index cache.
: J-FRAME ( -- )
   16 20 LOOPSP-CELL W-LDRX C-EMITW
   0 16 4 ENC-LSLI C-EMITW
   1 20 STACK-ABI:LOOP-BASE-CELL W-LDRX C-EMITW
   0 0 1 W-ADDX C-EMITW
   LIDX 0 0 W-STRX C-EMITW
   17 0 8 W-STRX C-EMITW
   16 16 1 ENC-ADDI C-EMITW
   16 20 LOOPSP-CELL W-STRX C-EMITW ;

\ LIDX-SET, : the code emitted next may read the index from x29. Clobbers x9.
: LIDX-SET, ( -- )  9 1 MOVZ,  9 DATA LIDX-CELL STR, ;

\ LIDX-CLEAR, : the code emitted next reloads the index first. Clobbers x9.
: LIDX-CLEAR, ( -- )  9 0 MOVZ,  9 DATA LIDX-CELL STR, ;

\ C-VS-TOP>REG, : move the VS top into register rd, outside the pools, and drop
\ it: a register entry by a mov, an integer constant by its movz chain, a float
: C-VS-TOP>REG, ( n -- )
   {: rd:n :}
   LBL LBL LBL {: con:label mem:label done:label :}
   6 DATA VSP-CELL LDR,  6 mem CBZ,
   5 6 1 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 LDR,     \ x11 = val
   7 1 CMPI,  C-EQ con BCOND,
   7 mem CBNZ,                                                    \ tags 2 and 3
      8 W-MOVRR rd or LIT64,  7 11 16 LSLI,  9 8 7 ORR,  LCEMIT LABEL@ BL,   \ mov rd,xR
      LVDROP LABEL@ BL,  done B,
   con LBL,
      14 rd MOVZ,  LVMOVK LABEL@ BL,                              \ movz/movk rd,val
      LVDROP LABEL@ BL,  done B,
   mem LBL,
      LVSPILL LABEL@ BL,
      W-POPR rd or C-EMITW                                        \ ldr rd,[x19,#-8]!
   done LBL, ;

\ C-LOOP-TOP, : x16 := the innermost loop frame's address, x17 scratch. x16 and
\ x17 are never pooled, so no virtual stack value moves.
: C-LOOP-TOP, ( -- )
   16 20 LOOPSP-CELL W-LDRX C-EMITW
   16 16 1 ENC-SUBI C-EMITW
   16 16 4 ENC-LSLI C-EMITW
   17 20 STACK-ABI:LOOP-BASE-CELL W-LDRX C-EMITW
   16 16 17 W-ADDX C-EMITW ;

\ C-LOOP-POP, : pop one loop frame through x16, the shared tail of `unloop`,
\ `loop` and `+loop`.
: C-LOOP-POP, ( -- )
   16 20 LOOPSP-CELL W-LDRX C-EMITW
   16 16 1 ENC-SUBI C-EMITW
   16 20 LOOPSP-CELL W-STRX C-EMITW ;
;package

\ The loop-emit family: the DO/LEAVE level stack (open a level, prove one is
\ open, chain a LEAVE placeholder onto the innermost one) and the loop keyword
\ handlers built on it. The loop keyword dispatch rows reopen this package
\ further down so they resolve the handlers bare.
package LOOP-EMIT

\ LVROOM refuses an opener while every level is in use, before `do` or `?do`
\ takes a snapshot frame or LVOPEN writes a level. Clobbers x9 and x12.
: LVROOM ( -- )
   9 DATA LVD-CELL LDR,
   12 LV-LEVELS MOVZ,  9 12 CMP,
   C-GE LCFCAP LABEL@ BCOND, ;           \ (TKA/TKL hold the `do`/`?do` token)

: LVOPEN ( -- )                                 \ open a LEAVE-chain level: LVH[LVD]=LVQ[LVD]=0, LVD++
   9 DATA LVD-CELL LDR,
   10 9 3 LSLI,  10 10 LVH-OFF ADDI,  10 DATA 10 ADD,
   12 0 MOVZ,  12 10 0 STR,
   10 9 3 LSLI,  10 10 LVQ-OFF ADDI,  10 DATA 10 ADD,
   12 10 0 STR,
   10 9 3 LSLI,  10 10 LVF-OFF ADDI,  10 DATA 10 ADD,
   12 DATA LOCF-CELL LDR,  12 10 0 STR,
   9 9 1 ADDI,  9 DATA LVD-CELL STR, ;

\ The DO/LEAVE level stack (LVD-CELL depth + the LVH/LVF/LVQ/LVS level arrays):
\ `do`/`?do` open a level in LVOPEN, `loop`/`+loop` close one in J-LOOPEND, and
\ `leave` chains onto the innermost one. LVQ holds a `?do`'s entry branch (0 for
\ `do`) for its `+loop` to rewrite; LVS the level's snapshot frame index, which
\ `leave` reconciles to. LCFPOP's orphan guard does not see this stack, and
\ LVH-OFF's cell -1 is LVD-CELL itself ($578/$580), so every consumer proves a
\ level exists before touching it and otherwise takes the closer-without-opener
\ reject. `loop` and `+loop` prove it
\ through their CFK-DO entry, opened and closed with the level; guarding `leave`
\ keeps LVD-CELL a sole-writer count of open levels.
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

\ The opening `do` and `?do` share: refuse first, then take the start into x29
\ and the limit into x17, snapshot the rest of the VS as the loop layout L
\ (LVSNAP: its forces write pool registers and x16 only), push the loop frame,
\ open the level and record L's snapshot frame index in LVS. The body, the
\ back edge, `?do`'s skip and every `leave` meet in L.
: C-DO-OPEN ( -- )
   C-CF-ROOM  LVROOM
   LIDX C-VS-TOP>REG,  17 C-VS-TOP>REG,
   LVSNAP LABEL@ BL,
   J-FRAME  LVOPEN
   9 DATA LVD-CELL LDR,  9 9 1 SUBI,     \ LVS[LVD-1] := the snapshot depth - 1
   10 9 LVS-OFF ADDI,  10 DATA 10 ADD,
   11 DATA JIT-SNAP:SP-CELL LDR,  11 11 1 SUBI,  11 10 0 STRB, ;

: J-DO ( -- )
   C-DO-OPEN  CFK-DO C-PUSHCP  LIDX-SET, ;

\ `?do` enters only where its closer lets the first turn run. It emits `loop`'s
\ test, start < limit, so a limit at or below the start takes no turn, and
\ records the branch in LVQ: `+loop` steps by a per-turn value that may count
\ down, so the `+loop` closing this level rewrites it to skip equal bounds only.
: J-?DO ( -- )                                  \ DO, but skip the loop unless start < limit
   C-DO-OPEN
   LIDX 17 ENC-CMP C-EMITW
   9 DATA LVD-CELL LDR,  9 9 1 SUBI,     \ LVQ[LVD-1] := the entry branch's offset
   10 9 3 LSLI,  10 10 LVQ-OFF ADDI,  10 DATA 10 ADD,
   11 CP DBASE SUB,  11 10 0 STR,
   $5400004B C-EMITW                     \ b.lt +8 (over the skip placeholder)
   LVLEAVE                               \ the skip leaves in L: the VS is the snapshot
   CFK-DO C-PUSHCP  LIDX-SET, ;

\ `leave` reconciles to its level's snapshot frame, L, popping neither it nor a
\ frame above it, and branches to the loop's exit, which arrives in L. The code
\ after it is dead until the next closer, which reconciles from VS = L.
: J-LEAVE ( -- )
   LVREQUIRE
   9 DATA LVD-CELL LDR,  9 9 1 SUBI,
   10 9 LVS-OFF ADDI,  10 DATA 10 ADD,  6 10 0 LDRB,
   LVRECONAT LABEL@ BL,
   LVLEAVE ;

: J-UNLOOP ( -- )                               \ pop one loop frame, no branch
   C-LOOP-POP,
   LIDX-CLEAR, ;                         \ x29 holds the popped loop's index

: J-LOOPEND ( -- )                              \ shared LOOP/+LOOP tail: pop frame, patch
   14 CP 0 ADDI,                         \ LEAVE/?DO skips to the pop point, LVD--
   C-LOOP-POP,
   9 DATA LVD-CELL LDR,  9 9 1 SUBI,  9 DATA LVD-CELL STR,
   10 9 3 LSLI,  10 10 LVH-OFF ADDI,  10 DATA 10 ADD,  9 10 0 LDR,
   LBCHAIN LABEL@ BL, ;

\ LIDX-LOAD?, : x29 := the index at [x16], the frame, unless LIDX-CELL says
\ x29 holds it. Clobbers x9.
: LIDX-LOAD?, ( -- )
   LBL {: kept:label :}
   9 DATA LIDX-CELL LDR,  9 kept CBNZ,
      LIDX 16 0 ENC-LDR C-EMITW
   kept LBL, ;

\ `loop` and `+loop` reconcile the VS to L before the step: the reconcile is
\ flag-neutral and touches neither x0 nor x17, and nothing sits between the
\ compare and its branch.
: J-LOOP ( -- )
   0 CFK-DO C-CF-AT                      \ no open DO entry: reject before emitting or popping
   LVRECON LABEL@ BL,                    \ VS -> L; pops the DO snapshot frame
   C-LOOP-TOP,
   LIDX-LOAD?,
   17 16 8 ENC-LDR C-EMITW               \ x17 = limit
   LIDX LIDX 1 ENC-ADDI C-EMITW
   LIDX 16 0 ENC-STR C-EMITW
   LIDX 17 ENC-CMP C-EMITW
   LCFPOP LABEL@ BL,
   10 9 CP SUB,  10 10 2 ASRI,  5 $7FFFF LIT64,  10 10 5 AND,  10 10 5 LSLI,
   9 $5400000B LIT64,  9 9 10 ORR,  LCEMIT LABEL@ BL,
   J-LOOPEND ;

: J-+LOOP ( -- )                                \ cross the limit boundary in the step's direction
   0 CFK-DO C-CF-AT                      \ no open DO entry: reject before emitting or popping
   LBL {: plain:label :}
   9 DATA LVD-CELL LDR,  9 9 1 SUBI,     \ a `?do` opened this level: rewrite its entry
   10 9 3 LSLI,  10 10 LVQ-OFF ADDI,  10 DATA 10 ADD,
   9 10 0 LDR,  9 plain CBZ,
   10 DBASE 9 ADD,  11 DATA REGION-ALIAS:DELTA-CELL LDR,  10 10 11 ADD,   \ through the alias
   11 $54000041 LIT64,  11 10 0 STRW,                   \ b.ne +8: skip equal bounds only
   plain LBL,
   0 C-VS-TOP>REG,
   LVRECON LABEL@ BL,                    \ VS -> L; pops the DO snapshot frame
   C-LOOP-TOP,
   LIDX-LOAD?,                           \ x29 = index
   17 16 8 ENC-LDR C-EMITW               \ x17 = limit
   1 LIDX 17 ENC-SUB C-EMITW             \ x1 = old index - limit
   LIDX LIDX 0 ENC-ADD C-EMITW
   LIDX 16 0 ENC-STR C-EMITW
   17 LIDX 17 ENC-SUB C-EMITW            \ x17 = new index - limit
   17 1 17 ENC-EOR C-EMITW               \ old xor new
   1 1 0 ENC-EOR C-EMITW                 \ old xor step
   1 1 17 ENC-AND C-EMITW                \ directed boundary crossing
   1 0 ENC-CMPI C-EMITW
   LCFPOP LABEL@ BL,
   10 9 CP SUB,  10 10 2 ASRI,  5 $7FFFF LIT64,  10 10 5 AND,  10 10 5 LSLI,
   9 $5400000A LIT64,  9 9 10 ORR,  LCEMIT LABEL@ BL,       \ b.ge loop-top
   J-LOOPEND ;

;package

package ENGINE-EMIT
\ LIDX-RELOAD?, : x29 := the innermost index from its frame, through x16 and
\ x17, which are never pooled, so no live value moves and no flag changes,
\ unless LIDX-CELL says x29 holds it; then it does. Clobbers x9.
: LIDX-RELOAD?, ( -- )
   LBL {: kept:label :}
   9 DATA LIDX-CELL LDR,  9 kept CBNZ,
      C-LOOP-TOP,
      LIDX 16 0 W-LDRX C-EMITW
      LIDX-SET,
   kept LBL, ;

public
\ `i` keeps the virtual stack (CFN-ENTRY) and pushes a copy of x29 as an
\ ordinary register entry, as a local read does; with the
\ pool exhausted it spills and pushes x29 to memory.
: J-I ( -- )
   LBL LBL {: mem:label done:label :}
   LIDX-RELOAD?,
   LVRALLOC LABEL@ BL,  14 mem CBZ,
      8 W-MOVRR LIDX 16 lshift or LIT64,  9 8 14 ORR,  LCEMIT LABEL@ BL,   \ mov xR,x29
      LVPUSHR LABEL@ BL,
      done B,
   mem LBL,
      LVSPILL LABEL@ BL,
      W-PUSHR LIDX or C-EMITW                                         \ str x29,[x19],#8
   done LBL, ;

: J-J ( -- )                                    \ outer loop index: frame[LOOPSP-2]
   4181780107 C-EMITW  $D100096B C-EMITW 3548179820 C-EMITW  LOOP-FRAME-ADDR,
   4181721481 C-EMITW  4177527401 C-EMITW  2432705139 C-EMITW ;

private
\ A fail-closed death that names the current token: the token on fd 2, the
\ newline from LOPENNL, then exit rc. Nothing recovers it, not even inside
\ evaluate, unlike C-DIE-TOKEN's LCOMPILEDIE tail. The token is the reader's
\ bytes, so the newline is a second write.
: C-EXIT-TOKEN ( n -- ) {: rc:n :}
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 2 MOVZ,  1 LOPENNL LABEL@ ADR,  2 NL-LEN MOVZ,  NR-WRITE SYS,
   0 rc MOVZ,  NR-EXIT-GROUP SYS, ;

public
\ The same death for a cause that is baked text: the label's len bytes on fd 2,
\ the newline from LOPENNL, then exit rc. A missing engine word names its own
\ lookup keyword (rc 70); a lowering transaction failure states it (rc 76).
: C-EXIT-LABEL ( ptr n n n -- ) {: msg:ptr len:n rc:n :}
   0 2 MOVZ,  1 msg LABEL@ ADR,  2 len MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 LOPENNL LABEL@ ADR,  2 NL-LEN MOVZ,  NR-WRITE SYS,
   0 rc MOVZ,  NR-EXIT-GROUP SYS, ;

\ C-TASK-GUARD is habu1.f B-TASK-GUARD's thread test; it names the keyword run
\ on a task's thread. Every entry to the outer loop on a task thread (B-EVAL,
\ the loaders) is refused before any text is read, so this keyword guard is the
\ sink's own invariant, not a refusal a program reaches: `: x ;`, `create x`,
\ `package PX` and `included` from a task all exit 79 with empty stderr.
: C-TASK-GUARD ( -- )
   LBL {: ok:label :}
   9 DATA TASK-TCB-CELL LDR,  9 ok CBZ,
      $4F C-EXIT-TOKEN
   ok LBL, ;

\ C-PUSH-DREC-NAME: push the definition's original qualified spelling for the
\ engine->checker record calls (trust/defer rows): the first token of the body
\ text block, which LBCAP seeds with "NAME " and C-QUALIFY-DEF leaves alone (it
\ moves only TKA/TKL), the spelling the CHECK cert path reads as NMA. The block,
\ read through its handle BODY-TEXT:BASE-CELL, survives a multi-line body
\ (DEF-TKA goes stale when the reader refills), and it is the qualified
\ `PKG:tail`, so CHECKER-RECORD-SYM records ( pkg, tail ) PUBLIC like the
\ certify record instead of a bare `tail` row that shadows core prims. The
\ Gforth stage0 mirror reads the body text for the same reason.
\ Scratch stays off x11: DEF-TRUST:FIND leaves the record XT
\ there for the caller's C-CALL-X11-SAVED.
: C-PUSH-DREC-NAME ( -- )
   LBL LBL {: scan:label done:label :}
   9 DATA BODY-TEXT:BASE-CELL LDR,      \ x9 = name start (body text base)
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

private
: C-PUSH-DATA-CELL ( n -- ) {: off :}
   9 DATA off LDR,  9 G-PUSH ;

public
: C-PUSH-TRUST-SIG ( n n -- ) {: aoff uoff :}
   aoff C-PUSH-DATA-CELL
   uoff C-PUSH-DATA-CELL ;

: C-CALL-X11-SAVED ( -- )
   SP SP 16 SUBI,  30 SP 0 STR,
   11 BLR,
   30 SP 0 LDR,  SP SP 16 ADDI, ;

private

public
;package

\ The active native compiler retains its checker even while a build replaces
\ the source dictionary. The record's fields are the actual checker operations;
\ a distinct target checker owns a second record during source replacement.
\ FIND, MARK-END and RETRACT-TO open the package in habu1.f, before their first use.
package DECL-OWNER
public

: TARGET ( n label -- ) {: off:n absent:label :}
   11 DATA NCOMP-DISPATCH:TARGET-DECL-CELL LDR,  11 absent CBZ,
   11 11 off LDR,  11 absent CBZ, ;

: SKIP-SAME ( n label -- ) {: off:n same:label :}
   LBL {: different:label :}
   12 DATA NCOMP-DISPATCH:DECL-CELL LDR,  12 different CBZ,
   12 12 off LDR,  11 12 CMP,  C-EQ same BCOND,
   different LBL, ;

: SIGNATURE ( n n n -- ) {: off:n aoff:n uoff:n :}
   LBL {: done:label :}
   off FIND  11 done CBZ,
   C-PUSH-DREC-NAME
   aoff uoff C-PUSH-TRUST-SIG
   C-CALL-X11-SAVED
   done LBL, ;

: NAME ( n -- ) {: off:n :}
   LBL {: done:label :}
   off FIND  11 done CBZ,
   C-PUSH-DREC-NAME
   C-CALL-X11-SAVED
   done LBL, ;

;package

\ `trust-decl` and `trust` register the same row; they differ in what the caller
\ may be asked to prove. A definer's record is not findable at its own publish
\ tail (measured: absent from `search-wl` there, present one statement later),
\ so this registrar never consults the dictionary. Source `trust` claims a word
\ exists and is refused when it does not.
package DEF-TRUST

public

\ Resolve `trust-decl` (checker.f TRUST-DECL) into x11. Same fail-closed shape
\ as LASTC-TRUST:FIND-RAW: a missing registrar names itself on fd 2 and exits 70
\ rather than publishing a definition whose effect nothing recorded. Public
\ because BDRAINPRETRUST needs the XT for its own push sequence.
: FIND ( -- )
   LBL LBL {: absent:label ready:label :}
   NCOMP-DISPATCH:DECL-EFFECT-OFF absent DECL-OWNER:TARGET
   ready B,
   absent LBL,  KWDATA:LKWTRUSTDECL KWDATA:TRUSTDECL$ nip 70 C-EXIT-LABEL
   ready LBL, ;

\ Register the pending definition's declared signature: name from the body
\ buffer, signature from the TSIG cells. Not named `CALL`: tools/lint/clobber-lint.f
\ reads an upper-case `PKG:CALL` as a register-move prelude plus a branch to a
\ shared engine helper (PROT-GUARD:CALL) and refuses any other.
: REGISTER ( -- )
   LBL {: done:label :}
   NCOMP-DISPATCH:DECL-EFFECT-OFF TSIG-A-CELL TSIG-U-CELL DECL-OWNER:SIGNATURE
   NCOMP-DISPATCH:DECL-EFFECT-OFF done DECL-OWNER:TARGET
   NCOMP-DISPATCH:DECL-EFFECT-OFF done DECL-OWNER:SKIP-SAME
   C-PUSH-DREC-NAME
   TSIG-A-CELL TSIG-U-CELL C-PUSH-TRUST-SIG
   C-CALL-X11-SAVED
   done LBL, ;

\ The identity declarers' registrar: REGISTER's push sequence, reaching the
\ owner field at `off` instead of `trust-decl`: DECL-CAST-OFF for `cast:`
\ (checker.f CHECKER-DEFCAST), DECL-LINEAR-OFF for `linear:` (CHECKER-LINEAR).
\ It first proves the row legal (CAST-CERTIFY's or LINEAR-CERTIFY's refusals)
\ and refuses by throwing, so C-IDENTITY calls it before counting the record
\ into NDICT.
: REGISTER-IDENTITY ( n -- ) {: off:n :}
   LBL {: done:label :}
   off TSIG-A-CELL TSIG-U-CELL DECL-OWNER:SIGNATURE
   off done DECL-OWNER:TARGET
   off done DECL-OWNER:SKIP-SAME
   C-PUSH-DREC-NAME
   TSIG-A-CELL TSIG-U-CELL C-PUSH-TRUST-SIG
   C-CALL-X11-SAVED
   done LBL, ;

;package

\ The two public words below are every place the engine gives a defining
\ word's creation an effect the checker believes. All register through
\ `trust-raw`: the effect's type variables are raw and cannot bind a nominal
\ family, so an integer stored into the cell never reads back as a sealed
\ nominal. PUBLISH: a `does>` clause's declared runtime effect (user definers
\ and PTR-VARIABLE); PUBLISH-A: `-- a` for constant.
package LASTC-TRUST

\ Resolve `trust-raw` (checker.f TRUST-RAW). Same shape as DEF-TRUST:FIND: a
\ missing registrar names itself on fd 2 and exits 70.
: FIND-RAW ( -- )
   LBL LBL {: absent:label ready:label :}
   NCOMP-DISPATCH:DECL-RAW-OFF absent DECL-OWNER:TARGET
   ready B,
   absent LBL,  KWDATA:LKWTRUSTRAW KWDATA:TRUSTRAW$ nip 70 C-EXIT-LABEL
   ready LBL, ;

public

: FIND-ACTIVE ( label -- ) {: absent:label :}
   LBL {: ready:label :}
   NCOMP-DISPATCH:DECL-RAW-OFF DECL-OWNER:FIND  11 ready CBNZ,
   9 DATA HOOK-CELL LDR,  9 absent CBZ,
   FIND-RAW
   ready LBL, ;

: FIND-TARGET ( label -- ) {: absent:label :}
   11 DATA NCOMP-DISPATCH:DECL-CELL LDR,  11 absent CBZ,
   NCOMP-DISPATCH:DECL-RAW-OFF absent DECL-OWNER:TARGET
   NCOMP-DISPATCH:DECL-RAW-OFF absent DECL-OWNER:SKIP-SAME ;

: PUBLISH ( -- )
   LBL {: nohook:label :}
   nohook FIND-ACTIVE
   C-PUSH-DREC-NAME
   CRSIG-A-CELL CRSIG-U-CELL C-PUSH-TRUST-SIG
   C-CALL-X11-SAVED
   nohook FIND-TARGET
   C-PUSH-DREC-NAME
   CRSIG-A-CELL CRSIG-U-CELL C-PUSH-TRUST-SIG
   C-CALL-X11-SAVED
   nohook LBL, ;

: PUBLISH-A ( -- )
   LBL {: nohook:label :}
   nohook FIND-ACTIVE
   C-PUSH-DREC-NAME
   9 LSIGA LABEL@ ADR,  9 G-PUSH
   9 4 MOVZ,  9 G-PUSH
   C-CALL-X11-SAVED
   nohook FIND-TARGET
   C-PUSH-DREC-NAME
   9 LSIGA LABEL@ ADR,  9 G-PUSH
   9 4 MOVZ,  9 G-PUSH
   C-CALL-X11-SAVED
   nohook LBL, ;

;package

package NCOMP-EMIT
public

\ Emit the shared missing-dispatch death. Both readers ask the same integrity
\ question, so the diagnostic and exit code have one owner too.
: UNSET ( -- )
   0 2 MOVZ,  1 LUNSET LABEL@ ADR,  2 UNSET-MSG$ nip MOVZ,  NR-WRITE SYS,
   0 ENGINE-ERROR:AOT-SEED MOVZ,  NR-EXIT-GROUP SYS, ;

\ Callers reach it only with a nonzero latch, so it has no JIT fall-through.
: LOAD ( -- )
   LBL LBL LBL {: xt:label have:label ready:label :}
   11 DATA NCOMP-DISPATCH:DEF-TIER-CELL LDR,  11 2 CMPI,  C-NE xt BCOND,
      11 DATA NCOMP-DISPATCH:COMP-CELL LDR,
      have B,
   xt LBL,
      11 DATA NCOMP-DISPATCH:XT-CELL LDR,
   have LBL,
   11 ready CBNZ,
      UNSET
   ready LBL, ;

\ Both `:` handlers reach it with every cell the JIT's closure reads reset. The
\ JIT's entry slot is the one EM-COMPILE-RET restores or rewrites to a nop.
: TIER-COLON-DISPATCH ( -- )
   LBL LBL {: jit:label done:label :}
   9 TIER-LATCH
   9 DATA NCOMP-DISPATCH:DEF-TIER-CELL STR,
   9 jit CBZ,
      LOAD
      done B,
   jit LBL,
      9 CP 0 ADDI,  9 DATA FRAME-CELL STR,         \ the entry slot, no call seen yet
      9 W-LINKSAVE LIT64,  LCEMIT LABEL@ BL,       \ str x30,[sp,#-16]!
   done LBL, ;
;package

package ENGINE-EMIT
: C-CALL-CHECKER-DEFER ( -- )
   LBL {: done:label :}
   NCOMP-DISPATCH:DECL-DEFER-OFF DECL-OWNER:NAME
   NCOMP-DISPATCH:DECL-DEFER-OFF done DECL-OWNER:TARGET
   NCOMP-DISPATCH:DECL-DEFER-OFF done DECL-OWNER:SKIP-SAME
   C-PUSH-DREC-NAME
   C-CALL-X11-SAVED
   done LBL, ;

public
\ x11 = the operation at OFF in the source owner's record. An owner whose record
\ ends before OFF (its byte size at [record-8])
\ or holds 0 there writes the line MA MU to fd 2 and exits 70. Nothing is found
\ by name: the scan and the certificate it leaves are one owner's.
: C-OWNER-FIELD ( n ptr u8 n -- ) {: off:n ma:ptr mu:n :}
   LBL LBL LBL {: absent:label msg:label ready:label :}
   11 DATA NCOMP-DISPATCH:DECL-CELL LDR,  11 absent CBZ,
   10 11 8 SUBI,  10 10 0 LDR,
   10 off CELL + CMPI,  C-LT absent BCOND,
   11 11 off LDR,  11 ready CBNZ,
   absent LBL,
      0 2 MOVZ,  1 msg ADR,  2 mu MOVZ,  NR-WRITE SYS,
      0 70 MOVZ,  NR-EXIT-GROUP SYS,
   msg LBL,  ma mu BYTES,
   ready LBL, ;

\ After ndict++ the checker's rec-wide-publish consumes the RECW latch for this
\ definition's effect and, when wide, marks record ndict-1 DNAME-WIDE. Both
\ calls go through the source owner's record, so the tail runs once checker.f
\ has stored it (DECL-CELL), not on the checking switch: a linker-built hb has
\ no HOOK-CELL, and its does> runtime publishes here (test/underdepth-gate.f
\ UDG-MQA). The engine prefix self-load before checker.f has no owner. The
\ same tail drains the RECMI latch through rec-min-in@ into DNAME-MIN-IN
\ (flags bits 52-59) through the alias, so interpret-find fails closed on a
\ bare underdepth call; min-in 0 (unchecked) pokes nothing.
: EM-REC-WIDE-PUBLISH ( -- )
   LBL LBL {: nohook:label nomark:label :}
   9 DATA NCOMP-DISPATCH:DECL-CELL LDR,  9 nohook CBZ,
   NCOMP-DISPATCH:DECL-REC-WIDE-PUBLISH-OFF RECWPUB$ C-OWNER-FIELD
   C-CALL-X11-SAVED
   NCOMP-DISPATCH:DECL-REC-MIN-IN-OFF RECMIQ$ C-OWNER-FIELD
   C-CALL-X11-SAVED
   10 G-POP                                            \ x10 = latched min-in cells (0 = none)
   10 nomark CBZ,
      9 NDICT 0 ADDI,  9 9 1 SUBI,  12 DREC MOVZ,  9 9 12 MUL,  9 DBASE 9 ADD,   \ x9 = &record[NDICT-1] (x10 latch survives via kernel x2-x15)
      10 10 $FF ANDI,  10 10 52 LSLI,                  \ min-in byte -> DNAME-MIN-IN bits 52-59
      11 DATA REGION-ALIAS:DELTA-CELL LDR,  11 9 11 ADD,   \ x11 = the record through the alias
      12 11 16 LDR,  12 12 10 ORR,  12 11 16 STR,
   nomark LBL,
   nohook LBL, ;

private

: C-EMIT-DATA-X16! ( n -- ) {: off :}
   16 20 off W-STRX C-EMITW ;

\ compile-mode PC-RELATIVE address: emit `adr x9, target`. Unlike C-LIT's
\ absolute movz/movk, the offset survives the AOT blob copy and the ASLR slide,
\ because the target (an embedded body) moves WITH this instruction. target in
\ x11; CP (the emit cursor / future ADR pc) is x28.
: C-ADR-X9 ( -- )
   5 11 28 SUB,                                                       \ x5 = d = target - CP
   8 $10000009 LIT64,                                                 \ ADR opcode | Rd=x9
   6 3 MOVZ,  7 5 6 AND,  7 7 29 LSLI,  8 8 7 ORR,                    \ | (d & 3) << 29
   7 5 2 LSRI,  6 $7FFFF LIT64,  7 7 6 AND,  7 7 5 LSLI,  8 8 7 ORR,  \ | ((d>>2) & 0x7FFFF) << 5
   9 8 0 ADDI,  LCEMIT LABEL@ BL, ;

\ Where C-PLACE-CREATED-SIG put J-DOES's signature, emitted as a string
\ literal's address is: DATA-relative when interned, PC-relative in the code.
: C-EMIT-CRSIG-A! ( -- )
   LBL LBL {: inline done :}
   11 DATA TCSIG-A-CELL LDR,
   9 DATA NSTR-ABI:INTERN-CELL LDR,  9 inline CBZ,
   C-DATA-ADDR-RAW  done B,
   inline LBL,  C-ADR-X9
   done LBL,
   9 20 CRSIG-A-CELL W-STRX C-EMITW ;

: C-EMIT-CRSIG-U! ( -- )
   11 DATA TCSIG-U-CELL LDR,  C-RAW-LIT
   CRSIG-U-CELL C-EMIT-DATA-X16! ;

: C-EMIT-CRSIG-SET ( -- )
   LBL {: none :}
   9 DATA TCSIG-U-CELL LDR,  9 none CBZ,
      C-EMIT-CRSIG-A!
      C-EMIT-CRSIG-U!
   none LBL, ;

public
: C-RUNTIME-CRSIG-CLEAR ( -- )
   9 0 MOVZ,
   9 DATA CRSIG-A-CELL STR,
   9 DATA CRSIG-U-CELL STR, ;

private
\ >R R> R@: the user return stack is a guarded mapping whose base is a header
\ cell (STACK-ABI:RETURN-BASE-CELL), so the slot address is that base + depth*8.
: RSTK-SLOT-ADDR, ( -- )                                      \ x11 = base + x10*8
   11 20 STACK-ABI:RETURN-BASE-CELL W-LDRX C-EMITW
   11 11 10 W-ADDX-LSL3 C-EMITW ;

public
: J-TOR ( -- )                                                \ pop data -> push RSTK
   W-POP9 C-EMITW                                      \ ldr x9,[x19,#-8]!
   10 20 RSP-CELL W-LDRX C-EMITW
   RSTK-SLOT-ADDR,
   9 11 0 W-STRX C-EMITW
   $9100054A C-EMITW                                   \ add x10,x10,#1
   10 20 RSP-CELL W-STRX C-EMITW ;

private
: J-RPOP ( -- )                                               \ x9 = RSTK top, x10 = RSP-1
   10 20 RSP-CELL W-LDRX C-EMITW
   $D100054A C-EMITW                                   \ sub x10,x10,#1
   RSTK-SLOT-ADDR,
   9 11 0 W-LDRX C-EMITW ;

public
: J-RFROM ( -- )  J-RPOP                                      \ pop RSTK -> push data
   10 20 RSP-CELL W-STRX C-EMITW
   W-PUSH9 C-EMITW ;                                   \ str x9,[x19],#8

: J-RFETCH ( -- )  J-RPOP                                     \ peek RSTK -> push data
   W-PUSH9 C-EMITW ;

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

\ C-BBACK builds the backward displacement for `b` and for `bl` alike, so
\ J-RECURSE counts its call in the frame cell itself.
: J-RECURSE ( -- )
   LIDX-CLEAR,                                         \ the call writes x29 when it loops
   9 10 FRAME-CALL,
   9 DATA PEND-CELL LDR,  9 9 0 LDR,  $94000000 $3FFFFFF C-BBACK ;   \ bl entry

private
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

\ Fires before the name is counted into NDICT, so a rollback inside evaluate
\ drops only compile state.
: C-SIG-BAD ( -- )
   0 2 MOVZ,  1 LSIGBAD LABEL@ ADR,  2 SIGBAD-MSG-LEN MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 76 MOVZ,  LCOMPILEDIE LABEL@ B, ;

public
\ The inside of the signature after `does>`: x11 its first byte, x10 its
\ length, INP past its `)`. A missing or open one refuses (C-SIG-BAD).
: C-CREATED-SIG$ ( -- )
   LBL LBL {: done bad :}
   bad C-SIG-START
   bad C-SIG-END
   15 DATA INP-CELL STR,
   C-SIG-INNER$
   10 12 0 ADDI,
   done B,
   bad LBL,  C-SIG-BAD
   done LBL, ;

private
\ The signature (x11, x10 bytes) is a string literal of the definer and goes
\ where C-SDQ puts one: interned in the literal store, or, before intern.f
\ installs the interner, in the code behind a branch. TCSIG-A/U take its
\ address and length.
: C-PLACE-CREATED-SIG ( -- )
   LBL LBL LBL LBL {: cl cd inline done :}
   10 DATA TCSIG-U-CELL STR,
   9 DATA NSTR-ABI:INTERN-CELL LDR,  9 inline CBZ,
   12 10 0 ADDI,  LNSTRINTERN LABEL@ BL,
   done B,
   inline LBL,
   15 CP 0 ADDI,  9 $14000000 LIT64,  LCEMIT LABEL@ BL,
   12 CP 0 ADDI,
   9 10 0 ADDI,
   1 DATA REGION-ALIAS:DELTA-CELL LDR,  1 CP 1 ADD,               \ x1 = CP through the alias
   cl LBL,  9 cd CBZ,
      14 11 0 LDRB,  14 1 0 STRB,  1 1 1 ADDI,  28 28 1 ADDI,
      11 11 1 ADDI,  9 9 1 SUBI,  cl B,
   cd LBL,
   28 28 3 ADDI,  5 -4 LIT64,  28 28 5 AND,
   9 15 0 ADDI,  LPAT LABEL@ BL,
   11 12 0 ADDI,
   done LBL,
   11 DATA TCSIG-A-CELL STR, ;

public

\ J-DOES is emitted further down, past the capacity exits its clause record
\ reaches for (C-DIE-DICT-FULL / C-DIE-CODE-FULL).

private
\ The word frame is decided at `;`, not at the colon: the JIT is single pass, so
\ the opener emits the link-register save and leaves the slot's address in
\ FRAME-CELL, and every emitter that puts a call in the body counts it in the
\ cell's low two bits (FRAME-CALL,, which J-RECURSE calls). A tail branch that
\ removed the only call has already rewritten the slot and zeroed the cell. A body that called gets the restore; one that did not has
\ its slot rewritten to a nop in place, so every address recorded during the
\ compile (branch placeholders, the record start, quotation entries,
\ breakpoints) still names its instruction. The nop is written through the
\ alias and flushed at `;`, like LPAT's and LBCHAIN's writes below CP.
\ SLOT-NOP, rewrites the entry slot at x9 to a nop; clobbers x1, x11.
: SLOT-NOP, ( -- )
   1 DATA REGION-ALIAS:DELTA-CELL LDR,  1 9 1 ADD,     \ through the alias
   11 W-NOP LIT64,  11 1 0 STRW, ;                     \ the entry slot stops framing

public
: EM-COMPILE-RET ( -- )
   LBL LBL {: leaf:label done:label :}
   11 DATA FRAME-CELL LDR,
   11 done CBZ,                                        \ no frame was opened for this body
   10 11 1 ANDI,  10 leaf CBZ,
      9 W-LINKREST LIT64,  LCEMIT LABEL@ BL,           \ ldr x30,[sp],#16
      done B,
   leaf LBL,                                           \ no call, so x11 IS the slot
      9 11 0 ADDI,  SLOT-NOP,
   done LBL,
   9 W-RET LIT64,  LCEMIT LABEL@ BL,
   9 0 MOVZ,  9 DATA FRAME-CELL STR, ;

private
\ The Q cells hold the innermost open quotation; each enclosing one waits in a
\ JIT-QUOT frame (src/habu/layout.f). `[:` parks the open one before it reuses
\ the cells and `;]` pops it back once its own body is closed.
: QUOT-FRAME, ( -- )                      \ x11 := DATA + STK-OFF + FRAME-BYTES*x10; x12 scratch
   12 JIT-QUOT:FRAME-BYTES MOVZ,  11 10 12 MUL,
   12 JIT-QUOT:STK-OFF MOVZ,  11 11 12 ADD,  11 DATA 11 ADD, ;

: QUOT-PARK, ( -- )                       \ frame x11 := QPATCH, QENT, QXH, QFRAME
   9 DATA QPATCH-CELL LDR,  9 11 0 STR,
   9 DATA QENT-CELL LDR,  9 11 8 STR,
   9 DATA QXH-CELL LDR,  9 11 16 STR,
   9 DATA QFRAME-CELL LDR,  9 11 24 STR, ;

: QUOT-UNPARK, ( -- )                     \ QPATCH, QENT, QXH, QFRAME := frame x11
   9 11 0 LDR,  9 DATA QPATCH-CELL STR,
   9 11 8 LDR,  9 DATA QENT-CELL STR,
   9 11 16 LDR,  9 DATA QXH-CELL STR,
   9 11 24 LDR,  9 DATA QFRAME-CELL STR, ;

: J-QUOT ( -- )
   LBL LBL {: qok:label qroom:label :}
   C-CF-ROOM                                       \ refuse before parking an enclosing quotation
   9 DATA QPATCH-CELL LDR,  9 qok CBZ,                    \ a quotation is open: park it
      10 DATA JIT-QUOT:SP-CELL LDR,
      10 JIT-QUOT:LEVELS 1- CMPI,  C-LT qroom BCOND,
         LQNEST LABEL@ B,                                 \ JIT-QUOT:LEVELS are open: x10 = the frames parked. Fires before J-QUOT writes or emits anything; rollback drops compile-state
      qroom LBL,
      QUOT-FRAME,  QUOT-PARK,
      10 10 1 ADDI,  10 DATA JIT-QUOT:SP-CELL STR,
   qok LBL,
   9 CP 0 ADDI,  9 DATA QPATCH-CELL STR,
   CFK-QUOT C-PUSHCP                               \ closers inside see the quotation, not the body around it
   9 $14000000 LIT64,  LCEMIT LABEL@ BL,               \ b-over placeholder
   9 CP 0 ADDI,  9 DATA QENT-CELL STR,            \ the quotation's entry
   9 DATA EXITH-CELL LDR,  9 DATA QXH-CELL STR,   \ scope the EXIT chain
   12 0 MOVZ,  12 DATA EXITH-CELL STR,
   9 DATA FRAME-CELL LDR,  9 DATA QFRAME-CELL STR,  \ ...and the frame with it
   9 CP 0 ADDI,  9 DATA FRAME-CELL STR,             \ its own entry slot, no call yet
   9 W-LINKSAVE LIT64,  LCEMIT LABEL@ BL, ;         \ str x30,[sp,#-16]!

: J-SEMIQUOT ( -- )
   LBL LBL LBL {: sqok:label sqbody:label sqdone:label :}
   9 DATA QPATCH-CELL LDR,  9 sqok CBNZ,                  \ ;] with no open quotation: recoverable inside evaluate (rc 75), fail-closed exit 75 at top level. Fires before J-SEMIQUOT emits; rollback drops compile-state
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 75 MOVZ,  LCOMPILEDIE LABEL@ B,
   sqok LBL,
   0 CFK-QUOT C-CF-AT  LCFPOP LABEL@ BL,               \ every structure opened inside is closed
   14 CP 0 ADDI,  9 DATA EXITH-CELL LDR,  LBCHAIN LABEL@ BL,   \ exits -> this epilogue
   9 DATA QXH-CELL LDR,  9 DATA EXITH-CELL STR,
   EM-COMPILE-RET                                       \ its own frame, its own ret
   9 DATA QFRAME-CELL LDR,  9 DATA FRAME-CELL STR,      \ the enclosing body is open again
   9 DATA QPATCH-CELL LDR,  LPAT LABEL@ BL,             \ b-over lands here
   11 DATA QENT-CELL LDR,  C-CODE-ADDR             \ push the xt in the outer word (relocatable code addr)
   10 DATA JIT-QUOT:SP-CELL LDR,  10 sqbody CBZ,
      10 10 1 SUBI,  10 DATA JIT-QUOT:SP-CELL STR,
      QUOT-FRAME,  QUOT-UNPARK,                     \ the enclosing quotation is innermost again
      sqdone B,
   sqbody LBL,
      12 0 MOVZ,  12 DATA QPATCH-CELL STR,          \ the definition's own body is open again
   sqdone LBL, ;
;package

\ The does>-patch runtime routine emitter (LDOESPATCH): patches the created
\ word's RET into a branch and publishes the declared runtime effect.
\ It clears the created record's definer kind beside the patch: `create`
\ stamped DKIND:ADDR because its body pushed the data address, and a reader
\ folding the stale stamp would compile that address in place of a call. An
\ `EXPORT` alias shares the body and is cleared with it (CLEAR-KIND).
\ A zero clause entry restores the bare body. A fresh created word keeps its RET and stamp; one an earlier
\ clause patched gets its RET back but keeps the cleared kind (LASTC may name a
\ constant, so the bare body is no evidence of a DATA push).
\ The publish tail is the colon tail: after LASTC-TRUST:PUBLISH,
\ EM-REC-WIDE-PUBLISH drains the checker's latches, so a wide clause effect
\ marks the created record (ndict-1, as `create` counted it) DNAME-WIDE and
\ declared inputs poke DNAME-MIN-IN
\ (test/underdepth-gate.f).
package DOESPATCH

: GRID-BEFORE ( -- )
   LBL LBL {: absent:label done:label :}
   CHECKER-OWNER-ABI:GRID-BEFORE-OFF absent DECL-OWNER:TARGET
   10 SP 40 LDR,  10 G-PUSH
   C-CALL-X11-SAVED
   9 G-POP
   done B,
   absent LBL,  9 0 MOVZ,
   done LBL, ;

: GRID-AFTER ( -- )
   LBL {: done:label :}
   9 SP 32 LDR,  9 done CBZ,
   CHECKER-OWNER-ABI:GRID-AFTER-OFF done DECL-OWNER:TARGET
   10 SP 40 LDR,  10 G-PUSH
   9 SP 32 LDR,  9 G-PUSH
   C-CALL-X11-SAVED
   done LBL, ;

: NOTIFY-PUBLICATION ( -- )
   LBL {: done:label :}
   11 DATA NCOMP-DISPATCH:DECL-CELL LDR,  11 done CBZ,
   12 11 CHECKER-OWNER-ABI:HEADER-BYTES SUBI,
   13 12 0 LDR,  14 CHECKER-OWNER-ABI:MAGIC LIT64,
   13 14 CMP,  C-NE done BCOND,
   13 12 8 LDR,
   14 CHECKER-OWNER-EXT:PUBLICATION-USE-OFF CELL + LIT64,
   13 14 CMP,  C-LT done BCOND,
   11 11 CHECKER-OWNER-EXT:PUBLICATION-USE-OFF LDR,  11 done CBZ,
   9 SP 40 LDR,  9 9 DBASE SUB,
   12 DREC MOVZ,  9 9 12 UDIV,
   9 G-PUSH  C-CALL-X11-SAVED
   done LBL, ;

\ Clear the kind of the patched record x11 and of every record that shares its
\ body. The body is a clause now, not a push or cell definer, so no record
\ whose code pointer [0] is that body may keep a stamp: an `EXPORT` alias
\ copies its target's code pointer and DKIND:VAL (C-EXPORT), and a stale VAL
\ folds the alias into the value it pushed when it was folded. An alias is
\ made of an existing word, so every one is newer than its target, and the
\ scan walks only the records above x11, in every wordlist. It ends at the
\ larger of NDICT and the dictionary high-water (layout.f NDICT-HW-CELL), not
\ at NDICT: an alias a lowered `ndict!` retired keeps its record, and a later
\ raise re-exposes it with whatever stamp it holds. A record with no stamp has
\ no alias with one, so it scans nothing. Each match is rewritten through the
\ alias. Clobbers x0, x5, x10 and x13-x15.
: CLEAR-KIND ( -- )
   LBL LBL LBL {: scan:label next:label done:label :}
   5 DKIND:MASK DNAME-CELL or -1 xor LIT64,
   13 11 16 LDR,  14 DKIND:MASK LIT64,  14 13 14 AND,
   13 13 5 AND,
   0 DATA REGION-ALIAS:DELTA-CELL LDR,  0 11 0 ADD,  13 0 16 STR,   \ through the alias
   14 done CBZ,
   13 11 0 LDR,                                          \ x13 = the patched body
   14 11 DREC ADDI,                                      \ x14 = the record above it
   15 DATA NDICT-HW-CELL LDR,  15 NDICT CMP,  15 15 NDICT C-HI CSEL,
   10 DREC MOVZ,  15 15 10 MUL,  15 DBASE 15 ADD,       \ x15 = the end of every record ever published
   scan LBL,
   14 15 CMP,  C-CS done BCOND,
   10 14 0 LDR,  10 13 CMP,  C-NE next BCOND,
      0 DATA REGION-ALIAS:DELTA-CELL LDR,  0 14 0 ADD,                \ x0 = the record through the alias
      10 14 16 LDR,  10 10 5 AND,  10 0 16 STR,
   next LBL,
   14 14 DREC ADDI,  scan B,
   done LBL, ;

public

\ Native defining words pass the measured clause entry and the already parsed
\ created-word signature. The existing patch routine remains the one owner of
\ the created record, branch, kind stamp, and checker publication.
: PRIM ( -- )
   C G-POP  B G-POP  A G-POP
   B DATA CRSIG-A-CELL STR,  C DATA CRSIG-U-CELL STR,
   B A 0 ADDI,  LDOESPATCH LABEL@ BL, ;

private

: NONE$ ( -- ptr u8 n )
   s" hb: does> has no created word" ;

\ A LASTC of zero names no record: none was created since boot, or the one it
\ named was retired by a motion that lowered NDICT (habu1.f LASTC-TRIM,). A
\ LASTC whose wordlist cell is WL:RETIRED names a record `undefine`
\ retired in place (xref.f XREF:RETIRE), which leaves NDICT alone. Either is
\ refused before anything is patched, recoverable inside evaluate like the
\ other definition errors.
: EMIT-NONE ( label -- ) {: have:label :}
   LBL LBL {: none:label msg:label :}
   11 DATA LASTC-CELL LDR,  11 none CBZ,
   12 11 40 LDR,  13 WL:RETIRED invert MOVN,  12 13 CMP,  C-NE have BCOND,
   none LBL,
   0 2 MOVZ,  1 msg ADR,  2 NONE$ nip MOVZ,  NR-WRITE SYS,
   0 70 MOVZ,  LCOMPILEDIE LABEL@ B,
   msg LBL,  NONE$ BYTES, ;

public

: EMIT ( -- )
   LBL LBL LBL LBL LBL LBL LBL
   {: nocr:label slot:label declared:label write:label flush:label
      have:label before:label :}
   LDOESPATCH LABEL@ LBL,
   have EMIT-NONE
   have LBL,
   SP SP 48 SUBI,  30 SP 0 STR,  10 SP 8 STR,
   11 DATA LASTC-CELL LDR,                               \ created slot
   11 SP 40 STR,
   12 11 0 LDR,  13 11 8 LDR,
   14 13 CODE-SPAN:FULL ANDI,  13 13 CODE-SPAN:MASK ANDI,
   14 slot CBZ,  13 13 4 SUBI,
   slot LBL,  12 12 13 ADD,                              \ final RET/B slot from either representation
   12 SP 16 STR,
   9 12 0 ADDI,
   10 before CBNZ,
   14 12 0 LDRW,  5 W-RET LIT64,  14 5 CMP,  C-NE before BCOND,
   9 0 MOVZ,
   before LBL,
   9 SP 24 STR,
   GRID-BEFORE  9 SP 32 STR,
   9 SP 24 LDR,  11 SP 40 LDR,
   10 11 0 ADDI,  EM-DEF-OCC:LBEFORE LABEL@ BL,
   10 SP 8 LDR,  9 SP 24 LDR,
   9 declared CBZ,
   10 SP 8 LDR,  12 SP 16 LDR,  11 DATA LASTC-CELL LDR,
   14 W-RET LIT64,  10 write CBZ,                       \ an empty replacement cancels the previous clause
   14 10 12 SUB,  14 14 2 ASRI,                          \ delta words (negative)
   5 $3FFFFFF LIT64,  14 14 5 AND,
   5 $14000000 LIT64,  14 14 5 ORR,                      \ b D
   write LBL,
   13 DATA REGION-ALIAS:DELTA-CELL LDR,  13 12 13 ADD,  14 13 0 STRW,   \ through the alias
   10 flush CBZ,
   CLEAR-KIND
   flush LBL,
   12 SP 16 LDR,
   12 DCCVAU,  DSB-ISH,  12 ICIVAU,  DSB-ISH,  ISB,      \ flush the patched line
   declared LBL,
   \ The patched body is now executable. Reissue its occurrence before the
   \ checker publishes the created word's contract through trust-raw.
   9 SP 24 LDR,  10 DATA LASTC-CELL LDR,  EM-DEF-OCC:LAFTER LABEL@ BL,
   LBL {: noshadow:label :}
   11 DATA NCOMP-DISPATCH:DOES-SHADOW-CELL LDR,  11 noshadow CBZ,
   9 DATA LASTC-CELL LDR,  9 9 DBASE SUB,
   10 DREC MOVZ,  9 9 10 UDIV,  9 G-PUSH
   9 SP 8 LDR,  9 G-PUSH
   11 DATA NCOMP-DISPATCH:DOES-SHADOW-CELL LDR,  C-CALL-X11-SAVED
   noshadow LBL,
   9 DATA CRSIG-U-CELL LDR,  9 nocr CBZ,
      LASTC-TRUST:PUBLISH
      \ A second does> may replace an existing clause. Its new effect replaces
      \ the old guard facts, including a zero min-in or a scalar result.
      9 DATA LASTC-CELL LDR,
      12 9 16 LDR,  10 DNAME-WIDE DNAME-MIN-IN-MASK or -1 xor LIT64,
      11 DATA REGION-ALIAS:DELTA-CELL LDR,  11 9 11 ADD,              \ x11 = the record through the alias
      12 12 10 AND,  12 11 16 STR,
      EM-REC-WIDE-PUBLISH
      C-RUNTIME-CRSIG-CLEAR
   nocr LBL,
   GRID-AFTER
   \ The source observer sees the final effect and retained storage grid.
   NOTIFY-PUBLICATION
   30 SP 0 LDR,  SP SP 48 ADDI,  RET, ;

;package

package ENGINE-EMIT
public
\ Record defining words for the checker: append the kind token and run the hook;
\ the verdict is ignored (create/variable/constant always publish). kwv is the
\ keyword's label cell (LKWCREATE / LKWCONST), read by LABEL@, not its bytes.
: C-DEFHOOK ( ptr n n -- )  LBL {: kwv:ptr klen:n nohk:label :}
   11 kwv LABEL@ ADR,  12 klen MOVZ,  LBCS LABEL@ BL,
   9 DATA HOOK-CELL LDR,  9 nohk CBZ,
   10 DATA BODY-TEXT:BASE-CELL LDR,  10 G-PUSH
   10 DATA BODYLEN-CELL LDR,  10 G-PUSH
   SP SP 16 SUBI,  30 SP 0 STR,  9 BLR,  30 SP 0 LDR,  SP SP 16 ADDI,
   10 G-POP
   nohk LBL, ;

private
\ Capacity exits: the fixed label first,
\ then the token and a newline. The exit codes are contracts: dict full $4D=77,
\ code space full $4C=76.
: C-CAP-LABEL ( ptr n n -- ) {: msg:ptr len:n :}
   0 2 MOVZ,  1 msg LABEL@ ADR,  2 len MOVZ,  NR-WRITE SYS, ;

public
: C-DIE-TOKEN ( n -- ) {: rc:n :}                  \ definer capacity die (dict full $4D / code full $4C): recoverable inside evaluate (rollback frees the aborted definition), fail-closed exit rc at top level
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 rc MOVZ,  LCOMPILEDIE LABEL@ B, ;

: C-DIE-DICT-FULL ( -- )
   LDICTFULL DICTFULL-MSG$ nip C-CAP-LABEL
   $4D C-DIE-TOKEN ;

: C-DIE-CODE-FULL ( -- )
   LCODEFULL CODEFULL-MSG$ nip C-CAP-LABEL
   $4C C-DIE-TOKEN ;

\ The duplicate-definition label on fd 2. Each wall writes the refused name
\ after it, then exits $4E through LCOMPILEDIE.
: C-DUP-DEF-SAY ( -- )
   0 2 MOVZ,  1 LKWDUPDEF LABEL@ ADR,  2 KWDUPDEF$ nip MOVZ,  NR-WRITE SYS, ;
;package

\ A record's name is stored once for every definer.
\ `:`, `create`, `export`, `package` and a qualified definition store the token
\ in TKA/TKL; the definition writers further down (DEFWRITE bodies) store a
\ name a TRUSTED: caller hands them. Both go through this one emitter, so the
\ inline and out-of-line forms and the code ceiling of a spilled name have one
\ owner.
package DEFWRITE

public

\ Refuse, through `full`, a name longer than DNAME-INL whose 4-aligned copy at
\ CP would reach the code ceiling, REGION - $4000 above DBASE. The comparisons
\ are unsigned and never wrap: a CP already at the ceiling, and a length no
\ region holds (a negative one among them), are refused rather than added.
\ x12 = the length. Clobbers x11, x15 and the flags.
: NAME-ROOM ( label -- ) {: full:label :}
   LBL {: fits:label :}
   12 DNAME-INL CMPI,  C-LS fits BCOND,
   11 REGION $4000 - LIT64,  11 DBASE 11 ADD,
   CP 11 CMP,  C-CS full BCOND,                       \ CP is already at the ceiling
   11 11 CP SUB,                                      \ x11 = the room below it
   12 11 CMP,  C-CS full BCOND,                       \ the bytes alone reach it
   15 12 3 ADDI,  15 15 2 LSRI,  15 15 2 LSLI,
   15 11 CMP,  C-CS full BCOND,                       \ so do the padded bytes
   fits LBL, ;

\ Store record x9's name, x12 >= 1 bytes at x10, in room NAME-ROOM admitted:
\ inline at [24], or at CP (moved) with DNAME-EXT. Clobbers x10 x11 x13-x16.
: NAME-COPY ( -- )
   LBL LBL LBL LBL LBL LBL {: short lcopy lcd scopy scd done :}
   11 DATA REGION-ALIAS:DELTA-CELL LDR,  11 9 11 ADD,   \ x11 = record x9 through the alias
   14 0 MOVZ,  14 11 24 STR,  14 11 32 STR,           \ canonical name/padding before either form
   13 12 0 ADDI,
   12 DNAME-INL CMPI,  C-LS short BCOND,
      14 DNAME-EXT LIT64,  13 13 14 ORR,  13 11 16 STR,
      15 12 3 ADDI,  15 15 2 LSRI,  15 15 2 LSLI,
      16 CP 15 ADD,
      CP 11 24 STR,
      11 DATA REGION-ALIAS:DELTA-CELL LDR,  11 CP 11 ADD,   \ the copy goes through the alias
      14 12 0 ADDI,
      lcopy LBL,  14 lcd CBZ,
         15 10 0 LDRB,  15 11 0 STRB,
         10 10 1 ADDI,  11 11 1 ADDI,  14 14 1 SUBI,  lcopy B,
      lcd LBL,
      CP 16 0 ADDI,
      done B,
   short LBL,
      13 11 16 STR,
      11 11 24 ADDI,  14 12 0 ADDI,
      scopy LBL,  14 scd CBZ,
         15 10 0 LDRB,  15 11 0 STRB,
         10 10 1 ADDI,  11 11 1 ADDI,  14 14 1 SUBI,  scopy B,
      scd LBL,
   done LBL, ;

: NAME-STORE ( label -- )
   NAME-ROOM  NAME-COPY ;

;package

package ENGINE-EMIT
: C-STORE-NAME ( -- )
   LBL LBL {: fail done :}
   10 DATA TKA-CELL LDR,  12 DATA TKL-CELL LDR,
   fail DEFWRITE:NAME-STORE
   done B,
   fail LBL,                                          \ long name would overflow the code region: the SAME capacity and exit code its sibling definer arms report, so it takes the same labeled tail instead of writing a bare token (rc 76 = $4C, recoverable inside evaluate, fail-closed exit at top level)
      C-DIE-CODE-FULL
   done LBL, ;

: C-QUALIFY-FAIL ( n -- ) {: rc:n :}                  \ qualified-name misuse ($4B) / dict-full ($4D) during C-QUALIFY-DEF: recoverable inside evaluate, fail-closed exit rc at top level
   0 2 MOVZ,  1 DATA DEF-TKA-CELL LDR,  2 DATA DEF-TKL-CELL LDR,  NR-WRITE SYS,
   0 rc MOVZ,  LCOMPILEDIE LABEL@ B, ;
;package

\ LDOESPATCH patches the created word's RET into `b D`, D inside the defining
\ word's body: a raw PC-relative branch the AOT seed cannot relocate (a capture
\ whose definer lies outside the window branched into another word). A record
\ of its own makes D a record
\ entry the seed resolves by name like every BL; a name plus byte delta is
\ refused, since a captured branch must not depend on a callee's interior.
\ The record overlaps its parent's span (`;` writes the parent's length past
\ the clause, as EXPORT overlaps records) and trims nothing: the inliner, the
\ snapshot and the size census read that span.
\ The name is the parent's plus `;does`: the parent's name is the only identity
\ two engines spell alike (a `:` would make it package-qualified). Its bytes
\ are out of line (DNAME-EXT) in the gap between the parent's exit and the
\ clause entry, code the parent branches over, which is why the `adr` that
\ computes D is assembled here.
\ The WID is the parent's, so the clause is as public as its word. The slot is
\ &dict[NDICT+1], above the parent's pending slot; one publish counts both and
\ every abandon path leaves both uncounted.
package DOES-REC

\ SUF-LEN and COPY-NAME's five byte stores spell src/core/does-clause.f
\ DOES-CLAUSE:SUFFIX$; this emitter runs before that word exists, so the two
\ change together.
5 constant SUF-LEN                                     \ ";does"

public

\ x11 = the parent's pending record, x13 = the clause name's length, x14 = the
\ parent's name bytes, x15 = the clause name's length rounded up to a word.
: NAME$ ( -- )
   LBL {: inl:label :}
   11 DATA PEND-CELL LDR,
   12 11 16 LDR,
   13 12 0 ADDI,  13 13 DNAME-FLAG-BITS LSLI,  13 13 DNAME-FLAG-BITS LSRI,       \ the parent's own name length
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
   9 DICT-CAP 1 - LIT64,  NDICT 9 CMP,  C-LT ndok BCOND,
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

\ Copy the derived name to x10, padded to x15 bytes. NAME$ has already supplied
\ x11..x15 and the caller owns the padded span.
: COPY-NAME ( -- )
   LBL LBL LBL LBL {: cpy:label cpd:label pad:label pend:label :}
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
   pend LBL, ;

\ Write the clause record in the existing pending+1 slot. Entry and length are
\ runtime registers so the legacy and native compilers share the exact layout.
: RECORD ( n n -- )
   {: entry:n len:n :}
   14 NDICT 1 ADDI,  9 DREC MOVZ,  14 14 9 MUL,  14 DBASE 14 ADD,
   9 DATA REGION-ALIAS:DELTA-CELL LDR,  14 14 9 ADD,   \ x14 = the slot through the alias
   9 0 MOVZ,                                           \ a slot an abandoned definition may have written
   9 14 0 STR,  9 14 8 STR,  9 14 16 STR,
   9 14 24 STR,  9 14 32 STR,  9 14 40 STR,
   entry 14 0 STR,                                     \ [0] = the clause entry
   len 14 8 STR,                                       \ [8] = its recorded body length
   9 DNAME-EXT LIT64,  9 13 9 ORR,  9 14 16 STR,       \ [16] = its name length, out of line
   9 CP 0 ADDI,  9 14 24 STR,                          \ [24] = the name bytes
   11 DATA PEND-CELL LDR,  9 11 40 LDR,  9 14 40 STR, ; \ [40] = the parent's wordlist

\ The legacy compiler places the clause right after the name bytes and fills
\ its length at `;`.
: MAKE ( -- )
   NAME$
   10 DATA REGION-ALIAS:DELTA-CELL LDR,  10 CP 10 ADD,  COPY-NAME   \ through the alias
   5 CP 15 ADD,  6 0 MOVZ,  5 6 RECORD
   CP CP 15 ADD, ;

\ A live word already holding the clause's name in the parent's wordlist
\ refuses the definer, as C-REJECT-DUP-DEF refuses that word when it comes
\ second: a wordlist keeps at most one live row per folded name, the rule
\ habu1.f WLFIND's hash probe rests on. J-DOES calls this at `does>`, before
\ anything is emitted. The name is
\ derived on the machine stack, in room rounded to keep sp 16-byte aligned,
\ because it is written nowhere else yet: the legacy compiler copies it to CP
\ after the opener, the native one at publication.
: REJECT-DUP ( -- )
   LBL {: fresh:label :}
   NAME$
   9 13 15 ADDI,  9 9 4 LSRI,  9 9 4 LSLI,             \ x9 = the room
   10 SP 0 ADDI,  10 10 9 SUB,  SP 10 0 ADDI,
   COPY-NAME
   0 SP 0 ADDI,  1 13 0 ADDI,  2 11 40 LDR,            \ the name in the parent's wordlist
   WLFIND:LENTRY LABEL@ BL,                            \ keeps x0-x2 and x13
   12 fresh CBZ,
      C-DUP-DEF-SAY
      0 2 MOVZ,  1 SP 0 ADDI,  2 13 0 ADDI,  NR-WRITE SYS,
      0 $4E MOVZ,  LCOMPILEDIE LABEL@ B,               \ its recovery restores sp
   fresh LBL,
   9 13 15 ADDI,  9 9 4 LSRI,  9 9 4 LSLI,
   10 SP 0 ADDI,  10 10 9 ADD,  SP 10 0 ADDI, ;

\ Native publication has already emitted the clause. Append only the permanent
\ derived name and record, using the measured entry and length it supplies.
: NATIVE-PRIM ( -- )
   B G-POP  A G-POP
   5 A 0 ADDI,  6 B 0 ADDI,
   NAME$
   10 DATA REGION-ALIAS:DELTA-CELL LDR,  10 CP 10 ADD,  COPY-NAME   \ through the alias
   5 6 RECORD
   9 1 MOVZ,  9 DATA CLAUSE-CELL STR,                  \ an imported definer had no `does>`
   CP CP 15 ADD, ;

\ An export of a definer publishes a second record of the clause's entry under
\ the clause's name, one slot above the export's own: the pair a definer defined
\ in that wordlist has. Without it a word made through a private definer's
\ public alias branches into a clause only the private wordlist names, which
\ the AOT seed (it qualifies through PUBLIC wordlists only) cannot resolve.
\ `undefine` retires the pair together (xref.f XREF-RETIRE-INDEX).

\ x<a> = the name bytes of record x<rec>, x<u> = their length; x<t> is
\ clobbered. The four registers differ.
: NAME-OF, ( n n n n -- ) {: rec:n a:n u:n t:n :}
   LBL {: inl:label :}
   u rec 16 LDR,  u u DNAME-FLAG-BITS LSLI,  u u DNAME-FLAG-BITS LSRI,
   a rec 24 ADDI,
   t rec 16 LDR,  t t DNAME-EXT ANDI,  t inl CBZ,
      a rec 24 LDR,                                    \ ... which is a pointer when it is long
   inl LBL, ;

\ Write the name of record x<rec> to fd 2; x0-x2 and x14 are clobbered.
: NAME-SAY, ( n -- ) {: rec:n :}
   rec 1 2 14 NAME-OF,  0 2 MOVZ,  NR-WRITE SYS, ;

private

\ Fold byte x<r> from A-Z to a-z, as the duplicate walls compare; x<t> is
\ clobbered.
: FOLD, ( n n -- ) {: r:n t:n :}
   t r $41 SUBI,  t $1A CMPI,  t C-CC CSET,  t t 5 LSLI,  r r t ORR, ;

\ x<dst> = the bytes code span x<raw> covers (code-span.f BYTES): an exact span
\ its body, a historical one its body and the RET slot past it. x<t> is
\ clobbered.
: SPAN-BYTES, ( n n n -- ) {: dst:n raw:n t:n :}
   LBL {: exact:label :}
   dst raw CODE-SPAN:MASK ANDI,
   t raw CODE-SPAN:FULL ANDI,  t exact CBNZ,
      dst dst 4 ADDI,
   exact LBL, ;

public

\ x5 = a record; x6 = the record of its clause, or 0. Record k+1 is k's clause
\ when it is all four that xref.f XREF-DOES-COMPANION? asks: below NDICT, in k's
\ wordlist, named k's name with `;does` added, and starting inside k's body and
\ ending with it. The body test tells the clause from a word someone named
\ X;does. x5 survives; x7-x10, x12 and x14-x16 are clobbered.
: CLAUSE-OF ( -- )
   LBL LBL LBL LBL {: cmp:label sfx:label no:label done:label :}
   6 5 DREC ADDI,
   7 DREC MOVZ,  7 NDICT 7 MUL,  7 DBASE 7 ADD,
   6 7 CMP,  C-CS no BCOND,                            \ no record above
   7 5 40 LDR,  8 6 40 LDR,  7 8 CMP,  C-NE no BCOND,  \ another wordlist
   7 5 0 LDR,  8 6 0 LDR,  8 7 CMP,  C-LS no BCOND,    \ not past the parent's entry
   9 5 8 LDR,  10 9 14 SPAN-BYTES,  7 7 10 ADD,
   9 6 8 LDR,  10 9 14 SPAN-BYTES,  8 8 10 ADD,
   8 7 CMP,  C-HI no BCOND,                            \ ends past the parent's body
   5 10 12 14 NAME-OF,  6 14 15 9 NAME-OF,
   9 12 SUF-LEN ADDI,  9 15 CMP,  C-NE no BCOND,
   7 0 MOVZ,
   cmp LBL,  7 12 CMP,  C-GE sfx BCOND,                \ the parent's name ...
      8 10 7 ADD,  8 8 0 LDRB,  8 9 FOLD,
      16 14 7 ADD,  16 16 0 LDRB,  16 9 FOLD,
      8 16 CMP,  C-NE no BCOND,
      7 7 1 ADDI,  cmp B,
   sfx LBL,  16 14 12 ADD,                             \ ... then `;does`
   8 16 0 LDRB,  8 9 FOLD,  8 $3B CMPI,  C-NE no BCOND,   \ ';'
   8 16 1 LDRB,  8 9 FOLD,  8 $64 CMPI,  C-NE no BCOND,   \ 'd'
   8 16 2 LDRB,  8 9 FOLD,  8 $6F CMPI,  C-NE no BCOND,   \ 'o'
   8 16 3 LDRB,  8 9 FOLD,  8 $65 CMPI,  C-NE no BCOND,   \ 'e'
   8 16 4 LDRB,  8 9 FOLD,  8 $73 CMPI,  C-NE no BCOND,   \ 's'
   done B,
   no LBL,  6 0 MOVZ,
   done LBL, ;

\ The clause record's own length, by the parent's rule: both bodies end at the
\ shared epilogue. It runs BEFORE the
\ parent's write, which leaves x9 live for the cache flush.
: FLUSH ( -- )
   LBL {: none:label :}
   9 DATA DOESB-CELL LDR,  9 none CBZ,
   11 NDICT 1 ADDI,  12 DREC MOVZ,  11 11 12 MUL,  11 DBASE 11 ADD,
   9 11 0 LDR,  10 CP 9 SUB,  10 10 4 SUBI,
   12 DATA REGION-ALIAS:DELTA-CELL LDR,  12 11 12 ADD,  10 12 8 STR,   \ through the alias
   none LBL, ;

;package

package ENGINE-EMIT
public
: C-CLEAR-TRUSTED-STATE ( -- )
   9 0 MOVZ,
   9 DATA NCOMP-DISPATCH:DEF-TIER-CELL STR,
   9 DATA NCOMP-DISPATCH:CALLBACK-ENTRY-CELL CELL + NATIVE-BODY-ABI:PENDING-TAKE cells + STR,
   9 DATA PEND-OPAQUE-CELL STR,
   9 DATA TSIG-A-CELL STR,   9 DATA TSIG-U-CELL STR,
   9 DATA TCSIG-A-CELL STR,  9 DATA TCSIG-U-CELL STR,
   9 DATA DOESB-CELL STR,  9 DATA CLAUSE-CELL STR,
   9 DATA TRUSTED-CELL STR, ;

\ No local outlives its definition. Wherever one ends, published or abandoned,
\ its locals' count, frame and block depth clear with their token-start marks,
\ so the next definition starts with none and no image holds them.
: C-CLEAR-LOCALS ( -- )
   9 0 MOVZ,
   9 DATA LOCN-CELL STR,  9 DATA LOCF-CELL STR,  9 DATA CAPDEPTH-CELL STR,
   9 DATA TOKLOCN-CELL STR,  9 DATA TOKDEPTH-CELL STR, ;

private
: B-NATIVE-PENDING-OPAQUE ( -- )
   9 DATA PEND-OPAQUE-CELL LDR,  9 G-PUSH ;
;package

\ The bodies of the fourteen rows src/habu/prims.f specifies under "the
\ definition writers" and of the six replay writers after them, registered
\ with ENGINE-PRIMS:GLOBAL-INT-WID in EMIT-PRIMITIVE-SECTIONS. Four lie further
\ on: def-abort's and stack-clear's under "the recovery rows", def-cast's past
\ the ends of a publish it shares with `cast:`, and imm-mark's is the
\ `immediate` keyword's own, C-IMMEDIATE. Each
\ checks everything before it writes, and a refusal exits the process, so no
\ reader ever sees a half-written row.
\ FPRIM-WID frames x30 around each body.
package DEFWRITE

: REFUSE-AT ( label n -- ) {: at:label rc:n :}
   at LBL,  0 rc MOVZ,  NR-EXIT-GROUP SYS, ;

\ x<dst> = the address of record x<ix>; dst and ix differ.
: REC-AT, ( n n -- ) {: dst:n ix:n :}
   dst DREC MOVZ,  dst ix dst MUL,  dst DBASE dst ADD, ;

\ A pending definition's record is slot NDICT, the slot a record writer fills.
: NOT-PENDING, ( label -- ) {: bad:label :}
   9 DATA PEND-CELL LDR,  9 bad CBNZ, ;

\ Slot DICT-CAP is the control-flow band, not a record.
: DICT-ROOM, ( label -- ) {: bad:label :}
   9 DICT-CAP LIT64,  NDICT 9 CMP,  C-CS bad BCOND, ;

\ An empty name, and one whose copy would reach the code ceiling. x1 = the
\ length; x12 takes a copy for NAME-ROOM.
: NAME-SIZE, ( label -- ) {: bad:label :}
   1 bad CBZ,
   12 1 0 ADDI,  bad NAME-ROOM ;

\ A live record for the folded name x0/x1 in wordlist x2. WLFIND keeps x0, x1,
\ x2 and x13, and clobbers x3-x12 and x14-x17.
: FRESH, ( label -- ) {: bad:label :}
   WLFIND:LENTRY LABEL@ BL,  12 bad CBNZ, ;

\ Wid x2 is one of wl.f's WL markers, not a wordlist: every wordlist id
\ is at least 0.
: REAL-WID, ( label -- ) {: bad:label :}
   2 0 CMPI,  C-LT bad BCOND, ;

\ After the seal, wid x2 is protected. LPROTWIDQ keeps x0-x4 and answers in x13.
: OPEN-WID, ( label -- ) {: prot:label :}
   LBL {: open:label :}
   9 DATA SEAL-NDICT-CELL LDR,  9 open CBZ,
   9 2 0 ADDI,  LPROTWIDQ LABEL@ BL,  13 prot CBNZ,
   open LBL, ;

\ Count record NDICT and index it.
: PUBLISH, ( -- )
   EM-DEF-OCC:LAPPEND LABEL@ BL,  LHIDXADD LABEL@ BL, ;

\ x<r> = the replay scope band (src/habu/layout.f REPLAY-SCOPE); its cells are
\ reached as offsets from that base (BAND-OFF).
: BAND, ( n -- ) {: r:n :}
   r REPLAY-SCOPE:LATCH LIT64,  r DATA r ADD, ;

\ A band cell's offset from BAND,'s base.
: BAND-OFF ( n -- n ) REPLAY-SCOPE:LATCH - ;

\ While an overlay is open, raise its high-water mark to NDICT: the records the
\ overlay published are [the saved NDICT, HW). Clobbers x8 and x14.
: HW-RAISE, ( -- )
   LBL {: keep:label :}
   8 BAND,
   14 8 REPLAY-SCOPE:LATCH BAND-OFF LDR,  14 keep CBZ,
   14 8 REPLAY-SCOPE:HW BAND-OFF LDR,  NDICT 14 CMP,  C-LS keep BCOND,
   NDICT 8 REPLAY-SCOPE:HW BAND-OFF STR,
   keep LBL, ;

\ One scope cell, at off from x<base>, into band cell `cell` at x8 when save is
\ true and back out of it when it is false. Clobbers x14.
: SAVED, ( n n n bool -- ) {: base:n off:n cell:n save:bool :}
   save if
      14 base off LDR,  14 8 cell BAND-OFF STR,
   else
      14 8 cell BAND-OFF LDR,  14 base off STR,
   then ;

\ The scope replay-open saves and replay-close restores, beside NDICT and CP:
\ WIDN, CURRENT, the open package's four cells, the enclosing package's four
\ and the using band's depth, the depth `package` saved and the used wids.
\ USE-RPKG-SAVE-CELL is not in the scope. x8 = the band. Clobbers
\ x10 and x14.
: SCOPE, ( bool -- ) {: save:bool :}
   DATA WIDN-CELL REPLAY-SCOPE:WIDN save SAVED,
   DATA CUR-CELL REPLAY-SCOPE:CUR save SAVED,
   DATA PKG-PUB-CELL REPLAY-SCOPE:PKG-PUB save SAVED,
   DATA PKG-PRI-CELL REPLAY-SCOPE:PKG-PRI save SAVED,
   DATA PKG-PARENT-CELL REPLAY-SCOPE:PKG-PARENT save SAVED,
   DATA PKG-REC-CELL REPLAY-SCOPE:PKG-REC save SAVED,
   ENCLOSING:END ENCLOSING:PUB - 0 do
      DATA ENCLOSING:PUB i +  REPLAY-SCOPE:ENCLOSING i +  save SAVED,
   CELL +loop
   10 USE-BAND-OFF LIT64,  10 DATA 10 ADD,
   10 USE-DEPTH-CELL USE-BAND-OFF - REPLAY-SCOPE:USE-DEPTH save SAVED,
   10 USE-PKG-SAVE-CELL USE-BAND-OFF - REPLAY-SCOPE:USE-PKG-SAVE save SAVED,
   USE-MAX 0 ?do
      10 USE-WIDS-OFF USE-BAND-OFF - i cells +  REPLAY-SCOPE:USE-WIDS i cells +  save SAVED,
   loop ;

\ Zero the cells [x10, x11). Clobbers x10 and x15.
: ZERO-SPAN, ( -- )
   LBL LBL {: next:label done:label :}
   15 0 MOVZ,
   next LBL,  10 11 CMP,  C-CS done BCOND,
      15 10 0 STR,  10 10 8 ADDI,  next B,
   done LBL, ;

public

\ The entry of every replay record: executing one is an error the record has no
\ code to report, so the trap reports it and exits 76.
variable LREPLAYTRAP

: EMIT-REPLAY-TRAP ( -- )
   LBL  S\" hb: replay record executed\n"  {: msg:label ma mu :}
   LREPLAYTRAP LABEL@ LBL,
   0 2 MOVZ,  1 msg ADR,  2 mu MOVZ,  NR-WRITE SYS,
   0 76 MOVZ,  NR-EXIT-GROUP SYS,
   msg LBL,  ma mu BYTES, ;

\ namespace-record ( ptr u8 n bool -- n )
\ A name holds at most one colon, never at an edge: a part's path `A:B`.
: NAMESPACE-RECORD ( -- )
   LBL LBL LBL LBL LBL LBL {: bad scan next clean nopri done :}
   B-TASK-GUARD
   13 G-POP  1 G-POP  0 G-POP                         \ x13 = the flag, x1 = the length, x0 = the name
   bad NOT-PENDING,
   bad DICT-ROOM,
   bad NAME-SIZE,
   14 0 MOVZ,  16 0 MOVZ,                             \ x16 = a colon seen
   scan LBL,  14 1 CMP,  C-CS clean BCOND,
      15 0 14 ADD,  15 15 0 LDRB,  15 $3A CMPI,  C-NE next BCOND,
         16 bad CBNZ,  14 bad CBZ,
         15 14 1 ADDI,  15 1 CMP,  C-CS bad BCOND,
         16 1 MOVZ,
   next LBL,  14 14 1 ADDI,  scan B,
   clean LBL,
   2 DATA BOOT:NAMESPACE-WL-CELL LDR,
   bad FRESH,
   10 0 0 ADDI,  12 1 0 ADDI,  3 13 0 ADDI,           \ the name, the flag
   9 NDICT REC-AT,
   NAME-COPY
   11 DATA REGION-ALIAS:DELTA-CELL LDR,  11 9 11 ADD,   \ x11 = the record through the alias
   14 DATA WIDN-CELL LDR,  14 11 0 STR,                \ [0] = a fresh wid
   14 14 1 ADDI,  15 0 MOVZ,
   3 nopri CBZ,
      15 14 0 ADDI,  14 14 1 ADDI,                    \ [8] = a second one
   nopri LBL,
   15 11 8 STR,  14 DATA WIDN-CELL STR,
   15 DATA BOOT:NAMESPACE-WL-CELL LDR,  15 11 40 STR,
   PUBLISH,
   HW-RAISE,                                          \ an overlay's row dies at replay-close
   9 NDICT 1 SUBI,  9 G-PUSH
   done B,
   bad ENGINE-ERROR:SEAL-VIOLATION REFUSE-AT
   done LBL, ;

\ namespace-private ( n -- )
: NAMESPACE-PRIVATE ( -- )
   LBL LBL {: bad done :}
   B-TASK-GUARD
   OVERLAY-EMIT:GUARD,
   0 G-POP
   0 NDICT CMP,  C-CS bad BCOND,
   9 0 REC-AT,
   14 9 40 LDR,  14 0 CMPI,  C-GE bad BCOND,             \ a live package row
   15 WL:RETIRED invert MOVN,  14 15 CMP,  C-EQ bad BCOND,
   15 WL:RETIRED-NAMESPACE invert MOVN,  14 15 CMP,  C-EQ bad BCOND,
   14 9 8 LDR,  14 bad CBNZ,
   11 DATA REGION-ALIAS:DELTA-CELL LDR,  11 9 11 ADD,   \ x11 = the record through the alias
   14 DATA WIDN-CELL LDR,  14 11 8 STR,
   14 14 1 ADDI,  14 DATA WIDN-CELL STR,
   done B,
   bad ENGINE-ERROR:SEAL-VIOLATION REFUSE-AT
   done LBL, ;

\ alias-record ( ptr u8 n n n -- ) name, source index, wid
: ALIAS-RECORD ( -- )
   LBL LBL LBL LBL {: bad prot notval done :}
   B-TASK-GUARD
   OVERLAY-EMIT:GUARD,
   2 G-POP  4 G-POP  1 G-POP  0 G-POP                 \ x2 = the wid, x4 = the source, x1/x0 = the name
   bad REAL-WID,
   prot OPEN-WID,
   bad NOT-PENDING,
   4 NDICT CMP,  C-CS bad BCOND,
   5 4 REC-AT,
   14 5 40 LDR,  14 0 CMPI,  C-LT bad BCOND,           \ a package row or a retired record has no live body
   14 5 16 LDR,  14 14 DNAME-INT ANDI,  14 bad CBNZ,  \ an internal body: the alias would drop its gate
   bad DICT-ROOM,
   bad NAME-SIZE,
   13 4 0 ADDI,
   bad FRESH,
   3 2 0 ADDI,  4 13 0 ADDI,  10 0 0 ADDI,  12 1 0 ADDI,
   9 NDICT REC-AT,
   NAME-COPY
   11 DATA REGION-ALIAS:DELTA-CELL LDR,  11 9 11 ADD,   \ x11 = the record through the alias
   5 4 REC-AT,
   14 5 0 LDR,  14 11 0 STR,                           \ [0] = the source's entry
   14 5 8 LDR,  14 11 8 STR,                           \ [8] = its recorded length
   16 5 16 LDR,
   15 DNAME-IMM DNAME-WIDE or DNAME-MIN-IN-MASK or LIT64,  14 16 15 AND,
   15 DKIND:MASK LIT64,  16 16 15 AND,
   15 DKIND:VAL LIT64,  16 15 CMP,  C-NE notval BCOND,
      14 14 15 ORR,                                    \ only an immutable value keeps its fixed kind
   notval LBL,
   15 9 16 LDR,  15 15 14 ORR,  15 11 16 STR,          \ its IMM, WIDE, MIN-IN and fixed-value bits
   3 11 40 STR,
   PUBLISH,
   done B,
   bad ENGINE-ERROR:SEAL-VIOLATION REFUSE-AT
   prot ENGINE-ERROR:SEAL-PACKAGE REFUSE-AT
   done LBL, ;

\ package-scope! ( n n -- ): src/habu/prims.f states its contract. The package
\ keywords hold its task guard (C-TASK-GUARD).
: PACKAGE-SCOPE ( -- )
   LBL LBL LBL LBL LBL LBL LBL LBL {: bad set inline scan part top store done :}
   1 G-POP  0 G-POP                                   \ x1 = the parent wid, x0 = the row
   15 0 MOVN,  0 15 CMP,  C-NE set BCOND,
      1 bad CBNZ,                                     \ only `-1 0` clears
      9 0 MOVZ,
      9 DATA PKG-PUB-CELL STR,  9 DATA PKG-PRI-CELL STR,
      9 DATA PKG-PARENT-CELL STR,  9 DATA PKG-REC-CELL STR,
      9 DATA ENCLOSING:PUB STR,  9 DATA ENCLOSING:PRI STR,
      9 DATA ENCLOSING:PARENT STR,  9 DATA ENCLOSING:REC STR,
      done B,
   set LBL,
   0 NDICT CMP,  C-CS bad BCOND,
   9 0 REC-AT,
   14 9 40 LDR,  14 0 CMPI,  C-GE bad BCOND,             \ a live package row
   15 WL:RETIRED invert MOVN,  14 15 CMP,  C-EQ bad BCOND,
   15 WL:RETIRED-NAMESPACE invert MOVN,  14 15 CMP,  C-EQ bad BCOND,
   12 9 8 LDR,  12 bad CBZ,
   2 9 16 LDR,  2 2 DNAME-FLAG-BITS LSLI,  2 2 DNAME-FLAG-BITS LSRI,   \ x2 = the name's length
   4 9 24 ADDI,
   14 9 16 LDR,  14 14 DNAME-EXT ANDI,  14 inline CBZ,
      4 9 24 LDR,                                     \ x4 = the name's bytes
   inline LBL,
   3 0 MOVZ,
   scan LBL,  3 2 CMP,  C-CS top BCOND,
      15 4 3 ADD,  15 15 0 LDRB,  15 $3A CMPI,  C-EQ part BCOND,
      3 3 1 ADDI,  scan B,
   part LBL,
      14 DATA PKG-PUB-CELL LDR,  14 bad CBZ,          \ no package open
      14 DATA ENCLOSING:PUB LDR,  14 bad CBNZ,        \ a part is open
      14 DATA PKG-PUB-CELL LDR,  14 DATA ENCLOSING:PUB STR,
      14 DATA PKG-PRI-CELL LDR,  14 DATA ENCLOSING:PRI STR,
      14 DATA PKG-PARENT-CELL LDR,  14 DATA ENCLOSING:PARENT STR,
      14 DATA PKG-REC-CELL LDR,  14 DATA ENCLOSING:REC STR,
      store B,
   top LBL,
      14 0 MOVZ,
      14 DATA ENCLOSING:PUB STR,  14 DATA ENCLOSING:PRI STR,
      14 DATA ENCLOSING:PARENT STR,  14 DATA ENCLOSING:REC STR,
   store LBL,
   11 9 0 LDR,
   11 DATA PKG-PUB-CELL STR,  12 DATA PKG-PRI-CELL STR,
   1 DATA PKG-PARENT-CELL STR,  9 DATA PKG-REC-CELL STR,
   done B,
   bad ENGINE-ERROR:SEAL-VIOLATION REFUSE-AT
   done LBL, ;

\ The six rows src/habu/prims.f types only for package CHECKER-OVERLAY.
\ replay-open saves the scope into the REPLAY-SCOPE band and replay-close puts
\ it back, so no caller hands the engine a state to restore; the one mark a
\ caller hands, replay-widn!'s, the band bounds below. While the latch is
\ set, replay-record and namespace-record raise HW as they publish. The owner
\ publishes through nothing else, nothing while a definition is pending, and
\ lowers NDICT only by its own rollback, so [the saved NDICT, HW) is the
\ overlay's and a record above HW is another writer's. Every other record or
\ wid writer dies while the latch is set (habu1.f OVERLAY-EMIT:GUARD,), so a
\ definition pends in an open overlay only through a raw store to PEND-CELL,
\ which reaches the pending-definition refusals below, as a raw ndict! reaches
\ the HW one.

\ replay-open ( -- )
: REPLAY-OPEN ( -- )
   LBL LBL {: bad done :}
   B-TASK-GUARD
   8 BAND,
   14 8 REPLAY-SCOPE:LATCH BAND-OFF LDR,  14 bad CBNZ,   \ an overlay is open
   bad NOT-PENDING,
   NDICT 8 REPLAY-SCOPE:NDICT BAND-OFF STR,
   CP 8 REPLAY-SCOPE:CP BAND-OFF STR,
   0 0= SCOPE,
   NDICT 8 REPLAY-SCOPE:HW BAND-OFF STR,
   14 1 MOVZ,  14 8 REPLAY-SCOPE:LATCH BAND-OFF STR,
   done B,
   bad ENGINE-ERROR:SEAL-VIOLATION REFUSE-AT
   done LBL, ;

\ replay-record ( ptr u8 n n -- ) name, wid: a codeless record, whose entry
\ is the trap. It needs no OPEN-WID: it can only trap, and replay-close
\ removes it.
: REPLAY-RECORD ( -- )
   LBL LBL {: bad done :}
   B-TASK-GUARD
   2 G-POP  1 G-POP  0 G-POP                          \ x2 = the wid, x1/x0 = the name
   bad REAL-WID,
   8 BAND,
   14 8 REPLAY-SCOPE:LATCH BAND-OFF LDR,  14 bad CBZ,   \ no overlay
   bad NOT-PENDING,
   bad DICT-ROOM,
   bad NAME-SIZE,
   bad FRESH,
   3 2 0 ADDI,  10 0 0 ADDI,  12 1 0 ADDI,
   9 NDICT REC-AT,
   NAME-COPY
   11 DATA REGION-ALIAS:DELTA-CELL LDR,  11 9 11 ADD,   \ x11 = the record through the alias
   14 LREPLAYTRAP LABEL@ ADR,  14 11 0 STR,            \ [0] = the trap
   14 0 MOVZ,  14 11 8 STR,
   3 11 40 STR,
   PUBLISH,
   HW-RAISE,
   done B,
   bad ENGINE-ERROR:SEAL-VIOLATION REFUSE-AT
   done LBL, ;

\ record-wid! ( n n -- ) wid, index: the overlay retires a record with wid -2
\ and later gives it back the wid it logged, in reverse order. The wid cell is
\ half the record's hash key, and a compaction between the two re-keys the
\ retired record on RETIRED's chain, so after each store the record is indexed
\ under the wid it now holds (HIDX-EMIT:LREKEY, src/habu/wl.f).
\ Between a restore and replay-close one wordlist can hold two live records of
\ one name, the restored record and a replay record, and the hash probe and
\ the wordlist scan may answer with different ones. The owner ends every such
\ window before any lookup (src/core/checker.f CHECKER-OVERLAY): CLOSE's
\ restore with replay-close, a frame ROLLBACK's with the ndict! that drops
\ the replay record.
: RECORD-WID ( -- )
   LBL LBL LBL LBL {: bad wid store done :}
   B-TASK-GUARD
   3 G-POP  13 G-POP                                  \ x3 = the index, x13 = the wid
   8 BAND,
   14 8 REPLAY-SCOPE:LATCH BAND-OFF LDR,  14 bad CBZ,   \ no overlay
   3 NDICT CMP,  C-CS bad BCOND,
   9 3 REC-AT,
   14 9 40 LDR,  14 0 CMPI,  C-GE wid BCOND,             \ a live package row
   15 WL:RETIRED invert MOVN,  14 15 CMP,  C-EQ wid BCOND,
   15 WL:RETIRED-NAMESPACE invert MOVN,  14 15 CMP,  C-NE bad BCOND,
   wid LBL,
   13 0 CMPI,  C-GE store BCOND,                         \ a live package row's marker
   15 WL:RETIRED invert MOVN,  13 15 CMP,  C-EQ store BCOND,
   15 WL:RETIRED-NAMESPACE invert MOVN,  13 15 CMP,  C-NE bad BCOND,
   store LBL,
   14 DATA REGION-ALIAS:DELTA-CELL LDR,  14 9 14 ADD,  13 14 40 STR,   \ through the alias
   HIDX-EMIT:LREKEY LABEL@ BL,
   done B,
   bad ENGINE-ERROR:SEAL-VIOLATION REFUSE-AT
   done LBL, ;

\ replay-private ( n bool -- ) namespace index, flag: true gives the row a
\ fresh private wid, as `package` gives one to a row a qualified definition
\ made (src/habu/interpreter.f PKG-REOPEN); false takes back the one it gave.
\ replay-close puts WIDN back but nothing of a row below the saved NDICT, so
\ the owner logs each grant and takes it back before the close.
: REPLAY-PRIVATE ( -- )
   LBL LBL LBL {: bad give done :}
   B-TASK-GUARD
   13 G-POP  3 G-POP                                  \ x13 = the flag, x3 = the index
   8 BAND,
   14 8 REPLAY-SCOPE:LATCH BAND-OFF LDR,  14 bad CBZ,   \ no overlay
   3 NDICT CMP,  C-CS bad BCOND,
   9 3 REC-AT,
   14 9 40 LDR,  14 0 CMPI,  C-GE bad BCOND,             \ a live package row
   15 WL:RETIRED invert MOVN,  14 15 CMP,  C-EQ bad BCOND,
   15 WL:RETIRED-NAMESPACE invert MOVN,  14 15 CMP,  C-EQ bad BCOND,
   14 9 8 LDR,
   13 give CBNZ,
      15 8 REPLAY-SCOPE:WIDN BAND-OFF LDR,  14 15 CMP,  C-CC bad BCOND,
      15 DATA REGION-ALIAS:DELTA-CELL LDR,  15 9 15 ADD,   \ x15 = the record through the alias
      14 0 MOVZ,  14 15 8 STR,
      done B,
   give LBL,
   14 bad CBNZ,
   15 DATA REGION-ALIAS:DELTA-CELL LDR,  15 9 15 ADD,   \ x15 = the record through the alias
   14 DATA WIDN-CELL LDR,  14 15 8 STR,
   14 14 1 ADDI,  14 DATA WIDN-CELL STR,
   done B,
   bad ENGINE-ERROR:SEAL-VIOLATION REFUSE-AT
   done LBL, ;

\ replay-close ( -- ): every check comes before the first store. It refuses
\ NDICT below the saved count or above HW, CP below the saved CP and WIDN below
\ the saved WIDN; then it zeroes the overlay's records, puts NDICT, CP and the
\ scope back and zeroes the band, the latch with it, and its enclosing copy.
\ While a task is live it retires the records instead and keeps NDICT and CP
\ (EM-DEF-OCC:LRETIRE).
\ The WIDN refusal is defensive: every engine writer of WIDN but replay-widn!
\ raises it, replay-widn! lowers it no further than the saved WIDN, and a raw
\ store to WIDN-CELL or the band exits 83 after the seal. It keeps a WIDN the
\ overlay did not leave from being restored over, as the NDICT and CP checks do.
: REPLAY-CLOSE ( -- )
   LBL LBL LBL {: bad kept done :}
   B-TASK-GUARD
   8 BAND,
   14 8 REPLAY-SCOPE:LATCH BAND-OFF LDR,  14 bad CBZ,   \ no overlay
   bad NOT-PENDING,
   4 8 REPLAY-SCOPE:NDICT BAND-OFF LDR,  NDICT 4 CMP,  C-CC bad BCOND,
   5 8 REPLAY-SCOPE:HW BAND-OFF LDR,  NDICT 5 CMP,  C-HI bad BCOND,
   6 8 REPLAY-SCOPE:CP BAND-OFF LDR,  CP 6 CMP,  C-CC bad BCOND,
   14 8 REPLAY-SCOPE:WIDN BAND-OFF LDR,  15 DATA WIDN-CELL LDR,
   15 14 CMP,  C-CC bad BCOND,
   6 GUARD-CODE-WORD
   9 4 0 ADDI,  EM-DEF-OCC:LRETIRE LABEL@ BL,  14 kept CBNZ,   \ a task live: retired, NDICT and CP kept
   EM-DEF-OCC:LCOUNT LABEL@ BL,  10 11 LASTC-TRIM,
   CP 6 0 ADDI,
   10 DREC MOVZ,  11 5 10 MUL,  10 4 10 MUL,
   10 DBASE 10 ADD,  11 DBASE 11 ADD,                 \ records [x10, x11)
   12 DATA REGION-ALIAS:DELTA-CELL LDR,  10 10 12 ADD,  11 11 12 ADD,   \ the records through the alias
   ZERO-SPAN,
   kept LBL,
   8 BAND,
   0 0<> SCOPE,
   10 8 0 ADDI,  11 8 REPLAY-SCOPE:END BAND-OFF ADDI,
   ZERO-SPAN,
   10 8 REPLAY-SCOPE:ENCLOSING BAND-OFF ADDI,
   11 10 ENCLOSING:END ENCLOSING:PUB - ADDI,
   ZERO-SPAN,
   done B,
   bad ENGINE-ERROR:SEAL-VIOLATION REFUSE-AT
   done LBL, ;

\ replay-widn! ( n -- ): put WIDN back to N, a mark the owner read while the
\ overlay was open, as a rollback inside the overlay drops what it made since
\ the mark (src/core/checker.f CHECKER-OVERLAY ROLLBACK). The records ndict!
\ drops and the private wids replay-private takes back held every wid at or
\ above N, so none is handed out twice. It refuses no overlay, N below the
\ saved WIDN, which would hand out again a wid from before the overlay, and N
\ above WIDN, both compared unsigned. A mark is never below the saved WIDN, so
\ replay-close's WIDN check keeps holding.
: REPLAY-WIDN ( -- )
   LBL LBL {: bad done :}
   B-TASK-GUARD
   13 G-POP                                           \ x13 = the mark
   8 BAND,
   14 8 REPLAY-SCOPE:LATCH BAND-OFF LDR,  14 bad CBZ,   \ no overlay
   14 8 REPLAY-SCOPE:WIDN BAND-OFF LDR,  13 14 CMP,  C-CC bad BCOND,
   15 DATA WIDN-CELL LDR,  13 15 CMP,  C-HI bad BCOND,
   13 DATA WIDN-CELL STR,
   done B,
   bad ENGINE-ERROR:SEAL-VIOLATION REFUSE-AT
   done LBL, ;

\ def-open ( ptr u8 n n n -- ) name, wid, kind
: DEF-OPEN ( -- )
   LBL LBL LBL {: bad prot done :}
   B-TASK-GUARD
   OVERLAY-EMIT:GUARD,
   4 G-POP  2 G-POP  1 G-POP  0 G-POP                 \ x4 = the kind, x2 = the wid, x1/x0 = the name
   14 DKIND:MASK DNAME-CELL or DNAME-ALLOC or invert LIT64,  14 4 14 AND,  14 bad CBNZ,
   bad REAL-WID,
   prot OPEN-WID,
   bad NOT-PENDING,
   9 REGION $4000 - LIT64,  9 DBASE 9 ADD,  CP 9 CMP,  C-CS bad BCOND,
   bad DICT-ROOM,
   bad NAME-SIZE,
   13 4 0 ADDI,
   bad FRESH,
   3 2 0 ADDI,  4 13 0 ADDI,  10 0 0 ADDI,  12 1 0 ADDI,
   9 NDICT REC-AT,
   NAME-COPY
   11 DATA REGION-ALIAS:DELTA-CELL LDR,  11 9 11 ADD,   \ x11 = the record through the alias
   CP 11 0 STR,                                       \ [0] = the entry, past a long name
   14 0 MOVZ,  14 11 8 STR,
   14 9 16 LDR,  14 14 4 ORR,  14 11 16 STR,          \ the kind beside the length
   3 11 40 STR,
   9 DATA PEND-CELL STR,  14 DATA TKA-CELL LDR,  14 DATA PENDTKA-CELL STR,
   14 0 MOVZ,
   14 DATA NCOMP-DISPATCH:CALLBACK-ENTRY-CELL CELL + NATIVE-BODY-ABI:PENDING-TAKE cells + STR,
   14 DATA TSIG-A-CELL STR,   14 DATA TSIG-U-CELL STR,
   14 DATA TCSIG-A-CELL STR,  14 DATA TCSIG-U-CELL STR,
   14 DATA PEND-OPAQUE-CELL STR,
   14 DATA DOESB-CELL STR,    14 DATA TRUSTED-CELL STR,
   14 DATA CLAUSE-CELL STR,
   14 TIER-LATCH  14 DATA NCOMP-DISPATCH:DEF-TIER-CELL STR,
   done B,
   bad ENGINE-ERROR:SEAL-VIOLATION REFUSE-AT
   prot ENGINE-ERROR:SEAL-PACKAGE REFUSE-AT
   done LBL, ;

\ body-append ( ptr u8 n -- ): LBCS is the capture `:` appends through; it grows
\ the body text block (habu1.f BCAP-GROW) as every capture does.
: BODY-APPEND ( -- )
   LBL LBL {: bad done :}
   12 G-POP  11 G-POP                                 \ LBCS: x11 = the bytes, x12 = their count
   14 DATA BODYLEN-CELL LDR,  15 DATA BODY-TEXT:CAP-CELL LDR,
   14 15 CMP,  C-HI bad BCOND,                        \ BODYLEN is already past the text capacity
   15 0 MOVN,  15 15 14 SUB,                          \ x15 = 2^64 - 1 - BODYLEN
   12 15 CMP,  C-CS bad BCOND,                        \ BODYLEN + u + 1 wraps
   LBCS LABEL@ BL,
   done B,
   bad ENGINE-ERROR:SEAL-VIOLATION REFUSE-AT
   done LBL, ;

\ ( ptr u8 n -- ): the span into the friend-arena cells aoff and uoff while a
\ definition is pending.
: PENDING-SPAN ( n n -- ) {: aoff:n uoff:n :}
   LBL LBL {: bad done :}
   12 G-POP  11 G-POP
   9 DATA PEND-CELL LDR,  9 bad CBZ,
   11 DATA aoff STR,  12 DATA uoff STR,
   done B,
   bad ENGINE-ERROR:SEAL-VIOLATION REFUSE-AT
   done LBL, ;

\ trust-sig! ( ptr u8 n -- )
: TRUST-SIG ( -- )
   TSIG-A-CELL TSIG-U-CELL PENDING-SPAN ;

\ created-sig! ( ptr u8 n -- )
: CREATED-SIG ( -- )
   TCSIG-A-CELL TCSIG-U-CELL PENDING-SPAN ;

\ The close def-close runs; PEND-CELL clears last.
: NATIVE-CLOSE ( -- )
   LBL {: noaccept:label :}
   11 DATA NCOMP-DISPATCH:CALLBACK-ENTRY-CELL CELL + NATIVE-BODY-ABI:PENDING-TAKE cells + LDR,
   11 noaccept CBZ,
      SP SP 16 SUBI,  11 SP 8 STR,
      9 0 MOVZ,
      9 DATA NCOMP-DISPATCH:CALLBACK-ENTRY-CELL CELL + NATIVE-BODY-ABI:PENDING-TAKE cells + STR,
      10 DATA BODY-TEXT:BASE-CELL LDR,  10 G-PUSH
      10 DATA BODYLEN-CELL LDR,  10 G-PUSH
      11 SP 8 LDR,
      C-CALL-X11-SAVED
      SP SP 16 ADDI,
   noaccept LBL,
   C-CLEAR-TRUSTED-STATE
   C-CLEAR-LOCALS
   9 0 MOVZ,  9 DATA PEND-CELL STR, ;

\ def-close ( -- )
: DEF-CLOSE ( -- )
   LBL LBL LBL {: bad done nolast :}
   9 DATA PEND-CELL LDR,  9 bad CBZ,
   10 DATA NCOMP-DISPATCH:DEF-TIER-CELL LDR,  10 bad CBZ,
   10 9 16 LDR,  10 10 DKIND:MASK ANDI,  10 nolast CBZ,   \ a published body that pushes a cell is the slot `does>` patches (LDOESPATCH), set after the body as the kernel's definers set it
   11 DKIND:CAST LIT64,  10 11 CMP,  C-EQ nolast BCOND,
   9 DATA LASTC-CELL STR,
   nolast LBL,
   NATIVE-CLOSE
   done B,
   bad ENGINE-ERROR:SEAL-VIOLATION REFUSE-AT
   done LBL, ;

\ The end of the def-create row, for record
\ x9 in slot NDICT: [0] is CP, DP rounds up to a cell under DP-CHECK,
\ COMMA-BASE records that body start, the code pushes it as an address-map site
\ and returns, [8] is its length and [16] carries DKIND:ADDR, LASTC is the
\ record, which is counted and indexed, its code is native and flushed.
: ADDR-TAIL, ( -- )
   11 DATA REGION-ALIAS:DELTA-CELL LDR,  11 9 11 ADD,  CP 11 0 STR,   \ through the alias
   11 DATA 0 LDR,  11 11 7 ADDI,  11 11 3 LSRI,  11 11 3 LSLI,   \ standard CREATE: round the data field up to a cell
   11 DP-CHECK  11 DATA 0 STR,
   11 DATA COMMA-BASE-CELL STR,
   C-DATA-ADDR
   9 W-RET LIT64,  LCEMIT LABEL@ BL,
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   11 DATA REGION-ALIAS:DELTA-CELL LDR,  11 9 11 ADD,   \ x11 = the record through the alias
   10 9 0 LDR,  10 CP 10 SUB,  10 10 4 SUBI,  10 11 8 STR,
   10 9 16 LDR,  10 10 DKIND:ADDR ORRI,
   10 10 DNAME-ALLOC ORRI,  10 11 16 STR,  \ the DATA allocation starts at its leading carrier
   9 DATA LASTC-CELL STR,
   EM-DEF-OCC:LAPPEND LABEL@ BL,  LHIDXADD LABEL@ BL,  9 9 0 LDR,   \ publish record NDICT-1; x9 = body start for the flush
   LFLUSH LABEL@ BL, ;

\ The native compiler's fixed shadow callback, when one is installed
\ (src/compiler/native/compiler.f SHADOW-FIXED), learns the created record
\ ADDR-TAIL, just published, NDICT - 1, and the DATA address its body pushes.
: ADDR-SHADOW, ( -- )
   LBL {: noshadow:label :}
   11 DATA NCOMP-DISPATCH:FIXED-SHADOW-CELL LDR,  11 noshadow CBZ,
   9 DATA DP-CELL LDR,  9 G-PUSH
   9 COMP-BODY:address COMP:BODY>N MOVZ,  9 G-PUSH
   9 NDICT 0 ADDI,  9 9 1 SUBI,  9 G-PUSH
   11 DATA NCOMP-DISPATCH:FIXED-SHADOW-CELL LDR,  C-CALL-X11-SAVED
   noshadow LBL, ;

private

\ The end of a definition with no compiled body, the def-create and def-cast
\ rows alike: the pending record must be of kind `kind`, with CP below the
\ code ceiling. `tail` writes the row's code, then the state def-open set
\ clears, PEND-CELL last.
: TAIL-CLOSE ( n [ -- ] -- )
   {: kind:n tail :}
   LBL LBL {: bad done :}
   9 DATA PEND-CELL LDR,  9 bad CBZ,
   9 NDICT REC-AT,                                    \ the pending record, slot NDICT
   10 9 16 LDR,  10 10 DKIND:MASK ANDI,  11 kind LIT64,  10 11 CMP,  C-NE bad BCOND,
   10 REGION $4000 - LIT64,  10 DBASE 10 ADD,  CP 10 CMP,  C-CS bad BCOND,
   tail execute
   C-CLEAR-TRUSTED-STATE
   9 0 MOVZ,  9 DATA PEND-CELL STR,
   done B,
   bad ENGINE-ERROR:SEAL-VIOLATION REFUSE-AT
   done LBL, ;

public

\ def-create ( -- ): a pending DKIND:ADDR record takes ADDR-TAIL, and its
\ shadow, as `create` does.
: DEF-CREATE ( -- ) DKIND:ADDR [: ADDR-TAIL,  ADDR-SHADOW, ;] TAIL-CLOSE ;

;package

package ENGINE-EMIT
: J-DOES ( -- )
   LBL LBL LBL {: p1:label sigd:label marked:label :}
   DOES-REC:REJECT-DUP
   C-CF-NONE                             \ a structure open across the split would branch into the clause
   \ Pass 2 re-reads a frozen source that holds no clause signature: it places
   \ the copy FREEZE kept, as pass 1's may lie in the code pass 2 rewrites.
   \ DOESB stands from pass 1, and the lowering transaction moves from the
   \ head's certificate to the clause's.
   9 DATA P2-CELL LDR,  9 p1 CBZ,
      LP2CLAUSE LABEL@ BL,
      11 DATA TCSIG-A-CELL LDR,  10 DATA TCSIG-U-CELL LDR,
      sigd B,
   p1 LBL,
   C-CREATED-SIG$
   sigd LBL,
   C-PLACE-CREATED-SIG
   C-EMIT-CRSIG-SET
   \ The head's locals end here. Their frame is dropped ahead of the opener, so
   \ word 4 stays one word, and the scope is emptied once nothing below can
   \ refuse: the clause cannot name a head local, and the shared `;` drops only
   \ the frame the clause opens itself. No quotation is open: C-CF-NONE.
   12 DATA LOCF-CELL LDR,  C-EMIT-DROP-X12
   DOES-REC:NAME$  DOES-REC:ROOM
   \ DOESB marks the clause only once nothing above can refuse: set before the
   \ signature, a refusal caught inside an immediate word's evaluate left it on
   \ a definition with no clause, and its `;` refused the `does>` (rc 70).
   9 DATA P2-CELL LDR,  9 marked CBNZ,
   9 DATA BODYLEN-CELL LDR,  9 DATA DOESB-CELL STR,
   9 1 MOVZ,  9 DATA CLAUSE-CELL STR,
   marked LBL,
   9 0 MOVZ,  9 DATA LOCN-CELL STR,  9 DATA LOCF-CELL STR,
   DOES-REC:ENTRY-ADR                    \ adr x10, D = past word 4 and the name bytes
   16 20 DOESP-CELL W-LDRX C-EMITW       \ x16 = LDOESPATCH runtime addr
   $D63F0200 C-EMITW                     \ blr x16
   J-EXIT                                \ word 4: the defining word ends here
   DOES-REC:MAKE                         \ the name bytes, then the clause's record
   \ D: a fresh entry slot for the does-body, opened as ALREADY HAVING CALLED.
   \ The clause and the defining word share one epilogue -- word 4's J-EXIT is
   \ chained to the same `;` -- and the defining word reached it through the
   \ `blr x16` just above, so its frame is standing when control arrives. A leaf
   \ clause must therefore still be given the restore, and both entry slots must
   \ keep their saves. Re-pointing the cell is what keeps the defining word's
   \ slot: EM-COMPILE-RET only ever patches the one the cell names.
   9 CP 0 ADDI,  9 9 1 ORRI,  9 DATA FRAME-CELL STR,
   9 W-LINKSAVE LIT64,  LCEMIT LABEL@ BL, ;

: C-QUALIFY-CAP ( -- )
   LBL {: room :}
   14 DICT-CAP LIT64,  NDICT 14 CMP,  C-LT room BCOND,
      LDICTFULL DICTFULL-MSG$ nip C-CAP-LABEL
      $4D C-QUALIFY-FAIL
   room LBL, ;

: C-DUP-DEF-FAIL ( -- )                               \ duplicate definition: recoverable inside evaluate (rc $4E), fail-closed exit $4E at top level
   C-DUP-DEF-SAY
   0 2 MOVZ,  1 DATA DEF-TKA-CELL LDR,  2 DATA DEF-TKL-CELL LDR,  NR-WRITE SYS,
   0 $4E MOVZ,  LCOMPILEDIE LABEL@ B, ;

\ A definition in the global wid is also refused by a row of its name in
\ WL:PRIMITIVES. Clobbers x0-x17 and x30.
: C-REJECT-DUP-DEF ( -- )
   LBL LBL LBL LBL LBL LBL LBL LBL LBL
   {: nloop:label nnext:label ncmp:label nmatch:label nend:label ninl:label done:label nlin:label fresh:label :}
   14 DATA HIDXP-CELL LDR,  14 nlin CBZ,  C-HIDX-DUP?  13 nmatch CBNZ,  done B,  nlin LBL,  5 DBASE 0 ADDI,  6 NDICT 0 ADDI,
   nloop LBL,
      6 nend CBZ,
      14 5 40 LDR,  15 DATA DEF-WL-CELL LDR,  14 15 CMP,  C-NE nnext BCOND,
      14 5 16 LDR,  14 14 DNAME-FLAG-BITS LSLI,  14 14 DNAME-FLAG-BITS LSRI,
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
   done LBL,
   15 DATA DEF-WL-CELL LDR,  14 DATA BOOT:GLOBAL-WL-CELL LDR,  15 14 CMP,  C-NE fresh BCOND,
   0 DATA TKA-CELL LDR,  1 DATA TKL-CELL LDR,  2 WL:PRIMITIVES MOVZ,
   WLFIND:LENTRY LABEL@ BL,
   12 nmatch CBNZ,
   fresh LBL, ;

\ Sealed system-package guard: the shared fail writes `E-SEALED-PACKAGE: `, the
\ offending token (in TKA/TKL) and its location, and exits
\ ENGINE-ERROR:SEAL-PACKAGE at any evaluate depth. Both guards read
\ FRIEND-LATCH-CELL: latch 0 (engine cold load) allows a reserved name, sealed
\ user source is refused. RESTAB and the fold are native for the reason
\ RESTAB-BUF states.
: C-SEAL-PACKAGE-FAIL ( -- )
   0 2 MOVZ,  1 LSEALED LABEL@ ADR,  2 SEALED-MSG-LEN MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   LSRCAT LABEL@ BL,
   0 ENGINE-ERROR:SEAL-PACKAGE MOVZ,  NR-EXIT-GROUP SYS, ;

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

public
\ A head LHEADROW resolves to a part, whose row is named by the longer path
\ `A:head`, names no sealed package. Clobbers x2-x9, x11-x17, x24 and x25.
: C-QUALIFY-SEAL-GUARD ( -- )   \ reject `NAME:tail` defs into a sealed system package
   LBL LBL LBL LBL {: scan:label have:label match:label ok:label :}
   9 DATA SEAL-NDICT-CELL LDR,  9 ok CBZ,               \ namespace open -> no guard
   \ Only a NAME:tail token (first ':' not at an edge, matching CHECKER-QUALIFIED?)
   \ can name a package; a leading/trailing ':' (e.g. `PRIM:`) is an ordinary name.
   25 DATA TKL-CELL LDR,  24 0 MOVZ,
   scan LBL,
      24 25 CMP,  C-GE ok BCOND,                        \ no ':' -> ordinary -> skip
      9 DATA TKA-CELL LDR,  9 9 24 ADD,  9 9 0 LDRB,
      9 $3A CMPI,  C-EQ have BCOND,                      \ first ':' at index x24
      24 24 1 ADDI,  scan B,
   have LBL,
      24 ok CBZ,                                         \ leading ':' -> ordinary -> skip
      9 24 1 ADDI,  9 25 CMP,  C-GE ok BCOND,            \ trailing ':' -> ordinary -> skip
      9 DATA TKA-CELL LDR,  17 24 0 ADDI,
      SP SP 16 SUBI,  30 SP 0 STR,
      LHEADROW LABEL@ BL,
      30 SP 0 LDR,  SP SP 16 ADDI,
      12 match CBZ,
      2 12 16 LDR,  2 2 DNAME-FLAG-BITS LSLI,  2 2 DNAME-FLAG-BITS LSRI,
      2 24 CMP,  C-NE ok BCOND,                          \ a part's row -> no sealed package
   match LBL,
      C-SEAL-MATCH                                       \ prefix len = x24; fail if reserved
   ok LBL, ;

private

public
variable LQUALIFYDEF      \ shared qualification helper entry
variable LSTOREDEFNAME    \ shared guarded-name-publication helper entry

private

public
: C-QUALIFY-DEF ( -- )  LQUALIFYDEF LABEL@ BL, ;

private

public
: C-STORE-DEF-NAME ( -- )  LSTOREDEFNAME LABEL@ BL, ;

\ Reader keywords that need an operand share this diagnostic. LTOK's zero
\ result means the stream ended: the previous TKA/TKL bytes are no name, so the
\ caller supplies the baked keyword label.
: C-DIE-KEYWORD-NAME ( label n -- ) {: kw:label kwlen:n :}
   0 2 MOVZ,  1 KWDATA:LKEYNONAME LABEL@ ADR,  2 KWDATA:KEYNONAME-LEN MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 kw ADR,  2 kwlen MOVZ,  NR-WRITE SYS,
   0 $4A MOVZ,  LCOMPILEDIE LABEL@ B, ;

private

public
\ Interpret-mode defining-word handlers. The interpret define-keyword dispatch
\ rows reopen this package further down and resolve them bare.
variable LTOPHOOK

: C-TOPHOOK-LIT ( n -- ) {: class:n :}     \ literal/keyword event: flags 0
   LBL {: nohk:label :}
   9 DATA TOP-HOOK-CELL LDR,  9 nohk CBZ,
   15 class MOVZ,  16 0 MOVZ,
   LTOPHOOK LABEL@ BL,
   nohk LBL, ;
;package

package INTERP-EMIT

: C-CREATE ( -- )
   C-TASK-GUARD
   15 1 MOVZ,  LCREATE LABEL@ BL, ;

\ The variable's cell is written zero: DP may come back over space a released
\ DP gave back (habu1.f DP-LOWER,), and the Habu loop's `variable` reaches
\ the same cell through `allot`, which zeroes it.
: C-VARIABLE ( -- )  C-CREATE
   9 DATA LASTC-CELL LDR,
   11 DATA REGION-ALIAS:DELTA-CELL LDR,  11 9 11 ADD,   \ x11 = the record through the alias
   10 9 16 LDR,  10 10 DNAME-CELL ORRI,  10 11 16 STR,
   10 DATA 0 LDR,  7 10 8 ADDI,  7 DP-CHECK  31 10 0 STR,  7 DATA 0 STR, ;

\ `constant` pops one cell and records the trust `-- a`: the interpret stack is
\ untyped, and the DNAME-WIDE dispatch keeps wider values off it. interpreter.f
\ DEF-CONSTANT, verify-source.f and public-signatures-core.f record the same.
: C-CONSTANT ( -- )
   C-TASK-GUARD
   LTOK LABEL@ BL,
   LBL {: named:label :}
   0 named CBNZ,
      LKWCONST LABEL@ KWCONST$ nip C-DIE-KEYWORD-NAME
   named LBL,
   12 0 MOVZ,  12 DATA BODYLEN-CELL STR,  LBCAP LABEL@ BL,   \ seed "NAME " for the hook
   C-QUALIFY-DEF
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   C-STORE-DEF-NAME
   15 G-POP                                             \ n -> x15 after name storage (clobbers x15)
   SP SP 16 SUBI,  15 SP 0 STR,                         \ and kept for the shadow below
   10 DATA REGION-ALIAS:DELTA-CELL LDR,  10 9 10 ADD,  CP 10 0 STR,   \ through the alias
   11 15 0 ADDI,  C-LIT
   9 W-RET LIT64,  LCEMIT LABEL@ BL,
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   11 DATA REGION-ALIAS:DELTA-CELL LDR,  11 9 11 ADD,   \ x11 = the record through the alias
   10 9 0 LDR,  10 CP 10 SUB,  10 10 4 SUBI,  10 11 8 STR,
   10 9 16 LDR,  10 10 DKIND:VAL ORRI,  10 11 16 STR,      \ this record's body pushes a decided number
   9 DATA LASTC-CELL STR,
   EM-DEF-OCC:LAPPEND LABEL@ BL,  LHIDXADD LABEL@ BL,  9 9 0 LDR,   \ publish record NDICT-1; x9 = body start for the flush
   LFLUSH LABEL@ BL,
   \ The native compiler's fixed shadow callback, when one is installed
   \ (src/compiler/native/compiler.f SHADOW-FIXED), learns the published record,
   \ NDICT - 1, and the number its body pushes, as DEFWRITE:ADDR-SHADOW, does
   \ for `create`.
   LBL {: noshadow:label :}
   9 SP 0 LDR,  SP SP 16 ADDI,
   11 DATA NCOMP-DISPATCH:FIXED-SHADOW-CELL LDR,  11 noshadow CBZ,
   9 G-PUSH
   9 COMP-BODY:value COMP:BODY>N MOVZ,  9 G-PUSH
   9 NDICT 0 ADDI,  9 9 1 SUBI,  9 G-PUSH
   11 DATA NCOMP-DISPATCH:FIXED-SHADOW-CELL LDR,  C-CALL-X11-SAVED
   noshadow LBL,
   LKWCONST KWCONST$ nip C-DEFHOOK
   LASTC-TRUST:PUBLISH-A ;

;package

package ENGINE-EMIT
public
: C-PARSE-REQUIRED-SIG ( -- )
   LBL LBL {: done bad :}
   bad C-SIG-START
   bad C-SIG-END
   C-SIG-CAPTURE-TSIG
   done B,
   bad LBL,  C-SIG-BAD
   done LBL, ;

private
\ C-TRUSTED has set PEND and PENDTKA before its signature, so an input that
\ ends before the signature closes is the source ending inside the definition,
\ refused through LSRCEND. C-SIG-START also misses at a byte
\ other than `(`, below INE: that is a bad signature. CAST: and defer take
\ C-PARSE-REQUIRED-SIG before they set PEND.
: C-PARSE-TRUST-SIG ( -- )
   LBL LBL LBL LBL {: done:label bad:label start:label ended:label :}
   start C-SIG-START
   ended C-SIG-END
   C-SIG-CAPTURE-TSIG
   done B,
   start LBL,  11 12 CMP,  C-LT bad BCOND,
   ended LBL,
      9 DATA PENDTKA-CELL LDR,  9 DATA INP-CELL STR,
      LSRCEND LABEL@ B,
   bad LBL,  C-SIG-BAD
   done LBL, ;

public
\ The second end of every definition's publish, emitted by the `;` tail below
\ and by the cast's empty body; it sits this early because a cast publishes
\ inside its keyword handler, which the interpret dispatch rows name. The first
\ end is EM-COMPILE-RET.
\ A body ending in its return takes the legacy length, which leaves the return
\ out; one `;` closed with a tail call (exact true) takes its whole extent as an
\ exact span (CODE-SPAN:FULL).
: EM-COMPILE-FLUSH-PEND ( bool -- )
   {: exact:bool :}
   DOES-REC:FLUSH
   11 DATA PEND-CELL LDR,
   9 11 0 LDR,  10 CP 9 SUB,
   exact if 10 10 CODE-SPAN:FULL ORRI, else 10 10 4 SUBI, then
   12 DATA REGION-ALIAS:DELTA-CELL LDR,  12 11 12 ADD,  10 12 8 STR,   \ through the alias
   LFLUSH LABEL@ BL, ;
;package

\ The cast's body and the def-cast row join the definition writers, past the
\ two ends of a publish they emit.
package DEFWRITE
public

\ The empty checked body of a cast, for record x9 pending in slot NDICT: [0] is
\ CP, the entry slot a leaf leaves a nop, then a return; the code is native,
\ [8] takes its length and the code is flushed. The end of `cast:` (C-IDENTITY) and of the
\ def-cast row alike, as ADDR-TAIL, is of `create` and def-create.
: EMPTY-BODY, ( -- )
   10 DATA REGION-ALIAS:DELTA-CELL LDR,  10 9 10 ADD,  CP 10 0 STR,   \ through the alias
   9 CP 0 ADDI,  9 DATA FRAME-CELL STR,              \ the entry slot
   9 W-LINKSAVE LIT64,  LCEMIT LABEL@ BL,            \ str x30,[sp,#-16]!
   EM-COMPILE-RET                                    \ empty body: the slot becomes a nop
   9 DATA PEND-CELL LDR,  9 9 0 LDR,
   false EM-COMPILE-FLUSH-PEND ;

;package

\ The cast declarer joins the other interpret-mode defining-word handlers in
\ their package: the define-keyword dispatch rows below reopen it and resolve
\ them bare.
package INTERP-EMIT

\ `cast:` with no following token. It names the cause, then the token it went
\ wrong on, then a newline — DEFER-DIAG's shape, and rc $4A for DEFER-DIAG's
\ reason: a reader keyword that needed a name and reached the end of the stream.
\ Recoverable inside evaluate, fail-closed exit at top level; it fires before any
\ dictionary write, so rollback drops only compile state.
: C-CAST-DIE-NO-NAME ( -- )
   0 2 MOVZ,  1 KWDATA:LCASTNONAME LABEL@ ADR,  2 KWDATA:CASTNONAME-LEN MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 $4A MOVZ,  LCOMPILEDIE LABEL@ B, ;

\ `cast: NAME ( in -- out )`, the checked retype declarer, and `linear: NAME
\ ( in -- out )`, a DEFLINEAR owner's mint or erase, are reader keywords: a word
\ would have to read its name and signature off the live input and evaluate
\ them, which nothing loaded before src/core/include.f can do.
\ A declaration, not a definition: the form completes inside this handler and
\ PEND-CELL is cleared before the return, so no engine state crosses a token.
\ The empty checked colon body keeps first-class execution and the
\ minimum-input guard; its DKIND:CAST stamp, owned by this declarer, tells both
\ compilers a direct mention is identity data flow (no body scan establishes it).
\ The registrar runs before the record is counted: CHECKER-DEFCAST and
\ CHECKER-LINEAR throw their refusal (checker.f package CAST-ERROR), and a
\ refused declaration leaves no callable name.
\ EM-REC-WIDE-PUBLISH pokes the latched facts into the record, for a cast its
\ minimum input arity: without it a bare `>ROLE` at an empty stack reads below
\ the interpret base (test/cast-negative-suite.f pins the refusal).
\ The registrar is asked with or without a hook, since it is a cast's whole
\ check. With no owner record or operation,
\ REGISTER-IDENTITY calls nothing. The drain stays hook-guarded: it finds its
\ two checker words by global name; the latches it would read are stored by
\ value at the next record (checker.f RECW, RECMI).
\ `off` selects the registrar and `noname` the missing-name diagnostic.
\ C-QUALIFY-DEF can allocate a package record; a LINEAR: row may never target
\ that package's public wordlist, so its certifier is asked before the
\ allocation. It keeps the payload, owner, scope refusal order; the no-hook boot
\ path exits 70 publishing nothing.
: C-LINEAR-QUALIFIED-GUARD ( -- )
   LBL LBL LBL LBL LBL {: scan:label first:label tail:label qualified:label done:label :}
   12 DATA TKL-CELL LDR,  13 0 MOVZ,
   scan LBL,
      13 12 CMP,  C-GE done BCOND,
      11 DATA TKA-CELL LDR,  11 11 13 ADD,  11 11 0 LDRB,
      11 $3A CMPI,  C-EQ first BCOND,
      13 13 1 ADDI,  scan B,
   first LBL,
      13 done CBZ,
      13 13 1 ADDI,  13 12 CMP,  C-GE done BCOND,
   tail LBL,
      11 DATA TKA-CELL LDR,  11 11 13 ADD,  11 11 0 LDRB,
      11 $3A CMPI,  C-EQ done BCOND,
      13 13 1 ADDI,  13 12 CMP,  C-LT tail BCOND,
   qualified LBL,
      NCOMP-DISPATCH:DECL-LINEAR-OFF DEF-TRUST:REGISTER-IDENTITY
      70 C-DIE-TOKEN
   done LBL, ;

public
: C-IDENTITY-SHADOW ( -- )
   LBL {: noshadow:label :}
   11 DATA NCOMP-DISPATCH:FIXED-SHADOW-CELL LDR,  11 noshadow CBZ,
   9 0 MOVZ,  9 G-PUSH
   9 COMP-BODY:cast COMP:BODY>N MOVZ,  9 G-PUSH
   9 NDICT 0 ADDI,  9 9 1 SUBI,  9 G-PUSH
   11 DATA NCOMP-DISPATCH:FIXED-SHADOW-CELL LDR,  C-CALL-X11-SAVED
   noshadow LBL, ;
private

: C-IDENTITY ( [ -- ] n -- )
   {: noname off:n :}
   C-TASK-GUARD
   LBL LBL LBL {: cpok:label ndok:label named:label :}
   9 REGION $4000 - LIT64,  9 DBASE 9 ADD,  CP 9 CMP,  C-LT cpok BCOND,
      C-DIE-CODE-FULL
   cpok LBL,
   9 DICT-CAP LIT64,  NDICT 9 CMP,  C-LT ndok BCOND,
      C-DIE-DICT-FULL
   ndok LBL,
   LTOK LABEL@ BL,  0 named CBNZ,
      noname execute
   named LBL,
   12 0 MOVZ,  12 DATA BODYLEN-CELL STR,
   LBCAP LABEL@ BL,                                  \ "NAME " — the spelling the registrar reads
   C-CLEAR-TRUSTED-STATE
   C-PARSE-REQUIRED-SIG                              \ malformed or missing sig dies named (rc 76)
   off NCOMP-DISPATCH:DECL-LINEAR-OFF = IF C-LINEAR-QUALIFIED-GUARD THEN
   C-QUALIFY-DEF
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   9 DATA PEND-CELL STR,
   C-STORE-DEF-NAME
   10 9 16 LDR,  10 10 DKIND:CAST ORRI,
   11 DATA REGION-ALIAS:DELTA-CELL LDR,  11 9 11 ADD,  10 11 16 STR,   \ through the alias
   DEFWRITE:EMPTY-BODY,
   off DEF-TRUST:REGISTER-IDENTITY
   EM-DEF-OCC:LAPPEND LABEL@ BL,  LHIDXADD LABEL@ BL,
   EM-REC-WIDE-PUBLISH
   C-IDENTITY-SHADOW
   C-CLEAR-TRUSTED-STATE
   9 0 MOVZ,  9 DATA PEND-CELL STR, ;

: C-CAST ( -- )
   [: C-CAST-DIE-NO-NAME ;] NCOMP-DISPATCH:DECL-CAST-OFF C-IDENTITY ;

: C-LINEAR ( -- )
   [: KWDATA:LKWLINEAR LABEL@ 7 C-DIE-KEYWORD-NAME ;] NCOMP-DISPATCH:DECL-LINEAR-OFF C-IDENTITY ;

;package

\ def-cast ( -- ): a pending DKIND:CAST record takes EMPTY-BODY, and is
\ counted and indexed, and its native shadow follows, as C-IDENTITY's does.
package DEFWRITE
public

: DEF-CAST ( -- )
   DKIND:CAST [: EMPTY-BODY,  EM-DEF-OCC:LAPPEND LABEL@ BL,  LHIDXADD LABEL@ BL,
      INTERP-EMIT:C-IDENTITY-SHADOW ;]
   TAIL-CLOSE ;

;package

package ENGINE-EMIT
: C-DEFER-DIE-TOKEN ( n -- ) {: rc:n :}               \ boot-integrity backstop: the bare token, then exit rc; recoverable inside evaluate, fail-closed exit rc at top level. Its ONE caller is C-DEFER-FIND-UNSET ($46), which fires before its DP alloc, so the eval-frame / REPL rollback drops only compile-state + region prot. It stays token-only on purpose: the token there is the engine's own `defer-unset` primitive name, not a name the source wrote, so there is no source misuse to explain. The four causes that ARE source misuse each name themselves through DEFER-DIAG below. Distinct from the pre-trust boot-integrity backstops (C-PD-DIE-FULL exit 72, BSEALCAP exit 73) which do NOT route here and STAY hard exits.
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 rc MOVZ,  LCOMPILEDIE LABEL@ B, ;
;package

\ Each defer/is misuse cause writes what went wrong, then the token, then a
\ newline. All fire before the defer record is counted into NDICT (C-DEFER
\ pre-publish; C-DEFER-TARGET-META before DEFER-META-CELL is written), so an
\ abort inside `evaluate` rolls back only compile state; C-PD-CAPTURE runs only
\ after the NDICT bump, so no stale pending-table entry is left.
package DEFER-DIAG

\ Cause then token, and NO newline: the LCOMPILEDIE tail this head's callers
\ branch to appends ` at <path>:<line>` and the newline. DIE-NOT-FOUND, whose
\ hint follows on a second line, writes that intervening newline itself.
: DIE-HEAD ( label n -- ) {: msg:label len:n :}
   0 2 MOVZ,  1 msg ADR,  2 len MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS, ;

: DIE-MSG ( label n n -- ) {: msg:label len:n rc:n :}
   msg len DIE-HEAD
   0 rc MOVZ,  LCOMPILEDIE LABEL@ B, ;

public

: DIE-NO-NAME ( -- )                       \ `defer` with no following token
   LDEFNONAME LABEL@ NONAME$ nip $4A DIE-MSG ;

: DIE-NO-TARGET ( -- )                     \ `is` with no following token
   LDEFNOTOKEN LABEL@ NOTOKEN$ nip $4A DIE-MSG ;

: DIE-NOT-DEFER ( -- )                     \ `is` target exists but is not deferred
   LDEFNOTDEFER LABEL@ NOTDEFER$ nip $4C DIE-MSG ;

\ `is` resolves its target through the engine's own lookup, which ignores
\ `using` imports, so a bare tail only a used public exports is not found here
\ and the hint says to qualify it (`is DPKG:DHOOK` works where `is DHOOK` under
\ `using DPKG` dies). A tail claimed by a global and a used public is refused by
\ the checker at the reference (E-USING-SHADOW-GLOBAL, throw 7141).
: DIE-NOT-FOUND ( -- )                     \ `is` target resolves to nothing
   LDEFNOTFOUND LABEL@ NOTFOUND$ nip DIE-HEAD
   0 2 MOVZ,  1 LOPENNL LABEL@ ADR,  2 NL-LEN MOVZ,  NR-WRITE SYS,   \ end the cause line; the hint is the located one
   0 2 MOVZ,  1 LDEFHINT LABEL@ ADR,  2 HINT$ nip MOVZ,  NR-WRITE SYS,
   0 $46 MOVZ,  LCOMPILEDIE LABEL@ B, ;

;package

package ENGINE-EMIT
: C-DEFER-FIND-UNSET ( -- )
   LBL {: found :}
   9 LKWDEFERUNSET LABEL@ ADR,  10 11 MOVZ,  LFIND LABEL@ BL,
   13 found CBNZ,
      $46 C-DEFER-DIE-TOKEN
   found LBL, ;

\ The dispatch cell of a deferred word holds an execution token, which is an
\ address in the JIT region, and it lives in the DP heap, which a snapshot
\ persists byte for byte. Declare it here, where the cell is created and its kind
\ is therefore known, so the snapshot writer and loader can move it with the
\ region.
: C-DEFER-CELL ( -- )
   C-DEFER-FIND-UNSET
   7 DATA DP-CELL LDR,
   7 7 7 ADDI,  7 7 3 LSRI,  7 7 3 LSLI,
   11 7 0 STR,
   7 DATA DEFER-XT-CELL STR,
   7 7 8 ADDI,  7 DP-CHECK  7 DATA DP-CELL STR,
   9 DATA DEFER-XT-CELL LDR,  RELOC-EMIT:LMARK LABEL@ BL, ;

\ The whole body in one emission, frame included: it reaches its target through
\ `blr x16`, so the frame is never the leaf kind and there is nothing for
\ EM-COMPILE-RET to decide. FRAME-CELL is not touched -- this word is emitted
\ from the interpret-mode `defer` declarer, outside any open body.
: C-DEFER-EMIT-CODE ( -- )
   W-LINKSAVE C-EMITW
   11 DATA DEFER-XT-CELL LDR,
   C-DATA-ADDR-RAW                                 \ movz/movk x9 = dispatch-cell addr (relocatable data addr)
   16 9 0 W-LDRX C-EMITW
   16 CP ENC-CMP C-EMITW                           \ habu1.f BCALLABLE's fence: `is` stores an xt
   8 C-CC ENC-BCOND C-EMITW                        \ below CP: past the ISB
   ENC-ISB C-EMITW
   C-CALL-BLR-X16 C-EMITW
   W-LINKREST C-EMITW
   W-RET C-EMITW ;

: C-DEFER-META-WRITE ( -- )
   1 DATA REGION-ALIAS:DELTA-CELL LDR,  1 CP 1 ADD,  \ through the alias
   11 DEFER-MAGIC LIT64,  11 1 0 STR,
   11 DATA DEFER-XT-CELL LDR,  11 1 8 STR,  28 28 16 ADDI, ;

public
\ The end of `defer` and of the def-defer row alike, for record x9 in slot
\ NDICT and the dispatch cell in DEFER-XT-CELL: [0] is CP, the code loads the
\ cell and calls what it holds, [8] takes the code's length, the DEFER-MAGIC
\ trailer naming the cell follows, the code is native and flushed.
: DEFER-BODY, ( -- )
   10 DATA REGION-ALIAS:DELTA-CELL LDR,  10 9 10 ADD,  CP 10 0 STR,   \ through the alias
   C-DEFER-EMIT-CODE
   9 DATA PEND-CELL LDR,
   10 9 0 LDR,  10 CP 10 SUB,  10 10 CODE-SPAN:FULL ORRI,
   11 DATA REGION-ALIAS:DELTA-CELL LDR,  11 9 11 ADD,  10 11 8 STR,   \ through the alias
   C-DEFER-META-WRITE
   9 DATA PEND-CELL LDR,  9 9 0 LDR,
   LFLUSH LABEL@ BL, ;

\ The native compiler's fixed shadow callback, when one is installed
\ (src/compiler/native/compiler.f SHADOW-FIXED), learns the deferred record
\ just published, NDICT - 1, and its dispatch cell.
: DEFER-SHADOW, ( -- )
   LBL {: noshadow:label :}
   11 DATA NCOMP-DISPATCH:FIXED-SHADOW-CELL LDR,  11 noshadow CBZ,
   9 DATA DEFER-XT-CELL LDR,  9 G-PUSH
   9 COMP-BODY:defer COMP:BODY>N MOVZ,  9 G-PUSH
   9 NDICT 0 ADDI,  9 9 1 SUBI,  9 G-PUSH
   11 DATA NCOMP-DISPATCH:FIXED-SHADOW-CELL LDR,  C-CALL-X11-SAVED
   noshadow LBL, ;

private
: C-DEFER-ROOM ( -- )
   LBL LBL {: cpok ndok :}
   9 REGION $4000 - LIT64,  9 DBASE 9 ADD,  CP 9 CMP,  C-LT cpok BCOND,
      C-DIE-CODE-FULL
   cpok LBL,
   9 DICT-CAP LIT64,  NDICT 9 CMP,  C-LT ndok BCOND,
      C-DIE-DICT-FULL
   ndok LBL, ;

\ A `defer NAME ( E )` declared before checker.f defines `trust`/`checker-defer`
\ cannot run DEF-TRUST:REGISTER / C-CALL-CHECKER-DEFER (they LFIND those words and
\ die exit 70). C-DEFER instead copies the defer's qualified name + effect
\ signature into the pending table (src/habu/layout.f PD-*); DRAIN-PRETRUST
\ replays both registrations after `: TRUST`.

: C-PD-COPY ( -- )                                   \ copy x5 bytes [x9..)->[x16..); advances x9/x16, x5->0
   LBL LBL {: top done :}
   top LBL,  5 done CBZ,
      6 9 0 LDRB,  6 16 0 STRB,
      9 9 1 ADDI,  16 16 1 ADDI,  5 5 1 SUBI,  top B,
   done LBL, ;

: C-PD-DIE-FULL ( -- )                               \ table full or name/sig over cap: name the offending defer token, exit 72
   SP SP 32 SUBI,  9 $2D657270203A6268 LIT64,  9 SP 0 STR,  9 $6564207473757274 LIT64,  9 SP 8 STR,  9 $6C62617420726566 LIT64,  9 SP 16 STR,  9 $203A6C6C75662065 LIT64,  9 SP 24 STR,
   0 2 MOVZ,  1 SP 0 ADDI,  2 32 MOVZ,  NR-WRITE SYS,  SP SP 32 ADDI,
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 2 MOVZ,  1 LOPENNL LABEL@ ADR,  2 NL-LEN MOVZ,  NR-WRITE SYS,
   0 72 MOVZ,  NR-EXIT-GROUP SYS, ;

: C-PD-CAPTURE ( -- )                                \ record this defer's (name,sig) into the next pending slot
   LBL LBL {: full done :}
   12 PD-TABLE-OFF LIT64,  12 DATA 12 ADD,           \ x12 = &band
   13 12 0 LDR,  14 PD-CAP MOVZ,  13 14 CMP,  C-GE full BCOND,
   15 PD-SLOT MOVZ,  15 13 15 MUL,                   \ x15 = count*PD-SLOT
   14 12 PD-SLOTS-REL ADDI,  14 14 15 ADD,           \ x14 = slot base (survives C-PUSH-DREC-NAME/copies)
   C-PUSH-DREC-NAME                                  \ G: name-addr, name-len (clobbers x9,x10,x12,x13)
   10 G-POP  9 G-POP                                 \ x10=name-len  x9=name-addr
   16 PD-NAME-CAP MOVZ,  10 16 CMP,  C-HI full BCOND,
   10 14 PD-NLEN-OFF STR,
   16 14 PD-NAME-OFF ADDI,  5 10 0 ADDI,  C-PD-COPY
   9 DATA TSIG-A-CELL LDR,  10 DATA TSIG-U-CELL LDR, \ x9=sig-addr x10=sig-len
   16 PD-SIG-CAP MOVZ,  10 16 CMP,  C-HI full BCOND,
   10 14 PD-SLEN-OFF STR,
   16 14 PD-SIG-OFF ADDI,  5 10 0 ADDI,  C-PD-COPY
   12 PD-TABLE-OFF LIT64,  12 DATA 12 ADD,           \ reload &band (C-PUSH-DREC-NAME clobbered x12)
   13 12 0 LDR,  13 13 1 ADDI,  13 12 0 STR,
   done B,
   full LBL,  C-PD-DIE-FULL
   done LBL, ;

: C-PRETRUST-READY? ( -- )
   LBL {: done:label :}
   13 0 MOVZ,
   NCOMP-DISPATCH:DECL-EFFECT-OFF done DECL-OWNER:TARGET
   NCOMP-DISPATCH:DECL-DEFER-OFF done DECL-OWNER:TARGET
   13 1 MOVZ,
   done LBL, ;

public
: C-DEFER ( -- )
   C-TASK-GUARD
   LBL {: named :}
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
   C-STORE-DEF-NAME
   DEFER-BODY,
   LBL LBL LBL {: ready registered pdone :}
   C-PRETRUST-READY?
   SP SP 16 SUBI,  13 SP 0 STR,                     \ target readiness survives the registrars and publication observer
   13 ready CBNZ,
      NCOMP-DISPATCH:DECL-EFFECT-OFF TSIG-A-CELL TSIG-U-CELL DECL-OWNER:SIGNATURE
      NCOMP-DISPATCH:DECL-DEFER-OFF DECL-OWNER:NAME
      registered B,
   ready LBL,
      DEF-TRUST:REGISTER
      C-CALL-CHECKER-DEFER
   registered LBL,
   \ Accepted rows name the still-pending ordinal before APPEND publishes it.
   9 DATA PEND-CELL LDR,
   EM-DEF-OCC:LAPPEND LABEL@ BL,  LHIDXADD LABEL@ BL,
   13 SP 0 LDR,  SP SP 16 ADDI,
   13 pdone CBNZ,
      C-PD-CAPTURE                                  \ retain the target checker's later registration only after publication
   pdone LBL,
   DEFER-SHADOW,
   EM-REC-WIDE-PUBLISH
   C-CLEAR-TRUSTED-STATE
   9 0 MOVZ,  9 DATA PEND-CELL STR, ;
;package

\ def-defer ( n -- ): a pending kind-0 record takes DEFER-BODY, for dispatch
\ cell n, is counted and indexed, and its fixed shadow follows, as C-DEFER's
\ does. The cell is popped before TAIL-CLOSE checks: its refusals exit, never
\ throw.
package DEFWRITE
public

: DEF-DEFER ( -- )
   11 G-POP  11 DATA DEFER-XT-CELL STR,
   0 [: DEFER-BODY,  EM-DEF-OCC:LAPPEND LABEL@ BL,  LHIDXADD LABEL@ BL,
      DEFER-SHADOW, ;]
   TAIL-CLOSE ;

;package

package ENGINE-EMIT
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
   12 12 CODE-SPAN:MASK ANDI,  14 11 12 ADD,
   15 14 0 LDR,
   5 DEFER-MAGIC LIT64,
   15 5 CMP,  C-EQ ok BCOND,
      DEFER-DIAG:DIE-NOT-DEFER
   ok LBL,
   14 14 8 ADDI,  14 14 0 LDR,
   14 DATA DEFER-META-CELL STR, ;

\ `is` stores a deferred word's execution token, so it declares the cell's
\ kind: a JIT-region address a snapshot moves with the region. Once per `is`;
\ the table refuses
\ duplicates, so a re-pointed defer stays one row.
: J-IS ( -- )
   C-DEFER-TARGET-META
   LVSPILL LABEL@ BL,
   C-POP-X16
   11 DATA DEFER-META-CELL LDR,
   9 11 0 ADDI,  RELOC-EMIT:LMARK LABEL@ BL,
   C-DATA-ADDR-RAW                                  \ movz/movk x9 = dispatch-cell addr (relocatable data addr)
   16 9 0 W-STRX C-EMITW ;

\ DRAIN-PRETRUST ( -- ): replay DEF-TRUST:REGISTER + C-CALL-CHECKER-DEFER for
\ every pending pre-trust defer, then empty the table. src/core/checker.f calls
\ it once right after `: TRUST`, where `trust-decl` and `checker-defer` both
\ exist. The band count is the loop state, so it survives the checker BLR
\ calls; each slot supplies the copied name and signature. A framed prim (FPRIM).
: BDRAINPRETRUST ( -- )
   LBL LBL {: loop done :}
   loop LBL,
      12 PD-TABLE-OFF LIT64,  12 DATA 12 ADD,  13 12 0 LDR,  13 done CBZ,   \ remaining==0 -> done
      DEF-TRUST:FIND                                  \ x11 = trust-decl XT (clobbers x12-x16)
      12 PD-TABLE-OFF LIT64,  12 DATA 12 ADD,  13 12 0 LDR,  13 13 1 SUBI,  \ index = remaining-1
      15 PD-SLOT MOVZ,  15 13 15 MUL,  14 12 PD-SLOTS-REL ADDI,  14 14 15 ADD,   \ x14 = slot base (x11 preserved)
      9 14 PD-NAME-OFF ADDI,  9 G-PUSH   9 14 PD-NLEN-OFF LDR,  9 G-PUSH        \ push name addr,len
      9 14 PD-SIG-OFF ADDI,   9 G-PUSH   9 14 PD-SLEN-OFF LDR,  9 G-PUSH        \ push sig addr,len
      C-CALL-X11-SAVED                                \ trust-decl( na nu sa su )
      NCOMP-DISPATCH:DECL-DEFER-OFF done DECL-OWNER:TARGET  \ x11 = target defer XT
      12 PD-TABLE-OFF LIT64,  12 DATA 12 ADD,  13 12 0 LDR,  13 13 1 SUBI,
      15 PD-SLOT MOVZ,  15 13 15 MUL,  14 12 PD-SLOTS-REL ADDI,  14 14 15 ADD,
      9 14 PD-NAME-OFF ADDI,  9 G-PUSH   9 14 PD-NLEN-OFF LDR,  9 G-PUSH        \ push name addr,len
      C-CALL-X11-SAVED                                \ checker-defer( ptr-u8 n )
      12 PD-TABLE-OFF LIT64,  12 DATA 12 ADD,  13 12 0 LDR,  13 13 1 SUBI,  13 12 0 STR,
      loop B,
   done LBL, ;

public
\ `immediate` writes one flag cell of the record just published and emits
\ nothing, through the alias as habu1.f BWIDEMARK and BINTMARK do. The imm-mark
\ row's body is this word too (EMIT-PRIMITIVE-SECTIONS).
: C-IMMEDIATE ( -- )
   9 NDICT 0 ADDI,  9 9 1 SUBI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   11 DATA REGION-ALIAS:DELTA-CELL LDR,  11 9 11 ADD,   \ x11 = the record through the alias
   10 9 16 LDR,  10 10 DNAME-IMM ORRI,  10 11 16 STR, ;

: C-QUOTE-START ( -- )
   12 DATA INP-CELL LDR,  12 12 1 ADDI,  13 12 0 ADDI, ;

private
: C-QUOTE-EOF ( -- )                               \ unterminated or bad-escape string literal: label fd 2, then recoverable inside evaluate (rc 74), fail-closed exit 74 at top level. Emitted into the scan/copy runtime routines (LESCSCAN/LESCCOPY) too; those run at compile-time and their local SP save is discarded by the LCOMPILEDIE recovery (SP reset from the catch/REPL boundary). INP not yet consumed at the die -> clean rollback
   1 LBADSTR LABEL@ ADR,  0 2 MOVZ,  2 BADSTR-MSG-LEN MOVZ,  NR-WRITE SYS,
   0 74 MOVZ,  LCOMPILEDIE LABEL@ B, ;

public
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

private
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

public
variable LESCSCAN  variable LESCCOPY
variable LSNAPRBD

private

public
: C-ESC-QUOTE-SCAN ( -- )
   LESCSCAN LABEL@ BL, ;

: C-ESC-QUOTE-CONSUME ( -- )
   15 12 13 SUB,  16 13 0 ADDI,  12 12 1 ADDI,  12 DATA INP-CELL STR, ;

private
: C-ESC-QUOTE-SAVE ( -- )
   SP SP 32 SUBI,  16 SP 0 STR,  15 SP 8 STR,  10 SP 16 STR, ;

: C-ESC-QUOTE-RESTORE ( -- )
   16 SP 0 LDR,  15 SP 8 LDR,  10 SP 16 LDR, ;

: C-ESC-QUOTE-SAVED-DROP ( -- )
   SP SP 32 ADDI, ;

: C-ESC-COPY-X17 ( -- )
   LESCCOPY LABEL@ BL, ;

\ With a hook installed through `set-top-check` (layout.f TOP-HOOK-CELL), a
\ site calls LTOPHOOK with x15 = TOP-EV-* class and x16 = LFIND flags (0 for
\ literals). Uninstalled, a site costs one load and a CBZ.
variable LCODEINV

: C-TOPHOOK-FLAGS ( n -- ) {: class:n :}   \ word/tick event: flags from x13 (LFIND)
   LBL {: nohk:label :}
   9 DATA TOP-HOOK-CELL LDR,  9 nohk CBZ,
   15 class MOVZ,  16 13 0 ADDI,
   LTOPHOOK LABEL@ BL,
   nohk LBL, ;

public

\ A session that observes what the source runs installs a callback. x5 is the
\ record selected by LFIND (zero for an already-bound code entry); x11 is the
\ entry actually about to run, and the callback returns the entry to run in
\ x11. x5 and x13 survive it: every site reads them after the call.
: C-HOST-SELECT ( -- )
   LBL {: ordinary:label :}
   9 DATA NATIVE-HOST-CELLS:SELECT LDR,  9 ordinary CBZ,
   SP SP 32 SUBI,  30 SP 0 STR,  13 SP 8 STR,  5 SP 16 STR,
   5 G-PUSH  11 G-PUSH
   9 BLR,
   11 G-POP
   5 SP 16 LDR,  13 SP 8 LDR,  30 SP 0 LDR,  SP SP 32 ADDI,
   ordinary LBL, ;

private

\ LEXITHOOK: layout.f EXIT-HOOK-CELL's trampoline, BL-called (BDIE, LUNCAUGHT, boot.f
\ CALL-EXIT,) with the code in x0; it keeps x0 and LR only. Sealed like (PROT-SPAN).
: EMIT-EXITHOOK ( -- )
   LBL LBL {: nohk:label end:label :}
   LEXITHOOK LABEL@ {: start:label :}
   s" (LEXITHOOK)" start LABEL>N end LABEL>N ENGINE-PRIMS:HELPER-REGISTER
   start LBL,
   9 DATA EXIT-HOOK-CELL LDR,  9 nohk CBZ,
   SP SP 16 SUBI,  30 SP 0 STR,  0 SP 8 STR,
   10 0 MOVZ,  10 DATA EXIT-HOOK-CELL STR,  10 DATA HND-CELL STR,
   9 BLR,
   0 SP 8 LDR,  30 SP 0 LDR,  SP SP 16 ADDI,
   nohk LBL,
   RET,
   end LBL, ;

public
: C-ISDQ ( -- )
   C-QUOTE-START
   C-QUOTE-SCAN
   C-QUOTE-CONSUME
   LBL LBL {: cl cd :}
   12 DATA 0 LDR,  15 12 0 ADDI,                        \ x12 = DP, x15 = string base
   14 12 10 ADD,  B-TASK-GUARD  14 DP-CHECK
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
   14 12 10 ADD,  14 14 1 ADDI,  B-TASK-GUARD  14 DP-CHECK
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
   1 13 0 ADDI,  2 10 0 ADDI,  G-OUT ;

: C-EISDQ ( -- )
   C-QUOTE-START
   C-ESC-QUOTE-SCAN
   C-ESC-QUOTE-CONSUME
   11 16 0 ADDI,  12 16 15 ADD,
   17 DATA 0 LDR,
   14 17 10 ADD,  B-TASK-GUARD  14 DP-CHECK
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
   14 17 10 ADD,  14 14 1 ADDI,  B-TASK-GUARD  14 DP-CHECK
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
   14 17 10 ADD,  B-TASK-GUARD  14 DP-CHECK
   C-ESC-COPY-X17
   17 DATA 0 STR,
   1 17 10 SUB,  2 10 0 ADDI,  G-OUT ;

: C-CHAR ( -- )
   LTOK LABEL@ BL,
   LBL {: named:label :}
   0 named CBNZ,
      LKWCHAR LABEL@ KWCHAR$ nip C-DIE-KEYWORD-NAME
   named LBL,
   9 DATA TKA-CELL LDR,  9 9 0 LDRB,  9 G-PUSH
   TOP-EV-CHAR C-TOPHOOK-LIT ;

private
: C-BCHAR ( -- )
   LTOK LABEL@ BL,
   LBL {: named:label :}
   0 named CBNZ,
      LKWBCHAR LABEL@ BCHAR-LEN C-DIE-KEYWORD-NAME
   named LBL,
   LBCAP LABEL@ BL,
   11 DATA TKA-CELL LDR,  11 11 0 LDRB,  LVPUSHC LABEL@ BL, ;

: C-SCOPE-ENTRY-TICK-GUARD ( n -- ) {: off:n :}
   off 0 < if exit then
   5 DICT-SIZE off + LIT64,  5 DBASE 5 ADD,
   11 5 CMP,  C-EQ LINTERNAL LABEL@ BCOND, ;

public
: C-SCOPE-ENTRY-GUARD ( -- )
   EM-SCOPE-CHILD-OFF @ C-SCOPE-ENTRY-TICK-GUARD
   EM-C2-MUT-OFF @ C-SCOPE-ENTRY-TICK-GUARD
   EM-C2-MUT-LOAN-OFF @ C-SCOPE-ENTRY-TICK-GUARD
   EM-C2-INIT-OFF @ C-SCOPE-ENTRY-TICK-GUARD
   EM-C2-RECORDS-OFF @ C-SCOPE-ENTRY-TICK-GUARD
   EM-C2-RECORD-OFF @ C-SCOPE-ENTRY-TICK-GUARD
   EM-C2-FIELD-OFF @ C-SCOPE-ENTRY-TICK-GUARD ;

private
: C-TRUSTED-TICK? ( -- )
   LBL LBL {: noquery:label restore:label :}
   SP SP 32 SUBI,
   5 SP 0 STR,  11 SP 8 STR,  12 SP 16 STR,  13 SP 24 STR,
   C-TICK-QUERY
   11 noquery CBZ,
   9 DATA TKA-CELL LDR,  9 G-PUSH
   9 DATA TKL-CELL LDR,  9 G-PUSH
   C-CALL-X11-SAVED
   17 G-POP
   restore B,
   noquery LBL,  17 0 MOVZ,
   restore LBL,
   5 SP 0 LDR,  11 SP 8 LDR,  12 SP 16 LDR,  13 SP 24 LDR,
   SP SP 32 ADDI, ;

\ A body's `['] W` carries C-TICK's DNAME-INT gate: an internal word has no
\ checker-known effect, so the tick erases to a plain cell an interpret-level
\ `execute` would run.
\ Resolution is the call site's: LFIND, then the used packages' publics, then
\ LUNDEF (test/xt-effect-test.f XE-USING).
\ C-TICK's DNAME-WIDE gate does not belong here: a body's tick hands the
\ address to a checked consumer, not the untyped interpret stack.
\ A TRUSTED: body skips C-TICK's trusted-only query, as tier 1 does: it may use
\ a trusted-only primitive (prims.f ETRUSTED-ONLY!) and the checker never walks
\ its tick. The native compiler's generated-call entries are such bodies,
\ compiled here when a source-only host captures the compiler.
: C-BTICK ( -- )
   LBL LBL LBL LBL LBL {: bk:label usedtry:label found:label named:label asserted:label :}
   LTOK LABEL@ BL,
   0 named CBNZ,
      LKWBTICK LABEL@ BTICK-LEN C-DIE-KEYWORD-NAME
   named LBL,
   C-QUALIFY-SEAL-GUARD                                 \ reject `['] RESERVED:tail` once sealed
   LBCAP LABEL@ BL,
   9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFIND LABEL@ BL,
   13 usedtry CBZ,                                       \ open-scope + global miss -> try used publics (`['] W` under a `using`)
   found LBL,
   14 13 16 ANDI,  14 LINTERNAL LABEL@ CBNZ,             \ DNAME-INT: C-TICK's gate, at the only other place a name becomes an address
   C-SCOPE-ENTRY-GUARD
   9 DATA TRUSTED-CELL LDR,  9 asserted CBNZ,            \ a TRUSTED: body asks no trusted-only query
   SP SP 48 SUBI,
   5 SP 0 STR,  11 SP 8 STR,  12 SP 16 STR,  13 SP 24 STR,
   C-TRUSTED-TICK?
   17 SP 32 STR,
   5 SP 0 LDR,  11 SP 8 LDR,  12 SP 16 LDR,  13 SP 24 LDR,
   17 SP 32 LDR,  SP SP 48 ADDI,
   17 LTRUSTTICK LABEL@ CBNZ,
   asserted LBL,
   C-CODE-ADDR
   bk B,
   usedtry LBL,
      9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFINDUSED LABEL@ BL,   \ used-publics resolution (ambiguity dies)
      13 LUNDEF LABEL@ CBZ,                              \ nothing resolves it -> undefined here, never a silent no-op
      found B,
   bk LBL, ;

public
\ A definition whose certified check recorded any wider-than-cell width fact is
\ re-run from a sealed copy of its source with width-aware transport lowering.
\ Width, bind and fetch facts come only from the immutable LOWER-CERT artifact
\ copied into the lowering transaction; pass 2 never calls the checker or reads
\ its mutable tables.
variable LP2CWAT  
variable LKWTUCK3  variable LKWROT3  variable LKWMROT3
variable LKW2DUP3  variable LKW2DROP3  variable LKW2SWAP3  variable LKW2OVER3
variable LKW2TOR3  variable LKW2RFROM3  variable LKW2RFET3
variable LKWAT2  variable LKWSTORE2
variable LP2COPY  variable LP2DROPN    variable LP2ROT  variable LP2RS
variable LP2FETCH  variable LP2STORE    variable LP2VEMIT
variable LP2SLOT
;package
package LOWER-TXN-CODE
public
variable MEM  variable FULL  variable DRIFT-FAIL
: MEM$ ( -- ptr u8 n )     s" hb: lowering transaction memory failed" ;
: FULL$ ( -- ptr u8 n )    s" hb: lowering transaction full" ;
;package

package ENGINE-EMIT
: EM-P2-SLOT-DIE ( -- )            \ frame slot beyond the scaled ldr/str range: fail closed
   0 2 MOVZ,  1 LP2SLOT LABEL@ ADR,  2 P2SLOT-MSG$ nip MOVZ,  NR-WRITE SYS,
   $4B C-EXIT-TOKEN ;

: EM-P2-LOCAL-GUARD ( n label -- ) {: reg:n ok:label :}
   reg 64 CMPI,  C-CC ok BCOND,
   EM-P2-SLOT-DIE
   ok LBL, ;

: EM-P2-CARVE-W ( -- )             \ x10 := next certified bind width; bind it to live local [SP]
   LBL LBL LBL {: localok:label bindok:label widthok:label :}
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
   \ The existing scaled local load/store slot admits exactly 4096 cells.
   12 4096 MOVZ,  10 12 CMP,  C-LS widthok BCOND,  EM-P2-SLOT-DIE
   widthok LBL,
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
\ loop) reads back by live index. A second width-validation
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
   LBL {: frameok:label :}
   9 SP 8 LDR,  5 9 3 LSLI,  5 5 15 ADDI,  5 5 $FFFFFFFFFFFFFFF0 ANDI,
   15 DATA LOCF-CELL LDR,  15 15 5 ADD,
   12 32768 MOVZ,  15 12 CMP,  C-LS frameok BCOND,  EM-P2-SLOT-DIE
   frameok LBL,
   8 $D10003FF LIT64,  7 5 0 ADDI,  LADDSUBIMM LABEL@ BL,   \ sub sp,sp,#frame
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
         9 W-POP9 LIT64,  LCEMIT LABEL@ BL,          \ ldr x9,[x19,#-8]!
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

: C-LBRACE-DIE ( -- )   \ locals opener inside a quotation: recoverable inside evaluate (rc $4B), fail-closed exit $4B at top level
   1 LBADLOC LABEL@ ADR,  0 2 MOVZ,  2 BADLOC-LEN 1- MOVZ,  NR-WRITE SYS,   \ the message without BADLOC-KW's trailing newline (the LCOMPILEDIE tail ends the line)
   0 $4B MOVZ,  LCOMPILEDIE LABEL@ B, ;

: C-LBRACE-GUARDS ( -- )
   LBL {: qlok:label :}
   11 DATA QPATCH-CELL LDR,  11 qlok CBZ,
      C-LBRACE-DIE
   qlok LBL, ;

: C-LBRACE-STORE-ONE ( -- )
   LBL LBL LBL LBL {: ncp ncd tsl tsd :}
   11 DATA LOCN-CELL LDR,  11 LOC-RECS CMPI,  C-GE LLOCMANY LABEL@ BCOND,   \ a LOC-RECS+1th local has no record: named refusal (rc 70, catchable inside evaluate), nothing stored. Only LOCN/LOCNAMES compile-state mutated (EM-RESET-COMPILE-STATE zeroes LOCN) -> clean rollback
   11 DATA LOCN-CELL LDR,  12 LOC-REC MOVZ,  11 11 12 MUL,  5 LOCNAMES LIT64,  11 11 5 ADD,  11 DATA 11 ADD,
   14 0 MOVZ,  8 DATA TKL-CELL LDR,  10 DATA TKA-CELL LDR,
   tsl LBL,  14 8 CMP,  C-GE tsd BCOND,
      15 10 14 ADD,  15 15 0 LDRB,  15 58 CMPI,  C-EQ tsd BCOND,
      14 14 1 ADDI,  tsl B,
   tsd LBL,
   14 LOC-NAME-CAP CMPI,  C-GT LLOCWIDE LABEL@ BCOND,          \ a bare name wider than the record's name field would run into the next record (or past LOCNAMES from the last slot): named refusal, nothing stored
   14 11 0 STR,
   12 11 8 ADDI,  13 DATA TKA-CELL LDR,
   ncp LBL,  14 ncd CBZ,  15 13 0 LDRB, 15 12 0 STRB, 12 12 1 ADDI, 13 13 1 ADDI, 14 14 1 SUBI, ncp B,
   ncd LBL,
   11 DATA LOCN-CELL LDR,  11 11 1 ADDI,  11 DATA LOCN-CELL STR, ;

public
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

private
\ Pass 1 binds the group (x6 = its first local) from the virtual stack's top:
\ local i, from the last down to the first, takes the top entry while one
\ exists. A register entry is stored straight into the slot and its register
\ freed; a constant goes through x16, which is never pooled. Once the virtual
\ stack is empty the rest pop from memory into x16. Entries deeper than the
\ group stay virtual. A float anywhere on the virtual stack spills it first,
\ so every local of that group pops from memory. The frame holds the group
\ start [sp], LOCF/8 [sp+8], i [sp+16] and the slot field [sp+24].
: C-LBRACE-BIND-VS ( -- )
   LBL LBL LBL LBL {: fl:label fd:label bl:label bd:label :}
   LBL LBL LBL LBL {: vcon:label vmem:label vx16:label vst:label :}
   SP SP 32 SUBI,  6 SP 0 STR,
   5 0 MOVZ,
   fl LBL,
      6 DATA VSP-CELL LDR,  5 6 CMP,  C-GE fd BCOND,
      7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
      5 5 1 ADDI,
      7 2 CMPI,  C-LT fl BCOND,
      LVSPILL LABEL@ BL,
   fd LBL,
   13 DATA LOCN-CELL LDR,  6 SP 0 LDR,  14 13 6 SUB,
   5 14 3 LSLI,  5 5 15 ADDI,  5 5 $FFFFFFFFFFFFFFF0 ANDI,
   8 $D10003FF LIT64,  7 5 0 ADDI,  LADDSUBIMM LABEL@ BL,   \ sub sp,sp,#frame
   15 DATA LOCF-CELL LDR,  15 15 5 ADD,  15 DATA LOCF-CELL STR,
   12 15 3 LSRI,  12 SP 8 STR,
   13 DATA LOCN-CELL LDR,  13 13 1 SUBI,  13 SP 16 STR,
   bl LBL,
      13 SP 16 LDR,  6 SP 0 LDR,  13 6 CMP,  C-LT bd BCOND,
      12 SP 8 LDR,  5 12 13 SUB,  5 5 1 SUBI,  5 5 10 LSLI,  5 SP 24 STR,
      6 DATA VSP-CELL LDR,  6 vmem CBZ,
      5 6 1 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
      8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 LDR,
      7 vcon CBNZ,
         9 $F90003E0 LIT64,  9 9 11 ORR,  vst B,                \ str xR
      vcon LBL,
         14 16 MOVZ,  LVMOVK LABEL@ BL,                         \ movz/movk x16,val
         vx16 B,
      vmem LBL,
         9 W-POPR 16 or LIT64,  LCEMIT LABEL@ BL,               \ ldr x16,[x19,#-8]!
      vx16 LBL,
         9 $F90003F0 LIT64,                                     \ str x16
      vst LBL,
         5 SP 24 LDR,  9 9 5 ORR,  LCEMIT LABEL@ BL,            \ ,[sp,#slot]
         LVDROP LABEL@ BL,             \ pop and free the bound entry; none once the VS is empty
      13 SP 16 LDR,  13 13 1 SUBI,  13 SP 16 STR,
      bl B,
   bd LBL,
   SP SP 32 ADDI, ;

\ `{:` enters without the dispatch spill (CFN-ENTRY): pass 1 binds from the
\ virtual stack, while the pass-2 carve pops every cell from memory, so it
\ spills first.
: C-LBRACE-CARVE-FRAME ( -- )
   LBL LBL {: p1c:label pjoin:label :}
   9 DATA P2-CELL LDR,  9 p1c CBZ,                    \ pass 2: width-aware carve
      LVSPILL LABEL@ BL,
      EM-P2-CARVE
      pjoin B,
   p1c LBL,
   C-LBRACE-BIND-VS
   pjoin LBL, ;

public
: C-LBRACE ( -- )
   9 DATA LOCN-CELL LDR,  9 DATA P2LOC0-CELL STR,     \ group start for the pass-2 carve
   C-LBRACE-GUARDS
   C-LBRACE-PARSE-NAMES
   C-LBRACE-CARVE-FRAME ;

private
\ Push the PC-relative address of the target in x11 (C-ADR-X9).
: C-ADR ( -- )
   C-ADR-X9
   9 W-PUSH9 LIT64,  LCEMIT LABEL@ BL, ;

: C-SDQ ( -- )
   LBL LBL LBL LBL {: cl cd inline done :}
   C-QUOTE-START
   C-QUOTE-SCAN
   C-QUOTE-CONSUME
   C-QUOTE-SAVE
   C-QUOTE-RESTORE
   11 16 0 ADDI,  12 10 1 ADDI,  LBCS LABEL@ BL,
   C-QUOTE-RESTORE
   9 DATA NSTR-ABI:INTERN-CELL LDR,  9 inline CBZ,
   11 16 0 ADDI,  12 10 0 ADDI,  LNSTRINTERN LABEL@ BL,   \ interned straight from the input
   C-DATA-ADDR  done B,
   inline LBL,                                     \ no interner yet: the bytes go in the code
   15 CP 0 ADDI,  9 $14000000 LIT64,  LCEMIT LABEL@ BL,
   12 CP 0 ADDI,
   C-QUOTE-RESTORE
   11 16 0 ADDI,  9 10 0 ADDI,
   1 DATA REGION-ALIAS:DELTA-CELL LDR,  1 CP 1 ADD,               \ x1 = CP through the alias
   cl LBL,  9 cd CBZ,
      14 11 0 LDRB,  14 1 0 STRB,  1 1 1 ADDI,  28 28 1 ADDI,
      11 11 1 ADDI,  9 9 1 SUBI,  cl B,
   cd LBL,
   28 28 3 ADDI,  5 -4 LIT64,  28 28 5 AND,
   9 15 0 ADDI,  15 10 0 ADDI,  LPAT LABEL@ BL,
   11 12 0 ADDI,  C-ADR                                \ push byte addr PC-relative (AOT/ASLR-safe)
   done LBL,
   C-QUOTE-RESTORE
   11 10 0 ADDI,  C-LIT                                \ push len (a value, absolute is fine)
   C-QUOTE-SAVED-DROP ;

\ The open package's record name: x10 its length, x16 its bytes; x10 is 0
\ outside a package. Clobbers x9 and x14.
: C-PKG-NAME ( -- )
   LBL LBL {: named:label ext:label :}
   10 0 MOVZ,
   9 DATA PKG-REC-CELL LDR,  9 named CBZ,
   10 9 16 LDR,  10 10 DNAME-FLAG-BITS LSLI,  10 10 DNAME-FLAG-BITS LSRI,   \ x10 = the name's length
   16 9 24 ADDI,
   14 9 16 LDR,  14 14 DNAME-EXT ANDI,  14 ext CBZ,
      16 9 24 LDR,                                    \ x16 = the name's bytes
   ext LBL,
   named LBL, ;

\ Copy the x10 bytes at x16 to x15 with A-Z folded to a-z. Clobbers x7, x14 and
\ x17.
: C-PKG-FOLD ( -- )
   LBL LBL LBL {: fl:label keep:label fd:label :}
   7 0 MOVZ,
   fl LBL,  7 10 CMP,  C-CS fd BCOND,
      14 16 7 ADD,  14 14 0 LDRB,
      14 $41 CMPI,  C-LT keep BCOND,  14 $5A CMPI,  C-GT keep BCOND,
         14 14 $20 ORRI,
      keep LBL,
      17 15 7 ADD,  14 17 0 STRB,
      7 7 1 ADDI,  fl B,
   fd LBL, ;

\ In a body `[package]` compiles the folded name as C-SDQ compiles a literal,
\ folding it into a machine-stack frame sized to the name.
: C-BPACKAGE ( -- )
   LBL LBL LBL LBL {: cl:label cd:label inline:label done:label :}
   C-PKG-NAME
   13 10 15 ADDI,  5 -16 LIT64,  13 13 5 AND,         \ x13 = the frame, 16-aligned
   14 SP 0 ADDI,  14 14 13 SUB,  SP 14 0 ADDI,
   15 SP 0 ADDI,  C-PKG-FOLD
   SP SP 16 SUBI,  13 SP 0 STR,  10 SP 8 STR,         \ the folded bytes at SP+16
   9 DATA NSTR-ABI:INTERN-CELL LDR,  9 inline CBZ,
   11 SP 16 ADDI,  12 10 0 ADDI,  LNSTRINTERN LABEL@ BL,
   C-DATA-ADDR  done B,
   inline LBL,                                        \ no interner yet: the bytes go in the code
   15 CP 0 ADDI,  9 $14000000 LIT64,  LCEMIT LABEL@ BL,
   12 CP 0 ADDI,
   10 SP 8 LDR,  11 SP 16 ADDI,  9 10 0 ADDI,
   1 DATA REGION-ALIAS:DELTA-CELL LDR,  1 CP 1 ADD,               \ x1 = CP through the alias
   cl LBL,  9 cd CBZ,
      14 11 0 LDRB,  14 1 0 STRB,  1 1 1 ADDI,  28 28 1 ADDI,
      11 11 1 ADDI,  9 9 1 SUBI,  cl B,
   cd LBL,
   28 28 3 ADDI,  5 -4 LIT64,  28 28 5 AND,
   9 15 0 ADDI,  15 10 0 ADDI,  LPAT LABEL@ BL,
   11 12 0 ADDI,  C-ADR
   done LBL,
   10 SP 8 LDR,  13 SP 0 LDR,  SP SP 16 ADDI,
   14 SP 0 ADDI,  14 14 13 ADD,  SP 14 0 ADDI,
   11 10 0 ADDI,  C-LIT ;

public
\ At top level `[package]` pushes the open package's folded name (C-PKG-NAME,
\ C-PKG-FOLD), copied into data space as C-ISDQ copies a literal.
: C-IBPACKAGE ( -- )
   C-PKG-NAME
   12 DATA 0 LDR,  15 12 0 ADDI,                        \ x12 = DP, x15 = string base
   14 12 10 ADD,  B-TASK-GUARD  14 DP-CHECK
   C-PKG-FOLD
   12 15 10 ADD,  12 DATA 0 STR,                        \ allot: DP advances past the copy
   15 G-PUSH  10 G-PUSH
   TOP-EV-STR C-TOPHOOK-LIT ;

private
: C-CQ ( -- )
   LBL LBL LBL LBL LBL LBL LBL {: capok cl cd inline done il id :}
   C-QUOTE-START
   C-QUOTE-SCAN
   C-QUOTE-LEN
   10 255 CMPI,  C-LE capok BCOND,  LCSTR LABEL@ B,   \ c" over 255 bytes (compile mode): named fd-2 label (LCSTR) then rc 76 via LCOMPILEDIE -- recoverable inside evaluate, fail-closed exit 76 at top level. Fires before C-QUOTE-CONSUME-DONE (INP not consumed, nothing emitted) -> clean rollback
   capok LBL,
   C-QUOTE-CONSUME-DONE
   C-QUOTE-SAVE
   C-QUOTE-RESTORE
   11 16 0 ADDI,  12 10 1 ADDI,  LBCS LABEL@ BL,
   C-QUOTE-RESTORE
   9 DATA NSTR-ABI:INTERN-CELL LDR,  9 inline CBZ,
   17 DATA REGION-ALIAS:DELTA-CELL LDR,  17 CP 17 ADD,            \ x17 = CP through the alias
   10 17 0 STRB,  17 17 1 ADDI,
   11 16 0 ADDI,  9 10 0 ADDI,
   il LBL,  9 id CBZ,
      14 11 0 LDRB,  14 17 0 STRB,  17 17 1 ADDI,  11 11 1 ADDI,  9 9 1 SUBI,  il B,
   id LBL,
   11 28 0 ADDI,  12 10 1 ADDI,  LNSTRINTERN LABEL@ BL,
   C-DATA-ADDR  done B,
   inline LBL,                                     \ no interner yet: the bytes go in the code
   15 CP 0 ADDI,  9 $14000000 LIT64,  LCEMIT LABEL@ BL,
   12 CP 0 ADDI,
   C-QUOTE-RESTORE
   1 DATA REGION-ALIAS:DELTA-CELL LDR,  1 CP 1 ADD,               \ x1 = CP through the alias
   10 1 0 STRB,  1 1 1 ADDI,  28 28 1 ADDI,
   11 16 0 ADDI,  9 10 0 ADDI,
   cl LBL,  9 cd CBZ,
      14 11 0 LDRB,  14 1 0 STRB,  1 1 1 ADDI,  28 28 1 ADDI,
      11 11 1 ADDI,  9 9 1 SUBI,  cl B,
   cd LBL,
   28 28 3 ADDI,  5 -4 LIT64,  28 28 5 AND,
   9 15 0 ADDI,  15 10 1 ADDI,  LPAT LABEL@ BL,
   11 12 0 ADDI,  C-ADR
   done LBL,
   C-QUOTE-SAVED-DROP ;

: C-DOTQ ( -- )
   LBL {: ok :}
   C-SDQ
   9 LKWTYPE LABEL@ ADR,  10 4 MOVZ,  LFIND LABEL@ BL,
   13 ok CBNZ,  0 70 MOVZ,  NR-EXIT-GROUP SYS,
   ok LBL,
   C-CALL ;

: C-ESDQ ( -- )
   LBL LBL {: inline:label done:label :}
   C-QUOTE-START
   C-ESC-QUOTE-SCAN
   C-ESC-QUOTE-CONSUME
   C-ESC-QUOTE-SAVE
   C-ESC-QUOTE-RESTORE
   11 16 0 ADDI,  12 15 1 ADDI,  LBCS LABEL@ BL,
   C-ESC-QUOTE-RESTORE
   9 DATA NSTR-ABI:INTERN-CELL LDR,  9 inline CBZ,
   11 16 0 ADDI,  12 16 15 ADD,
   17 DATA REGION-ALIAS:DELTA-CELL LDR,  17 CP 17 ADD,            \ x17 = CP through the alias
   C-ESC-COPY-X17
   11 28 0 ADDI,  12 10 0 ADDI,  LNSTRINTERN LABEL@ BL,
   C-DATA-ADDR  done B,
   inline LBL,                                     \ no interner yet: the bytes go in the code
   9 $14000000 LIT64,  LCEMIT LABEL@ BL,
   13 CP 0 ADDI,
   C-ESC-QUOTE-RESTORE
   11 16 0 ADDI,  12 16 15 ADD,
   17 DATA REGION-ALIAS:DELTA-CELL LDR,  17 CP 17 ADD,            \ x17 = CP through the alias
   C-ESC-COPY-X17
   28 DATA REGION-ALIAS:DELTA-CELL LDR,  28 17 28 SUB,            \ CP = the copy's end, back in the RX view
   28 28 3 ADDI,  5 -4 LIT64,  28 28 5 AND,
   12 13 0 ADDI,
   9 13 4 SUBI,  15 10 0 ADDI,  LPAT LABEL@ BL,
   11 12 0 ADDI,  C-ADR
   done LBL,
   C-ESC-QUOTE-RESTORE
   11 10 0 ADDI,  C-LIT
   C-ESC-QUOTE-SAVED-DROP ;

: C-ECQ ( -- )
   LBL LBL LBL {: capok:label inline:label done:label :}
   C-QUOTE-START
   C-ESC-QUOTE-SCAN
   10 255 CMPI,  C-LE capok BCOND,  LCSTR LABEL@ B,   \ c\" over 255 bytes (compile mode): named fd-2 label (LCSTR) then rc 76 via LCOMPILEDIE -- recoverable inside evaluate, fail-closed exit 76 at top level. Fires before C-ESC-QUOTE-CONSUME (INP not consumed) -> clean rollback
   capok LBL,
   C-ESC-QUOTE-CONSUME
   C-ESC-QUOTE-SAVE
   C-ESC-QUOTE-RESTORE
   11 16 0 ADDI,  12 15 1 ADDI,  LBCS LABEL@ BL,
   C-ESC-QUOTE-RESTORE
   9 DATA NSTR-ABI:INTERN-CELL LDR,  9 inline CBZ,
   17 DATA REGION-ALIAS:DELTA-CELL LDR,  17 CP 17 ADD,            \ x17 = CP through the alias
   10 17 0 STRB,  17 17 1 ADDI,
   11 16 0 ADDI,  12 16 15 ADD,
   C-ESC-COPY-X17
   11 28 0 ADDI,  12 10 1 ADDI,  LNSTRINTERN LABEL@ BL,
   C-DATA-ADDR  done B,
   inline LBL,                                     \ no interner yet: the bytes go in the code
   9 $14000000 LIT64,  LCEMIT LABEL@ BL,
   13 CP 0 ADDI,
   C-ESC-QUOTE-RESTORE
   17 DATA REGION-ALIAS:DELTA-CELL LDR,  17 CP 17 ADD,            \ x17 = CP through the alias
   10 17 0 STRB,  17 17 1 ADDI,
   11 16 0 ADDI,  12 16 15 ADD,
   C-ESC-COPY-X17
   28 DATA REGION-ALIAS:DELTA-CELL LDR,  28 17 28 SUB,            \ CP = the copy's end, back in the RX view
   28 28 3 ADDI,  5 -4 LIT64,  28 28 5 AND,
   12 13 0 ADDI,
   9 13 4 SUBI,  15 10 1 ADDI,  LPAT LABEL@ BL,
   11 12 0 ADDI,  C-ADR
   done LBL,
   C-ESC-QUOTE-SAVED-DROP ;

: C-EDOTQ ( -- )
   LBL {: ok:label :}
   C-ESDQ
   9 LKWTYPE LABEL@ ADR,  10 4 MOVZ,  LFIND LABEL@ BL,
   13 ok CBNZ,  0 70 MOVZ,  NR-EXIT-GROUP SYS,
   ok LBL,
   C-CALL ;
variable CFSK

public
\ Every registrar below honours jit.f's row-comparison mode (CF-DEF-GUARD).
: CF-ENTRY ( label ptr n n [ -- ] -- ) {: lmainlbl:label kwvar:ptr kwlen:n hxt :}
   CF-DEF-GUARD @ IF kwvar kwlen CF-DEF-GUARD-ROW exit THEN
   LBL CFSK !
   0 kwvar LABEL@ ADR,  1 kwlen MOVZ,  LKWCMP LABEL@ BL,
   0 CFSK LABEL@ CBZ,
   LVSPILL LABEL@ BL,
   hxt execute  lmainlbl B,
   CFSK LABEL@ LBL, ;

\ cfn-entry: keyword case WITHOUT the spill — join words manage the VS
\ themselves (BEGIN snapshots it; THEN, ELSE and the back edges reconcile).
: CFN-ENTRY ( label ptr n n [ -- ] -- ) {: lmainlbl:label kwvar:ptr kwlen:n hxt :}
   CF-DEF-GUARD @ IF kwvar kwlen CF-DEF-GUARD-ROW exit THEN
   LBL CFSK !
   0 kwvar LABEL@ ADR,  1 kwlen MOVZ,  LKWCMP LABEL@ BL,
   0 CFSK LABEL@ CBZ,
   hxt execute  lmainlbl B,
   CFSK LABEL@ LBL, ;
\ MAIN is split into emission-ordered phases sharing label variables.
variable LMAIN       \ LUNDEF is declared up with the diagnostics block
variable LCAPBIND           \ tier 1's captured token, asked whether it binds before LMAIN; placed nowhere: only row-walk code names it
public
variable LUN0   \ re-entrant evaluate: original-path continuation of LUNDEF
variable LEVALREC             \ re-entrant evaluate: throw-escape recovery entry (BTHROW branches here)
private
variable LEVLL  variable LEVLP  variable LEVLD  variable LEVLN \ LEVALREC internal labels
variable LEVCORRUPT  variable LEVCORRUPTMSG   \ eval-cross forged/corrupt handler-frame fail-closed path + its message
variable CFSK2

\ cfb-entry: branch keywords (if/while) with the condition on the VS. A
\ register top takes hxtr, with the register in x14; a constant or an empty VS
\ takes hxtm. checkxt makes the word's compile-time refusals first on both
\ paths, before anything is emitted (C-CF-ROOM's comment states the rule).
: CFB-ENTRY ( label ptr n n [ -- ] [ -- ] [ -- ] -- ) {: lmainlbl:label kwvar:ptr kwlen:n checkxt hxtm hxtr :}
   CF-DEF-GUARD @ IF kwvar kwlen CF-DEF-GUARD-ROW exit THEN
   LBL CFSK !  LBL CFSK2 !
   0 kwvar LABEL@ ADR,  1 kwlen MOVZ,  LKWCMP LABEL@ BL,
   0 CFSK LABEL@ CBZ,
   checkxt execute
   6 DATA VSP-CELL LDR,  6 CFSK2 LABEL@ CBZ,
   5 6 1 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   7 CFSK2 LABEL@ CBNZ,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  14 8 0 LDR,
   LVFUSE LABEL@ BL,
   SP SP 16 SUBI,  16 SP 0 STR,  14 SP 8 STR,
   LBL LBL {: kept:label snapped:label :}
   LVDROP LABEL@ BL,  12 kept CBZ,
      7 14 0 ADDI,  LVBIT LABEL@ BL,
      6 DATA VRFREE-CELL LDR,  6 6 8 EOR,  6 DATA VRFREE-CELL STR,
      LVSNAP LABEL@ BL,
      14 SP 8 LDR,  7 14 0 ADDI,  LVBIT LABEL@ BL,
      6 DATA VRFREE-CELL LDR,  6 6 8 ORR,  6 DATA VRFREE-CELL STR,
      snapped B,
   kept LBL,  LVSNAP LABEL@ BL,
   snapped LBL,
   16 SP 0 LDR,  14 SP 8 LDR,  SP SP 16 ADDI,
   hxtr execute
   lmainlbl B,
   CFSK2 LABEL@ LBL,
   LVSPILL LABEL@ BL,  LVSNAP LABEL@ BL,
   hxtm execute
   lmainlbl B,
   CFSK LABEL@ LBL, ;

\ cfbn-entry: like CFB-ENTRY but the register path neither spills nor saves.
: CFBN-ENTRY ( label ptr n n [ -- ] [ -- ] [ -- ] -- ) {: lmainlbl:label kwvar:ptr kwlen:n checkxt hxtm hxtr :}
   CF-DEF-GUARD @ IF kwvar kwlen CF-DEF-GUARD-ROW exit THEN
   LBL CFSK !  LBL CFSK2 !
   0 kwvar LABEL@ ADR,  1 kwlen MOVZ,  LKWCMP LABEL@ BL,
   0 CFSK LABEL@ CBZ,
   checkxt execute
   6 DATA VSP-CELL LDR,  6 CFSK2 LABEL@ CBZ,
   5 6 1 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   7 CFSK2 LABEL@ CBNZ,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  14 8 0 LDR,
   LVFUSE LABEL@ BL,
   LVDROP LABEL@ BL,
   hxtr execute
   lmainlbl B,
   CFSK2 LABEL@ LBL,
   LVSPILL LABEL@ BL,
   hxtm execute
   lmainlbl B,
   CFSK LABEL@ LBL, ;

\ A kind entry at the forward branch on the condition (CFB-ENTRY): the b.cond
\ word in x16 when it is nonzero, else cbz on the register x14.
: C-CBZR ( n -- )
   LBL LBL {: reg:label emit:label :}
   C-PUSHCP
   16 reg CBZ,
      9 16 0 ADDI,  emit B,
   reg LBL,
      8 $B4000000 LIT64,  9 8 14 ORR,
   emit LBL,  LCEMIT LABEL@ BL, ;

: J-IFR ( -- )  CFK-IF C-CBZR ;

: J-WHILER ( -- )  CFK-WHILE C-CBZR ;

\ A fused compare (x16 = b.cond) branches on the flags, which the reconcile's
\ loads, stores and moves leave alone. Otherwise the reg flag moves to x17
\ first, since the reconcile may reload into x14, and the branch is cbz x17.
: J-UNTILR ( -- )
   LBL {: fused:label :}
   16 fused CBNZ,
      8 $AA0003F1 LIT64,  7 14 16 LSLI,  9 8 7 ORR,  LCEMIT LABEL@ BL,
      16 $B4000011 LIT64,
   fused LBL,
   J-UNTILX ;

public
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

private

public
\ The boot task's return stack and DO/LOOP frame stack. They come after the DATA
\ region because their bases are published in header cells; a task maps its own
\ pair and publishes them into its own region copy (src/lib/task.f PREPARE).
: EM-FRAME-STACKS ( -- )
   STACK-ABI:RETURN-BYTES 10 STACK-GUARD:EMIT-MAP
   10 DATA STACK-ABI:RETURN-BASE-CELL STR,
   STACK-ABI:LOOP-BYTES 10 STACK-GUARD:EMIT-MAP
   10 DATA STACK-ABI:LOOP-BASE-CELL STR, ;

: EM-MMAP-CODE-REGION ( -- )
   LBL LBL {: rok:label rvok:label :}
   \ Hint the region at __text (XREG-RBASE/x20) + REGION-OFF and take the base
   \ the kernel returns. A fixed address cannot be reserved: measured on macOS
   \ 26.5.1 over 200 runs of a snapshot image, the runtime's own ~1.1 MiB of
   \ mappings (randomised, +0xE90000..+0x1214000) took the hint in 182. Region-to-
   \ text call displacements are relocated (RELOC-EMIT:EMIT-CALLS), so the base
   \ only has to lie within BL reach of __text, which the assertion below checks.
   \ PROT-PAGE-MAX extra bytes and a round-up keep the base PROT-PAGE-MAX aligned
   \ when the kernel returns an address only page aligned.
   6 REGION-OFF LIT64,  6 XREG-RBASE 6 ADD,
   9 PROT-PAGE-MAX 1 - MOVZ,  6 6 9 ADD,
   9 PROT-PAGE-MAX negate LIT64,  6 6 9 AND,       \ x6 = page-aligned canonical hint
   \ Clamp the hint after the last segment described by the loaded image.
   9 CODE-OFF MOVZ,  9 XREG-RBASE 9 SUB,           \ x9 = this run's image base
   HB-TARGET-LINUX? IF
      7 9 136 LDR,  8 9 80 LDR,  7 7 8 SUB,
      8 9 160 LDR,
   ELSE
      7 9 $1B0 LDR,  8 9 $80 LDR,  7 7 8 SUB,
      8 9 $1B8 LDR,
   THEN
   7 7 8 ADD,                                      \ x7 = final segment end offset
   7 9 7 ADD,
   8 PROT-PAGE-MAX 1 - MOVZ,  7 7 8 ADD,
   8 PROT-PAGE-MAX negate LIT64,  7 7 8 AND,
   6 7 CMP,  6 6 7 C-HI CSEL,                      \ x6 = the higher of hint and image end
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
   9 PROT-PAGE-MAX 1 - MOVZ,  0 0 9 ADD,
   9 PROT-PAGE-MAX negate LIT64,  0 0 9 AND,       \ page-aligned region base
   \ Boot BL-range assertion (permanent guarantee stage C's direct BL relies on): the
   \ whole region must lie within +/-128 MiB of __text. region_end - __text base is the
   \ farthest displacement; die named if it reaches BL-REACH. x0 (region base) survives.
   9 REGION LIT64,  9 0 9 ADD,  9 9 XREG-RBASE SUB,
   5 BL-REACH LIT64,  9 5 CMP,
   C-CC rvok BCOND,
      1 LBLRANGE LABEL@ ADR,  0 2 MOVZ,  2 BLRANGE-MSG-LEN MOVZ,  NR-WRITE SYS,
      0 BL-RANGE-RC MOVZ,  NR-EXIT-GROUP SYS,
   rvok LBL,
   \ The region's two views (layout.f REGION-ALIAS): code runs from this base,
   \ which turns RX here, and stores go through a RW alias of [base, base+REGION).
   \ x21 carries alias - base to the start-up that publishes it; nothing from
   \ here to there uses x21. The calls keep x19/x20 and argc/argv/envp. macOS
   \ remaps the anonymous pages; Linux maps one memfd twice, the RX view over the
   \ reservation, whose slack on both sides of [base, base+REGION), below the
   \ rounded-up base and past its end, stays anonymous RW.
   HB-TARGET-MACOS? IF
      LBL LBL LBL  S\" hb: cannot map the code region's alias\n"
      {: afail:label adone:label amsg:label ma:ptr mu:n :}
      SP SP 32 SUBI,  13 SP 0 STR,  14 SP 8 STR,  15 SP 16 STR,  0 SP 24 STR,
      16 XREG-RBASE 0 ADDI,  false afail LIBC-OS:REGION-REMAP
      21 0 0 ADDI,
      0 SP 24 LDR,  1 REGION LIT64,  2 5 MOVZ,  NR-MPROTECT SYS,
      C-CS afail BCOND,
      13 SP 0 LDR,  14 SP 8 LDR,  15 SP 16 LDR,  0 SP 24 LDR,  SP SP 32 ADDI,
      adone B,
      afail LBL,
         1 amsg ADR,  0 2 MOVZ,  2 mu MOVZ,  NR-WRITE SYS,
         0 78 MOVZ,  NR-EXIT-GROUP SYS,
      amsg LBL,  ma mu BYTES,
      adone LBL,
   ELSE
      LBL LBL LBL  S\" hb: cannot map the code region's views\n"
      {: vfail:label vdone:label vmsg:label va:ptr vu:n :}
      9 0 0 ADDI,  vfail LIBC-OS:REGION-MEMFD  10 0 0 ADDI,   \ x9 = base, x10 = fd
      0 9 0 ADDI,  1 REGION LIT64,  2 5 MOVZ,  3 MAP-SHARED-FIXED LIT64,
      4 10 0 ADDI,  5 0 MOVZ,  NR-MMAP SYS,  C-CS vfail BCOND,
      0 0 MOVZ,  1 REGION LIT64,  2 3 MOVZ,  3 MAP-SHARED LIT64,
      4 10 0 ADDI,  5 0 MOVZ,  NR-MMAP SYS,  C-CS vfail BCOND,
      21 0 9 SUB,
      0 10 0 ADDI,  NR-CLOSE SYS,  C-CS vfail BCOND,
      0 9 0 ADDI,  vdone B,
      vfail LBL,
         1 vmsg ADR,  0 2 MOVZ,  2 vu MOVZ,  NR-WRITE SYS,
         0 78 MOVZ,  NR-EXIT-GROUP SYS,
      vmsg LBL,  va vu BYTES,
      vdone LBL,
   THEN ;

private
;package

package ENGINE-EMIT

public
: EM-MMAP-DATA-REGION ( -- )
   LBL {: dvok :}
   0 DATA-VA VA>N LIT64,  1 DATA-SIZE LIT64,  2 3 MOVZ,  3 MAP-DATA-RESERVE LIT64,  4 0 MOVN,  5 0 MOVZ,
   NR-MMAP SYS,
   5 DATA-VA VA>N LIT64,  0 5 CMP,
   C-EQ dvok BCOND,
      1 LMMAPDATA LABEL@ ADR,  0 2 MOVZ,  2 MMAPDATA-MSG-LEN MOVZ,  NR-WRITE SYS,   \ name the fault on fd 2 (bytes in loaded __text) before exit; code region is already mapped, argc/argv/envp in x13/x14/x15 untouched
      0 78 MOVZ,  NR-EXIT-GROUP SYS,
   dvok LBL, ;

private

\ Shared by the loader (live region) and the snapshot writer (scratch copy):
\ x8 = region base, x15 = record count, x21 = detect base, x22 = detect len,
\ x25 = rebase target base (value - x21 + x25).
: EM-SNAPSHOT-REBASE-DICT ( -- )
   LBL LBL LBL LBL {: sdl2 sdn2 sds2 srn :}
   LSNAPRBD LABEL@ LBL,
   9 8 0 ADDI,  10 0 MOVZ,
   sdl2 LBL,  10 15 CMP,  C-GE sdn2 BCOND,
      13 9 40 LDR,  14 WL:NAMESPACE invert MOVN,  13 14 CMP,  C-EQ sds2 BCOND, \ package [0]/[8] are raw WID roles, never text pointers
      14 WL:RETIRED-NAMESPACE invert MOVN,  13 14 CMP,  C-EQ sds2 BCOND,
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
;package

\ In RELOC-EMIT with the labels above; the bands and exit statuses they read are
\ src/habu/layout.f's SNAP-RELOC.
package RELOC-EMIT
public

\ Region-to-text call relocation, the BL counterpart of the dictionary walk. The
\ writer and the restorer get their region base (kernel) and image base (loader)
\ independently, so calls into __text need relocating; in-region calls do not.
\ Sites come from CALLMAP-OFF (written by callmap-set, habu1.f BCALLMAPSET),
\ never a decode: a compiled word may carry inline data that looks like a call.
\ Writer: scratch copy, live -> canonical distance; loader: live region,
\ canonical -> live.
\   x8  = base of the region image being patched
\   x11 = its byte length (the map is scanned only that far)
\   x10 = signed byte amount to add to every recorded call displacement
\ x10 is a multiple of four, applied in instruction units and re-masked into
\ imm26; EM-MMAP-CODE-REGION's boot assertion keeps the result in BL reach.
\ Clobbers x3..x7, x9, x12..x14; x8/x10/x11 survive for the caller.
: EMIT-CALLS ( -- )
   LBL LBL LBL LBL LBL LBL {: scl:label scdone:label scnext:label
                              scbit:label scbnext:label scbad:label :}
   LCALLS LABEL@ LBL,
   6 SNAP-RELOC:CALLMAP-OFF LIT64,  6 DATA 6 ADD,   \ x6 = call-site map base
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
            14 14 4 ADDI,  14 11 CMP,  C-HI scbad BCOND,
            14 14 4 SUBI,                            \ the complete site must be inside the extent
            14 8 14 ADD,                            \ x14 = site address
            9 14 0 LDRW,
            \ A recorded site must still be a BL or a tail call's B,
            \ which differ only in bit 31; anything else is a corrupt image.
            3 9 26 LSRI,  3 3 $1F ANDI,  3 BL-OP-HI $1F and CMPI,  C-NE scbad BCOND,
            3 9 38 LSLI,  3 3 38 LSRI,              \ x3 = current displacement, in instructions
            3 3 7 ADD,  3 3 38 LSLI,  3 3 38 LSRI,  \ shifted by the delta, back into 26 bits
            9 9 26 LSRI,  9 9 26 LSLI,  9 9 3 ORR,  9 14 0 STRW,
         scbnext LBL,
         5 5 1 LSRI,  4 4 1 ADDI,  scbit B,
   scnext LBL,  12 12 1 ADDI,  scl B,
   scbad LBL,
      1 LCALLMSG LABEL@ ADR,  0 2 MOVZ,  2 CALLMSG-LEN MOVZ,  NR-WRITE SYS,
      0 SNAP-RELOC:CALLMAP-RC MOVZ,  NR-EXIT-GROUP SYS,
   scdone LBL,  RET, ;

\ Saves x0-x17, all MARK writes: its callers are mid-handler or mid-word.
: MARK-SAVE ( -- )
   SP SP 192 SUBI,
   18 0 ?do i SP i cells STR, loop
   16 DATA-VA VA>N LIT64,  16 SP 184 STR, ;

: MARK-RESTORE ( -- )
   18 0 ?do i SP i cells LDR, loop
   SP SP 192 ADDI, RET, ;

\ x16 = the table's base from SP+184: DATA-VA, never a task's x20, or the open
\ image's base for a cell inside it. SYS, clobbers x16: reload after each.
: MARK-HEADER ( -- )
   16 SP 184 LDR,
   4 SNAP-RELOC:XTCELL-N-CELL LIT64, 4 16 4 ADD, ;

\ One owner serializes lookup and publication, including backing replacement.
\ x16 is the table's base; all registers touched here are MARK-SAVE'd.
: MARK-LOCK ( -- )
   LBL {: retry:label :}
   17 16 ADDRESS-CELLS:LOCK-CELL ADDI,
   7 1 MOVZ,
   retry LBL, 6 0 MOVZ,
   $C8E6FE27 EMITW                                   \ CASAL x6,x7,[x17]
   6 retry CBNZ, ;

: MARK-UNLOCK ( -- )
   17 16 ADDRESS-CELLS:LOCK-CELL ADDI,
   6 0 MOVZ, 6 17 STLR, ;

\ Reacquire the row backing after SYS,. Only ordinals live in the index, so
\ moving this backing does not invalidate any entry.
: MARK-ROWS ( -- )
   5 4 ADDRESS-CELLS:BASE-FIELD LDR,
   11 4 ADDRESS-CELLS:MODE-FIELD LDR,
   LBL {: mapped:label :}
   11 mapped CBNZ, 5 16 5 ADD, mapped LBL, ;

\ x8 = owned index mapping; x10 = slot capacity on return. Requested bytes
\ and geometric rounding are bounded separately. An incoming snapshot pointer
\ is cleared without entering here: only this process can own the mapping.
: INDEX-SHAPE ( label -- ) {: bad:label :}
   8 0 CMPI, C-LE bad BCOND,
   6 8 7 ANDI, 6 bad CBNZ,
   7 $7FFFFFFFFFFFFFFF ADDRESS-CELLS:INDEX-HEADER - LIT64,
   8 7 CMP, C-HI bad BCOND,
   10 8 ADDRESS-CELLS:INDEX-SLOTS LDR,
   10 2 CMPI, C-LT bad BCOND,
   7 ADDRESS-CELLS:INDEX-MAX-SLOTS LIT64, 10 7 CMP, C-HI bad BCOND,
   7 10 1 SUBI, 6 10 7 AND, 6 bad CBNZ,
   6 $7FFFFFFFFFFFFFFF LIT64, 6 6 8 SUB,
   7 10 3 LSLI, 7 7 ADDRESS-CELLS:INDEX-HEADER ADDI,
   7 6 CMP, C-HI bad BCOND, ;

\ x8 = index, x10 = slots, x5 = current rows, x13 = row count, x12 = key.
\ A slot holds ordinal+1; compare the exact offset in that authoritative row.
\ Return x7 = tagged row on a hit, or x6 = empty slot / x11 = its bucket.
\ Probe at most the capacity even if a malformed derived table has no hole.
: INDEX-PROBE ( label label label -- ) {: hit:label miss:label bad:label :}
   LBL {: probe:label :}
   7 $9E3779B97F4A7C15 LIT64, 11 12 7 MUL,
   7 11 32 LSRI, 11 11 7 EOR,
   17 10 1 SUBI, 11 11 17 AND, 14 10 0 ADDI,
   probe LBL,
      14 bad CBZ,
      6 11 3 LSLI, 6 8 6 ADD, 6 6 ADDRESS-CELLS:INDEX-HEADER ADDI,
      9 6 0 LDR, 9 miss CBZ,
      9 9 1 SUBI, 9 13 CMP, C-CS bad BCOND,
      7 9 3 LSLI, 7 5 7 ADD, 7 7 0 LDR,
      9 SNAP-RELOC:XTCELL-OFF-MASK LIT64, 9 7 9 AND,
      9 12 CMP, C-EQ hit BCOND,
      11 11 1 ADDI, 11 11 17 AND,
      14 14 1 SUBI, probe B, ;

\ Build a replacement solely from the ordered rows. It is complete before the
\ old index is unmapped; failure leaves authoritative rows/header untouched.
\ Existing duplicate keys retain their first ordinal, exactly as the old scan.
\ Stack scratch: 144 new slots, 152 new map, 160 incoming key, 168 old map.
: INDEX-BUILD ( label label label -- ) {: lookup:label full:label bad:label :}
   LBL LBL LBL LBL LBL LBL LBL LBL LBL
   {: size:label sized:label mapped:label rows:label hole:label next:label
      built:label publish:label released:label :}
   8 SP 168 STR, 12 SP 160 STR,
   7 ADDRESS-CELLS:INDEX-MAX-SLOTS 2 / 1- LIT64,
   13 7 CMP, C-HI full BCOND,
   7 13 1 ADDI, 7 7 1 LSLI, 10 2 MOVZ,
   size LBL, 10 7 CMP, C-CS sized BCOND,
      6 ADDRESS-CELLS:INDEX-MAX-SLOTS 2 / LIT64,
      10 6 CMP, C-HI full BCOND,
      10 10 1 LSLI, size B,
   sized LBL,
   10 SP 144 STR,
   0 0 MOVZ, 1 10 3 LSLI, 1 1 ADDRESS-CELLS:INDEX-HEADER ADDI,
   2 3 MOVZ, 3 MAP-ANON-PRIVATE LIT64, 4 0 MOVN, 5 0 MOVZ,
   NR-MMAP SYS, C-CS full BCOND,
   0 mapped CBNZ,
      1 SP 144 LDR, 1 1 3 LSLI, 1 1 ADDRESS-CELLS:INDEX-HEADER ADDI,
      NR-MUNMAP SYS, full B,
   mapped LBL,
   0 SP 152 STR, 8 0 0 ADDI,
   MARK-HEADER MARK-ROWS
   10 SP 144 LDR,
   10 8 ADDRESS-CELLS:INDEX-SLOTS STR,
   13 8 ADDRESS-CELLS:INDEX-COUNT STR,
   3 0 MOVZ,
   rows LBL, 3 13 CMP, C-CS built BCOND,
      7 3 3 LSLI, 7 5 7 ADD, 12 7 0 LDR,
      7 SNAP-RELOC:XTCELL-OFF-MASK LIT64, 12 12 7 AND,
      next hole bad INDEX-PROBE
   hole LBL, 7 3 1 ADDI, 7 6 0 STR,
   next LBL, 3 3 1 ADDI, rows B,
   built LBL,
   0 SP 168 LDR, 0 publish CBZ,
   1 0 ADDRESS-CELLS:INDEX-SLOTS LDR,
   1 1 3 LSLI, 1 1 ADDRESS-CELLS:INDEX-HEADER ADDI,
   NR-MUNMAP SYS, 0 released CBZ,
      0 SP 152 LDR, 1 SP 144 LDR,
      1 1 3 LSLI, 1 1 ADDRESS-CELLS:INDEX-HEADER ADDI,
      NR-MUNMAP SYS, full B,
   released LBL,
   MARK-HEADER
   publish LBL,
   8 SP 152 LDR, 8 16 ADDRESS-CELLS:INDEX-CELL STR,
   12 SP 160 LDR,
   MARK-ROWS
   lookup B, ;

\ The row was written and every fallible allocation/release is over. Publish
\ its derived entry and finally the live count while still holding the mutex.
: MARK-COMMIT-INDEX ( -- )
   8 16 ADDRESS-CELLS:INDEX-CELL LDR,
   11 SP 176 LDR, 7 11 3 LSLI,
   7 8 7 ADD, 7 7 ADDRESS-CELLS:INDEX-HEADER ADDI,
   13 13 1 ADDI, 13 7 0 STR,
   13 8 ADDRESS-CELLS:INDEX-COUNT STR,
   13 4 0 STR, ;

\ Raise register rd to DATA plus the DATA floor when it is lower, through the
\ scratch register rt: a rewind of DATA puts DP no lower than the end of the last
\ segment the literal store published (layout.f DATA-FLOOR-CELL).
: FLOOR-CLAMP, ( n n -- ) {: rd:n rt:n :}
   rt DATA DATA-FLOOR-CELL LDR,  rt DATA rt ADD,
   rd rt CMP,  rd rd rt C-HI CSEL, ;

\ x12 = absolute DP cut. Preserve x0..x17 for both recovery callers.
: EMIT-ROLLBACK ( -- )
   LBL LBL LBL {: addrfull:label no-owner:label missing:label :}
   LROLLBACK LABEL@ LBL,
   \ Registration order is not address order: a late xt! can declare an
   \ older cell. Keep every row below the cut, preserving its relative order.
   \ The index records its row count; a reduced count makes the next marker
   \ rebuild it before lookup, so stale ordinals cannot survive this cut.
   MARK-SAVE
   SP SP 16 SUBI,  30 SP 0 STR,
   11 DATA NCOMP-DISPATCH:TARGET-DECL-CELL LDR,  11 no-owner CBZ,
   11 11 CHECKER-OWNER-ABI:GRID-REVOKE-OFF LDR,  11 missing CBZ,
   9 12 DATA SUB,  9 G-PUSH  11 BLR,
   no-owner LBL,
   30 SP 0 LDR,  SP SP 16 ADDI,
   12 SP 96 LDR,
   MARK-HEADER MARK-LOCK MARK-ROWS
   12 12 16 SUB,                                  \ cut as DATA offset
   13 4 0 LDR, 10 4 ADDRESS-CELLS:CAP-FIELD LDR,
   LBL LBL LBL LBL LBL LBL LBL
   {: acopy:label acloop:label acdone:label afilter:label
      arloop:label arnext:label ardone:label :}
   11 afilter CBNZ,                               \ mapped backing survives DP rewind
   6 5 16 SUB, 6 12 CMP, C-HI acopy BCOND,
   7 12 6 SUB, 7 7 3 LSRI, 10 7 CMP, C-LS afilter BCOND,
acopy LBL,
   \ A snapshot/PERSIST may have placed the row backing in the abandoned
   \ DATA suffix. Move that existing backing outside DATA before rewinding.
   5 SP 144 STR, 10 SP 152 STR, 12 SP 160 STR, 13 SP 168 STR,
   0 0 MOVZ, 1 10 3 LSLI, 2 3 MOVZ,
   3 MAP-ANON-PRIVATE LIT64, 4 0 MOVN, 5 0 MOVZ,
   NR-MMAP SYS, C-CS addrfull BCOND, 0 addrfull CBZ,
   17 0 0 ADDI,
   MARK-HEADER
   5 SP 144 LDR, 10 SP 152 LDR, 12 SP 160 LDR, 13 SP 168 LDR,
   3 0 MOVZ,
acloop LBL,
   3 13 CMP, C-CS acdone BCOND,
   6 3 3 LSLI, 7 5 6 ADD, 7 7 0 LDR,
   6 17 6 ADD, 7 6 0 STR,
   3 3 1 ADDI, acloop B,
acdone LBL,
   17 4 ADDRESS-CELLS:BASE-FIELD STR,
   6 1 MOVZ, 6 4 ADDRESS-CELLS:MODE-FIELD STR,
   5 17 0 ADDI,
afilter LBL,
   3 0 MOVZ, 10 0 MOVZ,
   6 SNAP-RELOC:XTCELL-OFF-MASK LIT64,
arloop LBL,
   3 13 CMP, C-CS ardone BCOND,
   9 3 3 LSLI, 9 5 9 ADD, 7 9 0 LDR,
   8 7 6 AND, 8 12 CMP, C-CS arnext BCOND,
   9 10 3 LSLI, 9 5 9 ADD, 7 9 0 STR,
   10 10 1 ADDI,
arnext LBL,
   3 3 1 ADDI, arloop B,
ardone LBL,
   10 4 0 STR,
   MARK-UNLOCK
   MARK-RESTORE
   addrfull LBL,
   1 LXTMSG LABEL@ ADR, 0 2 MOVZ,
   2 XTMSG-LEN MOVZ, NR-WRITE SYS,
   0 SNAP-RELOC:XTCELL-RC MOVZ, NR-EXIT-GROUP SYS,
   missing LBL,
   9 -8574 LIT64,  9 G-PUSH  BTHROW ;

\ (MARK) spans [LMARK, end) as one sealed record: xt!'s direct BL lands on its entry.
: EMIT-MARK ( -- )
   LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL
   {: common:label lookup:label full:label band:label kind:label
      ret:label shape:label mapped:label ready:label grow:label copy:label
      copied:label publish:label rebuild:label hit:label missing:label :}
   LMARK LABEL@ {: start:label :}
   LBL {: end:label :}
   s" (MARK)" start LABEL>N end LABEL>N ENGINE-PRIMS:HELPER-REGISTER
   start LBL, MARK-SAVE
   15 0 MOVZ, common B,
   LSCRATCHMARK LABEL@ LBL, MARK-SAVE
   15 SNAP-RELOC:XTCELL-DATA-TAG SNAP-RELOC:XTCELL-SCRATCH-TAG or LIT64,
   common B,
   LPTRMARK LABEL@ LBL, MARK-SAVE
   15 SNAP-RELOC:XTCELL-DATA-TAG LIT64,
   \ x9 = the cell, unchanged on return; x15 = its kind (layout.f XTCELL-DATA-TAG).
   common LBL,
   LBL {: host:label :}
   6 DATA BOOT:IMAGE-BASE-CELL LDR,  6 host CBZ,      \ x6 = the image's base
   9 6 CMP,  C-CC host BCOND,
   7 DATA-SIZE LIT64,  7 6 7 ADD,  9 7 CMP,  C-CS host BCOND,
   6 SP 184 STR,
   host LBL,
   MARK-HEADER
   \ A cell outside DATA and the open image, or a table that cannot grow, stops
   \ the process: a dropped row would leave a stale address in a saved image.
   12 9 16 SUB,
   6 SNAP-RELOC:XTCELL-OFF-MAX LIT64, 12 6 CMP, C-HI band BCOND,
   MARK-LOCK
   6 4 ADDRESS-CELLS:MAGIC-FIELD LDR,
   7 ADDRESS-CELLS:MAGIC LIT64, 6 7 CMP, C-NE shape BCOND,
   13 4 0 LDR, 10 4 ADDRESS-CELLS:CAP-FIELD LDR,
   7 ADDRESS-CELLS:MAX-ROWS LIT64,
   10 shape CBZ, 10 7 CMP, C-HI shape BCOND,
   13 10 CMP, C-HI shape BCOND,
   5 4 ADDRESS-CELLS:BASE-FIELD LDR,
   11 4 ADDRESS-CELLS:MODE-FIELD LDR,
   11 1 CMPI, C-EQ mapped BCOND, C-HI shape BCOND,
   6 DATA-SIZE LIT64, 5 6 CMP, C-HI shape BCOND,
   6 6 5 SUB, 6 6 3 LSRI, 10 6 CMP, C-HI shape BCOND,
   5 16 5 ADD, ready B,
   mapped LBL,
   5 0 CMPI, C-LE shape BCOND,
   6 5 7 ANDI, 6 shape CBNZ,
   6 $7FFFFFFFFFFFFFFF LIT64, 6 6 5 SUB, 6 6 3 LSRI,
   10 6 CMP, C-HI shape BCOND,
   ready LBL,
   8 16 ADDRESS-CELLS:INDEX-CELL LDR, 8 rebuild CBZ,
   shape INDEX-SHAPE
   7 8 ADDRESS-CELLS:INDEX-COUNT LDR,
   7 13 CMP, C-NE rebuild BCOND,
   7 10 1 LSRI, 13 7 CMP, C-HI shape BCOND,
   lookup LBL,
   8 16 ADDRESS-CELLS:INDEX-CELL LDR,
   10 8 ADDRESS-CELLS:INDEX-SLOTS LDR,
   hit missing shape INDEX-PROBE
   \ A cell declared again keeps one row, since a second would relocate it
   \ twice; a second kind stops the process.
   hit LBL,
      6 12 15 ORR, 7 6 CMP, C-EQ ret BCOND, kind B,
   missing LBL,
      7 13 1 ADDI, 6 10 1 LSRI, 7 6 CMP, C-HI rebuild BCOND,
      11 SP 176 STR,
      10 4 ADDRESS-CELLS:CAP-FIELD LDR,
      13 10 CMP, C-EQ grow BCOND,
      6 13 3 LSLI, 6 5 6 ADD, 7 12 15 ORR, 7 6 0 STR,
      MARK-COMMIT-INDEX ret B,
   rebuild LBL,
      lookup full shape INDEX-BUILD
   grow LBL,
      \ Overflow checks precede doubling and the byte product. mmap and copying
      \ complete before publication; failure preserves the old header and rows.
      7 ADDRESS-CELLS:MAX-ROWS 2 / LIT64,
      10 7 CMP, C-HI full BCOND,
      10 10 1 LSLI, 10 SP 144 STR,
      0 0 MOVZ, 1 10 3 LSLI, 2 3 MOVZ,
      3 MAP-ANON-PRIVATE LIT64, 4 0 MOVN, 5 0 MOVZ,
      NR-MMAP SYS, C-CS full BCOND,
      0 copied CBNZ,
      10 SP 144 LDR, 1 10 3 LSLI, NR-MUNMAP SYS, full B,
   copied LBL,
      0 SP 152 STR, 17 0 0 ADDI,
      MARK-HEADER
      5 4 ADDRESS-CELLS:BASE-FIELD LDR,
      11 4 ADDRESS-CELLS:MODE-FIELD LDR,
      LBL {: source:label :} 11 source CBNZ, 5 16 5 ADD, source LBL,
      14 0 MOVZ,
   copy LBL, 14 13 CMP, C-CS publish BCOND,
      6 14 3 LSLI, 7 5 6 ADD, 7 7 0 LDR,
      6 17 6 ADD, 7 6 0 STR,
      14 14 1 ADDI, copy B,
   publish LBL,
      \ An owned mapping is released only after the replacement is complete.
      \ A failed release discards the replacement and leaves the old view live.
      LBL LBL {: commit:label released:label :}
      11 commit CBZ,
      0 5 0 ADDI, 1 4 ADDRESS-CELLS:CAP-FIELD LDR, 1 1 3 LSLI,
      NR-MUNMAP SYS, 0 released CBZ,
      0 SP 152 LDR, 1 SP 144 LDR, 1 1 3 LSLI,
      NR-MUNMAP SYS, full B,
   released LBL,
      MARK-HEADER
   commit LBL,
      5 SP 152 LDR, 10 SP 144 LDR,
      6 13 3 LSLI, 6 5 6 ADD, 7 12 15 ORR, 7 6 0 STR,
      5 4 ADDRESS-CELLS:BASE-FIELD STR,
      10 4 ADDRESS-CELLS:CAP-FIELD STR,
      11 1 MOVZ, 11 4 ADDRESS-CELLS:MODE-FIELD STR,
      MARK-COMMIT-INDEX ret B,
   full LBL,
      1 LXTMSG LABEL@ ADR, 0 2 MOVZ, 2 XTMSG-LEN MOVZ, NR-WRITE SYS,
      0 SNAP-RELOC:XTCELL-RC MOVZ, NR-EXIT-GROUP SYS,
   shape LBL,
      1 LXTHEADERMSG LABEL@ ADR, 0 2 MOVZ, 2 XTHEADERMSG-LEN MOVZ, NR-WRITE SYS,
      0 SNAP-RELOC:XTCELL-RC MOVZ, NR-EXIT-GROUP SYS,
   band LBL,
      1 LXTBANDMSG LABEL@ ADR, 0 2 MOVZ, 2 XTBANDMSG-LEN MOVZ, NR-WRITE SYS,
      0 SNAP-RELOC:XTBAND-RC MOVZ, NR-EXIT-GROUP SYS,
   kind LBL,
      1 LXTKINDMSG LABEL@ ADR, 0 2 MOVZ, 2 XTKINDMSG-LEN MOVZ, NR-WRITE SYS,
      0 SNAP-RELOC:XTKIND-RC MOVZ, NR-EXIT-GROUP SYS,
   ret LBL, MARK-UNLOCK MARK-RESTORE
   end LBL, ;

: BVERSION ( -- ) A ADDRESS-CELLS:ABI-VERSION MOVZ, A G-PUSH ;
: BSNAPSHOTFORMAT ( -- ) A SNAPSHOT-FORMAT:VERSION MOVZ, A G-PUSH ;

: BPTRCELLMARK ( -- )
   A G-POP
   LPTRMARK LABEL@ BL, ;

: BSCRATCHCELLMARK ( -- )
   A G-POP
   LSCRATCHMARK LABEL@ BL, ;

\ While a build writes an image, a store into this engine's header goes to the
\ same offset in the image, and marks the cell: its address goes to the cell ROUTED-BYTES
\ below it and to the cell below the image, in the band the build maps there (linker.f MAP).
\ Clobbers x12 and x13, as the store's guard does.
: ROUTE-HEADER ( n -- ) {: reg:n :}
   LBL {: own:label :}
   13 reg DATA SUB,  12 DATA-START LIT64,  13 12 CMP,  C-CS own BCOND,
   12 DATA BOOT:IMAGE-BASE-CELL LDR,  12 own CBZ,
   reg 12 13 ADD,
   13 12 8 SUBI,  reg 13 0 STR,
   12 ROUTED-BYTES LIT64,  13 reg 12 SUB,  reg 13 0 STR,
   own LBL, ;

\ xt! ( q ptr q -- ): store an execution token into a persisted cell and declare
\ the cell. `defer` and `is` declare cells known at compile time; a table of
\ dispatch cells picks its cell at run time, so only the store knows. One
\ primitive keeps the declaration from drifting from the store. The store is
\ habu1.f BSTORE's, guard included (x10 = cell, x9 = token). The declaration
\ runs first, so a cell LMARK refuses is never written; LMARK wants the cell in
\ x9, so the token is parked in x11 (scratch here; LMARK preserves x9 and saves
\ the rest). Registered framed: the guard and LMARK are both BL.
: BXTSTORE ( -- )
   B G-POP  A G-POP
   B ROUTE-HEADER
   7 8 MOVZ,  B 7 PROT-GUARD:CALL
   C A 0 ADDI,                                      \ x11 = the token
   A B 0 ADDI,                                      \ x9 = the cell, for LMARK
   LMARK LABEL@ BL,
   C B 0 STR, ;

\ ptr-cell! ( a ptr -- ): BXTSTORE for a DATA-pointer cell; see BXTSTORE.
: BPTRCELLSTORE ( -- )
   B G-POP  A G-POP
   B ROUTE-HEADER
   7 8 MOVZ,  B 7 PROT-GUARD:CALL
   C A 0 ADDI,                                      \ x11 = the pointer
   A B 0 ADDI,                                      \ x9 = the cell, for LPTRMARK
   LPTRMARK LABEL@ BL,
   C B 0 STR, ;

\ Record the MOVZ/MOVK address carrier whose first word is in x9.
\ Compile handlers supply CP before emission; seed relocation supplies its site.
\ Preserve x11, the literal value held by compile and seed callers. Both callers
\ have no live x30 across this call; compile handlers also call LCEMIT afterward.
\ Clobbers x5, x6, x9.
: EMIT-ADDR-SITE ( -- )
   LADDRSITE LABEL@ LBL,
   6 9 DBASE SUB,                                   \ x6 = chain's byte offset within the region
   9 6 0 ADDI,  9 9 2 LSRI,  9 9 7 ANDI,            \ x9 = bit number = word index & 7
   6 6 5 LSRI,                                      \ x6 = map byte index = offset >> 5
   5 SNAP-RELOC:ADDRMAP-OFF LIT64,  6 6 5 ADD,  6 DATA 6 ADD, \ x6 = map byte address
   5 1 MOVZ,  5 5 9 LSLV,  9 6 0 LDRB,  9 9 5 ORR,  9 6 0 STRB,
   RET, ;

\ Address-literal relocation: the MOVZ/MOVK counterpart of the dictionary walk
\ and the call pass. A `[: ... ;]` or `[']` chain names code in the JIT region or
\ in __text, which move independently, so the pass is called once per band:
\   x21 = base of the band being moved
\   x22 = its length
\   x25 = the base those addresses are moving to  (value - x21 + x25)
\   x8  = base of the region image being patched
\   x11 = its byte length (the map is scanned only that far)
\ The bands are disjoint at write and restore (the region sits REGION-OFF above
\ __text; canonical text offsets lie far below the RBASE-VA sentinel), so each
\ chain is rewritten by one call; a chain in neither band is left alone. Sites
\ come from ADDRMAP-OFF (EMIT-ADDR-SITE, from C-CODE-ADDR and the AOT seed's
\ code-literal rebase), never a decode: inline data and DATA literals share the
\ chain's shape.
\ Clobbers x2, x3, x4, x5, x6, x7, x9, x10, x12, x13, x14; x8, x11, x21, x22 and
\ x25 survive for the caller.
: EMIT-ADDRS ( -- )
   LBL LBL LBL LBL LBL LBL LBL {: sal:label sadone:label sanext:label
                              sabit:label sabnext:label sabad:label full:label :}
   LADDRS LABEL@ LBL,
   6 SNAP-RELOC:ADDRMAP-OFF LIT64,  6 DATA 6 ADD,   \ x6 = address-literal map base
   13 11 31 ADDI,  13 13 5 LSRI,                    \ x13 = map bytes covering the image (rounded up)
   12 0 MOVZ,                                       \ x12 = map byte index
   7 ADDRESS-CARRIER:ADDR-OPC-MASK LIT64,          \ x7 = an instruction word minus its immediate
   2 ADDRESS-CARRIER:ADDR-IMM-MASK LIT64,          \ x2 = one 16-bit immediate field
   sal LBL,  12 13 CMP,  C-GE sadone BCOND,
      5 6 12 ADD,  5 5 0 LDRB,                      \ x5 = map byte, consumed one bit at a time
      5 sanext CBZ,                                 \ no site recorded in these eight words
      4 12 3 LSLI,                                  \ x4 = region word index of this byte's first bit
      sabit LBL,  5 sanext CBZ,                     \ every remaining bit is clear
         14 5 1 ANDI,  14 sabnext CBZ,
            14 4 2 LSLI,                            \ x14 = chain's byte offset within the region
            3 14 ADDRESS-CARRIER:DATA-CHAIN-BYTES ADDI, \ the shortest carrier must fit before its words are read
            3 11 CMP,  C-HI sabnext BCOND,          \ past this payload: not part of the image being patched
            14 8 14 ADD,                            \ x14 = chain address
            \ A recorded site must still be the chain, else the image is corrupt.
            \ Word 0 supplies the register (x3) and the other three lanes are
            \ compared against a scaffold carrying it, so four move-wides naming
            \ different registers are refused too.
            9 14 0 LDRW,   9 9 7 AND,
            3 9 ADDRESS-CARRIER:ADDR-RD-MASK ANDI,
            10 ADDRESS-CARRIER:DATA-MOVZ2 LIT64,  10 10 3 ORR,
            9 10 CMP,  C-NE full BCOND,
            \ A shared DATA address is validated but never CODE-rebased.
            9 14 4 LDRW,  9 9 7 AND,
            10 $F2A00000 LIT64,  10 10 3 ORR,  9 10 CMP,  C-NE sabad BCOND,
            9 14 8 LDRW,  9 9 7 AND,
            10 ADDRESS-CARRIER:DATA-MOVK0 LIT64,  10 10 3 ORR,  9 10 CMP,  C-NE sabad BCOND,
            sabnext B,
            full LBL,
            10 4 2 LSLI,  10 10 ADDRESS-CARRIER:ADDR-CHAIN-BYTES ADDI,
            10 11 CMP,  C-HI sabad BCOND,
            10 W-MOVZ0 LIT64,  10 10 ADDRESS-CARRIER:ADDR-RD-BITS LSRI,  10 10 ADDRESS-CARRIER:ADDR-RD-BITS LSLI,  10 10 3 ORR,
            9 10 CMP,  C-NE sabad BCOND,
            9 14 4 LDRW,   9 9 7 AND,
            10 W-MOVK1 LIT64,  10 10 ADDRESS-CARRIER:ADDR-RD-BITS LSRI,  10 10 ADDRESS-CARRIER:ADDR-RD-BITS LSLI,  10 10 3 ORR,
            9 10 CMP,  C-NE sabad BCOND,
            9 14 8 LDRW,   9 9 7 AND,
            10 W-MOVK2 LIT64,  10 10 ADDRESS-CARRIER:ADDR-RD-BITS LSRI,  10 10 ADDRESS-CARRIER:ADDR-RD-BITS LSLI,  10 10 3 ORR,
            9 10 CMP,  C-NE sabad BCOND,
            9 14 12 LDRW,  9 9 7 AND,
            10 W-MOVK3 LIT64,  10 10 ADDRESS-CARRIER:ADDR-RD-BITS LSRI,  10 10 ADDRESS-CARRIER:ADDR-RD-BITS LSLI,  10 10 3 ORR,
            9 10 CMP,  C-NE sabad BCOND,
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
      0 SNAP-RELOC:ADDRMAP-RC MOVZ,  NR-EXIT-GROUP SYS,
   sadone LBL,  RET, ;

;package

package ENGINE-EMIT

\ snap-rebase ( base end count dbase dlen newbase -- ): canonicalize a region copy
\ [base,end) in two passes so the image is byte-identical whatever VAs this run
\ got. Pass 1 folds engine-text pointers (dbase,dlen) to newbase (0); pass 2 folds
\ the live JIT region [DBASE, DBASE+end-base) to the RBASE-VA sentinel. The bands
\ are disjoint (REGION-OFF >> text size). Pool
\ registers are spilled before any prim call, so clobbering
\ x8/x15/x16/x21/x22/x25 is safe. The half-open span guard rejects every
\ protected-band intersection; the builder uses a high scratch copy.
: BSNAPREBASE ( -- )
   25 G-POP  22 G-POP  21 G-POP  15 G-POP  16 G-POP  8 G-POP
   11 16 8 SUB,  8 11 PROT-GUARD:CALL
   LSNAPRBD LABEL@ BL,
   \ Text band: a `[']` may name a primitive in __text, so pass 1's triple
   \ applies to address chains too.
   11 16 8 SUB,                                         \ x11 = region payload length
   RELOC-EMIT:LADDRS LABEL@ BL,
   \ Rewrite region-to-text calls to the distance they would have with the
   \ region exactly REGION-OFF above __text. x21 still holds the live text base.
   10 DBASE 21 SUB,  9 REGION-OFF LIT64,  10 10 9 SUB,  \ x10 = live distance - REGION-OFF
   11 16 8 SUB,                                         \ x11 = region payload length
   RELOC-EMIT:LCALLS LABEL@ BL,
   21 DBASE 0 ADDI,  22 16 8 SUB,  25 RBASE-VA LIT64,   \ pass 2: live region -> RBASE-VA sentinel
   LSNAPRBD LABEL@ BL,
   \ Region band: quotation entries and xts of region words, folded to the
   \ sentinel pass 2 uses.
   11 16 8 SUB,
   RELOC-EMIT:LADDRS LABEL@ BL, ;

public

\ The running image's text content size, read from its header, for layout.f
\ TEXT-SIZE-CELL. Stored with
\ CODE-END-CELL at both of its sites.
: EM-TEXT-SIZE ( -- )
   9 DATA RBASE-CELL LDR,  10 CODE-OFF LIT64,  9 9 10 SUB,
   9 9 IMAGE-TEXT-SIZE-OFF LDR,  10 IMAGE-TEXT-CONTENT-ADJ LIT64,  9 9 10 SUB,
   9 DATA TEXT-SIZE-CELL STR, ;

private

\ Checker package-scope resync drain. A throw's recovery rolls the engine
\ package scope back (PKGSNAP / RPKG), but the checker's own mirror
\ (CHECKER-PACKAGE-MODE/NAME/U) moved with the failed input. Recovery arms
\ PKGRESYNC-CELL; this drain clears it and has the checker re-read the restored
\ scope (checker.f CHECKER-RESYNC) whatever it is: resyncing only a global scope
\ left a package open across the boundary refusing E-PKG-CONTEXT. LEVALREC
\ drains before delivering the throw.
: EM-PKG-RESYNC ( -- )
   LBL {: nosync:label :}
   9 DATA PKGRESYNC-CELL LDR,  9 nosync CBZ,
      9 0 MOVZ,  9 DATA PKGRESYNC-CELL STR,
      NCOMP-DISPATCH:DECL-PKG-RESYNC-OFF nosync DECL-OWNER:TARGET   \ x11 = the checker's resync (nosync if absent: cold load / no checker)
      C-CALL-X11-SAVED
   nosync LBL, ;

public
\ Definition state only: a Habu catch that survived the engine's BTHROW still owns
\ the handler, return and loop stacks that receive the error.
: EM-ABORT-COMPILE-STATE ( -- )
   9 0 MOVZ,
   9 DATA NCOMP-DISPATCH:DEF-TIER-CELL STR,
   9 DATA NCOMP-DISPATCH:CALLBACK-ENTRY-CELL CELL + NATIVE-BODY-ABI:PENDING-TAKE cells + STR,
   9 DATA LVD-CELL STR,  9 DATA VSP-CELL STR,  9 DATA QPATCH-CELL STR,
   9 DATA FRAME-CELL STR,  9 DATA QFRAME-CELL STR,
   9 DATA JIT-SNAP:SP-CELL STR,   \ the JIT's BEGIN depth dies with the definition
   9 DATA FWDTGT-CELL STR,        \ ...and so does its last forward target
   9 DATA LIDX-CELL STR,          \ ...and its cached loop index
   9 DATA JIT-QUOT:SP-CELL STR,   \ ...and so do its parked quotations
   9 DATA BODYLEN-CELL STR,  9 DATA EXITH-CELL STR,
   9 DATA PEND-CELL STR,  9 DATA CMM-CELL STR,
   9 DATA PEND-OPAQUE-CELL STR,
   9 DATA CMFRD-CELL STR,  9 DATA CMBK-CELL STR,
   9 0 MOVZ,
   9 DATA TSIG-A-CELL STR,   9 DATA TSIG-U-CELL STR,
   9 DATA TCSIG-A-CELL STR,  9 DATA TCSIG-U-CELL STR,
   9 DATA CRSIG-A-CELL STR,  9 DATA CRSIG-U-CELL STR,
   9 DATA DOESB-CELL STR,  9 DATA CLAUSE-CELL STR,
   9 DATA TRUSTED-CELL STR,
   C-CLEAR-LOCALS
   9 VRALL MOVZ,  9 DATA VRFREE-CELL STR, ;
;package

\ The def-abort and stack-clear bodies (src/habu/prims.f); FPRIM-WID frames
\ x30 around each.
package DEFWRITE

public

: DEF-ABORT ( -- )
   EM-ABORT-COMPILE-STATE ;

: STACK-CLEAR ( -- )
   XDS DATA S0-CELL LDR, ;

;package

package NCOMP-EMIT

public

: CAPTURE-PLAIN-STRING ( -- )
   C-QUOTE-START
   C-QUOTE-SCAN
   C-QUOTE-CONSUME
   11 16 0 ADDI,  12 10 1 ADDI,  LBCS LABEL@ BL, ;

: CAPTURE-ESCAPED-STRING ( -- )
   C-QUOTE-START
   C-ESC-QUOTE-SCAN
   C-ESC-QUOTE-CONSUME
   11 16 0 ADDI,  12 15 1 ADDI,  LBCS LABEL@ BL, ;

: CAPTURE-STRING ( -- )
   LCAPBIND LABEL@ LKWSQ     2 ['] CAPTURE-PLAIN-STRING   CFN-ENTRY
   LCAPBIND LABEL@ LKWCQ     2 ['] CAPTURE-PLAIN-STRING   CFN-ENTRY
   LCAPBIND LABEL@ LKWDOTQ   2 ['] CAPTURE-PLAIN-STRING   CFN-ENTRY
   LCAPBIND LABEL@ LKWESQ    3 ['] CAPTURE-ESCAPED-STRING CFN-ENTRY
   LCAPBIND LABEL@ LKWECQ    3 ['] CAPTURE-ESCAPED-STRING CFN-ENTRY
   LCAPBIND LABEL@ LKWEDOTQ  3 ['] CAPTURE-ESCAPED-STRING CFN-ENTRY ;

private

public

;package

package ENGINE-EMIT
public
: C-SOURCE-EXEC-BEFORE ( -- )
   LBL {: absent:label :}
   10 DATA SOURCE-EXEC-CTX-CELL LDR,  10 absent CBZ,
   9 10 SX-ARMED LDR,  9 absent CBZ,
   11 10 SX-EVAL LDR,  12 DATA EVAL-TOP-CELL LDR,
   11 12 CMP,  C-NE absent BCOND,
   9 10 SX-SUSPEND LDR,  9 absent CBNZ,
   9 10 SX-PENDING LDR,  9 absent CBNZ,
   SP SP 80 SUBI,  30 SP 0 STR,
   10 SP 8 STR,  15 SP 16 STR,  16 SP 24 STR,  17 SP 32 STR,
   9 DATA TKA-CELL LDR,  9 SP 40 STR,
   9 DATA TKL-CELL LDR,  9 SP 48 STR,
   9 DATA INP-CELL LDR,  9 SP 56 STR,
   9 DATA INE-CELL LDR,  9 SP 64 STR,
   9 DATA SRCLOC:INB-CELL LDR,  9 SP 72 STR,
   10 SP 8 LDR,  15 SP 16 LDR,  16 SP 24 LDR,  17 SP 32 LDR,
   9 DATA S0-CELL LDR,  9 XDS 9 SUB,  9 9 3 LSRI,  9 10 SX-DEPTH STR,
   9 DATA RSP-CELL LDR,  9 10 SX-RDEPTH STR,
   9 DATA INP-CELL LDR,  11 10 SX-SRC LDR,  9 9 11 SUB,  \ the token's end, for source-exec-position
   9 10 SX-POS STR,
   15 10 SX-OP STR,  16 10 SX-REC STR,  17 10 SX-ENTRY STR,
   9 1 MOVZ,  9 10 SX-SUSPEND STR,
   9 10 SX-UNIT LDR,  9 G-PUSH
   9 SP 16 LDR,  9 G-PUSH
   9 SP 24 LDR,  9 G-PUSH
   9 SP 32 LDR,  9 G-PUSH
   9 SP 40 LDR,  9 G-PUSH
   9 SP 48 LDR,  9 G-PUSH
   10 SP 8 LDR,  9 10 SX-BEFORE LDR,  9 BLR,
   9 G-POP  11 SP 8 LDR,  9 11 SX-WRITES STR,
   9 G-POP  9 11 SX-TICKET STR,
   9 SP 40 LDR,  9 DATA TKA-CELL STR,
   9 SP 48 LDR,  9 DATA TKL-CELL STR,
   9 SP 56 LDR,  9 DATA INP-CELL STR,
   9 SP 64 LDR,  9 DATA INE-CELL STR,
   9 SP 72 LDR,  9 DATA SRCLOC:INB-CELL STR,
   11 SP 8 LDR,
   9 0 MOVZ,  9 11 SX-SUSPEND STR,
   9 1 MOVZ,  9 11 SX-PENDING STR,
   30 SP 0 LDR,  SP SP 80 ADDI,
   absent LBL, ;

: C-TICK ( -- )
   LBL LBL LBL LBL {: tk:label usedtry:label found:label named:label :}
   LTOK LABEL@ BL,
   0 named CBNZ,
      LKWTICK LABEL@ TICK-LEN C-DIE-KEYWORD-NAME
   named LBL,
   C-QUALIFY-SEAL-GUARD                                 \ reject `' RESERVED:tail` once sealed
   9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFIND LABEL@ BL,
   13 usedtry CBZ,                                       \ open-scope + global miss -> try used publics (` ' SUITE` under a `using`)
   INTERP-EMIT:LFINDSHADOW LABEL@ BL,                    \ a global a used public also exports refuses (105)
   found LBL,
   14 13 8 ANDI,  14 LWIDE LABEL@ CBNZ,                  \ `' <wide-effect word>` would launder the bundle past the dispatch gate
   14 13 16 ANDI,  14 LINTERNAL LABEL@ CBNZ,             \ `' <internal word>` would launder the xt past the dispatch gate (execute)
   SP SP 16 SUBI,  5 SP 0 STR,                           \ keep LFIND's record while the guard uses x5 for protected entries
   C-SCOPE-ENTRY-GUARD
   5 SP 0 LDR,  C-HOST-SELECT                            \ a top-level tick is a run fact, as a word the loop runs
   SP SP 16 ADDI,
   C-TRUSTED-TICK?
   17 LTRUSTTICK LABEL@ CBNZ,
   \ A source range observes the tick's actual target before the xt is pushed.
   \ Preserve LFIND's record and entry across the source callback.
   SP SP 32 SUBI,  11 SP 0 STR,  5 SP 8 STR,  13 SP 16 STR,
   16 SP 8 LDR,  16 16 DBASE SUB,
   17 DREC MOVZ,  16 16 17 UDIV,
   15 4 MOVZ,  17 SP 0 LDR,
   C-SOURCE-EXEC-BEFORE
   11 SP 0 LDR,  13 SP 16 LDR,  SP SP 32 ADDI,
   11 G-PUSH
   TOP-EV-TICK C-TOPHOOK-FLAGS
   tk B,
   usedtry LBL,
      9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFINDUSED LABEL@ BL,
      13 LUNDEF LABEL@ CBZ,                              \ nothing resolves it -> undefined, as a bare name and C-BTICK's `['] X` are
      found B,
   tk LBL, ;

private
: EMIT-SOURCE-EXEC-FINISH ( -- )
   LBL LBL LBL LBL LBL
   {: absent:label callback-ok:label keep-original:label done:label current:label :}
   LSOURCEEXECFINISH LABEL@ LBL,
   10 DATA SOURCE-EXEC-CTX-CELL LDR,  10 absent CBZ,
   9 10 SX-PENDING LDR,  9 absent CBZ,
   15 current CBNZ,                    \ a thrown continuation has already left its EVAL frame
   11 10 SX-EVAL LDR,  12 DATA EVAL-TOP-CELL LDR,
   11 12 CMP,  C-NE absent BCOND,
   current LBL,
   SP SP 80 SUBI,  30 SP 0 STR,  15 SP 8 STR,  10 SP 16 STR,
   9 DATA TKA-CELL LDR,  9 SP 24 STR,
   9 DATA TKL-CELL LDR,  9 SP 32 STR,
   9 DATA INP-CELL LDR,  9 SP 40 STR,
   9 DATA INE-CELL LDR,  9 SP 48 STR,
   9 DATA SRCLOC:INB-CELL LDR,  9 SP 56 STR,
   10 SP 16 LDR,
   9 0 MOVZ,  9 10 SX-PENDING STR,
   9 DATA S0-CELL LDR,  9 XDS 9 SUB,  9 9 3 LSRI,  9 10 SX-DEPTH STR,
   9 DATA RSP-CELL LDR,  9 10 SX-RDEPTH STR,
   9 DATA INP-CELL LDR,  11 10 SX-SRC LDR,  9 9 11 SUB,
   9 10 SX-POS STR,
   9 1 MOVZ,  9 10 SX-SUSPEND STR,
   9 10 SX-UNIT LDR,  9 G-PUSH
   9 10 SX-TICKET LDR,  9 G-PUSH
   9 SP 8 LDR,  9 G-PUSH
   10 SP 16 LDR,  9 10 SX-AFTER LDR,  9 G-PUSH
   BCATCH
   9 G-POP  9 callback-ok CBZ,
      XDS XDS 24 SUBI,                     \ catch restored the callback's three args
   callback-ok LBL,
   9 SP 64 STR,
   9 SP 24 LDR,  9 DATA TKA-CELL STR,
   9 SP 32 LDR,  9 DATA TKL-CELL STR,
   9 SP 40 LDR,  9 DATA INP-CELL STR,
   9 SP 48 LDR,  9 DATA INE-CELL STR,
   9 SP 56 LDR,  9 DATA SRCLOC:INB-CELL STR,
   10 SP 16 LDR,  11 0 MOVZ,  11 10 SX-SUSPEND STR,
   15 SP 8 LDR,  15 keep-original CBNZ,
      15 SP 64 LDR,
   keep-original LBL,
   16 SP 8 LDR,
   30 SP 0 LDR,  SP SP 80 ADDI,
   15 done CBZ,
      16 done CBNZ,                        \ source-run returns its caught action status
      9 DATA EVALD-CELL LDR,  9 done CBZ,   \ an after observer threw inside evaluate
      10 DATA EVALREC-CELL LDR,  10 BR,
   done LBL,
   absent LBL,
   RET, ;

\ The body hook (layout.f NCOMP-DISPATCH:BODY-HOOK-CELL), when set, runs with
\ the record pending as def-open leaves one: DEF-TIER-CELL takes the tier latch,
\ which the publication writers (ndict-append) and def-close require to be 1.
\ On true it has installed the definition: the source resumes at the address
\ it answers, past the body's `;`, and the record closes as def-close closes a
\ native one. On false the body compiles.
: C-BODY-HOOK ( -- )
   LBL {: none:label :}
   11 DATA NCOMP-DISPATCH:BODY-HOOK-CELL LDR,  11 none CBZ,
   9 TIER-LATCH  9 DATA NCOMP-DISPATCH:DEF-TIER-CELL STR,
   C-CALL-X11-SAVED
   9 G-POP  10 G-POP                                   \ x9 = installed, x10 = resume
   9 none CBZ,
      10 DATA INP-CELL STR,
      DEFWRITE:DEF-CLOSE
      LMAIN LABEL@ B,
   none LBL, ;

public
: C-TRUSTED ( -- )
   C-TASK-GUARD
   LBL LBL LBL {: cpok ndok named :}
   9 REGION $4000 - LIT64,  9 DBASE 9 ADD,  CP 9 CMP,  C-LT cpok BCOND,
      C-DIE-CODE-FULL
   cpok LBL,
   9 DICT-CAP LIT64,  NDICT 9 CMP,  C-LT ndok BCOND,
      C-DIE-DICT-FULL
   ndok LBL,
   LTOK LABEL@ BL,
   0 named CBNZ,
      LKWTRUSTED LABEL@ KWTRUSTED$ nip C-DIE-KEYWORD-NAME
   named LBL,
   12 0 MOVZ,  12 DATA BODYLEN-CELL STR,
   LBCAP LABEL@ BL,
   C-QUALIFY-DEF
   10 DATA TKA-CELL LDR,  10 DATA PENDTKA-CELL STR,     \ the opener's token (layout.f PENDTKA-CELL)
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   9 DATA PEND-CELL STR,
   C-STORE-DEF-NAME
   11 DATA REGION-ALIAS:DELTA-CELL LDR,  11 9 11 ADD,  CP 11 0 STR,   \ through the alias
   5 CFSTK-OFF LIT64,  11 DBASE 5 ADD,
   5 DATA REGION-ALIAS:DELTA-CELL LDR,  11 11 5 ADD,   \ the depth through the alias
   12 0 MOVZ,  12 11 0 STR,
   12 DATA LOCN-CELL STR,  12 DATA LOCF-CELL STR,  12 DATA CAPDEPTH-CELL STR,
   12 DATA CMM-CELL STR,  12 DATA CMFRD-CELL STR,  12 DATA CMBK-CELL STR,
   C-CLEAR-TRUSTED-STATE
   12 1 MOVZ,  12 DATA TRUSTED-CELL STR,
   C-PARSE-TRUST-SIG
   9 DATA BODYLEN-CELL LDR,  9 DATA P2BODY0-CELL STR,  \ body starts after name+sig
   12 0 MOVZ,  12 DATA VSP-CELL STR,
   12 DATA EXITH-CELL STR,  12 DATA LVD-CELL STR,
   12 DATA QPATCH-CELL STR,  12 DATA FWDTGT-CELL STR,  12 DATA LIDX-CELL STR,
   12 DATA JIT-SNAP:SP-CELL STR,   \ a fresh definition starts at BEGIN depth 0
   12 DATA JIT-QUOT:SP-CELL STR,   \ ...and with no quotation parked
   12 VRALL MOVZ,  12 DATA VRFREE-CELL STR,
   12 FRALL LIT64,  12 DATA FRFREE-CELL STR,
   C-BODY-HOOK
   NCOMP-EMIT:TIER-COLON-DISPATCH ;

private
: C-CALL-CHECKER-PACKAGE ( -- )
   LBL {: done:label :}
   NCOMP-DISPATCH:DECL-PACKAGE-OFF done DECL-OWNER:TARGET
   9 DATA TKA-CELL LDR,  9 G-PUSH
   9 DATA TKL-CELL LDR,  9 G-PUSH
   C-CALL-X11-SAVED
   done LBL, ;

: C-CALL-CHECKER-PUBLIC ( -- )
   LBL {: done:label :}
   NCOMP-DISPATCH:DECL-PUBLIC-OFF done DECL-OWNER:TARGET
   C-CALL-X11-SAVED
   done LBL, ;

: C-CALL-CHECKER-PRIVATE ( -- )
   LBL {: done:label :}
   NCOMP-DISPATCH:DECL-PRIVATE-OFF done DECL-OWNER:TARGET
   C-CALL-X11-SAVED
   done LBL, ;

: C-CALL-CHECKER-END-PACKAGE ( -- )
   LBL {: done:label :}
   NCOMP-DISPATCH:DECL-END-PACKAGE-OFF done DECL-OWNER:TARGET
   C-CALL-X11-SAVED
   done LBL, ;

\ The engine owns the using depth; each checker owns the names at those slots.
\ Notify the active compiler before any distinct target checker, as with the
\ declaration registrars. Dictionary replacement must not retire the compiler's
\ import context while that compiler still resolves source bodies.
: C-CALL-CHECKER-USING ( -- )
   LBL LBL {: target:label done:label :}
   NCOMP-DISPATCH:DECL-USING-OFF DECL-OWNER:FIND
   11 target CBZ,
   TKA-CELL TKL-CELL C-PUSH-TRUST-SIG
   C-CALL-X11-SAVED
   target LBL,
   NCOMP-DISPATCH:DECL-USING-OFF done DECL-OWNER:TARGET
   NCOMP-DISPATCH:DECL-USING-OFF done DECL-OWNER:SKIP-SAME
   TKA-CELL TKL-CELL C-PUSH-TRUST-SIG
   C-CALL-X11-SAVED
   done LBL, ;

: C-PACKAGE-FAIL ( n -- ) {: rc:n :}                  \ package keyword misuse ($4A no name / $4B wrong context): recoverable inside evaluate, fail-closed exit rc at top level
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 rc MOVZ,  LCOMPILEDIE LABEL@ B, ;

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

: C-PACKAGE-NEW-PRIVATE-WID ( -- )
   12 DATA WIDN-CELL LDR,
   13 12 1 ADDI,  13 DATA WIDN-CELL STR, ;

: C-PACKAGE-ALLOC-WIDS ( -- )
   17 DATA WIDN-CELL LDR,
   16 17 1 ADDI,
   15 17 2 ADDI,  15 DATA WIDN-CELL STR, ;

: C-PACKAGE-NEW-RECORD ( -- )
   OVERLAY-EMIT:GUARD,
   C-QUALIFY-CAP
   C-PACKAGE-ALLOC-WIDS
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   C-STORE-NAME
   11 DATA WIDN-CELL LDR,  11 11 2 SUBI,
   12 11 1 ADDI,
   1 DATA REGION-ALIAS:DELTA-CELL LDR,  1 9 1 ADD,     \ x1 = record x9 through the alias
   11 1 0 STR,  12 1 8 STR,
   15 DATA BOOT:NAMESPACE-WL-CELL LDR,  15 1 40 STR,
   EM-DEF-OCC:LAPPEND LABEL@ BL,  LHIDXADD LABEL@ BL,
   5 9 0 ADDI, ;

\ The record this writes is one the scan found anywhere below the watermark
\ (`package NAME` reopening a package rewrites its private-WID cell through the
\ alias).
: C-PACKAGE-EXISTING-PRIVATE ( label -- )
   {: done:label :}
   LBL {: havepri:label :}
   12 havepri CBNZ,
      OVERLAY-EMIT:GUARD,
      C-PACKAGE-NEW-PRIVATE-WID
      1 DATA REGION-ALIAS:DELTA-CELL LDR,  1 5 1 ADD,  12 1 8 STR,   \ through the alias
   havepri LBL,
   done B, ;

: C-PACKAGE-RECORD-MATCH ( label label -- ) {: hit:label miss:label :}
   LBL LBL {: cmp:label inline:label :}
   14 5 40 LDR,  15 DATA BOOT:NAMESPACE-WL-CELL LDR,  14 15 CMP,  C-NE miss BCOND,
   14 5 16 LDR,  14 14 DNAME-FLAG-BITS LSLI,  14 14 DNAME-FLAG-BITS LSRI,
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

: C-PACKAGE-ENSURE ( -- )
   LBL LBL LBL LBL LBL
   {: loop:label miss:label hit:label make:label done:label :}
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

\ Under the design seal (layout.f POLICY-NDICT-CELL) `package NAME` is refused
\ when NAME's record sits below the stored NDICT: reopening a package that
\ existed at the seal would search its private words before any admitted public
\ word. A package the sealed source made lies above the seal and opens as
\ usual. The refusal is the seal's located line and ENGINE-ERROR:POLICY through
\ LCOMPILEDIE.
: C-PACKAGE-POLICY-GUARD ( -- )
   LBL LBL LBL LBL {: loop:label miss:label hit:label done:label :}
   LBL s" hb: policy: package " {: pre:label pa:ptr pu:n :}
   LBL s"  predates the seal" {: post:label sa:ptr su:n :}
   6 DATA POLICY-NDICT-CELL LDR,  6 done CBZ,           \ unsealed: nothing predates a seal
   5 DBASE 0 ADDI,
   loop LBL,
      6 done CBZ,
      hit miss C-PACKAGE-RECORD-MATCH
      hit LBL,
         0 2 MOVZ,  1 pre ADR,  2 pu MOVZ,  NR-WRITE SYS,
         0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
         0 2 MOVZ,  1 post ADR,  2 su MOVZ,  NR-WRITE SYS,
         0 ENGINE-ERROR:POLICY MOVZ,  LCOMPILEDIE LABEL@ B,
         pre LBL,  pa pu BYTES,
         post LBL,  sa su BYTES,
      miss LBL,
         5 5 DREC ADDI,  6 6 1 SUBI,  loop B,
   done LBL, ;

: C-PACKAGE-SEAL-GUARD ( -- )   \ reject `package NAME` open/reopen of a sealed system package
   LBL {: ok:label :}
   9 DATA SEAL-NDICT-CELL LDR,  9 ok CBZ,               \ namespace open -> allow
   24 DATA TKL-CELL LDR,  C-SEAL-MATCH                  \ candidate len = TKL; fail if reserved
   C-PACKAGE-PROT-GUARD
   ok LBL, ;

\ `package` inside an open package opens a part of it: in the buffer that
\ package is open in (layout.f PKG-OUTSIDE-CELL), in its private section, under
\ no using it opened (USE-DEPTH = USE-PKG-SAVE), inside no part.
: C-PACKAGE-PART-GUARD ( -- )
   LBL LBL {: bad:label done:label :}
   14 DATA PKG-OUTSIDE-CELL LDR,  14 bad CBNZ,
   14 DATA ENCLOSING:PUB LDR,  14 bad CBNZ,
   14 DATA PKG-PRI-CELL LDR,  15 DATA CUR-CELL LDR,  14 15 CMP,  C-NE bad BCOND,
   9 USE-DEPTH-CELL LIT64,  9 DATA 9 ADD,  14 9 0 LDR,
   9 USE-PKG-SAVE-CELL LIT64,  9 DATA 9 ADD,  15 9 0 LDR,
   14 15 CMP,  C-EQ done BCOND,
   bad LBL,  $4B C-PACKAGE-FAIL
   done LBL, ;

\ Inside package A the token cells name layout.f PACKAGE-PATH holding
\ `A:<token>`, and a path of PACKAGE-PATH:CAP bytes or more is refused.
: C-PACKAGE-PATH ( -- )
   LBL LBL LBL LBL LBL LBL LBL
   {: inline:label fits:label head:label tail:label store:label copied:label done:label :}
   9 DATA PKG-REC-CELL LDR,  9 done CBZ,
   2 9 16 LDR,  2 2 DNAME-FLAG-BITS LSLI,  2 2 DNAME-FLAG-BITS LSRI,   \ x2 = A's name length
   4 9 24 ADDI,
   14 9 16 LDR,  14 14 DNAME-EXT ANDI,  14 inline CBZ,
      4 9 24 LDR,                                     \ x4 = A's name bytes
   inline LBL,
   3 DATA TKL-CELL LDR,  6 DATA TKA-CELL LDR,
   13 2 3 ADD,  13 13 1 ADDI,                         \ x13 = the path's length
   13 PACKAGE-PATH:CAP CMPI,  C-CC fits BCOND,
      $4B C-PACKAGE-FAIL
   fits LBL,
   5 PACKAGE-PATH:OFF LIT64,  5 DATA 5 ADD,
   7 0 MOVZ,
   head LBL,  7 2 CMP,  C-CS tail BCOND,
      14 4 7 ADD,  14 14 0 LDRB,  15 5 7 ADD,  14 15 0 STRB,
      7 7 1 ADDI,  head B,
   tail LBL,
   14 $3A MOVZ,  15 5 2 ADD,  14 15 0 STRB,
   16 15 1 ADDI,                                      \ x16 = where the token goes
   7 0 MOVZ,
   store LBL,  7 3 CMP,  C-CS copied BCOND,
      14 6 7 ADD,  14 14 0 LDRB,  15 16 7 ADD,  14 15 0 STRB,
      7 7 1 ADDI,  store B,
   copied LBL,
   5 DATA TKA-CELL STR,  13 DATA TKL-CELL STR,
   done LBL, ;

public
: C-PACKAGE ( -- )
   C-TASK-GUARD
   LBL LBL LBL {: top:label hastok:label stored:label :}
   14 DATA PKG-PUB-CELL LDR,  14 top CBZ,
      C-PACKAGE-PART-GUARD
   top LBL,
   LTOK LABEL@ BL,  0 hastok CBNZ,
      $4A C-PACKAGE-FAIL
   hastok LBL,
   C-PACKAGE-NAME-GUARD
   C-PACKAGE-PATH
   C-PACKAGE-POLICY-GUARD                               \ before the checker hears of the package
   C-CALL-CHECKER-PACKAGE
   C-PACKAGE-SEAL-GUARD
   C-PACKAGE-ENSURE
   \ Remember the using depth at package open: `;package` restores it, and
   \ `;using` refuses to pop below it (C-END-USING); a part's is its package's.
   \ x11/x12/x5 hold the package wids/record for the stores below.
   9 DATA PKG-PUB-CELL LDR,  9 stored CBZ,             \ a part: the open package encloses it
      9 DATA ENCLOSING:PUB STR,
      9 DATA PKG-PRI-CELL LDR,  9 DATA ENCLOSING:PRI STR,
      9 DATA PKG-PARENT-CELL LDR,  9 DATA ENCLOSING:PARENT STR,
      9 DATA PKG-REC-CELL LDR,  9 DATA ENCLOSING:REC STR,
   stored LBL,
   9 USE-DEPTH-CELL LIT64,  9 DATA 9 ADD,  10 9 0 LDR,
   9 USE-PKG-SAVE-CELL LIT64,  9 DATA 9 ADD,  10 9 0 STR,
   9 DATA CUR-CELL LDR,  9 DATA PKG-PARENT-CELL STR,
   11 DATA PKG-PUB-CELL STR,  12 DATA PKG-PRI-CELL STR,
   5 DATA PKG-REC-CELL STR,
   12 DATA CUR-CELL STR,
   9 0 MOVZ,  9 DATA PKG-OUTSIDE-CELL STR, ;          \ opened in this buffer

: C-PUBLIC ( -- )
   C-TASK-GUARD
   LBL {: active:label :}
   9 DATA PKG-PUB-CELL LDR,  9 active CBNZ,
      $4B C-PACKAGE-FAIL
   active LBL,
   C-CALL-CHECKER-PUBLIC
   9 DATA PKG-PUB-CELL LDR,
   9 DATA CUR-CELL STR, ;

: C-PRIVATE ( -- )
   C-TASK-GUARD
   LBL {: active:label :}
   9 DATA PKG-PRI-CELL LDR,  9 active CBNZ,
      $4B C-PACKAGE-FAIL
   active LBL,
   C-CALL-CHECKER-PRIVATE
   9 DATA PKG-PRI-CELL LDR,
   9 DATA CUR-CELL STR, ;

: C-SEAL-PACKAGE ( -- )
   C-TASK-GUARD
   LBL {: active:label :}
   9 DATA PKG-PUB-CELL LDR,  9 active CBNZ,
      $4B C-PACKAGE-FAIL
   active LBL,
   LPROTWIDADD LABEL@ BL,
   9 DATA PKG-PRI-CELL LDR,  LPROTWIDADD LABEL@ BL, ;

\ `;package` of a part puts its enclosing package back open, its private
\ section current; of any other package, it leaves none open.
: C-END-PACKAGE ( -- )
   C-TASK-GUARD
   LBL {: active:label :}
   9 DATA PKG-PUB-CELL LDR,  9 active CBNZ,
      $4B C-PACKAGE-FAIL
   active LBL,
   C-CALL-CHECKER-END-PACKAGE
   9 DATA PKG-PARENT-CELL LDR,  9 DATA CUR-CELL STR,
   9 DATA ENCLOSING:PUB LDR,  9 DATA PKG-PUB-CELL STR,
   9 DATA ENCLOSING:PRI LDR,  9 DATA PKG-PRI-CELL STR,
   9 DATA ENCLOSING:PARENT LDR,  9 DATA PKG-PARENT-CELL STR,
   9 DATA ENCLOSING:REC LDR,  9 DATA PKG-REC-CELL STR,
   9 0 MOVZ,
   9 DATA ENCLOSING:PUB STR,  9 DATA ENCLOSING:PRI STR,
   9 DATA ENCLOSING:PARENT STR,  9 DATA ENCLOSING:REC STR,
   \ Restore the using depth to the package-open boundary (the checker reads
   \ the same shared USE-DEPTH).
   9 USE-PKG-SAVE-CELL LIT64,  9 DATA 9 ADD,  10 9 0 LDR,
   9 USE-DEPTH-CELL LIT64,  9 DATA 9 ADD,  10 9 0 STR, ;

\ `using NAME` adds NAME's public wordlist to bare lookup until the matching
\ `;using`, the enclosing `;package` or the end of the load file; inside a
\ package `;using` closes only a using the package opened. Definitions still
\ target the current wordlist, and NAME:WORD is unchanged. EMIT-FIND-USED runs
\ only after the open-scope + global chain misses, so `using` never shadows an
\ existing binding; a bare tail resolving in two used packages is a hard error.

private
: C-USING-NAME-GUARD ( -- )   \ consume the next token; reject a missing name / a ':' in it
   LBL LBL LBL LBL LBL LBL {: mmiss:label hastok:label cscan:label cbad:label cmsg:label cok:label :}
   s" hb: using: missing package name" {: ma:ptr mu:n :}
   s" hb: using: package name must not contain ':': " {: ca:ptr cu:n :}
   LTOK LABEL@ BL,  0 hastok CBNZ,
      0 2 MOVZ,  1 mmiss ADR,  2 mu MOVZ,  NR-WRITE SYS,
      0 89 MOVZ,  LCOMPILEDIE LABEL@ B,               \ 89 = ENGINE-ERROR:USING-NO-NAME
   mmiss LBL,  ma mu BYTES,
   hastok LBL,
   14 0 MOVZ,
   cscan LBL,
      15 DATA TKL-CELL LDR,  14 15 CMP,  C-GE cok BCOND,
      15 DATA TKA-CELL LDR,  15 15 14 ADD,  15 15 0 LDRB,  15 $3A CMPI,  C-EQ cbad BCOND,
      14 14 1 ADDI,  cscan B,
   cbad LBL,
      0 2 MOVZ,  1 cmsg ADR,  2 cu MOVZ,  NR-WRITE SYS,
      90 C-DIE-TOKEN                               \ 90 = ENGINE-ERROR:USING-BAD-NAME
   cmsg LBL,  ca cu BYTES,
   cok LBL, ;

: C-USING-WID ( -- )   \ TKA/TKL name a package -> x2 = its public WID; die unknown otherwise
   LBL LBL LBL {: notfound:label umsg:label done:label :}
   s" hb: using: unknown package: " {: ua:ptr uu:n :}
   9 DATA TKA-CELL LDR,  17 DATA TKL-CELL LDR,  LHEADROW LABEL@ BL,
   12 notfound CBZ,
   2 12 0 LDR,  done B,                           \ x2 = record[0] = package public WID
   notfound LBL,
      0 2 MOVZ,  1 umsg ADR,  2 uu MOVZ,  NR-WRITE SYS,
      91 C-DIE-TOKEN                               \ 91 = ENGINE-ERROR:USING-UNKNOWN
   umsg LBL,  ua uu BYTES,
   done LBL, ;

: C-USING-PUSH ( -- )   \ x2 = public WID; push it onto the using stack (overflow -> die)
   LBL LBL LBL {: fmsg:label pushok:label nofloor:label :}
   s" hb: using: too many concurrent usings: " {: fa:ptr fu:n :}
   7 USE-DEPTH-CELL LIT64,  7 DATA 7 ADD,  8 7 0 LDR,       \ x8 = live using depth (overflow test)
   14 USE-MAX MOVZ,  8 14 CMP,  C-LT pushok BCOND,
      0 2 MOVZ,  1 fmsg ADR,  2 fu MOVZ,  NR-WRITE SYS,
      92 C-DIE-TOKEN                               \ 92 = ENGINE-ERROR:USING-OVERFLOW
   fmsg LBL,  fa fu BYTES,
   pushok LBL,
      7 USE-DEPTH-CELL LIT64,  7 DATA 7 ADD,  8 7 0 LDR,   \ reload depth on the accepted path (no cross-syscall live reg)
      \ A buffer whose `;package` closed a package opened before it: its usings
      \ start at this depth, so its floor comes down to meet them (layout.f
      \ EVAL-FRAME:USE-FLOOR). The REPL has no frame and no floor.
      9 DATA EVALD-CELL LDR,  9 nofloor CBZ,
         9 DATA EVAL-TOP-CELL LDR,  10 9 EVAL-FRAME:USE-FLOOR LDR,
         10 8 CMP,  10 10 8 C-LS CSEL,  10 9 EVAL-FRAME:USE-FLOOR STR,   \ floor = the lower of floor and depth
      nofloor LBL,
      14 USE-WIDS-OFF LIT64,  14 DATA 14 ADD,  15 8 3 LSLI,  14 14 15 ADD,  2 14 0 STR,   \ USE-WIDS[depth] = wid
      C-CALL-CHECKER-USING                                 \ mirror name into CK-USE-NAMES[depth] (reads pre-increment depth)
      7 USE-DEPTH-CELL LIT64,  7 DATA 7 ADD,  8 7 0 LDR,  8 8 1 ADDI,  8 7 0 STR, ;

public
: C-USING ( -- )
   C-TASK-GUARD
   C-USING-NAME-GUARD
   C-USING-WID
   C-USING-PUSH ;

: C-END-USING ( -- )
   C-TASK-GUARD
   LBL LBL LBL LBL {: ok:label umsg:label own:label omsg:label :}
   LBL LBL {: mine:label fmsg:label :}
   s" hb: ;using would close a using opened outside the file" {: fm:ptr fu:n :}
   s" hb: ;using without an open using" {: ua:ptr uu:n :}
   s" hb: ;using would close a using opened outside the package" {: oa:ptr ou:n :}
   7 USE-DEPTH-CELL LIT64,  7 DATA 7 ADD,  8 7 0 LDR,       \ x8 = live using depth (underflow test)
   8 ok CBNZ,
      0 2 MOVZ,  1 umsg ADR,  2 uu MOVZ,  NR-WRITE SYS,
      0 93 MOVZ,  LCOMPILEDIE LABEL@ B,               \ 93 = ENGINE-ERROR:USING-UNBALANCED
   umsg LBL,  ua uu BYTES,
   ok LBL,
   \ In an open package the most recent using must be the package's own: one
   \ opened before `package` lies at or below the depth `;package` restores, so
   \ closing it here would come undone there.
   9 DATA PKG-PUB-CELL LDR,  9 own CBZ,                     \ no package open -> any open using closes
   9 USE-PKG-SAVE-CELL LIT64,  9 DATA 9 ADD,  9 9 0 LDR,    \ x9 = depth at package open
   7 USE-DEPTH-CELL LIT64,  7 DATA 7 ADD,  8 7 0 LDR,       \ reload depth (no cross-syscall live reg)
   8 9 CMP,  C-HI own BCOND,                                \ x8 (depth) above it -> opened in the package
      0 2 MOVZ,  1 omsg ADR,  2 ou MOVZ,  NR-WRITE SYS,
      0 104 MOVZ,  LCOMPILEDIE LABEL@ B,              \ 104 = ENGINE-ERROR:USING-OUTER
   omsg LBL,  oa ou BYTES,
   own LBL,
   \ Nor may a buffer close a using its includer opened (at or below the
   \ buffer's floor, layout.f EVAL-FRAME:USE-FLOOR): it would reopen when the
   \ buffer ends and a later using would take its slot.
   9 DATA EVALD-CELL LDR,  9 mine CBZ,                      \ the REPL closes any open using
   9 DATA EVAL-TOP-CELL LDR,  9 9 EVAL-FRAME:USE-FLOOR LDR,        \ x9 = the buffer's floor
   7 USE-DEPTH-CELL LIT64,  7 DATA 7 ADD,  8 7 0 LDR,       \ reload depth (no cross-syscall live reg)
   8 9 CMP,  C-HI mine BCOND,                                \ x8 (depth) above it -> opened in the buffer
      0 2 MOVZ,  1 fmsg ADR,  2 fu MOVZ,  NR-WRITE SYS,
      0 104 MOVZ,  LCOMPILEDIE LABEL@ B,              \ 104 = ENGINE-ERROR:USING-OUTER
   fmsg LBL,  fm fu BYTES,
   mine LBL,
   7 USE-DEPTH-CELL LIT64,  7 DATA 7 ADD,  8 7 0 LDR,  8 8 1 SUBI,  8 7 0 STR, ;   \ reload depth (no cross-syscall live reg)

private
\ EMIT-FIND-USED (LFINDUSED leaf): searched only after the open-scope + global
\ chain misses. In: x9=token addr, x10=token len. Out: x13=0 (miss) or FIND
\ flags with x11=code x12=body-len, and x5 = the matched record (0 on a miss),
\ because callers reach this instead of LFIND and both must answer the same
\ shape. Each live USE-WIDS[0..depth) public wordlist is asked through LFIND's
\ one-wordlist hash probe; this matches the scan because a public wordlist holds
\ at most one live row per folded name (the definer's duplicate wall, BSWL's
\ reason). One package used twice reaches one record, counted once; a second
\ distinct record is the ambiguity error. The scan below stays authoritative
\ before LHIDXBUILD and for a chain with no empty slot. Qualified tokens never
\ resolve here. Preserves x9/x10/x19(XDS)/x20(DATA).
: EMIT-FIND-USED ( -- )
   LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL
   LBL LBL LBL LBL LBL LBL LBL LBL LBL
   {: qscan:label qnone:label ret:label uloop:label mloop:label member:label
      ninl:label ncmp:label nmatch:label unext:label udone:label amb:label ambmsg:label
      lscan:label kloop:label knext:label ploop:label pnext:label pinl:label
      pcmp:label pmatch:label pamb:label :}
   s" hb: ambiguous bare word resolves in multiple used packages: " {: aa:ptr au:n :}
   \ The compile and interpret paths die on an ambiguous tail; scope-find
   \ (BSCOPEFIND) reports it. LFINDUSED-CORE answers x6 = distinct records met
   \ (0, 1, 2); LFINDUSED adds the death. At 2 nothing binds (x13 = 0, x5 = 0)
   \ and both are named: x16 first, x12 second.
   LFINDUSED LABEL@ LBL,
      SP SP 16 SUBI,  30 SP 0 STR,
      LFINDUSED-CORE LABEL@ BL,
      30 SP 0 LDR,  SP SP 16 ADDI,
      6 2 CMPI,  C-GE amb BCOND,
      RET,
   LFINDUSED-CORE LABEL@ LBL,
   13 0 MOVZ,
   17 0 MOVZ,
   6 0 MOVZ,                                                        \ the match count every exit answers
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
      11 12 16 LDR,  11 11 DNAME-FLAG-BITS LSLI,  11 11 DNAME-FLAG-BITS LSRI,  11 10 CMP,  C-NE pnext BCOND,  \ name-len mismatch
      17 12 24 ADDI,
      11 12 16 LDR,  11 11 DNAME-EXT ANDI,  11 pinl CBZ,
         17 12 24 LDR,
      pinl LBL,
      13 0 MOVZ,
      pcmp LBL,
         \ x2 and x11 carry the two bytes, x12 is the fold scratch; the record
         \ pointer is recomputed at pmatch and the wid reloaded from USE-WIDS[x3]
         \ at pnext.
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
         6 6 1 ADDI,  6 2 CMPI,  C-GE pamb BCOND,                  \ 2nd distinct record -> ambiguous: x16 1st, x12 2nd
         16 12 0 ADDI,
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
         15 5 16 LDR,  15 15 DNAME-FLAG-BITS LSLI,  15 15 DNAME-FLAG-BITS LSRI,  15 10 CMP,  C-NE unext BCOND,   \ name-len mismatch
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
            6 6 1 ADDI,  12 5 0 ADDI,                              \ count++, x12 = this record
            6 2 CMPI,  C-GE pamb BCOND,                            \ 2nd distinct match -> ambiguous: x16 1st, x12 2nd
            16 5 0 ADDI,                                           \ remember record
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
   pamb LBL,  13 0 MOVZ,  5 0 MOVZ,  RET,                          \ a second distinct record: none binds, x6 = 2
   amb LBL,
      0 2 MOVZ,  1 ambmsg ADR,  2 au MOVZ,  NR-WRITE SYS,
      94 C-DIE-TOKEN                               \ 94 = ENGINE-ERROR:USING-AMBIGUOUS
   ambmsg LBL,  aa au BYTES, ;
;package

package ENGINE-EMIT
\ LSCOPEREC: the name lookup as one leaf for every reader but the call site, in
\ the call site's order: LFIND (open package, enclosing one,
\ global, or NAME:tail), then the used publics only when LFIND missed. In:
\ x9/x10 = the token, preserved. Out: x14 = the bound record (0 when nothing
\ binds), x16 = the used publics' record (0 when none, the first of two), x17 =
\ the second of two, x15 = flags: 1 when the bound record is a seeded primitive,
\ 2 when the used publics hold two distinct records (then nothing binds and both
\ are named). Clobbers x2-x17 as the leaves do.
\ Seeded is the identity of an operator row's word: the boot copies the
\ primitives' records to dict[0..LNCOUNT), and every later record (a package
\ word, an EXPORT alias, a redefinition after `undefine`) sits above. An
\ operator row may claim its token only when the lookup bound that primitive's
\ own record (src/compiler/native/hir-word.f INTRINSIC-BOUND?);
\ the checker uses the identity NORET-AXIOMS registers (src/core/checker.f
\ CTL-CORE-OP, CTL-INTRINSIC).
: EMIT-SCOPE-REC ( -- )
   LBL LBL LBL {: two:label have:label notseed:label :}
   LSCOPEREC LABEL@ LBL,
   SP SP 32 SUBI,  30 SP 0 STR,  9 SP 8 STR,  10 SP 16 STR,
   LFIND LABEL@ BL,                                       \ x5 = the record, 0 on a miss
   5 SP 24 STR,
   9 SP 8 LDR,  10 SP 16 LDR,                             \ LFIND moved them onto a qualified tail
   LFINDUSED-CORE LABEL@ BL,                              \ x5 = the one used record, x6 = records met
   17 12 0 ADDI,
   6 2 CMPI,  C-GE two BCOND,                             \ two: x16 the first, x17 the second, x5 = 0
      16 5 0 ADDI,  17 0 MOVZ,
   two LBL,
   14 SP 24 LDR,  14 have CBNZ,
      14 5 0 ADDI,                                        \ LFIND missed: the used publics bind
   have LBL,
   6 2 CMPI,  15 C-GE CSET,  15 15 1 LSLI,               \ flag 2: two distinct used records
   14 notseed CBZ,
   12 LNCOUNT LABEL@ ADR,  12 12 0 LDR,  13 DREC MOVZ,  12 12 13 MUL,  12 DBASE 12 ADD,
   14 12 CMP,  C-CS notseed BCOND,                        \ at or past dict[LNCOUNT]: not seeded
      13 1 MOVZ,  15 15 13 ORR,
   notseed LBL,
   9 SP 8 LDR,  10 SP 16 LDR,
   30 SP 0 LDR,  SP SP 32 ADDI,
   RET, ;

: C-CALL-CHECKER-EXPORT ( -- )
   LBL {: ready:label :}
   NCOMP-DISPATCH:DECL-EXPORT-OFF DECL-OWNER:FIND
   11 ready CBNZ,
      LCHKEXPORT CHKEXPORT$ nip 70 C-EXIT-LABEL
   ready LBL,
   9 DATA TKA-CELL LDR,  9 G-PUSH
   9 DATA TKL-CELL LDR,  9 G-PUSH
   C-CALL-X11-SAVED ;

\ Rewrite TKA/TKL to the tail of a NAME:tail token (DEF-TKA/DEF-TKL hold the
\ original spelling). A leading or trailing first colon keeps the token an
\ ordinary name (FIND parity); LFIND has already rejected a double colon.
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

\ The clause an export of a does> definer carries (DOES-REC "the clause an
\ export carries") sits at [sp+24] of C-EXPORT's frame, 0 for any other source.
\ Its slot (above the export's own) and its name in the export's wordlist are
\ refused here, before the checker hears of the export and before any write,
\ with the codes the export's own slot and name get.
: C-EXPORT-CLAUSE-ROOM ( -- )
   LBL LBL LBL {: none:label room:label fresh:label :}
   6 SP 24 LDR,  6 none CBZ,
   9 2 MOVZ,  EM-DEF-OCC:LROOM LABEL@ BL,
   14 DICT-CAP 1 - LIT64,  NDICT 14 CMP,  C-LT room BCOND,
      LDICTFULL DICTFULL-MSG$ nip C-CAP-LABEL
      $4D C-QUALIFY-FAIL
   room LBL,
   6 0 1 14 DOES-REC:NAME-OF,
   2 DATA DEF-WL-CELL LDR,
   WLFIND:LENTRY LABEL@ BL,
   12 fresh CBZ,
      C-DUP-DEF-SAY
      6 SP 24 LDR,  6 DOES-REC:NAME-SAY,
      0 $4E MOVZ,  LCOMPILEDIE LABEL@ B,
   fresh LBL,
   none LBL, ;

\ Write the clause's record (its entry and length under its name, in the
\ export's wordlist) in the slot above the export's own, which C-EXPORT has
\ written and not yet counted. A long name is copied at CP as the export's is;
\ a code ceiling that refuses it leaves both uncounted.
: C-EXPORT-CLAUSE-RECORD ( -- )
   LBL LBL {: full:label none:label :}
   6 SP 24 LDR,  6 none CBZ,
   9 NDICT 1 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   6 SP 24 LDR,  6 10 12 14 DOES-REC:NAME-OF,
   full DEFWRITE:NAME-STORE
   6 SP 24 LDR,
   1 DATA REGION-ALIAS:DELTA-CELL LDR,  1 9 1 ADD,     \ x1 = record x9 through the alias
   14 6 0 LDR,  14 1 0 STR,                            \ [0] = the clause's entry
   14 6 8 LDR,  14 1 8 STR,                            \ [8] = its recorded length
   14 DATA DEF-WL-CELL LDR,  14 1 40 STR,              \ [40] = the export's wordlist
   none B,
   full LBL,
      LCODEFULL CODEFULL-MSG$ nip C-CAP-LABEL
      6 SP 24 LDR,  6 DOES-REC:NAME-SAY,
      0 $4C MOVZ,  LCOMPILEDIE LABEL@ B,
   none LBL, ;

\ A restored target checker may predate the appended origin callback; only a
\ present target field is called.
: C-CALL-EXPORT-ACCEPT ( -- )
   LBL {: done:label :}
   11 DATA NCOMP-DISPATCH:TARGET-DECL-CELL LDR,  11 done CBZ,
   12 11 8 SUBI,  12 12 0 LDR,
   12 CHECKER-OWNER-ABI:EXPORT-ACCEPT-OFF CELL + CMPI,  C-LT done BCOND,
   11 11 NCOMP-DISPATCH:DECL-EXPORT-ACCEPT-OFF LDR,  11 done CBZ,
   9 SP 40 LDR,  9 G-PUSH                             \ original operand
   9 SP 48 LDR,  9 G-PUSH
   9 SP 32 LDR,  9 G-PUSH                             \ exact alias ordinal
   C-CALL-X11-SAVED
   done LBL, ;

public
\ EXPORT keyword: two roles split by package context, mirrored by verify-source
\ RECORD-EXPORT.
\ - Inside an open package: publish an existing word under its own tail into the
\   current section wordlist with the same code pointer, body span and
\   immediate/wide bits; no forwarding body. Internal source words are refused.
\   A does> definer's export publishes its clause in the slot above
\   (C-EXPORT-CLAUSE-ROOM, C-EXPORT-CLAUSE-RECORD).
\ - Top level: the engine consumes the name as a no-op.
\ CHECKER-EXPORT sees the original spelling and throws on unsigned/prim sources.
\ Native walls without the hook: missing name ($4A), sealed source prefix
\ (C-QUALIFY-SEAL-GUARD, ENGINE-ERROR:SEAL-PACKAGE), undefined word (rc 70),
\ internal word (DNAME-INT, LINTERNAL + rc 70: the alias would not carry the
\ mark), duplicate tail (C-REJECT-DUP-DEF $4E, before the checker so the native
\ diagnosis wins), protected-WID target (C-STORE-DEF-NAME).
: C-EXPORT ( -- )
   C-TASK-GUARD
   LBL LBL LBL LBL LBL LBL LBL
   {: active:label dnamed:label named:label found:label counted:label done:label notval:label :}
   9 DATA PKG-PUB-CELL LDR,  9 active CBNZ,
      LTOK LABEL@ BL,  0 dnamed CBNZ,
         $4A C-PACKAGE-FAIL
      dnamed LBL,
      done B,
   active LBL,
   LTOK LABEL@ BL,  0 named CBNZ,
      $4A C-PACKAGE-FAIL
   named LBL,
   OVERLAY-EMIT:GUARD,
   C-QUALIFY-SEAL-GUARD
   9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFIND LABEL@ BL,
   13 found CBNZ,                                          \ EXPORT <undefined>: recoverable inside evaluate (rc 70), fail-closed exit 70 at top level. C-EXPORT's own state is clean (only LTOK + LFIND ran; no SP push, no publish), and the recovery rolls the package scope back with the dictionary (PKGSNAP), so a string that also opened the package leaves it closed.
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 70 MOVZ,  LCOMPILEDIE LABEL@ B,
   found LBL,
   14 13 16 ANDI,  14 LINTERNAL LABEL@ CBNZ,             \ DNAME-INT source: TKA/TKL still hold the operand; nothing pushed or published
   SP SP 64 SUBI,  11 SP 0 STR,  12 SP 8 STR,  13 SP 16 STR,
   DOES-REC:CLAUSE-OF  6 SP 24 STR,                     \ x5 = LFIND's record; [24] = its clause or 0
   14 0 MOVZ,  14 SP 56 STR,                           \ [56] = the kind bits the alias copies; [32] is its ordinal
   14 5 16 LDR,  16 DKIND:MASK LIT64,  14 14 16 AND,
   16 DKIND:VAL LIT64,  14 16 CMP,  C-NE notval BCOND,
      16 SP 56 STR,                                      \ a constant's alias keeps its fixed-value kind until a does> patch of the shared body clears both, a retired alias included (DOESPATCH CLEAR-KIND)
   notval LBL,
   11 DATA TKA-CELL LDR,  11 DATA DEF-TKA-CELL STR,
   12 DATA TKL-CELL LDR,  12 DATA DEF-TKL-CELL STR,
   11 SP 40 STR,  12 SP 48 STR,
   14 DATA CUR-CELL LDR,  14 DATA DEF-WL-CELL STR,
   C-EXPORT-TAIL!
   C-QUALIFY-CAP
   C-REJECT-DUP-DEF                                      \ native dup wall first: labeled diagnosis + $4E
   C-EXPORT-CLAUSE-ROOM
   11 DATA DEF-TKA-CELL LDR,  11 DATA TKA-CELL STR,      \ checker sees the ORIGINAL spelling
   12 DATA DEF-TKL-CELL LDR,  12 DATA TKL-CELL STR,
   C-CALL-CHECKER-EXPORT
   C-EXPORT-TAIL!                                        \ tail again for the publish (pure rescan)
   9 NDICT 0 ADDI,  9 SP 32 STR,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   C-STORE-DEF-NAME
   1 DATA REGION-ALIAS:DELTA-CELL LDR,  1 9 1 ADD,     \ x1 = record x9 through the alias
   14 SP 0 LDR,  14 1 0 STR,                            \ [0] = source code ptr
   14 SP 8 LDR,  14 1 8 STR,                            \ [8] = source body len
   14 SP 16 LDR,                                        \ x14 = LFIND flags
   15 9 16 LDR,
   16 14 2 ANDI,  16 16 59 LSLI,  15 15 16 ORR,         \ flag bit1 -> DNAME-IMM
   16 14 8 ANDI,  16 16 59 LSLI,  15 15 16 ORR,         \ flag bit3 -> DNAME-WIDE
   16 14 $FF00 ANDI,  16 16 44 LSLI,  15 15 16 ORR,     \ flag bits 8-15 -> DNAME-MIN-IN (same body, same certified arity)
   16 SP 56 LDR,  15 15 16 ORR,                          \ constant alias still pushes the same fixed value
   15 1 16 STR,
   C-EXPORT-CLAUSE-RECORD
   EM-DEF-OCC:LAPPEND LABEL@ BL,  LHIDXADD LABEL@ BL,
   6 SP 24 LDR,  6 counted CBZ,
      EM-DEF-OCC:LAPPEND LABEL@ BL,  LHIDXADD LABEL@ BL,          \ the clause, counted with its definer
   counted LBL,
   C-CALL-EXPORT-ACCEPT
   SP SP 64 ADDI,
   done LBL, ;
;package

package INTERP-EMIT

: EM-INTERPRET-DEFINE-KEYWORDS ( -- )
   LMAIN LABEL@ LKWPACKAGE 7 ['] C-PACKAGE CF-ENTRY
   LMAIN LABEL@ LKWPUBLIC 6 ['] C-PUBLIC CF-ENTRY
   LMAIN LABEL@ LKWPRIVATE 7 ['] C-PRIVATE CF-ENTRY
   LMAIN LABEL@ LKWSEMIPACKAGE 8 ['] C-END-PACKAGE CF-ENTRY
   LMAIN LABEL@ LKWSEALPACKAGE 12 ['] C-SEAL-PACKAGE CF-ENTRY
   LMAIN LABEL@ LKWUSING 5 ['] C-USING CF-ENTRY
   LMAIN LABEL@ LKWSEMIUSING 6 ['] C-END-USING CF-ENTRY
   LMAIN LABEL@ LKWEXPORT 6 ['] C-EXPORT CF-ENTRY
   LMAIN LABEL@ LKWTRUSTED 8 ['] C-TRUSTED CF-ENTRY
   LMAIN LABEL@ KWDATA:LKWCAST 5 ['] C-CAST CF-ENTRY
   LMAIN LABEL@ KWDATA:LKWLINEAR 7 ['] C-LINEAR CF-ENTRY
   LMAIN LABEL@ LKWDEFER 5 ['] C-DEFER CF-ENTRY
   LMAIN LABEL@ LKWCREATE 6 ['] C-CREATE   CF-ENTRY
   LMAIN LABEL@ LKWVAR    8 ['] C-VARIABLE CF-ENTRY
   LMAIN LABEL@ LKWCONST  8 ['] C-CONSTANT CF-ENTRY
   LMAIN LABEL@ LKWTICK   1 ['] C-TICK     CF-ENTRY
   LMAIN LABEL@ LKWCHAR   4 ['] C-CHAR     CF-ENTRY
   LMAIN LABEL@ LKWIMM    9 ['] C-IMMEDIATE CF-ENTRY ;

: EM-INTERPRET-STRING-KEYWORDS ( -- )
   LMAIN LABEL@ LKWSQ     2 ['] C-ISDQ     CF-ENTRY
   LMAIN LABEL@ LKWCQ     2 ['] C-ICQ      CF-ENTRY
   LMAIN LABEL@ LKWDOTQ   2 ['] C-IDOTQ    CF-ENTRY
   LMAIN LABEL@ LKWESQ    3 ['] C-EISDQ    CF-ENTRY
   LMAIN LABEL@ LKWBPACKAGE 9 ['] C-IBPACKAGE CF-ENTRY
   LMAIN LABEL@ LKWECQ    3 ['] C-EICQ     CF-ENTRY
   LMAIN LABEL@ LKWEDOTQ  3 ['] C-EIDOTQ   CF-ENTRY ;

public

: EM-INTERPRET-KEYWORDS ( -- )
   EM-INTERPRET-DEFINE-KEYWORDS
   EM-INTERPRET-STRING-KEYWORDS ;

private

public

;package

package ENGINE-EMIT

\ keyword and message bytes for the pass-2 dispatch (the shuffle-op and rs
\ keyword names reuse the jit.f/loop-keyword labels).
: EMIT-P2KW ( -- )
   LP2SLOT LABEL@ LBL, P2SLOT-MSG$ BYTES,
   LOWER-TXN-CODE:MEM LABEL@ LBL, LOWER-TXN-CODE:MEM$ BYTES,
   LOWER-TXN-CODE:FULL LABEL@ LBL, LOWER-TXN-CODE:FULL$ BYTES,
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

\ Both pass-2 registrars honour jit.f's row-comparison mode (CF-DEF-GUARD).
variable P2SK
: P2W-ENTRY ( label ptr n n n [ -- ] -- ) {: lmainlbl:label kwvar:ptr kwlen:n k:n ext :}
   CF-DEF-GUARD @ IF kwvar kwlen CF-DEF-GUARD-ROW exit THEN
   LBL P2SK !
   0 kwvar LABEL@ ADR,  1 kwlen MOVZ,  LKWCMP LABEL@ BL,
   0 P2SK LABEL@ CBZ,
   k EM-P2-QUERY-WIDTHS
   13 P2SK LABEL@ CBZ,                                \ all-scalar: normal lowering
   LVSPILL LABEL@ BL,
   ext execute
   lmainlbl B,
   P2SK LABEL@ LBL, ;

: P2F-ENTRY ( label ptr n n [ -- ] -- ) {: lmainlbl:label kwvar:ptr kwlen:n ext :}
   CF-DEF-GUARD @ IF kwvar kwlen CF-DEF-GUARD-ROW exit THEN
   LBL P2SK !
   0 kwvar LABEL@ ADR,  1 kwlen MOVZ,  LKWCMP LABEL@ BL,
   0 P2SK LABEL@ CBZ,
   1 EM-P2-QUERY-WIDTHS
   LVSPILL LABEL@ BL,
   ext execute
   lmainlbl B,
   P2SK LABEL@ LBL, ;

\ Op bodies read the operand widths from P2W cells (pos 0 = stack top).
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

public

: EM-P2WIDE-ROWS ( -- )
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
   LMAIN LABEL@ LKWSTORE2  1 1 ['] EM-P2X-STORE   P2W-ENTRY ;

;package

package LOWER-TXN
private

: PROTECT ( n -- ) {: prot:n :}
   LBL {: ok:label :}
   0 DATA TXN-BLOB-A-CELL LDR,
   1 DATA TXN-BLOB-CAP-CELL LDR,
   2 prot MOVZ,  NR-MPROTECT SYS,
   0 0 CMPI,  C-EQ ok BCOND,
   LOWER-TXN-CODE:MEM LOWER-TXN-CODE:MEM$ nip 76 C-EXIT-LABEL
   ok LBL, ;

public

: SEAL ( -- ) 1 PROTECT ;

;package

package ENGINE-EMIT

public

: EM-COMPILE-CONTROL-KEYWORDS ( -- )
   LMAIN LABEL@ LKWIF     2 ['] C-CF-ROOM ['] J-IF   ['] J-IFR    CFB-ENTRY
   LMAIN LABEL@ LKWTHEN   4 ['] J-THEN   CFN-ENTRY
   LMAIN LABEL@ LKWELSE   4 ['] J-ELSE   CFN-ENTRY
   LMAIN LABEL@ LKWBEGIN  5 ['] J-BEGIN  CFN-ENTRY
   LMAIN LABEL@ LKWUNTIL  5 ['] C-UNTIL-CHECK ['] J-UNTIL ['] J-UNTILR CFBN-ENTRY
   LMAIN LABEL@ LKWAGAIN  5 ['] J-AGAIN  CFN-ENTRY
   LMAIN LABEL@ LKWWHILE  5 ['] C-WHILE-CHECK ['] J-WHILE ['] J-WHILER CFB-ENTRY
   LMAIN LABEL@ LKWREPEAT 6 ['] J-REPEAT CFN-ENTRY
   LMAIN LABEL@ LKWCASE 4 ['] J-CASE CFN-ENTRY
   LMAIN LABEL@ LKWOF 2 ['] J-OF CF-ENTRY
   LMAIN LABEL@ LKWENDOF 5 ['] J-ENDOF CF-ENTRY
   LMAIN LABEL@ LKWENDCASE 7 ['] J-ENDCASE CF-ENTRY
   LMAIN LABEL@ LKWCONSTRUCT 9 ['] J-CONSTRUCT CFN-ENTRY
   LMAIN LABEL@ LKWMATCH 5 ['] J-MATCH CF-ENTRY ;

: EM-COMPILE-STRING-KEYWORDS ( -- )
   LMAIN LABEL@ LKWSQ     2 ['] C-SDQ    CF-ENTRY
   LMAIN LABEL@ LKWCQ     2 ['] C-CQ     CF-ENTRY
   LMAIN LABEL@ LKWDOTQ   2 ['] C-DOTQ   CF-ENTRY
   LMAIN LABEL@ LKWESQ    3 ['] C-ESDQ   CF-ENTRY
   LMAIN LABEL@ LKWECQ    3 ['] C-ECQ    CF-ENTRY
   LMAIN LABEL@ LKWEDOTQ  3 ['] C-EDOTQ  CF-ENTRY ;

: EM-COMPILE-META-KEYWORDS ( -- )
   LMAIN LABEL@ LKWBTICK  3 ['] C-BTICK  CF-ENTRY
   LMAIN LABEL@ LKWBCHAR  6 ['] C-BCHAR  CF-ENTRY
   LMAIN LABEL@ LKWBPACKAGE 9 ['] C-BPACKAGE CF-ENTRY
   LMAIN LABEL@ LKWDOES   5 ['] J-DOES     CF-ENTRY
   LMAIN LABEL@ LKWQUOT   2 ['] J-QUOT     CF-ENTRY
   LMAIN LABEL@ LKWIS 2 ['] J-IS CF-ENTRY
   LMAIN LABEL@ LKWSEMIQ  2 ['] J-SEMIQUOT CF-ENTRY ;
;package

\ The loop keyword dispatch rows belong to the loop-emit family above: they
\ resolve its handlers bare, so the package is reopened here.
package LOOP-EMIT
public

: EM-COMPILE-LOOP-KEYWORDS ( -- )
   LMAIN LABEL@ LKWDO     2 ['] J-DO     CFN-ENTRY
   LMAIN LABEL@ LKWLOOP   4 ['] J-LOOP   CFN-ENTRY
   LMAIN LABEL@ LKWI      1 ['] J-I      CFN-ENTRY
   LMAIN LABEL@ LKWTOR    2 ['] J-TOR    CF-ENTRY
   LMAIN LABEL@ LKWRFROM  2 ['] J-RFROM  CF-ENTRY
   LMAIN LABEL@ LKWRFET   2 ['] J-RFETCH CF-ENTRY
   LMAIN LABEL@ LKWEXIT   4 ['] J-EXIT    CF-ENTRY
   LMAIN LABEL@ LKWREC    7 ['] J-RECURSE CF-ENTRY
   LMAIN LABEL@ LKWQDO    3 ['] J-?DO     CFN-ENTRY
   LMAIN LABEL@ LKWPLOOP  5 ['] J-+LOOP   CFN-ENTRY
   LMAIN LABEL@ LKWJ      1 ['] J-J       CF-ENTRY
   LMAIN LABEL@ LKWLEAVE  5 ['] J-LEAVE   CFN-ENTRY
   LMAIN LABEL@ LKWUNLOOP 6 ['] J-UNLOOP  CF-ENTRY
   LMAIN LABEL@ LKWLBRACE 2 ['] C-LBRACE CFN-ENTRY ;

;package

package COMPILE-EMIT

: EM-COMPILE-KEYWORDS ( -- )
   CF-DEF-GUARD @ 0= IF LBCAP LABEL@ BL, THEN
   EM-COMPILE-CONTROL-KEYWORDS
   EM-COMPILE-STRING-KEYWORDS
   EM-COMPILE-META-KEYWORDS
   LOOP-EMIT:EM-COMPILE-LOOP-KEYWORDS ;

;package

\ The arithmetic rows hand their emitters to the jit.f VOPI-ENTRY/VOP-ENTRY/
\ VDIVI-ENTRY registrars, private to the emitter package, so this block is in it.
package ENGINE-EMIT

: EM-COMPILE-ARITH-OPS ( -- )
   LMAIN LABEL@ LKWPLUS  1 ['] VF+ ['] E+ ['] EI+ 4095 VOPI-ENTRY
   LMAIN LABEL@ LKWMINUS 1 ['] VF- ['] E- ['] EI- 4095 VOPI-ENTRY
   LMAIN LABEL@ LKWSTAR  1 ['] VF* ['] E* VOP-ENTRY
   LMAIN LABEL@ LKWDIV   1 ['] VF/ ['] EI/ VDIVI-ENTRY
   LMAIN LABEL@ LKWMOD   3 ['] VFMOD ['] EIMOD VDIVI-ENTRY
   LMAIN LABEL@ LKWAND2  3 ['] FAND ['] EAND VOP-ENTRY
   LMAIN LABEL@ LKWOR2   2 ['] FOR2 ['] EOR2 VOP-ENTRY
   LMAIN LABEL@ LKWXOR2  3 ['] FXOR2 ['] EXOR VOP-ENTRY
   LMAIN LABEL@ LKWLSH 6 ['] FLSH ['] ELSH ['] EILSH 63 VOPI-ENTRY
   LMAIN LABEL@ LKWRSH 6 ['] FRSH ['] ERSH ['] EIRSH 63 VOPI-ENTRY ;

: EM-COMPILE-SHUFFLE-OPS ( -- )
   LMAIN LABEL@ LKWDUP2  3 1 ['] XDUP  VSHUF-ENTRY
   LMAIN LABEL@ LKWDROP2 4 0 ['] XDROP VSHUF-ENTRY
   LMAIN LABEL@ LKWSWAP2 4 2 ['] XSWAP VSHUF-ENTRY
   LMAIN LABEL@ LKWOVER2 4 2 ['] XOVER VSHUF-ENTRY
   LMAIN LABEL@ LKWNIP2  3 2 ['] XNIP  VSHUF-ENTRY ;

: EM-COMPILE-COMPARE-OPS ( -- )
   LMAIN LABEL@ LKWEQ2 1 0 VCMP-ENTRY
   LMAIN LABEL@ LKWNE2 2 1 VCMP-ENTRY
   LMAIN LABEL@ LKWLT2 1 11 VCMP-ENTRY
   LMAIN LABEL@ LKWGT2 1 12 VCMP-ENTRY
   LMAIN LABEL@ LKWLE2 2 13 VCMP-ENTRY
   LMAIN LABEL@ LKWGE2 2 10 VCMP-ENTRY ;

: EM-COMPILE-UNARY-OPS ( -- )
   LMAIN LABEL@ LKWINC  2 ['] FU1+ ['] EU1+ VUN-ENTRY
   LMAIN LABEL@ LKWDEC  2 ['] FU1- ['] EU1- VUN-ENTRY
   LMAIN LABEL@ LKWZEQ  2 ['] FU0= ['] EU0= VUN-ENTRY
   LMAIN LABEL@ LKWZLT  2 ['] FU0< ['] EU0< VUN-ENTRY
   LMAIN LABEL@ LKWNEG2 6 ['] FUNEG ['] EUNEG VUN-ENTRY
   LMAIN LABEL@ LKWINV2 6 ['] FUINV ['] EUINV VUN-ENTRY ;

\ `@` shares the pass-2 row's keyword bytes (LKWAT2).
: EM-COMPILE-MEM-OPS ( -- )
   LMAIN LABEL@ LKWAT2    1 W-LDR0  VLOAD-ENTRY
   LMAIN LABEL@ LKWCFETCH 2 W-LDRB0 VLOAD-ENTRY
   LMAIN LABEL@ LKWCELLS  5 ['] FUCELLS ['] EUCELLS VUN-ENTRY
   LMAIN LABEL@ LKWCELLP  5 ['] FUCELLP ['] EUCELLP VUN-ENTRY
   LMAIN LABEL@ LKWPFIELD 9 ['] FPFIELD ['] EPFIELD VOP-ENTRY ;

: EM-COMPILE-FLOAT-OPS ( -- )
   LMAIN LABEL@ LKWFPLUS  2 $1E602800 FOP-ENTRY
   LMAIN LABEL@ LKWFMINUS 2 $1E603800 FOP-ENTRY
   LMAIN LABEL@ LKWFSTAR  2 $1E600800 FOP-ENTRY
   LMAIN LABEL@ LKWFSLASH 2 $1E601800 FOP-ENTRY ;

public

\ The one export of the row block: COMPILE-EMIT wires it into the main loop.
: EM-COMPILE-OPS ( -- )
   EM-COMPILE-ARITH-OPS
   EM-COMPILE-SHUFFLE-OPS
   EM-COMPILE-COMPARE-OPS
   EM-COMPILE-UNARY-OPS
   EM-COMPILE-MEM-OPS
   EM-COMPILE-FLOAT-OPS ;

;package

package ENGINE-EMIT

public

private
: EM-RESET-COMPILE-STATE ( -- )
   EM-ABORT-COMPILE-STATE
   9 0 MOVZ,
   9 DATA RSP-CELL STR,  9 DATA HND-CELL STR,  9 DATA LOOPSP-CELL STR, ;

public
\ An undefined word in a `:` body is a rejected definition: 70 is both the
\ catchable throw code and the uncaught exit, as driver-io's "check reject 70";
\ the uncaught-throw reporter below exits it for a refusal the checker rendered.
70 constant RC-REJECT

private
\ x10 is the absolute floor. Recovery calls this before replacing CP or DP;
\ the installed owner removes every publication in the reclaimed suffix.
\ A callback is total and leaves no value. Preserve the recovery registers it
\ would otherwise clobber, including the throw code and active frame.
: EM-CODE-INVALIDATE ( -- )
   LBL {: done:label :}
   LCODEINV LABEL@ LBL,
   10 CP CMP,  C-CS done BCOND,
   9 DATA NATIVE-OBS-CELLS:INVALIDATE LDR,  9 done CBZ,
   SP SP 48 SUBI,
   30 SP 0 STR,  11 SP 8 STR,  13 SP 16 STR,
   14 SP 24 STR,  15 SP 32 STR,  10 SP 40 STR,
   10 G-PUSH
   9 BLR,
   10 SP 40 LDR,  15 SP 32 LDR,  14 SP 24 LDR,
   13 SP 16 LDR,  11 SP 8 LDR,  30 SP 0 LDR,
   SP SP 48 ADDI,
   done LBL,  RET, ;

\ LEVALREC: a throw whose nearest handler lies beyond active evaluate boundaries
\ lands here (BTHROW via EVALREC-CELL), x15 = throw code. Each escaped frame
\ gives back its input cursor, data-stack base (XDS), package and using scope,
\ and EVALERR-CELL records the code. A frame begun with no definition open also
\ rolls back everything compiled since: CP/NDICT, DP, compile state and the
\ checker's records (EVAL-FRAME:ROWS); while a task is live it retires the
\ dictionary records made since instead and keeps CP, NDICT and DP
\ (EM-DEF-OCC:LRETIRE), rolling back the rest. A frame begun inside an open
\ definition (EVAL-FRAME:PEND) is an immediate's evaluate compiling into an
\ outer buffer's definition, which it may not close: that
\ definition stays open, put back to the token-start mark (layout.f
\ TOKBODY-CELL: body capture, locals count, CP, virtual stack; C-CF-ROOM states
\ why every refusal precedes any change). The mark is the innermost buffer's
\ current token, or its end when it exited cleanly; frames begun in an open
\ definition nest inside the one that owns the compile, so all pop while the
\ mark holds. Popping stops when EVALD reaches 0 or the nearest handler (x11,
\ read once because EM-RESET-COMPILE-STATE zeroes HND-CELL) is inside the
\ current frame; delivery then matches the non-evaluate path.
: EM-EVAL-THROW-RECOVER ( -- )
   LBL LBL LBL LBL LBL {: bounds:label owned:label scope:label unrefused:label kept:label :}
   LBL {: retired:label :}
   S\" hb: catch frame corrupt\n" {: ma:ptr mu:n :}
   LEVALREC LABEL@ LBL,
   LBL LEVLL !  LBL LEVLP !  LBL LEVLD !  LBL LEVLN !
   LBL LEVCORRUPT !  LBL LEVCORRUPTMSG !
   11 DATA 8 LDR,                                     \ x11 = nearest handler (read once)
   LEVLL LABEL@ LBL,
      12 DATA EVALD-CELL LDR,  12 LEVLD LABEL@ CBZ,   \ no eval frame left → unwind to handler
      13 DATA EVAL-TOP-CELL LDR,                     \ x13 = current frame
      14 13 24 LDR,                                   \ x14 = the frame's boundary (interpreter.f FR-BOUND); x13 stays &frame
      11 LEVLP LABEL@ CBZ,                            \ no handler → pop (escape)
      \ Handler strictly inside this frame -> unwind to it. The boundary is the
      \ evaluate's own catch handler (interpreter.f FRAME-LINK), so an equal
      \ handler lies outside the buffer and the frame pops.
      11 14 CMP,  C-CC LEVLD LABEL@ BCOND,
   LEVLP LABEL@ LBL,
      14 13 STACK-ABI:EVAL-BASE LDR,  10 13 STACK-ABI:EVAL-CAP LDR,
      12 13 32 LDR,  0 bounds STACK-GUARD:CHECK-CURSOR
      14 DATA STACK-ABI:BASE-CELL STR,
      12 13 STACK-ABI:EVAL-CAP LDR,  12 DATA STACK-ABI:CAP-CELL STR,
      12 13 0 LDR,   12 DATA INP-CELL STR,
      12 13 8 LDR,   12 DATA INE-CELL STR,
      12 13 EVAL-INB LDR,  12 DATA SRCLOC:INB-CELL STR,
      XDS 13 32 LDR,
      12 13 EVAL-FRAME:PEND LDR,  12 owned CBZ,       \ no definition open as it began: it owns the compile
         \ The outer definition as the interrupted token began (TOKBODY-CELL).
         12 DATA TOKBODY-CELL LDR,  12 DATA BODYLEN-CELL STR,
         12 DATA TOKLOCN-CELL LDR,  12 DATA LOCN-CELL STR,
         12 DATA TOKDEPTH-CELL LDR,  12 DATA CAPDEPTH-CELL STR,
         12 DATA TOKCP-CELL LDR,  CP DBASE 12 ADD,
         12 DATA TOKVSP-CELL LDR,  12 DATA VSP-CELL STR,
         12 DATA TOKVRF-CELL LDR,  12 DATA VRFREE-CELL STR,
         12 DATA TOKFRF-CELL LDR,  12 DATA FRFREE-CELL STR,
         12 0 MOVZ,  12 DATA LIDX-CELL STR,              \ the refused token's reload is gone
         scope B,
      owned LBL,
      SP SP 32 SUBI,  11 SP 0 STR,  13 SP 8 STR,  15 SP 16 STR,
      \ The checker's records go with the definitions, back to the end as the
      \ buffer began (layout.f EVAL-FRAME:ROWS: end+1, 0 with no owner then).
      13 EVAL-FRAME:ROWS kept DECL-OWNER:RETRACT-TO
      kept LBL,
      13 SP 8 LDR,  9 13 48 LDR,  EM-DEF-OCC:LRETIRE LABEL@ BL,  14 retired CBNZ,
      10 13 40 LDR,  LCODEINV LABEL@ BL,
      9 13 40 LDR,  EM-DEF-OCC:LRECLAIM LABEL@ BL,
      13 SP 8 LDR,
      CP 13 40 LDR,
      9 13 48 LDR,  EM-DEF-OCC:LCOUNT LABEL@ BL,  NDICT 13 48 LDR,
      12 13 56 LDR,  12 9 RELOC-EMIT:FLOOR-CLAMP,
      RELOC-EMIT:LROLLBACK LABEL@ BL,
      9 10 DP-LOWER,
      12 DATA DP-CELL STR,
      9 10 LASTC-TRIM,
      retired LBL,
      11 SP 0 LDR,  13 SP 8 LDR,  15 SP 16 LDR,  SP SP 32 ADDI,
      scope LBL,
      10 13 EVAL-PKG ADDI,
      9 10 PKGSNAP-CUR LDR,     9 DATA CUR-CELL STR,
      9 10 PKGSNAP-PUB LDR,     9 DATA PKG-PUB-CELL STR,
      9 10 PKGSNAP-PRI LDR,     9 DATA PKG-PRI-CELL STR,
      9 10 PKGSNAP-PARENT LDR,  9 DATA PKG-PARENT-CELL STR,
      9 10 PKGSNAP-REC LDR,     9 DATA PKG-REC-CELL STR,
      13 EVAL-FRAME:ENCLOSING 9 ENGINE-EMIT:ENCLOSING-RESTORE,
      9 10 PKGSNAP-USE LDR,     12 USE-DEPTH-CELL LIT64,  12 DATA 12 ADD,  9 12 0 STR,   \ roll the using-scope depth back too (x12 reloaded below)
      9 10 PKGSNAP:FLOOR LDR,   12 USE-PKG-SAVE-CELL LIT64,  12 DATA 12 ADD,  9 12 0 STR,   \ and the open package's using floor
      9 13 EVAL-FRAME:PKG-OUTSIDE LDR,  9 DATA PKG-OUTSIDE-CELL STR,   \ and whether the includer opened it
      13 EVAL-FRAME:USE-WIDS 12 9 ENGINE-EMIT:USE-WIDS-RESTORE,  \ and the includer's used publics, which the resync names
      9 1 MOVZ,  9 DATA PKGRESYNC-CELL STR,                  \ arm the checker resync (drained below, before delivery)
      13 9 10 ENGINE-EMIT:SX-RESTORE,                   \ an adopted observer gets the evaluating word's run back
      12 13 EVAL-PREV LDR,  12 DATA EVAL-TOP-CELL STR,
      15 DATA EVALERR-CELL STR,                       \ EVALERR = code
      12 DATA EVALD-CELL LDR,  12 12 1 SUBI,  12 DATA EVALD-CELL STR,
      12 13 EVAL-FRAME:PEND LDR,  12 LEVLL LABEL@ CBNZ, \ the outer buffer's definition keeps compiling
      EM-RESET-COMPILE-STATE                          \ clobbers x9 only; x11,x15 preserved
      LEVLL LABEL@ B,
   LEVLD LABEL@ LBL,
   \ The handler is compiled code and may ask the checker before the loop reads
   \ on, so the checker re-reads the restored package scope here. The call owns
   \ every caller-saved register, so x11 and x15 ride on the stack.
   SP SP 16 SUBI,  11 SP 0 STR,  15 SP 8 STR,
   EM-PKG-RESYNC
   11 SP 0 LDR,  15 SP 8 LDR,  SP SP 16 ADDI,
   9 15 0 ADDI,                                       \ x9 = code
   11 LEVLN LABEL@ CBZ,
   \ Handler-frame integrity (habu1.f BTHROW's contract): validate before any restore.
   14 STACK-ABI:CATCH-MAGIC LIT64,  10 11 56 LDR,  10 14 CMP,  C-NE LEVCORRUPT LABEL@ BCOND,
   10 11 40 LDR,  14 STACK-ABI:RETURN-CELLS MOVZ,
   10 14 CMP,  C-HI LEVCORRUPT LABEL@ BCOND,
   10 11 48 LDR,  14 STACK-ABI:LOOP-FRAMES MOVZ,
   10 14 CMP,  C-HI LEVCORRUPT LABEL@ BCOND,
   14 11 STACK-ABI:CATCH-BASE LDR,  10 11 STACK-ABI:CATCH-CAP LDR,  12 11 8 LDR,
   8 LEVCORRUPT LABEL@ STACK-GUARD:CHECK-CURSOR
   19 11 8 LDR,
   14 DATA STACK-ABI:BASE-CELL STR,
   10 11 STACK-ABI:CATCH-CAP LDR,  10 DATA STACK-ABI:CAP-CELL STR,
   10 11 40 LDR,  10 DATA RSP-CELL STR,               \ restore user return-stack depth
   10 11 48 LDR,  10 DATA LOOPSP-CELL STR,            \ restore loop-stack depth
   10 11 0 LDR,  10 DATA 8 STR,                       \ HND = prev
   30 11 32 LDR,  12 11 24 LDR,  13 11 16 LDR,
   SP 13 0 ADDI,  12 BR,
   LEVLN LABEL@ LBL,
   \ No handler: the uncaught-throw exit, x9 = x15 = code. LEVLN falls in; BTHROW
   \ reaches LUNCAUGHT through UNCGH-CELL (link-data.f HEADER-CELLS writes it). A
   \ code in [1,255] is a deliberate exit contract (src/lib/argv.f usage 64,
   \ check hook 70, lint findings 1) and exits as itself with no output. Any
   \ other code would be masked to `code & 0xFF` (-2816 exited 0, -2802 exited
   \ 14), so it is reported as "hb: uncaught throw code <n>\n" on fd 2 (signed
   \ itoa as G-PRINT9) and exits UNCAUGHT-RC, or RC-REJECT when it is the refusal
   \ the checker last rendered (layout.f REFUSAL-ABI:CODE-CELL), already named on
   \ fd 2. x15 keeps the code across the write (the kernel preserves x2-x15).
   \ Never returns: no RET keeps the FPRIM-L throw leaf-safe.
     LUNCAUGHT LABEL@ LBL,
   LBL LUNCRPT !  LBL LUNCPOS !  LBL LUNCLOOP !  LBL LUNCDONE !
   0 9 0 ADDI,                                        \ x0 = code for the passthrough exit
   \ One hook call serves both legs below. x0 is the trampoline's to preserve;
   \ x9 and x15 are re-derived from it because the hook is compiled code and
   \ owns every caller-saved register.
   LEXITHOOK LABEL@ BL,  9 0 0 ADDI,  15 0 0 ADDI,
   9 1 CMPI,    C-LT LUNCRPT LABEL@ BCOND,
   9 255 CMPI,  C-GT LUNCRPT LABEL@ BCOND,
   NR-EXIT-GROUP SYS,                                  \ representable deliberate code: exit(code)
   LUNCRPT LABEL@ LBL,                                 \ out-of-range: report, then exit UNCAUGHT-RC or RC-REJECT
   1 LUNCMSG LABEL@ ADR,  0 2 MOVZ,  2 UNCMSG-LEN MOVZ,  NR-WRITE SYS,   \ "hb: uncaught throw code "
   9 15 0 ADDI,                                        \ x9 = code for the itoa
   SP SP $20 SUBI,  12 SP $20 ADDI,
   13 $A MOVZ,  12 12 1 SUBI,  13 12 0 STRB,           \ trailing newline
   14 0 MOVZ,  9 0 CMPI,
   C-GE LUNCPOS LABEL@ BCOND,
   14 1 MOVZ,  9 SP 9 SUB,                             \ x9 = -x9 (abs); x14 = sign flag
   LUNCPOS LABEL@ LBL,
   10 $A MOVZ,
   LUNCLOOP LABEL@ LBL,                                \ unsigned, as G-PRINT9: -MIN-N is MIN-N, magnitude 2^63
   11 9 10 UDIV,  13 11 10 MUL,  13 9 13 SUB,
   13 13 $30 ADDI,  12 12 1 SUBI,  13 12 0 STRB,
   9 11 0 ADDI,  9 LUNCLOOP LABEL@ CBNZ,
   14 LUNCDONE LABEL@ CBZ,
   13 $2D MOVZ,  12 12 1 SUBI,  13 12 0 STRB,          \ leading '-'
   LUNCDONE LABEL@ LBL,
   0 2 MOVZ,  1 12 0 ADDI,  2 SP $20 ADDI,  2 2 12 SUB,
   NR-WRITE SYS,                                       \ write(2, digits, len)
   0 UNCAUGHT-RC MOVZ,
   9 DATA REFUSAL-ABI:CODE-CELL LDR,  9 unrefused CBZ,  \ no refusal was rendered
   9 15 CMP,  C-NE unrefused BCOND,                    \ one was, but not this code
   0 RC-REJECT MOVZ,                                   \ the rendered refusal: exit as a refusal
   unrefused LBL,
   NR-EXIT-GROUP SYS,
   LEVCORRUPT LABEL@ LBL,                              \ eval-cross forged/corrupt handler frame: fail closed before any restore
   0 2 MOVZ,  1 LEVCORRUPTMSG LABEL@ ADR,  2 mu MOVZ,  NR-WRITE SYS,
   0 ENGINE-ERROR:CATCH-STACK MOVZ,  NR-EXIT-GROUP SYS,
   LEVCORRUPTMSG LABEL@ LBL,  ma mu BYTES,
   bounds LBL,  STACK-GUARD:EXIT-BOUNDS ;

\ Interpret-level reject diagnostics. LUNDEF: undefined token; its message and
\ the token end through the LCOMPILEDIE tail, which names the file and line and
\ throws RC-REJECT (70); interpreter.f UNDEFINED writes the same bytes. LWIDE: a
\ word with DNAME-WIDE executed or ticked at interpret level would land a
\ multi-cell bundle on the untyped interpret stack, where scalar dup/drop/swap
\ corrupt it; checked definitions own bundle work. LWIDE and the rejects after
\ it print their diagnostic, the token and a newline, then share the LDIAGRET
\ tail: a catchable RC-REJECT throw inside evaluate (via LEVALREC), else exit
\ 70.
: EM-COMPILE-UNDEF ( -- )
   LUNDEF LABEL@ LBL,
   9 DATA POLICY-NDICT-CELL LDR,  9 LPOLICY LABEL@ CBNZ,    \ under the design seal a token nothing resolves is outside the vocabulary like any other: the same located line and code
   SP SP 16 SUBI,  9 $494645444E552D45 LIT64,  9 SP 0 STR,  9 $000000203A44454E LIT64,  9 SP 8 STR,  0 2 MOVZ,  1 SP 0 ADDI,  2 13 MOVZ,  NR-WRITE SYS,  SP SP 16 ADDI,  0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 RC-REJECT MOVZ,  LCOMPILEDIE LABEL@ B,                 \ no newline: the LCOMPILEDIE tail appends the location and it
   LDIAGRET LABEL@ LBL,                                     \ shared recovery tail of the interpret-level rejects below (LWIDE, LINTERNAL, ...)
   9 DATA EVALD-CELL LDR,  9 LUN0 LABEL@ CBZ,               \ EVALD==0 -> top-level path (LUN0)
      \ Inside evaluate: unwind as a catchable RC-REJECT throw via LEVALREC,
      \ which drops a partial definition the frame opened and delivers to the
      \ enclosing catch, or exits 70 with no handler.
      15 RC-REJECT MOVZ,                                    \ x15 = throw code
      10 DATA EVALREC-CELL LDR,  10 BR,                     \ -> LEVALREC (frame unwind + deliver)
   LUN0 LABEL@ LBL,
   LRREC LABEL@ LBL,                                        \ nothing branches here: kernel-object.f ENGINE-SYMBOLS names it
   0 70 MOVZ,  NR-EXIT-GROUP SYS,
   LWIDE LABEL@ LBL,                                   \ branch target: TKA/TKL still hold the token
   0 2 MOVZ,  1 LWIDEMSG LABEL@ ADR,  2 WIDEMSG-LEN MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 2 MOVZ,  1 LOPENNL LABEL@ ADR,  2 NL-LEN MOVZ,  NR-WRITE SYS,
   LDIAGRET LABEL@ B,
   LINTERNAL LABEL@ LBL,                               \ branch target: TKA/TKL still hold the token (DNAME-INT reject)
   0 2 MOVZ,  1 LINTMSG LABEL@ ADR,  2 INTMSG-LEN MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 2 MOVZ,  1 LOPENNL LABEL@ ADR,  2 NL-LEN MOVZ,  NR-WRITE SYS,
   LDIAGRET LABEL@ B,
   LTRUSTTICK LABEL@ LBL,
   0 2 MOVZ,  1 LTRUSTTICKMSG LABEL@ ADR,  2 TRUSTTICKMSG-LEN MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 2 MOVZ,  1 LOPENNL LABEL@ ADR,  2 NL-LEN MOVZ,  NR-WRITE SYS,
   LDIAGRET LABEL@ B,
   \ LFLOORREC: crash.f C-CRASH-DATA-RECOVER resumes a thread here, every other
   \ register as it faulted, after an access to the data stack's low guard page:
   \ a word took more cells than the stack held, and no cell under the base was
   \ touched. XDS may be under the base, so the stub empties the stack before
   \ anything runs on it (LEVALREC restores XDS from the frame it delivers to;
   \ the uncaught exit runs the exit hook on the empty stack). TKA/TKL still
   \ name the token. The throw goes through LEVALREC, not LDIAGRET: the fault is
   \ inside running code, maybe under a catch, which receives it as a throw.
   LFLOORREC LABEL@ LBL,
   XDS DATA STACK-ABI:BASE-CELL LDR,
   0 2 MOVZ,  1 LMINMSG LABEL@ ADR,  2 MINMSG-LEN MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 2 MOVZ,  1 LOPENNL LABEL@ ADR,  2 NL-LEN MOVZ,  NR-WRITE SYS,
   15 RC-REJECT MOVZ,  LEVALREC LABEL@ B,
   LORPHAN LABEL@ LBL,                                 \ branch target from LCFPOP: closer with an empty control-flow stack (TKA/TKL hold the closer token)
   0 2 MOVZ,  1 LORPHANMSG LABEL@ ADR,  2 ORPHANMSG-LEN MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 2 MOVZ,  1 LOPENNL LABEL@ ADR,  2 NL-LEN MOVZ,  NR-WRITE SYS,
   LDIAGRET LABEL@ B,
   LCFKIND LABEL@ LBL,                                \ branch target from C-CF-AT/C-CF-NONE: the open entry is another structure's (TKA/TKL hold the token)
   0 2 MOVZ,  1 LCFKINDMSG LABEL@ ADR,  2 CFKINDMSG-LEN MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 2 MOVZ,  1 LOPENNL LABEL@ ADR,  2 NL-LEN MOVZ,  NR-WRITE SYS,
   LDIAGRET LABEL@ B,
   LCFCAP LABEL@ LBL,                                 \ branch target from LCFPUSH: opener at the control-flow stack depth cap (TKA/TKL hold the opener token)
   0 2 MOVZ,  1 LCFCAPMSG LABEL@ ADR,  2 CFCAPMSG-LEN MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 2 MOVZ,  1 LOPENNL LABEL@ ADR,  2 NL-LEN MOVZ,  NR-WRITE SYS,
   LDIAGRET LABEL@ B,
   LLOCWIDE LABEL@ LBL,                               \ branch target from C-LBRACE-STORE-ONE: bare local name wider than LOC-NAME-CAP (TKA/TKL hold the declaration token); rejected definition, rc 70 like an undefined word
   0 2 MOVZ,  1 LLOCWIDEMSG LABEL@ ADR,  2 LOCWIDEMSG-LEN MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 2 MOVZ,  1 LOPENNL LABEL@ ADR,  2 NL-LEN MOVZ,  NR-WRITE SYS,
   LDIAGRET LABEL@ B,
   LLOCMANY LABEL@ LBL,                               \ branch target from C-LBRACE-STORE-ONE: a LOC-RECS+1th local (TKA/TKL hold the declaration token); the same rejected-definition tail
   0 2 MOVZ,  1 LLOCMANYMSG LABEL@ ADR,  2 LOCMANYMSG-LEN MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 2 MOVZ,  1 LOPENNL LABEL@ ADR,  2 NL-LEN MOVZ,  NR-WRITE SYS,
   LDIAGRET LABEL@ B, ;

\ Shared recoverable compile-error tail, LDIAGRET's parametric sibling: the code
\ is in x0, so each reject keeps its own sysexits-style code (shared across
\ sites per tools/error-code-lint-core.f). Each die site writes its diagnostic
\ to fd 2, sets x0 and branches here. Inside evaluate (EVALD>0) it unwinds as a
\ catchable throw of that code via LEVALREC (HIDX skips any rolled-back record
\ index); at top level it exit_group(x0). x15 carries the code (the kernel
\ preserves x2-x15).
\ The location is printed by LSRCAT and nowhere else: die sites write their
\ message without a newline, and LSRCAT appends the place, then the newline.
\ With a source file open the place is ` at <path>`, the innermost include
\ frame's (src/core/include.f SRCLOC:PATH-CELL/PATHLEN-CELL). With no include
\ frame open the path is 0 and no place is printed: the root source stream's own
\ text (src/habu/main.f: stdin, tty or piped, or the `--load` and `--build`
\ streams) and the boot prefix. `:<line>` follows under the rule
\ SRCLOC:FILE-BUFFER? states, tested here in machine code: <line> is 1 + the
\ newlines in
\ [SRCLOC:OWNED-CELL, INP-CELL). LSRCAT is a leaf: x0-x2 and x9-x14 are
\ scratch, and a fail-closed site
\ (C-SEAL-PACKAGE-FAIL) calls it before its
\ own exit_group.
: EM-COMPILE-DIE ( -- )
   LBL LBL LBL LBL LBL LBL {: ldie:label noloc:label located:label nlloop:label nldone:label digloop:label :}
   LBL S" hb: definition while a checker replay is open: " {: omsg:label oma:ptr omu:n :}
   LBL {: dpbad-end:label :}
   LCOMPILEDIE LABEL@ LBL,                                  \ entry: x0 = sysexits exit/throw code (die site just wrote its diagnostic, newline NOT written)
   15 0 0 ADDI,                                             \ x15 = code (LEVALREC delivers it as the throw code)
   LSRCAT LABEL@ BL,
   9 DATA EVALD-CELL LDR,  9 ldie CBZ,                      \ EVALD==0 -> no eval frame (top level); EVALD>0 -> unwind escaped eval frames
      10 DATA EVALREC-CELL LDR,  10 BR,                     \ -> LEVALREC (escaped-frame unwind + deliver x15)
   ldie LBL,
   0 15 0 ADDI,                                             \ x0 = the code again
   NR-EXIT-GROUP SYS,
   LSRCAT LABEL@ LBL,
   9 DATA SRCLOC:PATHLEN-CELL LDR,  9 noloc CBZ,            \ no open source file -> newline only
      0 2 MOVZ,  1 LATMSG LABEL@ ADR,  2 ATMSG-LEN MOVZ,  NR-WRITE SYS,
      2 DATA SRCLOC:PATHLEN-CELL LDR,  1 DATA SRCLOC:PATH-CELL LDR,  0 2 MOVZ,  NR-WRITE SYS,
      10 DATA SRCLOC:OWNED-CELL LDR,  10 noloc CBZ,         \ x10 = the file's first owned byte; none -> the path alone
      11 DATA SRCLOC:INB-CELL LDR,  11 10 CMP,  C-CC noloc BCOND,       \ the buffer starts before the file -> the path alone
      12 DATA SRCLOC:OWNEDLEN-CELL LDR,  12 10 12 ADD,       \ x12 = the file's end
      11 DATA INE-CELL LDR,  11 12 CMP,  C-HI noloc BCOND,  \ the buffer ends past the file -> the path alone
      11 DATA INP-CELL LDR,                                 \ x11 = the cursor at the die
      12 1 MOVZ,                                            \ x12 = line, 1-based
      nlloop LBL,
         10 11 CMP,  C-CS nldone BCOND,
         13 10 0 LDRB,  10 10 1 ADDI,
         13 $A CMPI,  C-NE nlloop BCOND,
         12 12 1 ADDI,  nlloop B,
      nldone LBL,
      \ ":" + decimal line + newline, built backwards in a 32-byte stack scratch
      \ (the LUNCRPT itoa's shape), so it costs one write.
      SP SP $20 SUBI,  14 SP $20 ADDI,
      13 $A MOVZ,  14 14 1 SUBI,  13 14 0 STRB,
      9 $A MOVZ,                                            \ x9 = radix
      digloop LBL,
         11 12 9 UDIV,  13 11 9 MUL,  13 12 13 SUB,
         13 13 $30 ADDI,  14 14 1 SUBI,  13 14 0 STRB,
         12 11 0 ADDI,  12 digloop CBNZ,
      13 $3A MOVZ,  14 14 1 SUBI,  13 14 0 STRB,
      2 SP $20 ADDI,  2 2 14 SUB,  1 14 0 ADDI,  0 2 MOVZ,  NR-WRITE SYS,
      SP SP $20 ADDI,
      located B,
   noloc LBL,
   0 2 MOVZ,  1 LOPENNL LABEL@ ADR,  2 NL-LEN MOVZ,  NR-WRITE SYS,
   located LBL,
   RET,
   \ Counted-string-too-long diagnostic for the four c"/c\" cap sites
   \ (C-ICQ/C-EICQ/C-CQ/C-ECQ): a named fd-2 label, then LCOMPILEDIE with x0=76
   \ (catchable inside evaluate, exit 76 at top level).
   LCSTR LABEL@ LBL,
   0 2 MOVZ,  1 LCSTRMSG LABEL@ ADR,  2 CSTRMSG-LEN MOVZ,  NR-WRITE SYS,
   0 76 MOVZ,  LCOMPILEDIE LABEL@ B,
   \ DP-heap out-of-range diagnostic for habu1.f DP-CHECK, which hands the
   \ candidate DP in x15: "hb: data space out of range: DP <dp> of <cap> bytes"
   \ on fd 2, both numbers offsets from the space's base, floor - DATA-START,
   \ then LCOMPILEDIE with x0=76 (catchable inside evaluate, nothing written;
   \ exit 76 at top level).
   \ No definition is named: no DP sink runs while a colon body compiles
   \ (compile-mode string literals go to the code region). LCOMPILEDIE's ` at <path>:<line>` names the
   \ refusing line.
   LDPBAD LABEL@ LBL,
   0 2 MOVZ,  1 LDPBADMSG LABEL@ ADR,  2 DPBADMSG-LEN MOVZ,  NR-WRITE SYS,
   9 DATA BOOT:DP-FLOOR-CELL LDR,  10 DATA-START LIT64,  9 9 10 SUB,
   9 15 9 SUB,  LDIAGU LABEL@ BL,                           \ the refused DP
   0 2 MOVZ,  1 LDPBADOF LABEL@ ADR,  2 DPBAD-OF-LEN MOVZ,  NR-WRITE SYS,
   9 DATA BOOT:DP-FLOOR-CELL LDR,  10 DATA-START LIT64,  9 9 10 SUB,
   10 DATA BOOT:DP-CEILING-CELL LDR,  9 10 9 SUB,
   LDIAGU LABEL@ BL,                                        \ the ceiling DP-CHECK enforces
   0 2 MOVZ,  1 LDPBADUNIT LABEL@ ADR,  2 DPBAD-UNIT-LEN MOVZ,  NR-WRITE SYS,
   0 76 MOVZ,  LCOMPILEDIE LABEL@ B,
   dpbad-end LBL,
   s" (LDPBAD)" LDPBAD LABEL@ LABEL>N dpbad-end LABEL>N ENGINE-PRIMS:HELPER-REGISTER
   \ A writer of a record or a wid ran while a checker overlay was open
   \ (habu1.f OVERLAY-EMIT:GUARD,), before it wrote anything. TKA/TKL hold its
   \ token: the definition's name, the package's, the EXPORT operand, or the word
   \ that ran `wordlist` or a writer primitive. The code is a literal so a
   \ pre-window host need not carry the constant (docs/bootstrap.md "Generation
   \ Chain Check").
   OVERLAY-EMIT:LDIE LABEL@ LBL,
   0 2 MOVZ,  1 omsg ADR,  2 omu MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 108 MOVZ,  LCOMPILEDIE LABEL@ B,                     \ 108 = ENGINE-ERROR:OVERLAY-OPEN
   omsg LBL,  oma omu BYTES, ;

\ LPOLICY is the one refusal of a sealed process (layout.f POLICY-NDICT-CELL):
\ `hb: not in vocabulary: <token>`, then LCOMPILEDIE with ENGINE-ERROR:POLICY.
\ TKA/TKL hold the token at every site that reaches it: LKWCMP's match exit
\ and LUNDEF's head.
: EM-POLICY ( -- )
   LPOLICY LABEL@ LBL,
   0 2 MOVZ,  1 LPOLICYMSG LABEL@ ADR,  2 POLICY-MSG-LEN MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 ENGINE-ERROR:POLICY MOVZ,  LCOMPILEDIE LABEL@ B, ;

\ LDIAGU ( x9 = value ): unsigned decimal on fd 2, no newline. A diagnostic
\ never leaves through the output device (src/lib/genio-core.f), so a refusal
\ stating a count calls this, not `u.`/G-PRINTU9 (prof.f LPROFNUM is the fd-1
\ twin). Clobbers x0-x2, x9-x13, x16 (SYS,'s errno reconciliation) and x30, plus
\ 32 bytes of sp scratch; x14, x15 and x17 survive for a caller's count.
: EMIT-DIAGU ( -- )
   LDIAGU LABEL@ LBL,
   LBL {: dl:label :}
   SP SP 32 SUBI,
   12 SP 32 ADDI,
   11 $A MOVZ,
   dl LBL,
      13 9 11 UDIV,  10 13 11 MUL,  10 9 10 SUB,
      10 10 $30 ADDI,  12 12 1 SUBI,  10 12 0 STRB,
      9 13 0 ADDI,  9 dl CBNZ,
   0 2 MOVZ,  1 12 0 ADDI,  2 SP 32 ADDI,  2 2 12 SUB,  NR-WRITE SYS,
   SP SP 32 ADDI,  RET, ;

\ The body text block could not grow: `hb: cannot grow definition body text to
\ <n> bytes`, n the capacity habu1.f BCAP-GROW tried, then LCOMPILEDIE with
\ x0=71 (catchable inside evaluate, since the grow fails before anything is
\ stored; exit 71 at top level). Entered with x16 = that capacity; it moves to
\ x15 before the first write because SYS, reconciles errno in x16 and macOS
\ passes the syscall number there; LDIAGU takes it in x9.
: EM-BODY-GROW-DIE ( -- )
   LBGROWFAIL LABEL@ LBL,
   15 16 0 ADDI,                                   \ x15 = the capacity, across the write below
   0 2 MOVZ,  1 LBGROWMSG LABEL@ ADR,  2 BGROW-MSG-LEN MOVZ,  NR-WRITE SYS,
   9 15 0 ADDI,  LDIAGU LABEL@ BL,
   0 2 MOVZ,  1 LBGROWUNIT LABEL@ ADR,  2 BGROW-UNIT-LEN MOVZ,  NR-WRITE SYS,
   0 71 MOVZ,  LCOMPILEDIE LABEL@ B, ;

\ MATCH lowering legs. CMM=3/4/5 continue the token machine J-MATCH armed at
\ `match`: 3 = want family, 4 = want variant-or-`;match`, 5 = want `of`. Each
\ LBCAPs its operand so the checker's body sees it (the keyword-phase LBCAP sees
\ only `match`/`endof`); a resolution failure dies at its token, exit 70. The
\ scrutinee is the width-expanded bundle J-MATCH's CF-ENTRY spilled; J-MATCH
\ loads the tag at [x19,#-8] once into x9 and every variant compares that x9.

public
;package

\ EMIT-CODE-SECTIONS resolves EMIT-ROUTINES bare in the reopened emitter
\ package habu1.f opens for the primitive sections.
package ENGINE-EMIT

\ The routines primitives and the Habu loop's helpers branch to, and the label
\ allocations replayed registrars still read.
: EMIT-ROUTINES ( -- )
   LBL LMAIN !  LBL LUNDEF !  LBL LCAPBIND !
   LBL LCODEINV !
   EM-CODE-INVALIDATE
   EM-COMPILE-UNDEF
   EM-COMPILE-DIE
   EM-POLICY
   EMIT-DIAGU
   EM-BODY-GROW-DIE
   EM-EVAL-THROW-RECOVER
   EMIT-SOURCE-EXEC-FINISH ;

;package

package ENGINE-EMIT
public
: EMIT-RESET-BUILDER ( -- )
   ASM-INIT  ENGINE-PRIMS:RESET
   0 DIVISION-PRIM-ROW !  0 CF-DEF-GUARD ! ;
;package

package LABELS

: CORE ( -- )
   LBL LFIND !  LBL LNUM !  LBL LDICT !
   LBL LKERNELDIGEST !
   LBL LSOURCEEXECFINISH !
   LBL LCEMIT !  LBL LCEMITBL !  LBL LTOK !  LBL LPROTSPAN !  LBL LFLUSH !  LBL LFLUSHTO !  LBL LNCOUNT !
   LBL LDIVZERO !
   LBL LPROTWIDQ !  LBL LPROTWIDADD !
   LBL LBCAP !  LBL LBCS !  LBL LESCSCAN !  LBL LESCCOPY !  LBL LNSTRINTERN !
   LBL LSNAPRBD !  LBL LHIDXADD !  LBL LHIDXBUILD !
   LBL HIDX-EMIT:LREBUILD !  LBL HIDX-EMIT:LFULL !  LBL HIDX-EMIT:LREKEY !
   LBL WLFIND:LENTRY !  LBL WLFIND:LNEWEST !  LBL LHEADROW !
   LBL EM-DEF-OCC:LCOUNT !  LBL EM-DEF-OCC:LAPPEND !  LBL EM-DEF-OCC:LRESET !
   LBL EM-DEF-OCC:LROOM !  LBL EM-DEF-OCC:LISSUE !  LBL EM-DEF-OCC:LINIT !
   LBL EM-DEF-OCC:LBEFORE !  LBL EM-DEF-OCC:LAFTER !  LBL EM-DEF-OCC:LMATCH !
   LBL EM-DEF-OCC:LRECLAIM !  LBL EM-DEF-OCC:LRETIRE !
   LBL RELOC-EMIT:LCALLS !  LBL RELOC-EMIT:LMARK !  LBL RELOC-EMIT:LPTRMARK !  LBL RELOC-EMIT:LSCRATCHMARK !
   LBL RELOC-EMIT:LROLLBACK !
   LBL RELOC-EMIT:LADDRS !  LBL RELOC-EMIT:LADDRSITE !
   LBL LQUALIFYDEF !  LBL LSTOREDEFNAME !
   LBL LCFPUSH !  LBL LCFPOP !  LBL LPAT !  LBL LKWCMP !  LBL LTOPHOOK ! ;

: CONTROL ( -- )
   LBL LKWIF !  LBL LKWTHEN !  LBL LKWELSE !  LBL LKWBEGIN !
   LBL LKWUNTIL !  LBL LKWAGAIN !  LBL LKWWHILE !  LBL LKWREPEAT !
   LBL LKWCASE !  LBL LKWOF !  LBL LKWENDOF !  LBL LKWENDCASE !
   LBL LKWCREATE !  LBL LKWVAR !  LBL LKWSQ !  LBL LKWCQ !  LBL LKWDOTQ !
   LBL LKWESQ !  LBL LKWECQ !  LBL LKWEDOTQ !
   LBL LKWTYPE !
   LBL LKWTICK !  LBL LKWBTICK !
   LBL LKWLBRACE !  LBL LKWENDLOC !  LBL LKWCONST !
   LBL LKWDO !  LBL LKWLOOP !  LBL LKWI !
   LBL LKWTOR !  LBL LKWRFROM !  LBL LKWRFET !
   LBL LKWEXIT !  LBL LKWREC !
   LBL LKWQDO !  LBL LKWPLOOP !  LBL LKWJ !  LBL LKWLEAVE !  LBL LKWUNLOOP !
   LBL LKWCHAR !  LBL LKWBCHAR !  LBL LKWBPACKAGE !
   LBL LKWIMM !  LBL LKWDOES !
   LBL LKWTRUSTED !  LBL KWDATA:LKWTRUSTDECL !  LBL KWDATA:LKWTRUSTRAW !  LBL LKWKERNEL !
   LBL KWDATA:LKWCAST !  LBL KWDATA:LCASTNONAME !  LBL KWDATA:LKEYNONAME !
   LBL KWDATA:LKWLINEAR !
   LBL LKWPACKAGE !  LBL LKWPUBLIC !  LBL LKWPRIVATE !  LBL LKWSEMIPACKAGE !  LBL LKWSEALPACKAGE !
   LBL LKWDUPDEF !
   LBL LRESTAB !  LBL NCOMP-EMIT:LUNSET !  LBL LP2CLAUSE !
   LBL LKWEXPORT !  LBL LCHKEXPORT !
   LBL LKWUSING !  LBL LKWSEMIUSING !  LBL LKWDESIGNEND !  LBL LFINDUSED !  LBL LFINDUSED-CORE !  LBL LSCOPEREC !
   LBL INTERP-EMIT:LFINDSHADOW !  LBL DEFWRITE:LREPLAYTRAP !
   LBL LKWQUOT !  LBL LKWSEMIQ !  LBL LKWDEFER !  LBL LKWIS !  LBL LKWDEFERUNSET !
   LBL DEFER-DIAG:LDEFNOTOKEN !  LBL DEFER-DIAG:LDEFNOTFOUND !
   LBL DEFER-DIAG:LDEFNOTDEFER !  LBL DEFER-DIAG:LDEFNONAME !  LBL DEFER-DIAG:LDEFHINT !
   LBL LSIGA !
   LBL LKWCONSTRUCT !  LBL LKWMATCH !  LBL LKWSEMIMATCH ! ;

: RUNTIME ( -- )
   LBL LBCHAIN !  LBL LCREATE !  LBL LDOESPATCH !  LBL LGENIOOUT !
   LBL LRREC !
   LBL LUN0 !  LBL LEVALREC !  LBL LEXITHOOK !
   LBL LCRASHH !  LBL LSIGH !  LBL LHEX !  LBL LHDR !  LBL LTRAPH !  LBL LBPH !  LBL BP-CALLER:LBPLH !  LBL LBPSH !  LBL LBPWH !  LBL LBADLOC !
   LBL LOPENNL !
   LBL LUNCAUGHT !  LBL LUNCMSG !
   LBL LWIDE !  LBL LWIDEMSG !  LBL LDIAGRET !
   LBL LINTERNAL !  LBL LINTMSG !  LBL LTRUSTTICK !  LBL LTRUSTTICKMSG !
   LBL LPOLICY !  LBL LPOLICYMSG !
   LBL LMINMSG !  LBL LFLOORREC !
   LBL LDEFKWMSG !
   LBL LORPHAN !  LBL LORPHANMSG !
   LBL LCFKIND !  LBL LCFKINDMSG !
   LBL LCFCAP !  LBL LCFCAPMSG !
   LBL LLOCWIDE !  LBL LLOCWIDEMSG !  LBL LLOCMANY !  LBL LLOCMANYMSG !
   LBL LCSTR !  LBL LCSTRMSG !
   LBL LDPBAD !  LBL LDPBADMSG !  LBL LDPBADOF !  LBL LDPBADUNIT !
   LBL OVERLAY-EMIT:LDIE !
   LBL LDIAGU !
   LBL LBGROWFAIL !  LBL LBGROWMSG !  LBL LBGROWUNIT !
   LBL LQNEST !
   LBL LSRCEND !
   LBL LCOMPILEDIE !  LBL LSRCAT !
   LBL LDICTFULL !  LBL LCODEFULL !
   LBL RELOC-EMIT:LCALLMSG !  LBL RELOC-EMIT:LXTMSG !  LBL RELOC-EMIT:LXTHEADERMSG !
   LBL RELOC-EMIT:LADDRMSG !
   LBL RELOC-EMIT:LXTBANDMSG !  LBL RELOC-EMIT:LXTKINDMSG !
   LBL LBADSTR !  LBL LSIGBAD !  LBL LSEALED !  LBL LATMSG !
   LBL LMMAPCODE !  LBL LMMAPDATA !  LBL LBLRANGE !  LBL LADDSUBIMM !  LBL LADDSUBBIG ! ;

: JIT ( -- )
   LBL PROF:LPROFH !  LBL PROF:LPROFDUMP !  LBL PROF:LPROFFIND !  LBL PROF:LPROFEDGE !
   LBL PROF:LPROFJSON !  LBL PROF:LPROFNUM !  LBL PROF:LPROFPCT !
   LBL PROF:LPROFNAME !  LBL PROF:LPROFSYNC !
   LBL PROF:LPROFROW !
   LBL PROF-ABI:LPROFTRIM !  LBL PROF-ABI:LPROFRECLAIM !
   LBL LVSPILL !  LBL LVLITPUSH !  LBL LVPUSHC !
   LBL LVFOLDPUT !
   LBL LVRALLOC !  LBL LVBIT !  LBL LVMOVK !  LBL LVFORCEK !  LBL LVBINPREP !  LBL LVBINIPREP !  LBL LVPUSHR !  LBL LVNEED !
   LBL LVDEST !
   LBL LFBINPREP !
   LBL LKWFPLUS !  LBL LKWFMINUS !  LBL LKWFSTAR !  LBL LKWFSLASH !
   LBL LVDROP !  LBL LVSWAPX !  LBL LVNIPX !  LBL LVCOPY !
   LBL LVSNAP !  LBL LVRECON !  LBL LVRECONAT !  LBL LVFPOP !  LBL LVSETF !  LBL LVRESNAP !  LBL LVFUSE ! ;

: OPS ( -- )
   LBL LKWPLUS !  LBL LKWMINUS !  LBL LKWSTAR !  LBL LKWDIV !  LBL LKWMOD !
   LBL LKWAND2 !  LBL LKWOR2 !  LBL LKWXOR2 !  LBL LKWLSH !  LBL LKWRSH !
   LBL LKWDUP2 !  LBL LKWDROP2 !  LBL LKWSWAP2 !
   LBL LKWOVER2 !  LBL LKWNIP2 !
   LBL LKWEQ2 !  LBL LKWNE2 !  LBL LKWLT2 !
   LBL LKWGT2 !  LBL LKWLE2 !  LBL LKWGE2 !
   LBL LKWINC !  LBL LKWDEC !  LBL LKWZEQ !
   LBL LKWZLT !  LBL LKWNEG2 !  LBL LKWINV2 !
   LBL LKWCFETCH !  LBL LKWCELLS !  LBL LKWCELLP !  LBL LKWPFIELD ! ;

: P2 ( -- )
   LBL LP2CWAT !
   LBL LKWTUCK3 !  LBL LKWROT3 !  LBL LKWMROT3 !
   LBL LKW2DUP3 !  LBL LKW2DROP3 !  LBL LKW2SWAP3 !  LBL LKW2OVER3 !
   LBL LKW2TOR3 !  LBL LKW2RFROM3 !  LBL LKW2RFET3 !
   LBL LKWAT2 !  LBL LKWSTORE2 !
   LBL LP2COPY !  LBL LP2DROPN !  LBL LP2ROT !  LBL LP2RS !
   LBL LP2FETCH !  LBL LP2STORE !  LBL LP2VEMIT !  LBL LP2SLOT !
   LBL LOWER-TXN-CODE:MEM !  LBL LOWER-TXN-CODE:FULL !
   LBL LOWER-TXN-CODE:DRIFT-FAIL ! ;

public

: INIT ( -- )
   CORE
   CONTROL
   RUNTIME
   JIT
   OPS
   P2 ;

;package

package ENGINE-EMIT

\ tok-imm? ( ptr u8 n -- n ): live-dictionary immediate probe for the checker.
\ Runs the interpret dispatch's LFIND on a token name and pushes flags&2, the
\ DNAME-IMM bit (habu1.f FIND-HMATCH folds it to bit 1), so DO-TOK1 can reject a
\ live immediate as a checked body step. LFIND clobbers x3-x16 and preserves
\ XDS (x19)/DATA (x20); the FPRIM frame holds x30 across the BL.
: BTOKIMM ( -- )
   10 G-POP  9 G-POP                   \ x10 = len (TOS), x9 = addr: LFIND's token registers
   LFIND LABEL@ BL,                    \ x13 = found | imm<<1 | min-in byte | int flags
   9 13 2 ANDI,
   A G-PUSH ;
;package

package ENGINE-EMIT

\ Nonzero while KERNEL emits scope-kind? for a link that writes its entries
\ (LINK-SCOPE-ENTRIES); OPEN clears it.
variable LINKED-SCOPES
\ Kind k's chain label at cell k, k from 2 to 8.
create SCOPE-SITES 9 cells allot

\ Answers kind k for the entry in A when it equals kind k's entry: a linked
\ engine's from a chain, an emitted image's at off past the dictionary when off
\ >= 0. Every test binds its next label, so the emitted bytes and labels do not
\ depend on which entries a complete runtime binds.
: SCOPE-TEST ( n n label -- )
   {: off:n k:n done:label :}
   LBL {: next:label :}
   LINKED-SCOPES @ 0<> if
      LBL dup  SCOPE-SITES k cells +  LABEL!  LBL,
      \ the 16-byte chain LINKER's ARM64:CODE-ADDRESS! writes, 0 until written
      5 0 MOVZ,  NOP,  NOP,  NOP,
      5 next CBZ,
      A 5 CMP,  C-NE next BCOND,
      A k MOVZ,  done B,
   else
      off 0 >= if
         5 DICT-SIZE off + LIT64,  5 DBASE 5 ADD,
         A 5 CMP,  C-NE next BCOND,
         A k MOVZ,  done B,
      then
   then
   next LBL, ;

\ Entry kinds are fixed by the image emitter or the link's chains in code,
\ never by source declarations or writable DATA. A restored alias has the same
\ code entry and thus the same kind.
: BSCOPE-KIND ( -- )
   A G-POP
   LBL {: done:label :}
   EM-C2-RECORDS-OFF @ 6 done SCOPE-TEST
   EM-C2-RECORD-OFF @ 7 done SCOPE-TEST
   EM-C2-FIELD-OFF @ 8 done SCOPE-TEST
   EM-C2-INIT-OFF @ 5 done SCOPE-TEST
   EM-C2-MUT-OFF @ 3 done SCOPE-TEST
   EM-C2-MUT-LOAN-OFF @ 4 done SCOPE-TEST
   EM-SCOPE-CHILD-OFF @ 2 done SCOPE-TEST
   A 0 MOVZ,
   done LBL,
   A G-PUSH ;

public

\ The next KERNEL emits scope-kind? to read each entry from a chain the link
\ writes at SCOPE-ENTRY-SITE after it places the product's code.
: LINK-SCOPE-ENTRIES ( -- ) -1 LINKED-SCOPES ! ;

\ The label of the chain holding kind k's entry, k from 2 to 8.
: SCOPE-ENTRY-SITE ( n -- label ) cells SCOPE-SITES + LABEL@ ;

;package

package ENGINE-EMIT
\ scope-find ( ptr u8 n -- ptr n ptr n ptr n n ): LSCOPEREC's four answers for a
\ token (bound record, used publics' record, second used record when two hold
\ the tail, flags), so the checker, the optimizing compiler and the Habu
\ interpreter bind a spelling to the record the JIT's call binds. A global row,
\ checked like any other; a caller reads record cells as src/habu/xref.f
\ XREF-WORDLIST does.
: BSCOPEFIND ( -- )
   10 G-POP  9 G-POP                   \ x10 = len (TOS), x9 = addr
   LSCOPEREC LABEL@ BL,
   14 G-PUSH  16 G-PUSH  17 G-PUSH  15 G-PUSH ;

\ The writer fills this slot from the kernel bytes as emitted, before a link
\ writes scope-kind?'s chains. The runtime answer is independent of the source
\ tree later present on disk.
: BKERNELPRODUCER ( -- )
   9 LKERNELDIGEST LABEL@ ADR,  9 G-PUSH
   9 32 MOVZ,  9 G-PUSH
   9 0 MOVN,  9 G-PUSH ;
;package

\ The section emitters join the emitter package habu1.f opens for EMIT-PRIMS,
\ EMIT-PROTWID and EMIT-DICT, which they call bare.
package ENGINE-EMIT

create KERNEL-CONTEXT SHA256-CTX-BYTES allot
variable KERNEL-CODE-END

: KERNEL-TARGET$ ( -- ptr u8 n )
   HB-TARGET-LINUX? if s" linux-aarch64" exit then
   HB-TARGET-MACOS? if s" macos-aarch64" exit then
   HB-TARGET-LINUX-X86-64? if s" linux-x86-64" exit then
   s" engine: unknown kernel target" 76 die ;

\ Runs after EMIT-DIGEST-SLOT binds LKERNELDIGEST, so the ADR, in
\ BKERNELPRODUCER is patched in the bytes it hashes.
: FILL-KERNEL-DIGEST ( -- )
   KERNEL-CONTEXT SHA256-BEGIN
   KERNEL-CONTEXT s" native-kernel-bytes-v1" SHA256-FEED
   KERNEL-CONTEXT KERNEL-TARGET$ SHA256-FEED
   KERNEL-CONTEXT CODE KERNEL-CODE-END @ SHA256-FEED
   KERNEL-CONTEXT CODE KERNEL-CODE-END @ + SHA256-END ;

: EMIT-PRIMITIVE-SECTIONS ( -- )
   EMIT-PRIMS
   s" does-patch" ['] DOESPATCH:PRIM FPRIM
   s" does-record" ['] DOES-REC:NATIVE-PRIM FPRIM
   s" namespace-record" ['] DEFWRITE:NAMESPACE-RECORD ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" namespace-private" ['] DEFWRITE:NAMESPACE-PRIVATE ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" alias-record" ['] DEFWRITE:ALIAS-RECORD ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" package-scope!" ['] DEFWRITE:PACKAGE-SCOPE ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" def-open" ['] DEFWRITE:DEF-OPEN ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" body-append" ['] DEFWRITE:BODY-APPEND ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" trust-sig!" ['] DEFWRITE:TRUST-SIG ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" created-sig!" ['] DEFWRITE:CREATED-SIG ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" def-close" ['] DEFWRITE:DEF-CLOSE ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" def-create" ['] DEFWRITE:DEF-CREATE ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" def-abort" ['] DEFWRITE:DEF-ABORT ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" stack-clear" ['] DEFWRITE:STACK-CLEAR ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" imm-mark" ['] C-IMMEDIATE ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" def-cast" ['] DEFWRITE:DEF-CAST ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" def-defer" ['] DEFWRITE:DEF-DEFER ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" replay-open" ['] DEFWRITE:REPLAY-OPEN ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" replay-close" ['] DEFWRITE:REPLAY-CLOSE ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" replay-widn!" ['] DEFWRITE:REPLAY-WIDN ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" replay-record" ['] DEFWRITE:REPLAY-RECORD ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" record-wid!" ['] DEFWRITE:RECORD-WID ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" replay-private" ['] DEFWRITE:REPLAY-PRIVATE ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" native-pending-opaque?" ['] B-NATIVE-PENDING-OPAQUE FPRIM
   s" snap-rebase" ['] BSNAPREBASE FPRIM
   s" DRAIN-PRETRUST" ['] BDRAINPRETRUST FPRIM
   s" tok-imm?" ['] BTOKIMM FPRIM
   s" scope-find" ['] BSCOPEFIND FPRIM
   s" scope-kind?" ['] BSCOPE-KIND FPRIM
   s" xt!" ['] RELOC-EMIT:BXTSTORE FPRIM
   s" ptr-cell-mark" ['] RELOC-EMIT:BPTRCELLMARK FPRIM
   s" addr-cells-abi" ['] RELOC-EMIT:BVERSION FPRIM
   s" scratch-cell-mark" ['] RELOC-EMIT:BSCRATCHCELLMARK FPRIM
   s" ptr-cell!" ['] RELOC-EMIT:BPTRCELLSTORE FPRIM
   s" snapshot-format" ['] RELOC-EMIT:BSNAPSHOTFORMAT FPRIM
   s" kernel-producer$" ['] BKERNELPRODUCER FPRIM
   PROF:EMIT-PROF-PRIMS
   EMIT-FP-PRIMS
   ENGINE-PRIMS:COMPLETE
   EMIT-CEMIT
   EMIT-ADDSUB-IMM
   EMIT-BCAP
   EMIT-TOK
   EMIT-PROT
   EMIT-PROTWID
   EMIT-PROTWIDADD
   EMIT-FLUSH
   EMIT-FIND
   WLFIND:EMIT
   EMIT-HEAD-ROW
   EMIT-FIND-USED
   EMIT-SCOPE-REC
   DEFWRITE:EMIT-REPLAY-TRAP
   EMIT-HIDX
   EMIT-DEF-OCC
   EMIT-DEF-OCC-OVERLAP
   EMIT-DEF-OCC-RECLAIM
   EMIT-DEF-OCC-RETIRE
   EMIT-NUM
   EMIT-EXITHOOK ;

: EMIT-DICTIONARY-SECTIONS ( -- )
   DOESPATCH:EMIT
   EMIT-CF-HELPERS
   EM-SNAPSHOT-REBASE-DICT
   RELOC-EMIT:EMIT-CALLS  RELOC-EMIT:EMIT-MARK
   RELOC-EMIT:EMIT-ROLLBACK
   RELOC-EMIT:EMIT-ADDR-SITE  RELOC-EMIT:EMIT-ADDRS
   KWDATA:EMIT
   EMIT-FOLDKW
   EMIT-SHUFKW
   EMIT-CMPKW
   EMIT-UNKW
   EMIT-MEMKW
   EMIT-FOPKW
   EMIT-P2KW ;

: EMIT-RUNTIME-SECTIONS ( -- )
   EMIT-GENIO-OUT
   EMIT-CRASH-HANDLER
   EMIT-SIGNAL-HANDLER
   EMIT-TRAPH
   EMIT-HEX
   PROF:EMIT-PROFFIND
   PROF:EMIT-PROFEDGE
   PROF:EMIT-PROF-REPORTS
   PROF:EMIT-PROF ;

: EMIT-CODE-SECTIONS ( -- )
   EMIT-ROUTINES
   EMIT-PRIMITIVE-SECTIONS
   EMIT-DICTIONARY-SECTIONS
   EMIT-RUNTIME-SECTIONS
   EMIT-DICT ;

: EMIT-DIGEST-SLOT ( -- )
   LKERNELDIGEST LABEL@ LBL,
   0 DCQ,  0 DCQ,  0 DCQ,  0 DCQ, ;

public

\ Open a kernel emission at code offset 0 with the labels LABELS:INIT allocates.
: OPEN ( -- )
   \ An emission chooses no scoped entry; a link binds entries through
   \ LINK-SCOPE-ENTRIES.
   0 LINKED-SCOPES !
   -1 EM-SCOPE-CHILD-OFF !
   -1 EM-C2-MUT-OFF !
   -1 EM-C2-MUT-LOAN-OFF !
   -1 EM-C2-INIT-OFF !
   -1 EM-C2-RECORDS-OFF !
   -1 EM-C2-RECORD-OFF !
   -1 EM-C2-FIELD-OFF !
   EMIT-RESET-BUILDER
   LABELS:INIT ;

\ Emit the image in the order src/arch/arm64/icode.f's window header gives.
: KERNEL ( -- )
   EMIT-CODE-SECTIONS
   ASM-LEN KERNEL-CODE-END !
   EMIT-DIGEST-SLOT
   FILL-KERNEL-DIGEST ;

;package

;using
;using
;using
;using
;using
;using
;using
;using
;using

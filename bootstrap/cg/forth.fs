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

20 constant RBASE
26 constant DBASE  27 constant NDICT  28 constant CP

$400000 constant REGION       \ mmap region size (4 MB)
$300000000 constant RBASE-VA \ FIXED region VA: baked addresses survive re-runs (AOT)
$340000000 constant DATA-VA  \ FIXED data VA
$48425350414E5321 constant SNAP-MAGIC \ AOT snapshot trailer marker
$C1000  constant DICT-SIZE     \ dict + control-flow stack; code area follows
                               \ (= CFSTK-OFF + $1000; grown with DICT-CAP 16384,
                               \ mirrors src/habu/layout.f)
48      constant DREC          \ dict record: addr(8) clen(8) name-len|flags(8) name|ptr(16) wid(8)
16      constant DNAME-INL
$000FFFFFFFFFFFFF constant DNAME-LEN-MASK
\ DNAME-MIN-IN (bits 52-59): certified minimum input arity band, poked by the
\ native checker/seal pass (src/habu/layout.f, dot
\ habu-habu-certified-words-84e84eaf). Stage0 never sets the band (no checker),
\ so narrowing the length mask and the 12-bit LSLI/LSRI length reads below is
\ behavior-identical here and keeps the mirror's record layout numerically
\ equal to the native one on the recovery path.
$0FF0000000000000 constant DNAME-MIN-IN-MASK
$1000000000000000 constant DNAME-IMM
$2000000000000000 constant DNAME-EXT
\ DNAME-WIDE (bit 62): recorded effect carries a wider-than-cell layout value;
\ interpret dispatch/tick fail closed on it (src/habu/layout.f). Stage0 never
\ sets the bit (no checker), so the mirrored gate is inert parity.
$4000000000000000 constant DNAME-WIDE
16384   constant DICT-CAP      \ CFSTK-OFF / DREC; slots 0..16383 end exactly at CFSTK.
$C0000  constant CFSTK-OFF     \ control-flow stack: cell[0]=CFSP, then CF-REC frames
24      constant CF-REC
8       constant CF-LOCN
16      constant CF-LOCF
$2000000 constant DATA-SIZE    \ data-space mmap (always RW, separate from the RX code region)
$200000 constant IBUFSZ        \ stdin read buffer (2 MB; native mirror src/habu/layout.f)

require exec.fs
require templ.fs           \ g-push, XDS(=19)
require rt.fs              \ G-PRINT9 (shared signed-decimal printer)
require crash.fs           \ in-binary crash handler (register dump on signal)

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
\ Friend arena (TFAM 2b-i): contiguous write-protected band mirroring
\ src/habu/layout.f. Latch cell == arena base (0 open / FRIEND-ARENA-LEN sealed).
\ Package cells and protected-WID state mirror native; deferred-word state is
\ still absent. CUR/WIDN/HOOK/TSIG/TCSIG/CRSIG retain native offsets.
$20 constant FRIEND-ARENA               \ arena base offset within the DATA region
$90 constant FRIEND-ARENA-LEN           \ 18 cells: latch + 16 crown jewels + seal-ndict watermark
FRIEND-ARENA constant FRIEND-LATCH-CELL \ 0 = friend on/open, FRIEND-ARENA-LEN = sealed
$A8 constant SEAL-NDICT-CELL            \ seal-time ndict watermark (TFAM 2b-iii); inside the band so a post-seal store traps
83 constant E-SEAL-VIOLATION            \ process exit status for a post-seal protected write
84 constant E-SEAL-PACKAGE
85 constant E-BAD-TAG                    \ MATCH invalid-tag runtime exit (TFAM 10 slice 3; mirrors layout.f)
$D2800010 constant C-CALL-MOVZ-X16
$F2A00010 constant C-CALL-MOVK-X16-16
$F2C00010 constant C-CALL-MOVK-X16-32
$D63F0200 constant C-CALL-BLR-X16
67 constant UNCAUGHT-RC                 \ deterministic exit status for an uncaught top-level throw (see
                                        \ src/habu/layout.f); stage0 has no driver-io reporter, so it only
                                        \ maps the raw code to this fixed nonzero rc - never the masked value
\ Second guarded band (TFAM 2b-v): protected package WID registry plus uncaught
\ hook. Stage0 now owns the same registry shape as native so package reopen is
\ guarded by WID identity rather than a second hard-coded package-name list.
$3CB8 constant PROT-WID-N-CELL
$3CC0 constant PROT-WID-OFF
256 constant PROT-WID-MAX
PROT-WID-N-CELL constant PROT-REG-OFF
$410 constant PROT-REG-LEN
$28 constant CUR-CELL    \ get/set-current wordlist id (new defs go here)
$30 constant WIDN-CELL   \ next fresh wordlist id (WORDLIST hands these out)
$38 constant HOOK-CELL   \ check hook: a word addr run on each : body (0 = none)
$40 constant DEF-WL-CELL \ target WID selected for the definition in progress
$78 constant PKG-PUB-CELL
$80 constant PKG-PRI-CELL
$88 constant PKG-PARENT-CELL
$90 constant PKG-REC-CELL
$1B8 constant BODYLEN-CELL \ length of the captured body of the def in progress
$1C0 constant RBASE-CELL  \ saved __TEXT load base (RBASE) for the self-rebuild
$1C8 constant LOOPSP-CELL \ DO/LOOP frame stack depth
$1D0 constant S0-CELL     \ saved data-stack base (initial XDS) for the `.s` inspector
$1D8 constant SSCR-CELL   \ `.s` loop-pointer scratch (survives g-print9's x9..x15 clobber)
$200 constant VSP-CELL          \ VS depth
$210 constant VTAG-OFF          \ 32 tag bytes   (1=con, 0=reg)
$250 constant VVAL-OFF          \ 32 value cells (constant or register number)
32   constant VSMAX
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
$36D0 constant BPTAB-OFF   \ 16 breakpoints: (addr, saved-instr) 16 B each, addr 0 = empty
$43C0 constant EVAL-FRAME \ re-entrant evaluate save frames, 8 cells each:
                          \ +0 INP +8 INE +16 RET +24 SP +32 XDS +40 CP +48 NDICT +56 DP
$40 constant EVAL-FRAME-SIZE
$6 constant EVAL-FRAME-SHIFT
$10 constant EVAL-MAX-DEPTH
$48 constant TSIG-A-CELL  \ TRUSTED: pending word effect source pointer (friend arena)
$50 constant TSIG-U-CELL
$58 constant TCSIG-A-CELL \ TRUSTED: pending created-word effect pointer
$60 constant TCSIG-U-CELL
$68 constant CRSIG-A-CELL \ runtime created-word effect pending for CREATE
$70 constant CRSIG-U-CELL
$27B0 constant DOESB-CELL   \ BODYBUF offset of the DOES> body in current def
$27B8 constant TRUSTED-CELL \ open definition came from TRUSTED:
$27A8 constant CMM-CELL     \ compile-loop ADT-lowering mode (TFAM 10; mirrors src/habu/layout.f)
$5000 constant TXN-STATE-OFF
$3000 constant TXN-STATE-LEN
$10000 constant PROT-PAGE-MAX \ maximum supported arm64 page granule (DGX/Jetson Linux: 64 KiB)
TXN-STATE-OFF       constant TXN-ACTIVE-CELL
TXN-STATE-OFF $8  + constant TXN-SRC-A-CELL
TXN-STATE-OFF $10 + constant TXN-SRC-U-CELL
TXN-STATE-OFF $18 + constant TXN-CERT-A-CELL
TXN-STATE-OFF $20 + constant TXN-CERT-U-CELL
TXN-STATE-OFF $28 + constant TXN-BIND-I-CELL
TXN-STATE-OFF $30 + constant P2-CELL
TXN-STATE-OFF $38 + constant TXN-WF-I-CELL
TXN-STATE-OFF $40 + constant P2BODY0-CELL
TXN-STATE-OFF $48 + constant P2INP-CELL
TXN-STATE-OFF $50 + constant P2INE-CELL
TXN-STATE-OFF $58 + constant P2DP-CELL
TXN-STATE-OFF $60 + constant P2W0-CELL
TXN-STATE-OFF $68 + constant P2W1-CELL
TXN-STATE-OFF $70 + constant P2W2-CELL
TXN-STATE-OFF $78 + constant P2W3-CELL
TXN-STATE-OFF $80 + constant P2LOC0-CELL
TXN-STATE-OFF $88 + constant TXN-FETCH-I-CELL
TXN-STATE-OFF $90 + constant TXN-BLOB-A-CELL
TXN-STATE-OFF $98 + constant TXN-BLOB-CAP-CELL
TXN-STATE-OFF $100 + constant TXN-LIVE-W-OFF
64 constant TXN-LIVE-W-CAP

\ Private build-time mirror of the package LOWER-CERT ABI. The generated
\ stage0 runtime reads the package-owned artifact; these constants only encode
\ its wire format in the Gforth recovery compiler.
vocabulary LOWER-CERT
also LOWER-CERT definitions

$4841425543455254 constant MAGIC
2 constant VERSION
10 constant HEADER-CELLS
0 constant MAGIC-CELL
1 constant VERSION-CELL
2 constant TOTAL-BYTES-CELL
3 constant NEEDS-CELL
4 constant WF-COUNT-CELL
5 constant BIND-COUNT-CELL
6 constant FETCH-COUNT-CELL
7 constant FETCH-DATA-CELLS-CELL
8 constant BODY-LEN-CELL
9 constant BODY-HASH-CELL
4 constant WF-CELLS
3 constant FETCH-CELLS
3 constant CHECK-CELLS
3 constant GUARD-CELLS
1 constant FETCH-FLAG
$CBF29CE484222325 constant FNV-OFFSET
$100000001B3 constant FNV-PRIME

previous definitions

vocabulary LOWER-TXN
vocabulary LOWER-TXN-LOOKUP
$1B0 constant CMFAM-CELL    \ resolved construct family id between the operand tokens (mirrors layout.f)
$B0 constant CMBK-CELL      \ MATCH ENDOF branch-kind bitstack (TFAM 10 slice 3; mirrors layout.f)
$B8 constant CMTAG-CELL     \ pending MATCH variant tag (VAR -> OF)
$C0 constant CMPADS-CELL    \ pending MATCH variant zero pads M-p (VAR -> OF)
$C8 constant CMFRD-CELL     \ MATCH nesting depth (0 = not in a match)
$D0 constant CMFR-OFF       \ MATCH fam stack base (one cell per open match)
26 constant CMFR-MAX        \ levels: $D0..$1A0 = 26 cells
$37D0 constant EVALD-CELL  \ evaluate nesting depth (0 = top-level REPL/batch; gates the nested paths)
$37D8 constant EVALERR-CELL \ result of the last evaluate: 0 = clean, 1 = recovered from an error
$37E0 constant LMAINP-CELL  \ runtime addr of the interpret loop top (EM-STARTUP stores it; B-EVAL branches there)
$36B8 constant FRCLM-CELL       \ recon scratch: float claims found in a snapshot
$37F8 constant SNAP-CELL    \ nonzero after snapshot restore; source setup skips cold prefix reload
$600 constant LOOP-STK-OFF \ DO/LOOP frames (index,limit) — 32 nested, 16 B each
                           \ (baked into the j-do/j-loop/j-i precomputed words — don't move)
$800 constant BODYBUF-OFF \ captured body text (space-joined tokens), 8 KB
8000 constant BODYBUF-CAP \ fatal above this (truncation would let the checker certify unseen code)
$568 constant RSP-CELL    \ user return-stack depth (>r r> r@)
$570 constant EXITH-CELL  \ EXIT placeholder chain head (code offset; 0 = none)
$578 constant LVD-CELL    \ compile-time DO nesting depth (LEAVE chains)
$580 constant LVH-OFF     \ LEAVE chain head per nesting level — 16 levels
$2C0 constant LVF-OFF     \ loop-entry local-frame bytes per nesting level
$560 constant LASTC-CELL  \ last CREATEd slot addr (DOES> patches it)
$1F0 constant DOESP-CELL  \ runtime address of LDOESPATCH (stored at startup)
$230 constant CREATEP-CELL \ runtime address of LCREATE (prims must not name labels)
$238 constant QPATCH-CELL \ [: b-over patch site (0 = not inside a quotation)
$240 constant QENT-CELL   \ [: nested entry address (the xt ;] pushes)
$248 constant QXH-CELL    \ saved EXIT chain head across the quotation
$250 constant DEF-TKA-CELL \ original qualified definition spelling
$258 constant DEF-TKL-CELL
$2800 constant RSTK-OFF   \ user return stack — 256 cells, below DATA-START
TXN-STATE-OFF TXN-STATE-LEN + constant DATA-START \ user DP begins after protected lowering state; frozen artifacts live in mmap storage
create SQ-KW  115 c, 34 c,      \ build-time bytes for the keyword  s"  (s=115, "=34)
create CQ-KW  99 c, 34 c,
create DOTQ-KW 46 c, 34 c,
create ESQ-KW  115 c, 92 c, 34 c,
create ECQ-KW  99 c, 92 c, 34 c,
create EDOTQ-KW 46 c, 92 c, 34 c,
create BCHAR-KW 91 c, 99 c, 104 c, 97 c, 114 c, 93 c,   \ [char]
create QUOT-KW 91 c, 58 c,      \ [:
create SEMIQ-KW 59 c, 93 c,     \ ;]
create QNL-KW 63 c, 10 c,       \ ?\n  (REPL reject)
create CAPNL-KW 10 c,           \ \n  (capacity-exit diagnostic terminator)
create OKS-KW 32 c, 111 c, 107 c, 10 c,   \ \x20ok\n (REPL accept)
create BADTAG-SFX-KW 32 c, 116 c, 97 c, 103 c, 10 c,   \ " tag\n" for the MATCH bad-tag die
create TICK-KW   39 c,          \ '  (0x27)
create BTICK-KW  91 c, 39 c, 93 c,   \ ['] = [ ' ]  (0x5b 0x27 0x5d)
create LBRACE-KW 123 c, 58 c,   \ {:  (0x7b 0x3a)
create ENDLOC-KW 58 c, 125 c,   \ :}  (0x3a 0x7d)
variable STDIN?   STDIN? off   \ source mode: baked LSRC (off) vs read from stdin (on)
variable BUILD-SOURCE?   BUILD-SOURCE? off

\ runtime instruction-word constants the JIT compiler stamps out (verified encodings)
$D65F03C0 constant W-RET
$F9000269 constant W-PUSH0     \ str  x9,[x19,#0]
$91002273 constant W-PUSH1     \ add  x19,x19,#8
$D2800009 constant W-MOVZ0     \ movz x9,#0
$F2A00009 constant W-MOVK1     \ movk x9,#0,lsl#16
$F2C00009 constant W-MOVK2     \ movk x9,#0,lsl#32
$F2E00009 constant W-MOVK3     \ movk x9,#0,lsl#48

\ --- primitive registry (host-side, to build the seed dictionary) ---
128 constant PRIM-CAP
2048 constant PRIM-NAME-CAP
create PLBL PRIM-CAP cells allot   create PEL PRIM-CAP cells allot
create PLEN PRIM-CAP cells allot   create PNAM PRIM-CAP cells allot
create PNPOOL PRIM-NAME-CAP chars allot   variable PNP   variable #PL

: REG-ROOM? ( u -- )
   #PL @ PRIM-CAP >= if 1 abort" cg: primitive table overflow" then
   PNP @ + PRIM-NAME-CAP > if 1 abort" cg: primitive name pool overflow" then ;

: REG-PRIM ( ptr u8 n n -- ) {: na nu lbl elbl -- :}
   nu REG-ROOM?
   lbl  #PL @ cells PLBL + !
   elbl #PL @ cells PEL  + !
   nu   #PL @ cells PLEN + !
   PNPOOL PNP @ +  {: dst :}   dst #PL @ cells PNAM + !
   na dst nu move   nu PNP +!   1 #PL +! ;

: FPRIM ( ptr u8 xt -- ) {: na nu xt -- :}            \ define+register a primitive (start..RET..end labels)
   LBL LBL {: lbl elbl :}                \ both allocated BEFORE the locals bind:
   na nu lbl elbl REG-PRIM               \ a local named lbl shadows the LBL word
   lbl LBL,  SP SP 16 SUBI,  30 SP 0 STR,    \ prologue: save x30 (calls now nest, not inline)
   xt execute  30 SP 0 LDR,  SP SP 16 ADDI,  RET,  elbl LBL, ;

: FPRIM-L ( ptr u8 xt -- ) {: na nu xt -- :}          \ LEAF primitive: no BL/BLR in the body, so no
   LBL LBL {: lbl elbl :}          \ x30 frame — 2x cheaper calls, fully inlineable
   na nu lbl elbl REG-PRIM
   lbl LBL,  xt execute  RET,  elbl LBL, ;

\ shared label ids (forward refs)
variable LANCHOR  variable LFIND  variable LNUM  variable LDICT  variable LSRC  variable SRCN  variable SRCA
variable LCEMIT   variable LTOK   variable LPROT  variable LPROTSPAN  variable LPROTWIDQ  variable LFLUSH variable LNCOUNT
\ control-flow JIT helpers + keyword data labels (self-host 1b)
variable LCFPUSH  variable LCFPOP  variable LPAT   variable LKWCMP  variable LBCAP  variable LBCS
\ escape decoder/scan/copy routines, emitted once, BL-called (mirrors src/habu/habu2.f)
variable LESCDEC  variable LESCHEX  variable LESCSCAN  variable LESCCOPY
variable LBCHAIN  variable LCREATE  variable LDOESPATCH
variable LKWIF    variable LKWTHEN variable LKWELSE variable LKWBEGIN
variable LKWUNTIL variable LKWAGAIN variable LKWWHILE variable LKWREPEAT
variable LKWCASE  variable LKWOF    variable LKWENDOF variable LKWENDCASE
variable LKWCONSTRUCT  variable LKWMATCH  variable LKWSEMIMATCH   \ ADT lowering keywords (TFAM 10; MATCH rows are data-only until slice 3)
variable LTFLCONFAM  variable LTFLCVAR   \ TFL lowering-surface bridge names (mirrors habu2.f)
variable LTFLMATCHFAM  variable LTFLNAME \ MATCH bridge names: tfl-match-fam? / tfam-name$
variable LBADTAGPFX    variable LBADTAGSFX  \ bad-tag die message spans (C-DIE-BAD-TAG)
variable LKWCREATE variable LKWVAR variable LKWSQ variable LKWCQ variable LKWDOTQ
variable LKWESQ variable LKWECQ variable LKWEDOTQ
variable LKWTICK variable LKWBTICK
variable LKWTYPE
variable LREAD  variable LRBYE  variable LRDIE  variable LRREC  variable LQNL  variable LOKS
variable LEX0  variable LUN0   \ re-entrant evaluate: original-path continuations of LEXIT / LUNDEF
variable LKWLBRACE variable LKWENDLOC variable LLOC-FIND variable LKWCONST
variable LKWDO variable LKWLOOP variable LKWI
variable LKWTOR variable LKWRFROM variable LKWRFET
variable LKWEXIT variable LKWREC
variable LKWQDO variable LKWPLOOP variable LKWJ variable LKWLEAVE variable LKWUNLOOP
variable LKWCHAR variable LKWBCHAR
variable LKWIMM variable LKWPOST variable LKWCOMPC
variable LKWDOES variable LKWQUOT variable LKWSEMIQ
variable LKWTRUSTED variable LKWTRUST variable LKWCHKDOES variable LKWKERNEL
variable LKWPACKAGE variable LKWPUBLIC variable LKWPRIVATE variable LKWSEMIPACKAGE
variable LCHKPACKAGE variable LCHKPUB variable LCHKPRI variable LCHKENDPKG
variable LRESCHECKCERT variable LRESLOWERCERT variable LRESLOWERHOOK
variable LKWAT2 variable LKWSTORE2 variable LP2FETCH variable LP2STORE
variable LKWTUCK3 variable LKWROT3 variable LKWMROT3
variable LKW2DUP3 variable LKW2DROP3 variable LKW2SWAP3 variable LKW2OVER3
variable LKW2TOR3 variable LKW2RFROM3 variable LKW2RFET3
variable LP2COPY variable LP2DROPN variable LP2REV variable LP2ROT variable LP2RS
variable LP2VEXEC variable LP2VEMIT variable LVPBADMSG
variable LRECWPUB variable LP2DOESW
variable LCERTBYTES variable LP2CWAT variable LP2CDESC variable LP2NEST

\ Gforth recovery-host mirror of Habu's LOWER-TXN-CODE package. Keep these
\ code labels out of the global wordlist; callers select the vocabulary.
vocabulary LOWER-TXN-CODE
also LOWER-TXN-CODE definitions
variable BAD-LABEL variable MEM-LABEL variable FULL-LABEL variable DRIFT-LABEL
variable VDESC-LABEL variable DRIFT-FAIL-LABEL

: DIE ( n n -- ) {: msg len :} \ typed-local-lint: allow-bare-local
   0 2 MOVZ,  1 msg @ ADR,  2 len MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 LQNL @ ADR,  1 1 1 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
   0 76 MOVZ,  NR-EXIT-GROUP SYS, ;

previous definitions

9 constant A   10 constant B   11 constant C
12 constant DREG  13 constant EREG
require prof.fs           \ in-binary sampling profiler (emitters + prims)
require jit.fs          \ runtime abstract value stack for the : compiler

\ Span-aware stage0 mirror of src/habu/habu1.f. x13 holds the checked end and
\ x12 is scratch; address+length wrap and intersection with every compiler-owned
\ interval fail closed after the friend latch seals.
: GUARD-BAND ( n n n n -- )
   {: addr off len trap :} \ typed-local-lint: allow-bare-local
   LBL {: skip :} \ typed-local-lint: allow-bare-local
   DREG off len + LIT64,  DREG DATA DREG ADD,
   addr DREG CMP,  C-CS skip BCOND,
   DREG off LIT64,  DREG DATA DREG ADD,
   EREG DREG CMP,  C-HI trap BCOND,
   skip LBL, ;

: GUARD-ADDR-BAND ( n n n n -- )
   {: addr off len trap :} \ typed-local-lint: allow-bare-local
   DREG addr DATA SUB,
   EREG off LIT64,  EREG DREG EREG SUB,
   DREG len LIT64,
   EREG DREG CMP,  C-CC trap BCOND, ;

\ Gforth recovery-host mirror of Habu's GUARD package.
vocabulary GUARD
also GUARD definitions

: BLOB-SPAN ( n n -- )
   {: addr trap :} \ typed-local-lint: allow-bare-local
   LBL {: skip :} \ typed-local-lint: allow-bare-local
   DREG DATA TXN-BLOB-A-CELL LDR,
   DREG skip CBZ,
   EREG DREG CMP,  C-LS skip BCOND,
   EREG DATA TXN-BLOB-CAP-CELL LDR,
   DREG DREG EREG ADD,
   addr DREG CMP,  C-CC trap BCOND,
   skip LBL, ;

: BLOB-ADDR ( n n -- )
   {: addr trap :} \ typed-local-lint: allow-bare-local
   LBL {: skip :} \ typed-local-lint: allow-bare-local
   DREG DATA TXN-BLOB-A-CELL LDR,
   DREG skip CBZ,
   EREG addr DREG SUB,
   DREG DATA TXN-BLOB-CAP-CELL LDR,
   EREG DREG CMP,  C-CC trap BCOND,
   skip LBL, ;

previous definitions

: GUARD-SPAN ( n n -- )
   {: addr len :} \ typed-local-lint: allow-bare-local
   LBL LBL {: ok trap :} \ typed-local-lint: allow-bare-local
   DREG DATA FRIEND-LATCH-CELL LDR,
   DREG ok CBZ,
   len ok CBZ,
   EREG addr len ADD,
   EREG addr CMP,  C-CC trap BCOND,
   addr FRIEND-ARENA FRIEND-ARENA-LEN trap GUARD-BAND
   addr PROT-REG-OFF PROT-REG-LEN trap GUARD-BAND
   addr BODYBUF-OFF BODYBUF-CAP 2 + trap GUARD-BAND
   addr TXN-STATE-OFF TXN-STATE-LEN trap GUARD-BAND
   addr trap [ also GUARD ] BLOB-SPAN [ previous ]
   ok B,
   trap LBL,  0 E-SEAL-VIOLATION MOVZ,  NR-EXIT-GROUP SYS,
   ok LBL, ;

: PROT-GUARD ( n -- )
   {: addr :} \ typed-local-lint: allow-bare-local
   LBL LBL {: ok trap :} \ typed-local-lint: allow-bare-local
   DREG DATA FRIEND-LATCH-CELL LDR,
   DREG ok CBZ,
   addr FRIEND-ARENA FRIEND-ARENA-LEN trap GUARD-ADDR-BAND
   addr PROT-REG-OFF PROT-REG-LEN trap GUARD-ADDR-BAND
   addr BODYBUF-OFF BODYBUF-CAP 2 + trap GUARD-ADDR-BAND
   addr TXN-STATE-OFF TXN-STATE-LEN trap GUARD-ADDR-BAND
   addr trap [ also GUARD ] BLOB-ADDR [ previous ]
   ok B,
   trap LBL,  0 E-SEAL-VIOLATION MOVZ,  NR-EXIT-GROUP SYS,
   ok LBL, ;

: GUARD-CODE-WORD ( n -- )
   {: addr :} \ typed-local-lint: allow-bare-local
   LBL LBL {: ok trap :} \ typed-local-lint: allow-bare-local
   DREG DICT-SIZE LIT64,  DREG DBASE DREG ADD,
   addr DREG CMP,  C-CC trap BCOND,
   DREG REGION 4 - LIT64,  DREG DBASE DREG ADD,
   addr DREG CMP,  C-HI trap BCOND,
   EREG addr 3 ANDI,  EREG trap CBNZ,
   ok B,
   trap LBL,  0 E-SEAL-VIOLATION MOVZ,  NR-EXIT-GROUP SYS,
   ok LBL, ;

\ ---- primitive bodies (ICode operating on the x19 data stack) ----
: B+ ( -- )   B G-POP  A G-POP  A A B ADD,  A G-PUSH ;

: B- ( -- )   B G-POP  A G-POP  A A B SUB,  A G-PUSH ;

: B* ( -- )   B G-POP  A G-POP  A A B MUL,  A G-PUSH ;

: BDUP ( -- )  A G-POP  A G-PUSH  A G-PUSH ;

: BDROP ( -- ) XDS XDS 8 SUBI, ;

: BSWAP ( -- ) A G-POP  B G-POP  A G-PUSH  B G-PUSH ;

: BDOT ( -- )  A G-POP  G-PRINT9 ;          \ pop x9, print signed decimal + newline

: BU. ( -- )   A G-POP  G-PRINTU9 ;         \ pop x9, print unsigned decimal + newline

: BRUNRC ( -- )  A G-POP                    \ ( pathz -- rc ) spawn+wait; -1 = spawn failed
   LBL {: spok :}  LBL {: spdn :}  LBL {: spw :}
   SP SP 64 SUBI,
   9 SP 16 STR,                      \ argv[0] = path
   10 0 MOVZ,  10 SP 24 STR,         \ argv[1] = 0
   10 SP 48 STR,                     \ envp[0] = 0
   0 SP 0 ADDI,                      \ &pid
   1 9 0 ADDI,
   2 0 MOVZ,                         \ adesc = 0 (kernel API: 5 args, not libc's 6)
   3 SP 16 ADDI,  4 SP 48 ADDI,      \ argv, envp
   NR-SPAWN SYS,
   9 2 CSET,                         \ Darwin carry set means x0 is errno
   9 spok CBZ,                       \ either -> rc -1
      9 0 MOVN,  spdn B,
   spok LBL,
   0 SP 0 LDRW,                      \ pid_t is 32-bit
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
: BCPFETCH ( -- )    9 CP 0 ADDI,  A G-PUSH ;     \ ( -- addr ) live CP (snapshot writer)
: BNDICTFETCH ( -- ) 9 NDICT 0 ADDI,  A G-PUSH ;  \ ( -- n ) live dict count
: BDBASEFETCH ( -- ) 9 DBASE 0 ADDI,  A G-PUSH ;  \ ( -- addr ) region base
: BDATAFETCH ( -- )  9 DATA 0 ADDI,  A G-PUSH ;   \ ( -- addr ) live DATA base
\ cp!/ndict! are the FORGET code-emit sinks (native: habu1.f BCPSET/BNDSET).
\ Both PROT-GUARD the address the sink redirects a write to, so a post-seal
\ value landing in either sealed band fails closed at the sink. Legit FORGET
\ marks are DBASE/CP-region addresses, never a DATA-region band, so the
\ latch-gated guard leaves them intact.
: BCPSET ( -- )      A G-POP  7 4 MOVZ,  A 7 GUARD-SPAN  CP A 0 ADDI, ;   \ ( addr -- ) set CP
: BNDSET ( -- )      A G-POP                                 \ ( n -- ) set NDICT
   C DREC MOVZ,  B A C MUL,  B DBASE B ADD,  7 DREC MOVZ,  B 7 GUARD-SPAN
   NDICT A 0 ADDI, ;

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

: BCREATE ( -- )  15 0 MOVZ,  16 20 CREATEP-CELL LDR,  16 BLR, ;   \ ( "name" -- ) runtime CREATE via the
                                     \ startup-stored cell: subsets emit prims w/o labels

: BCOMPILE ( -- )  A G-POP  11 9 0 ADDI,    \ ( xt -- ) append `movz-chain x16 ; blr x16` at CP
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

: BEMIT ( -- ) A G-POP  13 9 0 ADDI,  G-EMITC ;   \ ( c -- ) write one byte

: BCR ( -- )   13 10 MOVZ,  G-EMITC ;

: BSPACE ( -- ) 13 32 MOVZ,  G-EMITC ;

\ .s — print the whole data stack (base..top), one signed decimal per line, WITHOUT
\ consuming it. The loop pointer lives in a DATA cell because G-PRINT9 clobbers x9..x15.
: B.S ( -- )
   9 DATA S0-CELL LDR,  9 DATA SSCR-CELL STR,
   LBL {: sl :}  LBL {: sd :}
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

\ comparisons -> Forth flag 0/-1 (CSET 0/1 then negate via the zero register SP)
: (CMP) ( n -- ) {: cond -- :}  B G-POP  A G-POP  A B CMP,  A cond CSET,  A SP A SUB,  A G-PUSH ;

: B= ( -- )  C-EQ (CMP) ;

: B<> ( -- ) C-NE (CMP) ;

: B< ( -- )  C-LT (CMP) ;

: B> ( -- )  C-GT (CMP) ;

: B<= ( -- ) C-LE (CMP) ;

: B>= ( -- ) C-GE (CMP) ;

: B0= ( -- ) A G-POP  A 0 CMPI,  A C-EQ CSET,  A SP A SUB,  A G-PUSH ;

: B0< ( -- ) A G-POP  A 0 CMPI,  A C-LT CSET,  A SP A SUB,  A G-PUSH ;

: B1+ ( -- ) A G-POP  A A 1 ADDI,  A G-PUSH ;

: B1- ( -- ) A G-POP  A A 1 SUBI,  A G-PUSH ;

\ bitwise / logic
: BAND ( -- ) B G-POP A G-POP  A A B AND, A G-PUSH ;

: BOR ( -- )  B G-POP A G-POP  A A B ORR, A G-PUSH ;

: BXOR ( -- ) B G-POP A G-POP  A A B EOR, A G-PUSH ;

: BINV ( -- ) A G-POP  B 0 MOVN,  A A B EOR,  A G-PUSH ;     \ A ^ -1

: BNEG ( -- ) A G-POP  A SP A SUB,  A G-PUSH ;               \ 0 - A

\ shifts (variable count); /, mod via SDIV/MUL
: BLSH ( -- ) B G-POP A G-POP  A A B LSLV, A G-PUSH ;

: BRSH ( -- ) B G-POP A G-POP  A A B LSRV, A G-PUSH ;

: BDIV0? ( -- ) LBL {: lok :} B lok CBNZ, BRK, lok LBL, ;   \ SDIV by 0 silently yields 0; trap a zero divisor (B)

: BDIV ( -- ) B G-POP A G-POP  BDIV0?  A A B SDIV, A G-PUSH ;

: BMOD ( -- ) B G-POP A G-POP  BDIV0?  C A B SDIV,  C C B MUL,  A A C SUB,  A G-PUSH ;

: BDIVMOD ( -- )
   B G-POP
   A G-POP
   BDIV0?
   C A B SDIV,
   DREG C B MUL,
   A A DREG SUB,
   A G-PUSH
   C G-PUSH ;

: BABS ( -- ) A G-POP  A 0 CMPI,  LBL {: done :}  C-GE done BCOND,  A SP A SUB,  done LBL,  A G-PUSH ;

: BMIN ( -- )
   B G-POP
   A G-POP
   A B CMP,
   \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
   LBL {: done :}
   C-LE done BCOND,
   A B 0 ADDI,
   done LBL,
   A G-PUSH ;

: BMAX ( -- )
   B G-POP
   A G-POP
   A B CMP,
   \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
   LBL {: done :}
   C-GE done BCOND,
   A B 0 ADDI,
   done LBL,
   A G-PUSH ;

\ stack shuffles (memory on x19)
: BNIP ( -- )  A G-POP  XDS XDS 8 SUBI,  A G-PUSH ;

: BOVER ( -- ) B G-POP A G-POP  A G-PUSH B G-PUSH A G-PUSH ;

: BTUCK ( -- ) B G-POP A G-POP  B G-PUSH A G-PUSH B G-PUSH ;

: BROT ( -- )  C G-POP B G-POP A G-POP  B G-PUSH C G-PUSH A G-PUSH ;

: BMROT ( -- ) C G-POP B G-POP A G-POP  C G-PUSH A G-PUSH B G-PUSH ;

: B2DUP ( -- ) B G-POP A G-POP  A G-PUSH B G-PUSH A G-PUSH B G-PUSH ;

: B2DROP ( -- ) XDS XDS 16 SUBI, ;

: B2SWAP ( -- ) EREG G-POP DREG G-POP C G-POP A G-POP  DREG G-PUSH EREG G-PUSH A G-PUSH C G-PUSH ;

: B2OVER ( -- )
   EREG G-POP
   DREG G-POP
   C G-POP
   A G-POP
   A G-PUSH
   C G-PUSH
   DREG G-PUSH
   EREG G-PUSH
   A G-PUSH
   C G-PUSH ;

: BQDUP ( -- ) A G-POP  A G-PUSH  LBL {: done :}  A done CBZ,  A G-PUSH  done LBL, ;

\ memory access (absolute addresses on the stack)
: BFETCH ( -- )  A G-POP  A A 0 LDR,  A G-PUSH ;

\ PROT-GUARD (the two-band write guard) is defined with the register constants
\ near the top of this file -- before the earliest guarded sink (cp!/ndict!).

: BSTORE ( -- )  B G-POP A G-POP  7 8 MOVZ,  B 7 GUARD-SPAN  A B 0 STR, ;  \ ( val addr -- )

: BPTRFIELD ( -- )  B G-POP  A G-POP  B B 3 LSLI,  A A B ADD,  A G-PUSH ;

: BPLUSSTORE ( -- ) B G-POP A G-POP  7 8 MOVZ,  B 7 GUARD-SPAN  C B 0 LDR,  C C A ADD,  C B 0 STR, ;

: BCFETCH ( -- ) A G-POP  A A 0 LDRB, A G-PUSH ;

: BCSTORE ( -- ) B G-POP A G-POP  7 1 MOVZ,  B 7 GUARD-SPAN  A B 0 STRB, ;

: BCELLS ( -- )  A G-POP  A A 3 LSLI, A G-PUSH ;             \ n*8

: BCELLPLUS ( -- ) A G-POP  A A 8 ADDI, A G-PUSH ;

: BCHARS ( -- ) ;

: BCHARPLUS ( -- ) A G-POP  A A 1 ADDI, A G-PUSH ;

: BCOUNT ( -- ) A G-POP  B A 0 LDRB,  A A 1 ADDI,  A G-PUSH  B G-PUSH ;

\ Labeled fd-2 diagnostic + exit(rc): branch over the inline message bytes,
\ write them to fd 2, then exit_group(rc). Mirrors the native engine's labeled
\ capacity/snapshot exits so no fatal path is a bare rc-only exit.
: C-EXIT-DIAG ( ptr u8 n rc -- ) {: a u rc :} \ typed-local-lint: allow-bare-local
   LBL LBL {: msg after :} \ typed-local-lint: allow-bare-local
   after B,
   msg LBL,  a u BYTES,
   after LBL,
   0 2 MOVZ,  1 msg ADR,  2 u MOVZ,  NR-WRITE SYS,
   0 rc MOVZ,  NR-EXIT-GROUP SYS, ;

: C-EXIT76 ( ptr u8 n -- )  76 C-EXIT-DIAG ;

\ data space: DP cell is [x20]; HERE/ALLOT/,/C, bump it (x20 region is always RW)
: BHERE ( -- )   7 DATA 0 LDR,  7 G-PUSH ;

: DP-CHECK ( n -- ) {: reg -- :}
   LBL LBL {: low-ok high-ok :}
   5 DATA-START LIT64,  5 DATA 5 ADD,
   reg 5 CMP,  C-GE low-ok BCOND,
      s" bootstrap: data pointer below data start" C-EXIT76
   low-ok LBL,
   5 DATA-SIZE LIT64,  5 DATA 5 ADD,
   reg 5 CMP,  C-LE high-ok BCOND,
      s" bootstrap: data pointer exceeds data region" C-EXIT76
   high-ok LBL, ;

: BALLOT ( -- )  A G-POP  7 DATA 0 LDR,  7 7 A ADD,  7 DP-CHECK  7 DATA 0 STR, ;

: BCOMMA ( -- )  A G-POP  7 DATA 0 LDR,  C 7 8 ADDI,  C DP-CHECK  A 7 0 STR,  C DATA 0 STR, ;

: BCCOMMA ( -- ) A G-POP  7 DATA 0 LDR,  C 7 1 ADDI,  C DP-CHECK  A 7 0 STRB, C DATA 0 STR, ;

: BTYPE ( -- )   2 G-POP  1 G-POP  0 1 MOVZ,  NR-WRITE SYS, ;   \ ( addr len -- ) write(1,..)

\ die ( a u code -- noreturn ): msg to stderr, exit(code). The in-subset abort for
\ compiler invariant violations — better a loud death than silent memory corruption.
\ The requested rc is honored when kernel-representable ([0,255]; 0 stays the
\ deliberate success exit); anything else would be silently masked to `rc & 0xFF`,
\ so it maps to UNCAUGHT-RC instead (mirrors src/habu/habu1.f BDIE).
: BDIE ( -- )
   LBL {: lfixed :} \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
   7 G-POP  2 G-POP  1 G-POP  0 2 MOVZ,  NR-WRITE SYS,
   0 7 0 ADDI,
   7 0 CMPI,    C-LT lfixed BCOND,
   7 255 CMPI,  C-GT lfixed BCOND,
   NR-EXIT-GROUP SYS,
   lfixed LBL,  0 UNCAUGHT-RC MOVZ,  NR-EXIT-GROUP SYS, ;

: SYS-PUSH ( -- )                         \ push x0, or -1 when the syscall carry is set
   LBL LBL {: ok done :}
   9 C-CS CSET,  9 ok CBZ,
      0 0 MOVN,  done B,
   ok LBL,
   done LBL,
   0 G-PUSH ;

\ file I/O (path must be NUL-terminated by the caller)
: BOPEN ( -- )
   2 G-POP  1 G-POP  0 G-POP
   HB-TARGET-LINUX? IF
      3 2 0 ADDI,
      OS-OPEN-FLAGS
      1 0 0 ADDI,
      0 99 MOVN,
   THEN
   NR-OPEN SYS,  SYS-PUSH ;   \ ( pathz flags mode -- fd )

: BOPENRD ( -- )
   A G-POP
   A OS-OPEN-RD
   SYS-PUSH ;

: BWRITE ( -- )  2 G-POP  1 G-POP  0 G-POP  NR-WRITE SYS,  0 G-PUSH ;   \ ( fd buf len -- n )

: BREAD ( -- )   2 G-POP  1 G-POP  0 G-POP  1 2 GUARD-SPAN  NR-READ SYS,  0 G-PUSH ;  \ ( fd buf len -- n )

: BIOCTL ( -- )  2 G-POP  1 G-POP  0 G-POP  2 PROT-GUARD  NR-IOCTL SYS,  0 G-PUSH ;

: BMMAP ( -- )
   5 G-POP  4 G-POP  3 G-POP  2 G-POP  1 G-POP  0 G-POP
   LBL {: notfixed :} \ typed-local-lint: allow-bare-local
   6 3 $10 ANDI,  6 notfixed CBZ,
      0 1 GUARD-SPAN
   notfixed LBL,
   HB-TARGET-LINUX? IF OS-MMAP-FLAGS THEN
   NR-MMAP SYS,  SYS-PUSH ; \ ( addr len prot flags fd off -- addr|-1 )

: C-FLUSH-X9-LINE ( -- )
   9 DCCVAU,  DSB-ISH,  9 ICIVAU,  DSB-ISH,  ISB, ;

: BPATCH32 ( -- )                       \ ( w addr -- ): RW-flip, store, RX, cache-sync —
   A G-POP  B G-POP              \ all inside ENGINE text (a JIT-resident caller
   7 4 MOVZ,  A 7 GUARD-SPAN      \ x9 is the target; protect its exact 4-byte write
   SP SP 32 SUBI,                \ flipping the region would unmap ITSELF)
   A SP 8 STR,  B SP 16 STR,
   2 3 MOVZ,  LPROT @ BL,
   9 SP 8 LDR,  10 SP 16 LDR,  10 9 0 STRW,
   2 5 MOVZ,  LPROT @ BL,
   9 SP 8 LDR,  C-FLUSH-X9-LINE
   SP SP 32 ADDI, ;

: BCLOSE ( -- )  0 G-POP  NR-CLOSE SYS, ;                               \ ( fd -- )

: BRBASE ( -- )  9 DATA RBASE-CELL LDR,  9 G-PUSH ;                            \ ( -- rbase ) __TEXT load base

: BEXEC ( -- )   A G-POP  SP SP 16 SUBI,  30 SP 0 STR,  A BLR,  30 SP 0 LDR,  SP SP 16 ADDI, ;  \ ( xt -- )

\ catch ( xt -- exc ) / throw ( exc -- ). Handler frames chain through [x20+8]
\ (=HND). A frame (48 B on the machine stack) saves: prev-HND, data-sp(x19),
\ machine-sp, resume-pc (an ADR within this stencil — PC-relative, survives the
\ memcpy that inlines the stencil), and the link register.
: BCATCH ( -- )
   A G-POP                               \ xt -> x9
   SP SP 48 SUBI,
   30 SP 32 STR,                         \ save link
   11 DATA 8 LDR,  11 SP 0 STR,          \ prev HND
   19 SP 8 STR,                          \ data sp
   13 SP 48 ADDI,  13 SP 16 STR,         \ machine sp to restore (= frame+48)
   LBL {: lres :}  LBL {: lpush :}
   12 lres ADR,  12 SP 24 STR,           \ resume pc
   14 SP 0 ADDI,  14 DATA 8 STR,         \ HND = this frame
   9 BLR,                                \ run xt (may throw)
   11 SP 0 LDR,  11 DATA 8 STR,          \ normal: HND = prev
   30 SP 32 LDR,  SP SP 48 ADDI,         \ restore link, pop frame
   9 0 MOVZ,  lpush B,                   \ exc = 0
   lres LBL,                             \ throw lands here (x9=exc, sp/HND/lr restored)
   lpush LBL,  9 G-PUSH ;                \ push exc (0 normal / exc on throw)

: BTHROW ( -- )
   A G-POP                               \ exc -> x9
   11 DATA 8 LDR,                        \ HND
   LBL {: lnoh :}  11 lnoh CBZ,
   19 11 8 LDR,                          \ restore data sp
   10 11 0 LDR,  10 DATA 8 STR,          \ HND = prev
   30 11 32 LDR,  12 11 24 LDR,  13 11 16 LDR,   \ link, resume pc, machine sp
   SP 13 0 ADDI,  12 BR,                 \ restore sp; jump to catch's resume
   lnoh LBL,
   10 DATA REPLH-CELL LDR,  LBL {: lnorec :}  10 lnorec CBZ,
   10 DATA RRECP-CELL LDR,  10 BR,                                \ tty REPL: recover instead of dying
   lnorec LBL,                           \ no handler: exit(exc) if kernel-representable [1,255], else the
   LBL {: lfixed :} \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
   0 9 0 ADDI,                           \ deterministic UNCAUGHT-RC, never the silently masked code
                                         \ (mirrors src/habu/habu1.f BTHROW THROW-NOREC-FB)
   9 1 CMPI,    C-LT lfixed BCOND,
   9 255 CMPI,  C-GT lfixed BCOND,
   NR-EXIT-GROUP SYS,
   lfixed LBL,  0 UNCAUGHT-RC MOVZ,  NR-EXIT-GROUP SYS, ;

\ wordlists: each dict record carries a wid (offset 40). New defs take CURRENT.
: BWORDLIST ( -- )  9 DATA WIDN-CELL LDR,  9 G-PUSH  9 9 1 ADDI,  9 DATA WIDN-CELL STR, ;  \ ( -- wid )

: BGETCUR ( -- )    9 DATA CUR-CELL LDR,  9 G-PUSH ;                                       \ ( -- wid )

: BSETCUR ( -- )    A G-POP  A DATA CUR-CELL STR, ;                                        \ ( wid -- )

: BSETCHECK ( -- )  A G-POP  A DATA HOOK-CELL STR, ;                                       \ ( xt -- ): install check hook

\ SEAL-CAPTURE (TFAM 2b-iii): freeze the seal-time ndict truncation watermark
\ (xref.f baseline token + the cold-prefix assembler's token at the true
\ engine-prefix end). The friend latch is already sealed by then, so a raw !
\ would trap; this direct STR from NDICT is the sanctioned bypass, mirroring
\ native src/habu/habu1.f BSEALCAP.
: BSEALCAP ( -- )   NDICT DATA SEAL-NDICT-CELL STR, ;                                      \ ( -- )
: BSEALFRIEND ( -- ) 9 FRIEND-ARENA-LEN MOVZ,  9 DATA FRIEND-LATCH-CELL STR, ;              \ ( -- )
\ wide-mark ( -- ): set DNAME-WIDE on the newest dict record (interpret-mode
\ wide-effect gate; mirrors src/habu/habu1.f BWIDEMARK incl. the LPROT bracket).
: BWIDEMARK ( -- )
   2 3 MOVZ,  LPROT @ BL,
   9 NDICT 0 ADDI,  9 9 1 SUBI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   10 9 16 LDR,  10 10 DNAME-WIDE ORRI,  10 9 16 STR,
   2 5 MOVZ,  LPROT @ BL, ;
\ Recovery and native use the same protected-WID registry contract.
: BPROTWIDADD ( -- )
   LBL LBL LBL {: room done msg :} \ typed-local-lint: allow-bare-local
   9 G-POP
   LPROTWIDQ @ BL,
   13 done CBNZ,
   14 DATA PROT-WID-N-CELL LDR,
   14 PROT-WID-MAX CMPI,  C-LT room BCOND,
      0 2 MOVZ,  1 msg ADR,  2 28 MOVZ,  NR-WRITE SYS,
      0 E-SEAL-PACKAGE MOVZ,  NR-EXIT-GROUP SYS,
      msg LBL,  s" hb: protected-WID table full" BYTES,
   room LBL,
   15 PROT-WID-OFF MOVZ,  15 DATA 15 ADD,
   16 14 2 LSLI,  15 15 16 ADD,
   9 15 0 STRW,
   14 14 1 ADDI,  14 DATA PROT-WID-N-CELL STR,
   done LBL, ;

\ search-wl ( a u wid -- addr|0 ): find name (a,u) in wordlist wid (case-folded)
: BSWL ( -- )
   LBL LBL LBL LBL LBL LBL LBL LBL {: wl wend wnext wcmp wmatch wf1 wf2 winl :}
   2 G-POP  1 G-POP  0 G-POP                      \ wid=x2, u=x1, a=x0
   3 $20 MOVZ,  5 DBASE 0 ADDI,  6 NDICT 0 ADDI,  11 0 MOVZ,   \ fold mask, rec, count, result
   wl LBL,  6 wend CBZ,
      9 5 40 LDR,  9 2 CMP,  C-NE wnext BCOND,    \ wid mismatch
      9 5 16 LDR,  9 9 12 LSLI,  9 9 12 LSRI,  9 1 CMP,  C-NE wnext BCOND,    \ namelen mismatch
      16 5 24 ADDI,
      9 5 16 LDR,  9 9 DNAME-EXT ANDI,  9 winl CBZ,
         16 5 24 LDR,
      winl LBL,
      7 0 MOVZ,
      wcmp LBL,  7 1 CMP,  C-GE wmatch BCOND,
         9 16 7 ADD,  9 9 0 LDRB,                 \ rec.name[j]
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

: BPARSE-NAME ( -- )
   LBL LBL {: none done :}
   LTOK @ BL,
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
   s" 2swap" ['] B2SWAP FPRIM-L  s" 2over" ['] B2OVER FPRIM-L  s" ?dup" ['] BQDUP FPRIM-L ;

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

: EMIT-ENGINE-PRIMS ( -- )
   s" run-rc" ['] BRUNRC FPRIM-L
   s" cp@" ['] BCPFETCH FPRIM-L   s" dbase@" ['] BDBASEFETCH FPRIM-L
   s" data-base" ['] BDATAFETCH FPRIM-L
   s" ndict@" ['] BNDICTFETCH FPRIM-L
   s" cp!" ['] BCPSET FPRIM-L   s" ndict!" ['] BNDSET FPRIM-L
   s" SEAL-CAPTURE" ['] BSEALCAP FPRIM-L
   s" SEAL-FRIEND" ['] BSEALFRIEND FPRIM-L
   s" wide-mark" ['] BWIDEMARK FPRIM
   s" prot-wid-add" ['] BPROTWIDADD FPRIM-L
   s" die"  ['] BDIE   FPRIM-L ;

: EMIT-FS-PRIMS ( -- )
   s" open" ['] BOPEN FPRIM-L   s" open-rd" ['] BOPENRD FPRIM-L
   s" write" ['] BWRITE FPRIM-L   s" read" ['] BREAD FPRIM-L   s" ioctl" ['] BIOCTL FPRIM-L
   s" mmap" ['] BMMAP FPRIM-L   s" patch32" ['] BPATCH32 FPRIM
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
   EMIT-ENGINE-PRIMS  EMIT-FS-PRIMS  EMIT-CHECKER-PRIMS ;

\ ---- CEMIT ( x9=word -- ) : str w9,[x28] ; CP += 4 ----
\ FP: doubles as raw IEEE754 bit-cells on the data stack; FMOV through D0/D1.
\ Compare conds per FP flag semantics: < MI, > GT, = EQ (NaN compares false).
: BF+ ( -- )    B G-POP  A G-POP  0 A FMOVXD,  1 B FMOVXD,  0 0 1 FADD,  A 0 FMOVDX,  A G-PUSH ;

: BF- ( -- )    B G-POP  A G-POP  0 A FMOVXD,  1 B FMOVXD,  0 0 1 FSUB,  A 0 FMOVDX,  A G-PUSH ;

: BF* ( -- )    B G-POP  A G-POP  0 A FMOVXD,  1 B FMOVXD,  0 0 1 FMUL,  A 0 FMOVDX,  A G-PUSH ;

: BF/ ( -- )    B G-POP  A G-POP  0 A FMOVXD,  1 B FMOVXD,  0 0 1 FDIV,  A 0 FMOVDX,  A G-PUSH ;

: BFNEG ( -- )  A G-POP  0 A FMOVXD,  0 0 FNEG,   A 0 FMOVDX,  A G-PUSH ;

: BFABS ( -- )  A G-POP  0 A FMOVXD,  0 0 FABS,   A 0 FMOVDX,  A G-PUSH ;

: BFSQRT ( -- ) A G-POP  0 A FMOVXD,  0 0 FSQRT,  A 0 FMOVDX,  A G-PUSH ;

: (FCMP) ( n -- ) {: cond :}  B G-POP  A G-POP  0 A FMOVXD,  1 B FMOVXD,  0 1 FCMP,
   A cond CSET,  A SP A SUB,  A G-PUSH ;

: BF< ( -- )  C-MI (FCMP) ;

: BF> ( -- )  C-GT (FCMP) ;

: BF= ( -- )  C-EQ (FCMP) ;

: (FCMP0) ( n -- ) {: cond :}  A G-POP  0 A FMOVXD,  0 FCMP0,
   A cond CSET,  A SP A SUB,  A G-PUSH ;

: BF0< ( -- ) C-MI (FCMP0) ;

: BF0= ( -- ) C-EQ (FCMP0) ;

: BS>F ( -- )  A G-POP  0 A SCVTF,   A 0 FMOVDX,  A G-PUSH ;

: BF>S ( -- )  A G-POP  0 A FMOVXD,  A 0 FCVTZS,  A G-PUSH ;

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

: EMIT-CEMIT ( -- )
   LCEMIT @ LBL,
   SP SP 16 SUBI,  12 SP 0 STR,  13 SP 8 STR,
   28 GUARD-CODE-WORD
   12 SP 0 LDR,  13 SP 8 LDR,  SP SP 16 ADDI,
   9 28 0 STRW,  28 28 4 ADDI,  RET, ;

\ LBCAP ( -- ) : append TKA/TKL + ' ' to the body capture. LBCS ( x11=a x12=u )
\ is the general entry (defining-word kind tokens). FATAL (exit 71) on overflow —
\ truncation would let the check hook certify code it never saw.
: EMIT-BCAP ( -- )
   LBCAP @ LBL,
   11 DATA TKA-CELL LDR,  12 DATA TKL-CELL LDR,
   LBCS @ LBL,
   LBL LBL LBL LBL {: bok bcp bcd go :} \ typed-local-lint: allow-bare-local - Gforth bootstrap labels cannot use Habu type suffixes.
   14 DATA P2-CELL LDR,  14 go CBZ,  RET,
   go LBL,
   17 12 0 ADDI,                  \ len in x17 (IP1): callers keep state in x5-x8
   14 DATA BODYLEN-CELL LDR,
   16 14 17 ADD,  16 16 1 ADDI,
   5 BODYBUF-CAP MOVZ,  16 5 CMP,  C-LE bok BCOND,
      0 2 MOVZ,  1 11 0 ADDI,  2 12 0 ADDI,  NR-WRITE SYS,
      0 71 MOVZ,  NR-EXIT-GROUP SYS,
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
   LBL {: tskip :}  LBL {: thas :}  LBL {: tscan :}
   LBL {: tgot :}   LBL {: tnone :}
   11 DATA INP-CELL LDR,  12 DATA INE-CELL LDR,
   tskip LBL,                                          \ skip whitespace (any byte <= 32)
      11 12 CMP,  C-GE tnone BCOND,
      9 11 0 LDRB,  9 32 CMPI,  C-HI thas BCOND,      \ c > 32 -> token start
      11 11 1 ADDI,  tskip B,
   thas LBL,  11 DATA TKA-CELL STR,
   tscan LBL,                                          \ scan to next whitespace
      11 12 CMP,  C-GE tgot BCOND,
      9 11 0 LDRB,  9 32 CMPI,  C-LS tgot BCOND,      \ c <= 32 -> token end
      11 11 1 ADDI,  tscan B,
   tgot LBL,  9 DATA TKA-CELL LDR,  9 11 9 SUB,  9 DATA TKL-CELL STR,
      11 DATA INP-CELL STR,  0 1 MOVZ,  RET,
   tnone LBL,  11 DATA INP-CELL STR,  0 0 MOVZ,  RET, ;

\ ---- PROT ( x2=prot -- ) : mprotect(region, REGION, prot) ----
: EMIT-PROT ( -- )
   LPROT @ LBL,
   0 DBASE 0 ADDI,  1 REGION LIT64,  NR-MPROTECT SYS,  RET,
   \ Runtime lowering guard: x10=address, x11=byte length.
   LBL dup LPROTSPAN ! LBL,
   10 11 GUARD-SPAN
   RET, ;

: EMIT-PROTWID ( -- )
   LBL LBL LBL {: loop next done :} \ typed-local-lint: allow-bare-local
   LPROTWIDQ @ LBL,
   13 0 MOVZ,
   6 DATA PROT-WID-N-CELL LDR,
   7 0 MOVZ,
   5 PROT-WID-OFF MOVZ,  5 DATA 5 ADD,
   loop LBL,  7 6 CMP,  C-GE done BCOND,
      14 5 0 LDRW,  14 9 CMP,  C-NE next BCOND,
         13 1 MOVZ,  done B,
      next LBL,  5 5 4 ADDI,  7 7 1 ADDI,  loop B,
   done LBL,  RET, ;

\ ---- FLUSH ( x9=start -- ) : DC CVAU + IC IVAU over [x9, CP) — just the words
\ emitted since the last flush, not the whole code area (that walk made every
\ `;` cost O(total code), O(n^2) over a program build) ----
: EMIT-FLUSH ( -- )
   LFLUSH @ LBL,
   LBL {: fdl :}  LBL {: fdd :}  LBL {: fil :}  LBL {: fid :}
   9 9 6 LSRI,  9 9 6 LSLI,                                 \ align start down to the
   10 9 0 ADDI,                                             \ line, or the 64-byte
                                                            \ stride skips the last one
   fdl LBL,  10 CP CMP,  C-GE fdd BCOND,  10 DCCVAU,  10 10 64 ADDI,  fdl B,
   fdd LBL,  DSB-ISH,
   10 9 0 ADDI,
   fil LBL,  10 CP CMP,  C-GE fid BCOND,  10 ICIVAU,  10 10 64 ADDI,  fil B,
   fid LBL,  DSB-ISH,  ISB,  RET, ;

\ ---- FIND ( x9=tka x10=tkl -- x11=addr x12=clen x13=found|imm<<1 ) over 48-byte records ----
\ Qualified tokens search the named package's public WID. Bare tokens inside an
\ open package search private, public, then FORTH; outside they search FORTH.
: EMIT-FIND ( -- )
   LFIND @ LBL,
   LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL
   {: qscan qunqual qpkg qloop qnext qcmp qmatch qinl qmiss floop fdone fnext frank fcmp fscan fhit finl pri pub have :} \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
   0 9 0 ADDI,  1 10 0 ADDI,                             \ original token
   2 0 MOVN,  2 2 1 SUBI,                               \ -2 = unqualified
   3 9 0 ADDI,  4 10 0 ADDI,  7 0 MOVZ,                \ search token, len, colon index
   qscan LBL,
      7 1 CMP,  C-GE qunqual BCOND,
      8 0 7 ADD,  8 8 0 LDRB,  8 $3A CMPI,  C-EQ qpkg BCOND,
      7 7 1 ADDI,  qscan B,
   qpkg LBL,
      7 qunqual CBZ,  8 7 1 ADDI,  8 1 CMP,  C-GE qunqual BCOND,
      5 DBASE 0 ADDI,  6 NDICT 0 ADDI,
   qloop LBL,
      6 qmiss CBZ,
      14 5 40 LDR,  15 0 MOVN,  14 15 CMP,  C-NE qnext BCOND,
      14 5 16 LDR,  14 14 12 LSLI,  14 14 12 LSRI,  14 7 CMP,  C-NE qnext BCOND,
      16 5 24 ADDI,
      14 5 16 LDR,  14 14 DNAME-EXT ANDI,  14 qinl CBZ,
         16 5 24 LDR,
      qinl LBL,
      8 0 MOVZ,
      qcmp LBL,
         8 7 CMP,  C-GE qmatch BCOND,
         14 16 8 ADD,  14 14 0 LDRB,
         15 14 $41 SUBI,  15 26 CMPI,  15 C-CC CSET,  15 15 5 LSLI,  14 14 15 ORR,
         17 0 8 ADD,  17 17 0 LDRB,
         15 17 $41 SUBI,  15 26 CMPI,  15 C-CC CSET,  15 15 5 LSLI,  17 17 15 ORR,
         14 17 CMP,  C-NE qnext BCOND,
         8 8 1 ADDI,  qcmp B,
      qmatch LBL,
         2 5 0 LDR,                                    \ package public WID
         3 0 7 ADD,  3 3 1 ADDI,
         4 1 7 SUB,  4 4 1 SUBI,
         5 DBASE 0 ADDI,  6 NDICT 0 ADDI,  7 0 MOVZ,  13 0 MOVZ,
         floop B,
      qnext LBL,  5 5 DREC ADDI,  6 6 1 SUBI,  qloop B,
   qmiss LBL,  13 0 MOVZ,  RET,
   qunqual LBL,
   5 DBASE 0 ADDI,  6 NDICT 0 ADDI,  7 0 MOVZ,  13 0 MOVZ,
   floop LBL,
      6 fdone CBZ,
      8 5 40 LDR,
      2 0 CMPI,  C-GE frank BCOND,
         14 DATA PKG-PRI-CELL LDR,  8 14 CMP,  C-NE pri BCOND,  14 3 MOVZ,  frank B,
      pri LBL,
         15 DATA PKG-PUB-CELL LDR,  8 15 CMP,  C-NE pub BCOND,  14 2 MOVZ,  frank B,
      pub LBL,
         8 fnext CBNZ,  14 1 MOVZ,
      frank LBL,
      2 0 CMPI,  C-LT fcmp BCOND,
         8 2 CMP,  C-NE fnext BCOND,  14 1 MOVZ,
      fcmp LBL,
      14 7 CMP,  C-LT fnext BCOND,
      15 5 16 LDR,  15 15 12 LSLI,  15 15 12 LSRI,  15 4 CMP,  C-NE fnext BCOND,
      16 5 24 ADDI,
      15 5 16 LDR,  15 15 DNAME-EXT ANDI,  15 finl CBZ,
         16 5 24 LDR,
      finl LBL,
      8 0 MOVZ,
      fscan LBL,
         8 4 CMP,  C-GE fhit BCOND,
         15 16 8 ADD,  15 15 0 LDRB,
         17 15 $41 SUBI,  17 26 CMPI,  17 C-CC CSET,  17 17 5 LSLI,  15 15 17 ORR,
         12 3 8 ADD,  12 12 0 LDRB,
         17 12 $41 SUBI,  17 26 CMPI,  17 C-CC CSET,  17 17 5 LSLI,  12 12 17 ORR,
         15 12 CMP,  C-NE fnext BCOND,
         8 8 1 ADDI,  fscan B,
      fhit LBL,                                            \ keep scanning: take the LAST
         11 5 0 ADDI,  13 1 MOVZ,  7 14 0 ADDI,
      fnext LBL,  5 5 DREC ADDI,  6 6 1 SUBI,  floop B,
   fdone LBL,
      13 have CBNZ,  RET,
   have LBL,
      5 11 0 ADDI,  12 5 8 LDR,
      15 5 16 LDR,
      8 15 DNAME-WIDE ANDI,  8 8 59 LSRI,
      15 15 DNAME-IMM ANDI,  15 15 59 LSRI,
      15 15 8 ORR,
      13 1 MOVZ,  13 13 15 ORR,
      11 5 0 LDR,  RET, ;

\ ---- NUMBER? ( x9=tka x10=tkl -- x11=val x12=ok ) ----
\ Accepts decimal and $hex, each with an optional leading '-'.  x6=base, x7=digit.
: C-NUM-INIT-REGS ( -- )
   11 0 MOVZ,  13 1 MOVZ,  14 0 MOVZ,  12 0 MOVZ,  6 10 MOVZ, ;

: C-NUM-SIGN ( n n -- ) {: ldone ndoll :}
   10 ldone CBZ,                                                \ empty token -> fail
   15 9 0 LDRB,  15 45 CMPI,  C-NE ndoll BCOND,                 \ leading '-'
      13 0 MOVN,  14 1 MOVZ,
   ndoll LBL,
   14 10 CMP,  C-GE ldone BCOND, ;                              \ "-" only -> fail

: C-NUM-BASE ( n n -- ) {: ldone nohex :}
   5 9 14 ADD,  15 5 0 LDRB,  15 36 CMPI,  C-NE nohex BCOND,    \ '$' prefix
      6 16 MOVZ,  14 14 1 ADDI,
   nohex LBL,
   2 0 MOVZ,                                                    \ frac mode off
   14 10 CMP,  C-GE ldone BCOND, ;                              \ nothing after sign/$ -> fail

: C-NUM-DOT ( n n n -- ) {: ldone lloop ndot :}
   15 46 CMPI,  C-NE ndot BCOND,                                \ '.' -> frac mode
      6 10 CMPI,  C-NE ldone BCOND,
      2 ldone CBNZ,
      2 1 MOVZ,  4 0 MOVZ,  3 1 MOVZ,
      14 14 1 ADDI,  lloop B,
   ndot LBL, ;

: C-NUM-DIGIT ( n n n n -- ) {: ldone gotd nd nuc :}
   15 48 CMPI,  C-LT ldone BCOND,                               \ < '0' -> fail
   15 57 CMPI,  C-GT nd BCOND,
      7 15 48 SUBI,  gotd B,                                    \ '0'..'9' -> c-48
   nd LBL,
   6 16 CMPI,  C-NE ldone BCOND,                                \ non-hex base -> fail
   15 97 CMPI,  C-LT nuc BCOND,  15 102 CMPI,  C-GT ldone BCOND,
      7 15 87 SUBI,  gotd B,                                    \ 'a'..'f' -> c-87
   nuc LBL,
   15 65 CMPI,  C-LT ldone BCOND,  15 70 CMPI,  C-GT ldone BCOND,
      7 15 55 SUBI, ;                                           \ 'A'..'F' -> c-55

: C-NUM-INT-STEP ( n -- ) {: lloop :}
   11 11 6 MUL,  11 11 7 ADD,                                   \ val = val*base + digit
   14 14 1 ADDI,  lloop B, ;

: C-NUM-FRAC-STEP ( n -- ) {: lloop :}
   5 10 MOVZ,  4 4 5 MUL,  4 4 7 ADD,  3 3 5 MUL,
   14 14 1 ADDI,  lloop B, ;

: C-NUM-FLOAT-FINISH ( n n -- ) {: ldone fpos :}
   3 1 CMPI,  C-EQ ldone BCOND,                                 \ "1." (no frac digits) -> fail
   0 11 SCVTF,  1 4 SCVTF,  2 3 SCVTF,                          \ int, frac, scale
   1 1 2 FDIV,  0 0 1 FADD,
   13 0 CMPI,  C-GE fpos BCOND,  0 0 FNEG,
   fpos LBL,  11 0 FMOVDX,  12 1 MOVZ,  RET, ;

: C-NUM-INT-FINISH ( -- )
   11 11 13 MUL,  12 1 MOVZ, ;

: EMIT-NUM ( -- )
   LNUM @ LBL,
   LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL
   {: ldone ndoll nohex lloop lok gotd nd nuc ndot isfrac lint fpos :}
   C-NUM-INIT-REGS
   ldone ndoll C-NUM-SIGN
   ldone nohex C-NUM-BASE
   lloop LBL,
   14 10 CMP,  C-GE lok BCOND,
   5 9 14 ADD,  15 5 0 LDRB,                                    \ c = next byte
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

: C-X9-LIT ( -- )
   6 11 0 ADDI,  5 $FFFF MOVZ,
   7 6 5 AND,    7 7 5 LSLI,  8 W-MOVZ0 LIT64,  9 8 7 ORR,  LCEMIT @ BL,
   7 6 16 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK1 LIT64,  9 8 7 ORR,  LCEMIT @ BL,
   7 6 32 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK2 LIT64,  9 8 7 ORR,  LCEMIT @ BL,
   7 6 48 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK3 LIT64,  9 8 7 ORR,  LCEMIT @ BL, ;

\ ---- compile-mode CALL-or-INLINE (x11=target addr, x12=clen from FIND) ----
\ Small leaf bodies are inlined (copy the meat between the x30 prologue/epilogue);
\ everything else gets an absolute `movz/movk x16 + blr x16` call. Absolute, not BL:
\ the JIT region is a kernel-placed mmap and prims live in __TEXT — BL's +-128MB imm26
\ would silently truncate if they land far apart. x16 is IP0, the ABI call scratch.
\ Inline criteria: meat <= INL-MAX bytes AND no call/branch/RET/ADR/ADRP word in it
\ (calls need the frame; ADR is PC-relative). Internal label branches are relative and
\ copy safely. Bodies without the prologue (CREATE/VARIABLE/CONSTANT literal-pushes)
\ inline whole. Dict clen: prim = end-start-4, user word = set at `;` — both excl RET.
$28 constant INL-MAX   \ 40 bytes = 10 instructions of meat

: C-CALL ( -- )
   LBL {: lcall :}  LBL {: lcopy :}  LBL {: lscan :}  LBL {: lsbody :}
   LBL {: lnopro :}  LBL {: linl :}  LBL {: ldone :}
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
      8 $FC000000 LIT64,  10 9 8 AND,  8 $14000000 LIT64,  10 8 CMP,  C-EQ lcall BCOND, \ B
      8 $FF000010 LIT64,  10 9 8 AND,  8 $54000000 LIT64,  10 8 CMP,  C-EQ lcall BCOND,  \ B.cond
      8 $7E000000 LIT64,  10 9 8 AND,  8 $34000000 LIT64,  10 8 CMP,  C-EQ lcall BCOND,  \ CBZ/CBNZ
      8 $7E000000 LIT64,  10 9 8 AND,  8 $36000000 LIT64,  10 8 CMP,  C-EQ lcall BCOND,  \ TBZ/TBNZ
      8 $FFFFFC1F LIT64,  10 9 8 AND,
         8 $D63F0000 LIT64,  10 8 CMP,  C-EQ lcall BCOND,                                \ BLR
         8 $D61F0000 LIT64,  10 8 CMP,  C-EQ lcall BCOND,                                \ BR
      8 $D65F03C0 LIT64,  9 8 CMP,  C-EQ lcall BCOND,                                   \ RET
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
variable LTRAPH   variable LBPH
variable LSRCRD   variable LSHBANG
variable LPLINUXTARGET  variable LPMACOSTARGET
variable LPLINUXLAYOUT  variable LPMACOSLAYOUT
variable LPUTIL         variable LPSTRUCTURES   variable LPBYTES        variable LPCHECKER      variable LPRENDER
variable LPLOWERCERTBASE
variable LPTYPESCHEMA   variable LPTYPEFAM      variable LPSUMTYPE      variable LPLAYOUTBUF  variable LPLAYOUTVALID
variable LPHOOK         variable LPSTRUCTEFF    variable LPHABULAYOUT   variable LPENVBASE      variable LPINCLUDE
variable LPSCRIPTARGV   variable LPROLES
variable LPENUMS        variable LPEXECVECTOR   variable LPSHA256       variable LPTFAMSHA
variable LPCOMBINATORS  variable LPXREF  variable LPLAYOUTSEAL
create BPH-KW 104 c, 97 c, 98 c, 117 c, 45 c, 98 c, 112 c, 58 c, 10 c,   \ habu-bp:\n
create ZBYTE 0 c,

: ZBYTES, ( ptr u8 n -- )
   BYTES, ZBYTE 1 BYTES, ;

: C-TRAP-MCTX>R9 ( -- )
   HB-TARGET-LINUX? IF
      9 2 LINUX-UC-MCTX-OFF ADDI,
   ELSE
      9 4 MCTX-OFF LDR,
   THEN ;

: C-MCTX-PC>R10 ( -- )
   HB-TARGET-LINUX? IF
      10 9 LINUX-MCTX-PC-OFF LDR,
   ELSE
      10 9 MACOS-MCTX-PC-OFF LDR,
   THEN ;

: C-MCTX-X19>R12 ( -- )
   HB-TARGET-LINUX? IF
      12 9 LINUX-MCTX-X19-OFF LDR,
   ELSE
      12 9 MACOS-MCTX-X19-OFF LDR,
   THEN ;

: C-MCTX-SP-16! ( -- )
   HB-TARGET-LINUX? IF
      12 9 LINUX-MCTX-SP-OFF LDR,  12 12 16 SUBI,  12 9 LINUX-MCTX-SP-OFF STR,
   ELSE
      12 9 MACOS-MCTX-SP-OFF LDR,  12 12 16 SUBI,  12 9 MACOS-MCTX-SP-OFF STR,
   THEN ;

: C-MCTX-PC+4! ( -- )
   HB-TARGET-LINUX? IF
      12 9 LINUX-MCTX-PC-OFF LDR,  12 12 4 ADDI,  12 9 LINUX-MCTX-PC-OFF STR,
   ELSE
      12 9 MACOS-MCTX-PC-OFF LDR,  12 12 4 ADDI,  12 9 MACOS-MCTX-PC-OFF STR,
   THEN ;

\ LTRAPH: target signal entry. A one-shot
\ breakpoint at [BPA-CELL]: print habu-bp: + pc + the data-stack top, restore
\ the original instruction, clear the bp, sigreturn to re-execute the word.
\ Any other trap falls through to the crash dump (x2/x4 untouched).
: EMIT-TRAPH ( -- )
   LTRAPH @ LBL,
   LBL {: tno :}
   C-TRAP-MCTX>R9                                    \ x9 = mcontext
   C-MCTX-PC>R10                                     \ x10 = pc
   LBL {: bscan :}  LBL {: bnext :}  LBL {: bhit :}
   LBL {: emu :}  LBL {: fin :}
   6 8 MOVZ,  7 0 MOVZ,                              \ MAXBP=8, i  (scan BPTAB[0..8))
   bscan LBL,
      7 6 CMP,  C-GE tno BCOND,
      8 7 5 LSLI,  14 BPTAB-OFF LIT64,  8 8 14 ADD,  8 DATA 8 ADD,   \ &BPTAB[i] (32 B stride)
      13 8 0 LDR,  13 bnext CBZ,                     \ empty slot (addr 0)
      10 13 CMP,  C-EQ bhit BCOND,
      bnext LBL,  7 7 1 ADDI,  bscan B,
   \ slot layout: +0 addr  +8 saved-instr  +16 hits  +24 ctrl(skip<<1 | persist)
   bhit LBL,                                         \ x8=&slot x9=mctx x10=pc
   SP SP 48 SUBI,
   1 SP 0 STR,  4 SP 8 STR,  5 SP 16 STR,  9 SP 24 STR,  10 SP 32 STR,  8 SP 40 STR,
   14 8 16 LDR,  14 14 1 ADDI,  14 8 16 STR,         \ hits++
   15 8 24 LDR,  12 15 1 LSRI,                       \ x15=ctrl  x12=skip
   14 12 CMP,  C-LS emu BCOND,                       \ hits <= skip -> silent, just emulate
   1 LBPH @ ADR,  0 2 MOVZ,  2 9 MOVZ,  NR-WRITE SYS,   \ "habu-bp:"
   9 SP 32 LDR,  LHEX @ BL,                          \ pc
   9 SP 24 LDR,  C-MCTX-X19>R12  9 12 8 SUBI,  9 9 0 LDR,  LHEX @ BL,   \ [x19-8] = tos
   8 SP 40 LDR,  15 8 24 LDR,  15 15 1 ANDI,  15 emu CBNZ,   \ persistent -> emulate, keep BRK
   2 3 MOVZ,  LPROT @ BL,                            \ one-shot: restore + remove
   8 SP 40 LDR,  11 8 0 LDR,  12 8 8 LDR,  12 11 0 STRW,
   2 5 MOVZ,  LPROT @ BL,
   9 11 0 ADDI,  LFLUSH @ BL,
   8 SP 40 LDR,  12 0 MOVZ,  12 8 0 STR,             \ clear slot addr (resume re-runs orig)
   fin B,
   emu LBL,                                          \ emulate the entry prologue, keep BRK:
   9 SP 24 LDR,                                      \ mctx
   C-MCTX-SP-16!                                     \ sp -= 16  (sub sp,sp,#16)
   C-MCTX-PC+4!                                      \ pc += 4   (skip the BRK)
   fin LBL,
   0 SP 8 LDR,  1 SP 0 LDR,  2 SP 16 LDR,  SP SP 48 ADDI,
   NR-SIGRETURN SYS,                                 \ sigreturn(uctx, infostyle, token)
   tno LBL,
   LCRASHH @ B,
   LBPH @ LBL,  BPH-KW 9 BYTES, ;

\ override SIGTRAP(5) to the resuming handler (G-INSTALL-CRASH pointed all four
\ at the dumper; this repoints just TRAP once LTRAPH is bound).
: G-INSTALL-TRAP ( -- )
   9 LTRAPH @ ADR,  9 C-SIGACTION-FRAME
   5 INSTALL-SIGACT
   C-SIGACTION-FRAME-DONE ;

: EMIT-SHEBANG-COMMENT ( -- )
   LSHBANG @ LBL,
   LBL {: done :}
   4 9 17 SUB,  4 2 CMPI,  C-LT done BCOND,
   4 17 0 LDRB,  4 $23 CMPI,  C-NE done BCOND,
   4 17 1 LDRB,  4 $21 CMPI,  C-NE done BCOND,
   4 92 MOVZ,  4 17 0 STRB,
   4 32 MOVZ,  4 17 1 STRB,
   done LBL,
   RET, ;

: EMIT-SOURCE-READ ( -- )
   LSRCRD @ LBL,
   LBL LBL LBL LBL LBL {: srl sdone sreaderr sopenerr sbufull :}   \ typed-local-lint: allow-bare-local - gforth host emitter; {: :} has no :type
   12 OS-OPEN-RD
   13 C-CS CSET,  13 sopenerr CBNZ,
   12 0 0 ADDI,
   17 9 0 ADDI,
   srl LBL,
      0 12 0 ADDI,  1 9 0 ADDI,
      2 11 0 ADDI,  5 IBUFSZ LIT64,  2 2 5 ADD,  2 2 9 SUB,
      2 sbufull CBZ,                              \ no room left: IBUFSZ overflow, not a read fault
      NR-READ SYS,
      13 C-CS CSET,  13 sreaderr CBNZ,
      0 sdone CBZ,
      9 9 0 ADD,  srl B,
   sdone LBL,
   0 12 0 ADDI,  NR-CLOSE SYS,
   SP SP 16 SUBI,  30 SP 0 STR,
   LSHBANG @ BL,
   30 SP 0 LDR,  SP SP 16 ADDI,
   RET,
   sreaderr LBL,  0 12 0 ADDI,  NR-CLOSE SYS,     \ read() fault: label fd 2 before exit 74
   s" hb: cannot read source" 74 C-EXIT-DIAG
   sbufull LBL,  0 12 0 ADDI,  NR-CLOSE SYS,      \ source outgrew IBUFSZ mid-read: name the buffer
   s" hb: source prefix buffer full" 74 C-EXIT-DIAG
   sopenerr LBL,                                  \ open error: label fd 2 before exit 74 (no per-path name in the tripwire)
   s" hb: cannot open source" 74 C-EXIT-DIAG ;

: C-TARGET-UNKNOWN ( -- )
   1 abort" hb: unknown target" ;

0 constant PFX-COMMON
1 constant PFX-LINUX
2 constant PFX-MACOS

: PFX-TARGET-OK ( -- )
   HB-TARGET-LINUX? if exit then
   HB-TARGET-MACOS? if exit then
   C-TARGET-UNKNOWN ;

: PFX-LOAD? ( n -- bool )
   dup PFX-COMMON = if drop 0 0= exit then
   dup PFX-LINUX = if drop HB-TARGET-LINUX? exit then
   PFX-MACOS = if HB-TARGET-MACOS? else 0 0= 0= then ;

: PFX-LOAD-ROW ( n ptr n ptr u8 n -- ) {: kind var a u :}
   kind PFX-LOAD? if 12 var @ ADR,  LSRCRD @ BL, then ;

: PFX-PATH-ROW ( n ptr n ptr u8 n -- ) {: kind var a u :}
   var @ LBL,  a u ZBYTES, ;

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
   PFX-COMMON LPLAYOUTSEAL   s" src/core/layout-buffer-seal.f" PFX-LOAD-ROW ;

: PFX-LOAD-SCRIPT-ARGV ( -- )
   PFX-COMMON LPSCRIPTARGV   s" src/os/script-argv.f"   PFX-LOAD-ROW ;

: PFX-LOAD-SCRIPT-ARGV-COLD ( -- )
   LBL {: done :}
   12 DATA SNAP-CELL LDR,
   12 done CBNZ,
   PFX-LOAD-SCRIPT-ARGV
   done LBL, ;

: PFX-LOAD-FILES ( -- )
   PFX-LOAD-BASE-FILES
   PFX-LOAD-SCRIPT-ARGV ;

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
   PFX-COMMON LPENUMS        s" src/core/enums.f"       PFX-PATH-ROW
   PFX-COMMON LPEXECVECTOR   s" src/core/exec-vector.f" PFX-PATH-ROW
   PFX-COMMON LPSHA256       s" src/core/sha256.f"      PFX-PATH-ROW
   PFX-COMMON LPTFAMSHA      s" src/core/type-family-sha.f" PFX-PATH-ROW
   PFX-COMMON LPCOMBINATORS  s" src/core/combinators.f" PFX-PATH-ROW
   PFX-COMMON LPXREF         s" src/habu/xref.f"        PFX-PATH-ROW
   PFX-COMMON LPLAYOUTSEAL   s" src/core/layout-buffer-seal.f" PFX-PATH-ROW ;

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

: C-EMIT-TTY-PROBE ( -- )
   0 0 MOVZ,
   HB-TARGET-LINUX? if 1 $5401 LIT64, else
      HB-TARGET-MACOS? if 1 $40487413 LIT64, else C-TARGET-UNKNOWN then
   then
   2 DATA BODYBUF-OFF ADDI,
   NR-IOCTL SYS, ;

variable SRC-TTY  variable SRC-FILE  variable SRC-SFAIL
variable SRC-RL   variable SRC-RD    variable SRC-PIPEOK
variable SRC-REPL variable SRC-DONE  variable SRC-FSCAN
variable SRC-FNEXT variable SRC-FREADY variable SRC-FPLAIN
variable SRC-FLOOP variable SRC-SHLOOP variable SRC-STDINPROG
variable SRC-BLOOP variable SRC-BDONE  variable SRC-BFAIL

: C-SOURCE-LABELS ( -- )
   LBL SRC-TTY !   LBL SRC-FILE !  LBL SRC-SFAIL !
   LBL SRC-RL !    LBL SRC-RD !    LBL SRC-PIPEOK !
   LBL SRC-REPL !  LBL SRC-DONE !  LBL SRC-FSCAN !
   LBL SRC-FNEXT ! LBL SRC-FREADY ! LBL SRC-FPLAIN !
   LBL SRC-FLOOP ! LBL SRC-SHLOOP ! LBL SRC-STDINPROG !
   LBL SRC-BLOOP ! LBL SRC-BDONE ! LBL SRC-BFAIL ! ;

: C-SOURCE-MMAP ( fail -- )
   >r
   0 0 MOVZ,  1 IBUFSZ LIT64,  2 3 MOVZ,
   3 MAP-ANON-PRIVATE LIT64,  4 0 MOVN,  5 0 MOVZ,
   NR-MMAP SYS,
   13 C-CS CSET,  13 r> CBNZ, ;

: C-ARG--LOAD? ( notload -- )
   >r
   4 12 0 LDRB,  4 $2D CMPI,  C-NE r@ BCOND,
   4 12 1 LDRB,  4 $2D CMPI,  C-NE r@ BCOND,
   4 12 2 LDRB,  4 108 CMPI,  C-NE r@ BCOND,
   4 12 3 LDRB,  4 111 CMPI,  C-NE r@ BCOND,
   4 12 4 LDRB,  4 97 CMPI,   C-NE r@ BCOND,
   4 12 5 LDRB,  4 100 CMPI,  C-NE r@ BCOND,
   4 12 6 LDRB,  4 0 CMPI,    C-NE r> BCOND, ;

: C-ARG-SEP? ( notsep -- )
   >r
   4 12 0 LDRB,  4 $2D CMPI,  C-NE r@ BCOND,
   4 12 1 LDRB,  4 $2D CMPI,  C-NE r@ BCOND,
   4 12 2 LDRB,  4 0 CMPI,    C-NE r> BCOND, ;

: C-SOURCE-SKIP-SHEBANG ( -- )
   12 9 11 SUB,  12 2 CMPI,  C-LT SRC-DONE @ BCOND,
   4 11 0 LDRB,  4 $23 CMPI,  C-NE SRC-DONE @ BCOND,
   4 11 1 LDRB,  4 $21 CMPI,  C-NE SRC-DONE @ BCOND,
   11 11 2 ADDI,
   SRC-SHLOOP @ LBL,
      11 9 CMP,  C-GE SRC-DONE @ BCOND,
      4 11 0 LDRB,  11 11 1 ADDI,
      11 DATA INP-CELL STR,
      4 10 CMPI,  C-EQ SRC-DONE @ BCOND,
      SRC-SHLOOP @ B, ;

: C-SOURCE-FIND-SEP ( -- )
   SRC-FSCAN @ LBL,
      13 10 CMP,  C-GE SRC-FREADY @ BCOND,
      12 DATA ARGV-CELL LDR,  5 13 3 LSLI,  12 12 5 ADD,  12 12 0 LDR,
      SRC-FNEXT @ C-ARG-SEP?
      15 13 0 ADDI,  SRC-FREADY @ B,
   SRC-FNEXT @ LBL,  13 13 1 ADDI,  SRC-FSCAN @ B, ;

: C-SOURCE-ARGV1 ( -- )
   12 DATA ARGV-CELL LDR,  12 12 8 LDR, ;

: C-SOURCE-FILE-MAP ( -- )
   SRC-SFAIL @ C-SOURCE-MMAP
   11 0 0 ADDI, ;

: C-SOURCE-APPEND-X4-TO ( n -- )
   \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
   {: fail :}
   2 11 0 ADDI,
   5 IBUFSZ LIT64,
   2 2 5 ADD,
   9 2 CMP,
   C-GE fail BCOND,
   4 9 0 STRB,
   9 9 1 ADDI, ;

: C-SOURCE-APPEND-X4 ( -- )
   SRC-SFAIL @ C-SOURCE-APPEND-X4-TO ;

: C-SOURCE-APPEND-CHAR ( n -- )
   \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
   {: c :}
   4 c MOVZ,
   C-SOURCE-APPEND-X4 ;

: C-SOURCE-APPEND-CHAR-TO ( n n -- )
   \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
   {: c fail :}
   4 c MOVZ,
   fail C-SOURCE-APPEND-X4-TO ;

: C-SOURCE-APPEND-Z12 ( -- )
   \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
   LBL LBL {: loop done :}
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

\ TFAM 2b-iii: append `SEAL-CAPTURE` as the LAST engine-prefix source token
\ (mirrors src/habu/habu2.f EMIT-SEAL-CAPTURE-TOKEN): xref.f's in-file call is
\ only the baseline - script-argv.f loads after it, so the truncation watermark
\ must re-freeze at the true engine-prefix end. Source token, not a native
\ store: nothing has evaluated at cold-prefix assembly time. Cold boots only.
: EMIT-SEAL-CAPTURE-TOKEN ( -- )
   LBL {: done :} \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
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

: EMIT-SEAL-FRIEND-TOKEN ( n -- )
   {: fail :} \ typed-local-lint: allow-bare-local
   BUILD-SOURCE? @ if exit then
   LBL {: done :} \ typed-local-lint: allow-bare-local
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

: PFX-PROVIDE-ROW ( n ptr n ptr u8 n -- )
   {: kind var a u :} \ typed-local-lint: allow-bare-local
   kind PFX-LOAD? if
      12 var @ ADR,
      C-SOURCE-APPEND-PROVIDED
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
   PFX-COMMON LPENUMS        s" src/core/enums.f"       PFX-PROVIDE-ROW
   PFX-COMMON LPEXECVECTOR   s" src/core/exec-vector.f" PFX-PROVIDE-ROW
   PFX-COMMON LPSHA256       s" src/core/sha256.f"      PFX-PROVIDE-ROW
   PFX-COMMON LPTFAMSHA      s" src/core/type-family-sha.f" PFX-PROVIDE-ROW
   PFX-COMMON LPCOMBINATORS  s" src/core/combinators.f" PFX-PROVIDE-ROW
   PFX-COMMON LPXREF         s" src/habu/xref.f"        PFX-PROVIDE-ROW
   PFX-COMMON LPLAYOUTSEAL   s" src/core/layout-buffer-seal.f" PFX-PROVIDE-ROW
   EMIT-SEAL-CAPTURE-TOKEN               \ watermark token at the true engine-prefix end
   SRC-SFAIL @ EMIT-SEAL-FRIEND-TOKEN ;  \ seal before user source (all stdin/file/repl paths)

: C-SOURCE-PIPE ( -- )
   SRC-STDINPROG @ LBL,
   SRC-SFAIL @ C-SOURCE-MMAP
   11 0 0 ADDI,  9 0 0 ADDI,
   EMIT-COLD-PREFIX
   PFX-LOAD-SCRIPT-ARGV-COLD
   PFX-PROVIDE-FILES
   17 9 0 ADDI,
   SRC-RL @ LBL,
      0 0 MOVZ,  1 9 0 ADDI,
      2 11 0 ADDI,  5 IBUFSZ LIT64,  2 2 5 ADD,  2 2 9 SUB,
      2 SRC-SFAIL @ CBZ,
      NR-READ SYS,
      13 C-CS CSET,  13 SRC-SFAIL @ CBNZ,
      0 SRC-RD @ CBZ,
      9 9 0 ADD,  SRC-RL @ B,
   SRC-RD @ LBL,
   LSHBANG @ BL,
   9 17 CMP,  C-NE SRC-PIPEOK @ BCOND,
   10 DATA ARGC-CELL LDR,  10 1 CMPI,  C-GT SRC-FILE @ BCOND,
   SRC-PIPEOK @ LBL,
   11 DATA INP-CELL STR,  9 DATA INE-CELL STR,
   C-SOURCE-SKIP-SHEBANG ;

: C-SOURCE-FILE-INIT ( -- )
   9 11 0 ADDI,
   10 DATA ARGC-CELL LDR,
   14 1 MOVZ,  15 2 MOVZ,
   C-SOURCE-ARGV1 ;

: C-SOURCE-FILE-PREFIX ( -- )
   SRC-FPLAIN @ C-ARG--LOAD?
   14 2 MOVZ,  15 10 0 ADDI,  13 2 MOVZ,
   EMIT-COLD-PREFIX
   PFX-LOAD-SCRIPT-ARGV-COLD
   PFX-PROVIDE-FILES
   C-SOURCE-FIND-SEP
   SRC-FPLAIN @ LBL,
   EMIT-COLD-PREFIX
   PFX-LOAD-SCRIPT-ARGV-COLD
   PFX-PROVIDE-FILES
   SRC-FREADY @ LBL, ;

: C-SOURCE-ARGV14 ( -- )
   12 DATA ARGV-CELL LDR,  5 14 3 LSLI,
   12 12 5 ADD,  12 12 0 LDR, ;

: C-SOURCE-APPEND-ARG ( -- )
   C-SOURCE-ARGV14
   C-SOURCE-APPEND-PROVIDED
   C-SOURCE-ARGV14
   LSRCRD @ BL,
   14 14 1 ADDI, ;

: C-SOURCE-APPEND-LF ( -- )
   2 11 0 ADDI,  5 IBUFSZ LIT64,  2 2 5 ADD,
   9 2 CMP,  C-GE SRC-SFAIL @ BCOND,
   5 10 MOVZ,  5 9 0 STRB,  9 9 1 ADDI, ;

: C-SOURCE-FILE-LOOP ( -- )
   SRC-FLOOP @ LBL,
      14 15 CMP,  C-GE SRC-PIPEOK @ BCOND,
      C-SOURCE-APPEND-ARG
      14 15 CMP,  C-GE SRC-PIPEOK @ BCOND,
      C-SOURCE-APPEND-LF
      SRC-FLOOP @ B, ;

: C-SOURCE-APPEND-LSRC ( -- )
   LBL LBL {: lsloop lsdone :} \ typed-local-lint: allow-bare-local
   12 LSRC @ ADR,  5 SRCN @ LIT64,  13 12 5 ADD,
   lsloop LBL,
      12 13 CMP,  C-GE lsdone BCOND,
      2 11 0 ADDI,  5 IBUFSZ LIT64,  2 2 5 ADD,  9 2 CMP,  C-GE SRC-SFAIL @ BCOND,
      4 12 0 LDRB,  4 9 0 STRB,
      12 12 1 ADDI,  9 9 1 ADDI,
      lsloop B,
   lsdone LBL, ;

: C-SOURCE-FAIL-REPL-DONE ( -- )
   SRC-SFAIL @ LBL,  s" hb: source prefix buffer full" 74 C-EXIT-DIAG   \ IBUFSZ source-prefix overflow
   SRC-REPL @ LBL,
   SRC-SFAIL @ C-SOURCE-MMAP
   11 0 0 ADDI,  9 11 0 ADDI,
   EMIT-COLD-PREFIX
   PFX-LOAD-SCRIPT-ARGV-COLD
   PFX-PROVIDE-FILES
   C-SOURCE-APPEND-LSRC
   11 DATA INP-CELL STR,  9 DATA INE-CELL STR,
   SRC-DONE @ B,
   SRC-DONE @ LBL, ;

: C-SOURCE-FILE-LIST ( -- )
   9 DATA ARGC-CELL LDR,  9 1 CMPI,  C-LE SRC-REPL @ BCOND,
   C-SOURCE-FILE-MAP
   SRC-FILE @ LBL,
   C-SOURCE-FILE-INIT
   C-SOURCE-FILE-PREFIX
   14 15 CMP,  C-GE SRC-SFAIL @ BCOND,
   C-SOURCE-FILE-LOOP
   C-SOURCE-FAIL-REPL-DONE ;

: C-SOURCE-STDIN ( -- )
   C-EMIT-TTY-PROBE
   0 SRC-TTY @ CBZ,
   10 DATA ARGC-CELL LDR,  10 1 CMPI,  C-LE SRC-STDINPROG @ BCOND,
   C-SOURCE-ARGV1
   SRC-STDINPROG @ C-ARG--LOAD?
   SRC-TTY @ B,
   C-SOURCE-PIPE
   SRC-TTY @ LBL,
   C-SOURCE-FILE-LIST ;

: C-SOURCE-BAKED ( -- )
   SRC-BFAIL @ C-SOURCE-MMAP
   11 0 0 ADDI,  9 0 0 ADDI,
   EMIT-COLD-PREFIX
   SRC-BFAIL @ EMIT-SEAL-FRIEND-TOKEN     \ seal after the prefix, before baked user source
   17 9 0 ADDI,
   12 LSRC @ ADR,  5 SRCN @ LIT64,  13 12 5 ADD,
   SRC-BLOOP @ LBL,
      12 13 CMP,  C-GE SRC-BDONE @ BCOND,
      2 11 0 ADDI,  5 IBUFSZ LIT64,  2 2 5 ADD,  9 2 CMP,  C-GE SRC-BFAIL @ BCOND,
      4 12 0 LDRB,  4 9 0 STRB,
      12 12 1 ADDI,  9 9 1 ADDI,
      SRC-BLOOP @ B,
   SRC-BDONE @ LBL,
   LSHBANG @ BL,
   11 DATA INP-CELL STR,  9 DATA INE-CELL STR,  SRC-DONE @ B,
   SRC-BFAIL @ LBL,  s" hb: source prefix buffer full" 74 C-EXIT-DIAG   \ IBUFSZ baked-prefix overflow
   SRC-DONE @ LBL, ;

: EMIT-SOURCE ( -- )
   C-SOURCE-LABELS
   STDIN? @ if C-SOURCE-STDIN else C-SOURCE-BAKED then ;

\ ---- control-flow JIT: a CF stack (region+CFSTK-OFF) of placeholder branch
\ addresses and local-frame snapshots. THEN/ELSE/REPEAT patch branches. ----
\ Lcfpush(x9=val), Lcfpop(->x9), Lpat(x9=addr: patch CBZ/B to current CP),
\ Lkwcmp(x0=kwaddr x1=kwlen -> x0=match? vs TKA/TKL, case-folded).
: C-EMIT-DROP-X12 ( -- )
   LBL {: done :} \ typed-local-lint: allow-bare-local
   12 done CBZ,
      9 $910003FF LIT64,  14 12 10 LSLI,  9 9 14 ORR,  LCEMIT @ BL,
   done LBL, ;

: EMIT-CF-HELPERS ( -- )
   LCFPUSH @ LBL,
      5 CFSTK-OFF LIT64,  10 DBASE 5 ADD,  11 10 0 LDR,
      12 CF-REC MOVZ,  12 11 12 MUL,  12 12 10 ADD,  12 12 8 ADDI,
      9 12 0 STR,
      13 DATA LOCN-CELL LDR,  13 12 CF-LOCN STR,
      13 DATA LOCF-CELL LDR,  13 12 CF-LOCF STR,
      11 11 1 ADDI,  11 10 0 STR,  RET,
   LCFPOP @ LBL,
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
   LPAT @ LBL,                                       \ patch imm19 (CBZ) / imm26 (B)
      11 9 0 LDRW,  10 CP 9 SUB,  10 10 2 ASRI,
      5 $80000000 LIT64,  13 11 5 AND,
      LBL {: pisb :}  LBL {: pdone :}
      13 pisb CBZ,                                   \ bit31==0 -> B (imm26)
         5 $7FFFF LIT64,  10 10 5 AND,  10 10 5 LSLI,  pdone B,
      pisb LBL,  5 $3FFFFFF LIT64,  10 10 5 AND,
      pdone LBL,  11 11 10 ORR,  11 9 0 STRW,  RET,
   LKWCMP @ LBL,
      LBL {: kno :}  LBL {: kyes :}  LBL {: kchk :}
      2 DATA TKL-CELL LDR,  2 1 CMP,  C-NE kno BCOND,
      2 0 MOVZ,  3 $20 MOVZ,
      kchk LBL,
         2 1 CMP,  C-GE kyes BCOND,
         4 DATA TKA-CELL LDR,  4 4 2 ADD,  4 4 0 LDRB,                    \ token byte
         LBL {: knf :}                             \ fold ONLY A-Z (symbols stay literal)
         4 $41 CMPI,  C-LT knf BCOND,  4 $5A CMPI,  C-GT knf BCOND,  4 4 3 ORR,
         knf LBL,
         5 0 2 ADD,    5 5 0 LDRB,                    \ keyword byte (stored lower-case / literal)
         4 5 CMP,  C-NE kno BCOND,
         2 2 1 ADDI,  kchk B,
      kyes LBL,  0 1 MOVZ,  RET,
      kno  LBL,  0 0 MOVZ,  RET,
   LBCHAIN @ LBL,                                    \ patch a B-placeholder chain:
      LBL {: bcl :}  LBL {: bcd :}             \ x9=head offset, x14=target;
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
   9 DATA LOCN-CELL LDR,  10 0 MOVZ,
   6 DATA TKL-CELL LDR,  7 DATA TKA-CELL LDR,                 \ x9=N  x10=i
   LBL {: ll :}  LBL {: lmiss :}  LBL {: lhit :}
   LBL {: lcmp :}  LBL {: lnext :}
   ll LBL,  10 9 CMP,  C-GE lmiss BCOND,
      12 LOC-REC MOVZ,  11 10 12 MUL,  5 LOCNAMES LIT64,  11 11 5 ADD,  11 DATA 11 ADD,   \ entry
      12 11 0 LDR,  12 6 CMP,  C-NE lnext BCOND,   \ len mismatch
      13 0 MOVZ,                                     \ j
      lcmp LBL,  13 6 CMP,  C-GE lhit BCOND,
         14 11 13 ADD,  14 14 8 ADDI,  14 14 0 LDRB, \ entry.name[j]
         15 7 13 ADD,  15 15 0 LDRB,               \ tok[j]
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
   LKWCASE @ LBL,   s" case"   BYTES,    LKWOF @ LBL,     s" of"     BYTES,
   LKWENDOF @ LBL,  s" endof"  BYTES,    LKWENDCASE @ LBL, s" endcase" BYTES,
   LKWCREATE @ LBL, s" create" BYTES,    LKWVAR @ LBL,    s" variable" BYTES,
   LKWSQ @ LBL,     SQ-KW 2 BYTES,                         \ the 2 bytes  s "
   LKWCQ @ LBL,     CQ-KW 2 BYTES,
   LKWDOTQ @ LBL,   DOTQ-KW 2 BYTES,
   LKWESQ @ LBL,    ESQ-KW 3 BYTES,
   LKWECQ @ LBL,    ECQ-KW 3 BYTES,
   LKWEDOTQ @ LBL,  EDOTQ-KW 3 BYTES,
   LKWTYPE @ LBL,   s" type" BYTES,
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
   LKWTRUSTED @ LBL, s" trusted:" BYTES,
   LKWKERNEL @ LBL, s" kernel:" BYTES,
   LKWTRUST @ LBL, s" trust" BYTES,      LKWCHKDOES @ LBL, s" check-does!" BYTES,
   LKWPACKAGE @ LBL, s" package" BYTES,  LKWPUBLIC @ LBL, s" public" BYTES,
   LKWPRIVATE @ LBL, s" private" BYTES,  LKWSEMIPACKAGE @ LBL, s" ;package" BYTES,
   LCHKPACKAGE @ LBL, s" checker-package" BYTES,  LCHKPUB @ LBL, s" checker-public" BYTES,
   LCHKPRI @ LBL, s" checker-private" BYTES,  LCHKENDPKG @ LBL, s" checker-end-package" BYTES,
   LRESCHECKCERT @ LBL, s" checker-cert" BYTES,
   LRESLOWERCERT @ LBL, s" lower-cert" BYTES,
   LRESLOWERHOOK @ LBL, s" lower-cert-hook" BYTES,
   LKWQUOT @ LBL,  QUOT-KW 2 BYTES,   LKWSEMIQ @ LBL,  SEMIQ-KW 2 BYTES,
   LKWCONSTRUCT @ LBL, s" construct" BYTES,  LKWMATCH @ LBL, s" match" BYTES,  LKWSEMIMATCH @ LBL, s" ;match" BYTES,
   LTFLCONFAM @ LBL, s" tfl-con-fam?" BYTES,  LTFLCVAR @ LBL, s" tfl-cvar?" BYTES,
   LTFLMATCHFAM @ LBL, s" tfl-match-fam?" BYTES,  LTFLNAME @ LBL, s" tfam-name$" BYTES,
   LBADTAGPFX @ LBL, s" hb: bad " BYTES,  LBADTAGSFX @ LBL, BADTAG-SFX-KW 5 BYTES,
   PFX-PATH-FILES ;

\ compile-time handler emitters (run at BUILD time, append JIT-emitter ICode)
: C-EMITW ( n -- )  9 swap LIT64,  LCEMIT @ BL, ;          \ emit one fixed instr word

: C-POPFLAG ( -- )  $D1002273 C-EMITW  $F9400269 C-EMITW ;     \ sub x19,#8 ; ldr x9,[x19]

: C-POP-X16 ( -- )  $D1002273 C-EMITW  $F9400270 C-EMITW ;

: C-PUSHCP ( -- )   9 CP 0 ADDI,  LCFPUSH @ BL, ;              \ push current CP

: C-BBACK ( n n -- ) {: opc mask -- :}                                    \ branch opc back to x9 target
   10 9 CP SUB,  10 10 2 ASRI,  5 mask LIT64,  10 10 5 AND,  9 opc LIT64,  9 9 10 ORR,  LCEMIT @ BL, ;

: J-IF ( -- )    C-POPFLAG  C-PUSHCP  $B4000009 C-EMITW ;             \ pop flag; cbz fwd (patched by THEN)

: J-THEN ( -- )  LCFPOP @ BL,  LPAT @ BL, ;

: J-ELSE ( -- )
   LCFPOP @ BL,
   14 9 0 ADDI,
   C-PUSHCP
   $14000000 C-EMITW
   9 14 0 ADDI,
   LPAT @ BL, ;

: J-CASE ( -- )
   9 0 MOVZ,  LCFPUSH @ BL, ;

\ construct keyword (TFAM 10 slice 2; mirrors habu2.f J-CONSTRUCT): arm the
\ operand capture (CMM=1). CFN entry — construct only ADDS VS constants.
: J-CONSTRUCT ( -- )
   12 1 MOVZ,  12 DATA CMM-CELL STR, ;

\ match keyword (TFAM 10 slice 3; mirrors habu2.f J-MATCH): CF-ENTRY spills the
\ width-expanded bundle to the physical stack, then push a J-CASE-shape CF
\ sentinel, open a match-frame level, arm CMM=3. No emission.
: J-MATCH ( -- )
   9 0 MOVZ,  LCFPUSH @ BL,
   9 DATA CMFRD-CELL LDR,  9 9 1 ADDI,  9 DATA CMFRD-CELL STR,
   12 3 MOVZ,  12 DATA CMM-CELL STR, ;

: J-OF ( -- )
   14 DATA CMBK-CELL LDR,  14 14 1 LSLI,  14 DATA CMBK-CELL STR,   \ push case-arm marker (0)
   C-POP-X16
   $F85F8269 C-EMITW
   $EB10013F C-EMITW
   $9A9F17E9 C-EMITW
   C-PUSHCP
   $B4000009 C-EMITW
   $D1002273 C-EMITW ;

: J-ENDOF ( -- )
   LBL {: nm :} \ typed-local-lint: allow-bare-local
   J-ELSE
   14 DATA CMBK-CELL LDR,  15 14 1 ANDI,  14 14 1 LSRI,  14 DATA CMBK-CELL STR,
   15 nm CBZ,  12 4 MOVZ,  12 DATA CMM-CELL STR,  nm LBL, ;

: J-ENDCASE ( -- )
   LBL LBL {: cloop done :} \ typed-local-lint: allow-bare-local
   $D1002273 C-EMITW
   cloop LBL,
      LCFPOP @ BL,
      9 done CBZ,
      LPAT @ BL,
      cloop B,
   done LBL, ;

\ BEGIN loops are register-resident: J-BEGIN snapshots the VS into registers
\ (Lvsnap), the back edges reconcile to that snapshot (Lvrecon) and branch on
\ x17 — never a VS register, so the reconcile reload can't clobber the flag.
: J-BEGIN ( -- )  LVSNAP @ BL,  C-PUSHCP ;

: J-AGAIN ( -- )  LVRECON @ BL,  LCFPOP @ BL,  $14000000 $3FFFFFF C-BBACK ;

: J-UNTILX ( -- )                          \ shared tail: reconcile + cbz x17,top
   LVRECON @ BL,
   LCFPOP @ BL,
   10 9 CP SUB,  10 10 2 ASRI,  5 $7FFFF LIT64,  10 10 5 AND,  10 10 5 LSLI,
   9 $B4000011 LIT64,  9 9 10 ORR,  LCEMIT @ BL, ;

: J-UNTIL ( -- )  $D1002273 C-EMITW  $F9400271 C-EMITW  J-UNTILX ;   \ pop flag -> x17

: J-WHILE ( -- ) C-POPFLAG  C-PUSHCP  $B4000009 C-EMITW ;

: J-REPEAT ( -- ) LVRECON @ BL,  LCFPOP @ BL,
   SP SP 16 SUBI,  9 SP 0 STR,  14 SP 8 STR,
   LCFPOP @ BL,  $14000000 $3FFFFFF C-BBACK
   12 0 MOVZ,  12 DATA VSP-CELL STR,                  \ exit path arrives from
   12 VRALL MOVZ,  12 DATA VRFREE-CELL STR,
   12 FRALL MOVZ,  12 DATA FRFREE-CELL STR,           \ WHILE's spilled state
   9 SP 0 LDR,  LPAT @ BL,
   14 SP 8 LDR,  15 DATA LOCF-CELL LDR,  12 14 15 SUB,  C-EMIT-DROP-X12
   SP SP 16 ADDI, ;

\ DO/LOOP/I — loop index/limit live in a data-region frame stack ([x20+LOOP-STK-OFF],
\ depth [x20+LOOPSP-CELL]) since x27/x28 are the compiler's NDICT/CP. Fixed encodings
\ (computed offline). J-DO pushes a frame + records loop-top; J-LOOP increments the
\ index, compares, b.lt back, then pops the frame on exit; J-I pushes the index.
: J-FRAME ( -- )                       \ pop limit/start, push a loop frame
   3506446963 C-EMITW  4181721705 C-EMITW  3506446963 C-EMITW  4181721706 C-EMITW
   4181780107 C-EMITW  3548179820 C-EMITW  2434269580 C-EMITW  2333344140 C-EMITW
   4177527177 C-EMITW  4177528202 C-EMITW  2432697707 C-EMITW  4177585803 C-EMITW ;

: J-LVOPEN ( -- )                       \ open a LEAVE-chain level: LVH[LVD]=0, LVD++
   9 DATA LVD-CELL LDR,
   10 9 3 LSLI,  10 10 LVH-OFF ADDI,  10 DATA 10 ADD,
   12 0 MOVZ,  12 10 0 STR,
   10 9 3 LSLI,  10 10 LVF-OFF ADDI,  10 DATA 10 ADD,
   12 DATA LOCF-CELL LDR,  12 10 0 STR,
   9 9 1 ADDI,  9 DATA LVD-CELL STR, ;

: J-LVLEAVE ( -- )                      \ chain a B placeholder on the current level
   9 DATA LVD-CELL LDR,  9 9 1 SUBI,
   10 9 3 LSLI,  10 10 LVF-OFF ADDI,  10 DATA 10 ADD,
   14 10 0 LDR,  15 DATA LOCF-CELL LDR,  12 15 14 SUB,  C-EMIT-DROP-X12
   9 DATA LVD-CELL LDR,  9 9 1 SUBI,
   10 9 3 LSLI,  10 10 LVH-OFF ADDI,  10 DATA 10 ADD,
   9 10 0 LDR,
   11 CP DBASE SUB,  11 10 0 STR,
   LCEMIT @ BL, ;

: J-DO ( -- )
   J-FRAME  J-LVOPEN  C-PUSHCP ;

: J-?DO ( -- )              \ DO, but skip the loop when limit = start
   J-FRAME  J-LVOPEN
   $EB0A013F C-EMITW                     \ cmp x9,x10  (start/limit still live)
   $54000041 C-EMITW                     \ b.ne +8 (over the skip placeholder)
   J-LVLEAVE
   C-PUSHCP ;

: J-LEAVE ( -- )  J-LVLEAVE ;

: J-UNLOOP ( -- )                               \ pop one loop frame, no branch
   4181780107 C-EMITW  3506439531 C-EMITW  4177585803 C-EMITW ;

: J-LOOPEND ( -- )                      \ shared LOOP/+LOOP tail: pop frame, patch
   14 CP 0 ADDI,                         \ LEAVE/?DO skips to the pop point, LVD--
   4181780107 C-EMITW  3506439531 C-EMITW  4177585803 C-EMITW
   9 DATA LVD-CELL LDR,  9 9 1 SUBI,  9 DATA LVD-CELL STR,
   10 9 3 LSLI,  10 10 LVH-OFF ADDI,  10 DATA 10 ADD,  9 10 0 LDR,
   LBCHAIN @ BL, ;

: J-LOOP ( -- )
   4181780107 C-EMITW  3506439531 C-EMITW  3548179820 C-EMITW  2434269580 C-EMITW  2333344140 C-EMITW
   4181721481 C-EMITW  4181722506 C-EMITW  2432697641 C-EMITW  4177527177 C-EMITW  3943301439 C-EMITW
   LCFPOP @ BL,                                        \ x9 = loop-top
   10 9 CP SUB,  10 10 2 ASRI,  5 $7FFFF LIT64,  10 10 5 AND,  10 10 5 LSLI,
   9 $5400000B LIT64,  9 9 10 ORR,  LCEMIT @ BL,       \ b.lt loop-top
   J-LOOPEND ;

: J-+LOOP ( -- )                   \ index += n; loop while (old-limit) and
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

: J-I ( -- )
   4181780107 C-EMITW  3506439531 C-EMITW  3548179820 C-EMITW  2434269580 C-EMITW  2333344140 C-EMITW
   4181721481 C-EMITW  4177527401 C-EMITW  2432705139 C-EMITW ;

: J-J ( -- )                                    \ outer loop index: frame[LOOPSP-2]
   4181780107 C-EMITW  $D100096B C-EMITW 3548179820 C-EMITW  2434269580 C-EMITW  2333344140 C-EMITW
   4181721481 C-EMITW  4177527401 C-EMITW  2432705139 C-EMITW ;

\ >R R> R@ — the user return stack lives in a data-region stack ([x20+RSTK-OFF],
\ depth at [x20+RSP-CELL]), like the DO/LOOP frames: x25/x28 belong to the
\ compiler, and word frames on the machine stack would unbalance the epilogue.
: W-LDRX ( n n n -- n ) {: rt RN off -- w :}                          \ ldr rt,[rn,#off]
   $F9400000  off 8 / 10 lshift or  RN 5 lshift or  rt or ;

: W-STRX ( n n n -- n ) {: rt RN off -- w :}                          \ str rt,[rn,#off]
   $F9000000  off 8 / 10 lshift or  RN 5 lshift or  rt or ;

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
   LBL {: qexit :} \ typed-local-lint: allow-bare-local
   9 DATA QPATCH-CELL LDR,  9 qexit CBNZ,
      12 DATA LOCF-CELL LDR,  C-EMIT-DROP-X12
   qexit LBL,
   9 DATA EXITH-CELL LDR,                              \ x9 = prev chain offset
   10 CP DBASE SUB,  10 DATA EXITH-CELL STR,           \ head := this placeholder
   LCEMIT @ BL, ;

: J-RECURSE ( -- )
   9 DATA PEND-CELL LDR,  9 9 0 LDR,  $94000000 $3FFFFFF C-BBACK ;   \ bl entry

: C-FIND-TRUST ( -- )  LBL {: ok :}
   9 LKWTRUST @ ADR,  10 5 MOVZ,  LFIND @ BL,
   13 ok CBNZ,
      0 2 MOVZ,  1 LKWTRUST @ ADR,  2 5 MOVZ,  NR-WRITE SYS,
      0 70 MOVZ,  NR-EXIT-GROUP SYS,
   ok LBL, ;

: C-PUSH-DREC-NAME ( -- )
   LBL {: pinl :}
   9 12 24 ADDI,
   10 12 16 LDR,  10 10 DNAME-EXT ANDI,  10 pinl CBZ,
      9 12 24 LDR,
   pinl LBL,
   9 G-PUSH
   9 12 16 LDR,  9 9 12 LSLI,  9 9 12 LSRI,  9 G-PUSH ;

: C-PUSH-DATA-CELL ( n -- ) {: off :}
   9 DATA off LDR,  9 G-PUSH ;

: C-PUSH-TRUST-SIG ( n n -- ) {: aoff uoff :}
   aoff C-PUSH-DATA-CELL
   uoff C-PUSH-DATA-CELL ;

: C-CALL-X11-SAVED ( -- )
   SP SP 16 SUBI,  30 SP 0 STR,
   11 BLR,
   30 SP 0 LDR,  SP SP 16 ADDI, ;

: C-CALL-EMIT-MOVZ-X16 ( -- )
   5 $FFFF MOVZ,
   7 11 5 AND,  7 7 5 LSLI,
   8 C-CALL-MOVZ-X16 LIT64,  9 8 7 ORR,  LCEMIT @ BL, ;

: C-CALL-EMIT-MOVK-X16 ( n n -- ) {: sh op :} \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
   7 11 sh LSRI,  7 7 5 AND,  7 7 5 LSLI,
   8 op LIT64,  9 8 7 ORR,  LCEMIT @ BL, ;

: C-CALL-EMIT-ABSOLUTE ( -- )
   C-CALL-EMIT-MOVZ-X16
   16 C-CALL-MOVK-X16-16 C-CALL-EMIT-MOVK-X16
   32 C-CALL-MOVK-X16-32 C-CALL-EMIT-MOVK-X16
   9 C-CALL-BLR-X16 LIT64,  LCEMIT @ BL, ;

: C-CALL-TRUST-LASTC ( -- )
   LBL {: nohook :} \ typed-local-lint: allow-bare-local
   9 DATA HOOK-CELL LDR,  9 nohook CBZ,
   C-FIND-TRUST
   12 DATA LASTC-CELL LDR,
   C-PUSH-DREC-NAME
   CRSIG-A-CELL CRSIG-U-CELL C-PUSH-TRUST-SIG
   C-CALL-X11-SAVED
   nohook LBL, ;

: C-CALL-TRUST-PEND ( -- )
   C-FIND-TRUST
   12 DATA PEND-CELL LDR,
   C-PUSH-DREC-NAME
   TSIG-A-CELL TSIG-U-CELL C-PUSH-TRUST-SIG
   C-CALL-X11-SAVED ;

: C-DIE-DOES ( -- )
   0 2 MOVZ,  1 LKWDOES @ ADR,  2 5 MOVZ,  NR-WRITE SYS,
   0 70 MOVZ,  NR-EXIT-GROUP SYS, ;

: C-CALL-CHECK-DOES ( -- )
   LBL LBL {: found good :}
   9 LKWCHKDOES @ ADR,  10 11 MOVZ,  LFIND @ BL,
   13 found CBNZ,
      0 2 MOVZ,  1 LKWCHKDOES @ ADR,  2 11 MOVZ,  NR-WRITE SYS,
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

: C-SIG-START ( n -- ) {: lmiss :}
   LBL LBL {: ws got :}
   11 DATA INP-CELL LDR,  12 DATA INE-CELL LDR,
   ws LBL,  11 12 CMP,  C-GE lmiss BCOND,
      13 11 0 LDRB,  13 32 CMPI,  C-HI got BCOND,
      11 11 1 ADDI,  ws B,
   got LBL,  13 40 CMPI,  C-NE lmiss BCOND,
   14 11 0 ADDI,  15 11 0 ADDI, ;

: C-SIG-END ( n -- ) {: lmiss :}
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
   C-SIG-FULL$  LBCS @ BL, ;

: C-SIG-BAD ( -- )
   s" bootstrap: signature parse failed" C-EXIT76 ;

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

\ DOES> — the defining word patches its LAST create into `push dfield ; b D`,
\ then exits; D (the does-body) follows with its own prologue and shares `;`'s
\ epilogue. The patch itself runs in LDOESPATCH (ENGINE text): flipping the
\ region to RW would un-map EXECUTE from the page the defining word runs on.
\ Locals BEFORE does> are refused (the shared teardown wouldn't match).
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
   9 $D10043FF LIT64,  LCEMIT @ BL,      \ D: fresh prologue for the does-body
   9 $F90003FE LIT64,  LCEMIT @ BL, ;

\ [: ... ;] — an anonymous nested definition: [: jumps over the body, gives it
\ its own prologue; ;] closes it (epilogue + patch) and pushes its address as a
\ literal in the OUTER word (xt on the stack at outer runtime). One level; the
\ EXIT chain is scoped to the quotation; locals inside are refused.
: J-QUOT ( -- )
   LBL {: qok :}
   9 DATA QPATCH-CELL LDR,  9 qok CBZ,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 75 MOVZ,  NR-EXIT-GROUP SYS,
   qok LBL,
   9 CP 0 ADDI,  9 DATA QPATCH-CELL STR,
   9 $14000000 LIT64,  LCEMIT @ BL,               \ b-over placeholder
   9 CP 0 ADDI,  9 DATA QENT-CELL STR,            \ the quotation's entry
   9 DATA EXITH-CELL LDR,  9 DATA QXH-CELL STR,   \ scope the EXIT chain
   12 0 MOVZ,  12 DATA EXITH-CELL STR,
   9 $D10043FF LIT64,  LCEMIT @ BL,               \ its own prologue
   9 $F90003FE LIT64,  LCEMIT @ BL, ;

: J-SEMIQUOT ( -- )
   LBL {: sqok :}
   9 DATA QPATCH-CELL LDR,  9 sqok CBNZ,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 75 MOVZ,  NR-EXIT-GROUP SYS,
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
   LBL {: nocr :}
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
   9 DATA CRSIG-U-CELL LDR,  9 nocr CBZ,
      C-CALL-TRUST-LASTC
      C-RUNTIME-CRSIG-CLEAR
   nocr LBL,
   30 SP 0 LDR,  SP SP 32 ADDI,  RET, ;

\ CREATE/VARIABLE (interpret-mode defining words): make a dict word whose body
\ pushes the current DP (a data-space address). Reuses the `:` slot pattern + the
\ C-LIT emitter (with x11 = DP) for the literal-push body.
\ record defining words for the checker: append the kind token + run the hook
\ (verdict ignored — create/variable/constant always publish).
: C-DEFHOOK ( ptr u8 -- )  LBL {: kwv klen nohk :}
   11 kwv @ ADR,  12 klen MOVZ,  LBCS @ BL,
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
      s" bootstrap: definition name too long" C-EXIT76
   done LBL, ;

\ A qualified definer token names a real package record and stores only its
\ tail in that package's public WID. The original spelling remains available
\ to the checker/hook through DEF-TKA/DEF-TKL; no textual prefix dictionary
\ entries are created.
: C-QUALIFY-DEF ( -- )
   LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL
   {: qscan qnone qhas qtail qseal qsealed qlookup nloop nnext ncmp nmatch ninl nmake nroom qapply qbad done :} \ typed-local-lint: allow-bare-local
   11 DATA TKA-CELL LDR,  11 DATA DEF-TKA-CELL STR,
   12 DATA TKL-CELL LDR,  12 DATA DEF-TKL-CELL STR,
   14 DATA CUR-CELL LDR,  14 DATA DEF-WL-CELL STR,
   17 0 MOVZ,
   qscan LBL,
      17 12 CMP,  C-GE qnone BCOND,
      14 11 17 ADD,  14 14 0 LDRB,  14 $3A CMPI,  C-EQ qhas BCOND,
      17 17 1 ADDI,  qscan B,
   qnone LBL,
      done B,
   qhas LBL,
      17 qnone CBZ,
      14 17 1 ADDI,  14 12 CMP,  C-GE qnone BCOND,
      14 17 1 ADDI,
   qtail LBL,
      14 12 CMP,  C-GE qseal BCOND,
      15 11 14 ADD,  15 15 0 LDRB,  15 $3A CMPI,  C-EQ qbad BCOND,
      14 14 1 ADDI,  qtail B,
   qseal LBL,
      17 DATA TKL-CELL STR,
      9 DATA FRIEND-LATCH-CELL LDR,  9 qlookup CBZ,
      0 LRESCHECKCERT @ ADR,  1 12 MOVZ,  LKWCMP @ BL,  0 qsealed CBNZ,
      0 LRESLOWERCERT @ ADR,  1 10 MOVZ,  LKWCMP @ BL,  0 qsealed CBNZ,
      0 LRESLOWERHOOK @ ADR,  1 15 MOVZ,  LKWCMP @ BL,  0 qsealed CBNZ,
      qlookup B,
   qsealed LBL,
      0 2 MOVZ,  1 DATA DEF-TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 2 MOVZ,  1 LQNL @ ADR,  1 1 1 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
      0 E-SEAL-PACKAGE MOVZ,  NR-EXIT-GROUP SYS,
   qlookup LBL,
      5 DBASE 0 ADDI,  6 NDICT 0 ADDI,
   nloop LBL,
      6 nmake CBZ,
      14 5 40 LDR,  15 0 MOVN,  14 15 CMP,  C-NE nnext BCOND,
      14 5 16 LDR,  14 14 12 LSLI,  14 14 12 LSRI,  14 17 CMP,  C-NE nnext BCOND,
      16 5 24 ADDI,
      14 5 16 LDR,  14 14 DNAME-EXT ANDI,  14 ninl CBZ,
         16 5 24 LDR,
      ninl LBL,
      7 0 MOVZ,
      ncmp LBL,
         7 17 CMP,  C-GE nmatch BCOND,
         15 16 7 ADD,  15 15 0 LDRB,
         3 15 $41 SUBI,  3 26 CMPI,  3 C-CC CSET,  3 3 5 LSLI,  15 15 3 ORR,
         4 11 7 ADD,  4 4 0 LDRB,
         3 4 $41 SUBI,  3 26 CMPI,  3 C-CC CSET,  3 3 5 LSLI,  4 4 3 ORR,
         15 4 CMP,  C-NE nnext BCOND,
         7 7 1 ADDI,  ncmp B,
      nmatch LBL,
         14 5 0 LDR,  14 DATA DEF-WL-CELL STR,
         qapply B,
      nnext LBL,  5 5 DREC ADDI,  6 6 1 SUBI,  nloop B,
   nmake LBL,
      9 DICT-CAP MOVZ,  NDICT 9 CMP,  C-LT nroom BCOND,
         0 77 MOVZ,  NR-EXIT-GROUP SYS,
      nroom LBL,
      9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
      C-STORE-NAME
      14 DATA WIDN-CELL LDR,  14 DATA DEF-WL-CELL STR,
      15 14 1 ADDI,  15 DATA WIDN-CELL STR,
      14 9 0 STR,
      15 0 MOVZ,  15 9 8 STR,
      15 0 MOVN,  15 9 40 STR,
      NDICT NDICT 1 ADDI,
   qapply LBL,
      11 DATA DEF-TKA-CELL LDR,
      12 DATA DEF-TKL-CELL LDR,
      11 11 17 ADD,  11 11 1 ADDI,  11 DATA TKA-CELL STR,
      12 12 17 SUB,  12 12 1 SUBI,  12 DATA TKL-CELL STR,
      done B,
   qbad LBL,
      0 75 MOVZ,  NR-EXIT-GROUP SYS,
   done LBL, ;

: C-STORE-DEF-NAME ( -- )
   C-STORE-NAME
   14 DATA DEF-WL-CELL LDR,  14 9 40 STR,
   11 DATA DEF-TKA-CELL LDR,  11 DATA TKA-CELL STR,
   12 DATA DEF-TKL-CELL LDR,  12 DATA TKL-CELL STR, ;

\ CREATE as a BL-able routine: the interpret keyword AND the runtime `create`
\ prim share it, so defining words (`: CONST create , does> @ ;`) work.
\ LCREATE ( x15=top-level? ): the hook KIND record (`NAME create` -> sig -- n)
\ applies to top-level creates. Created-word effects are recorded by DOESPATCH
\ after DOES> has parsed the created-word effect.
: EMIT-CREATE ( -- )
   LBL {: nokind :}
   LCREATE @ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,  15 SP 8 STR,
   2 3 MOVZ,  LPROT @ BL,                               \ region -> RW
   LTOK @ BL,                                            \ read NAME
   12 0 MOVZ,  12 DATA BODYLEN-CELL STR,  LBCAP @ BL,   \ seed "NAME " for the hook
   C-QUALIFY-DEF
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,   \ slot
   C-STORE-DEF-NAME
   CP 9 0 STR,
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
   7 DATA 0 LDR,  7 7 8 ADDI,  7 DP-CHECK  7 DATA 0 STR, ;          \ reserve 1 cell

\ CONSTANT ( n -- ) "name": define a word that pushes n. Pop n first (x15
\ survives the name copy), then emit a literal-push body via C-LIT (x11=n).
: C-CONSTANT ( -- )
   2 3 MOVZ,  LPROT @ BL,  LTOK @ BL,
   12 0 MOVZ,  12 DATA BODYLEN-CELL STR,  LBCAP @ BL,   \ seed "NAME " for the hook
   C-QUALIFY-DEF
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   C-STORE-DEF-NAME
   15 G-POP                                             \ n -> x15 after name storage (clobbers x15)
   CP 9 0 STR,
   11 15 0 ADDI,  C-LIT                                 \ body: push n
   9 W-RET LIT64,  LCEMIT @ BL,
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   10 9 0 LDR,  10 CP 10 SUB,  10 10 4 SUBI,  10 9 8 STR,
   NDICT NDICT 1 ADDI,  9 9 0 LDR,                      \ x9 = body start for the flush
   2 5 MOVZ,  LPROT @ BL,  LFLUSH @ BL,
   LKWCONST 8 C-DEFHOOK ;

\ IMMEDIATE: mark the LAST defined word — the compile loop EXECUTES immediate
\ words instead of compiling calls (flag = DNAME-IMM in slot.name-len|flags).
: C-IMMEDIATE ( -- )
   2 3 MOVZ,  LPROT @ BL,                               \ dict lives in the RX region
   9 NDICT 0 ADDI,  9 9 1 SUBI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   10 9 16 LDR,  10 10 DNAME-IMM ORRI,  10 9 16 STR,
   2 5 MOVZ,  LPROT @ BL, ;

\ POSTPONE NAME (compile): immediate -> compile the call; ordinary -> bake the
\ xt and compile a call to the `compile,` prim (appends the call at ITS runtime).
: C-POSTPONE ( -- )
   LBL {: pok :}  LBL {: pnimm :}  LBL {: pdone :}
   LTOK @ BL,  9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFIND @ BL,
   13 pok CBNZ,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 70 MOVZ,  NR-EXIT-GROUP SYS,
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
: C-CHAR ( -- )   LTOK @ BL,  9 DATA TKA-CELL LDR,  9 9 0 LDRB,  9 G-PUSH ;

: C-BCHAR ( -- )   LTOK @ BL,  11 DATA TKA-CELL LDR,  11 11 0 LDRB,  LVPUSHC @ BL, ;

\ ' NAME (interpret): find NAME, push its code address. ['] NAME (compile): bake
\ the address as a literal push into the word being compiled (via c-lit, x11=addr).
: C-TICK ( -- )
   LTOK @ BL,  9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFIND @ BL,
   LBL {: tk :}  13 tk CBZ, \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
   LBL {: twide :} \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
   14 13 8 ANDI,  14 twide CBNZ,        \ DNAME-WIDE gate (mirror of native C-TICK; inert in stage0)
   11 G-PUSH  tk B,
   twide LBL,  0 70 MOVZ,  NR-EXIT-GROUP SYS,
   tk LBL, ;

: C-BTICK ( -- )
   LTOK @ BL,  9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFIND @ BL,
   LBL {: bk :}  13 bk CBZ,  C-LIT  bk LBL, ;

: C-LBRACE-GUARDS ( -- )
   LBL {: qlok :} \ typed-local-lint: allow-bare-local
   11 DATA QPATCH-CELL LDR,  11 qlok CBZ,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 75 MOVZ,  NR-EXIT-GROUP SYS,
   qlok LBL, ;

: C-LBRACE-STORE-ONE ( -- )
   LBL LBL LBL LBL LBL LBL {: nlok noti ncp ncd tsl tsd :}
   11 DATA LOCN-CELL LDR,  11 64 CMPI,  C-LT nlok BCOND,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 75 MOVZ,  NR-EXIT-GROUP SYS,
   nlok LBL,
   13 DATA TKL-CELL LDR,  13 1 CMPI,  C-NE noti BCOND,
   13 DATA TKA-CELL LDR,  13 13 0 LDRB,  14 $20 MOVZ,  13 13 14 ORR,  13 105 CMPI,  C-NE noti BCOND,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 75 MOVZ,  NR-EXIT-GROUP SYS,
   noti LBL,
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

\ Stage0 has no package scope; pass-2 checker queries are global lookups.
: C-P2-FIND-GLOBAL ( n n -- ) {: lvar len :} \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
   LBL {: ok :} \ typed-local-lint: allow-bare-local - Gforth bootstrap labels cannot use Habu type suffixes.
   9 lvar @ ADR,  10 len MOVZ,  LFIND @ BL,
   13 ok CBNZ,
      0 2 MOVZ,  1 lvar @ ADR,  2 len MOVZ,  NR-WRITE SYS,
      0 70 MOVZ,  NR-EXIT-GROUP SYS,
   ok LBL, ;

\ Minimal stage0 package engine. Package records use wid=-1, addr=public WID,
\ and clen=private WID, matching the native dictionary representation. This is
\ sufficient for the early checker/certificate packages and keeps their words
\ out of the global vocabulary during recovery.
: C-PACKAGE-CHECK-CALL ( n n -- ) {: lvar len :} \ typed-local-lint: allow-bare-local
   lvar len C-P2-FIND-GLOBAL
   C-CALL-X11-SAVED ;

: C-PACKAGE-RECORD-MATCH ( n n -- )
   \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
   {: hit miss :}
   LBL LBL {: cmp inl :} \ typed-local-lint: allow-bare-local
   14 5 40 LDR,  15 0 MOVN,  14 15 CMP,  C-NE miss BCOND,
   14 5 16 LDR,  14 14 12 LSLI,  14 14 12 LSRI,
   15 DATA TKL-CELL LDR,  14 15 CMP,  C-NE miss BCOND,
   16 5 24 ADDI,
   14 5 16 LDR,  14 14 DNAME-EXT ANDI,  14 inl CBZ,
      16 5 24 LDR,
   inl LBL,
   7 0 MOVZ,
   cmp LBL,
      15 DATA TKL-CELL LDR,  7 15 CMP,  C-GE hit BCOND,
      15 16 7 ADD,  15 15 0 LDRB,
      3 15 $41 SUBI,  3 26 CMPI,  3 C-CC CSET,  3 3 5 LSLI,  15 15 3 ORR,
      4 DATA TKA-CELL LDR,  4 4 7 ADD,  4 4 0 LDRB,
      3 4 $41 SUBI,  3 26 CMPI,  3 C-CC CSET,  3 3 5 LSLI,  4 4 3 ORR,
      15 4 CMP,  C-NE miss BCOND,
      7 7 1 ADDI,  cmp B, ;

: C-PACKAGE-PROT-GUARD ( n -- )
   {: bad :} \ typed-local-lint: allow-bare-local
   LBL LBL LBL LBL {: loop miss hit done :} \ typed-local-lint: allow-bare-local
   5 DBASE 0 ADDI,  6 NDICT 0 ADDI,
   loop LBL,
      6 done CBZ,
      hit miss C-PACKAGE-RECORD-MATCH
      hit LBL,
         9 5 0 LDR,  LPROTWIDQ @ BL,
         13 done CBZ,
         bad B,
      miss LBL,
         5 5 DREC ADDI,  6 6 1 SUBI,  loop B,
   done LBL, ;

: C-PACKAGE-SEAL-GUARD ( -- )
   LBL LBL {: ok bad :} \ typed-local-lint: allow-bare-local
   9 DATA FRIEND-LATCH-CELL LDR,  9 ok CBZ,
   0 LRESCHECKCERT @ ADR,  1 12 MOVZ,  LKWCMP @ BL,  0 bad CBNZ,
   0 LRESLOWERCERT @ ADR,  1 10 MOVZ,  LKWCMP @ BL,  0 bad CBNZ,
   0 LRESLOWERHOOK @ ADR,  1 15 MOVZ,  LKWCMP @ BL,  0 bad CBNZ,
   bad C-PACKAGE-PROT-GUARD
   ok B,
   bad LBL,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 2 MOVZ,  1 LQNL @ ADR,  1 1 1 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
      0 E-SEAL-PACKAGE MOVZ,  NR-EXIT-GROUP SYS,
   ok LBL, ;

: C-PACKAGE-NAME-GUARD ( -- )
   LBL LBL LBL {: loop bad done :} \ typed-local-lint: allow-bare-local
   14 0 MOVZ,
   loop LBL,
      15 DATA TKL-CELL LDR,  14 15 CMP,  C-GE done BCOND,
      15 DATA TKA-CELL LDR,  15 15 14 ADD,  15 15 0 LDRB,
      15 $3A CMPI,  C-EQ bad BCOND,
      14 14 1 ADDI,  loop B,
   bad LBL,  0 75 MOVZ,  NR-EXIT-GROUP SYS,
   done LBL, ;

: C-PACKAGE-ENSURE ( -- )
   LBL LBL LBL LBL LBL LBL
   {: loop miss hit make room done :} \ typed-local-lint: allow-bare-local
   5 DBASE 0 ADDI,  6 NDICT 0 ADDI,
   loop LBL,
      6 make CBZ,
      hit miss C-PACKAGE-RECORD-MATCH
      hit LBL,
         11 5 0 LDR,  12 5 8 LDR,  done B,
      miss LBL,  5 5 DREC ADDI,  6 6 1 SUBI,  loop B,
   make LBL,
      9 DICT-CAP MOVZ,  NDICT 9 CMP,  C-LT room BCOND,
         0 77 MOVZ,  NR-EXIT-GROUP SYS,
      room LBL,
      9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
      C-STORE-NAME
      5 NDICT 0 ADDI,  10 DREC MOVZ,  5 5 10 MUL,  5 DBASE 5 ADD,
      11 DATA WIDN-CELL LDR,  12 11 1 ADDI,
      14 12 1 ADDI,  14 DATA WIDN-CELL STR,
      11 5 0 STR,  12 5 8 STR,
      14 0 MOVN,  14 5 40 STR,
      NDICT NDICT 1 ADDI,
   done LBL, ;

: C-PACKAGE ( -- )
   LBL LBL {: inactive hastok :} \ typed-local-lint: allow-bare-local
   9 DATA PKG-PUB-CELL LDR,  9 inactive CBZ,
      0 75 MOVZ,  NR-EXIT-GROUP SYS,
   inactive LBL,
   LTOK @ BL,  0 hastok CBNZ,
      0 74 MOVZ,  NR-EXIT-GROUP SYS,
   hastok LBL,
   C-PACKAGE-NAME-GUARD
   9 DATA TKA-CELL LDR,  9 G-PUSH
   9 DATA TKL-CELL LDR,  9 G-PUSH
   \ CHECKER-PACKAGE consumes its arguments; invoke it after they are present.
   LCHKPACKAGE 15 C-P2-FIND-GLOBAL
   C-CALL-X11-SAVED
   C-PACKAGE-SEAL-GUARD
   2 3 MOVZ,  LPROT @ BL,
   C-PACKAGE-ENSURE
   2 5 MOVZ,  LPROT @ BL,
   9 DATA CUR-CELL LDR,  9 DATA PKG-PARENT-CELL STR,
   11 DATA PKG-PUB-CELL STR,  12 DATA PKG-PRI-CELL STR,
   5 DATA PKG-REC-CELL STR,
   12 DATA CUR-CELL STR, ;

: C-PUBLIC ( -- )
   LBL {: active :} \ typed-local-lint: allow-bare-local
   9 DATA PKG-PUB-CELL LDR,  9 active CBNZ,
      0 75 MOVZ,  NR-EXIT-GROUP SYS,
   active LBL,
   LCHKPUB 14 C-PACKAGE-CHECK-CALL
   9 DATA PKG-PUB-CELL LDR,  9 DATA CUR-CELL STR, ;

: C-PRIVATE ( -- )
   LBL {: active :} \ typed-local-lint: allow-bare-local
   9 DATA PKG-PRI-CELL LDR,  9 active CBNZ,
      0 75 MOVZ,  NR-EXIT-GROUP SYS,
   active LBL,
   LCHKPRI 15 C-PACKAGE-CHECK-CALL
   9 DATA PKG-PRI-CELL LDR,  9 DATA CUR-CELL STR, ;

: C-END-PACKAGE ( -- )
   LBL {: active :} \ typed-local-lint: allow-bare-local
   9 DATA PKG-PUB-CELL LDR,  9 active CBNZ,
      0 75 MOVZ,  NR-EXIT-GROUP SYS,
   active LBL,
   LCHKENDPKG 19 C-PACKAGE-CHECK-CALL
   9 DATA PKG-PARENT-CELL LDR,  9 DATA CUR-CELL STR,
   9 0 MOVZ,
   9 DATA PKG-PUB-CELL STR,  9 DATA PKG-PRI-CELL STR,
   9 DATA PKG-PARENT-CELL STR,  9 DATA PKG-REC-CELL STR, ;

: EM-REC-WIDE-PUBLISH ( -- )
   LBL {: nohook :} \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
   9 DATA HOOK-CELL LDR,  9 nohook CBZ,
   LRECWPUB 16 C-P2-FIND-GLOBAL
   C-CALL-X11-SAVED
   nohook LBL, ;

: EM-P2-SLOT-DIE ( -- )
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 75 MOVZ,  NR-EXIT-GROUP SYS, ;

also LOWER-CERT

: EM-P2-CARVE-W ( -- )
   LBL LBL {: localok bindok :} \ typed-local-lint: allow-bare-local
   9 SP 0 LDR,  9 64 CMPI,  C-CC localok BCOND,
      EM-P2-SLOT-DIE
   localok LBL,
   11 DATA TXN-CERT-A-CELL LDR,
   12 11 WF-COUNT-CELL cells LDR,
   13 11 BIND-COUNT-CELL cells LDR,
   14 DATA TXN-BIND-I-CELL LDR,
   14 13 CMP,  C-CC bindok BCOND,
      0 76 MOVZ,  NR-EXIT-GROUP SYS,
   bindok LBL,
   13 WF-CELLS cells MOVZ,  12 12 13 MUL,
   11 11 HEADER-CELLS cells ADDI,  11 11 12 ADD,
   12 14 3 LSLI,  11 11 12 ADD,  10 11 0 LDR,
   12 TXN-LIVE-W-OFF MOVZ,  12 DATA 12 ADD,
   13 9 3 LSLI,  12 12 13 ADD,  10 12 0 STR,
   14 14 1 ADDI,  14 DATA TXN-BIND-I-CELL STR, ;

previous

: EM-P2-LIVE-W ( -- )
   LBL {: ok :} \ typed-local-lint: allow-bare-local
   9 SP 0 LDR,  9 64 CMPI,  C-CC ok BCOND,
      EM-P2-SLOT-DIE
   ok LBL,
   12 TXN-LIVE-W-OFF MOVZ,  12 DATA 12 ADD,
   13 9 3 LSLI,  12 12 13 ADD,  10 12 0 LDR, ;

: EM-P2-LIVE-CUM ( -- )
   LBL LBL LBL {: ok loop done :} \ typed-local-lint: allow-bare-local
   9 SP 0 LDR,  9 64 CMPI,  C-CC ok BCOND,
      EM-P2-SLOT-DIE
   ok LBL,
   10 0 MOVZ,  11 0 MOVZ,
   12 TXN-LIVE-W-OFF MOVZ,  12 DATA 12 ADD,
   loop LBL,  11 9 CMP,  C-HI done BCOND,
      13 11 3 LSLI,  14 12 13 ADD,  13 14 0 LDR,
      10 10 13 ADD,  11 11 1 ADDI,  loop B,
   done LBL, ;

: EM-P2-CARVE ( -- )
   LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL
   {: ql qd wl wd wok pl pd jl jd sok :} \ typed-local-lint: allow-bare-local - Gforth bootstrap labels cannot use Habu type suffixes.
   SP SP 32 SUBI,
   9 DATA P2LOC0-CELL LDR,  9 SP 0 STR,
   9 0 MOVZ,  9 SP 8 STR,
   ql LBL,
      9 SP 0 LDR,  10 DATA LOCN-CELL LDR,
      9 10 CMP,  C-GE qd BCOND,
      EM-P2-CARVE-W
      9 SP 8 LDR,  9 9 10 ADD,  9 SP 8 STR,
      9 SP 0 LDR,  9 9 1 ADDI,  9 SP 0 STR,
      ql B,
   qd LBL,
   9 0 MOVZ,  9 SP 0 STR,                             \ operand pos := 0
   wl LBL,
      9 SP 0 LDR,  10 DATA LOCN-CELL LDR,
      11 DATA P2LOC0-CELL LDR,  10 10 11 SUB,
      9 10 CMP,  C-GE wd BCOND,
      9 DATA TKA-CELL LDR,  11 DATA TXN-SRC-A-CELL LDR,
      9 9 11 SUB,  10 SP 0 LDR,  LP2CWAT @ BL,
      11 SP 0 LDR,  12 DATA LOCN-CELL LDR,
      12 12 1 SUBI,  11 12 11 SUB,                    \ local idx := N-1-pos
      12 TXN-LIVE-W-OFF MOVZ,  12 DATA 12 ADD,
      13 11 3 LSLI,  12 12 13 ADD,  11 12 0 LDR,
      10 11 CMP,  C-EQ wok BCOND,
         [ also LOWER-TXN-CODE ] DRIFT-FAIL-LABEL [ previous ] @ B,
      wok LBL,
      9 SP 0 LDR,  9 9 1 ADDI,  9 SP 0 STR,
      wl B,
   wd LBL,
   9 SP 8 LDR,  5 9 3 LSLI,
   5 5 15 ADDI,  5 5 $FFFFFFFFFFFFFFF0 ANDI,
   9 $D10003FF LIT64,  15 5 10 LSLI,  9 9 15 ORR,  LCEMIT @ BL,
   15 DATA LOCF-CELL LDR,  15 15 5 ADD,  15 DATA LOCF-CELL STR,
   9 DATA LOCN-CELL LDR,  9 9 1 SUBI,  9 SP 0 STR,
   pl LBL,
      9 SP 0 LDR,  10 DATA P2LOC0-CELL LDR,
      9 10 CMP,  C-LT pd BCOND,
      EM-P2-LIVE-W  10 SP 8 STR,
      EM-P2-LIVE-CUM  10 SP 16 STR,
      12 DATA LOCF-CELL LDR,  12 12 3 LSRI,
      10 SP 16 LDR,  12 12 10 SUB,
      10 SP 8 LDR,  10 10 1 SUBI,
      jl LBL,
         10 0 CMPI,  C-LT jd BCOND,
         9 $D1002273 LIT64,  LCEMIT @ BL,
         9 $F9400269 LIT64,  LCEMIT @ BL,
         5 12 10 ADD,
         5 4095 CMPI,  C-LE sok BCOND,
            EM-P2-SLOT-DIE
         sok LBL,
         9 $F90003E9 LIT64,  15 5 10 LSLI,
         9 9 15 ORR,  LCEMIT @ BL,
         10 10 1 SUBI,  jl B,
      jd LBL,
      9 SP 0 LDR,  9 9 1 SUBI,  9 SP 0 STR,
      pl B,
   pd LBL,
   SP SP 32 ADDI, ;

: EM-P2-LOCREF ( -- )
   LBL LBL LBL {: rl rd sok :} \ typed-local-lint: allow-bare-local
   SP SP 32 SUBI,
   0 SP 0 STR,
   EM-P2-LIVE-W  10 SP 8 STR,
   EM-P2-LIVE-CUM  10 SP 16 STR,
   LVSPILL @ BL,
   12 DATA LOCF-CELL LDR,  12 12 3 LSRI,
   10 SP 16 LDR,  12 12 10 SUB,
   10 0 MOVZ,
   rl LBL,
      11 SP 8 LDR,  10 11 CMP,  C-GE rd BCOND,
      5 12 10 ADD,
      5 4095 CMPI,  C-LE sok BCOND,
         EM-P2-SLOT-DIE
      sok LBL,
      9 $F94003E9 LIT64,  15 5 10 LSLI,
      9 9 15 ORR,  LCEMIT @ BL,
      9 W-PUSH0 LIT64,  LCEMIT @ BL,
      9 W-PUSH1 LIT64,  LCEMIT @ BL,
      10 10 1 ADDI,  rl B,
   rd LBL,
   SP SP 32 ADDI, ;

: C-LBRACE-PARSE-NAMES ( -- )
   LBL LBL LBL {: nl nd nstore :}
   6 DATA LOCN-CELL LDR,
   nl LBL,
      LTOK @ BL,  0 nd CBZ,
      LBCAP @ BL,                                          \ locals reach the checker too
      0 LKWENDLOC @ ADR,  1 2 MOVZ,  LKWCMP @ BL,  0 nstore CBZ,  nd B,   \ ":}" -> done
      nstore LBL,
      C-LBRACE-STORE-ONE
      nl B,
   nd LBL, ;

: C-LBRACE-CARVE-FRAME ( -- )
   LBL LBL {: pl pd :}
   LBL LBL {: p1c pjoin :} \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
   9 DATA P2-CELL LDR,  9 p1c CBZ,
      EM-P2-CARVE
      pjoin B,
   p1c LBL,
   13 DATA LOCN-CELL LDR,  14 13 6 SUB,       \ n = N - start
   5 14 3 LSLI,  5 5 15 ADDI,  5 5 $FFFFFFFFFFFFFFF0 ANDI,   \ carve = align16(n*8):
   9 $D10003FF LIT64,  15 5 10 LSLI,  9 9 15 ORR,  LCEMIT @ BL,   \ SP must stay 16-aligned
   15 DATA LOCF-CELL LDR,  15 15 5 ADD,  15 DATA LOCF-CELL STR,   \ (pad sits below the slots)
   12 DATA LOCF-CELL LDR,  12 12 3 LSRI,      \ x12 = total slots in the frame
   13 DATA LOCN-CELL LDR,  13 13 1 SUBI,      \ i = N-1
   LBL {: pl :}  LBL {: pd :}
   pl LBL,
      13 6 CMP,  C-LT pd BCOND,               \ i < start -> done
      9 $D1002273 LIT64,  LCEMIT @ BL,        \ sub x19,#8
      9 $F9400269 LIT64,  LCEMIT @ BL,        \ ldr x9,[x19]
      5 12 13 SUB,  5 5 1 SUBI,               \ scaled off = total - i - 1
      9 $F90003E9 LIT64,  5 5 10 LSLI,  9 9 5 ORR,  LCEMIT @ BL,   \ str x9,[sp,#off]
      13 13 1 SUBI,  pl B,
   pd LBL,
   pjoin LBL, ;

\ {: a b :} (compile): record names, carve a machine-stack frame, and pop
\ declared values into slots. References are resolved by LLOC-FIND.
: C-LBRACE ( -- )
   9 DATA LOCN-CELL LDR,  9 DATA P2LOC0-CELL STR,
   C-LBRACE-GUARDS
   C-LBRACE-PARSE-NAMES
   C-LBRACE-CARVE-FRAME ;

\ S" (interpret mode): copy the string to HERE (transient — no allot) and push
\ ( addr len ). Compile mode bakes bytes into the code image instead (c-sdq).
: C-QUOTE-START ( -- )
   12 DATA INP-CELL LDR,  12 12 1 ADDI,  13 12 0 ADDI, ;

: C-QUOTE-EOF ( -- )                               \ unterminated or bad-escape string literal: label fd 2 before exit 74
   s" hb: bad string literal" 74 C-EXIT-DIAG ;

: C-QUOTE-SCAN ( -- )
   LBL {: sl :}  LBL {: sd :}  LBL {: eof :}
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

: C-ESC-HEX-X9 ( label -- ) {: bad :} \ typed-local-lint: allow-bare-local - Gforth bootstrap labels cannot use Habu local type suffixes.
   LBL LBL LBL {: lower upper done :} \ typed-local-lint: allow-bare-local - Gforth bootstrap labels cannot use Habu local type suffixes.
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

: C-ESC-DECODE-BASIC ( label label -- ) {: hex bad :} \ typed-local-lint: allow-bare-local - Gforth bootstrap labels cannot use Habu local type suffixes.
   LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL {: dq bs bel bs8 esc lf ff cr tab vt nul done :} \ typed-local-lint: allow-bare-local - Gforth bootstrap labels cannot use Habu local type suffixes.
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

\ Escape decoder, emitted once by EMIT-ESC-DECODE, BL-called from the scan and
\ copy loops; entries clobber only x9/x10 (and LR). LESCDEC: x9 escape char ->
\ x9 byte, x10 0=ok 1=hex 2=bad. LESCHEX: x9 hex digit -> x9 nibble, x10 0=ok 2=bad.
\ Mirrors src/habu/habu2.f EMIT-ESC-DECODE.
: EMIT-ESC-DECODE ( -- )
   LBL LBL LBL {: hex bad hbad :} \ typed-local-lint: allow-bare-local - Gforth bootstrap labels cannot use Habu local type suffixes.
   LESCDEC @ LBL,  hex bad C-ESC-DECODE-BASIC  10 0 MOVZ,  RET,
   hex LBL,  10 1 MOVZ,  RET,
   bad LBL,  10 2 MOVZ,  RET,
   LESCHEX @ LBL,  hbad C-ESC-HEX-X9  10 0 MOVZ,  RET,
   hbad LBL,  10 2 MOVZ,  RET, ;

\ Escaped-literal scan, emitted once, BL-called (x12 cursor in/out, x10 count
\ out, x11/x14/x15 scratch; saves LR around the inner decoder BLs).
\ Mirrors src/habu/habu2.f EMIT-ESC-SCAN.
: EMIT-ESC-SCAN ( -- )
   LBL LBL LBL LBL LBL {: scan done esc hex bad :} \ typed-local-lint: allow-bare-local - Gforth bootstrap labels cannot use Habu local type suffixes.
   LESCSCAN @ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   11 0 MOVZ,
   scan LBL,  14 DATA INE-CELL LDR,  12 14 CMP,  C-GE bad BCOND,
      9 12 0 LDRB,  9 $22 CMPI,  C-EQ done BCOND,
      9 $5C CMPI,  C-EQ esc BCOND,
      12 12 1 ADDI,  11 11 1 ADDI,  scan B,
   esc LBL,  12 12 1 ADDI,  12 14 CMP,  C-GE bad BCOND,
      9 12 0 LDRB,  LESCDEC @ BL,
      10 1 CMPI,  C-EQ hex BCOND,  C-GT bad BCOND,
      12 12 1 ADDI,  11 11 1 ADDI,  scan B,
   hex LBL,
      15 12 3 ADDI,  15 14 CMP,  C-GT bad BCOND,
      9 12 1 LDRB,  LESCHEX @ BL,  10 bad CBNZ,
      9 12 2 LDRB,  LESCHEX @ BL,  10 bad CBNZ,
      12 15 0 ADDI,  11 11 1 ADDI,  scan B,
   bad LBL,  C-QUOTE-EOF
   done LBL,  10 11 0 ADDI,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ Escaped-literal copy, emitted once, BL-called (x11 src, x12 end, x17 dst
\ in/out; incoming x10 decoded count preserved; saves LR).
\ Mirrors src/habu/habu2.f EMIT-ESC-COPY.
: EMIT-ESC-COPY ( -- )
   LBL LBL LBL LBL LBL {: copy done esc hex bad :} \ typed-local-lint: allow-bare-local - Gforth bootstrap labels cannot use Habu local type suffixes.
   LESCCOPY @ LBL,
   SP SP 16 SUBI,  10 SP 0 STR,  30 SP 8 STR,
   copy LBL,  11 12 CMP,  C-GE done BCOND,
      9 11 0 LDRB,  9 $5C CMPI,  C-EQ esc BCOND,
      9 17 0 STRB,  17 17 1 ADDI,  11 11 1 ADDI,  copy B,
   esc LBL,
      11 11 1 ADDI,  11 12 CMP,  C-GE bad BCOND,
      9 11 0 LDRB,  LESCDEC @ BL,
      10 1 CMPI,  C-EQ hex BCOND,  C-GT bad BCOND,
      11 11 1 ADDI,  9 17 0 STRB,  17 17 1 ADDI,  copy B,
   hex LBL,
      15 11 3 ADDI,  15 12 CMP,  C-GT bad BCOND,
      9 11 1 LDRB,  LESCHEX @ BL,  10 bad CBNZ,  14 9 4 LSLI,
      9 11 2 LDRB,  LESCHEX @ BL,  10 bad CBNZ,  9 14 9 ORR,
      11 15 0 ADDI,  9 17 0 STRB,  17 17 1 ADDI,  copy B,
   bad LBL,  C-QUOTE-EOF
   done LBL,  10 SP 0 LDR,  30 SP 8 LDR,  SP SP 16 ADDI,  RET, ;

: C-ESC-QUOTE-SCAN ( -- )
   LESCSCAN @ BL, ;

: C-ESC-QUOTE-CONSUME ( -- )
   15 12 13 SUB,  16 13 0 ADDI,  12 12 1 ADDI,  12 DATA INP-CELL STR, ;

: C-ESC-QUOTE-SAVE ( -- )
   SP SP 32 SUBI,  16 SP 0 STR,  15 SP 8 STR,  10 SP 16 STR, ;

: C-ESC-QUOTE-RESTORE ( -- )
   16 SP 0 LDR,  15 SP 8 LDR,  10 SP 16 LDR, ;

: C-ESC-QUOTE-SAVED-DROP ( -- )
   SP SP 32 ADDI, ;

: C-ESC-COPY-X17 ( -- )
   LESCCOPY @ BL, ;

: C-ISDQ ( -- )
   C-QUOTE-START
   C-QUOTE-SCAN
   C-QUOTE-CONSUME
   12 DATA 0 LDR,  15 12 0 ADDI,                        \ x12 = DP, x15 = string base
   14 12 10 ADD,  14 DP-CHECK
   11 13 0 ADDI,  9 10 0 ADDI,
   LBL {: cl :}  LBL {: cd :}
   cl LBL,  9 cd CBZ,
      14 11 0 LDRB,  14 12 0 STRB,  12 12 1 ADDI,  11 11 1 ADDI,  9 9 1 SUBI,  cl B,
   cd LBL,
   12 DATA 0 STR,                                       \ allot: DP advances past the copy
   15 G-PUSH  10 G-PUSH ;

: C-ICQ ( -- )
   C-QUOTE-START
   C-QUOTE-SCAN
   C-QUOTE-CONSUME
   LBL {: capok :}  LBL {: cl :}  LBL {: cd :}
   10 255 CMPI,  C-LE capok BCOND,  s" bootstrap: counted string literal too long" C-EXIT76
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
   C-QUOTE-START
   C-ESC-QUOTE-SCAN
   LBL {: capok :} \ typed-local-lint: allow-bare-local - Gforth bootstrap labels cannot use Habu local type suffixes.
   10 255 CMPI,  C-LE capok BCOND,  s" bootstrap: counted string literal too long" C-EXIT76
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

\ S" string" (compile mode): emit bytes in the RX code image, then push the
\ byte address via C-ADR PC-relative addressing plus the counted length.
\ compile-mode PC-RELATIVE address push: emit `adr x9, target` then the push
\ stencil. Unlike C-LIT's absolute movz/movk, the offset survives the AOT blob
\ copy and the ASLR slide, because the target (an embedded S" body) moves WITH
\ this instruction. target in x11; CP (the emit cursor / future ADR pc) is x28.
: C-ADR ( -- )
   5 11 28 SUB,                                                       \ x5 = d = target - CP
   8 $10000009 LIT64,                                                 \ ADR opcode | Rd=x9
   6 3 MOVZ,  7 5 6 AND,  7 7 29 LSLI,  8 8 7 ORR,                    \ | (d & 3) << 29
   7 5 2 LSRI,  6 $7FFFF LIT64,  7 7 6 AND,  7 7 5 LSLI,  8 8 7 ORR,  \ | ((d>>2) & 0x7FFFF) << 5
   9 8 0 ADDI,  LCEMIT @ BL,                                          \ emit the ADR word
   9 W-PUSH0 LIT64,  LCEMIT @ BL,  9 W-PUSH1 LIT64,  LCEMIT @ BL, ;

: C-SDQ ( -- )
   C-QUOTE-START
   C-QUOTE-SCAN
   C-QUOTE-CONSUME
   C-QUOTE-SAVE
   C-QUOTE-RESTORE
   11 16 0 ADDI,  12 10 1 ADDI,  LBCS @ BL,
   15 CP 0 ADDI,  9 $14000000 LIT64,  LCEMIT @ BL,      \ x15 = B addr; emit B placeholder
   12 CP 0 ADDI,                                        \ x12 = byte addr (after the B)
   C-QUOTE-RESTORE
   11 16 0 ADDI,  9 10 0 ADDI,                          \ copy x10 bytes start->CP
   LBL {: cl :}  LBL {: cd :}
   cl LBL,  9 cd CBZ,
      14 11 0 LDRB,  14 28 0 STRB,  28 28 1 ADDI,  11 11 1 ADDI,  9 9 1 SUBI,  cl B,
   cd LBL,
   28 28 3 ADDI,  5 -4 LIT64,  28 28 5 AND,             \ pad CP to 4
   9 15 0 ADDI,  15 10 0 ADDI,  LPAT @ BL,              \ x9=B addr; save len in x15; patch B->here
   11 12 0 ADDI,  C-ADR                                 \ push byte addr PC-relative (AOT/ASLR-safe)
   11 15 0 ADDI,  C-LIT                                 \ push len (x15)
   C-QUOTE-SAVED-DROP ;

: C-CQ ( -- )
   C-QUOTE-START
   C-QUOTE-SCAN
   C-QUOTE-LEN
   LBL {: capok :}  LBL {: cl :}  LBL {: cd :}
   10 255 CMPI,  C-LE capok BCOND,  s" bootstrap: compiled counted string too long" C-EXIT76
   capok LBL,
   C-QUOTE-CONSUME-DONE
   C-QUOTE-SAVE
   C-QUOTE-RESTORE
   11 16 0 ADDI,  12 10 1 ADDI,  LBCS @ BL,
   15 CP 0 ADDI,  9 $14000000 LIT64,  LCEMIT @ BL,
   12 CP 0 ADDI,
   C-QUOTE-RESTORE
   10 28 0 STRB,  28 28 1 ADDI,
   11 16 0 ADDI,  9 10 0 ADDI,
   cl LBL,  9 cd CBZ,
      14 11 0 LDRB,  14 28 0 STRB,  28 28 1 ADDI,  11 11 1 ADDI,  9 9 1 SUBI,  cl B,
   cd LBL,
   28 28 3 ADDI,  5 -4 LIT64,  28 28 5 AND,
   9 15 0 ADDI,  15 10 1 ADDI,  LPAT @ BL,
   11 12 0 ADDI,  C-ADR
   C-QUOTE-SAVED-DROP ;

: C-DOTQ ( -- )
   LBL {: ok :}
   C-SDQ
   9 LKWTYPE @ ADR,  10 4 MOVZ,  LFIND @ BL,
   13 ok CBNZ,  0 70 MOVZ,  NR-EXIT-GROUP SYS,
   ok LBL,
   C-CALL ;

: C-ESDQ ( -- )
   C-QUOTE-START
   C-ESC-QUOTE-SCAN
   C-ESC-QUOTE-CONSUME
   C-ESC-QUOTE-SAVE
   C-ESC-QUOTE-RESTORE
   11 16 0 ADDI,  12 15 1 ADDI,  LBCS @ BL,
   9 $14000000 LIT64,  LCEMIT @ BL,
   13 CP 0 ADDI,
   C-ESC-QUOTE-RESTORE
   11 16 0 ADDI,  12 16 15 ADD,  17 28 0 ADDI,
   C-ESC-COPY-X17
   28 17 0 ADDI,
   28 28 3 ADDI,  5 -4 LIT64,  28 28 5 AND,
   12 13 0 ADDI,
   9 13 4 SUBI,  15 10 0 ADDI,  LPAT @ BL,
   11 12 0 ADDI,  C-ADR
   11 15 0 ADDI,  C-LIT
   C-ESC-QUOTE-SAVED-DROP ;

: C-ECQ ( -- )
   C-QUOTE-START
   C-ESC-QUOTE-SCAN
   LBL {: capok :} \ typed-local-lint: allow-bare-local - Gforth bootstrap labels cannot use Habu local type suffixes.
   10 255 CMPI,  C-LE capok BCOND,  s" bootstrap: compiled counted string too long" C-EXIT76
   capok LBL,
   C-ESC-QUOTE-CONSUME
   C-ESC-QUOTE-SAVE
   C-ESC-QUOTE-RESTORE
   11 16 0 ADDI,  12 15 1 ADDI,  LBCS @ BL,
   9 $14000000 LIT64,  LCEMIT @ BL,
   13 CP 0 ADDI,
   C-ESC-QUOTE-RESTORE
   10 28 0 STRB,  28 28 1 ADDI,
   11 16 0 ADDI,  12 16 15 ADD,  17 28 0 ADDI,
   C-ESC-COPY-X17
   28 17 0 ADDI,
   28 28 3 ADDI,  5 -4 LIT64,  28 28 5 AND,
   12 13 0 ADDI,
   9 13 4 SUBI,  15 10 1 ADDI,  LPAT @ BL,
   11 12 0 ADDI,  C-ADR
   C-ESC-QUOTE-SAVED-DROP ;

: C-EDOTQ ( -- )
   LBL {: ok :} \ typed-local-lint: allow-bare-local - Gforth bootstrap labels cannot use Habu local type suffixes.
   C-ESDQ
   9 LKWTYPE @ ADR,  10 4 MOVZ,  LFIND @ BL,
   13 ok CBNZ,  0 70 MOVZ,  NR-EXIT-GROUP SYS,
   ok LBL,
   C-CALL ;

\ emit one compile-mode keyword case: if TKA/TKL == kw, run handler then back to lmain
: CF-ENTRY ( n ptr u8 xt -- ) {: lmainlbl kwvar kwlen hxt -- :}
   0 kwvar @ ADR,  1 kwlen MOVZ,  LKWCMP @ BL,
   LBL {: skip :}  0 skip CBZ,
   LVSPILL @ BL,
   hxt execute  lmainlbl B,
   skip LBL, ;

\ cfn-entry: keyword case WITHOUT the spill — loop words manage the VS
\ themselves (BEGIN snapshots it, AGAIN/REPEAT reconcile to the snapshot).
: CFN-ENTRY ( n ptr u8 xt -- ) {: lmainlbl kwvar kwlen hxt -- :}
   0 kwvar @ ADR,  1 kwlen MOVZ,  LKWCMP @ BL,
   LBL {: skip :}  0 skip CBZ,
   hxt execute  lmainlbl B,
   skip LBL, ;

variable CFSK
variable CFSK2

\ cfb-entry: branch keywords (if/until/while) with the condition on the VS —
\ a REGISTER top branches directly (no spill + memory pop); con or empty falls
\ back to the spill + pop path. hxtr gets the condition reg in x14.
: CFB-ENTRY ( n ptr u8 xt xt -- ) {: lmainlbl kwvar kwlen hxtm hxtr :}
   LBL CFSK !  LBL CFSK2 !
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
: CFBN-ENTRY ( n ptr u8 xt xt -- ) {: lmainlbl kwvar kwlen hxtm hxtr :}
   LBL CFSK !  LBL CFSK2 !
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

: J-IFR ( -- )  C-PUSHCP  8 $B4000000 LIT64,  9 8 14 ORR,  LCEMIT @ BL, ;

: J-WHILER ( -- )  J-IFR ;

: J-UNTILR ( -- )                                 \ reg flag -> x17 first: the reconcile
   8 $AA0003F1 LIT64,  7 14 16 LSLI,  9 8 7 ORR,  LCEMIT @ BL,   \ may reload into it
   J-UNTILX ;

: EMIT-ENTRY-ARGS ( -- )
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

: EMIT-RUNTIME-STACK ( -- )
   RBASE LANCHOR @ ADR,
   SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,
   SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,
   XDS SP 0 ADDI, ;

: EMIT-MMAP-CODE-REGION ( -- )
   LBL {: rvok :}
   0 RBASE-VA LIT64,  1 REGION LIT64,  2 3 MOVZ,  3 MAP-ANON-PRIVATE-FIXED LIT64,  4 0 MOVN,  5 0 MOVZ,
   NR-MMAP SYS,
   5 RBASE-VA LIT64,  0 5 CMP,
   C-EQ rvok BCOND,
      0 78 MOVZ,  NR-EXIT-GROUP SYS,
   rvok LBL,
   DBASE 0 0 ADDI,
   CP DBASE 0 ADDI,  5 DICT-SIZE LIT64,  CP CP 5 ADD, ;

: EMIT-SEED-DICT ( -- )
   LBL LBL {: scopy scdone :}
   11 LNCOUNT @ ADR,  11 11 0 LDR,  NDICT 11 0 ADDI,
   9 LDICT @ ADR,  10 DBASE 0 ADDI,  12 11 0 ADDI,
   scopy LBL,
      12 scdone CBZ,
      5 9 0 LDR,  6 9 8 LDR,
      7 RBASE 5 ADD,  7 10 0 STR,
      6 6 5 SUB,  6 6 4 SUBI,  6 10 8 STR,
      5 9 16 LDR,  5 10 16 STR,
      5 9 24 LDR,  5 10 24 STR,  5 9 32 LDR,  5 10 32 STR,
      5 9 40 LDR,  5 10 40 STR,
      9 9 DREC ADDI,  10 10 DREC ADDI,  12 12 1 SUBI,  scopy B,
   scdone LBL, ;

: EMIT-DATA-VA>N ( -- n ) DATA-VA ;

: EMIT-MMAP-DATA-REGION ( -- )
   LBL {: dvok :}
   0 EMIT-DATA-VA>N LIT64,  1 DATA-SIZE LIT64,  2 3 MOVZ,  3 MAP-ANON-PRIVATE-FIXED LIT64,  4 0 MOVN,  5 0 MOVZ,
   NR-MMAP SYS,
   5 EMIT-DATA-VA>N LIT64,  0 5 CMP,
   C-EQ dvok BCOND,
      0 78 MOVZ,  NR-EXIT-GROUP SYS,
   dvok LBL, ;

: EMIT-DATA-INIT ( -- )
   20 0 RBASE-CELL STR,
   DATA 0 0 ADDI,
   XDS DATA S0-CELL STR,
   13 DATA ARGC-CELL STR,  14 DATA ARGV-CELL STR,  15 DATA ENVP-CELL STR,
   5 DATA-START LIT64,  7 DATA 5 ADD,  7 DATA DP-CELL STR, ;

: EMIT-SNAPSHOT-COPY-CODE ( -- )
   LBL LBL {: sc1 sc1d :}
   13 DBASE 0 ADDI,  14 0 MOVZ,
   sc1 LBL,  14 6 CMP,  C-GE sc1d BCOND,
      3 8 14 ADD,  3 3 0 LDRB,  4 13 14 ADD,  3 4 0 STRB,
      14 14 1 ADDI,  sc1 B,
   sc1d LBL, ;

: EMIT-SNAPSHOT-COPY-DATA ( -- )
   LBL LBL {: sc2 sc2d :}
   8 12 7 SUB,  13 DATA 0 ADDI,  14 0 MOVZ,
   sc2 LBL,  14 7 CMP,  C-GE sc2d BCOND,
      3 8 14 ADD,  3 3 0 LDRB,  4 13 14 ADD,  3 4 0 STRB,
      14 14 1 ADDI,  sc2 B,
   sc2d LBL, ;

: EMIT-SNAPSHOT-REBASE-DICT ( -- )
   LBL LBL LBL LBL {: sdl2 sdn2 sds2 srn :}
   9 DBASE 0 ADDI,  10 0 MOVZ,
   sdl2 LBL,  10 NDICT CMP,  C-GE sdn2 BCOND,
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
   sdn2 LBL, ;

: EMIT-SNAPSHOT-REBASE-CALLS ( -- )
   LBL LBL LBL {: srl srn srx :}
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
      13 13 21 SUB,  13 13 25 ADD,
      10 9 0 LDRW,  5 $FFE0001F LIT64,  10 10 5 AND,
        14 13 0 ADDI,  5 $FFFF LIT64,  14 14 5 AND,  14 14 5 LSLI,  10 10 14 ORR,  10 9 0 STRW,
      10 9 4 LDRW,  5 $FFE0001F LIT64,  10 10 5 AND,
        14 13 16 LSRI,  5 $FFFF LIT64,  14 14 5 AND,  14 14 5 LSLI,  10 10 14 ORR,  10 9 4 STRW,
      10 9 8 LDRW,  5 $FFE0001F LIT64,  10 10 5 AND,
        14 13 32 LSRI,  5 $FFFF LIT64,  14 14 5 AND,  14 14 5 LSLI,  10 10 14 ORR,  10 9 8 STRW,
      9 9 12 ADDI,
   srn LBL,  9 9 4 ADDI,  srl B,
   srx LBL, ;

: EMIT-SNAPSHOT-RX-FLUSH ( -- )
   2 5 MOVZ,  LPROT @ BL,
   9 DBASE 0 ADDI,  5 DICT-SIZE LIT64,  9 9 5 ADD,  LFLUSH @ BL, ;

: EMIT-SNAPSHOT-RESTORE ( -- )
   LBL LBL LBL {: snomag snbad snok :}
   24 0 MOVZ,
   9 DATA RBASE-CELL LDR,  25 9 0 ADDI,
   10 9 0 ADDI,  5 $1000 LIT64,  10 10 5 SUB,
   11 10 IMAGE-TEXT-SIZE-OFF LDR,
   12 10 11 ADD,  5 IMAGE-TEXT-TRAILER-ADJ LIT64,  12 12 5 ADD,  12 12 40 SUBI,
   13 12 0 LDR,  5 SNAP-MAGIC LIT64,  13 5 CMP,  C-NE snomag BCOND,
   5 IMAGE-TEXT-CONTENT-ADJ LIT64,  11 11 5 SUB,
   21 12 8 LDR,
   15 12 16 LDR,
   6 12 24 LDR,
   7 12 32 LDR,
   5 REGION LIT64,  6 5 CMP,  C-GT snbad BCOND,
   5 DATA-SIZE LIT64,  7 5 CMP,  C-GT snbad BCOND,
   5 DICT-CAP MOVZ,  15 5 CMP,  C-GT snbad BCOND,
   snok B,
   snbad LBL,  s" hb: snapshot trailer corrupt" 79 C-EXIT-DIAG   \ label the fd-2 diagnostic before exit 79 (no rc-80 version path in the gforth tripwire)
   snok LBL,
   9 DATA ARGC-CELL LDR,  10 DATA ARGV-CELL LDR,  0 DATA ENVP-CELL LDR,
   22 11 6 SUB,  22 22 7 SUB,  22 22 40 SUBI,
   8 12 7 SUB,  8 8 6 SUB,
   EMIT-SNAPSHOT-COPY-CODE
   EMIT-SNAPSHOT-COPY-DATA
   25 DATA RBASE-CELL STR,
   XDS DATA S0-CELL STR,
   9 DATA ARGC-CELL STR,  10 DATA ARGV-CELL STR,  0 DATA ENVP-CELL STR,
   NDICT 15 0 ADDI,
   CP DBASE 6 ADD,
   EMIT-SNAPSHOT-REBASE-DICT
   EMIT-SNAPSHOT-REBASE-CALLS
   EMIT-SNAPSHOT-RX-FLUSH
   24 1 MOVZ,
   24 DATA SNAP-CELL STR,
   snomag LBL, ;

: EMIT-STARTUP-RUNTIME-STATE ( -- )
   LBL {: cwok :}
   9 0 MOVZ,  9 DATA HND-CELL STR,
   9 DATA SNAP-CELL LDR,
   9 cwok CBNZ,
   9 0 MOVZ,  9 DATA CUR-CELL STR,
   9 1 MOVZ,  9 DATA WIDN-CELL STR,
   9 0 MOVZ,  9 DATA HOOK-CELL STR,
   9 DATA PROT-WID-N-CELL STR,
   cwok LBL,
   9 0 MOVZ,  9 DATA LOOPSP-CELL STR,
   9 DATA PKG-PUB-CELL STR,  9 DATA PKG-PRI-CELL STR,
   9 DATA PKG-PARENT-CELL STR,  9 DATA PKG-REC-CELL STR,
   9 DATA P2-CELL STR,  9 DATA TXN-WF-I-CELL STR,
   9 DATA P2BODY0-CELL STR,  9 DATA P2INP-CELL STR,
   9 DATA P2INE-CELL STR,  9 DATA P2DP-CELL STR,
   9 DATA P2W0-CELL STR,  9 DATA P2W1-CELL STR,
   9 DATA P2W2-CELL STR,  9 DATA P2W3-CELL STR,
   9 DATA P2LOC0-CELL STR,
   9 DATA TXN-ACTIVE-CELL STR,  9 DATA TXN-SRC-A-CELL STR,
   9 DATA TXN-SRC-U-CELL STR,  9 DATA TXN-CERT-A-CELL STR,
   9 DATA TXN-CERT-U-CELL STR,  9 DATA TXN-BIND-I-CELL STR,
   9 DATA TXN-FETCH-I-CELL STR,
   9 DATA TXN-BLOB-A-CELL STR,  9 DATA TXN-BLOB-CAP-CELL STR,
   G-INSTALL-CRASH
   G-INSTALL-TRAP ;

: EMIT-STARTUP ( -- )
   LANCHOR @ LBL,
   EMIT-ENTRY-ARGS
   EMIT-RUNTIME-STACK
   EMIT-MMAP-CODE-REGION
   EMIT-SEED-DICT
   EMIT-MMAP-DATA-REGION
   EMIT-DATA-INIT
   EMIT-SNAPSHOT-RESTORE
   EMIT-STARTUP-RUNTIME-STATE ;

: EMIT-MAIN-RUNTIME-LABELS ( n -- ) {: lmain :}
   9 LDOESPATCH @ ADR,  9 DATA DOESP-CELL STR,
   9 LCREATE @ ADR,  9 DATA CREATEP-CELL STR,
   9 LRREC @ ADR,  9 DATA RRECP-CELL STR,
   9 lmain ADR,  9 DATA LMAINP-CELL STR,
   LVRINIT @ BL,
   EMIT-SOURCE
   9 0 MOVZ,  9 DATA PEND-CELL STR, ;

: EMIT-TOKEN-DISPATCH ( n n n -- ) {: lmain lexit lcompile :}
   LBL LBL LBL {: notcom skln skpar :}
   LTOK @ BL,  0 lexit CBZ,
   9 DATA TKL-CELL LDR,  9 1 CMPI,  C-NE notcom BCOND,
   9 DATA TKA-CELL LDR,  9 9 0 LDRB,
   9 92 CMPI,  C-EQ skln BCOND,
   9 40 CMPI,  C-NE notcom BCOND,
   skpar LBL,  11 DATA INP-CELL LDR,  12 DATA INE-CELL LDR,  11 12 CMP,  C-GE lmain BCOND,
      9 11 0 LDRB,  11 11 1 ADDI,  11 DATA INP-CELL STR,  9 41 CMPI,  C-NE skpar BCOND,  lmain B,
   skln LBL,  11 DATA INP-CELL LDR,  12 DATA INE-CELL LDR,  11 12 CMP,  C-GE lmain BCOND,
      9 11 0 LDRB,  11 11 1 ADDI,  11 DATA INP-CELL STR,  9 10 CMPI,  C-NE skln BCOND,  lmain B,
   notcom LBL,
   9 DATA PEND-CELL LDR,  9 lcompile CBNZ, ;

: C-COLON-TOKEN? ( n -- ) {: lnotcolon :}
   LBL LBL {: ok ktry :}
   9 DATA TKL-CELL LDR,  9 1 CMPI,  C-NE ktry BCOND,
   9 DATA TKA-CELL LDR,  9 9 0 LDRB,  9 58 CMPI,  C-EQ ok BCOND,
   ktry LBL,
   0 LKWKERNEL @ ADR,  1 7 MOVZ,  LKWCMP @ BL,  0 lnotcolon CBZ,
   ok LBL, ;

: C-COLON-CODE-ROOM ( -- )
   LBL {: cpok :}
   9 REGION $4000 - LIT64,  9 DBASE 9 ADD,  CP 9 CMP,  C-LT cpok BCOND,
      s" bootstrap: code region full" C-EXIT76
   cpok LBL, ;

\ Dict-capacity exit (mirrors native C-DIE-DICT-FULL, dot
\ habu-gate-runner-entry-81c84af0): fixed label + the current token + newline
\ to fd 2, exit 77. The msg blob carries the newline at offset 24.
: C-COLON-DICT-ROOM ( -- )
   \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
   LBL LBL LBL {: ndok msg full :}
   9 DICT-CAP MOVZ,  NDICT 9 CMP,  C-LT ndok BCOND,
   full B,
   msg LBL,  s" hb: dictionary full at: " BYTES,  CAPNL-KW 1 BYTES,
   full LBL,
      0 2 MOVZ,  1 msg ADR,  2 24 MOVZ,  NR-WRITE SYS,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 2 MOVZ,  1 msg ADR,  1 1 24 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
      0 77 MOVZ,  NR-EXIT-GROUP SYS,
   ndok LBL, ;

: C-COLON-PENDING-DREC ( -- )
   LTOK @ BL,
   12 0 MOVZ,  12 DATA BODYLEN-CELL STR,
   LBCAP @ BL,                                           \ checker sees the original spelling
   C-QUALIFY-DEF
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   9 DATA PEND-CELL STR,
   C-STORE-DEF-NAME
   CP 9 0 STR,
   5 CFSTK-OFF LIT64,  11 DBASE 5 ADD,  12 0 MOVZ,  12 11 0 STR,
   12 0 MOVZ,  12 DATA LOCN-CELL STR,  12 DATA LOCF-CELL STR,
   12 DATA CMM-CELL STR,  12 DATA CMFRD-CELL STR,  12 DATA CMBK-CELL STR,
   ;

: C-COLON-MAYBE-SIG ( -- )
   LBL LBL {: nsig scd :}
   nsig C-SIG-START
   scd C-SIG-END
   scd LBL,
   C-SIG-CAPTURE-TSIG
   nsig LBL, ;

: C-COLON-RESET-COMPILE-STATE ( -- )
   12 0 MOVZ,  12 DATA VSP-CELL STR,  12 DATA SNAPSP-CELL STR,
   12 DATA EXITH-CELL STR,  12 DATA LVD-CELL STR,
   12 DATA QPATCH-CELL STR,
   12 VRALL MOVZ,  12 DATA VRFREE-CELL STR,
   12 FRALL MOVZ,  12 DATA FRFREE-CELL STR, ;

: C-COLON-WORD-PROLOGUE ( -- )
   9 $D10043FF LIT64,  LCEMIT @ BL,
   9 $F90003FE LIT64,  LCEMIT @ BL, ;

: C-CLEAR-TRUSTED-STATE ( -- )
   9 0 MOVZ,
   9 DATA TSIG-A-CELL STR,   9 DATA TSIG-U-CELL STR,
   9 DATA TCSIG-A-CELL STR,  9 DATA TCSIG-U-CELL STR,
   9 DATA DOESB-CELL STR,
   9 DATA TRUSTED-CELL STR, ;

: EMIT-P2-COPY ( -- )
   LP2COPY @ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   8 $D2800009 LIT64,  7 5 5 LSLI,  9 8 7 ORR,  LCEMIT @ BL,
   8 $D100026A LIT64,  7 6 13 LSLI,  9 8 7 ORR,  LCEMIT @ BL,
   $F940014B C-EMITW  $9100214A C-EMITW  $F900026B C-EMITW
   W-PUSH1 C-EMITW  $F1000529 C-EMITW  $54FFFF61 C-EMITW
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

: EMIT-P2-DROPN ( -- )
   LP2DROPN @ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   8 $D1000273 LIT64,  7 5 13 LSLI,  9 8 7 ORR,  LCEMIT @ BL,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

: EMIT-P2-REV ( -- )
   LP2REV @ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   8 $D100026A LIT64,  7 5 10 LSLI,  9 8 7 ORR,  LCEMIT @ BL,
   8 $D100026B LIT64,  7 6 10 LSLI,  9 8 7 ORR,  LCEMIT @ BL,
   $EB0B015F C-EMITW  $54000102 C-EMITW
   $F940014C C-EMITW  $F940016D C-EMITW
   $F900014D C-EMITW  $F900016C C-EMITW
   $9100214A C-EMITW  $D100216B C-EMITW  $17FFFFF8 C-EMITW
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

: EMIT-P2-ROT ( -- )
   LP2ROT @ LBL,
   SP SP 32 SUBI,  30 SP 0 STR,  5 SP 8 STR,  6 SP 16 STR,
   5 5 3 LSLI,
   7 SP 16 LDR,  6 SP 8 LDR,  6 6 7 SUB,  6 6 3 LSLI,  6 6 8 ADDI,
   LP2REV @ BL,
   7 SP 16 LDR,  5 SP 8 LDR,  5 5 7 SUB,  5 5 3 LSLI,  6 8 MOVZ,
   LP2REV @ BL,
   5 SP 8 LDR,  5 5 3 LSLI,  6 8 MOVZ,
   LP2REV @ BL,
   30 SP 0 LDR,  SP SP 32 ADDI,  RET, ;

: EMIT-P2-RS ( -- )
   LBL LBL {: rsto rsdone :} \ typed-local-lint: allow-bare-local
   LP2RS @ LBL,
   SP SP 32 SUBI,  30 SP 0 STR,  5 SP 8 STR,  6 SP 16 STR,
   10 20 RSP-CELL W-LDRX C-EMITW
   6 rsto CBZ,
   8 $D100014A LIT64,  5 SP 8 LDR,  7 5 10 LSLI,  9 8 7 ORR,  LCEMIT @ BL,
   $8B0A0E8B C-EMITW
   8 $D2800009 LIT64,  5 SP 8 LDR,  7 5 5 LSLI,  9 8 7 ORR,  LCEMIT @ BL,
   13 11 RSTK-OFF W-LDRX C-EMITW  $9100216B C-EMITW
   $F900026D C-EMITW  W-PUSH1 C-EMITW  $F1000529 C-EMITW  $54FFFF61 C-EMITW
   6 SP 16 LDR,  6 2 CMPI,  C-EQ rsdone BCOND,
   10 20 RSP-CELL W-STRX C-EMITW
   rsdone B,
   rsto LBL,
   $8B0A0E8B C-EMITW
   8 $D100026C LIT64,  5 SP 8 LDR,  7 5 13 LSLI,  9 8 7 ORR,  LCEMIT @ BL,
   8 $D2800009 LIT64,  5 SP 8 LDR,  7 5 5 LSLI,  9 8 7 ORR,  LCEMIT @ BL,
   $F940018D C-EMITW  $9100218C C-EMITW
   13 11 RSTK-OFF W-STRX C-EMITW  $9100216B C-EMITW
   $F1000529 C-EMITW  $54FFFF61 C-EMITW
   8 $D1000273 LIT64,  5 SP 8 LDR,  7 5 13 LSLI,  9 8 7 ORR,  LCEMIT @ BL,
   8 $9100014A LIT64,  5 SP 8 LDR,  7 5 10 LSLI,  9 8 7 ORR,  LCEMIT @ BL,
   10 20 RSP-CELL W-STRX C-EMITW
   rsdone LBL,
   30 SP 0 LDR,  SP SP 32 ADDI,  RET, ;

\ Validate each active descriptor row before a typed fetch mutates the stack.
also LOWER-CERT

: EMIT-P2-VALID-EXEC ( -- )
   LBL LBL LBL LBL LBL LBL {: outer guard active inactive invalid done :} \ typed-local-lint: allow-bare-local - Gforth labels cannot use Habu type suffixes.
   LP2VEXEC @ LBL,
   15 30 4 ADDI,
   9 15 0 LDR,  15 15 8 ADDI,
   19 19 8 SUBI,  10 19 0 LDR,  19 19 8 ADDI,
   outer LBL,
      9 done CBZ,
      11 15 0 LDR,
      12 15 8 LDR,
      13 15 16 LDR,  15 15 CHECK-CELLS cells ADDI,
   guard LBL,
      13 active CBZ,
      16 15 0 LDR,  17 15 8 LDR,  8 15 16 LDR,
      15 15 GUARD-CELLS cells ADDI,
      13 13 1 SUBI,
      16 16 3 LSLI,  16 10 16 ADD,  16 16 0 LDR,
      16 8 CMP,  C-CS invalid BCOND,
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
      16 GUARD-CELLS cells MOVZ,  16 13 16 MUL,
      15 15 16 ADD,
      9 9 1 SUBI,
      outer B,
   invalid LBL,
      0 2 MOVZ,  1 LVPBADMSG @ ADR,  2 18 MOVZ,  NR-WRITE SYS,
      0 2 MOVZ,  1 LQNL @ ADR,  1 1 1 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
      0 E-BAD-TAG MOVZ,  NR-EXIT-GROUP SYS,
   done LBL,
   RET, ;

previous

\ Copy the frozen certificate descriptor inline and emit an absolute validator call.
: EMIT-P2-VALID-EMIT ( -- )
   LBL LBL LBL {: copy patch done :} \ typed-local-lint: allow-bare-local - Gforth labels cannot use Habu type suffixes.
   LP2VEMIT @ LBL,
   SP SP 48 SUBI,  30 SP 0 STR,
   9 0 MOVZ,  9 SP 24 STR,
   9 DATA TKA-CELL LDR,
   10 DATA TXN-SRC-A-CELL LDR,  9 9 10 SUB,
   LP2CDESC @ BL,
   11 SP 8 STR,  12 SP 16 STR,
   12 done CBZ,
   11 LP2VEXEC @ ADR,
   C-CALL-EMIT-ABSOLUTE
   14 CP 0 ADDI,  14 SP 24 STR,
   $14000000 C-EMITW
   11 SP 8 LDR,  12 SP 16 LDR,
   copy LBL,
      12 patch CBZ,
      9 11 0 LDRW,  LCEMIT @ BL,
      11 11 4 ADDI,  12 12 4 SUBI,
      copy B,
   patch LBL,
      9 SP 24 LDR,
      LPAT @ BL,
   done LBL,
   30 SP 0 LDR,  SP SP 48 ADDI,  RET, ;

\ LP2FETCH emits a width-constant bundle load. The address is popped first;
\ memory slot 0..tag is then pushed in canonical ascending order.
: EMIT-P2-FETCH ( -- )
   LP2FETCH @ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   $D1002273 C-EMITW
   $F940026A C-EMITW
   8 $D2800009 LIT64,  7 5 5 LSLI,  9 8 7 ORR,  LCEMIT @ BL,
   $F940014B C-EMITW
   $9100214A C-EMITW
   $F900026B C-EMITW
   W-PUSH1 C-EMITW
   $F1000529 C-EMITW
   $54FFFF61 C-EMITW
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ LP2STORE emits a width-constant bundle store. Each destination cell passes
\ the authoritative whole-span protection primitive before mutation.
: EMIT-P2-STORE ( -- )
   LP2STORE @ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,  5 SP 8 STR,
   $D1002273 C-EMITW
   $F940026A C-EMITW
   8 $D100026E LIT64,  7 5 13 LSLI,  9 8 7 ORR,  LCEMIT @ BL,
   8 $D2800009 LIT64,  7 5 5 LSLI,  9 8 7 ORR,  LCEMIT @ BL,
   $D280010B C-EMITW
   11 LPROTSPAN @ ADR,
   C-CALL-EMIT-ABSOLUTE
   $F94001CF C-EMITW
   $F900014F C-EMITW
   $910021CE C-EMITW
   $9100214A C-EMITW
   $F1000529 C-EMITW
   $54FFFF61 C-EMITW
   5 SP 8 LDR,
   8 $D1000273 LIT64,  7 5 13 LSLI,  9 8 7 ORR,  LCEMIT @ BL,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

also LOWER-CERT also LOWER-TXN-LOOKUP definitions

: EMIT-WIDTH ( -- )
   LBL LBL LBL LBL {: missing keyeq found drift :} \ typed-local-lint: allow-bare-local
   LP2CWAT @ LBL,
   14 10 0 ADDI,
   11 DATA TXN-CERT-A-CELL LDR,
   12 11 WF-COUNT-CELL cells LDR,
   13 DATA TXN-WF-I-CELL LDR,
   13 12 CMP,  C-EQ missing BCOND,  C-HI drift BCOND,
   15 WF-CELLS cells MOVZ,  15 13 15 MUL,
   11 11 HEADER-CELLS cells ADDI,  11 11 15 ADD,
   15 11 0 LDR,  9 15 CMP,  C-EQ keyeq BCOND,  C-HI drift BCOND,
   missing LBL,  10 1 MOVZ,  RET,
   keyeq LBL,
      15 11 1 cells LDR,  14 15 CMP,  C-EQ found BCOND,  C-HI drift BCOND,
      10 1 MOVZ,  RET,
   found LBL,
      10 11 2 cells LDR,
      13 13 1 ADDI,  13 DATA TXN-WF-I-CELL STR,  RET,
   drift LBL,  [ also LOWER-TXN-CODE ] DRIFT-LABEL 37 DIE [ previous ] ;

: EMIT-DESC ( -- )
   LBL LBL LBL {: missing found drift :} \ typed-local-lint: allow-bare-local
   LP2CDESC @ LBL,
   10 DATA TXN-CERT-A-CELL LDR,
   13 10 WF-COUNT-CELL cells LDR,
   14 10 BIND-COUNT-CELL cells LDR,
   15 10 FETCH-COUNT-CELL cells LDR,
   11 10 HEADER-CELLS cells ADDI,
   12 WF-CELLS cells MOVZ,  13 13 12 MUL,  11 11 13 ADD,
   14 14 3 LSLI,  11 11 14 ADD,
   14 DATA TXN-FETCH-I-CELL LDR,
   14 15 CMP,  C-EQ missing BCOND,  C-HI drift BCOND,
   13 FETCH-CELLS cells MOVZ,  13 14 13 MUL,  11 11 13 ADD,
   13 11 0 LDR,  9 13 CMP,  C-EQ found BCOND,  C-HI drift BCOND,
   missing LBL,  11 0 MOVZ,  12 0 MOVZ,  RET,
   found LBL,
      13 11 1 cells LDR,  12 11 2 cells LDR,
      13 13 3 LSLI,  11 10 13 ADD,
      12 12 3 LSLI,
      14 14 1 ADDI,  14 DATA TXN-FETCH-I-CELL STR,  RET,
   drift LBL,  [ also LOWER-TXN-CODE ] DRIFT-LABEL 37 DIE [ previous ] ;

: EMIT ( -- )
   EMIT-WIDTH
   EMIT-DESC ;

previous previous definitions

: EMIT-P2-HELPERS ( -- )
   EMIT-P2-COPY  EMIT-P2-DROPN  EMIT-P2-REV  EMIT-P2-ROT  EMIT-P2-RS
   [ also LOWER-TXN-LOOKUP ] EMIT [ previous ]
   EMIT-P2-VALID-EXEC
   EMIT-P2-VALID-EMIT
   EMIT-P2-FETCH
   EMIT-P2-STORE ;

: EMIT-P2KW ( -- )
   LCERTBYTES @ LBL, s" lower-cert:bytes" BYTES,
   LVPBADMSG @ LBL, s" hb: bad layout tag" BYTES,
   LP2NEST @ LBL, s" hb: nested definition in pass 2: " BYTES,
   [ also LOWER-TXN-CODE ] BAD-LABEL [ previous ] @ LBL, s" hb: malformed lowering certificate" BYTES,
   [ also LOWER-TXN-CODE ] MEM-LABEL [ previous ] @ LBL, s" hb: lowering transaction memory failed" BYTES,
   [ also LOWER-TXN-CODE ] FULL-LABEL [ previous ] @ LBL, s" hb: lowering transaction full" BYTES,
   [ also LOWER-TXN-CODE ] DRIFT-LABEL [ previous ] @ LBL, s" hb: lowering certificate replay drift" BYTES,
   LRECWPUB @ LBL,  s" rec-wide-publish" BYTES,
   LKWTUCK3 @ LBL, s" tuck" BYTES,  LKWROT3 @ LBL, s" rot" BYTES,
   LKWMROT3 @ LBL, s" -rot" BYTES,  LKW2DUP3 @ LBL, s" 2dup" BYTES,
   LKW2DROP3 @ LBL, s" 2drop" BYTES,  LKW2SWAP3 @ LBL, s" 2swap" BYTES,
   LKW2OVER3 @ LBL, s" 2over" BYTES,  LKW2TOR3 @ LBL, s" 2>r" BYTES,
   LKW2RFROM3 @ LBL, s" 2r>" BYTES,  LKW2RFET3 @ LBL, s" 2r@" BYTES,
   LKWAT2 @ LBL,    s" @" BYTES,
   LKWSTORE2 @ LBL, s" !" BYTES,
   LP2DOESW @ LBL,
   s" hb: does>-split cannot lower layout width facts: " BYTES,
   ;

also LOWER-CERT also LOWER-TXN-CODE definitions

: EMIT-DESC ( -- )
   LBL LBL LBL LBL LBL {: outer guards next done bad :} \ typed-local-lint: allow-bare-local
   VDESC-LABEL @ LBL,
   15 12 3 LSLI,  15 11 15 ADD,
   16 13 3 LSLI,  16 15 16 ADD,
   12 15 0 LDR,  12 0 CMPI,  C-LT bad BCOND,
   15 15 1 cells ADDI,
   outer LBL,  12 done CBZ,
      10 15 CHECK-CELLS cells ADDI,  10 15 CMP,  C-CC bad BCOND,
      10 16 CMP,  C-HI bad BCOND,
      17 15 0 LDR,  17 14 CMP,  C-CS bad BCOND,
      17 15 1 cells LDR,  17 0 CMPI,  C-LE bad BCOND,
      13 15 2 cells LDR,  13 0 CMPI,  C-LT bad BCOND,
      15 15 CHECK-CELLS cells ADDI,
   guards LBL,  13 next CBZ,
      10 15 GUARD-CELLS cells ADDI,  10 15 CMP,  C-CC bad BCOND,
      10 16 CMP,  C-HI bad BCOND,
      17 15 0 LDR,  17 14 CMP,  C-CS bad BCOND,
      10 15 1 cells LDR,  10 0 CMPI,  C-LT bad BCOND,
      17 15 2 cells LDR,  17 0 CMPI,  C-LE bad BCOND,
      10 17 CMP,  C-CS bad BCOND,
      15 15 GUARD-CELLS cells ADDI,  13 13 1 SUBI,  guards B,
   next LBL,  12 12 1 SUBI,  outer B,
   done LBL,  15 16 CMP,  C-NE bad BCOND,  RET,
   bad LBL,  BAD-LABEL 34 DIE ;

: EMIT-DRIFT-FAIL ( -- )
   DRIFT-FAIL-LABEL @ LBL,
   DRIFT-LABEL 37 DIE ;

previous previous definitions

\ Gforth recovery-host mirror of Habu's LOWER-TXN package.
also LOWER-CERT also LOWER-TXN definitions

4095 constant MAX-WIDTH

: PROTECT ( n -- ) {: prot :} \ typed-local-lint: allow-bare-local
   LBL {: ok :} \ typed-local-lint: allow-bare-local
   0 DATA TXN-BLOB-A-CELL LDR,
   1 DATA TXN-BLOB-CAP-CELL LDR,
   2 prot MOVZ,  NR-MPROTECT SYS,
   0 0 CMPI,  C-EQ ok BCOND,
   [ also LOWER-TXN-CODE ] MEM-LABEL 38 DIE [ previous ]
   ok LBL, ;

: MAP ( n -- ) {: cap :} \ typed-local-lint: allow-bare-local
   LBL LBL LBL {: clear mapped ok :} \ typed-local-lint: allow-bare-local
   12 DATA TXN-BLOB-A-CELL LDR,
   13 DATA TXN-BLOB-CAP-CELL LDR,  12 12 13 ORR,
   13 DATA TXN-SRC-A-CELL LDR,  12 12 13 ORR,
   13 DATA TXN-SRC-U-CELL LDR,  12 12 13 ORR,
   13 DATA TXN-CERT-A-CELL LDR,  12 12 13 ORR,
   13 DATA TXN-CERT-U-CELL LDR,  12 12 13 ORR,
   12 clear CBZ,
      [ also LOWER-TXN-CODE ] MEM-LABEL 38 DIE [ previous ]
   clear LBL,
   0 0 MOVZ,  1 cap 0 ADDI,  2 3 MOVZ,
   3 MAP-ANON-PRIVATE LIT64,  4 0 MOVN,  5 0 MOVZ,
   NR-MMAP SYS,
   13 C-CS CSET,  13 mapped CBZ,
      [ also LOWER-TXN-CODE ] MEM-LABEL 38 DIE [ previous ]
   mapped LBL,
   0 ok CBNZ,
      1 cap 0 ADDI,  NR-MUNMAP SYS,
      [ also LOWER-TXN-CODE ] MEM-LABEL 38 DIE [ previous ]
   ok LBL,
   0 DATA TXN-BLOB-A-CELL STR,
   cap DATA TXN-BLOB-CAP-CELL STR, ;

: RELEASE-MAP ( -- )
   LBL LBL LBL {: empty mapped ok :} \ typed-local-lint: allow-bare-local
   0 DATA TXN-BLOB-A-CELL LDR,
   1 DATA TXN-BLOB-CAP-CELL LDR,
   0 mapped CBNZ,
      1 empty CBZ,
      [ also LOWER-TXN-CODE ] MEM-LABEL 38 DIE [ previous ]
   mapped LBL,
   1 ok CBNZ,
      [ also LOWER-TXN-CODE ] MEM-LABEL 38 DIE [ previous ]
   ok LBL,
   NR-MUNMAP SYS,
   0 0 CMPI,  C-EQ empty BCOND,
      [ also LOWER-TXN-CODE ] MEM-LABEL 38 DIE [ previous ]
   empty LBL,
   0 0 MOVZ,
   0 DATA TXN-BLOB-A-CELL STR,
   0 DATA TXN-BLOB-CAP-CELL STR,
   0 DATA TXN-SRC-A-CELL STR,
   0 DATA TXN-SRC-U-CELL STR,
   0 DATA TXN-CERT-A-CELL STR,
   0 DATA TXN-CERT-U-CELL STR, ;

: RELEASE ( -- ) RELEASE-MAP ;
: SEAL ( -- ) 1 PROTECT ;

: COPY-BYTES ( n n n -- ) {: src len dst :} \ typed-local-lint: allow-bare-local
   LBL LBL {: loop done :} \ typed-local-lint: allow-bare-local
   loop LBL,  len done CBZ,
      17 src 0 LDRB,  17 dst 0 STRB,
      src src 1 ADDI,  dst dst 1 ADDI,  len len 1 SUBI,
      loop B,
   done LBL, ;

: ROW-END ( n n n n n -- ) {: count stride cur total bad :} \ typed-local-lint: allow-bare-local
   16 stride MOVZ,
   15 count 16 MUL,
   17 15 16 UDIV,  17 count CMP,  C-NE bad BCOND,
   16 cur 0 ADDI,  cur cur 15 ADD,
   cur 16 CMP,  C-CC bad BCOND,
   cur total CMP,  C-HI bad BCOND, ;

: SOURCE-LEN ( n -- ) {: dst :} \ typed-local-lint: allow-bare-local
   LBL LBL {: full done :} \ typed-local-lint: allow-bare-local
   dst DATA DOESB-CELL LDR,
   dst full CBZ,
      dst dst 6 SUBI,  done B,
   full LBL,  dst DATA BODYLEN-CELL LDR,
   done LBL, ;

: HASH-BODY ( -- )
   LBL LBL {: loop done :} \ typed-local-lint: allow-bare-local
   12 DATA BODYBUF-OFF ADDI,
   13 SOURCE-LEN
   14 FNV-OFFSET LIT64,
   15 FNV-PRIME LIT64,
   loop LBL,  13 done CBZ,
      17 12 0 LDRB,  14 14 17 EOR,  14 14 15 MUL,
      12 12 1 ADDI,  13 13 1 SUBI,  loop B,
   done LBL, ;

: VALIDATE-WF ( n -- ) {: bad :} \ typed-local-lint: allow-bare-local
   LBL LBL LBL LBL LBL LBL {: loop ordered flags fetch advance done :} \ typed-local-lint: allow-bare-local
   5 11 WF-COUNT-CELL cells LDR,
   13 11 HEADER-CELLS cells ADDI,
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
      15 MAX-WIDTH CMPI,  C-HI bad BCOND,
      15 1 CMPI,  C-LS flags BCOND,  9 1 MOVZ,
   flags LBL,
      17 13 3 cells LDR,
      15 17 3 ANDI,  15 17 CMP,  C-NE bad BCOND,
      17 3 CMPI,  C-EQ bad BCOND,
      15 17 FETCH-FLAG ANDI,  15 fetch CBNZ,  advance B,
   fetch LBL,  8 8 1 ADDI,
   advance LBL,
      13 13 WF-CELLS cells ADDI,
      14 14 1 ADDI,  loop B,
   done LBL,
   15 11 FETCH-COUNT-CELL cells LDR,  15 8 CMP,  C-NE bad BCOND, ;

: VALIDATE-BINDS ( n -- ) {: bad :} \ typed-local-lint: allow-bare-local
   LBL LBL {: loop done :} \ typed-local-lint: allow-bare-local
   5 11 WF-COUNT-CELL cells LDR,
   6 11 BIND-COUNT-CELL cells LDR,
   13 11 HEADER-CELLS cells ADDI,
   12 WF-CELLS cells MOVZ,
   15 5 12 MUL,  13 13 15 ADD,
   14 0 MOVZ,
   loop LBL,  14 6 CMP,  C-GE done BCOND,
      15 13 0 LDR,  15 bad CBZ,
      15 MAX-WIDTH CMPI,  C-HI bad BCOND,
      13 13 1 cells ADDI,  14 14 1 ADDI,  loop B,
   done LBL, ;

: VALIDATE-FETCHES ( n -- ) {: bad :} \ typed-local-lint: allow-bare-local
   LBL LBL LBL LBL {: loop scan linked done :} \ typed-local-lint: allow-bare-local
   5 11 WF-COUNT-CELL cells LDR,
   6 11 BIND-COUNT-CELL cells LDR,
   7 11 FETCH-COUNT-CELL cells LDR,
   8 11 FETCH-DATA-CELLS-CELL cells LDR,
   0 11 HEADER-CELLS cells ADDI,
   1 5 0 ADDI,
   12 WF-CELLS cells MOVZ,
   13 5 12 MUL,  2 0 13 ADD,
   13 6 3 LSLI,  2 2 13 ADD,
   3 HEADER-CELLS MOVZ,
   12 WF-CELLS MOVZ,
   13 5 12 MUL,  3 3 13 ADD,  3 3 6 ADD,
   12 FETCH-CELLS MOVZ,
   13 7 12 MUL,  3 3 13 ADD,
   4 10 3 LSRI,
   5 7 0 ADDI,
   loop LBL,  5 done CBZ,
   scan LBL,  1 bad CBZ,
      12 0 3 cells LDR,
      13 12 FETCH-FLAG ANDI,
      13 linked CBNZ,
      0 0 WF-CELLS cells ADDI,
      1 1 1 SUBI,  scan B,
   linked LBL,
      12 0 0 LDR,  13 2 0 LDR,  12 13 CMP,  C-NE bad BCOND,
      12 0 1 cells LDR,  12 bad CBNZ,
      14 0 2 cells LDR,
      0 0 WF-CELLS cells ADDI,  1 1 1 SUBI,
      12 2 1 cells LDR,  12 3 CMP,  C-NE bad BCOND,
      13 2 2 cells LDR,  13 bad CBZ,
      8 3 13 ADD,  8 3 CMP,  C-CC bad BCOND,
      8 4 CMP,  C-HI bad BCOND,
      [ also LOWER-TXN-CODE ] VDESC-LABEL [ previous ] @ BL,
      3 8 0 ADDI,
      2 2 FETCH-CELLS cells ADDI,
      5 5 1 SUBI,  loop B,
   done LBL,
   3 4 CMP,  C-NE bad BCOND,
   12 11 NEEDS-CELL cells LDR,  12 9 CMP,  C-NE bad BCOND, ;

: VALIDATE ( -- )
   LBL LBL {: bad done :} \ typed-local-lint: allow-bare-local
   10 HEADER-CELLS cells CMPI,  C-CC bad BCOND,
   13 11 MAGIC-CELL cells LDR,
   14 MAGIC LIT64,  13 14 CMP,  C-NE bad BCOND,
   13 11 VERSION-CELL cells LDR,
   13 VERSION CMPI,  C-NE bad BCOND,
   13 11 TOTAL-BYTES-CELL cells LDR,
   13 10 CMP,  C-NE bad BCOND,
   13 10 7 ANDI,  13 bad CBNZ,
   13 11 BODY-LEN-CELL cells LDR,
   14 SOURCE-LEN  13 14 CMP,  C-NE bad BCOND,
   HASH-BODY
   13 11 BODY-HASH-CELL cells LDR,  13 14 CMP,  C-NE bad BCOND,
   5 11 WF-COUNT-CELL cells LDR,
   6 11 BIND-COUNT-CELL cells LDR,
   7 11 FETCH-COUNT-CELL cells LDR,
   8 11 FETCH-DATA-CELLS-CELL cells LDR,
   13 HEADER-CELLS cells MOVZ,
   5 WF-CELLS cells 13 10 bad ROW-END
   6 1 cells 13 10 bad ROW-END
   7 FETCH-CELLS cells 13 10 bad ROW-END
   8 1 cells 13 10 bad ROW-END
   13 10 CMP,  C-NE bad BCOND,
   bad VALIDATE-WF
   bad VALIDATE-BINDS
   bad VALIDATE-FETCHES
   done B,
   bad LBL,  [ also LOWER-TXN-CODE ] BAD-LABEL 34 DIE [ previous ]
   done LBL, ;

: ZERO-LIVE ( -- )
   LBL LBL {: loop done :} \ typed-local-lint: allow-bare-local
   11 TXN-LIVE-W-OFF MOVZ,  11 DATA 11 ADD,
   12 TXN-LIVE-W-CAP MOVZ,  13 0 MOVZ,
   loop LBL,  12 done CBZ,
      13 11 0 STR,  11 11 1 cells ADDI,  12 12 1 SUBI,  loop B,
   done LBL, ;

: VERIFY-COUNT ( n n -- ) {: state header :} \ typed-local-lint: allow-bare-local
   LBL {: ok :} \ typed-local-lint: allow-bare-local
   11 DATA TXN-CERT-A-CELL LDR,
   12 11 header cells LDR,
   13 DATA state LDR,
   12 13 CMP,  C-EQ ok BCOND,
      [ also LOWER-TXN-CODE ] DRIFT-LABEL 37 DIE [ previous ]
   ok LBL, ;

: VERIFY-DONE ( -- )
   TXN-WF-I-CELL WF-COUNT-CELL VERIFY-COUNT
   TXN-BIND-I-CELL BIND-COUNT-CELL VERIFY-COUNT
   TXN-FETCH-I-CELL FETCH-COUNT-CELL VERIFY-COUNT ;

: FREEZE ( -- )
   LBL LBL LBL LBL {: skip copy room alloc :} \ typed-local-lint: allow-bare-local
   LCERTBYTES 16 C-P2-FIND-GLOBAL
   C-CALL-X11-SAVED
   10 G-POP  11 G-POP
   VALIDATE
   10 11 TOTAL-BYTES-CELL cells LDR,
   13 11 NEEDS-CELL cells LDR,  13 copy CBNZ,
      13 0 MOVZ,  13 DATA TXN-ACTIVE-CELL STR,  skip B,
   copy LBL,
   14 SOURCE-LEN
   15 14 9 ADDI,  15 15 $FFFFFFFFFFFFFFF8 ANDI,
   16 15 10 ADD,  16 15 CMP,  C-CS room BCOND,
      [ also LOWER-TXN-CODE ] FULL-LABEL 29 DIE [ previous ]
   room LBL,
   17 PROT-PAGE-MAX 1 - LIT64,  17 16 17 ADD,
   17 16 CMP,  C-CS alloc BCOND,
      [ also LOWER-TXN-CODE ] FULL-LABEL 29 DIE [ previous ]
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
: NEEDS? ( -- ) 10 DATA TXN-ACTIVE-CELL LDR, ;

: START ( -- )
   2 3 MOVZ,  LPROT @ BL,
   SEAL
   9 DATA INP-CELL LDR,  9 DATA P2INP-CELL STR,
   9 DATA INE-CELL LDR,  9 DATA P2INE-CELL STR,
   10 DATA TXN-SRC-A-CELL LDR,
   9 DATA P2BODY0-CELL LDR,  9 10 9 ADD,  9 DATA INP-CELL STR,
   9 DATA TXN-SRC-U-CELL LDR,  9 10 9 ADD,  9 DATA INE-CELL STR,
   11 DATA PEND-CELL LDR,  CP 11 0 LDR,
   9 DATA P2DP-CELL LDR,  9 DATA DP-CELL STR,
   5 CFSTK-OFF LIT64,  11 DBASE 5 ADD,  12 0 MOVZ,  12 11 0 STR,
   12 DATA LOCN-CELL STR,  12 DATA LOCF-CELL STR,
   C-COLON-RESET-COMPILE-STATE
   C-COLON-WORD-PROLOGUE
   9 1 MOVZ,  9 DATA P2-CELL STR, ;

previous previous definitions

: EM-P2-TRIGGER ( n -- ) {: lmain :} \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
   LBL LBL {: nowide p2ok :} \ typed-local-lint: allow-bare-local - Gforth bootstrap labels cannot use Habu type suffixes.
   9 DATA P2-CELL LDR,  9 nowide CBNZ,
   [ also LOWER-TXN ] NEEDS? [ previous ]
   10 nowide CBZ,
   10 DATA DOESB-CELL LDR,  10 p2ok CBZ,
      0 2 MOVZ,  1 LP2DOESW @ ADR,  2 49 MOVZ,  NR-WRITE SYS,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,
      NR-WRITE SYS,
      0 75 MOVZ,  NR-EXIT-GROUP SYS,
   p2ok LBL,
   [ also LOWER-TXN ] START [ previous ]
   lmain B,
   nowide LBL, ;

: EM-P2-CHECK-DEFINER ( n -- ) {: lmain :} \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
   LBL {: p2sk :} \ typed-local-lint: allow-bare-local - Gforth bootstrap labels cannot use Habu type suffixes.
   9 DATA P2-CELL LDR,  9 p2sk CBNZ,
      C-CALL-CHECK-DEFINER
      [ also LOWER-TXN ] FREEZE [ previous ]
      lmain EM-P2-TRIGGER
   p2sk LBL, ;

also LOWER-CERT also LOWER-TXN definitions

: FINISH ( -- )
   LBL {: nop2 :} \ typed-local-lint: allow-bare-local - Gforth bootstrap labels cannot use Habu type suffixes.
   9 DATA P2-CELL LDR,  9 nop2 CBZ,
   VERIFY-DONE
   9 DATA P2INP-CELL LDR,  9 DATA INP-CELL STR,
   9 DATA P2INE-CELL LDR,  9 DATA INE-CELL STR,
   RELEASE
   9 0 MOVZ,  9 DATA P2-CELL STR,
   9 DATA P2INP-CELL STR,  9 DATA P2INE-CELL STR,
   9 DATA TXN-ACTIVE-CELL STR,
   9 DATA TXN-WF-I-CELL STR,  9 DATA TXN-BIND-I-CELL STR,
   9 DATA TXN-FETCH-I-CELL STR,
   nop2 LBL, ;

previous previous definitions

: EM-P2-W-CELL ( n -- n )
   dup 0 = if drop P2W0-CELL exit then
   dup 1 = if drop P2W1-CELL exit then
   2 = if P2W2-CELL exit then
   P2W3-CELL ;

: EM-P2-QUERY-1 ( n -- ) {: pos :} \ typed-local-lint: allow-bare-local
   pos EM-P2-W-CELL {: wcell :} \ typed-local-lint: allow-bare-local
   9 DATA TKA-CELL LDR,
   10 DATA TXN-SRC-A-CELL LDR,  9 9 10 SUB,
   10 pos MOVZ,
   LP2CWAT @ BL,
   10 DATA wcell STR, ;

: EM-P2-QUERY-WIDTHS ( n -- ) {: k :} \ typed-local-lint: allow-bare-local
   LBL {: scal :} \ typed-local-lint: allow-bare-local
   0 begin dup k < while dup EM-P2-QUERY-1 1 + repeat drop
   9 DATA P2W0-CELL LDR,
   k 1 > if 10 DATA P2W1-CELL LDR,  9 9 10 ADD, then
   k 2 > if 10 DATA P2W2-CELL LDR,  9 9 10 ADD, then
   k 3 > if 10 DATA P2W3-CELL LDR,  9 9 10 ADD, then
   13 0 MOVZ,  9 k CMPI,  C-LE scal BCOND,  13 1 MOVZ,
   scal LBL, ;

variable P2SK
: P2W-ENTRY ( n n n n n n -- ) {: lmain kwvar kwlen k hxt :} \ typed-local-lint: allow-bare-local
   LBL P2SK !
   0 kwvar @ ADR,  1 kwlen MOVZ,  LKWCMP @ BL,
   0 P2SK @ CBZ,
   k EM-P2-QUERY-WIDTHS
   13 P2SK @ CBZ,
   LVSPILL @ BL,
   hxt execute
   lmain B,
   P2SK @ LBL, ;

: P2F-ENTRY ( n n n n n -- ) {: lmain kwvar kwlen hxt :} \ typed-local-lint: allow-bare-local
   LBL P2SK !
   0 kwvar @ ADR,  1 kwlen MOVZ,  LKWCMP @ BL,
   0 P2SK @ CBZ,
   1 EM-P2-QUERY-WIDTHS
   LVSPILL @ BL,
   hxt execute
   lmain B,
   P2SK @ LBL, ;

: EM-P2X-DUP ( -- )
   5 DATA P2W0-CELL LDR,  6 5 0 ADDI,  LP2COPY @ BL, ;
: EM-P2X-DROP ( -- ) 5 DATA P2W0-CELL LDR,  LP2DROPN @ BL, ;
: EM-P2X-SWAP ( -- )
   9 DATA P2W0-CELL LDR,  10 DATA P2W1-CELL LDR,
   5 9 10 ADD,  6 10 0 ADDI,  LP2ROT @ BL, ;
: EM-P2X-OVER ( -- )
   9 DATA P2W0-CELL LDR,  10 DATA P2W1-CELL LDR,
   5 10 0 ADDI,  6 9 10 ADD,  LP2COPY @ BL, ;
: EM-P2X-NIP ( -- )
   EM-P2X-SWAP  5 DATA P2W1-CELL LDR,  LP2DROPN @ BL, ;
: EM-P2X-TUCK ( -- )
   EM-P2X-SWAP
   9 DATA P2W0-CELL LDR,  10 DATA P2W1-CELL LDR,
   5 9 0 ADDI,  6 9 10 ADD,  LP2COPY @ BL, ;
: EM-P2X-ROT ( -- )
   9 DATA P2W0-CELL LDR,  10 DATA P2W1-CELL LDR,  11 DATA P2W2-CELL LDR,
   5 9 10 ADD,  5 5 11 ADD,  6 11 0 ADDI,  LP2ROT @ BL, ;
: EM-P2X-MROT ( -- )
   9 DATA P2W0-CELL LDR,  10 DATA P2W1-CELL LDR,  11 DATA P2W2-CELL LDR,
   5 9 10 ADD,  5 5 11 ADD,  6 10 11 ADD,  LP2ROT @ BL, ;
: EM-P2X-2DUP ( -- )
   9 DATA P2W0-CELL LDR,  10 DATA P2W1-CELL LDR,
   5 9 10 ADD,  6 5 0 ADDI,  LP2COPY @ BL, ;
: EM-P2X-2DROP ( -- )
   9 DATA P2W0-CELL LDR,  10 DATA P2W1-CELL LDR,
   5 9 10 ADD,  LP2DROPN @ BL, ;
: EM-P2X-2SWAP ( -- )
   9 DATA P2W0-CELL LDR,  10 DATA P2W1-CELL LDR,  11 DATA P2W2-CELL LDR,  12 DATA P2W3-CELL LDR,
   5 9 10 ADD,  5 5 11 ADD,  5 5 12 ADD,  6 11 12 ADD,  LP2ROT @ BL, ;
: EM-P2X-2OVER ( -- )
   9 DATA P2W0-CELL LDR,  10 DATA P2W1-CELL LDR,  11 DATA P2W2-CELL LDR,  12 DATA P2W3-CELL LDR,
   5 11 12 ADD,  6 9 10 ADD,  6 6 11 ADD,  6 6 12 ADD,  LP2COPY @ BL, ;
: EM-P2X-TOR ( -- ) 5 DATA P2W0-CELL LDR,  6 0 MOVZ,  LP2RS @ BL, ;
: EM-P2X-RFROM ( -- ) 5 DATA P2W0-CELL LDR,  6 1 MOVZ,  LP2RS @ BL, ;
: EM-P2X-RFETCH ( -- ) 5 DATA P2W0-CELL LDR,  6 2 MOVZ,  LP2RS @ BL, ;
: EM-P2X-2TOR ( -- )
   9 DATA P2W0-CELL LDR,  10 DATA P2W1-CELL LDR,  5 9 10 ADD,  6 0 MOVZ,  LP2RS @ BL, ;
: EM-P2X-2RFROM ( -- )
   9 DATA P2W0-CELL LDR,  10 DATA P2W1-CELL LDR,  5 9 10 ADD,  6 1 MOVZ,  LP2RS @ BL, ;
: EM-P2X-2RFET ( -- )
   9 DATA P2W0-CELL LDR,  10 DATA P2W1-CELL LDR,  5 9 10 ADD,  6 2 MOVZ,  LP2RS @ BL, ;

: EM-P2X-FETCH ( -- )
   LP2VEMIT @ BL,
   5 DATA P2W0-CELL LDR,  LP2FETCH @ BL, ;

: EM-P2X-STORE ( -- )
   5 DATA P2W0-CELL LDR,  LP2STORE @ BL, ;

: EMIT-COMPILE-P2WIDE ( n -- ) {: lmain :} \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
   LBL {: notp2 :} \ typed-local-lint: allow-bare-local
   9 DATA P2-CELL LDR,  9 notp2 CBZ,
   lmain LKWDUP2    3 1 ['] EM-P2X-DUP P2W-ENTRY
   lmain LKWDROP2   4 1 ['] EM-P2X-DROP P2W-ENTRY
   lmain LKWSWAP2   4 2 ['] EM-P2X-SWAP P2W-ENTRY
   lmain LKWOVER2   4 2 ['] EM-P2X-OVER P2W-ENTRY
   lmain LKWNIP2    3 2 ['] EM-P2X-NIP P2W-ENTRY
   lmain LKWTUCK3   4 2 ['] EM-P2X-TUCK P2W-ENTRY
   lmain LKWROT3    3 3 ['] EM-P2X-ROT P2W-ENTRY
   lmain LKWMROT3   4 3 ['] EM-P2X-MROT P2W-ENTRY
   lmain LKW2DUP3   4 2 ['] EM-P2X-2DUP P2W-ENTRY
   lmain LKW2DROP3  5 2 ['] EM-P2X-2DROP P2W-ENTRY
   lmain LKW2SWAP3  5 4 ['] EM-P2X-2SWAP P2W-ENTRY
   lmain LKW2OVER3  5 4 ['] EM-P2X-2OVER P2W-ENTRY
   lmain LKWTOR     2 1 ['] EM-P2X-TOR P2W-ENTRY
   lmain LKWRFROM   2 1 ['] EM-P2X-RFROM P2W-ENTRY
   lmain LKWRFET    2 1 ['] EM-P2X-RFETCH P2W-ENTRY
   lmain LKW2TOR3   3 2 ['] EM-P2X-2TOR P2W-ENTRY
   lmain LKW2RFROM3 3 2 ['] EM-P2X-2RFROM P2W-ENTRY
   lmain LKW2RFET3  3 2 ['] EM-P2X-2RFET P2W-ENTRY
   lmain LKWAT2     1 ['] EM-P2X-FETCH P2F-ENTRY
   lmain LKWSTORE2  1 1 ['] EM-P2X-STORE P2W-ENTRY
   notp2 LBL, ;

: C-TRUSTED ( -- )
   2 3 MOVZ,  LPROT @ BL,
   C-COLON-CODE-ROOM
   C-COLON-DICT-ROOM
   C-COLON-PENDING-DREC
   C-CLEAR-TRUSTED-STATE
   12 1 MOVZ,  12 DATA TRUSTED-CELL STR,
   C-PARSE-TRUST-SIG
   C-COLON-RESET-COMPILE-STATE
   C-COLON-WORD-PROLOGUE ;

: EMIT-INTERPRET-COLON ( n n -- ) {: lmain lnotcolon :}
   lnotcolon C-COLON-TOKEN?
      LBL {: p2ok :} \ typed-local-lint: allow-bare-local
      9 DATA P2-CELL LDR,  9 p2ok CBZ,
         0 2 MOVZ,  1 LP2NEST @ ADR,  2 33 MOVZ,  NR-WRITE SYS,
         0 76 MOVZ,  NR-EXIT-GROUP SYS,
      p2ok LBL,
      2 3 MOVZ,  LPROT @ BL,
      C-COLON-CODE-ROOM
      C-COLON-DICT-ROOM
      C-COLON-PENDING-DREC
      C-COLON-MAYBE-SIG
      9 DATA DP-CELL LDR,  9 DATA P2DP-CELL STR,
      9 DATA BODYLEN-CELL LDR,  9 DATA P2BODY0-CELL STR,
      C-COLON-RESET-COMPILE-STATE
      C-COLON-WORD-PROLOGUE
      lmain B,
   lnotcolon LBL, ;

: EMIT-INTERPRET-WORDS ( n n -- ) {: lmain lundef :}
   LBL {: lnotnum :}
   lmain LKWTRUSTED 8 ['] C-TRUSTED CF-ENTRY
   lmain LKWPACKAGE 7 ['] C-PACKAGE CF-ENTRY
   lmain LKWPUBLIC 6 ['] C-PUBLIC CF-ENTRY
   lmain LKWPRIVATE 7 ['] C-PRIVATE CF-ENTRY
   lmain LKWSEMIPACKAGE 8 ['] C-END-PACKAGE CF-ENTRY
   lmain LKWCREATE 6 ['] C-CREATE   CF-ENTRY
   lmain LKWVAR    8 ['] C-VARIABLE CF-ENTRY
   lmain LKWCONST  8 ['] C-CONSTANT CF-ENTRY
   lmain LKWTICK   1 ['] C-TICK     CF-ENTRY
   lmain LKWCHAR   4 ['] C-CHAR     CF-ENTRY
   lmain LKWIMM    9 ['] C-IMMEDIATE CF-ENTRY
   lmain LKWSQ     2 ['] C-ISDQ     CF-ENTRY
   lmain LKWCQ     2 ['] C-ICQ      CF-ENTRY
   lmain LKWDOTQ   2 ['] C-IDOTQ    CF-ENTRY
   lmain LKWESQ    3 ['] C-EISDQ    CF-ENTRY
   lmain LKWECQ    3 ['] C-EICQ     CF-ENTRY
   lmain LKWEDOTQ  3 ['] C-EIDOTQ   CF-ENTRY
   9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LNUM @ BL,
   12 lnotnum CBZ,  11 G-PUSH  lmain B,
   lnotnum LBL,
   9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFIND @ BL,
   13 lundef CBZ,
   LBL {: lwide :} \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
   14 13 8 ANDI,  14 lwide CBNZ,        \ DNAME-WIDE gate (mirror of EM-INTERPRET-FIND; inert in stage0)
   11 BLR,  lmain B,
   lwide LBL,  0 70 MOVZ,  NR-EXIT-GROUP SYS, ;

: EMIT-INTERPRET ( n n -- ) {: lmain lundef :}
   LBL {: lnotcolon :}
   lmain lnotcolon EMIT-INTERPRET-COLON
   lmain lundef EMIT-INTERPRET-WORDS ;

: EMIT-COMPILE-DROP-LOCALS ( -- )
   LBL {: done :}
   12 DATA LOCF-CELL LDR,  12 done CBZ,
      9 $910003FF LIT64,  14 12 10 LSLI,  9 9 14 ORR,  LCEMIT @ BL,
   done LBL, ;

: EMIT-COMPILE-RET ( -- )
   9 $F94003FE LIT64,  LCEMIT @ BL,
   9 $910043FF LIT64,  LCEMIT @ BL,
   9 W-RET LIT64,  LCEMIT @ BL, ;

: EMIT-COMPILE-FLUSH-PEND ( -- )
   11 DATA PEND-CELL LDR,
   9 11 0 LDR,  10 CP 9 SUB,  10 10 4 SUBI,  10 11 8 STR,
   2 5 MOVZ,  LPROT @ BL,  LFLUSH @ BL, ;

: EMIT-COMPILE-PUBLISH-TRUSTED ( n -- ) {: lmain :} \ typed-local-lint: allow-bare-local
   LBL LBL LBL {: ttrusted ndhas ndchk :} \ typed-local-lint: allow-bare-local
   10 DATA TRUSTED-CELL LDR,  10 ttrusted CBNZ,
      lmain EM-P2-CHECK-DEFINER
   ttrusted LBL,
   10 DATA TCSIG-U-CELL LDR,  10 ndhas CBNZ,
   10 DATA DOESB-CELL LDR,  10 ndchk CBZ,
      C-DIE-DOES
   ndhas LBL,
   10 DATA DOESB-CELL LDR,  10 ndchk CBZ,
      C-CALL-CHECK-DOES
   ndchk LBL,
   C-CALL-TRUST-PEND
   NDICT NDICT 1 ADDI,
   EM-REC-WIDE-PUBLISH
   [ also LOWER-TXN ] FINISH [ previous ]
   C-CLEAR-TRUSTED-STATE
   9 0 MOVZ,  9 DATA PEND-CELL STR,
   lmain B, ;

: EMIT-COMPILE-PUBLISH-HOOKED ( n -- ) {: lmain :} \ typed-local-lint: allow-bare-local
   LBL LBL {: nohook rejected :} \ typed-local-lint: allow-bare-local
   9 DATA P2-CELL LDR,  9 nohook CBNZ,
   9 DATA HOOK-CELL LDR,  9 nohook CBZ,
      10 DATA BODYBUF-OFF ADDI,  10 G-PUSH
      10 DATA BODYLEN-CELL LDR,  10 G-PUSH
      SP SP 16 SUBI,  30 SP 0 STR,  9 BLR,  30 SP 0 LDR,  SP SP 16 ADDI,
      10 G-POP  10 rejected CBZ,
      [ also LOWER-TXN ] FREEZE [ previous ]
      lmain EM-P2-TRIGGER
   nohook LBL,
      NDICT NDICT 1 ADDI,
      EM-REC-WIDE-PUBLISH
   rejected LBL,
   [ also LOWER-TXN ] FINISH [ previous ]
   C-CLEAR-TRUSTED-STATE
   9 0 MOVZ,  9 DATA PEND-CELL STR,
   lmain B, ;

: EMIT-COMPILE-PUBLISH ( n -- ) {: lmain :} \ typed-local-lint: allow-bare-local
   LBL LBL {: checked unsigned :} \ typed-local-lint: allow-bare-local
   9 DATA HOOK-CELL LDR,  9 checked CBNZ,
      lmain EMIT-COMPILE-PUBLISH-HOOKED
   checked LBL,
   9 DATA TSIG-U-CELL LDR,  9 unsigned CBZ,
      lmain EMIT-COMPILE-PUBLISH-TRUSTED
   unsigned LBL,
   lmain EMIT-COMPILE-PUBLISH-HOOKED ;

: EMIT-COMPILE-SEMI ( n n -- ) {: lmain lnotsemi :}
   9 DATA TKL-CELL LDR,  9 1 CMPI,  C-NE lnotsemi BCOND,
   9 DATA TKA-CELL LDR,  9 9 0 LDRB,  9 59 CMPI,  C-NE lnotsemi BCOND,
      LVSPILL @ BL,
      EMIT-COMPILE-DROP-LOCALS
      14 CP 0 ADDI,  9 DATA EXITH-CELL LDR,  LBCHAIN @ BL,
      EMIT-COMPILE-RET
      EMIT-COMPILE-FLUSH-PEND
      lmain EMIT-COMPILE-PUBLISH
   lnotsemi LBL, ;

: EMIT-COMPILE-CONTROL-KEYWORDS ( n -- ) {: lmain :}
   lmain LKWIF     2 ['] J-IF   ['] J-IFR    CFB-ENTRY
   lmain LKWTHEN   4 ['] J-THEN   CF-ENTRY
   lmain LKWELSE   4 ['] J-ELSE   CF-ENTRY
   lmain LKWBEGIN  5 ['] J-BEGIN  CFN-ENTRY
   lmain LKWUNTIL  5 ['] J-UNTIL ['] J-UNTILR CFBN-ENTRY
   lmain LKWAGAIN  5 ['] J-AGAIN  CFN-ENTRY
   lmain LKWWHILE  5 ['] J-WHILE ['] J-WHILER CFB-ENTRY
   lmain LKWREPEAT 6 ['] J-REPEAT CFN-ENTRY
   lmain LKWCASE   4 ['] J-CASE   CFN-ENTRY
   lmain LKWOF     2 ['] J-OF     CF-ENTRY
   lmain LKWENDOF  5 ['] J-ENDOF  CF-ENTRY
   lmain LKWENDCASE 7 ['] J-ENDCASE CF-ENTRY
   lmain LKWCONSTRUCT 9 ['] J-CONSTRUCT CFN-ENTRY
   lmain LKWMATCH 5 ['] J-MATCH CF-ENTRY ;

: EMIT-COMPILE-STRING-KEYWORDS ( n -- ) {: lmain :}
   lmain LKWSQ     2 ['] C-SDQ    CF-ENTRY
   lmain LKWCQ     2 ['] C-CQ     CF-ENTRY
   lmain LKWDOTQ   2 ['] C-DOTQ   CF-ENTRY
   lmain LKWESQ    3 ['] C-ESDQ   CF-ENTRY
   lmain LKWECQ    3 ['] C-ECQ    CF-ENTRY
   lmain LKWEDOTQ  3 ['] C-EDOTQ  CF-ENTRY ;

: EMIT-COMPILE-META-KEYWORDS ( n -- ) {: lmain :}
   lmain LKWBTICK  3 ['] C-BTICK  CF-ENTRY
   lmain LKWBCHAR  6 ['] C-BCHAR  CF-ENTRY
   lmain LKWPOST   8 ['] C-POSTPONE CF-ENTRY
   lmain LKWDOES   5 ['] J-DOES     CF-ENTRY
   lmain LKWQUOT   2 ['] J-QUOT     CF-ENTRY
   lmain LKWSEMIQ  2 ['] J-SEMIQUOT CF-ENTRY ;

: EMIT-COMPILE-LOOP-KEYWORDS ( n -- ) {: lmain :}
   lmain LKWDO     2 ['] J-DO     CF-ENTRY
   lmain LKWLOOP   4 ['] J-LOOP   CF-ENTRY
   lmain LKWI      1 ['] J-I      CF-ENTRY
   lmain LKWTOR    2 ['] J-TOR    CF-ENTRY
   lmain LKWRFROM  2 ['] J-RFROM  CF-ENTRY
   lmain LKWRFET   2 ['] J-RFETCH CF-ENTRY
   lmain LKWEXIT   4 ['] J-EXIT    CF-ENTRY
   lmain LKWREC    7 ['] J-RECURSE CF-ENTRY
   lmain LKWQDO    3 ['] J-?DO     CF-ENTRY
   lmain LKWPLOOP  5 ['] J-+LOOP   CF-ENTRY
   lmain LKWJ      1 ['] J-J       CF-ENTRY
   lmain LKWLEAVE  5 ['] J-LEAVE   CF-ENTRY
   lmain LKWUNLOOP 6 ['] J-UNLOOP  CF-ENTRY
   lmain LKWLBRACE 2 ['] C-LBRACE CF-ENTRY ;

: EMIT-COMPILE-KEYWORDS ( n -- ) {: lmain :}
   LBCAP @ BL,
   lmain EMIT-COMPILE-CONTROL-KEYWORDS
   lmain EMIT-COMPILE-STRING-KEYWORDS
   lmain EMIT-COMPILE-META-KEYWORDS
   lmain EMIT-COMPILE-LOOP-KEYWORDS ;

: EMIT-COMPILE-LOCAL ( n -- ) {: lmain :} \ typed-local-lint: allow-bare-local
   LBL LBL LBL {: notloc lmem qok :} \ typed-local-lint: allow-bare-local
   LBL {: p1c :} \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
   LLOC-FIND @ BL,  0 0 CMPI,  C-LT notloc BCOND,
      LBCAP @ BL,
      11 DATA QPATCH-CELL LDR,  11 qok CBZ,
         0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
         0 75 MOVZ,  NR-EXIT-GROUP SYS,
      qok LBL,
      9 DATA P2-CELL LDR,  9 p1c CBZ,
         EM-P2-LOCREF
         lmain B,
      p1c LBL,
      LVRALLOC @ BL,  14 lmem CBZ,
      7 DATA LOCF-CELL LDR,  7 7 3 LSRI,  7 7 0 SUB,  7 7 1 SUBI,
      9 $F94003E0 LIT64,  9 9 14 ORR,  7 7 10 LSLI,  9 9 7 ORR,  LCEMIT @ BL,
      LVPUSHR @ BL,
      lmain B,
      lmem LBL,
      LVSPILL @ BL,
      7 DATA LOCF-CELL LDR,  7 7 3 LSRI,  7 7 0 SUB,  7 7 1 SUBI,
      9 $F94003E9 LIT64,  7 7 10 LSLI,  9 9 7 ORR,  LCEMIT @ BL,
      9 W-PUSH0 LIT64,  LCEMIT @ BL,  9 W-PUSH1 LIT64,  LCEMIT @ BL,
      lmain B,
   notloc LBL, ;

: EMIT-COMPILE-LITERAL ( n -- ) {: lmain :}
   LBL LBL {: lcnotnum lcflt :}
   9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LNUM @ BL,
   12 lcnotnum CBZ,
   2 lcflt CBNZ,
      LVPUSHC @ BL,  lmain B,
   lcflt LBL,
      LVPUSHF @ BL,  lmain B,
   lcnotnum LBL, ;

: EMIT-COMPILE-ARITH-OPS ( n -- ) {: lmain :}
   lmain LKWPLUS  1 ['] VF+ ['] E+ ['] EI+ VOPI-ENTRY
   lmain LKWMINUS 1 ['] VF- ['] E- ['] EI- VOPI-ENTRY
   lmain LKWSTAR  1 ['] VF* ['] E* VOP-ENTRY
   lmain LKWAND2  3 ['] FAND ['] EAND VOP-ENTRY
   lmain LKWOR2   2 ['] FOR2 ['] EOR2 VOP-ENTRY
   lmain LKWXOR2  3 ['] FXOR2 ['] EXOR VOP-ENTRY ;

: EMIT-COMPILE-SHUFFLE-OPS ( n -- ) {: lmain :}
   lmain LKWDUP2  3 1 ['] XDUP  VSHUF-ENTRY
   lmain LKWDROP2 4 1 ['] XDROP VSHUF-ENTRY
   lmain LKWSWAP2 4 2 ['] XSWAP VSHUF-ENTRY
   lmain LKWOVER2 4 2 ['] XOVER VSHUF-ENTRY
   lmain LKWNIP2  3 2 ['] XNIP  VSHUF-ENTRY ;

: EMIT-COMPILE-COMPARE-OPS ( n -- ) {: lmain :}
   lmain LKWEQ2 1 0 VCMP-ENTRY
   lmain LKWNE2 2 1 VCMP-ENTRY
   lmain LKWLT2 1 11 VCMP-ENTRY
   lmain LKWGT2 1 12 VCMP-ENTRY
   lmain LKWLE2 2 13 VCMP-ENTRY
   lmain LKWGE2 2 10 VCMP-ENTRY ;

: EMIT-COMPILE-UNARY-OPS ( n -- ) {: lmain :}
   lmain LKWINC  2 ['] FU1+ ['] EU1+ VUN-ENTRY
   lmain LKWDEC  2 ['] FU1- ['] EU1- VUN-ENTRY
   lmain LKWZEQ  2 ['] FU0= ['] EU0= VUN-ENTRY
   lmain LKWZLT  2 ['] FU0< ['] EU0< VUN-ENTRY
   lmain LKWNEG2 6 ['] FUNEG ['] EUNEG VUN-ENTRY
   lmain LKWINV2 6 ['] FUINV ['] EUINV VUN-ENTRY ;

: EMIT-COMPILE-FLOAT-OPS ( n -- ) {: lmain :}
   lmain LKWFPLUS  2 $1E602800 FOP-ENTRY
   lmain LKWFMINUS 2 $1E603800 FOP-ENTRY
   lmain LKWFSTAR  2 $1E600800 FOP-ENTRY
   lmain LKWFSLASH 2 $1E601800 FOP-ENTRY ;

: EMIT-COMPILE-OPS ( n -- ) {: lmain :}
   lmain EMIT-COMPILE-ARITH-OPS
   lmain EMIT-COMPILE-SHUFFLE-OPS
   lmain EMIT-COMPILE-COMPARE-OPS
   lmain EMIT-COMPILE-UNARY-OPS
   lmain EMIT-COMPILE-FLOAT-OPS ;

: EMIT-COMPILE-CALL ( n n -- ) {: lmain lundef :}
   LBL {: notimm :}
   LVSPILL @ BL,
   9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFIND @ BL,
   13 lundef CBZ,
   14 13 2 ANDI,  14 notimm CBZ,
      SP SP 16 SUBI,  30 SP 0 STR,  11 SP 8 STR,
      2 5 MOVZ,  LPROT @ BL,
      11 SP 8 LDR,  11 BLR,
      2 3 MOVZ,  LPROT @ BL,
      30 SP 0 LDR,  SP SP 16 ADDI,
      lmain B,
   notimm LBL,
   C-CALL  lmain B, ;

\ construct lowering (TFAM 10 slice 2; mirrors habu2.f C-FIND-GLOBAL and the
\ EM-ADT-CON-* operand steps. The bridge names are global compiler services, so
\ a plain LFIND is the exact lookup). CMM=1: family operand -> eager resolve via
\ tfl-con-fam?, stash id in CMFAM-CELL, arm CMM=2. CMM=2: variant operand ->
\ tfl-cvar? gives ( tag pads ok ); emit pads x VS-constant 0 pushes + tag push
\ (LVPUSHC, the generated-constructor literal path), mode off. Resolution
\ failure dies fail-closed at its token: named message + token + newline, 70.
\ find a global word by kwdata name or die 70
: C-FIND-GLOBAL ( n n -- ) {: lvar len :} \ typed-local-lint: allow-bare-local
   LBL {: ok :} \ typed-local-lint: allow-bare-local
   9 lvar @ ADR,  10 len MOVZ,  LFIND @ BL,
   13 ok CBNZ,
      0 2 MOVZ,  1 lvar @ ADR,  2 len MOVZ,  NR-WRITE SYS,
      0 70 MOVZ,  NR-EXIT-GROUP SYS,
   ok LBL, ;

\ Region protection: the compile loop holds the code REGION RW mid-body, but
\ the TFL bridge targets are checker words compiled INTO that region — an RX
\ window (LPROT 5) brackets the find + call, back to RW (LPROT 3) before any
\ emission resumes (mirrors habu2.f; DATA stores are outside the flipped
\ region, die legs exit the process).
\ CMM=1 leg: resolve family, arm state 2
: EM-ADT-CON-FAM ( n -- ) {: lmain :} \ typed-local-lint: allow-bare-local
   \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
   LBL LBL {: fmsg fok :}
   LBCAP @ BL,                          \ operand reaches the checker's body too
   2 5 MOVZ,  LPROT @ BL,               \ region -> RX: checker-call window
   LTFLCONFAM 12 C-FIND-GLOBAL
   9 DATA TKA-CELL LDR,  9 G-PUSH
   9 DATA TKL-CELL LDR,  9 G-PUSH
   C-CALL-X11-SAVED
   10 G-POP
   9 G-POP
   10 fok CBNZ,
      0 2 MOVZ,  1 fmsg ADR,  2 31 MOVZ,  NR-WRITE SYS,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 2 MOVZ,  1 LQNL @ ADR,  1 1 1 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
      0 70 MOVZ,  NR-EXIT-GROUP SYS,
   fmsg LBL,  s" hb: construct: unknown family: " BYTES,
   fok LBL,
   9 DATA CMFAM-CELL STR,
   12 2 MOVZ,  12 DATA CMM-CELL STR,
   2 3 MOVZ,  LPROT @ BL,               \ region -> RW: resume emission
   lmain B, ;

: EM-ADT-CON-PUSHES ( -- )              \ pads x 0 + tag as VS constants (x12=pads, x13=tag)
   \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
   LBL LBL {: ploop pdone :}
   SP SP 16 SUBI,  12 SP 0 STR,  13 SP 8 STR,   \ frame the counters: LVPUSHC may
   2 3 MOVZ,  LPROT @ BL,                       \ spill (emission -> region RW first)
   ploop LBL,
      12 SP 0 LDR,  12 pdone CBZ,
      11 0 MOVZ,  LVPUSHC @ BL,
      12 SP 0 LDR,  12 12 1 SUBI,  12 SP 0 STR,  ploop B,
   pdone LBL,
   11 SP 8 LDR,  LVPUSHC @ BL,
   SP SP 16 ADDI, ;

\ CMM=2 leg: resolve variant, emit, mode off
: EM-ADT-CON-VAR ( n -- ) {: lmain :} \ typed-local-lint: allow-bare-local
   \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
   LBL LBL {: vmsg vok :}
   LBCAP @ BL,                          \ operand reaches the checker's body too
   2 5 MOVZ,  LPROT @ BL,               \ region -> RX: checker-call window
   LTFLCVAR 9 C-FIND-GLOBAL
   9 DATA TKA-CELL LDR,  9 G-PUSH
   9 DATA TKL-CELL LDR,  9 G-PUSH
   9 DATA CMFAM-CELL LDR,  9 G-PUSH
   C-CALL-X11-SAVED
   10 G-POP
   12 G-POP
   13 G-POP
   10 vok CBNZ,
      0 2 MOVZ,  1 vmsg ADR,  2 32 MOVZ,  NR-WRITE SYS,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 2 MOVZ,  1 LQNL @ ADR,  1 1 1 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
      0 70 MOVZ,  NR-EXIT-GROUP SYS,
   vmsg LBL,  s" hb: construct: unknown variant: " BYTES,
   vok LBL,
   EM-ADT-CON-PUSHES                    \ frames the counters, then flips back to RW
   12 0 MOVZ,  12 DATA CMM-CELL STR,
   lmain B, ;

\ MATCH lowering legs (TFAM 10 slice 3; mirrors habu2.f EM-ADT-MATCH-*/EM-MATCH-
\ SEMI/C-DIE-BAD-TAG). CMM=3/4/5 continue the token machine J-MATCH armed: 3 =
\ want family, 4 = want variant-or-`;match`, 5 = want `of`. Each opens an RX
\ window (LPROT 5 -> 3) around the checker-friend bridge call and LBCAPs its
\ operand; resolution failure inlines the write-token-newline-exit die (70).

\ Emit the invalid-tag die INLINE into the user word (mirrors habu2.f): jump over
\ the message, "hb: bad <family> tag\n" copied inline, then a self-contained
\ write(2)+exit_group(E-BAD-TAG). x11=name addr, x12=name len. Region RW.
: C-DIE-BAD-TAG ( -- )
   \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
   LBL LBL LBL LBL LBL LBL {: p1 p2 s1 s2 t1 t2 :}
   SP SP $20 SUBI,  11 SP 0 STR,  12 SP 8 STR,
   15 CP 0 ADDI,  15 SP 16 STR,
   9 $14000000 LIT64,  LCEMIT @ BL,
   16 CP 0 ADDI,  16 SP 24 STR,
   11 LBADTAGPFX @ ADR,  9 8 MOVZ,
   p1 LBL,  9 p2 CBZ,  14 11 0 LDRB,  14 28 0 STRB,  28 28 1 ADDI,  11 11 1 ADDI,  9 9 1 SUBI,  p1 B,
   p2 LBL,
   11 SP 0 LDR,  9 SP 8 LDR,
   s1 LBL,  9 s2 CBZ,  14 11 0 LDRB,  14 28 0 STRB,  28 28 1 ADDI,  11 11 1 ADDI,  9 9 1 SUBI,  s1 B,
   s2 LBL,
   11 LBADTAGSFX @ ADR,  9 5 MOVZ,
   t1 LBL,  9 t2 CBZ,  14 11 0 LDRB,  14 28 0 STRB,  28 28 1 ADDI,  11 11 1 ADDI,  9 9 1 SUBI,  t1 B,
   t2 LBL,
   28 28 3 ADDI,  5 -4 LIT64,  28 28 5 AND,
   9 SP 16 LDR,  LPAT @ BL,
   $D2800040 C-EMITW
   11 SP 24 LDR,  5 11 28 SUB,  8 $10000001 LIT64,
      6 3 MOVZ,  7 5 6 AND,  7 7 29 LSLI,  8 8 7 ORR,
      7 5 2 LSRI,  6 $7FFFF LIT64,  7 7 6 AND,  7 7 5 LSLI,  8 8 7 ORR,
      9 8 0 ADDI,  LCEMIT @ BL,
   14 SP 8 LDR,  14 14 13 ADDI,
   9 $D2800002 LIT64,  14 14 5 LSLI,  9 9 14 ORR,  LCEMIT @ BL,
   9 SYS-EMIT-WRITE LIT64,  LCEMIT @ BL,
   SYS-EMIT-SVC C-EMITW
   9 $D2800000 E-BAD-TAG 32 * + LIT64,  LCEMIT @ BL,
   9 SYS-EMIT-EXIT LIT64,  LCEMIT @ BL,
   SYS-EMIT-SVC C-EMITW
   SP SP $20 ADDI, ;

: EM-MATCH-SEMI ( n -- ) {: lmain :} \ typed-local-lint: allow-bare-local
   \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
   LBL LBL {: jl jd :}
   2 5 MOVZ,  LPROT @ BL,
   LTFLNAME 10 C-FIND-GLOBAL
   14 DATA CMFRD-CELL LDR,  14 14 1 SUBI,  14 14 3 LSLI,  14 14 CMFR-OFF ADDI,  15 DATA 14 ADD,  9 15 0 LDR,
   9 G-PUSH
   C-CALL-X11-SAVED
   12 G-POP
   11 G-POP
   SP SP 16 SUBI,  11 SP 0 STR,  12 SP 8 STR,
   2 3 MOVZ,  LPROT @ BL,
   11 SP 0 LDR,  12 SP 8 LDR,  SP SP 16 ADDI,
   C-DIE-BAD-TAG
   jl LBL,
      LCFPOP @ BL,
      9 jd CBZ,
      LPAT @ BL,
      jl B,
   jd LBL,
   14 DATA CMFRD-CELL LDR,  14 14 1 SUBI,  14 DATA CMFRD-CELL STR,
   12 0 MOVZ,  12 DATA CMM-CELL STR,
   lmain B, ;

: EM-ADT-MATCH-FAM ( n -- ) {: lmain :} \ typed-local-lint: allow-bare-local
   \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
   LBL LBL {: fmsg fok :}
   LBCAP @ BL,
   2 5 MOVZ,  LPROT @ BL,
   LTFLMATCHFAM 14 C-FIND-GLOBAL
   9 DATA TKA-CELL LDR,  9 G-PUSH
   9 DATA TKL-CELL LDR,  9 G-PUSH
   C-CALL-X11-SAVED
   10 G-POP
   9 G-POP
   10 fok CBNZ,
      0 2 MOVZ,  1 fmsg ADR,  2 27 MOVZ,  NR-WRITE SYS,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 2 MOVZ,  1 LQNL @ ADR,  1 1 1 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
      0 70 MOVZ,  NR-EXIT-GROUP SYS,
   fmsg LBL,  s" hb: match: unknown family: " BYTES,
   fok LBL,
   14 DATA CMFRD-CELL LDR,  14 14 1 SUBI,  14 14 3 LSLI,  14 14 CMFR-OFF ADDI,  15 DATA 14 ADD,  9 15 0 STR,
   12 4 MOVZ,  12 DATA CMM-CELL STR,
   2 3 MOVZ,  LPROT @ BL,
   lmain B, ;

: EM-ADT-MATCH-VAR ( n -- ) {: lmain :} \ typed-local-lint: allow-bare-local
   \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
   LBL LBL LBL {: notsemi vmsg vok :}
   LBCAP @ BL,
   0 LKWSEMIMATCH @ ADR,  1 6 MOVZ,  LKWCMP @ BL,
   0 notsemi CBZ,
      lmain EM-MATCH-SEMI
   notsemi LBL,
   2 5 MOVZ,  LPROT @ BL,
   LTFLCVAR 9 C-FIND-GLOBAL
   9 DATA TKA-CELL LDR,  9 G-PUSH
   9 DATA TKL-CELL LDR,  9 G-PUSH
   14 DATA CMFRD-CELL LDR,  14 14 1 SUBI,  14 14 3 LSLI,  14 14 CMFR-OFF ADDI,  15 DATA 14 ADD,  9 15 0 LDR,  9 G-PUSH
   C-CALL-X11-SAVED
   10 G-POP
   12 G-POP
   13 G-POP
   10 vok CBNZ,
      0 2 MOVZ,  1 vmsg ADR,  2 28 MOVZ,  NR-WRITE SYS,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 2 MOVZ,  1 LQNL @ ADR,  1 1 1 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
      0 70 MOVZ,  NR-EXIT-GROUP SYS,
   vmsg LBL,  s" hb: match: unknown variant: " BYTES,
   vok LBL,
   13 DATA CMTAG-CELL STR,
   12 DATA CMPADS-CELL STR,
   12 5 MOVZ,  12 DATA CMM-CELL STR,
   2 3 MOVZ,  LPROT @ BL,
   lmain B, ;

: EM-ADT-MATCH-OF ( n -- ) {: lmain :} \ typed-local-lint: allow-bare-local
   \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
   LBL LBL {: emsg eok :}
   LBCAP @ BL,
   0 LKWOF @ ADR,  1 2 MOVZ,  LKWCMP @ BL,
   0 eok CBNZ,
      0 2 MOVZ,  1 emsg ADR,  2 24 MOVZ,  NR-WRITE SYS,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 2 MOVZ,  1 LQNL @ ADR,  1 1 1 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
      0 70 MOVZ,  NR-EXIT-GROUP SYS,
   emsg LBL,  s" hb: match: expected of: " BYTES,
   eok LBL,
   14 DATA CMTAG-CELL LDR,
   9 $D2800010 LIT64,  14 14 5 LSLI,  9 9 14 ORR,  LCEMIT @ BL,   \ movz x16, #tag
   $F85F8269 C-EMITW
   $EB10013F C-EMITW
   $9A9F17E9 C-EMITW
   C-PUSHCP
   $B4000009 C-EMITW
   14 DATA CMPADS-CELL LDR,  14 14 1 ADDI,  14 14 3 LSLI,
   9 $D1000273 LIT64,  14 14 10 LSLI,  9 9 14 ORR,  LCEMIT @ BL,   \ sub x19,x19,#(8*(1+pads))
   14 DATA CMBK-CELL LDR,  14 14 1 LSLI,  14 14 1 ORRI,  14 DATA CMBK-CELL STR,
   12 0 MOVZ,  12 DATA CMM-CELL STR,
   lmain B, ;

: EMIT-COMPILE-ADT-MODE ( n -- ) {: lmain :} \ typed-local-lint: allow-bare-local
   \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
   LBL LBL LBL LBL LBL {: s2 s3 s4 s5 off :} \ typed-local-lint: allow-bare-local
   9 DATA CMM-CELL LDR,  9 off CBZ,
   9 2 CMPI,  C-EQ s2 BCOND,
   9 3 CMPI,  C-EQ s3 BCOND,
   9 4 CMPI,  C-EQ s4 BCOND,
   9 5 CMPI,  C-EQ s5 BCOND,
   lmain EM-ADT-CON-FAM
   s2 LBL,  lmain EM-ADT-CON-VAR
   s3 LBL,  lmain EM-ADT-MATCH-FAM
   s4 LBL,  lmain EM-ADT-MATCH-VAR
   s5 LBL,  lmain EM-ADT-MATCH-OF
   off LBL, ;

: EMIT-COMPILE ( n n -- ) {: lmain lundef :}
   LBL {: lnotsemi :}
   lmain EMIT-COMPILE-ADT-MODE
   lmain lnotsemi EMIT-COMPILE-SEMI
   lmain EMIT-COMPILE-LOCAL
   lmain EMIT-COMPILE-P2WIDE
   lmain EMIT-COMPILE-KEYWORDS
   lmain EMIT-COMPILE-LITERAL
   lmain EMIT-COMPILE-OPS
   lmain lundef EMIT-COMPILE-CALL ;

: EMIT-RESET-COMPILE-STATE ( -- )
   9 0 MOVZ,
   9 DATA RSP-CELL STR,  9 DATA HND-CELL STR,  9 DATA LOOPSP-CELL STR,
   9 DATA LVD-CELL STR,  9 DATA VSP-CELL STR,  9 DATA QPATCH-CELL STR,
   9 DATA LOCN-CELL STR,  9 DATA BODYLEN-CELL STR,  9 DATA EXITH-CELL STR,
   9 DATA PEND-CELL STR,  9 DATA CMM-CELL STR,
   9 DATA CMFRD-CELL STR,  9 DATA CMBK-CELL STR,
   9 VRALL MOVZ,  9 DATA VRFREE-CELL STR, ;

: EMIT-EVAL-UNDEF-ROLLBACK ( -- )
   9 DATA EVALD-CELL LDR,  9 9 1 SUBI,  9 DATA EVALD-CELL STR,
   9 14 15 C-EVAL-FRAME-ADDR
   CP 14 40 LDR,  NDICT 14 48 LDR,  XDS 14 32 LDR,
   9 14 56 LDR,  9 DATA DP-CELL STR,
   EMIT-RESET-COMPILE-STATE
   9 14 0 LDR,  9 DATA INP-CELL STR,
   9 14 8 LDR,  9 DATA INE-CELL STR,
   9 1 MOVZ,  9 DATA EVALERR-CELL STR,
   9 14 24 LDR,  SP 9 0 ADDI,
   9 14 16 LDR,  9 BR, ;

: EMIT-REPL-RECOVER ( -- )
   LRREC @ LBL,
   0 2 MOVZ,  1 LQNL @ ADR,  2 2 MOVZ,  NR-WRITE SYS,
   CP DATA RSAVCP-CELL LDR,
   NDICT DATA RSAVND-CELL LDR,
   9 DATA RSAVDP-CELL LDR,  9 DATA DP-CELL STR,
   9 DATA S0-CELL LDR,  XDS 9 0 ADDI,
   EMIT-RESET-COMPILE-STATE
   9 DATA RSAVSP-CELL LDR,  SP 9 0 ADDI,
   LREAD @ B, ;

: EMIT-UNDEF ( n -- ) {: lundef :}
   lundef LBL,
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   9 DATA EVALD-CELL LDR,  9 LUN0 @ CBZ,
      EMIT-EVAL-UNDEF-ROLLBACK
   LUN0 @ LBL,
   9 DATA REPLH-CELL LDR,  9 LRDIE @ CBZ,
   EMIT-REPL-RECOVER
   LRDIE @ LBL,
   0 70 MOVZ,  NR-EXIT-GROUP SYS, ;

: EMIT-EVAL-CLEAN-EXIT ( -- )
   9 DATA EVALD-CELL LDR,  9 9 1 SUBI,  9 DATA EVALD-CELL STR,
   9 14 15 C-EVAL-FRAME-ADDR
   9 14 0 LDR,  9 DATA INP-CELL STR,
   9 14 8 LDR,  9 DATA INE-CELL STR,
   9 0 MOVZ,  9 DATA EVALERR-CELL STR,
   9 14 16 LDR,  9 BR, ;

: EMIT-REPL-READ ( n -- ) {: lmain :}
   LREAD @ LBL,
   9 SP 0 ADDI,  9 DATA RSAVSP-CELL STR,
   CP DATA RSAVCP-CELL STR,
   NDICT DATA RSAVND-CELL STR,
   9 DATA DP-CELL LDR,  9 DATA RSAVDP-CELL STR,
   9 DATA REPLH-CELL LDR,  9 BLR,
   XDS XDS 8 SUBI,  10 XDS 0 LDR,
   XDS XDS 8 SUBI,  11 XDS 0 LDR,
   10 LRBYE @ CBZ,
   11 DATA INP-CELL STR,  11 11 10 ADD,  11 DATA INE-CELL STR,  lmain B, ;

: EMIT-EXIT ( n n -- ) {: lexit lmain :}
   lexit LBL,
   9 DATA EVALD-CELL LDR,  9 LEX0 @ CBZ,
      EMIT-EVAL-CLEAN-EXIT
   LEX0 @ LBL,
   9 DATA REPLH-CELL LDR,  9 LRBYE @ CBZ,
   0 1 MOVZ,  1 LOKS @ ADR,  2 4 MOVZ,  NR-WRITE SYS,
   lmain EMIT-REPL-READ
   LRBYE @ LBL,
   0 0 MOVZ,  NR-EXIT-GROUP SYS, ;

\ ---- MAIN: startup (data stack + mmap + seed dict) then the outer interpreter ----
: EMIT-MAIN ( -- )
   EMIT-STARTUP
   LBL {: LMAIN :}  LBL {: LEXIT :}  LBL {: LCOMPILE :}  LBL {: LUNDEF :}   \ allocate up-front (byte-free) so the LMAIN store below is in scope
   LMAIN EMIT-MAIN-RUNTIME-LABELS
   LMAIN LBL,
      LMAIN LEXIT LCOMPILE EMIT-TOKEN-DISPATCH
      LMAIN LUNDEF EMIT-INTERPRET
	      \ ---------------- COMPILE ----------------
	   LCOMPILE LBL,
	      LMAIN LUNDEF EMIT-COMPILE
	   LUNDEF EMIT-UNDEF
	   LEXIT LMAIN EMIT-EXIT ;

: EMIT-RESET-BUILDER ( -- )
   ICODE-RESET  CF-RESET  0 #PL !  0 PNP ! ;

: EMIT-LABEL-CORE ( -- )
   LBL LANCHOR !  LBL LFIND !  LBL LNUM !  LBL LDICT !  LBL LSRC !
   LBL LCEMIT !  LBL LTOK !  LBL LPROT !  LBL LPROTWIDQ !  LBL LFLUSH !  LBL LNCOUNT !
   LBL LBCAP !  LBL LBCS !  LBL LESCDEC !  LBL LESCHEX !  LBL LESCSCAN !  LBL LESCCOPY !
   LBL LCFPUSH !  LBL LCFPOP !  LBL LPAT !  LBL LKWCMP ! ;

: EMIT-LABEL-RUNTIME ( -- )
   LBL LBCHAIN !  LBL LCREATE !  LBL LDOESPATCH !
   LBL LREAD !  LBL LRBYE !  LBL LRDIE !  LBL LRREC !  LBL LQNL !  LBL LOKS !
   LBL LEX0 !  LBL LUN0 ! ;

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
   LBL LKWPACKAGE !  LBL LKWPUBLIC !  LBL LKWPRIVATE !  LBL LKWSEMIPACKAGE !
   LBL LCHKPACKAGE !  LBL LCHKPUB !  LBL LCHKPRI !  LBL LCHKENDPKG !
   LBL LRESCHECKCERT !  LBL LRESLOWERCERT !  LBL LRESLOWERHOOK !
   LBL LKWQUOT !  LBL LKWSEMIQ !
   LBL LKWCONSTRUCT !  LBL LKWMATCH !  LBL LKWSEMIMATCH !
   LBL LTFLCONFAM !  LBL LTFLCVAR !
   LBL LTFLMATCHFAM !  LBL LTFLNAME !  LBL LBADTAGPFX !  LBL LBADTAGSFX ! ;

: EMIT-LABEL-SIGNALS ( -- )
   LBL LCRASHH !  LBL LHEX !  LBL LHDR !  LBL LTRAPH !  LBL LBPH !
   LBL LSRCRD !  LBL LSHBANG ! ;

: EMIT-LABEL-SOURCES ( -- )
   LBL LPLINUXTARGET !  LBL LPMACOSTARGET !
   LBL LPLINUXLAYOUT !  LBL LPMACOSLAYOUT !
   LBL LPUTIL !  LBL LPSTRUCTURES !  LBL LPBYTES !  LBL LPCHECKER !  LBL LPLOWERCERTBASE !  LBL LPRENDER !  LBL LPHOOK !
   LBL LPTYPESCHEMA !  LBL LPTYPEFAM !  LBL LPSUMTYPE !  LBL LPLAYOUTBUF !  LBL LPLAYOUTVALID !
   LBL LPSTRUCTEFF !  LBL LPHABULAYOUT !
   LBL LPENVBASE !  LBL LPINCLUDE !  LBL LPSCRIPTARGV !  LBL LPROLES !
   LBL LPENUMS !  LBL LPEXECVECTOR !  LBL LPSHA256 !  LBL LPTFAMSHA !
   LBL LPCOMBINATORS !  LBL LPXREF !  LBL LPLAYOUTSEAL ! ;

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
   LBL LRECWPUB !
   LBL LKWTUCK3 !  LBL LKWROT3 !  LBL LKWMROT3 !
   LBL LKW2DUP3 !  LBL LKW2DROP3 !  LBL LKW2SWAP3 !  LBL LKW2OVER3 !
   LBL LKW2TOR3 !  LBL LKW2RFROM3 !  LBL LKW2RFET3 !
   LBL LP2COPY !  LBL LP2DROPN !  LBL LP2REV !  LBL LP2ROT !  LBL LP2RS !
   LBL LKWAT2 !  LBL LKWSTORE2 !
   LBL LP2FETCH !  LBL LP2STORE !  LBL LP2VEXEC !  LBL LP2VEMIT !
   LBL LVPBADMSG !  LBL LP2DOESW !  LBL LP2NEST !
   LBL [ also LOWER-TXN-CODE ] BAD-LABEL [ previous ] !
   LBL [ also LOWER-TXN-CODE ] MEM-LABEL [ previous ] !
   LBL [ also LOWER-TXN-CODE ] FULL-LABEL [ previous ] !
   LBL [ also LOWER-TXN-CODE ] DRIFT-LABEL [ previous ] !
   LBL [ also LOWER-TXN-CODE ] VDESC-LABEL [ previous ] !
   LBL [ also LOWER-TXN-CODE ] DRIFT-FAIL-LABEL [ previous ] ! ;

: EMIT-LABELS ( -- )
   EMIT-LABEL-CORE
   EMIT-LABEL-RUNTIME
   EMIT-LABEL-CONTROL
   EMIT-LABEL-SIGNALS
   EMIT-LABEL-SOURCES
   EMIT-LABEL-JIT
   EMIT-LABEL-OPS
   EMIT-LABEL-P2 ;

: EMIT-PRIMITIVE-SECTIONS ( -- )
   EMIT-PRIMS
   EMIT-PROF-PRIMS
   EMIT-FP-PRIMS
   EMIT-CEMIT
   EMIT-BCAP
   EMIT-TOK
   EMIT-PROT
   EMIT-PROTWID
   EMIT-FLUSH
   EMIT-FIND
   EMIT-NUM ;

: EMIT-DICTIONARY-SECTIONS ( -- )
   EMIT-CREATE
   EMIT-DOESPATCH
   EMIT-CF-HELPERS  EMIT-ESC-DECODE  EMIT-ESC-SCAN  EMIT-ESC-COPY
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
   EMIT-JIT
   EMIT-P2-HELPERS
   [ also LOWER-TXN-CODE ] EMIT-DESC EMIT-DRIFT-FAIL [ previous ] ;

: EMIT-CODE-SECTIONS ( -- )
   EMIT-MAIN                                              \ entry @ offset 0
   EMIT-PRIMITIVE-SECTIONS
   EMIT-DICTIONARY-SECTIONS
   EMIT-RUNTIME-SECTIONS
   EMIT-DICT ;                                            \ after #PL is final

: EMIT-SOURCE-BYTES ( -- )
   LSRC @ LBL,  SRCA @ SRCN @ BYTES, ;

: EMIT-FORTH ( src-a src-u -- )
   SRCN !  SRCA !
   EMIT-RESET-BUILDER
   EMIT-LABELS
   EMIT-CODE-SECTIONS
   EMIT-SOURCE-BYTES ;

\ Build a standalone native Forth that interprets `src`, write it to `outfile`.
: FORTH-EXE ( src-a src-u out-a out-u -- )
   2>r  EMIT-FORTH  2r> EMIT-EXE ;

: FORTH-BUILD-EXE ( src-a src-u out-a out-u -- )
   2>r
   BUILD-SOURCE? on
   ['] EMIT-FORTH catch
   BUILD-SOURCE? off
   throw
   2r> EMIT-EXE ;

\ Build a standalone native Forth that reads its program from STDIN (batch REPL),
\ write it to `outfile`:  echo ': SQ DUP * ; 5 SQ .' | ./outfile
: FORTH-REPL-EXE ( out-a out-u -- )
   STDIN? on  s" "  ['] EMIT-FORTH catch  STDIN? off  throw  \ restore mode even on error
   EMIT-EXE ;

\ habu1.f, engine builder part 1: emits the native engine's primitive bodies,
\ helper routines and seed dictionary for arm64 (Linux and macOS). The
\ interpreter loop, keyword JIT and ENGINE-EMIT:FORTH are in habu2.f.
variable STDIN?   0 0= 0= STDIN? !
s" STDIN?" s" -- ptr bool" TRUST

\ A64ASM's encoders live in src/arch/arm64/asm.f.
require src/arch/arm64/icode.f
using A64ASM
using A64ICODE

package ENGINE-BUILD
variable ACTIVE
: ACTIVE? ( -- bool ) ACTIVE @ ;
: ARM ( -- ) 0 0= ACTIVE ! ;
: DISARM ( -- ) 0 0= 0= ACTIVE ! ;
public
: BUILDING? ( -- bool ) ACTIVE? ;
;package
$D65F03C0 constant W-RET
$D503201F constant W-NOP
$D2800009 constant W-MOVZ0
\ `;` writes W-DOESDECL over a does>-clause opener's `adr x10, D` when the clause
\ compiled nothing (habu2.f DOES-REC:ELIDE-EMPTY): a zero clause entry makes
\ LDOESPATCH publish the declared effect and patch nothing.
$D280000A constant W-DOESDECL              \ movz x10,#0
$F2A00009 constant W-MOVK1
$F2C00009 constant W-MOVK2
$F2E00009 constant W-MOVK3

\ XDS points just past the top cell: a push is a post-indexed store, a pop a
\ pre-indexed load. Encoded through asm.f so an out-of-field offset fails at
\ build time.
 9 XDS   8 ENC-STRPOST constant W-PUSH9    \ str x9,[x19],#8
11 XDS   8 ENC-STRPOST constant W-PUSH11   \ str x11,[x19],#8
13 XDS   8 ENC-STRPOST constant W-PUSH13   \ str x13,[x19],#8
16 XDS   8 ENC-STRPOST constant W-PUSH16   \ str x16,[x19],#8
 9 XDS  -8 ENC-LDRPRE  constant W-POP9     \ ldr x9,[x19,#-8]!
10 XDS  -8 ENC-LDRPRE  constant W-POP10    \ ldr x10,[x19,#-8]!
16 XDS  -8 ENC-LDRPRE  constant W-POP16    \ ldr x16,[x19,#-8]!
17 XDS  -8 ENC-LDRPRE  constant W-POP17    \ ldr x17,[x19,#-8]!

\ A compiled word saves LR with one pre-indexed store and restores it with one
\ post-indexed load; a word that calls nothing gets W-NOP in the entry slot
\ (EM-COMPILE-RET backpatches it). 16 bytes: AArch64 sp stays 16-byte aligned.
30 SP -16 ENC-STRPRE  constant W-LINKSAVE  \ str x30,[sp,#-16]!
30 SP  16 ENC-LDRPOST constant W-LINKREST  \ ldr x30,[sp],#16
require src/habu/primitive-registry.f
require src/habu/native-observer-extension.f
require src/habu/native-body-abi.f
require src/habu/data-claims.f          \ each band GUARD-SPAN tests is a claim
require src/habu/arith-abi.f           \ E-DIV-ZERO, the dividing bodies' refusal
require src/habu/task-abi.f
require src/habu/spawn-abi.f
require src/habu/code-span.f
require src/habu/prof-abi.f
PTR-VARIABLE FP-A  variable FP-U

defer FP-EMIT ( -- )
: FP-A@ ( -- ptr u8 ) FP-A @ ;

variable FPL  variable FPE
variable DIVISION-PRIM-ROW

\ Every body registers under a name src/habu/prims.f specifies: FP-ARGS checks
\ it (ENGINE-PRIMS:SPEC-CHECK) and habu2.f ENGINE-EMIT:EMIT-PRIMITIVE-SECTIONS
\ checks the registry is complete.
: FP-ARGS ( ptr u8 n [ -- ] -- )
   is FP-EMIT  FP-U !  FP-A !
   FP-A@ FP-U @ ENGINE-PRIMS:SPEC-CHECK ;

: FP-REG ( -- n )
   FP-A@ FP-U @ FPL LABEL@ FPE LABEL@ ENGINE-PRIMS:ADD ;

: FPRIM ( ptr u8 n [ -- ] -- )
   FP-ARGS
   LBL FPL !  LBL FPE !
   FP-REG drop
   FPL LABEL@ LBL,  SP SP 16 SUBI,  30 SP 0 STR,
   FP-EMIT  30 SP 0 LDR,  SP SP 16 ADDI,  RET,  FPE LABEL@ LBL, ;

\ FPRIM-L only for a body that emits no BL or BLR of its own; any other body
\ uses FPRIM, whose frame preserves the caller's return address in x30.
: FPRIM-L ( ptr u8 n [ -- ] -- )      \ body preserves LR; stack guards frame their own BL
   FP-ARGS
   LBL FPL !  LBL FPE !
   FP-REG drop
   FPL LABEL@ LBL,  FP-EMIT  RET,  FPE LABEL@ LBL, ;

: FPRIM-L-DIVISION ( ptr u8 n [ -- ] -- )
   FP-ARGS
   LBL FPL !  LBL FPE !
   FP-REG 1+ DIVISION-PRIM-ROW !
   FPL LABEL@ LBL,  FP-EMIT  RET,  FPE LABEL@ LBL, ;

\ For an LR-preserving straight-line body (BABS): MARK-SCALAR records that the
\ span has no helper or terminal call edge.
: FPRIM-L-SCALAR ( ptr u8 n [ -- ] -- )
   FP-ARGS
   LBL FPL !  LBL FPE !
   FP-REG {: row:n :}
   FPL LABEL@ LBL,  FP-EMIT  RET,  FPE LABEL@ LBL,
   row ENGINE-PRIMS:MARK-SCALAR ;

variable FP-WID

: FPRIM-WID ( ptr u8 n [ -- ] n -- )
   FP-WID !
   FP-ARGS
   LBL FPL !  LBL FPE !
   FP-REG FP-WID @ swap ENGINE-PRIMS:WID!
   FPL LABEL@ LBL,  SP SP 16 SUBI,  30 SP 0 STR,
   FP-EMIT  30 SP 0 LDR,  SP SP 16 ADDI,  RET,  FPE LABEL@ LBL, ;

\ label ids (forward refs)
variable LANCHOR  variable LFIND  variable LNUM  variable LDICT  variable LHOSTROW  variable LHOSTDIVROW  variable LSRC  variable SRCN
variable LCEMIT   variable LCEMITBL  variable LCEMITB  variable LCINLINE  variable LTOK   variable LPROTSPAN  variable LFLUSH variable LFLUSHTO variable LNCOUNT
variable LDIVZERO                       \ the (DIV-ZERO) refusal the dividing bodies branch to
\ The count EMIT-DICT bakes into LNCOUNT. The AOT call-site binder (habu2.f)
\ reads this, not the registry: a payload record's dictionary index is the
\ seeded count plus its position, and the registry can grow after EMIT-DICT.
variable SEEDED-PRIM-N
variable LREPLROUTE
\ The process-exit trampoline (habu2.f EMIT-EXITHOOK, layout.f EXIT-HOOK-CELL),
\ declared here because BDIE below calls it.
variable LEXITHOOK
\ A seed stub branches to this disjoint shared C entry. Reserve its label
\ before callback-entry is emitted, then register its own span afterward.
variable LCALLBACKTHUNK
variable LCALLBACKADDR
variable LAOTCODE  variable LAOTDICT  variable LAOTCODELEN
variable LAOTNREC  variable LAOTNSITE  variable LAOTSITES  variable LAOTNAMES  variable LAOTNAMESLEN
variable LAOTNDSITE  variable LAOTDSITES  variable LAOTDATAD0  variable LAOTDATASIZE
\ Label ids of the DATA window's content bake: its non-zero extents as a count,
\ (offset u32, length u32) rows and their bytes in row order, then the
\ window-relative offsets of declared address cells, which the seed re-traps
\ rather than copies. The rest of the package is in habu2.f.
package AOT-WINDOW
public
\ LBMGROUPS and LBMBYTES frame the grouped bitmap LBM carries: the groups the
\ presence map covers and the bytes the present ones take (src/habu/aot-decl.f
\ AOT-WINDOW:BM-COMPACT).
variable LBMGROUPS  variable LBMBYTES  variable LBM  variable LVALS
variable LNXTOFF  variable LXTOFFS
\ The window's wordlist base and span, and the window-relative ids it sealed. The
\ seed rebases every captured wid through the first pair and applies the third
\ after that rebase, which is the only moment the target's own base is known.
variable LWIDW0  variable LWIDSPAN  variable LNPWIN  variable LPWIN
;package
variable LAOTNCSITE  variable LAOTCSITES  variable LAOTCODEB0
\ Named code sites: a code-address literal holding a WORD's entry, which the
\ seed resolves by name instead of rebasing. Count + 8-byte rows. Label ids
\ only; the rest of the package is in habu2.f.
package AOT-XTSITE
public
variable LCOUNT  variable LROWS
;package
variable LAOTBOOTRUN
\ The checker payload the seed publishes into layout.f AOT-CELLS:SIG-POOL-CELL
\ and SIG-LEN-CELL: its byte length, then the span (signature rows, their
\ strings, the type registry delta, behind their own table).
package AOT-SIG
public
variable LLEN  variable LSPAN  variable LNAME
;package
variable LPROTWIDQ
variable LDPBAD   \ DP-CHECK out-of-range die target (habu2.f EM-COMPILE-DIE)
variable LBCAPFULL   \ body-capture over BODYBUF-CAP die target (habu2.f EM-BODY-CAP-DIE)
\ Hash-index label ids, bound in EMIT-HIDX: LREBUILD for the in-place compaction
\ BNDSET's raise leg calls, LFULL for the cannot-maintain exit, LREKEY for the
\ re-key record-wid! calls. Not in layout.f's HIDX: the engine bakes that one.
package HIDX-EMIT
public
variable LREBUILD
variable LFULL
variable LREKEY
;package
variable LHIDXADD

package EM-DEF-OCC
public
variable LCOUNT
variable LAPPEND
variable LRESET
variable LROOM
variable LISSUE
variable LINIT
variable LBEFORE
variable LAFTER
variable LMATCH
variable LRECLAIM
variable LRETIRE
\ Builder-side mirrors of layout.f's process-local cell and engine-error.f's
\ refusal ABI: the qualified predecessor that emits the first product bakes a
\ layout without DEF-OCC. They add no captured DATA band.
$2D08 constant PTR-CELL
DICT-CAP 1+ cells constant STATE-BYTES
-7231 constant E-STALE
-7232 constant E-SELECT
-7234 constant E-EXHAUSTED
;package
\ The one-wordlist search shared by the `search-wl` primitive and the AOT
\ seed's call-site pass.
package WLFIND
public
variable LENTRY
variable LNEWEST
;package
variable LCFPUSH  variable LCFPOP  variable LPAT   variable LKWCMP  variable LBCAP  variable LBCS
variable LROWWALK   \ every dispatch row's spelling test over x0/x1 (habu2.f COMPILE-EMIT:EMIT-ROW-WALK); BPOLICYADMIT calls it
variable LBCHAIN  variable LCREATE  variable LDOESPATCH
variable LKWIF    variable LKWTHEN variable LKWELSE variable LKWBEGIN
variable LKWUNTIL variable LKWAGAIN variable LKWWHILE variable LKWREPEAT
variable LKWCASE  variable LKWOF    variable LKWENDOF variable LKWENDCASE
variable LKWCREATE variable LKWVAR variable LKWSQ variable LKWCQ variable LKWDOTQ
variable LKWESQ variable LKWECQ variable LKWEDOTQ
variable LKWTICK variable LKWBTICK
variable LKWTYPE
variable LKWLBRACE variable LKWENDLOC variable LLOC-FIND variable LKWCONST
variable LKWDO variable LKWLOOP variable LKWI
variable LKWTOR variable LKWRFROM variable LKWRFET
variable LKWEXIT variable LKWREC
variable LKWQDO variable LKWPLOOP variable LKWJ variable LKWLEAVE variable LKWUNLOOP
variable LKWCHAR variable LKWBCHAR
variable LKWIMM
variable LKWDOES variable LKWQUOT variable LKWSEMIQ variable LKWPACKAGE variable LKWPUBLIC
variable LKWTRUSTED variable LKWKERNEL variable LKWPRIVATE variable LKWSEMIPACKAGE variable LKWDUPDEF variable LCHKPACKAGE variable LCHKPUB variable LCHKPRI variable LCHKENDPKG
variable LKWEXPORT variable LCHKEXPORT
variable LKWUSING variable LKWSEMIUSING variable LCHKUSING variable LFINDUSED variable LFINDUSED-CORE variable LSCOPEREC
\ The interpreter's shadow leaf (habu2.f INTERP-EMIT:FIND-SHADOW), which the
\ outer loop and `'` call on LFIND's hit path.
package INTERP-EMIT
public
variable LFINDSHADOW
;package
9 constant A   10 constant B   11 constant C
12 constant DREG  13 constant EREG

\ Emit one half-open interval-overlap test. x13 holds the checked span's end;
\ x12 is scratch. The caller has already rejected address+length overflow.
: GUARD-BAND ( n n n label -- ) {: addr:n off:n len:n trap:label :}
   LBL {: skip:label :}
   DREG off len + LIT64,  DREG DATA DREG ADD,   \ protected end
   addr DREG CMP,  C-CS skip BCOND,             \ start >= protected end
   DREG off LIT64,  DREG DATA DREG ADD,         \ protected start
   EREG DREG CMP,  C-HI trap BCOND,             \ checked end > protected start
   skip LBL, ;

package ENGINE-EMIT

\ The protected bands are src/habu/layout.f's DATA-BANDS table, which the
\ x86-64 (PROT-SPAN) helper reads as well.
variable BAND-IX

: BANDS-EMIT ( n label -- ) {: addr:n trap:label :}
   0 BAND-IX !
   BEGIN BAND-IX @ DATA-BANDS:LEN 0 <> WHILE
      addr BAND-IX @ DATA-BANDS:OFF BAND-IX @ DATA-BANDS:LEN trap GUARD-BAND
      BAND-IX @ 1+ BAND-IX !
   REPEAT ;

\ addr and len name runtime registers. A zero-length write is inert; an
\ address+length wrap traps before the band tests. Inactive only while the
\ canonical cold prefix owns the open friend latch. Clobbers x12/x13 only.
: GUARD-SPAN ( n n -- )
   {: addr:n len:n :}
   LBL LBL {: ok:label trap:label :}
   DREG DATA FRIEND-LATCH-CELL LDR,
   DREG ok CBZ,
   len ok CBZ,
   EREG addr len ADD,                   \ checked end = start + length
   EREG addr CMP,  C-CC trap BCOND,     \ unsigned wrap
   \ Every band lies in [DATA-BANDS:LO, DATA-BANDS:HI), so a span outside that
   \ hull skips the band walk. Every DP heap address takes this exit: the hull
   \ ends before pending scratch and DATA-START.
   DREG DATA-BANDS:HI LIT64,  DREG DATA DREG ADD,
   addr DREG CMP,  C-CS ok BCOND,       \ start >= hull end
   DREG DATA-BANDS:LO LIT64,  DREG DATA DREG ADD,
   EREG DREG CMP,  C-LS ok BCOND,       \ checked end <= hull start
   addr trap BANDS-EMIT
   ok B,
   trap LBL,  0 ENGINE-ERROR:SEAL-VIOLATION MOVZ,  NR-EXIT-GROUP SYS,
   ok LBL, ;

;package

package PROT-GUARD

public

\ Move addr/len (the caller's registers, any aliasing) into x10/x11 and BL the
\ one engine-resident guard body at LPROTSPAN, the (PROT-SPAN) helper. Clobbers
\ x10-x13 and x30: a sink needing one live across the guard saves it first
\ (BATCAS, BPATCH32). The body touches only x12/x13.
: CALL ( n n -- ) {: addr:n len:n :}
   len 10 = if
      addr 11 = if
         12 11 0 ADDI,  11 10 0 ADDI,  10 12 0 ADDI,  \ crossed: swap x10/x11 via x12
      else
         11 10 0 ADDI,  10 addr 0 ADDI,               \ len in x10: move it up before x10 is overwritten
      then
   else
      addr 10 <> if 10 addr 0 ADDI, then              \ addr -> x10 (skip a no-op move)
      len 11 <> if 11 len 0 ADDI, then                \ len  -> x11 (skip a no-op move)
   then
   LPROTSPAN LABEL@ BL, ;

;package

\ A code emission target is one aligned instruction inside the writable code
\ interval [DBASE+DICT-SIZE, DBASE+REGION), whatever the DATA protection latch
\ says: cp! may never redirect later LCEMIT writes into a DATA band. The $4000
\ reserve below REGION bounds the rest of a definition once its first
\ instruction is admitted.
: GUARD-CODE-WORD ( n -- ) {: addr:n :}
   LBL LBL {: ok:label trap:label :}
   DREG DICT-SIZE LIT64,  DREG DBASE DREG ADD,
   addr DREG CMP,  C-CC trap BCOND,
   DREG REGION 4 - LIT64,  DREG DBASE DREG ADD,
   addr DREG CMP,  C-HI trap BCOND,
   EREG addr 3 ANDI,  EREG trap CBNZ,
   ok B,
   trap LBL,  0 ENGINE-ERROR:SEAL-VIOLATION MOVZ,  NR-EXIT-GROUP SYS,
   ok LBL, ;

\ Every target selector below names the Linux-aarch64 and macOS arms and calls
\ this otherwise: this file holds primitive bodies for those two targets only,
\ so emitting one for any other target fails closed (docs/xcomp.md "Source Selection").
package ENGINE-EMIT
public
: TARGET-UNKNOWN ( -- )
   s" hb: habu1: no primitive body for this target" 76 die ;
;package

\ Guard the kernel-written extent encoded by the target ioctl ABI. Linux's
\ legacy TCGETS/TCSETS pair predates _IOC direction bits and is handled
\ explicitly; unknown unencoded requests fail closed instead of recovering an
\ unknowable pointer extent. Runtime registers: x1=request, x2=argument.
: GUARD-IOCTL ( -- )
   LBL LBL LBL LBL {: legacy:label write:label done:label trap:label :}
   HB-TARGET-LINUX? IF
      7 $5401 LIT64,  1 7 CMP,  C-EQ legacy BCOND,
      7 $5402 LIT64,  1 7 CMP,  C-EQ done BCOND,
      8 1 30 LSRI,  7 8 3 ANDI,             \ _IOC_DIR
      8 7 2 ANDI,  8 write CBNZ,             \ _IOC_READ: kernel writes
      7 done CBNZ,                           \ _IOC_WRITE: kernel reads only
      trap B,
      legacy LBL,
      7 36 MOVZ,  2 7 PROT-GUARD:CALL
      done B,
      write LBL,
      7 1 16 LSRI,  8 $3FFF LIT64,  7 7 8 AND,
      7 done CBZ,
      2 7 PROT-GUARD:CALL
   ELSE HB-TARGET-MACOS? IF
      8 $40000000 LIT64,  7 1 8 AND,         \ IOC_OUT: kernel writes
      7 write CBNZ,
      8 $A0000000 LIT64,  7 1 8 AND,         \ IOC_IN or IOC_VOID
      7 done CBNZ,
      trap B,
      write LBL,
      7 1 16 LSRI,  8 $1FFF LIT64,  7 7 8 AND,
      7 done CBZ,
      2 7 PROT-GUARD:CALL
   ELSE
      ENGINE-EMIT:TARGET-UNKNOWN
   THEN THEN
   done B,
   trap LBL,  0 ENGINE-ERROR:SEAL-VIOLATION MOVZ,  NR-EXIT-GROUP SYS,
   done LBL, ;

\ Primitive bodies operate on the x19 data stack.
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

variable LNX-ERR
variable LNX-FD
variable LNX-NEW
variable LNX-CWD
variable LNX-SKIP
variable LNX-OK
variable LNX-FAIL
variable LNX-DONE
variable LNX-CHILD
variable LNX-CLOSEFAIL
variable LNX-WAIT
variable LNX-REAL
variable LNX-PNEG
variable LNX-PCALL
variable LNX-PATH
variable LNX-ARGV
variable LNX-ENV
variable LNX-IN
variable LNX-OUT
variable TIME-OK
variable PS-LOOP
variable PS-DONE
variable CMP-COND
variable PRIM-DONE
variable RSTK-REG
variable DP-REG
variable DP-LOW
variable DP-HIGH
variable SYS-OK
variable SYS-DONE
variable FFI-DREG
variable FFI-OFF
variable FFI-SKIP
variable FFI-LOOP
variable FFI-DONE
variable STAT-BUF
variable STAT-OK
variable STAT-DONE
variable CATCH-RES
variable CATCH-PUSH
variable FINALLY-DONE
variable C2-INVOKE-DONE
variable C2-INVOKE-CLEAN
variable THROW-NOH
variable THROW-NOREC
variable THROW-NOREC-FB
variable THROW-NOREC-FB2
variable THROW-EVAL
variable THROW-CORRUPT
variable THROW-CORRUPT-MSG
variable SWL-LOOP
variable SWL-END
variable SWL-NEXT
variable SWL-CMP
variable SWL-MATCH
variable SWL-F1
variable SWL-F2
variable SWL-INL
variable PARSE-NONE
variable PARSE-DONE
variable FP-COND
variable FD-FRAC
variable FD-INT
variable FD-SIGN
variable BCAP-OK
variable BCAP-CP
variable BCAP-CD
variable BCAP-GO
variable TOK-SKIP
variable TOK-HAS
variable TOK-SCAN
variable TOK-GOT
variable TOK-NONE
variable FL-DL
variable FL-DD
variable FL-IL
variable FL-ID
variable FIND-QSCAN
variable FIND-QNONE
variable FIND-QHAS
variable FIND-QBAD
variable FIND-QTAIL
variable FIND-QTAILOK
variable FIND-NLOOP
variable FIND-NNEXT
variable FIND-NCMP
variable FIND-NMATCH
variable FIND-NEND
variable FIND-NINL
variable FIND-START
variable FIND-LOOP
variable FIND-DONE
variable FIND-NEXT
variable FIND-CMP
variable FIND-MATCH
variable FIND-INL
variable FIND-MISS
variable FIND-TRYG
variable FIND-FOUND
variable NUM-DONE
variable NUM-NDOLL
variable NUM-NOHEX
variable NUM-LOOP
variable NUM-OK
variable NUM-GOTD
variable NUM-ND
variable NUM-NUC
variable NUM-NDOT
variable NUM-ISFRAC
variable NUM-FRAC-NEXT
variable NUM-LINT

: LINUX-SPAWN-FAIL-N ( n -- )
   0 swap 0 ADDI,
   15 1 MOVZ,  15 SP LINUX-SPAWN-ERR-OFF STRB,
   1 SP LINUX-SPAWN-ERR-OFF ADDI,  2 1 MOVZ,  NR-WRITE SYS,
   0 127 MOVZ,  NR-EXIT-GROUP SYS, ;

: LINUX-SPAWN-FAIL ( reg -- )
   REG>N LINUX-SPAWN-FAIL-N ;

: LINUX-DUP2-ARGS ( reg fd reg -- )
   REG>N LNX-ERR !
   FD>N LNX-NEW !
   REG>N LNX-FD ! ;

\ Put the child's descriptor in the first reg on fd n: dup3 it there, or, when
\ it already sits on n, clear the close-on-exec flag a dup3 would have cleared.
\ A negative descriptor leaves n alone.
: LINUX-DUP2-FD ( reg fd reg -- )
   LINUX-DUP2-ARGS
   LBL {: skip:label :}
   LBL {: keep:label :}
   LBL {: done:label :}
   LNX-FD @ 0 CMPI,  C-LT skip BCOND,
   0 LNX-FD @ 0 ADDI,
   LNX-FD @ LNX-NEW @ CMPI,  C-EQ keep BCOND,
   1 LNX-NEW @ MOVZ,  2 0 MOVZ,  NR-DUP2 SYS,  done B,
   keep LBL,
   1 LINUX-F-SETFD MOVZ,  2 0 MOVZ,  NR-FCNTL SYS,
   done LBL,
   9 C-CS CSET,  9 skip CBZ,
      LNX-ERR @ LINUX-SPAWN-FAIL-N
   skip LBL, ;

: LINUX-CHDIR-ARGS ( reg reg -- )
   REG>N LNX-ERR !
   REG>N LNX-CWD ! ;

: LINUX-CHDIR-FD ( reg reg -- )
   LINUX-CHDIR-ARGS
   LBL {: skip:label :}
   LBL {: ok:label :}
   LNX-CWD @ 0 CMPI,  C-LT skip BCOND,
   0 LNX-CWD @ 0 ADDI,  NR-CHDIR SYS,
   9 C-CS CSET,  9 ok CBZ,
      LNX-ERR @ LINUX-SPAWN-FAIL-N
   ok LBL,
   skip LBL, ;

: LINUX-SETPGID-SELF ( reg -- )
   REG>N LNX-ERR !
   LBL {: ok:label :}
   0 0 MOVZ,  1 0 MOVZ,  NR-SETPGID SYS,
   9 C-CS CSET,  9 ok CBZ,
      LNX-ERR @ LINUX-SPAWN-FAIL-N
   ok LBL, ;

: LINUX-SPAWN-CLOSE-R ( -- )
   0 SP LINUX-SPAWN-PIPE-R-OFF LDRW,  NR-CLOSE SYS, ;

: LINUX-SPAWN-CLOSE-W ( -- )
   0 SP LINUX-SPAWN-PIPE-W-OFF LDRW,  NR-CLOSE SYS, ;

: LINUX-SPAWN-CLOSE-PIPE ( -- )
   LINUX-SPAWN-CLOSE-R
   LINUX-SPAWN-CLOSE-W ;

\ Move the pipe's write end in the frame to the fd in x12 or above, so no
\ descriptor the child places can replace it. x9 becomes 0, or the fcntl's
\ -errno.
: LINUX-SPAWN-LIFT-W ( -- )
   LBL {: fail:label :}
   LBL {: done:label :}
   9 0 MOVZ,
   0 SP LINUX-SPAWN-PIPE-W-OFF LDRW,
   0 12 CMP,  C-GE done BCOND,
      1 LINUX-F-DUPFD-CLOEXEC MOVZ,  2 12 0 ADDI,
      NR-FCNTL SYS,
      C-CS fail BCOND,
      14 0 0 ADDI,
      0 SP LINUX-SPAWN-PIPE-W-OFF LDRW,  NR-CLOSE SYS,
      14 SP LINUX-SPAWN-PIPE-W-OFF STRW,
      done B,
   fail LBL,
      9 0 0 ADDI,
   done LBL, ;

: LINUX-SPAWN-PREP-W ( -- )
   12 LINUX-SPAWN-MIN-ERRFD MOVZ,
   LINUX-SPAWN-LIFT-W ;

: LINUX-SPAWN-WAIT-STORED ( -- )
   0 SP LINUX-SPAWN-PID-OFF LDR,
   1 SP LINUX-SPAWN-STATUS-OFF ADDI,  2 0 MOVZ,  3 0 MOVZ,
   NR-WAIT4 SYS, ;

: LINUX-SPAWN-PARENT ( -- )
   LBL {: ok:label :}
   LBL {: fail:label :}
   LBL {: done:label :}
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

: LINUX-SKIP-SAME ( n label -- )
   {: off:n skip:label :}
   10 SP off LDR,  9 10 CMP,  C-EQ skip BCOND, ;

\ Close the stdio source in the frame cell at n (32, 40, 48: stdin, stdout,
\ stderr; the three roles sit on 0, 1 and 2). Nothing is closed for a
\ source at or below 2 (-1 or already a stdio number), for one an earlier cell
\ also holds (that cell's step closes it), or for the exec-failure pipe's write
\ end.
: LINUX-CLOSE-SOURCE ( n -- )
   {: off:n :}
   LBL {: skip:label :}
   9 SP off LDR,
   9 2 CMPI,  C-LE skip BCOND,
   32 BEGIN dup off < WHILE
      dup skip LINUX-SKIP-SAME
      8 +
   REPEAT drop
   14 SP LINUX-SPAWN-PIPE-W-OFF LDRW,  9 14 CMP,  C-EQ skip BCOND,
   0 9 0 ADDI,  NR-CLOSE SYS,
   9 C-CS CSET,  9 skip CBZ,
      14 LINUX-SPAWN-FAIL-N
   skip LBL, ;

: LINUX-SPAWN-CHILD ( -- )
   LINUX-SPAWN-CLOSE-R
   14 SP LINUX-SPAWN-PIPE-W-OFF LDRW,
   14 >REG LINUX-SETPGID-SELF
   14 SP LINUX-SPAWN-PIPE-W-OFF LDRW,
   9 SP 24 LDR,  9 >REG 14 >REG LINUX-CHDIR-FD
   14 SP LINUX-SPAWN-PIPE-W-OFF LDRW,
   9 SP 32 LDR,  9 >REG 0 >FD 14 >REG LINUX-DUP2-FD
   14 SP LINUX-SPAWN-PIPE-W-OFF LDRW,
   9 SP 40 LDR,  9 >REG 1 >FD 14 >REG LINUX-DUP2-FD
   14 SP LINUX-SPAWN-PIPE-W-OFF LDRW,
   9 SP 48 LDR,  9 >REG 2 >FD 14 >REG LINUX-DUP2-FD
   32 LINUX-CLOSE-SOURCE
   40 LINUX-CLOSE-SOURCE
   48 LINUX-CLOSE-SOURCE
   0 SP 0 LDR,  1 SP 8 LDR,  2 SP 16 LDR,
   NR-EXECVE SYS,
   14 SP LINUX-SPAWN-PIPE-W-OFF LDRW,
   14 >REG LINUX-SPAWN-FAIL ;

: LINUX-SPAWN ( reg reg reg reg reg reg reg -- )
   REG>N LNX-ERR !
   REG>N LNX-OUT !
   REG>N LNX-IN !
   REG>N LNX-CWD !
   REG>N LNX-ENV !
   REG>N LNX-ARGV !
   REG>N LNX-PATH !
   LBL {: child:label :}
   LBL {: closefail:label :}
   LBL {: fail:label :}
   LBL {: done:label :}
   SP SP LINUX-SPAWN-FRAME SUBI,
   LNX-PATH @ SP 0 STR,  LNX-ARGV @ SP 8 STR,  LNX-ENV @ SP 16 STR,  LNX-CWD @ SP 24 STR,
   LNX-IN @ SP 32 STR,  LNX-OUT @ SP 40 STR,  LNX-ERR @ SP 48 STR,
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

\ spawn-fds on Linux. The frame is LINUX-SPAWN's: the request's cells copied
\ to SPAWN-ABI's offsets, the lift floor in the cell at SPAWN-ABI:BYTES, then
\ the error pipe, the child's errno (four bytes), the pid and the wait status.
4 constant LINUX-CLOSE-RANGE-CLOEXEC
3 constant SPAWN-PROT-RW                \ PROT_READ | PROT_WRITE
22 constant SPAWN-EINVAL
4 constant SPAWN-EINTR
4 constant SPAWN-ERRNO-BYTES

\ The child, frame at SP. Every descriptor is marked close-on-exec first, so a
\ -1 slot and every fd at or above NFDS close at exec. Each source is then
\ lifted to a fresh fd at or above the floor in the cell past the request,
\ kept in a scratch array, before any slot is placed, so a source that is
\ another slot's target (map [0 2 1]) survives until it is read, and no lifted
\ copy takes the number of a later source the parent has closed; dup3 with no
\ flags places each one and clears its mark. A failing step writes its errno to
\ the pipe and exits 127.
: LINUX-SPAWN-FDS-CHILD ( -- )
   LBL {: fail:label :}
   LBL {: nocwd:label :}
   LBL {: lift:label :}
   LBL {: keep:label :}
   LBL {: lifted:label :}
   LBL {: place:label :}
   LBL {: skip:label :}
   LBL {: placed:label :}
   LINUX-SPAWN-CLOSE-R
   0 0 MOVZ,  1 0 MOVZ,  NR-SETPGID SYS,  C-CS fail BCOND,
   0 SP SPAWN-ABI:CWD LDR,  0 nocwd CBZ,
      NR-CHDIR SYS,  C-CS fail BCOND,
   nocwd LBL,
   0 0 MOVZ,  1 0 MOVN,  2 LINUX-CLOSE-RANGE-CLOEXEC MOVZ,
   NR-CLOSE-RANGE SYS,  C-CS fail BCOND,
   0 0 MOVZ,  1 SP SPAWN-ABI:NFDS LDR,  1 1 3 LSLI,  1 1 8 ADDI,
   2 SPAWN-PROT-RW MOVZ,  3 MAP-ANON-PRIVATE MOVZ,  4 0 MOVN,  5 0 MOVZ,
   NR-MMAP SYS,  C-CS fail BCOND,
   13 0 0 ADDI,
   10 SP SPAWN-ABI:NFDS LDR,  11 SP SPAWN-ABI:FDS LDR,  15 13 0 ADDI,
   lift LBL,
      10 lifted CBZ,
      9 11 0 LDR,  9 0 CMPI,  C-LT keep BCOND,
      0 9 0 ADDI,  1 LINUX-F-DUPFD-CLOEXEC MOVZ,  2 SP SPAWN-ABI:BYTES LDR,
      NR-FCNTL SYS,  C-CS fail BCOND,
      9 0 0 ADDI,
   keep LBL,
      9 15 0 STR,
      11 11 8 ADDI,  15 15 8 ADDI,  10 10 1 SUBI,
      lift B,
   lifted LBL,
   10 0 MOVZ,  12 SP SPAWN-ABI:NFDS LDR,  15 13 0 ADDI,
   place LBL,
      10 12 CMP,  C-GE placed BCOND,
      0 15 0 LDR,  0 0 CMPI,  C-LT skip BCOND,
      1 10 0 ADDI,  2 0 MOVZ,  NR-DUP2 SYS,  C-CS fail BCOND,
   skip LBL,
      15 15 8 ADDI,  10 10 1 ADDI,
      place B,
   placed LBL,
   0 SP SPAWN-ABI:PATH LDR,  1 SP SPAWN-ABI:ARGV LDR,  2 SP SPAWN-ABI:ENVP LDR,
   NR-EXECVE SYS,
   fail LBL,
   9 31 0 SUB,  9 SP LINUX-SPAWN-ERR-OFF STRW,
   0 SP LINUX-SPAWN-PIPE-W-OFF LDRW,  1 SP LINUX-SPAWN-ERR-OFF ADDI,
   2 SPAWN-ERRNO-BYTES MOVZ,  NR-WRITE SYS,
   0 127 MOVZ,  NR-EXIT-GROUP SYS, ;

\ The parent, with the child's pid in x0: read the pipe until end of file (an
\ execve that worked: x9 becomes the pid) or the child's errno (x9 becomes
\ -errno and the child is reaped). An interrupted read is retried; any other
\ failed read is its -errno, and the child is reaped.
: LINUX-SPAWN-FDS-PARENT ( -- )
   LBL {: retry:label :}
   LBL {: got:label :}
   LBL {: reap:label :}
   LBL {: done:label :}
   0 SP LINUX-SPAWN-PID-OFF STR,
   LINUX-SPAWN-CLOSE-W
   31 SP LINUX-SPAWN-ERR-OFF STRW,
   retry LBL,
   0 SP LINUX-SPAWN-PIPE-R-OFF LDRW,  1 SP LINUX-SPAWN-ERR-OFF ADDI,
   2 SPAWN-ERRNO-BYTES MOVZ,  NR-READ SYS,
   C-CC got BCOND,
      14 0 SPAWN-EINTR ADDI,  14 retry CBZ,
      9 0 0 ADDI,  reap B,
   got LBL,
   9 SP LINUX-SPAWN-PID-OFF LDR,  0 done CBZ,
   9 SP LINUX-SPAWN-ERR-OFF LDRW,  9 31 9 SUB,
   reap LBL,
   LINUX-SPAWN-WAIT-STORED
   done LBL,
   LINUX-SPAWN-CLOSE-R ;

\ The request's address is in x10. x9 becomes the child's pid or -errno. The
\ floor, max(NFDS, highest source + 1), goes in the cell past the request, and
\ the error pipe's write end is lifted to it or above: no slot targets it and no
\ source names it, so a closed source fd is EBADF in the child rather than the
\ pipe placed onto a slot.
: LINUX-SPAWN-FDS ( -- )
   LBL {: child:label :}
   LBL {: closefail:label :}
   LBL {: leave:label :}
   LBL {: top:label :}
   LBL {: below:label :}
   LBL {: topped:label :}
   SP SP LINUX-SPAWN-FRAME SUBI,
   SPAWN-ABI:BYTES 0 ?do
      11 10 i LDR,  11 SP i STR,
   8 +loop
   0 SP LINUX-SPAWN-PIPE-R-OFF ADDI,  1 LINUX-O-CLOEXEC LIT64,  NR-PIPE SYS,
   9 0 0 ADDI,  C-CS leave BCOND,
   12 SP SPAWN-ABI:NFDS LDR,  13 12 0 ADDI,  11 SP SPAWN-ABI:FDS LDR,
   top LBL,
      13 topped CBZ,
      9 11 0 LDR,  9 12 CMP,  C-LT below BCOND,
      12 9 1 ADDI,
   below LBL,
      11 11 8 ADDI,  13 13 1 SUBI,
      top B,
   topped LBL,
   12 SP SPAWN-ABI:BYTES STR,  LINUX-SPAWN-LIFT-W
   9 closefail CBNZ,
   0 17 MOVZ,  1 0 MOVZ,  2 0 MOVZ,  3 0 MOVZ,  4 0 MOVZ,
   NR-SPAWN SYS,
   9 0 0 ADDI,  C-CS closefail BCOND,
   0 child CBZ,
      LINUX-SPAWN-FDS-PARENT
      leave B,
   child LBL,
      LINUX-SPAWN-FDS-CHILD
   closefail LBL,
      LINUX-SPAWN-CLOSE-PIPE
   leave LBL,
   SP SP LINUX-SPAWN-FRAME ADDI, ;

: BRUNRC ( -- )                    \ ( pathz -- rc ) spawn+wait; -1 = spawn failed
   A G-POP
   LBL {: ok:label :}
   LBL {: done:label :}
   LBL {: waitok:label :}
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
      9 0 CMPI,  C-LT done BCOND,
      0 9 0 ADDI,
      1 SP 8 ADDI,  2 0 MOVZ,  3 0 MOVZ,
      NR-WAIT4 SYS,
      10 C-CS CSET,  10 waitok CBZ,
         9 0 MOVN,  done B,
      waitok LBL,
      9 SP 8 LDRW,
      9 9 8 LSRI,  9 9 $FF ANDI,
      done LBL,
      9 G-PUSH
      SP SP 64 ADDI,
      exit
   THEN
   HB-TARGET-MACOS? IF
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
      9 ok CBZ,                         \ either -> rc -1
         9 0 MOVN,  done B,
      ok LBL,
      0 SP 0 LDR,                       \ pid
      1 SP 8 ADDI,  2 0 MOVZ,  3 0 MOVZ,
      NR-WAIT4 SYS,
      9 2 CSET,  9 waitok CBZ,          \ wait4 error (no child) -> rc -1
         9 0 MOVN,  done B,
      waitok LBL,
      9 SP 8 LDRW,
      9 9 8 LSRI,  9 9 $FF ANDI,        \ WEXITSTATUS
      done LBL,
      9 G-PUSH
      SP SP 64 ADDI,
      exit
   THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

: BPIPE ( -- )                     \ ( -- rfd wfd rc ) rc=0, or -1 -1 -1
   LBL LNX-OK !
   LBL LNX-DONE !
   HB-TARGET-LINUX? IF
      SP SP 16 SUBI,
      0 SP 0 ADDI,  1 0 MOVZ,  NR-PIPE SYS,
      9 C-CS CSET,  9 LNX-OK LABEL@ CBZ,
         9 0 MOVN,  9 G-PUSH  9 G-PUSH  9 G-PUSH  LNX-DONE LABEL@ B,
      LNX-OK LABEL@ LBL,
      0 SP 0 LDRW,  1 SP 4 LDRW,
      0 G-PUSH  1 G-PUSH  9 0 MOVZ,  9 G-PUSH
      LNX-DONE LABEL@ LBL,
      SP SP 16 ADDI,
      exit
   THEN
   HB-TARGET-MACOS? IF
      NR-PIPE SYS,
      9 C-CS CSET,  9 LNX-OK LABEL@ CBZ,
         9 0 MOVN,  9 G-PUSH  9 G-PUSH  9 G-PUSH  LNX-DONE LABEL@ B,
      LNX-OK LABEL@ LBL,
      0 G-PUSH  1 G-PUSH  9 0 MOVZ,  9 G-PUSH
      LNX-DONE LABEL@ LBL,
      exit
   THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

: BDUP2 ( -- )                     \ ( oldfd newfd -- rc ) rc=newfd or -1
   1 G-POP  0 G-POP
   LBL LNX-OK !
   LBL LNX-DONE !
   HB-TARGET-LINUX? IF
      2 0 MOVZ,
      NR-DUP2 SYS,
      9 C-CS CSET,  9 LNX-OK LABEL@ CBZ,
         0 0 MOVN,  LNX-DONE LABEL@ B,
      LNX-OK LABEL@ LBL,
      LNX-DONE LABEL@ LBL,
      0 G-PUSH
      exit
   THEN
   HB-TARGET-MACOS? IF
      NR-DUP2 SYS,
      9 C-CS CSET,  9 LNX-OK LABEL@ CBZ,
         0 0 MOVN,  LNX-DONE LABEL@ B,
      LNX-OK LABEL@ LBL,
      LNX-DONE LABEL@ LBL,
      0 G-PUSH
      exit
   THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

13 constant LINUX-SIGPIPE
1 constant LINUX-SIG-IGN
8 constant LINUX-SIGSET-SIZE

: LINUX-IGNORE-SIGPIPE ( -- )
   LBL {: ok:label :}
   LBL {: done:label :}
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

: BFCNTL ( -- )                    \ ( fd cmd arg -- rc ) rc=sysret or -1
   2 G-POP  1 G-POP  0 G-POP
   LBL {: ok:label :}
   LBL {: done:label :}
   LBL LNX-REAL !
   HB-TARGET-LINUX? IF
      1 73 CMPI,  C-NE LNX-REAL LABEL@ BCOND,
         LINUX-IGNORE-SIGPIPE
         done B,
      LNX-REAL LABEL@ LBL,
      NR-FCNTL SYS,
      9 C-CS CSET,  9 ok CBZ,
         0 0 MOVN,  done B,
      ok LBL,
      done LBL,
      0 G-PUSH
      exit
   THEN
   HB-TARGET-MACOS? IF
      NR-FCNTL SYS,
      9 C-CS CSET,  9 ok CBZ,
         0 0 MOVN,  done B,
      ok LBL,
      done LBL,
      0 G-PUSH
      exit
   THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

\ Every syscall wrapper here collapses a failure to -1 (`0 0 MOVN`): its callers
\ only ask whether the call worked. BPOLL alone publishes -errno: poll(2) is the
\ one blocking call the kernel never restarts. read, write and wait4 restart
\ under SA_RESTART, which the engine's only handler sets (src/habu/prof-abi.f
\ LINUX-SA-PROF-FLAGS); a handler without it would need -errno on those too.
\ Linux leaves 0/-errno in x0; macOS sets carry with a positive errno, negated
\ here.
: BPOLL ( -- )                     \ ( fds nfds timeout -- rc ) rc=nready/0 or -errno
   2 G-POP  1 G-POP  0 G-POP
   LBL LBL {: plen:label pguard:label :}
   6 1 61 LSRI,  6 plen CBZ,                  \ nfds*8 overflow becomes an all-address span
      6 0 MOVN,  pguard B,
   plen LBL,  6 1 3 LSLI,
   pguard LBL,  0 6 PROT-GUARD:CALL             \ pollfd array: nfds * 8 bytes
   LBL LNX-PNEG !
   LBL LNX-PCALL !
   HB-TARGET-LINUX? IF
      SP SP 32 SUBI,
      2 0 CMPI,  C-LT LNX-PNEG LABEL@ BCOND,
         5 1000 MOVZ,  6 2 5 UDIV,
         7 6 5 MUL,  7 2 7 SUB,
         5 1000 MOVZ,  7 7 5 MUL,  5 1000 MOVZ,  7 7 5 MUL,
         6 SP 0 STR,  7 SP 8 STR,
         2 SP 0 ADDI,  LNX-PCALL LABEL@ B,
      LNX-PNEG LABEL@ LBL,
         2 0 MOVZ,
      LNX-PCALL LABEL@ LBL,
      3 0 MOVZ,  4 0 MOVZ,
      NR-POLL SYS,
      0 G-PUSH                                  \ x0 already holds nready/0 or -errno
      SP SP 32 ADDI,
      exit
   THEN
   HB-TARGET-MACOS? IF
      LBL {: ok:label :}
      NR-POLL SYS,
      9 C-CS CSET,  9 ok CBZ,          \ carry clear -> x0 already holds nready or 0
         10 0 MOVZ,  0 10 0 SUB,        \ carry set -> x0 = 0 - errno = -errno
      ok LBL,
      0 G-PUSH
      exit
   THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

: BKILL ( -- )                     \ ( pid sig -- rc ) rc=0 or -1
   1 G-POP  0 G-POP
   LBL LNX-OK !
   LBL LNX-DONE !
   NR-KILL SYS,
   9 C-CS CSET,  9 LNX-OK LABEL@ CBZ,
      0 0 MOVN,  LNX-DONE LABEL@ B,
   LNX-OK LABEL@ LBL,
   LNX-DONE LABEL@ LBL,
   0 G-PUSH ;

: BSETPGID ( -- )                  \ ( pid pgid -- rc ) rc=0 or -1
   1 G-POP  0 G-POP
   LBL LNX-OK !
   LBL LNX-DONE !
   NR-SETPGID SYS,
   9 C-CS CSET,  9 LNX-OK LABEL@ CBZ,
      0 0 MOVN,  LNX-DONE LABEL@ B,
   LNX-OK LABEL@ LBL,
   LNX-DONE LABEL@ LBL,
   0 G-PUSH ;

: BWAITSTATUS ( -- )               \ ( pid -- status ) wait4 raw status; -1 = wait failed
   A G-POP
   LBL LNX-OK !
   LBL LNX-DONE !
   SP SP 16 SUBI,
   0 9 0 ADDI,
   1 SP 0 ADDI,  2 0 MOVZ,  3 0 MOVZ,
   NR-WAIT4 SYS,
   9 C-CS CSET,  9 LNX-OK LABEL@ CBZ,
      9 0 MOVN,  LNX-DONE LABEL@ B,
   LNX-OK LABEL@ LBL,
   9 SP 0 LDRW,
   LNX-DONE LABEL@ LBL,
   9 G-PUSH
   SP SP 16 ADDI, ;

1040 constant SPAWN-ACTION-SIZE
7 constant SPAWN-ACTION-CAP            \ a chdir, three stdio records, three closes
0 constant SPAWN-PID-OFF
16 constant SPAWN-ARGV-OFF
24 constant SPAWN-ARGV-END-OFF
32 constant SPAWN-ENVP-OFF
48 constant SPAWN-ADESC-OFF
48 constant SPAWN-ADESC-ATTR-SIZE-OFF
56 constant SPAWN-ADESC-ATTR-PTR-OFF
64 constant SPAWN-ADESC-FA-SIZE-OFF
72 constant SPAWN-ADESC-FA-PTR-OFF
128 constant SPAWN-ADESC-SIZE
176 constant SPAWN-ATTR-OFF
184 constant SPAWN-ATTR-SIZE
0 constant SPAWN-ATTR-FLAGS-OFF
56 constant SPAWN-ATTR-RESERVED-OFF
68 constant SPAWN-ATTR-PRIORITY-OFF
72 constant SPAWN-ATTR-MEMLIMIT-ACTIVE-OFF
76 constant SPAWN-ATTR-MEMLIMIT-INACTIVE-OFF
112 constant SPAWN-ATTR-SUBCPU-OFF
2 constant POSIX-SPAWN-SETPGROUP
368 constant SPAWN-ACTIONS-OFF
0 constant SPAWN-FA-CAP-OFF
4 constant SPAWN-FA-COUNT-OFF
8 constant SPAWN-FA-ACTS-OFF
1 constant PSFA-CLOSE
2 constant PSFA-DUP2
3 constant PSFA-INHERIT
5 constant PSFA-CHDIR
8 constant SPAWN-CHDIR-PATH-OFF
SPAWN-ACTION-SIZE SPAWN-CHDIR-PATH-OFF - constant SPAWN-CHDIR-PATH-CAP

\ The frame ends with room for SPAWN-ACTION-CAP records, rounded to 16 bytes.
\ SUBI takes at most 4095, so it is entered in two steps.
SPAWN-ACTIONS-OFF SPAWN-FA-ACTS-OFF + SPAWN-ACTION-CAP SPAWN-ACTION-SIZE * +
   15 + -16 and constant SPAWN-FRAME
4080 constant SPAWN-FRAME-STEP

variable SDA-FD  variable SDA-NEW  variable SDA-SKIP
variable SCA-CWD  variable SCA-FAIL
variable SCA-COPY  variable SCA-OVER  variable SCA-DONE
variable SAD-HAS
variable SPD-PATH  variable SAE-ARGV  variable SAE-ENVP
variable SDEF-PATH  variable SADV-ARGV
variable SFIN-OK  variable SFIN-FAIL
variable BSP-OK  variable BSP-DN  variable BSP-SAD
variable SZA-I

\ Point x14 at the next record of the runtime file-actions blob at x13.
: SPAWN-NEXT-ACTION ( -- )
   14 13 SPAWN-FA-COUNT-OFF LDRW,  15 SPAWN-ACTION-SIZE MOVZ,  14 14 15 MUL,
   14 14 SPAWN-FA-ACTS-OFF ADDI,  14 14 13 ADD, ;

: SPAWN-COUNT-ACTION ( -- )
   14 13 SPAWN-FA-COUNT-OFF LDRW,  14 14 1 ADDI,  14 13 SPAWN-FA-COUNT-OFF STRW, ;

: SPAWN-DUP2-ARGS ( reg fd -- )
   SDA-NEW !  SDA-FD ! ;

\ Emit the record that puts the descriptor in reg on fd n: PSFA_DUP2, or
\ PSFA_INHERIT when it already sits on n, since xnu's dup2 of a descriptor onto
\ itself keeps close-on-exec. A negative descriptor emits nothing.
: SPAWN-DUP2-ACTION ( reg fd -- )
   SPAWN-DUP2-ARGS
   LBL SDA-SKIP !
   SDA-FD @ 0 CMPI,  C-LT SDA-SKIP LABEL@ BCOND,
   SPAWN-NEXT-ACTION
   15 PSFA-DUP2 MOVZ,  16 PSFA-INHERIT MOVZ,
   SDA-FD @ SDA-NEW @ CMPI,  15 16 15 C-EQ CSEL,
   15 14 0 STRW,
   SDA-FD @ 14 4 STRW,
   15 SDA-NEW @ MOVZ,  15 14 8 STRW,
   SPAWN-COUNT-ACTION
   SDA-SKIP LABEL@ LBL, ;

\ Emit a PSFA_CLOSE record for the stdio source in reg (x10, x11 or x12), after
\ the three stdio records. No record for a source at or below 2 (-1 or already
\ a stdio number) or for one an earlier stdio register also holds, whose record
\ closes it: xnu fails the whole spawn when a close action fails, so each
\ source is closed exactly once.
: SPAWN-CLOSE-ACTION ( reg -- )
   REG>N {: r:n :}
   LBL {: skip:label :}
   r 2 CMPI,  C-LE skip BCOND,
   10 BEGIN dup r < WHILE
      r over CMP,  C-EQ skip BCOND,
      1+
   REPEAT drop
   SPAWN-NEXT-ACTION
   15 PSFA-CLOSE MOVZ,  15 14 0 STRW,
   r 14 4 STRW,
   SPAWN-COUNT-ACTION
   skip LBL, ;

\ Emit one PSFA_CHDIR record into the runtime file-actions blob at x13.
: SPAWN-CHDIR-ARGS ( reg label -- )
   SCA-FAIL !  SCA-CWD ! ;

: SPAWN-CHDIR-LABELS ( -- )
   LBL SCA-COPY !  LBL SCA-OVER !  LBL SCA-DONE ! ;

: SPAWN-CHDIR-ACTION ( reg label -- )
   SPAWN-CHDIR-ARGS
   SPAWN-CHDIR-LABELS
   SPAWN-NEXT-ACTION
   15 PSFA-CHDIR MOVZ,  15 14 0 STRW,
   16 SCA-CWD @ 0 ADDI,
   17 14 SPAWN-CHDIR-PATH-OFF ADDI,
   5 SPAWN-CHDIR-PATH-CAP MOVZ,
   SCA-COPY LABEL@ LBL,
      5 0 CMPI,  C-EQ SCA-OVER LABEL@ BCOND,
      15 16 0 LDRB,
      15 17 0 STRB,
      16 16 1 ADDI,
      17 17 1 ADDI,
      5 5 1 SUBI,
      15 SCA-COPY LABEL@ CBNZ,
   SPAWN-COUNT-ACTION
   SCA-DONE LABEL@ B,
   SCA-OVER LABEL@ LBL,
   9 0 MOVN,  SCA-FAIL LABEL@ B,
   SCA-DONE LABEL@ LBL, ;

: SPAWN-DARWIN-FRAME-ENTER ( -- )
   SP SP SPAWN-FRAME-STEP SUBI,
   SP SP SPAWN-FRAME SPAWN-FRAME-STEP - SUBI, ;

: SPAWN-DARWIN-FRAME-LEAVE ( -- )
   SP SP SPAWN-FRAME SPAWN-FRAME-STEP - ADDI,
   SP SP SPAWN-FRAME-STEP ADDI, ;

: SPAWN-DARWIN-ACTIONS-RESET ( -- )
   13 SP SPAWN-ACTIONS-OFF ADDI,
   14 SPAWN-ACTION-CAP MOVZ,  14 13 SPAWN-FA-CAP-OFF STRW,
   14 0 MOVZ,  14 13 SPAWN-FA-COUNT-OFF STRW, ;

\ The stdio records, then the closes, so a source two roles share is closed
\ after both records read it.
: SPAWN-DARWIN-STDIO-ACTIONS ( -- )
   10 >REG 0 >FD SPAWN-DUP2-ACTION
   11 >REG 1 >FD SPAWN-DUP2-ACTION
   12 >REG 2 >FD SPAWN-DUP2-ACTION
   10 >REG SPAWN-CLOSE-ACTION
   11 >REG SPAWN-CLOSE-ACTION
   12 >REG SPAWN-CLOSE-ACTION ;

: SPAWN-DARWIN-ZERO-ADESC ( -- )
   14 0 MOVZ,
   0 SZA-I !
   BEGIN SZA-I @ SPAWN-ADESC-SIZE < WHILE
      14 SP SPAWN-ADESC-OFF SZA-I @ + STR,
      SZA-I @ 8 + SZA-I !
   REPEAT ;

: SPAWN-DARWIN-ZERO-ATTR ( -- )
   14 0 MOVZ,
   0 SZA-I !
   BEGIN SZA-I @ SPAWN-ATTR-SIZE < WHILE
      14 SP SPAWN-ATTR-OFF SZA-I @ + STR,
      SZA-I @ 8 + SZA-I !
   REPEAT ;

: SPAWN-DARWIN-ATTR-DEFAULTS ( -- )
   14 POSIX-SPAWN-SETPGROUP MOVZ,
   14 SP SPAWN-ATTR-OFF SPAWN-ATTR-FLAGS-OFF + STRW,
   14 1 MOVZ,
   14 SP SPAWN-ATTR-OFF SPAWN-ATTR-RESERVED-OFF + STR,
   14 0 MOVN,
   14 SP SPAWN-ATTR-OFF SPAWN-ATTR-PRIORITY-OFF + STRW,
   14 SP SPAWN-ATTR-OFF SPAWN-ATTR-MEMLIMIT-ACTIVE-OFF + STRW,
   14 SP SPAWN-ATTR-OFF SPAWN-ATTR-MEMLIMIT-INACTIVE-OFF + STRW,
   14 SP SPAWN-ATTR-OFF SPAWN-ATTR-SUBCPU-OFF + STRW,
   14 SP SPAWN-ATTR-OFF SPAWN-ATTR-SUBCPU-OFF 4 + + STRW,
   14 SP SPAWN-ATTR-OFF SPAWN-ATTR-SUBCPU-OFF 8 + + STRW,
   14 SP SPAWN-ATTR-OFF SPAWN-ATTR-SUBCPU-OFF 12 + + STRW, ;

: SPAWN-DARWIN-FILL-ADESC ( -- )
   LBL {: done:label :}
   SPAWN-DARWIN-ATTR-DEFAULTS
   14 SPAWN-ATTR-SIZE MOVZ,
   14 SP SPAWN-ADESC-ATTR-SIZE-OFF STR,
   14 SP SPAWN-ATTR-OFF ADDI,
   14 SP SPAWN-ADESC-ATTR-PTR-OFF STR,
   14 13 SPAWN-FA-COUNT-OFF LDRW,
   14 done CBZ,
   15 SPAWN-ACTION-SIZE MOVZ,
   14 14 15 MUL,
   14 14 SPAWN-FA-ACTS-OFF ADDI,
   14 SP SPAWN-ADESC-FA-SIZE-OFF STR,
   13 SP SPAWN-ADESC-FA-PTR-OFF STR,
   done LBL, ;

: SPAWN-DARWIN-USE-ADESC ( -- )
   2 SP SPAWN-ADESC-OFF ADDI, ;

: SPAWN-DARWIN-PID-PATH ( reg -- )
   SPD-PATH !
   0 SP SPAWN-PID-OFF ADDI,
   1 SPD-PATH @ 0 ADDI, ;

: SPAWN-DARWIN-ARGV-ENVP ( reg reg -- )
   SAE-ENVP !  SAE-ARGV !
   3 SAE-ARGV @ 0 ADDI,
   4 SAE-ENVP @ 0 ADDI, ;

: SPAWN-DARWIN-DEFAULT-ARGV-ENVP ( reg -- )
   SDEF-PATH !
   SDEF-PATH @ SP SPAWN-ARGV-OFF STR,
   14 0 MOVZ,
   14 SP SPAWN-ARGV-END-OFF STR,
   14 SP SPAWN-ENVP-OFF STR, ;

: SPAWN-DARWIN-DEFAULT-ENVP ( -- )
   14 0 MOVZ,
   14 SP SPAWN-ARGV-OFF STR, ;

: SPAWN-DARWIN-USE-DEFAULT-ARGV-ENVP ( -- )
   3 SP SPAWN-ARGV-OFF ADDI,
   4 SP SPAWN-ENVP-OFF ADDI, ;

: SPAWN-DARWIN-ARGV-DEFAULT-ENVP ( reg -- )
   SADV-ARGV !
   3 SADV-ARGV @ 0 ADDI,
   4 SP SPAWN-ARGV-OFF ADDI, ;

: SPAWN-DARWIN-FINISH ( label label -- )
   SFIN-FAIL !  SFIN-OK !
   NR-SPAWN SYS,
   9 C-CS CSET,  9 SFIN-OK LABEL@ CBZ,
      9 0 MOVZ,  9 9 0 SUB,  SFIN-FAIL LABEL@ B,
   SFIN-OK LABEL@ LBL,
   9 SP SPAWN-PID-OFF LDRW,
   SFIN-FAIL LABEL@ LBL,
   9 G-PUSH ;

: BSP-LABELS3 ( -- )
   LBL BSP-OK !  LBL BSP-DN !  LBL BSP-SAD ! ;

: BSP-LABELS2 ( -- )
   LBL BSP-OK !  LBL BSP-DN ! ;

: BSPAWNIO ( -- )                  \ ( pathz stdinfd stdoutfd stderrfd -- pid|-errno )
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
   HB-TARGET-MACOS? IF
      SPAWN-DARWIN-FRAME-ENTER
      9 >REG SPAWN-DARWIN-DEFAULT-ARGV-ENVP
      SPAWN-DARWIN-ACTIONS-RESET
      SPAWN-DARWIN-STDIO-ACTIONS
      SPAWN-DARWIN-ZERO-ADESC
      SPAWN-DARWIN-ZERO-ATTR
      SPAWN-DARWIN-FILL-ADESC
      9 >REG SPAWN-DARWIN-PID-PATH
      SPAWN-DARWIN-USE-ADESC
      SPAWN-DARWIN-USE-DEFAULT-ARGV-ENVP
      BSP-OK @ >LABEL BSP-DN @ >LABEL SPAWN-DARWIN-FINISH
      SPAWN-DARWIN-FRAME-LEAVE
      exit
   THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

: BSPAWNARGVIO ( -- )              \ ( pathz argvp stdinfd stdoutfd stderrfd -- pid|-errno )
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
   HB-TARGET-MACOS? IF
      SPAWN-DARWIN-FRAME-ENTER
      SPAWN-DARWIN-DEFAULT-ENVP
      SPAWN-DARWIN-ACTIONS-RESET
      SPAWN-DARWIN-STDIO-ACTIONS
      SPAWN-DARWIN-ZERO-ADESC
      SPAWN-DARWIN-ZERO-ATTR
      SPAWN-DARWIN-FILL-ADESC
      8 >REG SPAWN-DARWIN-PID-PATH
      SPAWN-DARWIN-USE-ADESC
      9 >REG SPAWN-DARWIN-ARGV-DEFAULT-ENVP
      BSP-OK @ >LABEL BSP-DN @ >LABEL SPAWN-DARWIN-FINISH
      SPAWN-DARWIN-FRAME-LEAVE
      exit
   THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

: BSPAWNARGVENVIO ( -- )           \ ( pathz argvp envp stdinfd stdoutfd stderrfd -- pid|-errno )
   BSP-LABELS3
   12 G-POP  11 G-POP  10 G-POP  7 G-POP  9 G-POP  8 G-POP
   HB-TARGET-LINUX? IF
      13 0 MOVN,
      8 >REG 9 >REG 7 >REG 13 >REG 10 >REG 11 >REG 12 >REG LINUX-SPAWN
      exit
   THEN
   HB-TARGET-MACOS? IF
      SPAWN-DARWIN-FRAME-ENTER
      SPAWN-DARWIN-ACTIONS-RESET
      SPAWN-DARWIN-STDIO-ACTIONS
      SPAWN-DARWIN-ZERO-ADESC
      SPAWN-DARWIN-ZERO-ATTR
      SPAWN-DARWIN-FILL-ADESC
      8 >REG SPAWN-DARWIN-PID-PATH
      SPAWN-DARWIN-USE-ADESC
      9 >REG 7 >REG SPAWN-DARWIN-ARGV-ENVP
      BSP-OK @ >LABEL BSP-DN @ >LABEL SPAWN-DARWIN-FINISH
      SPAWN-DARWIN-FRAME-LEAVE
      exit
   THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

: BSPAWNARGVENVCWDIO ( -- )        \ ( pathz argvp envp cwdz stdinfd stdoutfd stderrfd -- pid|-errno )
   BSP-LABELS2
   12 G-POP  11 G-POP  10 G-POP  6 G-POP  7 G-POP  9 G-POP  8 G-POP
   HB-TARGET-LINUX? IF
      8 >REG 9 >REG 7 >REG 6 >REG 10 >REG 11 >REG 12 >REG LINUX-SPAWN
      exit
   THEN
   HB-TARGET-MACOS? IF
      SPAWN-DARWIN-FRAME-ENTER
      SPAWN-DARWIN-ACTIONS-RESET
      6 >REG BSP-DN @ >LABEL SPAWN-CHDIR-ACTION
      SPAWN-DARWIN-STDIO-ACTIONS
      SPAWN-DARWIN-ZERO-ADESC
      SPAWN-DARWIN-ZERO-ATTR
      SPAWN-DARWIN-FILL-ADESC
      8 >REG SPAWN-DARWIN-PID-PATH
      SPAWN-DARWIN-USE-ADESC
      9 >REG 7 >REG SPAWN-DARWIN-ARGV-ENVP
      BSP-OK @ >LABEL BSP-DN @ >LABEL SPAWN-DARWIN-FINISH
      SPAWN-DARWIN-FRAME-LEAVE
      exit
   THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

\ spawn-fds on macOS. The frame keeps SPAWN-FRAME's descriptor, attributes and
\ pid offsets; the file-actions blob, three records per slot plus a chdir, is
\ mapped for the call, and its address and size sit where SPAWN-FRAME's own
\ blob starts, followed by the request, which the mmap's SYS, may clobber.
SPAWN-ACTIONS-OFF constant SPAWN-FDS-BLOB-OFF
SPAWN-ACTIONS-OFF 8 + constant SPAWN-FDS-BLOB-U-OFF
SPAWN-ACTIONS-OFF 16 + constant SPAWN-FDS-REQ-OFF
SPAWN-ACTIONS-OFF 32 + constant SPAWN-FDS-FRAME
3 constant SPAWN-FDS-SLOT-ACTIONS       \ the lift, the place, the temporary's close
$4000 constant POSIX-SPAWN-CLOEXEC-DEFAULT
63 constant SPAWN-ENAMETOOLONG

\ The request's address is in x8; x9 becomes the child's pid or -errno.
\ POSIX_SPAWN_CLOEXEC_DEFAULT closes every fd no record targets: a -1 slot and
\ every fd at or above NFDS. Each source is first duplicated to T+i, T above
\ NFDS and every source, so a source that is another slot's target (map
\ [0 2 1]) survives until it is read; then T+i goes to i and is closed.
: DARWIN-SPAWN-FDS ( -- )
   LBL {: mapped:label :}
   LBL {: nocwd:label :}
   LBL {: toolong:label :}
   LBL {: top:label :}
   LBL {: below:label :}
   LBL {: topped:label :}
   LBL {: lift:label :}
   LBL {: lskip:label :}
   LBL {: lifted:label :}
   LBL {: place:label :}
   LBL {: pskip:label :}
   LBL {: placed:label :}
   LBL {: spawned:label :}
   LBL {: unmap:label :}
   LBL {: leave:label :}
   SP SP SPAWN-FDS-FRAME SUBI,
   8 SP SPAWN-FDS-REQ-OFF STR,
   6 8 SPAWN-ABI:NFDS LDR,
   7 6 1 LSLI,  7 7 6 ADD,  7 7 1 ADDI,
   1 SPAWN-ACTION-SIZE MOVZ,  1 7 1 MUL,  1 1 SPAWN-FA-ACTS-OFF ADDI,
   1 SP SPAWN-FDS-BLOB-U-OFF STR,
   0 0 MOVZ,  2 SPAWN-PROT-RW MOVZ,  3 MAP-ANON-PRIVATE MOVZ,  4 0 MOVN,  5 0 MOVZ,
   NR-MMAP SYS,
   C-CC mapped BCOND,
      9 31 0 SUB,  leave B,
   mapped LBL,
   8 SP SPAWN-FDS-REQ-OFF LDR,
   0 SP SPAWN-FDS-BLOB-OFF STR,  13 0 0 ADDI,
   7 13 SPAWN-FA-CAP-OFF STRW,
   5 8 SPAWN-ABI:CWD LDR,  5 nocwd CBZ,
      5 >REG toolong SPAWN-CHDIR-ACTION
   nocwd LBL,
   12 6 0 ADDI,  11 8 SPAWN-ABI:FDS LDR,  10 6 0 ADDI,
   top LBL,
      10 topped CBZ,
      9 11 0 LDR,  9 12 CMP,  C-LT below BCOND,
      12 9 1 ADDI,
   below LBL,
      11 11 8 ADDI,  10 10 1 SUBI,
      top B,
   topped LBL,
   10 0 MOVZ,  11 8 SPAWN-ABI:FDS LDR,
   lift LBL,
      10 6 CMP,  C-GE lifted BCOND,
      9 11 0 LDR,  9 0 CMPI,  C-LT lskip BCOND,
      SPAWN-NEXT-ACTION
      15 PSFA-DUP2 MOVZ,  15 14 0 STRW,  9 14 4 STRW,
      15 12 10 ADD,  15 14 8 STRW,
      SPAWN-COUNT-ACTION
   lskip LBL,
      11 11 8 ADDI,  10 10 1 ADDI,
      lift B,
   lifted LBL,
   10 0 MOVZ,  11 8 SPAWN-ABI:FDS LDR,
   place LBL,
      10 6 CMP,  C-GE placed BCOND,
      9 11 0 LDR,  9 0 CMPI,  C-LT pskip BCOND,
      SPAWN-NEXT-ACTION
      15 PSFA-DUP2 MOVZ,  15 14 0 STRW,
      15 12 10 ADD,  15 14 4 STRW,  10 14 8 STRW,
      SPAWN-COUNT-ACTION
      SPAWN-NEXT-ACTION
      15 PSFA-CLOSE MOVZ,  15 14 0 STRW,
      15 12 10 ADD,  15 14 4 STRW,
      SPAWN-COUNT-ACTION
   pskip LBL,
      11 11 8 ADDI,  10 10 1 ADDI,
      place B,
   placed LBL,
   SPAWN-DARWIN-ZERO-ADESC
   SPAWN-DARWIN-ZERO-ATTR
   SPAWN-DARWIN-FILL-ADESC
   14 POSIX-SPAWN-SETPGROUP POSIX-SPAWN-CLOEXEC-DEFAULT or MOVZ,
   14 SP SPAWN-ATTR-OFF SPAWN-ATTR-FLAGS-OFF + STRW,
   0 SP SPAWN-PID-OFF ADDI,  1 8 SPAWN-ABI:PATH LDR,  2 SP SPAWN-ADESC-OFF ADDI,
   3 8 SPAWN-ABI:ARGV LDR,  4 8 SPAWN-ABI:ENVP LDR,
   NR-SPAWN SYS,
   C-CC spawned BCOND,
      9 31 0 SUB,  unmap B,
   spawned LBL,
   9 SP SPAWN-PID-OFF LDRW,  unmap B,
   toolong LBL,
   9 SPAWN-ENAMETOOLONG 1- MOVN,
   unmap LBL,
   0 SP SPAWN-FDS-BLOB-OFF LDR,  1 SP SPAWN-FDS-BLOB-U-OFF LDR,  NR-MUNMAP SYS,
   leave LBL,
   SP SP SPAWN-FDS-FRAME ADDI, ;

\ spawn-fds ( request -- pid|-errno ): src/habu/spawn-abi.f lays out the
\ request. A negative NFDS is -EINVAL.
: BSPAWNFDS ( -- )
   LBL {: valid:label :}
   LBL {: push:label :}
   HB-TARGET-LINUX? HB-TARGET-MACOS? or 0= IF ENGINE-EMIT:TARGET-UNKNOWN THEN
   10 G-POP
   11 10 SPAWN-ABI:NFDS LDR,  11 0 CMPI,  C-GE valid BCOND,
      9 SPAWN-EINVAL 1- MOVN,  push B,
   valid LBL,
   HB-TARGET-LINUX? IF LINUX-SPAWN-FDS THEN
   HB-TARGET-MACOS? IF 8 10 0 ADDI,  DARWIN-SPAWN-FDS THEN
   push LBL,
   9 G-PUSH ;

: BCPFETCH ( -- ) 9 CP 0 ADDI,  A G-PUSH ;     \ ( -- addr ) live CP (snapshot writer)
: BNDICTFETCH ( -- ) 9 NDICT 0 ADDI,  A G-PUSH ;  \ ( -- n ) live dict count
: BDBASEFETCH ( -- ) 9 DBASE 0 ADDI,  A G-PUSH ;  \ ( -- addr ) region base
: BDATAFETCH ( -- ) 9 DATA 0 ADDI,  A G-PUSH ;   \ ( -- addr ) live DATA base
: BNULLPTR ( -- ) 31 G-PUSH ;   \ ( -- 0 ) the null: register 31 as a store source is xzr
: BNUNITCUTCELL ( -- )
   9 NATIVE-OBS-EXT:CAPTURE-INVALIDATE MOVZ,
   9 DATA 9 ADD,  9 G-PUSH ;
: BNATIVEORIGINCELL ( -- )
   9 NCOMP-DISPATCH:CALLBACK-ENTRY-CELL CELL + MOVZ,
   9 DATA 9 ADD,  9 G-PUSH ;
: BCHECKFETCH ( -- ) 9 DATA HOOK-CELL LDR,  A G-PUSH ;  \ ( -- xt ) live checker hook — getter for set-check ([x20/DATA + HOOK-CELL])
: BTOPCHECKFETCH ( -- ) 9 DATA TOP-HOOK-CELL LDR,  A G-PUSH ;  \ ( -- xt ) live top-row hook — getter for set-top-check ([x20/DATA + TOP-HOOK-CELL])

\ The main thread is the compiler: only the thread whose DATA is the main
\ region, TASK-TCB-CELL 0 (layout.f), defines, allots, evaluates or changes
\ package scope. A task, an exposed region and a callback thread hold a TCB
\ there, and B-TASK-GUARD exits $4F on one. B-LOWER-GUARD exits $4F while any
\ task is live, for the sinks that lower CP or NDICT: lowering frees code a
\ task may be running and records it may reference. Clobbers x9.
: B-TASK-GUARD ( -- )
   LBL {: ok:label :}
   9 DATA TASK-TCB-CELL LDR,  9 ok CBZ,
      0 $4F MOVZ,  NR-EXIT-GROUP SYS,
   ok LBL, ;

: B-LOWER-GUARD ( -- )
   LBL {: ok:label :}
   9 DATA TASKS-LIVE-CELL LDR,  9 ok CBZ,
      0 $4F MOVZ,  NR-EXIT-GROUP SYS,
   ok LBL, ;

\ While a checker overlay is open the engine defines nothing (layout.f
\ REPLAY-SCOPE): replay-close and an overlay rollback restore NDICT, CP and WIDN
\ (src/core/checker.f CHECKER-OVERLAY ROLLBACK), so only the overlay's writers
\ (namespace-record, replay-record, replay-private) may grow them; a record
\ another writer appended would be dropped and a wid it took handed out again.
\ Every other writer runs GUARD, first: LQUALIFYDEF (each named definer, both
\ tiers), `package` making a row or a missing private wid, EXPORT, `wordlist`,
\ def-open, alias-record, namespace-private and native-unit-publish.
\ ndict-append needs none: it publishes the pending record, which replay-open
\ refuses and no head opens under the latch. Guarding at LHIDXADD is too late:
\ at `;` the check hook, trust registration and pass 2 precede it and
\ replay-close restores none of them. The refusal names the token and throws
\ ENGINE-ERROR:OVERLAY-OPEN (habu2.f EM-COMPILE-DIE); a live `undefine` throws
\ it before retiring anything (xref.f XREF-UNDEFINE-GUARD): replay-close gives
\ back only the retirements the overlay logged. outer.f OVERLAY-GUARD reads the
\ same latch. Clobbers x9.
package OVERLAY-EMIT
public
variable LDIE
: GUARD, ( -- )
   LBL {: ok:label :}
   9 REPLAY-SCOPE:LATCH LIT64,  9 DATA 9 ADD,  9 9 0 LDR,  9 ok CBZ,
      LDIE LABEL@ B,
   ok LBL, ;
;package

\ cp! and ndict! are the FORGET sinks: cp! moves JIT emission to the popped CP,
\ ndict! the next record write to DBASE+n*DREC. Each guards the span it
\ redirects to, so a post-seal value in a sealed band fails at the sink
\ (ENGINE-ERROR:SEAL-VIOLATION). FORGET marks are DBASE-relative and never
\ inside a data-base band, so the guard admits them.
: BCPSET ( -- ) B-TASK-GUARD  B-LOWER-GUARD  A G-POP  A GUARD-CODE-WORD
   EM-DEF-OCC:LRECLAIM LABEL@ BL,  CP A 0 ADDI, ;   \ ( addr -- ) set CP
\ LASTC-CELL names the record the last create, variable or constant wrote;
\ does> patches it (habu2.f DOESPATCH). A record at or above NDICT is retired
\ and its slot reused, so every motion that lowers NDICT (these two sinks,
\ evaluate's error recovery, the REPL's) calls this after setting the count. A
\ LASTC below the count is kept; one at or above it is cleared, not moved back:
\ a does>-patched record no longer says a create made it (layout.f DKIND). A
\ does> with none refuses by name. LASTC-CUT, cuts at the index in k instead,
\ for a rollback that retires records and keeps the count (EM-DEF-OCC:LRETIRE).
\ Clobbers the two scratch registers.
: LASTC-CUT, ( n n n -- ) {: k:n t:n u:n :}
   LBL {: live:label :}
   t DREC MOVZ,  t k t MUL,  t DBASE t ADD,            \ first retired record
   u DATA LASTC-CELL LDR,  u t CMP,  C-CC live BCOND,
   u 0 MOVZ,  u DATA LASTC-CELL STR,
   live LBL, ;

: LASTC-TRIM, ( n n -- ) {: t:n u:n :}
   NDICT t u LASTC-CUT, ;

\ The name lookup reads an empty hash slot as proof a name is absent, which
\ holds only while every record below NDICT is in the table. Raising NDICT
\ re-exposes records whose slots later publications may have reused, so a raise
\ rebuilds the table from [0,NDICT) in place (HIDX-EMIT:LREBUILD).
: BNDSET ( -- ) B-TASK-GUARD  B-LOWER-GUARD  A G-POP                                 \ ( n -- ) set NDICT — forget dict entries past a mark
   LBL LBL LBL LBL LBL {: bounded:label msg:label floor-ok:label keep:label done:label :}
   S\" hb: dictionary count out of range\n" {: ma:ptr mu:n :}
   \ NDICT is a count: DICT-CAP is valid. Unsigned comparison also rejects negatives.
   14 DICT-CAP LIT64,  A 14 CMP,  C-LS bounded BCOND,
      0 2 MOVZ,  1 msg ADR,  2 mu MOVZ,  NR-WRITE SYS,
      0 74 MOVZ,  NR-EXIT-GROUP SYS,
   msg LBL,  ma mu BYTES,
   bounded LBL,
   C DREC MOVZ,  B A C MUL,  B DBASE B ADD,  7 DREC MOVZ,  B 7 PROT-GUARD:CALL
   14 DATA SEAL-NDICT-CELL LDR,  14 floor-ok CBZ,
   A 14 CMP,  C-CS floor-ok BCOND,
      0 ENGINE-ERROR:SEAL-VIOLATION MOVZ,  NR-EXIT-GROUP SYS,
   floor-ok LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   A NDICT CMP,  C-LE keep BCOND,
      EM-DEF-OCC:LCOUNT LABEL@ BL,          \ issue before the raised count is visible
      HIDX-EMIT:LREBUILD LABEL@ BL,
      done B,
   keep LBL,
   EM-DEF-OCC:LCOUNT LABEL@ BL,
   A B LASTC-TRIM,
   done LBL,
   30 SP 0 LDR,  SP SP 16 ADDI, ;

\ Append the completed native pending record at the caller's expected index.
\ NPUB proves the emission and record shape; this sink owns the live count and
\ index and checks pending ownership before changing either. DOES> publishes
\ its parent, then its one prepared companion, which CLAUSE-CELL admits; no
\ other row is eligible.
: BNDAPPEND ( -- ) B-TASK-GUARD  A G-POP                  \ ( expected-index -- )
   LBL LBL LBL LBL LBL {: parent:label owner:label floor-ok:label bad:label done:label :}
   A NDICT CMP,  C-NE bad BCOND,
   14 DICT-CAP LIT64,  A 14 CMP,  C-CS bad BCOND,
   14 DATA NCOMP-DISPATCH:DEF-TIER-CELL LDR,  14 bad CBZ,
   C DREC MOVZ,  B A C MUL,  B DBASE B ADD,
   14 DATA PEND-CELL LDR,  14 bad CBZ,
   14 B CMP,  C-EQ parent BCOND,
   14 14 DREC ADDI,  14 B CMP,  C-NE bad BCOND,
   14 DATA CLAUSE-CELL LDR,  14 bad CBZ,
   owner B,
   parent LBL,
   14 DATA CLAUSE-CELL LDR,  14 owner CBZ,
   9 2 MOVZ,  EM-DEF-OCC:LROOM LABEL@ BL,
   9 NDICT 0 ADDI,
   owner LBL,
   14 DATA SEAL-NDICT-CELL LDR,  14 floor-ok CBZ,
   A 14 CMP,  C-CC bad BCOND,
   floor-ok LBL,
   7 DREC MOVZ,  B 7 PROT-GUARD:CALL
   EM-DEF-OCC:LAPPEND LABEL@ BL,
   LHIDXADD LABEL@ BL,
   done B,
   bad LBL,  0 ENGINE-ERROR:SEAL-VIOLATION MOVZ,  NR-EXIT-GROUP SYS,
   done LBL, ;

\ Lower the dictionary and open the namespace as one engine operation. Its
\ `( n -- )` checker row is a top-level boundary: top-level text (evaluated
\ text included) and TRUSTED: bodies call it, no checked body names it, and the
\ engine refuses its tick.
: BSEEDNDICTSET ( -- ) B-TASK-GUARD  B-LOWER-GUARD  A G-POP                         \ ( n -- )
   LBL LBL {: lower:label bad:label :}
   A 0 CMPI,  C-LT bad BCOND,
   A NDICT CMP,  C-LT lower BCOND,
   bad LBL,  0 74 MOVZ,  NR-EXIT-GROUP SYS,
   lower LBL,
   SP SP 16 SUBI,  A SP 0 STR,
   6 A 0 ADDI,  PROF-ABI:LPROFREWIND LABEL@ BL,
   A SP 0 LDR,  SP SP 16 ADDI,
   C DREC MOVZ,  B A C MUL,  B DBASE B ADD,  7 DREC MOVZ,  B 7 PROT-GUARD:CALL
   EM-DEF-OCC:LCOUNT LABEL@ BL,
   HIDX-EMIT:LREBUILD LABEL@ BL,
   A B LASTC-TRIM,
   9 0 MOVZ,  9 DATA SEAL-NDICT-CELL STR, ;

: BEPOCHSECONDS ( -- )
   LBL TIME-OK !
   0 DATA GTOD-SCRATCH ADDI,  1 0 MOVZ,  2 0 MOVZ,  NR-GETTIMEOFDAY SYS,
   9 C-CS CSET,  9 9 0 ORR,  9 0 CMPI,  C-EQ TIME-OK LABEL@ BCOND,  BRK,
   TIME-OK LABEL@ LBL,
   9 DATA GTOD-SCRATCH LDR,  9 G-PUSH ;

\ Monotonic nanoseconds for benchmarks. Darwin exposes `clock_gettime` and
\ `mach_absolute_time` through libSystem/commpage APIs, not this raw-syscall
\ engine. On arm64 macOS, EL0 can read CNTVCT_EL0 and CNTFRQ_EL0 directly; use
\ quotient/remainder conversion so the tick*1e9 multiply cannot overflow.
: BMONONS ( -- )
   LBL TIME-OK !
   $D53BE049 EMITW  $D53BE00A EMITW         \ mrs x9,CNTVCT_EL0 ; mrs x10,CNTFRQ_EL0
   10 TIME-OK LABEL@ CBNZ,  BRK,  TIME-OK LABEL@ LBL,
   11 9 10 UDIV,                            \ q = ticks / freq
   12 11 10 MUL,  9 9 12 SUB,               \ r = ticks % freq
   13 $3B9ACA00 LIT64,                      \ 1_000_000_000 ns/s
   11 11 13 MUL,
   9 9 13 MUL,  9 9 10 UDIV,
   9 11 9 ADD,  9 G-PUSH ;

package ENGINE-EMIT
public
\ The USE-MAX used-public wids between the using band (layout.f USE-WIDS-OFF)
\ and a snapshot at off from base. They are the includer's slots: a buffer
\ that closes the includer's package and then opens a using writes into one
\ (habu2.f LEVALREC, EM-REPL-RECOVER). band and tmp are scratch registers.
: USE-WIDS-SAVE, ( n n n n -- ) {: base:n off:n band:n tmp:n :}
   band USE-WIDS-OFF LIT64,  band DATA band ADD,
   USE-MAX 0 do  tmp band i cells LDR,  tmp base off i cells + STR,  loop ;

: USE-WIDS-RESTORE, ( n n n n -- ) {: base:n off:n band:n tmp:n :}
   band USE-WIDS-OFF LIT64,  band DATA band ADD,
   USE-MAX 0 do  tmp base off i cells + LDR,  tmp band i cells STR,  loop ;

\ Give the observer the evaluate frame in x<f> adopted (EMIT-EVAL-BODY) back
\ its frame (the one x<f> nests in, which adoption required), the evaluating
\ word's ticket and its source start, with that
\ word's run pending again and no callback open (habu2.f EM-EVAL-CLEAN-EXIT,
\ LEVALREC). Nothing when the frame adopted none. ctx and tmp are scratch.
: SX-RESTORE, ( n n n -- ) {: f:n ctx:n tmp:n :}
   LBL {: kept:label :}
   ctx f EVAL-FRAME:SX-CTX LDR,  ctx kept CBZ,
   tmp f EVAL-PREV LDR,  tmp ctx SX-EVAL STR,
   tmp f EVAL-FRAME:SX-TICKET LDR,  tmp ctx SX-TICKET STR,
   tmp f EVAL-FRAME:SX-SRC LDR,  tmp ctx SX-SRC STR,
   tmp 1 MOVZ,  tmp ctx SX-PENDING STR,
   tmp 0 MOVZ,  tmp ctx SX-SUSPEND STR,
   kept LBL, ;
;package

\ The source checker's operations and the record mark EMIT-EVAL-BODY takes;
\ habu2.f DECL-OWNER holds the rest of the package.
package DECL-OWNER
public

\ x11 = the operation at off in the source owner's record, or 0 with no record.
: FIND ( n -- ) {: off:n :}
   LBL {: done:label :}
   11 DATA NCOMP-DISPATCH:DECL-CELL LDR,  11 done CBZ,
   11 11 off LDR,
   done LBL, ;

\ Store the checker's record end at [base+off] as a mark: end+1, popped into
\ x<r>, or 0 when no owner records rows.
: MARK-END ( n n n -- ) {: r:n base:n off:n :}
   LBL {: unmarked:label :}
   9 0 MOVZ,  9 base off STR,
   NCOMP-DISPATCH:DECL-ROWS-END-OFF FIND  11 unmarked CBZ,
      11 BLR,  r G-POP  r r 1 ADDI,  r base off STR,
   unmarked LBL, ;

\ Cut the checker's records back to the mark MARK-END left at [base+off],
\ through x9; a 0 mark or no owner branches to kept, which the caller places.
: RETRACT-TO ( n n label -- ) {: base:n off:n kept:label :}
   9 base off LDR,  9 kept CBZ,
   NCOMP-DISPATCH:DECL-RETRACT-ROWS-OFF FIND  11 kept CBZ,
      9 9 1 SUBI,  9 G-PUSH  11 BLR, ;
;package

\ ( a u -- ) re-entrant interpret of a/u: save the outer input cursor and
\ compile state, point INP/INE at a/u, bump EVALD and jump to the interpret
\ loop top through LMAINP-CELL (prims cannot name labels). End of buffer
\ (LEXIT) with EVALD>0 restores the linked native-stack frame and returns here
\ with EVALERR-CELL 0. Interpret failures (LUNDEF, LUNDERFLOW, every throw)
\ unwind through LEVALREC instead, which rolls each escaped frame back, records
\ the code in EVALERR-CELL and delivers to the nearest handler, REPL or exit.
\
\ EMIT-EVAL-BODY takes a and u in x9/x10 and in x13 the data stack the frame
\ owns (0 for evaluate). The frame records the data-stack extent, that stack and
\ the definition open at entry (EVAL-FRAME:PEND); the clean exit returns to x30
\ (B-EVAL's caller, or the continuation of B-EVAL-CLOSED, which inlines it on a
\ stack of its own). The checker's record end (EVAL-FRAME:ROWS) is read last,
\ when x9, x10, x13, x14 and x30 are spent: the owner's ROWS-END field is
\ checked code.
variable LEVALBODY
variable LSOURCEEXECFINISH

: B-EVAL ( -- )
   B-TASK-GUARD
   B G-POP  A G-POP
   13 0 MOVZ,
   6 0 MOVZ,  7 10 0 ADDI,
   LEVALBODY LABEL@ B, ;

: B-EVAL-RANGE ( -- )
   B-TASK-GUARD
   7 G-POP  6 G-POP  B G-POP  A G-POP
   13 0 MOVZ,
   LEVALBODY LABEL@ B, ;

: EMIT-EVAL-BODY ( -- )
   LBL LBL LBL {: end:label sx-done:label sx-adopt:label :}
   s" (EVALUATE)" LEVALBODY LABEL@ LABEL>N
      end LABEL>N ENGINE-PRIMS:HELPER-REGISTER
   LEVALBODY LABEL@ LBL,
   SP SP STACK-ABI:EVAL-BYTES SUBI,
   14 SP 0 ADDI,
   11 DATA EVAL-TOP-CELL LDR,  11 14 EVAL-PREV STR,
   14 DATA EVAL-TOP-CELL STR,
   11 DATA INP-CELL LDR,  11 14 0 STR,
   12 DATA INE-CELL LDR,  12 14 8 STR,
   11 DATA SRCLOC:INB-CELL LDR,  11 14 EVAL-INB STR,   \ buffer START, for the refusal line number
   11 DATA PEND-CELL LDR,  11 14 EVAL-FRAME:PEND STR,   \ the definition open as this buffer begins
   30 14 16 STR,                                     \ x30 = where the clean exit returns
   11 SP STACK-ABI:EVAL-BYTES ADDI,  11 14 24 STR,
   XDS 14 32 STR,  CP 14 40 STR,  NDICT 14 48 STR,
   11 DATA DP-CELL LDR,  11 14 56 STR,
   11 DATA STACK-ABI:BASE-CELL LDR,  11 14 STACK-ABI:EVAL-BASE STR,
   11 DATA STACK-ABI:CAP-CELL LDR,  11 14 STACK-ABI:EVAL-CAP STR,
   13 14 STACK-ABI:EVAL-SEG STR,
   \ The source observer. An unarmed one arms when this range is the owned one.
   \ An armed one whose reading frame is the one this buffer nests in, with a
   \ word's run pending, no callback open and no definition open, is adopted:
   \ that word evaluates this buffer, so its tokens are observed in their own
   \ right, their offsets relative to this buffer's first byte, until the clean
   \ exit or the throw recovery gives the word's run back
   \ (ENGINE-EMIT:SX-RESTORE,). Only x11, x12 and x15 are free here.
   15 0 MOVZ,  15 14 EVAL-FRAME:SX-CTX STR,
   11 DATA SOURCE-EXEC-CTX-CELL LDR,  11 sx-done CBZ,
   12 11 SX-ARMED LDR,  12 sx-adopt CBNZ,
   12 11 SX-SRC LDR,  15 9 6 ADD,  12 15 CMP,  C-NE sx-done BCOND,
   12 11 SX-LEN LDR,  15 7 6 SUB,  12 15 CMP,  C-NE sx-done BCOND,
   14 11 SX-EVAL STR,
   12 DATA S0-CELL LDR,  12 XDS 12 SUB,  12 12 3 LSRI,  12 11 SX-FLOOR STR,
   12 DATA RSP-CELL LDR,  12 11 SX-RFLOOR STR,
   12 1 MOVZ,  12 11 SX-ARMED STR,
   sx-done B,
   sx-adopt LBL,
   12 11 SX-EVAL LDR,  15 14 EVAL-PREV LDR,  12 15 CMP,  C-NE sx-done BCOND,
   12 11 SX-PENDING LDR,  12 sx-done CBZ,
   12 11 SX-SUSPEND LDR,  12 sx-done CBNZ,
   12 DATA PEND-CELL LDR,  12 sx-done CBNZ,
   11 14 EVAL-FRAME:SX-CTX STR,
   12 11 SX-TICKET LDR,  12 14 EVAL-FRAME:SX-TICKET STR,
   12 11 SX-SRC LDR,  12 14 EVAL-FRAME:SX-SRC STR,
   14 11 SX-EVAL STR,
   15 9 6 ADD,  15 11 SX-SRC STR,
   12 0 MOVZ,  12 11 SX-PENDING STR,
   sx-done LBL,
   12 14 EVAL-PKG ADDI,
   11 DATA CUR-CELL LDR,        11 12 PKGSNAP-CUR STR,
   11 DATA PKG-PUB-CELL LDR,    11 12 PKGSNAP-PUB STR,
   11 DATA PKG-PRI-CELL LDR,    11 12 PKGSNAP-PRI STR,
   11 DATA PKG-PARENT-CELL LDR, 11 12 PKGSNAP-PARENT STR,
   11 DATA PKG-REC-CELL LDR,    11 12 PKGSNAP-REC STR,
   \ The `using` depth, rolled back to this boundary on clean exit
   \ (EM-EVAL-CLEAN-EXIT) and on throw (LEVALREC): usings are file-local.
   11 USE-DEPTH-CELL LIT64,  11 DATA 11 ADD,  11 11 0 LDR,  11 12 PKGSNAP-USE STR,
   \ and starts the buffer's using floor there: it closes only the usings it opens
   11 14 EVAL-FRAME:USE-FLOOR STR,
   12 14 EVAL-PKG ADDI,
   \ and the open package's using floor, which a throw restores with the package
   11 USE-PKG-SAVE-CELL LIT64,  11 DATA 11 ADD,  11 11 0 LDR,  11 12 PKGSNAP:FLOOR STR,
   \ and the includer's used publics, which a throw puts back with the depth
   14 EVAL-FRAME:USE-WIDS 11 12 ENGINE-EMIT:USE-WIDS-SAVE,
   11 DATA EVALD-CELL LDR,  11 11 1 ADDI,  11 DATA EVALD-CELL STR,
   11 9 6 ADD,  11 DATA INP-CELL STR,               \ INP = full base + first
   9 DATA SRCLOC:INB-CELL STR,                       \ INB = full source base
   11 9 7 ADD,  11 DATA INE-CELL STR,               \ INE = full base + end
   10 SP EVAL-FRAME:ROWS DECL-OWNER:MARK-END
   9 DATA LMAINP-CELL LDR,  9 BR,
   end LBL, ;

: BCREATE ( -- )
   B-TASK-GUARD                                   \ only main defines
   15 0 MOVZ,  16 20 CREATEP-CELL LDR,  16 BLR, ;   \ ( "name" -- ) runtime CREATE via the
                                     \ startup-stored cell: subsets emit prims w/o labels;
                                     \ LCREATE publishes the word's `-- ptr a` in both modes

: BEMIT ( -- )
   A G-POP  13 9 0 ADDI,  G-EMITC ;

: BCR ( -- )
   13 10 MOVZ,  G-EMITC ;

: BSPACE ( -- )
   13 32 MOVZ,  G-EMITC ;

: B.S ( -- )
   LBL PS-LOOP !
   LBL PS-DONE !
   9 DATA S0-CELL LDR,  9 DATA SSCR-CELL STR,
   PS-LOOP LABEL@ LBL,
      9 DATA SSCR-CELL LDR,  9 XDS CMP,  C-CS PS-DONE LABEL@ BCOND,
      9 9 0 LDR,  G-PRINT9
      9 DATA SSCR-CELL LDR,  9 9 8 ADDI,  9 DATA SSCR-CELL STR,
      PS-LOOP LABEL@ B,
   PS-DONE LABEL@ LBL, ;

: BDEPTH ( -- )
   A DATA S0-CELL LDR,
   A XDS A SUB,
   A A 3 LSRI,
   A G-PUSH ;

: (CMP) ( n -- )
   CMP-COND !
   B G-POP  A G-POP  A B CMP,  A CMP-COND @ CSET,  A SP A SUB,  A G-PUSH ;

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

\ arm64 SDIV by zero yields zero, so the divisor (B) is tested. A zero divisor
\ is a caller error the program can recover from, so the guard branches to the
\ engine-resident (DIV-ZERO) helper, which throws ARITH-ABI:E-DIV-ZERO. The
\ helper is out of line so the guard costs one branch: inlining BTHROW's forty
\ instructions pushes the divide out of the body's first cache lines (8.7% on a
\ million-division loop).
\
\ MIN-N -1 / is MIN-N, SDIV's modular answer, like the wrap of `+`, `-` and `*`
\ (docs/forth.md); a guard for it would cost every division a compare no
\ caller in this tree can fail.
: BDIV0? ( -- )
   LBL {: ok:label :}
   B ok CBNZ,  LDIVZERO LABEL@ BL,  ok LBL, ;

: BDIV ( -- )
   B G-POP A G-POP  BDIV0?  A A B SDIV, A G-PUSH ;

: BMOD ( -- )
   B G-POP A G-POP  BDIV0?  C A B SDIV,  C C B MUL,  A A C SUB,  A G-PUSH ;

: BDIVMOD ( -- )
   B G-POP A G-POP  BDIV0?  C A B SDIV,  DREG C B MUL,  A A DREG SUB,  A G-PUSH C G-PUSH ;

: BABS ( -- )
   LBL PRIM-DONE !
   A G-POP  A 0 CMPI,  C-GE PRIM-DONE LABEL@ BCOND,  A SP A SUB,  PRIM-DONE LABEL@ LBL,  A G-PUSH ;

: BMIN ( -- )
   LBL PRIM-DONE !
   B G-POP A G-POP  A B CMP,  C-LE PRIM-DONE LABEL@ BCOND,  A B 0 ADDI,  PRIM-DONE LABEL@ LBL,  A G-PUSH ;

: BMAX ( -- )
   LBL PRIM-DONE !
   B G-POP A G-POP  A B CMP,  C-GE PRIM-DONE LABEL@ BCOND,  A B 0 ADDI,  PRIM-DONE LABEL@ LBL,  A G-PUSH ;

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
   LBL PRIM-DONE !
   14 XDS 8 SUBI,  A 14 0 LDR,
   A PRIM-DONE LABEL@ CBZ,  A G-PUSH  PRIM-DONE LABEL@ LBL, ;

: BFETCH ( -- )
   A G-POP  A A 0 LDR,  A G-PUSH ;

: BSTORE ( -- )
   B G-POP A G-POP  7 8 MOVZ,  B 7 PROT-GUARD:CALL  A B 0 STR, ;

: BPTRFIELD ( -- )
   B G-POP  A G-POP  B B 3 LSLI,  A A B ADD,  A G-PUSH ;

: BPLUSSTORE ( -- )
   B G-POP A G-POP  7 8 MOVZ,  B 7 PROT-GUARD:CALL  C B 0 LDR,  C C A ADD,  C B 0 STR, ;

: BCFETCH ( -- )
   A G-POP  A A 0 LDRB, A G-PUSH ;

: BCSTORE ( -- )
   B G-POP A G-POP  7 1 MOVZ,  B 7 PROT-GUARD:CALL  A B 0 STRB, ;

\ Atomic primitives (ARMv8.1 LSE; Orin is ARMv8.2). A=x9 B=x10 C=x11.
: BATFETCH ( -- )   \ atomic@ ( ptr a -- a ) : LDAR x9,[x9]
   A G-POP  $C8DFFD29 EMITW  A G-PUSH ;
: BATSTORE ( -- )   \ atomic! ( a ptr a -- ) : STLR x9,[x10]
   B G-POP A G-POP  7 8 MOVZ,  B 7 PROT-GUARD:CALL  $C89FFD49 EMITW ;
: BATADD ( -- )     \ atomic-add ( delta addr -- old ) : LDADDAL x9,x9,[x10]
   B G-POP A G-POP  7 8 MOVZ,  B 7 PROT-GUARD:CALL  $F8E90149 EMITW  A G-PUSH ;
: BATCAS ( -- )     \ atomic-cas ( expected new addr -- actual ) : CASAL x9,x10,[x11]
   C G-POP B G-POP A G-POP  14 B 0 ADDI,  7 8 MOVZ,     \ x14 saves new (x10) across the guard's x10/x11 clobber
   C 7 PROT-GUARD:CALL  C B 0 ADDI,  B 14 0 ADDI,       \ restore addr->x11, new->x10 for the CASAL
   $C8E9FD6A EMITW  A G-PUSH ;
: BFENCE ( -- )     \ fence ( -- ) : DMB ISH
   $D5033BBF EMITW ;

: BCELLS ( -- )
   A G-POP  A A 3 LSLI, A G-PUSH ;

: BCELLPLUS ( -- )
   A G-POP  A A 8 ADDI, A G-PUSH ;

: BCHARS ( -- ) ;

\ Memory views change only the checked pointee; the machine address is unchanged.
: BADDRESSVIEW ( -- ) ;

: BCHARPLUS ( -- )
   A G-POP  A A 1 ADDI, A G-PUSH ;

: BCOUNT ( -- )
   A G-POP  B A 0 LDRB,  A A 1 ADDI,  A G-PUSH  B G-PUSH ;

\ The return stack is a guarded mapping whose base is in the header, not a
\ fixed DATA band: a slot is [RETURN-BASE-CELL] + depth*8. The depth is stored
\ before x14 is reloaded with the base, so two scratch registers suffice. A
\ push past RETURN-CELLS faults on the mapping's guard page; there is no
\ capacity compare.
: RSTK-PUSH ( n -- )
   RSTK-REG !
   14 DATA RSP-CELL LDR,
   15 14 3 LSLI,
   14 14 1 ADDI,  14 DATA RSP-CELL STR,
   14 DATA STACK-ABI:RETURN-BASE-CELL LDR,  15 14 15 ADD,
   RSTK-REG @ 15 0 STR, ;

: RSTK-POP ( n -- )
   RSTK-REG !
   14 DATA RSP-CELL LDR,
   14 14 1 SUBI,
   15 14 3 LSLI,
   14 DATA RSP-CELL STR,
   14 DATA STACK-ABI:RETURN-BASE-CELL LDR,  15 14 15 ADD,
   RSTK-REG @ 15 0 LDR, ;

: B2TOR ( -- )
   B G-POP A G-POP  A RSTK-PUSH  B RSTK-PUSH ;

: B2RFROM ( -- )
   B RSTK-POP  A RSTK-POP  A G-PUSH  B G-PUSH ;

: B2RFETCH ( -- )
   B RSTK-POP  A RSTK-POP  A RSTK-PUSH  B RSTK-PUSH  A G-PUSH  B G-PUSH ;

: BHERE ( -- )
   7 DATA 0 LDR,  7 G-PUSH ;

\ Bound the DP heap to [DATA-START, DATA-SIZE - PROF-CNT-BYTES] before any
\ allot , c, or definer stores a new DP: the top PROF-CNT-BYTES are the
\ profiler counter band (layout.f). Out of range goes to LDPBAD (habu2.f
\ EM-COMPILE-DIE): "hb: data space out of range: DP <dp> of <cap> bytes" on
\ fd 2, then rc 76 via LCOMPILEDIE, a catchable throw inside evaluate (nothing
\ was written: this check precedes every DP store) and exit 76 at top level.
\ Every site hands the candidate in x15: the sinks hold it in x7
\ (BALLOT/BALIGN/C-VARIABLE/C-DEFER-CELL), x11 (BCOMMA/BCCOMMA/LCREATE) or x14
\ (interpret-mode string copies, C-PARSE-CREATED-SIG). x15 is free on the
\ refusal arm: a site carrying a live x15 (the string copies' literal base)
\ needs it only on the returning arm, and it survives the write syscalls (the
\ kernel preserves x2-x15) and LDIAGU.
: DP-CHECK ( n -- )
   DP-REG !
   LBL DP-LOW !
   LBL DP-HIGH !
   5 DATA-START LIT64,  5 DATA 5 ADD,
   DP-REG @ 5 CMP,  C-GE DP-LOW LABEL@ BCOND,
      15 DP-REG @ 0 ADDI,  LDPBAD LABEL@ B,
   DP-LOW LABEL@ LBL,
   5 DATA-SIZE PROF-CNT-BYTES - LIT64,  5 DATA 5 ADD,
   DP-REG @ 5 CMP,  C-LE DP-HIGH LABEL@ BCOND,
      15 DP-REG @ 0 ADDI,  LDPBAD LABEL@ B,
   DP-HIGH LABEL@ LBL, ;

\ Before a path lowers DP, raise the release high-water (layout.f DPHW-CELL)
\ to the DP it lowers from, which the DP cell still holds: O(1), so releasing a
\ large allot costs nothing, and `allot` (BALLOT) zeroes the reused space when
\ it hands it out again. Clobbers the two scratch registers.
: DP-LOWER, ( n n -- )
   {: t:n u:n :}
   LBL {: done:label :}
   31 DATA COMMA-RUN-CELL STR,                     \ a `,` run ends where DP lowers
   t DATA DP-CELL LDR,  t t DATA SUB,
   u DATA DPHW-CELL LDR,  t u CMP,  C-LS done BCOND,
   t DATA DPHW-CELL STR,
   done LBL, ;

\ `create` applies the same rounding itself.
: BALIGN ( -- )
   B-TASK-GUARD
   7 DATA 0 LDR,  7 7 7 ADDI,  7 7 3 LSRI,  7 7 3 LSLI,  7 DP-CHECK  7 DATA 0 STR, ;

\ `,` keeps the latest run of its cells that starts at a created body
\ (layout.f COMMA-RUN-CELL): a cell written at the body start starts a run,
\ even where the last run ended; one written at the run's end extends it.
: BCOMMA ( -- )
   LBL LBL LBL {: more:label grow:label done:label :}
   B-TASK-GUARD
   A G-POP  7 DATA 0 LDR,  C 7 8 ADDI,  C DP-CHECK  A 7 0 STR,  C DATA 0 STR,
   A DATA COMMA-BASE-CELL LDR,  A 7 CMP,  C-NE more BCOND,
   7 DATA COMMA-RUN-CELL STR,  grow B,
   more LBL,  A DATA COMMA-END-CELL LDR,  A 7 CMP,  C-NE done BCOND,
   grow LBL,  C DATA COMMA-END-CELL STR,
   done LBL, ;

: BCCOMMA ( -- )
   B-TASK-GUARD
   A G-POP  7 DATA 0 LDR,  C 7 1 ADDI,  C DP-CHECK  A 7 0 STRB, C DATA 0 STR, ;

: BTYPE ( -- )
   2 G-POP  1 G-POP  G-OUT ;

10 constant DIE-NL                 \ the one byte that ends every die message

\ die ( ptr u8 n n -- ): write the span and one newline to fd 2, then exit. An
\ empty span writes nothing, so a site whose report already ends in a newline
\ passes `s" "`. The newline byte sits in a 16-byte stack frame (no label, no
\ ADR). An rc outside [0,255] maps to UNCAUGHT-RC: the kernel masks rc & 0xFF,
\ and a negative throw code reused as rc could exit 0. x7 holds the rc across
\ both syscalls.
: BDIE ( -- )
   LBL {: lfixed:label :}
   LBL {: quiet:label :}
   7 G-POP  2 G-POP  1 G-POP
   2 quiet CBZ,
      0 2 MOVZ,  NR-WRITE SYS,
      SP SP $10 SUBI,  A DIE-NL MOVZ,  A SP 0 STRB,
      0 2 MOVZ,  1 SP 0 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
      SP SP $10 ADDI,
   quiet LBL,
   0 7 0 ADDI,
   \ Run the process-exit hook (layout.f EXIT-HOOK-CELL) before exiting. The
   \ trampoline preserves x0; x7 is re-derived because the hook is compiled code
   \ owning every caller-saved register. BDIE never returns, so LR is free.
   LEXITHOOK LABEL@ BL,  7 0 0 ADDI,
   7 0 CMPI,    C-LT lfixed BCOND,
   7 255 CMPI,  C-GT lfixed BCOND,
   NR-EXIT-GROUP SYS,
   lfixed LBL,  0 UNCAUGHT-RC MOVZ,  NR-EXIT-GROUP SYS, ;

: SYS-PUSH ( -- )                  \ push x0, or -1 when the syscall carry is set
   LBL SYS-OK !
   LBL SYS-DONE !
   9 C-CS CSET,  9 SYS-OK LABEL@ CBZ,
      0 0 MOVN,  SYS-DONE LABEL@ B,
   SYS-OK LABEL@ LBL,
   SYS-DONE LABEL@ LBL,
   0 G-PUSH ;

\ x9 holds the carry sampled immediately after a filesystem syscall. Linux
\ leaves -errno in x0; Darwin leaves positive errno there. Preserve that
\ cause in the running task before publishing the existing 0/-1 result.
: FS-SYS-PUSH-FLAG ( -- )
   LBL LBL {: ok:label done:label :}
   9 ok CBZ,
      HB-TARGET-LINUX? IF 10 31 0 SUB, ELSE
         HB-TARGET-MACOS? IF 10 0 0 ADDI, ELSE ENGINE-EMIT:TARGET-UNKNOWN THEN
      THEN
      10 DATA FS-ABI:ERRNO-OFF STR,
      0 0 MOVN, done B,
   ok LBL,
   10 0 MOVZ,  10 DATA FS-ABI:ERRNO-OFF STR,
   done LBL,
   0 G-PUSH ;

: FS-SYS-PUSH ( -- )
   9 C-CS CSET,  FS-SYS-PUSH-FLAG ;

: BOPEN ( -- )
   2 G-POP  1 G-POP  0 G-POP
   HB-TARGET-LINUX? IF
      3 2 0 ADDI,
      OS-OPEN-FLAGS
      1 0 0 ADDI,
      0 99 MOVN,
      NR-OPEN SYS,  FS-SYS-PUSH
      exit
   THEN
   HB-TARGET-MACOS? IF NR-OPEN SYS,  FS-SYS-PUSH  exit THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

: BWRITE ( -- )
   2 G-POP  1 G-POP  0 G-POP  NR-WRITE SYS,  FS-SYS-PUSH ;

: BREAD ( -- )
   2 G-POP  1 G-POP  0 G-POP  1 2 PROT-GUARD:CALL  NR-READ SYS,  FS-SYS-PUSH ;

: BIOCTL ( -- )
   2 G-POP  1 G-POP  0 G-POP  GUARD-IOCTL  NR-IOCTL SYS,  SYS-PUSH ;

: BMMAP ( -- )                     \ ( addr len prot flags fd off -- addr|-1 )
   5 G-POP  4 G-POP  3 G-POP  2 G-POP  1 G-POP  0 G-POP
   LBL {: notfixed:label :}
   6 3 $10 ANDI,  6 notfixed CBZ,            \ only MAP_FIXED replaces an existing mapping
      0 1 PROT-GUARD:CALL                      \ x0 = address, x1 = mapping length
   notfixed LBL,
   HB-TARGET-LINUX? IF OS-MMAP-FLAGS  NR-MMAP SYS,  SYS-PUSH  exit THEN
   HB-TARGET-MACOS? IF NR-MMAP SYS,  SYS-PUSH  exit THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

\ Fresh anonymous storage has no prior element type. The caller chooses the
\ pointee type; the OS boundary returns a null pointer and -1 on failure.
\ Darwin accepts a zero-length mmap; an empty extent owns no storage here.
: BMAPANON ( -- )                      \ ( bytes -- ptr a ior )
   LBL {: failed:label :}
   LBL {: done:label :}
   1 G-POP
   1 0 CMPI,  C-LE failed BCOND,
   0 0 MOVZ,  2 3 MOVZ,  3 MAP-ANON-PRIVATE LIT64,
   4 0 MOVN,  5 0 MOVZ,
   NR-MMAP SYS,  C-CS failed BCOND,
   1 0 MOVZ,  done B,
   failed LBL,  0 0 MOVZ,  1 0 MOVN,
   done LBL,  0 G-PUSH  1 G-PUSH ;

: BMUNMAP ( -- )                   \ ( addr len -- 0|-1 ) release a mapping; span-guard the extent
   1 G-POP  0 G-POP
   0 1 PROT-GUARD:CALL             \ trap an unmap aimed at any protected engine band
   NR-MUNMAP SYS,  SYS-PUSH ;

: BGETPID ( -- )                   \ ( -- pid ) never fails; x0 holds the caller's pid
   NR-GETPID SYS,  SYS-PUSH ;

\ `ffi-call` loads 8 argbuf cells into x0-x7, BLRs fn and pushes x0;
\ `ffi-call-abi`/`ffi-call-abi-r` add x8, d0-d7, caller-packed stack spill and
\ integer/float returns for src/lib/ffi-abi.f. argbuf is at least 8 cells. XDS
\ (x19) is callee-saved, so the data stack survives the call; FPRIM frames x30.
\ Every foreign-call primitive is restricted to TRUSTED: definitions. Sealed
\ FFI bindings use ffi-call-bounded: one schema extent per live argument, zero
\ for scalar/read-only, and a whole-span guard for every writer before the BLR.
: BFFI-LOAD-X0-X7 ( -- )
   0 15 0  LDR,   1 15 8  LDR,   2 15 16 LDR,   3 15 24 LDR,
   4 15 32 LDR,   5 15 40 LDR,   6 15 48 LDR,   7 15 56 LDR, ;

\ Sound seal guard for the raw (unbounded) trampolines: the argbuf carries no
\ per-arg writable extents, so every LIVE integer/pointer arg is checked as a
\ band pointer -- a one-byte span-guard at the arg value. x14 = live arg count,
\ x15 = integer argbuf, x9 = sret flag (guard x8 = argbuf[8] when nonzero).
\ Stale slots [nargs..8) are skipped, so a low-arity call never trips on the
\ garbage cells above its args. x5/x6 are the only scratch; x14/x15/x9 stay live
\ across the shared LPROTSPAN body (it touches only x10-x13,x30).
: BFFI-GUARD-ARGS ( -- )
   LBL LBL LBL {: loop:label done:label nosret:label :}
   5 0 MOVZ,
   loop LBL,
      5 14 CMP,  C-GE done BCOND,
      6 5 3 LSLI,  6 15 6 ADD,  10 6 0 LDR,            \ x10 = argbuf[i]
      11 1 MOVZ,  LPROTSPAN LABEL@ BL,                 \ one-byte band guard on the arg value
      5 5 1 ADDI,  loop B,
   done LBL,
      9 nosret CBZ,                                    \ sret flag == 0 -> no x8 guard
      10 15 $40 LDR,  11 1 MOVZ,  LPROTSPAN LABEL@ BL, \ guard x8 = argbuf[8] indirect result
   nosret LBL, ;

\ The VM frame an outbound call leaves for C (src/lib/ffi-callback.f).
\ This thread's pointer, compared with CB-OWNER and a slot row: one value for a
\ thread's life, distinct across live threads. Linux: TPIDR_EL0, the TLS base
\ clone gives each thread. macOS: TPIDRRO_EL0, the kernel's current cthread
\ pointer (SDK Kernel.framework arm64/machine_machdep.h). Measured on macOS
\ 27.0.1 (26A434): pthread_self opens with `mrs x0, TPIDRRO_EL0 ; ldr x8,
\ [x0, #-0xe0]!` unmasked; seven threads reading it four million times each saw
\ one value per thread, low three bits clear, pthread_self + $E0. The low three
\ bits are cleared anyway, as os/tsd.h _os_tsd_get_base does for kernels that
\ kept the CPU number there. src/lib/ffi-callback-test.f OWNER-IDENTITY checks
\ the running host.
: CB-SELF, ( n -- ) {: r:n :}
   HB-TARGET-LINUX? IF
      $D53BD040 r or EMITW
   ELSE
      $D53BD060 r or EMITW
      r r 3 LSRI,  r r 3 LSLI,
   THEN ;

\ CASAL Xs, Xt, [Xn]: Xs holds the expected value and receives the old one.
: CASAL, ( n n n -- ) {: s:n t:n n:n :}
   $C8E0FC00 s 16 lshift or n 5 lshift or t or EMITW ;

\ YIELD, the spin-wait hint: this thread is waiting on another.
: YIELD, ( -- ) $D503203F EMITW ;

\ One refusal: `hb: callback: <reason>` on fd 2, then exit_group 106. The code
\ lands at the label; callers emit it off their straight-line path.
: CB-DIE, ( ptr u8 n label -- ) {: a u at:label :}
   LBL {: msg:label :}
   at LBL,
   0 2 MOVZ,  1 msg ADR,  2 u MOVZ,  NR-WRITE SYS,
   0 ENGINE-ERROR:CALLBACK MOVZ,  NR-EXIT-GROUP SYS,
   msg LBL,  a u BYTES, ;

\ Every outbound core runs this after its pops and guards, before its BLR. The
\ owner comes first: LDAR CB-OWNER; this thread already is nested and claims
\ nothing; 0 is CASAL'd 0 -> self; any other owner, or a lost CAS, dies 106
\ with the region untouched. Only then the $30 frame - the claim flag at [sp],
\ [sp+8] for the core's own x20 park, the four cells it replaces at
\ [sp+$10..$28] - and the live cursors go out. Scratch is x9..x12, which no
\ core holds across this point.
: BCB-ENTER ( -- )
   LBL LBL LBL {: held:label busy:label go:label :}
   12 CB-SELF,
   11 CB-OWNER MOVZ,  11 DATA 11 ADD,
   9 11 LDAR,
   10 0 MOVZ,
   9 12 CMP,  C-EQ held BCOND,
   9 busy CBNZ,
   9 12 11 CASAL,
   9 busy CBNZ,
   10 1 MOVZ,
   held LBL,
   SP SP $30 SUBI,
   10 SP 0 STR,
   9 DATA CB-XDS LDR,    9 SP $10 STR,
   9 DATA CB-DBASE LDR,  9 SP $18 STR,
   9 DATA CB-NDICT LDR,  9 SP $20 STR,
   9 DATA CB-CP LDR,     9 SP $28 STR,
   XDS DATA CB-XDS STR,
   DBASE DATA CB-DBASE STR,
   NDICT DATA CB-NDICT STR,
   CP DATA CB-CP STR,
   go B,
   S\" hb: callback: context busy on another thread\n" busy CB-DIE,
   go LBL, ;

\ After the BLR, with sp back at the $30 frame and x20 = DATA: the four cells
\ are restored first and a claimed owner is released last, by STLR, so a
\ thread that acquires the owner sees a complete frame or CB-XDS 0. x0 and d0
\ carry the result, so the scratch is x9..x11.
: BCB-LEAVE ( -- )
   LBL {: kept:label :}
   9 SP $10 LDR,  9 DATA CB-XDS STR,
   9 SP $18 LDR,  9 DATA CB-DBASE STR,
   9 SP $20 LDR,  9 DATA CB-NDICT STR,
   9 SP $28 LDR,  9 DATA CB-CP STR,
   9 SP 0 LDR,
   SP SP $30 ADDI,
   9 kept CBZ,
      11 CB-OWNER MOVZ,  11 DATA 11 ADD,  10 0 MOVZ,  10 11 STLR,
   kept LBL, ;

: BFFI-CALL ( -- )
   16 G-POP                                            \ x16 = fn
   14 G-POP                                            \ x14 = nargs
   15 G-POP                                            \ x15 = argbuf
   9 0 MOVZ,  BFFI-GUARD-ARGS                          \ no sret: guard the live x0..x7 args
   BCB-ENTER
   BFFI-LOAD-X0-X7
   16 BLR,
   BCB-LEAVE
   0 G-PUSH ;

: BFFI-LOAD-DREG ( n n -- )
   FFI-OFF !
   FFI-DREG !
   9 17 FFI-OFF @ LDR,  FFI-DREG @ 9 FMOVXD, ;

: BFFI-LOAD-D0-D7 ( -- )
   0 0 BFFI-LOAD-DREG    1 $8 BFFI-LOAD-DREG
   2 $10 BFFI-LOAD-DREG  3 $18 BFFI-LOAD-DREG
   4 $20 BFFI-LOAD-DREG  5 $28 BFFI-LOAD-DREG
   6 $30 BFFI-LOAD-DREG  7 $38 BFFI-LOAD-DREG ;

: BFFI-COPY-ABI-STACK ( -- )
   LBL FFI-SKIP !
   LBL FFI-LOOP !
   LBL FFI-DONE !
   14 0 CMPI,  C-LE FFI-SKIP LABEL@ BCOND,             \ stackcells <= 0 -> no spill
   10 14 0 ADDI,                                      \ x10 = cells left
   11 10 3 LSLI,  11 11 $F ADDI,  11 11 4 LSRI,  11 11 4 LSLI,
   12 SP 0 ADDI,  12 12 11 SUB,  SP 12 0 ADDI,        \ sp -= align(cells*8,16)
   12 13 0 ADDI,  13 SP 0 ADDI,                       \ x12=src, x13=dst
   FFI-LOOP LABEL@ LBL,
      10 FFI-DONE LABEL@ CBZ,
      9 12 0 LDR,  9 13 0 STR,
      12 12 $8 ADDI,  13 13 $8 ADDI,
      10 10 1 SUBI,  FFI-LOOP LABEL@ B,
   FFI-DONE LABEL@ LBL,
   FFI-SKIP LABEL@ LBL, ;

\ A relocated cell holds a prepacked-buffer address; it is moved by the
\ distance x13 from that buffer to the copy on the native stack. The mask in
\ x12 names the cells: bits 0-7 are x0-x7, bits 8-23 the stack cells 0-15.
\ x11 is the bit test's scratch, x9 the stack cell's.
: BFFI-RELOC-REG ( n -- ) LBL {: r:n skip:label :}
   11 12 63 r - LSLI,  11 11 63 LSRI,  11 skip CBZ,
   r r 13 ADD,
   skip LBL, ;

: BFFI-RELOC-CELL ( n -- ) LBL {: j:n skip:label :}
   11 12 55 j - LSLI,  11 11 63 LSRI,  11 skip CBZ,
   9 SP j 8 * LDR,  9 9 13 ADD,  9 SP j 8 * STR,
   skip LBL, ;

\ A relocating call (STRUCT-ARG's private copies) enters with the mask in x2
\ and keeps the prepacked buffer's address in x3 until the copy is made.
: BFFI-CALL-ABI-CORE ( bool -- ) {: reloc:bool :}
   16 G-POP                                            \ x16 = fn
   9 G-POP                                             \ x9  = sret-present flag
   14 G-POP                                            \ x14 = int-nargs (live x0..x7 count)
   1 G-POP                                             \ x1  = stack cell count (parked past the guard)
   13 G-POP                                            \ x13 = prepacked stack cells
   17 G-POP                                            \ x17 = FP argbuf
   15 G-POP                                            \ x15 = integer argbuf
   BFFI-GUARD-ARGS                                     \ band-guard live int args + x8 sret before any load
   14 1 0 ADDI,                                        \ x14 = stack cell count (restore for the spill copy)
   reloc if 3 13 0 ADDI, then                          \ x3 = prepacked stack cells
   BCB-ENTER
   20 SP $8 STR,                                       \ park caller x20 in frame slot
   20 SP 0 ADDI,                                       \ x20 = frame sp
   BFFI-COPY-ABI-STACK
   reloc if
      13 SP 0 ADDI,  13 13 3 SUB,                      \ x13 = native copy - prepacked cells
      12 2 0 ADDI,                                     \ x12 = mask; x0-x7 load over x2/x3
      16 0 do i BFFI-RELOC-CELL loop
   then
   BFFI-LOAD-X0-X7
   8 15 $40 LDR,                                       \ x8 = indirect-result address
   BFFI-LOAD-D0-D7
   reloc if 8 0 do i BFFI-RELOC-REG loop then
   16 BLR,
   SP 20 0 ADDI,
   20 SP $8 LDR,
   BCB-LEAVE ;

: BFFI-CALL-ABI ( -- )
   false BFFI-CALL-ABI-CORE
   0 G-PUSH ;

: BFFI-CALL-ABI-R ( -- )
   false BFFI-CALL-ABI-CORE
   9 0 FMOVDX,  9 G-PUSH ;

\ Sealed bindings carry one writable byte extent per live argument: zero for
\ scalars and read-only pointers; writable pointers are guarded as whole spans
\ before the BLR. Preconditions: x14=nargs, x15=argbuf, x17=writable-length
\ buffer, x20=DATA. Each (arg, extent) pair loads straight into LPROTSPAN's
\ x10/x11 ABI. x5/x6 are the only scratch; x14/x15/x17 stay live.
: BFFI-GUARD-BOUNDS ( -- )
   LBL LBL {: loop:label done:label :}
   5 0 MOVZ,
   loop LBL,
      5 14 CMP,  C-GE done BCOND,
      6 5 3 LSLI,  6 15 6 ADD,  10 6 0 LDR,            \ x10 = argbuf[i]
      6 5 3 LSLI,  6 17 6 ADD,  11 6 0 LDR,            \ x11 = writable extent
      LPROTSPAN LABEL@ BL,
      5 5 1 ADDI,  loop B,
   done LBL, ;

\ FFI: the general AAPCS64 trampoline, for any integer/pointer arity.
\ ( argbuf nargs fn -- ret ): x0-x7 from argbuf[0..7]; args 9..nargs spill to
\ the 16-byte aligned stack by an exact runtime loop. argbuf holds max(8,nargs)
\ cells. x20 (callee-saved) carries the frame sp across the BLR; the caller's
\ x20 parks at [sp,#8] of BCB-ENTER's $30 frame. Shifted-register SUB reads r31
\ as XZR, not SP, so sp is lowered through a temp. Every live arg is
\ PROT-GUARD'd first, so the checked FFI library routes integer/pointer calls
\ here.
: BFFI-CALL-N-CORE ( -- )
   BCB-ENTER
   20 SP $8 STR,                                       \ park caller x20 in frame slot
   20 SP 0 ADDI,                                       \ x20 = frame sp
   LBL FFI-SKIP !
   LBL FFI-LOOP !
   LBL FFI-DONE !
   14 8 CMPI,  C-LE FFI-SKIP LABEL@ BCOND,             \ nargs <= 8 -> no spill
      10 14 8 SUBI,                                    \ x10 = extra = nargs - 8
      11 10 3 LSLI,  11 11 $F ADDI,  11 11 4 LSRI,  11 11 4 LSLI,  \ salloc = (extra*8+$F)&~$F
      12 SP 0 ADDI,  12 12 11 SUB,  SP 12 0 ADDI,      \ sp -= salloc
      12 15 $40 ADDI,                                  \ x12 = src = argbuf + 8 cells
      13 SP 0 ADDI,                                    \ x13 = dst = sp
      FFI-LOOP LABEL@ LBL,
      10 FFI-DONE LABEL@ CBZ,                          \ extra == 0 -> done
         9 12 0 LDR,  9 13 0 STR,
         12 12 $8 ADDI,  13 13 $8 ADDI,
         10 10 1 SUBI,  FFI-LOOP LABEL@ B,
      FFI-DONE LABEL@ LBL,
   FFI-SKIP LABEL@ LBL,
   BFFI-LOAD-X0-X7
   16 BLR,
   SP 20 0 ADDI,                                       \ restore sp from x20
   20 SP $8 LDR,                                       \ restore caller x20
   BCB-LEAVE
   0 G-PUSH ;

: BFFI-CALL-N ( -- )
   16 G-POP  14 G-POP  15 G-POP
   BFFI-CALL-N-CORE ;

: BFFI-CALL-BOUNDED ( -- )
   16 G-POP                                            \ x16 = fn
   14 G-POP                                            \ x14 = nargs
   17 G-POP                                            \ x17 = writable lengths
   15 G-POP                                            \ x15 = argbuf
   BFFI-GUARD-BOUNDS
   BFFI-CALL-N-CORE ;

\ Returns an immutable pthread entry, not a Habu xt; its C argument is the
\ TASK-ABI descriptor and the body enters that task's VM state. It preserves
\ the whole AAPCS64 callee-save set, d8..d15 included. The alternate signal
\ stack is registered below the frame before the body and given back after it
\ (crash.f C-SIGNAL-STACK). It writes two task cells: the TCB address into the
\ task's data region, and DONE into the status after the body. Not RUNNING:
\ ACTIVATE stores it before pthread_create (src/lib/task.f), and a store here
\ could land after a TASK:HALT took RUNNING -> HALT-REQ in the create window.
\ DONE is release-published by STLR (as BATSTORE): it is the task's last write
\ another thread reads, so an owner polling TASK:DONE? (TASK-STATE@, an
\ acquire load) sees the body's writes without joining. STLR takes no offset,
\ so the status address is formed first, in x11.
: BTASK-ENTRY ( -- )
   LBL LBL {: entry:label done:label :}
   A entry ADR, A G-PUSH done B,
   entry LBL,
   SP SP $A0 SUBI,
   29 19 do i SP i 19 - cells STR, loop
   29 SP $50 STR, 30 SP $58 STR,
   16 8 do i SP i 8 - cells $60 + ENC-STRD EMITW loop
   29 SP $50 ADDI,                         \ previous FP/LR form the C frame
   19 0 0 ADDI,                            \ the descriptor across the registration
   C-SIGNAL-STACK                          \ this thread's own (crash.f): below the frame
   0 19 0 ADDI,
   9 0 TASK-ABI:XT-OFF LDR,
   XDS 0 TASK-ABI:STACK-OFF LDR,
   DATA 0 TASK-ABI:REGION-OFF LDR,
   DBASE 0 TASK-ABI:DBASE-OFF LDR,
   NDICT 0 TASK-ABI:NDICT-OFF LDR,
   CP 0 TASK-ABI:CP-OFF LDR,
   0 DATA TASK-TCB-CELL STR,
   ISB,  9 BLR,                            \ ISB: BCALLABLE's
   C-SIGNAL-STACK-OFF
   SP SP STACK-ABI:SIGNAL-BYTES ADDI-4K,   \ the band back: sp is the frame's again
   9 DATA TASK-TCB-CELL LDR,
   10 TASK-ABI:DONE MOVZ, 11 9 TASK-ABI:STATUS-OFF ADDI,  10 11 STLR,
   0 0 MOVZ,
   16 8 do i SP i 8 - cells $60 + ENC-LDRD EMITW loop
   29 19 do i SP i 19 - cells LDR, loop
   29 SP $50 LDR, 30 SP $58 LDR,
   SP SP $A0 ADDI, RET,
   done LBL, ;

\ Callback entry: CB-POOL C entries into checked code (src/lib/ffi-callback.f).
\ The thunk's frame on C's stack: C's callee-saved set at [sp..$A0) as
\ BTASK-ENTRY keeps it, the marshal frame CB-FRAME names (x0..x7, then d0..d7
\ from CB-FRAME-FLOATS), and the thunk's own state. Checked code may hold state
\ in x21..x25 and x29 (src/compiler/native/abi.f SCRATCH), so everything the
\ thunk needs after its BLR is on this frame, never in a register.
$A0 constant CBT-MARSHAL
CBT-MARSHAL CB-FRAME-FLOATS + constant CBT-FLOATS
$120 constant CBT-ROW                   \ the slot row's address
$128 constant CBT-REGION                \ the region the row named
$130 constant CBT-ROW-CLAIM             \ 1 when this entry claimed the row
$138 constant CBT-OWNER-CLAIM           \ 1 when this entry claimed CB-OWNER
$140 constant CBT-OUTER                 \ the CB-FRAME this entry replaced
$148 constant CBT-SLOT
$150 constant CBT-BYTES

\ Entered from a stub with the slot in x9. The row is claimed before the region
\ it names is read: LDAR the row owner; a thread already nested through this
\ slot claims nothing; 0 is CASAL'd 0 -> self; another thread dies. CB-ROW-BUSY
\ (read, or answered by a lost CAS) is a src/lib/task.f mover holding the row
\ for a few steps that call and wait on nothing, so the thunk spins on LDAR
\ with YIELD until the row is handed back and claims it as left; a row dropped
\ meanwhile reads unbound and dies unless a bind took it. A region stays mapped
\ while a row names it (task.f UNEXPOSE and TASK-END drop rows first), so the
\ region is read only then: its CB-OWNER claimed as BCB-ENTER claims it, a live
\ frame (CB-XDS <> 0) and the slot's xt, a ( -- ) dispatch that reads its
\ arguments through CB-FRAME and leaves its result in the x0 or d0 slot.
\ A body whose NDICT or CP differs from the frame's dies: the way out restores
\ C's callee-saved registers, the caller's VM among them, and would drop what a
\ definition added. Otherwise CB-FRAME, then the owner if claimed, then the
\ row are restored, each by STLR.
\ A C-started thread has no alternate signal stack. Only an owner-claiming
\ entry can be on one (an owner already its own means a registered thread), so
\ it queries before the branch and carves one below the frame when none is
\ registered (crash.f C-SIGNAL-STACK); a 16-byte cell at the branch's sp
\ records that, so the way out gives the band back before reading the frame.
: BCALLBACK-THUNK ( -- )
   LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL
   {: row-read:label row-wait:label row-held:label owner-held:label
      owner-kept:label row-kept:label no-table:label row-busy:label
      unbound:label owner-busy:label no-frame:label no-xt:label
      defined:label signal-had:label signal-kept:label :}
   SP SP CBT-BYTES SUBI,
   \ C's callee-saved set is saved and restored in this body, as BTASK-ENTRY
   \ does: tools/lint/clobber-lint.f proves x30 from the routine's own text.
   29 19 do i SP i 19 - cells STR, loop
   29 SP $50 STR, 30 SP $58 STR,
   16 8 do i SP i 8 - cells $60 + ENC-STRD EMITW loop
   29 SP $50 ADDI,                                     \ previous FP/LR form the C frame
   8 0 do i SP CBT-MARSHAL i cells + STR, loop
   8 0 do i SP CBT-FLOATS i cells + ENC-STRD EMITW loop
   9 SP CBT-SLOT STR,
   10 DATA-VA VA>N LIT64,                              \ x10 = the main region
   11 CB-ROWS MOVZ,  11 10 11 ADD,  11 11 LDAR,
   11 no-table CBZ,
   12 9 5 LSLI,  11 11 12 ADD,                         \ x11 = the row (four cells)
   11 SP CBT-ROW STR,
   12 CB-SELF,
   13 11 CB-ROW-OWNER ADDI,
   15 0 MOVZ,
   row-read LBL,
   14 13 LDAR,
   14 12 CMP,  C-EQ row-held BCOND,
   14 CB-ROW-BUSY CMPI,  C-EQ row-wait BCOND,
   14 row-busy CBNZ,
   14 12 13 CASAL,
   14 CB-ROW-BUSY CMPI,  C-EQ row-wait BCOND,
   14 row-busy CBNZ,
   15 1 MOVZ,
   row-held LBL,
   15 SP CBT-ROW-CLAIM STR,
   14 11 LDAR,                                         \ x14 = CB-ROW-REGION, offset 0
   14 unbound CBZ,
   14 SP CBT-REGION STR,
   13 CB-OWNER MOVZ,  13 14 13 ADD,
   15 13 LDAR,
   16 0 MOVZ,
   15 12 CMP,  C-EQ owner-held BCOND,
   15 owner-busy CBNZ,
   15 12 13 CASAL,
   15 owner-busy CBNZ,
   16 1 MOVZ,
   owner-held LBL,
   16 SP CBT-OWNER-CLAIM STR,
   15 14 CB-XDS LDR,
   15 no-frame CBZ,
   13 CB-XTS MOVZ,  13 10 13 ADD,  13 13 LDAR,
   13 no-xt CBZ,
   12 9 2 LSLI,  12 12 9 ADD,  12 12 3 LSLI,          \ five cells per descriptor
   13 13 12 ADD,  13 13 0 LDR,
   13 no-xt CBZ,
   12 14 CB-FRAME LDR,  12 SP CBT-OUTER STR,
   12 SP CBT-MARSHAL ADDI,  12 14 CB-FRAME STR,
   XDS 15 0 ADDI,
   DATA 14 0 ADDI,
   DBASE 14 CB-DBASE LDR,
   NDICT 14 CB-NDICT LDR,
   CP 14 CB-CP LDR,
   12 0 MOVZ,
   16 signal-had CBZ,                                  \ x16 = the owner claim, still live
      C-SIGNAL-STACK-ABSENT>R12
      12 signal-had CBZ,
      C-SIGNAL-STACK                                   \ sp drops below the band
      12 1 MOVZ,
   signal-had LBL,
   SP SP 16 SUBI,  12 SP 0 STR,                        \ 1 when the band below the frame is this entry's
   ISB,  13 BLR,                                       \ ISB: BCALLABLE's
   12 SP 0 LDR,  SP SP 16 ADDI,
   12 signal-kept CBZ,
      C-SIGNAL-STACK-OFF
      SP SP STACK-ABI:SIGNAL-BYTES ADDI-4K,            \ the band back: sp is the frame's again
   signal-kept LBL,
   14 SP CBT-REGION LDR,
   12 14 CB-NDICT LDR,  12 NDICT CMP,  C-NE defined BCOND,
   12 14 CB-CP LDR,  12 CP CMP,  C-NE defined BCOND,
   12 SP CBT-OUTER LDR,  12 14 CB-FRAME STR,
   10 0 MOVZ,
   12 SP CBT-OWNER-CLAIM LDR,  12 owner-kept CBZ,
      13 CB-OWNER MOVZ,  13 14 13 ADD,  10 13 STLR,
   owner-kept LBL,
   12 SP CBT-ROW-CLAIM LDR,  12 row-kept CBZ,
      13 SP CBT-ROW LDR,  13 13 CB-ROW-OWNER ADDI,  10 13 STLR,
   row-kept LBL,
   0 SP CBT-MARSHAL LDR,
   0 SP CBT-FLOATS ENC-LDRD EMITW
   16 8 do i SP i 8 - cells $60 + ENC-LDRD EMITW loop
   29 19 do i SP i 19 - cells LDR, loop
   29 SP $50 LDR, 30 SP $58 LDR,
   SP SP CBT-BYTES ADDI,
   RET,
   row-wait LBL,  YIELD,  row-read B,
   S\" hb: callback: no slot has been bound\n" no-table CB-DIE,
   S\" hb: callback: slot held by another thread\n" row-busy CB-DIE,
   S\" hb: callback: slot is not bound\n" unbound CB-DIE,
   S\" hb: callback: context busy on another thread\n" owner-busy CB-DIE,
   S\" hb: callback: context is not inside a foreign call\n" no-frame CB-DIE,
   S\" hb: callback: slot has no dispatch\n" no-xt CB-DIE,
   S\" hb: callback: the body defined\n" defined CB-DIE, ;

\ -1 answers the shared C thunk for the native compiler's marked address
\ carrier; other indices answer the seed stubs boot uses. This primitive ends
\ before the separately owned thunk begins.
: BCALLBACK-ENTRY ( -- )
   LBL LBL LBL LBL {: stubs:label done:label bad:label direct:label :}
   A G-POP
   9 -1 LIT64,  A 9 CMP,  C-EQ direct BCOND,
   A CB-POOL CMPI,  C-CS bad BCOND,
   B stubs ADR,
   A A 3 LSLI,  A B A ADD,  A G-PUSH
   done B,
   direct LBL,
   LCALLBACKADDR LABEL@ BL,  A G-PUSH  done B,
   S\" hb: callback: slot out of range\n" bad CB-DIE,
   stubs LBL,
   CB-POOL 0 do  9 i MOVZ,  LCALLBACKTHUNK LABEL@ B,  loop
   done LBL, ;

\ A native callback stub carries this address across FILE and image builds.
\ The thunk follows callback-entry's completed primitive record, so its sealed
\ helper record is disjoint even when an image also uses the public primitive.
: EMIT-CALLBACK-THUNK ( -- )
   LBL LBL {: body:label done:label :}
   s" (CALLBACK-THUNK)" LCALLBACKTHUNK LABEL@ LABEL>N
      done LABEL>N ENGINE-PRIMS:HELPER-REGISTER
   LCALLBACKTHUNK LABEL@ LBL,
   body B,
   \ The public primitive calls this tiny accessor. Its ADR points to the
   \ same member's C entry, so the image relocates it within this record.
   LCALLBACKADDR LABEL@ LBL,
   A LCALLBACKTHUNK LABEL@ ADR,  RET,
   body LBL,
   BCALLBACK-THUNK
   done LBL, ;

\ Mixed-ABI bounded calls guard every integer register slot (x0..x8) and
\ every caller-packed stack slot before the foreign branch.  The two extent
\ tables are distinct because stack slot zero is not integer slot zero.
: BFFI-CALL-ABI-BOUNDED-CORE ( bool -- ) {: reloc:bool :}
   16 G-POP                                            \ x16 = fn
   reloc if 2 G-POP then                               \ x2  = relocation mask
   14 G-POP                                            \ x14 = stack cell count
   12 G-POP                                            \ x12 = stack extents
   11 G-POP                                            \ x11 = x0..x8 extents
   13 G-POP                                            \ x13 = prepacked stack cells
   17 G-POP                                            \ x17 = FP argbuf
   15 G-POP                                            \ x15 = integer argbuf
   SP SP $40 SUBI,
   15 SP 0 STR,  17 SP $8 STR,  13 SP $10 STR,  11 SP $18 STR,
   12 SP $20 STR,  14 SP $28 STR,  16 SP $30 STR,
   reloc if 2 SP $38 STR, then
   17 11 0 ADDI,  14 9 MOVZ,                          \ guard x0..x8
   BFFI-GUARD-BOUNDS
   15 SP $10 LDR,  17 SP $20 LDR,  14 SP $28 LDR,     \ guard stack slots
   BFFI-GUARD-BOUNDS
   15 SP 0 LDR,  15 G-PUSH
   17 SP $8 LDR,  17 G-PUSH
   13 SP $10 LDR,  13 G-PUSH
   14 SP $28 LDR,  14 G-PUSH
   9 0 MOVZ,  9 G-PUSH  9 G-PUSH                       \ int-nargs=0, sret=0: BFFI-GUARD-BOUNDS already guarded x0..x8
   16 SP $30 LDR,  16 G-PUSH
   reloc if 2 SP $38 LDR, then
   SP SP $40 ADDI,
   reloc BFFI-CALL-ABI-CORE ;

: BFFI-CALL-ABI-BOUNDED ( -- )
   false BFFI-CALL-ABI-BOUNDED-CORE
   0 G-PUSH ;

: BFFI-CALL-ABI-R-BOUNDED ( -- )
   false BFFI-CALL-ABI-BOUNDED-CORE
   9 0 FMOVDX,  9 G-PUSH ;

\ A C struct returned by value: x0 then x1, both intact through BCB-LEAVE.
\ FFI stores the record they hold; an x8-indirect record is already written.
\ The relocation mask names the cells holding private struct copies'
\ prepacked addresses, which then point at the copies on the native stack.
: BFFI-CALL-STRUCT-BOUNDED ( -- )
   true BFFI-CALL-ABI-BOUNDED-CORE
   0 G-PUSH  1 G-PUSH ;

: BOPENRD ( -- )
   A G-POP  A OS-OPEN-RD  FS-SYS-PUSH ;

: BACCESS ( -- )
   1 G-POP  0 G-POP
   HB-TARGET-LINUX? IF
      2 1 0 ADDI,  1 0 0 ADDI,  0 99 MOVN,  3 0 MOVZ,
      NR-ACCESS SYS,  FS-SYS-PUSH
      exit
   THEN
   HB-TARGET-MACOS? IF NR-ACCESS SYS,  FS-SYS-PUSH  exit THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

: BUNLINK ( -- )
   0 G-POP
   HB-TARGET-LINUX? IF
      1 0 0 ADDI,  0 99 MOVN,  2 0 MOVZ,
      NR-UNLINK SYS,  FS-SYS-PUSH
      exit
   THEN
   HB-TARGET-MACOS? IF NR-UNLINK SYS,  FS-SYS-PUSH  exit THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

: BRENAME ( -- )
   1 G-POP  0 G-POP
   HB-TARGET-LINUX? IF
      3 1 0 ADDI,  1 0 0 ADDI,  0 99 MOVN,  2 99 MOVN,
      NR-RENAME SYS,  FS-SYS-PUSH
      exit
   THEN
   HB-TARGET-MACOS? IF NR-RENAME SYS,  FS-SYS-PUSH  exit THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

: BCHMOD ( -- )
   1 G-POP  0 G-POP
   HB-TARGET-LINUX? IF
      2 1 0 ADDI,  1 0 0 ADDI,  0 99 MOVN,  3 0 MOVZ,
      NR-CHMOD SYS,  FS-SYS-PUSH
      exit
   THEN
   HB-TARGET-MACOS? IF NR-CHMOD SYS,  FS-SYS-PUSH  exit THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

: BSYMLINK ( -- )
   2 G-POP  0 G-POP
   HB-TARGET-LINUX? IF 1 99 MOVN, ELSE
      HB-TARGET-MACOS? IF 1 1 MOVN, ELSE ENGINE-EMIT:TARGET-UNKNOWN THEN
   THEN
   3 0 MOVZ,  4 0 MOVZ,  5 0 MOVZ,
   NR-SYMLINKAT SYS,  FS-SYS-PUSH ;

: BREADLINK ( -- )
   3 G-POP  2 G-POP  1 G-POP
   2 3 PROT-GUARD:CALL                         \ x2 = kernel-written link buffer, x3 = length
   HB-TARGET-LINUX? IF 0 99 MOVN, ELSE
      HB-TARGET-MACOS? IF 0 1 MOVN, ELSE ENGINE-EMIT:TARGET-UNKNOWN THEN
   THEN
   4 0 MOVZ,  5 0 MOVZ,
   NR-READLINKAT SYS,  FS-SYS-PUSH ;

package LIBC-OS

\ Read this process's loader slot, as src/os/*/layout.f DLSYM-SLOT does.
\ Neither a library handle nor a resolved function survives this call.
\ x16 = this run's __text base, x1 = the NUL-terminated symbol name.
: DLSYM-AT ( -- )
   15 CODE-OFF LIT64,  16 16 15 SUB,
   17 16 IMAGE-TEXT-SIZE-OFF LDR,
   HB-TARGET-LINUX? if
      16 16 17 ADD,  16 16 $B8 LDR,
      0 0 MOVZ,
   else HB-TARGET-MACOS? if
      15 CODE-OFF LIT64,  17 17 15 ADD,  15 $3FFF LIT64,
      17 17 15 ADD,  15 $3FFF invert LIT64,  17 17 15 AND,
      16 16 17 ADD,  16 16 8 LDR,
      0 1 MOVN,
   else
      ENGINE-EMIT:TARGET-UNKNOWN
   then then
   16 BLR, ;

: DLSYM ( -- )   16 DATA RBASE-CELL LDR,  DLSYM-AT ;

\ Copy [x1, x2) of the region to the same offsets x12 bytes away, a cell at a
\ time; x1 and x2 are 8-byte aligned. Clobbers x1, x3, x4.
: ALIAS-COPY, ( -- )
   LBL LBL {: top:label done:label :}
   top LBL,  1 2 CMP,  C-CS done BCOND,
      3 1 0 LDR,  4 1 12 ADD,  3 4 0 STR,
      1 1 8 ADDI,  top B,
   done LBL, ;

\ The text's eight bytes from off as one little-endian cell, zero past len.
: TEXT-CELL ( ptr u8 n n -- n )
   {: a:ptr off:n len:n :}
   0
   8 0 ?do
      8 lshift
      off 7 + i -  dup len < if a swap + c@ or else drop then
   loop ;

\ Write the text to fd 2 from a stack frame filled by immediates, then release
\ the frame. A member the AOT linker copies holds only instructions:
\ src/habu/aot-link-arm.f RELOC-W32 decodes each of its words as one, so text
\ inline in the code is read as branches. Clobbers x0-x2, x9 and the system
\ call's registers.
: ERR-TEXT, ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 15 + -16 and {: frame:n :}
   SP SP frame SUBI,
   frame 8 / 0 ?do
      9 a i 8 * u TEXT-CELL LIT64,  9 SP i 8 * STR,
   loop
   0 2 MOVZ,  1 SP 0 ADDI,  2 u MOVZ,  NR-WRITE SYS,
   SP SP frame ADDI, ;

public

\ ( label -- ) Linux: x0 = a new memfd "hb-region" of REGION bytes,
\ close-on-exec and executable. A kernel before 6.3 refuses MFD_EXEC with
\ EINVAL, and that answer alone retries without it; any other failure jumps to
\ fail with no descriptor left open. Clobbers x0, x1, x5, x8, x16.
: REGION-MEMFD ( label -- ) {: fail:label :}
   LBL LBL LBL {: made:label retry:label sized:label :}
   SP SP 16 SUBI,
   1 $6F696765722D6268 LIT64,  1 SP 0 STR,          \ "hb-region" + NUL
   1 $6E MOVZ,  1 SP 8 STR,
   0 SP 0 ADDI,  1 $11 MOVZ,  NR-MEMFD-CREATE SYS,   \ MFD_CLOEXEC | MFD_EXEC
   C-CC made BCOND,
   1 21 MOVN,  0 1 CMP,  C-EQ retry BCOND,           \ -EINVAL
      SP SP 16 ADDI,  fail B,
   retry LBL,
   0 SP 0 ADDI,  1 1 MOVZ,  NR-MEMFD-CREATE SYS,     \ MFD_CLOEXEC
   made LBL,  SP SP 16 ADDI,  C-CS fail BCOND,
   5 0 0 ADDI,  1 REGION LIT64,  NR-FTRUNCATE SYS,  C-CC sized BCOND,
      0 5 0 ADDI,  NR-CLOSE SYS,  fail B,
   sized LBL,  0 5 0 ADDI, ;

\ ( fixed label -- ) macOS: map the region's RW alias over [x0, x0+REGION) with
\ mach_vm_remap (copy FALSE) and leave x0 = alias - x0. x16 = __text base on
\ entry. fixed maps the alias at exactly x1, else anywhere. The alias is made
\ with inheritance NONE: a forked child gets a private copy of the RX view and
\ no alias, and maps its own at the parent's address (FORK below).
\ Clobbers x0-x17; jumps to fail with SP restored if a lookup or the remap fails.
\ Frame: [0] &cur [8] &max [16] inheritance (the call's stack arguments),
\ [24] task port, [32] "mach_vm_remap", [48] "mach_task_self_", [64] alias,
\ [72] cur [76] max, [80] RX base, [88] __text base.
: REGION-REMAP ( bool label -- ) {: fixed:bool fail:label :}
   LBL LBL {: bad:label ok:label :}
   SP SP 96 SUBI,
   0 SP 80 STR,  16 SP 88 STR,
   9 SP 72 ADDI,  9 SP 0 STR,  9 SP 76 ADDI,  9 SP 8 STR,
   9 2 MOVZ,  9 SP 16 STR,                          \ VM_INHERIT_NONE
   fixed if 1 SP 64 STR, else 9 0 MOVZ,  9 SP 64 STR, then
   9 $5F6D765F6863616D LIT64,  9 SP 32 STR,
   9 $70616D6572 LIT64,  9 SP 40 STR,
   9 $7361745F6863616D LIT64,  9 SP 48 STR,
   9 $005F666C65735F6B LIT64,  9 SP 56 STR,
   1 SP 48 ADDI,  DLSYM-AT
   0 bad CBZ,  0 0 0 LDRW,  0 SP 24 STR,
   16 SP 88 LDR,  1 SP 32 ADDI,  DLSYM-AT
   0 bad CBZ,  16 0 0 ADDI,
   0 SP 24 LDR,  1 SP 64 ADDI,  2 REGION LIT64,  3 0 MOVZ,
   fixed if 4 0 MOVZ, else 4 1 MOVZ, then           \ VM_FLAGS_FIXED or _ANYWHERE
   5 0 0 ADDI,  6 SP 80 LDR,  7 0 MOVZ,             \ copy FALSE
   16 BLR,
   0 0 $FFFFFFFF ANDI,  0 bad CBNZ,                 \ kern_return_t
   0 SP 64 LDR,  1 SP 80 LDR,  0 0 1 SUB,
   SP SP 96 ADDI,  ok B,
   bad LBL,  SP SP 96 ADDI,  fail B,
   ok LBL, ;

\ ( pathz dst capacity -- length | -1 | -2 )
\ libc allocates its realpath result. Only a fitting result, including its NUL,
\ reaches dst; free runs after both success and insufficient capacity.
\ -1 = resolution/loader failure, -2 = no room for the complete C string.
: REALPATH ( -- )
   LBL LBL LBL LBL {: badcap:label failed:label short:label done:label :}
   LBL LBL LBL LBL {: count:label counted:label copy:label release:label :}
   2 G-POP  1 G-POP  0 G-POP
   SP SP 80 SUBI,
   0 SP 0 STR,  1 SP 8 STR,  2 SP 16 STR,
   2 0 CMPI,  C-LE badcap BCOND,
   1 2 PROT-GUARD:CALL
   9 $65657266 LIT64,  9 SP 56 STR,
   9 $687461706C616572 LIT64,  9 SP 64 STR,
   9 0 MOVZ,  9 SP 72 STR,
   1 SP 56 ADDI,  DLSYM
   0 failed CBZ,  0 SP 24 STR,
   1 SP 64 ADDI,  DLSYM
   0 failed CBZ,  16 0 0 ADDI,
   0 SP 0 LDR,  1 0 MOVZ,  16 BLR,
   0 failed CBZ,  0 SP 40 STR,
   9 0 0 ADDI,  10 0 MOVZ,
   count LBL,
      11 9 0 LDRB,  11 counted CBZ,
      9 9 1 ADDI,  10 10 1 ADDI,  count B,
   counted LBL,
   10 SP 48 STR,  12 SP 16 LDR,
   10 12 CMP,  C-CS short BCOND,
   9 SP 40 LDR,  11 10 1 ADDI,  10 SP 8 LDR,
   copy LBL,
      12 9 0 LDRB,  12 10 0 STRB,
      9 9 1 ADDI,  10 10 1 ADDI,  11 11 1 SUBI,
      11 copy CBNZ,
   release LBL,
   0 SP 40 LDR,  16 SP 24 LDR,  16 BLR,
   0 SP 48 LDR,  done B,
   short LBL,
   9 1 MOVN,  9 SP 48 STR,  release B,
   badcap LBL,  0 1 MOVN,  done B,
   failed LBL,  0 0 MOVN,
   done LBL,
   SP SP 80 ADDI,  0 G-PUSH ;

\ Darwin's libc fork reinitializes libSystem's child state, including Mach
\ reply ports. A raw syscall child can execute Habu but subsequent Mach VM
\ queries fail; the same query succeeds after libc fork. Linux keeps clone.
\ Each child owns its region before any Habu code runs, so its compiles stay
\ out of the parent's code and the parent's patches out of the child's. On
\ macOS libc fork gives the child a private copy of the RX view and no alias
\ (REGION-REMAP's inheritance NONE), and the child maps its own alias. On Linux
\ both views are MAP_SHARED, so a child would alias the parent's live memfd,
\ which the parent keeps writing once `fork` has returned to it: the parent
\ fills a fresh memfd with its region's used extent BEFORE the clone and the
\ child maps that memfd over both views, so the child's region is the parent's
\ as it stood when the parent made the copy, and no byte the parent writes
\ afterwards reaches it. A failure before the clone answers -1, as a refused
\ clone does; a child that cannot map its views dies named with exit 78.
: FORK ( -- )                         \ ( -- pid|-1 )
   HB-TARGET-LINUX? IF
      LBL LBL LBL LBL LBL {: unfd:label failed:label parent:label done:label dead:label :}
      LBL LBL LBL LBL LBL {: scan:label next:label full:label scanned:label plain:label :}
      SP SP 32 SUBI,                    \ [0] the child's memfd, [8] its filling view, [16] code end
      \ Delta 0 = no alias (a stripped image): the startup maps one anywhere but DBASE.
      9 DATA REGION-ALIAS:DELTA-CELL LDR,  9 plain CBZ,
      failed REGION-MEMFD  0 SP 0 STR,
      4 0 0 ADDI,  0 0 MOVZ,  1 REGION LIT64,  2 3 MOVZ,  3 MAP-SHARED LIT64,  5 0 MOVZ,
      NR-MMAP SYS,  C-CS unfd BCOND,
      0 SP 8 STR,  12 0 DBASE SUB,                          \ x12 = view - DBASE
      \ The used extent, every region byte a Habu word in the child can reach:
      \ the records below the larger of NDICT + 1 (DEF-OPEN keeps the open
      \ definition's record at NDICT) and the high-water (a raising ndict!
      \ re-exposes the retired ones), within DICT-CAP; then the control-flow stack
      \ and the code up to the larger of CP and the end of every record's span
      \ below the high-water (after a rollback a retired record's code sits above
      \ CP). CP and NDICT are this thread's registers; a task's are the snapshot
      \ ACTIVATE or EXPOSE took as it went live, and main may have published
      \ records and code past that snapshot since. The high-water is main's
      \ cell, read at DATA-VA: a task region does not hold it.
      2 DATA-VA VA>N LIT64,  2 2 NDICT-HW-CELL LDR,  3 NDICT 1 ADDI,  2 3 CMP,  2 2 3 C-HI CSEL,
      3 DICT-CAP LIT64,  2 3 CMP,  2 2 3 C-LS CSEL,
      3 DREC MOVZ,  2 2 3 MUL,  2 DBASE 2 ADD,  1 DBASE 0 ADDI,
      ALIAS-COPY,
      9 CP 0 ADDI,
      2 DATA-VA VA>N LIT64,  2 2 NDICT-HW-CELL LDR,  2 NDICT CMP,  2 2 NDICT C-HI CSEL,
      10 0 MOVZ,
      scan LBL,
         10 2 CMP,  C-CS scanned BCOND,
         11 DREC MOVZ,  11 10 11 MUL,  11 DBASE 11 ADD,
         13 11 40 LDR,  14 DICT-WL:NAMESPACE invert MOVN,  13 14 CMP,  C-EQ next BCOND,
         14 DICT-WL:RETIRED-NAMESPACE invert MOVN,  13 14 CMP,  C-EQ next BCOND,
         13 11 0 LDR,  14 11 8 LDR,
         15 CODE-SPAN:MASK LIT64,  15 14 15 AND,
         3 CODE-SPAN:FULL LIT64,  3 14 3 AND,  3 full CBNZ,  15 15 CODE-SPAN:INSN-BYTES ADDI,
         full LBL,
         15 13 15 ADD,  15 9 CMP,  C-LS next BCOND,  9 15 0 ADDI,
      next LBL,
         10 10 1 ADDI,  scan B,
      scanned LBL,
      9 9 7 ADDI,  9 9 3 LSRI,  9 9 3 LSLI,  9 SP 16 STR,
      1 CFSTK-OFF LIT64,  1 DBASE 1 ADD,  2 9 0 ADDI,
      ALIAS-COPY,
      0 SP 8 LDR,  1 REGION LIT64,  NR-MUNMAP SYS,  C-CS unfd BCOND,
      0 17 MOVZ,  1 0 MOVZ,  2 0 MOVZ,  3 0 MOVZ,  4 0 MOVZ,
      NR-FORK SYS,
      C-CS unfd BCOND,  0 parent CBNZ,
      \ The child: the filled memfd mapped RW at the parent's alias address, DBASE
      \ + delta, so every region's inherited delta cell stays true and none is
      \ rewritten, then RX over the inherited RX view, whose bytes the copy
      \ repeats under the code running here; the copied code is then flushed.
      10 SP 0 LDR,  12 DATA REGION-ALIAS:DELTA-CELL LDR,
      0 DBASE 12 ADD,  1 REGION LIT64,  2 3 MOVZ,  3 MAP-SHARED-FIXED LIT64,
      4 10 0 ADDI,  5 0 MOVZ,  NR-MMAP SYS,  C-CS dead BCOND,
      0 DBASE 0 ADDI,  1 REGION LIT64,  2 5 MOVZ,  3 MAP-SHARED-FIXED LIT64,
      4 10 0 ADDI,  5 0 MOVZ,  NR-MMAP SYS,  C-CS dead BCOND,
      0 10 0 ADDI,  NR-CLOSE SYS,  C-CS dead BCOND,
      9 DICT-SIZE LIT64,  9 DBASE 9 ADD,  11 SP 16 LDR,  LFLUSHTO LABEL@ BL,
      0 0 MOVZ,  done B,
      dead LBL,
         S\" hb: fork: cannot map the child's own region\n" ERR-TEXT,
         0 78 MOVZ,  NR-EXIT-GROUP SYS,
      parent LBL,                       \ the descriptor is released whatever close answers
         9 0 0 ADDI,  0 SP 0 LDR,  NR-CLOSE SYS,  0 9 0 ADDI,  done B,
      plain LBL,                        \ nothing to re-establish: a plain fork
         0 17 MOVZ,  1 0 MOVZ,  2 0 MOVZ,  3 0 MOVZ,  4 0 MOVZ,
         NR-FORK SYS,  C-CC done BCOND,  failed B,
      unfd LBL,  0 SP 0 LDR,  NR-CLOSE SYS,
      failed LBL,  0 0 MOVN,
      done LBL,
      SP SP 32 ADDI,  0 G-PUSH
      exit
   THEN
   HB-TARGET-MACOS? IF
      LBL LBL LBL {: failed:label done:label dead:label :}
      SP SP 16 SUBI,
      9 $6B726F66 LIT64,  9 SP 0 STR,    \ "fork" + NUL
      1 SP 0 ADDI,  DLSYM
      0 failed CBZ,  16 0 0 ADDI,  16 BLR,
      0 0 32 LSLI,  0 0 32 ASRI,        \ signed pid_t -> Habu cell
      0 done CBNZ,
      \ The child: its RX view is a private copy and it inherited no alias, so it
      \ maps its own over that copy at the parent's alias address, DBASE + delta,
      \ which the inherited NONE alias left free, and makes it RW. The alias
      \ address and delta are the same in parent and child, so every region's
      \ inherited delta cell stays true and none is rewritten.
      1 DATA REGION-ALIAS:DELTA-CELL LDR,  1 done CBZ,   \ delta 0: no alias to re-establish (a stripped image)
      1 1 DBASE ADD,
      0 DBASE 0 ADDI,  16 DATA RBASE-CELL LDR,  true dead REGION-REMAP
      0 0 DBASE ADD,  1 REGION LIT64,  2 3 MOVZ,  NR-MPROTECT SYS,
      C-CS dead BCOND,
      0 0 MOVZ,  done B,
      dead LBL,
         S\" hb: fork: cannot map the child's region alias\n" ERR-TEXT,
         0 78 MOVZ,  NR-EXIT-GROUP SYS,
      failed LBL,  0 0 MOVN,
      done LBL,
      SP SP 16 ADDI,  0 G-PUSH
      exit
   THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

;package

: BMKDIR ( -- )
   1 G-POP  0 G-POP
   HB-TARGET-LINUX? IF
      2 1 0 ADDI,  1 0 0 ADDI,  0 99 MOVN,
      NR-MKDIR SYS,  FS-SYS-PUSH
      exit
   THEN
   HB-TARGET-MACOS? IF NR-MKDIR SYS,  FS-SYS-PUSH  exit THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

: BRMDIR ( -- )
   0 G-POP
   HB-TARGET-LINUX? IF
      1 0 0 ADDI,  0 99 MOVN,  2 $200 MOVZ,
      NR-RMDIR SYS,  FS-SYS-PUSH
      exit
   THEN
   HB-TARGET-MACOS? IF NR-RMDIR SYS,  FS-SYS-PUSH  exit THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

: LINUX-STAT-FIX ( n -- )
   STAT-BUF !
   5 STAT-BUF @ 16 LDRW,  5 STAT-BUF @ 4 STRW,
   5 STAT-BUF @ 48 LDR,   6 STAT-BUF @ 88 LDR,   7 STAT-BUF @ 96 LDR,
   8 STAT-BUF @ 104 LDR,  9 STAT-BUF @ 112 LDR,
   5 STAT-BUF @ 96 STR,   6 STAT-BUF @ 48 STR,   7 STAT-BUF @ 56 STR,
   8 STAT-BUF @ 64 STR,   9 STAT-BUF @ 72 STR, ;

: BSTAT64 ( -- )
   1 G-POP  0 G-POP
   7 $90 MOVZ,  1 7 PROT-GUARD:CALL           \ x1 = kernel-written 144-byte statbuf
   LBL STAT-OK !
   LBL STAT-DONE !
   HB-TARGET-LINUX? IF
      2 1 0 ADDI,  1 0 0 ADDI,  0 99 MOVN,  3 0 MOVZ,
      NR-STAT64 SYS,
      9 C-CS CSET,  9 STAT-OK LABEL@ CBZ,
         STAT-DONE LABEL@ B,
      STAT-OK LABEL@ LBL,
      2 LINUX-STAT-FIX  9 0 MOVZ,
      STAT-DONE LABEL@ LBL,
      FS-SYS-PUSH-FLAG
      exit
   THEN
   HB-TARGET-MACOS? IF NR-STAT64 SYS,  FS-SYS-PUSH  exit THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

: BLSTAT64 ( -- )
   1 G-POP  0 G-POP  2 0 MOVZ,  3 0 MOVZ,  4 0 MOVZ,  5 0 MOVZ,
   7 $90 MOVZ,  1 7 PROT-GUARD:CALL           \ x1 = kernel-written 144-byte statbuf
   LBL STAT-OK !
   LBL STAT-DONE !
   HB-TARGET-LINUX? IF
      2 1 0 ADDI,  1 0 0 ADDI,  0 99 MOVN,  3 AT-SYMLINK-NOFOLLOW MOVZ,
      NR-LSTAT64 SYS,
      9 C-CS CSET,  9 STAT-OK LABEL@ CBZ,
         STAT-DONE LABEL@ B,
      STAT-OK LABEL@ LBL,
      2 LINUX-STAT-FIX  9 0 MOVZ,
      STAT-DONE LABEL@ LBL,
      FS-SYS-PUSH-FLAG
      exit
   THEN
   HB-TARGET-MACOS? IF NR-LSTAT64 SYS,  FS-SYS-PUSH  exit THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

: BGETDIRENTRIES64 ( -- )
   3 G-POP  2 G-POP  1 G-POP  0 G-POP
   1 2 PROT-GUARD:CALL  7 8 MOVZ,  3 7 PROT-GUARD:CALL
   NR-GETDIRENTRIES64 SYS,  FS-SYS-PUSH ;

: C-FLUSH-X9-LINE ( -- )
   9 DCCVAU,  DSB-ISH,  9 ICIVAU,  DSB-ISH,  ISB, ;

\ x9/x10 bound the changed bytes. The mandatory owner retires facts before
\ an instruction write or CP rewind can expose replacement code.
: INVALIDATE-CELL, ( n -- )
   {: cell:n :}
   LBL {: done:label :}
   9 10 CMP,  C-CS done BCOND,
   12 DATA cell LDR,  12 done CBZ,
   SP SP 64 SUBI,
   30 SP 0 STR,  9 SP 8 STR,  10 SP 16 STR,  11 SP 24 STR,
   13 SP 32 STR,  14 SP 40 STR,  15 SP 48 STR,
   9 G-PUSH  10 G-PUSH  12 BLR,
   15 SP 48 LDR,  14 SP 40 LDR,  13 SP 32 LDR,
   11 SP 24 LDR,  10 SP 16 LDR,  9 SP 8 LDR,  30 SP 0 LDR,
   SP SP 64 ADDI,
   done LBL, ;

: HOST-INVALIDATE, ( -- )
   NATIVE-OBS-CELLS:HOST-INVALIDATE INVALIDATE-CELL, ;

\ x9 is the dictionary count NDICT is about to drop to. The capture owner
\ ( n -- ) retires every captured definition at or past it. It runs inside
\ the count change every lowering shares, so it preserves x0-x17.
: CAPTURE-CUT, ( -- )
   LBL {: done:label :}
   12 DATA NATIVE-OBS-EXT:CAPTURE-INVALIDATE LDR,  12 done CBZ,
   SP SP 160 SUBI,
   18 0 ?do i SP i cells STR, loop
   30 SP 144 STR,
   9 G-PUSH  12 BLR,
   18 0 ?do i SP i cells LDR, loop
   30 SP 144 LDR,  SP SP 160 ADDI,
   done LBL, ;

: BPATCH32 ( -- )                \ ( w addr -- ): store through the alias, cache-sync
   A G-POP  B G-POP
   SP SP 32 SUBI,
   A SP 8 STR,  B SP 16 STR,     \ save w/addr first: the guard call clobbers x10 (B)
   7 4 MOVZ,  A 7 PROT-GUARD:CALL \ x9 is the target; protect its exact 4-byte write
   \ A generic instruction write carries no optimizer proof, even if its old
   \ bytes belonged to a native body. Invalidate before the write can publish.
   9 SP 8 LDR,  10 9 4 ADDI,  HOST-INVALIDATE,
   9 SP 8 LDR,  10 SP 16 LDR,
   11 DATA REGION-ALIAS:DELTA-CELL LDR,  11 9 11 ADD,  10 11 0 STRW,   \ through the alias
   C-FLUSH-X9-LINE
   SP SP 32 ADDI, ;

\ Both relocation maps are keyed on region offset, and their readers refuse the
\ image when the bytes lack the recorded shape (habu2.f RELOC-EMIT:EMIT-CALLS
\ and EMIT-ADDRS, exits CALLMAP-RC and ADDRMAP-RC), so a bit left over replaced
\ code is a defect. Both maps: one bit per region word, byte index = offset >>
\ 5, bit = (offset >> 2) & 7; only the base differs, so one emitter serves both.
\   x10 = the span's first region address, x9 = its byte length; both survive.
\ Clobbers x6, x7 and x12..x15.
package RELOC-EMIT
public
: CLEAR-SPAN, ( n -- ) {: mapoff:n :}
   LBL LBL {: loop:label done:label :}
   12 10 DBASE SUB,                             \ x12 = region byte offset of the first word
   13 12 9 ADD,                                 \ x13 = one past the last word's offset
   7 0 MOVZ,  7 7 1 SUBI,                       \ x7 = all ones, for inverting one bit mask
   loop LBL,
   12 13 CMP,  C-CS done BCOND,
   14 12 2 LSRI,  14 14 7 ANDI,                 \ x14 = bit number = word index & 7
   15 12 5 LSRI,                                \ x15 = map byte index = offset >> 5
   6 mapoff LIT64,  15 15 6 ADD,  15 DATA 15 ADD,
   6 1 MOVZ,  6 6 14 LSLV,  6 6 7 EOR,          \ x6 = ~(1 << bit)
   14 15 0 LDRB,  14 14 6 AND,  14 15 0 STRB,
   12 12 4 ADDI,
   loop B,
   done LBL, ;

: BCLEAR-MAPS ( -- )
   A G-POP  B G-POP
   SNAP-RELOC:CALLMAP-OFF RELOC-EMIT:CLEAR-SPAN,
   SNAP-RELOC:ADDRMAP-OFF RELOC-EMIT:CLEAR-SPAN, ;
;package

package NPUBWIN

private

\ BPATCH32 is the isolated writer: one instruction and its own line flush. The
\ three primitives below publish a whole routine or record pair as one guarded
\ copy through the RW alias and one flush.

\ Guard one aligned SPAN of the writable code interval, the span-wise form of
\ GUARD-CODE-WORD above: x9 = length, x10 = start. A zero length, a length or a
\ start that is not a whole number of instructions, a span that wraps, and a span
\ reaching outside [DBASE+DICT-SIZE, DBASE+REGION) all fail closed the way every
\ other protected-memory violation does. Clobbers x12/x13.
: GUARD-CODE-SPAN ( -- )
   LBL LBL {: ok:label trap:label :}
   9 trap CBZ,                                  \ a window of nothing is a caller bug
   12 9 3 ANDI,  12 trap CBNZ,                  \ length is whole instructions
   12 10 3 ANDI,  12 trap CBNZ,                 \ start is instruction aligned
   12 10 9 ADD,                                 \ x12 = end
   12 10 CMP,  C-CC trap BCOND,                 \ unsigned wrap
   13 DICT-SIZE LIT64,  13 DBASE 13 ADD,
   10 13 CMP,  C-CC trap BCOND,                 \ start below the code interval
   13 REGION LIT64,  13 DBASE 13 ADD,
   12 13 CMP,  C-HI trap BCOND,                 \ end past the region
   ok B,
   trap LBL,  0 ENGINE-ERROR:SEAL-VIOLATION MOVZ,  NR-EXIT-GROUP SYS,
   ok LBL, ;

\ Clear the call-map bit of every word in [x10, x10+x9): a bit left from a
\ reclaimed routine would make a snapshot restore relocate a displacement that
\ is no longer a call. Call map only: this publisher's address records go in
\ through `addrmap-set` after the copy; a CP rewind clears both maps over its
\ released span (src/habu/xref.f CODE-RECLAIM). Clobbers x6, x7 and x12..x15.
: CLEAR-CALLMAP-SPAN ( -- )
   SNAP-RELOC:CALLMAP-OFF RELOC-EMIT:CLEAR-SPAN, ;

public

\ code-publish ( src dst len -- ): write a whole emission into the code arena in
\ one copy: guard the span, copy through the region's RW alias, clear the call
\ map over the span, advance CP, flush. A publication is an append: dst must
\ equal CP or it fails closed, and CP advances in the same primitive, so a throw
\ never sees CP disagree with the arena. Rewriting a published instruction is
\ `patch32`. LFLUSH walks [x9, CP) once CP has advanced, the order EM-SEED-AOT
\ uses.
: BCODEPUBLISH ( -- )
   A G-POP  B G-POP  C G-POP     \ x9 = len, x10 = dst, x11 = src
   LBL LBL LBL {: loop:label done:label atcp:label :}
   SP SP 48 SUBI,
   A SP 8 STR,  B SP 16 STR,  C SP 24 STR,
   GUARD-CODE-SPAN                               \ x9/x10 survive: it clobbers x12/x13
   9 SP 8 LDR,  10 SP 16 LDR,
   10 CP CMP,  C-EQ atcp BCOND,                 \ a publication is an append, or nothing
      0 ENGINE-ERROR:SEAL-VIOLATION MOVZ,  NR-EXIT-GROUP SYS,
   atcp LBL,
   9 SP 8 LDR,  10 SP 16 LDR,  11 SP 24 LDR,
   12 0 MOVZ,                                    \ x12 = byte offset into the copy
   15 DATA REGION-ALIAS:DELTA-CELL LDR,  15 10 15 ADD,   \ x15 = dst through the alias
   loop LBL,
   12 9 CMP,  C-CS done BCOND,
   13 11 12 ADD,  14 13 0 LDRW,
   13 15 12 ADD,  14 13 0 STRW,
   12 12 4 ADDI,
   loop B,
   done LBL,
   9 SP 8 LDR,  10 SP 16 LDR,
   CLEAR-CALLMAP-SPAN                            \ the span's relocation record, reset with it
   9 SP 8 LDR,  10 SP 16 LDR,
   CP 10 9 ADD,                                  \ the slot is claimed
   9 10 0 ADDI,  LFLUSH LABEL@ BL,              \ flush [dst, CP), once
   SP SP 48 ADDI, ;

\ Install one prepared native package unit. The caller has relocated its code
\ and complete dictionary rows before this engine-owned publication boundary.
\ No dictionary name becomes live until both copies and the instruction-cache
\ flush finish. A namespace-only unit has records but no code; it copies only
\ the records and leaves CP and relocation maps unchanged.
: BNATIVEUNITPUBLISH ( -- ) B-TASK-GUARD
   OVERLAY-EMIT:GUARD,
   9 G-POP  10 G-POP  11 G-POP  12 G-POP  \ records-n, records-a, code-u, code-a
   LBL LBL LBL LBL LBL LBL
   {: bad:label widloop:label widdone:label cloop:label cdone:label rloop:label :}
   LBL LBL LBL LBL LBL LBL
   {: rdone:label iloop:label idone:label done:label records:label indexed:label :}
   SP SP 64 SUBI,
   9 SP 0 STR,  10 SP 8 STR,  11 SP 16 STR,  12 SP 24 STR,
   CP SP 32 STR,
   14 DICT-CAP LIT64,  14 14 NDICT SUB,
   9 14 CMP,  C-HI bad BCOND,       \ unsigned comparison rejects negative count
   EM-DEF-OCC:LROOM LABEL@ BL,
   14 DREC MOVZ,  14 9 14 MUL,  14 SP 48 STR,
   12 DREC MOVZ,  12 NDICT 12 MUL,  12 DBASE 12 ADD,  12 SP 40 STR,
   \ A sealed image cannot acquire a record in an already protected wordlist.
   17 DATA SEAL-NDICT-CELL LDR,  17 widdone CBZ,
   15 SP 8 LDR,  16 SP 0 LDR,
   widloop LBL,
      16 widdone CBZ,
      9 15 40 LDR,  LPROTWIDQ LABEL@ BL,
      13 bad CBNZ,
      15 15 DREC ADDI,  16 16 1 SUBI,  widloop B,
   widdone LBL,
   9 SP 16 LDR,  9 records CBZ,
   10 CP 0 ADDI,  GUARD-CODE-SPAN
   records LBL,
   9 SP 32 LDR,  10 SP 24 LDR,  11 SP 16 LDR,  12 0 MOVZ,
   15 DATA REGION-ALIAS:DELTA-CELL LDR,  15 9 15 ADD,    \ x15 = CP through the alias
   cloop LBL,
      12 11 CMP,  C-CS cdone BCOND,
      13 10 12 ADD,  14 13 0 LDRW,
      13 15 12 ADD,  14 13 0 STRW,
      12 12 4 ADDI,  cloop B,
   cdone LBL,
   9 SP 40 LDR,  10 SP 8 LDR,  11 SP 48 LDR,  12 0 MOVZ,
   15 DATA REGION-ALIAS:DELTA-CELL LDR,  15 9 15 ADD,    \ x15 = the rows through the alias
   rloop LBL,
      12 11 CMP,  C-CS rdone BCOND,
      13 10 12 ADD,  14 13 0 LDR,
      13 15 12 ADD,  14 13 0 STR,
      12 12 8 ADDI,  rloop B,
   rdone LBL,
   9 SP 16 LDR,  9 indexed CBZ,
   10 SP 32 LDR,
   SNAP-RELOC:CALLMAP-OFF RELOC-EMIT:CLEAR-SPAN,
   SNAP-RELOC:ADDRMAP-OFF RELOC-EMIT:CLEAR-SPAN,
   9 SP 16 LDR,  CP CP 9 ADD,
   9 SP 32 LDR,  LFLUSH LABEL@ BL,
   indexed LBL,
   15 SP 0 LDR,
   iloop LBL,
      15 idone CBZ,
      EM-DEF-OCC:LAPPEND LABEL@ BL,  LHIDXADD LABEL@ BL,
      15 15 1 SUBI,  iloop B,
   idone LBL,
   SP SP 64 ADDI,  done B,
   bad LBL,  0 ENGINE-ERROR:SEAL-VIOLATION MOVZ,  NR-EXIT-GROUP SYS,
   done LBL, ;

\ callmap-set ( addr -- ): record the instruction at addr as a call into the
\ engine's loaded __text, the only call a snapshot must rewrite (a region-to-
\ region call keeps its distance). Only the builder knows a branch's kind:
\ region bytes may hold data that decodes as a branch. habu2.f EMIT-CEMITBL
\ writes the same map for the other emitter; code-publish starts every span
\ cleared.
: BCALLMAPSET ( -- )
   LBL LBL {: inreg:label bad:label :}
   A G-POP                                       \ x9 = call-site address
   \ The map is indexed by region offset: an address outside the region would
   \ index into the DATA band after the map.
   10 9 3 ANDI,  10 bad CBNZ,                    \ a call site is a whole instruction
   10 9 DBASE SUB,                               \ x10 = candidate region byte offset
   9 DBASE CMP,  C-CC bad BCOND,                 \ below the region
   11 REGION LIT64,  10 11 CMP,  C-CS bad BCOND, \ at or past its end
   inreg B,
   bad LBL,  0 ENGINE-ERROR:SEAL-VIOLATION MOVZ,  NR-EXIT-GROUP SYS,
   inreg LBL,
   9 9 DBASE SUB,                                \ x9 = region byte offset
   10 9 2 LSRI,  10 10 7 ANDI,                   \ x10 = bit number
   11 9 5 LSRI,                                  \ x11 = map byte index
   12 SNAP-RELOC:CALLMAP-OFF LIT64,  11 11 12 ADD,  11 DATA 11 ADD,
   12 1 MOVZ,  12 12 10 LSLV,
   13 11 0 LDRB,  13 13 12 ORR,  13 11 0 STRB, ;

\ addrmap-set ( addr -- ): record the MOVZ/MOVK address carrier at addr, as
\ C-CODE-ADDR does (habu2.f EMIT-ADDR-SITE). One map serves code and DATA
\ addresses: the passes need only where a chain starts; the kind is per
\ relocation window, written beside it (src/compiler/native/publish.f).
\ EMIT-ADDRS runs once per moving band and leaves a chain whose value is
\ outside that band; DATA, at the fixed DATA-VA above both bands, is rewritten
\ by neither, and DATA does not move. Guarded as BCALLMAPSET is.
: BADDRMAPSET ( -- )
   LBL LBL {: inreg:label bad:label :}
   A G-POP                                       \ x9 = chain's first word address
   10 9 3 ANDI,  10 bad CBNZ,                    \ a chain starts on a whole instruction
   10 9 DBASE SUB,                               \ x10 = candidate region byte offset
   9 DBASE CMP,  C-CC bad BCOND,                 \ below the region
   11 REGION LIT64,  10 11 CMP,  C-CS bad BCOND, \ at or past its end
   inreg B,
   bad LBL,  0 ENGINE-ERROR:SEAL-VIOLATION MOVZ,  NR-EXIT-GROUP SYS,
   inreg LBL,
   9 9 DBASE SUB,                                \ x9 = region byte offset
   10 9 2 LSRI,  10 10 7 ANDI,                   \ x10 = bit number
   11 9 5 LSRI,                                  \ x11 = map byte index
   12 SNAP-RELOC:ADDRMAP-OFF LIT64,  11 11 12 ADD,  11 DATA 11 ADD,
   12 1 MOVZ,  12 12 10 LSLV,
   13 11 0 LDRB,  13 13 12 ORR,  13 11 0 STRB, ;

\ xref-retarget ( start len recidx -- ): point one record at one routine. A
\ record's first two cells (entry address, CODE-SPAN length) mean something
\ only together; patch32 pokes would tear each cell across two stores, so one
\ primitive writes both whole. Length first, then the address by STLR: a
\ reader that LDARs START sees the length that belongs to it and the code
\ code-publish flushed.
: BXREFRETARGET ( -- )
   LBL LBL LBL LBL {: checked:label pending:label done:label bad:label :}
   A G-POP  B G-POP  C G-POP     \ x9 = record index, x10 = len, x11 = start
   \ The index names a live row or the one pending row, record[NDICT]; one
   \ unsigned compare also rejects a negative index. The engine's own definition
   \ path writes the pending row before publishing its count (habu2.f
   \ EM-INTERPRET-COLON), and src/compiler/native/publish.f COMMIT-HELD prepares
   \ a withheld record's two cells this way. NDICT+1 and beyond trap.
   9 NDICT CMP,  C-HI bad BCOND,                 \ negative, or past the pending record
   14 CODE-SPAN:RAW-MAX LIT64,  10 14 CMP,  C-HI bad BCOND,
   14 10 3 ANDI,  14 bad CBNZ,                  \ encoded lengths remain instruction aligned
   14 CODE-SPAN:FULL LIT64,  10 14 CMP,  C-EQ bad BCOND, \ exact spans cannot be empty
   checked B,
   bad LBL,  0 ENGINE-ERROR:SEAL-VIOLATION MOVZ,  NR-EXIT-GROUP SYS,
   checked LBL,
   SP SP 32 SUBI,  30 SP 0 STR,  9 SP 8 STR,  10 SP 16 STR,  11 SP 24 STR,
   9 NDICT CMP,  C-EQ pending BCOND,
   9 1 MOVZ,  EM-DEF-OCC:LROOM LABEL@ BL,
   9 SP 8 LDR,
   14 DATA EM-DEF-OCC:PTR-CELL LDR,
   15 9 3 LSLI,  14 14 15 ADD,
   15 0 MOVZ,  15 14 8 STR,
   pending LBL,
   14 DREC MOVZ,  9 9 14 MUL,  9 DBASE 9 ADD,   \ x9 = &record[idx]
   15 DATA REGION-ALIAS:DELTA-CELL LDR,  15 9 15 ADD,   \ x15 = the record through the alias
   10 15 8 STR,                                  \ length first
   11 15 STLR,                                   \ then release-publish the address
   9 SP 8 LDR,  9 NDICT CMP,  C-EQ done BCOND,
   EM-DEF-OCC:LISSUE LABEL@ BL,
   done LBL,
   30 SP 0 LDR,  SP SP 32 ADDI, ;

;package

: BCLOSE ( -- )
   0 G-POP  NR-CLOSE SYS, ;

: BCLOSE-RC ( -- )
   0 G-POP  NR-CLOSE SYS,  SYS-PUSH ;

: BRBASE ( -- )
   9 DATA RBASE-CELL LDR,  9 G-PUSH ;

\ A typed quotation cell starts at zero. Reading it is valid, but no public
\ quotation call may branch to that address, even beneath a catch handler. The
\ ISB is the reader's half of publication (src/lib/task.f header), and it runs
\ only for an xt at or above the thread's CP register: an xt held as data may
\ name code main compiled after this thread started, and no instruction fetched
\ before the xt arrived may run in its place. An xt below CP needs none. On main
\ every xt is below the live CP and main's flushes end in an ISB. A task's CP is
\ the snapshot TASK:ACTIVATE took after every earlier flush, before the thread
\ existed, so an xt below it names code published before the thread. With a
\ definition pending the snapshot is that definition's body start, not CP: its
\ body is flushed only at `;`, after the thread exists, and CP may move back
\ within it. Everything below a live task's CP stays unwritten, since a rollback
\ with a task live lowers nothing (EM-DEF-OCC:LRETIRE), except a `does>` or
\ patch32 rewrite, which is outside the architecture's concurrent-modification
\ guarantee anyway (only B, BL, BRK, HVC, ISB, NOP, SMC and SVC may be rewritten
\ under a running thread). The compare is the control dependency that orders the
\ fence after the xt's load; the comparison is unsigned. A direct BL needs no
\ fence: its target was published before the caller was compiled. BEXECFLOOR and
\ the defer stub (habu2.f C-DEFER-EMIT-CODE) fence the same way.
: BCALLABLE ( -- )
   LBL LBL LBL {: live:label msg:label old:label :}
   S\" hb: unset quotation\n" {: ma:ptr mu:n :}
   9 live CBNZ,
   0 2 MOVZ,  1 msg ADR,  2 mu MOVZ,  NR-WRITE SYS,
   0 ENGINE-ERROR:CALLABLE-ABI MOVZ,  NR-EXIT-GROUP SYS,
   msg LBL,  ma mu BYTES,
   live LBL,  A CP CMP,  C-CC old BCOND,  ISB,
   old LBL, ;

: BEXEC ( -- )
   A G-POP  BCALLABLE
   SP SP 16 SUBI,  30 SP 0 STR,  A BLR,  30 SP 0 LDR,  SP SP 16 ADDI, ;

\ execute-floor ( xt -- flag ): run the xt, then flag whether it left XDS below
\ the base in S0-CELL, the comparison the interpreter's post-token depth floor
\ makes. Below the base, XDS is reset to the base before the flag is pushed, so
\ the push lands at the base and not in the low guard page. The FPRIM frame
\ holds the caller's x30 across the BLR.
: BEXECFLOOR ( -- )
   LBL LBL {: above:label old:label :}
   A G-POP  A CP CMP,  C-CC old BCOND,  ISB,           \ BCALLABLE's fence
   old LBL,  A BLR,
   B 0 MOVZ,
   A DATA S0-CELL LDR,  XDS A CMP,  C-CS above BCOND,   \ at or above the base: flag 0
      XDS A 0 ADDI,  B 0 MOVN,                          \ below: XDS := base, flag -1
   above LBL,
   B G-PUSH ;

\ catch ( xt -- exc ): a HNDF-SIZE handler frame chained through HND-CELL saves
\ the complete caller execution frame so a caught throw resumes it. Frame: 0
\ prev-HND | 8 data-sp(x19) | 16 machine-sp | 24 resume-pc | 32 link | 40
\ saved-RSP | 48 saved-LOOPSP | 56 sentinel | 64 saved base | 72 saved
\ capacity. BTHROW and EM-EVAL-THROW-RECOVER restore all of it; keep in step
\ with the bootstrap/cg/forth.fs mirror.
\ BCATCH-RUN is the frame alone: push, call, unlink. A normal return branches
\ to CATCH-PUSH with x9 = 0; a throw resumes at CATCH-RES, where it ends, with
\ the code in x9 and the frame already unlinked. Its caller places CATCH-PUSH.
: BCATCH-RUN ( -- )
   LBL CATCH-RES !
   LBL CATCH-PUSH !
   A G-POP  BCALLABLE
   SP SP STACK-ABI:CATCH-BYTES SUBI,
   30 SP 32 STR,
   11 DATA 8 LDR,  11 SP 0 STR,
   19 SP 8 STR,
   13 SP STACK-ABI:CATCH-BYTES ADDI,  13 SP 16 STR,
   12 CATCH-RES LABEL@ ADR,  12 SP 24 STR,
   11 DATA RSP-CELL LDR,     11 SP 40 STR,   \ save user return-stack depth
   11 DATA LOOPSP-CELL LDR,  11 SP 48 STR,   \ save loop-stack depth
   11 STACK-ABI:CATCH-MAGIC LIT64,  11 SP 56 STR,
   11 DATA STACK-ABI:BASE-CELL LDR,  11 SP STACK-ABI:CATCH-BASE STR,
   11 DATA STACK-ABI:CAP-CELL LDR,  11 SP STACK-ABI:CATCH-CAP STR,
   14 SP 0 ADDI,  14 DATA 8 STR,
   9 BLR,
   11 SP 0 LDR,  11 DATA 8 STR,
   30 SP 32 LDR,  SP SP STACK-ABI:CATCH-BYTES ADDI,
   9 0 MOVZ,  CATCH-PUSH LABEL@ B,
   CATCH-RES LABEL@ LBL, ;

\ BTHROW ( code -- ): unwind to the nearest catch handler; the tail of `throw`
\ (BTHROWPRIM) and the engine's throw sites. With EVALD>0 it first goes to
\ LEVALREC (habu2.f, through EVALREC-CELL: a leaf prim cannot name a habu2.f
\ label), which rolls back each escaped evaluate frame, so a checker or compile
\ error inside `evaluate` is a catchable throw and a catch inside the evaluated
\ source still handles its own throws.
: BTHROW ( -- )
   LBL THROW-NOH !  LBL THROW-EVAL !  LBL THROW-CORRUPT !  LBL THROW-CORRUPT-MSG !
   S\" hb: catch frame corrupt\n" {: ma:ptr mu:n :}
   A G-POP  15 9 0 ADDI,                               \ x15 = code
   12 DATA EVALD-CELL LDR,  12 THROW-EVAL LABEL@ CBNZ, \ inside evaluate → LEVALREC cleans frames first
   11 DATA 8 LDR,                                      \ x11 = nearest handler (HND-CELL)
   9 15 0 ADDI,                                        \ x9 = code
   11 THROW-NOH LABEL@ CBZ,
   \ handler-frame integrity: check sentinel + saved depths BEFORE any restore store
   14 STACK-ABI:CATCH-MAGIC LIT64,  10 11 56 LDR,  10 14 CMP,  C-NE THROW-CORRUPT LABEL@ BCOND,
   10 11 40 LDR,  14 STACK-ABI:RETURN-CELLS MOVZ,
   10 14 CMP,  C-HI THROW-CORRUPT LABEL@ BCOND,
   10 11 48 LDR,  14 STACK-ABI:LOOP-FRAMES MOVZ,
   10 14 CMP,  C-HI THROW-CORRUPT LABEL@ BCOND,
   14 11 STACK-ABI:CATCH-BASE LDR,  10 11 STACK-ABI:CATCH-CAP LDR,  12 11 8 LDR,
   8 THROW-CORRUPT LABEL@ STACK-GUARD:CHECK-CURSOR
   19 11 8 LDR,
   14 DATA STACK-ABI:BASE-CELL STR,
   10 11 STACK-ABI:CATCH-CAP LDR,  10 DATA STACK-ABI:CAP-CELL STR,
   10 11 40 LDR,  10 DATA RSP-CELL STR,               \ restore user return-stack depth
   10 11 48 LDR,  10 DATA LOOPSP-CELL STR,            \ restore loop-stack depth
   10 11 0 LDR,  10 DATA 8 STR,                       \ HND = prev
   30 11 32 LDR,  12 11 24 LDR,  13 11 16 LDR,
   SP 13 0 ADDI,  12 BR,
   THROW-EVAL LABEL@ LBL,
   10 DATA EVALREC-CELL LDR,  10 BR,                   \ x15 = code → LEVALREC (habu2.f)
   THROW-NOH LABEL@ LBL,
   LBL THROW-NOREC !  LBL THROW-NOREC-FB !  LBL THROW-NOREC-FB2 !
   LREPLROUTE LABEL@ BL,  9 THROW-NOREC LABEL@ CBZ,      \ only a live bare tty REPL may recover
   10 DATA RRECP-CELL LDR,  10 BR,
   THROW-NOREC LABEL@ LBL,
   9 15 0 ADDI,                                           \ restore the throw code after the route's ioctl
   10 DATA UNCGH-CELL LDR,  10 THROW-NOREC-FB LABEL@ CBZ, \ reporter installed? branch with x9 = code
   10 BR,                                                 \ LUNCAUGHT (habu2.f): rc-map + report out-of-range codes
   THROW-NOREC-FB LABEL@ LBL,                             \ pre-install boot fallback: silent but never masked
   0 9 0 ADDI,                                            \ code in [1,255] is kernel-representable: exit(code)
   9 1 CMPI,    C-LT THROW-NOREC-FB2 LABEL@ BCOND,
   9 255 CMPI,  C-GT THROW-NOREC-FB2 LABEL@ BCOND,
   NR-EXIT-GROUP SYS,
   THROW-NOREC-FB2 LABEL@ LBL,  0 UNCAUGHT-RC MOVZ,  NR-EXIT-GROUP SYS, \ else deterministic uncaught-throw rc
   THROW-CORRUPT LABEL@ LBL,                             \ forged/corrupt handler frame: fail closed before any restore
   0 2 MOVZ,  1 THROW-CORRUPT-MSG LABEL@ ADR,  2 mu MOVZ,  NR-WRITE SYS,
   0 ENGINE-ERROR:CATCH-STACK MOVZ,  NR-EXIT-GROUP SYS,
   THROW-CORRUPT-MSG LABEL@ LBL,  ma mu BYTES, ;

\ The `throw` primitive. A zero code is a caller bug, success passed as a
\ failure: it would unwind the body and let its catch read 0, so it unwinds as
\ ENGINE-ERROR:THROW-ZERO. A TASK:HALT unwind (HALT-UNWIND-CELL set) carries 0
\ by design and keeps it: only the task's root catch receives that code. The
\ engine's own BTHROW sites throw named nonzero codes or rethrow a caught one.
: BTHROWPRIM ( -- )
   LBL {: coded:label :}
   A G-POP  A coded CBNZ,
   10 DATA HALT-UNWIND-CELL LDR,  10 coded CBNZ,
   A ENGINE-ERROR:THROW-ZERO MOVZ,
   coded LBL,  A G-PUSH  BTHROW ;

\ BCATCH-FORWARD sits at CATCH-RES with the code in x9. With HALT-UNWIND-CELL
\ clear the code is delivered as always. Set, a TASK:HALT is unwinding the task:
\ while a frame remains below this one (HND nonzero) this is an inner catch,
\ which rethrows the code at once and never delivers it, so no code after a
\ `catch` observes a halt; the task's root catch (HND 0, src/lib/task.f
\ TASK-RUN-USER) clears the cell and delivers the code, so the runner ends the
\ task.
: BCATCH-FORWARD ( -- )
   LBL LBL {: inner:label keep:label :}
   10 DATA HALT-UNWIND-CELL LDR,  10 keep CBZ,
   10 DATA HND-CELL LDR,  10 inner CBNZ,
   10 DATA HALT-UNWIND-CELL STR,                 \ the root: x10 = HND = 0 clears it
   keep B,
   inner LBL,  9 G-PUSH  BTHROW
   keep LBL, ;

\ A catch that leaves its code in x9 and does not forward a halt, for the body
\ and cleanup of finally and c2-invoke, which take the halt into their own frame
\ and resume it after their cleanup.
: BCATCH-HELD ( -- )
   BCATCH-RUN  CATCH-PUSH LABEL@ LBL, ;

: BCATCH ( -- )
   BCATCH-RUN  BCATCH-FORWARD  CATCH-PUSH LABEL@ LBL,  9 G-PUSH ;

\ unit-compile-run ( guard source -- rc ): the engine owns the protected
\ dispatch hook for one source invocation. Busy or wild hooks return rc70
\ before source runs; a source throw returns its original catch status.
\ The hook remains callable after its donor dictionary records are reset,
\ so use the existing live-code interval rather than dictionary membership.
: BUNITCOMPILE ( -- )
   LBL LBL {: bad:label done:label :}
   A G-POP
   SP SP 16 SUBI,  9 SP 0 STR,             \ source quotation
   A G-POP                                  \ guard quotation
   10 UNIT-COMPILE-CELL LIT64,  10 DATA 10 ADD,
   11 10 0 LDR,  11 bad CBNZ,
   9 DBASE CMP,  C-CC bad BCOND,
   9 CP CMP,     C-CS bad BCOND,
   9 10 0 STR,
   9 SP 0 LDR,  9 G-PUSH
   BCATCH
   10 UNIT-COMPILE-CELL LIT64,  10 DATA 10 ADD,
   11 0 MOVZ,  11 10 0 STR,                  \ clear on return or throw
   SP SP 16 ADDI,  done B,
   bad LBL,
   SP SP 16 ADDI,  9 70 MOVZ,  9 G-PUSH
   done LBL, ;

\ ALLOT's missing owner callback throws through the engine helper above.
\ `allot` hands out zeroed space (docs/forth.md `create … allot`). Fresh DATA
\ pages are zero, so a positive count zeroes only the part of [old DP, new DP)
\ below the release high-water (DP-LOWER,), the space a lowered DP gave back;
\ above it nothing has been handed out, and the pages a large allot reserves
\ stay untouched. A negative count raises the high-water instead.
: BALLOT ( -- )
   LBL LBL LBL LBL LBL {: grow:label zero:label store:label no-owner:label missing:label :}
   B-TASK-GUARD
   A G-POP  7 DATA 0 LDR,  7 7 A ADD,  7 DP-CHECK
   A 0 CMPI,  C-GE grow BCOND,
   SP SP 32 SUBI,  30 SP 0 STR,  7 SP 8 STR,
   11 DATA NCOMP-DISPATCH:TARGET-DECL-CELL LDR,  11 no-owner CBZ,
   11 11 CHECKER-OWNER-ABI:GRID-REVOKE-OFF LDR,  11 missing CBZ,
   9 7 DATA SUB,  9 G-PUSH  11 BLR,
   no-owner LBL,
   7 SP 8 LDR,  30 SP 0 LDR,  SP SP 32 ADDI,
   10 11 DP-LOWER,  store B,
   grow LBL,
   10 7 A SUB,                                     \ x10 = old DP
   11 DATA DPHW-CELL LDR,  11 11 DATA ADD,  11 7 CMP,  11 11 7 C-LS CSEL,  \ x11 = min(high-water, new DP)
   zero LBL,  10 11 CMP,  C-CS store BCOND,
      31 10 0 STRB,  10 10 1 ADDI,  zero B,
   store LBL,
   7 DATA 0 STR,
   RET,
   missing LBL,
   9 -8574 LIT64,  9 G-PUSH  BTHROW ;

\ Selection takes the exact record a resolver has already borrowed. Validate
\ its region, stride and published bound before consulting the owner mapping.
: BDEFSELECT ( -- )
   LBL LBL {: bad:label done:label :}
   9 G-POP
   9 DBASE CMP,  C-CC bad BCOND,
   10 9 DBASE SUB,  11 DREC MOVZ,
   12 10 11 UDIV,  13 12 11 MUL,
   10 13 CMP,  C-NE bad BCOND,
   12 NDICT CMP,  C-CS bad BCOND,
   14 DATA EM-DEF-OCC:PTR-CELL LDR,
   13 12 3 LSLI,  13 14 13 ADD,  13 13 8 LDR,
   13 bad CBZ,
   12 G-PUSH  13 G-PUSH  done B,
   bad LBL,
   9 EM-DEF-OCC:E-SELECT LIT64,  9 G-PUSH  BTHROW
   done LBL, ;

\ A slot is a locator only. Equality with a nonzero issued occurrence is
\ checked before any byte of its record is read.
: BDEFRESOLVE ( -- )
   LBL LBL {: stale:label done:label :}
   10 G-POP  9 G-POP
   10 stale CBZ,
   9 NDICT CMP,  C-CS stale BCOND,
   11 DATA EM-DEF-OCC:PTR-CELL LDR,
   12 9 3 LSLI,  12 11 12 ADD,  12 12 8 LDR,
   12 10 CMP,  C-NE stale BCOND,
   11 DREC MOVZ,  9 9 11 MUL,  9 DBASE 9 ADD,
   9 G-PUSH  done B,
   stale LBL,
   9 EM-DEF-OCC:E-STALE LIT64,  9 G-PUSH  BTHROW
   done LBL, ;

\ A permitted CP rewind can release the tail of a still published code span.
\ Retire each affected slot before those bytes are reusable. Namespace rows
\ contain WIDs in their first two cells and have no code span.
: EMIT-DEF-OCC-RECLAIM ( -- )
   LBL LBL LBL LBL {: loop:label next:label full:label done:label :}
   EM-DEF-OCC:LRECLAIM LABEL@ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   PROF-ABI:LPROFRECLAIM LABEL@ BL,
   30 SP 0 LDR,  SP SP 16 ADDI,
   10 CP 0 ADDI,  HOST-INVALIDATE,
   10 0 MOVZ,
   loop LBL,
      10 NDICT CMP,  C-CS done BCOND,
      11 DREC MOVZ,  11 10 11 MUL,  11 DBASE 11 ADD,
      12 11 40 LDR,  13 DICT-WL:NAMESPACE invert MOVN,
      12 13 CMP,  C-EQ next BCOND,
      13 DICT-WL:RETIRED-NAMESPACE invert MOVN,  12 13 CMP,  C-EQ next BCOND,
      13 11 0 LDR,  13 CP CMP,  C-CS next BCOND,
      14 11 8 LDR,
      15 CODE-SPAN:MASK LIT64,  15 14 15 AND,
      12 CODE-SPAN:FULL LIT64,  12 14 12 AND,
      12 full CBNZ,  15 15 CODE-SPAN:INSN-BYTES ADDI,
      full LBL,
      15 13 15 ADD,
      15 9 CMP,  C-LS next BCOND,
      12 DATA EM-DEF-OCC:PTR-CELL LDR,
      14 10 3 LSLI,  12 12 14 ADD,
      14 0 MOVZ,  14 12 8 STR,
   next LBL,
      10 10 1 ADDI,  loop B,
   done LBL,  RET, ;

\ GUARDED-EXTENT? ( x14 = base, x11 = capacity ): prove the stack is one
\ MEM:ALLOC-GUARDED mapped. No push is bounds-checked: the inaccessible page above
\ the mapping enforces capacity, so a `create BUF … allot` base would overflow
\ into the DP heap. Proof: base and capacity non-zero and PAGE-BYTES aligned,
\ base+capacity does not wrap, and base lies outside the DATA region. The last
\ clause excludes every DP-heap address (DATA-VA is itself page aligned).
\ Clobbers x12 and x15.
: GUARDED-EXTENT? ( label -- )
   {: bad:label :}
   14 bad CBZ,
   11 bad CBZ,
   15 STACK-ABI:PAGE-BYTES 1 - LIT64,
   12 14 15 AND,  12 bad CBNZ,
   12 11 15 AND,  12 bad CBNZ,
   12 14 11 ADD,  12 14 CMP,  C-CC bad BCOND,
   15 DATA-VA VA>N LIT64,  12 14 15 SUB,
   15 DATA-SIZE LIT64,  12 15 CMP,  C-CC bad BCOND, ;

\ run-in-stack ( xt base size -- ): run xt on a fresh data stack (x19=base,
\ full-ascending). The extent becomes allocation authority, saved with XDS so
\ return and unwind restore the caller. An unguarded extent is a caller error
\ and throws STACK-ABI:E-STACK-UNGUARDED (BTHROW inlined as in BFINALLY, so
\ this word sits below it); a malformed descriptor is still the fail-closed
\ STACK-BOUNDS exit, since it can arrive from a saved frame or a task with no
\ caller to throw to. No STACK-GUARD:CHECK-CURSOR on entry: GUARDED-EXTENT?
\ already proves every branch it would emit with cursor = base. The exit check
\ stays: the callback may have moved the descriptor.
: BRUNSTACK ( -- )
   LBL LBL LBL {: bad:label unguarded:label done:label :}
   12 XDS 24 SUBI,
   9 12 0 LDR,  14 12 8 LDR,  11 12 16 LDR,     \ xt, base, capacity; no pop yet
   BCALLABLE
   unguarded GUARDED-EXTENT?
   XDS XDS 24 SUBI,
   SP SP 32 SUBI,  30 SP 0 STR,  XDS SP 8 STR,
   12 DATA STACK-ABI:BASE-CELL LDR,  12 SP 16 STR,
   12 DATA STACK-ABI:CAP-CELL LDR,  12 SP 24 STR,
   14 DATA STACK-ABI:BASE-CELL STR,  11 DATA STACK-ABI:CAP-CELL STR,
   XDS 14 0 ADDI,
   9 BLR,
   14 SP 16 LDR,  10 SP 24 LDR,  12 SP 8 LDR,
   0 bad STACK-GUARD:CHECK-CURSOR
   14 DATA STACK-ABI:BASE-CELL STR,
   12 SP 24 LDR,  12 DATA STACK-ABI:CAP-CELL STR,
   XDS SP 8 LDR,  30 SP 0 LDR,  SP SP 32 ADDI,
   done B,
   unguarded LBL,
   9 STACK-ABI:E-STACK-UNGUARDED LIT64,  9 G-PUSH  BTHROW
   bad LBL,  STACK-GUARD:EXIT-BOUNDS
   done LBL, ;

\ evaluate-closed ( ptr u8 n -- ): evaluate the text as a closed program on a
\ data stack of its own: the caller's extent and cursor wait in this frame and
\ the text takes a PAGE-BYTES guarded stack from the CLOSED-FREE-CELL pool (an
\ empty pool maps one with STACK-GUARD:EMIT-MAP, inline: a primitive names no
\ label). `depth` starts at 0 and the cell under the floor is the stack's low
\ guard page; a token reaching under it is refused, rc 70, through LEVALREC.
\ The evaluate frame EMIT-EVAL-BODY pushes records the stack (EVAL-SEG) and
\ returns to `back`. A throw out of the text unwinds to the caller's handler;
\ LEVALREC gives the stack back and restores the caller's extent from this
\ frame: base, capacity, cursor at [0], [8], [16], the text's stack at [24]
\ (habu2.f EM-EVAL-THROW-RECOVER). A text ending inside a definition it opened
\ is refused, rc 74 (habu2.f C-DEF-SOURCE-END). A clean return gives the stack
\ back here and refuses residue with STACK-ABI:E-EVAL-RESIDUE, BTHROW inlined
\ as in BRUNSTACK.
: B-EVAL-CLOSED ( -- )
   LBL LBL LBL LBL {: have:label take:label back:label done:label :}
   B-TASK-GUARD
   B G-POP  A G-POP                                  \ x10 = u, x9 = a
   SP SP 32 SUBI,
   11 DATA STACK-ABI:BASE-CELL LDR,  11 SP 0 STR,
   12 DATA STACK-ABI:CAP-CELL LDR,  12 SP 8 STR,
   XDS SP 16 STR,
   13 DATA CLOSED-FREE-CELL LDR,  13 have CBNZ,
   11 9 0 ADDI,                                      \ a waits in x11: the map never writes x10 or x11
   STACK-ABI:PAGE-BYTES 13 STACK-GUARD:EMIT-MAP
   9 11 0 ADDI,
   take B,
   have LBL,
   12 13 0 LDR,  12 DATA CLOSED-FREE-CELL STR,       \ an idle stack's first cell links the next
   take LBL,
   13 SP 24 STR,
   13 DATA STACK-ABI:BASE-CELL STR,
   12 STACK-ABI:PAGE-BYTES LIT64,  12 DATA STACK-ABI:CAP-CELL STR,
   XDS 13 0 ADDI,
   6 0 MOVZ,  7 10 0 ADDI,
   30 back ADR,
   LEVALBODY LABEL@ B,
   back LBL,
   11 SP 24 LDR,                                     \ the text's stack, back to the pool
   12 DATA CLOSED-FREE-CELL LDR,  12 11 0 STR,  11 DATA CLOSED-FREE-CELL STR,
   12 SP 0 LDR,  12 DATA STACK-ABI:BASE-CELL STR,
   12 SP 8 LDR,  12 DATA STACK-ABI:CAP-CELL STR,
   13 XDS 0 ADDI,  XDS SP 16 LDR,                    \ x13 = the text's cursor
   SP SP 32 ADDI,
   13 11 CMP,  C-EQ done BCOND,
   9 STACK-ABI:E-EVAL-RESIDUE LIT64,  9 G-PUSH  BTHROW
   done LBL, ;

\ source-unit-run ( xt -- ): close one whole callback at the caller's cursor.
\ Unlike evaluate-closed, this does not enter an evaluator frame or change
\ SOURCE. A callback that throws unwinds through the caller's existing catch;
\ a clean callback must return to the saved cursor. Compare before any data
\ stack push so a callback that fills the physical extent reports residue.
\ source-file-run ( xt -- ) runs the unit of a file the loader opened
\ (src/core/include.f LOAD-UNIT) the same way. That file is not the text of the
\ observed word whose run is pending (the include or require): the run is not
\ pending while the file runs, so no evaluate inside adopts the observer
\ (EMIT-EVAL-BODY), and it is pending again when the file returns or throws.
\ Text a word evaluates through source-unit-run (RUN-CLOSED, RUN-EVAL-RANGE)
\ is that word's own and stays observed. The frame keeps the suspended
\ observer at 32, 0 when the file suspended none.
: B-UNIT-RUN ( bool -- ) {: file:bool :}
   LBL LBL LBL LBL LBL LBL LBL LBL
      {: have:label take:label caught:label restore:label done:label no-snap:label
         other:label unpended:label :}
   file if 48 else 32 then {: bytes:n :}
   B-TASK-GUARD
   A G-POP  BCALLABLE
   \ The child uses a fresh guarded stack; retain the parent action's stack
   \ interval after the loader consumed its arguments and before that swap.
   10 DATA SOURCE-EXEC-CTX-CELL LDR,  10 no-snap CBZ,
   11 10 SX-ARMED LDR,  11 1 CMPI,  C-NE other BCOND,
   11 10 SX-PENDING LDR,  11 other CBZ,
   11 10 SX-SUSPEND LDR,  11 other CBNZ,
   11 10 SX-EVAL LDR,  12 DATA EVAL-TOP-CELL LDR,
   11 12 CMP,  C-NE other BCOND,
   11 DATA S0-CELL LDR,  11 XDS 11 SUB,  11 11 3 LSRI,
   11 10 SX-DEPTH STR,
   11 DATA RSP-CELL LDR,  11 10 SX-RDEPTH STR,
   file if
      11 0 MOVZ,  11 10 SX-PENDING STR,
      no-snap B,
      other LBL,  10 0 MOVZ,
   else
      other LBL,
   then
   no-snap LBL,
   SP SP bytes SUBI,
   file if  10 SP 32 STR,  then
   11 DATA STACK-ABI:BASE-CELL LDR,  11 SP 0 STR,
   12 DATA STACK-ABI:CAP-CELL LDR,   12 SP 8 STR,
   XDS SP 16 STR,
   9 SP 24 STR,
   13 DATA CLOSED-FREE-CELL LDR,  13 have CBNZ,
   STACK-ABI:PAGE-BYTES 13 STACK-GUARD:EMIT-MAP
   take B,
   have LBL,
   12 13 0 LDR,  12 DATA CLOSED-FREE-CELL STR,
   take LBL,
   9 SP 24 LDR,  13 SP 24 STR,
   13 DATA STACK-ABI:BASE-CELL STR,
   12 STACK-ABI:PAGE-BYTES LIT64,  12 DATA STACK-ABI:CAP-CELL STR,
   XDS 13 0 ADDI,
   \ Same handler shape as BCATCH, but do not push its status on the callback's
   \ stack: the clean callback may have filled that stack exactly. It reads no
   \ HALT-UNWIND-CELL: a task never reaches it, B-TASK-GUARD above exits
   \ on a task's thread.
   SP SP STACK-ABI:CATCH-BYTES SUBI,
   30 SP 32 STR,
   11 DATA 8 LDR,  11 SP 0 STR,
   19 SP 8 STR,
   13 SP STACK-ABI:CATCH-BYTES ADDI,  13 SP 16 STR,
   12 caught ADR,  12 SP 24 STR,
   11 DATA RSP-CELL LDR,     11 SP 40 STR,
   11 DATA LOOPSP-CELL LDR,  11 SP 48 STR,
   11 STACK-ABI:CATCH-MAGIC LIT64,  11 SP 56 STR,
   11 DATA STACK-ABI:BASE-CELL LDR,  11 SP STACK-ABI:CATCH-BASE STR,
   11 DATA STACK-ABI:CAP-CELL LDR,  11 SP STACK-ABI:CATCH-CAP STR,
   14 SP 0 ADDI,  14 DATA 8 STR,
   9 BLR,
   11 SP 0 LDR,  11 DATA 8 STR,
   30 SP 32 LDR,  SP SP STACK-ABI:CATCH-BYTES ADDI,
   9 0 MOVZ,
   caught LBL,
   9 restore CBNZ,
   11 SP 24 LDR,
   XDS 11 CMP,  C-EQ restore BCOND,
   9 STACK-ABI:E-EVAL-RESIDUE LIT64,
   C-CS restore BCOND,
   9 70 MOVZ,
   restore LBL,
   11 SP 24 LDR,
   12 DATA CLOSED-FREE-CELL LDR,  12 11 0 STR,  11 DATA CLOSED-FREE-CELL STR,
   12 SP 0 LDR,  12 DATA STACK-ABI:BASE-CELL STR,
   12 SP 8 LDR,  12 DATA STACK-ABI:CAP-CELL STR,
   file if
      12 SP 32 LDR,  12 unpended CBZ,
      11 1 MOVZ,  11 12 SX-PENDING STR,
      unpended LBL,
   then
   XDS SP 16 LDR,  SP SP bytes ADDI,
   9 done CBZ,
   9 G-PUSH  BTHROW
   done LBL, ;

: B-SOURCE-UNIT-RUN ( -- ) false B-UNIT-RUN ;
: B-SOURCE-FILE-RUN ( -- ) true B-UNIT-RUN ;

: BSOURCEEXECRUN ( -- )
   LBL LBL LBL LBL {: bad:label checked:label done:label no-parent:label :}
   SP SP SOURCE-EXEC-FRAME-BYTES SUBI,  30 SP SX-LR STR,
   9 G-POP  9 SP SX-CONT STR,
   9 G-POP  9 SP SX-AFTER STR,
   9 G-POP  9 SP SX-BEFORE STR,
   9 G-POP  9 SP SX-LEN STR,
   9 G-POP  9 SP SX-SRC STR,
   9 G-POP  9 SP SX-UNIT STR,
   9 SP SX-LEN LDR,  9 0 CMPI,  C-LT bad BCOND,
   9 SP SX-BEFORE LDR,  9 DBASE CMP,  C-CC bad BCOND,
   9 CP CMP,  C-CS bad BCOND,
   9 SP SX-AFTER LDR,  9 DBASE CMP,  C-CC bad BCOND,
   9 CP CMP,  C-CS bad BCOND,
   9 SP SX-CONT LDR,  9 DBASE CMP,  C-CC bad BCOND,
   9 CP CMP,  C-CS bad BCOND,
   9 DATA SOURCE-EXEC-CTX-CELL LDR,  9 SP SX-PREV STR,
   9 no-parent CBZ,
      10 0 MOVN,  10 9 SX-CONSTRUCTION STR,
   no-parent LBL,
   9 0 MOVZ,
   9 SP SX-ARMED STR,  9 SP SX-PENDING STR,  9 SP SX-SUSPEND STR,
   9 0 MOVN,  9 SP SX-CONSTRUCTION STR,  9 SP SX-POS STR,
   9 SP 0 ADDI,  9 DATA SOURCE-EXEC-CTX-CELL STR,
   9 SP SX-CONT LDR,  9 G-PUSH
   BCATCH
   9 G-POP  9 SP SX-LEN STR,                \ status; source extent is no longer needed
   15 9 0 ADDI,  LSOURCEEXECFINISH LABEL@ BL,
   9 SP SX-PREV LDR,  9 DATA SOURCE-EXEC-CTX-CELL STR,
   9 15 0 ADDI,
   checked B,
   bad LBL,
   9 70 MOVZ,
   checked LBL,
   30 SP SX-LR LDR,  SP SP SOURCE-EXEC-FRAME-BYTES ADDI,
   9 G-PUSH
   done LBL, ;


: BSOURCEEXECLIVE ( -- )
   LBL LBL {: absent:label done:label :}
   9 DATA SOURCE-EXEC-CTX-CELL LDR,  9 absent CBZ,
   9 0 MOVN,  done B,
   absent LBL,  9 0 MOVZ,
   done LBL,  9 G-PUSH ;

\ A loader cut may carry parent data cells across the child's guarded stack.
\ All compiler state and the return stack must still be closed at that cut.
\ The floors are the owned range's, so a loop reading a buffer an observed
\ word evaluates (its frame's EVAL-FRAME:SX-CTX set, EMIT-EVAL-BODY) is never
\ clean: no boundary, no cut and no balanced position there. x10 = the
\ observer, whose SX-EVAL every caller has matched to EVAL-TOP-CELL.
: BSOURCEEXECCLEAN ( -- )
   LBL LBL {: bad:label done:label :}
   9 10 SX-EVAL LDR,  9 9 EVAL-FRAME:SX-CTX LDR,  9 bad CBNZ,
   9 DATA PEND-CELL LDR,  9 bad CBNZ,
   9 DATA TXN-ACTIVE-CELL LDR,  9 bad CBNZ,
   9 DATA P2-CELL LDR,  9 bad CBNZ,
   9 DATA JIT-SNAP:SP-CELL LDR,  9 bad CBNZ,
   12 CFSTK-OFF LIT64,  12 DBASE 12 ADD,  9 12 0 LDR,  9 bad CBNZ,
   9 DATA DOESB-CELL LDR,  9 bad CBNZ,
   9 10 SX-RDEPTH LDR,  12 10 SX-RFLOOR LDR,
   9 12 CMP,  C-NE bad BCOND,
   9 0 MOVN,  done B,
   bad LBL,  9 0 MOVZ,
   done LBL,  9 G-PUSH ;

: BSOURCEEXECBALANCED ( -- )
   LBL LBL {: bad:label done:label :}
   BSOURCEEXECCLEAN
   9 G-POP  9 bad CBZ,
   9 10 SX-DEPTH LDR,  12 10 SX-FLOOR LDR,
   9 12 CMP,  C-NE bad BCOND,
   9 0 MOVN,  done B,
   bad LBL,  9 0 MOVZ,
   done LBL,  9 G-PUSH ;

: BSOURCEEXECBOUNDARY ( -- )
   LBL LBL {: bad:label done:label :}
   10 DATA SOURCE-EXEC-CTX-CELL LDR,  10 bad CBZ,
   11 10 SX-ARMED LDR,  11 bad CBZ,
   11 10 SX-SUSPEND LDR,  11 1 CMPI,  C-NE bad BCOND,
   11 10 SX-EVAL LDR,  12 DATA EVAL-TOP-CELL LDR,
   11 12 CMP,  C-NE bad BCOND,
   BSOURCEEXECBALANCED  done B,
   bad LBL,  9 0 MOVZ,  9 G-PUSH
   done LBL, ;

: BSOURCEEXECLOADERCUT ( -- )
   LBL LBL {: bad:label done:label :}
   10 DATA SOURCE-EXEC-CTX-CELL LDR,  10 bad CBZ,
   11 10 SX-ARMED LDR,  11 bad CBZ,
   11 10 SX-SUSPEND LDR,  11 1 CMPI,  C-NE bad BCOND,
   11 10 SX-EVAL LDR,  12 DATA EVAL-TOP-CELL LDR,
   11 12 CMP,  C-NE bad BCOND,
   BSOURCEEXECCLEAN  done B,
   bad LBL,  9 0 MOVZ,  9 G-PUSH
   done LBL, ;

: BSOURCEEXECPOSITION ( -- )
   LBL LBL {: absent:label done:label :}
   10 DATA SOURCE-EXEC-CTX-CELL LDR,  10 absent CBZ,
   11 10 SX-ARMED LDR,  11 absent CBZ,
   11 10 SX-SUSPEND LDR,  11 1 CMPI,  C-NE absent BCOND,
   11 10 SX-POS LDR,  11 0 CMPI,  C-LT absent BCOND,
   12 10 SX-EVAL LDR,  13 DATA EVAL-TOP-CELL LDR,
   12 13 CMP,  C-NE absent BCOND,
   11 G-PUSH
   BSOURCEEXECBALANCED done B,
   absent LBL,  9 0 MOVZ,  9 G-PUSH  9 G-PUSH
   done LBL, ;

: BSOURCEEXECHANDLER ( -- )
   9 0 MOVZ,  9 G-PUSH ;

\ finally ( body cleanup -- ): preserve the body's result row on success;
\ cleanup runs outside the body's handler so its throw supersedes that body's.
\ Frame: [0] cleanup | [8] code | [16] halt, the HALT-UNWIND-CELL value the
\ body's throw carried. Neither catch forwards a halt. The frame takes the cell
\ and clears it, so the cleanup's own catches resume as usual; a halt that
\ begins inside the cleanup abandons it and lands in the frame as a halt the
\ body carried, keeping [8], so the body's error survives it. A halt taken here
\ is set again and [8] thrown on, even 0; a cleanup's nonzero code supersedes.
: BFINALLY ( -- )
   LBL LBL LBL {: keep:label calm:label throw:label :}
   LBL FINALLY-DONE !
   A G-POP  BCALLABLE
   SP SP 32 SUBI,  9 SP 0 STR,
   BCATCH-HELD
   9 SP 8 STR,
   10 DATA HALT-UNWIND-CELL LDR,  10 SP 16 STR,
   10 0 MOVZ,  10 DATA HALT-UNWIND-CELL STR,
   9 SP 0 LDR,  9 G-PUSH  BCATCH-HELD
   9 keep CBZ,  9 SP 8 STR,                     \ a cleanup's code supersedes
   keep LBL,
   10 DATA HALT-UNWIND-CELL LDR,  10 calm CBZ,
   10 SP 16 STR,                                \ a halt begun in the cleanup
   calm LBL,
   9 SP 8 LDR,  10 SP 16 LDR,  SP SP 32 ADDI,
   10 DATA HALT-UNWIND-CELL STR,                 \ the halt taken, 0 when none
   10 throw CBNZ,
   9 FINALLY-DONE LABEL@ CBZ,
   throw LBL,  9 G-PUSH  BTHROW
   FINALLY-DONE LABEL@ LBL, ;

\ c2-invoke ( body cleanup finish -- ): both calls have independent handlers.
\ The finisher sees the selected code while the body's result row is still live;
\ a nonzero code is rethrown before compiled code reloads that row.
\ Frame: [0] cleanup | [8] finisher | [16] code | [24] halt. A halt is taken and
\ resumed as BFINALLY does, one begun inside the cleanup included, and the
\ finisher runs with the cell clear.
: BC2-INVOKE ( -- )
   LBL LBL {: calm:label throw:label :}
   LBL C2-INVOKE-DONE !
   LBL C2-INVOKE-CLEAN !
   A G-POP  10 A 0 ADDI,
   A G-POP
   SP SP 32 SUBI,  A SP 0 STR,  10 SP 8 STR,
   BCATCH-HELD
   9 SP 16 STR,
   10 DATA HALT-UNWIND-CELL LDR,  10 SP 24 STR,
   10 0 MOVZ,  10 DATA HALT-UNWIND-CELL STR,
   9 SP 0 LDR,  9 G-PUSH  BCATCH-HELD
   9 C2-INVOKE-CLEAN LABEL@ CBZ,
   9 SP 16 STR,
   C2-INVOKE-CLEAN LABEL@ LBL,
   10 DATA HALT-UNWIND-CELL LDR,  10 calm CBZ,
   10 SP 24 STR,                                \ a halt begun in the cleanup
   10 0 MOVZ,  10 DATA HALT-UNWIND-CELL STR,
   calm LBL,
   9 SP 16 LDR,  9 G-PUSH
   9 SP 8 LDR,  9 BLR,
   9 SP 16 LDR,  10 SP 24 LDR,  SP SP 32 ADDI,
   10 DATA HALT-UNWIND-CELL STR,                 \ the halt taken, 0 when none
   10 throw CBNZ,
   9 C2-INVOKE-DONE LABEL@ CBZ,
   throw LBL,  9 G-PUSH  BTHROW
   C2-INVOKE-DONE LABEL@ LBL, ;

\ c2-init-stow sees (R base bound value[W] callback W bytes align frame).
\ The checker records W/bytes/align for the authentic WITH-INIT call. OPEN has
\ already registered PENDING and installed INIT-CLEAR as the frame disposer.
\ Validate before mutation, arm LIVE before the first store, then consume the
\ value bundle into consecutive cells and leave (R base bytes callback).
: BC2-INIT-STOW ( -- )
   LBL LBL LBL {: bad:label copy:label done:label :}
   9 G-POP   10 G-POP   11 G-POP   12 G-POP   13 G-POP
   12 0 CMPI, C-LE bad BCOND,
   11 0 CMPI, C-LE bad BCOND,
   10 8 CMPI, C-NE bad BCOND,
   15 12 3 LSLI,  11 15 CMP, C-NE bad BCOND,
   14 XDS 15 SUB,  14 14 16 SUBI,
   16 14 0 LDR,  17 14 8 LDR,
   16 0 CMPI, C-EQ bad BCOND,
   15 16 7 ANDI,  15 bad CBNZ,
   17 11 CMP, C-LT bad BCOND,
   15 9 0 LDR,  15 1 CMPI, C-NE bad BCOND,
   16 9 16 STR,  11 9 24 STR,  17 9 40 STR,
   15 2 MOVZ,  15 9 0 STR,
   copy LBL,
      14 G-POP
      15 12 1 SUBI,  15 15 3 LSLI,  15 16 15 ADD,
      14 15 0 STR,
      12 12 1 SUBI,  12 copy CBNZ,
   14 G-POP  11 G-PUSH  13 G-PUSH  done B,
   bad LBL,
   9 -6101 LIT64,  9 G-PUSH  BTHROW
   done LBL, ;

\ (R base bound count seed[W] callback W stride align frame) ->
\ (R base count callback). One frame owns and clears count * stride bytes.
: BC2-RECORDS-STOW ( -- )
   LBL LBL LBL LBL LBL LBL LBL
   {: bad:label seed:label first:label copy:label cell-copy:label done:label exit-code:label :}
   9 G-POP  10 G-POP  11 G-POP  12 G-POP  13 G-POP
   12 0 CMPI, C-LE bad BCOND,
   11 0 CMPI, C-LE bad BCOND,
   10 8 CMPI, C-NE bad BCOND,
   16 12 3 LSLI,  11 16 CMP, C-NE bad BCOND,
   14 XDS 11 SUB,  14 14 24 SUBI,
   15 14 0 LDR,  16 14 8 LDR,  17 14 16 LDR,
   15 0 CMPI, C-EQ bad BCOND,
   14 15 7 ANDI,  14 bad CBNZ,
   17 0 CMPI, C-LT bad BCOND,
   16 0 CMPI, C-LT bad BCOND,
   14 16 11 UDIV,  17 14 CMP, C-HI bad BCOND,
   14 9 0 LDR,  14 1 CMPI, C-NE bad BCOND,
   15 9 16 STR,
   14 17 11 MUL,  14 9 24 STR,
   16 9 40 STR,
   14 2 MOVZ,  14 9 0 STR,
   17 first CBNZ,
   seed LBL,
      14 G-POP
      12 12 1 SUBI,  12 seed CBNZ,
      done B,
   first LBL,
      14 G-POP
      16 12 1 SUBI,  16 16 3 LSLI,  16 15 16 ADD,
      14 16 0 STR,
      12 12 1 SUBI,  12 first CBNZ,
   17 17 1 SUBI,  17 done CBZ,
   14 15 11 ADD,
   copy LBL,
      16 15 0 ADDI,
      12 11 3 LSRI,
   cell-copy LBL,
      10 16 0 LDR,  10 14 0 STR,
      16 16 8 ADDI,  14 14 8 ADDI,
      12 12 1 SUBI,  12 cell-copy CBNZ,
      17 17 1 SUBI,  17 copy CBNZ,
   done LBL,
   17 G-POP  16 G-POP
   17 9 24 LDR,  17 17 11 UDIV,
   17 G-PUSH  13 G-PUSH  exit-code B,
   bad LBL,
   9 -6101 LIT64,  9 G-PUSH  BTHROW
   exit-code LBL, ;

: BWORDLIST ( -- )
   OVERLAY-EMIT:GUARD,
   9 DATA WIDN-CELL LDR,  9 G-PUSH  9 9 1 ADDI,  9 DATA WIDN-CELL STR, ;

: BGETCUR ( -- )
   9 DATA CUR-CELL LDR,  9 G-PUSH ;

: BSETCUR ( -- )
   A G-POP  A DATA CUR-CELL STR, ;

\ set-check ( xt -- ): install the checker hook. 0 disables checking (the
\ audited `0 set-check` boundary) and parks an armed hook with its preflight in
\ the SUSP-HOOK pair; arming the parked hook while the preflight cell is empty
\ brings that preflight back, and every arm clears the pair (docs/forth.md
\ "`0 set-check` suspends the armed hook"). A non-zero xt must lie in
\ [DBASE, CP) (x26/x28): every real hook is a source-loaded word compiled
\ there, and a wild install (1, a DATA address, a baked-primitive xt) dies here
\ with a named rc-70 diagnostic instead of at the next publish's BLR. The
\ window cannot tell a word entry from another in-range address.
: BSETCHECK ( -- )
   LBL LBL LBL LBL LBL LBL {: bad:label disarm:label kept:label armed:label done:label msg:label :}
   S\" set-check: invalid checker xt\n" {: ma:ptr mu:n :}
   A G-POP                               \ x9 = candidate xt
   9 disarm CBZ,                         \ 0 -> checking off
      9 DBASE CMP,  C-CC bad BCOND,      \ xt < DBASE (unsigned) -> reject
      9 CP CMP,     C-CS bad BCOND,      \ xt >= CP (unsigned) -> reject
      A DATA HOOK-CELL STR,
      \ The parked hook brings its preflight back; any other arm starts bare.
      11 DATA COMPILE-PREFLIGHT-CELL LDR,  11 armed CBNZ,
      11 DATA SUSP-HOOK-CELL LDR,  9 11 CMP,  C-NE armed BCOND,
      11 DATA SUSP-PREFLIGHT-CELL LDR,  11 DATA COMPILE-PREFLIGHT-CELL STR,
   armed LBL,
      10 0 MOVZ,  10 DATA SUSP-HOOK-CELL STR,  10 DATA SUSP-PREFLIGHT-CELL STR,
      done B,
   disarm LBL,
      10 DATA HOOK-CELL LDR,  10 kept CBZ,   \ already off: the parked pair stands
      10 DATA SUSP-HOOK-CELL STR,
      11 DATA COMPILE-PREFLIGHT-CELL LDR,  11 DATA SUSP-PREFLIGHT-CELL STR,
   kept LBL,
      A DATA HOOK-CELL STR,  A DATA COMPILE-PREFLIGHT-CELL STR,
      done B,
   bad LBL,
      0 2 MOVZ,  1 msg ADR,  2 mu MOVZ,  NR-WRITE SYS,
      0 70 MOVZ,  NR-EXIT-GROUP SYS,
   msg LBL,  ma mu BYTES,
   done LBL,
      \ Each successful install invalidates the observer identity, including
      \ suspension and same-xt rearming. INSTALL pairs hook and completion.
      10 0 MOVZ,
      10 DATA NCOMP-DISPATCH:CALLBACK-ENTRY-CELL CELL + NATIVE-BODY-ABI:ORIGIN-HOOK cells + STR,
      10 DATA NCOMP-DISPATCH:CALLBACK-ENTRY-CELL CELL + NATIVE-BODY-ABI:ORIGIN-TAKE cells + STR, ;

\ discard-check ( -- ): drop the hook `0 set-check` parked, with its preflight,
\ so no later `set-check` resumes it. It clears the SUSP-HOOK pair and nothing
\ else; while a hook is armed the pair is already empty, so it is a no-op then.
\ A driver that rewinds the dictionary below the parked hook's checker and
\ reloads over its bytes (tools/bootstrap.sh emit_boot_hide) discards it here.
: BDISCARDCHECK ( -- )
   10 0 MOVZ,  10 DATA SUSP-HOOK-CELL STR,  10 DATA SUSP-PREFLIGHT-CELL STR, ;

\ executable-build-enter ( -- ) and executable-build-leave ( -- ): the build
\ scope. The outermost enter saves the caller's tier and the matching leave
\ restores it, so a tier selected inside the scope ends with it. The scope
\ never selects a tier itself: what it compiles uses the caller's.
: BBUILDENTER ( -- )
   LBL {: nested:label :}
   9 DATA NCOMP-DISPATCH:BUILD-DEPTH-CELL LDR,  9 nested CBNZ,
   10 DATA NCOMP-DISPATCH:TIER-CELL LDR,
   10 DATA NCOMP-DISPATCH:BUILD-TIER-CELL STR,
   nested LBL,
   9 9 1 ADDI,  9 DATA NCOMP-DISPATCH:BUILD-DEPTH-CELL STR, ;

: BBUILDLEAVE ( -- )
   LBL {: done:label :}
   9 DATA NCOMP-DISPATCH:BUILD-DEPTH-CELL LDR,  9 done CBZ,
   9 9 1 SUBI,  9 DATA NCOMP-DISPATCH:BUILD-DEPTH-CELL STR,
   9 done CBNZ,
   10 DATA NCOMP-DISPATCH:BUILD-TIER-CELL LDR,
   10 DATA NCOMP-DISPATCH:TIER-CELL STR,
   9 DATA NCOMP-DISPATCH:BUILD-TIER-CELL STR,
   done LBL, ;

\ set-tier ( n -- ): select which compiler the `:` handlers dispatch to: 0 =
\ tier 0, the compiler in NCOMP-DISPATCH:COMP-CELL or the JIT on an engine with
\ none; 1 = tier 1, the IR pipeline behind NCOMP-DISPATCH:XT-CELL (the rule is
\ at layout.f NCOMP-DISPATCH:TIER-CELL). A primitive because the cell is in
\ the engine header band and `1 set-tier` must work on a source file's first
\ line, before any of the driver's definitions exist. Anything but 0 or 1
\ dies: a tier is a deliberate choice, not a truthy flag.
: BSETTIER ( -- )
   LBL LBL LBL {: bad:label done:label msg:label :}
   S\" set-tier: tier must be 0 or 1\n" {: ma:ptr mu:n :}
   A G-POP                               \ x9 = requested tier
   9 1 CMPI,  C-HI bad BCOND,            \ unsigned > 1: rejects 2.. and every negative
   A DATA NCOMP-DISPATCH:TIER-CELL STR,
   done B,
   bad LBL,
      0 2 MOVZ,  1 msg ADR,  2 mu MOVZ,  NR-WRITE SYS,
      0 70 MOVZ,  NR-EXIT-GROUP SYS,
   msg LBL,  ma mu BYTES,
   done LBL, ;

\ tier@ ( -- n ): the live tier, for a driver that wants to assert its own
\ selection and for test/tier.f.
: BTIERFETCH ( -- ) 9 DATA NCOMP-DISPATCH:TIER-CELL LDR,  A G-PUSH ;

\ The same readiness supplies the live tick gate and the verifier's entry
\ observation. x11 is the installed query, x12 its owner, both zero if absent.
: C-TICK-QUERY ( -- )
   LBL LBL LBL {: noquery:label query:label done:label :}
   11 0 MOVZ,  12 0 MOVZ,
   \ The cold prefix installs the bridge before its seal watermark exists.
   9 DATA SEAL-NDICT-CELL LDR,  9 noquery CBZ,
   \ An executable build and a replacement target bypass the source query.
   9 DATA NCOMP-DISPATCH:BUILD-DEPTH-CELL LDR,  9 noquery CBNZ,
   13 DATA NCOMP-DISPATCH:TARGET-DECL-CELL LDR,  13 query CBZ,
   12 DATA NCOMP-DISPATCH:DECL-CELL LDR,
   13 12 CMP,  C-NE noquery BCOND,
   query LBL,
   \ This is the installed code identity, independent of later name lookup.
   12 DATA NCOMP-DISPATCH:DECL-CELL LDR,  12 noquery CBZ,
   11 12 NCOMP-DISPATCH:DECL-TRUSTED-TICK-OFF LDR,
   11 noquery CBZ,
   done B,
   noquery LBL,  11 0 MOVZ,  12 0 MOVZ,
   done LBL, ;

\ Load the latch the next `:` takes into register n: 1 for tier 1, 2 for tier
\ 0 through NCOMP-DISPATCH:COMP-CELL, 0 for the JIT. Only register n changes.
: TIER-LATCH ( n -- ) {: r:n :}
   LBL {: done:label :}
   r DATA NCOMP-DISPATCH:TIER-CELL LDR,  r done CBNZ,
   r DATA NCOMP-DISPATCH:COMP-CELL LDR,  r done CBZ,
   r 2 MOVZ,
   done LBL, ;

\ A prospective answer at this boundary: 0 means checking first, 1 means the
\ trusted gate would run first. The verifier captures it before loading tools.
: BTICKORDER ( -- )
   LBL LBL LBL LBL {: next:label checking:label tier:label done:label :}
   9 DATA PEND-CELL LDR,  9 next CBZ,
      9 DATA NCOMP-DISPATCH:DEF-TIER-CELL LDR,
      tier B,
   next LBL,
      9 TIER-LATCH
   tier LBL,
   9 checking CBNZ,
   C-TICK-QUERY
   11 checking CBZ,
   9 1 MOVZ,  9 G-PUSH  12 G-PUSH
   done B,
   checking LBL,
   9 0 MOVZ,  9 G-PUSH  9 G-PUSH
   done LBL, ;

\ set-preflight ( xt -- ): single-assignment installer for the
\ checker-owned immediate preflight. `0 set-check` suspends the paired
\ protected cell with the armed hook and empties it, and `set-check` of that
\ same hook resumes it (BSETCHECK); while it is empty the checker may install
\ one live JIT entry, and an identical reinstall is inert. While checking stays
\ armed, even a TRUSTED: caller cannot replace the hook.
: BSETPREFLIGHT ( -- )
   LBL LBL LBL LBL LBL LBL LBL
   {: bad:label invalid:label empty:label emit:label done:label msg:label invalid-msg:label :}
   S\" set-preflight: invalid or replaced hook\n" {: ma:ptr mu:n :}
   S\" set-preflight: invalid hook\n" {: ia:ptr iu:n :}
   A G-POP
   10 DATA COMPILE-PREFLIGHT-CELL LDR,
   10 empty CBZ,
      10 A CMP,  C-EQ done BCOND,
      bad B,
   empty LBL,
      A invalid CBZ,
      A DBASE CMP,  C-CC invalid BCOND,
      A CP CMP,     C-CS invalid BCOND,
   A DATA COMPILE-PREFLIGHT-CELL STR,
   done B,
   bad LBL,
      1 msg ADR,  2 mu MOVZ,  emit B,
   invalid LBL,
      1 invalid-msg ADR,  2 iu MOVZ,
   emit LBL,
      0 2 MOVZ,  NR-WRITE SYS,
      0 70 MOVZ,  NR-EXIT-GROUP SYS,
   msg LBL,  ma mu BYTES,
   invalid-msg LBL,  ia iu BYTES,
   done LBL, ;

\ set-top-check ( xt -- ): install the top-row token hook (src/core/top-row.f)
\ with BSETCHECK's window: 0 uninstalls; a non-zero xt outside [DBASE, CP)
\ dies with a named rc-70 diagnostic. The cell is a PROT-GUARD band (layout.f
\ TOP-HOOK-CELL), so this direct STR is its only post-seal writer.
: BSETTOPCHECK ( -- )
   LBL LBL LBL LBL {: bad:label ok:label done:label msg:label :}
   S\" set-top-check: invalid top-row hook xt\n" {: ma:ptr mu:n :}
   A G-POP                               \ x9 = candidate xt
   9 ok CBZ,                             \ 0 -> hook off, install as-is
      9 DBASE CMP,  C-CC bad BCOND,      \ xt < DBASE (unsigned) -> reject
      9 CP CMP,     C-CS bad BCOND,      \ xt >= CP (unsigned) -> reject
   ok LBL,
      A DATA TOP-HOOK-CELL STR,
      done B,
   bad LBL,
      0 2 MOVZ,  1 msg ADR,  2 mu MOVZ,  NR-WRITE SYS,
      0 70 MOVZ,  NR-EXIT-GROUP SYS,
   msg LBL,  ma mu BYTES,
   done LBL, ;

package ENGINE-EMIT

\ Read-only engine state needed while the checker is bootstrapping.
: BSEALCAPQ ( -- )
   9 DATA SEAL-NDICT-CELL LDR,  9 0 CMPI,
   9 C-NE CSET,  9 31 9 SUB,  9 G-PUSH ;

\ Capture the seal-time dictionary-truncation watermark. SEAL-CAPTURE tokens
\ call it (the xref.f baseline and the cold prefix's token at the engine-prefix
\ end, habu2.f EMIT-SEAL-CAPTURE-TOKEN), after engine definitions and before
\ any user token. The friend latch is already sealed, so a raw ! would trap;
\ this direct STR from NDICT (x27) is the sanctioned bypass, as in
\ BSETCUR/BSETCHECK. NDICT only grows once the engine is loaded, so a rerun
\ never lowers the watermark. The pre-trust defer table (layout.f PD-*) must be
\ empty by then: checker.f drains it after `: TRUST`, and SEAL-CAPTURE runs
\ after checker.f. A non-empty table means DRAIN-PRETRUST never ran: each
\ undrained defer is named on fd 2 and the exit is 73. Loop state stays off the
\ write-clobbered x0-x2/x9 and re-derives the band base each iteration.
: BSEALCAP ( -- )
   LBL LBL LBL {: pdok pdloop pdexit :}
   9 PD-TABLE-OFF LIT64,  9 DATA 9 ADD,  10 9 0 LDR,  10 pdok CBZ,   \ x10 = pending count; 0 -> drained
      13 0 MOVZ,                                                     \ x13 = index
      pdloop LBL,  13 10 CMP,  C-GE pdexit BCOND,
         9 PD-TABLE-OFF LIT64,  9 DATA 9 ADD,                        \ re-derive &band (writes clobber x9)
         15 PD-SLOT MOVZ,  15 13 15 MUL,  14 9 PD-SLOTS-REL ADDI,  14 14 15 ADD,   \ x14 = slot base
         SP SP 32 SUBI,  9 $72646E75203A6268 LIT64,  9 SP 0 STR,  9 $72702064656E6961 LIT64,  9 SP 8 STR,  9 $2074737572742D65 LIT64,  9 SP 16 STR,  9 $00203A7265666564 LIT64,  9 SP 24 STR,
         0 2 MOVZ,  1 SP 0 ADDI,  2 31 MOVZ,  NR-WRITE SYS,  SP SP 32 ADDI,       \ "hb: undrained pre-trust defer: "
         0 2 MOVZ,  1 14 PD-NAME-OFF ADDI,  2 14 PD-NLEN-OFF LDR,  NR-WRITE SYS,   \ the defer name
         SP SP 16 SUBI,  9 $0A LIT64,  9 SP 0 STR,  0 2 MOVZ,  1 SP 0 ADDI,  2 1 MOVZ,  NR-WRITE SYS,  SP SP 16 ADDI,   \ newline (LOPENNL is habu2.f-only)
         13 13 1 ADDI,  pdloop B,
      pdexit LBL,  0 73 MOVZ,  NR-EXIT-GROUP SYS,
   pdok LBL,
   NDICT DATA SEAL-NDICT-CELL STR, ;

;package

: BSEALFRIEND ( -- )
   9 FRIEND-ARENA-LEN MOVZ,  9 DATA FRIEND-LATCH-CELL STR, ;

\ wide-mark ( -- ): set DNAME-WIDE on the newest record: its effect carries a
\ wider-than-cell layout value, so interpret-mode execute and tick refuse it
\ (habu2.f LWIDE). Like `immediate` (habu2.f C-IMMEDIATE) it stores through the
\ region's RW alias: the record's own address is read-only. The checker calls
\ it at signature-record time.
: BWIDEMARK ( -- )
   9 NDICT 0 ADDI,  9 9 1 SUBI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,   \ x9 = &record[NDICT-1]
   12 DATA REGION-ALIAS:DELTA-CELL LDR,  12 9 12 ADD,   \ x12 = the record through the alias
   10 12 16 LDR,  10 10 DNAME-WIDE ORRI,  10 12 16 STR, ;

\ int-mark ( n -- ): set DNAME-INT on record n, for the seal-time internal-word
\ pass (src/core/internal-mark.f), which marks engine-prefix records with no
\ checker-known effect; interpret-mode execute and tick refuse them (habu2.f
\ LINTERNAL). Stored through the alias as in BWIDEMARK. Monotonic: no clear
\ prim exists. The pass marks its host record last; the primitive registry also
\ marks the newly emitted record so saved images stay sealed.
: BINTMARK ( -- )
   A G-POP
   C DREC MOVZ,  A A C MUL,  A DBASE A ADD,   \ x9 = &record[n]
   12 DATA REGION-ALIAS:DELTA-CELL LDR,  12 A 12 ADD,   \ x12 = the record through the alias
   C 12 16 LDR,  C C DNAME-INT ORRI,  C 12 16 STR, ;

\ rep-mark ( n -- ): set DNAME-REP on dictionary record n, the CAST: record the
\ checker certified as a representation cast (src/core/checker.f
\ CHECKER-DEFCAST). The twin of BINTMARK: the same alias store, no index
\ check, monotonic (no clear prim exists).
: BREPMARK ( -- )
   A G-POP
   C DREC MOVZ,  A A C MUL,  A DBASE A ADD,   \ x9 = &record[n]
   12 DATA REGION-ALIAS:DELTA-CELL LDR,  12 A 12 ADD,   \ x12 = the record through the alias
   C 12 16 LDR,  C C DNAME-REP ORRI,  C 12 16 STR, ;

\ min-in-mark ( rec-idx min -- ): OR the certified minimum input arity into
\ DNAME-MIN-IN (flags bits 52-59) of record rec-idx. The seal-time pass
\ (src/core/internal-mark.f) sets it on every engine-prefix record with a
\ checker-known effect, and EM-INTERPRET-FIND refuses a bare call with fewer
\ interpret-stack cells. Stored through the alias as in BINTMARK; monotonic.
\ The pass marks this prim's own record internal, so user source cannot
\ re-drive it.
: BMININMARK ( -- )
   B G-POP                               \ x10 = min-in cells
   A G-POP                               \ x9  = record index
   C DREC MOVZ,  A A C MUL,  A DBASE A ADD,   \ x9 = &record[n]
   B B $FF ANDI,  B B 52 LSLI,
   12 DATA REGION-ALIAS:DELTA-CELL LDR,  12 A 12 ADD,   \ x12 = the record through the alias
   C 12 16 LDR,  C C B ORR,  C 12 16 STR, ;

package ENGINE-EMIT
public

\ PROT-BITS-ADDR, ( base w a m scratch -- ): emit the bitmap addressing sequence.
\ xbase holds the region base (DATA live, or a snapshot/AOT image's DATA source) and
\ xw the WID; xa comes back holding &word-containing-the-bit and xm that bit's mask.
\ The band is PROT-BITS-BYTES of u64 words, so word index = w>>6 (scaled to a byte
\ offset by <<3) and bit index = w&63. Callers must have proved w < PROT-WID-MAX
\ first; that bound is what keeps the computed address inside the band. xscratch is
\ clobbered; xw is preserved.
\ PROT-BITS-AT, is the same for a bitmap whose base register already points at bit 0
\ -- the AOT seed blob and a snapshot image carry the band that way.
: PROT-BITS-AT, ( n n n n n -- ) {: bits:n w:n a:n m:n scratch:n :}
   scratch w 6 LSRI,  scratch scratch 3 LSLI,
   a bits scratch ADD,
   m w 63 ANDI,
   scratch 1 MOVZ,  m scratch m LSLV, ;

: PROT-BITS-ADDR, ( n n n n n -- ) {: base:n w:n a:n m:n scratch:n :}
   a PROT-BITS-OFF MOVZ,  a base a ADD,
   a w a m scratch PROT-BITS-AT, ;

private

\ prot-wid-add ( wid -- ): mark wid protected; idempotent. A wid at or above
\ PROT-WID-MAX has no bit, so the engine names the bound and exits rather than
\ write outside the band: this range check is the memory-safety argument for a
\ caller-supplied index. Acquire-load, OR, release-store of the containing
\ word; single writer, concurrent adders unsupported.
: BPROTWIDADD ( -- )
   LBL LBL LBL {: ok:label done:label msg:label :}
   S\" hb: protected-WID id above the bound\n" {: ma:ptr mu:n :}
   9 G-POP
   LPROTWIDQ LABEL@ BL,
   13 done CBNZ,
   15 PROT-WID-MAX MOVZ,  9 15 CMP,  C-CC ok BCOND,
      0 2 MOVZ,  1 msg ADR,  2 mu MOVZ,  NR-WRITE SYS,    \ name the exhausted bound on fd 2 before exit 84
      0 ENGINE-ERROR:SEAL-PACKAGE MOVZ,  NR-EXIT-GROUP SYS,
      msg LBL,  ma mu BYTES,                            \ data reached only via ADR
   ok LBL,
   DATA 9 15 14 16 PROT-BITS-ADDR,                   \ x15 = &word, x14 = mask
   16 15 LDAR,
   16 16 14 ORR,
   16 15 STLR,                                       \ release-publish the set bit
   done LBL, ;

\ The design seal's two writers (src/lib/vocabulary.f). The harness
\ admits packages and seals before it loads the design; once POLICY-NDICT-CELL
\ is set either prim writes `hb: policy: sealed` and exits ENGINE-ERROR:POLICY,
\ so nothing the design can reach widens its own vocabulary. Each refusal is one
\ line on fd 2 and that exit, as BPROTWIDADD refuses past its bound.
: POLICY-SEALED$ ( -- ptr u8 n ) S\" hb: policy: sealed\n" ;
: POLICY-NOPKG$ ( -- ptr u8 n ) s" hb: policy: no package " ;
: POLICY-BOUND$ ( -- ptr u8 n ) S\" hb: policy: package wid above the bound\n" ;
: POLICY-NL$ ( -- ptr u8 n ) S\" \n" ;
: POLICY-PKG$ ( -- ptr u8 n ) s" hb: policy: package " ;
: POLICY-KEYWORD$ ( -- ptr u8 n ) s"  publishes keyword " ;

\ What both writers open with: once sealed, name the refusal and exit. Unsealed
\ it falls through, having written x9 only.
: POLICY-OPEN, ( -- )
   LBL LBL {: open:label msg:label :}
   9 DATA POLICY-NDICT-CELL LDR,  9 open CBZ,
      0 2 MOVZ,  1 msg ADR,  2 POLICY-SEALED$ nip MOVZ,  NR-WRITE SYS,
      0 ENGINE-ERROR:POLICY MOVZ,  NR-EXIT-GROUP SYS,
      msg LBL,  POLICY-SEALED$ BYTES,
   open LBL, ;

\ policy-admit ( ptr u8 n -- ): admit the public wordlist of the package the
\ span names. The name resolves as `using` resolves one, to its row in
\ DICT-WL:NAMESPACE, whose cell [0] is the package's public wid. A name no
\ package owns writes `hb: policy: no package <name>`; a wid at or above
\ PROT-WID-MAX has no bit in the band. A package with a public word spelled like
\ a dispatch row (habu2.f LROWWALK) writes `hb: policy: package <name>
\ publishes keyword <word>`: under the seal tier 0 reads that name as the row,
\ which refuses it, and tier 1 as the admitted word, so admitting the package
\ would split the tiers. The bit is published as BPROTWIDADD publishes its own:
\ acquire-load, OR, release-store of the containing word.
: BPOLICYADMIT ( -- )
   LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL
   {: found:label bounded:label nopkg:label nl:label bound:label walk:label inline:label next:label spelled:label pkgmsg:label kwmsg:label walked:label :}
   POLICY-OPEN,
   1 G-POP  0 G-POP                                  \ x1 = name length, x0 = name
   13 0 0 ADDI,                                      \ the name, for the refusals: the lookup leaves x13 and x1 alone
   2 DICT-WL:NAMESPACE LIT64,
   WLFIND:LENTRY LABEL@ BL,                          \ x11 = public wid, x12 = the row or 0
   14 1 0 ADDI,                                      \ the walk and the kernel's writes keep x13/x14
   12 found CBNZ,
      0 2 MOVZ,  1 nopkg ADR,  2 POLICY-NOPKG$ nip MOVZ,  NR-WRITE SYS,
      0 2 MOVZ,  1 13 0 ADDI,  2 14 0 ADDI,  NR-WRITE SYS,
      0 2 MOVZ,  1 nl ADR,  2 POLICY-NL$ nip MOVZ,  NR-WRITE SYS,
      0 ENGINE-ERROR:POLICY MOVZ,  NR-EXIT-GROUP SYS,
   found LBL,
   15 PROT-WID-MAX MOVZ,  11 15 CMP,  C-CC bounded BCOND,
      0 2 MOVZ,  1 bound ADR,  2 POLICY-BOUND$ nip MOVZ,  NR-WRITE SYS,
      0 ENGINE-ERROR:POLICY MOVZ,  NR-EXIT-GROUP SYS,
   nopkg LBL,  POLICY-NOPKG$ BYTES,
   nl LBL,  POLICY-NL$ BYTES,
   bound LBL,  POLICY-BOUND$ BYTES,
   bounded LBL,
   \ Every record of the wordlist: [40] its wid, [16] its name length (below the
   \ DNAME-FLAG-BITS) and DNAME-EXT, [24] the name inline or, with DNAME-EXT, its
   \ address.
   6 DBASE 0 ADDI,  7 NDICT 0 ADDI,
   walk LBL,
      7 walked CBZ,
      8 6 40 LDR,  8 11 CMP,  C-NE next BCOND,
      8 6 16 LDR,  10 8 DNAME-FLAG-BITS LSLI,  10 10 DNAME-FLAG-BITS LSRI,      \ x10 = the name's length
      9 6 24 ADDI,
      8 8 DNAME-EXT ANDI,  8 inline CBZ,  9 6 24 LDR,
      inline LBL,                                    \ x9 = the name
      0 9 0 ADDI,  1 10 0 ADDI,  LROWWALK LABEL@ BL, \ writes x0-x5 only
      0 spelled CBNZ,
   next LBL,
      6 6 DREC ADDI,  7 7 1 SUBI,  walk B,
   spelled LBL,
      SP SP 16 SUBI,  9 SP 0 STR,  10 SP 8 STR,
      0 2 MOVZ,  1 pkgmsg ADR,  2 POLICY-PKG$ nip MOVZ,  NR-WRITE SYS,
      0 2 MOVZ,  1 13 0 ADDI,  2 14 0 ADDI,  NR-WRITE SYS,
      0 2 MOVZ,  1 kwmsg ADR,  2 POLICY-KEYWORD$ nip MOVZ,  NR-WRITE SYS,
      0 2 MOVZ,  1 SP 0 LDR,  2 SP 8 LDR,  NR-WRITE SYS,
      0 2 MOVZ,  1 nl ADR,  2 POLICY-NL$ nip MOVZ,  NR-WRITE SYS,
      0 ENGINE-ERROR:POLICY MOVZ,  NR-EXIT-GROUP SYS,
   pkgmsg LBL,  POLICY-PKG$ BYTES,
   kwmsg LBL,  POLICY-KEYWORD$ BYTES,
   walked LBL,
   14 POLICY-BITS-OFF MOVZ,  14 DATA 14 ADD,
   14 11 15 14 16 PROT-BITS-AT,                      \ x15 = &word, x14 = mask
   16 15 LDAR,
   16 16 14 ORR,
   16 15 STLR, ;

\ policy-seal ( -- ): seal the dictionary as it stands. Every record below the
\ stored NDICT is admitted only through its wordlist's bit; the design's own
\ records land at or above it. NDICT is never 0 in a running engine, so the
\ stored value cannot read as unsealed.
: BPOLICYSEAL ( -- )
   POLICY-OPEN,
   NDICT DATA POLICY-NDICT-CELL STR, ;

;package

\ prot-wid-room ( -- n ): how many more wordlists may still be allocated AND
\ protected. Every WID below PROT-WID-MAX has a bit, so the only thing that can run
\ out is WID ids themselves -- the resource a declaration actually consumes. Clamped
\ at 0 so a caller's `room 0=` and `need room >` tests stay meaningful if WIDN ever
\ passes the bound (unprotected wordlists may be allocated beyond it).
: BPROTWIDROOM ( -- )
   LBL {: pos:label :}
   14 DATA WIDN-CELL LDR,
   9 PROT-WID-MAX MOVZ,  9 9 14 SUB,
   14 0 MOVZ,  9 14 CMP,  C-GT pos BCOND,
      9 0 MOVZ,
   pos LBL,
   9 G-PUSH ;

\ Emit the FNV-1a fold+hash of the name at reg nr (ptr), length lr, into hr;
\ clobbers c3 c4 (byte/fold scratch) and c7 (cursor). The fold is FIND's
\ A-Z|0x20 idiom. This is the hash index's key derivation, shared by the
\ inserter, the duplicate wall, LFIND's two probes and BSWL; it sits here
\ because BSWL is the earliest user.
: C-HIDX-HASH ( n n n n n n -- ) {: nr:n lr:n hr:n c3:n c4:n c7:n :}
   LBL LBL {: hl:label hd:label :}
   hr $CBF29CE484222325 LIT64,
   c7 0 MOVZ,
   hl LBL,  c7 lr CMP,  C-GE hd BCOND,
      c4 nr c7 ADD,  c4 c4 0 LDRB,
      c3 c4 $41 SUBI,  c3 $1A CMPI,  c3 C-CC CSET,  c3 c3 5 LSLI,  c4 c4 c3 ORR,
      hr hr c4 EOR,
      c3 $100000001B3 LIT64,
      hr hr c3 MUL,
      c7 c7 1 ADDI,  hl B,
   hd LBL, ;

\ WLFIND:LENTRY is the name's row in one wordlist and the whole of
\ `search-wl`'s body: in x0 = name, x1 = length, x2 = wid; out x11 = the row's
\ code cell or 0, x12 = the row or 0. The AOT seed's call-site pass shares it
\ and needs the row for the sealed-WID gate.
\ The scan answers the last matching row; the hash probe, insert-once with one
\ slot per (name, wid), answers the first validated row. They agree only where
\ a wid holds one live row per folded name:
\   - a real wordlist (0, a package's private or public wid, DICT-WL:NAMESPACE)
\     is guarded by duplicate walls at its definers (habu2.f C-REJECT-DUP-DEF,
\     DOES-REC:REJECT-DUP). native-unit-publish can still append duplicates;
\     XREF's indexed probe below scans the whole chain for its newest-record
\     rule;
\   - DICT-WL:RETIRED is not a wordlist: xref.f XREF:RETIRE stamps it onto rows
\     still keyed under their published wid, and retiring one name twice puts
\     two rows under the key, so that wid always takes the scan.
\ The scan also answers for no table and for a chain with no empty slot (as
\ LFIND's FIND-START). An empty slot means absent: every record below NDICT is
\ in the table and slots only go empty -> occupied.
package WLFIND
public
: EMIT ( -- )
   LENTRY LABEL@ LBL,
   LBL SWL-LOOP !
   LBL SWL-END !
   LBL SWL-NEXT !
   LBL SWL-CMP !
   LBL SWL-MATCH !
   LBL SWL-F1 !
   LBL SWL-F2 !
   LBL SWL-INL !
   LBL LBL LBL LBL LBL LBL {: plin:label ploop:label pnext:label pinl:label pcmp:label pmatch:label :}
   11 0 MOVZ,                                              \ result defaults to absent
   12 0 MOVZ,                                              \ ... and so does the row: neither loop writes x12
   \ Hash probe. x0 (name), x1 (len), x2 (wid) and x11 (result) are preserved
   \ across it for the scan the two fallbacks reach.
   4 DICT-WL:RETIRED LIT64,  2 4 CMP,  C-EQ plin BCOND,   \ not a wordlist key -> scan
   14 DATA HIDXP-CELL LDR,  14 plin CBZ,                   \ no table -> scan
   0 1 15 4 16 7 C-HIDX-HASH
   6 15 2 EOR,  5 HIDX-SLOTS 1 - LIT64,  6 6 5 AND,                 \ slot = (hash XOR wid) & (HIDX-SLOTS-1)
   8 HIDX-SLOTS LIT64,
   ploop LBL,
      17 6 2 LSLI,  17 14 17 ADD,  3 17 0 LDRW,            \ x3 = slot value (index+1)
      3 SWL-END LABEL@ CBZ,                                \ empty slot -> absent from this wordlist
      4 3 1 SUBI,  4 NDICT CMP,  C-GE pnext BCOND,         \ stale (truncated) index
      5 DREC MOVZ,  5 4 5 MUL,  5 DBASE 5 ADD,             \ x5 = record ptr
      16 5 40 LDR,  16 2 CMP,  C-NE pnext BCOND,           \ wid mismatch (retired / other wordlist)
      16 5 16 LDR,  16 16 DNAME-FLAG-BITS LSLI,  16 16 DNAME-FLAG-BITS LSRI,  16 1 CMP,  C-NE pnext BCOND,  \ name-len mismatch
      16 5 24 ADDI,
      3 5 16 LDR,  3 3 DNAME-EXT ANDI,  3 pinl CBZ,
         16 5 24 LDR,
      pinl LBL,
      7 0 MOVZ,
      pcmp LBL,
         7 1 CMP,  C-GE pmatch BCOND,
         15 16 7 ADD,  15 15 0 LDRB,
         3 15 $41 SUBI,  3 $1A CMPI,  3 C-CC CSET,  3 3 5 LSLI,  15 15 3 ORR,
         4 0 7 ADD,     4 4 0 LDRB,
         3 4 $41 SUBI,   3 $1A CMPI,  3 C-CC CSET,  3 3 5 LSLI,  4 4 3 ORR,
         15 4 CMP,  C-NE pnext BCOND,
         7 7 1 ADDI,  pcmp B,
      pmatch LBL,
         11 5 0 LDR,  12 5 0 ADDI,  SWL-END LABEL@ B,
      pnext LBL,
         8 8 1 SUBI,  8 plin CBZ,
         6 6 1 ADDI,  5 HIDX-SLOTS 1 - LIT64,  6 6 5 AND,  ploop B,
   plin LBL,
   3 $20 MOVZ,  5 DBASE 0 ADDI,  6 NDICT 0 ADDI,
   SWL-LOOP LABEL@ LBL,  6 SWL-END LABEL@ CBZ,
      9 5 40 LDR,  9 2 CMP,  C-NE SWL-NEXT LABEL@ BCOND,
      9 5 16 LDR,  9 9 DNAME-FLAG-BITS LSLI,  9 9 DNAME-FLAG-BITS LSRI,  9 1 CMP,  C-NE SWL-NEXT LABEL@ BCOND,
      16 5 24 ADDI,
      9 5 16 LDR,  9 9 DNAME-EXT ANDI,  9 SWL-INL LABEL@ CBZ,
         16 5 24 LDR,
      SWL-INL LABEL@ LBL,
      7 0 MOVZ,
      SWL-CMP LABEL@ LBL,  7 1 CMP,  C-GE SWL-MATCH LABEL@ BCOND,
         9 16 7 ADD,  9 9 0 LDRB,
         9 $41 CMPI,  C-LT SWL-F1 LABEL@ BCOND,  9 $5A CMPI,  C-GT SWL-F1 LABEL@ BCOND,  9 9 3 ORR,
         SWL-F1 LABEL@ LBL,
         10 0 7 ADD,  10 10 0 LDRB,
         10 $41 CMPI,  C-LT SWL-F2 LABEL@ BCOND,  10 $5A CMPI,  C-GT SWL-F2 LABEL@ BCOND,  10 10 3 ORR,
         SWL-F2 LABEL@ LBL,
         9 10 CMP,  C-NE SWL-NEXT LABEL@ BCOND,
         7 7 1 ADDI,  SWL-CMP LABEL@ B,
      SWL-MATCH LABEL@ LBL,  11 5 0 LDR,  12 5 0 ADDI,  SWL-NEXT LABEL@ B,
      SWL-NEXT LABEL@ LBL,  5 5 DREC ADDI,  6 6 1 SUBI,  SWL-LOOP LABEL@ B,
   SWL-END LABEL@ LBL,  RET,

   WLFIND:LNEWEST LABEL@ LBL,
   LBL LBL LBL LBL LBL LBL
   {: nloop:label nnext:label ninl:label ncmp:label nmatch:label nlinear:label :}
   12 0 MOVZ,
   4 DICT-WL:RETIRED LIT64,  2 4 CMP,  C-EQ nlinear BCOND,
   14 DATA HIDXP-CELL LDR,  14 nlinear CBZ,
   0 1 15 4 16 7 C-HIDX-HASH
   6 15 2 EOR,  5 HIDX-SLOTS 1 - LIT64,  6 6 5 AND,
   8 HIDX-SLOTS LIT64,
   nloop LBL,
      17 6 2 LSLI,  17 14 17 ADD,  3 17 0 LDRW,
      3 SWL-END LABEL@ CBZ,
      4 3 1 SUBI,  4 NDICT CMP,  C-GE nnext BCOND,
      5 DREC MOVZ,  5 4 5 MUL,  5 DBASE 5 ADD,
      16 5 40 LDR,  16 2 CMP,  C-NE nnext BCOND,
      16 5 16 LDR,  16 16 DNAME-FLAG-BITS LSLI,  16 16 DNAME-FLAG-BITS LSRI,
      16 1 CMP,  C-NE nnext BCOND,
      16 5 24 ADDI,
      3 5 16 LDR,  3 3 DNAME-EXT ANDI,  3 ninl CBZ,
         16 5 24 LDR,
      ninl LBL,
      7 0 MOVZ,
      ncmp LBL,
         7 1 CMP,  C-GE nmatch BCOND,
         15 16 7 ADD,  15 15 0 LDRB,
         3 15 $41 SUBI,  3 $1A CMPI,  3 C-CC CSET,  3 3 5 LSLI,  15 15 3 ORR,
         4 0 7 ADD,     4 4 0 LDRB,
         3 4 $41 SUBI,  3 $1A CMPI,  3 C-CC CSET,  3 3 5 LSLI,  4 4 3 ORR,
         15 4 CMP,  C-NE nnext BCOND,
         7 7 1 ADDI,  ncmp B,
      nmatch LBL,
         12 5 CMP,  C-CS nnext BCOND,
         12 5 0 ADDI,
      nnext LBL,
         8 8 1 SUBI,  8 nlinear CBZ,
         6 6 1 ADDI,  5 HIDX-SLOTS 1 - LIT64,  6 6 5 AND,  nloop B,
   nlinear LBL,
      3 $20 MOVZ,  5 DBASE 0 ADDI,  6 NDICT 0 ADDI,
      SWL-LOOP LABEL@ B, ;
;package

\ `search-wl ( ptr u8 n n -- n )`: the routine above, minus one wordlist.
\ Engine-helper WID records and DNAME-INT records are never raw-searchable: the
\ first are private implementation entries, and the second would otherwise let
\ `search-wl execute` bypass the interpreter/tick internal-word gate. The refusal
\ belongs to the primitive, not WLFIND; the AOT relocator uses WLFIND directly.
: BSWL ( -- )
   LBL {: done:label :}
   2 G-POP  1 G-POP  0 G-POP
   11 0 MOVZ,
   2 OWNER-API-PRI-WID CMPI,  C-EQ done BCOND,
   WLFIND:LENTRY LABEL@ BL,
   12 done CBZ,
   9 12 16 LDR,  9 9 DNAME-INT ANDI,  9 done CBZ,
   11 0 MOVZ,
   done LBL,  11 G-PUSH ;

\ xref-search-wl ( ptr u8 n n -- ptr n ): the same lookup's record result.
\ DNAME-INT and a trusted-only checker row restrict it to a trusted boundary;
\ NDICT checks live callee visibility after resolving namespace records.
: BCOMPILERSWL ( -- )
   2 G-POP  1 G-POP  0 G-POP
   WLFIND:LENTRY LABEL@ BL,
   12 G-PUSH ;

: BXREFNEWEST ( -- )
   2 G-POP  1 G-POP  0 G-POP
   WLFIND:LNEWEST LABEL@ BL,
   12 G-PUSH ;

: BPARSE-NAME ( -- )
   LBL PARSE-NONE !
   LBL PARSE-DONE !
   LTOK LABEL@ BL,
   0 PARSE-NONE LABEL@ CBZ,
      9 DATA TKA-CELL LDR,  9 G-PUSH
      9 DATA TKL-CELL LDR,  9 G-PUSH
      PARSE-DONE LABEL@ B,
   PARSE-NONE LABEL@ LBL,
      9 DATA INP-CELL LDR,  9 G-PUSH
      9 0 MOVZ,  9 G-PUSH
   PARSE-DONE LABEL@ LBL, ;

\ num-parse ( ptr u8 n -- n bool bool ): the engine's own number reader, the
\ LNUM routine EM-INTERPRET-NUMBER and EM-COMPILE-LITERAL call, entered with the
\ same registers, so its answer is the cell they push or compile. x11 is the
\ value (a double's bits for a float), x2 whether it read a float, x12 whether
\ it read a number. A refused spelling answers zero (the two ANDs): a syntax
\ miss stops at the first unreadable byte (`12a` leaves 12) and an over-bound
\ literal ends with x12 false.
package ENGINE-EMIT
public

: BNUMPARSE ( -- )
   B G-POP  A G-POP
   2 0 MOVZ,
   LNUM LABEL@ BL,
   12 0 CMPI,  12 C-NE CSET,  12 SP 12 SUB,
   2 0 CMPI,   2 C-NE CSET,   2 SP 2 SUB,
   2 2 12 AND,
   C C 12 AND,
   C G-PUSH  2 G-PUSH  12 G-PUSH ;

;package

: EMIT-ARITH-PRIMS ( -- )
   s" +"    ['] B+    FPRIM-L   s" -"    ['] B-    FPRIM-L   s" *"    ['] B*    FPRIM-L
   s" /"    ['] BDIV  FPRIM-L-DIVISION   s" mod"  ['] BMOD  FPRIM-L   s" /mod" ['] BDIVMOD FPRIM-L
   s" abs"  ['] BABS  FPRIM-L-SCALAR   s" min"  ['] BMIN  FPRIM-L   s" max"  ['] BMAX FPRIM-L ;

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
   s" @"    ['] BFETCH FPRIM-L   s" !"    ['] BSTORE FPRIM   s" ptr-field" ['] BPTRFIELD FPRIM-L
   s" byte-view" ['] BADDRESSVIEW FPRIM-L
   s" cell-view" ['] BADDRESSVIEW FPRIM-L
   s" +!" ['] BPLUSSTORE FPRIM
   s" c@"   ['] BCFETCH FPRIM-L  s" c!"   ['] BCSTORE FPRIM
   s" atomic@" ['] BATFETCH FPRIM-L  s" atomic!" ['] BATSTORE FPRIM
   s" atomic-add" ['] BATADD FPRIM  s" atomic-cas" ['] BATCAS FPRIM  s" fence" ['] BFENCE FPRIM-L
   s" cells" ['] BCELLS FPRIM-L  s" cell+" ['] BCELLPLUS FPRIM-L
   s" chars" ['] BCHARS FPRIM-L  s" char+" ['] BCHARPLUS FPRIM-L  s" count" ['] BCOUNT FPRIM-L ;

: EMIT-OUTPUT-PRIMS ( -- )
   s" ."    ['] BDOT  FPRIM-L   s" .s"   ['] B.S   FPRIM-L   s" depth" ['] BDEPTH FPRIM-L
   s" u."   ['] BU.   FPRIM-L   s" emit" ['] BEMIT FPRIM-L
   s" cr"   ['] BCR   FPRIM-L   s" space" ['] BSPACE FPRIM-L
   s" type" ['] BTYPE  FPRIM-L ;

: EMIT-DICT-PRIMS ( -- )
   s" here" ['] BHERE  FPRIM-L   s" allot" ['] BALLOT FPRIM-L
   s" align" ['] BALIGN FPRIM-L
   s" ,"    ['] BCOMMA FPRIM-L   s" c,"   ['] BCCOMMA FPRIM-L
   s" execute" ['] BEXEC FPRIM
   s" execute-floor" ['] BEXECFLOOR FPRIM
   s" run-in-stack" ['] BRUNSTACK FPRIM
   s" create" ['] BCREATE FPRIM
   s" parse-name" ['] BPARSE-NAME FPRIM
   s" num-parse" ['] ENGINE-EMIT:BNUMPARSE FPRIM
   LBL LEVALBODY !
   s" evaluate" ['] B-EVAL FPRIM-L
   s" evaluate-range" ['] B-EVAL-RANGE FPRIM-L
   s" evaluate-closed" ['] B-EVAL-CLOSED FPRIM
   s" source-unit-run" ['] B-SOURCE-UNIT-RUN ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" source-file-run" ['] B-SOURCE-FILE-RUN ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" source-exec-run" ['] BSOURCEEXECRUN FPRIM
   s" source-exec-live?" ['] BSOURCEEXECLIVE FPRIM
   s" source-exec-boundary?" ['] BSOURCEEXECBOUNDARY FPRIM
   s" source-exec-loader-cut?" ['] BSOURCEEXECLOADERCUT FPRIM
   s" source-exec-position" ['] BSOURCEEXECPOSITION FPRIM
   s" source-exec-handler" ['] BSOURCEEXECHANDLER FPRIM
   EMIT-EVAL-BODY ;

: EMIT-PROCESS-PRIMS ( -- )
   s" run-rc" ['] BRUNRC FPRIM-L
   s" pipe" ['] BPIPE FPRIM-L   s" dup2" ['] BDUP2 FPRIM-L
   s" fcntl" ['] BFCNTL FPRIM-L   s" poll" ['] BPOLL FPRIM
   s" kill" ['] BKILL FPRIM-L
   s" setpgid" ['] BSETPGID FPRIM-L
   s" getpid" ['] BGETPID FPRIM-L
   s" proc-watch-open" ['] BPROCWATCHOPEN FPRIM-L
   s" kill-errno" ['] BKILLERRNO FPRIM-L
   s" execve" ['] BEXECVE FPRIM-L
   s" spawn-io" ['] BSPAWNIO FPRIM-L
   s" spawn-argv-io" ['] BSPAWNARGVIO FPRIM-L
   s" spawn-argv-env-io" ['] BSPAWNARGVENVIO FPRIM-L
   s" spawn-argv-env-cwd-io" ['] BSPAWNARGVENVCWDIO FPRIM-L
   s" spawn-fds" ['] BSPAWNFDS FPRIM-L
   s" fork" ['] LIBC-OS:FORK FPRIM
   s" wait-status" ['] BWAITSTATUS FPRIM-L ;

package ENGINE-EMIT

: EMIT-ENGINE-PRIMS ( -- )
   s" cp@" ['] BCPFETCH FPRIM-L   s" dbase@" ['] BDBASEFETCH FPRIM-L
   s" data-base" ['] BDATAFETCH FPRIM-L
   s" NULL-PTR" ['] BNULLPTR FPRIM-L
   s" native-unit-cut-cell" ['] BNUNITCUTCELL FPRIM-L
   s" native-origin-cell" ['] BNATIVEORIGINCELL FPRIM-L
   s" ndict@" ['] BNDICTFETCH FPRIM-L
   s" cp!" ['] BCPSET FPRIM   s" ndict!" ['] BNDSET FPRIM
   s" seed-ndict!" ['] BSEEDNDICTSET FPRIM
   s" ndict-append" ['] BNDAPPEND ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" def-occ-select" ['] BDEFSELECT ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" def-occ-resolve" ['] BDEFRESOLVE ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" SEAL-CAPTURE" ['] BSEALCAP FPRIM-L
   s" seal-captured?" ['] BSEALCAPQ FPRIM-L
   s" SEAL-FRIEND" ['] BSEALFRIEND FPRIM-L
   s" wide-mark" ['] BWIDEMARK FPRIM
   s" int-mark" ['] BINTMARK ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" rep-mark" ['] BREPMARK ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" min-in-mark" ['] BMININMARK ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" prot-wid-add" ['] BPROTWIDADD FPRIM
   s" prot-wid-room" ['] BPROTWIDROOM FPRIM
   s" policy-admit" ['] BPOLICYADMIT FPRIM
   s" policy-seal" ['] BPOLICYSEAL FPRIM
   s" epoch-seconds" ['] BEPOCHSECONDS FPRIM-L
   s" mono-ns" ['] BMONONS FPRIM-L
   s" die"  ['] BDIE   FPRIM-L ;

;package

: EMIT-FS-PRIMS ( -- )
   s" open" ['] BOPEN FPRIM-L   s" write" ['] BWRITE FPRIM-L   s" read" ['] BREAD FPRIM   s" ioctl" ['] BIOCTL FPRIM
   s" map-anon" ['] BMAPANON FPRIM
   s" mmap" ['] BMMAP FPRIM
   s" munmap" ['] BMUNMAP FPRIM
   s" ffi-call" ['] BFFI-CALL FPRIM
   s" ffi-call-n" ['] BFFI-CALL-N FPRIM
   s" ffi-call-bounded" ['] BFFI-CALL-BOUNDED FPRIM
   s" task-entry" ['] BTASK-ENTRY FPRIM
   LBL LCALLBACKTHUNK !
   LBL LCALLBACKADDR !
   s" callback-entry" ['] BCALLBACK-ENTRY FPRIM
   EMIT-CALLBACK-THUNK
   s" ffi-call-abi-bounded" ['] BFFI-CALL-ABI-BOUNDED FPRIM
   s" ffi-call-abi-r-bounded" ['] BFFI-CALL-ABI-R-BOUNDED FPRIM
   s" ffi-call-struct-bounded" ['] BFFI-CALL-STRUCT-BOUNDED FPRIM
   s" ffi-call-abi" ['] BFFI-CALL-ABI FPRIM
   s" ffi-call-abi-r" ['] BFFI-CALL-ABI-R FPRIM
   s" open-rd" ['] BOPENRD FPRIM-L
   s" access" ['] BACCESS FPRIM-L
   s" unlink" ['] BUNLINK FPRIM-L   s" rename" ['] BRENAME FPRIM-L   s" chmod" ['] BCHMOD FPRIM-L
   s" symlink" ['] BSYMLINK FPRIM-L   s" readlink" ['] BREADLINK FPRIM
   s" realpath" ['] LIBC-OS:REALPATH FPRIM
   s" mkdir" ['] BMKDIR FPRIM-L     s" rmdir" ['] BRMDIR FPRIM-L
   s" stat64" ['] BSTAT64 FPRIM   s" lstat64" ['] BLSTAT64 FPRIM
   s" getdirentries64" ['] BGETDIRENTRIES64 FPRIM
   s" patch32" ['] BPATCH32 FPRIM
   s" reloc-maps-clear" ['] RELOC-EMIT:BCLEAR-MAPS FPRIM-L
   s" code-publish" ['] NPUBWIN:BCODEPUBLISH FPRIM
   s" native-unit-publish" ['] NPUBWIN:BNATIVEUNITPUBLISH ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" callmap-set" ['] NPUBWIN:BCALLMAPSET FPRIM
   s" addrmap-set" ['] NPUBWIN:BADDRMAPSET FPRIM
   s" xref-retarget" ['] NPUBWIN:BXREFRETARGET FPRIM
   s" close" ['] BCLOSE FPRIM-L
   s" close-rc" ['] BCLOSE-RC FPRIM-L
   s" rbase" ['] BRBASE FPRIM-L ;

: EMIT-CHECKER-PRIMS ( -- )
   s" catch" ['] BCATCH FPRIM   s" throw" ['] BTHROWPRIM FPRIM-L
   s" finally" ['] BFINALLY FPRIM
   s" c2-invoke" ['] BC2-INVOKE FPRIM
   s" c2-init-stow" ['] BC2-INIT-STOW FPRIM
   s" c2-records-stow" ['] BC2-RECORDS-STOW FPRIM
   s" unit-compile-run" ['] BUNITCOMPILE ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" wordlist" ['] BWORDLIST FPRIM-L   s" get-current" ['] BGETCUR FPRIM-L
   s" set-current" ['] BSETCUR FPRIM-L  s" search-wl" ['] BSWL FPRIM
   s" xref-search-wl" ['] BCOMPILERSWL ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" xref-wl-newest" ['] BXREFNEWEST ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" set-check" ['] BSETCHECK FPRIM-L   s" check@" ['] BCHECKFETCH FPRIM-L
   s" discard-check" ['] BDISCARDCHECK FPRIM-L
   s" set-tier" ['] BSETTIER FPRIM     s" tier@" ['] BTIERFETCH FPRIM-L
   s" tick-order@" ['] BTICKORDER FPRIM-L
   s" executable-build-enter" ['] BBUILDENTER ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" executable-build-leave" ['] BBUILDLEAVE ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" set-preflight" ['] BSETPREFLIGHT FPRIM-L
   s" set-top-check" ['] BSETTOPCHECK FPRIM-L   s" top-check@" ['] BTOPCHECKFETCH FPRIM-L ;

package ENGINE-EMIT


: EMIT-PRIMS ( -- )
   EMIT-ARITH-PRIMS  EMIT-COMPARE-PRIMS  EMIT-STACK-PRIMS
   EMIT-MEMORY-PRIMS  EMIT-OUTPUT-PRIMS  EMIT-DICT-PRIMS
   EMIT-PROCESS-PRIMS  EMIT-ENGINE-PRIMS  EMIT-FS-PRIMS
   EMIT-CHECKER-PRIMS ;

;package

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
   FP-COND !
   B G-POP  A G-POP  0 A FMOVXD,  1 B FMOVXD,  0 1 FCMP,
   A FP-COND @ CSET,  A SP A SUB,  A G-PUSH ;

: BF< ( -- )
   C-MI (FCMP) ;

: BF> ( -- )
   C-GT (FCMP) ;

: BF= ( -- )
   C-EQ (FCMP) ;

: (FCMP0) ( n -- )
   FP-COND !  A G-POP  0 A FMOVXD,  0 FCMP0,
   A FP-COND @ CSET,  A SP A SUB,  A G-PUSH ;

: BF0< ( -- )
   C-MI (FCMP0) ;

: BF0= ( -- )
   C-EQ (FCMP0) ;

: BS>F ( -- )
   A G-POP  0 A SCVTF,   A 0 FMOVDX,  A G-PUSH ;

: BF>S ( -- )
   A G-POP  0 A FMOVXD,  A 0 FCVTZS,  A G-PUSH ;

: BFDOT ( -- )
   LBL FD-FRAC !
   LBL FD-INT !
   LBL FD-SIGN !
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
   FD-FRAC LABEL@ LBL,                                 \ six zero-padded frac digits
     11 14 10 SDIV,  13 11 10 MUL,  13 14 13 SUB,
     13 13 48 ADDI,  12 12 1 SUBI,  13 12 0 STRB,
     14 11 0 ADDI,  5 5 1 SUBI,  5 FD-FRAC LABEL@ CBNZ,
   13 46 MOVZ,  12 12 1 SUBI,  13 12 0 STRB,           \ '.'
   FD-INT LABEL@ LBL,                                  \ int digits (do-while)
     11 9 10 SDIV,  13 11 10 MUL,  13 9 13 SUB,
     13 13 48 ADDI,  12 12 1 SUBI,  13 12 0 STRB,
     9 11 0 ADDI,  9 FD-INT LABEL@ CBNZ,
   15 15 63 LSRI,  15 FD-SIGN LABEL@ CBZ,
     13 45 MOVZ,  12 12 1 SUBI,  13 12 0 STRB,         \ '-'
   FD-SIGN LABEL@ LBL,
   1 12 0 ADDI,  2 SP 48 ADDI,  2 2 12 SUB,
   G-OUT
   SP SP 48 ADDI, ;

: EMIT-FP-PRIMS ( -- )
   s" f+" ['] BF+ FPRIM-L   s" f-" ['] BF- FPRIM-L   s" f*" ['] BF* FPRIM-L
   s" f/" ['] BF/ FPRIM-L   s" fnegate" ['] BFNEG FPRIM-L
   s" fabs" ['] BFABS FPRIM-L  s" fsqrt" ['] BFSQRT FPRIM-L
   s" f<" ['] BF< FPRIM-L   s" f>" ['] BF> FPRIM-L   s" f=" ['] BF= FPRIM-L
   s" f0<" ['] BF0< FPRIM-L  s" f0=" ['] BF0= FPRIM-L
   s" s>f" ['] BS>F FPRIM-L  s" f>s" ['] BF>S FPRIM-L
   s" f." ['] BFDOT FPRIM-L ;

\ LCEMIT ( x9 = instruction word ): the one store every emission funnels
\ through. It guards the word at CP and stores it through the region's RW
\ alias, so the code at CP's own address stays executable and never writable.
: EMIT-CEMIT ( -- )
   LCEMIT LABEL@ LBL,
   SP SP 16 SUBI,  12 SP 0 STR,  13 SP 8 STR,
   28 GUARD-CODE-WORD
   12 DATA REGION-ALIAS:DELTA-CELL LDR,  12 28 12 ADD,  9 12 0 STRW,   \ through the alias
   12 SP 0 LDR,  13 SP 8 LDR,  SP SP 16 ADDI,
   28 28 4 ADDI,  RET, ;

\ LBCAP ( -- ): append TKA/TKL + ' ' to the body capture; LBCS ( x11=a x12=u )
\ is the general entry (defining-word kind tokens). Overflow is fatal, exit 71:
\ truncation would let the check hook certify code it never saw. The overflow
\ arm hands x16 (the size needed) to habu2.f LBCAPFULL, which names the
\ ceiling, the definition and that count.
: EMIT-BCAP ( -- )
   LBCAP LABEL@ LBL,
   11 DATA TKA-CELL LDR,  12 DATA TKL-CELL LDR,
   LBCS LABEL@ LBL,
   LBL BCAP-OK !
   LBL BCAP-CP !
   LBL BCAP-CD !
   LBL BCAP-GO !
   \ pass 2 re-runs the already-captured body: never re-capture
   14 DATA P2-CELL LDR,  14 BCAP-GO LABEL@ CBZ,  RET,
   BCAP-GO LABEL@ LBL,
   17 12 0 ADDI,                  \ len in x17 (IP1): callers keep state in x5-x8
   14 DATA BODYLEN-CELL LDR,
   16 14 17 ADD,  16 16 1 ADDI,
   5 BODYBUF-CAP MOVZ,  16 5 CMP,  C-LE BCAP-OK LABEL@ BCOND,
      LBCAPFULL LABEL@ B,            \ x16 = the size the capture needs: habu2.f EM-BODY-CAP-DIE names buffer, ceiling, definition and count, then rc 71
   BCAP-OK LABEL@ LBL,
   15 DATA BODYBUF-OFF ADDI,  15 15 14 ADD,
   BCAP-CP LABEL@ LBL,  12 BCAP-CD LABEL@ CBZ,  13 11 0 LDRB,  13 15 0 STRB,
      15 15 1 ADDI,  11 11 1 ADDI,  12 12 1 SUBI,  BCAP-CP LABEL@ B,
   BCAP-CD LABEL@ LBL,  13 32 MOVZ,  13 15 0 STRB,
   14 14 17 ADD,  14 14 1 ADDI,  14 DATA BODYLEN-CELL STR,
   RET, ;

: EMIT-TOK ( -- )
   LTOK LABEL@ LBL,
   LBL TOK-SKIP !
   LBL TOK-HAS !
   LBL TOK-SCAN !
   LBL TOK-GOT !
   LBL TOK-NONE !
   11 DATA INP-CELL LDR,  12 DATA INE-CELL LDR,
   TOK-SKIP LABEL@ LBL,
      11 12 CMP,  C-GE TOK-NONE LABEL@ BCOND,
      9 11 0 LDRB,  9 32 CMPI,  C-HI TOK-HAS LABEL@ BCOND,
      11 11 1 ADDI,  TOK-SKIP LABEL@ B,
   TOK-HAS LABEL@ LBL,  11 DATA TKA-CELL STR,
   TOK-SCAN LABEL@ LBL,
      11 12 CMP,  C-GE TOK-GOT LABEL@ BCOND,
      9 11 0 LDRB,  9 32 CMPI,  C-LS TOK-GOT LABEL@ BCOND,
      11 11 1 ADDI,  TOK-SCAN LABEL@ B,
   TOK-GOT LABEL@ LBL,  9 DATA TKA-CELL LDR,  9 11 9 SUB,  9 DATA TKL-CELL STR,
      11 DATA INP-CELL STR,  0 1 MOVZ,  RET,
   TOK-NONE LABEL@ LBL,  11 DATA INP-CELL STR,  0 0 MOVZ,  RET, ;

\ The shared runtime lowering guard body ( x10=address, x11=byte length ). It is
\ registered as the sealed (PROT-SPAN) engine helper so a guarded primitive that
\ reaches it by a direct branch is carried into an ahead-of-time image and
\ relocated. The label is reserved in the habu2.f LABELS allocator (its CORE
\ block) so the record spans exactly [start, end).
package ENGINE-EMIT

: EMIT-PROT-SPAN ( -- )
   LPROTSPAN LABEL@ {: start:label :}
   LBL {: end:label :}
   s" (PROT-SPAN)" start LABEL>N end LABEL>N ENGINE-PRIMS:HELPER-REGISTER
   start LBL,
   10 11 GUARD-SPAN
   RET,
   end LBL, ;

\ The dividing primitives' refusal ( no arguments; never returns ). `/`, `mod`
\ and `/mod` each test their divisor and branch here when it is zero, so the
\ throw exists ONCE and their bodies grow by nothing: the guard is the same two
\ instructions the trapping version had. Registered as the sealed (DIV-ZERO)
\ engine helper for the same reason (PROT-SPAN) is -- a direct branch out of a
\ primitive has to be a record the ahead-of-time closure walker can follow.
\ It inlines BTHROW, which is why it sits here and not beside the bodies.
: EMIT-DIV-ZERO ( -- )
   LDIVZERO LABEL@ {: start:label :}
   LBL {: end:label :}
   s" (DIV-ZERO)" start LABEL>N end LABEL>N ENGINE-PRIMS:HELPER-REGISTER
   start LBL,
   9 ARITH-ABI:E-DIV-ZERO LIT64,  9 G-PUSH  BTHROW
   end LBL,
   DIVISION-PRIM-ROW @ dup 0<> if 1- ENGINE-PRIMS:MARK-DIVISION else drop then ;

\ The shared bodies this file emits beside the span guard: (PROT-SPAN), then the
\ dividing primitives' (DIV-ZERO).
: EMIT-PROT ( -- )
   EMIT-PROT-SPAN
   EMIT-DIV-ZERO ;

;package

\ Protected-WID membership, BL routine: x9 = wid, x13 = 1 when protected else 0.
\ The OWNER-API wordlists (public=1, private=2; OWNER-API-PRI-WID marks the AOT
\ direct-branch helpers) are always protected, pinned here because three paths
\ populate the protected-WID table (cold init, AOT-REPL restore, snapshot
\ restore). Dynamic wordlists start at FIRST-DYNAMIC-WID (3). Any other WID is one
\ bit in the PROT-BITS-OFF bitmap; one at or above PROT-WID-MAX answers "not
\ protected", sound because prot-wid-add refuses to protect it. Preserves x5 x6 x7
\ x9 x14.
package ENGINE-EMIT

: EMIT-PROTWID ( -- )
   LBL LBL {: qdone:label qhit:label :}
   LPROTWIDQ LABEL@ LBL,
   SP SP 32 SUBI,
   5 SP 0 STR,  6 SP 8 STR,  7 SP 16 STR,  14 SP 24 STR,
   13 0 MOVZ,                                   \ result = 0 (not protected)
   9 OWNER-API-PUB-WID CMPI,  C-EQ qhit BCOND,  \ engine-reserved public API wordlist
   9 OWNER-API-PRI-WID CMPI,  C-EQ qhit BCOND,  \ engine-reserved private wordlist (helper marker)
   5 PROT-WID-MAX MOVZ,  9 5 CMP,  C-CS qdone BCOND,   \ above the bound: no bit, never protected
   DATA 9 5 14 6 PROT-BITS-ADDR,                \ x5 = &word, x14 = mask
   7 5 LDAR,                                    \ acquire-load the containing word
   7 7 14 AND,
   7 qdone CBZ,
   qhit LBL,  13 1 MOVZ,                        \ reserved pin or set bit -> protected
   qdone LBL,
   5 SP 0 LDR,  6 SP 8 LDR,  7 SP 16 LDR,  14 SP 24 LDR,
   SP SP 32 ADDI,  RET, ;

;package

\ LFLUSH makes the code in [x9, CP) visible to instruction fetch; LFLUSHTO takes
\ the end in x11 instead, for code that stands above CP (LIBC-OS:FORK's child).
\ Clobbers x9 (rounded down to its line), x10, x11.
\ LFLUSHTO is registered as the sealed (FLUSHTO) engine helper for the reason
\ (DIV-ZERO) is one: the fork primitive's Linux child branches to it directly,
\ and a direct branch out of a primitive has to be a record the ahead-of-time
\ closure walker can follow (aot-closure.f FINDADDR-PTR). LFLUSH needs none:
\ every primitive that reaches it writes code, which a stripped image cannot.
: EMIT-FLUSH ( -- )
   LFLUSHTO LABEL@ {: start:label :}
   LBL {: end:label :}
   s" (FLUSHTO)" start LABEL>N end LABEL>N ENGINE-PRIMS:HELPER-REGISTER
   LFLUSH LABEL@ LBL,
   11 CP 0 ADDI,
   start LBL,
   LBL FL-DL !
   LBL FL-DD !
   LBL FL-IL !
   LBL FL-ID !
   9 9 6 LSRI,  9 9 6 LSLI,                                 \ align start down to the
   10 9 0 ADDI,                                             \ line, or the 64-byte
                                                            \ stride skips the last one
   FL-DL LABEL@ LBL,  10 11 CMP,  C-GE FL-DD LABEL@ BCOND,  10 DCCVAU,  10 10 64 ADDI,  FL-DL LABEL@ B,
   FL-DD LABEL@ LBL,  DSB-ISH,
   10 9 0 ADDI,
   FL-IL LABEL@ LBL,  10 11 CMP,  C-GE FL-ID LABEL@ BCOND,  10 ICIVAU,  10 10 64 ADDI,  FL-IL LABEL@ B,
   FL-ID LABEL@ LBL,  DSB-ISH,  ISB,  RET,
   end LBL, ;

variable LHIDXBUILD

\ Emit: the chain of record x3's key, its folded name XOR its wid cell: x6 =
\ the chain's first slot, x8 = HIDX-SLOTS, the most slots one walk visits.
\ Clobbers x2 x4 x5 x7 x15 x16.
: C-HIDX-CHAIN ( -- )
   LBL {: rinl:label :}
   5 DREC MOVZ,  5 3 5 MUL,  5 DBASE 5 ADD,
   2 5 40 LDR,
   16 5 24 ADDI,
   15 5 16 LDR,  15 15 DNAME-FLAG-BITS LSLI,  15 15 DNAME-FLAG-BITS LSRI,
   4 5 16 LDR,  4 4 DNAME-EXT ANDI,  4 rinl CBZ,
      16 5 24 LDR,
   rinl LBL,
   16 15 6 4 5 7 C-HIDX-HASH
   6 6 2 EOR,  5 HIDX-SLOTS 1 - LIT64,  6 6 5 AND,
   8 HIDX-SLOTS LIT64, ;

\ Emit: insert record index x3 into table x14, storing index+1 in the first empty
\ or stale slot of its chain. Insert-once: the dictionary rejects duplicate
\ definitions. Only claiming an EMPTY slot raises HIDX:CLAIMS. LHIDXADD compacts
\ at HIDX:LOAD-MAX, so the exhausted-wrap exit is unreachable and dies loudly
\ (HIDX-EMIT:LFULL).
\ Clobbers x2 x4 x5 x6 x7 x8 x15 x16 x17.
: C-HIDX-INS ( -- )
   LBL LBL LBL LBL {: iloop:label inext:label iempty:label iput:label :}
   C-HIDX-CHAIN
   iloop LBL,
      17 6 2 LSLI,  17 14 17 ADD,  4 17 0 LDRW,
      4 iempty CBZ,
      4 4 1 SUBI,  4 NDICT CMP,  C-GE iput BCOND,
   inext LBL,
      8 8 1 SUBI,  8 HIDX-EMIT:LFULL LABEL@ CBZ,
      6 6 1 ADDI,  5 HIDX-SLOTS 1 - LIT64,  6 6 5 AND,  iloop B,
   iempty LBL,
      4 DATA HIDX:CLAIMS LDR,  4 4 1 ADDI,  4 DATA HIDX:CLAIMS STR,
   iput LBL,
      4 3 1 ADDI,  4 17 0 STRW, ;

\ Emit: branch to `held` when record x3's chain in table x14 holds x3 before
\ its first empty slot, the stretch every probe for the record's key walks;
\ else fall through. Clobbers x2 x4 x5 x6 x7 x8 x15 x16 x17.
: C-HIDX-HELD? ( label -- ) {: held:label :}
   LBL LBL {: hloop:label hnone:label :}
   C-HIDX-CHAIN
   hloop LBL,
      17 6 2 LSLI,  17 14 17 ADD,  4 17 0 LDRW,
      4 hnone CBZ,
      4 4 1 SUBI,  4 3 CMP,  C-EQ held BCOND,
      8 8 1 SUBI,  8 hnone CBZ,
      6 6 1 ADDI,  5 HIDX-SLOTS 1 - LIT64,  6 6 5 AND,  hloop B,
   hnone LBL, ;

\ C-HIDX-DUP?: x14 = live table ptr (caller ensures != 0). Sets x13 = 1 when a
\ live record with this definition's wordlist (DEF-WL-CELL) and folded name
\ (TKA/TKL) is already in the hash table, else 0, then falls through. The
\ ordinary definition refuses any live match in the chain; retired records
\ carry wid -2 and are skipped by the wid check. Same fold/compare as the
\ linear C-REJECT-DUP-DEF, so the two agree on every candidate.
: C-HIDX-DUP? ( -- )
   LBL LBL LBL LBL LBL LBL {: dloop:label dnext:label dinl:label dcmp:label dfound:label dret:label :}
   16 DATA TKA-CELL LDR,  15 DATA TKL-CELL LDR,
   16 15 3 4 5 7 C-HIDX-HASH
   4 DATA DEF-WL-CELL LDR,  6 3 4 EOR,  5 HIDX-SLOTS 1 - LIT64,  6 6 5 AND,
   13 0 MOVZ,
   8 HIDX-SLOTS LIT64,
   dloop LBL,
      4 6 2 LSLI,  4 14 4 ADD,  3 4 0 LDRW,                  \ x3 = slot value
      3 dret CBZ,                                            \ empty slot -> no dup
      4 3 1 SUBI,  4 NDICT CMP,  C-GE dnext BCOND,           \ stale index
      5 DREC MOVZ,  5 4 5 MUL,  5 DBASE 5 ADD,               \ x5 = record ptr
      4 5 40 LDR,  15 DATA DEF-WL-CELL LDR,  4 15 CMP,  C-NE dnext BCOND,          \ wid mismatch
      4 5 16 LDR,  4 4 DNAME-FLAG-BITS LSLI,  4 4 DNAME-FLAG-BITS LSRI,  15 DATA TKL-CELL LDR,  4 15 CMP,  C-NE dnext BCOND,  \ len mismatch
      16 5 24 ADDI,
      4 5 16 LDR,  4 4 DNAME-EXT ANDI,  4 dinl CBZ,
         16 5 24 LDR,
      dinl LBL,
      7 0 MOVZ,
      dcmp LBL,
         15 DATA TKL-CELL LDR,  7 15 CMP,  C-GE dfound BCOND,
         15 16 7 ADD,  15 15 0 LDRB,
         3 15 $41 SUBI,  3 $1A CMPI,  3 C-CC CSET,  3 3 5 LSLI,  15 15 3 ORR,
         4 DATA TKA-CELL LDR,  4 4 7 ADD,  4 4 0 LDRB,
         3 4 $41 SUBI,   3 $1A CMPI,  3 C-CC CSET,  3 3 5 LSLI,  4 4 3 ORR,
         15 4 CMP,  C-NE dnext BCOND,
         7 7 1 ADDI,  dcmp B,
      dnext LBL,
         8 8 1 SUBI,  8 dret CBZ,
         6 6 1 ADDI,  5 HIDX-SLOTS 1 - LIT64,  6 6 5 AND,  dloop B,
   dfound LBL,  13 1 MOVZ,
   dret LBL, ;

\ The frame LHIDXADD and HIDX-EMIT:LREKEY open: x30 and every register their
\ walks, insert and load bound clobber. EMIT-HIDX's aret closes it.
: HIDX-SAVE, ( -- )
   SP SP 96 SUBI,
   30 SP 0 STR,  2 SP 8 STR,  3 SP 16 STR,  4 SP 24 STR,  5 SP 32 STR,
   6 SP 40 STR,  7 SP 48 STR,  14 SP 56 STR,  15 SP 64 STR,  16 SP 72 STR,  17 SP 80 STR,
   8 SP 88 STR, ;

\ LHIDXADD inserts record NDICT-1 mid-publish, saving its whole clobber set, and
\ compacts the table when HIDX:CLAIMS crosses HIDX:LOAD-MAX, sweeping the stale
\ slots rollback leaves. LHIDXBUILD maps a zeroed table; a failed mmap exits,
\ never a degraded mode. HIDX-EMIT:LREBUILD zeroes the table and HIDX:CLAIMS and
\ re-inserts [0,NDICT): the only operation that lowers HIDX:CLAIMS, called by the
\ raise leg of ndict! (BNDSET). HIDX-EMIT:LFULL is the loud exit for every
\ unreachable state.
\ HIDX-EMIT:LREKEY (x3 = record index) re-indexes a record under its current wid
\ cell unless its chain already holds it, register-transparent. habu2.f
\ DEFWRITE:RECORD-WID calls it after rewriting a published record's wid, because a
\ rebuild keys a record retired across it on RETIRED's chain. x86-64 refuses
\ record-wid! (kernel-x64.f), so it has no twin.
: EMIT-HIDX ( -- )
   LBL LBL LBL LBL LBL LBL LBL LBL LBL
   {: aret:label bfail:label msg:label fmsg:label floop:label fdone:label
      rret:label zloop:label zdone:label :}
   LBL LBL {: bhave:label ains:label :}
   S\" hb: dictionary index alloc failed\n" {: ma:ptr mu:n :}
   S\" hb: dictionary index exhausted\n" {: fa:ptr fu:n :}
   LHIDXADD LABEL@ LBL,
      HIDX-SAVE,
      14 DATA HIDXP-CELL LDR,  14 aret CBZ,
      3 NDICT 0 ADDI,  3 3 1 SUBI,
      ains LBL,
      C-HIDX-INS
      \ the load bound: compact before chains can approach a full wrap
      4 DATA HIDX:CLAIMS LDR,  5 HIDX:LOAD-MAX LIT64,  4 5 CMP,  C-LT aret BCOND,
      HIDX-EMIT:LREBUILD LABEL@ BL,
      aret LBL,
      30 SP 0 LDR,  2 SP 8 LDR,  3 SP 16 LDR,  4 SP 24 LDR,  5 SP 32 LDR,
      6 SP 40 LDR,  7 SP 48 LDR,  14 SP 56 LDR,  15 SP 64 LDR,  16 SP 72 LDR,  17 SP 80 LDR,
      8 SP 88 LDR,
      SP SP 96 ADDI,  RET,
   HIDX-EMIT:LREKEY LABEL@ LBL,
      HIDX-SAVE,
      14 DATA HIDXP-CELL LDR,  14 aret CBZ,
      aret C-HIDX-HELD?
      ains B,
   LHIDXBUILD LABEL@ LBL,
      \ Startup BLs here between source setup and the interpret loop: save all
      \ it and the mmap syscall touch. A seeded start builds before the seed
      \ registers and the runtime-state pass reaches here again: an existing
      \ table is refilled in place, never mapped twice.
      SP SP 160 SUBI,
      30 SP 0 STR,   0 SP 8 STR,   1 SP 16 STR,  2 SP 24 STR,  3 SP 32 STR,
      4 SP 40 STR,   5 SP 48 STR,  6 SP 56 STR,  7 SP 64 STR,  8 SP 72 STR,
      13 SP 80 STR,  14 SP 88 STR, 15 SP 96 STR, 16 SP 104 STR, 17 SP 112 STR,
      14 DATA HIDXP-CELL LDR,  14 bhave CBNZ,
      0 0 MOVZ,  1 HIDX-BYTES LIT64,  2 3 MOVZ,
      3 MAP-ANON-PRIVATE LIT64,  4 0 MOVN,  5 0 MOVZ,  NR-MMAP SYS,
      4 C-CS CSET,  4 bfail CBNZ,
      14 0 0 ADDI,  14 DATA HIDXP-CELL STR,
      4 0 MOVZ,  4 DATA HIDX:CLAIMS STR,             \ fresh pages hold no claims
      bhave LBL,
      HIDX-EMIT:LREBUILD LABEL@ BL,                  \ zero + fill [0,NDICT)
      30 SP 0 LDR,   0 SP 8 LDR,   1 SP 16 LDR,  2 SP 24 LDR,  3 SP 32 LDR,
      4 SP 40 LDR,   5 SP 48 LDR,  6 SP 56 LDR,  7 SP 64 LDR,  8 SP 72 LDR,
      13 SP 80 LDR,  14 SP 88 LDR, 15 SP 96 LDR, 16 SP 104 LDR, 17 SP 112 LDR,
      SP SP 160 ADDI,  RET,
      bfail LBL,                                     \ dict hash-index mmap failed: label fd 2 before exit 74
         0 2 MOVZ,  1 msg ADR,  2 mu MOVZ,  NR-WRITE SYS,
         0 74 MOVZ,  NR-EXIT-GROUP SYS,
      msg LBL,  ma mu BYTES,
   HIDX-EMIT:LREBUILD LABEL@ LBL,
      \ register-transparent like LHIDXBUILD: BNDSET and LHIDXADD call it
      \ mid-primitive with caller state live.
      SP SP 160 SUBI,
      30 SP 0 STR,   2 SP 8 STR,   3 SP 16 STR,  4 SP 24 STR,  5 SP 32 STR,
      6 SP 40 STR,   7 SP 48 STR,  8 SP 56 STR,  13 SP 64 STR, 14 SP 72 STR,
      15 SP 80 STR,  16 SP 88 STR, 17 SP 96 STR,
      14 DATA HIDXP-CELL LDR,  14 rret CBZ,          \ pre-build window: nothing to compact
      \ the dictionary must fit under the load bound or the compaction could
      \ not restore it - impossible while DICT-CAP < HIDX:LOAD-MAX, loud if not
      5 HIDX:LOAD-MAX LIT64,  NDICT 5 CMP,  C-GE HIDX-EMIT:LFULL LABEL@ BCOND,
      13 0 MOVZ,  4 0 MOVZ,  15 HIDX-BYTES LIT64,
      zloop LBL,
         13 15 CMP,  C-GE zdone BCOND,
         17 14 13 ADD,  4 17 0 STR,
         13 13 8 ADDI,  zloop B,
      zdone LBL,
      4 DATA HIDX:CLAIMS STR,                         \ zero claims (x4 = 0)
      13 0 MOVZ,
      floop LBL,  13 NDICT CMP,  C-GE fdone BCOND,
         3 13 0 ADDI,  C-HIDX-INS
         13 13 1 ADDI,  floop B,
      fdone LBL,
      rret LBL,
      30 SP 0 LDR,   2 SP 8 LDR,   3 SP 16 LDR,  4 SP 24 LDR,  5 SP 32 LDR,
      6 SP 40 LDR,   7 SP 48 LDR,  8 SP 56 LDR,  13 SP 64 LDR, 14 SP 72 LDR,
      15 SP 80 LDR,  16 SP 88 LDR, 17 SP 96 LDR,
      SP SP 160 ADDI,  RET,
   HIDX-EMIT:LFULL LABEL@ LBL,                        \ cannot maintain the index: loud, never a quiet downgrade
      0 2 MOVZ,  1 fmsg ADR,  2 fu MOVZ,  NR-WRITE SYS,
      0 74 MOVZ,  NR-EXIT-GROUP SYS,
   fmsg LBL,  fa fu BYTES, ;

\ A dictionary slot's occurrence is owned by one anonymous mapping. The first
\ cell is the last issued number; the remaining DICT-CAP cells are indexed by
\ slot. None of these cells are in the rewindable DATA heap or captured image.
: DEF-OCC-EXHAUSTED, ( -- )
   9 EM-DEF-OCC:E-EXHAUSTED LIT64,  9 G-PUSH  BTHROW ;

\ x9 = number about to be issued. Preserve every touched caller register.
: EMIT-DEF-OCC ( -- )
   LBL LBL LBL LBL LBL LBL LBL LBL
   {: room-ok:label lower:label raise:label loop:label raised:label
      done:label clear-loop:label reset:label :}
   LBL LBL LBL LBL
   {: reset-loop:label init-bad:label init-msg:label append-done:label :}
   S\" hb: definition occurrence alloc failed\n" {: ma:ptr mu:n :}
   EM-DEF-OCC:LROOM LABEL@ LBL,
      SP SP 32 SUBI,  9 SP 0 STR,  10 SP 8 STR,  11 SP 16 STR,  12 SP 24 STR,
      10 DATA EM-DEF-OCC:PTR-CELL LDR,  11 10 0 LDR,
      12 0 MOVN,  12 12 11 SUB,
      9 12 CMP,  C-LS room-ok BCOND,
      SP SP 32 ADDI,  DEF-OCC-EXHAUSTED,
      room-ok LBL,
      9 SP 0 LDR,  10 SP 8 LDR,  11 SP 16 LDR,  12 SP 24 LDR,
      SP SP 32 ADDI,  RET,
   EM-DEF-OCC:LISSUE LABEL@ LBL,
      SP SP 48 SUBI,
      30 SP 0 STR,  9 SP 8 STR,  10 SP 16 STR,  11 SP 24 STR,  12 SP 32 STR,
      9 1 MOVZ,  EM-DEF-OCC:LROOM LABEL@ BL,
      9 SP 8 LDR,  10 DATA EM-DEF-OCC:PTR-CELL LDR,  11 10 0 LDR,
      11 11 1 ADDI,  11 10 0 STR,
      12 9 3 LSLI,  12 10 12 ADD,  11 12 8 STR,
      30 SP 0 LDR,  9 SP 8 LDR,  10 SP 16 LDR,  11 SP 24 LDR,  12 SP 32 LDR,
      SP SP 48 ADDI,  RET,
   EM-DEF-OCC:LCOUNT LABEL@ LBL,
      SP SP 64 SUBI,
      30 SP 0 STR,  9 SP 8 STR,  10 SP 16 STR,  11 SP 24 STR,
      12 SP 32 STR,  13 SP 40 STR,  14 SP 48 STR,  15 SP 56 STR,
      10 NDICT 0 ADDI,
      9 10 CMP,  C-LT lower BCOND,  C-GT raise BCOND,
       done B,
       raise LBL,
          12 9 10 SUB,
          9 12 0 ADDI,  EM-DEF-OCC:LROOM LABEL@ BL,
         9 SP 8 LDR,  10 NDICT 0 ADDI,
         11 DATA EM-DEF-OCC:PTR-CELL LDR,  13 11 0 LDR,
      loop LBL,
         10 9 CMP,  C-CS raised BCOND,
         13 13 1 ADDI,
         12 10 3 LSLI,  12 11 12 ADD,  13 12 8 STR,
         10 10 1 ADDI,  loop B,
      raised LBL,
         13 11 0 STR,
         done B,
      lower LBL,
         PROF-ABI:LPROFTRIM LABEL@ BL,
         CAPTURE-CUT,
         12 DATA NDICT-HW-CELL LDR,  12 NDICT CMP,  12 12 NDICT C-HI CSEL,   \ raise the high-water to the count this lowers from
         12 DATA NDICT-HW-CELL STR,                       \ (layout.f NDICT-HW-CELL)
         11 DATA EM-DEF-OCC:PTR-CELL LDR,  10 9 0 ADDI,
      clear-loop LBL,
         10 NDICT CMP,  C-CS done BCOND,
         12 10 3 LSLI,  12 11 12 ADD,
         13 0 MOVZ,  13 12 8 STR,
         10 10 1 ADDI,  clear-loop B,
      done LBL,
      NDICT 9 0 ADDI,
      30 SP 0 LDR,  9 SP 8 LDR,  10 SP 16 LDR,  11 SP 24 LDR,
      12 SP 32 LDR,  13 SP 40 LDR,  14 SP 48 LDR,  15 SP 56 LDR,
      SP SP 64 ADDI,  RET,
   EM-DEF-OCC:LAPPEND LABEL@ LBL,
      SP SP 80 SUBI,
      30 SP 0 STR,  9 SP 8 STR,  10 SP 16 STR,  11 SP 24 STR,
      12 SP 32 STR,  13 SP 40 STR,  14 SP 48 STR,  15 SP 56 STR,
      16 SP 64 STR,  17 SP 72 STR,
      9 NDICT 1 ADDI,  EM-DEF-OCC:LCOUNT LABEL@ BL,
      \ Only APPEND publishes a new record. COUNT also restores old rows.
      11 DATA NCOMP-DISPATCH:DECL-CELL LDR,
      11 append-done CBZ,
      12 11 CHECKER-OWNER-ABI:HEADER-BYTES SUBI,
      13 12 0 LDR,  14 CHECKER-OWNER-ABI:MAGIC LIT64,
      13 14 CMP,  C-NE append-done BCOND,
      13 12 8 LDR,  14 $440 LIT64,
      13 14 CMP,  C-LT append-done BCOND,
      11 11 $438 LDR,  11 append-done CBZ,
         9 NDICT 1 SUBI,  9 G-PUSH  11 BLR,
      append-done LBL,
      30 SP 0 LDR,  9 SP 8 LDR,  10 SP 16 LDR,  11 SP 24 LDR,
      12 SP 32 LDR,  13 SP 40 LDR,  14 SP 48 LDR,  15 SP 56 LDR,
      16 SP 64 LDR,  17 SP 72 LDR,  SP SP 80 ADDI,  RET,
   EM-DEF-OCC:LRESET LABEL@ LBL,
      SP SP 64 SUBI,
      30 SP 0 STR,  9 SP 8 STR,  10 SP 16 STR,  11 SP 24 STR,
      12 SP 32 STR,  13 SP 40 STR,  14 SP 48 STR,
      EM-DEF-OCC:LROOM LABEL@ BL,                         \ refuse exhaustion before clearing a live reference
      10 DATA EM-DEF-OCC:PTR-CELL LDR,  11 0 MOVZ,
      reset-loop LBL,
         12 DICT-CAP LIT64,  11 12 CMP,  C-CS reset BCOND,
         12 11 3 LSLI,  12 10 12 ADD,
         13 0 MOVZ,  13 12 8 STR,
         11 11 1 ADDI,  reset-loop B,
      reset LBL,
      NDICT 0 MOVZ,
      EM-DEF-OCC:LCOUNT LABEL@ BL,
      30 SP 0 LDR,  9 SP 8 LDR,  10 SP 16 LDR,  11 SP 24 LDR,
      12 SP 32 LDR,  13 SP 40 LDR,  14 SP 48 LDR,
      SP SP 64 ADDI,  RET,
   \ Fresh process installation. Seed records already occupy [0,NDICT).
   EM-DEF-OCC:LINIT LABEL@ LBL,
      SP SP 32 SUBI,  30 SP 0 STR,  13 SP 8 STR,  14 SP 16 STR,  15 SP 24 STR,
      0 0 MOVZ,  1 EM-DEF-OCC:STATE-BYTES LIT64,  2 3 MOVZ,
      3 MAP-ANON-PRIVATE LIT64,  4 0 MOVN,  5 0 MOVZ,
      NR-MMAP SYS,  C-CS init-bad BCOND,
      0 DATA EM-DEF-OCC:PTR-CELL STR,
      9 NDICT 0 ADDI,  EM-DEF-OCC:LRESET LABEL@ BL,
      30 SP 0 LDR,  13 SP 8 LDR,  14 SP 16 LDR,  15 SP 24 LDR,
      SP SP 32 ADDI,  RET,
   init-bad LBL,
      0 2 MOVZ,  1 init-msg ADR,  2 mu MOVZ,  NR-WRITE SYS,
      0 78 MOVZ,  NR-EXIT-GROUP SYS,
   init-msg LBL,  ma mu BYTES, ;

\ The one lower-or-retire decision of a rollback: evaluate's and the REPL's
\ recovery and replay-close (habu2.f) call it with x9 = the NDICT they lower to.
\ With no task live it returns x14 = 0 and the caller lowers as before; a zero
\ count cannot rise under it, since only main or a live task activates one.
\ While TASKS-LIVE-CELL is nonzero the engine lowers nothing: a task may run the
\ code above the caller's CP target and read the records above x9, and lowering
\ hands both out again. So it retires each record in [x9, NDICT) as `undefine`
\ does (xref.f XREF:RETIRE): DICT-WL:RETIRED over the wordlist cell, one aligned
\ store through the alias, the row left on its old hash chain, where a lookup's
\ wid compare passes it. It needs none of the rest of `undefine`: GRID-RETIRE's
\ work is done by RETRACT-TO's effect rewind, which clears the grids above the
\ mark (src/core/checker.f RECORD-LINK-REWIND-EFFECT); LCOUNT's CAPTURE-CUT
\ cuts NDICT, which stays; and the BEFORE-RETIRE hook
\ (NUNIT-OBJECT:FILE-RETIRE) refuses a chosen `undefine` of another file's
\ record, and a rollback that lowered those records never asked it either. It
\ clears a LASTC at or above x9 and returns x14 nonzero, and the caller keeps
\ CP, NDICT, DP and the relocation rows: the code stays in the region,
\ unreachable by name. LCODEINV and LRECLAIM do not run: both hand back code at
\ or above a floor, and none is handed back. Preserves every register but x14.
\ A package row gets DICT-WL:RETIRED-NAMESPACE by the same one store and keeps
\ its wids in [0] and [8]: a task's qualified lookup that read the row before
\ the store finds only the package's words, which are retiring too and whose
\ code is kept. Skipping the row would not do: REPLAY-CLOSE's SCOPE restore
\ puts WIDN back, and a live row would share its wid with the next package. A
\ row already retired, by an inner rollback or `undefine`, keeps its marker.
: EMIT-DEF-OCC-RETIRE ( -- )
   LBL LBL LBL LBL LBL {: loop:label pkg:label next:label cut:label done:label :}
   EM-DEF-OCC:LRETIRE LABEL@ LBL,
      14 DATA TASKS-LIVE-CELL LDR,  14 done CBZ,
      SP SP 48 SUBI,  9 SP 0 STR,  10 SP 8 STR,  11 SP 16 STR,  12 SP 24 STR,
      13 SP 32 STR,  15 SP 40 STR,
      11 DATA REGION-ALIAS:DELTA-CELL LDR,  11 DBASE 11 ADD,   \ the records through the alias
      12 DICT-WL:RETIRED invert MOVN,
      15 DICT-WL:NAMESPACE invert MOVN,
   loop LBL,
      9 NDICT CMP,  C-CS cut BCOND,
      10 DREC MOVZ,  10 9 10 MUL,  10 11 10 ADD,
      13 10 40 LDR,
      13 15 CMP,  C-EQ pkg BCOND,
      13 0 CMPI,  C-LT next BCOND,                          \ retired already
      12 10 40 STR,  next B,                                \ the wordlist cell
   pkg LBL,
      13 DICT-WL:RETIRED-NAMESPACE invert MOVN,  13 10 40 STR,
   next LBL,
      9 9 1 ADDI,  loop B,
   cut LBL,
      9 SP 0 LDR,  9 10 11 LASTC-CUT,
      9 SP 0 LDR,  10 SP 8 LDR,  11 SP 16 LDR,  12 SP 24 LDR,
      13 SP 32 LDR,  15 SP 40 LDR,
      SP SP 48 ADDI,
   done LBL,  RET, ;

\ x9 names the four-byte patch (zero for an effect-only change); x10 names the
\ created record. Every record covering changed bytes is replaced separately.
: EMIT-DEF-OCC-OVERLAP ( -- )
   LBL LBL LBL LBL LBL LBL {: hit:label miss:label raw:label
      count-loop:label count-next:label counted:label :}
   LBL LBL LBL LBL LBL LBL {: clear-loop:label clear-next:label cleared:label
      issue-loop:label issue-next:label issued:label :}
   EM-DEF-OCC:LMATCH LABEL@ LBL,
      12 0 MOVZ,
      11 10 CMP,  C-EQ hit BCOND,
      9 miss CBZ,
      15 11 40 LDR,  16 DICT-WL:NAMESPACE invert MOVN,
      15 16 CMP,  C-EQ miss BCOND,
      16 DICT-WL:RETIRED-NAMESPACE invert MOVN,  15 16 CMP,  C-EQ miss BCOND,
      15 11 0 LDR,  15 9 CMP,  C-HI miss BCOND,
      16 11 8 LDR,  17 CODE-SPAN:MASK LIT64,  17 16 17 AND,
      12 CODE-SPAN:FULL LIT64,  16 16 12 AND,
      16 raw CBNZ,  17 17 CODE-SPAN:INSN-BYTES ADDI,
   raw LBL,
      15 15 17 ADD,  15 9 CMP,  C-LS miss BCOND,
   hit LBL,
      12 1 MOVZ,  RET,
   miss LBL,
      12 0 MOVZ,  RET,
   EM-DEF-OCC:LBEFORE LABEL@ LBL,
      SP SP 80 SUBI,
      30 SP 0 STR,  9 SP 8 STR,  10 SP 16 STR,  11 SP 24 STR,
      12 SP 32 STR,  13 SP 40 STR,  14 SP 48 STR,  15 SP 56 STR,
      16 SP 64 STR,  17 SP 72 STR,
      13 0 MOVZ,  14 0 MOVZ,
   count-loop LBL,
      13 NDICT CMP,  C-CS counted BCOND,
      11 DREC MOVZ,  11 13 11 MUL,  11 DBASE 11 ADD,
      EM-DEF-OCC:LMATCH LABEL@ BL,
      12 count-next CBZ,
      14 14 1 ADDI,
   count-next LBL,
      13 13 1 ADDI,  count-loop B,
   counted LBL,
      9 14 0 ADDI,  EM-DEF-OCC:LROOM LABEL@ BL,
      9 SP 8 LDR,  13 0 MOVZ,
   clear-loop LBL,
      13 NDICT CMP,  C-CS cleared BCOND,
      11 DREC MOVZ,  11 13 11 MUL,  11 DBASE 11 ADD,
      EM-DEF-OCC:LMATCH LABEL@ BL,
      12 clear-next CBZ,
      15 DATA EM-DEF-OCC:PTR-CELL LDR,
      16 13 3 LSLI,  15 15 16 ADD,
      16 0 MOVZ,  16 15 8 STR,
   clear-next LBL,
      13 13 1 ADDI,  clear-loop B,
   cleared LBL,
      30 SP 0 LDR,  9 SP 8 LDR,  10 SP 16 LDR,  11 SP 24 LDR,
      12 SP 32 LDR,  13 SP 40 LDR,  14 SP 48 LDR,  15 SP 56 LDR,
      16 SP 64 LDR,  17 SP 72 LDR,
      SP SP 80 ADDI,  RET,
   EM-DEF-OCC:LAFTER LABEL@ LBL,
      SP SP 80 SUBI,
      30 SP 0 STR,  9 SP 8 STR,  10 SP 16 STR,  11 SP 24 STR,
      12 SP 32 STR,  13 SP 40 STR,  14 SP 48 STR,  15 SP 56 STR,
      16 SP 64 STR,  17 SP 72 STR,
      13 0 MOVZ,
   issue-loop LBL,
      13 NDICT CMP,  C-CS issued BCOND,
      11 DREC MOVZ,  11 13 11 MUL,  11 DBASE 11 ADD,
      EM-DEF-OCC:LMATCH LABEL@ BL,
      12 issue-next CBZ,
      9 13 0 ADDI,  EM-DEF-OCC:LISSUE LABEL@ BL,
      9 SP 8 LDR,
   issue-next LBL,
      13 13 1 ADDI,  issue-loop B,
   issued LBL,
      30 SP 0 LDR,  9 SP 8 LDR,  10 SP 16 LDR,  11 SP 24 LDR,
      12 SP 32 LDR,  13 SP 40 LDR,  14 SP 48 LDR,  15 SP 56 LDR,
      16 SP 64 LDR,  17 SP 72 LDR,
      SP SP 80 ADDI,  RET, ;

variable FIND-LINEAR
variable FIND-HLOOP
variable FIND-HNEXT
variable FIND-HINL
variable FIND-HCMP
variable FIND-HMATCH

\ LFIND's register ABI. In: x9 = name address, x10 = name length. Out:
\ x13 = 0 when the name is not in scope, else 1 | the folded record flags
\ (bit 1 immediate, bit 3 wide-effect, bit 4 internal, bits 8-15 certified
\ min-in); on a hit x11 = the record's code cell [0], x12 = its length cell [8],
\ and x5 = the matched record, zero on a miss. It clobbers x2-x17 and
\ preserves XDS (x19) and DATA (x20).
\ x5 is the only way to read another cell of the record the name resolved to
\ (the sealed-WID gate reads [40]): an xt does not name a record, because
\ `EXPORT` publishes one body under several records and wids. The linear scan
\ takes the LAST match, so it parks it in x17 and FIND-FOUND moves it to x5.
\ tools/lint/clobber-lint-core.f lists Lfind's and Lfindused's returns as
\ x11/x12/x13 and Laotwidgate's preserved set as x11, without x5: a false red,
\ never a false green.
: EMIT-FIND ( -- )
   LBL LBL LBL LBL LBL {: qlin:label qhloop:label qhnext:label qhinl:label qhcmp:label :}
   LFIND LABEL@ LBL,
   LBL FIND-QSCAN !
   LBL FIND-QNONE !
   LBL FIND-QHAS !
   LBL FIND-QBAD !
   LBL FIND-QTAIL !
   LBL FIND-QTAILOK !
   LBL FIND-NLOOP !
   LBL FIND-NNEXT !
   LBL FIND-NCMP !
   LBL FIND-NMATCH !
   LBL FIND-NEND !
   LBL FIND-NINL !
   LBL FIND-START !
   LBL FIND-LOOP !
   LBL FIND-DONE !
   LBL FIND-NEXT !
   LBL FIND-CMP !
   LBL FIND-MATCH !
   LBL FIND-INL !
   LBL FIND-MISS !
   LBL FIND-TRYG !
   LBL FIND-FOUND !
   LBL FIND-LINEAR !
   LBL FIND-HLOOP !
   LBL FIND-HNEXT !
   LBL FIND-HINL !
   LBL FIND-HCMP !
   LBL FIND-HMATCH !
   13 0 MOVZ,
   17 0 MOVZ,
   FIND-QSCAN LABEL@ LBL,
      17 10 CMP,  C-GE FIND-QNONE LABEL@ BCOND,
      14 9 17 ADD,  14 14 0 LDRB,  14 $3A CMPI,  C-EQ FIND-QHAS LABEL@ BCOND,
      17 17 1 ADDI,  FIND-QSCAN LABEL@ B,
   FIND-QNONE LABEL@ LBL,
      2 DATA PKG-PRI-CELL LDR,  FIND-START LABEL@ B,
   FIND-QHAS LABEL@ LBL,
      17 0 CMPI,  C-EQ FIND-QNONE LABEL@ BCOND,
      14 17 1 ADDI,  14 10 CMP,  C-GE FIND-QNONE LABEL@ BCOND,
      14 17 1 ADDI,
   FIND-QTAIL LABEL@ LBL,
      14 10 CMP,  C-GE FIND-QTAILOK LABEL@ BCOND,
      15 9 14 ADD,  15 15 0 LDRB,  15 $3A CMPI,  C-EQ FIND-QBAD LABEL@ BCOND,
      14 14 1 ADDI,  FIND-QTAIL LABEL@ B,
   FIND-QTAILOK LABEL@ LBL,
      \ NAME:tail: a wordlist is a record with the marker -1 in its wid cell,
      \ so the same probe finds it with the head as name and -1 as wid. x17 is
      \ the head's length; x9/x10/x13/x17 survive the probe for the linear
      \ fallback and FIND-NMATCH. An empty slot means no wordlist by that name.
      14 DATA HIDXP-CELL LDR,  14 qlin CBZ,                    \ no table -> linear
      2 0 MOVN,                                                \ wordlist records carry wid -1
      9 17 15 4 16 7 C-HIDX-HASH
      6 15 2 EOR,  5 HIDX-SLOTS 1 - LIT64,  6 6 5 AND,
      8 HIDX-SLOTS LIT64,
   qhloop LBL,
      16 6 2 LSLI,  16 14 16 ADD,  3 16 0 LDRW,                \ x3 = slot value (index+1)
      3 FIND-NEND LABEL@ CBZ,                                  \ empty slot -> no such wordlist
      4 3 1 SUBI,  4 NDICT CMP,  C-GE qhnext BCOND,            \ stale (truncated) index
      5 DREC MOVZ,  5 4 5 MUL,  5 DBASE 5 ADD,                 \ x5 = record ptr
      16 5 40 LDR,  16 2 CMP,  C-NE qhnext BCOND,              \ not a wordlist record
      16 5 16 LDR,  16 16 DNAME-FLAG-BITS LSLI,  16 16 DNAME-FLAG-BITS LSRI,  16 17 CMP,  C-NE qhnext BCOND,  \ name-len mismatch
      16 5 24 ADDI,
      3 5 16 LDR,  3 3 DNAME-EXT ANDI,  3 qhinl CBZ,
         16 5 24 LDR,
      qhinl LBL,
      7 0 MOVZ,
      qhcmp LBL,
         7 17 CMP,  C-GE FIND-NMATCH LABEL@ BCOND,
         15 16 7 ADD,  15 15 0 LDRB,
         3 15 $41 SUBI,  3 $1A CMPI,  3 C-CC CSET,  3 3 5 LSLI,  15 15 3 ORR,
         4 9 7 ADD,     4 4 0 LDRB,
         3 4 $41 SUBI,   3 $1A CMPI,  3 C-CC CSET,  3 3 5 LSLI,  4 4 3 ORR,
         15 4 CMP,  C-NE qhnext BCOND,
         7 7 1 ADDI,  qhcmp B,
      qhnext LBL,
         8 8 1 SUBI,  8 qlin CBZ,
         6 6 1 ADDI,  5 HIDX-SLOTS 1 - LIT64,  6 6 5 AND,  qhloop B,
   qlin LBL,
      5 DBASE 0 ADDI,  6 NDICT 0 ADDI,
   FIND-NLOOP LABEL@ LBL,
      6 FIND-NEND LABEL@ CBZ,
      14 5 40 LDR,  15 0 MOVN,  14 15 CMP,  C-NE FIND-NNEXT LABEL@ BCOND,
      14 5 16 LDR,  14 14 DNAME-FLAG-BITS LSLI,  14 14 DNAME-FLAG-BITS LSRI,  14 17 CMP,  C-NE FIND-NNEXT LABEL@ BCOND,
      16 5 24 ADDI,
      14 5 16 LDR,  14 14 DNAME-EXT ANDI,  14 FIND-NINL LABEL@ CBZ,
         16 5 24 LDR,
      FIND-NINL LABEL@ LBL,
      7 0 MOVZ,
      FIND-NCMP LABEL@ LBL,
         7 17 CMP,  C-GE FIND-NMATCH LABEL@ BCOND,
         15 16 7 ADD,  15 15 0 LDRB,
         3 15 $41 SUBI,  3 $1A CMPI,  3 C-CC CSET,  3 3 5 LSLI,  15 15 3 ORR,
         4 9 7 ADD,     4 4 0 LDRB,
         3 4 $41 SUBI,   3 $1A CMPI,  3 C-CC CSET,  3 3 5 LSLI,  4 4 3 ORR,
         15 4 CMP,  C-NE FIND-NNEXT LABEL@ BCOND,
         7 7 1 ADDI,  FIND-NCMP LABEL@ B,
      FIND-NMATCH LABEL@ LBL,
         2 5 0 LDR,
         9 9 17 ADD,  9 9 1 ADDI,
         10 10 17 SUB,  10 10 1 SUBI,
         FIND-START LABEL@ B,
      FIND-NNEXT LABEL@ LBL,  5 5 DREC ADDI,  6 6 1 SUBI,  FIND-NLOOP LABEL@ B,
   FIND-NEND LABEL@ LBL,  5 0 MOVZ,  RET,       \ no such wordlist: no record, and x5 says so
   FIND-QBAD LABEL@ LBL,  5 0 MOVZ,  RET,       \ malformed NAME:a:b - the one exit that never touched x5
   FIND-START LABEL@ LBL,
      \ Hash probe for (name XOR wid); a validated slot (index<NDICT, wid==x2,
      \ name equal) returns at once. An empty slot ends the search at FIND-DONE,
      \ where the linear scan's miss goes. That rests on every record in
      \ [0,NDICT) being in the table: LHIDXBUILD indexes the dictionary at
      \ startup, every publisher calls LHIDXADD as it raises NDICT, record-wid!
      \ re-indexes (HIDX-EMIT:LREKEY), and no slot is removed (a truncated
      \ record's stale index is skipped), so slots only go empty -> occupied and
      \ an insert takes the first empty or stale slot of its own chain.
      \ The linear scan serves a missing table and a chain walked in full
      \ without an empty slot; x2 (wid), x9/x10 (name), x13 (result) are
      \ preserved for it.
      14 DATA HIDXP-CELL LDR,  14 FIND-LINEAR LABEL@ CBZ,      \ no table -> linear
      9 10 15 4 16 7 C-HIDX-HASH
      6 15 2 EOR,  5 HIDX-SLOTS 1 - LIT64,  6 6 5 AND,                 \ slot = (hash XOR wid) & (HIDX-SLOTS-1)
      8 HIDX-SLOTS LIT64,
   FIND-HLOOP LABEL@ LBL,
      17 6 2 LSLI,  17 14 17 ADD,  3 17 0 LDRW,               \ x3 = slot value (index+1)
      3 FIND-DONE LABEL@ CBZ,                                 \ empty slot -> absent from this wordlist
      4 3 1 SUBI,  4 NDICT CMP,  C-GE FIND-HNEXT LABEL@ BCOND, \ stale (truncated) index
      5 DREC MOVZ,  5 4 5 MUL,  5 DBASE 5 ADD,                \ x5 = record ptr
      16 5 40 LDR,  16 2 CMP,  C-NE FIND-HNEXT LABEL@ BCOND,  \ wid mismatch (retired=-2 / other wl)
      16 5 16 LDR,  16 16 DNAME-FLAG-BITS LSLI,  16 16 DNAME-FLAG-BITS LSRI,  16 10 CMP,  C-NE FIND-HNEXT LABEL@ BCOND,  \ name-len mismatch
      16 5 24 ADDI,
      3 5 16 LDR,  3 3 DNAME-EXT ANDI,  3 FIND-HINL LABEL@ CBZ,
         16 5 24 LDR,
      FIND-HINL LABEL@ LBL,
      7 0 MOVZ,
      FIND-HCMP LABEL@ LBL,
         7 10 CMP,  C-GE FIND-HMATCH LABEL@ BCOND,
         15 16 7 ADD,  15 15 0 LDRB,
         3 15 $41 SUBI,  3 $1A CMPI,  3 C-CC CSET,  3 3 5 LSLI,  15 15 3 ORR,
         4 9 7 ADD,     4 4 0 LDRB,
         3 4 $41 SUBI,   3 $1A CMPI,  3 C-CC CSET,  3 3 5 LSLI,  4 4 3 ORR,
         15 4 CMP,  C-NE FIND-HNEXT LABEL@ BCOND,
         7 7 1 ADDI,  FIND-HCMP LABEL@ B,
      FIND-HMATCH LABEL@ LBL,
         11 5 0 LDR,  12 5 8 LDR,
         14 5 16 LDR,
         15 14 DNAME-WIDE ANDI,  15 15 59 LSRI,               \ wide-effect bit -> 8
         16 14 DNAME-INT ANDI,  16 16 59 LSRI,                \ internal bit -> 16
         15 15 16 ORR,
         16 14 DNAME-MIN-IN-MASK ANDI,  16 16 44 LSRI,        \ certified min-in byte -> bits 8-15
         15 15 16 ORR,
         14 14 DNAME-IMM ANDI,  14 14 59 LSRI,                \ immediate bit -> 2
         14 14 15 ORR,
         13 1 MOVZ,  13 13 14 ORR,  RET,
      FIND-HNEXT LABEL@ LBL,
         8 8 1 SUBI,  8 FIND-LINEAR LABEL@ CBZ,
         6 6 1 ADDI,  5 HIDX-SLOTS 1 - LIT64,  6 6 5 AND,  FIND-HLOOP LABEL@ B,
   FIND-LINEAR LABEL@ LBL,
      5 DBASE 0 ADDI,  6 NDICT 0 ADDI,
   FIND-LOOP LABEL@ LBL,
      6 FIND-DONE LABEL@ CBZ,
      14 5 40 LDR,  14 2 CMP,  C-NE FIND-NEXT LABEL@ BCOND,
      14 5 16 LDR,  14 14 DNAME-FLAG-BITS LSLI,  14 14 DNAME-FLAG-BITS LSRI,  14 10 CMP,  C-NE FIND-NEXT LABEL@ BCOND,
      16 5 24 ADDI,
      14 5 16 LDR,  14 14 DNAME-EXT ANDI,  14 FIND-INL LABEL@ CBZ,
         16 5 24 LDR,
      FIND-INL LABEL@ LBL,
      7 0 MOVZ,
      FIND-CMP LABEL@ LBL,
         7 10 CMP,  C-GE FIND-MATCH LABEL@ BCOND,
         15 16 7 ADD,  15 15 0 LDRB,
         3 15 $41 SUBI,  3 $1A CMPI,  3 C-CC CSET,  3 3 5 LSLI,  15 15 3 ORR,
         4 9 7 ADD,     4 4 0 LDRB,
         3 4 $41 SUBI,   3 $1A CMPI,  3 C-CC CSET,  3 3 5 LSLI,  4 4 3 ORR,
         15 4 CMP,  C-NE FIND-NEXT LABEL@ BCOND,
         7 7 1 ADDI,  FIND-CMP LABEL@ B,
      FIND-MATCH LABEL@ LBL,
         11 5 0 LDR,  12 5 8 LDR,
         14 5 16 LDR,
         15 14 DNAME-WIDE ANDI,  15 15 59 LSRI,               \ wide-effect bit -> 8
         16 14 DNAME-INT ANDI,  16 16 59 LSRI,                \ internal bit -> 16
         15 15 16 ORR,
         16 14 DNAME-MIN-IN-MASK ANDI,  16 16 44 LSRI,        \ certified min-in byte -> bits 8-15
         15 15 16 ORR,
         14 14 DNAME-IMM ANDI,  14 14 59 LSRI,                \ immediate bit -> 2
         14 14 15 ORR,
         13 1 MOVZ,  13 13 14 ORR,
         17 5 0 ADDI,                                         \ park the match: x5 is this loop's cursor, x17 is not touched by it
         FIND-NEXT LABEL@ B,
      FIND-NEXT LABEL@ LBL,  5 5 DREC ADDI,  6 6 1 SUBI,  FIND-LOOP LABEL@ B,
   FIND-DONE LABEL@ LBL,
      13 FIND-FOUND LABEL@ CBNZ,
      14 DATA PKG-PRI-CELL LDR,  14 FIND-MISS LABEL@ CBZ,
      14 2 CMP,  C-NE FIND-TRYG LABEL@ BCOND,
         2 DATA PKG-PUB-CELL LDR,  FIND-START LABEL@ B,
      FIND-TRYG LABEL@ LBL,
      14 DATA PKG-PUB-CELL LDR,  14 2 CMP,  C-NE FIND-MISS LABEL@ BCOND,
         2 0 MOVZ,  FIND-START LABEL@ B,
      \ Only the LINEAR scan can arrive here with x13 set - the hash probe's own
      \ match returns from FIND-HMATCH - so the parked pointer is this call's.
      FIND-FOUND LABEL@ LBL,  5 17 0 ADDI,  RET,
      FIND-MISS LABEL@ LBL,   5 0 MOVZ,  RET, ;

: C-NUM-INIT-REGS ( -- )
   11 0 MOVZ,  13 1 MOVZ,  14 0 MOVZ,  12 0 MOVZ,  6 10 MOVZ,
   16 0 MOVZ,  17 0 MOVZ, ;                                  \ overflow latch, final literal refusal

: C-NUM-SIGN ( -- )
   10 NUM-DONE LABEL@ CBZ,
   15 9 0 LDRB,  15 45 CMPI,  C-NE NUM-NDOLL LABEL@ BCOND,
      13 0 MOVN,  14 1 MOVZ,
   NUM-NDOLL LABEL@ LBL,
   14 10 CMP,  C-GE NUM-DONE LABEL@ BCOND, ;

: C-NUM-BASE ( -- )
   5 9 14 ADD,  15 5 0 LDRB,  15 36 CMPI,  C-NE NUM-NOHEX LABEL@ BCOND,
      6 16 MOVZ,  14 14 1 ADDI,
   NUM-NOHEX LABEL@ LBL,
   2 0 MOVZ,                                                    \ frac mode off
   14 10 CMP,  C-GE NUM-DONE LABEL@ BCOND, ;

: C-NUM-DOT ( -- )
   15 46 CMPI,  C-NE NUM-NDOT LABEL@ BCOND,                      \ '.' -> frac mode
      6 10 CMPI,  C-NE NUM-DONE LABEL@ BCOND,                    \ only base 10
      2 NUM-DONE LABEL@ CBNZ,                                    \ second dot -> fail
      2 1 MOVZ,  4 0 MOVZ,  3 1 MOVZ,                           \ frac=0 scale=1
      14 14 1 ADDI,  NUM-LOOP LABEL@ B,
   NUM-NDOT LABEL@ LBL, ;

: C-NUM-DIGIT ( -- )
   15 48 CMPI,  C-LT NUM-DONE LABEL@ BCOND,
   15 57 CMPI,  C-GT NUM-ND LABEL@ BCOND,
      7 15 48 SUBI,  NUM-GOTD LABEL@ B,
   NUM-ND LABEL@ LBL,
   6 16 CMPI,  C-NE NUM-DONE LABEL@ BCOND,
   15 97 CMPI,  C-LT NUM-NUC LABEL@ BCOND,  15 102 CMPI,  C-GT NUM-DONE LABEL@ BCOND,
      7 15 87 SUBI,  NUM-GOTD LABEL@ B,
   NUM-NUC LABEL@ LBL,
   15 65 CMPI,  C-LT NUM-DONE LABEL@ BCOND,  15 70 CMPI,  C-GT NUM-DONE LABEL@ BCOND,
      7 15 55 SUBI, ;

: C-NUM-INT-STEP ( -- )
   5 0 MOVN,  5 5 7 SUB,                                    \ unsigned cell limit minus this digit
   8 5 6 UDIV,  11 8 CMP,  5 C-HI CSET,  16 16 5 ORR,
   11 11 6 MUL,  11 11 7 ADD,
   14 14 1 ADDI,  NUM-LOOP LABEL@ B, ;

: C-NUM-FRAC-STEP ( -- )
   5 $7FFFFFFFFFFFFFFF LIT64,  5 5 7 SUB,
   8 10 MOVZ,  5 5 8 UDIV,  4 5 CMP,  5 C-GT CSET,  16 16 5 ORR,
   5 $0CCCCCCCCCCCCCCC LIT64,  3 5 CMP,  5 C-GT CSET,  16 16 5 ORR,
   16 NUM-FRAC-NEXT LABEL@ CBNZ,
   5 10 MOVZ,  4 4 5 MUL,  4 4 7 ADD,  3 3 5 MUL,
   NUM-FRAC-NEXT LABEL@ LBL,
   14 14 1 ADDI,  NUM-LOOP LABEL@ B, ;

: C-NUM-FLOAT-FINISH ( -- )
   \ x15 is the last consumed character; overflow can leave the scale at one.
   15 46 CMPI,  C-EQ NUM-DONE LABEL@ BCOND,                     \ "1." (no frac digits) -> ordinary word
   5 $7FFFFFFFFFFFFFFF LIT64,  11 5 CMP,  5 C-HI CSET,
   17 16 5 ORR,  17 NUM-DONE LABEL@ CBNZ,                      \ a complete decimal owns its range refusal
   LBL LBL LBL LBL LBL LBL LBL
   {: left:label right:label shift:label ready:label round:label pack:label sign:label :}
   \ x11 significand, x4 exact remainder, x3 scale; x6 exponent,
   \ x7 guard and x8 sticky. The remainder is below 10^18 throughout.
   5 11 4 ORR,  5 sign CBZ,                                    \ preserve signed zero
   6 52 MOVZ,
   5 $20000000000000 LIT64,  11 5 CMP,  C-LT left BCOND,
   4 0 CMPI,  8 C-NE CSET,  7 0 MOVZ,
   right LBL,
      8 8 7 ORR,  7 11 1 ANDI,  11 11 1 LSRI,  6 6 1 ADDI,
      11 5 CMP,  C-GE right BCOND,
   round B,
   left LBL,
   5 $10000000000000 LIT64,
   shift LBL,
      11 5 CMP,  C-GE ready BCOND,
      11 11 1 LSLI,  4 4 1 LSLI,
      4 3 CMP,  14 C-GE CSET,
      11 11 14 ADD,  15 14 3 MUL,  4 4 15 SUB,
      6 6 1 SUBI,  shift B,
   ready LBL,
   4 4 1 LSLI,  4 3 CMP,  7 C-GE CSET,
   14 7 3 MUL,  4 4 14 SUB,  4 0 CMPI,  8 C-NE CSET,
   round LBL,
   7 pack CBZ,
   14 11 1 ANDI,  14 14 8 ORR,  14 pack CBZ,                  \ ties retain an even low bit
   11 11 1 ADDI,
   5 $20000000000000 LIT64,  11 5 CMP,  C-NE pack BCOND,
      11 11 1 LSRI,  6 6 1 ADDI,                              \ rounding carried out of 53 bits
   pack LBL,
   6 6 1023 ADDI,  6 6 52 LSLI,
   11 11 $FFFFFFFFFFFFF ANDI,  11 11 6 ORR,
   sign LBL,
   13 13 63 LSRI,  13 13 63 LSLI,  11 11 13 ORR,
   12 1 MOVZ,  RET, ;

: C-NUM-INT-FINISH ( -- )
   LBL {: bounded:label :}
   17 16 0 ADDI,
   6 10 CMPI,  C-NE bounded BCOND,                             \ hex preserves all 64 bits, with modular negation
   5 $7FFFFFFFFFFFFFFF LIT64,  8 13 63 LSRI,  5 5 8 ADD,       \ decimal magnitude: INT64_MAX, or 2^63 when negative
   11 5 CMP,  5 C-HI CSET,  17 17 5 ORR,
   bounded LBL,
   17 NUM-DONE LABEL@ CBNZ,                                   \ a complete overflowed integer never becomes a name
   11 11 13 MUL,  12 1 MOVZ, ;

\ The engine's number reader ( x9=address, x10=length -> x11 value, x2 float?,
\ x12 read a number? ). It is registered as the sealed (NUM) helper because
\ `num-parse` (BNUMPARSE) reaches it by a direct BL, and the AOT closure walker
\ follows such a branch only to a record's exact code entry (aot-closure.f
\ FINDADDR-PTR). Its body reaches nothing outside [start, end), so the closure
\ copies it and rewrites its internal branches with no further claim.
: EMIT-NUM ( -- )
   LBL {: end:label :}
   s" (NUM)" LNUM LABEL@ LABEL>N end LABEL>N ENGINE-PRIMS:HELPER-REGISTER
   LNUM LABEL@ LBL,
   LBL NUM-DONE !
   LBL NUM-NDOLL !
   LBL NUM-NOHEX !
   LBL NUM-LOOP !
   LBL NUM-OK !
   LBL NUM-GOTD !
   LBL NUM-ND !
   LBL NUM-NUC !
   LBL NUM-NDOT !
   LBL NUM-ISFRAC !
   LBL NUM-FRAC-NEXT !
   LBL NUM-LINT !
   C-NUM-INIT-REGS
   C-NUM-SIGN
   C-NUM-BASE
   NUM-LOOP LABEL@ LBL,
   14 10 CMP,  C-GE NUM-OK LABEL@ BCOND,
   5 9 14 ADD,  15 5 0 LDRB,
   C-NUM-DOT
   C-NUM-DIGIT
   NUM-GOTD LABEL@ LBL,
   2 NUM-ISFRAC LABEL@ CBNZ,
   C-NUM-INT-STEP
   NUM-ISFRAC LABEL@ LBL,                                      \ frac digit: f=f*10+d, k*=10
   C-NUM-FRAC-STEP
   NUM-OK LABEL@ LBL,
   2 NUM-LINT LABEL@ CBZ,
   C-NUM-FLOAT-FINISH
   NUM-LINT LABEL@ LBL,  C-NUM-INT-FINISH
   NUM-DONE LABEL@ LBL,  RET,
   end LBL, ;

package ENGINE-EMIT
using ENGINE-PRIMS

\ Dictionary inline-name padding is always zero, independent of registry data.
create NAME-PADDING 0 , 0 ,

: EMIT-DICT ( -- )
   0 BEGIN dup ENGINE-PRIMS:COUNT < WHILE
      dup NAME-LEN DNAME-INL > IF
         LBL over NAME-LABEL!
         dup NAME-LABEL LBL,
         dup NAME$ BYTES,
      ELSE
         -1 >LABEL over NAME-LABEL!
      THEN
      1 + REPEAT drop
   LHOSTROW LABEL@ LBL,  ENGINE-PRIMS:SCALAR-ROW@ DCQ,
   LHOSTDIVROW LABEL@ LBL,  ENGINE-PRIMS:DIVISION-ROW@ DCQ,
   LNCOUNT LABEL@ LBL,  ENGINE-PRIMS:COUNT dup SEEDED-PRIM-N !  DCQ,
   LDICT LABEL@ LBL,
   0 BEGIN dup ENGINE-PRIMS:COUNT < WHILE
      dup FIRST-LABEL DLBL,
      dup LAST-LABEL DLBL,
      dup NAME-LEN DNAME-INL > IF
         dup DNAME DNAME-EXT or DCQ,
         dup NAME-LABEL DLBL,
         0 DCQ,
      ELSE
         dup DNAME DCQ,
         dup NAME$ BYTES,
         16 over NAME-LEN 3 + -4 and - dup 0 > IF NAME-PADDING swap BYTES, ELSE drop THEN
      THEN
      dup HELPER-WID DCQ,
      1 + REPEAT drop ;

;package

;using
;using

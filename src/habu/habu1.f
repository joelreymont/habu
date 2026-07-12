\ habu1.f — the ENGINE BUILDER part 1: emits the standalone native Forth's
\ primitives, helper routines, and seed dictionary. Needs asm.fs +
\ icode.fs + mnem.fs + rt.fs (g-push/g-pop/g-print9) + crash.fs + macho.fs.
\ Part 1: prims + tok/find/num/prot/flush/cemit + dict. The interpreter main
\ loop, keyword JIT and EMIT-FORTH follow in part 2 (habu2.f).
variable STDIN?   0 0= 0= STDIN? !
s" STDIN?" s" -- ptr bool" TRUST

package ENGINE-BUILD
variable ACTIVE
: ACTIVE? ( -- bool ) ACTIVE @ ;
: ARM ( -- ) 0 0= ACTIVE ! ;
: DISARM ( -- ) 0 0= 0= ACTIVE ! ;
public
: BUILDING? ( -- bool ) ACTIVE? ;
end-package
\ runtime instruction-word constants the JIT compiler stamps out
$D65F03C0 constant W-RET
$F9000269 constant W-PUSH0
$91002273 constant W-PUSH1
$D2800009 constant W-MOVZ0
$F2A00009 constant W-MOVK1
$F2C00009 constant W-MOVK2
$F2E00009 constant W-MOVK3
\ Pass-2 transaction cells are defined as one protected band in layout.f.
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

\ --- deref/execute arity-guard table (for the interpret-boundary guard) ---
\ A deref/execute prim (@ ! +! c@ c! atomic@ atomic! atomic-add atomic-cas count
\ type execute run-in-stack) faults inside its own body on a shallow stack, before
\ the post-token LMAIN depth-floor guard can see it. GDEREF-L / GDEREF-F register
\ such a prim like FPRIM-L / FPRIM and, when the prim is kept, record its entry
\ label + minimum input depth so EMIT-ARITY-GUARD can bake a pre-BLR depth check
\ keyed by the runtime xt (= entry-label address). Every other word stays min-in 0.
32 constant GDR-CAP
create GDR-LBL GDR-CAP cells allot   create GDR-MIN GDR-CAP cells allot
variable GDR-N
variable GD-MIN

: GUARD-ADD ( n n -- )                \ ( entry-label min-in ) append one guard-table row
   GDR-N @ GDR-CAP >= IF s" arity guard table full" 76 die THEN
   GDR-N @ cells GDR-MIN + !
   GDR-N @ cells GDR-LBL + !
   GDR-N @ 1+ GDR-N ! ;

: GD-RECORD ( -- )                    \ record ( FPL, GD-MIN ) iff the just-registered prim was kept
   FP-KEEP? IF FPL @ GD-MIN @ GUARD-ADD THEN ;

: GDEREF-L ( ptr u8 n n n -- )        \ leaf deref prim: name xt min-in
   GD-MIN !  FPRIM-L  GD-RECORD ;

: GDEREF-F ( ptr u8 n n n -- )        \ framed deref prim: name xt min-in
   GD-MIN !  FPRIM  GD-RECORD ;
\ shared label ids (forward refs)
variable LANCHOR  variable LFIND  variable LNUM  variable LDICT  variable LSRC  variable SRCN
variable LCEMIT   variable LTOK   variable LPROT  variable LPROTSPAN  variable LFLUSH variable LNCOUNT
variable LAOTCODE  variable LAOTDICT  variable LAOTCODELEN
variable LAOTNREC  variable LAOTNSITE  variable LAOTSITES  variable LAOTNAMES
variable LAOTNDSITE  variable LAOTDSITES  variable LAOTDATAD0  variable LAOTDATASIZE
variable LAOTNCSITE  variable LAOTCSITES  variable LAOTCODEB0
variable LAOTBOOTRUN
variable LAOTNPWID   variable LAOTPWID   \ protected-WID registry: count + u32 table (TFAM 2b-v)
variable LPROTWIDQ
variable LCFPUSH  variable LCFPOP  variable LPAT   variable LKWCMP  variable LBCAP  variable LBCS
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
variable LKWIMM variable LKWPOST variable LKWCOMPC
variable LKWDOES variable LKWQUOT variable LKWSEMIQ variable LKWPACKAGE variable LKWPUBLIC
variable LKWTRUSTED variable LKWTRUST variable LKWCHKDOES variable LKWKERNEL variable LKWPRIVATE variable LKWENDPACKAGE variable LKWDUPDEF variable LCHKPACKAGE variable LCHKPUB variable LCHKPRI variable LCHKENDPKG
variable LKWEXPORT variable LCHKEXPORT
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

: GUARD-ADDR-BAND ( n n n label -- ) {: addr:n off:n len:n trap:label :}
   DREG addr DATA SUB,
   EREG off LIT64,  EREG DREG EREG SUB,
   DREG len LIT64,
   EREG DREG CMP,  C-CC trap BCOND, ;

package GUARD

: BLOB-SPAN ( n label -- ) {: addr:n trap:label :}
   LBL {: skip:label :}
   DREG DATA TXN-BLOB-A-CELL LDR,
   DREG skip CBZ,
   EREG DREG CMP,  C-LS skip BCOND,
   EREG DATA TXN-BLOB-CAP-CELL LDR,
   DREG DREG EREG ADD,
   addr DREG CMP,  C-CC trap BCOND,
   skip LBL, ;

: BLOB-ADDR ( n label -- ) {: addr:n trap:label :}
   LBL {: skip:label :}
   DREG DATA TXN-BLOB-A-CELL LDR,
   DREG skip CBZ,
   EREG addr DREG SUB,
   DREG DATA TXN-BLOB-CAP-CELL LDR,
   EREG DREG CMP,  C-CC trap BCOND,
   skip LBL, ;

public

: SPAN ( n label -- ) BLOB-SPAN ;
: ADDR ( n label -- ) BLOB-ADDR ;

end-package

\ Span-aware protected-memory guard. addr and len name runtime registers. A
\ zero-length write is inert. Any address+length wrap traps before the region
\ tests, then every protected half-open interval is checked for intersection.
\ The guard is inactive only while the canonical cold prefix owns the open
\ friend latch. x12/x13 are the only clobbers.
: GUARD-SPAN ( n n -- ) {: addr:n len:n :}
   LBL LBL {: ok:label trap:label :}
   DREG DATA FRIEND-LATCH-CELL LDR,
   DREG ok CBZ,
   len ok CBZ,
   EREG addr len ADD,                   \ checked end = start + length
   EREG addr CMP,  C-CC trap BCOND,     \ unsigned wrap
   addr FRIEND-ARENA FRIEND-ARENA-LEN trap GUARD-BAND
   addr PROT-REG-OFF PROT-REG-LEN trap GUARD-BAND
   addr BODYBUF-OFF BODYBUF-CAP 2 + trap GUARD-BAND
   addr TXN-STATE-OFF TXN-STATE-LEN trap GUARD-BAND
   addr trap GUARD:SPAN
   ok B,
   trap LBL,  0 E-SEAL-VIOLATION MOVZ,  NR-EXIT-GROUP SYS,
   ok LBL, ;

: PROT-GUARD ( n -- )
   {: addr:n :}
   LBL LBL {: ok:label trap:label :}
   DREG DATA FRIEND-LATCH-CELL LDR,
   DREG ok CBZ,
   addr FRIEND-ARENA FRIEND-ARENA-LEN trap GUARD-ADDR-BAND
   addr PROT-REG-OFF PROT-REG-LEN trap GUARD-ADDR-BAND
   addr BODYBUF-OFF BODYBUF-CAP 2 + trap GUARD-ADDR-BAND
   addr TXN-STATE-OFF TXN-STATE-LEN trap GUARD-ADDR-BAND
   addr trap GUARD:ADDR
   ok B,
   trap LBL,  0 E-SEAL-VIOLATION MOVZ,  NR-EXIT-GROUP SYS,
   ok LBL, ;

\ A code emission target owns one aligned instruction inside the writable code
\ interval [DBASE+DICT-SIZE, DBASE+REGION). This invariant is independent of
\ the DATA protection latch: cp! may never redirect later LCEMIT writes into a
\ DATA band. The existing $4000 end reserve bounds a complete definition after
\ C-COLON-CODE-ROOM admits its first instruction.
: GUARD-CODE-WORD ( n -- ) {: addr:n :}
   LBL LBL {: ok:label trap:label :}
   DREG DICT-SIZE LIT64,  DREG DBASE DREG ADD,
   addr DREG CMP,  C-CC trap BCOND,
   DREG REGION 4 - LIT64,  DREG DBASE DREG ADD,
   addr DREG CMP,  C-HI trap BCOND,
   EREG addr 3 ANDI,  EREG trap CBNZ,
   ok B,
   trap LBL,  0 E-SEAL-VIOLATION MOVZ,  NR-EXIT-GROUP SYS,
   ok LBL, ;

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
      7 36 MOVZ,  2 7 GUARD-SPAN
      done B,
      write LBL,
      7 1 16 LSRI,  8 $3FFF LIT64,  7 7 8 AND,
      7 done CBZ,
      2 7 GUARD-SPAN
   ELSE
      8 $40000000 LIT64,  7 1 8 AND,         \ IOC_OUT: kernel writes
      7 write CBNZ,
      8 $A0000000 LIT64,  7 1 8 AND,         \ IOC_IN or IOC_VOID
      7 done CBNZ,
      trap B,
      write LBL,
      7 1 16 LSRI,  8 $1FFF LIT64,  7 7 8 AND,
      7 done CBZ,
      2 7 GUARD-SPAN
   THEN
   done B,
   trap LBL,  0 E-SEAL-VIOLATION MOVZ,  NR-EXIT-GROUP SYS,
   done LBL, ;

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
variable EVAL-DEPTH
variable EVAL-DST
variable EVAL-SCRATCH
variable EVAL-OK
variable PS-LOOP
variable PS-DONE
variable CMP-COND
variable DIV-OK
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
variable THROW-NOH
variable THROW-NOREC
variable THROW-NOREC-FB
variable THROW-NOREC-FB2
variable THROW-EVAL
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
variable NUM-LINT
variable NUM-FPOS

: LINUX-SPAWN-FAIL-N ( n -- )
   0 swap 0 ADDI,
   15 1 MOVZ,  15 SP LINUX-SPAWN-ERR-OFF STRB,
   1 SP LINUX-SPAWN-ERR-OFF ADDI,  2 1 MOVZ,  NR-WRITE SYS,
   0 127 MOVZ,  NR-EXIT-GROUP SYS, ;
s" linux-spawn-fail-n" s" n --" TRUST

: LINUX-SPAWN-FAIL ( reg -- )
   REG>N LINUX-SPAWN-FAIL-N ;
s" linux-spawn-fail" s" reg --" TRUST

: LINUX-DUP2-ARGS ( reg fd reg -- )
   REG>N LNX-ERR !
   FD>N LNX-NEW !
   REG>N LNX-FD ! ;

: LINUX-DUP2-FD ( reg fd reg -- )
   LINUX-DUP2-ARGS
   LBL {: skip:label :}
   LBL {: ok:label :}
   LNX-FD @ 0 CMPI,  C-LT skip BCOND,
   LNX-FD @ LNX-NEW @ CMPI,  C-EQ skip BCOND,
   0 LNX-FD @ 0 ADDI,  1 LNX-NEW @ MOVZ,  2 0 MOVZ,  NR-DUP2 SYS,
   9 C-CS CSET,  9 ok CBZ,
      LNX-ERR @ LINUX-SPAWN-FAIL-N
   ok LBL,
   skip LBL, ;
s" linux-dup2-fd" s" reg fd reg --" TRUST

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
s" linux-chdir-fd" s" reg reg --" TRUST

: LINUX-SETPGID-SELF ( reg -- )
   REG>N LNX-ERR !
   LBL {: ok:label :}
   0 0 MOVZ,  1 0 MOVZ,  NR-SETPGID SYS,
   9 C-CS CSET,  9 ok CBZ,
      LNX-ERR @ LINUX-SPAWN-FAIL-N
   ok LBL, ;
s" linux-setpgid-self" s" reg --" TRUST

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
   LBL {: fail:label :}
   LBL {: done:label :}
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
s" linux-spawn-parent" s" --" TRUST

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
   0 SP 0 LDR,  1 SP 8 LDR,  2 SP 16 LDR,
   NR-EXECVE SYS,
   14 SP LINUX-SPAWN-PIPE-W-OFF LDRW,
   14 >REG LINUX-SPAWN-FAIL ;
s" linux-spawn-child" s" --" TRUST

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
s" linux-spawn" s" reg reg reg reg reg reg reg --" TRUST

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
   SP SP 64 ADDI, ;

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
   NR-PIPE SYS,
   9 C-CS CSET,  9 LNX-OK LABEL@ CBZ,
      9 0 MOVN,  9 G-PUSH  9 G-PUSH  9 G-PUSH  LNX-DONE LABEL@ B,
   LNX-OK LABEL@ LBL,
   0 G-PUSH  1 G-PUSH  9 0 MOVZ,  9 G-PUSH
   LNX-DONE LABEL@ LBL, ;

: BDUP2 ( -- )                     \ ( oldfd newfd -- rc ) rc=newfd or -1
   1 G-POP  0 G-POP
   LBL LNX-OK !
   LBL LNX-DONE !
   HB-TARGET-LINUX? IF 2 0 MOVZ, THEN
   NR-DUP2 SYS,
   9 C-CS CSET,  9 LNX-OK LABEL@ CBZ,
      0 0 MOVN,  LNX-DONE LABEL@ B,
   LNX-OK LABEL@ LBL,
   LNX-DONE LABEL@ LBL,
   0 G-PUSH ;

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
s" linux-ignore-sigpipe" s" --" TRUST

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
   THEN
   NR-FCNTL SYS,
   9 C-CS CSET,  9 ok CBZ,
      0 0 MOVN,  done B,
   ok LBL,
   done LBL,
   0 G-PUSH ;

: BPOLL ( -- )                     \ ( fds nfds timeout -- rc ) rc=nready/0 or -1
   2 G-POP  1 G-POP  0 G-POP
   LBL LBL {: plen:label pguard:label :}
   6 1 61 LSRI,  6 plen CBZ,                  \ nfds*8 overflow becomes an all-address span
      6 0 MOVN,  pguard B,
   plen LBL,  6 1 3 LSLI,
   pguard LBL,  0 6 GUARD-SPAN                 \ pollfd array: nfds * 8 bytes
   LBL LNX-OK !
   LBL LNX-DONE !
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
      9 C-CS CSET,  9 LNX-OK LABEL@ CBZ,
         0 0 MOVN,  LNX-DONE LABEL@ B,
      LNX-OK LABEL@ LBL,
      LNX-DONE LABEL@ LBL,
      0 G-PUSH
      SP SP 32 ADDI,
      exit
   THEN
   NR-POLL SYS,
   9 C-CS CSET,  9 LNX-OK LABEL@ CBZ,
      0 0 MOVN,  LNX-DONE LABEL@ B,
   LNX-OK LABEL@ LBL,
   LNX-DONE LABEL@ LBL,
   0 G-PUSH ;

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

: BWAITRC ( -- )                   \ ( pid -- rc ) wait4; -1 = wait failed
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
   9 9 8 LSRI,  9 9 $FF ANDI,
   LNX-DONE LABEL@ LBL,
   9 G-PUSH
   SP SP 16 ADDI, ;

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
3584 constant SPAWN-FRAME3
2048 constant SPAWN-FRAME4-A
2048 constant SPAWN-FRAME4-B
512 constant SPAWN-FRAME4-C
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
2 constant PSFA-DUP2
5 constant PSFA-CHDIR
8 constant SPAWN-CHDIR-PATH-OFF
SPAWN-ACTION-SIZE SPAWN-CHDIR-PATH-OFF - constant SPAWN-CHDIR-PATH-CAP

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
   5 SPAWN-CHDIR-PATH-CAP MOVZ,
   SCA-COPY LABEL@ LBL,
      5 0 CMPI,  C-EQ SCA-OVER LABEL@ BCOND,
      15 16 0 LDRB,
      15 17 0 STRB,
      16 16 1 ADDI,
      17 17 1 ADDI,
      5 5 1 SUBI,
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

: SPAWN-DARWIN-ZERO-ATTR ( -- )
   14 0 MOVZ,
   0 SZA-I !
   BEGIN SZA-I @ SPAWN-ATTR-SIZE < WHILE
      14 SP SPAWN-ATTR-OFF SZA-I @ + STR,
      SZA-I @ 8 + SZA-I !
   REPEAT ;
s" spawn-darwin-zero-attr" s" --" TRUST

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
s" spawn-darwin-attr-defaults" s" --" TRUST

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
s" spawn-darwin-fill-adesc" s" --" TRUST

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
   9 C-CS CSET,  9 SFIN-OK LABEL@ CBZ,
      9 0 MOVZ,  9 9 0 SUB,  SFIN-FAIL LABEL@ B,
   SFIN-OK LABEL@ LBL,
   9 SP SPAWN-PID-OFF LDRW,
   SFIN-FAIL LABEL@ LBL,
   9 G-PUSH ;
s" spawn-darwin-finish" s" label label --" TRUST

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
   SPAWN-DARWIN-FRAME3-ENTER
   9 >REG SPAWN-DARWIN-DEFAULT-ARGV-ENVP
   3 >COUNT SPAWN-DARWIN-ACTIONS-RESET
   SPAWN-DARWIN-STDIO-ACTIONS
   SPAWN-DARWIN-ZERO-ADESC
   SPAWN-DARWIN-ZERO-ATTR
   SPAWN-DARWIN-FILL-ADESC
   9 >REG SPAWN-DARWIN-PID-PATH
   SPAWN-DARWIN-USE-ADESC
   SPAWN-DARWIN-USE-DEFAULT-ARGV-ENVP
   BSP-OK @ >LABEL BSP-DN @ >LABEL SPAWN-DARWIN-FINISH
   SPAWN-DARWIN-FRAME3-LEAVE ;

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
   SPAWN-DARWIN-FRAME3-ENTER
   SPAWN-DARWIN-DEFAULT-ENVP
   3 >COUNT SPAWN-DARWIN-ACTIONS-RESET
   SPAWN-DARWIN-STDIO-ACTIONS
   SPAWN-DARWIN-ZERO-ADESC
   SPAWN-DARWIN-ZERO-ATTR
   SPAWN-DARWIN-FILL-ADESC
   8 >REG SPAWN-DARWIN-PID-PATH
   SPAWN-DARWIN-USE-ADESC
   9 >REG SPAWN-DARWIN-ARGV-DEFAULT-ENVP
   BSP-OK @ >LABEL BSP-DN @ >LABEL SPAWN-DARWIN-FINISH
   SPAWN-DARWIN-FRAME3-LEAVE ;

: BSPAWNARGVENVIO ( -- )           \ ( pathz argvp envp stdinfd stdoutfd stderrfd -- pid|-errno )
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
   SPAWN-DARWIN-ZERO-ATTR
   SPAWN-DARWIN-FILL-ADESC
   8 >REG SPAWN-DARWIN-PID-PATH
   SPAWN-DARWIN-USE-ADESC
   9 >REG 7 >REG SPAWN-DARWIN-ARGV-ENVP
   BSP-OK @ >LABEL BSP-DN @ >LABEL SPAWN-DARWIN-FINISH
   SPAWN-DARWIN-FRAME3-LEAVE ;

: BSPAWNARGVENVCWDIO ( -- )        \ ( pathz argvp envp cwdz stdinfd stdoutfd stderrfd -- pid|-errno )
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
   SPAWN-DARWIN-ZERO-ATTR
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
: BCHECKFETCH ( -- ) 9 DATA HOOK-CELL LDR,  A G-PUSH ;  \ ( -- xt ) live checker hook — getter for set-check ([x20/DATA + HOOK-CELL])

: B-TASK-LIVE-GUARD ( -- )
   LBL {: ok:label :}
   9 DATA TASKS-LIVE-CELL LDR,  9 ok CBZ,
      0 $4F MOVZ,  NR-EXIT-GROUP SYS,
   ok LBL, ;

\ cp!/ndict! are the FORGET code-emit sinks: cp! redirects JIT emission to the
\ popped CP, ndict! points the next dict-record write at DBASE+n*DREC. Both guard
\ the address or full span each sink redirects a write to, so a post-seal value
\ landing in either sealed band fails closed at the sink (E-SEAL-VIOLATION), exactly
\ like the raw-store guards — not via the incidental word-creation bounds check.
\ Legit FORGET marks live in the code/dict region (DBASE-relative), whose region
\ offset is never inside a data-base band, so the latch-gated guard leaves them intact.
: BCPSET ( -- ) B-TASK-LIVE-GUARD  A G-POP  A GUARD-CODE-WORD  CP A 0 ADDI, ;   \ ( addr -- ) set CP — forget code back to a mark
: BNDSET ( -- ) B-TASK-LIVE-GUARD  A G-POP                                 \ ( n -- ) set NDICT — forget dict entries past a mark
   C DREC MOVZ,  B A C MUL,  B DBASE B ADD,  7 DREC MOVZ,  B 7 GUARD-SPAN
   NDICT A 0 ADDI, ;

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

\ ( a u -- ) re-entrant interpret of the string a/u in this process: save the
\ outer input cursor + compile state, point INP/INE at a/u, bump EVALD, and jump
\ to the interpret loop top (its runtime addr in LMAINP-CELL — prims can't name
\ labels). End-of-buffer (LEXIT) and an error (LUNDEF), when EVALD>0, restore the
\ depth-indexed frame and return here. Sets EVALERR-CELL: 0 = clean, 1 = recovered from an error.
: C-EVAL-FRAME-ARGS ( n n n -- )
   EVAL-SCRATCH !
   EVAL-DST !
   EVAL-DEPTH ! ;

: C-EVAL-FRAME-ADDR ( n n n -- )
   C-EVAL-FRAME-ARGS
   EVAL-DST @ EVAL-FRAME LIT64,
   EVAL-SCRATCH @ EVAL-DEPTH @ EVAL-FRAME-SHIFT LSLI,
   EVAL-DST @ EVAL-DST @ EVAL-SCRATCH @ ADD,
   EVAL-DST @ DATA EVAL-DST @ ADD, ;

: B-EVAL ( -- )
   LBL EVAL-OK !
   B-TASK-LIVE-GUARD
   B G-POP  A G-POP                                  \ x10 = u, x9 = a
   11 DATA EVALD-CELL LDR,
   12 EVAL-MAX-DEPTH MOVZ,  11 12 CMP,  C-LT EVAL-OK LABEL@ BCOND,
      BRK,
   EVAL-OK LABEL@ LBL,
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
   LBL PS-LOOP !
   LBL PS-DONE !
   9 DATA S0-CELL LDR,  9 DATA SSCR-CELL STR,
   PS-LOOP LABEL@ LBL,
      9 DATA SSCR-CELL LDR,  9 XDS CMP,  C-GE PS-DONE LABEL@ BCOND,
      9 9 0 LDR,  G-PRINT9
      9 DATA SSCR-CELL LDR,  9 9 8 ADDI,  9 DATA SSCR-CELL STR,
      PS-LOOP LABEL@ B,
   PS-DONE LABEL@ LBL, ;

: BDEPTH ( -- )
   A DATA S0-CELL LDR,
   A XDS A SUB,
   A A 3 ASRI,
   A G-PUSH ;

: (CMP) ( n -- )
   CMP-COND !  B G-POP  A G-POP  A B CMP,  A CMP-COND @ CSET,  A SP A SUB,  A G-PUSH ;

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
   LBL DIV-OK !
   B DIV-OK LABEL@ CBNZ, BRK, DIV-OK LABEL@ LBL, ;   \ SDIV by 0 silently yields 0; trap a zero divisor (B)

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
   A G-POP  A G-PUSH  A PRIM-DONE LABEL@ CBZ,  A G-PUSH  PRIM-DONE LABEL@ LBL, ;

: BFETCH ( -- )
   A G-POP  A A 0 LDR,  A G-PUSH ;

: BSTORE ( -- )
   B G-POP A G-POP  7 8 MOVZ,  B 7 GUARD-SPAN  A B 0 STR, ;

: BPTRFIELD ( -- )
   B G-POP  A G-POP  B B 3 LSLI,  A A B ADD,  A G-PUSH ;

: BPLUSSTORE ( -- )
   B G-POP A G-POP  7 8 MOVZ,  B 7 GUARD-SPAN  C B 0 LDR,  C C A ADD,  C B 0 STR, ;

: BCFETCH ( -- )
   A G-POP  A A 0 LDRB, A G-PUSH ;

: BCSTORE ( -- )
   B G-POP A G-POP  7 1 MOVZ,  B 7 GUARD-SPAN  A B 0 STRB, ;

\ Atomic primitives (ARMv8.1 LSE; Orin is ARMv8.2). A=x9 B=x10 C=x11.
: BATFETCH ( -- )   \ atomic@ ( ptr a -- a ) : LDAR x9,[x9]
   A G-POP  $C8DFFD29 EMITW  A G-PUSH ;
: BATSTORE ( -- )   \ atomic! ( a ptr a -- ) : STLR x9,[x10]
   B G-POP A G-POP  7 8 MOVZ,  B 7 GUARD-SPAN  $C89FFD49 EMITW ;
: BATADD ( -- )     \ atomic-add ( delta addr -- old ) : LDADDAL x9,x9,[x10]
   B G-POP A G-POP  7 8 MOVZ,  B 7 GUARD-SPAN  $F8E90149 EMITW  A G-PUSH ;
: BATCAS ( -- )     \ atomic-cas ( expected new addr -- actual ) : CASAL x9,x10,[x11]
   C G-POP B G-POP A G-POP  7 8 MOVZ,  C 7 GUARD-SPAN  $C8E9FD6A EMITW  A G-PUSH ;
: BFENCE ( -- )     \ fence ( -- ) : DMB ISH
   $D5033BBF EMITW ;

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
   RSTK-REG !
   14 DATA RSP-CELL LDR,
   15 14 3 LSLI,  15 DATA 15 ADD,
   RSTK-REG @ 15 RSTK-OFF STR,
   14 14 1 ADDI,  14 DATA RSP-CELL STR, ;

: RSTK-POP ( n -- )
   RSTK-REG !
   14 DATA RSP-CELL LDR,
   14 14 1 SUBI,
   15 14 3 LSLI,  15 DATA 15 ADD,
   RSTK-REG @ 15 RSTK-OFF LDR,
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
   DP-REG !
   LBL DP-LOW !
   LBL DP-HIGH !
   5 DATA-START LIT64,  5 DATA 5 ADD,
   DP-REG @ 5 CMP,  C-GE DP-LOW LABEL@ BCOND,
      0 76 MOVZ,  NR-EXIT-GROUP SYS,
   DP-LOW LABEL@ LBL,
   5 DATA-SIZE LIT64,  5 DATA 5 ADD,
   DP-REG @ 5 CMP,  C-LE DP-HIGH LABEL@ BCOND,
      0 76 MOVZ,  NR-EXIT-GROUP SYS,
   DP-HIGH LABEL@ LBL, ;

: BALLOT ( -- )
   B-TASK-LIVE-GUARD
   A G-POP  7 DATA 0 LDR,  7 7 A ADD,  7 DP-CHECK  7 DATA 0 STR, ;

: BCOMMA ( -- )
   B-TASK-LIVE-GUARD
   A G-POP  7 DATA 0 LDR,  C 7 8 ADDI,  C DP-CHECK  A 7 0 STR,  C DATA 0 STR, ;

: BCCOMMA ( -- )
   B-TASK-LIVE-GUARD
   A G-POP  7 DATA 0 LDR,  C 7 1 ADDI,  C DP-CHECK  A 7 0 STRB, C DATA 0 STR, ;

: BTYPE ( -- )
   2 G-POP  1 G-POP  0 1 MOVZ,  NR-WRITE SYS, ;

\ die ( ptr u8 n n -- ): write the message to fd 2, then exit. The requested rc is
\ honored when kernel-representable ([0,255]; 0 stays the deliberate success exit,
\ DRV-EXIT-OK); any other rc would be silently masked to `rc & 0xFF` (a negative
\ throw code re-used as rc could exit 0 - the DRV-FAIL second masked layer of the
\ BTHROW no-handler class), so it maps to the deterministic UNCAUGHT-RC instead.
: BDIE ( -- )
   LBL {: lfixed:label :}
   7 G-POP  2 G-POP  1 G-POP  0 2 MOVZ,  NR-WRITE SYS,
   0 7 0 ADDI,
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
   2 G-POP  1 G-POP  0 G-POP  1 2 GUARD-SPAN  NR-READ SYS,  SYS-PUSH ;

: BIOCTL ( -- )
   2 G-POP  1 G-POP  0 G-POP  GUARD-IOCTL  NR-IOCTL SYS,  SYS-PUSH ;

: BMMAP ( -- )
   5 G-POP  4 G-POP  3 G-POP  2 G-POP  1 G-POP  0 G-POP
   LBL {: notfixed:label :}
   6 3 $10 ANDI,  6 notfixed CBZ,            \ only MAP_FIXED replaces an existing mapping
      0 1 GUARD-SPAN                          \ x0 = address, x1 = mapping length
   notfixed LBL,
   HB-TARGET-LINUX? IF OS-MMAP-FLAGS THEN
   NR-MMAP SYS,  SYS-PUSH ; \ ( addr len prot flags fd off -- addr|-1 )

: BFORK ( -- )                     \ ( -- pid|-1 ) parent gets pid, child gets 0
   HB-TARGET-LINUX? IF
      0 17 MOVZ,  1 0 MOVZ,  2 0 MOVZ,  3 0 MOVZ,  4 0 MOVZ,
      NR-FORK SYS,  SYS-PUSH
      exit
   THEN
   LBL {: ok:label :}
   LBL {: done:label :}
   NR-FORK SYS,
   9 C-CS CSET,  9 ok CBZ,
      0 0 MOVN,  done B,
   ok LBL,
   1 done CBZ,
      0 0 MOVZ,
   done LBL,
   0 G-PUSH ;

\ ---- FFI: AAPCS64 trampolines ----
\ `ffi-call` keeps the old fast path: load 8 cells from argbuf into x0-x7,
\ BLR fn, push x0. `ffi-call-abi`/`ffi-call-abi-r` add x8, d0-d7, caller-packed
\ stack spill, and integer/float return variants for the checked lib/ffi.f API.
\ argbuf must be a >=8-cell (64-byte) buffer; trailing cells are ignored by a
\ callee that takes fewer args. XDS (x19) is AAPCS64 callee-saved so the C call
\ preserves the data stack; x30 is framed by FPRIM (these prims have a BLR).
\
\ Every foreign-call primitive is checker-restricted to explicit TRUSTED:
\ definitions. Sealed FFI bindings use ffi-call-bounded: one fixed schema entry
\ per live argument, zero for scalar/read-only, and a whole-span guard for every
\ writer before the foreign BLR. Mixed ABI uses the bounded variants below.
: BFFI-LOAD-X0-X7 ( -- )
   0 15 0  LDR,   1 15 8  LDR,   2 15 16 LDR,   3 15 24 LDR,
   4 15 32 LDR,   5 15 40 LDR,   6 15 48 LDR,   7 15 56 LDR, ;

: BFFI-CALL ( -- )
   16 G-POP                                            \ x16 = fn
   15 G-POP                                            \ x15 = argbuf
   BFFI-LOAD-X0-X7
   16 BLR,
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

\ Sealed foreign bindings carry one writable byte extent per live argument.
\ Scalars and read-only pointers use zero; writable pointers are guarded as
\ complete spans before the BLR. Preconditions: x14=nargs, x15=argbuf,
\ x17=writable-length buffer, x20=DATA.
: BFFI-GUARD-BOUNDS ( -- )
   LBL LBL {: loop:label done:label :}
   10 0 MOVZ,                                          \ x10 = i = 0
   loop LBL,
      10 14 CMP,  C-GE done BCOND,                     \ i >= nargs -> done
      11 10 3 LSLI,  11 15 11 ADD,                     \ x11 = argbuf + i*8
      9 11 0 LDR,                                      \ x9 = argbuf[i]
      11 10 3 LSLI,  11 17 11 ADD,
      6 11 0 LDR,                                      \ x6 = writable extent
      9 6 GUARD-SPAN
      10 10 1 ADDI,  loop B,
   done LBL, ;

\ ---- FFI: general AAPCS64 trampoline, any integer/pointer arity ----
\ ( argbuf nargs fn -- ret ) : x0-x7 from argbuf[0..7]; args 9..nargs spilled to
\ the stack (16-byte aligned per the ABI) by an exact runtime loop -- no arity
\ cap, no garbage slots. argbuf must hold max(8,nargs) cells. The BLR clobbers
\ caller-saved regs, so x20 (callee-saved) carries the frame sp across the call
\ to restore it afterward; the caller's x20 parks in the FPRIM frame's free
\ [sp,#8] slot. Shifted-register SUB treats r31 as XZR not SP, so sp is lowered
\ via a temp. Integer/pointer args only. Every live arg is PROT-GUARD'd before the
\ call, so this is the sound sink for sealed-band pointers (the checked FFI library
\ routes its integer/pointer calls here).
: BFFI-CALL-N-CORE ( -- )
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
         9 12 0 LDR,  9 13 0 STR,                      \ [dst] = [src]
         12 12 $8 ADDI,  13 13 $8 ADDI,               \ src++, dst++
         10 10 1 SUBI,  FFI-LOOP LABEL@ B,             \ extra--, loop
      FFI-DONE LABEL@ LBL,
   FFI-SKIP LABEL@ LBL,
   BFFI-LOAD-X0-X7
   16 BLR,
   SP 20 0 ADDI,                                       \ restore sp from x20
   20 SP $8 LDR,                                       \ restore caller x20
   0 G-PUSH ;

\ Raw unbounded FFI is checker-restricted to explicit TRUSTED: definitions.
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

\ Mixed-ABI bounded calls guard every integer register slot (x0..x8) and
\ every caller-packed stack slot before the foreign branch.  The two extent
\ tables are distinct because stack slot zero is not integer slot zero.
: BFFI-CALL-ABI-BOUNDED-CORE ( -- )
   16 G-POP                                            \ x16 = fn
   14 G-POP                                            \ x14 = stack cell count
   12 G-POP                                            \ x12 = stack extents
   11 G-POP                                            \ x11 = x0..x8 extents
   13 G-POP                                            \ x13 = prepacked stack cells
   17 G-POP                                            \ x17 = FP argbuf
   15 G-POP                                            \ x15 = integer argbuf
   SP SP $40 SUBI,
   15 SP 0 STR,  17 SP $8 STR,  13 SP $10 STR,  11 SP $18 STR,
   12 SP $20 STR,  14 SP $28 STR,  16 SP $30 STR,
   17 11 0 ADDI,  14 9 MOVZ,                          \ guard x0..x8
   BFFI-GUARD-BOUNDS
   15 SP $10 LDR,  17 SP $20 LDR,  14 SP $28 LDR,     \ guard stack slots
   BFFI-GUARD-BOUNDS
   15 SP 0 LDR,  15 G-PUSH
   17 SP $8 LDR,  17 G-PUSH
   13 SP $10 LDR,  13 G-PUSH
   14 SP $28 LDR,  14 G-PUSH
   16 SP $30 LDR,  16 G-PUSH
   SP SP $40 ADDI,
   BFFI-CALL-ABI-CORE ;

: BFFI-CALL-ABI-BOUNDED ( -- )
   BFFI-CALL-ABI-BOUNDED-CORE
   0 G-PUSH ;

: BFFI-CALL-ABI-R-BOUNDED ( -- )
   BFFI-CALL-ABI-BOUNDED-CORE
   9 0 FMOVDX,  9 G-PUSH ;

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
   2 3 GUARD-SPAN                             \ x2 = kernel-written link buffer, x3 = length
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
   STAT-BUF !
   5 STAT-BUF @ 16 LDRW,  5 STAT-BUF @ 4 STRW,
   5 STAT-BUF @ 48 LDR,   6 STAT-BUF @ 88 LDR,   7 STAT-BUF @ 96 LDR,
   8 STAT-BUF @ 104 LDR,  9 STAT-BUF @ 112 LDR,
   5 STAT-BUF @ 96 STR,   6 STAT-BUF @ 48 STR,   7 STAT-BUF @ 56 STR,
   8 STAT-BUF @ 64 STR,   9 STAT-BUF @ 72 STR, ;
s" linux-stat-fix" s" n --" TRUST

: BSTAT64 ( -- )
   1 G-POP  0 G-POP
   7 $90 MOVZ,  1 7 GUARD-SPAN               \ x1 = kernel-written 144-byte statbuf
   LBL STAT-OK !
   LBL STAT-DONE !
   HB-TARGET-LINUX? IF
      2 1 0 ADDI,  1 0 0 ADDI,  0 99 MOVN,  3 0 MOVZ,
      NR-STAT64 SYS,
      9 C-CS CSET,  9 STAT-OK LABEL@ CBZ,
         0 0 MOVN,  STAT-DONE LABEL@ B,
      STAT-OK LABEL@ LBL,
      2 LINUX-STAT-FIX
      STAT-DONE LABEL@ LBL,
      0 G-PUSH
      exit
   THEN
   NR-STAT64 SYS,  SYS-PUSH ;

: BLSTAT64 ( -- )
   1 G-POP  0 G-POP  2 0 MOVZ,  3 0 MOVZ,  4 0 MOVZ,  5 0 MOVZ,
   7 $90 MOVZ,  1 7 GUARD-SPAN               \ x1 = kernel-written 144-byte statbuf
   LBL STAT-OK !
   LBL STAT-DONE !
   HB-TARGET-LINUX? IF
      2 1 0 ADDI,  1 0 0 ADDI,  0 99 MOVN,  3 AT-SYMLINK-NOFOLLOW MOVZ,
      NR-LSTAT64 SYS,
      9 C-CS CSET,  9 STAT-OK LABEL@ CBZ,
         0 0 MOVN,  STAT-DONE LABEL@ B,
      STAT-OK LABEL@ LBL,
      2 LINUX-STAT-FIX
      STAT-DONE LABEL@ LBL,
      0 G-PUSH
      exit
   THEN
   NR-LSTAT64 SYS,  SYS-PUSH ;

: BGETDIRENTRIES64 ( -- )
   3 G-POP  2 G-POP  1 G-POP  0 G-POP
   1 2 GUARD-SPAN  7 8 MOVZ,  3 7 GUARD-SPAN
   NR-GETDIRENTRIES64 SYS,  SYS-PUSH ;

: C-FLUSH-X9-LINE ( -- )
   9 DCCVAU,  DSB-ISH,  9 ICIVAU,  DSB-ISH,  ISB, ;

: BPATCH32 ( -- )                \ ( w addr -- ): RW-flip, store, RX, cache-sync —
   A G-POP  B G-POP              \ all inside ENGINE text (a JIT-resident caller
   7 4 MOVZ,  A 7 GUARD-SPAN      \ x9 is the target; protect its exact 4-byte write
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

\ run-in-stack ( xt base size -- ) : run xt on a fresh data stack (x19=base,
\ full-ascending). Proves per-task data stacks for the threads work. size is the
\ buffer capacity (caller's guarantee of headroom); x20/region unchanged here.
: BRUNSTACK ( -- )
   C G-POP B G-POP A G-POP                        \ x11=size(unused) x10=base x9=xt
   SP SP 16 SUBI,  30 SP 0 STR,  19 SP 8 STR,     \ save lr + caller XDS(x19)
   19 10 0 ADDI,                                  \ x19 = base
   9 BLR,                                         \ run the xt on the fresh stack
   19 SP 8 LDR,  30 SP 0 LDR,  SP SP 16 ADDI, ;   \ restore XDS + lr

: BCATCH ( -- )
   LBL CATCH-RES !
   LBL CATCH-PUSH !
   A G-POP
   SP SP 48 SUBI,
   30 SP 32 STR,
   11 DATA 8 LDR,  11 SP 0 STR,
   19 SP 8 STR,
   13 SP 48 ADDI,  13 SP 16 STR,
   12 CATCH-RES LABEL@ ADR,  12 SP 24 STR,
   14 SP 0 ADDI,  14 DATA 8 STR,
   9 BLR,
   11 SP 0 LDR,  11 DATA 8 STR,
   30 SP 32 LDR,  SP SP 48 ADDI,
   9 0 MOVZ,  CATCH-PUSH LABEL@ B,
   CATCH-RES LABEL@ LBL,
   CATCH-PUSH LABEL@ LBL,  9 G-PUSH ;

\ throw ( code -- ) : unwind to the nearest catch handler (ANS semantics). When
\ EVALD>0 the throw may cross one or more active `evaluate` boundaries before it
\ reaches its handler; the loop in LEVALREC (habu2.f, reached via EVALREC-CELL since
\ a leaf prim cannot name a habu2.f label) rolls back each escaped eval frame first
\ so the handler resumes with clean compile/dictionary/input state. A checker/compile
\ error inside `evaluate` therefore stays a normal propagating throw — catchable
\ in-process without a process exit — while a catch INSIDE the evaluated source still
\ handles its own throws, and a throw with no eval frame active behaves exactly as
\ before.
: BTHROW ( -- )
   LBL THROW-NOH !  LBL THROW-EVAL !
   A G-POP  15 9 0 ADDI,                               \ x15 = code
   12 DATA EVALD-CELL LDR,  12 THROW-EVAL LABEL@ CBNZ, \ inside evaluate → LEVALREC cleans frames first
   11 DATA 8 LDR,                                      \ x11 = nearest handler (HND-CELL)
   9 15 0 ADDI,                                        \ x9 = code
   11 THROW-NOH LABEL@ CBZ,
   19 11 8 LDR,
   10 11 0 LDR,  10 DATA 8 STR,
   30 11 32 LDR,  12 11 24 LDR,  13 11 16 LDR,
   SP 13 0 ADDI,  12 BR,
   THROW-EVAL LABEL@ LBL,
   10 DATA EVALREC-CELL LDR,  10 BR,                   \ x15 = code → LEVALREC (habu2.f)
   THROW-NOH LABEL@ LBL,
   LBL THROW-NOREC !  LBL THROW-NOREC-FB !  LBL THROW-NOREC-FB2 !
   10 DATA REPLH-CELL LDR,  10 THROW-NOREC LABEL@ CBZ,   \ tty REPL: recover instead of exiting
   10 DATA RRECP-CELL LDR,  10 BR,
   THROW-NOREC LABEL@ LBL,                                \ x9 = code; no handler, no REPL
   10 DATA UNCGH-CELL LDR,  10 THROW-NOREC-FB LABEL@ CBZ, \ reporter installed? branch with x9 = code
   10 BR,                                                 \ LUNCAUGHT (habu2.f): rc-map + report out-of-range codes
   THROW-NOREC-FB LABEL@ LBL,                             \ pre-install boot fallback: silent but never masked
   0 9 0 ADDI,                                            \ code in [1,255] is kernel-representable: exit(code)
   9 1 CMPI,    C-LT THROW-NOREC-FB2 LABEL@ BCOND,
   9 255 CMPI,  C-GT THROW-NOREC-FB2 LABEL@ BCOND,
   NR-EXIT-GROUP SYS,
   THROW-NOREC-FB2 LABEL@ LBL,  0 UNCAUGHT-RC MOVZ,  NR-EXIT-GROUP SYS, ; \ else deterministic uncaught-throw rc

: BWORDLIST ( -- )
   9 DATA WIDN-CELL LDR,  9 G-PUSH  9 9 1 ADDI,  9 DATA WIDN-CELL STR, ;

: BGETCUR ( -- )
   9 DATA CUR-CELL LDR,  9 G-PUSH ;

: BSETCUR ( -- )
   A G-POP  A DATA CUR-CELL STR, ;

\ set-check ( xt -- ): install the checker hook, fail-closed at install. 0
\ disables checking (the audited `0 set-check` boundary). A non-zero argument
\ must be a live JIT code entry: DBASE <= xt < CP (x26/x28). Every real hook —
\ ' HOOK, SNAP-CHECK-HOOK, USER-HOOK, ES-VERDICT-HOOK — is a source-loaded
\ colon/TRUSTED: word JIT-compiled into [DBASE, CP), so this never rejects a
\ valid install; while garbage (1, a DATA-region address, a baked-primitive xt,
\ or a code word mis-read via `dbase@`) lies outside the window and dies here
\ with a named rc-70 diagnostic instead of BLRing into it at the next publish.
\ Limit: the window cannot tell a true word entry from any other in-range address
\ (mid-instruction, a dict record), so it catches wild installs — the crash class
\ — not a well-formed pointer that already lands inside live code.
: BSETCHECK ( -- )
   LBL LBL LBL LBL {: bad:label ok:label done:label msg:label :}
   A G-POP                               \ x9 = candidate xt
   9 ok CBZ,                             \ 0 -> checking off, install as-is
      9 DBASE CMP,  C-CC bad BCOND,      \ xt < DBASE (unsigned) -> reject
      9 CP CMP,     C-CS bad BCOND,      \ xt >= CP (unsigned) -> reject
   ok LBL,
      A DATA HOOK-CELL STR,
      done B,
   bad LBL,
      0 2 MOVZ,  1 msg ADR,  2 29 MOVZ,  NR-WRITE SYS,
      0 70 MOVZ,  NR-EXIT-GROUP SYS,
   msg LBL,  s" set-check: invalid checker xt" BYTES,
   done LBL, ;

\ TFAM 2b-iii: capture the seal-time dictionary-truncation watermark. Called from
\ SEAL-CAPTURE source tokens - the xref.f baseline plus the cold-prefix
\ assembler's token at the true engine-prefix end (habu2.f
\ EMIT-SEAL-CAPTURE-TOKEN, after script-argv.f) - always after engine
\ definitions and before any user token. The friend latch is already sealed (it is
\ set much earlier, before the engine source is evaluated), so a raw ! would trap;
\ this direct STR from NDICT (x27) is the engine's sanctioned bypass, mirroring how
\ BSETCUR/BSETCHECK update sealed crown-jewel cells. ndict only grows once the
\ engine is loaded (its records cannot be forgotten past this mark), so re-running
\ the capture is monotonic and never lowers the watermark.
: BSEALCAP ( -- )
   NDICT DATA SEAL-NDICT-CELL STR, ;

: BSEALFRIEND ( -- )
   9 FRIEND-ARENA-LEN MOVZ,  9 DATA FRIEND-LATCH-CELL STR, ;

\ wide-mark ( -- ): set DNAME-WIDE on the newest dictionary record - the word's
\ recorded effect carries a wider-than-cell layout value, so interpret-mode
\ execute/tick fail closed on it (habu2.f LWIDE; dot
\ habu-tfam-12-interpret-10b385b1). Mirrors the `immediate` flag write
\ (habu2.f C-IMMEDIATE) including the LPROT RW/RX bracket - the dict region is
\ read-only at runtime, so a raw store SIGBUSes. Engine-half marking surface;
\ the sequenced checker half calls it at signature-record time.
: BWIDEMARK ( -- )
   2 3 MOVZ,  LPROT LABEL@ BL,
   9 NDICT 0 ADDI,  9 9 1 SUBI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   10 9 16 LDR,  10 10 DNAME-WIDE ORRI,  10 9 16 STR,
   2 5 MOVZ,  LPROT LABEL@ BL, ;

: BPROTWIDADD ( -- )
   LBL LBL LBL {: room:label done:label msg:label :}
   9 G-POP
   LPROTWIDQ LABEL@ BL,
   13 done CBNZ,
   14 DATA PROT-WID-N-CELL LDR,
   14 PROT-WID-MAX CMPI,  C-LT room BCOND,
      0 2 MOVZ,  1 msg ADR,  2 28 MOVZ,  NR-WRITE SYS,    \ registry full: name the cap on fd 2 before exit 84
      0 E-SEAL-PACKAGE MOVZ,  NR-EXIT-GROUP SYS,
      msg LBL,  s" hb: protected-WID table full" BYTES,   \ 28 bytes; data reached only via ADR
   room LBL,
   15 PROT-WID-OFF MOVZ,  15 DATA 15 ADD,
   16 14 2 LSLI,  15 15 16 ADD,
   9 15 0 STRW,
   14 14 1 ADDI,  14 DATA PROT-WID-N-CELL STR,
   done LBL, ;

: BSWL ( -- )
   LBL SWL-LOOP !
   LBL SWL-END !
   LBL SWL-NEXT !
   LBL SWL-CMP !
   LBL SWL-MATCH !
   LBL SWL-F1 !
   LBL SWL-F2 !
   LBL SWL-INL !
   2 G-POP  1 G-POP  0 G-POP
   3 $20 MOVZ,  5 DBASE 0 ADDI,  6 NDICT 0 ADDI,  11 0 MOVZ,
   SWL-LOOP LABEL@ LBL,  6 SWL-END LABEL@ CBZ,
      9 5 40 LDR,  9 2 CMP,  C-NE SWL-NEXT LABEL@ BCOND,
      9 5 16 LDR,  9 9 4 LSLI,  9 9 4 LSRI,  9 1 CMP,  C-NE SWL-NEXT LABEL@ BCOND,
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
      SWL-MATCH LABEL@ LBL,  11 5 0 LDR,  SWL-NEXT LABEL@ B,
      SWL-NEXT LABEL@ LBL,  5 5 DREC ADDI,  6 6 1 SUBI,  SWL-LOOP LABEL@ B,
   SWL-END LABEL@ LBL,  11 G-PUSH ;

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
   s" @"    ['] BFETCH 1 GDEREF-L   s" !"    ['] BSTORE 2 GDEREF-L   s" ptr-field" ['] BPTRFIELD FPRIM-L
   s" +!" ['] BPLUSSTORE 2 GDEREF-L
   s" c@"   ['] BCFETCH 1 GDEREF-L  s" c!"   ['] BCSTORE 2 GDEREF-L
   s" atomic@" ['] BATFETCH 1 GDEREF-L  s" atomic!" ['] BATSTORE 2 GDEREF-L
   s" atomic-add" ['] BATADD 2 GDEREF-L  s" atomic-cas" ['] BATCAS 3 GDEREF-L  s" fence" ['] BFENCE FPRIM-L
   s" cells" ['] BCELLS FPRIM-L  s" cell+" ['] BCELLPLUS FPRIM-L
   s" chars" ['] BCHARS FPRIM-L  s" char+" ['] BCHARPLUS FPRIM-L  s" count" ['] BCOUNT 1 GDEREF-L ;

: EMIT-OUTPUT-PRIMS ( -- )
   s" ."    ['] BDOT  FPRIM-L   s" .s"   ['] B.S   FPRIM-L   s" depth" ['] BDEPTH FPRIM-L
   s" u."   ['] BU.   FPRIM-L   s" emit" ['] BEMIT FPRIM-L
   s" cr"   ['] BCR   FPRIM-L   s" space" ['] BSPACE FPRIM-L
   s" type" ['] BTYPE  2 GDEREF-L ;

: EMIT-DICT-PRIMS ( -- )
   s" here" ['] BHERE  FPRIM-L   s" allot" ['] BALLOT FPRIM-L
   s" ,"    ['] BCOMMA FPRIM-L   s" c,"   ['] BCCOMMA FPRIM-L
   s" execute" ['] BEXEC 1 GDEREF-F
   s" run-in-stack" ['] BRUNSTACK 3 GDEREF-F
   s" compile," ['] BCOMPILE FPRIM
   s" create" ['] BCREATE FPRIM
   s" parse-name" ['] BPARSE-NAME FPRIM
   s" evaluate" ['] B-EVAL FPRIM-L ;

: EMIT-PROCESS-PRIMS ( -- )
   s" run-rc" ['] BRUNRC FPRIM-L
   s" pipe" ['] BPIPE FPRIM-L   s" dup2" ['] BDUP2 FPRIM-L
   s" fcntl" ['] BFCNTL FPRIM-L   s" poll" ['] BPOLL FPRIM-L
   s" kill" ['] BKILL FPRIM-L
   s" setpgid" ['] BSETPGID FPRIM-L
   s" spawn-io" ['] BSPAWNIO FPRIM-L
   s" spawn-argv-io" ['] BSPAWNARGVIO FPRIM-L
   s" spawn-argv-env-io" ['] BSPAWNARGVENVIO FPRIM-L
   s" spawn-argv-env-cwd-io" ['] BSPAWNARGVENVCWDIO FPRIM-L
   s" fork" ['] BFORK FPRIM-L
   s" wait-rc" ['] BWAITRC FPRIM-L
   s" wait-status" ['] BWAITSTATUS FPRIM-L ;

: EMIT-ENGINE-PRIMS ( -- )
   s" cp@" ['] BCPFETCH FPRIM-L   s" dbase@" ['] BDBASEFETCH FPRIM-L
   s" data-base" ['] BDATAFETCH FPRIM-L
   s" ndict@" ['] BNDICTFETCH FPRIM-L
   s" cp!" ['] BCPSET FPRIM-L   s" ndict!" ['] BNDSET FPRIM-L
   s" SEAL-CAPTURE" ['] BSEALCAP FPRIM-L
   s" SEAL-FRIEND" ['] BSEALFRIEND FPRIM-L
   s" wide-mark" ['] BWIDEMARK FPRIM
   s" prot-wid-add" ['] BPROTWIDADD FPRIM
   s" epoch-seconds" ['] BEPOCHSECONDS FPRIM-L
   s" mono-ns" ['] BMONONS FPRIM-L
   s" die"  ['] BDIE   FPRIM-L ;

: EMIT-FS-PRIMS ( -- )
   s" open" ['] BOPEN FPRIM-L   s" write" ['] BWRITE FPRIM-L   s" read" ['] BREAD FPRIM-L   s" ioctl" ['] BIOCTL FPRIM-L
   s" mmap" ['] BMMAP FPRIM-L
   s" ffi-call" ['] BFFI-CALL FPRIM
   s" ffi-call-n" ['] BFFI-CALL-N FPRIM
   s" ffi-call-bounded" ['] BFFI-CALL-BOUNDED FPRIM
   s" ffi-call-abi-bounded" ['] BFFI-CALL-ABI-BOUNDED FPRIM
   s" ffi-call-abi-r-bounded" ['] BFFI-CALL-ABI-R-BOUNDED FPRIM
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
   s" set-check" ['] BSETCHECK FPRIM-L   s" check@" ['] BCHECKFETCH FPRIM-L ;

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
   FP-COND !  B G-POP  A G-POP  0 A FMOVXD,  1 B FMOVXD,  0 1 FCMP,
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
   LCEMIT LABEL@ LBL,
   SP SP 16 SUBI,  12 SP 0 STR,  13 SP 8 STR,
   28 GUARD-CODE-WORD
   12 SP 0 LDR,  13 SP 8 LDR,  SP SP 16 ADDI,
   9 28 0 STRW,  28 28 4 ADDI,  RET, ;

\ LBCAP ( -- ) : append TKA/TKL + ' ' to the body capture. LBCS ( x11=a x12=u )
\ is the general entry (defining-word kind tokens). FATAL (exit 71) on overflow —
\ truncation would let the check hook certify code it never saw.
: EMIT-BCAP ( -- )
   LBCAP LABEL@ LBL,
   11 DATA TKA-CELL LDR,  12 DATA TKL-CELL LDR,
   LBCS LABEL@ LBL,
   LBL BCAP-OK !
   LBL BCAP-CP !
   LBL BCAP-CD !
   LBL BCAP-GO !
   \ pass 2 re-runs the already-captured body: never re-capture (item 12 3b)
   14 DATA P2-CELL LDR,  14 BCAP-GO LABEL@ CBZ,  RET,
   BCAP-GO LABEL@ LBL,
   17 12 0 ADDI,                  \ len in x17 (IP1): callers keep state in x5-x8
   14 DATA BODYLEN-CELL LDR,
   16 14 17 ADD,  16 16 1 ADDI,
   5 BODYBUF-CAP MOVZ,  16 5 CMP,  C-LE BCAP-OK LABEL@ BCOND,
      0 2 MOVZ,  1 11 0 ADDI,  2 12 0 ADDI,  NR-WRITE SYS,
      0 71 MOVZ,  NR-EXIT-GROUP SYS,
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

: EMIT-PROT ( -- )
   LPROT LABEL@ LBL,
   0 DBASE 0 ADDI,  1 REGION LIT64,  NR-MPROTECT SYS,  RET,
   \ Runtime lowering guard: x10=address, x11=byte length.
   LBL dup LPROTSPAN ! LBL,
   10 11 GUARD-SPAN
   RET, ;

\ Protected-WID membership (TFAM 2b-v). BL routine: x9 = wid on entry, x13 = 1 if
\ wid is recorded in the protected-WID registry (PROT-WID-N-CELL entries of the
\ u32 PROT-WID-OFF table, both inside the sealed friend arena), else 0. Linear
\ scan — the registry is tiny (sealed system + generated constructor package WIDs
\ only). Clobbers x5 x6 x7 x14; x9 is preserved. Called by the sealed-WID guards
\ (record publish, AOT relocation/bootrun, snap-rebase) and the AOT registry
\ restore's dedup.
: EMIT-PROTWID ( -- )
   LBL LBL LBL {: qloop:label qnext:label qdone:label :}
   LPROTWIDQ LABEL@ LBL,
   13 0 MOVZ,                                   \ result = 0 (not protected)
   6 DATA PROT-WID-N-CELL LDR,                  \ x6 = registry count
   7 0 MOVZ,                                    \ x7 = i
   5 PROT-WID-OFF MOVZ,  5 DATA 5 ADD,          \ x5 = &table[0] (offset > imm12: materialize + add)
   qloop LBL,  7 6 CMP,  C-GE qdone BCOND,
      14 5 0 LDRW,  14 9 CMP,  C-NE qnext BCOND, \ table[i] == wid?
         13 1 MOVZ,  qdone B,                    \ found -> protected
      qnext LBL,  5 5 4 ADDI,  7 7 1 ADDI,  qloop B,
   qdone LBL,  RET, ;

: EMIT-FLUSH ( -- )
   LFLUSH LABEL@ LBL,
   LBL FL-DL !
   LBL FL-DD !
   LBL FL-IL !
   LBL FL-ID !
   9 9 6 LSRI,  9 9 6 LSLI,                                 \ align start down to the
   10 9 0 ADDI,                                             \ line, or the 64-byte
                                                            \ stride skips the last one
   FL-DL LABEL@ LBL,  10 CP CMP,  C-GE FL-DD LABEL@ BCOND,  10 DCCVAU,  10 10 64 ADDI,  FL-DL LABEL@ B,
   FL-DD LABEL@ LBL,  DSB-ISH,
   10 9 0 ADDI,
   FL-IL LABEL@ LBL,  10 CP CMP,  C-GE FL-ID LABEL@ BCOND,  10 ICIVAU,  10 10 64 ADDI,  FL-IL LABEL@ B,
   FL-ID LABEL@ LBL,  DSB-ISH,  ISB,  RET, ;

variable LHIDXADD
variable LHIDXBUILD

\ Emit the FNV-1a fold+hash of the name at reg `nr` (ptr), length `lr`,
\ into reg `hr`; clobbers c3 c4 (byte/fold scratch) and c7 (cursor). The
\ fold is the same A-Z|0x20 idiom the FIND compare uses.
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

\ Emit: insert record index x3 into table x14. The dictionary rejects
\ duplicate definitions, so the table is insert-once: probe to the first
\ empty slot or stale rolled-back slot and store index+1 (no dedupe pass). If
\ every slot has been consumed by live/stale entries, disable HIDX; linear FIND
\ and duplicate checks remain authoritative. Clobbers x2 x4 x5 x6 x7 x8 x15
\ x16 x17.
: C-HIDX-INS ( -- )
   LBL LBL LBL LBL LBL LBL {: iloop:label inext:label ifull:label idone:label iret:label rinl:label :}
   5 DREC MOVZ,  5 3 5 MUL,  5 DBASE 5 ADD,
   2 5 40 LDR,
   16 5 24 ADDI,
   15 5 16 LDR,  15 15 4 LSLI,  15 15 4 LSRI,
   4 5 16 LDR,  4 4 DNAME-EXT ANDI,  4 rinl CBZ,
      16 5 24 LDR,
   rinl LBL,
   16 15 6 4 5 7 C-HIDX-HASH
   6 6 2 EOR,  5 HIDX-SLOTS 1 - LIT64,  6 6 5 AND,
   8 HIDX-SLOTS MOVZ,
   iloop LBL,
      17 6 2 LSLI,  17 14 17 ADD,  4 17 0 LDRW,
      4 idone CBZ,
      4 4 1 SUBI,  4 NDICT CMP,  C-GE idone BCOND,
   inext LBL,
      8 8 1 SUBI,  8 ifull CBZ,
      6 6 1 ADDI,  5 HIDX-SLOTS 1 - LIT64,  6 6 5 AND,  iloop B,
   ifull LBL,
      4 0 MOVZ,  4 DATA HIDXP-CELL STR,  iret B,
   idone LBL,
      4 3 1 ADDI,  4 17 0 STRW,
   iret LBL, ;

\ C-HIDX-DUP?: x14 = live table ptr (caller ensures != 0). Sets x13 = 1 when a
\ live record with this definition's wordlist (DEF-WL-CELL) and folded name
\ (TKA/TKL) is already in the hash table, else 0, then falls through. The
\ dictionary is insert-once so at most one live match exists per chain; retired
\ records carry wid -2 and are skipped by the wid check. Same fold/compare as the
\ linear C-REJECT-DUP-DEF, so the two agree on every candidate.
: C-HIDX-DUP? ( -- )
   LBL LBL LBL LBL LBL LBL {: dloop:label dnext:label dinl:label dcmp:label dfound:label dret:label :}
   16 DATA TKA-CELL LDR,  15 DATA TKL-CELL LDR,
   16 15 3 4 5 7 C-HIDX-HASH
   4 DATA DEF-WL-CELL LDR,  6 3 4 EOR,  5 HIDX-SLOTS 1 - LIT64,  6 6 5 AND,
   13 0 MOVZ,
   8 HIDX-SLOTS MOVZ,
   dloop LBL,
      4 6 2 LSLI,  4 14 4 ADD,  3 4 0 LDRW,                  \ x3 = slot value
      3 dret CBZ,                                            \ empty slot -> no dup
      4 3 1 SUBI,  4 NDICT CMP,  C-GE dnext BCOND,           \ stale index
      5 DREC MOVZ,  5 4 5 MUL,  5 DBASE 5 ADD,               \ x5 = record ptr
      4 5 40 LDR,  15 DATA DEF-WL-CELL LDR,  4 15 CMP,  C-NE dnext BCOND,          \ wid mismatch
      4 5 16 LDR,  4 4 4 LSLI,  4 4 4 LSRI,  15 DATA TKL-CELL LDR,  4 15 CMP,  C-NE dnext BCOND,  \ len mismatch
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

\ LHIDXADD: insert the just-published record (index NDICT-1). Called
\ mid-publish, so it saves its whole clobber set. LHIDXBUILD: fresh
\ zeroed mmap (anonymous pages are zero), then add every record
\ [0,NDICT); a failed mmap is a startup failure, not a degraded mode.
: EMIT-HIDX ( -- )
   LBL LBL LBL LBL LBL {: aret:label bloop:label bdone:label bfail:label msg:label :}
   LHIDXADD LABEL@ LBL,
      SP SP 96 SUBI,
      30 SP 0 STR,  2 SP 8 STR,  3 SP 16 STR,  4 SP 24 STR,  5 SP 32 STR,
      6 SP 40 STR,  7 SP 48 STR,  14 SP 56 STR,  15 SP 64 STR,  16 SP 72 STR,  17 SP 80 STR,
      8 SP 88 STR,
      14 DATA HIDXP-CELL LDR,  14 aret CBZ,
      3 NDICT 0 ADDI,  3 3 1 SUBI,
      C-HIDX-INS
      aret LBL,
      30 SP 0 LDR,  2 SP 8 LDR,  3 SP 16 LDR,  4 SP 24 LDR,  5 SP 32 LDR,
      6 SP 40 LDR,  7 SP 48 LDR,  14 SP 56 LDR,  15 SP 64 LDR,  16 SP 72 LDR,  17 SP 80 LDR,
      8 SP 88 LDR,
      SP SP 96 ADDI,  RET,
   LHIDXBUILD LABEL@ LBL,
      \ startup runs this by BL between source setup and the interpret
      \ loop, so it must be register-transparent: save everything it or
      \ the mmap syscall can touch.
      SP SP 160 SUBI,
      30 SP 0 STR,   0 SP 8 STR,   1 SP 16 STR,  2 SP 24 STR,  3 SP 32 STR,
      4 SP 40 STR,   5 SP 48 STR,  6 SP 56 STR,  7 SP 64 STR,  8 SP 72 STR,
      13 SP 80 STR,  14 SP 88 STR, 15 SP 96 STR, 16 SP 104 STR, 17 SP 112 STR,
      0 0 MOVZ,  1 HIDX-BYTES LIT64,  2 3 MOVZ,
      3 MAP-ANON-PRIVATE LIT64,  4 0 MOVN,  5 0 MOVZ,  NR-MMAP SYS,
      4 C-CS CSET,  4 bfail CBNZ,
      14 0 0 ADDI,  14 DATA HIDXP-CELL STR,
      13 0 MOVZ,
      bloop LBL,  13 NDICT CMP,  C-GE bdone BCOND,
         3 13 0 ADDI,  C-HIDX-INS
         13 13 1 ADDI,  bloop B,
      bdone LBL,
      30 SP 0 LDR,   0 SP 8 LDR,   1 SP 16 LDR,  2 SP 24 LDR,  3 SP 32 LDR,
      4 SP 40 LDR,   5 SP 48 LDR,  6 SP 56 LDR,  7 SP 64 LDR,  8 SP 72 LDR,
      13 SP 80 LDR,  14 SP 88 LDR, 15 SP 96 LDR, 16 SP 104 LDR, 17 SP 112 LDR,
      SP SP 160 ADDI,  RET,
      bfail LBL,                                     \ dict hash-index mmap failed: label fd 2 before exit 74
         0 2 MOVZ,  1 msg ADR,  2 33 MOVZ,  NR-WRITE SYS,   \ write(2,"hb: dictionary index alloc failed",33)
         0 74 MOVZ,  NR-EXIT-GROUP SYS,
      msg LBL,  s" hb: dictionary index alloc failed" BYTES, ;

variable FIND-LINEAR
variable FIND-HLOOP
variable FIND-HNEXT
variable FIND-HINL
variable FIND-HCMP
variable FIND-HMATCH

: EMIT-FIND ( -- )
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
      5 DBASE 0 ADDI,  6 NDICT 0 ADDI,
   FIND-NLOOP LABEL@ LBL,
      6 FIND-NEND LABEL@ CBZ,
      14 5 40 LDR,  15 0 MOVN,  14 15 CMP,  C-NE FIND-NNEXT LABEL@ BCOND,
      14 5 16 LDR,  14 14 4 LSLI,  14 14 4 LSRI,  14 17 CMP,  C-NE FIND-NNEXT LABEL@ BCOND,
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
   FIND-NEND LABEL@ LBL,  RET,
   FIND-QBAD LABEL@ LBL,  RET,
   FIND-START LABEL@ LBL,
      \ hash probe (fast path): fold+hash the name once, walk the open-addressed
      \ chain for (name XOR wid). A validated slot (index<NDICT, wid==x2, name
      \ equal) returns immediately; an empty slot is a probe miss and falls
      \ through to the linear scan, which stays the authoritative fallback. x2
      \ (wid), x9/x10 (name), x13 (result) are preserved for that fallback.
      14 DATA HIDXP-CELL LDR,  14 FIND-LINEAR LABEL@ CBZ,      \ no table yet -> linear
      9 10 15 4 16 7 C-HIDX-HASH
      6 15 2 EOR,  5 HIDX-SLOTS 1 - LIT64,  6 6 5 AND,                 \ slot = (hash XOR wid) & (HIDX-SLOTS-1)
      8 HIDX-SLOTS MOVZ,
   FIND-HLOOP LABEL@ LBL,
      17 6 2 LSLI,  17 14 17 ADD,  3 17 0 LDRW,               \ x3 = slot value (index+1)
      3 FIND-LINEAR LABEL@ CBZ,                               \ empty slot -> probe miss
      4 3 1 SUBI,  4 NDICT CMP,  C-GE FIND-HNEXT LABEL@ BCOND, \ stale (truncated) index
      5 DREC MOVZ,  5 4 5 MUL,  5 DBASE 5 ADD,                \ x5 = record ptr
      16 5 40 LDR,  16 2 CMP,  C-NE FIND-HNEXT LABEL@ BCOND,  \ wid mismatch (retired=-2 / other wl)
      16 5 16 LDR,  16 16 4 LSLI,  16 16 4 LSRI,  16 10 CMP,  C-NE FIND-HNEXT LABEL@ BCOND,  \ name-len mismatch
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
      14 5 16 LDR,  14 14 4 LSLI,  14 14 4 LSRI,  14 10 CMP,  C-NE FIND-NEXT LABEL@ BCOND,
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
         14 14 DNAME-IMM ANDI,  14 14 59 LSRI,                \ immediate bit -> 2
         14 14 15 ORR,
         13 1 MOVZ,  13 13 14 ORR,  FIND-NEXT LABEL@ B,
      FIND-NEXT LABEL@ LBL,  5 5 DREC ADDI,  6 6 1 SUBI,  FIND-LOOP LABEL@ B,
   FIND-DONE LABEL@ LBL,
      13 FIND-FOUND LABEL@ CBNZ,
      14 DATA PKG-PRI-CELL LDR,  14 FIND-MISS LABEL@ CBZ,
      14 2 CMP,  C-NE FIND-TRYG LABEL@ BCOND,
         2 DATA PKG-PUB-CELL LDR,  FIND-START LABEL@ B,
      FIND-TRYG LABEL@ LBL,
      14 DATA PKG-PUB-CELL LDR,  14 2 CMP,  C-NE FIND-MISS LABEL@ BCOND,
         2 0 MOVZ,  FIND-START LABEL@ B,
      FIND-FOUND LABEL@ LBL,
      FIND-MISS LABEL@ LBL,  RET, ;

: C-NUM-INIT-REGS ( -- )
   11 0 MOVZ,  13 1 MOVZ,  14 0 MOVZ,  12 0 MOVZ,  6 10 MOVZ, ;

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
   11 11 6 MUL,  11 11 7 ADD,
   14 14 1 ADDI,  NUM-LOOP LABEL@ B, ;

: C-NUM-FRAC-STEP ( -- )
   5 10 MOVZ,  4 4 5 MUL,  4 4 7 ADD,  3 3 5 MUL,
   14 14 1 ADDI,  NUM-LOOP LABEL@ B, ;

: C-NUM-FLOAT-FINISH ( -- )
   3 1 CMPI,  C-EQ NUM-DONE LABEL@ BCOND,                       \ "1." (no frac digits) -> fail
   0 11 SCVTF,  1 4 SCVTF,  2 3 SCVTF,                          \ int, frac, scale
   1 1 2 FDIV,  0 0 1 FADD,
   13 0 CMPI,  C-GE NUM-FPOS LABEL@ BCOND,  0 0 FNEG,
   NUM-FPOS LABEL@ LBL,  11 0 FMOVDX,  12 1 MOVZ,  RET, ;

: C-NUM-INT-FINISH ( -- )
   11 11 13 MUL,  12 1 MOVZ, ;

: EMIT-NUM ( -- )
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
   LBL NUM-LINT !
   LBL NUM-FPOS !
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
   NUM-DONE LABEL@ LBL,  RET, ;

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

\ jit.fs — runtime abstract value stack for the native `:` compiler.
\ Tag 1 = constant (no code yet), tag 0 = live register; LVSPILL materializes all
\ entries as [x19] pushes bottom-up and empties the VS. State in DATA header cells.
\ Load after prof.fs, before habu2.f.
\ This is tier 0, a second ARM64 codegen beside the platform codegen (tier 1).
\ `--load` and the REPL use it because tier 1 compiles a trivial definition far
\ slower (tools/compile-floor.f); an executable build selects tier 1 for all it
\ captures (src/lib/executable-build.f), so no image contains tier-0 code.
\ The ARM64 encoders are package A64ASM's public surface (src/arch/arm64/asm.f).
require src/arch/arm64/icode.f
using A64ASM
using A64ICODE
\ The allocator's DATA cells are package REGALLOC-ABI's (src/habu/regalloc-abi.f).
using REGALLOC-ABI

variable LVSPILL   variable LVLITPUSH   variable LVPUSHC
variable LVPUSHF   variable LFFORCEK  variable LFBINPREP
variable LKWFPLUS  variable LKWFMINUS  variable LKWFSTAR  variable LKWFSLASH
variable LVTOP2C   variable LVFOLDPUT
variable LVMOVK  variable LVFORCEK  variable LVBINPREP  variable LVBINIPREP  variable LVPUSHR
variable LVNEED
 0 XDS  8 ENC-STRPOST  constant W-PUSHR   \ str xR,[x19],#8  (or with R)
 0 XDS -8 ENC-LDRPRE   constant W-POPR    \ ldr xR,[x19,#-8]!
 0 XDS  8 ENC-STRDPOST constant W-FPUSHR  \ str dR,[x19],#8  — tag 2 = FLOAT reg
 0 XDS -8 ENC-LDRDPRE  constant W-FPOPR   \ ldr dR,[x19,#-8]!
$9E670200 constant W-FMOVD16    \ fmov dR, x16  (or with R)

\ Emit one fixed instruction into the running compiler's code window.
: C-EMITW ( n -- ) 9 swap LIT64,  LCEMIT LABEL@ BL, ;

\ Emit one target syscall stencil (src/os/<target>/sys.f) into that window. The
\ seam publishes a stencil as a byte string because an x86_64 instruction is one
\ to fifteen bytes; on this ARM64 emitter every stencil is a whole number of
\ four-byte words, so a length that is not is a seam defect and dies here rather
\ than emitting a torn instruction.
: C-STENCIL-W@ ( ptr u8 n -- n ) {: a:ptr i:n :}
   a i + c@
   a i 1 + + c@ 8 lshift or
   a i 2 + + c@ 16 lshift or
   a i 3 + + c@ 24 lshift or ;

: C-EMIT-STENCIL ( ptr u8 n -- ) {: a:ptr u:n :}
   u 4 mod 0<> if
      s" jit: syscall stencil is not whole instruction words" ICODE-EXIT-RC die
   then
   u 4 / 0 ?do  a i 4 * C-STENCIL-W@ C-EMITW  loop ;

: EMIT-VLITPUSH ( -- )
   LVLITPUSH LABEL@ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   14 16 MOVZ,  LVMOVK LABEL@ BL,                            \ movz/movk x16,val (x16: never pooled)
   9 W-PUSH16 LIT64,  LCEMIT LABEL@ BL,                      \ str x16,[x19],#8
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ Emit the push of one VS entry, x7=tag x11=val: str xR / str dR, or the
\ constant through LVLITPUSH. Clobbers x5-x10, x12, x14.
: VPUSH-ENTRY, ( -- )
   LBL LBL LBL {: vcon vfr vnext :}
   7 1 CMPI,  C-EQ vcon BCOND,
   7 3 CMPI,  C-EQ vcon BCOND,
   7 2 CMPI,  C-EQ vfr BCOND,
      8 W-PUSHR LIT64,  9 8 11 ORR,  LCEMIT LABEL@ BL,        \ str xR,[x19],#8
      vnext B,
   vfr LBL,
      8 W-FPUSHR LIT64,  9 8 11 ORR,  LCEMIT LABEL@ BL,       \ str dR,[x19],#8
      vnext B,
   vcon LBL,  LVLITPUSH LABEL@ BL,
   vnext LBL, ;

: EMIT-VSPILL ( -- )
   LVSPILL LABEL@ LBL,
   LBL LBL {: vl vd :}
   SP SP 16 SUBI,  30 SP 0 STR,
   \ Empty flushes emit nothing, including outside an open code-write window.
   \ A nonempty flush is one physical transfer, including constant slots.
   6 DATA VSP-CELL LDR,  6 vd CBZ,
   5 0 MOVZ,  6 6 3 LSLI,
   5 0 MOVZ,  5 SP 8 STR,                                   \ k (in the frame: the
   vl LBL,                                                  \ helper calls clobber x5)
      5 SP 8 LDR,
      6 DATA VSP-CELL LDR,  5 6 CMP,  C-GE vd BCOND,
      7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,        \ tag[k]
      8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 LDR,   \ val[k]
      VPUSH-ENTRY,
      5 SP 8 LDR,  5 5 1 ADDI,  5 SP 8 STR,  vl B,
   vd LBL,
   6 0 MOVZ,  6 DATA VSP-CELL STR,
   6 VRALL MOVZ,  6 DATA VRFREE-CELL STR,
   6 FRALL MOVZ,  6 DATA FRFREE-CELL STR,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ LVPUSHT ( x8=tag x11=val ) : THE constant-push body — record one unmaterialized
\ VS entry, spilling first if the stack is full. Only the tag byte differs between
\ an integer and a float constant, so both entries share this one body: LVPUSHC
\ (below) sets tag 1 and falls straight in, LVPUSHF (with the float ops) sets
\ tag 3 and branches back here. x8 outlives the spill call in the frame.
variable LVPUSHT

: EMIT-VPUSHC ( -- )
   LVPUSHC LABEL@ LBL,
   8 1 MOVZ,                                   \ tag 1 = integer constant, then fall in
   LBL LVPUSHT !  LVPUSHT LABEL@ LBL,
   LBL {: room :}
   SP SP 32 SUBI,  30 SP 0 STR,  11 SP 8 STR,
   6 DATA VSP-CELL LDR,  6 VSMAX CMPI,  C-LT room BCOND,
      8 SP 16 STR,  LVSPILL LABEL@ BL,  8 SP 16 LDR,  6 0 MOVZ,
   room LBL,
   11 SP 8 LDR,
   7 6 VTAG-OFF ADDI,  7 DATA 7 ADD,  8 7 0 STRB,
   8 6 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 STR,
   6 6 1 ADDI,  6 DATA VSP-CELL STR,
   30 SP 0 LDR,  SP SP 32 ADDI,  RET, ;

\ LVTOP2C ( -- x13=ok x11=a x12=b ) : are the top two VS entries constants? (no pop)
: EMIT-VTOP2C ( -- )
   LVTOP2C LABEL@ LBL,
   LBL {: no :}
   13 0 MOVZ,
   6 DATA VSP-CELL LDR,  6 2 CMPI,  C-LT no BCOND,
   5 6 1 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,  7 1 CMPI,  C-NE no BCOND,
   5 6 2 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,  7 1 CMPI,  C-NE no BCOND,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 LDR,
   5 6 1 SUBI,  8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  12 8 0 LDR,
   13 1 MOVZ,
   no LBL,  RET, ;

\ LVFOLDPUT ( x11=result ) : val[VSP-2] = result (still con), VSP--
: EMIT-VFOLDPUT ( -- )
   LVFOLDPUT LABEL@ LBL,
   6 DATA VSP-CELL LDR,  5 6 2 SUBI,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 STR,
   6 6 1 SUBI,  6 DATA VSP-CELL STR,  RET, ;

\ Row-comparison mode. A routine that asks whether a name spells a dispatch row
\ (habu2.f EMIT-DEF-KW-GUARD, EMIT-ROW-WALK) replays the row registrars with
\ CF-DEF-GUARD set: each then emits only its spelling test, a branch to the
\ label CF-DEF-HIT holds when LKWCMP matches, in place of its handler. The mode
\ exists only in this image generator; a generated engine holds the tests.
variable CF-DEF-GUARD
variable CF-DEF-HIT

: CF-DEF-GUARD-ROW ( ptr n n -- ) {: kwvar:ptr kwlen:n :}
   0 kwvar LABEL@ ADR,  1 kwlen MOVZ,  LKWCMP LABEL@ BL,
   0 CF-DEF-HIT @ CBNZ, ;

variable LKWPLUS  variable LKWMINUS  variable LKWSTAR
variable LKWAND2  variable LKWOR2   variable LKWXOR2
variable FESK

\ fold-entry: if the token is this operator AND the top two VS entries are
\ constants, fold at JIT time (no code) and continue the main loop; else fall
\ through to the generic dispatch (which spills, then
\ inlines the body or calls the prim (habu2.f EMIT-CINLINE)).
: FOLD-ENTRY ( label ptr n n [ -- ] -- ) {: lmainlbl:label kwvar:ptr kwlen:n fxt :}
   LBL FESK !
   0 kwvar LABEL@ ADR,  1 kwlen MOVZ,  LKWCMP LABEL@ BL,
   0 FESK LABEL@ CBZ,
   LVTOP2C LABEL@ BL,  13 FESK LABEL@ CBZ,
   fxt execute
   LVFOLDPUT LABEL@ BL,
   lmainlbl B,
   FESK LABEL@ LBL, ;

: VF+ ( -- )
   11 11 12 ADD, ;   \ fold helpers -- NOT f+/f-/f*: those are the FLOAT prims

: VF- ( -- )
   11 11 12 SUB, ;

: VF* ( -- )
   11 11 12 MUL, ;

: FAND ( -- )
   11 11 12 AND, ;

: FOR2 ( -- )
   11 11 12 ORR, ;

: FXOR2 ( -- )
   11 11 12 EOR, ;

\ The keyword-string label cells are global like every other LKW* cell in this
\ file: the habu2.f LABELS allocator (its OPS block) mints them, and it is not
\ this package.
variable LKWLSH  variable LKWRSH

package ENGINE-EMIT

: EMIT-FOLDKW ( -- )
   LKWPLUS LABEL@ LBL,  s" +" BYTES,    LKWMINUS LABEL@ LBL,  s" -" BYTES,
   LKWSTAR LABEL@ LBL,  s" *" BYTES,    LKWAND2 LABEL@ LBL,   s" and" BYTES,
   LKWOR2 LABEL@ LBL,   s" or" BYTES,   LKWXOR2 LABEL@ LBL,   s" xor" BYTES,
   LKWLSH LABEL@ LBL,   s" lshift" BYTES,  LKWRSH LABEL@ LBL,  s" rshift" BYTES, ;

;package

variable LKWDUP2  variable LKWDROP2  variable LKWSWAP2  variable LKWOVER2  variable LKWNIP2

: EMIT-SHUFKW ( -- )
   LKWDUP2 LABEL@ LBL,   s" dup" BYTES,    LKWDROP2 LABEL@ LBL,  s" drop" BYTES,
   LKWSWAP2 LABEL@ LBL,  s" swap" BYTES,   LKWOVER2 LABEL@ LBL,  s" over" BYTES,
   LKWNIP2 LABEL@ LBL,   s" nip" BYTES, ;

\ LVMOVK ( x11=val x14=rd ) : emit a MINIMAL movz/movn + movk chain targeting rd —
\ movn form when $FFFF chunks dominate; chunks the base op already set are skipped.
\ x5=k x6=val x7=nz/started x8=nf/chunk x9=instr x10=form x12=$FFFF (Lcemit saves all).
: C-VMOVK-FRAME ( -- )
   SP SP 16 SUBI,  30 SP 0 STR, ;

: C-VMOVK-INIT ( -- )
   6 11 0 ADDI,  12 $FFFF MOVZ,
   7 0 MOVZ,  8 0 MOVZ,  5 0 MOVZ, ;

: C-VMOVK-COUNT-CHUNK ( -- )
   9 5 4 LSLI,  10 6 9 LSRV,  10 10 12 AND, ;

: C-VMOVK-COUNT-STEP ( label -- ) {: cl:label :}
   C-VMOVK-COUNT-CHUNK
   10 0 CMPI,  9 1 CSET,  7 7 9 ADD,
   10 12 CMP,  9 1 CSET,  8 8 9 ADD,
   5 5 1 ADDI,  cl B, ;

: C-VMOVK-FORM ( -- )
   8 7 CMP,  10 11 CSET,
   7 0 MOVZ,  5 0 MOVZ, ;

: C-VMOVK-LOAD-CHUNK ( -- )
   9 5 4 LSLI,  8 6 9 LSRV,  8 8 12 AND, ;

: C-VMOVK-SKIP? ( label -- ) {: mnext:label :}
   9 12 10 MUL,  8 9 CMP,  C-EQ mnext BCOND, ;

: C-VMOVK-INSTR-FIELDS ( -- )
   9 5 21 LSLI,  8 8 5 LSLI,  9 9 8 ORR,  9 9 14 ORR, ;

: C-VMOVK-EMIT-MOVZ ( label -- ) {: mset:label :}
   C-VMOVK-INSTR-FIELDS
   8 $D2800000 LIT64,  9 9 8 ORR,  LCEMIT LABEL@ BL,  mset B, ;

: C-VMOVK-EMIT-MOVN ( -- )
   8 8 12 EOR,
   C-VMOVK-INSTR-FIELDS
   8 $92800000 LIT64,  9 9 8 ORR,  LCEMIT LABEL@ BL, ;

: C-VMOVK-MARK-STARTED ( label -- ) {: mnext:label :}
   7 1 MOVZ,  mnext B, ;

: C-VMOVK-EMIT-MOVK ( -- )
   C-VMOVK-INSTR-FIELDS
   8 $F2800000 LIT64,  9 9 8 ORR,  LCEMIT LABEL@ BL, ;

: C-VMOVK-FALLBACK ( label label -- ) {: mz1:label mout:label :}
   7 mout CBNZ,
   10 mz1 CBNZ,
      8 $D2800000 LIT64,  9 8 14 ORR,  LCEMIT LABEL@ BL,  mout B,
   mz1 LBL,
      8 $92800000 LIT64,  9 8 14 ORR,  LCEMIT LABEL@ BL, ;

: C-VMOVK-RETURN ( -- )
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

: EMIT-VMOVK ( -- )
   LVMOVK LABEL@ LBL,
   LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL
   {: cl cd ml mk mn mset mnext md mz1 mout :}
   C-VMOVK-FRAME
   C-VMOVK-INIT
   cl LBL,
      5 4 CMPI,  C-GE cd BCOND,
      cl C-VMOVK-COUNT-STEP
   cd LBL,
   C-VMOVK-FORM
   ml LBL,
      5 4 CMPI,  C-GE md BCOND,
      C-VMOVK-LOAD-CHUNK
      mnext C-VMOVK-SKIP?
      7 mk CBNZ,
      10 mn CBNZ,
         mset C-VMOVK-EMIT-MOVZ
      mn LBL,
         C-VMOVK-EMIT-MOVN
      mset LBL,  mnext C-VMOVK-MARK-STARTED
      mk LBL,
         C-VMOVK-EMIT-MOVK
   mnext LBL,  5 5 1 ADDI,  ml B,
   md LBL,
   mz1 mout C-VMOVK-FALLBACK
   mout LBL,
   C-VMOVK-RETURN ;

\ LVFORCEK ( x5=k -- x14=reg | 0 ) : force VS entry k into a register, in place.
\ Atomic: an allocation failure mutates nothing.
: EMIT-VFORCEK ( -- )
   LVFORCEK LABEL@ LBL,
   LBL LBL {: fr fd :}
   SP SP 32 SUBI,  30 SP 0 STR,  5 SP 8 STR,
   7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 LDR,
   LBL {: f2no :}
   7 2 CMPI,  C-EQ f2no BCOND,
   7 fr CBZ,                                            \ tag 0 = already a register
   11 SP 16 STR,
   LVRALLOC LABEL@ BL,  14 fd CBZ,                           \ no reg -> 0 (nothing mutated)
   11 SP 16 LDR,  14 SP 24 STR,
   LVMOVK LABEL@ BL,                                         \ emit movz/movk rd,val
   14 SP 24 LDR,  5 SP 8 LDR,
   7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  8 0 MOVZ,  8 7 0 STRB,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  14 8 0 STR,
   fd B,
   f2no LBL,  14 11 0 ADDI,  fd B,
   fr LBL,  14 11 0 ADDI,
   fd LBL,  30 SP 0 LDR,  SP SP 32 ADDI,  RET, ;

\ LVNEED ( x5=need -- x13=ok ) : pull data-stack cells beneath the VS until it
\ holds need (1 or 2) entries. A pull claims a pool register, emits the runtime
\ pop into it and records it as entry 0 (tag 0: bits in an integer register),
\ shifting the at most one entry already there up. Each entry is complete
\ before the next allocation, so an exhausted pool returns 0 with the VS a
\ valid representation that the caller's fallback spills. Clobbers x5-x9, x14.
: EMIT-VNEED ( -- )
   LVNEED LABEL@ LBL,
   LBL LBL LBL LBL {: nl nput nok nd :}
   SP SP 16 SUBI,  30 SP 0 STR,  5 SP 8 STR,
   nl LBL,
      6 DATA VSP-CELL LDR,  5 SP 8 LDR,  6 5 CMP,  C-GE nok BCOND,
      LVRALLOC LABEL@ BL,  13 0 MOVZ,  14 nd CBZ,               \ no reg -> 0
      8 W-POPR LIT64,  9 8 14 ORR,  LCEMIT LABEL@ BL,          \ ldr xR,[x19,#-8]!
      6 DATA VSP-CELL LDR,  6 nput CBZ,
         7 DATA VTAG-OFF ADDI,  8 7 0 LDRB,  8 7 1 STRB,        \ entry 0 -> entry 1
         7 DATA VVAL-OFF ADDI,  8 7 0 LDR,  8 7 8 STR,
      nput LBL,
      7 DATA VTAG-OFF ADDI,  8 0 MOVZ,  8 7 0 STRB,
      7 DATA VVAL-OFF ADDI,  14 7 0 STR,
      6 6 1 ADDI,  6 DATA VSP-CELL STR,
      nl B,
   nok LBL,  13 1 MOVZ,
   nd LBL,  30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ LVBINPREP ( -- x13=mode ) : 0 fall-through; 1 fold (x11=a x12=b, VS untouched);
\ 2 registers ready (x14=rd result slot, x15=rm; rm freed; VSP already --).
: C-VBIN-FRAME ( -- )
   SP SP 32 SUBI,  30 SP 0 STR,
;

: C-VBIN-TAGS ( label label -- ) {: bno:label bfold:label :}
   5 2 MOVZ,  LVNEED LABEL@ BL,  13 bno CBZ,
   6 DATA VSP-CELL LDR,
   5 6 1 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   5 6 2 SUBI,  8 5 VTAG-OFF ADDI,  8 DATA 8 ADD,  8 8 0 LDRB,
   7 2 CMPI,  C-EQ bno BCOND,  8 2 CMPI,  C-EQ bno BCOND,
   7 7 8 AND,  7 1 CMPI,  C-EQ bfold BCOND, ;

: C-VBIN-REG-PATH ( label label -- ) {: bno:label bdone:label :}
   \ register path: force deep then top
   6 DATA VSP-CELL LDR,  5 6 2 SUBI,  LVFORCEK LABEL@ BL,  14 bno CBZ,
   14 SP 8 STR,
   6 DATA VSP-CELL LDR,  5 6 1 SUBI,  LVFORCEK LABEL@ BL,  14 bno CBZ,
   15 14 0 ADDI,  14 SP 8 LDR,
   \ free rm's bit; VSP--  (entry[VSP-2] already = reg rd via force-in-place)
   7 15 0 ADDI,  LVBIT LABEL@ BL,
   6 DATA VRFREE-CELL LDR,  6 6 8 ORR,  6 DATA VRFREE-CELL STR,
   6 DATA VSP-CELL LDR,  6 6 1 SUBI,  6 DATA VSP-CELL STR,
   13 2 MOVZ,  bdone B, ;

: C-VBIN-FOLD-PATH ( label -- ) {: bdone:label :}
   6 DATA VSP-CELL LDR,
   5 6 2 SUBI,  8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 LDR,
   5 6 1 SUBI,  8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  12 8 0 LDR,
   13 1 MOVZ,  bdone B, ;

: C-VBIN-RET ( -- )
   30 SP 0 LDR,  SP SP 32 ADDI,  RET, ;

: EMIT-VBINPREP ( -- )
   LVBINPREP LABEL@ LBL,
   LBL LBL LBL {: bno bfold bdone :}
   C-VBIN-FRAME
   bno bfold C-VBIN-TAGS
   bno bdone C-VBIN-REG-PATH
   bfold LBL,
   bdone C-VBIN-FOLD-PATH
   bno LBL,  13 0 MOVZ,
   bdone LBL,  C-VBIN-RET ;

\ LVBINIPREP ( x10=imm-max -- x13=mode ) : LVBINPREP plus mode 3 for top
\ nonnegative constant no greater than imm-max.
\ mode 3: x14=rd/rn for the deep operand, x15=imm12, VSP already --.
\ This routine is a PROBE ONLY: it decides mode 3 or nothing. Every other
\ outcome unwinds this frame and tail-branches into the single LVBINPREP body,
\ whose RET returns straight to our caller — so modes 0/1/2 have exactly one
\ implementation instead of a second copy here.
\ Only tag 0 (a live register) reaches the immediate lowering: tags 1 and 3 make
\ the pair foldable and tag 2 makes it mode 0, and LVBINPREP already classifies
\ all three. The probe first completes the pulls LVNEED makes, so the VS may
\ already hold pulled registers when it classifies; past them it mutates nothing
\ before it commits (the imm12 goes to this frame's scratch slot). Every failure,
\ an exhausted pool in LVNEED or LVFORCEK included, leaves the VS a valid
\ representation that the mode 0 fallback spills.
package ENGINE-EMIT

: C-VBINI-PROBE ( label label label -- ) {: bno:label b2:label bdone:label :}
   10 SP 16 STR,  5 2 MOVZ,  LVNEED LABEL@ BL,  10 SP 16 LDR,
   13 bno CBZ,
   6 DATA VSP-CELL LDR,
   5 6 1 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   7 1 CMPI,  C-NE b2 BCOND,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  12 8 0 LDR,
   12 0 CMPI,  C-LT b2 BCOND,
   12 10 CMP,  C-GT b2 BCOND,
   12 SP 8 STR,
   5 6 2 SUBI,  8 5 VTAG-OFF ADDI,  8 DATA 8 ADD,  8 8 0 LDRB,
   8 b2 CBNZ,
   LVFORCEK LABEL@ BL,  14 bno CBZ,
   15 SP 8 LDR,
   6 DATA VSP-CELL LDR,  6 6 1 SUBI,  6 DATA VSP-CELL STR,
   13 3 MOVZ,  bdone B, ;

: C-VBIN-TAIL ( -- )
   30 SP 0 LDR,  SP SP 32 ADDI,  LVBINPREP LABEL@ B, ;

: EMIT-VBINIPREP ( -- )
   LVBINIPREP LABEL@ LBL,
   LBL LBL LBL {: bno b2 bdone :}
   C-VBIN-FRAME
   bno b2 bdone C-VBINI-PROBE
   bno LBL,  13 0 MOVZ,
   bdone LBL,  C-VBIN-RET
   b2 LBL,  C-VBIN-TAIL ;

;package

\ LVPUSHR ( x14=reg ) : push a register entry (spill-on-full keeps x14 claimed)
: EMIT-VPUSHR ( -- )
   LVPUSHR LABEL@ LBL,
   LBL {: pr :}
   SP SP 16 SUBI,  30 SP 0 STR,  14 SP 8 STR,
   6 DATA VSP-CELL LDR,  6 VSMAX CMPI,  C-LT pr BCOND,
      LVSPILL LABEL@ BL,
      14 SP 8 LDR,  7 14 0 ADDI,  LVBIT LABEL@ BL,
      6 DATA VRFREE-CELL LDR,  6 6 8 EOR,  6 DATA VRFREE-CELL STR,   \ re-claim x14
   pr LBL,
   14 SP 8 LDR,  6 DATA VSP-CELL LDR,
   7 6 VTAG-OFF ADDI,  7 DATA 7 ADD,  8 0 MOVZ,  8 7 0 STRB,
   8 6 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  14 8 0 STR,
   6 6 1 ADDI,  6 DATA VSP-CELL STR,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;
variable FESK2

\ vop-entry: fold when both con, register op when forceable, else fall through
: VOP-ENTRY ( label ptr n n [ -- ] [ -- ] -- ) {: lmainlbl:label kwvar:ptr kwlen:n foldxt emitxt :}
   CF-DEF-GUARD @ IF kwvar kwlen CF-DEF-GUARD-ROW exit THEN
   LBL FESK !  LBL FESK2 !
   0 kwvar LABEL@ ADR,  1 kwlen MOVZ,  LKWCMP LABEL@ BL,
   0 FESK LABEL@ CBZ,
   LVBINPREP LABEL@ BL,
   13 FESK LABEL@ CBZ,
   13 1 CMPI,  C-NE FESK2 LABEL@ BCOND,
      foldxt execute
      LVFOLDPUT LABEL@ BL,
      lmainlbl B,
   FESK2 LABEL@ LBL,
      emitxt execute
      9 8 14 ORR,  7 14 5 LSLI,  9 9 7 ORR,  7 15 16 LSLI,  9 9 7 ORR,  LCEMIT LABEL@ BL,
      lmainlbl B,
   FESK LABEL@ LBL, ;

variable FESK6

\ vopi-entry: VOP-ENTRY plus small top-constant immediate lowering.
package ENGINE-EMIT

: VOPI-ENTRY ( label ptr n n [ -- ] [ -- ] [ -- ] n -- ) {: lmainlbl:label kwvar:ptr kwlen:n foldxt emitxt immxt max:n :}
   CF-DEF-GUARD @ IF kwvar kwlen CF-DEF-GUARD-ROW exit THEN
   LBL FESK !  LBL FESK2 !  LBL FESK6 !
   0 kwvar LABEL@ ADR,  1 kwlen MOVZ,  LKWCMP LABEL@ BL,
   0 FESK LABEL@ CBZ,
   10 max MOVZ,
   LVBINIPREP LABEL@ BL,
   13 FESK LABEL@ CBZ,
   13 1 CMPI,  C-NE FESK2 LABEL@ BCOND,
      foldxt execute
      LVFOLDPUT LABEL@ BL,
      lmainlbl B,
   FESK2 LABEL@ LBL,
   13 3 CMPI,  C-NE FESK6 LABEL@ BCOND,
      immxt execute
      lmainlbl B,
   FESK6 LABEL@ LBL,
      emitxt execute
      9 8 14 ORR,  7 14 5 LSLI,  9 9 7 ORR,  7 15 16 LSLI,  9 9 7 ORR,  LCEMIT LABEL@ BL,
      lmainlbl B,
   FESK LABEL@ LBL, ;

: FLSH ( -- )
   11 11 12 LSLV, ;

: FRSH ( -- )
   11 11 12 LSRV, ;

: E+ ( -- )
   8 $8B000000 LIT64, ;

: E- ( -- )
   8 $CB000000 LIT64, ;

: E* ( -- )
   8 $9B007C00 LIT64, ;

: EAND ( -- )
   8 $8A000000 LIT64, ;

: EOR2 ( -- )
   8 $AA000000 LIT64, ;

: EXOR ( -- )
   8 $CA000000 LIT64, ;

: ELSH ( -- )
   8 $9AC02000 LIT64, ;

: ERSH ( -- )
   8 $9AC02400 LIT64, ;

: EI2N ( -- )
   9 8 14 ORR,  7 14 5 LSLI,  9 9 7 ORR,  7 15 10 LSLI,  9 9 7 ORR,  LCEMIT LABEL@ BL, ;

: EI+ ( -- )
   8 $91000000 LIT64,  EI2N ;

: EI- ( -- )
   8 $D1000000 LIT64,  EI2N ;

: EILSH ( -- )
   8 $D3400000 LIT64,  9 8 14 ORR,  7 14 5 LSLI,  9 9 7 ORR,
   8 64 MOVZ,  8 8 15 SUB,  8 8 63 ANDI,  8 8 16 LSLI,  9 9 8 ORR,
   8 63 MOVZ,  8 8 15 SUB,  8 8 10 LSLI,  9 9 8 ORR,  LCEMIT LABEL@ BL, ;

: EIRSH ( -- )
   8 $D340FC00 LIT64,  9 8 14 ORR,  7 14 5 LSLI,  9 9 7 ORR,
   7 15 16 LSLI,  9 9 7 ORR,  LCEMIT LABEL@ BL, ;

;package

variable LKWEQ2  variable LKWNE2  variable LKWLT2  variable LKWGT2  variable LKWLE2  variable LKWGE2

\ comparison entry: fold -> dispatch computes the flag; registers -> emit
\ cmp rd,rm (cmp rd,#imm12 when the top is a constant in 0..4095) ;
\ cset rd,cond ; sub rd,xzr,rd  (Forth flag 0/-1)
: VCMP-ENTRY ( label ptr n n n -- ) {: lmainlbl:label kwvar:ptr kwlen:n cond:n :}
   CF-DEF-GUARD @ IF kwvar kwlen CF-DEF-GUARD-ROW exit THEN
   LBL FESK !  LBL FESK2 !  LBL FESK6 !  LBL {: flag :}
   0 kwvar LABEL@ ADR,  1 kwlen MOVZ,  LKWCMP LABEL@ BL,
   0 FESK LABEL@ CBZ,
   10 4095 MOVZ,
   LVBINIPREP LABEL@ BL,
   13 FESK LABEL@ CBZ,
   13 1 CMPI,  C-NE FESK2 LABEL@ BCOND,
      11 12 CMP,  11 cond CSET,  11 SP 11 SUB,
      LVFOLDPUT LABEL@ BL,
      lmainlbl B,
   FESK2 LABEL@ LBL,
   13 3 CMPI,  C-NE FESK6 LABEL@ BCOND,
      8 $F100001F LIT64,  7 14 5 LSLI,  9 8 7 ORR,  7 15 10 LSLI,  9 9 7 ORR,  LCEMIT LABEL@ BL,
      flag B,
   FESK6 LABEL@ LBL,
      8 $EB00001F LIT64,  7 14 5 LSLI,  9 8 7 ORR,  7 15 16 LSLI,  9 9 7 ORR,  LCEMIT LABEL@ BL,
   flag LBL,
      8 $9A9F07E0 cond 1 xor 12 lshift or LIT64,  9 8 14 ORR,  LCEMIT LABEL@ BL,
      8 $CB0003E0 LIT64,  9 8 14 ORR,  7 14 16 LSLI,  9 9 7 ORR,  LCEMIT LABEL@ BL,
      lmainlbl B,
   FESK LABEL@ LBL, ;

\ LVFUSE ( x14=VS top reg -- x16=b.cond word with imm19 0, or 0 ) : a leaf the
\ branch keywords (habu2.f CFB-ENTRY, CFBN-ENTRY) call on a register top. When
\ the last two words emitted are the `cset x14 ; neg x14` that VCMP-ENTRY ends a
\ compare with, it rewinds CP over them and answers the conditional branch that
\ takes the cset's condition field: that field holds the inverted condition, so
\ the branch is taken exactly when the flag would be 0, as cbz on it would be.
\ The flags hold until the branch: what the keywords emit in between (spills,
\ the BEGIN reconcile) is loads, stores and moves.
\ Fence: no open control-flow entry may record an origin at or above CP-8.
\ Only `begin` records CP without emitting, so a loop top between the compare
\ and the branch would land in the rewound words.
\ Clobbers x5, x10-x13; preserves x14 and x30.
variable LVFUSE

: EMIT-VFUSE ( -- )
   LVFUSE LABEL@ LBL,
   LBL LBL LBL {: walk chk no :}
   16 0 MOVZ,
   12 CP 8 SUBI,                                            \ x12 = CP-8
   5 CFSTK-OFF LIT64,  10 DBASE 5 ADD,  11 10 0 LDR,        \ x11 = control-flow depth
   10 10 8 ADDI,                                            \ x10 = entry 0
   walk LBL,
      11 chk CBZ,
      13 10 0 LDR,  13 12 CMP,  C-CS no BCOND,              \ origin >= CP-8: fence
      10 10 CF-REC ADDI,  11 11 1 SUBI,  walk B,
   chk LBL,
   11 12 0 LDRW,  12 12 4 LDRW,                             \ x11 = [CP-8], x12 = [CP-4]
   5 $CB0003E0 LIT64,  13 5 14 ORR,  5 14 16 LSLI,  13 13 5 ORR,
   12 13 CMP,  C-NE no BCOND,                               \ neg x14,x14
   5 $FFFF0FFF LIT64,  13 11 5 AND,
   5 $9A9F07E0 LIT64,  5 5 14 ORR,
   13 5 CMP,  C-NE no BCOND,                                \ cset x14,<any>
   CP CP 8 SUBI,
   11 11 12 LSRI,  11 11 $F ANDI,
   16 $54000000 LIT64,  16 16 11 ORR,
   no LBL,  RET, ;

: EMIT-CMPKW ( -- )
   LKWEQ2 LABEL@ LBL,  s" =" BYTES,    LKWNE2 LABEL@ LBL,  s" <>" BYTES,
   LKWLT2 LABEL@ LBL,  s" <" BYTES,    LKWGT2 LABEL@ LBL,  s" >" BYTES,
   LKWLE2 LABEL@ LBL,  s" <=" BYTES,   LKWGE2 LABEL@ LBL,  s" >=" BYTES, ;
variable LVDROP  variable LVSWAPX  variable LVNIPX  variable LVCOPY
$AA0003E0 constant W-MOVRR        \ orr rd,xzr,rs (| rd | rs<<16)

\ LVDROP ( -- x13=ok ) : drop ANY top entry (reg -> free, con -> forget); no code
: EMIT-VDROP ( -- )
   LVDROP LABEL@ LBL,
   LBL LBL {: no fr :}
   SP SP 16 SUBI,  30 SP 0 STR,
   13 0 MOVZ,
   6 DATA VSP-CELL LDR,  6 no CBZ,
   5 6 1 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   LBL {: dfr :}
   7 2 CMPI,  C-EQ dfr BCOND,
   7 fr CBNZ,
      8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 LDR,
      7 11 0 ADDI,  LVBIT LABEL@ BL,
      6 DATA VRFREE-CELL LDR,  6 6 8 ORR,  6 DATA VRFREE-CELL STR,
      6 DATA VSP-CELL LDR,  fr B,
   dfr LBL,
      8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 LDR,
      7 11 8 SUBI,  8 1 MOVZ,  8 8 7 LSLV,
      6 DATA FRFREE-CELL LDR,  6 6 8 ORR,  6 DATA FRFREE-CELL STR,
      6 DATA VSP-CELL LDR,
   fr LBL,
   6 6 1 SUBI,  6 DATA VSP-CELL STR,  13 1 MOVZ,
   no LBL,  30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ LVSWAPX ( -- x13=ok ) : swap ANY top two entries (pure relabel; no code)
: EMIT-VSWAPX ( -- )
   LVSWAPX LABEL@ LBL,
   LBL {: no :}
   13 0 MOVZ,
   6 DATA VSP-CELL LDR,  6 2 CMPI,  C-LT no BCOND,
   5 6 1 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,
   5 6 2 SUBI,  8 5 VTAG-OFF ADDI,  8 DATA 8 ADD,
   9 7 0 LDRB,  10 8 0 LDRB,  10 7 0 STRB,  9 8 0 STRB,
   5 6 1 SUBI,  7 5 3 LSLI,  7 7 VVAL-OFF ADDI,  7 DATA 7 ADD,
   5 6 2 SUBI,  8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,
   9 7 0 LDR,  10 8 0 LDR,  10 7 0 STR,  9 8 0 STR,
   13 1 MOVZ,
   no LBL,  RET, ;

\ LVNIPX ( -- x13=ok ) : remove the DEEP entry (free if reg), keep top; no code
: EMIT-VNIPX ( -- )
   LVNIPX LABEL@ LBL,
   LBL LBL {: no fr :}
   SP SP 16 SUBI,  30 SP 0 STR,
   13 0 MOVZ,
   6 DATA VSP-CELL LDR,  6 2 CMPI,  C-LT no BCOND,
   5 6 2 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   LBL {: nfr :}
   7 2 CMPI,  C-EQ nfr BCOND,
   7 fr CBNZ,
      8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 LDR,
      7 11 0 ADDI,  LVBIT LABEL@ BL,
      6 DATA VRFREE-CELL LDR,  6 6 8 ORR,  6 DATA VRFREE-CELL STR,
      6 DATA VSP-CELL LDR,  fr B,
   nfr LBL,
      8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 LDR,
      7 11 8 SUBI,  8 1 MOVZ,  8 8 7 LSLV,
      6 DATA FRFREE-CELL LDR,  6 6 8 ORR,  6 DATA FRFREE-CELL STR,
      6 DATA VSP-CELL LDR,
   fr LBL,
   5 6 1 SUBI,
   7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  9 7 0 LDRB,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  10 8 0 LDR,
   5 6 2 SUBI,
   7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  9 7 0 STRB,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  10 8 0 STR,
   6 6 1 SUBI,  6 DATA VSP-CELL STR,  13 1 MOVZ,
   no LBL,  30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ LVCOPY ( x5=k -- x13=ok ) : push a copy of entry k (con free; reg = one mov)
: EMIT-VCOPY ( -- )
   LVCOPY LABEL@ LBL,
   LBL LBL LBL {: no isreg done :}
   SP SP 32 SUBI,  30 SP 0 STR,
   13 0 MOVZ,
   7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 LDR,
   7 2 CMPI,  C-EQ no BCOND,
   LBL {: isf3 :}
   7 3 CMPI,  C-EQ isf3 BCOND,
   7 isreg CBZ,
      LVPUSHC LABEL@ BL,  13 1 MOVZ,  done B,
   isf3 LBL,
      LVPUSHF LABEL@ BL,  13 1 MOVZ,  done B,
   isreg LBL,
      11 SP 8 STR,  LVRALLOC LABEL@ BL,  14 no CBZ,  11 SP 8 LDR,
      8 W-MOVRR LIT64,  9 8 14 ORR,  7 11 16 LSLI,  9 9 7 ORR,  LCEMIT LABEL@ BL,
      LVPUSHR LABEL@ BL,
      13 1 MOVZ,  done B,
   no LBL,
   done LBL,  30 SP 0 LDR,  SP SP 32 ADDI,  RET, ;

\ ---- FLOAT ops on the VS: tag 2 = live in a d-register, tag 3 = FLOAT
\ constant (double bits, unmaterialized — LNUM's x2 flag marks float parses
\ so the loop snapshot knows to force them into d-regs, not x-regs) ----
\ LVPUSHF ( x11=bits ) : record a float constant; spill first if full.
\ Tag-3 entry wrapper for the shared LVPUSHT body (see EMIT-VPUSHC).
: EMIT-VPUSHF ( -- )
   LVPUSHF LABEL@ LBL,
   8 3 MOVZ,  LVPUSHT LABEL@ B, ;

\ LFFORCEK ( x5=k -- x14=dreg | 0 ) : force entry k into a d-reg, in place.
\ tag 2 -> already there; tag 1 (a con holding double BITS) -> materialize via
\ movz/movk x16 + fmov dN,x16; tag 0 (bits in an x-reg) -> fmov dN,xJ, freeing xJ.
\ Each returns 0, nothing mutated, when no d-reg is free.
: EMIT-FFORCEK ( -- )
   LFFORCEK LABEL@ LBL,
   LBL LBL LBL {: ffr ffd ffno :}
   SP SP 32 SUBI,  30 SP 0 STR,  5 SP 8 STR,
   7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 LDR,
   7 2 CMPI,  C-EQ ffr BCOND,
   7 ffno CBZ,                                          \ tag 0: bits in an x-reg
   11 SP 16 STR,
   LFRALLOC LABEL@ BL,  14 ffd CBZ,                          \ no d-reg -> 0
   11 SP 16 LDR,  14 SP 24 STR,
   14 16 MOVZ,  LVMOVK LABEL@ BL,                            \ bits -> x16
   14 SP 24 LDR,
   8 W-FMOVD16 LIT64,  9 8 14 ORR,  LCEMIT LABEL@ BL,        \ fmov dN, x16
   5 SP 8 LDR,
   7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  8 2 MOVZ,  8 7 0 STRB,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  14 8 0 STR,
   ffd B,
   ffno LBL,
   11 SP 16 STR,
   LFRALLOC LABEL@ BL,  14 ffd CBZ,
   11 SP 16 LDR,  14 SP 24 STR,
   8 $9E670000 LIT64,  9 8 14 ORR,  7 11 5 LSLI,  9 9 7 ORR,  LCEMIT LABEL@ BL,   \ fmov dN, xJ
   7 11 0 ADDI,  LVBIT LABEL@ BL,                            \ free the x-reg
   6 DATA VRFREE-CELL LDR,  6 6 8 ORR,  6 DATA VRFREE-CELL STR,
   14 SP 24 LDR,  5 SP 8 LDR,
   7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  8 2 MOVZ,  8 7 0 STRB,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  14 8 0 STR,
   ffd B,
   ffr LBL,  14 11 0 ADDI,
   ffd LBL,  30 SP 0 LDR,  SP SP 32 ADDI,  RET, ;

\ LFBINPREP ( -- x13=mode ) : 0 fall-through; 2 d-regs ready (x14=rd, x15=rm;
\ rm freed in FRFREE; VSP--). Mirrors LVBINPREP without a fold mode.
: EMIT-FBINPREP ( -- )
   LFBINPREP LABEL@ LBL,
   LBL LBL {: fbno fb2 :}
   SP SP 32 SUBI,  30 SP 0 STR,
   5 2 MOVZ,  LVNEED LABEL@ BL,  13 fbno CBZ,
   6 DATA VSP-CELL LDR,  5 6 2 SUBI,  LFFORCEK LABEL@ BL,  14 fbno CBZ,
   14 SP 8 STR,
   6 DATA VSP-CELL LDR,  5 6 1 SUBI,  LFFORCEK LABEL@ BL,  14 fbno CBZ,
   15 14 0 ADDI,  14 SP 8 LDR,
   7 15 8 SUBI,  8 1 MOVZ,  8 8 7 LSLV,
   6 DATA FRFREE-CELL LDR,  6 6 8 ORR,  6 DATA FRFREE-CELL STR,
   6 DATA VSP-CELL LDR,  6 6 1 SUBI,  6 DATA VSP-CELL STR,
   13 2 MOVZ,  fb2 B,
   fbno LBL,  13 0 MOVZ,
   fb2 LBL,  30 SP 0 LDR,  SP SP 32 ADDI,  RET, ;
variable FESK5

\ FOP-ENTRY: float binop keyword -> FADD-class dd,dn,dm on the d-pool; anything
\ not forceable falls through to the generic path (spills, then inlines the
\ body or calls the memory prim (habu2.f EMIT-CINLINE)).
: FOP-ENTRY ( label ptr n n n -- ) {: lmainlbl:label kwvar:ptr kwlen:n base:n :}
   CF-DEF-GUARD @ IF kwvar kwlen CF-DEF-GUARD-ROW exit THEN
   LBL FESK5 !
   0 kwvar LABEL@ ADR,  1 kwlen MOVZ,  LKWCMP LABEL@ BL,
   0 FESK5 LABEL@ CBZ,
   LFBINPREP LABEL@ BL,
   13 FESK5 LABEL@ CBZ,
   8 base LIT64,  9 8 14 ORR,  7 14 5 LSLI,  9 9 7 ORR,  7 15 16 LSLI,  9 9 7 ORR,  LCEMIT LABEL@ BL,
   lmainlbl B,
   FESK5 LABEL@ LBL, ;

: EMIT-FOPKW ( -- )
   LKWFPLUS LABEL@ LBL,  s" f+" BYTES,   LKWFMINUS LABEL@ LBL,  s" f-" BYTES,
   LKWFSTAR LABEL@ LBL,  s" f*" BYTES,   LKWFSLASH LABEL@ LBL,  s" f/" BYTES, ;

variable LVSNAP  variable LVRECON  variable LVFPOP  variable LVSETF

\ The depth cell and the frame area live in JIT-SNAP (src/habu/layout.f), an
\ appended band above DATA-START's old edge. They used to be $358 and $360; both
\ addresses now belong to NCOMP-DISPATCH, and the frame area's old extent
\ ($360..$600) had grown over LASTC/RSP/EXITH/LVD/LVH besides.
\
\ The FRAME AREA is above every 12-bit immediate form, so `ADDI` no longer
\ reaches it: SNAP-FRAME, loads the base as a 64-bit literal instead. Two extra
\ instructions per BEGIN and per back edge, at compile time.
\
\ The DEPTH CELL is deliberately still low, so it keeps the plain
\ `DATA <off> LDR/STR` form here and -- the part that matters -- can be zeroed
\ by the definition-scoped reset runs in habu2.f that already clear VSP, LVD
\ and EXITH. It is per-definition state and has to die with the definition.
: SNAP-FRAME, ( -- )              \ x7 := DATA + STK-OFF + FRAME-BYTES*x6; x8 scratch
   7 6 4 LSLI,  8 6 3 LSLI,  7 7 8 ADD,                       \ 16*sp + 8*sp = 24*sp
   8 JIT-SNAP:STK-OFF LIT64,  7 7 8 ADD,  7 DATA 7 ADD, ;

\ LVSNAP ( -- ) : BEGIN. VSP<=13: force every VS entry into a register (movz
\ chains for cons emitted HERE, before the loop top) and push (k, packed regs —
\ a byte per slot, bottom-up) on the snapshot stack. Deep VS or a failed
\ force: spill-all and push (0,0) — that loop runs memory-resident as before.
: EMIT-SNAP-FORCE-LOOP ( label label label -- ) {: fl:label fd:label fail:label :}
   5 0 MOVZ,
   fl LBL,                                  \ force entry x5 (Lvforcek saves x5)
      6 DATA VSP-CELL LDR,  5 6 CMP,  C-GE fd BCOND,
      LBL {: fif :}  LBL {: fgo :}
      7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
      7 3 CMPI,  C-EQ fif BCOND,
      LVFORCEK LABEL@ BL,  14 fail CBZ,  fgo B,
      fif LBL,  LFFORCEK LABEL@ BL,  14 fail CBZ,
      fgo LBL,  5 5 1 ADDI,  fl B, ;

: EMIT-SNAP-PACK-LOOP ( label label label label -- ) {: pl:label pd:label plo:label pnx:label :}
   12 0 MOVZ,  10 0 MOVZ,  11 0 MOVZ,
   pl LBL,
      6 DATA VSP-CELL LDR,  11 6 CMP,  C-GE pd BCOND,
      8 11 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  7 8 0 LDR,
      5 11 VTAG-OFF ADDI,  5 DATA 5 ADD,  5 5 0 LDRB,
      LBL {: pnf :}
      5 2 CMPI,  C-NE pnf BCOND,  7 7 $80 ORRI,
      pnf LBL,
      8 11 7 ANDI,  8 8 3 LSLI,  7 7 8 LSLV,
      11 8 CMPI,  C-GE plo BCOND,
         12 12 7 ORR,  pnx B,
      plo LBL,  10 10 7 ORR,
      pnx LBL,  11 11 1 ADDI,  pl B,
   pd LBL, ;

: EMIT-SNAP-FALLBACK ( label -- ) {: spush:label :}
   LVSPILL LABEL@ BL,  13 0 MOVZ,  12 0 MOVZ,  10 0 MOVZ,
   spush B, ;

: EMIT-SNAP-PUSH-FRAME ( -- )
   6 DATA JIT-SNAP:SP-CELL LDR,
   SNAP-FRAME,
   13 7 0 STR,  12 7 8 STR,  10 7 16 STR,
   6 6 1 ADDI,  6 DATA JIT-SNAP:SP-CELL STR,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

: EMIT-VSNAP ( -- )
   LVSNAP LABEL@ LBL,
   LBL LBL LBL LBL LBL LBL {: fl fd fail spush pl pd :}
   LBL {: plo :}  LBL {: pnx :}
   SP SP 16 SUBI,  30 SP 0 STR,
   6 DATA VSP-CELL LDR,  6 14 CMPI,  C-GE fail BCOND,   \ two cells pack 13 (the pool)
   fl fd fail EMIT-SNAP-FORCE-LOOP
   fd LBL,                                  \ pack: x12 = slots 0-7, x10 = slots 8+
   pl pd plo pnx EMIT-SNAP-PACK-LOOP
   6 DATA VSP-CELL LDR,  13 6 0 ADDI,  spush B,
   fail LBL,
      spush EMIT-SNAP-FALLBACK
   spush LBL,                               \ frame = (k, p0, p1); 24 B stride
   EMIT-SNAP-PUSH-FRAME ;

\ LVRECON ( -- ) : back edge (UNTIL/AGAIN/REPEAT). Pop the BEGIN frame (k slots,
\ a register byte each: bit 7 = a d-reg) and emit a parallel copy that moves
\ the VS into it, so loop-carried values stay in their BEGIN registers. With m
\ the smaller depth, VS entry j belongs in slot k-m+j:
\   A  VS deeper than the frame: push its bottom entries (as LVSPILL does).
\   B  move each register entry into its slot once no other entry reads that
\      slot; a pass that moves nothing breaks a cycle through x16.
\   C  constants: materialize each into its slot.
\   D  frame deeper than the VS: pop the slots below the entries.
\   E  LVSETF: the VS becomes the frame.
\ Pushes precede the moves because a pushed entry's register may be a slot;
\ constants and pops follow them because they write slots the moves still
\ read. An exact layout emits nothing. The code is movs, fmovs, movz/movk/movn,
\ loads and stores only: a fused `until`'s b.cond reads the flags after it.
\ Compile-time state lives in the 80-byte frame: 0 lr, 8 k, 16 p0, 24 p1,
\ 32 entry or slot index, 40 k-m (A: the pushed count), 48 progress,
\ 56 the blocked slot's byte, 64 its holder's index + 1 (0: none blocked).
$9E670000 constant W-FMOVDX     \ fmov dR, xN  (| rd | rn<<5)
$9E660000 constant W-FMOVXD     \ fmov xR, dN
$1E604000 constant W-FMOVDD     \ fmov dR, dN

: VTAG-AT, ( n n -- ) {: rd:n ri:n :}       \ x<rd> := &tag[x<ri>]
   rd ri VTAG-OFF ADDI,  rd DATA rd ADD, ;

: VVAL-AT, ( n n -- ) {: rd:n ri:n :}       \ x<rd> := &val[x<ri>]
   rd ri 3 LSLI,  rd rd VVAL-OFF ADDI,  rd DATA rd ADD, ;

\ x7 := the byte of frame slot x5, from x12 (slots 0-7) or x14 (8+); x6 scratch.
: FRAME-BYTE, ( -- )
   LBL LBL {: hi done :}
   6 5 7 ANDI,  6 6 3 LSLI,
   5 8 CMPI,  C-GE hi BCOND,
      7 12 6 LSRV,  done B,
   hi LBL,  7 14 6 LSRV,
   done LBL,  7 7 $FF ANDI, ;

\ VS entry x5 := (tag 2 if bit 7 of x<rb> else 0, x<rb> & $7F); x6 x8 scratch.
: VS-SET-REG, ( n -- ) {: rb:n :}
   6 rb 6 LSRI,  6 6 2 ANDI,  8 5 VTAG-AT,  6 8 0 STRB,
   6 rb $7F ANDI,  8 5 VVAL-AT,  6 8 0 STR, ;

\ LVFPOP ( -- x13=k x12=p0 x14=p1 ) : pop the BEGIN frame. Clobbers x6-x8.
: EMIT-VFPOP ( -- )
   LVFPOP LABEL@ LBL,
   6 DATA JIT-SNAP:SP-CELL LDR,  6 6 1 SUBI,  6 DATA JIT-SNAP:SP-CELL STR,
   SNAP-FRAME,
   13 7 0 LDR,  12 7 8 LDR,  14 7 16 LDR,  RET, ;

\ LVSETF ( x13=k x12=p0 x14=p1 -- ) : the VS := the frame, and each free mask
\ := its pool minus the frame's registers. Emits nothing; clobbers x5-x11, x15.
: EMIT-VSETF ( -- )
   LVSETF LABEL@ LBL,
   LBL LBL LBL LBL {: sl sd sint snx :}
   SP SP 16 SUBI,  30 SP 0 STR,
   13 DATA VSP-CELL STR,
   11 0 MOVZ,  15 0 MOVZ,  5 0 MOVZ,                  \ x11/x15 = claimed x/d bits
   sl LBL,
      5 13 CMP,  C-GE sd BCOND,
      FRAME-BYTE,  7 VS-SET-REG,
      6 7 $80 ANDI,  6 sint CBZ,
         6 7 $7F ANDI,  6 6 8 SUBI,  8 1 MOVZ,  8 8 6 LSLV,  15 15 8 ORR,
         snx B,
      sint LBL,  LVBIT LABEL@ BL,  11 11 8 ORR,
      snx LBL,  5 5 1 ADDI,  sl B,
   sd LBL,
   6 VRALL MOVZ,  6 6 11 EOR,  6 DATA VRFREE-CELL STR,
   6 FRALL MOVZ,  6 6 15 EOR,  6 DATA FRFREE-CELL STR,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ x11 := entry x5's source byte (bit 7 = a d-reg); a constant branches to kon.
\ Clobbers x7, x8.
: RECON-SRC, ( label -- ) {: kon:label :}
   LBL {: sint :}
   7 5 VTAG-AT,  7 7 0 LDRB,  8 5 VVAL-AT,  11 8 0 LDR,
   8 7 1 ANDI,  8 kon CBNZ,                           \ tags 1 and 3: constants
   7 sint CBZ,  11 11 $80 ORRI,
   sint LBL, ;

\ x7 := the byte of entry x5's slot, k-m + x5; x6 x12 x14 scratch, x5 kept.
: RECON-SLOT, ( -- )
   12 SP 16 LDR,  14 SP 24 LDR,  6 SP 40 LDR,  5 5 6 ADD,
   FRAME-BYTE,
   6 SP 40 LDR,  5 5 6 SUB, ;

\ Branch to found with x5 = the entry whose source byte is x15; fall through
\ when none is. Clobbers x6-x8, x11.
: RECON-HOLDER, ( label -- ) {: found:label :}
   LBL LBL LBL {: hl hd hnx :}
   5 0 MOVZ,
   hl LBL,
      6 DATA VSP-CELL LDR,  5 6 CMP,  C-GE hd BCOND,
      hnx RECON-SRC,
      11 15 CMP,  C-EQ found BCOND,
   hnx LBL,  5 5 1 ADDI,  hl B,
   hd LBL, ;

\ Emit the move into register byte x15 from register byte x13. Clobbers x6-x9.
: RECON-MOVE, ( -- )
   LBL LBL LBL LBL {: dfl xfd dfd mgo :}
   6 15 $7F ANDI,  7 13 $7F ANDI,  7 7 5 LSLI,         \ x6 = rd, x7 = rn<<5
   8 15 $80 ANDI,  8 dfl CBNZ,
      8 13 $80 ANDI,  8 xfd CBNZ,
         8 W-MOVRR LIT64,  7 7 11 LSLI,  mgo B,        \ orr's rm is at bit 16
      xfd LBL,  8 W-FMOVXD LIT64,  mgo B,
   dfl LBL,
      8 13 $80 ANDI,  8 dfd CBNZ,
         8 W-FMOVDX LIT64,  mgo B,
      dfd LBL,  8 W-FMOVDD LIT64,
   mgo LBL,
   9 8 6 ORR,  9 9 7 ORR,  LCEMIT LABEL@ BL, ;

\ Phase A: VS k' > frame k: push entries 0..k'-k-1, then shift the rest down.
: RECON-PUSH, ( -- )
   LBL LBL LBL LBL LBL {: al ash sl sd done :}
   6 DATA VSP-CELL LDR,  13 SP 8 LDR,  6 13 CMP,  C-LE done BCOND,
   6 6 13 SUB,  6 SP 40 STR,
   5 0 MOVZ,  5 SP 32 STR,
   al LBL,
      5 SP 32 LDR,  6 SP 40 LDR,  5 6 CMP,  C-GE ash BCOND,
      7 5 VTAG-AT,  7 7 0 LDRB,  8 5 VVAL-AT,  11 8 0 LDR,
      VPUSH-ENTRY,
      5 SP 32 LDR,  5 5 1 ADDI,  5 SP 32 STR,  al B,
   ash LBL,
   5 0 MOVZ,  6 SP 40 LDR,  13 SP 8 LDR,
   sl LBL,
      5 13 CMP,  C-GE sd BCOND,
      7 5 6 ADD,
      8 7 VTAG-AT,  8 8 0 LDRB,  9 5 VTAG-AT,  8 9 0 STRB,
      8 7 VVAL-AT,  8 8 0 LDR,   9 5 VVAL-AT,  8 9 0 STR,
      5 5 1 ADDI,  sl B,
   sd LBL,  13 DATA VSP-CELL STR,
   done LBL, ;

\ Phase B: passes over the entries until every register entry is in its slot.
\ An entry whose slot another entry still reads waits; a pass that moves
\ nothing saves the first waiting slot in x16, and that slot's one reader
\ (registers are owned exclusively) reads x16 instead. The broken cycle is a
\ chain, so it drains before another pass can stall.
: RECON-COPY, ( -- )
   LBL LBL LBL LBL {: bpass bl bend bdone :}  LBL LBL {: bblk bnext :}
   bpass LBL,
      5 0 MOVZ,  5 SP 32 STR,  5 SP 48 STR,  5 SP 64 STR,
   bl LBL,
      5 SP 32 LDR,  6 DATA VSP-CELL LDR,  5 6 CMP,  C-GE bend BCOND,
      bnext RECON-SRC,  13 11 0 ADDI,                  \ x13 = source
      RECON-SLOT,  15 7 0 ADDI,                        \ x15 = slot
      15 13 CMP,  C-EQ bnext BCOND,
      bblk RECON-HOLDER,
      RECON-MOVE,
      5 SP 32 LDR,  15 VS-SET-REG,
      6 1 MOVZ,  6 SP 48 STR,
      bnext B,
   bblk LBL,
      6 SP 64 LDR,  6 bnext CBNZ,
      5 5 1 ADDI,  5 SP 64 STR,  15 SP 56 STR,
   bnext LBL,
      5 SP 32 LDR,  5 5 1 ADDI,  5 SP 32 STR,  bl B,
   bend LBL,
      6 SP 64 LDR,  6 bdone CBZ,                       \ nothing waits: done
      6 SP 48 LDR,  6 bpass CBNZ,
      13 SP 56 LDR,  15 16 MOVZ,  RECON-MOVE,          \ x16 <- the slot
      5 SP 64 LDR,  5 5 1 SUBI,  15 VS-SET-REG,        \ its reader reads x16
      bpass B,
   bdone LBL, ;

\ Phase C: each constant entry into its slot; a d slot through x16.
: RECON-CONSTS, ( -- )
   LBL LBL LBL LBL {: cl cd cfl cnx :}
   5 0 MOVZ,  5 SP 32 STR,
   cl LBL,
      5 SP 32 LDR,  6 DATA VSP-CELL LDR,  5 6 CMP,  C-GE cd BCOND,
      7 5 VTAG-AT,  7 7 0 LDRB,  7 7 1 ANDI,  7 cnx CBZ,
      RECON-SLOT,  8 5 VVAL-AT,  11 8 0 LDR,
      8 7 $80 ANDI,  8 cfl CBNZ,
         14 7 0 ADDI,  LVMOVK LABEL@ BL,  cnx B,
      cfl LBL,
         7 7 $7F ANDI,  7 SP 48 STR,
         14 16 MOVZ,  LVMOVK LABEL@ BL,
         7 SP 48 LDR,  8 W-FMOVD16 LIT64,  9 8 7 ORR,  LCEMIT LABEL@ BL,
      cnx LBL,  5 SP 32 LDR,  5 5 1 ADDI,  5 SP 32 STR,  cl B,
   cd LBL, ;

\ Phase D: frame k > VS m: pop slots k-m-1 down to 0.
: RECON-POPS, ( -- )
   LBL LBL LBL LBL {: dl dd dfl dgo :}
   6 SP 40 LDR,  6 SP 32 STR,
   dl LBL,
      5 SP 32 LDR,  5 dd CBZ,  5 5 1 SUBI,  5 SP 32 STR,
      12 SP 16 LDR,  14 SP 24 LDR,  FRAME-BYTE,
      8 7 $80 ANDI,  8 dfl CBNZ,
         8 W-POPR LIT64,  dgo B,
      dfl LBL,  7 7 $7F ANDI,  8 W-FPOPR LIT64,
      dgo LBL,  9 8 7 ORR,  LCEMIT LABEL@ BL,  dl B,
   dd LBL, ;

: EMIT-VRECON ( -- )
   LVRECON LABEL@ LBL,
   SP SP 80 SUBI,  30 SP 0 STR,
   LVFPOP LABEL@ BL,
   13 SP 8 STR,  12 SP 16 STR,  14 SP 24 STR,
   RECON-PUSH,
   6 DATA VSP-CELL LDR,  13 SP 8 LDR,  13 13 6 SUB,  13 SP 40 STR,   \ k-m
   RECON-COPY,
   RECON-CONSTS,
   RECON-POPS,
   13 SP 8 LDR,  12 SP 16 LDR,  14 SP 24 LDR,
   30 SP 0 LDR,  SP SP 80 ADDI,
   LVSETF LABEL@ B, ;
variable FESK3

\ vshuf-entry: reg-aware stack ops — relabels and register moves, no memory
\ traffic beyond pulling the need operands the op reads (LVNEED). A row that
\ reads no value (drop) pulls nothing: its handler refuses an empty VS itself.
: VSHUF-ENTRY ( label ptr n n n [ -- ] -- ) {: lmainlbl:label kwvar:ptr kwlen:n need:n sxt :}
   CF-DEF-GUARD @ IF kwvar kwlen CF-DEF-GUARD-ROW exit THEN
   LBL FESK3 !
   0 kwvar LABEL@ ADR,  1 kwlen MOVZ,  LKWCMP LABEL@ BL,
   0 FESK3 LABEL@ CBZ,
   need 0<> IF  5 need MOVZ,  LVNEED LABEL@ BL,  13 FESK3 LABEL@ CBZ,  THEN
   sxt execute
   13 FESK3 LABEL@ CBZ,
   lmainlbl B,
   FESK3 LABEL@ LBL, ;

: XDUP ( -- )
   6 DATA VSP-CELL LDR,  5 6 1 SUBI,  LVCOPY LABEL@ BL, ;

: XOVER ( -- )
   6 DATA VSP-CELL LDR,  5 6 2 SUBI,  LVCOPY LABEL@ BL, ;

: XDROP ( -- )
   LVDROP LABEL@ BL, ;

: XSWAP ( -- )
   LVSWAPX LABEL@ BL, ;

: XNIP ( -- )
   LVNIPX LABEL@ BL, ;
variable LKWINC  variable LKWDEC  variable LKWZEQ  variable LKWZLT
variable LKWNEG2  variable LKWINV2
variable LKWCFETCH  variable LKWCELLS  variable LKWCELLP  variable LKWPFIELD
variable FESK4

\ vun-entry: unary op on the VS top. Tag 1 (integer constant) folds at JIT time
\ (no code); tag 0 (integer register) gets an in-place op (rd = rs, entry
\ unchanged); tags 2 and 3 (float register, float constant) fall through to the
\ prim. An empty VS first pulls the operand off the data stack (LVNEED); an
\ exhausted pool falls through.
: VUN-ENTRY ( label ptr n n [ -- ] [ -- ] -- ) {: lmainlbl:label kwvar:ptr kwlen:n foldxt emitxt :}
   CF-DEF-GUARD @ IF kwvar kwlen CF-DEF-GUARD-ROW exit THEN
   LBL FESK4 !  LBL FESK2 !
   0 kwvar LABEL@ ADR,  1 kwlen MOVZ,  LKWCMP LABEL@ BL,
   0 FESK4 LABEL@ CBZ,
   5 1 MOVZ,  LVNEED LABEL@ BL,  13 FESK4 LABEL@ CBZ,
   6 DATA VSP-CELL LDR,
   5 6 1 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 LDR,
   7 FESK2 LABEL@ CBZ,
   7 1 CMPI,  C-NE FESK4 LABEL@ BCOND,
      foldxt execute
      11 8 0 STR,
      lmainlbl B,
   FESK2 LABEL@ LBL,
      14 11 0 ADDI,
      emitxt execute
      lmainlbl B,
   FESK4 LABEL@ LBL, ;

: FU1+ ( -- )
   11 11 1 ADDI, ;

: FU1- ( -- )
   11 11 1 SUBI, ;

: FUNEG ( -- )
   11 SP 11 SUB, ;

: FUINV ( -- )
   7 0 MOVN,  11 11 7 EOR, ;

: FU0= ( -- )
   11 0 CMPI,  11 0 CSET,  11 SP 11 SUB, ;

: FU0< ( -- )
   11 0 CMPI,  11 11 CSET,  11 SP 11 SUB, ;

: EU2R ( -- )
   9 8 14 ORR,  7 14 16 LSLI,  9 9 7 ORR,  LCEMIT LABEL@ BL, ;   \ base | rd | rm<<16

: EU2N ( -- )
   9 8 14 ORR,  7 14 5 LSLI,  9 9 7 ORR,  LCEMIT LABEL@ BL, ;    \ base | rd | rn<<5

: EU1+ ( -- )
   8 $91000400 LIT64,  EU2N ;

: EU1- ( -- )
   8 $D1000400 LIT64,  EU2N ;

: EUNEG ( -- )
   8 $CB0003E0 LIT64,  EU2R ;

: EUINV ( -- )
   8 $AA2003E0 LIT64,  EU2R ;

: EUCMP0 ( -- )
   8 $F100001F LIT64,  7 14 5 LSLI,  9 8 7 ORR,  LCEMIT LABEL@ BL, ;

: EUCSET ( n -- ) {: cond:n :}
   8 $9A9F07E0 cond 1 xor 12 lshift or LIT64,
   9 8 14 ORR,
   LCEMIT LABEL@ BL, ;

: EU0= ( -- )
   EUCMP0  0 EUCSET  EUNEG ;

: EU0< ( -- )
   EUCMP0  11 EUCSET  EUNEG ;

: EMIT-UNKW ( -- )
   LKWINC LABEL@ LBL,   s" 1+" BYTES,      LKWDEC LABEL@ LBL,   s" 1-" BYTES,
   LKWZEQ LABEL@ LBL,   s" 0=" BYTES,      LKWZLT LABEL@ LBL,   s" 0<" BYTES,
   LKWNEG2 LABEL@ LBL,  s" negate" BYTES,  LKWINV2 LABEL@ LBL,  s" invert" BYTES, ;

\ ---- memory words on the VS. None of their primitive bodies carries a guard,
\ so one instruction on the operand registers is the whole word; `!` and the
\ other stores keep their guarded bodies (habu1.f PROT-GUARD:CALL).

\ vload-entry: a load from the VS top address, in place (rd = rn, result tag 0).
\ A constant address (tag 1) is forced into a register first, so the load is a
\ runtime instruction: memory is never read at compile time. Tags 2 and 3, an
\ empty pull or an exhausted pool fall through to the primitive.
: VLOAD-ENTRY ( label ptr n n n -- ) {: lmainlbl:label kwvar:ptr kwlen:n base:n :}
   CF-DEF-GUARD @ IF kwvar kwlen CF-DEF-GUARD-ROW exit THEN
   LBL FESK4 !
   0 kwvar LABEL@ ADR,  1 kwlen MOVZ,  LKWCMP LABEL@ BL,
   0 FESK4 LABEL@ CBZ,
   5 1 MOVZ,  LVNEED LABEL@ BL,  13 FESK4 LABEL@ CBZ,
   6 DATA VSP-CELL LDR,  5 6 1 SUBI,
   7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   7 2 CMPI,  C-GE FESK4 LABEL@ BCOND,
   LVFORCEK LABEL@ BL,  14 FESK4 LABEL@ CBZ,
   8 base LIT64,  EU2N
   lmainlbl B,
   FESK4 LABEL@ LBL, ;

$F9400000 constant W-LDR0        \ ldr xT,[xN]  (| rt | rn<<5)
$39400000 constant W-LDRB0       \ ldrb wT,[xN], zero-extending

: FUCELLS ( -- )
   11 11 3 LSLI, ;

: FUCELLP ( -- )
   11 11 8 ADDI, ;

: EUCELLS ( -- )
   8 $D37DF000 LIT64,  EU2N ;    \ lsl xd,xn,#3 (ubfm xd,xn,#61,#60)

: EUCELLP ( -- )
   8 $91002000 LIT64,  EU2N ;    \ add xd,xn,#8

\ ptr-field ( base index -- base+index*8 ): the slot address, not its contents.
: FPFIELD ( -- )
   12 12 3 LSLI,  11 11 12 ADD, ;

: EPFIELD ( -- )
   8 $8B000C00 LIT64, ;          \ add xd,xn,xm,lsl #3

: EMIT-MEMKW ( -- )
   LKWCFETCH LABEL@ LBL,  s" c@" BYTES,     LKWCELLS LABEL@ LBL,   s" cells" BYTES,
   LKWCELLP LABEL@ LBL,   s" cell+" BYTES,  LKWPFIELD LABEL@ LBL,  s" ptr-field" BYTES, ;

package ENGINE-EMIT

: EMIT-JIT ( -- )
   EMIT-VLITPUSH  EMIT-VSPILL  EMIT-VPUSHC  EMIT-VTOP2C  EMIT-VFOLDPUT
   EMIT-VRALLOC  EMIT-VBIT  EMIT-VRINIT  EMIT-FRALLOC  EMIT-VPUSHF  EMIT-FFORCEK  EMIT-FBINPREP  EMIT-FOPKW  EMIT-VMOVK  EMIT-VFORCEK  EMIT-VNEED  EMIT-VBINPREP  EMIT-VBINIPREP  EMIT-VPUSHR
   EMIT-VDROP  EMIT-VSWAPX  EMIT-VNIPX  EMIT-VCOPY  EMIT-VSNAP  EMIT-VFPOP  EMIT-VSETF  EMIT-VRECON  EMIT-VFUSE ;

;package

;using
;using
;using

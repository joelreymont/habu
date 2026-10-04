\ disasm.f - bounded decoder for the Intel instructions Habu emits.
\ DIS1 receives the remaining bytes of one recorded code span and its live PC.

require lib/fmt.f

package X64DIS
public
-9440 constant E-FIRST
-9449 constant E-LAST
-9440 constant E-TRUNCATED
-9441 constant E-UNKNOWN
-9442 constant E-LENGTH

0 constant FLOW-NEXT
1 constant FLOW-COND
2 constant FLOW-JUMP
3 constant FLOW-CALL
4 constant FLOW-END
5 constant FLOW-INDIRECT

private

TYPED-VARIABLE DA ptr u8
variable DU  variable DI  variable DPC
variable DREX  variable D-HASREX  variable DPFX  variable DLOCK  variable DOP
variable DMOD  variable DREG  variable DRM  variable DBASE  variable DINDEX
variable DSCALE  variable DDISP  variable DRIP
variable DPRINT  variable DDELTA

: OUT ( ptr u8 n -- ) {: a:ptr u:n :}
   DPRINT @ if a u type then ;

: CHAR-OUT ( n -- ) {: c:n :}
   DPRINT @ if c emit then ;

: GAP ( -- )
   DPRINT @ if space then ;

: INT-OUT ( n -- ) {: n:n :}
   DPRINT @ if n FMT:.INT then ;

: HEX-DIGIT ( n -- )
   dup 10 < if [char] 0 + else 10 - [char] a + then CHAR-OUT ;

: HEX-FIXED ( n -- ) {: v:n :}
   16 0 ?do v 60 i 4 * - rshift $F and HEX-DIGIT loop ;

: HEX-N ( n -- )
   dup 0 < if HEX-FIXED exit then
   dup 16 >= if dup 4 rshift RECURSE then
   $F and HEX-DIGIT ;

: HEX. ( n -- ) s" 0x" OUT HEX-N ;

: REFUSE ( n ptr u8 n -- ) {: code:n msg:ptr u:n :}
   1 DPRINT !
   s" x64dis: " OUT msg u OUT s"  at " OUT
   DPC @ DI @ + HEX. cr code throw ;

: TRUNC ( -- ) E-TRUNCATED s" truncated instruction" REFUSE ;
: UNKNOWN ( -- ) E-UNKNOWN s" unknown instruction" REFUSE ;
: LONG ( -- ) E-LENGTH s" instruction exceeds 15 bytes" REFUSE ;

: NEXT ( -- n )
   DI @ DU @ >= if TRUNC then
   DI @ 15 >= if LONG then
   DA @ DI @ + c@ DI @ 1+ DI ! ;

: IMM ( n -- n ) {: bytes:n :}
   0 bytes 0 ?do NEXT i 8 * lshift or loop ;

: SIGNED ( n n -- n ) {: value:n bits:n :}
   value 1 bits 1- lshift and 0<> if value 1 bits lshift - exit then value ;

: S8 ( -- n ) NEXT 8 SIGNED ;
: S16 ( -- n ) 2 IMM 16 SIGNED ;
: S32 ( -- n ) 4 IMM 32 SIGNED ;

: REX-W? ( -- bool ) DREX @ 8 and 0<> ;
: REX-R ( -- n ) DREX @ 4 and 0<> if 8 else 0 then ;
: REX-X ( -- n ) DREX @ 2 and 0<> if 8 else 0 then ;
: REX-B ( -- n ) DREX @ 1 and 0<> if 8 else 0 then ;

: PREFIX? ( n -- bool ) {: c:n :}
   c $66 = c $F2 = or c $F3 = or c $F0 = or
   c $40 >= c $4F <= and or ;

: PREFIXES ( -- )
   begin DI @ DU @ < if DA @ DI @ + c@ PREFIX? else false then while
      NEXT {: c:n :}
      c $40 >= c $4F <= and 0= if 0 DREX ! 0 D-HASREX ! then
      c $F0 = if 1 DLOCK ! else
      c $66 = c $F2 = or c $F3 = or if c DPFX ! else
         c $F and DREX ! 1 D-HASREX ! then then
   repeat ;

: OPCODE ( -- )
   NEXT dup $0F = if drop NEXT $100 or then DOP ! ;

: WIDTH ( -- n )
   REX-W? if 64 exit then
   DPFX @ $66 = if 16 exit then 32 ;

: MODRM ( -- )
   NEXT {: modrm:n :}
   modrm 6 rshift DMOD !
   modrm 3 rshift 7 and REX-R + DREG !
   modrm 7 and {: rm:n :}
   rm REX-B + DRM !
   -1 DBASE !  -1 DINDEX !  1 DSCALE !  0 DDISP !  0 DRIP !
   DMOD @ 3 = if exit then
   rm 4 = if
      NEXT {: sib:n :}
      sib 6 rshift 1 swap lshift DSCALE !
      sib 3 rshift 7 and {: ix:n :}
      ix 4 <> REX-X 0<> or if ix REX-X + DINDEX ! then
      sib 7 and {: base:n :}
      DMOD @ 0= base 5 = and if 4 IMM 32 SIGNED DDISP !
      else base REX-B + DBASE ! then
   else
      DMOD @ 0= rm 5 = and if 1 DRIP ! 4 IMM 32 SIGNED DDISP !
      else rm REX-B + DBASE ! then
   then
   DMOD @ 1 = if S8 DDISP ! then
   DMOD @ 2 = if S32 DDISP ! then ;

: R64$ ( n -- ptr u8 n )
   case
      0 of s" rax" endof  1 of s" rcx" endof
      2 of s" rdx" endof  3 of s" rbx" endof
      4 of s" rsp" endof  5 of s" rbp" endof
      6 of s" rsi" endof  7 of s" rdi" endof
      8 of s" r8" endof   9 of s" r9" endof
     10 of s" r10" endof 11 of s" r11" endof
     12 of s" r12" endof 13 of s" r13" endof
     14 of s" r14" endof 15 of s" r15" endof
      UNKNOWN
   endcase ;

: R32$ ( n -- ptr u8 n )
   case
      0 of s" eax" endof  1 of s" ecx" endof
      2 of s" edx" endof  3 of s" ebx" endof
      4 of s" esp" endof  5 of s" ebp" endof
      6 of s" esi" endof  7 of s" edi" endof
      8 of s" r8d" endof  9 of s" r9d" endof
     10 of s" r10d" endof 11 of s" r11d" endof
     12 of s" r12d" endof 13 of s" r13d" endof
     14 of s" r14d" endof 15 of s" r15d" endof
      UNKNOWN
   endcase ;

: R16$ ( n -- ptr u8 n )
   case
      0 of s" ax" endof  1 of s" cx" endof
      2 of s" dx" endof  3 of s" bx" endof
      4 of s" sp" endof  5 of s" bp" endof
      6 of s" si" endof  7 of s" di" endof
      8 of s" r8w" endof  9 of s" r9w" endof
     10 of s" r10w" endof 11 of s" r11w" endof
     12 of s" r12w" endof 13 of s" r13w" endof
     14 of s" r14w" endof 15 of s" r15w" endof
      UNKNOWN
   endcase ;

: R8$ ( n -- ptr u8 n )
   case
      0 of s" al" endof  1 of s" cl" endof
      2 of s" dl" endof  3 of s" bl" endof
      4 of s" spl" endof 5 of s" bpl" endof
      6 of s" sil" endof 7 of s" dil" endof
      8 of s" r8b" endof  9 of s" r9b" endof
     10 of s" r10b" endof 11 of s" r11b" endof
     12 of s" r12b" endof 13 of s" r13b" endof
     14 of s" r14b" endof 15 of s" r15b" endof
      UNKNOWN
   endcase ;

: BYTE-REG-CHECK ( n -- )
   dup 4 >= swap 7 <= and D-HASREX @ 0= and if UNKNOWN then ;

: REG. ( n n -- ) {: reg:n width:n :}
   width 64 = if reg R64$ OUT exit then
   width 32 = if reg R32$ OUT exit then
   width 16 = if reg R16$ OUT exit then
   reg BYTE-REG-CHECK
   reg R8$ OUT ;

: XMM. ( n -- ) s" xmm" OUT INT-OUT ;

: MEM. ( -- )
   [char] [ CHAR-OUT
   DRIP @ if s" rip" OUT else
      DBASE @ 0 >= if DBASE @ 64 REG. then
   then
   DINDEX @ 0 >= if
      DBASE @ 0 >= DRIP @ 0<> or if [char] + CHAR-OUT then
      DINDEX @ 64 REG.
      DSCALE @ 1 <> if [char] * CHAR-OUT DSCALE @ INT-OUT then
   then
   DDISP @ dup 0<> DBASE @ 0 < DINDEX @ 0 < and DRIP @ 0= and or if
      dup 0 < if [char] - CHAR-OUT negate else
         DBASE @ 0 >= DRIP @ 0<> or DINDEX @ 0 >= or if [char] + CHAR-OUT then
      then
      INT-OUT
   else drop then
   [char] ] CHAR-OUT ;

: MEM-WIDTH. ( n -- )
   case
      8 of s" byte ptr " OUT endof
     16 of s" word ptr " OUT endof
     32 of s" dword ptr " OUT endof
     64 of s" qword ptr " OUT endof
      UNKNOWN
   endcase ;

: RM. ( n -- ) {: width:n :}
   DMOD @ 3 = if DRM @ width REG. else width MEM-WIDTH. MEM. then ;

: XRM. ( -- )
   DMOD @ 3 = if DRM @ XMM. else MEM. then ;

: SEP ( -- ) s" , " OUT ;

: PRINT-REL ( n -- ) {: delta:n :}
   delta DDELTA !
   DPC @ DI @ + delta + HEX. ;

: INT-PREFIX ( -- )
   DPFX @ 0= DPFX @ $66 = or DLOCK @ 0= and 0= if UNKNOWN then ;

: PLAIN-PREFIX ( -- )
   DPFX @ 0<> DLOCK @ 0<> or if UNKNOWN then ;

: ALU$ ( n -- ptr u8 n )
   case
      0 of s" add" endof 1 of s" or" endof
      2 of s" adc" endof 3 of s" sbb" endof
      4 of s" and" endof 5 of s" sub" endof
      6 of s" xor" endof 7 of s" cmp" endof
      UNKNOWN
   endcase ;

: SHIFT$ ( n -- ptr u8 n )
   case
      0 of s" rol" endof 1 of s" ror" endof
      4 of s" shl" endof 5 of s" shr" endof
      7 of s" sar" endof
      UNKNOWN
   endcase ;

: JCC$ ( n -- ptr u8 n )
   case
      0 of s" jo" endof   1 of s" jno" endof
      2 of s" jb" endof   3 of s" jae" endof
      4 of s" je" endof   5 of s" jne" endof
      6 of s" jbe" endof  7 of s" ja" endof
      8 of s" js" endof   9 of s" jns" endof
     10 of s" jp" endof  11 of s" jnp" endof
     12 of s" jl" endof  13 of s" jge" endof
     14 of s" jle" endof 15 of s" jg" endof
     UNKNOWN
   endcase ;

: CMOV$ ( n -- ptr u8 n )
   case
      0 of s" cmovo" endof  1 of s" cmovno" endof
      2 of s" cmovb" endof  3 of s" cmovae" endof
      4 of s" cmove" endof  5 of s" cmovne" endof
      6 of s" cmovbe" endof 7 of s" cmova" endof
      8 of s" cmovs" endof  9 of s" cmovns" endof
     10 of s" cmovp" endof 11 of s" cmovnp" endof
     12 of s" cmovl" endof 13 of s" cmovge" endof
     14 of s" cmovle" endof 15 of s" cmovg" endof
     UNKNOWN
   endcase ;

: SET$ ( n -- ptr u8 n )
   case
      0 of s" seto" endof  1 of s" setno" endof
      2 of s" setb" endof  3 of s" setae" endof
      4 of s" sete" endof  5 of s" setne" endof
      6 of s" setbe" endof 7 of s" seta" endof
      8 of s" sets" endof  9 of s" setns" endof
     10 of s" setp" endof 11 of s" setnp" endof
     12 of s" setl" endof 13 of s" setge" endof
     14 of s" setle" endof 15 of s" setg" endof
     UNKNOWN
   endcase ;

: IMM. ( n -- ) HEX. ;

: SIMM. ( n -- )
   dup 0 < if [char] - CHAR-OUT negate then IMM. ;

: REG-RM ( ptr u8 n n -- ) {: name:ptr u:n width:n :}
   name u OUT GAP DREG @ width REG. SEP width RM. ;

: RM-REG ( ptr u8 n n -- ) {: name:ptr u:n width:n :}
   name u OUT GAP width RM. SEP DREG @ width REG. ;

: ALU-REG ( -- )
   INT-PREFIX MODRM
   DOP @ 3 rshift ALU$ WIDTH {: name:ptr u:n width:n :}
   DOP @ 7 and 1 = if name u width RM-REG else name u width REG-RM then ;

: ALU-IMM ( -- )
   INT-PREFIX MODRM
   DREG @ ALU$ {: name:ptr u:n :}
   DOP @ $83 = if S8 else WIDTH 16 = if S16 else S32 then then {: value:n :}
   name u OUT GAP WIDTH RM. SEP value SIMM. ;

: MOVE ( -- )
   INT-PREFIX MODRM
   DOP @ $88 = DOP @ $8A = or if 8 else WIDTH then {: width:n :}
   width 8 = if
      DREG @ BYTE-REG-CHECK
      DMOD @ 3 = if DRM @ BYTE-REG-CHECK then
   then
   DOP @ $8A = DOP @ $8B = or if s" mov" width REG-RM
   else s" mov" width RM-REG then ;

: MOVE-IMM ( -- )
   INT-PREFIX MODRM
   DREG @ 0<> if UNKNOWN then
   WIDTH 16 = if 2 IMM else 4 IMM then {: value:n :}
   s" mov" OUT GAP WIDTH RM. SEP
   WIDTH 64 = if value 32 SIGNED SIMM. else value IMM. then ;

: MOVE-B8 ( -- )
   INT-PREFIX
   DOP @ $B8 - REX-B + {: reg:n :}
   WIDTH 64 = if 8 else WIDTH 16 = if 2 else 4 then then IMM {: value:n :}
   s" mov" OUT GAP reg WIDTH REG. SEP value IMM. ;

: BRANCH ( -- )
   PLAIN-PREFIX
   DOP @ $E8 = if S32 s" call" else
   DOP @ $E9 = if S32 s" jmp" else S8 s" jmp" then then
   {: delta:n name:ptr u:n :}
   name u OUT GAP delta PRINT-REL ;

: COND-BRANCH ( -- )
   PLAIN-PREFIX
   DOP @ $100 and 0<> if S32 else S8 then {: delta:n :}
   DOP @ $F and JCC$ OUT GAP delta PRINT-REL ;

: ALU? ( -- bool )
   DOP @ $40 < DOP @ 7 and dup 1 = swap 3 = or and ;

: DECODE-ONE ( -- bool )
   DOP @ $C3 = if PLAIN-PREFIX s" ret" OUT true exit then
   DOP @ $90 = if
      DLOCK @ 0<> if UNKNOWN then
      DREX @ 7 and 0<> if UNKNOWN then
      DPFX @ $F3 = if s" pause" OUT else
         DPFX @ 0<> if UNKNOWN then s" nop" OUT then true exit then
   DOP @ $99 = if
      INT-PREFIX REX-W? if s" cqo" else
         DPFX @ $66 = if s" cwd" else s" cdq" then then OUT true exit then
   DOP @ $50 >= DOP @ $5F <= and if
      PLAIN-PREFIX
      DOP @ $58 < if s" push " else s" pop " then OUT
      DOP @ 7 and REX-B + 64 REG. true exit then
   DOP @ $B8 >= DOP @ $BF <= and if MOVE-B8 true exit then
   DOP @ $E8 = DOP @ $E9 = or DOP @ $EB = or if BRANCH true exit then
   DOP @ $70 >= DOP @ $7F <= and if COND-BRANCH true exit then
   false ;

: DECODE-GROUP ( -- bool )
   ALU? if ALU-REG true exit then
   DOP @ $81 = DOP @ $83 = or if ALU-IMM true exit then
   DOP @ $88 >= DOP @ $8B <= and if MOVE true exit then
   DOP @ $C7 = if MOVE-IMM true exit then
   DOP @ $85 = if INT-PREFIX MODRM s" test" WIDTH RM-REG true exit then
   DOP @ $87 = if INT-PREFIX MODRM s" xchg" WIDTH RM-REG true exit then
   DOP @ $8D = if
      INT-PREFIX MODRM DMOD @ 3 = if UNKNOWN then
      s" lea" OUT GAP DREG @ WIDTH REG. SEP MEM. true exit then
   DOP @ $63 = if
      INT-PREFIX MODRM REX-W? 0= if UNKNOWN then
      s" movsxd" OUT GAP DREG @ 64 REG. SEP 32 RM. true exit then
   DOP @ $69 = DOP @ $6B = or if
      INT-PREFIX MODRM
      DOP @ $69 = if WIDTH 16 = if S16 else S32 then else S8 then {: value:n :}
      s" imul" OUT GAP DREG @ WIDTH REG. SEP WIDTH RM. SEP value SIMM.
      true exit then
   false ;

: SHIFT-GROUP ( -- )
   INT-PREFIX MODRM
   DREG @ SHIFT$ OUT GAP WIDTH RM. SEP
   DOP @ $C1 = if NEXT IMM. else s" cl" OUT then ;

: F7-GROUP ( -- )
   INT-PREFIX MODRM
   DREG @ 0 = if
      WIDTH 16 = if 2 else 4 then IMM {: value:n :}
      s" test" OUT GAP WIDTH RM. SEP value IMM. exit then
   DREG @ case
      2 of s" not" endof 3 of s" neg" endof
      4 of s" mul" endof 5 of s" imul" endof
      6 of s" div" endof 7 of s" idiv" endof
      UNKNOWN
   endcase OUT GAP WIDTH RM. ;

: FF-GROUP ( -- )
   INT-PREFIX MODRM
   DREG @ case
      0 of s" inc" endof 1 of s" dec" endof
      2 of s" call" endof 4 of s" jmp" endof
      UNKNOWN
   endcase OUT GAP
   DREG @ 2 = DREG @ 4 = or if 64 else WIDTH then RM. ;

: SIMPLE-GROUP ( -- bool )
   DOP @ $C1 = DOP @ $D3 = or if SHIFT-GROUP true exit then
   DOP @ $F7 = if F7-GROUP true exit then
   DOP @ $FF = if FF-GROUP true exit then
   false ;

: SSE-PREFIX ( n -- ) {: want:n :}
   DPFX @ want <> DLOCK @ 0<> or if UNKNOWN then ;

: SSE-REG-RM ( ptr u8 n -- ) {: name:ptr u:n :}
   MODRM name u OUT GAP DREG @ XMM. SEP XRM. ;

: SSE-RM-REG ( ptr u8 n -- ) {: name:ptr u:n :}
   MODRM name u OUT GAP XRM. SEP DREG @ XMM. ;

: SSE-SCALAR ( -- bool )
   DOP @ $110 = if $F2 SSE-PREFIX s" movsd" SSE-REG-RM true exit then
   DOP @ $111 = if $F2 SSE-PREFIX s" movsd" SSE-RM-REG true exit then
   DOP @ $158 = if $F2 SSE-PREFIX s" addsd" SSE-REG-RM true exit then
   DOP @ $15C = if $F2 SSE-PREFIX s" subsd" SSE-REG-RM true exit then
   DOP @ $159 = if $F2 SSE-PREFIX s" mulsd" SSE-REG-RM true exit then
   DOP @ $15E = if $F2 SSE-PREFIX s" divsd" SSE-REG-RM true exit then
   DOP @ $151 = if $F2 SSE-PREFIX s" sqrtsd" SSE-REG-RM true exit then
   DOP @ $154 = if $66 SSE-PREFIX s" andpd" SSE-REG-RM true exit then
   DOP @ $157 = if $66 SSE-PREFIX s" xorpd" SSE-REG-RM true exit then
   DOP @ $12E = if $66 SSE-PREFIX s" ucomisd" SSE-REG-RM true exit then
   DOP @ $1C2 = if
      $F2 SSE-PREFIX MODRM NEXT {: pred:n :}
      s" cmpsd" OUT GAP DREG @ XMM. SEP XRM. SEP pred IMM.
      true exit then
   false ;

: SSE-CONVERT ( -- bool )
   DOP @ $12A = if
      $F2 SSE-PREFIX REX-W? 0= if UNKNOWN then MODRM
      s" cvtsi2sd" OUT GAP DREG @ XMM. SEP 64 RM. true exit then
   DOP @ $12C = if
      $F2 SSE-PREFIX REX-W? 0= if UNKNOWN then MODRM
      s" cvttsd2si" OUT GAP DREG @ 64 REG. SEP XRM. true exit then
   DOP @ $16E = if
      $66 SSE-PREFIX REX-W? 0= if UNKNOWN then MODRM
      s" movq" OUT GAP DREG @ XMM. SEP 64 RM. true exit then
   DOP @ $17E = if
      $66 SSE-PREFIX REX-W? 0= if UNKNOWN then MODRM
      s" movq" OUT GAP 64 RM. SEP DREG @ XMM. true exit then
   false ;

: SSE-OTHER ( -- bool )
   DOP @ $110 = DOP @ $111 = or DPFX @ $F3 = and if
      DLOCK @ 0<> if UNKNOWN then
      DOP @ $110 = if s" movss" SSE-REG-RM else s" movss" SSE-RM-REG then
      true exit then
   DOP @ $11E = DPFX @ $F3 = and if
      DLOCK @ 0<> if UNKNOWN then
      MODRM DREG @ 7 <> DRM @ 2 <> or DMOD @ 3 <> or if UNKNOWN then
      s" endbr64" OUT true exit then
   false ;

: TWO-CONTROL ( -- bool )
   DOP @ $105 = if PLAIN-PREFIX s" syscall" OUT true exit then
   DOP @ $10B = if PLAIN-PREFIX s" ud2" OUT true exit then
   DOP @ $1AE = if
      PLAIN-PREFIX MODRM DREG @ 6 <> DRM @ 0 <> or DMOD @ 3 <> or if UNKNOWN then
      s" mfence" OUT true exit then
   DOP @ $180 >= DOP @ $18F <= and if COND-BRANCH true exit then
   DOP @ $190 >= DOP @ $19F <= and if
      INT-PREFIX MODRM DREG @ 0<> if UNKNOWN then
      DMOD @ 3 = if DRM @ BYTE-REG-CHECK then
      DOP @ $F and SET$ OUT GAP 8 RM. true exit then
   DOP @ $140 >= DOP @ $14F <= and if
      INT-PREFIX MODRM DOP @ $F and CMOV$ WIDTH REG-RM true exit then
   false ;

: TWO-INT ( -- bool )
   DOP @ $1B6 = DOP @ $1B7 = or DOP @ $1BE = or DOP @ $1BF = or if
      INT-PREFIX MODRM
      DOP @ $1B6 = DOP @ $1B7 = or if s" movzx" else s" movsx" then
      OUT GAP DREG @ WIDTH REG. SEP
      DOP @ 1 and 0<> if 16 else 8 then RM.
      true exit then
   DOP @ $1AF = if INT-PREFIX MODRM s" imul" WIDTH REG-RM true exit then
   DOP @ $1C1 = DOP @ $1B1 = or if
      DLOCK @ 0= if UNKNOWN then
      DPFX @ 0<> if UNKNOWN then
      MODRM
      DMOD @ 3 = if UNKNOWN then
      s" lock " OUT
      DOP @ $1C1 = if s" xadd" else s" cmpxchg" then
      WIDTH RM-REG true exit then
   false ;

: DECODE-TWO ( -- bool )
   TWO-CONTROL if true exit then
   SSE-OTHER if true exit then
   SSE-SCALAR if true exit then
   SSE-CONVERT if true exit then
   TWO-INT if true exit then
   false ;

private

: DECODE ( ptr u8 n n -- ) {: a:ptr u:n pc:n :}
   a DA ! u DU ! pc DPC ! 0 DI ! 0 DREX ! 0 D-HASREX ! 0 DPFX ! 0 DLOCK !
   0 DDELTA !
   u 0 <= if TRUNC then
   PREFIXES OPCODE
   DECODE-ONE if exit then
   DECODE-GROUP if exit then
   SIMPLE-GROUP if exit then
   DECODE-TWO if exit then
   UNKNOWN ;

: FLOW ( -- n )
   DOP @ $70 >= DOP @ $7F <= and if FLOW-COND exit then
   DOP @ $180 >= DOP @ $18F <= and if FLOW-COND exit then
   DOP @ $E9 = DOP @ $EB = or if FLOW-JUMP exit then
   DOP @ $E8 = if FLOW-CALL exit then
   DOP @ $C3 = DOP @ $10B = or if FLOW-END exit then
   DOP @ $FF = if
      DREG @ 4 = if FLOW-INDIRECT exit then
      DREG @ 2 = if FLOW-CALL exit then
   then
   FLOW-NEXT ;

public

\ Prints one real instruction and returns its consumed byte count (1..15).
: DIS1 ( ptr u8 n n -- n )
   1 DPRINT ! DECODE DI @ ;

\ Decode once without presentation. A direct target is in the caller's PC
\ coordinate system; all other flows return zero for the target.
: STEP ( ptr u8 n n -- n n n )
   0 DPRINT ! DECODE
   FLOW {: kind:n :}
   DI @ kind
   kind FLOW-COND = kind FLOW-JUMP = or kind FLOW-CALL = or if
      DOP @ $FF = if 0 else DPC @ DI @ + DDELTA @ + then
   else 0 then ;

;package

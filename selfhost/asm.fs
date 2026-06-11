\ asm.fs — ARM64 instruction encoders in the STANDALONE's Forth (operands -> u32).
\ Ported from src/cg/asm.fs (decimal constants; same bit layout). First step of the
\ codegen port: the standalone can now ENCODE ARM64, not just run baked code. Verified
\ byte-for-byte against caf in test/t-sh-asm.fs.
4294967295 constant W32
: MSK W32 and ;
\ move-wide: rd imm16 hw -> u32
: MOVZHW {: rd imm hw :} 3531603968 rd or  imm 5 lshift or  hw 21 lshift or MSK ;
: MOVKHW {: rd imm hw :} 4068474880 rd or  imm 5 lshift or  hw 21 lshift or MSK ;
: MOVNHW {: rd imm hw :} 2457862144 rd or  imm 5 lshift or  hw 21 lshift or MSK ;
\ shifted-register 3-operand: rd rn rm
: RRR {: base rd rn rm :} base rd or  rn 5 lshift or  rm 16 lshift or MSK ;
: ENC-ADD  {: rd rn rm :} 2332033024 rd rn rm RRR ;
: ENC-SUB  {: rd rn rm :} 3405774848 rd rn rm RRR ;
: ENC-AND  {: rd rn rm :} 2315255808 rd rn rm RRR ;
: ENC-ORR  {: rd rn rm :} 2852126720 rd rn rm RRR ;
: ENC-EOR  {: rd rn rm :} 3388997632 rd rn rm RRR ;
: ENC-MUL  {: rd rn rm :} 2600500224 rd rn rm RRR ;
\ add/sub immediate: rd rn imm12
: ENC-ADDI {: rd rn imm :} 2432696320 rd or  rn 5 lshift or  imm 10 lshift or MSK ;
: ENC-SUBI {: rd rn imm :} 3506438144 rd or  rn 5 lshift or  imm 10 lshift or MSK ;
\ logical-shift-left immediate (LSL #sh via UBFM): rd rn sh
: ENC-LSLI {: rd rn sh :} 3544186880 rd or  rn 5 lshift or
   64 sh - 63 and 16 lshift or  63 sh - 10 lshift or MSK ;
\ logical-shift-right immediate: rd rn sh
: ENC-LSRI {: rd rn sh :} 3544251392 rd or  rn 5 lshift or  sh 16 lshift or MSK ;
\ compare (shifted reg) rn rm  -> subs xzr
: ENC-CMP  {: rn rm :} 3942645791 rn 5 lshift or  rm 16 lshift or MSK ;
\ compare immediate rn imm12
: ENC-CMPI {: rn imm :} 4043309087 rn 5 lshift or  imm 10 lshift or MSK ;
\ svc #imm  ; ret
: ENC-SVC  {: imm :} 3556769793 imm 5 lshift or MSK ;
: ENC-RET  3596551104 ;

\ opt-ir.f - line-oriented instruction-table IR over emitted PTX text.
\
\ A captured PTX module (src/arch/ptx/emit.f PTX-CAPTURE$) is regular by
\ construction: our emitters print one instruction per line, `mnemonic dst, src,
\ src[, src];`, plus directives / labels / braces / predicated lines. PTX-PARSE
\ copies that text into a private arena and classifies each line into a typed
\ record. Recognized side-effect-free register-writing ops (add/sub/mul/div/
\ mad/fma/neg/abs/min/max/cvt/cvta/ex2/lg2/rcp/rsqrt/sqrt/mov and the logical/
\ shift ops) become PURE records with a mnemonic, a dst register symbol and up to
\ three source operand symbols. EVERYTHING else - directives, labels, `{`/`}`,
\ predicated `@%p` lines, loads/stores/atomics/barriers/branches/mma, mov of a
\ special register (`%tid`/`%ctaid`), and any mnemonic we do not model - is kept
\ as an OPAQUE record and rendered back byte-identically. Fail-closed: an
\ unmodelled or memory-touching line is passthrough, never rewritten or removed.
\ The optimizer (lib/ptx/opt.f) consumes this table; PTX-RENDER reproduces text.
\
\ Scope is exactly the instruction forms our own emitters produce (lib/ptx/cg.f,
\ cg-collective.f, cg-activation.f, cg-mma.f, cg-matmul.f, ...). Anything outside
\ that shape round-trips unchanged. Load after lib/errors.f and lib/string.f.
\ Checked Habu.

\ ---- capacities (a bk=32 fused-epilogue kernel is ~40 KB / ~1600 lines) ----
$20000 constant OPTX-SRC-CAP    \ 128 KB private copy of the input module
$20000 constant OPTX-OUT-CAP    \ 128 KB render target (output never exceeds input)
4096   constant OPTX-MAXLINE
8192   constant OPTX-MAXSYM
19     constant OPTX-REC         \ cells per line record

OPTX-SRC-CAP BUFFER: OPTX-SRC
variable OPTX-SRC-U
OPTX-OUT-CAP BUFFER: OPTX-OUT
variable OPTX-OUT-U

create OPTX-TAB OPTX-MAXLINE OPTX-REC * cells allot
variable OPTX-N

create OPTX-SYM-OFF OPTX-MAXSYM cells allot   \ symbol text offset into OPTX-SRC
create OPTX-SYM-LEN OPTX-MAXSYM cells allot
variable OPTX-SYM-N

\ ---- record field indices ----
0  constant OPTX.KIND      \ 0 = opaque, 1 = pure
1  constant OPTX.RESET     \ value-numbering barrier (directive / label / brace / branch)
2  constant OPTX.SIDEFX    \ has a side effect or is otherwise never removable
3  constant OPTX.PRED      \ predicated (@%p ...) line
4  constant OPTX.OFF       \ raw line offset into OPTX-SRC (verbatim render source)
5  constant OPTX.LEN       \ raw line length
6  constant OPTX.MOFF      \ mnemonic offset into OPTX-SRC
7  constant OPTX.MLEN      \ mnemonic length
8  constant OPTX.CLASS     \ operator class (see below)
9  constant OPTX.COMMUT    \ commutative binary op (add/mul/and/or/xor/min/max)
10 constant OPTX.DST       \ destination register symbol id (or -1)
11 constant OPTX.NSRC      \ source operand count (0..3)
12 constant OPTX.S0
13 constant OPTX.S1
14 constant OPTX.S2
15 constant OPTX.VN        \ result value number (assigned by the optimizer)
16 constant OPTX.REMOVED   \ dropped by a pass
17 constant OPTX.ENTRY     \ starts a new kernel entry (`.visible .entry` / `.func`)
18 constant OPTX.REW       \ sources were rewritten; render canonically, not verbatim

\ ---- operator classes ----
0 constant OPTX-C-OPAQUE
1 constant OPTX-C-UNARY    \ one source (neg/abs/cvt/cvta/ex2/rcp/...)
2 constant OPTX-C-BIN      \ two sources (add/sub/mul/div/shl/...)
3 constant OPTX-C-TERN     \ three sources (mad/fma)
4 constant OPTX-C-MOVREG   \ mov of a register (copy)
5 constant OPTX-C-MOVIMM   \ mov of an immediate

-1 constant OPTX-NONE

44 constant OPTX-COMMA-CH
46 constant OPTX-DOT-CH
59 constant OPTX-SEMI-CH
91 constant OPTX-BRA-CH
37 constant OPTX-PCT-CH
64 constant OPTX-AT-CH
123 constant OPTX-LB-CH
125 constant OPTX-RB-CH
58 constant OPTX-COLON-CH

\ ================= arena helpers =================
: OPTX-AT ( n -- ptr u8 ) {: off:n :} OPTX-SRC off + ;
: OPTX-CH ( n -- n ) OPTX-AT c@ ;
: OPTX-SYM$ ( n -- ptr u8 n ) {: id:n :}
   id 0 < if E-PTX-OPT-SYNTAX throw then
   OPTX-SYM-OFF id cells + @ OPTX-AT
   OPTX-SYM-LEN id cells + @ ;
: OPTX-SYM-REG? ( n -- bool )
   OPTX-SYM$ drop c@ OPTX-PCT-CH = ;
: OPTX-SYM-HAS-DOT? ( n -- bool )
   OPTX-SYM$ s" ." CONTAINS? ;

: OPTX-ROW ( n -- ptr a ) {: i:n :} OPTX-TAB i OPTX-REC * cells + ;
: OPTX@ ( n n -- n ) {: i:n f:n :} i OPTX-ROW f cells + @ ;
: OPTX! ( n n n -- ) {: v:n i:n f:n :} v i OPTX-ROW f cells + ! ;
: OPTX? ( n n -- bool ) OPTX@ 0 <> ;

\ ================= symbol interning =================
\ symbols are substrings of OPTX-SRC; dedup by content so identical registers /
\ identical immediates share one id (identical immediates fold to one constant).
: OPTX-SYM-FIND ( n n -- n ) {: off:n len:n :}
   OPTX-SYM-N @ 0 ?do
      off OPTX-AT len i OPTX-SYM$ STR= if i unloop exit then
   loop OPTX-NONE ;
: OPTX-SYM-ADD ( n n -- n ) {: off:n len:n :}
   OPTX-SYM-N @ OPTX-MAXSYM >= if E-PTX-OPT-OVERFLOW throw then
   off OPTX-SYM-OFF OPTX-SYM-N @ cells + !
   len OPTX-SYM-LEN OPTX-SYM-N @ cells + !
   OPTX-SYM-N @ dup 1+ OPTX-SYM-N ! ;
: OPTX-INTERN ( n n -- n ) {: off:n len:n :}
   off len OPTX-SYM-FIND {: hit:n :}
   hit OPTX-NONE <> if hit exit then
   off len OPTX-SYM-ADD ;

\ ================= offset-space text predicates =================
: OPTX-SPACE? ( n -- bool )
   dup STR-SPACE = swap STR-TAB = or ;
: OPTX-STARTS? ( n n ptr u8 n -- bool ) {: off:n len:n b:ptr v:n :}
   off OPTX-AT len b v STARTS-WITH? ;
: OPTX-HAS-CH? ( n n n -- bool ) {: off:n len:n c:n :}
   len 0 ?do off i + OPTX-CH c = if 0 0= unloop exit then loop 0 0= 0= ;
: OPTX-FIRST-SPACE ( n n -- n ) {: off:n len:n :}
   len 0 ?do off i + OPTX-CH OPTX-SPACE? if i unloop exit then loop len ;
: OPTX-ENDS-SEMI? ( n n -- bool ) {: off:n len:n :}
   len 0= if 0 0= 0= exit then
   off len 1- + OPTX-CH OPTX-SEMI-CH = ;
: OPTX-LTRIM ( n n -- n n ) {: off:n len:n :}
   0 begin dup len < while
      off over + OPTX-CH OPTX-SPACE? 0= if off over + swap len swap - exit then 1+
   repeat drop off len + 0 ;
: OPTX-RTRIM ( n n -- n n ) {: off:n len:n :}
   len begin dup 0 > while
      off over 1- + OPTX-CH OPTX-SPACE? if 1- else off swap exit then
   repeat drop off 0 ;
: OPTX-TRIM ( n n -- n n ) OPTX-LTRIM OPTX-RTRIM ;

\ base op token = mnemonic up to its first '.' (add.rn.f32 -> add)
: OPTX-BASE-LEN ( n n -- n ) {: off:n len:n :}
   len 0 ?do off i + OPTX-CH OPTX-DOT-CH = if i unloop exit then loop len ;
: OPTX-BASE= ( n n ptr u8 n -- bool ) {: off:n len:n b:ptr v:n :}
   off len OPTX-BASE-LEN {: bl:n :}
   off OPTX-AT bl b v STR= ;
: OPTX-COMMA-IDX ( n n n -- n ) {: off:n len:n start:n :}
   start begin dup len < while
      off over + OPTX-CH OPTX-COMMA-CH = if exit then 1+
   repeat drop len ;

\ ================= mnemonic classification =================
\ returns class, sidefx (0/1), commut (0/1); opaque covers every unmodelled op.
: OPTX-CLASSIFY ( n n -- n n n ) {: off:n len:n :}
   off len s" mov"   OPTX-BASE= if OPTX-C-MOVREG 0 0 exit then
   off len s" add"   OPTX-BASE= if OPTX-C-BIN   0 1 exit then
   off len s" mul"   OPTX-BASE= if OPTX-C-BIN   0 1 exit then
   off len s" and"   OPTX-BASE= if OPTX-C-BIN   0 1 exit then
   off len s" or"    OPTX-BASE= if OPTX-C-BIN   0 1 exit then
   off len s" xor"   OPTX-BASE= if OPTX-C-BIN   0 1 exit then
   off len s" min"   OPTX-BASE= if OPTX-C-BIN   0 1 exit then
   off len s" max"   OPTX-BASE= if OPTX-C-BIN   0 1 exit then
   off len s" sub"   OPTX-BASE= if OPTX-C-BIN   0 0 exit then
   off len s" div"   OPTX-BASE= if OPTX-C-BIN   0 0 exit then
   off len s" shl"   OPTX-BASE= if OPTX-C-BIN   0 0 exit then
   off len s" shr"   OPTX-BASE= if OPTX-C-BIN   0 0 exit then
   off len s" mad"   OPTX-BASE= if OPTX-C-TERN  0 0 exit then
   off len s" fma"   OPTX-BASE= if OPTX-C-TERN  0 0 exit then
   off len s" neg"   OPTX-BASE= if OPTX-C-UNARY 0 0 exit then
   off len s" abs"   OPTX-BASE= if OPTX-C-UNARY 0 0 exit then
   off len s" not"   OPTX-BASE= if OPTX-C-UNARY 0 0 exit then
   off len s" cvta"  OPTX-BASE= if OPTX-C-UNARY 0 0 exit then
   off len s" cvt"   OPTX-BASE= if OPTX-C-UNARY 0 0 exit then
   off len s" ex2"   OPTX-BASE= if OPTX-C-UNARY 0 0 exit then
   off len s" lg2"   OPTX-BASE= if OPTX-C-UNARY 0 0 exit then
   off len s" rcp"   OPTX-BASE= if OPTX-C-UNARY 0 0 exit then
   off len s" rsqrt" OPTX-BASE= if OPTX-C-UNARY 0 0 exit then
   off len s" sqrt"  OPTX-BASE= if OPTX-C-UNARY 0 0 exit then
   OPTX-C-OPAQUE 1 0 ;

\ base op is a control-flow op (ends a straight-line region for value numbering)
: OPTX-CTRL? ( n n -- n ) {: off:n len:n :}
   off len s" bra"  OPTX-BASE= if 1 exit then
   off len s" ret"  OPTX-BASE= if 1 exit then
   off len s" call" OPTX-BASE= if 1 exit then
   0 ;

\ ================= record init =================
: OPTX-ROW-CLEAR ( n -- ) {: i:n :}
   OPTX-C-OPAQUE i OPTX.CLASS OPTX!
   0 i OPTX.KIND OPTX!  0 i OPTX.RESET OPTX!  0 i OPTX.SIDEFX OPTX!
   0 i OPTX.PRED OPTX!  0 i OPTX.COMMUT OPTX! 0 i OPTX.ENTRY OPTX!
   0 i OPTX.REW OPTX!   0 i OPTX.REMOVED OPTX! 0 i OPTX.NSRC OPTX!
   OPTX-NONE i OPTX.DST OPTX! OPTX-NONE i OPTX.S0 OPTX!
   OPTX-NONE i OPTX.S1 OPTX!  OPTX-NONE i OPTX.S2 OPTX!
   OPTX-NONE i OPTX.VN OPTX! ;

: OPTX-MARK-OPAQUE-2 ( n n n -- ) {: reset:n sidefx:n i:n :}
   0 i OPTX.KIND OPTX!  reset i OPTX.RESET OPTX!  sidefx i OPTX.SIDEFX OPTX! ;

\ ================= operand parsing (offset space) =================
variable OPTX-OK  variable OPTX-IDX  variable OPTX-START
variable OPTX-DST-TMP variable OPTX-S0-TMP variable OPTX-S1-TMP variable OPTX-S2-TMP

: OPTX-STORE-OP ( n n -- ) {: id:n idx:n :}
   idx 0 = if id OPTX-DST-TMP ! exit then
   idx 1 = if id OPTX-S0-TMP ! exit then
   idx 2 = if id OPTX-S1-TMP ! exit then
   idx 3 = if id OPTX-S2-TMP ! exit then
   0 OPTX-OK ! ;   \ more than three sources: not modelled
: OPTX-OP1 ( n n -- ) {: off:n len:n :}   \ one operand region (offset space)
   off len OPTX-TRIM {: soff:n slen:n :}
   slen 0 = if 0 OPTX-OK ! exit then
   soff slen OPTX-INTERN OPTX-IDX @ OPTX-STORE-OP
   OPTX-IDX @ 1+ OPTX-IDX ! ;
: OPTX-PARSE-OPS ( n n -- ) {: off:n len:n :}   \ trimmed operand region, no ';'
   1 OPTX-OK !  0 OPTX-IDX !  0 OPTX-START !
   begin OPTX-START @ len < while
      off len OPTX-START @ OPTX-COMMA-IDX {: ci:n :}
      off OPTX-START @ + ci OPTX-START @ - OPTX-OP1
      ci 1+ OPTX-START !
   repeat ;

\ ================= pure-instruction record =================
: OPTX-INSN-STORE ( n n n -- ) {: i:n class:n commut:n :}
   1 i OPTX.KIND OPTX!  class i OPTX.CLASS OPTX!  commut i OPTX.COMMUT OPTX!
   0 i OPTX.SIDEFX OPTX!  0 i OPTX.RESET OPTX!
   OPTX-DST-TMP @ i OPTX.DST OPTX!
   OPTX-IDX @ 1- i OPTX.NSRC OPTX!
   OPTX-S0-TMP @ i OPTX.S0 OPTX!  OPTX-S1-TMP @ i OPTX.S1 OPTX!
   OPTX-S2-TMP @ i OPTX.S2 OPTX! ;

: OPTX-ARITY-OK? ( n n -- bool ) {: class:n nsrc:n :}
   class OPTX-C-UNARY  = if nsrc 1 = exit then
   class OPTX-C-BIN    = if nsrc 2 = exit then
   class OPTX-C-TERN   = if nsrc 3 = exit then
   class OPTX-C-MOVREG = if nsrc 1 = exit then
   class OPTX-C-MOVIMM = if nsrc 1 = exit then
   0 0= 0= ;

\ resolve mov class (register copy vs immediate vs special-reg) after parse
: OPTX-MOV-CLASS ( n -- n ) {: class:n :}
   class OPTX-C-MOVREG <> if class exit then
   OPTX-S0-TMP @ OPTX-SYM-REG? 0= if OPTX-C-MOVIMM exit then
   OPTX-S0-TMP @ OPTX-SYM-HAS-DOT? if OPTX-C-OPAQUE exit then   \ special reg (%tid.x)
   OPTX-C-MOVREG ;

: OPTX-INSN ( n n n -- ) {: toff:n tlen:n i:n :}
   toff tlen OPTX-FIRST-SPACE {: msp:n :}
   toff msp OPTX-ENDS-SEMI? if msp 1- else msp then {: mlen:n :}
   toff mlen OPTX-CLASSIFY {: class:n sidefx:n commut:n :}
   toff i OPTX.MOFF OPTX!  mlen i OPTX.MLEN OPTX!
   class OPTX-C-OPAQUE = if
      toff tlen OPTX-CTRL? sidefx i OPTX-MARK-OPAQUE-2 exit then
   toff msp + tlen msp - OPTX-TRIM {: roff:n rlen:n :}
   roff rlen OPTX-ENDS-SEMI? if roff rlen 1- else roff rlen then OPTX-TRIM {: qoff:n qlen:n :}
   qoff qlen OPTX-BRA-CH OPTX-HAS-CH? if
      0 sidefx i OPTX-MARK-OPAQUE-2 exit then          \ memory operand -> opaque
   qoff qlen OPTX-PARSE-OPS
   OPTX-OK @ 0= if 0 sidefx i OPTX-MARK-OPAQUE-2 exit then
   OPTX-IDX @ 1 < if 0 sidefx i OPTX-MARK-OPAQUE-2 exit then
   OPTX-DST-TMP @ OPTX-SYM-REG? 0= if 0 sidefx i OPTX-MARK-OPAQUE-2 exit then
   class OPTX-MOV-CLASS {: mclass:n :}
   mclass OPTX-C-OPAQUE = if 0 sidefx i OPTX-MARK-OPAQUE-2 exit then
   mclass OPTX-IDX @ 1- OPTX-ARITY-OK? 0= if 0 sidefx i OPTX-MARK-OPAQUE-2 exit then
   i mclass commut OPTX-INSN-STORE ;

\ ================= line dispatcher =================
: OPTX-ENTRY? ( n n -- bool ) {: toff:n tlen:n :}
   toff tlen s" .visible .entry" OPTX-STARTS? if 0 0= exit then
   toff tlen s" .entry" OPTX-STARTS? if 0 0= exit then
   toff tlen s" .func"  OPTX-STARTS? if 0 0= exit then
   0 0= 0= ;
: OPTX-LABEL? ( n n -- bool ) {: toff:n tlen:n :}
   tlen 0= if 0 0= 0= exit then
   toff tlen OPTX-FIRST-SPACE tlen <> if 0 0= 0= exit then
   toff tlen OPTX-ENDS-SEMI? if 0 0= 0= exit then
   toff tlen 1- + OPTX-CH OPTX-COLON-CH = ;

: OPTX-LINE ( n n -- ) {: roff:n rlen:n :}
   OPTX-N @ OPTX-MAXLINE >= if E-PTX-OPT-OVERFLOW throw then
   OPTX-N @ {: i:n :}
   i OPTX-ROW-CLEAR
   roff i OPTX.OFF OPTX!  rlen i OPTX.LEN OPTX!
   OPTX-N @ 1+ OPTX-N !
   roff rlen OPTX-TRIM {: toff:n tlen:n :}
   tlen 0= if 0 0 i OPTX-MARK-OPAQUE-2 exit then
   toff OPTX-CH {: c0:n :}
   c0 OPTX-DOT-CH = if
      1 0 i OPTX-MARK-OPAQUE-2
      toff tlen OPTX-ENTRY? if 1 i OPTX.ENTRY OPTX! then exit then
   c0 OPTX-LB-CH = c0 OPTX-RB-CH = or if 1 0 i OPTX-MARK-OPAQUE-2 exit then
   c0 OPTX-AT-CH = if
      0 1 i OPTX-MARK-OPAQUE-2  1 i OPTX.PRED OPTX! exit then
   toff tlen OPTX-LABEL? if 1 0 i OPTX-MARK-OPAQUE-2 exit then
   toff tlen i OPTX-INSN ;

\ ================= parse driver =================
variable OPTX-LSTART
: OPTX-NL-IDX ( n -- n ) {: start:n :}
   start begin dup OPTX-SRC-U @ < while
      dup OPTX-CH STR-LF = if exit then 1+
   repeat ;
: PTX-PARSE ( ptr u8 n -- ) {: a:ptr u:n :}
   u OPTX-SRC-CAP > if E-PTX-OPT-OVERFLOW throw then
   a OPTX-SRC u BYTE-COPY
   u OPTX-SRC-U !
   0 OPTX-N !  0 OPTX-SYM-N !
   0 OPTX-LSTART !
   begin OPTX-LSTART @ OPTX-SRC-U @ < while
      OPTX-LSTART @ OPTX-NL-IDX {: nl:n :}
      OPTX-LSTART @ nl OPTX-LSTART @ - OPTX-LINE
      nl 1+ OPTX-LSTART !
   repeat ;

\ ================= render =================
: OPTX-EMIT ( ptr u8 n -- ) {: a:ptr u:n :}
   OPTX-OUT-U @ u + OPTX-OUT-CAP > if E-PTX-OPT-OVERFLOW throw then
   a OPTX-OUT OPTX-OUT-U @ + u BYTE-COPY
   OPTX-OUT-U @ u + OPTX-OUT-U ! ;
: OPTX-OUT-C ( n -- ) {: c:n :}
   OPTX-OUT-U @ 1+ OPTX-OUT-CAP > if E-PTX-OPT-OVERFLOW throw then
   c OPTX-OUT OPTX-OUT-U @ + c!
   OPTX-OUT-U @ 1+ OPTX-OUT-U ! ;
: OPTX-SYM-OUT ( n -- ) OPTX-SYM$ OPTX-EMIT ;
: OPTX-REND-RAW ( n -- ) {: i:n :}
   i OPTX.OFF OPTX@ OPTX-AT  i OPTX.LEN OPTX@  OPTX-EMIT ;
: OPTX-REND-CANON ( n -- ) {: i:n :}
   i OPTX.MOFF OPTX@ OPTX-AT  i OPTX.MLEN OPTX@  OPTX-EMIT
   s"  " OPTX-EMIT
   i OPTX.DST OPTX@ OPTX-SYM-OUT
   i OPTX.NSRC OPTX@ {: ns:n :}
   ns 0 > if s" , " OPTX-EMIT i OPTX.S0 OPTX@ OPTX-SYM-OUT then
   ns 1 > if s" , " OPTX-EMIT i OPTX.S1 OPTX@ OPTX-SYM-OUT then
   ns 2 > if s" , " OPTX-EMIT i OPTX.S2 OPTX@ OPTX-SYM-OUT then
   s" ;" OPTX-EMIT ;
: PTX-RENDER ( -- )
   0 OPTX-OUT-U !
   OPTX-N @ 0 ?do
      i OPTX.REMOVED OPTX? 0= if
         i OPTX.REW OPTX? if i OPTX-REND-CANON else i OPTX-REND-RAW then
         STR-LF OPTX-OUT-C
      then
   loop ;
: PTX-RENDER$ ( -- ptr u8 n ) OPTX-OUT OPTX-OUT-U @ ;

\ ================= instruction count =================
\ counts real instructions (pure + opaque insns); skips directives/labels/braces/blanks
: OPTX-IS-INSN? ( n -- bool ) {: i:n :}
   i OPTX.REMOVED OPTX? if 0 0= 0= exit then
   i OPTX.KIND OPTX? if 0 0= exit then
   i OPTX.OFF OPTX@ i OPTX.LEN OPTX@ OPTX-TRIM {: toff:n tlen:n :}
   tlen 0= if 0 0= 0= exit then
   toff OPTX-CH {: c0:n :}
   c0 OPTX-DOT-CH = if 0 0= 0= exit then
   c0 OPTX-LB-CH = c0 OPTX-RB-CH = or if 0 0= 0= exit then
   toff tlen OPTX-LABEL? if 0 0= 0= exit then
   0 0= ;
: PTX-INSN-COUNT ( -- n )
   0 OPTX-N @ 0 ?do i OPTX-IS-INSN? if 1+ then loop ;

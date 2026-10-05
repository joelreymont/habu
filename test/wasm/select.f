\ select.f - WSEL, src/arch/wasm/select.f: its calls, status and integer rows
\ and its admission matrix, under WPROF's V1 profile.
\
\ Each row compiles Habu source, selects it and compares what the module holds
\ (test/wasm/select-lib.f): its shape, or a function's signature, or a call's
\ callee.

require lib/test.f
require lib/string.f
require lib/fmt.f
require src/compiler/ir/type.f
require src/compiler/ir/op.f
require src/compiler/ir/attr.f
require src/compiler/ir/fun.f
require src/compiler/ir/build.f
require src/compiler/native/dict.f
require src/compiler/native/frozen.f
require src/compiler/native/hir.f
require src/compiler/native/elaborate.f
require src/compiler/native/backend.f
require src/arch/wasm/profile.f
require src/arch/wasm/wstruct.f
require src/arch/wasm/select.f
require test/compiler/native-source-fixture.f
require test/wasm/select-lib.f

\ Host words the rows call, named globally as source text names them.
: SEL-INC ( n -- n ) 1 + ;

\ One input past WPROF's sixteen lanes, so a call to it takes the frame.
: SEL-SUM17 ( n n n n n n n n n n n n n n n n n -- n )
   + + + + + + + + + + + + + + + + ;

\ Dead by its own body, so the elaborator stages a fault after a call to it.
: SEL-BOOM ( n -- ) throw ;

package WSEL-TEST
private

\ Function k's signature as the type table renders it.
: SIG$ ( IR-BUILD:module n -- ptr u8 n )
   {: m:IR-BUILD:module k:n :}
   m IR-BUILD:FFUN-ROWS m IR-BUILD:FKEY  m IR-BUILD:FKEY k IR-ID:PACK-FUN
   IR-FUN:FSIGNATURE@ {: sig:IR-ID:ir-type-id :}
   m IR-BUILD:FTYPE-POOL m IR-BUILD:FTYPE-ROWS sig NB 256 IR-TYPE:FRENDER
   NB swap ;

\ The first call function zero makes, its blocks read in order.
variable FOUND?
1 TYPED-BUFFER FOUND IR-ID:ir-op-id

: CALL-SEEN ( IR-ID:ir-op-id -- )
   {: o:IR-ID:ir-op-id :}
   FOUND? @ 0<> if exit then
   o NFROZEN:OPCODE-AT SYM$ s" wstruct.call" STR= 0= if exit then
   o 0 FOUND !
   1 FOUND? ! ;

: BLOCK-CALLS ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk NFROZEN:OP-COUNT 0 ?do  bk i NFROZEN:OP-AT CALL-SEEN  loop ;

\ Its callee's spelling.
: CALLEE$ ( IR-BUILD:module -- ptr u8 n )
   {: m:IR-BUILD:module :}
   m NFROZEN:VIEWS!
   0 FOUND? !
   NFROZEN:MKEY 0 IR-ID:PACK-FUN {: f:IR-ID:ir-fun-id :}
   f NFROZEN:BLOCK-COUNT 0 ?do  f i NFROZEN:BLOCK-AT BLOCK-CALLS  loop
   FOUND? @ 0= if s" no call" exit then
   0 FOUND @ {: o:IR-ID:ir-op-id :}
   NFROZEN:V-OPP NFROZEN:VW NFROZEN:V-OPR NFROZEN:VW NFROZEN:MKEY
   o  o s" wstruct.callee" KEY-AT  IR-OP:FATTR@ {: at:IR-ID:ir-attr-id :}
   NFROZEN:V-ATTR NFROZEN:VW NFROZEN:MKEY at IR-ATTR:FSYM@ SYM$ ;

\ ---- the rows -------------------------------------------------------------------
: SIG-RUN ( IR-CTX:ctx -- )      COMPILE 0 SIG$ KEEP ;
: CALLEE-RUN ( IR-CTX:ctx -- )   COMPILE CALLEE$ KEEP ;

\ The source, its arity and the signature function zero must declare.
: SIG-ROW ( ptr u8 n n n ptr u8 n -- )
   {: src:ptr sn:n in:n out:n want:ptr wn:n :}
   src sn in out SOURCE!
   [: SIG-RUN ;] OUTCOME
   want wn KEPT ;

\ What function zero of the last module selected takes and leaves, and whether
\ that is the frame.
: ARITY-IS ( n n bool -- )
   {: in:n out:n fr:bool :}
   0 WSEL:ARITY {: gi:n go:n :}
   gi in T=  go out T=
   in out WSEL:FRAMED?  fr if TTRUE else TFALSE then ;

: INTEGER-ROWS ( -- )
   s" wrapping add: the lanes, the entry block and status 0" T-LABEL
   s" PA +" 2 1
   s" 4: br>1 | 2: i64.add i32.const=0 return | " ROW
   s" a compare's mask is 0 - extend_u(p) and brz tests the whole cell" T-LABEL
   s" PE < if 1 else 2 then" 2 1
   s" 4: br>1 | 2: i64.lt_s i64.extend_i32_u i64.const=0 i64.sub i64.eqz brz>3,2 | 0: br>4 | 0: i64.const=1 br>5 | 0: i64.const=2 br>5 | 1: i32.const=0 return | " ROW
   s" invert, rshift and and" T-LABEL
   s" PI invert 3 rshift -1 and" 1 1
   s" 3: br>1 | 1: i64.const=-1 i64.xor i64.const=3 i64.shr_u i64.const=-1 i64.and i32.const=0 return | " ROW
   s" division tests zero, then -1, before any i64.div_s" T-LABEL
   s" PC 7 /" 1 1
   s" 3: br>1 | 1: i64.const=7 i64.eqz brz>3,2 | 0: i64.const=-6400 i64.store br>7 | 0: i64.const=-1 i64.eq brz>4,5 | 0: i64.div_s br>6 | 0: i64.const=0 i64.sub br>6 | 1: i32.const=0 return | 0: i32.const=1 i64.const=0 return | " ROW
   s" mod is the division then elaborate.f's multiply and subtract" T-LABEL
   s" PD mod" 2 1
   s" 4: br>1 | 2: i64.eqz brz>3,2 | 0: i64.const=-6400 i64.store br>7 | 0: i64.const=-1 i64.eq brz>4,5 | 0: i64.div_s br>6 | 0: i64.const=0 i64.sub br>6 | 1: i64.mul i64.sub i32.const=0 return | 0: i32.const=1 i64.const=0 return | " ROW ;

\ A loop header without a token argument, entered first from the block before
\ the loop and again by the backedge, whose division's zero path stores with
\ the token it enters with: a token read off the backedge, a block not yet
\ selected, is refused.
: LOOP-ROWS ( -- )
   s" a loop header's token is the one the loop is entered with" T-LABEL
   s" PL begin 7 / dup 0= until" 1 1
   s" 3: br>1 | 1: br>2 | 1: i64.const=7 i64.eqz brz>4,3 | 0: i64.const=-6400 i64.store br>10 | 0: i64.const=-1 i64.eq brz>5,6 | 0: i64.div_s br>7 | 0: i64.const=0 i64.sub br>7 | 1: i64.const=0 i64.eq i64.extend_i32_u i64.const=0 i64.sub i64.eqz brz>9,8 | 0: br>2 | 0: i32.const=0 return | 0: i32.const=1 i64.const=0 return | " ROW ;

\ A call's host target is `host` and the entry the native call site records.
: HOST$ ( ptr u8 n -- ptr u8 n )
   NDICT:CALL-TARGET {: e:n :}
   SB-RESET  s" host " SB-APPEND  e FMT:SB-U  SB$ ;

: CALL-ROWS ( -- )
   s" a wordcall stores the kept row, tests status and reloads it" T-LABEL
   s" PB SEL-INC +" 3 2
   s" 5: br>1 | 3: i32.load i32.const=16 i32.add i64.extend_i32_u i64.const=200704 i64.gt_s brz>3,2 | 0: i32.const=102 i32.store i64.store unreachable | 0: i64.store*2 i32.store call brz>4,5 | 0: i64.load*2 i32.store i64.add i32.const=0 return | 0: i32.const=1 i64.const=0 return | " ROW
   s" a wordcall names its host entry" T-LABEL
   s" PB SEL-INC +" 3 2 SOURCE!
   [: CALLEE-RUN ;] OUTCOME
   s" SEL-INC" HOST$ KEPT
   s" throw is a terminal: the row stored, status 1 propagated, a return a fault" T-LABEL
   s" PF throw" 1 0
   s" 3: br>1 | 1: i32.load i32.const=8 i32.add i64.extend_i32_u i64.const=200704 i64.gt_s brz>3,2 | 0: i32.const=102 i32.store i64.store unreachable | 0: i64.store i32.store call brz>4,5 | 0: i32.const=88 i64.const=0 i32.store i64.store unreachable | 0: i32.const=1 return | " ROW
   s" a return from a dead callee is a trap: kind and message recorded, unreachable" T-LABEL
   s" PH SEL-BOOM" 1 0
   s" 3: br>1 | 1: call brz>2,3 | 0: i64.const@data i64.const=21 i64.const=88 i32.wrap_i64 i32.store i64.store unreachable | 0: i32.const=1 return | " ROW
   s" a tail RECURSE is a call then a return" T-LABEL
   s" PG dup 0= if drop 1 exit then 1 - RECURSE" 1 1
   s" 3: br>1 | 1: i64.const=0 i64.eq i64.extend_i32_u i64.const=0 i64.sub i64.eqz brz>3,2 | 0: br>4 | 0: i64.const=1 br>6 | 2: i64.const=1 i64.sub call brz>5,7 | 0: br>6 | 2: i32.const=0 return | 0: i32.const=1 i64.const=0 return | " ROW
   s" RECURSE names the definition itself" T-LABEL
   s" PG dup 0= if drop 1 exit then 1 - RECURSE" 1 1 SOURCE!
   [: CALLEE-RUN ;] OUTCOME
   s" PG" KEPT ;

\ W06, the direct half: sixteen lanes each way, one more and the frame.
: FRAME-ROWS ( -- )
   s" sixteen inputs are lanes" T-LABEL
   s" PV + + + + + + + + + + + + + + +" 16 1
   s" 18: br>1 | 16: i64.add*15 i32.const=0 return | " ROW
   16 1 false ARITY-IS
   s" PV + + + + + + + + + + + + + + +" 16 1
   s" ( i32 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 -- i32 i64 )" SIG-ROW
   s" seventeen inputs come off the frame, the output goes onto it" T-LABEL
   s" PW + + + + + + + + + + + + + + + +" 17 1
   s" 2: i32.load i32.const=136 i32.sub i64.load*17 i32.store br>1 | 17: i64.add*16 i32.load i32.const=8 i32.add i64.extend_i32_u i64.const=200704 i64.gt_s brz>3,2 | 0: i32.const=102 i32.store i64.store unreachable | 0: i64.store i32.store i32.const=0 return | " ROW
   17 1 true ARITY-IS
   s" PW + + + + + + + + + + + + + + + +" 17 1
   s" ( i32 -- i32 )" SIG-ROW
   s" seventeen outputs go onto the frame" T-LABEL
   s" PY dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup" 1 17
   s" 2: i32.load i32.const=8 i32.sub i64.load i32.store br>1 | 1: i32.load i32.const=136 i32.add i64.extend_i32_u i64.const=200704 i64.gt_s brz>3,2 | 0: i32.const=102 i32.store i64.store unreachable | 0: i64.store*17 i32.store i32.const=0 return | " ROW
   1 17 true ARITY-IS
   s" a call to a seventeen-input callee passes its arguments in the frame" T-LABEL
   s" PX 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 SEL-SUM17" 0 1
   s" 2: br>1 | 0: i64.const=1 i64.const=2 i64.const=3 i64.const=4 i64.const=5 i64.const=6 i64.const=7 i64.const=8 i64.const=9 i64.const=10 i64.const=11 i64.const=12 i64.const=13 i64.const=14 i64.const=15 i64.const=16 i64.const=17 i32.load i32.const=136 i32.add i64.extend_i32_u i64.const=200704 i64.gt_s brz>3,2 | 0: i32.const=102 i32.store i64.store unreachable | 0: i64.store*17 i32.store call brz>4,5 | 0: i64.load i32.store i32.const=0 return | 0: i32.const=1 i64.const=0 return | " ROW ;

\ Every store to the context stack first tests where its cells end against the
\ stack region's end, static data at $31000 (200704): past it, the block built
\ next records STACK-BOUNDS (102) and that end, and executes unreachable.
: STACK-ROWS ( -- )
   s" a kept row is stored only where it ends inside the stack region" T-LABEL
   s" PK 1 2 depth drop 2drop" 0 0
   s" 2: br>1 | 0: i64.const=1 i64.const=2 i32.load i32.const=16 i32.add i64.extend_i32_u i64.const=200704 i64.gt_s brz>3,2 | 0: i32.const=102 i32.store i64.store unreachable | 0: i64.store*2 i32.store call brz>4,5 | 0: i64.load*2 i32.store i32.const=0 return | 0: i32.const=1 return | " ROW
   s" a framed output is stored only where it ends inside the stack region" T-LABEL
   s" PU" 17 17
   s" 2: i32.load i32.const=136 i32.sub i64.load*17 i32.store br>1 | 17: i32.load i32.const=136 i32.add i64.extend_i32_u i64.const=200704 i64.gt_s brz>3,2 | 0: i32.const=102 i32.store i64.store unreachable | 0: i64.store*17 i32.store i32.const=0 return | " ROW ;

\ ---- the refusals -------------------------------------------------------------
: TRAP-RUN ( -- )
   CNUM-OVERFLOW:TRAP [: COMPILE drop ;] BOUND ;

\ A second selection of one frozen module, with no DECLARE of its own.
: AGAIN-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c FROZEN {: m:IR-BUILD:module :}
   DECLARED
   SES @ m NBACK:SELECT drop
   SES @ m NBACK:SELECT drop ;

\ The module a selection answers, handed to selection.
: RESELECT-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c COMPILE {: w:IR-BUILD:module :}
   DECLARED
   SES @ w NBACK:SELECT drop ;

\ `PZ ( n -- )`, a call the elaborator never writes, built straight in HIR. Its
\ wordcall's callee takes one cell and leaves Z-OUT, and the call carries Z-OPS
\ copies of the argument and Z-RES cell results, each after its token.
variable Z-OPS
variable Z-RES
variable Z-OUT

\ The first token, the wordcall on it and the argument, and a bare return.
: Z-BODY ( IR-ID:ir-value-id -- )
   {: a:IR-ID:ir-value-id :}
   HIR-OPCODE:MEM Z-OPEN
   ZC ZB  ZC ZB HIR:MEM-TYPE  IR-BUILD:ADD-RESULT
   ZC ZB IR-BUILD:END-OP {: m:IR-ID:ir-op-id :}
   HIR-OPCODE:WORDCALL Z-OPEN
   ZC ZB  ZC ZB m 0 IR-BUILD:OP-RESULT@  IR-BUILD:ADD-OPERAND
   Z-OPS @ 0 ?do  ZC ZB a IR-BUILD:ADD-OPERAND  loop
   ZC ZB  ZC ZB HIR:MEM-TYPE  IR-BUILD:ADD-RESULT
   Z-RES @ 0 ?do  ZC ZB  ZC ZB HIR:CELL-TYPE  IR-BUILD:ADD-RESULT  loop
   ZC ZB HIR:KEY-ENTRY  s" SEL-INC" NDICT:CALL-TARGET  Z-INT
   ZC ZB HIR:KEY-IN  1  Z-INT
   ZC ZB HIR:KEY-OUT  Z-OUT @  Z-INT
   ZC ZB IR-BUILD:END-OP drop
   HIR-OPCODE:RETURN Z-OPEN
   ZC ZB IR-BUILD:END-OP drop ;

: SHAPED-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c s" PZ" Z-SOURCE
   s" PZ" 1 0 Z-BEGIN
   ZC ZB  ZC ZB HIR:CELL-TYPE  IR-BUILD:ADD-BLOCK-ARG  Z-BODY
   c Z-SELECT drop ;

\ PZ's call shaped as stated, refused by its code.
: SHAPED ( n n n -- )
   {: ops:n res:n out:n :}
   ops Z-OPS !  res Z-RES !  out Z-OUT !
   1 DECL-IN !  0 DECL-OUT !
   [: [: SHAPED-BODY ;] WRAPPED ;] E-WSEL-CALL TTHROWSQ ;

\ `PV ( -- 17 cells )`, framed, with a bare return: its signature states no
\ output lane, so only the selector can see the seventeen cells it never wrote.
: BARE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c s" PV" Z-SOURCE
   s" PV" 0 17 Z-BEGIN
   HIR-OPCODE:RETURN Z-OPEN
   ZC ZB IR-BUILD:END-OP drop
   c Z-SELECT drop ;

: REFUSAL-ROWS ( -- )
   s" a load is the checked-memory sibling's, refused by name" T-LABEL
   s" PR @" 1 1 E-WSEL-REFUSED REFUSED
   s" an add a trapping unit may trap on is refused, never wrapped" T-LABEL
   s" PT +" 2 1 SOURCE!
   [: TRAP-RUN ;] E-WSEL-TRAP TTHROWSQ
   s" a selection takes its own DECLARE: a second one without is refused" T-LABEL
   s" PA +" 2 1 SOURCE!
   [: [: AGAIN-BODY ;] WRAPPED ;] E-WSEL-DECLARE TTHROWSQ
   s" a DECLARE whose arity is not the definition's is refused" T-LABEL
   s" PA +" 2 1 SOURCE!
   1 DECL-IN !
   [: [: COMPILE drop ;] WRAPPED ;] E-WSEL-DECLARE TTHROWSQ
   s" a module that is not HIR is refused" T-LABEL
   s" PA +" 2 1 SOURCE!
   [: [: RESELECT-BODY ;] WRAPPED ;] E-WSEL-SOURCE TTHROWSQ
   s" a call whose results are not its token, kept row and outputs is refused" T-LABEL
   1 1 0 SHAPED
   s" a call with fewer operands than its token and arguments is refused" T-LABEL
   0 0 1 SHAPED
   s" a framed return that leaves none of its 17 output cells is refused" T-LABEL
   0 DECL-IN !  17 DECL-OUT !
   [: [: BARE-BODY ;] WRAPPED ;] E-WSEL-DECLARE TTHROWSQ ;

\ ---- the admission matrix -------------------------------------------------------
\ Every HIR opcode answers: selected here, or the sibling that selects it.
variable N-SEL
variable N-TAIL
variable N-MEM
variable N-DYN

: ADMIT+ ( WSEL:admission -- )
   MATCH WSEL:admission
      selected       OF 1 N-SEL +! ENDOF
      unbounded-tail OF 1 N-TAIL +! ENDOF
      memory         OF 1 N-MEM +! ENDOF
      dynamic        OF 1 N-DYN +! ENDOF
   ;MATCH ;

: COUNTS-RESET ( -- )
   0 N-SEL !  0 N-TAIL !  0 N-MEM !  0 N-DYN ! ;

: ADMISSION-ROWS ( -- )
   COUNTS-RESET
   HIR:OPCODES 0 ?do  i HIR:NTH WSEL:ADMISSION ADMIT+  loop
   s" every HIR opcode is selected or names its sibling" T-LABEL
   N-SEL @ N-TAIL @ + N-MEM @ + N-DYN @ +  HIR:OPCODES T=
   s" calls, status, integers and the 17 f64 operations: 42 selected here" T-LABEL
   N-SEL @ N-TAIL @ + 42 T=
   s" checked memory and the quotation descriptor are the siblings'" T-LABEL
   N-MEM @ 4 T=  N-DYN @ 1 T=
   s" the matrix says a tail call, self or host, is not bounded space" T-LABEL
   N-TAIL @ 2 T=
   COUNTS-RESET
   HIR-OPCODE:CALL WSEL:ADMISSION ADMIT+
   HIR-OPCODE:WORDCALL WSEL:ADMISSION ADMIT+
   N-TAIL @ 2 T= ;

public
: RUN ( -- )
   WPROF:V1 INSTALL
   ADMISSION-ROWS
   INTEGER-ROWS
   LOOP-ROWS
   CALL-ROWS
   FRAME-ROWS
   STACK-ROWS
   REFUSAL-ROWS
   T-REPORT ;

;package

WSEL-TEST:RUN

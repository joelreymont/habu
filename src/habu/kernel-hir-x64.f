\ kernel-hir-x64.f - one x86-64 routine compiled from a staged HIR function,
\ package X64KHIR: the seam the x86-64 kernel builds its pure-op rows through
\ (src/habu/kernel-x64.f PRIM-HIR), so a row's body is the compiler's own
\ lowering of the operations a compiled word stages for it.
\
\ COMPILE opens a context under X64ABI:BINDING and a HIR module whose source
\ text is the row's name, so every span names the row. It stages one function
\ of `in` cells: the stager builds its body with ARG (argument 0 is the
\ deepest cell), LIT, OP1, OP2 and FETCH, and names each answer, deepest
\ first, with RESULT. OP1 and OP2 answer the type the opcode's schema declares,
\ so a float row crosses its cells into reals and its real answer back. The
\ function then goes down the backend rows in the order
\ src/compiler/native/compiler.f runs them: declare with no linkage, select,
\ prune and lower to a fixpoint. X64PASS:EMIT-UNPLACED seals a shadow
\ emission, since the kernel lays the bytes where its stream is and links the
\ calls itself, and `use` reads it through X64EMIT's readers before the retire
\ row gives it back.
\
\ A stager that leaves a result count other than `out`, or reads an argument
\ the function does not take, ends the build with REFUSE-RC.
require lib/prelude.f
require src/compiler/ir/id.f
require src/compiler/ir/symbol.f
require src/compiler/ir/context.f
require src/compiler/ir/build.f
require src/compiler/native/backend.f
require src/compiler/native/hir.f
require src/compiler/native/emit-x64.f
require src/arch/x86-64/abi.f
require src/arch/x86-64/passes.f
require src/compiler/session/emission.f
require src/compiler/session/lease.f
require src/compiler/session/backend.f

package X64KHIR
private

76 constant REFUSE-RC                   \ X64KERNEL:REFUSE-RC's, a body it lacks
8 constant ARG-CAP                      \ the most cells a row takes
8 constant RES-CAP                      \ and the most it leaves

\ The module the stager builds into, held while it runs: a stager is a
\ quotation, which captures nothing.
1 TYPED-BUFFER W-CTX IR-CTX:ctx
1 TYPED-BUFFER W-BLD IR-BUILD:builder
1 TYPED-BUFFER W-SRC IR-ID:ir-source-id
1 TYPED-BUFFER W-TOK IR-ID:ir-value-id  \ the memory the next fetch reads
TYPED-VARIABLE W-LEASE NLEASE:lease
TYPED-VARIABLE W-SESSION NSESSION:session
ARG-CAP TYPED-BUFFER ARGV IR-ID:ir-value-id
RES-CAP TYPED-BUFFER RESV IR-ID:ir-value-id
PTR-VARIABLE NAME-A                     \ the row's name, the module's source
variable NAME-U
variable N-ARGS
variable N-RES
variable TOK?                           \ nonzero once the function has a token

: CC ( -- IR-CTX:ctx ) 0 W-CTX @ ;
: BB ( -- IR-BUILD:builder ) 0 W-BLD @ ;

: SPAN ( -- IR-SOURCE:span )
   BB 0 W-SRC @ 0 NAME-U @ IR-BUILD:ADD-SPAN ;

: CELLT ( -- IR-ID:ir-type-id )
   CC BB IR--TYPE-WIDTH:W64 IR--TYPE-SIGN:SIGNED IR-BUILD:INTERN-INT ;

: MEMT ( -- IR-ID:ir-type-id )
   CC BB HIR:MEM-TYPE ;

: REFUSE ( ptr u8 n -- )
   s" x64khir: " type  NAME-A @ NAME-U @ type  s"  " type  type cr
   s" x64khir: a stager does not fit its row" REFUSE-RC die ;

\ ---- the module and its one function -----------------------------------------
: MODULE ( IR-CTX:ctx ptr u8 n -- )
   {: c:IR-CTX:ctx a:ptr u:n :}
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   c HIR:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b HIR:REGISTER
   b 0 W-BLD !
   a NAME-A !
   u NAME-U !
   c b a u IR-BUILD:ADD-SOURCE 0 W-SRC ! ;

: SIGN ( n n -- IR-ID:ir-type-id )
   {: in:n out:n :}
   CELLT {: t:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   in 0 ?do t IR-TYPE:FN-PARAM loop
   out 0 ?do t IR-TYPE:FN-RESULT loop
   CC BB IR-BUILD:INTERN-CODE-REF ;

: OPEN-FUN ( ptr u8 n n n -- )
   {: a:ptr u:n in:n out:n :}
   in ARG-CAP > if s" takes more cells than the kernel stages" REFUSE then
   CC BB  CC BB a u IR-BUILD:INTERN-SYMBOL  IR-BUILD:BEGIN-FUN
   CC BB  in out SIGN  IR-BUILD:SET-SIGNATURE
   CC BB IR--FUN-LINKAGE:DEFINED IR-BUILD:SET-LINKAGE
   CC BB IR--FUN-VISIBILITY:EXPORTED IR-BUILD:SET-VISIBILITY
   CC BB IR--FUN-CONVENTION:HABU IR-BUILD:SET-CONVENTION
   CC BB SPAN IR-BUILD:SET-FUN-SPAN
   CC BB IR-BUILD:BEGIN-BLOCK
   CC BB SPAN IR-BUILD:SET-BLOCK-SPAN
   in 0 ?do  CC BB CELLT IR-BUILD:ADD-BLOCK-ARG  i ARGV !  loop
   in N-ARGS !
   0 N-RES !
   0 TOK? ! ;

: OPEN-OP ( HIR:opcode -- )
   {: o:HIR:opcode :}
   CC BB  CC BB o HIR:OPCODE  IR-BUILD:BEGIN-OP
   CC BB SPAN IR-BUILD:SET-OP-SPAN ;

: OPERAND ( IR-ID:ir-value-id -- )
   {: v:IR-ID:ir-value-id :}
   CC BB v IR-BUILD:ADD-OPERAND ;

: CLOSE-OP ( -- IR-ID:ir-op-id )
   CC BB IR-BUILD:END-OP ;

: RESULT-AT ( IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: id:IR-ID:ir-op-id k:n :}
   CC BB id k IR-BUILD:OP-RESULT@ ;

: CELL-RESULT ( -- ) CC BB CELLT IR-BUILD:ADD-RESULT ;

\ The answer an operation's schema declares: a cell for the integer operations,
\ a real for the float ones, whose answer the verifier holds to that type.
: SCHEMA-RESULT ( HIR:opcode -- )
   {: o:HIR:opcode :}
   CC BB  CC BB  CC BB o HIR:OPCODE 0 IR-BUILD:SCHEMA-RESULT@  IR-BUILD:ADD-RESULT ;

\ The memory the function is entered with, minted where the first fetch needs
\ it; each fetch then answers the memory after it.
: TOKEN ( -- IR-ID:ir-value-id )
   TOK? @ 0= if
      HIR-OPCODE:MEM OPEN-OP
      CC BB MEMT IR-BUILD:ADD-RESULT
      CLOSE-OP 0 RESULT-AT 0 W-TOK !
      1 TOK? !
   then
   0 W-TOK @ ;

: RETURN ( -- )
   HIR-OPCODE:RETURN OPEN-OP
   N-RES @ 0 ?do  i RESV @ OPERAND  loop
   CLOSE-OP drop
   CC BB IR-BUILD:END-BLOCK drop
   CC BB IR-BUILD:END-FUN drop ;

\ ---- the chain ---------------------------------------------------------------
: CHAIN ( ptr u8 n n n [ -- ] [ NART:emission -- ] NSESSION:session -- )
   {: a:ptr u:n in:n out:n stage use s:NSESSION:session :}
   s W-SESSION !
   s NSESSION:RESOLVE drop {: c:IR-CTX:ctx :}
   c 0 W-CTX !
   c a u MODULE
   a u in out OPEN-FUN
   stage execute
   N-RES @ out <> if s" leaves another result count" REFUSE then
   RETURN
   s in out NBACK:L-NONE NBACK:DECLARE
   s BB NBACK:FREEZE {: hm:IR-BUILD:module :}
   s hm NBACK:SELECT {: m0:IR-BUILD:module :}
   hm IR-BUILD:RETIRE
   s m0 NBACK:PRUNE {: m1:IR-BUILD:module :}
   s m1 NBACK:FIXPOINT {: m:IR-BUILD:module :}
   s m NBACK:EMIT-UNPLACED
   c NART:COPY use execute ;

: CLEAN ( -- )
   W-SESSION @ NBACK:RELEASE
   W-SESSION @ NBACK:RETIRE ;

: WORK ( ptr u8 n n n [ -- ] [ NART:emission -- ] NSESSION:session -- )
   [: CHAIN ;] [: CLEAN ;] finally ;

: BODY ( ptr u8 n n n [ -- ] [ NART:emission -- ] IR-CTX:ctx -- )
   W-LEASE @ NSESSION:NEW [: WORK ;] NSESSION:WITH-WORK ;

: OWNED ( ptr u8 n n n [ -- ] [ NART:emission -- ] NLEASE:lease -- )
   W-LEASE !
   X64ABI:BINDING [: BODY ;] IR-CTX:WITH-CONTEXT ;

public

\ ---- what a stager builds with -----------------------------------------------
\ Argument n, 0 the deepest cell the row takes.
: ARG ( n -- IR-ID:ir-value-id )
   {: k:n :}
   k 0 <  k N-ARGS @ >=  or if s" reads an argument the row does not take" REFUSE then
   k ARGV @ ;

\ The next answer, deepest first.
: RESULT ( IR-ID:ir-value-id -- )
   {: v:IR-ID:ir-value-id :}
   N-RES @ RES-CAP >= if s" leaves more cells than the kernel stages" REFUSE then
   v N-RES @ RESV !
   N-RES @ 1+ N-RES ! ;

: LIT ( n -- IR-ID:ir-value-id )
   {: v:n :}
   HIR-OPCODE:CONST OPEN-OP
   CELL-RESULT
   CC BB  CC BB HIR:KEY-VALUE  CC BB v IR-BUILD:INTERN-INT-ATTR  IR-BUILD:ADD-ATTR
   CC BB  CC BB HIR:KEY-ADDR  CC BB HIR:ADDR-NONE HIR:ADDR-ATTR  IR-BUILD:ADD-ATTR
   CLOSE-OP 0 RESULT-AT ;

: OP1 ( IR-ID:ir-value-id HIR:opcode -- IR-ID:ir-value-id )
   {: x:IR-ID:ir-value-id o:HIR:opcode :}
   o OPEN-OP  x OPERAND  o SCHEMA-RESULT
   CLOSE-OP 0 RESULT-AT ;

\ The deeper operand first, as the source order is: `a b sub` is a - b.
: OP2 ( IR-ID:ir-value-id IR-ID:ir-value-id HIR:opcode -- IR-ID:ir-value-id )
   {: x:IR-ID:ir-value-id y:IR-ID:ir-value-id o:HIR:opcode :}
   o OPEN-OP  x OPERAND  y OPERAND  o SCHEMA-RESULT
   CLOSE-OP 0 RESULT-AT ;

\ A load, `load` or `bload`, of the address.
: FETCH ( IR-ID:ir-value-id HIR:opcode -- IR-ID:ir-value-id )
   {: x:IR-ID:ir-value-id o:HIR:opcode :}
   TOKEN {: t:IR-ID:ir-value-id :}
   o OPEN-OP  x OPERAND  t OPERAND
   CELL-RESULT  CC BB MEMT IR-BUILD:ADD-RESULT
   CLOSE-OP {: id:IR-ID:ir-op-id :}
   id 1 RESULT-AT 0 W-TOK !
   id 0 RESULT-AT ;

\ ---- the seam ----------------------------------------------------------------
\ Compile the function the stager builds for the row named, of `in` cells and
\ `out` results, and run `use` while its sealed emission stands.
: COMPILE ( ptr u8 n n n [ -- ] [ NART:emission -- ] -- )
   [: OWNED ;] NLEASE:WITH ;

;package

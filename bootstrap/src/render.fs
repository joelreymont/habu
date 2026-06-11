\ render.fs — RENDER-EFFECT: resolved effect -> canonical signature text.
\ =GENERALIZE: free type vars named a b c… and row vars R S T… by first
\ appearance across the whole effect. Output is re-parseable by PARSE-SIG and
\ matches the surface grammar in PLAN. Shared with diagnostics.
\
\ Grammar emitted, single space between tokens:
\   <eff>   = <din> -- <dout> [ SPACE | SPACE <rin> -- <rout> ]
\   <stack> = <rowname> { SPACE <type> }      bottom->top, tail row var first
\   <type>  = i64|u8|u32|cell|bool|char|str|addr | a|b|… | ptr SPACE <type>
\             | [ SPACE <din> -- <dout> SPACE ]    quotation, recurse
\ The return clause is appended only when rin/rout are NOT the trivial untouched
\ case: the same single row var with no pushes on either side.

\ --- output buffer ---
4096 constant RENDER-CAP            \ canonical text never approaches this
create RENDER-BUF  RENDER-CAP allot
variable RENDER-LEN                 \ bytes used in RENDER-BUF
create CH-SCRATCH  1 chars allot    \ one-byte staging for EMIT-CH

: BUF-RESET ( -- )   0 RENDER-LEN ! ;

\ Append c-addr u to the buffer.
: EMIT$ ( c-addr u -- )
   dup RENDER-LEN @ + RENDER-CAP > if E-ARENA throw then
   RENDER-BUF RENDER-LEN @ +          ( c-addr u dst )
   over RENDER-LEN +!                 \ bump len by u
   swap move ;                        \ move ( src dst u )

: EMIT-CH ( c -- )   CH-SCRATCH c!  CH-SCRATCH 1 EMIT$ ;
: SPACE$  ( -- )     bl EMIT-CH ;

\ --- render-time id -> name maps. Names by first appearance. ---
\ A var's slot holds its 1-based first-appearance index (0 = unseen). The SEEN
\ counters give the next index. Reset at the start of each RENDER-EFFECT.
create TV-NAME  MAX-TV cells allot    \ TV-NAME[id] = index+1, 0 = unseen
create RV-NAME  MAX-RV cells allot
variable TV-SEEN                      \ distinct type vars named so far
variable RV-SEEN                      \ distinct row vars named so far

: NAMES-RESET ( -- )
   MAX-TV 0 ?do  0 i cells TV-NAME + !  loop
   MAX-RV 0 ?do  0 i cells RV-NAME + !  loop
   0 TV-SEEN !  0 RV-SEEN ! ;

\ Emit the n-th distinct name (n 0-based) over a letter base ('a' or 'R'):
\ n<26 -> single letter; else letter + decimal suffix (n/26).
: EMIT-NAME ( n base -- )
   over 26 mod + EMIT-CH              ( n )
   26 / dup if 0 .r else drop then ;

\ First-appearance index of id in the given map+counter (0-based).
: VAR-INDEX ( id slot-addr counter -- n )
   {: slot counter :}                ( id )
   cells slot + {: a :}
   a @ 0= if  counter @ 1+ a !  counter @ 1+ counter !  then
   a @ 1- ;

: EMIT-TVNAME ( id -- )
   TV-NAME TV-SEEN VAR-INDEX  [char] a EMIT-NAME ;
: EMIT-RVNAME ( id -- )
   RV-NAME RV-SEEN VAR-INDEX  [char] R EMIT-NAME ;

\ --- concrete type-code names. s" inside a colon def compiles the bytes
\ permanently, so each branch yields a stable ( c-addr u ). One branch per code
\ keyed off TC-* — a closed set, no magic numbers. ---
: CODE$ ( code -- c-addr u )
   case
     TC-I64  of  s" i64"  endof
     TC-U8   of  s" u8"   endof
     TC-U32  of  s" u32"  endof
     TC-CELL of  s" cell" endof
     TC-BOOL of  s" bool" endof
     TC-CHAR of  s" char" endof
     TC-STR  of  s" str"  endof
     TC-ADDR of  s" addr" endof
     TC-F64  of  s" f64"  endof
     E-BADTYPE throw
   endcase ;
: EMIT-CODE ( code -- )   CODE$ EMIT$ ;

\ --- type / stack renderers (mutually recursive via DEFER) ---
defer RENDER-TYPE   ( t -- )
defer RENDER-STACK  ( s -- )

\ Render an effect's data part as  <din> -- <dout> , no return clause.
: RENDER-DATA ( eff -- )
   dup EFF>DIN  RESOLVE-ROW RENDER-STACK
   s"  -- " EMIT$
   EFF>DOUT RESOLVE-ROW RENDER-STACK ;

: RENDER-PTR ( t -- )
   s" ptr " EMIT$  PTR>INNER RESOLVE-TYPE RENDER-TYPE ;

: RENDER-QUOT ( t -- )
   s" [ " EMIT$  QUOT>EFF RENDER-DATA  s"  ]" EMIT$ ;

: (RENDER-TYPE) ( t -- )
   dup TYVAR? if  TERM>PAYLOAD EMIT-TVNAME  exit then
   dup TYCON? if  TERM>PAYLOAD EMIT-CODE    exit then
   dup TERM>TAG T-PTR = if  RENDER-PTR  exit then
   RENDER-QUOT ;
' (RENDER-TYPE) is RENDER-TYPE

\ Render a stack bottom->top: tail row var, then each pushed type top-last.
\ Recurse on rest first, emit a space, then the top type.
: (RENDER-STACK) ( s -- )
   dup SROW? if  TERM>PAYLOAD EMIT-RVNAME  exit then
   dup STACK-REST RESOLVE-ROW RENDER-STACK   \ deeper part first
   SPACE$
   STACK-TOP RESOLVE-TYPE RENDER-TYPE ;
' (RENDER-STACK) is RENDER-STACK

\ Trivial return case: rin and rout are the SAME single row var, no pushes.
: TRIVIAL-RET? ( eff -- f )
   dup EFF>RIN  RESOLVE-ROW {: rin :}
   EFF>ROUT RESOLVE-ROW {: rout :}
   rin SROW? rout SROW? and
   rin TERM>PAYLOAD rout TERM>PAYLOAD = and ;

: RENDER-RET ( eff -- )
   s"  | " EMIT$
   dup EFF>RIN  RESOLVE-ROW RENDER-STACK
   s"  -- " EMIT$
   EFF>ROUT RESOLVE-ROW RENDER-STACK ;

\ RENDER-EFFECT — public: canonical text of the resolved effect.
: RENDER-EFFECT ( eff -- c-addr u )
   BUF-RESET  NAMES-RESET
   dup RENDER-DATA
   dup TRIVIAL-RET? 0= if  dup RENDER-RET  then
   drop
   RENDER-BUF RENDER-LEN @ ;

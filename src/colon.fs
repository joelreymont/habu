\ colon.fs — override `:` so checked code is ordinary Forth. A checked
\ definition is parsed (name, effect, body), the body is checked, and on success
\ the real word is compiled by the native colon via a reentrancy guard.

variable RE-EMIT?
variable CHECKING-ON?   CHECKING-ON? on   \ toggle: off → `:` is exactly native
' : constant NCOLON                      \ the native colon, saved before override

\ Outcome of the most recent checked `:` (for the REPL/tools to read):
\   -2 = not a checked def (native colon)   0 = checked OK
\   E-UNCHECKED/E-UNSAFE = escaped checking   else = the type-error code.
-2 constant CK-NONE
variable CHECK-CODE   CK-NONE CHECK-CODE !

create NM-BUF 128 chars allot   variable NM-LEN
create EF-BUF 256 chars allot   variable EF-LEN
: NM! ( a u -- )  128 min dup NM-LEN !  NM-BUF swap move ;
: EF! ( a u -- )  256 min dup EF-LEN !  EF-BUF swap move ;
: NM@ ( -- a u )  NM-BUF NM-LEN @ ;
: EF@ ( -- a u )  EF-BUF EF-LEN @ ;

\ clean catch target: check the captured definition (charts it on success).
: CHECK-CUR ( -- )  NM@ EF@ CAP$ CHECK-DEF ;

\ Re-emit ": NAME body ;" under the native colon.
create RE-BUF 1280 chars allot  variable RE-LEN
: RE+ ( a u -- )
   dup RE-LEN @ + 1280 > if E-ARENA throw then
   {: a u :}  a  RE-BUF RE-LEN @ chars +  u move  u RE-LEN +! ;
\ Typed locals `a:u8` are checked with the type but must compile as the bare
\ name `a` (gforth locals are untyped). Re-emit strips `:type` inside { … } / {: … :}.
variable RB-A   variable RB-U   variable IN-LOC
: RB-TOK ( -- a u )                       \ next space-token from CAP$ cursor
   begin RB-U @ 0> RB-A @ c@ bl = and while 1 RB-A +! -1 RB-U +! repeat
   RB-A @ 0  begin RB-U @ 0> RB-A @ c@ bl <> and while 1+ 1 RB-A +! -1 RB-U +! repeat ;
: NAME-PART ( a u -- a u' )  2dup [char] : scan nip - ;   \ truncate at ':'
: RE+BODY ( -- )                          \ append CAP$, stripping :type in locals
   CAP$ RB-U ! RB-A !  IN-LOC off
   begin RB-TOK dup 0> while  {: a u :}
      a u s" {:" CI=  a u s" {" CI= or if  IN-LOC on   a u
      else a u s" :}" CI=  a u s" }" CI= or if  IN-LOC off  a u
      else a u s" --" CI= if  a u
      else IN-LOC @ if a u NAME-PART else a u then
      then then then
      RE+  s"  " RE+
   repeat 2drop ;
: RE-EVAL ( -- )
   0 RE-LEN !
   s" : " RE+  NM@ RE+  s"  " RE+  RE+BODY  s"  ; " RE+
   RE-EMIT? on   RE-BUF RE-LEN @ evaluate   RE-EMIT? off ;

\ Re-emit, but never let a native-compile failure abort the whole load.
: RE-EVAL-SAFE ( -- )
   ['] RE-EVAL catch if
      RE-EMIT? off
      cr ." caf: " NM@ type ."  — checker accepted but native colon rejected it" cr
   then ;

\ A definition that escaped checking must SAY SO — never silently pass an
\ unverified typed signature as if it were checked.
: WARN-UNCHECKED ( code -- )
   cr ." caf: WARNING: " NM@ type ."  was NOT type-checked ("
   E-UNSAFE = if ." forbidden" else ." unmodeled" then ."  word in body) — compiled natively" cr ;

\ Does the captured effect text parse as a typed signature? (Throws if not — a
\ plain stack comment like ( n -- n2 ) is not a valid sig, so it falls back.)
: TRY-SIG ( -- )  ARENA-RESET  EF@ PARSE-SIG drop ;

\ A definition is CHECKED iff its name is immediately followed by `( … )` whose
\ text parses as a typed signature. Otherwise `:` is the ordinary Forth colon.
: :  ( -- )
   RE-EMIT? @ if NCOLON execute exit then      \ re-emit pass → native colon
   CHECKING-ON? @ 0= if NCOLON execute exit then  \ checking disabled → native colon
   >in @ {: save :}                            \ to backtrack for unchecked defs
   parse-name NM!
   parse-name s" (" compare 0= if
      [char] ) parse EF!                       \ effect/comment text (copied)
      ['] TRY-SIG catch if                     \ not a typed sig → native colon
         save >in !  NCOLON execute  exit  then
      CAPTURE-BODY 2drop
      \ Overlay rule: check what we can, never break valid Forth. A real type
      \ error on modeled code is reported and the def refused; a body that uses
      \ words the checker doesn't model (E-UNCHECKED) or disallows (E-UNSAFE)
      \ falls back to the native colon so existing code still compiles.
      ['] CHECK-CUR catch {: code :}
      code CHECK-CODE !                          \ record outcome for the REPL/tools
      code 0= if RE-EVAL-SAFE CODEGEN-HOOK exit then
      code E-UNCHECKED = code E-UNSAFE = or if
         code WARN-UNCHECKED  RE-EVAL-SAFE  CODEGEN-HOOK  exit  \ escaped checking — say so
      then
      code DIAG-CODE!  DIAG-REPORT  exit   \ real type error → report, refuse
   then
   CK-NONE CHECK-CODE !                          \ unchecked native colon
   save >in !  NCOLON execute ;                \ no effect → native colon

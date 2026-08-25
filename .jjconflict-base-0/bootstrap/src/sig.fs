\ sig.fs — signature parser (INST) + canonical renderer (GENERALIZE) + effect DB.
\ PARSE-SIG reads a signature string into a 4-row effect, allocating FRESH type
\ and row vars by name (call it twice -> independent vars = fresh instantiation).
\ RENDER-EFFECT prints a resolved effect to its canonical string; the two
\ round-trip. CHART/EFFECT-OF store/retrieve schemes by name in the EFFECTS
\ wordlist. The primitive table is authored here at load time.

\ ---------------------------------------------------------------------------
\ Tokenizer: copy the sig into a private buffer, hand out bl-separated tokens.
\ ---------------------------------------------------------------------------
256 constant SIG-BUF-MAX
create SIG-BUF  SIG-BUF-MAX chars allot
variable SIG-LEN     \ live length of SIG-BUF
variable SIG-POS     \ cursor into SIG-BUF

: SIG-COPY  ( c-addr u -- )   \ stash the input, reset the cursor
   SIG-BUF-MAX min  dup SIG-LEN !
   SIG-BUF swap move  0 SIG-POS ! ;

: SIG-AT-END?  ( -- f )   SIG-POS @ SIG-LEN @ >= ;
: SIG-CUR      ( -- c )   SIG-BUF SIG-POS @ chars + c@ ;

: SIG-SKIP-BL  ( -- )     \ advance over leading blanks
   begin  SIG-AT-END? if exit then  SIG-CUR bl = while
      1 SIG-POS +!  repeat ;

\ Next bl-separated token. Returns ( c-addr u ), u=0 at end of input.
: SIG-TOKEN  ( -- c-addr u )
   SIG-SKIP-BL
   SIG-AT-END? if  SIG-BUF 0  exit then
   SIG-BUF SIG-POS @ chars +              ( start )
   begin  SIG-AT-END? 0=  SIG-CUR bl <> and while
      1 SIG-POS +!  repeat
   SIG-BUF SIG-POS @ chars +  over -      ( start len )
   ;

\ One-token lookahead so leading-row-var detection can back off.
variable TOK-A    \ pushed-back token addr
variable TOK-U    \ pushed-back token len (-1 = none pending)
: PEEK-RESET  ( -- )   -1 TOK-U ! ;
: NEXT-TOK    ( -- c-addr u )
   TOK-U @ -1 = if  SIG-TOKEN  exit then
   TOK-A @ TOK-U @  -1 TOK-U ! ;
: PUSH-TOK    ( c-addr u -- )   TOK-U !  TOK-A ! ;

\ ---------------------------------------------------------------------------
\ Per-parse var tables: letter -> allocated id, or -1 if unseen.
\ Reset each PARSE-SIG so the same string re-parses to fresh ids.
\ ---------------------------------------------------------------------------
create TVAR-TAB  26 cells allot   \ a..z -> type-var id
create RVAR-TAB  26 cells allot   \ A..Z -> row-var id
variable IMP-DROW   \ shared implicit data row var (-1 until first use)
variable IMP-RROW   \ shared implicit return row var

: VARTAB-RESET  ( -- )
   26 0 ?do  -1  TVAR-TAB i cells +  !   -1  RVAR-TAB i cells +  !  loop
   -1 IMP-DROW !  -1 IMP-RROW ! ;

\ a..z -> a fresh-or-cached type-var term
: TVAR-OF  ( c -- t )
   [char] a -  cells TVAR-TAB +  {: slot :}
   slot @ -1 = if  1 TV-ALLOC  slot !  then
   slot @ MK-VAR ;

\ A..Z -> a fresh-or-cached row-var id
: RVAR-OF  ( c -- id )
   [char] A -  cells RVAR-TAB +  {: slot :}
   slot @ -1 = if  1 RV-ALLOC  slot !  then
   slot @ ;

: IMP-DROW-ID  ( -- id )
   IMP-DROW @ -1 = if  1 RV-ALLOC IMP-DROW !  then  IMP-DROW @ ;
: IMP-RROW-ID  ( -- id )
   IMP-RROW @ -1 = if  1 RV-ALLOC IMP-RROW !  then  IMP-RROW @ ;

\ ---------------------------------------------------------------------------
\ Token classifiers.
\ ---------------------------------------------------------------------------
: LOWER?     ( c -- f )   dup [char] a >=  swap [char] z <=  and ;
: UPPER?     ( c -- f )   dup [char] A >=  swap [char] Z <=  and ;
: TYVAR-TOK? ( c-addr u -- f )   dup 1 = if drop c@ LOWER? else 2drop false then ;
: RVAR-TOK?  ( c-addr u -- f )   dup 1 = if drop c@ UPPER? else 2drop false then ;

\ Concrete type-name table: name string -> type code.
: TYNAME>CODE  ( c-addr u -- code true | false )
   2dup s" i64"  compare 0= if 2drop TC-I64  true exit then
   2dup s" u8"   compare 0= if 2drop TC-U8   true exit then
   2dup s" u32"  compare 0= if 2drop TC-U32  true exit then
   2dup s" cell" compare 0= if 2drop TC-CELL true exit then
   2dup s" bool" compare 0= if 2drop TC-BOOL true exit then
   2dup s" char" compare 0= if 2drop TC-CHAR true exit then
   2dup s" str"  compare 0= if 2drop TC-STR  true exit then
   2dup s" addr" compare 0= if 2drop TC-ADDR true exit then
   2drop false ;

\ ---------------------------------------------------------------------------
\ Recursive-descent type parser.  PARSE-TYPE reads one type term from the
\ token stream; PARSE-EFFECT and PARSE-QUOT are mutually recursive with it.
\ ---------------------------------------------------------------------------
defer PARSE-EFFECT-DEF   ( drow-id rrow-id -- e )

: PARSE-TYPE  ( c-addr u -- t )       \ token already taken
   2dup s" ptr" compare 0= if
      2drop  NEXT-TOK RECURSE MK-PTR exit then
   2dup s" [" compare 0= if
      2drop  1 RV-ALLOC  1 RV-ALLOC  PARSE-EFFECT-DEF MK-QUOT exit then
   2dup TYVAR-TOK? if  drop c@ TVAR-OF exit then
   2dup TYNAME>CODE if  nip nip MK-CON exit then
   E-BADTYPE throw ;

\ Read one clause (leading optional row var + elements) up to a stop token.
\ stop is checked by the caller; we return the built stack and leave the stop
\ token pushed back.  drow-id is the row var to use if no explicit one given.
\ Terminators: "--"  "|"  "]"  end-of-input.
: CLAUSE-STOP?  ( c-addr u -- f )
   dup 0= if 2drop true exit then
   2dup s" --" compare 0= if 2drop true exit then
   2dup s" |"  compare 0= if 2drop true exit then
        s" ]"  compare 0= ;

\ Build a stack: leading row var (explicit or implicit), then push each elem.
: PARSE-CLAUSE  ( default-rowid -- s )
   NEXT-TOK 2dup RVAR-TOK? if
      drop c@ RVAR-OF nip            ( rowid )
   else                               ( default tok-a tok-u )
      PUSH-TOK                        ( default )
   then  MK-ROW                       ( s )
   begin  NEXT-TOK 2dup CLAUSE-STOP? 0= while  ( s c-addr u )
      PARSE-TYPE MK-PUSH
   repeat  PUSH-TOK ;                  \ leave stop token pushed back

\ A full effect: Din -- Dout [ | Rin -- Rout ].  drow/rrow are the row ids the
\ implicit (omitted) leading row vars resolve to within this effect.
: PARSE-EFFECT  ( drow-id rrow-id -- e )
   {: drow rrow :}
   drow PARSE-CLAUSE                  ( din )
   NEXT-TOK 2drop                     \ consume "--"
   drow PARSE-CLAUSE                  ( din dout )
   NEXT-TOK 2dup s" |" compare 0= if  ( din dout c-addr u )
      2drop
      rrow PARSE-CLAUSE               ( din dout rin )
      NEXT-TOK 2drop                  \ consume "--"
      rrow PARSE-CLAUSE              ( din dout rin rout )
   else
      PUSH-TOK                        \ no return clause: untouched return row
      rrow MK-ROW  rrow MK-ROW
   then
   MK-EFFECT ;
' PARSE-EFFECT is PARSE-EFFECT-DEF

\ Quotations get their OWN fresh implicit rows; the top-level effect uses one
\ shared implicit data row and one shared implicit return row.
: PARSE-SIG  ( c-addr u -- eff )
   SIG-COPY  PEEK-RESET  VARTAB-RESET
   IMP-DROW-ID IMP-RROW-ID PARSE-EFFECT ;

\ ---------------------------------------------------------------------------
\ Renderer (GENERALIZE): deeply resolve, name vars by first appearance, print
\ canonical text into a reusable buffer.  Round-trips through PARSE-SIG.
\ ---------------------------------------------------------------------------
1024 constant OUT-BUF-MAX
create OUT-BUF  OUT-BUF-MAX chars allot
variable OUT-LEN

: OUT-RESET  ( -- )   0 OUT-LEN ! ;
: OUT-C      ( c -- )   OUT-BUF OUT-LEN @ chars + c!  1 OUT-LEN +! ;
: OUT-STR    ( c-addr u -- )   0 ?do  dup c@ OUT-C  char+  loop drop ;
: OUT-SP     ( -- )   bl OUT-C ;

\ Var naming: map a raw id to a printed letter by order of first appearance.
\ Separate seen-tables for type vars (a..) and row vars (R..).
create TV-SEEN  MAX-TV cells allot    \ raw id -> printed index (or -1)
create RV-SEEN  MAX-RV cells allot
variable TV-NAMED   \ count of distinct type vars named so far
variable RV-NAMED

: NAME-RESET  ( -- )
   MAX-TV 0 ?do  -1 TV-SEEN i cells + !  loop
   MAX-RV 0 ?do  -1 RV-SEEN i cells + !  loop
   0 TV-NAMED !  0 RV-NAMED ! ;

: TV-LETTER  ( id -- )   \ emit type var as a,b,c,... (wraps with ' after z)
   cells TV-SEEN +  {: slot :}
   slot @ -1 = if  TV-NAMED @ slot !  1 TV-NAMED +!  then
   [char] a slot @ +  OUT-C ;
: RV-LETTER  ( id -- )   \ emit row var as R,S,T,...
   cells RV-SEEN +  {: slot :}
   slot @ -1 = if  RV-NAMED @ slot !  1 RV-NAMED +!  then
   [char] R slot @ +  OUT-C ;

defer RENDER-EFF-INTO   ( e -- )
defer RENDER-TYPE-DEF   ( t -- )

: RENDER-TYPE  ( t -- )
   RESOLVE-TYPE {: t :}
   t TYVAR? if  t TERM>PAYLOAD TV-LETTER exit then
   t TYCON? if
      t TERM>PAYLOAD
      dup TC-I64  = if drop s" i64"  OUT-STR exit then
      dup TC-U8   = if drop s" u8"   OUT-STR exit then
      dup TC-U32  = if drop s" u32"  OUT-STR exit then
      dup TC-CELL = if drop s" cell" OUT-STR exit then
      dup TC-BOOL = if drop s" bool" OUT-STR exit then
      dup TC-CHAR = if drop s" char" OUT-STR exit then
      dup TC-STR  = if drop s" str"  OUT-STR exit then
      dup TC-ADDR = if drop s" addr" OUT-STR exit then
      drop  E-BADTYPE throw
   then
   t TERM>TAG T-PTR = if
      s" ptr" OUT-STR OUT-SP  t PTR>INNER RENDER-TYPE-DEF exit then
   t TERM>TAG T-QUOT = if
      s" [" OUT-STR OUT-SP  t QUOT>EFF RENDER-EFF-INTO  OUT-SP s" ]" OUT-STR exit then
   E-BADTYPE throw ;
' RENDER-TYPE is RENDER-TYPE-DEF

\ Render a stack's elements bottom->top after its (already-emitted) row var.
: RENDER-PUSHES  ( s -- )
   RESOLVE-ROW {: s :}
   s SROW? if exit then
   s STACK-REST RECURSE             \ deeper (bottom) elements first
   OUT-SP  s STACK-TOP RENDER-TYPE ;

\ Render a full stack: leading row var then its pushes.
: RENDER-STACK  ( s -- )
   RESOLVE-ROW {: s :}
   \ find and emit the spine row var
   s  begin dup SROW? 0= while RESOLVE-ROW STACK-REST RESOLVE-ROW repeat
   TERM>PAYLOAD RV-LETTER
   s RENDER-PUSHES ;

\ Are two rows the identity-untouched pair (same row var, no pushes)?
: ROW-UNTOUCHED?  ( rin rout -- f )
   RESOLVE-ROW swap RESOLVE-ROW swap {: a b :}
   a SROW? b SROW? and if  a TERM>PAYLOAD b TERM>PAYLOAD =  exit then
   false ;

: RENDER-EFFECT-BODY  ( e -- )
   {: e :}
   e EFF>DIN  RENDER-STACK
   OUT-SP s" --" OUT-STR
   e EFF>DOUT RENDER-STACK
   e EFF>RIN e EFF>ROUT ROW-UNTOUCHED? 0= if
      OUT-SP s" |" OUT-STR
      e EFF>RIN  RENDER-STACK
      OUT-SP s" --" OUT-STR
      e EFF>ROUT RENDER-STACK
   then ;
' RENDER-EFFECT-BODY is RENDER-EFF-INTO

: RENDER-EFFECT  ( e -- c-addr u )
   OUT-RESET NAME-RESET
   RENDER-EFF-INTO
   OUT-BUF OUT-LEN @ ;

\ ---------------------------------------------------------------------------
\ Effect DB: a wordlist whose words hold the canonical scheme string.
\ ---------------------------------------------------------------------------
wordlist constant EFFECTS

: CHART  ( eff c-addr u -- )          \ render eff, store under name c-addr u
   2>r RENDER-EFFECT                  ( saddr su )  ( R: name )
   get-current >r  EFFECTS set-current
   2r> nextname create                \ define name in EFFECTS holding the string
   r> set-current
   dup ,  here over chars allot swap move ;   ( store len then bytes )

\ Stored body layout: [ len ][ bytes... ].  Fetch it back.
: SCHEME@  ( xt -- saddr su )   >body  dup cell+ swap @ ;

: EFFECT-OF  ( c-addr u -- saddr su | 0 )
   EFFECTS search-wordlist  dup 0= if exit then  ( nt true )
   drop name>interpret SCHEME@ ;

\ ---------------------------------------------------------------------------
\ Primitive table — authored through PARSE-SIG + CHART at load time.
\ ---------------------------------------------------------------------------
\ Helper: chart a primitive given two strings on the stack.
: CHART"  ( name$a name$u sig$a sig$u -- )
   PARSE-SIG -rot CHART ;

s" DUP"    s" R a -- R a a"            CHART"
s" DROP"   s" R a -- R"                CHART"
s" SWAP"   s" R a b -- R b a"          CHART"
s" OVER"   s" R a b -- R a b a"        CHART"
s" ROT"    s" R a b c -- R b c a"      CHART"
s" NIP"    s" R a b -- R b"            CHART"
s" TUCK"   s" R a b -- R b a b"        CHART"
s" ?DUP"   s" R a -- R a a"            CHART"

s" +"      s" R a i64 -- R a"          CHART"
s" -"      s" R i64 i64 -- R i64"      CHART"
s" *"      s" R i64 i64 -- R i64"      CHART"
s" /"      s" R i64 i64 -- R i64"      CHART"
s" MOD"    s" R i64 i64 -- R i64"      CHART"
s" AND"    s" R i64 i64 -- R i64"      CHART"
s" OR"     s" R i64 i64 -- R i64"      CHART"
s" XOR"    s" R i64 i64 -- R i64"      CHART"
s" LSHIFT" s" R i64 i64 -- R i64"      CHART"
s" RSHIFT" s" R i64 i64 -- R i64"      CHART"
s" NEGATE" s" R i64 -- R i64"          CHART"

s" 0="     s" R i64 -- R bool"         CHART"
s" ="      s" R a a -- R bool"         CHART"
s" <"      s" R i64 i64 -- R bool"     CHART"
s" >"      s" R i64 i64 -- R bool"     CHART"
s" <="     s" R i64 i64 -- R bool"     CHART"
s" >="     s" R i64 i64 -- R bool"     CHART"
s" <>"     s" R a a -- R bool"         CHART"

s" @"      s" R ptr a -- R a"          CHART"
s" !"      s" R a ptr a -- R"          CHART"
s" c@"     s" R ptr u8 -- R u8"        CHART"
s" c!"     s" R u8 ptr u8 -- R"        CHART"

s" >R"     s" R a | S -- R | S a"      CHART"
s" R>"     s" R | S a -- R a | S"      CHART"
s" R@"     s" R | S a -- R a | S a"    CHART"

s" EXECUTE" s" R [ R -- S ] -- S"               CHART"
s" DIP"     s" R a [ R -- S ] -- S a"           CHART"

s" ."      s" R i64 -- R"              CHART"
s" EMIT"   s" R i64 -- R"              CHART"
s" CR"     s" R -- R"                  CHART"

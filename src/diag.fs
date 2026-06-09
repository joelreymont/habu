\ diag.fs — format the diagnostic record (filled before a THROW) for humans/LLMs.
\ Reads diag-state (CUR-WORD, CUR-TOKEN, DIAG-EXP/ACT/CODE). Self-contained buffer
\ and a small single-type renderer (mismatches are usually concrete-vs-concrete).

create DBUF  256 chars allot
variable DLEN
: D-RESET ( -- )        0 DLEN ! ;
: D$      ( -- c-addr u )  DBUF DLEN @ ;
: DPUT      ( c-addr u -- )                          \ append, clamped (never overflow)
   dup DLEN @ + 256 > if 2drop exit then
   {: ca u :}  ca  DBUF DLEN @ chars +  u move  u DLEN +! ;

: CODE-MSG ( code -- c-addr u )
   dup E-MISMATCH  = if drop s" type mismatch"                exit then
   dup E-OCCURS    = if drop s" stack depth mismatch"         exit then
   dup E-ARITY     = if drop s" arity mismatch"               exit then
   dup E-UNDERFLOW = if drop s" stack underflow"              exit then
   dup E-UNKNOWN   = if drop s" unknown word"                 exit then
   dup E-UNSAFE    = if drop s" unsafe word (needs TRUSTED:)" exit then
   dup E-UNCHECKED = if drop s" word has no checked effect"   exit then
   dup E-BADTYPE   = if drop s" bad type in signature"        exit then
   dup E-BRANCH    = if drop s" branches leave different stacks" exit then
   dup E-LOOP      = if drop s" loop changes stack depth"     exit then
   drop s" error" ;

: TY-NAME ( code -- c-addr u )
   dup TC-I64 = if drop s" i64"  exit then
   dup TC-U8  = if drop s" u8"   exit then
   dup TC-U32 = if drop s" u32"  exit then
   dup TC-CELL = if drop s" cell" exit then
   dup TC-BOOL = if drop s" bool" exit then
   dup TC-CHAR = if drop s" char" exit then
   dup TC-STR = if drop s" str"  exit then
   dup TC-ADDR = if drop s" addr" exit then
   drop s" ?" ;

\ Render a single type term into the diag buffer (concrete fully; var/ptr/quot terse).
: D-TYPE ( t -- )
   RESOLVE-TYPE
   dup TYCON? if TERM>PAYLOAD TY-NAME DPUT exit then
   dup TYVAR? if drop s" <var>" DPUT exit then
   dup TERM>TAG T-PTR = if PTR>INNER s" ptr " DPUT RECURSE exit then
   drop s" [quot]" DPUT ;

\ Build the diagnostic into the buffer; return it. Format (single line):
\   in WORD: MESSAGE at 'TOKEN' (expected EXP, got ACT)
\ The expected/actual clause is included only for code E-MISMATCH.
: FORMAT-DIAG ( -- c-addr u )
   D-RESET
   s" in " DPUT  CUR-WORD@ DPUT  s" : " DPUT
   DIAG-CODE@ CODE-MSG DPUT
   s"  at '" DPUT  CUR-TOKEN@ DPUT  s" '" DPUT
   DIAG-CODE@ E-MISMATCH = if
      s"  (expected " DPUT  DIAG-EXP@ D-TYPE  s" , got " DPUT  DIAG-ACT@ D-TYPE  s" )" DPUT
   then
   D$ ;

: DIAG-REPORT ( -- )  FORMAT-DIAG type cr ;   \ what colon calls on failure

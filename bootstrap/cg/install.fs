\ install.fs — wire the codegen into the live `:` checker. When CODEGEN-ON?,
\ every successfully-handled definition whose body is in the native subset is
\ recorded in CODE-TABLE; bodies using unmodeled words are skipped silently
\ (all-or-nothing, never breaks the definition). RUN-NATIVE (link.fs) compiles a
\ recorded word + its callees to a native Mac executable and runs it.
\ Load via habu-cg.fs (after the checker + colon override).

require link.fs

variable CODEGEN-ON?   CODEGEN-ON? off

: TRY-WALK ( ba bu -- )  ICODE-RESET cf-reset  WALK-BODY ;

\ input arity from the declared effect (EF@); default 1 if it won't parse.
: TRY-ARITY ( -- n )  ARENA-RESET  EF@ PARSE-SIG EFF>DIN STACK-ARITY ;

: BODY-ARITY ( -- n )  ['] TRY-ARITY catch if 1 else ( n ) then ;

: DO-CODEGEN ( -- )
   CODEGEN-ON? @ 0= if exit then
   CAP$  ['] TRY-WALK catch if  2drop exit then   \ uncompilable → skip
   NM@ CAP$ BODY-ARITY CG-RECORD ;

' DO-CODEGEN is CODEGEN-HOOK

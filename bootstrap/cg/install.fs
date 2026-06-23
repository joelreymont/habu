\ install.fs — wire the codegen into the live `:` checker. When CODEGEN-ON?,
\ every successfully-handled definition whose body is in the native subset is
\ recorded in CODE-TABLE; bodies using unmodeled words are skipped silently
\ (all-or-nothing, never breaks the definition). RUN-NATIVE (link.fs) compiles a
\ recorded word + its callees to a native Mac executable and runs it.
\ Load via habu-cg.fs (after the checker + colon override).

require link.fs

variable CODEGEN-ON?   CODEGEN-ON? off

: TRY-WALK ( ba bu -- )  ICODE-RESET CF-RESET  WALK-BODY ;

\ input arity from the declared effect (EF@); default 1 if it won't parse.
: TRY-ARITY ( -- n )  ARENA-RESET  EF@ PARSE-SIG EFF>DIN STACK-ARITY ;

: BODY-ARITY ( -- n )  ['] TRY-ARITY catch if 1 else ( n ) then ;

1 constant CGF-IN-INTLIKE
2 constant CGF-OUT-INTLIKE
4 constant CGF-OUT-BOOL
8 constant CGF-CONCRETE-INTBOOL

: INTBOOL? ( t -- f )
   RESOLVE-TYPE dup TYCON? if
      TERM>PAYLOAD dup TC-I64 = swap TC-BOOL = or
   else drop false then ;

: BOOL? ( t -- f )
   RESOLVE-TYPE dup TYCON? if TERM>PAYLOAD TC-BOOL = else drop false then ;

variable EFOK
: ROW-ALL-INTBOOL? ( s -- f )
   true EFOK !
   begin RESOLVE-ROW dup SROW? 0= while
      dup STACK-TOP INTBOOL? 0= if EFOK off then
      STACK-REST
   repeat drop EFOK @ ;

: ROW-HAS-BOOL? ( s -- f )
   false EFOK !
   begin RESOLVE-ROW dup SROW? 0= while
      dup STACK-TOP BOOL? if EFOK on then
      STACK-REST
   repeat drop EFOK @ ;

: TRY-EFFECT ( -- e )  ARENA-RESET  EF@ PARSE-SIG ;

: EFFECT-FLAGS ( -- flags )
   ['] TRY-EFFECT catch if 0 else
      0 swap {: flags eff :}
      eff EFF>DIN  ROW-ALL-INTBOOL? if flags CGF-IN-INTLIKE or to flags then
      eff EFF>DOUT ROW-ALL-INTBOOL? if flags CGF-OUT-INTLIKE or to flags then
      eff EFF>DOUT ROW-HAS-BOOL? if flags CGF-OUT-BOOL or to flags then
      flags CGF-IN-INTLIKE CGF-OUT-INTLIKE or and
         CGF-IN-INTLIKE CGF-OUT-INTLIKE or = if flags CGF-CONCRETE-INTBOOL or to flags then
      flags
   then ;

: DO-CODEGEN ( -- )
   CODEGEN-ON? @ 0= if exit then
   CAP$  ['] TRY-WALK catch if  2drop exit then   \ uncompilable → skip
   NM@ CAP$ BODY-ARITY EFFECT-FLAGS CG-RECORD ;

' DO-CODEGEN is CODEGEN-HOOK

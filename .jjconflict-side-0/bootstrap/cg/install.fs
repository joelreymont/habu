\ install.fs — wire the codegen into the live `:` checker. When CODEGEN-ON?,
\ every successfully-handled definition is recorded in CODE-TABLE. When codegen
\ is enabled, generator failures are fatal and diagnostic; no alternate threaded
\ path is allowed. RUN-NATIVE (link.fs) compiles a recorded word + its callees and runs it.
\ Load via habu-cg.fs (after the checker + colon override).

require link.fs

variable CODEGEN-ON?   CODEGEN-ON? off

: TRY-WALK ( ba bu -- )  ICODE-RESET CF-RESET  WALK-BODY ;

: CG-CATCH ( xt -- ... )
   catch ?dup if CG-DIAG throw then ;

: TRY-ARITY ( -- n )  ARENA-RESET  EF@ PARSE-SIG EFF>DIN STACK-ARITY ;

: BODY-ARITY ( -- n )  ['] TRY-ARITY CG-CATCH ;

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
   ['] TRY-EFFECT CG-CATCH
   0 swap {: flags eff :}
   eff EFF>DIN  ROW-ALL-INTBOOL? if flags CGF-IN-INTLIKE or to flags then
   eff EFF>DOUT ROW-ALL-INTBOOL? if flags CGF-OUT-INTLIKE or to flags then
   eff EFF>DOUT ROW-HAS-BOOL? if flags CGF-OUT-BOOL or to flags then
   flags CGF-IN-INTLIKE CGF-OUT-INTLIKE or and
      CGF-IN-INTLIKE CGF-OUT-INTLIKE or = if flags CGF-CONCRETE-INTBOOL or to flags then
   flags ;

: DO-CODEGEN ( -- )
   CODEGEN-ON? @ 0= if exit then
   CAP$  ['] TRY-WALK CG-CATCH
   NM@ CAP$ BODY-ARITY EFFECT-FLAGS CG-RECORD ;

' DO-CODEGEN is CODEGEN-HOOK

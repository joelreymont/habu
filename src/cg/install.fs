\ install.fs — wire the codegen into the live `:` checker. When CODEGEN-ON?,
\ every successfully-handled definition whose body is in the native subset is
\ recorded in CODE-TABLE; bodies using unmodeled words are skipped silently
\ (all-or-nothing, never breaks the definition). RUN-NATIVE (link.fs) compiles a
\ recorded word + its callees to a native Mac executable and runs it.
\ Load via caf-cg.fs (after the checker + colon override).

require link.fs

variable CODEGEN-ON?   CODEGEN-ON? off

: TRY-WALK ( ba bu -- )  ICODE-RESET cf-reset  WALK-BODY ;

: DO-CODEGEN ( -- )
   CODEGEN-ON? @ 0= if exit then
   CAP$  ['] TRY-WALK catch if  2drop exit then   \ uncompilable → skip
   NM@ CAP$ CG-RECORD ;

' DO-CODEGEN is CODEGEN-HOOK

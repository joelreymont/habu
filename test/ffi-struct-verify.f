\ ffi-struct-verify.f - LSP-CHECK:VERIFY-BYTES, the check of tools/check.f
\ --verify-only, reads FUNCTION:'s struct clauses. Loaded by src/lib/ffi-test.f
\ inside package FFI-TEST. Each declaration is its own source,
\ checked in a verifier child that stops at its first refusal. Sources select
\ PROCESS-SYMBOLS (LIBRARY leaves the rest of its file to the run and would
\ hide the clauses) and name no defined symbol, so a clean certify shows the
\ check resolves none.

: FFI-SV-HEAD$ ( -- ptr u8 n )
   S\" require src/lib/ffi-abi.f\nPROCESS-SYMBOLS\n24 constant FFI-SV-W\n" ;

: FFI-SV-SETUP ( -- )
   s" ffi-struct-verify" E2E-TREE:TREE
   E2E-TREE:ROOT$ FS:CLEANUP-TREE+ ;


\ The source under check, copied out of SB, which the verifier also uses.
DYNAMIC-BUFFER FFI-SV-SRC u8
variable FFI-SV-SRC-U
\ A deadlock guard.
60000 constant FFI-SV-MS

: FFI-SV-SRC$ ( -- ptr u8 n )
   0 FFI-SV-SRC FFI-SV-SRC-U @ ;

\ Write the source where the load children read it.
: FFI-SV-WRITE ( -- )
   s" test/ffi-sv.f" E2E-TREE:AT-A FFI-SV-SRC$ FS:WRITE-ALL ;

\ 0 verified, 1 refused, 2 any other verdict.
: FFI-SV-KIND ( LSP-CHECK:verdict -- n )
   MATCH LSP-CHECK:verdict
      verified OF 0 ENDOF
      refused OF 1 ENDOF
      engine-provided OF 2 ENDOF
      held OF 2 ENDOF
      incomplete OF {: status :} 2 ENDOF
      deferred OF 2 ENDOF
   ;MATCH ;

\ Check the source SB holds as the tree's test/ffi-sv.f.
: FFI-SV-CHECK ( -- n )
   SB$ {: a:ptr u:n :}
   u 1 max FFI-SV-SRC-RESERVE
   a 0 FFI-SV-SRC u BYTE-COPY
   u FFI-SV-SRC-U !
   FFI-SV-SRC$ s" test/ffi-sv.f" E2E-TREE:AT-A [: drop ;] FFI-SV-MS >MS
   LSP-CHECK:VERIFY-BYTES FFI-SV-KIND ;

\ Check `FUNCTION: FFI-SV no_symbol <decl> ;FUNCTION`.
: FFI-SV-RUN ( ptr u8 n -- n )
   {: decl:ptr declu:n :}
   SB-RESET
   FFI-SV-HEAD$ SB-APPEND
   s" FUNCTION: FFI-SV no_symbol " SB-APPEND
   decl declu SB-APPEND
   S\"  ;FUNCTION\n" SB-APPEND
   FFI-SV-CHECK ;


: FFI-SV-SHOW ( -- )
   LSP-CHECK:VERIFY-OUT$ type LSP-CHECK:VERIFY-LOG$ type ;


\ The source certifies: verified, and no packet, deferred or refused.
: FFI-SV-CLEAN ( n -- )
   {: kind:n :}
   kind 0<> LSP-CHECK:VERIFY-OUT$ S\" \"code\":" CONTAINS? or IF FFI-SV-SHOW THEN
   kind 0 T=
   LSP-CHECK:VERIFY-OUT$ S\" \"code\":" CONTAINS? TFALSE ;

: FFI-SV-PASS ( ptr u8 n -- )
   {: decl:ptr declu:n :}
   decl declu T-LABEL
   decl declu FFI-SV-RUN
   FFI-SV-CLEAN ;


\ The source is refused, with CODE in a packet.
: FFI-SV-REFUSES ( n ptr u8 n -- )
   {: kind:n code:ptr codeu:n :}
   kind 1 <> IF FFI-SV-SHOW THEN
   kind 1 T=
   LSP-CHECK:VERIFY-OUT$ code codeu CONTAINS? TTRUE ;

: FFI-SV-REFUSED ( ptr u8 n ptr u8 n -- )
   {: decl:ptr declu:n code:ptr codeu:n :}
   decl declu FFI-SV-RUN
   code codeu FFI-SV-REFUSES ;

: FFI-SV-REFUSE ( ptr u8 n ptr u8 n -- )
   {: decl:ptr declu:n code:ptr codeu:n :}
   decl declu T-LABEL
   decl declu code codeu FFI-SV-REFUSED ;


\ A refusal's packet names the load's code as throw_code: -3501 E-FFI-SYNTAX,
\ -3500 E-FFI-ARITY.
: FFI-SV-SYNTAX ( ptr u8 n -- )
   S\" \"throw_code\":-3501" FFI-SV-REFUSE ;
: FFI-SV-ARITY ( ptr u8 n -- )
   S\" \"throw_code\":-3500" FFI-SV-REFUSE ;


\ Sixteen record inputs whose payloads sum, modulo 2^64, to almost nothing.
: FFI-SV-WRAP$ ( -- ptr u8 n )
   FFI-T-SRC CODEGEN:RESET
   s" ( " FFI-T-SRC+
   16 0 ?do s" ptr u8 " FFI-T-SRC+ loop
   s" -- )" FFI-T-SRC+
   15 0 ?do
      s"  " FFI-T-SRC+ i FFI-T-SRC CODEGEN:APPEND-DECIMAL
      s"  $7FFFFFFFFFFFFFFF STRUCT-ARG" FFI-T-SRC+
   loop
   s"  15 $7FFFFFFFFFFFFF7F STRUCT-ARG" FFI-T-SRC+
   FFI-T-SRC CODEGEN:CONTENTS ;


: FFI-SV-ACCEPTED ( -- )
   s" ( n ptr u8 -- ) 16 STRUCT-RESULT" FFI-SV-PASS
   s" ( ptr u8 n -- n ) 0 24 STRUCT-ARG" FFI-SV-PASS
   s" ( r ptr u8 f32 -- n ) 1 12 STRUCT-ARG" FFI-SV-PASS
   s" ( ptr u8 ptr u8 -- ) 0 16 STRUCT-ARG 16 STRUCT-RESULT" FFI-SV-PASS
   s" ( ptr u8 n ptr u8 -- n ) 0 8 STRUCT-ARG 2 1 WRITES-ARG" FFI-SV-PASS
   s" ( ptr u8 -- n ) 0 128 STRUCT-ARG" FFI-SV-PASS
   s" ( n n n n n n n n ptr u8 -- n ) 8 112 STRUCT-ARG" FFI-SV-PASS
   \ An operand the check cannot establish leaves the clause to the load.
   s" ( ptr u8 -- ) FFI-SV-W STRUCT-RESULT" FFI-SV-PASS
   s" ( ptr u8 -- n ) 0 FFI-SV-W STRUCT-ARG" FFI-SV-PASS ;


: FFI-SV-STRUCT-ARG ( -- )
   s" ( n -- n ) 0 16 STRUCT-ARG" FFI-SV-SYNTAX
   s" ( ptr u8 -- n ) 1 16 STRUCT-ARG" FFI-SV-SYNTAX
   s" ( ptr u8 -- n ) -1 16 STRUCT-ARG" FFI-SV-SYNTAX
   s" ( ptr u8 -- n ) 0 0 STRUCT-ARG" FFI-SV-SYNTAX
   s" ( ptr u8 -- n ) 0 -8 STRUCT-ARG" FFI-SV-SYNTAX
   s" ( ptr u8 -- n ) 0 16 STRUCT-ARG 0 16 STRUCT-ARG" FFI-SV-SYNTAX
   s" ( ptr u8 -- n ) 0 16 WRITES-BYTES 0 16 STRUCT-ARG" FFI-SV-SYNTAX
   s" ( ptr u8 -- n ) 0 16 STRUCT-ARG 0 16 WRITES-BYTES" FFI-SV-SYNTAX
   s" ( ptr u8 n -- n ) 0 1 WRITES-ARG 0 16 STRUCT-ARG" FFI-SV-SYNTAX
   s" ( ptr u8 n -- n ) 0 16 STRUCT-ARG 0 1 WRITES-ARG" FFI-SV-SYNTAX
   s" ( n ptr u8 -- n ) 1 VARIADIC 1 16 STRUCT-ARG" FFI-SV-SYNTAX
   s" ( n ptr u8 -- n ) 1 16 STRUCT-ARG 1 VARIADIC" FFI-SV-SYNTAX
   s" ( ptr u8 -- r ) 0 16 STRUCT-ARG" FFI-SV-SYNTAX
   s" ( ptr u8 -- f32 ) 0 16 STRUCT-ARG" FFI-SV-SYNTAX
   s" ( ptr u8 -- ) 0 16 STRUCT-ARG 16 STRUCT-RESULT" FFI-SV-SYNTAX
   s" ( ptr u8 -- ) 16 STRUCT-RESULT 0 16 STRUCT-ARG" FFI-SV-SYNTAX
   s" ( ptr u8 -- n ) 0 129 STRUCT-ARG" FFI-SV-ARITY
   s" ( n n n n n n n n ptr u8 -- n ) 8 113 STRUCT-ARG" FFI-SV-ARITY
   s" ( r r r r r r r r r ptr u8 -- n ) 9 16 STRUCT-ARG" FFI-SV-ARITY
   s" sixteen record payloads that wrap 2^64 are refused" T-LABEL
   FFI-SV-WRAP$ S\" \"throw_code\":-3500" FFI-SV-REFUSED ;


: FFI-SV-STRUCT-RESULT ( -- )
   s" ( n -- ) 0 STRUCT-RESULT" FFI-SV-SYNTAX
   s" ( -- ) 8 STRUCT-RESULT" FFI-SV-SYNTAX
   s" ( ptr u8 n -- ) 8 STRUCT-RESULT" FFI-SV-SYNTAX
   s" ( ptr u8 -- n ) 8 STRUCT-RESULT" FFI-SV-SYNTAX
   s" ( ptr u8 -- ) 0 STRUCT-RESULT" FFI-SV-SYNTAX
   s" ( ptr u8 -- ) -16 STRUCT-RESULT" FFI-SV-SYNTAX
   s" ( ptr u8 -- ) 8 STRUCT-RESULT 8 STRUCT-RESULT" FFI-SV-SYNTAX
   s" ( ptr u8 -- ) 0 8 WRITES-BYTES 8 STRUCT-RESULT" FFI-SV-SYNTAX
   s" ( ptr u8 -- ) 8 STRUCT-RESULT 0 8 WRITES-BYTES" FFI-SV-SYNTAX
   s" ( n ptr u8 -- ) 1 0 WRITES-ARG 8 STRUCT-RESULT" FFI-SV-SYNTAX
   s" ( n ptr u8 -- ) 8 STRUCT-RESULT 1 0 WRITES-ARG" FFI-SV-SYNTAX
   s" ( n ptr u8 -- ) 1 VARIADIC 8 STRUCT-RESULT" FFI-SV-SYNTAX
   s" ( n ptr u8 -- ) 8 STRUCT-RESULT 1 VARIADIC" FFI-SV-SYNTAX
   s" ( ptr u8 ptr u8 -- ) 0 129 STRUCT-ARG 16 STRUCT-RESULT" FFI-SV-ARITY
   HB-TARGET-LINUX-X86-64? if
      s" ( n n n n n ptr u8 ptr u8 -- ) 5 128 STRUCT-ARG 32 STRUCT-RESULT" FFI-SV-PASS
      s" ( n n n n n n ptr u8 ptr u8 -- ) 6 128 STRUCT-ARG 32 STRUCT-RESULT" FFI-SV-ARITY
   then ;


\ A clause is the declarer's word only where the token resolves to it: a
\ package's own STRUCT-ARG is an ordinary call, so the check certifies the
\ source and the load, which calls that word, accepts it. The load runs in a
\ child because inside this file's FFI-TEST the source's `package` would open
\ a part of FFI-TEST, not a package of its own.
: FFI-SV-ORDINARY ( ptr u8 n -- )
   {: src:ptr srcu:n :}
   SB-RESET src srcu SB-APPEND
   FFI-SV-CHECK
   FFI-SV-CLEAN
   FFI-SV-WRITE
   s" test/ffi-sv.f" E2E-TREE:ARGS
   ENGINE-CANDIDATE:PATH$ E2E-TREE:ROOT$ E2E-TREE:RUN-ON
   E2E-TREE:NEED-OK ;

: FFI-SV-SHADOWED ( -- )
   s" a package's own STRUCT-ARG is an ordinary call" T-LABEL
   S\" package FFI-SV-P\n: STRUCT-ARG ( n n -- ) 2drop ;\nPROCESS-SYMBOLS\nFUNCTION: FFI-SV-X no_symbol ( n -- n )\n   0 16 STRUCT-ARG\n;FUNCTION\n;package\n"
   FFI-SV-ORDINARY ;


\ A re-export of the declarer's word is that word (docs/forth.md, EXPORT): the
\ load binds the alias, bare inside its package or qualified outside it, to
\ STRUCT-ARG's code and refuses the clause, so the check refuses it with the
\ load's code. The load runs in a child for FFI-SV-SHADOWED's reason.
: FFI-SV-ALIASED ( ptr u8 n -- )
   {: src:ptr srcu:n :}
   SB-RESET src srcu SB-APPEND
   FFI-SV-CHECK S\" \"throw_code\":-3501" FFI-SV-REFUSES
   FFI-SV-WRITE
   s" test/ffi-sv.f" E2E-TREE:ARGS
   ENGINE-CANDIDATE:PATH$ E2E-TREE:ROOT$ E2E-TREE:RUN-ON
   E2E-TREE:RC@ 0 <> TTRUE
   E2E-TREE:ERR$ s" throw code -3501" CONTAINS? TTRUE ;

: FFI-SV-REEXPORTED ( -- )
   s" a bare re-export of STRUCT-ARG is the clause" T-LABEL
   S\" package FFI-SV-AL\npublic\nEXPORT STRUCT-ARG\nPROCESS-SYMBOLS\nFUNCTION: FFI-SV-X no_symbol ( n -- n )\n   0 16 STRUCT-ARG\n;FUNCTION\n;package\n"
   FFI-SV-ALIASED
   s" a qualified re-export of STRUCT-ARG is the clause" T-LABEL
   S\" package FFI-SV-AL\npublic\nEXPORT STRUCT-ARG\n;package\nPROCESS-SYMBOLS\nFUNCTION: FFI-SV-X no_symbol ( n -- n )\n   0 16 FFI-SV-AL:STRUCT-ARG\n;FUNCTION\n"
   FFI-SV-ALIASED ;


\ A re-export is the declarer's word only when it names that word: an export
\ of another package's STRUCT-ARG, or of a global one defined after
\ `undefine`, is an ordinary call to it, so the check certifies the source and
\ the load accepts it. The loads run in children for FFI-SV-SHADOWED's reason.
: FFI-SV-EXPORTED ( -- )
   s" a re-export of another package's STRUCT-ARG is an ordinary call" T-LABEL
   S\" package FFI-SV-Q\npublic\n: STRUCT-ARG ( n n -- ) 2drop ;\n;package\npackage FFI-SV-P\npublic\nEXPORT FFI-SV-Q:STRUCT-ARG\nPROCESS-SYMBOLS\nFUNCTION: FFI-SV-X no_symbol ( n -- n )\n   0 16 STRUCT-ARG\n;FUNCTION\n;package\n"
   FFI-SV-ORDINARY
   s" a re-export of a redefined global STRUCT-ARG is an ordinary call" T-LABEL
   S\" undefine STRUCT-ARG\n: STRUCT-ARG ( n n -- ) 2drop ;\npackage FFI-SV-P\npublic\nEXPORT STRUCT-ARG\nPROCESS-SYMBOLS\nFUNCTION: FFI-SV-X no_symbol ( n -- n )\n   0 16 STRUCT-ARG\n;FUNCTION\n;package\n"
   FFI-SV-ORDINARY ;


: FFI-T-STRUCT-VERIFY ( -- )
   FFI-SV-SETUP
   FFI-SV-ACCEPTED
   FFI-SV-STRUCT-ARG
   FFI-SV-STRUCT-RESULT
   FFI-SV-SHADOWED
   FFI-SV-REEXPORTED
   FFI-SV-EXPORTED ;

FFI-T-STRUCT-VERIFY

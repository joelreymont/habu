split-str
\ ffi-struct-verify.f - LSP-CHECK:VERIFY-BYTES, the check of tools/check.f
\ --verify-only, reads FUNCTION: declarations. Loaded by
\ src/lib/ffi-test.f inside package FFI-TEST. Each
\ declaration is its own source, checked in a verifier child that stops at its
\ first refusal. Sources select PROCESS-SYMBOLS (LIBRARY leaves the rest of
\ its file to the run and would hide the clauses) and name no defined symbol,
\ so a clean certify shows the check resolves none.

: FFI-SV-HEAD$ ( -- str )
   S\" require src/lib/ffi-abi.f\nPROCESS-SYMBOLS\n24 constant FFI-SV-W\n" ;

: FFI-SV-SETUP ( -- )
   s" ffi-struct-verify" E2E-TREE:TREE
   E2E-TREE:ROOT$ FS:CLEANUP-TREE+ ;


\ The source under check, copied out of SB, which the verifier also uses.
DYNAMIC-BUFFER FFI-SV-SRC u8
variable FFI-SV-SRC-U

: FFI-SV-SRC$ ( -- str )
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

\ Check the source SB holds as the tree's test/ffi-sv.f, the records packets
\ when JSON is true and prose otherwise.
: FFI-SV-CHECK-AS ( bool -- n )
   {: json:bool :}
   SB$ {: a:^ u:n :}
   u 1 max FFI-SV-SRC-RESERVE
   a 0 FFI-SV-SRC u BYTE-COPY
   u FFI-SV-SRC-U !
   FFI-SV-SRC$ s" test/ffi-sv.f" E2E-TREE:AT-A [: drop ;] PROCESS:WAIT-FOREVER
   json LSP-CHECK:VERIFY-BYTES FFI-SV-KIND ;

: FFI-SV-CHECK ( -- n )
   true FFI-SV-CHECK-AS ;

\ Check `FUNCTION: FFI-SV no_symbol <decl> ;FUNCTION`.
: FFI-SV-RUN ( str -- n )
   {: decl:^ declu:n :}
   SB-RESET
   FFI-SV-HEAD$ SB-APPEND
   s" FUNCTION: FFI-SV no_symbol " SB-APPEND
   decl declu SB-APPEND
   S\"  ;FUNCTION\n" SB-APPEND
   FFI-SV-CHECK ;

\ Check FFI-SV-HEAD$ and then the source, a newline after it.
: FFI-SV-SOURCE ( str -- n )
   {: src:^ srcu:n :}
   SB-RESET
   FFI-SV-HEAD$ SB-APPEND
   src srcu SB-APPEND
   S\" \n" SB-APPEND
   FFI-SV-CHECK ;


: FFI-SV-SHOW ( -- )
   LSP-CHECK:VERIFY-OUT$ type LSP-CHECK:VERIFY-LOG$ type ;

: FFI-SV-SAYS ( str -- )
   {: s:^ su:n :}
   LSP-CHECK:VERIFY-OUT$ s su STR:CONTAINS? 0= IF FFI-SV-SHOW THEN
   LSP-CHECK:VERIFY-OUT$ s su STR:CONTAINS? TTRUE ;


\ The source certifies: verified, and no packet, deferred or refused.
: FFI-SV-CLEAN ( n -- )
   {: kind:n :}
   kind 0<> LSP-CHECK:VERIFY-OUT$ S\" \"code\":" STR:CONTAINS? or IF FFI-SV-SHOW THEN
   kind 0 T=
   LSP-CHECK:VERIFY-OUT$ S\" \"code\":" STR:CONTAINS? TFALSE ;

: FFI-SV-PASS ( str -- )
   {: decl:^ declu:n :}
   decl declu T-LABEL
   decl declu FFI-SV-RUN
   FFI-SV-CLEAN ;


\ The source is refused, with CODE in a packet.
: FFI-SV-REFUSES ( n str -- )
   {: kind:n code:^ codeu:n :}
   kind 1 <> IF FFI-SV-SHOW THEN
   kind 1 T=
   LSP-CHECK:VERIFY-OUT$ code codeu STR:CONTAINS? TTRUE ;

\ The source is deferred, with W-CHECK-DEFERRED in the output.
: FFI-SV-DEFERS ( n -- )
   {: kind:n :}
   kind 2 <> IF FFI-SV-SHOW THEN
   kind 2 T=
   S\" W-CHECK-DEFERRED" FFI-SV-SAYS ;

: FFI-SV-REFUSED ( str str -- )
   {: decl:^ declu:n code:^ codeu:n :}
   decl declu FFI-SV-RUN
   code codeu FFI-SV-REFUSES ;

: FFI-SV-REFUSE ( str str -- )
   {: decl:^ declu:n code:^ codeu:n :}
   decl declu T-LABEL
   decl declu code codeu FFI-SV-REFUSED ;


\ A rejection's record names the load's code.
: FFI-SV-SYNTAX ( str str -- )
   {: decl:^ declu:n msg:^ msgu:n :}
   decl declu S\" \"code\":\"E-FFI-SYNTAX\"" FFI-SV-REFUSE
   FFI-T-SRC CODEGEN:RESET
   S\" \"message\":\"" FFI-T-SRC+
   msg msgu FFI-T-SRC+
   S\" \"" FFI-T-SRC+
   FFI-T-SRC CODEGEN:CONTENTS FFI-SV-SAYS ;
: FFI-SV-ARITY ( str -- )
   S\" \"code\":\"E-FFI-ARITY\"" FFI-SV-REFUSE ;

\ A struct plan the check rejects with E-FFI-ARITY prints the load's record,
\ its header and its help, placed at ;FUNCTION with the definer's label.
: FFI-SV-RECORD ( str str -- )
   {: head:^ headu:n help:^ helpu:n :}
   LSP-CHECK:VERIFY-OUT$ head headu STR:CONTAINS? TTRUE
   LSP-CHECK:VERIFY-OUT$ S\" \"token\":\";FUNCTION\",\"source_line\":" STR:CONTAINS? TTRUE
   LSP-CHECK:VERIFY-OUT$ S\" \"label\":\"the definer was reading here\"" STR:CONTAINS? TTRUE
   LSP-CHECK:VERIFY-OUT$ help helpu STR:CONTAINS? TTRUE ;

: FFI-SV-STAGED ( -- )
   S\" \"code\":\"E-FFI-ARITY\",\"message\":\"a struct call stages at most 16 cells of stack inputs and record copies\""
   S\" \"help\":\"pass the large records by address through a C function\""
   FFI-SV-RECORD ;


\ Sixteen record inputs whose payloads sum, modulo 2^64, to almost nothing.
: FFI-SV-WRAP$ ( -- str )
   FFI-T-SRC CODEGEN:RESET
   s" ( " FFI-T-SRC+
   16 0 ?do s" ^u8 " FFI-T-SRC+ loop
   s" -- )" FFI-T-SRC+
   15 0 ?do
      s"  " FFI-T-SRC+ i FFI-T-SRC CODEGEN:APPEND-DECIMAL
      s"  $7FFFFFFFFFFFFFFF STRUCT-ARG" FFI-T-SRC+
   loop
   s"  15 $7FFFFFFFFFFFFF7F STRUCT-ARG" FFI-T-SRC+
   FFI-T-SRC CODEGEN:CONTENTS ;


: FFI-SV-ACCEPTED ( -- )
   s" ( n ^u8 -- ) 16 STRUCT-RESULT" FFI-SV-PASS
   s" ( ^u8 n -- n ) 0 24 STRUCT-ARG" FFI-SV-PASS
   s" ( r ^u8 f32 -- n ) 1 12 STRUCT-ARG" FFI-SV-PASS
   s" ( i32 ^u8 -- n ) 1 16 STRUCT-ARG" FFI-SV-PASS
   s" ( str ^u8 -- n ) 1 16 STRUCT-ARG" FFI-SV-PASS
   s" ( ^u8 ^u8 -- ) 0 16 STRUCT-ARG 16 STRUCT-RESULT" FFI-SV-PASS
   s" ( ^u8 -- n ) 0 128 STRUCT-ARG" FFI-SV-PASS
   s" ( n n n n n n n n ^u8 -- n ) 8 112 STRUCT-ARG" FFI-SV-PASS
   \ An operand the check cannot establish leaves the clause to the load.
   s" ( ^u8 -- ) FFI-SV-W STRUCT-RESULT" FFI-SV-PASS
   s" ( ^u8 -- n ) 0 FFI-SV-W STRUCT-ARG" FFI-SV-PASS
   s" ( r r r r r r r r r f32 ^u8 -- n ) 10 16 STRUCT-ARG" FFI-SV-PASS
   \ macOS packs the seven u8s in one stack cell, leaving room for the copy.
   HB-TARGET-MACOS? if
      s" ( n n n n n n n n u8 u8 u8 u8 u8 u8 u8 ^u8 -- n ) 15 72 STRUCT-ARG" FFI-SV-PASS
   else
      s" ( n n n n n n n n u8 u8 u8 u8 u8 u8 u8 ^u8 -- n ) 15 72 STRUCT-ARG"
      FFI-SV-ARITY FFI-SV-STAGED
   then ;


: FFI-SV-STRUCT-ARG ( -- )
   s" ( n -- n ) 0 16 STRUCT-ARG"
   s" the clause names an input that is not ^u8" FFI-SV-SYNTAX
   S\" \"token\":\"STRUCT-ARG\"" FFI-SV-SAYS
   s" ( str -- n ) 0 16 STRUCT-ARG"
   s" the clause names an input that is not ^u8" FFI-SV-SYNTAX
   s" ( ^u8 -- n ) 1 16 STRUCT-ARG"
   s" the clause names no input of the declaration" FFI-SV-SYNTAX
   s" ( ^u8 -- n ) -1 16 STRUCT-ARG"
   s" the clause names no input of the declaration" FFI-SV-SYNTAX
   s" ( ^u8 -- n ) 0 0 STRUCT-ARG"
   s" STRUCT-ARG's byte count is not positive" FFI-SV-SYNTAX
   s" ( ^u8 -- n ) 0 -8 STRUCT-ARG"
   s" STRUCT-ARG's byte count is not positive" FFI-SV-SYNTAX
   s" ( ^u8 -- n ) 0 16 STRUCT-ARG 0 16 STRUCT-ARG"
   s" a second clause names the same input" FFI-SV-SYNTAX
   s" ( n ^u8 -- n ) 1 VARIADIC 1 16 STRUCT-ARG"
   s" a struct call with VARIADIC" FFI-SV-SYNTAX
   s" ( n ^u8 -- n ) 1 16 STRUCT-ARG 1 VARIADIC"
   s" a struct call with VARIADIC" FFI-SV-SYNTAX
   S\" \"token\":\"VARIADIC\"" FFI-SV-SAYS
   s" ( ^u8 -- r ) 0 16 STRUCT-ARG"
   s" STRUCT-ARG with a float result" FFI-SV-SYNTAX
   s" ( ^u8 -- f32 ) 0 16 STRUCT-ARG"
   s" STRUCT-ARG with a float result" FFI-SV-SYNTAX
   s" ( ^u8 -- ) 0 16 STRUCT-ARG 16 STRUCT-RESULT"
   s" a second clause names the same input" FFI-SV-SYNTAX
   S\" \"token\":\"STRUCT-RESULT\"" FFI-SV-SAYS
   s" ( ^u8 -- ) 16 STRUCT-RESULT 0 16 STRUCT-ARG"
   s" the clause names STRUCT-RESULT's destination, the last input"
   FFI-SV-SYNTAX
   s" ( ^u8 -- n ) 0 129 STRUCT-ARG" FFI-SV-ARITY FFI-SV-STAGED
   s" ( n n n n n n n n ^u8 -- n ) 8 113 STRUCT-ARG" FFI-SV-ARITY FFI-SV-STAGED
   s" sixteen record payloads that wrap 2^64 are refused" T-LABEL
   FFI-SV-WRAP$ S\" \"code\":\"E-FFI-ARITY\"" FFI-SV-REFUSED FFI-SV-STAGED ;


: FFI-SV-STRUCT-RESULT ( -- )
   s" ( n -- ) 0 STRUCT-RESULT"
   s" STRUCT-RESULT's byte count is not positive" FFI-SV-SYNTAX
   s" ( -- ) 8 STRUCT-RESULT"
   s" STRUCT-RESULT with no input to write to" FFI-SV-SYNTAX
   s" ( ^u8 n -- ) 8 STRUCT-RESULT"
   s" the clause names an input that is not ^u8" FFI-SV-SYNTAX
   s" ( ^u8 -- n ) 8 STRUCT-RESULT"
   s" STRUCT-RESULT with a declared output" FFI-SV-SYNTAX
   s" ( ^u8 -- ) 0 STRUCT-RESULT"
   s" STRUCT-RESULT's byte count is not positive" FFI-SV-SYNTAX
   s" ( ^u8 -- ) -16 STRUCT-RESULT"
   s" STRUCT-RESULT's byte count is not positive" FFI-SV-SYNTAX
   s" ( ^u8 -- ) 8 STRUCT-RESULT 8 STRUCT-RESULT"
   s" a second STRUCT-RESULT" FFI-SV-SYNTAX
   s" ( n ^u8 -- ) 1 VARIADIC 8 STRUCT-RESULT"
   s" a struct call with VARIADIC" FFI-SV-SYNTAX
   s" ( n ^u8 -- ) 8 STRUCT-RESULT 1 VARIADIC"
   s" a struct call with VARIADIC" FFI-SV-SYNTAX
   s" ( ^u8 ^u8 -- ) 0 129 STRUCT-ARG 16 STRUCT-RESULT" FFI-SV-ARITY FFI-SV-STAGED
   HB-TARGET-LINUX-X86-64? if
      s" ( n n n n n ^u8 ^u8 -- ) 5 128 STRUCT-ARG 32 STRUCT-RESULT" FFI-SV-PASS
      s" ( n n n n n n ^u8 ^u8 -- ) 6 128 STRUCT-ARG 32 STRUCT-RESULT"
      FFI-SV-ARITY FFI-SV-STAGED
   then ;


\ A whole source the load rejects, with its message and token.
: FFI-SV-UNOPENED ( str str str -- )
   {: src:^ srcu:n msg:^ msgu:n tok:^ toku:n :}
   src srcu T-LABEL
   src srcu FFI-SV-SOURCE
   S\" \"code\":\"E-FFI-SYNTAX\"" FFI-SV-REFUSES
   msg msgu FFI-SV-SAYS
   tok toku FFI-SV-SAYS ;

\ The check rejects each declaration where the load does, with the load's record.
: FFI-SV-DECLARATIONS ( -- )
   s" ( -- n n )"
   s" a FUNCTION: declaration has at most one output" FFI-SV-SYNTAX
   S\" \"token\":\"n\"" FFI-SV-SAYS
   S\" \"column\":35," FFI-SV-SAYS
   s" ( fd -- n )"
   s" 'fd' is not an input type: an input is ^u8, str, f32, r, i32, n, i64, cell, addr, u8, u16, u32 or char"
   FFI-SV-SYNTAX
   S\" \"token\":\"fd\"" FFI-SV-SAYS
   s" ( ^ -- n )" s" expected 'u8' after '^'" FFI-SV-SYNTAX
   s" ( n n n n n n n n n n n n n n n n n -- n )" FFI-SV-ARITY
   S\" \"message\":\"a FUNCTION: declaration has at most 16 inputs, a str counting two\""
   FFI-SV-SAYS
   s" 16 STRUCT-RESULT"
   S\" \"message\":\"a clause outside a FUNCTION: declaration\""
   S\" \"token\":\"STRUCT-RESULT\"" FFI-SV-UNOPENED
   \ A declaration still open when the next opens is rejected at its opener.
   s" FUNCTION: FFI-SV no_symbol ( -- ) FUNCTION: FFI-SW no_symbol ( -- ) ;FUNCTION"
   2dup T-LABEL  FFI-SV-SOURCE
   S\" \"code\":\"E-UNCLOSED\"" FFI-SV-REFUSES
   S\" \"message\":\"`FUNCTION:` is still open at `FUNCTION:`\""
   FFI-SV-SAYS
   S\" \"column\":1," FFI-SV-SAYS
   S\" \"help\":\"close it with `;FUNCTION` before `FUNCTION:`\"" FFI-SV-SAYS
   \ A load inside a declaration is rejected at its opener, before the load
   \ resolves its path.
   S\" FUNCTION: FFI-SV no_symbol ( -- )\ninclude ffi-sv-none.f\n;FUNCTION"
   2dup T-LABEL  FFI-SV-SOURCE
   S\" \"code\":\"E-UNCLOSED\"" FFI-SV-REFUSES
   S\" \"message\":\"`FUNCTION:` is still open when `include` loads a file\""
   FFI-SV-SAYS
   S\" \"column\":1," FFI-SV-SAYS
   S\" \"help\":\"close it with `;FUNCTION` before `include`\"" FFI-SV-SAYS
   S\" BEGIN-STRUCTURE FFI-SV-S\ninclude ffi-sv-none.f\nEND-STRUCTURE"
   2dup T-LABEL  FFI-SV-SOURCE
   S\" \"code\":\"E-UNCLOSED\"" FFI-SV-REFUSES
   S\" \"message\":\"`BEGIN-STRUCTURE` is still open when `include` loads a file\""
   FFI-SV-SAYS
   S\" \"column\":1," FFI-SV-SAYS
   S\" \"help\":\"close it with `END-STRUCTURE` before `include`\"" FFI-SV-SAYS
   \ A renderer in a structure body is deferred at its token.
   S\" BEGIN-STRUCTURE FFI-SV-S\ns\" include ffi-sv-none.f\" evaluate\nEND-STRUCTURE"
   2dup T-LABEL  FFI-SV-SOURCE FFI-SV-DEFERS
   s" ;FUNCTION"
   S\" \"message\":\";FUNCTION with no declaration open\""
   S\" \"token\":\";FUNCTION\"" FFI-SV-UNOPENED
   s" FUNCTION: FFI-SV no_symbol ( n --"
   S\" \"message\":\"the line ended before the declaration did\""
   S\" \"token\":\"--\"" FFI-SV-UNOPENED ;

\ A declaration a deferral leaves to the run takes its clauses and ;FUNCTION
\ silently.
: FFI-SV-DEFERRED ( -- )
   S\" : FFI-SV-DEF: ( -- ) FFI:OPEN-FUNCTION ;\nparses: FFI-SV-DEF: 9\nFFI-SV-DEF: FFI-SV-G printf ( ^u8 n -- n )\n1 VARIADIC\n;FUNCTION"
   {: src:^ srcu:n :}
   src srcu T-LABEL
   src srcu FFI-SV-SOURCE FFI-SV-DEFERS
   LSP-CHECK:VERIFY-OUT$ s" E-FFI-SYNTAX" STR:CONTAINS?
   dup IF FFI-SV-SHOW THEN TFALSE ;


\ After a group, a name nothing resolves and a type nothing declares are
\ rejected, not left to the run.
: FFI-SV-GROUP-CLOSE ( -- )
   s" FUNCTION: FFI-SV no_symbol ( ^u8 n -- i32 ) ;FUNCTION : FFI-SV-F1 ( -- ) FFI-SV-NOSUCH ;"
   2dup T-LABEL  FFI-SV-SOURCE
   S\" \"code\":\"E-UNDEFINED\"" FFI-SV-REFUSES
   s" FUNCTION: FFI-SV no_symbol ( ^u8 n -- i32 ) ;FUNCTION : FFI-SV-F2 ( ^u8 -- n ) {: buf :} buf 1 FFI-SV {: got:i32 :} buf CELL-VIEW @ ;"
   2dup T-LABEL  FFI-SV-SOURCE
   S\" \"code\":\"E-UNKNOWN-SIGNATURE-TYPE\"" FFI-SV-REFUSES ;

\ A program's own `;FUNCTION` closes no group: called at top level, this
\ defer leaves the rest of the source to the run, which rejects the group.
: FFI-SV-GROUP-REDEFINED ( -- )
   S\" package FFI-SV-P\npublic\n: NOP ( -- ) ;\ndefer ;FUNCTION ( -- )\n: SET ( -- ) ['] NOP is ;FUNCTION ;\nSET\nPROCESS-SYMBOLS FUNCTION: FFI-SV-H no_symbol ( ^u8 n -- i32 ) ;FUNCTION\n;package"
   2dup T-LABEL  FFI-SV-SOURCE FFI-SV-DEFERS ;


\ A clause is the declarer's word only where the token resolves to it: a
\ package's own STRUCT-ARG is an ordinary call, so the check certifies the
\ source and the load, which calls that word, accepts it. The load runs in a
\ child because inside this file's FFI-TEST the source's `package` would open
\ a part of FFI-TEST, not a package of its own.
: FFI-SV-ORDINARY ( str -- )
   {: src:^ srcu:n :}
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
: FFI-SV-ALIASED ( str -- )
   {: src:^ srcu:n :}
   SB-RESET src srcu SB-APPEND
   FFI-SV-CHECK S\" \"code\":\"E-FFI-SYNTAX\"" FFI-SV-REFUSES
   FFI-SV-WRITE
   s" test/ffi-sv.f" E2E-TREE:ARGS
   ENGINE-CANDIDATE:PATH$ E2E-TREE:ROOT$ E2E-TREE:RUN-ON
   E2E-TREE:RC@ 70 T=
   E2E-TREE:ERR$ s" error[E-FFI-SYNTAX]: the clause names an input that is not ^u8"
   STR:CONTAINS? TTRUE ;

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


\ A declaration is read before its library is checked, so one that cannot be
\ read and selects none gets the same record from the load and the check.
: FFI-SV-UNSELECTED-HAS ( str -- )
   {: out:^ outu:n :}
   out outu s" error[E-FFI-SYNTAX]: expected '(' after the C symbol" STR:CONTAINS? TTRUE
   out outu s" ffi-sv.f:2:20" STR:CONTAINS? TTRUE
   out outu s" = help: write FUNCTION: NAME symbol ( inputs -- output ) ;FUNCTION"
   STR:CONTAINS? TTRUE ;

: FFI-SV-UNSELECTED ( -- )
   s" an unreadable declaration with no library selected" T-LABEL
   SB-RESET S\" require src/lib/ffi-abi.f\nFUNCTION: FFI-SV ( -- )\n" SB-APPEND
   false FFI-SV-CHECK-AS 1 T=
   LSP-CHECK:VERIFY-OUT$ FFI-SV-UNSELECTED-HAS
   FFI-SV-WRITE
   s" test/ffi-sv.f" E2E-TREE:ARGS
   ENGINE-CANDIDATE:PATH$ E2E-TREE:ROOT$ E2E-TREE:RUN-ON
   E2E-TREE:RC@ 70 T=
   E2E-TREE:ERR$ FFI-SV-UNSELECTED-HAS ;

: FFI-T-STRUCT-VERIFY ( -- )
   FFI-SV-SETUP
   FFI-SV-ACCEPTED
   FFI-SV-STRUCT-ARG
   FFI-SV-STRUCT-RESULT
   FFI-SV-DECLARATIONS
   FFI-SV-DEFERRED
   FFI-SV-GROUP-CLOSE
   FFI-SV-GROUP-REDEFINED
   FFI-SV-SHADOWED
   FFI-SV-REEXPORTED
   FFI-SV-EXPORTED
   FFI-SV-UNSELECTED ;

FFI-T-STRUCT-VERIFY

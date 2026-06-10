\ install.fs — wire the codegen into the live `:` checker. When CODEGEN-ON?,
\ every successfully-checked definition whose body is in the native subset is
\ recorded in CODE-TABLE (name -> body text); bodies using words the codegen
\ doesn't model are skipped (all-or-nothing, never breaks the definition).
\ RUN-NATIVE compiles a recorded word to a native Mac executable and runs it.
\ Load via caf-cg.fs (after the checker + colon override).

require walk.fs

variable CODEGEN-ON?   CODEGEN-ON? off

wordlist constant CODE-TABLE

: CG-RECORD ( na nu ba bu -- )            \ store body bytes under name
   2swap nextname                          \ name the next word
   get-current >r  CODE-TABLE set-current  create  r> set-current
   dup ,                                   \ field0 = body length
   here >r  dup allot  r> swap move ;      \ then the body bytes

: CG-FIND ( a u -- ba bu )                 \ recorded body for a name
   CODE-TABLE search-wordlist 0= if E-NO-ENC throw then
   execute  dup cell+ swap @ ;             \ ( bytes-addr len )

: TRY-WALK ( ba bu -- )  ICODE-RESET cf-reset  WALK-BODY ;

\ Hook: validate the body compiles, then record it. Silent on unmodeled words.
: DO-CODEGEN ( -- )
   CODEGEN-ON? @ 0= if exit then
   CAP$  ['] TRY-WALK catch if  2drop exit then   \ uncompilable → skip
   NM@ CAP$ CG-RECORD ;

' DO-CODEGEN is CODEGEN-HOOK

\ Compile a recorded word to native and run it with one i64 input.
: RUN-NATIVE ( input "name" -- exit-code )
   parse-name CG-FIND  rot  NATIVE-EVAL ;

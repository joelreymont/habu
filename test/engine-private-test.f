\ engine-private-test.f - a program cannot name the engine's private words.
\ Each case's source is rejected with the same error line by `--load`'s
\ evaluate-closed and by tools/check.f over that source written to a file.

require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/test/ending.f
require src/lib/engine-candidate.f

package ENGINE-PRIVATE
private

T-RESET

TYPED-VARIABLE TCE-A ^u8   variable TCE-U
: TCE-GO ( -- )  TCE-A @ TCE-U @ evaluate-closed ;

2048 constant TCE-CAP
create TCE-PATH PATH-CAP allot   variable TCE-PATH-U
create TCE-OUT TCE-CAP allot
create TCE-ERR TCE-CAP allot
create TCE-NONE 1 allot

: TCE-FILE ( -- ^u8 n )  TCE-PATH TCE-PATH-U @ ;

FS:CLEANUP-RESET
s" habu-engine-private" FS:MKTEMP-DIR 2dup FS:CLEANUP-TREE+
s" case.f" TCE-PATH FS:JOIN-PATH TCE-PATH-U !

\ One tools/check.f process per case: it stops at a file's first rejection.
: TCE-CHECK ( ^u8 n ^u8 n -- ) {: src:^ srcu:n wa:^ wu:n :}
   TCE-FILE src srcu FS:WRITE-ALL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+  s" tools/check.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+  TCE-FILE >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN  TCE-NONE 0 >LEN  TCE-OUT TCE-CAP >LEN
   TCE-ERR TCE-CAP >LEN  PROCESS:WAIT-FOREVER
   RUN-ARGV-STDIN-CAPTURE-OUTCOME {: outu:len erru:len oc :}
   TCE-ERR erru LEN>N wa wu CONTAINS? TTRUE
   src srcu TCE-OUT outu LEN>N TCE-ERR erru LEN>N oc 70
   T-ENDING-EXITED= ;

public

\ TCE-REJECTS asserts the string stops with the checker's status after a record
\ that starts with the text given, under evaluate-closed, and holds it under
\ tools/check.f. The record quotes the calling line, which holds the text.
: TCE-REJECTS ( ^u8 n ^u8 n -- ) {: src:^ srcu:n wa:^ wu:n :}
   src TCE-A !  srcu TCE-U !
   REJECTION:CAPTURE  [: TCE-GO ;] catch  REJECTION:BUFFER-OFF
   70 T=  REJECTION:BUFFER$ wa wu STARTS-WITH? TTRUE
   src srcu wa wu TCE-CHECK ;

;package

\ PE-OPEN and BODY-ARM are private to the checker's sealed package CHECK: user
\ source naming them is undefined.
s\" : EPV-ROWED ( -- n ) 1 ; s\q EPV-ROWED\q PE-OPEN PE-N PE-OUT PE-CLOSE"
   s" error[E-UNDEFINED]: cannot find word `PE-OPEN`" ENGINE-PRIVATE:TCE-REJECTS
s\" : EPV-ARM ( -- bool ) s\q EPV-ONE\q CHECK:BODY-ARM ;"
   s" error[E-UNDEFINED]: cannot find word `CHECK:BODY-ARM`" ENGINE-PRIVATE:TCE-REJECTS

\ The effect registrars are private: a program that could rewrite a word's
\ recorded effect would load code that underflows at run time.
s\" : EPV-TWO ( -- n ) 2 ;\ns\q EPV-TWO\q s\q -- n n\q trust-decl\n: EPV-USE ( -- n n ) EPV-TWO ;\nEPV-USE . . cr"
   s" error[E-UNDEFINED]: cannot find word `trust-decl`" ENGINE-PRIVATE:TCE-REJECTS
s\" : EPV-ONE ( -- n ) 1 ;\ns\q EPV-ONE\q s\q -- n n\q trust-raw"
   s" error[E-UNDEFINED]: cannot find word `trust-raw`" ENGINE-PRIVATE:TCE-REJECTS
s\" : EPV-ROW ( -- bool ) s\q EPV-ONE\q s\q -- n n\q CHECK:ROW-DECLARED ;"
   s" error[E-UNDEFINED]: cannot find word `CHECK:ROW-DECLARED`" ENGINE-PRIVATE:TCE-REJECTS

\ TRUST-DECL's row registrar and its outcome probes are private too: a program
\ passing a name span no memory has would fault in the registration.
s\" NULL-PTR 5 s\q -- n\q TRUST-DECL-ROW ."
   s" error[E-UNDEFINED]: cannot find word `TRUST-DECL-ROW`" ENGINE-PRIVATE:TCE-REJECTS
s\" : EPV-KEPT ( -- bool ) s\q EPV-ONE\q s\q -- n n\q TRUST-DECL? ;"
   s" error[E-UNDEFINED]: cannot find word `TRUST-DECL?`" ENGINE-PRIVATE:TCE-REJECTS
s\" : EPV-REJ ( -- bool ) s\q EPV-ONE\q s\q -- n n\q TRUST-DECL-REJECTED? ;"
   s" error[E-UNDEFINED]: cannot find word `TRUST-DECL-REJECTED?`" ENGINE-PRIVATE:TCE-REJECTS

\ A program rebinds the family-arity defer the checker runs.
s" : MYAR ( n -- n ) drop 0 ; : SETIT ( -- ) ['] MYAR is TFAM-ARITY-XT ;"
   s" error[E-UNDEFINED]: `TFAM-ARITY-XT` names no word" ENGINE-PRIVATE:TCE-REJECTS

\ A program rebinds the defer that checks each declared name.
s" : MYPRE ( ^u8 n -- ) 2drop ; : SETIT ( -- ) ['] MYPRE is TYPE-DECL:TDECL-NAME-PREFLIGHT-XT ;"
   s" error[E-UNDEFINED]: `TYPE-DECL:TDECL-NAME-PREFLIGHT-XT` names no word" ENGINE-PRIVATE:TCE-REJECTS

\ A program states a primitive row for its own word, so a caller is checked
\ against an effect the definition does not have.
s\" : FOOB ( -- n ) 1 ;  s\q FOOB\q CHECK:PRIM-OPEN 8 CHECK:PRIM-ATOM 2 CHECK:PRIM-ATOM s\q \q true false CHECK:PRIM-CLOSE  : T2 ( -- n ) FOOB ;"
   s" error[E-UNDEFINED]: cannot find word `CHECK:PRIM-OPEN`" ENGINE-PRIVATE:TCE-REJECTS

\ A program ends the primitive rows.
s" CHECK:PRIMS-END"
   s" error[E-UNDEFINED]: cannot find word `CHECK:PRIMS-END`" ENGINE-PRIVATE:TCE-REJECTS

\ A program trusts its own word to an effect it does not have.
s\" : FOOB ( -- n ) 1 ; s\q FOOB\q s\q -- n n\q TRUST : T ( -- n ) FOOB + ;"
   s" error[E-UNDEFINED]: cannot find word `TRUST`" ENGINE-PRIVATE:TCE-REJECTS

\ A program takes the xt installer that walks every visible word.
s" ' CHECKER-RESOLVE:EACH-VISIBLE drop"
   s" error[E-UNDEFINED]: cannot find word `CHECKER-RESOLVE:EACH-VISIBLE`" ENGINE-PRIVATE:TCE-REJECTS

\ The child makers are private: a child a program makes with one faults at
\ run time.
s\" s\q FOO\q s\q ^u8\q TYPE-DECL:TDECL-STORED-CHILD drop"
   s" error[E-UNDEFINED]: cannot find word `TYPE-DECL:TDECL-STORED-CHILD`" ENGINE-PRIVATE:TCE-REJECTS
s\" s\q FOO\q s\q n\q LBUF:BUFFER-CHILD drop"
   s" error[E-UNDEFINED]: cannot find word `LBUF:BUFFER-CHILD`" ENGINE-PRIVATE:TCE-REJECTS

FS:CLEANUP-RUN
T-REPORT

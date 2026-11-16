\ engine-private-test.f - a program cannot name the engine's private words.
\ Each case evaluates one source string, so the record captured is that string's.

require src/lib/test.f

package ENGINE-PRIVATE
private

T-RESET

TYPED-VARIABLE TCE-A ^u8   variable TCE-U
: TCE-GO ( -- )  TCE-A @ TCE-U @ evaluate-closed ;

public

\ TCE-REJECTS asserts the string stops with the checker's status after a record
\ that holds the text given.
: TCE-REJECTS ( ^u8 n ^u8 n -- ) {: wa:^ wu:n :}
   TCE-U ! TCE-A !  REJECTION:CAPTURE  [: TCE-GO ;] catch  REJECTION:BUFFER-OFF
   CHECKER-REJECT-RC T=  REJECTION:BUFFER$ wa wu CONTAINS? TTRUE ;

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

\ A program opens a primitive row and ends the primitive rows.
s\" s\q T:R\q CHECK:PRIM-OPEN s\q \q false false CHECK:PRIM-CLOSE : P6 ( -- ) ; CHECK:PRIMS-END"
   s" error[E-UNDEFINED]: cannot find word `CHECK:PRIM-OPEN`" ENGINE-PRIVATE:TCE-REJECTS

\ A program trusts its own word to an effect it does not have.
s\" : FOOB ( -- n ) 1 ; s\q FOOB\q s\q -- n n\q TRUST : T ( -- n ) FOOB + ;"
   s" error[E-UNDEFINED]: cannot find word `TRUST`" ENGINE-PRIVATE:TCE-REJECTS

\ A program takes the xt installer that walks every visible word.
s" ' CHECKER-RESOLVE:EACH-VISIBLE drop"
   s" error[E-UNDEFINED]: cannot find word `CHECKER-RESOLVE:EACH-VISIBLE`" ENGINE-PRIVATE:TCE-REJECTS

T-REPORT

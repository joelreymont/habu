\ structure-mutual.f - MUTUAL (src/core/structure-decl.f): two structures that
\ point at each other load and run, and each MUTUAL rule rejects one program
\ with its code, reason and token. A rejected MUTUAL leaves its names free, and
\ one whose list is ordered against its bodies closes in body order. The
\ verifier's scan (src/habu/verify-source.f RECORD-MUTUAL) registers what a
\ MUTUAL declares, as the load does.
require src/lib/test.f
require src/habu/verify-source.f
require test/decl-diag-capture.f

package SMUTUAL-LIN
public
DEFLINEAR SMUTUAL-LIN:tok
;package

package SMUTUAL

MUTUAL smnode smlink
STRUCTURE smnode
   DERIVE addr
   FIELD id n
   FIELD first ^smlink
;STRUCTURE
STRUCTURE smlink
   DERIVE addr
   FIELD owner ^smnode
   FIELD next ^smlink
;STRUCTURE
;MUTUAL

TYPED-VARIABLE NODE-V smnode
TYPED-VARIABLE LINK-V smlink

\ Stores the node and the link each in the other, then reads the node's id
\ back through link-to-node and through node-to-link-to-link-to-node.
: BOTH-WAYS ( -- n n )
   7 NODE-V SMNODE-ID !
   LINK-V NODE-V SMNODE-FIRST !
   NODE-V LINK-V SMLINK-OWNER !
   LINK-V LINK-V SMLINK-NEXT !
   NODE-V SMNODE-FIRST @ SMLINK-OWNER @ SMNODE-ID @
   LINK-V SMLINK-OWNER @ SMNODE-FIRST @ SMLINK-NEXT @ SMLINK-OWNER @ SMNODE-ID @ ;

\ REJECTS asserts the text stops the load with a declaration record whose
\ token ta carries the label la, raised with code.
: REJECTS ( str str str n -- )
   {: sa:str ta:str la:str code:n :}
   DECL-DIAG:PROSE
   sa TEST-EVAL:RC 70 T=
   s\" error[E-BAD-DECLARATION]: bad mutual declaration" DECL-DIAG:HAS? TTRUE
   ta la DECL-DIAG:CAUSE? TTRUE
   code DECL-REJECT:REASON$ la DECL-DIAG:SAME? TTRUE
   DECL-DIAG:OFF ;

\ The rejections a multi-error load of the text tallies.
: TALLY ( str -- n ) {: s:str :}
   CHECK:MULTI-ERR-BEGIN s TEST-EVAL:RC drop CHECK:MULTI-ERR-END ;

\ The rejections a multi-error verifier scan of the text tallies.
: SCAN-TALLY ( str -- n ) {: s:str :}
   CHECK:MULTI-ERR-BEGIN [: s VERIFY:SOURCE-BUF ;] catch drop CHECK:MULTI-ERR-END ;

\ Definitions after the pair call both members' MAKE and an accessor. Both
\ members hold a `next`: a field name repeats across members.
: SMV-SRC ( -- str )
   s" MUTUAL smvna smvlb STRUCTURE smvna DERIVE addr FIELD id n FIELD next ^smvlb ;STRUCTURE STRUCTURE smvlb DERIVE addr FIELD owner ^smvna FIELD next ^smvlb ;STRUCTURE ;MUTUAL : SMV-MK ( n ^smvlb -- smvna ) SMVNA-MAKE ; : SMV-MKB ( ^smvna ^smvlb -- smvlb ) SMVLB-MAKE ; : SMV-ID ( ^smvlb -- n ) SMVLB-OWNER @ SMVNA-ID @ ;" ;
: SMV-SCAN ( -- ) SMV-SRC VERIFY:SOURCE-BUF ;
\ Within one member a field name is still a duplicate.
: SMV-DUP ( -- )
   s" MUTUAL smvdc smvdd STRUCTURE smvdc FIELD id n FIELD id ^smvdd ;STRUCTURE STRUCTURE smvdd FIELD owner ^smvdc ;STRUCTURE ;MUTUAL"
   VERIFY:SOURCE-BUF ;

\ The list is ordered against the bodies: smoentry holds smokey by value and
\ both derive eq, so smoentry's EQ calls smokey's, which the close generates
\ first because its ;STRUCTURE was read first.
: SMO-SRC ( -- str )
   s" MUTUAL smoentry smokey smotree STRUCTURE smokey DERIVE eq FIELD id n ;STRUCTURE STRUCTURE smoentry DERIVE eq FIELD k smokey ;STRUCTURE STRUCTURE smotree FIELD e smoentry FIELD up ^smotree ;STRUCTURE ;MUTUAL : SMO-SAME ( -- bool ) 5 SMOKEY-MAKE SMOENTRY-MAKE 5 SMOKEY-MAKE SMOENTRY-MAKE SMOENTRY-EQ ;" ;

: RUN ( -- )
   T-RESET
   BOTH-WAYS 2dup ." mutual: " . . cr
   7 T=  7 T=

   s" MUTUAL smva smvb STRUCTURE smva FIELD x smvb ;STRUCTURE STRUCTURE smvb FIELD y n ;STRUCTURE ;MUTUAL"
      s" smvb" s" type names a MUTUAL member before its `;STRUCTURE`" 7109 REJECTS
   \ The same names declare again: the rejected MUTUAL left no family.
   s" MUTUAL smva smvb STRUCTURE smva FIELD x ^smvb ;STRUCTURE STRUCTURE smvb FIELD y smva ;STRUCTURE ;MUTUAL"
      TEST-EVAL:RC 0 T=

   s" MUTUAL smla smlb STRUCTURE smla FIELD x ^smlb ;STRUCTURE ;MUTUAL"
      s" smlb" s" member has no STRUCTURE" 7107 REJECTS
   s" MUTUAL smua smub STRUCTURE smua FIELD x ^smub ;STRUCTURE STRUCTURE smuc FIELD y n ;STRUCTURE ;MUTUAL"
      s" smuc" s" structure is not a member of this MUTUAL" 7107 REJECTS
   \ A member's name is judged as a plain STRUCTURE's before it is looked up.
   s" MUTUAL smcna smcnb STRUCTURE SMCNA FIELD x ^smcnb ;STRUCTURE STRUCTURE smcnb FIELD y n ;STRUCTURE ;MUTUAL"
      s" SMCNA" s" name must be a lowercase family tail" 7101 REJECTS
   \ The pointee was read fieldless; ;MUTUAL judges it again once it has fields.
   s" MUTUAL smra smrb STRUCTURE smra FIELD x ^smrb ;STRUCTURE STRUCTURE smrb FIELD r SMUTUAL-LIN:tok ;STRUCTURE ;MUTUAL"
      s" ;MUTUAL" s" type is a pointer to a linear value and cannot own it" 7109 REJECTS
   \ A member left open reads ;MUTUAL as a clause and is rejected there; the load
   \ goes on after it, as the verifier's scan does, and rejects both definitions.
   s" MUTUAL smqa smqb STRUCTURE smqa FIELD x ^smqb ;MUTUAL : SMQ-W1 ( -- n ) ; : SMQ-W2 ( -- n ) ;"
      TALLY 3 T=
   \ A member's unclosed application stops at ;MUTUAL, where the verifier's scan
   \ ends the declaration, and the load goes on to reject the definition after it.
   s" MUTUAL smsa smsb STRUCTURE smsa FIELD x box< ;MUTUAL : SMS-W ( -- n ) ;"
      TALLY 2 T=
   \ Every ;MUTUAL ends the declaration, a member's name position included: the
   \ load and the verifier's scan each reject the MUTUAL and the definition.
   s" MUTUAL smta smtb STRUCTURE ;MUTUAL : SMT-W1 ( -- n ) ;"
      dup TALLY 2 T=  SCAN-TALLY 2 T=
   \ A pointer reaches no family with a scope parameter. smkb's slot becomes a
   \ scope only in its own field, so that field rejects smka's earlier pointer,
   \ placed at the pointee's name.
   DECL-DIAG:PROSE
   s" MUTUAL smka smkb<p> STRUCTURE smka FIELD x n FIELD q ^smkb<n> ;STRUCTURE STRUCTURE smkb<p> FIELD v read-view<p,p,u8> ;STRUCTURE ;MUTUAL"
      TEST-EVAL:RC 70 T=
   s\" error[E-BAD-DECLARATION]: bad mutual declaration `smkb`\n" DECL-DIAG:HAS? TTRUE
   s" smkb" s" a pointer cannot reach `smkb`, which takes a scope or region" DECL-DIAG:CAUSE? TTRUE
   7109 DECL-REJECT:REASON$ s" a pointer cannot reach `smkb`, which takes a scope or region" DECL-DIAG:SAME? TTRUE
   DECL-DIAG:OFF
   \ One member's pointer at itself does not hold another member's scope.
   s" MUTUAL smja<x> smjb<p> STRUCTURE smja<x> FIELD n1 ^smja<x> ;STRUCTURE STRUCTURE smjb<p> FIELD v read-view<p,p,u8> ;STRUCTURE ;MUTUAL"
      TEST-EVAL:RC 0 T=

   ['] SMV-SCAN catch 0 T=
   SMV-SRC TEST-EVAL:RC 0 T=
   DECL-DIAG:PROSE
   ['] SMV-DUP catch 70 T=
   s\" error[E-BAD-DECLARATION]: bad mutual declaration `smvdc`\n --> <input>:1:53\n" DECL-DIAG:HAS? TTRUE
   s\" ^^ duplicate field name\n" DECL-DIAG:HAS? TTRUE
   DECL-DIAG:OFF

   \ It loads, and the holder's EQ answers true on two equal records.
   SMO-SRC TEST-EVAL:RC 0 T=
   s" SMO-SAME" TEST-EVAL:FLAG TTRUE
   T-REPORT ;

RUN
;package

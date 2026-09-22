\ native-qualified-name.f - pending record identity for qualified definitions.
\
\ The engine stores a qualified definition under its bare tail and target WID,
\ while the compiler tape retains the original PACKAGE:TAIL token. This runs the
\ ordinary colon path in both directions: a RESULT:OK-shaped definition must
\ compile, and a pending record moved to another package with the same tail must
\ fail at the compiler's name-identity gate rather than pass on its tail alone.
\ Tier 1 first: that gate is the optimizing compiler's - at the default tier
\ the rehomed record passes it and the file dies on its own diagnostic.
1 set-tier

require src/compiler/native/compiler.f

package NCOMP-NAME-A ;package
package NCOMP-NAME-B ;package
package NCOMP-NAME-RESULT ;package

: NCOMP-NAME-RESULT:OK ( n -- n )
   1+ ;

package NCOMP-NAME-TEST

private

s" NCOMP-NAME-B" XREF-NAMESPACE-WL XREF-FIND-WL XREF-START
constant OTHER-WID

PTR-VARIABLE SRC-A
variable SRC-U

: SOURCE-GO ( -- )
   SRC-A @ SRC-U @ INCLUDE-EVALUATE ;

: SOURCE-RC ( ptr u8 n -- n )
   SRC-U ! SRC-A ! [: SOURCE-GO ;] catch ;

: POSITIVE ( -- )
   66 NCOMP-NAME-RESULT:OK 67 = if exit then
   s" native qualified name: owning namespace rejected" 1 die ;

: NEGATIVE ( -- )
   s" : NCOMP-NAME-A:SAME ( -- n ) NCOMP-NAME-TEST:REHOME-PENDING 1 ;"
   SOURCE-RC E-NCOMP-NAME = if exit then
   s" native qualified name: wrong namespace passed the name gate" 1 die ;

public

\ Parsing immediates execute after the engine has made the pending record and
\ before the native compiler checks it. Move only that record's WID to another
\ live namespace with the same prospective tail; the source token stays A:SAME.
: REHOME-PENDING ( -- )
   ndict@ XREF-REC XREF-WORDLIST-SLOT cells XREF-REC+
   OTHER-WID swap XREF-PATCH32 ;
immediate
s" NCOMP-NAME-TEST:REHOME-PENDING" 0 parse-imm

private

: RUN ( -- )
   POSITIVE
   NEGATIVE
   s" test: ok" type cr ;

RUN

;package

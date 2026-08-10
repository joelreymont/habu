\ asm-package-test.f - the ARM64 assembler's package boundary.
\
\ src/arch/arm64/asm.f used to publish its whole surface globally, so every program
\ carried `ENC-B`, `ENC-ADR` and the rest whether it wanted them or not, and a
\ package that published one of those tails could not be imported beside it: a
\ bare reference hit E-USING-SHADOW-GLOBAL (checker code 7141), the global and
\ the used public exporting one name. Package A64ASM ends that, and this file
\ measures the boundary from both sides.
\
\ Each case runs a source string through INCLUDE-EVALUATE under catch, so an
\ interpret- or compile-time reject surfaces as the engine's own throw code and 0
\ means accepted. What each case answered while the assembler was global is
\ recorded beside it, so reverting the package boundary reds this file instead of
\ quietly restoring the collision.
\
\ THE SECOND PARTY. COLLIDER publishes a `ENC-B ( n -- )` of its own - the shape
\ maki/onnx/encode.f's protobuf byte appender has, which is the collision that
\ measured the problem. It is declared here rather than imported because the
\ habu<-maki dependency guard (tools/maki-dep-lint-core.f) forbids a test/ file
\ from naming a maki/ path; maki/onnx/asm-collide-test.f runs the same inversion
\ against the real encoder from the side that is allowed to. What the fixture has
\ to be is any package publishing that tail with an effect the assembler's ENC-B
\ ( n -- n ) does not share, and the differing effects are what make the cases
\ observable: a body certifying as ( n -- ) reached COLLIDER and a body
\ certifying as ( n -- n ) reached the assembler, so each pair pins WHICH word a
\ bare tail found rather than only that something resolved.

require lib/test.f
require lib/string.f
require src/arch/arm64/asm.f

\ The colliding second party: one public tail, ENC-B, with an effect of its own.
package COLLIDER
private

variable CO-LAST

public

: ENC-B ( n -- )  CO-LAST ! ;

: ENC-LAST ( -- n )  CO-LAST @ ;

;package

package ASM-PACKAGE-TEST
private

\ Evaluate a source string, answering its throw code (0 = accepted).
variable AP-A   variable AP-U
: AP-GO ( -- )  AP-A @ AP-U @ INCLUDE-EVALUATE ;
: AP-EVAL ( ptr u8 n -- n )  AP-U ! AP-A !  [: AP-GO ;] catch ;

70   constant E-REJECT      \ E-UNDEFINED, or a body the checker refuses
94   constant E-AMBIGUOUS   \ the tail resolves in more than one used package

\ The unconditional branch with a zero displacement: what the assembler's ENC-B
\ answers, and nothing a byte appender could produce.
$14000000 constant B-ZERO

: RESOLUTION-CASES ( -- )
   \ THE INVERSION. No import is open, so a bare ENC-B has nowhere to resolve.
   \ While the assembler was global this was 0 and the branch encoder answered.
   s" : APT-NO-IMPORT ( n -- n ) ENC-B ;" AP-EVAL E-REJECT T=
   \ THE INVERSION, other side. A package's own public of that tail is reachable
   \ bare beside the loaded assembler. While it was global this was 7141.
   s" using COLLIDER : APT-CO ( n -- ) ENC-B ; ;using" AP-EVAL 0 T=
   \ ...and it is COLLIDER's word: the branch encoder's effect does not certify.
   s" using COLLIDER : APT-CO-BAD ( n -- n ) ENC-B ; ;using" AP-EVAL E-REJECT T=
   \ The assembler's ENC-B is still reachable, through its own package.
   s" using A64ASM : APT-ASM ( n -- n ) ENC-B ; ;using" AP-EVAL 0 T=
   \ ...and it is the encoder, not the appender.
   s" using A64ASM : APT-ASM-BAD ( n -- ) ENC-B ; ;using" AP-EVAL E-REJECT T=
   \ Both imports open: the tail really does live in two packages now, so the
   \ checker refuses it as a genuine ambiguity rather than picking one. This is
   \ what proves the two cases above were not both answered by one word.
   s" using A64ASM using COLLIDER : APT-BOTH ( n -- ) ENC-B ; ;using ;using"
      AP-EVAL E-AMBIGUOUS T=
   \ Qualified names always work and never collide, in one body, in either order.
   s" : APT-QUAL ( n n -- n ) A64ASM:ENC-B swap COLLIDER:ENC-B ;" AP-EVAL 0 T= ;

\ The public surface is a decision, not a section marker: the machinery the
\ encoders are built from stays behind the boundary. A bit-layout stencil, an
\ operand screen and the word mask are all unreachable under a qualifier.
: PRIVACY-CASES ( -- )
   s" : APT-PRIV-RRR ( n n n n -- n ) A64ASM:RRR ;" AP-EVAL E-REJECT T=
   s" : APT-PRIV-XREG ( n -- n ) A64ASM:XREG? ;" AP-EVAL E-REJECT T=
   s" : APT-PRIV-MSK ( n -- n ) A64ASM:MSK ;" AP-EVAL E-REJECT T=
   s" : APT-PRIV-COND ( n -- n ) A64ASM:?COND ;" AP-EVAL E-REJECT T= ;

using COLLIDER

\ Both words RUN, from one file, under the import that used to be refused.
: VALUE-CASES ( -- )
   $2A ENC-B                                   \ COLLIDER's appender: bare
   ENC-LAST $2A T=
   0 A64ASM:ENC-B B-ZERO T= ;

;using

public

: RUN ( -- )
   RESOLUTION-CASES
   PRIVACY-CASES
   VALUE-CASES ;

;package

T-RESET
ASM-PACKAGE-TEST:RUN
T-REPORT

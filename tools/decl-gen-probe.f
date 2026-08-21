\ decl-gen-probe.f — staged render-vs-certify separator for declaration-time
\ constructor generation.
\
\ Why this exists. Constructor generation is four stages behind one entry point:
\ capture the family's payload view through a provider, render each constructor's
\ text into the plan, evaluate the whole plan, then seal the constructor
\ wordlist. When generation fails, the single throw code that escapes says
\ nothing about WHICH stage produced it, and the stages fail for completely
\ different reasons — a payload view the provider could not answer, text that
\ renders but does not certify, or a protection/capacity limit reached after the
\ words already exist. This tool ran each stage under its own `catch` and located
\ exactly that boundary while the ORDER 820 constructor participant was being
\ designed; it answered "the committed provider already sees the field rows, the
\ wall is ordering, not readership" in one run. Future generation work starts
\ here rather than by print-bisecting the generator.
\
\ It also prints the rendered text for each constructor, so a generation failure
\ can be read as text rather than inferred from a code.
\
\ The probe RENDERS but never publishes: it drives the plan buffer and reads the
\ text back, and it clears the pending-constructor authority it touched on the
\ way out. It deliberately refuses a family whose constructors already exist.
\ sumtype.f's TDPLAN-NAME+ dies on re-rendering a defined name, and that guard is
\ correct — a second plan row for a live constructor is exactly the corruption it
\ exists to stop. So the probe's subject is a declaration whose generation did
\ NOT complete: a private family, a family whose front end does not arm
\ generation, or a failed declaration being diagnosed. For one that generated
\ successfully the probe reports that and renders nothing, which is the honest
\ answer rather than a bypassed guard.
\
\ Usage:
\   bin/hb --load tools/decl-gen-probe.f -- <family-tail> '<declaration source>'
\ The declaration source is evaluated first, then the named family's constructor
\ plan is captured, rendered, and dumped. The family tail is resolved in whatever
\ package is active when the source finishes, so a source that opens a package
\ and leaves it open lets the probe reach a package-scoped family:
\   bin/hb --load tools/decl-gen-probe.f -- pmsg \
\     'package pm ENUM-DECL:ED-RUN pmsg 0 VARIANT quit ;VARIANT ;ENUM'
\ That form is also how to reach the render path at all today: a top-level public
\ ENUM generates during its own declaration, so the probe reports it as already
\ generated, while the package-private form leaves the plan unrendered.

require lib/errors.f
require lib/string.f
require lib/argv.f

using TFAM
using TYPE-DECL

package DECL-GEN-PROBE

\ Trusted forwarders to the pre-hook generator, plan buffer, and family registry.
\ Each is a metaprogramming boundary the checker cannot type from a post-hook
\ checked body, the same idiom src/core/enum-decl.f uses for its registry seams.
TRUSTED: EV ( ptr u8 n -- ) evaluate ;
TRUSTED: TRY ( ptr u8 n -- n ) ['] EV catch ;
TRUSTED: FAM-FIND ( ptr u8 n -- n bool ) TFAM-ACTIVE-PKG$ 2swap TFAM-SIG-RESOLVE ;
TRUSTED: FAM-VAR-START ( n -- n ) TFAM-VAR-START@ ;
TRUSTED: FAM-VAR-COUNT ( n -- n ) TFAM-VAR-COUNT@ ;
TRUSTED: FAM-SLOTS ( n -- n ) TFAM-SLOTS@ ;
TRUSTED: FAM-PUBLIC? ( n -- bool ) TFAM-PUBLIC? ;
TRUSTED: PROVIDER ( -- n [ n n n -- n ] [ n n n n -- n ] [ n n n -- n ] )
   TDECL-SUMV-PROVIDER ;
TRUSTED: CAPTURE ( n [ n n n -- n ] [ n n n n -- n ] [ n n n -- n ] n -- )
   TDPV-CAPTURE ;
TRUSTED: PLAN-BEGIN ( -- ) TDPLAN-BEGIN ;
TRUSTED: PLAN-ROWS ( -- n ) TDPLAN-N @ ;
TRUSTED: PLAN-DEF$ ( n -- ptr u8 n ) TDPLAN-DEF$ ;
TRUSTED: RENDER-ONE ( n n -- ) TDECL-CTOR-WORD ;
TRUSTED: PEND-CLEAR ( -- ) CTOR-PEND-CLEAR ;
TRUSTED: VAR-CTOR-SYM ( n -- n ) SUMV-CTOR-SYM@ ;

7177 constant E-PROBE-FAMILY   \ named family does not resolve in the active package

variable FAM        \ family under probe
variable RC-DECL    \ throw code from evaluating the declaration source
variable RC-CAP     \ throw code from capturing the payload view
variable RC-REND    \ throw code from rendering the whole constructor set
variable PI         \ dump cursor

: FAM! ( ptr u8 n -- )                    \ resolve the family tail or fail closed
   FAM-FIND 0= IF drop E-PROBE-FAMILY throw THEN
   FAM ! ;

: CAPTURE-VIEW ( -- ) PROVIDER FAM @ CAPTURE ;
: RENDER-SET ( -- )                       \ one plan row per variant, in tag order
   PLAN-BEGIN
   0 PI !
   BEGIN PI @ FAM @ FAM-VAR-COUNT < WHILE
      FAM @  FAM @ FAM-VAR-START PI @ +  RENDER-ONE
      PI @ 1 + PI !
   REPEAT ;

: STAGE. ( ptr u8 n n -- ) {: la:ptr lu:n rc:n :}   \ one stage line: label + throw code
   la lu type rc . cr ;

: DUMP-PLAN ( -- )                        \ the rendered text, one definition per line
   0 PI !
   BEGIN PI @ PLAN-ROWS < WHILE
      s" def " type PI @ . s" : " type PI @ PLAN-DEF$ type cr
      PI @ 1 + PI !
   REPEAT ;

\ A generated variant carries the constructor symbol TDPLAN-CTOR+ recorded for
\ it; an ungenerated one carries none.
: GENERATED? ( -- bool )
   FAM @ FAM-VAR-COUNT 0 <= IF 0 0= 0= EXIT THEN
   FAM @ FAM-VAR-START VAR-CTOR-SYM 0 <> ;

: HEADER ( -- )
   s" family:   " type FAM @ . cr
   s" public:   " type FAM @ FAM-PUBLIC? IF s" yes" ELSE s" no" THEN type cr
   s" variants: " type FAM @ FAM-VAR-COUNT . cr
   s" slots:    " type FAM @ FAM-SLOTS . cr ;

public

\ Probe one already-declared family: capture, render, dump. Each stage runs under
\ its own catch so the report names the stage that failed instead of one code.
: PROBE ( ptr u8 n -- ) {: na:ptr nu:n :}
   na nu FAM!
   HEADER
   GENERATED? IF
      s" already generated: constructors are live, nothing to re-render" type cr
      EXIT THEN
   [: CAPTURE-VIEW ;] catch RC-CAP !
   s" capture throw: " RC-CAP @ STAGE.
   RC-CAP @ 0 <> IF PEND-CLEAR EXIT THEN
   [: RENDER-SET ;] catch RC-REND !
   s" render throw:  " RC-REND @ STAGE.
   RC-REND @ 0= IF DUMP-PLAN THEN
   PEND-CLEAR ;

: DECLARE ( ptr u8 n -- )                 \ evaluate the declaration under catch
   TRY RC-DECL !
   s" declare throw: " RC-DECL @ STAGE. ;

\ RUN is public and is invoked AFTER the package closes. A declaration evaluated
\ while this package is still open would inherit its private visibility, and a
\ private family publishes no constructors at all — the probe would then report a
\ correct render of the wrong thing.
: RUN ( -- )
   s" tools/decl-gen-probe.f <family-tail> '<declaration source>'" ARGV:USAGE!
   ARGV:PARSE
   2 2 ARGV:EXPECT-POS
   1 ARGV:POS$ DECLARE
   RC-DECL @ 0 <> IF EXIT THEN
   0 ARGV:POS$ PROBE ;

private

;package

DECL-GEN-PROBE:RUN

;using
;using

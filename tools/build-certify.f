\ build-certify.f - the two lines a certify of generated engine source prints:
\ the rejection report and the self-check census. tools/build-fixpoint.f prints
\ them for a source it certifies in its own process, and its certify child
\ (tools/build-fixpoint-certify.f) for a source certified against the core
\ prefix, so each text has this one definition.

require src/lib/fmt.f                        \ FMT:.INT
require src/habu/verify-source.f         \ VERIFY:CENSUS
require tools/build-target.f             \ the target the census names

package BUILD-CERTIFY
private

74 constant BUILD-RC

: TARGET-UNKNOWN ( -- )
   s" build-certify: unknown target" BUILD-RC die ;

public

\ The build's target, by the name the census line gives it.
: TARGET$ ( -- ptr u8 n )
   BUILD-TARGET:LINUX? if s" linux-arm64" exit then
   BUILD-TARGET:MACOS? if s" macos-arm64" exit then
   BUILD-TARGET:LINUX-X86-64? if s" linux-x86-64" exit then
   TARGET-UNKNOWN ;

\ A certify its scan refused, which blocks the build: LABEL, the throw code,
\ then the diagnostic the scan rendered, when it rendered one.
: REPORT ( ptr u8 n n ptr u8 n -- )
   {: lab:ptr labu:n rc:n diag:ptr diagu:n :}
   s" certify: " type lab labu type
   s"  rejected rc " type rc FMT:.INT s"  (blocking)" type cr
   diagu 0 > IF diag diagu type cr THEN ;

\ Self-check certification census: the boot prefix and the assembled stage
\ source each print, under PHASE, the colon definitions their scan certified
\ and those it deferred (VERIFY:CENSUS). A certify fails closed on the first
\ refused definition, so a phase that reaches its line refused none. Both
\ assemblies include the target's src/os leg, so the line names TARGET, the
\ build's, whichever process prints it.
: CENSUS ( ptr u8 n ptr u8 n -- )
   {: phase:ptr phaseu:n target:ptr targetu:n :}
   VERIFY:CENSUS {: certified:n deferred:n :}
   s" self-check census (" type target targetu type s" ): " type phase phaseu type
   s"  = " type certified FMT:.INT s"  certified, " type
   deferred FMT:.INT s"  deferred to the run" type cr ;

;package

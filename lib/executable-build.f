\ Scoped native compilation for executable build drivers. Cleanup owns the
\ prior tier; changing source tier inside the scope cannot enable JIT emission.
package EXECUTABLE-BUILD
public
\ The two engine scope operations are sealed against direct source calls.
\ This boundary preserves the quotation effect and always runs its cleanup.
TRUSTED: WITH ( R [ R -- S ] -- S )
   executable-build-enter [: executable-build-leave ;] finally ;
;package

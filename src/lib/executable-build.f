\ The build scope for executable build drivers. It compiles at the caller's
\ tier, and its cleanup restores the caller's tier when the scope's source
\ selected another.
package EXECUTABLE-BUILD
public
\ The two scope primitives are this package's owner-private rows.
\ This boundary preserves the quotation effect and always runs its cleanup.
: WITH ( R [ R -- S ] -- S )
   executable-build-enter [: executable-build-leave ;] finally ;
;package

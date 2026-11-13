\ The build scope for executable build drivers: its cleanup closes the scope
\ however the scope's source ends.
package EXECUTABLE-BUILD
public
\ The two scope primitives are this package's owner-private rows.
: WITH ( R [ R -- S ] -- S )
   executable-build-enter [: executable-build-leave ;] finally ;
;package

split-str
\ sign.f -- Linux executables do not need a post-link code-signing pass.

package ELF
public
: SET-SIGID ( str -- )
   2drop ;

: CODESIG2 ( img -- img ) ;
;package

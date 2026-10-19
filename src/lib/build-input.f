\ Concrete non-source files embedded while an application loads. FILE takes
\ an absolute path; ordinary source loading has no collector and does nothing.
\ hb-build --repl records these inputs. The stripped outer artifact/object
\ caches still use their static source closure.

package BUILD-INPUT
private

defer COLLECT ( ptr u8 n -- )

: IGNORE ( ptr u8 n -- ) 2drop ;

public

: FILE ( ptr u8 n -- ) COLLECT ;
: USE ( [ ptr u8 n -- ] -- ) is COLLECT ;
: RESET ( -- ) [: IGNORE ;] USE ;

;package

BUILD-INPUT:RESET

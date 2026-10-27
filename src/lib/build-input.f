\ Concrete non-source files embedded while an application loads. FILE takes
\ an absolute path; ordinary source loading has no collector and does nothing.
\ hb-build --repl records these inputs, and so does the native definition
\ cache (tools/native-definition-cache.f).
\
\ STORAGE CLASS. PROCESS-WIDE: one deferred COLLECT, set by USE and reset to
\ ignore by RESET.

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

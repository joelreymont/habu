\ bootstrap-using-checker-hook-src.f - stage0 mirrors `using` into the checker.

\ Recovery-emitter fixture, and stage0-only: it defines a stand-in CHECKER-USING
\ so the engine's mirror call is observable. Do NOT load it with the native
\ engine, whose real src/core/checker.f CHECKER-USING would be shadowed by the
\ stand-in and stop recording the import.
\
\ The engine calls the hook with the package-name token, at the depth the import
\ is about to occupy, exactly as the `package` / `public` / `private` hooks are
\ called. That mirror is what lets the checker resolve the same used publics as
\ the engine once the recovery build compiles src/core/checker.f.

: CHECKER-USING ( ptr u8 n -- )
   s" checker-using: " type type cr ;

package BUS-A
public
: BUS-VALUE ( -- n ) 7 ;
;package

using BUS-A
BUS-VALUE .
;using

s" BOOTSTRAP-USING-HOOK-OK" type cr

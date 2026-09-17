\ dynamic-tail-manifest.f - declared dynamic-tail boundaries for discovery.
\
\ The whole-file discovery pass (tools/source-discovery.f) rejects fail-closed
\ any source whose loader dataflow is not statically visible: a dynamic
\ (non-literal) loader path, a shadowed/undefined/retired loader word, or an
\ unsupported string opener before a loader. A file listed here is a reviewed
\ boundary whose loader dataflow the pass cannot read: discovery tolerates
\ (skips) exactly those forms in it and records only its statically-visible
\ loader events, so the event log is a lower bound on what that file loads.
\ Keep this table minimal; every entry carries a one-line reason, and an entry
\ is retired when its unreadable form is replaced by static loader forms.

require lib/errors.f
require lib/string.f

package DTM
using SOURCE-ROOT

create CANON-PATH $401 allot
variable CANON-U

public

3 constant COUNT

: PATH$ ( n -- ptr u8 n ) {: i:n :}
   i 0 = if s" src/habu/driver-io.f" exit then
   i 1 = if s" tools/source-discovery.f" exit then
   i 2 = if s" src/core/include.f" exit then
   E-TBL-BOUNDS throw ;

: REASON$ ( n -- ptr u8 n ) {: i:n :}
   i 0 = if s" DRV-RETIRE-RELOADS retires the loader words by name so built driver images cannot re-enter source composition" exit then
   i 1 = if s" the discovery walker itself drives loader words with scanned path strings in record-only mode (SD-CALL-LOADER)" exit then
   i 2 = if s" it defines the loader words, so their names stand as definition names and inside each other's bodies; it loads no source of its own" exit then
   E-TBL-BOUNDS throw ;

: KNOWN? ( ptr u8 n -- bool )
   CANONICAL drop {: a:ptr u:n :}
   a CANON-PATH u BYTE-COPY u CANON-U !
   0 begin dup COUNT < while
      dup PATH$ CANONICAL drop
      CANON-PATH CANON-U @ STR= if drop STR-TRUE exit then
      1+
   repeat drop STR-FALSE ;

;using
;package

\ Appended after the production common emitter and driver-io.f by
\ pre-trust-defer.f. Emit no captured runtime: every boot reads its cwd's prefix.
package PRE-TRUST-COLD-ENGINE

: RUN ( -- )
   SCRIPT-ARGC 1 <> if s" pre-trust cold engine: expected output path" 76 die then
   0 0= STDIN? !
   NULL$ ENGINE-EMIT:FORTH
   s" hb" 0 SCRIPT-ARGV$ DRV-EMIT-IMAGE ;

RUN
;package

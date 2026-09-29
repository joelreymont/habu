require lib/process-argv.f

package ENTRY-GUARD-ALIAS-LAUNCH

: LAUNCH ( -- )
   s" ./test/gate-entry-guard-target.f" >LEN PROC-ARGV+ ;

;package

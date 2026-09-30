require lib/process-argv.f

package ENTRY-GUARD-SELF-LAUNCH

: LAUNCH ( -- )
   s" test/gate-entry-guard-self-launch.f" >LEN PROC-ARGV+ ;

;package

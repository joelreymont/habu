require lib/process-argv.f

package ENTRY-GUARD-LAUNCH-WRAPPER

: LAUNCH ( -- )
   s" test/gate-entry-guard-import-wrapper.f" >LEN PROC-ARGV+ ;

;package

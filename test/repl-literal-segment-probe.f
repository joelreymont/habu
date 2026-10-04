\ Loaded inside the child REPL before a line opens a literal segment and fails.
require test/compiler/literal-segment.f
package REPL-SEGMENT
public
: CHECK ( -- )
   LITERAL-SEGMENT:SURVIVED?
   if s" segment-survived-pass" else s" segment-survived-fail" then type cr ;
;package

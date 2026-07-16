\ diff-capture-types.f - public capture outcome schemas.

package DIFF-CAPTURE
public

ENUM command-phase
   snapshot
   resolve-from
   resolve-to
   metadata
   raw
   old-content
   new-content
;ENUM

ENUM command-outcome
   succeeded
   exited
   fault
;ENUM

ENUM capture-outcome
   ok
   primary-failed
   cleanup-failed
   combined-failed
;ENUM

;package

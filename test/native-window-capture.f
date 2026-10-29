\ The capture seam of test/native-window-owner.f's tier-1 window, loaded last.
\ It requires each fixture whose checks run here, so each compiles against the
\ fresh checker before any preparation; the family readers and the compiler
\ adapter assert the by-name regime at their load. A fixture dropped from the
\ list fails the window at its block. The order below is each check's
\ precondition.

require test/native-window-owner-family.f
require test/native-window-owner-adapter.f
require test/native-window-owner-payload.f
require test/native-window-tape-detach.f

\ Production binds typed storage's quotation cells before capture. This
\ reduced window needs the same address registry walker for its fresh owner.
require src/core/layout-buffer-address.f

\ One adapter capture ends the by-name regime; the adapter and family captured
\ halves run against the owner-record dispatch it selects, before any checker
\ preparation, as each did alone. Each check runs inside its own package, as at
\ its fixture's load: the checker resolves some names in the current search
\ order (NFAM:CON-FAM of the family `payload`, a scan that calls
\ PAYLOAD-NUMERIC), and only their own package finds them.
CHECKER-OWNER:CAPTURE-PREPARE

package OWNER-ADAPTER-CHECK
CAPTURED
;package

package OWNER-FAMILY-CHECK
CAPTURED
;package

\ Payload validation takes the window's first checker preparations. Its RUN
\ refuses a checker whose signature store a preparation already moved into the
\ data span, or one with a boundary marked, so these compact the no-return rows
\ without a boundary: the NORET-COMPACT branch the build's own capture never
\ takes.
package OWNER-PAYLOAD-CHECK
RUN
;package

\ The prefix-boundary rollback compiles and runs next; its MARK stays taken.
require test/compiler/native-prefix-rollback.f

\ Tape-detach takes the later preparations, which compact against that mark as
\ the build's capture does against src/core/lower-cert-seal.f's. Each follows
\ its own OBSERVE or a checked detach. Nothing prepares after it in this window.
package CHECKER-TAPE
RUN
;package

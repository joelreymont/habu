\ native-window-capture.f - the capture seam of test/native-window-owner.f's
\ tier-1 window, loaded last. It owns the roster of the fixtures whose checks
\ run here: it requires each one below, so each compiles against the fresh
\ checker before any preparation, and the family readers and the compiler
\ adapter assert the by-name regime at their load. A fixture dropped from the
\ list fails the window at its block.
\
\ The order is each check's precondition:
\   - one adapter capture ends the by-name regime, and the adapter and family
\     captured halves run against the owner-record dispatch it selects, before
\     any checker preparation, as each did alone;
\   - payload validation takes the window's first checker preparations. Its RUN
\     refuses a checker whose signature store a preparation already moved into
\     the data span, or one with a boundary marked, so these compact the
\     no-return rows without a boundary: the NORET-COMPACT branch the build's
\     own capture never takes;
\   - the prefix-boundary rollback compiles and runs next, and its MARK stays
\     taken;
\   - tape-detach takes the later preparations, which compact against that
\     mark as the build's capture does against src/core/lower-cert-seal.f's.
\     Each follows its own OBSERVE or a checked detach. Nothing prepares after
\     it in this window.
\
\ Each check runs inside its own package, as it did at its fixture's load: the
\ checker resolves some names it is asked about in the current search order
\ (NFAM:CON-FAM of the family `payload`, a scan that calls PAYLOAD-NUMERIC),
\ and only their own package finds them.

require test/native-window-owner-family.f
require test/native-window-owner-adapter.f
require test/native-window-owner-payload.f
require test/native-window-tape-detach.f

CHECKER-OWNER:CAPTURE-PREPARE

package OWNER-ADAPTER-CHECK
CAPTURED
;package

package OWNER-FAMILY-CHECK
CAPTURED
;package

package OWNER-PAYLOAD-CHECK
RUN
;package

require test/compiler/native-prefix-rollback.f

package CHECKER-TAPE
RUN
;package

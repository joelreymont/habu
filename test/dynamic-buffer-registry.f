\ dynamic-buffer-registry.f - no image carries a dynamic-storage mapping.
\
\ WHAT THIS LOCKS, and it is an image invariant read from inside the image. A
\ DYNAMIC-BUFFER's control record is two DATA cells, a mapping pointer and a byte
\ capacity (src/core/dynamic-storage.f), and the AOT capture copies the window's
\ DATA. A record that still held a mapping when the capture ran would bake a
\ pointer into a process that has exited, beside a capacity that is real: RESERVE's
\ early return on `need <= old` never replaces it, the generated reader's bounds
\ check passes because the capacity is real, and the first access dereferences it.
\ Measured on an engine built with one reserve after the window's load: rc 134,
\ SIGSEGV, x11 holding the building process's mmap address.
\
\ THE FIRST TWO CASES ARE THE IMAGE, AND THEY READ IT BEFORE THIS FILE DECLARES
\ ANYTHING. Every DYNAMIC-BUFFER declaration registers its control record
\ (src/core/layout-buffer.f DBUF-SOURCE emits the REGISTER call), and the capture
\ walks that registry, releases every mapping, zeroes both cells of every record
\ and gives the registry itself back last (src/habu/aot-capture.f
\ ACAP-RELEASE-DYNAMIC). So a booted engine has registered nothing yet. On an engine
\ built without that walk the same two lines read the ~140 records the window
\ registered, several of them still holding a mapping - which is how this file is
\ red first rather than by construction.
\
\ THE REST IS THE REGISTRY ITSELF, in this process: a declaration registers, a
\ reserve makes the record dirty, a release leaves it registered and clean. The
\ count is what the capture's walk asserts on, so a test that never saw it move
\ would not be testing the thing the build depends on.
\
\ Standalone:
\   bin/hb --load lib/test.f test/dynamic-buffer-registry.f
require lib/test.f

package DYNAMIC-BUFFER-REGISTRY-TEST
private

variable BOOT-REGISTERED
variable BOOT-DIRTY

\ Read first, before the declaration below can register anything of its own.
DYNAMIC-STORAGE:REGISTERED-N BOOT-REGISTERED !
DYNAMIC-STORAGE:DIRTY-N      BOOT-DIRTY !

DYNAMIC-BUFFER SLOTS n

: RUN ( -- )
   T-RESET
   \ The image: an empty registry, and nothing in it holding a mapping.
   BOOT-REGISTERED @ 0 T=
   BOOT-DIRTY @ 0 T=
   \ This process: the declaration above registered exactly one record, and it is
   \ clean until something reserves it.
   DYNAMIC-STORAGE:REGISTERED-N 1 T=
   DYNAMIC-STORAGE:DIRTY-N 0 T=
   1 SLOTS-RESERVE
   7 0 SLOTS !
   0 SLOTS @ 7 T=
   DYNAMIC-STORAGE:DIRTY-N 1 T=
   \ A release leaves the record registered and zero, which is the state the
   \ capture's walk leaves every record in.
   SLOTS-RELEASE
   DYNAMIC-STORAGE:REGISTERED-N 1 T=
   DYNAMIC-STORAGE:DIRTY-N 0 T=
   \ Reserving again re-acquires a mapping through the same record, so a released
   \ record is reusable rather than retired.
   1 SLOTS-RESERVE
   9 0 SLOTS !
   0 SLOTS @ 9 T=
   DYNAMIC-STORAGE:DIRTY-N 1 T=
   SLOTS-RELEASE
   DYNAMIC-STORAGE:DIRTY-N 0 T=
   T-REPORT ;

RUN
;package

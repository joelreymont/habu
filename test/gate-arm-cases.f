\ These tests need the ARM host or its source recovery.

\ shadow-kinds.f reads a native arm64 shadow beside the host's tier-0 routines.
SUITE compiler-shadow
   test/compiler/shadow-kinds.f
;SUITE

\ arm64-place.f writes stripped programs from one build and runs them, checks
\ that each map names only the words its entry reaches and that a second link
\ gives the same bytes, and refuses a reached word a stripped program cannot run.
SUITE arm64-place
   test/compiler/arm64-place.f
;SUITE

\ arm64-link-data.f writes two stripped programs from one build and runs them,
\ checking their DATA cells and DP ceiling, and runs the link's refusals.
SUITE arm64-link-data
   test/compiler/arm64-link-data.f
;SUITE

\ arm64-link-run.f writes four stripped programs from one build and runs them.
SUITE arm64-link-run
   test/compiler/arm64-link-run.f
;SUITE

\ file-objects.f builds three files into a program, runs it and counts the
\ files the build compiled; LINKER:WRITE writes arm64 programs.
SUITE file-objects
   test/file-objects.f
;SUITE

SUITE engine-stack-debugger
   test/engine-stack-debugger.f
;SUITE

SUITE debugger-resume
   test/debugger-resume.f
;SUITE

SUITE prop
   test/prop-test.f
;SUITE

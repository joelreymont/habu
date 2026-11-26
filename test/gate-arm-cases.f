\ These tests need the ARM host or its source recovery.

\ shadow-kinds.f reads a native arm64 shadow beside the host's tier-0 routines.
TEST:SUITE compiler-shadow
   test/compiler/shadow-kinds.f
TEST:;SUITE

\ arm64-place.f writes stripped programs from one build and runs them, checks
\ that each map names only the words its entry reaches and that a second link
\ gives the same bytes, and rejects a reached word a stripped program cannot run.
TEST:SUITE arm64-place
   test/compiler/arm64-place.f
TEST:;SUITE

\ arm64-link-data.f writes two stripped programs from one build and runs them,
\ checking their DATA cells and DP ceiling, and runs the link's rejections.
TEST:SUITE arm64-link-data
   test/compiler/arm64-link-data.f
TEST:;SUITE

\ arm64-link-run.f writes four stripped programs from one build and runs them.
TEST:SUITE arm64-link-run
   test/compiler/arm64-link-run.f
TEST:;SUITE

\ file-objects.f builds three files into a program, runs it and counts the
\ files the build compiled; LINKER:WRITE writes arm64 programs. warm-engine.f
\ builds an engine warm after an edit and compares it with a cold build's, and
\ sees a primitive row whose cells differ from hb's rejected.
TEST:SUITE file-objects
   test/file-objects.f
   test/warm-engine.f
TEST:;SUITE

TEST:SUITE prop
   test/prop-test.f
TEST:;SUITE

\ t0-child.f - T0 installed as the runtime compiler for a child process: the
\ file loaded after this one compiles through T0. test/t0-install.f runs a
\ child this way for a row whose behavior exits the process.

require src/habu/t0-arm64.f
require src/habu/t0.f

CTARGET-ARCH:AARCH64 T0-EMIT:SELECT
T0:INSTALL
1 set-tier

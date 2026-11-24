\ sent-release-main.f - its top level captures a rejection and releases it, so
\ the record reaches stderr and fails the build.

REJECTION:CAPTURE
s" E-SENT-RELEASE" REJECTION:OPEN  s" released" REJECTION:EMIT-STR  REJECTION:KEEP
REJECTION:RELEASE

: MAIN ( -- ) ;

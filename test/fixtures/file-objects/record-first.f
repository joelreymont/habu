\ record-first.f - the record program's first file: its top level prints a warning
\ record through REJECTION, so the build maps the text the record is built in, and
\ drops a captured rejection, which never reaches stderr and so leaves the build linking.

package FILE-OBJECTS-RECORD-FIRST
public
s" E-FIRST" REJECTION:WARN  s" built" REJECTION:EMIT-STR  REJECTION:KEEP
REJECTION:CAPTURE
s" E-DROPPED" REJECTION:OPEN  s" dropped" REJECTION:EMIT-STR  REJECTION:KEEP
REJECTION:BUFFER-OFF
;package

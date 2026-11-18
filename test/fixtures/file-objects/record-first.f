split-str
\ record-first.f - the record program's first file: its top level prints a
\ record through REJECTION, so the build maps the text the record is built in.

package FILE-OBJECTS-RECORD-FIRST
public
s" E-FIRST" REJECTION:OPEN  s" built" REJECTION:EMIT-STR  REJECTION:KEEP
;package

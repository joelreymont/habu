\ An immediate for test/snapshot-writer.f that saves the application image to
\ the first script argument when it runs. Declared a stack-neutral parsing
\ immediate, it may be named in a checked body, so it captures while that
\ definition is still being compiled.

require src/habu/app-image.f
require src/os/script-argv.f

package SNAP-WRITER-ACTIVE
public

: SAVE ( -- ) 0 SCRIPT-ARGV$ APP-IMAGE:SAVE ; immediate
s" SNAP-WRITER-ACTIVE:SAVE" 0 parse-imm

;package

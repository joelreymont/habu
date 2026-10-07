\ checker-completion-dep.f - the dependency test/checker-completion.f's
\ composed subject requires. It declares a mixed-case word and no body, so
\ the subject's body is the first one checked. The loader releases this
\ file's bytes when its scan ends, and the word must keep the spelling it was
\ declared with after that.
17 constant CmpDep

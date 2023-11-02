((haskell-mode
  . (
     (haskell-process-args-stack-ghci . ("--ghci-options=-ferror-spans" "--no-build" "--no-load"
                                         "myers-diff:lib"
                                         "myers-diff:test:myers-diff-test"
                                         ))
     )))

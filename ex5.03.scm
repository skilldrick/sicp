(controller
  test-g
  (test (op ge?) (reg g) (reg x))
  (branch (label sqrt-done))
  (assign g (op imp) (reg g))
  (goto (label test-g))
  sqrt-done)

(controller
  test-g
  (test (op <) ((op abs) ((op -) ((op sq) (reg g)) (reg x))))
  (branch (label sqrt-done))
  (assign g (op avg) (reg g) ((op /) (reg g) (reg x)))
  (goto (label test-g))
  sqrt-done)

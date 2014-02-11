(load "s.l")
(load "test.l")
(setq *print-case* :downcase)
(defmacro pp (x) (pprint (symbol-function x)))

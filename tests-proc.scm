(add-tests-with-string-output "closure"
  [(let ((u 3))
     (app (lambda (v)
            (+ v u)) u)) => "6\n"]
  [(let* ((u 3)
          (f (lambda (v)
              (+ v u))))
     (f 3)) => "6\n"]
  [((lambda (v)
      (app
        (lambda (x)
          (+ x v))
        v)) 3) => "6\n"]
  )

#|(add-tests-with-string-output "non-tail-call"
  [(+ 2 ((lambda (v) v) 3)) => "5\n"]
  [(let ((f (lambda (v) v)))
     (+ (f 3) 2)) => "5\n"]
  )|#

(add-tests-with-string-output "tail-call"
  [(let ((f (lambda (v) v)))
     (f 3)) => "3\n"]
  [(let* ((f (lambda (v) v))
          (g (lambda (v)
               (f v))))
     (g 3)) => "3\n"])

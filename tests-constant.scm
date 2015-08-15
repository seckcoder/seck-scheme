(add-tests-with-string-output "string"
  ;[(cons #\a #\b) => "(#\\a . #\\b)\n"]
  #|[(let ([s (string #\a #\b)])
     (cons (string-ref s 0)
           (string-ref s 1))) => "(#\\a . #\\b)\n"]|#
  #|[(let ([s (make-string 2)])
     [>(string-set! s 0 #\a)
     (string-set! s 1 #\b)<]
     (cons #\a #\b)
     ) => "(#\\a . #\\b)\n"]|#
  [(let ([s (make-string 1)])
     (string-set! s 0 #\")
     s) => "\"\\\"\"\n"]
  )

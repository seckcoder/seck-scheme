(add-tests-with-string-output "print"
  [(print 3) => "3\n0\n"]
  [(print (cons 1 2)) => "(1 . 2)\n0\n"]
  )

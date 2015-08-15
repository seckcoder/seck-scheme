(add-tests-with-string-output "divide"
  [(quotient 100 3) => "33\n"]
  [(remainder 100 3) => "1\n"])

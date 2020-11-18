(defun do-all()
  (ql:quickload :jpeg-turbo/tests)
  (uiop:quit
   (if (uiop:call-function "jpeg-turbo-tests:run-tests")
       0 1)))

(do-all)

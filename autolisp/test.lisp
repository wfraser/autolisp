(flet ((testfun ()
    "inside testfun"))

    (cl-who:with-html-output-to-string (out nil :prologue t)
        (:html
            (:head
                (:title "AutoLisp Test")
                (:style :type "text/css"
                    "body {"
                        "font-family: \"Palatino Linotype\";"
                        "font-size: 12pt;"
                    "}"
                    "h1 {"
                        "margin-top: 8px;"
                    "}"))
            (:body
                (:h1 "AutoLisp Test")
                (:p "This is "
                    (:tt (format out "~A~A" (autolisp:getcwd) (subseq (hunchentoot:script-name*) 1))))
                (:p (format out "Today is ~A" 
                    (read-line 
                        (sb-ext:process-output 
                            (sb-ext:run-program "/bin/date" '("+%A, %B %e, %l:%M:%S %p %Z") :output :stream)))))
                (:p (format out "One more test: ~A" (testfun)))))))

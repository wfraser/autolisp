(cl-who:with-html-output-to-string (out nil :prologue t)
    (:html
        (:head
            (:title "AutoLisp")
            (:style :type "text/css"
                "body {"
                    "font-family: \"Palatino Linotype\";"
                    "font-size: 12pt;"
                "}"
                "h1 {"
                    "margin-top: 8px;"
               "}"))
        (:body
            (:h1 "AutoLisp")
            (:p "AutoLisp works with Hunchentoot to serve up Lisp files in any directory configuration you can come up with."
                " It removes the need to restart Hunchentoot for a change in one of your Lisp files.")
            (:p "A test file can be found here: "
                (:a :href "autolisp/test.lisp"
                    (:tt "autolisp/test.lisp"))
                " and its "
                (:a :href "autolisp/test.lisp.source"
                    (:i "source")))
            (:p "The source for AutoLisp can be found "
                (:a :href "https://github.com/wfraser/autolisp" "on GitHub")))))

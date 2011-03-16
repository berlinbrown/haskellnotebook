;;; ASDF - simple code gen

(defsystem simple-codegen
    :name "simple-codegen"
    :author "Berlin Brown"
    :licence "MIT"
    :description "Simple Code Gen Library"
    :depends-on ()
    :components ((:file "package")
		 (:file "utilities"
			:depends-on ("package"))
		 (:file "accesslog-database"
			:depends-on ("package" "utilities"))
		 (:file "simple-codegen"
			:depends-on ("package"))
		 (:file "file-stats"
			:depends-on ("package"))
		 (:file "file-walk-parse"
			:depends-on ("package" "file-stats"))
		 (:file "accesslog-parser"
			:depends-on ("package"
				     "utilities"
				     "accesslog-database" 
				     "file-stats" 
				     "simple-codegen"))))
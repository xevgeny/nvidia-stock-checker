LISP ?= sbcl

build:
	$(LISP) --non-interactive \
		--load nvidia-stock-checker.asd \
		--eval '(ql:quickload :nvidia-stock-checker)' \
		--eval '(asdf:make :nvidia-stock-checker)' \
		--eval '(quit)'

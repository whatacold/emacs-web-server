EMACS = emacs
BATCH = $(EMACS) -batch -Q

compile : simple-httpd.elc simple-httpd-test.elc

simple-httpd.elc : simple-httpd.el
	$(BATCH) -f batch-byte-compile $^

simple-httpd-test.elc : simple-httpd-test.el
	$(BATCH) -L . -f batch-byte-compile $^

test : simple-httpd.elc simple-httpd-test.elc
	$(BATCH) -L . -l simple-httpd-test.el -f ert-run-tests-batch

clean :
	rm -f simple-httpd.elc simple-httpd-test.elc

.PHONY : compile test clean

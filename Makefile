PROJECT=dwim-coder-mode

HAS_PKG=(and package-alist (assoc (quote $(PROJECT)) package-alist))
RM_PKG=(package-delete (cadr (assoc (quote $(PROJECT)) package-alist)))
INIT_PKG=(require (quote package)) (package-initialize)


all:
	@echo "Run with 'test', 'install' or 'uninstall'"

# Uninstall first so that byte-compilation uses the latest source
install: uninstall
	emacs --batch --eval="(package-install-file \"$(CURDIR)\")"
uninstall:
	emacs --batch --eval="(progn $(INIT_PKG) (if $(HAS_PKG) $(RM_PKG)))"
test:
	emacs -batch -f package-initialize -L . -f buttercup-run-discover

.PHONY: coverage
coverage:
	cask install
	UNDERCOVER_FORCE=true cask exec buttercup -L . -L tests
	genhtml --precision 2 ./coverage/lcov.info -o coverage

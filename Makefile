.PHONY: build
build:
	@stack build

.PHONY: test
test:
	@stack test

.PHONY: watch-test
watch-test:
	@stack test --file-watch

.PHONY: clean
clean:
	@${RM} -r .stack-work/
	@${RM} -r tests/
	@${RM} -r resources/.stack-work/
	@${RM} -r solutions/.stack-work/
	@${RM} -r resources/docs/
	@${RM} -r solutions/docs/
	@${RM} resources/benchmark*.html
	@${RM} solutions/benchmark*.html

.PHONY: test-%
test-%:
	@stack test --ta '--match "/$*/"'

.PHONY: coverage
coverage:
	@stack test --coverage
	@stack hpc report --all --destdir coverage

.PHONY: watch-coverage
watch-coverage:
	@stack test --coverage --file-watch

coverage-%:
	@stack test --coverage --no-strip cis1940-$*
	@stack hpc report cis1940-$* --destdir coverage

browse-coverage: coverage
	@open -b com.apple.Safari coverage/hpc_index.html

.PHONY: docs
docs:
	@stack haddock --no-haddock-deps --haddock-arguments '-o docs'

.PHONY: redocs
redocs:
	@stack haddock --reconfigure
	@${MAKE} docs

browse-docs: docs
	@open -b com.apple.Safari resources/docs/index.html
	@open -b com.apple.Safari solutions/docs/index.html

benchmark:
	@stack bench --ba '--output benchmark.html'

browse-benchmark:
	@open -b com.apple.Safari resources/benchmark.html 2> /dev/null || true
	@open -b com.apple.Safari solutions/benchmark.html 2> /dev/null || true

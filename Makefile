PACKAGES = \
	github.com/varnish/vclr/lexer \
	github.com/varnish/vclr/ast  \
        github.com/varnish/vclr/parser \
	github.com/varnish/vclr/token

test:
	@for pkg in ${PACKAGES}; do \
		echo ; \
		echo "*** Test: $$pkg ***" ; \
		echo ; \
		go test -cover -coverprofile=coverage.out $$pkg || exit 1 ; \
		echo ; \
		go tool cover -func=coverage.out ; \
		echo ; \
		go test -v $$pkg ; \
	done

check: test


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

build:
	mkdir -p bin/
	GOOS=linux GOARCH=amd64 go build -o bin/vclr -ldflags "-s -w -X main.revision=${REVISION} -X main.version=${VERSION}" 

osxbuild:
	mkdir -p bin/
	GOOS=darwin GOARCH=amd64 go build -o bin/vclr -ldflags "-s -w -X main.revision=${REVISION} -X main.version=${VERSION}"

clean:
	rm -f coverage.out
	rm -f coverage.html
	rm -f bin/vclr

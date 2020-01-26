# Intro

VCLR(eader) is meant to be a vcl parser.
It parses a vcl file in a single read-through and checks for various logic that should be available for each specific token.

Currently, as it is still WIP it will adhere to [Varnish](https://varnish-cache.org/docs/trunk/reference/vcl.html) 6 specs only.

It follows the [Pratt](https://tdop.github.io/) parser guidelines.

## Build

```sh
$ make build
```

## Run tests

```sh
$ make test
```

## Usage 

Start the parser with any of the following command line args:

| Option        | About                                             | Default       | Required       |
| ------------- |:--------------------------------------------------|:-------------:|:--------------:|
|*f*            | Parse the specified file                          | N/A           |
|*p*            | Parse recursivelly the specified folder and look for .vcl files   | N/A |
|*V*            | Display current version and exit.                 |               |                |


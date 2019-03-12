package main

import (
	"errors"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"strings"

	"github.com/varnish/vclr/lexer"
	"github.com/varnish/vclr/parser"
)

var (
	commandLine = flag.NewFlagSet(os.Args[0], flag.ExitOnError)
	versionFlag = commandLine.Bool("V", false, "Show version and exit")
	vclFile     = commandLine.String("f", "", "Parse the specified vcl.")
	vclFolder   = commandLine.String("p", "", "Parse vcl files in specified folder. ")

	version  = "N/A"
	revision = "N/A"
)

func parseVcl(vclFile string) error {

	fileContent, err := ioutil.ReadFile(vclFile)

	if err != nil {
		return err
	}

	vclFileContent := string(fileContent)

	l := lexer.New(vclFileContent)
	p := parser.New(l)
	program := p.ParseProgram()

	if len(p.Errors()) > 0 {
		return errors.New(strings.Join(p.Errors(), "\n"))
	}

	fmt.Println(program.String())

	return nil
}

func main() {

	commandLine.Usage = func() {
		fmt.Fprint(os.Stdout, "Usage of the VCL parser:\n")
		commandLine.PrintDefaults()
		os.Exit(0)
	}

	if err := commandLine.Parse(os.Args[1:]); err != nil {
		log.Fatalln(err)
	}

	if *versionFlag {
		fmt.Println("Version:  " + version)
		fmt.Println("Revision: " + revision)
		os.Exit(0)
	}

	err := parseVcl(*vclFile)

	if err != nil {
		fmt.Println(err)
	}
}

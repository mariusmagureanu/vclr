package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
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

	_, err = p.ParseProgram()

	return err
}

func parseFolder(vclFolder string) error {
	err := filepath.Walk(vclFolder,
		func(path string, info os.FileInfo, err error) error {
			if err != nil {
				return err
			}

			if strings.ToLower(filepath.Ext(path)) != ".vcl" {
				return nil
			}

			return parseVcl(path)
		})
	return err
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

	if *vclFile == "" && *vclFolder == "" {
		fmt.Println("The parser needs either -f or -p to run. See help (-h)")
		os.Exit(1)
	}

	if *vclFile != "" {
		err := parseVcl(*vclFile)

		if err != nil {
			fmt.Println(err)
		}
	}

	if *vclFolder != "" {
		err := parseFolder(*vclFolder)

		if err != nil {
			fmt.Println(err)
		}
	}
}

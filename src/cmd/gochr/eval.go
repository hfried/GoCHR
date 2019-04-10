package main

import (
	"flag"
	"fmt"
	"log"
	"os"

	chr "github.com/hfried/GoCHR/src/engine/CHR"
	trace "github.com/hfried/GoCHR/src/engine/terms"
	// "github.com/hfried/GoCHR/src/engine/parser"
)

const helpEval = `
usage: gochr eval [-o output-file] [input-file]

Evaluates Constraint Handling Rules and prints the relult.

If no input-file is specified, input is read from stdin. 

The -o flag specifies the output file name. If the -o flag is not used, 
output goes to stdout.
`

func contains(l []string, s1 string) bool {
	for _, s2 := range l {
		if s1 == s2 {
			return true
		}
	}
	return false
}

// ###
func evalCmd() {
	eval := flag.NewFlagSet("eval", flag.ContinueOnError)
	// fromFlag := eval.String("f", "yaml", "the format of the source file")
	// toFlag := eval.String("t", "graphml", "the format of the output file")
	outFileFlag := eval.String("o", "", "the filename of the output file")

	var inFile *os.File
	var outFile *os.File
	var err error

	if err := eval.Parse(os.Args[2:]); err != nil {
		log.Fatal(err)
	}

	switch eval.NArg() {
	case 0:
		inFile = os.Stdin
	case 1:
		inFile, err = os.Open(eval.Args()[0])
		if err != nil {
			log.Fatal(err)
		}
	default:
		log.Fatal(fmt.Errorf("incorrect number of arguments after the command flags; should be 0, to read from stdin, or 1, naming the input file\n"))
		return
	}
	if *outFileFlag == "" {
		outFile = os.Stdout
	} else {
		outFile, err = os.Create(*outFileFlag)
		if err != nil {
			log.Fatal(fmt.Errorf("%s\n", err))
			return
		}
	}
	rs := chr.MakeRuleStore()
	ok := rs.ParseFileCHRRulesGoals(inFile)
	if !ok {
		log.Fatal(fmt.Errorf("%s\n", err))
	}
	trace.CHRtrace = 0
	chr.CHRsolver(rs, 1000000)

	trace.CHRtrace = 1
	chr.WriteCHRStore(rs, outFile)

}

// Copyright © 2015 The Carneades Authors
// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL
// was not distributed with this file, You can obtain one
// at http://mozilla.org/MPL/2.0/.

package main

import (
	"fmt"
	"os"
)

const help = `
GoCHR is a tool for evaluating of Constraint Handling Rules. 

Usage: gochr command [arguments]

The commands are:

eval - evaluate Constraint Handling Rules
help - displays instructions

Execute "gochr help [command]" for further information.
`
const (
	Name    = "GoCHR"
	Version = "2.00"
)

func main() {
	if len(os.Args) == 1 {
		fmt.Printf("%s\nversion: %s\n'gochr ?' for help\n", Name, Version)
	} else {
		switch os.Args[1] {
		case "eval":
			evalCmd()
		default:
			if len(os.Args) == 2 {
				fmt.Printf("%s\n", help)
			} else {
				switch os.Args[2] {
				case "eval":
					fmt.Printf("%s\n", helpEval)
				default:
					fmt.Printf("%s\n", help)
				}
			}
		}
	}
}

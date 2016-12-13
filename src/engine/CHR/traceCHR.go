// Copyright © 2016 The Carneades Authors
// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL
// was not distributed with this file, You can obtain one
// at http://mozilla.org/MPL/2.0/.

// Constraint Handling Rules

package chr

import (
	"fmt"

	. "github.com/hfried/GoCHR/src/engine/terms"
	// "math/big"
	// "strconv"
	// "strings"
)

var CHRtrace int

// ---------------
// trace functions
// ---------------

func TraceHeadln(l, n int, s ...interface{}) {
	if CHRtrace >= l {
		for i := 0; i < n; i++ {
			fmt.Printf("      ")
		}
		fmt.Printf("*** ")
		for _, s1 := range s {
			fmt.Printf("%v", s1)
		}
		fmt.Printf("\n")
	}
}

func TraceHead(l, n int, s ...interface{}) {
	if CHRtrace >= l {
		for i := 0; i < n; i++ {
			fmt.Printf("      ")
		}
		fmt.Printf("*** ")
		for _, s1 := range s {
			fmt.Printf("%v", s1)
		}
	}
}

func Trace(l int, s ...interface{}) {
	if CHRtrace >= l {
		for _, s1 := range s {
			fmt.Printf("%v", s1)
		}
	}
}

func Traceln(l int, s ...interface{}) {
	if CHRtrace >= l {
		for _, s1 := range s {
			fmt.Printf("%v", s1)
		}
		fmt.Printf("\n")
	}
}

func TraceEnv(l int, e Bindings) {
	if e == nil {
		Trace(l, "nil")
	} else {
		if e.Var.Name == "" {
			Trace(l, "[\"\"=nil]")
		} else {
			if e.Next == nil || e.Next.Var.Name == "" {
				Trace(l, "[", e.Var.Name, "=", e.T.String(), ", nil]")
			} else {
				Trace(l, "[", e.Var.Name, "=", e.T.String(), ",...]")
			}
		}

	}
}

func TraceEMap(l int, n int, h *Compound, eMap *EnvMap) {
	if CHRtrace >= l {
		for i := 0; i < n; i++ {
			fmt.Printf("      ")
		}
		fmt.Printf("*** head: %s inBind: ", h.String())
		TraceEnv(l, eMap.inBinding)
		fmt.Printf(" outBind:[")
		env := eMap.outBindings
		for i, e := range env {
			fmt.Printf("[ %d ] =", i)
			// for _, e1 := range e.inBinding {
			if e == nil {
				fmt.Printf("NIL")
			} else {
				TraceEnv(l, e.inBinding)
			}

			// }
			fmt.Printf(" | ")
		}
		fmt.Printf("]\n")
	}
}

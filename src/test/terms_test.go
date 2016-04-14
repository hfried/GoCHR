// Copyright © 2016 The Carneades Authors
// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL
// was not distributed with this file, You can obtain one
// at http://mozilla.org/MPL/2.0/.

package test

import (
	"fmt"
	. "github.com/hfried/GoCHR/src/engine/parser"
	. "github.com/hfried/GoCHR/src/engine/terms"
	"testing"
)

func TestMatch01(t *testing.T) {
	//	checkErr := func(e error) {
	//		if e != nil {
	//			t.Errorf(e.Error())
	//		}
	//	}
	t1 := Atom("joe")
	t2 := Atom("sally")
	t3 := Compound{Functor: "parent", Args: []Term{t1, t2}}
	t4 := Compound{Functor: "parent", Args: []Term{Variable{Name: "X"}, Variable{Name: "Y"}}}
	_, ok := Match(t4, t3, nil)
	if ok == false {
		t.Errorf("TestMatch1 failed\n")
	}
}

func TestMatch02(t *testing.T) {
	// check that a variable is not bound to two different terms
	t3 := Compound{Functor: "parent", Args: []Term{Atom("joe"), Atom("sally")}}
	t4 := Compound{Functor: "parent", Args: []Term{Variable{Name: "X"}, Variable{Name: "X"}}}
	_, ok := Match(t4, t3, nil)
	if ok == true {
		t.Errorf("TestMatch2 failed\n")
	}
}

func TestEqual(t *testing.T) {
	t1 := Compound{Functor: "parent", Args: []Term{Atom("joe"), Atom("sally")}}
	t2 := Compound{Functor: "parent", Args: []Term{Atom("joe"), Atom("sally")}}
	if !Equal(t1, t2) {
		t.Errorf("TestEqual failed\n")
	}
}

func TestSubstitute(t *testing.T) {
	t1 := Compound{Functor: "parent", Args: []Term{Variable{Name: "X"}, Variable{Name: "Y"}}}
	t2 := Compound{Functor: "parent", Args: []Term{Atom("joe"), Atom("sally")}}
	env := AddBinding(Variable{Name: "Y"}, Atom("sally"), nil)
	env = AddBinding(Variable{Name: "X"}, Atom("joe"), env)
	t5 := Substitute(t1, env)
	if !Equal(t2, t5) {
		t.Errorf("TestSubstitute failed\n")
	}
}

func TestVariableChain1(t *testing.T) {
	t1 := Compound{Functor: "person", Args: []Term{Variable{Name: "X"}}}
	t2 := Compound{Functor: "person", Args: []Term{Atom("joe")}}
	env := AddBinding(Variable{Name: "Y"}, Atom("joe"), nil)
	env = AddBinding(Variable{Name: "X"}, Variable{Name: "Y"}, env)
	t5 := Substitute(t1, env)
	if !Equal(t2, t5) {
		t.Errorf("TestVariableChain1 failed\n")
	}
}

func TestVariableChain2(t *testing.T) {
	t1 := Compound{Functor: "person", Args: []Term{NewVariable("X")}}
	t2 := Compound{Functor: "person", Args: []Term{NewVariable("Y")}}
	env := AddBinding(NewVariable("X"), NewVariable("Y"), nil)
	t5 := Substitute(t1, env)
	if !Equal(t2, t5) {
		t.Errorf("TestVariableChain2 failed\n")
	}
}

func tmatch(t *testing.T, str1, str2 string) bool {

	term1, ok := ReadString(str1)
	if !ok {
		t.Errorf(fmt.Sprintf("Scan str1 in match \"%s\" failed, term1: %s", str1, term1.String()))
		return false
	}

	term2, ok := ReadString(str2)
	if !ok {
		t.Errorf(fmt.Sprintf("Scan str2 in match \"%s\" failed, term2: %s", str2, term2.String()))
	}
	fmt.Printf("  Unitfy  %s  \n       mit  %s \n", term1.String(), term2.String())
	env, ok := Match(term1, term2, nil)
	fmt.Printf("---Binding---\n")
	for ; env != nil; env = env.Next {
		fmt.Printf(" %s == %s \n", env.Var, env.T)
	}
	return ok
}

func TestMatch1(t *testing.T) {
	ok := tmatch(t, "parent(X,Y)", "parent(joe, sally)")
	if !ok {
		t.Errorf("TestMatch1 failed\n")
	}
}

func TestMatch2(t *testing.T) {
	// check that a variable is not bound to two different terms
	//	t3 := Compound{Functor: "parent", Args: []Term{Atom("joe"), Atom("sally")}}
	//	t4 := Compound{Functor: "parent", Args: []Term{Variable{Name: "X"}, Variable{Name: "X"}}}
	//	_, ok := Match(t4, t3, nil)
	ok := tmatch(t, "parent(X,X)", "parent(joe, sally)")
	if ok {
		t.Errorf("TestMatch2 failed\n")
	}
}

func TestMatch3(t *testing.T) {
	ok := tmatch(t, "[color(X), color(Y), mix(X,Y,Z), color(Z)]",
		"[color(blue), color(yellow), mix(blue,yellow,green), color(green)]")
	if !ok {
		t.Errorf("TestMatch3 failed\n")
	}
}

func TestMatch3a(t *testing.T) {
	ok := tmatch(t, "[color(X), color(Y), mix(X,Y,Z), color(Z)]",
		"[color(blue), color(yellow), mix(blue,yellow,green), color(blue)]")
	if ok {
		t.Errorf("TestMatch3a failed\n")
	}
}

func TestMatch3b(t *testing.T) {
	ok := tmatch(t, "[color(X), color(Y), mix(X,Y,Z), color(Z)]",
		"[color(blue), color(yellow), mix(blue,blue,green), color(green)]")
	if ok {
		t.Errorf("TestMatch3b failed\n")
	}
}

func TestMatch3c(t *testing.T) {
	ok := tmatch(t, "[color(X), color(Y), mix(X,Y,Z), color(Z)]",
		"[color(green), color(yellow), mix(blue,yellow,green), color(green)]")
	if ok {
		t.Errorf("TestMatch3c failed\n")
	}
}

func TestMatch4(t *testing.T) {
	ok := tmatch(t, "[p(X, f(X)), f(A, Z), f(g(A,D), E)]",
		"[p(a, f(a)), f(Y, b), f(g(Y,b), h(c)) ]")
	if !ok {
		t.Errorf("TestMatch4 failed\n")
	}
}

func TestMatch4a(t *testing.T) {
	ok := tmatch(t, "[p(X, f(X)), f(A, Z), f(g(A,D), E)]",
		"[p(A, f(A)), f(Y, b), f(g(Y,B), h(C)) ]")
	if !ok {
		t.Errorf("TestMatch4a failed\n")
	}
}

func TestMatch5(t *testing.T) {
	ok := tmatch(t, "g(A,A)",
		"g(X,f(X))")
	if ok {
		t.Errorf("TestMatch5 failed\n")
	}
}

func TestMatch6(t *testing.T) {
	ok := tmatch(t, "g(A, A)",
		"g(X, p(E,f(X,a)))")
	if ok {
		t.Errorf("TestMatch6 failed\n")
	}
}

func TestMatch7(t *testing.T) {
	ok := tmatch(t, "p(X,X)",
		"p(Y,f(Y))")
	if ok {
		t.Errorf("TestMatch7 failed\n")
	}
}

func TestMatch7a(t *testing.T) {
	// !!!
	ok := tmatch(t, "p(X,X)",
		"p(Y,Z)")
	if ok {
		t.Errorf("TestMatch7a failed\n")
	}
}

func TestMatch7b(t *testing.T) {
	// !!!
	ok := tmatch(t, "p(X,Y)",
		"p(Z,Z)")
	if !ok {
		t.Errorf("TestMatch7b failed\n")
	}
}

func TestMatch8(t *testing.T) {
	ok := tmatch(t, "f(g(a,D),E, E)",
		"f(g(a,b),h(Y),Y)")
	if ok {
		t.Errorf("TestMatch8 failed\n")
	}
}

func TestMatch9(t *testing.T) {
	ok := tmatch(t, "f(D, g(a,D))",
		"f(X, X)")
	if ok {
		t.Errorf("TestMatch9 failed\n")
	}
}

func TestMatch10(t *testing.T) {
	// !!!
	ok := tmatch(t,
		"p(f(A,A),g(B,B),B)",
		"p(f(X,Y),g(Y,Z),X)")
	if ok {
		t.Errorf("TestMatch10 failed\n")
	}
}

func TestMatch11(t *testing.T) {
	ok := tmatch(t,
		"p(f(A,A),g(B,B),B)",
		"p(f(X,Y),g(Y,Z),h(X))")
	if ok {
		t.Errorf("TestMatch11 failed\n")
	}
}

func TestMatch12(t *testing.T) {
	ok := tmatch(t,
		"[A, B | C]",
		"[1, 2]")
	if !ok {
		t.Errorf("TestMatch12 failed\n")
	}
}

func TestMatch13(t *testing.T) {
	ok := tmatch(t,
		"[A, B | C]",
		"[1, 2, 3]")
	if !ok {
		t.Errorf("TestMatch13 failed\n")
	}
}

func TestMatch14(t *testing.T) {
	ok := tmatch(t,
		"[A, B | C]",
		"[1, 2, 3, 4, 5, 6]")
	if !ok {
		t.Errorf("TestMatch14 failed\n")
	}
}

func TestMatch15(t *testing.T) {
	ok := tmatch(t,
		"add(s(X),Y,Z)",
		"add(s(0),A,B)")
	if !ok {
		t.Errorf("TestMatch15 failed\n")
	}
}

func TestMatch16(t *testing.T) {
	ok := tmatch(t,
		"add(s(X),Y,Z)",
		"add(s(0),X,Y)")
	if !ok {
		t.Errorf("TestMatch16 failed\n")
	}
}

func TestMatch17(t *testing.T) {
	// !!!
	ok := tmatch(t,
		"p(X,X)",
		"p(s(A),s(22))")
	if ok {
		t.Errorf("TestMatch17 failed\n")
	}
}

func TestMatch18(t *testing.T) {
	ok := tmatch(t,
		"p(X,X)",
		"p(s(A),s(B))")
	if ok {
		t.Errorf("TestMatch18 failed\n")
	}
}

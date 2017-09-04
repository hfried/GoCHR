// Copyright © 2016 The Carneades Authors
// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL
// was not distributed with this file, You can obtain one
// at http://mozilla.org/MPL/2.0/.

package chr

import (
	//	"fmt"
	"strings"
	"testing"
	sc "text/scanner"

	. "github.com/hfried/GoCHR/src/engine/parser"
	. "github.com/hfried/GoCHR/src/engine/terms"
)

/*
func TestCHR01(t *testing.T) {
	ok := tAtt(t, "p(a, b), p(b, a), p(a,a), p(b, b)", "p(A,a)", "p(a, b), p(b, a), p(a,a), p(b, b)")
	if ok != true {
		t.Errorf("TestStore01 failed\n")
	}
}

func TestCHR02(t *testing.T) {
	ok := tAtt(t, "p(a, b), p(b, a), p(a,a), p(b, b)", "p(a,a)", "p(a, b), p(a,a)")
	if ok != true {
		t.Errorf("TestStore02 failed\n")
	}
}

func TestCHR03(t *testing.T) {
	ok := tAtt(t, "p(a, b), p(b, a), p(a,a), p(b, b)", "p(b,a)", "p(b, a), p(b, b)")
	if ok != true {
		t.Errorf("TestStore03 failed\n")
	}
}

func TestCHR04(t *testing.T) {
	ok := tAtt(t, "p(2.0,4.0),p(\"Hallo\", a),p(true, b),p(7,a),p(false, a),p(34, b),p(17.3,b),p(\"Welt\",b)",
		"p(true,a)", "p(true, b),p(false, a)")
	if ok != true {
		t.Errorf("TestStore04 failed\n")
	}
}

func TestCHR05(t *testing.T) {
	ok := tAtt(t, "p(2.0,4.0),p(\"Hallo\", a),p(true, b),p(7,a),p(false, a),p(34, b),p(17.3,b),p(\"Welt\",b)",
		"p(162,a)", "p(7,a),p(34, b)")
	if ok != true {
		t.Errorf("TestStore05 failed\n")
	}
}

func TestCHR06(t *testing.T) {
	ok := tAtt(t, "p(2.0,4.0),p(\"Hallo\", a),p(true, b),p(7,a),p(false, a),p(34, b),p(17.3,b),p(\"Welt\",b)",
		"p(736.6,a)", "p(2.0,4.0),p(17.3,b)")
	if ok != true {
		t.Errorf("TestStore06 failed\n")
	}
}

func TestCHR07(t *testing.T) {
	ok := tAtt(t, "p(2.0,4.0),p(\"Hallo\", a),p(true, b),p(7,a),p(false, a),p(34, b),p(17.3,b),p(\"Welt\",b)",
		"p(\"OK\",a)", "p(\"Hallo\", a),p(\"Welt\",b)")
	if ok != true {
		t.Errorf("TestStore07 failed\n")
	}
}

func TestCHR08(t *testing.T) {
	ok := tAtt(t, "p(q(2.0),4.0),p(q(\"Hallo\"), a),p(r(true), b),p(r(7),a),p(s(false), a),p(s(34), b),p(t(),b),p(t(),b)",
		"p(r(77),a)", "p(r(true), b),p(r(7),a)")
	if ok != true {
		t.Errorf("TestStore08 failed\n")
	}
}

func TestCHR09(t *testing.T) {
	ok := tAtt(t, "p(2.0,4.0),p(\"Hallo\", a),p(true, b),p(7,a),p(false, a),p(34, b),p(17.3,b),p(\"Welt\",b)",
		"p(B,a)", "p(2.0,4.0),p(\"Hallo\", a),p(true, b),p(7,a),p(false, a),p(34, b),p(17.3,b),p(\"Welt\",b)")
	if ok != true {
		t.Errorf("TestStore09 failed\n")
	}
}

func TestCHR10(t *testing.T) {
	ok := tAtt(t, "2.0+4.0, \"Hallo\"+a, true == b, 7 *a, false != a, 34>= b, 17.3 < b ,\"Welt\"< b",
		"A+4", "2.0+4.0, \"Hallo\"+a")
	if ok != true {
		t.Errorf("TestStore10 failed\n")
	}
}

func TestCHR11(t *testing.T) {
	ok := tAtt(t, "2.0+4.0, \"Hallo\"+a, true == b, 7 *a, false != a, 34>= b, 17.3 < b ,\"Welt\"< b",
		"3.0+A", "2.0+4.0")
	if ok != true {
		t.Errorf("TestStore11 failed\n")
	}
}

func TestCHR12(t *testing.T) {
	ok := tAtt(t, "2.0+4.0, \"Hallo\"+a, true == b, 7 *a, false != a, 34>= b, 17.3 < b ,\"Welt\"< b",
		"\"Welt\"+x", "\"Hallo\"+a")
	if ok != true {
		t.Errorf("TestStore12 failed\n")
	}
}

//func TestCHRnn(t *testing.T) {
//	ok := tAtt(t, "[p(2.0,4.0),p(\"Hallo\", a),p(true, b),p(7,a),p(false, a),p(34, b),p(17.3,b),p(\"Welt\",b)]",
//	"p(true,a)", "[p(2.0,4.0),p(\"Hallo\", a),p(true, b),p(7,a),p(false, a),p(34, b),p(17.3,b),p(\"Welt\",b)]")
//	if ok != true {
//		t.Errorf("TestStorenn failed\n")
//	}
//}

//func TestCHRxx(t *testing.T) {
//	ok := tAtt(t, "[p(a, b), p(b, a), q(a,a), p(b, b)]", "p(c,A)", "[]")
//	if ok != true {
//		t.Errorf("TestStore02 failed\n")
//	}
//}

func TestCHRRule00(t *testing.T) {
	CHRtrace = 0
	rs := MakeRuleStore()
	ok := rs.ParseStringCHRRulesGoals(`
	sum([], S) <=> S == 0 .
	sum([X|Xs], S) <=> sum(Xs, S2), S == X + S2.
	sum([1,2,3,4,5,6,7,8,9,10], S).
	#result: S == 55 .
//	sum([X,2,3], 6).
//	#result: X == 1 .
//	sum([1,X,3], 6).
//	#result: X == 2 .
	`)
	CHRtrace = 0
	if !ok {
		t.Error("TestCHRRule00 fails")
	}
}

func TestCHRRule01(t *testing.T) {
	CHRtrace = 0
	rs := MakeRuleStore()
	ok := rs.ParseStringCHRRulesGoals(`
	prime01 @ prime(N) ==> N>2 | prime(N-1).
	prime02 @ prime(A) | prime(B) <=> B > A, B mod A == 0 | true.
	//prime(100).
	// #result:  prime(97), prime(89), prime(83), prime(79), prime(73), prime(71), prime(67), prime(61), prime(59), prime(53), prime(47), prime(43), prime(41), prime(37), prime(31), prime(29), prime(23), prime(19), prime(17), prime(13), prime(11), prime(7), prime(5), prime(3), prime(2).
	prime(20).
	#result: prime(19), prime(17), prime(13), prime(11), prime(7), prime(5), prime(3), prime(2).
	`)
	if !ok {
		t.Error("TestCHRRule01 fails")
	}
}

func TestCHRRule02(t *testing.T) {
	CHRtrace = 0
	rs := MakeRuleStore()
	ok := rs.ParseStringCHRRulesGoals(`
	// first rule set with assignment
	// logarithmic complexity
	gcd01@ gcd(0) <=> true .
	gcd02@ gcd(N) \ gcd(M) <=> N <= M, L := M mod N | gcd(L).
	 gcd(94017), gcd(1155),gcd(2035).
	 #result == gcd(11).
	gcd(12),gcd(18).
	#result == gcd(6).
	gcd(3528),gcd(3780).
	#result == gcd(252).
	// second rule set without assignment
	// logarithmic complexity
	gcd01@ gcd(0) <=> true .
	gcd02@ gcd(N) \ gcd(M) <=> N <= M | gcd(M mod N).
	gcd(94017), gcd(1155),gcd(2035).
	#result == gcd(11).
	gcd(12),gcd(18).
	#result == gcd(6).
	gcd(3528),gcd(3780).
	#result == gcd(252).
	// third rule set
	// linear complexity
	gcd01@ gcd(0) <=> true .
	gcd02@ gcd(N) \ gcd(M) <=> 0<N, N=<M | gcd(M-N).
	gcd(94017), gcd(1155),gcd(2035).
	#result == gcd(11).
	gcd(12),gcd(18).
	#result == gcd(6).
	gcd(3528),gcd(3780).
	#result == gcd(252).
	// fourth rule set with assignment
	// linear complexity
	gcd01@ gcd(0) <=> true .
	gcd02@ gcd(N) \ gcd(M) <=> 0<N, N=<M, L := M - N | gcd(L).
	gcd(94017), gcd(1155),gcd(2035).
	#result == gcd(11).
	gcd(12),gcd(18).
	#result == gcd(6).
	gcd(3528),gcd(3780).
	#result == gcd(252).
	// fifth rule set with assingment in goal
	// linear complexity
	gcd01@ gcd(0) <=> true .
	gcd02@ gcd(N) \ gcd(M) <=> 0<N, N=<M | L := M - N, gcd(L).
	gcd(12), gcd(27).
	#result: gcd(3), L2:=15, L4:=3, L6:=9, L8:=6, L10:=3, L12:=0 .
	gcd(12),gcd(18).
	#result: gcd(6), L2:=6, L4:=6, L6:=0 .
	`)
	if !ok {
		t.Error("TestCHRRule02 fails")
	}
}

func TestCHRRule04(t *testing.T) {
	CHRtrace = 0
	rs := MakeRuleStore()
	ok := rs.ParseStringCHRRulesGoals(`
	fib01@ upto(A) ==> fib(0,1), fib(1,1).
	fib02@ upto(Max), fib(N1,M1), fib(N2,M2) ==> Max > N2, N2 == N1+1 | fib(N2+1,M1+M2).
	upto(10).
	#result: upto(10), fib(0,1), fib(1,1), fib(2,2), fib(3,3), fib(4,5), fib(5,8), fib(6,13), fib(7,21), fib(8,34), fib(9,55), fib(10,89).
    upto(20).
    #result: upto(20), fib(0,1), fib(1,1), fib(2,2), fib(3,3), fib(4,5), fib(5,8), fib(6,13), fib(7,21), fib(8,34), fib(9,55), fib(10,89), fib(11,144), fib(12,233), fib(13,377), fib(14,610), fib(15,987), fib(16,1597), fib(17,2584), fib(18,4181), fib(19,6765), fib(20,10946).
`)

	if !ok {
		t.Error("TestCHRRule04a fails")
	}

}

func TestCHRRule05(t *testing.T) {
	CHRtrace = 0
	rs := MakeRuleStore()
	ok := rs.ParseStringCHRRulesGoals(`
	leq_reflexivity  @ leq(X,X) <=> true.
	leq_antisymmetry @ leq(X,Y), leq(Y,X) <=> X==Y.
	leq_idempotence  @ leq(X,Y)\ leq(X,Y) <=> true.
	leq_transitivity @ leq(X,Y), leq(Y,Z) ==> leq(X,Z).
	leq(A,B), leq(B,C), leq(C,A).
	#result: A==C, B==C .
	`)
	if !ok {
		t.Error("TestCHRRule05a fails")
	}
}

func TestCHRRule06(t *testing.T) {
	CHRtrace = 0
	rs := MakeRuleStore()
	ok := rs.ParseStringCHRRulesGoals(`
	data1 @ data() ==> edge(berlin, 230, wolfsburg), edge(hannover, 89, wolfsburg), edge(hannover, 108, bielefeld), edge(bielefeld, 194, köln).
	data2 @ data() ==> edge(berlin,259, jena), edge(jena,55, erfurt), edge(erfurt,205,giessen), edge(giessen,158,köln), edge(köln, 85, aachen).
	data3 @ data() <=> true .
	source @ source(V) ==> dist(V, 0).
	del @ dist(V,D1) \ dist(V, D2) <=> D1 <= D2 | true.
	dist_plus1 @ dist(V,D1), edge(V, D2, V2) ==> dist(V2, D1+D2).
	dist_plus2 @ dist(V,D1), edge(V2, D2, V) ==> dist(V2, D1+D2).
	del_data @ edge(X,Y,Z) <=> true.
	data(), source(berlin).
	#result: source(berlin), dist(berlin,0), dist(wolfsburg,230), dist(jena,259), dist(erfurt,314), dist(giessen,519), dist(hannover,319), dist(bielefeld,427), dist(köln,621), dist(aachen,706) .
`)
	if !ok {
		t.Error("TestCHRRule06 fails")
	}
}

func TestCHRRule07(t *testing.T) {
	CHRtrace = 0
	rs := MakeRuleStore()
	ok := rs.ParseStringCHRRulesGoals(`

	data1 @ data() ==> edge(berlin, 230, wolfsburg), edge(hannover, 89, wolfsburg), edge(hannover, 108, bielefeld), edge(bielefeld, 194, köln).
	data2 @ data() ==> edge(berlin,259, jena), edge(jena,55, erfurt), edge(erfurt,205,giessen), edge(giessen,158,köln), edge(köln, 85, aachen).
    data3 @ data() <=> true .
	source @ source(V) ==> dist(V, [V], 0).
	del @ dist(V, L, D1) \ dist(V, M, D2) <=> D1 <= D2 | true.
	dist_plus1 @ dist(V, L, D1), edge(V, D2, V2) ==> dist(V2,[V2|L], D1+D2).
	dist_plus2 @ dist(V, L, D1), edge(V2, D2, V) ==> dist(V2,[V2|L], D1+D2).
	del_data @ edge(X,Y,Z) <=> true.
	data(), source(berlin).
	#result: source(berlin), dist(berlin,[berlin],0), dist(wolfsburg,[wolfsburg, berlin],230), dist(jena,[jena, berlin],259), dist(erfurt,[erfurt, jena, berlin],314), dist(giessen,[giessen, erfurt, jena, berlin],519), dist(hannover,[hannover, wolfsburg, berlin],319), dist(bielefeld,[bielefeld, hannover, wolfsburg, berlin],427), dist(köln,[köln, bielefeld, hannover, wolfsburg, berlin],621), dist(aachen,[aachen, köln, bielefeld, hannover, wolfsburg, berlin],706) .
`)
	if !ok {
		t.Error("TestCHRRule07 fails")
	}
}

func TestCHRRule08(t *testing.T) {
	CHRtrace = 0
	rs := MakeRuleStore()
	ok := rs.ParseStringCHRRulesGoals(`

	data1 @ data() ==> edge(berlin, 230, wolfsburg), edge(hannover, 89, wolfsburg), edge(hannover, 108, bielefeld), edge(bielefeld, 194, köln).
	data2 @ data() ==> edge(berlin,259, jena), edge(jena,55, erfurt), edge(erfurt,205,giessen), edge(giessen,158,köln), edge(köln, 85, aachen).
    data3 @ data() <=> true .
	source @ source(V) ==> dist([V], 0).
	del @ dist([V|L], D1) \ dist([V|M], D2) <=> D1 <= D2 | true.
	dist_plus_a@ dist([V|L], D1), edge(V, D2, V2) ==> dist([V2, V|L], D1+D2).
	dist_plus_b@ dist([V|L], D1), edge(V2, D2, V) ==> dist([V2, V|L], D1+D2).
	del_data @ edge(X,Y,Z) <=> true.
	data(), source(berlin).
	#result: source(berlin), dist([berlin],0), dist([wolfsburg, berlin],230), dist([jena, berlin],259), dist([erfurt, jena, berlin],314), dist([giessen, erfurt, jena, berlin],519), dist([hannover, wolfsburg, berlin],319), dist([bielefeld, hannover, wolfsburg, berlin],427), dist([köln, bielefeld, hannover, wolfsburg, berlin],621), dist([aachen, köln, bielefeld, hannover, wolfsburg, berlin],706).
`)
	if !ok {
		t.Error("TestCHRRule08 fails")
	}
}

func TestCHRRule09(t *testing.T) {
	CHRtrace = 0
	rs := MakeRuleStore()
	ok := rs.ParseStringCHRRulesGoals(`

	data1 @ data() ==> edge(berlin, 230, wolfsburg), edge(hannover, 89, wolfsburg), edge(hannover, 108, bielefeld), edge(bielefeld, 194, köln).
	data2 @ data() ==> edge(berlin,259, jena), edge(jena,55, erfurt), edge(erfurt,205,giessen), edge(giessen,158,köln), edge(köln, 85, aachen).
    data3 @ data() <=> true .
	source @ source(V) ==> dist([V], 0).
	del @ dist([V|L], D1) \ dist([V|M], D2) <=> D1 <= D2 | true.
	dist_plus_a@ dist([V|L], D1), edge(V, D2, V2) ==> dist([V2, V|L], D1+D2).
	dist_plus_b@ dist([V|L], D1), edge(V2, D2, V) ==> dist([V2, V|L], D1+D2).
	del_data @ edge(X,Y,Z) <=> true.
	data(), source(berlin).
	#result: source(berlin), dist([berlin],0), dist([wolfsburg, berlin],230), dist([jena, berlin],259), dist([erfurt, jena, berlin],314), dist([giessen, erfurt, jena, berlin],519), dist([hannover, wolfsburg, berlin],319), dist([bielefeld, hannover, wolfsburg, berlin],427), dist([köln, bielefeld, hannover, wolfsburg, berlin],621), dist([aachen, köln, bielefeld, hannover, wolfsburg, berlin],706).
`)
	if !ok {
		t.Error("TestCHRRule09 fails")
	}
}

func TestCHRRule10(t *testing.T) {
	CHRtrace = 0
	rs := MakeRuleStore()
	ok := rs.ParseStringCHRRulesGoals(`
// first  rule set: change only the search-rule in orginal code
zero1 @ add(0,Y,Z) <=> Y == Z.
zero2 @ add(X,0,Z) <=> X == Z.
zero3 @ add(X,Y,0) <=> X == 0, Y == 0 .

same1 @ add(X,E,E) <=> X == 0 .
same2 @ add(E,Y,E) <=> Y == 0 .

succ1 @ add(s(X),Y,Z) <=> Z == s(W), add(X,Y,W).
succ2 @ add(X,s(Y),Z) <=> Z == s(W), add(X,Y,W).
succ3 @ add(X,X,s(Z)) <=> Z == s(W), X == s(Y), add(Y,Y,W).

// search @ add(X,Y,s(Z)) <=> true | add(X1,Y1,Z), (X = s(X1),Y = Y1 ; X = X1,Y = s(Y1)).

search @ add(X,Y,s(Z)) <=> add(X1,Y1,Z), X == s(X1),Y == Y1.
search @ add(X,Y,s(Z)) <=> add(X1,Y1,Z), X == X1,Y == s(Y1).
add(X,s(s(0)),s(s(s(0)))).
#result: X==s(0).
add(s(s(0)), s(0), Z).
#result: Z==s(s(s(0))).
add(X,Y,s(s(0))).
#result: X==s(s(0)), Y==0 .
add(X,X,s(s(0))).
#result: X==s(0).

add(X,X,s(s(s(0)))).
#result: false .
add(s(0),X,Y), add(X,s(s(0)),s(s(s(0)))).
#result: Y==s(s(0)), X==s(0) .

// second rule set: change succ3- and search-rule in orginal code
zero1 @ add(0,Y,Z) <=> Y == Z.
zero2 @ add(X,0,Z) <=> X == Z.
zero3 @ add(X,Y,0) <=> X == 0, Y == 0 .

same1 @ add(X,E,E) <=> X == 0 .
same2 @ add(E,Y,E) <=> Y == 0 .
// replace succ3

// new_succ3 @ add(s(X), s(X), s(s(W))) <=> add(X,X,W)
succ1 @ add(s(X),Y,Z) <=> Z == s(W), add(X,Y,W).
// new_succ1 @ add(s(X), Y, s(W)) <=> add(X, Y, W)
succ2 @ add(X,s(Y),Z) <=> Z == s(W), add(X,Y,W).
// new_succ2 @ add(X,s(Y),s(W)) <=> add(X,Y,W).
//succ3 @ add(X,X,s(Z)) <=> Z == s(W), X == s(Y), add(Y,Y,W).
succ3_1 @ add(X,X,s(s(W))) <=> X == s(Y), add(Y,Y,W).
succ3_1a @ add(X,Y,s(s(W))) <=> X == s(A), Y == s(B), add(A,B,W).

// search @ add(X,Y,s(Z)) <=> true | add(X1,Y1,Z),
//                                (X = s(X1),Y = Y1 ; X = X1,Y = s(Y1)).

// search1 @ add(X,Y,s(Z)) <=> X == s(X1),Y == Y1 | add(X1,Y1,Z).
// search1 @ add(s(X1), Y, s(Z)) <=> add(X1, Y, Z).
// search2 @ add(X,Y,s(Z)) <=> X == X1, Y == s(Y1)| add(X1,Y1,Z).
// add(s(s(0)),s(s(s(0))),s(s(s(s(s(0)))))). // yes
add(X,s(s(0)),s(s(s(0)))).
#result: X == s(0).
add(s(s(0)), s(0), Z).
#result: Z == s(s(s(0))) .
add(X,Y,s(s(0))).
#result: X == s(0), Y == s(0).
add(X,X,s(s(0))).
#result: X == s(0) .
`)
	if !ok {
		t.Error("TestCHRRule10 fails")
	}
}
---
*/

/*  --- OK
func TestCHRRule11(t *testing.T) {
	CHRtrace = 3
	src := `
modus_ponens @ implies(P,Q), P ==> Q.
implies(farbe(rot), farbe(blau)), farbe(rot) .
#result: implies(farbe(rot), farbe(blau)), farbe(rot), farbe(blau) .
`

	var s sc.Scanner
	// Initialize the scanner.
	s.Init(strings.NewReader(src))

	s.Error = Err
	rs := MakeRuleStore()
	ok := parseEvalRules(rs, &s)

	for _, rule := range rs.CHRruleStore {
		fmt.Printf(" Rule: %s @ ", rule.name)
		for _, h := range rule.keepHead {
			fmt.Printf("%s, ", h)
		}
		fmt.Printf("\\ ")
		for _, k := range rule.delHead {
			fmt.Printf("%s, ", k)
		}
		fmt.Printf(" <=> ")
		for _, g := range rule.guard {
			fmt.Printf("%s, ", g)
		}
		fmt.Printf(" | ")
		for _, b := range rule.body {
			fmt.Printf("%s, ", b)
		}
		fmt.Printf("\n")
	}

	if !ok {
		t.Error("TestCHRRule11 fails")
	}
}
*/
func TestCHRRule13(t *testing.T) {
	CHRtrace = 4
	src := `
modus_ponens @ implies(P,Q), P ==> Q.
implies(farbe(rot), farbe(blau)), implies(farbe(blau),farbe(grün)), farbe(rot) .
#result: implies(farbe(rot), farbe(blau)), implies(farbe(blau),farbe(grün)), farbe(rot), farbe(blau), farbe(grün) .
`

	var s sc.Scanner
	// Initialize the scanner.
	s.Init(strings.NewReader(src))

	s.Error = Err
	rs := MakeRuleStore()
	ok := parseEvalRules(rs, &s)

	if !ok {
		t.Error("TestCHRRule13 fails")
	}
}

/*
func TestCHRRule14(t *testing.T) {
	CHRtrace = 0
	src := `
modus_ponens @ implies(P,Q), P ==> Q.
implies(rot(), blau()), rot() .
#result: implies(rot(), blau()), rot(), blau() .
`

	var s sc.Scanner
	// Initialize the scanner.
	s.Init(strings.NewReader(src))

	s.Error = Err
	rs := MakeRuleStore()
	ok := parseEvalRules(rs, &s)

	if !ok {
		t.Error("TestCHRRule14 fails")
	}
}
*/

/*
func TestCHRRule16(t *testing.T) {
	CHRtrace = 0
	src := `
modus_ponens @ gelb(), implies(P,Q), P ==> Q.
implies(rot(), blau()), implies(blau(),grün()), rot(), gelb() .
#result: implies(rot(), blau()), implies(blau(),grün()), rot(), gelb(), blau(), grün() .
`

	var s sc.Scanner
	// Initialize the scanner.
	s.Init(strings.NewReader(src))

	s.Error = Err
	rs := MakeRuleStore()
	ok := parseEvalRules(rs, &s)

	if !ok {
		t.Error("TestCHRRule16 fails")
	}
}

func TestCHRRule17(t *testing.T) {
	CHRtrace = 0
	src := `
modus_ponens @ implies(P,Q), P ==> Q.
implies(rot, blau), rot .
#result: implies(rot, blau), rot, blau .
`
	var s sc.Scanner
	// Initialize the scanner.
	s.Init(strings.NewReader(src))

	s.Error = Err
	rs := MakeRuleStore()
	ok := parseEvalRules(rs, &s)

	if !ok {
		t.Error("TestCHRRule17 fails")
	}
}
*/
/*
func TestCHRRule19(t *testing.T) {
	CHRtrace = 0
	src := `
modus_ponens @ gelb, implies(P,Q), P ==> Q.
implies(rot, blau), implies(blau,grün), rot, gelb .
#result: implies(rot, blau), implies(blau,grün), rot, gelb, blau, grün .
`

	var s sc.Scanner
	// Initialize the scanner.
	s.Init(strings.NewReader(src))

	s.Error = Err
	rs := MakeRuleStore()
	ok := parseEvalRules(rs, &s)

	if !ok {
		t.Error("TestCHRRule19 fails")
	}
}
*/
/*
func TestCHRRule12(t *testing.T) {
	CHRtrace = 0
	src := `
modus_ponens @ implies(P,Q), P <=> Q.
implies(farbe(rot), farbe(blau)), farbe(rot) .
#result: farbe(blau) .
`

	var s sc.Scanner
	// Initialize the scanner.
	s.Init(strings.NewReader(src))

	s.Error = Err
	rs := MakeRuleStore()
	ok := parseEvalRules(rs, &s)

	if !ok {
		t.Error("TestCHRRule12 fails")
	}
}

func TestCHRRule15(t *testing.T) {
	CHRtrace = 0
	src := `
modus_ponens @ implies(P,Q), P <=> Q.
implies(rot(), blau()), rot() .
#result: blau() .
`

	var s sc.Scanner
	// Initialize the scanner.
	s.Init(strings.NewReader(src))

	s.Error = Err
	rs := MakeRuleStore()
	ok := parseEvalRules(rs, &s)

	if !ok {
		t.Error("TestCHRRule15 fails")
	}
}



func TestCHRRule18(t *testing.T) {
	CHRtrace = 0
	src := `
modus_ponens @ implies(P,Q), P <=> Q.
implies(rot, blau), rot .
#result: blau .
`
	var s sc.Scanner
	// Initialize the scanner.
	s.Init(strings.NewReader(src))

	s.Error = Err
	rs := MakeRuleStore()
	ok := parseEvalRules(rs, &s)

	if !ok {
		t.Error("TestCHRRule18 fails")
	}
}


func TestRS01(t *testing.T) {
	rs := MakeRuleStore()
	keep := []string{}
	del := []string{"sum([], S)"}
	guard := []string{}
	body := []string{"S == 0"}
	err := rs.AddRule("Sum01", keep, del, guard, body)
	if err != nil {
		fmt.Printf("Add Rule Sum01 fail \n")
		fmt.Print(err.Error())
		t.Error(err.Error())
	}
	keep = []string{}
	del = []string{"sum([X|Xs], S)"}
	guard = []string{}
	body = []string{"sum(Xs, S2)", "S == X + S2"}
	err = rs.AddRule("Sum02", keep, del, guard, body)
	if err != nil {
		fmt.Printf("Add Rule Sum02 fail \n")
		fmt.Print(err.Error())
		t.Error(err.Error())
	}
	CHRtrace = 0
	//	sum([1,2,3,4,5,6,7,8,9,10], S).
	//	#result: S == 55 .
	rBool, rList, err := rs.Infer([]string{"sum([1,2,3,4,5,6,7,8,9,10], S)"})
	CHRtrace = 0
	if err != nil {
		fmt.Printf("Infer fail \n")
		fmt.Print(err.Error())
		t.Error(err.Error())
	}
	fmt.Printf("\nresult: %v = %v \n", rBool, rList)
	checkResult(rs, t, "", "S == 55")

	//	//	sum([X,2,3], 6).
	//	//	#result: X == 1 .
	//	CHRtrace = 4
	//	rBool, rList, err = rs.Infer([]string{"sum([X,2,3], 6)"})
	//	CHRtrace = 1
	//	if err != nil {
	//		fmt.Printf("Infer fail \n")
	//		fmt.Print(err.Error())
	//		t.Error(err.Error())
	//	}
	//	fmt.Printf("\nresult: %v = %v \n", rBool, rList)
	//	checkResult(rs, t, "", "X == 1")
	//	//	sum([1,X,3], 6).
	//	//	#result: X == 2 .
	//	CHRtrace = 0
	//	rBool, rList, err = rs.Infer([]string{"sum([1,X,3], 6)"})
	//	CHRtrace = 1
	//	if err != nil {
	//		fmt.Printf("Infer fail \n")
	//		fmt.Print(err.Error())
	//		t.Error(err.Error())
	//	}
	//	fmt.Printf("\nresult: %v = %v \n", rBool, rList)
	//	checkResult(rs, t, "", "X == 2")
	//	CHRtrace = 0
}

func TestCHRRule20(t *testing.T) {
	CHRtrace = 0
	src := `
gov_stats_scheme @ gov_stats(C,S) ==> safety(C,S), argument(gov_stats_scheme,[C,S]).
advertising_scheme @ advertising(C,S) ==> safety(C,S), argument(advertising_scheme,[C,S]).
car_buying_scheme @ price(C,P), type(C,T), speed(C,S), safety(C,F) ==> buy(C), argument(car_buying_scheme,[C,P,S,F,T]).
price(volvo,medium), price(porsche,high), advertising(volvo,high), safety(porsche,medium), speed(porsche,fast), type(porsche,sports), type(volvo,family),   gov_stats(volvo,medium), speed(volvo,medium).
// advertising(volvo,high), type(volvo,family), price(porsche,high), type(porsche,sports), price(volvo,medium), speed(porsche,fast), gov_stats(volvo,medium), speed(volvo,medium), safety(porsche,medium).
// #result: implies(rot, blau), implies(blau,grün), rot, gelb, blau, grün .
`

	var s sc.Scanner
	// Initialize the scanner.
	s.Init(strings.NewReader(src))

	s.Error = Err
	rs := MakeRuleStore()
	ok := parseEvalRules(rs, &s)

	if !ok {
		t.Error("TestCHRRule20 fails")
	}
}

func TestCHRRule21a(t *testing.T) {
	CHRtrace = 1
	src := `selectors @ dataUseStatement(dus(ResultScope,ID)) ==> resultScope(dus(ResultScope,ID),ResultScope).
smallerOrEqualScope1 @ resultScope(S11,C),resultScope(S12,C) ==> smallerOrEqualScope(S11,S12),argument(smallerOrEqualScope1,[S11,S12,C]).
// smallerOrEqualScope2 @ smallerOrEqualScope(S21,S22) \ smallerOrEqualScope(S21,S22) <=> true,argument(smallerOrEqualScope2,[S21,S22]).
smallerOrEqualScope2 @ smallerOrEqualScope(S21,S22) \ smallerOrEqualScope(S21,S22) <=> argument(smallerOrEqualScope2,[S21,S22]).
smallerOrEqualScope3 @ smallerOrEqualScope(S31,S32), smallerOrEqualScope(S32,S33) ==> smallerOrEqualScope(S31,S33),argument(smallerOrEqualScope3,[S31,S32,S33]).


dataUseStatement(dus(capability,id01)),
      dataUseStatement(dus(capability,id02)),
      dataUseStatement(dus(capability,id03)).
`

	var s sc.Scanner
	// Initialize the scanner.
	s.Init(strings.NewReader(src))

	s.Error = Err
	rs := MakeRuleStore()
	ok := parseEvalRules(rs, &s)

	if !ok {
		t.Error("TestCHRRule21 fails")
	}
}

func TestCHRRule21(t *testing.T) {
	CHRtrace = 1
	src := `selectors @ dataUseStatement(dus(UseScope,Qualifier,DataCategory,SourceScope,Action,ResultScope,ID,Passive)) ==> resultScope(dus(ResultScope,ID),ResultScope).
smallerOrEqualScope1 @ resultScope(S1,C),resultScope(S2,C) ==> smallerOrEqualScope(S1,S2),argument(smallerOrEqualScope1,[S1,S2,C]).
smallerOrEqualScope2 @ smallerOrEqualScope(S1,S2) \ smallerOrEqualScope(S1,S2) <=> true,argument(smallerOrEqualScope2,[S1,S2]).
smallerOrEqualScope3 @ smallerOrEqualScope(S1,S2),smallerOrEqualScope(S2,S3) ==> smallerOrEqualScope(S1,S3),argument(smallerOrEqualScope3,[S1,S2,S3]).


dataUseStatement(dus(capability,identified_data,provider_data_authentication,capability,provide,capability,id01,false)),
      dataUseStatement(dus(capability,pseudonymized_data,provider_data_operations,capability,provide,capability,id02,false)),
      dataUseStatement(dus(capability,aggregated_data,derived_data_user_social,capability,provide,capability,id03,false)),
//      dataUseStatement(dus(capability,unlinked_data,customer_content_genetic,capability,provide,capability,id04,false)),
    go.
`

	var s sc.Scanner
	// Initialize the scanner.
	s.Init(strings.NewReader(src))

	s.Error = Err
	rs := MakeRuleStore()
	ok := parseEvalRules(rs, &s)

	if !ok {
		t.Error("TestCHRRule21 fails")
	}
}
--
*/
/*
func TestCHRRule21(t *testing.T) {
	CHRtrace = 1
	src := `selectors @ dataUseStatement(dus(UseScope,Qualifier,DataCategory,SourceScope,Action,ResultScope,ID,Passive)) ==> resultScope(dus(UseScope,Qualifier,DataCategory,SourceScope,Action,ResultScope,ID,Passive),ResultScope).
// selectors @ dataUseStatement(dus(UseScope,Qualifier,DataCategory,SourceScope,Action,ResultScope,ID,Passive)) ==> useScope(dus(UseScope,Qualifier,DataCategory,SourceScope,Action,ResultScope,ID,Passive),UseScope),qualifier(dus(UseScope,Qualifier,DataCategory,SourceScope,Action,ResultScope,ID,Passive),Qualifier),dataCategory(dus(UseScope,Qualifier,DataCategory,SourceScope,Action,ResultScope,ID,Passive),DataCategory),sourceScope(dus(UseScope,Qualifier,DataCategory,SourceScope,Action,ResultScope,ID,Passive),SourceScope),action(dus(UseScope,Qualifier,DataCategory,SourceScope,Action,ResultScope,ID,Passive),Action),resultScope(dus(UseScope,Qualifier,DataCategory,SourceScope,Action,ResultScope,ID,Passive),ResultScope),id(dus(UseScope,Qualifier,DataCategory,SourceScope,Action,ResultScope,ID,Passive),ID),passive(dus(UseScope,Qualifier,DataCategory,SourceScope,Action,ResultScope,ID,Passive),Passive),argument(selectors,[UseScope,Qualifier,DataCategory,SourceScope,Action,ResultScope,ID,Passive]).
//kindOfTransitivity @ kindOf(X,Y),kindOf(Y,Z) ==> kindOf(X,Z),argument(kindOfTransitivity,[X,Y,Z]).
//partOfTransitivity @ partOf(X,Y),partOf(Y,Z) ==> partOf(X,Z),argument(partOfTransitivity,[X,Y,Z]).
//qualifier1 @ qualifier(S,identified_data) ==> qualifier(S,unqualified_or_identified),argument(qualifier1,[S]).
//qualifier2 @ qualifier(S,unqualified) ==> qualifier(S,unqualified_or_identified),argument(qualifier2,[S]).
// scope1 @ scope(S,csp_products) ==> scope(S,csp_services),argument(scope1,[S]).
// scope2 @ scope(S,csp_services) ==> scope(S,services_agreement),argument(scope2,[S]).
// scope3 @ scope(S,services_agreement) ==> scope(S,service),argument(scope3,[S]).
// scope4 @ scope(S,service) ==> scope(S,cability),argument(scope4,[S]).
//category0 @ dataCategory(S,X),kindOf(X,Y) ==> dataCategory(S,Y),argument(category0,[S,X,Y]).
//category1 @ go ==> kindOf(derived_data_user_telemetry,derived_data_user),argument(category1,[]).
// category2 @ go ==> kindOf(derived_data_user_connectivity,derived_data_user),argument(category2,[]).
// category3 @ go ==> kindOf(derived_data_user_usage,derived_data_user),argument(category3,[]).
// category4 @ go ==> kindOf(derived_data_user_demographic,derived_data_user),argument(category4,[]).
//category5 @ go ==> kindOf(derived_data_user_profiling,derived_data_user),argument(category5,[]).
//category6 @ go ==> kindOf(derived_data_user_content,derived_data_user),argument(category6,[]).
//category7 @ go ==> kindOf(derived_data_user_browsing,derived_data_user),argument(category7,[]).
//category8 @ go ==> kindOf(derived_data_user_search,derived_data_user),argument(category8,[]).
//category9 @ go ==> kindOf(derived_data_user_location,derived_data_user),argument(category9,[]).
//category10 @ go ==> kindOf(derived_data_user_social,derived_data_user),argument(category10,[]).
//category11 @ go ==> kindOf(derived_data_user_biometric,derived_data_user),argument(category11,[]).
//category12 @ go ==> kindOf(derived_data_user_contact,derived_data_user),argument(category12,[]).
//category13 @ go ==> kindOf(derived_data_user_environmental,derived_data_user),argument(category13,[]).
//category14 @ go ==> kindOf(customer_content_credentials,customer_content),argument(category14,[]).
//category15 @ go ==> kindOf(customer_content_contact,customer_content),argument(category15,[]).
//category16 @ go ==> kindOf(customer_content_health,customer_content),argument(category16,[]).
//category17 @ go ==> kindOf(customer_content_genetic,customer_content),argument(category17,[]).
//category18 @ go ==> kindOf(customer_content_biometric,customer_content),argument(category18,[]).
//category19 @ go ==> kindOf(customer_content_children,customer_content),argument(category19,[]).
//category20 @ go ==> kindOf(customer_content_opinions,customer_content),argument(category20,[]).
//category21 @ go ==> kindOf(customer_content_financial,customer_content),argument(category21,[]).
//category22 @ go ==> kindOf(derived_data_user,derived_data),argument(category22,[]).
//category23 @ go ==> kindOf(derived_data_organization,derived_data),argument(category23,[]).
//category24 @ go ==> kindOf(provider_data_authentication,provider_data),argument(category24,[]).
//category25 @ go ==> kindOf(provider_data_operations,provider_data),argument(category25,[]).
//category26 @ go ==> kindOf(account_data_customer,account_data),argument(category26,[]).
//category27 @ go ==> kindOf(account_data_payment,account_data),argument(category27,[]).
//action0 @ action(S,X),kindOf(X,Y) ==> action(S,Y),argument(action0,[S,X,Y]).
//action1 @ go ==> kindOf(market,market_advertise_promote),argument(action1,[]).
//action2 @ go ==> kindOf(advertise,market_advertise_promote),argument(action2,[]).
//action3 @ go ==> kindOf(promote,market_advertise_promote),argument(action3,[]).
//action4 @ go ==> kindOf(market_contextual,market),argument(action4,[]).
//action5 @ go ==> kindOf(market_personalization,market),argument(action5,[]).
//action6 @ go ==> kindOf(advertise_contextual,advertise),argument(action6,[]).
//action7 @ go ==> kindOf(advertise_personalization,advertise),argument(action7,[]).
//action8 @ go ==> kindOf(promote_contextual,promote),argument(action8,[]).
//action9 @ go ==> kindOf(promote_personalization,promote),argument(action9,[]).
//action10 @ go ==> kindOf(share_provide,share),argument(action10,[]).
//docConsent1 @ go ==> notDocConsentRequired,argument(docConsent1,[]).
//docConsent2 @ consentRequired(S) ==> docConsentRequired,argument(docConsent2,[S]).
//pii0 @ dataUseStatement(S) ==> pii(S),argument(pii0,[S]).
//pii3 @ qualifier(S,unlinked_data) ==> notPii(S),argument(pii3,[S]).
//pii4 @ qualifier(S,anonymized_data) ==> notPii(S),argument(pii4,[S]).
//pii5 @ qualifier(S,aggregated_data) ==> notPii(S),argument(pii5,[S]).
//pii6 @ qualifier(S,pseudonymized_data),dataCategory(S,derived_data_organization) ==> notPii(S),argument(pii6,[S]).
//pii7 @ qualifier(S,pseudonymized_data),dataCategory(S,provider_data_authentication) ==> notPii(S),argument(pii7,[S]).
//pii8 @ qualifier(S,unqualified_or_identified),dataCategory(S,derived_data_organization) ==> notPii(S),argument(pii8,[S]).
//pii9 @ qualifier(S,unqualified_or_identified),dataCategory(S,provider_data_authentication) ==> notPii(S),argument(pii9,[S]).
//pii11 @ qualifier(S,unqualified),dataCategory(S,derived_data_organization) ==> notPii(S),argument(pii11,[S]).
//pii12 @ qualifier(S,unqualified),dataCategory(S,provider_data_authentication) ==> notPii(S),argument(pii12,[S]).
//li0 @ dataUseStatement(S) ==> notLi(S),argument(li0,[S]).
//li1 @ action(S,provide),resultScope(S,capability) ==> li(S),argument(li1,[S]).
//li2 @ action(S,provide),resultScope(S,service) ==> li(S),argument(li2,[S]).
//li3 @ action(S,improve),resultScope(S,capability) ==> li(S),argument(li3,[S]).
//li4 @ action(S,improve),resultScope(S,service) ==> li(S),argument(li4,[S]).
//li5 @ action(S,improve),resultScope(S,services_agreement) ==> li(S),argument(li5,[S]).
//li6 @ action(S,improve),resultScope(S,csp_services) ==> li(S),argument(li6,[S]).
//li7 @ action(S,improve),resultScope(S,csp_products) ==> li(S),argument(li7,[S]).
//li8 @ action(S,personalize),resultScope(S,capability) ==> li(S),argument(li8,[S]).
//li9 @ action(S,personalize),resultScope(S,service) ==> li(S),argument(li9,[S]).
//li10 @ action(S,upgrades),resultScope(S,capability) ==> li(S),argument(li10,[S]).
//li11 @ action(S,upgrades),resultScope(S,service) ==> li(S),argument(li11,[S]).
//li12 @ action(S,upgrades),resultScope(S,services_agreement) ==> li(S),argument(li12,[S]).
//li13 @ action(S,market_advertise_promote),resultScope(S,capability) ==> li(S),argument(li13,[S]).
//li14 @ action(S,market_advertise_promote),resultScope(S,service) ==> li(S),argument(li14,[S]).
//equivalentScope0 @ resultScope(S1,S),resultScope(S2,S) ==> equivalentScope(S1,S2),argument(equivalentScope0,[S1,S2]).
//equivalentScope1 @ resultScope(S1,capability),resultScope(S2,service) ==> equivalentScope(S1,S2),argument(equivalentScope1,[S1,S2]).
//equivalentScope2 @ resultScope(S1,services_agreement),resultScope(S2,csp_services) ==> equivalentScope(S1,S2),argument(equivalentScope2,[S1,S2]).
//equivalentScope3 @ resultScope(S1,third_party_partners),resultScope(S2,third_party_services) ==> equivalentScope(S1,S2),argument(equivalentScope3,[S1,S2]).
//equivalentScope4 @ resultScope(S1,third_party_services),resultScope(S2,third_party_partners) ==> equivalentScope(S1,S2),argument(equivalentScope4,[S1,S2]).
smallerOrEqualScope1 @ resultScope(S1,C),resultScope(S2,C) ==> smallerOrEqualScope(S1,S2),argument(smallerOrEqualScope1,[S1,S2,C]).
smallerOrEqualScope2 @ smallerOrEqualScope(S1,S2) \ smallerOrEqualScope(S1,S2) <=> true,argument(smallerOrEqualScope2,[S1,S2]).
smallerOrEqualScope3 @ smallerOrEqualScope(S1,S2),smallerOrEqualScope(S2,S3) ==> smallerOrEqualScope(S1,S3),argument(smallerOrEqualScope3,[S1,S2,S3]).
//smallerOrEqualScope4 @ resultScope(S1,P1),resultScope(S2,P2),lesserScope(P1,P2) ==> smallerOrEqualScope(S1,S2),argument(smallerOrEqualScope4,[S1,S2,P1,P2]).
//lesserScope1 @ go ==> lesserScope(capability,service),lesserScope(service,services_agreement),lesserScope(services_agreement,csp_services),lesserScope(csp_services,csp_products),argument(lesserScope1,[]).
//compatiblePurposeDuplicates @ compatiblePurpose(S1,S2) \ compatiblePurpose(S1,S2) <=> true,argument(compatiblePurposeDuplicates,[S1,S2]).
//compatiblePurpose1 @ action(S1,A),action(S2,A),equivalentScope(S1,S2) ==> compatiblePurpose(S1,S2),argument(compatiblePurpose1,[S1,S2,A]).
//compatiblePurpose2 @ action(S1,provide),action(S2,improve),smallerOrEqualScope(S2,csp_products) ==> compatiblePurpose(S1,S2),argument(compatiblePurpose2,[S1,S2]).
//compatiblePurpose3 @ action(S1,provide),action(S2,upgrades),equivalentScope(S1,S2) ==> compatiblePurpose(S1,S2),argument(compatiblePurpose3,[S1,S2]).
//compatiblePurpose4 @ action(S1,provide),action(S2,market_advertise_promote),equivalentScope(S1,S2) ==> compatiblePurpose(S1,S2),argument(compatiblePurpose4,[S1,S2]).
//compatiblePurpose5 @ action(S1,improve),action(S2,improve),smallerOrEqualScope(S2,csp_products) ==> compatiblePurpose(S1,S2),argument(compatiblePurpose5,[S1,S2]).
//compatiblePurpose6 @ action(S1,personalize),action(S2,personalize),smallerOrEqualScope(S2,services_agreement) ==> compatiblePurpose(S1,S2),argument(compatiblePurpose6,[S1,S2]).
//compatiblePurpose7 @ action(S1,personalize),action(S2,market_advertise_promote),smallerOrEqualScope(S2,services_agreement) ==> compatiblePurpose(S1,S2),argument(compatiblePurpose7,[S1,S2]).
//compatiblePurpose8 @ action(S1,upgrades),action(S2,upgrades),smallerOrEqualScope(S2,services_agreement) ==> compatiblePurpose(S1,S2),argument(compatiblePurpose8,[S1,S2]).
// consentRequired0 @ consentRequired(S1) \ consentRequired(S1) <=> true,argument(consentRequired0,[S1]).
// consentRequired1 @ notConsentRequired(S1) \ notConsentRequired(S1) <=> true,argument(consentRequired1,[S1]).
// consentRequired2 @ dataUseStatement(S) ==> consentRequired(S),argument(consentRequired2,[S]).
//consentRequired3 @ notPii(S) ==> notConsentRequired(S),argument(consentRequired3,[S]).
//consentRequired4 @ pii(S),li(S) ==> notConsentRequired(S),argument(consentRequired4,[S]).
//consentRequired5 @ pii(S1),pii(S2),li(S2),compatiblePurpose(S1,S2) ==> notConsentRequired(S1),argument(consentRequired5,[S1,S2]).


dataUseStatement(dus(capability,identified_data,provider_data_authentication,capability,provide,capability,a6bf5e759ee6f4177ac862579a1f89570,false)),
      dataUseStatement(dus(capability,pseudonymized_data,provider_data_operations,capability,provide,capability,abfd5f3573ea94310bf57d282d02789f4,false)),
      dataUseStatement(dus(capability,aggregated_data,derived_data_user_social,capability,provide,capability,acc18624e860c4feaaa4fc95f8f107ad3,false)),
//      dataUseStatement(dus(capability,aggregated_data,derived_data_user_browsing,capability,provide,capability,a49b72f578f2f4124a496f408f79c14aa,false)),
//      dataUseStatement(dus(capability,identified_data,customer_content_credentials,capability,provide,capability,a3de0b1dfd69c492487adce5a39dc7795,false)),
//      dataUseStatement(dus(capability,anonymized_data,customer_content_children,capability,provide,capability,a382c3ac6971c4f45b46849548ce339d9,false)),
//      dataUseStatement(dus(capability,unlinked_data,account_data_payment,capability,provide,capability,a94acb27e063746e89853999eabfba608,false)),
//      dataUseStatement(dus(capability,unlinked_data,customer_content_financial,capability,provide,capability,af9f7160067be4a82b078f6d38a3ae892,false)),
//      dataUseStatement(dus(capability,anonymized_data,derived_data_user_telemetry,capability,provide,services_agreement,abcce38032ccd48f1b078deb91e34c306,false)),
//      dataUseStatement(dus(capability,pseudonymized_data,derived_data_user_connectivity,capability,provide,capability,adf059a79d31c4c88b05cef7376e199ac,false)),
      dataUseStatement(dus(capability,unlinked_data,customer_content_genetic,capability,provide,capability,ac74bb5e616e9449f8f504f6a8ccbde1e,false)),
    go.
`

	var s sc.Scanner
	// Initialize the scanner.
	s.Init(strings.NewReader(src))

	s.Error = Err
	rs := MakeRuleStore()
	ok := parseEvalRules(rs, &s)

	if !ok {
		t.Error("TestCHRRule21 fails")
	}
}
*/

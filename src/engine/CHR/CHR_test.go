// Copyright © 2016 The Carneades Authors
// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL
// was not distributed with this file, You can obtain one
// at http://mozilla.org/MPL/2.0/.

package chr

import (
	"fmt"
	"strings"
	"testing"
	sc "text/scanner"

	. "github.com/hfried/GoCHR/src/engine/parser"
)

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
	CHRtrace = 1
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
	prime(100).
	#result:  prime(97), prime(89), prime(83), prime(79), prime(73), prime(71), prime(67), prime(61), prime(59), prime(53), prime(47), prime(43), prime(41), prime(37), prime(31), prime(29), prime(23), prime(19), prime(17), prime(13), prime(11), prime(7), prime(5), prime(3), prime(2).
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
	CHRtrace = 1
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

func TestCHRRule11(t *testing.T) {
	CHRtrace = 1
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

func TestCHRRule12(t *testing.T) {
	CHRtrace = 1
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

func TestCHRRule13(t *testing.T) {
	CHRtrace = 1
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

func TestCHRRule14(t *testing.T) {
	CHRtrace = 1
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

func TestCHRRule15(t *testing.T) {
	CHRtrace = 1
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

func TestCHRRule16(t *testing.T) {
	CHRtrace = 1
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
	CHRtrace = 1
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

func TestCHRRule18(t *testing.T) {
	CHRtrace = 1
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

func TestCHRRule19(t *testing.T) {
	CHRtrace = 1
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
	CHRtrace = 1
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

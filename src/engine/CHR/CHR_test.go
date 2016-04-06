// Copyright © 2016 The Carneades Authors
// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL
// was not distributed with this file, You can obtain one
// at http://mozilla.org/MPL/2.0/.

package chr

import (
	. "github.com/hfried/GoCHR/src/engine/parser"
	"testing"
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
		t.Errorf("TestStoreß7 failed\n")
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

func TestCHRRule01(t *testing.T) {
	CHRtrace = 0
	ok := ParseStringCHRRulesGoals(`
	prime01 @ prime(N) ==> N>2 | prime(N-1).
	prime02 @ prime(A) | prime(B) <=> B > A, B mod A == 0 | true.
	prime(100).`)
	if !ok {
		t.Error("TestCHRRule01 fails, Error in parse string")
	}

	CHRsolver()
	CHRtrace = 1
	printCHRStore()
	CHRtrace = 0
	tNewQuery(t, "prime(20)")
	checkResult(t, "prime(19), prime(17), prime(13), prime(11), prime(7), prime(5), prime(3), prime(2)", "")
	CHRtrace = 1
	printCHRStore()
}

func TestCHRRule02(t *testing.T) {
	CHRtrace = 0
	ok := ParseStringCHRRulesGoals(`
	gcd01@ gcd(0) <=> true .
	// logarithmic complexity
	gcd02@ gcd(N) \ gcd(M) <=> N <= M, L := M mod N | gcd(L).
	gcd(94017), gcd(1155),gcd(2035).`)
	if !ok {
		t.Error("TestCHRRule02 fails, Error in parse string")
	}
	CHRsolver()
	checkResult(t, "gcd(11)", "")
	CHRtrace = 1
	printCHRStore()
	CHRtrace = 0
	tNewQuery(t, "gcd(12),gcd(18)")
	checkResult(t, "gcd(6)", "")

	CHRtrace = 1
	printCHRStore()
	CHRtrace = 0
	tNewQuery(t, "gcd(3528),gcd(3780)")
	checkResult(t, "gcd(252)", "")

	CHRtrace = 1
	printCHRStore()
}

func TestCHRRule02a(t *testing.T) {
	CHRtrace = 0
	ok := ParseStringCHRRulesGoals(`
	gcd01@ gcd(0) <=> true .
	// logarithmic complexity
	gcd02@ gcd(N) \ gcd(M) <=> N <= M | gcd(M mod N).
	gcd(94017), gcd(1155),gcd(2035).`)
	if !ok {
		t.Error("TestCHRRule02a fails, Error in parse string")
	}
	CHRsolver()
	checkResult(t, "gcd(11)", "")
	CHRtrace = 1
	printCHRStore()
	CHRtrace = 0
	tNewQuery(t, "gcd(12),gcd(18)")
	checkResult(t, "gcd(6)", "")

	CHRtrace = 1
	printCHRStore()
	CHRtrace = 0
	tNewQuery(t, "gcd(3528),gcd(3780)")
	checkResult(t, "gcd(252)", "")

	CHRtrace = 1
	printCHRStore()
}

func TestCHRRule02b(t *testing.T) {
	CHRtrace = 0
	ok := ParseStringCHRRulesGoals(`
	gcd01@ gcd(0) <=> true .
	// linear complexity
	gcd02@ gcd(N) \ gcd(M) <=> 0<N, N=<M | gcd(M-N).
	gcd(94017), gcd(1155),gcd(2035).`)
	if !ok {
		t.Error("TestCHRRule02b fails, Error in parse string")
	}

	CHRsolver()

	checkResult(t, "gcd(11)", "")
	CHRtrace = 1
	printCHRStore()
	CHRtrace = 0
	tNewQuery(t, "gcd(12),gcd(18)")
	checkResult(t, "gcd(6)", "")

	CHRtrace = 1
	printCHRStore()
	CHRtrace = 0
	tNewQuery(t, "gcd(3528),gcd(3780)")
	checkResult(t, "gcd(252)", "")

	CHRtrace = 1
	printCHRStore()
}

func TestCHRRule02c(t *testing.T) {
	CHRtrace = 0
	ok := ParseStringCHRRulesGoals(`
	gcd01@ gcd(0) <=> true .
	// linear complexity
	gcd02@ gcd(N) \ gcd(M) <=> 0<N, N=<M, L := M - N | gcd(L).
	gcd(94017), gcd(1155),gcd(2035).`)
	if !ok {
		t.Error("TestCHRRule02c fails, Error in parse string")
	}

	CHRsolver()

	checkResult(t, "gcd(11)", "")
	CHRtrace = 1
	printCHRStore()
	CHRtrace = 0
	tNewQuery(t, "gcd(12),gcd(18)")
	checkResult(t, "gcd(6)", "")

	CHRtrace = 1
	printCHRStore()
	CHRtrace = 0
	tNewQuery(t, "gcd(3528),gcd(3780)")
	checkResult(t, "gcd(252)", "")

	CHRtrace = 1
	printCHRStore()
}

func TestCHRRule02d(t *testing.T) {
	CHRtrace = 0
	ok := ParseStringCHRRulesGoals(`
	gcd01@ gcd(0) <=> true .
	// linear complexity
	gcd02@ gcd(N) \ gcd(M) <=> 0<N, N=<M | L := M - N, gcd(L).
	gcd(12), gcd(27).`)
	if !ok {
		t.Error("TestCHRRule02d fails, Error in parse string")
	}

	CHRsolver()
	checkResult(t, "gcd(3)", "L2:=15, L4:=3, L6:=9, L8:=6, L10:=3, L12:=0")
	CHRtrace = 1
	printCHRStore()
	CHRtrace = 0
	tNewQuery(t, "gcd(12),gcd(18)")
	checkResult(t, "gcd(6)", "L2:=6, L4:=6, L6:=0")
	CHRtrace = 1
	printCHRStore()
}

func TestCHRRule04a(t *testing.T) {
	CHRtrace = 0
	ok := ParseStringCHRRulesGoals(`
	fib01@ upto(A) ==> fib(0,1), fib(1,1).
	fib02@ upto(Max), fib(N1,M1), fib(N2,M2) ==> Max > N2, N2 == N1+1 | fib(N2+1,M1+M2).
	upto(10).`)
	if !ok {
		t.Error("TestCHRRule04a fails, Error in parse string")
	}

	CHRsolver()

	checkResult(t, "upto(10), fib(0,1), fib(1,1), fib(2,2), fib(3,3), fib(4,5), fib(5,8), fib(6,13), fib(7,21), fib(8,34), fib(9,55), fib(10,89)", "")
	CHRtrace = 1
	printCHRStore()
	CHRtrace = 0
	tNewQuery(t, "upto(20)")
	checkResult(t, "upto(20), fib(0,1), fib(1,1), fib(2,2), fib(3,3), fib(4,5), fib(5,8), fib(6,13), fib(7,21), fib(8,34), fib(9,55), fib(10,89), fib(11,144), fib(12,233), fib(13,377), fib(14,610), fib(15,987), fib(16,1597), fib(17,2584), fib(18,4181), fib(19,6765), fib(20,10946)", "")

	CHRtrace = 1
	printCHRStore()
}

func TestCHRRule05a(t *testing.T) {
	CHRtrace = 0
	ok := ParseStringCHRRulesGoals(`
	leq_reflexivity  @ leq(X,X) <=> true.
	leq_antisymmetry @ leq(X,Y), leq(Y,X) <=> X==Y.
	leq_idempotence  @ leq(X,Y)\ leq(X,Y) <=> true.
	leq_transitivity @ leq(X,Y), leq(Y,Z) ==> leq(X,Z).
	leq(A,B), leq(B,C), leq(C,A).`)
	if !ok {
		t.Error("TestCHRRule05a fails, Error in parse string")
	}

	CHRsolver()

	checkResult(t, "", "A==C, B==C")
	CHRtrace = 1
	printCHRStore()
}

func TestCHRRule06(t *testing.T) {
	CHRtrace = 0
	ok := ParseStringCHRRulesGoals(`
	data1 @ data() ==> edge(berlin, 230, wolfsburg), edge(hannover, 89, wolfsburg), edge(hannover, 108, bielefeld), edge(bielefeld, 194, köln).
	data2 @ data() ==> edge(berlin,259, jena), edge(jena,55, erfurt), edge(erfurt,205,giessen), edge(giessen,158,köln), edge(köln, 85, aachen).
	source @ source(V) ==> dist(V, 0).
	del @ dist(V,D1) \ dist(V, D2) <=> D1 <= D2 | true.
	dist_plus1 @ dist(V,D1), edge(V, D2, V2) ==> dist(V2, D1+D2).
	dist_plus2 @ dist(V,D1), edge(V2, D2, V) ==> dist(V2, D1+D2).
	del_data @ edge(X,Y,Z) <=> true.
	data(), source(berlin).`)
	if !ok {
		t.Error("TestCHRRule06 fails, Error in parse string")
	}

	CHRsolver()

	checkResult(t, "source(berlin), dist(berlin,0), dist(wolfsburg,230), dist(jena,259), dist(erfurt,314), dist(giessen,519), dist(hannover,319), dist(bielefeld,427), dist(köln,621), dist(aachen,706)", "")
	CHRtrace = 1
	printCHRStore()
}

func TestCHRRule07(t *testing.T) {
	CHRtrace = 0
	ok := ParseStringCHRRulesGoals(`

	data1 @ data() ==> edge(berlin, 230, wolfsburg), edge(hannover, 89, wolfsburg), edge(hannover, 108, bielefeld), edge(bielefeld, 194, köln).
	data2 @ data() ==> edge(berlin,259, jena), edge(jena,55, erfurt), edge(erfurt,205,giessen), edge(giessen,158,köln), edge(köln, 85, aachen).
	source @ source(V) ==> dist(V, [V], 0).
	del @ dist(V, L, D1) \ dist(V, M, D2) <=> D1 <= D2 | true.
	dist_plus1 @ dist(V, L, D1), edge(V, D2, V2) ==> dist(V2,[V2|L], D1+D2).
	dist_plus2 @ dist(V, L, D1), edge(V2, D2, V) ==> dist(V2,[V2|L], D1+D2).
	del_data @ edge(X,Y,Z) <=> true.
	data(), source(berlin).`)
	if !ok {
		t.Error("TestCHRRule07 fails, Error in parse string")
	}

	CHRsolver()

	checkResult(t, "source(berlin), dist(berlin,[berlin],0), dist(wolfsburg,[wolfsburg, berlin],230), dist(jena,[jena, berlin],259), dist(erfurt,[erfurt, jena, berlin],314), dist(giessen,[giessen, erfurt, jena, berlin],519), dist(hannover,[hannover, wolfsburg, berlin],319), dist(bielefeld,[bielefeld, hannover, wolfsburg, berlin],427), dist(köln,[köln, bielefeld, hannover, wolfsburg, berlin],621), dist(aachen,[aachen, köln, bielefeld, hannover, wolfsburg, berlin],706)", "")
	CHRtrace = 1
	printCHRStore()
}

func TestCHRRule08(t *testing.T) {
	CHRtrace = 0
	ok := ParseStringCHRRulesGoals(`

	data1 @ data() ==> edge(berlin, 230, wolfsburg), edge(hannover, 89, wolfsburg), edge(hannover, 108, bielefeld), edge(bielefeld, 194, köln).
	data2 @ data() ==> edge(berlin,259, jena), edge(jena,55, erfurt), edge(erfurt,205,giessen), edge(giessen,158,köln), edge(köln, 85, aachen).
	source @ source(V) ==> dist([V], 0).
	del @ dist([V|L], D1) \ dist([V|M], D2) <=> D1 <= D2 | true.
	dist_plus_a@ dist([V|L], D1), edge(V, D2, V2) ==> dist([V2, V|L], D1+D2).
	dist_plus_b@ dist([V|L], D1), edge(V2, D2, V) ==> dist([V2, V|L], D1+D2).
	del_data @ edge(X,Y,Z) <=> true.
	data(), source(berlin).`)
	if !ok {
		t.Error("TestCHRRule08 fails, Error in parse string")
	}

	CHRsolver()

	checkResult(t, "source(berlin), dist([berlin],0), dist([wolfsburg, berlin],230), dist([jena, berlin],259), dist([erfurt, jena, berlin],314), dist([giessen, erfurt, jena, berlin],519), dist([hannover, wolfsburg, berlin],319), dist([bielefeld, hannover, wolfsburg, berlin],427), dist([köln, bielefeld, hannover, wolfsburg, berlin],621), dist([aachen, köln, bielefeld, hannover, wolfsburg, berlin],706)", "")
	CHRtrace = 1
	printCHRStore()
}

func TestCHRRule09(t *testing.T) {
	CHRtrace = 0
	ok := ParseStringCHRRulesGoals(`

	data1 @ data() ==> edge(berlin, 230, wolfsburg), edge(hannover, 89, wolfsburg), edge(hannover, 108, bielefeld), edge(bielefeld, 194, köln).
	data2 @ data() ==> edge(berlin,259, jena), edge(jena,55, erfurt), edge(erfurt,205,giessen), edge(giessen,158,köln), edge(köln, 85, aachen).
	source @ source(V) ==> dist([V], 0).
	del @ dist([V|L], D1) \ dist([V|M], D2) <=> D1 <= D2 | true.
	dist_plus_a@ dist([V|L], D1), edge(V, D2, V2) ==> dist([V2, V|L], D1+D2).
	dist_plus_b@ dist([V|L], D1), edge(V2, D2, V) ==> dist([V2, V|L], D1+D2).
	del_data @ edge(X,Y,Z) <=> true.
	data(), source(berlin).`)
	if !ok {
		t.Error("TestCHRRule09 fails, Error in parse string")
	}

	CHRsolver()

	checkResult(t, "source(berlin), dist([berlin],0), dist([wolfsburg, berlin],230), dist([jena, berlin],259), dist([erfurt, jena, berlin],314), dist([giessen, erfurt, jena, berlin],519), dist([hannover, wolfsburg, berlin],319), dist([bielefeld, hannover, wolfsburg, berlin],427), dist([köln, bielefeld, hannover, wolfsburg, berlin],621), dist([aachen, köln, bielefeld, hannover, wolfsburg, berlin],706)", "")
	CHRtrace = 1
	printCHRStore()
}

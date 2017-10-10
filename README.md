These are the source files of Version 2.0 of the
Constraint Handling Rule system, written in the Go programming language.

This source code is subject to the terms of the Mozilla Public
License, version 2.0 (MPL-2.0). If a copy of the MPL was not
distributed with this software, it is also available online at
<http://mozilla.org/MPL/2.0/>.  For futher information about the MPL see <http://www.mozilla.org/MPL/2.0/FAQ.html>.

For more information about Constraint Handling Rules see: 
https://en.wikipedia.org/wiki/Constraint_Handling_Rules  


# GoCHR Interface to integrate the GoCHR-interpreter
# --------------------------------------------------

 
// Ceate a RuleStore
//------------------

func MakeRuleStore() *RuleStore

// Add one rule
// ------------

func (rs *RuleStore) AddRule(name string, keep []string, del []string, guard []string, body []string) error 

// Trace the wrokfow, 1,2, or 3 - to trace the workflow of the CHR-interpreter
// -----------------

var CHRtrace int = 0 

// Start the inference engine with goals
// --------------------------

func (rs *RuleStore) Infer(goals []string, max int) (b bool, store []string, err error)

// Results of the inference engine:
// -------
// b == false && err == nil ==> result: FALSE
// b == true && store == []string{} ==> result: TRUE // empty store
// b == true && store == []string{term1, term2, term3, ...} ==> result: term1, term2, term3, ...
// b == false && err != nil ==> Error: err

# List of the built-in constraints to use in guard and body
# ---------------------------------------------------------
Constants
---------
true  (the constraint that always holds)
false  (the constraint that never holds, and is used to signal failure)

Operators
---------
Following operators from the Go programming language (see: https://golang.org/ref/spec#Operators )
Precedence  Operator
    6         unary operators +, -, !, ^, ¬ 
    5         *, /, %, div, mod, &, &^, <<, >>
    4        +, -, ^, or 
    3        ==, !=, <, <=, >, >= and =< (only for Prolog-like)
    2        &&
    1        ||

The operator | will be used as list-operator, as in [a|B]

# Example 1
# ---------
 
CHR-Rules: 
    Sum01 @ sum([], S) <=> S == 0 . 
    Sum02 @ sum([X|Xs], S) <=> sum(Xs, S2), S == X + S2.
    sum([1,2,3,4,5,6,7,8,9,10], S) // Goal 1
    #result: S == 55 .
    sum([X,2,3], 6). // Goal 2
    #result: X == 1 .
    sum([1,X,3], 6). // Goal 3
    #result: X == 2 .

In Go: 
    rs := MakeRuleStore() 
	keep := []string{} 
	del := []string{"sum([], S)"} 
	guard := []string{} 
	body := []string{"S == 0"} 
	err := rs.AddRule("Sum01", keep, del, guard, body) 
	if err != nil { 
		panic(err) 
	} 
	keep = []string{} 
	del = []string{"sum([X|Xs], S)"} 
	guard = []string{} 
	body = []string{"sum(Xs, S2)", "S == X + S2"} 
	err = rs.AddRule("Sum02", keep, del, guard, body) 
	if err != nil { 
		panic(err) 
	} 
	CHRtrace = 0 // trace off 
	rBool, rList, err := rs.Infer([]string{"sum([1,2,3,4,5,6,7,8,9,10], S)"}, 100000) 
	if err != nil { 
		panic(err)  
	} 
	fmt.Printf("\nresult: %v = %v \n", rBool, rList) 

	rBool, rList, err := rs.Infer([]string{"sum([X,2,3], 6)"}, 100000) 
	if err != nil { 
		panic(err)  
	} 
	fmt.Printf("\nresult: %v = %v \n", rBool, rList) 

	rBool, rList, err := rs.Infer([]string{"sum([1,X,3], 6)"}, 100000) 
	if err != nil { 
		panic(err)  
	} 
	fmt.Printf("\nresult: %v = %v \n", rBool, rList) 

# Example 2
# ---------
	
	
	prime01 @ prime(N) ==> N>2 | prime(N-1).
	prime02 @ prime(A) | prime(B) <=> B > A, B mod A == 0 | true.
	prime(20).
	#result: prime(19), prime(17), prime(13), prime(11), prime(7), prime(5), prime(3), prime(2).

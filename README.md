These are the source files of Version 2.1 of the
Constraint Handling Rule system, written in the Go programming language.

This source code is subject to the terms of the Mozilla Public
License, version 2.0 (MPL-2.0). If a copy of the MPL was not
distributed with this software, it is also available online at
 &lt;http://mozilla.org/MPL/2.0/>.  For futher information about the MPL see  &lt;http://www.mozilla.org/MPL/2.0/FAQ.html>.

# Syntax CHR-rules

[ &lt;rulename> '**@**']  &lt;keep-heads> '**==>**' [ &lt;guards> '**|**']  &lt;body> '**.**'

[ &lt;rulename> '**@**']  &lt;keep-heads> '**\**'  &lt;del-heads> '** &lt;=>**' [ &lt;guards> '**|**']  &lt;body>'**.**'

[ &lt;rulename> '**@**']  &lt;del-heads> '** &lt;=>**' [ &lt;guards> '**|**']  &lt;body>'**.**'

// goals

 &lt;predicates> '**.**'

// test

'**#result:**'  &lt;expected d  predicates> '**.**'

# Example

gcd01@ gcd(0)  &lt;=> true .

// logarithmic complexity

gcd02@ gcd(N) \ gcd(M)  &lt;=> N  &lt;= M, L := M mod N | gcd(L).

gcd(94017), gcd(1155), gcd(2035).

More example see: GoCHR/example

For more information about Constraint Handling Rules see: 
https://en.wikipedia.org/wiki/Constraint_Handling_Rules  
# Use GoCHR 

usage: gochr eval   [-o output-file] [input-file]

       gochr trace [-o output-file][input-file]

Evaluates/ Trace the evaluation of Constraint Handling Rules and prints the relult.

If no input-file is specified, input is read from stdin.

The -o flag specifies the output file name. If the -o flag is not used,
output goes to stdout.


# GoCHR Interface to integrate the GoCHR-interpreter

```go
// Create a RuleStore
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
```

# List of the built-in constraints to use in guard and body

## Constants   

true  (the constraint that always holds)    
false  (the constraint that never holds, and is used to signal failure)

## Operators

The following operators are from the Go programming language (see:  &lt;https://golang.org/ref/spec#Operators>)

Precedence | Operator    
---------- | --------
6   |      unary operators +, -, !, ^, ¬ 
5   |     *, /, %, div, mod, &, &^,  &lt; &lt;, >>
4   |    +, -, ^, or 
3   |    ==, !=,  &lt;,  &lt;=, >, >= and = &lt; (only for Prolog-like)
2   |     &&
1   |    ||

The operator | will be used as list-operator, as in [a|B]

# Example 1
 
## CHR-Rules

```prolog
    Sum01 @ sum([], S)  &lt;=> S == 0 . 
    Sum02 @ sum([X|Xs], S)  &lt;=> sum(Xs, S2), S == X + S2.
    sum([1,2,3,4,5,6,7,8,9,10], S) // Goal 1
    #result: S == 55 .
    sum([X,2,3], 6). // Goal 2
    #result: X == 1 .
    sum([1,X,3], 6). // Goal 3
    #result: X == 2 .
```

# In Go

```go
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
```

# Example 2

```prolog	
	prime01 @ prime(N) ==> N>2 | prime(N-1).
	prime02 @ prime(A) | prime(B)  &lt;=> B > A, B mod A == 0 | true.
	prime(20).
	#result: prime(19), prime(17), prime(13), prime(11), prime(7), prime(5), prime(3), prime(2).
```

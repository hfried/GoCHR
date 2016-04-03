// Copyright © 2016 The Carneades Authors
// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL
// was not distributed with this file, You can obtain one
// at http://mozilla.org/MPL/2.0/.

// parse Constraint Handling Rules

package chr

import (
	"fmt"
	. "github.com/hfried/GoCHR/src/engine/parser"
	. "github.com/hfried/GoCHR/src/engine/terms"
	"os"
	"strings"
	sc "text/scanner"
)

type parseType int

const (
	ParseHead parseType = iota
	ParseBI
	ParseGoal     // CHR and Built-In
	ParseRuleBody // Chr, Built-In and Variable
)

func CHRerr(format string, a ...interface{}) {
	fmt.Fprintln(os.Stderr, format, a)
}

func toClist(l Term) (CList, bool) {
	cl := CList{}
	if l.Type() != ListType {
		return cl, false
	}
	for _, t1 := range l.(List) {
		if t1.Type() != CompoundType {
			return cl, false
		}
		t2 := t1.(Compound)
		t2.EMap = &EnvMap{}
		cl = append(cl, &t2)
	}
	return cl, true
}

// parse CHR-rules and goals from string src
// CHR-rules:
//
// [<rulename>] '@' <keep-heads> '==>' [<guards> '|'] <body> '.'
// [<rulename>] '@' <keep-heads> '/' <del-heads> '<=>' [<guards> '|'] <body>'.'
// [<rulename>] '@' <del-heads> '<=>' [<guards> '|'] <body>'.'
//
// goals
// <predicates> '.'
func ParseStringCHRRulesGoals(src string) (ok bool) {
	// src is the input that we want to tokenize.
	// var s sc.Scanner
	var s sc.Scanner
	// Initialize the scanner.
	s.Init(strings.NewReader(src))

	s.Error = Err

	ok = parseRules(&s)
	return
}

func parseRules(s *sc.Scanner) (ok bool) {
	var t Term
	InitStore()

	nameNr := 1
	tok := s.Scan()

	TraceHeadln(4, 4, " parse rule tok: ", Tok2str(tok))
	if tok == sc.EOF {
		Err(s, " Empty input")
		return false
	}

	for tok != sc.EOF {
		tok1 := s.Peek()
		TraceHeadln(4, 4, " in loop parse rule tok: ", Tok2str(tok), ", tok1: [", Tok2str(tok1), "]")
		switch tok {
		case sc.Ident:
			t, tok, ok = Factor_name(s.TokenText(), s, s.Scan())
			if !ok {
				return ok
			}
			if tok == '@' {
				tok, ok = parseKeepHead(s, s.Scan(), t.String())
			} else {
				tok, ok = parseKeepHead1(s, tok, fmt.Sprintf("(%d)", nameNr), t)
				nameNr++
			}
		default:
			Err(s, fmt.Sprintf("Missing a rule-name or a predicate-name at the beginning (not \"%v\")", Tok2str(tok)))
			return false
		}

	}
	return true
}

func parseKeepHead(s *sc.Scanner, tok rune, name string) (rune, bool) {

	// ParseGoal - it is not clear, a goal-list or a head-list
	TraceHeadln(4, 4, " parse Keep Head:", name, " tok: ", Tok2str(tok))

	if tok != sc.Ident {
		Err(s, fmt.Sprintf("Missing predicate-name in rule: %s (not \"%v\")", name, Tok2str(tok)))
		return tok, false
	}
	t, tok, ok := Factor_name(s.TokenText(), s, s.Scan())
	if !ok {
		return tok, ok
	}

	return parseKeepHead1(s, tok, name, t)
}

func parseKeepHead1(s *sc.Scanner, tok rune, name string, t Term) (tok1 rune, ok bool) {

	if t.Type() != CompoundType {
		Err(s, fmt.Sprintf("Missing a predicate in rule %s (not %s)", name, t.String()))
		return tok, false
	}
	keepList := List{t}

	for tok == ',' {
		tok = s.Scan()
		if tok != sc.Ident {
			Err(s, fmt.Sprintf("Missing predicate-name in rule %s (not \"%v\")", name, Tok2str(tok)))
			return tok, false
		}
		t, tok, ok = Factor_name(s.TokenText(), s, s.Scan())
		if !ok {
			return tok, ok
		}
		keepList = append(keepList, t)
	}

	if tok == '.' {
		// Goals-List
		cGoalList, ok := prove2Clist(ParseGoal, name, keepList)
		if !ok {
			return tok, false
		}
		for _, g := range cGoalList {
			addRefConstraintToStore(g)
		}
		return s.Scan(), true
	}

	// keep- or del-head
	cKeepList, ok := prove2Clist(ParseHead, name, keepList)
	if !ok {
		return tok, false
	}
	var delList Term
	switch tok {
	case '\\', '|':
		delList, tok, ok = parseDelHead(s, s.Scan())

		cDelList, ok := prove2Clist(ParseHead, name, delList)
		if !ok {
			return tok, false
		}
		if tok != '<' {
			Err(s, fmt.Sprintf(" '<' in '<=>' excpected, not: %s", Tok2str(tok)))
			return tok, false
		}
		tok = s.Scan()
		if tok != '=' {
			Err(s, fmt.Sprintf(" '=' in '<=>' excpected, not: %s", Tok2str(tok)))
			return tok, false
		}
		tok = s.Scan()
		if tok != '>' {
			Err(s, fmt.Sprintf(" '>' in '<=>' excpected, not: %s", Tok2str(tok)))
			return tok, false
		}
		return parseGuardHead(s, s.Scan(), name, cKeepList, cDelList)

	case '<':
		tok = s.Scan()
		if tok != '=' {
			Err(s, fmt.Sprintf(" '=' in '<=>' excpected, not: %s", Tok2str(tok)))
			return tok, false
		}
		tok = s.Scan()
		if tok != '>' {
			Err(s, fmt.Sprintf(" '>' in '<=>' excpected, not: %s", Tok2str(tok)))
			return tok, false
		}
		// the scaned keep-list is the del-list
		return parseGuardHead(s, s.Scan(), name, nil, cKeepList)
	case '=':
		tok = s.Scan()
		if tok != '=' {
			Err(s, fmt.Sprintf(" '=' in '<=>' excpected, not: %s", Tok2str(tok)))
			return tok, false
		}
		tok = s.Scan()
		if tok != '>' {
			Err(s, fmt.Sprintf(" '>' in '<=>' excpected, not: %s", Tok2str(tok)))
			return tok, false
		}
		return parseGuardHead(s, s.Scan(), name, cKeepList, nil)
	default:
		Err(s, fmt.Sprintf(" unexcpected token: %s, excpect in head-rule '\\', '<=>' or '==>'", Tok2str(tok)))
	}

	return tok, false
}

func parseDelHead(s *sc.Scanner, tok rune) (delList List, tok1 rune, ok bool) {
	var t Term
	TraceHeadln(4, 4, "parse Del-Head tok[", Tok2str(tok))
	delList = List{}
	if tok != sc.Ident {
		Err(s, fmt.Sprintf("Missing predicate-name (not \"%v\")", Tok2str(tok)))
		return delList, tok, false
	}
	t, tok1, ok = Factor_name(s.TokenText(), s, s.Scan())
	if !ok {
		return
	}
	delList = append(delList, t)
	for tok1 == ',' {
		tok = s.Scan()
		if tok != sc.Ident {
			Err(s, fmt.Sprintf("Missing predicate-name (not \"%v\")", Tok2str(tok)))
			return delList, tok, false
		}
		t, tok1, ok = Factor_name(s.TokenText(), s, s.Scan())
		if !ok {
			return
		}
		delList = append(delList, t)
	}
	return
}

func parseGuardHead(s *sc.Scanner, tok rune, name string, cKeepList, cDelList CList) (tok1 rune, ok bool) {
	// ParseRuleGoal - it is no clear, if it a guard or body
	bodyList, tok, ok := parseConstraints1(ParseRuleBody, s, tok)
	TraceHead(4, 4, " parseGuardHead(1): ", bodyList, ", tok: '", Tok2str(tok), "'")
	cGuardList := CList{}
	if tok == '.' {
		tok = s.Scan()
	}
	if tok == '|' {
		cGuardList, ok = prove2Clist(ParseBI, name, bodyList)
		tok = s.Scan()
		bodyList, tok, ok = parseConstraints1(ParseRuleBody, s, tok)
		TraceHead(4, 4, " parseBodyHead(2): ", bodyList, ", tok: '", Tok2str(tok), "'")
		if !ok {
			return tok, false
		}
		if tok != '.' {
			Err(s, fmt.Sprintf(" After rule %s a '.' exspected, not a %s", name, Tok2str(tok)))
		}
		tok = s.Scan()

	}

	CHRruleStore = append(CHRruleStore, &chrRule{name: name, id: nextRuleId,
		delHead:  cDelList,
		keepHead: cKeepList,
		guard:    cGuardList,
		body:     bodyList.(List)})
	nextRuleId++

	return tok, true
}

func addChrRule(name string, keepList, delList, guardList, bodyList Term) bool {

	cKeepList, ok := prove2Clist(ParseHead, name, keepList)
	if !ok {
		return ok
	}
	//		return errors.New(fmt.SprintTok2str("Convert Keep-Head in rule %s failed: %s\n", name, keepList))

	//	if delList.Type() != ListType {
	//		return errors.New(fmt.Sprintf("DEl-Head in rule %s must be a List, not:  %s\n", name, delList))
	//	}
	cDelList, ok := prove2Clist(ParseHead, name, delList)
	if !ok {
		return ok
	}
	// 		return errors.New(fmt.Sprintf("Convert DEl-Head in rule %s failed: %s\n", name, delList))

	//	if guardList.Type() != ListType {
	//		return errors.New(fmt.Sprintf("GUARD in rule %s must be a List, not:  %s (%v)\n", name, guardList, ok))
	//	}
	cGuardList, ok := prove2Clist(ParseBI, name, guardList)
	if !ok {
		return ok
	}
	//		return errors.New(fmt.Sprintf("Convert GUARD in rule %s failed: %s\n", name, guardList))

	// bodyList, err = prove2BodyList(bodyList)

	//	if bodyList.Type() != ListType {
	//		return errors.New(fmt.Sprintf("BODY in rule %s must be a List, not:  %s\n", name, bodyList))
	//	}

	CHRruleStore = append(CHRruleStore, &chrRule{name: name, id: nextRuleId,
		delHead:  cDelList,
		keepHead: cKeepList,
		guard:    cGuardList,
		body:     bodyList.(List)})
	nextRuleId++
	return true
}

func addStringGoals(goals string) bool {
	goalList, ok := ParseGoalString(goals)
	if !ok || goalList.Type() != ListType {
		CHRerr("Scan GOAL-List failed: %s\n", goalList)
		return false
	}
	for _, g := range goalList.(List) {
		if g.Type() == CompoundType {
			addConstraintToStore(g.(Compound))
		} else {
			CHRerr(" GOAL is not a predicate: %s\n", g)
			return false
		}

	}
	return true
}

func ParseHeadString(src string) (result Term, ok bool) {
	// src is the input that we want to tokenize.
	var s sc.Scanner
	// var s *sc.Scanner
	// Initialize the scanner.
	s.Init(strings.NewReader(src))
	s.Error = Err

	result, _, ok = parseConstraints(ParseHead, &s)
	return
}

func ParseBIString(src string) (result Term, ok bool) {
	// src is the input that we want to tokenize.
	var s sc.Scanner
	// var s *sc.Scanner
	// Initialize the scanner.
	s.Init(strings.NewReader(src))
	s.Error = Err

	result, _, ok = parseConstraints(ParseBI, &s)
	return
}

func ParseGoalString(src string) (result Term, ok bool) {
	// src is the input that we want to tokenize.
	var s sc.Scanner
	// var s *sc.Scanner
	// Initialize the scanner.
	s.Init(strings.NewReader(src))
	s.Error = Err

	result, _, ok = parseConstraints(ParseGoal, &s)
	return
}

func ParseRuleBodyString(src string) (result Term, ok bool) {
	// src is the input that we want to tokenize.
	var s sc.Scanner
	// var s *sc.Scanner
	// Initialize the scanner.
	s.Init(strings.NewReader(src))
	s.Error = Err

	result, _, ok = parseConstraints(ParseRuleBody, &s)
	return
}

func prove2Clist(ty parseType, name string, t Term) (cl CList, ok bool) {
	// ty == ParseCHR, ParseBI, ParseGoal-CHR and Built-In,
	// no: ParseRuleGoal-Chr, Built-In and Variable
	cl = CList{}
	switch t.Type() {
	case AtomType:
		switch ty {
		case ParseHead:
			CHRerr(" unexpected atom ", t, " in head of rule ", name)
			return cl, false
		case ParseBI:
			CHRerr(" unexpected atom ", t, " in guard of rule ", name)
			return cl, false
		case ParseGoal:
			CHRerr(" unexpected atom ", t, " in goal-list ")
			return cl, false
		}
	case BoolType:
		switch ty {
		case ParseHead:
			CHRerr(" unexpected boolean ", t, " in head of rule ", name)
			return cl, false
		case ParseBI:
			CHRerr(" unexpected boolean ", t, " in guard of rule ", name)
			return cl, false
		case ParseGoal:
			CHRerr(" unexpected boolean ", t, " in goal-list ")
			return cl, false
		}
	case IntType:
		switch ty {
		case ParseHead:
			CHRerr(" unexpected integer ", t, " in head of rule ", name)
			return cl, false
		case ParseBI:
			CHRerr(" unexpected integer ", t, " in guard of rule ", name)
			return cl, false
		case ParseGoal:
			CHRerr(" unexpected integer ", t, " in goal-list ")
			return cl, false
		}
	case FloatType:
		switch ty {
		case ParseHead:
			CHRerr(" unexpected float-number ", t, " in head of rule ", name)
			return cl, false
		case ParseBI:
			CHRerr(" unexpected float-number ", t, " in guard of rule ", name)
			return cl, false
		case ParseGoal:
			CHRerr(" unexpected float-number ", t, " in goal-list ")
			return cl, false
		}
	case StringType:
		switch ty {
		case ParseHead:
			CHRerr(" unexpected string ", t, " in head of rule ", name)
			return cl, false
		case ParseBI:
			CHRerr(" unexpected string ", t, " in guard of rule ", name)
			return cl, false
		case ParseGoal:
			CHRerr(" unexpected string ", t, " in goal-list ")
			return cl, false
		}
	case CompoundType:
		comp := t.(Compound)
		switch ty {
		case ParseHead: // CHR, no Build-In
			if comp.Prio != 0 {
				CHRerr(" unexpected Build-In predicate ", t, " in head of rule ", name)
				return cl, false
			}
			comp.EMap = &EnvMap{}
			cl = append(cl, &comp)
			return cl, true
		case ParseBI: // only Build-In
			if comp.Prio == 0 {
				CHRerr(" unexpected CHR predicate ", t, " in guard of rule ", name)
				return cl, false
			}
			cl = append(cl, &comp)
			return cl, true
		case ParseGoal: // both
			cl = append(cl, &comp)
			return cl, true
		}
	case ListType:

		for _, t1 := range t.(List) {
			if t1.Type() != CompoundType {
				return prove2Clist(ty, name, t1)
			}
			t2 := t1.(Compound)
			if ty == ParseHead {
				t2.EMap = &EnvMap{}
			}
			cl = append(cl, &t2)
		}
		return cl, true

	case VariableType:
		switch ty {
		case ParseHead:
			CHRerr(" unexpected variable ", t, " in head of rule ", name)
			return cl, false
		case ParseBI:
			CHRerr(" unexpected variable ", t, " in guard of rule ", name)
			return cl, false
		case ParseGoal:
			CHRerr(" unexpected variable ", t, " in goal-list ")
			return cl, false
		}
	}
	return nil, false
}

func parseConstraints(ty parseType, s *sc.Scanner) (t Term, tok rune, ok bool) {
	TraceHeadln(4, 4, " parse constraints ")
	return parseConstraints1(ty, s, s.Scan())
}

func parseConstraints1(ty parseType, s *sc.Scanner, tok1 rune) (t Term, tok rune, ok bool) {
	TraceHeadln(4, 4, " parse constraints ", Tok2str(tok1))
	tok = tok1
	if tok == sc.EOF {
		return List{}, tok, true
	}

	t, tok, ok = Assignexpr(s, tok)
	if !ok {
		return
	}
	switch ty {
	case ParseHead:
		if t.Type() != CompoundType || t.(Compound).Prio != 0 {
			Err(s, fmt.Sprintf(" Not a CHR-predicate: %s ", t))
		}
	case ParseBI:
		if t.Type() != CompoundType || t.(Compound).Prio == 0 {
			Err(s, fmt.Sprintf(" Not a Built-in constraint: %s ", t))
		}
	case ParseGoal:
		if t.Type() != CompoundType {
			Err(s, fmt.Sprintf(" Not a CHR-predicate, a predicate or a build-in function: %s ", t))
		}
	case ParseRuleBody:
		if t.Type() != CompoundType && t.Type() != VariableType && t.Type() != BoolType {
			Err(s, fmt.Sprintf(" Not a CHR-predicate, a predicatea, a build-in function or variable: %s ", t))
		}
	}

	TraceHeadln(4, 4, "<-- assing-expression: term: ", t.String(), ", tok: '", Tok2str(tok), "' ok: ", ok)

	if tok == ',' {
		t1 := List{t}
		for tok == ',' {
			t, tok, ok = Assignexpr(s, s.Scan())
			if !ok {
				return t1, tok, false
			}
			switch ty {
			case ParseHead:
				if t.Type() != CompoundType || t.(Compound).Prio != 0 {
					Err(s, fmt.Sprintf(" Not a CHR-predicate: %s ", t))
				}
			case ParseBI:
				if t.Type() != CompoundType || t.(Compound).Prio == 0 {
					Err(s, fmt.Sprintf(" Not a Built-in constraint: %s ", t))
				}
			case ParseGoal:
				if t.Type() != CompoundType {
					Err(s, fmt.Sprintf(" Not a CHR-predicate, a predicate or a build-in function: %s ", t))
				}
			case ParseRuleBody:
				if t.Type() != CompoundType && t.Type() != VariableType && t.Type() != BoolType {
					Err(s, fmt.Sprintf(" Not a CHR-predicate, a predicatea, a build-in function or variable: %s ", t))
				}
			}

			TraceHeadln(4, 4, "<-- expression: term: %s tok: '%s' ok: %v \n", t.String(), Tok2str(tok), ok)

			t1 = append(t1, t)
		}
		t = t1
	} else {
		t = List{t}
	}
	return
}

func parseBIConstraint(s *sc.Scanner) (t Term, tok rune, ok bool) {

	TraceHeadln(4, 4, "--> readBIConstraint : ")

	tok = s.Scan()
	if tok == sc.EOF {
		return List{}, tok, true
	}

	t, tok, ok = Assignexpr(s, tok)

	TraceHeadln(4, 4, "<-- expression: term: %s tok: '%s' ok: %v \n", t.String(), Tok2str(tok), ok)

	if t.Type() != CompoundType || t.(Compound).Prio == 0 {
		Err(s, fmt.Sprintf(" Not a Built-in constraint: %s ", t))
	}

	if tok == sc.EOF || !ok {
		return
	}
	if tok == ',' {
		t1 := List{t}
		for tok == ',' {
			t, tok, ok = Assignexpr(s, s.Scan())

			TraceHeadln(4, 4, "<-- expression: term: %s tok: '%s' ok: %v ", t.String(), Tok2str(tok), ok)

			if t.Type() != CompoundType || t.(Compound).Prio == 0 {
				Err(s, fmt.Sprintf(" Not a Built-in constraint: %s ", t))
			}
			if !ok {
				return t1, tok, false
			}
			t1 = append(t1, t)
		}
		t = t1
	} else {
		t = List{t}
	}
	return
}

func printCHRStore() {
	first := true
	for _, aChr := range CHRstore {
		for _, con := range aChr.varArg {
			if !con.IsDeleted {
				if first {
					TraceHead(1, 0, "CHR-Store: [", con.String())
					first = false
				} else {
					Trace(1, ", ", con.String())
				}
			}
		}
	}
	if first {
		TraceHeadln(1, 0, "CHR-Store: []")
	} else {
		Traceln(1, "]")
	}

	first = true
	for _, aChr := range BuiltInStore {
		for _, con := range aChr.varArg {
			if !con.IsDeleted {
				if first {
					TraceHead(1, 0, "Built-In Store: [", con.String())
					first = false
				} else {
					Trace(1, ", ", con.String())
				}
			}
		}
	}
	if first {
		TraceHeadln(1, 0, "Built-In Store: []")
	} else {
		Traceln(1, "]")
	}

}

func chr2List() (l List) {
	l = List{}
	for _, aChr := range CHRstore {
		for _, con := range aChr.varArg {
			if !con.IsDeleted {
				l = append(l, *con)
			}
		}
	}
	return
}

func bi2List() (l List) {
	l = List{}
	for _, aChr := range BuiltInStore {
		for _, con := range aChr.varArg {
			if !con.IsDeleted {
				l = append(l, *con)
			}
		}
	}
	return
}

func chr2string() (str string) {
	first := true
	for _, aChr := range CHRstore {
		for _, con := range aChr.varArg {
			if !con.IsDeleted {
				if first {
					str = "[" + con.String()
					first = false
				} else {
					str = str + ", " + con.String()
				}
			}
		}
	}
	if first {
		str = "[]"
	} else {
		str = str + "]"
	}
	return
}

func bi2string() (str string) {
	first := true
	for _, aChr := range BuiltInStore {
		for _, con := range aChr.varArg {
			if !con.IsDeleted {
				if first {
					str = "[" + con.String()
					first = false
				} else {
					str = str + ", " + con.String()
				}
			}
		}
	}
	if first {
		str = "[]"
	} else {
		str = str + "]"
	}
	return
}

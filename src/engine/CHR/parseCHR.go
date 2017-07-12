// Copyright © 2016 The Carneades Authors
// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL
// was not distributed with this file, You can obtain one
// at http://mozilla.org/MPL/2.0/.

// parse Constraint Handling Rules

package chr

import (
	"errors"
	"fmt"
	"io"
	"os"
	"strings"
	sc "text/scanner"

	. "github.com/hfried/GoCHR/src/engine/parser"
	. "github.com/hfried/GoCHR/src/engine/terms"
)

type parseType int

const (
	ParseHead parseType = iota
	ParseBI
	ParseGoal     // CHR and Built-In
	ParseRuleBody // Chr, Built-In, true, false and Variable
)

func CHRerr(s *sc.Scanner, format string, a ...interface{}) {
	s.Error(s, fmt.Sprintf(format, a...))
}

func toClist(l Term) (CList, bool) {
	cl := CList{}
	if l.Type() != ListType {
		fmt.Printf("** in toClist - keine Liste\n")
		return cl, false
	}
	for _, t1 := range l.(List) {
		if t1.Type() != CompoundType {
			fmt.Printf("** in toClist kein  CompoundType %v \n", t1)
			return cl, false
		}
		t2 := t1.(Compound)
		//		t2.EMap = &EnvMap{}
		cl = append(cl, &t2)
	}
	return cl, true
}

func parseRule(name string, keep []string, del []string, guard []string, body []string) (cKeep, cDel, cGuard CList, cBody List, err error) {
	errMsgList := ""
	Errfunc := func(s *sc.Scanner, str string) {
		if str != "illegal char literal" {
			if errMsgList != "" {
				errMsgList += "\n"
			}
			errMsgList += fmt.Sprintf("*** Parse Error before[%v]: %s", s.Pos(), str)
		}
	}
	// var s sc.Scanner
	var s sc.Scanner
	// Initialize the scanner.
	keepList := List{}
	for _, src := range keep {
		s.Init(strings.NewReader(src))
		s.Error = Errfunc
		t, _, ok := parseConstraints(ParseHead, &s)
		if !ok {
			return nil, nil, nil, nil, errors.New(errMsgList)
		}
		switch t.Type() {
		case ListType:
			if len(keepList) == 0 {
				keepList = t.(List)
			} else {
				for _, e := range t.(List) {
					keepList = append(keepList, e)
				}
			}
		default:
			keepList = append(keepList, t)
		}
	}

	cKeep, ok := toClist(keepList)
	if !ok {
		return nil, nil, nil, nil, errors.New(fmt.Sprintf("Convert Keep-Head in rule %s failed: %s\n", name, keepList))
	}

	delList := List{}
	for _, src := range del {
		s.Init(strings.NewReader(src))
		s.Error = Errfunc
		t, _, ok := parseConstraints(ParseHead, &s)
		if !ok {
			return nil, nil, nil, nil, errors.New(errMsgList)
		}
		switch t.Type() {
		case ListType:
			if len(delList) == 0 {
				delList = t.(List)
			} else {
				for _, e := range t.(List) {
					delList = append(delList, e)
				}
			}
		default:
			delList = append(delList, t)
		}
	}

	cDel, ok = toClist(delList)

	if !ok {
		return nil, nil, nil, nil, errors.New(fmt.Sprintf("Convert DEL-Head in rule %s failed: %s\n", name, delList))
	}

	guardList := List{}
	for _, src := range guard {
		s.Init(strings.NewReader(src))
		s.Error = Errfunc
		t, _, ok := parseConstraints(ParseBI, &s)
		if !ok {
			return nil, nil, nil, nil, errors.New(errMsgList)
		}
		switch t.Type() {
		case ListType:
			if len(guardList) == 0 {
				guardList = t.(List)
			} else {
				for _, e := range t.(List) {
					guardList = append(guardList, e)
				}
			}
		default:
			guardList = append(guardList, t)
		}
	}

	cGuard, ok = toClist(guardList)

	if !ok {
		return nil, nil, nil, nil, errors.New(fmt.Sprintf("Convert GUARD in rule %s failed: %s\n", name, guardList))
	}

	cBody = List{}
	for _, src := range body {
		s.Init(strings.NewReader(src))
		s.Error = Errfunc
		t, _, ok := parseConstraints(ParseRuleBody, &s)
		if !ok {
			return nil, nil, nil, nil, errors.New(errMsgList)
		}
		switch t.Type() {
		case ListType:
			if len(cBody) == 0 {
				cBody = t.(List)
			} else {
				for _, e := range t.(List) {
					cBody = append(cBody, e)
				}
			}
		default:
			cBody = append(cBody, t)
		}
	}

	err = nil
	return
}

func parseGoals(goals []string) (CList, error) {
	var s sc.Scanner
	errMsgList := ""
	Errfunc := func(s *sc.Scanner, str string) {
		if str != "illegal char literal" {
			if errMsgList != "" {
				errMsgList += "\n"
			}
			errMsgList += fmt.Sprintf("*** Parse Error before[%v]: %s", s.Pos(), str)
		}
	}

	gList := List{}
	for _, src := range goals {
		s.Init(strings.NewReader(src))
		s.Error = Errfunc
		t, _, ok := parseConstraints(ParseGoal, &s)
		if !ok {
			fmt.Printf("parseConstraints Fehler !!!\n ")
			return nil, errors.New(errMsgList)
		}
		switch t.Type() {
		case ListType:
			if len(gList) == 0 {
				gList = t.(List)
			} else {
				for _, e := range t.(List) {
					gList = append(gList, e)
				}
			}
		default:
			gList = append(gList, t)
		}
	}
	cLiBody, ok := toClist(gList)
	if !ok {
		fmt.Printf("toClist Fehler !! \n")
		return nil, errors.New(errMsgList)
	}
	return cLiBody, nil
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
func (rs *RuleStore) ParseStringCHRRulesGoals(src string) (ok bool) {
	// src is the input that we want to tokenize.
	// var s sc.Scanner
	var s sc.Scanner
	// Initialize the scanner.
	s.Init(strings.NewReader(src))

	s.Error = Err

	ok = parseEvalRules(rs, &s)
	return
}

func (rs *RuleStore) ParseFileCHRRulesGoals(inFile io.Reader) (ok bool) {
	// src is the input that we want to tokenize.
	// var s sc.Scanner
	var s sc.Scanner
	// Initialize the scanner.
	s.Init(inFile)

	s.Error = Err
	ok = parseEvalRules(rs, &s)
	return
}

func parseEvalRules(rs *RuleStore, s *sc.Scanner) (ok bool) {
	var t Term
	var rule *chrRule
	var goals CList
	newGoals := false
	//        C O N I T I O N S         ||            R E S U L T
	// newGoals | new rules | new goals || RuleStore | CHRStore  | newGoals
	// ---------------------------------||----------------------------------
	//   false  |   true    |    -      || add rules |     -     |  false
	//   false  |     -     |   true    ||           | new goals |  true
	//                                               | & solve   |
	//   true   |   true    |    -      || new rules |     -     |  false
	//   true   |   false   |   true    ||           | new goals |  true
	//                                               | && solve  |

	InitStore(rs)

	nameNr := 1
	tok := s.Scan()

	TraceHeadln(4, 4, " parse rule tok: ", Tok2str(tok))
	if tok == sc.EOF {
		s.Error(s, " Empty input")
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
				tok, rule, goals, ok = parseKeepHead(rs, s, s.Scan(), t.String())
			} else {
				tok, rule, goals, ok = parseKeepHead1(rs, s, tok, fmt.Sprintf("(%d)", nameNr), t)
				nameNr++
			}
			TraceHeadln(4, 4, " after parseKeep, rule", rule, ", goals: ", goals, "ok: ", ok)
			if rule != nil {
				if newGoals {
					InitStore(rs)
					rule.eMap = &EnvMap{InBinding: rs.emptyBinding, OutBindings: map[int]*EnvMap{}}
					rs.CHRruleStore = []*chrRule{rule}
					addRuleToPred2rule(rs, rule)
					newGoals = false
				} else {
					rule.eMap = &EnvMap{InBinding: rs.emptyBinding, OutBindings: map[int]*EnvMap{}}
					rs.CHRruleStore = append(rs.CHRruleStore, rule)
					addRuleToPred2rule(rs, rule)
					rs.nextRuleId++
				}
			}
			if goals != nil {
				if newGoals {
					ClearCHRStore(rs)
				} else {
					newGoals = true
				}

				for _, g := range goals {
					addRefConstraintToStore(rs, g)
				}

				CHRsolver(rs, 100000)

				if CHRtrace == 0 {
					printCHRStore(rs, "Result: ")
					CHRtrace = 0
				} else {
					printCHRStore(rs, "Result: ")
				}
			}

		case '#':
			tok = s.Scan()
			if tok == sc.Ident {
				switch s.TokenText() {
				case "store", "result":
					tok = s.Scan()
					if tok == '=' || tok == ':' {
						tok1 = s.Peek()
						if tok1 == '=' {
							tok = s.Scan()
						}
					}
					// read and compare exspected result
					t, tok, ok = parseConstraints(ParseRuleBody, s)
					if !ok {
						Err1(s, " Scan exspected chr result failed: %s\n", t)
						return
					}
					if tok == '.' {
						tok = s.Scan()
					}
					compCHR := chr2List(rs)
					chrOK := EqualVarNameCList(compCHR, t)
					compBI := bi2List(rs)
					biOK := EqualVarNameCList(compBI, t)
					if chrOK || biOK {
						continue
					}
					if !chrOK && !biOK {
						if compCHR == nil {
							compCHR = compBI
						} else {
							if compBI != nil {
								for _, bi := range compBI {
									compCHR = append(compCHR, bi)
								}
							}
						}
						if t.Type() != ListType {
							Err1(s, " exspected chr result (no List): '%s' \n !=computed chr result: '%s'", t, compCHR)
							return false
						}
						lenCompCHR := len(compCHR)
						if lenCompCHR != len(t.(List)) || lenCompCHR == 0 {
							Err1(s, " exspected chr result: '%s' \n != len computed chr result: '%s'", t, compCHR)
							return false
						}
						vec := make([]bool, lenCompCHR)
						for _, c := range compCHR {
							found := false
							for i, e := range t.(List) {
								if !vec[i] && EqualVarName(c, e) {
									vec[i] = true
									found = true
									break
								}
							}
							if !found {
								Err1(s, " exspected chr result: '%s' \n != len computed chr result: '%s'", t, compCHR)
								return false
							}
						}
					}

				case "bistore":
					if tok == '=' {
						tok = s.Scan()
						if tok == '=' {
							tok = s.Scan()
						}
					}
					// read and compare exspected bi-result

				}
			}

		default:
			s.Error(s, fmt.Sprintf("Missing a rule-name, a predicate-name or a '#' at the beginning (not \"%v\")", Tok2str(tok)))
			return false
		}

	}
	return true
}

// parseKeepHead - it is not clear, a goal-list or a head-list
// - name: the name of the rule
func parseKeepHead(rs *RuleStore, s *sc.Scanner, tok rune, name string) (rune, *chrRule, CList, bool) {

	TraceHeadln(4, 4, " parse Keep Head:", name, " tok: ", Tok2str(tok))

	if tok != sc.Ident {
		s.Error(s, fmt.Sprintf("Missing predicate-name in rule: %s (not \"%v\")", name, Tok2str(tok)))
		return tok, nil, nil, false
	}
	t, tok, ok := Factor_name(s.TokenText(), s, s.Scan())
	if !ok {
		return tok, nil, nil, ok
	}

	return parseKeepHead1(rs, s, tok, name, t)
}

// parseKeepHead1 - it is not clear, a goal-list or a head-list
// - name: the name of the rule
// - t: the first predicate
func parseKeepHead1(rs *RuleStore, s *sc.Scanner, tok rune, name string, t Term) (tok1 rune, rule *chrRule, cl CList, ok bool) {

	if t.Type() != CompoundType && t.Type() != VariableType {
		//		if t.Type == VariableType {
		//			t = Compound{Functor: "0", Args: []Term{t}, Prio: 7}
		//		} else {
		s.Error(s, fmt.Sprintf("Missing a predicate in rule %s (not %s)", name, t.String()))
		return tok, nil, nil, false
	}
	//	}

	keepList := List{t}
	TraceHeadln(4, 4, " Head (in parseKeepHead1): ", t)
	for tok == ',' {
		tok = s.Scan()
		if tok != sc.Ident {
			s.Error(s, fmt.Sprintf("Missing predicate-name in rule %s (not \"%v\")", name, Tok2str(tok)))
			return tok, nil, nil, false
		}
		t, tok, ok = Factor_name(s.TokenText(), s, s.Scan())
		if !ok {
			return tok, nil, nil, ok
		}
		TraceHeadln(4, 4, " Head (in parseKeepHead1): ", t)
		keepList = append(keepList, t)
	}

	if tok == '.' {
		// Goals-List
		cGoalList, ok := prove2Clist(ParseGoal, name, keepList, s)
		if !ok {
			return tok, nil, nil, false
		}
		// a new Goal-List
		//		for _, g := range cGoalList {
		//			addRefConstraintToStore(g)
		//		}
		return s.Scan(), nil, cGoalList, true
	}

	// keep- or del-head
	cKeepList, ok := prove2Clist(ParseHead, name, keepList, s)
	if !ok {
		return tok, nil, nil, false
	}
	var delList Term
	switch tok {
	case '\\', '|':
		delList, tok, ok = parseDelHead(s, s.Scan())

		cDelList, ok := prove2Clist(ParseHead, name, delList, s)
		if !ok {
			return tok, nil, nil, false
		}
		if tok != '<' {
			s.Error(s, fmt.Sprintf(" '<' in '<=>' excpected, not: %s", Tok2str(tok)))
			return tok, nil, nil, false
		}
		tok = s.Scan()
		if tok != '=' {
			s.Error(s, fmt.Sprintf(" '=' in '<=>' excpected, not: %s", Tok2str(tok)))
			return tok, nil, nil, false
		}
		tok = s.Scan()
		if tok != '>' {
			s.Error(s, fmt.Sprintf(" '>' in '<=>' excpected, not: %s", Tok2str(tok)))
			return tok, nil, nil, false
		}
		return parseGuardHead(rs, s, s.Scan(), name, cKeepList, cDelList)

	case '<':
		tok = s.Scan()
		if tok != '=' {
			s.Error(s, fmt.Sprintf(" '=' in '<=>' excpected, not: %s", Tok2str(tok)))
			return tok, nil, nil, false
		}
		tok = s.Scan()
		if tok != '>' {
			s.Error(s, fmt.Sprintf(" '>' in '<=>' excpected, not: %s", Tok2str(tok)))
			return tok, nil, nil, false
		}
		// the scaned keep-list is the del-list
		return parseGuardHead(rs, s, s.Scan(), name, nil, cKeepList)
	case '=':
		tok = s.Scan()
		if tok != '=' {
			s.Error(s, fmt.Sprintf(" second '=' in '==>' excpected, not: %s", Tok2str(tok)))
			return tok, nil, nil, false
		}
		tok = s.Scan()
		if tok != '>' {
			s.Error(s, fmt.Sprintf(" '>' in '==>' excpected, not: %s", Tok2str(tok)))
			return tok, nil, nil, false
		}
		return parseGuardHead(rs, s, s.Scan(), name, cKeepList, nil)
	default:
		s.Error(s, fmt.Sprintf(" unexcpected token: %s, excpect in head-rule '\\', '<=>' or '==>'", Tok2str(tok)))
	}

	return tok, nil, nil, false
}

func parseDelHead(s *sc.Scanner, tok rune) (delList List, tok1 rune, ok bool) {
	var t Term
	TraceHeadln(4, 4, "parse Del-Head tok[", Tok2str(tok))
	delList = List{}
	if tok != sc.Ident {
		s.Error(s, fmt.Sprintf("Missing predicate-name (not \"%v\")", Tok2str(tok)))
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
			s.Error(s, fmt.Sprintf("Missing predicate-name (not \"%v\")", Tok2str(tok)))
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

// parseGuardHead - it is no clear, if it a guard or body
func parseGuardHead(rs *RuleStore, s *sc.Scanner, tok rune, name string, cKeepList, cDelList CList) (tok1 rune, rule *chrRule, goals CList, ok bool) {

	bodyList, tok, ok := parseConstraints1(ParseRuleBody, s, tok)
	TraceHead(4, 4, " parseGuardHead(1): ", bodyList, ", tok: '", Tok2str(tok), "'")
	cGuardList := CList{}
	switch tok {
	case '.':
		tok = s.Scan()
	case '|':
		cGuardList, ok = prove2Clist(ParseBI, name, bodyList, s)
		tok = s.Scan()
		bodyList, tok, ok = parseConstraints1(ParseRuleBody, s, tok)
		TraceHead(4, 4, " parseBodyHead(2): ", bodyList, ", tok: '", Tok2str(tok), "'")
		if !ok {
			return tok, nil, nil, false
		}
		if tok != '.' {
			s.Error(s, fmt.Sprintf(" After rule %s a '.' exspected, not a %s", name, Tok2str(tok)))
		}
		tok = s.Scan()
	default:
		s.Error(s, fmt.Sprintf(" In rule %s a '|' (after guard) or '.' (after head) exspected, not a %s", name, Tok2str(tok)))
	}

	//	CHRruleStore = append(CHRruleStore, &chrRule{name: name, id: nextRuleId,
	//		delHead:  cDelList,
	//		keepHead: cKeepList,
	//		guard:    cGuardList,
	//		body:     bodyList.(List)})
	//	nextRuleId++

	return tok, &chrRule{name: name, id: rs.nextRuleId,
		delHead:  cDelList,
		keepHead: cKeepList,
		keepEnv:  makeKeepEnv(cKeepList),
		guard:    cGuardList,
		body:     bodyList.(List)}, nil, true
}

func addChrRule(rs *RuleStore, s *sc.Scanner, name string, keepList, delList, guardList, bodyList Term) bool {

	cKeepList, ok := prove2Clist(ParseHead, name, keepList, s)
	if !ok {
		return ok
	}
	//		return errors.New(fmt.SprintTok2str("Convert Keep-Head in rule %s failed: %s\n", name, keepList))

	//	if delList.Type() != ListType {
	//		return errors.New(fmt.Sprintf("DEl-Head in rule %s must be a List, not:  %s\n", name, delList))
	//	}
	cDelList, ok := prove2Clist(ParseHead, name, delList, s)
	if !ok {
		return ok
	}
	// 		return errors.New(fmt.Sprintf("Convert DEl-Head in rule %s failed: %s\n", name, delList))

	//	if guardList.Type() != ListType {
	//		return errors.New(fmt.Sprintf("GUARD in rule %s must be a List, not:  %s (%v)\n", name, guardList, ok))
	//	}
	cGuardList, ok := prove2Clist(ParseBI, name, guardList, s)
	if !ok {
		return ok
	}
	//		return errors.New(fmt.Sprintf("Convert GUARD in rule %s failed: %s\n", name, guardList))

	// bodyList, err = prove2BodyList(bodyList)

	//	if bodyList.Type() != ListType {
	//		return errors.New(fmt.Sprintf("BODY in rule %s must be a List, not:  %s\n", name, bodyList))
	//	}

	r := &chrRule{name: name, id: rs.nextRuleId,
		delHead:  cDelList,
		keepHead: cKeepList,
		keepEnv:  makeKeepEnv(cKeepList),
		guard:    cGuardList,
		body:     bodyList.(List),
		eMap:     &EnvMap{InBinding: rs.emptyBinding, OutBindings: map[int]*EnvMap{}},
		isOn:     false,
		wasOn:    true}
	rs.CHRruleStore = append(rs.CHRruleStore, r)
	TraceHeadln(3, 3, " OFF rule: ", name, " (Add CHR-Rule) ")
	addRuleToPred2rule(rs, r)
	rs.nextRuleId++
	return true
}

func addStringGoals(rs *RuleStore, s *sc.Scanner, goals string) bool {
	goalList, ok := ParseGoalString(goals)
	if !ok || goalList.Type() != ListType {
		CHRerr(s, "Scan GOAL-List failed: %s\n", goalList)
		return false
	}
	for _, g := range goalList.(List) {
		if g.Type() == CompoundType {
			addConstraintToStore(rs, g.(Compound))
		} else {
			CHRerr(s, " GOAL is not a predicate: %s\n", g)
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

//func NewQuery(goals string) bool {
//	ClearCHRStore()
//	if addStringGoals(goals) {
//		CHRsolver()
//		return true
//	}
//	return false
//}

func prove2Clist(ty parseType, name string, t Term, s *sc.Scanner) (cl CList, ok bool) {
	// ty == ParseCHR, ParseBI, ParseGoal-CHR and Built-In,
	// no: ParseRuleGoal-Chr, Built-In and Variable
	cl = CList{}
	switch t.Type() {
	case AtomType:
		switch ty {
		case ParseHead:
			CHRerr(s, " unexpected atom ", t, " in head of rule ", name)
			return cl, false
		case ParseBI:
			CHRerr(s, " unexpected atom ", t, " in guard of rule ", name)
			return cl, false
		case ParseGoal:
			CHRerr(s, " unexpected atom ", t, " in goal-list ")
			return cl, false
		}
	case BoolType:
		switch ty {
		case ParseHead:
			CHRerr(s, " unexpected boolean ", t, " in head of rule ", name)
			return cl, false
		case ParseBI:
			CHRerr(s, " unexpected boolean ", t, " in guard of rule ", name)
			return cl, false
		case ParseGoal:
			CHRerr(s, " unexpected boolean ", t, " in goal-list ")
			return cl, false
		}
	case IntType:
		switch ty {
		case ParseHead:
			CHRerr(s, " unexpected integer ", t, " in head of rule ", name)
			return cl, false
		case ParseBI:
			CHRerr(s, " unexpected integer ", t, " in guard of rule ", name)
			return cl, false
		case ParseGoal:
			CHRerr(s, " unexpected integer ", t, " in goal-list ")
			return cl, false
		}
	case FloatType:
		switch ty {
		case ParseHead:
			CHRerr(s, " unexpected float-number ", t, " in head of rule ", name)
			return cl, false
		case ParseBI:
			CHRerr(s, " unexpected float-number ", t, " in guard of rule ", name)
			return cl, false
		case ParseGoal:
			CHRerr(s, " unexpected float-number ", t, " in goal-list ")
			return cl, false
		}
	case StringType:
		switch ty {
		case ParseHead:
			CHRerr(s, " unexpected string ", t, " in head of rule ", name)
			return cl, false
		case ParseBI:
			CHRerr(s, " unexpected string ", t, " in guard of rule ", name)
			return cl, false
		case ParseGoal:
			CHRerr(s, " unexpected string ", t, " in goal-list ")
			return cl, false
		}
	case CompoundType:
		comp := t.(Compound)
		switch ty {
		case ParseHead: // CHR, no Build-In
			if comp.Prio != 0 {
				CHRerr(s, " unexpected Build-In predicate ", t, " in head of rule ", name)
				return cl, false
			}
			//			comp.EMap = &EnvMap{}
			cl = append(cl, &comp)
			return cl, true
		case ParseBI: // only Build-In
			if comp.Prio == 0 {
				CHRerr(s, " unexpected CHR predicate ", t, " in guard of rule ", name)
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
			if ty == ParseHead && t1.Type() == VariableType {
				t1 = Compound{Functor: "", Args: []Term{t1}}
			} else {
				if t1.Type() != CompoundType {
					return prove2Clist(ty, name, t1, s)
				}
			}
			t2 := t1.(Compound)

			//			if ty == ParseHead {
			//				t2.EMap = &EnvMap{}
			//			}
			cl = append(cl, &t2)
		}
		return cl, true

	case VariableType:
		switch ty {
		case ParseHead:
			CHRerr(s, " unexpected variable ", t, " in head of rule ", name)
			return cl, false
		case ParseBI:
			CHRerr(s, " unexpected variable ", t, " in guard of rule ", name)
			return cl, false
		case ParseGoal:
			CHRerr(s, " unexpected variable ", t, " in goal-list ")
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
			s.Error(s, fmt.Sprintf(" Not a CHR-predicate: %s ", t))
		}
	case ParseBI:
		if t.Type() != CompoundType || t.(Compound).Prio == 0 {
			s.Error(s, fmt.Sprintf(" Not a Built-in constraint: %s ", t))
		}
	case ParseGoal:
		if t.Type() != CompoundType {
			s.Error(s, fmt.Sprintf(" Not a CHR-predicate, a predicate or a build-in function: %s ", t))
		}
	case ParseRuleBody:
		if t.Type() != CompoundType && t.Type() != VariableType && t.Type() != BoolType {
			s.Error(s, fmt.Sprintf(" Not a CHR-predicate, a predicatea, a build-in function or variable: %s ", t))
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
					s.Error(s, fmt.Sprintf(" Not a CHR-predicate: %s ", t))
				}
			case ParseBI:
				if t.Type() != CompoundType || t.(Compound).Prio == 0 {
					s.Error(s, fmt.Sprintf(" Not a Built-in constraint: %s ", t))
				}
			case ParseGoal:
				if t.Type() != CompoundType {
					s.Error(s, fmt.Sprintf(" Not a CHR-predicate, a predicate or a build-in function: %s ", t))
				}
			case ParseRuleBody:
				if t.Type() != CompoundType && t.Type() != VariableType && t.Type() != BoolType {
					s.Error(s, fmt.Sprintf(" Not a CHR-predicate, a predicatea, a build-in function or variable: %s ", t))
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
		s.Error(s, fmt.Sprintf(" Not a Built-in constraint: %s ", t))
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
				s.Error(s, fmt.Sprintf(" Not a Built-in constraint: %s ", t))
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

func printCHRStore(rs *RuleStore, h string) {
	switch rs.Result {
	case REmpty:
		if h != "New goal:" {
			TraceHeadln(1, 0, h, " No rule fired (!)")
			return
		}
	case RFalse:
		TraceHeadln(1, 0, h, " false (!)")
		return
	case RTrue:
		TraceHeadln(1, 0, h, " true (!)")
		return
	}
	// default: Result == RStore
	first := true
	for _, aChr := range rs.CHRstore {
		for _, con := range aChr.varArg {
			if con != nil && !con.IsDeleted {
				if first {
					TraceHead(1, 0, h, " CHR-Store: [", con.String())
					first = false
				} else {
					Trace(1, ", ", con.String())
				}
			}
		}
		for _, con := range aChr.noArg {
			if con != nil && !con.IsDeleted {
				if first {
					TraceHead(1, 0, h, " CHR-Store: [", con.String())
					first = false
				} else {
					Trace(1, ", ", con.String())
				}
			}
		}
	}
	if first {
		TraceHeadln(1, 0, h, " CHR-Store: []")
	} else {
		Traceln(1, "]")
	}

	first = true
	for _, aChr := range rs.BuiltInStore {
		for _, con := range aChr.varArg {
			if con != nil && !con.IsDeleted {
				if first {
					TraceHead(1, 0, h, " Built-In Store: [", con.String())
					first = false
				} else {
					Trace(1, ", ", con.String())
				}
			}
		}
		for _, con := range aChr.noArg {
			if con != nil && !con.IsDeleted {
				if first {
					TraceHead(1, 0, h, " Built-In Store: [", con.String())
					first = false
				} else {
					Trace(1, ", ", con.String())
				}
			}
		}
	}
	if first {
		TraceHeadln(1, 0, h, " Built-In Store: []")
	} else {
		Traceln(1, "]")
	}

}

func WriteCHRStore(rs *RuleStore, out *os.File) {
	switch rs.Result {
	case REmpty:
		// if h != "New goal:" {
		fmt.Fprintf(out, "No rule fired (!)")
		return
		// }
	case RFalse:
		fmt.Fprintf(out, "false")
		return
	case RTrue:
		fmt.Fprintf(out, "true")
		return
	}
	// default: Result == RStore
	first := true
	for _, aChr := range rs.CHRstore {
		for _, con := range aChr.varArg {
			if con != nil && !con.IsDeleted {
				if first {
					fmt.Fprintf(out, "[%s", con.String())
					first = false
				} else {
					fmt.Fprintf(out, ", %s", con.String())
				}
			}
		}
		for _, con := range aChr.noArg {
			if con != nil && !con.IsDeleted {
				if first {
					fmt.Fprintf(out, "[%s", con.String())
					first = false
				} else {
					fmt.Fprintf(out, ", %s", con.String())
				}
			}
		}
	}
	if first {
		fmt.Fprintf(out, "[]\n")

	} else {
		fmt.Fprintf(out, "]\n")
	}

	first = true
	for _, aChr := range rs.BuiltInStore {
		for _, con := range aChr.varArg {
			if con != nil && !con.IsDeleted {
				if first {
					fmt.Fprintf(out, "[%s", con.String())
					first = false
				} else {
					fmt.Fprintf(out, ", %s", con.String())
				}
			}
		}
		for _, con := range aChr.noArg {
			if con != nil && !con.IsDeleted {
				if first {
					fmt.Fprintf(out, "[%s", con.String())
					first = false
				} else {
					fmt.Fprintf(out, ", %s", con.String())
				}
			}
		}
	}
	if first {
		fmt.Fprintf(out, "[]\n")
	} else {
		fmt.Fprintf(out, "]\n")
	}

}

func chr2CList(rs *RuleStore) (l CList) {
	l = CList{}
	if rs.Result != RStore {
		return
	}
	for _, aChr := range rs.CHRstore {
		for _, con := range aChr.varArg {
			if con != nil && !con.IsDeleted {
				l = append(l, con)
			}
		}
		for _, con := range aChr.noArg {
			if con != nil && !con.IsDeleted {
				l = append(l, con)
			}
		}
	}
	return
}

func bi2CList(rs *RuleStore) (l CList) {
	l = CList{}
	for _, aChr := range rs.BuiltInStore {
		for _, con := range aChr.varArg {
			if con != nil && !con.IsDeleted {
				l = append(l, con)
			}
		}
		for _, con := range aChr.noArg {
			if con != nil && !con.IsDeleted {
				l = append(l, con)
			}
		}
	}
	return
}

func chr2List(rs *RuleStore) (l List) {
	l = List{}
	if rs.Result != RStore {
		return
	}
	for _, aChr := range rs.CHRstore {
		for _, con := range aChr.varArg {
			if con != nil && !con.IsDeleted {
				l = append(l, *con)
			}
		}
		for _, con := range aChr.noArg {
			if con != nil && !con.IsDeleted {
				l = append(l, *con)
			}
		}
	}
	return
}

func bi2List(rs *RuleStore) (l List) {
	switch rs.Result {
	case REmpty:
		l = List{String("no rule fired")}
	case RFalse:
		l = List{Bool(false)}
	case RTrue:
		l = List{Bool(true)}
	default:

		l = List{}
		for _, aChr := range rs.BuiltInStore {
			for _, con := range aChr.varArg {
				if con != nil && !con.IsDeleted {
					l = append(l, *con)
				}
			}
		}
	}
	return
}

func chr2string(rs *RuleStore) (str string) {
	first := true
	for _, aChr := range rs.CHRstore {
		for _, con := range aChr.varArg {
			if con != nil && !con.IsDeleted {
				if first {
					str = "[" + con.String()
					first = false
				} else {
					str = str + ", " + con.String()
				}
			}
		}
		for _, con := range aChr.noArg {
			if con != nil && !con.IsDeleted {
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

func bi2string(rs *RuleStore) (str string) {
	first := true
	for _, aChr := range rs.BuiltInStore {
		for _, con := range aChr.varArg {
			if con != nil && !con.IsDeleted {
				if first {
					str = "[" + con.String()
					first = false
				} else {
					str = str + ", " + con.String()
				}
			}
		}
		for _, con := range aChr.noArg {
			if con != nil && !con.IsDeleted {
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

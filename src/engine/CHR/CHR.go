// Copyright © 2016 The Carneades Authors
// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL
// was not distributed with this file, You can obtain one
// at http://mozilla.org/MPL/2.0/.

// Constraint Handling Rules

package chr

import (
	//"fmt"
	. "github.com/hfried/GoCHR/src/engine/parser"
	. "github.com/hfried/GoCHR/src/engine/terms"
	"math/big"
)

// types

type argCHR struct {
	atomArg  map[string]CList
	boolArg  CList
	intArg   CList
	floatArg CList
	strArg   CList
	compArg  map[string]CList
	listArg  CList
	varArg   CList
	noArg    CList
}

type store map[string]*argCHR

type chrRule struct {
	name     string
	id       int
	his      history
	delHead  CList // removed constraints
	keepHead CList // kept constraint
	guard    CList // built-in constraint
	body     List  // add CHR and built-in constraint
}

var CHRruleStore []*chrRule

var QueryVars Vars

var QueryStore List

var CHRstore store

var BuiltInStore store

var nextRuleId int = 0
var emptyBinding Bindings

var RenameRuleVars *big.Int

var chrCounter *big.Int
var bigOne = big.NewInt(1)

// init, add and read CHR- and Build-In-store
// -----------------------------------------

func InitStore() {
	v := NewVariable("")
	emptyBinding = &BindEle{Var: v, T: nil, Next: nil}
	chrCounter = big.NewInt(0)
	nextRuleId = 0
	CHRruleStore = []*chrRule{}
	CHRstore = store{}
	BuiltInStore = store{}
	QueryStore = List{}
	QueryVars = Vars{}
}

func NewArgCHR() *argCHR {
	return &argCHR{atomArg: map[string]CList{},
		boolArg: CList{}, intArg: CList{}, floatArg: CList{}, strArg: CList{},
		compArg: map[string]CList{}, listArg: CList{}, varArg: CList{}, noArg: CList{}}
}

func addGoal1(g *Compound, s store) {
	aArg, ok := s[g.Functor]
	if !ok {
		aArg = NewArgCHR()
		s[g.Functor] = aArg
	}
	args := g.Args
	if len(args) == 0 {
		aArg.noArg = append(aArg.noArg, g)
		return
	}
	arg0 := args[0]
	switch arg0.Type() {
	case AtomType:
		cl, ok := aArg.atomArg[string(arg0.(Atom))]
		if !ok {
			cl = CList{}
		}
		aArg.atomArg[string(arg0.(Atom))] = append(cl, g)
	case BoolType:
		aArg.boolArg = append(aArg.boolArg, g)
	case IntType:
		aArg.intArg = append(aArg.intArg, g)
	case FloatType:
		aArg.floatArg = append(aArg.floatArg, g)
	case StringType:
		aArg.strArg = append(aArg.strArg, g)
	case CompoundType:
		cl, ok := aArg.compArg[arg0.(Compound).Functor]
		if !ok {
			cl = CList{}
		}
		aArg.compArg[arg0.(Compound).Functor] = append(cl, g)
	case ListType:
		aArg.listArg = append(aArg.listArg, g)
	}
	aArg.varArg = append(aArg.varArg, g) // a veriable match to all types
}

func addConstraintToStore(g Compound) {
	addRefConstraintToStore(&g)
}
func addRefConstraintToStore(g *Compound) {
	// TraceHeadln(3, 3, " a) Counter %v \n", chrCounter)
	g.Id = chrCounter
	chrCounter = new(big.Int).Add(chrCounter, bigOne)
	// TraceHeadln(3, 3, " b) Counter++ %v , Id: %v \n", chrCounter, g.Id)
	if g.Prio == 0 {
		addGoal1(g, CHRstore)
	} else {
		addGoal1(g, BuiltInStore)
	}
}

func readProperConstraintsFromCHR_Store(t *Compound, env Bindings) CList {
	argAtt, ok := CHRstore[t.Functor]
	if ok {
		return readProperConstraintsFromStore(t, argAtt, env)
	}
	return CList{}
}

func readProperConstraintsFromBI_Store(t *Compound, env Bindings) CList {
	argAtt, ok := BuiltInStore[t.Functor]
	if ok {
		return readProperConstraintsFromStore(t, argAtt, env)
	}
	return CList{}
}

func readProperConstraintsFromStore(t *Compound, aAtt *argCHR, env Bindings) CList {
	args := t.Args
	l := len(args)
	if l == 0 {
		return aAtt.noArg
	}
	arg0 := args[0]
	argTyp := arg0.Type()
	for argTyp == VariableType {
		t2, ok := GetBinding(arg0.(Variable), env)
		if ok {
			arg0 = t2
			argTyp = arg0.Type()
		} else {
			break
		}
	}
	switch arg0.Type() {
	case AtomType:
		cl, ok := aAtt.atomArg[string(arg0.(Atom))]
		if ok {
			return cl
		}
	case BoolType:
		return aAtt.boolArg
	case IntType:
		return aAtt.intArg
	case FloatType:
		return aAtt.floatArg
	case StringType:
		return aAtt.strArg
	case CompoundType:
		cl, ok := aAtt.compArg[arg0.(Compound).Functor]
		if ok {
			return cl
		}
	case ListType:
		return aAtt.listArg
	case VariableType:
		return aAtt.varArg
	}
	return CList{}
}

// old History

type history [][]*big.Int

// var History []idSequence

// OccurVars
// ---------

var CurVarCounter *big.Int

// CHR solver
// ----------

// Try all rules in 'CHRruleStore' with CHR-goals in CHR-store
// until no rule fired.
// CHRsolver used the trace- or no-trace function
func CHRsolver() {

	if CHRtrace != 0 {
		printCHRStore()
	}
	i := 0
	ruleFound := true
	for ruleFound, i = true, 0; ruleFound && i < 10000; i++ {
		// for ruleFound := true; ruleFound; {
		ruleFound = false
		for _, rule := range CHRruleStore {
			RenameRuleVars = <-Counter
			if CHRtrace != 0 {
				TraceHeadln(2, 1, "trial rule ", rule.name, "(ID: ", rule.id, ") @ ", rule.keepHead.String(),
					" \\ ", rule.delHead.String(), " <=> ", rule.guard.String(), " | ", rule.body.String(), ".")

				if TraceRuleFired(rule) {
					TraceHeadln(1, 1, "rule ", rule.name, " fired (id: ", rule.id, ")")
					ruleFound = true
					break
				}
				TraceHeadln(2, 1, "rule ", rule.name, " NOT fired (id: ", rule.id, ")")
			} else {
				if pRuleFired(rule) {
					ruleFound = true
					break
				}
			}

		}
		if ruleFound && CHRtrace != 0 {
			printCHRStore()
		}
	}
	if i == 10000 {
		TraceHeadln(0, 1, "!!! Time-out !!!")
	}
	if CHRtrace > 1 {
		printCHRStore()
	}
}

// prove whether rule fired
func pRuleFired(rule *chrRule) (ok bool) {
	headList := rule.delHead
	len_head := len(headList)
	if len_head != 0 {
		ok = unifyDelHead(rule, headList, 0, len_head, 0, nil)
		return ok
	}

	headList = rule.keepHead
	len_head = len(headList)
	if len_head == 0 {
		return false
	}

	ok = unifyKeepHead(rule, []*big.Int{}, headList, 0, len_head, 0, emptyBinding)
	return ok
}

// prove and trace whether rule fired
func TraceRuleFired(rule *chrRule) (ok bool) {
	headList := rule.delHead
	len_head := len(headList)
	if len_head != 0 {
		ok = traceUnifyDelHead(rule, headList, 0, len_head, 0, nil)
		return ok
	}

	headList = rule.keepHead
	len_head = len(headList)
	if len_head == 0 {
		return false
	}

	ok = traceUnifyKeepHead(rule, []*big.Int{}, headList, 0, len_head, 0, emptyBinding)
	return ok
}

// Try to unify the del-head 'it' from the 'headlist' ('nt'==len of 'headlist')
// with the 'ienv'-te environmen 'env'
// If unifying ok, call 'unifyKeepHead' or 'checkGuards'
func unifyDelHead(r *chrRule, headList CList, it int, nt int, ienv int, env Bindings) (ok bool) {
	var env2 Bindings
	var mark bool
	head := headList[it]
	chrList := readProperConstraintsFromCHR_Store(head, env)
	len_chr := len(chrList)
	if len_chr == 0 {
		return false
	}
	// begin check the next head
	lastDelHead := it+1 == nt
	lastHead := false
	if lastDelHead {
		// last del head
		headList = r.keepHead
		nt = len(headList)
		if nt == 0 {
			lastHead = true
		}
	}
	// End next check next head, if lastDelHead the headList == r.keephead
	// check in head stored environment map
	ie := 0
	len_ie := 0
	senv, ok := (*head.EMap)[ienv]
	if ok {
		len_ie = len(senv)
		if lastHead {
			ie = len_ie
		} else {
			if lastDelHead {
				for ; ie < len_ie; ie++ {
					env2 = senv[ie]
					if env2 != nil {
						chr := chrList[ie]
						mark = markCHR(chr)
						if mark {
							ok = unifyKeepHead(r, nil, headList, 0, nt, ie, env2)
							if ok {
								return ok
							}
							unmarkDelCHR(chr)
						}
					}
				}
			} else { // not a last Del-Head
				for ; ie < len_ie; ie++ {
					env2 = senv[ie]
					if env2 != nil {
						chr := chrList[ie]
						mark = markCHR(chr)
						if mark {
							ok = unifyDelHead(r, headList, it+1, nt, ie, env2)
							if ok {
								// not unmarkDelCHR(chr), markt == deleted
								return ok
							}
							unmarkDelCHR(chr)
						}

					}
				} // for ; ie < len_ie; ie++
			} // ! lastDelHead
		} // ! lastHead
	} else {
		senv = []Bindings{}
		(*head.EMap)[ienv] = senv
	}
	// End check in head stored environment map
	// normal head-check, start at ie (not at 0 !!)
	if lastHead {
		for ok, ic := false, ie; !ok && ic < len_chr; ic++ {
			chr := chrList[ic]
			env2, ok, mark = markCHRAndUnifyDelHead(r.id, head, chr, env)
			if ok {
				senv = append(senv, env2)
				ok = checkGuards(r, env2)
				if ok {
					(*head.EMap)[ienv] = senv
					return ok
				}
			} else {
				senv = append(senv, nil)
			}
			if mark {
				unmarkDelCHR(chr)
			}
		}
		(*head.EMap)[ienv] = senv
		return false
	}
	if lastDelHead {
		for ok, ic := false, ie; !ok && ic < len_chr; ic++ {
			chr := chrList[ic]
			env2, ok, mark = markCHRAndUnifyDelHead(r.id, head, chr, env)
			if ok {
				senv = append(senv, env2)

				ok = unifyKeepHead(r, nil, headList, 0, nt, ic, env2)
				if ok {
					(*head.EMap)[ienv] = senv
					return ok
				}
			} else {
				senv = append(senv, nil)
			}
			if mark {
				unmarkDelCHR(chr)
			}
		}
		(*head.EMap)[ienv] = senv
		return false
	}

	for ok, ic := false, ie; !ok && ic < len_chr; ic++ {

		chr := chrList[ic]

		env2, ok, mark = markCHRAndUnifyDelHead(r.id, head, chr, env) // mark chr and Unify, if fail unmark chr
		if ok {
			senv = append(senv, env2)
			ok = unifyDelHead(r, headList, it+1, nt, ic, env2)
			if ok {
				// not unmarkDelCHR(chr), markt == deleted
				(*head.EMap)[ienv] = senv
				return ok
			}
		} else {
			senv = append(senv, nil)
		}
		if mark {
			unmarkDelCHR(chr)
		}
	}
	(*head.EMap)[ienv] = senv
	return false
}

// Try to unify and trace the del-head 'it' from the 'headlist' ('nt'==len of 'headlist')
// with the 'ienv'-te environmen 'env'
// If unifying ok, call 'unifyKeepHead' or 'checkGuards'
func traceUnifyDelHead(r *chrRule, headList CList, it int, nt int, ienv int, env Bindings) (ok bool) {
	var env2 Bindings
	var mark bool
	head := headList[it]
	chrList := readProperConstraintsFromCHR_Store(head, env)
	TraceHead(3, 3, "unify Del-Head (", ienv, ") ", head, " with [")
	len_chr := len(chrList)
	if len_chr == 0 {
		Traceln(3, "]")
		return false
	}
	// begin trace
	first := true
	for _, c := range chrList {
		if !c.IsDeleted {
			if first {
				Trace(3, c)
				first = false
			} else {
				Trace(3, ", ", c)
			}
		}
	}
	Traceln(3, "]")
	// end trace
	// begin check the next head
	lastDelHead := it+1 == nt
	lastHead := false
	if lastDelHead {
		// last del head
		headList = r.keepHead
		nt = len(headList)
		if nt == 0 {
			lastHead = true
		}
	}
	// End next check next head, if lastDelHead the headList == r.keephead
	// check in head stored environment map
	ie := 0
	len_ie := 0
	senv, ok := (*head.EMap)[ienv]
	if ok {
		TraceEMap(4, 4, head)
		len_ie = len(senv)
		// trace
		TraceHead(4, 3, "unify Del-Head (", ienv, ") ", head, " Env: [")
		first = true
		for _, e := range senv {
			if first {
				first = false
			} else {
				Trace(4, ", ")
			}
			TraceEnv(4, e)
		}
		Traceln(4, "]")

		// End trace

		if lastHead {
			ie = len_ie
		} else {
			if lastDelHead {
				for ; ie < len_ie; ie++ {
					env2 = senv[ie]
					if env2 != nil {
						chr := chrList[ie]
						mark = markCHR(chr)
						if mark {
							ok = traceUnifyKeepHead(r, nil, headList, 0, nt, ie, env2)
							if ok {
								return ok
							}
							traceUnmarkDelCHR(chr)
						}
					}
				}
			} else { // not a last Del-Head
				for ; ie < len_ie; ie++ {
					env2 = senv[ie]
					if env2 != nil {
						chr := chrList[ie]
						mark = markCHR(chr)
						if mark {
							ok = traceUnifyDelHead(r, headList, it+1, nt, ie, env2)
							if ok {
								// not unmarkDelCHR(chr), markt == deleted
								return ok
							}
							traceUnmarkDelCHR(chr)
						}

					}
				} // for ; ie < len_ie; ie++
			} // ! lastDelHead
		} // ! lastHead
	} else {
		// head.EMap = &EnvMap{}
		TraceEMap(4, 4, head)
		senv = []Bindings{}
		(*head.EMap)[ienv] = senv
	}
	// End check in head stored environment map
	// normal head-check, start at ie (not at 0 !!)
	TraceHeadln(3, 3, "unify del-Head (", ienv, ") ", head, " from: ", ie, " < ", len_chr)
	if lastHead {
		for ok, ic := false, ie; !ok && ic < len_chr; ic++ {
			chr := chrList[ic]
			// env = lateRenameVars(env)
			env2, ok, mark = traceMarkCHRAndUnifyDelHead(r.id, head, chr, env)
			if ok {
				senv = append(senv, env2)
				// trace senv changes

				TraceHead(4, 3, "New environment ", "Head: ", head.String(), ", Env: (", ienv, ") [", ic, "], =")
				TraceEnv(4, env2)
				Traceln(4, "")

				ok = traceCheckGuards(r, env2)
				if ok {
					(*head.EMap)[ienv] = senv
					TraceEMap(4, 4, head)
					return ok
				}
			} else {
				senv = append(senv, nil)
			}
			if mark {
				traceUnmarkDelCHR(chr)
			}
		}
		(*head.EMap)[ienv] = senv
		TraceEMap(4, 4, head)
		return false
	}
	if lastDelHead {
		for ok, ic := false, ie; !ok && ic < len_chr; ic++ {
			chr := chrList[ic]
			// env = lateRenameVars(env)
			env2, ok, mark = traceMarkCHRAndUnifyDelHead(r.id, head, chr, env)
			if ok {
				senv = append(senv, env2)
				// trace senv changes

				TraceHead(4, 3, "New environment ", "Head: ", head.String(), ", Env: (", ienv, ") [", ic, "], =")
				TraceEnv(4, env2)
				Traceln(4, "")

				ok = traceUnifyKeepHead(r, nil, headList, 0, nt, ic, env2)
				if ok {
					(*head.EMap)[ienv] = senv
					TraceEMap(4, 4, head)
					return ok
				}
			} else {
				senv = append(senv, nil)
			}
			if mark {
				traceUnmarkDelCHR(chr)
			}
		}
		(*head.EMap)[ienv] = senv
		TraceEMap(4, 4, head)
		return false
	}

	for ok, ic := false, ie; !ok && ic < len_chr; ic++ {

		chr := chrList[ic]
		// env = lateRenameVars(env)  // ???
		env2, ok, mark = traceMarkCHRAndUnifyDelHead(r.id, head, chr, env) // mark chr and Unify, if fail unmark chr
		if ok {
			senv = append(senv, env2)
			// trace senv changes

			TraceHead(4, 3, "New environment ", "Head: ", head.String(), ", Env: (", ienv, ") [", ic, "], =")
			TraceEnv(4, env2)
			Traceln(4, "")
			ok = traceUnifyDelHead(r, headList, it+1, nt, ic, env2)
			if ok {
				// not unmarkDelCHR(chr), markt == deleted
				(*head.EMap)[ienv] = senv
				TraceEMap(4, 4, head)
				return ok
			}
		} else {
			senv = append(senv, nil)
		}
		if mark {
			traceUnmarkDelCHR(chr)
		}
	}
	(*head.EMap)[ienv] = senv
	TraceEMap(4, 4, head)
	return false
}

// mark chr - no other head-predicate can match that constraint
func markCHR(chr *Compound) bool {
	if chr.IsDeleted {
		return false
	}
	chr.IsDeleted = true
	return true
}

func traceMarkCHRAndUnifyDelHead(id int, head, chr *Compound, env Bindings) (env2 Bindings, ok bool, m bool) {
	// mark and unmark chr
	if chr.IsDeleted {
		return env, false, false
	}
	// TraceHeadln(3, 3, "     *** mark del %v, ID: %v\n", chr, chr.Id)
	chr.IsDeleted = true
	env2, ok = Unify(*head, *chr, env)
	if ok {
		TraceHead(3, 3, "Unify head ", head, " with CHR ", chr, " (Id: ", chr.Id, ") is ", ok, " (Binding: ")
		TraceEnv(3, env2)
		Traceln(3, ")")
	} else {
		TraceHead(4, 3, "Unify head ", head, " with mark CHR ", chr, " (Id: ", chr.Id, ") is ", ok, " (Binding: ")
		TraceEnv(4, env2)
		Traceln(4, ")")
	}
	return env2, ok, true
}

func markCHRAndUnifyDelHead(id int, head, chr *Compound, env Bindings) (env2 Bindings, ok bool, m bool) {
	// mark and unmark chr
	if chr.IsDeleted {
		return env, false, false
	}
	chr.IsDeleted = true
	env2, ok = Unify(*head, *chr, env)
	return env2, ok, true
}

func unmarkDelCHR(chr *Compound) {
	chr.IsDeleted = false
	return
}

func traceUnmarkDelCHR(chr *Compound) {
	chr.IsDeleted = false
	TraceHeadln(4, 3, "unmark del ", chr, ", ID: ", chr.Id)
	return
}

func traceMarkCHRAndUnifyKeepHead(id int, head, chr *Compound, env Bindings) (env2 Bindings, ok bool, m bool) {
	// mark and unmark chr

	if chr.IsDeleted {
		return env, false, false
	}
	// TraceHeadln(3, 3, "mark keep ",chr,", ID: ",chr.Id )
	chr.IsDeleted = true
	env2, ok = Unify(*head, *chr, env)
	if ok {
		TraceHead(3, 3, "Unify head ", head, " with CHR ", chr, " (Id: ", chr.Id, ") is ", ok, " (Binding: ")
		TraceEnv(3, env2)
		Traceln(3, ")")
	} else {
		TraceHead(4, 3, "Unify head ", head, " with mark CHR ", chr, " (Id: ", chr.Id, ") is ", ok, " (Binding: ")
		TraceEnv(4, env2)
		Traceln(4, ")")
	}
	return env2, ok, true
}

func markCHRAndUnifyKeepHead(id int, head, chr *Compound, env Bindings) (env2 Bindings, ok bool, m bool) {
	// mark and unmark chr

	if chr.IsDeleted {
		return env, false, false
	}
	chr.IsDeleted = true
	env2, ok = Unify(*head, *chr, env)
	return env2, ok, true
}

func traceUnmarkKeepCHR(chr *Compound) {
	chr.IsDeleted = false
	TraceHeadln(4, 3, "unmark keep ", chr, ", ID: ", chr.Id)
	return
}

func unmarkKeepCHR(chr *Compound) {
	chr.IsDeleted = false
	return
}

// Try to unify the keep-head 'it' from the 'headlist' ('nt'==len of 'headlist')
// with the 'ienv'-te environmen 'env'
// If unifying for all keep-heads ok, call 'checkGuards'
func unifyKeepHead(r *chrRule, his []*big.Int, headList CList, it int, nt int, ienv int, env Bindings) (ok bool) {
	var env2 Bindings
	var mark bool
	head := headList[it]
	chrList := readProperConstraintsFromCHR_Store(head, env)
	len_chr := len(chrList)
	if len_chr == 0 {
		return false
	}
	// begin check the next head
	lastKeepHead := it+1 == nt
	// End next check next head
	// check in head stored environment map
	ie := 0
	len_ie := 0
	senv, ok := (*head.EMap)[ienv]
	if ok {
		len_ie = len(senv)
		if lastKeepHead {
			ie = len_ie
		} else {
			for ; ie < len_ie; ie++ {
				env2 = senv[ie]
				if env2 != nil {
					chr := chrList[ie]
					mark = markCHR(chr)
					if mark {
						ok = unifyKeepHead(r, nil, headList, it+1, nt, ie, env2)
						if ok {
							unmarkKeepCHR(chr)
							return ok
						}
						unmarkKeepCHR(chr)
					}
				}
			}

		} // ! lastHead
	} else { // if !ok
		// head.EMap = &EnvMap{}
		senv = []Bindings{}
		(*head.EMap)[ienv] = senv
	}
	// End check in head stored environment map
	// normal head-check, start at ie (not at 0 !!)
	if lastKeepHead {
		for ok, ic := false, ie; !ok && ic < len_chr; ic++ {
			chr := chrList[ic]
			env2, ok, mark = markCHRAndUnifyKeepHead(r.id, head, chr, env)
			if ok {
				senv = append(senv, env2)
				ok = checkGuards(r, env2)
				if ok {
					unmarkKeepCHR(chr)
					(*head.EMap)[ienv] = senv
					return ok
				}
			} else {
				senv = append(senv, nil)
			}
			if mark {
				unmarkKeepCHR(chr)
			}
		}
		(*head.EMap)[ienv] = senv
		return false
	}

	for ok, ic := false, ie; !ok && ic < len_chr; ic++ {

		chr := chrList[ic]

		env2, ok, mark = markCHRAndUnifyKeepHead(r.id, head, chr, env) // mark chr and Unify, if fail unmark chr
		if ok {
			senv = append(senv, env2)

			ok = unifyKeepHead(r, nil, headList, it+1, nt, ic, env2)
			if ok {
				unmarkKeepCHR(chr)
				(*head.EMap)[ienv] = senv
				return ok
			}
		} else {
			senv = append(senv, nil)
		}
		if mark {
			unmarkDelCHR(chr)
		}
	}
	(*head.EMap)[ienv] = senv
	return false
}

// Try to unify and trace the keep-head 'it' from the 'headlist' ('nt'==len of 'headlist')
// with the 'ienv'-te environmen 'env'
// If unifying for all keep-heads ok, call 'checkGuards'
func traceUnifyKeepHead(r *chrRule, his []*big.Int, headList CList, it int, nt int, ienv int, env Bindings) (ok bool) {
	var env2 Bindings
	var mark bool
	head := headList[it]
	chrList := readProperConstraintsFromCHR_Store(head, env)
	TraceHead(4, 3, "unify keep-Head (", ienv, ") ", head, " with [")
	len_chr := len(chrList)
	if len_chr == 0 {
		Traceln(4, "] - empty chr")
		return false
	}
	// begin trace
	first := true
	for _, c := range chrList {
		if first {
			Trace(4, c)
			first = false
		} else {
			Trace(4, ", ", c)
		}
	}
	Traceln(4, "]")
	// end trace
	// begin check the next head

	lastKeepHead := it+1 == nt
	TraceHeadln(4, 4, " last keep head = ", lastKeepHead)

	// End next check next head
	// check in head stored environment map
	ie := 0
	len_ie := 0
	senv, ok := (*head.EMap)[ienv]
	if !ok {
		TraceHeadln(4, 4, " !!! head: ", head, " with no Emap[ ", ienv, " ]")
	}
	if ok {
		TraceEMap(4, 4, head)
		len_ie = len(senv)
		TraceHeadln(4, 4, " len env (", ienv, ") = ", len_ie)
		if lastKeepHead {
			ie = len_ie
			TraceHeadln(4, 4, " ie == len_ie == ", ie, " = ", len_ie)
		} else {
			// trace
			TraceHead(4, 3, "unify Keep-Head (", ienv, ") ", head, " Env: [")
			first = true
			for _, e := range senv {
				if first {
					first = false
				} else {
					Trace(4, ", ")
				}
				TraceEnv(4, e)
			}
			Traceln(4, "]")

			// End trace
			for ; ie < len_ie; ie++ {
				env2 = senv[ie]
				if env2 != nil {
					chr := chrList[ie]
					mark = markCHR(chr)
					TraceHeadln(4, 4, " mark keep chr:", chr.String(), " = ", mark)
					if mark {
						ok = traceUnifyKeepHead(r, nil, headList, it+1, nt, ie, env2)
						if ok {
							traceUnmarkKeepCHR(chr)
							return ok
						}
						traceUnmarkKeepCHR(chr)
					}
				}
			}

		} // ! lastHead
	} else { // if !ok
		// head.EMap = &EnvMap{}
		senv = []Bindings{}
		(*head.EMap)[ienv] = senv
	}
	// End check in head stored environment map
	// normal head-check, start at ie (not at 0 !!)
	TraceHeadln(4, 3, "unify keep-Head ", head, " from: ", ie, " < ", len_chr)
	if lastKeepHead {
		for ok, ic := false, ie; !ok && ic < len_chr; ic++ {
			chr := chrList[ic]
			env2, ok, mark = traceMarkCHRAndUnifyKeepHead(r.id, head, chr, env)
			if ok {
				senv = append(senv, env2)
				// trace senv changes

				TraceHead(4, 3, "New environment ", "Head: ", head.String(), ", Env (", ienv, ") [", ic, "], =")
				TraceEnv(4, env2)
				Traceln(4, "")

				ok = traceCheckGuards(r, env2)
				if ok {
					traceUnmarkKeepCHR(chr)
					(*head.EMap)[ienv] = senv
					TraceEMap(4, 4, head)
					return ok
				}
			} else {
				senv = append(senv, nil)
			}
			if mark {
				traceUnmarkKeepCHR(chr)
			}
		}
		(*head.EMap)[ienv] = senv
		TraceEMap(4, 4, head)
		return false
	}

	for ok, ic := false, ie; !ok && ic < len_chr; ic++ {

		chr := chrList[ic]

		env2, ok, mark = traceMarkCHRAndUnifyKeepHead(r.id, head, chr, env) // mark chr and Unify, if fail unmark chr
		if ok {
			senv = append(senv, env2)
			// trace senv changes

			TraceHead(4, 3, "New environment ", "Head: ", head.String(), ", Env: (", ienv, ") [", ic, "], =")
			TraceEnv(4, env2)
			Traceln(4, "")

			ok = traceUnifyKeepHead(r, nil, headList, it+1, nt, ic, env2)
			if ok {
				traceUnmarkKeepCHR(chr)
				(*head.EMap)[ienv] = senv
				TraceEMap(4, 4, head)
				return ok
			}
		} else {
			senv = append(senv, nil)
		}
		if mark {
			unmarkDelCHR(chr)
		}
	}
	(*head.EMap)[ienv] = senv
	TraceEMap(4, 4, head)
	return false
}

// check and trace guards of the rule r with the binding env
// if all guards are true, fire rule
func traceCheckGuards(r *chrRule, env Bindings) (ok bool) {
	for _, g := range r.guard {
		env2, ok := traceCheckGuard(g, env)
		if !ok {
			return false
		}
		env = env2
	}
	if traceFireRule(r, env) {
		return true
	}
	// dt do setFail
	return true
}

// check and trace a guard g with the binding env
// if guards are true, return the new binding (if ':=', '=' or 'is' guard)
func traceCheckGuard(g *Compound, env Bindings) (env2 Bindings, ok bool) {
	TraceHead(3, 3, "check guard: ", g.String())
	g1 := Substitute(*g, env).(Compound)
	Trace(3, ", subst: ", g1)
	if g.Functor == ":=" || g1.Functor == "is" || g1.Functor == "=" {
		if !(g1.Args[0].Type() == VariableType) {
			return env, false
		}
		a := Eval(g1.Args[1])
		env2 = AddBinding(g1.Args[0].(Variable), a, env)
		return env2, true
	}

	t1 := Eval(g1)
	Traceln(3, ", eval: ", t1)
	switch t1.Type() {
	case BoolType:
		if t1.(Bool) {
			return env, true
		}
		return env, false
	case CompoundType:
		t2 := t1.(Compound)
		biChrList := readProperConstraintsFromBI_Store(&t2, nil)
		len_chr := len(biChrList)
		if len_chr == 0 {
			return env, false
		}
		for _, chr := range biChrList {
			if Equal(t1, chr) {
				return env, true
			}
		}
		// to do for the operators(@): ==, !=, <, <=, >, >=, =<
		// symmetry: x @ y --> y @ x
		// transitivity: x @ y && y @ z --> x @ z
		//
		// case AtomType, IntType, FloatType, StringType:
		//	case ListType:
		//	case VariableType:
	}
	return env, false
}

// check guards of the rule r with the binding env
// if all guards are true, fire rule
func checkGuards(r *chrRule, env Bindings) (ok bool) {
	for _, g := range r.guard {
		env2, ok := checkGuard(g, env)
		if !ok {
			return false
		}
		env = env2
	}
	if fireRule(r, env) {
		return true
	}
	// dt do setFail
	return true
}

// check a guard g with the binding env
// if guards are true, return the new binding (if ':=', '=' or 'is' guard)
func checkGuard(g *Compound, env Bindings) (env2 Bindings, ok bool) {

	g1 := Substitute(*g, env).(Compound)

	if g.Functor == ":=" || g1.Functor == "is" || g1.Functor == "=" {
		if !(g1.Args[0].Type() == VariableType) {
			return env, false
		}
		a := Eval(g1.Args[1])
		env2 = AddBinding(g1.Args[0].(Variable), a, env)
		return env2, true
	}
	t1 := Eval(g1)
	switch t1.Type() {
	case BoolType:
		if t1.(Bool) {
			return env, true
		}
		return env, false
	case CompoundType:
		t2 := t1.(Compound)
		biChrList := readProperConstraintsFromBI_Store(&t2, nil)
		len_chr := len(biChrList)
		if len_chr == 0 {
			return env, false
		}
		for _, chr := range biChrList {
			if Equal(t1, chr) {
				return env, true
			}
		}
		// to do for the operators(@): ==, !=, <, <=, >, >=, =<
		// symmetry: x @ y --> y @ x
		// transitivity: x @ y && y @ z --> x @ z
		//
		// case AtomType, IntType, FloatType, StringType:
		//	case ListType:
		//	case VariableType:
	}
	return env, false
}

// rule fired and trace with the environment env
func traceFireRule(rule *chrRule, env Bindings) bool {
	var biVarEqTerm Bindings
	biVarEqTerm = nil
	goals := rule.body

	if goals.Type() == ListType {
		for _, g := range goals {
			TraceHead(3, 3, " Goal: ", g.String())
			g = RenameAndSubstitute(g, RenameRuleVars, env)
			Traceln(3, " after rename&subst: ", g.String())
			g = Eval(g)

			if g.Type() == CompoundType {
				g1 := g.(Compound)
				if len(g1.Args) == 2 {
					arg0 := g1.Args[0]
					arg0ty := arg0.Type()
					arg1 := g1.Args[1]
					switch g1.Functor {
					case ":=", "is", "=":
						if !(arg0ty == VariableType) {
							TraceHeadln(1, 3, "Missing Variable in assignment in body: ", g.String(), ", in rule:", rule.name)
							return false
						}

						env = AddBinding(arg0.(Variable), arg1, env)
						TraceHeadln(1, 3, "in fire Rule add Binding: ", arg0.(Variable).String(), " = ", arg1.String())
						// add assignment or not add assignment - thats the question
						// up to now the assignment will be added
					case "==":
						arg1ty := arg1.Type()
						if arg0ty == VariableType && arg1ty == VariableType {
							arg0var := arg0.(Variable)
							arg1var := arg1.(Variable)
							if arg0var.Name > arg1var.Name {
								g1 = CopyCompound(g1)
								g1.Args[0] = arg1var
								g1.Args[1] = arg0var
								g = g1
								biVarEqTerm = AddBinding(arg1var, arg0var, biVarEqTerm)
							} else {
								biVarEqTerm = AddBinding(arg0var, arg1var, biVarEqTerm)
							}
						} else if arg0ty == VariableType {

							biVarEqTerm = AddBinding(arg0.(Variable), arg1, biVarEqTerm)

						} else if arg1ty == VariableType {
							g1 = CopyCompound(g1)
							g1.Args[0] = arg1
							g1.Args[1] = arg0
							g = g1
							biVarEqTerm = AddBinding(arg1.(Variable), arg0, biVarEqTerm)
						} else {
							env2, ok := Unify(arg0, arg1, biVarEqTerm)
							if ok {
								biVarEqTerm = env2
							} else {
								env2, ok := Unify(arg1, arg0, biVarEqTerm)
								if ok {
									biVarEqTerm = env2
								}
							}
						}

					} // end switch g1.Functor
				} // end if len(g1.Args) == 2
				TraceHeadln(3, 3, "Add Goal: ", g)
				addConstraintToStore(g.(Compound))
			} else {
				if g.Type() == BoolType && !g.(Bool) {
					return false
				}
			}
		}
		if biVarEqTerm != nil {
			substituteStores(biVarEqTerm)
		}
	}
	return true
}

func substituteStores(biEnv Bindings) {
	newCHR := []Compound{}
	for _, aChr := range CHRstore {
		for _, con := range aChr.varArg {
			if !con.IsDeleted {
				con1, ok := SubstituteBiEnv(*con, biEnv)
				if ok && con1.Type() == CompoundType {
					newCHR = append(newCHR, con1.(Compound))
					con.IsDeleted = true
				}
			}
		}
	}
	for _, con := range newCHR {
		addConstraintToStore(con)
	}
	/*
		newBI := []Compound{}
		for _, aChr := range BuiltInStore {
			for _, con := range aChr.varArg {
				if !con.IsDeleted {
					con1, ok := SubstituteBiEnv(*con, biEnv)
					if ok && con1.Type() == CompoundType {
						newBI = append(newBI, con1.(Compound))
						con.IsDeleted = true
					}
				}
			}
		}
		for _, con := range newBI {
			addConstraintToStore(con)
		}
	*/
}

// rule fired with the environment env
func fireRule(rule *chrRule, env Bindings) bool {
	var biVarEqTerm Bindings
	biVarEqTerm = nil
	goals := rule.body

	if goals.Type() == ListType {
		for _, g := range goals {
			g = RenameAndSubstitute(g, RenameRuleVars, env)
			g = Eval(g)

			if g.Type() == CompoundType {
				g1 := g.(Compound)
				if len(g1.Args) == 2 {
					arg0 := g1.Args[0]
					arg0ty := arg0.Type()
					arg1 := g1.Args[1]
					switch g1.Functor {
					case ":=", "is", "=":
						if !(arg0ty == VariableType) {
							TraceHeadln(1, 3, "Missing Variable in assignment in body: ", g.String(), ", in rule:", rule.name)
							return false
						}

						env = AddBinding(arg0.(Variable), arg1, env)
						// add assignment or not add assignment - thats the question
						// up to now the assignment will be added
					case "==":
						arg1ty := arg1.Type()
						if arg0ty == VariableType && arg1ty == VariableType {
							arg0var := arg0.(Variable)
							arg1var := arg1.(Variable)
							if arg0var.Name > arg1var.Name {
								g1 = CopyCompound(g1)
								g1.Args[0] = arg1var
								g1.Args[1] = arg0var
								g = g1
								biVarEqTerm = AddBinding(arg1var, arg0var, biVarEqTerm)
							} else {
								biVarEqTerm = AddBinding(arg0var, arg1var, biVarEqTerm)
							}
						} else if arg0ty == VariableType {

							biVarEqTerm = AddBinding(arg0.(Variable), arg1, biVarEqTerm)

						} else if arg1ty == VariableType {
							g1 = CopyCompound(g1)
							g1.Args[0] = arg1
							g1.Args[1] = arg0
							g = g1
							biVarEqTerm = AddBinding(arg1.(Variable), arg0, biVarEqTerm)
						} else {
							env2, ok := Unify(arg0, arg1, biVarEqTerm)
							if ok {
								biVarEqTerm = env2
							} else {
								env2, ok := Unify(arg1, arg0, biVarEqTerm)
								if ok {
									biVarEqTerm = env2
								}
							}
						}
					} // end switch g1.Functor
				} // end if len(g1.Args) == 2
				addConstraintToStore(g.(Compound))
			} else {
				if g.Type() == BoolType && !g.(Bool) {
					return false
				}
			}
		}
		if biVarEqTerm != nil {
			substituteStores(biVarEqTerm)
		}
	}
	return true
}

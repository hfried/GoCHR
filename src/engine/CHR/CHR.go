// Copyright © 2016 The Carneades Authors
// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL
// was not distributed with this file, You can obtain one
// at http://mozilla.org/MPL/2.0/.

// Constraint Handling Rules

package chr

import (
	"fmt"
	"math/big"

	//	. "github.com/hfried/GoCHR/src/engine/parser"
	. "github.com/hfried/GoCHR/src/engine/terms"
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

type EnvMap struct {
	inBinding   Bindings
	outBindings map[int]*EnvMap
}

type chrRule struct {
	name     string
	id       int
	his      history
	delHead  CList // removed constraints
	keepHead CList // kept constraint
	guard    CList // built-in constraint
	body     List  // add CHR and built-in constraint
	eMap     *EnvMap
}

type resultType int

const (
	REmpty resultType = iota
	RStore
	RTrue
	RFalse
)

type RuleStore struct {
	Result         resultType
	CHRruleStore   []*chrRule
	QueryVars      Vars
	QueryStore     List
	CHRstore       store
	BuiltInStore   store
	nextRuleId     int // = 0
	emptyBinding   Bindings
	RenameRuleVars *big.Int
	chrCounter     *big.Int
}

var bigOne = big.NewInt(1)

// init, add and read CHR- and Build-In-store
// -----------------------------------------

func MakeRuleStore() *RuleStore {
	rs := &RuleStore{}
	InitStore(rs)
	return rs
}

func (rs *RuleStore) AddRule(name string, keep []string, del []string, guard []string, body []string) error {

	cKeepList, cDelList, cGuardList, bodyList, err := parseRule(name, keep, del, guard, body)

	if err == nil {
		rs.CHRruleStore = append(rs.CHRruleStore, &chrRule{name: name, id: rs.nextRuleId,
			delHead:  cDelList,
			keepHead: cKeepList,
			guard:    cGuardList,
			body:     bodyList,
			eMap:     &EnvMap{inBinding: rs.emptyBinding, outBindings: map[int]*EnvMap{}}})
		rs.nextRuleId++
	}
	return err
}

func (rs *RuleStore) Infer(goals []string) (bool, []string, error) {
	cGoals, err := parseGoals(goals)
	if err == nil {
		// fmt.Printf("** parseGoals OK\n")
		ClearCHRStore(rs)
		for _, g := range cGoals {
			addRefConstraintToStore(rs, g)
		}
		CHRsolver(rs)

		switch rs.Result {
		case REmpty:
			return true, []string{}, err
		case RFalse:
			return false, []string{}, err
		case RTrue:
			return true, []string{}, err
		}
		result := []string{}
		// default: Result == RStore
		for _, aChr := range rs.CHRstore {
			for _, con := range aChr.varArg {
				if !con.IsDeleted {
					result = append(result, con.String())
				}
			}
			for _, con := range aChr.noArg {
				if !con.IsDeleted {
					result = append(result, con.String())
				}
			}
		}

		for _, aChr := range rs.BuiltInStore {
			for _, con := range aChr.varArg {
				if !con.IsDeleted {
					result = append(result, con.String())
				}
			}
			for _, con := range aChr.noArg {
				if !con.IsDeleted {
					result = append(result, con.String())
				}
			}
		}
		return true, result, err
	}
	// fmt.Printf("** parseGoals  mit Fehler !!! \n")
	return false, []string{}, err
}

func InitStore(rs *RuleStore) {
	rs.Result = REmpty
	InitRenamingVariables()
	v := NewVariable("")
	rs.emptyBinding = &BindEle{Var: v, T: nil, Next: nil}
	rs.chrCounter = big.NewInt(0)
	rs.nextRuleId = 0
	rs.CHRruleStore = []*chrRule{}
	rs.CHRstore = store{}
	rs.BuiltInStore = store{}
	rs.QueryStore = List{}
	rs.QueryVars = Vars{}
}

func ClearCHRStore(rs *RuleStore) {
	rs.Result = REmpty
	InitRenamingVariables()
	rs.chrCounter = big.NewInt(0)
	rs.CHRstore = store{}
	rs.BuiltInStore = store{}
	rs.QueryStore = List{}
	rs.QueryVars = Vars{}
	// clear EMaps
	for _, rule := range rs.CHRruleStore {
		rule.eMap = &EnvMap{inBinding: rs.emptyBinding, outBindings: map[int]*EnvMap{}}
	}
}

func NewArgCHR() *argCHR {
	return &argCHR{atomArg: map[string]CList{},
		boolArg: CList{}, intArg: CList{}, floatArg: CList{}, strArg: CList{},
		compArg: map[string]CList{}, listArg: CList{}, varArg: CList{}, noArg: CList{}}
}

func addGoal1(g *Compound, s store) {
	switch len(g.Args) {
	case 0:
		TraceHeadln(3, 3, "  addGoal1: '", g.Functor, "'() ")
	case 1:
		TraceHeadln(3, 3, "  addGoal1: Functor:'", g.Functor, "'(", g.Args[0], ")")
	case 2:
		TraceHeadln(3, 3, "  addGoal1: Functor:'", g.Functor, "'(", g.Args[0], ",", g.Args[1], ")")
	default:
		TraceHeadln(3, 3, "  addGoal1: Functor:'", g.Functor, "'(", g.Args[0], ",", g.Args[1], ", ... )")
	}

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
	aArg.varArg = append(aArg.varArg, g) // a variable match to all types
}

func addConstraintToStore(rs *RuleStore, g Compound) {
	addRefConstraintToStore(rs, &g)
}
func addRefConstraintToStore(rs *RuleStore, g *Compound) {
	// TraceHeadln(3, 3, " a) Counter %v \n", chrCounter)
	g.Id = rs.chrCounter
	rs.chrCounter = new(big.Int).Add(rs.chrCounter, bigOne)
	// TraceHeadln(3, 3, " b) Counter++ %v , Id: %v \n", chrCounter, g.Id)
	if g.Prio == 0 {
		addGoal1(g, rs.CHRstore)
	} else {
		addGoal1(g, rs.BuiltInStore)
	}
}

func readProperConstraintsFromCHR_Store(rs *RuleStore, t *Compound, env Bindings) CList {
	argAtt, ok := rs.CHRstore[t.Functor]
	if ok {
		return readProperConstraintsFromStore(t, argAtt, env)
	}
	return CList{}
}

func readProperConstraintsFromBI_Store(rs *RuleStore, t *Compound, env Bindings) CList {
	argAtt, ok := rs.BuiltInStore[t.Functor]
	if ok {
		return readProperConstraintsFromStore(t, argAtt, env)
	}
	return CList{}
}

func readProperConstraintsFromStore(t *Compound, aAtt *argCHR, env Bindings) CList {
	if t.Functor == "safety" {
		TraceHeadln(3, 3, "  readProperConstrain: ", t.Functor)
	}
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
			if t.Functor == "safety" {
				TraceHeadln(3, 3, "  Binding: ", t2)
			}
			arg0 = t2
			argTyp = arg0.Type()
		} else {
			break
		}
	}

	switch arg0.Type() {
	case AtomType:
		cl, ok := aAtt.atomArg[string(arg0.(Atom))]
		if t.Functor == "safety" {
			TraceHeadln(3, 3, "  arg0 == AtomType: ", string(arg0.(Atom)), " OK:", ok, "CL: ", cl)
		}
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
		if t.Functor == "safety" {
			TraceHeadln(3, 3, "  arg0 == VariableType: ", arg0.(Variable), " aAtt.varArg: ", aAtt.varArg)
		}
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
func CHRsolver(rs *RuleStore) {

	if CHRtrace != 0 {
		printCHRStore(rs, "New goal:")
	}
	i := 0
	ruleFound := true
	for ruleFound, i = true, 0; ruleFound && rs.Result != RFalse && i < 100000; i++ {
		// for ruleFound := true; ruleFound; {
		ruleFound = false
		for _, rule := range rs.CHRruleStore {
			rs.RenameRuleVars = <-Counter
			if CHRtrace != 0 {
				TraceHeadln(2, 1, "trial rule ", rule.name, "(ID: ", rule.id, ") @ ", rule.keepHead.String(),
					" \\ ", rule.delHead.String(), " <=> ", rule.guard.String(), " | ", rule.body.String(), ".")

				if TraceRuleFired(rs, rule) {
					TraceHeadln(1, 1, "rule ", rule.name, " fired (id: ", rule.id, ")")
					ruleFound = true
					break
				}
				TraceHeadln(2, 1, "rule ", rule.name, " NOT fired (id: ", rule.id, ")")
			} else {
				if pRuleFired(rs, rule) {
					ruleFound = true
					break
				}
			}

		}
		if ruleFound && CHRtrace != 0 {
			printCHRStore(rs, "Intermediary result:")
		}
	}
	if i == 100000 {
		TraceHeadln(0, 1, "!!! Time-out !!!")
	}

	reduceStore(rs)

	if CHRtrace > 1 {
		printCHRStore(rs, "Result:")
	}
}

func equationSolver(arg1, arg2 Term, env Bindings) (Bindings, bool) {
	v1 := arg1.OccurVars()
	v2 := arg2.OccurVars()
	lenv1 := len(v1)
	lenv2 := len(v2)
	if lenv1 == 0 && lenv2 == 0 {
		if Equal(Eval(arg1), Eval(arg2)) {
			return env, true
		}
		return env, false
	}
	// fmt.Printf("** In equationsSolver \n")
	var v Variable
	vFound := 0
	for _, v3 := range v1 {
		if IsNewVariable(v3) {
			if vFound != 0 { // found two variables
				return env, false
			}
			v = v3
			vFound = 1
		}
	}
	for _, v3 := range v2 {
		if IsNewVariable(v3) {
			if vFound != 0 { // found two variables
				return env, false
			}
			v = v3
			vFound = 2
		}
	}
	if vFound == 0 {
		return env, false
	}
	if vFound == 2 {
		arg1, arg2 = arg2, arg1
	}
	// fmt.Printf("** Vor eqSolver1 \n")
	return eqSolver1(v, arg1, arg2, env)
}

func eqSolver1(v Variable, arg1, arg2 Term, env Bindings) (Bindings, bool) {
	// fmt.Printf("** In eqSolver1 Arg1: %s, Arg2: %s\n", arg1, arg2)
	//	switch arg1.Type() {
	//	case CompoundType:
	//	case ListType:
	//	case VariableType:
	//		v2 := arg1.(Variable)
	//		if EqVars(v, v2)
	//		/*		AtomType Type = iota
	//		BoolType
	//		IntType
	//		FloatType
	//		StringType
	//		CompoundType
	//		ListType
	//		VariableType
	//		*/
	//	}

	return env, false
}

func reduceStore(rs *RuleStore) {
	if rs.Result != RStore {
		return
	}
	// fmt.Printf("** In reduce Store\n")
	bi := bi2CList(rs)
	var env Bindings = nil
	// search alle bindings in equals
	idxList := []int{}
	for idx, b := range bi {
		if b.Functor == "==" {
			env2, ok := Unify(b.Args[0], b.Args[1], env)
			if !ok {
				idxList = append(idxList, idx)
			} else {
				env = env2
			}
		}
	}

	reduce2true := false
	// fmt.Printf("** Nach Unify idxList: %v \n", idxList)
	for _, idx := range idxList {
		b := bi[idx]
		b2 := Eval(Substitute(*b, env))
		if b2.Type() == BoolType {
			if b2.(Bool) == true {
				reduce2true = true
				b.IsDeleted = true
			} else {
				rs.Result = RFalse
				return
			}
		}
		if b2.Type() == CompoundType {
			b3 := b2.(Compound)
			bi[idx] = &b3
			env2, ok := equationSolver(b3.Args[0], b3.Args[1], env)
			if !ok {
				rs.Result = RFalse
				return
			}
			env = env2
			reduce2true = true
			b.IsDeleted = true
		}

	}

	// fmt.Printf("** Nach equationSover \n")
	if env == nil {
		return
	}
	//	for env2 := env; env2 != nil; env2 = env2.Next {
	//		v1 := env2.Var
	//		b1 :=
	//	}
	// reduce all equals

	pcount := 0
	visited := map[string]bool{}
	for i, b := range bi {
		pcount++
		if b.Functor == "==" {
			arg0 := b.Args[0]
			arg1 := b.Args[1]
			if arg0.Type() == VariableType && IsNewVariable(arg0.(Variable)) &&
				arg1.Type() == VariableType && IsNewVariable(arg1.(Variable)) {
				if visited[arg0.(Variable).Name+"#"+arg1.(Variable).Name] ||
					visited[arg1.(Variable).Name+"#"+arg0.(Variable).Name] {
					b.IsDeleted = true
					pcount--
				} else {
					visited[arg0.(Variable).Name+"#"+arg1.(Variable).Name] = true
				}
				continue
			}
			if arg0.Type() == VariableType && IsNewVariable(arg0.(Variable)) {
				if visited[arg0.(Variable).Name] {
					b.IsDeleted = true
					pcount--
				} else {
					visited[arg0.(Variable).Name] = true
					b.Args[1] = Eval(Substitute(arg1, env))
				}
				continue
			}
			if arg1.Type() == VariableType && IsNewVariable(arg1.(Variable)) {
				if visited[arg1.(Variable).Name] {
					b.IsDeleted = true
					pcount--
				} else {
					visited[arg1.(Variable).Name] = true
					b.Args[0] = arg1
					b.Args[1] = Eval(Substitute(arg0, env))
				}
				continue
			}
			pcount--
			b.IsDeleted = true

		} else {
			pcount--
			// subst && eval
			sb := Substitute(*b, env)
			sb = Eval(sb)
			if sb.Type() == BoolType {
				if sb.(Bool) == true {
					reduce2true = true
					b.IsDeleted = true
				} else {
					rs.Result = RFalse
					return
				}
			}
			if sb.Type() == CompoundType {
				sb1 := sb.(Compound)
				bi[i] = &sb1
			}
		}
	}
	chrs := chr2CList(rs)
	for i, c := range chrs {
		pcount++
		c1 := Substitute(*c, env)
		c1 = Eval(c1)
		if c1.Type() == BoolType {
			if c1.(Bool) == true {
				reduce2true = true
				c.IsDeleted = true
				pcount--
			} else {
				rs.Result = RFalse
				return
			}
		}
		if c1.Type() == CompoundType {
			c2 := c1.(Compound)
			chrs[i] = &c2
		}
	}
	if pcount == 0 && reduce2true {
		rs.Result = RTrue
	}
}

// prove whether rule fired
func pRuleFired(rs *RuleStore, rule *chrRule) (ok bool) {
	headList := rule.delHead
	len_head := len(headList)
	if rule.eMap == nil {
		fmt.Printf(" ######### empty envMap in RULE !!!!!!!!!!!!!!!\n")
		rule.eMap = &EnvMap{inBinding: rs.emptyBinding, outBindings: map[int]*EnvMap{}}
	}

	if len_head != 0 {

		ok = matchDelHead(rs, rule, headList, 0, len_head, rule.eMap)
		return ok
	}

	headList = rule.keepHead
	len_head = len(headList)
	if len_head == 0 {
		return false
	}

	ok = matchKeepHead(rs, rule, []*big.Int{}, headList, 0, len_head, rule.eMap)
	return ok
}

// prove and trace whether rule fired
func TraceRuleFired(rs *RuleStore, rule *chrRule) (ok bool) {
	headList := rule.delHead
	len_head := len(headList)

	if rule.eMap == nil {
		rule.eMap = &EnvMap{inBinding: rs.emptyBinding, outBindings: map[int]*EnvMap{}}
	}

	if len_head != 0 {
		ok = traceMatchDelHead(rs, rule, headList, 0, len_head, rule.eMap)
		return ok
	}

	headList = rule.keepHead
	len_head = len(headList)
	if len_head == 0 {
		return false
	}

	ok = traceMatchKeepHead(rs, rule, []*big.Int{}, headList, 0, len_head, rule.eMap)
	return ok
}

// Try to match the del-head 'it' from the 'headlist' ('nt'==len of 'headlist')
// with the 'ienv'-te environmen 'env'
// If matching ok, call 'matchKeepHead' or 'checkGuards'
func matchDelHead(rs *RuleStore, r *chrRule, headList CList, it int, nt int, envMap *EnvMap) (ok bool) {
	var env2 *EnvMap
	var mark bool
	head := headList[it]
	head2 := head
	env := envMap.inBinding
	chrList := readProperConstraintsFromCHR_Store(rs, head, env)
	len_chr := len(chrList)
	if len_chr == 0 {
		// variabel in head
		if head.Functor == "" {
			// ###
			b, ok := GetBinding(head.Args[0].(Variable), env)
			if !ok {
				return false
			}
			if b.Type() != CompoundType {
				return false
			}
			bc := b.(Compound)
			head2 = &bc
			chrList = readProperConstraintsFromCHR_Store(rs, head2, env)
			len_chr = len(chrList)
			if len_chr == 0 {
				return false
			}
		} else {
			return false
		}
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
	senv := envMap.outBindings
	// senv, ok := (*head.EMap)[ienv]
	//	if ok {
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
						ok = matchKeepHead(rs, r, nil, headList, 0, nt, env2)
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
						ok = matchDelHead(rs, r, headList, it+1, nt, env2)
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
	//	} else {
	//		// senv = []Bindings{}
	//		envMap.nextBindings[ienv] = &EnvMap{storedBinding: rs.emptyBinding, nextBindings: map[int]*EnvMap{}}
	//		// (*head.EMap)[ienv] = senv
	//	}
	// End check in head stored environment map
	// normal head-check, start at ie (not at 0 !!)
	if lastHead {
		for ok, ic := false, ie; !ok && ic < len_chr; ic++ {
			chr := chrList[ic]
			envNew, ok, mark := markCHRAndMatchDelHead(r.id, head2, chr, env)
			if ok {

				//senv = append(senv, env2)
				ok = checkGuards(rs, r, envNew)
				if ok {
					senv[ic] = &EnvMap{inBinding: envNew, outBindings: map[int]*EnvMap{}}
					// (*head.EMap)[ienv] = senv
					return ok
				}
			} else {
				senv[ic] = nil
			}
			if mark {
				unmarkDelCHR(chr)
			}
		}
		// (*head.EMap)[ienv] = senv
		return false
	}
	if lastDelHead {
		for ok, ic := false, ie; !ok && ic < len_chr; ic++ {
			chr := chrList[ic]
			envNew, ok, mark := markCHRAndMatchDelHead(r.id, head2, chr, env)
			if ok {
				// senv = append(senv, env2)
				senv[ic] = &EnvMap{inBinding: envNew, outBindings: map[int]*EnvMap{}}
				ok = matchKeepHead(rs, r, nil, headList, 0, nt, senv[ic])
				if ok {
					// (*head.EMap)[ienv] = senv
					return ok
				}
			} else {
				senv[ic] = nil
			}
			if mark {
				unmarkDelCHR(chr)
			}
		}
		// (*head.EMap)[ienv] = senv
		return false
	}

	for ok, ic := false, ie; !ok && ic < len_chr; ic++ {

		chr := chrList[ic]

		envNew, ok, mark := markCHRAndMatchDelHead(r.id, head2, chr, env) // mark chr and Match, if fail unmark chr
		if ok {
			senv[ic] = &EnvMap{inBinding: envNew, outBindings: map[int]*EnvMap{}}
			ok = matchDelHead(rs, r, headList, it+1, nt, senv[ic])
			if ok {
				// not unmarkDelCHR(chr), markt == deleted
				// (*head.EMap)[ienv] = senv
				return ok
			}
		} else {
			senv[ic] = nil
		}
		if mark {
			unmarkDelCHR(chr)
		}
	}
	// (*head.EMap)[ienv] = senv
	return false
}

// Try to match and trace the del-head 'it' from the 'headlist' ('nt'==len of 'headlist')
// with the 'ienv'-te environmen 'env'
// If matching ok, call 'matchKeepHead' or 'checkGuards'
func traceMatchDelHead(rs *RuleStore, r *chrRule, headList CList, it int, nt int, envMap *EnvMap) (ok bool) {
	var env2 Bindings
	var mark bool
	env := envMap.inBinding
	head := headList[it]
	head2 := head
	chrList := readProperConstraintsFromCHR_Store(rs, head, env)
	TraceHead(3, 3, "match Del-Head ", head, " with [")
	len_chr := len(chrList)
	if len_chr == 0 {
		// variabel in head
		if head.Functor == "" {
			// ###
			b, ok := GetBinding(head.Args[0].(Variable), env)
			if !ok {
				Traceln(3, "]")
				return false
			}
			if b.Type() != CompoundType {
				Traceln(3, "]")
				return false
			}
			bc := b.(Compound)
			head2 = &bc
			chrList = readProperConstraintsFromCHR_Store(rs, head2, env)
			len_chr = len(chrList)
			if len_chr == 0 {
				Traceln(3, "]")
				return false
			}
		} else {
			Traceln(3, "]")
			return false
		}
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
	senv := envMap.outBindings
	// senv, ok := (*head.EMap)[ienv]
	//	if ok {
	TraceEMap(4, 4, head, envMap)
	len_ie = len(senv)
	// trace
	TraceHead(4, 3, "match Del-Head ", head, " Env: [")
	first = true
	for _, e := range senv {
		if first {
			first = false
		} else {
			Trace(4, ", ")
		}
		if e != nil {
			TraceEnv(4, e.inBinding)
		}
	}
	Traceln(4, "]")

	// End trace

	if lastHead {
		ie = len_ie
	} else {
		if lastDelHead {
			for ; ie < len_ie; ie++ {
				envOut := senv[ie]
				if envOut != nil {
					chr := chrList[ie]
					mark = markCHR(chr)
					if mark {
						ok = traceMatchKeepHead(rs, r, nil, headList, 0, nt, envOut)
						if ok {
							return ok
						}
						traceUnmarkDelCHR(chr)
					}
				}
			}
		} else { // not a last Del-Head
			for ; ie < len_ie; ie++ {
				envOut := senv[ie]
				if envOut != nil {
					chr := chrList[ie]
					mark = markCHR(chr)
					if mark {
						ok = traceMatchDelHead(rs, r, headList, it+1, nt, envOut)
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
	//	} else {
	//		// head.EMap = &EnvMap{}
	//		TraceEMap(4, 4, head)
	//		senv = []Bindings{}
	//		(*head.EMap)[ienv] = senv
	//	}
	// End check in head stored environment map
	// normal head-check, start at ie (not at 0 !!)
	TraceHeadln(3, 3, "match del-Head ", head, " from: ", ie, " < ", len_chr)
	if lastHead {
		for ok, ic := false, ie; !ok && ic < len_chr; ic++ {
			chr := chrList[ic]
			// env = lateRenameVars(env)
			env2, ok, mark = traceMarkCHRAndMatchDelHead(r.id, head2, chr, env)
			if ok {
				senv[ic] = &EnvMap{inBinding: env2, outBindings: map[int]*EnvMap{}}
				// trace senv changes

				TraceHead(4, 3, "New environment ", "Head: ", head.String(), ", Env: [", ic, "], =")
				TraceEnv(4, env2)
				Traceln(4, "")

				ok = traceCheckGuards(rs, r, env2)
				if ok {
					// (*head.EMap)[ienv] = senv
					TraceEMap(4, 4, head, envMap)
					return ok
				}
			} else {
				senv[ic] = nil
			}
			if mark {
				traceUnmarkDelCHR(chr)
			}
		}
		// (*head.EMap)[ienv] = senv
		TraceEMap(4, 4, head, envMap)
		return false
	}
	if lastDelHead {
		for ok, ic := false, ie; !ok && ic < len_chr; ic++ {
			chr := chrList[ic]
			// env = lateRenameVars(env)
			env2, ok, mark = traceMarkCHRAndMatchDelHead(r.id, head2, chr, env)
			if ok {
				senv[ic] = &EnvMap{inBinding: env2, outBindings: map[int]*EnvMap{}}
				// trace senv changes

				TraceHead(4, 3, "New environment ", "Head: ", head.String(), ", Env: [", ic, "], =")
				TraceEnv(4, env2)
				Traceln(4, "")

				ok = traceMatchKeepHead(rs, r, nil, headList, 0, nt, senv[ic])
				if ok {
					// (*head.EMap)[ienv] = senv
					TraceEMap(4, 4, head, envMap)
					return ok
				}
			} else {
				senv[ic] = nil
			}
			if mark {
				traceUnmarkDelCHR(chr)
			}
		}
		// (*head.EMap)[ienv] = senv
		TraceEMap(4, 4, head, envMap)
		return false
	}

	for ok, ic := false, ie; !ok && ic < len_chr; ic++ {

		chr := chrList[ic]
		// env = lateRenameVars(env)  // ???
		env2, ok, mark = traceMarkCHRAndMatchDelHead(r.id, head2, chr, env) // mark chr and Match, if fail unmark chr
		if ok {
			senv[ic] = &EnvMap{inBinding: env2, outBindings: map[int]*EnvMap{}}
			// trace senv changes

			TraceHead(4, 3, "New environment ", "Head: ", head.String(), ", Env: [", ic, "], =")
			TraceEnv(4, env2)
			Traceln(4, "")
			ok = traceMatchDelHead(rs, r, headList, it+1, nt, senv[ic])
			if ok {
				// not unmarkDelCHR(chr), markt == deleted
				// (*head.EMap)[ienv] = senv
				TraceEMap(4, 4, head, envMap)
				return ok
			}
		} else {
			senv[ic] = nil
		}
		if mark {
			traceUnmarkDelCHR(chr)
		}
	}
	// (*head.EMap)[ienv] = senv
	TraceEMap(4, 4, head, envMap)
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

func traceMarkCHRAndMatchDelHead(id int, head, chr *Compound, env Bindings) (env2 Bindings, ok bool, m bool) {
	// mark and unmark chr
	if chr.IsDeleted {
		return env, false, false
	}
	// TraceHeadln(3, 3, "     *** mark del %v, ID: %v\n", chr, chr.Id)
	chr.IsDeleted = true
	env2, ok = Match(*head, *chr, env)
	if ok {
		TraceHead(3, 3, "Match head ", head, " with CHR ", chr, " (Id: ", chr.Id, ") is ", ok, " (Binding: ")
		TraceEnv(3, env2)
		Traceln(3, ")")
	} else {
		TraceHead(4, 3, "Match head ", head, " with mark CHR ", chr, " (Id: ", chr.Id, ") is ", ok, " (Binding: ")
		TraceEnv(4, env2)
		Traceln(4, ")")
	}
	return env2, ok, true
}

func markCHRAndMatchDelHead(id int, head, chr *Compound, env Bindings) (env2 Bindings, ok bool, m bool) {
	// mark and unmark chr
	if chr.IsDeleted {
		return env, false, false
	}
	chr.IsDeleted = true
	env2, ok = Match(*head, *chr, env)
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

func traceMarkCHRAndMatchKeepHead(id int, head, chr *Compound, env Bindings) (env2 Bindings, ok bool, m bool) {
	// mark and unmark chr

	if chr.IsDeleted {
		return env, false, false
	}
	// TraceHeadln(3, 3, "mark keep ",chr,", ID: ",chr.Id )
	chr.IsDeleted = true
	env2, ok = Match(*head, *chr, env)
	if ok {
		TraceHead(3, 3, "Match head ", head, " with CHR ", chr, " (Id: ", chr.Id, ") is ", ok, " (Binding: ")
		TraceEnv(3, env2)
		Traceln(3, ")")
	} else {
		TraceHead(4, 3, "Match head ", head, " with mark CHR ", chr, " (Id: ", chr.Id, ") is ", ok, " (Binding: ")
		TraceEnv(4, env2)
		Traceln(4, ")")
	}
	return env2, ok, true
}

func markCHRAndMatchKeepHead(id int, head, chr *Compound, env Bindings) (env2 Bindings, ok bool, m bool) {
	// mark and unmark chr

	if chr.IsDeleted {
		return env, false, false
	}
	chr.IsDeleted = true
	env2, ok = Match(*head, *chr, env)
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

// Try to match the keep-head 'it' from the 'headlist' ('nt'==len of 'headlist')
// with the 'ienv'-te environmen 'env'
// If matching for all keep-heads ok, call 'checkGuards'
func matchKeepHead(rs *RuleStore, r *chrRule, his []*big.Int, headList CList, it int, nt int, envMap *EnvMap) (ok bool) {
	var env2 Bindings
	var mark bool
	head := headList[it]
	head2 := head
	env := envMap.inBinding
	chrList := readProperConstraintsFromCHR_Store(rs, head, env)
	len_chr := len(chrList)
	if len_chr == 0 {
		// variabel in head
		if head.Functor == "" {
			// ###
			b, ok := GetBinding(head.Args[0].(Variable), env)
			if !ok {
				return false
			}
			if b.Type() != CompoundType {
				return false
			}
			bc := b.(Compound)
			head2 = &bc
			chrList = readProperConstraintsFromCHR_Store(rs, head2, env)
			len_chr = len(chrList)
			if len_chr == 0 {
				return false
			}
		} else {
			return false
		}
	}

	// begin check the next head
	lastKeepHead := it+1 == nt
	// End next check next head
	// check in head stored environment map
	ie := 0
	len_ie := 0
	senv := envMap.outBindings
	//	if ok {
	len_ie = len(senv)
	if lastKeepHead {
		ie = len_ie
	} else {
		for ; ie < len_ie; ie++ {
			envOut := senv[ie]
			if envOut != nil {
				chr := chrList[ie]
				mark = markCHR(chr)
				if mark {
					ok = matchKeepHead(rs, r, nil, headList, it+1, nt, envOut)
					if ok {
						unmarkKeepCHR(chr)
						return ok
					}
					unmarkKeepCHR(chr)
				}
			}
		}

	} // ! lastHead
	//	} else { // if !ok
	//		// head.EMap = &EnvMap{}
	//		senv = []Bindings{}
	//		(*head.EMap)[ienv] = senv
	//	}
	// End check in head stored environment map
	// normal head-check, start at ie (not at 0 !!)
	if lastKeepHead {
		for ok, ic := false, ie; !ok && ic < len_chr; ic++ {
			chr := chrList[ic]
			env2, ok, mark = markCHRAndMatchKeepHead(r.id, head2, chr, env)
			if ok {
				senv[ic] = &EnvMap{inBinding: env2, outBindings: map[int]*EnvMap{}}
				ok = checkGuards(rs, r, env2)
				if ok {
					unmarkKeepCHR(chr)
					// (*head.EMap)[ienv] = senv
					return ok
				}
			} else {
				senv[ic] = nil
			}
			if mark {
				unmarkKeepCHR(chr)
			}
		}
		// (*head.EMap)[ienv] = senv
		return false
	}

	for ok, ic := false, ie; !ok && ic < len_chr; ic++ {

		chr := chrList[ic]

		env2, ok, mark = markCHRAndMatchKeepHead(r.id, head2, chr, env) // mark chr and Match, if fail unmark chr
		if ok {
			senv[ic] = &EnvMap{inBinding: env2, outBindings: map[int]*EnvMap{}}

			ok = matchKeepHead(rs, r, nil, headList, it+1, nt, senv[ic])
			if ok {
				unmarkKeepCHR(chr)
				// (*head.EMap)[ienv] = senv
				return ok
			}
		} else {
			senv[ic] = nil
		}
		if mark {
			unmarkDelCHR(chr)
		}
	}
	// (*head.EMap)[ienv] = senv
	return false
}

// Try to match and trace the keep-head 'it' from the 'headlist' ('nt'==len of 'headlist')
// with the 'ienv'-te environmen 'env'
// If matching for all keep-heads ok, call 'checkGuards'
func traceMatchKeepHead(rs *RuleStore, r *chrRule, his []*big.Int, headList CList, it int, nt int, envMap *EnvMap) (ok bool) {
	var env2 Bindings
	var mark bool
	env := envMap.inBinding
	head := headList[it]
	head2 := head
	chrList := readProperConstraintsFromCHR_Store(rs, head, env)
	TraceHead(4, 3, "match keep-Head ", head, " with [")
	len_chr := len(chrList)
	if len_chr == 0 {
		// variabel in head
		if head.Functor == "" {
			// ###
			Trace(4, " {HeadVariable=", head.Args[0].(Variable), "}")
			b, ok := GetBinding(head.Args[0].(Variable), env)
			if !ok {
				Traceln(3, "] - empty chr (Variable not bind)")
				return false
			}
			if b.Type() != CompoundType {
				Traceln(3, "] - empty chr (Variable!=CompoundType)", b)
				return false
			}
			bc := b.(Compound)
			head2 = &bc
			chrList = readProperConstraintsFromCHR_Store(rs, head2, env)
			len_chr = len(chrList)
			if len_chr == 0 {
				Traceln(3, "] - empty chr (Variable=) ", b)
				return false
			}
		} else {
			Traceln(3, "] - empty chr")
			return false
		}
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
	// senv, ok := (*head.EMap)[ienv]
	senv := envMap.outBindings
	//	if !ok {
	//		TraceHeadln(4, 4, " !!! head: ", head, " with no Emap[ ", ienv, " ]")
	//	}
	//	if ok {
	TraceEMap(4, 4, head, envMap)
	len_ie = len(senv)
	TraceHeadln(4, 4, " len env = ", len_ie)
	if lastKeepHead {
		ie = len_ie
		TraceHeadln(4, 4, " ie == len_ie == ", ie, " = ", len_ie)
	} else {
		// trace
		TraceHead(4, 3, "match Keep-Head ", head, " Env: [")
		first = true
		for _, e := range senv {
			if first {
				first = false
			} else {
				Trace(4, ", ")
			}
			TraceEnv(4, e.inBinding)
		}
		Traceln(4, "]")

		// End trace
		for ; ie < len_ie; ie++ {
			envOut := senv[ie]
			if envOut != nil {
				chr := chrList[ie]
				mark = markCHR(chr)
				TraceHeadln(4, 4, " mark keep chr:", chr.String(), " = ", mark)
				if mark {
					ok = traceMatchKeepHead(rs, r, nil, headList, it+1, nt, envOut)
					if ok {
						traceUnmarkKeepCHR(chr)
						return ok
					}
					traceUnmarkKeepCHR(chr)
				}
			}
		}

	} // ! lastHead
	//	} else { // if !ok
	//		// head.EMap = &EnvMap{}
	//		senv = []Bindings{}
	//		(*head.EMap)[ienv] = senv
	//	}
	// End check in head stored environment map
	// normal head-check, start at ie (not at 0 !!)
	TraceHeadln(4, 3, "match keep-Head ", head, " from: ", ie, " < ", len_chr)
	if lastKeepHead {
		for ok, ic := false, ie; !ok && ic < len_chr; ic++ {
			chr := chrList[ic]
			env2, ok, mark = traceMarkCHRAndMatchKeepHead(r.id, head2, chr, env)
			if ok {
				senv[ic] = &EnvMap{inBinding: env2, outBindings: map[int]*EnvMap{}}
				// trace senv changes

				TraceHead(4, 3, "New environment ", "Head: ", head.String(), ", Env [", ic, "], =")
				TraceEnv(4, env2)
				Traceln(4, "")

				ok = traceCheckGuards(rs, r, env2)
				if ok {
					traceUnmarkKeepCHR(chr)
					// (*head.EMap)[ienv] = senv
					TraceEMap(4, 4, head, envMap)
					return ok
				}
			} else {
				senv[ic] = nil
			}
			if mark {
				traceUnmarkKeepCHR(chr)
			}
		}
		// (*head.EMap)[ienv] = senv
		TraceEMap(4, 4, head, envMap)
		return false
	}

	for ok, ic := false, ie; !ok && ic < len_chr; ic++ {

		chr := chrList[ic]

		env2, ok, mark = traceMarkCHRAndMatchKeepHead(r.id, head2, chr, env) // mark chr and Match, if fail unmark chr
		if ok {
			senv[ic] = &EnvMap{inBinding: env2, outBindings: map[int]*EnvMap{}}
			// trace senv changes

			TraceHead(4, 3, "New environment ", "Head: ", head.String(), ", Env:  [", ic, "], =")
			TraceEnv(4, env2)
			Traceln(4, "")

			ok = traceMatchKeepHead(rs, r, nil, headList, it+1, nt, senv[ic])
			if ok {
				traceUnmarkKeepCHR(chr)
				// (*head.EMap)[ienv] = senv
				TraceEMap(4, 4, head, envMap)
				return ok
			}
		} else {
			senv[ic] = nil
		}
		if mark {
			unmarkDelCHR(chr)
		}
	}
	// (*head.EMap)[ienv] = senv
	TraceEMap(4, 4, head, envMap)
	return false
}

// check and trace guards of the rule r with the binding env
// if all guards are true, fire rule
func traceCheckGuards(rs *RuleStore, r *chrRule, env Bindings) (ok bool) {
	for _, g := range r.guard {
		env2, ok := traceCheckGuard(rs, g, env)
		if !ok {
			return false
		}
		env = env2
	}
	if traceFireRule(rs, r, env) {
		return true
	}
	// dt do setFail
	return true
}

// check and trace a guard g with the binding env
// if guards are true, return the new binding (if ':=', '=' or 'is' guard)
func traceCheckGuard(rs *RuleStore, g *Compound, env Bindings) (env2 Bindings, ok bool) {
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
		biChrList := readProperConstraintsFromBI_Store(rs, &t2, nil)
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
func checkGuards(rs *RuleStore, r *chrRule, env Bindings) (ok bool) {
	for _, g := range r.guard {
		env2, ok := checkGuard(rs, g, env)
		if !ok {
			return false
		}
		env = env2
	}
	if fireRule(rs, r, env) {
		return true
	}
	// dt do setFail
	return true
}

// check a guard g with the binding env
// if guards are true, return the new binding (if ':=', '=' or 'is' guard)
func checkGuard(rs *RuleStore, g *Compound, env Bindings) (env2 Bindings, ok bool) {

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
		biChrList := readProperConstraintsFromBI_Store(rs, &t2, nil)
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
func traceFireRule(rs *RuleStore, rule *chrRule, env Bindings) bool {
	var biVarEqTerm Bindings
	biVarEqTerm = nil
	goals := rule.body

	if goals.Type() == ListType {
		for _, g := range goals {
			TraceHead(3, 3, " Goal: ", g.String())
			g = RenameAndSubstitute(g, rs.RenameRuleVars, env)
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
							env2, ok := Match(arg0, arg1, biVarEqTerm)
							if ok {
								biVarEqTerm = env2
							} else {
								env2, ok := Match(arg1, arg0, biVarEqTerm)
								if ok {
									biVarEqTerm = env2
								}
							}
						}

					} // end switch g1.Functor
				} // end if len(g1.Args) == 2
				TraceHeadln(3, 3, "Add Goal: ", g)
				addConstraintToStore(rs, g.(Compound))
				rs.Result = RStore
			} else {
				if g.Type() == BoolType && !g.(Bool) {
					rs.Result = RFalse
					return false
				}
			}
		}
		if biVarEqTerm != nil {
			substituteStores(rs, biVarEqTerm)
		}
	}
	return true
}

func substituteStores(rs *RuleStore, biEnv Bindings) {
	newCHR := []Compound{}
	for _, aChr := range rs.CHRstore {
		for _, con := range aChr.varArg {
			if !con.IsDeleted {
				con1, ok := SubstituteBiEnv(*con, biEnv)
				if ok && con1.Type() == CompoundType {
					newCHR = append(newCHR, con1.(Compound))
					con.IsDeleted = true
				}
			}
		}
		for _, con := range aChr.noArg {
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
		addConstraintToStore(rs, con)
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
func fireRule(rs *RuleStore, rule *chrRule, env Bindings) bool {
	var biVarEqTerm Bindings
	biVarEqTerm = nil
	goals := rule.body

	if goals.Type() == ListType {
		g2, ok := GetImplicitEquals(env)
		if ok {
			for _, g := range goals {
				g2 = append(g2, g)
			}
			goals = g2
		}
		for _, g := range goals {
			g = RenameAndSubstitute(g, rs.RenameRuleVars, env)
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
							env2, ok := Match(arg0, arg1, biVarEqTerm)
							if ok {
								biVarEqTerm = env2
							} else {
								env2, ok := Match(arg1, arg0, biVarEqTerm)
								if ok {
									biVarEqTerm = env2
								}
							}
						}
					} // end switch g1.Functor
				} // end if len(g1.Args) == 2
				addConstraintToStore(rs, g.(Compound))
				rs.Result = RStore
			} else {
				if g.Type() == BoolType && !g.(Bool) {
					rs.Result = RFalse
					return false
				}
			}
		}
		if biVarEqTerm != nil {
			substituteStores(rs, biVarEqTerm)
		}
	}
	return true
}

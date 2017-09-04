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

//type EnvMap struct {
//	inBinding   Bindings
//	outBindings map[int]*EnvMap
//}

const EnvCache = 1

type (
	KeepCall int
	KeepReq  int
)

const (
	C_noLast KeepCall = iota
	C_last
)

func keepCall2str(k KeepCall) string {
	switch k {
	case C_noLast:
		return "no-Last"
	case C_last:
		return "last"
	}
	return "last-Fail"
}

func keepReq2str(r KeepReq) string {
	switch r {
	case R_noNews:
		return "no-News"
	case R_nextNew:
		return "next-New"
	}
	return "next"
}

const (
	R_noNews KeepReq = iota
	R_nextNew
	R_next
)

type keepMem struct {
	// 	startIdx, curIdx, endIdx, lenOld, lenNew int
	startIdx, curIdx, endIdx int
	idxState                 KeepCall
}

type ruleIdx struct {
	rule *chrRule
	idx  int
}

type predicateRule map[string][]*ruleIdx

type chrRule struct {
	name     string
	id       int
	isOn     bool
	wasOn    bool
	his      history
	delHead  CList      // removed constraints
	keepHead CList      // kept constraint
	keepEnv  []*keepMem //
	keepReq  KeepReq
	guard    CList // built-in constraint
	body     List  // add CHR and built-in constraint
	eMap     *EnvMap
}

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
	pred2rule      predicateRule
}

type resultType int

const (
	REmpty resultType = iota
	RStore
	RTrue
	RFalse
)

var bigOne = big.NewInt(1)

// init, add and read CHR- and Build-In-store
// -----------------------------------------

func MakeRuleStore() *RuleStore {
	rs := &RuleStore{}
	InitStore(rs)
	return rs
}

func makeKeepEnv(kl CList) []*keepMem {
	kEnv := make([]*keepMem, len(kl))
	for idx, _ := range kEnv {
		kEnv[idx] = &keepMem{}
	}
	return kEnv
}

func (rs *RuleStore) AddRule(name string, keep []string, del []string, guard []string, body []string) error {

	cKeepList, cDelList, cGuardList, bodyList, err := parseRule(name, keep, del, guard, body)

	if err == nil {

		r := &chrRule{name: name, id: rs.nextRuleId,
			delHead:  cDelList,
			keepHead: cKeepList,
			keepEnv:  makeKeepEnv(cKeepList),
			guard:    cGuardList,
			body:     bodyList,
			eMap:     &EnvMap{InBinding: rs.emptyBinding, OutBindings: map[int]*EnvMap{}},
			isOn:     false,
			wasOn:    true}
		TraceHeadln(3, 3, " OFF rule: ", name, " (AddRule) ")
		rs.CHRruleStore = append(rs.CHRruleStore, r)

		addRuleToPred2rule(rs, r)
		rs.nextRuleId++
	}
	return err
}
func addRuleToPred2rule(rs *RuleStore, r *chrRule) {
	p2r := rs.pred2rule

	del := r.delHead
	keep := r.keepHead

	for _, pred := range del {
		if pred.Type() == CompoundType {
			r2idx, ok := p2r[pred.Functor]
			if !ok {
				p2r[pred.Functor] = append(p2r[pred.Functor], &ruleIdx{rule: r, idx: 0})
			} else {
				found := false
				for _, val := range r2idx {
					if val.rule == r {
						found = true
						break
					}
				}
				if !found {
					p2r[pred.Functor] = append(p2r[pred.Functor], &ruleIdx{rule: r, idx: 0})
				}
			}
		} else {
			r.isOn = true
			r.wasOn = true
			TraceHeadln(3, 3, " ON rule: ", r.name, " (variable in del) ")
		}
	}

	for _, pred := range keep {
		if pred.Type() == CompoundType {
			r2idx, ok := p2r[pred.Functor]
			if !ok {
				p2r[pred.Functor] = append(p2r[pred.Functor], &ruleIdx{rule: r, idx: 0})
			} else {
				found := false
				for _, val := range r2idx {
					if val.rule == r {
						found = true
						break
					}
				}
				if !found {
					p2r[pred.Functor] = append(p2r[pred.Functor], &ruleIdx{rule: r, idx: 0})
				}
			}
		} else {
			r.isOn = true
			r.wasOn = true
			TraceHeadln(3, 3, " ON rule: ", r.name, " (variable in keep) ")
		}
	}

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
				if con != nil && !con.IsDeleted {
					result = append(result, con.String())
				}
			}
			for _, con := range aChr.noArg {
				if con != nil && !con.IsDeleted {
					result = append(result, con.String())
				}
			}
		}

		for _, aChr := range rs.BuiltInStore {
			for _, con := range aChr.varArg {
				if con != nil && !con.IsDeleted {
					result = append(result, con.String())
				}
			}
			for _, con := range aChr.noArg {
				if con != nil && !con.IsDeleted {
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
	rs.pred2rule = predicateRule{}
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
		rule.eMap = &EnvMap{InBinding: rs.emptyBinding, OutBindings: map[int]*EnvMap{}}
		rule.isOn = false
		TraceHeadln(3, 3, " OFF rule: ", rule.name, " (Clear Store) ")
		rule.wasOn = true
		for _, keepIdx := range rule.keepEnv {
			keepIdx.startIdx = 0
			keepIdx.curIdx = 0
			keepIdx.endIdx = 0
		}
		rule.keepReq = R_noNews
	}
}

func NewArgCHR() *argCHR {
	return &argCHR{atomArg: map[string]CList{},
		boolArg: CList{}, intArg: CList{}, floatArg: CList{}, strArg: CList{},
		compArg: map[string]CList{}, listArg: CList{}, varArg: CList{}, noArg: CList{}}
}

func delConstraint(g *Compound, rs *RuleStore) {
	delGoal1(g, rs.CHRstore)
}

func delGoal1(g *Compound, s store) {

	aArg, ok := s[g.Functor]
	if !ok {
		return
	}
	for idx, val := range aArg.varArg {
		if val == g {
			aArg.varArg[idx] = nil
			if CHRtrace != 0 {
				switch len(g.Args) {
				case 0:
					TraceHeadln(3, 3, "  delGoal1: '", g.Functor, "'() ")
				case 1:
					TraceHeadln(3, 3, "  delGoal1: Functor:'", g.Functor, "'(", g.Args[0], ")")
				case 2:
					TraceHeadln(3, 3, "  delGoal1: Functor:'", g.Functor, "'(", g.Args[0], ",", g.Args[1], ")")
				default:
					TraceHeadln(3, 3, "  delGoal1: Functor:'", g.Functor, "'(", g.Args[0], ",", g.Args[1], ", ... )")
				}
			}
			return
		}
	}
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
		p2r := rs.pred2rule
		ruleSlice, _ := p2r[g.Functor]
		for _, rIdx := range ruleSlice {
			rIdx.rule.isOn = true
			TraceHeadln(3, 3, " ON rule: ", rIdx.rule.name, " (Add Constraint to Store) ")
		}
	} else {
		addGoal1(g, rs.BuiltInStore)
	}
}

func readProperConstraintsFromCHR_Store(rs *RuleStore, t *Compound, env Bindings) CList {
	argAtt, ok := rs.CHRstore[t.Functor]
	if ok {
		chr := readProperConstraintsFromStore(t, argAtt, env)
		// TraceHeadln(3, 3, " ++> read ", t.Functor, " constraint(", len(chr), ") = ", chr)
		return chr
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
	//	if t.Functor == "safety" {
	//		TraceHeadln(3, 3, "  readProperConstrain: ", t.Functor)
	//	}
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
			//			if t.Functor == "safety" {
			//				TraceHeadln(3, 3, "  Binding: ", t2)
			//			}
			arg0 = t2
			argTyp = arg0.Type()
		} else {
			break
		}
	}

	switch arg0.Type() {
	case AtomType:
		cl, ok := aAtt.atomArg[string(arg0.(Atom))]
		//		if t.Functor == "safety" {
		//			TraceHeadln(3, 3, "  arg0 == AtomType: ", string(arg0.(Atom)), " OK:", ok, "CL: ", cl)
		//		}
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

func readProperKeepConstraintsFromCHR_Store(rs *RuleStore, t *Compound) CList {
	argAtt, ok := rs.CHRstore[t.Functor]
	if ok {
		chr := readProperKeepConstraintsFromStore(t, argAtt)
		// TraceHeadln(3, 3, " ++> read ", t.Functor, " constraint(", len(chr), ") = ", chr)
		return chr
	}
	return CList{}
}

func readProperKeepConstraintsFromStore(t *Compound, aAtt *argCHR) CList {
	//	if t.Functor == "safety" {
	//		TraceHeadln(3, 3, "  readProperConstrain: ", t.Functor)
	//	}
	args := t.Args
	l := len(args)
	if l == 0 {
		return aAtt.noArg
	}
	arg0 := args[0]
	//  argTyp := arg0.Type()
	//	for argTyp == VariableType {
	//		t2, ok := GetBinding(arg0.(Variable), env)
	//		if ok {
	//			//			if t.Functor == "safety" {
	//			//				TraceHeadln(3, 3, "  Binding: ", t2)
	//			//			}
	//			arg0 = t2
	//			argTyp = arg0.Type()
	//		} else {
	//			break
	//		}
	//	}

	switch arg0.Type() {
	case AtomType:
		cl, ok := aAtt.atomArg[string(arg0.(Atom))]
		//		if t.Functor == "safety" {
		//			TraceHeadln(3, 3, "  arg0 == AtomType: ", string(arg0.(Atom)), " OK:", ok, "CL: ", cl)
		//		}
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
	if CHRtrace == 0 {
		for ruleFound, i = true, 0; ruleFound && rs.Result != RFalse && i < 100000; i++ {
			// for ruleFound := true; ruleFound; {
			ruleFound = false
			for _, rule := range rs.CHRruleStore {
				if rule.isOn {
					rs.RenameRuleVars = <-Counter
					if pRuleFired(rs, rule) {
						ruleFound = true
						break
					}
					rule.isOn = false
					// fmt.Printf("     OFF rule %s (Rule not fired 2) \n", rule.name)
					// TraceHeadln(1, 1, " OFF rule: ", rule.name, " (Rule not fired2) ")
				}
			}
		}
	} else { // CHRtrace != 0
		for ruleFound, i = true, 0; ruleFound && rs.Result != RFalse && i < 100000; i++ {
			// for ruleFound := true; ruleFound; {
			ruleFound = false
			for _, rule := range rs.CHRruleStore {

				if rule.isOn {
					rs.RenameRuleVars = <-Counter

					TraceHeadln(2, 1, "trial rule ", rule.name, "(ID: ", rule.id, ") @ ", rule.keepHead.String(),
						" \\ ", rule.delHead.String(), " <=> ", rule.guard.String(), " | ", rule.body.String(), ".")

					if TraceRuleFired(rs, rule) {
						TraceHeadln(1, 1, "rule ", rule.name, " fired (id: ", rule.id, ")")
						ruleFound = true
						break
					}
					rule.isOn = false
					TraceHeadln(1, 1, " OFF rule: ", rule.name, " (Rule not fired) ")
					TraceHeadln(2, 1, "rule ", rule.name, " NOT fired (id: ", rule.id, ")")
					rule.isOn = false
					// fmt.Printf("     OFF rule %s (Rule not fired 2) \n", rule.name)
					// TraceHeadln(1, 1, " OFF rule: ", rule.name, " (Rule not fired2) ")
				} else {
					TraceHeadln(2, 1, "rule is OFF: ", rule.name)
				}
			}

			if ruleFound {
				printCHRStore(rs, "Intermediary result:")
			}
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
	keepList := rule.keepHead
	len_keepHead := len(keepList)
	delList := rule.delHead
	len_delHead := len(delList)

	if rule.eMap == nil {
		fmt.Printf(" ######### empty envMap in RULE !!!!!!!!!!!!!!!\n")
		rule.eMap = &EnvMap{InBinding: rs.emptyBinding, OutBindings: map[int]*EnvMap{}}
	}

	if len_delHead == 0 {
		if len_keepHead == 0 {
			return false
		}
		// only keepHead
		req := rule.keepReq // Request
		// fmt.Printf("->>[> %s <] Rule %s stored req\n", keepReq2str(req), rule.name)
		if req != R_next {
			CHRtrace = 4
			ok, req = handleLastFail(rs, rule, []*big.Int{}, keepList, 0, len_keepHead, rule.eMap, nil)
			CHRtrace = 0
			// fmt.Printf("<<-[> %v, %s <] << [> lastFail <], Rule %s \n", ok, keepReq2str(req), rule.name)
		}
		for req == R_next {
			CHRtrace = 4
			ok, req = matchKeepHead(rs, rule, []*big.Int{}, keepList, 0, len_keepHead, rule.eMap, nil, C_last)
			CHRtrace = 0
			// fmt.Printf("<<-[> %v, %s <] << [> next <] Rule %s  \n", ok, keepReq2str(req), rule.name)
			if ok {
				rule.keepReq = req
				return ok
			}
		}
		rule.keepReq = req
		return ok // == false

	}

	if len_keepHead != 0 {

		ok = matchKeepDelHead(true, rs, rule, keepList, 0, len_keepHead, rule.eMap, nil)
		return ok
	}

	return matchDelHead(rs, rule, delList, 0, len_delHead, rule.eMap, nil)

}

// prove and trace whether rule fired
func TraceRuleFired(rs *RuleStore, rule *chrRule) (ok bool) {
	keepList := rule.keepHead
	len_keepHead := len(keepList)
	delList := rule.delHead
	len_delHead := len(delList)

	if rule.eMap == nil {
		fmt.Printf(" ######### empty envMap in RULE !!!!!!!!!!!!!!!\n")
		rule.eMap = &EnvMap{InBinding: rs.emptyBinding, OutBindings: map[int]*EnvMap{}}
	}

	if len_delHead == 0 {
		if len_keepHead == 0 {
			return false
		}
		// only keepHead
		req := rule.keepReq // Request
		// fmt.Printf("->>[> %s <] Rule %s stored req\n", keepReq2str(req), rule.name)
		if req != R_next {
			ok, req = traceHandleLastFail(rs, rule, []*big.Int{}, keepList, 0, len_keepHead, rule.eMap, nil)
			// fmt.Printf("<<-[> %v, %s <] << [> lastFail <], Rule %s \n", ok, keepReq2str(req), rule.name)
		}
		for req == R_next {
			ok, req = traceMatchKeepHead(rs, rule, []*big.Int{}, keepList, 0, len_keepHead, rule.eMap, nil, C_last)
			// fmt.Printf("<<-[> %v, %s <] << [> next <] Rule %s  \n", ok, keepReq2str(req), rule.name)
			if ok {
				rule.keepReq = req
				return ok
			}
		}
		rule.keepReq = req
		return ok // == false

	}

	if len_keepHead != 0 {

		ok = traceMatchKeepDelHead(true, rs, rule, keepList, 0, len_keepHead, rule.eMap, nil)
		return ok
	}

	return traceMatchDelHead(rs, rule, delList, 0, len_delHead, rule.eMap, nil)

}

// Try to match the del-head 'it' from the 'headlist' ('nt'==len of 'headlist')
// with the 'ienv'-te environmen 'env'
// If matching ok, call 'matchKeepHead' or 'checkGuards'
func matchKeepDelHead(isKeep bool, rs *RuleStore, r *chrRule, headList CList, it int, nt int, envMap *EnvMap, env1 Bindings) (ok bool) {
	var env Bindings
	var env2 *EnvMap
	var senv map[int]*EnvMap
	var mark bool

	head := headList[it]
	head2 := head
	if env1 == nil {
		env = envMap.InBinding
	} else {
		env = env1
	}

	chrList := readProperConstraintsFromCHR_Store(rs, head, env)
	len_chr := len(chrList)

	// trace
	CHRtrace = 3
	if isKeep {
		TraceHeadln(3, 3, "-----")
		TraceHead(3, 3, "match Keep-Head >", head, "< with [")
	} else {
		TraceHead(3, 3, "match Del-Head >", head, "< with [")
	}
	CHRtrace = 0
	// end trace

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

	// begin trace
	CHRtrace = 3
	first := true
	for _, c := range chrList {
		if c != nil && !c.IsDeleted {
			if first {
				Trace(3, c)
				first = false
			} else {
				Trace(3, ", ", c)
			}
		}
	}
	Traceln(3, "]")
	CHRtrace = 0
	// end trace
	// end trace

	// begin check the next head
	lastKeepDelHead := it+1 == nt // last Keep or last Del-Head
	lastHead := false
	if lastKeepDelHead {
		// last keep head
		if isKeep {
			// headList = r.keepHead
			headList = r.delHead
			nt = len(headList)
			if nt == 0 {
				lastHead = true
			}
		} else {
			lastHead = true
		}
	}
	// End next check next head, if lastDelHead the headList == r.keephead
	// check in head stored environment map
	ie := 0
	len_ie := 0

	if env1 == nil {

		senv = envMap.OutBindings
		// senv, ok := (*head.EMap)[ienv]
		//	if ok {
		len_ie = len(senv)
		if lastHead {
			ie = len_ie
		} else {
			if lastKeepDelHead {
				for ; ie < len_ie; ie++ {
					env2 = senv[ie]
					if env2 != nil {
						chr := chrList[ie]
						mark = markCHR(chr)
						if mark {
							ok = matchKeepDelHead(false, rs, r, headList, 0, nt, env2, nil)
							if ok {
								unmarkDelCHR(chr)
								// unmarkDelCHR(chr), chr == keepCHR
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
							ok = matchKeepDelHead(isKeep, rs, r, headList, it+1, nt, env2, nil)
							if ok {
								if isKeep {
									unmarkDelCHR(chr)
								} else {
									chrList[ie] = nil
									delConstraint(chr, rs)
								}
								// else: not unmarkDelCHR(chr), markt == deleted
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
	}
	// normal head-check, start at ie (not at 0 !!)
	if lastHead {
		for ok, ic := false, ie; !ok && ic < len_chr; ic++ {
			chr := chrList[ic]
			envNew, ok, mark := markCHRAndMatchDelHead(r.id, head2, chr, env)
			if ok {
				if it < EnvCache {
					senv[ic] = &EnvMap{InBinding: envNew, OutBindings: map[int]*EnvMap{}}
				}
				ok = checkGuards(rs, r, envNew)
				if ok {
					if isKeep {
						unmarkDelCHR(chr)
					} else {
						chrList[ic] = nil
						delConstraint(chr, rs)
					}
					// (*head.EMap)[ienv] = senv
					return ok
				}

			} else {
				if it < EnvCache {
					senv[ic] = nil
				}
			}
			if mark {
				unmarkDelCHR(chr)
			}
		}
		// (*head.EMap)[ienv] = senv
		return false
	} // lastHead

	if lastKeepDelHead { // last keepHead in front of delHead
		for ok, ic := false, ie; !ok && ic < len_chr; ic++ {
			chr := chrList[ic]
			envNew, ok, mark := markCHRAndMatchKeepHead(r.id, head2, chr, env)
			if ok {
				// senv = append(senv, env2)
				if it < EnvCache {
					senv[ic] = &EnvMap{InBinding: envNew, OutBindings: map[int]*EnvMap{}}
					if isKeep {
						ok = matchKeepDelHead(false, rs, r, headList, 0, nt, senv[ic], nil)
					} else {
						ok = checkGuards(rs, r, envNew)
					}
				} else {
					if isKeep {
						ok = matchKeepDelHead(false, rs, r, headList, 0, nt, nil, envNew)
					} else {
						ok = checkGuards(rs, r, envNew)
					}
				}
				if ok {
					if isKeep {
						unmarkDelCHR(chr)
					} else {
						chrList[ic] = nil
						delConstraint(chr, rs)
					}
					return ok
				}
			} else {
				if it < EnvCache {
					senv[ic] = nil
				}
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
			if it < EnvCache {
				senv[ic] = &EnvMap{InBinding: envNew, OutBindings: map[int]*EnvMap{}}
				ok = matchKeepDelHead(isKeep, rs, r, headList, it+1, nt, senv[ic], nil)
			} else {
				ok = matchKeepDelHead(isKeep, rs, r, headList, it+1, nt, nil, envNew)
			}

			if ok {
				if isKeep {
					unmarkDelCHR(chr)
				} else {
					chrList[ic] = nil
					delConstraint(chr, rs)
				}
				// not unmarkDelCHR(chr), markt == deleted
				// (*head.EMap)[ienv] = senv
				return ok
			}
		} else {
			if it < EnvCache {
				senv[ic] = nil
			}
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

func traceMatchKeepDelHead(isKeep bool, rs *RuleStore, r *chrRule, headList CList, it int, nt int, envMap *EnvMap, env1 Bindings) (ok bool) {
	var env Bindings
	var env2 *EnvMap
	var senv map[int]*EnvMap
	var mark bool

	head := headList[it]
	head2 := head
	if env1 == nil {
		env = envMap.InBinding
	} else {
		env = env1
	}

	chrList := readProperConstraintsFromCHR_Store(rs, head, env)
	len_chr := len(chrList)

	// trace

	if isKeep {
		TraceHeadln(3, 3, "-----")
		TraceHead(3, 3, "match Keep-Head > ", head, " < with [")
	} else {
		TraceHead(3, 3, "match Del-Head > ", head, " < with [")
	}

	// end trace

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
		if c != nil && !c.IsDeleted {
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
	lastKeepDelHead := it+1 == nt // last Keep or last Del-Head
	lastHead := false
	if lastKeepDelHead {
		// last keep head
		if isKeep {
			// headList = r.keepHead
			headList = r.delHead
			nt = len(headList)
			if nt == 0 {
				lastHead = true
			}
		} else {
			lastHead = true
		}
	}
	// End next check next head, if lastKeepHead the headList == r.delHead
	// check in head stored environment map
	ie := 0
	len_ie := 0

	if env1 == nil {
		senv = envMap.OutBindings
		// senv, ok := (*head.EMap)[ienv]
		//	if ok {
		TraceHead(4, 4, "(1) Emap before head match")
		TraceEMap(4, 4, head, envMap)
		len_ie = len(senv)

		// trace
		if isKeep {
			TraceHead(4, 3, "match Keep-Head > ", head, " < Env: [")
		} else {
			TraceHead(4, 3, "match Del-Head > ", head, " < Env: [")
		}
		first = true
		for _, e := range senv {
			if first {
				first = false
			} else {
				Trace(4, ", ")
			}
			if e != nil {
				TraceEnv(4, e.InBinding)
			}
		}
		Traceln(4, "]")
		// End trace3

		if lastHead {
			ie = len_ie
		} else {
			if lastKeepDelHead {
				for ; ie < len_ie; ie++ {
					env2 = senv[ie]
					if env2 != nil {
						chr := chrList[ie]
						mark = markCHR(chr)
						if mark {
							ok = traceMatchKeepDelHead(false, rs, r, headList, 0, nt, env2, nil)
							if ok {
								traceUnmarkDelCHR(chr)
								// unmarkDelCHR(chr) - chr == keepCHR
								return ok
							}
							traceUnmarkDelCHR(chr)
						}
					}
				}
			} else { // not a last Keep-Head
				for ; ie < len_ie; ie++ {
					env2 = senv[ie]
					if env2 != nil {
						chr := chrList[ie]
						mark = markCHR(chr)
						if mark {
							ok = traceMatchKeepDelHead(isKeep, rs, r, headList, it+1, nt, env2, nil)
							if ok {
								if isKeep {
									unmarkDelCHR(chr)
								} else {
									chrList[ie] = nil
									delConstraint(chr, rs)
								}
								// else: not unmarkDelCHR(chr), markt == deleted
								return ok
							}
							traceUnmarkDelCHR(chr)
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
	}
	// normal head-check, start at ie (not at 0 !!)
	//	// trace
	//	if isKeep {
	//		TraceHeadln(3, 3, "normal match keep-Head > ", head, " < from: ", ie, " < ", len_chr)
	//	} else {
	//		TraceHeadln(3, 3, "normal match del-Head > ", head, " < from: ", ie, " < ", len_chr)
	//	}
	//	// end trace
	if lastHead {
		// trace
		if isKeep {
			TraceHeadln(3, 3, "ERROR: normal match last keep-Head > ", head, " < from: ", ie, " < ", len_chr)
		} else {
			TraceHeadln(3, 3, "normal match last del-Head > ", head, " < from: ", ie, " < ", len_chr)
		}
		// end trace
		for ok, ic := false, ie; !ok && ic < len_chr; ic++ {
			chr := chrList[ic]
			envNew, ok, mark := traceMarkCHRAndMatchDelHead(r.id, head2, chr, env)
			if ok {
				// trace senv changes
				TraceHead(4, 3, "New environment, Head: ", head.String(), ", Env: [", ic, "], =")
				TraceEnv(4, envNew)
				Traceln(4, "")
				// end trace

				if it < EnvCache {
					senv[ic] = &EnvMap{InBinding: envNew, OutBindings: map[int]*EnvMap{}}
				}

				ok = traceCheckGuards(rs, r, envNew)
				if ok {
					if isKeep {
						traceUnmarkDelCHR(chr)
					} else {
						chrList[ic] = nil
						delConstraint(chr, rs)
					}
					// (*head.EMap)[ienv] = senv
					TraceHead(4, 3, "(2) After rule fired Emap for the last del-Head.")
					TraceEMap(4, 4, head, envMap)
					return ok
				}

			} else {
				if it < EnvCache {
					senv[ic] = nil
				}
			}
			if mark {
				traceUnmarkDelCHR(chr)
			}
		}
		// (*head.EMap)[ienv] = senv
		TraceHead(4, 3, "(3) After rule NOT fired Emap for the last del-Head.")
		TraceEMap(4, 4, head, envMap)
		return false
	} // lastHead

	if lastKeepDelHead { // last keepHead in front of delHead
		// trace
		if isKeep {
			TraceHeadln(3, 3, "normal match last keep-Head (.. delHead) > ", head, " < from: ", ie, " < ", len_chr)
		} else {
			TraceHeadln(3, 3, "ERROR: normal match del-Head > ", head, " < from: ", ie, " < ", len_chr)
		}
		// end trace
		for ok, ic := false, ie; !ok && ic < len_chr; ic++ {
			chr := chrList[ic]
			envNew, ok, mark := traceMarkCHRAndMatchKeepHead(r.id, head2, chr, env)
			if ok {
				// trace senv changes
				TraceHead(4, 3, "New environment ", "Head: ", head.String(), ", Env: [", ic, "], =")
				TraceEnv(4, envNew)
				Traceln(4, "")
				// end trace

				// senv = append(senv, env2)
				if it < EnvCache {
					senv[ic] = &EnvMap{InBinding: envNew, OutBindings: map[int]*EnvMap{}}
					if isKeep {
						ok = traceMatchKeepDelHead(false, rs, r, headList, 0, nt, senv[ic], nil)
					} else {
						ok = traceCheckGuards(rs, r, envNew)
					}
				} else {
					if isKeep {
						ok = traceMatchKeepDelHead(false, rs, r, headList, 0, nt, nil, envNew)
					} else {
						ok = traceCheckGuards(rs, r, envNew)
					}
				}
				if ok {
					// trace
					TraceHead(4, 4, "(4) Environment after rule fired: ")
					TraceEMap(4, 4, head, envMap)
					// end trace
					if isKeep {
						traceUnmarkDelCHR(chr)
					} else {
						chrList[ic] = nil
						delConstraint(chr, rs)
					}
					return ok
				}
			} else {
				if it < EnvCache {
					senv[ic] = nil
				}
			}
			if mark {
				traceUnmarkDelCHR(chr)
			}
		}
		// (*head.EMap)[ienv] = senv
		// trace
		TraceHead(4, 4, "(5) Environment after rule NOT fired: ")
		TraceEMap(4, 4, head, envMap)
		// end trace
		return false
	}

	// trace
	if isKeep {
		TraceHeadln(3, 3, "normal match keep-Head > ", head, " < from: ", ie, " < ", len_chr)
	} else {
		TraceHeadln(3, 3, "normal match del-Head > ", head, " < from: ", ie, " < ", len_chr)
	}
	// end trace

	for ok, ic := false, ie; !ok && ic < len_chr; ic++ {

		chr := chrList[ic]

		envNew, ok, mark := traceMarkCHRAndMatchDelHead(r.id, head2, chr, env) // mark chr and Match, if fail unmark chr
		if ok {
			// trace senv changes
			TraceHead(4, 3, "New environment, Head: ", head.String(), ", Env: [", ic, "], =")
			TraceEnv(4, envNew)
			Traceln(4, "")
			// end trace

			if it < EnvCache {
				senv[ic] = &EnvMap{InBinding: envNew, OutBindings: map[int]*EnvMap{}}
				ok = traceMatchKeepDelHead(isKeep, rs, r, headList, it+1, nt, senv[ic], nil)
			} else {
				ok = traceMatchKeepDelHead(isKeep, rs, r, headList, it+1, nt, nil, envNew)
			}

			if ok {
				if isKeep {
					unmarkDelCHR(chr)
				} else {
					chrList[ic] = nil
					delConstraint(chr, rs)
				}
				// not unmarkDelCHR(chr), markt == deleted
				// (*head.EMap)[ienv] = senv
				TraceHead(4, 4, "(6) Emap after rule fired:")
				TraceEMap(4, 4, head, envMap)
				return ok
			}
		} else {
			if it < EnvCache {
				senv[ic] = nil
			}
		}
		if mark {
			traceUnmarkDelCHR(chr)
		}
	}
	// (*head.EMap)[ienv] = senv
	TraceHead(4, 4, "(7) Emap after rule NOT fired:")
	TraceEMap(4, 4, head, envMap)

	return false
}

//func traceMatchDelKeepHead(isDel bool, rs *RuleStore, r *chrRule, headList CList, it int, nt int, envMap *EnvMap, env1 Bindings) (ok bool) {
//	var env, env2 Bindings
//	var mark bool
//	var senv map[int]*EnvMap

//	if env1 == nil {
//		env = envMap.InBinding
//	} else {
//		env = env1
//	}

//	head := headList[it]
//	head2 := head
//	chrList := readProperConstraintsFromCHR_Store(rs, head, env)
//	if isDel {
//		TraceHead(3, 3, "match Del-Head ", head, " with [")
//	} else {
//		TraceHead(3, 3, "match Keep-Head ", head, " with [")
//	}
//	len_chr := len(chrList)
//	if len_chr == 0 {
//		// variabel in head
//		if head.Functor == "" {
//			// ###
//			b, ok := GetBinding(head.Args[0].(Variable), env)
//			if !ok {
//				Traceln(3, "]")
//				return false
//			}
//			if b.Type() != CompoundType {
//				Traceln(3, "]")
//				return false
//			}
//			bc := b.(Compound)
//			head2 = &bc
//			chrList = readProperConstraintsFromCHR_Store(rs, head2, env)
//			len_chr = len(chrList)
//			if len_chr == 0 {
//				Traceln(3, "]")
//				return false
//			}
//		} else {
//			Traceln(3, "]")
//			return false
//		}
//	}
//	// begin trace
//	first := true
//	for _, c := range chrList {
//		if c != nil && !c.IsDeleted {
//			if first {
//				Trace(3, c)
//				first = false
//			} else {
//				Trace(3, ", ", c)
//			}
//		}
//	}
//	Traceln(3, "]")
//	// end trace
//	// begin check the next head
//	lastDelKeepHead := it+1 == nt
//	lastHead := false
//	if lastDelKeepHead {
//		// last del head
//		if isDel {
//			headList = r.keepHead
//			nt = len(headList)
//			if nt == 0 {
//				lastHead = true
//			}
//		} else {
//			lastHead = true
//		}
//	}
//	// End next check next head, if lastDelHead the headList == r.keephead
//	// check in head stored environment map
//	ie := 0
//	len_ie := 0
//	if env1 == nil {
//		senv = envMap.OutBindings
//		// senv, ok := (*head.EMap)[ienv]
//		//	if ok {
//		TraceEMap(4, 4, head, envMap)
//		len_ie = len(senv)
//		// trace
//		if isDel {
//			TraceHead(4, 3, "match Del-Head ", head, " Env: [")
//		} else {
//			TraceHead(4, 3, "match Keep-Head ", head, " Env: [")
//		}
//		first = true
//		for _, e := range senv {
//			if first {
//				first = false
//			} else {
//				Trace(4, ", ")
//			}
//			if e != nil {
//				TraceEnv(4, e.InBinding)
//			}
//		}
//		Traceln(4, "]")

//		// End trace

//		if lastHead {
//			ie = len_ie
//		} else {
//			if lastDelKeepHead {
//				for ; ie < len_ie; ie++ {
//					envOut := senv[ie]
//					if envOut != nil {
//						chr := chrList[ie]
//						mark = traceMarkCHR(chr)
//						if mark {

//							ok = traceMatchDelKeepHead(false, rs, r, headList, 0, nt, envOut, nil)

//							if ok {
//								return ok
//							}
//							traceUnmarkDelCHR(chr)
//						}
//					}
//				}
//			} else { // not a last Del-Head
//				for ; ie < len_ie; ie++ {
//					envOut := senv[ie]
//					if envOut != nil {
//						chr := chrList[ie]
//						mark = markCHR(chr)
//						if mark {
//							ok = traceMatchDelKeepHead(isDel, rs, r, headList, it+1, nt, envOut, nil)
//							if ok {
//								if !isDel {
//									unmarkDelCHR(chr)
//								} else {
//									chrList[ie] = nil
//									delConstraint(chr, rs)
//								}
//								// not unmarkDelCHR(chr), markt == deleted
//								return ok
//							}
//							traceUnmarkDelCHR(chr)
//						}

//					}
//				} // for ; ie < len_ie; ie++
//			} // ! lastDelHead
//		} // ! lastHead
//		//	} else {
//		//		// head.EMap = &EnvMap{}
//		//		TraceEMap(4, 4, head)
//		//		senv = []Bindings{}
//		//		(*head.EMap)[ienv] = senv
//		//	}
//		// End check in head stored environment map
//	}

//	// normal head-check, start at ie (not at 0 !!)
//	if isDel {
//		TraceHeadln(3, 3, "match del-Head ", head, " from: ", ie, " < ", len_chr)
//	} else {
//		TraceHeadln(3, 3, "match keep-Head ", head, " from: ", ie, " < ", len_chr)
//	}
//	if lastHead {
//		if isDel {
//			TraceHeadln(3, 3, "last del-Head ")
//		} else {
//			TraceHeadln(3, 3, "last keep-Head ")
//		}
//		for ok, ic := false, ie; !ok && ic < len_chr; ic++ {
//			chr := chrList[ic]
//			// env = lateRenameVars(env)
//			env2, ok, mark = traceMarkCHRAndMatchDelHead(r.id, head2, chr, env)
//			if ok {
//				if it < EnvCache {
//					senv[ic] = &EnvMap{InBinding: env2, OutBindings: map[int]*EnvMap{}}
//				}
//				// trace senv changes

//				TraceHead(4, 3, "New environment ", "Head: ", head.String(), ", Env: [", ic, "], =")
//				TraceEnv(4, env2)
//				Traceln(4, "")

//				ok = traceCheckGuards(rs, r, env2)
//				if ok {
//					if !isDel {
//						traceUnmarkDelCHR(chr)
//					} else {
//						chrList[ic] = nil
//						delConstraint(chr, rs)
//					}
//					// (*head.EMap)[ienv] = senv
//					TraceEMap(4, 4, head, envMap)
//					return ok
//				}
//			} else {
//				if it < EnvCache {
//					senv[ic] = nil
//				}
//			}
//			if mark {
//				traceUnmarkDelCHR(chr)
//			}
//		}
//		// (*head.EMap)[ienv] = senv
//		TraceEMap(4, 4, head, envMap)
//		return false
//	}
//	if lastDelKeepHead {
//		for ok, ic := false, ie; !ok && ic < len_chr; ic++ {
//			chr := chrList[ic]
//			// env = lateRenameVars(env)
//			env2, ok, mark = traceMarkCHRAndMatchDelHead(r.id, head2, chr, env)
//			if ok {

//				// trace senv changes

//				TraceHead(4, 3, "New environment ", "Head: ", head.String(), ", Env: [", ic, "], =")
//				TraceEnv(4, env2)
//				Traceln(4, "")
//				if it < EnvCache {
//					senv[ic] = &EnvMap{InBinding: env2, OutBindings: map[int]*EnvMap{}}

//					if isDel {
//						ok = traceMatchDelKeepHead(false, rs, r, headList, 0, nt, senv[ic], nil)
//					} else {
//						ok = checkGuards(rs, r, env2)
//					}

//				} else {
//					if isDel {
//						ok = traceMatchDelKeepHead(false, rs, r, headList, 0, nt, nil, env2)
//					} else {
//						ok = checkGuards(rs, r, env2)
//					}
//				}
//				if ok {
//					// (*head.EMap)[ienv] = senv
//					TraceEMap(4, 4, head, envMap)
//					if !isDel {
//						traceUnmarkDelCHR(chr)
//					} else {
//						chrList[ic] = nil
//						delConstraint(chr, rs)
//					}
//					return ok
//				}
//			} else {
//				if it < EnvCache {
//					senv[ic] = nil
//				}
//			}
//			if mark {
//				traceUnmarkDelCHR(chr)
//			}
//		}
//		// (*head.EMap)[ienv] = senv
//		TraceEMap(4, 4, head, envMap)
//		return false
//	}

//	for ok, ic := false, ie; !ok && ic < len_chr; ic++ {

//		chr := chrList[ic]
//		// env = lateRenameVars(env)  // ???
//		env2, ok, mark = traceMarkCHRAndMatchDelHead(r.id, head2, chr, env) // mark chr and Match, if fail unmark chr
//		if ok {

//			// trace senv changes

//			TraceHead(4, 3, "New environment ", "Head: ", head.String(), ", Env: [", ic, "], =")
//			TraceEnv(4, env2)
//			Traceln(4, "")
//			if it < EnvCache {
//				senv[ic] = &EnvMap{InBinding: env2, OutBindings: map[int]*EnvMap{}}
//				ok = traceMatchDelKeepHead(isDel, rs, r, headList, it+1, nt, senv[ic], nil)
//			} else {
//				ok = traceMatchDelKeepHead(isDel, rs, r, headList, it+1, nt, nil, env2)
//			}
//			if ok {
//				// not unmarkDelCHR(chr), markt == deleted
//				// (*head.EMap)[ienv] = senv
//				TraceEMap(4, 4, head, envMap)
//				return ok
//			}
//		} else {
//			if it < EnvCache {
//				senv[ic] = nil
//			}
//		}
//		if mark {
//			traceUnmarkDelCHR(chr)
//		}
//	}
//	// (*head.EMap)[ienv] = senv
//	TraceEMap(4, 4, head, envMap)
//	return false
//}

// Try to match the del-head 'it' from the 'headlist' ('nt'==len of 'headlist')
// with the 'ienv'-te environmen 'env'
// If matching ok, call 'checkGuards'
func matchDelHead(rs *RuleStore, r *chrRule, headList CList, it int, nt int, envMap *EnvMap, env1 Bindings) (ok bool) {
	var env Bindings
	var senv map[int]*EnvMap

	head := headList[it]
	head2 := head
	if env1 == nil {
		env = envMap.InBinding
	} else {
		env = env1
	}

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

	lastHead := it+1 == nt

	// End next check next head, if lastDelHead the headList == r.keephead
	// check in head stored environment map
	ie := 0
	len_ie := 0

	if env1 == nil {

		senv = envMap.OutBindings
		// senv, ok := (*head.EMap)[ienv]
		//	if ok {
		len_ie = len(senv)
		if lastHead {
			ie = len_ie
		}
		//	} else {
		//		// senv = []Bindings{}
		//		envMap.nextBindings[ienv] = &EnvMap{storedBinding: rs.emptyBinding, nextBindings: map[int]*EnvMap{}}
		//		// (*head.EMap)[ienv] = senv
		//	}
		// End check in head stored environment map
	}
	// normal head-check, start at ie (not at 0 !!)
	if lastHead {
		for ok, ic := false, ie; !ok && ic < len_chr; ic++ {
			chr := chrList[ic]
			envNew, ok, mark := markCHRAndMatchDelHead(r.id, head2, chr, env)
			if ok {
				if it < EnvCache {
					senv[ic] = &EnvMap{InBinding: envNew, OutBindings: map[int]*EnvMap{}}
				}
				ok = checkGuards(rs, r, envNew)
				if ok {

					chrList[ic] = nil
					delConstraint(chr, rs)

					// (*head.EMap)[ienv] = senv
					return ok
				}

			} else {
				if it < EnvCache {
					senv[ic] = nil
				}
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
			if it < EnvCache {
				senv[ic] = &EnvMap{InBinding: envNew, OutBindings: map[int]*EnvMap{}}
				ok = matchDelHead(rs, r, headList, it+1, nt, senv[ic], nil)
			} else {
				ok = matchDelHead(rs, r, headList, it+1, nt, nil, envNew)
			}

			if ok {

				chrList[ic] = nil
				delConstraint(chr, rs)

				// not unmarkDelCHR(chr), markt == deleted
				// (*head.EMap)[ienv] = senv
				return ok
			}
		} else {
			if it < EnvCache {
				senv[ic] = nil
			}
		}
		if mark {
			unmarkDelCHR(chr)
		}
	}
	// (*head.EMap)[ienv] = senv
	return false
}

// Try to match the del-head 'it' from the 'headlist' ('nt'==len of 'headlist')
// with the 'ienv'-te environmen 'env'
// If matching ok, call 'checkGuards'
func traceMatchDelHead(rs *RuleStore, r *chrRule, headList CList, it int, nt int, envMap *EnvMap, env1 Bindings) (ok bool) {
	var env Bindings
	var senv map[int]*EnvMap

	head := headList[it]
	head2 := head
	if env1 == nil {
		env = envMap.InBinding
	} else {
		env = env1
	}

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

	lastHead := it+1 == nt

	// End next check next head, if lastDelHead the headList == r.keephead
	// check in head stored environment map
	ie := 0
	len_ie := 0

	if env1 == nil {

		senv = envMap.OutBindings
		// senv, ok := (*head.EMap)[ienv]
		//	if ok {
		len_ie = len(senv)
		if lastHead {
			ie = len_ie
		}
		//	} else {
		//		// senv = []Bindings{}
		//		envMap.nextBindings[ienv] = &EnvMap{storedBinding: rs.emptyBinding, nextBindings: map[int]*EnvMap{}}
		//		// (*head.EMap)[ienv] = senv
		//	}
		// End check in head stored environment map
	}
	// normal head-check, start at ie (not at 0 !!)
	if lastHead {
		for ok, ic := false, ie; !ok && ic < len_chr; ic++ {
			chr := chrList[ic]
			envNew, ok, mark := traceMarkCHRAndMatchDelHead(r.id, head2, chr, env)
			if ok {
				if it < EnvCache {
					senv[ic] = &EnvMap{InBinding: envNew, OutBindings: map[int]*EnvMap{}}
				}
				ok = traceCheckGuards(rs, r, envNew)
				if ok {

					chrList[ic] = nil
					delConstraint(chr, rs)

					// (*head.EMap)[ienv] = senv
					return ok
				}

			} else {
				if it < EnvCache {
					senv[ic] = nil
				}
			}
			if mark {
				traceUnmarkDelCHR(chr)
			}
		}
		// (*head.EMap)[ienv] = senv
		return false
	}

	for ok, ic := false, ie; !ok && ic < len_chr; ic++ {

		chr := chrList[ic]

		envNew, ok, mark := traceMarkCHRAndMatchDelHead(r.id, head2, chr, env) // mark chr and Match, if fail unmark chr
		if ok {
			if it < EnvCache {
				senv[ic] = &EnvMap{InBinding: envNew, OutBindings: map[int]*EnvMap{}}
				ok = traceMatchDelHead(rs, r, headList, it+1, nt, senv[ic], nil)
			} else {
				ok = traceMatchDelHead(rs, r, headList, it+1, nt, nil, envNew)
			}

			if ok {

				chrList[ic] = nil
				delConstraint(chr, rs)

				// not unmarkDelCHR(chr), markt == deleted
				// (*head.EMap)[ienv] = senv
				return ok
			}
		} else {
			if it < EnvCache {
				senv[ic] = nil
			}
		}
		if mark {
			traceUnmarkDelCHR(chr)
		}
	}
	// (*head.EMap)[ienv] = senv
	return false
}

// mark chr - no other head-predicate can match that constraint
func markCHR(chr *Compound) bool {
	if chr == nil || chr.IsDeleted {
		return false
	}
	chr.IsDeleted = true
	return true
}

func traceMarkCHR(chr *Compound) bool {
	if chr == nil || chr.IsDeleted {
		TraceHeadln(4, 3, " Not marked: ", chr)
		return false
	}
	TraceHeadln(3, 3, " Marked: ", chr)
	chr.IsDeleted = true
	return true
}

func traceMarkCHRAndMatchDelHead(id int, head, chr *Compound, env Bindings) (env2 Bindings, ok bool, m bool) {
	// mark and unmark chr
	if chr == nil || chr.IsDeleted {
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
	if chr == nil || chr.IsDeleted {
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

	if chr == nil || chr.IsDeleted {
		TraceHeadln(3, 3, " No match, CHR is marked, head ", head, " CHR ", chr)
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

	if chr == nil || chr.IsDeleted {
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

var spaces int = 1

//func matchKeepHead(rs *RuleStore, r *chrRule, his []*big.Int, headList CList, it int, nt int, envMap *EnvMap, env1 Bindings, call KeepCall) (ok bool, req KeepReq) {
//	var env Bindings
//	if env1 == nil {
//		env = envMap.InBinding
//	} else {
//		env = env1
//	}
//	head := headList[it]
//	chrList := readProperConstraintsFromCHR_Store(rs, head, env)
//	len_chr := len(chrList)
//	keepIdx := r.keepEnv[it]
//	ie := keepIdx.curIdx
//	endIdx := keepIdx.endIdx
//	lastKeepHead := it+1 == nt
//	for k := 0; k < spaces; k++ {
//		fmt.Printf("  ")
//	}
//	spaces++

//	if lastKeepHead {
//		fmt.Printf(" >>>[> %s <], Rule: %s, Last-Head(%d): %s,  start: %d, cur: %d, end: %d chr: %d ---\n", keepCall2str(call), r.name, it, head, keepIdx.startIdx, ie, endIdx, len_chr)

//	} else {
//		fmt.Printf(" >>>[> %s <], Rule: %s, Head(%d): %s,  start: %d, cur: %d, end: %d chr: %d ---\n", keepCall2str(call), r.name, it, head, keepIdx.startIdx, ie, endIdx, len_chr)
//	}
//	ok, req = matchKeepHead1(rs, r, his, headList, it, nt, envMap, env1, call)
//	spaces--
//	for k := 0; k < spaces; k++ {
//		fmt.Printf("  ")
//	}

//	keepIdx = r.keepEnv[it]
//	if lastKeepHead {
//		fmt.Printf(" <<<[> %v, %s <] << [> %s <],  Rule: %s, Last-Head(%d): %s, start: %d, cur: %d, end: %d chr: %d ---\n", ok, keepReq2str(req), keepCall2str(call), r.name, it, head,
//			keepIdx.startIdx, keepIdx.curIdx, keepIdx.endIdx, len_chr)
//	} else {

//		fmt.Printf(" <<<[> %v, %s <] << [> %s <], Rule: %s, Head(%d): %s, start: %d, cur: %d, end: %d chr: %d ---\n", ok, keepReq2str(req), keepCall2str(call), r.name, it, head,
//			keepIdx.startIdx, keepIdx.curIdx, keepIdx.endIdx, len_chr)
//	}

//	return
//}

func handleLastFail(rs *RuleStore, r *chrRule, his []*big.Int, headList CList, it int, nt int, envMap *EnvMap, env1 Bindings) (ok bool, req KeepReq) {
	var env Bindings

	head := headList[it]
	//	head2 := head

	if env1 == nil {
		env = envMap.InBinding
	} else {
		env = env1
	}

	chrList := readProperKeepConstraintsFromCHR_Store(rs, head)
	len_chr := len(chrList)
	if len_chr == 0 {
		// variabel in head
		if head.Functor == "" {
			if it+1 == nt {
				return false, R_next
			} else {
				return handleLastFail(rs, r, nil, headList, it+1, nt, nil, env)
			}

			//			b, ok := GetBinding(head.Args[0].(Variable), env)
			//			if !ok {
			//				return false, R_noNews
			//			}
			//			if b.Type() != CompoundType {
			//				return false, R_noNews
			//			}
			//			bc := b.(Compound)
			//			head2 = &bc
			//			chrList = readProperKeepConstraintsFromCHR_Store(rs, head2)
			//			len_chr = len(chrList)
			//			if len_chr == 0 {
			//				return false, R_noNews
			//			}
		} else {
			return false, R_noNews
		}
	}

	// begin check the next head

	keepIdx := r.keepEnv[it]
	endIdx := keepIdx.endIdx
	itpl1 := it + 1
	for itpl1 < nt && headList[itpl1].Functor == "" {
		itpl1++
	}
	lastKeepHead := itpl1 == nt

	// fmt.Printf(" - Rule: %s, Head(%d): %s, call: %s, start: %d, cur: %d, end: %d chr: %d --- \n", r.name, it, head, keepCall2str(call), keepIdx.startIdx, ie, endIdx, len_chr)

	if lastKeepHead {
		req = R_nextNew
	} else {
		if env1 == nil {
		}
		ok, req = handleLastFail(rs, r, nil, headList, itpl1, nt, nil, env)
	}
	switch req {
	case R_next:
		keepIdx.startIdx, keepIdx.curIdx = 0, 0
		if endIdx == 0 {
			// fmt.Printf("#B# %s.endidx[%d] = %d, lenChr= %d\n", r.name, it, endIdx, len_chr)
			keepIdx.endIdx = len_chr
		}
		return false, R_next

	case R_nextNew:
		if endIdx < len_chr {
			// News
			// fmt.Printf("#C# %s.endidx[%d] = %d, lenChr= %d\n", r.name, it, endIdx, len_chr)
			keepIdx.startIdx, keepIdx.curIdx = endIdx, endIdx
			keepIdx.endIdx = len_chr
			return false, R_next
		} else {
			// old U new
			keepIdx.startIdx, keepIdx.curIdx = 0, 0
			return false, R_nextNew
		}
	}

	return false, R_noNews
}

// Try to match the keep-head 'it' from the 'headlist' ('nt'==len of 'headlist')
// with the 'ienv'-te environmen 'env'
// If matching for all keep-heads ok, call 'checkGuards'
func matchKeepHead(rs *RuleStore, r *chrRule, his []*big.Int, headList CList, it int, nt int, envMap *EnvMap, env1 Bindings, call KeepCall) (ok bool, req KeepReq) {
	var env2 Bindings
	var env Bindings
	var senv map[int]*EnvMap
	var mark bool

	head := headList[it]
	head2 := head

	if env1 == nil {
		env = envMap.InBinding
	} else {
		env = env1
	}

	chrList := readProperKeepConstraintsFromCHR_Store(rs, head)
	len_chr := len(chrList)
	if len_chr == 0 {
		// variabel in head
		if head.Functor == "" {
			b, ok := GetBinding(head.Args[0].(Variable), env)
			if !ok {
				return false, R_noNews
			}
			if b.Type() != CompoundType {
				return false, R_noNews
			}
			bc := b.(Compound)
			head2 = &bc
			chrList = readProperKeepConstraintsFromCHR_Store(rs, head2)
			len_chr = len(chrList)
			if len_chr == 0 {
				return false, R_noNews
			}
		} else {
			return false, R_noNews
		}
	}

	// begin check the next head

	keepIdx := r.keepEnv[it]
	ie := keepIdx.curIdx
	endIdx := keepIdx.endIdx
	if endIdx > len_chr {
		fmt.Printf(" !!!!!!!! #a# %s.endidx[%d] = %d, lenChr= %d\n", r.name, it, endIdx, len_chr)
	} /*else {
		fmt.Printf("#a# %s.endidx[%d] = %d, lenChr= %d\n", r.name, it, endIdx, len_chr)
	}*/
	lastKeepHead := it+1 == nt

	// fmt.Printf(" - Rule: %s, Head(%d): %s, call: %s, start: %d, cur: %d, end: %d chr: %d --- \n", r.name, it, head, keepCall2str(call), keepIdx.startIdx, ie, endIdx, len_chr)

	// End next check next head
	// check in head stored environment map
	call_1 := C_noLast
	len_ie := 0
	if env1 == nil {
		senv = envMap.OutBindings
		len_ie = len(senv)
		for ; ie < len_ie; ie++ {
			envOut := senv[ie]
			if envOut != nil {
				chr := chrList[ie]
				if lastKeepHead {
					// env2, ok, mark = traceMarkCHRAndMatchKeepHead(r.id, head2, chr, env)
					// env2, ok, mark = markCHRAndMatchKeepHead(r.id, head2, chr, env)
					mark = markCHR(chr)
					if mark {
						//						if it < EnvCache {
						//							senv[ie] = &EnvMap{InBinding: env2, OutBindings: map[int]*EnvMap{}}
						//						}
						ok = checkGuards(rs, r, envOut.InBinding)
						unmarkKeepCHR(chr)
						if ok {
							keepIdx.curIdx = ie + 1
							return ok, R_next
						}
					}
					//					else {
					//						if it < EnvCache {
					//							senv[ie] = nil
					//						}
					//					}
					//					if mark {
					//						traceUnmarkKeepCHR(chr)
					//					}
				} else { // not last keepHead
					mark = markCHR(chr)
					if mark {
						if call == C_last && ie+1 == endIdx {
							call_1 = C_last
						}
						ok, req = matchKeepHead(rs, r, nil, headList, it+1, nt, envOut, nil, call_1)
						unmarkKeepCHR(chr)
						if ok {
							keepIdx.curIdx = ie
							return ok, R_next
						}
						switch req {
						case R_next:
							if call_1 == C_last {
								// reset
								keepIdx.curIdx = keepIdx.startIdx
								return false, R_next
							}
						case R_nextNew:
							if endIdx < len_chr {
								// News
								// fmt.Printf("#b# %s.endidx[%d] = %d, lenChr= %d\n", r.name, it, endIdx, len_chr)
								keepIdx.startIdx, keepIdx.curIdx, ie = endIdx, endIdx, endIdx-1 // for .... ie++ { ...}
								keepIdx.endIdx, endIdx = len_chr, len_chr
								return false, R_next
							} else {
								// old U new
								keepIdx.startIdx, keepIdx.curIdx = 0, 0
								return false, R_nextNew
							}
						case R_noNews:
							keepIdx.curIdx = ie
							return false, R_noNews
						}
					}
				}
			}
		}
		// TraceHeadln(3, 3, "End check stored environment, ie=", ie)
	} // End check in head stored environment map

	// ###
	// normal head-check, start at ie (not at 0 !!)

	if lastKeepHead {
		// TraceHeadln(3, 3, "Start last Keep Head from:", ie, " to:", endIdx)
		for ic := ie; ic < endIdx; ic++ {
			// fmt.Printf("#z# %s . len_chr= %d, endIdx[%d]= %d, ic= %d ##\n", r.name, len_chr, it, endIdx, ic)
			chr := chrList[ic]
			env2, ok, mark = markCHRAndMatchKeepHead(r.id, head2, chr, env)
			// env2, ok, mark = markCHRAndMatchKeepHead(r.id, head2, chr, env)
			if ok {
				if it < EnvCache {
					senv[ic] = &EnvMap{InBinding: env2, OutBindings: map[int]*EnvMap{}}
				}
				ok = checkGuards(rs, r, env2)
				if ok {
					unmarkKeepCHR(chr)
					// (*head.EMap)[ienv] = senv
					keepIdx.curIdx = ic + 1
					return ok, R_next
				}
			} else {
				if it < EnvCache {
					senv[ic] = nil
				}
			}
			if mark {
				unmarkKeepCHR(chr)
			}
		}
		// TraceHeadln(3, 3, "End check last Keep Head")
		// (*head.EMap)[ienv] = senv
		if call == C_last {
			if endIdx < len_chr {
				// News
				keepIdx.startIdx, keepIdx.curIdx = endIdx, endIdx
				keepIdx.endIdx = len_chr
				return false, R_next
			} else {
				// old U new
				keepIdx.startIdx, keepIdx.curIdx = 0, 0
				return false, R_nextNew
			}
		}
		keepIdx.curIdx = keepIdx.startIdx
		return false, R_next
	}

	// TraceHeadln(3, 3, "Start check Keep Head from:", ie, " to:", endIdx)
	for ic := ie; ic < endIdx; ic++ {
		chr := chrList[ic]
		env2, ok, mark = markCHRAndMatchKeepHead(r.id, head2, chr, env)
		// env2, ok, mark = markCHRAndMatchKeepHead(r.id, head2, chr, env) // mark chr and Match, if fail unmark chr
		if ok {
			if call == C_last && ic+1 == len_chr {
				call_1 = C_last
			} // else call_1 == C_noLast
			if it < EnvCache {
				senv[ic] = &EnvMap{InBinding: env2, OutBindings: map[int]*EnvMap{}}
				ok, req = matchKeepHead(rs, r, nil, headList, it+1, nt, senv[ic], nil, call_1)
			} else {
				ok, req = matchKeepHead(rs, r, nil, headList, it+1, nt, nil, env2, call_1)
			}
			unmarkKeepCHR(chr)
			if ok {
				keepIdx.curIdx = ic
				return ok, R_next
			}
			switch req {
			case R_next:
				if call_1 == C_last {
					// reset
					keepIdx.curIdx = keepIdx.startIdx
					return false, R_next
				}
			case R_nextNew: // only if call_1 == C_last
				if endIdx < len_chr {
					// News
					keepIdx.startIdx, keepIdx.curIdx = endIdx, endIdx
					keepIdx.endIdx = len_chr
					return false, R_next
				} else {
					// old U new
					keepIdx.startIdx, keepIdx.curIdx = 0, 0
					return false, R_nextNew
				}
			case R_noNews:
				keepIdx.curIdx = ic
				return false, R_noNews
			}
		} else {
			if it < EnvCache {
				senv[ic] = nil
			}
		}
		if mark {
			unmarkDelCHR(chr)
		}
	}
	// TraceHeadln(3, 3, "End check Keep Head from:", ie, " to:", endIdx)

	if call == C_last && call_1 == C_noLast {
		ok, req = handleLastFail(rs, r, nil, headList, it+1, nt, nil, env)
		switch req {
		case R_next:
			keepIdx.startIdx, keepIdx.curIdx = 0, 0
			return false, req
		case R_nextNew:
			if endIdx < len_chr {
				// News
				keepIdx.startIdx, keepIdx.curIdx = endIdx, endIdx
				keepIdx.endIdx = len_chr
				return false, R_next
			} else {
				// old U new
				keepIdx.startIdx, keepIdx.curIdx = 0, 0
				return false, R_nextNew
			}
		case R_noNews:
			return false, R_noNews
		}

	}

	// (*head.EMap)[ienv] = senv
	return false, R_next
}

func traceHandleLastFail(rs *RuleStore, r *chrRule, his []*big.Int, headList CList, it int, nt int, envMap *EnvMap, env1 Bindings) (ok bool, req KeepReq) {
	var env Bindings

	head := headList[it]
	//	head2 := head

	if env1 == nil {
		env = envMap.InBinding
	} else {
		env = env1
	}

	chrList := readProperKeepConstraintsFromCHR_Store(rs, head)
	TraceHead(4, 3, "(last fail) match keep-Head ", head, " with [")
	len_chr := len(chrList)
	if len_chr == 0 {
		// variabel in head
		if head.Functor == "" {

			Traceln(4, " {skip HeadVariable=", head.Args[0].(Variable), "}")
			if it+1 == nt {
				TraceHeadln(4, 3, "(last fail) return R_next ")
				return false, R_next
			} else {
				return traceHandleLastFail(rs, r, nil, headList, it+1, nt, nil, env)
			}
			//			b, ok := GetBinding(head.Args[0].(Variable), env)
			//			if !ok {
			//				Traceln(3, "] - empty chr (Variable not bind)")
			//				return false, R_noNews
			//			}
			//			if b.Type() != CompoundType {
			//				Traceln(3, "] - empty chr (Variable Binding!=CompoundType)", b)
			//				return false, R_noNews
			//			}
			//			bc := b.(Compound)
			//			head2 = &bc
			//			chrList = readProperKeepConstraintsFromCHR_Store(rs, head2)
			//			len_chr = len(chrList)
			//			if len_chr == 0 {
			//				Traceln(3, "] - empty chr (Variable=) ", b)
			//				return false, R_noNews
			//			}
		} else {
			Traceln(3, "] - empty chr")
			TraceHeadln(4, 3, "(last fail) return R_noNews ")
			return false, R_noNews
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

	keepIdx := r.keepEnv[it]
	endIdx := keepIdx.endIdx
	itpl1 := it + 1
	for itpl1 < nt && headList[itpl1].Functor == "" {
		itpl1++
	}
	lastKeepHead := itpl1 == nt
	TraceHeadln(4, 4, " last keep head = ", lastKeepHead)

	// fmt.Printf(" - Rule: %s, Head(%d): %s, call: %s, start: %d, cur: %d, end: %d chr: %d --- \n", r.name, it, head, keepCall2str(call), keepIdx.startIdx, ie, endIdx, len_chr)

	if lastKeepHead {
		req = R_nextNew
	} else {
		ok, req = traceHandleLastFail(rs, r, nil, headList, itpl1, nt, nil, env)
	}
	switch req {
	case R_next:
		keepIdx.startIdx, keepIdx.curIdx = 0, 0
		if endIdx == 0 {
			keepIdx.endIdx = len_chr
		}
		TraceHeadln(4, 3, "(last fail) return R_next 2 ")
		return false, R_next

	case R_nextNew:
		if endIdx < len_chr {
			// News
			keepIdx.startIdx, keepIdx.curIdx = endIdx, endIdx
			keepIdx.endIdx = len_chr
			TraceHeadln(4, 3, "(last fail) return R_next3 ")
			return false, R_next
		} else {
			// old U new
			keepIdx.startIdx, keepIdx.curIdx = 0, 0
			TraceHeadln(4, 3, "(last fail) return R_nextNew 2 ")
			return false, R_nextNew
		}
	}
	TraceHeadln(4, 3, "(last fail) return R_noNews 2 ")
	return false, R_noNews
}

// Try to match and trace the keep-head 'it' from the 'headlist' ('nt'==len of 'headlist')
// with the 'ienv'-te environmen 'env'
// If matching for all keep-heads ok, call 'checkGuards'
func traceMatchKeepHead(rs *RuleStore, r *chrRule, his []*big.Int, headList CList, it int, nt int, envMap *EnvMap, env1 Bindings, call KeepCall) (ok bool, req KeepReq) {
	var env2 Bindings
	var env Bindings
	var senv map[int]*EnvMap
	var mark bool
	TraceHeadln(4, 3, " call MatchKeepHead, env1=", env1, ", call=", keepCall2str(call))
	head := headList[it]
	head2 := head

	if env1 == nil {
		env = envMap.InBinding
	} else {
		env = env1
	}

	chrList := readProperKeepConstraintsFromCHR_Store(rs, head)
	TraceHead(4, 3, "in Match keep-Head ", head, " with [")
	len_chr := len(chrList)
	if len_chr == 0 {
		// variabel in head
		if head.Functor == "" {
			Trace(4, " {HeadVariable=", head.Args[0].(Variable), "}")
			b, ok := GetBinding(head.Args[0].(Variable), env)
			if !ok {
				Traceln(3, "] - empty chr (Variable not bind)")
				return false, R_noNews
			}
			if b.Type() != CompoundType {
				Traceln(3, "] - empty chr (Variable Binding!=CompoundType)", b)
				return false, R_noNews
			}
			bc := b.(Compound)
			head2 = &bc
			chrList = readProperKeepConstraintsFromCHR_Store(rs, head2)
			len_chr = len(chrList)
			if len_chr == 0 {
				Traceln(3, "] - empty chr (Variable=) ", b)
				return false, R_noNews
			}
		} else {
			Traceln(3, "] - empty chr")
			return false, R_noNews
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

	keepIdx := r.keepEnv[it]
	ie := keepIdx.curIdx
	endIdx := keepIdx.endIdx
	lastKeepHead := it+1 == nt
	TraceHeadln(4, 4, " last keep head = ", lastKeepHead)

	// fmt.Printf(" - Rule: %s, Head(%d): %s, call: %s, start: %d, cur: %d, end: %d chr: %d --- \n", r.name, it, head, keepCall2str(call), keepIdx.startIdx, ie, endIdx, len_chr)

	// End next check next head
	// check in head stored environment map
	call_1 := C_noLast
	len_ie := 0
	if env1 == nil {
		senv = envMap.OutBindings
		TraceEMap(4, 4, head, envMap)
		len_ie = len(senv)
		TraceHeadln(4, 4, " len env = ", len_ie)
		for ; ie < len_ie; ie++ {
			envOut := senv[ie]
			if envOut != nil {
				chr := chrList[ie]
				if lastKeepHead {
					// env2, ok, mark = traceMarkCHRAndMatchKeepHead(r.id, head2, chr, env)
					// env2, ok, mark = markCHRAndMatchKeepHead(r.id, head2, chr, env)
					TraceHeadln(4, 4, " ie == len_ie == ", ie, " = ", len_ie)
					mark = traceMarkCHR(chr)
					if mark {
						//						if it < EnvCache {
						//							senv[ie] = &EnvMap{InBinding: env2, OutBindings: map[int]*EnvMap{}}
						//						}
						ok = checkGuards(rs, r, envOut.InBinding)
						traceUnmarkKeepCHR(chr)
						if ok {
							keepIdx.curIdx = ie + 1
							return ok, R_next
						}
					}
					//					else {
					//						if it < EnvCache {
					//							senv[ie] = nil
					//						}
					//					}
					//					if mark {
					//						traceUnmarkKeepCHR(chr)
					//					}
				} else { // not last keepHead
					// trace
					TraceHead(4, 3, "match Keep-Head ", head, " Env: [")
					first = true
					for _, e := range senv {
						if first {
							first = false
						} else {
							Trace(4, ", ")
						}
						if e == nil {
							Trace(4, "e= NIL")
						} else {
							TraceEnv(4, e.InBinding)
						}
					}
					Traceln(4, "]")

					// End trace
					mark = traceMarkCHR(chr)
					if mark {
						if call == C_last && ie+1 == endIdx {
							call_1 = C_last
						}
						ok, req = traceMatchKeepHead(rs, r, nil, headList, it+1, nt, envOut, nil, call_1)
						traceUnmarkKeepCHR(chr)
						if ok {
							keepIdx.curIdx = ie
							return ok, R_next
						}
						switch req {
						case R_next:
							if call_1 == C_last {
								// reset
								keepIdx.curIdx = keepIdx.startIdx
								TraceHeadln(4, 4, " req=next, reset curIdx <- startIdx ", keepIdx.startIdx)
								return false, R_next
							}
						case R_nextNew:
							if endIdx < len_chr {
								// News
								TraceHeadln(4, 4, " req=nextNew, new CHR found, startIdx, curIdx <- endIdx ", endIdx, " endIdx <- lenChr ", len_chr)
								keepIdx.startIdx, keepIdx.curIdx, ie = endIdx, endIdx, endIdx-1 // for .... ie++ { ...}
								keepIdx.endIdx, endIdx = len_chr, len_chr
								return false, R_next
							} else {
								// old U new
								TraceHeadln(4, 4, " req=nextNew, no new CHR found, old U new ")
								keepIdx.startIdx, keepIdx.curIdx = 0, 0
								return false, R_nextNew
							}
						case R_noNews:
							TraceHeadln(4, 4, " req=noNews ")
							keepIdx.curIdx = ie
							return false, R_noNews
						}
					}
				}
			}
		}
		TraceHeadln(3, 3, "End check stored environment, ie=", ie)
	} // End check in head stored environment map

	// ###
	// normal head-check, start at ie (not at 0 !!)

	if lastKeepHead {
		TraceHeadln(3, 3, "Start last Keep Head from:", ie, " to:", endIdx)
		for ic := ie; ic < endIdx; ic++ {
			chr := chrList[ic]
			env2, ok, mark = traceMarkCHRAndMatchKeepHead(r.id, head2, chr, env)
			// env2, ok, mark = markCHRAndMatchKeepHead(r.id, head2, chr, env)
			if ok {
				if it < EnvCache {
					senv[ic] = &EnvMap{InBinding: env2, OutBindings: map[int]*EnvMap{}}
				}

				TraceHead(4, 3, "New environment ", "Head: ", head.String(), ", Env [", ic, "], =")
				TraceEnv(4, env2)
				Traceln(4, "")

				ok = traceCheckGuards(rs, r, env2)
				if ok {
					traceUnmarkKeepCHR(chr)
					// (*head.EMap)[ienv] = senv
					// TraceEMap(4, 4, head, envMap)
					keepIdx.curIdx = ic + 1
					return ok, R_next
				}
			} else {
				if it < EnvCache {
					senv[ic] = nil
				}
			}
			if mark {
				traceUnmarkKeepCHR(chr)
			}
		}
		TraceHeadln(3, 3, "End check last Keep Head")
		// (*head.EMap)[ienv] = senv
		if call == C_last {
			if endIdx < len_chr {
				// News
				TraceHeadln(3, 3, " lastKeep & call=last, new Chr found, startIdx, keepIdx <- endIdx ", endIdx, " endIdx <- lenChr ", len_chr)
				keepIdx.startIdx, keepIdx.curIdx = endIdx, endIdx
				keepIdx.endIdx = len_chr
				return false, R_next
			} else {
				// old U new
				TraceHeadln(3, 3, " lastKeep & call=last, no new Chr found, old U new ")
				keepIdx.startIdx, keepIdx.curIdx = 0, 0
				return false, R_nextNew
			}
		}
		TraceHeadln(3, 3, " lastKeep & call=noLast, reset curIdx <- startIdx ", keepIdx.startIdx)
		keepIdx.curIdx = keepIdx.startIdx
		return false, R_next
	}

	TraceHeadln(3, 3, "Start check Keep Head from:", ie, " to:", endIdx)
	for ic := ie; ic < endIdx; ic++ {
		chr := chrList[ic]
		env2, ok, mark = traceMarkCHRAndMatchKeepHead(r.id, head2, chr, env)
		// env2, ok, mark = markCHRAndMatchKeepHead(r.id, head2, chr, env) // mark chr and Match, if fail unmark chr
		if ok {

			// trace senv changes

			TraceHead(4, 3, "New environment ", "Head: ", head.String(), ", Env:  [", ic, "], =")
			TraceEnv(4, env2)
			Traceln(4, "")

			if call == C_last && ic+1 == len_chr {
				call_1 = C_last
			} // else call_1 == C_noLast
			if it < EnvCache {
				senv[ic] = &EnvMap{InBinding: env2, OutBindings: map[int]*EnvMap{}}
				ok, req = traceMatchKeepHead(rs, r, nil, headList, it+1, nt, senv[ic], nil, call_1)
			} else {
				ok, req = traceMatchKeepHead(rs, r, nil, headList, it+1, nt, nil, env2, call_1)
			}
			traceUnmarkKeepCHR(chr)
			if ok {
				keepIdx.curIdx = ic
				return ok, R_next
			}
			switch req {
			case R_next:
				if call_1 == C_last {
					// reset
					TraceHeadln(3, 3, " req= next, Reset curIdx <- startIdx ", keepIdx.startIdx)
					keepIdx.curIdx = keepIdx.startIdx
					return false, R_next
				}
			case R_nextNew: // only if call_1 == C_last
				if endIdx < len_chr {
					// News
					TraceHeadln(3, 3, " req= nextNew, new CHR found startIdx, curIdx <- startIdx ", endIdx, " endIdx <- lenChr ", len_chr)
					keepIdx.startIdx, keepIdx.curIdx = endIdx, endIdx
					keepIdx.endIdx = len_chr
					return false, R_next
				} else {
					// old U new
					TraceHeadln(3, 3, " req= nextNew, old U new ")
					keepIdx.startIdx, keepIdx.curIdx = 0, 0
					return false, R_nextNew
				}
			case R_noNews:
				TraceHeadln(3, 3, " req= noNews, curIdx = ", ic)
				keepIdx.curIdx = ic
				return false, R_noNews
			}
		} else {
			if it < EnvCache {
				senv[ic] = nil
			}
		}
		if mark {
			traceUnmarkDelCHR(chr)
		}
	}
	TraceHeadln(3, 3, "End check Keep Head from:", ie, " to:", endIdx)

	if call == C_last && call_1 == C_noLast {
		ok, req = traceHandleLastFail(rs, r, nil, headList, it+1, nt, nil, env)
		switch req {
		case R_next:
			TraceHeadln(3, 3, " after lastFail, req=next old U new")
			keepIdx.startIdx, keepIdx.curIdx = 0, 0
			return false, req
		case R_nextNew:
			if endIdx < len_chr {
				// News
				TraceHeadln(3, 3, " after lastFail, req=nextNew, new CHR found startIdx <- endIdx ", endIdx, " endIdx <- lenChr ", len_chr)
				keepIdx.startIdx, keepIdx.curIdx = endIdx, endIdx
				keepIdx.endIdx = len_chr
				return false, R_next
			} else {
				// old U new
				TraceHeadln(3, 3, " after lastFail, req=nextNew, no new CHR found, old U new ")
				keepIdx.startIdx, keepIdx.curIdx = 0, 0
				return false, R_nextNew
			}
		case R_noNews:
			TraceHeadln(3, 3, " after lastFail, req=noNew ")

			return false, R_noNews
		}

	}

	// (*head.EMap)[ienv] = senv
	return false, R_next
}

//	var env, env2 Bindings
//	var senv map[int]*EnvMap
//	var mark bool

//	if env1 == nil {
//		env = envMap.InBinding
//	} else {
//		env = env1
//	}

//	head := headList[it]
//	head2 := head
//	chrList := readProperConstraintsFromCHR_Store(rs, head, env)
//	TraceHead(4, 3, "match keep-Head ", head, " with [")
//	len_chr := len(chrList)
//	if len_chr == 0 {
//		// variabel in head
//		if head.Functor == "" {
//			// ###
//			Trace(4, " {HeadVariable=", head.Args[0].(Variable), "}")
//			b, ok := GetBinding(head.Args[0].(Variable), env)
//			if !ok {
//				Traceln(3, "] - empty chr (Variable not bind)")
//				return false
//			}
//			if b.Type() != CompoundType {
//				Traceln(3, "] - empty chr (Variable!=CompoundType)", b)
//				return false
//			}
//			bc := b.(Compound)
//			head2 = &bc
//			chrList = readProperConstraintsFromCHR_Store(rs, head2, env)
//			len_chr = len(chrList)
//			if len_chr == 0 {
//				Traceln(3, "] - empty chr (Variable=) ", b)
//				return false
//			}
//		} else {
//			Traceln(3, "] - empty chr")
//			return false
//		}
//	}
//	// begin trace
//	first := true
//	for _, c := range chrList {
//		if first {
//			Trace(4, c)
//			first = false
//		} else {
//			Trace(4, ", ", c)
//		}
//	}
//	Traceln(4, "]")
//	// end trace
//	// begin check the next head

//	lastKeepHead := it+1 == nt
//	TraceHeadln(4, 4, " last keep head = ", lastKeepHead)

//	// End next check next head
//	// check in head stored environment map
//	ie := 0
//	len_ie := 0
//	if env1 == nil {
//		// senv, ok := (*head.EMap)[ienv]
//		senv = envMap.OutBindings
//		//	if !ok {
//		//		TraceHeadln(4, 4, " !!! head: ", head, " with no Emap[ ", ienv, " ]")
//		//	}
//		//	if ok {
//		TraceEMap(4, 4, head, envMap)
//		len_ie = len(senv)
//		TraceHeadln(4, 4, " len env = ", len_ie)
//		if lastKeepHead {
//			ie = len_ie
//			TraceHeadln(4, 4, " ie == len_ie == ", ie, " = ", len_ie)
//		} else {
//			// trace
//			TraceHead(4, 3, "match Keep-Head ", head, " Env: [")
//			first = true
//			for _, e := range senv {
//				if first {
//					first = false
//				} else {
//					Trace(4, ", ")
//				}
//				TraceEnv(4, e.InBinding)
//			}
//			Traceln(4, "]")

//			// End trace
//			for ; ie < len_ie; ie++ {
//				envOut := senv[ie]
//				if envOut != nil {
//					chr := chrList[ie]
//					mark = markCHR(chr)
//					TraceHeadln(4, 4, " mark keep chr:", chr.String(), " = ", mark)
//					if mark {
//						ok = traceMatchKeepHead(rs, r, nil, headList, it+1, nt, envOut, nil)
//						if ok {
//							traceUnmarkKeepCHR(chr)
//							return ok
//						}
//						traceUnmarkKeepCHR(chr)
//					}
//				}
//			}

//		} // ! lastHead
//		//	} else { // if !ok
//		//		// head.EMap = &EnvMap{}
//		//		senv = []Bindings{}
//		//		(*head.EMap)[ienv] = senv
//		//	}
//		// End check in head stored environment map
//	}
//	// normal head-check, start at ie (not at 0 !!)
//	TraceHeadln(4, 3, "match keep-Head ", head, " from: ", ie, " < ", len_chr)
//	if lastKeepHead {
//		for ok, ic := false, ie; !ok && ic < len_chr; ic++ {
//			chr := chrList[ic]
//			env2, ok, mark = traceMarkCHRAndMatchKeepHead(r.id, head2, chr, env)
//			if ok {
//				if it < EnvCache {
//					senv[ic] = &EnvMap{InBinding: env2, OutBindings: map[int]*EnvMap{}}
//				}
//				// trace senv changes

//				TraceHead(4, 3, "New environment ", "Head: ", head.String(), ", Env [", ic, "], =")
//				TraceEnv(4, env2)
//				Traceln(4, "")

//				ok = traceCheckGuards(rs, r, env2)
//				if ok {
//					traceUnmarkKeepCHR(chr)
//					// (*head.EMap)[ienv] = senv
//					TraceEMap(4, 4, head, envMap)
//					return ok
//				}
//			} else {
//				if it < EnvCache {
//					senv[ic] = nil
//				}
//			}
//			if mark {
//				traceUnmarkKeepCHR(chr)
//			}
//		}
//		// (*head.EMap)[ienv] = senv
//		TraceEMap(4, 4, head, envMap)
//		return false
//	}

//	for ok, ic := false, ie; !ok && ic < len_chr; ic++ {

//		chr := chrList[ic]

//		env2, ok, mark = traceMarkCHRAndMatchKeepHead(r.id, head2, chr, env) // mark chr and Match, if fail unmark chr
//		if ok {

//			// trace senv changes

//			TraceHead(4, 3, "New environment ", "Head: ", head.String(), ", Env:  [", ic, "], =")
//			TraceEnv(4, env2)
//			Traceln(4, "")
//			if it < EnvCache {
//				senv[ic] = &EnvMap{InBinding: env2, OutBindings: map[int]*EnvMap{}}
//				ok = traceMatchKeepHead(rs, r, nil, headList, it+1, nt, senv[ic], nil)
//			} else {
//				ok = traceMatchKeepHead(rs, r, nil, headList, it+1, nt, nil, env2)
//			}
//			if ok {
//				traceUnmarkKeepCHR(chr)
//				// (*head.EMap)[ienv] = senv
//				TraceEMap(4, 4, head, envMap)
//				return ok
//			}
//		} else {
//			if it < EnvCache {
//				senv[ic] = nil
//			}
//		}
//		if mark {
//			unmarkDelCHR(chr)
//		}
//	}
//	// (*head.EMap)[ienv] = senv
//	TraceEMap(4, 4, head, envMap)
//	return false
//}

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
			if con != nil && !con.IsDeleted {
				con1, ok := SubstituteBiEnv(*con, biEnv)
				if ok && con1.Type() == CompoundType {
					newCHR = append(newCHR, con1.(Compound))
					con.IsDeleted = true
				}
			}
		}
		for _, con := range aChr.noArg {
			if con != nil && !con.IsDeleted {
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
				if con != nil && !con.IsDeleted {
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

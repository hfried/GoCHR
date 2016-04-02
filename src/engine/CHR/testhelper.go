// Copyright © 2016 The Carneades Authors
// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL
// was not distributed with this file, You can obtain one
// at http://mozilla.org/MPL/2.0/.

package chr

import (
	"fmt"
	. "github.com/hfried/GoCHR/src/engine/parser"
	. "github.com/hfried/GoCHR/src/engine/terms"
	"testing"
)

func teval(t *testing.T, str1 string, result string) bool {

	term1, ok := ReadString(str1)
	if !ok {
		t.Errorf(fmt.Sprintf("Scan str1 in test eval \"%s\" failed, term1: %s", str1, term1.String()))
		return false
	}
	term2, ok := ReadString(result)
	if !ok {
		t.Errorf(fmt.Sprintf("Scan result in test eval \"%s\" failed, term2: %s", result, term2.String()))
		return false
	}
	term3 := Eval(term1)
	if Equal(term3, term2) {
		fmt.Printf(" '%s' eval to:'%s' == '%s'\n", term1.String(), term3.String(), term2.String())
		return true
	}
	fmt.Printf(" '%s' eval to:'%s' NOT= '%s'\n", term1.String(), term3.String(), term2)

	return false

}

func tAtt(t *testing.T, store string, head string, result string) bool {

	term1, ok := ParseGoalString(store)
	if !ok {
		t.Errorf(fmt.Sprintf("Scan store in add/read store test \"%s\" failed, term1: %s", store, term1.String()))
		return false
	}
	term2, ok := ParseRuleBodyString(head)
	if !ok {
		t.Errorf(fmt.Sprintf("Scan head in add/read store test \"%s\" failed, term2: %s", head, term2.String()))
		return false
	}
	term3, ok := ParseRuleBodyString(result)
	if !ok {
		t.Errorf(fmt.Sprintf("Scan result add/read store test \"%s\" failed, term3: %s", result, term3.String()))
		return false
	}
	InitStore()
	switch term1.Type() {
	case ListType:
		fmt.Printf(" store [")
		for _, g := range term1.(List) {
			if g.Type() == CompoundType {
				addConstraintToStore(g.(Compound))
				fmt.Printf("%s, ", g)
			} else {
				fmt.Printf(" no CHR predicate: %s \n", g)
			}
		}
		fmt.Printf("]\n")
	case CompoundType:
		addConstraintToStore(term1.(Compound))
		fmt.Printf("store [%s]\n", term1)
	default:
		fmt.Printf(" no CHR predicate or list: %s \n", term1)
	}

	if term2.Type() == ListType {
		t2 := term2.(List)
		if len(t2) > 0 {
			term2 = t2[0]
		}
	}
	if term2.Type() != CompoundType {
		fmt.Printf(" head must be a predicate, not %s", term2)
		return false
	}
	att := CList{}
	fmt.Printf(" Head: %s ", term2)
	t2 := term2.(Compound)
	if term2.(Compound).Prio == 0 {
		att = readProperConstraintsFromCHR_Store(&t2, nil)
	} else {
		att = readProperConstraintsFromBI_Store(&t2, nil)
	}
	if term3.Type() != ListType {
		fmt.Printf(" result is not a list %s \n", term3)
	}
	cl := term3.(List)

	if len(cl) != len(att) {
		fmt.Printf("\n length of result not OK (exspected)%d != (computed)%d\n", len(cl), len(att))
		fmt.Printf(" Select: [")
		for _, a := range att {
			fmt.Printf("%s ,", a)
		}
		fmt.Printf("]\n")
		return false

	}
	fmt.Printf(" Select: [")
	for i, a := range att {
		if !Equal(*a, cl[i]) {
			fmt.Printf(" term %d (%s) is not equal result %d (%s) \n", i, a, i, cl[i])
			return false
		}
		fmt.Printf("%s ,", a)
	}

	for i, c := range cl {
		if !Equal(c, *att[i]) {
			fmt.Printf(" term %d (%s) is not equal result %d (%s) \n", i, att[i], i, c)
			return false
		}
	}

	fmt.Printf("]\n")
	return true

}

func tAddStringChrRule(t *testing.T, name, keep, del, guard, body string) bool {

	keepList, ok := ParseHeadString(keep)
	if !ok || keepList.Type() != ListType {
		t.Errorf(fmt.Sprintf("Scan KEEP-Head in rule %s failed(%v): %s\n", name, ok, keepList))
		return false
	}
	cKeepList, ok := toClist(keepList)
	if !ok {
		t.Errorf(fmt.Sprintf("Convert Keep-Head in rule %s failed: %s\n", name, keepList))
		return false
	}

	delList, ok := ParseHeadString(del)
	if !ok || delList.Type() != ListType {
		t.Errorf(fmt.Sprintf("Scan DEl-Head in rule %s failed: %s\n", name, delList))
		return false
	}
	cDelList, ok := toClist(delList)
	if !ok {
		t.Errorf(fmt.Sprintf("Convert DEl-Head in rule %s failed: %s\n", name, delList))
		return false
	}

	guardList, ok := ParseBIString(guard)
	if !ok || guardList.Type() != ListType {
		t.Errorf(fmt.Sprintf("Scan GUARD in rule %s failed: %s (%v)\n", name, guardList, ok))
		return false
	}
	cGuardList, ok := toClist(guardList)
	if !ok {
		t.Errorf(fmt.Sprintf("Convert GUARD in rule %s failed: %s\n", name, guardList))
		return false
	}

	bodyList, ok := ParseRuleBodyString(body)
	if !ok || bodyList.Type() != ListType {
		t.Errorf(fmt.Sprintf("Scan BODY in rule %s failed: %s\n", name, bodyList))
		return false
	}

	CHRruleStore = append(CHRruleStore, &chrRule{name: name, id: nextRuleId,
		delHead:  cDelList,
		keepHead: cKeepList,
		guard:    cGuardList,
		body:     bodyList.(List)})
	nextRuleId++
	return true

}

func tAddStringGoals(t *testing.T, goals string) bool {
	goalList, ok := ParseGoalString(goals)
	if !ok || goalList.Type() != ListType {
		t.Errorf(fmt.Sprintf("Scan GOAL-List failed: %s\n", goalList))
		return false
	}
	for _, g := range goalList.(List) {
		if g.Type() == CompoundType {
			addConstraintToStore(g.(Compound))
		} else {
			t.Errorf(fmt.Sprintf(" GOAL is not a predicate: %s\n", g))
			return false
		}

	}
	return true
}

func checkResult(t *testing.T, chr, bi string) {
	if chr != chr2string() {
		t.Error(fmt.Sprintf(" exspected chr result: '%s' \n !=computed chr result: '%s'", chr, chr2string()))
	}
	if bi != bi2string() {
		t.Error(fmt.Sprintf(" exspected BI result: '%s' \n !=computed BI result: '%s'", bi, bi2string()))
	}
}

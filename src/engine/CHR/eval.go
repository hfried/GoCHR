// Copyright © 2016 The Carneades Authors
// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL
// was not distributed with this file, You can obtain one
// at http://mozilla.org/MPL/2.0/.

// eval terms in Constraint Handling Rules

package chr

import (
	"fmt"
	"strings"

	. "github.com/hfried/GoCHR/src/engine/terms"
)

func Eval(t1 Term) Term {
	switch t1.Type() {
	case AtomType, BoolType, IntType, FloatType, StringType:
		return t1
	case CompoundType:

		args := []Term{}
		tArgs := []Type{}
		for _, a := range t1.(Compound).Args {
			a = Eval(a)
			args = append(args, a)
			tArgs = append(tArgs, a.Type())
		}
		t2 := t1.(Compound)
		t2.Args = args
		t1 = t2
		if t1.(Compound).Prio != 0 {
			an := len(args)
			switch an {
			case 1:
				return evalUnaryOperator(t1, args[0], tArgs[0])
			case 2:
				return evalBinaryOperator(t1, args[0], tArgs[0], args[1], tArgs[1])
			default:
				return evalN_aryOperator(t1, args, tArgs, an)
			}
		}
	case ListType:
		t2 := t1.(List)
		lent2 := len(t2)
		if lent2 == 0 {
			return t1
		}
		lent2m1 := lent2 - 1
		last := t2[lent2m1]
		t3 := List{}
		if last.Type() == CompoundType && last.(Compound).Functor == "|" {
			for i := 0; i < lent2m1; i++ {
				t3 = append(t3, Eval(t2[i]))
			}
			t4 := last.(Compound).Args[0]
			if t4.Type() == ListType {
				for _, t5 := range t4.(List) {
					t3 = append(t3, Eval(t5))
				}
			}
			t1 = t3
		} else {

			for _, t4 := range t2 {
				t3 = append(t3, Eval(t4))
			}
			t1 = t3
		}
	}
	return t1
}

func evalUnaryOperator(t1, arg Term, typ Type) Term {
	switch t1.(Compound).Functor {
	case "+":
		return arg
	case "-":
		return evalUnaryMinus(t1, arg, typ)
	case "!", "¬":
		return evalNot(t1, arg, typ)
	case "^":
		return evalComp(t1, arg, typ)
	}
	return t1
}

func evalBinaryOperator(t1, a1 Term, typ1 Type, a2 Term, typ2 Type) Term {
	switch t1.(Compound).Functor {
	case "*":
		return evalTimes(t1, a1, typ1, a2, typ2)
	case "/":
		return evalDivision(t1, a1, typ1, a2, typ2)
	case "div":
		return evalDiv(t1, a1, typ1, a2, typ2)
	case "%", "mod":
		return evalMod(t1, a1, typ1, a2, typ2)
	case "&":
		return evalBitAnd(t1, a1, typ1, a2, typ2)
	case "&^":
		return evalBitAndNot(t1, a1, typ1, a2, typ2)
	case "<<":
		return evalLeftShift(t1, a1, typ1, a2, typ2)
	case ">>":
		return evalRightShift(t1, a1, typ1, a2, typ2)
	case "+":
		return evalPlus(t1, a1, typ1, a2, typ2)
	case "-":
		return evalMinus(t1, a1, typ1, a2, typ2)
	case "^":
		return evalBitXOr(t1, a1, typ1, a2, typ2)
	case "or":
		return evalBitOr(t1, a1, typ1, a2, typ2)
	case "==":
		return evalEq(t1, a1, typ1, a2, typ2)
	case "!=":
		return evalNotEq(t1, a1, typ1, a2, typ2)
	case "<":
		return evalLess(t1, a1, typ1, a2, typ2)
	case "<=", "=<":
		return evalLessEq(t1, a1, typ1, a2, typ2)
	case ">":
		return evalGt(t1, a1, typ1, a2, typ2)
	case ">=":
		return evalGtEq(t1, a1, typ1, a2, typ2)
	case "&&":
		return evalLogAnd(t1, a1, typ1, a2, typ2)
	case "||":
		return evalLogOr(t1, a1, typ1, a2, typ2)
	case "|":
		return evalCons(t1, a1, typ1, a2, typ2)
	}
	return t1
}

func evalN_aryOperator(t1 Term, args []Term, typs []Type, n int) Term {
	switch t1.(Compound).Functor {
	case "sprint":

	}
	return t1
}

func evalUnaryMinus(t1 Term, a1 Term, typ1 Type) Term {
	// -a1
	switch typ1 {
	case IntType:
		return -a1.(Int)
	case FloatType:
		return -a1.(Float)
	}
	return t1
}

func evalNot(t1 Term, a1 Term, typ1 Type) Term {
	// !a1 or ¬a1
	if typ1 == BoolType {
		return !a1.(Bool)
	}
	if typ1 == CompoundType {
		a := a1.(Compound)
		n := len(a.Args)
		if n == 1 {
			switch a.Functor {
			case "!", "¬":
				return a.Args[0]
			}
			return t1
		}
		if n == 2 {
			c := Compound{}
			newc := false
			args := a.Args
			arg1 := args[0]
			arg2 := args[1]
			switch a.Functor {
			case "<":
				c = Compound{Functor: "<=", Args: []Term{arg2, arg1}}
				newc = true
			case "<=":
				c = Compound{Functor: "<", Args: []Term{arg2, arg1}}
				newc = true
			case "==":
				c = Eval(Term(Compound{Functor: "!=", Prio: 3, Args: args})).(Compound)
				newc = true
			case "!=":
				c = Eval(Term(Compound{Functor: "==", Prio: 3, Args: args})).(Compound)
				newc = true
			}
			if newc {
				c.Id = a.Id
				c.Prio = a.Prio
				c.IsDeleted = a.IsDeleted
				return c
			}
		}
	}
	return t1
}

func evalComp(t1 Term, a1 Term, typ1 Type) Term {
	// ^a1
	if typ1 == IntType {
		return ^a1.(Int)
	}
	return t1
}

func evalTimes(t1 Term, a1 Term, typ1 Type, a2 Term, typ2 Type) Term {
	// a1 * a2
	switch typ1 {
	case IntType:
		switch typ2 {
		case IntType:
			return a1.(Int) * a2.(Int)
		case FloatType:
			return Float(float64(a1.(Int)) * float64(a2.(Float)))
		default:
			return t1
		}
	case FloatType:
		switch typ2 {
		case IntType:
			return Float(float64(a1.(Float)) * float64(a2.(Int)))
		case FloatType:
			return a1.(Float) * a2.(Float)
		default:
			return t1
		}
	}
	return t1
}

func evalDivision(t1 Term, a1 Term, typ1 Type, a2 Term, typ2 Type) Term {
	// if a2 != 0 { a1 / a2 }
	switch typ1 {
	case IntType:
		switch typ2 {
		case IntType:
			if a2.(Int) != 0 {
				return a1.(Int) / a2.(Int)
			}
		case FloatType:
			if a2.(Float) != 0.0 {
				return Float(float64(a1.(Int)) / float64(a2.(Float)))
			}
		default:
			return t1
		}
	case FloatType:
		switch typ2 {
		case IntType:
			if a2.(Int) != 0 {
				return Float(float64(a1.(Float)) / float64(a2.(Int)))
			}
		case FloatType:
			if a2.(Float) != 0.0 {
				return a1.(Float) / a2.(Float)
			}
		default:
			return t1
		}
	}
	return t1
}

func evalDiv(t1 Term, a1 Term, typ1 Type, a2 Term, typ2 Type) Term {
	// a1 / a2 for integer
	if typ1 != IntType || typ2 != IntType || a2.(Int) == 0 {
		return t1
	}
	return a1.(Int) / a2.(Int)
}

func evalMod(t1 Term, a1 Term, typ1 Type, a2 Term, typ2 Type) Term {
	// a1 % a2 for integer
	if typ1 != IntType || typ2 != IntType || a2.(Int) == 0 {
		return t1
	}
	return a1.(Int) % a2.(Int)
}

func evalBitAnd(t1 Term, a1 Term, typ1 Type, a2 Term, typ2 Type) Term {
	// a1 & a2 for integer
	if typ1 != IntType || typ2 != IntType {
		return t1
	}
	return a1.(Int) & a2.(Int)
}

func evalBitAndNot(t1 Term, a1 Term, typ1 Type, a2 Term, typ2 Type) Term {
	// a1 &^ a2 for integer
	if typ1 != IntType || typ2 != IntType {
		return t1
	}
	return a1.(Int) &^ a2.(Int)
}

func evalLeftShift(t1 Term, a1 Term, typ1 Type, a2 Term, typ2 Type) Term {
	// a1 << a2 for integer
	if typ1 != IntType || typ2 != IntType {
		return t1
	}
	return Int(uint(a1.(Int)) << uint(a2.(Int)))
}

func evalRightShift(t1 Term, a1 Term, typ1 Type, a2 Term, typ2 Type) Term {
	// a1 >> a2 for integer
	if typ1 != IntType || typ2 != IntType {
		return t1
	}
	return Int(uint(a1.(Int)) >> uint(a2.(Int)))
}

func evalPlus(t1 Term, a1 Term, typ1 Type, a2 Term, typ2 Type) Term {
	switch typ1 {
	case IntType:
		switch typ2 {
		case IntType:
			return a1.(Int) + a2.(Int)
		case FloatType:
			return Float(float64(a1.(Int)) + float64(a2.(Float)))
		default:
			return t1
		}
	case FloatType:
		switch typ2 {
		case IntType:
			return Float(float64(a1.(Float)) + float64(a2.(Int)))
		case FloatType:
			return a1.(Float) + a2.(Float)
		default:
			return t1
		}
	case StringType:
		if typ2 == StringType {
			return a1.(String) + a2.(String)
		}
	}
	return t1
}

func evalMinus(t1 Term, a1 Term, typ1 Type, a2 Term, typ2 Type) Term {
	switch typ1 {
	case IntType:
		switch typ2 {
		case IntType:
			return a1.(Int) - a2.(Int)
		case FloatType:
			return Float(float64(a1.(Int)) - float64(a2.(Float)))
		default:
			return t1
		}
	case FloatType:
		switch typ2 {
		case IntType:
			return Float(float64(a1.(Float)) - float64(a2.(Int)))
		case FloatType:
			return a1.(Float) - a2.(Float)
		default:
			return t1
		}
	}
	return t1
}

func evalBitOr(t1 Term, a1 Term, typ1 Type, a2 Term, typ2 Type) Term {
	// a1 or a2 == a1 | a2 for integer
	if typ1 != IntType || typ2 != IntType {
		return t1
	}
	return a1.(Int) | a2.(Int)
}

func evalBitXOr(t1 Term, a1 Term, typ1 Type, a2 Term, typ2 Type) Term {
	// a1 ^ a2 for integer
	if typ1 != IntType || typ2 != IntType {
		return t1
	}
	return Int(uint(a1.(Int)) ^ uint(a2.(Int)))
}

func evalEq(t1 Term, a1 Term, typ1 Type, a2 Term, typ2 Type) Term {
	// a1 == a2
	switch typ1 {
	case IntType:
		switch typ2 {
		case IntType:
			return Bool(a1.(Int) == a2.(Int))
		case FloatType:
			return Bool(float64(a1.(Int)) == float64(a2.(Float)))
		default:
			return t1
		}
	case FloatType:
		switch typ2 {
		case IntType:
			return Bool(float64(a1.(Float)) == float64(a2.(Int)))
		case FloatType:
			return Bool(a1.(Float) == a2.(Float))
		default:
			return t1
		}
	case StringType:
		if typ2 == StringType {
			return Bool(a1.(String) == a2.(String))
		}
	case BoolType:
		if typ2 == BoolType {
			return Bool(a1.(Bool) == a2.(Bool))
		}
	default:
		if Equal(a1, a2) {
			return Bool(true)
		}
	}
	return t1
}

func evalNotEq(t1 Term, a1 Term, typ1 Type, a2 Term, typ2 Type) Term {
	// a1 != a2
	switch typ1 {
	case IntType:
		switch typ2 {
		case IntType:
			return Bool(a1.(Int) != a2.(Int))
		case FloatType:
			return Bool(float64(a1.(Int)) != float64(a2.(Float)))
		default:
			return t1
		}
	case FloatType:
		switch typ2 {
		case IntType:
			return Bool(float64(a1.(Float)) != float64(a2.(Int)))
		case FloatType:
			return Bool(a1.(Float) != a2.(Float))
		default:
			return t1
		}
	case StringType:
		if typ2 == StringType {
			return Bool(a1.(String) != a2.(String))
		}
	case BoolType:
		if typ2 == BoolType {
			return Bool(a1.(Bool) != a2.(Bool))
		}
	default:
		if Equal(a1, a2) {
			return Bool(false)
		}
	}
	return t1
}

func evalLess(t1 Term, a1 Term, typ1 Type, a2 Term, typ2 Type) Term {
	// a1 < a2
	switch typ1 {
	case IntType:
		switch typ2 {
		case IntType:
			return Bool(a1.(Int) < a2.(Int))
		case FloatType:
			return Bool(float64(a1.(Int)) < float64(a2.(Float)))
		default:
			return t1
		}
	case FloatType:
		switch typ2 {
		case IntType:
			return Bool(float64(a1.(Float)) < float64(a2.(Int)))
		case FloatType:
			return Bool(a1.(Float) < a2.(Float))
		default:
			return t1
		}
	case StringType:
		if typ2 == StringType {
			return Bool(a1.(String) < a2.(String))
		}
	}
	return t1
}

func evalLessEq(t1 Term, a1 Term, typ1 Type, a2 Term, typ2 Type) Term {
	// a1 <= a2 or a1 =< a2
	switch typ1 {
	case IntType:
		switch typ2 {
		case IntType:
			return Bool(a1.(Int) <= a2.(Int))
		case FloatType:
			return Bool(float64(a1.(Int)) <= float64(a2.(Float)))
		default:
			return t1
		}
	case FloatType:
		switch typ2 {
		case IntType:
			return Bool(float64(a1.(Float)) <= float64(a2.(Int)))
		case FloatType:
			return Bool(a1.(Float) <= a2.(Float))
		default:
			return t1
		}
	case StringType:
		if typ2 == StringType {
			return Bool(a1.(String) <= a2.(String))
		}
	}
	return t1
}

func evalGt(t1 Term, a1 Term, typ1 Type, a2 Term, typ2 Type) Term {
	// a1 > a2
	switch typ1 {
	case IntType:
		switch typ2 {
		case IntType:
			return Bool(a1.(Int) > a2.(Int))
		case FloatType:
			return Bool(float64(a1.(Int)) > float64(a2.(Float)))
		}
	case FloatType:
		switch typ2 {
		case IntType:
			return Bool(float64(a1.(Float)) > float64(a2.(Int)))
		case FloatType:
			return Bool(a1.(Float) > a2.(Float))
		}
	case StringType:
		if typ2 == StringType {
			return Bool(a1.(String) > a2.(String))
		}
	}
	t := t1.(Compound)
	c := Compound{Functor: "<", Id: t.Id, Prio: t.Prio, IsDeleted: t.IsDeleted, Args: []Term{a2, a1}}
	return c
}

func evalGtEq(t1 Term, a1 Term, typ1 Type, a2 Term, typ2 Type) Term {
	// a1 >= a2
	switch typ1 {
	case IntType:
		switch typ2 {
		case IntType:
			return Bool(a1.(Int) >= a2.(Int))
		case FloatType:
			return Bool(float64(a1.(Int)) >= float64(a2.(Float)))
		}
	case FloatType:
		switch typ2 {
		case IntType:
			return Bool(float64(a1.(Float)) >= float64(a2.(Int)))
		case FloatType:
			return Bool(a1.(Float) >= a2.(Float))
		}
	case StringType:
		if typ2 == StringType {
			return Bool(a1.(String) >= a2.(String))
		}
	}
	t := t1.(Compound)
	c := Compound{Functor: "<=", Id: t.Id, Prio: t.Prio, IsDeleted: t.IsDeleted, Args: []Term{a2, a1}}
	return c
}

func evalLogAnd(t1 Term, a1 Term, typ1 Type, a2 Term, typ2 Type) Term {
	// a1 && a2
	if typ1 == BoolType && typ2 == BoolType {
		return a1.(Bool) && a2.(Bool)
	}
	if typ1 == BoolType {
		if a1.(Bool) {
			return a2
		} else {
			return Bool(false)
		}
	}
	if typ2 == BoolType {
		if a2.(Bool) {
			return a1
		} else {
			return Bool(false)
		}
	}
	if Equal(a1, Eval(Compound{Functor: "!", Prio: 6, Args: []Term{a2}})) {
		return Bool(false)
	}
	return t1
}

func evalLogOr(t1 Term, a1 Term, typ1 Type, a2 Term, typ2 Type) Term {
	// a1 || a2
	if typ1 == BoolType && typ2 == BoolType {
		return a1.(Bool) || a2.(Bool)
	}
	if typ1 == BoolType {
		if a1.(Bool) {
			return a1
		} else {
			return a2
		}
	}
	if typ2 == BoolType {
		if a2.(Bool) {
			return a2
		} else {
			return a1
		}
	}
	a3 := Eval(Compound{Functor: "!", Prio: 6, Args: []Term{a2}})
	// fmt.Printf(" A1: %s, A2: %s, Eval !A2: %s Equal(A1,!A2) %v\n", a1, a2, a3, Equal(a1, a3))
	if Equal(a1, a3) {
		return Bool(true)
	}
	return t1
}

func evalCons(t1 Term, a1 Term, typ1 Type, a2 Term, typ2 Type) Term {
	// fmt.Printf(" Start EvalCons A1: %s, A2: %s, \n", a1, a2)
	t2 := List{}
	if typ1 == ListType {
		for _, e := range a1.(List) {
			t2 = append(t2, e)
		}
	} else {
		t2 = append(t2, a1)
	}

	if typ2 == ListType {
		for _, e := range a2.(List) {
			t2 = append(t2, e)
		}
	} else {
		t2 = append(t2, a2)
	}
	// fmt.Printf(" Eval-Cons:  A1: '%s', | A2: '%s', == '%s' \n", a1, a2, t2)
	return t2
}

var Char2SpellMap = map[rune]string{
	'1': "Eins", '2': "Zwei", '3': "Drei", '4': "Vier", '5': "Fünf",
	'6': "Sechs", '7': "Sieben", '8': "Acht", '9': "Neun", '0': "Null",
	'ß': "Eszet",
	'.': "Punkt", '/': "Schrägstrich", '@': "Ät", '%': "Prozent", '-': "Bindestrich",
	'_': "Unterstrich", '#': "Raute", ':': "Doppelpunkt", ',': "Komma", ';': "Semikolon",
	'|': "Senkrechter Strich", '\'': "Hochkomma",
	'=': "Gleich", '&': "Und", '!': "Ausrufezeichen", '?': "Fragezeichen",
	'+': "Plus", '*': "Sternchen", '´': "Akut-Akzent", '`': "Gravis-Akzent", '~': "Tilde",
	'(': "Runde Klammer auf", ')': "Runde Klammer zu", '{': "Geschweifte Klammer auf", '}': "Geschweifte Klammer zu",
	'[': "Eckige Klammer auf", ']': "Eckige Klammer zu", '<': "Spitze Klammer auf", '>': "Spitze Klammer zu",
	'^': "Hochzeichen", '°': "Grad-Zeichen",
	'"': "Anführungszeichen", '§': "Paragraph", '$': "Dollarzeichen",
	'\\': "Umgekehrter Schrägstrich", '€': "Euro"}

var initSpell2CharMap = false

// Synonyme
var Spell2CharMap = map[string]rune{
	"stern": '*', "at": '@', "klammeraffe": '@',
	"minus": '-', "minuszeichen": '-',
	"apostroph":        '\'',
	"anführungsstrich": '"', "anführungsstriche": '"',
	"anführungszeichen": '"', "gänsefüßchen": '"',
	"backslash": '\\', "rückschrägstrich": '\\',
	"einschaltungszeichen": '^', "einschaltzeichen": '^', "häkchen": '^', "dach": '^',
	"grad": '°', "gleichheitszeichen": '=',
	"strichpunkt": ';', "nummernzeichen": '#', "doppelkreuz": '#',
	// fFlLmMnNrRsS

	"ah": 'A', "be": 'B', "zeh": 'C', "de": 'D', "eh": 'E',
	"ef": 'F', "geh": 'G', "ha": 'H', "ie": 'I', "jot": 'J',
	"ka": 'K', "el": 'L', "em": 'M', "en": 'N', "oh": 'O', "pe": 'P', "kuh": 'Q', "qu": 'Q',
	"er": 'R', "es": 'S', "tee": 't', "uh": 'U', "Pfau": 'V', "weh": 'W',
	"ixs": 'x', "ysilon": 'y', "zet": 'Z'}

var Spell2WordsMap = map[string]map[string]rune{
	"ist":         {"gleich": '='},
	"senkrechter": {"strich": '|'},
	"vertikaler":  {"strich": '|'},
	"umgekehrter": {"schrägstrich": '\\'},
	"rückwärts":   {"schrägstrich": '\\'},
	"accent":      {"aigu": '´', "akut": '´', "grave": '`', "gravis": '`'},
	"akzent":      {"aigu": '´', "akut": '´', "grave": '`', "gravis": '`'},
	"kleiner":     {"als": '<'}, "größer": {"als": '>'},
	"einfaches": {"ausführungszeichen": '\'', "anführungszeichen": '\''},
	"scharfes":  {"es": 'ß', "s": 'ß', "S": 'ß', "Es": 'ß'},
	"es":        {"zet": 'ß', "z": 'ß'},
	"new":       {"york": 'N'},
	"doppeltes": {"anführungszeichen": '"'}}

var Spell3WordsMap = map[string]map[string]map[string]rune{
	"runde":        {"klammer": {"auf": '(', "zu": ')'}},
	"geschweifte":  {"klammer": {"auf": '{', "zu": '}'}},
	"geschwungene": {"klammer": {"auf": '{', "zu": '}'}},
	"eckige":       {"klammer": {"auf": '[', "zu": ']'}},
	"spitze":       {"klammer": {"auf": '<', "zu": '>'}},
}

func InitSpell2CharMap() {
	for runeVar, strVar := range Char2SpellMap {
		Spell2CharMap[strings.ToLower(strVar)] = runeVar
	}
	initSpell2CharMap = true
}

func selectOneRune(m map[string]rune, str string) rune {
	r, ok := m[str]
	if ok {
		// fmt.Println(" One Rune >", string(r), "<")
		return r
	}
	for _, val := range m {
		r = val
		break
	}
	// fmt.Println(" Ersatz: = >", string(r), "<")
	return r
}

func Spell2text(spell Term) (Term, bool) {
	if spell.Type() == ListType {
		if !initSpell2CharMap {
			InitSpell2CharMap()
		}
		spell3Words2rest := map[string]map[string]rune{}
		spell3Words1rest := map[string]rune{}
		spell2Words1rest := map[string]rune{}
		list := spell.(List)
		var text String
		var r rune
		var first rune
		var ok bool
		for _, ele := range list {
			if ele.Type() == StringType {
				s := ele.(String)
				str := string(s)
				// fmt.Println("String ", idx, "=", str)

				l := len(str)
				if l < 3 {
					continue
				}
				str = strings.ToLower(str[1 : l-1])
				for _, ru := range str {
					first = ru
					break
				}
				if len(spell3Words1rest) == 0 {
					if len(spell3Words2rest) == 0 {
						if len(spell2Words1rest) == 0 {
							spell3Words2rest, ok = Spell3WordsMap[str]
							if ok {
								// fmt.Println("Spell 3 WordsMap OK:", str)
								continue
							}
							spell2Words1rest, ok = Spell2WordsMap[str]
							if ok {
								// fmt.Println("Spell 2 WordsMap OK:", str)
								continue
							}
							r, ok = Spell2CharMap[str]
							if ok {
								text += String(r)
							} else {
								// "ch..." oder "Ch..."
								if (str[0] == 'c' || str[0] == 'C') && l > 3 && str[1] == 'h' {
									if str == "Charly" || str == "Charlie" {
										text += String("C")
									} else {
										text += String("CH")
									}
									continue
								}
								if (str[0] == 'S' || str[0] == 's') && l > 4 && str[1] == 'c' && str[2] == 'h' {
									text += String("SCH")
									continue
								}
								text += String(first) // main case: the first letter
							}
						} else { // len(spell2Words1rest) > 0
							// fmt.Println("in spell 2 Words 1 rest")
							text += String(selectOneRune(spell2Words1rest, str))
							spell2Words1rest = map[string]rune{}
						}
					} else { // len(spell3Words2rest) > 0
						// fmt.Println("Klammer = ", str)
						spell3Words1rest = spell3Words2rest["Klammer"]
						spell3Words2rest = map[string]map[string]rune{}
					}
				} else { // len(spell3Words1rest) > 0
					// fmt.Println("in spell 3 Words 1 rest")
					text += String(selectOneRune(spell3Words1rest, str))
					spell3Words1rest = map[string]rune{}
				}
			} else if ele.Type() == CompoundType {
				comp := ele.(Compound)
				// fmt.Println("  ############# Functor: >", comp.Functor, "< #########")
				if comp.Functor == "zahl" {
					text = text + String(fmt.Sprint(comp.Args[0]))
				} else if len(comp.Args) > 1 && comp.Args[1].Type() == StringType {
					text = text + String(comp.Args[1].(String)[0])
				}

			}
		}
		text = "\"" + text + "\""
		return text, true
	}
	return spell, false
}

func Text2spell(text Term) (Term, bool) {
	var spell List
	if text.Type() == StringType {
		str := text.(String)
		// fmt.Println("Str:", str)
		l := len(str)
		if l < 3 {
			return spell, false
		}
		str = str[1 : l-1]
		for _, ele := range str {

			s, ok := Char2SpellMap[ele]
			if !ok {
				s = string(ele)
			}
			spell = append(spell, String(s))
		}
		return list2cstring(spell), true
	}
	return spell, false
}

func CheckSpellAndText(spell Term, text Term) bool {
	//if text.Type
	return false
}

func Email2text(spell Term) (Term, bool) {
	if spell.Type() == ListType {
		if !initSpell2CharMap {
			InitSpell2CharMap()
		}
		list := spell.(List)
		last := len(list) - 1
		var text String
		var r rune
		var ok bool
		for idx, ele := range list {
			if ele.Type() == StringType {
				s := ele.(String)
				str := string(s)
				// fmt.Println("String ", idx, "=", str)

				l := len(str)
				if l < 3 {
					continue
				}
				str = strings.ToLower(str[1 : l-1])

				r, ok = Spell2CharMap[str]
				if ok && idx != last {
					text += String(r)
				} else {
					text += String(str) // main case: the first letter
				}

			}
		}
		text = "\"" + text + "\""
		return text, true
	}
	return spell, false
}

func Text2email(text Term) (Term, bool) {
	var spell List
	if text.Type() == StringType {
		str := text.(String)
		// fmt.Println("Str:", str)
		l := len(str)
		if l < 3 {
			return spell, false
		}
		str = str[1 : l-1]
		substr := ""
		for _, ele := range str {

			s, ok := Char2SpellMap[ele]
			if ok {
				if substr == "" {
					spell = append(spell, String(s))
				} else {
					spell = append(spell, String(substr))
					substr = ""
					spell = append(spell, String(s))
				}
			} else {
				substr += string(ele)
			}

		}
		if substr != "" {
			spell = append(spell, String(substr))
		}
		return list2cstring(spell), true
	}
	return spell, false
}

func list2cstring(list Term) Term {
	text := ""
	if list.Type() == ListType {
		l := list.(List)
		for idx, ele := range l {
			if ele.Type() == StringType {
				s := ele.(String)
				str := string(s)
				if idx == 0 {
					text = str
				} else {
					text = text + ", " + str
				}
			}
		}
		text = "\"" + text + "\""
		return String(text)
	}
	return list
}

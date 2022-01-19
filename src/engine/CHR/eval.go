// Copyright © 2016 The Carneades Authors
// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL
// was not distributed with this file, You can obtain one
// at http://mozilla.org/MPL/2.0/.

// eval terms in Constraint Handling Rules

package chr

import (
	"fmt"
	"strconv"
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
		case StringType:
			s := string(a2.(String))
			s = s[1 : len(s)-1]
			s = "\"" + strconv.Itoa(int(a1.(Int))) + s + "\""
			// fmt.Println(" >>> i + s >>>>> ", s)
			return String(s)
		default:
			return t1
		}
	case FloatType:
		switch typ2 {
		case IntType:
			return Float(float64(a1.(Float)) + float64(a2.(Int)))
		case FloatType:
			return a1.(Float) + a2.(Float)
		case StringType:
			s := string(a2.(String))
			s = s[1:]
			s = fmt.Sprintf("\"%g", float64(a1.(Float))) + s
			// fmt.Println(" >>> f + s >>>>> ", s)
			return String(s)
		default:
			return t1
		}
	case StringType:
		switch typ2 {
		case StringType:
			s1 := string(a1.(String))
			s2 := string(a2.(String))
			s1 = s1[:len(s1)-1] + s2[1:]
			// fmt.Println(" >> s + s >>>>>> ", s1)
			return String(s1)
		case IntType:
			s := string(a1.(String))
			s = s[1 : len(s)-1]
			s = "\"" + s + strconv.Itoa(int(a2.(Int))) + "\""
			//fmt.Println(" >> s + i >>>>>> ", s)
			return String(s)
		case FloatType:
			s := string(a1.(String))
			s = s[:len(s)-1]
			s = s + fmt.Sprintf("%g\"", float64(a2.(Float)))
			// fmt.Println(" >> s + f >>>>>> ", s)
			return String(s)
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
	'\\': "Umgekehrter Schrägstrich", '€': "Euro",
	' ': "Leerzeichen"}

var initSpellAndStructMap = false

// Synonyme
var Spell2CharMap = map[string]rune{
	"stern": '*', "at": '@', "klammeraffe": '@',
	"affenschwanz": '@', "affenohr": '@', "affenschaukel": '@',
	"minus": '-', "minuszeichen": '-', "strich": '-',
	"apostroph":        '\'',
	"anführungsstrich": '"', "anführungsstriche": '"',
	"anführungszeichen": '"', "gänsefüßchen": '"',
	"backslash": '\\', "rückschrägstrich": '\\',
	"einschaltungszeichen": '^', "einschaltzeichen": '^', "häkchen": '^', "dach": '^',
	"grad": '°', "gleichheitszeichen": '=',
	"strichpunkt": ';', "nummernzeichen": '#', "doppelkreuz": '#',
	"leerzeichen": ' ', "space": ' ', "zwischenraum": ' ',
	"leerstelle": ' ',
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

func InitSpellAndStructMap() {
	for runeVar, strVar := range Char2SpellMap {
		Spell2CharMap[strings.ToLower(strVar)] = runeVar
	}
	for name, pu := range Punkt {
		Struct1Word[name] = pu
	}
	for w1, str := range Punkt2Words {
		for w2, name := range str {
			Struct2Words[w1][w2] = name
		}
	}
	initSpellAndStructMap = true
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
		if !initSpellAndStructMap {
			InitSpellAndStructMap()
		}
		spell3Words2rest := map[string]map[string]rune{}
		spell3Words1rest := map[string]rune{}
		spell2Words1rest := map[string]rune{}
		var rune1of3, rune2of3, rune1of2 rune
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
								rune1of3 = first
								continue
							}
							spell2Words1rest, ok = Spell2WordsMap[str]
							if ok {
								// fmt.Println("Spell 2 WordsMap OK:", str)
								rune1of2 = first
								continue
							}
							r, ok = Spell2CharMap[str]
							if ok {
								text += String(r)
							} else {
								// "ch..." oder "Ch..."
								if (str[0] == 'c' || str[0] == 'C') && l > 3 && str[1] == 'h' {
									// fmt.Println(" ########### str:", str)
									if str == "chemnitz" || str == "charlotte" || str == "chiasso" {
										text += String("ch")
									} else {
										text += String("c")
									}
									continue
								}
								if (str[0] == 'S' || str[0] == 's') && l > 4 && str[1] == 'c' && str[2] == 'h' {
									text += String("sch")
									continue
								}
								text += String(first) // main case: the first letter
							}
						} else { // len(spell2Words1rest) > 0
							// fmt.Println("in spell 2 Words 1 rest")
							r, ok = spell2Words1rest[str]
							if ok {
								text += String(r)
							} else {
								text += String(rune1of2)
								text += String(first)
							}
							// text += String(selectOneRune(spell2Words1rest, str))
							spell2Words1rest = map[string]rune{}
						}
					} else { // len(spell3Words2rest) > 0
						// fmt.Println("Klammer = ", str)
						// spell3Words1rest, ok = spell3Words2rest[str]
						spell3Words1rest = spell3Words2rest["klammer"]
						rune2of3 = first
						spell3Words2rest = map[string]map[string]rune{}
					}
				} else { // len(spell3Words1rest) > 0
					// fmt.Println("in spell 3 Words 1 rest")
					r, ok = spell3Words1rest[str]
					if ok {
						text += String(r)
					} else {
						text += String(rune1of3)
						text += String(rune2of3)
						text += String(first)
					}
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
		return list2sstring(spell), true
		// return list2cstring(spell), true
	}
	return spell, false
}

func CheckSpellAndText(spell Term, text Term) bool {
	//if text.Type
	return false
}

func Email2text(spell Term) (Term, int, bool) {
	if spell.Type() == ListType {
		if !initSpellAndStructMap {
			InitSpellAndStructMap()
		}
		// list := spell.(List)
		list, korrAnz := improveEmail(spell.(List))
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
				if l > 2 && str[0] == '"' {
					str = strings.ToLower(str[1 : l-1])
				}

				r, ok = Spell2CharMap[str]
				if ok && idx != last {
					text += String(r)
				} else {
					text += String(str) // main case: the whole word
				}

			} else if ele.Type() == CompoundType {
				comp := ele.(Compound)
				// fmt.Println("  ############# Functor: >", comp.Functor, "< #########")
				if comp.Functor == "zahl" {
					text = text + String(fmt.Sprint(comp.Args[0]))
				} else if len(comp.Args) > 1 && comp.Args[1].Type() == StringType {
					s := comp.Args[1].(String)
					str := string(s)
					l := len(str)
					if l > 2 {
						str = str[1 : l-1]
					}
					text = text + String(str)
				}

			}
		}
		text = "\"" + text + "\""
		return text, korrAnz, true
	}
	return spell, 0, false
}

// Maps 4 Top Level Domain
var TLD1WordMap = map[string]string{
	"com":    "com",
	"kommen": "com",
	"de":     "de",
	"d":      "de",
	"be":     "de",
	"b":      "de",
	"geh":    "de",
	"e":      "de",
	"net":    "net",
	"org":    "org",
	"ort":    "org",
	"auch":   "org",
	"bork":   "org",
	"eu":     "eu"}

var TLD2WordsMap = map[string]map[string]string{
	"d":    {"e": "de"},
	"b":    {"e": "de"},
	"plus": {"t": "posteo"},
	"da":   {"a": "de"},
}

var TLD3WordsMap = map[string]map[string]map[string]string{}

var Punkt = map[string]string{
	"punkt": "punkt",
	"dot":   "punkt",
	"und":   "punkt",
	"um":    "punkt",
	"dort":  "punkt",
	"gott":  "punkt",
	"doch":  "punkt",
	"muss":  "punkt",
	"bot":   "punkt",
}

var Punkt2Words = map[string]map[string]string{
	"und": {"um": "punkt"},
}

var Et = map[string]string{
	"ät":  "@",
	"@":   "@",
	"et":  "@",
	"ed":  "@",
	"and": "@",
	"der": "@",
	"und": "@",
	"f":   "@",
}

var Struct1Word = map[string]string{
	"bindestrich":  "bindestrich",
	"minus":        "minus",
	"unterstrich":  "unterstrich",
	"unterbricht":  "unterstrich",
	"strich":       "strich",
	"unterschrift": "unterstrich",
	"mine":         "minus",
}

var Struct2Words = map[string]map[string]string{
	"bin": {"ich": "bindestrich"},
	"und": {"das": "unterstrich",
		"der": "unterstrich",
		"ich": "unterstrich"},
}

// Maps 4 Hostname
var HN1WordMap = map[string]string{
	"mailbox":    "mailbox",
	"inbox":      "mailbox",
	"posteo":     "posteo",
	"mail":       "mail",
	"gmx":        "gmx",
	"gmail":      "gmail",
	"emil":       "gmail",
	"hotmail":    "hotmail",
	"googlemail": "googlemail",
	"web":        "web",
	"magenta":    "magenta",
	"aol":        "aol",
	"live":       "live",
}

var HN2WordsMap = map[string]map[string]string{
	"t":      {"online": "t-online"},
	"die":    {"online": "t-online"},
	"de":     {"online": "t-online"},
	"mail":   {"box": "mailbox"},
	"post":   {"t": "posteo", "theo": "posteo", "rio": "posteo"},
	"kost":   {"t": "posteo", "theo": "posteo", "rio": "posteo"},
	"plus":   {"t": "posteo"},
	"google": {"mail": "googlemail"},
	"dem":    {"ex": "gmx"},
}

var HN3WordsMap = map[string]map[string]map[string]string{
	"post": {"d": {"o": "posteo"},
		"e": {"o": "posteo"}}}

//@ Hotname
var EtHN1Word = map[string]string{
	"etwa":     "web",
	"erklärt":  "web",
	"entweder": "mail",
}

var EtHN2Words = map[string]map[string]string{
	"etwa": {"günter": "magenta",
		"agenta": "magenta",
		"agenda": "magenta"},
}
var StructEle = map[string]bool{
	"bindestrich": true,
	"minus":       true,
	"unterstrich": true,
	"strich":      true,
	"punkt":       true,
	//	"dot":         true,
}

func improveEmail(l List) (List, int) {
	if len(l) < 3 {
		return l, 5
	}
	korrAnz := 0
	sl := []string{}
	sb := []bool{}
	rl := List{}
	// split strings to sl and rest in rl
	// sb[i] == true if sl[i] == type string
	w1, w2, s1, ok := "", "", map[string]string{}, false
	for _, ele := range l {
		if ele.Type() == StringType {
			s := ele.(String)
			str := string(s)
			l := len(str)
			if l < 3 {
				sl = append(sl, "")
				sb = append(sb, false)
				rl = append(rl, ele)
				continue
			}
			str = strings.ToLower(str[1 : l-1])
			str1 := ""
			for _, char := range str {
				switch char {
				case 'ä':
					str1 += "ae"
				case 'ö':
					str1 += "oe"
				case 'ü':
					str1 += "ue"
				case 'ß':
					str1 += "sz"
				default:
					str1 += string(char)
				}
			}
			str = str1
			if w1 != "" {
				w2, ok = s1[str]
				if ok {
					// fmt.Println("W1: ", w1, "+W2: ", str, " ==> ", w2)
					sl = append(sl, w2)
					sb = append(sb, true)
					rl = append(rl, ele)
					w1 = ""
					continue
				} else {
					sl = append(sl, w1)
					sb = append(sb, true)
					rl = append(rl, ele)
				}
				w1 = ""
			}
			s1, ok = Struct2Words[str]
			if ok {
				// fmt.Println("W1: ", str, "S1: ", s1)
				w1 = str
				continue
			}
			w2, ok = Struct1Word[str]
			if ok {
				sl = append(sl, w2)
				sb = append(sb, true)
				rl = append(rl, ele)
				continue
			}
			sl = append(sl, str)
			sb = append(sb, true)
			rl = append(rl, ele)
			// 				text += String(str) // main case: the whole word
		} else {
			sl = append(sl, "")
			sb = append(sb, false)
			rl = append(rl, ele)
			w1 = ""
		}
	}
	// fmt.Println("sl>>", sl, "<<")
	// fmt.Println("sb>>", sb, "<<")
	// fmt.Println("rl>>", rl, "<<")

	revErg := List{}
	// search Top-Level-Domain
	idx := len(sl) - 1
	if !sb[idx] || idx < 2 {
		return l, 5
	}

	tld1, ok1 := TLD1WordMap[sl[idx]]
	tld2, ok2 := TLD2WordsMap[sl[idx-1]][sl[idx]]
	tld3, ok3 := TLD3WordsMap[sl[idx-2]][sl[idx-1]][sl[idx]]

	if ok3 {
		revErg = List{String(tld3)}
		idx -= 3
	} else if ok2 {
		revErg = List{String(tld2)}
		idx -= 2
	} else {
		if ok1 {
			revErg = List{String(tld1)}
			idx--
		} else {
			revErg = List{String("de")}
			korrAnz++
		}
	}
	//	fmt.Println("TLD revErg >>", revErg, "<< ")
	// search DOT
	if sb[idx] {
		_, ok := Punkt[sl[idx]]
		if ok {
			idx--
		}
	}
	revErg = append(revErg, String("punkt"))
	//	fmt.Println("Punkt revErg >>", revErg, "<< ")
	//search  @ Hostname
	etFound := false
	if sb[idx] {
		hn1, ok1 := HN1WordMap[sl[idx]]
		hn2, ok2 := "", false
		if idx > 0 {
			hn2, ok2 = HN2WordsMap[sl[idx-1]][sl[idx]]
		}
		hn3, ok3 := "", false
		if idx > 1 {
			hn3, ok3 = HN3WordsMap[sl[idx-2]][sl[idx-1]][sl[idx]]
		}
		//		fmt.Println("HN2Word aus: ", sl[idx-1], sl[idx], " wird: ", hn2)
		if ok1 || ok2 || ok3 {
			if ok3 {
				revErg = append(revErg, String(hn3))
				idx -= 3
			} else if ok2 {
				revErg = append(revErg, String(hn2))
				idx -= 2
			} else if ok1 {
				revErg = append(revErg, String(hn1))
				idx--
			}
			_, ok := Et[sl[idx]]
			if ok {
				idx--
			} else {
				korrAnz++
			}
			revErg = append(revErg, String("@"))
			etFound = true
		} else {
			hn1, ok1 = EtHN1Word[sl[idx]]
			ok2 = false
			if idx > 0 {
				hn2, ok2 = EtHN2Words[sl[idx-1]][sl[idx]]
			}
			if ok1 || ok2 {
				if ok2 {
					idx -= 2
					revErg = append(revErg, String(hn2))
				} else {
					idx--
					revErg = append(revErg, String(hn1))
				}
				revErg = append(revErg, String("@"))
				etFound = true
				korrAnz++
			}

		}
	}

	if !etFound {
		// search @
		// s1 s2 s3 [idx2] @ [idx1] sn sn+1 [idx]

		idx1 := idx
		idx2 := idx
		for i := idx; i > 0; i-- {
			_, ok = Et[sl[i]]
			if ok {
				idx1 = i + 1
				idx2 = i - 1
				etFound = true
				break
			}

		}
		//		fmt.Println(" echtes @ etFound1:", etFound)
		if !etFound {
			// add a @ beween two words
			firstWord := false
			for i := idx; i > 0; i-- {
				if sb[i] {
					_, ok = StructEle[sl[i]]
					//					fmt.Println(" i:", i, " ele:", sl[i], " structeEle:", ok)
					if ok {
						firstWord = false
					} else {
						if firstWord {
							// two word found
							idx2 = i
							idx1 = i + 1
							korrAnz += 3
							etFound = true
							break
						} else {
							firstWord = true
						}
					}
				} else {
					firstWord = true
				}
			}
		}
		//		fmt.Println(" etFound1:", etFound, "idx:", idx, " idx1:", idx1, " idx2:", idx2)
		if etFound {
			for i := idx; i >= idx1; i-- {
				if sb[i] {
					revErg = append(revErg, String(sl[i]))
				} else {
					revErg = append(revErg, rl[i])
				}
			}
			// revErg = append(revErg, String("@"))
			idx = idx2
		} else {
			return l, 5
		}
		revErg = append(revErg, String("@"))
		idx = idx2
	}

	//	fmt.Println("@ Host revErg >>", revErg, "<< ")
	// @ found
	for ; idx >= 0; idx-- {
		if sb[idx] {
			revErg = append(revErg, String(sl[idx]))
		} else {
			revErg = append(revErg, rl[idx])
		}
	}
	//	fmt.Println("loc.Name revErg >>", revErg, "<< ")
	// reverse revErg
	l = List{}
	for j := len(revErg) - 1; 0 <= j; j-- {

		l = append(l, revErg[j])
	}
	// fmt.Println(" improveEmail >>", l, "<< korr:", korrAnz)

	return l, korrAnz
}

func ZiffernZuZahl(spell Term) (Term, bool) {
	if spell.Type() == ListType {
		if !initSpellAndStructMap {
			InitSpellAndStructMap()
		}

		list := spell.(List)
		var zahl String

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
					if ru >= '0' && ru <= '9' {
						zahl += String(ru)
					}
				}
			} else if ele.Type() == CompoundType {
				comp := ele.(Compound)
				// fmt.Println("  ############# Functor: >", comp.Functor, "< #########")
				if comp.Functor == "zahl" {
					zahl = zahl + String(fmt.Sprint(comp.Args[0]))
				}
			}
		}
		zahl = "\"" + zahl + "\""
		return zahl, true
	}
	return spell, false
}

func Text2email(text Term) (Term, Term, bool) {
	var spell List
	if text.Type() == StringType {
		str := text.(String)
		// fmt.Println("Str:", str)
		l := len(str)
		if l < 3 {
			return spell, spell, false
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
		return list2sstring(spell), list2stringList(spell), true
		// return list2cstring(spell), true
	}
	return spell, spell, false
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

func list2sstring(list Term) Term {
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
					text = text + "; " + str
				}
			}
		}
		text = "\"" + text + "\""
		return String(text)
	}
	return list
}

func list2stringList(list Term) Term {
	var spell List
	if list.Type() == ListType {
		l := list.(List)
		for _, ele := range l {
			if ele.Type() == StringType {
				s := ele.(String)
				str := string(s)
				spell = append(spell, String("\""+str+"\""))
			}
		}
		return spell
	}
	return list
}

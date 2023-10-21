package impl

import (
	"math/big"

	pr "github.com/qredo/blackbird/verifier/golang/protobuf"
)

const (
	Opcode_NOP       = byte(0x0)
	Opcode_YANK_1    = byte(0x01)
	Opcode_YANK_2    = byte(0x02)
	Opcode_YANK_3    = byte(0x03)
	Opcode_PEEK_0    = byte(0x10)
	Opcode_PEEK_1    = byte(0x11)
	Opcode_PEEK_2    = byte(0x12)
	Opcode_PEEK_3    = byte(0x13)
	Opcode_LOADTXN   = byte(0x40)
	Opcode_FOCUS     = byte(0x41)
	Opcode_OUTFLOW   = byte(0x42)
	Opcode_ADDASSETS = byte(0x50)
	Opcode_SUBASSETS = byte(0x51)
	Opcode_GTZ       = byte(0x60)
	Opcode_GEZ       = byte(0x61)
)

var opcodeNames = map[uint8](string){
	Opcode_NOP:       "NOP",
	Opcode_YANK_1:    "YANK_1",
	Opcode_YANK_2:    "YANK_2",
	Opcode_YANK_3:    "YANK_3",
	Opcode_PEEK_0:    "PEEK_0",
	Opcode_PEEK_1:    "PEEK_1",
	Opcode_PEEK_2:    "PEEK_2",
	Opcode_PEEK_3:    "PEEK_3",
	Opcode_LOADTXN:   "LOADTXN",
	Opcode_FOCUS:     "FOCUS",
	Opcode_OUTFLOW:   "OUTFLOW",
	Opcode_ADDASSETS: "ADDASSETS",
	Opcode_SUBASSETS: "SUBASSETS",
	Opcode_GTZ:       "GTZ",
	Opcode_GEZ:       "GEZ",
}

func validOpcode(opcode uint8) bool {
	_, present := opcodeNames[opcode]
	return present
}

type evaluation struct {
	unknown  func(opcode byte) error
	loadtxn  func() (*pr.Policy, error)
	gez, gtz func(p *pr.Policy) (*pr.Policy, error)
	add, sub func(x *pr.Policy, y *pr.Policy) (*pr.Policy, error)
}

func opcodeArithmetic(tables *table_stack, m *merge[*big.Int, *big.Int, *big.Int]) func(x *pr.Policy, y *pr.Policy) (*pr.Policy, error) {
	return func(x *pr.Policy, y *pr.Policy) (*pr.Policy, error) {
		ax := &AssetInfo{
			defs:        x.AssetDefs,
			adjustments: x.Assets,
		}
		ay := &AssetInfo{
			defs:        y.AssetDefs,
			adjustments: y.Assets,
		}

		a := mergeAssets(tables, ax, ay, m)
		p := &pr.Policy{
			Tag:       pr.PolicyTag_POLICY_ASSETS,
			Assets:    a.adjustments,
			AssetDefs: a.defs,
		}
		return p, nil
	}
}

func evalThunk(closure *pr.Policy, eval *evaluation) (*pr.Policy, error) {
	stack := copySlice(closure.Subpolicies)
	l := uint(len(stack))

	push := func(top *pr.Policy) {
		stack = stack[:l]
		stack = append(stack, top)
		l++
	}

	pop := func() (*pr.Policy, error) {
		if l == 0 {
			return nil, ErrStackOverrun{0}
		}
		top := stack[l-1]
		l--
		return top, nil
	}

	stack1_1 := func(f func(*pr.Policy) (*pr.Policy, error)) error {
		x, err := pop()
		if err != nil {
			return err
		}
		if y, err := f(x); err != nil {
			return err
		} else {
			push(y)
			return nil
		}
	}

	stack2_1 := func(f func(*pr.Policy, *pr.Policy) (*pr.Policy, error)) error {
		x, err := pop()
		if err != nil {
			return err
		}
		return stack1_1(func(y *pr.Policy) (*pr.Policy, error) { return f(y, x) })
	}

	peek := func(n uint) error {
		if n >= l {
			return ErrStackOverrun{n}
		}
		push(stack[l-1-n])
		return nil
	}

	yank := func(n uint) error {
		if n >= l {
			return ErrStackOverrun{n}
		}
		p := stack[l-1-n]
		for i := l - n; i < l; i++ {
			stack[i-1] = stack[i]
		}
		stack[l-1] = p
		return nil
	}

	focus := func(p *pr.Policy) (*pr.Policy, error) {
		return Focus(p.Transaction)
	}

	outflow := func(p *pr.Policy) (*pr.Policy, error) {
		return Outflow(p.Transaction), nil
	}

	exec := func(opcode byte) error {
		switch opcode {
		case Opcode_NOP:
			return nil
		case Opcode_LOADTXN:
			if p, err := eval.loadtxn(); err != nil {
				return err
			} else {
				push(p)
				return nil
			}

		case Opcode_FOCUS:
			return stack1_1(focus)
		case Opcode_OUTFLOW:
			return stack1_1(outflow)
		case Opcode_YANK_1:
			return yank(1)
		case Opcode_YANK_2:
			return yank(2)
		case Opcode_YANK_3:
			return yank(3)
		case Opcode_PEEK_0:
			return peek(0)
		case Opcode_PEEK_1:
			return peek(1)
		case Opcode_PEEK_2:
			return peek(2)
		case Opcode_PEEK_3:
			return peek(3)
		case Opcode_GEZ:
			return stack1_1(eval.gez)
		case Opcode_GTZ:
			return stack1_1(eval.gtz)
		case Opcode_ADDASSETS:
			return stack2_1(eval.add)
		case Opcode_SUBASSETS:
			return stack2_1(eval.sub)
		default:
			return eval.unknown(opcode)
		}
	}

	for _, opcode := range closure.Thunk {
		if err := exec(opcode); err != nil {
			return nil, err
		}
	}

	return pop()
}

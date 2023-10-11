package impl

import (
	"errors"
	"fmt"

	pr "gitlab.qredo.com/edmund/blackbird/verifier/golang/protobuf"
)

type stack_frame interface {
	pop() stack_frame
}

type choice_point struct {
	prev        stack_frame
	prev_choice *choice_point
	policy      *pr.Policy
	witness     *pr.Witness
	offset      uint64
	needed      uint64
	unknowns    uint64
}

func (this choice_point) String() string {
	return fmt.Sprintf("choice_point{o=%d n=%d u=%d p=%v w=%v c=%p} %v", this.offset, this.needed, this.unknowns, this.policy, this.witness, this.prev_choice, this.prev)
}

type all_frame struct {
	prev    stack_frame
	policy  *pr.Policy
	witness *pr.Witness
	offset  uint64
}

func (this all_frame) String() string {
	return fmt.Sprintf("all_frame{o=%d p=%v w=%v} %v", this.offset, this.policy, this.witness, this.prev)
}

type any_frame struct {
	prev    stack_frame
	policy  *pr.Policy
	witness *pr.Witness
	keys    []uint64
	offset  uint64
}

func (this any_frame) String() string {
	return fmt.Sprintf("any_frame{o=%d k=%v p=%v w=%v} %v", this.offset, this.keys, this.policy, this.witness, this.prev)
}

func (this *choice_point) pop() stack_frame {
	return this.prev
}

func (this *all_frame) pop() stack_frame {
	return this.prev
}

func (this *any_frame) pop() stack_frame {
	return this.prev
}

func (this *table_frame) pop() stack_frame {
	return this.prev
}

type table_frame struct {
	prev    stack_frame
	policy  *pr.Policy
	witness *pr.Witness

	done bool
}

func checkTransaction(t *pr.Transaction) error {
	l := len(t.Assets)
	for i_f, f := range t.Flows {
		for _, k := range sortedKeys(f.Adjustments) {
			if k >= uint64(l) {
				return ErrUnresolvedTransactionAssets{
					flow:   uint64(i_f),
					offset: uint64(k),
					len:    uint(l),
				}
			}
		}
	}
	return nil
}

func cherryPickPolicyFields(old *pr.Policy) (*pr.Policy, error) {
	p := &pr.Policy{
		Tag: old.Tag,
	}

	unknown := func() (*pr.Policy, error) { return nil, ErrUnknownPolicy(p.Tag) }

	threshold := func() { p.Threshold = old.Threshold }
	subpolicies := func() { p.Subpolicies = old.Subpolicies }
	addressPrefix := func() { p.AddressPrefix = old.AddressPrefix }
	address := func() { p.Address = old.Address }
	assets := func() { p.Assets = old.Assets }
	assetDefs := func() { p.AssetDefs = old.AssetDefs }
	thunk := func() { p.Thunk = old.Thunk }
	transaction := func() { p.Transaction = old.Transaction }

	switch old.Tag {
	case pr.PolicyTag_POLICY_UNSPECIFIED:
	case pr.PolicyTag_POLICY_ALL:
		subpolicies()
	case pr.PolicyTag_POLICY_ANY:
		threshold()
		subpolicies()
	case pr.PolicyTag_POLICY_SIGNATURE:
		addressPrefix()
		address()
	case pr.PolicyTag_POLICY_REF_LOCAL:
		return unknown()
	case pr.PolicyTag_POLICY_ASSETS:
		assets()
		assetDefs()
	case pr.PolicyTag_POLICY_LIST:
		return unknown()
	case pr.PolicyTag_POLICY_CLOSURE:
		subpolicies()
		thunk()
	case pr.PolicyTag_POLICY_TABLE:
		subpolicies()
		assetDefs()
	case pr.PolicyTag_POLICY_TRANSACTION:
		transaction()
	case pr.PolicyTag_POLICY_ASSET_FLOW:
		assets()
		assetDefs()
	case pr.PolicyTag_POLICY_REF_TEMP:
		return unknown()
	}

	return p, nil
}

func wellFormedPolicyShallow(p *pr.Policy) error {
	c, err := cherryPickPolicyFields(p)
	if err != nil {
		return err
	}

	if c.Threshold != p.Threshold {
		return ErrMalformedPolicy{}
	}
	if len(c.Subpolicies) != len(p.Subpolicies) {
		return ErrMalformedPolicy{}
	}
	if c.AddressPrefix != p.AddressPrefix {
		return ErrMalformedPolicy{}
	}
	if c.Address != p.Address {
		return ErrMalformedPolicy{}
	}
	if len(c.Assets) != len(p.Assets) {
		return ErrMalformedPolicy{}
	}
	if len(c.AssetDefs) != len(p.AssetDefs) {
		return ErrMalformedPolicy{}
	}
	if len(c.Thunk) != len(p.Thunk) {
		return ErrMalformedPolicy{}
	}
	if c.Transaction != p.Transaction {
		return ErrMalformedPolicy{}
	}

	return nil
}

func cherryPickWitnessFields(old *pr.Witness) (*pr.Witness, error) {
	w := &pr.Witness{
		Tag: old.Tag,
	}
	unknown := func() (*pr.Witness, error) { return nil, ErrUnknownWitness(w.Tag) }

	all := func() { w.AllWitnesses = old.AllWitnesses }
	any := func() { w.AnyWitnesses = old.AnyWitnesses }
	next := func() { w.NextWitness = old.NextWitness }

	switch w.Tag {
	case pr.WitnessTag_WITNESS_UNSPECIFIED:
		return unknown()
	case pr.WitnessTag_WITNESS_GUESS:
	case pr.WitnessTag_WITNESS_ALL:
		all()
	case pr.WitnessTag_WITNESS_ANY:
		any()
	case pr.WitnessTag_WITNESS_PRECHECKED_SIGNATURE:
	case pr.WitnessTag_WITNESS_TABLE:
		any()
	case pr.WitnessTag_WITNESS_EVAL:
		next()
	case pr.WitnessTag_WITNESS_REF:
		return unknown()
	}
	return w, nil
}

func wellFormedWitnessShallow(w *pr.Witness) error {
	c, err := cherryPickWitnessFields(w)
	if err != nil {
		return err
	}

	if len(c.AllWitnesses) != len(w.AllWitnesses) {
		return ErrUnexpectedAllWitnesses(w.Tag)
	}
	if len(c.AnyWitnesses) != len(w.AnyWitnesses) {
		return ErrUnexpectedAnyWitnesses(w.Tag)
	}

	if c.NextWitness != w.NextWitness {
		return ErrUnexpectedNextWitness(w.Tag)
	}

	return nil
}

func VerifyEngine(fuel int, policy pr.Policy, witness pr.Witness, transaction pr.Transaction, sigs map[string]bool) error {
	var stack stack_frame = nil
	var choice_stack *choice_point = nil
	var r error = nil
	var couldNotGuess *ErrFailedToGuessWitness

	tables := newTableStack()

	p := &policy
	w := &witness
	popping := false
	pushing := false

	unexpectedTag := func() ErrUnexpectedTag {
		return ErrUnexpectedTag{
			policyTag:  p.Tag,
			witnessTag: w.Tag,
		}
	}

	if err := checkTransaction(&transaction); err != nil {
		return err
	}

	for {
		// fmt.Printf("pu=%v po=%v p=%v w=%v r=%v st=%v\n\n",pushing,popping,p,w,r,stack)
		switch {
		case popping:
			if r != nil && choice_stack != nil {
				stack = choice_stack
			} else if r != nil || stack == nil {
				return r
			}
			switch top := stack.(type) {
			case *choice_point:
				top.offset += 1
				if r == nil {
					top.needed -= 1
				} else if errors.As(r, &couldNotGuess) {
					top.unknowns += 1
				}
			case *any_frame:
				top.offset += 1
			case *all_frame:
				top.offset += 1
			case *table_frame:
				top.done = true
			}
			popping = false
			pushing = true
		case pushing:
			pushing = false
			switch top := stack.(type) {
			case *choice_point:
				choice_stack = top
				stack = top
				if top.needed <= 0 {
					r = nil
					stack = top.prev
					choice_stack = top.prev_choice
					popping = true
				} else if top.offset >= uint64(len(top.policy.Subpolicies)) || top.offset+top.needed > uint64(len(top.policy.Subpolicies))+top.unknowns {
					if top.unknowns >= top.needed {
						r = ErrFailedToGuessWitness{}
					} else {
						r = ErrInvalidWitness{}
					}
					stack = top.prev
					choice_stack = top.prev_choice
					popping = true
				} else if fuel <= 0 {
					return ErrFailedToGuessWitness{}
				} else {
					fuel -= 1
					p = top.policy.Subpolicies[top.offset]
					w = top.witness
				}
			case *any_frame:
				stack = top
				if top.offset >= uint64(len(top.keys)) {
					r = nil
					stack = top.prev
					popping = true
				} else if top.keys[top.offset] < uint64(len(top.policy.Subpolicies)) {
					p = top.policy.Subpolicies[top.keys[top.offset]]
					w = top.witness.AnyWitnesses[top.keys[top.offset]]
				} else {
					r = ErrInvalidWitness{}
					stack = top.prev
					popping = true
				}
			case *all_frame:
				stack = top
				if top.offset >= uint64(len(top.policy.Subpolicies)) {
					r = nil
					stack = top.prev
					popping = true
				} else {
					p = top.policy.Subpolicies[top.offset]
					w = top.witness.AllWitnesses[top.offset]
				}
			case *table_frame:
				stack = top
				if top.done {
					popTableEntry(tables)

					r = nil
					stack = top.prev
					popping = true
				} else {
					tables = addTableEntry(tables, p.AssetDefs)
					p = p.Subpolicies[0]
					w = w.AnyWitnesses[0]
				}
			}
		default:
			if err := wellFormedWitnessShallow(w); err != nil {
				return err
			}

			switch p.Tag {
			case pr.PolicyTag_POLICY_ALL:
				if w.Tag == pr.WitnessTag_WITNESS_GUESS {
					w = &pr.Witness{Tag: pr.WitnessTag_WITNESS_ALL}
					w.AllWitnesses = make([]*pr.Witness, 0, len(p.Subpolicies))
					for range p.Subpolicies {
						w.AllWitnesses = append(w.AllWitnesses, &pr.Witness{Tag: pr.WitnessTag_WITNESS_GUESS})
					}
				}
				if w.Tag == pr.WitnessTag_WITNESS_ALL && len(w.AllWitnesses) == len(p.Subpolicies) {
					stack = &all_frame{
						prev:    stack,
						policy:  p,
						witness: w,
						offset:  0,
					}
					pushing = true
				} else {
					r = ErrInvalidWitness{}
					popping = true
				}

			case pr.PolicyTag_POLICY_ANY:
				if w.Tag == pr.WitnessTag_WITNESS_GUESS {
					choice_stack = &choice_point{
						prev:        stack,
						prev_choice: choice_stack,
						policy:      p,
						witness:     w,
						needed:      p.Threshold,
						unknowns:    0,
						offset:      0,
					}
					stack = choice_stack
					pushing = true
				} else if w.Tag == pr.WitnessTag_WITNESS_ANY && uint64(len(w.AnyWitnesses)) == p.Threshold {
					ks := sortedKeys(w.AnyWitnesses)
					stack = &any_frame{
						prev:    stack,
						policy:  p,
						witness: w,
						keys:    ks,
						offset:  0,
					}
					pushing = true
				} else {
					r = ErrInvalidWitness{}
					popping = true
				}

			case pr.PolicyTag_POLICY_SIGNATURE:
				if w.Tag == pr.WitnessTag_WITNESS_GUESS || w.Tag == pr.WitnessTag_WITNESS_PRECHECKED_SIGNATURE {
					cooked, ok := p.Address.(*pr.Policy_CookedAddress)
					if !ok {
						r = ErrInvalidWitness{}
						popping = true
					} else if set, present := sigs[cooked.CookedAddress]; present && set {
						r = nil
						popping = true
					} else {
						r = ErrInvalidWitness{}
						popping = true
					}
				} else {
					r = ErrInvalidWitness{}
					popping = true
				}

			case pr.PolicyTag_POLICY_TABLE:
				if w.Tag == pr.WitnessTag_WITNESS_GUESS {
					w = &pr.Witness{Tag: pr.WitnessTag_WITNESS_TABLE}
					w.AnyWitnesses = map[uint64]*pr.Witness{}
					for i, _ := range p.Subpolicies {
						w.AnyWitnesses[uint64(i)] = Guess()
					}
				}
				if w.Tag != pr.WitnessTag_WITNESS_TABLE {
					r = unexpectedTag()
					popping = true
				} else if len(w.AnyWitnesses) != assumesSingletonTable(1) {
					r = ErrInvalidWitness{}
					popping = true
				} else {
					stack = &table_frame{
						prev:    stack,
						policy:  p,
						witness: w,
						done:    false,
					}
					pushing = true
				}

			case pr.PolicyTag_POLICY_CLOSURE:
				if w.Tag == pr.WitnessTag_WITNESS_GUESS {
					w.Tag = pr.WitnessTag_WITNESS_EVAL
					w.NextWitness = Guess()
				}
				if w.Tag != pr.WitnessTag_WITNESS_EVAL {
					r = unexpectedTag()
					popping = true
				} else {

					comparison := func(strict bool) func(p *pr.Policy) (*pr.Policy, error) {
						return func(p *pr.Policy) (*pr.Policy, error) {
							return constantPolicy(assetsGTZ(p.Assets, strict)), nil
						}
					}

					e := &evaluation{
						gez: comparison(false),
						gtz: comparison(true),
						add: opcodeArithmetic(tables, mergeAdd),
						sub: opcodeArithmetic(tables, mergeSub),

						loadtxn: func() (*pr.Policy, error) {
							return TransactionP(&transaction), nil
						},

						unknown: func(opcode byte) error {
							return ErrInvalidBytecode{opcode}
						},
					}

					r = nil
					if p1, err := evalThunk(p, e); err != nil {
						return err
					} else {
						p = p1
						w = w.NextWitness
					}
				}
			default:
				r = unverifiablePolicy(p)
				popping = true
			}
		}
	}
}

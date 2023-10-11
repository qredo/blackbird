package impl

import (
	pr "gitlab.qredo.com/edmund/blackbird/verifier/golang/protobuf"
)

func Guess() *pr.Witness {
	return &pr.Witness{Tag: pr.WitnessTag_WITNESS_GUESS}
}

func AllW(subs []*pr.Witness) *pr.Witness {
	return &pr.Witness{
		Tag:          pr.WitnessTag_WITNESS_ALL,
		AllWitnesses: subs,
	}
}

func AnyW(subs map[uint64]*pr.Witness) *pr.Witness {
	return &pr.Witness{
		Tag:          pr.WitnessTag_WITNESS_ANY,
		AnyWitnesses: subs,
	}
}

func Prechecked() *pr.Witness {
	return &pr.Witness{Tag: pr.WitnessTag_WITNESS_PRECHECKED_SIGNATURE}
}

func All(subs []*pr.Policy) *pr.Policy {
	return &pr.Policy{
		Tag:         pr.PolicyTag_POLICY_ALL,
		Subpolicies: subs,
	}
}

func Any(threshold uint64, subs []*pr.Policy) *pr.Policy {
	return &pr.Policy{
		Tag:         pr.PolicyTag_POLICY_ANY,
		Threshold:   threshold,
		Subpolicies: subs,
	}
}

func SigP(participant string) *pr.Policy {
	return &pr.Policy{
		Tag:     pr.PolicyTag_POLICY_SIGNATURE,
		Address: &pr.Policy_CookedAddress{participant},
	}
}

func SigK(participant KeyAuthority) *pr.Policy {
	if participant.Raw != nil && len(participant.Raw) > 0 {
		return &pr.Policy{
			Tag:           pr.PolicyTag_POLICY_SIGNATURE,
			AddressPrefix: participant.Prefix,
			Address:       &pr.Policy_RawAddress{participant.Raw},
		}
	} else {
		return &pr.Policy{
			Tag:           pr.PolicyTag_POLICY_SIGNATURE,
			AddressPrefix: participant.Prefix,
			Address:       &pr.Policy_CookedAddress{participant.Cooked},
		}
	}
}

func ClosureP(stack []*pr.Policy, thunk []byte) *pr.Policy {
	return &pr.Policy{
		Tag:         pr.PolicyTag_POLICY_CLOSURE,
		Subpolicies: stack,
		Thunk:       thunk,
	}
}

func EvalW(next *pr.Witness) *pr.Witness {
	return &pr.Witness{
		Tag:         pr.WitnessTag_WITNESS_EVAL,
		NextWitness: next,
	}
}

func TableP(policies []*pr.Policy, assets []*pr.Asset) *pr.Policy {
	return &pr.Policy{
		Tag:         pr.PolicyTag_POLICY_TABLE,
		Subpolicies: policies,
		AssetDefs:   assets,
	}
}

func TableW(sub *pr.Witness) *pr.Witness {
	return &pr.Witness{
		Tag:          pr.WitnessTag_WITNESS_TABLE,
		AnyWitnesses: map[uint64]*pr.Witness{0: sub},
	}
}

func TransactionP(tx *pr.Transaction) *pr.Policy {
	return &pr.Policy{
		Tag:         pr.PolicyTag_POLICY_TRANSACTION,
		Transaction: tx,
	}
}

const hardcodedFocus = uint64(0)

func Focus(tx *pr.Transaction) (*pr.Policy, error) {
	l := uint64(len(tx.Flows))
	if tx.Focus >= l {
		return nil, ErrUnresolvedFocus{
			focus: hardcodedFocus,
			flows: l,
		}
	}
	p := &pr.Policy{
		Tag: pr.PolicyTag_POLICY_ASSET_FLOW,
		Transaction: &pr.Transaction{
			Tag:    tx.Tag,
			Assets: tx.Assets,
			Flows:  []*pr.AssetFlow{tx.Flows[tx.Focus]},
			Focus:  0,
		},
	}
	return p, nil
}

func Outflow(tx *pr.Transaction) *pr.Policy {
	return &pr.Policy{
		Tag:       pr.PolicyTag_POLICY_ASSETS,
		Assets:    tx.Flows[0].Adjustments,
		AssetDefs: tx.Assets,
	}
}

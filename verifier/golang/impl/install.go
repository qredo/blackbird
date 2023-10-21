package impl

import (
	"bytes"
	"math/big"
	"net/url"
	"regexp"
	"unicode/utf8"

	pr "github.com/qredo/blackbird/verifier/golang/protobuf"
)

type address2participant struct {
	raw2participant    map[string]string
	cooked2participant map[string]string
}

func invertParticipants(participants map[string]Authority) map[string]address2participant {
	r := map[string]address2participant{}
	for participant, authority := range participants {

		insertIfBetterFit := insertIf(
			func(k string, _ string) bool { return len(k) > 0 },
			func(_ string, v string, old string) bool { return v < old },
		)

		switch v := authority.(type) {
		case KeyAuthority:
			elem, ok := r[v.Prefix]
			if !ok {
				elem = address2participant{map[string]string{}, map[string]string{}}
				r[v.Prefix] = elem
			}
			insertIfBetterFit(elem.raw2participant, string(v.Raw), participant)
			insertIfBetterFit(elem.cooked2participant, v.Cooked, participant)
		default:
		}
	}
	return r
}

func InstallCheck(policy pr.Policy, participants map[string]Authority) (pr.Policy, error) {
	var inverted map[string]address2participant
	return checkInstaller(policy, newTableStack(), participants, &inverted)
}

var participant_regexp *regexp.Regexp

func init() {
	participant_regexp, _ = regexp.Compile(`\A[A-Za-z0-9.]+\z`)
}

func validateAssetDefs(xs []*pr.Asset, tag pr.PolicyTag) error {
	m := map[AssetComparable]uint{}
	for i, a := range xs {
		ui := uint(i)
		ad := asset2AssetComparable(a)
		if low, present := m[ad]; present {
			return ErrDuplicatedAssets{
				tag:  tag,
				low:  low,
				high: ui,
			}
		} else {
			m[ad] = ui
		}
	}

	for _, a := range xs {
		if !validProtocol(a.Tag) {
			return ErrInvalidProtocol{a.Tag}
		}
		if !utf8.ValidString(a.AddressPrefix) {
			return ErrInvalidUTF8{a.AddressPrefix}
		}
	}
	return nil
}

func canonicalizeThunk(bytecode []byte) ([]byte, error) {
	res := []byte{}
	for _, op := range bytecode {
		if !validOpcode(op) {
			return res, ErrInvalidBytecode{op}
		}
		if op == Opcode_NOP {
			continue
		}
		res = append(res, op)
	}
	return res, nil
}

func checkClosureAssets(s *table_stack, policy *pr.Policy) (*pr.Policy, error) {
	if err := validateAssetDefs(policy.AssetDefs, policy.Tag); err != nil {
		return &pr.Policy{}, err
	}

	zero := bignum2bytes(big.NewInt(0))
	for k, v := range policy.Assets {
		if bytes.Equal(v, zero) {
			delete(policy.Assets, k)
		}
	}

	{
		l := uint64(len(policy.AssetDefs))
		xs := []*pr.Asset{}
		ys := map[uint64][]byte{}

		for _, k := range sortedKeys(policy.Assets) {
			if k < l {
				ys[uint64(len(xs))] = policy.Assets[k]
				xs = append(xs, policy.AssetDefs[k])
			} else {
				ys[k-l+uint64(len(xs))] = policy.Assets[k]
			}
		}
		policy.Assets = ys
		policy.AssetDefs = xs
	}

	count := tableStackSize(s) + uint64(len(policy.AssetDefs))

	for k := range policy.Assets {
		if k >= count {
			return &pr.Policy{}, ErrUnresolvedPolicyAssets{}
		}
	}

	p := pr.Policy{}
	p.Reset()
	p.Tag = pr.PolicyTag_POLICY_ASSETS
	p.Assets = policy.Assets
	p.AssetDefs = policy.AssetDefs
	return &p, nil
}

func checkInstaller(policy pr.Policy, tables *table_stack, participants map[string]Authority, inverted *map[string]address2participant) (pr.Policy, error) {
	checkSubpolicies := func(s *table_stack, inv *map[string]address2participant) ([]*pr.Policy, error) {
		vs := make([]*pr.Policy, 0, len(policy.Subpolicies))
		for _, v := range policy.Subpolicies {
			if r, err := checkInstaller(*v, s, participants, inv); err != nil {
				return []*pr.Policy{}, err
			} else {
				vs = append(vs, &r)
			}
		}
		return vs, nil
	}

	if err := wellFormedPolicyShallow(&policy); err != nil {
		return pr.Policy{}, err
	}

	switch policy.Tag {
	case pr.PolicyTag_POLICY_ALL:
		if vs, err := checkSubpolicies(tables, inverted); err != nil {
			return pr.Policy{}, err
		} else {
			p := pr.Policy{}
			p.Reset()
			p.Tag = pr.PolicyTag_POLICY_ALL
			p.Subpolicies = vs
			return p, nil
		}
	case pr.PolicyTag_POLICY_ANY:
		if vs, err := checkSubpolicies(tables, inverted); err != nil {
			return pr.Policy{}, err
		} else {
			p := pr.Policy{}
			p.Reset()
			p.Tag = pr.PolicyTag_POLICY_ANY
			p.Threshold = policy.Threshold
			p.Subpolicies = vs
			return p, nil
		}
	case pr.PolicyTag_POLICY_SIGNATURE:
		var participant string
		not_found := ErrNonParticipantReference{KeyAuthority{policy.AddressPrefix, policy.GetCookedAddress(), policy.GetRawAddress()}.String()}
		if len(policy.AddressPrefix) > 0 {
			if _, err := url.Parse(policy.AddressPrefix); err != nil {
				return pr.Policy{}, ErrInvalidAddressPrefix{policy.AddressPrefix, err.Error()}
			}
			if *inverted == nil {
				*inverted = invertParticipants(participants)
			}
			middle, ok := (*inverted)[policy.AddressPrefix]
			if !ok {
				return pr.Policy{}, not_found
			}
			var ok2 bool
			if len(policy.GetCookedAddress()) > 0 {
				participant, ok2 = middle.cooked2participant[policy.GetCookedAddress()]
			} else {
				participant, ok2 = middle.raw2participant[string(policy.GetRawAddress())]
			}
			if !ok2 {
				return pr.Policy{}, not_found
			}
		} else {
			participant = policy.GetCookedAddress()
		}
		if !participant_regexp.MatchString(participant) {
			return pr.Policy{}, ErrInvalidParticipantReference{participant}
		}
		if _, ok := participants[participant]; !ok {
			return pr.Policy{}, not_found
		}
		p := pr.Policy{}
		p.Reset()
		p.Tag = pr.PolicyTag_POLICY_SIGNATURE
		p.Address = &pr.Policy_CookedAddress{participant}
		return p, nil

	case pr.PolicyTag_POLICY_TABLE:
		l := len(policy.Subpolicies)
		if l == 0 {
			return pr.Policy{}, ErrMalformedPolicy{}
		}
		if l != assumesSingletonTable(1) {
			return pr.Policy{}, ErrUnimplementedRef{}
		}

		s := addTableEntry(tables, policy.AssetDefs)
		if vs, err := checkSubpolicies(s, inverted); err != nil {
			return pr.Policy{}, err
		} else if err := validateAssetDefs(policy.AssetDefs, policy.Tag); err != nil {
			return pr.Policy{}, err
		} else {
			popTableEntry(s)
			p := pr.Policy{}
			p.Reset()
			p.Tag = pr.PolicyTag_POLICY_TABLE
			p.AssetDefs = policy.AssetDefs
			p.Subpolicies = vs
			return p, nil
		}

	case pr.PolicyTag_POLICY_CLOSURE:
		ps := []*pr.Policy{}
		for _, sp := range policy.Subpolicies {
			if sp.Tag != pr.PolicyTag_POLICY_ASSETS {
				return pr.Policy{}, ErrUnsupportedSubpolicy{
					policy: policy.Tag,
					sub:    sp.Tag,
				}
			}
			if p, err := checkClosureAssets(tables, sp); err != nil {
				return pr.Policy{}, err
			} else {
				ps = append(ps, p)
			}
		}

		t, err := canonicalizeThunk(policy.Thunk)
		if err != nil {
			return pr.Policy{}, err
		}

		p := pr.Policy{}
		p.Reset()
		p.Tag = pr.PolicyTag_POLICY_CLOSURE
		p.Thunk = t
		p.Subpolicies = ps
		return p, nil

	default:
		return pr.Policy{}, unverifiablePolicy(&policy)
	}
}

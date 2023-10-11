package impl

import (
	"bytes"
	"cmp"
	"math/big"
	"math/rand"
	"net/url"

	"github.com/leanovate/gopter"
	"github.com/leanovate/gopter/gen"
	pr "gitlab.qredo.com/edmund/blackbird/verifier/golang/protobuf"
	"gitlab.qredo.com/edmund/blackbird/verifier/golang/tree"
)

func guessable(r *rand.Rand, w *pr.Witness) *pr.Witness {
	return oneof(r, w, Guess())
}

func InvalidString(r *rand.Rand) string {
	start := byte(0x80)
	end := byte(0xC2)
	return string([]byte{start + byte(r.Intn(int(end-start)))})
}

type AssetInfo struct {
	defs        []*pr.Asset
	adjustments map[uint64][]byte
}

type Witnessed struct {
	Policy      *pr.Policy
	Transaction *pr.Transaction
	Witness     *pr.Witness
}

type BrokenlyWitnessed Verification

type Broken struct {
	Policy *pr.Policy
	Error  any
}

type Unit struct{}
type VerificationT[T any] struct {
	Policy      *pr.Policy
	Witness     *pr.Witness
	Transaction T
	Error       any
}
type Verification = VerificationT[*pr.Transaction]

const TEST_FUEL = 100

const BooleanNS string = "https://example.qredo.com/boolean_policy?"

var BooleanParticipants []KeyAuthority = []KeyAuthority{
	{Cooked: "F"},
	{Cooked: "T"},
	{Prefix: BooleanNS, Cooked: "false"},
	{Prefix: BooleanNS, Cooked: "true"},
	{Prefix: BooleanNS, Raw: []byte{0}},
	{Prefix: BooleanNS, Raw: []byte{1}},
}

var kaF KeyAuthority = KeyAuthority{Prefix: BooleanNS, Cooked: "false", Raw: []byte{0}}
var kaT KeyAuthority = KeyAuthority{Prefix: BooleanNS, Cooked: "true", Raw: []byte{1}}

var BooleanParticipantsMap map[string]Authority = map[string]Authority{
	"F":  kaF,
	"T":  kaT,
	"FF": kaF,
	"TT": kaT,
}

func FuelLimitPolicy(within bool) Witnessed {
	kids := TEST_FUEL
	if !within {
		kids++
	}

	subs := make([]*pr.Policy, kids)
	for i := 0; i < kids; i++ {
		subs[i] = All(nil)
	}
	return Witnessed{
		Policy:  Any(uint64(kids), subs),
		Witness: Guess(),
	}
}

func MkPolicyPBroken(participants []KeyAuthority, kids []tree.Sampler, r *rand.Rand) Broken {
	invalidAddressPrefix := func(prefix, cooked string) Broken {
		_, err := url.Parse(prefix)
		return Broken{
			Policy: SigK(KeyAuthority{Prefix: prefix, Cooked: cooked}),
			Error: ErrInvalidAddressPrefix{
				NonPrefix: prefix,
				URLError:  err.Error(),
			},
		}
	}

	invalidParticipant := func(cooked string) Broken {
		return Broken{
			Policy: SigK(KeyAuthority{Cooked: cooked}),
			Error:  ErrInvalidParticipantReference{cooked},
		}
	}
	nonParticipant := func(auth KeyAuthority) Broken {
		return Broken{
			Policy: SigK(auth),
			Error:  ErrNonParticipantReference{auth.String()},
		}
	}

	if len(kids) == 0 {
		switch r.Intn(14) {
		case 0:
			return invalidAddressPrefix("NOT%A%URI", "BAD")
		case 1:
			return nonParticipant(KeyAuthority{Prefix: BooleanNS})
		case 2:
			return nonParticipant(KeyAuthority{Prefix: BooleanNS, Cooked: "WHO"})
		case 3:
			return nonParticipant(KeyAuthority{Prefix: BooleanNS, Raw: []byte{86}})
		case 4:
			return nonParticipant(KeyAuthority{Cooked: "WHO"})
		case 5:
			return invalidParticipant("BAD!")
		case 6:
			return invalidParticipant("B_A_D")
		case 7:
			return invalidParticipant("")
		case 8:
			return nonParticipant(KeyAuthority{Prefix: "https://example.qredo.com/WHAT/", Cooked: "WHO"})
		case 9:
			var e ErrMalformedPolicy
			p := All([]*pr.Policy{})
			p.Threshold = 1
			return Broken{Policy: p, Error: e}
		case 10:
			var e ErrMalformedPolicy
			var p *pr.Policy
			if len(participants) > 0 {
				p = SigK(participants[r.Intn(len(participants))])
			} else {
				p = SigP("desperation")
			}
			p.Tag = pr.PolicyTag_POLICY_ALL
			return Broken{Policy: p, Error: e}
		case 11:
			var e ErrMalformedPolicy
			var p *pr.Policy
			if len(participants) > 0 {
				p = SigK(participants[r.Intn(len(participants))])
			} else {
				p = SigP("desperation")
			}
			p.Tag = pr.PolicyTag_POLICY_ANY
			return Broken{Policy: p, Error: e}
		case 12:
			var e ErrMalformedPolicy
			var p *pr.Policy
			if len(participants) > 0 {
				p = SigK(participants[r.Intn(len(participants))])
			} else {
				p = SigP("desperation")
			}
			p.Subpolicies = []*pr.Policy{&pr.Policy{}}
			return Broken{Policy: p, Error: e}
		default:
			p := &pr.Policy{}
			return Broken{
				Policy: p,
				Error:  ErrUnknownPolicy(p.Tag),
			}
		}
	}
	subs := make([]*pr.Policy, 0, len(kids))
	i := r.Intn(len(kids))
	var b Broken
	for ix, v := range kids {
		if ix == i {
			b = MkPolicyPBroken(participants, v.NextTrees(r), r)
			subs = append(subs, b.Policy)
		} else {
			subs = append(subs, MkPolicyP(participants, v.NextTrees(r), r))
		}
	}
	if r.Intn(2) == 0 {
		return Broken{
			Policy: All(subs),
			Error:  b.Error,
		}
	}
	return Broken{
		Policy: Any(uint64(r.Intn(2+len(kids))), subs),
		Error:  b.Error,
	}
}

func MkPolicyP(participants []KeyAuthority, kids []tree.Sampler, r *rand.Rand) *pr.Policy {
	if len(kids) == 0 {
		if kx := r.Intn(1+len(participants)) - 1; kx >= 0 {
			k := participants[kx]
			return SigK(k)
		}
	}
	subs := make([]*pr.Policy, 0, len(kids))
	for _, v := range kids {
		subs = append(subs, MkPolicyP(participants, v.NextTrees(r), r))
	}
	if r.Intn(2) == 0 {
		return All(subs)
	}
	return Any(uint64(r.Intn(2+len(kids))), subs)
}

func MkPolicyPP(participants []KeyAuthority) func([]tree.Sampler, *rand.Rand) *pr.Policy {
	return func(kids []tree.Sampler, r *rand.Rand) *pr.Policy {
		return MkPolicyP(participants, kids, r)
	}
}

var tf []KeyAuthority = []KeyAuthority{{Cooked: "F"}, {Cooked: "T"}}
var defaultSigs map[string]bool = map[string]bool{"F": false, "T": true}

func MkPolicy(kids []tree.Sampler, r *rand.Rand) *pr.Policy {
	return MkPolicyP(tf, kids, r)
}

func MkPolicyBroken(kids []tree.Sampler, r *rand.Rand) Broken {
	return MkPolicyPBroken(BooleanParticipants, kids, r)
}

func MkPolicyYes(kids []tree.Sampler, r *rand.Rand) *pr.Policy {
	n := len(kids)
	if n == 0 {
		if r.Intn(3) > 0 {
			return SigP("T")
		}
	}
	subs := make([]*pr.Policy, 0, n)
	if r.Intn(2) == 0 {
		for _, v := range kids {
			subs = append(subs, MkPolicyYes(v.NextTrees(r), r))
		}
		return All(subs)
	}
	mk := MkPolicy
	if r.Intn(2) == 0 {
		mk = MkPolicyNo
	}
	i := r.Intn(n + 1)
	j := i
	for _, v := range kids {
		var p *pr.Policy
		if r.Intn(n) < j {
			p = MkPolicyYes(v.NextTrees(r), r)
			j = j - 1
		} else {
			p = mk(v.NextTrees(r), r)
		}
		n = n - 1
		subs = append(subs, p)
	}
	return Any(uint64(i), subs)
}

func MkPolicyNo(kids []tree.Sampler, r *rand.Rand) *pr.Policy {
	n := len(kids)
	if n == 0 {
		if r.Intn(3) > 0 {
			return SigP("F")
		}
	}
	subs := make([]*pr.Policy, 0, n)
	mk := MkPolicy
	if r.Intn(2) == 0 {
		mk = MkPolicyYes
	}
	if n > 0 && r.Intn(2) == 0 {
		i := r.Intn(n)
		for j, v := range kids {
			if j == i {
				subs = append(subs, MkPolicyNo(v.NextTrees(r), r))
			} else {
				subs = append(subs, mk(v.NextTrees(r), r))
			}
		}
		return All(subs)
	}
	i := r.Intn(n + 1)
	j := i
	for _, v := range kids {
		var p *pr.Policy
		if r.Intn(n) >= j {
			p = MkPolicyNo(v.NextTrees(r), r)
		} else {
			p = mk(v.NextTrees(r), r)
			j = j - 1
		}
		n = n - 1
		subs = append(subs, p)
	}
	return Any(uint64(i)+1, subs)
}

func MkWitnessedYes(kids []tree.Sampler, r *rand.Rand) Witnessed {
	n := len(kids)
	if n == 0 {
		if r.Intn(3) > 0 {
			return Witnessed{
				Policy:  SigP("T"),
				Witness: Prechecked(),
			}
		}
	}
	subs := make([]*pr.Policy, 0, n)
	if r.Intn(2) == 0 {
		wits := make([]*pr.Witness, 0, n)
		for _, v := range kids {
			wd := MkWitnessedYes(v.NextTrees(r), r)
			subs = append(subs, wd.Policy)
			wits = append(wits, wd.Witness)
		}
		return Witnessed{
			Policy:  All(subs),
			Witness: AllW(wits),
		}
	}
	i := r.Intn(n + 1)
	j := i
	wits := map[uint64]*pr.Witness{}
	for ix, v := range kids {
		var p *pr.Policy
		if r.Intn(n) < j {
			wd := MkWitnessedYes(v.NextTrees(r), r)
			p = wd.Policy
			wits[uint64(ix)] = wd.Witness
			j = j - 1
		} else {
			// garbage because if these get looked at it's bad
			p = &pr.Policy{}
		}
		n = n - 1
		subs = append(subs, p)
	}
	return Witnessed{
		Policy:  Any(uint64(i), subs),
		Witness: AnyW(wits),
	}
}

func MkWitnessedNo(kids []tree.Sampler, r *rand.Rand) Witnessed {
	n := len(kids)
	if n == 0 {
		switch r.Intn(8) {
		case 1:
			return Witnessed{
				Policy:  SigP("F"),
				Witness: Prechecked(),
			}
		case 2:
			return Witnessed{
				Policy:  SigP("WHO"),
				Witness: Prechecked(),
			}
		case 3:
			return Witnessed{
				Policy:  SigP("F"),
				Witness: AllW([]*pr.Witness{}),
			}
		case 4:
			return Witnessed{
				Policy:  SigP("F"),
				Witness: AnyW(map[uint64]*pr.Witness{}),
			}
		case 5:
			return Witnessed{
				Policy:  SigK(KeyAuthority{Prefix: "", Raw: []byte{86}}),
				Witness: Prechecked(),
			}
		case 6:
			return Witnessed{
				Policy:  All(nil),
				Witness: Prechecked(),
			}
		case 7:
			return FuelLimitPolicy(false)
		default:
		}
	}
	subs := make([]*pr.Policy, 0, n)
	if n > 0 && r.Intn(2) == 0 {
		wits := make([]*pr.Witness, 0, n)
		i := r.Intn(n)
		for j, v := range kids {
			var wd Witnessed
			if j == i {
				wd = MkWitnessedNo(v.NextTrees(r), r)
			} else {
				wd = MkWitnessedYes(v.NextTrees(r), r)
			}
			subs = append(subs, wd.Policy)
			wits = append(wits, wd.Witness)
		}
		return Witnessed{
			Policy:  All(subs),
			Witness: AllW(wits),
		}
	}
	wits := map[uint64]*pr.Witness{}
	if n == 0 {
		if r.Intn(2) > 0 {
			wits[0] = Guess()
		}
		return Witnessed{
			Policy:  Any(1, subs),
			Witness: AnyW(wits),
		}
	}
	i := r.Intn(n) + 1
	j := i - 1
	bad := r.Intn(n)
	n = n - 1
	for ix, v := range kids {
		var p *pr.Policy
		if ix == bad {
			wd := MkWitnessedNo(v.NextTrees(r), r)
			p = wd.Policy
			wits[uint64(ix)] = wd.Witness
			n = n + 1
		} else if r.Intn(n) < j {
			wd := MkWitnessedYes(v.NextTrees(r), r)
			p = wd.Policy
			wits[uint64(ix)] = wd.Witness
			j = j - 1
		} else {
			p = &pr.Policy{}
		}
		n = n - 1
		subs = append(subs, p)
	}
	// XXX do insufficient ones as well as bad ones?
	return Witnessed{
		Policy:  Any(uint64(i), subs),
		Witness: AnyW(wits),
	}
}

func MkWitnessedBroken(kids []tree.Sampler, r *rand.Rand) BrokenlyWitnessed {
	n := len(kids)

	brokenAll := func(p *pr.Policy, tag pr.WitnessTag) BrokenlyWitnessed {
		return BrokenlyWitnessed{
			Policy: p,
			Witness: &pr.Witness{
				Tag:          tag,
				AllWitnesses: []*pr.Witness{Guess()},
			},
			Error: ErrUnexpectedAllWitnesses(tag),
		}
	}

	brokenAny := func(p *pr.Policy, tag pr.WitnessTag) BrokenlyWitnessed {
		return BrokenlyWitnessed{
			Policy: p,
			Witness: &pr.Witness{
				Tag:          tag,
				AnyWitnesses: map[uint64]*pr.Witness{0: Guess()},
			},
			Error: ErrUnexpectedAnyWitnesses(tag),
		}
	}

	if n == 0 {
		switch r.Intn(8) {

		case 0:
			w := &pr.Witness{}
			return BrokenlyWitnessed{
				Policy:  SigP("T"),
				Witness: w,
				Error:   ErrUnknownWitness(w.Tag),
			}
		case 1:
			return brokenAll(SigP("T"), pr.WitnessTag_WITNESS_GUESS)
		case 2:
			return brokenAny(SigP("T"), pr.WitnessTag_WITNESS_GUESS)
		case 3:
			return brokenAny(SigP("T"), pr.WitnessTag_WITNESS_ALL)
		case 4:
			return brokenAll(SigP("T"), pr.WitnessTag_WITNESS_ANY)
		case 5:
			return brokenAll(SigP("T"), pr.WitnessTag_WITNESS_PRECHECKED_SIGNATURE)
		case 6:
			return brokenAny(SigP("T"), pr.WitnessTag_WITNESS_PRECHECKED_SIGNATURE)
		default:
			p := &pr.Policy{}
			return BrokenlyWitnessed{
				Policy:  p,
				Witness: Guess(),
				Error:   ErrUnknownPolicy(p.Tag),
			}
		}
	}
	subs := make([]*pr.Policy, 0, n)
	if r.Intn(2) == 0 {
		wits := make([]*pr.Witness, 0, n)
		i := r.Intn(n)
		var e any
		for j, v := range kids {
			var wd Witnessed
			if j == i {
				bwd := MkWitnessedBroken(v.NextTrees(r), r)
				wd = Witnessed{Policy: bwd.Policy, Witness: bwd.Witness}
				e = bwd.Error
			} else {
				wd = MkWitnessedYes(v.NextTrees(r), r)
			}
			subs = append(subs, wd.Policy)
			wits = append(wits, wd.Witness)
		}
		return BrokenlyWitnessed{
			Policy:  All(subs),
			Witness: AllW(wits),
			Error:   e,
		}
	}
	wits := map[uint64]*pr.Witness{}
	i := r.Intn(n) + 1
	j := i - 1
	bad := r.Intn(n)
	n = n - 1
	var e any
	for ix, v := range kids {
		var p *pr.Policy
		if ix == bad {
			bwd := MkWitnessedBroken(v.NextTrees(r), r)
			p = bwd.Policy
			wits[uint64(ix)] = bwd.Witness
			e = bwd.Error
			n = n + 1
		} else if r.Intn(n) < j {
			wd := MkWitnessedYes(v.NextTrees(r), r)
			p = wd.Policy
			wits[uint64(ix)] = wd.Witness
			j = j - 1
		} else {
			p = &pr.Policy{}
		}
		n = n - 1
		subs = append(subs, p)
	}
	return BrokenlyWitnessed{
		Policy:  Any(uint64(i), subs),
		Witness: AnyW(wits),
		Error:   e,
	}
}

func MkAssetFlow(adjustments map[uint64][]byte, r *rand.Rand) *pr.AssetFlow {
	p := BooleanParticipants[r.Intn(len(BooleanParticipants))]

	return &pr.AssetFlow{
		AddressPrefix: p.Prefix,
		CookedAddress: p.Cooked,
		RawAddress:    p.Raw,
		Adjustments:   adjustments,
		AntiReplay:    false,
	}
}

func MkEmptyTransaction(r *rand.Rand) *pr.Transaction {
	return &pr.Transaction{
		Tag:    pr.TransactionTag_TRANSACTION_IMMEDIATE,
		Assets: []*pr.Asset{},
		Flows:  []*pr.AssetFlow{},
		Focus:  0,
	}
}

func oneof[A any](r *rand.Rand, options ...A) A {
	return options[r.Intn(len(options))]
}

func GenPolicyThresholdWellFormed(kids []tree.Sampler, r *rand.Rand) Witnessed {
	v := GenThresholdClosure(r.Intn(2) == 0, kids, r)
	return Witnessed{
		Policy:      v.Policy,
		Witness:     v.Witness,
		Transaction: v.Transaction,
	}
}

func GenPolicyThresholdIllFormed(kids []tree.Sampler, r *rand.Rand) Broken {
	w := GenThresholdClosure(r.Intn(2) == 0, kids, r)
	err := malform(w.Policy, 0, r)

	return Broken{
		Policy: w.Policy,
		Error:  err,
	}
}

func GenPolicyThresholdNo(kids []tree.Sampler, r *rand.Rand) BrokenlyWitnessed {
	return BrokenlyWitnessed(GenPolicyThreshold(false)(kids, r))
}

func GenPolicyThresholdYes(kids []tree.Sampler, r *rand.Rand) Witnessed {
	v := GenPolicyThreshold(true)(kids, r)
	return Witnessed{
		Policy:      v.Policy,
		Transaction: v.Transaction,
		Witness:     v.Witness,
	}
}

func GenPolicyThreshold(satisfied bool) func([]tree.Sampler, *rand.Rand) Verification {
	return func(kids []tree.Sampler, r *rand.Rand) Verification {
		return GenThresholdClosure(satisfied, kids, r)
	}
}

func shuffle[X any](xs []X, r *rand.Rand) []X {
	shuffled := copySlice(xs)

	r.Shuffle(len(xs), func(i, j int) {
		xs[i], xs[j] = xs[j], xs[i]
	})

	return shuffled
}

func malform(p *pr.Policy, runoff uint64, r *rand.Rand) error {
	clearSubpolicies := func() error {
		p.Subpolicies = []*pr.Policy{}
		return ErrMalformedPolicy{}
	}

	malformSubpolicy := func() error {
		sp := p.Subpolicies[r.Intn(len(p.Subpolicies))]
		return malform(sp, runoff, r)
	}

	useUnimplementedTableDAG := func() error {
		p.Subpolicies = append(p.Subpolicies, p.Subpolicies[0])
		return ErrUnimplementedRef{}
	}

	spliceUnverifiablePolicy := func() error {
		tag := pr.PolicyTag_POLICY_ASSETS
		p.Subpolicies[r.Intn(len(p.Subpolicies))] = &pr.Policy{
			Tag: tag,
		}
		return ErrUnverifiablePolicy{tag}
	}

	spliceMalformedSubpolicy := func() error {
		p.Subpolicies[r.Intn(len(p.Subpolicies))] = &pr.Policy{
			Tag:       pr.PolicyTag_POLICY_ALL,
			Threshold: 1,
		}
		return ErrMalformedPolicy{}
	}

	spliceUnsupportedSubpolicy := func() error {
		tag := pr.PolicyTag_POLICY_ALL
		p.Subpolicies[r.Intn(len(p.Subpolicies))] = &pr.Policy{
			Tag: tag,
		}
		return ErrUnsupportedSubpolicy{
			policy: p.Tag,
			sub:    tag,
		}
	}

	injectMalformedOpcode := func() error {
		op := 0x7F + byte(r.Intn(0xFF-0x7F)+1)
		p.Thunk = append(p.Thunk, op)
		return ErrInvalidBytecode{op}
	}

	malformAsset := func() error {
		a := p.AssetDefs[r.Intn(len(p.AssetDefs))]
		if r.Intn(2) == 0 {
			a.Tag = pr.AssetProtocolTag_ASSET_PROTOCOL_UNSPECIFIED
			return ErrInvalidProtocol{a.Tag}
		} else {
			a.AddressPrefix = InvalidString(r)
			return ErrInvalidUTF8{a.AddressPrefix}
		}
	}

	duplicateAsset := func() error {
		a := GenAsset(GenBytes(r), r)
		low := uint(len(p.AssetDefs))
		p.AssetDefs = append(p.AssetDefs, a, a)
		return ErrDuplicatedAssets{
			tag:  p.Tag,
			low:  low,
			high: low + 1,
		}
	}
	/*

		clearAssets := func() error {
			p.AssetDefs = []*pr.Asset{}
			return ErrUnresolvedPolicyAssets{}
		}
			popAsset := func() error {
				p.AssetDefs = p.AssetDefs[:len(p.AssetDefs)-1]
				return ErrUnresolvedPolicyAssets{}
			}
	*/
	options := []func() error{}
	o := func(f func() error) {
		options = append(options, f)
	}

	switch p.Tag {
	case pr.PolicyTag_POLICY_TABLE:
		runoff += uint64(len(p.Subpolicies))

		o(duplicateAsset)
		o(spliceUnverifiablePolicy)
		o(spliceMalformedSubpolicy)

		if len(p.Subpolicies) > 0 {
			o(clearSubpolicies)
			o(malformSubpolicy)
			o(useUnimplementedTableDAG)
		}
	case pr.PolicyTag_POLICY_CLOSURE:
		o(injectMalformedOpcode)
		if len(p.Subpolicies) > 0 {
			o(malformSubpolicy)
			o(spliceUnsupportedSubpolicy)
		}

	case pr.PolicyTag_POLICY_ASSETS:
		o(duplicateAsset)

		if len(p.AssetDefs) > 0 {
			o(malformAsset)
		}
		/*
					isUsed := func(i int) bool {
				zero := bignum2bytes(big.NewInt(0))
				v, present := p.Assets[uint64(i)]
				return present && bytes.Compare(v, zero) != 0
			}

					if isUsed(len(p.AssetDefs) - 1) {
						o(popAsset)
					}

				for i, _ := range p.AssetDefs {
					if isUsed(i) {
						o(clearAssets)
						break
					}
				}
		*/

	}

	return options[r.Intn(len(options))]()
}

type InvalidateThresholdTag uint

const (
	// Keep 0 free so we can use default value as no invalidation
	InvalidateThresholdTag_Comparison InvalidateThresholdTag = iota + 1
	InvalidateThresholdTag_Focus
	InvalidateThresholdTag_AllEval
	InvalidateThresholdTag_AllTable
	InvalidateThresholdTag_TagEval
	InvalidateThresholdTag_TagTable
	InvalidateThresholdTag_Peek
	InvalidateThresholdTag_Pop
	InvalidateThresholdTag_TableSubwitness
	InvalidateThresholdTag_Transaction
	InvalidateThresholdTag_Yank
)

type rollTable[K comparable] struct {
	odds map[K]float64
}

func MakeRollTable[K comparable](odds map[K]uint32) rollTable[K] {
	if len(odds) == 0 {
		panic("Invalid odds")
	}

	i := rollTable[K]{
		odds: map[K]float64{},
	}

	total := float64(0)
	for _, v := range odds {
		total += float64(v)
	}
	for k, v := range odds {
		i.odds[k] = float64(v) / total
	}

	return i
}

func Roll[K cmp.Ordered](r *rand.Rand, i rollTable[K]) K {
	roll := r.Float64()
	ks := sortedKeys(i.odds)
	for _, k := range ks {
		v := i.odds[k]
		if roll <= v {
			return k
		} else {
			roll -= v
		}
	}

	return ks[len(ks)-1]
}

var invalidationRollTable = MakeRollTable(map[InvalidateThresholdTag]uint32{
	InvalidateThresholdTag_Comparison:      1,
	InvalidateThresholdTag_Focus:           1,
	InvalidateThresholdTag_TagEval:         1,
	InvalidateThresholdTag_TagTable:        1,
	InvalidateThresholdTag_AllEval:         1,
	InvalidateThresholdTag_AllTable:        1,
	InvalidateThresholdTag_Peek:            1,
	InvalidateThresholdTag_Pop:             1,
	InvalidateThresholdTag_TableSubwitness: 1,
	InvalidateThresholdTag_Transaction:     1,
	InvalidateThresholdTag_Yank:            1,
})

type OutflowThreshold struct {
	policy  *AssetInfo
	table   *AssetInfo
	tx      *AssetInfo
	allowEQ bool
}

func MkOutflowThreshold(satisfied bool, kids []tree.Sampler, r *rand.Rand) *OutflowThreshold {
	addUnused := func(ai *AssetInfo, n int) {
		unusedAsset := func(ai *AssetInfo) {
			i := uint64(len(ai.defs))
			prefix := []byte{uint8(i)} //TODO: more robust
			a := GenAsset(prefix, r)
			ai.defs = append(ai.defs, a)
			if r.Intn(2) == 0 {
				ai.adjustments[i] = bignum2bytes(big.NewInt(0))
			}
		}

		for i := 0; i < n; i++ {
			unusedAsset(ai)
		}
	}

	policyAssets := GenAssets(r, uint8(r.Intn(5)), 0)
	addUnused(policyAssets, r.Intn(3))

	tableAssets := GenAssets(r, uint8(r.Intn(5)), uint8(len(policyAssets.defs)))

	l := uint64(len(policyAssets.defs))
	for k, v := range tableAssets.adjustments {
		policyAssets.adjustments[l+k] = v
	}

	txAssets := &AssetInfo{
		defs:        append(copySlice(policyAssets.defs), tableAssets.defs...),
		adjustments: cloneMap(policyAssets.adjustments),
	}

	addUnused(txAssets, r.Intn(3))

	for _, a := range policyAssets.defs {
		if r.Intn(2) == 0 {
			tableAssets.defs = append(tableAssets.defs, a)
		}
	}

	allowEQ := r.Intn(2) == 0

	mustModify := satisfied != allowEQ

	if mustModify || r.Intn(2) == 0 {
		modifyAssets(r, txAssets, satisfied)
	}

	return &OutflowThreshold{
		policy:  policyAssets,
		table:   tableAssets,
		tx:      txAssets,
		allowEQ: allowEQ,
	}
}

func GenThresholdClosure2(kids []tree.Sampler, r *rand.Rand) Verification {
	type Locations = map[AssetComparable]*table_location

	pool := GenAssetDefs(5, 0, r)

	register := func(defs []*pr.Asset, above uint64, locations Locations) Locations {
		locs := cloneMap(locations)

		te := &table_entry{
			assets: defs,
			added:  []*pr.Asset{},
			above:  above,
		}

		for i, a := range defs {
			insertIfMissing(locs, asset2AssetComparable(a), &table_location{
				frame: te,
				index: uint64(i),
			})
		}

		return locs
	}

	draw := func() []*pr.Asset {
		defs := []*pr.Asset{}
		for _, a := range pool {
			if r.Intn(2) == 0 {
				defs = append(defs, a)
			}
		}
		return defs
	}

	type V = VerificationT[Unit]
	allV := func(vs ...*V) *VerificationT[Unit] {
		return &V{
			Policy:      All(mapSlice(vs, func(v *V) *pr.Policy { return v.Policy })),
			Witness:     Guess(),
			Transaction: Unit{},
			Error:       nil,
		}
	}

	tableV := func(v *VerificationT[Unit], defs []*pr.Asset) *VerificationT[Unit] {
		return &VerificationT[Unit]{
			Policy:  TableP([]*pr.Policy{v.Policy}, defs),
			Witness: guessable(r, TableW(v.Witness)),
			Error:   nil,
		}
	}

	transaction := &pr.Transaction{}

	var rec func(uint64, Locations, bool, []tree.Sampler) *VerificationT[Unit]
	rec = func(above uint64, locations Locations, outflowBranch bool, children []tree.Sampler) *VerificationT[Unit] {
		defs := draw()

		verification := func(p *pr.Policy) *VerificationT[Unit] {
			return &VerificationT[Unit]{
				Policy:      p,
				Witness:     guessable(r, EvalW(guessable(r, AllW([]*pr.Witness{})))),
				Transaction: Unit{},
				Error:       nil,
			}
		}

		outflowClosure := func() (*pr.Policy, *pr.Transaction) {
			policyAdjs := map[uint64][]byte{}
			txAdjs := map[uint64][]byte{}

			for i, d := range defs {
				amount := bignum2bytes(GenBigInt(false, true)(r))
				index := uint64(i)
				txAdjs[index] = amount

				if l, present := locations[asset2AssetComparable(d)]; present {
					runoff := above - l.frame.above - uint64(len(l.frame.assets)) + l.index
					index = runoff + uint64(len(defs))
				}

				policyAdjs[index] = amount
			}
			return mkOutflowClosure(defs, policyAdjs, txAdjs, r)
		}

		k := len(children)
		if k > 0 {
			o := r.Intn(k)
			locs := register(defs, above, locations)
			branches := make([]*VerificationT[Unit], k)
			for i, v := range children {
				branches[i] = rec(above+uint64(len(defs)), locs, outflowBranch && i == o, v.NextTrees(r))
			}
			return tableV(allV(branches...), defs)
		} else {
			if outflowBranch {
				p, tx := outflowClosure()
				transaction = tx
				return verification(p)
			} else {
				return verification(mkSimpleAssetsClosure(r, true))
			}
		}
	}

	v := rec(0, Locations{}, true, kids)
	return Verification{
		Policy:      v.Policy,
		Witness:     v.Witness,
		Transaction: transaction,
		Error:       nil,
	}
}

func mkSimpleAssetsClosure(r *rand.Rand, result bool) *pr.Policy {
	zero := &pr.Policy{
		Tag:       pr.PolicyTag_POLICY_ASSETS,
		Assets:    map[uint64][]byte{},
		AssetDefs: []*pr.Asset{},
	}

	switch r.Intn(10) {
	case 0:
		return ClosureP(
			[]*pr.Policy{zero},
			[]byte{Opcode_GEZ},
		)
	default:
		defs := GenAssetDefs(uint8(r.Intn(5)), 0, r)

		pa := &pr.Policy{
			Tag:       pr.PolicyTag_POLICY_ASSETS,
			Assets:    GenAdjustments(uint64(len(defs)), false, false, r),
			AssetDefs: defs,
		}

		comparison := Opcode_GEZ
		if len(defs) > 0 && r.Intn(2) == 0 {
			comparison = Opcode_GTZ
		}

		return ClosureP([]*pr.Policy{pa}, []byte{comparison})
	}
}

func mkOutflowClosure(defs []*pr.Asset, policyAdjustments, txAdjustments map[uint64][]byte, r *rand.Rand) (*pr.Policy, *pr.Transaction) {
	t := &pr.Transaction{
		Tag:    pr.TransactionTag_TRANSACTION_IMMEDIATE,
		Assets: defs,
		Flows: []*pr.AssetFlow{
			MkAssetFlow(txAdjustments, r),
		},
		Focus: hardcodedFocus,
	}

	pa := &pr.Policy{
		Tag:       pr.PolicyTag_POLICY_ASSETS,
		Assets:    policyAdjustments,
		AssetDefs: defs,
	}

	opcodes := append(comparisonOpcodes, Opcode_GEZ)

	return ClosureP([]*pr.Policy{pa}, opcodes), t
}

var comparisonOpcodes = []byte{
	Opcode_LOADTXN,
	Opcode_FOCUS,
	Opcode_OUTFLOW,
	Opcode_SUBASSETS,
}

func GenThresholdClosure(satisfied bool, kids []tree.Sampler, r *rand.Rand) Verification {
	var err error = ErrInvalidWitness{}
	var invalidation InvalidateThresholdTag

	if !satisfied {
		invalidation = Roll(r, invalidationRollTable)
	}

	ot := MkOutflowThreshold(invalidation != InvalidateThresholdTag_Comparison, kids, r)
	policyAssets := ot.policy
	tableAssets := ot.table
	txAssets := ot.tx

	comparison := ifThenElse(ot.allowEQ, Opcode_GEZ, Opcode_GTZ)

	if invalidation == InvalidateThresholdTag_Transaction {
		l := len(txAssets.defs)
		offset := uint64(l + 1)
		txAssets.adjustments[offset] = bignum2bytes(big.NewInt(0))
		err = ErrUnresolvedTransactionAssets{
			flow:   hardcodedFocus,
			offset: offset,
			len:    uint(l),
		}
	}

	t := &pr.Transaction{
		Tag:    pr.TransactionTag_TRANSACTION_IMMEDIATE,
		Assets: txAssets.defs,
		Flows: []*pr.AssetFlow{
			MkAssetFlow(txAssets.adjustments, r),
		},
		Focus: hardcodedFocus,
	}

	if invalidation == InvalidateThresholdTag_Focus {
		t.Focus = hardcodedFocus + 1
		err = ErrUnresolvedFocus{
			focus: hardcodedFocus,
			flows: uint64(len(t.Flows)),
		}
	}

	pa := &pr.Policy{
		Tag:       pr.PolicyTag_POLICY_ASSETS,
		Assets:    policyAssets.adjustments,
		AssetDefs: policyAssets.defs,
	}

	mkClosure := func() *pr.Policy {
		stack := []*pr.Policy{pa}

		if invalidation == InvalidateThresholdTag_Pop {
			err = ErrStackOverrun{0}
			sub := []byte{Opcode_SUBASSETS}
			switch r.Intn(3) {
			case 0:
				return ClosureP(nil, nil) // fails when popping result of thunk
			case 1:
				return ClosureP(nil, sub) // fails when popping 1st arg
			default:
				return ClosureP(stack, sub) // fails when popping 2nd arg
			}
		}

		opcodes := []byte{}

		repeatStackTop := func(n byte) {
			var opcode byte

			if n == 0 {
				opcode = Opcode_NOP
			} else {
				opcode = Opcode_YANK_1 + n - 1
			}

			for i := byte(0); i < n+1; i++ {
				opcodes = append(opcodes, opcode)
			}

			for i := len(stack); i < int(n)+1; i++ {
				stack = append(stack, pa)
			}
		}

		switch invalidation {
		case InvalidateThresholdTag_Peek:
			stack = []*pr.Policy{}
			span := Opcode_PEEK_3 - Opcode_PEEK_0
			i := 1 + byte(r.Intn(int(span)))
			opcodes = append(opcodes, Opcode_PEEK_0+i)
			err = ErrStackOverrun{uint(i)}
		case InvalidateThresholdTag_Yank:
			span := 1 + Opcode_YANK_3 - Opcode_YANK_1
			i := r.Intn(int(span))
			repeatStackTop(byte(i))
			stack = stack[:len(stack)-1]
			err = ErrStackOverrun{uint(i)}
		default:
			span := 1 + Opcode_YANK_3 - Opcode_NOP
			i := r.Intn(int(span))
			repeatStackTop(byte(i))
		}

		opcodes = append(opcodes, comparisonOpcodes...)

		// Doubling should not interfere with comparisons to zero
		if r.Intn(2) == 0 {
			opcodes = append(opcodes, Opcode_PEEK_0)
			opcodes = append(opcodes, Opcode_ADDASSETS)
		}

		opcodes = append(opcodes, comparison)

		return ClosureP(stack, opcodes)
	}

	unexpectedSubwitnesses := func(w *pr.Witness) error {
		w.AllWitnesses = []*pr.Witness{Guess()}
		return ErrUnexpectedAllWitnesses(w.Tag)
	}

	invalidateTag := func(v *Verification, tag pr.WitnessTag) {
		v.Witness.Tag = tag
		v.Error = ErrUnexpectedTag{
			policyTag:  v.Policy.Tag,
			witnessTag: tag,
		}
	}

	pc := mkClosure()

	wc := &pr.Witness{
		Tag:         pr.WitnessTag_WITNESS_EVAL,
		NextWitness: guessable(r, AllW([]*pr.Witness{})),
	}

	if invalidation == InvalidateThresholdTag_AllEval {
		err = unexpectedSubwitnesses(wc)
	} else {
		wc = guessable(r, wc)
	}

	v := &Verification{
		Policy:      pc,
		Transaction: t,
		Witness:     wc,
		Error:       err,
	}

	if invalidation == InvalidateThresholdTag_TagEval {
		invalidateTag(v, pr.WitnessTag_WITNESS_ALL)
		wc.NextWitness = nil
		return *wrapInTables(v, tableAssets.defs, false, r)
	}

	tbl := wrapInTables(v, tableAssets.defs, satisfied, r)

	if invalidation == InvalidateThresholdTag_AllTable {
		tbl.Error = unexpectedSubwitnesses(tbl.Witness)
	}

	if invalidation == InvalidateThresholdTag_TagTable {
		tbl := wrapInTable(tbl, []*pr.Asset{}, false, r)
		invalidateTag(tbl, pr.WitnessTag_WITNESS_ANY)
		return *tbl
	}

	if invalidation == InvalidateThresholdTag_TableSubwitness {
		tbl := wrapInTable(tbl, []*pr.Asset{}, false, r)
		any := map[uint64]*pr.Witness{}

		if assumesSingletonTable(r.Intn(2) == 0) {
			any[0] = Guess()
			any[1] = Guess()
		}

		tbl.Witness.AnyWitnesses = any
		tbl.Error = ErrInvalidWitness{}
		return *tbl
	}

	return *tbl
}

func wrapInTable(v *Verification, defs []*pr.Asset, canGuess bool, r *rand.Rand) *Verification {
	wt := &pr.Witness{
		Tag: pr.WitnessTag_WITNESS_TABLE,
		AnyWitnesses: map[uint64]*pr.Witness{
			0: v.Witness,
		},
	}

	pt := TableP([]*pr.Policy{v.Policy}, defs)
	if canGuess {
		wt = guessable(r, wt)
	}

	return &Verification{
		Policy:      pt,
		Transaction: v.Transaction,
		Witness:     wt,
		Error:       v.Error,
	}
}

func wrapInTables(v *Verification, defs []*pr.Asset, canGuess bool, r *rand.Rand) *Verification {
	if len(defs) == 0 && r.Intn(10) > 0 {
		return v
	}

	cut := (len(defs) + 1) / 2
	inner := defs[:cut]
	outer := defs[cut:]

	return wrapInTables(wrapInTable(v, inner, canGuess, r), outer, canGuess, r)
}

func modifyAssets(r *rand.Rand, assetGen *AssetInfo, signBit bool) {
	modification := func() *big.Int {
		abs := big.NewInt(int64(1 + r.Uint32()))
		if signBit {
			return abs.Neg(abs)
		} else {
			return abs
		}
	}

	increases := uint(0)
	for k, v := range assetGen.adjustments {
		if r.Intn(2) == 0 {
			increases++
			assetGen.adjustments[k] = bignum2bytes(bigAdd(bytes2bignum(v), modification()))
		}
	}

	if increases > 0 && r.Intn(2) == 0 {
		return
	}

	//TODO: more robust
	indexStart := len(assetGen.defs)

	inserts := 1 + uint8(r.Intn(4))
	assetGen.defs = append(assetGen.defs, GenAssetDefs(inserts, uint8(indexStart), r)...)
	for i := uint(0); i < uint(inserts); i++ {
		assetGen.adjustments[uint64(indexStart)] = bignum2bytes(modification())
		indexStart++
	}
}

func RandBigInt(r *rand.Rand, min, span *big.Int) *big.Int {
	z := big.NewInt(0)
	z.Rand(r, span)
	z.Add(z, min)
	return z
}

func GenBigInt(allowZero, allowNegative bool) func(r *rand.Rand) *big.Int {
	return func(r *rand.Rand) *big.Int {
		one := big.NewInt(1)
		two := big.NewInt(2)
		maxBits := big.NewInt(int64(r.Intn(255)))

		maxBits.Sub(maxBits, one)
		magnitude := big.NewInt(0)
		magnitude.Exp(two, maxBits, nil)

		min := big.NewInt(0)
		if !allowZero {
			min = one
		}

		res := RandBigInt(r, min, magnitude)

		if allowNegative && r.Intn(2) == 0 {
			res.Neg(res)
		}

		return res
	}
}

var protocolCount = len(pr.AssetProtocolTag_name)

func GenProtocol(r *rand.Rand) pr.AssetProtocolTag {
	return pr.AssetProtocolTag(1 + r.Intn(protocolCount-1)) // Skip _UNSPECIFIED
}

func GenBytes(r *rand.Rand) []byte {
	bs := []byte{}
	l := 1 + r.Intn(5)
	for i := 0; i < l; i++ {
		bs = append(bs, byte(r.Uint64()%256))
	}
	return bs
}

func GenString(r *rand.Rand) string {
	bs := []byte{}
	l := 1 + r.Intn(5)
	for i := 0; i < l; i++ {
		bs = append(bs, byte(r.Uint64()%128))
	}
	return string(bs)
}

func GenAsset(prefix []byte, r *rand.Rand) *pr.Asset {
	utf8 := func() []byte {
		return append(prefix, []byte(GenString(r))...)
	}

	return &pr.Asset{
		Tag:           GenProtocol(r),
		ChainId:       utf8(),
		AddressPrefix: GenString(r),
		RawAddress:    utf8(),
		Selector:      utf8(),
	}
}

// TODO: support more than generating 128 accumulated asset defs?
func GenAssetDefs(count, uniquePrefixStart uint8, r *rand.Rand) []*pr.Asset {
	defs := []*pr.Asset{}

	mkAsset := func(i uint8) *pr.Asset {
		return GenAsset([]byte{uniquePrefixStart + i}, r)
	}

	if count == 0 {
		return defs
	}

	a := *mkAsset(0)
	defs = append(defs, &a)

	for i := uint8(1); i < count; i++ {
		if r.Intn(2) == 0 {
			defs = append(defs, mkAsset(i))
		} else {
			copy := a
			prefix := []byte{uniquePrefixStart + i}

			switch r.Intn(4) {
			case 0:
				copy.ChainId = append(prefix, bytes.Clone(a.ChainId)...)
			case 1:
				copy.RawAddress = append(prefix, bytes.Clone(a.RawAddress)...)
			case 2:
				copy.Selector = append(prefix, bytes.Clone(a.Selector)...)
			default:
				copy.AddressPrefix = string(prefix) + a.AddressPrefix
			}
			defs = append(defs, &copy)
		}
	}
	return shuffle(defs, r)
}

func GenAdjustments(assetsCount uint64, allowZero, allowNegative bool, r *rand.Rand) map[uint64][]byte {
	adjustments := make(map[uint64][]byte)

	for i := uint64(0); i < assetsCount; i++ {
		adjustments[i] = bignum2bytes(GenBigInt(allowZero, allowNegative)(r))
	}

	return adjustments
}

func GenAssets(r *rand.Rand, count, uniquePrefixStart uint8) *AssetInfo {
	defs := GenAssetDefs(count, uniquePrefixStart, r)

	return &AssetInfo{
		defs:        defs,
		adjustments: GenAdjustments(uint64(count), false, true, r),
	}
}

func GenSimple[T any](mk func(*rand.Rand) T) gopter.Gen {
	return func(genParams *gopter.GenParameters) *gopter.GenResult {
		return gopter.NewGenResult(mk(genParams.Rng), gopter.NoShrinker)
	}
}

func Gen[T any](f func([]tree.Sampler, *rand.Rand) T, t func(uint64) tree.Sampler) gopter.Gen {
	return gen.Sized(func(size int) gopter.Gen {
		return func(params *gopter.GenParameters) *gopter.GenResult {
			if size == 0 {
				size = 1
			}
			return gopter.NewGenResult(f(t(uint64(size)).NextTrees(params.Rng), params.Rng), gopter.NoShrinker)
		}
	})
}

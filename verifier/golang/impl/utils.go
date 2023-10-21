package impl

import (
	"cmp"
	"fmt"
	"math/big"
	"sort"

	pr "github.com/qredo/blackbird/verifier/golang/protobuf"
)

func ifThenElse[A any](b bool, t, f A) A {
	if b {
		return t
	} else {
		return f
	}
}

func copySlice[T any](xs []T) []T {
	ys := make([]T, len(xs))
	copy(ys, xs)
	return ys
}

func cloneMap[K comparable, V any](m map[K]V) map[K]V {
	m_ := make(map[K]V, len(m))
	for k, v := range m {
		m_[k] = v
	}
	return m_
}

func mapSlice[X, Y any](xs []X, f func(X) Y) []Y {
	ys := make([]Y, len(xs))
	for i, x := range xs {
		ys[i] = f(x)
	}
	return ys
}

func keys[K comparable, V any](m map[K]V) []K {
	ks := make([]K, 0, len(m))
	for k := range m {
		ks = append(ks, k)
	}
	return ks
}

func sortedKeysBy[K comparable, V any](m map[K]V, less func(K, K) bool) []K {
	ks := keys(m)
	sort.Slice(ks, func(i, j int) bool { return less(ks[i], ks[j]) })
	return ks
}

func sortedKeys[K cmp.Ordered, V any](m map[K]V) []K {
	return sortedKeysBy(m, func(a, b K) bool { return a < b })
}

func insertIf[K comparable, V any](shouldInsert func(K, V) bool, shouldReplace func(K, V, V) bool) func(map[K]V, K, V) {
	return func(m map[K]V, k K, v V) {
		if shouldInsert(k, v) {
			if old, conflict := m[k]; !conflict || shouldReplace(k, v, old) {
				m[k] = v
			}
		}
	}
}

func insertIfMissing[K comparable, V any](m map[K]V, k K, v V) {
	insertIf(
		func(K, V) bool { return true },
		func(K, V, V) bool { return false },
	)(m, k, v)
}

type merge[L any, R any, Z any] struct {
	left  func(L) Z
	right func(R) Z
	both  func(L, R) Z
}

func id[X any](x X) X {
	return x
}
func bigNeg(x *big.Int) *big.Int {
	return x.Neg(x)
}

// TODO: error checking
func bytes2bignum(bs []byte) *big.Int {
	i := new(big.Int)
	i.SetBytes(bs[1:])
	neg := bs[0] == 1
	if neg {
		i.Neg(i)
	}
	return i
}

// TODO: error checking
func bignum2bytes(i *big.Int) []byte {
	var neg byte = 0
	if i.Sign() < 0 {
		neg = 1
	}
	return append([]byte{byte(neg)}, i.Bytes()...)
}

func bigAdd(x, y *big.Int) *big.Int {
	var i big.Int
	i.Add(x, y)
	return &i
}
func bigSub(x, y *big.Int) *big.Int {
	var i big.Int
	i.Sub(x, y)
	return &i
}

var mergeAdd = &merge[*big.Int, *big.Int, *big.Int]{
	left:  id[*big.Int],
	right: id[*big.Int],
	both:  bigAdd,
}
var mergeSub = &merge[*big.Int, *big.Int, *big.Int]{
	left:  id[*big.Int],
	right: bigNeg,
	both:  bigSub,
}

func constantPolicy(val bool) *pr.Policy {
	if val {
		return All([]*pr.Policy{})
	} else {
		return Any(1, []*pr.Policy{})
	}
}

func unverifiablePolicy(policy *pr.Policy) error {
	return unexpectedPolicy(policy, func(tag pr.PolicyTag) error {
		return ErrUnverifiablePolicy{tag}
	})
}

func unexpectedPolicy(policy *pr.Policy, err func(pr.PolicyTag) error) error {
	tag := policy.Tag
	if _, present := pr.PolicyTag_name[int32(tag)]; present && tag != pr.PolicyTag_POLICY_UNSPECIFIED {
		return err(tag)
	} else {
		return ErrUnknownPolicy(tag)
	}
}

func printTags(p *pr.Policy, indent uint) {
	spaces := ""
	for i := uint(0); i < indent*2; i++ {
		spaces += " "
	}
	fmt.Println(spaces + p.Tag.String())
	for _, sp := range p.Subpolicies {
		printTags(sp, indent+1)
	}
}

func prettyPrintProtocol(tag pr.AssetProtocolTag) string {
	switch tag {
	case pr.AssetProtocolTag_ASSET_PROTOCOL_BTC:
		return "BTC"
	case pr.AssetProtocolTag_ASSET_PROTOCOL_EVM:
		return "EVM"
	case pr.AssetProtocolTag_ASSET_PROTOCOL_ERC20:
		return "ERC20"
	case pr.AssetProtocolTag_ASSET_PROTOCOL_COSMOS:
		return "COSMOS"
	default:
		return "???"
	}
}

func validProtocol(tag pr.AssetProtocolTag) bool {
	return tag > 0 && tag < pr.AssetProtocolTag(len(pr.AssetProtocolTag_name))
}

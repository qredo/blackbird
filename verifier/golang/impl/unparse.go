package impl

import (
	"encoding/hex"
	"fmt"
	"math/big"
	"strconv"
	"strings"

	pr "gitlab.qredo.com/edmund/blackbird/verifier/golang/protobuf"
)

const (
	inApp = iota
	inAnd
	inOr
	inList
)

var FAKE_ASSET_PROTOCOL_OUTFLOW = pr.AssetProtocolTag(-1)

func appendAssets(defs []*pr.Asset, builder *strings.Builder) map[AssetComparable]string {
	tickers := map[AssetComparable]string{}

	for i, d := range defs {
		if d.Tag == FAKE_ASSET_PROTOCOL_OUTFLOW {
			tickers[asset2AssetComparable(d)] = "gross_outflow"
			continue
		}

		hx := hex.EncodeToString

		str := builder.WriteString

		kv := func(k, v string) {
			str("  ")
			str(k)
			str(" = ")
			str(v)
			str("\n")
		}

		quoted := func(s string) string {
			return "\"" + s + "\""
		}

		ticker := "Asset_" + strconv.Itoa(i)
		tickers[asset2AssetComparable(d)] = ticker

		str("define asset ")
		str(ticker)
		str(": {\n")

		kv("protocol", prettyPrintProtocol(d.Tag))
		kv("chain", quoted(hx(d.ChainId)))
		kv("address", quoted(hx([]byte(assetAuthority(d).String()))))
		kv("decimals", strconv.Itoa(0))

		str("};\n")
	}

	return tickers
}

func appendThreshold(strict bool, defs []*pr.Asset, adjs map[uint64][]byte, context int, builder *strings.Builder) {
	tickers := appendAssets(defs, builder)

	type Side = map[AssetComparable]*big.Int

	if len(adjs) == 0 {
		appendPolicy(constantPolicy(!strict), nil, context, builder)
		return
	}

	lhs := Side{}
	rhs := Side{}

	for _, k := range sortedKeys(adjs) {
		if k >= uint64(len(defs)) {
			break
		}

		amount := bytes2bignum(adjs[k])
		a := asset2AssetComparable(defs[k])

		if amount.Sign() > 0 {
			rhs[a] = amount
		} else if amount.Sign() < 0 {
			lhs[a] = amount.Neg(amount)
		}
	}

	one := big.NewInt(1)

	pp := func(s Side) {
		if len(s) == 0 {
			builder.WriteString("0")
			return
		}

		for i, d := range sortedKeysBy(s, assetLess) {
			if i > 0 {
				builder.WriteString(" + ")
			}

			amount := s[d]

			if d.Tag == FAKE_ASSET_PROTOCOL_OUTFLOW {
				if amount.Cmp(one) != 0 {
					builder.WriteString(amount.String())
					builder.WriteString(" * ")
				}
			} else {
				builder.WriteString(amount.String())
			}
			builder.WriteString(tickers[d])
		}
	}

	pp(lhs)
	builder.WriteString(" " + ifThenElse(strict, "<", "<=") + " ")
	pp(rhs)
}

func appendPolicy(policy *pr.Policy, tables *table_stack, context int, builder *strings.Builder) {
	unknown := "???"

	switch policy.Tag {

	case pr.PolicyTag_POLICY_ALL:
		switch len(policy.Subpolicies) {
		case 0:
			builder.WriteString("all []")
		case 1:
			appendPolicy(policy.Subpolicies[0], tables, context, builder)
		default:
			if context < inAnd {
				builder.WriteString("(")
			}
			appendPolicy(policy.Subpolicies[0], tables, inAnd, builder)
			for _, p := range policy.Subpolicies[1:] {
				builder.WriteString(" and ")
				appendPolicy(p, tables, inAnd, builder)
			}
			if context < inAnd {
				builder.WriteString(")")
			}
		}

	case pr.PolicyTag_POLICY_ANY:
		switch len(policy.Subpolicies) {
		case 0:
			fmt.Fprintf(builder, "any %d []", policy.Threshold)
		case 1:
			if policy.Threshold == 1 {
				appendPolicy(policy.Subpolicies[0], tables, context, builder)
			} else {
				fmt.Fprintf(builder, "any %d [", policy.Threshold)
				appendPolicy(policy.Subpolicies[0], tables, inList, builder)
				builder.WriteString("]")
			}
		default:
			if policy.Threshold == 1 {
				if context < inOr {
					builder.WriteString("(")
				}
				appendPolicy(policy.Subpolicies[0], tables, inOr, builder)
				for _, p := range policy.Subpolicies[1:] {
					builder.WriteString(" or ")
					appendPolicy(p, tables, inAnd, builder)
				}
				if context < inOr {
					builder.WriteString(")")
				}
			} else {
				fmt.Fprintf(builder, "any %d [", policy.Threshold)
				appendPolicy(policy.Subpolicies[0], tables, inList, builder)
				for _, p := range policy.Subpolicies[1:] {
					builder.WriteString(", ")
					appendPolicy(p, tables, inList, builder)
				}
				builder.WriteString("]")
			}
		}

	case pr.PolicyTag_POLICY_SIGNATURE:
		if policy.AddressPrefix == "" && participant_regexp.MatchString(policy.GetCookedAddress()) {
			builder.WriteString("@")
			builder.WriteString(policy.GetCookedAddress())
		} else {
			builder.WriteString(unknown)
		}

	case pr.PolicyTag_POLICY_TABLE:
		ts := addTableEntry(tables, policy.AssetDefs)
		if len(policy.Subpolicies) != assumesSingletonTable(1) {
			builder.WriteString(unknown)
		}
		appendPolicy(policy.Subpolicies[0], ts, context, builder)
		popTableEntry(ts)

	case pr.PolicyTag_POLICY_CLOSURE:
		if len(policy.Subpolicies) != 1 {
			builder.WriteString(unknown)
		}

		comparison := func(strict bool) func(p *pr.Policy) (*pr.Policy, error) {
			return func(p *pr.Policy) (*pr.Policy, error) {
				appendThreshold(strict, p.AssetDefs, p.Assets, context, builder)
				return nil, nil
			}
		}

		e := &evaluation{
			gez: comparison(false),
			gtz: comparison(true),

			add: opcodeArithmetic(tables, mergeAdd),
			sub: opcodeArithmetic(tables, mergeSub),

			loadtxn: func() (*pr.Policy, error) {
				d := &pr.Asset{
					Tag:           FAKE_ASSET_PROTOCOL_OUTFLOW,
					ChainId:       []byte{},
					AddressPrefix: "",
					RawAddress:    []byte{},
					Selector:      []byte{},
				}

				f := &pr.AssetFlow{
					AddressPrefix: "",
					CookedAddress: "",
					RawAddress:    []byte{},
					Adjustments:   map[uint64][]byte{0: bignum2bytes(big.NewInt(1))},
					AntiReplay:    false,
				}

				t := &pr.Transaction{
					Tag:    0,
					Assets: []*pr.Asset{d},
					Flows:  []*pr.AssetFlow{f},
					Focus:  0,
				}
				return TransactionP(t), nil
			},

			unknown: func(opcode byte) error {
				return ErrInvalidBytecode{opcode}
			},
		}

		evalThunk(policy, e)

	default:
		builder.WriteString(unknown)
	}
}

func Unparse(policy *pr.Policy) string {
	var builder strings.Builder
	appendPolicy(policy, newTableStack(), inList, &builder)
	return builder.String()
}

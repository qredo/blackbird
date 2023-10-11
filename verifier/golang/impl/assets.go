package impl

import (
	"bytes"
	"encoding/hex"
	"math/big"
	"strings"

	pr "gitlab.qredo.com/edmund/blackbird/verifier/golang/protobuf"
)

func assetAuthority(a *pr.Asset) *KeyAuthority {
	return &KeyAuthority{
		Prefix: a.AddressPrefix,
		Cooked: "",
		Raw:    a.RawAddress,
	}
}

// Like pr.Asset, but with byte[] encoded as hex strings to serve as map keys
type AssetComparable struct {
	Tag           pr.AssetProtocolTag
	ChainId       string
	AddressPrefix string
	RawAddress    string
	Selector      string
}

// TODO: Use base32 or so?
func asset2AssetComparable(a *pr.Asset) AssetComparable {
	encode := hex.EncodeToString

	return AssetComparable{
		Tag:           a.Tag,
		ChainId:       encode(a.ChainId),
		AddressPrefix: a.AddressPrefix,
		RawAddress:    encode(a.RawAddress),
		Selector:      encode(a.Selector),
	}
}

func assetLess(a, b AssetComparable) bool {
	return assetCompare(a, b) < 0
}

func assetCompare(a, b AssetComparable) int {
	if cmp := int(a.Tag - b.Tag); cmp != 0 {
		return cmp
	}
	if cmp := strings.Compare(a.ChainId, b.ChainId); cmp != 0 {
		return cmp
	}
	if cmp := strings.Compare(a.AddressPrefix, b.AddressPrefix); cmp != 0 {
		return cmp
	}
	if cmp := strings.Compare(a.RawAddress, b.RawAddress); cmp != 0 {
		return cmp
	}
	return strings.Compare(a.Selector, b.Selector)
}

func assetsGTZ(assets map[uint64][]byte, strict bool) bool {
	nonzero := false
	for _, a := range assets {
		b := bytes2bignum(a)
		if b.Sign() < 0 {
			return false
		}
		nonzero = nonzero || b.Sign() > 0
	}

	return !strict || nonzero
}

func mergeAssets(s *table_stack, x, y *AssetInfo, combine *merge[*big.Int, *big.Int, *big.Int]) *AssetInfo {
	defs := []*pr.Asset{}
	adjustments := map[uint64][]byte{}
	offsets := map[AssetComparable]uint64{}

	insertDefs := func(as []*pr.Asset) {
		for _, a := range as {
			ad := asset2AssetComparable(a)
			if _, present := lookupDef(s, a); !present {
				if _, present := offsets[ad]; !present {
					offsets[ad] = uint64(len(defs))
					defs = append(defs, a)
				}
			}
		}
	}

	insertDefs(x.defs)
	insertDefs(y.defs)

	l := uint64(len(defs))

	adjs := func(a *AssetInfo, only func(*big.Int) *big.Int, both func(*big.Int, *big.Int) *big.Int) {
		for k, v := range a.adjustments {
			var d *pr.Asset
			runoff := int(k) - len(a.defs)
			if runoff < 0 {
				d = a.defs[k]
			} else {
				d = lookupOffset(s, uint64(runoff))
			}
			ad := asset2AssetComparable(d)

			var o uint64
			if ot, present := lookupDef(s, d); present {
				o = l + ot
			} else {
				o = offsets[ad]
			}

			v_ := bytes2bignum(v)
			if old, present := adjustments[o]; present {
				adjustments[o] = bignum2bytes(both(bytes2bignum(old), v_))
			} else {
				adjustments[o] = bignum2bytes(only(v_))
			}

			if bytes.Compare(adjustments[o], bignum2bytes(big.NewInt(0))) == 0 {
				delete(adjustments, o)
			}
		}
	}

	adjs(x, combine.left, combine.both)
	adjs(y, combine.right, combine.both)

	return &AssetInfo{
		defs:        defs,
		adjustments: adjustments,
	}
}

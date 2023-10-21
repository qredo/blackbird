package simple

import (
	"github.com/qredo/blackbird/verifier/golang/impl"
	pr "github.com/qredo/blackbird/verifier/golang/protobuf"
	"google.golang.org/protobuf/proto"
)

func Verify(policy []byte, witness []byte, transaction []byte, query_fn func([]byte) ([]byte, error), signer_map map[string]bool) error {
	p := pr.Policy{}
	w := *impl.Guess()
	tx := pr.Transaction{}

	if p_err := proto.Unmarshal(policy, &p); p_err != nil {
		return impl.ErrCouldNotDecodePolicy{p_err}
	}
	if len(witness) == 0 {
		// don't need to decode witness
	} else if w_err := proto.Unmarshal(witness, &w); w_err != nil {
		return impl.ErrCouldNotDecodeWitness{w_err}
	}
	if len(transaction) == 0 {
		// don't need to decode
	} else if tx_err := proto.Unmarshal(transaction, &tx); tx_err != nil {
		return impl.ErrCouldNotDecodeTransaction{tx_err}
	}
	return impl.VerifyEngine(impl.TEST_FUEL, p, w, tx, signer_map)
}

func InstallCheck(policy []byte, query_fn func([]byte) ([]byte, error), participants map[string]impl.Authority) ([]byte, error) {
	p := pr.Policy{}
	if p_err := proto.Unmarshal(policy, &p); p_err != nil {
		return nil, impl.ErrCouldNotDecodePolicy{p_err}
	}
	p2, err := impl.InstallCheck(p, participants)
	if err != nil {
		return nil, err
	}
	policy2, p2_err := proto.MarshalOptions{Deterministic: true}.Marshal(&p2)
	if p2_err != nil {
		return nil, impl.ErrCouldNotEncodePolicy{p2_err}
	}
	return policy2, nil
}

func SyntacticallyEqual(policy1 []byte, policy2 []byte) (bool, error) {
	p1 := pr.Policy{}
	p2 := pr.Policy{}
	if p1_err := proto.Unmarshal(policy1, &p1); p1_err != nil {
		return false, impl.ErrCouldNotDecodePolicy{p1_err}
	}
	if p2_err := proto.Unmarshal(policy2, &p2); p2_err != nil {
		return false, impl.ErrCouldNotDecodePolicy{p2_err}
	}
	return proto.Equal(&p1, &p2), nil
}

func Unparse(policy []byte) (string, error) {
	p := pr.Policy{}
	if err := proto.Unmarshal(policy, &p); err != nil {
		return "???", impl.ErrCouldNotDecodePolicy{err}
	}
	return impl.Unparse(&p), nil
}

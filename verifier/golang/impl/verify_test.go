package impl

import (
	"fmt"
	"math/big"
	"os"
	"reflect"
	"strconv"
	"testing"

	"github.com/leanovate/gopter"
	"github.com/leanovate/gopter/prop"
	pr "github.com/qredo/blackbird/verifier/golang/protobuf"
	"github.com/qredo/blackbird/verifier/golang/tree"
)

func testParameters() *gopter.TestParameters {
	parameters := gopter.DefaultTestParameters()
	parameters.MinSuccessfulTests = 1e3
	parameters.SetSeed(1695190480687391000)
	if str, present := os.LookupEnv("GO_MIN_SUCCESSFUL_TESTS"); present {
		if m, err := strconv.Atoi(str); err != nil {
			panic("Found GO_MIN_SUCCESSFUL_TESTS but could not parse it")
		} else {
			parameters.MinSuccessfulTests = m
		}
	}
	return parameters
}

func withVerify(fuel int, policy pr.Policy, witness pr.Witness, transaction pr.Transaction, sigs map[string]bool, f func(err any) bool) bool {
	err := VerifyEngine(fuel, policy, witness, transaction, sigs)
	return f(err)
}

func outputUnexpected[X any](expected, received X) {
	fmt.Println("Expected:", expected)
	fmt.Println("Received:", received)
	fmt.Println("")
}

func expecting(expected any) func(received any) bool {
	return func(received any) bool {
		if reflect.DeepEqual(expected, received) {
			return true
		}
		outputUnexpected(expected, received)
		return false
	}
}

func noError(err any) bool {
	return expecting(nil)(err)
}

func TestVerify(t *testing.T) {
	parameters := testParameters()

	properties := gopter.NewProperties(parameters)

	properties.Property("Roundtrips bignum encode/decode", prop.ForAll(
		func(i *big.Int) bool {
			return i.Cmp(bytes2bignum(bignum2bytes(i))) == 0
		}, GenSimple(GenBigInt(true, true)),
	))

	properties.Property("Guesses yes for yes instances", prop.ForAll(
		func(policy *pr.Policy, tx *pr.Transaction) bool {
			return withVerify(TEST_FUEL, *policy, *Guess(), *tx, defaultSigs, noError)
		},
		Gen(MkPolicyYes, tree.DoCatalan),
		GenSimple(MkEmptyTransaction),
	))

	properties.Property("Guesses no for no instances", prop.ForAll(
		func(policy *pr.Policy, tx *pr.Transaction) bool {
			return withVerify(TEST_FUEL, *policy, *Guess(), *tx, defaultSigs, expecting(ErrInvalidWitness{}))
		},
		Gen(MkPolicyNo, tree.DoCatalan),
		GenSimple(MkEmptyTransaction),
	))

	properties.Property("Checks yes for yes instances", prop.ForAll(
		func(wd Witnessed, tx *pr.Transaction) bool {
			return withVerify(TEST_FUEL, *wd.Policy, *wd.Witness, *tx, defaultSigs, noError)
		},
		Gen(MkWitnessedYes, tree.DoCatalan),
		GenSimple(MkEmptyTransaction),
	))

	properties.Property("Checks no for no instances", prop.ForAll(
		func(wd Witnessed, tx *pr.Transaction) bool {
			return withVerify(TEST_FUEL, *wd.Policy, *wd.Witness, *tx, defaultSigs, func(err any) bool {
				return err == ErrInvalidWitness{} || err == ErrFailedToGuessWitness{}
			})
		},
		Gen(MkWitnessedNo, tree.DoCatalan),
		GenSimple(MkEmptyTransaction),
	))

	properties.Property("Errors for broken instances", prop.ForAll(
		func(br BrokenlyWitnessed, tx *pr.Transaction) bool {
			return withVerify(TEST_FUEL, *br.Policy, *br.Witness, *tx, defaultSigs, expecting(br.Error))
		},
		Gen(MkWitnessedBroken, tree.DoCatalan),
		GenSimple(MkEmptyTransaction),
	))

	properties.Property("Is deterministic for asset thresholds", prop.ForAll(
		func(wd Witnessed) bool {
			return withVerify(100, *wd.Policy, *wd.Witness, *wd.Transaction, defaultSigs, func(err1 any) bool {
				return withVerify(100, *wd.Policy, *wd.Witness, *wd.Transaction, defaultSigs, expecting(err1))
			})
		},
		Gen(GenPolicyThresholdWellFormed, tree.DoCatalan),
	))

	properties.Property("Checks yes for satisfied asset thresholds", prop.ForAll(
		func(w Witnessed) bool {
			return withVerify(100, *w.Policy, *w.Witness, *w.Transaction, defaultSigs, noError)
		},
		Gen(GenPolicyThresholdYes, tree.DoCatalan),
	))

	properties.Property("Errors for unsatisfied asset thresholds", prop.ForAll(
		func(b BrokenlyWitnessed) bool {
			return withVerify(100, *b.Policy, *b.Witness, *b.Transaction, defaultSigs, expecting(b.Error))
		},
		Gen(GenPolicyThresholdNo, tree.DoCatalan),
	))

	properties.TestingRun(t)
}

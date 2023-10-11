package impl

import (
	"reflect"
	"testing"

	"github.com/leanovate/gopter"
	"github.com/leanovate/gopter/prop"
	pr "gitlab.qredo.com/edmund/blackbird/verifier/golang/protobuf"
	"gitlab.qredo.com/edmund/blackbird/verifier/golang/tree"
)

func withInstalled(pre pr.Policy, participants map[string]Authority, f func(pr.Policy) bool) bool {
	post, err := InstallCheck(pre, participants)
	if err == nil {
		return f(post)
	}
	outputUnexpected(nil, err)
	return false
}

func TestInstall(t *testing.T) {
	parameters := testParameters()

	properties := gopter.NewProperties(parameters)

	properties.Property("Guesses yes for yes instances", prop.ForAll(
		func(p0 *pr.Policy, tx *pr.Transaction) bool {
			return withInstalled(*p0, BooleanParticipantsMap, func(p1 pr.Policy) bool {
				return withVerify(TEST_FUEL, p1, *Guess(), *tx, defaultSigs, noError)
			})
		},
		Gen(MkPolicyYes, tree.DoCatalan),
		GenSimple(MkEmptyTransaction),
	))

	properties.Property("Guesses no for no instances", prop.ForAll(
		func(p0 *pr.Policy, tx *pr.Transaction) bool {
			return withInstalled(*p0, BooleanParticipantsMap, func(p1 pr.Policy) bool {
				return withVerify(TEST_FUEL, p1, *Guess(), *tx, defaultSigs, func(err any) bool {
					return err == ErrInvalidWitness{} || err == ErrFailedToGuessWitness{}
				})
			})
		},
		Gen(MkPolicyNo, tree.DoCatalan),
		GenSimple(MkEmptyTransaction),
	))

	properties.Property("Guesses same for general instances 1", prop.ForAll(
		func(p0 *pr.Policy, tx *pr.Transaction) bool {
			err0 := VerifyEngine(TEST_FUEL, *p0, *Guess(), *tx, defaultSigs)
			return withInstalled(*p0, BooleanParticipantsMap, func(p1 pr.Policy) bool {
				return withVerify(TEST_FUEL, p1, *Guess(), *tx, defaultSigs, expecting(err0))
			})
		},
		Gen(MkPolicy, tree.DoCatalan),
		GenSimple(MkEmptyTransaction),
	))

	properties.Property("Guesses same for general instances 2", prop.ForAll(
		func(p0 *pr.Policy, tx *pr.Transaction) bool {
			return withInstalled(*p0, BooleanParticipantsMap, func(p1 pr.Policy) bool {
				return withVerify(TEST_FUEL, *p0, *Guess(), *tx, defaultSigs, func(err0 any) bool {
					return withVerify(TEST_FUEL, p1, *Guess(), *tx, defaultSigs, expecting(err0))
				})
			})
		},
		Gen(MkPolicy, tree.DoCatalan),
		GenSimple(MkEmptyTransaction),
	))

	properties.Property("Is deterministic for good instances", prop.ForAll(
		func(policy *pr.Policy, tx *pr.Transaction) bool {
			policy2, err := InstallCheck(*policy, BooleanParticipantsMap)
			policy3, err2 := InstallCheck(policy2, BooleanParticipantsMap)

			return reflect.DeepEqual(&policy2, &policy3) && reflect.DeepEqual(err, err2)
		},
		Gen(MkPolicyPP(BooleanParticipants), tree.DoCatalan),
		GenSimple(MkEmptyTransaction),
	))

	properties.Property("Is idempotent for good instances", prop.ForAll(
		func(p0 *pr.Policy, tx *pr.Transaction) bool {
			return withInstalled(*p0, BooleanParticipantsMap, func(p1 pr.Policy) bool {
				return withInstalled(p1, BooleanParticipantsMap, func(p2 pr.Policy) bool {
					return reflect.DeepEqual(&p1, &p2)
				})
			})
		},
		Gen(MkPolicyPP(BooleanParticipants), tree.DoCatalan),
		GenSimple(MkEmptyTransaction),
	))

	properties.Property("Errors for broken instances", prop.ForAll(
		func(br Broken) bool {
			_, result := InstallCheck(*br.Policy, BooleanParticipantsMap)
			return reflect.DeepEqual(result, br.Error)
		},
		Gen(MkPolicyBroken, tree.DoCatalan),
	))

	properties.Property("Succeeds for well-formed satisfied thresholds", prop.ForAll(
		func(w Witnessed) bool {
			return withInstalled(*w.Policy, BooleanParticipantsMap, func(p1 pr.Policy) bool {
				return withVerify(100, p1, *w.Witness, *w.Transaction, defaultSigs, noError)
			})
		},
		Gen(GenPolicyThresholdYes, tree.DoCatalan),
	))

	properties.Property("Fails verification for unsatisfied thresholds", prop.ForAll(
		func(v BrokenlyWitnessed) bool {
			return withInstalled(*v.Policy, BooleanParticipantsMap, func(p1 pr.Policy) bool {
				return withVerify(100, p1, *v.Witness, *v.Transaction, defaultSigs, expecting(v.Error))
			})
		},
		Gen(GenPolicyThresholdNo, tree.DoCatalan),
	))

	properties.Property("Fails install for malformed asset thresholds", prop.ForAll(
		func(br Broken) bool {
			_, err := InstallCheck(*br.Policy, BooleanParticipantsMap)
			if reflect.DeepEqual(err, br.Error) {
				return true
			}
			outputUnexpected[any](err, br.Error)
			return false
		},
		Gen(GenPolicyThresholdIllFormed, tree.DoCatalan),
	))

	properties.Property("Really works", prop.ForAll(
		func(v Verification) bool {
			return withInstalled(*v.Policy, BooleanParticipantsMap, func(p1 pr.Policy) bool {
				return withVerify(100, p1, *v.Witness, *v.Transaction, defaultSigs, func(err any) bool {
					if err != nil {
						outputUnexpected(nil, err)
						VerifyEngine(100, p1, *v.Witness, *v.Transaction, defaultSigs)
					}
					return err == nil
				})
			})
		},
		Gen(GenThresholdClosure2, tree.DoCatalan),
	))

	properties.TestingRun(t)
}

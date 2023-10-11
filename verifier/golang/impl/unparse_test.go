package impl

import (
	"regexp"
	"strings"
	"testing"

	"github.com/leanovate/gopter"
	"github.com/leanovate/gopter/prop"
	pr "gitlab.qredo.com/edmund/blackbird/verifier/golang/protobuf"
	"gitlab.qredo.com/edmund/blackbird/verifier/golang/tree"
)

var crunches []*regexp.Regexp = []*regexp.Regexp{
	regexp.MustCompile(`[@][A-Za-z0-9.]+`),
	regexp.MustCompile(`\?\?\?`),
	regexp.MustCompile(`!( and !)+`),
	regexp.MustCompile(`!( or !)+`),
	regexp.MustCompile(`(any (0|[1-9][0-9]*)|all) \[(!(, !)*)?\]`),
	regexp.MustCompile(`\(!\)`),
}

func crunch(s string) string {
	for {
		old := s
		for _, re := range crunches {
			s = re.ReplaceAllLiteralString(s, "!")
			if s == "!" {
				return s
			}
		}
		if old == s {
			return s
		}
	}
}

func crunchable(s string) bool {
	if strings.Contains(s, "!") {
		return false
	}
	return crunch(s) == "!"
}

func TestUnparse(t *testing.T) {
	parameters := testParameters()

	properties := gopter.NewProperties(parameters)
	properties.Property("Prints simple policies according to syntax", prop.ForAll(
		func(policy *pr.Policy) bool {
			result := Unparse(policy)
			return !strings.Contains(result, "???") && crunchable(result)
		},
		Gen(MkPolicy, tree.DoCatalan),
	))

	properties.Property("Prints oddish policies basically okay", prop.ForAll(
		func(policy *pr.Policy) bool {
			result := Unparse(policy)
			return crunchable(result)
		},
		Gen(MkPolicyPP(BooleanParticipants), tree.DoCatalan),
	))

	properties.Property("Safely prints broken instances", prop.ForAll(
		func(br Broken) bool {
			result := Unparse(br.Policy)
			return crunchable(result)
		},
		Gen(MkPolicyBroken, tree.DoCatalan),
	))

	properties.TestingRun(t)
}

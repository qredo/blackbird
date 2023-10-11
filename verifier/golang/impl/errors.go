package impl

import (
	"fmt"

	pr "gitlab.qredo.com/edmund/blackbird/verifier/golang/protobuf"
)

type ErrUnimplementedRef struct {
}

func (ErrUnimplementedRef) Error() string {
	return "POLICY_TABLE currently only supports a single sub-policy"
}

type ErrInvalidBytecode struct {
	opcode byte
}

func (this ErrInvalidBytecode) Error() string {
	return fmt.Sprintf("Found invalid opcode: %v", this.opcode)
}

type ErrInvalidUTF8 struct {
	text string
}

func (this ErrInvalidUTF8) Error() string {
	return fmt.Sprintf("Supplied string is not valid utf-8: %v", this.text)
}

type ErrInvalidProtocol struct {
	tag pr.AssetProtocolTag
}

func (this ErrInvalidProtocol) Error() string {
	return fmt.Sprintf("Asset has invalid protocol tag: %v", this.tag)
}

type ErrDuplicatedAssets struct {
	tag  pr.PolicyTag
	low  uint
	high uint
}

func (this ErrDuplicatedAssets) Error() string {
	return fmt.Sprintf("Found duplicate asset definitions at indexes %v and %v of %v", this.low, this.high, this.tag)
}

type ErrUnreferencedAssets struct {
}

func (ErrUnreferencedAssets) Error() string {
	return "Supplied asset definitions are not all used."
}

type ErrUnresolvedTransactionAssets struct {
	flow   uint64
	offset uint64
	len    uint
}

func (this ErrUnresolvedTransactionAssets) Error() string {
	return fmt.Sprintf("Flow %v refers to asset %v which is too high for array of asset definitions with len %v", this.flow, this.offset, this.len)
}

type ErrUnresolvedPolicyAssets struct{}

func (ErrUnresolvedPolicyAssets) Error() string {
	return "Supplied asset offsets are not defined in policy or surrounding tables."
}

type ErrUnresolvedFocus struct {
	focus uint64
	flows uint64
}

func (this ErrUnresolvedFocus) Error() string {
	return fmt.Sprintf("Supplied focus %v too high for array of defined asset flows with len %v.", this.focus, this.flows)
}

type ErrStackOverrun struct {
	depth uint
}

func (this ErrStackOverrun) Error() string {
	return fmt.Sprintf("Stack overrun when trying to access depth %v", this.depth)
}

type ErrFailedToGuessWitness struct {
}

func (ErrFailedToGuessWitness) Error() string {
	return "Could not guess witness; please supply a more explicit witness."
}

type ErrInvalidWitness struct {
}

func (ErrInvalidWitness) Error() string {
	return "Supplied witness is not a valid witness for the relevant policy."
}

type ErrUnexpectedPolicyField struct {
	fieldName string
	tag       pr.PolicyTag
}

func (this ErrUnexpectedPolicyField) Error() string {
	return fmt.Sprintf("%v field isn't valid for %v.", this.fieldName, this.tag)
}

type ErrUnexpectedWitnessesField struct {
	fieldName string
	tag       pr.WitnessTag
}

func (this ErrUnexpectedWitnessesField) Error() string {
	return fmt.Sprintf("%v field isn't valid for %v.", this.fieldName, this.tag)
}

func ErrUnexpectedAllWitnesses(tag pr.WitnessTag) ErrUnexpectedWitnessesField {
	return ErrUnexpectedWitnessesField{
		fieldName: "AllWitnesses",
		tag:       tag,
	}
}
func ErrUnexpectedAnyWitnesses(tag pr.WitnessTag) ErrUnexpectedWitnessesField {
	return ErrUnexpectedWitnessesField{
		fieldName: "AnyWitnesses",
		tag:       tag,
	}
}

func ErrUnexpectedNextWitness(tag pr.WitnessTag) ErrUnexpectedWitnessesField {
	return ErrUnexpectedWitnessesField{
		fieldName: "NextWitness",
		tag:       tag,
	}
}

type ErrUnexpectedTag struct {
	policyTag  pr.PolicyTag
	witnessTag pr.WitnessTag
}

func (this ErrUnexpectedTag) Error() string {
	return fmt.Sprintf("%v is not a valid witness for %v.", this.witnessTag, this.policyTag)
}

type ErrUnknownTag struct {
	tagged string
	tag    int32
}

func (this ErrUnknownTag) Error() string {
	return fmt.Sprintf("The current policy engine does not support %v with tag %v", this.tagged, this.tag)
}

func ErrUnknownWitness(tag pr.WitnessTag) ErrUnknownTag {
	return ErrUnknownTag{
		tagged: "witness",
		tag:    int32(tag),
	}
}
func ErrUnknownPolicy(tag pr.PolicyTag) ErrUnknownTag {
	return ErrUnknownTag{
		tagged: "policy",
		tag:    int32(tag),
	}
}

type ErrUnverifiablePolicy struct {
	policy pr.PolicyTag
}

func (this ErrUnverifiablePolicy) Error() string {
	return fmt.Sprintf("%v is not a standalone verifiable policy", this.policy)
}

type ErrUnsupportedSubpolicy struct {
	policy pr.PolicyTag
	sub    pr.PolicyTag
}

func (this ErrUnsupportedSubpolicy) Error() string {
	return fmt.Sprintf("%v cannot be a subpolicy of %v", this.sub, this.policy)
}

type ErrCouldNotDecodeTransaction struct {
	PBError error
}

func (this ErrCouldNotDecodeTransaction) Error() string {
	return "Transaction could not be decoded from serialized format:\n" + this.PBError.Error()
}

func (this ErrCouldNotDecodeTransaction) Unwrap() error {
	return this.PBError
}

type ErrCouldNotDecodeWitness struct {
	PBError error
}

func (this ErrCouldNotDecodeWitness) Error() string {
	return "Witness could not be decoded from serialized format:\n" + this.PBError.Error()
}

func (this ErrCouldNotDecodeWitness) Unwrap() error {
	return this.PBError
}

type ErrCouldNotDecodePolicy struct {
	PBError error
}

func (this ErrCouldNotDecodePolicy) Error() string {
	return "Policy could not be decoded from serialized format:\n" + this.PBError.Error()
}

func (this ErrCouldNotDecodePolicy) Unwrap() error {
	return this.PBError
}

type ErrCouldNotEncodePolicy struct {
	PBError error
}

func (this ErrCouldNotEncodePolicy) Error() string {
	return "Policy could not be encoded to serialized format:\n" + this.PBError.Error()
}

func (this ErrCouldNotEncodePolicy) Unwrap() error {
	return this.PBError
}

type ErrMalformedPolicy struct {
}

func (ErrMalformedPolicy) Error() string {
	return "Policy was not well-formed."
}

type ErrMalformedWitness struct {
}

func (ErrMalformedWitness) Error() string {
	return "Witness was not well-formed."
}

type ErrInvalidParticipantReference struct {
	NonParticipant string
}

func (this ErrInvalidParticipantReference) Error() string {
	return fmt.Sprintf("This policy refers to %q which is not a valid participant specification.", this.NonParticipant)
}

type ErrInvalidAddressPrefix struct {
	NonPrefix string
	URLError  string
}

func (this ErrInvalidAddressPrefix) Error() string {
	return fmt.Sprintf("This policy refers to %q which is not a valid address prefix:\n%s", this.NonPrefix, this.URLError)
}

type ErrNonParticipantReference struct {
	NonParticipant string
}

func (this ErrNonParticipantReference) Error() string {
	return fmt.Sprintf("This policy refers to %s which is not a recognized participant.", this.NonParticipant)
}

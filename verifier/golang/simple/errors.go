package simple

import (
	errors "github.com/qredo/blackbird/verifier/golang/impl"
)

type ErrCouldNotDecodePolicy = errors.ErrCouldNotDecodePolicy
type ErrCouldNotDecodeTransaction = errors.ErrCouldNotDecodeTransaction
type ErrCouldNotDecodeWitness = errors.ErrCouldNotDecodeWitness
type ErrCouldNotEncodePolicy = errors.ErrCouldNotEncodePolicy
type ErrDuplicatedAssets = errors.ErrDuplicatedAssets
type ErrFailedToGuessWitness = errors.ErrFailedToGuessWitness
type ErrInvalidAddressPrefix = errors.ErrInvalidAddressPrefix
type ErrInvalidBytecode = errors.ErrInvalidBytecode
type ErrInvalidParticipantReference = errors.ErrInvalidParticipantReference
type ErrInvalidProtocol = errors.ErrInvalidProtocol
type ErrInvalidUTF8 = errors.ErrInvalidUTF8
type ErrInvalidWitness = errors.ErrInvalidWitness
type ErrMalformedPolicy = errors.ErrMalformedPolicy
type ErrMalformedWitness = errors.ErrMalformedWitness
type ErrNonParticipantReference = errors.ErrNonParticipantReference
type ErrStackOverrun = errors.ErrStackOverrun
type ErrUnexpectedPolicyField = errors.ErrUnexpectedPolicyField
type ErrUnexpectedTag = errors.ErrUnexpectedTag
type ErrUnexpectedWitnessesField = errors.ErrUnexpectedWitnessesField
type ErrUnimplementedRef = errors.ErrUnimplementedRef
type ErrUnknownTag = errors.ErrUnknownTag
type ErrUnreferencedAssets = errors.ErrUnreferencedAssets
type ErrUnresolvedFocus = errors.ErrUnresolvedFocus
type ErrUnresolvedPolicyAssets = errors.ErrUnresolvedPolicyAssets
type ErrUnresolvedTransactionAssets = errors.ErrUnresolvedTransactionAssets
type ErrUnsupportedSubpolicy = errors.ErrUnsupportedSubpolicy
type ErrUnverifiablePolicy = errors.ErrUnverifiablePolicy

var ErrUnexpectedAllWitnesses = errors.ErrUnexpectedAllWitnesses
var ErrUnexpectedAnyWitnesses = errors.ErrUnexpectedAnyWitnesses
var ErrUnexpectedNextWitness = errors.ErrUnexpectedNextWitness
var ErrUnknownPolicy = errors.ErrUnknownPolicy
var ErrUnknownWitness = errors.ErrUnknownWitness

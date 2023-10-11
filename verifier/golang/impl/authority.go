package impl

import (
  "fmt"
  "net/url"
)

type Authority interface {
  AsAuthority() Authority
  fmt.Stringer
}

type AbstractAuthority struct {
}

func (this AbstractAuthority) AsAuthority() Authority {
  return this
}

func (AbstractAuthority) String() string {
  return "<unprintable>"
}

type KeyAuthority struct {
  Prefix string // prefix to turn the cooked string into a URI
  Cooked string
  Raw []byte
}

func (this KeyAuthority) AsAuthority() Authority {
  return this
}

func (this KeyAuthority) String() string {
  if this.Cooked == "" {
    return fmt.Sprintf("%s%02x", this.Prefix, this.Raw)
  } else if this.Prefix != "" {
    return this.Prefix + url.QueryEscape(this.Cooked)
  } else {
    return this.Cooked
  }
}

func ParticipantAsAuthority(participant string) Authority {
  return KeyAuthority{Cooked: participant}
}

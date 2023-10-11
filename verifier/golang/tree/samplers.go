package tree

import (
  // "fmt"
  "math/rand"
)

// top-down tree sampler
type Sampler interface {
  NextTrees(rand *rand.Rand) []Sampler
}

type Precomputed []Sampler

func (this Precomputed) NextTrees (r *rand.Rand) []Sampler {
  return this
}

type Catalan struct {
  Weight uint64
}

// Devroye, Flajolet, Hurtado, Noy, Steiger, Properties of Random Triangulations and Trees, 1999
func (this Catalan) NextTrees (r *rand.Rand) []Sampler {
  if this.Weight == 0 {
    return nil
  }
  n := this.Weight - 1
  var i uint64 = 0
  var j uint64 = 0
  stack := make([][]Sampler,0)
  node := make([]Sampler,0)
  if n >= 1 << 63 {
    panic("weight too big")
  }
  for i < n || j < n {
    if i < j {
      panic("bug in Catalan")
    }
    top := i + 2 - j
    if ((1 << 63) - 1)/top < n - i {
      panic("weight too big")
    }
    top = top * (n - i)
    bottom := i + 1 - j
    if ((1 << 63) - 1)/bottom < (n - i) + (n - j) {
      panic("weight too big")
    }
    bottom = bottom * ((n - i) + (n - j))
    // fmt.Printf("n=%d,i=%d,j=%d,bottom=%d\n", n, i, j, bottom)
    if r.Int63n(int64(bottom)) < int64(top) {
      i = i + 1
      stack = append(stack,node)
      node = make([]Sampler,0)
    } else {
      j = j + 1
      node = append(stack[len(stack)-1], Precomputed(node))
      stack = stack[:len(stack)-1]
    }
  }
  if len(stack) > 0 {
    panic("Catalan finished with non-empty stack")
  } else {
    return node
  }
}

func DoCatalan (n uint64) Sampler {
  return Catalan{n}
}

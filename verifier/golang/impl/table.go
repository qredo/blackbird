package impl

import (
	"fmt"

	pr "github.com/qredo/blackbird/verifier/golang/protobuf"
)

func assumesSingletonTable[X any](x X) X {
	return x
}

type table_location struct {
	frame *table_entry
	index uint64
}

type table_stack struct {
	locations map[AssetComparable]*table_location
	frames    []*table_entry
}

type table_entry struct {
	assets []*pr.Asset
	added  []*pr.Asset
	above  uint64
}

func tableStackSize(s *table_stack) uint64 {
	if len(s.frames) == 0 {
		return 0
	} else {
		top := s.frames[len(s.frames)-1]
		return top.above + uint64(len(top.assets))
	}
}

func newTableStack() *table_stack {
	t := &table_stack{
		locations: map[AssetComparable]*table_location{},
		frames:    []*table_entry{},
	}
	return t
}

func popTableEntry(s *table_stack) {
	l := len(s.frames)
	f := s.frames[l-1]
	for _, a := range f.added {
		delete(s.locations, asset2AssetComparable(a))
	}
	s.frames = s.frames[:l-1]
}

func addTableEntry(outer *table_stack, defs []*pr.Asset) *table_stack {
	f := &table_entry{
		assets: defs,
		above:  tableStackSize(outer),
	}

	inner := &table_stack{
		locations: outer.locations,
		frames:    copySlice(outer.frames),
	}

	inner.frames = append(inner.frames, f)

	for i, a := range defs {
		ad := asset2AssetComparable(a)
		if _, present := inner.locations[ad]; !present {
			f.added = append(f.added, a)
			inner.locations[ad] = &table_location{
				frame: f,
				index: uint64(i),
			}
		}
	}

	return inner
}

func lookupDef(s *table_stack, a *pr.Asset) (uint64, bool) {
	total := tableStackSize(s)
	f, present := s.locations[asset2AssetComparable(a)]
	if present {
		return f.index + total - f.frame.above - uint64(len(f.frame.assets)), true
	} else {
		return 0, false
	}
}

func lookupOffset(s *table_stack, offset uint64) *pr.Asset {
	for i := len(s.frames) - 1; i >= 0; i-- {
		t := s.frames[i]
		runoff := int(offset) - len(t.assets)
		if runoff < 0 {
			return t.assets[offset]
		} else {
			offset = uint64(runoff)
		}
	}
	return nil
}

func printStack(action string, s *table_stack) {
	fmt.Printf("=============\n")
	fmt.Printf("%v\n", action)
	fmt.Printf("Location: %v\n", len(s.locations))
	fmt.Printf("Frames\n")
	for _, f := range s.frames {
		fmt.Printf("  Above: %v, Assets: %v, Added: %v\n", f.above, len(f.assets), len(f.added))
	}
}

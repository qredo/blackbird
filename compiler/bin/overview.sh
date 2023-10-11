#!/bin/sh
find src -name "*.hs" -print | xargs graphmod --no-cluster -C Blackbird.Syntax -C Blackbird.Pretty -C Blackbird.Inference -C Blackbird.Unification -C Blackbird.Parser -C Blackbird.Console -C Blackbird.Binary -C Blackbird.Pattern -C Blackbird.Constraint -C Blackbird.Monitor -C Blackbird.Core -C Blackbird.Builtin -r Main | dot -Tpng > etc/overview.png

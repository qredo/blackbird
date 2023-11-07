mkdir -p verifier/golang/protobuf
protoc \
  --proto_path=protobuf \
  --go_opt=Mpolicy.proto=github.com/qredo/blackbird/verifier/golang/protobuf \
  --go_opt=module=github.com/qredo/blackbird/verifier/golang/protobuf \
  --go_out=verifier/golang/protobuf \
  protobuf/policy.proto

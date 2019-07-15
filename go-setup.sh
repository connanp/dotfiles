#!/usr/bin/env bash

export GOPATH=~/workspace/go
mkdir -p $GOPATH

go get -u github.com/motemen/gore/cmd/gore
go get -u github.com/mdempsky/gocode
go get -u golang.org/x/tools/cmd/godoc
go get -u golang.org/x/tools/cmd/goimports
go get -u golang.org/x/tools/cmd/gorename
go get -u golang.org/x/tools/cmd/guru
go get -u github.com/golangci/golangci-lint/cmd/golangci-lint
go get -u golang.org/x/tools/cmd/gopls
go get -u github.com/go-delve/delve/cmd/dlv
go get -u github.com/davidrjenni/reftools/cmd/fillstruct
go get -u github.com/josharian/impl
go get -u github.com/rogpeppe/godef
go get -u github.com/kisielk/errcheck

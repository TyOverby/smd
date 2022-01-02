#!/usr/bin/env bash

opam switch create ./ ocaml-base-compiler.4.12.1
opam repo add janestreet-bleeding https://ocaml.janestreet.com/opam-repository
opam repo add janestreet-bleeding-external https://github.com/janestreet/opam-repository.git#external-packages
opam update
opam install       \
  base             \
  dune             \
  core             \
  merlin           \
  bignum           \
  ppx_jane         \
  incr_map         \
  ocamlformat      \
  incremental      \
  incr_select      \
  ocaml-lsp-server \
  expect_test_helpers_core

# graphqlfmt-clj-experiment

A GraphQL (June 2018 edition) formatter made with Clojure & instaparse,
compiled via GraalVM.

## Goals

- To learn how to write a parser and a formatter
- To produce equivalent results compared to `prettier` using `--parser=graphql`

## Status

This is a learning project and not fit for production use. PRs welcome.

## Prerequisites for building

- Leiningen
  - https://leiningen.org/#install
- GNU Make
- wget
- openssl

## How to build and run

```
make build-linux
./target/graphqlfmt <<EOF
{hello world}
EOF

make build-darwin
./graphqlfmt <<EOF
{hello world}
EOF
```

## Scope

Some bits have been scoped out to get this out the door. PRs are welcome for
any of these.

- When we check for next elements from the head of a collection (like for
  example in `amend-newline-opts`), we don't account for Ignored tags such as
  Comments. It's likely if you add comments here and there, the formatting will
  break due to this.
- The consistency of usage for Ignored tags in the EBNF definitions is not well
  thought out and is likely lacking in some definitions as well.
- Comment formatting
- Error reporting
- Performance
- Support for parameterized grammar productions

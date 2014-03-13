Core-Haskell
============


# Description

A subset of Haskell using in UCC for teaching purpose. 
It enables a tiny subset of Haskell default, but the syntax can be customized,
teacher can enable more syntax along with the teaching progress.

# How to Install
First, install [Haskell Platform](http://www.haskell.org/platform/).

Then open your terminal or powershell, type following command.

## Via cabal (Recommand)
```
cabal install core-haskell
```

## Manually
```bash
cabal install hint haskeline haskell-src-exts
git clone https://github.com/happlebao/Core-Haskell.git
runghc Setup configure --user
runghc Setup build
runghc Setup install
```

Finally type `core-haskell` to invoke programme.

# Usage
`core-haskell` or `core-haskell filename`

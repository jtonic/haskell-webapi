
# Haskell common commands

## Stack commands

  - `stack build --fast && stack exec -- haskell-webapi`
  - `stack test`
  - `stack haddock`
  - debug and get documentation with file watch
    `stack build --fast --haddock-deps --file-watch`

## Formatters config

My favorite formatter is `Stylish Haskell`.
See how to configure Haskell formatters https://mmhaskell.com/blog/2018/8/6/keeping-it-clean-haskell-code-formatters

- Install with stylish-haskell

  `stack install stylish-haskell`

- Make the stylish-haskell the default formatter in your editor of choice (mine is vs code)

- generate the default format configuration

  `stylish-haskell --defaults > .stylish-haskell.yaml`

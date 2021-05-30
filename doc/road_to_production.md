# Haskell road to PRODUCTION

- [ ] Setup (visual studio code):
  - [x] [Stack](https://docs.haskellstack.org/en/stable/README/)
  - [ ] Proxy
  - [ ] Private Repo (Artifactory)
- [ ] Logging
- [ ] Data Modeling with Haskell. Data structures
  - https://haskell-at-work.com/episodes/2018-01-19-domain-modelling-with-haskell-data-structures.html
- [ ] Testing
  - [ ] [QuickCheck](https://hackage.haskell.org/package/QuickCheck)
- [ ] Work with (large) Files
- [ ] RESTful API
  - [ ] Yesod: [here](https://hackage.haskell.org/package/yesod) and [here](https://www.yesodweb.com)
  - [x] [Servant] (https://docs.servant.dev/en/stable/tutorial/ApiType.html)
  - [ ] [Scotty] (https://hackage.haskell.org/package/scotty)
- [ ] HTTP client
  - [ ] [Simple http-client](https://github.com/snoyberg/http-client/blob/master/TUTORIAL.md)
  - [ ] [http-tls client](<[http-client-tls](https://www.stackage.org/package/http-client-tls)>)
- [ ] CLI
  - [x] [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)
- [ ] work with JSON
  - [x] [Aeson](https://www.stackage.org/lts-16.22/package/aeson-1.4.7.1)
- [ ] Web Application
  - [ ] [yesodweb](https://www.yesodweb.com/book/widgets)
- [ ] Persistence
  - [ ] [esqueleto](https://hackage.haskell.org/package/esqueleto)
  - [x] sqlite-simple
- [ ] Common Effects
  - [ ] Random
  - [ ] UUID
  - [ ] Scheduler
- [ ] DSL
  - [ ] [Polysemy](polysemy is a library for writing high-power, low-boilerplate domain specific languages). More about polysemy
    - [here](https://haskell-explained.gitlab.io/blog/posts/2019/07/28/polysemy-is-cool-part-1/index.html)
    - [here](https://youtu.be/idU7GdlfP9Q?t=1394)
- [ ] Docker

## Low priority

- [ ] Developer experience
  - [ ] vs code setup
  - [ ] Debugging (vs code)
- [ ] work with MySQL
- [ ] work with XML
- [ ] work with NoSQL
- [ ] pipeline (Microsoft Azure DevOps, Gitlab actions)
- [ ] Kubernetes
- [ ] Performance
- [ ] Reactive Programming ([Reflex](https://hackage.haskell.org/package/reflex))
- [ ] Data Streaming ([conduit](https://github.com/snoyberg/conduit#readme))
- [ ] Versioning
  - [ ] Different project with different toolchain versions (stack, cabal, ghc, ghci, ghcid)
- [ ] Accessing remote data concurrently
  - [ ] [Haxl](http://hackage.haskell.org/package/haxl)

## Releasing internal/external resources
- [ ] [bracket](https://wiki.haskell.org/Bracket_pattern)

## Exception handling
  - [try](https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Exception.html#v:try)
  - [catch](https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Exception.html#g:5)
  - [throw](https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Exception.html#g:2)

## Popular effects

- [ ] logging
- [ ] random numbers
  - import System.Random -> [link](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-warnings.html)
- [ ] UUID
- [ ] read/write to file
- [ ] read/write to console
- [ ] current date/time
- [ ] metrics
- [ ] tracing

## Data Structures

{-# START_FILE doc/monadic functions.md #-}
# Monadic functions

- __import Control.Monad__

## Semigroup

- __(<>)__

  ```Haskell
  [1..3] <> [4,5]         -- [1,2,3,4,5]
  Just [1] <> Just [2]    -- Just [1,2]
  ```

## Monoid

- __mempty__

  ```Haskell
  [1,2,3] <> mempty` -- [1,2,3]
  ```

## Functor
---

- __fmap__

  ```Haskell
  fmap (+1) [1,2,3] -- [2,3,4]
  ```

- __(<$>)__ infix synonym for fmap

  ```Haskell
  (+1) <$> pure 1  -- 2
  ```

- __(<$)__

  ```Haskell
  True <$ Just False -- Just True
  10 <$ pure 1       -- 10
  ```

## Applicative
---

- __pure__ or __return__

  ```Haskell
  pure 10
  ```

- __(>>=)__

  ```Haskell
  Just 1 >>= \a -> Just 2 -- Just 2
  [1,2] >>= \a -> [a,a]   -- [1,1,2,2]
  ```

- __(=<<)__

  ```Haskell
  (take 2 . repeat) =<< [1,2]  - [1,1,2,2]
  ```

- __<*>__ Sequential application

  ```Haskell
  pure (+1) <*> pure (2) -- pure 3
  ```

- __liftA__

  ```Haskell
  liftA (+1) (pure 1) -- 2
  liftA (+1) [1,2,3]  -- [2,3,4]
  ```

## Monadic functions
---

- __(>>)__ execute 2 actions and discard the result of the first one

  ```Haskell
  getName msg = putStrLn msg >> getLine
  ```

- __forM (forM_)__

  ```Haskell
  forM_ (Just 1) print -- 1
  ```

- __mapM (mapM_)__

  ```Haskell
  mapM_ print (Just 1) -- 1
  ```

- __replicateM (replicateM_)__

  ```Haskell
  λ > replicateM_ 2 $ print "hello"
  "hello"
  "hello"
  ```

- __when__

  ```Haskell
  λ > debug = True
  λ > when debug $ putStrLn "Debug"
  Debug
  ```
- __unless__

  ```Haskell
  λ > debug = True
  λ > unless debug $ putStrLn "Debug" -- nothing printed
  ```

- __liftM__

  ```Haskell
  liftM (+1) $ Just 10 -- Just 11
  ```

- __liftM2__

  ```Haskell
  liftM2 (+) (Just 10) (Just 1) -- Just 11
  ```

- __filterM__

  ```Haskell
  keep :: [Int] -> IO [Int]
  keep = filterM doKeep
      where
          doKeep :: Int -> IO Bool
          doKeep x
            |   x >= 0 = pure True
            | otherwise = pure False
  ```

- __FoldM__

  ```Haskell
  sum'' :: [Int] -> IO Int
  sum'' = foldM (\a b -> pure (a + b)) 0
  ```
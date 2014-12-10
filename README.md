# Solar Australia
A competition to find the best places to place solar panels in Australia.
Includes a server for ranking solutions.

## Installing
Currently there are a few too many steps here, but it's being worked on.
The following steps assume a UNIX-y platform.
But assuming you can get Haskell and Cabal up and running it will work anywhere.


1. Install haskell-platform
  
  ```
  apt-get install haskell-platform
  ```

2. Get the latest version of cabal
  
  ```
  cabal install cabal-install
  ln ~/.cabal/bin/cabal /usr/bin/cabal -f
  ```

3. Download this repo
  
  ```
  git clone https://github.com/kennib/solar-australia.git
  ```

4. Install, build and then run the server
  
  ```
  cd solar-australia/haskell
  make build
  make run
  ```

5. Visit in your browser (make take a few seconds for the server to get started) at `localhost:3000`

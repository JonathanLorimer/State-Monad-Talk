---
author: "Jonathan Lorimer"
title: "λ State Monad from First Principles λ"
patat:
    wrap: true
    incrementalLists: true
    theme:
        header: [bold, dullYellow]
        bulletList: [dullWhite, dullWhite]
        emph: [bold]
        strong: [bold]
        code: [dullRed, onDullBlack, bold]
        codeBlock: [dullWhite, onDullBlack ]
        syntaxHighlighting:
          string: [dullGreen]
          char: [dullGreen]
          specialChar: [dullCyan]
          verbatimString: [dullGreen]
---

# λ State Monad from First Principles λ

## What are the core components of Mutable State?

- Access
- Mutation

# Access

## An example from the wild wild west

```javascript
// thermostats.js
export function getThermostat(thermostatId) {
  return fetch(`${this.host}:${this.port}/thermostats/${thermostatId}`)
}

// buildings.js
export function getBuilding(buildingId) {
  return fetch(`${this.host}:${this.port}/buildings/${buildingId}`)
}

// server.js
import { getThermostats } from "thermostats.js"
import { getBuildings } from "buildings.js"

class ServerProxyFactoryService {
  constructor(host, port){
    this.host = host
    this.port = port
    this.getBuildings = getBuildings.bind(this)
    this.getThermostats = getThermsotats.bind(this)
  }
}

const devServer = new ServerProxyFactoryService("localhost", 3008)
const prodServer = new ServerProxyFactoryService(AWS_IP_ADDR, AWS_HOST)
```

## Haskell translation

```haskell
getThermostat :: String -> IO (Response ByteString)
getThermostat tstatId =
  get $ this.host ++ ":" ++ this.port "/thermostats/" ++ tstatId

getBuilding :: String -> IO (Response ByteString)
getBuilding buildingId =
  get $ this.host ++ ":" ++ this.port "/buildings/" ++ buildingId
```

- We need to get a hold of host and port

## A naive solution

```haskell
getThermostat :: String -> String -> String -> IO (Response ByteString)
getThermostat host port tstatId =
  get $ host ++ ":" ++ port "/thermostats/" ++ tstatId

getBuilding :: String -> String -> String -> IO (Response ByteString)
getBuilding host port buildingId =
  get $ host ++ ":" ++ port "/buildings/" ++ buildingId
```
- However, this doesn't compose very well

```haskell
thermostatIdFromBuilding :: IO (Response ByteString) -> String

getThermostats :: String -> String -> String -> IO (Response ByteString)
getThermostats host port buildingId =
  getThermostat host port . thermostatIdFromBuilding
  $ getBuilding host port buildingId
```
- Problem: We have to thread this data through our entire application

## A more robust solution - Reader

```haskell
newtype Reader r a = Reader { runReader :: r -> a }
```
- Let's write the instances

# Mutation

## Back to the wild west

```javascript
// thermostats.js
export function async getThermostat(thermostatId) {
    const res = await fetch(
      `${this.host}:${this.port}/thermostats/${thermostatId}`
      )
    this.log = `${this.log}\n[${new Date()}] GET: /thermostats/${thermostatId}`
    return res
}

// buildings.js
export function async getBuilding(buildingId) {
    const res = await fetch(
      `${this.host}:${this.port}/buildings/${buildingId}`
      )
    this.log = `${this.log}\n[${new Date()}] GET: /buildings/${buildingId}`
    return res
}

// server.js
import { getThermostats } from "thermostats.js"
import { getBuildings } from "buildings.js"

class ServerProxyFactoryService {
  constructor(host, port){
    this.host = host
    this.port = port
    this.log = ""
    this.getBuildings = getBuildings.bind(this)
    this.getThermostats = getThermsotats.bind(this)
  }
}
```

## How can we maintain purity with 'Mutability'

- For each function we return the `value` and the `mutation` as a single value
```haskell
functionWithState :: a -> (w, b)

newtype Writer w a = Writer { runWriter :: (w, a) }
```

- Let's write some instances

# Putting it all together

## Accessing and Mutating
```haskell
newtype State s a = State { runState :: s -> (a, s) }
```
- Takes an initial state
- Returns a value and the new state
- More instances!

## Memoization in JS - A Final Example

```javascript
class MemoFib {
  #calls = {}
  run(index) {
    if (index === 0) return 0
    if (index === 1) return 1

    if (this.#calls[index]) {
      return this.#calls[index]
    } else {
      const a = this.run(index - 1)
      const b = this.run(index - 2)
      this.#calls[index - 1] = a
      this.#calls[index - 2] = b
      return a + b
    }
  }
}
```

- Let's try and implement this in haskell

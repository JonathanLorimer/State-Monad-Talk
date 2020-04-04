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

## Back to the wild west


```javascript
// thermostats.js
export function async getThermostat(thermostatId) {
    const res = await fetch(`${this.host}:${this.port}/thermostats/${thermostatId}`)
    this.log = `${this.log}\n[${new Date()}] GET: /thermostats/${thermostatId}`
    return res
}

// buildings.js
export function async getBuilding(buildingId) {
    const res = await fetch(`${this.host}:${this.port}/buildings/${buildingId}`)
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
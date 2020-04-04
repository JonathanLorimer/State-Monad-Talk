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

const naiveFib = (index) => {
  if (index === 0) return 0
  if (index === 1) return 1
  return naiveFib(index - 1) + naiveFib(index - 2)
}

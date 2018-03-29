export class FatalException extends Error {
  constructor(public message: string) {
    super()
  }
}

Contributing
------------

1. Fork the project and clone your fork.

2. Checkout the development branch with: `git checkout development`

3. Run `stack init` to setup the project's stack environment.

4. Create a feature branch with: `git checkout -b <branch>`

5. Add your functionality:

  a. Put it in the appropriate place under the `./src` folder.

  b. Add tests for the functionality in appropriate files under the `./test` folder.

  c. Import and run the tests inside the [`test/Main.hs`](test/Main.hs) testing entry point.

  d. Fill out the test coverage as best you can.

6. Run `stack test` to run the tests.

7. Push your changes: `git push origin <branch>`

8. Open a pull request.

# How to contribute

Contributions are welcome!

* Please file bug reports and feature requests to
  https://github.com/ilmoraunio/graphqlfmt-clj-experiment/issues
* For small changes, such as bug fixes or documentation changes, feel free to
  send a pull request
* If you want to make a big change or implement a big new feature, please open
  an issue to discuss it first

## Environment setup

1. Clone this git repository
2. Have [Clojure installed](https://clojure.org/guides/getting_started)

## Making changes

* Fork the repository on Github
* Create a topic branch from where you want to base your work (typically, the
  main branch)
* If you add a new GraphQL format use case, please add a test under the 
  `test-resources/graphql` directory
  * Files ending in `.graphql` in this directory are automatically run as a
    test (with the file as both input and as expected output)
  * You can also submit a different input and output using the `.input.graphql`
    and `.expected.graphql` filetype extensions, respectively
* Also consider whether you need to add a test to
  [core_test.clj](https://github.com/ilmoraunio/graphqlfmt-clj-experiment/blob/main/test/graphqlfmt/core_test.clj),
  as well
* Push your code to your fork of the repository
* Make a Pull Request

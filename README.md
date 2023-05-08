# CIS 194 homework assignment

Resources and solutions for the [CIS 194: Introduction to Haskell](https://www.cis.upenn.edu/~cis1940/spring13/) course by [Brent Yorgey](http://www.cis.upenn.edu/~byorgey/).

## Table of Contents

- [About](#about)
- [Getting Started](#getting-started)
- [Usage](#usage)

## About

This monorepo contains two projects:

- `resources` - Unmodified (functionally-speaking) copy of the resources available as part of the assignments. Check the haddock about the `Provided` module for more info
- `solutions` - a collection of solutions for the homework assignments, done by yours truly. Take them with a grain of salt

## Getting Started

You need stack to interact with the projects here. [Check out how to install it](https://docs.haskellstack.org/en/stable/#how-to-install-stack) on your OS of preference.

## Usage

The `resources` project has an up-to-date material to solve the assignments. For instance the `Size` type is now an instance of `Semigroup` due to recent Haskell changes on the `Monoid` instance, which makes the original material fail to compile.

You can build everything with `stack build` or `make build`.

Both projects have documentation. You can build it with either `stack haddock` or `make docs`.

There's extensive tests, with over 90% of coverage. Use `stack test` or `make test` to run it all; there's also custom Make targets for testing a single project, such as `make test-solutions` and a `make coverage` to generate the HPC report.

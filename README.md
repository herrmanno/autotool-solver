# Autotool-Solution-Helper

[![asciicast](https://asciinema.org/a/w4pZWKj9jVQNm05mvsGmk33W6.png)](https://asciinema.org/a/w4pZWKj9jVQNm05mvsGmk33W6)

Helper tool for solving arbritary autotool tasks of the course 'Modellierung WS 20' by brute force.

This tool is meant to be used *only* as a help if one just can't find a solution to a given
task; sometims it's easier to understand a problem by lookuing at the solution ;)

This tool is *not meant* to be used for cheating, solving highscore tasks or similiar intentions.

## usage

```
USAGE
  run task:              <task> <task description file>
  show task types:       tasks
  show task description: help <task>
  show usage:            help
  show version:          version
  show license:          license
```

### show available tasks

```
autotool-helper tasks
```

### show description for a specific task

```
autotool-helper help <task type>
```

### run task

```
autotool-helper <task type> <task description file>
```

Example task description files can be found at [examples](./examples) directory.


## build
The final executables are built and tested by [stack](stack). To build the final executable run

```
stack build
stack install
```

Afterwards the executable should be placed inside stack's local binary directory and is accessible
as 
```
autotool-helper
```
## test
```
stack test
```

[stack]: https://docs.haskellstack.org/en/stable/README/

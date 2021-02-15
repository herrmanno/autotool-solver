# Autotool-Solution-Helper

```
  ____             _         ______                                       
 |  _ \           | |       |  ____|                                      
 | |_) |_ __ _   _| |_ ___  | |__ ___  _ __ ___ ___                       
 |  _ <| '__| | | | __/ _ \ |  __/ _ \| '__/ __/ _ \                      
 | |_) | |  | |_| | ||  __/ | | | (_) | | | (_|  __/                      
 |____/|_|   \__,_|\__\___| |_|  \___/|_|  \___\___|                      
                _        _              _   _    _      _                 
     /\        | |      | |            | | | |  | |    | |                
    /  \  _   _| |_ ___ | |_ ___   ___ | | | |__| | ___| |_ __   ___ _ __ 
   / /\ \| | | | __/ _ \| __/ _ \ / _ \| | |  __  |/ _ \ | '_ \ / _ \ '__|
  / ____ \ |_| | || (_) | || (_) | (_) | | | |  | |  __/ | |_) |  __/ |   
 /_/    \_\__,_|\__\___/ \__\___/ \___/|_| |_|  |_|\___|_| .__/ \___|_|   
                                                         | |              
```

Helper tool for solving arbritary autotool tasks of the course 'Modellierung WS 20' by brute force.

This tool is meant to be used *only* as a help if one just can't find a solution to a given
task; sometims it's easier to understand a problem by lookuing at the solution ;)

This tool is *not meant* to be used for cheating, solving highscore tasks or similiar intentions.

## usage

### show available tasks

```
autotool-helper help
```

### show description for a specific task

```
autotool-helper help <tasktype>
```

### run task

```
autotool-helper <tasktype> <path-to-task-description-file>
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
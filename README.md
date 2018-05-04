# TaskPad

Tool for task management in Haskell

## Generate task file

example:

```
$ cat 20180504.yaml
memo: []
tasks:
  1:
    done: true
    children: []
    name: hello
  4:
    done: false
    children: []
    name: てすと
  2:
    done: true
    children: []
    name: world
  3:
    done: false
    children: []
    name: hahaha
date: '20180504'
```

## CLI

```
$ taskpad --help
taskpad - operate daily tasks

Usage: taskpad [-v|--verbose] [-d|--date DATE] COMMAND [--version]

Available options:
  -v,--verbose             Enable verbose mode: verbosity level "debug"
  -d,--date DATE           Task's date
  --version                Show version
  -h,--help                Show this help text

Available commands:
  new                      Create a new task file. Note: if don't use --date
                           option then use today's date.
  add                      Add Task
  done                     Done Task
  tasks                    Show Tasks
```

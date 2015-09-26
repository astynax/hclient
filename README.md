## hCLIent

### Installation

```shell
$ git clone <repo>
$ cd hclient
$ stack install
...
$ hclient --help
```

*Prerequisites:*

GUI (but not TUI!) requires *Tk*. Tk can be installed with command:

```shell
$ sudo apt-get install tk
```

Or with the some other way, native for your system.

### Usage

Simpliest variant of usage:

```shell
$ hclient ls
```

Argument substitution (like ``xargs``):

```shell
$ hclient -I {} "find -iname {}"
```

Text UI (TUI) also available!

```shell
$ hclient -T echo
> hello
hello
> ping
ping
> ^D
Bye!
$
```


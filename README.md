# The SMASH Shell

## What Works

### Builtins

- `cd`
- `echo`
- `exit`
- `pwd`

### Redirection

```
cmd  > file
cmd 2> file
cmd &> file

cmd  >> file
cmd 2>> file
cmd &>> file

meansd < numbers
```

## Simplification / Linter

### Merge Argument Parts

```
# from
echo ${HOME}/foo'$bar_'"baz"

# to (keep slash)
echo ${HOME}/'foo$bar_baz'

# or (don't keep slash)
echo ${HOME}'/foo$bar_baz'

## maybe separator list that aren't merged
```

## variable filters like jinja

instead of:

```
echo $foo | ??? | md5
```

use this (looks weird though in shell):

```
{{ foo | default('42') | md5 }}
```

maybe one of:

```
${  foo | default('42') | md5  }
$(( foo | default('42') | md5 ))
```

## Thanks

These were some very helpful posts.

- [Write a Shell in C](https://brennan.io/2015/01/16/write-a-shell-in-c/), Stephen Brennan
- [How bash redirection works under the hood](https://brandonwamboldt.ca/how-bash-redirection-works-under-the-hood-1512/), Brandon Wamboldt
- [Understanding how Linux creates processes](https://brandonwamboldt.ca/how-linux-creates-processes-1528/), Brandon Wamboldt
- [How Linux pipes work under the hood](https://brandonwamboldt.ca/how-linux-pipes-work-under-the-hood-1518/), Brandon Wamboldt

There are a number of corner cases to consider when dealing with Docker,
multiple processes, and signals. Probably the most famous post on this matter
is [from the Phusion
blog](https://blog.phusion.nl/2015/01/20/docker-and-the-pid-1-zombie-reaping-problem/).
Here, we'll see some examples of how to see these problems first hand, and one
way to work around it.

The Phusion blog post recommends using their
[baseimage-docker](http://phusion.github.io/baseimage-docker/). We ran into
problems with Phusion's usage of syslog-ng, in particular with it creating
unkillable processes pegged at 100% CPU usage. We're still investigating, but
in the meanwhile we created the [pid1 Haskell
package](https://github.com/fpco/pid1#readme) together with a simple [fpco/pid1
Docker image](https://hub.docker.com/r/fpco/pid1/).

__NOTE__ The primary reason we wrote our own implementation in Haskell was to
be able to embed it within [the Stack build tool](http://haskellstack.org).
There are other lightweight init processes already available, such as
[dumb-init](https://engineeringblog.yelp.com/2016/01/dumb-init-an-init-for-docker.html).
I've also [blogged about using
dumb-init](https://www.fpcomplete.com/blog/2016/08/bitrot-free-scripts). While
this post uses `pid1`, there's nothing specific to it versus other init
processes.

## Playing with entrypoints

The default entrypoint for the ubuntu Docker image is a null entrypoint,
meaning that the provided command will be run directly without any wrapping.
We're going to simulate that experience by using `/usr/bin/env` as an
entrypoint, since [switching entrypoint back to null isn't yet supported in
released Docker](https://github.com/docker/docker/issues/23498). When you run
`/usr/bin/env foo bar baz`, the `env` process will `exec` the `foo` command,
making `foo` the new PID 1, which for our purposes gives it the same behavior
as a null entrypoint.

## Sending TERM signal to process

We'll start with our
[sigterm.hs](https://gist.github.com/snoyberg/caf8aeba588aaeecfa6ccf4c26f9f870#file-sigterm-hs)
program, which runs `ps` (we'll see why soon), then sends itself a `SIGTERM`
and then loops forever. On a Unix system, the default process behavior when
receiving a `SIGTERM` is to exit. Therefore, we'd expect that our process will
just exit when run. Let's see:

```
 docker run --rm --entrypoint /usr/bin/env snoyberg/docker-testing sigterm
  PID TTY          TIME CMD
    1 ?        00:00:00 sigterm
    9 ?        00:00:00 ps
Still alive!
Still alive!
Still alive!
^C
```

The process ignored the `SIGTERM` and kept running, until I hit Ctrl-C (we'll
see what _that_ does later). Another feature in the sigterm code base, though,
is that if you give it the command line argument `install-handler`, it will
explicitly install a SIGTERM handler which will kill the process. Perhaps
surprisingly, this has a significant impact on our application:

```
$ docker run --rm --entrypoint /usr/bin/env snoyberg/docker-testing sigterm install-handler
  PID TTY          TIME CMD
    1 ?        00:00:00 sigterm
    8 ?        00:00:00 ps
Still alive!
```

The reason for this is some Linux kernel magic: the kernel treats a process
with PID 1 specially, and does _not_, by default, kill the process when
receiving the `SIGTERM` or `SIGINT` signals. This can be very surprising
behavior. For a simpler example, try running the following commands in two
different terminals:

```
$ docker run --rm --name sleeper ubuntu:16.04 sleep 100
$ docker kill -s TERM sleeper
```

Notice how the `docker run` command does not exit, and if you check your `ps
aux` output, you'll see that the process is still running. That's because the
`sleep` process was not designed to be PID 1, and does not install a special
signal handler. To work around this problem, you've got two choices:

1. Ensure every command you run from `docker run` has explicit handling of
   `SIGTERM`.
2. Make sure the command you run isn't PID 1, but instead use a process that is
   designed to handle `SIGTERM` correctly.

Let's see how the `sigterm` program works with our `/sbin/pid1` entrypoint:

```
$ docker run --rm --entrypoint /sbin/pid1 snoyberg/docker-testing sigterm
  PID TTY          TIME CMD
    1 ?        00:00:00 pid1
    8 ?        00:00:00 sigterm
   12 ?        00:00:00 ps
```

The program exits immediately, as we'd like. But look at the `ps` output: our
first process is now `pid1` instead of `sigterm`. Since `sigerm` is being
launched as a different PID (8 in this case), the special casing from the Linux
kernel does not come into play, and default `SIGTERM` handling is active. To
step through exactly what happens in our case:

1. Our container is created, and the command `/usr/sbin/pid1 sigterm` is run
   inside of it.
2. `pid1` starts as PID-1, does its business, and then `fork`/`exec`s the
   `sigterm` executable.
3. `sigterm` raises the `SIGTERM` signal to itself, causing it to die.
4. `pid1` sees that its child died from SIGTERM (== signal 15) and exits with
   exit code 143 (== 128 + 15).
5. Since our PID1 is dead, our container dies too.

This isn't just some magic with `sigterm`, you can do the same thing with `sleep`:

```
$ docker run --rm --name sleeper fpco/pid1 sleep 100
$ docker kill -s TERM sleeper
```

Unlike with the `ubuntu` image, this will kill the container immediately, due
to the `/sbin/pid1` entrypoint used by `fpco/pid1`.

## Reaping orphans

Suppose you have process A, which `fork`/`exec`s process B. When process B
dies, process A must call `waitpid` to get its exit status from the kernel, and
until it does so, process B will be dead but with an entry in the system
process table. This is known as being a zombie.

But what happens if process B outlives process A? In this case, process B is
known as an orphan, and needs to be adopted by the init process, aka PID1. It
is the init process's job to reap orphans so they do not remain as zombies.

The [orphans.hs
program](https://gist.github.com/snoyberg/caf8aeba588aaeecfa6ccf4c26f9f870#file-orphans-hs)
will:

* Spawn a child process, and then loop forever calling `ps`
* In the child process: run the `echo` command a few times, without calling
  `waitpid`, and then exit

As you can see, none of the processes involved will reap the zombie `echo`
processes. The output from the process confirms that we have, in fact, created
zombies:

```
$ docker run --rm --entrypoint /usr/bin/env snoyberg/docker-testing orphans                                                
1
2
3
4
Still alive!
  PID TTY          TIME CMD
    1 ?        00:00:00 orphans
    8 ?        00:00:00 orphans
   13 ?        00:00:00 echo <defunct>
   14 ?        00:00:00 echo <defunct>
   15 ?        00:00:00 echo <defunct>
   16 ?        00:00:00 echo <defunct>
   17 ?        00:00:00 ps
Still alive!
  PID TTY          TIME CMD
    1 ?        00:00:00 orphans
   13 ?        00:00:00 echo <defunct>
   14 ?        00:00:00 echo <defunct>
   15 ?        00:00:00 echo <defunct>
   16 ?        00:00:00 echo <defunct>
   18 ?        00:00:00 ps
Still alive!
```

And so on until we kill the container. That `<defunct>` indicates a zombie
process. The issue is that our PID 1, orphans, doesn't do reaping. As you
probably guessed, we can solve this by just using the `/sbin/pid1` entrypoint:

```
$ docker run --rm --entrypoint /sbin/pid1 snoyberg/docker-testing orphans
1
2
3
4
Still alive!
  PID TTY          TIME CMD
    1 ?        00:00:00 pid1
   10 ?        00:00:00 orphans
   14 ?        00:00:00 orphans
   19 ?        00:00:00 echo <defunct>
   20 ?        00:00:00 echo <defunct>
   21 ?        00:00:00 echo <defunct>
   22 ?        00:00:00 echo <defunct>
   23 ?        00:00:00 ps
Still alive!
  PID TTY          TIME CMD
    1 ?        00:00:00 pid1
   10 ?        00:00:00 orphans
   24 ?        00:00:00 ps
Still alive!
```

`pid1` now adopts the `echo` processes when the child `orphans` process dies,
and reaps accordingly.

## Surviving children

Let's try out something else: process A is the primary command for the Docker
container, and it spawns process B. Before process B exits, process A exits,
causing the Docker container to exit. In this case, the running process B will
be forcibly closed (see [this Stack Overflow question for
details](http://stackoverflow.com/questions/39739658/what-happens-to-other-processes-when-a-docker-containers-pid1-exits)).
We can see this with our [surviving.hs
program](https://gist.github.com/snoyberg/caf8aeba588aaeecfa6ccf4c26f9f870#file-surviving-hs):

```
$ docker run --rm --entrypoint /usr/bin/env snoyberg/docker-testing surviving
Parent sleeping
Child: 1
Child: 2
Child: 4
Child: 3
Child: 1
Child: 2
Child: 3
Child: 4
Parent exiting
```

Unfortunately this doesn't give our child processes a chance to do any cleanup.
Instead, we would rather send them a `SIGTERM`, and after a grace period send
them a `SIGKILL`. This is exactly what `pid1` does:

```
$ docker run --rm --entrypoint /sbin/pid1 snoyberg/docker-testing surviving                                                  
Parent sleeping
Child: 2
Child: 3
Child: 1
Child: 4
Child: 2
Child: 1
Child: 4
Child: 3
Parent exiting
Got a TERM
Got a TERM
Got a TERM
Got a TERM
```

## Signaling `docker run` vs PID1

When you run `sleep 60` and then hit Ctrl-C, the `sleep` process itself
receives a `SIGINT`. When you instead run `docker run --rm fpco/pid1 sleep 60`
and hit Ctrl-C, you may think that the same thing is happening. However, in
reality, it's not at all the same. Your `docker run` call creates a `docker
run` process, which and that process sends a command to the Docker daemon on
your machine, and that daemon creates the actual `sleep` process (inside a
container). When you hit Ctrl-C on your terminal, you're sending `SIGINT` to
`docker run`, which is in fact sending a command to the Docker daemon, which in
turn sends a `SIGINT` to your `sleep` process.

Want proof? Try out the following:

```
$ docker run --rm fpco/pid1 sleep 60&
[1] 417
$ kill -KILL $!
$ docker ps
CONTAINER ID        IMAGE                       COMMAND                  CREATED             STATUS              PORTS               NAMES
69fbc70e95e2        fpco/pid1                   "/sbin/pid1 sleep 60"    11 seconds ago      Up 11 seconds                           hopeful_mayer
[1]+  Killed                  docker run --rm fpco/pid1 sleep 60
```

In this case, we sent a `SIGKILL` to the `docker run` command. Unlike `SIGINT`
or `SIGTERM`, and `SIGKILL` cannot be handled, and therefore `docker run` is
unable to delegate signal handling to a different process. As a result, the
`docker run` command itself dies, but the `sleep` process (and its container)
continue running.

Some takeaways from this:

* Make sure you use something like `pid1` so that your `SIGINT` or `SIGTERM` to
  the `docker run` process actually get your container to reliably shut down
* If you _must_ send a `SIGKILL` to your process, use the `docker kill` command
  instead

## Alternative to entrypoint

We've used `--entrypoint /sbin/pid1` a lot here. In fact, each usage of that
has been superfluous, since the `fpco/pid1` and `snoyberg/docker-testing`
images both use `/sbin/pid1` as their default entrypoint anyway. I included it
for explicitness. To prove it to you:

```
$ docker run --rm fpco/pid1 sleep 60
^C$
```

But if you don't want to muck with entrypoints, you can always just include
`/sbin/pid1` at the beginning of your command, e.g.:

```
$ docker run --rm --entrypoint /usr/bin/env fpco/pid1 /sbin/pid1 sleep 60
^C$
```

And if you have your own Docker image and you'd just like to include the `pid1`
executable, you can download it from the [Github releases
page](https://github.com/fpco/pid1/releases).

## Dockerfiles, command vs exec form

You may be tempted to put something like `ENTRYPOINT /sbin/pid1` in your
Dockerfile. Let's see why that won't work:

```
$ cat Dockerfile
FROM fpco/pid1
ENTRYPOINT /sbin/pid1
$ docker build --tag test .
Sending build context to Docker daemon 2.048 kB
Step 1 : FROM fpco/pid1
 ---> aef1f7b702b9
Step 2 : ENTRYPOINT /sbin/pid1
 ---> Using cache
 ---> f875b43a9e40
Successfully built f875b43a9e40
$ docker run --rm test ps
pid1: No arguments provided
```

The issue here is that we specified /sbin/pid1 in what Docker calls _command
form_. This is just a raw string which is interpreted by the shell. It is
unable to be passed an additional command (like `ps`), and therefore `pid1`
itself complains that it hasn't been told what to run. The correct way to
specify your entrypoint is `ENTRYPOINT ["/sbin/pid1"]`, e.g.:

```
$ cat Dockerfile                                                                                                        
FROM fpco/pid1
ENTRYPOINT ["/sbin/pid1"]
$ docker build --tag test .
Sending build context to Docker daemon 2.048 kB
Step 1 : FROM fpco/pid1
 ---> aef1f7b702b9
Step 2 : ENTRYPOINT /sbin/pid1
 ---> Running in ba0fa8c5bd41
 ---> 4835dec4aae6
Removing intermediate container ba0fa8c5bd41
Successfully built 4835dec4aae6
$ docker run --rm test ps
  PID TTY          TIME CMD
    1 ?        00:00:00 pid1
    8 ?        00:00:00 ps
```

Generally speaking, you should stick with command form in your Dockerfiles at
all times. It is explicit about whitespace handling, and avoids the need to use
a shell as an interpreter.

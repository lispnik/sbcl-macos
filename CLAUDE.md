# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

A Lisp Listener in a native Cocoa window, for SBCL on macOS: you type forms into
an `NSTextView` and the values, the output and the debugger come back in the same
transcript. Every Objective-C class in it is defined from Lisp through
[lispnik/objc](https://github.com/lispnik/objc); the `.app` bundle is built by
[lispnik/asdf-macos-app](https://github.com/lispnik/asdf-macos-app). One package,
`LISP-LISTENER`; `OBJC` is deliberately not `:USE`d, because it exports `INVOKE`,
`RELEASE`, `RETAIN` and `DESCRIPTION` and this is the program where an accidental
capture of one of those is hardest to see.

**It needs an SBCL built `--with-sb-safepoint`.** Darwin refuses to
`pthread_kill` a libdispatch workqueue thread, so `stop_the_world` calls `lose()`
and the process dies with no condition and no backtrace. AppKit reaches
libdispatch on its own account. The program starts on a stock build and says so
in the transcript rather than refusing.

## Build & test

```sh
ocicl setup         # once per machine
make deps           # ocicl install -- restores ./ocicl/ from ocicl.csv
make check          # all three off-macOS checks, about three seconds
make run            # a listener from a REPL, on thread 1
make app            # => build/Lisp Listener.app
```

The three checks run **anywhere, Linux included**, and that is the point — this
system cannot be *loaded* off macOS at all, because objc opens libobjc as it
initialises.

| target | question it answers |
|---|---|
| `make syntax-check` | does it parse? Reads every form with `*read-suppress*`. |
| `make compile-check` | does it compile? Builds `src/` against `tools/stubs/`. |
| `make test` | **does the listener work?** |

`make check` needs no dependencies at all — it runs against the stubs, so it
works in a fresh clone before `make deps`.

### The headless test is the unusual one

`tools/headless-test.lisp` runs a **real listener** on the stubs — real thread,
real gray streams, the real reader, evaluator, printer and debugger — and drives
it through a session, an error, `use-value`, `store-value`, `y-or-n-p` and an
abort, reading the transcript back and asserting on it. Only Cocoa is hollow.

It also runs **two listeners at once** (`case-two-listeners`), which is the half
of New Listener that is not the window: two threads, two queues, two
transcripts, and the registry that decides which is which.

Two things make it possible, and both are load-bearing:

- `schedule-flush` (`src/streams.lisp`) declines to do anything while
  `*main-thread-target*` is NIL, so output piles up in the stream's own segments
  where the test reads it.
- `tools/stubs/stubs.lisp`'s `bordeaux-threads` is **not** a stub — it delegates
  to `sb-thread`. Everything else in that file is a name with no behaviour.

There is no CLI selector for one case; edit the `dolist` at the foot of the file,
or call one `case-*` function from a REPL after loading the stubs and `src/`.

**Build a verification for a change here before reaching for CI.** This harness
found, in seconds, a bug that three rounds of CI screenshots had not.

## Architecture

Two threads per listener, and the split is the whole design.

**Thread 1** owns AppKit and ends in `-[NSApplication run]`. Every message to a
view, a window or the text storage happens there. **The listener thread** is an
ordinary SBCL thread running read-eval-print and touches only Lisp state. So a
form that loops forever never freezes the window, and ⌘. gets the prompt back.

They meet at two queues: characters main → listener (the listener's
`*standard-input*` blocks on `src/queue.lisp`), and closures listener → main,
drained by an IMP that `-performSelectorOnMainThread:withObject:waitUntilDone:modes:`
delivers (`src/main-thread.lisp`).

**There can be more than one, and New Listener (⌘N) opens one.** `*listeners*`
is the live set; `*listener*` names whichever listener the code running now
speaks for and is **bound, never read as "the" listener**: `define-listener-method`
binds it from the view the IMP arrived on, `start-listener-thread` binds it in
each thread, and the restarts controller — one per listener — binds it from its
own slot. Anything reached from a menu item asks `current-listener` instead,
which is the key window's. A plain function that will be called from more than
one place resolves from its own argument (see `submit-input`) rather than
trusting the ambient value; the queue behind `*main-thread-target*` is shared
and global, so that one target is deliberately just "a view that is still
alive", repointed on close and never cleared.

Because `read` simply blocks on an incomplete form, **there is no Lisp parser in
the view**: Return always submits, and if the form is not finished no new prompt
appears. That falls out of the design rather than being arranged.

`src/` loads `:serial t` and **the component order in `lisp-listener.asd` is
load-bearing**: `package main-thread queue listener view streams restarts repl
window screenshot app`. `tools/compile-check.lisp` carries the same list by hand;
a new file has to be added in both.

- `src/listener.lisp` — the `listener` struct, holding both halves. **Nothing in
  it may be filled in at load time**: a foreign pointer does not survive
  `save-lisp-and-die` and the bundle is a dumped core.
- `src/view.lisp` — the `LispListenerView` `NSTextView` subclass, the
  `define-listener-method` macro (every IMP wrapped in `handler-case`), the
  transcript primitives and the AppKit constants.
- `src/streams.lisp` — the gray streams, and the segment buffer that coalesces a
  thousand `write-char`s into one hop to the main thread.
- `src/restarts.lisp` — the LispWorks-style restarts panel: an `NSTableView` of
  whatever `compute-restarts` returned, plus Cancel and Invoke.
- `src/repl.lisp` — the listener thread, the debugger and the backtrace.
- `src/screenshot.lisp` — drives a real listener and photographs it; this is what
  produces `doc/screenshots/`, on a CI runner, on every push.

Two `.asd` files, and they must stay separate: `:defsystem-depends-on` is
resolved when a `.asd` is **read**, not when its system is built, so declaring
the bundle in `lisp-listener.asd` would make `asdf-macos-app` a hard requirement
for anyone who only wants to load the library.

## CI

`check.yml` runs the three checks on Linux in seconds. `macos.yml` builds SBCL
`--with-sb-safepoint` (cached, pinned to a tag), verifies the build really has
them, runs the self-test, builds and runs the bundle, proves the ocicl-only path,
and takes the screenshots — on **arm64 and Intel**. The Intel leg is not
box-ticking: a struct over sixteen bytes returns through `objc_msgSend_stret` on
x86-64 and through `x8` on arm64, and every `NSRange` and `NSRect` here crosses
that boundary.

`ocicl.csv` pins the whole closure by digest, `objc` and `asdf-macos-app`
included, so a fresh clone needs no sibling checkouts. CI nevertheless checks
both out and puts them **ahead of `ocicl/`** on `CL_SOURCE_REGISTRY`, so the
checkout shadows the published copy — this repository is meant to break when
objc's `master` breaks. ASDF takes the first match, so that ordering is the
entire mechanism.

## Things that are easy to get wrong

Each of these is a bug that actually happened here.

- **`run-listener` must not use a modal session.** It used
  `-[NSApplication runModalForWindow:]`, which blocks events to every OTHER
  window of the application — so New Listener opened a window you could see and
  not type in. The restarts panel escapes that only by being an `NSPanel`, which
  works during a modal session; an ordinary second window does not. It is
  `-[NSApplication run]` now, stopped by `stop-run-loop-soon` when the last
  window closes.

- **`-[NSApplication stop:]` needs an event behind it.** It raises a flag that
  `-run` tests after finishing the event in hand and then asking for the NEXT
  one. Closing the last window is very often the last event there is, so
  without `post-wakeup-event` the loop sits blocked with the flag set and the
  REPL never comes back.

- **`applicationShouldTerminateAfterLastWindowClosed:` must answer NIL in a REPL
  session.** `-terminate:` exits the process, and under `run-listener` that
  process is somebody's SBCL: answering T killed the REPL instead of returning
  to it, and did it before `run-listener`'s own unwinding had run. It keys off
  `*stop-run-loop-on-last-close*`, which is bound — soundly, because the IMP
  runs on thread 1 inside the `-run` that `run-listener` is blocked in.

- **Never clear `*main-thread-target*` while a listener thread may still
  write.** A closing listener is still unwinding and still printing, and with a
  NIL target each write signals inside the debugger hook, which aborts, which
  loops. It span 204 times in the space of one close. `retarget-main-thread`
  only ever repoints it; the old view stays a valid receiver because
  `-releasedWhenClosed` is off and closing deallocates nothing.

- **`unwind-protect` around `listener-loop`, never `handler-case`.** A handler for
  `error` established out there *handles* the condition, and a handled condition
  never reaches `invoke-debugger` — so `*debugger-hook*` does not run and an
  error in an evaluated form quietly restarts the listener. This function had
  exactly that shape; **the entire debugger was dead code and nothing said so**,
  until the first CI screenshot that reached it.

- **Bind both `cl:*debugger-hook*` and `sb-ext:*invoke-debugger-hook*`.** SBCL
  nulls the former before calling it, so a nested error inside the debugger
  reaches only the latter.

- **`fresh-line` on a two-way stream asks the INPUT half for its column.** So
  `listener-input-stream` needs a `stream-line-column` method although it
  implements no output protocol. Without it every `~&` on `*query-io*` signalled
  `no-applicable-method`, which took out `invoke-restart-interactively` entirely
  — `use-value` and `store-value` could not be used at all — and left `y-or-n-p`
  printing its question and never receiving the answer. It looks like dead code.
  It is not; `make test` has three cases on it.

- **Find the top-level restart by OBJECT, never by index.** On an unbound
  variable — the commonest error there is — SBCL puts `continue`, `use-value` and
  `store-value` in front of the listener's own `abort`, which sits at **index 3**.
  Index 0 there is `Retry using *FOO*`, which retries, and retries. `make test`
  and the macOS workflow both assert the index is not 0.

- **Escape needs two selectors, and the conventional one is not the one that
  works.** Escape is `-cancelOperation:` in most controls, but inside an
  `NSTextView` the standard key bindings send it to `-complete:`. Overriding only
  `-cancelOperation:` looks correct and does nothing.

- **Compute the restarts panel's labels on the LISTENER thread.** A restart's
  report may read the *current* thread rather than the one it was established on;
  SBCL's per-thread abort restart does exactly that, via
  `sb-thread:*current-thread*` at print time. Printed from thread 1 while laying
  out the panel it named the main thread and was quietly wrong.

- **Insert output AT `input-start`, not at the end.** Otherwise a `format` from a
  computation still running is spliced into the middle of the line being typed.

- **Reset the output stream's column after `read`.** The view already appended
  the newline the user pressed, but the stream last wrote the prompt and still
  believes it is nine columns in — so `fresh-line` emits a newline that is
  already on screen and every value gets a blank line above it.

- **`NSInteger` is `(:signed :long-long)`.** `'l'`/`'L'` are 32 bits even on
  LP64; `NSInteger` encodes as `'q'`.

- **An `NSRange` arrives as a CONS** `(location . length)`. Other Cocoa structs
  arrive as vectors.

- **An object returned from a Lisp method is the caller's to release.** The table
  delegate's row views are autoreleased for that reason — AppKit asks again on
  every redraw, and a +1 object there leaks one per row per repaint.

- **Name run-loop modes individually**, never `kCFRunLoopCommonModes`, when
  hopping to the main thread.

- **`macos-26-intel`, never `macos-13`.** The old free Intel image is retired and
  a job labelled with it is never assigned a runner: it queues indefinitely
  rather than failing, so the workflow never reports at all. This is the most
  expensive mistake available in the workflow, because it looks like a slow queue.

- **Two of the four screenshots can never be byte-compared.** `debugger.png` and
  `restarts.png` show a restart list, every restart list ends with SBCL's
  per-thread abort restart, and that prints a fresh `tid` on every run. Only
  `session.png` and `interrupt.png` are deterministic; comparing the other two
  fires a "refresh it" warning forever.

- **Write CI scratch files to `$RUNNER_TEMP`,** and screenshots too. Writing them
  under `doc/screenshots/` makes `test -s` pass against the *committed* files and
  the check proves nothing.

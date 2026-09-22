# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

A Lisp Listener in a native window: you type forms into a text view and the
values, the output and the debugger come back in the same transcript. **Two
front ends over one core**: AppKit (`NSTextView`) for SBCL on macOS, and UIKit
(`UITextView`) for ECL on iOS. Every Objective-C class in it is defined from Lisp through
[lispnik/objc](https://github.com/lispnik/objc); the Mac bundle is built by
[lispnik/asdf-macos-app](https://github.com/lispnik/asdf-macos-app) and the iOS
app by [lispnik/asdf-ios-app](https://github.com/lispnik/asdf-ios-app). One package,
`LISP-LISTENER`; `OBJC` is deliberately not `:USE`d, because it exports `INVOKE`,
`RELEASE`, `RETAIN` and `DESCRIPTION` and this is the program where an accidental
capture of one of those is hardest to see.

**On the Mac it needs an SBCL built `--with-sb-safepoint`.** Darwin refuses to
`pthread_kill` a libdispatch workqueue thread, so `stop_the_world` calls `lose()`
and the process dies with no condition and no backtrace. AppKit reaches
libdispatch on its own account. The program starts on a stock build and says so
in the transcript rather than refusing. (ECL never signals a thread to collect,
so on iOS the question does not arise.)

## Build & test

```sh
ocicl setup         # once per machine
make deps           # ocicl install -- restores ./ocicl/ from ocicl.csv
make check          # all three off-macOS checks; the listener runs on SBCL and ECL
make run            # a listener from a REPL, on thread 1
make app            # => build/Lisp Listener.app

make ios-toolchain  # once: asdf-ios-app builds the host and iOS ECLs (~10 min)
make ios            # => build/iphonesimulator/Lisp Listener.app
make run-ios        # build, install and launch in the booted simulator
```

The iOS targets run under **ECL**, not SBCL: asdf-ios-app is ECL code and
cross-compiles with the ECL `ios-toolchain` built. The iOS app has a self-test:
`SIMCTL_CHILD_LISP_LISTENER_SELF_TEST=6 xcrun simctl launch <device>
org.lispnik.lisp-listener` drives a session, Tab, an error, the restarts sheet
and Cancel, holding N seconds on the screens worth photographing, and writes
`selftest: PASS` to `Documents/console.log` in the app's data container.
Take `xcrun simctl io` screenshots **in the same shell command as the launch**:
anything slower misses the holds.

The three checks run **anywhere, Linux included**, and that is the point — this
system cannot be *loaded* off macOS at all, because objc opens libobjc as it
initialises.

| target | question it answers |
|---|---|
| `make syntax-check` | does it parse? Reads every form with `*read-suppress*`. |
| `make compile-check` | does it compile? Builds the core plus **each** front end against `tools/stubs/`, one process per front end. |
| `make test` | **does the listener work?** On SBCL, with the Mac front end. |
| `make test-ecl` | the same, on ECL with the iOS front end. `make check` runs it when `ecl` is on the PATH. |

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
  to `sb-thread`, or to `mp` on ECL. Everything else in that file is a name with
  no behaviour.

ECL establishes no `USE-VALUE` or `STORE-VALUE` around an unbound variable, so on
ECL the test reaches one through `cl-user::missing-value`, which establishes
SBCL's three restarts in SBCL's order and then signals a real `unbound-variable`.

There is no CLI selector for one case; edit the `dolist` at the foot of the file,
or call one `case-*` function from a REPL after loading the stubs and `src/`.

**Build a verification for a change here before reaching for CI.** This harness
found, in seconds, a bug that three rounds of CI screenshots had not.

## Architecture

Two threads per listener, and the split is the whole design.

**Thread 1** owns AppKit or UIKit; on the Mac it ends in `-[NSApplication run]`,
and on iOS asdf-ios-app's `UIApplicationMain` owns it and calls `ios-start`,
which must return. Every message to a
view, a window or the text storage happens there. **The listener thread** is an
ordinary Lisp thread running read-eval-print and touches only Lisp state. So a
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

### Core and front ends

Three systems in `lisp-listener.asd`, each `:serial t`, and **the component
order is load-bearing**:

- `lisp-listener/core` — `src/`: `package impl main-thread queue listener
  transcript completion streams restarts repl`. No toolkit; SBCL and ECL.
- `lisp-listener` — the core plus `src/macos/`: `view window restarts-panel
  screenshot app`. The name it always had.
- `lisp-listener/ios` — the core plus `src/ios/`: `view restarts-sheet app`.

`tools/compile-check.lisp` and `tools/headless-test.lisp` each carry the same
lists by hand; a new file has to be added in all three places.

**The seam is `src/impl.lisp`.** It is the only file in `src/` with `#+sbcl` or
`#+ecl` (debugger hook, backtrace, restart internals, exit, getenv). The one
exception is the Gray streams package, which `package.lisp` names with a local
nickname, `gray-streams`. `impl.lisp` also declaims what each front end must
define: `transcript-color`, `transcript-font`, `main-thread-run-loop-modes`,
`show-restarts-panel`, `hide-restarts-panel`, `restarts-panel-visible-p`,
`current-listener`, and the class `listener-text-view`, with the same slots on
both. The core only ever touches that class through `-textStorage`,
`-selectedRange`, `-scrollRangeToVisible:` and `-typingAttributes`, which
NSTextView and UITextView share.

- `src/impl.lisp` — the seam, above.
- `src/listener.lisp` — the `listener` struct, holding both halves. **Nothing in
  it may be filled in at load time**: a foreign pointer does not survive
  `save-lisp-and-die` and the bundle is a dumped core.
- `src/transcript.lisp` — the transcript primitives over either text view, and
  the `define-listener-method` macro (every IMP wrapped in `handler-case`).
- `src/completion.lisp` — symbol completion, from the listener's package, which
  `emit-prompt` publishes in the `listener-package` slot because thread 1 cannot
  see the thread's `*package*`. It also has `complete-at-caret`, the shell-style
  completion (insert, extend, or list) for a toolkit with no popup.
- `src/streams.lisp` — the gray streams, and the segment buffer that coalesces a
  thousand `write-char`s into one hop to the main thread.
- `src/restarts.lisp` — what the restarts panel does, on either platform: the
  titles, which restart Cancel means, and the hop to put them up and take them down.
- `src/repl.lisp` — the listener thread, the debugger and the backtrace.
- `src/macos/view.lisp` — `LispListenerView` over `NSTextView`: Return, the
  arrows, and Tab through NSTextView's own completion popup.
- `src/macos/restarts-panel.lisp` — the LispWorks-style `NSPanel`: an
  `NSTableView` of whatever `compute-restarts` returned, plus Cancel and Invoke.
- `src/macos/screenshot.lisp` — drives a real listener and photographs it; this is what
  produces `doc/screenshots/`, on a CI runner, on every push.
- `src/ios/view.lisp` — `LispListenerView` over `UITextView`: Return through the
  delegate, `UIKeyCommand`s for Tab, ↑, ↓, Esc, ⌘. and ⌘K, and a key bar with the
  same keys above the on-screen keyboard.
- `src/ios/restarts-sheet.lisp` — the restarts as a sheet: a `UIViewController`
  with a `UITableView` whose data source is the same `restarts-controller` the
  Mac's table uses, presented at `UISheetPresentationController`'s medium
  detent and draggable to full height.
- `src/ios/app.lisp` — `ios-start`, and the self-test.

`lisp-alien.png` is the icon's source art, and `res/` is what the two builders
take: `res/icon.png`, the alien inset in a rounded rectangle, which
asdf-macos-app turns into an `.icns`; and `res/LispListener.xcassets`, whose
icon is full-bleed, 1024x1024 and **without an alpha channel** -- iOS requires
both, and rounds the corners itself. Rebuilt with ImageMagick from the source
art; neither file is generated by the build.

The bundles are separate `.asd` files, `lisp-listener-app.asd` and
`lisp-listener-ios.asd`, and they must stay separate: `:defsystem-depends-on` is
resolved when a `.asd` is **read**, not when its system is built, so declaring
either bundle in `lisp-listener.asd` would make its builder a hard requirement
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

- **Bind both `cl:*debugger-hook*` and `sb-ext:*invoke-debugger-hook*`** (ECL:
  `ext:`), **and bind them again at every debugger level.** `invoke-debugger`
  nulls whichever hook it calls while it runs, so each level down uses one up:
  with the two bound once, level 3 fell through to the Lisp's own debugger.
  `listener-debugger` rebinds both from `*listener-debugger-hook*`.

- **An error evaluated at a debugger prompt must be sent to `invoke-debugger`
  by hand.** All of `listener-debugger` runs inside the hook's `handler-case`,
  which would otherwise handle it. That is the same trap as `handler-case` around
  `listener-loop`, one level down: every error typed at `[1]` silently returned
  to `CL-USER>`, and no level below the first ever opened. The `handler-bind`
  around the debugger's evaluation does this; `case-nested-debugger` covers it.

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
  `-cancelOperation:` looks correct and does nothing. The reverse holds for Tab:
  it calls **super's** `-complete:`, because the view's own override cancels a
  debugger level first.

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

- **On iOS, Interrupt can only reach a listener that is WAITING.** A running
  computation cannot be stopped at all: in the app this ECL delivers no
  interrupt to a thread -- measured on a plain spinning thread, not just on the
  listener's -- and `mp:process-kill` leaves it running too. So `(loop)` typed
  on a phone is there until the app is killed, and the iOS self-test types no
  such form, because nothing could get the listener back. On the Mac both
  states work. Worth raising with lispnik/ecl rather than working around here.

- **Interrupt needs two mechanisms, and the choice must be made under the
  queue's lock.** An interrupt that aborts does not reliably unwind a thread out
  of a condition wait: on ECL it does not unwind it at all -- the abort is lost
  -- and it leaves the lock held-but-not-owned, so the next unlock signals
  `Attempted to give up lock ... that is not owned by process'. So a thread
  parked in `read` is asked through the queue's flag and aborts itself as it
  wakes, and only a thread off in a computation is interrupted.
  `queue-request-abort-if-waiting` answers under the lock, which is what makes
  the choice exact; a flag set while the thread was computing would otherwise
  be left for a later read to trip over, and clearing it at the prompt lost a
  Stop pressed in the gap between a value and its prompt. `case-interrupt`
  covers both states, and it took a fifteen-run hammer to see the race.

- **Output inserted at the caret must carry the caret along.** NSTextView
  moves a caret that sits at the insertion point; UITextView leaves it behind,
  in front of the output. `transcript-insert` moves it only if the toolkit did
  not, so the one code path is right on both.

- **On iOS, Return is not `-insertNewline:`.** It arrives as the delegate being
  asked whether `"\n"` may replace a range, through
  `-textView:shouldChangeTextInRange:replacementText:`. AppKit's selector ends in
  `replacementString:`, and defining that name on iOS does nothing.

- **UIKit resets the typing attributes whenever the selection moves**, so
  `-textViewDidChangeSelection:` puts them back. **A text view keeps Tab and the
  arrows for itself** unless each `UIKeyCommand` sets
  `wantsPriorityOverSystemBehavior`.

- **A `UIAlertController` cannot be made taller.** It is sized by its content,
  with no supported way to ask for more room, so the restarts came up as a stub
  at the foot of the screen. A presented controller with **detents** is what
  has a height of its own; that is why the sheet is one.

- **There is no `NSModalPanelRunLoopMode` on iOS.** That is why the run loop
  modes belong to the front end.

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

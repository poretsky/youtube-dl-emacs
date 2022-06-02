# A youtube-dl download manager for Emacs

This package manages a video download queue for [youtube-dl][yt], which
serves as the back end. It manages a single youtube-dl subprocess,
downloading one video at a time. New videos can be queued at any time.

To make use of this package compile it with the command:

```bash
$ make
```

And then add the following line to your Emacs startup file:

```elisp
(load "path/to/this/directory/youtube-dl-autoloads")
```

The `youtube-dl` command queues a URL for download. The command
`youtube-dl-audio` does the same, but only audio content is
retrieved. If URL points to a playlist, all its items are added to the
download queue with the respective index prefixes. If these commands
are called with prefix argument (<kbd>C-u</kbd>) playlist order will
be reversed. Failures are retried up to
`youtube-dl-max-failures`. Items can be paused or set to be downloaded
at a slower rate (`youtube-dl-slow-rate`).

All these actions are also available via `menu / tools`.

The `youtube-dl-list` command displays a list of all active video
downloads. From this list, items under point can be canceled
(<kbd>d</kbd>), paused (<kbd>p</kbd>), slowed (<kbd>s</kbd>), played
(<kbd>SPC</kbd>), and have its priority adjusted
(<kbd>[</kbd> and <kbd>]</kbd>). Among this, an item description can
be loaded and shown in a separate buffer (<kbd>RET</kbd>). Highlighted
references in this buffer can be navigated with <kbd>tab</kbd> and
<kbd>backtab</kbd> and activated with <kbd>RET</kbd>.

Integration with w3m web browser is also provided. Two additional
keystrokes are available in `w3m-mode`: <kbd>y</kbd> pops up the menu
of youtube-dl actions and <kbd>M-RET</kbd> pressed on an anchor
suggests an action applicable to the anchor depending on the URL
nature or visit the URL with `w3m-view-this-url`. When youtube-dl
playback or download submission is invoked on an anchor, the anchor
URL is used. Otherwise the URL is requested from user. After download
submission the download listing pops up with pointer positioned on the
newly added item.

![](https://i.imgur.com/wDWNsMf.png)

## Limitations

To display the size and progress, this package relies on a specific
output format from youtube-dl. Using an external downloader
(`--external-downloader`) breaks this, as can mucking around too much
with the command line switches.

[yt]: https://rg3.github.io/youtube-dl/

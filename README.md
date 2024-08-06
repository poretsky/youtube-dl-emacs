# A youtube-dl download manager for Emacs

This package manages a video download queue for
[youtube-dl](https://rg3.github.io/youtube-dl/), which serves as the
back end. It manages a single youtube-dl subprocess, downloading one
video at a time. New videos can be queued at any time. With this
package you can also view video descriptions and, in conjunction with
[mpv media player](https://mpv.io/), even play videos online.

## Installation

To make use of this package compile it with the command:

```bash
$ make
```

And then add the following line to your Emacs startup file:

```elisp
(load "path/to/this/directory/youtube-dl-autoloads")
```

## Usage

Since [youtube-dl](https://github.com/ytdl-org/youtube-dl) itself is
somewhat conservative, it is recommended to consider using its
actively developed fork [yt-dlp](https://github.com/yt-dlp/yt-dlp)
instead. The `youtube-dl` executable name doesn't matter for this
package, it is customizable, but [mpv media player](https://mpv.io/)
requires just `youtube-dl`. Thus, if some other fork is used in fact,
the respective alias should be created anyway.

The following actions are available from the `tools / youtube-dl`
submenu in the general Emacs menu bar when this package is installed:

- **Customize** -- Open youtube-dl customization group.
- **Submit download** -- Submit download from a URL provided by user
  adding it to the download queue.
- **Submit download audio** -- Submit audio content download from a
  URL provided by user adding it to the download queue.
- **Play video clip** -- Play video clip from a URL provided by user
  online.
- **View video clip info** -- Retrieve video clip description from a
  URL provided by user and show it in the dedicated buffer.
- **Show download queue** -- Get to the download queue control
  buffer.

### Download directory

The downloaded stuff is placed in the directory that can be customized
via the `youtube-dl-download-directory` option. It is
`~/download/youtube` by default. It will be automatically created if
absent.

Evident video and audio downloads go to their respective subfolders
that are automatically created by need in the download directory. By
default these subfolders are named `video` and `audio`, and, of
course, their names are customizable.

### Download queue control

In the download queue control buffer each item is presented by a line
where its title, total size, and progress are displayed along with
some additional status information if any. Currently active item is
highlighted.

The following actions are available in this buffer via hot keys:

- <kbd>a</kbd> -- Add new download by a URL from user.
- <kbd>l</kbd> -- Show download log for the item under point.
- <kbd>L</kbd> -- Close the download log.
- <kbd>y</kbd> -- Yank URL from the item under point into kill ring.
- <kbd>SPC</kbd> -- Start playback for the item under point.
- <kbd>RET</kbd> -- Open video description for the item under point.
- <kbd>k</kbd> -- Stop playback if any.
- <kbd>q</kbd> -- Close the window.
- <kbd>Q</kbd> -- Stop download process, clear queue and quit.
- <kbd>d</kbd> -- Delete the item under point from the download queue.
- <kbd>p</kbd> -- Pause / unpause download for the item under point.
- <kbd>P</kbd> -- Pause / unpause all downloads.
- <kbd>s</kbd> -- Toggle slow download mode for the item under point.
- <kbd>S</kbd> -- Toggle slow download mode for all items.
- <kbd>[</kbd> -- Decrement priority for the item under point.
- <kbd>]</kbd> -- Increment priority for the item under point.

### Playlists

When submitting a download from a URL that actually points to a
playlist, the entire playlist will be submitted. In the download queue
control buffer playlist is represented by a group of items with head
and bottom separators. Playlist item titles are prefixed by their
respective indices. The most of download control actions described
above can be applied to an entire playlist when the playlist header is
under point. If download submission action is invoked with prefix
argument (<kbd>C-u</kbd>), playlist order will be reversed.

Unless the `youtube-dl-restrict-filenames` option is set (it is not
set by default), playlists are downloaded in their own subdirectories.

When video description view is requested for a URL that actually
points to a playlist, this playlist is submitted for download as
paused and presented to the user for further manipulations.

### Viewing video description

Video description if requested is shown in a specially dedicated
buffer. Along with the descriptive information this buffer may contain
some action buttons and clickable links and following keystrokes are
available there:

- <kbd>y</kbd> -- Yank clip URL into kill ring.
- <kbd>a</kbd> -- Save clip URL as a w3m bookmark.
- <kbd>SPC</kbd> -- Start clip playback.
- <kbd>k</kbd> -- Stop playback if any.
- <kbd>q</kbd> -- Quit view and close the window.

## Integration with w3m web browser

An additional keystroke <kbd>y</kbd> in `w3m-mode` pops up the menu of
youtube-dl available actions. The keystroke <kbd>RET</kbd> in
`w3m-mode` is redefined in such a manner that being pressed on an
anchor tries to suggest an applicable youtube-dl action guessing it
from the URL nature. When no special actions are guessed, the anchor
is visited with `w3m-view-this-url` in the usual way.

Link to a video clip can be saved as a w3m bookmark directly from the
youtube-dl view buffer by pressing <kbd>a</kbd>.

When youtube-dl playback or download submission is invoked on an
anchor, the anchor URL is used. Otherwise the URL is requested from
the user. After download submission the download control buffer pops
up with pointer positioned on the newly added item.

## Cooperation with Invidious

Web interface of Youtube in practice doesn't fit for browsing with
w3m. But there is an opensource alternative frontend called
[Invidious](https://invidious.io/) that can be used instead. You can
[install it locally](https://docs.invidious.io/installation/) if you
prefer or just use one of the
[publicly available instances](https://docs.invidious.io/instances/). In
either case you should specify the address of the chosen instance in
the customization option `youtube-dl-w3m-invidious-url`. And that is
the only thing necessary to take advantage of the Invidious frontend.

The following additional keystrokes are available in `w3m-mode` when
this package is installed:

- <kbd>Y</kbd> -- Youtube search.
- <kbd>i</kbd> -- Visit homepage of the Invidious instance in use.

This package also takes care of proper redirection of the Youtube
requests and Invidious pages preparation for better view in W3m.

## Limitations

To display the size and progress, this package relies on a specific
output format from youtube-dl. Using an external downloader
(`--external-downloader`) breaks this, as can mucking around too much
with the command line switches.

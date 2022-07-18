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

The following actions are available from the `tools / youtube-dl`
submenu in the general Emacs menu bar:

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

## Limitations

To display the size and progress, this package relies on a specific
output format from youtube-dl. Using an external downloader
(`--external-downloader`) breaks this, as can mucking around too much
with the command line switches.

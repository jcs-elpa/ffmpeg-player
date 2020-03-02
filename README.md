[![Build Status](https://travis-ci.com/jcs090218/ffmpeg-player.svg?branch=master)](https://travis-ci.com/jcs090218/ffmpeg-player)
[![MELPA](https://melpa.org/packages/ffmpeg-player-badge.svg)](https://melpa.org/#/ffmpeg-player)
[![MELPA Stable](https://stable.melpa.org/packages/ffmpeg-player-badge.svg)](https://stable.melpa.org/#/ffmpeg-player)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)


# ffmpeg-player
> Play video using ffmpeg.


## Acknowledge

This package will output video into images (in frame). This might causes
many I/O times and disk space (Even though these frames will eventually get 
cleaned up). I would like to mention you all the information and consequences
before you use this package.

*See [melpa/6560](https://github.com/melpa/melpa/pull/6560) for more information.*


## External Program

This package uses these following programs, make sure these program are added
to your path.

* [ffmpeg](https://www.ffmpeg.org/)
* [ffplay](https://www.ffmpeg.org/) - Bundle with `ffmpeg`.


## Capability

Base on `ffplay`'s website.

> ffplay is a very simple and portable media player using the FFmpeg libraries
and the SDL library. It is mostly used as a testbed for the various FFmpeg APIs.

Hence, I have encounter some issues synchronize video and audio. Try move the
timeline by pressing `<left>` or `<right>` key multiple times could resolve
such an issue.


## Usage

You can play the video by calling function `ffmpeg-player-video`.

```el
(ffmpeg-player-video (expand-file-name "./test/1.avi"))
```

And here is the control of the media.

* <kbd>space</kbd> - Pause/Unpause.
* <kbd>up</kbd> - Increase volume by 5.
* <kbd>down</kbd> - Decrease volume by 5.
* <kbd>left</kbd> - Backward timeline 10 seconds.
* <kbd>right</kbd> - Forward timeline 10 seconds.
* <kbd>m</kbd> - Mute/Unmute.
* <kbd>r</kbd> - Replay.


## Todo List

- [ ] Play youtube video through url.


## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!

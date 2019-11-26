[![Build Status](https://travis-ci.com/jcs090218/ffmpeg-player.svg?branch=master)](https://travis-ci.com/jcs090218/ffmpeg-player)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)


# ffmpeg-player
> Play video using ffmpeg.


## External Program

This package uses these following programs, make sure these program are added
to your path.

* [ffmpeg](https://www.ffmpeg.org/)
* [ffplay](https://www.ffmpeg.org/) - Build with `ffmpeg`.


## Capability

Base on `ffplay`'s website.

> `ffplay` is a very simple and portable media player using the FFmpeg libraries
and the SDL library. It is mostly used as a testbed for the various FFmpeg APIs.

Hence, I have encounter some issue synchronize video and audio. Try move the
timeline by pressing `<left>` or `<right>` key.


## Usage

You can play the video by calling function `ffmpeg-player-video`.

```el
(ffmpeg-player-video (expand-file-name "./test/1.avi"))
```

## Todo List

- [] Play youtube video through url.


## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!

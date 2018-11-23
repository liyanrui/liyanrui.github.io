<!--
.. title: 用 FFmpeg 为视频做点小手术
.. slug: some-minor-operation-for-videos-with-ffmpeg
.. date: 2018-11-14 10:34:36 UTC+08:00
.. tags: FFmpeg
.. category: Linux
.. link: 
.. description: 
.. type: text
-->

# FFmpeg 命令的一般格式

```console
$ ffmpeg [全局选项] [输入选项] -i input.mp4 [输出选项] output.mp4
```

# 屏幕录制

在 Linux 中，我的桌面屏幕分辨率是 \(1920\times 1080\)，使用命令

```console
$ ffmpeg -video_size 1920x1080 -framerate 25 -f x11grab -i :0.0 output.mp4
```

可将录制整个屏幕。`-i ：0.0` 表示将当前的屏幕作为 ffmpeg 的输入。

若想录制屏幕中的指定区域，例如录制左上角点坐标为 (300, 100)、宽 500、高 200 的区域，只需

```console
$ ffmpeg -video_size 500x200 -framerate 25 -f x11grab -i :0.0+300,100 output.mp4
```

摁 `q` 键可退出屏幕录制过程。

FFmpeg 的 Wiki 页面[1]给出了 Windows、Mac OS X 中的屏幕录制命令。

# 从时间轴上裁剪

使用命令

```console
$ ffmpeg -ss 00:10:25 -i input.mp4 -frames:v 1 output.png
```

可从视频的时间轴上截取时间戳为 00:10:25 时的画面。`-i` 表输入。`-ss` 位于 `-i` 之前，意味着跳到所输入的视频文件的给定的时间戳对应的位置。至于 `-i` 之后的设定，作用于输出。`-frames:v 1` 的意思是获取视频的 1 帧，亦即图片；若给定的帧数大于 1，那么输出的文件应当是视频，因此可以用这个参数截取一段视频。

若给定起始时间戳和持续时长，截取输入的视频里的一段，可以用 `-t` 选项与 `-ss` 选项相配合而实现。`-ss` 选项放在 `-i` 选项之前，用于确定所截取视频的起始帧。`-t` 选项尾随 `-ss`。例如，从输入的视频中截取从时间戳 00:10:25 开始，持续 10 分钟的一段，只需

```console
$ ffmpeg -ss 00:10:25 -t 600 -i input.mp4 output.mp4
```

或

```console
$ ffmpeg -ss 00:10:25 -t 00:10:00 -i input.mp4 output.mp4
```

若想按给定起始和终止时间戳截取视频，需要用 `-to` 选项，但这个选项只能作用于视频的输出，亦即它必须位于 `-i` 选项之后。例如，

```console
$ ffmpeg -i input.mp4 -ss 00:10:25 -to 00:20:25 output.mp4
```

也能实现从时间戳 00:10:25 开始并持续 10 分钟的一段视频的截取，但是与上述将 `-ss` 选项放在 `-i` 选项之前的命令相比，这个命令通常较为低效。因为 ffmpeg 会逐帧对输入视频进行解码，这个过程结束后，进入输出阶段时，再按照给定的起止时间戳截取视频。`-ss` 选项放在 `-i` 之前，ffmpeg 不会对 `-ss` 给定的时间戳之前的帧进行解码。

在 `-ss` 和 `-to` 配合使用时，将 `-ss` 放在 `-i` 选项之前，并非不可以，但是 `-to` 的含义就变了，它不再表示视频截取的终止点。因为 `-to` 原本的起点被 `-ss` 篡改了。例如

```console
$ ffmpeg -ss 00:10:25 -i input.mp4 -to 00:20:25 output.mp4
```

实际上截取的是输入视频中的从 00:10:25 到 00:30:25 这一段，而非从 00:10:25 到 00:20:25 这一段。在这种情况下，可以通过 `-copyts` 选项进行修正，例如

```console
$ ffmpeg -ss 00:10:25 -i input.mp4 -to 00:20:25 -copyts output.mp4
```

在上述命令中，输出视频是经过 ffmpeg 重新编码的，因此与输入视频的编码不相同。若希望输入视频的编码格式与输入视频相同，需要用 `-c` 选项将输出视频的编码格式设为 `copy`，例如

```console
$ ffmpeg -i input.mp4 -ss 00:10:25 -to 00:20:25 -c copy output.mp4
```

实际上，`-c copy` 选项可分解为

```
-c:v copy -c:a copy
```

即复制输入视频的视频编码和音频编码。

不过，在截取视频时，若使用 `-c copy` 选项，并且截取的视频要与其他视频拼接，那么应当再加上 `-avoid_negative_ts 1` 选项，例如

```console
$ ffmpeg -ss 00:10:25 -t 600 -i input.mp4 -c copy -avoid_negative_ts 1 output.mp4
```

原因不明，未作深究。推测是，若将截取视频拼接到其他视频的尾部，`-avoid_negative_ts 1` 可以避免前者的第 1 帧覆盖后者的最后一帧。

# 去除视频水印

ffmpeg 具有类似于图像处理软件那样的滤镜（Filter）功能。滤镜有很多种。delogo 滤镜可用于消除视频上的水印。

**引用的文档**
--------

**\[1\]**　[https://trac.ffmpeg.org/wiki/Capture/Desktop](https://trac.ffmpeg.org/wiki/Capture/Desktop)

**\[2\]**　[https://trac.ffmpeg.org/wiki/Seeking](https://trac.ffmpeg.org/wiki/Seeking)

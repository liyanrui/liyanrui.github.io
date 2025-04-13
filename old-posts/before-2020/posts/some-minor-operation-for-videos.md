<!--
.. title: 为视频做小手术
.. slug: some-minor-operation-for-videos
.. date: 2018-11-14 10:34:36 UTC+08:00
.. tags: FFmpeg
.. category: Linux
.. link: 
.. description: 
.. type: text
-->

FFmpeg 命令的一般格式为：

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

实际上截取的是输入视频中的从 00:10:25 到 00:30:25 这一段，而非从 00:10:25 到 00:20:25 这一段。

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

ffmpeg 具有类似于图像处理软件那样的滤镜（Filter）功能，作用于视频输出阶段。滤镜有很多种。delogo 滤镜可用于消除视频上的水印。

delogo 滤镜的基本用法是

```console
$ ffmpeg -i input.mp4 -vf "delogo=x=a:y=b:w=c:h=d" -c:a copy output.mp4
```

表示将一个左上角点坐标为 (a, b)、宽为 c、高为 d 的矩形区域置于视频的每一帧上。这个矩形区域内的像素由矩形边缘之外的像素确定。因此，若用 delogo 滤镜覆盖视频中的水印，结果产生的效果类似于模糊的毛玻璃。

若视频里有多处水印，可以用多个 delogo 去覆盖，各条 delogo 语句之间以「`,`」隔开。例如

```
-vf "delogo=x=a:y=b:w=c:h=d, delogo=x=e:y=f:w=g:h=h, ..."
```

有些人为了保护视频里的水印不会被他人轻易消除，在添加水印的时候，会让水印在一定时间内出现，例如每隔 5 分钟出现，并停留 1 分钟。对于这样的视频，若是将 delogo 滤镜作用于视频的每一帧画面，就会对视频里没有水印的帧造成一些破坏。应对此类水印，可以使用 delogo 滤镜的 `enable` 参数，使得 delogo 滤镜与水印保持同时出现和消失的效果。例如，

```
-vf "delogo=x=a:y=b:w=c:h=d:enable='between(t, e, f)'"
```

`t` 表示视频当前的时间戳，`e` 表示一段时间的起点，`f` 表示一段时间的终点。这样的 `enable` 参数，其作用是，当视频当前的时间戳 `t` 不小于 `e` 且不大于 `f` 时，就让 delogo 滤镜生效。亦即，只要让 `e` 和 `f` 的值与水印出现和消失的时间吻合，就能够保证 delogo 滤镜与水印保持同时出现和消失。

滤镜作用于视频的输出过程，这意味着 FFmpeg 会对输入视频重新进行编码，视频画面质量有所损失在所难免。不过，通过调整输出视频的码率，可以使得输出视频的画面质量与输入视频相近。调整输出视频码率的选项为 `-b:v`，我通常将其值设为 `1600k`（对于蓝光高清视频，这个值有些小），例如：

```console
$ ffmpeg -i input.mp4 -vf "delogo=x=a:y=b:w=c:h=d" -c:a copy -b:v 1600k output.mp4
```

# 为视频增加水印

使用 overlay 滤镜可以将图片置于到视频的每一帧画面上。用这种滤镜可以在视频中增加水印，当然也能用于覆盖视频中一些不希望出现的区域。

overlay 滤镜的基本用法为

```console
$ ffmpeg -i input.mp4 -i logo.png -vf "overlay=a:b" -c:a copy -b:v 1600k output.mp4
```

`a` 和 `b` 用于设定 `logo.png` 图片在视频每一帧上的位置，即图片的左上角点在帧画面上的坐标。

增加多个水印也是可以的，例如

```console
$ ffmpeg -i input.mp4 -i logo1.png -i logo2.png \
  -filter_complex "overlay=a:b, overlay=c:d" -c:a copy -b:v 1600k output.mp4
```

需要注意每个 `-i` 输入的图片文件与 `overlay` 的对应关系。`-vf` 用于设定单输入文件的滤镜，对于多份输入，需要用 `-filter_complex` 选项。

overlay 滤镜也支持 `enable` 参数。

**引用的文档**
--------

**\[1\]**　[https://trac.ffmpeg.org/wiki/Capture/Desktop](https://trac.ffmpeg.org/wiki/Capture/Desktop)

**\[2\]**　[https://trac.ffmpeg.org/wiki/Seeking](https://trac.ffmpeg.org/wiki/Seeking)

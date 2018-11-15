<!--
.. title: FFmpeg 笔记
.. slug: ffmpeg
.. date: 2018-11-14 10:34:36 UTC+08:00
.. tags: FFmpeg
.. category: Linux
.. link: 
.. description: 
.. type: text
-->

# 按时间裁剪

下面这个脚本，可以按给定时间点去除当前目录内所有视频的片头：

```console
$ ffmpeg -ss 起始时间点 -accurate_seek -i input.mp4 \
  -c:v copy -c:a copy -avoid_negative_ts 1 output.mp4
```

起始时间按「`小时:分钟:秒`」格式给出。在给定起始时间点的情况下，通过 `-t` 选项可设定截取时长，也可以用 `-to` 指定视频截取的终止时间点。

# 去静态水印

```console
$ ffmpeg -threads 4 -i input.mp4 \
    -vf "delogo=x=5:y=320:w=110:h=75, delogo=x=625:y=350:w=90:h=45" \
    -qscale 2.5 -c:a copy output.mp4
```

`qscale` 用于设定输出视频的画面质量，取值范围为 [0.01,255]，此值越小，画面质量越好。在 Gentoo 中，在编译安装 ffmpeg 时，若开启了 `x264` USE 标志，可无需设定 `qscale`，ffmpeg 似乎会将源 MP4 文件转化为 H264 格式，待视频处理完毕后，在输出时再由 H264 转换为 MP4 格式。

`delogo` 参数用于设定过滤水印的矩形区域，`x` 和 `y` 为矩形的左上角坐标，`w` 和 `h` 分别为矩形的宽度和高度。可以通过视频播放软件提供的截屏功能，再用 GIMP 对所截图像确定这个矩形范围。

使用 ffplay 可预览硬盘中的水印过滤区域：

```console
$ ffplay -i input.mp4 \
  -vf "delogo=x=5:y=320:w=110:h=75:show=1, delogo=x=625:y=350:w=90:h=45:show=1"
```

# 去动态水印

有些视频会按一定的时间规律在视频中插入水印，例如，在 1、3、5、7……奇数分钟时插入水印，并且水印每次停留时间为 1 分钟。对于此类水印，可事先通过程序生成一组 `delogo` 语句，再将这些语句嵌入 ffmpeg 命令。例如

```console
$ ffmpeg -i input.mp4 -vf \
 "$(for ((i = 1; i < 60; i++)); do \
  if [ $((i%2)) -eq 1 ]; then \
  start=$((i*60-8)); stop=$((start + 60));\
  echo -n "delogo=x=1:y=7:w=718:h=23:enable='between(t, $start, $stop)', ";\
  fi; done | sed 's/, $//g')" \-c:a copy output.mp4
```

可以去除在 52 秒、2 分 52 秒、4 分 52 秒、6 分 52 秒……出现并停留一分钟的水印。


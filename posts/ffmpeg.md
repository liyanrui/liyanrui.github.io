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

# 去水印

```console
$ ffmpeg -threads 4 -i input.mp4 \
    -filter_complex "delogo=x=5:y=320:w=110:h=75, delogo=x=625:y=350:w=90:h=45" \
    -qscale 2.5 output.mp4
```

`qscale` 用于设定输出视频的画面质量，取值范围为 [0.01,255]，此值越小，画面质量越好。

`delogo` 参数用于设定过滤水印的矩形区域，`x` 和 `y` 为矩形的左上角坐标，`w` 和 `h` 分别为矩形的宽度和高度。可以通过视频播放软件提供的截屏功能，再用 GIMP 对所截图像确定这个矩形范围。

使用 ffplay 可预览硬盘中的水印过滤区域：

```console
$ ffplay -i input.mp4 \
  -vf "delogo=x=5:y=320:w=110:h=75:show=1, delogo=x=625:y=350:w=90:h=45:show=1"
```

# 时间裁剪

下面这个脚本，可以按给定时间点去除当前目录内所有视频的片头：

```Bash
#!/bin/bash

clip_all() {
    for i in ./*
    do
        if [ -d $i ]
        then
            cd $i
            clip_all $1
        else
            ffmpeg -ss $1 -accurate_seek -i $i \
                   -vcodec copy -acodec copy -avoid_negative_ts 1 new-${i}
            rm $i
            mv new-${i} $i
        fi
    done
}

clip_all $1
```

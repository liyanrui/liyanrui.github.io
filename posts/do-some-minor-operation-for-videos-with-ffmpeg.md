<!--
.. title: 用 FFmpeg 为视频做点小手术
.. slug: do-some-minor-operation-for-videos-with-ffmpeg
.. date: 2018-11-14 10:34:36 UTC+08:00
.. tags: FFmpeg
.. category: Linux
.. link: 
.. description: 
.. type: text
-->

# 屏幕录制

在 Linux 中，我的桌面屏幕分辨率是 \(1920\times 1080\)，命令

```console
$ ffmpeg -video_size 1920x1080 -framerate 25 -f x11grab -i :0.0 output.mp4
```

可将录制整个屏幕。若想录制屏幕中的指定区域，例如录制左上角点坐标为 (300, 100)、宽 500、高 200 的区域，只需

```console
$ ffmpeg -video_size 500x200 -framerate 25 -f x11grab -i :0.0+300,100 output.mp4
```

摁 `q` 键可退出屏幕录制过程。

FFmpeg 的 Wiki 页面[1]给出了 Windows、Mac OS X 中的屏幕录制命令。

# 从时间轴上裁剪

从时间轴上截取某一秒
ffmpeg -video_size 1920x1080 -framerate 25 -f x11grab -i :0.0 output.mp4


\[1\]　https://trac.ffmpeg.org/wiki/Capture/Desktop

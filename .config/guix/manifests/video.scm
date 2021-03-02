;; Video creation tools

(specifications->manifest
 '(;; Screen Capture and Streaming
   "obs"
   "obs-websocket"
   "ffmpeg"    ;; ffmpeg and ffplay
   "v4l-utils" ;; Get details about webcams: v4l2-ctl --list-devices

   ;; Screen recording with pulseaudio source 0 (-i 0)
   ;; ffmpeg -y -f x11grab -video_size 2560x1440 -i :0.0+0,0 -f pulse -ac 2 -i 0 -c:v libx264 -pix_fmt yuv420p -crf 0 -preset ultrafast ~/output.mp4 -v 0

   ;; Scaling video down to 1080p
   ;; ffmpeg -i output2.mp4 -s 1920x1080 ~/output2-scaled.mp4

   ;; Show webcam with specific resolution
   ;; ffplay -f v4l2 -framerate 60 -video_size hd480 /dev/video2 -v 0

   ;; Video Editing
   "blender"))

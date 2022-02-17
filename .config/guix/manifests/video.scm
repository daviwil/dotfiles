;; Video creation tools

(specifications->manifest
 '(;; Screen Capture and Streaming
   ;; "obs" -- Use Flatpak instead
   ;; "obs-websocket"
   "ffmpeg"    ;; ffmpeg and ffplay
   "v4l-utils" ;; Get details about webcams: v4l2-ctl --list-devices

   ;; Video Editing
   "blender"))

(let  port (open-output-file "songs")
  (show '(this is the song) port )
  (show '(andt second) port)
  (show '(fuck you btw) port)
  (close-output-file port))



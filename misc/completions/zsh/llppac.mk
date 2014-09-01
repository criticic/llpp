#compdef llppac

_arguments -s \
  '-m[Mime/type]:mime:_mime_types' \
  '-t[Filter]' \
  '-f[Force]' \
  '*::document:_files -g "*.@re@"'

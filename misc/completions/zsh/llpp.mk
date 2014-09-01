#compdef llpp

_arguments -s \
  '-p[Set password]' \
  '-f[Set path to the user interface font]:font:_files' \
  '-c[Set path to the configuration file]:config:_files' \
  '-last[Open last document]' \
  '-page[Jump to page]' \
  '-tcf[Set path to the trim cache file]:trim:_files' \
  '-dest[Set named destination]' \
  '-wtmode[Operate in wt mode]' \
  '-cxack[Cut corners]' \
  '-remote[Set path to the remote commands source]:remote:_directories' \
  '-origin[Set original path]:origin:_directories' \
  '-gc[Collect garbage with the help of a script]:script:_files' \
  '-v[Print version and exit]' \
  '-embed[Embed into window]' \
  '-help[Display this list of options]' \
  '--help[Display this list of options]' \
  '*::document:_files -g "*.@re@"'

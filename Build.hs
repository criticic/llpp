import Data.Maybe
import Development.Shake
import Development.Shake.Util
import Development.Shake.FilePath

data CM = CMO | CMI

outdir = "build"
mudir = "/home/malc/x/rcs/git/mupdf"

ocamlc = "ocamlc.opt"
ocamldep = "ocamldep.opt"
ocamlflags = "-warn-error +a -w +a -g -safe-string"
ocamlflagstbl = [("main.cmo", ("-I +lablGL", "sed -f pp.sed"))
                ,("config.cmo", ("-I +lablGL", ""))
                ,("wsi.cmo", ("-I " ++ outdir ++ "/le", ""))]

cc = "gcc"
cflags = "-Wall -Werror -D_GNU_SOURCE -O\
         \ -g -std=c99 -pedantic-errors\
         \ -Wunused-parameter -Wsign-compare -Wshadow"
cflagstbl =
  [("link.o"
   ,"-I " ++ mudir ++ "/include -I "
    ++ mudir ++ "/thirdparty/freetype/include")
  ]

cm' outdir t =
  target `op` \out -> do
    let key = dropDirectory1 out
    let src = key -<.> suffix
    need [src]
    let (flags, pp) = case lookup key ocamlflagstbl of
          Nothing -> (ocamlflags, [])
          Just (f, []) -> (ocamlflags ++ " " ++ f, [])
          Just (f, pp) -> (ocamlflags ++ " " ++ f, ["-pp", pp])
    Stdout stdout <- cmd ocamldep "-one-line -I" outdir "-I le" pp src
    need $ deplist $ parseMakefile stdout
    cmd ocamlc "-c -I" outdir flags "-o" out pp src
  where (target, suffix, op) = case t of
          CMO -> (outdir ++ "/*.cmo", ".ml", (%>))
          CMI -> (outdir ++ "/*.cmi", ".mli", (%>))
        deplist [] = []
        deplist ((_, reqs) : _) =
          map
            (\n -> if takeDirectory1 n == outdir then n else outdir </> n) reqs

main = shakeArgs shakeOptions { shakeFiles = outdir
                              , shakeVerbosity = Normal } $ do
  want $ map ((</>) outdir) ["main.cmo", "help.ml", "link.o"]
  outdir ++ "/help.ml" %> \out -> do
    need ["mkhelp.sh", "KEYS"]
    Stdout f <- cmd "/bin/sh mkhelp.sh KEYS"
    writeFileChanged out f
  outdir ++ "/help.cmo" %> \out -> do
    let src = outdir </> "help.ml"
    need [src]
    cmd "ocamlc -c -o" out src
  outdir ++ "/link.o" %> \out -> do
    let key = dropDirectory1 out
    let flags = case lookup key cflagstbl of
          Nothing -> cflags
          Just f -> cflags ++ " " ++ f
    let src = key -<.> ".c"
    let dep = out -<.> ".d"
    unit $ cmd ocamlc "-ccopt"
      [flags ++ " -MMD -MF " ++ dep ++ " -o " ++ out]  "-c" src
    needMakefileDependencies dep
  let cmos = map (\name -> outdir </> name ++ ".cmo")
             ["help", "utils", "parser", "le/bo", "wsi", "config", "main"]
  outdir ++ "/llpp" %> \out -> do
    need $ map ((</>) outdir) ["link.o", "main.cmo", "wsi.cmo", "help.ml"]
    let cclib = "-lmupdf -lz -lfreetype -ljpeg \
                \-ljbig2dec -lopenjpeg -lmujs \
                \-lpthread -L" ++ mudir ++ "/build/native -lcrypto"
    unit $ cmd ocamlc "-custom -I +lablGL -o " out
      "unix.cma str.cma lablgl.cma" cmos (outdir </> "link.o") "-cclib" [cclib]
  cm' outdir CMI
  cm' outdir CMO
  cm' (outdir ++ "/le") CMO

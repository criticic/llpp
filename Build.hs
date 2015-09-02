{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import System.Exit
import Development.Shake
import Development.Shake.Util
import Development.Shake.Config
import Development.Shake.Classes
import Development.Shake.FilePath

newtype OcamlCmdLineOracle = OcamlCmdLineOracle String
                           deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype CCmdLineOracle = CCmdLineOracle String
                       deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype GitDescribeOracle = GitDescribeOracle ()
                          deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
data CM = CMO | CMI

outdir = "build"
mudir = "/home/malc/x/rcs/git/mupdf"

ocamlc = "ocamlc.opt"
ocamldep = "ocamldep.opt"
ocamlflags = "-warn-error +a -w +a -g -safe-string"
ocamlflagstbl = [("main.cmo", ("-I +lablGL", "sed -f pp.sed"))
                ,("config.cmo", ("-I +lablGL", ""))
                ,("wsi.cmo", ("-I le", ""))
                ]
cflags = "-Wall -Werror -D_GNU_SOURCE -O\
         \ -g -std=c99 -pedantic-errors\
         \ -Wunused-parameter -Wsign-compare -Wshadow"
cflagstbl =
  [("link.o"
   ,"-I " ++ mudir ++ "/include -I "
    ++ mudir ++ "/thirdparty/freetype/include")
  ]
cmos = map (\name -> outdir </> name ++ ".cmo")
       ["help", "utils", "parser", "le/bo", "wsi", "config", "main"]
cclib = "-lmupdf -lz -lfreetype -ljpeg \
        \-ljbig2dec -lopenjpeg -lmujs \
        \-lpthread -L" ++ mudir ++ "/build/native -lcrypto"

getincludes :: [String] -> [String]
getincludes [] = []
getincludes ("-I":arg:tl) = arg : getincludes tl
getincludes (_:tl) = getincludes tl

isabsinc :: String -> Bool
isabsinc [] = False
isabsinc (hd:tl) = hd == '+' || hd == '/'

fixincludes [] = []
fixincludes ("-I":d:tl)
  | isabsinc d = "-I":d:fixincludes tl
  | otherwise = "-I":d:"-I":(outdir </> d):fixincludes tl
fixincludes (e:tl) = e:fixincludes tl

ocamlKey key =
  case lookup key ocamlflagstbl of
  Nothing -> (ocamlc, ocamlflags, [])
  Just (f, []) -> (ocamlc, ocamlflags ++ " " ++ f, [])
  Just (f, pp) -> (ocamlc, ocamlflags ++ " " ++ f, ["-pp", pp])

cKey key =
  case lookup key cflagstbl of
  Nothing -> cflags
  Just f -> f ++ " " ++ cflags

fixppfile :: String -> [String] -> [String]
fixppfile s ("File":_:tl) = ("File \"" ++ s ++ "\","):tl
fixppfile _ l = l

fixpp :: String -> String -> String
fixpp r s =
  unlines $ (unwords $ fixppfile r $ words hd) : tl
  where hd:tl = lines s

cm' outdir t oracle =
  target `op` \out -> do
    let key = dropDirectory1 out
    let src' = key -<.> suffix
    let src = if src' == "help.ml" then outdir </> src' else src'
    need [src]
    (comp, flags, ppflags) <- oracle $ OcamlCmdLineOracle key
    let flagl = words flags
    let incs = unwords $ ["-I " ++ d | d <- getincludes flagl
                                     , not $ isabsinc d]
    (Stdout stdout, Stderr emsg, Exit ex) <-
          cmd ocamldep "-one-line -I" outdir incs ppflags src
    ppppe ex src emsg
    need $ deplist $ parseMakefile stdout
    let fixedflags = fixincludes flagl
    (Stderr emsg, Exit ex) <-
      cmd comp "-c -I" outdir fixedflags  "-o" out ppflags src
    ppppe ex src emsg
  where (target, suffix, op) = case t of
          CMO -> ("//*.cmo", ".ml", (%>))
          CMI -> ("//*.cmi", ".mli", (%>))
        deplist ((_, reqs) : _) =
          [if takeDirectory1 n == outdir then n else outdir </> n | n <- reqs]
        ppppe ExitSuccess _ _ = do return ()
        ppppe _ src emsg = error $ fixpp src emsg

main = shakeArgs shakeOptions { shakeFiles = outdir
                              , shakeVerbosity = Normal
                              , shakeChange = ChangeModtimeAndDigest } $ do
  want ["build/llpp"]

  gitDescribeOracle <- addOracle $ \(GitDescribeOracle ()) -> do
    Stdout out <- cmd "git describe --tags --dirty"
    return $ (out :: String)

  ocamlOracle <- addOracle $ \(OcamlCmdLineOracle s) -> do return $ ocamlKey s

  cOracle <- addOracle $ \(CCmdLineOracle s) -> do return $ cKey s

  outdir ++ "/help.ml" %> \out -> do
    version <- gitDescribeOracle $ GitDescribeOracle ()
    need ["mkhelp.sh", "KEYS"]
    Stdout f <- cmd "/bin/sh mkhelp.sh KEYS" version
    writeFileChanged out f

  outdir ++ "/link.o" %> \out -> do
    let key = dropDirectory1 out
    flags <- cOracle $ CCmdLineOracle key
    let src = key -<.> ".c"
    let dep = out -<.> ".d"
    unit $ cmd ocamlc "-ccopt"
      [flags ++ " -MMD -MF " ++ dep ++ " -o " ++ out] "-c" src
    needMakefileDependencies dep

  outdir ++ "/llpp" %> \out -> do
    need $ map ((</>) outdir) ["link.o", "main.cmo", "wsi.cmo", "help.ml"]
    unit $ cmd ocamlc "-custom -I +lablGL -o " out
      "unix.cma str.cma lablgl.cma" cmos (outdir </> "link.o") "-cclib" [cclib]

  cm' outdir CMI ocamlOracle
  cm' outdir CMO ocamlOracle

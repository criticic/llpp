{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
import Data.List
import System.Exit
import Control.Monad
import Control.Concurrent.MVar
import Development.Shake
import Development.Shake.Util
import Development.Shake.Classes
import Development.Shake.FilePath

newtype OcamlOrdOracle = OcamlOrdOracle String
                       deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype OcamlOrdOracleN = OcamlOrdOracleN String
                       deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype OcamlCmdLineOracle = OcamlCmdLineOracle String
                           deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype OcamlCmdLineOracleN = OcamlCmdLineOracleN String
                           deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype CCmdLineOracle = CCmdLineOracle String
                       deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype GitDescribeOracle = GitDescribeOracle ()
                          deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

outdir = "build"
mudir = "mupdf"
inOutDir s = outdir </> s

ocamlc = "ocamlc.opt"
ocamlopt = "ocamlopt.opt"
ocamldep = "ocamldep.opt"
ocamlflags = "-warn-error +a -w +a -g -safe-string"
ocamlflagstbl = [("main.cmo", ("-I lablGL", "sed -f pp.sed"))
                ,("config.cmo", ("-I lablGL", ""))
                ]
ocamlflagstbln = [("main.cmx", ("-I lablGL", "sed -f pp.sed"))
                 ,("config.cmx", ("-I lablGL", ""))
                 ]
cflags = "-Wall -Werror -D_GNU_SOURCE -O\
         \ -g -std=c99 -pedantic-errors\
         \ -Wunused-parameter -Wsign-compare -Wshadow"
cflagstbl =
  [("link.o"
   ,"-I " ++ mudir ++ "/include -I "
    ++ mudir ++ "/thirdparty/freetype/include -Wextra")
  ]
cclib = "-lGL -lX11 -lmupdf -lz -lfreetype -ljpeg\
        \ -ljbig2dec -lopenjpeg -lmujs\
        \ -lpthread -L" ++ mudir ++ "/build/native -lcrypto"

getincludes :: [String] -> [String]
getincludes [] = []
getincludes ("-I":arg:tl) = arg : getincludes tl
getincludes (_:tl) = getincludes tl

isabsinc :: String -> Bool
isabsinc [] = False
isabsinc (hd:_) = hd == '+' || hd == '/'

fixincludes [] = []
fixincludes ("-I":d:tl)
  | isabsinc d = "-I":d:fixincludes tl
  | otherwise = "-I":inOutDir d:fixincludes tl
fixincludes (e:tl) = e:fixincludes tl

ocamlKey key | "lablGL/" `isPrefixOf` key = (ocamlc, "-I lablGL", [])
             | otherwise = case lookup key ocamlflagstbl of
               Nothing -> (ocamlc, ocamlflags, [])
               Just (f, []) -> (ocamlc, ocamlflags ++ " " ++ f, [])
               Just (f, pp) -> (ocamlc, ocamlflags ++ " " ++ f, ["-pp", pp])

ocamlKeyN key | "lablGL/" `isPrefixOf` key = (ocamlopt, "-I lablGL", [])
              | otherwise = case lookup key ocamlflagstbln of
                Nothing -> (ocamlopt, ocamlflags, [])
                Just (f, []) -> (ocamlopt, ocamlflags ++ " " ++ f, [])
                Just (f, pp) -> (ocamlopt, ocamlflags ++ " " ++ f, ["-pp", pp])

cKey key | "lablGL/" `isPrefixOf` key = "-Wno-pointer-sign -O2"
         | otherwise = case lookup key cflagstbl of
           Nothing -> cflags
           Just f -> f ++ " " ++ cflags

fixppfile s ("File":_:tl) = ("File \"" ++ s ++ "\","):tl
fixppfile _ l = l

fixpp :: String -> String -> String
fixpp r s = unlines [unwords $ fixppfile r $ words x | x <- lines s]

ppppe ExitSuccess _ _ = return ()
ppppe _ src emsg = error $ fixpp src emsg

needsrc key suff = do
  let src' = key -<.> suff
  let src = if src' == "help.ml" then inOutDir src' else src'
  need [src]
  return src

cmio target suffix oracle ordoracle = do
  target %> \out -> do
    let key = dropDirectory1 out
    src <- needsrc key suffix
    (comp, flags, ppflags) <- oracle $ OcamlCmdLineOracle key
    let flagl = words flags
    let dep = out ++ "_dep"
    need [dep]
    ddep <- liftIO $ readFile dep
    let deps = deplist $ parseMakefile ddep
    need deps
    let fixedflags = fixincludes flagl
    (Stderr emsg2, Exit ex2) <-
      cmd comp "-c -I" outdir fixedflags "-o" out ppflags src
    ppppe ex2 src emsg2
    return ()
  target ++ "_dep" %> \out -> do
    let key' = dropDirectory1 out
    let key = reverse (drop 4 $ reverse key')
    src <- needsrc key suffix
    (_, flags, ppflags) <- oracle $ OcamlCmdLineOracle key
    let flagl = words flags
    let incs = unwords ["-I " ++ d | d <- getincludes flagl
                                   , not $ isabsinc d]
    (Stdout stdout, Stderr emsg, Exit ex) <-
          cmd ocamldep "-one-line" incs "-I" outdir ppflags src
    ppppe ex src emsg
    writeFileChanged out stdout
    let depo = deps ++ [dep -<.> ".cmo" | dep <- deps, fit dep]
          where
            deps = deplist $ parseMakefile stdout
            fit dep = ext == ".cmi" && base /= baseout
              where (base, ext) = splitExtension dep
                    baseout = dropExtension out
    need $ map (++ "_dep") depo
    let ord = reverse (drop 4 $ reverse out)
    unit $ ordoracle $ OcamlOrdOracle ord
    return ()
  where
    deplist [] = []
    deplist ((_, reqs) : _) =
      [if takeDirectory1 n == outdir then n else inOutDir n | n <- reqs]

cmx oracle ordoracle =
  "//*.cmx" %> \out -> do
    let key = dropDirectory1 out
    src <- needsrc key ".ml"
    (comp, flags, ppflags) <- oracle $ OcamlCmdLineOracleN key
    let flagl = words flags
    let incs = unwords ["-I " ++ d | d <- getincludes flagl
                                   , not $ isabsinc d]
    (Stdout stdout, Stderr emsg, Exit ex) <-
          cmd ocamldep "-one-line" incs "-I" outdir ppflags src
    ppppe ex src emsg
    need $ deplist $ parseMakefile stdout
    unit $ ordoracle $ OcamlOrdOracleN out
    let fixedflags = fixincludes flagl
    (Stderr emsg2, Exit ex2) <-
      cmd comp "-c -I" outdir fixedflags "-o" out ppflags src
    ppppe ex2 src emsg2
    return ()
  where
    deplist (_ : (_, reqs) : _) =
      [if takeDirectory1 n == outdir then n else inOutDir n | n <- reqs]
    deplist _ = []

main = do
  depl <- newMVar ([] :: [String])
  depln <- newMVar ([] :: [String])
  shakeArgs shakeOptions { shakeFiles = outdir
                         , shakeVerbosity = Normal
                         , shakeChange = ChangeModtimeAndDigest } $ do
  want [inOutDir "llpp"]

  gitDescribeOracle <- addOracle $ \(GitDescribeOracle ()) -> do
    Stdout out <- cmd "git describe --tags --dirty"
    return (out :: String)

  ocamlOracle <- addOracle $ \(OcamlCmdLineOracle s) ->
    return $ ocamlKey s

  ocamlOracleN <- addOracle $ \(OcamlCmdLineOracleN s) ->
    return $ ocamlKeyN s

  ocamlOrdOracle <- addOracle $ \(OcamlOrdOracle s) ->
    unless (takeExtension s == ".cmi") $
      liftIO $ modifyMVar_ depl $ \l -> return $ s:l

  ocamlOrdOracleN <- addOracle $ \(OcamlOrdOracleN s) ->
    unless (takeExtension s == ".cmi") $
      liftIO $ modifyMVar_ depln $ \l -> return $ s:l

  cOracle <- addOracle $ \(CCmdLineOracle s) -> return $ cKey s

  inOutDir "help.ml" %> \out -> do
    version <- gitDescribeOracle $ GitDescribeOracle ()
    need ["mkhelp.sh", "KEYS"]
    Stdout f <- cmd "/bin/sh mkhelp.sh KEYS" version
    writeFileChanged out f

  "//*.o" %> \out -> do
    let key = dropDirectory1 out
    flags <- cOracle $ CCmdLineOracle key
    let src = key -<.> ".c"
    let dep = out -<.> ".d"
    unit $ cmd ocamlc "-ccopt"
      [flags ++ " -MMD -MF " ++ dep ++ " -o " ++ out] "-c" src
    needMakefileDependencies dep

  inOutDir "llpp" %> \out -> do
    let objs = map (inOutDir . (++) "lablGL/ml_") ["gl.o", "glarray.o", "raw.o"]
    need (objs ++ map inOutDir ["link.o", "main.cmo", "help.cmo"])
    cmos <- liftIO $ readMVar depl
    need cmos
    unit $ cmd ocamlc "-g -custom -I lablGL -o" out
      "unix.cma str.cma" (reverse cmos)
      (inOutDir "link.o") "-cclib" (cclib : objs)

  inOutDir "llpp.native" %> \out -> do
    let objs = map (inOutDir . (++) "lablGL/ml_") ["gl.o", "glarray.o", "raw.o"]
    need (objs ++ map inOutDir ["link.o", "main.cmx", "help.cmx"])
    cmxs <- liftIO $ readMVar depln
    need cmxs
    unit $ cmd ocamlopt "-g -I lablGL -o" out
      "unix.cmxa str.cmxa" (reverse cmxs)
      (inOutDir "link.o") "-cclib" (cclib : objs)

  cmio "//*.cmi" ".mli" ocamlOracle ocamlOrdOracle
  cmio "//*.cmo" ".ml" ocamlOracle ocamlOrdOracle
  cmx ocamlOracleN ocamlOrdOracleN

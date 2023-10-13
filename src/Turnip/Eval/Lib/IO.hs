{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Turnip.Eval.Lib.IO (loadBaseLibraryGen) where

import Turnip.Eval.Types

io_close :: NativeFunction
io_close (f : _) = file_close [f]            -- close the output file
io_close [] = file_close [defaultOutputFile] -- close the default output file

 -- Equivalent to file:flush over the default output file.
io_flush :: NativeFunction
io_flush _ = file_flush [defaultOutputFile]

-- When called with a file name, it opens the named file (in text mode), and sets its handle as the default input file.
-- When called with a file handle, it simply sets this file handle as the default input file.
-- When called without parameters, it returns the current default input file.
-- In case of errors this function raises the error, instead of returning an error code.
io_input :: NativeFunction
io_input (Str filename : _) = undefined
io_input (Userdata handle : _) = undefined
io_input [] = return defaultInputFile
io_input _ = throwErrorStr "Wrong argument to 'io.input'"

io_lines :: NativeFunction
io_lines (Str filename : _) = undefined
io_lines [] = file_lines [defaultInputFile]

io_open :: NativeFunction
io_open (Str filename : Str mode : _) = undefined -- TODO delegate to fopen cb
io_open (Str filename : []) = undefined
io_open _ = throwErrorStr "Wrong argument to 'io.open', filename expected."

-- Similar to io.input, but operates over the default output file.
io_output :: NativeFunction
io_output [] = return defaultOutputFile
io_output _ = throwErrorStr "Wrong argument to 'io.output'"

-- io.popen (prog [, mode])
-- Starts program prog in a separated process and returns a file handle that you can use to read data from this program (if mode is "r", the default) or to write data to this program (if mode is "w").
-- This function is system dependent and is not available on all platforms.
-- io_popen :: NativeFunction

-- Equivalent to io.input():read.
io_read :: NativeFunction
io_read params = file_read (defaultInputFile:params)

-- Returns a handle for a temporary file. This file is opened in update mode and it is automatically removed when the program ends.
io_tmpfile :: NativeFunction
-- delegate to io.open(os.tmpname())

io_type :: NativeFunction
io_type (Userdata handle : _) = "file" / "closed file" / nil
io_type (x : _) = return [Nil]
io_type _ = throwErrorStr "Wrong argument to 'io.type', value expected."

-- Equivalent to io.output():write.
io_write :: NativeFunction
io_write params = file_write (defaultOutputFile:params)


-- TODO create a metatable for all files and fill with those
-- when a file handle is created as userdata, set its metatable to that
file_close :: NativeFunction
file_close (Userdata file : _) = ...
file_close _ = throwErrorStr "Close called on not a file ??" -- TODO

file_flush :: NativeFunction
file_lines :: NativeFunction
file_read :: NativeFunction
file_seek :: NativeFunction
file_setvbuf :: NativeFunction
file_write :: NativeFunction


loadBaseLibraryGen :: String -> LuaM ()
loadBaseLibraryGen modName = addNativeModule modName [
        ("close", (BuiltinFunction io_close)),
        ("flush", (BuiltinFunction io_flush)),
        ("input", (BuiltinFunction io_input)),
        ("lines", (BuiltinFunction io_lines)),
        ("open",  (BuiltinFunction io_open)),
    ]
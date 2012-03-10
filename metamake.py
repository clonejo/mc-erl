#!/usr/bin/env python
import os

win_erlc_path = "d:/erl5.9/bin/erlc.exe"
linux_erlc_path = "erlc"
makefile_name = "make.bat" if os.name == "nt" else "make.sh"

if os.name == "nt":
	pattern = "echo compiling %%s\n %s -o ../ebin %%s\n\n" % (win_erlc_path,)
	prologue = """@echo off
cd src\n\n"""

elif os.name == "posix":
	pattern = "echo compiling %%s\n %s -o ../ebin %%s\n\n" % (linux_erlc_path,)
	prologue = """#!/bin/bash
cd src\n\n"""

files = os.listdir("./src")
mkf = open(makefile_name, "w")
mkf.write(prologue)

f2 = []
for f in files:
	if f[-4:] == ".erl":
		f2.append(f)

print f2
for f in f2:
	mkf.write(pattern % (f,f))

mkf.close()
os.chmod(makefile_name, 0777)
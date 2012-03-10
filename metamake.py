import os

if os.name == "nt":
	pattern = "echo compiling %s\n d:/erl5.9/bin/erlc.exe -o ebin %s\n\n"
	prologue = "@echo off\ndel ebin/*\n\n"
elif os.name == "posix":
	pattern = "echo compiling %s\n erlc -o ebin %s\n\n"
	prologue = "#!/bin/sh\nrm ebin/*\n\n"

files = os.listdir(".")
mkf = open("make.bat" if os.name == "nt" else "make.sh", "w")
mkf.write(prologue)

f2 = []
for f in files:
	if f[-4:] == ".erl":
		f2.append(f)

print f2
for f in f2:
	mkf.write(pattern % (f,f))

mkf.close()
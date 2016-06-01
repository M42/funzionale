all: hashlist.dll listset.dll

hashlist.dll: hashlist.fs set.fsi
	fsharpc -a set.fsi hashlist.fs

listset.dll: listset.fs set.fsi
	fsharpc -a set.fsi listset.fs

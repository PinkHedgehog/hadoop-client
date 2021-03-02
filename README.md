# hadoop-client
Simple hadoop client

## Client supports these operations:

```console
mkdir  <HDFS  directory>                  create directory in HDFS
put    <local file name>                  upload file to HDFS
get    <HDFS  file name>                  download file from HDFS
append <local file name> <HDFS file name> append local file to file in HFDS
delete <HDFS  file name>                  delete file in HDFS

lcd    <local dir name>     change local working directory, ".." for parent directory
cd     <HDFS  dir name>     change HDFS  working directory, ".." for level up

ls     list HDFS  working directory
lls    list local working directory

CTRL-C exit
```

## Installation
1. install and upgrade haskell-stack
2. run in your shell:
```console
$ stack build
```

## Usage
inside project directory run:
```console
$ stack exec -- hadoop-client <url> <port> <username>
```
* url - where your hadoop is
* port - usually 50070
* username - your username in hdfs

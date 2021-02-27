# hadoop-client
Simple hadoop client

Supports operations (ACHTUNG! Russian!):

* mkdir <имя каталога в HDFS> (создание каталога в HDFS);
* put <имя локального файла> (загрузка файла в HDFS);
* get <имя файла в HDFS> (скачивание файла из HDFS);
* append <имя локального файла> <имя файла в HDFS> (конкатенация файла в HDFS с локальным файлом);
* delete <имя файла в HDFS> (удаление файла в HDFS);
* ls (отображение содержимого текущего каталога в HDFS с разделением файлов и каталогов);
* cd <имя каталога в HDFS> (переход в другой каталог в HDFS, ".." - на уровень выше);
* lls (отображение содержимого текущего локального каталога с разделением файлов и каталогов);
* lcd <имя локального каталога> (переход в другой локальный каталог, ".." - на уровень выше)

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

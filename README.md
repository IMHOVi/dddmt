# dddmt

Distributed Dependent Data Maintenance Tool

## What is it?

`dddmt` is a workflow support tool for Big Data. It has the following three major functions:

* check dependencies between dynamically mutating data sets 
* launch rebuild if needed
* provide status information

Each of these data sets may be located:

* On a local file system
* On a HDFS
* On a remote file system (FTP and SFTP are supported)

Rebuild may be one of

* Copy from one FS to the other
* MapReduce/Spark/Generic OS Task

The main purpose of this tool's existence is optimization. Only those rebuild processes are launched that are actually needed. 

## Use cases? 

* Collecting and processing log files from a distributed remote system or a remote file storage



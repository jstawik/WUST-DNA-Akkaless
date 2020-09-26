# WUST-DNA-Akkaless

## About

This project is a version of WUST-DNA rewritten to run without Akka - which is beneficient when running on a single machine (it can still parallelize across multiple cores).
As for the foreseeable future this is the main use case for us, this project is the one actively developed. 

WUST-DNA - Wroclaw University of Science and Technology: Disperse Network Aggregation is a framework allowing for simulation of network of nodes running data propagation algorithms like the one from [Jacek Cichoń and Karol Gotfryd. 2018. Average Counting via Approximate Histograms](https://cs.pwr.edu.pl/cichon/Opus5/99-AverageMain.pdf)
Currently it sports the following features:
1. Reads input provided as a `.json` file (or multiple files) specyfying type of the network, type of the node (i.e. algorithm it's running) and parameters of those. The files will be processed sequentialy, but the networks will be simulated in parallel if possible.
1. Saves output as either `.json` or `.csv` file. Has the possibility of creating plots on the fly, but jupyter [notebooks](src/main/resources/Reporting_workbook.ipynb) are also in use to allow for data exploration.
1. Offers the following network shapes:
    - Grid
1. Offers the following node types:
    - JoinersLeavers 
    - JoinersLeaversNoZeroCheck

## Building the code

The code can be build after pulling it from github with a simple `sbt compile` task.

## Running the code

The jar file can be run with one optional parameter: path to a directory containing `.json` jobs. It will Try to read each of those files as a `Config` of a simulation and if succesful - execute it. If run without parameters, the current path will be used.
An example of `.json` job can be seen [here](src/main/resources/jobs/config.json)

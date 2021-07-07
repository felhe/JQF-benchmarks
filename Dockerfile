# syntax=docker/dockerfile:1
FROM ubuntu:latest

## Install all Packages needed
RUN apt-get -y update
RUN apt-get install -y make clang openjdk-8-jdk maven
RUN apt-get install -y python3
RUN apt-get install -y python3-matplotlib
RUN apt-get install -y git
RUN apt-get install -y nano
RUN apt-get install -y bash-completion
RUN apt-get update

## copy scripts Folder
WORKDIR /zest-artifact
RUN mkdir scripts

COPY scripts /zest-artifact/scripts

#Clone JQF repos

#Pest1
RUN git clone https://github.com/felhe/JQF.git
RUN mv JQF jqf-PEST

#Pest2 (for comparing changes within Pest)
RUN git clone https://github.com/felhe/JQF.git
RUN mv JQF jqf-PEST2

RUN git clone https://github.com/rohanpadhye/JQF.git
RUN mv JQF jqf

#Clone AFL REPO
RUN git clone https://github.com/google/AFL.git
RUN mv AFL afl


WORKDIR /zest-artifact/jqf
RUN mvn install
RUN make clean all


WORKDIR /zest-artifact/jqf-PEST
RUN mvn install
RUN make clean all

WORKDIR /zest-artifact/jqf-PEST2
RUN mvn install
RUN make clean all


WORKDIR /zest-artifact/afl
RUN make clean all



ENV JQF_DIR=/zest-artifact/jqf
ENV JQF_PEST_DIR=/zest-artifact/jqf-PEST
ENV JQF_PEST2_DIR=/zest-artifact/jqf-PEST2
ENV AFL_DIR=/zest-artifact/afl

WORKDIR /zest-artifact

CMD ["/bin/bash"]

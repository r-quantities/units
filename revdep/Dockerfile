FROM rocker/r-ubuntu:22.04
COPY bootstrap.R /tmp/
RUN Rscript /tmp/bootstrap.R
ENV workers 2
CMD Rscript /tmp/bootstrap.R

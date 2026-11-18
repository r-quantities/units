FROM rocker/r2u:latest
ENV RUSTUP_HOME=/usr/local/rustup \
    CARGO_HOME=/usr/local/cargo \
    PATH=/usr/local/cargo/bin:$PATH
RUN apt-get -y update && apt-get -y install curl && \
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | \
    sh -s -- -y --default-toolchain stable --profile minimal
COPY bootstrap.R /tmp/
RUN Rscript /tmp/bootstrap.R
ENV workers 2
CMD Rscript /tmp/bootstrap.R

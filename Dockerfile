FROM haskell:latest

COPY . /opt/build

RUN curl -sL https://deb.nodesource.com/setup_8.x | bash - && \
    apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y locales nodejs && \
    sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    dpkg-reconfigure --frontend=noninteractive locales && \
    update-locale LANG=en_US.UTF-8 && \
    rm -rf /var/lib/apt/lists/* && \
    cd /opt/build && cabal update && cabal --dependencies-only build && rm -rf /opt/build

ENV LANG en_US.UTF-8

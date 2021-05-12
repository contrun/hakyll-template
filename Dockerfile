FROM haskell:latest

RUN curl -sL https://deb.nodesource.com/setup_16.x | bash - && \
    apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y locales nodejs && \
    sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    dpkg-reconfigure --frontend=noninteractive locales && \
    update-locale LANG=en_US.UTF-8 && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /hakyll/builder

COPY stack.yaml stack.yaml.lock site.cabal /hakyll/builder/
RUN stack setup

COPY . /hakyll/builder
RUN make prepare

ENV HAKYLL_PROVIDER_DIRECTORY=/hakyll/builder
ENV HAKYLL_DESTINATION_DIRECTORY=/hakyll/builder/public
ENV LANG en_US.UTF-8
ENTRYPOINT ["make"]

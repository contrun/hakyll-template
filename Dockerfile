FROM haskell:latest

RUN curl -sL https://deb.nodesource.com/setup_16.x | bash - && \
    apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y locales nodejs openssl bash && \
    sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    dpkg-reconfigure --frontend=noninteractive locales && \
    update-locale LANG=en_US.UTF-8 && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /hakyll/builder

COPY site.cabal cabal.project.freeze /hakyll/builder/
RUN cabal fetch

COPY . /hakyll/builder
RUN make prepare

ENV HAKYLL_PROVIDER_DIRECTORY=/hakyll/builder
ENV HAKYLL_DESTINATION_DIRECTORY=/hakyll/builder/public
ENV HAKYLL_BUILDER_DIRECTORY=/hakyll/builder
ENV LANG en_US.UTF-8
ENTRYPOINT ["make"]

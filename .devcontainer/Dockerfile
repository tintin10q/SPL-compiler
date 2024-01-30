FROM debian:12.4

# Versions
ARG GHC=9.6.3
ARG HLS=recommended
ARG CABAL=recommended

# Install necessary dependencies
RUN apt-get update && \
    apt-get install -y \
        build-essential \
        curl \
        git \
	libffi-dev \
        libffi8 \
        libgmp-dev \
        libgmp10 \
        libncurses-dev \
        libncurses5 \
        libtinfo5 \
        zlib1g-dev && \
    rm -rf /var/lib/apt/lists/*

# Install GHCup
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
RUN bash -c "curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh"

# Add GHCup and global Cabal dependencies to PATH
ENV PATH=${PATH}:/root/.local/bin
ENV PATH=${PATH}:/root/.ghcup/bin
ENV PATH=${PATH}:/root/.cabal/bin

# Install Haskell and tooling
RUN ghcup install ghc ${GHC} --set
RUN ghcup install cabal ${CABAL} --set
RUN ghcup install hls ${HLS} --set

# Install some useful global packages
RUN cabal update && \
    cabal install --haddock-hoogle --minimize-conflict-set \
        haskell-dap-0.0.16.0 \
        ghci-dap-0.0.22.0 \
        haskell-debug-adapter-0.0.39.0 \
        hlint-3.6.1 \
        hoogle-5.0.18.3 \
        ormolu-0.7.2.0

# Generate Hoogle database
RUN hoogle generate --download --haskell

# Open shell
ENTRYPOINT ['/bin/bash']

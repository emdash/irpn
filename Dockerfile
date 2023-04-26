# cache dependencies to avoid needing to rebuild.
# I may live to regret this.
FROM ghcr.io/emdash/irpn-base:latest
WORKDIR /root/
COPY irpn.ipkg .
COPY pack.toml .
COPY src       src
RUN pack build irpn.ipkg

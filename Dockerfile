FROM ghcr.io/stefan-hoeck/idris2-pack:latest

RUN mkdir -p /opt/irpn

WORKDIR /opt/irpn

COPY irpn.ipkg .
COPY src       src
RUN  true

# cache dependencies to avoid needing to rebuild.
# I may live to regret this.
RUN pack install-deps irpn.ipkg

RUN pack build irpn.ipkg

FROM ghcr.io/stefan-hoeck/idris2-pack:latest

RUN mkdir -p /opt/irpn

WORKDIR /opt/irpn

COPY irpn.ipkg .
COPY src       src
RUN  true

RUN pack build irpn.ipkg

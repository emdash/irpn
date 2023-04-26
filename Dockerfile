# cache dependencies to avoid needing to rebuild.
# I may live to regret this.
FROM ghcr.io/stefan-hoeck/idris2-pack:latest AS irpn-base
WORKDIR /root/
COPY irpn.ipkg .
RUN pack install-deps irpn.ipkg

# now copy the source and perform the build phase in this container
FROM irpn-base
WORKDIR /root/
COPY src       src
RUN pack build irpn.ipkg

# ubuntu-rebar includes erlang
FROM envirosoftwaresolutions/ubuntu-rebar:0.0.9

# set working directory
RUN mkdir -p /build
WORKDIR /build

# copy erlang source to /build
COPY wavegen wavegen

# build
WORKDIR wavegen

# TODO: replace with: RUN rebar3 as prod release?
RUN rebar3 do clean, compile, escriptize, eunit

# serving metrics on port  
EXPOSE 4444

ENTRYPOINT ["/bin/bash", "-l", "-c", "/build/wavegen/_build/default/bin/wavegen"]

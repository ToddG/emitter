# ubuntu-rebar includes erlang
FROM envirosoftwaresolutions/ubuntu-rebar:latest as LAYER0

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

CMD ["/build/wavegen/_build/default/bin/wavegen"]

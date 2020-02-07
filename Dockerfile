# Compilaton and testing
FROM erlang:21.0 as util

COPY ./ /build
WORKDIR /build

RUN rebar3 compile
#RUN rebar3 do eunit, ct

RUN rm -rf _build/default/rel
RUN rebar3 release

# Make an app image
FROM erlang:21.0

WORKDIR /opt/chat_srv
COPY --from=util /build/_build/default/rel/chat_srv ./

EXPOSE 8080
EXPOSE 8081

CMD bin/chat_srv foreground

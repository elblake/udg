@echo off
erl -pa %~dp0/../ebin -pa %~dp0/ebin -run erlc_udg run -noshell -noinput -run erlang halt -- %*

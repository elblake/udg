@echo off
erl -pa %~dp0/../ebin -pa %~dp0/ebin -run udg_tests_a run -noshell -run erlang halt
